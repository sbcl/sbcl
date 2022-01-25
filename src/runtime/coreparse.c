/*
 * A saved SBCL system is a .core file; the code here helps us accept
 * such a file as input.
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#include "sbcl.h"

#ifndef LISP_FEATURE_WIN32
#include <sys/mman.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "os.h"
#include "runtime.h"
#include "globals.h"
#include "core.h"
#include "arch.h"
#include "interr.h"
#include "thread.h"

#include "validate.h"
#include "gc-internal.h"
#include "gc-private.h"
#include "getallocptr.h"
#include "code.h"

#include <errno.h>

#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
# include <zlib.h>
#endif

/* build_id must match between the C code and .core file because a core
 * is only guaranteed to be compatible with the C runtime that created it.
 * We can't easily detect whether the sources were patched after saving
 * a core, but we can easily enforce a matching build_id.
 * Note that fasls have a different way of ensuring compatibility with the
 * core: the contents of version.lisp-expr are written into the fasl.
 * Both checks avoid confusion for end-users, but the build_id test
 * is more geared toward developers as it can change with each rebuild.
 */
unsigned char build_id[] =
// The suffix added to build-id indicates which flavor of C compiler was used.
// This enforces that when you put :MSAN in your Lisp features, you don't
// omit "-fsanitize=memory -DMEMORY_SANITIZE" from CFLAGS and LINKFLAGS.
// (Some Lisp features affect C flags, but not this one.)
// It is important for out-of-tree builds: once they work, you can produce
// two trees of artifacts from identical sources *including* the build-id.inc
// (but not including "local-target-features.lisp-expr"), and end up with two
// Lisp cores and two C runtimes. The extra suffix avoids accidental mismatch.
#include "../../output/build-id.inc"
#ifdef MEMORY_SANITIZER
"-msan"
#endif
;

static int
open_binary(char *filename, int mode)
{
#ifdef LISP_FEATURE_WIN32
    mode |= O_BINARY;
#endif

    return open(filename, mode);
}

#if defined(LISP_FEATURE_ELF) && defined(LISP_FEATURE_IMMOBILE_CODE)
#define ELFCORE 1
#elif !defined(ELFCORE)
#define ELFCORE 0
#endif

#if !ELFCORE
int lisp_code_in_elf() { return 0; }
#else
extern __attribute__((weak)) lispobj
 lisp_code_start, lisp_jit_code, lisp_code_end, lisp_linkage_values;
int lisp_code_in_elf() { return &lisp_code_start != 0; }
#endif

/* Search 'filename' for an embedded core.  An SBCL core has, at the
 * end of the file, a trailer containing optional saved runtime
 * options, the start of the core (an os_vm_offset_t), and a final
 * signature word (the lispobj CORE_MAGIC).  If this trailer is found
 * at the end of the file, the start of the core can be determined
 * from the core size.
 *
 * If an embedded core is present, this returns the offset into the
 * file to load the core from, or -1 if no core is present. */
os_vm_offset_t
search_for_embedded_core(char *filename, struct memsize_options *memsize_options)
{
    extern os_vm_offset_t search_for_elf_core(int);
    lispobj header = 0;
    os_vm_offset_t lispobj_size = sizeof(lispobj);
    int fd;

    if ((fd = open_binary(filename, O_RDONLY)) < 0)
        return -1;

    if (read(fd, &header, lispobj_size) == lispobj_size && header == CORE_MAGIC) {
        /* This file is a real core, not an embedded core.  Return 0 to
         * indicate where the core starts, and do not look for runtime
         * options in this case. */
        close(fd);
        return 0;
    }

    os_vm_offset_t core_start = -1; // invalid value
    if (lseek(fd, -lispobj_size, SEEK_END) < 0 ||
        read(fd, &header, (size_t)lispobj_size) != lispobj_size)
        goto lose;

    if (header == CORE_MAGIC) {
        // the last word in the file could be CORE_MAGIC by pure coincidence
        if (lseek(fd, -(lispobj_size + sizeof(os_vm_offset_t)), SEEK_END) < 0 ||
            read(fd, &core_start, sizeof(os_vm_offset_t)) != sizeof(os_vm_offset_t))
            goto lose;
        if (lseek(fd, core_start, SEEK_SET) != core_start ||
            read(fd, &header, lispobj_size) != lispobj_size || header != CORE_MAGIC)
            core_start = -1; // reset to invalid
    }
#if ELFCORE && !defined(LISP_FEATURE_DARWIN)
    // Specifying "--core" as an ELF file with a lisp.core section doesn't work.
    // (There are bunch of reasons) So only search for a core section if this
    // is an implicit search for a core embedded in an executable.
    // The two cases can be distinguished based on whether the core is able
    // to set the memsize_options. (Implicit can set them, explicit can't)
    if (core_start < 0 && memsize_options) {
        if (!(core_start = search_for_elf_core(fd)) ||
            lseek(fd, core_start, SEEK_SET) != core_start ||
            read(fd, &header, lispobj_size) != lispobj_size || header != CORE_MAGIC)
            core_start = -1; // reset to invalid
    }
#endif
    if (core_start > 0 && memsize_options) {
        core_entry_elt_t optarray[RUNTIME_OPTIONS_WORDS];
        // file is already positioned to the first core header entry
        if (read(fd, optarray, sizeof optarray) == sizeof optarray
            && optarray[0] == RUNTIME_OPTIONS_MAGIC) {
            memsize_options->dynamic_space_size = optarray[2];
            memsize_options->thread_control_stack_size = optarray[3];
            memsize_options->thread_tls_bytes = optarray[4];
            memsize_options->present_in_core = 1;
        }
    }
lose:
    close(fd);
    return core_start;
}

#ifndef LISP_FEATURE_SB_CORE_COMPRESSION
# define inflate_core_bytes(fd,offset,addr,len) \
    lose("This runtime was not built with zlib-compressed core support... aborting")
#else
# define ZLIB_BUFFER_SIZE (1u<<16)
static void inflate_core_bytes(int fd, os_vm_offset_t offset,
                               os_vm_address_t addr, int len)
{
    z_stream stream;
    unsigned char* buf = successful_malloc(ZLIB_BUFFER_SIZE);
    int ret;

# ifdef LISP_FEATURE_WIN32
    /* Ensure the memory is committed so zlib doesn't segfault trying to
       inflate. */
    os_commit_memory(addr, len);
# endif

    if (-1 == lseek(fd, offset, SEEK_SET)) {
        lose("Unable to lseek() on corefile");
    }

    stream.zalloc = NULL;
    stream.zfree = NULL;
    stream.opaque = NULL;
    stream.avail_in = 0;
    stream.next_in = buf;

    ret = inflateInit(&stream);
    if (ret != Z_OK)
        lose("zlib error %i", ret);

    stream.next_out  = (void*)addr;
    stream.avail_out = len;
    do {
        ssize_t count = read(fd, buf, ZLIB_BUFFER_SIZE);
        if (count < 0)
            lose("unable to read core file (errno = %i)", errno);
        stream.next_in = buf;
        stream.avail_in = count;
        if (count == 0) break;
        ret = inflate(&stream, Z_NO_FLUSH);
        switch (ret) {
        case Z_STREAM_END:
            break;
        case Z_OK:
            if (stream.avail_out == 0)
                lose("Runaway gzipped core directory... aborting");
            if (stream.avail_in > 0)
                lose("zlib inflate returned without fully"
                     "using up input buffer... aborting");
            break;
        default:
            lose("zlib inflate error: %i", ret);
            break;
        }
    } while (ret != Z_STREAM_END);

    if (stream.avail_out > 0) {
        if (stream.avail_out >= os_vm_page_size)
            fprintf(stderr, "Warning: gzipped core directory significantly"
                    "shorter than expected (%lu bytes)", (unsigned long)stream.avail_out);
        /* Is this needed? */
        memset(stream.next_out, 0, stream.avail_out);
    }

    inflateEnd(&stream);
    free(buf);
}
# undef ZLIB_BUFFER_SIZE
#endif

#define DYNAMIC_SPACE_ADJ_INDEX 0
struct heap_adjust {
    /* range[0] is dynamic space, ranges[1] and [2] are immobile spaces */
    struct range {
        lispobj start, end;
        sword_t delta;
    } range[3];
    int n_ranges;
    int n_relocs_abs; // absolute
    int n_relocs_rel; // relative
};

#include "genesis/gc-tables.h"
#include "genesis/cons.h"
#include "genesis/hash-table.h"
#include "genesis/layout.h"
#include "genesis/vector.h"

static inline sword_t calc_adjustment(struct heap_adjust* adj, lispobj x)
{
    if (adj->range[0].start <= x && x < adj->range[0].end)
        return adj->range[0].delta;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (adj->range[1].start <= x && x < adj->range[1].end)
        return adj->range[1].delta;
    if (adj->range[2].start <= x && x < adj->range[2].end)
        return adj->range[2].delta;
#endif
    return 0;
}

// Return the adjusted value of 'word' without testing whether it looks
// like a pointer. But do test whether it points to a relocatable space.
static inline lispobj adjust_word(struct heap_adjust* adj, lispobj word) {
    return word + calc_adjustment(adj, word);
}

// Given a post-relocation object 'x', compute the address at which
// it was originally expected to have been placed as per the core file.
static inline lispobj inverse_adjust(struct heap_adjust* adj, lispobj x)
{
    int j;
    for (j=0; j<3; ++j)
        if (adj->range[j].start + adj->range[j].delta <= x &&
            x < adj->range[j].end + adj->range[j].delta)
            return x - adj->range[j].delta;
    return x;
}

#define SHOW_SPACE_RELOCATION 0
#if SHOW_SPACE_RELOCATION > 1
# define FIXUP(expr, addr) fprintf(stderr, "%p: (a) %lx", addr, *(long*)(addr)), \
   expr, fprintf(stderr, " -> %lx\n", *(long*)(addr)), ++adj->n_relocs_abs
# define FIXUP32(expr, addr) fprintf(stderr, "%p: (a) %x", addr, *(int*)(addr)), \
   expr, fprintf(stderr, " -> %x\n", *(int*)(addr)), ++adj->n_relocs_abs
# define FIXUP_rel(expr, addr) fprintf(stderr, "%p: (r) %x", addr, *(int*)(addr)), \
   expr, fprintf(stderr, " -> %x\n", *(int*)(addr)), ++adj->n_relocs_rel
#elif SHOW_SPACE_RELOCATION
# define FIXUP(expr, addr) expr, ++adj->n_relocs_abs
# define FIXUP32(expr, addr) expr, ++adj->n_relocs_abs
# define FIXUP_rel(expr, addr) expr, ++adj->n_relocs_rel
#else
# define FIXUP(expr, addr) expr
# define FIXUP32(expr, addr) expr
# define FIXUP_rel(expr, addr) expr
#endif

// Fix the word at 'where' without testing whether it looks pointer-like.
// Avoid writing if there is no adjustment.
static inline void adjust_word_at(lispobj* where, struct heap_adjust* adj) {
    lispobj word = *where;
    sword_t adjustment = calc_adjustment(adj, word);
    if (adjustment != 0)
        FIXUP(*where = word + adjustment, where);
}

// Adjust the words in range [where,where+n_words)
// skipping any words that have non-pointer nature.
static void adjust_pointers(lispobj *where, sword_t n_words, struct heap_adjust* adj)
{
    long i;
    for (i=0;i<n_words;++i) {
        lispobj word = where[i];
        sword_t adjustment;
        if (is_lisp_pointer(word) && (adjustment = calc_adjustment(adj, word)) != 0) {
            FIXUP(where[i] = word + adjustment, where+i);
        }
    }
}

#include "var-io.h"
#include "unaligned.h"
static void
adjust_code_refs(struct heap_adjust __attribute__((unused)) *adj,
                 struct code __attribute__((unused)) *code,
                 lispobj __attribute__((unused)) original_vaddr)
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    // Dynamic space always gets relocated before immobile space does,
    // and dynamic space code does not use fixups (except on 32-bit x86).
    // So if we're here, it must be to relocate an immobile object.
    // If 'code->fixups' is a bignum, the pointer itself was already fixed up.
    char* instructions = code_text_start(code);
    struct varint_unpacker unpacker;

    varint_unpacker_init(&unpacker, code->fixups);
    int prev_loc = 0, loc;
    while (varint_unpack(&unpacker, &loc) && loc != 0) {
        // For extra compactness, each loc is relative to the prior,
        // so that the magnitudes are smaller.
        loc += prev_loc;
        prev_loc = loc;
        void* fixup_where = instructions + loc;
        lispobj ptr = UNALIGNED_LOAD32(fixup_where);
        lispobj adjusted = ptr + calc_adjustment(adj, ptr);
        if (!(adjusted <= UINT32_MAX))
            lose("Absolute fixup @ %p exceeds 32 bits", fixup_where);
        if (adjusted != ptr)
            FIXUP32(UNALIGNED_STORE32(fixup_where, adjusted), fixup_where);
    }
    sword_t displacement = (lispobj)code - original_vaddr;
    prev_loc = 0;
    while (varint_unpack(&unpacker, &loc) && loc != 0) {
        loc += prev_loc;
        prev_loc = loc;
        void* fixup_where = instructions + loc;
        int32_t rel32operand = UNALIGNED_LOAD32(fixup_where);
        int32_t abs_operand = (sword_t)fixup_where + 4 + rel32operand - displacement;
        int32_t new_abs_operand = abs_operand + calc_adjustment(adj, abs_operand);
        sword_t new_rel32operand = new_abs_operand - ((sword_t)fixup_where + 4);
        // check for overflow before checking whether to write the new value
        if (!(new_rel32operand >= INT32_MIN && new_rel32operand <= INT32_MAX))
            lose("Relative fixup @ %p exceeds 32 bits", fixup_where);
        if (new_rel32operand != rel32operand)
            FIXUP_rel(UNALIGNED_STORE32(fixup_where, new_rel32operand), fixup_where);
    }
#endif
}

static inline void fix_fun_header_layout(lispobj __attribute__((unused)) *fun,
                                         struct heap_adjust __attribute__((unused)) *adj)
{
#if defined(LISP_FEATURE_COMPACT_INSTANCE_HEADER) && defined(LISP_FEATURE_64_BIT)
    lispobj ptr = funinstance_layout(fun);
    lispobj adjusted = adjust_word(adj, ptr);
    if (adjusted != ptr) FIXUP(funinstance_layout(fun)=adjusted, fun);
#endif
}

static void relocate_space(uword_t start, lispobj* end, struct heap_adjust* adj)
{
    lispobj *where = (lispobj*)start;
    int widetag;
    long nwords;
    lispobj layout, adjusted_layout;
    struct code* code;
    sword_t delta;
    int i;

    adj->n_relocs_abs = adj->n_relocs_rel = 0;
    for ( ; where < end ; where += nwords ) {
        lispobj word = *where;
        if (!is_header(word)) {
            adjust_pointers(where, 2, adj);
            nwords = 2;
            continue;
        }
        widetag = header_widetag(word);
        nwords = sizetab[widetag](where);
        switch (widetag) {
        case FUNCALLABLE_INSTANCE_WIDETAG:
            // Special note on the word at where[1] in funcallable instances:
            // - If no immobile code, then the word points to read-only space,
            ///  hence needs no adjustment.
            // - Otherwise, the word might point to a relocated range,
            //   either the instance itself, or a trampoline in immobile space.
            adjust_word_at(where+1, adj);
            /* FALLTHROUGH */
        case INSTANCE_WIDETAG:
            layout = layout_of(where);
            adjusted_layout = adjust_word(adj, layout);
            // writeback the layout if it changed. The layout is not a tagged slot
            // so it would not be fixed up otherwise.
            if (adjusted_layout != layout) layout_of(where) = adjusted_layout;
            struct bitmap bitmap = get_layout_bitmap(LAYOUT(adjusted_layout));
            lispobj* slots = where+1;
            for (i=0; i<(nwords-1); ++i)
                if (bitmap_logbitp(i, bitmap)) adjust_pointers(slots+i, 1, adj);
            continue;
#ifdef LISP_FEATURE_COMPACT_SYMBOL
          case SYMBOL_WIDETAG:
            { // Copied from scav_symbol() in gc-common
            struct symbol* s = (void*)where;
            adjust_pointers(&s->value, 2, adj);
            lispobj name = decode_symbol_name(s->name);
            lispobj adjusted_name = adjust_word(adj, name);
            // writeback the name if it changed
            if (adjusted_name != name) set_symbol_name(s, adjusted_name);
            int indicated_nwords = (*where>>N_WIDETAG_BITS) & 0xFF;
            adjust_pointers(&s->fdefn, indicated_nwords - 4, adj);
            }
            continue;
#endif
        case FDEFN_WIDETAG:
            adjust_pointers(where+1, 2, adj);
            // For most architectures, 'raw_addr' doesn't satisfy is_lisp_pointer()
            // so adjust_pointers() would ignore it. Therefore we need to
            // forcibly adjust it. This is correct whether or not there are tag bits.
            adjust_word_at(where+3, adj);
            continue;
        case CODE_HEADER_WIDETAG:
            if (filler_obj_p(where)) {
                // OMGWTF! Why does a filler code object merit adjustment?
                if (where[2]) adjust_word_at(where+2, adj);
                continue;
            }
            // Fixup the constant pool. The word at where+1 is a fixnum.
            code = (struct code*)where;
            adjust_pointers(where+2, code_header_words(code)-2, adj);
#ifdef LISP_FEATURE_UNTAGGED_FDEFNS
            // Process each untagged fdefn pointer.
            lispobj* fdefns_start = code->constants + code_n_funs(code)
              * CODE_SLOTS_PER_SIMPLE_FUN;
            int i;
            for (i=code_n_named_calls(code)-1; i>=0; --i)
                adjust_word_at(fdefns_start+i, adj);
#endif
#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64 || \
    defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64
            // Fixup absolute jump table
            lispobj* jump_table = code_jumptable_start(code);
            int count = jumptable_count(jump_table);
            for (i = 1; i < count; ++i) adjust_word_at(jump_table+i, adj);
#endif
            // Fixup all embedded simple-funs
            for_each_simple_fun(i, f, code, 1, {
                fix_fun_header_layout((lispobj*)f, adj);
#if FUN_SELF_FIXNUM_TAGGED
                if (f->self != (lispobj)f->insts)
                    FIXUP(f->self = (lispobj)f->insts, &f->self);
#else
                adjust_pointers(&f->self, 1, adj);
#endif
            });
            {
              // Now that the packed integer comprising the list of fixup locations
              // has been fixed-up (if necessary), apply them to the code.
              lispobj original_vaddr = inverse_adjust(adj, (lispobj)code);
              // code->fixups, if a bignum pointer, was fixed up as part of
              // the constant pool.
              gencgc_apply_code_fixups((struct code*)original_vaddr, code);
              adjust_code_refs(adj, code, original_vaddr);
            }
            continue;
        case CLOSURE_WIDETAG:
            fix_fun_header_layout(where, adj);
#if FUN_SELF_FIXNUM_TAGGED
            // For x86[-64], arm64, the closure fun appears to be a fixnum,
            // and might need adjustment unless pointing to immobile code.
            // Then fall into the general case; where[1] won't get re-adjusted
            // because it doesn't satisfy is_lisp_pointer().
            adjust_word_at(where+1, adj);
#endif
            break;
        // Vectors require extra care because of address-based hashing.
        case SIMPLE_VECTOR_WIDETAG:
          if (vector_flagp(*where, VectorAddrHashing)) {
              struct vector* v = (struct vector*)where;
              // If you could make a hash-table vector with space for exactly 1 k/v pair,
              // it would have length 5.
              gc_assert(vector_len(v) >= 5); // KLUDGE: need a manifest constant for fixed overhead
              lispobj* data = (lispobj*)v->data;
              adjust_pointers(&data[vector_len(v)-1], 1, adj);
              int hwm = KV_PAIRS_HIGH_WATER_MARK(data);
              boolean needs_rehash = 0;
              lispobj *where = &data[2], *end = &data[2*(hwm+1)];
              // Adjust the elements, checking for need to rehash.
              for ( ; where < end ; where += 2) {
                  // Really we should use the hash values to figure out which
                  // keys were address-sensitive. This simply overapproximates
                  // by assuming that any change forces rehash.
                  // (Similar issue exists in 'fixup_space' in immobile-space.c)
                  lispobj ptr = *where; // key
                  if (is_lisp_pointer(ptr) && (delta = calc_adjustment(adj, ptr)) != 0) {
                      FIXUP(*where = ptr + delta, where);
                      needs_rehash = 1;
                  }
                  ptr = where[1]; // value
                  if (is_lisp_pointer(ptr) && (delta = calc_adjustment(adj, ptr)) != 0)
                      FIXUP(where[1] = ptr + delta, where+1);
              }
              if (needs_rehash) // set v->data[1], the need-to-rehash bit
                  KV_PAIRS_REHASH(data) |= make_fixnum(1);
              continue;
          }
        // All the array header widetags.
        case SIMPLE_ARRAY_WIDETAG:
#ifdef COMPLEX_CHARACTER_STRING_WIDETAG
        case COMPLEX_CHARACTER_STRING_WIDETAG:
#endif
        case COMPLEX_BASE_STRING_WIDETAG:
        case COMPLEX_BIT_VECTOR_WIDETAG:
        case COMPLEX_VECTOR_WIDETAG:
        case COMPLEX_ARRAY_WIDETAG:
        // And the rest of the purely descriptor objects.
#ifndef LISP_FEATURE_COMPACT_SYMBOL
        case SYMBOL_WIDETAG:
#endif
        case VALUE_CELL_WIDETAG:
        case WEAK_POINTER_WIDETAG:
        case RATIO_WIDETAG:
        case COMPLEX_WIDETAG:
            break;

        // Other
        case SAP_WIDETAG:
            if ((delta = calc_adjustment(adj, where[1])) != 0) {
                fprintf(stderr,
                        "WARNING: SAP at %p -> %p in relocatable core\n",
                        where, (void*)where[1]);
                FIXUP(where[1] += delta, where+1);
            }
            continue;
        case BIGNUM_WIDETAG:
#ifndef LISP_FEATURE_64_BIT
        case SINGLE_FLOAT_WIDETAG:
#endif
        case DOUBLE_FLOAT_WIDETAG:
        case COMPLEX_SINGLE_FLOAT_WIDETAG:
        case COMPLEX_DOUBLE_FLOAT_WIDETAG:
#ifdef SIMD_PACK_WIDETAG
        case SIMD_PACK_WIDETAG:
#endif
#ifdef SIMD_PACK_256_WIDETAG
        case SIMD_PACK_256_WIDETAG:
#endif
            continue;
        default:
          if (other_immediate_lowtag_p(widetag)
              && specialized_vector_widetag_p(widetag))
              continue;
          else
              lose("Unrecognized heap object: @%p: %"OBJ_FMTX, where, *where);
        }
        adjust_pointers(where+1, nwords-1, adj);
    }
#if SHOW_SPACE_RELOCATION
    fprintf(stderr, "space @ %p: fixed %d absolute + %d relative pointers\n",
            (lispobj*)start, adj->n_relocs_abs, adj->n_relocs_rel);
#endif
}

static void relocate_heap(struct heap_adjust* adj)
{
    if (!lisp_startup_options.noinform && SHOW_SPACE_RELOCATION) {
        int i;
        for (i = 0; i < adj->n_ranges; ++i)
            if (adj->range[i].delta)
                fprintf(stderr, "NOTE: Relocating [%p:%p] into [%p:%p]\n",
                        (char*)adj->range[i].start,
                        (char*)adj->range[i].end,
                        (char*)adj->range[i].start + adj->range[i].delta,
                        (char*)adj->range[i].end + adj->range[i].delta);
    }
    relocate_space(NIL_SYMBOL_SLOTS_START, (lispobj*)NIL_SYMBOL_SLOTS_END, adj);
    relocate_space(STATIC_SPACE_OBJECTS_START, static_space_free_pointer, adj);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    relocate_space(FIXEDOBJ_SPACE_START, fixedobj_free_pointer, adj);
#endif
#ifdef LISP_FEATURE_CHENEYGC
    relocate_space(DYNAMIC_0_SPACE_START, (lispobj*)get_alloc_pointer(), adj);
#else
    relocate_space(DYNAMIC_SPACE_START, (lispobj*)get_alloc_pointer(), adj);
#endif
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    // Pointers within varyobj space to varyobj space do not need adjustment
    // so remove any delta before performing the relocation pass on this space.
    // FIXME: this was probably for PIE mode, which doesn't work.
    if (lisp_code_in_elf() && adj->range[2].delta != 0) {
        lose("code-in-elf + PIE not supported yet\n");
        adj->range[2].delta = 0; // FIXME: isn't this this already the case?
    }
    relocate_space(VARYOBJ_SPACE_START, varyobj_free_pointer, adj);
#endif
}

static void
set_adjustment(struct heap_adjust* adj,
               uword_t actual_addr,
               uword_t desired_addr,
               uword_t len)
{
    int j = adj->n_ranges;
    gc_assert(j <= 2);
    adj->range[j].start = (lispobj)desired_addr;
    adj->range[j].end   = (lispobj)desired_addr + len;
    adj->range[j].delta = len ? actual_addr - desired_addr : 0;
    adj->n_ranges = j+1;
}

#if defined(LISP_FEATURE_ELF) && defined(LISP_FEATURE_IMMOBILE_SPACE)
    extern int apply_pie_relocs(long,long,int);
#else
#   define apply_pie_relocs(dummy1,dummy2,dummy3) (0)
#endif

/// Compute the bounds of the lisp assembly routine code object
void calc_asm_routine_bounds()
{
#ifdef LISP_FEATURE_METASPACE
    if (widetag_of((lispobj*)READ_ONLY_SPACE_START) == CODE_HEADER_WIDETAG)
        asm_routines_start = READ_ONLY_SPACE_START;
    else
        asm_routines_start = READ_ONLY_SPACE_START + (256+2)*N_WORD_BYTES;
#elif defined LISP_FEATURE_IMMOBILE_CODE
    asm_routines_start = VARYOBJ_SPACE_START;
#else
    if (widetag_of((lispobj*)READ_ONLY_SPACE_START) == CODE_HEADER_WIDETAG) {
        asm_routines_start = READ_ONLY_SPACE_START;
    } else {
        lispobj *where = (lispobj*)STATIC_SPACE_OBJECTS_START;
        for (; where < static_space_free_pointer; where += OBJECT_SIZE(*where, where))
            if (widetag_of((lispobj*)where) == CODE_HEADER_WIDETAG) {
                asm_routines_start = (uword_t)where;
                break;
            }
        if (!asm_routines_start) lose("Can't find asm routines");
    }
#endif
    asm_routines_end = asm_routines_start +
      N_WORD_BYTES * sizetab[CODE_HEADER_WIDETAG]((lispobj*)asm_routines_start);
}

#ifdef LISP_FEATURE_IMMOBILE_SPACE
void calc_immobile_space_bounds()
{
    /* Suppose we have:
     *   A               B                             C                D
     *   | varyobj space | .... other random stuff ... | fixedobj space | ...
     * then the lower bound is A, the upper bound is D,
     * the max_offset is the distance from A to D,
     * and the excluded middle is the range spanned by B to C.
     */
    struct range {
        uword_t start, end;
    };
    struct range range1 =
        {FIXEDOBJ_SPACE_START, FIXEDOBJ_SPACE_START + FIXEDOBJ_SPACE_SIZE};
    struct range range2 =
        {VARYOBJ_SPACE_START, VARYOBJ_SPACE_START + varyobj_space_size};
    if (range2.start < range1.start) { // swap
        struct range temp = range1;
        range1 = range2;
        range2 = temp;
    }
    immobile_space_lower_bound  = range1.start;
    immobile_space_max_offset   = range2.end - range1.start;
    immobile_range_1_max_offset = range1.end - range1.start;
    immobile_range_2_min_offset = range2.start - range1.start;
}
#endif

__attribute__((unused)) static void check_dynamic_space_addr_ok(uword_t start, uword_t size)
{
#ifdef LISP_FEATURE_64_BIT // don't want a -Woverflow warning on 32-bit
    uword_t end_word_addr = start + size - N_WORD_BYTES;
    // Word-aligned pointers can't address more than 48 significant bits for now.
    // If you want to lift that restriction, look at how SYMBOL-PACKAGE and
    // SYMBOL-NAME are combined into one lispword.
    uword_t unaddressable_bits = 0xFFFF000000000000;
    if ((start & unaddressable_bits) || (end_word_addr & unaddressable_bits))
        lose("Panic! This version of SBCL can not address memory\n"
             "in the range %p:%p given by the OS.\nPlease report this as a bug.",
             (void*)start, (void*)(start + size));
#endif
}

/* TODO: If static + readonly were mapped as desired without disabling ASLR
 * but one of the large spaces couldn't be mapped as desired, start over from
 * the top, disabling ASLR. This should help to avoid relocating the heap
 * if at all possible. It might make sense to parse the core header sooner in
 * startup to avoid wasting time on all actions performed prior to re-exec.
 */

static void
process_directory(int count, struct ndir_entry *entry,
                  int fd, os_vm_offset_t file_offset,
                  int __attribute__((unused)) merge_core_pages,
                  struct heap_adjust __attribute__((unused)) *adj)
{
    extern void immobile_space_coreparse(uword_t,uword_t);

    struct {
        size_t desired_size; // size wanted, ORed with 1 if addr must be <2GB
        // Values from the core file:
        uword_t len; // length in bytes, as an integral multiple of os_vm_page_size
        uword_t base;
        lispobj** pfree_pointer; // pointer to x_free_pointer
    } spaces[MAX_CORE_SPACE_ID+1] = {
        {0, 0, 0, 0}, // blank for space ID 0
#ifdef LISP_FEATURE_GENCGC
        {dynamic_space_size, 0, DYNAMIC_SPACE_START, 0},
#else
        // Whatever address the core's dynamic space has
        // becomes subspace 0. The other is subspace 1.
        // It makes no difference which has the lower address.
        {dynamic_space_size, 0, DYNAMIC_0_SPACE_START, 0},
#endif
        // This order is determined by constants in compiler/generic/genesis
        {0, 0, STATIC_SPACE_START, &static_space_free_pointer},

        {0, 0, READ_ONLY_SPACE_START, &read_only_space_free_pointer},

#ifdef LISP_FEATURE_DARWIN_JIT
        {0, 0, STATIC_CODE_SPACE_START, &static_code_space_free_pointer},
#endif

#ifdef LISP_FEATURE_IMMOBILE_SPACE
        {FIXEDOBJ_SPACE_SIZE | 1, 0,
            FIXEDOBJ_SPACE_START, &fixedobj_free_pointer},
        {1, 0, VARYOBJ_SPACE_START, &varyobj_free_pointer}
#endif
    };

#if ELFCORE
    if (&lisp_code_start) {
        VARYOBJ_SPACE_START = (uword_t)&lisp_code_start;
        varyobj_free_pointer = &lisp_jit_code;
        varyobj_space_size = (uword_t)&lisp_code_end - VARYOBJ_SPACE_START;
        spaces[IMMOBILE_VARYOBJ_CORE_SPACE_ID].len = varyobj_space_size;
        if (varyobj_free_pointer < (lispobj*)VARYOBJ_SPACE_START
            || !PTR_IS_ALIGNED(&lisp_code_end, 4096))
            lose("ELF core alignment bug. Check for proper padding in 'editcore'");
#ifdef DEBUG_COREPARSE
        printf("Lisp code present in executable @ %lx:%lx (freeptr=%p)\n",
               (uword_t)&lisp_code_start, (uword_t)&lisp_code_end,
               varyobj_free_pointer);
#endif
        // Prefill the Lisp linkage table so that shrinkwrapped executables which link in
        // all their C library dependencies can avoid linking with -ldl.
        // All data references are potentially needed because aliencomp doesn't emit
        // SAP-REF-n in a way that admits elision of the linkage entry. e.g.
        //     MOV RAX, [#x20200AA0] ; some_c_symbol
        //     MOV RAX, [RAX]
        // might be rendered as
        //     MOV RAX, some_c_symbol(%rip)
        // but that's more of a change to the asm instructions than I'm comfortable making;
        // whereas "CALL linkage_entry_for_f" -> "CALL f" is quite straightforward.
        // (Rarely would a jmp indirection be used; maybe for newly compiled code?)
        lispobj* ptr = &lisp_linkage_values;
        gc_assert(ptr);
        int entry_index = 0;
        int count;
        extern int lisp_linkage_table_n_prelinked;
        count = lisp_linkage_table_n_prelinked = *ptr++;
        for ( ; count-- ; entry_index++ ) {
            boolean datap = *ptr == (lispobj)-1; // -1 can't be a function address
            if (datap)
                ++ptr;
            arch_write_linkage_table_entry(entry_index, (void*)*ptr++, datap);
        }

        // unprotect the pages
        os_protect((void*)VARYOBJ_SPACE_START, varyobj_space_size, OS_VM_PROT_ALL);
    } else
#endif
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    {
        spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].desired_size += VARYOBJ_SPACE_SIZE;
    }
#endif

    for ( ; --count>= 0; ++entry) {
        long id = entry->identifier;
        uword_t addr = entry->address;
#ifdef DEBUG_COREPARSE
        printf("space %d @ %10lx pg=%4d+%4d nwords=%ld\n",
               (int)id, addr, (int)entry->data_page, (int)entry->page_count,
               entry->nwords);
#endif
        int compressed = id & DEFLATED_CORE_SPACE_ID_FLAG;
        id -= compressed;
        if (id < 1 || id > MAX_CORE_SPACE_ID)
            lose("unknown space ID %ld addr %p", id, (void*)addr);

#ifdef LISP_FEATURE_IMMOBILE_SPACE
        // Enforce address of readonly, static, immobile varyobj
        int enforce_address = id != DYNAMIC_CORE_SPACE_ID
          && id != IMMOBILE_FIXEDOBJ_CORE_SPACE_ID
          && id != IMMOBILE_VARYOBJ_CORE_SPACE_ID;
#else
        // Enforce address of readonly and static spaces.
        int enforce_address = id != DYNAMIC_CORE_SPACE_ID;
#endif

        // We'd like to enforce proper alignment of 'addr' but there's
        // a problem: dynamic space has a stricter requirement (usually 32K)
        // than code space (4K). So don't assert the alignment.
        if (enforce_address) {
            int fail;
            if ((fail = (addr != spaces[id].base)) != 0)
                fprintf(stderr, "in core: %p; in runtime: %p\n",
                        (void*)addr, (void*)spaces[id].base);
            char *names[] = {
              "DYNAMIC", "STATIC", "READ_ONLY", "IMMOBILE", "IMMOBILE"
            };
            if (fail)
                lose("core/runtime address mismatch: %s_SPACE_START", names[id-1]);
        }
        spaces[id].base = addr;
        uword_t len = os_vm_page_size * entry->page_count;
        if (id == DYNAMIC_CORE_SPACE_ID && len > dynamic_space_size) {
            lose("dynamic space too small for core: %luKiB required, %luKiB available.",
                 (unsigned long)len >> 10,
                 (unsigned long)dynamic_space_size >> 10);
        }
        if (len != 0) {
            spaces[id].len = len;
            // Try to map at address requested by the core file.
            size_t request = spaces[id].desired_size;
            int sub_2gb_flag = (request & 1);
            request &= ~(size_t)1;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
            if (id == IMMOBILE_VARYOBJ_CORE_SPACE_ID)
                // Pretend an os_validate() happened based on the address that
                // would be obtained by a constant offset from fixedobj space
                addr = FIXEDOBJ_SPACE_START + FIXEDOBJ_SPACE_SIZE;
            else
#endif
            if (request) {
#ifdef LISP_FEATURE_WIN32
                if (id == DYNAMIC_CORE_SPACE_ID) {
                    addr = (uword_t)os_validate_nocommit(sub_2gb_flag ? MOVABLE_LOW : MOVABLE,
                                                         (os_vm_address_t)addr, request);
                }
                else
#endif
                {
                    addr = (uword_t)os_validate(sub_2gb_flag ? MOVABLE_LOW : MOVABLE,
                                                (os_vm_address_t)addr, request,
                                                id == READ_ONLY_CORE_SPACE_ID,
                                                id == DYNAMIC_CORE_SPACE_ID);
                }
                if (!addr) {
                    lose("Can't allocate %#"OBJ_FMTX" bytes for space %ld",
                         (lispobj)request, id);
                }
            }
            switch (id) {
#ifdef LISP_FEATURE_IMMOBILE_SPACE
            case IMMOBILE_FIXEDOBJ_CORE_SPACE_ID:
            case IMMOBILE_VARYOBJ_CORE_SPACE_ID:
                if (addr + request > 0x80000000)
                    lose("Won't map immobile space above 2GB");
                if (id == IMMOBILE_FIXEDOBJ_CORE_SPACE_ID)
                    FIXEDOBJ_SPACE_START = addr;
                else
                    VARYOBJ_SPACE_START = addr;
                break;
#endif
            case DYNAMIC_CORE_SPACE_ID:
#ifdef LISP_FEATURE_CHENEYGC
                {
                  uword_t semispace_0_start = ALIGN_UP(addr, BACKEND_PAGE_BYTES);
                  uword_t semispace_0_end = ALIGN_DOWN(addr + request, BACKEND_PAGE_BYTES);
                  // assign to 'addr' too, because that's where we load the core file
                  DYNAMIC_0_SPACE_START = addr = semispace_0_start;
                  current_dynamic_space = (lispobj*)addr;
                  // Request that much again now
                  uword_t addr1 = (uword_t)os_validate(MOVABLE, 0, request, 1, 0);
                  uword_t semispace_1_start = ALIGN_UP(addr1, BACKEND_PAGE_BYTES);
                  uword_t semispace_1_end = ALIGN_DOWN(addr1 + request, BACKEND_PAGE_BYTES);

                  DYNAMIC_1_SPACE_START = semispace_1_start;
                  uword_t semispace_0_size = semispace_0_end - semispace_0_start;
                  uword_t semispace_1_size = semispace_1_end - semispace_1_start;
                  dynamic_space_size =
                      semispace_0_size < semispace_1_size ? semispace_0_size : semispace_1_size;
                }
#else /* gencgc */
                {
                uword_t aligned_start = ALIGN_UP(addr, GENCGC_PAGE_BYTES);
                /* Misalignment can happen only if card size exceeds OS page.
                 * Drop one card to avoid overrunning the allocated space */
                if (aligned_start > addr) // not card-aligned
                    dynamic_space_size -= GENCGC_PAGE_BYTES;
                DYNAMIC_SPACE_START = addr = aligned_start;
                check_dynamic_space_addr_ok(addr, dynamic_space_size);
                }
#endif
                break;
            }

            sword_t offset = os_vm_page_size * (1 + entry->data_page);
            if (compressed) {
#ifdef LISP_FEATURE_DARWIN_JIT
                if (id == READ_ONLY_CORE_SPACE_ID)
                    os_protect((os_vm_address_t)addr, len, OS_VM_PROT_WRITE);
#endif
                inflate_core_bytes(fd, offset + file_offset, (os_vm_address_t)addr, len);

#ifdef LISP_FEATURE_DARWIN_JIT
                if (id == READ_ONLY_CORE_SPACE_ID)
                    os_protect((os_vm_address_t)addr, len, OS_VM_PROT_READ | OS_VM_PROT_EXECUTE);
#endif

            }
            else
#ifdef LISP_FEATURE_DARWIN_JIT
            if (id == DYNAMIC_CORE_SPACE_ID || id == STATIC_CODE_CORE_SPACE_ID) {
                load_core_bytes_jit(fd, offset + file_offset, (os_vm_address_t)addr, len);
            } else
#endif
              {
                load_core_bytes(fd, offset + file_offset, (os_vm_address_t)addr, len, id == READ_ONLY_CORE_SPACE_ID);
            }
        }

#ifdef MADV_MERGEABLE
        if ((merge_core_pages == 1)
            || ((merge_core_pages == -1) && compressed)) {
            madvise((void *)addr, len, MADV_MERGEABLE);
        }
#endif

        lispobj *free_pointer = (lispobj *) addr + entry->nwords;
        switch (id) {
        default:
            // varyobj free ptr is already nonzero if Lisp code in executable
            if (!*spaces[id].pfree_pointer)
                *spaces[id].pfree_pointer = free_pointer;
            break;
        case DYNAMIC_CORE_SPACE_ID:
#ifdef LISP_FEATURE_CHENEYGC
            /* 'addr' is the actual address if relocatable.
             * For cheneygc, this will be whatever the GC was using
             * at the time the core was saved.
             * For gencgc this is #defined as DYNAMIC_SPACE_START */
            current_dynamic_space = (lispobj *)addr;
#endif
            set_alloc_pointer((lispobj)free_pointer);

            anon_dynamic_space_start = (os_vm_address_t)(addr + len);
        }
    }

    calc_asm_routine_bounds();
#  ifdef LISP_FEATURE_GENCGC
    set_adjustment(adj, DYNAMIC_SPACE_START, // actual
                   spaces[DYNAMIC_CORE_SPACE_ID].base, // expected
                   spaces[DYNAMIC_CORE_SPACE_ID].len);
#    ifdef LISP_FEATURE_IMMOBILE_SPACE
    set_adjustment(adj, FIXEDOBJ_SPACE_START, // actual
                   spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].base, // expected
                   spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].len);
    if (!apply_pie_relocs(VARYOBJ_SPACE_START
                          - spaces[IMMOBILE_VARYOBJ_CORE_SPACE_ID].base,
                          DYNAMIC_SPACE_START - spaces[DYNAMIC_CORE_SPACE_ID].base,
                          fd))
        set_adjustment(adj, VARYOBJ_SPACE_START, // actual
                       spaces[IMMOBILE_VARYOBJ_CORE_SPACE_ID].base, // expected
                       spaces[IMMOBILE_VARYOBJ_CORE_SPACE_ID].len);
#    endif
#  else
    set_adjustment(adj, DYNAMIC_0_SPACE_START, // actual
                   spaces[DYNAMIC_CORE_SPACE_ID].base, // expected
                   spaces[DYNAMIC_CORE_SPACE_ID].len);
#  endif // LISP_FEATURE_GENCGC
    if (adj->range[0].delta | adj->range[1].delta | adj->range[2].delta) {
        relocate_heap(adj);
    }

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    /* Now determine page characteristics (such as object spacing)
     * after relocation, because we need to know which objects are layouts
     * based on knowing layout-of-layout.  The test for that is dependent
     * on what it's address should be, not what it was in the file */
    immobile_space_coreparse(spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].len,
                             spaces[IMMOBILE_VARYOBJ_CORE_SPACE_ID].len);
    calc_immobile_space_bounds();
#endif
#ifdef LISP_FEATURE_X86_64
    tune_asm_routines_for_microarch(); // before WPing immobile space
#endif
#ifdef LISP_FEATURE_DARWIN_JIT
    if (!static_code_space_free_pointer)
        static_code_space_free_pointer = (lispobj *)STATIC_CODE_SPACE_START;
#endif
}

#ifdef LISP_FEATURE_GENCGC
extern void gc_load_corefile_ptes(int, core_entry_elt_t, core_entry_elt_t,
                                  os_vm_offset_t offset, int fd);
#else
#define gc_load_corefile_ptes(dummy1,dummy2,dummy3,dummy4,dummy5)
#endif

static void sanity_check_loaded_core(lispobj);

/* 'merge_core_pages': Tri-state flag to determine whether we attempt to mark
 * pages as targets for virtual memory deduplication via MADV_MERGEABLE.
 * 1: Yes
 * 0: No
 * -1: default, yes for compressed cores, no otherwise.
 */
lispobj
load_core_file(char *file, os_vm_offset_t file_offset, int merge_core_pages)
{
    void *header;
    core_entry_elt_t val, *ptr;
    os_vm_size_t len, remaining_len, stringlen;
    int fd = open_binary(file, O_RDONLY);
    ssize_t count;
    lispobj initial_function = NIL;
    struct heap_adjust adj;
    memset(&adj, 0, sizeof adj);

    if (fd < 0) {
        fprintf(stderr, "could not open file \"%s\"\n", file);
        perror("open");
        exit(1);
    }

    lseek(fd, file_offset, SEEK_SET);
    header = calloc(os_vm_page_size, 1);

    count = read(fd, header, os_vm_page_size);
    if (count < (ssize_t) os_vm_page_size) {
        lose("premature end of core file");
    }

    ptr = header;
    val = *ptr++;

    if (val != CORE_MAGIC)
        lose("invalid magic number in core: %"OBJ_FMTX" should have been %x",
             (lispobj)val, CORE_MAGIC);

    for ( ; ; ptr += remaining_len) {
        val = *ptr++;
        len = *ptr++;
        remaining_len = len - 2; /* (-2 to cancel the two ++ operations) */
        switch (val) {
        case BUILD_ID_CORE_ENTRY_TYPE_CODE:
            stringlen = *ptr++;
            --remaining_len;
            gc_assert(remaining_len * sizeof (core_entry_elt_t) >= stringlen);
            if (stringlen+1 != sizeof build_id || memcmp(ptr, build_id, stringlen))
                lose("core was built for runtime \"%.*s\" but this is \"%s\"",
                     (int)stringlen, (char*)ptr, build_id);
            break;
        case DIRECTORY_CORE_ENTRY_TYPE_CODE:
            process_directory(remaining_len / NDIR_ENTRY_LENGTH,
                              (struct ndir_entry*)ptr, fd, file_offset,
                              merge_core_pages, &adj);
            break;
        case PAGE_TABLE_CORE_ENTRY_TYPE_CODE:
            gc_load_corefile_ptes(ptr[0], ptr[1], ptr[2],
                                  file_offset + (ptr[3] + 1) * os_vm_page_size, fd);
            break;
        case INITIAL_FUN_CORE_ENTRY_TYPE_CODE:
            initial_function = adjust_word(&adj, (lispobj)*ptr);
            break;
        case END_CORE_ENTRY_TYPE_CODE:
            free(header);
            close(fd);
#ifdef LISP_FEATURE_SB_THREAD
            if ((int)SymbolValue(FREE_TLS_INDEX,0) >= dynamic_values_bytes) {
                dynamic_values_bytes = (int)SymbolValue(FREE_TLS_INDEX,0) * 2;
                // fprintf(stderr, "NOTE: TLS size increased to %x\n", dynamic_values_bytes);
            }
#endif
#ifdef LISP_FEATURE_GENCGC
            if (widetag_of(native_pointer(initial_function)) == SIMPLE_FUN_WIDETAG
                && !lisp_startup_options.noinform) {
                fprintf(stderr, "Initial page table:\n");
                extern void print_generation_stats(void);
                print_generation_stats();
            }
#endif
            sanity_check_loaded_core(initial_function);
            return initial_function;
        case RUNTIME_OPTIONS_MAGIC: break; // already processed
        default:
            lose("unknown core header entry: %"OBJ_FMTX, (lispobj)val);
        }
    }
}

#include "genesis/hash-table.h"
#include "genesis/vector.h"
#include "genesis/cons.h"
char* get_asm_routine_by_name(const char* name, int *index)
{
    struct code* code = (struct code*)asm_routines_start;
    lispobj ht = CONS(code->debug_info)->car;
    if (ht) {
        struct vector* table =
            VECTOR(((struct hash_table*)native_pointer(ht))->pairs);
        lispobj sym;
        int i;
        // ASSUMPTION: hash-table representation is known (same as in gc-common of course)
        for (i=2 ; i < vector_len(table) ; i += 2)
            if (lowtag_of(sym = table->data[i]) == OTHER_POINTER_LOWTAG
                && widetag_of(&SYMBOL(sym)->header) == SYMBOL_WIDETAG
                && !strcmp(name, (char*)(symbol_name(SYMBOL(sym))->data))) {
                lispobj value = table->data[i+1];
                // value = (start-address . (end-address . index))
                if (index)
                  *index = fixnum_value(CONS(CONS(value)->cdr)->cdr); // take cddr
                return code_text_start(code) + fixnum_value(CONS(value)->car);
            }
        // Something is wrong if we have a hashtable but find nothing.
        fprintf(stderr, "WARNING: get_asm_routine_by_name(%s) failed\n",
                name);
    }
    if (index) *index = 0;
    return NULL;
}

void asm_routine_poke(const char* routine, int offset, char byte)
{
    char *address = get_asm_routine_by_name(routine, 0);
    if (address)
        address[offset] = byte;
}

// Caution: use at your own risk
#if defined DEBUG_CORE_LOADING && DEBUG_CORE_LOADING
#include "hopscotch.h"
#include "genesis/cons.h"
#include "genesis/layout.h"
#include "genesis/gc-tables.h"
#include "code.h"
#include "gc-private.h"

struct visitor {
    // one item per value of widetag>>2
    // element 0 is for conses, element 64 is for totals.
    struct {
        int count;
        int words;
    } headers[65], sv_subtypes[3];
    struct hopscotch_table *reached;
};

static void trace_sym(lispobj, struct symbol*, struct hopscotch_table*);
#define RECURSE(x) if(is_lisp_pointer(x))graph_visit(ptr,x,seen)
static void graph_visit(lispobj __attribute__((unused)) referer,
                        lispobj ptr,
                        struct hopscotch_table* seen)
{
    if (lowtag_of(ptr) == FUN_POINTER_LOWTAG
        && widetag_of(FUNCTION(ptr)) == SIMPLE_FUN_WIDETAG)
        ptr = fun_code_tagged(FUNCTION(ptr));
    if (hopscotch_get(seen, ptr, 0))
        return;
    hopscotch_insert(seen, ptr, 1);
    lispobj layout, *obj;
    int nwords, i;
    if (lowtag_of(ptr) == LIST_POINTER_LOWTAG) {
        RECURSE(CONS(ptr)->car);
        RECURSE(CONS(ptr)->cdr);
    } else switch (widetag_of(obj = native_pointer(ptr))) {
        case SIMPLE_VECTOR_WIDETAG:
            {
            struct vector* v = (void*)obj;
            sword_t len = vector_len(v);
            for(i=0; i<len; ++i) RECURSE(v->data[i]);
            }
            break;
        case INSTANCE_WIDETAG:
        case FUNCALLABLE_INSTANCE_WIDETAG:
            layout = layout_of(obj);
            graph_visit(ptr, layout, seen);
            nwords = sizetab[widetag_of(obj)](obj);
            struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
            for (i=0; i<(nwords-1); ++i)
                if (bitmap_logbitp(i, bitmap)) RECURSE(obj[1+i]);
            break;
        case CODE_HEADER_WIDETAG:
            nwords = code_header_words((struct code*)obj);
            for(i=2; i<nwords; ++i) RECURSE(obj[i]);
            break;
        // In all the remaining cases, 'nwords' is the count of payload words
        // (following the header), so we iterate up to and including that
        // word index. For example, if there are 2 payload words,
        // then we scan word indices 1 and 2 off the object base address.
        case CLOSURE_WIDETAG:
            // We must scan the closure's trampoline word.
            graph_visit(ptr, fun_taggedptr_from_self(obj[1]), seen);
            // Closures can utilize one payload word beyond what the header
            // indicates. This is quite sucky and I don't know why I did that.
            // However, it is correctly accounted for by SHORT_BOXED_NWORDS
            // which gives you the right number of words to scan.
            nwords = SHORT_BOXED_NWORDS(*obj);
            for(i=2; i<=nwords; ++i) RECURSE(obj[i]);
            break;
        case SYMBOL_WIDETAG:
            trace_sym(ptr, SYMBOL(ptr), seen);
            break;
        case WEAK_POINTER_WIDETAG:
            nwords = TINY_BOXED_NWORDS(*obj);
            for(i=1; i<=nwords; ++i) RECURSE(obj[i]);
            break;
        case FDEFN_WIDETAG:
            RECURSE(obj[1]);
            RECURSE(obj[2]);
            RECURSE(fdefn_callee_lispobj((struct fdefn*)obj));
            break;
        default:
            if (!leaf_obj_widetag_p(widetag_of(obj))) {
                int size = sizetab[widetag_of(obj)](obj);
                for(i=1; i<size; ++i) RECURSE(obj[i]);
            }
      }
}
static void trace_sym(lispobj ptr, struct symbol* sym, struct hopscotch_table* seen)
{
    RECURSE(decode_symbol_name(sym->name));
    RECURSE(sym->value);
    RECURSE(sym->info);
    RECURSE(sym->fdefn);
    int indicated_nwords = HeaderValue(sym->header) & 0xFF;
    if (indicated_nwords + 1 > SYMBOL_SIZE) // has one more slot with no name
        RECURSE(1[&sym->fdefn]); // ASSUMPTION: slot order
}

static void tally(lispobj ptr, struct visitor* v)
{
    if (lowtag_of(ptr) == LIST_POINTER_LOWTAG)
        ++v->headers[0].count;
    else {
        lispobj* obj = native_pointer(ptr);
        lispobj header = *obj;
        int widetag = header_widetag(header);
        int header_index = widetag>>2;
        int words = OBJECT_SIZE(header, obj);
        ++v->headers[header_index].count;
        v->headers[header_index].words += words;
        if (widetag == SIMPLE_VECTOR_WIDETAG) {
            int subtype = 0;
            if (vector_flagp(header, VectorHashing))
                subtype = 2;
            else if (vector_flagp(header, VectorWeak))
                subtype = 1;
            ++v->sv_subtypes[subtype].count;
            v->sv_subtypes[subtype].words += words;
        }
    }
}

/* This printing in here is useful, but it's too much to output in make-target-2,
 * because genesis dumps a ton of unreachable objects.
 * The reason is this: cold-load has no way of knowing if a literal loaded
 * from fasl and written to core is really supposed to be consumed by target.
 * e.g. if it's a string naming a foreign fixup, who reads that string?
 * Answer: The host, but it appears in the cold core as if the target will need it.
 * The only way to rectify this defect is just not worth doing - each literal
 * would have to be kept only as a host proxy until such time as we actually refer
 * to it from another object that is definitely in the cold core, via FOP-LOAD-CODE
 * and who know what else. Thus it remains a graph tracing problem in nature,
 * which is best left to GC */
static uword_t visit(lispobj* where, lispobj* limit, uword_t arg)
{
    struct visitor* v = (struct visitor*)arg;
    lispobj* obj = where;
    while (obj < limit) {
        lispobj ptr = compute_lispobj(obj);
        tally(ptr, v);
        if (!hopscotch_get(v->reached, ptr, 0)) printf("unreachable: %p\n", (void*)ptr);
        obj += OBJECT_SIZE(*obj, obj);
    }
    return 0;
}

#ifdef LISP_FEATURE_GENCGC
#define count_this_pointer_p(ptr) (find_page_index((void*)ptr) >= 0)
#endif
#ifdef LISP_FEATURE_CHENEYGC
#define count_this_pointer_p(ptr) (1)
#endif

static void sanity_check_loaded_core(lispobj initial_function)
{
    struct visitor v[2];
    struct hopscotch_table reached;
    memset(v, 0, sizeof v);
    // Pass 1: Count objects reachable from known roots.
    hopscotch_create(&reached, HOPSCOTCH_HASH_FUN_DEFAULT,
                     0, // no values
                     1<<18, /* initial size */
                     0);
    {
      trace_sym(NIL, SYMBOL(NIL), &reached);
      lispobj* where = (lispobj*)STATIC_SPACE_OBJECTS_START;
      lispobj* end = static_space_free_pointer;
      while (where<end) {
        graph_visit(0, compute_lispobj(where), &reached);
        where += OBJECT_SIZE(*where, where);
      }
    }
    graph_visit(0, initial_function, &reached); // not otherwise reachable
    // having computed the reaching graph, tally up the dynamic space objects
    int key_index;
    lispobj ptr;
    for_each_hopscotch_key(key_index, ptr, reached)
        if (count_this_pointer_p(ptr)) tally(ptr, &v[0]);
    // Pass 2: Count all heap objects
    v[1].reached = &reached;
#ifdef LISP_FEATURE_GENCGC
    walk_generation(visit, -1, (uword_t)&v[1]);
#endif
#ifdef LISP_FEATURE_CHENEYGC
    visit((lispobj*)READ_ONLY_SPACE_START, read_only_space_free_pointer, (uword_t)&v[1]);
    visit((lispobj*)STATIC_SPACE_START, static_space_free_pointer, (uword_t)&v[1]);
#endif

    // Pass 3: Compare
    // Start with the conses
    v[0].headers[0].words = v[0].headers[0].count * 2;
    v[1].headers[0].words = v[1].headers[0].count * 2;
    printf("-----------------------------------------------|\n");
    printf("       Graph walk     |         Actual         |\n");
    printf("----------------------+------------------------|\n");
    int i;
    for(i=0; i<=64; ++i) {
        // print all valid widetags (not unknown) that aren't for immediates,
        // but always print if nonzero.
        if (v[1].headers[i].count ||
            ((strncmp(widetag_names[i], "unk", 3)
              && (i != CHARACTER_WIDETAG>>2)
              && (i != SIMPLE_FUN_WIDETAG>>2)
              && (i != NO_TLS_VALUE_MARKER_WIDETAG>>2)
              && (i != UNBOUND_MARKER_WIDETAG>>2)))) {
            int mismatch = v[0].headers[i].count != v[1].headers[i].count;
            printf("%8d %11d  | %8d %11d   | %s%s\n",
                   v[0].headers[i].count, v[0].headers[i].words,
                   v[1].headers[i].count, v[1].headers[i].words,
                   i<64 ? (i ? widetag_names[i] : "cons") : "TOTAL",
                   mismatch ? " <<<<" : "");
            if (i == SIMPLE_VECTOR_WIDETAG>>2) {
                int j;
                for(j=1; j <= 2; ++j)
                    printf("%8d %11d  | %8d %11d   |   %s\n",
                           v[0].sv_subtypes[j].count, v[0].sv_subtypes[j].words,
                           v[1].sv_subtypes[j].count, v[1].sv_subtypes[j].words,
                           j==1 ? "weak" : "hashing");

            }
            v[0].headers[64].count += v[0].headers[i].count;
            v[1].headers[64].count += v[1].headers[i].count;
            v[0].headers[64].words += v[0].headers[i].words;
            v[1].headers[64].words += v[1].headers[i].words;
        }
    }
    hopscotch_destroy(&reached);
}
#else
static void sanity_check_loaded_core(lispobj __attribute__((unused)) initial_function) {}
#endif
