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
#include "code.h"
#include "graphvisit.h"

#include <errno.h>

#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
# include <zstd.h>
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
 lisp_code_start, lisp_jit_code, lisp_code_end, alien_linkage_values;
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
    lose("This runtime was not built with zstd-compressed core support... aborting")
#else
static void inflate_core_bytes(int fd, os_vm_offset_t offset,
                               os_vm_address_t addr, int len)
{
# ifdef LISP_FEATURE_WIN32
    /* Ensure the memory is committed so zstd doesn't segfault trying
       to inflate. */
    os_commit_memory(addr, len);
# endif
    if (-1 == lseek(fd, offset, SEEK_SET))
        lose("Unable to lseek() on corefile");

    int ret;
    size_t buf_size = ZSTD_DStreamInSize();
    unsigned char* buf = successful_malloc(buf_size);
    ZSTD_inBuffer input;
    input.src = buf;
    input.pos = 0;

    ZSTD_outBuffer output;
    output.dst = (void*)addr;
    output.size = len;
    output.pos = 0;

    ZSTD_DStream *stream = ZSTD_createDStream();
    if (stream == NULL)
        lose("unable to create zstd decompression context");
    ret = ZSTD_initDStream(stream);
    if (ZSTD_isError(ret))
        lose("ZSTD_initDStream failed with error: %s", ZSTD_getErrorName(ret));

    /* Read in exactly one frame. */
    do {
        ssize_t count = read(fd, buf, buf_size);
        if (count < 0)
            lose("unable to read core file (errno = %i)", errno);
        input.size = count;
        input.pos = 0;
        ret = ZSTD_decompressStream(stream, &output, &input);
        if (ZSTD_isError(ret))
            lose("ZSTD_decompressStream failed with error: %s",
                 ZSTD_getErrorName(ret));
    } while (ret != 0);

    ZSTD_freeDStream(stream);
}
#endif

#define MAX_SPACE_RELOCATION_RANGES 4
struct heap_adjust {
    struct range {
        lispobj start, end;
        sword_t delta;
    } range[MAX_SPACE_RELOCATION_RANGES];
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
    int j;
    for (j = adj->n_ranges - 1 ; j >= 0 ; --j)
        if (adj->range[j].start <= x && x < adj->range[j].end) return adj->range[j].delta;
    return 0;
}

// Given a post-relocation object 'x', compute the address at which
// it was originally expected to have been placed as per the core file.
static inline lispobj inverse_adjust(struct heap_adjust* adj, lispobj x)
{
    int j;
    for (j = adj->n_ranges - 1 ; j >= 0 ; --j)
        if (adj->range[j].start + adj->range[j].delta <= x &&
            x < adj->range[j].end + adj->range[j].delta)
            return x - adj->range[j].delta;
    return x;
}

// Return the adjusted value of 'word' without testing whether it looks
// like a pointer. But do test whether it points to a relocatable space.
static inline lispobj adjust_word(struct heap_adjust* adj, lispobj word) {
    return word + calc_adjustment(adj, word);
}

static void
set_adjustment(struct heap_adjust* adj,
               uword_t actual_addr,
               uword_t desired_addr,
               uword_t len)
{
    sword_t delta = len ? actual_addr - desired_addr : 0;
    if (!delta) return;
    int j = adj->n_ranges;
    gc_assert(j < MAX_SPACE_RELOCATION_RANGES);
    adj->range[j].start = (lispobj)desired_addr;
    adj->range[j].end   = (lispobj)desired_addr + len;
    adj->range[j].delta = delta;
    adj->n_ranges = j+1;
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

/* Refer to the figures above CALCULATE-DD-BITMAP in src/code/defstruct
 * for the meaning of these constants.
 * I think this fixes what may have been a latent bug in space relocation.
 * Due to inconsistent use of the bitmap, it could have processed funinstance
 * LAYOUT slots twice.  Layout is supposed to have a 0 bit, meaning do NOT scan
 * in a loop over slots, but instead externally to the loop. By marking it as 1,
 * it could be processed both outside and inside the loop. In the extremely rare
 * case of partial overlap of the desired and actual heap addresses, a pointer
 * somewhere in the middle can not be discerned as having been fixed up or not
 * fixed up based on its value. You have to know based on code flow.
 * Therefore layouts slots always have to be treated as raw in the bitmap,
 * but I did not see where it is enforced in lisp for funinstances. */
#ifndef LISP_FEATURE_EXECUTABLE_FUNINSTANCES
# define FUNINSTANCE_BITMAP -4
#elif defined LISP_FEATURE_COMPACT_INSTANCE_HEADER
# define FUNINSTANCE_BITMAP -8
#else
# define FUNINSTANCE_BITMAP -24
#endif

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
        case INSTANCE_WIDETAG:
            layout = layout_of(where);
            adjusted_layout = adjust_word(adj, layout);
            // writeback the layout if it changed. The layout is not a tagged slot
            // so it would not be fixed up otherwise.
            if (adjusted_layout != layout) layout_of(where) = adjusted_layout;
            struct bitmap bitmap = get_layout_bitmap(LAYOUT(adjusted_layout));
            {
            lispobj* slots = where+1;
            for (i=0; i<(nwords-1); ++i) // -1 from nwords because 'slots' is +1 from 'where'
                if (bitmap_logbitp(i, bitmap)) adjust_pointers(slots+i, 1, adj);
            }
            continue;
        case FUNCALLABLE_INSTANCE_WIDETAG:
            /* If non-executable funinstance, then word index 1 points at read-only space,
             * hence needs no adjustment. Otherwise, the word points within the funinstance.
             * Either way, adjust_word_at will do the right thing */
            adjust_word_at(where+1, adj);
            /* We have to adjust the layout, but its bitmap can be disregarded.
             * There is no variation in bitmap by particular subtype of funinstance */
            layout = funinstance_layout(where);
            adjusted_layout = adjust_word(adj, layout);
            if (adjusted_layout != layout) funinstance_layout(where) = adjusted_layout;
            const int mask = FUNINSTANCE_BITMAP;
            gc_assert((mask & (1<<0)) == 0); // bitmap must reflect it as untagged
            {
            lispobj* slots = where+1;
            // slots[0] is where[1] which we just adjusted, so skip it.
            for (i=1; i<(nwords-1); ++i) // -1 from nwords because 'slots' is +1 from 'where'
                if ((1<<i) & mask) adjust_pointers(slots+i, 1, adj);
            }
            continue;
        case SYMBOL_WIDETAG:
            { // Copied from scav_symbol() in gc-common
            struct symbol* s = (void*)where;
            adjust_pointers(&s->value, 3, adj); // value, fdefn, info
            lispobj name = decode_symbol_name(s->name);
            lispobj adjusted_name = adjust_word(adj, name);
            // writeback the name if it changed
            if (adjusted_name != name) FIXUP(set_symbol_name(s, adjusted_name), &s->name);
            }
            continue;
        case FDEFN_WIDETAG:
            adjust_pointers(where+1, 2, adj);
            // For most architectures, 'raw_addr' doesn't satisfy is_lisp_pointer()
            // so adjust_pointers() would ignore it. Therefore we need to
            // forcibly adjust it. This is correct whether or not there are tag bits.
            adjust_word_at(where+3, adj);
            continue;
        case CODE_HEADER_WIDETAG:
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
        default:
          if (other_immediate_lowtag_p(widetag) && leaf_obj_widetag_p(widetag))
              continue;
          else
              lose("Unrecognized heap object: @%p: %"OBJ_FMTX, where, *where);
        }
        adjust_pointers(where+1, nwords-1, adj);
    }
#if SHOW_SPACE_RELOCATION
    fprintf(stderr, "space @ %p..%p: fixed %d absolute + %d relative pointers\n",
            (lispobj*)start, end, adj->n_relocs_abs, adj->n_relocs_rel);
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
    relocate_space(DYNAMIC_SPACE_START, (lispobj*)dynamic_space_highwatermark(),
                   adj);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    relocate_space(TEXT_SPACE_START, text_space_highwatermark, adj);
#endif
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
    asm_routines_start = TEXT_SPACE_START;
#else
    if ((uword_t)read_only_space_free_pointer > READ_ONLY_SPACE_START &&
        widetag_of((lispobj*)READ_ONLY_SPACE_START) == CODE_HEADER_WIDETAG) {
        asm_routines_start = READ_ONLY_SPACE_START;
    } else {
        lispobj *where = (lispobj*)STATIC_SPACE_OBJECTS_START;
        for (; where < static_space_free_pointer; where += object_size(where))
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
     *   | text space    | .... other random stuff ... | fixedobj space | ...
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
        {TEXT_SPACE_START, TEXT_SPACE_START + text_space_size};
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

static os_vm_address_t reserve_space(int space_id, int attr,
                                     os_vm_address_t addr, os_vm_size_t size)
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (space_id == IMMOBILE_TEXT_CORE_SPACE_ID) {
        // Carve out the text space from the earlier request that was made
        // for the fixedobj space.
        ALIEN_LINKAGE_TABLE_SPACE_START = FIXEDOBJ_SPACE_START + FIXEDOBJ_SPACE_SIZE;
        return (os_vm_address_t)(ALIEN_LINKAGE_TABLE_SPACE_START + ALIEN_LINKAGE_TABLE_SPACE_SIZE);
    }
#endif
    if (size == 0) return addr;
    addr = os_alloc_gc_space(space_id, attr, addr, size);
    if (!addr) lose("Can't allocate %#"OBJ_FMTX" bytes for space %d", size, space_id);
    return addr;
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
        {dynamic_space_size, 0, DYNAMIC_SPACE_START, 0},
        // This order is determined by constants in compiler/generic/genesis
        {0, 0, STATIC_SPACE_START, &static_space_free_pointer},

        {0, 0, READ_ONLY_SPACE_START, &read_only_space_free_pointer},

#ifdef LISP_FEATURE_DARWIN_JIT
        {0, 0, STATIC_CODE_SPACE_START, &static_code_space_free_pointer},
#endif

#ifdef LISP_FEATURE_IMMOBILE_SPACE
        {FIXEDOBJ_SPACE_SIZE | 1, 0,
            FIXEDOBJ_SPACE_START, &fixedobj_free_pointer},
        {1, 0, TEXT_SPACE_START, &text_space_highwatermark}
#endif
    };

#if ELFCORE
    if (&lisp_code_start) {
        TEXT_SPACE_START = (uword_t)&lisp_code_start;
        text_space_highwatermark = &lisp_jit_code;
        text_space_size = (uword_t)&lisp_code_end - TEXT_SPACE_START;
        spaces[IMMOBILE_TEXT_CORE_SPACE_ID].len = text_space_size;
        if (text_space_highwatermark < (lispobj*)TEXT_SPACE_START
            || !PTR_IS_ALIGNED(&lisp_code_end, 4096))
            lose("ELF core alignment bug. Check for proper padding in 'editcore'");
#ifdef DEBUG_COREPARSE
        printf("Lisp code present in executable @ %lx:%lx (freeptr=%p)\n",
               (uword_t)&lisp_code_start, (uword_t)&lisp_code_end,
               text_space_highwatermark);
#endif
        ALIEN_LINKAGE_TABLE_SPACE_START =
            (uword_t)os_alloc_gc_space(ALIEN_LINKAGE_TABLE_CORE_SPACE_ID, 0, 0,
                                       ALIEN_LINKAGE_TABLE_SPACE_SIZE);
        // Prefill the alien linkage table so that shrinkwrapped executables which link in
        // all their C library dependencies can avoid linking with -ldl
        // but extern-alien still works for newly compiled code.
        lispobj* ptr = &alien_linkage_values;
        gc_assert(ptr);
        int entry_index = 0;
        int count;
        extern int alien_linkage_table_n_prelinked;
        count = alien_linkage_table_n_prelinked = *ptr++;
        for ( ; count-- ; entry_index++ ) {
            boolean datap = *ptr == (lispobj)-1; // -1 can't be a function address
            if (datap)
                ++ptr;
            arch_write_linkage_table_entry(entry_index, (void*)*ptr++, datap);
        }

        // unprotect the pages
        os_protect((void*)TEXT_SPACE_START, text_space_size, OS_VM_PROT_ALL);
    } else
#endif
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    {
        spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].desired_size +=
            text_space_size + ALIEN_LINKAGE_TABLE_SPACE_SIZE;
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

#ifdef LISP_FEATURE_DARWIN_JIT
        int enforce_address = (id == STATIC_CORE_SPACE_ID) || (id == READ_ONLY_CORE_SPACE_ID);
#else
        int enforce_address = id == STATIC_CORE_SPACE_ID;
#endif

        // We'd like to enforce proper alignment of 'addr' but there's
        // a problem: dynamic space has a stricter requirement (usually 32K)
        // than code space (4K). So don't assert the alignment.
        if (enforce_address && addr != spaces[id].base)
            lose("core address mismatch: %s_SPACE_START=%p but runtime expects %p\n",
                 id==READ_ONLY_CORE_SPACE_ID?"READ_ONLY":"STATIC",
                 (void*)addr, (void*)spaces[id].base);

        spaces[id].base = addr;
        uword_t len = os_vm_page_size * entry->page_count;
        if (id == DYNAMIC_CORE_SPACE_ID && len > dynamic_space_size) {
            lose("dynamic space too small for core: %luKiB required, %luKiB available.",
                 (unsigned long)len >> 10,
                 (unsigned long)dynamic_space_size >> 10);
        }
#ifndef LISP_FEATURE_DARWIN_JIT
        if (id == READ_ONLY_CORE_SPACE_ID) {
            if (len) // There is no "nominal" size of readonly space, so give it a size
                spaces[id].desired_size = len;
            else // Assign some address, so free_pointer does enclose [0 .. addr+0]
                READ_ONLY_SPACE_START = READ_ONLY_SPACE_END = addr;
        }
#endif
        if (len != 0) {
            spaces[id].len = len;
            // Try to map at address requested by the core file.
            size_t request = spaces[id].desired_size;
            int sub_2gb_flag = (request & 1);
            request &= ~(size_t)1;
            addr = (uword_t)reserve_space(id, sub_2gb_flag ? MOVABLE_LOW : MOVABLE,
                                          (os_vm_address_t)addr, request);
            switch (id) {
#ifndef LISP_FEATURE_DARWIN_JIT
            case READ_ONLY_CORE_SPACE_ID:
                READ_ONLY_SPACE_START = addr;
                READ_ONLY_SPACE_END = addr + len;
                break;
#endif
#ifdef LISP_FEATURE_IMMOBILE_SPACE
            case IMMOBILE_FIXEDOBJ_CORE_SPACE_ID:
            case IMMOBILE_TEXT_CORE_SPACE_ID:
                if (addr + request > 0x80000000)
                    lose("Won't map immobile space above 2GB");
                if (id == IMMOBILE_FIXEDOBJ_CORE_SPACE_ID)
                    FIXEDOBJ_SPACE_START = addr;
                else
                    TEXT_SPACE_START = addr;
                break;
#endif
            case DYNAMIC_CORE_SPACE_ID:
                {
                uword_t aligned_start = ALIGN_UP(addr, GENCGC_PAGE_BYTES);
                /* Misalignment can happen only if GC page size exceeds OS page.
                 * Drop one GC page to avoid overrunning the allocated space */
                if (aligned_start > addr) // not card-aligned
                    dynamic_space_size -= GENCGC_PAGE_BYTES;
                DYNAMIC_SPACE_START = addr = aligned_start;
                check_dynamic_space_addr_ok(addr, dynamic_space_size);
                }
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
            // text free ptr is already nonzero if Lisp code in executable
            if (!*spaces[id].pfree_pointer)
                *spaces[id].pfree_pointer = free_pointer;
            break;
        case DYNAMIC_CORE_SPACE_ID:
            next_free_page = ALIGN_UP(entry->nwords<<WORD_SHIFT, GENCGC_PAGE_BYTES)
              / GENCGC_PAGE_BYTES;
            anon_dynamic_space_start = (os_vm_address_t)(addr + len);
        }
    }

    calc_asm_routine_bounds();
#ifndef LISP_FEATURE_DARWIN_JIT
    set_adjustment(adj, READ_ONLY_SPACE_START, // actual
                   spaces[READ_ONLY_CORE_SPACE_ID].base, // expected
                   spaces[READ_ONLY_CORE_SPACE_ID].len);
#endif
    set_adjustment(adj, DYNAMIC_SPACE_START, // actual
                   spaces[DYNAMIC_CORE_SPACE_ID].base, // expected
                   spaces[DYNAMIC_CORE_SPACE_ID].len);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (lisp_code_in_elf() && TEXT_SPACE_START != spaces[IMMOBILE_TEXT_CORE_SPACE_ID].base) {
        lose("code-in-elf + PIE not supported");
    }
    set_adjustment(adj, FIXEDOBJ_SPACE_START, // actual
                   spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].base, // expected
                   spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].len);
    if (!apply_pie_relocs(TEXT_SPACE_START
                          - spaces[IMMOBILE_TEXT_CORE_SPACE_ID].base,
                          DYNAMIC_SPACE_START - spaces[DYNAMIC_CORE_SPACE_ID].base,
                          fd))
        set_adjustment(adj, TEXT_SPACE_START, // actual
                       spaces[IMMOBILE_TEXT_CORE_SPACE_ID].base, // expected
                       spaces[IMMOBILE_TEXT_CORE_SPACE_ID].len);
#endif
    if (adj->n_ranges) relocate_heap(adj);

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    /* Now determine page characteristics such as object spacing
     * (tbh it would be better to output the immobile-space page tables to the core file).
     * This used to depend critically on space relocation already having been performed.
     * It doesn't any more, but this is an OK time to do it */
    immobile_space_coreparse(spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].len,
                             spaces[IMMOBILE_TEXT_CORE_SPACE_ID].len);
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

extern void gc_load_corefile_ptes(int, core_entry_elt_t, core_entry_elt_t,
                                  os_vm_offset_t offset, int fd);

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
            // elements = gencgc-card-table-index-nbits, n-ptes, nbytes, data-page
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
#else
            SYMBOL(FREE_TLS_INDEX)->value = sizeof (struct thread);
#endif
            // simple-fun implies cold-init, not a warm core (it would be a closure then)
            if (widetag_of(native_pointer(initial_function)) == SIMPLE_FUN_WIDETAG
                && !lisp_startup_options.noinform) {
                fprintf(stderr, "Initial page table:\n");
                extern void print_generation_stats(void);
                print_generation_stats();
            }
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
#ifdef LISP_FEATURE_DARWIN_JIT
    lispobj ht = CONS(code->debug_info)->car;
#else
    lispobj ht = code->debug_info;
#endif
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

static void trace_sym(lispobj, struct symbol*, struct grvisit_context*);

#define RECURSE(x) if(is_lisp_pointer(x))graph_visit(ptr,x,context)

/* Despite this being a nice concise expression of a pointer tracing algorithm,
 * it turns out to be almost unusable in any sufficiently complicated object graph
 * due to stack overflow. The pristine core alone hits a recursion depth of >8000. */
static void graph_visit(lispobj referer, lispobj ptr, struct grvisit_context* context)
{
#define ILLEGAL_WORD 0xFFFFFFFFDEADBEEF
    if (ptr == ILLEGAL_WORD) lose("object %"OBJ_FMTX" contains a garbage word", referer);
    if (lowtag_of(ptr) == FUN_POINTER_LOWTAG
        && widetag_of(FUNCTION(ptr)) == SIMPLE_FUN_WIDETAG)
        ptr = fun_code_tagged(FUNCTION(ptr));
    if (hopscotch_get(context->seen, ptr, 0)) return;
    if (++context->depth > context->maxdepth) context->maxdepth = context->depth;
    // TODO: add rejection function for off-heap objects as part of supplied context
    hopscotch_insert(context->seen, ptr, 1);
    if (context->action) context->action(ptr, context->data);
    lispobj layout, *obj;
    sword_t nwords, i;
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
            graph_visit(ptr, layout, context);
            nwords = headerobj_size(obj);
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
            graph_visit(ptr, fun_taggedptr_from_self(obj[1]), context);
            // Closures can utilize one payload word beyond what the header
            // indicates. This is quite sucky and I don't know why I did that.
            // However, it is correctly accounted for by SHORT_BOXED_NWORDS
            // which gives you the right number of words to scan.
            nwords = SHORT_BOXED_NWORDS(*obj);
            for(i=2; i<=nwords; ++i) RECURSE(obj[i]);
            break;
        case SYMBOL_WIDETAG:
            trace_sym(ptr, SYMBOL(ptr), context);
            break;
        case WEAK_POINTER_WIDETAG:
            nwords = TINY_BOXED_NWORDS(*obj);
            for(i=1; i<=nwords; ++i) RECURSE(obj[i]);
            break;
        case FDEFN_WIDETAG:
            RECURSE(obj[1]);
            RECURSE(obj[2]);
            RECURSE(decode_fdefn_rawfun((struct fdefn*)obj));
            break;
        default:
            if (!leaf_obj_widetag_p(widetag_of(obj))) {
                sword_t size = headerobj_size(obj);
                for(i=1; i<size; ++i) RECURSE(obj[i]);
            }
      }
      --context->depth;
}

static void trace_sym(lispobj ptr, struct symbol* sym, struct grvisit_context* context)
{
    RECURSE(decode_symbol_name(sym->name));
    RECURSE(sym->value);
    RECURSE(sym->info);
    RECURSE(sym->fdefn);
}

/* Caller must provide an uninitialized hopscotch table.
 * This function will initialize it and perform a graph visit.
 * Caller may subsequently inspect the table and/or visit other objects as
 * dictated by thread stacks, etc. Caller may - but need not - provide
 * an 'action' to invoke on each object */
struct grvisit_context*
visit_heap_from_static_roots(struct hopscotch_table* reached,
                             void (*action)(lispobj, void*),
                             void* data)
{
    hopscotch_create(reached, HOPSCOTCH_HASH_FUN_DEFAULT,
                     0, // no values
                     1<<18, /* initial size */
                     0);

    struct grvisit_context* context = malloc(sizeof (struct grvisit_context));
    context->seen = reached;
    context->action = action;
    context->data = data;
    context->depth = context->maxdepth = 0;
    trace_sym(NIL, SYMBOL(NIL), context);
    lispobj* where = (lispobj*)STATIC_SPACE_OBJECTS_START;
    lispobj* end = static_space_free_pointer;
    while (where<end) {
        graph_visit(0, compute_lispobj(where), context);
        where += object_size(where);
    }
    return context;
}

// Caution: use at your own risk
#if defined DEBUG_CORE_LOADING && DEBUG_CORE_LOADING
struct visitor {
    // one item per value of widetag>>2
    // element 0 is for conses, element 64 is for totals.
    struct {
        int count;
        int words;
    } headers[65], sv_subtypes[3];
    struct hopscotch_table *reached;
};

static void tally(lispobj ptr, struct visitor* v)
{
    sword_t words;
    if (lowtag_of(ptr) == LIST_POINTER_LOWTAG)
        ++v->headers[0].count, words = 2;
    else {
        lispobj* obj = native_pointer(ptr);
        lispobj header = *obj;
        words = object_size2(obj, header);
        int widetag = header_widetag(header);
        int header_index = widetag>>2;
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
        if (widetag_of(obj) == FILLER_WIDETAG) {
            obj += object_size(obj);
            continue;
        }
        lispobj ptr = compute_lispobj(obj);
        tally(ptr, v);
        if (!hopscotch_get(v->reached, ptr, 0)) printf("unreachable: %p\n", (void*)ptr);
        obj += object_size(obj);
    }
    return 0;
}

#define count_this_pointer_p(ptr) (find_page_index((void*)ptr) >= 0)

static void sanity_check_loaded_core(lispobj initial_function)
{
    struct visitor v[2];
    struct hopscotch_table reached;
    memset(v, 0, sizeof v);
    // Pass 1: Count objects reachable from known roots.
    struct grvisit_context* c
        = visit_heap_from_static_roots(&reached, 0, 0);
    graph_visit(0, initial_function, c); // initfun is not otherwise reachable
    // having computed the reaching graph, tally up the dynamic space objects
    int key_index;
    lispobj ptr;
    for_each_hopscotch_key(key_index, ptr, reached)
        if (count_this_pointer_p(ptr)) tally(ptr, &v[0]);
    printf("graphvisit.maxdepth=%d\n", c->maxdepth);
    free(c);
    // Pass 2: Count all heap objects
    v[1].reached = &reached;
    walk_generation(visit, -1, (uword_t)&v[1]);

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
