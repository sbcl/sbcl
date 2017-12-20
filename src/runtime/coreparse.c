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
#ifdef LISP_FEATURE_LINUX
/* For madvise */
#define _BSD_SOURCE
#include <sys/mman.h>
#undef _BSD_SOURCE
#else
#include <sys/mman.h>
#endif
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
#include "pseudo-atomic.h"

#include <errno.h>

#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
# include <zlib.h>
#endif

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

#if defined(LISP_FEATURE_LINUX) && defined(LISP_FEATURE_IMMOBILE_CODE)
#define ELFCORE 1
extern __attribute__((weak)) lispobj __lisp_code_start, __lisp_code_end;
#else
#define ELFCORE 0
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
#if ELFCORE
    if (core_start < 0) {
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
            memsize_options->present_in_core = 1;
        }
    }
lose:
    close(fd);
    return core_start;
}

#ifndef LISP_FEATURE_HPUX
#define load_core_bytes(fd, where, addr, len) os_map(fd, where, addr, len)
#else
#define load_core_bytes(fd, where, addr, len) copy_core_bytes(fd, where, addr, len)
/* If more platforms don't support overlapping mmap rename this
 * def to something like ifdef nommapoverlap */
/* currently hpux only */
static void copy_core_bytes(int fd, os_vm_offset_t offset,
                            os_vm_address_t addr, int len)
{
  unsigned char buf[4096];
  int c,x;
  int old_fd = lseek(fd, 0, SEEK_CUR);

  if(len & (4096-1)){
    fprintf(stderr, "cant copy a slice of core because slice-length is not of page size(4096)\n");
    exit(-1);
  }
  if(old_fd < 0){
    fprintf(stderr, "cant perform lseek() on corefile\n");
  }
  lseek(fd, offset, SEEK_SET);
  if(fd < 0){
    fprintf(stderr, "cant perform lseek(%u,%lu,SEEK_SET) on corefile\n", fd, offset);
  }
  for(x = 0; x < len; x += 4096){
    c = read(fd, buf, 4096);
    if(c != 4096){
      fprintf(stderr, "cant read memory area from corefile at position %lu, got %d\n", offset + x, c);
      exit(-1);
    }
    memcpy(addr+x, buf, 4096);
  }
  os_flush_icache(addr, len);
}
#endif

#ifndef LISP_FEATURE_SB_CORE_COMPRESSION
# define inflate_core_bytes(fd,offset,addr,len) \
    lose("This runtime was not built with zlib-compressed core support... aborting\n")
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
    os_validate_recommit(addr, len);
# endif

    if (-1 == lseek(fd, offset, SEEK_SET)) {
        lose("Unable to lseek() on corefile\n");
    }

    stream.zalloc = NULL;
    stream.zfree = NULL;
    stream.opaque = NULL;
    stream.avail_in = 0;
    stream.next_in = buf;

    ret = inflateInit(&stream);
    if (ret != Z_OK)
        lose("zlib error %i\n", ret);

    stream.next_out  = (void*)addr;
    stream.avail_out = len;
    do {
        ssize_t count = read(fd, buf, ZLIB_BUFFER_SIZE);
        if (count < 0)
            lose("unable to read core file (errno = %i)\n", errno);
        stream.next_in = buf;
        stream.avail_in = count;
        if (count == 0) break;
        ret = inflate(&stream, Z_NO_FLUSH);
        switch (ret) {
        case Z_STREAM_END:
            break;
        case Z_OK:
            if (stream.avail_out == 0)
                lose("Runaway gzipped core directory... aborting\n");
            if (stream.avail_in > 0)
                lose("zlib inflate returned without fully"
                     "using up input buffer... aborting\n");
            break;
        default:
            lose("zlib inflate error: %i\n", ret);
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

#ifndef LISP_FEATURE_RELOCATABLE_HEAP
#define adjust_word(ignore,thing) thing
#define relocate_heap(ignore)
#else
#include "genesis/gc-tables.h"
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
static void __attribute__((unused))
adjust_code_refs(struct heap_adjust* adj, lispobj fixups, struct code* code)
{
    struct varint_unpacker unpacker;
    varint_unpacker_init(&unpacker, fixups);
    char* instructions = (char*)((lispobj*)code + code_header_words(code->header));
    int prev_loc = 0, loc;
    while (varint_unpack(&unpacker, &loc) && loc != 0) {
        // For extra compactness, each loc is relative to the prior,
        // so that the magnitudes are smaller.
        loc += prev_loc;
        prev_loc = loc;
        void* fixup_where = instructions + loc;
        lispobj ptr = UNALIGNED_LOAD32(fixup_where);
        lispobj adjusted = ptr + calc_adjustment(adj, ptr);
        if (adjusted != ptr)
            FIXUP32(UNALIGNED_STORE32(fixup_where, adjusted), fixup_where);
    }
}

static inline void fix_fun_header_layout(lispobj* fun, struct heap_adjust* adj)
{
#if defined(LISP_FEATURE_COMPACT_INSTANCE_HEADER) && defined(LISP_FEATURE_64_BIT)
    lispobj ptr = function_layout(fun);
    lispobj adjusted = adjust_word(adj, ptr);
    if (adjusted != ptr)
        FIXUP(set_function_layout(fun, adjusted), fun);
#endif
}

static void relocate_space(uword_t start, lispobj* end, struct heap_adjust* adj)
{
    lispobj *where = (lispobj*)start;
    lispobj header_word;
    int widetag;
    long nwords;
    lispobj layout, adjusted_layout, bitmap;
    struct code* code;
    sword_t delta;

    adj->n_relocs_abs = adj->n_relocs_rel = 0;
    for ( ; where < end ; where += nwords ) {
        header_word = *where;
        if (is_cons_half(header_word)) {
            adjust_pointers(where, 2, adj);
            nwords = 2;
            continue;
        }
        widetag = widetag_of(header_word);
        nwords = sizetab[widetag](where);
        switch (widetag) {
        case FUNCALLABLE_INSTANCE_WIDETAG:
            // Special note on the word at where[1] in funcallable instances:
            // - If no immobile code, then the word points to read-only space,
            ///  hence needs no adjustment.
            // - Otherwise, the word might point to a relocated range,
            //   either the instance itself, or a trampoline in immobile space.
            adjust_word_at(where+1, adj);
        case INSTANCE_WIDETAG:
            layout = (widetag == FUNCALLABLE_INSTANCE_WIDETAG) ?
                funinstance_layout(where) : instance_layout(where);
            adjusted_layout = adjust_word(adj, layout);
            // Do not alter the layout as stored in the instance if non-compact
            // header. instance_scan() will do it if necessary.
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
            if (adjusted_layout != layout)
                instance_layout(where) = adjusted_layout;
#endif
            bitmap = LAYOUT(adjusted_layout)->bitmap;
            gc_assert(fixnump(bitmap)
                      || widetag_of(*native_pointer(bitmap))==BIGNUM_WIDETAG);
            // If the post-adjustment address of 'layout' is higher than 'where',
            // then the layout's pointer slots need adjusting.
            // This is true regardless of whether the core was mapped at a higher
            // or lower address than desired.
            if (is_lisp_pointer(bitmap) && adjusted_layout > (lispobj)where) {
                // Do not write back the adjusted bitmap pointer. Each heap word
                // must be touched at most once. When the layout itself gets scanned,
                // the bitmap slot will be rewritten if needed.
                bitmap = adjust_word(adj, bitmap);
            }

            instance_scan((void(*)(lispobj*,sword_t,uword_t))adjust_pointers,
                          where+1, nwords-1, bitmap, (uintptr_t)adj);
            continue;
        case FDEFN_WIDETAG:
            adjust_pointers(where+1, 2, adj);
            // 'raw_addr' doesn't satisfy is_lisp_pointer() for x86,
            // so adjust_pointers() would ignore it. Therefore we need to
            // forcibly adjust it.
#ifndef LISP_FEATURE_IMMOBILE_CODE
            adjust_word_at(where+3, adj);
#elif defined(LISP_FEATURE_X86_64)
            // the offset from fdefns to code can change if:
            // - static space fdefn -> immobile space code and immobile space moved
            // - immobile spaces changed relative position
            if ((delta = calc_adjustment(adj, fdefn_callee_lispobj((struct fdefn*)where)))) {
                void* prel32 = 1 + (char*)(where+3);
                FIXUP_rel(UNALIGNED_STORE32(prel32, UNALIGNED_LOAD32(prel32) + delta),
                          prel32);
            }
#endif
            continue;
        case CODE_HEADER_WIDETAG:
            // Fixup the constant pool. The word at where+1 is a fixnum.
            adjust_pointers(where+2, code_header_words(header_word)-2, adj);
            // Fixup all embedded simple-funs
            code = (struct code*)where;
            for_each_simple_fun(i, f, code, 1, {
                fix_fun_header_layout((lispobj*)f, adj);
#if FUN_SELF_FIXNUM_TAGGED
                if (f->self != (lispobj)f->code)
                    FIXUP(f->self = (lispobj)f->code, &f->self);
#endif
                adjust_pointers(SIMPLE_FUN_SCAV_START(f), SIMPLE_FUN_SCAV_NWORDS(f), adj);
            });
            // Compute the address where the code "was" as the first argument
            // by negating the adjustment for 'where'.
            // Can't call calc_adjustment to get the negative of the adjustment!
            gencgc_apply_code_fixups((struct code*)((char*)where -
                                                    adj->range[DYNAMIC_SPACE_ADJ_INDEX].delta),
                                     code);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
            // Now that the packed integer comprising the list of fixup locations
            // has been fixed-up (if necessary), apply them to the code.
            if (code->fixups != 0)
                adjust_code_refs(adj, code->fixups, code);
#endif
            continue;
        case CLOSURE_WIDETAG:
            fix_fun_header_layout(where, adj);
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
            // For x86[-64], the closure fun appears to be a fixnum,
            // and might need adjustment unless pointing to immobile code.
            // Then fall into the general case; where[1] won't get re-adjusted
            // because it doesn't satisfy is_lisp_pointer().
            adjust_word_at(where+1, adj);
#endif
            break;
        // Vectors require extra care because of EQ-based hashing.
        case SIMPLE_VECTOR_WIDETAG:
          if (is_vector_subtype(*where, VectorValidHashing)) {
              struct vector* v = (struct vector*)where;
              gc_assert(v->length > 0 &&
                        !(v->length & make_fixnum(1)) && // length must be even
                        lowtag_of(v->data[0]) == INSTANCE_POINTER_LOWTAG);
              lispobj* data = (lispobj*)v->data;
              adjust_pointers(&data[0], 1, adj); // adjust the hash-table structure
              boolean needs_rehash = 0;
              lispobj *where = &data[2], *end = &data[fixnum_value(v->length)];
              // Adjust the elements, checking for need to rehash.
              for ( ; where < end ; where += 2) {
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
                  data[1] = make_fixnum(1);
              continue;
          }
        // All the array header widetags.
        case SIMPLE_ARRAY_WIDETAG:
#ifdef COMPLEX_CHARACTER_STRING_WIDETAG
        case COMPLEX_CHARACTER_STRING_WIDETAG:
#endif
        case COMPLEX_BASE_STRING_WIDETAG:
        case COMPLEX_VECTOR_NIL_WIDETAG:
        case COMPLEX_BIT_VECTOR_WIDETAG:
        case COMPLEX_VECTOR_WIDETAG:
        case COMPLEX_ARRAY_WIDETAG:
        // And the rest of the purely descriptor objects.
        case SYMBOL_WIDETAG:
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
            continue;
        default:
          if (other_immediate_lowtag_p(widetag)
              && specialized_vector_widetag_p(widetag))
              continue;
          else
              lose("Unrecognized heap object: @%p: %lx\n", where, header_word);
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
    if (!lisp_startup_options.noinform || SHOW_SPACE_RELOCATION) {
        int i;
        for (i = 0; i < adj->n_ranges; ++i)
            if (adj->range[i].delta)
                fprintf(stderr, "NOTE: Relocating [%p:%p] into [%p:%p]\n",
                        (char*)adj->range[i].start,
                        (char*)adj->range[i].end,
                        (char*)adj->range[i].start + adj->range[i].delta,
                        (char*)adj->range[i].end + adj->range[i].delta);
    }
    relocate_space(STATIC_SPACE_START, static_space_free_pointer, adj);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    relocate_space(FIXEDOBJ_SPACE_START, fixedobj_free_pointer, adj);
    SYMBOL(FUNCTION_LAYOUT)->value = \
        adjust_word(adj, SYMBOL(FUNCTION_LAYOUT)->value >> 32) << 32;
#endif
    relocate_space(DYNAMIC_SPACE_START, (lispobj*)get_alloc_pointer(), adj);

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    // Pointers within varyobj space to varyobj space do not need adjustment
    // so remove any delta before performing the relocation pass on this space.
#if ELFCORE
    if (&__lisp_code_start)
        adj->range[2].delta = 0;
#endif
    relocate_space(VARYOBJ_SPACE_START, varyobj_free_pointer, adj);
#endif
}
#endif

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
    adj->range[j].delta = actual_addr - desired_addr;
    adj->n_ranges = j+1;
}

static void
process_directory(int count, struct ndir_entry *entry,
                  int fd, os_vm_offset_t file_offset, int merge_core_pages,
                  struct heap_adjust* adj)
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
        {0, 0, 0, 0},
#endif
        // This order is determined by constants in compiler/generic/genesis
        {0, 0, STATIC_SPACE_START, &static_space_free_pointer},
        {0, 0, READ_ONLY_SPACE_START, &read_only_space_free_pointer},
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        {FIXEDOBJ_SPACE_SIZE | 1, 0,
            FIXEDOBJ_SPACE_START, &fixedobj_free_pointer},
        {1, 0, VARYOBJ_SPACE_START, &varyobj_free_pointer}
#endif
    };

#if ELFCORE
    if (&__lisp_code_start) {
        VARYOBJ_SPACE_START = (uword_t)&__lisp_code_start;
        varyobj_free_pointer = &__lisp_code_end;
        uword_t aligned_end = ALIGN_UP((uword_t)&__lisp_code_end, IMMOBILE_CARD_BYTES);
        varyobj_space_size = aligned_end - VARYOBJ_SPACE_START;
        spaces[IMMOBILE_VARYOBJ_CORE_SPACE_ID].len = varyobj_space_size;
        gc_assert(varyobj_free_pointer >= (lispobj*)VARYOBJ_SPACE_START);
#if !ENABLE_PAGE_PROTECTION
        printf("Lisp code present in executable @ %lx:%lx (freeptr=%p)\n",
               (uword_t)&__lisp_code_start, aligned_end, varyobj_free_pointer);
#endif
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
        sword_t id = entry->identifier;
        uword_t addr = entry->address;
        int compressed = id & DEFLATED_CORE_SPACE_ID_FLAG;
        id -= compressed;
        if (id < 1 || id > MAX_CORE_SPACE_ID)
            lose("unknown space ID %ld addr %p\n", id, addr);

#ifndef LISP_FEATURE_RELOCATABLE_HEAP
        int enforce_address = 1;
#elif defined(LISP_FEATURE_IMMOBILE_SPACE)
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
#ifdef LISP_FEATURE_CHENEYGC
            if (id == DYNAMIC_CORE_SPACE_ID) {
                if ((fail = (addr != DYNAMIC_0_SPACE_START) &&
                            (addr != DYNAMIC_1_SPACE_START)) != 0)
                    fprintf(stderr, "in core: %p; in runtime: %p or %p\n",
                            (void*)addr,
                            (void*)DYNAMIC_0_SPACE_START,
                            (void*)DYNAMIC_1_SPACE_START);
            } else
#endif
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
            lose("dynamic space too small for core: %luKiB required, %luKiB available.\n",
                 (unsigned long)len >> 10,
                 (unsigned long)dynamic_space_size >> 10);
        }
        if (len != 0) {
            spaces[id].len = len;
            uword_t __attribute__((unused)) aligned_start;
#ifdef LISP_FEATURE_RELOCATABLE_HEAP
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
            if (request)
                addr = (uword_t)os_validate(sub_2gb_flag ? MOVABLE_LOW : MOVABLE,
                                            (os_vm_address_t)addr, request);
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
                aligned_start = ALIGN_UP(addr, GENCGC_CARD_BYTES);
                /* Misalignment can happen only if card size exceeds OS page.
                 * Drop one card to avoid overrunning the allocated space */
                if (aligned_start > addr) // not card-aligned
                    dynamic_space_size -= GENCGC_CARD_BYTES;
                DYNAMIC_SPACE_START = addr = aligned_start;
                break;
            }
#endif /* LISP_FEATURE_RELOCATABLE_HEAP */

            sword_t offset = os_vm_page_size * (1 + entry->data_page);
            if (compressed)
                inflate_core_bytes(fd, offset + file_offset, (os_vm_address_t)addr, len);
            else
                load_core_bytes(fd, offset + file_offset, (os_vm_address_t)addr, len);
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
            /* This assertion safeguards the test in zero_pages_with_mmap()
             * which trusts that if addr > anon_dynamic_space_start
             * then addr did not come from any file mapping. */
            gc_assert((lispobj)anon_dynamic_space_start > STATIC_SPACE_END);
        }
    }

#ifdef LISP_FEATURE_RELOCATABLE_HEAP
    set_adjustment(adj, DYNAMIC_SPACE_START, // actual
                   spaces[DYNAMIC_CORE_SPACE_ID].base, // expected
                   spaces[DYNAMIC_CORE_SPACE_ID].len);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    set_adjustment(adj, FIXEDOBJ_SPACE_START, // actual
                   spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].base, // expected
                   spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].len);
    set_adjustment(adj, VARYOBJ_SPACE_START, // actual
                   spaces[IMMOBILE_VARYOBJ_CORE_SPACE_ID].base, // expected
                   spaces[IMMOBILE_VARYOBJ_CORE_SPACE_ID].len);
#endif
    if (adj->range[0].delta | adj->range[1].delta | adj->range[2].delta)
        relocate_heap(adj);
#endif

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    /* Now determine page characteristics (such as object spacing)
     * after relocation, because we need to know which objects are layouts
     * based on knowing layout-of-layout.  The test for that is dependent
     * on what it's address should be, not what it was in the file */
    immobile_space_coreparse(spaces[IMMOBILE_FIXEDOBJ_CORE_SPACE_ID].len,
                             spaces[IMMOBILE_VARYOBJ_CORE_SPACE_ID].len);
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
        {VARYOBJ_SPACE_START, VARYOBJ_SPACE_START + VARYOBJ_SPACE_SIZE};
    if (range2.start < range1.start) { // swap
        struct range temp = range1;
        range1 = range2;
        range2 = temp;
    }
    immobile_space_lower_bound  = range1.start;
    immobile_space_max_offset   = range2.end - range1.start;
    immobile_range_1_max_offset = range1.end - range1.start;
    immobile_range_2_min_offset = range2.start - range1.start;
#endif
#ifdef LISP_FEATURE_X86_64
    tune_asm_routines_for_microarch(); // before WPing immobile space
#endif
}

#ifdef LISP_FEATURE_GENCGC
extern void gc_load_corefile_ptes(core_entry_elt_t, core_entry_elt_t,
                                  off_t offset, int fd);
#else
#define gc_load_corefile_ptes(dummy1,dummy2,dummy3,dummy4)
#endif

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
    os_vm_size_t len, remaining_len;
    int fd = open_binary(file, O_RDONLY);
    ssize_t count;
    lispobj initial_function = NIL;
    struct heap_adjust adj;
    memset(&adj, 0, sizeof adj);

    FSHOW((stderr, "/entering load_core_file(%s)\n", file));
    if (fd < 0) {
        fprintf(stderr, "could not open file \"%s\"\n", file);
        perror("open");
        exit(1);
    }

    lseek(fd, file_offset, SEEK_SET);
    header = calloc(os_vm_page_size, 1);

    count = read(fd, header, os_vm_page_size);
    if (count < (ssize_t) os_vm_page_size) {
        lose("premature end of core file\n");
    }
    SHOW("successfully read first page of core");

    ptr = header;
    val = *ptr++;

    if (val != CORE_MAGIC) {
        lose("invalid magic number in core: 0x%lx should have been 0x%x.\n",
             val,
             CORE_MAGIC);
    }
    SHOW("found CORE_MAGIC");

#define WORD_FMTX OS_VM_SIZE_FMTX
    for ( ; ; ptr += remaining_len) {
        val = *ptr++;
        len = *ptr++;
        remaining_len = len - 2; /* (-2 to cancel the two ++ operations) */
        FSHOW((stderr, "/val=0x%"WORD_FMTX", remaining_len=0x%"WORD_FMTX"\n",
               val, remaining_len));

        switch (val) {

        case END_CORE_ENTRY_TYPE_CODE:
            free(header);
            close(fd);
            return initial_function;

        case BUILD_ID_CORE_ENTRY_TYPE_CODE:
            SHOW("BUILD_ID_CORE_ENTRY_TYPE_CODE case");
            {
                os_vm_size_t stringlen = *ptr++;
                --remaining_len;
                gc_assert(remaining_len * sizeof (core_entry_elt_t) >= stringlen);
                if (sizeof build_id == stringlen+1 && !memcmp(ptr, build_id, stringlen))
                    break;
                /* .core files are not binary-compatible between
                 * builds because we can't easily detect whether the
                 * sources were patched between the time the
                 * dumping-the-.core runtime was built and the time
                 * that the loading-the-.core runtime was built.
                 *
                 * (We could easily detect whether version.lisp-expr
                 * was changed, but people experimenting with patches
                 * don't necessarily update version.lisp-expr.) */
                fprintf(stderr,
                        "core was built for runtime \"%.*s\" but this is \"%s\"\n",
                        (int)stringlen, (char*)ptr, build_id);
                lose("can't load .core for different runtime, sorry\n");
            }

        case NEW_DIRECTORY_CORE_ENTRY_TYPE_CODE:
            SHOW("NEW_DIRECTORY_CORE_ENTRY_TYPE_CODE case");
            process_directory(remaining_len / NDIR_ENTRY_LENGTH,
                              (struct ndir_entry*)ptr, fd, file_offset,
                              merge_core_pages, &adj);
            break;

        case INITIAL_FUN_CORE_ENTRY_TYPE_CODE:
            SHOW("INITIAL_FUN_CORE_ENTRY_TYPE_CODE case");
            initial_function = adjust_word(&adj, (lispobj)*ptr);
            break;

        case PAGE_TABLE_CORE_ENTRY_TYPE_CODE:
            gc_load_corefile_ptes(ptr[0], ptr[1],
                                  file_offset + (ptr[2] + 1) * os_vm_page_size, fd);
            break;
        case RUNTIME_OPTIONS_MAGIC: break; // already processed
        default:
            lose("unknown core file entry: 0x%"WORD_FMTX"\n", val);
        }
    }
}

#include "genesis/hash-table.h"
#include "genesis/vector.h"
#include "genesis/cons.h"
os_vm_address_t get_asm_routine_by_name(const char* name)
{
#ifdef LISP_FEATURE_IMMOBILE_CODE
    struct code* code = (struct code*)VARYOBJ_SPACE_START;
#else
    struct code* code = (struct code*)READ_ONLY_SPACE_START;
#endif
    if (lowtag_of(code->debug_info) == LIST_POINTER_LOWTAG) {
        struct hash_table* ht =
            (struct hash_table*)native_pointer(CONS(code->debug_info)->car);
        struct vector* table = VECTOR(ht->table);
        lispobj sym;
        int i;
        for (i=2 ; i < fixnum_value(table->length) ; i += 2)
            if (lowtag_of(sym = table->data[i]) == OTHER_POINTER_LOWTAG
                && widetag_of(SYMBOL(sym)->header) == SYMBOL_WIDETAG
                && !strcmp(name, (char*)(VECTOR(SYMBOL(sym)->name)->data)))
                return ALIGN_UP(offsetof(struct code,constants), 2*N_WORD_BYTES)
                    + fixnum_value(CONS(table->data[i+1])->car) + (os_vm_address_t)code;
        // Something is wrong if we have a hashtable but find nothing.
        fprintf(stderr, "WARNING: get_asm_routine_by_name(%s) failed\n",
                name);
    }
    return NULL;
}

void asm_routine_poke(const char* routine, int offset, char byte)
{
    char *address = (char *)get_asm_routine_by_name(routine);
    if (address)
        address[offset] = byte;
}
