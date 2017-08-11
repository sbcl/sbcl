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
#include "runtime-options.h"

#include <errno.h>

#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
# include <zlib.h>
#endif

unsigned char build_id[] =
#include "../../output/build-id.tmp"
;

int
open_binary(char *filename, int mode)
{
#ifdef LISP_FEATURE_WIN32
    mode |= O_BINARY;
#endif

    return open(filename, mode);
}


static struct runtime_options *
read_runtime_options(int fd)
{
    os_vm_size_t optarray[RUNTIME_OPTIONS_WORDS];
    struct runtime_options *options = NULL;

    if (read(fd, optarray, RUNTIME_OPTIONS_WORDS * sizeof(os_vm_size_t)) !=
        RUNTIME_OPTIONS_WORDS * sizeof(size_t)) {
        return NULL;
    }

    if ((RUNTIME_OPTIONS_MAGIC != optarray[0]) || (0 == optarray[1])) {
        return NULL;
    }

    options = successful_malloc(sizeof(struct runtime_options));

    options->dynamic_space_size = optarray[2];
    options->thread_control_stack_size = optarray[3];

    return options;
}

void
maybe_initialize_runtime_options(int fd)
{
    struct runtime_options *new_runtime_options;
    off_t end_offset = sizeof(lispobj) +
        sizeof(os_vm_offset_t) +
        (RUNTIME_OPTIONS_WORDS * sizeof(size_t));

    lseek(fd, -end_offset, SEEK_END);

    if ((new_runtime_options = read_runtime_options(fd))) {
        runtime_options = new_runtime_options;
    }
}

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
search_for_embedded_core(char *filename)
{
    lispobj header;
    os_vm_offset_t lispobj_size = sizeof(lispobj);
    os_vm_offset_t trailer_size = lispobj_size + sizeof(os_vm_offset_t);
    os_vm_offset_t core_start, pos;
    int fd = -1;

    if ((fd = open_binary(filename, O_RDONLY)) < 0)
        goto lose;

    if (read(fd, &header, (size_t)lispobj_size) < lispobj_size)
        goto lose;
    if (header == CORE_MAGIC) {
        /* This file is a real core, not an embedded core.  Return 0 to
         * indicate where the core starts, and do not look for runtime
         * options in this case. */
        return 0;
    }

    if (lseek(fd, -lispobj_size, SEEK_END) < 0)
        goto lose;
    if (read(fd, &header, (size_t)lispobj_size) < lispobj_size)
        goto lose;

    if (header == CORE_MAGIC) {
        if (lseek(fd, -trailer_size, SEEK_END) < 0)
            goto lose;
        if (read(fd, &core_start, sizeof(os_vm_offset_t)) < 0)
            goto lose;

        if (lseek(fd, core_start, SEEK_SET) < 0)
            goto lose;
        pos = lseek(fd, 0, SEEK_CUR);

        if (read(fd, &header, (size_t)lispobj_size) < lispobj_size)
            goto lose;

        if (header != CORE_MAGIC)
            goto lose;

        maybe_initialize_runtime_options(fd);

        close(fd);
        return pos;
    }

lose:
    if (fd != -1)
        close(fd);

    return -1;
}

/* If more platforms doesn't support overlapping mmap rename this
 * def to something like ifdef nommapoverlap */
/* currently hpux only */
#ifdef LISP_FEATURE_HPUX
void copy_core_bytes(int fd, os_vm_offset_t offset,
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

#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
# define ZLIB_BUFFER_SIZE (1u<<16)
void inflate_core_bytes(int fd, os_vm_offset_t offset,
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
        ssize_t count = read(fd, buf, sizeof(buf));
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

#ifndef LISP_FEATURE_RELOCATABLE_HEAP
#define adjust_word(x) x
#else
#include "genesis/gc-tables.h"
#include "genesis/hash-table.h"
#include "genesis/layout.h"
#include "genesis/vector.h"
static lispobj expected_range_start, expected_range_end;
static sword_t heap_adjustment;

/* Define this in CFLAGS as 1 to deliberately mess up the mapped address */
#ifndef MOCK_MMAP_FAILURE
#define MOCK_MMAP_FAILURE 0
#endif

#if !MOCK_MMAP_FAILURE
#define maybe_fuzz_address(x) x
#else
/// "Pseudo-randomly" alter requested address for testing purposes so that
/// we can test interesting scenarios such as:
///  - partially overlapping ranges for the desired and actual space
///  - aligned to OS page but misaligned for GC card
#define maybe_fuzz_address(x) fuzz_address(x)
#include <time.h>
#include <stdio.h>
#include <stdlib.h>
uword_t fuzz_address(uword_t addr)
{
    char * pathname;
    FILE * f;
    char line[100];
    int line_number, i;

    if ((pathname = getenv("SBCL_FAKE_MMAP_INSTRUCTION_FILE")) == NULL) {
        fprintf(stderr, "WARNING: image built with MOCK_MMAP_FAILURE\n");
        fprintf(stderr, "         but no mock configuration data found.\n");
        exit(1);
    }
    if ((f = fopen(pathname, "r+")) == NULL) {
        // If the file can't be found, don't silently ignore it,
        // because if the relocator "worked" you don't want to get all happy
        // that it worked only to find that it didn't actually perform relocation.
        fprintf(stderr, "Could not read 'fakemap' file\n");
        exit(1);
    }
    ignore_value(fgets(line, sizeof line, f));
    line_number = atoi(line);
    uword_t result = 0;
    for (i = 0 ; i <= line_number ; ++i) {
        boolean ok;
        ok = fgets(line, sizeof line, f) != NULL && line[0] != '\n';
        if (i == line_number - 1)
            result = strtol(line, 0, 16);
        if (!ok) {
            // fprintf(stderr, "*** Rewinding fake mmap instructions\n");
            line_number = 0;
            break;
        }
    }
    rewind(f);
    fprintf(f, "%02d", 1+line_number);
    fclose(f);
    fprintf(stderr, "//dynamic space @ %p\n", (void*)result);
    return result;
}
#endif

static inline boolean needs_adjusting(lispobj x)
{
  return (expected_range_start <= x && x < expected_range_end);
}

// Return the adjusted value of 'word' without testing whether it looks
// like a pointer. But do test whether it points to the dynamic space.
static inline lispobj adjust_word(lispobj word)
{
  return (needs_adjusting(word) ? heap_adjustment : 0) + word;
}

// Adjust the words in range [where,where+n_words)
// skipping any words that have non-pointer nature.
static void adjust_pointers(lispobj *where, long n_words)
{
    long i;
    for (i=0;i<n_words;++i) {
        lispobj word = where[i], adjusted;
        if (is_lisp_pointer(word) && (adjusted = adjust_word(word)) != word) {
            where[i] = adjusted;
        }
    }
}

static void fixup_space(lispobj* where, uword_t len)
{
    lispobj *end = (lispobj*)((char*)where + len);
    lispobj header_word;
    int widetag;
    long nwords;
    lispobj layout, layout_slots_adjustp, bitmap;
    struct code* code;

    for ( ; where < end ; where += nwords ) {
        header_word = *where;
        if (is_cons_half(header_word)) {
            adjust_pointers(where, 2);
            nwords = 2;
            continue;
        }
        widetag = widetag_of(header_word);
        nwords = sizetab[widetag](where);
        switch (widetag) {
        case INSTANCE_WIDETAG:
        case FUNCALLABLE_INSTANCE_WIDETAG:
            // Special note on the word at where[1] in funcallable instances:
            // - If the instance lives in immobile space and has a self-contained
            //   trampoline, that word does not need adjustment.
            // - If the instance does not have a self-contained trampoline,
            //   then the word points to read-only space hence needs no adjustment.
            layout = (widetag == FUNCALLABLE_INSTANCE_WIDETAG) ?
                fin_layout(where) : instance_layout(where);
            // Possibly adjust, but do not alter the in-memory value of, this layout.
            // instance_scan_interleaved() on this instance will actually adjust
            // the layout slot if needed.
            bitmap = LAYOUT(adjust_word(layout))->bitmap;

            // If the layout of this instance resides at a higher address
            // than the instance itself, then the layout's pointer slots were
            // not fixed up yet. This is true regardless of whether the core
            // was mapped at a higher or lower address than desired.
            // i.e. If we haven't yet scanned the layout in this fixup pass, and if
            // it resides in the expected dynamic space range, then it needs adjusting.
            layout_slots_adjustp = layout > (lispobj)where && needs_adjusting(layout);

            // If the layout has not itself been adjusted, then its bitmap,
            // if not a fixnum, needs (nondestructive) adjustment.
            if (!fixnump(bitmap) && layout_slots_adjustp)
                bitmap = adjust_word(bitmap);

            instance_scan(adjust_pointers, where+1, nwords-1, bitmap);
            continue;
        case FDEFN_WIDETAG:
            adjust_pointers(where+1, 2);
            // 'raw_addr' doesn't satisfy is_lisp_pointer() for x86,
            // so adjust_pointers() would ignore it. Force adjustment if
            // it points to dynamic space. For x86-64 with IMMOBILE_CODE,
            // the fdefn can only point to immobile space,
            // and it does so via a JMP instruction, so we must ignore it,
            // lest it accidentally be adjusted (if somehow the next 8 bytes
            // when interpreted as a word seemed to point to dynamic space
            // though in fact it's an opcode followed by a 4 byte displacement).
            // For all others, just adjust it here for uniformity.
#ifndef LISP_FEATURE_IMMOBILE_CODE
            where[3] = adjust_word(where[3]);
#endif
            continue;
        case CODE_HEADER_WIDETAG:
            // Fixup the constant pool. The word at where+1 is a fixnum.
            adjust_pointers(where+2, code_header_words(header_word)-2);
            // Fixup all embedded simple-funs
            code = (struct code*)where;
            for_each_simple_fun(i, f, code, 1, {
                f->self = adjust_word(f->self);
                adjust_pointers(SIMPLE_FUN_SCAV_START(f), SIMPLE_FUN_SCAV_NWORDS(f));
            });
            // Compute the address where the code "was" as the first argument
            gencgc_apply_code_fixups((struct code*)((char*)where - heap_adjustment),
                                     code);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
            // Now that the packed integer comprising the list of fixup locations
            // has been fixed-up (if necessary), apply them to the code.
            if (code->fixups != 0)
                fixup_immobile_refs(adjust_word, code->fixups, code);
#endif
            continue;
        case CLOSURE_WIDETAG:
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
            // For x86[-64], the closure fun appears to be a fixnum,
            // and might need adjustment unless pointing to immobile code.
            // Then fall into the general case; where[1] won't get re-adjusted
            // because it doesn't satisfy is_lisp_pointer().
            where[1] = adjust_word(where[1]);
#endif
            break;
        // Vectors require extra care because of EQ-based hashing.
        case SIMPLE_VECTOR_WIDETAG:
          if ((HeaderValue(*where) & 0xFF) == subtype_VectorValidHashing) {
              struct vector* v = (struct vector*)where;
              gc_assert(v->length > 0 &&
                        !(fixnum_value(v->length) & 1) &&  // length must be even
                        lowtag_of(v->data[0]) == INSTANCE_POINTER_LOWTAG);
              lispobj* data = v->data;
              adjust_pointers(&data[0], 1); // adjust the hash-table structure
              boolean needs_rehash = 0;
              int i;
              // Adjust the elements, checking for need to rehash.
              // v->data[1] is the unbound marker (a non-pointer)
              for (i = fixnum_value(v->length)-1 ; i>=2 ; --i) {
                  lispobj ptr = data[i];
                  if (is_lisp_pointer(ptr) && needs_adjusting(ptr)) {
                      data[i] += heap_adjustment;
                      needs_rehash = 1;
                  }
              }
              if (needs_rehash) {
                  struct hash_table *ht = (struct hash_table*)native_pointer(v->data[0]);
                  ht->needs_rehash_p = T;
              }
              continue;
          }
        // All the array header widetags.
        case SIMPLE_ARRAY_WIDETAG:
        case COMPLEX_CHARACTER_STRING_WIDETAG:
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
            if (needs_adjusting(where[1])) {
                fprintf(stderr,
                        "WARNING: SAP at %p -> %p in relocatable core\n",
                        where, (void*)where[1]);
                where[1] += heap_adjustment;
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
        adjust_pointers(where+1, nwords-1);
    }
}

void relocate_heap(lispobj* want, lispobj* got, uword_t len)
{
  expected_range_start = (lispobj)want;
  expected_range_end = (lispobj)want + len;
  heap_adjustment = (lispobj)got - (lispobj)want;
#if MOCK_MMAP_FAILURE
  fprintf(stderr, "Relocating heap from [%p:%p] to [%p:%p]\n",
          want, (char*)want+len, got, (char*)got+len);
#endif

  fixup_space((lispobj*)STATIC_SPACE_START, STATIC_SPACE_SIZE);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
  fixup_space((lispobj*)IMMOBILE_SPACE_START, IMMOBILE_SPACE_SIZE);
#endif
  fixup_space((lispobj*)got, len); // the dynamic space itself
}
#endif

int merge_core_pages = -1;

#ifdef LISP_FEATURE_LINUX
os_vm_address_t anon_dynamic_space_start;
#endif

static void
process_directory(int fd, lispobj *ptr, int count, os_vm_offset_t file_offset)
{
    extern void immobile_space_coreparse(uword_t,uword_t);
    struct ndir_entry *entry;
    int compressed;
    extern void write_protect_immobile_space();

    FSHOW((stderr, "/process_directory(..), count=%d\n", count));

    for (entry = (struct ndir_entry *) ptr; --count>= 0; ++entry) {

        compressed = 0;
        sword_t id = entry->identifier;
        if (id <= (MAX_CORE_SPACE_ID | DEFLATED_CORE_SPACE_ID_FLAG)) {
            if (id & DEFLATED_CORE_SPACE_ID_FLAG)
                compressed = 1;
            id &= ~(DEFLATED_CORE_SPACE_ID_FLAG);
        }
        sword_t offset = os_vm_page_size * (1 + entry->data_page);
        uword_t corefile_addr = os_vm_page_size * entry->address;
        os_vm_address_t addr = (os_vm_address_t)corefile_addr;
        uword_t len = os_vm_page_size * entry->page_count;
        if (len != 0) {
#ifdef LISP_FEATURE_RELOCATABLE_HEAP
            // If this entry is for the dynamic space,
            // try to map the space now at the address
            // specified in the core file.
            if (id == DYNAMIC_CORE_SPACE_ID) {
                uword_t aligned_start;
                addr = (os_vm_address_t)
                    os_validate(MOVABLE,
                                (os_vm_address_t)maybe_fuzz_address(corefile_addr),
                                dynamic_space_size);
                aligned_start = ((uword_t)addr + GENCGC_CARD_BYTES-1)
                    & ~(GENCGC_CARD_BYTES-1);
                DYNAMIC_SPACE_START = aligned_start;
                if (aligned_start > (uword_t)addr) { // not card-aligned
                    // This could happen only if the OS page size
                    // is smaller than the GC card size.
                    // Drop the final card and decrease the dynamic_space_size
                    // to make the absolute end come out the same.
                    dynamic_space_size -= GENCGC_CARD_BYTES;
                    --page_table_pages;
                }
                addr = (os_vm_address_t)aligned_start;
            }
#endif
            FSHOW((stderr, "/mapping %ld(0x%lx) bytes at 0x%lx\n",
                   len, len, (uword_t)addr));
            if (compressed) {
#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
                inflate_core_bytes(fd, offset + file_offset, addr, len);
#else
                lose("This runtime was not built with zlib-compressed core support... aborting\n");
#endif
            } else {
#ifdef LISP_FEATURE_HPUX
                copy_core_bytes(fd, offset + file_offset, addr, len);
#else
                os_map(fd, offset + file_offset, addr, len);
#endif
            }
        }
        // Compute this only after we possibly affected 'addr' if relocatable
        lispobj *free_pointer = (lispobj *) addr + entry->nwords;

#ifdef MADV_MERGEABLE
        if ((merge_core_pages == 1)
            || ((merge_core_pages == -1) && compressed)) {
                madvise(addr, len, MADV_MERGEABLE);
        }
#endif
        FSHOW((stderr, "/space id = %ld, free pointer = %p\n",
               id, (uword_t)free_pointer));

        switch (id) {
        case DYNAMIC_CORE_SPACE_ID:
            if (len > dynamic_space_size) {
                fprintf(stderr,
                        "dynamic space too small for core: %luKiB required, %luKiB available.\n",
                        (unsigned long)len >> 10,
                        (unsigned long)dynamic_space_size >> 10);
                exit(1);
            }
#ifdef LISP_FEATURE_GENCGC
#  ifdef LISP_FEATURE_RELOCATABLE_HEAP
            if (DYNAMIC_SPACE_START != corefile_addr)
                relocate_heap((lispobj*)corefile_addr, (lispobj*)addr, len);
#  else
            if (addr != (os_vm_address_t)DYNAMIC_SPACE_START) {
                fprintf(stderr, "in core: %p; in runtime: %p \n",
                        (void*)addr, (void*)DYNAMIC_SPACE_START);
                lose("core/runtime address mismatch: DYNAMIC_SPACE_START\n");
            }
#  endif
#  ifdef LISP_FEATURE_IMMOBILE_SPACE
            // Delayed until after dynamic space has been mapped
            // so that writes into immobile space don't fault.
            write_protect_immobile_space();
#  endif
#else
            if ((addr != (os_vm_address_t)DYNAMIC_0_SPACE_START) &&
                (addr != (os_vm_address_t)DYNAMIC_1_SPACE_START)) {
                fprintf(stderr, "in core: %p; in runtime: %p or %p\n",
                        (void*)addr,
                        (void*)DYNAMIC_0_SPACE_START,
                        (void*)DYNAMIC_1_SPACE_START);
                lose("warning: core/runtime address mismatch: DYNAMIC_SPACE_START\n");
            }
#endif
#if defined(ALLOCATION_POINTER)
            SetSymbolValue(ALLOCATION_POINTER, (lispobj)free_pointer,0);
#else
            dynamic_space_free_pointer = free_pointer;
#endif
            /* For stop-and-copy GC, this will be whatever the GC was
             * using at the time. With GENCGC, this will always be
             * space 0. (We checked above that for GENCGC,
             * addr==DYNAMIC_SPACE_START.) */
            current_dynamic_space = (lispobj *)addr;
#ifdef LISP_FEATURE_LINUX
            anon_dynamic_space_start = addr + len;
            // This assertion is here because of the test in zero_pages_with_mmap()
            // which trusts that if addr > anon_dynamic_space_start
            // then addr did not come from any file mapping.
            gc_assert((lispobj)anon_dynamic_space_start > STATIC_SPACE_END);
#endif
            break;
        case STATIC_CORE_SPACE_ID:
            if (addr != (os_vm_address_t)STATIC_SPACE_START) {
                fprintf(stderr, "in core: %p - in runtime: %p\n",
                        (void*)addr, (void*)STATIC_SPACE_START);
                lose("core/runtime address mismatch: STATIC_SPACE_START\n");
            }
            break;
        case READ_ONLY_CORE_SPACE_ID:
            if (addr != (os_vm_address_t)READ_ONLY_SPACE_START) {
                fprintf(stderr, "in core: %p - in runtime: %p\n",
                        (void*)addr, (void*)READ_ONLY_SPACE_START);
                lose("core/runtime address mismatch: READ_ONLY_SPACE_START\n");
            }
            break;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
         // Immobile space is subdivided into fixed-size and variable-size.
         // There is no margin between the two, though for efficiency
         // they are written separately to eliminate waste in the core file.
        case IMMOBILE_FIXEDOBJ_CORE_SPACE_ID:
            if (addr != (os_vm_address_t)IMMOBILE_SPACE_START) {
                fprintf(stderr, "in core: %p - in runtime: %p\n",
                        (void*)addr, (void*)IMMOBILE_SPACE_START);
                lose("core/runtime address mismatch: IMMOBILE_SPACE_START\n");
            }
            immobile_space_coreparse(IMMOBILE_SPACE_START, len);
            break;
        case IMMOBILE_VARYOBJ_CORE_SPACE_ID:
            if (addr != (os_vm_address_t)IMMOBILE_VARYOBJ_SUBSPACE_START) {
                fprintf(stderr, "in core: %p - in runtime: %p\n",
                        (void*)addr, (void*)IMMOBILE_VARYOBJ_SUBSPACE_START);
                lose("core/runtime address mismatch: IMMOBILE_VARYOBJ_SUBSPACE_START\n");
            }
            immobile_space_coreparse(IMMOBILE_VARYOBJ_SUBSPACE_START, len);
            break;
#endif
        default:
            lose("unknown space ID %ld addr %p\n", id, addr);
        }
    }
}

lispobj
load_core_file(char *file, os_vm_offset_t file_offset)
{
    void *header;
#ifndef LISP_FEATURE_ALPHA
    word_t val, *ptr;
#else
    u32 val, *ptr;
#endif
    os_vm_size_t len, remaining_len;
    int fd = open_binary(file, O_RDONLY);
    ssize_t count;
    lispobj initial_function = NIL;

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

    while (val != END_CORE_ENTRY_TYPE_CODE) {
        val = *ptr++;
        len = *ptr++;
        remaining_len = len - 2; /* (-2 to cancel the two ++ operations) */
        FSHOW((stderr, "/val=0x%"WORD_FMTX", remaining_len=0x%"WORD_FMTX"\n",
               val, remaining_len));

        switch (val) {

        case END_CORE_ENTRY_TYPE_CODE:
            SHOW("END_CORE_ENTRY_TYPE_CODE case");
            break;

        case BUILD_ID_CORE_ENTRY_TYPE_CODE:
            SHOW("BUILD_ID_CORE_ENTRY_TYPE_CODE case");
            {
                os_vm_size_t i;

                FSHOW((stderr, "build_id[]=\"%s\"\n", build_id));
                FSHOW((stderr, "remaining_len = %d\n", remaining_len));
                if (remaining_len != strlen((const char *)build_id))
                    goto losing_build_id;
                for (i = 0; i < remaining_len; ++i) {
                    FSHOW((stderr, "ptr[%d] = char = %d, expected=%d\n",
                           i, ptr[i], build_id[i]));
                    if (ptr[i] != build_id[i])
                        goto losing_build_id;
                }
                break;
            losing_build_id:
                /* .core files are not binary-compatible between
                 * builds because we can't easily detect whether the
                 * sources were patched between the time the
                 * dumping-the-.core runtime was built and the time
                 * that the loading-the-.core runtime was built.
                 *
                 * (We could easily detect whether version.lisp-expr
                 * was changed, but people experimenting with patches
                 * don't necessarily update version.lisp-expr.) */

                fprintf(stderr, "core was built for runtime '");
                for (i = 0; i < remaining_len; ++i) putc(ptr[i], stderr);
                fprintf(stderr, "' but this is '%s'\n", build_id);
                lose("can't load .core for different runtime, sorry\n");
            }

        case NEW_DIRECTORY_CORE_ENTRY_TYPE_CODE:
            SHOW("NEW_DIRECTORY_CORE_ENTRY_TYPE_CODE case");
            process_directory(fd,
                              ptr,
#ifndef LISP_FEATURE_ALPHA
                              remaining_len / (sizeof(struct ndir_entry) /
                                               sizeof(lispobj)),
#else
                              remaining_len / (sizeof(struct ndir_entry) /
                                               sizeof(u32)),
#endif
                              file_offset);
            break;

        case INITIAL_FUN_CORE_ENTRY_TYPE_CODE:
            SHOW("INITIAL_FUN_CORE_ENTRY_TYPE_CODE case");
            initial_function = adjust_word((lispobj)*ptr);
            break;

#ifdef LISP_FEATURE_GENCGC
        case PAGE_TABLE_CORE_ENTRY_TYPE_CODE:
        {
            os_vm_size_t remaining = *ptr;
            os_vm_size_t fdoffset = (*(ptr+1) + 1) * (os_vm_page_size);
            page_index_t page = 0, npages;
            ssize_t bytes_read;
            char data[8192];
            // A corefile_pte is 10 bytes for x86-64
            // Process an integral number of ptes on each read.
            os_vm_size_t chunksize = sizeof (struct corefile_pte)
                * (sizeof data / sizeof (struct corefile_pte));
            lseek(fd, fdoffset + file_offset, SEEK_SET);
            bytes_read = read(fd, &npages, sizeof npages);
            gc_assert(bytes_read == sizeof npages);
            remaining -= sizeof npages;
            while ((bytes_read = read(fd, data,
                                      remaining < chunksize ? remaining : chunksize)) > 0) {

                int i = 0;
                remaining -= bytes_read;
                while (bytes_read) {
                    bytes_read -= sizeof(struct corefile_pte);
                    /* Ignore all zeroes. The size of the page table
                     * core entry was rounded up to os_vm_page_size
                     * during the save, and might now have more
                     * elements than the page table.
                     *
                     * The low bits of each word are allocation flags.
                     */
                    struct corefile_pte pte;
                    memcpy(&pte, data+i*sizeof (struct corefile_pte), sizeof pte);
                    set_page_bytes_used(page, pte.bytes_used);
                    set_page_scan_start_offset(page, pte.sso & ~0x03);
                    page_table[page].allocated = pte.sso & 0x03;
                    if (++page == npages) // break out of both loops
                        goto done;
                    i++;
                }
            }
          done:

            gencgc_partial_pickup = 1;
            break;
        }
#endif
        default:
            lose("unknown core file entry: 0x%"WORD_FMTX"\n", val);
        }

        ptr += remaining_len;
        FSHOW((stderr, "/new ptr=0x%"WORD_FMTX"\n", ptr));
    }
    SHOW("about to free(header)");
    free(header);
    close(fd);
    SHOW("returning from load_core_file(..)");
    return initial_function;
}

#include "genesis/hash-table.h"
#include "genesis/vector.h"
os_vm_address_t get_asm_routine_by_name(const char* name)
{
    lispobj routines = SYMBOL(ASSEMBLER_ROUTINES)->value;
    if (lowtag_of(routines) == INSTANCE_POINTER_LOWTAG) {
        struct hash_table* ht = (struct hash_table*)native_pointer(routines);
        struct vector* table = VECTOR(ht->table);
        lispobj sym;
        int i;
        for (i=2 ; i < fixnum_value(table->length) ; i += 2) {
          sym = table->data[i];
          if (lowtag_of(sym) == OTHER_POINTER_LOWTAG
              && widetag_of(SYMBOL(sym)->header) == SYMBOL_WIDETAG
              && !strcmp(name, (char*)(VECTOR(SYMBOL(sym)->name)->data)))
              return (os_vm_address_t)fixnum_value(table->data[i+1]);
        }
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
