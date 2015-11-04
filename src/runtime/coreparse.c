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
os_vm_address_t copy_core_bytes(int fd, os_vm_offset_t offset,
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
  return addr;
}
#endif

#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
# define ZLIB_BUFFER_SIZE (1u<<16)
os_vm_address_t inflate_core_bytes(int fd, os_vm_offset_t offset,
                                   os_vm_address_t addr, int len)
{
    z_stream stream;
    unsigned char buf[ZLIB_BUFFER_SIZE];
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
    return addr;
}
# undef ZLIB_BUFFER_SIZE
#endif

int merge_core_pages = -1;

static void
process_directory(int fd, lispobj *ptr, int count, os_vm_offset_t file_offset)
{
    struct ndir_entry *entry;
    int compressed;

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
        os_vm_address_t addr =
            (os_vm_address_t) (os_vm_page_size * entry->address);
        lispobj *free_pointer = (lispobj *) addr + entry->nwords;
        uword_t len = os_vm_page_size * entry->page_count;
        if (len != 0) {
            os_vm_address_t real_addr;
            FSHOW((stderr, "/mapping %ld(0x%lx) bytes at 0x%lx\n",
                   len, len, (uword_t)addr));
            if (compressed) {
#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
                real_addr = inflate_core_bytes(fd, offset + file_offset, addr, len);
#else
                lose("This runtime was not built with zlib-compressed core support... aborting\n");
#endif
            } else {
#ifdef LISP_FEATURE_HPUX
                real_addr = copy_core_bytes(fd, offset + file_offset, addr, len);
#else
                real_addr = os_map(fd, offset + file_offset, addr, len);
#endif
            }
            if (real_addr != addr) {
                lose("file mapped in wrong place! "
                     "(0x%08x != 0x%08lx)\n",
                     real_addr,
                     addr);
            }
        }

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
                        "dynamic space too small for core: %ldKiB required, %ldKiB available.\n",
                        len >> 10,
                        (uword_t)dynamic_space_size >> 10);
                exit(1);
            }
#ifdef LISP_FEATURE_GENCGC
            if (addr != (os_vm_address_t)DYNAMIC_SPACE_START) {
                fprintf(stderr, "in core: %p; in runtime: %p \n",
                        (void*)addr, (void*)DYNAMIC_SPACE_START);
                lose("core/runtime address mismatch: DYNAMIC_SPACE_START\n");
            }
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
    if (count < os_vm_page_size) {
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
            initial_function = (lispobj)*ptr;
            break;

#ifdef LISP_FEATURE_GENCGC
        case PAGE_TABLE_CORE_ENTRY_TYPE_CODE:
        {
            os_vm_size_t size = *ptr;
            os_vm_size_t fdoffset = (*(ptr+1) + 1) * (os_vm_page_size);
            page_index_t offset = 0;
            ssize_t bytes_read;
            word_t data[4096];
            word_t word;
            lseek(fd, fdoffset + file_offset, SEEK_SET);
            while ((bytes_read = read(fd, data, (size < 4096 ? size : 4096 )))
                    > 0)
            {
                int i = 0;
                size -= bytes_read;
                while (bytes_read) {
                    bytes_read -= sizeof(word_t);
                    /* Ignore all zeroes. The size of the page table
                     * core entry was rounded up to os_vm_page_size
                     * during the save, and might now have more
                     * elements than the page table.
                     *
                     * The low bits of each word are allocation flags.
                     */
                    if ((word=data[i])) {
                        page_table[offset].scan_start_offset = word & ~0x03;
                        page_table[offset].allocated = word & 0x03;
                    }
                    i++;
                    offset++;
                }
            }

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
    SHOW("returning from load_core_file(..)");
    return initial_function;
}
