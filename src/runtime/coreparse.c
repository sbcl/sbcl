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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>

#include "sbcl.h"
#include "os.h"
#include "runtime.h"
#include "globals.h"
#include "core.h"
#include "arch.h"
#include "interr.h"
#include "thread.h"

#include "validate.h"
#include "gc-internal.h"

/* lutex stuff */
#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_SB_LUTEX)
#include "genesis/sap.h"
#include "pthread-lutex.h"
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
    size_t optarray[RUNTIME_OPTIONS_WORDS];
    struct runtime_options *options = NULL;

    if (read(fd, optarray, RUNTIME_OPTIONS_WORDS * sizeof(size_t)) !=
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

    if (new_runtime_options = read_runtime_options(fd)) {
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

static void
process_directory(int fd, lispobj *ptr, int count, os_vm_offset_t file_offset)
{
    struct ndir_entry *entry;

    FSHOW((stderr, "/process_directory(..), count=%d\n", count));

    for (entry = (struct ndir_entry *) ptr; --count>= 0; ++entry) {

        long id = entry->identifier;
        long offset = os_vm_page_size * (1 + entry->data_page);
        os_vm_address_t addr =
            (os_vm_address_t) (os_vm_page_size * entry->address);
        lispobj *free_pointer = (lispobj *) addr + entry->nwords;
        unsigned long len = os_vm_page_size * entry->page_count;
        if (len != 0) {
            os_vm_address_t real_addr;
            FSHOW((stderr, "/mapping %ld(0x%lx) bytes at 0x%lx\n",
                   (long)len, (long)len, (unsigned long)addr));
#ifdef LISP_FEATURE_HPUX
            real_addr = copy_core_bytes(fd, offset + file_offset, addr, len);
#else
            real_addr = os_map(fd, offset + file_offset, addr, len);
#endif
            if (real_addr != addr) {
                lose("file mapped in wrong place! "
                     "(0x%08x != 0x%08lx)\n",
                     real_addr,
                     addr);
            }
        }

        FSHOW((stderr, "/space id = %ld, free pointer = 0x%lx\n",
               id, (unsigned long)free_pointer));

        switch (id) {
        case DYNAMIC_CORE_SPACE_ID:
            if (len > dynamic_space_size) {
                fprintf(stderr,
                        "dynamic space too small for core: %ldKiB required, %ldKiB available.\n",
                        len >> 10,
                        (long)dynamic_space_size >> 10);
                exit(1);
            }
#ifdef LISP_FEATURE_GENCGC
            if (addr != (os_vm_address_t)DYNAMIC_SPACE_START) {
                fprintf(stderr, "in core: 0x%lx; in runtime: 0x%lx \n",
                        (long)addr, (long)DYNAMIC_SPACE_START);
                lose("core/runtime address mismatch: DYNAMIC_SPACE_START\n");
            }
#else
            if ((addr != (os_vm_address_t)DYNAMIC_0_SPACE_START) &&
                (addr != (os_vm_address_t)DYNAMIC_1_SPACE_START)) {
                fprintf(stderr, "in core: 0x%lx; in runtime: 0x%lx or 0x%lx\n",
                        (long)addr,
                        (long)DYNAMIC_0_SPACE_START,
                        (long)DYNAMIC_1_SPACE_START);
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
                fprintf(stderr, "in core: 0x%lx - in runtime: 0x%lx\n",
                        (long)addr, (long)STATIC_SPACE_START);
                lose("core/runtime address mismatch: STATIC_SPACE_START\n");
            }
            break;
        case READ_ONLY_CORE_SPACE_ID:
            if (addr != (os_vm_address_t)READ_ONLY_SPACE_START) {
                fprintf(stderr, "in core: 0x%lx - in runtime: 0x%lx\n",
                        (long)addr, (long)READ_ONLY_SPACE_START);
                lose("core/runtime address mismatch: READ_ONLY_SPACE_START\n");
            }
            break;
        default:
            lose("unknown space ID %ld addr 0x%lx\n", id, (long)addr);
        }
    }
}

lispobj
load_core_file(char *file, os_vm_offset_t file_offset)
{
    lispobj *header, val, len, *ptr, remaining_len;
    int fd = open_binary(file, O_RDONLY);
    unsigned int count;

    lispobj initial_function = NIL;
    FSHOW((stderr, "/entering load_core_file(%s)\n", file));
    if (fd < 0) {
        fprintf(stderr, "could not open file \"%s\"\n", file);
        perror("open");
        exit(1);
    }

    lseek(fd, file_offset, SEEK_SET);
    header = calloc(os_vm_page_size / sizeof(u32), sizeof(u32));

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
        FSHOW((stderr, "/val=0x%ld, remaining_len=0x%ld\n",
               (long)val, (long)remaining_len));

        switch (val) {

        case END_CORE_ENTRY_TYPE_CODE:
            SHOW("END_CORE_ENTRY_TYPE_CODE case");
            break;

        case VERSION_CORE_ENTRY_TYPE_CODE:
            SHOW("VERSION_CORE_ENTRY_TYPE_CODE case");
            if (*ptr != SBCL_CORE_VERSION_INTEGER) {
                lose("core file version (%d) != runtime library version (%d)\n",
                     *ptr,
                     SBCL_CORE_VERSION_INTEGER);
            }
            break;

        case BUILD_ID_CORE_ENTRY_TYPE_CODE:
            SHOW("BUILD_ID_CORE_ENTRY_TYPE_CODE case");
            {
                unsigned int i;

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
                                               sizeof(long)),
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

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_SB_LUTEX)
        case LUTEX_TABLE_CORE_ENTRY_TYPE_CODE:
            SHOW("LUTEX_TABLE_CORE_ENTRY_TYPE_CODE case");
            {
                size_t n_lutexes = *ptr;
                size_t fdoffset = (*(ptr + 1) + 1) * (os_vm_page_size);
                size_t data_length = n_lutexes * sizeof(struct sap *);
                struct lutex **lutexes_to_resurrect = malloc(data_length);
                long bytes_read;

                lseek(fd, fdoffset + file_offset, SEEK_SET);

                FSHOW((stderr, "attempting to read %ld lutexes from core\n", n_lutexes));
                bytes_read = read(fd, lutexes_to_resurrect, data_length);

                /* XXX */
                if (bytes_read != data_length) {
                    lose("Could not read the lutex table");
                }
                else {
                    int i;

                    for (i=0; i<n_lutexes; ++i) {
                        struct lutex *lutex = lutexes_to_resurrect[i];

                        FSHOW((stderr, "re-init'ing lutex @ %p\n", lutex));
                        lutex_init((tagged_lutex_t) lutex);
                    }

                    free(lutexes_to_resurrect);
                }
                break;
            }
#endif

#ifdef LISP_FEATURE_GENCGC
        case PAGE_TABLE_CORE_ENTRY_TYPE_CODE:
        {
            size_t size = *ptr;
            size_t fdoffset = (*(ptr+1) + 1) * (os_vm_page_size);
            size_t offset = 0;
            long bytes_read;
            unsigned long data[4096];
            unsigned long word;
            lseek(fd, fdoffset + file_offset, SEEK_SET);
            while ((bytes_read = read(fd, data, (size < 4096 ? size : 4096 )))
                    > 0)
            {
                int i = 0;
                size -= bytes_read;
                while (bytes_read) {
                    bytes_read -= sizeof(long);
                    /* Ignore all zeroes. The size of the page table
                     * core entry was rounded up to os_vm_page_size
                     * during the save, and might now have more
                     * elements than the page table.
                     *
                     * The low bits of each word are allocation flags.
                     */
                    if (word=data[i]) {
                        page_table[offset].region_start_offset = word & ~0x03;
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
            lose("unknown core file entry: %ld\n", (long)val);
        }

        ptr += remaining_len;
        FSHOW((stderr, "/new ptr=%lx\n", (unsigned long)ptr));
    }
    SHOW("about to free(header)");
    free(header);
    SHOW("returning from load_core_file(..)");
    return initial_function;
}

