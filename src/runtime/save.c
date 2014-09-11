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

#ifndef LISP_FEATURE_WIN32
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/file.h>

#include "sbcl.h"
#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD)
#include "pthreads_win32.h"
#else
#include <signal.h>
#endif
#include "runtime.h"
#include "os.h"
#include "core.h"
#include "globals.h"
#include "save.h"
#include "dynbind.h"
#include "lispregs.h"
#include "validate.h"
#include "gc-internal.h"
#include "thread.h"

#include "genesis/static-symbols.h"
#include "genesis/symbol.h"

#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
# include <zlib.h>
#endif

/* write_runtime_options uses a simple serialization scheme that
 * consists of one word of magic, one word indicating whether options
 * are actually saved, and one word per struct field. */
static void
write_runtime_options(FILE *file, struct runtime_options *options)
{
    size_t optarray[RUNTIME_OPTIONS_WORDS];

    memset(&optarray, 0, sizeof(optarray));
    optarray[0] = RUNTIME_OPTIONS_MAGIC;

    if (options != NULL) {
        /* optarray[1] is a flag indicating that options are present */
        optarray[1] = 1;
        optarray[2] = options->dynamic_space_size;
        optarray[3] = options->thread_control_stack_size;
    }

    if (RUNTIME_OPTIONS_WORDS !=
        fwrite(optarray, sizeof(size_t), RUNTIME_OPTIONS_WORDS, file)) {
        perror("Error writing runtime options to file");
    }
}

static void
write_lispobj(lispobj obj, FILE *file)
{
    if (1 != fwrite(&obj, sizeof(lispobj), 1, file)) {
        perror("Error writing to file");
    }
}

static void
write_bytes_to_file(FILE * file, char *addr, long bytes, int compression)
{
    if (compression == COMPRESSION_LEVEL_NONE) {
        while (bytes > 0) {
            sword_t count = fwrite(addr, 1, bytes, file);
            if (count > 0) {
                bytes -= count;
                addr += count;
            }
            else {
                perror("error writing to save file");
                bytes = 0;
            }
        }
#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
    } else if ((compression >= -1) && (compression <= 9)) {
# define ZLIB_BUFFER_SIZE (1u<<16)
        z_stream stream;
        unsigned char buf[ZLIB_BUFFER_SIZE];
        unsigned char * written, * end;
        long total_written = 0;
        int ret;
        stream.zalloc = NULL;
        stream.zfree = NULL;
        stream.opaque = NULL;
        stream.avail_in = bytes;
        stream.next_in  = (void*)addr;
        ret = deflateInit(&stream, compression);
        if (ret != Z_OK)
            lose("deflateInit: %i\n", ret);
        do {
            stream.avail_out = sizeof(buf);
            stream.next_out = buf;
            ret = deflate(&stream, Z_FINISH);
            if (ret < 0) lose("zlib deflate error: %i... exiting\n", ret);
            written = buf;
            end     = buf+sizeof(buf)-stream.avail_out;
            total_written += end - written;
            while (written < end) {
                long count = fwrite(written, 1, end-written, file);
                if (count > 0) {
                    written += count;
                } else {
                    lose("unable to write to core file\n");
                }
            }
        } while (stream.avail_out == 0);
        deflateEnd(&stream);
        printf("compressed %lu bytes into %lu at level %i\n",
               bytes, total_written, compression);
# undef ZLIB_BUFFER_SIZE
#endif
    } else {
#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
        lose("Unknown core compression level %i, exiting\n", compression);
#else
        lose("zlib-compressed core support not built in this runtime\n");
#endif
    }

    fflush(file);
};


static long
write_and_compress_bytes(FILE *file, char *addr, long bytes, os_vm_offset_t file_offset,
                         int compression)
{
    long here, data;

    bytes = (bytes+os_vm_page_size-1)&~(os_vm_page_size-1);

#ifdef LISP_FEATURE_WIN32
    long count;
    /* touch every single page in the space to force it to be mapped. */
    for (count = 0; count < bytes; count += 0x1000) {
        volatile int temp = addr[count];
    }
#endif

    fflush(file);
    here = ftell(file);
    fseek(file, 0, SEEK_END);
    data = (ftell(file)+os_vm_page_size-1)&~(os_vm_page_size-1);
    fseek(file, data, SEEK_SET);
    write_bytes_to_file(file, addr, bytes, compression);
    fseek(file, here, SEEK_SET);
    return ((data - file_offset) / os_vm_page_size) - 1;
}

static long
write_bytes(FILE *file, char *addr, long bytes, os_vm_offset_t file_offset)
{
    return write_and_compress_bytes(file, addr, bytes, file_offset,
                                    COMPRESSION_LEVEL_NONE);
}

static void
output_space(FILE *file, int id, lispobj *addr, lispobj *end,
             os_vm_offset_t file_offset,
             int core_compression_level)
{
    size_t words, bytes, data, compressed_flag;
    static char *names[] = {NULL, "dynamic", "static", "read-only"};

    compressed_flag
            = ((core_compression_level != COMPRESSION_LEVEL_NONE)
               ? DEFLATED_CORE_SPACE_ID_FLAG : 0);

    write_lispobj(id | compressed_flag, file);
    words = end - addr;
    write_lispobj(words, file);

    bytes = words * sizeof(lispobj);

    printf("writing %lu bytes from the %s space at %p\n",
           bytes, names[id], addr);

    data = write_and_compress_bytes(file, (char *)addr, bytes, file_offset,
                                    core_compression_level);

    write_lispobj(data, file);
    write_lispobj((uword_t)addr / os_vm_page_size, file);
    write_lispobj((bytes + os_vm_page_size - 1) / os_vm_page_size, file);
}

FILE *
open_core_for_saving(char *filename)
{
    /* Open the output file. We don't actually need the file yet, but
     * the fopen() might fail for some reason, and we want to detect
     * that and back out before we do anything irreversible. */
    unlink(filename);
    return fopen(filename, "wb");
}

boolean
save_to_filehandle(FILE *file, char *filename, lispobj init_function,
                   boolean make_executable,
                   boolean save_runtime_options,
                   int core_compression_level)
{
    struct thread *th;
    os_vm_offset_t core_start_pos;

    /* Smash the enclosing state. (Once we do this, there's no good
     * way to go back, which is a sufficient reason that this ends up
     * being SAVE-LISP-AND-DIE instead of SAVE-LISP-AND-GO-ON). */
    printf("[undoing binding stack and other enclosing state... ");
    fflush(stdout);
    for_each_thread(th) {       /* XXX really? */
        unbind_to_here((lispobj *)th->binding_stack_start,th);
        SetSymbolValue(CURRENT_CATCH_BLOCK, 0,th);
        SetSymbolValue(CURRENT_UNWIND_PROTECT_BLOCK, 0,th);
    }
    printf("done]\n");
    fflush(stdout);

    /* (Now we can actually start copying ourselves into the output file.) */

    printf("[saving current Lisp image into %s:\n", filename);
    fflush(stdout);

    core_start_pos = ftell(file);
    write_lispobj(CORE_MAGIC, file);

    write_lispobj(BUILD_ID_CORE_ENTRY_TYPE_CODE, file);
    write_lispobj(/* (We're writing the word count of the entry here, and the 2
          * term is one word for the leading BUILD_ID_CORE_ENTRY_TYPE_CODE
          * word and one word where we store the count itself.) */
         2 + strlen((const char *)build_id),
         file);
    {
        unsigned char *p;
        for (p = (unsigned char *)build_id; *p; ++p)
            write_lispobj(*p, file);
    }

    write_lispobj(NEW_DIRECTORY_CORE_ENTRY_TYPE_CODE, file);
    write_lispobj(/* (word count = 3 spaces described by 5 words each, plus the
          * entry type code, plus this count itself) */
         (5*3)+2, file);
    output_space(file,
                 READ_ONLY_CORE_SPACE_ID,
                 (lispobj *)READ_ONLY_SPACE_START,
                 (lispobj *)SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0),
                 core_start_pos,
                 core_compression_level);
    output_space(file,
                 STATIC_CORE_SPACE_ID,
                 (lispobj *)STATIC_SPACE_START,
                 (lispobj *)SymbolValue(STATIC_SPACE_FREE_POINTER,0),
                 core_start_pos,
                 core_compression_level);
#ifdef LISP_FEATURE_GENCGC
    /* Flush the current_region, updating the tables. */
    gc_alloc_update_all_page_tables();
    update_dynamic_space_free_pointer();
#endif
#ifdef reg_ALLOC
#ifdef LISP_FEATURE_GENCGC
    output_space(file,
                 DYNAMIC_CORE_SPACE_ID,
                 (lispobj *)DYNAMIC_SPACE_START,
                 dynamic_space_free_pointer,
                 core_start_pos,
                 core_compression_level);
#else
    output_space(file,
                 DYNAMIC_CORE_SPACE_ID,
                 (lispobj *)current_dynamic_space,
                 dynamic_space_free_pointer,
                 core_start_pos,
                 core_compression_level);
#endif
#else
    output_space(file,
                 DYNAMIC_CORE_SPACE_ID,
                 (lispobj *)DYNAMIC_SPACE_START,
                 (lispobj *)SymbolValue(ALLOCATION_POINTER,0),
                 core_start_pos,
                 core_compression_level);
#endif

    write_lispobj(INITIAL_FUN_CORE_ENTRY_TYPE_CODE, file);
    write_lispobj(3, file);
    write_lispobj(init_function, file);

#ifdef LISP_FEATURE_GENCGC
    {
        size_t size = (last_free_page*sizeof(sword_t)+os_vm_page_size-1)
            &~(os_vm_page_size-1);
        uword_t *data = calloc(size, 1);
        if (data) {
            uword_t word;
            sword_t offset;
            page_index_t i;
            for (i = 0; i < last_free_page; i++) {
                /* Thanks to alignment requirements, the two low bits
                 * are always zero, so we can use them to store the
                 * allocation type -- region is always closed, so only
                 * the two low bits of allocation flags matter. */
                word = page_table[i].scan_start_offset;
                gc_assert((word & 0x03) == 0);
                data[i] = word | (0x03 & page_table[i].allocated);
            }
            write_lispobj(PAGE_TABLE_CORE_ENTRY_TYPE_CODE, file);
            write_lispobj(4, file);
            write_lispobj(size, file);
            offset = write_bytes(file, (char *)data, size, core_start_pos);
            write_lispobj(offset, file);
        }
    }
#endif

    write_lispobj(END_CORE_ENTRY_TYPE_CODE, file);

    /* Write a trailing header, ignored when parsing the core normally.
     * This is used to locate the start of the core when the runtime is
     * prepended to it. */
    fseek(file, 0, SEEK_END);

    /* If NULL runtime options are passed to write_runtime_options,
     * command-line processing is performed as normal in the SBCL
     * executable. Otherwise, the saved runtime options are used and
     * all command-line arguments are available to Lisp in
     * SB-EXT:*POSIX-ARGV*. */
    write_runtime_options(file,
                          (save_runtime_options ? runtime_options : NULL));

    if (1 != fwrite(&core_start_pos, sizeof(os_vm_offset_t), 1, file)) {
        perror("Error writing core starting position to file");
        fclose(file);
    } else {
        write_lispobj(CORE_MAGIC, file);
        fclose(file);
    }

#ifndef LISP_FEATURE_WIN32
    if (make_executable)
        chmod (filename, 0755);
#endif

    printf("done]\n");
    exit(0);
}

/* Check if the build_id for the current runtime is present in a
 * buffer. */
int
check_runtime_build_id(void *buf, size_t size)
{
    size_t idlen;
    char *pos;

    idlen = strlen((const char*)build_id) - 1;
    while ((pos = memchr(buf, build_id[0], size)) != NULL) {
        size -= (pos + 1) - (char *)buf;
        buf = (pos + 1);
        if (idlen <= size && memcmp(buf, build_id + 1, idlen) == 0)
            return 1;
    }

    return 0;
}

/* Slurp the executable portion of the runtime into a malloced buffer
 * and return it.  Places the size in bytes of the runtime into
 * 'size_out'.  Returns NULL if the runtime cannot be loaded from
 * 'runtime_path'. */
void *
load_runtime(char *runtime_path, size_t *size_out)
{
    void *buf = NULL;
    FILE *input = NULL;
    size_t size, count;
    os_vm_offset_t core_offset;

    core_offset = search_for_embedded_core (runtime_path);
    if ((input = fopen(runtime_path, "rb")) == NULL) {
        fprintf(stderr, "Unable to open runtime: %s\n", runtime_path);
        goto lose;
    }

    fseek(input, 0, SEEK_END);
    size = (size_t) ftell(input);
    fseek(input, 0, SEEK_SET);

    if (core_offset != -1 && size > core_offset)
        size = core_offset;

    buf = successful_malloc(size);
    if ((count = fread(buf, 1, size, input)) != size) {
        fprintf(stderr, "Premature EOF while reading runtime.\n");
        goto lose;
    }

    if (!check_runtime_build_id(buf, size)) {
        fprintf(stderr, "Failed to locate current build_id in runtime: %s\n",
            runtime_path);
        goto lose;
    }

    fclose(input);
    *size_out = size;
    return buf;

lose:
    if (input != NULL)
        fclose(input);
    if (buf != NULL)
        free(buf);
    return NULL;
}

boolean
save_runtime_to_filehandle(FILE *output, void *runtime, size_t runtime_size,
                           int application_type)
{
    size_t padding;
    void *padbytes;

#ifdef LISP_FEATURE_WIN32
    {
        PIMAGE_DOS_HEADER dos_header = (PIMAGE_DOS_HEADER)runtime;
        PIMAGE_NT_HEADERS nt_header = (PIMAGE_NT_HEADERS)((char *)dos_header +
                                                          dos_header->e_lfanew);

        int sub_system;
        switch (application_type) {
        case 0:
            sub_system = IMAGE_SUBSYSTEM_WINDOWS_CUI;
            break;
        case 1:
            sub_system = IMAGE_SUBSYSTEM_WINDOWS_GUI;
            break;
        default:
            fprintf(stderr, "Invalid application type %d\n", application_type);
            return 0;
        }

        nt_header->OptionalHeader.Subsystem = sub_system;
    }
#endif

    if (runtime_size != fwrite(runtime, 1, runtime_size, output)) {
        perror("Error saving runtime");
        return 0;
    }

    padding = (os_vm_page_size - (runtime_size % os_vm_page_size)) & ~os_vm_page_size;
    if (padding > 0) {
        padbytes = successful_malloc(padding);
        memset(padbytes, 0, padding);
        if (padding != fwrite(padbytes, 1, padding, output)) {
            perror("Error saving runtime");
            free(padbytes);
            return 0;
        }
        free(padbytes);
    }

    return 1;
}

FILE *
prepare_to_save(char *filename, boolean prepend_runtime, void **runtime_bytes,
                size_t *runtime_size)
{
    FILE *file;
    char *runtime_path;

    if (prepend_runtime) {
        runtime_path = os_get_runtime_executable_path(0);

        if (runtime_path == NULL && saved_runtime_path == NULL) {
            fprintf(stderr, "Unable to get default runtime path.\n");
            return NULL;
        }

        if (runtime_path == NULL)
            *runtime_bytes = load_runtime(saved_runtime_path, runtime_size);
        else {
            *runtime_bytes = load_runtime(runtime_path, runtime_size);
            free(runtime_path);
        }

        if (*runtime_bytes == NULL)
            return 0;
    }

    file = open_core_for_saving(filename);
    if (file == NULL) {
        free(*runtime_bytes);
        perror(filename);
        return NULL;
    }

    return file;
}

boolean
save(char *filename, lispobj init_function, boolean prepend_runtime,
     boolean save_runtime_options, boolean compressed, int compression_level,
     int application_type)
{
    FILE *file;
    void *runtime_bytes = NULL;
    size_t runtime_size;

    file = prepare_to_save(filename, prepend_runtime, &runtime_bytes, &runtime_size);
    if (file == NULL)
        return 1;

    if (prepend_runtime)
        save_runtime_to_filehandle(file, runtime_bytes, runtime_size, application_type);

    return save_to_filehandle(file, filename, init_function, prepend_runtime,
                              save_runtime_options,
                              compressed ? compressed : COMPRESSION_LEVEL_NONE);
}
