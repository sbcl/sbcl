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
#include <errno.h>
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
#include "arch.h"
#include "pseudo-atomic.h"

#include "genesis/static-symbols.h"
#include "genesis/symbol.h"

#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
# include <zlib.h>
#endif

#if defined(LISP_FEATURE_SB_ELF_CORE)
# include "sbcl_elf.h"

# define SECTION_START_REFERENCE(name) \
        int __section_ ## name __attribute__((weak)) = 0
#endif

void smash_enclosing_state(lispobj init_function);

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
                perror("error writing to core file");
                lose("core file is incomplete or corrupt\n");
            }
        }
#ifdef LISP_FEATURE_SB_CORE_COMPRESSION
    } else if ((compression >= -1) && (compression <= 9)) {
# define ZLIB_BUFFER_SIZE (1u<<16)
        z_stream stream;
        unsigned char* buf = successful_malloc(ZLIB_BUFFER_SIZE);
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
            stream.avail_out = ZLIB_BUFFER_SIZE;
            stream.next_out = buf;
            ret = deflate(&stream, Z_FINISH);
            if (ret < 0) lose("zlib deflate error: %i... exiting\n", ret);
            written = buf;
            end     = buf+ZLIB_BUFFER_SIZE-stream.avail_out;
            total_written += end - written;
            while (written < end) {
                long count = fwrite(written, 1, end-written, file);
                if (count > 0) {
                    written += count;
                } else {
                    perror("error writing to core file");
                    lose("core file is incomplete or corrupt\n");
                }
            }
        } while (stream.avail_out == 0);
        deflateEnd(&stream);
        free(buf);
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

    if (fflush(file) != 0) {
      perror("error writing to core file");
      lose("core file is incomplete or corrupt\n");
    }
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

static long __attribute__((__unused__))
write_bytes(FILE *file, char *addr, long bytes, os_vm_offset_t file_offset)
{
    return write_and_compress_bytes(file, addr, bytes, file_offset,
                                    COMPRESSION_LEVEL_NONE);
}

extern struct lisp_startup_options lisp_startup_options;

static void
output_space(FILE *file, int id, lispobj *addr, lispobj *end,
             os_vm_offset_t file_offset,
             int core_compression_level)
{
    size_t words, bytes, data, compressed_flag;
    static char *names[] = {NULL, "dynamic", "static", "read-only",
                            "immobile", "immobile"};

    compressed_flag
            = ((core_compression_level != COMPRESSION_LEVEL_NONE)
               ? DEFLATED_CORE_SPACE_ID_FLAG : 0);

    write_lispobj(id | compressed_flag, file);
    words = end - addr;
    write_lispobj(words, file);

    bytes = words * sizeof(lispobj);

    if (!lisp_startup_options.noinform)
        printf("writing %lu bytes from the %s space at %p\n",
               (long unsigned)bytes, names[id], addr);

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

#ifdef LISP_FEATURE_IMMOBILE_SPACE
extern void prepare_immobile_space_for_save();
#  define N_SPACES_TO_SAVE 5
#  ifdef LISP_FEATURE_IMMOBILE_CODE
lispobj code_component_order;
extern void defrag_immobile_space(lispobj,boolean);
#  endif
#else
#  define N_SPACES_TO_SAVE 3
#endif
boolean
save_to_filehandle(FILE *file, char *filename, lispobj init_function,
                   boolean make_executable,
                   boolean save_runtime_options,
                   int core_compression_level)
{
    os_vm_offset_t core_start_pos;
    boolean verbose = !lisp_startup_options.noinform;

    smash_enclosing_state(init_function);

    if (verbose) {
        printf("[saving current Lisp image into %s:\n", filename);
        fflush(stdout);
    }

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
    write_lispobj(/* (word count = N spaces described by 5 words each, plus the
          * entry type code, plus this count itself) */
         (5*N_SPACES_TO_SAVE)+2, file);
    output_space(file,
                 READ_ONLY_CORE_SPACE_ID,
                 (lispobj *)READ_ONLY_SPACE_START,
                 read_only_space_free_pointer,
                 core_start_pos,
                 core_compression_level);
    output_space(file,
                 STATIC_CORE_SPACE_ID,
                 (lispobj *)STATIC_SPACE_START,
                 static_space_free_pointer,
                 core_start_pos,
                 core_compression_level);
#ifdef LISP_FEATURE_GENCGC
    /* Flush the current_region, updating the tables. */
    gc_alloc_update_all_page_tables(1);
    update_dynamic_space_free_pointer();
#endif
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    prepare_immobile_space_for_save();
    output_space(file,
                 IMMOBILE_FIXEDOBJ_CORE_SPACE_ID,
                 (lispobj *)IMMOBILE_SPACE_START,
                 immobile_fixedobj_free_pointer,
                 core_start_pos,
                 core_compression_level);
    output_space(file,
                 IMMOBILE_VARYOBJ_CORE_SPACE_ID,
                 (lispobj *)IMMOBILE_VARYOBJ_SUBSPACE_START,
                 immobile_space_free_pointer,
                 core_start_pos,
                 core_compression_level);
#endif
    output_space(file,
                 DYNAMIC_CORE_SPACE_ID,
                 current_dynamic_space,
                 (lispobj *)get_alloc_pointer(),
                 core_start_pos,
                 core_compression_level);

    write_lispobj(INITIAL_FUN_CORE_ENTRY_TYPE_CODE, file);
    write_lispobj(3, file);
    write_lispobj(init_function, file);

#ifdef LISP_FEATURE_GENCGC
    {
        size_t true_size = sizeof last_free_page
            + (last_free_page * sizeof(struct corefile_pte));
        size_t rounded_size = CEILING(true_size, os_vm_page_size);
        char* data = successful_malloc(rounded_size);
        *(page_index_t*)data = last_free_page;
        struct corefile_pte *ptes = (struct corefile_pte*)(data + sizeof(page_index_t));
        page_index_t i;
        for (i = 0; i < last_free_page; i++) {
                /* Thanks to alignment requirements, the two low bits
                 * are always zero, so we can use them to store the
                 * allocation type -- region is always closed, so only
                 * the two low bits of allocation flags matter. */
                uword_t word = page_scan_start_offset(i);
                gc_assert((word & 0x03) == 0);
                ptes[i].sso = word | (0x03 & page_table[i].allocated);
                ptes[i].bytes_used = page_bytes_used(i);
        }
        write_lispobj(PAGE_TABLE_CORE_ENTRY_TYPE_CODE, file);
        write_lispobj(4, file);
        write_lispobj(rounded_size, file);
        sword_t offset = write_bytes(file, data, rounded_size, core_start_pos);
        write_lispobj(offset, file);
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

    if (verbose) printf("done]\n");
    exit(0);
}
#undef N_SPACES_TO_SAVE

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

    if (core_offset != -1 && size > (size_t) core_offset)
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

void
smash_enclosing_state(lispobj init_function) {
    struct thread *th;
    boolean verbose = !lisp_startup_options.noinform;

#ifdef LISP_FEATURE_X86_64
    untune_asm_routines_for_microarch();
#endif

    /* Smash the enclosing state. (Once we do this, there's no good
     * way to go back, which is a sufficient reason that this ends up
     * being SAVE-LISP-AND-DIE instead of SAVE-LISP-AND-GO-ON). */
    if (verbose) {
        printf("[undoing binding stack and other enclosing state... ");
        fflush(stdout);
    }
    for_each_thread(th) {       /* XXX really? */
        unbind_to_here((lispobj *)th->binding_stack_start,th);
        SetSymbolValue(CURRENT_CATCH_BLOCK, 0,th);
        SetSymbolValue(CURRENT_UNWIND_PROTECT_BLOCK, 0,th);
    }
    if (verbose) printf("done]\n");
#ifdef LISP_FEATURE_IMMOBILE_CODE
    // It's better to wait to defrag until after the binding stack is undone,
    // because we explicitly don't fixup code refs from stacks.
    // i.e. if there *were* something on the binding stack that cared that code
    // moved, it would be wrong. This way we can be sure we don't care.
    if (code_component_order) {
        // Assert that defrag will not move the init_function
        gc_assert(!immobile_space_p(init_function));
        if (verbose) {
            printf("[defragmenting immobile space... ");
            fflush(stdout);
        }
        defrag_immobile_space(code_component_order, verbose);
        if (verbose) printf("done]\n");
    }
#endif

    /* (Now we can actually start copying ourselves into the output file.) */
}

#if defined(LISP_FEATURE_SB_ELF_CORE)

int
save_elf_section(FILE *f, const char *section, size_t offset)
{
    return fprintf(f, "--section-start %s=0x%zx\n", section, offset);
}

int
save_linker_options(const char *filename, sbcl_elf_gc_area *areas, size_t size)
{
    const char *lds_ext = ".lds";
    char linker_opts_file[PATH_MAX];

    size_t flen = strlen(filename);
    size_t extlen = strlen(lds_ext);
    if (flen + extlen >= PATH_MAX) {
        errno = ENAMETOOLONG;
        return -1;
    }

    memcpy(linker_opts_file, filename, flen);
    memcpy(linker_opts_file + flen, lds_ext, extlen);
    linker_opts_file[flen + extlen] = '\0';

    FILE *f = fopen(linker_opts_file, "w");
    if (f == NULL) return -1;

    size_t i;
    for (i = 0; i < size; i++) {
        save_elf_section(f, areas[i].name, areas[i].start);
        save_elf_section(f, areas[i].zero_name, areas[i].free);
    }

    return fclose(f) == EOF ? -1 : 0;
}

SECTION_START_REFERENCE(__sbcl_readonly_start);
SECTION_START_REFERENCE(__sbcl_readonly_zero_start);
SECTION_START_REFERENCE(__sbcl_static_start);
SECTION_START_REFERENCE(__sbcl_static_zero_start);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
SECTION_START_REFERENCE(__sbcl_immobile_fixedobj_start);
SECTION_START_REFERENCE(__sbcl_immobile_fixedobj_zero_start);
SECTION_START_REFERENCE(__sbcl_immobile_varyobj_start);
SECTION_START_REFERENCE(__sbcl_immobile_varyobj_zero_start);
#endif
SECTION_START_REFERENCE(__sbcl_dynamic_start);
SECTION_START_REFERENCE(__sbcl_dynamic_zero_start);

boolean
save_elf_to_filehandle(int fd, char *filename, lispobj init_function)
{
    sbcl_elf e;
    sbcl_elf_open(&e, fd);

    smash_enclosing_state(init_function);

#if defined(LISP_FEATURE_GENCGC)
    /* Flush the current_region, updating the tables. */
     gc_alloc_update_all_page_tables(1);
     update_dynamic_space_free_pointer();
#endif

#ifdef LISP_FEATURE_IMMOBILE_SPACE
     prepare_immobile_space_for_save();
#endif

#if defined(LISP_FEATURE_GENCGC)
    lispobj *dyn_start = (lispobj *)DYNAMIC_SPACE_START;
#else
    lispobj *dyn_start = (lispobj *)current_dynamic_space;
#endif

#if defined(ALLOCATION_POINTER)
    lispobj *dyn_free = (lispobj *)SymbolValue(ALLOCATION_POINTER,0);
#else
    lispobj *dyn_free = dynamic_space_free_pointer;
#endif

    enum gc_spaces_t {
        READONLY,
        STATIC,
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        IMMOBILE_FIXEDOBJ,
        IMMOBILE_VARYOBJ,
#endif
        DYNAMIC
    };
    const int ELF_RO_CODE = SHF_ALLOC | SHF_EXECINSTR;
    const int ELF_RW_CODE = SHF_ALLOC | SHF_EXECINSTR | SHF_WRITE;
    sbcl_elf_gc_area areas[] = {
        {
            // Must be writeable because tune_asm_routines_for_microarch()
            // writes into the code object for FILL-VECTOR/T.
            // FIXME: figure out a way to move FILL-VECTOR/T elsewhere.
            .name       = ".sbcl_readonly",
            .flags      = ELF_RW_CODE,
            .refsym     = "__section_sbcl_readonly_start",
            .zero_name  = ".sbcl_readonly.zero",
            .zero_flags = ELF_RO_CODE,
            .zero_refsym = "__section_sbcl_readonly_zero_start",
            .start = (uintptr_t)READ_ONLY_SPACE_START,
            .free  = (uintptr_t)read_only_space_free_pointer,
            .end   = (uintptr_t)READ_ONLY_SPACE_END,
        },
        {
            .name       = ".sbcl_static",
            .flags      = ELF_RW_CODE,
            .refsym     = "__section_sbcl_static_start",
            .zero_name  = ".sbcl_static.zero",
            .zero_flags = ELF_RW_CODE,
            .zero_refsym = "__section_sbcl_static_zero_start",
            .start = (uintptr_t)STATIC_SPACE_START,
            .free  = (uintptr_t)static_space_free_pointer,
            .end   = (uintptr_t)STATIC_SPACE_END,
        },
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        {
            .name       = ".sbcl_immobile.fixedobj",
            .flags      = ELF_RW_CODE,
            .refsym     = "__section_sbcl_immobile_fixedobj_start",
            .zero_name  = ".sbcl_immobile.fixedobj.zero",
            .zero_flags = ELF_RW_CODE,
            .zero_refsym = "__section_sbcl_immobile_fixedobj_zero_start",
            .start = (uintptr_t)IMMOBILE_SPACE_START,
            .free  = (uintptr_t)immobile_fixedobj_free_pointer,
            .end   = (uintptr_t)IMMOBILE_VARYOBJ_SUBSPACE_START,
        },
        {
            .name       = ".sbcl_immobile.varyobj",
            .flags      = ELF_RW_CODE,
            .refsym     = "__section_sbcl_immobile_varyobj_start",
            .zero_name  = ".sbcl_immobile.varyobj.zero",
            .zero_flags = ELF_RW_CODE,
            .zero_refsym = "__section_sbcl_immobile_varyobj_zero_start",
            .start = (uintptr_t)IMMOBILE_VARYOBJ_SUBSPACE_START,
            .free  = (uintptr_t)immobile_space_free_pointer,
            .end   = (uintptr_t)IMMOBILE_SPACE_END,
        },
#endif
        {
            .name       = ".sbcl_dynamic",
            .flags      = ELF_RW_CODE,
            .refsym     = "__section_sbcl_dynamic_start",
            .zero_name  = ".sbcl_dynamic.zero",
            .zero_flags = ELF_RW_CODE,
            .zero_refsym = "__section_sbcl_dynamic_zero_start",
            .start = (uintptr_t)dyn_start,
            .free  = (uintptr_t)dyn_free,
            .end   = (uintptr_t)MAX_DYNAMIC_SPACE_END,
        },
    };

    linked_in_core_data_struct data;
    memset((void*)&data, 0, sizeof(data));

    data.found                     = true;
    data.initial_function          = init_function;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    data.immobile_fixedobj_space_offset = areas[IMMOBILE_FIXEDOBJ].start;
    data.immobile_fixedobj_space_size =
        areas[IMMOBILE_FIXEDOBJ].free - areas[IMMOBILE_FIXEDOBJ].start;
    data.immobile_varyobj_space_offset = areas[IMMOBILE_VARYOBJ].start;
    data.immobile_varyobj_space_size =
        areas[IMMOBILE_VARYOBJ].free - areas[IMMOBILE_VARYOBJ].start;
#endif
    data.dynamic_space_free_offset = areas[DYNAMIC].free;
    data.dynamic_space_size        = areas[DYNAMIC].end - areas[DYNAMIC].start;

    size_t nareas = sizeof(areas) / sizeof(sbcl_elf_gc_area);
    sbcl_elf_align_gc_areas(areas, nareas);
    sbcl_elf_output_gc_areas(&e, areas, nareas);

    // Mark the .o as not requiring an executable stack.
    // The non-NULL pointer ensures the section is marked PROGBITS.
    sbcl_elf_output_space(&e, ".note.GNU-stack", (void*)1, 0, 0);

    size_t rodata_shndx = sbcl_elf_output_space(
                              &e,
                              ".rodata",
                              (void*)&data,
                              sizeof(data),
                              SHF_ALLOC);
    sbcl_buffer rodata;
    sbcl_buffer_init(&rodata);

    size_t off = sbcl_buffer_add(&rodata, (void*)&data, sizeof(data));
    sbcl_elf_add_symtab_entry(
        &e,
        "linked_in_core_data",
        STB_GLOBAL,
        STT_OBJECT,
        rodata_shndx,
        off,
        sizeof(data));

    sbcl_elf_close(&e);

    close(fd);
    printf("Core saved.\n");

    if (save_linker_options(filename, areas, nareas) < 0) {
        lose("Failed to save linker options file");
    }

    exit(0);
}

boolean
save_elf_core(char *filename, lispobj init_function)
{
    FILE *file = open_core_for_saving(filename);
    if (file == NULL) {
        perror(filename);
        return 1;
    }
    int fd = fileno(file);

    return save_elf_to_filehandle(fd, filename, init_function);
}

#endif // LISP_FEATURE_SB_ELF_CORE

#ifdef LISP_FEATURE_CHENEYGC
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
#endif
