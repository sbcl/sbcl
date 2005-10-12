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

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#include <sys/file.h>

#include "sbcl.h"
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

static void
write_lispobj(lispobj obj, FILE *file)
{
    fwrite(&obj, sizeof(lispobj), 1, file);
}

static long
write_bytes(FILE *file, char *addr, long bytes)
{
    long count, here, data;

    bytes = (bytes+os_vm_page_size-1)&~(os_vm_page_size-1);

    fflush(file);
    here = ftell(file);
    fseek(file, 0, 2);
    data = (ftell(file)+os_vm_page_size-1)&~(os_vm_page_size-1);
    fseek(file, data, 0);

    while (bytes > 0) {
        count = fwrite(addr, 1, bytes, file);
        if (count > 0) {
            bytes -= count;
            addr += count;
        }
        else {
            perror("error writing to save file");
            bytes = 0;
        }
    }
    fflush(file);
    fseek(file, here, 0);
    return data/os_vm_page_size - 1;
}

static void
output_space(FILE *file, int id, lispobj *addr, lispobj *end)
{
    int words, bytes, data;
    static char *names[] = {NULL, "dynamic", "static", "read-only"};

    write_lispobj(id, file);
    words = end - addr;
    write_lispobj(words, file);

    bytes = words * sizeof(lispobj);

    printf("writing %d bytes from the %s space at 0x%08lx\n",
           bytes, names[id], (unsigned long)addr);

    data = write_bytes(file, (char *)addr, bytes);

    write_lispobj(data, file);
    write_lispobj((long)addr / os_vm_page_size, file);
    write_lispobj((bytes + os_vm_page_size - 1) / os_vm_page_size, file);
}

FILE *
open_core_for_saving(char *filename)
{
    /* Open the output file. We don't actually need the file yet, but
     * the fopen() might fail for some reason, and we want to detect
     * that and back out before we do anything irreversible. */
    unlink(filename);
    return fopen(filename, "w");
}

boolean
save_to_filehandle(FILE *file, char *filename, lispobj init_function)
{
    struct thread *th;

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

    write_lispobj(CORE_MAGIC, file);

    write_lispobj(VERSION_CORE_ENTRY_TYPE_CODE, file);
    write_lispobj(3, file);
    write_lispobj(SBCL_CORE_VERSION_INTEGER, file);

    write_lispobj(BUILD_ID_CORE_ENTRY_TYPE_CODE, file);
    write_lispobj(/* (We're writing the word count of the entry here, and the 2
          * term is one word for the leading BUILD_ID_CORE_ENTRY_TYPE_CODE
          * word and one word where we store the count itself.) */
         2 + strlen((const char *)build_id),
         file);
    {
        unsigned char *p;
        for (p = build_id; *p; ++p)
            write_lispobj(*p, file);
    }

    write_lispobj(NEW_DIRECTORY_CORE_ENTRY_TYPE_CODE, file);
    write_lispobj(/* (word count = 3 spaces described by 5 words each, plus the
          * entry type code, plus this count itself) */
         (5*3)+2, file);
    output_space(file,
                 READ_ONLY_CORE_SPACE_ID,
                 (lispobj *)READ_ONLY_SPACE_START,
                 (lispobj *)SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0));
    output_space(file,
                 STATIC_CORE_SPACE_ID,
                 (lispobj *)STATIC_SPACE_START,
                 (lispobj *)SymbolValue(STATIC_SPACE_FREE_POINTER,0));
#ifdef reg_ALLOC
    output_space(file,
                 DYNAMIC_CORE_SPACE_ID,
                 (lispobj *)current_dynamic_space,
                 dynamic_space_free_pointer);
#else
#ifdef LISP_FEATURE_GENCGC
    /* Flush the current_region, updating the tables. */
    gc_alloc_update_all_page_tables();
    update_dynamic_space_free_pointer();
#endif
    output_space(file,
                 DYNAMIC_CORE_SPACE_ID,
                 (lispobj *)DYNAMIC_SPACE_START,
                 (lispobj *)SymbolValue(ALLOCATION_POINTER,0));
#endif

    write_lispobj(INITIAL_FUN_CORE_ENTRY_TYPE_CODE, file);
    write_lispobj(3, file);
    write_lispobj(init_function, file);

#ifdef LISP_FEATURE_GENCGC
    {
        size_t size = (last_free_page*sizeof(long)+os_vm_page_size-1)
            &~(os_vm_page_size-1);
        long *data = calloc(size, 1);
        if (data) {
            long offset;
            int i;
            for (i = 0; i < last_free_page; i++) {
                data[i] = page_table[i].first_object_offset;
            }
            write_lispobj(PAGE_TABLE_CORE_ENTRY_TYPE_CODE, file);
            write_lispobj(4, file);
            write_lispobj(size, file);
            offset = write_bytes(file, (char *) data, size);
            write_lispobj(offset, file);
        }
    }
#endif

    write_lispobj(END_CORE_ENTRY_TYPE_CODE, file);

    fclose(file);
    printf("done]\n");

    exit(0);
}

boolean
save(char *filename, lispobj init_function)
{
    FILE *file = open_core_for_saving(filename);

    if (!file) {
        perror(filename);
        return 1;
    }

    return save_to_filehandle(file, filename, init_function);
}
