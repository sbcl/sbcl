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
#include <signal.h>
#include <sys/file.h>

#include "runtime.h"
#include "os.h"
#include "sbcl.h"
#include "core.h"
#include "globals.h"
#include "save.h"
#include "dynbind.h"
#include "lispregs.h"
#include "validate.h"

#ifdef GENCGC
#include "gencgc.h"
#endif

static long
write_bytes(FILE *file, char *addr, long bytes)
{
    long count, here, data;

    bytes = (bytes+CORE_PAGESIZE-1)&~(CORE_PAGESIZE-1);

    fflush(file);
    here = ftell(file);
    fseek(file, 0, 2);
    data = (ftell(file)+CORE_PAGESIZE-1)&~(CORE_PAGESIZE-1);
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
    return data/CORE_PAGESIZE - 1;
}

static void
output_space(FILE *file, int id, lispobj *addr, lispobj *end)
{
    int words, bytes, data;
    static char *names[] = {NULL, "dynamic", "static", "read-only"};

    putw(id, file);
    words = end - addr;
    putw(words, file);

    bytes = words * sizeof(lispobj);

    printf("writing %d bytes from the %s space at 0x%08lx\n",
           bytes, names[id], (unsigned long)addr);

    data = write_bytes(file, (char *)addr, bytes);

    putw(data, file);
    putw((long)addr / CORE_PAGESIZE, file);
    putw((bytes + CORE_PAGESIZE - 1) / CORE_PAGESIZE, file);
}

boolean
save(char *filename, lispobj init_function)
{
    FILE *file;
#if defined WANT_CGC
    volatile lispobj*func_ptr = &init_function;
    char sbuf[128];
    strcpy(sbuf,filename);
    filename=sbuf;
    /* Get rid of remnant stuff. This is a MUST so that the memory
     * manager can get started correctly when we restart after this
     * save. Purify is going to maybe move the args so we need to
     * consider them volatile, especially if the gcc optimizer is
     * working!! */
    purify(NIL,NIL);

    init_function = *func_ptr;
    /* Set dynamic space pointer to base value so we don't write out
     * MBs of just cleared heap. */
    if(SymbolValue(X86_CGC_ACTIVE_P) != NIL)
      SetSymbolValue(ALLOCATION_POINTER, DYNAMIC_SPACE_START);
#endif
    /* Open the file: */
    unlink(filename);
    file = fopen(filename, "w");
    if (file == NULL) {
        perror(filename);
        return 1;
    }
    printf("[undoing binding stack... ");
    fflush(stdout);
    unbind_to_here((lispobj *)BINDING_STACK_START);
    SetSymbolValue(CURRENT_CATCH_BLOCK, 0);
    SetSymbolValue(CURRENT_UNWIND_PROTECT_BLOCK, 0);
    SetSymbolValue(EVAL_STACK_TOP, 0);
    printf("done]\n");
#if defined WANT_CGC && defined X86_CGC_ACTIVE_P
    SetSymbolValue(X86_CGC_ACTIVE_P, T);
#endif
    printf("[saving current Lisp image into %s:\n", filename);

    putw(CORE_MAGIC, file);

    putw(CORE_VERSION, file);
    putw(3, file);
    putw(SBCL_CORE_VERSION_INTEGER, file);

    putw(CORE_NDIRECTORY, file);
    putw((5*3)+2, file);

    output_space(file, READ_ONLY_SPACE_ID, (lispobj *)READ_ONLY_SPACE_START,
		 (lispobj *)SymbolValue(READ_ONLY_SPACE_FREE_POINTER));
    output_space(file, STATIC_SPACE_ID, (lispobj *)STATIC_SPACE_START,
		 (lispobj *)SymbolValue(STATIC_SPACE_FREE_POINTER));
#ifdef reg_ALLOC
    output_space(file, DYNAMIC_SPACE_ID, (lispobj *)DYNAMIC_SPACE_START,
		 dynamic_space_free_pointer);
#else
#ifdef GENCGC
    /* Flush the current_region updating the tables. */
    gc_alloc_update_page_tables(0,&boxed_region);
    gc_alloc_update_page_tables(1,&unboxed_region);
    update_x86_dynamic_space_free_pointer();
#endif
    output_space(file, DYNAMIC_SPACE_ID, (lispobj *)DYNAMIC_SPACE_START,
		 (lispobj *)SymbolValue(ALLOCATION_POINTER));
#endif

    putw(CORE_INITIAL_FUNCTION, file);
    putw(3, file);
    putw(init_function, file);

    putw(CORE_END, file);
    fclose(file);

    printf("done]\n");

    exit(0);
}
