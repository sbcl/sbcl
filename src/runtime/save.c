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
#include "gc-internal.h"
#include "thread.h"

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

    putw(id, file);
    words = end - addr;
    putw(words, file);

    bytes = words * sizeof(lispobj);

    printf("writing %d bytes from the %s space at 0x%08lx\n",
           bytes, names[id], (unsigned long)addr);

    data = write_bytes(file, (char *)addr, bytes);

    putw(data, file);
    putw((long)addr / os_vm_page_size, file);
    putw((bytes + os_vm_page_size - 1) / os_vm_page_size, file);
}

boolean
save(char *filename, lispobj init_function)
{
    FILE *file;
    struct thread *th;

    /* Open the output file. We don't actually need the file yet, but
     * the fopen() might fail for some reason, and we want to detect
     * that and back out before we do anything irreversible. */
    unlink(filename);
    file = fopen(filename, "w");
    if (!file) {
        perror(filename);
        return 1;
    }

    /* Smash the enclosing state. (Once we do this, there's no good
     * way to go back, which is a sufficient reason that this ends up
     * being SAVE-LISP-AND-DIE instead of SAVE-LISP-AND-GO-ON). */
    printf("[undoing binding stack and other enclosing state... ");
    fflush(stdout);
    for_each_thread(th)	{	/* XXX really? */
	unbind_to_here((lispobj *)th->binding_stack_start,th);
	SetSymbolValue(CURRENT_CATCH_BLOCK, 0,th);
	SetSymbolValue(CURRENT_UNWIND_PROTECT_BLOCK, 0,th);
    }
    printf("done]\n");
    fflush(stdout);
    
    /* (Now we can actually start copying ourselves into the output file.) */

    printf("[saving current Lisp image into %s:\n", filename);
    fflush(stdout);

    putw(CORE_MAGIC, file);

    putw(VERSION_CORE_ENTRY_TYPE_CODE, file);
    putw(3, file);
    putw(SBCL_CORE_VERSION_INTEGER, file);

    putw(BUILD_ID_CORE_ENTRY_TYPE_CODE, file);
    putw(/* (We're writing the word count of the entry here, and the 2
	  * term is one word for the leading BUILD_ID_CORE_ENTRY_TYPE_CODE
	  * word and one word where we store the count itself.) */
	 2 + strlen(build_id),
	 file);
    {
	char *p;
	for (p = build_id; *p; ++p)
	    putw(*p, file);
    }

    putw(NEW_DIRECTORY_CORE_ENTRY_TYPE_CODE, file);
    putw(/* (word count = 3 spaces described by 5 words each, plus the
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
    /* I don't know too much about the circumstances in which we could
     * end up here.  It may be that current_region_free_pointer is
     * guaranteed to be relevant and we could skip these slightly
     * paranoid checks.  TRT would be to rid the code of
     * current_region_foo completely - dan 2002.09.17 */
    if((boxed_region.free_pointer < current_region_free_pointer) &&
       (boxed_region.end_addr == current_region_end_addr))
	boxed_region.free_pointer = current_region_free_pointer;
    /* Flush the current_region, updating the tables. */
    gc_alloc_update_page_tables(0,&boxed_region);
    gc_alloc_update_page_tables(1,&unboxed_region);
    update_x86_dynamic_space_free_pointer();
#endif
    output_space(file,
		 DYNAMIC_CORE_SPACE_ID,
		 (lispobj *)DYNAMIC_SPACE_START,
		 (lispobj *)SymbolValue(ALLOCATION_POINTER,0));
#endif

    putw(INITIAL_FUN_CORE_ENTRY_TYPE_CODE, file);
    putw(3, file);
    putw(init_function, file);

    putw(END_CORE_ENTRY_TYPE_CODE, file);

    fclose(file);
    printf("done]\n");

    exit(0);
}
