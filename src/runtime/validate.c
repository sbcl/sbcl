/*
 * memory validation
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

#include "sbcl.h"
#include "runtime.h"
#include "os.h"
#include "globals.h"
#include "validate.h"

static void
ensure_space(lispobj *start, unsigned long size)
{
    if (os_validate((os_vm_address_t)start,(os_vm_size_t)size)==NULL) {
	fprintf(stderr,
		"ensure_space: failed to validate %ld bytes at 0x%08lx\n",
		size,
		(unsigned long)start);
	fprintf(stderr,
		"(hint: Try \"ulimit -a\"; maybe you should increase memory limits.)\n");
	exit(1);
    }
}

void
validate(void)
{
#ifdef PRINTNOISE
    printf("validating memory ...");
    fflush(stdout);
#endif
    
    ensure_space( (lispobj *)READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE);
    ensure_space( (lispobj *)STATIC_SPACE_START   , STATIC_SPACE_SIZE);
#ifdef LISP_FEATURE_GENCGC
    ensure_space( (lispobj *)DYNAMIC_SPACE_START  , DYNAMIC_SPACE_SIZE);
#else
    ensure_space( (lispobj *)DYNAMIC_0_SPACE_START  , DYNAMIC_SPACE_SIZE);
    ensure_space( (lispobj *)DYNAMIC_1_SPACE_START  , DYNAMIC_SPACE_SIZE);
#endif

#ifdef LISP_FEATURE_LINKAGE_TABLE
    ensure_space( (lispobj *)LINKAGE_TABLE_SPACE_START, LINKAGE_TABLE_SPACE_SIZE);
#endif
 
#ifdef PRINTNOISE
    printf(" done.\n");
#endif
}

void protect_control_stack_guard_page(pid_t t_id, int protect_p) {
    struct thread *th = find_thread_by_pid(t_id);
    os_protect(CONTROL_STACK_GUARD_PAGE(th),
	       os_vm_page_size,protect_p ?
	       (OS_VM_PROT_READ|OS_VM_PROT_EXECUTE) : OS_VM_PROT_ALL);
}

void protect_control_stack_return_guard_page(pid_t t_id, int protect_p) {
    struct thread *th = find_thread_by_pid(t_id);
    os_protect(CONTROL_STACK_RETURN_GUARD_PAGE(th),
	       os_vm_page_size,protect_p ?
	       (OS_VM_PROT_READ|OS_VM_PROT_EXECUTE) : OS_VM_PROT_ALL);
}
