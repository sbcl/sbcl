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

/*
 * $Header$
 */

#include <stdio.h>
#include "runtime.h"
#include "os.h"
#include "globals.h"
#include "validate.h"

static void ensure_space(lispobj *start, unsigned long size)
{
    if (os_validate((os_vm_address_t)start,(os_vm_size_t)size)==NULL) {
	fprintf(stderr,
		"ensure_space: failed to validate %ld bytes at 0x%08X\n",
		size,
		(unsigned long)start);
	exit(1);
    }
}

#ifdef HOLES

static os_vm_address_t holes[] = HOLES;

static void make_holes(void)
{
    int i;

    for (i = 0; i < sizeof(holes)/sizeof(holes[0]); i++) {
	if (os_validate(holes[i], HOLE_SIZE) == NULL) {
	    fprintf(stderr,
		    "make_holes: failed to validate %ld bytes at 0x%08X\n",
		    HOLE_SIZE,
		    (unsigned long)holes[i]);
	    exit(1);
	}
	os_protect(holes[i], HOLE_SIZE, 0);
    }
}
#endif

void validate(void)
{
#ifdef PRINTNOISE
	printf("validating memory ...");
	fflush(stdout);
#endif

	/* Read-Only Space */
	read_only_space = (lispobj *) READ_ONLY_SPACE_START;
	ensure_space(read_only_space, READ_ONLY_SPACE_SIZE);

	/* Static Space */
	static_space = (lispobj *) STATIC_SPACE_START;
	ensure_space(static_space, STATIC_SPACE_SIZE);

	/* Dynamic-0 Space */
	dynamic_0_space = (lispobj *) DYNAMIC_0_SPACE_START;
	ensure_space(dynamic_0_space, DYNAMIC_SPACE_SIZE);

	current_dynamic_space = dynamic_0_space;

	/* Dynamic-1 Space */
	dynamic_1_space = (lispobj *) DYNAMIC_1_SPACE_START;
#ifndef GENCGC
 	ensure_space(dynamic_1_space, DYNAMIC_SPACE_SIZE);
#endif

	/* Control Stack */
	control_stack = (lispobj *) CONTROL_STACK_START;
#ifdef __i386__
	control_stack_end = (lispobj *) (CONTROL_STACK_START
					 + CONTROL_STACK_SIZE);
#endif
	ensure_space(control_stack, CONTROL_STACK_SIZE);

	/* Binding Stack */
	binding_stack = (lispobj *) BINDING_STACK_START;
	ensure_space(binding_stack, BINDING_STACK_SIZE);

#ifdef HOLES
	make_holes();
#endif

#ifdef PRINTNOISE
	printf(" done.\n");
#endif
}
