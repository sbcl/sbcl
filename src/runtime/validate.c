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

#include "runtime.h"
#include "os.h"
#include "globals.h"
#include "sbcl.h"
#include "validate.h"

static void
ensure_space(lispobj *start, unsigned long size)
{
    if (os_validate((os_vm_address_t)start,(os_vm_size_t)size)==NULL) {
	fprintf(stderr,
		"ensure_space: failed to validate %ld bytes at 0x%08lx\n",
		size,
		(unsigned long)start);
	exit(1);
    }
}

#ifdef HOLES

static os_vm_address_t holes[] = HOLES;

static void
make_holes(void)
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

void
validate(void)
{
#ifdef PRINTNOISE
	printf("validating memory ...");
	fflush(stdout);
#endif

	ensure_space( (lispobj *)READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE);
	ensure_space( (lispobj *)STATIC_SPACE_START   , STATIC_SPACE_SIZE);
#ifdef GENCGC
	ensure_space( (lispobj *)DYNAMIC_SPACE_START  , DYNAMIC_SPACE_SIZE);
#else
	ensure_space( (lispobj *)DYNAMIC_0_SPACE_START  , DYNAMIC_SPACE_SIZE);
	ensure_space( (lispobj *)DYNAMIC_1_SPACE_START  , DYNAMIC_SPACE_SIZE);
#endif
	ensure_space( (lispobj *)CONTROL_STACK_START  , CONTROL_STACK_SIZE);
	ensure_space( (lispobj *)BINDING_STACK_START  , BINDING_STACK_SIZE);

#ifdef HOLES
	make_holes();
#endif
#ifndef GENCGC
        current_dynamic_space = DYNAMIC_0_SPACE_START;
#endif

#ifdef PRINTNOISE
	printf(" done.\n");
#endif
}
