/*
 * variables everybody needs to look at or frob on
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
#include "sbcl.h"
#include "globals.h"

int foreign_function_call_active;

lispobj *current_control_stack_pointer;
lispobj *current_control_frame_pointer;
#ifndef BINDING_STACK_POINTER
lispobj *current_binding_stack_pointer;
#endif

#ifndef ALLOCATION_POINTER
lispobj *dynamic_space_free_pointer;
#endif
#ifndef INTERNAL_GC_TRIGGER
lispobj *current_auto_gc_trigger;
#endif

void globals_init(void)
{
    /* Space, stack, and free pointer vars are initialized by
     * validate() and coreparse(). */

#ifndef INTERNAL_GC_TRIGGER
    /* no GC trigger yet */
    current_auto_gc_trigger = NULL;
#endif

    /* Set foreign function call active. */
    foreign_function_call_active = 1;

    /* Initialize the current Lisp state. */
#ifndef __i386__
    current_control_stack_pointer = (lispobj *)CONTROL_STACK_START;
#else
    current_control_stack_pointer = (lispobj *)CONTROL_STACK_END;
#endif

    current_control_frame_pointer = (lispobj *)0;
#ifndef BINDING_STACK_POINTER
    current_binding_stack_pointer = BINDING_STACK_START;
#endif
}
