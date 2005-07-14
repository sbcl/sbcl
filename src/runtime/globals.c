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

#include <stdio.h>
#include <sys/types.h>
#include <unistd.h>

#include "sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "validate.h"

int foreign_function_call_active;

lispobj *current_control_stack_pointer;
lispobj *current_control_frame_pointer;
#ifndef BINDING_STACK_POINTER
lispobj *current_binding_stack_pointer;
#endif

/* ALLOCATION_POINTER is x86 or RT.  Anyone want to do an RT port?   */

#ifndef ALLOCATION_POINTER
/* The Object Formerly Known As current_dynamic_space_free_pointer */
lispobj *dynamic_space_free_pointer;
#endif

#ifndef LISP_FEATURE_GENCGC /* GENCGC has its own way to record trigger */
lispobj *current_auto_gc_trigger;
#endif

#ifdef LISP_FEATURE_SB_THREAD
boolean stop_the_world=0;
#endif

/* For copying GCs, this points to the start of the dynamic space
 * currently in use (that will become the from_space when the next GC
 * is done).  For the GENCGC, it always points to DYNAMIC_SPACE_START. */
lispobj *current_dynamic_space;

#if defined(LISP_FEATURE_SB_THREAD)
pthread_key_t specials=0;
#endif

void globals_init(void)
{
    /* Space, stack, and free pointer vars are initialized by
     * validate() and coreparse(). */
    current_control_frame_pointer = (lispobj *)0;

#ifndef LISP_FEATURE_GENCGC
    /* no GC trigger yet */
    current_auto_gc_trigger = NULL;
#endif

    /* Set foreign function call active. */
    foreign_function_call_active = 1;
#if defined(LISP_FEATURE_SB_THREAD)
    pthread_key_create(&specials,0);
#endif
}
