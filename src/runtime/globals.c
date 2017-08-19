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

#ifndef LISP_FEATURE_SB_THREAD
int foreign_function_call_active;
#endif

#if !defined(LISP_FEATURE_SB_THREAD)
lispobj *current_control_stack_pointer;
#endif
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || !defined(LISP_FEATURE_SB_THREAD)
lispobj *current_control_frame_pointer;
#endif
#if !defined(BINDING_STACK_POINTER) && !defined(LISP_FEATURE_SB_THREAD)
lispobj *current_binding_stack_pointer;
#endif

/* ARM backends use ALLOCATION-POINTER, not dynamic_space_free_pointer */

# if !defined(LISP_FEATURE_ARM) && !defined(LISP_FEATURE_ARM64)
/* The Object Formerly Known As current_dynamic_space_free_pointer */
lispobj *dynamic_space_free_pointer;
#endif
lispobj *read_only_space_free_pointer;
lispobj *static_space_free_pointer;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
lispobj *immobile_space_free_pointer;
lispobj *immobile_fixedobj_free_pointer;
#endif
os_vm_address_t anon_dynamic_space_start;

#ifndef LISP_FEATURE_GENCGC /* GENCGC has its own way to record trigger */
lispobj *current_auto_gc_trigger;
#endif

/* For cheneygc, this points to the start of the semi-space currently in use
 * (that will become the from_space when the next GC is done).
 * Gencgc defines it as DYNAMIC_SPACE_START via a C preprocessor macro. */
#ifdef LISP_FEATURE_CHENEYGC
lispobj *current_dynamic_space;
#endif

#if defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_GCC_TLS)
pthread_key_t specials=0;
#endif

void globals_init(void)
{
    /* Space, stack, and free pointer vars are initialized by
     * allocate_spaces() and coreparse(). */
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || !defined(LISP_FEATURE_SB_THREAD)
    current_control_frame_pointer = (lispobj *)0;
#endif

#ifndef LISP_FEATURE_GENCGC
    /* no GC trigger yet */
    current_auto_gc_trigger = NULL;
#endif

#ifndef LISP_FEATURE_SB_THREAD
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    /* KLUDGE: x86oids always think they're in lisp code.  See the
     * comment at the bottom of
     * interrupt.c/fake_foreign_function_call() and the lack of any
     * access to foreign_function_call_active or the corresponding
     * thread slot in x86{,-64}-assem.S. */
    foreign_function_call_active = 0;
#else
    foreign_function_call_active = 1;
#endif
#endif

#if defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_GCC_TLS)
    pthread_key_create(&specials,0);
#endif
}
