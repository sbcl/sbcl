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
// Access to this variable from Lisp is more confusing than it should be for the 64-bit
// machines, because they have to use a half-sized store, which if nothing else forces
// the programmer to have to remember that fact. More critically, some of the uses try
// to "cheat" a write of 1 by storing from any known nonzero register into this variable
// under the assumption that any nonzero value is as good as a 1.  That's fine if the low
// half of the source register is in fact nonzero. But what if the register holds
// #x8ff00000000 ? Then the clever trick doesn't work. This is not merely a theoretical
// problem - it happened on x86-64 where thread->pa was set to true by storing the
// frame pointer ($RBP), but someone optimized the store to a 32-bit store (from $EBP)
// when the frame pointer could have held a value exactly as illustrated above.
// Ironically the CPU can do an immediate-to-mememory move, making that perhaps the most
// egregious example of premature optimization that I had ever witnessed.
// (That bug was fixed immediately upon discovery)
// Perhaps this should be turned into a 'uword_t'?
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

/* ARM + RISCV use ALLOCATION-POINTER, not dynamic_space_free_pointer */

# if !(defined LISP_FEATURE_ARM || defined LISP_FEATURE_ARM64 || defined LISP_FEATURE_RISCV)
/* The Object Formerly Known As current_dynamic_space_free_pointer */
/* The ARM ports (32- and 64-bit) use a static-space lisp symbol for this pointer.
 * They also do not have a reg_ALLOC, so there is no ambiguity as to where the
 * pointer is obtained from: it is always in the static-space lisp symbol.
 * We do not copy that value back and forth to the C variable.
 * Moreover, because they do not have a reg_ALLOC, the pseudo-atomic flag
 * is not overlayed on the reg_ALLOC but is instead in another static-space
 * lisp symbol */
lispobj *dynamic_space_free_pointer;
#endif
lispobj *read_only_space_free_pointer;
lispobj *static_space_free_pointer;

#ifdef LISP_FEATURE_DARWIN_JIT
lispobj *static_code_space_free_pointer;
#endif

#ifdef LISP_FEATURE_IMMOBILE_SPACE
lispobj *varyobj_free_pointer;
lispobj *fixedobj_free_pointer;
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

lispobj lisp_package_vector;

void globals_init(void)
{
    /* Space, stack, and free pointer vars are initialized by
     * validate() and coreparse(). */
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
}
