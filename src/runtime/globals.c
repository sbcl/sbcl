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

#include "genesis/sbcl.h"
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

lispobj *read_only_space_free_pointer;
lispobj *static_space_free_pointer;

#ifdef LISP_FEATURE_DARWIN_JIT
lispobj *static_code_space_free_pointer;
#endif

#ifdef LISP_FEATURE_IMMOBILE_SPACE
lispobj *fixedobj_free_pointer;
lispobj ALIEN_LINKAGE_SPACE_START;
#endif
os_vm_address_t anon_dynamic_space_start;
// The end of immobile text mapped from disk, equivalently the starting address
// of new objects handed out by the code allocator.
lispobj* tlsf_mem_start; // meaningful only if immobile space

lispobj lisp_package_vector;
// Tagged lisp pointer to a 'struct arena' (which is also a lisp DEFSTRUCT)
// The chain terminates with NIL.
lispobj arena_chain;

void globals_init(void)
{
    /* Space, stack, and free pointer vars are initialized by
     * validate() and coreparse(). */
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || !defined(LISP_FEATURE_SB_THREAD)
    current_control_frame_pointer = (lispobj *)0;
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

uword_t permgen_bounds[2];
lispobj *permgen_space_free_pointer;

uword_t FIXEDOBJ_SPACE_START, TEXT_SPACE_START;
lispobj *text_space_highwatermark;
#ifndef LISP_FEATURE_IMMOBILE_SPACE
/* this is a KLUDGE. If #+immobile-space then text_space_size gets statically
 * initialized from the genesis constant TEXT_SPACE_SIZE, as it requires because
 * the text space card table is allocated before parsing the core.
 * But if #-immobile-space then genesis does not emit that constant.
 * The right thing might be to read the space size from the core before making
 * the card table. But I don't want to, since I have WIP to remove the card table
 * for code and instead track written objects in a linked list. (Card table scan
 * time is proportional to space size, but remembered set scan time is proportional
 * to old object mutation rate which is near zero for pages of code)
 */
unsigned int text_space_size;
#endif
