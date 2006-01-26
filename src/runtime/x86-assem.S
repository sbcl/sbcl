/*
 * very-low-level utilities for runtime support
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

#define LANGUAGE_ASSEMBLY
#include "sbcl.h"
#include "validate.h"
#include "genesis/closure.h"
#include "genesis/fdefn.h"
#include "genesis/static-symbols.h"
#include "genesis/symbol.h"
#include "genesis/thread.h"
	
/* Minimize conditionalization for different OS naming schemes. 
 *
 * (As of sbcl-0.8.10, this seems no longer to be much of an issue, 
 * since everyone has converged on ELF. If this generality really 
 * turns out not to matter, perhaps it's just clutter we could get
 * rid of? -- WHN 2004-04-18)
 *
 * (Except Win32, which is unlikely ever to be ELF, sorry. -- AB 2005-12-08)
 */
#if defined __linux__  || defined __FreeBSD__ || defined __NetBSD__ || defined __OpenBSD__ || defined __sun
#define GNAME(var) var
#else
#define GNAME(var) _##var
#endif

/* Get the right type of alignment. Linux, FreeBSD and NetBSD (but not OpenBSD)
 * want alignment in bytes. 
 *
 * (As in the GNAME() definitions above, as of sbcl-0.8.10, this seems 
 * no longer to be much of an issue, since everyone has converged on
 * the same value. If this generality really turns out not to 
 * matter any more, perhaps it's just clutter we could get
 * rid of? -- WHN 2004-04-18)
 */
#if defined(__linux__) || defined(__FreeBSD__) || defined(__NetBSD__) || defined(__OpenBSD__) || defined(__sun) || defined(LISP_FEATURE_WIN32)
#define align_4byte	4
#define align_8byte	8
#define align_16byte	16
#else
#define	align_4byte	2
#define	align_8byte	3
#define	align_16byte	4	
#endif			

/*
 * The assembler used for win32 doesn't like .type or .size directives,
 * so we want to conditionally kill them out. So let's wrap them in macros
 * that are defined to be no-ops on win32. Hopefully this still works on
 * other platforms.
 */
#ifndef LISP_FEATURE_WIN32
#define TYPE(name) .type name,@function
#define SIZE(name) .size name,.-name
#else
#define TYPE(name)
#define SIZE(name)
#endif

	.text
	.global	GNAME(foreign_function_call_active)
	.global	GNAME(all_threads)

/*
 * A call to call_into_c preserves esi, edi, and ebp.	
 * (The C function will preserve ebx, esi, edi, and ebp across its
 * function call, but we trash ebx ourselves by using it to save the
 * return Lisp address.)
 *
 * Return values are in eax and maybe edx for quads, or st(0) for
 * floats.
 *
 * This should work for Lisp calls C calls Lisp calls C..
 */
	.text
	.align	align_16byte,0x90
	.global GNAME(call_into_c)
	TYPE(GNAME(call_into_c))
GNAME(call_into_c):
	movl	$1,GNAME(foreign_function_call_active)

/* Save the return Lisp address in ebx. */
	popl	%ebx

/* Setup the NPX for C */
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)
	fstp	%st(0)

#ifdef LISP_FEATURE_WIN32
	cld
#endif

	call	*%eax		# normal callout using Lisp stack

	movl	%eax,%ecx	# remember integer return value

/* Check for a return FP value. */
	fxam
	fnstsw	%eax
	andl	$0x4500,%eax
	cmpl	$0x4100,%eax
	jne	Lfp_rtn_value

/* The return value is in eax, or eax,edx? */
/* Set up the NPX stack for Lisp. */
	fldz			# Ensure no regs are empty.
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz

/* Restore the return value. */
	movl	%ecx,%eax	# maybe return value

	movl	$0,GNAME(foreign_function_call_active)
/* Return. */
	jmp	*%ebx

Lfp_rtn_value:
/* The return result is in st(0). */
/* Set up the NPX stack for Lisp, placing the result in st(0). */
	fldz			# Ensure no regs are empty.
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fxch	%st(7)		# Move the result back to st(0).

/* We don't need to restore eax, because the result is in st(0). */

	movl	$0,GNAME(foreign_function_call_active)
/* Return. */	
	jmp	*%ebx

	SIZE(GNAME(call_into_c))


	.text	
	.global GNAME(call_into_lisp_first_time)
	TYPE(GNAME(call_into_lisp_first_time))
		
/* The *ALIEN-STACK* pointer is set up on the first call_into_lisp when
 * the stack changes.  We don't worry too much about saving registers 
 * here, because we never expect to return from the initial call to lisp 
 * anyway */
	
	.align	align_16byte,0x90
GNAME(call_into_lisp_first_time):
	pushl	%ebp		# Save old frame pointer.
	movl	%esp,%ebp	# Establish new frame.
#ifndef LISP_FEATURE_WIN32
	movl    %esp,ALIEN_STACK + SYMBOL_VALUE_OFFSET
	movl    GNAME(all_threads),%eax
	movl    THREAD_CONTROL_STACK_START_OFFSET(%eax) ,%esp
	/* don't think too hard about what happens if we get interrupted
	* here */
	addl 	$THREAD_CONTROL_STACK_SIZE-4,%esp
#else
/* Win32 -really- doesn't like you switching stacks out from under it. */
	movl	GNAME(all_threads),%eax
#endif
	jmp     Lstack

	.text	
	.global GNAME(call_into_lisp)
	TYPE(GNAME(call_into_lisp))
		
/* The C conventions require that ebx, esi, edi, and ebp be preserved
 * across function calls. */
	
	.align	align_16byte,0x90
GNAME(call_into_lisp):
	pushl	%ebp		# Save old frame pointer.
	movl	%esp,%ebp	# Establish new frame.
Lstack:
/* Save the NPX state */
	fwait			# Catch any pending NPX exceptions.
	subl	$108,%esp	# Make room for the NPX state.
	fnsave	(%esp)		# save and reset NPX

	movl	(%esp),%eax	# Load NPX control word.
	andl	$0xfffff2ff,%eax	# Set rounding mode to nearest.
	orl	$0x00000200,%eax	# Set precision to 64 bits.  (53-bit mantissa)
	pushl	%eax
	fldcw	(%esp)		# Recover modes.
	popl	%eax

	fldz			# Ensure no FP regs are empty.
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	fldz
	
/* Save C regs: ebx esi edi. */
	pushl	%ebx
	pushl	%esi
	pushl	%edi
	
/* Clear descriptor regs. */
	xorl	%eax,%eax	# lexenv
	xorl	%ebx,%ebx	# available
	xorl	%ecx,%ecx	# arg count
	xorl	%edx,%edx	# first arg
	xorl	%edi,%edi	# second arg
	xorl	%esi,%esi	# third arg

/* no longer in function call */
	movl	%eax, GNAME(foreign_function_call_active)

	movl	%esp,%ebx	# remember current stack
	pushl	%ebx		# Save entry stack on (maybe) new stack.

	/* Establish Lisp args. */
	movl	 8(%ebp),%eax	# lexenv?
	movl	12(%ebp),%ebx	# address of arg vec
	movl	16(%ebp),%ecx	# num args
	shll	$2,%ecx		# Make num args into fixnum.
	cmpl	$0,%ecx
	je	Ldone
	movl	(%ebx),%edx	# arg0
	cmpl	$4,%ecx
	je	Ldone
	movl	4(%ebx),%edi	# arg1
	cmpl	$8,%ecx
	je	Ldone
	movl	8(%ebx),%esi	# arg2
Ldone:	
	/* Registers eax, ecx, edx, edi, and esi are now live. */

	/* Alloc new frame. */
	mov	%esp,%ebx	# The current sp marks start of new frame.
	push	%ebp		# fp in save location S0
	sub	$8,%esp		# Ensure 3 slots are allocated, one above.
	mov	%ebx,%ebp	# Switch to new frame.

	call	*CLOSURE_FUN_OFFSET(%eax)
	
	/* If the function returned multiple values, it will return to
	   this point.  Lose them */
	mov	%ebx, %esp
	/* A singled value function returns here */

/* Restore the stack, in case there was a stack change. */
	popl	%esp		# c-sp

/* Restore C regs: ebx esi edi. */
	popl	%edi
	popl	%esi
	popl	%ebx

/* Restore the NPX state. */
	frstor  (%esp)
	addl	$108, %esp
	
	popl	%ebp		# c-sp
	movl	%edx,%eax	# c-val
	ret
	SIZE(GNAME(call_into_lisp))

/* support for saving and restoring the NPX state from C */
	.text
	.global	GNAME(fpu_save)
	TYPE(GNAME(fpu_save))
	.align	2,0x90
GNAME(fpu_save):
	movl	4(%esp),%eax
	fnsave	(%eax)		# Save the NPX state. (resets NPX)
	ret
	SIZE(GNAME(fpu_save))

	.global	GNAME(fpu_restore)
	TYPE(GNAME(fpu_restore))
	.align	2,0x90
GNAME(fpu_restore):
	movl	4(%esp),%eax
	frstor	(%eax)		# Restore the NPX state.
	ret
	SIZE(GNAME(fpu_restore))

/*
 * the undefined-function trampoline
 */
	.text
	.align	align_4byte,0x90
	.global GNAME(undefined_tramp)
	TYPE(GNAME(undefined_tramp))
        .byte   0, 0, 0, SIMPLE_FUN_HEADER_WIDETAG
GNAME(undefined_tramp):
	int3
	.byte	trap_Error
        .byte   2
        .byte   UNDEFINED_FUN_ERROR
        .byte   sc_DescriptorReg # eax in the Descriptor-reg SC
	ret
	SIZE(GNAME(undefined_tramp))

/*
 * the closure trampoline
 */
	.text
	.align	align_4byte,0x90
	.global GNAME(closure_tramp)
	TYPE(GNAME(closure_tramp))
        .byte   0, 0, 0, SIMPLE_FUN_HEADER_WIDETAG
GNAME(closure_tramp):
	movl	FDEFN_FUN_OFFSET(%eax),%eax
	/* FIXME: The '*' after "jmp" in the next line is from PVE's
	 * patch posted to the CMU CL mailing list Oct 6, 1999. It looks
	 * reasonable, and it certainly seems as though if CMU CL needs it,
	 * SBCL needs it too, but I haven't actually verified that it's
	 * right. It would be good to find a way to force the flow of
	 * control through here to test it. */
	jmp	*CLOSURE_FUN_OFFSET(%eax)
	SIZE(GNAME(closure_tramp))

/*
 * fun-end breakpoint magic
 */
	.text
	.global	GNAME(fun_end_breakpoint_guts)
	.align	align_4byte
GNAME(fun_end_breakpoint_guts):
	/* Multiple Value return */
	jmp	multiple_value_return
	/* Single value return: The eventual return will now use the
	   multiple values return convention but with a return values
	   count of one. */
	movl	%esp,%ebx	# Setup ebx - the ofp.
	subl	$4,%esp		# Allocate one stack slot for the return value
	movl	$4,%ecx		# Setup ecx for one return value.
	movl	$NIL,%edi	# default second value
	movl	$NIL,%esi	# default third value
		
multiple_value_return:
	
	.global GNAME(fun_end_breakpoint_trap)
GNAME(fun_end_breakpoint_trap):
	int3
	.byte 	trap_FunEndBreakpoint
	hlt			# We should never return here.

	.global GNAME(fun_end_breakpoint_end)
GNAME(fun_end_breakpoint_end):


	.global	GNAME(do_pending_interrupt)
	TYPE(GNAME(do_pending_interrupt))
	.align	align_4byte,0x90
GNAME(do_pending_interrupt):
	int3
	.byte 	trap_PendingInterrupt
	ret
	SIZE(GNAME(do_pending_interrupt))


/*
 * Allocate bytes and return the start of the allocated space
 * in the specified destination register.
 *
 * In the general case the size will be in the destination register.
 *
 * All registers must be preserved except the destination.
 * The C conventions will preserve ebx, esi, edi, and ebp.
 * So only eax, ecx, and edx need special care here.
 */
	
	.globl	GNAME(alloc_to_eax)
	TYPE(GNAME(alloc_to_eax))
	.align	align_4byte,0x90
GNAME(alloc_to_eax):
	pushl	%ecx	# Save ecx and edx as C could destroy them.
	pushl	%edx
	pushl	%eax	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	popl	%edx	# Restore ecx and edx.
	popl	%ecx
	ret
	SIZE(GNAME(alloc_to_eax))

	.globl	GNAME(alloc_8_to_eax)
	TYPE(GNAME(alloc_8_to_eax))
	.align	align_4byte,0x90
GNAME(alloc_8_to_eax):
	pushl	%ecx	# Save ecx and edx as C could destroy them.
	pushl	%edx
	pushl	$8	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	popl	%edx	# Restore ecx and edx.
	popl	%ecx
	ret
	SIZE(GNAME(alloc_8_to_eax))

	.globl	GNAME(alloc_8_to_eax)
	TYPE(GNAME(alloc_8_to_eax))
	.align	align_4byte,0x90

	.globl	GNAME(alloc_16_to_eax)
	TYPE(GNAME(alloc_16_to_eax))
	.align	align_4byte,0x90
GNAME(alloc_16_to_eax):
	pushl	%ecx	# Save ecx and edx as C could destroy them.
	pushl	%edx
	pushl	$16	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	popl	%edx	# Restore ecx and edx.
	popl	%ecx
	ret
	SIZE(GNAME(alloc_16_to_eax))

	.globl	GNAME(alloc_to_ecx)
	TYPE(GNAME(alloc_to_ecx))
	.align	align_4byte,0x90
GNAME(alloc_to_ecx):
	pushl	%eax	# Save eax and edx as C could destroy them.
	pushl	%edx
	pushl	%ecx	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	movl	%eax,%ecx	# Set up the destination.
	popl	%edx	# Restore eax and edx.
	popl	%eax
	ret
	SIZE(GNAME(alloc_to_ecx))

	.globl	GNAME(alloc_8_to_ecx)
	TYPE(GNAME(alloc_8_to_ecx))
	.align	align_4byte,0x90
GNAME(alloc_8_to_ecx):
	pushl	%eax	# Save eax and edx as C could destroy them.
	pushl	%edx
	pushl	$8	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	movl	%eax,%ecx	# Set up the destination.
	popl	%edx	# Restore eax and edx.
	popl	%eax
	ret
	SIZE(GNAME(alloc_8_to_ecx))

	.globl	GNAME(alloc_16_to_ecx)
	TYPE(GNAME(alloc_16_to_ecx))
	.align	align_4byte,0x90
GNAME(alloc_16_to_ecx):
	pushl	%eax	# Save eax and edx as C could destroy them.
	pushl	%edx
	pushl	$16	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	movl	%eax,%ecx	# Set up the destination.
	popl	%edx	# Restore eax and edx.
	popl	%eax
	ret
	SIZE(GNAME(alloc_16_to_ecx))


	.globl	GNAME(alloc_to_edx)
	TYPE(GNAME(alloc_to_edx))
	.align	align_4byte,0x90
GNAME(alloc_to_edx):
	pushl	%eax	# Save eax and ecx as C could destroy them.
	pushl	%ecx
	pushl	%edx	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	movl	%eax,%edx	# Set up the destination.
	popl	%ecx	# Restore eax and ecx.
	popl	%eax
	ret
	SIZE(GNAME(alloc_to_edx))

	.globl	GNAME(alloc_8_to_edx)
	TYPE(GNAME(alloc_8_to_edx))
	.align	align_4byte,0x90
GNAME(alloc_8_to_edx):
	pushl	%eax	# Save eax and ecx as C could destroy them.
	pushl	%ecx
	pushl	$8	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	movl	%eax,%edx	# Set up the destination.
	popl	%ecx	# Restore eax and ecx.
	popl	%eax
	ret
	SIZE(GNAME(alloc_8_to_edx))

	.globl	GNAME(alloc_16_to_edx)
	TYPE(GNAME(alloc_16_to_edx))
	.align	align_4byte,0x90
GNAME(alloc_16_to_edx):
	pushl	%eax	# Save eax and ecx as C could destroy them.
	pushl	%ecx
	pushl	$16	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	movl	%eax,%edx	# Set up the destination.
	popl	%ecx	# Restore eax and ecx.
	popl	%eax
	ret
	SIZE(GNAME(alloc_16_to_edx))



	.globl	GNAME(alloc_to_ebx)
	TYPE(GNAME(alloc_to_ebx))
	.align	align_4byte,0x90
GNAME(alloc_to_ebx):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	%ebx	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	movl	%eax,%ebx	# Set up the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	SIZE(GNAME(alloc_to_ebx))

	.globl	GNAME(alloc_8_to_ebx)
	TYPE(GNAME(alloc_8_to_ebx))
	.align	align_4byte,0x90
GNAME(alloc_8_to_ebx):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$8	# Push the size.
	call	GNAME(alloc)
	addl	$4,%esp	# Pop the size arg.
	movl	%eax,%ebx	# Set up the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	SIZE(GNAME(alloc_8_to_ebx))

	.globl	GNAME(alloc_16_to_ebx)
	TYPE(GNAME(alloc_16_to_ebx))
	.align	align_4byte,0x90
GNAME(alloc_16_to_ebx):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ebx	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	SIZE(GNAME(alloc_16_to_ebx))



	.globl	GNAME(alloc_to_esi)
	TYPE(GNAME(alloc_to_esi))
	.align	align_4byte,0x90
GNAME(alloc_to_esi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	%esi	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%esi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	SIZE(GNAME(alloc_to_esi))

	.globl	GNAME(alloc_8_to_esi)
	TYPE(GNAME(alloc_8_to_esi))
	.align	align_4byte,0x90
GNAME(alloc_8_to_esi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%esi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	SIZE(GNAME(alloc_8_to_esi))

	.globl	GNAME(alloc_16_to_esi)
	TYPE(GNAME(alloc_16_to_esi))
	.align	align_4byte,0x90
GNAME(alloc_16_to_esi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%esi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	SIZE(GNAME(alloc_16_to_esi))


	.globl	GNAME(alloc_to_edi)
	TYPE(GNAME(alloc_to_edi))
	.align	align_4byte,0x90
GNAME(alloc_to_edi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	%edi	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	SIZE(GNAME(alloc_to_edi))

	.globl	GNAME(alloc_8_to_edi)
	TYPE(GNAME(alloc_8_to_edi))
	.align	align_4byte,0x90
GNAME(alloc_8_to_edi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$8	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	SIZE(GNAME(alloc_8_to_edi))

	.globl	GNAME(alloc_16_to_edi)
	TYPE(GNAME(alloc_16_to_edi))
	.align	align_4byte,0x90
GNAME(alloc_16_to_edi):
	pushl	%eax	# Save eax, ecx, and edx as C could destroy them.
	pushl	%ecx
	pushl	%edx
	pushl	$16	# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edi	# setup the destination.
	popl	%edx	# Restore eax, ecx and edx.
	popl	%ecx
	popl	%eax
	ret
	SIZE(GNAME(alloc_16_to_edi))

	
/* Called from lisp when an inline allocation overflows.
   Every register except the result needs to be preserved.
   We depend on C to preserve ebx, esi, edi, and ebp.
   But where necessary must save eax, ecx, edx. */

#ifdef LISP_FEATURE_SB_THREAD
#define START_REGION %fs:THREAD_ALLOC_REGION_OFFSET
#else
#define START_REGION GNAME(boxed_region)
#endif
		
/* This routine handles an overflow with eax=crfp+size. So the
   size=eax-crfp. */
        .align  align_4byte
        .globl  GNAME(alloc_overflow_eax)
	TYPE(GNAME(alloc_overflow_eax))
GNAME(alloc_overflow_eax):
        pushl   %ecx            # Save ecx
        pushl   %edx            # Save edx
        /* Calculate the size for the allocation. */
        subl    START_REGION,%eax
        pushl   %eax            # Push the size
        call    GNAME(alloc)
        addl    $4,%esp # pop the size arg.
        popl    %edx    # Restore edx.
        popl    %ecx    # Restore ecx.
        ret
        SIZE(GNAME(alloc_overflow_eax))

        .align  align_4byte
        .globl  GNAME(alloc_overflow_ecx)
	TYPE(GNAME(alloc_overflow_ecx))
GNAME(alloc_overflow_ecx):
        pushl   %eax            # Save eax
        pushl   %edx            # Save edx
        /* Calculate the size for the allocation. */
        subl    START_REGION,%ecx
        pushl   %ecx            # Push the size
        call    GNAME(alloc)
        addl    $4,%esp # pop the size arg.
        movl    %eax,%ecx       # setup the destination.
        popl    %edx    # Restore edx.
        popl    %eax    # Restore eax.
        ret
        SIZE(GNAME(alloc_overflow_ecx))

        .align  align_4byte
        .globl  GNAME(alloc_overflow_edx)
        TYPE(GNAME(alloc_overflow_edx))
GNAME(alloc_overflow_edx):
        pushl   %eax            # Save eax
        pushl   %ecx            # Save ecx
        /* Calculate the size for the allocation. */
        subl    START_REGION,%edx
        pushl   %edx            # Push the size
        call    GNAME(alloc)
        addl    $4,%esp # pop the size arg.
        movl    %eax,%edx       # setup the destination.
        popl    %ecx    # Restore ecx.
        popl    %eax    # Restore eax.
        ret
        SIZE(GNAME(alloc_overflow_edx))

/* This routine handles an overflow with ebx=crfp+size. So the
   size=ebx-crfp. */
        .align  align_4byte
        .globl  GNAME(alloc_overflow_ebx)
        TYPE(GNAME(alloc_overflow_ebx))
GNAME(alloc_overflow_ebx):
        pushl   %eax            # Save eax
        pushl   %ecx            # Save ecx
        pushl   %edx            # Save edx
        /* Calculate the size for the allocation. */
        subl    START_REGION,%ebx
        pushl   %ebx            # Push the size
        call    GNAME(alloc)
        addl    $4,%esp # pop the size arg.
        movl    %eax,%ebx       # setup the destination.
        popl    %edx    # Restore edx.
        popl    %ecx    # Restore ecx.
        popl    %eax    # Restore eax.
        ret
        SIZE(GNAME(alloc_overflow_ebx))

/* This routine handles an overflow with esi=crfp+size. So the
   size=esi-crfp. */
        .align  align_4byte
        .globl  GNAME(alloc_overflow_esi)
        TYPE(GNAME(alloc_overflow_esi))
GNAME(alloc_overflow_esi):
        pushl   %eax            # Save eax
        pushl   %ecx            # Save ecx
        pushl   %edx            # Save edx
        /* Calculate the size for the allocation. */
        subl    START_REGION,%esi
        pushl   %esi            # Push the size
        call    GNAME(alloc)
        addl    $4,%esp # pop the size arg.
        movl    %eax,%esi       # setup the destination.
        popl    %edx    # Restore edx.
        popl    %ecx    # Restore ecx.
        popl    %eax    # Restore eax.
        ret
        SIZE(GNAME(alloc_overflow_esi))

        .align  align_4byte
        .globl  GNAME(alloc_overflow_edi)
        TYPE(GNAME(alloc_overflow_edi))
GNAME(alloc_overflow_edi):
        pushl   %eax            # Save eax
        pushl   %ecx            # Save ecx
        pushl   %edx            # Save edx
        /* Calculate the size for the allocation. */
        subl    START_REGION,%edi
        pushl   %edi            # Push the size
        call    GNAME(alloc)
        addl    $4,%esp # pop the size arg.
        movl    %eax,%edi       # setup the destination.
        popl    %edx    # Restore edx.
        popl    %ecx    # Restore ecx.
        popl    %eax    # Restore eax.
        ret
        SIZE(GNAME(alloc_overflow_edi))

	.align	align_4byte,0x90
	.globl	GNAME(post_signal_tramp)
	TYPE(GNAME(post_signal_tramp))
GNAME(post_signal_tramp):
	/* this is notionally the second half of a function whose first half
 	 * doesn't exist.  This is where call_into_lisp returns when called 
	 * using return_to_lisp_function */
	addl $12,%esp  	/* clear call_into_lisp args from stack */
	popal           /* restore registers */
        popfl
	leave
	ret
	SIZE(GNAME(post_signal_tramp))

#ifdef LISP_FEATURE_WIN32
	/*
	 * This is part of the funky magic for exception handling on win32.
	 * see sigtrap_emulator() in win32-os.c for details.
	 */
	.global GNAME(sigtrap_trampoline)
GNAME(sigtrap_trampoline):
	pushl	%eax
	pushl	%ebp
	movl	%esp, %ebp
	call	GNAME(sigtrap_wrapper)
	pop	%eax
	pop	%eax
	int3
	.byte 	trap_ContextRestore
	hlt			# We should never return here.
        
	/*
	 * This is part of the funky magic for exception handling on win32.
	 * see handle_exception() in win32-os.c for details.
	 */
	.global GNAME(exception_trampoline)
GNAME(exception_trampoline):
	pushl	%eax
	pushl	%ebp
	movl	%esp, %ebp
	call	GNAME(handle_win32_exception_wrapper)
	pop	%eax
	pop	%eax
	int3
	.byte 	trap_ContextRestore
	hlt			# We should never return here.
#endif

        /* fast_bzero implementations and code to detect which implementation
         * to use.
         */

	.global GNAME(fast_bzero_pointer)
	.data
        .align 4
GNAME(fast_bzero_pointer):
        /* Variable containing a pointer to the bzero function to use.
         * Initially points to a basic function.  Change this variable
         * to fast_bzero_detect if OS supports SSE.  */
        .long GNAME(fast_bzero_base)

	.text
	.align	align_8byte,0x90
	.global GNAME(fast_bzero)
	TYPE(GNAME(fast_bzero))
GNAME(fast_bzero):        
        /* Indirect function call */
        jmp *GNAME(fast_bzero_pointer)
	SIZE(GNAME(fast_bzero))
        
      
	.text
	.align	align_8byte,0x90
	.global GNAME(fast_bzero_detect)
	TYPE(GNAME(fast_bzero_detect))
GNAME(fast_bzero_detect):
        /* Decide whether to use SSE, MMX or REP version */
        push %eax /* CPUID uses EAX-EDX */
        push %ebx
        push %ecx
        push %edx
        mov $1, %eax
        cpuid
        test $0x04000000, %edx    /* SSE2 needed for MOVNTDQ */
        jnz Lsse2
        /* Originally there was another case here for using the
         * MOVNTQ instruction for processors that supported MMX but
         * not SSE2. This turned out to be a loss especially on
         * Athlons (where this instruction is apparently microcoded
         * somewhat slowly). So for simplicity revert to REP STOSL
         * for all non-SSE2 processors.
         */
Lbase:
        movl $GNAME(fast_bzero_base), GNAME(fast_bzero_pointer)
        jmp Lrestore
Lsse2:
        movl $GNAME(fast_bzero_sse), GNAME(fast_bzero_pointer)
        jmp Lrestore
        
Lrestore:
        pop %edx
        pop %ecx
        pop %ebx
        pop %eax
        jmp *GNAME(fast_bzero_pointer)
        
	SIZE(GNAME(fast_bzero_detect))
        

	.text
	.align	align_8byte,0x90
	.global GNAME(fast_bzero_sse)
	TYPE(GNAME(fast_bzero_sse))
        
GNAME(fast_bzero_sse):
        /* A fast routine for zero-filling blocks of memory that are
         * guaranteed to start and end at a 4096-byte aligned address.
         */        
        push %esi                 /* Save temporary registers */
        push %edi
        mov 16(%esp), %esi        /* Parameter: amount of bytes to fill */
        mov 12(%esp), %edi        /* Parameter: start address */
        shr $6, %esi              /* Amount of 64-byte blocks to copy */
        jz Lend_sse               /* If none, stop */
        movups %xmm7, -16(%esp)   /* Save XMM register */
        xorps  %xmm7, %xmm7       /* Zero the XMM register */
        jmp Lloop_sse
        .align 16
Lloop_sse:

        /* Copy the 16 zeroes from xmm7 to memory, 4 times. MOVNTDQ is the
         * non-caching double-quadword moving variant, i.e. the memory areas
         * we're touching are not fetched into the L1 cache, since we're just
         * going to overwrite the memory soon anyway.
         */
        movntdq %xmm7, 0(%edi)
        movntdq %xmm7, 16(%edi)
        movntdq %xmm7, 32(%edi)
        movntdq %xmm7, 48(%edi)
 
        add $64, %edi /* Advance pointer */
        dec %esi      /* Decrement 64-byte block count */
        jnz Lloop_sse
        movups -16(%esp), %xmm7 /* Restore the XMM register */
        sfence        /* Ensure that weakly ordered writes are flushed. */
Lend_sse:
        mov 12(%esp), %esi      /* Parameter: start address */
        prefetcht0 0(%esi)      /* Prefetch the start of the block into cache,
                                 * since it's likely to be used immediately. */
        pop %edi      /* Restore temp registers */
        pop %esi
	ret
	SIZE(GNAME(fast_bzero_sse))
                

	.text
	.align	align_8byte,0x90
	.global GNAME(fast_bzero_base)
	TYPE(GNAME(fast_bzero_base))
        
GNAME(fast_bzero_base):
        /* A fast routine for zero-filling blocks of memory that are
         * guaranteed to start and end at a 4096-byte aligned address.
         */        
        push %eax                 /* Save temporary registers */
        push %ecx
        push %edi
        mov 20(%esp), %ecx        /* Parameter: amount of bytes to fill */
        mov 16(%esp), %edi        /* Parameter: start address */
        xor %eax, %eax            /* Zero EAX */
        shr $2, %ecx              /* Amount of 4-byte blocks to copy */
        jz  Lend_base
        cld                       /* Set direction of STOSL to increment */
        rep stosl                 /* Store EAX to *EDI, ECX times, incrementing
                                   * EDI by 4 after each store */
Lend_base:        
        pop %edi                  /* Restore temp registers */
        pop %ecx
        pop %eax
	ret
	SIZE(GNAME(fast_bzero_base))
        
	
	.end
