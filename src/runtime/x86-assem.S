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
#include "validate.h"
#include "sbcl.h"
#include "genesis/closure.h"
#include "genesis/fdefn.h"
#include "genesis/static-symbols.h"
#include "genesis/symbol.h"
#include "genesis/thread.h"
	
/* Minimize conditionalization for different OS naming schemes. */
#if defined __linux__  || defined __FreeBSD__ /* (but *not* OpenBSD) */
#define GNAME(var) var
#else
#define GNAME(var) _##var
#endif

/* Get the right type of alignment. Linux and FreeBSD (but not OpenBSD)
 * want alignment in bytes. */
#if defined(__linux__) || defined(__FreeBSD__)
#define align_4byte	4
#define align_8byte	8
#define align_16byte	16
#else
#define	align_4byte	2
#define	align_8byte	3
#define	align_16byte	4	
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
	.type	GNAME(call_into_c),@function
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

	.size	GNAME(call_into_c), . - GNAME(call_into_c)


	.text	
	.global GNAME(call_into_lisp_first_time)
	.type  GNAME(call_into_lisp_first_time),@function
		
/* The *ALIEN-STACK* pointer is set up on the first call_into_lisp when
 * the stack changes.  We don't worry too much about saving registers 
 * here, because we never expect to return from the initial call to lisp 
 * anyway */
	
	.align	align_16byte,0x90
GNAME(call_into_lisp_first_time):
	pushl	%ebp		# Save old frame pointer.
	movl	%esp,%ebp	# Establish new frame.
	movl    %esp,ALIEN_STACK + SYMBOL_VALUE_OFFSET
	movl    GNAME(all_threads),%eax
	movl    THREAD_CONTROL_STACK_START_OFFSET(%eax) ,%esp
	/* don't think too hard about what happens if we get interrupted
	* here */
	addl 	$THREAD_CONTROL_STACK_SIZE-4,%esp
	jmp     Lstack

	.text	
	.global GNAME(call_into_lisp)
	.type  GNAME(call_into_lisp),@function
		
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
	fnsave	(%esp)		# resets NPX

	movl	(%esp),%eax	# Load NPX control word.
	andl	$0xfffff3ff,%eax	# Set rounding mode to nearest.
	orl	$0x00000300,%eax	# Set precision to 64 bits.
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

	/* Indirect the closure. */
	call	*CLOSURE_FUN_OFFSET(%eax)
	
	/* Multi-value return; blow off any extra values. */
	mov	%ebx, %esp
	/* single value return */

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
	.size	GNAME(call_into_lisp), . - GNAME(call_into_lisp)

/* support for saving and restoring the NPX state from C */
	.text
	.global	GNAME(fpu_save)
	.type	GNAME(fpu_save),@function
	.align	2,0x90
GNAME(fpu_save):
	movl	4(%esp),%eax
	fnsave	(%eax)		# Save the NPX state. (resets NPX)
	ret
	.size	GNAME(fpu_save),.-GNAME(fpu_save)

	.global	GNAME(fpu_restore)
	.type	GNAME(fpu_restore),@function
	.align	2,0x90
GNAME(fpu_restore):
	movl	4(%esp),%eax
	frstor	(%eax)		# Restore the NPX state.
	ret
	.size	GNAME(fpu_restore),.-GNAME(fpu_restore)

/*
 * the undefined-function trampoline
 */
	.text
	.align	align_4byte,0x90
	.global GNAME(undefined_tramp)
	.type	GNAME(undefined_tramp),@function
GNAME(undefined_tramp):
	int3
	.byte	trap_Error
        .byte   2
        .byte   UNDEFINED_FUN_ERROR
        .byte   sc_DescriptorReg # eax in the Descriptor-reg SC
	ret
	.size	GNAME(undefined_tramp), .-GNAME(undefined_tramp)

/*
 * the closure trampoline
 */
	.text
	.align	align_4byte,0x90
	.global GNAME(closure_tramp)
	.type	GNAME(closure_tramp),@function
GNAME(closure_tramp):
	movl	FDEFN_FUN_OFFSET(%eax),%eax
	/* FIXME: The '*' after "jmp" in the next line is from PVE's
	 * patch posted to the CMU CL mailing list Oct 6, 1999. It looks
	 * reasonable, and it certainly seems as though if CMU CL needs it,
	 * SBCL needs it too, but I haven't actually verified that it's
	 * right. It would be good to find a way to force the flow of
	 * control through here to test it. */
	jmp	*CLOSURE_FUN_OFFSET(%eax)
	.size	GNAME(closure_tramp), .-GNAME(closure_tramp)

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
	.type	GNAME(do_pending_interrupt),@function
	.align	align_4byte,0x90
GNAME(do_pending_interrupt):
	int3
	.byte 	trap_PendingInterrupt
	ret
	.size	GNAME(do_pending_interrupt),.-GNAME(do_pending_interrupt)

#ifdef LISP_FEATURE_GENCGC
/* This is a fast bzero using the FPU. The first argument is the start
 * address which needs to be aligned on an 8 byte boundary, the second
 * argument is the number of bytes, which must be a nonzero multiple
 * of 8 bytes. */
/* FIXME whether this is still faster than using the OS's bzero or
 * equivalent, we don't know */
	.text
	.globl	GNAME(i586_bzero)
	.type	GNAME(i586_bzero),@function
	.align	align_4byte,0x90
GNAME(i586_bzero):
	movl	4(%esp),%edx	# Load the start address.
	movl	8(%esp),%eax	# Load the number of bytes.
	fldz
l1:	fstl	0(%edx)
	addl	$8,%edx
	subl	$8,%eax
	jnz	l1
	fstp	%st(0)
	ret
	.size	GNAME(i586_bzero),.-GNAME(i586_bzero)
#endif	


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
	.type	GNAME(alloc_to_eax),@function
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
	.size	GNAME(alloc_to_eax),.-GNAME(alloc_to_eax)

	.globl	GNAME(alloc_8_to_eax)
	.type	GNAME(alloc_8_to_eax),@function
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
	.size	GNAME(alloc_8_to_eax),.-GNAME(alloc_8_to_eax)

	.globl	GNAME(alloc_8_to_eax)
	.type	GNAME(alloc_8_to_eax),@function
	.align	align_4byte,0x90

	.globl	GNAME(alloc_16_to_eax)
	.type	GNAME(alloc_16_to_eax),@function
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
	.size	GNAME(alloc_16_to_eax),.-GNAME(alloc_16_to_eax)

	.globl	GNAME(alloc_to_ecx)
	.type	GNAME(alloc_to_ecx),@function
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
	.size	GNAME(alloc_to_ecx),.-GNAME(alloc_to_ecx)

	.globl	GNAME(alloc_8_to_ecx)
	.type	GNAME(alloc_8_to_ecx),@function
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
	.size	GNAME(alloc_8_to_ecx),.-GNAME(alloc_8_to_ecx)

	.globl	GNAME(alloc_16_to_ecx)
	.type	GNAME(alloc_16_to_ecx),@function
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
	.size	GNAME(alloc_16_to_ecx),.-GNAME(alloc_16_to_ecx)


	.globl	GNAME(alloc_to_edx)
	.type	GNAME(alloc_to_edx),@function
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
	.size	GNAME(alloc_to_edx),.-GNAME(alloc_to_edx)

	.globl	GNAME(alloc_8_to_edx)
	.type	GNAME(alloc_8_to_edx),@function
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
	.size	GNAME(alloc_8_to_edx),.-GNAME(alloc_8_to_edx)

	.globl	GNAME(alloc_16_to_edx)
	.type	GNAME(alloc_16_to_edx),@function
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
	.size	GNAME(alloc_16_to_edx),.-GNAME(alloc_16_to_edx)



	.globl	GNAME(alloc_to_ebx)
	.type	GNAME(alloc_to_ebx),@function
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
	.size	GNAME(alloc_to_ebx),.-GNAME(alloc_to_ebx)

	.globl	GNAME(alloc_8_to_ebx)
	.type	GNAME(alloc_8_to_ebx),@function
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
	.size	GNAME(alloc_8_to_ebx),.-GNAME(alloc_8_to_ebx)

	.globl	GNAME(alloc_16_to_ebx)
	.type	GNAME(alloc_16_to_ebx),@function
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
	.size	GNAME(alloc_16_to_ebx),.-GNAME(alloc_16_to_ebx)



	.globl	GNAME(alloc_to_esi)
	.type	GNAME(alloc_to_esi),@function
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
	.size	GNAME(alloc_to_esi),.-GNAME(alloc_to_esi)

	.globl	GNAME(alloc_8_to_esi)
	.type	GNAME(alloc_8_to_esi),@function
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
	.size	GNAME(alloc_8_to_esi),.-GNAME(alloc_8_to_esi)

	.globl	GNAME(alloc_16_to_esi)
	.type	GNAME(alloc_16_to_esi),@function
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
	.size	GNAME(alloc_16_to_esi),.-GNAME(alloc_16_to_esi)


	.globl	GNAME(alloc_to_edi)
	.type	GNAME(alloc_to_edi),@function
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
	.size	GNAME(alloc_to_edi),.-GNAME(alloc_to_edi)

	.globl	GNAME(alloc_8_to_edi)
	.type	GNAME(alloc_8_to_edi),@function
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
	.size	GNAME(alloc_8_to_edi),.-GNAME(alloc_8_to_edi)

	.globl	GNAME(alloc_16_to_edi)
	.type	GNAME(alloc_16_to_edi),@function
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
	.size	GNAME(alloc_16_to_edi),.-GNAME(alloc_16_to_edi)
		


#ifdef GENCGC_INLINE_ALLOC /* LISP_FEATURE_GENCGC */

/* These routines are called from Lisp when an inline allocation 
 * overflows. Every register except the result needs to be preserved.
 * We depend on C to preserve ebx, esi, edi, and ebp.
 * But where necessary must save eax, ecx, edx. */

/* This routine handles an overflow with eax=crfp+size. So the
 * size=eax-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_eax)
	.type	GNAME(alloc_overflow_eax),@function
GNAME(alloc_overflow_eax):
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%eax
	pushl	%eax		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_eax),.-GNAME(alloc_overflow_eax)

/* This routine handles an overflow with ecx=crfp+size. So the
 * size=ecx-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_ecx)
	.type	GNAME(alloc_overflow_ecx),@function
GNAME(alloc_overflow_ecx):
	pushl	%eax		# Save eax
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%ecx
	pushl	%ecx		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ecx	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_ecx),.-GNAME(alloc_overflow_ecx)

/* This routine handles an overflow with edx=crfp+size. So the
 * size=edx-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_edx)
	.type	GNAME(alloc_overflow_edx),@function
GNAME(alloc_overflow_edx):
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%edx
	pushl	%edx		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edx	# setup the destination.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_edx),.-GNAME(alloc_overflow_edx)

/* This routine handles an overflow with ebx=crfp+size. So the
 * size=ebx-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_ebx)
	.type	GNAME(alloc_overflow_ebx),@function
GNAME(alloc_overflow_ebx):
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%ebx
	pushl	%ebx		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%ebx	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_ebx),.-GNAME(alloc_overflow_ebx)

/* This routine handles an overflow with esi=crfp+size. So the
 * size=esi-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_esi)
	.type	GNAME(alloc_overflow_esi),@function
GNAME(alloc_overflow_esi):
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%esi
	pushl	%esi		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%esi	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_esi),.-GNAME(alloc_overflow_esi)

/* This routine handles an overflow with edi=crfp+size. So the
 * size=edi-crfp. */
	.align	align_4byte
	.globl	GNAME(alloc_overflow_edi)
	.type	GNAME(alloc_overflow_edi),@function
GNAME(alloc_overflow_edi):
	pushl	%eax		# Save eax
	pushl	%ecx		# Save ecx
	pushl	%edx		# Save edx
	/* Calculate the size for the allocation. */
	subl	GNAME(current_region_free_pointer),%edi
	pushl	%edi		# Push the size
	call	GNAME(alloc)
	addl	$4,%esp	# pop the size arg.
	movl	%eax,%edi	# setup the destination.
	popl	%edx	# Restore edx.
	popl	%ecx	# Restore ecx.
	popl	%eax	# Restore eax.
	addl	$6,(%esp) # Adjust the return address to skip the next inst.
	ret
	.size	 GNAME(alloc_overflow_edi),.-GNAME(alloc_overflow_edi)

#endif

	.end
