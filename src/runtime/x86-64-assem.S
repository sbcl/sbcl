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
#define align_32byte	32
#else
#define	align_4byte	2
#define	align_8byte	3
#define	align_16byte	4	
#endif			

	.text
	.global	GNAME(foreign_function_call_active)
	.global	GNAME(all_threads)
	

/* From lower to higher-numbered addresses, the stack contains 
 * return address, arg 0, arg 1, arg 2 ...
 * rax contains the address of the function to call
 * Lisp expects return value in rax, whic is already consistent with C
 * XXXX correct floating point handling is unimplemented so far
 * Based on comments cleaned from x86-assem.S, we believe that 
 * Lisp is expecting us to preserve rsi, rdi, rsp (no idea about r8-15)
 */
	.text
	.align	align_16byte,0x90
	.global GNAME(call_into_c)
	.type	GNAME(call_into_c),@function
GNAME(call_into_c):
	push	%rbp		# Save old frame pointer.
	mov	%rsp,%rbp	# Establish new frame.

	push	%rsi            # args are going in here
	push	%rdi
	mov	16(%rbp),%rdi
	mov	24(%rbp),%rsi
	mov	32(%rbp),%rdx
	mov	40(%rbp),%rcx
	mov	48(%rbp),%rcx
	mov	56(%rbp),%r8
	mov	64(%rbp),%r9
	call	*%rax
	mov	%rbp,%rsp
	pop	%rbp
	ret
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
	push	%rbp		# Save old frame pointer.
	mov	%rsp,%rbp	# Establish new frame.
	mov    %rsp,ALIEN_STACK + SYMBOL_VALUE_OFFSET
	mov    GNAME(all_threads),%rax
	mov    THREAD_CONTROL_STACK_START_OFFSET(%rax) ,%rsp
	/* don't think too hard about what happens if we get interrupted
	* here */
	add 	$THREAD_CONTROL_STACK_SIZE-8,%rsp
	jmp     Lstack

	.text	
	.global GNAME(call_into_lisp)
	.type  GNAME(call_into_lisp),@function
		
/*
 * amd64 calling convention: C expects that
 * arguments go in rdi rsi rdx rcx r8 r9
 * return values in rax rdx
 * callee saves rbp rbx r12-15 if it uses them
 */
	
	.align	align_16byte,0x90
GNAME(call_into_lisp):
	push	%rbp		# Save old frame pointer.
	mov	%rsp,%rbp	# Establish new frame.
Lstack:
	/* FIXME x86 saves FPU state here */
	push	%rbx
	push	%r12
	push	%r13
	push	%r14
	push	%r15


	mov	%rsp,%rbx	# remember current stack
	push	%rbx		# Save entry stack on (maybe) new stack.

	/* Establish Lisp args. */
	mov	%rdi,%rax	# lexenv?
	mov	%rsi,%rbx	# address of arg vec
	mov	%rdx,%rcx	# num args

	xor	%rdx,%rdx	# clear any descriptor registers 
	xor	%rdi,%rdi	# that we can't be sure we'll 
	xor	%rsi,%rsi	# initialise properly.  XX do r8-r15 too?
	shl	$3,%rcx		# (fixnumize num-args)
	cmp	$0,%rcx
	je	Ldone
	mov	0(%rbx),%rdx	# arg0
	cmp	$8,%rcx
	je	Ldone
	mov	8(%rbx),%rdi	# arg1
	cmp	$16,%rcx
	je	Ldone
	mov	16(%rbx),%rsi	# arg2
Ldone:	
	/* Registers rax, rcx, rdx, rdi, and rsi are now live. */
	xor	%rbx,%rbx	# available

	/* Alloc new frame. */
	mov	%rsp,%rbx	# The current sp marks start of new frame.
	push	%rbp		# fp in save location S0
	sub	$16,%rsp	# Ensure 3 slots are allocated, one above.
	mov	%rbx,%rbp	# Switch to new frame.

Lcall:
	call	*CLOSURE_FUN_OFFSET(%rax)
	
	/* If the function returned multiple values, it will return to
	   this point.  Lose them */
	mov	%rbx, %rsp
	/* A singled value function returns here */

/* Restore the stack, in case there was a stack change. */
	pop	%rsp		# c-sp

/* Restore C regs */
	pop	%r15
	pop	%r14
	pop	%r13
	pop	%r12
	pop	%rbx

/* FIXME Restore the NPX state. */
	pop	%rbp		# c-sp
	/* return value is already in rax where lisp expects it */
	ret
	.size	GNAME(call_into_lisp), . - GNAME(call_into_lisp)

/* support for saving and restoring the NPX state from C */
	.text
	.global	GNAME(fpu_save)
	.type	GNAME(fpu_save),@function
	.align	2,0x90
GNAME(fpu_save):
	mov	4(%rsp),%rax
	fnsave	(%rax)		# Save the NPX state. (resets NPX)
	ret
	.size	GNAME(fpu_save),.-GNAME(fpu_save)

	.global	GNAME(fpu_restore)
	.type	GNAME(fpu_restore),@function
	.align	2,0x90
GNAME(fpu_restore):
	mov	4(%rsp),%rax
	frstor	(%rax)		# Restore the NPX state.
	ret
	.size	GNAME(fpu_restore),.-GNAME(fpu_restore)

/*
 * the undefined-function trampoline
 */
	.text
	.align	align_8byte,0x90
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


	.text
	.align	align_8byte,0x90
	.global GNAME(alloc_tramp)
	.type	GNAME(alloc_tramp),@function
GNAME(alloc_tramp):
	push	%rbp		# Save old frame pointer.
	mov	%rsp,%rbp	# Establish new frame.
	push	%rax
	push	%rcx
	push	%rdx
	push	%rsi
	push	%rdi
	push	%r8
	push	%r9
	push	%r10
	push	%r11
	mov	16(%rbp),%rdi	
	call	alloc
	mov	%rax,16(%rbp)
	pop	%r11
	pop	%r10
	pop	%r9
	pop	%r8
	pop	%rdi
	pop	%rsi
	pop	%rdx
	pop	%rcx
	pop	%rax
	pop	%rbp
	ret
	.size	GNAME(alloc_tramp),.-GNAME(alloc_tramp)

		
/*
 * the closure trampoline
 */
	.text
	.align	align_8byte,0x90
	.global GNAME(closure_tramp)
	.type	GNAME(closure_tramp),@function
GNAME(closure_tramp):
	mov	FDEFN_FUN_OFFSET(%rax),%rax
	/* FIXME: The '*' after "jmp" in the next line is from PVE's
	 * patch posted to the CMU CL mailing list Oct 6, 1999. It looks
	 * reasonable, and it certainly seems as though if CMU CL needs it,
	 * SBCL needs it too, but I haven't actually verified that it's
	 * right. It would be good to find a way to force the flow of
	 * control through here to test it. */
	jmp	*CLOSURE_FUN_OFFSET(%rax)
	.size	GNAME(closure_tramp), .-GNAME(closure_tramp)

/*
 * fun-end breakpoint magic
 */
	.text
	.global	GNAME(fun_end_breakpoint_guts)
	.align	align_8byte
GNAME(fun_end_breakpoint_guts):
	/* Multiple Value return */
	jmp	multiple_value_return
	/* the above jmp is only 2 bytes long, we need to add a nop for 
	 * padding since the single value return convention jumps to original
	 * return address + 3 bytes */
	nop
	/* Single value return: The eventual return will now use the
	   multiple values return convention but with a return values
	   count of one. */
	mov	%rsp,%rbx	# Setup ebx - the ofp.
	sub	$8,%rsp		# Allocate one stack slot for the return value
	mov	$8,%rcx		# Setup ecx for one return value.
	mov	$NIL,%rdi	# default second value
	mov	$NIL,%rsi	# default third value
		
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
	.align	align_8byte,0x90
GNAME(do_pending_interrupt):
	int3
	.byte 	trap_PendingInterrupt
	ret
	.size	GNAME(do_pending_interrupt),.-GNAME(do_pending_interrupt)

	.globl	GNAME(post_signal_tramp)
	.type	GNAME(post_signal_tramp),@function
	.align	align_8byte,0x90
GNAME(post_signal_tramp):
	/* this is notionally the second half of a function whose first half
 	 * doesn't exist.  This is where call_into_lisp returns when called 
	 * using return_to_lisp_function */
	popq %r15
	popq %r14
	popq %r13
	popq %r12
	popq %r11
	popq %r10
	popq %r9
	popq %r8
	popq %rdi
	popq %rsi
	addq $8, %rsp
	popq %rsp
	popq %rdx
	popq %rbx
	popq %rcx
	popq %rax
	leave
	ret
	.size GNAME(post_signal_tramp),.-GNAME(post_signal_tramp)

	.end
