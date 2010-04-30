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
#include "genesis/config.h"
#include "validate.h"
#include "sbcl.h"
#include "genesis/closure.h"
#include "genesis/funcallable-instance.h"
#include "genesis/fdefn.h"
#include "genesis/static-symbols.h"
#include "genesis/symbol.h"
#include "genesis/thread.h"
	
/* Minimize conditionalization for different OS naming schemes. */
#if defined __linux__  || defined __FreeBSD__ || defined __OpenBSD__ || defined __NetBSD__ || defined __sun
#define GNAME(var) var
#else
#define GNAME(var) _##var
#endif

/* Get the right type of alignment. Linux, FreeBSD and OpenBSD
 * want alignment in bytes. */
#if defined(__linux__) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined __NetBSD__ || defined(__sun)
#define align_4byte	4
#define align_8byte	8
#define align_16byte	16
#define align_32byte	32
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
#if !defined(LISP_FEATURE_WIN32) && !defined(LISP_FEATURE_DARWIN)
#define TYPE(name) .type name,@function
#define SIZE(name) .size name,.-name
#define DOLLAR(name) $(name)
#else
#define TYPE(name)
#define SIZE(name)
#endif

/*
 * x86/darwin (as of MacOS X 10.4.5) doesn't reliably fire signal
 * handlers (SIGTRAP or Mach exception handlers) for 0xCC, wo we have
 * to use ud2 instead. ud2 is an undefined opcode, #x0b0f, or
 * 0F 0B in low-endian notation, that causes SIGILL to fire. We check
 * for this instruction in the SIGILL handler and if we see it, we
 * advance the EIP by two bytes to skip over ud2 instruction and
 * call sigtrap_handler. */
#if defined(LISP_FEATURE_UD2_BREAKPOINTS)
#define TRAP ud2
#else
#define TRAP int3
#endif

/*
 * More Apple assembler hacks
 */

#if defined(LISP_FEATURE_DARWIN)
/* global symbol x86-64 sym(%rip) hack:*/
#define GSYM(name) name(%rip)
#define END()
#else
#define GSYM(name) $name
#define END() .end
#endif

        
	.text
        .globl  GNAME(all_threads)
        
	

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
	.globl  GNAME(call_into_c)
	TYPE(GNAME(call_into_c))
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
	SIZE(GNAME(call_into_c))


	.text	
	.globl  GNAME(call_into_lisp_first_time)
	TYPE(GNAME(call_into_lisp_first_time))
		
/* We don't worry too much about saving registers 
 * here, because we never expect to return from the initial call to lisp 
 * anyway */
	
	.align	align_16byte,0x90
GNAME(call_into_lisp_first_time):
	push	%rbp		# Save old frame pointer.
	mov	%rsp,%rbp	# Establish new frame.
#if defined(LISP_FEATURE_DARWIN)
	movq    GSYM(GNAME(all_threads)),%rax
#else
        movq    GNAME(all_threads),%rax
#endif
	mov     THREAD_CONTROL_STACK_END_OFFSET(%rax) ,%rsp
	jmp     Lstack

	.text	
	.globl  GNAME(call_into_lisp)
	TYPE(GNAME(call_into_lisp))
		
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
	push	%rbx  	# these regs are callee-saved according to C
	push	%r12	# so must be preserved and restored when 
	push	%r13    # the lisp function returns
	push	%r14    #
	push	%r15    #

	mov	%rsp,%rbx	# remember current stack
	push	%rbx		# Save entry stack on (maybe) new stack.

	push	%rdi	# args from C
	push	%rsi	#
	push	%rdx	#
#ifdef LISP_FEATURE_SB_THREAD
#ifdef LISP_FEATURE_GCC_TLS
	movq	%fs:0, %rax
	movq	GNAME(current_thread)@TPOFF(%rax), %r12
#else
#ifdef LISP_FEATURE_DARWIN
	mov	GSYM(GNAME(specials)),%rdi
#else
	mov	specials,%rdi
#endif
	call    GNAME(pthread_getspecific)
	mov	%rax,%r12
#endif
#endif
	pop	%rcx	# num args
	pop	%rbx	# arg vector
	pop	%rax	# function ptr/lexenv

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
	push	%rbp            # Dummy for return address
	push	%rbp		# fp in save location S1
	mov	%rsp,%rbp	# The current sp marks start of new frame.
	sub	$8,%rsp	        # Ensure 3 slots are allocated, two above.

Lcall:
	call	*CLOSURE_FUN_OFFSET(%rax)
	
	/* If the function returned multiple values, it will return to
	   this point.  Lose them */
	jnc	LsingleValue	
	mov	%rbx, %rsp
LsingleValue:	

/* Restore the stack, in case there was a stack change. */
	pop	%rsp		# c-sp

/* Restore C regs */
	pop	%r15
	pop	%r14
	pop	%r13
	pop	%r12
	pop	%rbx

/* FIXME Restore the NPX state. */

	mov	%rdx,%rax	# c-val
	leave
	ret
	SIZE(GNAME(call_into_lisp))

/* support for saving and restoring the NPX state from C */
	.text
	.globl 	GNAME(fpu_save)
	TYPE(GNAME(fpu_save))
	.align	align_16byte,0x90
GNAME(fpu_save):
	fnsave	(%rdi)		# Save the NPX state. (resets NPX)
	ret
	SIZE(GNAME(fpu_save))

	.globl 	GNAME(fpu_restore)
	TYPE(GNAME(fpu_restore))
	.align	align_16byte,0x90
GNAME(fpu_restore):
	frstor	(%rdi)		# Restore the NPX state.
	ret
	SIZE(GNAME(fpu_restore))

/*
 * the undefined-function trampoline
 */
	.text
	.align	align_16byte,0x90
	.globl  GNAME(undefined_tramp)
	TYPE(GNAME(undefined_tramp))
GNAME(undefined_tramp):
	pop	8(%rbp)         # Save return PC for backtrace.
	TRAP
	.byte	trap_Error
        .byte   2
        .byte   UNDEFINED_FUN_ERROR
        .byte   sc_DescriptorReg # eax in the Descriptor-reg SC
	ret
	SIZE(GNAME(undefined_tramp))


	.text
	.align	align_16byte,0x90
	.globl  GNAME(alloc_tramp)
	TYPE(GNAME(alloc_tramp))
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
	call	GNAME(alloc)
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
	SIZE(GNAME(alloc_tramp))

		
/*
 * the closure trampoline
 */
	.text
	.align	align_16byte,0x90
	.globl  GNAME(closure_tramp)
	TYPE(GNAME(closure_tramp))
GNAME(closure_tramp):
	mov	FDEFN_FUN_OFFSET(%rax),%rax
	/* FIXME: The '*' after "jmp" in the next line is from PVE's
	 * patch posted to the CMU CL mailing list Oct 6, 1999. It looks
	 * reasonable, and it certainly seems as though if CMU CL needs it,
	 * SBCL needs it too, but I haven't actually verified that it's
	 * right. It would be good to find a way to force the flow of
	 * control through here to test it. */
	jmp	*CLOSURE_FUN_OFFSET(%rax)
	SIZE(GNAME(closure_tramp))

	.text
	.align	align_16byte,0x90
	.globl  GNAME(funcallable_instance_tramp)
#if !defined(LISP_FEATURE_DARWIN)
        .type	GNAME(funcallable_instance_tramp),@function
#endif
        GNAME(funcallable_instance_tramp):
	mov	FUNCALLABLE_INSTANCE_FUNCTION_OFFSET(%rax),%rax
	/* KLUDGE: on this platform, whatever kind of function is in %rax
	 * now, the first word of it contains the address to jump to. */
	jmp	*CLOSURE_FUN_OFFSET(%rax)
#if !defined(LISP_FEATURE_DARWIN)
	.size	GNAME(funcallable_instance_tramp), .-GNAME(funcallable_instance_tramp)
#endif
/*
 * fun-end breakpoint magic
 */
	.text
	.globl 	GNAME(fun_end_breakpoint_guts)
	.align	align_16byte
GNAME(fun_end_breakpoint_guts):
	/* Multiple Value return */
	jc	multiple_value_return
	/* Single value return: The eventual return will now use the
	   multiple values return convention but with a return values
	   count of one. */
	mov	%rsp,%rbx	# Setup ebx - the ofp.
	sub	$8,%rsp		# Allocate one stack slot for the return value
	mov	$8,%rcx		# Setup ecx for one return value.
#if defined(LISP_FEATURE_DARWIN)
	mov	GSYM(NIL),%rdi	# default second value
	mov	GSYM(NIL),%rsi	# default third value
#else
	mov	$NIL,%rdi	# default second value
	mov	$NIL,%rsi	# default third value
#endif
multiple_value_return:
	
	.globl  GNAME(fun_end_breakpoint_trap)
	.align	align_16byte,0x90
GNAME(fun_end_breakpoint_trap):
	TRAP
	.byte 	trap_FunEndBreakpoint
	hlt			# We should never return here.

	.globl  GNAME(fun_end_breakpoint_end)
GNAME(fun_end_breakpoint_end):


	.globl 	GNAME(do_pending_interrupt)
	TYPE(GNAME(do_pending_interrupt))
	.align	align_16byte,0x90
GNAME(do_pending_interrupt):
	TRAP
	.byte 	trap_PendingInterrupt
	ret
	SIZE(GNAME(do_pending_interrupt))

	.globl	GNAME(post_signal_tramp)
	TYPE(GNAME(post_signal_tramp))
	.align	align_16byte,0x90
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
        /* skip RBP and RSP */
	popq %rbx
	popq %rdx
	popq %rcx
	popq %rax
        popfq
	leave
	ret
	SIZE(GNAME(post_signal_tramp))

	.text
	.align	align_16byte,0x90
	.globl  GNAME(fast_bzero)
	TYPE(GNAME(fast_bzero))
        
GNAME(fast_bzero):
        /* A fast routine for zero-filling blocks of memory that are
         * guaranteed to start and end at a 4096-byte aligned address.
         */
        shr $6, %rsi              /* Amount of 64-byte blocks to copy */
        jz Lend                   /* If none, stop */
        mov %rsi, %rcx            /* Save start address */
        movups %xmm7, -16(%rsp)   /* Save XMM register */
        xorps  %xmm7, %xmm7       /* Zero the XMM register */
        jmp Lloop
        .align align_16byte                 
Lloop:

        /* Copy the 16 zeroes from xmm7 to memory, 4 times. MOVNTDQ is the
         * non-caching double-quadword moving variant, i.e. the memory areas
         * we're touching are not fetched into the L1 cache, since we're just
         * going to overwrite the memory soon anyway.
         */
        movntdq %xmm7, 0(%rdi)
        movntdq %xmm7, 16(%rdi)
        movntdq %xmm7, 32(%rdi)
        movntdq %xmm7, 48(%rdi)

        add $64, %rdi  /* Advance pointer */
        dec %rsi       /* Decrement 64-byte block count */
        jnz Lloop
        mfence         /* Ensure that the writes are globally visible, since
                        * MOVNTDQ is weakly ordered */
        movups -16(%rsp), %xmm7 /* Restore the XMM register */
        prefetcht0 0(%rcx)      /* Prefetch the start of the block into cache,
                                 * since it's likely to be used immediately. */
Lend:        
	ret
	SIZE(GNAME(fast_bzero))

        END()
