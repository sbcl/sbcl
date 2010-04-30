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
#include "genesis/funcallable-instance.h"
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
#if !defined(LISP_FEATURE_WIN32) && !defined(LISP_FEATURE_DARWIN)
#define TYPE(name) .type name,@function
#define SIZE(name) .size name,.-name
#else
#define TYPE(name)
#define SIZE(name)
#endif

/*
 * x86/darwin (as of MacOS X 10.4.5) doesn't reliably file signal
 * handlers (SIGTRAP or Mach exception handlers) for 0xCC, wo we have
 * to use ud2 instead. ud2 is an undefined opcode, #x0b0f, or
 * 0F 0B in low-endian notation, that causes SIGILL to fire. We check
 * for this instruction in the SIGILL handler and if we see it, we
 * advance the EIP by two bytes to skip over ud2 instruction and
 * call sigtrap_handler. */
#if defined(LISP_FEATURE_UD2_BREAKPOINTS)
#define END()
#define TRAP ud2
#else
#define END() .end
#define TRAP int3
#endif

	.text
	.globl	GNAME(all_threads)

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
 *
 * FIXME & OAOOM: This duplicates call-out in src/compiler/x86/c-call.lisp,
 * so if you tweak this, change that too!
 */
	.text
	.align	align_16byte,0x90
	.globl GNAME(call_into_c)
	TYPE(GNAME(call_into_c))
GNAME(call_into_c):
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

	call	*%eax		  # normal callout using Lisp stack
	movl	%eax,%ecx	  # remember integer return value

/* Check for a return FP value. */
	fxam
	fnstsw	%ax
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

/* Return. FIXME: It would be nice to restructure this to use RET. */	
	jmp	*%ebx

	SIZE(GNAME(call_into_c))


	.text	
	.globl GNAME(call_into_lisp_first_time)
	TYPE(GNAME(call_into_lisp_first_time))
		
/* We don't worry too much about saving registers 
 * here, because we never expect to return from the initial call to lisp 
 * anyway */
	
	.align	align_16byte,0x90
GNAME(call_into_lisp_first_time):
	pushl	%ebp		# Save old frame pointer.
	movl	%esp,%ebp	# Establish new frame.
#ifndef LISP_FEATURE_WIN32
	movl    GNAME(all_threads),%eax
	/* pthread machinery takes care of this for other threads */
	movl    THREAD_CONTROL_STACK_END_OFFSET(%eax) ,%esp
#else
/* Win32 -really- doesn't like you switching stacks out from under it. */
	movl	GNAME(all_threads),%eax
#endif
	jmp     Lstack

	.text	
	.globl GNAME(call_into_lisp)
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

#ifdef LISP_FEATURE_WIN32
	/* Establish an SEH frame. */
#ifdef LISP_FEATURE_SB_THREAD
	/* FIXME: need to save BSP here. */
#error "need to save BSP here, but don't know how yet."
#else
	pushl	BINDING_STACK_POINTER + SYMBOL_VALUE_OFFSET
#endif
	pushl	$GNAME(exception_handler_wrapper)
	pushl	%fs:0
	movl	%esp, %fs:0
#endif

	/* Alloc new frame. */
	push	%ebp            # Dummy for return address
	push	%ebp		# fp in save location S1
	mov	%esp,%ebp	# The current sp marks start of new frame.
	sub	$4,%esp		# Ensure 3 slots are allocated, two above.

	call	*CLOSURE_FUN_OFFSET(%eax)
	
	/* If the function returned multiple values, it will return to
	   this point.  Lose them */
	jnc	LsingleValue
	mov	%ebx, %esp
LsingleValue:
	/* A singled value function returns here */

#ifdef LISP_FEATURE_WIN32
	/* Remove our SEH frame. */
	popl	%fs:0
	add	$8, %esp
#endif

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
	.globl	GNAME(fpu_save)
	TYPE(GNAME(fpu_save))
	.align	2,0x90
GNAME(fpu_save):
	movl	4(%esp),%eax
	fnsave	(%eax)		# Save the NPX state. (resets NPX)
	ret
	SIZE(GNAME(fpu_save))

	.globl	GNAME(fpu_restore)
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
	.align	align_16byte,0x90
	.globl GNAME(undefined_tramp)
	TYPE(GNAME(undefined_tramp))
        .byte   0, 0, 0, SIMPLE_FUN_HEADER_WIDETAG
GNAME(undefined_tramp):
	pop	4(%ebp)         # Save return PC for backtrace.
	TRAP
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
	.align	align_16byte,0x90
	.globl GNAME(closure_tramp)
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

	.text
	.align	align_16byte,0x90
	.globl GNAME(funcallable_instance_tramp)
	TYPE(GNAME(funcallable_instance_tramp))
GNAME(funcallable_instance_tramp):
	movl	FUNCALLABLE_INSTANCE_FUNCTION_OFFSET(%eax),%eax	
	/* KLUDGE: on this platform, whatever kind of function is in %rax
	 * now, the first word of it contains the address to jump to. */
	jmp	*CLOSURE_FUN_OFFSET(%eax)
	SIZE(GNAME(funcallable_instance_tramp))
	
/*
 * fun-end breakpoint magic
 */
	.text
	.globl	GNAME(fun_end_breakpoint_guts)
	.align	align_16byte
GNAME(fun_end_breakpoint_guts):
	/* Multiple Value return */
	jc	multiple_value_return
	/* Single value return: The eventual return will now use the
	   multiple values return convention but with a return values
	   count of one. */
	movl	%esp,%ebx	# Setup ebx - the ofp.
	subl	$4,%esp		# Allocate one stack slot for the return value
	movl	$4,%ecx		# Setup ecx for one return value.
	movl	$(NIL),%edi	# default second value
	movl	$(NIL),%esi	# default third value
		
multiple_value_return:
	
	.globl GNAME(fun_end_breakpoint_trap)
GNAME(fun_end_breakpoint_trap):
	TRAP
	.byte 	trap_FunEndBreakpoint
	hlt			# We should never return here.

	.globl GNAME(fun_end_breakpoint_end)
GNAME(fun_end_breakpoint_end):


	.globl	GNAME(do_pending_interrupt)
	TYPE(GNAME(do_pending_interrupt))
	.align	align_16byte,0x90
GNAME(do_pending_interrupt):
	TRAP
	.byte 	trap_PendingInterrupt
	ret
	SIZE(GNAME(do_pending_interrupt))

/* Allocate bytes and return the start of the allocated space
 * in the specified destination register.
 *
 * In the general case the size will be in the destination register.
 *
 * All registers must be preserved except the destination.
 * The C conventions will preserve ebx, esi, edi, and ebp.
 * So only eax, ecx, and edx need special care here.
 *
 * ALLOC factors out the logic of calling alloc(): stack alignment, etc.
 *
 * DEFINE_ALLOC_TO_FOO defines an alloction routine.
 */

#ifdef LISP_FEATURE_DARWIN
#define ALLOC(size)                                             \
        pushl   %ebp;              /* Save EBP               */ \
        movl    %esp,%ebp;         /* Save ESP to EBP        */ \
        pushl   $0;                /* Reserve space for arg  */ \
        andl    $0xfffffff0,%esp;  /* Align stack to 16bytes */ \
        movl    size, (%esp);      /* Argument to alloc      */ \
        call    GNAME(alloc);                                   \
        movl    %ebp,%esp;         /* Restore ESP from EBP   */ \
        popl    %ebp;              /* Restore EBP            */
#else
#define ALLOC(size)                                             \
        pushl   size;              /* Argument to alloc      */ \
        call    GNAME(alloc);                                   \
        addl    $4,%esp;           /* Pop argument           */
#endif

#define DEFINE_ALLOC_TO_EAX(name,size)                          \
        .globl  GNAME(name);                                    \
        TYPE(GNAME(name));                                      \
        .align  align_16byte,0x90;                              \
GNAME(name):                                                    \
        pushl   %ecx;              /* Save ECX and EDX       */ \
        pushl   %edx;                                           \
        ALLOC(size)                                             \
        popl    %edx;              /* Restore ECX and EDX    */ \
        popl    %ecx;                                           \
        ret;                                                    \
        SIZE(GNAME(name))

#define DEFINE_ALLOC_TO_ECX(name,size)                          \
        .globl  GNAME(name);                                    \
        TYPE(GNAME(name));                                      \
        .align  align_16byte,0x90;                              \
GNAME(name):                                                    \
        pushl   %eax;              /* Save EAX and EDX       */ \
        pushl   %edx;                                           \
        ALLOC(size)                                             \
        movl    %eax,%ecx;         /* Result to destination  */ \
        popl    %edx;                                           \
        popl    %eax;                                           \
        ret;                                                    \
        SIZE(GNAME(name))
        
#define DEFINE_ALLOC_TO_EDX(name,size)                          \
        .globl  GNAME(name);                                    \
        TYPE(GNAME(name));                                      \
        .align  align_16byte,0x90;                              \
GNAME(name):                                                    \
        pushl   %eax;               /* Save EAX and ECX      */ \
        pushl   %ecx;                                           \
        ALLOC(size)                                             \
        movl    %eax,%edx;          /* Restore EAX and ECX   */ \
        popl    %ecx;                                           \
        popl    %eax;                                           \
        ret;                                                    \
        SIZE(GNAME(name))

#define DEFINE_ALLOC_TO_REG(name,reg,size)                      \
        .globl  GNAME(name);                                    \
        TYPE(GNAME(name));                                      \
        .align  align_16byte,0x90;                              \
GNAME(name):                                                    \
        pushl   %eax;              /* Save EAX, ECX, and EDX */ \
        pushl   %ecx;                                           \
        pushl   %edx;                                           \
        ALLOC(size)                                             \
        movl    %eax,reg;          /* Restore them           */ \
        popl    %edx;                                           \
        popl    %ecx;                                           \
        popl    %eax;                                           \
        ret;                                                    \
	SIZE(GNAME(name))

DEFINE_ALLOC_TO_EAX(alloc_to_eax,%eax)
DEFINE_ALLOC_TO_EAX(alloc_8_to_eax,$8)
DEFINE_ALLOC_TO_EAX(alloc_16_to_eax,$16)

DEFINE_ALLOC_TO_ECX(alloc_to_ecx,%ecx)
DEFINE_ALLOC_TO_ECX(alloc_8_to_ecx,$8)
DEFINE_ALLOC_TO_ECX(alloc_16_to_ecx,$16)

DEFINE_ALLOC_TO_EDX(alloc_to_edx,%edx)
DEFINE_ALLOC_TO_EDX(alloc_8_to_edx,$8)
DEFINE_ALLOC_TO_EDX(alloc_16_to_edx,$16)

DEFINE_ALLOC_TO_REG(alloc_to_ebx,%ebx,%ebx)
DEFINE_ALLOC_TO_REG(alloc_8_to_ebx,%ebx,$8)
DEFINE_ALLOC_TO_REG(alloc_16_to_ebx,%ebx,$16)

DEFINE_ALLOC_TO_REG(alloc_to_esi,%esi,%esi)
DEFINE_ALLOC_TO_REG(alloc_8_to_esi,%esi,$8)
DEFINE_ALLOC_TO_REG(alloc_16_to_esi,%esi,$16)

DEFINE_ALLOC_TO_REG(alloc_to_edi,%edi,%edi)
DEFINE_ALLOC_TO_REG(alloc_8_to_edi,%edi,$8)
DEFINE_ALLOC_TO_REG(alloc_16_to_edi,%edi,$16)

/* Called from lisp when an inline allocation overflows.
 * Every register except the result needs to be preserved.
 * We depend on C to preserve ebx, esi, edi, and ebp.
 * But where necessary must save eax, ecx, edx. */

#ifdef LISP_FEATURE_SB_THREAD
#define START_REGION %fs:THREAD_ALLOC_REGION_OFFSET
#else
#define START_REGION GNAME(boxed_region)
#endif

#define ALLOC_OVERFLOW(size)                                    \
        /* Calculate the size for the allocation. */            \
        subl    START_REGION,size;                              \
        ALLOC(size)

/* This routine handles an overflow with eax=crfp+size. So the
   size=eax-crfp. */
        .align  align_16byte
        .globl  GNAME(alloc_overflow_eax)
	TYPE(GNAME(alloc_overflow_eax))
GNAME(alloc_overflow_eax):
        pushl   %ecx            # Save ecx
        pushl   %edx            # Save edx
	ALLOC_OVERFLOW(%eax)
        popl    %edx    # Restore edx.
        popl    %ecx    # Restore ecx.
        ret
        SIZE(GNAME(alloc_overflow_eax))

        .align  align_16byte
        .globl  GNAME(alloc_overflow_ecx)
	TYPE(GNAME(alloc_overflow_ecx))
GNAME(alloc_overflow_ecx):
        pushl   %eax            # Save eax
        pushl   %edx            # Save edx
	ALLOC_OVERFLOW(%ecx)
        movl    %eax,%ecx       # setup the destination.
        popl    %edx    # Restore edx.
        popl    %eax    # Restore eax.
        ret
        SIZE(GNAME(alloc_overflow_ecx))

        .align  align_16byte
        .globl  GNAME(alloc_overflow_edx)
        TYPE(GNAME(alloc_overflow_edx))
GNAME(alloc_overflow_edx):
        pushl   %eax            # Save eax
        pushl   %ecx            # Save ecx
	ALLOC_OVERFLOW(%edx)
        movl    %eax,%edx       # setup the destination.
        popl    %ecx    # Restore ecx.
        popl    %eax    # Restore eax.
        ret
        SIZE(GNAME(alloc_overflow_edx))

/* This routine handles an overflow with ebx=crfp+size. So the
   size=ebx-crfp. */
        .align  align_16byte
        .globl  GNAME(alloc_overflow_ebx)
        TYPE(GNAME(alloc_overflow_ebx))
GNAME(alloc_overflow_ebx):
        pushl   %eax            # Save eax
        pushl   %ecx            # Save ecx
        pushl   %edx            # Save edx
	ALLOC_OVERFLOW(%ebx)
        movl    %eax,%ebx       # setup the destination.
        popl    %edx    # Restore edx.
        popl    %ecx    # Restore ecx.
        popl    %eax    # Restore eax.
        ret
        SIZE(GNAME(alloc_overflow_ebx))

/* This routine handles an overflow with esi=crfp+size. So the
   size=esi-crfp. */
        .align  align_16byte
        .globl  GNAME(alloc_overflow_esi)
        TYPE(GNAME(alloc_overflow_esi))
GNAME(alloc_overflow_esi):
        pushl   %eax            # Save eax
        pushl   %ecx            # Save ecx
        pushl   %edx            # Save edx
	ALLOC_OVERFLOW(%esi)
        movl    %eax,%esi       # setup the destination.
        popl    %edx    # Restore edx.
        popl    %ecx    # Restore ecx.
        popl    %eax    # Restore eax.
        ret
        SIZE(GNAME(alloc_overflow_esi))

        .align  align_16byte
        .globl  GNAME(alloc_overflow_edi)
        TYPE(GNAME(alloc_overflow_edi))
GNAME(alloc_overflow_edi):
        pushl   %eax            # Save eax
        pushl   %ecx            # Save ecx
        pushl   %edx            # Save edx
	ALLOC_OVERFLOW(%edi)
        movl    %eax,%edi       # setup the destination.
        popl    %edx    # Restore edx.
        popl    %ecx    # Restore ecx.
        popl    %eax    # Restore eax.
        ret
        SIZE(GNAME(alloc_overflow_edi))


#ifdef LISP_FEATURE_WIN32
	/* The guts of the exception-handling system doesn't use
	 * frame pointers, which manages to throw off backtraces
	 * rather badly.  So here we grab the (known-good) EBP
	 * and EIP from the exception context and use it to fake
	 * up a stack frame which will skip over the system SEH
	 * code. */
	.align	align_16byte
	.globl	GNAME(exception_handler_wrapper)
	TYPE(GNAME(exception_handler_wrapper))
GNAME(exception_handler_wrapper):
	/* Context layout is: */
	/* 7 dwords before FSA. (0x1c) */
	/* 8 dwords and 0x50 bytes in the FSA. (0x70/0x8c) */
	/* 4 dwords segregs. (0x10/0x9c) */
	/* 6 dwords non-stack GPRs. (0x18/0xb4) */
	/* EBP (at 0xb4) */
	/* EIP (at 0xb8) */
#define CONTEXT_EBP_OFFSET 0xb4
#define CONTEXT_EIP_OFFSET 0xb8
	/* some other stuff we don't care about. */
	pushl	%ebp
	movl	0x10(%esp), %ebp	/* context */
	pushl	CONTEXT_EIP_OFFSET(%ebp)
	pushl	CONTEXT_EBP_OFFSET(%ebp)
	movl	%esp, %ebp
	pushl	0x1c(%esp)
	pushl	0x1c(%esp)
	pushl	0x1c(%esp)
	pushl	0x1c(%esp)
	call	GNAME(handle_exception)
	lea	8(%ebp), %esp
	popl	%ebp
	ret
	SIZE(GNAME(exception_handler_wrapper))
#endif

#ifdef LISP_FEATURE_DARWIN
        .align align_16byte
        .globl GNAME(call_into_lisp_tramp)
        TYPE(GNAME(call_into_lisp_tramp))
GNAME(call_into_lisp_tramp):
        /* 1. build the stack frame from the block that's pointed to by ECX
           2. free the block
           3. set ECX to 0
           4. call the function via call_into_lisp
        */
        pushl   0(%ecx)          /* return address */

        pushl   %ebp
        movl    %esp, %ebp

        pushl   32(%ecx)         /* eflags */
        pushl   28(%ecx)         /* EAX */
        pushl   20(%ecx)         /* ECX */
        pushl   16(%ecx)         /* EDX */
        pushl   24(%ecx)         /* EBX */
        pushl   $0                /* popal is going to ignore esp */
        pushl   %ebp              /* is this right?? */
        pushl   12(%ecx)         /* ESI */
        pushl   8(%ecx)          /* EDI */
        pushl   $0                /* args for call_into_lisp */
        pushl   $0
        pushl   4(%ecx)          /* function to call */

        /* free our save block */
        pushl   %ecx              /* reserve sufficient space on stack for args */
        pushl   %ecx
        andl    $0xfffffff0, %esp  /* align stack */
        movl    $0x40, 4(%esp)
        movl    %ecx, (%esp)
        call    GNAME(os_invalidate)

        /* call call_into_lisp */
        leal    -48(%ebp), %esp
        call    GNAME(call_into_lisp)

        /* Clean up our mess */
        leal    -36(%ebp), %esp
        popal
        popfl
        leave
        ret
        
        SIZE(call_into_lisp_tramp)
#endif
        
	.align	align_16byte,0x90
	.globl	GNAME(post_signal_tramp)
	TYPE(GNAME(post_signal_tramp))
GNAME(post_signal_tramp):
	/* this is notionally the second half of a function whose first half
 	 * doesn't exist.  This is where call_into_lisp returns when called 
	 * using return_to_lisp_function */
	addl $12,%esp  	/* clear call_into_lisp args from stack */
	popal           /* restore registers */
        popfl
#ifdef LISP_FEATURE_DARWIN
        /* skip two padding words */
        addl $8,%esp
#endif
	leave
	ret
	SIZE(GNAME(post_signal_tramp))


        /* fast_bzero implementations and code to detect which implementation
         * to use.
         */

	.globl GNAME(fast_bzero_pointer)
	.data
        .align  align_16byte
GNAME(fast_bzero_pointer):
        /* Variable containing a pointer to the bzero function to use.
         * Initially points to a basic function.  Change this variable
         * to fast_bzero_detect if OS supports SSE.  */
        .long GNAME(fast_bzero_base)

	.text
	.align	align_16byte,0x90
	.globl GNAME(fast_bzero)
	TYPE(GNAME(fast_bzero))
GNAME(fast_bzero):        
        /* Indirect function call */
        jmp *GNAME(fast_bzero_pointer)
	SIZE(GNAME(fast_bzero))
        
      
	.text
	.align	align_16byte,0x90
	.globl GNAME(fast_bzero_detect)
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
        movl $(GNAME(fast_bzero_base)), GNAME(fast_bzero_pointer)
        jmp Lrestore
Lsse2:
        movl $(GNAME(fast_bzero_sse)), GNAME(fast_bzero_pointer)
        jmp Lrestore
        
Lrestore:
        pop %edx
        pop %ecx
        pop %ebx
        pop %eax
        jmp *GNAME(fast_bzero_pointer)
        
	SIZE(GNAME(fast_bzero_detect))
        

	.text
	.align	align_16byte,0x90
	.globl GNAME(fast_bzero_sse)
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
        .align align_16byte
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
	.align	align_16byte,0x90
	.globl GNAME(fast_bzero_base)
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

        rep
        stosl                     /* Store EAX to *EDI, ECX times, incrementing
                                   * EDI by 4 after each store */
        
Lend_base:        
        pop %edi                  /* Restore temp registers */
        pop %ecx
        pop %eax
	ret
	SIZE(GNAME(fast_bzero_base))
        
	
        END()
