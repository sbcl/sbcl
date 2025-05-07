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

#ifdef __ELF__
// Mark the object as not requiring an executable stack.
.section .note.GNU-stack,"",%progbits
#endif

#include "genesis/sbcl.h"
#include "validate.h"
#include "genesis/closure.h"
#include "genesis/static-symbols.h"
#include "genesis/thread.h"

/* Minimize conditionalization for different OS naming schemes. */
#if defined __linux__  || defined LISP_FEATURE_HAIKU || defined LISP_FEATURE_FREEBSD || \
   defined __OpenBSD__ || defined __NetBSD__ || defined __sun || defined _WIN64 || defined __DragonFly__
#define GNAME(var) var
#else
#define GNAME(var) _##var
#endif

// Produce position-independent code.
// macOS does not like the general way we do this, so do something different.
#if defined(LISP_FEATURE_DARWIN) || defined(LISP_FEATURE_WIN32)
#define LOAD_PIC_VAR(x,dest) mov GNAME(x)(%rip), dest
#else
#define LOAD_PIC_VAR(x,dest) mov GNAME(x)@GOTPCREL(%rip), dest ; mov (dest), dest
#endif

/* Get the right type of alignment. Linux, FreeBSD and OpenBSD
 * want alignment in bytes. */
#if defined(__linux__) || defined(LISP_FEATURE_FREEBSD) || defined(__OpenBSD__) || defined __NetBSD__ || defined(__sun) || defined _WIN64 || defined(__DragonFly__)
#define align_16byte	16
#else
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

#ifdef LISP_FEATURE_INT4_BREAKPOINTS
// assembler won't emit the invalid instruction INTO, so use .byte to encode
#define TRAP .byte 0xCE
#else
#define TRAP int3
#endif

#define THREAD_BASE_REG %r13
#define reg_NULL %r12

#ifdef LISP_FEATURE_WIN32
#define CARG1 %rcx
#define CARG2 %rdx
#define CARG3 %r8
#else
#define CARG1 %rdi
#define CARG2 %rsi
#define CARG3 %rdx
#endif
	
#ifdef LISP_FEATURE_OS_THREAD_STACK
        .text
	.globl  GNAME(funcall1_switching_stack)
	TYPE(GNAME(funcall1_switching_stack))
	.align	align_16byte,0x90
GNAME(funcall1_switching_stack):
        /* The arguments are switched, funcall1_switching_stack(arg, function)
           to avoid shuffling registers.
        */
        push    %rbp
        mov     %rsp,%rbp

        mov     THREAD_CONTROL_STACK_END_OFFSET(CARG1),%rsp

#ifdef LISP_FEATURE_WIN32	
	/* _chkstk() is called at some unknown points and is
           expecting that
	*/
	mov 	THREAD_CONTROL_STACK_START_OFFSET(CARG1), %rax
	movq    %rax, %gs:16
#endif
        call    *CARG2

        mov     %rbp, %rsp
        pop     %rbp
        ret
        SIZE(GNAME(funcall1_switching_stack))
#endif

	.text
	.globl  GNAME(call_into_lisp_first_time_)
	TYPE(GNAME(call_into_lisp_first_time_))

/* We don't worry too much about saving registers
 * here, because we never expect to return from the initial call to lisp
 * anyway */

	.align	align_16byte,0x90
GNAME(call_into_lisp_first_time_):
GNAME(lspmain): # so much easier to type 'b lspmain' in gdb
	push	%rbp		# Save old frame pointer.
	mov	%rsp,%rbp	# Establish new frame.
	LOAD_PIC_VAR(all_threads, %rax)
	mov     THREAD_CONTROL_STACK_END_OFFSET(%rax) ,%rsp
	jmp     Lstack

	.text
	.globl  GNAME(call_into_lisp_)
	TYPE(GNAME(call_into_lisp_))

/*
 * amd64 calling convention: C expects that
 * arguments go in rdi rsi rdx rcx r8 r9
 * return values in rax rdx
 * callee saves rbp rbx r12-15 if it uses them
 */
#ifdef LISP_FEATURE_WIN32
# define SUPPORT_FOMIT_FRAME_POINTER
#endif
	.align	align_16byte,0x90
GNAME(call_into_lisp_):
#ifdef SUPPORT_FOMIT_FRAME_POINTER
	mov	%rbp,%rax
#endif
	push	%rbp		# Save old frame pointer.
	mov	%rsp,%rbp	# Establish new frame.
Lstack:
#ifdef SUPPORT_FOMIT_FRAME_POINTER
	/* If called through call_into_lisp_first_time, %r15 becomes invalid
	 * here, but we will not return in that case. */
	push	%r15
	mov	%rax,%r15
#endif
	/* FIXME x86 saves FPU state here */
	push	%rbx  	# these regs are callee-saved according to C
	push	%r12	# so must be preserved and restored when
	push	%r13    # the lisp function returns
	push	%r14    #
	push	%r15    #
	push	%rdi	# args from C
	push	%rsi	#
	push	%rdx	#
	movq	%rcx, THREAD_BASE_REG
	pop	%rcx	# num args
	pop	%rbx	# arg vector
	pop	%rax	# function ptr/lexenv

	# Why do we care what goes in unused argument-passing regs?
	# These just seem like wasted instructions.
	xor	%rdx,%rdx	# clear any descriptor registers
	xor	%rdi,%rdi	# that we can't be sure we'll
	xor	%rsi,%rsi	# initialise properly.  XX do r8-r15 too?
	cmp	$2,%rcx
	je	Ltwo
	cmp	$1,%rcx
	je	Lone
	jl	Lzero
	mov	16(%rbx),%rsi	# arg2
Ltwo:	mov	8(%rbx),%rdi	# arg1
Lone:	mov	0(%rbx),%rdx	# arg0
Lzero:
	shl	$(N_FIXNUM_TAG_BITS),%rcx	# (fixnumize num-args)
	/* Registers rax, rcx, rdx, rdi, and rsi are now live. */
	xor	%rbx,%rbx	# available

	/* Alloc new frame. */
	push	%rbp            # Dummy for return address
	push	%rbp		# fp in save location S1
	mov	%rsp,%rbp	# The current sp marks start of new frame.
Lcall:
	LOAD_PIC_VAR(gc_card_mark, reg_NULL)
	sub	NIL_CARDTABLE_DISP, reg_NULL
	call	*CLOSURE_FUN_OFFSET(%rax)

	/* If the function returned multiple values, the carry flag will be set.
           Lose them */
	jnc	LsingleValue
	mov	%rbx, %rsp
LsingleValue:

/* Restore C regs */
	pop	%r15
	pop	%r14
	pop	%r13
	pop	%r12
	pop	%rbx

/* FIXME Restore the NPX state. */

	mov	%rdx,%rax	# c-val
#ifdef SUPPORT_FOMIT_FRAME_POINTER
	mov	%r15,%rbp	# orig rbp
	pop	%r15		# orig r15
	add	$8,%rsp		# no need for saved (overridden) rbp
#else
	leave
#endif
	ret
	SIZE(GNAME(call_into_lisp_))

	.text
	.globl  GNAME(funcall_alien_callback)
	TYPE(GNAME(funcall_alien_callback))
	.align	align_16byte,0x90
GNAME(funcall_alien_callback):
/* Specialized call_into_lisp for callbacks
   rdi arg1
   rsi arg2
   rdx arg0
   rcx thread #+sb-thread
*/

	push	%rbp		# Save old frame pointer.
	mov	%rsp,%rbp	# Establish new frame.

	push	%rbx  	# these regs are callee-saved according to C
	push	%r12	# so must be preserved and restored when
	push	%r13    # the lisp function returns
	push	%r14    #
	push	%r15    #
#ifdef LISP_FEATURE_SB_THREAD
        mov     %rcx, THREAD_BASE_REG
#else
        LOAD_PIC_VAR(all_threads, THREAD_BASE_REG)
#endif
        mov     $(3 << N_FIXNUM_TAG_BITS),%rcx

	/* Alloc new frame. */
        push	%rbp            # Dummy for return address
	push	%rbp		# fp in save location S1
        mov	%rsp,%rbp
	LOAD_PIC_VAR(gc_card_mark, reg_NULL)
	sub	NIL_CARDTABLE_DISP, reg_NULL
	LOAD_PIC_VAR(linkage_space, %rax)
	call	*(8*ENTER_ALIEN_CALLBACK_fname_index)(%rax)

/* Restore C regs */
	pop	%r15
	pop	%r14
	pop	%r13
	pop	%r12
	pop	%rbx

	leave

	ret
	SIZE(GNAME(funcall_alien_callback))

/*
 * fun-end breakpoint magic
 */

/*
 * For an explanation of the magic involved in function-end
 * breakpoints, see the implementation in ppc-assem.S.
 */

	.text
	.globl 	GNAME(fun_end_breakpoint_guts)
GNAME(fun_end_breakpoint_guts):
	/* Multiple Value return */
	jc	multiple_value_return
	/* Single value return: The eventual return will now use the
	   multiple values return convention but with a return values
	   count of one. */
	mov	%rsp,%rbx	# Setup ebx - the ofp.
	sub	$8,%rsp		# Allocate one stack slot for the return value
	mov	$(1 << N_FIXNUM_TAG_BITS),%rcx		# Setup ecx for one return value.
	mov	reg_NULL,%rdi	# default second value
	mov	reg_NULL,%rsi	# default third value
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

#ifdef LISP_FEATURE_SB_SAFEPOINT
	.globl 	GNAME(handle_global_safepoint_violation)
	TYPE(GNAME(handle_global_safepoint_violation))
	.align	align_16byte,0x90
GNAME(handle_global_safepoint_violation):
	TRAP
	.byte 	trap_GlobalSafepoint
	ret
	SIZE(GNAME(handle_global_safepoint_violation))

	.globl 	GNAME(handle_csp_safepoint_violation)
	TYPE(GNAME(handle_csp_safepoint_violation))
	.align	align_16byte,0x90
GNAME(handle_csp_safepoint_violation):
	TRAP
	.byte 	trap_CspSafepoint
	ret
	SIZE(GNAME(handle_csp_safepoint_violation))
#endif /* SB-SAFEPOINT */

	.globl 	GNAME(memory_fault_emulation_trap)
	TYPE(GNAME(memory_fault_emulation_trap))
	.align	align_16byte,0x90
GNAME(memory_fault_emulation_trap):
	TRAP
	.byte 	trap_MemoryFaultEmulation
	SIZE(GNAME(memory_fault_emulation_trap))

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

/* When LISP_FEATURE_C_STACK_IS_CONTROL_STACK, we cannot safely scrub
 * the control stack from C, largely due to not knowing where the
 * active stack frame ends.  On such platforms, we reimplement the
 * core scrubbing logic in assembly, in this case here:
 */
	.text
	.align	align_16byte,0x90
	.globl GNAME(arch_scrub_control_stack)
	TYPE(GNAME(arch_scrub_control_stack))
GNAME(arch_scrub_control_stack):
	/* We are passed three parameters:
	 * A (struct thread *) in RDI,
	 * the address of the guard page in RSI, and
	 * the address of the hard guard page in RDX.
	 * We may trash RAX, RCX, and R8-R11 with impunity.
	 * [RSP] is our return address, [RSP-8] is the first
	 * stack slot to scrub. */

	/* We start by setting up our scrub pointer in RAX, our
	 * guard page upper bound in R8, and our hard guard
	 * page upper bound in R9. */
	lea	-8(%rsp), %rax
#ifdef LISP_FEATURE_WIN32
	LOAD_PIC_VAR(win32_page_size, %r9)
#else
	LOAD_PIC_VAR(os_vm_page_size, %r9)
#endif
	lea	(%rsi,%r9), %r8
	lea	(%rdx,%r9), %r9

	/* Now we begin our main scrub loop. */
ascs_outer_loop:

	/* If we're about to scrub the hard guard page, exit. */
	cmp	%r9, %rax
	jae	ascs_check_guard_page
	cmp	%rax, %rdx
	jbe	ascs_finished

ascs_check_guard_page:
	/* If we're about to scrub the guard page, and the guard
	 * page is protected, exit. */
	cmp	%r8, %rax
	jae	ascs_clear_loop
	cmp	%rax, %rsi
	ja	ascs_clear_loop
	/* test state_word.control_stack_guard_page_protected */
	cmpb	$0, THREAD_STATE_WORD_OFFSET(%rdi)
	jne	ascs_finished

	/* Clear memory backwards to the start of the (4KiB) page */
ascs_clear_loop:
	movq	$0, (%rax)
	test	$0xfff, %rax
	lea	-8(%rax), %rax
	jnz	ascs_clear_loop

	/* If we're about to hit the hard guard page, exit. */
	cmp	%r9, %rax
	jae	ascs_finished

	/* If the next (previous?) 4KiB page contains a non-zero
	 * word, continue scrubbing. */
ascs_check_loop:
	testq	$-1, (%rax)
	jnz	ascs_outer_loop
	test	$0xfff, %rax
	lea	-8(%rax), %rax
	jnz	ascs_check_loop

ascs_finished:
	ret
	SIZE(GNAME(arch_scrub_control_stack))
