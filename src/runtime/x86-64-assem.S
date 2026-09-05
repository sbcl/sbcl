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
#define LOAD_PIC_UINT(x,reg) mov GNAME(x)(%rip), %e##reg
#else
#define LOAD_PIC_VAR(x,dest) mov GNAME(x)@GOTPCREL(%rip), dest ; mov (dest), dest
#define LOAD_PIC_UINT(x,reg) mov GNAME(x)@GOTPCREL(%rip), %r##reg ; mov (%r##reg), %e##reg
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

#define THREAD_SAVED_CSP_OFFSET (- N_WORD_BYTES)

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

/* macros to save and restore the callee-saved regs which are
 * potentially clobbered by Lisp, but which are guaranteed to be
 * preserved across functions calls in accordance with the ABI.
 * I do not understand why this is adequate for Windows calling
 * convention, which additionally specifies RDI and RSI as preserved.
 */
#define PUSH_C_NONVOLATILE_REGS \
 push %rbx ; push %r12 ; push %r13 ; push %r14 ; push %r15
#define POP_C_NONVOLATILE_REGS \
 pop %r15 ; pop %r14 ; pop %r13 ; pop %r12 ; pop %rbx

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
	PUSH_C_NONVOLATILE_REGS
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
	mov	16(%rbx),%rdx	# arg2
Ltwo:	mov	8(%rbx),%rsi	# arg1
Lone:	mov	0(%rbx),%rdi	# arg0
Lzero:
	shl	$(N_FIXNUM_TAG_BITS),%rcx	# (fixnumize num-args)
	
#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
	mov     THREAD_SAVED_CSP_OFFSET(THREAD_BASE_REG), %r8
        push    %r8
        test    %r8, %r8
        je      1f
	movq     $0, THREAD_SAVED_CSP_OFFSET(THREAD_BASE_REG)
1:
#endif

	/* Alloc new frame. */
	push	%rbp            # Dummy for return address
	push	%rbp		# fp in save location S1
	mov	%rsp,%rbp	# The current sp marks start of new frame.
Lcall:
	LOAD_PIC_VAR(gc_card_mark, reg_NULL)
	sub	NIL_CARDTABLE_DISP, reg_NULL
	call	*CLOSURE_FUN_OFFSET(%rax)

#ifndef LISP_FEATURE_TLS_BASED_MV_RETURN
	/* If the function returned multiple values, the carry flag will be set.
           Lose them */
	jnc	LsingleValue
	mov	%rbx, %rsp
LsingleValue:
#endif

#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
        pop    %r8
        test   %r8, %r8
        je     1f
	movq   %r8, THREAD_SAVED_CSP_OFFSET(THREAD_BASE_REG)
1:
#endif

	POP_C_NONVOLATILE_REGS

/* FIXME Restore the NPX state. */

	mov	%rdi,%rax	# c-val
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
   rdi arg0
   rsi arg1
   rdx arg2
   rcx thread #+sb-thread
*/

	push	%rbp		# Save old frame pointer.
	mov	%rsp,%rbp	# Establish new frame.

	PUSH_C_NONVOLATILE_REGS
#ifdef LISP_FEATURE_SB_THREAD
        mov     %rcx, THREAD_BASE_REG
#else
        LOAD_PIC_VAR(all_threads, THREAD_BASE_REG)
#endif
        mov     $(3 << N_FIXNUM_TAG_BITS),%rcx

#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
        push    THREAD_SAVED_CSP_OFFSET(THREAD_BASE_REG)
	movq    $0, THREAD_SAVED_CSP_OFFSET(THREAD_BASE_REG)
#endif

	/* Alloc new frame. */
        push	%rbp            # Dummy for return address
	push	%rbp		# fp in save location S1
        mov	%rsp,%rbp
	LOAD_PIC_VAR(gc_card_mark, reg_NULL)
	sub	NIL_CARDTABLE_DISP, reg_NULL
	LOAD_PIC_VAR(linkage_space, %rax)
	call	*(8*ENTER_ALIEN_CALLBACK_fname_index)(%rax)

#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
	pop   THREAD_SAVED_CSP_OFFSET(THREAD_BASE_REG)
#endif
	POP_C_NONVOLATILE_REGS

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
	mov	reg_NULL,%rsi	# default second value
	mov	reg_NULL,%rdx	# default third value
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
         * guard size is in RCX
	 * We may trash RAX, and R8-R11 with impunity.
	 * [RSP] is our return address, [RSP-8] is the first
	 * stack slot to scrub. */

	/* Select guard boundary based on page protection status */
	cmpb	$0, THREAD_STATE_WORD_OFFSET(%rdi) // see genesis/thread.h
	cmovne	%rsi, %rdx              // if protected, stop at soft guard
	lea	64(%rdx, %rcx), %r9       // R9 = guard page upper bound, 64 byte safety margin

	LOAD_PIC_UINT(aggressive_scrub_control_stack, ax)
	test	%eax, %eax
	/* if this branch gets taken, then rdi becomes exactly r9
	 * which is exactly where to start clearing the entire stack */
	jnz	.Lhit_bottom

	/* tentative_bottom = rsp but cache-line aligned. RSP is 16-byte-aligned coming in,
	 * but want to ensure that all 4 loads (the movdqa and por) are in one L1 cache line. */
	/* We may want to start one 4k page down from RSP, ensuring that at least that
	 * much is scrubbed. Doing so would replicate the older behavior of the quasi-broken
	 * scrub routine that scrubbed that much and then stopped */
	mov	%rsp, %rdi
	and	$-64, %rdi
	cmp	%r9, %rdi
	jbe	.Lhit_bottom

	LOAD_PIC_UINT(cpuid_fn1_ecx, ax)
	test	$(1 << 19), %eax
	jnz	.Lprobe_sse

	/* This loop differs from its previous incarnation in several ways:
	 * 1. We're no longer interleaving the looking-for-stopping-point and zeroing.
	 *    Instead we look first, then blast zeros over the whole dirty range.
	 *    The only possible downside is that there is a tiny chance the OS could
	 *    evict a certain page between looking at it and clearing it. Not our problem!
	 * 2. We were wasting bus cycles looking at every byte just to write over it.
	 *    Sampling is good enough and it saves some memory bandwidth
	 *    which is generally better citizenship in a shared compute environment.
	 * 3. There's no reason to operate on 4k at a time. That granularity
	 *    has nothing to do with anything about how the stack is used. */

.Lprobe: // slower way
	/* Check 8 quadwords (64 bytes) below RDI */
	movq	-64(%rdi), %rax
	orq	-56(%rdi), %rax
	orq	-48(%rdi), %rax
	orq	-40(%rdi), %rax
	orq	-32(%rdi), %rax
	orq	-24(%rdi), %rax
	orq	-16(%rdi), %rax
	orq	-8(%rdi), %rax
	jz	.Lfound_clear
	sub	$1024, %rdi             // Step arbitrarily -128 words (1KiB)
	cmp	%r9, %rdi
	ja	.Lprobe
	jmp	.Lhit_bottom

.Lprobe_sse: // faster way
	movdqa	-64(%rdi), %xmm0
	por	-48(%rdi), %xmm0
	por	-32(%rdi), %xmm0
	por	-16(%rdi), %xmm0
	ptest	%xmm0, %xmm0            // Sets ZF=1 if all 64 bytes are zero
	jz	.Lfound_clear
	sub	$1024, %rdi             // Step arbitrarily -128 words (1KiB)
	cmp	%r9, %rdi
	ja	.Lprobe_sse

.Lhit_bottom:
	/* %rdi landed somewhere at or below the cutoff, so restore it to the exact
	 * address of the top of the chosen guard page. We initially added 64 to %r9
	 * as a buffer so that the movdqa could safely look 64 bytes below without
	 * stepping into the guard page */
	lea	-64(%r9), %rdi

.Lfound_clear: // Below RDI is fully clear (we assume) so clear from it up through RSP
	mov	%rsp, %rcx
	sub	%rdi, %rcx              // RCX = (RSP - RDI)
	jbe	.Ldone
	shrq	$3, %rcx                // RCX = word count
	xor	%eax, %eax
	/* Two factors support unconditional use of stosq versus a memset call:
	 * 1. Lisp codegen uses it in the SPLAT vops so we've already decided it's fine.
	 * 2. Whether or not the CPU has EMRS, stosq runs as fast as or faster than individual
	 *    stores. If present, ERMS has the added benefit of effectively doing stores
	 *    that are equal in bit width to the widest FPR (YMM or ZMM). */
	rep stosq
.Ldone:
	ret
	SIZE(GNAME(arch_scrub_control_stack))
