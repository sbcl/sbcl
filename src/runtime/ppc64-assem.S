#include "genesis/sbcl.h"
#include "lispregs.h"
#include "genesis/closure.h"
#include "genesis/static-symbols.h"
#include "genesis/thread.h"

#define FUNCDEF(x)	.text ; \
			.align 3 ; \
			.type x,@function ; \
x:

#define GFUNCDEF(x)	.globl x ; \
	FUNCDEF(x)
#define SET_SIZE(x) .size x,.-x

/*

From PPC-elf64abi:
The following figure shows the stack frame organization.
SP in the figure denotes the stack pointer (general purpose register r1) of the called
function after it has executed code establishing its stack frame.

High Address

          +-> Back chain
          |   Floating point register save area
          |   General register save area
          |   Alignment padding
          |   Vector register save area (quadword aligned)
          |   Local variable space
          |   Parameter save area    (SP + 32)
          |   TOC save doubleword    (SP + 24)
          |   LR save doubleword     (SP + 16)
          |   reserved word
          |   CR save word           (SP + 8)
SP  --->  +-- Back chain             (SP + 0)

Low Address

*/

/* "Floating-point register fN is saved in the doubleword located 8x(32-N)
    bytes before the back-chain word of the previous frame.
    General-purpose register rN is saved in the doubleword located 8x(32-N)
    bytes below the floating-point save area."

    While not entirely obvious from that description, it seems conventional
    to spill the nonvolatile registers such that the higher the register number
    the higher the address in which it is stored.
    Additionally, it seems that gcc spills the floating-pointer registers
    before it lowers the stack pointer. This is permissible because there is
    a red zone of 512 bytes, of which leaf functions are allowed to access
    288 bytes below the stack pointer, the rest being reserved for system use.
    It also spills r31 before it lowers the stack pointer, which allows
    moving r1 into r31 as recommented in the ABI for functions which require
    a frame pointer.
    It looks like the size of the user-accessible part of the red zone was
    exactly calculated to allow spilling every nonvolatile GPR and FPR (18 of each)
    without lowering the stack pointer (36 doublewords = 288 bytes).
*/

// We don't need a frame pointer since the frame is fixed in size.
// Access the spill/restore areas off the stack pointer.
#define sp 1

// 18 nonvolatile GPRs (14 through 31)
// 18 nonvolatile FPRs (14 through 31)
//  4 mandatory doublewords
#define FRAME_SIZE ((18+18+4)*8)
#define SAVE_FPR(n)     stfd n, (FRAME_SIZE+8*(n-32))(sp)
#define RESTORE_FPR(n)  lfd n,  (FRAME_SIZE+8*(n-32))(sp)
#define SAVE_GPR(n)     std n,  (FRAME_SIZE+8*(n-32-18))(sp)
#define RESTORE_GPR(n)  ld n,   (FRAME_SIZE+8*(n-32-18))(sp)

// Note that CR and LR are saved/restored in the frame of the *CALLER*
// But based on absence of language similar to
//   "The caller frame {CR Save Word | LR Save Doubleword} may be used as the save location"
// in the description of the TOC save doubleword, I infer that you may NOT
// use the caller's frame for this purpose.
// Examining some C compiler output for V2 of the ABI shows that it
// saves the TOC register in the new frame, not the old.

#define C_FULL_PROLOG \
	mfcr 0 ; stw 0,  8(sp)   /* save CR */ ; \
	mflr 0 ; std 0, 16(sp)   /* save LR */ ; \
	stdu sp, -FRAME_SIZE(sp) /* Update SP and back chain atomically */; \
	std 2, 24(sp)            /* save TOC ptr  */ ; \
	SAVE_FPR(31) ; SAVE_FPR(30) ; SAVE_FPR(29) ; SAVE_FPR(28) ; \
	SAVE_FPR(27) ; SAVE_FPR(26) ; SAVE_FPR(25) ; SAVE_FPR(24) ; \
	SAVE_FPR(23) ; SAVE_FPR(22) ; SAVE_FPR(21) ; SAVE_FPR(20) ; \
	SAVE_FPR(19) ; SAVE_FPR(18) ; SAVE_FPR(17) ; SAVE_FPR(16) ; \
	SAVE_FPR(15) ; SAVE_FPR(14) ; \
	SAVE_GPR(31) ; SAVE_GPR(30) ; SAVE_GPR(29) ; SAVE_GPR(28) ; \
	SAVE_GPR(27) ; SAVE_GPR(26) ; SAVE_GPR(25) ; SAVE_GPR(24) ; \
	SAVE_GPR(23) ; SAVE_GPR(22) ; SAVE_GPR(21) ; SAVE_GPR(20) ; \
	SAVE_GPR(19) ; SAVE_GPR(18) ; SAVE_GPR(17) ; SAVE_GPR(16) ; \
	SAVE_GPR(15) ; SAVE_GPR(14)

#define C_FULL_EPILOG \
	RESTORE_GPR(14) ; RESTORE_GPR(15) ; \
	RESTORE_GPR(16) ; RESTORE_GPR(17) ; RESTORE_GPR(18) ; RESTORE_GPR(19) ; \
	RESTORE_GPR(20) ; RESTORE_GPR(21) ; RESTORE_GPR(22) ; RESTORE_GPR(23) ; \
	RESTORE_GPR(24) ; RESTORE_GPR(25) ; RESTORE_GPR(26) ; RESTORE_GPR(27) ; \
	RESTORE_GPR(28) ; RESTORE_GPR(29) ; RESTORE_GPR(30) ; RESTORE_GPR(31) ; \
	RESTORE_FPR(14) ; RESTORE_FPR(15) ; \
	RESTORE_FPR(16) ; RESTORE_FPR(17) ; RESTORE_FPR(18) ; RESTORE_FPR(19) ; \
	RESTORE_FPR(20) ; RESTORE_FPR(21) ; RESTORE_FPR(22) ; RESTORE_FPR(23) ; \
	RESTORE_FPR(24) ; RESTORE_FPR(25) ; RESTORE_FPR(26) ; RESTORE_FPR(27) ; \
	RESTORE_FPR(28) ; RESTORE_FPR(29) ; RESTORE_FPR(30) ; RESTORE_FPR(31) ; \
	addi sp, sp, FRAME_SIZE     /* Restore SP */ ; \
	ld 0, 16(sp) ; mtlr 0       /* Restore LR  */ ; \
	lwz 0, 8(sp) ; mtcr 0       /* Restore CR */

	.text

#define BEGIN_PSEUDO_ATOMIC stb reg_NULL,THREAD_PSEUDO_ATOMIC_BITS_OFFSET(reg_THREAD)
#define END_PSEUDO_ATOMIC \
	stb reg_THREAD, THREAD_PSEUDO_ATOMIC_BITS_OFFSET(reg_THREAD) ; \
	lhz reg_NL3, (2+THREAD_PSEUDO_ATOMIC_BITS_OFFSET)(reg_THREAD) ; \
	twnei reg_NL3, 0

/*
 * Function to transfer control into lisp.  The lisp object to invoke is
 * passed as the first argument, which puts it in NL0
 */

	GFUNCDEF(call_into_lisp)
	C_FULL_PROLOG
	/* NL0 - function, NL1 - frame pointer, NL2 - nargs. */
	// Copy ABI TLS value of current_thread into lisp thread base
	addis reg_THREAD,13,current_thread@tprel@ha
	addi reg_THREAD,reg_THREAD,current_thread@tprel@l
	ld reg_THREAD,0(reg_THREAD)
	/* Initialize tagged registers */
	li reg_ZERO,0
	li reg_CODE,0
	li reg_LEXENV,0
	li reg_FDEFN,0
	li reg_OCFP,0
	li reg_LRA,0
	li reg_A0,0
	li reg_A1,0
	li reg_A2,0
	li reg_A3,0
	li reg_L0,0
	li reg_L1,0
	li reg_LIP,0
	lis reg_NULL,NIL@h
	ori reg_NULL,reg_NULL,NIL@l

	/* Turn on pseudo-atomic */
	BEGIN_PSEUDO_ATOMIC
	std reg_ZERO,THREAD_FFCALL_ACTIVE_P_OFFSET(reg_THREAD)
	ld reg_BSP,THREAD_BINDING_STACK_POINTER_OFFSET(reg_THREAD)
	ld reg_CSP,THREAD_CONTROL_STACK_POINTER_OFFSET(reg_THREAD)
	ld reg_OCFP,THREAD_CONTROL_FRAME_POINTER_OFFSET(reg_THREAD)

	/* No longer atomic, and check for interrupt */
	END_PSEUDO_ATOMIC

	/* Pass in the arguments */

	mr reg_CFP,reg_NL1
	mr reg_LEXENV,reg_NL0
	ld reg_A0,0(reg_CFP)
	ld reg_A1,8(reg_CFP)
	ld reg_A2,16(reg_CFP)
	ld reg_A3,24(reg_CFP)

        /* load card table base */
        ld reg_CARDTABLE, THREAD_CARD_TABLE_OFFSET(reg_THREAD)

	/* Calculate LRA */
	lis reg_LRA,lra@h
	ori reg_LRA,reg_LRA,lra@l
	addi reg_LRA,reg_LRA,OTHER_POINTER_LOWTAG

	/* Function is an indirect closure */
	addi reg_NL0,reg_LEXENV,SIMPLE_FUN_SELF_OFFSET
	ld reg_CODE,0(reg_NL0)
	addi reg_LIP,reg_CODE,0
	mtctr reg_LIP
	sldi reg_NARGS, reg_NL2, N_FIXNUM_TAG_BITS
	bctr

	.align 3
lra:
	.quad RETURN_PC_WIDETAG

	/* Blow off any extra values. */
	mr reg_CSP,reg_OCFP
	nop

	/* Return the one value. */
	mr REG(3),reg_A0

	/* Turn on  pseudo-atomic */
	BEGIN_PSEUDO_ATOMIC

	/* Lisp does not understand the TOC register, so we have to restore it
	   now - prior to the epilogue - because there are memory accesses
	   by way of the TOC register in this code, in particular
	   the load() and store() macros uses register 2 as the base */
	ld 2, (24)(sp)

	/* Store lisp state */
	std reg_BSP,THREAD_BINDING_STACK_POINTER_OFFSET(reg_THREAD)
	std reg_CSP,THREAD_CONTROL_STACK_POINTER_OFFSET(reg_THREAD)
	std reg_CFP,THREAD_CONTROL_FRAME_POINTER_OFFSET(reg_THREAD)

	/* No longer in Lisp. */
	std reg_THREAD,THREAD_FFCALL_ACTIVE_P_OFFSET(reg_THREAD)

	/* Check for interrupt */
	END_PSEUDO_ATOMIC

	/* Back to C */
	C_FULL_EPILOG
	blr
	SET_SIZE(call_into_lisp)


	GFUNCDEF(call_into_c)
	/* There is very much a chicken-and-egg problem here.
	 * This code knows how to reference C globals via r2,
	 * but how can we load r2 ? */

	/* We're kind of low on unboxed, non-dedicated registers here:
	most of the unboxed registers may have outgoing C args in them.
	CFUNC is going to have to go in the CTR in a moment, anyway
	so we'll free it up soon.  reg_NFP is preserved by lisp if it
	has a meaningful value in it, so we can use it.  reg_NARGS is
	free when it's not holding a copy of the "real" reg_NL3, which
	gets tied up by the pseudo-atomic mechanism */
	mtctr reg_CFUNC
	mflr reg_LIP
	/* Build a lisp stack frame */
	mr reg_OCFP,reg_CFP
	mr reg_CFP,reg_CSP
	la reg_CSP,32(reg_CSP)
	std reg_OCFP,0(reg_CFP)
	std reg_CODE,16(reg_CFP)
	/* The pseudo-atomic mechanism wants to use reg_NL3, but that
	may be an outgoing C argument.  Copy reg_NL3 to something that's
	unboxed and -not- one of the C argument registers */
	mr reg_NARGS,reg_NL3

	/* Turn on pseudo-atomic */
	BEGIN_PSEUDO_ATOMIC

	/* Convert the return address to an offset and save it on the stack. */
	sub reg_NFP,reg_LIP,reg_CODE
#if 	N_FIXNUM_TAG_BITS == 3
	sldi reg_NFP,reg_NFP,1
#endif
	std reg_NFP,8(reg_CFP)

	/* Store Lisp state */
	std reg_BSP,THREAD_BINDING_STACK_POINTER_OFFSET(reg_THREAD)
	std reg_CSP,THREAD_CONTROL_STACK_POINTER_OFFSET(reg_THREAD)
	std reg_CFP,THREAD_CONTROL_FRAME_POINTER_OFFSET(reg_THREAD)

	/* No longer in Lisp. */
	std reg_CSP,THREAD_FFCALL_ACTIVE_P_OFFSET(reg_THREAD)
	/* Disable pseudo-atomic; check pending interrupt */
	END_PSEUDO_ATOMIC

	mr reg_NL3,reg_NARGS

	/* "If a function changes the value of the TOC pointer register,
	* it shall first save it in the TOC pointer doubleword." */
	std reg_TOC, 24(1)


	/* "When a function is entered through its global entry point,
	 * register r12 contains the entry-point address." */
#ifdef LISP_FEATURE_BIG_ENDIAN
	mfctr 11
	ld reg_CFUNC, 0(11)
	/* In the v1 64-bit ABI, a function pointer is a pointer to a
	 * 3-word "function descriptor" the first word of which contains the
	 * entry address, and the second the value that the callee needs
	 * in the TOC register. */
	ld reg_TOC, 8(11)
	/* The third function descriptor word can allegedly be ignored for C.
	 * Actually not all descriptors have 3 words afaict.
	 * I don't see how they are ABI-compliant */
	// ld 11, 16(11)
	mtctr reg_CFUNC
#else
	mfctr reg_CFUNC
#endif
        /* Into C we go. */
	bctrl

	/* Re-establish NIL */
	lis reg_NULL,NIL@h
	ori reg_NULL,reg_NULL,NIL@l
	/* And reg_ZERO */
	li reg_ZERO,0
	/* And the TOC register (which is not used in lisp) */
	ld reg_TOC, 24(1)

	/* If we GC'ed during the FF code (as the result of a callback ?)
	the tagged lisp registers may now contain garbage (since the
	registers were saved by C and not seen by the GC.)  Put something
	harmless in all such registers before allowing an interrupt */
        li reg_FDEFN,0
	li reg_CODE,0
	li reg_LEXENV,0
	/* reg_OCFP was pointing to a control stack frame & was preserved by C */
	li reg_LRA,0
	li reg_A0,0
	li reg_A1,0
	li reg_A2,0
	li reg_A3,0
	li reg_L0,0
	li reg_L1,0
	li reg_LIP,0

	/* Atomic ... */
	BEGIN_PSEUDO_ATOMIC

	/* No longer in foreign function call. */
	std reg_ZERO,THREAD_FFCALL_ACTIVE_P_OFFSET(reg_THREAD)

	/* The binding stack pointer isn't preserved by C. */
	ld reg_BSP,THREAD_BINDING_STACK_POINTER_OFFSET(reg_THREAD)

	/* Other lisp stack/frame pointers were preserved by C.
	I can't imagine why they'd have moved */

	/* Get the return address back. */
	ld reg_LIP,8(reg_CFP)
#if 	N_FIXNUM_TAG_BITS == 3
	srdi reg_LIP,reg_LIP,1
#endif
	ld reg_CODE,16(reg_CFP)
	add reg_LIP,reg_CODE,reg_LIP

        /* Debugger expects LR to be valid when we come out of PA */
	mtlr reg_LIP

	/* No longer atomic */
	END_PSEUDO_ATOMIC

	/* Reset the lisp stack. */
	mr reg_CSP,reg_CFP
	mr reg_CFP,reg_OCFP

	/* And back into Lisp. */
	blr

	SET_SIZE(call_into_c)

	/* The fun_end_breakpoint support here is considered by the
	authors of the other $ARCH-assem.S files to be magic, and it
	is.  It is a small fragment of code that is copied into a heap
	code-object when needed, and contains an LRA object, code to
	convert a single-value return to unknown-values format, and a
	trap_FunEndBreakpoint. */
	GFUNCDEF(fun_end_breakpoint_guts)
	.globl fun_end_breakpoint_trap
	.globl fun_end_breakpoint_end

	/* We are receiving unknown multiple values, thus must deal
	with the single-value and multiple-value cases separately. */
	b fun_end_breakpoint_multiple_values
	nop

	/* Multiple values are stored relative to reg_OCFP, which we
	set to be the current top-of-stack. */
	mr reg_OCFP, reg_CSP

	/* Reserve a save location for the one value we have. */
	addi reg_CSP, reg_CSP, 4

	/* Record the number of values we have as a FIXNUM. */
	li reg_NARGS, 1<<N_FIXNUM_TAG_BITS

	/* Blank the remaining arg-passing registers. */
	mr reg_A1, reg_NULL
	mr reg_A2, reg_NULL
	mr reg_A3, reg_NULL

fun_end_breakpoint_multiple_values:

	/* The actual magic trap. */
fun_end_breakpoint_trap:
        tdlgti  reg_NULL, trap_FunEndBreakpoint

	/* Finally, the debugger needs to know where the end of the
	fun_end_breakpoint_guts are, so that it may calculate its size
	in order to populate out a suitably-sized code object. */
fun_end_breakpoint_end:
	SET_SIZE(fun_end_breakpoint_guts)


	GFUNCDEF(ppc_flush_cache_line)
	dcbf 0,REG(3)
	sync
	icbi 0,REG(3)
	sync
	isync
	blr
	SET_SIZE(ppc_flush_cache_line)

        GFUNCDEF(do_pending_interrupt)
	trap
	blr
/* King Nato's branch has a nop here. Do we need this? */
	SET_SIZE(do_pending_interrupt)

#ifdef __ELF__
// Mark the object as not requiring an executable stack.
.section .note.GNU-stack,"",%progbits
#endif
