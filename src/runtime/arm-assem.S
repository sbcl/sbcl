#define LANGUAGE_ASSEMBLY

#include "lispregs.h"
#include "globals.h"
#include "sbcl.h"

#include "genesis/closure.h"
#include "genesis/funcallable-instance.h"
#include "genesis/fdefn.h"
#include "genesis/static-symbols.h"
#include "genesis/simple-fun.h"
#include "genesis/symbol.h"

#define STATIC_SYMBOL_VALUE(sym) [reg_NULL, #(((sym)-NIL)+SYMBOL_VALUE_OFFSET)]

	.align	2
	.global	call_into_lisp
	.type	call_into_lisp, %function
call_into_lisp:
	@@ At this point, we have:
	@@ R0 - function
	@@ R1 - pointer to args
	@@ R2 - number of args (unboxed)
	@@ There will be no more than three args, so we don't need to
	@@ worry about parameters to be passed on the stack.

	@@ All registers other than R0-R3 and R12 are callee-saves.
	stmfd	sp!, {r4-r11, lr}

	@@ Start by finding NIL.
	ldr	reg_NULL, .known_nil

	@@ Set up NARGS.
	mov	reg_NARGS, r2, lsl #2

	@@ Move args pointer out of the way of the args to be loaded.
	mov	reg_R8, r1

	@@ Move the function to its passing location.
	mov	reg_LEXENV, r0

	@@ Clear the boxed registers that don't already have something
	@@ in them.
	mov	reg_CODE, #0
	mov     reg_R2, #0

	@@ Find the lisp stack and frame pointers.  We're allocating a
	@@ new lisp stack frame, so load the stack pointer into CFP.
	@@ And we need the frame pointer, but OCFP is in use, so use
	@@ NFP instead.
	ldr	reg_NFP, .frame_pointer_address
	ldr	reg_CFP, .stack_pointer_address
	ldr	reg_NFP, [reg_NFP]
	ldr	reg_CFP, [reg_CFP]

	@@ Enter PSEUDO-ATOMIC.
	str     pc, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)

	@@ Clear FFCA, so the runtime knows that we're "in lisp".
	ldr     reg_OCFP, =foreign_function_call_active
	str     reg_R2, [reg_OCFP]

	@@ Save off the C stack pointer (should probably set it as the
	@@ number stack or something).
	stmea	reg_CFP!, {sp}

	@@ We need to set up the lisp stack pointer and the basics of
	@@ our stack frame while we're still in P-A.  Any sooner and
	@@ our stack frame can be clobbered by a stray interrupt, any
	@@ later and we can end up with a half-configured stack frame
	@@ when we catch a stray interrupt.

	@@ Set up the Lisp stack pointer
	mov	reg_CSP, reg_CFP

	@@ Set up the "frame link"
	stmea   reg_CSP!, {reg_NFP}

	@@ Set up the return address
	ldr	reg_NL3, =.lra
	stmea   reg_CSP!, {reg_NL3}

	@@ Leave PSEUDO-ATOMIC and check for interrupts.
	str     reg_NULL, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)
	ldr     reg_OCFP, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_INTERRUPTED)
	cmp     reg_OCFP, reg_NULL
	movlt   reg_OCFP, reg_OCFP, lsl #N_FIXNUM_TAG_BITS
	swilt   #0

	@@ Load our function args.  Cleverness abounds!
	rsb	reg_NL3, reg_NARGS, #8
	add	pc, pc, reg_NL3
	ldr	reg_R2, [reg_R8, #8]
	ldr	reg_R1, [reg_R8, #4]
	ldr	reg_R0, [reg_R8]

	@@ And, finally, call into Lisp!
	add	reg_PC, reg_LEXENV, #SIMPLE_FUN_CODE_OFFSET

	.align 3
	.equ	.lra, .+OTHER_POINTER_LOWTAG
	.word	RETURN_PC_HEADER_WIDETAG

	@@ Correct stack pointer for return processing.
	moveq	reg_CSP, reg_OCFP

	@@ FIXME: Save the various stack and frame pointers here.

	@@ Enter PSEUDO-ATOMIC.
	str     pc, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)

	@@ Set FFCA, so the runtime knows that we're not "in lisp".
	ldr     reg_OCFP, =foreign_function_call_active
	str     pc, [reg_OCFP]

	@@ Restore the C stack pointer.
	ldr	sp, [reg_CSP, #-4]

	@@ Leave PSEUDO-ATOMIC and check for interrupts.
	str     reg_NULL, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)
	ldr     reg_OCFP, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_INTERRUPTED)
	cmp     reg_OCFP, reg_NULL
	movlt   reg_OCFP, reg_OCFP, lsl #N_FIXNUM_TAG_BITS
	swilt   #0

	@@ Restore saved registers.
	ldmfd	sp!, {r4-r11, lr}
	bx	lr
	.size	call_into_lisp, .-call_into_lisp
.frame_pointer_address:	.word	current_control_frame_pointer
.stack_pointer_address: .word	current_control_stack_pointer

/* Trampolines, like on SPARC, use Lisp calling conventions. */
	.align	3
	.global	undefined_tramp
	.type	undefined_tramp, %object
	.word	SIMPLE_FUN_HEADER_WIDETAG
	.equ	undefined_tramp, .+1
	.word	undefined_tramp
.known_nil:
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL

        @@ The BREAK_POINT syscall.
        ldr     r7, =0x000f0001
        swi     #0

        @@ Error arguments for an undefined function.
        .byte   trap_Error
        .byte   .error_args_end - . - 1
        .byte   UNDEFINED_FUN_ERROR
        @@ Need to indicate reg_LEXENV here, which is R3.  Encoding
        @@ rules are to produce an "sc-offset" with the SC number in
        @@ the low five bits and the offset (3 in our case) in the
        @@ high $n$ bits.  sc_DescriptorReg happens to be 5, but we
        @@ should use the constant for it.  So long as the overall
        @@ value of the sc-offset is less than 254, we can use a
        @@ single byte.  Overflowing that will take having the SC
        @@ number being 30 or 31, and as of this writing the highest
        @@ SC number is sc_CatchBlock at 16.  It would also take an
        @@ offset of 7, not the 3 that we use for LEXENV.
        .byte   sc_DescriptorReg + (0x20 * 3)
.error_args_end:

	.align	3
	.global	closure_tramp
	.type	closure_tramp, %object
	.word	SIMPLE_FUN_HEADER_WIDETAG
	.equ	closure_tramp, .+1
	.word	closure_tramp
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL

	ldr	reg_LEXENV, [reg_LEXENV, #FDEFN_FUN_OFFSET]
	ldr	reg_CODE, [reg_LEXENV, #CLOSURE_FUN_OFFSET]
	add	reg_PC, reg_CODE, #SIMPLE_FUN_CODE_OFFSET

	.align	3
	.global	funcallable_instance_tramp
	.type	funcallable_instance_tramp, %object
	.word	SIMPLE_FUN_HEADER_WIDETAG
	.equ	funcallable_instance_tramp, .+1
	.word	funcallable_instance_tramp
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL

	ldr	reg_LEXENV, [reg_LEXENV, #FUNCALLABLE_INSTANCE_FUNCTION_OFFSET]
	ldr	reg_CODE, [reg_LEXENV, #CLOSURE_FUN_OFFSET]
	add	reg_PC, reg_CODE, #SIMPLE_FUN_CODE_OFFSET

	/* EOF */
