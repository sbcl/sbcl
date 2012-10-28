#define LANGUAGE_ASSEMBLY

#include "lispregs.h"
#include "globals.h"
#include "sbcl.h"

#include "genesis/closure.h"
#include "genesis/funcallable-instance.h"
#include "genesis/fdefn.h"
#include "genesis/static-symbols.h"
#include "genesis/simple-fun.h"

	.align	2
	.global	call_into_lisp
	.type	call_into_lisp, %function
call_into_lisp:
	@@ At this point, we have:
	@@ R0 - function
	@@ R1 - pointer to args
	@@ R2 - number of args (unboxed)
	@@ There will be no more than three args.

	@@ All registers other than R0-R3 and R12 are callee-saves.
	stmfd	sp!, {r4-r11, lr}

	@@ Start by finding NIL.
	ldr	reg_NULL, .known_nil

	@@ Find the lisp stack and frame pointers.
	@@ As a minor cleverness, load the pointers into registers
	@@ from which they will be used.
	ldr	reg_OCFP, .frame_pointer_address
	ldr	reg_CFP, .stack_pointer_address
	ldr	reg_OCFP, [reg_OCFP]
	ldr	reg_CFP, [reg_CFP]

	@@ FIXME: Should enter PSEUDO-ATOMIC and clear FFCA here.

	@@ Save off the C stack pointer (should probably set it as the
	@@ number stack or something).
	stmfa	reg_CFP!, {sp}

	@@ Move the function to its passing location.
	mov	reg_LEXENV, r0

	@@ Set up NARGS.
	mov	reg_NARGS, r2, lsl #2

	@@ Move args pointer out of the way of the args to be loaded.
	mov	reg_R8, r1

	@@ Clear boxed registers otherwise uninitialized.
	mov	reg_CODE, #0
	mov	reg_R2, #0
	mov	reg_R1, #0
	mov	reg_R0, #0

	@@ Load our function args.  Cleverness abounds!
	rsb	reg_LIP, reg_NARGS, #8
	add	pc, pc, reg_LIP
	ldr	reg_R2, [reg_R8, #8]
	ldr	reg_R1, [reg_R8, #4]
	ldr	reg_R0, [reg_R8]

	@@ Set up the Lisp stack pointer
	mov	reg_CSP, reg_CFP

	@@ Set up the "frame link"
	stmea   reg_CSP!, {reg_OCFP}

	@@ Set up the return address
	ldr	reg_LRA, =.lra

	@@ And, finally, call into Lisp!
	add	reg_PC, reg_LEXENV, #SIMPLE_FUN_CODE_OFFSET

	.align 3
	.equ	.lra, .+OTHER_POINTER_LOWTAG
	.word	RETURN_PC_HEADER_WIDETAG

	@@ Correct stack pointer for return processing.
	@@ FIXME: May want to be conditional.
	moveq	reg_CSP, reg_OCFP
	
	@@ FIXME: Should enter PSEUDO-ATOMIC and set FFCA here.

	@@ Restore the C stack pointer.
	ldr	sp, [reg_CSP]

	@@ FIXME: Save the various stack and frame pointers here.

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
