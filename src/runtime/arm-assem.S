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

	.align
	.global	call_into_lisp
	.type	call_into_lisp, %function
        .fpu vfp
call_into_lisp:
	@@ At this point, we have:
	@@ R0 - function
	@@ R1 - pointer to args
	@@ R2 - number of args (unboxed)
	@@ There will be no more than three args, so we don't need to
	@@ worry about parameters to be passed on the stack.

	@@ All registers other than R0-R3 and R12 are callee-saves.
        @@ Save R3 to get 8-byte alignemnt.
	stmfd	sp!, {r3-r11, lr}
        fstmfdd sp!, {d8-d15}

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

#ifndef LISP_FEATURE_GENCGC
        @@ Copy the current allocation pointer into the symbol.
        ldr     reg_OCFP, =dynamic_space_free_pointer
        ldr     reg_OCFP, [reg_OCFP]
        str     reg_OCFP, STATIC_SYMBOL_VALUE(ALLOCATION_POINTER)
#endif

	@@ Clear FFCA, so the runtime knows that we're "in lisp".
	ldr     reg_OCFP, =foreign_function_call_active
	str     reg_R2, [reg_OCFP]

	@@ We need to set up the lisp stack pointer and the basics of
	@@ our stack frame while we're still in P-A.  Any sooner and
	@@ our stack frame can be clobbered by a stray interrupt, any
	@@ later and we can end up with a half-configured stack frame
	@@ when we catch a stray interrupt.

	@@ Allocate our frame and set up the Lisp stack pointer
        add     reg_OCFP, reg_CFP, #8
        str     reg_OCFP, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)

	@@ Set up the "frame link"
	str     reg_NFP, [reg_OCFP, #-8]

	@@ Set up the return address
	ldr	reg_NL3, =.lra
        str     reg_NL3, [reg_OCFP, #-4]

	@@ Leave PSEUDO-ATOMIC and check for interrupts.
	str     reg_NULL, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)
	ldr     reg_OCFP, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_INTERRUPTED)
	cmp     reg_OCFP, #0
	blxne   reg_OCFP

	@@ Load our function args.  Cleverness abounds!
	rsb	reg_NL3, reg_NARGS, #8
	add	pc, pc, reg_NL3
	ldr	reg_R2, [reg_R8, #8]
	ldr	reg_R1, [reg_R8, #4]
	ldr	reg_R0, [reg_R8]

        @@ Load the closure-fun (or simple-fun-self), in case we're
	@@ trying to call a closure.
        ldr     reg_CODE, [reg_LEXENV, #CLOSURE_FUN_OFFSET]

	@@ And, finally, call into Lisp!
	add	reg_PC, reg_CODE, #SIMPLE_FUN_CODE_OFFSET

	.align 3
	.equ	.lra, .+OTHER_POINTER_LOWTAG
	.word	RETURN_PC_HEADER_WIDETAG

	@@ Correct stack pointer for return processing.
	streq	reg_OCFP, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)

	@@ Enter PSEUDO-ATOMIC.
	str     pc, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)

        @@ Save the lisp stack and frame pointers.
	ldr	reg_NFP, .frame_pointer_address
	str	reg_CFP, [reg_NFP]
        ldr     reg_NFP, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)
	ldr	reg_OCFP, .stack_pointer_address
	str	reg_NFP, [reg_OCFP]

	@@ Set FFCA, so the runtime knows that we're not "in lisp".
	ldr     reg_OCFP, =foreign_function_call_active
	str     pc, [reg_OCFP]

#ifndef LISP_FEATURE_GENCGC
        @@ Copy the current allocation pointer out from the symbol.
        ldr     reg_OCFP, =dynamic_space_free_pointer
        ldr     reg_NFP, STATIC_SYMBOL_VALUE(ALLOCATION_POINTER)
        str     reg_NFP, [reg_OCFP]
#endif

	@@ Leave PSEUDO-ATOMIC and check for interrupts.
	str     reg_NULL, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)
	ldr     reg_OCFP, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_INTERRUPTED)
        cmp     reg_OCFP, #0
	blxne   reg_OCFP

	@@ Restore saved registers.
        fldmfdd sp!, {d8-d15}
	ldmfd	sp!, {r3-r11, lr}
	bx	lr
	.size	call_into_lisp, .-call_into_lisp
.frame_pointer_address:	.word	current_control_frame_pointer
.stack_pointer_address: .word	current_control_stack_pointer


	.align
	.global	call_into_c
	.type	call_into_c, %function
call_into_c:
	@@ At this point, we have:
	@@ R8 -- C function to call.  This routine doesn't support
        @@       thumb interworking, but linkage-table does, so we
        @@       don't have to.
        @@ LR -- Return address within the code component.
        @@ OCFP -- First C register argument.
        @@ NARGS -- Second C register argument.
        @@ NL2 -- Third C register argument.
        @@ NL3 -- Fourth C register argument.
        @@ All other C arguments are already stashed on the C stack.

	@@ We need to convert our return address to a GC-safe format,
	@@ build a stack frame to count for the "foreign" frame,
	@@ switch to C mode, move the register arguments to the
        @@ correct locations, call the C function, move the result to
        @@ the correct location, switch back to Lisp mode, tear down
        @@ our stack frame, restore the return address, and return to
        @@ our caller.

        @@ We have ONE unboxed scratch register: NFP.  Use it as a
	@@ temporary while we convert the (unboxed) return address to
        @@ a (fixnum) offset within the component.
        sub     reg_NFP, reg_LR, reg_CODE
        add     reg_NFP, reg_NFP, #OTHER_POINTER_LOWTAG

        @@ Build a Lisp stack frame.  We need to stash our frame link,
        @@ the code component, and our return offset.  Frame link goes
	@@ in slot 0 (OCFP-SAVE-OFFSET), the offset (a FIXNUM) goes in
        @@ slot 1 (LRA-SAVE-OFFSET), and reg_CODE goes in slot 2.  The
        @@ debugger knows about this layout (see COMPUTE-CALLING-FRAME
        @@ in SYS:SRC;CODE;DEBUG-INT.LISP).  The stack is aligned, so
        @@ we can use R0 (a boxed register) as our temporary.
        ldr     reg_R0, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)
        add     reg_R0, reg_R0, #12
        str     reg_R0, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)
        str     reg_CFP, [reg_R0, #-12]
        str     reg_NFP, [reg_R0, #-8]
        str     reg_CODE, [reg_R0, #-4]

        @@ Enter PSEUDO-ATOMIC.
	str     pc, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)

        @@ Save the lisp stack and frame pointers.
	ldr	reg_NFP, .stack_pointer_address
	str	reg_R0, [reg_NFP]
        sub     reg_R0, reg_R0, #12
	ldr	reg_NFP, .frame_pointer_address
	str	reg_R0, [reg_NFP]

        @@ We're done with R0, and we need to use OCFP when leaving
        @@ pseudo-atomic, so move the first of the C register
	@@ arguments to its final resting place now.
        mov     r0, reg_OCFP

	@@ Set FFCA, so the runtime knows that we're not "in lisp".
	ldr     reg_NFP, =foreign_function_call_active
	str     pc, [reg_NFP]

#ifndef LISP_FEATURE_GENCGC
        @@ Copy the current allocation pointer out from the symbol.
        ldr     reg_OCFP, =dynamic_space_free_pointer
        ldr     reg_NFP, STATIC_SYMBOL_VALUE(ALLOCATION_POINTER)
        str     reg_NFP, [reg_OCFP]
#endif

	@@ Leave PSEUDO-ATOMIC and check for interrupts.
	str     reg_NULL, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)
	ldr     reg_OCFP, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_INTERRUPTED)
        cmp     reg_OCFP, #0
	blxne   reg_OCFP

        @@ Now that we're in C mode, move the remaining register args
	@@ into place.
        mov     r1, reg_NARGS
        mov     r2, reg_NL2
        mov     r3, reg_NL3

        @@ And call the C function.  We don't support interworking
	@@ here because we have to be able to pass the function
	@@ pointer in a boxed register, but the linkage-table is quite
        @@ capable of doing a tail-call to a Thumb routine.
        @@
        @@ R8 is important for undefined_alien_function.
        blx      reg_R8

        @@ We're back.  Our main tasks are to move the C return value
        @@ to where Lisp expects it, and to re-establish the Lisp
        @@ environment.

        @@ Stash the return value into NARGS for Lisp.
        mov     reg_NARGS, r0
        @@ For returning long-long, and doubles with softfp.
        mov     reg_NL3, r1

        @@ Re-establish NIL.
        ldr     reg_NULL, .known_nil

        @@ Blank the boxed registers.
        mov     reg_R0, #0
        mov     reg_R1, #0
        mov     reg_R2, #0
        mov     reg_LEXENV, #0
        mov     reg_R8, #0
        mov     reg_CODE, #0

        @@ Enter PSEUDO-ATOMIC.
	str     pc, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)

	@@ Clear FFCA, so the runtime knows that we're "in lisp".
	ldr     reg_OCFP, =foreign_function_call_active
	str     reg_R2, [reg_OCFP]

#ifndef LISP_FEATURE_GENCGC
        @@ Copy the current allocation pointer into the symbol.
        ldr     reg_OCFP, =dynamic_space_free_pointer
        ldr     reg_OCFP, [reg_OCFP]
        str     reg_OCFP, STATIC_SYMBOL_VALUE(ALLOCATION_POINTER)
#endif

        @@ Restore the Lisp stack and frame pointers, but store the
        @@ control frame pointer in reg_NFP (saving a register move
        @@ later).
	ldr	reg_NFP, .stack_pointer_address
	ldr	reg_CFP, [reg_NFP]
        str     reg_CFP, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)
	ldr	reg_NFP, .frame_pointer_address
	ldr	reg_NFP, [reg_NFP]

	@@ Leave PSEUDO-ATOMIC and check for interrupts.
	str     reg_NULL, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)
	ldr     reg_OCFP, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_INTERRUPTED)
        cmp     reg_OCFP, #0
	blxne   reg_OCFP

        @@ Restore our caller state from our stack frame.
        ldr     reg_CODE, [reg_NFP, #8]
        ldr     reg_NL2, [reg_NFP, #4]
        ldr     reg_CFP, [reg_NFP]
        str     reg_NFP, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)

        @@ Restore our return address... into the program counter.
        sub     reg_NL2, reg_NL2, #OTHER_POINTER_LOWTAG
        add     reg_PC, reg_NL2, reg_CODE

	.size	call_into_c, .-call_into_c


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

        @@ As in ppc-assem.S, point reg_CODE to the header with a
        @@ function lowtag...  Which the address already has.
        ldr     reg_CODE, =undefined_tramp

        @@ The magic (undefined) "BREAK_POINT" instruction.
        .word 0xe7f001f0

        @@ Error arguments for an undefined function.
        .byte   trap_Error
        .byte   .error_args_end - . - 1
        .byte   UNDEFINED_FUN_ERROR
        @@ Need to indicate reg_LEXENV here, which is R3.  Encoding
        @@ rules are to produce an "sc-offset" with the SC number in
        @@ the low six bits and the offset (3 in our case) in the
        @@ high $n$ bits.  sc_DescriptorReg happens to be 5, but we
        @@ should use the constant for it.  So long as the overall
        @@ value of the sc-offset is less than 254, we can use a
        @@ single byte.  Overflowing that will take having the SC
        @@ number being 30 or 31, and as of this writing the highest
        @@ SC number is sc_CatchBlock at 16.  It would also take an
        @@ offset of 7, not the 3 that we use for LEXENV.
        .byte   sc_DescriptorReg + (0x40 * 3)
.error_args_end:

        .align	3
	.global	undefined_alien_function
	.type	undefined_alien_function, %object
	.word	SIMPLE_FUN_HEADER_WIDETAG
	.equ	undefined_alien_function, .+1
	.word	undefined_alien_function
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL
undefined_alien_function:
        ldr     reg_CODE, = undefined_alien_function

        @@ The magic (undefined) "BREAK_POINT" instruction.
        .word 0xe7f001f0

        @@ Error arguments for an undefined function.
        .byte   trap_Error
        .byte   4
        .byte   UNDEFINED_ALIEN_FUN_ERROR
        @@ Encode unsigned R8, which comes from call_into_c
        .byte   0xFE
        .byte   0x11
        .byte   0x02

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

        @@ FIXME-ARM: The following is random garbage, to make
        @@ code/debug-int compile. To get the debugger working, this
        @@ needs to be implemented.
        .align
        .global fun_end_breakpoint_guts
        .type   fun_end_breakpoint_guts, %object
fun_end_breakpoint_guts:
	.global	fun_end_breakpoint_trap
	.type	fun_end_breakpoint_trap, %function
fun_end_breakpoint_trap:
        b       fun_end_breakpoint_trap
        .global fun_end_breakpoint_end
fun_end_breakpoint_end:

#ifdef LISP_FEATURE_GENCGC
	.align
	.global	alloc_tramp
	.type	alloc_tramp, %function
alloc_tramp:
        stmfd	sp!, {r4, r6, r12, lr}

        ldr     r4, =foreign_function_call_active
        str     pc, [r4]

        ldr     r4, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)
        add     r6, r4, #8*4
        str     r6, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)

        @@ Create a new frame and save descriptor regs on the stack
	@@ for the GC to see.
        str     reg_CFP, [r4, #0]
        str     reg_NULL, [r4, #4]
        str     reg_CODE, [r4, #8]
        add     r4, r4, #3*4
        stmea   r4, {r0-reg_LEXENV, r8}

        ldr     r0,  [sp, #4*4]
        fstmfdd sp!, {d0-d7}

        mov     lr, pc
        ldr     pc,=alloc

        fldmfdd sp!, {d0-d7}
        str     r0,  [sp, #4*4]
        ldr     r4, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)
        ldmea   r4, {r0-reg_LEXENV, r8}
        sub     r4, r4, #8*4
        str     r4, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER)

        ldr     r4, =foreign_function_call_active
        mov     r6, #0
	str     r6, [r4]

	ldmfd	sp!, {r4, r6, r12, lr}
	bx	lr

        .align
	.global	fpu_save
	.type	fpu_save, %function
fpu_save:
        fstmiad r0, {d0-d7}
	bx	lr

        .align
	.global	fpu_restore
	.type	fpu_restore, %function
fpu_restore:
        add     r0, r0, #16
        fldmiad r0, {d0-d7}
	bx	lr

        .align
	.global	do_pending_interrupt
	.type	do_pending_interrupt, %function
do_pending_interrupt:
        .word 0xe7f001f0
        .byte trap_PendingInterrupt
        .byte 0
        .byte 0
        .byte 0
	bx	lr
#endif
