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

.known_nil:
	.word	NIL
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
        vstmdb	sp!, {d8-d15}

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
	add	reg_PC, reg_CODE, #SIMPLE_FUN_INSTS_OFFSET

	.align 3
	.equ	.lra, .+OTHER_POINTER_LOWTAG
	.word	RETURN_PC_WIDETAG

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

	@@ Leave PSEUDO-ATOMIC and check for interrupts.
	str     reg_NULL, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_ATOMIC)
	ldr     reg_OCFP, STATIC_SYMBOL_VALUE(PSEUDO_ATOMIC_INTERRUPTED)
        cmp     reg_OCFP, #0
	blxne   reg_OCFP

	@@ Restore saved registers.
        vldmia	sp!, {d8-d15}
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

	@@ Set FFCA, so the runtime knows that we're not "in lisp".
	ldr     reg_NFP, =foreign_function_call_active
	str     pc, [reg_NFP]

        @@ We're done with R0, and we need to use OCFP when leaving
        @@ pseudo-atomic, so move the first of the C register
	@@ arguments to its final resting place now.
        mov     r0, reg_OCFP

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

@@ FIXME: writing this as a lisp assembly routine would eliminate all the
@@ preprocessor noise. There's no reasom it can't be done, but we lack
@@ instruction encoders for: stmfd, ldmfd, stmea, ldmea, fstmfdd, fldfdd.

#define DEFINE_TRAMPOLINE(name, entry) \
	.align ;\
	.global	name ;\
	.type	name, %function ;\
name: \
        stmfd	sp!, {r4, r6, r12, lr} ;\
\
        ldr     r4, =foreign_function_call_active ;\
        str     pc, [r4] ;\
\
        ldr     r4, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER) ;\
        add     r6, r4, #8*4 ;\
        str     r6, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER) ;\
	/*
	@@ The following comment is misleading: GC won't need to see
	@@ anything when we're pseudo-atomic.
        @@ Create a new frame and save descriptor regs on the stack
	@@ for the GC to see. */ \
        str     reg_CFP, [r4, #0] ;\
        str     reg_NULL, [r4, #4] ;\
        str     reg_CODE, [r4, #8] ;\
        add     r4, r4, #3*4 ;\
        stmea   r4, {r0-reg_LEXENV, r8} ;\
\
        ldr     r0,  [sp, #4*4] ;\
        vstmdb  sp!, {d0-d7} ;\
\
        mov     lr, pc ;\
        ldr     pc,=entry ;\
\
        vldmia sp!, {d0-d7} ;\
        str     r0,  [sp, #4*4] ;\
        ldr     r4, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER) ;\
        ldmea   r4, {r0-reg_LEXENV, r8} ;\
        sub     r4, r4, #8*4 ;\
        str     r4, STATIC_SYMBOL_VALUE(CONTROL_STACK_POINTER) ;\
\
        ldr     r4, =foreign_function_call_active ;\
        mov     r6, #0 ;\
	str     r6, [r4] ;\
\
	ldmfd	sp!, {r4, r6, r12, lr} ;\
	bx	lr

#ifdef LISP_FEATURE_GENCGC
DEFINE_TRAMPOLINE(alloc_tramp, alloc)
DEFINE_TRAMPOLINE(list_alloc_tramp, alloc_list)
#endif

        .align
	.global	do_pending_interrupt
	.type	do_pending_interrupt, %function
do_pending_interrupt:
#if defined(LISP_FEATURE_LINUX)
	.word 0xe7f001f0
#elif defined(LISP_FEATURE_NETBSD) || defined(LISP_FEATURE_OPENBSD)
	.word 0xe7ffdefe
#endif
        .byte trap_PendingInterrupt
        .byte 0
        .byte 0
        .byte 0
	bx	lr

#ifdef __ELF__
// Mark the object as not requiring an executable stack.
.section .note.GNU-stack,"",%progbits
#endif
