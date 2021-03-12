#include "globals.h"
#include "lispregs.h"
#include "sbcl.h"
#include "validate.h"

#include "genesis/closure.h"
#include "genesis/funcallable-instance.h"
#include "genesis/fdefn.h"
#include "genesis/static-symbols.h"
#include "genesis/simple-fun.h"
#include "genesis/symbol.h"
#ifdef LISP_FEATURE_SB_THREAD
#include "genesis/thread.h"

// For LDP/STP to work right
#if THREAD_CONTROL_STACK_POINTER_OFFSET - THREAD_CONTROL_FRAME_POINTER_OFFSET != 8
#error "THREAD_CONTROL_FRAME_POINTER_OFFSET - THREAD_CONTROL_STACK_POINTER_OFFSET not sequential"
#endif

#endif

#ifdef LISP_FEATURE_DARWIN
#define GNAME(var) _##var
#else
#define GNAME(var) var
#endif

#ifdef LISP_FEATURE_DARWIN
#define TYPE(name)
#define SIZE(name)
#else
#define TYPE(name) .type name,%function
#define SIZE(name) .size name,.-name
#endif

#ifdef LISP_FEATURE_SB_THREAD
.macro ENTER_PA
        str reg_wNULL,[reg_THREAD,THREAD_PSEUDO_ATOMIC_BITS_OFFSET]
.endm

.macro LEAVE_PA
        str wzr,[reg_THREAD,THREAD_PSEUDO_ATOMIC_BITS_OFFSET]
        ldr reg_wTMP,[reg_THREAD,THREAD_PSEUDO_ATOMIC_BITS_OFFSET+4]
        cbz reg_wTMP,1f
	brk trap_PendingInterrupt
1:
.endm
#else
#define LOAD_STATIC_SYMBOL_VALUE(value,sym) \
        mov reg_TMP,((sym)-NIL+SYMBOL_VALUE_OFFSET) ;\
        ldr value,[reg_NULL, reg_TMP]

#define STORE_STATIC_SYMBOL_VALUE(value,sym) \
        mov reg_TMP,((sym)-NIL+SYMBOL_VALUE_OFFSET) ;\
        str value,[reg_NULL, reg_TMP]
#define ENTER_PA \
        STORE_STATIC_SYMBOL_VALUE(reg_CFP,PSEUDO_ATOMIC_ATOMIC)

#define LEAVE_PA \
        STORE_STATIC_SYMBOL_VALUE(reg_NULL,PSEUDO_ATOMIC_ATOMIC) ;\
        LOAD_STATIC_SYMBOL_VALUE(reg_TMP,PSEUDO_ATOMIC_INTERRUPTED) ;\
        cbz reg_TMP, 1f     ; \
	brk trap_PendingInterrupt         ; \
1:
#endif
#ifdef LISP_FEATURE_SB_SAFEPOINT
/* the CSP page sits right before the thread */
# define THREAD_SAVED_CSP_OFFSET (-N_WORD_BYTES)
#endif


#ifdef LISP_FEATURE_OS_THREAD_STACK
	.align 2
	.global GNAME(funcall1_switching_stack)
	TYPE(funcall1_switching_stack)
GNAME(funcall1_switching_stack):
        /* The arguments are switched, funcall1_switching_stack(arg, function)
           to avoid shuffling registers
        */
	/* save FP and LR to old stack */
	stp	x29, x30, [sp, #-16]!
	mov	x29, sp
	/* switch to new stack */
	ldr	x9, [x0, #THREAD_ALIEN_STACK_START_OFFSET]
	add	x9, x9, #ALIEN_STACK_SIZE >> 12, lsl #12
#if ALIEN_STACK_SIZE % 0x1000 != 0
	add	x9, x9, #ALIEN_STACK_SIZE & 0xfff
#endif
	sub	sp, x9, #16
	/* call function */
	blr	x1
	/* restore old SP, FP, LR and return */
	add	sp, x29, #16
	ldp	x29, x30, [x29]
	ret
	SIZE(funcall1_switching_stack)
#endif


	.align 2
	.global	GNAME(call_into_lisp)
	TYPE(call_into_lisp)
GNAME(call_into_lisp):
	// At this point, we have:
	// X0 - function
	// X1 - pointer to args
	// X2 - number of args (unboxed)
	// There will be no more than three args, so we don't need to
	// worry about parameters to be passed on the stack.

	// X19-X28 are callee-saved registers.

        stp     x19,x20, [sp,#-160]!
        stp     x21,x22, [sp,#16]
        stp     x23,x24, [sp,#32]
        stp     x25,x26, [sp,#48]
        stp     x27,x28, [sp,#64]
        stp     x29,x30, [sp,#80] // save the return address in x30 aka LR

        stp     d8,d9, [sp,#96]
        stp     d10,d11, [sp,#112]
        stp     d12,d13, [sp,#128]
        stp     d14,d15, [sp,#144]

	// Start by finding NIL.
	ldr	reg_NULL, =NIL

	// Set up NARGS.
	lsl	reg_NARGS, x2, #N_FIXNUM_TAG_BITS

	// Move args pointer out of the way of the args to be loaded.
	mov	reg_R9, x1

	// Move the function to its passing location.
	mov	reg_LEXENV, x0

#ifdef LISP_FEATURE_SB_THREAD
#ifdef LISP_FEATURE_GCC_TLS
	adrp	x0, :gottprel:current_thread
	ldr	x0, [x0, #:gottprel_lo12:current_thread]
	mrs	reg_THREAD, tpidr_el0
	ldr	reg_THREAD, [reg_THREAD,x0]
#elif defined(LISP_FEATURE_DARWIN)
        adrp    x0, _specials@GOTPAGE
        ldr     x0, [x0, _specials@GOTPAGEOFF]
        ldr     x0, [x0]
        bl      GNAME(pthread_getspecific)
        mov     reg_THREAD, x0
#else
        ldr	x0, specials
        bl      pthread_getspecific
        mov     reg_THREAD, x0
#endif
#else
        mov     reg_R10, #0
#endif
	// Clear the boxed registers that don't already have something
	// in them.
        mov     reg_R0, #0
        mov     reg_R1, #0
        mov     reg_R2, #0
        mov     reg_R3, #0
        mov     reg_R4, #0
        mov     reg_R5, #0
        mov     reg_R6, #0
        mov     reg_R7, #0
#ifndef LISP_FEATURE_DARWIN
        mov     reg_R8, #0
#endif

        mov     reg_CODE, #0

	// Find the lisp stack and frame pointers.  We're allocating a
	// new lisp stack frame, so load the stack pointer into CFP.
#ifdef LISP_FEATURE_SB_THREAD
	ldp     reg_OCFP,reg_CFP, [reg_THREAD, THREAD_CONTROL_FRAME_POINTER_OFFSET]
#else
	ldr	reg_OCFP, =GNAME(current_control_frame_pointer)
	ldr	reg_CFP, =GNAME(current_control_stack_pointer)
	ldr	reg_OCFP, [reg_OCFP]
	ldr	reg_CFP, [reg_CFP]
#endif
        ENTER_PA

	// Clear FFCA, so the runtime knows that we're "in lisp".
#ifdef LISP_FEATURE_SB_THREAD
        str     xzr,[reg_THREAD, THREAD_FOREIGN_FUNCTION_CALL_ACTIVE_OFFSET]
#else
	ldr     reg_NL3, =GNAME(foreign_function_call_active)
	str     xzr, [reg_NL3]
#endif
	// We need to set up the lisp stack pointer and the basics of
	// our stack frame while we're still in P-A.  Any sooner and
	// our stack frame can be clobbered by a stray interrupt, any
	// later and we can end up with a half-configured stack frame
	// when we catch a stray interrupt.

	// Allocate our frame and set up the Lisp stack pointer
        add     reg_CSP, reg_CFP, #16

	// Set up the "frame link"
	str     reg_OCFP, [reg_CFP]

	// Set up the return address
	adr	reg_NL3, .lra
        str     reg_NL3, [reg_CFP, #8]

	LEAVE_PA

	// Load our function args.
	cbz reg_NARGS, no_args
        cmp reg_NARGS, #2
        beq two_args
        bmi one_arg
three_args:
	ldr	reg_R2, [reg_R9, #16]
two_args:
	ldr	reg_R1, [reg_R9, #8]
one_arg:
	ldr	reg_R0, [reg_R9]
no_args:

        // Load the closure-fun (or simple-fun-self), in case we're
	// trying to call a closure.
        ldr     reg_CODE, [reg_LEXENV, #CLOSURE_FUN_OFFSET]

	// And, finally, call into Lisp!
	add	reg_LR, reg_CODE, #SIMPLE_FUN_INSTS_OFFSET
        br      reg_LR

	.align 4
	.equ	.lra, .+OTHER_POINTER_LOWTAG
	.xword	RETURN_PC_WIDETAG

	// Correct stack pointer for return processing.
        csel reg_CSP, reg_OCFP, reg_CSP, eq

        // Return value
        mov     x0, reg_R0

	ENTER_PA

// Save the lisp stack and frame pointers.
#ifdef LISP_FEATURE_SB_THREAD
	stp     reg_CFP,reg_CSP, [reg_THREAD, THREAD_CONTROL_FRAME_POINTER_OFFSET]
        str     reg_CSP, [reg_THREAD, THREAD_FOREIGN_FUNCTION_CALL_ACTIVE_OFFSET]
#else
	ldr	reg_NFP, =GNAME(current_control_frame_pointer)
	str	reg_CFP, [reg_NFP]
	ldr	reg_OCFP, =GNAME(current_control_stack_pointer)
	str	reg_CSP, [reg_OCFP]

	// Set FFCA, so the runtime knows that we're not "in lisp".
	ldr     reg_OCFP, =GNAME(foreign_function_call_active)
	str     reg_OCFP, [reg_OCFP]
#endif

        LEAVE_PA

	// Restore saved registers.

        ldp     d14,d15, [sp,#144]
        ldp     d12,d13, [sp,#128]
        ldp     d10,d11, [sp,#112]
        ldp     d8,d9, [sp,#96]


        ldp     x29,x30, [sp,#80]
        ldp     x27,x28, [sp,#64]
        ldp     x25,x26, [sp,#48]
        ldp     x23,x24, [sp,#32]
        ldp     x21,x22, [sp,#16]
        ldp     x19,x20, [sp],#160

	ret
	SIZE(call_into_lisp)


	.align 2
	.global	GNAME(call_into_c)
	TYPE(call_into_c)
GNAME(call_into_c):
	// At this point, we have:
	// R9 -- C function to call.
        // LR -- Return address within the code component.
        // X0-X7 arguments
        // All other C arguments are already stashed on the C stack.

	// We need to convert our return address to a GC-safe format,
	// build a stack frame to count for the "foreign" frame,
	// switch to C mode, move the register arguments to the
        // correct locations, call the C function, move the result to
        // the correct location, switch back to Lisp mode, tear down
        // our stack frame, restore the return address, and return to
        // our caller.

        sub     reg_NARGS, reg_LR, reg_CODE
        add     reg_NFP, reg_NARGS, #OTHER_POINTER_LOWTAG

        // Build a Lisp stack frame.  We need to stash our frame link,
        // the code component, and our return offset.  Frame link goes
	// in slot 0 (OCFP-SAVE-OFFSET), the offset (a FIXNUM) goes in
        // slot 1 (LRA-SAVE-OFFSET), and reg_CODE goes in slot 2.  The
        // debugger knows about this layout (see COMPUTE-CALLING-FRAME
        // in SYS:SRC;CODE;DEBUG-INT.LISP).
        add     reg_CSP, reg_CSP, #4*8
        stp     reg_CFP, reg_NFP, [reg_CSP, #-4*8]
        str     reg_CODE, [reg_CSP, #-2*8]

	ENTER_PA

        // Save the lisp stack and frame pointers.

#ifdef LISP_FEATURE_SB_THREAD
        sub     reg_TMP, reg_CSP, #4*8
        stp     reg_TMP,reg_CSP, [reg_THREAD, THREAD_CONTROL_FRAME_POINTER_OFFSET]
        str     reg_CSP, [reg_THREAD, THREAD_FOREIGN_FUNCTION_CALL_ACTIVE_OFFSET]
#else
	ldr	reg_NFP, =GNAME(current_control_stack_pointer)
	str	reg_CSP, [reg_NFP]
	ldr	reg_NFP, =GNAME(current_control_frame_pointer)
        sub     reg_TMP, reg_CSP, #4*8
	str	reg_TMP, [reg_NFP]

        // Set FFCA, so the runtime knows that we're not "in lisp".
	ldr     reg_OCFP, =GNAME(foreign_function_call_active)
	str     reg_OCFP, [reg_OCFP]
#endif
	LEAVE_PA
#ifdef LISP_FEATURE_SB_SAFEPOINT
	/* OK to run GC without stopping this thread from this point on. */
	str     reg_CSP, [reg_THREAD, THREAD_SAVED_CSP_OFFSET]
#endif

        // And call the C function.
        //
        // R9 is important for undefined_alien_function.
        blr      reg_R9

        // We're back.  Our main tasks are to move the C return value
        // to where Lisp expects it, and to re-establish the Lisp
        // environment.

        // Blank the boxed registers.
        mov     reg_R0, #0
        mov     reg_R1, #0
        mov     reg_R2, #0
        mov     reg_R3, #0
        mov     reg_R4, #0
        mov     reg_R5, #0
        mov     reg_R6, #0
        mov     reg_R7, #0
#ifndef LISP_FEATURE_DARWIN
        mov     reg_R8, #0
#endif
        mov     reg_R9, #0
#ifndef LISP_FEATURE_SB_THREAD
        mov     reg_R10, #0
#endif
        mov     reg_LEXENV, #0
        mov     reg_CODE, #0

# ifdef LISP_FEATURE_SB_SAFEPOINT
	/* No longer OK to run GC except at safepoints. */
	str     xzr, [reg_THREAD, THREAD_SAVED_CSP_OFFSET]
# endif

	ENTER_PA

        // Restore the Lisp stack and frame pointers, but store the
        // control frame pointer in reg_NFP (saving a register move
        // later).
#ifdef LISP_FEATURE_SB_THREAD
        str     xzr, [reg_THREAD, THREAD_FOREIGN_FUNCTION_CALL_ACTIVE_OFFSET]

	ldp     reg_NFP,reg_CSP, [reg_THREAD, THREAD_CONTROL_FRAME_POINTER_OFFSET]
#else
	// Clear FFCA, so the runtime knows that we're "in lisp".
	str     xzr, [reg_OCFP]

        ldr	reg_NFP, =GNAME(current_control_stack_pointer)
	ldr	reg_CSP, [reg_NFP]
	ldr	reg_NFP, =GNAME(current_control_frame_pointer)
	ldr	reg_NFP, [reg_NFP]
#endif
	LEAVE_PA

        // Restore our caller state from our stack frame.
        ldr     reg_CODE, [reg_NFP, #16]
        ldr     reg_CFP, [reg_NFP]
        mov     reg_CSP, reg_NFP

        // Return
        add     reg_LR, reg_NARGS, reg_CODE // reg_NARGS has the offset from reg_CODE
        ret

	SIZE(call_into_c)

        // FIXME-ARM: The following is random garbage, to make
        // code/debug-int compile. To get the debugger working, this
        // needs to be implemented.
        .align 2
        .global GNAME(fun_end_breakpoint_guts)
        TYPE(fun_end_breakpoint_guts)
GNAME(fun_end_breakpoint_guts):
	.global	GNAME(fun_end_breakpoint_trap)
	TYPE(fun_end_breakpoint_trap)
GNAME(fun_end_breakpoint_trap):
        b      GNAME(fun_end_breakpoint_trap)
        .global GNAME(fun_end_breakpoint_end)
GNAME(fun_end_breakpoint_end):

        .align 2
	.global	GNAME(do_pending_interrupt)
	TYPE(do_pending_interrupt)
GNAME(do_pending_interrupt):
        brk trap_PendingInterrupt
	ret

#ifdef __ELF__
// Mark the object as not requiring an executable stack.
.section .note.GNU-stack,"",%progbits
#endif
