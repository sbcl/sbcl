#define _ASM

#include "sparc-funcdef.h"

#define LANGUAGE_ASSEMBLY
#include "lispregs.h"
#include "globals.h"
#include "sbcl.h"
#include "genesis/closure.h"
#include "genesis/funcallable-instance.h"
#include "genesis/fdefn.h"
#include "genesis/static-symbols.h"
#include "genesis/simple-fun.h"	

#define load(sym, reg) \
        sethi %hi(sym), reg; ld [reg+%lo(sym)], reg
#define store(reg, sym) \
        sethi %hi(sym), reg_L0; st reg, [reg_L0+%lo(sym)]

/* FIXME */
#define FRAMESIZE 0x48
#define ST_FLUSH_WINDOWS 0x03
	.seg    "text"
        .global call_into_lisp
	FUNCDEF(call_into_lisp)
call_into_lisp:
        save    %sp, -FRAMESIZE, %sp

	/* Flush all of C's register windows to the stack. */
	ta	ST_FLUSH_WINDOWS

        /* Save the return address. */
        st      %i7, [%fp-4]

        /* Clear the descriptor regs. (See sparc/vm.lisp) */
        mov     reg_ZERO, reg_A0
        mov     reg_ZERO, reg_A1
        mov     reg_ZERO, reg_A2
        mov     reg_ZERO, reg_A3
        mov     reg_ZERO, reg_A4
        mov     reg_ZERO, reg_A5
        mov     reg_ZERO, reg_OCFP
        mov     reg_ZERO, reg_LRA
        mov     reg_ZERO, reg_CODE

        /* Establish NIL */
        set     NIL, reg_NIL

	/* Set the pseudo-atomic flag. */
	set	4, reg_ALLOC

	/* Turn off foreign function call. */
        sethi   %hi(foreign_function_call_active), reg_NL0
        st      reg_ZERO, [reg_NL0+%lo(foreign_function_call_active)]

        /* Load the rest of lisp state. */
        load(dynamic_space_free_pointer, reg_NL0)
	add	reg_NL0, reg_ALLOC, reg_ALLOC
        load(current_binding_stack_pointer, reg_BSP)
        load(current_control_stack_pointer, reg_CSP)
        load(current_control_frame_pointer, reg_OCFP)

        /* No longer atomic, and check for interrupt. */
	sub	reg_ALLOC, 4, reg_ALLOC
	andcc	reg_ALLOC, 3, reg_ZERO
	
	tne	PSEUDO_ATOMIC_TRAP
        /* Pass in the args. */
        sll     %i2, 2, reg_NARGS
        mov     %i1, reg_CFP
	mov	%i0, reg_LEXENV
        ld      [reg_CFP+0], reg_A0
        ld      [reg_CFP+4], reg_A1
        ld      [reg_CFP+8], reg_A2
        ld      [reg_CFP+12], reg_A3
        ld      [reg_CFP+16], reg_A4
        ld      [reg_CFP+20], reg_A5

        /* Calculate LRA */
        set     lra + OTHER_POINTER_LOWTAG, reg_LRA

        /* Indirect closure */
        ld      [reg_LEXENV+CLOSURE_FUN_OFFSET], reg_CODE

        jmp     reg_CODE+SIMPLE_FUN_CODE_OFFSET
        nop

        .align  8
lra:
        .word   RETURN_PC_HEADER_WIDETAG

        /* Blow off any extra values. */
        mov     reg_OCFP, reg_CSP
        nop

        /* Return the one value. */
        mov     reg_A0, %i0

        /* Turn on pseudo_atomic */
	add	reg_ALLOC, 4, reg_ALLOC

        /* Store LISP state */
	andn	reg_ALLOC, 7, reg_NL1
        store(reg_NL1,dynamic_space_free_pointer)
        store(reg_BSP,current_binding_stack_pointer)
        store(reg_CSP,current_control_stack_pointer)
        store(reg_CFP,current_control_frame_pointer)

        /* No longer in Lisp. */
        store(reg_NL1,foreign_function_call_active)

        /* Were we interrupted? */
	sub	reg_ALLOC, 4, reg_ALLOC
	andcc	reg_ALLOC, 3, reg_ZERO
	tne	PSEUDO_ATOMIC_TRAP

        /* Back to C we go. */
	ld	[%sp+FRAMESIZE-4], %i7
        ret
        restore	%sp, FRAMESIZE, %sp

        .global call_into_c
	FUNCDEF(call_into_c)
call_into_c:
        /* Build a lisp stack frame */
        mov     reg_CFP, reg_OCFP
        mov     reg_CSP, reg_CFP
        add     reg_CSP, 32, reg_CSP
        st      reg_OCFP, [reg_CFP]
        st      reg_CODE, [reg_CFP+8]

        /* Turn on pseudo-atomic. */
	add	reg_ALLOC, 4, reg_ALLOC

	/* Convert the return address to an offset and save it on the stack. */
	sub	reg_LIP, reg_CODE, reg_L0
	add	reg_L0, OTHER_POINTER_LOWTAG, reg_L0
	st	reg_L0, [reg_CFP+4]

        /* Store LISP state */
        store(reg_BSP,current_binding_stack_pointer)
        store(reg_CSP,current_control_stack_pointer)
        store(reg_CFP,current_control_frame_pointer)
	/* Use reg_CFP as a work register, and restore it */
	andn	reg_ALLOC, 7, reg_CFP
        store(reg_CFP,dynamic_space_free_pointer)
		load(current_control_frame_pointer, reg_CFP)

        /* No longer in Lisp. */
        store(reg_CSP,foreign_function_call_active)

        /* Were we interrupted? */
	sub	reg_ALLOC, 4, reg_ALLOC
	andcc	reg_ALLOC, 3, reg_ZERO
	tne	PSEUDO_ATOMIC_TRAP

        /* Into C we go. */
        call    reg_CFUNC
        nop

	/*
	 * Note: C calling conventions (32-bit) say that %o0 and %o1
	 * are used to return function results.  In particular 64-bit
	 * results are in %o0 (hi) and %o1 (low).  
	 */
	
        /* Re-establish NIL */
        set     NIL, reg_NIL

	/* Atomic. */
	set	4, reg_ALLOC

        /* No longer in foreign function call. */
        sethi   %hi(foreign_function_call_active), reg_NL2
        st      reg_ZERO, [reg_NL2+%lo(foreign_function_call_active)]

        /* Load the rest of lisp state. */
        load(dynamic_space_free_pointer, reg_NL2)
	add	reg_NL2, reg_ALLOC, reg_ALLOC
        load(current_binding_stack_pointer, reg_BSP)
        load(current_control_stack_pointer, reg_CSP)
        load(current_control_frame_pointer, reg_CFP)

	/* Get the return address back. */
	ld	[reg_CFP+4], reg_LIP
	ld	[reg_CFP+8], reg_CODE
	add	reg_LIP, reg_CODE, reg_LIP
	sub	reg_LIP, OTHER_POINTER_LOWTAG, reg_LIP

        /* No longer atomic. */
	sub	reg_ALLOC, 4, reg_ALLOC
	andcc	reg_ALLOC, 3, reg_ZERO
	tne	PSEUDO_ATOMIC_TRAP

        /* Reset the lisp stack. */
        /* Note: OCFP is in one of the locals, it gets preserved across C. */
        mov     reg_CFP, reg_CSP
        mov     reg_OCFP, reg_CFP

        /* And back into lisp. */
        ret
        nop

/* Lisp calling convention. notice the first .byte line.
 */		
        .global undefined_tramp
	FUNCDEF(undefined_tramp)
        .align  8
        .byte   0, 0, 0, SIMPLE_FUN_HEADER_WIDETAG
undefined_tramp = . + 1
	.word	undefined_tramp
        .word   NIL
        .word   NIL
        .word   NIL
        .word   NIL
        .word   NIL

	b	1f
        unimp   trap_Cerror
	.byte	4
        .byte   UNDEFINED_FUN_ERROR
	.byte	254, sc_DescriptorReg, 3
	.align	4
1:
	ld	[reg_FDEFN+FDEFN_RAW_ADDR_OFFSET], reg_CODE
	jmp	reg_CODE+SIMPLE_FUN_CODE_OFFSET
	nop

/* Lisp calling convention. Notice the first .byte line.
 */		
	.global	closure_tramp
	FUNCDEF(closure_tramp)
	.align	8
	.byte	0, 0, 0, SIMPLE_FUN_HEADER_WIDETAG
closure_tramp = . + 1
	.word	closure_tramp
	.word	NIL
        .word   NIL
	.word	NIL
	.word	NIL
	.word	NIL

	ld	[reg_FDEFN+FDEFN_FUN_OFFSET], reg_LEXENV
	ld	[reg_LEXENV+CLOSURE_FUN_OFFSET], reg_CODE
	jmp	reg_CODE+SIMPLE_FUN_CODE_OFFSET
	nop

	.global funcallable_instance_tramp
	FUNCDEF(funcallable_instance_tramp)
	.align 8
	.word SIMPLE_FUN_HEADER_WIDETAG
funcallable_instance_tramp = . + 1
	.word funcallable_instance_tramp
	.word NIL
	.word NIL
	.word NIL
	.word NIL
	.word NIL

	ld	[reg_LEXENV+FUNCALLABLE_INSTANCE_FUNCTION_OFFSET], reg_LEXENV
	ld	[reg_LEXENV+CLOSURE_FUN_OFFSET], reg_CODE
	jmp	reg_CODE+SIMPLE_FUN_CODE_OFFSET
	nop
/*
 * Function-end breakpoint magic.
 */

	.text
	.align	8
	.global	fun_end_breakpoint_guts
fun_end_breakpoint_guts:
	.word	RETURN_PC_HEADER_WIDETAG
	b	1f
	nop
	mov	reg_CSP, reg_OCFP
	add	4, reg_CSP, reg_CSP
	mov	4, reg_NARGS
	mov	reg_NIL, reg_A1
	mov	reg_NIL, reg_A2
	mov	reg_NIL, reg_A3
	mov	reg_NIL, reg_A4
	mov	reg_NIL, reg_A5
1:

	.global	fun_end_breakpoint_trap
fun_end_breakpoint_trap:
	unimp	trap_FunEndBreakpoint
	b	1b
	nop

	.global	fun_end_breakpoint_end
fun_end_breakpoint_end:

	.global sparc_flush_icache
	FUNCDEF(sparc_flush_icache)
sparc_flush_icache:
        add %o0,%o1,%o2
1:      iflush %o0			! flush instruction cache
        add %o0,8,%o0
        cmp %o0,%o2
        blt 1b
        nop
	retl				! return from leaf routine
        nop

	.global save_context
	FUNCDEF(save_context)
save_context:
	ta	ST_FLUSH_WINDOWS	! flush register windows
	retl				! return from leaf routine
	nop 
