#define LANGUAGE_ASSEMBLY

#include "sbcl.h"	
#include "lispregs.h"
#include "genesis/fdefn.h"
#include "genesis/closure.h"
#include "genesis/simple-fun.h"
#include "genesis/static-symbols.h"
		
#define zero $0
#define at $1
#define v0 $2
#define v1 $3
#define a0 $4
#define a1 $5
#define a2 $6
#define a3 $7
#define t0 $8
#define t1 $9
#define t2 $10
#define t3 $11
#define t4 $12
#define t5 $13
#define t6 $14
#define t7 $15
#define s0 $16
#define s1 $17
#define s2 $18
#define s3 $19
#define s4 $20
#define s5 $21
#define s6 $22
#define s7 $23
#define t8 $24
#define t9 $25
#define k0 $26
#define k1 $27
#define gp $28
#define sp $29
#define s8 $30
#define ra $31

	
/*
 * Function to transfer control into lisp.
 */
	.text
	.globl	call_into_lisp
	.ent	call_into_lisp
call_into_lisp:
#define framesize 12*4
	subu	sp, framesize
	.frame	sp, framesize, ra
	/* Save all the C regs. */
	.mask	0xc0ff0000, 0
	sw	ra, framesize(sp)
	sw	s8, framesize-4(sp)
	sw	s7, framesize-12(sp)
	sw	s6, framesize-16(sp)
	sw	s5, framesize-20(sp)
	sw	s4, framesize-24(sp)
	sw	s3, framesize-28(sp)
	sw	s2, framesize-32(sp)
	sw	s1, framesize-36(sp)
	sw	s0, framesize-40(sp)

	/* Clear descriptor regs */
	move	t0, zero
	move	t1, zero
	move	t2, zero
	move	t3, zero
	move	t4, zero
	move	t5, zero
	move	t6, zero
	move	t7, zero
	move	t8, zero
	move	s0, zero
	move	s1, zero
	move	s2, zero
	move	s3, zero
	move	ra, zero

	li	reg_NIL, NIL

	/* Start pseudo-atomic. */
	.set	noreorder
	li	reg_NL4, 0
	li	reg_ALLOC, 1
        .set    reorder

	/* No longer in foreign call. */
	sw	zero, foreign_function_call_active

	/* Load the allocation pointer, preserving the low-bit of alloc */
	lw	reg_BSP, dynamic_space_free_pointer
	add	reg_ALLOC, reg_BSP

	/* Load the rest of the LISP state. */
	lw	reg_BSP, current_binding_stack_pointer
	lw	reg_CSP, current_control_stack_pointer
	lw	reg_OCFP, current_control_frame_pointer

	/* Check for interrupt */
        .set    noreorder
	bgez	reg_NL4, pa1
	nop
	break	0x10
pa1:
	subu	reg_ALLOC, 1
	.set	reorder

	/* Pass in args */
	move	reg_LEXENV, $4
	move	reg_CFP, $5
	sll	reg_NARGS, $6, 2
	lw	reg_A0, 0(reg_CFP)
	lw	reg_A1, 4(reg_CFP)
	lw	reg_A2, 8(reg_CFP)
	lw	reg_A3, 12(reg_CFP)
	lw	reg_A4, 16(reg_CFP)
	lw	reg_A5, 20(reg_CFP)

	/* Calculate LRA */
	la	reg_LRA, lra + OTHER_POINTER_LOWTAG

	/* Indirect closure */
	lw	reg_CODE, -1(reg_LEXENV)

	/* Jump into lisp land. */
	addu	reg_LIP, reg_CODE, 6*4 - FUN_POINTER_LOWTAG
	j	reg_LIP

	.set	noreorder
	.align	3
#ifdef irix
	/* This particular KLUDGE is kept here as a reminder; for more
	details, see irix-asm-munge.c from CMUCL's lisp directory.
	Other examples have been deleted from later in the file in the
	hope that they will not be needed. */
.globl  mipsmungelra /* for our munging afterwards in irix-asm-munge */
mipsmungelra:
#endif
lra:
	.word	RETURN_PC_HEADER_WIDETAG

	/* Multiple value return spot, clear stack */
	move	reg_CSP, reg_OCFP
	nop

	/* Set pseudo-atomic flag. */
	li	reg_NL4, 0
	addu	reg_ALLOC, 1
	.set	reorder

	/* Save LISP registers. */
	subu	reg_NL0, reg_ALLOC, 1
	sw	reg_NL0, dynamic_space_free_pointer
	sw	reg_BSP, current_binding_stack_pointer
	sw	reg_CSP, current_control_stack_pointer
	sw	reg_CFP, current_control_frame_pointer

	/* Pass one return value back to C land. */ 
	/* v0 is reg_ALLOC in this new world, so do this after saving
           reg_ALLOC in dynamic_space_free_pointer */
	move	v0, reg_A0

	/* Back in foreign function call */
	sw	reg_CFP, foreign_function_call_active

	/* Check for interrupt */
	.set	noreorder
	bgez	reg_NL4, pa2
	nop
	break	0x10
pa2:
	subu	reg_ALLOC, 1
	.set	reorder

	/* Restore C regs */
	lw	ra, framesize(sp)
	lw	s8, framesize-4(sp)
	lw	s7, framesize-12(sp)
	lw	s6, framesize-16(sp)
	lw	s5, framesize-20(sp)
	lw	s4, framesize-24(sp)
	lw	s3, framesize-28(sp)
	lw	s2, framesize-32(sp)
	lw	s1, framesize-36(sp)
	lw	s0, framesize-40(sp)

	/* Restore C stack. */
	addu	sp, framesize

	/* Back we go. */
	j	ra

	.end	call_into_lisp

/*
 * Transfering control from Lisp into C
 */
	.text
	.globl	call_into_c
	.ent	call_into_c
call_into_c:
	/* Set up a stack frame. */
	move	reg_OCFP, reg_CFP
	move	reg_CFP, reg_CSP
	addu	reg_CSP, reg_CFP, 32
	sw	reg_OCFP, 0(reg_CFP)
	subu	reg_NL4, reg_LIP, reg_CODE
	addu	reg_NL4, OTHER_POINTER_LOWTAG
	sw	reg_NL4, 4(reg_CFP)
	sw	reg_CODE, 8(reg_CFP)
        sw      gp, 12(reg_CFP)

	/* Note: the C stack is already set up. */

	/* Set the pseudo-atomic flag. */
	.set	noreorder
	li	reg_NL4, 0
	addu	reg_ALLOC, 1
	.set	reorder

	/* Save lisp state. */
	subu	t0, reg_ALLOC, 1
	sw	t0, dynamic_space_free_pointer
	sw	reg_BSP, current_binding_stack_pointer
	sw	reg_CSP, current_control_stack_pointer
	sw	reg_CFP, current_control_frame_pointer

	/* Mark us as in C land. */
	sw	reg_CSP, foreign_function_call_active

	/* Were we interrupted? */
	.set	noreorder
	bgez	reg_NL4, pa3
	nop
	break	0x10
pa3:
	subu	reg_ALLOC, 1
	.set	reorder

	/* Into C land we go. */
	move    t9, reg_CFUNC
	jal	t9
	nop

	lw      gp, 12(reg_CFP)
	
	/* Clear unsaved descriptor regs */
	move	t0, zero
	move	t1, zero
	move	t2, zero
	move	t3, zero
	move	t4, zero
	move	t5, zero
	move	t6, zero
	move	t7, zero
	move	t8, zero
	move	s0, zero
	move	s2, zero
	move	s3, zero
	move	ra, zero

	/* Turn on pseudo-atomic. */
	.set	noreorder
	li	reg_NL4, 0
	li	reg_ALLOC, 1
	.set	reorder

	/* Mark us at in Lisp land. */
	sw	zero, foreign_function_call_active

	/* Restore ALLOC, preserving pseudo-atomic-atomic */
	lw	a0, dynamic_space_free_pointer
	addu	reg_ALLOC, a0

	/* Check for interrupt */
	.set	noreorder
	bgez	reg_NL4, pa4
	nop
	break	0x10
pa4:
	subu	reg_ALLOC, 1
	.set	reorder

	/* Restore LRA & CODE (they may have been GC'ed) */
	lw	reg_CODE, 8(reg_CFP)
	lw	a0, 4(reg_CFP)
	subu	a0, OTHER_POINTER_LOWTAG
	addu	reg_LIP, reg_CODE, a0

	/* Reset the lisp stack. */
	/* Note: OCFP and CFP are in saved regs. */
	move	reg_CSP, reg_CFP
	move	reg_CFP, reg_OCFP

	/* Return to LISP. */
	j	reg_LIP

	.end	call_into_c

	.text
	.globl	start_of_tramps
start_of_tramps:

/*
 * The undefined-function trampoline.
 */
        .text
        .globl  undefined_tramp
        .ent    undefined_tramp
undefined_tramp:
        break   10
        .byte    4
        .byte    UNDEFINED_FUN_ERROR
        .byte    254
        .byte    (0xc0 + sc_DescriptorReg)
        .byte    1
        .align 2
        .end    undefined_tramp

/*
 * The closure trampoline.
 */
        .text
        .globl  closure_tramp
        .ent    closure_tramp
closure_tramp:
        lw      reg_LEXENV, FDEFN_FUN_OFFSET(reg_FDEFN)
        lw      reg_L0, CLOSURE_FUN_OFFSET(reg_LEXENV)
        addu    reg_LIP, reg_L0, SIMPLE_FUN_CODE_OFFSET
        j       reg_LIP
        .end    closure_tramp

	.text
	.globl	end_of_tramps
end_of_tramps:


/*
 * Function-end breakpoint magic.
 */

	.text
	.align	2
	.set	noreorder
	.globl	function_end_breakpoint_guts
fun_end_breakpoint_guts:
	.word	RETURN_PC_HEADER_WIDETAG
	
	beq	zero, zero, 1f
	nop
	move	reg_OCFP, reg_CSP
	addu	reg_CSP, 4
	li	reg_NARGS, 4
	move	reg_A1, reg_NIL
	move	reg_A2, reg_NIL
	move	reg_A3, reg_NIL
	move	reg_A4, reg_NIL
	move	reg_A5, reg_NIL
1:

	.globl	fun_end_breakpoint_trap
fun_end_breakpoint_trap:
	break	trap_FunEndBreakpoint
	beq	zero, zero, 1b
	nop

	.globl	fun_end_breakpoint_end
fun_end_breakpoint_end:
	.set	reorder

/* FIXME:	 I don't think the below are actually used anywhere */
	.text
	.align	2
	.globl	call_on_stack
	.ent	call_on_stack
call_on_stack:
	subu	sp, a1, 16
	jal	a0
	break	0
	.end	call_on_stack

	.globl	save_state
	.ent	save_state
save_state:
	subu	sp, 40
	.frame	sp, 40, ra
	/* Save all the C regs. */
	.mask	0xc0ff0000, 0
	sw	ra, 40(sp)
	sw	s8, 40-4(sp)
	sw	s7, 40-8(sp)
	sw	s6, 40-12(sp)
	sw	s5, 40-16(sp)
	sw	s4, 40-20(sp)
	sw	s3, 40-24(sp)
	sw	s2, 40-28(sp)
	sw	s1, 40-32(sp)
	sw	s0, 40-36(sp)

	/* Should also save the floating point state. */

	move	t0, a0
	move	a0, sp

	jal	t0

_restore_state:

	lw	ra, 40(sp)
	lw	s8, 40-4(sp)
	lw	s7, 40-8(sp)
	lw	s6, 40-12(sp)
	lw	s5, 40-16(sp)
	lw	s4, 40-20(sp)
	lw	s3, 40-24(sp)
	lw	s2, 40-28(sp)
	lw	s1, 40-32(sp)
	lw	s0, 40-36(sp)

	addu	sp, 40
	j	ra

	.globl	restore_state
restore_state:
	move	sp, a0
	move	v0, a1
	j	_restore_state
	.end	save_state





