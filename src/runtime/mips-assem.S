#include "sbcl.h"
#include "lispregs.h"
#include "globals.h"
#include "genesis/fdefn.h"
#include "genesis/closure.h"
#include "genesis/simple-fun.h"
#include "genesis/static-symbols.h"
		
#define zero $0
#define AT $1
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
 * LEAF - declare leaf routine
 */
#define	LEAF(symbol)                                    \
		.globl	symbol;                         \
		.align	2;                              \
		.type	symbol,@function;               \
		.ent	symbol,0;                       \
symbol:		.frame	sp,0,ra

/*
 * NESTED - declare nested routine entry point
 */
#define	NESTED(symbol, framesize, rpc)                  \
		.globl	symbol;                         \
		.align	2;                              \
		.type	symbol,@function;               \
		.ent	symbol,0;                       \
symbol:		.frame	sp, framesize, rpc

/*
 * END - mark end of function
 */
#define	END(function)                                   \
		.end	function;		        \
		.size	function,.-function

/*
 * EXPORT - export definition of symbol
 */
#define EXPORT(symbol)					\
		.globl	symbol;                         \
symbol:

/*
 * FEXPORT - export definition of a function symbol
 */
#define FEXPORT(symbol)					\
		.globl	symbol;				\
		.type	symbol,@function;		\
symbol:


	.text
	
/*
 * Function to transfer control into lisp.
 */
#define framesize 16*4
	NESTED(call_into_lisp, framesize, ra)
	.set	noreorder
	.cpload t9
	.set	reorder
	subu	sp, framesize

	/* Save all the C regs. */
	.mask	0xc0ff0000, -8
	sw	ra, framesize-8(sp)
	sw	s8, framesize-12(sp)
	/* No .cprestore, we don't want automatic gp restauration. */
	sw	gp, framesize-16(sp)
	sw	s7, framesize-20(sp)
	sw	s6, framesize-24(sp)
	sw	s5, framesize-28(sp)
	sw	s4, framesize-32(sp)
	sw	s3, framesize-36(sp)
	sw	s2, framesize-40(sp)
	sw	s1, framesize-44(sp)
	sw	s0, framesize-48(sp)

	li	reg_NIL, NIL

	/* Clear unsaved boxed descriptor regs */
	li	reg_FDEFN, 0		# t6
	li	reg_L1, 0		# t8

	/* Turn on pseudo-atomic. */
	.set	noreorder
	li	reg_NL4, 0
	li	reg_ALLOC, 1
        .set    reorder

	/* Mark us as in Lisp land. */
	sw	zero, foreign_function_call_active

	/* Load the allocation pointer, preserving the low-bit of alloc */
	lw	reg_BSP, dynamic_space_free_pointer
	addu	reg_ALLOC, reg_BSP

	/* Load the rest of the LISP state. */
	lw	reg_BSP, current_binding_stack_pointer
	lw	reg_CSP, current_control_stack_pointer
	lw	reg_OCFP, current_control_frame_pointer

	/* Check for interrupt */
        .set    noreorder
	bgez	reg_NL4, 1f
	 subu	reg_ALLOC, 1
	break	0x10
1:	.set	reorder

	/* Pass in args */
	move	reg_LEXENV, a0
	move	reg_CFP, a1
	sll	reg_NARGS, a2, 2
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
	jr	reg_LIP

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

	/* Multiple value return spot, clear stack. */
	move	reg_CSP, reg_OCFP
	 nop

	/* Single value return spot. */

	/* Pass one return value back to C land. */ 
	move	v0, reg_A0	# reg_CFUNC

	/* Nested lisp -> C calls may have clobbered gp. */
	lw	gp, framesize-16(sp)

	/* Set the pseudo-atomic flag. */
	li	reg_NL4, 0
	addu	reg_ALLOC, 1
	.set	reorder

	/* Save LISP state. */
	subu	reg_NL0, reg_ALLOC, 1
	sw	reg_NL0, dynamic_space_free_pointer
	sw	reg_BSP, current_binding_stack_pointer
	sw	reg_CSP, current_control_stack_pointer
	sw	reg_CFP, current_control_frame_pointer

	/* Mark us as in C land. */
	sw	reg_CSP, foreign_function_call_active

	/* Check for interrupt */
	.set	noreorder
	bgez	reg_NL4, 1f
	 subu	reg_ALLOC, 1
	break	0x10
1:	.set	reorder

	/* Restore C regs */
	lw	ra, framesize-8(sp)
	lw	s8, framesize-12(sp)
	lw	s7, framesize-20(sp)
	lw	s6, framesize-24(sp)
	lw	s5, framesize-28(sp)
	lw	s4, framesize-32(sp)
	lw	s3, framesize-36(sp)
	lw	s2, framesize-40(sp)
	lw	s1, framesize-44(sp)
	lw	s0, framesize-48(sp)

	/* Restore C stack. */
	addu	sp, framesize

	/* Back we go. */
	jr	ra

	END(call_into_lisp)

/*
 * Transfering control from Lisp into C
 */
 	NESTED(call_into_c, 0, ra)
	/* The stack frame was already set up from lisp. We have
	   to fake the correct gp value for this function, though. */
	.set	noreorder
	.set	noat
	lui	gp, %hi(_gp_disp)
	addiu	gp, %lo(_gp_disp)
	lui	reg_NL3, %hi(call_into_c)
	addiu	reg_NL3, %lo(call_into_c)
	addu	gp, reg_NL3
	.set	at
	.set	reorder

	move	reg_OCFP, reg_CFP
	move	reg_CFP, reg_CSP
	addu	reg_CSP, reg_CFP, 32

	subu	reg_LIP, reg_CODE
	addu	reg_LIP, OTHER_POINTER_LOWTAG
	sw	reg_LIP, (reg_CFP)
	sw	reg_CODE, 4(reg_CFP)
	sw	gp, 8(reg_CFP)

	/* Set the pseudo-atomic flag. */
	.set	noreorder
	li	reg_NL4, 0
	addu	reg_ALLOC, 1
	.set	reorder

	/* Save LISP state. */
	subu	reg_A0, reg_ALLOC, 1
	sw	reg_A0, dynamic_space_free_pointer
	sw	reg_BSP, current_binding_stack_pointer
	sw	reg_CSP, current_control_stack_pointer
	sw	reg_CFP, current_control_frame_pointer

	/* Mark us as in C land. */
	sw	reg_CSP, foreign_function_call_active

	/* Check for interrupt */
	.set	noreorder
	bgez	reg_NL4, 1f
	 subu	reg_ALLOC, 1
	break	0x10
1:	.set	reorder

	/* Into C land we go. */
	move    t9, reg_CFUNC
	jalr	t9

	lw      gp, 8(reg_CFP)
	
	li	reg_NIL, NIL

	/* Clear unsaved boxed descriptor regs */
	li	reg_A0, 0		# t0
	li	reg_A1, 0		# t1
	li	reg_A2, 0		# t2
	li	reg_A3, 0		# t3
	li	reg_A4, 0		# t4
	li	reg_A5, 0		# t5
	li	reg_FDEFN, 0		# t6
	li	reg_LEXENV, 0		# t7
	li	reg_L1, 0		# t8

	/* Turn on pseudo-atomic. */
	.set	noreorder
	li	reg_NL4, 0
	li	reg_ALLOC, 1
	.set	reorder

	/* Mark us as in Lisp land. */
	sw	zero, foreign_function_call_active

	/* Load the allocation pointer, preserving the low-bit of alloc */
	lw	reg_BSP, dynamic_space_free_pointer
	addu	reg_ALLOC, reg_BSP

	lw	reg_BSP, current_binding_stack_pointer

	/* Restore LRA & CODE */
	lw	reg_LIP, (reg_CFP)
	lw	reg_CODE, 4(reg_CFP)
	subu	reg_LIP, OTHER_POINTER_LOWTAG
	addu	reg_LIP, reg_CODE

	/* Check for interrupt */
	.set	noreorder
	bgez	reg_NL4, 1f
	 subu	reg_ALLOC, 1
	break	0x10
1:	.set	reorder

	/* Reset the lisp stack. */
	/* Note: OCFP and CFP are in saved regs. */
	move	reg_CSP, reg_CFP
	move	reg_CFP, reg_OCFP

	/* Return to LISP. */
	jr	reg_LIP
	END(call_into_c)

	EXPORT(start_of_tramps)

/*
 * The undefined-function trampoline.
 */
	LEAF(undefined_tramp)
        break	10
        .byte	4
        .byte	UNDEFINED_FUN_ERROR
        .byte	254
        .byte	(0xc0 + sc_DescriptorReg)
        .byte	1
	.align	2
	END(undefined_tramp)

/*
 * The closure trampoline.
 */
	LEAF(closure_tramp)
        lw	reg_LEXENV, FDEFN_FUN_OFFSET(reg_FDEFN)
        lw	reg_L0, CLOSURE_FUN_OFFSET(reg_LEXENV)
        addu	reg_LIP, reg_L0, SIMPLE_FUN_CODE_OFFSET
        jr	reg_LIP
	END(closure_tramp)

	EXPORT(end_of_tramps)

/*
 * Function-end breakpoint magic.
 */
	LEAF(fun_end_breakpoint_guts)
	.set	noreorder
	.align	3
	.word	RETURN_PC_HEADER_WIDETAG
	
	b	multiple_value_return
	 nop

	move	reg_OCFP, reg_CSP
	addu	reg_CSP, 4
	li	reg_NARGS, 4
	move	reg_A1, reg_NIL
	move	reg_A2, reg_NIL
	move	reg_A3, reg_NIL
	move	reg_A4, reg_NIL
	move	reg_A5, reg_NIL

multiple_value_return:

	FEXPORT(fun_end_breakpoint_trap)
	break	trap_FunEndBreakpoint
1:	b	1b
	 nop

	EXPORT(fun_end_breakpoint_end)
	.set	reorder
	END(fun_end_breakpoint_guts)
