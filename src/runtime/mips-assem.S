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
#include "lispregs.h"
#include "genesis/closure.h"
#include "genesis/static-symbols.h"
#include "genesis/symbol.h"

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
//#define gp $28
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

/* zero (or anything non-nil) -> atomic=true and NIL -> atomic=false  */
#define BEGIN_PSEUDO_ATOMIC \
	.set	noreorder ; \
	sw 	zero, (PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET-NIL)(reg_NIL) ; \
	.set	reorder
#define END_PSEUDO_ATOMIC(temp) \
	.set	noreorder ; \
	sw	reg_NIL, (PSEUDO_ATOMIC_ATOMIC+SYMBOL_VALUE_OFFSET-NIL)(reg_NIL) ; \
	lw	temp, (PSEUDO_ATOMIC_INTERRUPTED+SYMBOL_VALUE_OFFSET-NIL)(reg_NIL) ; \
	tne	zero, temp, 0 ; \
	.set	reorder

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
	sw	$gp, framesize-16(sp)
	/* No .cprestore, we don't want automatic gp restoration. */
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

	BEGIN_PSEUDO_ATOMIC

	/* Load the rest of the LISP state. */
	lw	reg_BSP, current_binding_stack_pointer
	lw	reg_CSP, current_control_stack_pointer
	lw	reg_OCFP, current_control_frame_pointer

	END_PSEUDO_ATOMIC(reg_NL4)

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
	lw	reg_CODE, CLOSURE_FUN_OFFSET(reg_LEXENV)

	addu	reg_LIP, reg_CODE, SIMPLE_FUN_INSTS_OFFSET

	/* Mark us as in Lisp land. */
	sw	zero, foreign_function_call_active
	lw	reg_CARDBASE, gc_card_mark

	/* Jump into lisp land. */
	jr	reg_LIP

	.align	3
	.set	noreorder
lra:	.word	RETURN_PC_WIDETAG

	/* Multiple value return spot, clear stack. */
	move	reg_CSP, reg_OCFP
	nop

	/* Single value return spot. */

	/* Nested lisp -> C calls may have clobbered gp. */
	lw	$gp, framesize-16(sp)

	/* Mark us as in C land. */
	sw	reg_CSP, foreign_function_call_active

	BEGIN_PSEUDO_ATOMIC

	/* Save LISP state. */
	sw	reg_BSP, current_binding_stack_pointer
	sw	reg_CSP, current_control_stack_pointer
	sw	reg_CFP, current_control_frame_pointer

	END_PSEUDO_ATOMIC(reg_NL4)

	/* Pass one return value back to C land. For a 64bit value, we may
	   need to clobber v1 aka reg_NL4. */
	move	v0, reg_A0	# reg_CFUNC
	move	v1, reg_A1	# reg_NL4

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

	/* Setup the lisp stack. */
	move	reg_OCFP, reg_CFP
	move	reg_CFP, reg_CSP
	addu	reg_CSP, reg_CFP, 32

	/* Mark us as in C land. */
	sw	reg_CSP, foreign_function_call_active

	BEGIN_PSEUDO_ATOMIC

	/* Convert the return address to an offset and save it on the stack. */
	subu	reg_NFP, reg_LIP, reg_CODE
	addu	reg_NFP, OTHER_POINTER_LOWTAG
        sw      reg_OCFP, (reg_CFP)
	sw	reg_NFP, 4(reg_CFP)
	sw	reg_CODE, 8(reg_CFP)
	sw	$gp, 12(reg_CFP) // should this be saved? on the control stack?

	/* Save LISP state. */
	sw	reg_BSP, current_binding_stack_pointer
	sw	reg_CSP, current_control_stack_pointer
	sw	reg_CFP, current_control_frame_pointer

	/* from the ABI: "When calling position independent functions
	   $25 must cotain the address of the called function." */
	lw	$25, (reg_CFUNC) /* linkage-table indirection */

	END_PSEUDO_ATOMIC(reg_NL4)

	/* Into C land we go. */
	jalr	$25

	lw	$gp, 12(reg_CFP)

	/* Pass 64bit return value to lisp land. */
	move	reg_NL0, v0		# reg_CFUNC
	move	reg_NL1, v1		# reg_NL4

	/*
	 * Clear boxed descriptor registers before allowing an interrupt.
	 * We can't rely on C saving some of those registers, they might
	 * have been GCed in the meanwhile.
	 */
	li	reg_A0, 0		# t0
	li	reg_A1, 0		# t1
	li	reg_A2, 0		# t2
	li	reg_A3, 0		# t3
	li	reg_A4, 0		# t4
	li	reg_A5, 0		# t5
	li	reg_FDEFN, 0		# t6
	li	reg_LEXENV, 0		# t7
	/*
	 * reg_NFP and reg_OCFP are pointing to fixed locations and are
	 * preserved by C.
	 */
	li	reg_LRA, 0		# s2
	li	reg_L0, 0		# s3
	li	reg_L1, 0		# t8
	li	reg_CODE, 0		# s8
	li	reg_LIP, 0		# ra

	BEGIN_PSEUDO_ATOMIC

	lw	reg_BSP, current_binding_stack_pointer

	/* Restore CODE */
	lw	reg_CODE, 8(reg_CFP)
        /* Setup the return address */
	subu	reg_LIP, reg_NFP, OTHER_POINTER_LOWTAG
	addu	reg_LIP, reg_CODE

	END_PSEUDO_ATOMIC(reg_NL4)

	/* Reset the lisp stack. */
	/* Note: OCFP and CFP are in saved regs. */
	move	reg_CSP, reg_CFP
	move	reg_CFP, reg_OCFP

	/* Mark us as in Lisp land. */
	sw	zero, foreign_function_call_active
	lw	reg_CARDBASE, gc_card_mark

	/* Return to LISP. */
	jr	reg_LIP
	END(call_into_c)

/*
 * Function-end breakpoint magic. This is truely magic, the code is
 * copied and has to be relocatable.
 */

/*
 * For an explanation of the magic involved in function-end
 * breakpoints, see the implementation in ppc-assem.S.
 */
	LEAF(fun_end_breakpoint_guts)
	.set    noreorder
	b       multiple_value_return
	 nop
	.set    reorder

	/* single value return */

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
	.set    noreorder
	b	multiple_value_return
	 break	0x0, trap_FunEndBreakpoint
	.set    reorder
	EXPORT(fun_end_breakpoint_end)
	END(fun_end_breakpoint_guts)


	.align  3 /* minimum alignment for a lisp object */
        LEAF(do_pending_interrupt)
	break	0x0, trap_PendingInterrupt
	jr	reg_LIP
	END(do_pending_interrupt)
