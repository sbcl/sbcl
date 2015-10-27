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

#include "sbcl.h"
#include "lispregs.h"
#include "globals.h"
#include "genesis/fdefn.h"
#include "genesis/closure.h"
#include "genesis/funcallable-instance.h"
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
	break	0x0, 0x10
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
	lw	reg_CODE, CLOSURE_FUN_OFFSET(reg_LEXENV)

	addu	reg_LIP, reg_CODE, SIMPLE_FUN_CODE_OFFSET

	/* Mark us as in Lisp land. */
	sw	zero, foreign_function_call_active

	/* Jump into lisp land. */
	jr	reg_LIP

	.align	3
	.set	noreorder
lra:	.word	RETURN_PC_HEADER_WIDETAG

	/* Multiple value return spot, clear stack. */
	move	reg_CSP, reg_OCFP
	nop

	/* Single value return spot. */

	/* Nested lisp -> C calls may have clobbered gp. */
	lw	gp, framesize-16(sp)

	/* Mark us as in C land. */
	sw	reg_CSP, foreign_function_call_active

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

	/* Check for interrupt */
	.set	noreorder
	bgez	reg_NL4, 1f
	 subu	reg_ALLOC, 1
	break	0x0, 0x10
1:	.set	reorder

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
	/* The C stack frame was already set up from lisp, and the
	   argument registers as well. We have to fake the correct
	   gp value for this function, though. */
	.set	noreorder
	/* reg_NL3 is AT. */
	.set	noat
	lui	gp, %hi(_gp_disp)
	addiu	gp, %lo(_gp_disp)
	lui	reg_NL3, %hi(call_into_c)
	addiu	reg_NL3, %lo(call_into_c)
	addu	gp, reg_NL3
	.set	at
	.set	reorder

	/* Setup the lisp stack. */
	move	reg_OCFP, reg_CFP
	move	reg_CFP, reg_CSP
	addu	reg_CSP, reg_CFP, 32

	/* Mark us as in C land. */
	sw	reg_CSP, foreign_function_call_active

	/* Set the pseudo-atomic flag. */
	.set	noreorder
	li	reg_NL4, 0
	addu	reg_ALLOC, 1
	.set	reorder

	/* Convert the return address to an offset and save it on the stack. */
	subu	reg_NFP, reg_LIP, reg_CODE
	addu	reg_NFP, OTHER_POINTER_LOWTAG
        sw      reg_OCFP, (reg_CFP)
	sw	reg_NFP, 4(reg_CFP)
	sw	reg_CODE, 8(reg_CFP)
	sw	gp, 12(reg_CFP) // should this be saved? on the control stack?

	/* Save LISP state. */
	subu	reg_A0, reg_ALLOC, 1
	sw	reg_A0, dynamic_space_free_pointer
	sw	reg_BSP, current_binding_stack_pointer
	sw	reg_CSP, current_control_stack_pointer
	sw	reg_CFP, current_control_frame_pointer

	/* Check for interrupt */
	.set	noreorder
	bgez	reg_NL4, 1f
	 subu	reg_ALLOC, 1
	break	0x0, 0x10
1:	.set	reorder

	/* Into C land we go. */
	move    t9, reg_CFUNC		# reg_ALLOC
	jalr	t9

	lw	gp, 12(reg_CFP)

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

	/* Turn on pseudo-atomic. */
	.set	noreorder
	li	reg_NL4, 0
	li	reg_ALLOC, 1
	.set	reorder

	/* Load the allocation pointer, preserving the low-bit of alloc */
	lw	reg_BSP, dynamic_space_free_pointer
	addu	reg_ALLOC, reg_BSP

	lw	reg_BSP, current_binding_stack_pointer

	/* Restore CODE */
	lw	reg_CODE, 8(reg_CFP)
        /* Setup the return address */
	subu	reg_LIP, reg_NFP, OTHER_POINTER_LOWTAG
	addu	reg_LIP, reg_CODE

	/* Check for interrupt */
	.set	noreorder
	bgez	reg_NL4, 1f
	 subu	reg_ALLOC, 1
	break	0x0, 0x10
1:	.set	reorder

	/* Reset the lisp stack. */
	/* Note: OCFP and CFP are in saved regs. */
	move	reg_CSP, reg_CFP
	move	reg_CFP, reg_OCFP

	/* Mark us as in Lisp land. */
	sw	zero, foreign_function_call_active

	/* Return to LISP. */
	jr	reg_LIP
	END(call_into_c)

/*
 * Trampolines follow the Lisp calling convention.
 *
 * The undefined-function trampoline.
 */
	.align	3 /* minimum alignment for a lisp object */
	.word	SIMPLE_FUN_HEADER_WIDETAG /* header */
	.word	undefined_tramp - SIMPLE_FUN_CODE_OFFSET /* self */
	.word	NIL /* next */
	.word   NIL /* name */
	.word	NIL /* arglist */
	.word	NIL /* type */
	.word	NIL /* xrefs */
	LEAF(undefined_tramp)
	/* Point reg_CODE to the header and tag it as function, since
	   the debugger regards a function pointer in reg_CODE which
	   doesn't point to a code object as undefined function.  */
	lui	reg_CODE, %hi(undefined_tramp)
	addiu	reg_CODE, %lo(undefined_tramp)
	addiu	reg_CODE, -SIMPLE_FUN_CODE_OFFSET

	/* If we are called with stack arguments (or in a tail-call
	   scenario), we end up with an allocated stack frame, but the
	   frame link information is uninitialized.  Fix things by
	   allocating and initializing our stack frame "properly". */
	addiu reg_NL0,reg_NARGS,-24
	bltz reg_NL0,1f
	addu reg_CSP,reg_CFP,reg_NARGS
	addiu reg_CSP,reg_CFP,24
1:	sw reg_OCFP,0(reg_CFP)
	sw reg_LRA,4(reg_CFP)

	/* Now that the preliminaries are dealt with, actually trap. */
	.set	noreorder
	b	1f
	 break	0x0, trap_Cerror
	/* Error data length. */
	.byte	4
	/* Error number. */
	.byte	UNDEFINED_FUN_ERROR
	/* Magic value 254 means a 16bit little endian value follows.
	   See debug-var-io.lisp. */
	.byte	254
	/* reg_FDEFN is #14. */
	.byte	((14 << 6) + sc_DescriptorReg) % 0x100
	.byte	((14 << 6) + sc_DescriptorReg) / 0x100
	.align	2
	.set	reorder
1:	lw	reg_CODE, FDEFN_FUN_OFFSET(reg_FDEFN)
	lw	reg_LIP, SIMPLE_FUN_CODE_OFFSET(reg_CODE)
	jr	reg_LIP
	END(undefined_tramp)

/*
 * The closure trampoline.
 */
 	.align	5 /* common MIPS cacheline size */
	.word	0 /* pad 1 */
	.word	0 /* pad 2 */
	.word	SIMPLE_FUN_HEADER_WIDETAG /* header */
	.word	closure_tramp - SIMPLE_FUN_CODE_OFFSET /* self */
	.word	NIL /* next */
	.word   NIL /* name */
	.word	NIL /* arglist */
	.word	NIL /* type */
	.word	NIL /* xrefs */
	LEAF(closure_tramp)
        lw	reg_LEXENV, FDEFN_FUN_OFFSET(reg_FDEFN)
        lw	reg_CODE, CLOSURE_FUN_OFFSET(reg_LEXENV)
        addu	reg_LIP, reg_CODE, SIMPLE_FUN_CODE_OFFSET
        jr	reg_LIP
	END(closure_tramp)

/*
 * The trampoline for funcallable instances
 */
	.globl funcallable_instance_tramp
	.align	3
	.word	SIMPLE_FUN_HEADER_WIDETAG
funcallable_instance_tramp = . + 1
	.word	funcallable_instance_tramp
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL

	lw	reg_LEXENV, FUNCALLABLE_INSTANCE_FUNCTION_OFFSET(reg_LEXENV)
	lw	reg_CODE, CLOSURE_FUN_OFFSET(reg_LEXENV)
	addu	reg_LIP, reg_CODE, SIMPLE_FUN_CODE_OFFSET
	jr	reg_LIP
	nop

/*
 * Function-end breakpoint magic. This is truely magic, the code is
 * copied and has to be relocatable. It also needs a properly aligned
 * header tag after the fun_end_breakpoint_guts symbol.
 */

/*
 * For an explanation of the magic involved in function-end
 * breakpoints, see the implementation in ppc-assem.S.
 */

	.align  3 /* minimum alignment for a lisp object */
	LEAF(fun_end_breakpoint_guts)
	.set    noreorder
	.word   RETURN_PC_HEADER_WIDETAG + 0x600
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
