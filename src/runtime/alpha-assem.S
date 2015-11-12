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
	
#include "validate.h"		
#include <alpha/regdef.h>
#ifdef linux
#include <asm/pal.h> 
#else
#include <alpha/pal.h>
#endif
#include "sbcl.h"
#include "lispregs.h"
#include "genesis/fdefn.h"
#include "genesis/closure.h"
#include "genesis/funcallable-instance.h"
#include "genesis/simple-fun.h"
#include "genesis/static-symbols.h"

/* #include "globals.h" */
	
/*
 * Function to transfer control into lisp.
 */
	.text
	.align	4
	.globl	call_into_lisp
	.ent	call_into_lisp
call_into_lisp:
#define framesize 8*8
	ldgp    gp, 0($27)                  
	/* Save all the C regs. */
	lda	sp,-framesize(sp)
	stq	ra, framesize-8*8(sp)
	stq	s0, framesize-8*7(sp)
	stq	s1, framesize-8*6(sp)
	stq	s2, framesize-8*5(sp)
	stq	s3, framesize-8*4(sp)
	stq	s4, framesize-8*3(sp)
	stq	s5, framesize-8*2(sp)
	stq	s6, framesize-8*1(sp)
	.mask	0x0fc001fe, -framesize
	.frame	sp,framesize,ra

	/* Clear descriptor regs */
	ldil	reg_CODE,0
	ldil	reg_FDEFN,0
	mov	a0,reg_LEXENV
	sll	a2,2,reg_NARGS
	ldil	reg_OCFP,0
	ldil	reg_LRA,0
	ldil	reg_L0,0
	ldil	reg_L1,0
	

	/* Establish NIL. */
	ldil	reg_NULL,NIL

	/* The CMUCL comment here is "Start pseudo-atomic.", but */
	/* there's no obvious code that would have that effect  */

	/* No longer in foreign call. */
	stl	zero,foreign_function_call_active

	/* Load lisp state. */
	ldq	reg_ALLOC,dynamic_space_free_pointer
	ldq	reg_BSP,current_binding_stack_pointer
	ldq	reg_CSP,current_control_stack_pointer
	ldq	reg_OCFP,current_control_frame_pointer
	mov	a1,reg_CFP

	.set	noat
	ldil	reg_L2,0
	.set at

	/* End of pseudo-atomic. */

	/* Establish lisp arguments. */
	ldl	reg_A0,0(reg_CFP)
	ldl	reg_A1,4(reg_CFP)
	ldl	reg_A2,8(reg_CFP)
	ldl	reg_A3,12(reg_CFP)
	ldl	reg_A4,16(reg_CFP)
	ldl	reg_A5,20(reg_CFP)

	/* This call will 'return' into the LRA page below */
	lda	reg_LRA,call_into_lisp_LRA_page+OTHER_POINTER_LOWTAG

	/* Indirect the closure */
	ldl	reg_CODE, CLOSURE_FUN_OFFSET(reg_LEXENV)
	addl	reg_CODE, SIMPLE_FUN_CODE_OFFSET, reg_LIP

	/* And into lisp we go. */
	jsr	reg_ZERO,(reg_LIP)

	
	/* a page of the following code (from call_into_lisp_LRA
	onwards) is copied into the LRA page at arch_init() time. */
	
	.set noreorder
	.align	3
	.globl	call_into_lisp_LRA
call_into_lisp_LRA:	

	.long	RETURN_PC_HEADER_WIDETAG

	/* execution resumes here*/
	mov	reg_OCFP,reg_CSP
	nop

	/* return value already there */
	mov	reg_A0,v0

	/* Turn on pseudo-atomic. */

	/* Save LISP registers */
	stq	reg_ALLOC, dynamic_space_free_pointer 
	stq	reg_BSP,current_binding_stack_pointer
	stq	reg_CSP,current_control_stack_pointer
	stq	reg_CFP,current_control_frame_pointer
	
	/* Back in C land.  [CSP is just a handy non-zero value.] */
	stl	reg_CSP,foreign_function_call_active
	
	/* Turn off pseudo-atomic and check for traps. */
	
	/* Restore C regs */
	ldq	ra, framesize-8*8(sp)
	ldq	s0, framesize-8*7(sp)
	ldq	s1, framesize-8*6(sp)
	ldq	s2, framesize-8*5(sp)
	ldq	s3, framesize-8*4(sp)
	ldq	s4, framesize-8*3(sp)
	ldq	s5, framesize-8*2(sp)
	ldq	s6, framesize-8*1(sp)

	/* Restore the C stack! */
	lda	sp, framesize(sp)

	ret	zero,(ra),1
	.globl	call_into_lisp_end
call_into_lisp_end:
	.end	call_into_lisp

/*
 * Transfering control from Lisp into C.  reg_CFUNC (t10, 24) contains
 * the address of the C function to call
 */
	.set noreorder
	.text
	.align	4
	.globl	call_into_c
	.ent	call_into_c
call_into_c:
	.mask	0x0fc001fe, -12
	.frame	sp,12,ra
	mov	reg_CFP, reg_OCFP
	mov	reg_CSP, reg_CFP
	addq	reg_CFP, 32, reg_CSP
	stl	reg_OCFP, 0(reg_CFP)
	subl	reg_LIP, reg_CODE, reg_L1
	addl	reg_L1, OTHER_POINTER_LOWTAG, reg_L1
	stl	reg_L1, 4(reg_CFP)
	stl	reg_CODE, 8(reg_CFP)
	stl	reg_NULL, 12(reg_CFP)

	/* Set the pseudo-atomic flag. */
	addq	reg_ALLOC,1,reg_ALLOC

	/* Get the top two register args and fix the NSP to point to arg 7 */
	ldq	reg_NL4,0(reg_NSP)
	ldq	reg_NL5,8(reg_NSP)
	addq	reg_NSP,16,reg_NSP

	/* Save lisp state. */
	subq	reg_ALLOC,1,reg_L1
	stq	reg_L1, dynamic_space_free_pointer
	
	stq	reg_BSP, current_binding_stack_pointer
	stq	reg_CSP, current_control_stack_pointer
	stq	reg_CFP, current_control_frame_pointer

	/* Mark us as in C land. */
	stl	reg_CSP, foreign_function_call_active

	/* Were we interrupted? */
	subq	reg_ALLOC,1,reg_ALLOC
	stl	reg_ZERO,0(reg_ALLOC)

	/* Into C land we go. */

	mov	reg_CFUNC, reg_L1    /* L1=pv: this is a hint to the cache */
				  
	jsr	ra, (reg_CFUNC)
	ldgp	$29,0(ra)

	/* restore NSP */
	subq	reg_NSP,16,reg_NSP

	/* Clear unsaved descriptor regs */
	mov	reg_ZERO, reg_NARGS
	mov	reg_ZERO, reg_A0
	mov	reg_ZERO, reg_A1
	mov	reg_ZERO, reg_A2
	mov	reg_ZERO, reg_A3
	mov	reg_ZERO, reg_A4
	mov	reg_ZERO, reg_A5
	mov	reg_ZERO, reg_L0
	.set noat
	mov	reg_ZERO, reg_L2
	.set at
	
	/* Turn on pseudo-atomic. */
	lda	reg_ALLOC,1(reg_ZERO)

	/* Mark us at in Lisp land. */
	stl	reg_ZERO, foreign_function_call_active

	/* Restore ALLOC, preserving pseudo-atomic-atomic */
	ldq	reg_NL0,dynamic_space_free_pointer
	addq	reg_ALLOC,reg_NL0,reg_ALLOC
	
	/* Check for interrupt */
	subq	reg_ALLOC,1,reg_ALLOC
	stl	reg_ZERO,0(reg_ALLOC)

	ldl	reg_NULL, 12(reg_CFP)

	/* Restore LRA & CODE (they may have been GC'ed) */
	/* can you see anything here which touches LRA?  I can't ...*/
	ldl	reg_CODE, 8(reg_CFP)
	ldl	reg_NL0, 4(reg_CFP)
	subq	reg_NL0, OTHER_POINTER_LOWTAG, reg_NL0
	addq	reg_CODE, reg_NL0, reg_NL0

	mov	reg_CFP, reg_CSP
	mov	reg_OCFP, reg_CFP

	ret	zero, (reg_NL0), 1

	.end	call_into_c

	.text
	.globl	start_of_tramps
start_of_tramps:

/*
 * The undefined-function trampoline.  Causes a trap_Error trap which
 * sigtrap_handler catches and eventaully calls the Lisp
 * INTERNAL-ERROR function
 */
        .text
	.globl	start_of_tramps
	.globl	undefined_tramp
	.globl	undefined_tramp_offset
        .ent    undefined_tramp_offset
        .long   SIMPLE_FUN_HEADER_WIDETAG                       /* header */
        .long   undefined_tramp - SIMPLE_FUN_CODE_OFFSET        /* self */
        .long   NIL                                             /* next */
        .long   NIL                                             /* name */
        .long   NIL                                             /* arglist */
        .long   NIL                                             /* type */
        .long   NIL                                             /* xref */
undefined_tramp_offset:
	/* an explanation is called for here.  0x140 is the difference
	 * between undefined_tramp_offset and call_into_lisp_LRA, but
	 * the assembler is too dumb to allow that as an expression.
	 * So, change this number whenever you add or remove any code
	 * in this file */

undefined_tramp= call_into_lisp_LRA_page+0x15c
        subl    reg_LIP, SIMPLE_FUN_CODE_OFFSET, reg_CODE

	/* If we are called with stack arguments (or in a tail-call
	   scenario), we end up with an allocated stack frame, but the
	   frame link information is uninitialized.  Fix things by
	   allocating and initializing our stack frame "properly". */
	cmpule  reg_NARGS,24,reg_NL0
	beq     reg_NL0,1f
	addq    reg_CFP,24,reg_CSP
	br      reg_ZERO,2f
1:	addq    reg_CFP,reg_NARGS,reg_CSP
2:	stl     reg_OCFP,0(reg_CFP)
	stl     reg_LRA,4(reg_CFP)

	call_pal PAL_bugchk
	.long    trap_Error
        .byte    4                         /* what are these numbers? */
        .byte    UNDEFINED_FUN_ERROR
        .byte    254
        .byte    (0xc0 + sc_DescriptorReg)
        .byte    5
        .align 2
        .end     undefined_tramp_offset


/* The closure trampoline. */
        .text
        .globl  closure_tramp
        .globl  closure_tramp_offset
        .ent    closure_tramp_offset
closure_tramp_offset:
closure_tramp= call_into_lisp_LRA_page+0x18c
        ldl     reg_LEXENV, FDEFN_FUN_OFFSET(reg_FDEFN)
        ldl     reg_L0, CLOSURE_FUN_OFFSET(reg_LEXENV)
        addl    reg_L0, SIMPLE_FUN_CODE_OFFSET, reg_LIP
        jmp     reg_ZERO,(reg_LIP)
        .end    closure_tramp_offset

	.text
	.globl	end_of_tramps
end_of_tramps:

	.text
	.globl funcallable_instance_tramp
	.align 2
	.long SIMPLE_FUN_HEADER_WIDETAG
funcallable_instance_tramp = . + 1
	.long funcallable_instance_tramp
	.long NIL
	.long NIL
	.long NIL
	.long NIL
	.long NIL

	ldl reg_LEXENV, FUNCALLABLE_INSTANCE_FUNCTION_OFFSET(reg_LEXENV)
	/* I think we don't actually need to use reg_CODE here, because
	   $CODE is computed from $LIP in the function itself */
	ldl reg_CODE, CLOSURE_FUN_OFFSET(reg_LEXENV) 
	addl reg_CODE, SIMPLE_FUN_CODE_OFFSET, reg_LIP
	jmp reg_ZERO, (reg_LIP)

/*
 * fun-end breakpoint magic.
 */

/*
 * For an explanation of the magic involved in function-end
 * breakpoints, see the implementation in ppc-assem.S.
 */

	.text
	.align	2
	.set	noreorder
	.globl	fun_end_breakpoint_guts
fun_end_breakpoint_guts:
	.long	RETURN_PC_HEADER_WIDETAG + 0x600
	br	zero, fun_end_breakpoint_trap
	nop
	mov	reg_CSP, reg_OCFP
	addl	reg_CSP, 4, reg_CSP
	addl	zero, 4, reg_NARGS
	mov	reg_NULL, reg_A1
	mov	reg_NULL, reg_A2
	mov	reg_NULL, reg_A3
	mov	reg_NULL, reg_A4
	mov	reg_NULL, reg_A5
1:

	.globl	fun_end_breakpoint_trap
fun_end_breakpoint_trap:
	call_pal PAL_bugchk
	.long	trap_FunEndBreakpoint
	br	zero, fun_end_breakpoint_trap

	.globl	fun_end_breakpoint_end
fun_end_breakpoint_end:

