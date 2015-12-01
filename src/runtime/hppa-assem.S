#define LANGUAGE_ASSEMBLY

#include "sbcl.h"
#include "lispregs.h"
#include "genesis/closure.h"
#include "genesis/fdefn.h"
#include "genesis/simple-fun.h"
#include "genesis/return-pc.h"
#include "genesis/static-symbols.h"
#include "genesis/funcallable-instance.h"

        .level  2.0
        .text

	.import $global$,data
        .import $$dyncall,MILLICODE
	.import foreign_function_call_active,data
	.import current_control_stack_pointer,data
	.import current_control_frame_pointer,data
	.import current_binding_stack_pointer,data
	.import dynamic_space_free_pointer,data
/*	.import return_from_lisp_function,data */


/*
 * Call-into-lisp
 */

	.export call_into_lisp
call_into_lisp:
	.proc
	.callinfo entry_gr=18,save_rp
	.entry
	/* %arg0=function, %arg1=cfp, %arg2=nargs */

        stw     %rp,-0x14(%sr0,%sp)
        stwm    %r3,0x40(%sr0,%sp)
        stw     %r4,-0x3c(%sr0,%sp)
        stw     %r5,-0x38(%sr0,%sp)
        stw     %r6,-0x34(%sr0,%sp)
        stw     %r7,-0x30(%sr0,%sp)
        stw     %r8,-0x2c(%sr0,%sp)
        stw     %r9,-0x28(%sr0,%sp)
        stw     %r10,-0x24(%sr0,%sp)
        stw     %r11,-0x20(%sr0,%sp)
        stw     %r12,-0x1c(%sr0,%sp)
        stw     %r13,-0x18(%sr0,%sp)
        stw     %r14,-0x14(%sr0,%sp)
        stw     %r15,-0x10(%sr0,%sp)
        stw     %r16,-0xc(%sr0,%sp)
        stw     %r17,-0x8(%sr0,%sp)
        stw     %r18,-0x4(%sr0,%sp)

	/* Clear the descriptor regs, moving in args as approporate. */
	copy	%r0,reg_CODE
	copy	%r0,reg_FDEFN
	copy	%arg0,reg_LEXENV
	zdep	%arg2,29,30,reg_NARGS
	copy	%r0,reg_OCFP
	copy	%r0,reg_LRA
	copy	%r0,reg_A0
	copy	%r0,reg_A1
	copy	%r0,reg_A2
	copy	%r0,reg_A3
	copy	%r0,reg_A4
	copy	%r0,reg_A5
	copy	%r0,reg_L0
	copy	%r0,reg_L1
	copy	%r0,reg_L2

	/* Establish NIL. */
	ldil	L%NIL,reg_NULL
	ldo	R%NIL(reg_NULL),reg_NULL

	/* Turn on pseudo-atomic. */
	ldo	4(%r0),reg_ALLOC

	/* No longer in foreign function call land. */
	addil	L%foreign_function_call_active-$global$,%dp
	stw	%r0,R%foreign_function_call_active-$global$(0,%r1)

	/* Load lisp state. */
	addil	L%dynamic_space_free_pointer-$global$,%dp
	ldw	R%dynamic_space_free_pointer-$global$(0,%r1),%r1
	add	reg_ALLOC,%r1,reg_ALLOC
	addil	L%current_binding_stack_pointer-$global$,%dp
	ldw	R%current_binding_stack_pointer-$global$(0,%r1),reg_BSP
	addil	L%current_control_stack_pointer-$global$,%dp
	ldw	R%current_control_stack_pointer-$global$(0,%r1),reg_CSP
	addil	L%current_control_frame_pointer-$global$,%dp
	ldw	R%current_control_frame_pointer-$global$(0,%r1),reg_OCFP
	copy	%arg1,reg_CFP

	/* End of pseudo-atomic. */
	addit,od	-4,reg_ALLOC,reg_ALLOC

	/* Establish lisp arguments. */
	ldw	0(reg_CFP),reg_A0
	ldw	4(reg_CFP),reg_A1
	ldw	8(reg_CFP),reg_A2
	ldw	12(reg_CFP),reg_A3
	ldw	16(reg_CFP),reg_A4
	ldw	20(reg_CFP),reg_A5

	/* Calculate the LRA. */
  ldil  L%lra-RETURN_PC_RETURN_POINT_OFFSET,reg_LRA
  ldo R%lra-RETURN_PC_RETURN_POINT_OFFSET(reg_LRA),reg_LRA

	/* Indirect the closure */
	ldw	CLOSURE_FUN_OFFSET(0,reg_LEXENV),reg_CODE
  addi  SIMPLE_FUN_CODE_OFFSET,reg_CODE,reg_LIP

#ifdef LISP_FEATURE_HPUX
  /*  Get the stub address, ie assembly-routine return-from-lisp */
  addil   L%return_from_lisp_stub-$global$,%dp
  ldw     R%return_from_lisp_stub-$global$(0,%r1),reg_NL0
  be,n  0(%sr5,reg_NL0)
#else
  be,n  0(%sr5,reg_LIP)
#endif

	break	0,0

	.align	8
lra:
  nop /* a few nops because we dont know where we land */
  nop /* the return convention would govern this */
  nop
  nop

	/* Copy CFP (%r4) into someplace else and restore r4. */
	copy	reg_CFP,reg_NL1
  ldw -0x3c(0,%sp),%r4

	/* Copy the return value. */
	copy	reg_A0,%ret0

	/* Turn on pseudo-atomic. */
	addi	4,reg_ALLOC,reg_ALLOC

	/* Store the lisp state. */
	copy	reg_ALLOC,reg_NL0
	depi	0,31,3,reg_NL0
	addil	L%dynamic_space_free_pointer-$global$,%dp
	stw	reg_NL0,R%dynamic_space_free_pointer-$global$(0,%r1)
	addil	L%current_binding_stack_pointer-$global$,%dp
	stw	reg_BSP,R%current_binding_stack_pointer-$global$(0,%r1)
	addil	L%current_control_stack_pointer-$global$,%dp
	stw	reg_CSP,R%current_control_stack_pointer-$global$(0,%r1)
	addil	L%current_control_frame_pointer-$global$,%dp
	stw	reg_NL1,R%current_control_frame_pointer-$global$(0,%r1)

	/* Back in C land.  [CSP is just a handy non-zero value.] */
	addil	L%foreign_function_call_active-$global$,%dp
	stw	reg_CSP,R%foreign_function_call_active-$global$(0,%r1)

	/* Turn off pseudo-atomic and check for traps. */
	addit,od	-4,reg_ALLOC,reg_ALLOC

        ldw     -0x54(%sr0,%sp),%rp
        ldw     -0x4(%sr0,%sp),%r18
        ldw     -0x8(%sr0,%sp),%r17
        ldw     -0xc(%sr0,%sp),%r16
        ldw     -0x10(%sr0,%sp),%r15
        ldw     -0x14(%sr0,%sp),%r14
        ldw     -0x18(%sr0,%sp),%r13
        ldw     -0x1c(%sr0,%sp),%r12
        ldw     -0x20(%sr0,%sp),%r11
        ldw     -0x24(%sr0,%sp),%r10
        ldw     -0x28(%sr0,%sp),%r9
        ldw     -0x2c(%sr0,%sp),%r8
        ldw     -0x30(%sr0,%sp),%r7
        ldw     -0x34(%sr0,%sp),%r6
        ldw     -0x38(%sr0,%sp),%r5
        ldw     -0x3c(%sr0,%sp),%r4
        bv      %r0(%rp)
        ldwm    -0x40(%sr0,%sp),%r3

	/* And thats all. */
	.exit
	.procend


/*
 * Call-into-C
 */

	.export call_into_c
call_into_c:
	/* Set up a lisp stack frame. */
	copy	reg_CFP, reg_OCFP
	copy	reg_CSP, reg_CFP
	addi	32, reg_CSP, reg_CSP
	stw	reg_OCFP, 0(0,reg_CFP) ; save old cfp
	stw	reg_CFP, 4(0,reg_CFP)  ; save old csp
        /* convert raw return PC into a fixnum PC-offset, because we dont
           have ahold of an lra object */
	sub	reg_LIP, reg_CODE, reg_NL5
	addi	3-OTHER_POINTER_LOWTAG, reg_NL5, reg_NL5
	stw	reg_NL5, 8(0,reg_CFP)
	stw	reg_CODE, 0xc(0,reg_CFP)

	/* set pseudo-atomic flag */
	addi	4, reg_ALLOC, reg_ALLOC

	/* Store the lisp state. */
	copy	reg_ALLOC,reg_NL5
	depi	0,31,3,reg_NL5
	addil	L%dynamic_space_free_pointer-$global$,%dp
	stw	reg_NL5,R%dynamic_space_free_pointer-$global$(0,%r1)
	addil	L%current_binding_stack_pointer-$global$,%dp
	stw	reg_BSP,R%current_binding_stack_pointer-$global$(0,%r1)
	addil	L%current_control_stack_pointer-$global$,%dp
	stw	reg_CSP,R%current_control_stack_pointer-$global$(0,%r1)
	addil	L%current_control_frame_pointer-$global$,%dp
	stw	reg_CFP,R%current_control_frame_pointer-$global$(0,%r1)

	/* Back in C land.  [CSP is just a handy non-zero value.] */
	addil	L%foreign_function_call_active-$global$,%dp
	stw	reg_CSP,R%foreign_function_call_active-$global$(0,%r1)

	/* Turn off pseudo-atomic and check for traps. */
	addit,od	-4,reg_ALLOC,reg_ALLOC

	/* in order to be able to call incrementally linked (ld -A) functions,
	   we have to do some mild trickery here */
        copy reg_CFUNC, %r22
        bl      $$dyncall,%r31
        copy    %r31, %r2
call_into_c_return:
	/* Clear the callee saves descriptor regs. */
	copy	%r0, reg_A5
	copy	%r0, reg_L0
	copy	%r0, reg_L1
	copy	%r0, reg_L2

	/* Turn on pseudo-atomic. */
	ldi	4, reg_ALLOC

	/* Turn off foreign function call. */
	addil	L%foreign_function_call_active-$global$,%dp
	stw	%r0,R%foreign_function_call_active-$global$(0,%r1)

	/* Load ALLOC. */
	addil	L%dynamic_space_free_pointer-$global$,%dp
	ldw	R%dynamic_space_free_pointer-$global$(0,%r1),%r1
	add	reg_ALLOC,%r1,reg_ALLOC

	/* We don't need to load OCFP, CFP, CSP, or BSP because they are
	 * in caller saves registers.
	 */

	/* End of pseudo-atomic. */
	addit,od	-4,reg_ALLOC,reg_ALLOC

	/* Restore CODE.  Even though it is in a callee saves register
	 * it might have been GC'ed.
	 */
	ldw	0xc(0,reg_CFP), reg_CODE

	/* Restore the return pc. */
	ldw	8(0,reg_CFP), reg_NL0
	addi	OTHER_POINTER_LOWTAG-3, reg_NL0, reg_NL0
/*
	addi	-3, reg_NL0, reg_NL0
        ldi OTHER_POINTER_LOWTAG, reg_NL1
	sub	reg_NL0, reg_NL1, reg_NL0
*/
	add	reg_CODE, reg_NL0, reg_LIP

	/* Pop the lisp stack frame, and back we go. */
	ldw	4(0,reg_CFP), reg_CSP
	ldw	0(0,reg_CFP), reg_OCFP
	copy	reg_OCFP, reg_CFP
	be	0(5,reg_LIP)
        nop


/*
 * Stuff to sanctify a block of memory for execution.
 */

	.EXPORT sanctify_for_execution
sanctify_for_execution:
	.proc
	.callinfo
	.entry
	/* %arg0=start addr, %arg1=length in bytes */
	add	%arg0,%arg1,%arg1
        copy	%arg0,%arg2
	ldo	-1(%arg1),%arg1
	depi	0,31,5,%arg0
	depi	0,31,5,%arg1
	ldsid	(%arg0),%r1
	mtsp	%r1,%sr1
	ldi	32,%r1			; bytes per cache line
        /* parisc 1.1 and 2.0 manuals say to flush the dcache, SYNC,
	 * flush the icache, SYNC again, and burn seven instructions
	 * before executing modified code. */
sanctify_loop:
	comb,<	%arg0,%arg1,sanctify_loop
	fdc,m	%r1(%sr1,%arg0)
        sync
sanctify_loop_2:
	comb,<	%arg2,%arg1,sanctify_loop_2
	fic,m	%r1(%sr1,%arg2)
        sync

	bv	%r0(%rp)
	nop

	.exit
	.procend


/*
 * Core saving/restoring support
 */

	.export call_on_stack
call_on_stack:
	/* %arg0 = fn to invoke, %arg1 = new stack base */

	/* Compute the new stack pointer. */
	addi	64,%arg1,%sp

	/* Zero out the previous stack pointer. */
	stw	%r0,-4(0,%sp)

	/* Invoke the function. */
	ble	0(4,%arg0)
	copy	%r31, %r2

	/* Flame out. */
	break	0,0

	.export	save_state
save_state:
	.proc
	.callinfo entry_gr=18,entry_fr=21,save_rp,calls
	.entry

	stw	%rp,-0x14(%sr0,%sp)
	fstds,ma	%fr12,8(%sr0,%sp)
	fstds,ma	%fr13,8(%sr0,%sp)
	fstds,ma	%fr14,8(%sr0,%sp)
	fstds,ma	%fr15,8(%sr0,%sp)
	fstds,ma	%fr16,8(%sr0,%sp)
	fstds,ma	%fr17,8(%sr0,%sp)
	fstds,ma	%fr18,8(%sr0,%sp)
	fstds,ma	%fr19,8(%sr0,%sp)
	fstds,ma	%fr20,8(%sr0,%sp)
	fstds,ma	%fr21,8(%sr0,%sp)
	stwm	%r3,0x70(%sr0,%sp)
	stw	%r4,-0x6c(%sr0,%sp)
	stw	%r5,-0x68(%sr0,%sp)
	stw	%r6,-0x64(%sr0,%sp)
	stw	%r7,-0x60(%sr0,%sp)
	stw	%r8,-0x5c(%sr0,%sp)
	stw	%r9,-0x58(%sr0,%sp)
	stw	%r10,-0x54(%sr0,%sp)
	stw	%r11,-0x50(%sr0,%sp)
	stw	%r12,-0x4c(%sr0,%sp)
	stw	%r13,-0x48(%sr0,%sp)
	stw	%r14,-0x44(%sr0,%sp)
	stw	%r15,-0x40(%sr0,%sp)
	stw	%r16,-0x3c(%sr0,%sp)
	stw	%r17,-0x38(%sr0,%sp)
	stw	%r18,-0x34(%sr0,%sp)


	/* Remember the function we want to invoke */
	copy	%arg0,%r19

	/* Pass the new stack pointer in as %arg0 */
	copy	%sp,%arg0

	/* Leave %arg1 as %arg1. */

	/* do the call. */
	ble	0(4,%r19)
	copy	%r31, %r2

	.export _restore_state
_restore_state:

	ldw	-0xd4(%sr0,%sp),%rp
	ldw	-0x34(%sr0,%sp),%r18
	ldw	-0x38(%sr0,%sp),%r17
	ldw	-0x3c(%sr0,%sp),%r16
	ldw	-0x40(%sr0,%sp),%r15
	ldw	-0x44(%sr0,%sp),%r14
	ldw	-0x48(%sr0,%sp),%r13
	ldw	-0x4c(%sr0,%sp),%r12
	ldw	-0x50(%sr0,%sp),%r11
	ldw	-0x54(%sr0,%sp),%r10
	ldw	-0x58(%sr0,%sp),%r9
	ldw	-0x5c(%sr0,%sp),%r8
	ldw	-0x60(%sr0,%sp),%r7
	ldw	-0x64(%sr0,%sp),%r6
	ldw	-0x68(%sr0,%sp),%r5
	ldw	-0x6c(%sr0,%sp),%r4
	ldwm	-0x70(%sr0,%sp),%r3
	fldds,mb	-8(%sr0,%sp),%fr21
	fldds,mb	-8(%sr0,%sp),%fr20
	fldds,mb	-8(%sr0,%sp),%fr19
	fldds,mb	-8(%sr0,%sp),%fr18
	fldds,mb	-8(%sr0,%sp),%fr17
	fldds,mb	-8(%sr0,%sp),%fr16
	fldds,mb	-8(%sr0,%sp),%fr15
	fldds,mb	-8(%sr0,%sp),%fr14
	fldds,mb	-8(%sr0,%sp),%fr13
	bv	%r0(%rp)
	fldds,mb	-8(%sr0,%sp),%fr12


	.exit
	.procend

	.export	restore_state
restore_state:
	.proc
	.callinfo
	copy	%arg0,%sp
	b	_restore_state
	copy	%arg1,%ret0
	.procend



/* FIX, add support for singlestep
	break	trap_SingleStepBreakpoint,0
	break	trap_SingleStepBreakpoint,0
*/
	.export SingleStepTraps
SingleStepTraps:

/* Missing !! NOT
	there's a break 0,0 in the new version here!!!
*/

/*
 * For an explanation of the magic involved in function-end
 * breakpoints, see the implementation in ppc-assem.S.
 */

	.align	8
	.export fun_end_breakpoint_guts
fun_end_breakpoint_guts:
	.word	RETURN_PC_HEADER_WIDETAG + 0x600
	/* multiple value return point -- just jump to trap. */
	b,n	fun_end_breakpoint_trap
	/* single value return point -- convert to multiple w/ n=1 */
	copy	reg_CSP, reg_OCFP
	addi	4, reg_CSP, reg_CSP
	addi	4, %r0, reg_NARGS
	copy	reg_NULL, reg_A1
	copy	reg_NULL, reg_A2
	copy	reg_NULL, reg_A3
	copy	reg_NULL, reg_A4
	copy	reg_NULL, reg_A5

	.export	fun_end_breakpoint_trap
fun_end_breakpoint_trap:
	break	trap_FunEndBreakpoint,0
	b,n	fun_end_breakpoint_trap

	.export	fun_end_breakpoint_end
fun_end_breakpoint_end:

/* FIX-lav: these are found in assem-rtns.lisp too, but
   genesis.lisp has problem referencing them, so we keep
   these old versions too.  Lisp code cant jump to them
   because it is an inter space jump but lisp do intra
   space jumps */

	.align	8
        .EXPORT closure_tramp
closure_tramp:
        /* reg_FDEFN holds the fdefn object. */
        ldw     FDEFN_FUN_OFFSET(0,reg_FDEFN),reg_LEXENV
        ldw     CLOSURE_FUN_OFFSET(0,reg_LEXENV),reg_L0
        addi    SIMPLE_FUN_CODE_OFFSET, reg_L0, reg_LIP
        bv,n    0(reg_LIP)

	.align	8
        .EXPORT undefined_tramp
undefined_tramp:
        break   trap_Error,0
        .byte   4
        .byte   UNDEFINED_FUN_ERROR
        .byte   254
        .byte   (0x40 + sc_DescriptorReg)
        .byte   2
        .align  4

