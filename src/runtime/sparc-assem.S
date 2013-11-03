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
	.byte	254, sc_DescriptorReg, 6
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

/*
 * For an explanation of the magic involved in function-end
 * breakpoints, see the implementation in ppc-assem.S.
 */

	.text
	.align	8
	.global	fun_end_breakpoint_guts
fun_end_breakpoint_guts:
	.word	RETURN_PC_HEADER_WIDETAG + 0x800
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

	.global do_pending_interrupt
	FUNCDEF(do_pending_interrupt)
do_pending_interrupt:
        unimp	trap_PendingInterrupt
        retl
        nop

/*
 * Save the FPU state.  %o0 contains a pointer to where we can
 * store our state.
 */

/*
 * Note we only save the 16 double-float registers (which saves
 * the 32 single-float values too, I think).  If we're compiling for
 * a sparc v9, the Lisp code can actually use all 32 double-float
 * registers.  For later.
 */
	.global fpu_save
	FUNCDEF(fpu_save)
fpu_save:
	std	%f0, [%o0 + 4*0]
	std	%f2, [%o0 + 4*2]
	std	%f4, [%o0 + 4*4]
	std	%f6, [%o0 + 4*6]
	std	%f8, [%o0 + 4*8]
	std	%f10, [%o0 + 4*10]
	std	%f12, [%o0 + 4*12]
	std	%f14, [%o0 + 4*14]
	std	%f16, [%o0 + 4*16]
	std	%f18, [%o0 + 4*18]
	std	%f20, [%o0 + 4*20]
	std	%f22, [%o0 + 4*22]
	std	%f24, [%o0 + 4*24]
	std	%f26, [%o0 + 4*26]
	std	%f28, [%o0 + 4*28]
	std	%f30, [%o0 + 4*30]
#ifdef FEATURE_SPARC_V9
	std	%f32, [%o0 + 4*32]
	std	%f34, [%o0 + 4*34]
	std	%f36, [%o0 + 4*36]
	std	%f38, [%o0 + 4*38]
	std	%f40, [%o0 + 4*40]
	std	%f42, [%o0 + 4*42]
	std	%f44, [%o0 + 4*44]
	std	%f46, [%o0 + 4*46]
	std	%f48, [%o0 + 4*48]
	std	%f50, [%o0 + 4*50]
	std	%f52, [%o0 + 4*52]
	std	%f54, [%o0 + 4*54]
	std	%f56, [%o0 + 4*56]
	std	%f58, [%o0 + 4*58]
	std	%f60, [%o0 + 4*60]
	std	%f62, [%o0 + 4*62]
	st	%fsr, [%o0 + 4*64]
#else
	st	%fsr, [%o0 + 4*32]
#endif
	retl
	nop

	.global fpu_restore
	FUNCDEF(fpu_restore)
fpu_restore:
	ldd	[%o0 + 4*0], %f0
	ldd	[%o0 + 4*2], %f2
	ldd	[%o0 + 4*4], %f4
	ldd	[%o0 + 4*6], %f6
	ldd	[%o0 + 4*8], %f8
	ldd	[%o0 + 4*10], %f10
	ldd	[%o0 + 4*12], %f12
	ldd	[%o0 + 4*14], %f14
	ldd	[%o0 + 4*16], %f16
	ldd	[%o0 + 4*18], %f18
	ldd	[%o0 + 4*20], %f20
	ldd	[%o0 + 4*22], %f22
	ldd	[%o0 + 4*24], %f24
	ldd	[%o0 + 4*26], %f26
	ldd	[%o0 + 4*28], %f28
	ldd	[%o0 + 4*30], %f30
#ifdef FEATURE_SPARC_V9
	ldd	[%o0 + 4*32], %f32
	ldd	[%o0 + 4*34], %f34
	ldd	[%o0 + 4*36], %f36
	ldd	[%o0 + 4*38], %f38
	ldd	[%o0 + 4*40], %f40
	ldd	[%o0 + 4*42], %f42
	ldd	[%o0 + 4*44], %f44
	ldd	[%o0 + 4*46], %f46
	ldd	[%o0 + 4*48], %f48
	ldd	[%o0 + 4*50], %f50
	ldd	[%o0 + 4*52], %f52
	ldd	[%o0 + 4*54], %f54
	ldd	[%o0 + 4*56], %f56
	ldd	[%o0 + 4*58], %f58
	ldd	[%o0 + 4*60], %f60
	ldd	[%o0 + 4*62], %f62
	ld	[%o0 + 4*64], %fsr
#else
	ld	[%o0 + 4*32], %fsr
#endif
	retl
	nop

	.global save_context
	FUNCDEF(save_context)
save_context:
	ta	ST_FLUSH_WINDOWS	! flush register windows
	retl				! return from leaf routine
	nop
