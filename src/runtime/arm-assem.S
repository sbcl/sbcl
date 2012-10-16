#define LANGUAGE_ASSEMBLY

#include "lispregs.h"
#include "globals.h"
#include "sbcl.h"

#include "genesis/closure.h"
#include "genesis/funcallable-instance.h"
#include "genesis/fdefn.h"
#include "genesis/static-symbols.h"
#include "genesis/simple-fun.h"

	.align	2
	.global	call_into_lisp
	.type	call_into_lisp, %function
call_into_lisp:

	/* FIXME: Implement. */

	bx	lr
	.size	call_into_lisp, .-call_into_lisp

/* Trampolines, like on SPARC, use Lisp calling conventions. */
	.align	3
	.global	undefined_tramp
	.type	undefined_tramp, %function
	.word	SIMPLE_FUN_HEADER_WIDETAG
undefined_tramp = . + 1
	.word	undefined_tramp
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL

	/* FIXME: Implement. */

	.align	3
	.global	closure_tramp
	.type	closure_tramp, %function
	.word	SIMPLE_FUN_HEADER_WIDETAG
closure_tramp = . + 1
	.word	closure_tramp
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL

	ldr	reg_LEXENV, [reg_LEXENV, #FDEFN_FUN_OFFSET]
	ldr	reg_CODE, [reg_LEXENV, #CLOSURE_FUN_OFFSET]
	add	reg_PC, reg_CODE, #SIMPLE_FUN_CODE_OFFSET

	.align	3
	.global	funcallable_instance_tramp
	.type	funcallable_instance_tramp, %function
	.word	SIMPLE_FUN_HEADER_WIDETAG
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL
	.word	NIL

	ldr	reg_LEXENV, [reg_LEXENV, #FUNCALLABLE_INSTANCE_FUNCTION_OFFSET]
	ldr	reg_CODE, [reg_LEXENV, #CLOSURE_FUN_OFFSET]
	add	reg_PC, reg_CODE, #SIMPLE_FUN_CODE_OFFSET

	/* EOF */
