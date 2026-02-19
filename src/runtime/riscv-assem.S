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

#include "globals.h"
#include "lispregs.h"

        .text
        .align 2

        .global  fun_end_breakpoint_guts
        .type fun_end_breakpoint_guts,%function
fun_end_breakpoint_guts:
        bge reg_NARGS, zero, Lmultiple_value_return
        mv  reg_OCFP, reg_CSP
        addi reg_CSP, reg_CSP, N_WORD_BYTES
        li reg_NARGS, (1 << N_FIXNUM_TAG_BITS)
        mv reg_A1, reg_NULL
        mv reg_A2, reg_NULL
        mv reg_A3, reg_NULL

Lmultiple_value_return:

        .global fun_end_breakpoint_trap
        .type fun_end_breakpoint_trap,%function

fun_end_breakpoint_trap:
        ebreak   
        .byte trap_FunEndBreakpoint
        .byte 0
        .byte 0
        .byte 0

        .global  fun_end_breakpoint_end
fun_end_breakpoint_end:
