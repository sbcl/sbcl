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
#include "validate.h"
#include "genesis/closure.h"
#include "genesis/static-symbols.h"

        .text
        .align 2

        .global  fun_end_breakpoint_guts
        .type fun_end_breakpoint_guts,%function
fun_end_breakpoint_guts:
        addi.d $r20, $r0, 2                        # li   $tmp 2
        bge $r18, $r20, multiple_value_return      # bge  $nargs $tmp multiple_value_return
        addi.d $r14, $r23, 0                       # move $ocfp $csp
        addi.d $r23, $r23, N_WORD_BYTES            # addi $cfp $cfp N_WORD_BYTES
        addi.d $r18, $r0, (1 << N_FIXNUM_TAG_BITS) # li   $nargs (1 << N_FIXNUM_TAG_BITS)
        addi.d $r5, $r16, 0                        # move $a1 $NULL
        addi.d $r6, $r16, 0                        # move $a2 $NULL
        addi.d $r7, $r16, 0                        # move $a3 $NULL

multiple_value_return:

        .global fun_end_breakpoint_trap
        .type fun_end_breakpoint_trap,%function

fun_end_breakpoint_trap:
        break   trap_FunEndBreakpoint

        .global  fun_end_breakpoint_end
fun_end_breakpoint_end:

        .align 2
        .global do_pending_interrupt
        .type do_pending_interrupt,%function
do_pending_interrupt:
        break   trap_PendingInterrupt
        .byte   trap_PendingInterrupt
        .align 2
        ret
