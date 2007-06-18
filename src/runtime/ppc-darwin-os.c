/*
 * This is the PowerPC/Darwin incarnation of arch-dependent
 * OS-dependent routines. See also "bsdos.c".
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
#include "globals.h"
#include "runtime.h"
#include <signal.h>
#include <ucontext.h>
#include <limits.h>
#include <mach-o/dyld.h>
#include "arch.h"
#include "interr.h"                     /* for declaration of lose */

#ifdef LISP_FEATURE_SB_THREAD
#error "Define threading support functions"
#else
int arch_os_thread_init(struct thread *thread) {
    return 1;                   /* success */
}
int arch_os_thread_cleanup(struct thread *thread) {
    return 1;                   /* success */
}
#endif

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    ppc_saved_state_t *state = &context->uc_mcontext->ss;
    switch(offset) {
    case 0:
        return (os_context_register_t *) &state->r0;
    case 1:
        return (os_context_register_t *) &state->r1;
    case 2:
        return (os_context_register_t *) &state->r2;
    case 3:
        return (os_context_register_t *) &state->r3;
    case 4:
        return (os_context_register_t *) &state->r4;
    case 5:
        return (os_context_register_t *) &state->r5;
    case 6:
        return (os_context_register_t *) &state->r6;
    case 7:
        return (os_context_register_t *) &state->r7;
    case 8:
        return (os_context_register_t *) &state->r8;
    case 9:
        return (os_context_register_t *) &state->r9;
    case 10:
        return (os_context_register_t *) &state->r10;
    case 11:
        return (os_context_register_t *) &state->r11;
    case 12:
        return (os_context_register_t *) &state->r12;
    case 13:
        return (os_context_register_t *) &state->r13;
    case 14:
        return (os_context_register_t *) &state->r14;
    case 15:
        return (os_context_register_t *) &state->r15;
    case 16:
        return (os_context_register_t *) &state->r16;
    case 17:
        return (os_context_register_t *) &state->r17;
    case 18:
        return (os_context_register_t *) &state->r18;
    case 19:
        return (os_context_register_t *) &state->r19;
    case 20:
        return (os_context_register_t *) &state->r20;
    case 21:
        return (os_context_register_t *) &state->r21;
    case 22:
        return (os_context_register_t *) &state->r22;
    case 23:
        return (os_context_register_t *) &state->r23;
    case 24:
        return (os_context_register_t *) &state->r24;
    case 25:
        return (os_context_register_t *) &state->r25;
    case 26:
        return (os_context_register_t *) &state->r26;
    case 27:
        return (os_context_register_t *) &state->r27;
    case 28:
        return (os_context_register_t *) &state->r28;
    case 29:
        return (os_context_register_t *) &state->r29;
    case 30:
        return (os_context_register_t *) &state->r30;
    case 31:
        return (os_context_register_t *) &state->r31;
    case 41:
        /* PT_DAR */
        return (os_context_register_t *) &context->uc_mcontext->es.dar;
    case 42:
        /* PT_DSISR */
        return (os_context_register_t *) &context->uc_mcontext->es.dsisr;
    default:
        lose("bad offset to os_context_register_addr");
    }
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return (os_context_register_t *) &context->uc_mcontext->ss.lr;
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &context->uc_mcontext->ss.srr0;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    /* see ppc-arch.c */
    ppc_flush_icache(address,length);
}

