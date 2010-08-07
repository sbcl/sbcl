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
    ppc_ss_struct_t *state = &context->uc_mcontext->PPC_DARWIN_REGIFY(ss);
    switch(offset) {
    case 0:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r0);
    case 1:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r1);
    case 2:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r2);
    case 3:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r3);
    case 4:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r4);
    case 5:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r5);
    case 6:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r6);
    case 7:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r7);
    case 8:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r8);
    case 9:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r9);
    case 10:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r10);
    case 11:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r11);
    case 12:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r12);
    case 13:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r13);
    case 14:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r14);
    case 15:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r15);
    case 16:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r16);
    case 17:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r17);
    case 18:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r18);
    case 19:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r19);
    case 20:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r20);
    case 21:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r21);
    case 22:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r22);
    case 23:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r23);
    case 24:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r24);
    case 25:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r25);
    case 26:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r26);
    case 27:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r27);
    case 28:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r28);
    case 29:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r29);
    case 30:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r30);
    case 31:
        return (os_context_register_t *) &state->PPC_DARWIN_REGIFY(r31);
    case 41:
        /* PT_DAR */
        return (os_context_register_t *) &context->uc_mcontext->PPC_DARWIN_REGIFY(es).PPC_DARWIN_REGIFY(dar);
    case 42:
        /* PT_DSISR */
        return (os_context_register_t *) &context->uc_mcontext->PPC_DARWIN_REGIFY(es).PPC_DARWIN_REGIFY(dsisr);
    default:
        lose("bad offset to os_context_register_addr");
    }
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return (os_context_register_t *) &context->uc_mcontext->PPC_DARWIN_REGIFY(ss).PPC_DARWIN_REGIFY(lr);
}

os_context_register_t *
os_context_ctr_addr(os_context_t *context)
{
    return (os_context_register_t *) &context->uc_mcontext->PPC_DARWIN_REGIFY(ss).PPC_DARWIN_REGIFY(ctr);
}

os_context_register_t *
os_context_cr_addr(os_context_t *context)
{
    return (os_context_register_t *) &context->uc_mcontext->PPC_DARWIN_REGIFY(ss).PPC_DARWIN_REGIFY(cr);
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
  return &context->uc_mcontext->PPC_DARWIN_REGIFY(ss).PPC_DARWIN_REGIFY(srr0);
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    /* see ppc-arch.c */
    ppc_flush_icache(address,length);
}

