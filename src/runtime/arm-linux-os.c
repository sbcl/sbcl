/*
 * This is the ARM Linux incarnation of arch-dependent OS-dependent
 * routines. See also "linux-os.c".
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

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include "sbcl.h"
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "validate.h"

#ifdef LISP_FEATURE_SB_THREAD
#error "Define threading support functions"
#else
int arch_os_thread_init(struct thread *thread) {
    stack_t sigstack;
    /* Signal handlers are normally run on the main stack, but we've
     * swapped stacks, require that the control stack contain only
     * boxed data, and expands upwards while the C stack expands
     * downwards. */
    sigstack.ss_sp    = calc_altstack_base(thread);
    sigstack.ss_flags = 0;
    sigstack.ss_size  = calc_altstack_size(thread);
    if(sigaltstack(&sigstack,0)<0)
        lose("Cannot sigaltstack: %s",strerror(errno));

    return 1;                   /* success */
}
int arch_os_thread_cleanup(struct thread *thread) {
    return 1;                   /* success */
}
#endif

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    /* KLUDGE: All of the context registers are stored in the
     * structure consecutively, in order, as separately-named fields.
     * Rather than do a big switch/case and all that, just take the
     * address of the first one (R0) and treat it as the start of an
     * array. */
    return (os_context_register_t*)&(&context->uc_mcontext.arm_r0)[offset];
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return os_context_register_addr(context, reg_PC);
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return os_context_register_addr(context, reg_LR);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &(context->uc_sigmask);
}

void
os_restore_fp_control(os_context_t *context)
{
    /* FIXME: Implement. */
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    os_vm_address_t end_address
        = (os_vm_address_t)(((uintptr_t) address) + length);
    __clear_cache(address, end_address);
}

static void
sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    unsigned int code = *((unsigned char *)(4+*os_context_pc_addr(context)));
    uint32_t trap_instruction = *((uint32_t *)*os_context_pc_addr(context));

    if (trap_instruction != 0xe7f001f0) {
        lose("Unrecognized trap instruction %08lx in sigtrap_handler()",
             trap_instruction);
    }

    if (code == trap_PendingInterrupt) {
      arch_skip_instruction(context);
    }

    handle_trap(context, code);
}

void arch_install_interrupt_handlers()
{
    ll_install_handler(SIGTRAP, sigtrap_handler);
}
