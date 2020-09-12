/*
 * This is the ARM BSD incarnation of arch-dependent OS-dependent
 * routines. See also "bsd-os.c".
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

#if defined(LISP_FEATURE_NETBSD)

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    /* KLUDGE: All of the context registers are stored in the
     * structure consecutively, in order, as separately-named fields.
     * Rather than do a big switch/case and all that, just take the
     * address of the first one (R0) and treat it as the start of an
     * array. */
    return &context->uc_mcontext.__gregs[offset];
}

#elif defined(LISP_FEATURE_OPENBSD)

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    switch (offset) {
        case 0:       return (os_context_register_t *)(&context->sc_r0);
        case 1:       return (os_context_register_t *)(&context->sc_r1);
        case 2:       return (os_context_register_t *)(&context->sc_r2);
        case 3:       return (os_context_register_t *)(&context->sc_r3);
        case 4:       return (os_context_register_t *)(&context->sc_r4);
        case 5:       return (os_context_register_t *)(&context->sc_r5);
        case 6:       return (os_context_register_t *)(&context->sc_r6);
        case 7:       return (os_context_register_t *)(&context->sc_r7);
        case 8:       return (os_context_register_t *)(&context->sc_r8);
        case 9:       return (os_context_register_t *)(&context->sc_r9);
        case 10:      return (os_context_register_t *)(&context->sc_r10);
        case 11:      return (os_context_register_t *)(&context->sc_r11);
        case 12:      return (os_context_register_t *)(&context->sc_r12);
        case reg_NSP: return (os_context_register_t *)(&context->sc_usr_sp);
        case reg_LR:  return (os_context_register_t *)(&context->sc_usr_lr);
        case reg_PC:  return (os_context_register_t *)(&context->sc_pc);
    }
    lose("illegal register number: %d", offset);
}

#endif

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

#if defined(LISP_FEATURE_OPENBSD)

void
os_restore_fp_control(os_context_t *context)
{
    asm ("fmxr fpscr,%0" : : "r" (context->sc_fpscr));
}

#else

void
os_restore_fp_control(os_context_t *context)
{
    /* FIXME: Implement. */
}

#endif

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    arm_sync_icache(address, length);
}

static void
sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    unsigned int code = *((unsigned char *)(4+*os_context_pc_addr(context)));
    uint32_t trap_instruction = *((uint32_t *)*os_context_pc_addr(context));

    if (trap_instruction != 0xe7ffdefe) {
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
