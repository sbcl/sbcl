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
#include <sys/socket.h>
#include <sys/utsname.h>
#include <machine/sysarch.h>

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "validate.h"
size_t os_vm_page_size;

#ifdef LISP_FEATURE_SB_THREAD
#error "Define threading support functions"
#else
int arch_os_thread_init(struct thread *thread) {
    stack_t sigstack;
    /* Signal handlers are normally run on the main stack, but we've
     * swapped stacks, require that the control stack contain only
     * boxed data, and expands upwards while the C stack expands
     * downwards. */
    sigstack.ss_sp=((void *) thread)+dynamic_values_bytes;
    sigstack.ss_flags=0;
    sigstack.ss_size = 32*SIGSTKSZ;
    if(sigaltstack(&sigstack,0)<0)
        lose("Cannot sigaltstack: %s\n",strerror(errno));

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
    return &context->uc_mcontext.__gregs[offset];
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

void
os_restore_fp_control(os_context_t *context)
{
    /* FIXME: Implement. */
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    arm_sync_icache(address, length);
}

static void
sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    unsigned int code = *((unsigned char *)(4+*os_context_pc_addr(context)));
    u32 trap_instruction = *((u32 *)*os_context_pc_addr(context));

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
    undoably_install_low_level_interrupt_handler(SIGTRAP, sigtrap_handler);
}
