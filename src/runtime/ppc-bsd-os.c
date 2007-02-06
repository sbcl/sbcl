#include <signal.h>
#include <machine/cpu.h>
#include "sbcl.h"
#include "runtime.h"
#include "thread.h"


int *
os_context_register_addr(os_context_t *context, int offset)
{
    return &context->uc_mcontext.__gregs[offset];
}

int *
os_context_sp_addr(os_context_t *context)
{
    return &(_UC_MACHINE_SP(context));
}


int *
os_context_pc_addr(os_context_t *context)
{
    return &(_UC_MACHINE_PC(context));
}

int *
os_context_lr_addr(os_context_t *context)
{
    return &context->uc_mcontext.__gregs[_REG_LR];
}

/* FIXME: If this can be a no-op on BSD/x86, then it
 * deserves a more precise name.
 *
 * (Perhaps os_prepare_data_area_to_be_executed()?) */
void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
   ppc_flush_icache(address, length);
}

int arch_os_thread_init(struct thread *thread) {

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    stack_t sigstack;

    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp=((void *) thread)+dynamic_values_bytes;
    sigstack.ss_flags=0;
    sigstack.ss_size = 32*SIGSTKSZ;
    sigaltstack(&sigstack,0);
#endif

    return 1;                  /* success */
}

int arch_os_thread_cleanup(struct thread *thread) {

    return 1;                  /* success */
}
