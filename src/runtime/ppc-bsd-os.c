#include <signal.h>
#include <machine/cpu.h>
#include "sbcl.h"
#include "runtime.h"
#include "thread.h"


int *
os_context_register_addr(os_context_t *context, int offset)
{
#if defined(LISP_FEATURE_NETBSD)
    return &context->uc_mcontext.__gregs[offset];
#elif defined(LISP_FEATURE_OPENBSD)
    return &context->sc_frame.fixreg[offset];
#endif
}

#if defined(ARCH_HAS_STACK_POINTER) /* It's not defined on PPC. */
int *
os_context_sp_addr(os_context_t *context)
{
#if defined(LISP_FEATURE_NETBSD)
    return &(_UC_MACHINE_SP(context));
#endif
}
#endif


int *
os_context_pc_addr(os_context_t *context)
{
#if defined(LISP_FEATURE_NETBSD)
    return &(_UC_MACHINE_PC(context));
#elif defined(LISP_FEATURE_OPENBSD)
    return &context->sc_frame.srr0;
#endif
}

int *
os_context_lr_addr(os_context_t *context)
{
#if defined(LISP_FEATURE_NETBSD)
    return &context->uc_mcontext.__gregs[_REG_LR];
#elif defined(LISP_FEATURE_OPENBSD)
    return &context->sc_frame.lr;
#endif
}

os_context_register_t *
os_context_ctr_addr(os_context_t *context)
{
#if defined(LISP_FEATURE_NETBSD)
    return &context->uc_mcontext.__gregs[_REG_CTR];
#elif defined(LISP_FEATURE_OPENBSD)
    return &context->sc_frame.ctr;
#endif
}

os_context_register_t *
os_context_cr_addr(os_context_t *context)
{
#if defined(LISP_FEATURE_NETBSD)
    return &context->uc_mcontext.__gregs[_REG_CR];
#elif defined(LISP_FEATURE_OPENBSD)
    return &context->sc_frame.cr;
#endif
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
    sigstack.ss_sp    = calc_altstack_base(thread);
    sigstack.ss_flags = 0;
    sigstack.ss_size  = calc_altstack_size(thread);
    sigaltstack(&sigstack,0);
#endif
    return 1;                  /* success */
}

int arch_os_thread_cleanup(struct thread *thread) {
    return 1;                  /* success */
}
