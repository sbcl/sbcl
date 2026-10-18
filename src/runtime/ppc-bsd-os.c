#include <signal.h>
#include <machine/cpu.h>
#include <errno.h>
#include "genesis/sbcl.h"
#include "interr.h"
#include "runtime.h"
#include "thread.h"


os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
#if defined(LISP_FEATURE_NETBSD)
    return &context->uc_mcontext.__gregs[offset];
#elif defined(LISP_FEATURE_OPENBSD)
    return &context->sc_frame.fixreg[offset];
#elif defined(LISP_FEATURE_FREEBSD)
    return &context->uc_mcontext.mc_frame[offset];
#endif
}

#if defined(ARCH_HAS_STACK_POINTER) /* It's not defined on PPC. */
os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
#if defined(LISP_FEATURE_NETBSD)
    return &(_UC_MACHINE_SP(context));
#elif defined(LISP_FEATURE_FREEBSD)
    return &context->uc_mcontext.mc_frame[1];
#endif
}
#endif

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
#if defined(LISP_FEATURE_NETBSD)
    return &context->uc_mcontext.__gregs[_REG_LR];
#elif defined(LISP_FEATURE_OPENBSD)
    return &context->sc_frame.lr;
#elif defined(LISP_FEATURE_FREEBSD)
    return &context->uc_mcontext.mc_lr;
#endif
}

os_context_register_t *
os_context_ctr_addr(os_context_t *context)
{
#if defined(LISP_FEATURE_NETBSD)
    return &context->uc_mcontext.__gregs[_REG_CTR];
#elif defined(LISP_FEATURE_OPENBSD)
    return &context->sc_frame.ctr;
#elif defined(LISP_FEATURE_FREEBSD)
    return &context->uc_mcontext.mc_ctr;
#endif
}

os_context_register_t *
os_context_cr_addr(os_context_t *context)
{
#if defined(LISP_FEATURE_NETBSD)
    return &context->uc_mcontext.__gregs[_REG_CR];
#elif defined(LISP_FEATURE_OPENBSD)
    return &context->sc_frame.cr;
#elif defined(LISP_FEATURE_FREEBSD)
    return &context->uc_mcontext.mc_cr;
#endif
}

#ifdef LISP_FEATURE_FREEBSD
os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t *)&context->uc_mcontext.mc_fpreg[offset];
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    return context->uc_mcontext.mc_fpreg[32];
}

void
os_restore_fp_control(os_context_t *context)
{
    unsigned long long control;
    double d;
    control = os_context_fp_control(context) &
        (FLOAT_TRAPS_BYTE_MASK | FLOAT_ROUNDING_MODE_MASK);
    /* Clear exception bits from the saved context so sigreturn doesn't
     * restore OX=1 + OE=1 which would re-trigger SIGFPE. */
    context->uc_mcontext.mc_fpreg[32] = control;
    d = *((double *) &control);
    asm volatile ("mtfsf 0xff,%0" : : "f" (d));
}
#endif

/* FIXME: If this can be a no-op on BSD/x86, then it
 * deserves a more precise name.
 *
 * (Perhaps os_prepare_data_area_to_be_executed()?) */
void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
   ppc_flush_icache(address, length);
}

int arch_os_thread_init(struct thread __attribute__((unused)) *thread) {
    return 1;                  /* success */
}

int arch_os_thread_cleanup(struct thread __attribute__((unused)) *thread) {
    return 1;                  /* success */
}
