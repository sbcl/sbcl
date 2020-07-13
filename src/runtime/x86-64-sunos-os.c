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

#include <errno.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>


#include "validate.h"


int arch_os_thread_init(struct thread *thread) {
#if defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    stack_t sigstack;
    sigstack.ss_sp    = calc_altstack_base(thread);
    sigstack.ss_flags = 0;
    sigstack.ss_size  = calc_altstack_size(thread);
    if (sigaltstack(&sigstack,0) < 0) {
        lose("Cannot sigaltstack: %s",strerror(errno));
    }
#endif
    return 1;                  /* success */
}

int arch_os_thread_cleanup(struct thread *thread) {
    return 1;
}


os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
#define RCASE(name) case reg_ ## name: return &context->uc_mcontext.gregs[REG_ ## name];
    switch(offset) {
        RCASE(RAX)
        RCASE(RCX)
        RCASE(RDX)
        RCASE(RBX)
        RCASE(RSP)
        RCASE(RBP)
        RCASE(RSI)
        RCASE(RDI)
        RCASE(R8)
        RCASE(R9)
        RCASE(R10)
        RCASE(R11)
        RCASE(R12)
        RCASE(R13)
        RCASE(R14)
        RCASE(R15)
      default:
        if(offset<NGREG)
            return &context->uc_mcontext.gregs[offset/2+4];
        else return 0;
    }
    return &context->uc_mcontext.gregs[offset];
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[REG_RIP];
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[REG_RSP];
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &(context->uc_sigmask);
}

void os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

unsigned long
os_context_fp_control(os_context_t *context)
{
  return context->uc_mcontext.fpregs.fp_reg_set.fpchip_state.cw;
}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    fpregset_t *fp = &context->uc_mcontext.fpregs;

    return (os_context_register_t *)&fp->fp_reg_set.fpchip_state.xmm[offset];
}
