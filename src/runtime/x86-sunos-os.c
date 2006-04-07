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

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include "validate.h"
#ifdef LISP_FEATURE_SB_THREAD
#error "Define threading support functions"
#else
int arch_os_thread_init(struct thread *thread) {
  stack_t sigstack;
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp=((void *) thread)+dynamic_values_bytes;
    sigstack.ss_flags=0;
    sigstack.ss_size = 32*SIGSTKSZ;
    sigaltstack(&sigstack,0);
#endif
     return 1;                   /* success */
}
int arch_os_thread_cleanup(struct thread *thread) {
    return 1;                   /* success */
}
#endif

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    /* Solaris x86 holds %esp value in UESP */
    switch(offset) {
    case reg_EAX: return &context->uc_mcontext.gregs[11];
    case reg_ECX: return &context->uc_mcontext.gregs[10];
    case reg_EDX: return &context->uc_mcontext.gregs[9];
    case reg_EBX: return &context->uc_mcontext.gregs[8];
    case reg_ESP: return &context->uc_mcontext.gregs[17]; /* REG_UESP */
    case reg_EBP: return &context->uc_mcontext.gregs[6];
    case reg_ESI: return &context->uc_mcontext.gregs[5];
    case reg_EDI: return &context->uc_mcontext.gregs[4];
    default: return 0;
    }
    return &context->uc_mcontext.gregs[offset];
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &(context->uc_mcontext.gregs[14]); /* REG_EIP */
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return &(context->uc_mcontext.gregs[17]); /* REG_UESP */
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
    int *state = context->uc_mcontext.fpregs.fp_reg_set.fpchip_state.state;
    /* The STATE array is in the format used by the x86 instruction FNSAVE,
     * so the FPU control word is in the first 16 bits */
    int cw = (state[0] & 0xffff);
    int sw = context->uc_mcontext.fpregs.fp_reg_set.fpchip_state.status;
    return (cw ^ 0x3f) | (sw << 16);
}
