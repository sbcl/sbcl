#include "os.h"
#include "gencgc-alloc-region.h"
#include "thread.h"
#include "genesis/thread.h"
#include <signal.h>

int arch_os_thread_init(struct thread __attribute__((unused)) *thread) {
    return 1;
}

int arch_os_thread_cleanup(struct thread __attribute__((unused)) *thread) {
    return 1;
}

os_context_register_t *os_context_pc_addr(os_context_t *context) {
    return (os_context_register_t *)&context->uc_mcontext.rip;
}
os_context_register_t *os_context_sp_addr(os_context_t *context) {
    return (os_context_register_t *)&context->uc_mcontext.rsp;
}

os_context_register_t*
os_context_register_addr(os_context_t *context, int reg_encoding)
{
#define OFFSET_OF(r) offsetof(ucontext_t, uc_mcontext.r)
     static unsigned int offset_of[16] = {
       OFFSET_OF(rax), OFFSET_OF(rcx), OFFSET_OF(rdx), OFFSET_OF(rbx),
       OFFSET_OF(rsp), OFFSET_OF(rbp), OFFSET_OF(rsi), OFFSET_OF(rdi),
       OFFSET_OF(r8 ), OFFSET_OF(r9 ), OFFSET_OF(r10), OFFSET_OF(r11),
       OFFSET_OF(r12), OFFSET_OF(r13), OFFSET_OF(r14), OFFSET_OF(r15),
    };
#undef OFFSET_OF
    if (reg_encoding >= 0 && reg_encoding < 16)
        return (os_context_register_t*)((char*)context + offset_of[reg_encoding]);
    return 0;
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

void
os_restore_fp_control(os_context_t *context)
{
    // just guessing here

    /* reset exception flags and restore control flags on SSE2 FPU */
    unsigned int temp = (context->uc_mcontext.fpu.fp_fxsave.mxcsr) & ~0x3F;
    asm ("ldmxcsr %0" : : "m" (temp));
    /* same for x87 FPU. */
    asm ("fldcw %0" : : "m" (context->uc_mcontext.fpu.fp_fxsave.control));
}

void
os_flush_icache(os_vm_address_t __attribute__((unused)) address,
                os_vm_size_t __attribute__((unused)) length)
{
}
