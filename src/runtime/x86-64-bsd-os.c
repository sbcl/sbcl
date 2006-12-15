#include <signal.h>
#include "sbcl.h"
#include "runtime.h"
#include "thread.h"
#include "lispregs.h"

#if defined(LISP_FEATURE_FREEBSD)
#include <machine/fpu.h>
#endif

/* KLUDGE: There is strong family resemblance in the signal context
 * stuff in FreeBSD and OpenBSD, but in detail they're different in
 * almost every line of code. It would be nice to find some way to
 * factor out the commonality better; failing that, it might be best
 * just to split this generic-BSD code into one variant for each BSD.
 *
 * KLUDGE II: this split has begun with the addition of the Darwin BSD
 * flavour, with the cross-architecture complications that this
 * entails; unfortunately, currently the situation is worse, not
 * better, than in the above paragraph. */

#if defined(LISP_FEATURE_FREEBSD)
os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case reg_RAX:
        return CONTEXT_ADDR_FROM_STEM(rax);
    case reg_RCX:
        return CONTEXT_ADDR_FROM_STEM(rcx);
    case reg_RDX:
        return CONTEXT_ADDR_FROM_STEM(rdx);
    case reg_RBX:
        return CONTEXT_ADDR_FROM_STEM(rbx);
    case reg_RSP:
        return CONTEXT_ADDR_FROM_STEM(rsp);
    case reg_RBP:
        return CONTEXT_ADDR_FROM_STEM(rbp);
    case reg_RSI:
        return CONTEXT_ADDR_FROM_STEM(rsi);
    case reg_RDI:
        return CONTEXT_ADDR_FROM_STEM(rdi);
    case reg_R8:
        return CONTEXT_ADDR_FROM_STEM(r8);
    case reg_R9:
        return CONTEXT_ADDR_FROM_STEM(r9);
    case reg_R10:
        return CONTEXT_ADDR_FROM_STEM(r10);
    case reg_R11:
        return CONTEXT_ADDR_FROM_STEM(r11);
    case reg_R12:
        return CONTEXT_ADDR_FROM_STEM(r12);
    case reg_R13:
        return CONTEXT_ADDR_FROM_STEM(r13);
    case reg_R14:
        return CONTEXT_ADDR_FROM_STEM(r14);
    case reg_R15:
        return CONTEXT_ADDR_FROM_STEM(r15);
    default:
        return 0;
    }
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return CONTEXT_ADDR_FROM_STEM(rsp);
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return CONTEXT_ADDR_FROM_STEM(rip);
}

#endif

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

int arch_os_thread_init(struct thread *thread) {
    stack_t sigstack;
#ifdef LISP_FEATURE_SB_THREAD
    pthread_setspecific(specials,thread);
#endif
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
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

#if defined(LISP_FEATURE_FREEBSD)
void
os_restore_fp_control(os_context_t *context)
{
    struct envxmm *ex = (struct envxmm*)(&context->uc_mcontext.mc_fpstate);
    /* reset exception flags and restore control flags on SSE2 FPU */
    unsigned int temp = (ex->en_mxcsr) & ~0x3F;
    asm ("ldmxcsr %0" : : "m" (temp));
    /* same for x87 FPU. */
    asm ("fldcw %0" : : "m" (ex->en_cw));
}
#endif
