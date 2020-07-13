#include <signal.h>
#include "sbcl.h"
#include "runtime.h"
#include "thread.h"
#include "lispregs.h"

#if defined(LISP_FEATURE_FREEBSD)
#include <machine/fpu.h>
#endif

#if defined(LISP_FEATURE_OPENBSD)
#include <machine/fpu.h>
#endif

#if defined(LISP_FEATURE_DRAGONFLY)
#include <machine/npx.h>
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

#if defined(LISP_FEATURE_FREEBSD) || defined(LISP_FEATURE_DARWIN) || defined(LISP_FEATURE_OPENBSD) || defined(LISP_FEATURE_DRAGONFLY)
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
os_context_fp_addr(os_context_t *context)
{
    return CONTEXT_ADDR_FROM_STEM(rbp);
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return CONTEXT_ADDR_FROM_STEM(rip);
}

#elif defined(LISP_FEATURE_NETBSD)
os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case reg_RAX:
        return CONTEXT_ADDR_FROM_STEM(RAX);
    case reg_RCX:
        return CONTEXT_ADDR_FROM_STEM(RCX);
    case reg_RDX:
        return CONTEXT_ADDR_FROM_STEM(RDX);
    case reg_RBX:
        return CONTEXT_ADDR_FROM_STEM(RBX);
    case reg_RSP:
        return CONTEXT_ADDR_FROM_STEM(RSP);
    case reg_RBP:
        return CONTEXT_ADDR_FROM_STEM(RBP);
    case reg_RSI:
        return CONTEXT_ADDR_FROM_STEM(RSI);
    case reg_RDI:
        return CONTEXT_ADDR_FROM_STEM(RDI);
    case reg_R8:
        return CONTEXT_ADDR_FROM_STEM(R8);
    case reg_R9:
        return CONTEXT_ADDR_FROM_STEM(R9);
    case reg_R10:
        return CONTEXT_ADDR_FROM_STEM(R10);
    case reg_R11:
        return CONTEXT_ADDR_FROM_STEM(R11);
    case reg_R12:
        return CONTEXT_ADDR_FROM_STEM(R12);
    case reg_R13:
        return CONTEXT_ADDR_FROM_STEM(R13);
    case reg_R14:
        return CONTEXT_ADDR_FROM_STEM(R14);
    case reg_R15:
        return CONTEXT_ADDR_FROM_STEM(R15);
    default:
        return 0;
    }
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return CONTEXT_ADDR_FROM_STEM(RSP);
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return CONTEXT_ADDR_FROM_STEM(RIP);
}

#endif

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

int arch_os_thread_init(struct thread *thread) {
#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
    mach_lisp_thread_init(thread);
#elif defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    stack_t sigstack;
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

#if defined(LISP_FEATURE_DRAGONFLY)
void
os_restore_fp_control(os_context_t *context)
{
    struct envxmm *ex = (struct envxmm*)(&context->uc_mcontext.mc_fpregs);
    /* reset exception flags and restore control flags on SSE2 FPU */
    unsigned int temp = (ex->en_mxcsr) & ~0x3F;
    asm ("ldmxcsr %0" : : "m" (temp));
    /* same for x87 FPU. */
    asm ("fldcw %0" : : "m" (ex->en_cw));
}
#endif

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

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t *)&((struct savefpu*)&context->uc_mcontext.mc_fpstate)->sv_xmm[offset];
}

#endif

#if defined(LISP_FEATURE_OPENBSD)
void
os_restore_fp_control(os_context_t *context)
{
    if (context->sc_fpstate != NULL) {
        u_int32_t mxcsr = context->sc_fpstate->fx_mxcsr & ~0x3F;
        u_int16_t cw = context->sc_fpstate->fx_fcw;
        asm ("ldmxcsr %0" : : "m" (mxcsr));
        asm ("fldcw %0" : : "m" (cw));
    }
}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t *)&context->sc_fpstate->fx_xmm[offset];
}
#endif
