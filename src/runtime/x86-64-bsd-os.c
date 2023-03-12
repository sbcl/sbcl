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

#ifdef LISP_FEATURE_NETBSD
#define _REG_rax _REG_RAX
#define _REG_rcx _REG_RCX
#define _REG_rdx _REG_RDX
#define _REG_rbx _REG_RBX
#define _REG_rsp _REG_RSP
#define _REG_rbp _REG_RBP
#define _REG_rsi _REG_RSI
#define _REG_rdi _REG_RDI
#define _REG_r8  _REG_R8
#define _REG_r9  _REG_R9
#define _REG_r10 _REG_R10
#define _REG_r11 _REG_R11
#define _REG_r12 _REG_R12
#define _REG_r13 _REG_R13
#define _REG_r14 _REG_R14
#define _REG_r15 _REG_R15
#endif

void visit_context_registers(void (*proc)(os_context_register_t,int), os_context_t *context)
{
    proc(OS_CONTEXT_PC(context), 1);
    proc(CONTEXT_SLOT(context, rax), 1); proc(CONTEXT_SLOT(context, r8),  1);
    proc(CONTEXT_SLOT(context, rcx), 1); proc(CONTEXT_SLOT(context, r9),  1);
    proc(CONTEXT_SLOT(context, rdx), 1); proc(CONTEXT_SLOT(context, r10), 1);
    proc(CONTEXT_SLOT(context, rbx), 1); proc(CONTEXT_SLOT(context, r11), 1);
    /* ignore rsp */                     proc(CONTEXT_SLOT(context, r12), 1);
    /* ignore rbp */                     proc(CONTEXT_SLOT(context, r13), 1);
    proc(CONTEXT_SLOT(context, rsi), 1); proc(CONTEXT_SLOT(context, r14), 1);
    proc(CONTEXT_SLOT(context, rdi), 1); proc(CONTEXT_SLOT(context, r15), 1);
}

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

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

int arch_os_thread_init(struct thread *thread) {
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    stack_t sigstack;
    sigstack.ss_sp    = calc_altstack_base(thread);
    sigstack.ss_flags = 0;
    sigstack.ss_size  = calc_altstack_size(thread);
    sigaltstack(&sigstack,0);
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
