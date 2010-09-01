#ifndef _X86_64_BSD_OS_H
#define _X86_64_BSD_OS_H

#ifdef LISP_FEATURE_FREEBSD
#include <machine/fpu.h>
#endif

typedef register_t os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

/* The different BSD variants have diverged in exactly where they
 * store signal context information, but at least they tend to use the
 * same stems to name the structure fields, so by using this macro we
 * can share a fair amount of code between different variants. */
#if defined __FreeBSD__
#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext.mc_ ## stem
#elif defined(__OpenBSD__)
#define CONTEXT_ADDR_FROM_STEM(stem) &context->sc_ ## stem
#elif defined __NetBSD__
#define CONTEXT_ADDR_FROM_STEM(stem) &((context)->uc_mcontext.__gregs[_REG_ ## stem])
#else
#error unsupported BSD variant
#endif

#if defined LISP_FEATURE_FREEBSD
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);

/* FreeBSD does not setup si_code on XMM exception. */
#define X86_64_SIGFPE_FIXUP

static inline unsigned int *
arch_os_context_mxcsr_addr(os_context_t *context)
{
    struct envxmm *ex = (struct envxmm *)(&context->uc_mcontext.mc_fpstate);
    return &ex->en_mxcsr;
}
#endif

#if defined LISP_FEATURE_OPENBSD
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);
#endif

#endif /* _X86_64_BSD_OS_H */
