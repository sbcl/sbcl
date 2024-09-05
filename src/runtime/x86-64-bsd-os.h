#ifndef _X86_64_BSD_OS_H
#define _X86_64_BSD_OS_H

#ifdef LISP_FEATURE_FREEBSD
#include <machine/fpu.h>
#endif

#ifdef LISP_FEATURE_DRAGONFLY
#include <machine/npx.h>
#endif

#ifdef LISP_FEATURE_NETBSD
typedef unsigned long os_context_register_t;
#else
typedef register_t os_context_register_t;
#endif

/* The different BSD variants have diverged in exactly where they
 * store signal context information, but at least they tend to use the
 * same stems to name the structure fields, so by using this macro we
 * can share a fair amount of code between different variants. */
#if defined LISP_FEATURE_FREEBSD || defined(__DragonFly__)
#define CONTEXT_SLOT(c,stem) context->uc_mcontext.mc_ ## stem
#elif defined(__OpenBSD__)
#define CONTEXT_SLOT(c,stem) context->sc_ ## stem
#elif defined __NetBSD__
#define CONTEXT_SLOT(c,stem) ((context)->uc_mcontext.__gregs[_REG_ ## stem])
#else
#error unsupported BSD variant
#endif
// "&" binds weaker than {->,.,[]} so this does what we want
// Note that this macro is unhygienic.
#define CONTEXT_ADDR_FROM_STEM(stem) &CONTEXT_SLOT(context,stem)

#if defined LISP_FEATURE_DRAGONFLY
/* I am not sure if following definition is needed after this:
   http://gitweb.dragonflybsd.org/dragonfly.git/commit/e6e019a801e99ba7888ed009c5c3b3c7b047af1e

   But It will not harm if I leave it here. */
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);

#define X86_64_SIGFPE_FIXUP

static inline unsigned int *
arch_os_context_mxcsr_addr(os_context_t *context)
{
    struct envxmm *ex = (struct envxmm *)(&context->uc_mcontext.mc_fpregs);
    return &ex->en_mxcsr;
}
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

#if defined LISP_FEATURE_FREEBSD || defined LISP_FEATURE_OPENBSD || defined LISP_FEATURE_DRAGONFLY
#  define OS_CONTEXT_PC(context) CONTEXT_SLOT(context,rip)
#elif defined LISP_FEATURE_NETBSD
#  define OS_CONTEXT_PC(context) CONTEXT_SLOT(context,RIP)
#endif

#endif /* _X86_64_BSD_OS_H */
