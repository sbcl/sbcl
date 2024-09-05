#ifndef _X86_BSD_OS_H
#define _X86_BSD_OS_H

#ifdef LISP_FEATURE_FREEBSD
#include <machine/segments.h>
#include <machine/cpufunc.h>
#endif

typedef int os_context_register_t;

/* The different BSD variants have diverged in exactly where they
 * store signal context information, but at least they tend to use the
 * same stems to name the structure fields, so by using this macro we
 * can share a fair amount of code between different variants. */
#if defined(LISP_FEATURE_FREEBSD) || defined(__DragonFly__)
#define CONTEXT_SLOT(c,stem) c->uc_mcontext.mc_ ## stem
#elif defined(__OpenBSD__)
#define CONTEXT_SLOT(c,stem) c->sc_ ## stem
#elif defined __NetBSD__
#define CONTEXT_SLOT(c,stem) ((c)->uc_mcontext.__gregs[_REG_ ## stem])
#else
#error unsupported BSD variant
#endif

// "&" binds weaker than {->,.,[]} so this does what we want
// Note that this macro is unhygienic.
#define CONTEXT_ADDR_FROM_STEM(stem) &CONTEXT_SLOT(context,stem)

#if defined LISP_FEATURE_FREEBSD
#if defined(LISP_FEATURE_RESTORE_TLS_SEGMENT_REGISTER_FROM_CONTEXT)
void os_restore_tls_segment_register(os_context_t *context);
#endif
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);
#endif

#if defined(LISP_FEATURE_OPENBSD) || defined(LISP_FEATURE_DRAGONFLY)
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);
#endif

#if defined LISP_FEATURE_FREEBSD || defined __DragonFly__
#  define OS_CONTEXT_PC(context) CONTEXT_SLOT(context,eip)
#elif defined __OpenBSD__
#  define OS_CONTEXT_PC(context) CONTEXT_SLOT(context,pc)
#elif defined __NetBSD__
#  define OS_CONTEXT_PC(context) CONTEXT_SLOT(context,EIP)
#else
#  error "unsupported BSD variant"
#endif

#endif /* _X86_BSD_OS_H */
