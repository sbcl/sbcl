#ifndef _X86_BSD_OS_H
#define _X86_BSD_OS_H

#ifdef LISP_FEATURE_FREEBSD
#include <machine/segments.h>
#include <machine/cpufunc.h>
#endif

typedef int os_context_register_t;

#include "arch-os-generic.inc"

/* The different BSD variants have diverged in exactly where they
 * store signal context information, but at least they tend to use the
 * same stems to name the structure fields, so by using this macro we
 * can share a fair amount of code between different variants. */
#if defined(LISP_FEATURE_FREEBSD) || defined(__DragonFly__)
#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext.mc_ ## stem
#elif defined(__OpenBSD__)
#define CONTEXT_ADDR_FROM_STEM(stem) &context->sc_ ## stem
#elif defined __NetBSD__
#define CONTEXT_ADDR_FROM_STEM(stem) &((context)->uc_mcontext.__gregs[_REG_ ## stem])
#else
#error unsupported BSD variant
#endif

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

#endif /* _X86_BSD_OS_H */
