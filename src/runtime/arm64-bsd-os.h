#ifndef _ARM64_BSD_OS_H
#define _ARM64_BSD_OS_H

typedef long os_context_register_t;

unsigned long os_context_fp_control(os_context_t *context);
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);

#ifdef LISP_FEATURE_OPENBSD
#  define OS_CONTEXT_PC(context) context->sc_elr
#elif defined LISP_FEATURE_NETBSD
#  define OS_CONTEXT_PC(context) context->uc_mcontext.__gregs[32]
#elif defined LISP_FEATURE_FREEBSD
#  define OS_CONTEXT_PC(context) context->uc_mcontext.mc_gpregs.gp_x[32]
#endif

#endif /* _ARM64_BSD_OS_H */
