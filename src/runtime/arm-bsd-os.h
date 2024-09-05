#ifndef _ARM_BSD_OS_H
#define _ARM_BSD_OS_H

typedef long os_context_register_t;

unsigned long os_context_fp_control(os_context_t *context);
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);

#ifdef LISP_FEATURE_NETBSD
#  define OS_CONTEXT_PC(context) context->uc_mcontext.__gregs[reg_PC]
#elif defined LISP_FEATURE_OPENBSD
#  define OS_CONTEXT_PC(context) context->sc_pc
#endif

#endif /* _ARM_BSD_OS_H */
