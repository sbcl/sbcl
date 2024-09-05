#ifndef _ARM_LINUX_OS_H
#define _ARM_LINUX_OS_H

typedef ucontext_t os_context_t;
typedef long os_context_register_t;

unsigned long os_context_fp_control(os_context_t *context);
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);

#define OS_CONTEXT_PC(context) context->uc_mcontext.arm_pc

#endif /* _ARM_LINUX_OS_H */
