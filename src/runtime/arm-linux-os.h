#ifndef _ARM_LINUX_OS_H
#define _ARM_LINUX_OS_H

typedef struct ucontext os_context_t;
typedef long os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

unsigned long os_context_fp_control(os_context_t *context);
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);

#endif /* _ARM_LINUX_OS_H */
