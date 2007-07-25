#ifndef _MIPS_LINUX_OS_H
#define _MIPS_LINUX_OS_H

typedef struct ucontext os_context_t;
typedef unsigned long long os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

unsigned int os_context_fp_control(os_context_t *context);
void os_restore_fp_control(os_context_t *context);
unsigned int os_context_bd_cause(os_context_t *context);

#endif /* _MIPS_LINUX_OS_H */
