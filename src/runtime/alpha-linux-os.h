#ifndef _ALPHA_LINUX_OS_H
#define _ALPHA_LINUX_OS_H

typedef struct ucontext os_context_t;
typedef long os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

unsigned long os_context_fp_control(os_context_t *context);

#endif /* _ALPHA_LINUX_OS_H */
