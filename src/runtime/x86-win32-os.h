#ifndef _X86_WIN32_OS_H
#define _X86_WIN32_OS_H

typedef CONTEXT os_context_t;
typedef long os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

unsigned long os_context_fp_control(os_context_t *context);
void os_restore_fp_control(os_context_t *context);

#endif /* _X86_WIN32_OS_H */
