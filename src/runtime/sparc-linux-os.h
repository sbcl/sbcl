#ifndef _SPARC_LINUX_OS_H
#define _SPARC_LINUX_OS_H

typedef struct sigcontext os_context_t;
typedef unsigned long os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    asm volatile ("ta 0x03"); /* ta ST_FLUSH_WINDOWS */
    return (os_context_t *) (*void_context);
}

unsigned long os_context_fp_control(os_context_t *context);
void os_restore_fp_control(os_context_t *context);

#endif /* _SPARC_LINUX_OS_H */
