#ifndef _SPARC_SOLARIS_OS_H
#define _SPARC_SOLARIS_OS_H

typedef ucontext_t os_context_t;
typedef int os_context_register_t ;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    asm volatile ("ta 0x03"); /* ta ST_FLUSH_WINDOWS */
    return (os_context_t *) (*void_context);
}

#endif /* _SPARC_SOLARIS_OS_H */
