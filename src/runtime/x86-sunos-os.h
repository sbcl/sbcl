#ifndef _X86_SOLARIS_OS_H
#define _X86_SOLARIS_OS_H

typedef ucontext_t os_context_t;
typedef int os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

#endif /* _X86_SOLARIS_OS_H */
