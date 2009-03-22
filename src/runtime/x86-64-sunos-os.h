#ifndef _X86_64_SOLARIS_OS_H
#define _X86_64_SOLARIS_OS_H

typedef struct ucontext os_context_t;
typedef long os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

#endif /* _X86_64_SOLARIS_OS_H */
