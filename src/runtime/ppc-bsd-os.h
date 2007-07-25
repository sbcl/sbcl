#ifndef _PPC_BSD_OS_H
#define _PPC_BSD_OS_H

typedef int os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

#endif /* _PPC_BSD_OS_H */
