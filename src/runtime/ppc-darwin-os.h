#ifndef _PPC_DARWIN_OS_H
#define _PPC_DARWIN_OS_H

typedef unsigned int os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

#define DARWIN_FIX_CONTEXT(c) (c->uc_mcontext->ss.xer)^=0x80;
#endif /* _PPC_DARWIN_OS_H */
