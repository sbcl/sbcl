#ifndef _X86_64_DARWIN_OS_H
#define _X86_64_DARWIN_OS_H

#include "darwin-os.h"

typedef register_t os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext->ss.stem
#define DARWIN_FIX_CONTEXT(context)

#endif /* _X86_64_DARWIN_OS_H */
