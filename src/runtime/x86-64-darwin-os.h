#ifndef _X86_64_DARWIN_OS_H
#define _X86_64_DARWIN_OS_H

#include "darwin-os.h"

typedef register_t os_context_register_t;

static inline os_context_t *arch_os_get_context(void **void_context)
{
    return (os_context_t *) *void_context;
}

#if __DARWIN_UNIX03
#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext->__ss.__##stem
#else
#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext->ss.stem
#endif /* __DARWIN_UNIX03 */

#endif /* _X86_64_DARWIN_OS_H */
