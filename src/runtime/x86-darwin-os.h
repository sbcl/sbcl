#ifndef _X86_DARWIN_OS_H
#define _X86_DARWIN_OS_H

static inline os_context_t *arch_os_get_context(void **void_context) {
    return (os_context_t *) *void_context;
}

#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext->ss.stem
#define DARWIN_FIX_CONTEXT(context)

#endif /* _X86_DARWIN_OS_H */
