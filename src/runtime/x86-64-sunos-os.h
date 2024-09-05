#ifndef _X86_64_SOLARIS_OS_H
#define _X86_64_SOLARIS_OS_H

typedef struct ucontext os_context_t;
typedef long os_context_register_t;

#define OS_CONTEXT_PC(context) context->uc_mcontext.gregs[REG_RIP]

#endif /* _X86_64_SOLARIS_OS_H */
