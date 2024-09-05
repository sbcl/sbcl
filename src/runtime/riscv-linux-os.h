#ifndef _RISCV_LINUX_OS_H
#define _RISCV_LINUX_OS_H

typedef ucontext_t os_context_t;
typedef long os_context_register_t;

#define OS_CONTEXT_PC(context) context->uc_mcontext.__gregs[0]

#endif /* _RISCV_LINUX_OS_H */
