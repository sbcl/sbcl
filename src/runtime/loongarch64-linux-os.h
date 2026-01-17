#ifndef _LOONGARCH64_LINUX_OS_H
#define _LOONGARCH64_LINUX_OS_H

typedef ucontext_t os_context_t;
typedef long os_context_register_t;

#define OS_CONTEXT_PC(context) ((context)->uc_mcontext.__pc)

#endif /* _LOONGARCH64_LINUX_OS_H */
