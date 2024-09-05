#ifndef _PPC_LINUX_OS_H
#define _PPC_LINUX_OS_H

typedef ucontext_t os_context_t;
typedef unsigned long os_context_register_t;

unsigned long os_context_fp_control(os_context_t *context);
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);

#include "ppc-linux-mcontext.h" // Selects one of these two definitions
#ifdef GLIBC231_STYLE_UCONTEXT
#  define OS_CONTEXT_PC(context) (context->uc_mcontext.regs)->nip
#elif defined GLIBC232_STYLE_UCONTEXT
#  define OS_CONTEXT_PC(context) (context->uc_mcontext.uc_regs->gregs)[PT_NIP]
#else
#  error "Need a definition of OS_CONTEXT_PC"
#endif

#endif /* _PPC_LINUX_OS_H */
