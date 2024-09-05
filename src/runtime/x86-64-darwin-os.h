#ifndef _X86_64_DARWIN_OS_H
#define _X86_64_DARWIN_OS_H

#include "darwin-os.h"

#define X86_64_SIGFPE_FIXUP    /* Darwin doesn't handle accrued bits correctly. */

static inline unsigned int *
arch_os_context_mxcsr_addr(os_context_t *context)
{
  return &context->uc_mcontext->__fs.__fpu_mxcsr;

}

typedef register_t os_context_register_t;

#if __DARWIN_UNIX03
#define CONTEXT_SLOT(c,stem) c->uc_mcontext->__ss.__##stem
#define CONTEXT_ADDR_FROM_STEM(stem) (os_context_register_t*)&context->uc_mcontext->__ss.__##stem
#define OS_CONTEXT_PC(context) context->uc_mcontext->__ss.__rip
#else
#define CONTEXT_SLOT(c,stem) c->uc_mcontext->ss.stem
#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext->ss.stem
#define OS_CONTEXT_PC(context) context->uc_mcontext->__ss.rip
#endif /* __DARWIN_UNIX03 */

#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);
void set_thread_stack(void *);

#endif /* _X86_64_DARWIN_OS_H */
