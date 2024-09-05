#ifndef _ARM64_DARWIN_OS_H
#define _ARM64_DARWIN_OS_H

#include "darwin-os.h"

/* static inline unsigned int * */
/* arch_os_context_mxcsr_addr(os_context_t *context) */
/* { */
/*   return &context->uc_mcontext->__fs.__fpu_mxcsr; */

/* } */

typedef register_t os_context_register_t;

#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);
void set_thread_stack(void *);

#define OS_CONTEXT_PC(context) context->uc_mcontext->__ss.__pc

#endif /* _ARM64_DARWIN_OS_H */
