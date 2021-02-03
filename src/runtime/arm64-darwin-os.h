#ifndef _ARM64_DARWIN_OS_H
#define _ARM64_DARWIN_OS_H

#include "darwin-os.h"

/* static inline unsigned int * */
/* arch_os_context_mxcsr_addr(os_context_t *context) */
/* { */
/*   return &context->uc_mcontext->__fs.__fpu_mxcsr; */

/* } */

typedef register_t os_context_register_t;

#include "arch-os-generic.inc"

/* #if __DARWIN_UNIX03 */
/* #define CONTEXT_ADDR_FROM_STEM(stem) (os_context_register_t*)&context->uc_mcontext->__ss.__##stem */
/* #else */
/* #define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext->ss.stem */
/* #endif /\* __DARWIN_UNIX03 *\/ */

#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);
void set_thread_stack(void *);

#endif /* _ARM64_DARWIN_OS_H */
