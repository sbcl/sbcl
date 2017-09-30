#ifndef _ARM_ANDROID_OS_H
#define _ARM_ANDROID_OS_H

#include <signal.h>

typedef struct sigcontext mcontext_t;

typedef struct os_context {
  unsigned long uc_flags;
  struct ucontext* uc_link;
  stack_t uc_stack;
  mcontext_t uc_mcontext;
  sigset_t uc_sigmask;
  char __padding[128 - sizeof(sigset_t)];
  unsigned long uc_regspace[128] __attribute__((__aligned__(8)));
} os_context_t;


typedef long os_context_register_t;

#include "arch-os-generic.inc"

unsigned long os_context_fp_control(os_context_t *context);
#define RESTORE_FP_CONTROL_FROM_CONTEXT
void os_restore_fp_control(os_context_t *context);

#endif /* _ARM_LINUX_OS_H */
