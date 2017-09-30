#ifndef _ALPHA_LINUX_OS_H
#define _ALPHA_LINUX_OS_H

typedef ucontext_t os_context_t;
typedef long os_context_register_t;

#include "arch-os-generic.inc"

unsigned long os_context_fp_control(os_context_t *context);

#endif /* _ALPHA_LINUX_OS_H */
