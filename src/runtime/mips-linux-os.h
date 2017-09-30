#ifndef _MIPS_LINUX_OS_H
#define _MIPS_LINUX_OS_H

typedef ucontext_t os_context_t;
typedef unsigned long long os_context_register_t;

#include "arch-os-generic.inc"

unsigned int os_context_fp_control(os_context_t *context);
void os_restore_fp_control(os_context_t *context);
unsigned int os_context_bd_cause(os_context_t *context);

#endif /* _MIPS_LINUX_OS_H */
