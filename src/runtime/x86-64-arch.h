#ifndef _X86_64_ARCH_H
#define _X86_64_ARCH_H

#define ARCH_HAS_STACK_POINTER

extern int avx_supported, avx2_supported;
extern unsigned int cpuid_fn1_ecx;
/* When single stepping, single_stepping holds the original instruction
 * PC location. */
extern unsigned int *single_stepping;


#endif /* _X86_64_ARCH_H */
