/* FIXME: Aren't preprocessor symbols with underscore prefixes
 * reserved for the system libraries? If so, it would be tidy to
 * rename flags like _X86_ARCH_H so their names are in a part of the
 * namespace that we control. */
#ifndef _X86_64_ARCH_H
#define _X86_64_ARCH_H

#define ARCH_HAS_STACK_POINTER
#define ALIEN_STACK_GROWS_DOWNWARD

extern int avx_supported, avx2_supported;
extern unsigned int cpuid_fn1_ecx;
/* When single stepping, single_stepping holds the original instruction
 * PC location. */
extern unsigned int *single_stepping;


#endif /* _X86_64_ARCH_H */
