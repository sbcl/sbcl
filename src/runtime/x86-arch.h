/* FIXME: Aren't preprocessor symbols with underscore prefixes
 * reserved for the system libraries? If so, it would be tidy to
 * rename flags like _X86_ARCH_H so their names are in a part of the
 * namespace that we control. */
#ifndef _X86_ARCH_H
#define _X86_ARCH_H

#define ARCH_HAS_STACK_POINTER

/* FIXME: Do we also want
 *   #define ARCH_HAS_FLOAT_REGISTERS
 * here? (The answer wasn't obvious to me when merging the
 * architecture-abstracting patches for CSR's SPARC port. -- WHN 2002-02-15) */

#endif /* _X86_ARCH_H */
