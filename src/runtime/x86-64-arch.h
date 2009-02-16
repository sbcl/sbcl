/* FIXME: Aren't preprocessor symbols with underscore prefixes
 * reserved for the system libraries? If so, it would be tidy to
 * rename flags like _X86_ARCH_H so their names are in a part of the
 * namespace that we control. */
#ifndef _X86_64_ARCH_H
#define _X86_64_ARCH_H

#ifndef SBCL_GENESIS_CONFIG
#error genesis/config.h (or sbcl.h) must be included before this file
#endif

#include "interr.h"                     /* for declaration of lose() */

#define ARCH_HAS_STACK_POINTER

/* FIXME: Do we also want
 *   #define ARCH_HAS_FLOAT_REGISTERS
 * here? (The answer wasn't obvious to me when merging the
 * architecture-abstracting patches for CSR's SPARC port. -- WHN 2002-02-15) */

#define COMPILER_BARRIER \
    do { __asm__ __volatile__ ( "" : : : "memory"); } while (0)

static inline void
get_spinlock(volatile lispobj *word, unsigned long value)
{
#ifdef LISP_FEATURE_SB_THREAD
    u64 rax=0;
    if(*word==value)
        lose("recursive get_spinlock: 0x%x,%ld\n",word,value);
    do {
#if defined(LISP_FEATURE_DARWIN)
        asm volatile
            ("xor %0,%0\n\
              lock/cmpxchg %1,%2"
             : "=a" (rax)
             : "r" (value), "m" (*word)
             : "memory", "cc");
#else
        asm volatile
            ("xor %0,%0\n\
              lock cmpxchg %1,%2"
             : "=a" (rax)
             : "r" (value), "m" (*word)
             : "memory", "cc");
#endif
    } while(rax!=0);
#else
    *word=value;
#endif
}

static inline void
release_spinlock(volatile lispobj *word)
{
    /* See comment in RELEASE-SPINLOCK in target-thread.lisp. */
    COMPILER_BARRIER;
    *word=0;
    COMPILER_BARRIER;
}

static inline lispobj
swap_lispobjs(volatile lispobj *dest, lispobj value)
{
    lispobj old_value;
    asm volatile
        ("lock xchg %0,(%1)"
         : "=r" (old_value)
         : "r" (dest), "0" (value)
         : "memory");
    return old_value;
}

#endif /* _X86_64_ARCH_H */
