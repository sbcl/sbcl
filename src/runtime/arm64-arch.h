#ifndef _ARM64_ARCH_H
#define _ARM64_ARCH_H

#define ALIEN_STACK_GROWS_DOWNWARD

#ifdef LISP_FEATURE_SB_THREAD
static inline lispobj
swap_lispobjs(volatile lispobj *dest, lispobj value)
{
    lispobj old_value;
    int temp;

    asm volatile ("1: ldaxr %0,[%2];"
                  "   stlxr %w1,%3,[%2];"
                  "   cbnz %w1,1b;"
                  : "=&r" (old_value), "=&r" (temp)
                  : "r" (dest), "r" (value)
                  : "memory");
    return old_value;
}
#endif

#define ARCH_HAS_LINK_REGISTER

#endif /* _ARM64_ARCH_H */
