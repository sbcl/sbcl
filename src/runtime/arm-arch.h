#ifndef _ARM_ARCH_H
#define _ARM_ARCH_H

#define ALIEN_STACK_GROWS_DOWNWARD

#ifdef LISP_FEATURE_SB_THREAD
static inline lispobj
swap_lispobjs(volatile lispobj *dest, lispobj value)
{
    #error "swap_lispobjs not defined for threads"
}
#endif

#define ARCH_HAS_LINK_REGISTER

#endif /* _ARM_ARCH_H */
