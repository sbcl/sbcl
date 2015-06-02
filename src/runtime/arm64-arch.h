#ifndef _ARM_ARCH_H
#define _ARM_ARCH_H

#define ALIEN_STACK_GROWS_DOWNWARD

static inline long
get_spinlock(lispobj *word,long value)
{
#ifdef LISP_FEATURE_SB_THREAD
    #error "get_spinlock not defined for threads"
#else
    *word=value;
    return 0;
#endif
}

static inline void
release_spinlock(lispobj *word)
{
#ifdef LISP_FEATURE_SB_THREAD
    #error "release_spinlock not defined for threads"
#endif
    *word=0;
}

#ifdef LISP_FEATURE_SB_THREAD
static inline lispobj
swap_lispobjs(volatile lispobj *dest, lispobj value)
{
    #error "swap_lispobjs not defined for threads"
}
#endif

#define ARCH_HAS_LINK_REGISTER

#endif /* _ARM_ARCH_H */
