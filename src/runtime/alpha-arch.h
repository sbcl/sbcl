#ifndef _ALPHA_ARCH_H
#define _ALPHA_ARCH_H


static inline void
get_spinlock(lispobj *word,long value)
{
    *word=value;                /* FIXME for threads */
}

static inline void
release_spinlock(lispobj *word)
{
    *word=0;
}

#define ARCH_HAS_FLOAT_REGISTERS

#endif /* _ALPHA_ARCH_H */
