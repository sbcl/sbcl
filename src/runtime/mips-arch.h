#ifndef _MIPS_ARCH_H
#define _MIPS_ARCH_H


static inline void 
get_spinlock(lispobj *word,int value)
{
    *word=value;		/* FIXME for threads */
}

static inline void
release_spinlock(lispobj *word)
{
    *word=0;
}

#endif /* _MIPS_ARCH_H */
