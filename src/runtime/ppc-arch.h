#ifndef _PPC_ARCH_H
#define _PPC_ARCH_H

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


#define ARCH_HAS_LINK_REGISTER

#endif /* _PPC_ARCH_H */
