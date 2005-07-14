#ifndef _SPARC_ARCH_H
#define _SPARC_ARCH_H

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

#define ARCH_HAS_NPC_REGISTER

#endif /* _SPARC_ARCH_H */
