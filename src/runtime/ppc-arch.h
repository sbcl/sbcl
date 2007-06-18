#ifndef _PPC_ARCH_H
#define _PPC_ARCH_H

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


#define ARCH_HAS_LINK_REGISTER

extern void ppc_flush_icache(os_vm_address_t address, os_vm_size_t length);

#endif /* _PPC_ARCH_H */
