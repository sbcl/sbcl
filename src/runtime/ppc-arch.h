#ifndef _PPC_ARCH_H
#define _PPC_ARCH_H

static inline long
get_spinlock(lispobj *word,long value)
{
#ifdef LISP_FEATURE_SB_THREAD
    long temp;

    asm volatile("1: lwarx %0,0,%1;"
                 "   cmpwi %0,0;"
                 "   bne- 1b;"
                 "   stwcx. %2,0,%1;"
                 "   bne- 1b;"
                 "   isync"
                 : "=&r" (temp)
                 : "r" (word), "r" (value)
                 : "cr0", "memory");
    return temp;
#else
    *word=value;
    return 0;
#endif
}

static inline void
release_spinlock(lispobj *word)
{
#ifdef LISP_FEATURE_SB_THREAD
    asm volatile ("sync" : : : "memory");
#endif
    *word=0;
}

#ifdef LISP_FEATURE_SB_THREAD
static inline lispobj
swap_lispobjs(volatile lispobj *dest, lispobj value)
{
    lispobj old_value;
    asm volatile ("1: lwarx %0,0,%1;"
                  "   stwcx. %2,0,%1;"
                  "   bne- 1b;"
                  "   isync"
         : "=&r" (old_value)
         : "r" (dest), "r" (value)
         : "cr0", "memory");
    return old_value;
}
#endif

#define ARCH_HAS_LINK_REGISTER

extern void ppc_flush_icache(os_vm_address_t address, os_vm_size_t length);

os_context_register_t *os_context_ctr_addr(os_context_t *context);
os_context_register_t *os_context_cr_addr(os_context_t *context);

#endif /* _PPC_ARCH_H */
