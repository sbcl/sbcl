#ifndef _PPC_ARCH_H
#define _PPC_ARCH_H

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
#define ALIEN_STACK_GROWS_DOWNWARD

extern void ppc_flush_icache(os_vm_address_t address, os_vm_size_t length);

os_context_register_t *os_context_ctr_addr(os_context_t *context);
os_context_register_t *os_context_cr_addr(os_context_t *context);

#endif /* _PPC_ARCH_H */
