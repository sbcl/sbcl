#ifndef _PPC_ARCH_H
#define _PPC_ARCH_H

#define ARCH_HAS_LINK_REGISTER
#define ALIEN_STACK_GROWS_DOWNWARD

extern void ppc_flush_icache(os_vm_address_t address, os_vm_size_t length);

os_context_register_t *os_context_ctr_addr(os_context_t *context);
os_context_register_t *os_context_cr_addr(os_context_t *context);

#endif /* _PPC_ARCH_H */
