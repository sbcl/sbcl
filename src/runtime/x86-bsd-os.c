#include <signal.h>
#include "target-arch-os.h"
#include "target-os.h"

/* KLUDGE: There is strong family resemblance in the signal context
 * stuff in FreeBSD and OpenBSD, but in detail they're different in
 * almost every line of code. It would be nice to find some way to
 * factor out the commonality better; failing that, it might be best
 * just to split this generic-BSD code into one variant for each BSD. 
 *
 * KLUDGE II: this split has begun with the addition of the Darwin BSD
 * flavour, with the cross-architecture complications that this
 * entails; unfortunately, currently the situation is worse, not
 * better, than in the above paragraph. */
   
int *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case  0:
	return CONTEXT_ADDR_FROM_STEM(eax);
    case  2:
	return CONTEXT_ADDR_FROM_STEM(ecx);
    case  4:
	return CONTEXT_ADDR_FROM_STEM(edx);
    case  6:
	return CONTEXT_ADDR_FROM_STEM(ebx);
    case  8:
	return CONTEXT_ADDR_FROM_STEM(esp);
    case 10:
	return CONTEXT_ADDR_FROM_STEM(ebp);
    case 12:
	return CONTEXT_ADDR_FROM_STEM(esi);
    case 14:
	return CONTEXT_ADDR_FROM_STEM(edi);
    default:
	return 0;
    }
}

int *
os_context_sp_addr(os_context_t *context)
{
    return CONTEXT_ADDR_FROM_STEM(esp);
}


/* FIXME: If this can be a no-op on BSD/x86, then it 
 * deserves a more precise name.
 *
 * (Perhaps os_prepare_data_area_to_be_executed()?) */
void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}
