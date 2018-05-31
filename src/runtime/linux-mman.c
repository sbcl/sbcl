#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>

#include "genesis/config.h"
#include "os.h"
#include "interr.h"

os_vm_address_t
os_validate(int movable, os_vm_address_t addr, os_vm_size_t len)
{
    int flags =  MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;
    os_vm_address_t actual;

#ifdef LISP_FEATURE_ALPHA
    if (!addr) {
        addr=under_2gb_free_pointer;
    }
#endif
#ifdef MAP_32BIT
    if (movable & MOVABLE_LOW)
        flags |= MAP_32BIT;
#endif
    actual = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);
    if (actual == MAP_FAILED) {
        perror("mmap");
        return 0;               /* caller should check this */
    }

    if (!movable && (addr!=actual)) {
        fprintf(stderr, "mmap: wanted %lu bytes at %p, actually mapped at %p\n",
                (unsigned long) len, addr, actual);
        return 0;
    }

#ifdef LISP_FEATURE_ALPHA

    len=(len+(os_vm_page_size-1))&(~(os_vm_page_size-1));
    under_2gb_free_pointer+=len;
#endif

    return actual;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap(addr,len) == -1) {
        perror("munmap");
    }
}
