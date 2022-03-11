#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>

#include "genesis/config.h"
#include "genesis/constants.h"
#include "globals.h"
#include "os.h"
#include "interr.h"
#include "sys_mmap.inc"

os_vm_address_t
os_validate(int attributes, os_vm_address_t addr, os_vm_size_t len,
            int __attribute__((unused)) execute, int __attribute__((unused)) jit)
{
    int protection = attributes & IS_GUARD_PAGE ? OS_VM_PROT_NONE : OS_VM_PROT_ALL;
    attributes &= ~IS_GUARD_PAGE;
    int flags =  MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;
    os_vm_address_t actual;

#ifdef MAP_32BIT
    if (attributes & ALLOCATE_LOW)
        flags |= MAP_32BIT;
#endif
    actual = sbcl_mmap(addr, len, protection, flags, -1, 0);
    if (actual == MAP_FAILED) {
        perror("mmap");
        return 0;               /* caller should check this */
    }

    // If requested addr was 0, the MOVABLE attribute means nothing.
    if (addr && !(attributes & MOVABLE) && (addr != actual)) {
        fprintf(stderr, "mmap: wanted %lu bytes at %p, actually mapped at %p\n"
                "Dump of /proc/self/maps:\n",
                (unsigned long) len, addr, actual);
        FILE *maps = fopen("/proc/self/maps","r");
        if (maps) {
            char line[512];
            while (fgets(line, sizeof line, maps))
                ignore_value(write(2, line, strlen(line)));
            fclose(maps);
        }
        return 0;
    }

    return actual;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (sbcl_munmap(addr,len) == -1) {
        perror("munmap");
    }
}
