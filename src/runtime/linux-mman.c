#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>

#include "genesis/config.h"
#include "genesis/constants.h"
#include "globals.h"
#include "os.h"
#include "interr.h"

#ifdef LISP_FEATURE_ALPHA
/* The Alpha is a 64 bit CPU.  SBCL is a 32 bit application.  Due to all
 * the places that assume we can get a pointer into a fixnum with no
 * information loss, we have to make sure it allocates all its ram in the
 * 0-2Gb region.  */

static void * under_2gb_free_pointer;
os_set_cheneygc_spaces(uword_t space0_start, uword_t space1_start)
{
    uword_t max;
    max = (space1_start > space0_start) ? space1_start : space0_start;
    under_2gb_free_pointer = max + dynamic_space_size;
}

#endif

os_vm_address_t
os_validate(int attributes, os_vm_address_t addr, os_vm_size_t len)
{
    int protection = attributes & IS_GUARD_PAGE ? OS_VM_PROT_NONE : OS_VM_PROT_ALL;
    attributes &= ~IS_GUARD_PAGE;
    int flags =  MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;
    os_vm_address_t actual;

#ifdef LISP_FEATURE_ALPHA
    if (!addr) {
        addr=under_2gb_free_pointer;
    }
#endif
#ifdef MAP_32BIT
    if (attributes & ALLOCATE_LOW)
        flags |= MAP_32BIT;
#endif
    actual = mmap(addr, len, protection, flags, -1, 0);
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
