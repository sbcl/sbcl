#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>

#include "genesis/sbcl.h"
#include "globals.h"
#include "os.h"
#include "interr.h"
#include "sys_mmap.inc"

static void dumpmaps()
{
    FILE *maps = fopen("/proc/self/maps","r");
    if (maps) {
        fprintf(stderr, "Dump of /proc/self/maps:\n");
        char line[512];
        while (fgets(line, sizeof line, maps))
            ignore_value(write(2, line, strlen(line)));
        fclose(maps);
    }
}

/* "man mmap" cites 4 reasons that ENOMEM can be returned, roughly:
 * - No memory is available
 * - maximum number of mappings would have been exceeded
 * - RLIMIT_DATA would have been exceeded
 * - addr exceeds the virtual address space of the CPU
 * but apparently there's at least one more reason which is that even though an
 * address space hole exists, the kernel didn't use it.
 * Probably because MAP_32BIT messes up the algorithm that selects an available range.
 * Passing in MAP_FIXED_NOREPLACE should work in that case.
 */
__attribute__((unused)) static void* try_find_hole(os_vm_size_t len)
{
#ifdef MAP_FIXED_NOREPLACE
    int flags = MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE | MAP_FIXED_NOREPLACE;
#else
    int flags = MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;
#endif
#define TWO_GB_LIM 0x80000000
    FILE*f = fopen("/proc/self/maps", "r");
    if (!f) return 0;
    char line[36], *endptr1, *endptr2;
    uintptr_t previous_high = 0;
    while (fgets(line, sizeof line, f)) {
        uintptr_t low = strtoul(line, &endptr1, 16);
        uintptr_t high = strtoul(endptr1+1, &endptr2, 16);
        if (*endptr1 != '-' || *endptr2 != ' ') break; // didn't parse correctly
        size_t gap = low - previous_high;
        if (gap >= len && previous_high != 0) { // attempt it
            void* actual = sbcl_mmap((void*)previous_high, len, PROT_READ|PROT_WRITE,
                                     flags, -1, 0);
            if (actual != MAP_FAILED && (uword_t)actual + len <= TWO_GB_LIM) {
                fclose(f);
                return actual;
            }
            // maybe unmap. Probably occurs only if the FIXED_NOREPLACE flag doesn't exist
            if (actual != MAP_FAILED) sbcl_munmap(actual, len);
        }
        if (high >= TWO_GB_LIM) break;
        previous_high = high;
        int ch;
        while ((ch = getc(f)) != EOF && ch != '\n') {} // skip to newline
    }
    fclose(f);
    return 0;
}

os_vm_address_t
os_alloc_gc_space(int __attribute__((unused)) space_id,
                  int attributes, os_vm_address_t addr, os_vm_size_t len)
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

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (actual == MAP_FAILED && space_id == IMMOBILE_FIXEDOBJ_CORE_SPACE_ID)
        actual = try_find_hole(len);
#endif

    if (actual == MAP_FAILED) {
        if (errno == ENOMEM)
            fprintf(stderr, "os_alloc_gc_space(%d,%p,%zu) failed with ENOMEM\n",
                    attributes, addr, len);
        else
            perror("mmap");
        dumpmaps();
        return 0;               /* caller should check this */
    }

    // If requested addr was 0, the MOVABLE attribute means nothing.
    if (addr && !(attributes & MOVABLE) && (addr != actual)) {
        fprintf(stderr, "mmap: wanted %lu bytes at %p, actually mapped at %p\n",
                (unsigned long) len, addr, actual);
        dumpmaps();
        return 0;
    }

    return actual;
}
