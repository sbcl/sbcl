#include <stdio.h>
#include <stdlib.h>
#include "../../src/runtime/os.h"

static int verbose=1;

/// "Pseudo-randomly" alter requested address for testing purposes so that
/// we can test interesting scenarios such as:
///  - partially overlapping ranges for the desired and actual space
///  - aligned to OS page but misaligned for GC card

/* Each line of the instruction file contains two addresses,
 * a sub-2GB address and a high address.
 *
 * 64-bit builds use the first address for immobile space
 * and the second address for dynamic space.
 * If the current build does not support immobile space,
 * the first address in the pair is simply ignored.
 *
 * 32-bit builds use only the first address
 */
void pick_fuzzed_addresses(unsigned long *addr1,
                           unsigned long *addr2)
{
    char * pathname;
    FILE * f;
    char line[100];
    int line_number, i;

    if ((pathname = getenv("SBCL_FAKE_MMAP_INSTRUCTION_FILE")) == NULL) {
        fprintf(stderr, "WARNING: image built with MOCK_MMAP_FAILURE\n");
        fprintf(stderr, "         but no mock configuration data found.\n");
        exit(1);
    }
    if ((f = fopen(pathname, "r+")) == NULL) {
        // If the file can't be found, don't silently ignore it,
        // because if the relocator "worked" you don't want to get all happy
        // that it worked only to find that it didn't actually perform relocation.
        fprintf(stderr, "Could not read '%s'\n", pathname);
        exit(1);
    }
    char __attribute__((unused)) *buf = fgets(line, sizeof line, f);
    line_number = atoi(line);
    for (i = 0 ; i <= line_number ; ++i) {
        int ok;
        ok = fgets(line, sizeof line, f) != NULL && line[0] != '\n';
        if (i == line_number - 1) {
            char *end;
            *addr1 = strtoul(line, &end, 16);
            *addr2 = strtoul(end, 0, 16);
        }
        if (!ok) {
            // fprintf(stderr, "*** Rewinding fake mmap instructions\n");
            line_number = 0;
            break;
        }
    }
    rewind(f);
    fprintf(f, "%02d", 1+line_number);
    fclose(f);
}

os_vm_address_t
os_validate(int attributes, os_vm_address_t addr, os_vm_size_t len,
            int __attribute__((unused)) execute, int __attribute__((unused)) jit)
{
    int flags =  MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;
    void *fuzzed = addr;
    os_vm_address_t actual;
    int movable = attributes & MOVABLE;

#ifdef MAP_32BIT
    if (attributes & ALLOCATE_LOW)
        flags |= MAP_32BIT;
#endif
    if (addr && movable) {
        static unsigned long addr1, addr2;
        if (!addr1)
            pick_fuzzed_addresses(&addr1, &addr2);
#ifdef LISP_FEATURE_64_BIT
        if (!(attributes & ALLOCATE_LOW))
            fuzzed = (void*)addr2;
        else
#endif
            fuzzed = (void*)addr1;
    }
    actual = mmap(fuzzed, len, OS_VM_PROT_ALL, flags, -1, 0);
    if (actual == MAP_FAILED) {
        perror("mmap");
        return 0;               /* caller should check this */
    }
    if (addr && !movable && (actual != addr)) {
        fprintf(stderr, "mmap: wanted %lu bytes at %p, actually mapped at %p\n",
                (unsigned long) len, addr, actual);
        return 0;
    }
    if (verbose && addr != fuzzed)
      fprintf(stderr, actual == fuzzed ?
              "//Fuzzed %12p into %12p successfully\n":
              "//Tried fuzzing %p into %p but actually got %p\n",
              addr, fuzzed, actual);
    return actual;
}

void os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap(addr,len) == -1) {
        perror("munmap");
    }
}
