#include <stdio.h>
#include <stdlib.h>
#include "../../src/runtime/os.h"

static int verbose=1;

/// "Pseudo-randomly" alter requested address for testing purposes so that
/// we can test interesting scenarios such as:
///  - partially overlapping ranges for the desired and actual space
///  - aligned to OS page but misaligned for GC card

void pick_fuzzed_addresses(void** addr1, void** addr2, void** addr3)
{
    char * pathname;
    FILE * f;
    char line[100];
    int line_number = 1, i;

    if ((pathname = getenv("SBCL_FAKE_MMAP_INSTRUCTION_FILE")) == NULL) {
        fprintf(stderr, "WARNING: image built with MOCK_MMAP_FAILURE\n");
        fprintf(stderr, "         but no mock configuration data found.\n");
        exit(1);
    }
    char *opt = getenv("SBCL_FAKE_MMAP_INSTRUCTION_LINE");
    if (opt) line_number = atoi(opt);
    if ((f = fopen(pathname, "r")) == NULL) {
        // If the file can't be found, don't silently ignore it,
        // because if the relocator "worked" you don't want to get all happy
        // that it worked only to find that it didn't actually perform relocation.
        fprintf(stderr, "Could not read '%s'\n", pathname);
        exit(1);
    }
    // skip the comment line and read 'line_number' more lines
    for (i = 0 ; i <= line_number ; ++i) fgets(line, sizeof line, f);
    char *end;
    *addr1 = (void*)strtoul(line, &end, 16);
    *addr2 = (void*)strtoul(end, &end, 16);
    *addr3 = (void*)strtoul(end, &end, 16);
    fprintf(stderr, "Trial %d:\n", line_number);
    fclose(f);
}

os_vm_address_t
os_alloc_gc_space(int space_id, int attributes, os_vm_address_t addr, os_vm_size_t len)
{
    int flags =  MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;
    void *fuzzed = addr;
    os_vm_address_t actual;
    int movable = attributes & MOVABLE;

    if (addr && movable) {
        static void *val1, *val2, *val3;
        if (!val1)  pick_fuzzed_addresses(&val1, &val2, &val3);
        switch (space_id) {
        case READ_ONLY_CORE_SPACE_ID: fuzzed = val1; break;
        case PERMGEN_CORE_SPACE_ID:
        case IMMOBILE_FIXEDOBJ_CORE_SPACE_ID: fuzzed = val2; break;
        case DYNAMIC_CORE_SPACE_ID: fuzzed = val3; break;
#ifdef LISP_FEATURE_RELOCATABLE_STATIC_SPACE
        // 1+ to ensure that the space is displaced by a nonzero amount
        case STATIC_CORE_SPACE_ID: fuzzed = addr + (1+(getpid()&0xfffff))*4096; break;
#endif
        }
    }
#ifdef MAP_32BIT
    if (attributes & ALLOCATE_LOW) flags |= MAP_32BIT;
#endif
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
              "// space %d: Fuzzed %12p into %12p successfully\n":
              "// space %d: Tried fuzzing %p into %p but actually got %p\n",
              space_id, addr, fuzzed, actual);
    return actual;
}
