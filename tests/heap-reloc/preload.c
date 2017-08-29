#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

static int verbose;

// For LISP_FEATURE_64_BIT
#include "../../src/runtime/genesis/config.h"

/// "Pseudo-randomly" alter requested address for testing purposes so that
/// we can test interesting scenarios such as:
///  - partially overlapping ranges for the desired and actual space
///  - aligned to OS page but misaligned for GC card

void* fuzz(void* addr)
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
        fprintf(stderr, "Could not read 'fakemap' file\n");
        exit(1);
    }
    char __attribute__((unused)) *buf = fgets(line, sizeof line, f);
    line_number = atoi(line);
    void* result = 0;
    for (i = 0 ; i <= line_number ; ++i) {
        int ok;
        ok = fgets(line, sizeof line, f) != NULL && line[0] != '\n';
        if (i == line_number - 1)
            result = (void*)strtol(line, 0, 16);
        if (!ok) {
            // fprintf(stderr, "*** Rewinding fake mmap instructions\n");
            line_number = 0;
            break;
        }
    }
    rewind(f);
    fprintf(f, "%02d", 1+line_number);
    fclose(f);
    fprintf(stderr, "//dynamic space @ %p\n", (void*)result);
    return result;
}

void *maybe_fuzz(void* addr, size_t length)
{
    if (length >= 1024*1024*1024)
        return fuzz(addr);
    return addr;
}

void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset)
{
    if (verbose)
        fprintf(stderr, "//mmap @ %p for %ld, fd=%d\n", addr, length, fd);
    void *(*real_mmap)() = dlsym(RTLD_NEXT, "mmap");
    return real_mmap(maybe_fuzz(addr,length), length, prot, flags, fd, offset);
}

#ifdef LISP_FEATURE_64_BIT
void *mmap64(void *addr, size_t length, int prot, int flags, int fd, off64_t offset)
{
    if (verbose)
        fprintf(stderr, "//mmap64 @ %p for %ld, fd=%d\n", addr, length, fd);
    void *(*real_mmap64)() = dlsym(RTLD_NEXT, "mmap");
    return real_mmap64(maybe_fuzz(addr,length), length, prot, flags, fd, offset);
}
#endif
