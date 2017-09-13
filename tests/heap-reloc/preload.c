#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

static int verbose=1;

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
    return result;
}

// Fuzzer has to guess whether this was a relocatable request based on size
// because we eschew the MAP_FIXED flag in all cases.
#define CALL_REAL_FUN() \
  if (addr && (length == 1024*1024*1024 || length == 128*1024*1024)) { \
    void *changed_addr = fuzz(addr); \
    void *got = realfun(changed_addr, length, prot, flags, fd, offset); \
    if (verbose) \
      fprintf(stderr, got==changed_addr ?                               \
              "//Fuzzed %p into %p successfully\n":                     \
              "//Tried fuzzing %p into %p but actually got %p\n",       \
              addr, changed_addr, got);                                 \
    return got; } else { return realfun(addr, length, prot, flags, fd, offset); }

void *mmap(void *addr, size_t length, int prot, int flags, int fd, off_t offset)
{
    if (verbose>1)
        fprintf(stderr, "//mmap @ %p for %ld, fd=%d\n", addr, length, fd);
    void *(*realfun)() = dlsym(RTLD_NEXT, "mmap");
    CALL_REAL_FUN();
}

#ifdef LISP_FEATURE_64_BIT
void *mmap64(void *addr, size_t length, int prot, int flags, int fd, off64_t offset)
{
    if (verbose>1)
        fprintf(stderr, "//mmap64 @ %p for %ld, fd=%d\n", addr, length, fd);
    void *(*realfun)() = dlsym(RTLD_NEXT, "mmap64");
    CALL_REAL_FUN();
}
#endif
