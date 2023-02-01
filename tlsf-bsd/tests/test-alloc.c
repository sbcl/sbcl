#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
#include <assert.h>
#include <err.h>

#include <tlsf.h>

static void random_test(const size_t spacelen, const size_t cap)
{
    const size_t overhead = tlsf_size() + tlsf_pool_overhead() +
                            tlsf_alloc_overhead();
    const size_t maxitems = spacelen;
    size_t len;
    uint8_t *space, *data;
    unsigned i = 0;
    tlsf_t *tlsf;
    void **p;

    space = malloc(spacelen + overhead);
    if (space == NULL) {
        err(EXIT_FAILURE, "malloc");
    }

    p = malloc(maxitems * sizeof(void *));
    if (p == NULL) {
        err(EXIT_FAILURE, "malloc");
    }

    tlsf = tlsf_create_with_pool(space,
                                 spacelen + overhead);
    assert(tlsf != NULL);

    /*
     * Allocate random sizes up to the cap threshold.
     * Track them in an array.
     */
    for (;;) {
        len = (random() % cap) + 1;
        p[i] = tlsf_malloc(tlsf, len);
        if (!p[i])
            break;

        /* Fill with magic (only when testing up to 1MB). */
        data = p[i];
        if (spacelen <= 1024 * 1024) {
            memset(data, 0, len);
        }
        data[0] = 0xa5;

        if (i++ == maxitems)
            break;
    }

    /*
     * Randomly deallocate the memory blocks until all of them are freed.
     * The free space should match the free space after initialisation.
     */
    for (unsigned n = i; n;) {
        unsigned target = random() % i;
        if (p[target] == NULL)
            continue;
        data = p[target];
        assert(data[0] == 0xa5);
        tlsf_free(tlsf, p[target]);
        p[target] = NULL;
        n--;
    }

    tlsf_destroy(tlsf);
    free(space);
    free(p);
}

#define __arraycount(__x) \
    (sizeof(__x) / sizeof(__x[0]))

static void random_sizes_test(void)
{
    const uint32_t sizes[] = {128, 1024, 1024 * 1024, 128 * 1024 * 1024};

    for (unsigned i = 0; i < __arraycount(sizes); i++) {
        unsigned n = 1024;

        while (n--) {
            uint32_t cap = random() % sizes[i] + 1;
            printf("sizes = %d, cap = %d\n", sizes[i], cap);
            random_test(sizes[i], cap);
        }
    }
}

int main(void)
{
    srandom(time(NULL) ^ getpid());
    random_sizes_test();
    puts("OK!");
    return 0;
}
