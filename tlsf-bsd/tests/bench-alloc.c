/* Copyright (c) 2016 National Cheng Kung University, Taiwan.
 * All rights reserved.
 *
 * Use of this source code is governed by a BSD-style license that can be
 * found in the LICENSE file.
 */

#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE
#endif
#define _POSIX_C_SOURCE 199309L

#include <unistd.h>
#include <assert.h>
#include <errno.h>
#include <sched.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <time.h>

#include "lran2.h"
#include "tlsf.h"

static tlsf_t *tlsf;

static void usage(const char *name)
{
    printf("run a malloc benchmark.\n"
           "usage: %s [-s blk-size|blk-min:blk-max] [-l loop-count] "
           "[-n num-blocks] [-c]\n",
           name);
    exit(-1);
}

/* Parse an integer argument. */
static int parse_int_arg(const char *arg, const char *exe_name)
{
    long int ret;

    errno = 0;
    ret = strtol(arg, NULL, 0);
    if (errno)
        usage(exe_name);

    return (int) ret;
}

/* Parse a size argument, which is either an integer or two integers
   separated by a colon, denoting a range. */
static void
parse_size_arg(const char *arg, const char *exe_name,
               size_t *blk_min, size_t *blk_max)
{
    long int ret;
    char *endptr;

    errno = 0;
    ret = strtol(arg, &endptr, 0);

    if (errno)
        usage(exe_name);

    *blk_min = (int)ret;

    if (endptr && *endptr == ':') {
        errno = 0;
        ret = strtol(endptr + 1, NULL, 0);

        if (errno)
            usage(exe_name);
    }

    *blk_max = (int)ret;

    if (blk_min > blk_max)
        usage(exe_name);
}

/* Get a random block size between blk_min and blk_max. */
static size_t
get_random_block_size(size_t blk_min, size_t blk_max,
                      struct lran2_st *lran2_state)
{
    size_t blk_size;

    if (blk_max > blk_min) {
        blk_size = blk_min + (lran2(lran2_state) % (blk_max - blk_min));
    } else
        blk_size = blk_min;

    return blk_size;
}

static void
run_alloc_benchmark(int loops, size_t blk_min, size_t blk_max,
                    void **blk_array, size_t num_blks, bool clear,
                    struct lran2_st *lran2_state)
{
    while (loops--) {
        int next_idx = lran2(lran2_state) % num_blks;
        size_t blk_size = get_random_block_size(blk_min, blk_max, lran2_state);

        if (blk_array[next_idx])
            tlsf_free(tlsf, blk_array[next_idx]);

        /* Insert the newly alloced block into the array at a random point. */
        blk_array[next_idx] = tlsf_malloc(tlsf, blk_size);
        if (clear)
            memset(blk_array[next_idx], 0, blk_size);
    }

    /* Free up all allocated blocks. */
    for (size_t i = 0; i < num_blks; i++) {
        if (blk_array[i])
            tlsf_free(tlsf, blk_array[i]);
    }
}

struct alloc_desc {
    /* Generic fields. */
    int loops;
    size_t blk_min;
    size_t blk_max;
    void **blk_array;
    size_t num_blks;
    bool clear;
};

static void start_bench(void *arg)
{
    struct alloc_desc *desc = arg;
    struct lran2_st lran2_state;

    lran2_init(&lran2_state, time(NULL) ^ getpid());

    run_alloc_benchmark(desc->loops, desc->blk_min, desc->blk_max,
                        desc->blk_array, desc->num_blks, desc->clear,
                        &lran2_state);
}

static void stop_bench(void *arg)
{
    struct alloc_desc *desc = arg;
    if (!desc) return;
    free(desc->blk_array);
}

int main(int argc, char **argv)
{
    size_t blk_min = 512, blk_max = 512, num_blks = 10000;
    int loops = 10000000;
    bool clear = false;
    int opt;

    while ((opt = getopt(argc, argv, "s:l:r:t:n:b:ch")) > 0) {
        switch (opt) {
            case 's':
                parse_size_arg(optarg, argv[0], &blk_min, &blk_max);
                break;
            case 'l':
                loops = parse_int_arg(optarg, argv[0]);
                break;
            case 'n':
                num_blks = parse_int_arg(optarg, argv[0]);
                break;
            case 'c':
                clear = true;
                break;
            case 'h':
                usage(argv[0]);
                break;
            default:
                usage(argv[0]);
                break;
        }
    }

    const size_t overhead = tlsf_size() + tlsf_pool_overhead() +
                            tlsf_alloc_overhead();
    char *space = malloc(blk_max * num_blks + overhead);
    assert(space);
    tlsf = tlsf_create_with_pool(space, blk_max * num_blks + overhead);

    struct alloc_desc desc = {
        .loops = loops,
        .blk_min = blk_min,
        .blk_max = blk_max,
        .blk_array = malloc(num_blks * sizeof(unsigned char *)),
        .num_blks = num_blks,
        .clear = clear,
    };
    assert(desc.blk_array != NULL);
    memset(desc.blk_array, 0, num_blks * sizeof(unsigned char *));

    struct timespec start, end;

    int err = clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &start);
    assert(err == 0);
    start_bench(&desc);

    err = clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &end);
    assert(err == 0);
    stop_bench(&desc);

    double elapsed = (end.tv_sec - start.tv_sec) +
                     (end.tv_nsec - start.tv_nsec) * 1e-9;

    struct rusage usage;
    err = getrusage(RUSAGE_SELF, &usage);
    assert(err == 0);

    tlsf_destroy(tlsf);
    free(space);

    /* Dump both machine and human readable versions */
    printf("%u:%u:%u:%u:%u:%.6f: took %.6f s for %u malloc/free\n"
           "benchmark loops of %u-%u bytes.  ~%.3f us per loop\n",
           blk_min, blk_max, loops,
           (int)clear, usage.ru_maxrss, elapsed, elapsed, loops, blk_min,
           blk_max, (double)(elapsed / loops) * 1e6);

    return 0;
}
