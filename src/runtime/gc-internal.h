/*
 * garbage collection - shared definitions for modules "inside" the GC system
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#ifndef _GC_INTERNAL_H_
#define _GC_INTERNAL_H_

#include <genesis/simple-fun.h>
#include "thread.h"

/* disabling gc assertions made no discernable difference to GC speed,
 * last I tried it - dan 2003.12.21
 *
 * And it's unsafe to do so while things like gc_assert(0 ==
 * thread_mutex_lock(&allocation_lock)) exist. - MG 2009-01-13
 */
#if 1
# define gc_assert(ex)                                                 \
do {                                                                   \
    if (!(ex)) gc_abort();                                             \
} while (0)
# define gc_assert_verbose(ex, fmt, ...)                               \
do {                                                                   \
    if (!(ex)) {                                                       \
        fprintf(stderr, fmt, ## __VA_ARGS__);                          \
        gc_abort();                                                    \
    }                                                                  \
} while (0)
#else
# define gc_assert(ex)
# define gc_assert_verbose(ex, fmt, ...)
#endif

#define gc_abort()                                                     \
  lose("GC invariant lost, file \"%s\", line %d\n", __FILE__, __LINE__)

#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))

static inline unsigned long
NWORDS(unsigned long x, unsigned long n_bits)
{
    /* A good compiler should be able to constant-fold this whole thing,
       even with the conditional. */
    if(n_bits <= N_WORD_BITS) {
        unsigned long elements_per_word = N_WORD_BITS/n_bits;

        return CEILING(x, elements_per_word)/elements_per_word;
    }
    else {
        /* FIXME: should have some sort of assertion that N_WORD_BITS
           evenly divides n_bits */
        return x * (n_bits/N_WORD_BITS);
    }
}

/* FIXME: Shouldn't this be defined in sbcl.h? */

#if defined(LISP_FEATURE_SPARC)
#define FUN_RAW_ADDR_OFFSET 0
#else
#define FUN_RAW_ADDR_OFFSET (offsetof(struct simple_fun, code) - FUN_POINTER_LOWTAG)
#endif

/* values for the *_alloc_* parameters */
#define FREE_PAGE_FLAG 0
#define BOXED_PAGE_FLAG 1
#define UNBOXED_PAGE_FLAG 2
#define OPEN_REGION_PAGE_FLAG 4
#define CODE_PAGE_FLAG        (BOXED_PAGE_FLAG|UNBOXED_PAGE_FLAG)

#define ALLOC_BOXED 0
#define ALLOC_UNBOXED 1
#define ALLOC_QUICK 1

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-alloc-region.h"
void *
gc_alloc_with_region(long nbytes,int page_type_flag, struct alloc_region *my_region,
                     int quick_p);
static inline void *
gc_general_alloc(long nbytes, int page_type_flag, int quick_p)
{
    struct alloc_region *my_region;
    if (UNBOXED_PAGE_FLAG == page_type_flag) {
        my_region = &unboxed_region;
    } else if (BOXED_PAGE_FLAG & page_type_flag) {
        my_region = &boxed_region;
    } else {
        lose("bad page type flag: %d", page_type_flag);
    }
    return gc_alloc_with_region(nbytes, page_type_flag, my_region, quick_p);
}
#else
extern void *gc_general_alloc(long nbytes,int page_type_flag,int quick_p);
#endif

extern long (*scavtab[256])(lispobj *where, lispobj object);
extern lispobj (*transother[256])(lispobj object);
extern long (*sizetab[256])(lispobj *where);

extern struct weak_pointer *weak_pointers; /* in gc-common.c */
extern struct hash_table *weak_hash_tables; /* in gc-common.c */

extern void scavenge(lispobj *start, long n_words);
extern void scavenge_interrupt_contexts(struct thread *thread);
extern void scav_weak_hash_tables(void);
extern void scan_weak_hash_tables(void);
extern void scan_weak_pointers(void);

lispobj  copy_large_unboxed_object(lispobj object, long nwords);
lispobj  copy_unboxed_object(lispobj object, long nwords);
lispobj  copy_large_object(lispobj object, long nwords);
lispobj  copy_object(lispobj object, long nwords);
lispobj  copy_code_object(lispobj object, long nwords);

lispobj *search_read_only_space(void *pointer);
lispobj *search_static_space(void *pointer);
lispobj *search_dynamic_space(void *pointer);

lispobj *gc_search_space(lispobj *start, size_t words, lispobj *pointer);

extern void scrub_control_stack();

#include "fixnump.h"

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#else
#include "cheneygc-internal.h"
#endif

#if N_WORD_BITS == 32
# define SIMPLE_ARRAY_WORD_WIDETAG SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG
#elif N_WORD_BITS == 64
# define SIMPLE_ARRAY_WORD_WIDETAG SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG
#endif

#endif /* _GC_INTERNAL_H_ */
