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
#include "interr.h"

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#else
#include "cheneygc-internal.h"
#endif

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

static inline uword_t
NWORDS(uword_t x, uword_t n_bits)
{
    /* A good compiler should be able to constant-fold this whole thing,
       even with the conditional. */
    if(n_bits <= N_WORD_BITS) {
        uword_t elements_per_word = N_WORD_BITS/n_bits;

        return CEILING(x, elements_per_word)/elements_per_word;
    }
    else {
        /* FIXME: should have some sort of assertion that N_WORD_BITS
           evenly divides n_bits */
        return x * (n_bits/N_WORD_BITS);
    }
}

/* FIXME: Shouldn't this be defined in sbcl.h? */

#if defined(LISP_FEATURE_SPARC) || defined(LISP_FEATURE_ARM)
#define FUN_RAW_ADDR_OFFSET 0
#else
#define FUN_RAW_ADDR_OFFSET (offsetof(struct simple_fun, code) - FUN_POINTER_LOWTAG)
#endif

/* values for the *_alloc_* parameters, also see the commentary for
 * struct page in gencgc-internal.h.  FIXME: Perhaps these constants
 * should be there, or at least defined on gencgc only? */
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
gc_alloc_with_region(sword_t nbytes,int page_type_flag, struct alloc_region *my_region,
                     int quick_p);
static inline void *
gc_general_alloc(sword_t nbytes, int page_type_flag, int quick_p)
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
extern void *gc_general_alloc(word_t nbytes,int page_type_flag,int quick_p);
#endif

static inline lispobj
gc_general_copy_object(lispobj object, long nwords, int page_type_flag)
{
    lispobj *new;

    gc_assert(is_lisp_pointer(object));
    gc_assert(from_space_p(object));
    gc_assert((nwords & 0x01) == 0);

    /* Allocate space. */
    new = gc_general_alloc(nwords*N_WORD_BYTES, page_type_flag, ALLOC_QUICK);

    /* Copy the object. */
    memcpy(new,native_pointer(object),nwords*N_WORD_BYTES);

    return make_lispobj(new, lowtag_of(object));
}

extern sword_t (*scavtab[256])(lispobj *where, lispobj object);
extern lispobj (*transother[256])(lispobj object);
extern sword_t (*sizetab[256])(lispobj *where);

extern struct weak_pointer *weak_pointers; /* in gc-common.c */
extern struct hash_table *weak_hash_tables; /* in gc-common.c */

extern void scavenge(lispobj *start, sword_t n_words);
extern void scavenge_interrupt_contexts(struct thread *thread);
extern void scav_weak_hash_tables(void);
extern void scan_weak_hash_tables(void);
extern void scan_weak_pointers(void);

lispobj  copy_large_unboxed_object(lispobj object, sword_t nwords);
lispobj  copy_unboxed_object(lispobj object, sword_t nwords);
lispobj  copy_large_object(lispobj object, sword_t nwords);
lispobj  copy_object(lispobj object, sword_t nwords);
lispobj  copy_code_object(lispobj object, sword_t nwords);

lispobj *search_read_only_space(void *pointer);
lispobj *search_static_space(void *pointer);
lispobj *search_dynamic_space(void *pointer);

lispobj *gc_search_space(lispobj *start, size_t words, lispobj *pointer);

extern int looks_like_valid_lisp_pointer_p(lispobj pointer, lispobj *start_addr);

extern void scavenge_control_stack(struct thread *th);
extern void scrub_control_stack(void);
extern void scrub_thread_control_stack(struct thread *);

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
