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

/* disabling gc assertions made no discernable difference to GC speed,
 * last I tried it - dan 2003.12.21 */
#if 1
#define gc_assert(ex) do { \
	if (!(ex)) gc_abort(); \
} while (0)
#else
#define gc_assert(ex)
#endif
#define gc_abort() lose("GC invariant lost, file \"%s\", line %d", \
			__FILE__, __LINE__)

#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))

static inline unsigned int
NWORDS(unsigned int x, unsigned int n_bits)
{
    unsigned int elements_per_word = N_WORD_BITS/n_bits;

    return CEILING(x, elements_per_word)/elements_per_word;
}

/* FIXME: Shouldn't this be defined in sbcl.h? */
#define FUN_RAW_ADDR_OFFSET (6*sizeof(lispobj) - FUN_POINTER_LOWTAG)

/* values for the *_alloc_* parameters */
#define FREE_PAGE_FLAG 0
#define BOXED_PAGE_FLAG 1
#define UNBOXED_PAGE_FLAG 2
#define OPEN_REGION_PAGE_FLAG 4

#define ALLOC_BOXED 0
#define ALLOC_UNBOXED 1
#define ALLOC_QUICK 1

void *gc_general_alloc(int nbytes,int unboxed_p,int quick_p);

extern int (*scavtab[256])(lispobj *where, lispobj object);
extern lispobj (*transother[256])(lispobj object);
extern int (*sizetab[256])(lispobj *where);

extern struct weak_pointer *weak_pointers; /* in gc-common.c */

extern void scavenge(lispobj *start, long n_words);
extern void scan_weak_pointers(void);

lispobj  copy_large_unboxed_object(lispobj object, int nwords);
lispobj  copy_unboxed_object(lispobj object, int nwords);
lispobj  copy_large_object(lispobj object, int nwords);
lispobj  copy_object(lispobj object, int nwords);

lispobj *search_read_only_space(void *pointer);
lispobj *search_static_space(void *pointer);
lispobj *search_dynamic_space(void *pointer);

/* Scan an area looking for an object which encloses the given pointer.
 * Return the object start on success or NULL on failure. */
static lispobj *
search_space(lispobj *start, size_t words, lispobj *pointer)
{
    while (words > 0) {
	size_t count = 1;
	lispobj thing = *start;

	/* If thing is an immediate then this is a cons. */
	if (is_lisp_pointer(thing)
	    || ((thing & 3) == 0) /* fixnum */
	    || (widetag_of(thing) == CHARACTER_WIDETAG)
	    || (widetag_of(thing) == UNBOUND_MARKER_WIDETAG))
	    count = 2;
	else
	    count = (sizetab[widetag_of(thing)])(start);

	/* Check whether the pointer is within this object. */
	if ((pointer >= start) && (pointer < (start+count))) {
	    /* found it! */
	    /*FSHOW((stderr,"/found %x in %x %x\n", pointer, start, thing));*/
	    return(start);
	}

	/* Round up the count. */
	count = CEILING(count,2);

	start += count;
	words -= count;
    }
    return (NULL);
}

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#else
#include "cheneygc-internal.h"
#endif

#endif /* _GC_INTERNAL_H_ */
