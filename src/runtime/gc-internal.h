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
#define NWORDS(x,y) (CEILING((x),(y)) / (y))

/* FIXME: Shouldn't this be defined in sbcl.h? */
#define FUN_RAW_ADDR_OFFSET (6*sizeof(lispobj) - FUN_POINTER_LOWTAG)

/* values for the *_alloc_* parameters */
#define FREE_PAGE 0
#define BOXED_PAGE 1
#define UNBOXED_PAGE 2
#define OPEN_REGION_PAGE 4

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

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#else
#include "cheneygc-internal.h"
#endif

#endif /* _GC_INTERNAL_H_ */
