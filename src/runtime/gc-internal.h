/*
 * garbage collection - shared definitions for modules "inside" the GC system
 *
 * Despite the preceding claim, this header is a bit of a mashup of things
 * that are "internal to strictly GC" vs "for all SBCL-internal C code"
 * as opposed to gc.h which is some kind of external API,
 * though it's unclear for what, since hardly anything includes it.
 * GC-internal pieces that don't need to be revealed more widely
 * should be declared in 'gc-private.h'
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

#include "gc-assert.h"
#include "code.h"
extern const char *widetag_names[];

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#else
#include "cheneygc-internal.h"
#endif

#include "align.h"

// Distinguish penultimate GC (iteration 1) from ultimate GC (iteration 2) in save-lisp
extern int save_lisp_gc_iteration;

// This bit can be anything that doesn't conflict with a bit seen by lisp.
// Byte index 0 is the widetag, byte indices 1 and 2 are for the array-rank
// and vector-flags, depending on how src/compiler/generic/early-objdef assigns them.
#define WEAK_VECTOR_VISITED_BIT (1<<30)

// Assert that the 'v' is a visited weak object, and then clear the visited bit.
#define UNSET_WEAK_VECTOR_VISITED(v) \
  gc_assert(v->header & WEAK_VECTOR_VISITED_BIT); v->header ^= WEAK_VECTOR_VISITED_BIT

lispobj *gc_search_space3(void *pointer, lispobj *start, void *limit);
static inline lispobj *gc_search_space(lispobj *start, void *pointer) {
    return gc_search_space3(pointer,
                            start,
                            (void*)(1+((lispobj)pointer | LOWTAG_MASK)));
}

#ifdef LISP_FEATURE_X86
void gencgc_apply_code_fixups(struct code *old_code, struct code *new_code);
#else
#define gencgc_apply_code_fixups(ignore1,ignore2)
#endif

extern void gc_close_collector_regions(int);

/* The various sorts of pointer swizzling in SBCL. */
enum source {
  SOURCE_NORMAL,
  SOURCE_ZERO_TAG,              /* code, lflist */
  SOURCE_CLOSURE,
  SOURCE_SYMBOL_NAME,
  SOURCE_FDEFN_RAW
};

#endif /* _GC_INTERNAL_H_ */
