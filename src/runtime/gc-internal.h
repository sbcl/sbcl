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

#include "genesis/code.h"
#include "genesis/simple-fun.h"
#include "thread.h"
#include "interr.h" /* for lose() */
#include "gc-assert.h"
#include "code.h"
extern const char *widetag_names[];
extern struct weak_pointer *weak_pointer_chain; /* in gc-common.c */

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#else
#include "cheneygc-internal.h"
#endif

#include "align.h"

// Offset from an fdefn raw address to the underlying simple-fun,
// if and only if it points to a simple-fun.
// For those of us who are too memory-impaired to know how to use the value:
//  - it is the amount to ADD to a tagged simple-fun pointer to get its entry address
//  - or the amount to SUBTRACT from an entry address to get a tagged fun pointer
#if defined(LISP_FEATURE_SPARC) || defined(LISP_FEATURE_ARM) || defined(LISP_FEATURE_RISCV)
#define FUN_RAW_ADDR_OFFSET 0
#else
#define FUN_RAW_ADDR_OFFSET (offsetof(struct simple_fun, insts) - FUN_POINTER_LOWTAG)
#endif

// For x86[-64], a simple-fun or closure's "self" slot is a fixum
// On other backends, it is a lisp ointer.
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
#define FUN_SELF_FIXNUM_TAGGED 1
#else
#define FUN_SELF_FIXNUM_TAGGED 0
#endif

// Return only the lisp-visible vector header flag bits,
// masking out flag_VectorWeakVisited.
#define vector_flags(header) (HeaderValue(header) & 7)
// Test for presence of a bit in vector's header.
#define vector_flagp(header, val) (HeaderValue(header) & flag_##val)

// Mask out the fullcgc mark bit when asserting header validity
#define UNSET_WEAK_VECTOR_VISITED(v) \
  gc_assert((v->header & 0xffff) == \
    (((flag_VectorWeakVisited|flag_VectorWeak) << N_WIDETAG_BITS) \
     | SIMPLE_VECTOR_WIDETAG)); \
  v->header ^= flag_VectorWeakVisited << N_WIDETAG_BITS

/* values for the *_alloc_* parameters, also see the commentary for
 * struct page in gencgc-internal.h. These constants are used in gc-common,
 * so they can't easily be made gencgc-only */
#define FREE_PAGE_FLAG        0
#define PAGE_TYPE_MASK        7 // mask out the 'single-object flag'
/* Note: MAP-ALLOCATED-OBJECTS expects this value to be 1 */
#define BOXED_PAGE_FLAG       1
#define UNBOXED_PAGE_FLAG     2
/* CONS_PAGE_FLAG doesn't get stored in the page table, though I am considering
 * doing that. If conses went on segregated pages, then testing for a valid
 * conservative root on a cons page is as simple as seeing whether the address
 * is correctly aligned and lowtagged.
 * Also, we could reserve bytes at the end of each page to act as a mark bitmap
 * which is useful since conses are headerless objects, and one GC strategy
 * demands mark bitmaps which are currently placed in a side table.
 * That would unfortunately complicate the task of allocating a huge list,
 * because hitting the line of demarcation between conses and the mark bits would
 * require chaining the final cons to another page of conses and so on. */
#define CONS_PAGE_FLAG        4
#define OPEN_REGION_PAGE_FLAG 8
#define CODE_PAGE_TYPE        (BOXED_PAGE_FLAG|UNBOXED_PAGE_FLAG)

extern sword_t (*sizetab[256])(lispobj *where);
#define OBJECT_SIZE(header,where) \
  (is_header(header)?sizetab[header_widetag(header)](where):CONS_SIZE)

lispobj *gc_search_space3(void *pointer, lispobj *start, void *limit);
static inline lispobj *gc_search_space(lispobj *start, void *pointer) {
    return gc_search_space3(pointer,
                            start,
                            (void*)(1+((lispobj)pointer | LOWTAG_MASK)));
}

struct vector *symbol_name(lispobj*);

extern void scrub_control_stack(void);
extern void scrub_thread_control_stack(struct thread *);

#ifdef LISP_FEATURE_X86
void gencgc_apply_code_fixups(struct code *old_code, struct code *new_code);
#else
#define gencgc_apply_code_fixups(ignore1,ignore2)
#endif

#include "fixnump.h"

#if N_WORD_BITS == 32
# define SIMPLE_ARRAY_WORD_WIDETAG SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG
#elif N_WORD_BITS == 64
# define SIMPLE_ARRAY_WORD_WIDETAG SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG
#endif

extern void
instance_scan(void (*proc)(lispobj*, sword_t, uword_t),
              lispobj *instance_ptr, sword_t n_words,
              lispobj bitmap, uword_t arg);

extern int simple_fun_index(struct code*, struct simple_fun*);

extern lispobj fdefn_callee_lispobj(struct fdefn *fdefn);

boolean valid_widetag_p(unsigned char widetag);

#endif /* _GC_INTERNAL_H_ */
