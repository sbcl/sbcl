#ifndef _RUNTIME_LISPOBJ_H_
#define _RUNTIME_LISPOBJ_H_

#include <stdint.h>
typedef intptr_t sword_t;
typedef uintptr_t uword_t;
typedef uword_t lispobj;

#include "sbcl.h"

static inline int fixnump(lispobj obj)
{
    return((obj & FIXNUM_TAG_MASK) == 0);
}

static inline int header_widetag(lispobj obj)
{
    return obj & WIDETAG_MASK;
}

static inline uword_t HeaderValue(lispobj obj)
{
    return obj >> N_WIDETAG_BITS;
}

/* Is the Lisp object obj something with immediate nature (e.g. a
 * fixnum or character or unbound marker)? */
static inline int
is_lisp_immediate(lispobj obj)
{
    int widetag;
    return (fixnump(obj)
            || ((widetag = header_widetag(obj)) == CHARACTER_WIDETAG)
#if N_WORD_BITS == 64
            || (widetag == SINGLE_FLOAT_WIDETAG)
#endif
            || (widetag == UNBOUND_MARKER_WIDETAG));
}

static inline int lowtag_of(lispobj obj)
{
    return obj & LOWTAG_MASK;
}

static inline int widetag_of(lispobj* obj)
{
#ifdef LISP_FEATURE_BIG_ENDIAN
    return ((unsigned char*)obj)[N_WORD_BYTES-1];
#else
    return *(unsigned char*)obj;
#endif
}

static inline int listp(lispobj obj) {
    return lowtag_of(obj) == LIST_POINTER_LOWTAG;
}
static inline int instancep(lispobj obj) {
    return lowtag_of(obj) == INSTANCE_POINTER_LOWTAG;
}
static inline int functionp(lispobj obj) {
    return lowtag_of(obj) == FUN_POINTER_LOWTAG;
}
static inline int other_pointer_p(lispobj obj) {
    return lowtag_of(obj) == OTHER_POINTER_LOWTAG;
}
static inline int simple_vector_p(lispobj obj) {
    return other_pointer_p(obj) &&
           widetag_of((lispobj*)(obj-OTHER_POINTER_LOWTAG)) == SIMPLE_VECTOR_WIDETAG;
}

/* Convert from a lispobj with type bits to a native (ordinary
 * C/assembly) pointer to the beginning of the object. */
static inline lispobj *
native_pointer(lispobj obj)
{
    return (lispobj *) ((uintptr_t) (obj & ~LOWTAG_MASK));
}

/* inverse operation: create a suitably tagged lispobj from a native pointer. */
static inline lispobj
make_lispobj(void *o, int low_tag)
{
    return (lispobj)o | low_tag;
}

#define make_fixnum(n) ((uword_t)(n) << N_FIXNUM_TAG_BITS)

static inline sword_t
fixnum_value(lispobj n)
{
    return (sword_t)n >> N_FIXNUM_TAG_BITS;
}

/* Is the Lisp object obj something with pointer nature (as opposed to
 * e.g. a fixnum or character or unbound marker)? */
static inline int is_lisp_pointer(lispobj obj)
{
#ifdef LISP_FEATURE_PPC64
    return (obj & 5) == 4;
#elif N_WORD_BITS == 64
    return (obj & 3) == 3;
#else
    return obj & 1;
#endif
}

static inline int is_cons_half(lispobj obj)
{
    if (fixnump(obj) || is_lisp_pointer(obj)) return 1;
    int widetag = header_widetag(obj);
    return widetag == CHARACTER_WIDETAG ||
#if N_WORD_BITS == 64
           widetag == SINGLE_FLOAT_WIDETAG ||
#endif
           widetag == UNBOUND_MARKER_WIDETAG;
}

/* Each size category is designed to allow for some flag bits, and the payload
 * length in words, which is always an odd number so that total word count is even.
 * There are three size categories for most non-vector objects,
 * differing in how many flag bits versus size bits there are.
 */

/* The largest payload count is expressed in 23 bits. These objects
 * can't reside in immobile space as there is no room for generation bits.
 * All sorts of objects fall into this category, but mostly due to inertia.
 * There are no non-vector boxed objects whose size should be so large.
 * Header:   size |    tag
 *          -----   ------
 *        23 bits | 8 bits
 */
#define BOXED_NWORDS(obj) ((HeaderValue(obj) & 0x7FFFFF) | 1)

/* Medium-sized payload count is expressed in 15 bits. Objects in this category
 * may reside in immobile space: CLOSURE, FUNCALLABLE-INSTANCE.
 * The single data bit is used as a closure's NAMED flag.
 *
 * Header:  gen# |  data |     size |    tag
 *         -----   -----    -------   ------
 *        8 bits | 1 bit |  15 bits | 8 bits
 */
#define SHORT_BOXED_NWORDS(obj) ((HeaderValue(obj) & SHORT_HEADER_MAX_WORDS) | 1)

/* Tiny payload count is expressed in 8 bits. Objects in this size category
 * can reside in immobile space: SYMBOL, FDEFN.
 * Header:  gen# | flags |   size |    tag
 *         -----   ------  ------   ------
 *        8 bits   8 bits  8 bits | 8 bits
 * FDEFN  flag bits: 1 bit for statically-linked
 * SYMBOL flag bits: 1 bit for present in initial core image
 */
#define TINY_BOXED_NWORDS(obj) ((HeaderValue(obj) & 0xFF) | 1)

// other_immediate_lowtag_p is the least strict of the tests for whether a word
// is potentially an object header, merely checking whether the bits fit the general
// pattern of header widetags without regard for whether some headered object type
// could in fact have those exact low bits. Specifically, this falsely returns 1
// for UNBOUND_MARKER_WIDETAG, CHARACTER_WIDETAG, and on 64-bit machines,
// SINGLE_FLOAT_WIDETAG; as well as unallocated and unused widetags (e.g. LRA on x86)
// none of which denote the start of a headered object.
// The ambiguous cases are for words would start a cons - the three mentioned above.
// Other cases (NO_TLS_VALUE_MARKER_WIDETAG and other things) do not cause a problem
// in practice because they can't be the first word of a lisp object.
static inline int other_immediate_lowtag_p(lispobj header)
{
    /* These lowtags are spaced 4 apart throughout the lowtag space. */
    return (lowtag_of(header) & 3) == OTHER_IMMEDIATE_0_LOWTAG;
}

// widetag_lowtag encodes in the sign bit whether the byte corresponds
// to a headered object, and in the low bits the lowtag of a tagged pointer
// pointing to this object, be it headered or a cons.
extern unsigned char widetag_lowtag[256];
#define LOWTAG_FOR_WIDETAG(x) (widetag_lowtag[x] & LOWTAG_MASK)

// is_header() and is_cons_half() are logical complements when invoked
// on the first word of any lisp object. However, given a word which is
// only *potentially* the first word of a lisp object, they can both be false.
// In ambiguous root detection, is_cons_half() is to be used, as it is the more
// stringent check. The set of valid bit patterns in the low byte of the car
// of a cons is smaller than the set of patterns accepted by !is_header().
static inline int is_header(lispobj potential_header_word) {
    return widetag_lowtag[potential_header_word & WIDETAG_MASK] & 0x80;
}

#endif
