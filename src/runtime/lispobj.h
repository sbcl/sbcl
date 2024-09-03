#ifndef _RUNTIME_LISPOBJ_H_
#define _RUNTIME_LISPOBJ_H_

#include <stdint.h>
typedef intptr_t sword_t;
typedef uintptr_t uword_t;
typedef uword_t lispobj;

#include "genesis/sbcl.h"

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
#ifdef LISP_FEATURE_BIG_ENDIAN
# define assign_widetag(addr, byte) ((unsigned char*)addr)[N_WORD_BYTES-1] = byte
#else
# define assign_widetag(addr, byte) *(unsigned char*)addr = byte
#endif

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
static inline int non_nil_symbol_p(lispobj x) {
    return lowtag_of(x) == OTHER_POINTER_LOWTAG
      && widetag_of((lispobj*)(x-OTHER_POINTER_LOWTAG)) == SYMBOL_WIDETAG;
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

static inline int simple_base_string_p(lispobj x) {
    // caller should ensure that 'x' is a headered object
    return widetag_of(native_pointer(x)) == SIMPLE_BASE_STRING_WIDETAG;
}
static inline int string_widetag_p(int widetag)
{
    return
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
        widetag == SIMPLE_CHARACTER_STRING_WIDETAG ||
#endif
        widetag == SIMPLE_BASE_STRING_WIDETAG;
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

static inline lispobj compute_lispobj(lispobj* base_addr) {
    return make_lispobj(base_addr, LOWTAG_FOR_WIDETAG(*base_addr & WIDETAG_MASK));
}

// is_header() and is_cons_half() are logical complements when invoked
// on the first word of any lisp object. However, given a word which is
// only *potentially* the first word of a lisp object, they can both be false.
// In ambiguous root detection, is_cons_half() is to be used, as it is the more
// stringent check. The set of valid bit patterns in the low byte of the car
// of a cons is smaller than the set of patterns accepted by !is_header().
static inline int is_header(lispobj potential_header_word) {
    return widetag_lowtag[potential_header_word & WIDETAG_MASK] & 0x80;
}

extern sword_t (*sizetab[256])(lispobj *where);
typedef sword_t (*sizerfn)(lispobj*);
static inline sword_t object_size(lispobj* where) {
    sizerfn f = sizetab[widetag_of(where)];
    return f ? f(where) : CONS_SIZE;
}
// These three variants are potentially more efficient -
//  (1) if the widetag was loaded, avoids one memory read
//  (2) if you know the object isn't a cons, use headerobj_size
//  (3) both of the above pertain.
// Cases 1 and 3 exist only because C doesn't have optional args.
// These might be premature optimizations, I really don't know.
static inline sword_t object_size2(lispobj* where, unsigned int header) {
    sizerfn f = sizetab[header & WIDETAG_MASK];
    return f ? f(where) : CONS_SIZE;
}
static inline sword_t headerobj_size(lispobj* where) {
    return sizetab[widetag_of(where)](where);
}
static inline sword_t headerobj_size2(lispobj* where, unsigned int header) {
    return sizetab[header & WIDETAG_MASK](where);
}

#ifdef LISP_FEATURE_64_BIT
# define make_filler_header(n) (((uword_t)(n)<<32)|FILLER_WIDETAG)
# define filler_total_nwords(header) ((header)>>32)
#else
# define make_filler_header(n) (((n)<<N_WIDETAG_BITS)|FILLER_WIDETAG)
# define filler_total_nwords(header) ((header)>>N_WIDETAG_BITS)
#endif

/*
 * Predicates rather than bit extractors should be used to test the flags
 * in a vector header, because:
 *
 * - while trying to place the flags into a different header byte, I found it
 *   unobvious whether to treat flags as part of the "Header data" (which is a
 *   3-byte or 7-byte wide field starting at bit 8) versus the entire "Header word".
 *   So e.g. if the Lisp VECTOR-WEAK value were redefined to #x0100, which would
 *   place a 1 bit into byte index 3 (using SET-HEADER-DATA), it isn't clear that
 *   "vector_flags(vector) == vectorWeak" is the proper test, because vector_flags()
 *   could reasonably be defined to right-shift by 0, 8, or 16 bits.
 *   (i.e. leave the bits where they are, but mask out the widetag; or make them
 *   act like "Header data"; or right-align as if we had Lisp bitfield extractors)
 *   Looked at differently, the natural values for the first 3 flag bits should be
 *   1, 2, and 4 but this would force you to write expressions such as:
 *    (SET-HEADER-DATA V (ASH SB-VM:VECTOR-HASHING-FLAG SB-VM:VECTOR-FLAG-BITS-SHIFT))
 *   which looks to be terribly inconvenient for Lisp.
 *   Alternatively, the constants can be defined as their "natural" values for C
 *   which would have flag_VectorWeak = 0x010000, but then you need the inverse
 *   shift in Lisp which expects SET-HEADER-DATA to get #x0100 as the argument.
 *   Hypothetically, that is.
 *
 * - With smarter macros it ought to be possible to avoid 8-byte loads and shifts.
 *   They would need to be endian-aware, which I didn't want to do just yet.
 */
#define vector_flagp(header, val) ((int)header & (flag_##val << ARRAY_FLAGS_POSITION))
#define vector_flags_zerop(header) ((int)(header) & 0x07 << ARRAY_FLAGS_POSITION) == 0
// Return true if vector is a weak vector that is not a hash-table <k,v> vector.
static inline int vector_is_weak_not_hashing_p(unsigned int header) {
    return (header & ((flag_VectorWeak|flag_VectorHashing) << ARRAY_FLAGS_POSITION)) ==
      flag_VectorWeak << ARRAY_FLAGS_POSITION;
}

#endif
