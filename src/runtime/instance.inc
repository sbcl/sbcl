#include <stddef.h> // for offsetof()
#include "align.h"

// This is index of the bit that differentiates FUNCALLABLE_INSTANCE_WIDETAG
// from INSTANCE_WIDETAG.
#define FUNINSTANCE_SELECTOR_BIT_NUMBER 2

// Return 1 if widetag is either for INSTANCE or FUNCALLABLE-INSTANCE
static inline int instanceoid_widetag_p(unsigned char widetag) {
    return (widetag | (1<<FUNINSTANCE_SELECTOR_BIT_NUMBER)) == FUNCALLABLE_INSTANCE_WIDETAG;
}

/* This is NOT the same value that lisp's %INSTANCE-LENGTH returns.
 * Lisp always uses the logical length (as originally allocated),
 * except when heap-walking which requires exact physical sizes */
static inline int instance_length(lispobj header)
{
    // * Byte 3 of an instance header word holds the immobile gen# and visited bit,
    //   so those have to be masked off.
    // * If the object is in hashed-and-moved state and the original instance payload
    //   length was odd (total object length was even), then add 1.
    //   This can be detected by ANDing some bits, bit 10 being the least-significant
    //   bit of the original size, and bit 9 being the 'hashed+moved' bit.
    // * 64-bit machines do not need 'long' right-shifts, so truncate to int.

    int extra = ((unsigned int)header >> 10) & ((unsigned int)header >> 9) & 1;
    return (((unsigned int)header >> INSTANCE_LENGTH_SHIFT) & INSTANCE_LENGTH_MASK) + extra;
}

// Calculate length of an object that is either INSTANCE or FUNCALLABLE-INSTANCE
static inline int instanceoid_length(lispobj header) {
    return (header & (1<<FUNINSTANCE_SELECTOR_BIT_NUMBER))
        ? (int)(HeaderValue(header) & SHORT_HEADER_MAX_WORDS) : instance_length(header);
}

/// instance_layout() and layout_of() macros takes a lispobj* and are lvalues
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER

# ifdef LISP_FEATURE_LITTLE_ENDIAN
#  define instance_layout(native_ptr) ((uint32_t*)(native_ptr))[1]
# else
#  error "No instance_layout() defined"
# endif
# define funinstance_layout(native_ptr) instance_layout(native_ptr)
// generalize over either metatype, but not as general as SB-KERNEL:LAYOUT-OF
# define layout_of(native_ptr) instance_layout(native_ptr)

#else

// first 2 words of ordinary instance are: header, layout
# define instance_layout(native_ptr) ((struct instance*)native_ptr)->slots[0]
// first 4 words of funcallable instance are: header, trampoline, layout, fin-fun
// unless funinstances contains executable bytes, in which case it's a little different.
# define funinstance_layout(native_ptr) \
   ((struct funcallable_instance*)native_ptr)->layout
// This macro is complicated so that it remains an lvalue.
# define layout_of(native_ptr) \
  ((lispobj*)native_ptr)[(widetag_of(native_ptr)&(1<<FUNINSTANCE_SELECTOR_BIT_NUMBER))? \
       offsetof(struct funcallable_instance,layout)>>WORD_SHIFT:1]

#endif

static inline int layout_depth2_id(struct layout* layout) {
    int32_t* vector = (int32_t*)&layout->uw_id_word0;
    return vector[0];
}
static inline int layout_depthN_id(int n, struct layout* layout) {
    int32_t* vector = (int32_t*)&layout->uw_id_word0;
    // depthoids 0 and 1 are not stored because they have to be T and STRUCTURE-OBJECT,
    // so the array index is off by 2.
    return vector[n-2];
}

/// Return true if 'thing' is a layout.
/// This predicate is careful, as is it used to verify heap invariants.
static inline int layoutp(lispobj thing)
{
    lispobj layout;
    if (lowtag_of(thing) != INSTANCE_POINTER_LOWTAG) return 0;
    if ((layout = instance_layout(INSTANCE(thing))) == 0) return 0;
    return layout_depth2_id(LAYOUT(layout)) == LAYOUT_LAYOUT_ID;
}
/// Return true if 'thing' is the layout of any subtype of sb-lockless::list-node.
static inline int lockfree_list_node_layout_p(struct layout* layout) {
    return layout_depth2_id(layout) == LFLIST_NODE_LAYOUT_ID;
}
/// Return true if 'thing' is a finalizer list node
/// FINALIZER-NODE is-a SO-DATA-NODE is-a SO-KEY-NODE is-a SO-NODE is-a
///    LIST-NODE is-a STRUCTURE-OBJECT
/// so layout depthoid 6 is the one to check the ID of.
static inline int finalizer_node_layout_p(struct layout* layout) {
    return layout_depthN_id(6, layout) == FINALIZER_NODE_LAYOUT_ID;
}

struct bitmap { sword_t *bits; unsigned int nwords; };
static inline struct bitmap get_layout_bitmap(struct layout* layout)
{
    struct bitmap bitmap;
    const int layout_id_vector_fixed_capacity = 7;
#ifdef LISP_FEATURE_64_BIT
    sword_t depthoid = layout->sw_flags;
    // Depthoid is stored in the upper 4 bytes of 'flags', as a fixnum.
    depthoid >>= (32 + N_FIXNUM_TAG_BITS);
    int extra_id_words =
      (depthoid > layout_id_vector_fixed_capacity) ?
      ALIGN_UP(depthoid - layout_id_vector_fixed_capacity, 2) / 2 : 0;
#else
    sword_t depthoid = fixnum_value(layout->depthoid);
    int extra_id_words = (depthoid > layout_id_vector_fixed_capacity) ?
      depthoid - layout_id_vector_fixed_capacity : 0;
#endif
    // The 2 bits for stable address-based hashing can't ever bet set
    // in an instance of LAYOUT.
    const int baseline_payload_words = (sizeof (struct layout) / N_WORD_BYTES) - 1;
    int payload_words = ((unsigned int)layout->header >> INSTANCE_LENGTH_SHIFT)
                        & INSTANCE_LENGTH_MASK;
    bitmap.bits = (sword_t*)((char*)layout + sizeof (struct layout)) + extra_id_words;
    bitmap.nwords = payload_words - baseline_payload_words - extra_id_words;
    return bitmap;
}

/* Return true if the INDEXth bit is set in BITMAP. Index 0 corresponds to the word
 * immediately following the instance header word, which is the instance's layout
 * if #-compact-instance-header */
static inline int bitmap_logbitp(unsigned int index, struct bitmap bitmap)
{
    unsigned int word_index = index / N_WORD_BITS;
    unsigned int bit_index  = index % N_WORD_BITS;
    if (word_index >= bitmap.nwords) return bitmap.bits[bitmap.nwords-1] < 0;
    return (bitmap.bits[word_index] >> bit_index) & 1;
}

/* KLUDGE: this would be autogenerated, but now that use of DEF!STRUCT
 * has been reduced to the bare minimum needed for bootstrapping,
 * CTYPE and its descendants types are not known to 1st genesis */
struct classoid {
    lispobj header; // = word_0_
#ifndef LISP_FEATURE_COMPACT_INSTANCE_HEADER
    lispobj _layout;
#endif
    lispobj bits; // from CTYPE
    lispobj name;
    lispobj layout;
    /* more slots */
};
