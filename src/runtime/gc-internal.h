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

#include "genesis/simple-fun.h"
#include "thread.h"
#include "interr.h" /* for lose() */
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

// For x86[-64], arm64, a simple-fun or closure's "self" slot is a fixum
// On other backends, it is a lisp ointer.
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || defined(LISP_FEATURE_ARM64)
#define FUN_SELF_FIXNUM_TAGGED 1
#else
#define FUN_SELF_FIXNUM_TAGGED 0
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

extern void scrub_control_stack(void);
extern void scrub_thread_control_stack(struct thread *);

#ifdef LISP_FEATURE_X86
void gencgc_apply_code_fixups(struct code *old_code, struct code *new_code);
#else
#define gencgc_apply_code_fixups(ignore1,ignore2)
#endif

#if N_WORD_BITS == 32
# define SIMPLE_ARRAY_WORD_WIDETAG SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG
#elif N_WORD_BITS == 64
# define SIMPLE_ARRAY_WORD_WIDETAG SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG
#endif

extern void
instance_scan(void (*proc)(lispobj*, sword_t, uword_t),
              lispobj *instance_ptr, sword_t n_words,
              lispobj bitmap, uword_t arg);

extern lispobj decode_fdefn_rawfun(struct fdefn *fdefn);
extern void gc_close_thread_regions(struct thread*, int);
extern void gc_close_collector_regions(int);
#endif /* _GC_INTERNAL_H_ */
