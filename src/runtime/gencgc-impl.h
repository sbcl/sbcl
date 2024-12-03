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

#ifndef GENCGC_IS_PRECISE
#error "GENCGC_IS_PRECISE must be #defined as 0 or 1"
#endif

/* One bit of page_words_t is the need_zerofill flag.
 * That leaves 15 bits to store page_words_used. This can represent
 * a page size of up to 64KiB on 32-bit and 128KiB on 64-bit.
 * Note that since the allocation quantum is actually 2 words
 * the words_used is always an even number, and so technically
 * we could store as "dualwords used" to achieve double the range */
#define GENCGC_PAGE_WORDS (GENCGC_PAGE_BYTES/N_WORD_BYTES)
#if GENCGC_PAGE_WORDS > 32767
#   error "GENCGC_PAGE_WORDS unexpectedly large."
#endif

#include "gencgc-alloc-region.h"
#include "genesis/cardmarks.h"
#include "genesis/cons.h"
#include "genesis/weak-pointer.h"
#include "globals.h" // for DYNAMIC_SPACE_START
#include "hopscotch.h"
#include <limits.h> // for UINT_MAX

#if N_WORD_BITS == 64
  // It's more economical to store scan_start_offset using 4 bytes than 8.
  // Doing so makes struct page fit in 8 bytes if words_used takes 2 bytes.
  //   scan_start_offset = 4
  //   words_used        = 2
  //   flags             = 1
  //   gen               = 1
  // If words_used takes 4 bytes, then the above is 10 bytes which is padded to
  // 12, which is still an improvement over the 16 that it would have been.
# define CONDENSED_PAGE_TABLE 1
#else
# define CONDENSED_PAGE_TABLE 0
#endif

typedef unsigned short page_words_t;

/* Note that this structure is also used from Lisp-side in
 * src/code/room.lisp, and the Lisp-side structure layout is currently
 * not groveled from C code but hardcoded. Any changes to the
 * structure layout need to be also made there.
 *
 * FIXME: We should probably just define this structure in Lisp, and
 * output the C version in genesis. -- JES, 2006-12-30.
 */
struct page {
    /* This is the offset from the first byte of some object in memory
     * prior to and no closer than the start of the page to the start
     * of the page.  Lower values here are better, 0 is ideal.  This
     * is useful for determining where to start when scanning forward
     * through a heap page (either for conservative root validation or
     * for scavenging). MUST be 0 for unallocated pages.
     */
#if CONDENSED_PAGE_TABLE
    // The low bit of the offset indicates the scale factor:
    // 0 = double-lispwords, 1 = gc cards. Large objects are page-aligned,
    // and this representation allows for a 32TB contiguous block using 32K
    // page size. Larger allocations will have pages that can't directly
    // store the full offset. That has to be dealt with by the accessor.
    unsigned int scan_start_offset_;
#else
    os_vm_size_t scan_start_offset_;
#endif

    /* the number of lispwords of this page that are used. This may be less
     * than the usage at an instant in time for pages within the current
     * allocation regions. The 0th bit of the physical uint16 indicates
     * that the page needs to be zero-filled for the next use.
     * Let the C compiler figure it out, so we can't get it wrong in C.
     * But we need to reverse the order of the packed fields depending on
     * endianness so that the Lisp side is easier to understand */
#ifdef LISP_FEATURE_BIG_ENDIAN
    page_words_t words_used_   : 15;
    page_words_t need_zerofill :  1;
#else
    page_words_t need_zerofill :  1;
    page_words_t words_used_   : 15;
#endif

    // !!! If bit positions are changed, be sure to reflect the changes into
    // page_extensible_p() as well as ALLOCATION-INFORMATION in sb-introspect
    // and WALK-DYNAMIC-SPACE.
        /*
         * The 4 low bits of 'type' are defined by PAGE_TYPE_x constants.
         *  0000 free
         *  ?001 strictly boxed data (pointers, immediates, object headers)
         *  ?010 strictly unboxed data
         *  ?011 mixed boxed/unboxed non-code objects
         *  ?111 code
         * The next two bits are SINGLE_OBJECT and OPEN_REGION.
         * The top two can be used for segregating objects by widetag
         * which will important once we have "destructors" to run for a
         * for a category of object, such as SYMBOL, hypothetically for
         * recycling TLS indices or something like that. */
    unsigned char type;

    /* the generation that this page belongs to. This should be valid
     * for all pages that may have objects allocated, even current
     * allocation region pages - this allows the space of an object to
     * be easily determined. */
    generation_index_t gen;
};
extern struct page *page_table;

/* New objects are allocated to PAGE_TYPE_MIXED or PAGE_TYPE_CONS */
/* If you change these constants, then possibly also change the following
 * functions in 'room.lisp':
 *  MAP-CODE-OBJECTS
 *  PRINT-ALL-CODE
 *  PRINT-LARGE-CODE
 *  PRINT-LARGE-UNBOXED
 */

// 4 bits for the type, 1 bit for SINGLE_OBJECT, 1 bit for OPEN_REGION

#define PAGE_TYPE_MASK        15 // mask out 'single-object' and 'open-region' flags
#define PAGE_TYPE_UNBOXED      1 // #b001
#define PAGE_TYPE_BOXED        2 // #b010
#define PAGE_TYPE_MIXED        3 // #b011
/* Small-Mixed pages hold objects that don't span cards. This is relatively easy to
 * arrange for by adding filler at the end of a card to align prior to the next object,
 * subject to a maximum allowable waste. Root scavenging can respect card boundaries
 * and use scavenge functions specific to each object - the best of both worlds */
#define PAGE_TYPE_SMALL_MIXED  4 // #b100
#define PAGE_TYPE_CONS         5 // #b101
#define PAGE_TYPE_CODE         7 // #b111
#define THREAD_PAGE_FLAG       8
#define SINGLE_OBJECT_FLAG    16
#define OPEN_REGION_PAGE_FLAG 32
#define FREE_PAGE_FLAG        0

/* As usually configured, generations 0-5 are normal collected generations,
   6 is pseudo-static (the objects in which are never moved nor reclaimed),
   and 7 is scratch space used when collecting a generation without promotion,
   wherein it is moved to generation 7 and back again.
 */
/*
 * SCRATCH_GENERATION as we've defined it is kinda stupid because "<"
 * doesn't do what you want. Other choices of value do, and since this an
 * enum, it should be possible to change. Except it isn't because .. reasons.
 * Here are some alternatives:
 *  A: gen 0 through 6 remain as-is and SCRATCH becomes -1
 *
 *  B: 1 = nursery, 2 = older, ... up through "old" 6 which becomes the new 7;
 *     and SCRATCH becomes 0. This is like alternative (A) but avoids negatives.
 *
 *  C: (probably the best)
 *  generations are stored with an implied decimal and one bit of fraction
 *  representing a half step so that:
 *     #b0000 = 0, #b0001 = 1/2   | #b0010 = 1, #b0011 = 1 1/2
 *     #b0100 = 2, #b0101 = 2 1/2 | #b0110 = 3, #b0111 = 3 1/2 ...
 *  up to 6 1/2. When GCing without promotion, we'd raise each object by half
 *  a generation, and then demote en masse, which is good because it makes the
 *  scratch pages older than from_space but younger than the youngest root gen.
 *
 * Of course, you could try to solve all this by keeping the existing numbering,
 * but expressing comparison "a < b" as either:
 *     "logical_gen(a) < logical_gen(b)" // re-map numerically before compare
 *  or "gen_lessp(a,b)" // just rename the comparator
 *
 * I generally prefer numeric comparison to just work, though we have a further
 * difficulty that page_table[page].gen is not always the generation of an object,
 * as when it is non-large and pinned. So the helpers might be needed anyway.
 */

enum {
    SCRATCH_GENERATION = PSEUDO_STATIC_GENERATION+1,
    NUM_GENERATIONS
};
#define ARTIFICIALLY_HIGH_GEN 8

/* a structure to hold the state of a generation
 *
 * CAUTION: If you modify this, make sure to touch up the alien
 * definition in src/code/gc.lisp accordingly. ...or better yes,
 * deal with the FIXME there...
 */
struct generation {
    /* the bytes allocated to this generation */
    os_vm_size_t bytes_allocated;

    /* the number of bytes at which to trigger a GC */
    os_vm_size_t gc_trigger;

    /* to calculate a new level for gc_trigger */
    os_vm_size_t bytes_consed_between_gc;

    /* the number of GCs since the last raise */
    int num_gc;

    /* the number of GCs to run on the generations before raising objects to the
     * next generation */
    int number_of_gcs_before_promotion;

    /* the cumulative sum of the bytes allocated to this generation. It is
     * cleared after a GC on this generations, and update before new
     * objects are added from a GC of a younger generation. Dividing by
     * the bytes_allocated will give the average age of the memory in
     * this generation since its last GC. */
    os_vm_size_t cum_sum_bytes_allocated;

    /* a minimum average memory age before a GC will occur helps
     * prevent a GC when a large number of new live objects have been
     * added, in which case a GC could be a waste of time */
    double minimum_age_before_gc;
};

extern struct generation generations[NUM_GENERATIONS];

struct __attribute__((packed)) corefile_pte {
  uword_t sso; // scan start offset
  page_words_t words_used; // low bit is the 'large object' flag
};

/* When computing a card index we never subtract the heap base, which simplifies
 * code generation. The heap base is guaranteed to be GC-page-aligned.
 * The low bits can wraparound from all 1s to all 0s such that lowest numbered
 * page index in linear order may have a higher card index.
 * Two small examples of the distinction between page index and card index.
 * For both examples: heap base = 0xEB00, page-size = 256b, npages = 8.
 *
 * Scenario A:
 *   CARDS_PER_PAGE = 1, card-size = 256b, ncards = 8, right-shift = 8, mask = #b111
 *     page     page      card
 *    index     base     index
 *       0      EB00        3
 *       1      EC00        4
 *       2      ED00        5
 *       3      EE00        6
 *       4      EF00        7
 *       5      F000        0
 *       6      F100        1
 *       7      F200        2
 *
 * Scenario B:
 *   CARDS_PER_PAGE = 2, card-size = 128b, ncards = 16, right-shift = 7, mask = #b1111
 *     page     page            card
 *    index     base         indices
 *       0      EB00, EB80      6, 7
 *       1      EC00, EC80      8, 9
 *       2      ED00, ED80    10, 11
 *       3      EE00, EE80    12, 13
 *       4      EF00, EF80    14, 15
 *       5      F000, F080      0, 1
 *       6      F100, F180      2, 3
 *       7      F200, F280      4, 5
 *
 */
extern unsigned char *gc_card_mark;
extern long gc_card_table_mask;
#define addr_to_card_index(addr) ((((uword_t)addr)>>GENCGC_CARD_SHIFT) & gc_card_table_mask)
#define page_to_card_index(n) addr_to_card_index(page_address(n))

extern const char *widetag_names[];

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

#include "code.h"
#ifdef LISP_FEATURE_X86
void gencgc_apply_code_fixups(struct code *old_code, struct code *new_code);
#else
#define gencgc_apply_code_fixups(ignore1,ignore2)
#endif

extern void gc_close_collector_regions(int);

#define SET_ALLOCATED_BIT(x)
void *collector_alloc_fallback(struct alloc_region*,sword_t,int);
static inline void* __attribute__((unused))
gc_general_alloc(struct alloc_region* region, sword_t nbytes, int page_type)
{
    void *new_obj = region->free_pointer;
    void *new_free_pointer = (char*)new_obj + nbytes;
    // Large objects will never fit in a region, so we automatically dtrt
    if (new_free_pointer <= region->end_addr) {
        region->free_pointer = new_free_pointer;
        return new_obj;
    }
    return collector_alloc_fallback(region, nbytes, page_type);
}
lispobj copy_potential_large_object(lispobj object, sword_t nwords,
                                   struct alloc_region*, int page_type);

#define compacting_p() (from_space>=0)

#define page_single_obj_p(page) ((page_table[page].type & SINGLE_OBJECT_FLAG)!=0)

extern unsigned char* gc_page_pins;
#ifdef RETURN_PC_WIDETAG
#include "code.h" // for fun_code_header
#endif
static inline bool pinned_p(lispobj obj, page_index_t page)
{
    extern struct hopscotch_table pinned_objects;
    // Single-object pages can be pinned, but the object doesn't go
    // in the hashtable. pinned_p can be queried on those pages,
    // but the answer is always 'No', because if pinned, the page would
    // already have had its generation changed to newspace.
    if (page_single_obj_p(page)) return 0;

#ifdef RETURN_PC_WIDETAG
    // Yet another complication from the despised LRA objects- with the
    // refinement of 8 pin bits per page, we either must set all possible bits
    // for a simple-fun, or map LRAs to the code base address.
    if (widetag_of(native_pointer(obj)) == RETURN_PC_WIDETAG) {
        // The hash-table stores tagged pointers.
        obj = make_lispobj(fun_code_header((struct simple_fun*)native_pointer(obj)),
                           OTHER_POINTER_LOWTAG);
        page = find_page_index((void*)obj);
    }
#endif

    unsigned char pins = gc_page_pins[page];
    if (!pins) return 0;
    unsigned addr_lowpart = obj & (GENCGC_PAGE_BYTES-1);
    // Divide the page into 8 parts, see whether that part is pinned.
    unsigned subpage = addr_lowpart / (GENCGC_PAGE_BYTES/8);
    return (pins & (1<<subpage)) && hopscotch_containsp(&pinned_objects, obj);
}

extern generation_index_t from_space, new_space;
// Return true only if 'obj' must be *physically* transported to survive gc.
// Return false if obj is in the immobile space regardless of its generation.
// Pretend pinned objects are not in oldspace so that they don't get moved.
static bool __attribute__((unused))
from_space_p(lispobj obj)
{
    gc_dcheck(compacting_p());
    page_index_t page_index = find_page_index((void*)obj);
    // NOTE: It is legal to access page_table at index -1,
    // and the 'gen' of page -1 is an otherwise unused value.
    return page_table[page_index].gen == from_space && !pinned_p(obj, page_index);
}

static bool __attribute__((unused)) new_space_p(lispobj obj)
{
    gc_dcheck(compacting_p());
    page_index_t page_index = find_page_index((void*)obj);
    // NOTE: It is legal to access page_table at index -1,
    // and the 'gen' of page -1 is an otherwise unused value.
    return page_table[page_index].gen == new_space;
}

#define CHECK_COPY_PRECONDITIONS(object, nwords) \
    gc_dcheck(is_lisp_pointer(object)); \
    gc_dcheck(from_space_p(object)); \
    gc_dcheck((nwords & 0x01) == 0)

#define CHECK_COPY_POSTCONDITIONS(copy, lowtag) \
    gc_dcheck(lowtag_of(copy) == lowtag); \
    gc_dcheck(!from_space_p(copy));

#define GC_LOGGING 0

/* For debugging purposes, you can make this macro as complicated as you like,
 * such as checking various other aspects of the object in 'old' */
#if GC_LOGGING
#define NOTE_TRANSPORTING(old, new, nwords) really_note_transporting(old,new,nwords)
void really_note_transporting(lispobj old,void*new,sword_t nwords);
#elif defined COLLECT_GC_STATS && COLLECT_GC_STATS
#define NOTE_TRANSPORTING(old, new, nwords) gc_copied_nwords += nwords
#else
#define NOTE_TRANSPORTING(old, new, nwords) /* do nothing */
#endif

// In-situ live objects are those which get logically "moved" from oldspace to newspace
// by frobbing the generation byte in the page table, not copying.
extern uword_t gc_copied_nwords, gc_in_situ_live_nwords;
static inline lispobj
gc_copy_object(lispobj object, size_t nwords, void* region, int page_type)
{
    CHECK_COPY_PRECONDITIONS(object, nwords);

    /* Allocate space. */
    lispobj *new = gc_general_alloc(region, nwords*N_WORD_BYTES, page_type);
    NOTE_TRANSPORTING(object, new,  nwords);

    /* Copy the object. */
    memcpy(new,native_pointer(object),nwords*N_WORD_BYTES);

    return make_lispobj(new, lowtag_of(object));
}

// Like above but copy potentially fewer words than are allocated.
// ('old_nwords' can be, but does not have to be, smaller than 'nwords')
static inline lispobj
gc_copy_object_resizing(lispobj object, long nwords, void* region, int page_type,
                        int old_nwords)
{
    CHECK_COPY_PRECONDITIONS(object, nwords);
    lispobj *new = gc_general_alloc(region, nwords*N_WORD_BYTES, page_type);
    NOTE_TRANSPORTING(object, new, old_nwords);
    memcpy(new, native_pointer(object), old_nwords*N_WORD_BYTES);
    return make_lispobj(new, lowtag_of(object));
}

extern sword_t (*const scavtab[256])(lispobj *where, lispobj object);
extern struct cons *weak_vectors; /* in gc-common.c */
extern struct hash_table *weak_hash_tables; /* in gc-common.c */
extern void acquire_gc_page_table_lock(void), release_gc_page_table_lock(void);

void gc_mark_range(lispobj*start, long count);
void gc_mark_obj(lispobj);
void gc_dispose_private_pages();
void add_to_weak_vector_list(lispobj* vector, lispobj header);

extern void heap_scavenge(lispobj *start, lispobj *limit);
extern sword_t scavenge(lispobj *start, sword_t n_words);
extern void scav_binding_stack(lispobj*, lispobj*, void(*)(lispobj));
extern void scan_binding_stack(void);
extern void scan_finalizers();
extern void cull_weak_hash_tables(int (*[4])(lispobj,lispobj));
extern void smash_weak_pointers(void);
extern bool scan_weak_hashtable(struct hash_table *hash_table,
                                   int (*)(lispobj,lispobj),
                                   void (*)(lispobj*));
extern int (*weak_ht_alivep_funs[4])(lispobj,lispobj);
extern void gc_scav_pair(lispobj where[2]);
extern void gc_common_init();
extern bool test_weak_triggers(bool (*)(lispobj), void (*)(lispobj));

lispobj  copy_unboxed_object(lispobj object, sword_t nwords);

lispobj *search_read_only_space(void *pointer);
lispobj *search_static_space(void *pointer);
lispobj *search_dynamic_space(void *pointer);

extern int properly_tagged_p_internal(lispobj pointer, lispobj *start_addr);
static inline int properly_tagged_descriptor_p(void *pointer, lispobj *start_addr) {
  return is_lisp_pointer((lispobj)pointer) &&
    properly_tagged_p_internal((lispobj)pointer, start_addr);
}

static inline void add_to_weak_pointer_chain(struct weak_pointer *wp) {
    // Better already be fixed in position or we're in trouble
    gc_dcheck(!compacting_p() || !from_space_p(make_lispobj(wp,OTHER_POINTER_LOWTAG)));
    /* Link 'wp' into weak_pointer_chain using its 'next' field.
     * We ensure that 'next' is always NULL when the weak pointer isn't
     * in the chain, and not NULL otherwise. The end of the chain
     * is denoted by WEAK_POINTER_CHAIN_END which is distinct from NULL.
     * The test of whether the weak pointer has been placed in the chain
     * is performed in 'scav_weak_pointer' */
    set_weak_pointer_next(wp, weak_pointer_chain);
    weak_pointer_chain = wp;
}

// "assign" as the operation name is a little clearer than "set"
// which tends to be synonymous with setting a bit to 1.
#define assign_page_card_marks(page, val) \
  memset(gc_card_mark+page_to_card_index(page), val, CARDS_PER_PAGE)

#ifdef LISP_FEATURE_SOFT_CARD_MARKS

#define NON_FAULTING_STORE(operation, addr) { operation; }
// The low bit of 0 implies "marked". So CARD_MARKED and STICKY_MARK
// are both considered marked. All bits of UNMARKED are 1s, so that
// one word full of mark bytes reads as -1. See the autogenerated
// functions in "genesis/cardmarks.h" for the use-case.
#define CARD_MARKED 0
#define STICKY_MARK 2
#define CARD_UNMARKED 0xff
#define MARK_BYTE_MASK 0xff

#else

// With physical page protection, bit index 0 is the MARKED bit (inverted WP bit)
// and bit index 1 is the WP_CLEARED (write_protect_cleared) bit.
// The fault handler always sets both bits.
// The only other bit pair value would be illegal.
#define CARD_UNMARKED         0 /* write-protected = 1 */
#define CARD_MARKED           1 /* write-protected = 0 */
#define WP_CLEARED_AND_MARKED 3 /* write-protected = 0, wp-cleared = 1 */
// CODE-HEADER-SET can store the low byte from NULL-TN into the mark array.
// This sets the two low bits on, but also spuriously sets other bits,
// which we can ignore when reading the byte.
#define MARK_BYTE_MASK 3

#define PAGE_WRITEPROTECTED_P(n) (~gc_card_mark[page_to_card_index(n)] & CARD_MARKED)
#define SET_PAGE_PROTECTED(n,val) gc_card_mark[page_to_card_index(n)] = \
      (val ? CARD_UNMARKED : CARD_MARKED)

#define cardseq_any_marked(card_index) (gc_card_mark[card_index] & CARD_MARKED)
#define cardseq_all_marked_nonsticky(card_index) cardseq_any_marked(card_index)
#define page_cards_all_marked_nonsticky(page_index) \
  cardseq_all_marked_nonsticky(page_to_card_index(page_index))

/* NON_FAULTING_STORE is only for fixnums and GC metadata where we need
 * the ability to write-through the store barrier.
 * The uses are limited to updating the weak vector chain, weak hash-table
 * chain, and vector rehash flags. Those don't affect a page's marked state */
#define PAGE_BASE(addr) ((char*)ALIGN_DOWN((uword_t)(addr),GENCGC_PAGE_BYTES))
#define NON_FAULTING_STORE(operation, addr) { \
  page_index_t page_index = find_page_index(addr); \
  if (page_index < 0 || !PAGE_WRITEPROTECTED_P(page_index)) { operation; } \
  else { os_protect(PAGE_BASE(addr), GENCGC_PAGE_BYTES, OS_VM_PROT_ALL); \
         operation; \
         os_protect(PAGE_BASE(addr), GENCGC_PAGE_BYTES, OS_VM_PROT_READ); } }

/* This is used by the fault handler, and potentially during GC
 * if we need to remember that a pointer store occurred.
 * The fault handler should supply WP_CLEARED_AND_MARKED as the mark,
 * but the collector should use CARD_MARKED */
static inline void unprotect_page(void* addr, unsigned char mark)
{
    // No atomic op needed for a 1-byte store.
    gc_card_mark[addr_to_card_index(addr)] = mark;
    os_protect(PAGE_BASE(addr), GENCGC_PAGE_BYTES, OS_VM_PROT_ALL);
}
#endif

// Helpers to avoid invoking the memory fault signal handler.
// For clarity, distinguish between words which *actually* need to frob
// physical (MMU-based) protection versus those which don't,
// but are forced to call mprotect() because it's the only choice.
// Unlike with NON_FAULTING_STORE, in this case we actually do want to record that
// the ensuing store toggles the WP bit without invoking the fault handler.
static inline void notice_pointer_store(__attribute__((unused)) void* base_addr,
                                        __attribute__((unused)) void* slot_addr) {
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    int card = addr_to_card_index(base_addr);
    // STICKY is stronger than MARKED. Only change if UNMARKED.
    if (gc_card_mark[card] == CARD_UNMARKED) gc_card_mark[card] = CARD_MARKED;
#else
    page_index_t index = find_page_index(slot_addr);
    gc_assert(index >= 0);
    if (PAGE_WRITEPROTECTED_P(index)) unprotect_page(slot_addr, CARD_MARKED);
#endif
}
static inline void vector_notice_pointer_store(void* addr) {
    notice_pointer_store(addr, addr);
}
static inline void ensure_non_ptr_word_writable(__attribute__((unused)) void* addr)
{
    // there's nothing to "ensure" if using software card marks
#ifndef LISP_FEATURE_SOFT_CARD_MARKS
    // #-soft-card-marks ignores the first argument of notice_pointer_store()
    notice_pointer_store(0, addr);
#endif
}

// #+soft-card-mark: this expresion is true of both CARD_MARKED and STICKY_MARK
// #-soft-card-mark: this expresion is true of both CARD_MARKED and WP_CLEARED_AND_MARKED
#define card_dirtyp(index) (gc_card_mark[index] & MARK_BYTE_MASK) != CARD_UNMARKED

/* Check whether 'pointee' was forwarded. If it has been, update the contents
 * of 'cell' to point to it. Otherwise, set 'cell' to 'broken'.
 * Note that this macro has no braces around the body because one of the uses
 * of it needs to stick on another 'else' or two */
#define TEST_WEAK_CELL(cell, pointee, broken) \
    lispobj *native = native_pointer(pointee); \
    if (from_space_p(pointee)) \
        cell = forwarding_pointer_p(native) ? forwarding_pointer_value(native) : broken; \
    else if (immobile_space_p(pointee)) { \
        if (immobile_obj_gen_bits(base_pointer(pointee)) == from_space) cell = broken; \
    }

#ifdef MAX_CONSES_PER_PAGE
static const int CONS_PAGE_USABLE_BYTES = MAX_CONSES_PER_PAGE*CONS_SIZE*N_WORD_BYTES;
#endif

/* Return true if 'addr' has a lowtag and widetag that correspond,
 * given that the words at 'addr' are within range for an allocated page.
 * 'addr' could be a pointer to random data, and this check is merely
 * a heuristic. False positives are possible. */
static inline bool plausible_tag_p(lispobj addr)
{
    if (listp(addr))
        return is_cons_half(CONS(addr)->car)
            && is_cons_half(CONS(addr)->cdr)
            // -1 can be left by the */signed=>integer vop
            // and is also useful as filler on cons pages.
            && CONS(addr)->car != (uword_t)-1;
    unsigned char widetag = widetag_of(native_pointer(addr));
    return other_immediate_lowtag_p(widetag)
        && lowtag_of(addr) == LOWTAG_FOR_WIDETAG(widetag);
}

extern char * gc_logfile;
extern void log_generation_stats(char *logfile, char *header);
extern void print_generation_stats(void);
extern double generation_average_age(generation_index_t);
static inline os_vm_size_t npage_bytes(page_index_t npages)
{
    gc_assert(npages>=0);
    return ((os_vm_size_t)npages)*GENCGC_PAGE_BYTES;
}
extern os_vm_size_t auto_gc_trigger;

typedef unsigned int page_bytes_t;
#define page_words_used(index) page_table[index].words_used_
#define page_bytes_used(index) ((page_bytes_t)page_table[index].words_used_<<WORD_SHIFT)
#if defined LISP_FEATURE_RISCV && defined LISP_FEATURE_LINUX // KLUDGE
#define page_need_to_zero(index) (mmap_does_not_zero || page_table[index].need_zerofill)
#else
#define page_need_to_zero(index) page_table[index].need_zerofill
#endif
#define set_page_bytes_used(index,val) page_table[index].words_used_ = ((val)>>WORD_SHIFT)
#define set_page_need_to_zero(index,val) page_table[index].need_zerofill = val

#if !CONDENSED_PAGE_TABLE

// 32-bit doesn't need magic to reduce the size of scan_start_offset.
#define set_page_scan_start_offset(index,val) \
  page_table[index].scan_start_offset_ = val
#define page_scan_start_offset(index) page_table[index].scan_start_offset_

#else

/// A "condensed" offset reduces page table size, which improves scan locality.
/// As stored, the offset is scaled down either by card size or double-lispwords.
/// If the offset is the maximum, then we must check if the page pointed to by
/// that offset is actually the start of a region, and retry if not.
/// For debugging the iterative algorithm it helps to use a max value
/// that is less than UINT_MAX to get a pass/fail more quickly.

//#define SCAN_START_OFS_MAX 0x3fff
#define SCAN_START_OFS_MAX UINT_MAX

static void __attribute__((unused))
set_page_scan_start_offset(page_index_t index, os_vm_size_t offset)
{
    // If the offset is nonzero and page-aligned
    unsigned int lsb = offset !=0 && IS_ALIGNED(offset, GENCGC_PAGE_BYTES);
    os_vm_size_t scaled = (offset >> (lsb ? GENCGC_PAGE_SHIFT-1 : WORD_SHIFT)) | lsb;
    if (scaled > SCAN_START_OFS_MAX) {
        // Assert that if offset exceed the max representable value,
        // then it is a page-aligned offset, not a cons-aligned offset.
        gc_assert(lsb == 1);
        // Clip it to the maximum representable value.
        // The accessor will have to iterate to find the true start of region.
        scaled = SCAN_START_OFS_MAX;
    }
    page_table[index].scan_start_offset_ = scaled;
}

static os_vm_size_t scan_start_offset_iterated(page_index_t index)
{
    // The low bit of the MAX is the 'scale' bit. The max pages we can look
    // backwards is therefore the max shifted right by 1 bit.
    page_index_t tot_offset_in_pages = 0;
    unsigned int offset;
    do {
        page_index_t lookback_page = index - tot_offset_in_pages;
        offset = page_table[lookback_page].scan_start_offset_;
        tot_offset_in_pages += offset >> 1;
    } while (offset == SCAN_START_OFS_MAX);
    return (os_vm_size_t)tot_offset_in_pages << GENCGC_PAGE_SHIFT;
}

static os_vm_size_t  __attribute__((unused)) page_scan_start_offset(page_index_t index)
{
    return page_table[index].scan_start_offset_ != SCAN_START_OFS_MAX
        ? (os_vm_size_t)(page_table[index].scan_start_offset_ & ~1)
          << ((page_table[index].scan_start_offset_ & 1) ?
              (GENCGC_PAGE_SHIFT-1) : WORD_SHIFT)
        : scan_start_offset_iterated(index);
}

#endif

#define is_code(type) ((type & PAGE_TYPE_MASK) == PAGE_TYPE_CODE)

// If *all* pages use soft card marks, then protection_mode() is not a thing.
// Otherwise, only pages of code use soft card marks; return an enum indicating
// whether the page protection for the specified page is applied in harware.
#ifndef LISP_FEATURE_SOFT_CARD_MARKS
enum prot_mode { PHYSICAL, LOGICAL };
static inline enum prot_mode protection_mode(page_index_t page) {
    // code pages can be marked as logically read-only without OS protection,
    // and everything else uses hardware-based protection where applicable.
    return is_code(page_table[page].type) ? LOGICAL : PHYSICAL;
}
#endif

static inline bool page_free_p(page_index_t page) {
    return (page_table[page].type == FREE_PAGE_FLAG);
}

/* Check that X is a higher address than Y and return offset from Y to
 * X in bytes. */
static inline os_vm_size_t
addr_diff(void *x, void *y)
{
    gc_assert(x >= y);
    return (uintptr_t)x - (uintptr_t)y;
}

extern os_vm_size_t bytes_allocated;

extern void reset_page_flags(page_index_t page);

/* True if the page starts a contiguous block. */
static inline bool
page_starts_contiguous_block_p(page_index_t page_index)
{
    // Don't use the preprocessor macro: 0 means 0.
    return page_table[page_index].scan_start_offset_ == 0;
}

/* True if the page is the last page in a contiguous block. */
static inline bool
page_ends_contiguous_block_p(page_index_t page_index,
                             generation_index_t __attribute__((unused)) gen)
{
    /* Re. this next test: git rev c769dd53 said that there was a bug when we don't
     * test page_bytes_used, but I fail to see how 'page_starts_contiguous_block_p'
     * on the next page is not a STRONGER condition, i.e. it should imply that
     * 'page_index' ends a block without regard for the number of bytes used.
     * Apparently at some point I understood this and now I don't again.
     * That's what comments are for, damnit.
     * Anyway, I *think* the issue was, at some point, as follows:
     * |   page             |     page   |
     *        pinned-obj
     *     <------------------- scan-start
     * where the first of the two pages had a small object pinned. This used to
     * adjust the bytes used to account _only_ for the pins.  That was wrong -
     * the page has to be counted as if it is completely full.
     * So _maybe_ both these conditions do not need to be present now ?
     */
    // There is *always* a next page in the page table.
    bool answer = page_words_used(page_index) < GENCGC_PAGE_WORDS
                  || page_starts_contiguous_block_p(page_index+1);
#ifdef DEBUG
    bool safe_answer =
           (/* page doesn't fill block */
            (page_words_used(page_index) < GENCGC_PAGE_WORDS)
            /* page is last allocated page */
            || ((page_index + 1) >= next_free_page)
            /* next page contains no data */
            || !page_words_used(page_index + 1)
            /* next page is in different generation */
            || (page_table[page_index + 1].gen != gen)
            /* next page starts its own contiguous block */
            || (page_starts_contiguous_block_p(page_index + 1)));
    gc_assert(answer == safe_answer);
#endif
    return answer;
}

static inline page_index_t contiguous_block_final_page(page_index_t first) {
    page_index_t last = first;
    while (!page_ends_contiguous_block_p(last, page_table[first].gen)) ++last;
    return last;
}

int gencgc_handle_wp_violation(void*, void*);


// The flags control the behavior of sync_close_regions()
#define LOCK_PAGE_TABLE 1
#define LOCK_CODE_ALLOCATOR 2
#define CONSUME_REMAINDER 4

void gc_close_region(struct alloc_region *alloc_region, int page_type);
static inline void ensure_region_closed(struct alloc_region *alloc_region,
                                        int page_type)
{
    if (alloc_region->start_addr)
        gc_close_region(alloc_region, page_type);
}

#ifdef LISP_FEATURE_IMMOBILE_SPACE
struct fixedobj_page { // 8 bytes per page
    unsigned int free_index; // index is in bytes. 4 bytes
    union immobile_page_attr {
      int packed;
      struct {
        unsigned char flags;
        unsigned char obj_align; // object spacing expressed in lisp words
        unsigned char unused1;
        /* Which generations have data on this page */
        unsigned char gens_; // a bitmap
      } parts;
    } attr;
};
extern struct fixedobj_page *fixedobj_pages;
#define fixedobj_page_obj_align(i) (fixedobj_pages[i].attr.parts.obj_align<<WORD_SHIFT)
#endif

extern uword_t
walk_generation(uword_t (*proc)(lispobj*,lispobj*,uword_t),
                generation_index_t generation, uword_t extra);

generation_index_t gc_gen_of(lispobj obj, int defaultval);

#include "forwarding-ptr.inc"

/* Return true if OBJ has already survived the current GC.
 * This is actually a lot of code, but the C compiler may choose to
 * inline because it's static. "unused" since not all includers need it */
__attribute__((unused)) static bool taggedptr_alivep_impl(lispobj obj)
{
    /* Check for a pointer to dynamic space before considering immobile space.
       Based on the relative size of the spaces, this should be a win because
       if the object is in the dynamic space and not the 'from' generation
       we don't want to test immobile_space_p() at all.
       Additionally, pinned_p() is both more expensive and less likely than
       forwarding_pointer_p(), so we want to reverse those conditions, which
       would not be possible with pinned_p() buried inside from_space_p(). */
    page_index_t page_index = find_page_index((void*)obj);
    if (page_index >= 0)
        return page_table[page_index].gen != from_space ||
               forwarding_pointer_p(native_pointer(obj)) ||
               pinned_p(obj, page_index);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (immobile_space_p(obj))
        return immobile_obj_gen_bits(base_pointer(obj)) != from_space;
#endif
    return 1;
}

static inline lispobj* next_object(lispobj *previous, uword_t size, lispobj *end) {
    return (previous + size >= end) ? NULL : previous + size;
}

static inline lispobj* page_limit(page_index_t page) {
    return (lispobj*)page_address(page) + page_words_used(page);
}
