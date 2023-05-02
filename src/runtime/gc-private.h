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

/* Include this header only in files that are _really_ part of GC
   or intimately tied to GC like 'raceroot'. */

#ifndef _GC_PRIVATE_H_
#define _GC_PRIVATE_H_

#include "genesis/instance.h"
#include "genesis/funcallable-instance.h"
#include "genesis/weak-pointer.h"
#include "immobile-space.h"
#include "code.h"

#ifdef LISP_FEATURE_GENCGC
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
#else
void *gc_general_alloc(void*,sword_t,int);
lispobj copy_potential_large_object(lispobj object, sword_t nwords,
                                   void*, int page_type);
#endif

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

// These next two are prototyped for both GCs
// but only gencgc will ever call them.
void gc_mark_range(lispobj*start, long count);
void gc_mark_obj(lispobj);
void gc_dispose_private_pages();
void add_to_weak_vector_list(lispobj* vector, lispobj header);

extern void heap_scavenge(lispobj *start, lispobj *limit);
extern sword_t scavenge(lispobj *start, sword_t n_words);
extern void scavenge_interrupt_contexts(struct thread *thread);
extern void scav_binding_stack(lispobj*, lispobj*, void(*)(lispobj));
extern void scan_binding_stack(void);
extern void scan_finalizers();
extern void cull_weak_hash_tables(int (*[4])(lispobj,lispobj));
extern void smash_weak_pointers(void);
extern boolean scan_weak_hashtable(struct hash_table *hash_table,
                                   int (*)(lispobj,lispobj),
                                   void (*)(lispobj*));
extern int (*weak_ht_alivep_funs[4])(lispobj,lispobj);
extern void gc_scav_pair(lispobj where[2]);
extern void gc_common_init();
extern boolean test_weak_triggers(int (*)(lispobj), void (*)(lispobj));

lispobj  copy_unboxed_object(lispobj object, sword_t nwords);

lispobj *search_read_only_space(void *pointer);
lispobj *search_static_space(void *pointer);
lispobj *search_dynamic_space(void *pointer);

extern int properly_tagged_p_internal(lispobj pointer, lispobj *start_addr);
static inline int properly_tagged_descriptor_p(void *pointer, lispobj *start_addr) {
  return is_lisp_pointer((lispobj)pointer) &&
    properly_tagged_p_internal((lispobj)pointer, start_addr);
}

extern void scavenge_control_stack(struct thread *th);
extern void scrub_control_stack(void);
extern void scrub_thread_control_stack(struct thread *);

// for code ojects, this bit signifies that this object is in the remembered set.
// KLUDGE: this constant needs to be autogenerated. It is currently hardcoded into
// the CODE-HEADER-SET assembly routine for x86 and x86-64.
#define OBJ_WRITTEN_FLAG 0x40
#ifdef LISP_FEATURE_LITTLE_ENDIAN
#define CLEAR_WRITTEN_FLAG(obj) ((unsigned char*)obj)[3] &= ~OBJ_WRITTEN_FLAG
#define SET_WRITTEN_FLAG(obj)   ((unsigned char*)obj)[3] |= OBJ_WRITTEN_FLAG
#else
#define CLEAR_WRITTEN_FLAG(obj) *obj &= ~(OBJ_WRITTEN_FLAG<<24)
#define SET_WRITTEN_FLAG(obj)   *obj |=  (OBJ_WRITTEN_FLAG<<24)
#endif
static inline int header_rememberedp(lispobj header) {
  return (header & (OBJ_WRITTEN_FLAG << 24)) != 0;
}

#ifdef LISP_FEATURE_IMMOBILE_SPACE

extern void enliven_immobile_obj(lispobj*,int);

#define IMMOBILE_OBJ_VISITED_FLAG    0x10

// Immobile object header word:
//                 generation byte --|    |-- widetag
//                                   v    v
//                       0xzzzzzzzz GGzzzzww
//         arbitrary data  --------   ---- length in words
//
// An an exception to the above, FDEFNs omit the length:
//                       0xzzzzzzzz zzzzGGww
//         arbitrary data  -------- ----
// so that there are 6 consecutive bytes of arbitrary data.
// The length of an FDEFN is implicitly fixed at 4 words.

// There is a hard constraint on NUM_GENERATIONS, which is currently 8.
// (0..5=normal, 6=pseudostatic, 7=scratch)
// Shifting a 1 bit left by the contents of the generation byte
// must not overflow a register.

// Mask off the VISITED flag to get the generation number
#define immobile_obj_generation(x) (immobile_obj_gen_bits(x) & 0xf)

#ifdef LISP_FEATURE_LITTLE_ENDIAN
// Return the generation bits which means the generation number
// in the 4 low bits (there's 1 excess bit) and the VISITED flag.
static inline int immobile_obj_gen_bits(lispobj* obj) // native pointer
{
    // When debugging, assert that we're called only on a headered object
    // whose header contains a generation byte.
    gc_dcheck(!embedded_obj_p(widetag_of(obj)));
    char gen;
    switch (widetag_of(obj)) {
    default:
        gen = ((generation_index_t*)obj)[3]; break;
    case FDEFN_WIDETAG:
        gen = ((generation_index_t*)obj)[1]; break;
    }
    return gen & 0x1F;
}
// Turn a grey node black.
static inline void set_visited(lispobj* obj)
{
    gc_dcheck(widetag_of(obj) != SIMPLE_FUN_WIDETAG);
    gc_dcheck(immobile_obj_gen_bits(obj) == new_space);
    int byte = widetag_of(obj) == FDEFN_WIDETAG ? 1 : 3;
    ((generation_index_t*)obj)[byte] |= IMMOBILE_OBJ_VISITED_FLAG;
}
static inline void assign_generation(lispobj* obj, generation_index_t gen)
{
    gc_dcheck(widetag_of(obj) != SIMPLE_FUN_WIDETAG);
    int byte = widetag_of(obj) == FDEFN_WIDETAG ? 1 : 3;
    generation_index_t* ptr = (generation_index_t*)obj + byte;
    // Clear the VISITED flag, assign a new generation, preserving the three
    // high bits which include the OBJ_WRITTEN flag as well as two
    // opaque flag bits for use by Lisp.
    *ptr = (*ptr & 0xE0) | gen;
}
#else
#error "Need to define immobile_obj_gen_bits() for big-endian"
#endif /* little-endian */

#endif /* immobile space */

#ifdef LISP_FEATURE_64_BIT
#define WEAK_POINTER_CHAIN_END (void*)(intptr_t)1
#define in_weak_pointer_list(wp) ((wp->header>>16)!=0)
static inline struct weak_pointer *get_weak_pointer_next(struct weak_pointer *wp) {
    // 6 bytes to encode 'next' is way more than enough
    uword_t offset = wp->header >> 16;
    if (offset <= 1) return (void*)offset;
    return (void*)((lispobj*)DYNAMIC_SPACE_START + offset);
}
static inline void reset_weak_pointer_next(struct weak_pointer *wp) {
    wp->header = wp->header & 0xffff;
}
static inline lispobj encode_weakptr_next(void* x) {
    if ((uword_t)x <= 1) return (uword_t)x;
    sword_t wordindex = (lispobj*)x - (lispobj*)DYNAMIC_SPACE_START;
    gc_assert(wordindex != 0); // wp can't be the first object in dynamic space
    return wordindex;
}
#else
#define WEAK_POINTER_CHAIN_END (void*)(intptr_t)-1
#define in_weak_pointer_list(wp) (wp->next)
#define get_weak_pointer_next(wp) wp->next
#define reset_weak_pointer_next(wp) wp->next = 0
#endif

static inline void add_to_weak_pointer_chain(struct weak_pointer *wp) {
    // Better already be fixed in position or we're in trouble
    gc_dcheck(!compacting_p() || !from_space_p(make_lispobj(wp,OTHER_POINTER_LOWTAG)));
    /* Link 'wp' into weak_pointer_chain using its 'next' field.
     * We ensure that 'next' is always NULL when the weak pointer isn't
     * in the chain, and not NULL otherwise. The end of the chain
     * is denoted by WEAK_POINTER_CHAIN_END which is distinct from NULL.
     * The test of whether the weak pointer has been placed in the chain
     * is performed in 'scav_weak_pointer' */
#ifdef LISP_FEATURE_64_BIT
    wp->header = (encode_weakptr_next(weak_pointer_chain)<<16) | (wp->header & 0xffff);
#else
    wp->next = weak_pointer_chain;
#endif
    weak_pointer_chain = wp;
}

#include "genesis/layout.h"
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
    // The 2 bits for stable address-based hashing can't ever bet set.
    const int baseline_payload_words = (sizeof (struct layout) / N_WORD_BYTES) - 1;
    int payload_words = ((unsigned int)layout->header >> INSTANCE_LENGTH_SHIFT) & 0x3FFF;
    bitmap.bits = (sword_t*)((char*)layout + sizeof (struct layout)) + extra_id_words;
    bitmap.nwords = payload_words - baseline_payload_words - extra_id_words;
    return bitmap;
}

/* Return true if the INDEXth bit is set in BITMAP.
 * Index 0 corresponds to the word just after the instance header.
 * So index 0 may be the layout pointer if #-compact-instance-header,
 * or a user data slot if #+compact-instance-header
 */
static inline boolean bitmap_logbitp(unsigned int index, struct bitmap bitmap)
{
    unsigned int word_index = index / N_WORD_BITS;
    unsigned int bit_index  = index % N_WORD_BITS;
    if (word_index >= bitmap.nwords) return bitmap.bits[bitmap.nwords-1] < 0;
    return (bitmap.bits[word_index] >> bit_index) & 1;
}

/* Keep in sync with 'target-hash-table.lisp' */
#define hashtable_kind(ht) ((ht->flags >> (4+N_FIXNUM_TAG_BITS)) & 3)
#define hashtable_weakp(ht) (ht->flags & (8<<N_FIXNUM_TAG_BITS))
#define hashtable_weakness(ht) (ht->flags >> (6+N_FIXNUM_TAG_BITS))

extern unsigned char* gc_card_mark;

#ifdef LISP_FEATURE_DARWIN_JIT
#define OS_VM_PROT_JIT_READ OS_VM_PROT_READ
#define OS_VM_PROT_JIT_ALL OS_VM_PROT_READ | OS_VM_PROT_WRITE
#else
#define OS_VM_PROT_JIT_READ OS_VM_PROT_READ | OS_VM_PROT_EXECUTE
#define OS_VM_PROT_JIT_ALL OS_VM_PROT_ALL
#endif

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
  else { os_protect(PAGE_BASE(addr), GENCGC_PAGE_BYTES, OS_VM_PROT_JIT_ALL); \
         operation; \
         os_protect(PAGE_BASE(addr), GENCGC_PAGE_BYTES, OS_VM_PROT_JIT_READ); } }

/* This is used by the fault handler, and potentially during GC
 * if we need to remember that a pointer store occurred.
 * The fault handler should supply WP_CLEARED_AND_MARKED as the mark,
 * but the collector should use CARD_MARKED */
static inline void unprotect_page(void* addr, unsigned char mark)
{
    // No atomic op needed for a 1-byte store.
    gc_card_mark[addr_to_card_index(addr)] = mark;
    os_protect(PAGE_BASE(addr), GENCGC_PAGE_BYTES, OS_VM_PROT_JIT_ALL);
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

#define KV_PAIRS_HIGH_WATER_MARK(kvv) fixnum_value(kvv[0])
#define KV_PAIRS_REHASH(kvv) kvv[1]

/* This is NOT the same value that lisp's %INSTANCE-LENGTH returns.
 * Lisp always uses the logical length (as originally allocated),
 * except when heap-walking which requires exact physical sizes */
static inline int instance_length(lispobj header)
{
    // * Byte 3 of an instance header word holds the immobile gen# and visited bit,
    //   so those have to be masked off.
    // * fullcgc uses bit index 31 as a mark bit, so that has to
    //   be cleared. Lisp does not have to clear bit 31 because fullcgc does not
    //   operate concurrently.
    // * If the object is in hashed-and-moved state and the original instance payload
    //   length was odd (total object length was even), then add 1.
    //   This can be detected by ANDing some bits, bit 10 being the least-significant
    //   bit of the original size, and bit 9 being the 'hashed+moved' bit.
    // * 64-bit machines do not need 'long' right-shifts, so truncate to int.

    int extra = ((unsigned int)header >> 10) & ((unsigned int)header >> 9) & 1;
    return (((unsigned int)header >> INSTANCE_LENGTH_SHIFT) & 0x3FFF) + extra;
}

// This is index of the bit that differentiates FUNCALLABLE_INSTANCE_WIDETAG
// from INSTANCE_WIDETAG.
#define FUNINSTANCE_SELECTOR_BIT_NUMBER 2
static inline boolean instanceoid_widetag_p(unsigned char widetag) {
    return (widetag | (1<<FUNINSTANCE_SELECTOR_BIT_NUMBER)) == FUNCALLABLE_INSTANCE_WIDETAG;
}
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
// Keep in sync with hardwired IDs in CHOOSE-LAYOUT-ID
// in src/compiler/generic/layout-ids.lisp
#define WRAPPER_LAYOUT_ID 2
#define LAYOUT_LAYOUT_ID 3
#define LFLIST_NODE_LAYOUT_ID 4
#define BROTHERTREE_UNARY_NODE_LAYOUT_ID 5

/// Return true if 'thing' is a layout.
/// This predicate is careful, as is it used to verify heap invariants.
static inline boolean layoutp(lispobj thing)
{
    lispobj layout;
    if (lowtag_of(thing) != INSTANCE_POINTER_LOWTAG) return 0;
    if ((layout = instance_layout(INSTANCE(thing))) == 0) return 0;
    return layout_depth2_id(LAYOUT(layout)) == LAYOUT_LAYOUT_ID;
}
#ifdef LISP_FEATURE_METASPACE
static inline boolean wrapperp(lispobj thing)
{
    lispobj layout;
    if (lowtag_of(thing) != INSTANCE_POINTER_LOWTAG) return 0;
    if ((layout = instance_layout(INSTANCE(thing))) == 0) return 0;
    return layout_depth2_id(LAYOUT(layout)) == WRAPPER_LAYOUT_ID;
}
static inline int wrapper_id(lispobj wrapper)
{
    struct layout* layout = LAYOUT(WRAPPER(wrapper)->friend);
    return layout_depth2_id(layout);
}
#endif
/// Return true if 'thing' is the layout of any subtype of sb-lockless::list-node.
static inline boolean lockfree_list_node_layout_p(struct layout* layout) {
    return layout_depth2_id(layout) == LFLIST_NODE_LAYOUT_ID;
}

#ifdef LISP_FEATURE_METASPACE
#define METASPACE_START (READ_ONLY_SPACE_START+32768) /* KLUDGE */
// Keep in sync with the macro definitions in src/compiler/generic/early-vm.lisp
struct slab_header {
    short sizeclass;
    short capacity;
    short chunksize;
    short count;
    void* freelist;
    struct slab_header *next;
    struct slab_header *prev;
};
#endif

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

#include "genesis/cons.h"
/* Return true if 'addr' has a lowtag and widetag that correspond,
 * given that the words at 'addr' are within range for an allocated page.
 * 'addr' could be a pointer to random data, and this check is merely
 * a heuristic. False positives are possible. */
static inline boolean plausible_tag_p(lispobj addr)
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

#ifdef LISP_FEATURE_64_BIT
# define make_filler_header(n) (((uword_t)(n)<<32)|FILLER_WIDETAG)
# define filler_total_nwords(header) ((header)>>32)
#else
# define make_filler_header(n) (((n)<<N_WIDETAG_BITS)|FILLER_WIDETAG)
# define filler_total_nwords(header) ((header)>>N_WIDETAG_BITS)
#endif

#ifdef LISP_FEATURE_BIG_ENDIAN
# define assign_widetag(addr, byte) ((unsigned char*)addr)[N_WORD_BYTES-1] = byte
#else
# define assign_widetag(addr, byte) *(unsigned char*)addr = byte
#endif

#endif /* _GC_PRIVATE_H_ */
