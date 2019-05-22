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

#include "genesis/weak-pointer.h"
#include "immobile-space.h"

// Gencgc distinguishes between "quick" and "ordinary" requests.
// Even on cheneygc we need this flag, but it's actually just ignored.
#define ALLOC_QUICK 1

#define CUSTOM_GC_SCAVENGE_FLAG 0x800000

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-alloc-region.h"
void *
gc_alloc_with_region(struct alloc_region *my_region, sword_t nbytes,
                     int page_type_flag, int quick_p);
static inline void *
gc_general_alloc(sword_t nbytes, int page_type_flag, int quick_p)
{
    if (1 <= page_type_flag && page_type_flag <= 3)
        return gc_alloc_with_region(&gc_alloc_region[page_type_flag-1],
                                    nbytes, page_type_flag, quick_p);
    lose("bad page type flag: %d", page_type_flag);
}
#else
extern void *gc_general_alloc(sword_t nbytes,int page_type_flag,int quick_p);
#endif

#define CHECK_COPY_PRECONDITIONS(object, nwords) \
    gc_dcheck(is_lisp_pointer(object)); \
    gc_dcheck(from_space_p(object)); \
    gc_dcheck((nwords & 0x01) == 0)

#define CHECK_COPY_POSTCONDITIONS(copy, lowtag) \
    gc_dcheck(lowtag_of(copy) == lowtag); \
    gc_dcheck(!from_space_p(copy));

#define note_transported_object(old, new) /* do nothing */

static inline lispobj
gc_general_copy_object(lispobj object, long nwords, int page_type_flag)
{
    lispobj *new;

    CHECK_COPY_PRECONDITIONS(object, nwords);

    /* Allocate space. */
    new = gc_general_alloc(nwords*N_WORD_BYTES, page_type_flag, ALLOC_QUICK);

    /* Copy the object. */
    memcpy(new,native_pointer(object),nwords*N_WORD_BYTES);

    note_transported_object(object, new);

    return make_lispobj(new, lowtag_of(object));
}

extern sword_t (*scavtab[256])(lispobj *where, lispobj object);
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
extern void cull_weak_hash_tables(int (*[4])(lispobj,lispobj));
extern void smash_weak_pointers(void);
extern boolean scav_hash_table_entries(struct hash_table *hash_table,
                                       int (*)(lispobj,lispobj),
                                       void (*)(lispobj*));
extern int (*weak_ht_alivep_funs[4])(lispobj,lispobj);
extern void gc_scav_pair(lispobj where[2]);
extern void weakobj_init();
extern boolean test_weak_triggers(int (*)(lispobj), void (*)(lispobj));

lispobj  copy_unboxed_object(lispobj object, sword_t nwords);
lispobj  copy_object(lispobj object, sword_t nwords);
lispobj  copy_large_object(lispobj object, sword_t nwords, int page_type_flag);

lispobj *search_read_only_space(void *pointer);
lispobj *search_static_space(void *pointer);
lispobj *search_immobile_space(void *pointer);
lispobj *search_dynamic_space(void *pointer);

extern int properly_tagged_p_internal(lispobj pointer, lispobj *start_addr);
static inline int properly_tagged_descriptor_p(void *pointer, lispobj *start_addr) {
  return is_lisp_pointer((lispobj)pointer) &&
    properly_tagged_p_internal((lispobj)pointer, start_addr);
}

extern void scavenge_control_stack(struct thread *th);
extern void scrub_control_stack(void);
extern void scrub_thread_control_stack(struct thread *);

#ifndef LISP_FEATURE_IMMOBILE_SPACE

static inline boolean filler_obj_p(lispobj __attribute__((unused)) *obj) { return 0; }

#else

extern void enliven_immobile_obj(lispobj*,int);

#define IMMOBILE_OBJ_VISITED_FLAG    0x10
#define IMMOBILE_OBJ_GENERATION_MASK 0x0f // mask off the VISITED flag

// Note: this does not work on a SIMPLE-FUN
// because a simple-fun header does not contain a generation.
#define __immobile_obj_generation(x) (__immobile_obj_gen_bits(x) & IMMOBILE_OBJ_GENERATION_MASK)

#ifdef LISP_FEATURE_LITTLE_ENDIAN
static inline int immobile_obj_gen_bits(lispobj* pointer) // native pointer
{
  if (widetag_of(pointer) == SIMPLE_FUN_WIDETAG)
    pointer = fun_code_header(pointer);
  return ((generation_index_t*)pointer)[3] & 0x3F;
}
// Faster way when we know that the object can't be a simple-fun,
// such as when walking the immobile space.
static inline int __immobile_obj_gen_bits(lispobj* pointer) // native pointer
{
  return ((generation_index_t*)pointer)[3] & 0x3F;
}
#else
#error "Need to define immobile_obj_gen_bits() for big-endian"
#endif /* little-endian */

static inline boolean filler_obj_p(lispobj* obj) {
    return widetag_of(obj) == CODE_HEADER_WIDETAG && obj[1] == 0;
}

#endif /* immobile space */

#define WEAK_POINTER_CHAIN_END (void*)(intptr_t)-1
#define WEAK_POINTER_NWORDS ALIGN_UP(WEAK_POINTER_SIZE,2)

static inline boolean weak_pointer_breakable_p(struct weak_pointer *wp)
{
    lispobj pointee = wp->value;
    // A broken weak-pointer's value slot has unbound-marker
    // which does not satisfy is_lisp_pointer().
    return is_lisp_pointer(pointee) && (from_space_p(pointee)
#ifdef LISP_FEATURE_IMMOBILE_SPACE
         || (immobile_space_p(pointee) &&
             immobile_obj_gen_bits(native_pointer(pointee)) == from_space)
#endif
            );
}

static inline void add_to_weak_pointer_chain(struct weak_pointer *wp) {
    /* Link 'wp' into weak_pointer_chain using its 'next' field.
     * We ensure that 'next' is always NULL when the weak pointer isn't
     * in the chain, and not NULL otherwise. The end of the chain
     * is denoted by WEAK_POINTER_CHAIN_END which is distinct from NULL.
     * The test of whether the weak pointer has been placed in the chain
     * is performed in 'scav_weak_pointer' for gencgc.
     * In cheneygc, chaining is performed in 'trans_weak_pointer'
     * which works just as well, since an object is transported
     * at most once per GC cycle */
    wp->next = (struct weak_pointer *)LOW_WORD(weak_pointer_chain);
    weak_pointer_chain = wp;
}

/// Same as Lisp LOGBITP, except no negative bignums allowed.
static inline boolean layout_bitmap_logbitp(int index, lispobj bitmap)
{
    if (fixnump(bitmap))
      return (index < (N_WORD_BITS - N_FIXNUM_TAG_BITS))
          ? (bitmap >> (index+N_FIXNUM_TAG_BITS)) & 1
          : (sword_t)bitmap < 0;
    return positive_bignum_logbitp(index, (struct bignum*)native_pointer(bitmap));
}

/* Keep in sync with 'target-hash-table.lisp' */
#define hashtable_weakp(ht) (ht->flags & (1<<N_FIXNUM_TAG_BITS))
#define hashtable_weakness(ht) (ht->flags >> (3+N_FIXNUM_TAG_BITS))

#if defined(LISP_FEATURE_GENCGC)

/* Define a macro to avoid a detour through the write fault handler.
 *
 * It's usually more efficient to do these extra tests than to receive
 * a signal. And it leaves the page protected, which is a bonus.
 * The downside is that multiple operations on the same page ought to
 * be batched, so that there is at most one unprotect/reprotect per page
 * rather than per write operation per page.
 *
 * This also should fix -fsanitize=thread which makes handling of SIGSEGV
 * during GC difficult. Not impossible, but definitely broken.
 * It has to do with the way the sanitizer intercepts calls
 * to sigaction() - it mucks with your sa_mask :-(.
 *
 * This macro take an aribtrary expression as the 'operation' rather than
 * an address and value to assign, for two reasons:
 * 1. there may be more than one store operation that has to be
 *    within the scope of the lifted write barrier,
 *    so a single lvalue and rvalue is maybe inadequate.
 * 2. it might need to use a sync_fetch_and_<frob>() gcc intrinsic,
 *    so it's not necessarily just going to be an '=' operator
 *
 * KLUDGE: assume that faults do not occur in immobile space.
 * for the most part. (This is pretty obviously not true,
 * but seems only to be a problem in fullcgc)
 */

#define NON_FAULTING_STORE(operation, addr) { \
  page_index_t page_index = find_page_index(addr); \
  if (page_index < 0 || !page_table[page_index].write_protected) { operation; } \
  else { unprotect_page_index(page_index); \
         operation; \
         protect_page(page_address(page_index), page_index); }}

/* This is used bu the fault handler, and potentially during GC */
static inline void unprotect_page_index(page_index_t page_index)
{
    os_protect(page_address(page_index), GENCGC_CARD_BYTES, OS_VM_PROT_ALL);
    unsigned char *pflagbits = (unsigned char*)&page_table[page_index].gen - 1;
    __sync_fetch_and_or(pflagbits, WP_CLEARED_FLAG);
    __sync_fetch_and_and(pflagbits, ~WRITE_PROTECTED_FLAG);
}

static inline void protect_page(void* page_addr, page_index_t page_index)
{
    os_protect((void *)page_addr,
               GENCGC_CARD_BYTES,
               OS_VM_PROT_READ|OS_VM_PROT_EXECUTE);

    /* Note: we never touch the write_protected_cleared bit when protecting
     * a page. Consider two random threads that reach their SIGSEGV handlers
     * concurrently, each checking why it got a write fault. One thread wins
     * the race to remove the memory protection, and marks our shadow bit.
     * wp_cleared is set so that the other thread can conclude that the fault
     * was reasonable.
     * If GC unprotects and reprotects a page, it's probably OK to reset the
     * cleared bit 0 if it was 0 before. (Because the fault handler blocks
     * SIG_STOP_FOR_GC which is usually SIGUSR2, handling the wp fault is
     * atomic with respect to invocation of GC)
     * But nothing is really gained by resetting the cleared flag.
     * It is explicitly zeroed on pages marked as free though.
     */
    page_table[page_index].write_protected = 1;
}

#else

#define NON_FAULTING_STORE(operation, addr) operation

#endif

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

#if defined(LISP_FEATURE_X86_64) || defined(LISP_FEATURE_X86)
# define CODE_PAGES_USE_SOFT_PROTECTION 1
#else
# define CODE_PAGES_USE_SOFT_PROTECTION 0
#endif

#endif /* _GC_PRIVATE_H_ */
