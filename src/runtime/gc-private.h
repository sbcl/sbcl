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

// Gencgc distinguishes between "quick" and "ordinary" requests.
// Even on cheneygc we need this flag, but it's actually just ignored.
#define ALLOC_QUICK 1

#ifdef LISP_FEATURE_GENCGC
#include "gencgc-alloc-region.h"
void *
gc_alloc_with_region(sword_t nbytes,int page_type_flag, struct alloc_region *my_region,
                     int quick_p);
static inline void *
gc_general_alloc(sword_t nbytes, int page_type_flag, int quick_p)
{
    struct alloc_region *my_region;
#ifdef SEGREGATED_CODE
    if (1 <= page_type_flag && page_type_flag <= 3) {
        my_region = &gc_alloc_region[page_type_flag-1];
#else
    if (UNBOXED_PAGE_FLAG == page_type_flag) {
        my_region = &unboxed_region;
    } else if (BOXED_PAGE_FLAG & page_type_flag) {
        my_region = &boxed_region;
#endif
    } else {
        lose("bad page type flag: %d", page_type_flag);
    }
    return gc_alloc_with_region(nbytes, page_type_flag, my_region, quick_p);
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
extern struct weak_pointer *weak_pointers; /* in gc-common.c */
extern struct hash_table *weak_hash_tables; /* in gc-common.c */

// These next two are prototyped for both GCs
// but only gencgc will ever call them.
void gc_mark_range(lispobj*start, long count);
void gc_mark_obj(lispobj);
void gc_dispose_private_pages();

extern void heap_scavenge(lispobj *start, lispobj *limit);
extern sword_t scavenge(lispobj *start, sword_t n_words);
extern void scavenge_interrupt_contexts(struct thread *thread);
extern void scav_weak_hash_tables(int (*[5])(lispobj,lispobj),
                                  void (*)(lispobj*));
extern void scav_binding_stack(lispobj*, lispobj*, void(*)(lispobj));
extern void scan_binding_stack(void);
extern void cull_weak_hash_tables(int (*[5])(lispobj,lispobj));
extern void scan_weak_pointers(void);
extern void scav_hash_table_entries (struct hash_table *hash_table,
                                     int (*[5])(lispobj,lispobj),
                                     void (*)(lispobj*));
extern int (*weak_ht_alivep_funs[5])(lispobj,lispobj);
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

static inline int instruction_ptr_p(void *pointer, lispobj *start_addr)
{
    return widetag_of(*start_addr) == CODE_HEADER_WIDETAG &&
        pointer >= (void*)(start_addr + code_header_words(*start_addr));
}
extern int properly_tagged_p_internal(lispobj pointer, lispobj *start_addr);
static inline int properly_tagged_descriptor_p(void *pointer, lispobj *start_addr) {
  return is_lisp_pointer((lispobj)pointer) &&
    properly_tagged_p_internal((lispobj)pointer, start_addr);
}

extern void scavenge_control_stack(struct thread *th);
extern void scrub_control_stack(void);
extern void scrub_thread_control_stack(struct thread *);

#ifndef LISP_FEATURE_IMMOBILE_SPACE

static inline boolean filler_obj_p(lispobj* obj) { return 0; }

#else

extern void enliven_immobile_obj(lispobj*,int);
extern void fixup_immobile_refs(lispobj (*)(lispobj), lispobj, struct code*);

#define IMMOBILE_OBJ_VISITED_FLAG    0x10
#define IMMOBILE_OBJ_GENERATION_MASK 0x0f // mask off the VISITED flag

// Note: this does not work on a SIMPLE-FUN
// because a simple-fun header does not contain a generation.
#define __immobile_obj_generation(x) (__immobile_obj_gen_bits(x) & IMMOBILE_OBJ_GENERATION_MASK)

typedef int low_page_index_t;

#ifdef LISP_FEATURE_LITTLE_ENDIAN
static inline int immobile_obj_gen_bits(lispobj* pointer) // native pointer
{
  if (widetag_of(*pointer) == SIMPLE_FUN_WIDETAG)
    pointer = fun_code_header(pointer);
  return ((generation_index_t*)pointer)[3];
}
// Faster way when we know that the object can't be a simple-fun,
// such as when walking the immobile space.
static inline int __immobile_obj_gen_bits(lispobj* pointer) // native pointer
{
  return ((generation_index_t*)pointer)[3];
}
#else
#error "Need to define immobile_obj_gen_bits() for big-endian"
#endif /* little-endian */

static inline boolean filler_obj_p(lispobj* obj) {
  return *(int*)obj == (2<<N_WIDETAG_BITS | CODE_HEADER_WIDETAG);
}

#endif /* immobile space */

#define WEAK_POINTER_NWORDS \
        ALIGN_UP((sizeof(struct weak_pointer) / sizeof(lispobj)), 2)

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

/// Same as Lisp LOGBITP, except no negative bignums allowed.
static inline boolean layout_bitmap_logbitp(int index, lispobj bitmap)
{
    if (fixnump(bitmap))
      return (index < (N_WORD_BITS - N_FIXNUM_TAG_BITS))
          ? (bitmap >> (index+N_FIXNUM_TAG_BITS)) & 1
          : (sword_t)bitmap < 0;
    return positive_bignum_logbitp(index, (struct bignum*)native_pointer(bitmap));
}

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
    __sync_fetch_and_or(pflagbits, WP_CLEARED_BIT);
    __sync_fetch_and_and(pflagbits, ~WRITE_PROTECTED_BIT);
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

#ifdef LISP_FEATURE_IMMOBILE_SPACE
static inline void *
fixedobj_page_address(low_page_index_t page_num)
{
    return (void*)(FIXEDOBJ_SPACE_START + (page_num * IMMOBILE_CARD_BYTES));
}
static inline void *
varyobj_page_address(low_page_index_t page_num)
{
    return (void*)(VARYOBJ_SPACE_START + (page_num * IMMOBILE_CARD_BYTES));
}
#endif

#endif /* _GC_PRIVATE_H_ */
