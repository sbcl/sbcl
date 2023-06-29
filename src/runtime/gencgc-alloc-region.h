#ifndef _GENCGC_ALLOC_REGION_H_
#define _GENCGC_ALLOC_REGION_H_

/* Abstract out the data for an allocation region allowing a single
 * routine to be used for allocation and closing. */
/* Caution: if you change this, you may have to change compiler/generic/objdef
 * (for the THREAD object), all the backends' allocators, and room.lisp.
 * But as long as the first two words are left alone,
 * it's generally OK to add or remove other words.
 */
struct alloc_region {
    /* These two are needed for quick allocation. */
    void  *free_pointer;
    void  *end_addr; /* pointer to the byte after the last usable byte */
    void  *start_addr;
};

typedef struct {
    struct alloc_region cons;
    struct alloc_region mixed;
    uword_t token;
} arena_state;

// One region for each of page type.
// These indices have no correlation to PAGE_TYPE constants.
// MIXED has to always be at array index 0 because lisp accesses
// it directly in #-sb-thread builds.
extern struct alloc_region  gc_alloc_region[6];
#define mixed_region   (&gc_alloc_region[0])
#define small_mixed_region (&gc_alloc_region[1])
#define unboxed_region (&gc_alloc_region[2])
#define code_region    (&gc_alloc_region[3])
#define boxed_region   (&gc_alloc_region[4])
#define cons_region    (&gc_alloc_region[5])
#define ASSERT_REGIONS_CLOSED() \
    gc_assert(!((uintptr_t)gc_alloc_region[0].start_addr \
               |(uintptr_t)gc_alloc_region[1].start_addr \
               |(uintptr_t)gc_alloc_region[2].start_addr \
               |(uintptr_t)gc_alloc_region[3].start_addr \
               |(uintptr_t)gc_alloc_region[4].start_addr \
               |(uintptr_t)gc_alloc_region[5].start_addr))

extern int gencgc_alloc_profiler;

#endif /*  _GENCGC_ALLOC_REGION_H_ */
