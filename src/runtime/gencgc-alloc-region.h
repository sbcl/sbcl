#ifndef _GENCGC_ALLOC_REGION_H_
#define _GENCGC_ALLOC_REGION_H_

#include "gc.h"

#ifndef LISP_FEATURE_GENCGC
#error "gencgc-alloc-region.h included, but LISP_FEATURE_GENCGC not defined"
#endif

/* Abstract out the data for an allocation region allowing a single
 * routine to be used for allocation and closing. */
/* Caution: if you change this, you may have to change compiler/generic/objdef
 * (for the THREAD object), all the backends' allocators, and room.lisp */
struct alloc_region {

    /* These two are needed for quick allocation. */
    void  *free_pointer;
    void  *end_addr; /* pointer to the byte after the last usable byte */

    /* These are needed when closing the region. */
    /* 'last_page' is identical to 'find_page_index((char*)end_addr - 1)'
     * whenever the region is in an open state. The value is preserved on
     * closing so that allocation can potentially resume where it left off,
     * though that's not quite how things are implemented at present.
     */
    page_index_t  last_page;
    void  *start_addr;
};

// One region for each of PAGE_TYPE_{MIXED,UNBOXED,CODE}
// CONS is not used yet. We distinguish at the call sites
// between PAGE_TYPE_MIXED and BOXED, but they are the same for now.
extern struct alloc_region  gc_alloc_region[3];
#define mixed_region   &gc_alloc_region[0]
#define unboxed_region &gc_alloc_region[1]
#define code_region    &gc_alloc_region[2]
#define cons_region    mixed_region

extern generation_index_t from_space, new_space;
extern int gencgc_alloc_profiler;

#endif /*  _GENCGC_ALLOC_REGION_H_ */
