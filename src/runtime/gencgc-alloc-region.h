#ifndef _GENCGC_ALLOC_REGION_H_
#define _GENCGC_ALLOC_REGION_H_

#define SEGREGATED_CODE 1
#include "gc.h"

#ifndef LISP_FEATURE_GENCGC
#error "gencgc-alloc-region.h included, but LISP_FEATURE_GENCGC not defined"
#endif

/* Abstract out the data for an allocation region allowing a single
 * routine to be used for allocation and closing. */
struct alloc_region {

    /* These two are needed for quick allocation. */
    void  *free_pointer;
    void  *end_addr; /* pointer to the byte after the last usable byte */

    /* These are needed when closing the region. */
    page_index_t  first_page;
    page_index_t  last_page;
    void  *start_addr;
};

#if SEGREGATED_CODE
// One region for each of {BOXED,UNBOXED,CODE}_PAGE_FLAG
extern struct alloc_region  gc_alloc_region[3];
#else
extern struct alloc_region  boxed_region;
extern struct alloc_region  unboxed_region;
#endif
extern generation_index_t from_space, new_space;
extern struct weak_pointer *weak_pointers;

#endif /*  _GENCGC_ALLOC_REGION_H_ */
