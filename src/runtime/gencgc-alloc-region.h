#ifndef _GENCGC_ALLOC_REGION_H_
#define _GENCGC_ALLOC_REGION_H_

#include "gc.h"

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

extern struct alloc_region  boxed_region;
extern struct alloc_region  unboxed_region;
extern generation_index_t from_space, new_space;
extern struct weak_pointer *weak_pointers;

#endif /*  _GENCGC_ALLOC_REGION_H_ */
