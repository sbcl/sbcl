/*
 * Generational Conservative Garbage Collector for SBCL x86
 *
 * inline functions that gc-common.c needs sight of
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

#ifndef _GENCGC_INTERNAL_H_
#define _GENCGC_INTERNAL_H_

#include "gencgc-alloc-region.h"
#include "genesis/code.h"

/* Size of a page, in bytes. FIXME: needs to be conditionalized per
 * architecture, preferably by someone with a clue as to what page
 * sizes are on archs other than x86 and PPC - Patrik */
#define PAGE_BYTES 4096


void gc_free_heap(void);
inline int find_page_index(void *);
inline void *page_address(int);
int gencgc_handle_wp_violation(void *);

struct page {

    unsigned
        /* This is set when the page is write-protected. This should
	 * always reflect the actual write_protect status of a page.
	 * (If the page is written into, we catch the exception, make
	 * the page writable, and clear this flag.) */
        write_protected :1,
	/* This flag is set when the above write_protected flag is 
	 * cleared by the SIGBUS handler (or SIGSEGV handler, for some
	 * OSes). This is useful for re-scavenging pages that are
	 * written during a GC. */
	write_protected_cleared :1,
	/* the region the page is allocated to: 0 for a free page; 1
         * for boxed objects; 2 for unboxed objects. If the page is
         * free the following slots are invalid (well the bytes_used
         * must be 0). */
	allocated :3,
	/* If this page should not be moved during a GC then this flag
         * is set. It's only valid during a GC for allocated pages. */
	dont_move :1,
	/* If the page is part of a large object then this flag is
         * set. No other objects should be allocated to these pages.
         * This is only valid when the page is allocated. */
	large_object :1;

    /* the generation that this page belongs to. This should be valid
     * for all pages that may have objects allocated, even current
     * allocation region pages - this allows the space of an object to
     * be easily determined. */
    int  gen;

    /* the number of bytes of this page that are used. This may be less
     * than the actual bytes used for pages within the current
     * allocation regions. It should be 0 for all unallocated pages (not
     * hard to achieve). */
    int  bytes_used;

    /* The name of this field is not well-chosen for its actual use.
     * This is the offset from the start of the page to the start 
     * of the alloc_region which contains/contained it.  It's negative or 0
     */
    int  first_object_offset;
};

/* values for the page.allocated field */


/* the number of pages needed for the dynamic space - rounding up */
#define NUM_PAGES ((DYNAMIC_SPACE_SIZE+PAGE_BYTES-1)/PAGE_BYTES)

extern struct page page_table[NUM_PAGES];


/* forward declarations */

void sniff_code_object(struct code *code, unsigned displacement);
void gencgc_apply_code_fixups(struct code *old_code, struct code *new_code);

int  update_x86_dynamic_space_free_pointer(void);
void  gc_alloc_update_page_tables(int unboxed,
				  struct alloc_region *alloc_region);
void gc_alloc_update_all_page_tables(void);
void gc_set_region_empty(struct alloc_region *region);

/*
 * predicates
 */
static inline int 
space_matches_p(lispobj obj, int space)
{
    int page_index=(void*)obj - (void *)DYNAMIC_SPACE_START;
    return ((page_index >= 0)
	    && ((page_index =
		 ((unsigned int)page_index)/PAGE_BYTES) < NUM_PAGES)
	    && (page_table[page_index].gen == space));
}

static inline boolean
from_space_p(lispobj obj)
{
    return space_matches_p(obj,from_space);
}

static inline boolean
new_space_p(lispobj obj)
{
    return space_matches_p(obj,new_space);
}



#endif 
