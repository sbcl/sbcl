/*
 * Generational Conservative Garbage Collector for SBCL x86
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

#ifndef _GENCGC_H_
#define _GENCGC_H_

void gc_free_heap(void);
inline int find_page_index(void *);
inline void *page_address(int);
int gencgc_handle_wp_violation(void *);
lispobj *search_dynamic_space(lispobj *);

struct page {

    unsigned
        /* This is set when the page is write-protected. This should
	 * always reflect the actual write_protect status of a page.
	 * (If the page is written into, we catch the exception, make
	 * the page writable, and clear this flag.) */
        write_protected :1,
	/* This flag is set when the above write_protected flag is 
	 * cleared by the SIGBUS handler (or SIGSEGV handler, for some
	 * OSes). This is useful for * re-scavenging pages that are
	 * written during a GC. */
	write_protected_cleared :1,
	/* the region the page is allocated to: 0 for a free page; 1
         * for boxed objects; 2 for unboxed objects. If the page is
         * free the following slots are invalid (well the bytes_used
         * must be 0). */
	allocated :2,
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

    /* It is important to know the offset to the first object in the
     * page. Currently it's only important to know if an object starts
     * at the beginning of the page in which case the offset would be 0. */
    int  first_object_offset;
};

/* values for the page.allocated field */
#define FREE_PAGE 0
#define BOXED_PAGE 1
#define UNBOXED_PAGE 2

/* the number of pages needed for the dynamic space - rounding up */
#define NUM_PAGES ((DYNAMIC_SPACE_SIZE+4095)/4096)
extern struct page page_table[NUM_PAGES];

/* Abstract out the data for an allocation region allowing a single
 * routine to be used for allocation and closing. */
struct alloc_region {

    /* These two are needed for quick allocation. */
    void  *free_pointer;
    void  *end_addr; /* pointer to the byte after the last usable byte */

    /* These are needed when closing the region. */
    int  first_page;
    int  last_page;
    void  *start_addr;
};

extern struct alloc_region  boxed_region;
extern struct alloc_region  unboxed_region;

void  gencgc_pickup_dynamic(void);

void sniff_code_object(struct code *code, unsigned displacement);

int  update_x86_dynamic_space_free_pointer(void);
void  gc_alloc_update_page_tables(int unboxed,
				  struct alloc_region *alloc_region);

#endif _GENCGC_H_
