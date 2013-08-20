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

#include <limits.h>
#include "gc.h"
#include "gencgc-alloc-region.h"
#include "genesis/code.h"

void gc_free_heap(void);
extern page_index_t find_page_index(void *);
extern void *page_address(page_index_t);
int gencgc_handle_wp_violation(void *);


#if GENCGC_CARD_BYTES > USHRT_MAX
# if GENCGC_CARD_BYTES > UINT_MAX
#   error "GENCGC_CARD_BYTES unexpectedly large."
# else
#   define PAGE_BYTES_FMT "u"
    typedef unsigned int page_bytes_t;
# endif
#else
# define PAGE_BYTES_FMT "hu"
  typedef unsigned short page_bytes_t;
#endif

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
     * for scavenging).
     */
    os_vm_size_t scan_start_offset;

    /* the number of bytes of this page that are used. This may be less
     * than the actual bytes used for pages within the current
     * allocation regions. It should be 0 for all unallocated pages (not
     * hard to achieve).
     */
    page_bytes_t bytes_used;

    unsigned char
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
        /*  000 free
         *  ?01 boxed data
         *  ?10 unboxed data
         *  ?11 code
         *  1?? open region
         *
         * Constants for this field are defined in gc-internal.h, the
         * xxx_PAGE_FLAG definitions.
         *
         * If the page is free the following slots are invalid, except
         * for the bytes_used which must be zero. */
        allocated :3,
        /* If this page should not be moved during a GC then this flag
         * is set. It's only valid during a GC for allocated pages. */
        dont_move :1,
        /* If the page is part of a large object then this flag is
         * set. No other objects should be allocated to these pages.
         * This is only valid when the page is allocated. */
        large_object :1,
        /* Cleared if the page is known to contain only zeroes. */
        need_to_zero :1;

    /* the generation that this page belongs to. This should be valid
     * for all pages that may have objects allocated, even current
     * allocation region pages - this allows the space of an object to
     * be easily determined. */
    generation_index_t gen;
};


/* values for the page.allocated field */


extern page_index_t page_table_pages;
extern struct page *page_table;


/* forward declarations */
#ifdef LISP_FEATURE_X86
void sniff_code_object(struct code *code, os_vm_size_t displacement);
void gencgc_apply_code_fixups(struct code *old_code, struct code *new_code);
#endif

sword_t update_dynamic_space_free_pointer(void);
void gc_alloc_update_page_tables(int page_type_flag, struct alloc_region *alloc_region);
void gc_alloc_update_all_page_tables(void);
void gc_set_region_empty(struct alloc_region *region);

/*
 * predicates
 */

static inline boolean
space_matches_p(lispobj obj, generation_index_t space)
{
    if (obj >= DYNAMIC_SPACE_START) {
        page_index_t page_index=((pointer_sized_uint_t)obj
                                 - DYNAMIC_SPACE_START) / GENCGC_CARD_BYTES;
        return ((page_index < page_table_pages) &&
                (page_table[page_index].gen == space));
    } else {
        return 0;
    }
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

extern page_index_t last_free_page;
extern boolean gencgc_partial_pickup;

#endif
