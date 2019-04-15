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
#include "hopscotch.h"

#ifndef GENCGC_IS_PRECISE
#error "GENCGC_IS_PRECISE must be #defined as 0 or 1"
#endif

extern char *page_address(page_index_t);
int gencgc_handle_wp_violation(void *);


#if N_WORD_BITS == 64
  // It's more economical to store scan_start_offset using 4 bytes than 8.
  // Doing so makes struct page fit in 8 bytes if bytes_used takes 2 bytes.
  //   scan_start_offset = 4
  //   bytes_used        = 2
  //   flags             = 1
  //   gen               = 1
  // If bytes_used takes 4 bytes, then the above is 10 bytes which is padded to
  // 12, which is still an improvement over the 16 that it would have been.
# define CONDENSED_PAGE_TABLE 1
#else
# define CONDENSED_PAGE_TABLE 0
#endif

#if GENCGC_CARD_BYTES > USHRT_MAX
# if GENCGC_CARD_BYTES > UINT_MAX
#   error "GENCGC_CARD_BYTES unexpectedly large."
# else
    typedef unsigned int page_bytes_t;
# endif
#else
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
     * for scavenging). MUST be 0 for unallocated pages.
     */
#if CONDENSED_PAGE_TABLE
    // The low bit of the offset indicates the scale factor:
    // 0 = double-lispwords, 1 = gc cards. Large objects are card-aligned,
    // and this representation allows for a 32TB contiguous block using 32K
    // card size. Larger allocations will have pages that can't directly
    // store the full offset. That has to be dealt with by the accessor.
    unsigned int scan_start_offset_;
#else
    os_vm_size_t scan_start_offset_;
#endif

    /* the number of bytes of this page that are used. This may be less
     * than the actual bytes used for pages within the current
     * allocation regions. MUST be 0 for unallocated pages.
     * When read, the low bit has to be masked off.
     */
    page_bytes_t bytes_used_;

    // !!! If bit positions are changed, be sure to reflect the changes into
    // page_extensible_p() as well as ALLOCATION-INFORMATION in sb-introspect
    unsigned char
        /*
         * The low 4 bits of 'type' are interpreted as:
         *  0000 free
         *  ?001 boxed data
         *  ?010 unboxed data
         *  ?011 code
         *  1??? open region
         * The high bit indicates that the page holds part of or the entirety
         * of a single object and no other objects.
         * Constants for this field are defined in gc-internal.h, the
         * xxx_PAGE_FLAG definitions.
         *
         * If the page is free, all the following fields are zero. */
        type :5,
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
        /* If this page should not be moved during a GC then this flag
         * is set. It's only valid during a GC for allocated pages. */
        pinned :1;

    /* the generation that this page belongs to. This should be valid
     * for all pages that may have objects allocated, even current
     * allocation region pages - this allows the space of an object to
     * be easily determined. */
    generation_index_t gen;
};
extern struct page *page_table;
#ifdef LISP_FEATURE_BIG_ENDIAN
# define WP_CLEARED_FLAG      (1<<1)
# define WRITE_PROTECTED_FLAG (1<<2)
#else
# define WRITE_PROTECTED_FLAG (1<<5)
# define WP_CLEARED_FLAG      (1<<6)
#endif

struct __attribute__((packed)) corefile_pte {
  uword_t sso; // scan start offset
  page_bytes_t bytes_used;
};

/* values for the page.allocated field */


extern page_index_t page_table_pages;


/* forward declarations */

void update_dynamic_space_free_pointer(void);
void gc_close_region(struct alloc_region *alloc_region, int page_type_flag);
static inline void ensure_region_closed(struct alloc_region *alloc_region,
                                        int page_type_flag)
{
    if (alloc_region->start_addr)
        gc_close_region(alloc_region, page_type_flag);
}

static inline void gc_set_region_empty(struct alloc_region *region)
{
    /* last_page is not reset. It can be used as a hint where to resume
     * allocating after closing and re-opening the region */
    region->start_addr = region->free_pointer = region->end_addr = 0;
}

static inline void gc_init_region(struct alloc_region *region)
{
    region->last_page = 0; // must always be a valid page index
    gc_set_region_empty(region);
}

/*
 * predicates
 */

#define compacting_p() (from_space>=0)

/* Find the page index within the page_table for the given
 * address. Return -1 on failure. */
static inline page_index_t
find_page_index(void *addr)
{
    if (addr >= (void*)DYNAMIC_SPACE_START) {
        page_index_t index = ((uintptr_t)addr -
                              (uintptr_t)DYNAMIC_SPACE_START) / GENCGC_CARD_BYTES;
        if (index < page_table_pages)
            return (index);
    }
    return (-1);
}

#define SINGLE_OBJECT_FLAG (1<<4)
#define page_single_obj_p(page) ((page_table[page].type & SINGLE_OBJECT_FLAG)!=0)

#define page_has_smallobj_pins(page) \
  (page_table[page].pinned && !page_single_obj_p(page))
static inline boolean pinned_p(lispobj obj, page_index_t page)
{
    extern struct hopscotch_table pinned_objects;
    gc_dcheck(compacting_p());
#if !GENCGC_IS_PRECISE
    return page_has_smallobj_pins(page)
        && hopscotch_containsp(&pinned_objects, obj);
#else
    /* There is almost never anything in the hashtable on precise platforms */
    if (!pinned_objects.count || !page_has_smallobj_pins(page))
        return 0;
# ifdef RETURN_PC_WIDETAG
    /* Conceivably there could be a precise GC without RETURN-PC objects */
    if (widetag_of(native_pointer(obj)) == RETURN_PC_WIDETAG)
        obj = make_lispobj(fun_code_header(native_pointer(obj)),
                           OTHER_POINTER_LOWTAG);
# endif
    return hopscotch_containsp(&pinned_objects, obj);
#endif
}

// Return true only if 'obj' must be *physically* transported to survive gc.
// Return false if obj is in the immobile space regardless of its generation.
// Pretend pinned objects are not in oldspace so that they don't get moved.
static boolean __attribute__((unused))
from_space_p(lispobj obj)
{
    gc_dcheck(compacting_p());
    page_index_t page_index = find_page_index((void*)obj);
    return page_index >= 0
        && page_table[page_index].gen == from_space
        && !pinned_p(obj, page_index);
}

static boolean __attribute__((unused)) new_space_p(lispobj obj)
{
    gc_dcheck(compacting_p());
    page_index_t page_index = find_page_index((void*)obj);
    return page_index >= 0 && page_table[page_index].gen == new_space;
}

#ifdef LISP_FEATURE_IMMOBILE_SPACE
struct fixedobj_page { // 12 bytes per page
    union immobile_page_attr {
      int packed;
      struct {
        unsigned char flags;
        /* space per object in Lisp words. Can exceed obj_size
           to align on a larger boundary */
        unsigned char obj_align;
        unsigned char obj_size; /* in Lisp words, incl. header */
        /* Which generations have data on this page */
        unsigned char gens_; // a bitmap
      } parts;
    } attr;
    int free_index; // index is in bytes. 4 bytes
    short int prior_gc_free_word_index; // index is in words. 2 bytes
    /* page index of next page with same attributes */
    short int page_link; // 2 bytes
};
extern struct fixedobj_page *fixedobj_pages;
#define fixedobj_page_obj_align(i) (fixedobj_pages[i].attr.parts.obj_align<<WORD_SHIFT)
#define fixedobj_page_obj_size(i) fixedobj_pages[i].attr.parts.obj_size
#endif

extern page_index_t next_free_page;

extern uword_t
walk_generation(uword_t (*proc)(lispobj*,lispobj*,uword_t),
                generation_index_t generation, uword_t extra);

generation_index_t gc_gen_of(lispobj obj, int defaultval);

#endif /* _GENCGC_INTERNAL_H_*/
