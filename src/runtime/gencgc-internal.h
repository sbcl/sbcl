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

#define GENCGC_PAGE_WORDS (GENCGC_PAGE_BYTES/N_WORD_BYTES)
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

#if GENCGC_PAGE_WORDS > USHRT_MAX
# if GENCGC_PAGE_WORDS > UINT_MAX
#   error "GENCGC_PAGE_WORDS unexpectedly large."
# else
    typedef unsigned int page_words_t;
# endif
#else
  typedef unsigned short page_words_t;
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

    /* the number of lispwords of this page that are used. This may be less
     * than the usage at an instant in time for pages within the current
     * allocation regions. MUST be 0 for unallocated pages.
     */
    page_words_t words_used_;

    // !!! If bit positions are changed, be sure to reflect the changes into
    // page_extensible_p() as well as ALLOCATION-INFORMATION in sb-introspect
    unsigned char
        /*
         * The low 4 bits of 'type' are interpreted as:
         *  0000 free
         *  ?001 strictly boxed data (pointers, immediates, object headers)
         *  ?010 strictly unboxed data
         *  ?011 mixed boxed/unboxed non-code objects
         *  ?111 code
         *  1??? open region
         * The high bit indicates that the page holds part of or the entirety
         * of a single object and no other objects.
         * Constants for this field are defined in gc-internal.h, the
         * xxx_PAGE_FLAG definitions. */
        type :5,
        /* Whether the page was used at all. This is the only bit that can
         * be 1 on a free page */
        need_zerofill :1,
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
#else
# define WP_CLEARED_FLAG      (1<<6)
#endif

/* When computing a card index we never subtract the heap base, which simplifies
 * code generation. Because there is no alignment constraint beyond being card-aligned,
 * the low bits can wraparound from all 1s to all 0s such that lowest numbered
 * page index in linear order may have a higher card index.
 * As a small example of the distinction between page index and card index:
 *   heap base = 0xEB00, card size = 256 bytes, total cards = 8, mask = #b111
 *
 *     page     page      card
 *    index     addr     index
 *       0      EB00        3
 *       1      EC00        4
 *       2      ED00        5
 *       3      EE00        6
 *       4      EF00        7
 *       5      F000        0
 *       6      F100        1
 *       7      F200        2
 */
extern unsigned char *gc_card_mark;
extern long gc_card_table_mask;
#define addr_to_card_index(addr) ((((uword_t)addr)>>GENCGC_CARD_SHIFT) & gc_card_table_mask)
#define page_to_card_index(n) addr_to_card_index(page_address(n))
#define PAGE_WRITEPROTECTED_P(n) (gc_card_mark[page_to_card_index(n)] & 1)
#define SET_PAGE_PROTECTED(n,val) gc_card_mark[page_to_card_index(n)] =\
      (val?CARD_UNMARKED:CARD_MARKED)

struct __attribute__((packed)) corefile_pte {
  uword_t sso; // scan start offset
  page_words_t words_used; // low bit is the 'large object' flag
};

/* values for the page.allocated field */


extern page_index_t page_table_pages;


/* forward declarations */

void update_dynamic_space_free_pointer(void);
void gc_close_region(struct alloc_region *alloc_region, int page_type);
static inline void ensure_region_closed(struct alloc_region *alloc_region,
                                        int page_type)
{
    if (alloc_region->start_addr)
        gc_close_region(alloc_region, page_type);
}

static inline void gc_set_region_empty(struct alloc_region *region)
{
    /* Free-pointer has to be not equal to 0 because it's undefined behavior
     * to add any value whatsoever to the null pointer.
     * Annoying, isn't it.  http://c-faq.com/null/machexamp.html */
    region->free_pointer = region->end_addr = (void*)0x1000;
    /* Start 0 is the indicator of closed-ness. */
    region->start_addr = 0;
    /* last_page is not reset. It can be used as a hint where to resume
     * allocating after closing and re-opening the region */
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
                              (uintptr_t)DYNAMIC_SPACE_START) / GENCGC_PAGE_BYTES;
        if (index < page_table_pages)
            return (index);
    }
    return (-1);
}

#define SINGLE_OBJECT_FLAG (1<<4)
#define page_single_obj_p(page) ((page_table[page].type & SINGLE_OBJECT_FLAG)!=0)

static inline boolean pinned_p(lispobj obj, page_index_t page)
{
    extern struct hopscotch_table pinned_objects;
    // Single-object pages can be pinned, but the object doesn't go
    // in the hashtable. I'm a little surprised that the return value
    // should be 0 in such case, but I think this never gets called
    // on large objects because they've all been "moved" to newspace
    // by adjusting the page table. Perhaps this should do:
    //   gc_assert(!page_single_obj_p(page))
    if (!page_table[page].pinned || page_single_obj_p(page)) return 0;
#ifdef RETURN_PC_WIDETAG
    if (widetag_of(native_pointer(obj)) == RETURN_PC_WIDETAG)
        obj = make_lispobj(fun_code_header(native_pointer(obj)),
                           OTHER_POINTER_LOWTAG);
#endif
    return hopscotch_containsp(&pinned_objects, obj);
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
        unsigned char obj_align; // object spacing expressed in lisp words
        unsigned char unused1;
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
#endif

extern page_index_t next_free_page;

extern uword_t
walk_generation(uword_t (*proc)(lispobj*,lispobj*,uword_t),
                generation_index_t generation, uword_t extra);

generation_index_t gc_gen_of(lispobj obj, int defaultval);

#endif /* _GENCGC_INTERNAL_H_*/
