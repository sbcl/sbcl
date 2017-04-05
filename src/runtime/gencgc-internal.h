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

void gc_free_heap(void);
extern void *page_address(page_index_t);
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
# define CONDENSED_PAGE_TABLE
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
     * for scavenging).
     */
#ifdef CONDENSED_PAGE_TABLE
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
     * allocation regions. It should be 0 for all unallocated pages (not
     * hard to achieve).
     * When read, the low bit has to be masked off.
     */
    page_bytes_t bytes_used_;

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
        // FIXME: this should be identical to (dont_move & !large_object),
        // so we don't need to store it as a bit unto itself.
        /* If this page is not a large object page and contains
         * any objects which are pinned */
        has_pins :1,
        /* If the page is part of a large object then this flag is
         * set. No other objects should be allocated to these pages.
         * This is only valid when the page is allocated. */
        large_object :1;

    /* the generation that this page belongs to. This should be valid
     * for all pages that may have objects allocated, even current
     * allocation region pages - this allows the space of an object to
     * be easily determined. */
    generation_index_t gen;
};
extern struct page *page_table;

#ifndef CONDENSED_PAGE_TABLE

// 32-bit doesn't need magic to reduce the size of scan_start_offset.
#define set_page_scan_start_offset(index,val) \
  page_table[index].scan_start_offset_ = val
#define page_scan_start_offset(index) page_table[index].scan_start_offset_

#else

/// A "condensed" offset reduces page table size, which improves scan locality.
/// As stored, the offset is scaled down either by card size or double-lispwords.
/// If the offset is the maximum, then we must check if the page pointed to by
/// that offset is actually the start of a region, and retry if not.
/// For debugging the iterative algorithm it helps to use a max value
/// that is less than UINT_MAX to get a pass/fail more quickly.

//#define SCAN_START_OFS_MAX 0x3fff
#define SCAN_START_OFS_MAX UINT_MAX

#define page_scan_start_offset(index) \
  (page_table[index].scan_start_offset_ != SCAN_START_OFS_MAX \
    ? (os_vm_size_t)(page_table[index].scan_start_offset_ & ~1) \
       << ((page_table[index].scan_start_offset_ & 1)?(GENCGC_CARD_SHIFT-1):WORD_SHIFT) \
    : scan_start_offset_iterated(index))

static os_vm_size_t __attribute__((unused))
scan_start_offset_iterated(page_index_t index)
{
    // The low bit of the MAX is the 'scale' bit. The max pages we can look
    // backwards is therefore the max shifted right by 1 bit.
    page_index_t tot_offset_in_pages = 0;
    unsigned int offset;
    do {
        page_index_t lookback_page = index - tot_offset_in_pages;
        offset = page_table[lookback_page].scan_start_offset_;
        tot_offset_in_pages += offset >> 1;
    } while (offset == SCAN_START_OFS_MAX);
    return (os_vm_size_t)tot_offset_in_pages << GENCGC_CARD_SHIFT;
}

/// This is a macro, but it could/should be an inline function.
/// Problem is that we need gc_assert() which is in gc-internal,
/// and it's easy enough for GC to flip around some stuff, but then
/// you have a different problem that more things get messed up,
/// such as {foo}-os.c. Basically we have inclusion order issues
/// that nobody ever bothers to deal with, in addition to the fact
/// that a something-internal header is *directly* included by others.
/// (Indirect inclusion should be allowed, direct should not be)
#define set_page_scan_start_offset(index, ofs) \
  { os_vm_size_t ofs_ = ofs; \
    unsigned int lsb_ = ofs_!=0 && !(ofs_ & (GENCGC_CARD_BYTES-1)); \
    os_vm_size_t scaled_ = \
     (ofs_ >> (lsb_ ? GENCGC_CARD_SHIFT-1 : WORD_SHIFT)) | lsb_; \
    if (scaled_ > SCAN_START_OFS_MAX) \
     { gc_assert(lsb_ == 1); scaled_ = SCAN_START_OFS_MAX; } \
    page_table[index].scan_start_offset_ = scaled_; }

#endif

/// There is some additional cleverness that could potentially be had -
/// the "need_to_zero" bit (a/k/a "page dirty") is obviously 1 if the page
/// contains objects. Only for an empty page must we distinguish between pages
/// not needing be zero-filled before next use and those which must be.
/// Thus, masking off the dirty bit could be avoided by not storing it for
/// any in-use page. But since that's not what we do - we set the bit to 1
/// as soon as a page is used - we do have to mask off the bit.
#define page_bytes_used(index) (page_table[index].bytes_used_ & ~1)
#define page_need_to_zero(index) (page_table[index].bytes_used_ & 1)
#define set_page_bytes_used(index,val) \
  page_table[index].bytes_used_ = (val) | page_need_to_zero(index)
#define set_page_need_to_zero(index,val) \
  page_table[index].bytes_used_ = page_bytes_used(index) | val

/* values for the page.allocated field */


extern page_index_t page_table_pages;


/* forward declarations */
#ifdef LISP_FEATURE_X86
void sniff_code_object(struct code *code, os_vm_size_t displacement);
void gencgc_apply_code_fixups(struct code *old_code, struct code *new_code);
#endif

sword_t update_dynamic_space_free_pointer(void);
void gc_alloc_update_page_tables(int page_type_flag, struct alloc_region *alloc_region);
void gc_alloc_update_all_page_tables(int);
void gc_set_region_empty(struct alloc_region *region);

/*
 * predicates
 */

/* Find the page index within the page_table for the given
 * address. Return -1 on failure. */
static inline page_index_t
find_page_index(void *addr)
{
    if (addr >= (void*)DYNAMIC_SPACE_START) {
        page_index_t index = ((pointer_sized_uint_t)addr -
                              (pointer_sized_uint_t)DYNAMIC_SPACE_START) / GENCGC_CARD_BYTES;
        if (index < page_table_pages)
            return (index);
    }
    return (-1);
}

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
static inline boolean pinned_p(lispobj obj, page_index_t page)
{
    extern struct hopscotch_table pinned_objects;
    return page_table[page].has_pins
        && hopscotch_containsp(&pinned_objects, obj);
}
#else
#  define pinned_p(obj, page) (0)
#endif

// Return true only if 'obj' must be *physically* transported to survive gc.
// Return false if obj is in the immobile space regardless of its generation.
// Pretend pinned objects are not in oldspace so that they don't get moved.
// Any lowtag bits on 'obj' are ignored.
static boolean __attribute__((unused))
from_space_p(lispobj obj)
{
    page_index_t page_index = find_page_index((void*)obj);
    return page_index >= 0
        && page_table[page_index].gen == from_space
        && !pinned_p(obj, page_index);
}

extern page_index_t last_free_page;
extern boolean gencgc_partial_pickup;

#endif
