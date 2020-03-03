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

#ifndef _GENCGC_PRIVATE_H_
#define _GENCGC_PRIVATE_H_

void zero_dirty_pages(page_index_t start, page_index_t end, int page_type);

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

#if !CONDENSED_PAGE_TABLE

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

static void __attribute__((unused))
set_page_scan_start_offset(page_index_t index, os_vm_size_t offset)
{
    // If the offset is nonzero and page-aligned
    unsigned int lsb = offset !=0 && IS_ALIGNED(offset, GENCGC_CARD_BYTES);
    os_vm_size_t scaled = (offset >> (lsb ? GENCGC_CARD_SHIFT-1 : WORD_SHIFT)) | lsb;
    if (scaled > SCAN_START_OFS_MAX) {
        // Assert that if offset exceed the max representable value,
        // then it is a page-aligned offset, not a cons-aligned offset.
        gc_assert(lsb == 1);
        // Clip it to the maximum representable value.
        // The accessor will have to iterate to find the true start of region.
        scaled = SCAN_START_OFS_MAX;
    }
    page_table[index].scan_start_offset_ = scaled;
}

static os_vm_size_t scan_start_offset_iterated(page_index_t index)
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

static os_vm_size_t  __attribute__((unused)) page_scan_start_offset(page_index_t index)
{
    return page_table[index].scan_start_offset_ != SCAN_START_OFS_MAX
        ? (os_vm_size_t)(page_table[index].scan_start_offset_ & ~1)
          << ((page_table[index].scan_start_offset_ & 1) ?
              (GENCGC_CARD_SHIFT-1) : WORD_SHIFT)
        : scan_start_offset_iterated(index);
}

#endif

enum prot_mode { PHYSICAL, LOGICAL };
static inline enum prot_mode protection_mode(page_index_t page) {
#if CODE_PAGES_USE_SOFT_PROTECTION
    // code pages can be marked as logically read-only without OS protection,
    // and everything else uses hardware-based protection where applicable.
    return ((page_table[page].type & PAGE_TYPE_MASK) == CODE_PAGE_TYPE)
        ? LOGICAL : PHYSICAL;
#else
    return PHYSICAL; // all pages, if protected, use hardware-based protection
#endif
}

#ifndef LISP_FEATURE_SB_THREAD
#define SINGLE_THREAD_BOXED_REGION (struct alloc_region*)(STATIC_SPACE_START + 2*N_WORD_BYTES)
#endif
#endif /* _GENCGC_PRIVATE_H_ */
