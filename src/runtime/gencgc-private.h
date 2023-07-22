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

#include <stdbool.h>
void zeroize_pages_if_needed(page_index_t start, page_index_t end, int page_type);

typedef unsigned int page_bytes_t;
#define page_words_used(index) page_table[index].words_used_
#define page_bytes_used(index) ((page_bytes_t)page_table[index].words_used_<<WORD_SHIFT)
#if defined LISP_FEATURE_RISCV && defined LISP_FEATURE_LINUX // KLUDGE
#define page_need_to_zero(index) (mmap_does_not_zero || page_table[index].need_zerofill)
#else
#define page_need_to_zero(index) page_table[index].need_zerofill
#endif
#define set_page_bytes_used(index,val) page_table[index].words_used_ = ((val)>>WORD_SHIFT)
#define set_page_need_to_zero(index,val) page_table[index].need_zerofill = val

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
    unsigned int lsb = offset !=0 && IS_ALIGNED(offset, GENCGC_PAGE_BYTES);
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

#define is_code(type) ((type & PAGE_TYPE_MASK) == PAGE_TYPE_CODE)

// If *all* pages use soft card marks, then protection_mode() is not a thing.
// Otherwise, only pages of code use soft card marks; return an enum indicating
// whether the page protection for the specified page is applied in harware.
#ifndef LISP_FEATURE_SOFT_CARD_MARKS
enum prot_mode { PHYSICAL, LOGICAL };
static inline enum prot_mode protection_mode(page_index_t page) {
    // code pages can be marked as logically read-only without OS protection,
    // and everything else uses hardware-based protection where applicable.
    return is_code(page_table[page].type) ? LOGICAL : PHYSICAL;
}
#endif

static inline bool page_free_p(page_index_t page) {
    return (page_table[page].type == FREE_PAGE_FLAG);
}

#include "genesis/cardmarks.h"

/* Check that X is a higher address than Y and return offset from Y to
 * X in bytes. */
static inline os_vm_size_t
addr_diff(void *x, void *y)
{
    gc_assert(x >= y);
    return (uintptr_t)x - (uintptr_t)y;
}

/* As usually configured, generations 0-5 are normal collected generations,
   6 is pseudo-static (the objects in which are never moved nor reclaimed),
   and 7 is scratch space used when collecting a generation without promotion,
   wherein it is moved to generation 7 and back again.
 */
/*
 * SCRATCH_GENERATION as we've defined it is kinda stupid because "<"
 * doesn't do what you want. Other choices of value do, and since this an
 * enum, it should be possible to change. Except it isn't because .. reasons.
 * Here are some alternatives:
 *  A: gen 0 through 6 remain as-is and SCRATCH becomes -1
 *
 *  B: 1 = nursery, 2 = older, ... up through "old" 6 which becomes the new 7;
 *     and SCRATCH becomes 0. This is like alternative (A) but avoids negatives.
 *
 *  C: (probably the best)
 *  generations are stored with an implied decimal and one bit of fraction
 *  representing a half step so that:
 *     #b0000 = 0, #b0001 = 1/2   | #b0010 = 1, #b0011 = 1 1/2
 *     #b0100 = 2, #b0101 = 2 1/2 | #b0110 = 3, #b0111 = 3 1/2 ...
 *  up to 6 1/2. When GCing without promotion, we'd raise each object by half
 *  a generation, and then demote en masse, which is good because it makes the
 *  scratch pages older than from_space but younger than the youngest root gen.
 *
 * Of course, you could try to solve all this by keeping the existing numbering,
 * but expressing comparison "a < b" as either:
 *     "logical_gen(a) < logical_gen(b)" // re-map numerically before compare
 *  or "gen_lessp(a,b)" // just rename the comparator
 *
 * I generally prefer numeric comparison to just work, though we have a further
 * difficulty that page_table[page].gen is not always the generation of an object,
 * as when it is non-large and pinned. So the helpers might be needed anyway.
 */

enum {
    SCRATCH_GENERATION = PSEUDO_STATIC_GENERATION+1,
    NUM_GENERATIONS
};

extern struct generation generations[NUM_GENERATIONS];
extern os_vm_size_t bytes_allocated;

extern void reset_page_flags(page_index_t page);

/* True if the page starts a contiguous block. */
static inline bool
page_starts_contiguous_block_p(page_index_t page_index)
{
    // Don't use the preprocessor macro: 0 means 0.
    return page_table[page_index].scan_start_offset_ == 0;
}

/* True if the page is the last page in a contiguous block. */
static inline bool
page_ends_contiguous_block_p(page_index_t page_index,
                             generation_index_t __attribute__((unused)) gen)
{
    /* Re. this next test: git rev c769dd53 said that there was a bug when we don't
     * test page_bytes_used, but I fail to see how 'page_starts_contiguous_block_p'
     * on the next page is not a STRONGER condition, i.e. it should imply that
     * 'page_index' ends a block without regard for the number of bytes used.
     * Apparently at some point I understood this and now I don't again.
     * That's what comments are for, damnit.
     * Anyway, I *think* the issue was, at some point, as follows:
     * |   page             |     page   |
     *        pinned-obj
     *     <------------------- scan-start
     * where the first of the two pages had a small object pinned. This used to
     * adjust the bytes used to account _only_ for the pins.  That was wrong -
     * the page has to be counted as if it is completely full.
     * So _maybe_ both these conditions do not need to be present now ?
     */
    // There is *always* a next page in the page table.
    bool answer = page_words_used(page_index) < GENCGC_PAGE_WORDS
                  || page_starts_contiguous_block_p(page_index+1);
#ifdef DEBUG
    bool safe_answer =
           (/* page doesn't fill block */
            (page_words_used(page_index) < GENCGC_PAGE_WORDS)
            /* page is last allocated page */
            || ((page_index + 1) >= next_free_page)
            /* next page contains no data */
            || !page_words_used(page_index + 1)
            /* next page is in different generation */
            || (page_table[page_index + 1].gen != gen)
            /* next page starts its own contiguous block */
            || (page_starts_contiguous_block_p(page_index + 1)));
    gc_assert(answer == safe_answer);
#endif
    return answer;
}

static inline page_index_t contiguous_block_final_page(page_index_t first) {
    page_index_t last = first;
    while (!page_ends_contiguous_block_p(last, page_table[first].gen)) ++last;
    return last;
}

#endif /* _GENCGC_PRIVATE_H_ */
