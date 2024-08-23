/*
 * GENerational Conservative Garbage Collector for SBCL
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

/*
 * For a review of garbage collection techniques (e.g. generational
 * GC) and terminology (e.g. "scavenging") see Paul R. Wilson,
 * "Uniprocessor Garbage Collection Techniques" available at
 *   <https://www.cs.rice.edu/~javaplt/311/Readings/wilson92uniprocessor.pdf>
 * or
 *   <ftp://ftp.cs.utexas.edu/pub/garbage/bigsurv.ps>.
 */

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <inttypes.h>
#include "genesis/sbcl.h"
#include "runtime.h"
#include "os.h"
#include "interr.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"
#include "arch.h"
#include "gc.h"
#include "thread.h"
#include "pseudo-atomic.h"
#include "code.h"
#include "genesis/gc-tables.h"
#include "genesis/vector.h"
#include "genesis/weak-pointer.h"
#include "genesis/symbol.h"
#include "genesis/hash-table.h"
#include "genesis/instance.h"
#include "hopscotch.h"
#include "genesis/cons.h"
#include "genesis/brothertree.h"
#include "genesis/split-ordered-list.h"
#include "var-io.h"

/* forward declarations */
extern FILE *gc_activitylog();

/* Largest allocation seen since last GC. */
os_vm_size_t large_allocation = 0;
int n_lisp_gcs;


/*
 * debugging
 */

/* the verbosity level. All non-error messages are disabled at level 0;
 * and only a few rare messages are printed at level 1. */
int gencgc_verbose = 0;

/*
 * GC structures and variables
 */

/* the total bytes allocated. These are seen by Lisp DYNAMIC-USAGE. */
os_vm_size_t bytes_allocated = 0;
os_vm_size_t auto_gc_trigger = 0;

/* the source and destination generations. These are set before a GC starts
 * scavenging. */
generation_index_t from_space;
generation_index_t new_space;

/* Set to 1 when in GC */
bool gc_active_p = 0;

/* should the GC be conservative on stack. If false (only right before
 * saving a core), don't scan the stack / mark pages pinned. */
bool conservative_stack = 1;
int save_lisp_gc_iteration;

/* An array of page structures is allocated on gc initialization.
 * This helps to quickly map between an address and its page structure.
 * page_table_pages is set from the size of the dynamic space. */
page_index_t page_table_pages;
struct page *page_table;
unsigned char *gc_page_pins;
unsigned char *gc_card_mark;
// Filtered pins include code but not simple-funs,
// and must not include invalid pointers.
static lispobj* gc_filtered_pins;
static int pins_alloc_size;
static int gc_pin_count;
struct hopscotch_table pinned_objects;

/* This is always 0 except during gc_and_save() */
lispobj lisp_init_function;

static inline bool boxed_type_p(int type) { return type > 1; }
static inline bool page_boxed_p(page_index_t page) {
    // ignore SINGLE_OBJECT_FLAG and OPEN_REGION_PAGE_FLAG
    return boxed_type_p(page_table[page].type & PAGE_TYPE_MASK);
}

#ifndef LISP_FEATURE_SOFT_CARD_MARKS
static inline bool protect_page_p(page_index_t page, generation_index_t generation) {
    return (page_boxed_p(page)
            && !(page_table[page].type & OPEN_REGION_PAGE_FLAG)
            && (page_words_used(page) != 0)
            && !gc_page_pins[page]
            && (page_table[page].gen == generation));
}
#endif

/* Calculate the address where the allocation region associated with
 * the page starts. */
static inline void *
page_scan_start(page_index_t page_index)
{
    return page_address(page_index)-page_scan_start_offset(page_index);
}

/* We maintain the invariant that pages with FREE_PAGE_FLAG have
 * scan_start of zero, to optimize page_ends_contiguous_block_p().
 * Clear all the flags that don't pertain to a free page.
 * Particularly the 'need_zerofill' bit MUST remain as-is */
void reset_page_flags(page_index_t page) {
    page_table[page].scan_start_offset_ = 0;
    set_page_type(page_table[page], FREE_PAGE_FLAG);
    gc_page_pins[page] = 0;
    // Why can't the 'gen' get cleared? It caused failures. THIS MAKES NO SENSE!!!
    //    page_table[page].gen = 0;
    // Free pages are dirty (MARKED) because MARKED is equivalent
    // to not-write-protected, which is what you want for allocation.
    assign_page_card_marks(page, CARD_MARKED);
}

#include "genesis/cardmarks.h"
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
int page_cards_all_marked_nonsticky(page_index_t page) {
    return cardseq_all_marked_nonsticky(page_to_card_index(page));
}
#endif

/// External function for calling from Lisp.
page_index_t ext_find_page_index(void *addr) { return find_page_index(addr); }

/* an array of generation structures. There needs to be one more
 * generation structure than actual generations as the oldest
 * generation is temporarily raised then lowered. */
struct generation generations[NUM_GENERATIONS];

/* the oldest generation that is will currently be GCed by default.
 * Valid values are: 0, 1, ... HIGHEST_NORMAL_GENERATION
 *
 * The default of HIGHEST_NORMAL_GENERATION enables GC on all generations.
 *
 * Setting this to 0 effectively disables the generational nature of
 * the GC. In some applications generational GC may not be useful
 * because there are no long-lived objects.
 *
 * An intermediate value could be handy after moving long-lived data
 * into an older generation so an unnecessary GC of this long-lived
 * data can be avoided. */
generation_index_t gencgc_oldest_gen_to_gc = HIGHEST_NORMAL_GENERATION;

page_index_t next_free_page; // upper (exclusive) bound on used page range

#ifdef LISP_FEATURE_SB_THREAD
/* This lock is to prevent multiple threads from simultaneously
 * allocating new regions which overlap each other.  Note that the
 * majority of GC is single-threaded, but alloc() may be called from
 * >1 thread at a time and must be thread-safe.  This lock must be
 * seized before all accesses to generations[] or to parts of
 * page_table[] that other threads may want to see */
#ifdef LISP_FEATURE_WIN32
static CRITICAL_SECTION free_pages_lock;
#else
static pthread_mutex_t free_pages_lock = PTHREAD_MUTEX_INITIALIZER;
#endif
#endif

void acquire_gc_page_table_lock() { ignore_value(mutex_acquire(&free_pages_lock)); }
void release_gc_page_table_lock() { ignore_value(mutex_release(&free_pages_lock)); }

extern os_vm_size_t gencgc_alloc_granularity;
os_vm_size_t gencgc_alloc_granularity = GENCGC_ALLOC_GRANULARITY;


/*
 * miscellaneous heap functions
 */

static void show_pinnedobj_count()
{
    page_index_t page;
    int nwords = 0;
    int n_pinned_largeobj = 0;
    for (page = 0; page < next_free_page; ++page) {
        if (page_table[page].gen == from_space && gc_page_pins[page]
                && page_single_obj_p(page)) {
            nwords += page_words_used(page);
            if (page_starts_contiguous_block_p(page))
                ++n_pinned_largeobj;
        }
    }
    fprintf(stderr,
            "/pinned objects(g%d): large=%d (%d words), small=%d\n",
            from_space, n_pinned_largeobj, nwords, pinned_objects.count);
}

/* Work through the pages and add up the number of bytes used for the
 * given generation. */
static __attribute__((unused)) os_vm_size_t
count_generation_bytes_allocated (generation_index_t gen)
{
    page_index_t i;
    os_vm_size_t result = 0;
    for (i = 0; i < next_free_page; i++) {
        if (!page_free_p(i) && page_table[i].gen == gen)
            result += page_words_used(i);
    }
    return result*N_WORD_BYTES;
}


/* The generation currently being allocated to. */
static generation_index_t gc_alloc_generation;

__attribute__((unused)) static const char * const page_type_description[8] =
  {0, "unboxed", "boxed", "mixed", "sm_mix", "cons", "?", "code"};

/*
 * To support quick and inline allocation, regions of memory can be
 * allocated and then allocated from with just a free pointer and a
 * check against an end address.
 *
 * Since objects can be allocated to spaces with different properties
 * e.g. boxed/unboxed, generation, ages; there may need to be many
 * allocation regions.
 *
 * Each allocation region may start within a partly used page. Many
 * features of memory use are noted on a page wise basis, e.g. the
 * generation; so if a region starts within an existing allocated page
 * it must be consistent with this page.
 *
 * During the scavenging of the newspace, objects will be transported
 * into an allocation region, and pointers updated to point to this
 * allocation region. It is possible that these pointers will be
 * scavenged again before the allocation region is closed, e.g. due to
 * trans_list which jumps all over the place to cleanup the list. It
 * is important to be able to determine properties of all objects
 * pointed to when scavenging, e.g to detect pointers to the oldspace.
 * Thus it's important that the allocation regions have the correct
 * properties set when allocated, and not just set when closed. The
 * region allocation routines return regions with the specified
 * properties, and grab all the pages, setting their properties
 * appropriately, except that the amount used is not known.
 *
 * These regions are used to support quicker allocation using just a
 * free pointer. The actual space used by the region is not reflected
 * in the pages tables until it is closed. It can't be scavenged until
 * closed.
 *
 * When finished with the region it should be closed, which will
 * update the page tables for the actual space used returning unused
 * space. Further it may be noted in the new regions which is
 * necessary when scavenging the newspace.
 *
 * Large objects may be allocated directly without an allocation
 * region, the page table is updated immediately.
 *
 * Unboxed objects don't contain pointers to other objects and so
 * don't need scavenging. Further they can't contain pointers to
 * younger generations so WP is not needed. By allocating pages to
 * unboxed objects the whole page never needs scavenging or
 * write-protecting. */

/* We use five regions for the current newspace generation. */
struct alloc_region gc_alloc_region[6];

static page_index_t
  alloc_start_pages[8], // one for each value of PAGE_TYPE_x
  max_alloc_start_page; // the largest of any array element
page_index_t gencgc_alloc_start_page; // initializer for the preceding array

/* Each 'start_page' informs the region-opening logic where it should
 * attempt to continue allocating after closing a region associated
 * with a particular page type. We aren't very clever about this -
 * either the start_page has space remaining or it doesn't, and when it
 * doesn't, then we should hop over *all* allocated pages regardless of
 * type that intercede between the page we couldn't use up to next_free_page.
 * It's kind of dumb that there is one start_page per type,
 * other than it serves its purpose for picking up where it left off
 * on a partially full page during GC */
#define RESET_ALLOC_START_PAGES() \
        alloc_start_pages[0] = gencgc_alloc_start_page; \
        alloc_start_pages[1] = gencgc_alloc_start_page; \
        alloc_start_pages[2] = gencgc_alloc_start_page; \
        alloc_start_pages[3] = gencgc_alloc_start_page; \
        alloc_start_pages[4] = gencgc_alloc_start_page; \
        alloc_start_pages[5] = gencgc_alloc_start_page; \
        alloc_start_pages[6] = gencgc_alloc_start_page; \
        alloc_start_pages[7] = gencgc_alloc_start_page; \
        max_alloc_start_page = gencgc_alloc_start_page;

static page_index_t
get_alloc_start_page(unsigned int page_type)
{
    if (page_type > 7) lose("bad page_type: %d", page_type);
    struct thread* th = get_sb_vm_thread();
    page_index_t global_start = alloc_start_pages[page_type];
    page_index_t hint;
    switch (page_type) {
    case PAGE_TYPE_MIXED:
        if ((hint = thread_extra_data(th)->mixed_page_hint) > 0 && hint <= global_start) {
            thread_extra_data(th)->mixed_page_hint = - 1;
            return hint;
        }
        break;
    case PAGE_TYPE_CONS:
        if ((hint = thread_extra_data(th)->cons_page_hint) > 0 && hint <= global_start) {
            thread_extra_data(th)->cons_page_hint = - 1;
            return hint;
        }
        break;
    }
    return global_start;
}

static inline void
set_alloc_start_page(unsigned int page_type, page_index_t page)
{
    if (page_type > 7) lose("bad page_type: %d", page_type);
    if (page > max_alloc_start_page) max_alloc_start_page = page;
    alloc_start_pages[page_type] = page;
}
#include "private-cons.inc"

/* Find a new region with room for at least the given number of bytes.
 *
 * It starts looking at the current generation's alloc_start_page. So
 * may pick up from the previous region if there is enough space. This
 * keeps the allocation contiguous when scavenging the newspace.
 *
 * The alloc_region should have been closed by a call to
 * gc_close_region(), and will thus be in an empty state.
 *
 * To assist the scavenging functions write-protected pages are not
 * used. Free pages should not be write-protected.
 *
 * It is critical to the conservative GC that the start of regions be
 * known. To help achieve this only small regions are allocated at a
 * time.
 *
 * During scavenging, pointers may be found to within the current
 * region and the page generation must be set so that pointers to the
 * from space can be recognized. Therefore the generation of pages in
 * the region are set to gc_alloc_generation. To prevent another
 * allocation call using the same pages, all the pages in the region
 * are allocated, although they will initially be empty.
 */

#ifdef LISP_FEATURE_ALLOCATOR_METRICS
#define INSTRUMENTING(expression, metric) { \
    struct timespec t0, t1; clock_gettime(CLOCK_REALTIME, &t0); expression; \
    clock_gettime(CLOCK_REALTIME, &t1); \
    struct thread* th = get_sb_vm_thread(); \
    th->metric += (t1.tv_sec - t0.tv_sec)*1000000000 + (t1.tv_nsec - t0.tv_nsec); }
#else
#define INSTRUMENTING(expression, metric) expression
#endif

/* Test whether page 'index' can continue a non-large-object region
 * having specified 'gen' and 'type' values. It must not be pinned
 * and must be marked but not referenced from the stack */
static inline bool
page_extensible_p(page_index_t index, generation_index_t gen, int type) {
#ifdef LISP_FEATURE_BIG_ENDIAN /* TODO: implement this as single comparison */
    int attributes_match =
           page_table[index].type == type
        && page_table[index].gen == gen
        && !gc_page_pins[index];
#else
    // FIXME: "warning: dereferencing type-punned pointer will break strict-aliasing rules"
    int attributes_match =
        *(int16_t*)&page_table[index].type == ((gen<<8)|type);
#endif
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    return attributes_match && page_cards_all_marked_nonsticky(index);
#else
    return attributes_match && !PAGE_WRITEPROTECTED_P(index);
#endif
}

void gc_heap_exhausted_error_or_lose (sword_t available, sword_t requested) never_returns;

/* Find a single page for conses or SMALL_MIXED objects.
 * CONS differs because:
 * - not all GENCGC_PAGE_BYTES of the page can be used.
 * - a region can't be extended from one page to the next
 *   (implied by the preceding restriction).
 * SMALL_MIXED is similar to cons, but all bytes of the page can be used
 * for storing objects, subject to the non-card-spaning constraint. */
static page_index_t find_single_page(int page_type, sword_t nbytes, generation_index_t gen)
{
    page_index_t page = alloc_start_pages[page_type];;
    // Compute the max words that could already be used while satisfying the request.
    page_words_t usage_allowance =
        usage_allowance = GENCGC_PAGE_BYTES/N_WORD_BYTES - (nbytes>>WORD_SHIFT);
    if (page_type == PAGE_TYPE_CONS) {
        gc_assert(nbytes <= CONS_PAGE_USABLE_BYTES);
        usage_allowance = (CONS_SIZE*MAX_CONSES_PER_PAGE) - (nbytes>>WORD_SHIFT);
    }
    for ( ; page < page_table_pages ; ++page) {
        if (page_words_used(page) <= usage_allowance
            && (page_free_p(page) || page_extensible_p(page, gen, page_type))) return page;
    }
    /* Compute the "available" space for the lossage message. This is kept out of the
     * search loop because it's needless overhead. Any free page would have been returned,
     * so we just have to find the least full page meeting the gen+type criteria */
    sword_t min_used = GENCGC_PAGE_WORDS;
    for ( page = alloc_start_pages[page_type]; page < page_table_pages ; ++page) {
        if (page_words_used(page) < min_used && page_extensible_p(page, gen, page_type))
            min_used = page_words_used(page);
    }
    sword_t bytes_avail;
    if (page_type == PAGE_TYPE_CONS) {
        bytes_avail = CONS_PAGE_USABLE_BYTES - (min_used<<WORD_SHIFT);
        /* The sentinel value initially in 'least_words_used' exceeds a cons
         * page's capacity, so clip to 0 instead of showing a negative value
         * if no page matched on gen+type */
        if (bytes_avail < 0) bytes_avail = 0;
    } else {
        bytes_avail = GENCGC_PAGE_BYTES - (min_used<<WORD_SHIFT);
    }
    gc_heap_exhausted_error_or_lose(bytes_avail, nbytes);
}

#if 0
bool page_is_zeroed(page_index_t page)
{
    int nwords_per_page = GENCGC_PAGE_BYTES/N_WORD_BYTES;
    uword_t *pagebase = (void*)page_address(page);
    int i;
    for (i=0; i<nwords_per_page; ++i) if (pagebase[i]) return 0;
    return 1;
}
#endif

static void*
gc_alloc_new_region(sword_t nbytes, int page_type, struct alloc_region *alloc_region, int unlock)
{
    /* Check that the region is in a reset state. */
    gc_dcheck(!alloc_region->start_addr);

    if (page_type == PAGE_TYPE_CONS || page_type == PAGE_TYPE_SMALL_MIXED) {
        // No mutex release, because either this is:
        //   - not called from Lisp, as in the SMALL_MIXED case
        //   - called from lisp_alloc() which does its own unlock
        gc_dcheck(!unlock);
        page_index_t page;
        INSTRUMENTING(page = find_single_page(page_type, nbytes, gc_alloc_generation),
                      et_find_freeish_page);
        if (page+1 > next_free_page) next_free_page = page+1;
        page_table[page].gen = gc_alloc_generation;
        set_page_type(page_table[page], OPEN_REGION_PAGE_FLAG | page_type);
        if (!page_words_used(page))
            prepare_pages(1, page, page, page_type, gc_alloc_generation);
        // Don't need to set the scan_start_offset because free pages have it 0
        // (and each of these page types starts a new contiguous block)
        gc_dcheck(page_table[page].scan_start_offset_ == 0);
        alloc_region->start_addr = page_address(page) + page_bytes_used(page);
        if (page_type == PAGE_TYPE_CONS) {
            alloc_region->end_addr = page_address(page) + CONS_PAGE_USABLE_BYTES;
        } else {
            alloc_region->end_addr =
              (char*)ALIGN_DOWN((uword_t)alloc_region->start_addr, GENCGC_CARD_BYTES) + GENCGC_CARD_BYTES;
          }
        alloc_region->free_pointer = alloc_region->start_addr;
        gc_assert(find_page_index(alloc_region->start_addr) == page);
        return alloc_region->free_pointer;
    }

    page_index_t first_page = get_alloc_start_page(page_type), last_page;

    INSTRUMENTING(
    last_page = gc_find_freeish_pages(&first_page, nbytes,
                                      ((nbytes >= (sword_t)GENCGC_PAGE_BYTES) ?
                                       SINGLE_OBJECT_FLAG : 0) | page_type,
                                      gc_alloc_generation),
    et_find_freeish_page);

    /* Set up the alloc_region. */
    alloc_region->start_addr = page_address(first_page) + page_bytes_used(first_page);
    alloc_region->free_pointer = alloc_region->start_addr;
    alloc_region->end_addr = page_address(last_page+1);
    gc_assert(find_page_index(alloc_region->start_addr) == first_page);

    /* Set up the pages. */

    /* The first page may have already been in use. */
    /* If so, just assert that it's consistent, otherwise, set it up. */
    if (page_words_used(first_page)) {
        gc_assert(page_table[first_page].type == page_type);
        gc_assert(page_table[first_page].gen == gc_alloc_generation);
    } else {
        page_table[first_page].gen = gc_alloc_generation;
    }
    set_page_type(page_table[first_page], OPEN_REGION_PAGE_FLAG | page_type);

    page_index_t i;
    for (i = first_page+1; i <= last_page; i++) {
        set_page_type(page_table[i], OPEN_REGION_PAGE_FLAG | page_type);
        page_table[i].gen = gc_alloc_generation;
        set_page_scan_start_offset(i,
            addr_diff(page_address(i), alloc_region->start_addr));
    }
    if (unlock) {
        int __attribute__((unused)) ret = mutex_release(&free_pages_lock);
        gc_assert(ret);
    }

    if (page_words_used(first_page)) ++first_page;
    if (first_page <= last_page)
        INSTRUMENTING(prepare_pages(1, first_page, last_page, page_type, gc_alloc_generation),
                      et_bzeroing);

    return alloc_region->free_pointer;
}

/* The new_object structure holds the page, byte offset, and size of
 * new regions of objects. Each new area is placed in the array of
 * these structures pointer to by new_areas. new_areas_index holds the
 * offset into new_areas.
 *
 * If new_area overflows NUM_NEW_AREAS then it stops adding them. The
 * later code must detect this and handle it, probably by doing a full
 * scavenge of a generation. */
#define NUM_NEW_AREAS 512

/* 'record_new_regions_below' is the page number (strictly) below which
 * allocations must be tracked. Choosing the boundary cases with care allows
 * for all the required modes of operation without an additional control flag:
 * (1) When allocating from Lisp code, we need not record regions into areas.
 *     In this case 'record_new_regions_below' is 0,
 *     because no page index is less than that value.
 * (2) When performing a full scavenge of newspace, we record regions below the
 *     highest scavenged page thus far. Pages ahead of (at a higher index than)
 *     the pointer which walks all pages can be ignored, because those pages
 *     will be scavenged in the future regardless of where allocations occur.
 * (3) When iteratively scavenging newspace, all regions are tracked in areas,
 *     so this variable is set to 1+page_table_pages,
 *     because every page index is less than that sentinel value.
 */
static page_index_t record_new_regions_below;
struct new_area {
    page_index_t page;
    size_t offset;
    size_t size;
};
static struct new_area *new_areas;
static int new_areas_index;
int new_areas_index_hwm; // high water mark

/* Add a new area to new_areas. */
static void
add_new_area(page_index_t first_page, size_t offset, size_t size)
{
    if (!(first_page < record_new_regions_below))
        return;

    /* Ignore if full. */
    // Technically overflow occurs at 1+ this number, but it's not worth
    // losing sleep (or splitting hairs) over one potentially wasted array cell.
    // i.e. overflow did not necessarily happen if we needed _exactly_ this
    // many areas. But who cares? The limit should not be approached at all.
    if (new_areas_index >= NUM_NEW_AREAS)
        return;

    size_t new_area_start = npage_bytes(first_page) + offset;
    int i, c;
    if (GC_LOGGING) {
        char* base = page_address(first_page) + offset;
        fprintf(gc_activitylog(), "enqueue rescan [%p:%p]\n", base, base+size);
    }
    /* Search backwards for a prior area that this follows from. If
       found this will save adding a new area. */
    for (i = new_areas_index-1, c = 0; (i >= 0) && (c < 8); i--, c++) {
        size_t area_end =
            npage_bytes(new_areas[i].page) + new_areas[i].offset + new_areas[i].size;
        if (new_area_start == area_end) {
            new_areas[i].size += size;
            return;
        }
    }

    new_areas[new_areas_index].page = first_page;
    new_areas[new_areas_index].offset = offset;
    new_areas[new_areas_index].size = size;
    new_areas_index++;
}

/* Update the PTEs for the alloc_region. The region may be added to
 * the new_areas.
 *
 * When done the alloc_region is set up so that the next quick alloc
 * will fail safely and thus a new region will be allocated. Further
 * it is safe to try to re-update the page table of this reset
 * alloc_region.
 *
 * This is the internal implementation of ensure_region_closed(),
 * and not to be invoked as the interface to closing a region.
 *
 * Note that in no case will closing a region alter the need_to_zero bit
 * on any page in the region. It is legal to set that bit as late as possible,
 * because we only have to know just-in-time - when changing the page
 * (at some point later) from FREE to non-free - whether to zeroize it.
 * Therefore, we can set the need_to_zero bit only when there is otherwise
 * no way to detect that it ever held nonzero data, namely immediately
 * before doing reset_page_flags() or setting the words_used to 0.
 * Reflecting the words_used into that bit each time we update words_used
 * from a region's free pointer would be redundant (newspace scavenging
 * can open/close/open/close a region several times on the same page).
 */
void
gc_close_region(struct alloc_region *alloc_region, int page_type)
{
    page_index_t first_page = find_page_index(alloc_region->start_addr);
    page_index_t next_page = first_page+1;
    char *page_base = page_address(first_page);
    char *free_pointer = alloc_region->free_pointer;

    // page_bytes_used() can be done without holding a lock. Nothing else
    // affects the usage on the first page of a region owned by this thread.
    page_bytes_t orig_first_page_bytes_used = page_bytes_used(first_page);
    gc_assert(alloc_region->start_addr == page_base + orig_first_page_bytes_used);

    // Mark the region as closed on its first page.
    page_table[first_page].type &= ~(OPEN_REGION_PAGE_FLAG);

    if (free_pointer != alloc_region->start_addr) {
        /* some bytes were allocated in the region */

        /* All the pages used need to be updated */

        /* Update the first page. */
        if (!orig_first_page_bytes_used)
            gc_assert(page_starts_contiguous_block_p(first_page));

        gc_assert(page_table[first_page].type == page_type);
        gc_assert(page_table[first_page].gen == gc_alloc_generation);

        /* Calculate the number of bytes used in this page. This is not
         * always the number of new bytes, unless it was free. */
        os_vm_size_t bytes_used = addr_diff(free_pointer, page_base);
        bool more;
        if ((more = (bytes_used > GENCGC_PAGE_BYTES)))
            bytes_used = GENCGC_PAGE_BYTES;
        set_page_bytes_used(first_page, bytes_used);

        /* 'region_size' will be the sum of new bytes consumed by the region,
         * EXCLUDING any part of the first page already in use,
         * and any unused part of the final used page */
        os_vm_size_t region_size = bytes_used - orig_first_page_bytes_used;

        /* All the rest of the pages should be accounted for. */
        while (more) {
            gc_assert(page_table[next_page].type ==
                      (OPEN_REGION_PAGE_FLAG | page_type));
            page_table[next_page].type ^= OPEN_REGION_PAGE_FLAG;
            gc_assert(page_words_used(next_page) == 0);
            gc_assert(page_table[next_page].gen == gc_alloc_generation);
            page_base += GENCGC_PAGE_BYTES;
            gc_assert(page_scan_start_offset(next_page) ==
                      addr_diff(page_base, alloc_region->start_addr));

            /* Calculate the number of bytes used in this page. */
            bytes_used = addr_diff(free_pointer, page_base);
            if ((more = (bytes_used > GENCGC_PAGE_BYTES)))
                bytes_used = GENCGC_PAGE_BYTES;
            set_page_bytes_used(next_page, bytes_used);
            region_size += bytes_used;

            next_page++;
        }

        // Now 'next_page' is 1 page beyond those fully accounted for.
        gc_assert(addr_diff(free_pointer, alloc_region->start_addr) == region_size);
        // Update the global totals
        bytes_allocated += region_size;
        generations[gc_alloc_generation].bytes_allocated += region_size;

        /* Set the alloc restart page to the last page of the region. */
        set_alloc_start_page(page_type, next_page-1);

        /* Add the region to the new_areas if requested. */
        if (boxed_type_p(page_type))
            add_new_area(first_page, orig_first_page_bytes_used, region_size);

    } else if (!orig_first_page_bytes_used) {
        /* The first page is completely unused. Unallocate it */
        reset_page_flags(first_page);
    }

    /* Unallocate any unused pages. */
    page_index_t region_last_page = find_page_index((char*)alloc_region->end_addr-1);
    while (next_page <= region_last_page) {
        gc_assert(page_words_used(next_page) == 0);
        reset_page_flags(next_page);
        next_page++;
    }
    gc_set_region_empty(alloc_region);
}

/* Allocate a possibly large object. */
void *gc_alloc_large(sword_t nbytes, int page_type)
{
    page_index_t first_page, last_page;
    // Large BOXED would serve no purpose beyond MIXED, and "small large" is illogical.
    if (page_type == PAGE_TYPE_BOXED || page_type == PAGE_TYPE_SMALL_MIXED)
        page_type = PAGE_TYPE_MIXED;

    int locked = !gc_active_p;
    if (locked) {
        int __attribute__((unused)) ret = mutex_acquire(&free_pages_lock);
        gc_assert(ret);
    }

    first_page = max_alloc_start_page;
    INSTRUMENTING(
    last_page = gc_find_freeish_pages(&first_page, nbytes,
                                      SINGLE_OBJECT_FLAG | page_type,
                                      gc_alloc_generation),
    et_find_freeish_page);
    // No need to check whether last_page > old max; it's gotta be.
    max_alloc_start_page = last_page;

    /* Set up the pages. */
    page_index_t page;
    for (page = first_page; page <= last_page; ++page) {
        /* Large objects don't share pages with other objects. */
        gc_assert(page_words_used(page) == 0);
        set_page_type(page_table[page], SINGLE_OBJECT_FLAG | page_type);
        page_table[page].gen = gc_alloc_generation;
    }

#ifdef LISP_FEATURE_WIN32
    // don't incur access violations
    os_commit_memory(page_address(first_page), npage_bytes(1+last_page-first_page));
#endif

    // Store a filler so that a linear heap walk does not try to examine
    // these pages cons-by-cons (or whatever they happen to look like).
    // A concurrent walk would probably crash anyway, and most certainly
    // will if it uses the page tables while this allocation is partway
    // through assigning bytes_used per page.
    // The fix for that is clear: MAP-OBJECTS-IN-RANGE should acquire
    // free_pages_lock when computing the extent of a contiguous block.
    // Anyway it's best if the new page resembles a valid object ASAP.
    uword_t nwords = nbytes >> WORD_SHIFT;
    lispobj* addr = (lispobj*)page_address(first_page);

    /* The test of whether to use THREAD_JIT_WP here is not based on 'page_type'
     * but rather how the page _is_mapped_now_. Conservatively do the call
     * because thning about all 4 combinations of how-it-was-mapped x how-it-will-be-mapped,
     * here and down below is too confusing */
    if (locked) { THREAD_JIT_WP(0); }
    *addr = make_filler_header(nwords);

    os_vm_size_t scan_start_offset = 0;
    for (page = first_page; page < last_page; ++page) {
        set_page_scan_start_offset(page, scan_start_offset);
        set_page_bytes_used(page, GENCGC_PAGE_BYTES);
        scan_start_offset += GENCGC_PAGE_BYTES;
    }
    page_bytes_t final_bytes_used = nbytes - scan_start_offset;
    gc_dcheck((nbytes % GENCGC_PAGE_BYTES ? nbytes % GENCGC_PAGE_BYTES
               : GENCGC_PAGE_BYTES) == final_bytes_used);
    set_page_scan_start_offset(last_page, scan_start_offset);
    set_page_bytes_used(last_page, final_bytes_used);
    bytes_allocated += nbytes;
    generations[gc_alloc_generation].bytes_allocated += nbytes;

    if (locked) {
        int __attribute__((unused)) ret = mutex_release(&free_pages_lock);
        gc_assert(ret);
    }
    INSTRUMENTING(prepare_pages(0, first_page, last_page, page_type, gc_alloc_generation),
                  et_bzeroing);

    /* Add the region to the new_areas if requested. */
    if (boxed_type_p(page_type)) add_new_area(first_page, 0, nbytes);

    // page may have not needed zeroing, but first word was stored,
    // turning the putative object temporarily into a page filler object.
    // Now turn it back into free space.
    *addr = 0;
    if (locked) { THREAD_JIT_WP(1); }

    return addr;
}

/* Search for at least nbytes of space, possibly picking up any
 * remaining space on the tail of a page that was not fully used.
 *
 * The found space is guaranteed to be page-aligned if the SINGLE_OBJECT_FLAG
 * bit is set in page_type.
 */
page_index_t
gc_find_freeish_pages(page_index_t *restart_page_ptr, sword_t nbytes,
                      int page_type, generation_index_t gen)
{
    page_index_t most_bytes_found_from = 0, most_bytes_found_to = 0;
    page_index_t first_page, last_page, restart_page = *restart_page_ptr;
    sword_t nbytes_goal = nbytes;
    sword_t bytes_found = 0;
    sword_t most_bytes_found = 0;
    int multi_object = !(page_type & SINGLE_OBJECT_FLAG);
    /* FIXME: assert(free_pages_lock is held); */

    if (multi_object) {
        if (nbytes_goal < (sword_t)gencgc_alloc_granularity)
            nbytes_goal = gencgc_alloc_granularity;
#if !defined(LISP_FEATURE_64_BIT)
        // Increase the region size to avoid excessive fragmentation
        if (page_type == PAGE_TYPE_CODE && nbytes_goal < 65536)
            nbytes_goal = 65536;
#endif
    }
    page_type &= ~SINGLE_OBJECT_FLAG;

    gc_assert(nbytes>=0);
    first_page = restart_page;
    while (first_page < page_table_pages) {
        bytes_found = 0;
        if (page_free_p(first_page)) {
            gc_dcheck(!page_words_used(first_page));
            bytes_found = GENCGC_PAGE_BYTES;
        } else if (multi_object &&
                   // Never return a range starting with a 100% full page
                   (bytes_found = GENCGC_PAGE_BYTES
                    - page_bytes_used(first_page)) > 0 &&
                   // "extensible" means all PTE fields are compatible
                   page_extensible_p(first_page, gen, page_type)) {
            // TODO: Now that BOXED, CONS, and SMALL_MIXED pages exist, investigate
            // whether the bias against returning partial pages is still useful.
            // It probably isn't.
            if (bytes_found < nbytes && !is_code(page_type)) {
                if (bytes_found > most_bytes_found)
                    most_bytes_found = bytes_found;
                first_page++;
                continue;
            }
        } else {
            first_page++;
            continue;
        }
#ifndef LISP_FEATURE_SOFT_CARD_MARKS
        gc_dcheck(!PAGE_WRITEPROTECTED_P(first_page));
#endif
        /* page_free_p() can legally be used at index 'page_table_pages'
         * because the array dimension is 1+page_table_pages */
        for (last_page = first_page+1;
             bytes_found < nbytes_goal &&
               page_free_p(last_page) && last_page < page_table_pages;
             last_page++) {
            /* page_free_p() implies 0 bytes used, thus GENCGC_PAGE_BYTES available.
             * It also implies !write_protected, and if the OS's conception were
             * otherwise, lossage would routinely occur in the fault handler) */
            bytes_found += GENCGC_PAGE_BYTES;
            gc_dcheck(!page_words_used(last_page));
#ifndef LISP_FEATURE_SOFT_CARD_MARKS
            gc_dcheck(!PAGE_WRITEPROTECTED_P(last_page));
#endif
        }

        if (bytes_found > most_bytes_found) {
            most_bytes_found = bytes_found;
            most_bytes_found_from = first_page;
            most_bytes_found_to = last_page;
        }
        if (bytes_found >= nbytes_goal)
            break;

        first_page = last_page;
    }

    bytes_found = most_bytes_found;
    restart_page = first_page + 1;

    /* Check for a failure */
    if (bytes_found < nbytes) {
        gc_assert(restart_page >= page_table_pages);
        gc_heap_exhausted_error_or_lose(most_bytes_found, nbytes);
    }

    gc_assert(most_bytes_found_to);
    // most_bytes_found_to is the upper exclusive bound on the found range.
    // next_free_page is the high water mark of most_bytes_found_to.
    if (most_bytes_found_to > next_free_page) next_free_page = most_bytes_found_to;
    *restart_page_ptr = most_bytes_found_from;
    return most_bytes_found_to-1;
}

/* Allocate bytes.  The fast path of gc_general_alloc() calls this
 * when it can't fit in the open region.
 * This entry point is only for use within the GC itself.
 * The Lisp region overflow handler either directly calls gc_alloc_large
 * or closes and opens a region if the allocation is small.
 *
 * There are two general approaches to handling SMALL_MIXED allocations:
 *  1. always open the alloc region as whole page, but hack up gc_general_alloc
 *     to avoid spanning cards in the fast case.
 *  2. open the region as one card, and alter the slow case to try consuming
 *     the next card on the same page if it can.
 * Choice 2 is better because choice 1 makes an extra test for page_type
 * in each call to gc_general_alloc.
 */
static void *new_region(struct alloc_region* region, sword_t nbytes, int page_type)
{
    ensure_region_closed(region, page_type);
    void* new_obj = gc_alloc_new_region(nbytes, page_type, region, 0);
    region->free_pointer = (char*)new_obj + nbytes;
    gc_assert(region->free_pointer <= region->end_addr);
    return new_obj;
}
void *collector_alloc_fallback(struct alloc_region* region, sword_t nbytes, int page_type)
{
    /* If this is a normal GC - as opposed to "final" GC just prior to saving
     * a core, then we should never copy a large object (not that that's the best
     * strategy always, because it entirely precludes defragmenting those objects).
     * But unfortunately we can't assert that only small objects are seen here,
     * because genesis does not use large-object pages. So cold-init could fail,
     * depending on whether objects in the cold core are sufficiently large that
     * they ought to have gone on large object pages if they could have. */
    if (nbytes >= LARGE_OBJECT_SIZE) return gc_alloc_large(nbytes, page_type);

    if (page_type != PAGE_TYPE_SMALL_MIXED) return new_region(region, nbytes, page_type);

#define SMALL_MIXED_NWORDS_LIMIT 10
#define SMALL_MIXED_NBYTES_LIMIT (SMALL_MIXED_NWORDS_LIMIT * N_WORD_BYTES)
    /* We're want to try to place mix raw/tagged slot objects such that they don't span cards.
     * There are essentially three cases:
     * (1) If the object size exceeds one card, we go straight to the MIXED region.
     * (2) If the object size is <= SMALL_MIXED_NWORDS_LIMIT, we will _always_ place it
     *     on one card. To do that, just align up to the next card or whole page
     *     if it would span cards based on the current free_pointer.
     *     This wastes at most SMALL_MIXED_NWORDS_LIMIT - 2 words, per card.
     * (3) If the object is larger than that, we will waste at most the threshold number
     *     of words, but if it would waste more, we use the MIXED region.
     *     So this case opportunistically uses the subcard region if it can */
    if ((int)nbytes > (int)GENCGC_CARD_BYTES)
        return new_region(mixed_region, nbytes, PAGE_TYPE_MIXED);
    if (!region->start_addr) { // region is not in an open state
        /* Don't try to request too much, because that might return a brand new page,
         * when we could have kept going on the same page with small objects.
         * Better to put the threshold-exceeding object in the MIXED region */
        int request = nbytes > SMALL_MIXED_NBYTES_LIMIT ? SMALL_MIXED_NBYTES_LIMIT : nbytes;
        void* new_obj = gc_alloc_new_region(request, page_type, region, 0);
        char* new_freeptr = (char*)new_obj + nbytes;
        /* alloc_new_region() ensures that the page it returns has at least 'nbytes' more
         * but does *not* ensure that there is that much space below the end of the region.
         * This is a little weird, but doing things this way confines the filler insertion
         * logic to just here instead of also being also in alloc_new_region.
         * You could try to put that logic only in alloc_new_region, but doing that has
         * its own down-side: to call alloc_new_region, you first have to close the region,
         * which entails extra work in sync'ing the PTE when we don't really need to */
        if (new_freeptr <= (char*)region->end_addr) {
            region->free_pointer = new_freeptr;
            return new_obj;
        }
    }
    __attribute__((unused)) page_index_t fpi = find_page_index(region->start_addr);
    __attribute__((unused)) page_index_t lpi = find_page_index((char*)region->end_addr-1);
    gc_assert(fpi == lpi);
    gc_assert(page_table[fpi].type & OPEN_REGION_PAGE_FLAG);
    // Region is open, but card at free_pointer lacks sufficient space.
    // See if there's another card on the same page.
    char* page_base = PTR_ALIGN_DOWN(region->start_addr, GENCGC_PAGE_BYTES);
    char* next_card = PTR_ALIGN_UP(region->free_pointer, GENCGC_CARD_BYTES);
    if (next_card < page_base + GENCGC_PAGE_BYTES) {
        int fill_nbytes = next_card - (char*)region->free_pointer;
        if (fill_nbytes) {
            int fill_nwords = fill_nbytes >> WORD_SHIFT;
            /* Object size might strictly exceed SMALL_MIXED_NWORDS_LIMIT.
             * Never insert that much filler */
            if (fill_nwords >= SMALL_MIXED_NWORDS_LIMIT)
                return new_region(mixed_region, nbytes, PAGE_TYPE_MIXED);
            *(lispobj*)region->free_pointer = make_filler_header(fill_nwords);
        }
        region->free_pointer = next_card;
        region->end_addr = next_card + GENCGC_CARD_BYTES;
        void* new_obj = next_card;
        region->free_pointer = (char*)new_obj + nbytes;
        gc_assert(region->free_pointer <= region->end_addr);
        return new_obj;
    }
    /* Now be careful not to waste too much at the end of the page in the following situation:
     * page has 20 words more, but we need 24 words. Use the MIXED region because the subcard
     * region has room for anywhere from 2 to 10 more objects depending on how small */
    if (nbytes > SMALL_MIXED_NBYTES_LIMIT) page_type = PAGE_TYPE_MIXED, region = mixed_region;
    return new_region(region, nbytes, page_type);
}


/* Free any trailing pages of the object starting at 'first_page'
 * that are currently unused due to object shrinkage.
 * Possibly assign different 'gen' and 'allocated' values.
 *
 * maybe_adjust_large_object() specifies 'from_space' for 'new_gen'
 * and copy_potential_large_object() specifies 'new_space'
 *
 * Note that creating a large object might not affect the 'need_to_zero'
 * flag on any of pages consumed (it would if the page type demands prezeroing
 * and wasn't zero), but freeing the unused pages of a shrunken object DOES
 * set the need_to_zero bit unconditionally.  We have to suppose that the object
 * constructor wrote bytes on each of its pages, and we don't know whether the tail
 * of the object got zeroed versus bashed into FILLER_WIDETAG + random bits.
 */

static uword_t adjust_obj_ptes(page_index_t first_page,
                               sword_t nwords,
                               generation_index_t new_gen,
                               int new_allocated)
{
    int old_allocated = page_table[first_page].type;
    sword_t remaining_bytes = nwords * N_WORD_BYTES;
    page_index_t n_full_pages = nwords / (GENCGC_PAGE_BYTES / N_WORD_BYTES);
    page_bytes_t excess = remaining_bytes & (GENCGC_PAGE_BYTES - 1);
    // page number of ending page of this object at its new size
    page_index_t final_page = first_page + (n_full_pages - 1) + (excess != 0);

    /* Decide whether there is anything to do by checking whether:
     *  (1) the page at n_full_pages-1 beyond the first is fully used,
     *  (2) the next fractional page, if any, has correct usage, and
     *  (3) the page after that is not part of this object.
     * If all those conditions are met, this is the easy case,
     * though we may still have to change the generation and/or page type. */
    if ((!n_full_pages || page_words_used(first_page+(n_full_pages-1))
                          == GENCGC_PAGE_WORDS) &&
        (!excess || page_bytes_used(final_page) == excess) &&
        page_starts_contiguous_block_p(1+final_page)) {
        /* The 'if' below has an 'else' which subsumes the 'then' in generality.
         * Why? Because usually we only need perform one assignment.
         * Moreover, after a further change which makes us not look at the 'gen'
         * of the *interior* of a page-spanning object, then the fast case
         * reduces to "page_table[first_page].gen = new_gen". And we're done.
         * At present, some logic assumes that every page's gen was updated */
        page_index_t page;
        if (old_allocated == new_allocated) { // Almost always true,
            // except when bignums or specialized arrays change from thread-local
            // (boxed) allocation to unboxed, for downstream efficiency.
            for (page = first_page; page <= final_page; ++page)
                page_table[page].gen = new_gen;
        } else {
            for (page = first_page; page <= final_page; ++page) {
                set_page_type(page_table[page], new_allocated);
                page_table[page].gen = new_gen;
            }
        }
        return 0;
    }

    /* The assignments to the page table here affect only one object
     * since its pages can't be shared with other objects */
#define CHECK_AND_SET_PTE_FIELDS() \
        gc_assert(page_table[page].type == old_allocated); \
        gc_assert(page_table[page].gen == from_space); \
        gc_assert(page_scan_start_offset(page) == npage_bytes(page-first_page)); \
        page_table[page].gen = new_gen; \
        set_page_type(page_table[page], new_allocated)

    gc_assert(page_starts_contiguous_block_p(first_page));
    page_index_t page = first_page;
    while (remaining_bytes > (sword_t)GENCGC_PAGE_BYTES) {
        gc_assert(page_words_used(page) == GENCGC_PAGE_WORDS);
        CHECK_AND_SET_PTE_FIELDS();
        remaining_bytes -= GENCGC_PAGE_BYTES;
        page++;
    }

    /* Now at most one page of data in use by the object remains,
     * but there may be more unused pages beyond which will be freed. */

    /* This page must have at least as many bytes in use as expected */
    gc_assert((sword_t)page_bytes_used(page) >= remaining_bytes);
    CHECK_AND_SET_PTE_FIELDS();

    /* Adjust the bytes_used. */
    page_bytes_t prev_bytes_used = page_bytes_used(page);
    set_page_bytes_used(page, remaining_bytes);

    uword_t bytes_freed = prev_bytes_used - remaining_bytes;

    /* Free unused pages that were originally allocated to this object. */
    page++;
    while (prev_bytes_used == GENCGC_PAGE_BYTES &&
           page_table[page].gen == from_space &&
           page_table[page].type == old_allocated &&
           page_scan_start_offset(page) == npage_bytes(page - first_page)) {
        // These pages are part of oldspace, which was un-write-protected.
        gc_assert(page_cards_all_marked_nonsticky(page));

        /* Zeroing must have been done before shrinking the object.
         * (It is strictly necessary for correctness with objects other
         * than simple-vector, but pragmatically it reduces accidental
         * conservativism when done for simple-vectors as well) */
#ifdef DEBUG
        { lispobj* words = (lispobj*)page_address(page);
          int i;
          for(i=0; i<(int)(GENCGC_PAGE_BYTES/N_WORD_BYTES); ++i)
              if (words[i])
                lose("non-zeroed trailer of shrunken object @ %p",
                     page_address(first_page));
        }
#endif
        /* It checks out OK, free the page. */
        prev_bytes_used = page_bytes_used(page);
        set_page_need_to_zero(page, 1);
        set_page_bytes_used(page, 0);
        reset_page_flags(page);
        bytes_freed += prev_bytes_used;
        page++;
    }

    // If this freed nothing, it ought to have gone through the fast path.
    gc_assert(bytes_freed != 0);
    return bytes_freed;
}

/* "Copy" a large object. If the object is on large object pages,
 * and satisifies the condition to remain where it is,
 * it is simply promoted, else it is copied.
 * To stay on large-object pages, the object must either be at least
 * LARGE_OBJECT_SIZE, or must waste fewer than about 1% of the space
 * on its allocated pages. Using 32k pages as a reference point:
 *   3 pages - ok if size >= 97552
 *   2 pages - ...   size >= 65040
 *   1 page  - ...   size >= 32528
 *
 * Bignums and vectors may have shrunk. If the object is not copied,
 * the slack needs to be reclaimed, and the page_tables corrected.
 *
 * Code objects can't shrink, but it's not worth adding an extra test
 * for large code just to avoid the loop that performs adjustment, so
 * go through the adjustment motions even though nothing happens.
 *
 */
lispobj
copy_potential_large_object(lispobj object, sword_t nwords,
                           struct alloc_region* region, int page_type)
{
    page_index_t first_page;

    CHECK_COPY_PRECONDITIONS(object, nwords);

    /* Check whether it's a large object. */
    first_page = find_page_index((void *)object);
    gc_dcheck(first_page >= 0);

    os_vm_size_t nbytes = nwords * N_WORD_BYTES;
    os_vm_size_t rounded = ALIGN_UP(nbytes, GENCGC_PAGE_BYTES);
    if (page_single_obj_p(first_page) &&
        (nbytes >= LARGE_OBJECT_SIZE || (rounded - nbytes < rounded / 128))) {

        // Large BOXED would serve no purpose beyond MIXED, and "small large" is illogical.
        if (page_type == PAGE_TYPE_BOXED || page_type == PAGE_TYPE_SMALL_MIXED)
            page_type = PAGE_TYPE_MIXED;
        os_vm_size_t bytes_freed =
          adjust_obj_ptes(first_page, nwords, new_space,
                          SINGLE_OBJECT_FLAG | page_type);

        generations[from_space].bytes_allocated -= (bytes_freed + nbytes);
        generations[new_space].bytes_allocated += nbytes;
        bytes_allocated -= bytes_freed;

        /* Add the region to the new_areas if requested. */
        gc_in_situ_live_nwords += nbytes>>WORD_SHIFT;
        if (boxed_type_p(page_type)) add_new_area(first_page, 0, nbytes);

        return object;
    }
    return gc_copy_object(object, nwords, region, page_type);
}

/* to copy unboxed objects */
lispobj
copy_unboxed_object(lispobj object, sword_t nwords)
{
    return gc_copy_object(object, nwords, unboxed_region, PAGE_TYPE_UNBOXED);
}

/* This WILL NOT reliably work for objects in a currently open allocation region,
 * because page_words_used() is not sync'ed to the free pointer until closing.
 * However it should work reliably for codeblobs, because if you can hold
 * a reference to the codeblob, then either you'll find it in the generation 0
 * tree, or else can linearly scan for it in an older generation */
static lispobj dynspace_codeblob_tree_snapshot; // valid only during GC
lispobj *search_dynamic_space(void *pointer)
{
    page_index_t page_index = find_page_index(pointer);

    /* The address may be invalid, so do some checks.
     * page_index -1 is legal, and page_free_p returns true in that case. */
    if (page_free_p(page_index)) return NULL;

    int type = page_table[page_index].type & PAGE_TYPE_MASK;
    // Generation 0 code is in the tree usually - it isn't for objects
    // in generation 0 following a non-promotion cycle.
    if (type == PAGE_TYPE_CODE && page_table[page_index].gen == 0) {
        lispobj tree = dynspace_codeblob_tree_snapshot ? dynspace_codeblob_tree_snapshot :
                       SYMBOL(DYNSPACE_CODEBLOB_TREE)->value;
        lispobj node = brothertree_find_lesseql((uword_t)pointer, tree);
        if (node != NIL) {
            lispobj *found = (lispobj*)((struct binary_node*)INSTANCE(node))->uw_key;
            int widetag = widetag_of(found);
            if (widetag != CODE_HEADER_WIDETAG && widetag != FUNCALLABLE_INSTANCE_WIDETAG)
                lose("header not OK for code page: @ %p = %"OBJ_FMTX"\n", found, *found);
            sword_t nwords = object_size(found);
            lispobj *upper_bound = found + nwords;
            if (pointer < (void*)upper_bound) return found;
        }
    }
    char* limit = page_address(page_index) +  page_bytes_used(page_index);
    if ((char*)pointer > limit) return NULL;
    if (type == PAGE_TYPE_CONS) {
        return (lispobj*)ALIGN_DOWN((uword_t)pointer, 2*N_WORD_BYTES);
    }
    lispobj *start;
    if (type == PAGE_TYPE_SMALL_MIXED) { // find the nearest card boundary below 'pointer'
        start = (lispobj*)ALIGN_DOWN((uword_t)pointer, GENCGC_CARD_BYTES);
    } else {
        start = (lispobj *)page_scan_start(page_index);
    }
    return gc_search_space(start, pointer);
}

/* Return true if and only if everything on the specified page is NOT subject
 * to evacuation, i.e. either the page is not in 'from_space', or is entirely
 * pinned.  "Entirely pinned" is predicated on being marked as pinned,
 * and satisfying one of two additional criteria:
 *   1. the page is a single-object page
 *   2. the page contains only code, and all code objects are pinned.
 *
 * A non-large-object page that is marked "pinned" does not suffice
 * to be considered entirely pinned if it contains other than code.
 */
int pin_all_dynamic_space_code;
static inline int immune_set_memberp(page_index_t page)
{
    return (page_table[page].gen != from_space)
        || (gc_page_pins[page] &&
            (page_single_obj_p(page) ||
             (is_code(page_table[page].type) && pin_all_dynamic_space_code)));
}

#ifndef LISP_FEATURE_WEAK_VECTOR_READBARRIER
// Only a bignum, code blob, or vector could be on a single-object page.
#define potential_largeobj_p(w) \
  (w==BIGNUM_WIDETAG || w==CODE_HEADER_WIDETAG || \
   (w>=SIMPLE_VECTOR_WIDETAG && w < COMPLEX_BASE_STRING_WIDETAG))
#else
// also include WEAK_POINTER_WIDETAG because it could be vector-like
#define potential_largeobj_p(w) \
  (w==BIGNUM_WIDETAG || w==CODE_HEADER_WIDETAG || w==WEAK_POINTER_WIDETAG || \
   (w>=SIMPLE_VECTOR_WIDETAG && w < COMPLEX_BASE_STRING_WIDETAG))
#endif

static inline __attribute__((unused))
int lowtag_ok_for_page_type(__attribute__((unused)) lispobj ptr,
                                          __attribute__((unused)) int page_type) {
    // If the young generation goes to mixed-region, this filter is not valid
#ifdef LISP_FEATURE_USE_CONS_REGION
    // This doesn't currently decide on acceptability for code/non-code
    if (lowtag_of(ptr) == LIST_POINTER_LOWTAG) {
        if (page_type != PAGE_TYPE_CONS) return 0;
    } else {
        if (page_type == PAGE_TYPE_CONS) return 0;
    }
#endif
    return 1;
}

/*
 * We offer many variations on root scanning:
 * 1. X86: all refs from them stack are ambiguous, and pin their referent
 *    if there is one. All refs from registers (interrupt contexts)
 *    are ambiguous, and similarly pin their referent if there is one.
 *    Interior pointers are disallowed to anything except code.
 *    (FIXME: the PC to the jump instruction into an immobile fdefn
 *    or self-contained trampoline GF - what does it do wrt pinning???)
 *
 * 2. ARM64: interior code pointers from the stack are ambiguous
 *    and pin their referent if there is one,
 *    Non-code references are unambiguous, and do NOT pin their referent.
 *    Only the call chain is scanned for code pointers.
 *    Interrupt context registers are unambiguous, and can get
 *    altered by GC.
 *
 * 3. PPC64: interior code pointers from the stack are ambiguous roots,
 *    and pin their referent if there is one.
 *    Non-code pointers are unambiguous, and do NOT pin
 *    their referent from the stack.
 *    Interrupt context registers are unambiguous and DO pin their referent.
 *    The entire control stack is scanned for code pointers, thus avoiding
 *    reliance on a correct backtrace. (I doubt the veracity of all claims
 *    to the backtrace chain being correct in the presence of interrupts)
 *
 * 4. All references from the stack are tagged, and precise, and none pin
 *    their referent.
 *    Interrupt contexts registers are unambiguous, and do not pin their referent.
 *    (pertains to any architecture not specifically mentione above)
 *
 * A single boolean value for GENCGC_IS_PRECISE is inadequate to express
 * the possibilities. Anything except case 1 is considered "precise".
 * Because of the variations, there are many other #ifdefs surrounding
 * the logic pertaining to stack and interrupt context scanning.
 * Anyway, the above is the theory, but in practice, we have to treat
 * some unambiguous pointers as ambiguous for lack of information
 * in conservative_root_p what the intent is.
 */
#define AMBIGUOUS_POINTER 1
#if !GENCGC_IS_PRECISE
// Return the starting address of the object containing 'addr'
// if and only if the object is one which would be evacuated from 'from_space'
// were it allowed to be either discarded as garbage or moved.
// 'addr_page_index' is the page containing 'addr' and must not be -1.
// Return 0 if there is no such object - that is, if addr is past the
// end of the used bytes, or its pages are not in 'from_space' etc.
static lispobj conservative_root_p(lispobj addr, page_index_t addr_page_index)
{
    /* quick check 1: Address is quite likely to have been invalid. */
    struct page* page = &page_table[addr_page_index];
    bool enforce_lowtag = !is_code(page->type);

    if ((addr & (GENCGC_PAGE_BYTES - 1)) >= page_bytes_used(addr_page_index) ||
        (!is_lisp_pointer(addr) && enforce_lowtag) ||
        (compacting_p() && immune_set_memberp(addr_page_index)))
        return 0;
    gc_assert(!(page->type & OPEN_REGION_PAGE_FLAG));

    /* If this page can hold only one object, the test is very simple.
     * Code pages allow random interior pointers, but only a correctly
     * tagged pointer to the boxed words. Tagged interior pointers to SIMPLE-FUNs
     * are just as good as any untagged instruction pointer. */
    if (page_single_obj_p(addr_page_index)) {
        lispobj* object_start = page_scan_start(addr_page_index);
        int widetag = widetag_of(object_start);
        if (instruction_ptr_p((char*)addr, object_start) ||
            (potential_largeobj_p(widetag) &&
             // Conveniently all potential largeobjs are OTHER_POINTER
             make_lispobj(object_start, OTHER_POINTER_LOWTAG) == addr))
            return make_lispobj(object_start, OTHER_POINTER_LOWTAG);
        return 0;
    }

    /* For pages of code:
     * - we can't enforce a particular lowtag on the pointer.
     * - we have to find the object base, because pinning a code object
     *   pins its embedded simple-funs and vice-versa.
     * I don't know what to think about pointing to filler objects.
     * It seems like a bad idea, but what if Lisp code does that?
     * Can it crash if we free the page? I'll assume we're fine
     * unless someone can show otherwise */
    if (is_code(page->type)) {
        lispobj* object_start = search_dynamic_space((void*)addr);
        /* This search must not fail. We've already verified that the
         * pointer is within range for its page. */
        gc_assert(object_start);
        switch (widetag_of(object_start)) {
        case CODE_HEADER_WIDETAG:
            /* If 'addr' points anywhere beyond the boxed words, it's valid
             * (i.e. allow it even if an incorrectly tagged pointer to a simple-fun header)
             * FIXME: Do we want to allow pointing at the untagged base address too?
             * It'll find a key in the codeblob tree, but why would Lisp have the
             * untagged pointer and expect it to be a strong reference? */
            if (instruction_ptr_p((void*)addr, object_start)
                || addr == make_lispobj(object_start, OTHER_POINTER_LOWTAG))
                return make_lispobj(object_start, OTHER_POINTER_LOWTAG);
            return 0;
#ifdef LISP_FEATURE_X86_64
        case FUNCALLABLE_INSTANCE_WIDETAG:
            // Allow any of these to pin a funcallable instance:
            //  - pointer to embedded machine instructions
            //  - untagged pointer to trampoline word
            //  - correctly tagged pointer
            if ((addr >= (uword_t)(object_start+2) && addr < (uword_t)(object_start+4))
                || addr == (lispobj)(object_start+1)
                || addr == make_lispobj(object_start, FUN_POINTER_LOWTAG))
                return make_lispobj(object_start, FUN_POINTER_LOWTAG);
            return 0;
#endif
        }
        return 0;
    }

    /* For non-code, the pointer's lowtag and widetag must correspond.
     * The putative object header can safely be read even if it turns out
     * that the pointer is not valid, because 'addr' was in bounds for the page.
     * Note that this can falsely pass if looking at the interior of an unboxed
     * array that masquerades as a Lisp object header by random chance. */
    if (widetag_of(native_pointer(addr)) != FILLER_WIDETAG
        && lowtag_ok_for_page_type(addr, page->type)
        && plausible_tag_p(addr)) return AMBIGUOUS_POINTER;

    // FIXME: I think there is a window of GC vulnerability regarding FINs
    // and FDEFNs containing executable bytes. In either case if the only pointer
    // to such an object is the program counter, the object could be considered
    // garbage because there is no _tagged_ pointer to it.
    // This is an almost impossible situation to arise, but seems worth some study.

    return 0;
}
#elif defined LISP_FEATURE_MIPS || defined LISP_FEATURE_PPC64
/* Consider interior pointers to code as roots.
 * But most other pointers are *unambiguous* conservative roots.
 * This is not "less conservative" per se, than the non-precise code,
 * because it's actually up to the user of this predicate to decide whehther
 * the control stack as a whole is scanned for objects to pin.
 * The so-called "precise" code should generally NOT scan the stack,
 * and not call this on stack words.
 * Anyway, this code isn't as performance-critical as the x86 variant,
 * so it's not worth trying to optimize out the search for the object */
static lispobj conservative_root_p(lispobj addr, page_index_t addr_page_index)
{
    struct page* page = &page_table[addr_page_index];

    // quick check: within from_space and within page usage
    if ((addr & (GENCGC_PAGE_BYTES - 1)) >= page_bytes_used(addr_page_index) ||
        (compacting_p() && immune_set_memberp(addr_page_index)))
        return 0;
    gc_assert(!(page->type & OPEN_REGION_PAGE_FLAG));

    /* Find the containing object, if any
     * This is slightly less quick than could be: if sticky_preserve_pointer() was
     * called on the contents of a boxed register, then we know that the value is
     * a properly tagged descriptor, and don't really need to "search" for an object.
     * (And in fact we should rule out fixnums up front)
     * Unfortunately sticky_preserve_pointer() does not inform conservative_root_p()
     * whether the pointer is known good. So we need a slightly different interface
     * to achieve that extra bit of efficiency */
    lispobj* object_start = search_dynamic_space((void*)addr);
    if (!object_start) return 0;

    if (is_code(page->type))
        return make_lispobj(object_start, OTHER_POINTER_LOWTAG);

    /* Take special care not to return fillers. A real-world example:
     * - a boxed register contains 0x528b4000
     * - the object formerly at 0x528b4000 is a filler
     * - compute_lispobj(0x528b4000) returns 0x528b4000 because LOWTAG_FOR_WIDETAG
     *   says that FILLER_WIDTAG has a 0 lowtag.
     *   compute_lispobj simply ORs in the 0 which gives back the original address
     *   and that of course satisfies the equality test. */

    // Correctly tagged pointer: ok
    if (addr == compute_lispobj(object_start)
        && widetag_of(object_start) != FILLER_WIDETAG)
        return addr;
    return 0;
}
#endif

/* Adjust large bignum and vector objects. This will adjust the
 * allocated region if the size has shrunk, and change boxed pages
 * into unboxed pages. The pages are not promoted here, and the
 * object is not added to the new_regions; this is really
 * only designed to be called from preserve_pointer(). Shouldn't fail
 * if this is missed, just may delay the moving of objects to unboxed
 * pages, and the freeing of pages. */
static void
maybe_adjust_large_object(lispobj* where, page_index_t first_page, sword_t nwords)
{
    int page_type;

    /* Check whether it's a vector or bignum object. */
    /* There is no difference between MIXED and BOXED for large objects,
     * because in any event we'll use the large simple-vector optimization
     * for root scavenging if applicable. */
    lispobj widetag = widetag_of(where);
    if (widetag == SIMPLE_VECTOR_WIDETAG)
        page_type = SINGLE_OBJECT_FLAG | PAGE_TYPE_MIXED;
#ifndef LISP_FEATURE_UBSAN
    else if (specialized_vector_widetag_p(widetag) || widetag == BIGNUM_WIDETAG)
        page_type = SINGLE_OBJECT_FLAG | PAGE_TYPE_UNBOXED;
#endif
    else
        return;

    os_vm_size_t bytes_freed =
      adjust_obj_ptes(first_page, nwords, from_space, page_type);
    generations[from_space].bytes_allocated -= bytes_freed;
    bytes_allocated -= bytes_freed;
}

/* After scavenging of the roots is done, we go back to the pinned objects
 * and look within them for pointers. Additionally we delete any keys
 * from the list of pins that were not legal object addresses,
 * but passed through all the filters in conservative_root_p.
 */
#define SMALL_MAX_PINS 200
static uword_t small_pins_vector[SMALL_MAX_PINS];

uword_t gc_pinned_nwords;
static void refine_ambiguous_roots()
{
    void gc_heapsort_uwords(uword_t*, int);

    int pre_deletion_count = pinned_objects.count;
    gc_pin_count = pre_deletion_count;
    if (pre_deletion_count == 0) return;

    /* We need a place to sort the keys of pinned_objects. If the key count is small,
     * use the small_pins vector; otherwise grab some memory via mmap */
    lispobj* workspace;
    if (pre_deletion_count < SMALL_MAX_PINS) { // leave room for sentinel at end
        workspace = small_pins_vector;
    } else {
        pins_alloc_size = ALIGN_UP((pre_deletion_count+1)*N_WORD_BYTES, BACKEND_PAGE_BYTES);
        workspace = (lispobj*)os_allocate(pins_alloc_size);
        gc_assert(workspace);
    }
    gc_filtered_pins = workspace; // needed for obliterate_nonpinned_words
    lispobj key;
    int count = 0, index;
    for_each_hopscotch_key(index, key, pinned_objects) {
        gc_assert(is_lisp_pointer(key));
        // Preserve only the object base addresses, including any "false" pointers.
        if (listp(key) || widetag_of(native_pointer(key)) != SIMPLE_FUN_WIDETAG)
            workspace[count++] = key;
    }
    gc_heapsort_uwords(workspace, count);
    /* Algorithm:
     * for each group of keys with the same page_scan_start
     *   - scan the heap at the indicated start address
     *   - "intersect" the list of objects visited with the list of
     *     ambiguous roots (this is easy because the keys are sorted)
     *   - change any missed key to 0 as we go
     */
    lispobj *where = 0, // as is tradition
            *previous_scan_start = 0;
    int removed = 0;
    for (index = 0 ; index < count ; ++index) {
        lispobj* key = native_pointer(workspace[index]);
        lispobj* scan_start = page_scan_start(find_page_index(key));
        if (scan_start != previous_scan_start) where = previous_scan_start = scan_start;
        /* Scan forward from 'where'. This does not need a termination test based
         * on page_bytes_used because we know that 'key' was in-bounds for its page.
         * Therefore at least as many bytes are in use on the page as are needed
         * to enclose 'where'. If the next object we would visit is beyond it,
         * then we're done; the key was not found */
        while (1) {
            if (where < key) {
                where += object_size(where);
            } else if (where == key) {
                break;
            } else { // 'where' went past the key, so the key is bad
                workspace[index] = 0;
                removed = 1;
                break;
            }
        }
    }
    // Delete any 0s
    if (removed) {
        int new_index = 0;
        for (index = 0 ; index < count; ++index) {
            key = workspace[index];
            if (key) workspace[new_index++] = key;
        }
        gc_assert(new_index < count);
        count = new_index;
    }
    gc_pin_count = count;
    if (!(gencgc_verbose & 4)) return;
    // Print in multiple columns to fit more on a screen
    // and sort like 'ls' (down varying fastest)
    char description[24];
    fprintf(stderr, "Sorted pin list (%d):\n", count);
    const int ncolumns = 4;
    int nrows = ALIGN_UP(count,ncolumns) / ncolumns;
    int row, col;
    for (row = 0; row < nrows; ++row) {
        for (col = 0; col < ncolumns; ++col) {
            int index = col * nrows + row;
            if (index < count) {
                lispobj* obj = native_pointer(workspace[index]);
                lispobj word = *obj;
                strcpy(description, "cons");
                if (is_header(word))
                    snprintf(description, sizeof description, "%s,%ldw",
                             widetag_names[header_widetag(word)>>2],
                             (long)object_size(obj));
                fprintf(stderr, " %"OBJ_FMTX": %-24s", (uword_t)obj, description);
            }
        }
        putc('\n', stderr);
    }
}

/* After scavenging of the roots is done, we go back to the pinned objects
 * and look within them for pointers. */
static void
scavenge_pinned_ranges()
{
    int i;
    lispobj key;
    sword_t nwords = 0;
    for (i = 0; i < gc_pin_count; ++i) {
        key = gc_filtered_pins[i];
        gc_assert(is_lisp_pointer(key));
        lispobj* obj = native_pointer(key);
        if (listp(key)) {
            scavenge(obj, 2);
            nwords += 2;
        } else {
            lispobj header = *obj;
            nwords += scavtab[header_widetag(header)](obj, header);
        }
    }
    gc_pinned_nwords = nwords;
}

/* visit_freed_objects() was designed to support post-GC actions such as
 * recycling of unused symbol TLS indices. However, I could not make this work
 * as claimed at the time that it gets called, so at best this is reserved
 * for debugging, and only when you can tolerate some inaccuracy.
 *
 * The problem is that oldspace pages which were not pinned should eventually
 * be scanned en masse using contiguous blocks as large as possible without
 * encroaching on pinned pages. But we need to visit the dead objects on partially
 * pinned pages prior to turning those objects into page-filling objects.
 * Based on a real-life example, finding a correct approach is difficult.
 * Consider three pages all having the same scan_start of 0x1008e78000,
 * with the final page and only the final containing a pinned object:
 *
 *   start: 0x1008e78000       0x1008e80000       0x1008e88000
 *                                                 pin: 0x1008e8bec0
 *          ^------------------+------------------|
 * There is a page-spanning (SIMPLE-ARRAY (UNSIGNED-BYTE 64) 8192)
 * from 0x1008e78000 to 0x1008E88010 (exclusive). The penultimate word
 * of that array appears to be a valid widetag:
 *
 *   0x1008e88000: 0x0000000000001df1
 *   0x1008e88008: 0x0000000000000000
 * followed by:
 *   0x1008e88010: 0x0000001006c798c7  CONS
 *   0x1008e88018: 0x0000001008e88447
 *   0x1008e88020: 0x00000000000000ad  (SIMPLE-ARRAY (UNSIGNED-BYTE 64) 32)
 *   0x1008e88028: 0x0000000000000040
 *   ... pretty much anything in here ...
 *   0x1008e8bec0:                     any valid pinned object
 *
 * Page wiping ignores the pages based at 0x1008e78000 and 0x1008e80000
 * and it is only concerned with the range from 0x1008e88000..0x1008e8bec0
 * which becomes filler. The question is how to traverse objects in the filled
 * range. You can't start scanning dead objects at the page base address
 * of the final page because that would parse these objects as:
 *
 *   0x1008e88000: 0x0000000000001df1 (complex-vector-nil) ; 30 words
 *   0x1008e880f0: any random garbage
 *
 * But if you scan from the correct scan start of 0x1008e78000 then how do you
 * know to skip that page later (in free_oldspace), as it is entirely in oldspace,
 * but partially visited already? This what in malloc/free terms would be
 * a "double free", and there is no obvious solution to that.
 */
void visit_freed_objects(char __attribute__((unused)) *start,
                         sword_t __attribute__((unused)) nbytes)
{
#ifdef TRAVERSE_FREED_OBJECTS
    /* At this point we could attempt to recycle unused TLS indices
     * as follows: For each now-garbage symbol that had a nonzero index,
     * return that index to a "free TLS index" pool, perhaps a linked list
     * or bitmap. Then either always try the free pool first (for better
     * locality) or if ALLOC-TLS-INDEX detects exhaustion (for speed). */
    lispobj* where = (lispobj*)start;
    lispobj* end = (lispobj*)(start + nbytes);
    while (where < end) {
        lispobj word = *where;
        if (forwarding_pointer_p(where)) { // live oject
            /* CAUTION: This CAN NOT WORK RELIABLY. Due to gc_copy_object_resizing()
             * we might compute the wrong size because we take it from the copy.
             * Are there other places where we get this wrong??? I sure hope not */
            lispobj* fwd_where = native_pointer(forwarding_pointer_value(where));
            fprintf(stderr, "%p: -> %p\n", where, fwd_where);
            where += object_size(fwd_where);
        } else { // dead object
            fprintf(stderr, "%p: %"OBJ_FMTX" %"OBJ_FMTX"\n", where, where[0], where[1]);
            if (is_header(word)) {
                // Do something interesting
                where += headerobj_size(where, word);
            } else {
                /* Can't do much useful with conses because often we can't distinguish
                 * filler from data. visit_freed_objects is called on ranges of pages
                 * without regard to whether each intervening page was completely full.
                 * (This is not usually the way, but freeing of pages is slightly
                 * imprecise in that regard).
                 * And it's probably broken, since we leave detritus on code pages */
                where += 2;
            }
        }
    }
#endif
}

/* Deposit a FILLER_WIDETAG object covering one or more dead objects.
 * If using more than 1 card per page, scavenge_root_gens() is able to scan
 * some pages without aligning to object boundaries. For that to work,
 * it must not accidentally see a raw word or leftover garbage.
 * Note that while CONS and SMALL_MIXED pages never have card-spanning objects,
 * deposit_filler() deals with the "mirror image" of the pinned objects,
 * hence it might get a card-spanning filler. It has to do something to ensure
 * that no card will see garbage if scanned from its base address.
 * To achieve that, an extra filler may be needed at the start of any spanned card.
 * The sizes of extra fillers don't have to sum up to the total filler size.
 * They serve the vital purpose of getting descriptors_scavenge() to skip a
 * portion of the card they're on, but those fillers are never visited in a
 * heap walk that steps by object from a page's page_scan_start.
 * The final filler must be the correct size, so any algorithm that achieves
 * the desired end result is OK */
void deposit_filler(char* from, char* to) {
    sword_t nbytes = to - from;
    if (!nbytes) return;
    gc_assert(nbytes > 0);
    sword_t nwords = nbytes >> WORD_SHIFT;
    gc_assert((nwords - 1) <= 0x7FFFFF);
    page_index_t page = find_page_index(from);
    gc_assert(find_page_index(to-1) == page);
    *(lispobj*)from = make_filler_header(nwords);
    long unsigned last_card;
    switch (page_table[page].type) {
    case PAGE_TYPE_BOXED:
    case PAGE_TYPE_CONS:
    case PAGE_TYPE_SMALL_MIXED:
        last_card = addr_to_card_index(to-1);
        while (addr_to_card_index(from) != last_card) {
            from = PTR_ALIGN_DOWN(from, GENCGC_CARD_BYTES) + GENCGC_CARD_BYTES;
            nwords = (to - from) >> WORD_SHIFT;
            *(lispobj*)from = make_filler_header(nwords);
        }
    }
}

/* Deposit filler objects on small object pinned pages.
 * Also ensure that no scan_start_offset points to a page in
 * oldspace that will be freed.
 */
static void obliterate_nonpinned_words()
{
    if (!gc_pin_count) return;

#define page_base(x) ALIGN_DOWN(x, GENCGC_PAGE_BYTES)
// This macro asserts that space accounting happens exactly
// once per affected page (a page with any pins, no matter how many)
#define adjust_gen_usage(i) \
            gc_assert(page_table[i].gen == from_space); \
            bytes_moved += page_bytes_used(i); \
            page_table[i].gen = new_space

    lispobj* keys = gc_filtered_pins;
    int n_pins = gc_pin_count;
    // Store a sentinel at the end.
    // It is safe to write one more word than there are pins.
    keys[n_pins] = ~(uword_t)0;

    // Each pinned object begets two ranges of bytes to be turned into filler:
    // - the range preceding it back to its page start or predecessor object
    // - the range after it, up to the lesser of page bytes used or successor object

    // Prime the loop
    uword_t fill_from = page_base(keys[0]);
    os_vm_size_t bytes_moved = 0; // i.e. virtually moved
    int i;

    for (i = 0; i < n_pins; ++i) {
        lispobj* obj = native_pointer(keys[i]);
        page_index_t begin_page_index = find_page_index(obj);
        // Create a filler object occupying space from 'fill_from' up to but
        // excluding 'obj'.
        deposit_filler((char*)fill_from, (char*)obj);
        if (fill_from == page_base((uword_t)obj)) {
            adjust_gen_usage(begin_page_index);
            // This pinned object started a new page of pins.
            // scan_start must not see any page prior to this page,
            // as those might be in oldspace and about to be marked free.
            set_page_scan_start_offset(begin_page_index, 0);
        }
        // If 'obj' spans pages, move its successive page(s) to newspace and
        // ensure that those pages' scan_starts point at the same address
        // that this page's scan start does, which could be this page or earlier.
        sword_t nwords = object_size(obj);
        uword_t obj_end = (uword_t)(obj + nwords); // non-inclusive address bound
        page_index_t end_page_index = find_page_index((char*)obj_end - 1); // inclusive bound

        if (end_page_index > begin_page_index) {
            char *scan_start = page_scan_start(begin_page_index);
            page_index_t index;
            for (index = begin_page_index + 1; index <= end_page_index; ++index) {
                set_page_scan_start_offset(index,
                                           addr_diff(page_address(index), scan_start));
                adjust_gen_usage(index);
            }
        }
        // Compute page base address of last page touched by this obj.
        uword_t obj_end_pageaddr = page_base(obj_end - 1);
        // See if there's another pinned object on this page.
        // There is always a next object, due to the sentinel.
        if (keys[i+1] < obj_end_pageaddr + GENCGC_PAGE_BYTES) {
            // Next object starts within the same page.
            fill_from = obj_end;
        } else {
            /* Next pinned object does not start on the same page this obj ends on.
             * Any bytes following 'obj' up to its page end are garbage.
             * The reason we don't merely reduce the page_bytes_used is that decreasing
             * the grand total bytes allocated had a tendency to delay triggering the
             * next GC. This phenomenon was especially bad if the only pinned objects
             * were at the start of a page, as it caused the entire rest of the page to
             * be unusable. :SMALLOBJ-AUTO-GC-TRIGGER from rev dfddbc8a tests this */
            deposit_filler((char*)obj_end,
                           (char*)obj_end_pageaddr + page_bytes_used(end_page_index));
            fill_from = page_base(keys[i+1]);
        }
    }
    generations[from_space].bytes_allocated -= bytes_moved;
    generations[new_space].bytes_allocated += bytes_moved;
#undef adjust_gen_usage
#undef page_base
    if (pins_alloc_size) {
        os_deallocate((char*)gc_filtered_pins, pins_alloc_size);
        gc_filtered_pins = 0;
        gc_pin_count = 0;
        pins_alloc_size = 0;
    }
}

int sb_introspect_pinnedp(lispobj obj) {
    return hopscotch_containsp(&pinned_objects, obj);
}

/* Add 'object' to the hashtable, and if the object is a code component,
 * then also add all of the embedded simple-funs.
 * It is OK to call this function on an object which is already pinned-
 * it will do nothing.
 * But it is not OK to call this if the object is not one which merits
 * pinning in the first place. i.e. It MUST be an object in from_space
 * and moreover must be in the condemned set, which means that it can't
 * be a code object if pin_all_dynamic_space_code is 1.
 *
 * The rationale for doing some extra work on code components is that without it,
 * every call to pinned_p() would entail this logic:
 *   if the object is a simple-fun then
 *     read the header
 *     if already forwarded then return "no"
 *     else go backwards to the code header and test pinned_p().
 * But we can avoid that by making every embedded function pinned
 * whenever the containing object is pinned.
 * Experimentation bears out that this is the better technique.
 * Also, we wouldn't often expect code components in the collected generation
 * so the extra work here is quite minimal, even if it can generally add to
 * the number of keys in the hashtable.
 */
#define PAGE_PINNED 0xFF
static void pin_object(lispobj object)
{
    if (!compacting_p()) {
        gc_mark_obj(object);
        return;
    }

    lispobj* object_start = native_pointer(object);
    page_index_t page = find_page_index(object_start);

    /* Large object: the 'pinned' bit in the PTE on the first page should be definitive
     * for that object. However, all occupied pages have to marked pinned,
     * because move_pinned_pages_to_newspace() looks at pages as if they're independent.
     * That seems to be the only place that cares how many pages' pinned bits are affected
     * here for large objects, though I do wonder why we can't move the object right now
     * and be done with it */
    if (page_single_obj_p(page)) {
        if (gc_page_pins[page]) return;
        sword_t nwords = object_size(object_start);
        maybe_adjust_large_object(object_start, page, nwords);
        page_index_t last_page = find_page_index(object_start + nwords - 1);
        while (page <= last_page) gc_page_pins[page++] = PAGE_PINNED;
        return;
    }

    // Multi-object page (the usual case) - presence in the hash table is the pinned criterion.
    // The 'pinned' bit is a coarse-grained test of whether to bother looking in the table.
    if (hopscotch_containsp(&pinned_objects, object)) return;

    hopscotch_insert(&pinned_objects, object, 1);
    unsigned int addr_lowpart = object & (GENCGC_PAGE_BYTES-1);
    // Divide the page into 8 parts, mark that part pinned
    gc_page_pins[page] |= 1 << (addr_lowpart / (GENCGC_PAGE_BYTES/8));
    struct code* maybe_code = (struct code*)native_pointer(object);
    // Avoid iterating over embedded simple-funs until the debug info is set.
    // Prior to that, the unboxed payload will contain random bytes.
    // There can't be references to any of the simple-funs
    // until the object is fully constructed.
    if (widetag_of(&maybe_code->header) == CODE_HEADER_WIDETAG && maybe_code->debug_info) {
        for_each_simple_fun(i, fun, maybe_code, 0, {
            hopscotch_insert(&pinned_objects, make_lispobj(fun, FUN_POINTER_LOWTAG), 1);
            addr_lowpart = (uword_t)fun & (GENCGC_PAGE_BYTES-1);
            gc_page_pins[find_page_index(fun)] |=
                1 << (addr_lowpart / (GENCGC_PAGE_BYTES/8));
        })
    }
}

/* Additional logic for soft marks: any word that is potentially a
 * tagged pointer to a page being written must preserve the mark regardless
 * of what update_writeprotection() thinks. That's because the mark is set
 * prior to storing. If GC occurs in between setting the mark and storing,
 * then resetting the mark would be wrong if the subsequent store
 * creates an old->young pointer.
 * Mark stickiness is checked only once per invocation of collect_garbage(),
 * when scanning interrupt contexts for generation 0 but not higher gens.
 * There are two cases:
 * (1) tagged pointer to a large simple-vector, but we scan card-by-card
 *     for specifically the marked cards.  This has to be checked first
 *     so as not to fail to see subsequent cards if the first is marked.
 * (2) tagged pointer to an object that marks only the page containing
 *     the object base.
 * And note a subtle point: only an already-marked card can acquire sticky
 * status. So we can ignore any unmarked (a/k/a WRITEPROTECTED_P) card
 * regardless of a context register pointing to it, because if a mark was not
 * stored, then the pointer was not stored. Without examining the next few
 * instructions, there's no reason even to suppose that a store occurs.
 * It seems like the stop-for-GC handler must be enforcing that GC sees things
 * stored in the correct order for out-of-order memory models */
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
static void impart_mark_stickiness(lispobj word)
{
    // This function does not care whether 'word' points to a valid object.
    // At worst this will spurisouly mark a card as sticky,
    // which can happen only if it was already marked as dirty.
    page_index_t page = find_page_index((void*)word);
    if (page >= 0 && page_boxed_p(page) // stores to raw bytes are uninteresting
        && (word & (GENCGC_PAGE_BYTES - 1)) < page_bytes_used(page)
        && page_table[page].gen != 0
        && lowtag_ok_for_page_type(word, page_table[page].type)
        && plausible_tag_p(word)) { // "plausible" is good enough
        /* if 'word' is the correctly-tagged pointer to the base of a SIMPLE-VECTOR,
         * then set the sticky mark on every marked card. The only other large
         * objects are CODE (writes to which are pseudo-atomic),
         * and BIGNUM (which aren't on boxed pages)
         * I'm not sure if it's inadvertent that this first 'if' is taken
         * for non-large simple-vectors. It probably can't hurt,
         * but I think it's not necessary */
        if (lowtag_of(word) == OTHER_POINTER_LOWTAG &&
            widetag_of(native_pointer(word)) == SIMPLE_VECTOR_WIDETAG) {
            generation_index_t gen = page_table[page].gen;
            while (1) {
                long card = page_to_card_index(page);
                int i;
                for(i=0; i<CARDS_PER_PAGE; ++i)
                    if (gc_card_mark[card+i]==CARD_MARKED) gc_card_mark[card+i]=STICKY_MARK;
                if (page_ends_contiguous_block_p(page, gen)) return;
                ++page;
            }
        } else if (gc_card_mark[addr_to_card_index((void*)word)] == CARD_MARKED) {
            gc_card_mark[addr_to_card_index((void*)word)] = STICKY_MARK;
        }
    }
}
#endif

#if !GENCGC_IS_PRECISE || defined LISP_FEATURE_MIPS || defined LISP_FEATURE_PPC64
/* Take a possible pointer to a Lisp object and mark its page in the
 * page_table so that it will not be relocated during a GC.
 *
 * This involves locating the page it points to, then backing up to
 * the start of its region, then marking all pages pinned from there
 * up to the first page that's not full or has a different generation
 *
 * It is assumed that all the pages' pin flags have been cleared at
 * the start of a GC.
 *
 * It is also assumed that the current gc_alloc() region has been
 * flushed and the tables updated. */

static void NO_SANITIZE_MEMORY preserve_pointer(os_context_register_t word, void* arg)
{
    int contextp = arg == (void*)1;
    page_index_t page = find_page_index((void*)word);
    if (page < 0) {
        // Though immobile_space_preserve_pointer accepts any pointer,
        // there's a benefit to testing immobile_space_p first
        // because it's inlined. Either is a no-op if no immobile space.
        if (immobile_space_p(word))
            immobile_space_preserve_pointer((void*)word);
        return;
    }

    // Special case for untagged instance pointers in registers. This might belong in
    // conservative_root_p() but the pointer has to be adjusted here or else the wrong
    // value will be inserted into 'pinned_objects' (which demands tagged pointers)
    if (contextp && lowtag_of(word) == 0 &&
        (page_table[page].type == PAGE_TYPE_MIXED ||
         page_table[page].type == PAGE_TYPE_SMALL_MIXED) &&
        widetag_of((lispobj*)word) == INSTANCE_WIDETAG)
        word |= INSTANCE_POINTER_LOWTAG;

    lispobj object = conservative_root_p(word, page);
    if (!object) return;
    if (object != AMBIGUOUS_POINTER) {
        pin_object(object);
        return;
    }
    // It's a non-large non-code ambiguous pointer.
    if (compacting_p()) {
        if (!hopscotch_containsp(&pinned_objects, word)) {
            hopscotch_insert(&pinned_objects, word, 1);
            unsigned int addr_lowpart = word & (GENCGC_PAGE_BYTES-1);
            // Divide the page into 8 parts, mark that part pinned
            gc_page_pins[page] |= 1 << (addr_lowpart / (GENCGC_PAGE_BYTES/8));
        }
        return;
    }
    // Mark only: search for the object, because fullcgc can't handle random pointers
    lispobj* found = search_dynamic_space((void*)word);
    if (found) gc_mark_obj(compute_lispobj(found));
}
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
static void sticky_preserve_pointer(os_context_register_t register_word, void* arg)
{
    // registers can be wider than words. This could accept uword_t as the arg type
    // but I like it to be directly callable with os_context_register.
    uword_t word = register_word;
    if (is_lisp_pointer(word)) impart_mark_stickiness(word);
    preserve_pointer(word, arg);
}
#endif
#endif

/* Pin an unambiguous descriptor object which may or may not be a pointer.
 * Ignore immediate objects, and heuristically skip some objects that are
 * known to be pinned without looking in pinned_objects.
 * pin_object() will always do the right thing and ignore multiple
 * calls with the same object in the same collection pass.
 */
static void pin_exact_root(lispobj obj)
{
    // These tests are performed in approximate order of quickness to check.

    // 1. pointerness
    if (!is_lisp_pointer(obj)) return;
    // 2. If not moving, then pinning is irrelevant. 'obj' is a-priori live given
    //    the reference from *PINNED-OBJECTS*, and obviously it won't move.
    if (!compacting_p()) return;
    // 3. If pointing off-heap, why are you pinning? Just ignore it.
    // Would this need to do anything if immobile-space were ported
    // to the precise GC platforms. FIXME?
    page_index_t page = find_page_index((void*)obj);
    if (page < 0) return;
    // 4. Ignore if not in the condemned set.
    if (immune_set_memberp(page)) return;

    // Never try to pin an interior pointer - always use base pointers.
    lispobj *object_start = native_pointer(obj);
    switch (widetag_of(object_start)) {
    case SIMPLE_FUN_WIDETAG:
#ifdef RETURN_PC_WIDETAG
    case RETURN_PC_WIDETAG:
#endif
        obj = make_lispobj(fun_code_header((struct simple_fun*)object_start),
                           OTHER_POINTER_LOWTAG);
    }
    pin_object(obj);
}


/* Return true if 'ptr' is OK to be on a write-protected page
 * of an object in 'gen'. That is, if the pointer does not point to a younger object.
 * Note: 'ptr' is _sometimes_ an ambiguous pointer - we do not utilize the layout bitmap
 * when scanning instances for pointers, so we will occasionally see a raw word for 'ptr'.
 * Also, 'ptr might not have a lowtag (such as lockfree list node successor), */
static bool ptr_ok_to_writeprotect(lispobj ptr, generation_index_t gen)
{
    page_index_t index;
    lispobj __attribute__((unused)) header;

    /* Check that it's in the dynamic space */
    if ((index = find_page_index((void*)ptr)) != -1) {
            int pointee_gen = page_table[index].gen;
            if (/* Does it point to a younger or the temp. generation? */
                (pointee_gen < gen || pointee_gen == SCRATCH_GENERATION) &&
                /* and an in-use part of the page?
                 * Formerly this examined the bounds of each open region,
                 * but that is extra work with little benefit. It is faster
                 * to treat all of any page with an open region as in-use.
                 * It will self-correct when the region gets closed */
                ((page_table[index].type & OPEN_REGION_PAGE_FLAG)
                 || (ptr & (GENCGC_PAGE_BYTES-1)) < page_bytes_used(index)))
                return 0;
        }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        else if (immobile_space_p(ptr) &&
                 other_immediate_lowtag_p(header = *native_pointer(ptr))) {
            // This is *possibly* a pointer to an object in immobile space,
            // given that above two conditions were satisfied.
            // But unlike in the dynamic space case, we need to read a byte
            // from the object to determine its generation, which requires care.
            // Consider an unboxed word that looks like a pointer to a word that
            // looks like simple-fun-widetag. We can't naively back up to the
            // underlying code object since the alleged header might not be one.
            int pointee_gen = gen; // Make comparison fail if we fall through
            switch (header_widetag(header)) {
            case SIMPLE_FUN_WIDETAG:
                if (functionp(ptr)) {
                    lispobj* code = (lispobj*)fun_code_header(FUNCTION(ptr));
                    // This is a heuristic, since we're not actually looking for
                    // an object boundary. Precise scanning of 'page' would obviate
                    // the guard conditions here.
                    if (immobile_space_p((lispobj)code)
                        && widetag_of(code) == CODE_HEADER_WIDETAG)
                        pointee_gen = immobile_obj_generation(code);
                }
                break;
            default:
                pointee_gen = immobile_obj_generation(native_pointer(ptr));
            }
            // A bogus generation number implies a not-really-pointer,
            // but it won't cause misbehavior.
            if (pointee_gen < gen || pointee_gen == SCRATCH_GENERATION) {
                return 0;
            }
        }
#endif
    return 1;
}

#ifndef LISP_FEATURE_SOFT_CARD_MARKS
static inline void protect_page(void* page_addr)
{
    os_protect((void *)page_addr, GENCGC_PAGE_BYTES, OS_VM_PROT_READ);
    gc_card_mark[addr_to_card_index(page_addr)] = CARD_UNMARKED;
}
#endif

#define LOCKFREE_LIST_NEXT(x) ((struct list_node*)x)->_node_next

/* Helper function for update_writeprotection.
 * If the [where,limit) contain an old->young pointer, then return
 * the address - or approximate address - containing such pointer.
 * The return value is used as a boolean, but if debugging, you might
 * want to see the address */
static lispobj* range_dirty_p(lispobj* where, lispobj* limit, generation_index_t gen)
{
    sword_t nwords;
    for ( ; where < limit ; where += nwords ) {
        lispobj word = *where;
        if (is_cons_half(word)) {
            if (is_lisp_pointer(word) && !ptr_ok_to_writeprotect(word, gen)) return where;
            word = where[1];
            if (is_lisp_pointer(word) && !ptr_ok_to_writeprotect(word, gen)) return where;
            nwords = 2;
            continue;
        }
        int widetag = widetag_of(where);
        gc_dcheck(widetag != CODE_HEADER_WIDETAG); // This can't be called on a code page
        nwords = sizetab[widetag](where);
        if (leaf_obj_widetag_p(widetag)) continue; // Do nothing
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
        if (instanceoid_widetag_p(widetag)) {
            // instance_layout works on funcallable or regular instances
            // and we have to specially check it because it's in the upper
            // bytes of the 0th word.
            lispobj layout = instance_layout(where);
            if (layout) {
                if (!ptr_ok_to_writeprotect(layout, gen)) return where;
                if (lockfree_list_node_layout_p(LAYOUT(layout)) &&
                    !ptr_ok_to_writeprotect(LOCKFREE_LIST_NEXT(where), gen))
                    return where;
            }
        }
#else
        if (widetag == INSTANCE_WIDETAG) {
            // instance_layout works only on regular instances,
            // we don't have to treat it specially but we do have to
            // check for lockfree list nodes.
            lispobj layout = instance_layout(where);
            if (layout && lockfree_list_node_layout_p(LAYOUT(layout)) &&
                !ptr_ok_to_writeprotect(LOCKFREE_LIST_NEXT(where), gen))
                return where;
        }
#endif
#ifdef LISP_FEATURE_LINKAGE_SPACE
        else if (widetag == SYMBOL_WIDETAG) {
            struct symbol* s = (void*)where;
            if (!ptr_ok_to_writeprotect(linkage_cell_function(symbol_linkage_index(s)), gen))
                return where;
            // Process the value and info slots normally, and the bit-packed package ID + name
            // can't be younger, so that slot's contents are irrelevant
        } else if (widetag == FDEFN_WIDETAG) {
            struct fdefn* f = (void*)where;
            if (!ptr_ok_to_writeprotect(linkage_cell_function(fdefn_linkage_index(f)), gen))
                return where;
        }
#endif
        // Scan all the rest of the words even if some of them are raw bits.
        // At worst this overestimates the set of pointer words.
        sword_t index;
        for (index=1; index<nwords; ++index)
            if (is_lisp_pointer(where[index]) && !ptr_ok_to_writeprotect(where[index], gen))
                return where;
    }
    return 0;
}

/* Given a range of pages at least one of which is not WPed (logically or physically,
 * depending on SOFT_CARD_MARKS), scan all those pages for pointers to younger generations.
 * If no such pointers are found, then write-protect the range.
 *
 * Care is taken to check for pointers to any open allocation regions,
 * which by design contain younger objects.
 *
 * If we find a word which is a witness for the inability to apply write-protection,
 * then return the address of the object containing the witness pointer.
 * Otherwise return 0. The word address is just for debugging; there are cases
 * where we don't apply write protectection, but nonetheless return 0.
 *
 * This function is still buggy, but not in a fatal way.
 * The issue is that for any kind of weak object - hash-table vector,
 * weak pointer, or weak simple-vector, we skip scavenging the object
 * which might leave some pointers to younger generation objects
 * which will later be smashed when processing weak objects.
 * That is, the referent is non-live. But when we scanned this page range,
 * it looks like it still had the pointer to the younger object.
 * To get this really right, we would have to wait until after weak objects
 * have been processed.
 * It may or may not be possible to get verify_range to croak
 * about suboptimal application of WP. Possibly not, because of the hack
 * for pinned pages without soft card marking (which won't WP).
 *
 * See also 'doc/internals-notes/fdefn-gc-safety' for execution schedules
 * that lead to invariant loss with FDEFNs. This might not be a problem
 * in practice. At least it seems like it never has been.
 */
static lispobj*
update_writeprotection(page_index_t first_page, page_index_t last_page,
                       lispobj* where, lispobj* limit)
{
    /* Shouldn't be a free page. */
    gc_dcheck(!page_free_p(first_page)); // Implied by the next assertion
    gc_assert(page_words_used(first_page) != 0);

    if (!ENABLE_PAGE_PROTECTION) return 0;
    if (!page_boxed_p(first_page)) return 0;

    page_index_t page;
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    /* If any page is referenced from the stack (mark byte = 2), then we're
     * can not apply protection even if we see no witness, because the
     * absence of synchronization between mutator and GC means that the next
     * instruction issued when the mutator resumes might create the witness,
     * and it thinks it already marked a card */
    for (page = first_page; page <= last_page; ++page)
        if (cardseq_any_sticky_mark(page_to_card_index(page))) return 0;
#else
    /* Skip if any page is pinned.
     * The 'pinned' check is sort of bogus but sort of necessary,
     * but doesn't completely fix the problem that it tries to, which is
     * passing a memory address to the OS for it to write into.
     * An object on a never-written protected page would still fail.
     * It's probably rare to pass boxed pages to the OS, but it could be
     * to read fixnums into a simple-vector. */
    for (page = first_page; page <= last_page; ++page)
        if (gc_page_pins[page]) return 0;
#endif

    /* Now we attempt to find any 1 "witness" that the pages should NOT be protected.
     * If such witness is found, then return without doing anything, otherwise
     * apply protection to the range. */
    lispobj* witness = range_dirty_p(where, limit, page_table[first_page].gen);
    if (witness) return witness;

    for (page = first_page; page <= last_page; ++page) {
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        // Don't worry, the cards are all clean - if any card mark was sticky,
        // then we would have bailed out as the first thing (way up above).
        assign_page_card_marks(page, CARD_UNMARKED);
#else
        // Try to avoid a system call
        if (!PAGE_WRITEPROTECTED_P(page)) protect_page(page_address(page));
#endif
    }
    return 0;
}

/* Decide if this single-object page holds a normal simple-vector.
 * "Normal" now includes non-weak address-insensitive k/v vectors */
static inline bool large_scannable_vector_p(page_index_t page) {
    lispobj header = *(lispobj *)page_address(page);
    if (header_widetag(header) == SIMPLE_VECTOR_WIDETAG) {
        int mask = (flag_VectorWeak | flag_VectorAddrHashing) << ARRAY_FLAGS_POSITION;
        if (header & mask) return 0;
        if (vector_flagp(header, VectorHashing)) {
            lispobj* data = ((struct vector*)page_address(page))->data;
            // If not very full, use the normal path.
            // The exact boundary here doesn't matter too much.
            if (KV_PAIRS_HIGH_WATER_MARK(data) < (int)(GENCGC_PAGE_BYTES/N_WORD_BYTES))
                return 0;
        }
        return 1;
    }
    return 0;
}

/* Attempt to re-protect code from first_page to last_page inclusive.
 * The object bounds are 'start' and 'limit', the former being redundant
 * with page_address(first_page).
 * Immobile space is dealt with in "immobile-space.c"
 */
static void
update_code_writeprotection(page_index_t first_page, page_index_t last_page,
                            lispobj* start, lispobj* limit)
{
    if (!ENABLE_PAGE_PROTECTION) return;
    page_index_t i;
    for (i=first_page; i <= last_page; ++i) {// last_page is inclusive
        gc_assert(is_code(page_table[i].type));
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        if (cardseq_any_sticky_mark(page_to_card_index(i))) {
            return;
        }
#endif
    }

    lispobj* where = start;
    for (; where < limit; where += headerobj_size(where)) {
        switch (widetag_of(where)) {
        case CODE_HEADER_WIDETAG:
            if (header_rememberedp(*where)) return;
            break;
        case FUNCALLABLE_INSTANCE_WIDETAG:
            if (range_dirty_p(where, where+headerobj_size(where), page_table[first_page].gen))
                return;
            break;
        }
    }
    for (i = first_page; i <= last_page; i++) assign_page_card_marks(i, CARD_UNMARKED);
}

#ifdef LISP_FEATURE_SOFT_CARD_MARKS
# define card_stickymarked_p(x) (gc_card_mark[x] == STICKY_MARK)
#endif
extern int descriptors_scavenge(lispobj *, lispobj*, generation_index_t, int);
int root_boxed_words_scanned, root_vector_words_scanned, root_mixed_words_scanned;

/* Special treatment for strictly boxed pages improves on the general case as follows:
 * - It can skip determining the extent of the contiguous block up front,
 *   instead just blasting through the cards as it sees them.
 * - If only a subset of cards in a contiguous block are dirty, the scan
 *   can be restricted to that subset. We don't need to align at object boundaries.
 * - It is not necessary to invoke a scavenge method specific to each object type.
 * - new write-protection status can be recomputed as we go.
 * This combination of aspects will be especially beneficial if cards are
 * are much smaller than they currently are (like 1K)

 * We have two choices for object traversal: walk object-by-object,
 * or card-by-card just blasting through the words looking for pointers.
 * But the latter can fail on a card-spanning object if care is not taken.
 * Example: Suppose the card size is 1K, and an instance has 200 slots.
 * The instance consumes around 1600 bytes (@ 8 bytes/word), which conceivably
 * could use 3 cards: header + 10 slots on the end of the first card,
 * 128 slots on the next, and the remainder on the final card. The soft write
 * barrier marks only the card with the header, so we don't know exactly
 * which card contains a modified pointer. Therefore, in all cases when using
 * card-by-card scan that disregards object boundaries, we have to assume
 * that 1 card beyond any marked card contains part of a marked object,
 * if that next card has the same scan start as its predecessor.
 * But where to stop scanning under this assumption? We shouldn't assume
 * that any marked card implies scanning an unbounded number of cards.
 * Therefore, a big instance should not be put on a purely boxed card.
 * (And granted, a massive instance will go on single-object pages.)
 * The other purely boxed objects are cons-sized, so they don't have a problem.
 * And (SETF SVREF) does mark an exact card, so it's all good.
 * Also, the hardware write barrier does not have this concern.
 */
#define WORDS_PER_CARD (GENCGC_CARD_BYTES/N_WORD_BYTES)
static page_index_t scan_boxed_root_cards_spanning(page_index_t page, generation_index_t gen)
{
    __attribute__((unused)) int prev_marked = 0;
    do {
        lispobj* start = (void*)page_address(page);
        lispobj* limit = start + page_words_used(page);
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        long card = addr_to_card_index(start);
        /* Cards can change from marked to unmarked (just like with physical protection),
         * but also unmarked to marked, if transferring the card mark from the object's
         * header card to a cell in that object on a later card.
         * Lisp is given leeway because marking the header is easier. So the
         * algorithm accepts either way on input, but makes its output canonical.
         * (similar in spirit to Postel's Law) */
        if (prev_marked || cardseq_any_marked(card)) {
            if (GC_LOGGING) fprintf(gc_activitylog(), "scan_roots spanning %p\n", page_address(page));
            int j;
            for (j=0; j<CARDS_PER_PAGE; ++j, ++card, start += WORDS_PER_CARD) {
                int marked = card_dirtyp(card);
                if (marked || prev_marked) {
                    lispobj* end = start + WORDS_PER_CARD;
                    if (end > limit) end = limit;
                    int dirty = descriptors_scavenge(start, end, gen, card_stickymarked_p(card));
                    root_boxed_words_scanned += end - start;
                    gc_card_mark[card] =
                      (gc_card_mark[card] != STICKY_MARK) ? (dirty ? CARD_MARKED : CARD_UNMARKED) :
                      STICKY_MARK;
                    prev_marked = marked;
                }
            }
        }
#else
        if (!PAGE_WRITEPROTECTED_P(page)) {
            int dirty = descriptors_scavenge(start, limit, gen, 0);
            if (ENABLE_PAGE_PROTECTION && !dirty) protect_page(start);
        }
#endif
        ++page;
    } while (!page_ends_contiguous_block_p(page-1, gen));
    return page;
}

/* Large simple-vectors and pages of conses are even easier than strictly boxed root pages
 * because individual cons cells can't span cards, and vectors always mark the card of a
 * specific element. So there is no looking back 1 card to check for a marked header */
static page_index_t scan_boxed_root_cards_non_spanning(page_index_t page, generation_index_t gen)
{
#ifndef LISP_FEATURE_SOFT_CARD_MARKS
    /* Physical protection doesn't distinguish between card-spanning and non-card-spanning,
     * because the write fault always occurs on the page that is getting dirtied by a store,
     * unlike soft marks which can mark an object header, but store onto the next card */
    return scan_boxed_root_cards_spanning(page, gen);
#else
    do {
        lispobj* start = (void*)page_address(page);
        long card = addr_to_card_index(start);
        if (cardseq_any_marked(card)) {
            if (GC_LOGGING) fprintf(gc_activitylog(), "scan_roots non-spanning %p\n", page_address(page));
            lispobj* limit = start + page_words_used(page);
            int j;
            for (j=0; j<CARDS_PER_PAGE; ++j, ++card, start += WORDS_PER_CARD) {
                if (card_dirtyp(card)) {
                    lispobj* end = start + WORDS_PER_CARD;
                    if (end > limit) end = limit;
                    int dirty = descriptors_scavenge(start, end, gen,
                                                     card_stickymarked_p(card));
                    root_vector_words_scanned += end - start;
                    if (!dirty) gc_card_mark[card] = CARD_UNMARKED;
                }
            }
        }
        ++page;
    } while (!page_ends_contiguous_block_p(page-1, gen));
    return page;
#endif
}

#ifdef LISP_FEATURE_SOFT_CARD_MARKS
/* PAGE_TYPE_SMALL_MIXED roots are walked object-by-object to avoid affecting any raw word.
 * By construction, objects will never span cards */
static page_index_t scan_mixed_root_cards(page_index_t page, generation_index_t gen)
{
    do {
        lispobj* start = (void*)page_address(page);
        long card = addr_to_card_index(start);
        if (cardseq_any_marked(card)) {
            if (GC_LOGGING) fprintf(gc_activitylog(), "scan_roots subcard mixed %p\n", page_address(page));
            lispobj* limit = start + page_words_used(page);
            int j;
            for (j=0; j<CARDS_PER_PAGE; ++j, ++card, start += WORDS_PER_CARD) {
                if (card_dirtyp(card)) {
                    lispobj* end = start + WORDS_PER_CARD;
                    if (end > limit) end = limit;
                    // heap_scavenge doesn't take kindly to inverted start+end
                    if (start < limit) {
                        heap_scavenge(start,  limit);
                        if (!card_stickymarked_p(card) && !range_dirty_p(start, limit, gen))
                            gc_card_mark[card] = CARD_UNMARKED;
                    } else
                        gc_card_mark[card] = CARD_UNMARKED;
                }
            }
        }
        ++page;
    } while (!page_ends_contiguous_block_p(page-1, gen));
    return page;
}
#endif

/* Scavenge all generations greater than or equal to FROM.
 *
 * Under the current scheme when a generation is GCed, the generations
 * younger than it are empty. So, when a generation is being GCed it
 * is only necessary to examine generations older than it for pointers.
 *
 * Logical or physical write-protection is used to note pages that don't
 * contain old->young pointers. But pages can be written without having
 * such pointers. After the pages are scavenged here, they are examined
 * for old->young pointer, are marked clean (unprotected) if there are none.
 *
 * Write-protected pages will not have any pointers to the
 * from_space so do not need scavenging, but might be visited
 * as part of a contiguous range containing a relevant page.
 *
 */
static void
scavenge_root_gens(generation_index_t from)
{
    page_index_t i = 0;
    page_index_t limit = next_free_page;
    gc_dcheck(compacting_p());

    while (i < limit) {
        generation_index_t generation = page_table[i].gen;
        if (generation < from || generation == SCRATCH_GENERATION
            /* Not sure why word_used is checked. Probably because reset_page_flags()
             * does not change the page's gen to an unused number. Perhaps it should */
            || !page_boxed_p(i) || !page_words_used(i)) {
            ++i;
            continue;
        }

        /* This should be the start of a region */
        gc_assert(page_starts_contiguous_block_p(i));

        if (page_table[i].type == PAGE_TYPE_BOXED) {
            i = scan_boxed_root_cards_spanning(i, generation);
        } else if ((page_table[i].type == PAGE_TYPE_CONS) ||
                   (page_single_obj_p(i) && large_scannable_vector_p(i))) {
            i = scan_boxed_root_cards_non_spanning(i, generation);
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        } else if (page_table[i].type == PAGE_TYPE_SMALL_MIXED) {
            i = scan_mixed_root_cards(i, generation);
#endif
        } else {
            page_index_t last_page;
            int marked = 0;
            /* Now work forward until the end of the region */
            for (last_page = i; ; last_page++) {
                long card_index = page_to_card_index(last_page);
                marked = marked || cardseq_any_marked(card_index);
                if (page_ends_contiguous_block_p(last_page, generation))
                    break;
            }
            if (marked) {
                lispobj* start = (lispobj*)page_address(i);
                lispobj* limit =
                    (lispobj*)page_address(last_page) + page_words_used(last_page);
                if (GC_LOGGING) fprintf(gc_activitylog(), "scan_roots mixed %p:%p\n", start, limit);
                root_mixed_words_scanned += limit - start;
                heap_scavenge(start, limit);
                /* Now scan the pages and write protect those that
                 * don't have pointers to younger generations. */
                if (is_code(page_table[i].type))
                    update_code_writeprotection(i, last_page, start, limit);
                else
                    update_writeprotection(i, last_page, start, limit);
            }
            i = 1 + last_page;
        }
    }
}


/* Scavenge a newspace generation. As it is scavenged new objects may
 * be allocated to it; these will also need to be scavenged. This
 * repeats until there are no more objects unscavenged in the
 * newspace generation.
 *
 * To help improve the efficiency, areas written are recorded by
 * gc_alloc() and only these scavenged. Sometimes a little more will be
 * scavenged, but this causes no harm. An easy check is done that the
 * scavenged bytes equals the number allocated in the previous
 * scavenge.
 *
 * Write-protected pages are not scanned except if they are marked
 * pinned, in which case they may have been promoted and still have
 * pointers to the from space.
 *
 * Write-protected pages could potentially be written by alloc however
 * to avoid having to handle re-scavenging of write-protected pages
 * gc_alloc() does not write to write-protected pages.
 *
 * New areas of objects allocated are recorded alternatively in the two
 * new_areas arrays below. */
static struct new_area new_areas_1[NUM_NEW_AREAS];
static struct new_area new_areas_2[NUM_NEW_AREAS];

/* Do one full scan of the new space generation. This is not enough to
 * complete the job as new objects may be added to the generation in
 * the process which are not scavenged. */
static void newspace_full_scavenge(generation_index_t generation)
{
    page_index_t i;

    for (i = 0; i < next_free_page; i++) {
        if ((page_table[i].gen == generation) && page_boxed_p(i)
            && (page_words_used(i) != 0)
            && cardseq_any_marked(page_to_card_index(i))) {
            page_index_t last_page;

            /* The scavenge will start at the scan_start_offset of
             * page i.
             *
             * We need to find the full extent of this contiguous
             * block in case objects span pages. */
            for (last_page = i; ;last_page++) {
                /* Check whether this is the last page in this
                 * contiguous block */
                if (page_ends_contiguous_block_p(last_page, generation))
                    break;
            }

            record_new_regions_below = 1 + last_page;
            heap_scavenge(page_scan_start(i),
                          (lispobj*)page_address(last_page) + page_words_used(last_page));
            i = last_page;
        }
    }
    /* Enable recording of all new allocation regions */
    record_new_regions_below = 1 + page_table_pages;
}

void gc_close_collector_regions(int flag)
{
    ensure_region_closed(code_region, flag|PAGE_TYPE_CODE);
    ensure_region_closed(boxed_region, PAGE_TYPE_BOXED);
    ensure_region_closed(unboxed_region, PAGE_TYPE_UNBOXED);
    ensure_region_closed(mixed_region, PAGE_TYPE_MIXED);
    ensure_region_closed(small_mixed_region, PAGE_TYPE_SMALL_MIXED);
    ensure_region_closed(cons_region, PAGE_TYPE_CONS);
}

/* Do a complete scavenge of the newspace generation. */
static void
scavenge_newspace(generation_index_t generation)
{
    /* Flush the current regions updating the page table. */
    gc_close_collector_regions(0);

    /* Turn on the recording of new areas. */
    gc_assert(new_areas_index == 0);
    new_areas = new_areas_1;

    /* Start with a full scavenge. */
    if (GC_LOGGING) fprintf(gc_activitylog(), "newspace full scav\n");
    newspace_full_scavenge(generation);

    /* Flush the current regions updating the page table. */
    gc_close_collector_regions(0);

    while (1) {
        if (GC_LOGGING) fprintf(gc_activitylog(), "newspace loop\n");
        if (!new_areas_index && !immobile_scav_queue_count) { // possible stopping point
            if (!test_weak_triggers(0, 0))
                break; // no work to do
            // testing of triggers can't detect whether any triggering object
            // actually entails new work - it only knows which triggers were removed
            // from the pending list. So check again if allocations occurred,
            // which is only if not all triggers referenced already-live objects.
            gc_close_collector_regions(0); // update new_areas from regions
            if (!new_areas_index && !immobile_scav_queue_count)
                break; // still no work to do
        }
        /* Move the current to the previous new areas */
        struct new_area *previous_new_areas = new_areas;
        int previous_new_areas_index = new_areas_index;
        /* Note the max new_areas used. */
        if (new_areas_index > new_areas_index_hwm)
            new_areas_index_hwm = new_areas_index;

        /* Prepare to record new areas. Alternate between using new_areas_1 and 2 */
        new_areas = (new_areas == new_areas_1) ? new_areas_2 : new_areas_1;
        new_areas_index = 0;

        scavenge_immobile_newspace();
        /* Check whether previous_new_areas had overflowed. */
        if (previous_new_areas_index >= NUM_NEW_AREAS) {

            /* New areas of objects allocated have been lost so need to do a
             * full scan to be sure! If this becomes a problem try
             * increasing NUM_NEW_AREAS. */
            newspace_full_scavenge(generation);

        } else {

            int i;
            /* Work through previous_new_areas. */
            for (i = 0; i < previous_new_areas_index; i++) {
                page_index_t page = previous_new_areas[i].page;
                size_t offset = previous_new_areas[i].offset;
                size_t size = previous_new_areas[i].size;
                gc_assert(size % (2*N_WORD_BYTES) == 0);
                lispobj *start = (lispobj*)(page_address(page) + offset);
                if (GC_LOGGING) fprintf(gc_activitylog(), "heap_scav %p..%p\n",
                                     start, (lispobj*)((char*)start + size));
                heap_scavenge(start, (lispobj*)((char*)start + size));
            }

        }
        /* Flush the current regions updating the page table. */
        gc_close_collector_regions(0);
    }

    /* Turn off recording of allocation regions. */
    record_new_regions_below = 0;
    new_areas = NULL;
    new_areas_index = 0;
}

/* Un-write-protect all the pages in from_space. This is done at the
 * start of a GC else there may be many page faults while scavenging
 * the newspace (I've seen drive the system time to 99%). These pages
 * would need to be unprotected anyway before unmapping in
 * free_oldspace; not sure what effect this has on paging..
 *
 * Here is a real-life example of what can go wrong if we don't
 * unprotect oldspace:
 * Scenario:
 *   - gc-with-promotion (raise=1) of gen2 to gen3
 *   - symbol FOO in gen 3 on page 1000
 *   - large vector 'v' in gen 2 on page 1300..1305
 *   - 'v' points only to gen 2 objects (so it is unmarked, or "protected")
 *   - symbol-value of FOO is 'v'
 *   - root generations are 4 and higher
 *   - no roots point to vector 'v' or any of its contents
 * Thence:
 *   - scavenge_newspace_full_scan visits page 1000
 *   - assigns 'record_new_regions_below' = 1001
 *   - traces slots of FOO, calls copy_potential_large_object(v)
 *   - 'v' is promoted into gen3
 *   - call add_new_area on page 1300..1305
 *   - 1300 exceeds 1001 so we skip this area
 * So because 'v' is ahead of the wavefront, and theoretically page 1300
 * will be picked up by the remainder of the full_scan loop, we optimized out
 * the addition of the area. But then the scan loop sees that page 1300
 * is protected and it decides that it can can skip it even though it was
 * originally part of 'from_space' and points to other 'from_space' things.
 * The consequence is that everything 'v' pointed to in gen2 becomes freed
 * while 'v' holds dangling pointers to all that garbage.
 */
static void
unprotect_oldspace(void)
{
    page_index_t i;

    /* Gen0 never has protection applied, so we can usually skip the un-protect step,
     * however, in the final GC, because everything got moved to gen0 by brute force
     * adjustment of the page table, we don't know the state of the protection.
     * Therefore only skip out if NOT in the final GC */
    if (conservative_stack && from_space == 0) return;

#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    for (i = 0; i < next_free_page; i++)
        /* Why does this even matter? Obviously it did for physical protection
         * (storing the forwarding pointers shouldn't fault)
         * but there's no physical protection, so ... why bother?
         * But I tried removing it and got assertion failures */
        if (page_words_used(i) && page_table[i].gen == from_space)
            assign_page_card_marks(i, CARD_MARKED);
#else
    char *page_addr = 0;
    char *region_addr = 0;
    uword_t region_bytes = 0;
    for (i = 0; i < next_free_page; i++) {
        if ((page_words_used(i) != 0)
            && (page_table[i].gen == from_space)) {

            /* Remove any write-protection. We should be able to rely
             * on the write-protect flag to avoid redundant calls. */
            if (PAGE_WRITEPROTECTED_P(i)) {
                SET_PAGE_PROTECTED(i, 0);
                if (protection_mode(i) == PHYSICAL) {
                    page_addr = page_address(i);
                    if (!region_addr) {
                        /* First region. */
                        region_addr = page_addr;
                        region_bytes = GENCGC_PAGE_BYTES;
                    } else if (region_addr + region_bytes == page_addr) {
                        /* Region continue. */
                        region_bytes += GENCGC_PAGE_BYTES;
                    } else {
                        /* Unprotect previous region. */
                        os_protect(region_addr, region_bytes, OS_VM_PROT_ALL);
                        /* First page in new region. */
                        region_addr = page_addr;
                        region_bytes = GENCGC_PAGE_BYTES;
                    }
                }
            }
        }
    }
    if (region_addr) {
        /* Unprotect last region. */
        os_protect(region_addr, region_bytes, OS_VM_PROT_ALL);
    }
#endif
}

/* Work through all the pages and free any in from_space.
 * Live non-pinned objects will have been copied to new pages.
 * Pinned objects are no longer in 'from_space', as the containing
 * page is now in a different generation.
 * Bytes_allocated and the generation bytes_allocated
 * counter are updated. */
static void free_oldspace(void)
{
    uword_t bytes_freed = 0;
    page_index_t page;
    for (page = 0; page < next_free_page; ++page) {
        if (page_table[page].gen == from_space) {
            /* Should already be unprotected by unprotect_oldspace(). */
            gc_dcheck(page_cards_all_marked_nonsticky(page));
            /* Free the page. */
            int used = page_words_used(page);
            if (used) set_page_need_to_zero(page, 1);
            set_page_bytes_used(page, 0);
            reset_page_flags(page);
            bytes_freed += used << WORD_SHIFT;
        }
    }
    generations[from_space].bytes_allocated -= bytes_freed;
    bytes_allocated -= bytes_freed;
}
void free_large_object(lispobj* where, lispobj* end)
{
    page_index_t first = find_page_index(where);
    page_index_t last = find_page_index((char*)end - 1);
    generation_index_t g = page_table[first].gen;
    gc_assert(page_ends_contiguous_block_p(last, g));
    uword_t bytes_freed = 0;
    page_index_t page;
    // Perform all assertions before clobbering anything
    for (page = first ; page <= last ; ++page) {
        gc_assert(page_single_obj_p(page)); // redundant for the first page
        gc_assert(page_table[page].gen == g); // also redundant
        gc_assert(page_scan_start(page) == where);
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        gc_dcheck(page_cards_all_marked_nonsticky(page));
#else
        /* Force page to be writable. As much as memory faults should not occur
         * during GC, they are allowed, and this step will ensure writability. */
        *page_address(page) = 0;
#endif
    }
    // Copied from free_oldspace
    for (page = first ; page <= last ; ++page) {
        int used = page_words_used(page);
        if (used) set_page_need_to_zero(page, 1);
        set_page_bytes_used(page, 0);
        reset_page_flags(page);
        bytes_freed += used << WORD_SHIFT;
    }
    generations[g].bytes_allocated -= bytes_freed;
    bytes_allocated -= bytes_freed;
}

/* Call 'proc' with pairs of addresses demarcating ranges in the
 * specified generation.
 * Stop if any invocation returns non-zero, and return that value */
uword_t
walk_generation(uword_t (*proc)(lispobj*,lispobj*,uword_t),
                generation_index_t generation, uword_t extra)
{
    page_index_t i;
    int genmask = generation >= 0 ? 1 << generation : ~0;

    for (i = 0; i < next_free_page; i++) {
        if ((page_words_used(i) != 0) && ((1 << page_table[i].gen) & genmask)) {
            page_index_t last_page;

            /* This should be the start of a contiguous block */
            gc_assert(page_starts_contiguous_block_p(i));

            /* Need to find the full extent of this contiguous block in case
               objects span pages. */

            /* Now work forward until the end of this contiguous area is
               found. */
            for (last_page = i; ;last_page++)
                /* Check whether this is the last page in this contiguous
                 * block. */
                if (page_ends_contiguous_block_p(last_page, page_table[i].gen))
                    break;

            uword_t result =
                proc((lispobj*)page_address(i),
                     (lispobj*)page_address(last_page) + page_words_used(last_page),
                     extra);
            if (result) return result;

            i = last_page;
        }
    }
    return 0;
}


/* Write-protect all the dynamic boxed pages in the given generation. */
static void
write_protect_generation_pages(generation_index_t generation)
{
    // Neither 0 nor scratch can be protected. Additionally, protection of
    // pseudo-static space is applied only in gc_load_corefile_ptes().
    gc_assert(generation != 0 && generation != SCRATCH_GENERATION
              && generation != PSEUDO_STATIC_GENERATION);

#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    page_index_t page;
    for (page = 0; page < next_free_page; ++page) {
        if (page_table[page].gen == generation && page_boxed_p(page)
            && page_words_used(page)) {
            long card = page_to_card_index(page);
            int j;
            // must not touch a card referenced from the control stack
            // because the next instruction executed by user code
            // might store an old->young pointer.
            // There's probably a clever SIMD-in-a-register algorithm for this...
            for (j=0; j<CARDS_PER_PAGE; ++j, card++)
                if (gc_card_mark[card] != STICKY_MARK) gc_card_mark[card] = CARD_UNMARKED;
        }
    }
#else
    page_index_t start = 0, end;
    int n_hw_prot = 0, n_sw_prot = 0;

    while (start  < next_free_page) {
        if (!protect_page_p(start, generation)) {
            ++start;
            continue;
        }
        if (protection_mode(start) == LOGICAL) {
            SET_PAGE_PROTECTED(start, 1);
            ++n_sw_prot;
            ++start;
            continue;
        }

        /* Note the page as protected in the page tables. */
        SET_PAGE_PROTECTED(start, 1);

        /* Find the extent of pages desiring physical protection */
        for (end = start + 1; end < next_free_page; end++) {
            if (!protect_page_p(end, generation) || protection_mode(end) == LOGICAL)
                break;
            SET_PAGE_PROTECTED(end, 1);
        }

        n_hw_prot += end - start;
        os_protect(page_address(start), npage_bytes(end - start), OS_VM_PROT_READ);

        start = end;
    }

    if (gencgc_verbose > 1) {
        printf("HW protected %d, SW protected %d\n", n_hw_prot, n_sw_prot);
    }
#endif
}

static void
move_pinned_pages_to_newspace()
{
    page_index_t i;

    /* scavenge() will evacuate all oldspace pages, but no newspace
     * pages.  Pinned pages are precisely those pages which must not
     * be evacuated, so move them to newspace directly. */

    for (i = 0; i < next_free_page; i++) {
        /* 'pinned' is cleared lazily, so test the 'gen' field as well. */
        if (gc_page_pins[i] == PAGE_PINNED &&
            page_table[i].gen == from_space &&
            (page_single_obj_p(i) ||
             (is_code(page_table[i].type) && pin_all_dynamic_space_code))) {
            page_table[i].gen = new_space;
            /* And since we're moving the pages wholesale, also adjust
             * the generation allocation counters. */
            page_bytes_t used = page_bytes_used(i);
            generations[new_space].bytes_allocated += used;
            generations[from_space].bytes_allocated -= used;
        }
    }
}

static void __attribute__((unused)) maybe_pin_code(lispobj addr) {
    page_index_t page = find_page_index((char*)addr);

    if (page < 0) {
        if (immobile_space_p(addr))
            immobile_space_preserve_pointer((void*)addr);
        return;
    }
    if (immune_set_memberp(page)) return;

    struct code* code = (struct code*)dynamic_space_code_from_pc((char *)addr);
    if (code) {
        pin_exact_root(make_lispobj(code, OTHER_POINTER_LOWTAG));
    }
}

#if defined reg_RA
static void conservative_pin_code_from_return_addresses(struct thread* th) {
    lispobj *object_ptr;
    // We need more information to reliably backtrace through a call
    // chain, as these backends may generate leaf functions where the
    // return address does not get spilled. Therefore, fall back to
    // scanning the entire stack for potential interior code pointers.
    for (object_ptr = th->control_stack_start;
         object_ptr < access_control_stack_pointer(th);
         object_ptr++)
        maybe_pin_code(*object_ptr);
    int i = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th));
    // Scan program counters and return registers in interrupted
    // frames: They may contain interior code pointers that weren't
    // spilled onto the stack, as is the case for leaf functions.
    for (i = i - 1; i >= 0; --i) {
        os_context_t* context = nth_interrupt_context(i, th);
        maybe_pin_code(os_context_pc(context));
        maybe_pin_code((lispobj)*os_context_register_addr(context, reg_RA));
    }
}
#endif

#if defined LISP_FEATURE_MIPS || defined LISP_FEATURE_PPC64
static void semiconservative_pin_stack(struct thread* th,
                                       generation_index_t gen) {
    /* Stack can only pin code, since it contains return addresses.
     * Non-code pointers on stack do *not* pin anything, and may be updated
     * when scavenging.
     * Interrupt contexts' boxed registers do pin their referents */
    lispobj *object_ptr;
    for (object_ptr = th->control_stack_start;
         object_ptr < access_control_stack_pointer(th);
         object_ptr++)
        maybe_pin_code(*object_ptr);
    int i = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th));
    for (i = i - 1; i >= 0; --i) {
        os_context_t* context = nth_interrupt_context(i, th);
        int j;
#if defined LISP_FEATURE_MIPS
        mcontext_t *mctx = &context->uc_mcontext;
        for(j=1; j<32; ++j) {
            // context registers have more significant bits than lispobj.
            uword_t word = mctx->gregs[j];
            if (gen == 0) sticky_preserve_pointer(word, (void*)1);
            else preserve_pointer(word, (void*)1);
        }
#elif defined LISP_FEATURE_PPC64
        static int boxed_registers[] = BOXED_REGISTERS;
        for (j = (int)(sizeof boxed_registers / sizeof boxed_registers[0])-1; j >= 0; --j) {
            lispobj word = *os_context_register_addr(context, boxed_registers[j]);
            if (gen == 0) sticky_preserve_pointer(word, (void*)1);
            else preserve_pointer(word, (void*)1);
        }
        // What kinds of data do we put in the Count register?
        // maybe it's count (raw word), maybe it's a PC. I just don't know.
        preserve_pointer(*os_context_lr_addr(context), (void*)1);
        preserve_pointer(*os_context_ctr_addr(context), (void*)1);
#endif
        preserve_pointer(os_context_pc(context), (void*)1);
    }
}
#endif

#if GENCGC_IS_PRECISE && !defined(reg_CODE)

static int boxed_registers[] = BOXED_REGISTERS;

/* Pin all (condemned) code objects pointed to by the chain of in-flight calls
 * based on scanning from the innermost frame pointer. This relies on an exact backtrace,
 * which some of our architectures have trouble obtaining. But it's theoretically
 * more efficient to do it this way versus looking at all stack words to see
 * whether each points to a code object. */
static void pin_call_chain_and_boxed_registers(struct thread* th) {
    lispobj *cfp = access_control_frame_pointer(th);

    if (cfp) {
      while (1) {
        lispobj* ocfp = (lispobj *) cfp[0];
        lispobj lr = cfp[1];
        if (ocfp == 0)
            break;
        maybe_pin_code(lr);
        cfp = ocfp;
      }
    }
    int i = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th));
    for (i = i - 1; i >= 0; --i) {
        os_context_t* context = nth_interrupt_context(i, th);
        maybe_pin_code((lispobj)*os_context_register_addr(context, reg_LR));

        for (unsigned i = 0; i < (sizeof(boxed_registers) / sizeof(int)); i++) {
            lispobj word = *os_context_register_addr(context, boxed_registers[i]);
            if (is_lisp_pointer(word)) {
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
                impart_mark_stickiness(word);
#endif
                pin_exact_root(word);
            }
        }
    }

}
#endif

#if !GENCGC_IS_PRECISE
extern void visit_context_registers(void (*proc)(os_context_register_t, void*),
                                    os_context_t *context, void*);
static void NO_SANITIZE_ADDRESS NO_SANITIZE_MEMORY
conservative_stack_scan(struct thread* th,
                        __attribute__((unused)) generation_index_t gen,
                        // #+sb-safepoint uses os_get_csp() and not this arg
                        __attribute__((unused)) lispobj* cur_thread_approx_stackptr)
{
    /* there are potentially two stacks for each thread: the main
     * stack, which may contain Lisp pointers, and the alternate stack.
     * We don't ever run Lisp code on the altstack, but it may
     * host a sigcontext with lisp objects in it.
     * Actually, STOP_FOR_GC has a signal context on the main stack,
     * and the values it in will be *above* the stack-pointer in it
     * at the point of interruption, so we would not scan all registers
     * unless the context is scanned.
     *
     * For the thread which initiates GC there will usually not be a
     * sigcontext, though there could, in theory be if it performs
     * GC while handling an interruption */

    __attribute__((unused)) void (*context_method)(os_context_register_t,void*) =
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        gen == 0 ? sticky_preserve_pointer : preserve_pointer;
#else
        preserve_pointer;
#endif

    void* esp = (void*)-1;
# if defined(LISP_FEATURE_SB_SAFEPOINT)
    /* Conservative collect_garbage is always invoked with a
     * foreign C call or an interrupt handler on top of every
     * existing thread, so the stored SP in each thread
     * structure is valid, no matter which thread we are looking
     * at.  For threads that were running Lisp code, the pitstop
     * and edge functions maintain this value within the
     * interrupt or exception handler. */
    esp = os_get_csp(th);
    assert_on_stack(th, esp);

    /* And on platforms with interrupts: scavenge ctx registers. */

    /* Disabled on Windows, because it does not have an explicit
     * stack of `interrupt_contexts'.  The reported CSP has been
     * chosen so that the current context on the stack is
     * covered by the stack scan.  See also set_csp_from_context(). */
#  ifndef LISP_FEATURE_WIN32
    if (th != get_sb_vm_thread()) {
        int k = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th));
        while (k > 0) {
            os_context_t* context = nth_interrupt_context(--k, th);
            if (context)
                visit_context_registers(context_method, context, (void*)1);
        }
    }
#  endif
# elif defined(LISP_FEATURE_SB_THREAD)
    int i;
    /* fprintf(stderr, "Thread %p, ici=%d stack[%p:%p] (%dw)",
            th, fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th)),
            th->control_stack_start, th->control_stack_end,
            th->control_stack_end - th->control_stack_start); */
    for (i = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th))-1; i>=0; i--) {
        os_context_t *c = nth_interrupt_context(i, th);
        visit_context_registers(context_method, c, (void*)1);
        lispobj* esp1 = (lispobj*) *os_context_register_addr(c,reg_SP);
        if (esp1 >= th->control_stack_start && esp1 < th->control_stack_end && (void*)esp1 < esp)
            esp = esp1;
    }
    if (th == get_sb_vm_thread()) {
        if ((void*)cur_thread_approx_stackptr < esp) esp = cur_thread_approx_stackptr;
    }
# else
    esp = cur_thread_approx_stackptr;
# endif
    if (!esp || esp == (void*) -1)
        UNKNOWN_STACK_POINTER_ERROR("garbage_collect", th);
    /* fprintf(stderr, " SP=%p (%dw)%s\n",
            esp, (int)(th->control_stack_end - (lispobj*)esp),
            (th == get_sb_vm_thread()) ? " CURRENT":""); */

    // Words on the stack which point into the stack are likely
    // frame pointers or alien or DX object pointers. In any case
    // there's no need to call preserve_pointer on them since
    // they definitely don't point to the heap.
    // See the picture at alloc_thread_struct() as a reminder.
#ifdef LISP_FEATURE_UNIX
    lispobj exclude_from = (lispobj)th->control_stack_start;
    lispobj exclude_to = (lispobj)th + dynamic_values_bytes;
#define potential_heap_pointer(word) !(exclude_from <= word && word < exclude_to)
#else
    // We can't use the heuristic of excluding words that appear to point into
    // 'struct thread' on win32 because ... I don't know why.
    // See https://groups.google.com/g/sbcl-devel/c/8s7mrapq56s/m/UaAjYPqKBAAJ
#define potential_heap_pointer(word) 1
#endif

    lispobj* ptr;
    for (ptr = esp; ptr < th->control_stack_end; ptr++) {
        lispobj word = *ptr;
        // Also note that we can eliminate small fixnums from consideration
        // since there is no memory on the 0th page.
        // (most OSes don't let users map memory there, though they used to).
        if (word >= BACKEND_PAGE_BYTES && potential_heap_pointer(word)) {
            preserve_pointer(word, 0);
        }
    }
}
#endif

static void scan_explicit_pins(__attribute__((unused)) struct thread* th)
{
    lispobj pin_list = read_TLS(PINNED_OBJECTS, th);
    for ( ; pin_list != NIL ; pin_list = CONS(pin_list)->cdr ) {
        lispobj object = CONS(pin_list)->car;
        pin_exact_root(object);
        if (lowtag_of(object) == INSTANCE_POINTER_LOWTAG) {
            struct instance* instance = INSTANCE(object);
            lispobj layout = instance_layout((lispobj*)instance);
            // Since we're still in the pinning phase of GC, layouts can't have moved yet,
            // so there is no forwarding check needed here.
            if (layout && lockfree_list_node_layout_p(LAYOUT(layout))) {
                /* A logically-deleted explicitly-pinned lockfree list node pins its
                 * successor too, since Lisp reconstructs the next node's tagged pointer
                 * from an untagged pointer currently stored in %NEXT of this node. */
                lispobj successor = ((struct list_node*)instance)->_node_next;
                // Be sure to ignore an uninitialized word containing 0.
                if (successor && fixnump(successor))
                    pin_exact_root(successor | INSTANCE_POINTER_LOWTAG);
            }
        }
    }
}

/* Given the slightly asymmetric formulation of page_ends_contiguous_block_p()
 * you might think that it could cause the next page's assertion about start_block_p()
 * to fail, but it does not seem to. That's really weird! */
__attribute__((unused)) static void check_contiguity()
{
      page_index_t first = 0;
      while (first < next_free_page) {
        if (!page_words_used(first)) { ++first; continue; }
        gc_assert(page_starts_contiguous_block_p(first));
        page_index_t last = first;
        while (!page_ends_contiguous_block_p(last, page_table[first].gen)) ++last;
        first = last + 1;
      }
}

static void finish_code_metadata();
int show_gc_generation_throughput = 0;
/* Garbage collect a generation. If raise is 0 then the remains of the
 * generation are not raised to the next generation. */
void NO_SANITIZE_ADDRESS NO_SANITIZE_MEMORY
garbage_collect_generation(generation_index_t generation, int raise,
                           void* cur_thread_approx_stackptr)
{
    struct thread *th;

    if (gencgc_verbose > 2) fprintf(stderr, "BEGIN gc_gen(%d,%d)\n", generation, raise);

#ifdef COLLECT_GC_STATS
    struct timespec t0;
    clock_gettime(CLOCK_MONOTONIC, &t0);
    uword_t gen_usage_at_start = generations[generation].bytes_allocated;
    uword_t higher_gen_usage_at_start =
      raise ? generations[generation+1].bytes_allocated : 0;
#endif

    gc_assert(generation <= PSEUDO_STATIC_GENERATION);

    /* The oldest generation can't be raised. */
    gc_assert(!raise || generation < HIGHEST_NORMAL_GENERATION);

    /* Check that weak hash tables were processed in the previous GC. */
    gc_assert(weak_hash_tables == NULL);

    /* When a generation is not being raised it is transported to a
     * temporary generation (NUM_GENERATIONS), and lowered when
     * done. Set up this new generation. There should be no pages
     * allocated to it yet. */
    if (!raise) {
         gc_assert(generations[SCRATCH_GENERATION].bytes_allocated == 0);
    }

    hopscotch_reset(&pinned_objects);

#ifdef LISP_FEATURE_SB_THREAD
    pin_all_dynamic_space_code = 0;
    for_each_thread(th) {
        if (th->state_word.state != STATE_DEAD && \
            (read_TLS(GC_PIN_CODE_PAGES, th) & make_fixnum(1))) {
            pin_all_dynamic_space_code = 1;
            break;
        }
    }
#else
    pin_all_dynamic_space_code = read_TLS(GC_PIN_CODE_PAGES, 0) & make_fixnum(1);
#endif

    /* Set the global src and dest. generations */
    generation_index_t original_alloc_generation = gc_alloc_generation;

    if (generation < PSEUDO_STATIC_GENERATION) {

        from_space = generation;
        if (raise)
            new_space = generation+1;
        else
            new_space = SCRATCH_GENERATION;

    /* Change to a new space for allocation, resetting the alloc_start_page */
        gc_alloc_generation = new_space;
        RESET_ALLOC_START_PAGES();

        if (pin_all_dynamic_space_code) {
          /* This needs to happen before ambiguous root pinning, as the mechanisms
           * overlap in a way that all-code pinning wouldn't do the right thing if flipped.
           * FIXME: why would it not? More explanation needed!
           * Code objects should never get into the pins table in this case */
            page_index_t i;
            for (i = 0; i < next_free_page; i++) {
                if (page_table[i].gen == from_space
                    && is_code(page_table[i].type) && page_words_used(i))
                    gc_page_pins[i] = PAGE_PINNED;
            }
        }

    /* Un-write-protect the old-space pages. This is essential for the
     * promoted pages as they may contain pointers into the old-space
     * which need to be scavenged. It also helps avoid unnecessary page
     * faults as forwarding pointers are written into them. They need to
     * be un-protected anyway before unmapping later. */
        unprotect_oldspace();

    } else { // "full" [sic] GC

        gc_assert(!pin_all_dynamic_space_code); // not supported (but could be)

        /* This is a full mark-and-sweep of all generations without compacting
         * and without returning free space to the allocator. The intent is to
         * break chains of objects causing accidental reachability.
         * Subsequent GC cycles will compact and reclaims space as usual. */
        from_space = new_space = -1;

        // Allocate pages from dynamic space for the work queue.
        extern void prepare_for_full_mark_phase();
        prepare_for_full_mark_phase();

    }

    /* Possibly pin stack roots and/or *PINNED-OBJECTS*, unless saving a core.
     * Scavenging (fixing up pointers) will occur later on */

    if (conservative_stack) {
        for_each_thread(th) {
            if (th->state_word.state == STATE_DEAD) continue;
            scan_explicit_pins(th);
#if !GENCGC_IS_PRECISE
            /* Pin everything in fromspace with a stack root, and also set the
             * sticky card mark on any page (in any generation)
             * referenced from the stack. */
            conservative_stack_scan(th, generation, cur_thread_approx_stackptr);
#elif defined LISP_FEATURE_MIPS || defined LISP_FEATURE_PPC64
            // Pin code if needed
            semiconservative_pin_stack(th, generation);
#elif defined REG_RA
            conservative_pin_code_from_return_addresses(th);
#elif !defined(reg_CODE)
            pin_call_chain_and_boxed_registers(th);
#endif
        }
    }

    // Thread creation optionally no longer synchronizes the creating and
    // created thread. When synchronized, the parent thread is responsible
    // for pinning the start function for handoff to the created thread.
    // When not synchronized, The startup parameters are pinned via this list
    // which will always be NIL if the feature is not enabled.
#ifdef STARTING_THREADS
    lispobj pin_list = SYMBOL(STARTING_THREADS)->value;
    for ( ; pin_list != NIL ; pin_list = CONS(pin_list)->cdr ) {
        lispobj thing = CONS(pin_list)->car;
        if (!thing) continue; // Nothing to worry about when 'thing' is already smashed
        // It might be tempting to say that only the SB-THREAD:THREAD instance
        // requires pinning - because right after we access it to extract the
        // primitive thread, we link into all_threads - but it may be that the code
        // emitted by the C compiler in new_thread_trampoline computes untagged pointers
        // when accessing the vector and the start function, so those would not be
        // seen as valid lisp pointers by the implicit pinning logic.
        // And the precisely GC'd platforms would not pin anything from C code.
        // The tests in 'threads.impure.lisp' are good at detecting omissions here.
        gc_assert(instancep(thing));
        struct thread_instance *lispthread = (void*)(thing - INSTANCE_POINTER_LOWTAG);
        lispobj info = lispthread->startup_info;
        // INFO gets set to a fixnum when the thread is exiting. I *think* it won't
        // ever be seen in the starting-threads list, but let's be cautious.
        if (is_lisp_pointer(info)) {
            gc_assert(simple_vector_p(info));
            gc_assert(vector_len(VECTOR(info)) >= 1);
            lispobj fun = VECTOR(info)->data[0];
            gc_assert(functionp(fun));
#ifdef LISP_FEATURE_X86_64
                /* FIXME: re. the following remark that pin_exact_root() "does not
                 * work", does it have to be that way? It seems the issue is that
                 * pin_exact_root does absolutely nothing for objects in immobile space.
                 * Are there other objects we call it on which could be in immobile-space
                 * and should it be made to deal with them? */
                // slight KLUDGE: 'fun' is a simple-fun in immobile-space,
                // and pin_exact_root() doesn't work. In all probability 'fun'
                // is pseudo-static, but let's use the right pinning function.
                // (This line of code is so rarely executed that it doesn't
                // impact performance to search for the object)
            preserve_pointer(fun, 0);
#else
            pin_exact_root(fun);
#endif
            // pin_exact_root is more efficient than preserve_pointer()
            // because it does not search for the object.
            pin_exact_root(thing);
            pin_exact_root(info);
        }
    }
#endif

    /* Remove any key from pinned_objects this does not identify an object.
     * This is done more efficiently by delaying until after all keys are
     * inserted rather than at each insertion */
    refine_ambiguous_roots();

    if (gencgc_verbose > 1) {
        extern void dump_marked_objects();
        if (compacting_p()) show_pinnedobj_count(); /*else dump_marked_objects();*/
    }

    /* Now that all of the pinned pages are known, and
     * before we start to scavenge (and thus relocate) objects,
     * relocate the pinned pages to newspace, so that the scavenger
     * will not attempt to relocate their contents. */
    if (compacting_p())
        move_pinned_pages_to_newspace();

    /* Scavenge all the rest of the roots. */

#if GENCGC_IS_PRECISE
    /*
     * If not x86, we need to scavenge the interrupt context(s) and the
     * control stack, unless in final GC then don't.
     */
    if (conservative_stack) {
        struct thread *th;
        for_each_thread(th) {
#if !defined(LISP_FEATURE_MIPS) && defined(reg_CODE) // interrupt contexts already pinned everything they see
            scavenge_interrupt_contexts(th);
#endif
            scavenge_control_stack(th);
        }

# ifdef LISP_FEATURE_SB_SAFEPOINT
        /* In this case, scrub all stacks right here from the GCing thread
         * instead of doing what the comment below says.  Suboptimal, but
         * easier. */
        for_each_thread(th)
            scrub_thread_control_stack(th);
# else
        /* Scrub the unscavenged control stack space, so that we can't run
         * into any stale pointers in a later GC (this is done by the
         * stop-for-gc handler in the other threads). */
        scrub_control_stack();
# endif
    }
#endif

    /* Scavenge the Lisp functions of the interrupt handlers */
    if (GC_LOGGING) fprintf(gc_activitylog(), "begin scavenge sighandlers\n");
    if (compacting_p())
        scavenge(lisp_sig_handlers, NSIG);
    else
        gc_mark_range(lisp_sig_handlers, NSIG);

    /* Scavenge the binding stacks. */
    if (GC_LOGGING) fprintf(gc_activitylog(), "begin scavenge thread roots\n");
    {
        struct thread *th;
        for_each_thread(th) {
            scav_binding_stack((lispobj*)th->binding_stack_start,
                               (lispobj*)get_binding_stack_pointer(th),
                               compacting_p() ? 0 : gc_mark_obj);
            /* do the tls as well */
            lispobj* from = &th->lisp_thread;
            lispobj* to = (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th);
            sword_t nwords = to - from;
            if (compacting_p())
                scavenge(from, nwords);
            else
                gc_mark_range(from, nwords);
        }
    }

    if (!compacting_p()) {
#ifdef LISP_FEATURE_PERMGEN
        remember_all_permgen();
#endif
        extern void execute_full_mark_phase();
        extern void execute_full_sweep_phase();
        execute_full_mark_phase();
        execute_full_sweep_phase();
        goto maybe_verify;
    }

    if (GC_LOGGING) fprintf(gc_activitylog(), "begin scavenge static roots\n");
    heap_scavenge((lispobj*)NIL_SYMBOL_SLOTS_START, (lispobj*)NIL_SYMBOL_SLOTS_END);
    heap_scavenge((lispobj*)STATIC_SPACE_OBJECTS_START, static_space_free_pointer);
#ifdef LISP_FEATURE_PERMGEN
    // Remembered objects below the core permgen end, and all objects above it, are roots.
    heap_scavenge((lispobj*)permgen_bounds[1], permgen_space_free_pointer);
    int i, n = permgen_remset_count;
    for (i=0; i<n; ++i) {
        lispobj* o = native_pointer(permgen_remset[i]);
        heap_scavenge(o, object_size(o)+o);
    }
#endif
#ifdef LISP_FEATURE_LINKAGE_SPACE
    extern void scavenge_elf_linkage_space();
    scavenge_elf_linkage_space();
#endif
#ifndef LISP_FEATURE_IMMOBILE_SPACE
    // TODO: use an explicit remembered set of modified objects in this range
    if (TEXT_SPACE_START) heap_scavenge((lispobj*)TEXT_SPACE_START, text_space_highwatermark);
#endif
#ifdef LISP_FEATURE_SYSTEM_TLABS
    extern void gc_scavenge_arenas();
    gc_scavenge_arenas();
#endif

    /* All generations but the generation being GCed need to be
     * scavenged. The new_space generation needs special handling as
     * objects may be moved in - it is handled separately below. */

    // SCRATCH_GENERATION is scavenged in immobile space
    // because pinned objects will already have had their generation
    // number reassigned to that generation if applicable.
    scavenge_immobile_roots(generation+1, SCRATCH_GENERATION);

    // When collecting gen0, ordinarily the roots would be gen1 and higher,
    // but if gen0 is getting raised to 1 on this cycle, then we skip roots in gen1
    // because we'll eventually examine all of gen1 as part of newspace.
    // Similarly for higher generations. So if raising, the minimum root gen is
    // always the collected generation + 2, otherwise it's the collected + 1.
    if (GC_LOGGING) fprintf(gc_activitylog(), "begin scavenge_root_gens\n");
    scavenge_root_gens(generation+1+raise);
    scavenge_pinned_ranges();
    /* The Lisp start function is stored in the core header, not a static
     * symbol. It is passed to gc_and_save() in this C variable */
    if (lisp_init_function) scavenge(&lisp_init_function, 1);
    if (lisp_package_vector) scavenge(&lisp_package_vector, 1);
    if (alloc_profile_data) scavenge(&alloc_profile_data, 1);

    /* If SB-SPROF was used, enliven all pages of code.
     * Note that some objects may have already been transported off the page.
     * Despite the extra scan, it is more efficient than scanning all trace buffers
     * and potentially updating them and/or invalidating hashes.
     * This really wants a better algorithm. Each code blob could have one byte
     * indicating whether it is present in any trace buffer; the SIGPROF handler
     * can update that byte. */
    if (sb_sprof_enabled) {
        page_index_t first = 0;
        while (first < next_free_page) {
            if (page_table[first].gen != from_space
                || !is_code(page_table[first].type)
                || !page_words_used(first)) {
                ++first;
                continue;
            }
            page_index_t last = first;
            while (!page_ends_contiguous_block_p(last, from_space)) ++last;
            // [first,last] are inclusive bounds on a code range
            /* FIXME: should 'where' be initialized to page_scan_start()? I think so,
             * because ends_contiguous_block(page-1) does NOT imply
             * starts_contiguous_block(page). This is very unfortunate.
             * I've seen things such as the following:
             * page base: 0x20000  0x21000  0x22000
             *      used:    1000       10        0
             *       ss:  0x20000  0x20000  0x21010
             * where the first two pages were opened together and then closed
             * after consuming all of the first + 0x10 bytes more, and then the next
             * page extends the region (so not to waste the entire rest of the second
             * page), pointing its scan_start to the end of the range that was updated
             * into the page table. In that scenario, ends_p() is true of the page
             * based at 0x21000 but starts_p() is false of the next page,
             * because its scan start is an earlier page than itself.
             * How does this assertion NOT fail sometimes? Yet, it does not. */
            gc_assert(page_starts_contiguous_block_p(first));
            lispobj* where = (lispobj*)page_address(first);
            lispobj* limit = (lispobj*)page_address(last) + page_words_used(last);
            while (where < limit) {
                if (forwarding_pointer_p(where)) {
                    // The codeblob already survived GC, so we just need to step over it.
                    lispobj* copy = native_pointer(forwarding_pointer_value(where));
                    // NOTE: it's OK to size the newspace copy rather than the original
                    // because code size can't change.
                    where += headerobj_size(copy);
                } else {
                    // Compute 'nwords' before potentially moving the object
                    // at 'where', because moving it stomps on the header word.
                    sword_t nwords = headerobj_size(where);
                    // If the object is not a filler and not a trampline, then create
                    // a pointer to it and eliven the pointee.
                    if (widetag_of(where) == CODE_HEADER_WIDETAG
                        && where[1] != 0 /* has at least one boxed word */
                        && code_serialno((struct code*)where) != 0) {
                        lispobj ptr = make_lispobj(where, OTHER_POINTER_LOWTAG);
                        scavenge(&ptr, 1);
                    }
                    where += nwords;
                }
            }
            first = last + 1;
        }
    }

    /* Finally scavenge the new_space generation. Keep going until no
     * more objects are moved into the new generation */
    scavenge_newspace(new_space);
    if (save_lisp_gc_iteration == 2) finish_code_metadata();

    scan_binding_stack();
    smash_weak_pointers();
    /* Return private-use pages to the general pool so that Lisp can have them */
    gc_dispose_private_pages();
    cull_weak_hash_tables(weak_ht_alivep_funs);
    scan_finalizers();

    obliterate_nonpinned_words();
    // Do this last, because until obliterate_nonpinned_words() happens,
    // not all page table entries have the 'gen' value updated,
    // which we need to correctly find all old->young pointers.
    sweep_immobile_space(raise);

    ASSERT_REGIONS_CLOSED();
    hopscotch_log_stats(&pinned_objects, "pins");

    free_oldspace();

    /* If this cycle was not a promotion cycle, change SCRATCH_GENERATION back
     * to its correct generation number */
    struct generation* g = &generations[generation];
    if (!raise) {
        page_index_t i;
        for (i = 0; i < next_free_page; i++)
            if (page_table[i].gen == SCRATCH_GENERATION) page_table[i].gen = generation;
        gc_assert(g->bytes_allocated == 0);
        g->bytes_allocated = generations[SCRATCH_GENERATION].bytes_allocated;
        generations[SCRATCH_GENERATION].bytes_allocated = 0;
    }
#ifdef COLLECT_GC_STATS
    if (show_gc_generation_throughput) {
    struct timespec t1;
    clock_gettime(CLOCK_MONOTONIC, &t1);
    long et_nsec = (t1.tv_sec - t0.tv_sec)*1000000000 + (t1.tv_nsec - t0.tv_nsec);
    sword_t bytes_retained, bytes_freed;
    if (raise) {
      bytes_retained = (generations[generation+1].bytes_allocated
                        - higher_gen_usage_at_start);
    } else {
      bytes_retained = generations[generation].bytes_allocated;
    }
    bytes_freed = gen_usage_at_start - bytes_retained;

    double pct_freed = gen_usage_at_start ? (double)bytes_freed / gen_usage_at_start : 0.0;
    double et_sec = (double)et_nsec / 1000000000.0;
    double speed = (double)(gc_copied_nwords << WORD_SHIFT) / 1024 / et_sec;
    char *units = "KiB";
    if (speed > 1024.0) speed /= 1024.0, units = "MiB";
    /* The pre-GC bytes allocated should sum to copied + pinned + freed, which it
     * more-or-less does, but there can be discrepancies because structure instances
     * can be extended with a stable-hash slot (which isn't accounted for at all),
     * vectors can be shrunk (part being "freed" and part being "copied", depending
     * on the size and partial pinning),and the finalizer hash-table can have cons
     * cells allocated to record the list of functions to call.
     * In particular, there could be 0 usage before, and some usage after due to
     * the finalizer table, which causes "freed" to be negative.
     * While those factors could be accounted for in the report, it would be needlessly
     * pedantic and confusing, and not really affect the big picture.
     * If the MiB per sec is low, it could be that not many bytes were copied.
     * Low speed + large count is bad though */
    char buffer[200];
    // can't use fprintf() inside GC because of malloc. snprintf() can deadlock too,
    // but seems to do so much less often.
    int n = snprintf(buffer, sizeof buffer,
                     "gen%d: %ldw copied in %f sec (%.0f %s/sec), %ldw in-situ,"
                     " %d pins (%ldw), %ldw freed (%.1f%%)\n",
                     generation, gc_copied_nwords, et_sec, speed, units,
                     gc_in_situ_live_nwords,
                     gc_pin_count, gc_pinned_nwords,
                     bytes_freed >> WORD_SHIFT, pct_freed*100.0);
    write(2, buffer, n);
    n = snprintf(buffer, sizeof buffer,
                 "root word counts: %d + %d + %d\n", root_boxed_words_scanned,
                 root_vector_words_scanned, root_mixed_words_scanned);
    write(2, buffer, n);
    }
    gc_copied_nwords = gc_in_situ_live_nwords = gc_pinned_nwords = 0;
    root_boxed_words_scanned = root_vector_words_scanned = root_mixed_words_scanned = 0;
#endif

    /* Reset the alloc_start_page for generation. */
    RESET_ALLOC_START_PAGES();

    /* Set the new gc trigger for the GCed generation. */
    g->gc_trigger = g->bytes_allocated + g->bytes_consed_between_gc;
    g->num_gc = raise ? 0 : (1 + g->num_gc);

maybe_verify:
    // Have to kill this structure from its root, because any of the nodes would have
    // been on pages that got freed by free_oldspace.
    dynspace_codeblob_tree_snapshot = 0;
    if (generation >= verify_gens)
        hexdump_and_verify_heap(cur_thread_approx_stackptr,
                                VERIFY_POST_GC | (generation<<1) | raise);

    extern int n_unboxed_instances;
    n_unboxed_instances = 0;
    gc_alloc_generation = original_alloc_generation;
}

static page_index_t
find_next_free_page(void)
{
    page_index_t last_page = -1, i;

    for (i = 0; i < next_free_page; i++)
        if (page_words_used(i) != 0)
            last_page = i;

    /* 1 page beyond the last used page is the next free page */
    return last_page + 1;
}

generation_index_t small_generation_limit = 1;

extern int finalizer_thread_runflag;

/* GC all generations newer than last_gen, raising the objects in each
 * to the next older generation - we finish when all generations below
 * last_gen are empty.  Then if last_gen is due for a GC, or if
 * last_gen==NUM_GENERATIONS (the scratch generation?  eh?) we GC that
 * too.  The valid range for last_gen is: 0,1,...,NUM_GENERATIONS.
 *
 * We stop collecting at gencgc_oldest_gen_to_gc, even if this is less than
 * last_gen (oh, and note that by default it is NUM_GENERATIONS-1) */
long tot_gc_nsec;
void NO_SANITIZE_ADDRESS NO_SANITIZE_MEMORY
collect_garbage(generation_index_t last_gen)
{
    ++n_lisp_gcs;
    THREAD_JIT_WP(0);
    generation_index_t gen = 0, i;
    bool gc_mark_only = 0;
    int raise, more = 0;
    int gen_to_wp;
    /* The largest value of next_free_page seen since the time
     * remap_free_pages was called. */
    static page_index_t high_water_mark = 0;

#ifdef COLLECT_GC_STATS
    struct timespec t_gc_start;
    clock_gettime(CLOCK_MONOTONIC, &t_gc_start);
#endif
    log_generation_stats(gc_logfile, "=== GC Start ===");

    gc_active_p = 1;

    if (last_gen == 1+PSEUDO_STATIC_GENERATION) {
        // Pseudostatic space undergoes a non-moving collection
        last_gen = PSEUDO_STATIC_GENERATION;
        gc_mark_only = 1;
    } else if (last_gen > 1+PSEUDO_STATIC_GENERATION) {
        // This is a completely non-obvious thing to do, but whatever...
        last_gen = 0;
    }

    /* Flush the alloc regions updating the page table.
     *
     * GC is single-threaded and all memory allocations during a collection
     * happen in the GC thread, so it is sufficient to update PTEs for the
     * per-thread regions exactly once at the beginning of a collection
     * and update only from the GC's regions thereafter during collection.
     *
     * The GC's regions are probably empty already, except:
     * - The code region is shared across all threads
     * - The boxed region is used in lieu of thread-specific regions
     *   in a unithread build.
     * So we need to close them for those two cases.
     */
    struct thread *th;
    for_each_thread(th) {
        gc_close_thread_regions(th, 0);
#ifdef LISP_FEATURE_PERMGEN
        // transfer the thread-local remset to the global remset
        remset_union(th->remset);
        th->remset = 0;
#endif
    }
#ifdef LISP_FEATURE_PERMGEN
    // transfer the remsets from threads that exited
    remset_union(remset_transfer_list);
    remset_transfer_list = 0;
#endif

    ensure_region_closed(code_region, PAGE_TYPE_CODE);
    if (gencgc_verbose > 2) fprintf(stderr, "[%d] BEGIN gc(%d)\n", n_lisp_gcs, last_gen);

#ifdef LISP_FEATURE_IMMOBILE_SPACE
  if (ENABLE_PAGE_PROTECTION) {
      // Unprotect the in-use ranges. Any page could be written during scavenge
      os_protect((os_vm_address_t)FIXEDOBJ_SPACE_START,
                 (lispobj)fixedobj_free_pointer - FIXEDOBJ_SPACE_START,
                 OS_VM_PROT_ALL);
  }
#endif

    lispobj* cur_thread_approx_stackptr =
        (lispobj*)ALIGN_DOWN((uword_t)&last_gen, N_WORD_BYTES);
    /* Verify the new objects created by Lisp code. */
    if (pre_verify_gen_0)
        hexdump_and_verify_heap(cur_thread_approx_stackptr, VERIFY_PRE_GC);

    if (gencgc_verbose > 1) {
        fprintf(stderr, "Pre-GC:\n");
        print_generation_stats();
    }

    /* After a GC, pages of code are safe to linearly scan because
     * there won't be random junk on them below page_bytes_used.
     * But generation 0 pages are _not_ safe to linearly scan because they aren't
     * pre-zeroed. The SIGPROF handler could have a bad time if were to misread
     * the header of an object mid-creation. Therefore, codeblobs newly made by Lisp
     * are kept in a lock-free and threadsafe datastructure. But we don't want to
     * enliven nodes of that structure for Lisp to see (absent any other references)
     * because the whole thing becomes garbage after this GC. So capture the tree
     * for GC's benefit, and delete the view of it from Lisp.
     * Incidentally, immobile text pages have their own tree, for other purposes
     * (among them being to find page scan start offsets) which is pruned as
     * needed by a finalizer. */
    dynspace_codeblob_tree_snapshot = SYMBOL(DYNSPACE_CODEBLOB_TREE)->value;
    SYMBOL(DYNSPACE_CODEBLOB_TREE)->value = NIL;

    page_index_t initial_nfp = next_free_page;
    if (gc_mark_only) {
        garbage_collect_generation(PSEUDO_STATIC_GENERATION, 0,
                                   cur_thread_approx_stackptr);
        goto finish;
    }

    do {
        /* Collect the generation. */

        if (more || (gen >= gencgc_oldest_gen_to_gc)) {
            /* Never raise the oldest generation. Never raise the extra generation
             * collected due to more-flag. */
            raise = 0;
            more = 0;
        } else {
            raise =
                (gen < last_gen)
                || (generations[gen].num_gc >= generations[gen].number_of_gcs_before_promotion);
            /* If we would not normally raise this one, but we're
             * running low on space in comparison to the object-sizes
             * we've been seeing, raise it and collect the next one
             * too. */
            if (!raise && gen == last_gen) {
                more = (2*large_allocation) >= (dynamic_space_size - bytes_allocated);
                raise = more;
            }
        }

        /* If an older generation is being filled, then update its
         * memory age. */
        if (raise == 1) {
            generations[gen+1].cum_sum_bytes_allocated +=
                generations[gen+1].bytes_allocated;
        }

        garbage_collect_generation(gen, raise, cur_thread_approx_stackptr);

        /* Reset the memory age cum_sum. */
        generations[gen].cum_sum_bytes_allocated = 0;

        if (gencgc_verbose > 1) {
            fprintf(stderr, "Post-GC(gen=%d):\n", gen);
            print_generation_stats();
        }

        gen++;
    } while ((gen <= gencgc_oldest_gen_to_gc)
             && ((gen < last_gen)
                 || more
                 || (raise
                     && (generations[gen].bytes_allocated
                         > generations[gen].gc_trigger)
                     && (generation_average_age(gen)
                         > generations[gen].minimum_age_before_gc))));

    /* Now if gen-1 was raised all generations before gen are empty.
     * If it wasn't raised then all generations before gen-1 are empty.
     *
     * Now objects within this gen's pages cannot point to younger
     * generations unless they are written to. This can be exploited
     * by write-protecting the pages of gen; then when younger
     * generations are GCed only the pages which have been written
     * need scanning. */
    if (raise)
        gen_to_wp = gen;
    else
        gen_to_wp = gen - 1;

    /* There's not much point in WPing pages in generation 0 as it is
     * never scavenged (except promoted pages). */
    if ((gen_to_wp > 0) && ENABLE_PAGE_PROTECTION) {
        /* Check that they are all empty. */
        for (i = 0; i < gen_to_wp; i++) {
            if (generations[i].bytes_allocated)
                lose("trying to write-protect gen. %d when gen. %d nonempty",
                     gen_to_wp, i);
        }
        write_protect_generation_pages(gen_to_wp);
    }
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    {
    // Turn sticky cards marks to the regular mark.
    page_index_t page;
    for (page=0; page<next_free_page; ++page) {
        long card = page_to_card_index(page);
        int j;
        for (j=0; j<CARDS_PER_PAGE; ++j, ++card)
            if (gc_card_mark[card] == STICKY_MARK) gc_card_mark[card] = CARD_MARKED;
    }
    }
#endif

    /* Save the high-water mark before updating next_free_page */
    if (next_free_page > high_water_mark)
        high_water_mark = next_free_page;

    next_free_page = find_next_free_page();

    /* Update auto_gc_trigger. Make sure we trigger the next GC before
     * running out of heap! */
    if (bytes_consed_between_gcs <= (dynamic_space_size - bytes_allocated))
        auto_gc_trigger = bytes_allocated + bytes_consed_between_gcs;
    else
        auto_gc_trigger = bytes_allocated + (dynamic_space_size - bytes_allocated)/2;

    if(gencgc_verbose) {
#define MESSAGE ("Next gc when %"OS_VM_SIZE_FMT" bytes have been consed\n")
        char buf[64];
        int n;
        // fprintf() can - and does - cause deadlock here.
        // snprintf() seems to work fine.
        n = snprintf(buf, sizeof buf, MESSAGE, (uintptr_t)auto_gc_trigger);
        ignore_value(write(2, buf, n));
#undef MESSAGE
    }

    /* If we did a big GC (arbitrarily defined as gen > 1), release memory
     * back to the OS.
     */
    if (gen > small_generation_limit) {
        if (next_free_page > high_water_mark)
            high_water_mark = next_free_page;
        // BUG? high_water_mark is the highest value of next_free_page,
        // which means that page_table[high_water_mark] was actually NOT ever
        // used, because next_free_page is an exclusive bound on the range
        // of pages used. But remap_free_pages takes to 'to' as an *inclusive*
        // bound. The only reason it's not an array overrun error is that
        // the page_table has one more element than there are pages.
        remap_free_pages(0, high_water_mark);
        high_water_mark = 0;
    }

    large_allocation = 0;
 finish:
    write_protect_immobile_space();
    gc_active_p = 0;

#ifdef COLLECT_GC_STATS
    struct timespec t_gc_done;
    clock_gettime(CLOCK_MONOTONIC, &t_gc_done);
    long et_nsec = (t_gc_done.tv_sec - t_gc_start.tv_sec)*1000000000
      + (t_gc_done.tv_nsec - t_gc_start.tv_nsec);
    tot_gc_nsec += et_nsec;
#endif

    log_generation_stats(gc_logfile, "=== GC End ===");
    // Increment the finalizer runflag.  This acts as a count of the number
    // of GCs as well as a notification to wake the finalizer thread.
    if (finalizer_thread_runflag != 0) {
        int newval = 1 + finalizer_thread_runflag;
        // check if counter wrapped around. Don't store 0 as the new value,
        // as that causes the thread to exit.
        finalizer_thread_runflag = newval ? newval : 1;
    }
    THREAD_JIT_WP(1);
    // Clear all pin bits for the next GC cycle.
    // This could be done in the background somehow maybe.
    page_index_t max_nfp = initial_nfp > next_free_page ? initial_nfp : next_free_page;
    memset(gc_page_pins, 0, max_nfp);
#ifdef LISP_FEATURE_LINKAGE_SPACE
    sweep_linkage_space();
#endif
    // It's confusing to see 'from_space=5' and such in the next *pre* GC verification
    from_space = -1;
    new_space = 0;
}

/* Initialization of gencgc metadata is split into two steps:
 * 1. gc_init() - allocation of a fixed-address space via mmap(),
 *    failing which there's no reason to go on. (safepoint only)
 * 2. gc_allocate_ptes() - page table entries
 */
void
gc_init(void)
{
    hopscotch_create(&pinned_objects, HOPSCOTCH_HASH_FUN_DEFAULT, 0 /* hashset */,
                     32 /* logical bin count */, 0 /* default range */);
#ifdef LISP_FEATURE_WIN32
    InitializeCriticalSection(&free_pages_lock);
#endif
}

int gc_card_table_nbits;
long gc_card_table_mask;


/* alloc() and alloc_list() are external interfaces for memory allocation.
 * They allocate to generation 0 and are not called from within the garbage
 * collector as it is only external uses that need the check for heap
 * size (GC trigger) and to disable the interrupts (interrupts are
 * always disabled during a GC).
 *
 * The vops that allocate assume that the returned space is zero-filled.
 * (E.g. the most significant word of a 2-word bignum in MOVE-FROM-UNSIGNED.)
 *
 * The check for a GC trigger is only performed when the current
 * region is full, so in most cases it's not needed. */

int gencgc_alloc_profiler;
NO_SANITIZE_MEMORY lispobj*
lisp_alloc(int flags, struct alloc_region *region, sword_t nbytes,
           int page_type, struct thread *thread)
{
    os_vm_size_t trigger_bytes = 0;

    gc_assert(nbytes > 0);

    /* Check for alignment allocation problems. */
    gc_assert((((uword_t)region->free_pointer & LOWTAG_MASK) == 0)
              && ((nbytes & LOWTAG_MASK) == 0));

#define SYSTEM_ALLOCATION_FLAG 2
#ifdef LISP_FEATURE_SYSTEM_TLABS
    lispobj* handle_arena_alloc(struct thread*, struct alloc_region *, int, sword_t);
    if (page_type != PAGE_TYPE_CODE && thread->arena && !(flags & SYSTEM_ALLOCATION_FLAG))
        return handle_arena_alloc(thread, region, page_type, nbytes);
#endif

    ++thread->slow_path_allocs;
    if ((os_vm_size_t) nbytes > large_allocation)
        large_allocation = nbytes;

    /* maybe we can do this quickly ... */
    /* I'd really like this "quick" case to be more uniform in terms of whether
     * it's allowed to occur at all. Some of the inconsistencies are:
     * - 32-bit x86 will (or would, not sure any more) choose to use
     *   out-of-line allocation if lexical policy favors space.
     * - PPC at git rev 28aaa39f4e had a subtle "but-not-wrong" bug at the edge
     *   where it trapped to C if the new free pointer was ':lge' instead of ':lgt'
     *   the region end, fixed in rev 05047647.
     * - other architectures may have similar issues.
     * So because of those reasons, even if we satisfy the allocation
     * from the TLAB it might be worth a check of whether to refill
     * the TLAB now. */
    void *new_obj = region->free_pointer;
    char *new_free_pointer = (char*)new_obj + nbytes;
    if (new_free_pointer <= (char*)region->end_addr) {
        region->free_pointer = new_free_pointer;
#if defined LISP_FEATURE_MIPS || defined LISP_FEATURE_PPC || \
    defined LISP_FEATURE_PPC64 || defined LISP_FEATURE_X86_64
        /* Most allocations should never get here, but two page types are special.
         * - CODE always comes through here.
         * - CONS can come through here because when overflow occurs in lisp,
         *   the fallback logic will call lisp_alloc one or more times,
         *   obtaining possibly discontiguous pages of conses */
        gc_assert(page_type == PAGE_TYPE_CONS || page_type == PAGE_TYPE_CODE);
#endif
        return new_obj;
    }

    /* We don't want to count nbytes against auto_gc_trigger unless we
     * have to: it speeds up the tenuring of objects and slows down
     * allocation. However, unless we do so when allocating _very_
     * large objects we are in danger of exhausting the heap without
     * running sufficient GCs.
     */
    if ((os_vm_size_t) nbytes >= bytes_consed_between_gcs)
        trigger_bytes = nbytes;

    /* we have to go the long way around, it seems. Check whether we
     * should GC in the near future
     */
    if (auto_gc_trigger && (bytes_allocated+trigger_bytes > auto_gc_trigger)) {
        /* Don't flood the system with interrupts if the need to gc is
         * already noted. This can happen for example when SUB-GC
         * allocates or after a gc triggered in a WITHOUT-GCING. */
        if (read_TLS(GC_PENDING,thread) == NIL) {
            /* set things up so that GC happens when we finish the PA
             * section */
            write_TLS(GC_PENDING, LISP_T, thread);
            if (read_TLS(GC_INHIBIT,thread) == NIL) {
#ifdef LISP_FEATURE_SB_SAFEPOINT
                thread_register_gc_trigger();
#else
                set_pseudo_atomic_interrupted(thread);
                maybe_save_gc_mask_and_block_deferrables
# if HAVE_ALLOCATION_TRAP_CONTEXT
                    (thread_interrupt_data(thread).allocation_trap_context);
# else
                    (0);
# endif
#endif
            }
        }
    }

    /* For the architectures which do NOT use a trap instruction for allocation,
     * overflow, record a backtrace now if statistical profiling is enabled.
     * The ones which use a trap will backtrace from the signal handler.
     * Code allocations are ignored, because every code allocation
     * comes through lisp_alloc() which makes this not a statistical
     * sample. Also the trapping ones don't trap for code.
     * #+win32 doesn't seem to work, but neither does CPU profiling */
#if !(defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64 \
      || defined LISP_FEATURE_SPARC || defined LISP_FEATURE_WIN32)
    extern void allocator_record_backtrace(void*, struct thread*);
    if (page_type != PAGE_TYPE_CODE && gencgc_alloc_profiler
        && thread->state_word.sprof_enable)
        allocator_record_backtrace(__builtin_frame_address(0), thread);
#endif

    if (flags & 1) return gc_alloc_large(nbytes, page_type);

    int __attribute__((unused)) ret = mutex_acquire(&free_pages_lock);
    gc_assert(ret);
    ensure_region_closed(region, page_type);
    // hold the lock after alloc_new_region if a cons page
    int release = page_type != PAGE_TYPE_CONS;
    new_obj = gc_alloc_new_region(nbytes, page_type, region, release);
    region->free_pointer = (char*)new_obj + nbytes;
    // addr_diff asserts that 'end' >= 'free_pointer'
    int remaining = addr_diff(region->end_addr, region->free_pointer);

    // System TLABs are not important to refill right away (in the nearly-empty case)
    // so put a high-enough number in 'remaining' to suppress the grab-another-page test
    if (flags & SYSTEM_ALLOCATION_FLAG) remaining = 256;

    // Try to avoid the next Lisp -> C -> Lisp round-trip by possibly
    // requesting yet another region.
    if (page_type == PAGE_TYPE_CONS) {
        if (remaining <= CONS_SIZE * N_WORD_BYTES) { // Refill now if <= 1 more cons to go
            gc_close_region(region, page_type);
            // Request > 2 words, forcing a new page to be claimed.
            gc_alloc_new_region(4 * N_WORD_BYTES, page_type, region, 0); // don't release
        }
        ret = mutex_release(&free_pages_lock);
        gc_assert(ret);
    } else if (remaining <= 4 * N_WORD_BYTES
               && TryEnterCriticalSection(&free_pages_lock)) {
        gc_close_region(region, page_type);
        // Request > 4 words, forcing a new page to be claimed.
        gc_alloc_new_region(6 * N_WORD_BYTES, page_type, region, 1); // do release
    }

    return new_obj;
}

#ifdef LISP_FEATURE_SPARC
void mixed_region_rollback(sword_t size)
{
    struct alloc_region *region = main_thread_mixed_region;
    gc_assert(region->free_pointer > region->end_addr);
    region->free_pointer = (char*)region->free_pointer - size;
    gc_assert(region->free_pointer >= region->start_addr
              && region->free_pointer <= region->end_addr);
}
#endif

/*
 * shared support for the OS-dependent signal handlers which
 * catch GENCGC-related write-protect violations
 */
void unhandled_sigmemoryfault(void* addr);

/* Depending on which OS we're running under, different signals might
 * be raised for a violation of write protection in the heap. This
 * function factors out the common generational GC magic which needs
 * to invoked in this case, and should be called from whatever signal
 * handler is appropriate for the OS we're running under.
 *
 * Return true if this signal is a normal generational GC thing that
 * we were able to handle, or false if it was abnormal and control
 * should fall through to the general SIGSEGV/SIGBUS/whatever logic.
 *
 * We have two control flags for this: one causes us to ignore faults
 * on unprotected pages completely, and the second complains to stderr
 * but allows us to continue without losing.
 */
extern bool ignore_memoryfaults_on_unprotected_pages;
bool ignore_memoryfaults_on_unprotected_pages = 0;

extern bool continue_after_memoryfault_on_unprotected_pages;
bool continue_after_memoryfault_on_unprotected_pages = 0;

int gencgc_handle_wp_violation(__attribute__((unused)) void* context, void* fault_addr)
{
    page_index_t page_index = find_page_index(fault_addr);

    /* Check whether the fault is within the dynamic space. */
    if (page_index == (-1)) {
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        extern int immobile_space_handle_wp_violation(void*);
        if (immobile_space_handle_wp_violation(fault_addr))
            return 1;
#endif

        /* It can be helpful to be able to put a breakpoint on this
         * case to help diagnose low-level problems. */
        unhandled_sigmemoryfault(fault_addr);

        /* not within the dynamic space -- not our responsibility */
        return 0;

    }
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    fake_foreign_function_call(context);
    lose("misuse of mprotect() on dynamic space @ %p", fault_addr);
#else
    // Pages of code are never have MMU-based protection, except on darwin,
    // where they do, but they are thread-locally-un-protected when creating
    // objets on those pages.
    gc_assert(!is_code(page_table[page_index].type));

    // There can not be an open region. gc_close_region() does not attempt
    // to flip that bit atomically. (What does this mean?)
    gc_assert(!(page_table[page_index].type & OPEN_REGION_PAGE_FLAG));

    // The collector should almost never incur page faults, but I haven't
    // found all the trouble spots. It may or may not be worth doing.
    // See git rev 8a0af65bfd24
    // if (gc_active_p && compacting_p()) lose("unexpected WP fault @ %p during GC", fault_addr);

    // Because this signal handler can not be interrupted by STOP_FOR_GC,
    // the only possible state change between reading the mark and deciding how
    // to proceed is due to another thread also unprotecting the address.
    // That's fine; in fact it's OK to read a stale value here.
    // The only harmful case would be where the mark byte says it was
    // never protected, and the fault occurred nonetheless. That can't happen.
    unsigned char mark = gc_card_mark[addr_to_card_index(fault_addr)];
    switch (mark) {
    case CARD_UNMARKED:
    case WP_CLEARED_AND_MARKED: // possible data race
        unprotect_page(fault_addr, WP_CLEARED_AND_MARKED);
        break;
    default:
        if (!ignore_memoryfaults_on_unprotected_pages) {
            void lisp_backtrace(int frames);
            lisp_backtrace(10);
            fprintf(stderr,
                    "Fault @ %p, PC=%p, page %"PAGE_INDEX_FMT" (~WP) mark=%#x gc_active=%d\n"
                    "  mixed_region=%p:%p\n"
                    "  page.scan_start: %p .words_used: %u .type: %d .gen: %d\n",
                    fault_addr, (void*)(context?os_context_pc(context):(uword_t)-1), page_index,
                    mark, gc_active_p,
                    mixed_region->start_addr, mixed_region->end_addr,
                    page_scan_start(page_index),
                    page_words_used(page_index),
                    page_table[page_index].type,
                    page_table[page_index].gen);
            if (!continue_after_memoryfault_on_unprotected_pages) lose("Feh.");
        }
    }
#endif
    return 1; // Handled
}
/* This is to be called when we catch a SIGSEGV/SIGBUS, determine that
 * it's not just a case of the program hitting the write barrier, and
 * are about to let Lisp deal with it. It's basically just a
 * convenient place to set a gdb breakpoint. */
void
unhandled_sigmemoryfault(void __attribute__((unused)) *addr)
{}

void zero_all_free_ranges() /* called only by gc_and_save() */
{
    page_index_t i;
    // gc_and_save() dumps at the granularity of "backend" pages, not GC pages
    // so make sure that any extra GC pages are zeroed
#if BACKEND_PAGE_BYTES > GENCGC_PAGE_BYTES
    const int gc_pagecount_align = BACKEND_PAGE_BYTES/GENCGC_PAGE_BYTES;
#else
    const int gc_pagecount_align = 1;
#endif
    page_index_t limit = ALIGN_UP(next_free_page, gc_pagecount_align);
    for (i = 0; i < limit; i++) {
        char* start = page_address(i);
        char* page_end = start + GENCGC_PAGE_BYTES;
        start += page_bytes_used(i);
        memset(start, 0, page_end-start);
    }
#ifndef LISP_FEATURE_SB_THREAD
    // zero the allocation regions at the start of static-space
    // This gets a spurious warning:
    //   warning: 'memset' offset [0, 71] is out of the bounds [0, 0] [-Warray-bounds]
    // which 'volatile' works around.
    char * volatile region = (char*)STATIC_SPACE_START;
    bzero((char*)region, 3*sizeof (struct alloc_region));
#endif
}

generation_index_t gc_gen_of(lispobj obj, int defaultval) {
    int page = find_page_index((void*)obj);
    if (page >= 0) return page_table[page].gen;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (immobile_space_p(obj))
        return immobile_obj_generation(base_pointer(obj));
#endif
    return defaultval;
}

/* Return 1 if 'a' is strictly younger than 'b'.
 * This asserts that 'a' is pinned if in 'from_space' because it is
 * specifically a helper function for scav_code_blob(), where this is
 * called after scavenging the header. So if something didn't get moved
 * out of from_space, then it must have been pinned.
 * So don't call this for anything except that use-case. */
static inline bool obj_gen_lessp(lispobj obj, generation_index_t b)
{
    generation_index_t a = gc_gen_of(obj, ARTIFICIALLY_HIGH_GEN);
    if (a == from_space) {
        gc_assert(pinned_p(obj, find_page_index((void*)obj)));
        a  = new_space;
    }
    return ((a==SCRATCH_GENERATION) ? from_space : a) < b;
}

/* Loosely inspired by the code in 'purify' */
#define LATERBLOCKSIZE 50000 // arbitrary
static struct later {
    struct later *next;
    struct code *list[LATERBLOCKSIZE];
    int count;
} *later_blocks = NULL;

static void delay_code_metadata_scavenge(struct code* code)
{
    struct later* block = later_blocks;
    if (!block || block->count == LATERBLOCKSIZE) {
        block = calloc(1, sizeof (struct later));
        block->next = later_blocks;
        later_blocks = block;
    }
    block->list[block->count] = code;
    ++block->count;
}

static void finish_code_metadata()
{
    struct later *block = later_blocks;
    int i;
    save_lisp_gc_iteration = 3; // ensure no more delaying of metadata scavenge
    for ( ; block ; block = block->next ) {
        for (i = 0; i < block->count; ++i) {
            struct code*c = block->list[i];
            gc_assert(!forwarding_pointer_p((lispobj*)c));
            // first two words are non-pointer, then come debug_info and fixups
            // in whatever order they were defined in objdef.
            scavenge((lispobj*)c + 2, 2);
            CLEAR_WRITTEN_FLAG((lispobj*)c);
        }
    }
    scavenge_newspace(new_space);
}

sword_t scav_code_blob(lispobj *object, lispobj header)
{
    struct code* code = (struct code*)object;
    int nboxed = code_header_words(code);
    if (!nboxed) goto done;

    int my_gen = gc_gen_of((lispobj)object, ARTIFICIALLY_HIGH_GEN);
    if (my_gen < ARTIFICIALLY_HIGH_GEN && ((my_gen & 7) == from_space)) {
        // Since 'from_space' objects are not directly scavenged - they can
        // only be scavenged after moving to newspace, then this object
        // must be pinned. (It's logically in newspace). Assert that.
        gc_assert(pinned_p(make_lispobj(object, OTHER_POINTER_LOWTAG),
                           find_page_index(object)));
        my_gen = new_space;
    }

    // If the header's 'written' flag is off and it was not copied by GC
    // into newspace, then the object should be ignored.

    // This test could stand to be tightened up: in a GC promotion cycle
    // (e.g. 0 becomes 1), we can't discern objects that got copied to newspace
    // from objects that started out there. Of the ones that were already there,
    // we need only scavenge those marked as written. All the copied one
    // should always be scavenged. So really what we could do is mark anything
    // that got copied as written, which would allow dropping the second half
    // of the OR condition. As is, we scavenge "too much" of newspace which
    // is not an issue of correctness but rather efficiency.
    if (header_rememberedp(header) || (my_gen == new_space) ||
#ifndef LISP_FEATURE_IMMOBILE_SPACE
        // if NO immobile-space, then text space is equivalent to static space
        ((uword_t)object >= TEXT_SPACE_START && object < text_space_highwatermark) ||
#endif
        ((uword_t)object >= STATIC_SPACE_START && object < static_space_free_pointer)) {
        // FIXME: We sometimes scavenge protected pages.
        // This assertion fails, but things work nonetheless.
        // gc_assert(!card_protected_p(object));

        if (save_lisp_gc_iteration == 2 &&
            lowtag_of(code->debug_info) == INSTANCE_POINTER_LOWTAG) {
            // Attempt to place debug-info at end of the heap by not scavenging now
            scavenge(object + 4, nboxed - 4);
            delay_code_metadata_scavenge(code);
        } else {
            /* Scavenge the boxed section of the code data block. */
            scavenge(object + 2, nboxed - 2);
        }

        extern void scav_code_linkage_cells(struct code*);
        scav_code_linkage_cells(code);

        // What does this have to do with DARWIN_JIT?
#if defined LISP_FEATURE_64_BIT && !defined LISP_FEATURE_DARWIN_JIT
        /* If any function in this code object redirects to a function outside
         * the object, then scavenge all entry points. Otherwise there is no need,
         * as trans_code() made necessary adjustments to internal entry points.
         * This test is just an optimization to avoid some work */
        if (((*object >> 16) & 0xff) == CODE_IS_TRACED) {
#else
        { /* Not enough spare bits in the header to hold random flags.
           * Just do the extra work always */
#endif
            for_each_simple_fun(i, fun, code, 1, {
                if (simplefun_is_wrapped(fun)) {
                    lispobj target_fun = fun_taggedptr_from_self(fun->self);
                    lispobj new = target_fun;
                    scavenge(&new, 1);
                    if (new != target_fun) fun->self = fun_self_from_taggedptr(new);
                }
            })
        }

        if (save_lisp_gc_iteration == 2) goto done;

        /* If my_gen is other than newspace, then scan for old->young
         * pointers. If my_gen is newspace, there can be no such pointers
         * because newspace is the lowest numbered generation post-GC
         * (regardless of whether this is a promotion cycle) */
        if (my_gen != new_space) {
            lispobj *where, *end = object + nboxed, ptr;
            for (where= object + 2; where < end; ++where)
                if (is_lisp_pointer(ptr = *where) && obj_gen_lessp(ptr, my_gen))
                    goto done;
        }
        CLEAR_WRITTEN_FLAG(object);
    }
done:
    return code_total_nwords(code);
}

void really_note_transporting(lispobj old,void*new,sword_t nwords)
{
    page_index_t p = find_page_index((void*)old);
    __attribute__((unused)) uword_t page_usage_limit = (uword_t)((lispobj*)page_address(p) + page_words_used(p));
    gc_assert(old < (uword_t)page_usage_limit); // this helps find bogus pointers
    if (GC_LOGGING)
        fprintf(gc_activitylog(),
                listp(old)?"t %"OBJ_FMTX" %"OBJ_FMTX"\n":
                           "t %"OBJ_FMTX" %"OBJ_FMTX" %x\n",
                old, (uword_t)new, (int)nwords);
}

/** heap invariant checker **/

static bool card_markedp(void* addr)
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (immobile_space_p((lispobj)addr))
        return !immobile_card_protected_p(addr);
#endif
    return gc_card_mark[addr_to_card_index(addr)] != CARD_UNMARKED;
}

// Check a single pointer. Return 1 if we should stop verifying due to too many errors.
// (Otherwise continue showing errors until then)
// NOTE: This function can produces false failure indications,
// usually related to dynamic space pointing to the stack of a
// dead thread, but there may be other reasons as well.
static void note_failure(lispobj thing, lispobj *where, struct verify_state *state,
                         char *str)
{
    if (state->flags & VERIFY_PRINT_HEADER_ON_FAILURE) {
        if (state->flags & VERIFY_PRE_GC) fprintf(stderr, "pre-GC failure\n");
        if (state->flags & VERIFY_POST_GC) fprintf(stderr, "post-GC failure\n");
        state->flags &= ~VERIFY_PRINT_HEADER_ON_FAILURE;
    }
    if (state->object_addr) {
        lispobj obj = compute_lispobj(state->object_addr);
        page_index_t pg = find_page_index(state->object_addr);
        fprintf(stderr, "Ptr %p @ %"OBJ_FMTX" (lispobj %"OBJ_FMTX",pg%d,h=%"OBJ_FMTX") sees %s\n",
                (void*)thing, (uword_t)where, obj, (int)pg, *native_pointer(obj), str);
        // Record this in state->err_objs if possible
        int i;
        for(i=0; i<MAX_ERR_OBJS; ++i)
            if (!state->err_objs[i]) {
                state->err_objs[i] = (uword_t)state->object_addr;
                break;
            }
    } else {
        fprintf(stderr, "Ptr %p @ %"OBJ_FMTX" sees %s\n", (void*)thing, (uword_t)where, str);
    }
}

static int
verify_pointer(lispobj thing, lispobj *where, struct verify_state *state)
{
    /* Strict containment: no pointer from a heap space may point
     * to anything outside of a heap space. */
    // bool strict_containment = state->flags & VERIFY_FINAL;

#define FAIL_IF(cond, why) \
    if (cond) { if (++state->nerrors > 25) return 1; note_failure(thing,where,state,why); }

    if (!is_lisp_pointer(thing)) {
        FAIL_IF(!is_lisp_immediate(thing), "strange non-pointer");
        return 0;
    }
    // if (strict_containment && !gc_managed_heap_space_p(thing)) GC_WARN("non-Lisp memory");
    page_index_t source_page_index = find_page_index(where);
    page_index_t target_page_index = find_page_index((void*)thing);
    int source_is_generational = source_page_index >= 0 || immobile_space_p((lispobj)where);
    if (!(target_page_index >= 0 || immobile_space_p(thing))) return 0; // can't do much with it
    if ((state->flags & VERIFY_TAGS) && target_page_index >= 0) {
        if (listp(thing)) {
            FAIL_IF(!(is_cons_half(CONS(thing)->car) && is_cons_half(CONS(thing)->cdr)),
                    "non-cons");
        } else {
            FAIL_IF(LOWTAG_FOR_WIDETAG(widetag_of(native_pointer(thing))) != lowtag_of(thing),
                    "incompatible widetag");
        }
    }
    generation_index_t to_gen =
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        points_to_asm_code_p((uword_t)thing)?
        gc_gen_of(make_lispobj((void*)asm_routines_start,OTHER_POINTER_LOWTAG),0):
#endif
        gc_gen_of(thing, ARTIFICIALLY_HIGH_GEN);
    if (to_gen < state->min_pointee_gen) state->min_pointee_gen = to_gen;
    if (state->flags & VERIFY_QUICK) return 0;
    if (target_page_index >= 0) {
        // If it's within the dynamic space it should point to a used page.
        FAIL_IF(page_free_p(target_page_index), "free page");
        FAIL_IF(!(page_table[target_page_index].type & OPEN_REGION_PAGE_FLAG)
                && (thing & (GENCGC_PAGE_BYTES-1)) >= page_bytes_used(target_page_index),
                "unallocated space");
    } else {
        // The object pointed to must not have been discarded as garbage.
        FAIL_IF(!other_immediate_lowtag_p(*native_pointer(thing)), "trashed object");
    }
    // Must not point to a forwarding pointer
    FAIL_IF(*native_pointer(thing) == FORWARDING_HEADER, "forwarding ptr");
    // Forbid pointers from R/O space into a GCed space
    FAIL_IF((READ_ONLY_SPACE_START <= (uword_t)where && where < read_only_space_free_pointer),
            "dynamic space from RO space");
    // Card marking invariant check, but only if the source of pointer is a heap object
    if (header_widetag(state->object_header) == CODE_HEADER_WIDETAG
        && ! is_in_static_space(state->object_addr)
        && to_gen < state->object_gen) {
        // two things must be true:
        // 1. the card containing the code must be marked
        FAIL_IF(!card_markedp(state->object_addr), "younger obj from WP'd code header page");
        // 2. the object header must be marked as written
        if (!header_rememberedp(state->object_header))
            lose("code @ %p (g%d). word @ %p -> %"OBJ_FMTX" (g%d)",
                 state->object_addr, state->object_gen, where, thing, to_gen);
    } else if ((state->flags & VERIFYING_GENERATIONAL) && to_gen < state->object_gen
               && source_is_generational) {
        /* The WP criteria are:
         *  - CONS marks the exact card since it can't span cards
         *  - SIMPLE-VECTOR marks the card containing the cell with the old->young pointer.
         *  - Everything else marks the object header -OR- the card with the pointer.
         *    (either/or because Lisp marks the header card,
         *     but the collector marks the cell's card.) */
        int marked = card_markedp(where)
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        || (state->object_header
            && header_widetag(state->object_header) != SIMPLE_VECTOR_WIDETAG
            && card_markedp(state->object_addr))
#elif defined LISP_FEATURE_EXECUTABLE_FUNINSTANCES
        /* #+(and (not soft-card-marks) executable-funinstances) could find the mark
         * for a page-spanning funinstance on the preceding page, because it uses
         * logical marking, not physical protection of the page holding the pointer */
        || (header_widetag(state->object_header) == FUNCALLABLE_INSTANCE_WIDETAG
            && card_markedp(state->object_addr))
#endif
        ;
        FAIL_IF(!marked, "younger obj from WP page");
    }
    int valid;
    if (state->flags & VERIFY_AGGRESSIVE) // Extreme paranoia mode
        valid = valid_tagged_pointer_p(thing);
    else {
        /* Efficiently decide whether 'thing' is plausible.
         * This MUST NOT use properly_tagged_descriptor_p() which
         * assumes a known good object base address, and would
         * "dangerously" scan a code component for embedded funs. */
        valid = plausible_tag_p(thing);
    }
    /* If 'thing' points to a stack, we can only hope that the stack
     * frame is ok, or the object at 'where' is unreachable. */
    FAIL_IF(!valid && !is_in_stack_space(thing), "junk");
    return 0;
}
#define CHECK(pointer, where) if (verify_pointer(pointer, where, state)) return 1
#define CHECK_LINKAGE_CELL(index, where) CHECK(linkage_cell_function(index), where)

/* Return 0 if good, 1 if bad.
 * Take extra pains to process weak SOLIST nodes - Finalizer list nodes weakly point
 * to a referent via an untagged pointer, so the GC doesn't even have to know that
 * the reference is weak - it simply is ignored as a non-pointer.
 * This makes invariant verification a little tricky. We want to restore the tagged
 * pointer, but only if the list is the finalizer list. */
extern bool finalizer_list_node_p(struct instance*);
static int verify_headered_object(lispobj* object, sword_t nwords,
                                  struct verify_state *state)
{
    long i;
    int widetag = widetag_of(object);
    if (instanceoid_widetag_p(widetag)) {
        lispobj layout = layout_of(object);
        if (layout) {
            CHECK(layout, object);
            struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
            if (lockfree_list_node_layout_p(LAYOUT(layout))) {
                // These objects might have _two_ untagged references -
                //  1) the 'next' slot may or may not have tag bits
                //  2) finalizer list node always stores its referent as untagged
                struct list_node* node = (void*)object;
                lispobj next = node->_node_next;
                if (fixnump(next) && next)
                  CHECK(next | INSTANCE_POINTER_LOWTAG, &node->_node_next);
                if (finalizer_node_layout_p(LAYOUT(layout))) {
                    struct solist_node* node = (void*)object;
                    // !fixnump(next) implies that this node is NOT deleted, nor in
                    // the process of getting deleted by CANCEL-FINALIZATION
                    if (node->so_key && !fixnump(next)) {
                        gc_assert(fixnump(node->so_key));
                        lispobj key = compute_lispobj((lispobj*)node->so_key);
                        CHECK(key, &node->so_key);
                    }
                }
            }
            for (i=0; i<(nwords-1); ++i)
                if (bitmap_logbitp(i, bitmap)) CHECK(object[1+i], object+1+i);
        }
        return 0;
    }
    if (widetag == CODE_HEADER_WIDETAG) {
        struct code *code = (struct code *)object;
        gc_assert(fixnump(object[1])); // boxed size, needed for code_header_words()
        sword_t nheader_words = code_header_words(code);
        /* Verify the boxed section of the code data block */
        state->min_pointee_gen = ARTIFICIALLY_HIGH_GEN;
        for (i=2; i <nheader_words; ++i) CHECK(object[i], object+i);
#ifndef NDEBUG // avoid "unused" warnings on auto vars of for_each_simple_fun()
        // Check the SIMPLE-FUN headers
        for_each_simple_fun(i, fheaderp, code, 1, {
#if defined LISP_FEATURE_COMPACT_INSTANCE_HEADER
            lispobj __attribute__((unused)) layout = funinstance_layout((lispobj*)fheaderp);
            gc_assert(!layout || layout == LAYOUT_OF_FUNCTION);
#elif defined LISP_FEATURE_64_BIT
            gc_assert((fheaderp->header >> 32) == 0);
#endif
        });
#endif
#if 0 // this looks redundant. It's checked with each pointer, no?
        bool rememberedp = header_rememberedp(code->header);
        /* The remembered set invariant is that an object is marked "written"
         * if and only if either it points to a younger object or is pointed
         * to by a register or stack. (The pointed-to case assumes that the
         * very next instruction on return from GC would store an old->young
         * pointer into that object). Non-compacting GC does not have the
         * "only if" part of that, nor does pre-GC verification because we
         * don't test the generation of the newval when storing into code. */
        if (is_in_static_space(object)) { }
        else if (compacting_p() && (state->flags & VERIFY_POST_GC) ?
            (state->min_pointee_gen < state->object_gen) != rememberedp :
            (state->min_pointee_gen < state->object_gen) && !rememberedp)
            lose("object @ %p is gen%d min_pointee=gen%d %s",
                 (void*)state->tagged_object, state->object_gen, state->min_pointee_gen,
                 rememberedp ? "written" : "not written");
#endif
        return 0;
    }
    if (widetag == SYMBOL_WIDETAG) {
        struct symbol* s = (void*)object;
        CHECK(s->value, &s->value);
        CHECK(s->fdefn, &s->fdefn);
        CHECK(s->info, &s->info);
#ifdef LISP_FEATURE_LINKAGE_SPACE
        CHECK_LINKAGE_CELL(symbol_linkage_index(s), &s->fdefn);
#endif
        CHECK(decode_symbol_name(s->name), &s->name);
        return 0;
    }
    if (widetag == FDEFN_WIDETAG) {
        struct fdefn* f = (void*)object;
        CHECK(f->name, &f->name);
        CHECK(f->fun, &f->fun);
#ifdef LISP_FEATURE_LINKAGE_SPACE
        CHECK_LINKAGE_CELL(fdefn_linkage_index(f), &f->fun);
#else
        CHECK(decode_fdefn_rawfun(f), (lispobj*)&f->raw_addr);
#endif
        return 0;
    }
    for (i=1; i<nwords; ++i) CHECK(object[i], object+i);
    return 0;
}

static __attribute__((unused)) bool acceptable_filler_cons_p(lispobj* where)
{
    if (where[0] == 0 && where[1] == 0) return 1;
    // These "conses" can result from bignum multiplication-
    // trailing insigificant sign bits which get chopped.
    if (where[0] == (uword_t)-1 && where[1] == (uword_t)-1) return 1;
    if (where[0] == (uword_t)-1 && where[1] == 0) return 1;
    return 0;
}
static int verify_range(lispobj* start, lispobj* end, struct verify_state* state)
{
    lispobj* where = start;
    if (state->flags & VERIFYING_GENERATIONAL && find_page_index(start)>=0) {
        page_index_t page = find_page_index(start);
        if (page_table[page].type == PAGE_TYPE_CONS)
            gc_assert(page_words_used(page) <=  MAX_CONSES_PER_PAGE*CONS_SIZE);
    }
    if ((state->flags & VERIFYING_UNFORMATTED)) {
        while (where < end) {
            if (*where != NO_TLS_VALUE_MARKER) {
                int result = verify_pointer(*where, where, state);
                if (result) return result;
            }
            ++where;
        }
        return 0;
    }
    while (where < end) {
        int widetag = is_header(*where) ? header_widetag(*where) : LIST_POINTER_LOWTAG;
        /* Technically we should wait until after performing the widetag validity
         * tests before calling the sizer.  Otherwise the lossage message would
         * not be as good as it could be. I guess that failure doesn't happen much */
        sword_t nwords = object_size(where);
        state->object_addr = where;
        state->object_header = is_cons_half(*where) ? 0 : *where;
        if (state->flags & VERIFYING_GENERATIONAL) {
            page_index_t pg = find_page_index(where);
            state->object_gen = pg >= 0 ? page_table[pg].gen :
              gc_gen_of((lispobj)where, ARTIFICIALLY_HIGH_GEN);
#ifdef LISP_FEATURE_PPC64
            // Cons fillers (two words of all 1s) cause failure of
            // the default verification logic, so brute-force skip them
            // regardless of whether the page type is PAGE_TYPE_CONS.
            if (*where == (uword_t)-1 && where[1] == (uword_t)-1) {
                where +=2;
                continue;
            }
#endif
            if (widetag != FILLER_WIDETAG && pg >= 0) {
                    // Assert proper page type
                    if (state->object_header) // is not a cons
                        gc_assert(page_table[pg].type != PAGE_TYPE_CONS);
#ifdef LISP_FEATURE_USE_CONS_REGION
                    else if (page_table[pg].type != PAGE_TYPE_CONS) {
                      if (is_cons_half(where[0]))
                          gc_assert(acceptable_filler_cons_p(where));
                    }
#endif
                    if (widetag == CODE_HEADER_WIDETAG) {
                        if (!is_code(page_table[pg].type))
                            lose("object @ %p is code on non-code page", where);
                    } else if (widetag == FUNCALLABLE_INSTANCE_WIDETAG) {
                        // where these reside depends on the architecture
                    } else {
                        if (is_code(page_table[pg].type))
                            lose("object @ %p is non-code on code page", where);
                    }
            }
        }
        if (!state->object_header) {
            if (verify_pointer(where[0], where+0, state) ||
                verify_pointer(where[1], where+1, state)) break;
        } else if (widetag == FILLER_WIDETAG) { // do nothing
        } else if (!(other_immediate_lowtag_p(widetag) && LOWTAG_FOR_WIDETAG(widetag))) {
            lose("Unhandled widetag %"OBJ_FMTX" @ %p", *where, where);
        } else if (leaf_obj_widetag_p(widetag)) {
#ifdef LISP_FEATURE_UBSAN
            if (specialized_vector_widetag_p(widetag)) {
                if (is_lisp_pointer(object[1])) {
                    struct vector* bits = (void*)native_pointer(object[1]);
                    if (header_widetag(bits->header) != SIMPLE_BIT_VECTOR_WIDETAG)
                      lose("bad shadow bits for %p", where);
                    gc_assert(header_widetag(bits->header) == SIMPLE_BIT_VECTOR_WIDETAG);
                    gc_assert(vector_len(bits) >= vector_len((struct vector*)object));
                }
            }
#endif
            bool strict_containment = state->flags & VERIFY_FINAL;
            if (strict_containment && gencgc_verbose && widetag == SAP_WIDETAG && where[1])
                fprintf(stderr, "\nStrange SAP %p -> %p\n", where, (void*)where[1]);
        } else {
            if (verify_headered_object(where, nwords, state)) break;
        }
        where += nwords;
    }
    return 0;
}

static int verify(lispobj start, lispobj* end, struct verify_state* state, int flags)
{
    int savedflags = state->flags;
    state->flags |= flags;
    int result = verify_range((lispobj*)start, end, state);
    state->flags = savedflags;
    return result;
}

extern void save_gc_crashdump(char *, lispobj*);
/* Return the number of verification errors found.
 * You might want to use that as a deciding factor for dump the heap
 * to a file (which takes time, and generally isn't needed).
 * But if a particular verification fails, then do dump it */
int verify_heap(__attribute__((unused)) lispobj* cur_thread_approx_stackptr,
                int flags)
{
    int verbose = gencgc_verbose | ((flags & VERIFY_VERBOSE) != 0);

    struct verify_state state;
    memset(&state, 0, sizeof state);
    state.flags = flags;

    if (verbose)
        fprintf(stderr,
                flags & VERIFY_PRE_GC ? "Verify before GC" :
                flags & VERIFY_POST_GC ? "Verify after GC(%d,%d)" :
                "Heap check", // if called at a random time
                (flags >> 1) & 7, // generation number
                flags & 1); // 'raise'
    else
        state.flags |= VERIFY_PRINT_HEADER_ON_FAILURE;

#ifdef LISP_FEATURE_IMMOBILE_SPACE
#  ifdef __linux__
    // Try this verification if immobile-space was compiled with extra debugging.
    // But weak symbols don't work on macOS.
    extern void __attribute__((weak)) check_text_pages();
    if (&check_text_pages) check_text_pages();
#  endif
    if (verbose)
        fprintf(stderr, " [immobile]");
    if (verify(FIXEDOBJ_SPACE_START,
               fixedobj_free_pointer, &state,
               flags | VERIFYING_GENERATIONAL)) goto out;
    if (verify(TEXT_SPACE_START,
               text_space_highwatermark, &state,
               flags | VERIFYING_GENERATIONAL)) goto out;
#endif
    struct thread *th;
    if (verbose)
        fprintf(stderr, " [threads]");
    state.object_addr = 0;
    state.object_gen = 0;
    for_each_thread(th) {
        if (th->state_word.state == STATE_DEAD) continue;
        if (verify((lispobj)th->binding_stack_start,
                   (lispobj*)get_binding_stack_pointer(th), &state,
                   VERIFYING_UNFORMATTED)) goto out;
        if (verify((lispobj)&th->lisp_thread,
                   (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th), &state,
                   VERIFYING_UNFORMATTED))
            goto out;
    }
    if (verbose)
        fprintf(stderr, " [RO]");
    if (verify(READ_ONLY_SPACE_START, read_only_space_free_pointer, &state, 0)) goto out;
    if (verbose)
        fprintf(stderr, " [static]");
    // Just don't worry about NIL, it's seldom the problem
    // if (verify(NIL_SYMBOL_SLOTS_START, (lispobj*)NIL_SYMBOL_SLOTS_END, &state, 0)) goto out;
    if (verify(STATIC_SPACE_OBJECTS_START, static_space_free_pointer, &state, 0)) goto out;
    if (verbose)
        fprintf(stderr, " [permgen]");
    if (verify(PERMGEN_SPACE_START, permgen_space_free_pointer, &state,0)) goto out;
    if (verbose)
        fprintf(stderr, " [dynamic]");
    state.flags |= VERIFYING_GENERATIONAL;
    walk_generation((uword_t(*)(lispobj*,lispobj*,uword_t))verify_range,
                    -1, (uword_t)&state);
    if (verbose && state.nerrors==0) fprintf(stderr, " passed\n");
 out:
    if (state.nerrors && !(flags & VERIFY_DONT_LOSE)) {
        // dump_spaces(&state, "verify failed");
        lose("Verify failed: %d errors", state.nerrors);
    }
    return state.nerrors;
}

void gc_show_pte(lispobj obj)
{
    char marks[1+CARDS_PER_PAGE];
    page_index_t page = find_page_index((void*)obj);
    if (page>=0) {
        printf("page %"PAGE_INDEX_FMT" base %p gen %d type %x ss %p used %x",
               page, page_address(page), page_table[page].gen, page_table[page].type,
               page_scan_start(page), page_bytes_used(page));
        if (page_starts_contiguous_block_p(page)) printf(" startsblock");
        if (page_ends_contiguous_block_p(page, page_table[page].gen)) printf(" endsblock");
        printf(" (%s)\n", page_card_mark_string(page, marks));
        return;
    }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    page = find_text_page_index((void*)obj);
    if (page>=0) {
        lispobj* text_page_scan_start(low_page_index_t page);
        int gens = text_page_genmask[page];
        char genstring[9];
        int i;
        for (i=0;i<8;++i) genstring[i] = (gens & (1<<i)) ? '0'+i : '-';
        genstring[8] = 0;
        printf("page %d (v) base %p gens %s ss=%p%s\n",
               (int)page, text_page_address(page), genstring,
               text_page_scan_start(page),
               card_markedp((void*)obj)?"":" WP");
        return;
    }
    page = find_fixedobj_page_index((void*)obj);
    if (page>=0) {
        printf("page %d (f) align %d gens %x%s\n", (int)page,
               fixedobj_pages[page].attr.parts.obj_align,
               fixedobj_pages[page].attr.parts.gens_,
               card_markedp((void*)obj)?"": " WP");
        return;
    }
#endif
    printf("not in GC'ed space\n");
}

static int count_immobile_objects(__attribute__((unused)) int gen, int res[3])
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    lispobj* where = (lispobj*)FIXEDOBJ_SPACE_START;
    lispobj* end = fixedobj_free_pointer;
    while (where < end) {
        if (immobile_obj_generation(where) == gen) {
            switch (widetag_of(where)) {
            case INSTANCE_WIDETAG: ++res[0]; break;
            case SYMBOL_WIDETAG: ++res[1]; break;
            }
        }
        where += object_size(where);
    }
    where = (lispobj*)TEXT_SPACE_START;
    end = text_space_highwatermark;
    while (where < end) {
        if (widetag_of(where) != FILLER_WIDETAG && immobile_obj_generation(where) == gen)
            ++res[2];
        where += object_size(where);
    }
#endif
    return (res[0] | res[1] | res[2]) != 0;
}

/* Count the number of pages in the given generation.
 * Additionally, if 'n_dirty' is non-NULL, then assign
 * into *n_dirty the count of marked pages.
 */
page_index_t
count_generation_pages(generation_index_t generation, page_index_t* n_dirty)
{
    page_index_t i, total = 0, dirty = 0;
    int j;

    for (i = 0; i < next_free_page; i++)
        if (!page_free_p(i) && (page_table[i].gen == generation)) {
            total++;
            long card = page_to_card_index(i);
            for (j=0; j<CARDS_PER_PAGE; ++j, ++card)
                if (card_dirtyp(card)) ++dirty;
        }
    // divide by cards per page rounding up
    if (n_dirty) *n_dirty = (dirty + (CARDS_PER_PAGE-1)) / CARDS_PER_PAGE;
    return total;
}

// You can call this with 0 and NULL to perform its assertions silently
void gc_gen_report_to_file(int filedes, FILE *file)
{
#ifdef LISP_FEATURE_X86
    extern void fpu_save(void *), fpu_restore(void *);
    int fpu_state[27];

    /* Can end up here after calling alloc_tramp which doesn't prepare
     * the x87 state, and the C ABI uses a different mode */
    fpu_save(fpu_state);
#endif

#define OUTPUT(str, len) \
    {if (file) fwrite(str, 1, len, file); if (filedes>=0) ignore_value(write(filedes, str, len));}

    /* Print the heap stats. */
    char header1[] =
            "     | Immobile Objects |\n";
    OUTPUT(header1, sizeof header1-1);
    char header2[] =
            " Gen layout symbol   code  Boxed   Cons    Raw   Code  SmMix  Mixed  LgRaw LgCode  LgMix"
            " Waste%       Alloc        Trig   Dirty GCs Mem-age\n";
    OUTPUT(header2, sizeof header2-1);

    generation_index_t gen_num, begin, end;
    int immobile_matrix[8][3], have_immobile_obj = 0;
    int immobile_totals[3];
    memset(immobile_matrix, 0, sizeof immobile_matrix);
    memset(immobile_totals, 0, sizeof immobile_totals);
    for (gen_num = 0; gen_num <= 6; ++gen_num) {
        if (count_immobile_objects(gen_num, immobile_matrix[gen_num]))
            have_immobile_obj |= 1 << gen_num;
        immobile_totals[0] += immobile_matrix[gen_num][0];
        immobile_totals[1] += immobile_matrix[gen_num][1];
        immobile_totals[2] += immobile_matrix[gen_num][2];
    }
    // Print from the lowest gen that has any allocated pages.
    for (begin = 0; begin <= PSEUDO_STATIC_GENERATION; ++begin)
        if ((have_immobile_obj>>begin)&1 || generations[begin].bytes_allocated) break;
    // Print up to and including the highest gen that has any allocated pages.
    for (end = SCRATCH_GENERATION; end >= 0; --end)
        if (generations[end].bytes_allocated) break;

    char linebuf[180];
    page_index_t coltot[9];
    uword_t eden_words_allocated = 0;
    page_index_t eden_pages = 0;
    memset(coltot, 0, sizeof coltot);
    for (gen_num = begin; gen_num <= end; gen_num++) {
        page_index_t page;
        page_index_t pagect[9];
        int *objct = immobile_matrix[gen_num];
        memset(pagect, 0, sizeof pagect);
        if (gen_num == 0) { // Count the eden pages
            for (page = 0; page < next_free_page; page++)
                if (page_table[page].gen == 0 && page_table[page].type & THREAD_PAGE_FLAG) {
                    int column;
                    switch (page_table[page].type & ~THREAD_PAGE_FLAG) {
                    case PAGE_TYPE_BOXED:   column = 0; break;
                    case PAGE_TYPE_CONS:    column = 1; break;
                    case PAGE_TYPE_CODE:    column = 3; break;
                    case PAGE_TYPE_MIXED:   column = 5; break;
                    default: lose("Bad eden page subtype: %x\n", page_table[page].type);
                    }
                    pagect[column]++;
                    coltot[column]++;
                    ++eden_pages;
                    eden_words_allocated += page_words_used(page);
                }
            uword_t waste = npage_bytes(eden_pages) - (eden_words_allocated<<WORD_SHIFT);
            double pct_waste = eden_pages > 0 ?
                               (double)waste / (double)npage_bytes(eden_pages) * 100 : 0.0;
            if (eden_pages) {
              printf("HORKED\n");
                int linelen = snprintf(linebuf, sizeof linebuf,
                        "  E %6d %6d %6d %6d %7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%14"PAGE_INDEX_FMT
                        "%14"PAGE_INDEX_FMT
                        "%28.1f %11"OS_VM_SIZE_FMT"\n",
                        objct[0], objct[1], objct[2], objct[3],
                        pagect[0], pagect[1], pagect[3], pagect[5],
                        pct_waste, eden_words_allocated<<WORD_SHIFT);
                OUTPUT(linebuf, linelen);
            }
            memset(pagect, 0, sizeof pagect);
        }
        uword_t words_allocated = 0;
        page_index_t tot_pages = 0;
        for (page = 0; page < next_free_page; page++)
            if (!page_free_p(page) && page_table[page].gen == gen_num
                && !(page_table[page].type & THREAD_PAGE_FLAG)) {
                int column;
                switch (page_table[page].type & (SINGLE_OBJECT_FLAG|PAGE_TYPE_MASK)) {
                case PAGE_TYPE_BOXED:   column = 0; break;
                case PAGE_TYPE_CONS:    column = 1; break;
                case PAGE_TYPE_UNBOXED: column = 2; break;
                case PAGE_TYPE_CODE:    column = 3; break;
                case PAGE_TYPE_SMALL_MIXED:   column = 4; break;
                case PAGE_TYPE_MIXED:   column = 5; break;
                case SINGLE_OBJECT_FLAG|PAGE_TYPE_UNBOXED: column = 6; break;
                case SINGLE_OBJECT_FLAG|PAGE_TYPE_CODE:    column = 7; break;
                case SINGLE_OBJECT_FLAG|PAGE_TYPE_MIXED:   column = 8; break;
                default: lose("Invalid page type %#x (p%"PAGE_INDEX_FMT")", page_table[page].type, page);
                }
                pagect[column]++;
                coltot[column]++;
                ++tot_pages;
                words_allocated += page_words_used(page);
            }
        struct generation* gen = &generations[gen_num];
        if (gen_num == 0)
            gc_assert(gen->bytes_allocated ==
                      (words_allocated+eden_words_allocated) << WORD_SHIFT);
        /* private-cons.inc doesn't update bytes_allocated */
        /*
        else {
            gc_assert(gen->bytes_allocated == words_allocated << WORD_SHIFT);
        }
        */
        page_index_t n_dirty;
        count_generation_pages(gen_num, &n_dirty);
        uword_t waste = npage_bytes(tot_pages) - (words_allocated<<WORD_SHIFT);
        double pct_waste = tot_pages > 0 ?
          (double)waste / (double)npage_bytes(tot_pages) * 100 : 0.0;
        int linelen =
            snprintf(linebuf, sizeof linebuf,
                "  %d %6d %6d %6d"
                "%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT
                "%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT
                "%7"PAGE_INDEX_FMT" %6.1f %11"OS_VM_SIZE_FMT" %11"OS_VM_SIZE_FMT,
                gen_num, objct[0], objct[1], objct[2],
                pagect[0], pagect[1], pagect[2], pagect[3], pagect[4], pagect[5],
                pagect[6], pagect[7], pagect[8],
                pct_waste, words_allocated<<WORD_SHIFT,
                (uintptr_t)gen->gc_trigger);
        // gen0 pages are never WPed
        linelen += snprintf(linebuf+linelen, sizeof linebuf-linelen,
                            gen_num==0?"       -" : " %7"PAGE_INDEX_FMT, n_dirty);
        linelen += snprintf(linebuf+linelen, sizeof linebuf-linelen,
                            " %3d %7.4f\n", gen->num_gc, generation_average_age(gen_num));
        OUTPUT(linebuf, linelen);
    }
    page_index_t tot_pages = coltot[0] + coltot[1] + coltot[2] + coltot[3] + coltot[4] +
                             coltot[5] + coltot[6] + coltot[7] + coltot[8];
    uword_t waste = npage_bytes(tot_pages) - bytes_allocated;
    double pct_waste = (double)waste / (double)npage_bytes(tot_pages) * 100;
    double heap_use_frac = 100 * (double)bytes_allocated / (double)dynamic_space_size;
    int *objct = immobile_totals;
    int linelen =
        snprintf(linebuf, sizeof linebuf,
            "Tot %6d %6d %6d"
            "%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT
            "%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT
            "%7"PAGE_INDEX_FMT" %6.1f%12"OS_VM_SIZE_FMT
            " [%.1f%% of %"OS_VM_SIZE_FMT" max]\n",
            objct[0], objct[1], objct[2],
            coltot[0], coltot[1], coltot[2], coltot[3], coltot[4], coltot[5], coltot[6],
            coltot[7], coltot[8], pct_waste,
            (uintptr_t)bytes_allocated, heap_use_frac, (uintptr_t)dynamic_space_size);
    OUTPUT(linebuf, linelen);
#undef OUTPUT

#ifdef LISP_FEATURE_X86
    fpu_restore(fpu_state);
#endif
}
