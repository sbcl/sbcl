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
#include "sbcl.h"
#ifndef LISP_FEATURE_WIN32
#include <signal.h>
#endif
#include "runtime.h"
#include "os.h"
#include "interr.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"
#include "arch.h"
#include "gc.h"
#include "gc-internal.h"
#include "gc-private.h"
#include "gencgc-private.h"
#include "thread.h"
#include "getallocptr.h"
#include "alloc.h"
#include "code.h"
#include "genesis/gc-tables.h"
#include "genesis/vector.h"
#include "genesis/weak-pointer.h"
#include "genesis/fdefn.h"
#include "genesis/simple-fun.h"
#include "save.h"
#include "genesis/hash-table.h"
#include "genesis/instance.h"
#include "genesis/layout.h"
#include "hopscotch.h"
#include "genesis/cons.h"
#include "forwarding-ptr.h"
#include "lispregs.h"
#include "var-io.h"

/* forward declarations */
page_index_t  gc_find_freeish_pages(page_index_t *restart_page_ptr, sword_t nbytes,
                                    int page_type, generation_index_t gen);


/*
 * GC parameters
 */

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

/* Largest allocation seen since last GC. */
os_vm_size_t large_allocation = 0;


/*
 * debugging
 */

/* the verbosity level. All non-error messages are disabled at level 0;
 * and only a few rare messages are printed at level 1. */
boolean gencgc_verbose = 0;

/* FIXME: At some point enable the various error-checking things below
 * and see what they say. */

/* We hunt for pointers to old-space, when GCing generations >= verify_gen.
 * Set verify_gens to HIGHEST_NORMAL_GENERATION + 2 to disable this kind of
 * check. */
generation_index_t verify_gens = HIGHEST_NORMAL_GENERATION + 2;

/* Should we do a pre-scan of the heap before it's GCed? */
boolean pre_verify_gen_0 = 0; // FIXME: should be named 'pre_verify_gc'


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
boolean gc_active_p = 0;

/* should the GC be conservative on stack. If false (only right before
 * saving a core), don't scan the stack / mark pages pinned. */
static boolean conservative_stack = 1;

/* An array of page structures is allocated on gc initialization.
 * This helps to quickly map between an address and its page structure.
 * page_table_pages is set from the size of the dynamic space. */
page_index_t page_table_pages;
struct page *page_table;
unsigned char *gc_card_mark;
lispobj gc_object_watcher;
int gc_traceroot_criterion;
// Filtered pins include code but not simple-funs,
// and must not include invalid pointers.
lispobj* gc_filtered_pins;
static int pins_alloc_size;
int gc_pin_count;
struct hopscotch_table pinned_objects;

/* This is always 0 except during gc_and_save() */
lispobj lisp_init_function;

static inline boolean page_free_p(page_index_t page) {
    return (page_table[page].type == FREE_PAGE_FLAG);
}

static inline boolean page_boxed_p(page_index_t page) {
    return (page_table[page].type & BOXED_PAGE_FLAG);
}

#ifndef LISP_FEATURE_SOFT_CARD_MARKS
/// Return true if low 4 'type' bits are 0zz1, false otherwise (z = don't-care)
/// i.e. true of pages which could hold boxed or partially boxed (mixed) objects,
/// including pages of code.
static inline boolean page_boxed_no_region_p(page_index_t page) {
    return (page_table[page].type & 9) == BOXED_PAGE_FLAG;
}

static inline boolean protect_page_p(page_index_t page, generation_index_t generation) {
    return (page_boxed_no_region_p(page)
            && (page_words_used(page) != 0)
            && !page_table[page].pinned
            && (page_table[page].gen == generation));
}
#endif

/* Calculate the start address for the given page number. */
inline char *
page_address(page_index_t page_num)
{
    return (void*)(DYNAMIC_SPACE_START + (page_num * GENCGC_PAGE_BYTES));
}

/* Calculate the address where the allocation region associated with
 * the page starts. */
static inline void *
page_scan_start(page_index_t page_index)
{
    return page_address(page_index)-page_scan_start_offset(page_index);
}

/* True if the page starts a contiguous block. */
static inline boolean
page_starts_contiguous_block_p(page_index_t page_index)
{
    // Don't use the preprocessor macro: 0 means 0.
    return page_table[page_index].scan_start_offset_ == 0;
}

/* True if the page is the last page in a contiguous block. */
static inline boolean
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
    boolean answer = page_words_used(page_index) < GENCGC_PAGE_WORDS
                  || page_starts_contiguous_block_p(page_index+1);
#ifdef DEBUG
    boolean safe_answer =
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

/* We maintain the invariant that pages with FREE_PAGE_FLAG have
 * scan_start of zero, to optimize page_ends_contiguous_block_p().
 * Clear all the flags that don't pertain to a free page.
 * Particularly the 'need_zerofill' bit has to remain unchanged */
static inline void reset_page_flags(page_index_t page) {
    page_table[page].scan_start_offset_ = 0;
    page_table[page].type = 0;
    page_table[page].write_protected_cleared = page_table[page].pinned = 0;
    SET_PAGE_PROTECTED(page,0);
}

/// External function for calling from Lisp.
page_index_t ext_find_page_index(void *addr) { return find_page_index(addr); }

static os_vm_size_t
npage_bytes(page_index_t npages)
{
    gc_assert(npages>=0);
    return ((os_vm_size_t)npages)*GENCGC_PAGE_BYTES;
}

/* Check that X is a higher address than Y and return offset from Y to
 * X in bytes. */
static inline os_vm_size_t
addr_diff(void *x, void *y)
{
    gc_assert(x >= y);
    return (uintptr_t)x - (uintptr_t)y;
}

/* a structure to hold the state of a generation
 *
 * CAUTION: If you modify this, make sure to touch up the alien
 * definition in src/code/gc.lisp accordingly. ...or better yes,
 * deal with the FIXME there...
 */
struct generation {
    /* the bytes allocated to this generation */
    os_vm_size_t bytes_allocated;

    /* the number of bytes at which to trigger a GC */
    os_vm_size_t gc_trigger;

    /* to calculate a new level for gc_trigger */
    os_vm_size_t bytes_consed_between_gc;

    /* the number of GCs since the last raise */
    int num_gc;

    /* the number of GCs to run on the generations before raising objects to the
     * next generation */
    int number_of_gcs_before_promotion;

    /* the cumulative sum of the bytes allocated to this generation. It is
     * cleared after a GC on this generations, and update before new
     * objects are added from a GC of a younger generation. Dividing by
     * the bytes_allocated will give the average age of the memory in
     * this generation since its last GC. */
    os_vm_size_t cum_sum_bytes_allocated;

    /* a minimum average memory age before a GC will occur helps
     * prevent a GC when a large number of new live objects have been
     * added, in which case a GC could be a waste of time */
    double minimum_age_before_gc;
};

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

/* The maximum used page in the heap is maintained and used to update
 * ALLOCATION_POINTER which is used by the room function to limit its
 * search of the heap. XX Gencgc obviously needs to be better
 * integrated with the Lisp code. */

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

extern os_vm_size_t gencgc_release_granularity;
os_vm_size_t gencgc_release_granularity = GENCGC_RELEASE_GRANULARITY;

extern os_vm_size_t gencgc_alloc_granularity;
os_vm_size_t gencgc_alloc_granularity = GENCGC_ALLOC_GRANULARITY;


/*
 * miscellaneous heap functions
 */

/* Count the number of pages in the given generation.
 * Additionally, if 'n_write_protected' is non-NULL, then assign
 * into *n_write_protected the count of write-protected pages.
 */
static page_index_t
count_generation_pages(generation_index_t generation, page_index_t* n_dirty)
{
    page_index_t i, total = 0, dirty = 0;

    for (i = 0; i < next_free_page; i++)
        if (!page_free_p(i) && (page_table[i].gen == generation)) {
            total++;
            if (!PAGE_WRITEPROTECTED_P(i)) dirty++;
        }
    if (n_dirty) *n_dirty = dirty;
    return total;
}

static void show_pinnedobj_count()
{
    page_index_t page;
    int nwords = 0;
    int n_pinned_largeobj = 0;
    for (page = 0; page < next_free_page; ++page) {
        if (page_table[page].gen == from_space && page_table[page].pinned
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

/* Return the average age of the memory in a generation. */
extern double
generation_average_age(generation_index_t gen_index)
{
    struct generation* gen = &generations[gen_index];
    if (gen->bytes_allocated == 0)
        return 0.0;

    return (double)gen->cum_sum_bytes_allocated / (double)gen->bytes_allocated;
}

#ifdef LISP_FEATURE_X86
extern void fpu_save(void *);
extern void fpu_restore(void *);
#endif

#define PAGE_INDEX_FMT PRIdPTR

extern void
write_generation_stats(FILE *file)
{
#ifdef LISP_FEATURE_X86
    int fpu_state[27];

    /* Can end up here after calling alloc_tramp which doesn't prepare
     * the x87 state, and the C ABI uses a different mode */
    fpu_save(fpu_state);
#endif

    /* Print the heap stats. */
    fprintf(file,
            "Gen  Boxed    Raw   Code  Mixed  LgRaw LgCode  LgMix  "
            "Pin       Alloc     Waste        Trig   Dirty GCs Mem-age\n");

    generation_index_t i, begin, end;
    // Print from the lowest gen that has any allocated pages.
    for (begin = 0; begin <= PSEUDO_STATIC_GENERATION; ++begin)
        if (generations[begin].bytes_allocated) break;
    // Print up to and including the highest gen that has any allocated pages.
    for (end = SCRATCH_GENERATION; end >= 0; --end)
        if (generations[end].bytes_allocated) break;

    for (i = begin; i <= end; i++) {
        page_index_t page;
        // page kinds: small {boxed,unboxed,code,mixed} and then large of same
        page_index_t pagect[8], pinned_cnt = 0;

        memset(pagect, 0, sizeof pagect);
        for (page = 0; page < next_free_page; page++)
            if (!page_free_p(page) && page_table[page].gen == i) {
                int column;
                switch (page_table[page].type & PAGE_TYPE_MASK) {
                case PAGE_TYPE_BOXED: column = 0; break;
                case PAGE_TYPE_UNBOXED: column = 1; break;
                case PAGE_TYPE_CODE: column = 2; break;
                case PAGE_TYPE_MIXED: column = 3; break;
                default: lose("Invalid page type %x", page_table[page].type);
                }
                if (page_single_obj_p(page)) column += 4;
                pagect[column]++;
                if (page_table[page].pinned) pinned_cnt++;
            }
        struct generation* gen = &generations[i];
        gc_assert(gen->bytes_allocated == count_generation_bytes_allocated(i));
        gc_assert(pagect[4] == 0); // should not be anything in large boxed
        page_index_t tot_pages, n_dirty    ;
        tot_pages = count_generation_pages(i, &n_dirty);
        gc_assert(tot_pages ==
                  pagect[0] + pagect[1] + pagect[2] + pagect[3] +
                  pagect[4] + pagect[5] + pagect[6] + pagect[7]);
        fprintf(file,
                " %d %7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT
                "%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT"%7"PAGE_INDEX_FMT
                " %4"PAGE_INDEX_FMT
                " %11"OS_VM_SIZE_FMT
                " %9"OS_VM_SIZE_FMT
                " %11"OS_VM_SIZE_FMT,
                i,
                pagect[0], pagect[1], pagect[2], pagect[3], pagect[5], pagect[6], pagect[7],
                pinned_cnt,
                (uintptr_t)gen->bytes_allocated,
                (uintptr_t)npage_bytes(tot_pages) - generations[i].bytes_allocated,
                (uintptr_t)gen->gc_trigger);
        // gen0 pages are never WPed
        fprintf(file, i==0?"       -" : " %7"PAGE_INDEX_FMT, n_dirty);
        fprintf(file, " %3d %7.4f\n", gen->num_gc, generation_average_age(i));
    }
    fprintf(file,"           Total bytes allocated    = %13"OS_VM_SIZE_FMT"\n",
            (uintptr_t)bytes_allocated);
    fprintf(file,"           Dynamic-space-size bytes = %13"OS_VM_SIZE_FMT"\n",
            (uintptr_t)dynamic_space_size);

#ifdef LISP_FEATURE_X86
    fpu_restore(fpu_state);
#endif
}

extern void
write_heap_exhaustion_report(FILE *file, long available, long requested,
                             struct thread __attribute__((unused)) *thread)
{
    fprintf(file,
            "Heap exhausted during %s: %ld bytes available, %ld requested.\n",
            gc_active_p ? "garbage collection" : "allocation",
            available,
            requested);
    write_generation_stats(file);
    fprintf(file, "GC control variables:\n");
    fprintf(file, "   *GC-INHIBIT* = %s\n   *GC-PENDING* = %s\n",
            read_TLS(GC_INHIBIT,thread)==NIL ? "false" : "true",
            (read_TLS(GC_PENDING, thread) == T) ?
            "true" : ((read_TLS(GC_PENDING, thread) == NIL) ?
                      "false" : "in progress"));
#ifdef LISP_FEATURE_SB_THREAD
    fprintf(file, "   *STOP-FOR-GC-PENDING* = %s\n",
            read_TLS(STOP_FOR_GC_PENDING,thread)==NIL ? "false" : "true");
#endif
}

extern void
print_generation_stats(void)
{
    write_generation_stats(stderr);
}

extern char* gc_logfile;
char * gc_logfile = NULL;

extern void
log_generation_stats(char *logfile, char *header)
{
    if (logfile) {
        FILE * log = fopen(logfile, "a");
        if (log) {
            fprintf(log, "%s\n", header);
            write_generation_stats(log);
            fclose(log);
        } else {
            fprintf(stderr, "Could not open gc logfile: %s\n", logfile);
            fflush(stderr);
        }
    }
}

extern void
report_heap_exhaustion(long available, long requested, struct thread *th)
{
    if (gc_logfile) {
        FILE * log = fopen(gc_logfile, "a");
        if (log) {
            write_heap_exhaustion_report(log, available, requested, th);
            fclose(log);
        } else {
            fprintf(stderr, "Could not open gc logfile: %s\n", gc_logfile);
            fflush(stderr);
        }
    }
    /* Always to stderr as well. */
    write_heap_exhaustion_report(stderr, available, requested, th);
}


#if defined LISP_FEATURE_X86 && !defined LISP_FEATURE_LINUX
void fast_bzero(void*, size_t); /* in <arch>-assem.S */
#else
#define fast_bzero(addr, count) memset(addr, 0, count)
#endif

/* Zero the memory at ADDR for LENGTH bytes, but use mmap/munmap instead
 * of zeroing it ourselves, i.e. in practice give the memory back to the
 * OS. Generally done after a large GC.
 */
static void __attribute__((unused))
zero_range_with_mmap(os_vm_address_t addr, os_vm_size_t length) {
#ifdef LISP_FEATURE_LINUX
    // We use MADV_DONTNEED only on Linux due to differing semantics from BSD.
    // Linux treats it as a demand that the memory be 0-filled, or refreshed
    // from a file that backs the range. BSD takes it as a hint that you don't
    // care if the memory has to brought in from swap when next accessed,
    // i.e. it's not a request to make a user-visible alteration to memory.
    // So in theory this can bring a page in from the core file, if we happen
    // to hit a page that resides in the portion of memory mapped by coreparse.
    // In practice this should not happen because objects from a core file can't
    // become garbage. Except in save-lisp-and-die they can, and we must be
    // cautious not to resurrect bytes that originally came from the file.
    if ((os_vm_address_t)addr >= anon_dynamic_space_start) {
        if (madvise(addr, length, MADV_DONTNEED) != 0)
            lose("madvise failed");
    } else
#endif
#ifdef LISP_FEATURE_WIN32
        os_revalidate_bzero(addr, length);
#else
    {
        void *new_addr;
        os_invalidate(addr, length);
        new_addr = os_validate(NOT_MOVABLE, addr, length, 0, 1);
        if (new_addr == NULL || new_addr != addr) {
            lose("remap_free_pages: page moved, %p ==> %p",
                 addr, new_addr);
        }
    }
#endif
}

/*
Regarding zero_pages(), if the next operation would be memcpy(),
then zeroing is a total waste of time and we should skip it.

The most simple case seems to be ALLOCATE-CODE-OBJECT because we can treat pages
of code consistently in terms of whether the newly allocated page is for Lisp or
for the garbage collector's transport function. The object is basically an unboxed
object, so there are no "wild pointers" in it, at least until the boxed size is
written. So we can skip prezeroing because the bulk of the object will be subject
to memcpy() from either an octet vector produced by the assember, or a fasl stream.
We only need to prezero the boxed words. GC avoids prezeroing of its code pages.

The next simplest case seems to be unboxed pages - Lisp can never directly request
an unboxed page (at least in the current design), so any unboxed page is for GC,
and will always be used for a transport function. We can skip zeroing those pages,
because even if GC does not fill the page completely, it can not be used for other
object allocations from Lisp.

Boxed pages are the problem. Except for pages which are 100% used by GC, they  might
later by consumed in part by Lisp. Unfortunately we don't know whether it will be
100% used until it's 100% used. So we can't skip zeroing.
However, in general, we should try to convert Lisp allocators to be aware of
the issue of zeroing rather than relying on C to do it, as this will relieve
a pain point (a so-called "impedence mismatch") when trying to plug in other
allocators that do not intrinsically give you zero-initialized memory.

The cases can be broken down as follows:
 - unboxed objects can always be zeroed at leisure in Lisp. This is hard only because
   Lisp does not distinguish in the slow path allocator whether it is asking for boxed
   or unboxed memory, so even if we made the Lisp code perform explicit zero-filling of
   strings and numeric vectors, the allocation macros needs to be enhanced
   to inform C of the fact that zeroing will happen in Lisp whenever we have to go to
   the slow path; and we'll need unboxed thread-local regions of course.

 - structure objects almost always have all slots written immediately after
   allocation, so they don't necessarily demand prezeroing, but we have to think about
   to the scope of the pseudatomic wrapping. One of the following must pertain:
   * Widen the pseudo-atomic scope so that initialization happens within it,
     never permitting GC to see old garbage, OR
   * Store the layout last rather than first, and say that until the layout is stored,
     GC might see garbage, treating any bit pattern as a conservative pointer.
     (because there are two separate issues: ignoring old values, and ensuring that
     newly written slots are perceived as enlivening what they point to)
   * Add some bits indicating how many slots of the object are initialized.
     This seems impractical

 - general arrays present the largest problem - the choice of when to zero should be
   based on whether the object is large or not and whether one of :initial-element
   or :initial-contents were specified. If the initial-element is NIL, then the initial
   zero-fill was a waste.

 - closures and everything else except arrays are basically structure-like
   and have the same issue. Fixed-sized objects are simple though - e.g. value-cells
   can move the store of the 1 word payload inside pseudo-atomic if it isn't already.
   Thusly, any value-cell could go on a non-prezeroed page.

In general, deciding when to zero-initialize to attain maximum performance is nontrivial.
See "Why Nothing Matters: The Impact of Zeroing"
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/zero-oopsla-2011.pdf

 */

/* Zero the pages from START to END (inclusive). Generally done just after
 * a new region has been allocated.
 */
static inline void zero_pages(page_index_t start, page_index_t end) {
    if (start <= end)
#ifdef LISP_FEATURE_DARWIN_JIT
        zero_range_with_mmap(page_address(start), npage_bytes(1+end-start));
#else
    fast_bzero(page_address(start), npage_bytes(1+end-start));
#endif
}
/* Zero the address range from START up to but not including END */
static inline void zero_range(char* start, char* end) {
    if (start < end)
        fast_bzero(start, end-start);
}

// A robustness testing flag: when true, pages claimed for purpose of the current
// GC cycle are prefilled with garbage bytes instead of zeroes.
// This is fine for GC because it always uses memcpy() from the old data.
// Lisp never directly requests unboxed pages, so this is always acceptable
// on unboxed pages. Lisp does directly request pages of code, but the code allocator
// is compatible with the non-prezeroing convention.
// The last case is boxed pages- we can only do this on boxed pages that will be
// consumed by GC and only by GC (i.e. not pages that were either directly
// allocated by lisp, or whose tail could be picked up by lisp)
// This could be made to work for those pages as well if we remember which pages
// haven't been fully used prior to returning to lisp, and then zero-filling
// any unused bytes just in case lisp picks up the remaining part of the page.
char gc_allocate_dirty = 0;
/* The generation currently being allocated to. */
static generation_index_t gc_alloc_generation;

/* Zero the pages from START to END (inclusive), except for those
 * pages that are known to already zeroed. Mark all pages in the
 * ranges as non-zeroed.
 */
void zero_dirty_pages(page_index_t start, page_index_t end, int page_type) {
    page_index_t i, j;

    // If allocating mixed pages to gen0 (or scratch which becomes gen0) then
    // this allocation is potentially going to be extended by lisp (if it happens to
    // pick up the tail of the page as its next available region)
    // and we really have to zeroize the page. Otherwise, if not mixed or allocating
    // memory that is entirely within GC, then lisp will never use parts of the page.
    // So we can avoid pre-zeroing all codes pages, all unboxed pages,
    // all strictly boxed pages, and all mixed pages allocated to gen>=1.

#ifdef LISP_FEATURE_DARWIN_JIT
    /* Must always zero, as it may need changing the protection bits. */
    boolean must_zero = 1;
#else
    boolean usable_by_lisp =
        gc_alloc_generation == 0 || (gc_alloc_generation == SCRATCH_GENERATION
                                     && from_space == 0);
    boolean must_zero =
        (page_type == PAGE_TYPE_MIXED && usable_by_lisp) || page_type == 0;
#endif

    if (gc_allocate_dirty) { // For testing only
#ifdef LISP_FEATURE_64_BIT
        lispobj word = 0x100 | 5;
#else
        lispobj word = 0x100 | 2;
#endif
        if (must_zero)
            for (i = start; i <= end; i++)
                zero_pages(i, i);
        else
            for (i = start; i <= end; i++) {
                // Write every word with a widetag which if scavenged inavertentiy would call scav_lose().
                // This is purely for testing that the lisp side can deal with unzeroed code pages.
                // Non-code unboxed pages won't been seen by lisp allocation routines.
                lispobj *where = (lispobj*)page_address(i);
                lispobj *limit = (lispobj*)((char*)where + GENCGC_PAGE_BYTES);
                char* page_type_description[7] = {"Boxed","Raw","Mixed",0,"Cons",0,"Code"};
                if (gc_allocate_dirty > 1)
                    fprintf(stderr, "dirtying g%d %s %d (%p..%p) [%d->%d]\n",
                            gc_alloc_generation, page_type_description[page_type-1],
                            (int)i, where, limit, from_space, new_space);
                while (where < limit) *where++ = word;
            }
    } else if (must_zero) {
        // look for contiguous ranges. This is probably without merit, since bzero
        // does not go significantly faster for 2 pages than for 1 page and 1 page.
        for (i = start; i <= end; i++) {
            if (!page_need_to_zero(i)) continue;
            // compute 'j' as the upper exclusive page index bound
            for (j = i+1; (j <= end) && page_need_to_zero(j) ; j++)
                ; /* empty body */
            zero_pages(i, j-1);
            i = j;
        }
    }

    for (i = start; i <= end; i++) {
        set_page_need_to_zero(i, 1);
    }
}


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

/* We use four regions for the current newspace generation. */
struct alloc_region gc_alloc_region[4];

static page_index_t
  alloc_start_pages[8], // one for each value of PAGE_TYPE_x
  gencgc_alloc_start_page; // initializer for the preceding array

#define RESET_ALLOC_START_PAGES() \
        alloc_start_pages[0] = gencgc_alloc_start_page; \
        alloc_start_pages[1] = gencgc_alloc_start_page; \
        alloc_start_pages[2] = gencgc_alloc_start_page; \
        alloc_start_pages[3] = gencgc_alloc_start_page; \
        alloc_start_pages[4] = gencgc_alloc_start_page; \
        alloc_start_pages[5] = gencgc_alloc_start_page; \
        alloc_start_pages[6] = gencgc_alloc_start_page; \
        alloc_start_pages[7] = gencgc_alloc_start_page;

static inline page_index_t
alloc_start_page(unsigned int page_type, int large)
{
    if (page_type > 7) lose("bad page_type: %d", page_type);
    return alloc_start_pages[large ? 0 : page_type];
}

static inline void
set_alloc_start_page(unsigned int page_type, int large, page_index_t page)
{
    if (page_type > 7) lose("bad page_type: %d", page_type);
    alloc_start_pages[large ? 0 : page_type] = page;
}
#include "private-cons.inc"

static inline boolean __attribute__((unused)) region_closed_p(struct alloc_region* region) {
    return !region->start_addr;
}
#define ASSERT_REGIONS_CLOSED() \
    gc_assert(!((uintptr_t)gc_alloc_region[0].start_addr \
               |(uintptr_t)gc_alloc_region[1].start_addr \
               |(uintptr_t)gc_alloc_region[2].start_addr \
               |(uintptr_t)gc_alloc_region[3].start_addr))

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

static void
gc_alloc_new_region(sword_t nbytes, int page_type, struct alloc_region *alloc_region, int unlock)
{
    page_index_t first_page;
    page_index_t last_page;
    page_index_t i;

    /* Check that the region is in a reset state. */
    gc_dcheck(region_closed_p(alloc_region));
    first_page = alloc_start_page(page_type, 0);

    INSTRUMENTING(
    last_page = gc_find_freeish_pages(&first_page, nbytes,
                                      ((nbytes >= (sword_t)GENCGC_PAGE_BYTES) ?
                                       SINGLE_OBJECT_FLAG : 0) | page_type,
                                      gc_alloc_generation),
    et_find_freeish_page);

    /* Set up the alloc_region. */
    alloc_region->last_page = last_page;
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
    page_table[first_page].type = OPEN_REGION_PAGE_FLAG | page_type;

    for (i = first_page+1; i <= last_page; i++) {
        page_table[i].type = OPEN_REGION_PAGE_FLAG | page_type;
        page_table[i].gen = gc_alloc_generation;
        set_page_scan_start_offset(i,
            addr_diff(page_address(i), alloc_region->start_addr));
    }
    if (unlock) {
        int __attribute__((unused)) ret = mutex_release(&free_pages_lock);
        gc_assert(ret);
    }

    /* If the first page was only partial, don't check whether it's
     * zeroed (it won't be) and don't zero it (since the parts that
     * we're interested in are guaranteed to be zeroed).
     */
    if (page_words_used(first_page)) {
        first_page++;
    }

    INSTRUMENTING(zero_dirty_pages(first_page, last_page, page_type), et_bzeroing);

#ifdef LISP_FEATURE_DARWIN_JIT
    if (page_type == PAGE_TYPE_CODE) {
        page_index_t first = first_page;
        if (page_words_used(first) != 0) {
            first++;
        }
        if (last_page >= first) {
            os_protect(page_address(first), npage_bytes(1+last_page-first), OS_VM_PROT_ALL);
        }
    }
#endif
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
        boolean more;
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
        set_alloc_start_page(page_type, 0, next_page-1);

        /* Add the region to the new_areas if requested. */
        if (BOXED_PAGE_FLAG & page_type)
            add_new_area(first_page,orig_first_page_bytes_used, region_size);

    } else if (!orig_first_page_bytes_used) {
        /* The first page is completely unused. Unallocate it */
        reset_page_flags(first_page);
    }

    /* Unallocate any unused pages. */
    while (next_page <= alloc_region->last_page) {
        gc_assert(page_words_used(next_page) == 0);
        reset_page_flags(next_page);
        next_page++;
    }
    gc_set_region_empty(alloc_region);
}

/* Allocate a possibly large object. */
void *
gc_alloc_large(sword_t nbytes, int page_type, struct alloc_region *alloc_region, int unlock)
{
    page_index_t first_page, last_page;
    // Large BOXED would seem to serve no purpose beyond MIXED
    if (page_type == PAGE_TYPE_BOXED) page_type = PAGE_TYPE_MIXED;

    first_page = alloc_start_page(page_type, 1);
    // FIXME: really we want to try looking for space following the highest of
    // the last page of all other small object regions. That's impossible - there's
    // not enough information. At best we can skip some work in only the case where
    // the supplied region was the one most recently created. To do this right
    // would entail a malloc-like allocator at the page granularity.
    if (first_page <= alloc_region->last_page) {
        first_page = alloc_region->last_page+1;
    }

    INSTRUMENTING(
    last_page = gc_find_freeish_pages(&first_page, nbytes,
                                      SINGLE_OBJECT_FLAG | page_type,
                                      gc_alloc_generation),
    et_find_freeish_page);

    // FIXME: Should this be 1+last_page ?
    // (Doesn't matter too much since it'll be skipped on restart if unusable)
    set_alloc_start_page(page_type, 1, last_page);

    /* Set up the pages. */
    page_index_t page;
    for (page = first_page; page <= last_page; ++page) {
        /* Large objects don't share pages with other objects. */
        gc_assert(page_words_used(page) == 0);
        page_table[page].type = SINGLE_OBJECT_FLAG | page_type;
        page_table[page].gen = gc_alloc_generation;
    }

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
    if (unlock)
        THREAD_JIT(0);
    *addr = (nwords - 1) << N_WIDETAG_BITS | FILLER_WIDETAG;
    if (unlock) // avoid enabling while GCing
        THREAD_JIT(1);

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

    if (unlock) {
        int __attribute__((unused)) ret = mutex_release(&free_pages_lock);
        gc_assert(ret);
    }
    INSTRUMENTING(zero_dirty_pages(first_page, last_page, page_type), et_bzeroing);

    /* Add the region to the new_areas if requested. */
    if (BOXED_PAGE_FLAG & page_type)
        add_new_area(first_page, 0, nbytes);

    // page may have not needed zeroing, but first word was stored,
    // turning the putative object temporarily into a page filler object.
    // Now turn it back into free space.
    *addr = 0;

#ifdef LISP_FEATURE_DARWIN_JIT
    if (page_type == PAGE_TYPE_CODE) {
        os_protect(page_address(first_page), npage_bytes(1+last_page-first_page), OS_VM_PROT_ALL);
    }
#endif

    return addr;
}

void
gc_heap_exhausted_error_or_lose (sword_t available, sword_t requested)
{
    struct thread *thread = get_sb_vm_thread();
    /* Write basic information before doing anything else: if we don't
     * call to lisp this is a must, and even if we do there is always
     * the danger that we bounce back here before the error has been
     * handled, or indeed even printed.
     */
    report_heap_exhaustion(available, requested, thread);
    if (gc_active_p || (available == 0)) {
        /* If we are in GC, or totally out of memory there is no way
         * to sanely transfer control to the lisp-side of things.
         */
        lose("Heap exhausted, game over.");
    }
    else {
        (void)mutex_release(&free_pages_lock);
#ifndef LISP_FEATURE_WIN32
        gc_assert(get_pseudo_atomic_atomic(thread));
        clear_pseudo_atomic_atomic(thread);
        if (get_pseudo_atomic_interrupted(thread))
            do_pending_interrupt();
#endif
        /* Another issue is that signalling HEAP-EXHAUSTED error leads
         * to running user code at arbitrary places, even in a
         * WITHOUT-INTERRUPTS which may lead to a deadlock without
         * running out of the heap. So at this point all bets are
         * off. */
        if (read_TLS(INTERRUPTS_ENABLED,thread) == NIL)
            corruption_warning_and_maybe_lose
                ("Signalling HEAP-EXHAUSTED in a WITHOUT-INTERRUPTS.");
        /* available and requested should be double word aligned, thus
           they can passed as fixnums and shifted later. */
        funcall2(StaticSymbolFunction(HEAP_EXHAUSTED_ERROR), available, requested);
        lose("HEAP-EXHAUSTED-ERROR fell through");
    }
}

/* Test whether page 'index' can continue a non-large-object region
 * having specified 'gen' and 'allocated' values. */
static inline boolean
page_extensible_p(page_index_t index, generation_index_t gen, int type) {
#ifdef LISP_FEATURE_BIG_ENDIAN /* TODO: implement the simpler test */
    /* Counterintuitively, gcc prefers to see sequential tests of the bitfields,
     * versus one test "!(p.write_protected | p.pinned)".
     * When expressed as separate tests, it figures out that this can be optimized
     * as an AND. On the other hand, by attempting to *force* it to do that,
     * it shifts each field to the right to line them all up at bit index 0 to
     * test that 1 bit, which is a literal rendering of the user-written code.
     */
    boolean result =
           page_table[index].type == type
        && page_table[index].gen == gen
        && !PAGE_WRITEPROTECTED_P(index)
        && !page_table[index].pinned;
    return result;
#else
    /* Test all 4 conditions above as a single comparison against a mask.
     * (The C compiler doesn't understand how to do that)
     * Any bit that has a 1 in this mask must match the desired input.
     * Lisp allocates to generation 0 which is never write-protected, so both
     * WP bits should be zero. Newspace is not write-protected during GC,
     * however in the case of GC with promotion (raise=1), there may be a page
     * in the 'to' generation that is currently un-write-protected but with
     * write_protected_cleared flag = 1 because it was at some point WP'ed.
     * Those pages are usable, so we do have to mask out the 'cleared' bit.
     * Also, the 'need_zerofill' bit can have any value.
     *
     *      pin -\   /-- need_zerofill
     *            v v
     * #b11111111_10011111
     *             ^ ^^^^^ -- type
     *     WP-clr /
     *
     * The flags reside at 1 byte prior to 'gen' in the page structure.
     */
    int bits_match = ((*(int16_t*)(&page_table[index].gen-1) & 0xFF9F) == ((gen<<8)|type));
    return bits_match && !PAGE_WRITEPROTECTED_P(index);
#endif
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
            // XXX: Prefer to start non-code on new pages.
            //      This is temporary until scavenging of small-object pages
            //      is made a little more intelligent (work in progress).
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

        gc_dcheck(!PAGE_WRITEPROTECTED_P(first_page));
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
            gc_dcheck(!PAGE_WRITEPROTECTED_P(last_page));
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
    if (most_bytes_found_to > next_free_page) {
        next_free_page = most_bytes_found_to;
        set_alloc_pointer((lispobj)(page_address(next_free_page)));
    }
    *restart_page_ptr = most_bytes_found_from;
    return most_bytes_found_to-1;
}

/* Allocate bytes.  All the rest of the special-purpose allocation
 * functions will eventually call this.
 * This entry point is only for use within the GC itself.
 * The Lisp region overflow handler either directly calls gc_alloc_large
 * or closes and opens a region if the allocation is small */

/* TODO: The following objects should be allocated to BOXED pages instead of MIXED pages:
 * - CONS, VALUE-CELL, (COMPLEX INTEGER), RATIO, ARRAY headers
 * - simple-vector that is neither weak nor hashing
 * - wholly-boxed instances
 * These pointer-containing objects MUST NOT be allocated to BOXED pages:
 * - weak-pointer (requires deferred scavenge)
 * - hash-table vector and weak vector (complicated)
 * - closure and funcallable-instance (contains untagged pointer)
 * - symbol (contains encoded pointer)
 * - fdefn (contains untagged pointer)
 *
 * Each BOXED page can be linearly scanned without calling type-specific methods
 * when processing the root set
 */
void *
gc_general_alloc(struct alloc_region* region, sword_t nbytes, int page_type)
{
    if (nbytes >= LARGE_OBJECT_SIZE) {
        /* If this is a normal GC - as opposed to "final" GC just prior to saving
         * a core, then we should never copy a large object (not that that's the best
         * strategy always, because it entirely precludes defragmenting those objects).
         * But unfortunately we can't assert that only small objects are seen here,
         * because genesis does not use large-object pages. So cold-init could fail,
         * depending on whether objects in the cold core are sufficiently large that
         * they ought to have gone on large object pages if they could have. */
        return gc_alloc_large(nbytes, page_type, region, 0);
    }

    void *new_obj = region->free_pointer;
    void *new_free_pointer = (char*)new_obj + nbytes;
    /* Check whether there is room in the current alloc region. */
    if (new_free_pointer <= region->end_addr) {
        /* If so then allocate from the current alloc region. */
        region->free_pointer = new_free_pointer;
        return new_obj;
    }
    /* Else not enough free space in the current region: retry with a
     * new region. */
    ensure_region_closed(region, page_type);
    gc_alloc_new_region(nbytes, page_type, region, 0);
    new_obj = region->free_pointer;
    new_free_pointer = (char*)new_obj + nbytes;
    gc_assert(new_free_pointer <= region->end_addr);
    region->free_pointer = new_free_pointer;
    return new_obj;
}

/* Free any trailing pages of the object starting at 'first_page'
 * that are currently unused due to object shrinkage.
 * Possibly assign different 'gen' and 'allocated' values.
 *
 * maybe_adjust_large_object() specifies 'from_space' for 'new_gen'
 * and copy_possibly_large_object() specifies 'new_space'
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
                page_table[page].type = new_allocated;
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
        gc_assert(!PAGE_WRITEPROTECTED_P(page)); \
        page_table[page].gen = new_gen; \
        page_table[page].type = new_allocated

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
        gc_assert(!PAGE_WRITEPROTECTED_P(page));

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
        page_table[page].words_used_ = 0;
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
copy_possibly_large_object(lispobj object, sword_t nwords,
                           struct alloc_region* region, int page_type)
{
    page_index_t first_page;

    CHECK_COPY_PRECONDITIONS(object, nwords);

    /* Check whether it's a large object. */
    first_page = find_page_index((void *)object);
    gc_assert(first_page >= 0);

    os_vm_size_t nbytes = nwords * N_WORD_BYTES;
    os_vm_size_t rounded = ALIGN_UP(nbytes, GENCGC_PAGE_BYTES);
    if (page_single_obj_p(first_page) &&
        (nbytes >= LARGE_OBJECT_SIZE || (rounded - nbytes < rounded / 128))) {

        // Large BOXED would seem to serve no purpose beyond MIXED
        if (page_type == PAGE_TYPE_BOXED) page_type = PAGE_TYPE_MIXED;
        os_vm_size_t bytes_freed =
          adjust_obj_ptes(first_page, nwords, new_space,
                          SINGLE_OBJECT_FLAG | page_type);

        generations[from_space].bytes_allocated -= (bytes_freed + nbytes);
        generations[new_space].bytes_allocated += nbytes;
        bytes_allocated -= bytes_freed;

        /* Add the region to the new_areas if requested. */
        if (page_type & BOXED_PAGE_FLAG)
            add_new_area(first_page, 0, nbytes);

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

/* a faster version for searching the dynamic space. This will work even
 * if the object is in a current allocation region. */
lispobj *
search_dynamic_space(void *pointer)
{
    page_index_t page_index = find_page_index(pointer);
    lispobj *start;

    /* The address may be invalid, so do some checks. */
    if ((page_index == -1) || page_free_p(page_index))
        return NULL;
    start = (lispobj *)page_scan_start(page_index);
    return gc_search_space(start, pointer);
}

/* Return true if 'addr' has a lowtag and widetag that correspond,
 * given that the words at 'addr' are within range for an allocated page.
 * 'addr' could be a pointer to random data, and this check is merely
 * a heuristic. False positives are possible. */
static inline boolean plausible_tag_p(lispobj addr)
{
    if (listp(addr))
        return is_cons_half(CONS(addr)->car)
            && is_cons_half(CONS(addr)->cdr);
    unsigned char widetag = widetag_of(native_pointer(addr));
    return other_immediate_lowtag_p(widetag)
        && lowtag_of(addr) == LOWTAG_FOR_WIDETAG(widetag);
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
        || (page_table[page].pinned &&
            (page_single_obj_p(page) ||
             (is_code(page_table[page].type) && pin_all_dynamic_space_code)));
}

// Only a bignum, code blob, or vector could be on a single-object page.
#define potential_largeobj_p(w) \
  (w==BIGNUM_WIDETAG || w==CODE_HEADER_WIDETAG || \
   (w>=SIMPLE_VECTOR_WIDETAG && w < COMPLEX_BASE_STRING_WIDETAG))

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
    boolean enforce_lowtag = !is_code(page->type);

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
    if (is_code(page_table[addr_page_index].type)) {
        lispobj* object_start = search_dynamic_space((void*)addr);
        /* This search must not fail. We've already verified that the
         * pointer is within range for its page. */
        gc_assert(object_start);
        // If 'addr' points anywhere beyond the boxed words, it's valid
        if (widetag_of(object_start) != FILLER_WIDETAG
            && (instruction_ptr_p((void*)addr, object_start)
                || addr == make_lispobj(object_start, OTHER_POINTER_LOWTAG)))
            return make_lispobj(object_start, OTHER_POINTER_LOWTAG);
        return 0;
    }

    /* For non-code, the pointer's lowtag and widetag must correspond.
     * The putative object header can safely be read even if it turns out
     * that the pointer is not valid, because 'addr' was in bounds for the page.
     * Note that this can falsely pass if looking at the interior of an unboxed
     * array that masquerades as a Lisp object header by pure luck. */
    if (widetag_of(native_pointer(addr)) != FILLER_WIDETAG
        && plausible_tag_p(addr)) return AMBIGUOUS_POINTER;

    // FIXME: I think there is a window of GC vulnerability regarding FINs
    // and FDEFNs containing executable bytes. In either case if the only pointer
    // to such an object is the program counter, the object could be considered
    // garbage because there is no _tagged_ pointer to it.
    // This is an almost impossible situation to arise, but seems worth some study.

    return 0;
}
#elif defined LISP_FEATURE_PPC64
static inline int untagged_fdefn_p(lispobj addr) {
    return ((addr & LOWTAG_MASK) == 0) && widetag_of((lispobj*)addr) == FDEFN_WIDETAG;
}
/* "Less conservative" than above - only consider code pointers as ambiguous
 * roots, not all pointers.  Eventually every architecture could use this
 * because life is so much easier when on-stack code does not move */
static lispobj conservative_root_p(lispobj addr, page_index_t addr_page_index)
{
    struct page* page = &page_table[addr_page_index];
    // We allow ambiguous pointers to code and untagged fdefn pointers.
    if (!(is_code(page->type) || untagged_fdefn_p(addr))) return 0;

    // quick check 1: within from_space and within page usage
    if ((addr & (GENCGC_PAGE_BYTES - 1)) >= page_bytes_used(addr_page_index) ||
        (compacting_p() && immune_set_memberp(addr_page_index)))
        return 0;
    gc_assert(!(page->type & OPEN_REGION_PAGE_FLAG));

    // Find the containing object, if any
    lispobj* object_start = search_dynamic_space((void*)addr);
    if (!object_start) return 0;

    /* If 'addr' points to object_start exactly or anywhere in
     * the boxed words, then it points to the object */
    if ((lispobj*)addr == object_start || instruction_ptr_p((void*)addr, object_start))
        return compute_lispobj(object_start);
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
    gc_filtered_pins = workspace; // needed for wipe_nonpinned_words
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
                where += OBJECT_SIZE(*where, where);
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
#if 0
    fprintf(stderr, "Sorted pin list (%d):\n", count);
    for (index = 0; index < count; ++index) {
      lispobj* obj = native_pointer(workspace[index]);
      lispobj word = *obj;
      int widetag = header_widetag(word);
      if (is_header(word))
          fprintf(stderr, "%p: %d words (%s)\n", obj,
                  (int)sizetab[widetag](obj), widetag_names[widetag>>2]);
      else
          fprintf(stderr, "%p: (cons)\n", obj);
    }
#endif
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
            lispobj* fwd_where = native_pointer(forwarding_pointer_value(where));
            fprintf(stderr, "%p: -> %p\n", where, fwd_where);
            where += OBJECT_SIZE(*fwd_where, fwd_where);
        } else { // dead object
            fprintf(stderr, "%p: %"OBJ_FMTX" %"OBJ_FMTX"\n", where, where[0], where[1]);
            if (is_header(word)) {
                // Do something interesting
                where += sizetab[header_widetag(word)](where);
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

static void deposit_filler(uword_t addr, sword_t nbytes) {
    if (!nbytes) return;
    gc_assert(nbytes > 0);
    sword_t nwords = nbytes >> WORD_SHIFT;
    visit_freed_objects((char*)addr, nbytes);
    gc_assert((nwords - 1) <= 0x7FFFFF);
    *(lispobj*)addr = (nwords - 1) << N_WIDETAG_BITS | FILLER_WIDETAG;
    page_index_t page = find_page_index((void*)addr);
    if (page_table[page].type == PAGE_TYPE_BOXED) {
        page_index_t last_page = find_page_index((char*)addr + nbytes - 1);
        /* Strictly boxed cards are scanned without respect to object boundaries,
         * so we might need to clobber all pointers that formerly occupied the bytes.
         * This step can be skipped if the space to be cleared does not span cards,
         * because in that case descriptor_scavenge() correctly jumps over the filler.
         * There is conceivably a way to avoid memsetting by ensuring that other
         * cards containing a portion of this filler each have their own
         * FILLER_WIDETAG at the start. But this doesn't happen often */
        if (last_page > page) {
            memset((char*)addr+N_WORD_BYTES, 0, nbytes-N_WORD_BYTES);
        }
    }
}

/* Deposit filler objects on small object pinned pages.
 * Also ensure that no scan_start_offset points to a page in
 * oldspace that will be freed.
 */
static void
wipe_nonpinned_words()
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
        // excluding 'obj'. If obj directly abuts its predecessor then don't.
        deposit_filler(fill_from, (uword_t)obj - fill_from);
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
        size_t nwords = OBJECT_SIZE(*obj, obj);
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
        if ((uword_t)native_pointer(keys[i+1]) < obj_end_pageaddr + GENCGC_PAGE_BYTES) {
            // Next object starts within the same page.
            fill_from = obj_end;
        } else {
            // Next pinned object does not start on the same page this obj ends on.
            // Any bytes following 'obj' up to its page end are garbage.
            uword_t page_end = obj_end_pageaddr + page_bytes_used(end_page_index);
            deposit_filler(obj_end, page_end - obj_end);
            fill_from = page_base(keys[i+1]);
        }
    }
    generations[from_space].bytes_allocated -= bytes_moved;
    generations[new_space].bytes_allocated += bytes_moved;
#undef adjust_gen_usage
#undef page_base
    if (pins_alloc_size) {
        os_deallocate((char*)gc_filtered_pins, pins_alloc_size);
        // Traceroot can't use the pinned objects in this case
        // But hopefully you're not relying on traceroot
        // with thousands of pins. That's not really it's intent
        // (which is to find retention due to paths in the heap)
        gc_filtered_pins = 0;
        gc_pin_count = 0;
        pins_alloc_size = 0;
    }
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
        if (page_table[page].pinned) return;
        sword_t nwords = OBJECT_SIZE(*object_start, object_start);
        maybe_adjust_large_object(object_start, page, nwords);
        page_index_t last_page = find_page_index(object_start + nwords - 1);
        while (page <= last_page) page_table[page++].pinned = 1;
        return;
    }

    // Multi-object page (the usual case) - presence in the hash table is the pinned criterion.
    // The 'pinned' bit is a coarse-grained test of whether to bother looking in the table.
    if (hopscotch_containsp(&pinned_objects, object)) return;

    hopscotch_insert(&pinned_objects, object, 1);
    page_table[page].pinned = 1;
    struct code* maybe_code = (struct code*)native_pointer(object);
    // Avoid iterating over embedded simple-funs until the debug info is set.
    // Prior to that, the unboxed payload will contain random bytes.
    // There can't be references to any of the simple-funs
    // until the object is fully constructed.
    if (widetag_of(&maybe_code->header) == CODE_HEADER_WIDETAG && maybe_code->debug_info) {
        for_each_simple_fun(i, fun, maybe_code, 0, {
            hopscotch_insert(&pinned_objects, make_lispobj(fun, FUN_POINTER_LOWTAG), 1);
            page_table[find_page_index(fun)].pinned = 1;
        })
#ifdef RETURN_PC_WIDETAG
        /* Return PCs can't go in the hash-table, because there's no way to find them.
         * But from_space_p() has to return false on return PCs in pinned code.
         * pinned_p is mostly OK, but it needs to see the 'pinned' bit on for the page
         * having the return PC. There's a chance that we would not set it properly
         * in page-spanning objects, so loop over pages just like for a large object */
        size_t nwords = OBJECT_SIZE(*object_start, object_start);
        page_index_t last_page = find_page_index(object_start + nwords - 1);
        while (page <= last_page) page_table[page++].pinned = 1;
#endif
    }
}

#if !GENCGC_IS_PRECISE || defined LISP_FEATURE_PPC64
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

static boolean NO_SANITIZE_MEMORY preserve_pointer(void *addr)
{
#ifdef LISP_FEATURE_METASPACE
    extern lispobj valid_metaspace_ptr_p(void* addr);
#endif
    page_index_t page = find_page_index(addr);
    if (page < 0) {
        // Though immobile_space_preserve_pointer accepts any pointer,
        // there's a benefit to testing immobile_space_p first
        // because it's inlined. Either is a no-op if no immobile space.
        if (immobile_space_p((lispobj)addr))
            return immobile_space_preserve_pointer(addr);
#ifdef LISP_FEATURE_METASPACE
        // Treat layout pointers as transparent - it's possible that no pointer
        // to a wrapper exists, other than a layout which is in a CPU register.
        if ((uword_t)addr >= METASPACE_START
            && (uword_t)addr < READ_ONLY_SPACE_END
            && lowtag_of((uword_t)addr) == INSTANCE_POINTER_LOWTAG
            && valid_metaspace_ptr_p(addr)) {
            lispobj wrapper = LAYOUT((lispobj)addr)->friend;
            // fprintf(stderr, "stack -> metaspace ptr %p -> %p\n", addr, (void*)wrapper);
            preserve_pointer((void*)wrapper);
        }
#endif
        return 0;
    }
    lispobj object = conservative_root_p((lispobj)addr, page);
    if (!object) return 0;
    if (object != AMBIGUOUS_POINTER) {
        pin_object(object);
        return 1;
    }
    // It's a non-large non-code ambiguous pointer.
    if (compacting_p()) {
        if (!hopscotch_containsp(&pinned_objects, (lispobj)addr)) {
            hopscotch_insert(&pinned_objects, (lispobj)addr, 1);
            page_table[page].pinned = 1;
        }
        return 1;
    }
    // Mark only: search for the object, because the mark bit is stored
    // in the object. Writing to random addresses would be bad.
    lispobj* found = search_dynamic_space(addr);
    // fprintf(stderr, "search %p -> %p\n", addr, found);
    if (found) gc_mark_obj(compute_lispobj(found));
    return found != 0;
}
/* Additional logic for soft marks: any word that is potentially a
 * tagged pointer to a page being written must preserve the mark regardless
 * of what update_writeprotection() thinks. That's because the mark is set
 * prior to storing. If GC occurs in between setting the mark and storing,
 * then resetting the mark would be wrong if the subsequent store
 * creates an old->young pointer.
 * Mark stickiness is checked only once per invocation of collect_garbge(),
 * so it when scanning interrupt contexts for generation 0 but not higher gens.
 * Also note the two scenarios:
 * (1) tagged pointer to a large simple-vector, but we scan card-by-card
 *     for specifically the marked cards.  This has to be checked first
 *     so as not to fail to see subsequent cards if the first is marked.
 * (2) tagged pointer to an object that marks only the page containing
 *     the object base.
 * And note a subtle point: only an already-marked card can acquire stick
 * status. So we can ignore any unmarked (a/k/a WRITEPROTECTED_P) card
 * regardless of a context register pointing to it, because if a mark was not
 * stored, then the pointer was not stored. Without examining the next few
 * instructions, there's no reason even to suppose that a store occurs.
 * It seems like the stop-for-GC handler must be enforcing that GC sees things
 * stored in the correct order for out-of-order memory models */
__attribute__((unused)) static void sticky_preserve_pointer(os_context_register_t word)
{
    if (is_lisp_pointer(word)) {
        page_index_t page = find_page_index((void*)word);
        if (page >= 0 && page_boxed_p(page) // stores to raw bytes are uninteresting
            && (word & (GENCGC_PAGE_BYTES - 1)) < page_bytes_used(page)
            && page_table[page].gen != 0
            && plausible_tag_p(word)) { // "plausible" is good enough
            if (page_single_obj_p(page)) {
                /* if 'word' is the correctly-tagged pointer to the base of a SIMPLE-VECTOR,
                 * then set the sticky mark on every marked page. The only other large
                 * objects are CODE (writes to which are pseudo-atomic),
                 * and BIGNUM (which aren't on boxed pages) */
                lispobj* scan_start = page_scan_start(page);
                if  (widetag_of(scan_start) == SIMPLE_VECTOR_WIDETAG
                     && (uword_t)word == make_lispobj(scan_start, OTHER_POINTER_LOWTAG)) {
                    generation_index_t gen = page_table[page].gen;
                    while (1) {
                        long card = page_to_card_index(page);
                        if (gc_card_mark[card]==CARD_MARKED) gc_card_mark[card]=STICKY_MARK;
                        if (page_ends_contiguous_block_p(page, gen)) return;
                        ++page;
                    }
                }
            } else if (gc_card_mark[addr_to_card_index((void*)word)] == 0) {
                long card = page_to_card_index(page);
                if (gc_card_mark[card]==CARD_MARKED) gc_card_mark[card]=STICKY_MARK;
            }
        }
    }
    preserve_pointer((void*)word);
}
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
        obj = make_lispobj(fun_code_header(object_start), OTHER_POINTER_LOWTAG);
    }
    pin_object(obj);
}


/* Return true if 'ptr' is OK to be on a write-protected page
 * of an object in 'gen'. That is, if the pointer does not point to a younger object.
 * Note: 'ptr' is _sometimes_ an ambiguous pointer - we do not utilize the layout bitmap
 * when scanning instances for pointers, so we will occasionally see a raw word for 'ptr'.
 * Also, 'ptr might not have a lowtag (such as lockfree list node successor), */
static boolean ptr_ok_to_writeprotect(lispobj ptr, generation_index_t gen)
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
                    lispobj* code = fun_code_header(FUNCTION(ptr));
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

    {
    page_index_t page;
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
    /* If any page is referenced from the stack (mark byte = 2), then we're
     * can not apply protection even if we see no witness, because the
     * absence of synchronization between mutator and GC means that the next
     * instruction issued when the mutator resumes might create the witness,
     * and it thinks it already marked a card */
    for (page = first_page; page <= last_page; ++page)
        if (gc_card_mark[page_to_card_index(page)] == STICKY_MARK) return 0;
#else
    /* Skip if any page is pinned.
     * The 'pinned' check is sort of bogus but sort of necessary,
     * but doesn't completely fix the problem that it tries to, which is
     * passing a memory address to the OS for it to write into.
     * An object on a never-written protected page would still fail.
     * It's probably rare to pass boxed pages to the OS, but it could be
     * to read fixnums into a simple-vector. */
    for (page = first_page; page <= last_page; ++page)
        if (page_table[page].pinned) return 0;
#endif
    }

    /* Now we attempt to find any 1 "witness" that the pages should NOT be protected.
     * If such witness is found, then return without doing anything, otherwise
     * apply protection to the range. */
    generation_index_t gen = page_table[first_page].gen;
    while ( where < limit ) {
        lispobj word = *where;
        if (is_cons_half(word)) {
            if (is_lisp_pointer(word) && !ptr_ok_to_writeprotect(word, gen)) return where;
            word = where[1];
            if (is_lisp_pointer(word) && !ptr_ok_to_writeprotect(word, gen)) return where;
            where += 2;
            continue;
        }
        int widetag = widetag_of(where);
        sword_t nwords = sizetab[widetag](where);
        sword_t index;
        if (leaf_obj_widetag_p(widetag)) {
            // Do nothing
        } else if (widetag == CODE_HEADER_WIDETAG) {
            // This function will never be called on a page of code, hence if we
            // see genuine (non-filler) code, that's a bug. */
            if (!filler_obj_p(where)) lose("code @ %p on non-code page", where);
        } else {
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
            if (instanceoid_widetag_p(widetag)) {
                // instance_layout works on funcallable or regular instances
                // and we have to specially check it because it's in the upper
                // bytes of the 0th word.
                lispobj layout = instance_layout(where);
                if (layout) {
                    if (!ptr_ok_to_writeprotect(layout, gen)) return where;
                    if (lockfree_list_node_layout_p(LAYOUT(layout)) &&
                        !ptr_ok_to_writeprotect(((struct instance*)where)
                                                ->slots[INSTANCE_DATA_START], gen))
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
                    !ptr_ok_to_writeprotect(((struct instance*)where)
                                            ->slots[INSTANCE_DATA_START], gen))
                    return where;
            }
#endif
            // Scan all the rest of the words even if some of them are raw bits.
            // At worst this overestimates the set of pointer words.
            for (index=1; index<nwords; ++index) {
                if (is_lisp_pointer(where[index]) && !ptr_ok_to_writeprotect(where[index], gen))
                    return where;
            }
        }
        where += nwords;
    }
    page_index_t page;
    for (page = first_page; page <= last_page; ++page)
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        // Don't worry, the cards are all clean - if any card mark was sticky,
        // then we would have bailed out as the first thing (way up above).
        gc_card_mark[addr_to_card_index(page_address(page))] = CARD_UNMARKED;
#else
        // Try to avoid a system call
        if (!PAGE_WRITEPROTECTED_P(page)) protect_page(page_address(page), page);
#endif
    return 0;
}

/* Decide if this single-object page holds a normal simple-vector.
 * "Normal" now includes non-weak address-insensitive k/v vectors */
static inline boolean large_scannable_vector_p(page_index_t page) {
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
    for (i=first_page+1; i <= last_page; ++i) // last_page is inclusive
        gc_assert(is_code(page_table[i].type));

    lispobj* where = start;
    for (; where < limit; where += sizetab[widetag_of(where)](where)) {
        switch (widetag_of(where)) {
        case CODE_HEADER_WIDETAG:
            if (header_rememberedp(*where)) return;
            break;
        }
    }
    for (i = first_page; i <= last_page; i++)
        SET_PAGE_PROTECTED(i, 1);
}

#ifdef LISP_FEATURE_SOFT_CARD_MARKS
# define sticky_marked_p(page) (gc_card_mark[page_to_card_index(page)] == STICKY_MARK)
#else
# define sticky_marked_p(page) 0
#endif
extern int descriptors_scavenge(lispobj *, lispobj*, generation_index_t, int);

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
static page_index_t scan_boxed_root_page(page_index_t page, generation_index_t gen)
{
    __attribute__((unused)) int prev_marked = 0;
    while (1) {
        lispobj* start = (void*)page_address(page);
        lispobj* end = start + page_words_used(page);
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        long card = addr_to_card_index(start);
        int marked = gc_card_mark[card] != CARD_UNMARKED;
        if (marked || prev_marked) {
            int dirty = descriptors_scavenge(start, end, gen, sticky_marked_p(page));
            /* Card can go from marked to unmarked (just like with physical protection),
             * but also unmarked to marked, if transferring the card mark from the object's
             * header card to a cell in that object on a later card.
             * Lisp is given leeway because marking the header is easier. So the
             * algorithm accepts either way on input, but makes its output canonical.
             * (similar in spirit to Postel's Law) */
            gc_card_mark[card] =
              (gc_card_mark[card] != STICKY_MARK) ? (dirty ? CARD_MARKED : CARD_UNMARKED) :
              STICKY_MARK;
            prev_marked = marked;
        }
#else
        if (!PAGE_WRITEPROTECTED_P(page)) {
            int dirty = descriptors_scavenge(start, end, gen, 0);
            if (!dirty) protect_page(start, page);
        }
#endif
        if (page_ends_contiguous_block_p(page, gen)) return page;
        ++page;
    }
}

/* Large vectors are scanned just like strictly boxed root pages,
 * but even easier because there is no looking back 1 card */
static page_index_t scan_vector_root_page(page_index_t page, generation_index_t gen)
{
#ifndef LISP_FEATURE_SOFT_CARD_MARKS
    return scan_boxed_root_page(page, gen);
#else
    while (1) {
        lispobj* start = (void*)page_address(page);
        long card = addr_to_card_index(start);
        if (gc_card_mark[card] != CARD_UNMARKED) {
            lispobj* end = start + page_words_used(page);
            int dirty = descriptors_scavenge(start, end, gen, sticky_marked_p(page));
            if (!dirty) gc_card_mark[card] = CARD_UNMARKED;
        }
        if (page_ends_contiguous_block_p(page, gen)) return page;
        ++page;
    }
#endif
}

/* Scavenge all generations from FROM to TO, inclusive, except for
 * new_space which needs special handling, as new objects may be
 * added which are not checked here - use scavenge_newspace generation.
 *
 * Write-protected pages should not have any pointers to the
 * from_space so do need scavenging; thus write-protected pages are
 * not always scavenged. There is some code to check that these pages
 * are not written; but to check fully the write-protected pages need
 * to be scavenged by disabling the code to skip them.
 *
 * Under the current scheme when a generation is GCed the younger
 * generations will be empty. So, when a generation is being GCed it
 * is only necessary to scavenge the older generations for pointers
 * not the younger. So a page that does not have pointers to younger
 * generations does not need to be scavenged.
 *
 * The write-protection can be used to note pages that don't have
 * pointers to younger pages. But pages can be written without having
 * pointers to younger generations. After the pages are scavenged here
 * they can be scanned for pointers to younger generations and if
 * there are none the page can be write-protected.
 *
 * One complication is when the newspace is the top temp. generation.
 */
static void
scavenge_root_gens(generation_index_t from, generation_index_t to)
{
    page_index_t i;
    gc_dcheck(compacting_p());

    for (i = 0; i < next_free_page; i++) {
        generation_index_t generation = page_table[i].gen;
        if (page_boxed_p(i)
            && (page_words_used(i) != 0)
            && (generation != new_space)
            && (generation >= from)
            && (generation <= to)) {

            /* This should be the start of a region */
            gc_assert(page_starts_contiguous_block_p(i));

            if (page_table[i].type == PAGE_TYPE_BOXED) {
                i = scan_boxed_root_page(i, generation);
            } else if (page_single_obj_p(i) && large_scannable_vector_p(i)) {
                i = scan_vector_root_page(i, generation);
            } else {
                page_index_t last_page;
                boolean write_protected = 1;
                /* Now work forward until the end of the region */
                for (last_page = i; ; last_page++) {
                    write_protected = write_protected && PAGE_WRITEPROTECTED_P(last_page);
                    if (page_ends_contiguous_block_p(last_page, generation))
                        break;
                }
                if (!write_protected) {
                    lispobj* start = (lispobj*)page_address(i);
                    lispobj* limit =
                        (lispobj*)page_address(last_page) + page_words_used(last_page);
                    heap_scavenge(start, limit);
                    /* Now scan the pages and write protect those that
                     * don't have pointers to younger generations. */
                    if (is_code(page_table[i].type))
                        update_code_writeprotection(i, last_page, start, limit);
                    else
                        update_writeprotection(i, last_page, start, limit);
                }
                i = last_page;
            }
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
            && !PAGE_WRITEPROTECTED_P(i)) {
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

void gc_close_all_regions()
{
    ensure_region_closed(code_region, PAGE_TYPE_CODE);
    ensure_region_closed(boxed_region, PAGE_TYPE_BOXED);
    ensure_region_closed(unboxed_region, PAGE_TYPE_UNBOXED);
    ensure_region_closed(mixed_region, PAGE_TYPE_MIXED);
}

/* Do a complete scavenge of the newspace generation. */
static void
scavenge_newspace(generation_index_t generation)
{
    /* Flush the current regions updating the page table. */
    gc_close_all_regions();

    /* Turn on the recording of new areas. */
    gc_assert(new_areas_index == 0);
    new_areas = new_areas_1;

    /* Start with a full scavenge. */
    newspace_full_scavenge(generation);

    /* Flush the current regions updating the page table. */
    gc_close_all_regions();

    while (1) {
        if (!new_areas_index && !immobile_scav_queue_count) { // possible stopping point
            if (!test_weak_triggers(0, 0))
                break; // no work to do
            // testing of triggers can't detect whether any triggering object
            // actually entails new work - it only knows which triggers were removed
            // from the pending list. So check again if allocations occurred,
            // which is only if not all triggers referenced already-live objects.
            gc_close_all_regions(); // update new_areas from regions
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
            if (gencgc_verbose) {
                SHOW("new_areas overflow, doing full scavenge");
            }

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
                heap_scavenge(start, (lispobj*)((char*)start + size));
            }

        }
        /* Flush the current regions updating the page table. */
        gc_close_all_regions();
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
 * free_oldspace; not sure what effect this has on paging.. */
static void
unprotect_oldspace(void)
{
    page_index_t i;
    char *region_addr = 0;
    __attribute__((unused)) char *page_addr = 0;
    uword_t region_bytes = 0;

    // should never have protection applied to gen0, do so nothing.
    if (from_space == 0) return;

    for (i = 0; i < next_free_page; i++) {
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        if (page_words_used(i) && page_table[i].gen == from_space)
            SET_PAGE_PROTECTED(i, 0);
#else
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
                        os_protect(region_addr, region_bytes, OS_VM_PROT_JIT_ALL);
                        /* First page in new region. */
                        region_addr = page_addr;
                        region_bytes = GENCGC_PAGE_BYTES;
                    }
                }
            }
        }
#endif
    }
    if (region_addr) {
        /* Unprotect last region. */
        os_protect(region_addr, region_bytes, OS_VM_PROT_JIT_ALL);
    }
}

/* Work through all the pages and free any in from_space. This
 * assumes that all objects have been copied or promoted to an older
 * generation. Bytes_allocated and the generation bytes_allocated
 * counter are updated. The number of bytes freed is returned. */
static uword_t
free_oldspace(void)
{
    uword_t bytes_freed = 0;
    page_index_t first_page, last_page;

    first_page = 0;

    do {
        /* Find a first page for the next region of pages. */
        while ((first_page < next_free_page)
               && ((page_words_used(first_page) == 0)
                   || (page_table[first_page].gen != from_space)))
            first_page++;

        if (first_page >= next_free_page)
            break;

        /* Find the last page of this region. */
        last_page = first_page;

        page_bytes_t last_page_bytes;
        do {
            /* Free the page. */
            last_page_bytes = page_bytes_used(last_page);
            bytes_freed += last_page_bytes;
            reset_page_flags(last_page);
            set_page_bytes_used(last_page, 0);
            /* Should already be unprotected by unprotect_oldspace(). */
            gc_assert(!PAGE_WRITEPROTECTED_P(last_page));
            last_page++;
        }
        while ((last_page < next_free_page)
               && page_table[last_page].gen == from_space
               && page_words_used(last_page));

        /* 'last_page' is the exclusive upper bound on the page range starting
         * at 'first'page'. We have an accurate count of the bytes in use on
         * last_page but there may be intervening pages not 100% full which are
         * treated as full. This can spuriously visit some (0 . 0) conses
         * but is otherwise not a big deal */
        visit_freed_objects(page_address(first_page),
                            npage_bytes(last_page-first_page-1) + last_page_bytes);
        first_page = last_page;
    } while (first_page < next_free_page);

    generations[from_space].bytes_allocated -= bytes_freed;
    bytes_allocated -= bytes_freed;
    return bytes_freed;
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
            && page_words_used(page)
            // must not touch a card referenced from the control stack
            // because the next instruction executed by user code
            // might store an old->young pointer.
            && gc_card_mark[page_to_card_index(page)] != STICKY_MARK)
            SET_PAGE_PROTECTED(page, 1);
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
        os_protect(page_address(start), npage_bytes(end - start), OS_VM_PROT_JIT_READ);

        start = end;
    }

    if (gencgc_verbose > 1) {
        printf("HW protected %d, SW protected %d\n", n_hw_prot, n_sw_prot);
        page_index_t __attribute((unused)) n_total, n_protected;
        n_total = count_generation_pages(generation, &n_protected);
    }
#endif
}

#if !GENCGC_IS_PRECISE
static void
preserve_context_registers (void __attribute__((unused)) (*proc)(os_context_register_t),
                            os_context_t __attribute__((unused)) *c)
{
#ifdef LISP_FEATURE_SB_THREAD
    /* On Darwin the signal context isn't a contiguous block of memory,
     * so just preserve_pointering its contents won't be sufficient.
     */
#if defined(LISP_FEATURE_DARWIN)||defined(LISP_FEATURE_WIN32)
#if defined LISP_FEATURE_X86
    proc(*os_context_register_addr(c,reg_EAX));
    proc(*os_context_register_addr(c,reg_ECX));
    proc(*os_context_register_addr(c,reg_EDX));
    proc(*os_context_register_addr(c,reg_EBX));
    proc(*os_context_register_addr(c,reg_ESI));
    proc(*os_context_register_addr(c,reg_EDI));
    proc(*os_context_pc_addr(c));
#elif defined LISP_FEATURE_X86_64
    proc(*os_context_register_addr(c,reg_RAX));
    proc(*os_context_register_addr(c,reg_RCX));
    proc(*os_context_register_addr(c,reg_RDX));
    proc(*os_context_register_addr(c,reg_RBX));
    proc(*os_context_register_addr(c,reg_RSI));
    proc(*os_context_register_addr(c,reg_RDI));
    proc(*os_context_register_addr(c,reg_R8));
    proc(*os_context_register_addr(c,reg_R9));
    proc(*os_context_register_addr(c,reg_R10));
    proc(*os_context_register_addr(c,reg_R11));
    proc(*os_context_register_addr(c,reg_R12));
    proc(*os_context_register_addr(c,reg_R13));
    proc(*os_context_register_addr(c,reg_R14));
    proc(*os_context_register_addr(c,reg_R15));
    proc(*os_context_pc_addr(c));
#else
    #error "preserve_context_registers needs to be tweaked for non-x86 Darwin"
#endif
#endif
#if !defined(LISP_FEATURE_WIN32)
    void **ptr;
    for(ptr = ((void **)(c+1))-1; ptr>=(void **)c; ptr--) {
        proc((os_context_register_t)*ptr);
    }
#endif
#endif // LISP_FEATURE_SB_THREAD
}
#endif

static void
move_pinned_pages_to_newspace()
{
    page_index_t i;

    /* scavenge() will evacuate all oldspace pages, but no newspace
     * pages.  Pinned pages are precisely those pages which must not
     * be evacuated, so move them to newspace directly. */

    for (i = 0; i < next_free_page; i++) {
        /* 'pinned' is cleared lazily, so test the 'gen' field as well. */
        if (page_table[i].gen == from_space && page_table[i].pinned &&
            (page_single_obj_p(i) || (is_code(page_table[i].type)
                                      && pin_all_dynamic_space_code))) {
            page_table[i].gen = new_space;
            /* And since we're moving the pages wholesale, also adjust
             * the generation allocation counters. */
            page_bytes_t used = page_bytes_used(i);
            generations[new_space].bytes_allocated += used;
            generations[from_space].bytes_allocated -= used;
        }
    }
}

lispobj *
dynamic_space_code_from_pc(char *pc)
{
    /* Only look at untagged pointers, otherwise they won't be in the PC.
     * (which is a valid precondition for fixed-length 4-byte instructions,
     * not variable-length) */
    if((long)pc % 4 == 0 && is_code(page_table[find_page_index(pc)].type)) {
        lispobj *object = search_dynamic_space(pc);
        if (object != NULL && widetag_of(object) == CODE_HEADER_WIDETAG)
            return object;
    }

    return NULL;
}

static void __attribute__((unused)) maybe_pin_code(lispobj addr) {
    page_index_t page = find_page_index((char*)addr);

    if (page < 0) return;
    if (immune_set_memberp(page)) return;

    struct code* code = (struct code*)dynamic_space_code_from_pc((char *)addr);
    if (code) {
        pin_exact_root(make_lispobj(code, OTHER_POINTER_LOWTAG));
    }
}

#ifdef LISP_FEATURE_PPC64
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
        // FIXME: if we pick a register to consistently use with m[ft]lr
        // then we would only need to examine that, and LR and CTR here.
        // We may already be consistent, I just don't what the consistency is.
        static int boxed_registers[] = BOXED_REGISTERS;
        for (j = (int)(sizeof boxed_registers / sizeof boxed_registers[0])-1; j >= 0; --j) {
            lispobj word = *os_context_register_addr(context, boxed_registers[j]);
            if (gen == 0) sticky_preserve_pointer(word);
            else preserve_pointer((void*)word);
        }
        preserve_pointer((void*)*os_context_lr_addr(context));
        preserve_pointer((void*)*os_context_ctr_addr(context));
    }
}
#endif

#if GENCGC_IS_PRECISE && !defined(reg_CODE)

static void pin_stack(struct thread* th) {
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
    }

}
#endif

#if !GENCGC_IS_PRECISE
static void NO_SANITIZE_ADDRESS NO_SANITIZE_MEMORY
conservative_stack_scan(struct thread* th,
                        __attribute__((unused)) generation_index_t gen,
                        void* stack_hot_end)
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

    __attribute__((unused)) void (*context_method)(os_context_register_t) =
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        gen == 0 ? sticky_preserve_pointer : (void (*)(os_context_register_t))preserve_pointer;
#else
        (void (*)(os_context_register_t))preserve_pointer;
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
                preserve_context_registers(context_method, context);
        }
    }
#  endif
# elif defined(LISP_FEATURE_SB_THREAD)
    int i;
    for (i = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th))-1; i>=0; i--) {
        os_context_t *c = nth_interrupt_context(i, th);
        preserve_context_registers(context_method, c);
        lispobj* esp1 = (lispobj*) *os_context_register_addr(c,reg_SP);
        if (esp1 >= th->control_stack_start && esp1 < th->control_stack_end && (void*)esp1 < esp)
            esp = esp1;
    }
    if (th == get_sb_vm_thread()) {
        lispobj* esp1 = PTR_ALIGN_DOWN(stack_hot_end, N_WORD_BYTES);
        if ((void*)esp1 < esp) esp = esp1;
    }
# else
    esp = PTR_ALIGN_DOWN(stack_hot_end, N_WORD_BYTES);
# endif
    if (!esp || esp == (void*) -1)
        UNKNOWN_STACK_POINTER_ERROR("garbage_collect", th);

    // Words on the stack which point into the stack are likely
    // frame pointers or alien or DX object pointers. In any case
    // there's no need to call preserve_pointer on them since
    // they definitely don't point to the heap.
    // See the picture at create_thread_struct() as a reminder.
    lispobj exclude_from = (lispobj)th->control_stack_start;
    lispobj exclude_to = (lispobj)th + dynamic_values_bytes;

    lispobj* ptr;
    for (ptr = esp; ptr < th->control_stack_end; ptr++) {
        lispobj word = *ptr;
        // Also note that we can eliminate small fixnums from consideration
        // since there is no memory on the 0th page.
        // (most OSes don't let users map memory there, though they used to).
        if (word >= BACKEND_PAGE_BYTES &&
            !(exclude_from <= word && word < exclude_to)) {
            preserve_pointer((void*)word);
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
                lispobj successor = instance->slots[INSTANCE_DATA_START];
                // Be sure to ignore an uninitialized word containing 0.
                if (successor && fixnump(successor))
                    pin_exact_root(successor | INSTANCE_POINTER_LOWTAG);
            }
        }
    }
}

int show_gc_generation_throughput = 0;
/* Garbage collect a generation. If raise is 0 then the remains of the
 * generation are not raised to the next generation. */
static void NO_SANITIZE_ADDRESS NO_SANITIZE_MEMORY
garbage_collect_generation(generation_index_t generation, int raise,
                           void* approximate_stackptr)
{
    page_index_t i;
    struct thread *th;

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
    if (generation < PSEUDO_STATIC_GENERATION) {

        from_space = generation;
        if (raise)
            new_space = generation+1;
        else
            new_space = SCRATCH_GENERATION;

    /* Change to a new space for allocation, resetting the alloc_start_page */
        gc_alloc_generation = new_space;
        RESET_ALLOC_START_PAGES();

    /* Before any pointers are preserved, the pinned flags on the
     * pages need to be cleared. */
    /* FIXME: consider moving this bitmap into its own range of words,
     * out of the page table. Then we can just bzero() it.
     * This will also obviate the extra test at the comment
     * "pinned is cleared lazily" in move_pinned_pages_to_newspace().
     */
        if (pin_all_dynamic_space_code) {
          /* This needs to happen before ambiguous root pinning, as the mechanisms
           * overlap in a way that all-code pinning wouldn't do the right thing if flipped.
           * FIXME: why would it not? More explanation needed!
           * Code objects should never get into the pins table in this case */
          for (i = 0; i < next_free_page; i++) {
              if (page_table[i].gen == from_space)
                  page_table[i].pinned = page_words_used(i) != 0
                                         && is_code(page_table[i].type);
          }
        } else {
          for (i = 0; i < next_free_page; i++)
              if (page_table[i].gen == from_space) page_table[i].pinned = 0;
        }

    /* Un-write-protect the old-space pages. This is essential for the
     * promoted pages as they may contain pointers into the old-space
     * which need to be scavenged. It also helps avoid unnecessary page
     * faults as forwarding pointers are written into them. They need to
     * be un-protected anyway before unmapping later. */
        if (ENABLE_PAGE_PROTECTION)
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
            conservative_stack_scan(th, generation, approximate_stackptr);
#elif defined LISP_FEATURE_PPC64
            // Pin code if needed
            semiconservative_pin_stack(th, generation);
#elif !defined(reg_CODE)
            pin_stack(th);
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
        // It might be tempting to say that only the SB-THREAD:THREAD instance
        // requires pinning - because right after we access it to extract the
        // primitive thread, we link into all_threads - but it may be that the code
        // emitted by the C compiler in new_thread_trampoline computes untagged pointers
        // when accessing the vector and the start function, so those would not be
        // seen as valid lisp pointers by the implicit pinning logic.
        // And the precisely GC'd platforms would not pin anything from C code.
        // The tests in 'threads.impure.lisp' are good at detecting omissions here.
        if (thing) { // Nothing to worry about when 'thing' is already smashed
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
                preserve_pointer((void*)fun);
#else
                pin_exact_root(fun);
#endif
                // pin_exact_root is more efficient than preserve_pointer()
                // because it does not search for the object.
                pin_exact_root(thing);
                pin_exact_root(info);
                pin_exact_root(lispthread->name);
            }
        }
    }
#endif

    /* Remove any key from pinned_objects this does not identify an object.
     * This is done more efficiently by delaying until after all keys are
     * inserted rather than at each insertion */
    refine_ambiguous_roots();

    if (gencgc_verbose > 1)
        show_pinnedobj_count();

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
            scavenge_interrupt_contexts(th);
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
    if (compacting_p())
        scavenge(lisp_sig_handlers, NSIG);
    else
        gc_mark_range(lisp_sig_handlers, NSIG);

    /* Scavenge the binding stacks. */
    {
        struct thread *th;
        for_each_thread(th) {
            scav_binding_stack((lispobj*)th->binding_stack_start,
                               (lispobj*)get_binding_stack_pointer(th),
                               compacting_p() ? 0 : gc_mark_obj);
#ifdef LISP_FEATURE_SB_THREAD
            /* do the tls as well */
            lispobj* from = &th->lisp_thread;
            lispobj* to = (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th);
            sword_t nwords = to - from;
            if (compacting_p())
                scavenge(from, nwords);
            else
                gc_mark_range(from, nwords);
#endif
        }
    }

    if (!compacting_p()) {
        extern void execute_full_mark_phase();
        extern void execute_full_sweep_phase();
        execute_full_mark_phase();
        execute_full_sweep_phase();
        goto maybe_verify;
    }

    heap_scavenge((lispobj*)NIL_SYMBOL_SLOTS_START, (lispobj*)NIL_SYMBOL_SLOTS_END);
    heap_scavenge((lispobj*)STATIC_SPACE_OBJECTS_START, static_space_free_pointer);

    /* All generations but the generation being GCed need to be
     * scavenged. The new_space generation needs special handling as
     * objects may be moved in - it is handled separately below. */

    // SCRATCH_GENERATION is scavenged in immobile space
    // because pinned objects will already have had their generation
    // number reassigned to that generation if applicable.
    scavenge_immobile_roots(generation+1, SCRATCH_GENERATION);

    scavenge_root_gens(generation+1, PSEUDO_STATIC_GENERATION);
    scavenge_pinned_ranges();
    /* The Lisp start function is stored in the core header, not a static
     * symbol. It is passed to gc_and_save() in this C variable */
    if (lisp_init_function) scavenge(&lisp_init_function, 1);
    if (lisp_package_vector) scavenge(&lisp_package_vector, 1);
    if (gc_object_watcher)  scavenge(&gc_object_watcher, 1);
    if (alloc_profile_data) scavenge(&alloc_profile_data, 1);

    /* If SB-SPROF was used, enliven all pages of code.
     * Note that some objects may have already been transported off the page.
     * Despite the extra scan, it is more efficient than scanning all trace buffers
     * and potentially updating them and/or invalidating hashes */
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
            lispobj* where = (lispobj*)page_address(first);
            lispobj* limit = (lispobj*)page_address(last) + page_words_used(last);
            while (where < limit) {
                if (forwarding_pointer_p(where)) {
                    lispobj* copy = native_pointer(forwarding_pointer_value(where));
                    where += sizetab[widetag_of(copy)](copy);
                } else {
                    sword_t nwords = sizetab[widetag_of(where)](where);
                    if (widetag_of(where) == CODE_HEADER_WIDETAG
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

    scan_binding_stack();
    smash_weak_pointers();
#ifdef LISP_FEATURE_METASPACE
    // *PRIMITIVE-OBJECT-LAYOUTS* (in readonly space) is a root, but it only points
    // to other objects in readonly space; however, those other objects (above
    // the read_only_space_free_pointer) point weakly to dynamic space.
    struct slab_header *slab = (void*)METASPACE_START;
    // This is not maximally efficient, in that it visits all slabs instead of just the
    // used ones, but it's not so bad, because there are only 1024 slabs.
    while ((uword_t)slab < READ_ONLY_SPACE_END) {
        if (slab->sizeclass) {
            lispobj* chunk = (lispobj*)((char*)slab + METASPACE_SLAB_SIZE);
            int i;
            for (i=0; i<slab->capacity; ++i) {
                chunk = (lispobj*)((char*)chunk - slab->chunksize);
                if (chunk[1]) { // in-use chunk
                    // Freeing this layout is more involved than merely zeroing some memory,
                    // because freelists have to be maintained. A finalizer will do that.
                    TEST_WEAK_CELL(chunk[1], chunk[1], 0);
                }
            }
        }
        slab = (void*)((char*)slab + METASPACE_SLAB_SIZE);
    }
#endif
    /* Return private-use pages to the general pool so that Lisp can have them */
    gc_dispose_private_pages();
    cull_weak_hash_tables(weak_ht_alivep_funs);

    wipe_nonpinned_words();
    // Do this last, because until wipe_nonpinned_words() happens,
    // not all page table entries have the 'gen' value updated,
    // which we need to correctly find all old->young pointers.
    sweep_immobile_space(raise);

    ASSERT_REGIONS_CLOSED();
    hopscotch_log_stats(&pinned_objects, "pins");

    /* Free the pages in oldspace, but not those marked pinned. */
    free_oldspace();

    /* If the GC is not raising the age then lower the generation back
     * to its normal generation number */
    struct generation* g = &generations[generation];
    if (!raise) {
        for (i = 0; i < next_free_page; i++)
            if ((page_words_used(i) != 0)
                && (page_table[i].gen == SCRATCH_GENERATION))
                page_table[i].gen = generation;
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
                     "gen%d: %ldw copied in %f sec (%.0f %s/sec),"
                     " %d pinned objects (%ldw), %ldw freed (%.1f%%)\n",
                     generation, gc_copied_nwords, et_sec, speed, units,
                     gc_pin_count, gc_pinned_nwords,
                     bytes_freed >> WORD_SHIFT, pct_freed*100.0);
    write(2, buffer, n);
    }
    gc_copied_nwords = gc_pinned_nwords = 0;
#endif

    /* Reset the alloc_start_page for generation. */
    RESET_ALLOC_START_PAGES();

    /* Set the new gc trigger for the GCed generation. */
    g->gc_trigger = g->bytes_allocated + g->bytes_consed_between_gc;
    g->num_gc = raise ? 0 : (1 + g->num_gc);

maybe_verify:
    if (generation >= verify_gens)
        verify_heap(VERIFY_POST_GC | (generation<<16));
    extern int n_unboxed_instances;
    n_unboxed_instances = 0;
}

static page_index_t
find_next_free_page(void)
{
    page_index_t last_page = -1, i;

    for (i = 0; i < next_free_page; i++)
        if (page_words_used(i) != 0)
            last_page = i;

    /* The last free page is actually the first available page */
    return last_page + 1;
}

/*
 * Supposing the OS can only operate on ranges of a certain granularity
 * (which we call 'gencgc_release_granularity'), then given any page rage,
 * align the lower bound up and the upper down to match the granularity.
 *
 *     |-->| OS page | OS page |<--|
 *
 * If the interior of the aligned range is nonempty,
 * perform three operations: unmap/remap, fill before, fill after.
 * Otherwise, just one operation to fill the whole range.
 *
 * This will make more sense once we do a few other things:
 *  - enable manual card marking in codegen
 *  - disable mmap-based page protection
 *  - enable hugepages (so the OS page is much larger than a card)
 */
static void
remap_page_range (page_index_t from, page_index_t to)
{
    /* There's a mysterious Solaris/x86 problem with using mmap
     * tricks for memory zeroing. See sbcl-devel thread
     * "Re: patch: standalone executable redux".
     */
    /* I have no idea what the issue with Haiku is, but using the simpler
     * zero_pages() works where the unmap,map technique does not. Yet the
     * trick plus a post-check that the pages were correctly zeroed finds
     * no problem at that time. So what's failing later and why??? */
#if defined LISP_FEATURE_SUNOS || defined LISP_FEATURE_HAIKU
    zero_pages(from, to);
#else
    size_t granularity = gencgc_release_granularity;
    // page_address "works" even if 'to' == page_table_pages-1
    char* start = page_address(from);
    char* end   = page_address(to+1);
    char* aligned_start = PTR_ALIGN_UP(start, granularity);
    char* aligned_end   = PTR_ALIGN_DOWN(end, granularity);

    if (aligned_start < aligned_end) {
        zero_range_with_mmap(aligned_start, aligned_end-aligned_start);
        zero_range(start, aligned_start);
        zero_range(aligned_end, end);
    } else {
        zero_pages(from, to);
    }
#endif
    page_index_t i;
    for (i = from; i <= to; i++)
        set_page_need_to_zero(i, 0);
}

static void
remap_free_pages (page_index_t from, page_index_t to)
{
    page_index_t first_page, last_page;

    for (first_page = from; first_page <= to; first_page++) {
        if (!page_free_p(first_page) || !page_need_to_zero(first_page))
            continue;

        last_page = first_page + 1;
        while (page_free_p(last_page) &&
               (last_page <= to) &&
               (page_need_to_zero(last_page)))
            last_page++;

        remap_page_range(first_page, last_page-1);

        first_page = last_page;
    }
}

generation_index_t small_generation_limit = 1;

// one pair of counters per widetag, though we're only tracking code as yet
int n_scav_calls[64], n_scav_skipped[64];
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
    THREAD_JIT(0);
    generation_index_t gen = 0, i;
    boolean gc_mark_only = 0;
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
#ifdef SINGLE_THREAD_MIXED_REGION
    ensure_region_closed(SINGLE_THREAD_MIXED_REGION, PAGE_TYPE_MIXED);
#endif
    struct thread *th;
    for_each_thread(th) {
        ensure_region_closed(&th->mixed_tlab, PAGE_TYPE_MIXED);
        ensure_region_closed(&th->unboxed_tlab, PAGE_TYPE_UNBOXED);
    }
    gc_close_all_regions();

    /* Immobile space generation bits are lazily updated for gen0
       (not touched on every object allocation) so do it now */
    update_immobile_nursery_bits();

    /* Verify the new objects created by Lisp code. */
    if (pre_verify_gen_0)
        verify_heap(VERIFY_PRE_GC);

    if (gencgc_verbose > 1)
        print_generation_stats();

    if (gc_mark_only) {
        garbage_collect_generation(PSEUDO_STATIC_GENERATION, 0, &last_gen);
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

        memset(n_scav_calls, 0, sizeof n_scav_calls);
        memset(n_scav_skipped, 0, sizeof n_scav_skipped);
        garbage_collect_generation(gen, raise, &last_gen);

        if (gencgc_verbose)
            fprintf(stderr,
                    "code scavenged: %d total, %d skipped\n",
                    n_scav_calls[CODE_HEADER_WIDETAG/4],
                    n_scav_skipped[CODE_HEADER_WIDETAG/4]);

        /* Reset the memory age cum_sum. */
        generations[gen].cum_sum_bytes_allocated = 0;

        if (gencgc_verbose > 1) {
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
    page_index_t page;
    for (page=0; page<next_free_page; ++page)
        if (gc_card_mark[page_to_card_index(page)] == STICKY_MARK)
            gc_card_mark[page_to_card_index(page)] = 0;
    }
#endif

    /* Set gc_alloc() back to generation 0. The global regions were
     * already asserted to be closed after each generation's collection.
     * i.e. no more allocations can accidentally occur to any other
     * generation than 0 */
    gc_alloc_generation = 0;

    /* Save the high-water mark before updating next_free_page */
    if (next_free_page > high_water_mark)
        high_water_mark = next_free_page;

    next_free_page = find_next_free_page();
    set_alloc_pointer((lispobj)(page_address(next_free_page)));

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
        remap_free_pages(0, high_water_mark);
        high_water_mark = 0;
    }

    large_allocation = 0;
 finish:
    write_protect_immobile_space();
    gc_active_p = 0;

    if (gc_object_watcher) {
        extern void gc_prove_liveness(void(*)(), lispobj, int, uword_t*, int);
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        gc_prove_liveness(preserve_context_registers,
                          gc_object_watcher,
                          gc_pin_count, gc_filtered_pins,
                          gc_traceroot_criterion);
#else
        gc_prove_liveness(0, gc_object_watcher, 0, 0, gc_traceroot_criterion);
#endif
    }

#ifdef COLLECT_GC_STATS
    struct timespec t_gc_done;
    clock_gettime(CLOCK_MONOTONIC, &t_gc_done);
    long et_nsec = (t_gc_done.tv_sec - t_gc_start.tv_sec)*1000000000
      + (t_gc_done.tv_nsec - t_gc_start.tv_nsec);
    tot_gc_nsec += et_nsec;
#endif

    log_generation_stats(gc_logfile, "=== GC End ===");
    SHOW("returning from collect_garbage");
    // Increment the finalizer runflag.  This acts as a count of the number
    // of GCs as well as a notification to wake the finalizer thread.
    if (finalizer_thread_runflag != 0) {
        int newval = 1 + finalizer_thread_runflag;
        // check if counter wrapped around. Don't store 0 as the new value,
        // as that causes the thread to exit.
        finalizer_thread_runflag = newval ? newval : 1;
    }
    THREAD_JIT(1);
}

/* Initialization of gencgc metadata is split into two steps:
 * 1. gc_init() - allocation of a fixed-address space via mmap(),
 *    failing which there's no reason to go on. (safepoint only)
 * 2. gc_allocate_ptes() - page table entries
 */
void
gc_init(void)
{
#ifdef LISP_FEATURE_WIN32
    InitializeCriticalSection(&free_pages_lock);
#endif
#if defined(LISP_FEATURE_SB_SAFEPOINT)
    extern void safepoint_init(void);
    safepoint_init();
#endif
    // Verify that WP_CLEARED_FLAG agrees with the C compiler's bit packing
    // and that we can compute the correct adddress of the bitfield.
    struct page test;
    unsigned char *pflagbits = (unsigned char*)&test.gen - 1;
    memset(&test, 0, sizeof test);
    *pflagbits = WP_CLEARED_FLAG;
    gc_assert(test.write_protected_cleared);
    gc_assert(leaf_obj_widetag_p(FILLER_WIDETAG));
}

int gc_card_table_nbits;
long gc_card_table_mask;

static void __attribute__((unused)) gcbarrier_patch_code_range(uword_t start, void* limit)
{
    extern void gcbarrier_patch_code(void*, int);
    struct varint_unpacker unpacker;
    struct code* code;
    lispobj *where = (lispobj*)start;
    while (where < (lispobj*)limit) {
        if (widetag_of(where) == CODE_HEADER_WIDETAG && ((struct code*)where)->fixups) {
            code = (struct code*)where;
            varint_unpacker_init(&unpacker, code->fixups);
            // There are two other data streams preceding the one we want
            skip_data_stream(&unpacker);
            skip_data_stream(&unpacker);
            char* instructions = code_text_start(code);
            int prev_loc = 0, loc;
            while (varint_unpack(&unpacker, &loc) && loc != 0) {
                loc += prev_loc;
                prev_loc = loc;
                void* patch_where = instructions + loc;
                gcbarrier_patch_code(patch_where, gc_card_table_nbits);
            }
        }
        where += OBJECT_SIZE(*where, where);
    }
}
static void gc_allocate_ptes()
{
    page_index_t i;

    /* Compute the number of pages needed for the dynamic space.
     * Dynamic space size should be aligned on page size. */
    page_table_pages = dynamic_space_size/GENCGC_PAGE_BYTES;
    gc_assert(dynamic_space_size == npage_bytes(page_table_pages));

    /* Default nursery size to 5% of the total dynamic space size,
     * min 1Mb. */
    bytes_consed_between_gcs = dynamic_space_size/(os_vm_size_t)20;
    if (bytes_consed_between_gcs < (1024*1024))
        bytes_consed_between_gcs = 1024*1024;

    /* The page_table is allocated using "calloc" to zero-initialize it.
     * The C library typically implements this efficiently with mmap() if the
     * size is large enough.  To further avoid touching each page structure
     * until first use, FREE_PAGE_FLAG must be 0, statically asserted here:
     */
#if FREE_PAGE_FLAG != 0
#error "FREE_PAGE_FLAG is not 0"
#endif

    /* An extra struct exists as the end as a sentinel. Its 'scan_start_offset'
     * and 'bytes_used' must be zero.
     * Doing so avoids testing in page_ends_contiguous_block_p() whether the
     * next page_index is within bounds, and whether that page contains data.
     */
    page_table = calloc(1+page_table_pages, sizeof(struct page));
    gc_assert(page_table);

    // The card table size is a power of 2 at *least* as large
    // as the number of cards. These are the default values.
    int nbits = 14;
    long num_gc_cards = 1L << nbits;

    // Sure there's a fancier way to round up to a power-of-2
    // but this is executed exactly once, so KISS.
    while (num_gc_cards < page_table_pages) { ++nbits; num_gc_cards <<= 1; }
    // 2 Gigacards should suffice for now. That would span 2TiB of memory
    // using 1Kb card size, or more if larger card size.
    gc_assert(nbits < 32);
    // If the space size is less than or equal to the number of cards
    // that 'gc_card_table_nbits' cover, we're fine. Otherwise, problem.
    // 'nbits' is what we need, 'gc_card_table_nbits' is what the core was compiled for.
    if (nbits > gc_card_table_nbits) {
        gc_card_table_nbits = nbits;
#if defined LISP_FEATURE_PPC64 || defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64
        // The value needed based on dynamic space size exceeds the value that the
        // core was compiled for, so we need to patch all code blobs.
        gcbarrier_patch_code_range(READ_ONLY_SPACE_START, read_only_space_free_pointer);
        gcbarrier_patch_code_range(STATIC_SPACE_START, static_space_free_pointer);
        gcbarrier_patch_code_range(DYNAMIC_SPACE_START, dynamic_space_free_pointer);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        gcbarrier_patch_code_range(VARYOBJ_SPACE_START, varyobj_free_pointer);
#endif
#endif
    }
    // Regardless of the mask implied by space size, it has to be gc_card_table_nbits wide
    // even if that is excessive - when the core is restarted using a _smaller_ dynamic space
    // size than saved at - otherwise lisp could overrun the mark table.
    num_gc_cards = 1L << gc_card_table_nbits;

    gc_card_table_mask =  num_gc_cards - 1;
    gc_card_mark = calloc(num_gc_cards, 1);
    if (gc_card_mark == NULL)
        lose("failed to calloc() %ld bytes", num_gc_cards);


    gc_common_init();
    hopscotch_create(&pinned_objects, HOPSCOTCH_HASH_FUN_DEFAULT, 0 /* hashset */,
                     32 /* logical bin count */, 0 /* default range */);

    bytes_allocated = 0;

    /* Initialize the generations. */
    for (i = 0; i < NUM_GENERATIONS; i++) {
        struct generation* gen = &generations[i];
        gen->bytes_allocated = 0;
        gen->gc_trigger = 2000000;
        gen->num_gc = 0;
        gen->cum_sum_bytes_allocated = 0;
        /* the tune-able parameters */
        gen->bytes_consed_between_gc
            = bytes_consed_between_gcs/(os_vm_size_t)HIGHEST_NORMAL_GENERATION;
        gen->number_of_gcs_before_promotion = 1;
        gen->minimum_age_before_gc = 0.75;
    }

    /* Initialize gc_alloc. */
    gc_alloc_generation = 0;
    gc_init_region(mixed_region);
    gc_init_region(boxed_region);
    gc_init_region(unboxed_region);
    gc_init_region(code_region);
}


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
static NO_SANITIZE_MEMORY lispobj*
lisp_alloc(int largep, struct alloc_region *region, sword_t nbytes,
           int page_type, struct thread *thread)
{
    os_vm_size_t trigger_bytes = 0;

    gc_assert(nbytes > 0);

    /* Check for alignment allocation problems. */
    gc_assert((((uword_t)region->free_pointer & LOWTAG_MASK) == 0)
              && ((nbytes & LOWTAG_MASK) == 0));

    ++thread->slow_path_allocs;
    if ((os_vm_size_t) nbytes > large_allocation)
        large_allocation = nbytes;

    /* maybe we can do this quickly ... */
    void *new_obj = region->free_pointer;
    char *new_free_pointer = (char*)new_obj + nbytes;
    if (new_free_pointer <= (char*)region->end_addr) {
        region->free_pointer = new_free_pointer;
#ifdef LISP_FEATURE_X86_64
        // Non-code allocations should never get here - it would mean there's
        // something wrong in the inline allocator. This assertion pertains
        // to any architecture that always uses an inline allocator.
        // That's actually most of them, but I haven't tested that they're right.
        // e.g. x86 forgoes inline allocation depending on policy,
        // and git revision 05047647 tweaked the edge case for PPC.
        gc_assert(page_type == PAGE_TYPE_CODE);
#endif
        return(new_obj);        /* yup */
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
            write_TLS(GC_PENDING,T,thread);
            if (read_TLS(GC_INHIBIT,thread) == NIL) {
#ifdef LISP_FEATURE_SB_SAFEPOINT
                thread_register_gc_trigger();
#else
                set_pseudo_atomic_interrupted(thread);
#if GENCGC_IS_PRECISE
                /* PPC calls alloc() from a trap
                 * look up the most context if it's from a trap. */
                {
                    os_context_t *context =
                        thread_interrupt_data(thread).allocation_trap_context;
                    maybe_save_gc_mask_and_block_deferrables
                        (context ? os_context_sigmask_addr(context) : NULL);
                }
#else
                maybe_save_gc_mask_and_block_deferrables(NULL);
#endif
#endif
            }
        }
    }
    int __attribute__((unused)) ret = mutex_acquire(&free_pages_lock);
    gc_assert(ret);
    if (largep)
        new_obj = gc_alloc_large(nbytes, page_type, region, 1);
    else {
        ensure_region_closed(region, page_type);
        gc_alloc_new_region(nbytes, page_type, region, 1);
        new_obj = region->free_pointer;
        new_free_pointer = (char*)new_obj + nbytes;
        gc_assert(new_free_pointer <= (char*)region->end_addr);
        region->free_pointer = new_free_pointer;
        // Refill now if the region is almost empty.
        // This can often avoid the next Lisp -> C -> Lisp round-trip.
        if (addr_diff(region->end_addr, region->free_pointer) <= 4 * N_WORD_BYTES
            && TryEnterCriticalSection(&free_pages_lock)) {
            ensure_region_closed(region, page_type);
            // Request > 4 words, forcing a new page to be claimed.
            gc_alloc_new_region(6 * N_WORD_BYTES, page_type, region, 1);
        }
    }

#if !(defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64 \
      || defined LISP_FEATURE_SPARC || defined LISP_FEATURE_WIN32)
    // Architectures which utilize a trap instruction to invoke the overflow
    // handler use the signal context from which to record a backtrace.
    // That's reliable, but access_control_frame_pointer(thread) isn't.
    // x86[-64] use the ABI frame pointer register which seems not to work
    // for win32, but sb-sprof never did work there anyway.
    extern void allocator_record_backtrace(void*, struct thread*);
    if (gencgc_alloc_profiler && thread->state_word.sprof_enable)
        allocator_record_backtrace(__builtin_frame_address(0), thread);
#endif

    return (new_obj);
}

#ifdef LISP_FEATURE_SB_THREAD
# define TLAB(x) x
#else
# define TLAB(x) SINGLE_THREAD_MIXED_REGION
#endif

// Code allocation is always serialized
#ifdef LISP_FEATURE_WIN32
CRITICAL_SECTION code_allocator_lock; // threads are mandatory for win32
#elif defined LISP_FEATURE_SB_THREAD
static pthread_mutex_t code_allocator_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

#define DEFINE_LISP_ENTRYPOINT(name, largep, tlab, page_type) \
NO_SANITIZE_MEMORY lispobj AMD64_SYSV_ABI *name(sword_t nbytes) { \
    struct thread *self = get_sb_vm_thread(); \
    return lisp_alloc(largep, TLAB(tlab), nbytes, page_type, self); }

DEFINE_LISP_ENTRYPOINT(alloc_unboxed, nbytes >= LARGE_OBJECT_SIZE, &self->unboxed_tlab,
                       PAGE_TYPE_UNBOXED)
DEFINE_LISP_ENTRYPOINT(alloc, nbytes >= LARGE_OBJECT_SIZE, &self->mixed_tlab,
                       PAGE_TYPE_MIXED)
DEFINE_LISP_ENTRYPOINT(alloc_list, 0, &self->mixed_tlab, PAGE_TYPE_MIXED)

lispobj AMD64_SYSV_ABI alloc_code_object(unsigned total_words)
{
    struct thread *th = get_sb_vm_thread();
    // x86-64 uses pseudo-atomic. Others should too, but instead use WITHOUT-GCING
#ifndef LISP_FEATURE_X86_64
    if (read_TLS(GC_INHIBIT, th) == NIL)
        lose("alloc_code_object called with GC enabled.");
#endif

    sword_t nbytes = total_words * N_WORD_BYTES;
    /* Allocations of code are all serialized. We might also acquire
     * free_pages_lock depending on availability of space in the region */
    int result = mutex_acquire(&code_allocator_lock);
    gc_assert(result);
    struct code *code =
        (void*)lisp_alloc(nbytes >= LARGE_OBJECT_SIZE, code_region, nbytes, PAGE_TYPE_CODE, th);
    result = mutex_release(&code_allocator_lock);
    gc_assert(result);
    THREAD_JIT(0);

    code->header = ((uword_t)total_words << CODE_HEADER_SIZE_SHIFT) | CODE_HEADER_WIDETAG;
    // Code pages are not prezeroed, so these assignments are essential to prevent GC
    // from seeing bad pointers if it runs as soon as the mutator allows GC.
    code->boxed_size = 0;
    code->debug_info = 0;
    ((lispobj*)code)[total_words-1] = 0; // zeroize the simple-fun table count
    THREAD_JIT(1);

    return make_lispobj(code, OTHER_POINTER_LOWTAG);
}
/* The two exported "close_region" functions are called from Lisp prior to
 * heap-walking. They must never get interrupted by STOP_FOR_GC while holding
 * either the free page lock or code allocation lock.
 * Normally this is guaranteed by pseudo-atomic, but in the interest of simplicity,
 * these are plain foreign calls without aid of a vop. */
void sync_close_region(struct alloc_region *region, int pt, int block_signals)
{
    sigset_t savedmask;
    int result;
    if (block_signals) block_blockable_signals(&savedmask);
    if (pt == PAGE_TYPE_CODE) {
        result = mutex_acquire(&code_allocator_lock);
        gc_assert(result);
    }
    result = mutex_acquire(&free_pages_lock);
    gc_assert(result);
    ensure_region_closed(region, pt);
    result = mutex_release(&free_pages_lock);
    gc_assert(result);
    if (pt == PAGE_TYPE_CODE) {
        result = mutex_release(&code_allocator_lock);
        gc_assert(result);
    }
    if (block_signals) thread_sigmask(SIG_SETMASK, &savedmask, 0);
}
void close_thread_region() {
    __attribute__((unused)) struct thread *self = get_sb_vm_thread();
    struct alloc_region *region = TLAB(&self->mixed_tlab);
    sync_close_region(region, PAGE_TYPE_MIXED, 1);
}
void close_code_region() {
    sync_close_region(code_region, PAGE_TYPE_CODE, 1);
}

#ifdef LISP_FEATURE_SPARC
void mixed_region_rollback(sword_t size)
{
    struct alloc_region *region = SINGLE_THREAD_MIXED_REGION;
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
extern boolean ignore_memoryfaults_on_unprotected_pages;
boolean ignore_memoryfaults_on_unprotected_pages = 0;

extern boolean continue_after_memoryfault_on_unprotected_pages;
boolean continue_after_memoryfault_on_unprotected_pages = 0;

int gencgc_handle_wp_violation(void* fault_addr)
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
    lose("misuse of mprotect() on dynamic space @ %p", fault_addr);
#else
    // This assertion is almost always correct, but what about a page-spanning
    // object with part on a writable page and part on a physically-protected page?
    // Some of the scavenge functions might not take care to avoid rewriting an
    // unchanged word. So they can fault even though they didn't need to store at all.
    // if (gc_active_p && compacting_p()) lose("unexpected WP fault @ %p during GC", fault_addr);
    gc_assert(!is_code(page_table[page_index].type));
    // There can not be an open region. gc_close_region() does not attempt
    // to flip that bit atomically. Other threads in the wp violation handler
    // concurrently for the same page are fine because they're all doing
    // the same bit operations.
    gc_assert(!(page_table[page_index].type & OPEN_REGION_PAGE_FLAG));
    if (PAGE_WRITEPROTECTED_P(page_index)) {
            unprotect_page_index(page_index);
    } else if (!ignore_memoryfaults_on_unprotected_pages) {
            unsigned char *pflagbits = (unsigned char*)&page_table[page_index].gen - 1;
            unsigned char flagbits = __sync_fetch_and_add(pflagbits, 0);
            /* The only acceptable reason for this signal on a heap
             * access is that GENCGC write-protected the page.
             * However, if two CPUs hit a wp page near-simultaneously,
             * we had better not have the second one lose here if it
             * does this test after the first one has already set wp=0
             */
            if (!(flagbits & WP_CLEARED_FLAG)) {
                void lisp_backtrace(int frames);
                lisp_backtrace(10);
                fprintf(stderr,
                        "Fault @ %p, page %"PAGE_INDEX_FMT" not marked as write-protected:\n"
                        "  mixed_region.first_page: %"PAGE_INDEX_FMT","
                        "  mixed_region.last_page %"PAGE_INDEX_FMT"\n"
                        "  page.scan_start_offset: %"OS_VM_SIZE_FMT"\n"
                        "  page.bytes_used: %u\n"
                        "  page.allocated: %d\n"
                        "  page.write_protected: %d\n"
                        "  page.write_protected_cleared: %d\n"
                        "  page.generation: %d\n",
                        fault_addr,
                        page_index,
                        find_page_index(mixed_region->start_addr),
                        mixed_region->last_page,
                        (uintptr_t)page_scan_start_offset(page_index),
                        page_bytes_used(page_index),
                        page_table[page_index].type,
                        PAGE_WRITEPROTECTED_P(page_index),
                        page_table[page_index].write_protected_cleared,
                        page_table[page_index].gen);
                if (!continue_after_memoryfault_on_unprotected_pages)
                    lose("Feh.");
            }
    }
#endif
    /* Don't worry, we can handle it. */
    return 1;
}
/* This is to be called when we catch a SIGSEGV/SIGBUS, determine that
 * it's not just a case of the program hitting the write barrier, and
 * are about to let Lisp deal with it. It's basically just a
 * convenient place to set a gdb breakpoint. */
void
unhandled_sigmemoryfault(void __attribute__((unused)) *addr)
{}

static void
zero_all_free_ranges() /* called only by gc_and_save() */
{
    page_index_t i;
    for (i = 0; i < next_free_page; i++) {
        char* start = page_address(i);
        char* page_end = start + GENCGC_PAGE_BYTES;
        start += page_bytes_used(i);
        memset(start, 0, page_end-start);
    }
}

/* Things to do before doing a final GC before saving a core (without
 * purify).
 *
 * + Pages in singleton pages aren't moved by the GC, so we need to
 *   unset that flag from all pages.
 * + The pseudo-static generation isn't normally collected, but it seems
 *   reasonable to collect it at least when saving a core. So move the
 *   pages to a normal generation.
 * + Instances on unboxed pages need to have their layout pointer visited,
 *   so all pages have to be turned to boxed.
 */
static void
prepare_for_final_gc ()
{
    page_index_t i;

    prepare_immobile_space_for_final_gc ();
    for (i = 0; i < next_free_page; i++) {
        // Compaction requires that we permit large objects to be copied henceforth.
        // Object of size >= LARGE_OBJECT_SIZE get re-allocated to single-object pages.
        page_table[i].type &= ~SINGLE_OBJECT_FLAG;
        // Turn every page to boxed so that the layouts of instances
        // which were relocated to unboxed pages get scanned and fixed.
        if ((page_table[i].type & PAGE_TYPE_MASK) == PAGE_TYPE_UNBOXED)
            page_table[i].type = PAGE_TYPE_MIXED;
        if (page_table[i].gen == PSEUDO_STATIC_GENERATION) {
            int used = page_bytes_used(i);
            page_table[i].gen = HIGHEST_NORMAL_GENERATION;
            generations[PSEUDO_STATIC_GENERATION].bytes_allocated -= used;
            generations[HIGHEST_NORMAL_GENERATION].bytes_allocated += used;
        }
    }

#ifdef LISP_FEATURE_SB_THREAD
    // Avoid tenuring of otherwise-dead objects referenced by
    // dynamic bindings which disappear on image restart.
    struct thread *thread = get_sb_vm_thread();
    char *start = (char*)&thread->lisp_thread;
    char *end = (char*)thread + dynamic_values_bytes;
    memset(start, 0, end-start);
#endif
    // Make sure that it's done after zeroing above, the GC needs to
    // see a list there
#ifdef PINNED_OBJECTS
    struct thread *th;
    for_each_thread(th) {
        write_TLS(PINNED_OBJECTS, NIL, th);
    }
#endif
}

/* Set this switch to 1 for coalescing of strings dumped to fasl,
 * or 2 for coalescing of those,
 * plus literal strings in code compiled to memory. */
char gc_coalesce_string_literals = 0;

/* Do a non-conservative GC, and then save a core with the initial
 * function being set to the value of 'lisp_init_function' */
void
gc_and_save(char *filename, boolean prepend_runtime,
            boolean save_runtime_options, boolean compressed,
            int compression_level, int application_type)
{
    FILE *file;
    void *runtime_bytes = NULL;
    size_t runtime_size;
    extern void coalesce_similar_objects();
    boolean verbose = !lisp_startup_options.noinform;

    file = prepare_to_save(filename, prepend_runtime, &runtime_bytes,
                           &runtime_size);
    if (file == NULL)
       return;

    /* The filename might come from Lisp, and be moved by the now
     * non-conservative GC. */
    filename = strdup(filename);

    /* We're committed to process death at this point, and interrupts can not
     * possibly be handled in Lisp. Let the installed handler closures become
     * garbage, since new ones will be made by ENABLE-INTERRUPT on restart */
#ifndef LISP_FEATURE_WIN32
    {
        int i;
        for (i=0; i<NSIG; ++i)
            lisp_sig_handlers[i] = 0;
    }
#endif

    /* Collect twice: once into relatively high memory, and then back
     * into low memory. This compacts the retained data into the lower
     * pages, minimizing the size of the core file.
     *
     * But note: There is no assurance that this technique actually works,
     * and that the final GC can fit all data below the starting allocation
     * page in the penultimate GC. If it doesn't fit, things are technically
     * ok, but horrible in terms of core file size.  Consider:
     *
     * Penultimate GC: (moves all objects higher in memory)
     *   | ... from_space ... |
     *                        ^--  gencgc_alloc_start_page = next_free_page
     *                        | ... to_space ... |
     *                                           ^ new next_free_page
     *
     * Utimate GC: (moves all objects lower in memory)
     *   | ... to_space ...   | ... from_space ...| ... |
     *                                                  ^ new next_free_page ?
     * Question:
     *  In the ultimate GC, can next_free_page actually increase past
     *  its ending value from the penultimate GC?
     * Answer:
     *  Yes- Suppose the sequence of copying is so adversarial to the allocator
     *  that attempts to fit an object in a region fail often, and require
     *  frequent opening of new regions. (And/or imagine a particularly bad mix
     *  of boxed and non-boxed allocations such that the logic for resuming
     *  at the tail of a partially filled page in gc_find_freeish_pages()
     *  is seldom applicable)  If this occurs, then some allocation must
     *  be on a higher page than all of to_space and from_space.
     *  Then the entire (zeroed) from_space will be present in the saved core
     *  as empty pages, because we can't represent discontiguous ranges.
     */
    conservative_stack = 0;
    /* We MUST collect all generations now, or else the coalescing by similarity
     * would have to be extra cautious not to create any old->young pointers.
     * Resetting oldest_gen_to_gc to its default is legal, because it is merely
     * a hint to the collector that no significant amount of memory would be
     * freed by increasingly aggressive levels of collection. It is NOT a mandate
     * that some objects be retained despite appearing to be unreachable.
     */
    gencgc_oldest_gen_to_gc = HIGHEST_NORMAL_GENERATION;
    // From here on until exit, there is no chance of continuing
    // in Lisp if something goes wrong during GC.
    prepare_for_final_gc();
    unwind_binding_stack();
    gencgc_alloc_start_page = next_free_page;
    collect_garbage(HIGHEST_NORMAL_GENERATION+1);

    THREAD_JIT(0);

    // We always coalesce copyable numbers. Additional coalescing is done
    // only on request, in which case a message is shown (unless verbose=0).
    if (gc_coalesce_string_literals && verbose) {
        printf("[coalescing similar vectors... ");
        fflush(stdout);
    }
    /* FIXME: add comment explaining why coalescing is deferred until
     * after the penultimate GC. Must it wait ? */
    coalesce_similar_objects();
    if (gc_coalesce_string_literals && verbose)
        printf("done]\n");

    /* FIXME: now that relocate_heap() works, can we just memmove() everything
     * down and perform a relocation instead of a collection? */
    if (verbose) { printf("[performing final GC..."); fflush(stdout); }
    prepare_for_final_gc();
    gencgc_alloc_start_page = 0;
    collect_garbage(HIGHEST_NORMAL_GENERATION+1);
#ifdef SINGLE_THREAD_MIXED_REGION // clean up static-space object pre-save.
    gc_init_region(SINGLE_THREAD_MIXED_REGION);
#endif
    /* All global allocation regions should be empty */
    ASSERT_REGIONS_CLOSED();
    // Enforce (rather, warn for lack of) self-containedness of the heap
    verify_heap(VERIFY_FINAL | VERIFY_QUICK);
    if (verbose)
        printf(" done]\n");

    THREAD_JIT(0);
    // Scrub remaining garbage
    zero_all_free_ranges();
    // Assert that defrag will not move the init_function
    gc_assert(!immobile_space_p(lisp_init_function));
    // Defragment and set all objects' generations to pseudo-static
    prepare_immobile_space_for_save(verbose);

#ifdef LISP_FEATURE_X86_64
    untune_asm_routines_for_microarch();
#endif
    os_unlink_runtime();

    /* The number of dynamic space pages saved is based on the allocation
     * pointer, while the number of PTEs is based on next_free_page.
     * Make sure they agree */
    gc_assert((char*)get_alloc_pointer() == page_address(next_free_page));

    if (prepend_runtime)
        save_runtime_to_filehandle(file, runtime_bytes, runtime_size,
                                   application_type);

    save_to_filehandle(file, filename, lisp_init_function,
                       prepend_runtime, save_runtime_options,
                       compressed ? compression_level : COMPRESSION_LEVEL_NONE);
    /* Oops. Save still managed to fail. Since we've mangled the stack
     * beyond hope, there's not much we can do.
     * (beyond FUNCALLing lisp_init_function, but I suspect that's
     * going to be rather unsatisfactory too... */
    lose("Attempt to save core after non-conservative GC failed.");
}

#ifdef LISP_FEATURE_DARWIN_JIT
/* Inexplicably, an executable page can generate spurious faults if
 * it's not written to after changing its protection flags.
 * Touch every page... */
void darwin_jit_code_pages_kludge () {
    THREAD_JIT(0);
    page_index_t page;
    for (page = 0; page  < next_free_page; page++) {
        if(is_code(page_table[page].type)) {
            char* addr = page_address(page);
            for (unsigned i = 0; i < GENCGC_PAGE_BYTES; i+=4096) {
                volatile char* page_start = addr + i;
                page_start[0] = page_start[0];
            }
        }
    }
    THREAD_JIT(1);
}
#endif

/* Read corefile ptes from 'fd' which has already been positioned
 * and store into the page table */
void gc_load_corefile_ptes(int card_table_nbits,
                           core_entry_elt_t n_ptes, core_entry_elt_t total_bytes,
                           os_vm_offset_t offset, int fd)
{
    gc_assert(ALIGN_UP(n_ptes * sizeof (struct corefile_pte), N_WORD_BYTES)
              == (size_t)total_bytes);

    // Allocation of PTEs is delayed 'til now so that calloc() doesn't
    // consume addresses that would have been taken by a mapped space.
    gc_card_table_nbits = card_table_nbits;
    gc_allocate_ptes();

    if (
#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_64_BIT)
        _lseeki64
#else
        lseek
#endif
        (fd, offset, SEEK_SET) != offset)
        lose("failed seek");

    char data[8192];
    // Process an integral number of ptes on each read.
    // Parentheses around sizeof (type) are necessary to suppress a
    // clang warning (-Wsizeof-array-div) that we're dividing the array size
    // by a divisor that is not the size of one element in that array.
    page_index_t max_pages_per_read = sizeof data / (sizeof (struct corefile_pte));
    page_index_t page = 0;
    generation_index_t gen = CORE_PAGE_GENERATION;
    while (page < n_ptes) {
        page_index_t pages_remaining = n_ptes - page;
        page_index_t npages =
            pages_remaining < max_pages_per_read ? pages_remaining : max_pages_per_read;
        ssize_t bytes = npages * sizeof (struct corefile_pte);
        if (read(fd, data, bytes) != bytes) lose("failed read");
        int i;
        for ( i = 0 ; i < npages ; ++i, ++page ) {
            struct corefile_pte pte;
            memcpy(&pte, data+i*sizeof (struct corefile_pte), sizeof pte);
            // Low 3 bits of the scan_start hold the 'type' flags.
            // Low bit of words_used indicates a large (a/k/a single) object.
            char type = ((pte.words_used & 1) ? SINGLE_OBJECT_FLAG : 0)
                        | (pte.sso & 0x07);
            page_table[page].type = type;
            pte.words_used &= ~1;
            /* It is possible, though rare, for the saved page table
             * to contain free pages below alloc_ptr. */
            if (type != FREE_PAGE_FLAG) {
                page_table[page].words_used_ = pte.words_used;
                set_page_scan_start_offset(page, pte.sso & ~0x07);
                page_table[page].gen = gen;
                set_page_need_to_zero(page, 1);
            }
            bytes_allocated += pte.words_used << WORD_SHIFT;
        }
    }
    generations[gen].bytes_allocated = bytes_allocated;
    gc_assert((ssize_t)bytes_allocated <=
              ((char*)get_alloc_pointer() - page_address(0)));
    // write-protecting needs the current value of next_free_page
    next_free_page = n_ptes;
    if (gen != 0 && ENABLE_PAGE_PROTECTION) {
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        page_index_t p;
        for (p = 0; p < next_free_page; ++p)
            if (page_words_used(p)) SET_PAGE_PROTECTED(p, 1);
#else
        // coreparse can avoid hundreds to thousands of mprotect() calls by
        // treating the whole range from the corefile as protectable, except
        // that soft-marked code pages must NOT be subject to mprotect.
        // So just watch out for empty pages and code.  Unboxed object pages
        // will get unprotected on demand.
#define non_protectable_page_p(x) !page_words_used(x) || is_code(page_table[x].type)
        page_index_t start = 0, end;
        // cf. write_protect_generation_pages()
        while (start  < next_free_page) {
#ifdef LISP_FEATURE_DARWIN_JIT
            if(is_code(page_table[start].type)) {
              SET_PAGE_PROTECTED(start,1);
                for (end = start + 1; end < next_free_page; end++) {
                    if (!page_words_used(end) || !is_code(page_table[end].type))
                        break;
                    SET_PAGE_PROTECTED(end,1);
                }
                os_protect(page_address(start), npage_bytes(end - start), OS_VM_PROT_ALL);
                start = end+1;
                continue;
            }
#endif
            if (non_protectable_page_p(start)) {
                ++start;
                continue;
            }
            SET_PAGE_PROTECTED(start,1);
            for (end = start + 1; end < next_free_page; end++) {
                if (non_protectable_page_p(end))
                    break;
                SET_PAGE_PROTECTED(end,1);
            }
            os_protect(page_address(start), npage_bytes(end - start), OS_VM_PROT_JIT_READ);
            start = end;
        }
#endif
    }

#ifdef LISP_FEATURE_DARWIN_JIT
    darwin_jit_code_pages_kludge();
    /* For some reason doing an early pthread_jit_write_protect_np sometimes fails.
       Which is weird, because it's done many times in arch_write_linkage_table_entry later.
       Adding the executable bit here avoids calling pthread_jit_write_protect_np */
    os_protect((os_vm_address_t)STATIC_CODE_SPACE_START, STATIC_CODE_SPACE_SIZE, OS_VM_PROT_ALL);
#endif
}

/* Prepare the array of corefile_ptes for save */
void gc_store_corefile_ptes(struct corefile_pte *ptes)
{
    page_index_t i;
    for (i = 0; i < next_free_page; i++) {
        /* Thanks to alignment requirements, the two three bits
         * are always zero, so we can use them to store the
         * allocation type -- region is always closed, so only
         * the three low bits of allocation flags matter. */
        uword_t word = page_scan_start_offset(i);
        gc_assert((word & 0x07) == 0);
        ptes[i].sso = word | (0x07 & page_table[i].type);
        int used = page_table[i].words_used_;
        gc_assert(!(used & 1));
        ptes[i].words_used = used | page_single_obj_p(i);
    }
}

#define ARTIFICIALLY_HIGH_GEN 8
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
 * specifically a helper function for scav_code_header(), where this is
 * called after scavenging the header. So if something didn't get moved
 * out of from_space, then it must have been pinned.
 * So don't call this for anything except that use-case. */
static inline boolean obj_gen_lessp(lispobj obj, generation_index_t b)
{
    generation_index_t a = gc_gen_of(obj, ARTIFICIALLY_HIGH_GEN);
    if (a == from_space) {
        gc_assert(pinned_p(obj, find_page_index((void*)obj)));
        a  = new_space;
    }
    return ((a==SCRATCH_GENERATION) ? from_space : a) < b;
}

sword_t scav_code_header(lispobj *object, lispobj header)
{
    struct code* code = (struct code*)object;
    if (filler_obj_p(object)) goto done; /* it's not code at all */

    ++n_scav_calls[CODE_HEADER_WIDETAG/4];

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
        ((uword_t)object >= STATIC_SPACE_START && object < static_space_free_pointer)) {
        // FIXME: We sometimes scavenge protected pages.
        // This assertion fails, but things work nonetheless.
        // gc_assert(!card_protected_p(object));

        /* Scavenge the boxed section of the code data block. */
        sword_t n_header_words = code_header_words((struct code *)object);
        scavenge(object + 2, n_header_words - 2);

#ifdef LISP_FEATURE_UNTAGGED_FDEFNS
        // Process each untagged fdefn pointer.
        // TODO: assert that the generation of any fdefn is older than that of 'code'.
        lispobj* fdefns_start = code->constants + code_n_funs(code)
                                * CODE_SLOTS_PER_SIMPLE_FUN;
        int n_fdefns = code_n_named_calls(code);
        int i;
        for (i=0; i<n_fdefns; ++i) {
            lispobj word = fdefns_start[i];
            if ((word & LOWTAG_MASK) == 0 && word != 0) {
                lispobj tagged_word = word | OTHER_POINTER_LOWTAG;
                scavenge(&tagged_word, 1);
                if (tagged_word - OTHER_POINTER_LOWTAG != word) {
                    fdefns_start[i] = tagged_word - OTHER_POINTER_LOWTAG;
                }
            }
        }
#endif

#ifdef LISP_FEATURE_64_BIT
        /* If any function in this code object redirects to a function outside
         * the object, then scavenge all entry points. Otherwise there is no need,
         * as trans_code() made necessary adjustments to internal entry points.
         * This test is just an optimization to avoid some work */
        if (((*object >> 8) & 0xff) == CODE_IS_TRACED) {
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

        /* If my_gen is other than newspace, then scan for old->young
         * pointers. If my_gen is newspace, there can be no such pointers
         * because newspace is the lowest numbered generation post-GC
         * (regardless of whether this is a promotion cycle) */
        if (my_gen != new_space) {
            lispobj *where, *end = object + n_header_words, ptr;
            for (where= object + 2; where < end; ++where)
                if (is_lisp_pointer(ptr = *where) && obj_gen_lessp(ptr, my_gen))
                    goto done;
        }
        CLEAR_WRITTEN_FLAG(object);
    } else {
        ++n_scav_skipped[CODE_HEADER_WIDETAG/4];
    }
done:
    return code_total_nwords(code);
}

#include "verify.inc"
