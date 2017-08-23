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
 * "Uniprocessor Garbage Collection Techniques". As of 20000618, this
 * had been accepted for _ACM Computing Surveys_ and was available
 * as a PostScript preprint through
 *   <http://www.cs.utexas.edu/users/oops/papers.html>
 * as
 *   <ftp://ftp.cs.utexas.edu/pub/garbage/bigsurv.ps>.
 */

#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <inttypes.h>
#include "sbcl.h"
#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD)
#include "pthreads_win32.h"
#else
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
#include "thread.h"
#include "pseudo-atomic.h"
#include "alloc.h"
#include "genesis/gc-tables.h"
#include "genesis/vector.h"
#include "genesis/weak-pointer.h"
#include "genesis/fdefn.h"
#include "genesis/simple-fun.h"
#include "save.h"
#include "genesis/hash-table.h"
#include "genesis/instance.h"
#include "genesis/layout.h"
#include "gencgc.h"
#include "hopscotch.h"
#ifdef GENCGC_IS_PRECISE
#include "genesis/cons.h" /* for accessing *pinned-objects* */
#endif
#include "forwarding-ptr.h"

/* forward declarations */
page_index_t  gc_find_freeish_pages(page_index_t *restart_page_ptr, sword_t nbytes,
                                    int page_type_flag);


/*
 * GC parameters
 */

/* As usually configured, generations 0-5 are normal collected generations,
   6 is pseudo-static (the objects in which are never moved nor reclaimed),
   and 7 is scratch space used when collecting a generation without promotion,
   wherein it is moved to generation 7 and back again.
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
#if QSHOW == 2
boolean gencgc_verbose = 1;
#else
boolean gencgc_verbose = 0;
#endif

/* FIXME: At some point enable the various error-checking things below
 * and see what they say. */

/* We hunt for pointers to old-space, when GCing generations >= verify_gen.
 * Set verify_gens to HIGHEST_NORMAL_GENERATION + 1 to disable this kind of
 * check. */
generation_index_t verify_gens = HIGHEST_NORMAL_GENERATION + 1;

/* Should we do a pre-scan verify of generation 0 before it's GCed? */
boolean pre_verify_gen_0 = 0;

/* Should we check that newly allocated regions are zero filled? */
boolean gencgc_zero_check = 0;

/* Should we check that the free space is zero filled? */
boolean gencgc_enable_verify_zero_fill = 0;

/* When loading a core, don't do a full scan of the memory for the
 * memory region boundaries. (Set to true by coreparse.c if the core
 * contained a pagetable entry).
 */
boolean gencgc_partial_pickup = 0;

/* If defined, free pages are read-protected to ensure that nothing
 * accesses them.
 */

/* #define READ_PROTECT_FREE_PAGES */


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
 * saving a core), don't scan the stack / mark pages dont_move. */
static boolean conservative_stack = 1;

/* An array of page structures is allocated on gc initialization.
 * This helps to quickly map between an address and its page structure.
 * page_table_pages is set from the size of the dynamic space. */
page_index_t page_table_pages;
struct page *page_table;
#ifdef LISP_FEATURE_SB_TRACEROOT
lispobj gc_object_watcher;
int gc_traceroot_criterion;
#endif
#ifdef PIN_GRANULARITY_LISPOBJ
int gc_n_stack_pins;
struct hopscotch_table pinned_objects;
#endif

/* This is always 0 except during gc_and_save() */
lispobj lisp_init_function;

/// Constants defined in gc-internal:
///   #define BOXED_PAGE_FLAG 1
///   #define UNBOXED_PAGE_FLAG 2
///   #define OPEN_REGION_PAGE_FLAG 4

/// Return true if  'allocated' bits are: {001, 010, 011}, false if 1zz or 000.
static inline boolean page_allocated_no_region_p(page_index_t page) {
    return (page_table[page].allocated ^ OPEN_REGION_PAGE_FLAG) > OPEN_REGION_PAGE_FLAG;
}

static inline boolean page_free_p(page_index_t page) {
    return (page_table[page].allocated == FREE_PAGE_FLAG);
}

static inline boolean page_boxed_p(page_index_t page) {
    return (page_table[page].allocated & BOXED_PAGE_FLAG);
}

/// Return true if 'allocated' bits are: {001, 011}, false otherwise.
/// i.e. true of pages which could hold boxed or partially boxed objects.
static inline boolean page_boxed_no_region_p(page_index_t page) {
    return (page_table[page].allocated & 5) == BOXED_PAGE_FLAG;
}

/// Return true if page MUST NOT hold boxed objects (including code).
static inline boolean page_unboxed_p(page_index_t page) {
    /* Both flags set == boxed code page */
    return (page_table[page].allocated & 3) == UNBOXED_PAGE_FLAG;
}

static inline boolean protect_page_p(page_index_t page, generation_index_t generation) {
    return (page_boxed_no_region_p(page)
            && (page_bytes_used(page) != 0)
            && !page_table[page].dont_move
            && (page_table[page].gen == generation));
}

/* Calculate the start address for the given page number. */
inline char *
page_address(page_index_t page_num)
{
    return (void*)(DYNAMIC_SPACE_START + (page_num * GENCGC_CARD_BYTES));
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
page_ends_contiguous_block_p(page_index_t page_index, generation_index_t gen)
{
    return (/* page doesn't fill block */
            (page_bytes_used(page_index) < GENCGC_CARD_BYTES)
            /* page is last allocated page */
            || ((page_index + 1) >= last_free_page)
            /* next page free */
            || page_free_p(page_index + 1)
            /* next page contains no data */
            || (page_bytes_used(page_index + 1) == 0)
            /* next page is in different generation */
            || (page_table[page_index + 1].gen != gen)
            /* next page starts its own contiguous block */
            || (page_starts_contiguous_block_p(page_index + 1)));
}

/// External function for calling from Lisp.
page_index_t ext_find_page_index(void *addr) { return find_page_index(addr); }

static os_vm_size_t
npage_bytes(page_index_t npages)
{
    gc_assert(npages>=0);
    return ((os_vm_size_t)npages)*GENCGC_CARD_BYTES;
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

#ifdef LISP_FEATURE_SEGREGATED_CODE
    // A distinct start page per nonzero value of 'page_type_flag'.
    // The zeroth index is the large object start page.
    page_index_t alloc_start_page_[4];
#define alloc_large_start_page alloc_start_page_[0]
#define alloc_start_page alloc_start_page_[BOXED_PAGE_FLAG]
#define alloc_unboxed_start_page alloc_start_page_[UNBOXED_PAGE_FLAG]
#else
    /* the first page that gc_alloc_large (boxed) considers on its next
     * call. (Although it always allocates after the boxed_region.) */
    page_index_t alloc_large_start_page;

    /* the first page that gc_alloc() checks on its next call */
    page_index_t alloc_start_page;

    /* the first page that gc_alloc_unboxed() checks on its next call */
    page_index_t alloc_unboxed_start_page;
#endif

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

/* META: Is nobody aside from me bothered by this especially misleading
 * use of the word "last"?  It could mean either "ultimate" or "prior",
 * but in fact means neither. It is the *FIRST* page that should be grabbed
 * for more space, so it is min free page, or 1+ the max used page. */
/* The maximum free page in the heap is maintained and used to update
 * ALLOCATION_POINTER which is used by the room function to limit its
 * search of the heap. XX Gencgc obviously needs to be better
 * integrated with the Lisp code. */

page_index_t last_free_page;

#ifdef LISP_FEATURE_SB_THREAD
/* This lock is to prevent multiple threads from simultaneously
 * allocating new regions which overlap each other.  Note that the
 * majority of GC is single-threaded, but alloc() may be called from
 * >1 thread at a time and must be thread-safe.  This lock must be
 * seized before all accesses to generations[] or to parts of
 * page_table[] that other threads may want to see */
static pthread_mutex_t free_pages_lock = PTHREAD_MUTEX_INITIALIZER;
/* This lock is used to protect non-thread-local allocation. */
static pthread_mutex_t allocation_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

extern os_vm_size_t gencgc_release_granularity;
os_vm_size_t gencgc_release_granularity = GENCGC_RELEASE_GRANULARITY;

extern os_vm_size_t gencgc_alloc_granularity;
os_vm_size_t gencgc_alloc_granularity = GENCGC_ALLOC_GRANULARITY;


/*
 * miscellaneous heap functions
 */

/* Count the number of pages which are write-protected within the
 * given generation. */
static page_index_t
count_write_protect_generation_pages(generation_index_t generation)
{
    page_index_t i, count = 0;

    for (i = 0; i < last_free_page; i++)
        if (!page_free_p(i)
            && (page_table[i].gen == generation)
            && page_table[i].write_protected)
            count++;
    return count;
}

/* Count the number of pages within the given generation. */
static page_index_t
count_generation_pages(generation_index_t generation)
{
    page_index_t i;
    page_index_t count = 0;

    for (i = 0; i < last_free_page; i++)
        if (!page_free_p(i)
            && (page_table[i].gen == generation))
            count++;
    return count;
}

#if QSHOW
static page_index_t
count_dont_move_pages(void)
{
    page_index_t i;
    page_index_t count = 0;
    for (i = 0; i < last_free_page; i++) {
        if (!page_free_p(i)
            && (page_table[i].dont_move != 0)) {
            ++count;
        }
    }
    return count;
}
#endif /* QSHOW */

/* Work through the pages and add up the number of bytes used for the
 * given generation. */
static __attribute__((unused)) os_vm_size_t
count_generation_bytes_allocated (generation_index_t gen)
{
    page_index_t i;
    os_vm_size_t result = 0;
    for (i = 0; i < last_free_page; i++) {
        if (!page_free_p(i)
            && (page_table[i].gen == gen))
            result += page_bytes_used(i);
    }
    return result;
}

/* Return the average age of the memory in a generation. */
extern double
generation_average_age(generation_index_t gen)
{
    if (generations[gen].bytes_allocated == 0)
        return 0.0;

    return
        ((double)generations[gen].cum_sum_bytes_allocated)
        / ((double)generations[gen].bytes_allocated);
}

#ifdef LISP_FEATURE_X86
extern void fpu_save(void *);
extern void fpu_restore(void *);
#endif

#define PAGE_INDEX_FMT PRIdPTR

extern void
write_generation_stats(FILE *file)
{
    generation_index_t i;

#ifdef LISP_FEATURE_X86
    int fpu_state[27];

    /* Can end up here after calling alloc_tramp which doesn't prepare
     * the x87 state, and the C ABI uses a different mode */
    fpu_save(fpu_state);
#endif

    /* Print the heap stats. */
    fprintf(file,
            " Gen  StaPg UbSta LaSta Boxed Unbox    LB   LUB !move    Alloc  Waste     Trig   WP GCs Mem-age\n");

    for (i = 0; i <= SCRATCH_GENERATION; i++) {
        page_index_t j;
        page_index_t boxed_cnt = 0;
        page_index_t unboxed_cnt = 0;
        page_index_t large_boxed_cnt = 0;
        page_index_t large_unboxed_cnt = 0;
        page_index_t pinned_cnt=0;

        for (j = 0; j < last_free_page; j++)
            if (page_table[j].gen == i) {

                /* Count the number of boxed pages within the given
                 * generation. */
                if (page_boxed_p(j)) {
                    if (page_table[j].large_object)
                        large_boxed_cnt++;
                    else
                        boxed_cnt++;
                }
                if(page_table[j].dont_move) pinned_cnt++;
                /* Count the number of unboxed pages within the given
                 * generation. */
                if (page_unboxed_p(j)) {
                    if (page_table[j].large_object)
                        large_unboxed_cnt++;
                    else
                        unboxed_cnt++;
                }
            }

        gc_assert(generations[i].bytes_allocated
                  == count_generation_bytes_allocated(i));
        fprintf(file,
                "   %1d: %5ld %5ld %5ld",
                i,
                (long)generations[i].alloc_start_page,
                (long)generations[i].alloc_unboxed_start_page,
                (long)generations[i].alloc_large_start_page);
        fprintf(file,
                " %5"PAGE_INDEX_FMT" %5"PAGE_INDEX_FMT" %5"PAGE_INDEX_FMT
                " %5"PAGE_INDEX_FMT" %5"PAGE_INDEX_FMT,
                boxed_cnt, unboxed_cnt, large_boxed_cnt,
                large_unboxed_cnt, pinned_cnt);
        fprintf(file,
                " %8"OS_VM_SIZE_FMT
                " %6"OS_VM_SIZE_FMT
                " %8"OS_VM_SIZE_FMT
                " %4"PAGE_INDEX_FMT" %3d %7.4f\n",
                generations[i].bytes_allocated,
                (npage_bytes(count_generation_pages(i)) - generations[i].bytes_allocated),
                generations[i].gc_trigger,
                count_write_protect_generation_pages(i),
                generations[i].num_gc,
                generation_average_age(i));
    }
    fprintf(file,"   Total bytes allocated    = %"OS_VM_SIZE_FMT"\n", bytes_allocated);
    fprintf(file,"   Dynamic-space-size bytes = %"OS_VM_SIZE_FMT"\n", dynamic_space_size);

#ifdef LISP_FEATURE_X86
    fpu_restore(fpu_state);
#endif
}

extern void
write_heap_exhaustion_report(FILE *file, long available, long requested,
                             struct thread *thread)
{
    fprintf(file,
            "Heap exhausted during %s: %ld bytes available, %ld requested.\n",
            gc_active_p ? "garbage collection" : "allocation",
            available,
            requested);
    write_generation_stats(file);
    fprintf(file, "GC control variables:\n");
    fprintf(file, "   *GC-INHIBIT* = %s\n   *GC-PENDING* = %s\n",
            SymbolValue(GC_INHIBIT,thread)==NIL ? "false" : "true",
            (SymbolValue(GC_PENDING, thread) == T) ?
            "true" : ((SymbolValue(GC_PENDING, thread) == NIL) ?
                      "false" : "in progress"));
#ifdef LISP_FEATURE_SB_THREAD
    fprintf(file, "   *STOP-FOR-GC-PENDING* = %s\n",
            SymbolValue(STOP_FOR_GC_PENDING,thread)==NIL ? "false" : "true");
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


#if defined(LISP_FEATURE_X86)
void fast_bzero(void*, size_t); /* in <arch>-assem.S */
#endif

/* Zero the pages from START to END (inclusive), but use mmap/munmap instead
 * if zeroing it ourselves, i.e. in practice give the memory back to the
 * OS. Generally done after a large GC.
 */
void zero_pages_with_mmap(page_index_t start, page_index_t end) {
    page_index_t i;
    void *addr = page_address(start), *new_addr;
    os_vm_size_t length = npage_bytes(1+end-start);

    if (start > end)
      return;

    gc_assert(length >= gencgc_release_granularity);
    gc_assert((length % gencgc_release_granularity) == 0);

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
            lose("madvise failed\n");
    } else
#endif
    {
        os_invalidate(addr, length);
        new_addr = os_validate(NOT_MOVABLE, addr, length);
        if (new_addr == NULL || new_addr != addr) {
            lose("remap_free_pages: page moved, 0x%08x ==> 0x%08x",
                 start, new_addr);
        }
    }

    for (i = start; i <= end; i++)
        set_page_need_to_zero(i, 0);
}

/* Zero the pages from START to END (inclusive). Generally done just after
 * a new region has been allocated.
 */
static void
zero_pages(page_index_t start, page_index_t end) {
    if (start > end)
      return;

#if defined(LISP_FEATURE_X86)
    fast_bzero(page_address(start), npage_bytes(1+end-start));
#else
    bzero(page_address(start), npage_bytes(1+end-start));
#endif

}

static void
zero_and_mark_pages(page_index_t start, page_index_t end) {
    page_index_t i;

    zero_pages(start, end);
    for (i = start; i <= end; i++)
        set_page_need_to_zero(i, 0);
}

/* Zero the pages from START to END (inclusive), except for those
 * pages that are known to already zeroed. Mark all pages in the
 * ranges as non-zeroed.
 */
static void
zero_dirty_pages(page_index_t start, page_index_t end) {
    page_index_t i, j;

    for (i = start; i <= end; i++) {
        if (!page_need_to_zero(i)) continue;
        for (j = i+1; (j <= end) && page_need_to_zero(j) ; j++)
            ; /* empty body */
        zero_pages(i, j-1);
        i = j;
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
 * region, the page tables are updated immediately.
 *
 * Unboxed objects don't contain pointers to other objects and so
 * don't need scavenging. Further they can't contain pointers to
 * younger generations so WP is not needed. By allocating pages to
 * unboxed objects the whole page never needs scavenging or
 * write-protecting. */

/* We use either two or three regions for the current newspace generation. */
#ifdef LISP_FEATURE_SEGREGATED_CODE
struct alloc_region gc_alloc_regions[3];
#define boxed_region   gc_alloc_regions[BOXED_PAGE_FLAG-1]
#define unboxed_region gc_alloc_regions[UNBOXED_PAGE_FLAG-1]
#define code_region    gc_alloc_regions[CODE_PAGE_FLAG-1]
#else
struct alloc_region boxed_region;
struct alloc_region unboxed_region;
#endif

/* The generation currently being allocated to. */
static generation_index_t gc_alloc_generation;

static inline page_index_t
generation_alloc_start_page(generation_index_t generation, int page_type_flag, int large)
{
    if (!(page_type_flag >= 1 && page_type_flag <= 3))
        lose("bad page_type_flag: %d", page_type_flag);
    if (large)
        return generations[generation].alloc_large_start_page;
#ifdef LISP_FEATURE_SEGREGATED_CODE
    return generations[generation].alloc_start_page_[page_type_flag];
#else
    if (UNBOXED_PAGE_FLAG == page_type_flag)
        return generations[generation].alloc_unboxed_start_page;
    /* Both code and data. */
    return generations[generation].alloc_start_page;
#endif
}

static inline void
set_generation_alloc_start_page(generation_index_t generation, int page_type_flag, int large,
                                page_index_t page)
{
    if (!(page_type_flag >= 1 && page_type_flag <= 3))
        lose("bad page_type_flag: %d", page_type_flag);
    if (large)
        generations[generation].alloc_large_start_page = page;
#ifdef LISP_FEATURE_SEGREGATED_CODE
    else
        generations[generation].alloc_start_page_[page_type_flag] = page;
#else
    else if (UNBOXED_PAGE_FLAG == page_type_flag)
        generations[generation].alloc_unboxed_start_page = page;
    else /* Both code and data. */
        generations[generation].alloc_start_page = page;
#endif
}

/* Find a new region with room for at least the given number of bytes.
 *
 * It starts looking at the current generation's alloc_start_page. So
 * may pick up from the previous region if there is enough space. This
 * keeps the allocation contiguous when scavenging the newspace.
 *
 * The alloc_region should have been closed by a call to
 * gc_alloc_update_page_tables(), and will thus be in an empty state.
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
static void
gc_alloc_new_region(sword_t nbytes, int page_type_flag, struct alloc_region *alloc_region)
{
    page_index_t first_page;
    page_index_t last_page;
    page_index_t i;
    int ret;

    /*
    FSHOW((stderr,
           "/alloc_new_region for %d bytes from gen %d\n",
           nbytes, gc_alloc_generation));
    */

    /* Check that the region is in a reset state. */
    gc_assert((alloc_region->first_page == 0)
              && (alloc_region->last_page == -1)
              && (alloc_region->free_pointer == alloc_region->end_addr));
    ret = thread_mutex_lock(&free_pages_lock);
    gc_assert(ret == 0);
    first_page = generation_alloc_start_page(gc_alloc_generation, page_type_flag, 0);
    last_page=gc_find_freeish_pages(&first_page, nbytes, page_type_flag);

    /* Set up the alloc_region. */
    alloc_region->first_page = first_page;
    alloc_region->last_page = last_page;
    alloc_region->start_addr = page_address(first_page) + page_bytes_used(first_page);
    alloc_region->free_pointer = alloc_region->start_addr;
    alloc_region->end_addr = page_address(last_page+1);

    /* Set up the pages. */

    /* The first page may have already been in use. */
    /* If so, just assert that it's consistent, otherwise, set it up. */
    if (page_bytes_used(first_page)) {
        gc_assert(page_table[first_page].allocated == page_type_flag);
        gc_assert(page_table[first_page].gen == gc_alloc_generation);
        gc_assert(page_table[first_page].large_object == 0);
    } else {
        page_table[first_page].allocated = page_type_flag;
        page_table[first_page].gen = gc_alloc_generation;
        page_table[first_page].large_object = 0;
        set_page_scan_start_offset(first_page, 0);
    }
    page_table[first_page].allocated |= OPEN_REGION_PAGE_FLAG;

    for (i = first_page+1; i <= last_page; i++) {
        page_table[i].allocated = page_type_flag;
        page_table[i].gen = gc_alloc_generation;
        page_table[i].large_object = 0;
        /* This may not be necessary for unboxed regions (think it was
         * broken before!) */
        set_page_scan_start_offset(i,
            addr_diff(page_address(i), alloc_region->start_addr));
        page_table[i].allocated |= OPEN_REGION_PAGE_FLAG;
    }
    /* Bump up last_free_page. */
    if (last_page+1 > last_free_page) {
        last_free_page = last_page+1;
        /* do we only want to call this on special occasions? like for
         * boxed_region? */
        set_alloc_pointer((lispobj)page_address(last_free_page));
    }
    ret = thread_mutex_unlock(&free_pages_lock);
    gc_assert(ret == 0);

#ifdef READ_PROTECT_FREE_PAGES
    os_protect(page_address(first_page),
               npage_bytes(1+last_page-first_page),
               OS_VM_PROT_ALL);
#endif

    /* If the first page was only partial, don't check whether it's
     * zeroed (it won't be) and don't zero it (since the parts that
     * we're interested in are guaranteed to be zeroed).
     */
    if (page_bytes_used(first_page)) {
        first_page++;
    }

    zero_dirty_pages(first_page, last_page);

    /* we can do this after releasing free_pages_lock */
    if (gencgc_zero_check) {
        lispobj *p;
        for (p = alloc_region->start_addr;
             (void*)p < alloc_region->end_addr; p++) {
            if (*p != 0) {
                lose("The new region is not zero at %p (start=%p, end=%p).\n",
                     p, alloc_region->start_addr, alloc_region->end_addr);
            }
        }
    }
}

/* If the record_new_objects flag is 2 then all new regions created
 * are recorded.
 *
 * If it's 1 then then it is only recorded if the first page of the
 * current region is <= new_areas_ignore_page. This helps avoid
 * unnecessary recording when doing full scavenge pass.
 *
 * The new_object structure holds the page, byte offset, and size of
 * new regions of objects. Each new area is placed in the array of
 * these structures pointer to by new_areas. new_areas_index holds the
 * offset into new_areas.
 *
 * If new_area overflows NUM_NEW_AREAS then it stops adding them. The
 * later code must detect this and handle it, probably by doing a full
 * scavenge of a generation. */
#define NUM_NEW_AREAS 512
static int record_new_objects = 0;
static page_index_t new_areas_ignore_page;
struct new_area {
    page_index_t page;
    size_t offset;
    size_t size;
};
static struct new_area (*new_areas)[];
static size_t new_areas_index;
size_t max_new_areas;

/* Add a new area to new_areas. */
static void
add_new_area(page_index_t first_page, size_t offset, size_t size)
{
    size_t new_area_start, c;
    ssize_t i;

    /* Ignore if full. */
    if (new_areas_index >= NUM_NEW_AREAS)
        return;

    switch (record_new_objects) {
    case 0:
        return;
    case 1:
        if (first_page > new_areas_ignore_page)
            return;
        break;
    case 2:
        break;
    default:
        gc_abort();
    }

    new_area_start = npage_bytes(first_page) + offset;

    /* Search backwards for a prior area that this follows from. If
       found this will save adding a new area. */
    for (i = new_areas_index-1, c = 0; (i >= 0) && (c < 8); i--, c++) {
        size_t area_end =
            npage_bytes((*new_areas)[i].page)
            + (*new_areas)[i].offset
            + (*new_areas)[i].size;
        /*FSHOW((stderr,
               "/add_new_area S1 %d %d %d %d\n",
               i, c, new_area_start, area_end));*/
        if (new_area_start == area_end) {
            /*FSHOW((stderr,
                   "/adding to [%d] %d %d %d with %d %d %d:\n",
                   i,
                   (*new_areas)[i].page,
                   (*new_areas)[i].offset,
                   (*new_areas)[i].size,
                   first_page,
                   offset,
                    size);*/
            (*new_areas)[i].size += size;
            return;
        }
    }

    (*new_areas)[new_areas_index].page = first_page;
    (*new_areas)[new_areas_index].offset = offset;
    (*new_areas)[new_areas_index].size = size;
    /*FSHOW((stderr,
           "/new_area %d page %d offset %d size %d\n",
           new_areas_index, first_page, offset, size));*/
    new_areas_index++;

    /* Note the max new_areas used. */
    if (new_areas_index > max_new_areas)
        max_new_areas = new_areas_index;
}

/* Update the tables for the alloc_region. The region may be added to
 * the new_areas.
 *
 * When done the alloc_region is set up so that the next quick alloc
 * will fail safely and thus a new region will be allocated. Further
 * it is safe to try to re-update the page table of this reset
 * alloc_region. */
void
gc_alloc_update_page_tables(int page_type_flag, struct alloc_region *alloc_region)
{
    boolean more;
    page_index_t first_page;
    page_index_t next_page;
    os_vm_size_t bytes_used;
    os_vm_size_t region_size;
    os_vm_size_t byte_cnt;
    page_bytes_t orig_first_page_bytes_used;
    int ret;


    first_page = alloc_region->first_page;

    /* Catch an unused alloc_region. */
    if ((first_page == 0) && (alloc_region->last_page == -1))
        return;

    next_page = first_page+1;

    ret = thread_mutex_lock(&free_pages_lock);
    gc_assert(ret == 0);
    if (alloc_region->free_pointer != alloc_region->start_addr) {
        /* some bytes were allocated in the region */
        orig_first_page_bytes_used = page_bytes_used(first_page);

        gc_assert(alloc_region->start_addr ==
                  (page_address(first_page) + page_bytes_used(first_page)));

        /* All the pages used need to be updated */

        /* Update the first page. */

        /* If the page was free then set up the gen, and
         * scan_start_offset. */
        if (page_bytes_used(first_page) == 0)
            gc_assert(page_starts_contiguous_block_p(first_page));
        page_table[first_page].allocated &= ~(OPEN_REGION_PAGE_FLAG);

#ifdef LISP_FEATURE_SEGREGATED_CODE
        gc_assert(page_table[first_page].allocated == page_type_flag);
#else
        gc_assert(page_table[first_page].allocated & page_type_flag);
#endif
        gc_assert(page_table[first_page].gen == gc_alloc_generation);
        gc_assert(page_table[first_page].large_object == 0);

        byte_cnt = 0;

        /* Calculate the number of bytes used in this page. This is not
         * always the number of new bytes, unless it was free. */
        more = 0;
        if ((bytes_used = addr_diff(alloc_region->free_pointer,
                                    page_address(first_page)))
            >GENCGC_CARD_BYTES) {
            bytes_used = GENCGC_CARD_BYTES;
            more = 1;
        }
        set_page_bytes_used(first_page, bytes_used);
        byte_cnt += bytes_used;


        /* All the rest of the pages should be free. We need to set
         * their scan_start_offset pointer to the start of the
         * region, and set the bytes_used. */
        while (more) {
            page_table[next_page].allocated &= ~(OPEN_REGION_PAGE_FLAG);
#ifdef LISP_FEATURE_SEGREGATED_CODE
            gc_assert(page_table[next_page].allocated == page_type_flag);
#else
            gc_assert(page_table[next_page].allocated & page_type_flag);
#endif
            gc_assert(page_bytes_used(next_page) == 0);
            gc_assert(page_table[next_page].gen == gc_alloc_generation);
            gc_assert(page_table[next_page].large_object == 0);
            gc_assert(page_scan_start_offset(next_page) ==
                      addr_diff(page_address(next_page),
                                alloc_region->start_addr));

            /* Calculate the number of bytes used in this page. */
            more = 0;
            if ((bytes_used = addr_diff(alloc_region->free_pointer,
                                        page_address(next_page)))>GENCGC_CARD_BYTES) {
                bytes_used = GENCGC_CARD_BYTES;
                more = 1;
            }
            set_page_bytes_used(next_page, bytes_used);
            byte_cnt += bytes_used;

            next_page++;
        }

        region_size = addr_diff(alloc_region->free_pointer,
                                alloc_region->start_addr);
        bytes_allocated += region_size;
        generations[gc_alloc_generation].bytes_allocated += region_size;

        gc_assert((byte_cnt- orig_first_page_bytes_used) == region_size);

        /* Set the generations alloc restart page to the last page of
         * the region. */
        set_generation_alloc_start_page(gc_alloc_generation, page_type_flag, 0, next_page-1);

        /* Add the region to the new_areas if requested. */
        if (BOXED_PAGE_FLAG & page_type_flag)
            add_new_area(first_page,orig_first_page_bytes_used, region_size);

        /*
        FSHOW((stderr,
               "/gc_alloc_update_page_tables update %d bytes to gen %d\n",
               region_size,
               gc_alloc_generation));
        */
    } else {
        /* There are no bytes allocated. Unallocate the first_page if
         * there are 0 bytes_used. */
        page_table[first_page].allocated &= ~(OPEN_REGION_PAGE_FLAG);
        if (page_bytes_used(first_page) == 0)
            page_table[first_page].allocated = FREE_PAGE_FLAG;
    }

    /* Unallocate any unused pages. */
    while (next_page <= alloc_region->last_page) {
        gc_assert(page_bytes_used(next_page) == 0);
        page_table[next_page].allocated = FREE_PAGE_FLAG;
        next_page++;
    }
    ret = thread_mutex_unlock(&free_pages_lock);
    gc_assert(ret == 0);

    /* alloc_region is per-thread, we're ok to do this unlocked */
    gc_set_region_empty(alloc_region);
}

/* Allocate a possibly large object. */
void *
gc_alloc_large(sword_t nbytes, int page_type_flag, struct alloc_region *alloc_region)
{
    boolean more;
    page_index_t first_page, next_page, last_page;
    os_vm_size_t byte_cnt;
    os_vm_size_t bytes_used;
    int ret;

    ret = thread_mutex_lock(&free_pages_lock);
    gc_assert(ret == 0);

    first_page = generation_alloc_start_page(gc_alloc_generation, page_type_flag, 1);
    // FIXME: really we want to try looking for space following the highest of
    // the last page of all other small object regions. That's impossible - there's
    // not enough information. At best we can skip some work in only the case where
    // the supplied region was the one most recently created. To do this right
    // would entail a malloc-like allocator at the page granularity.
    if (first_page <= alloc_region->last_page) {
        first_page = alloc_region->last_page+1;
    }

    last_page=gc_find_freeish_pages(&first_page,nbytes, page_type_flag);

    gc_assert(first_page > alloc_region->last_page);

    set_generation_alloc_start_page(gc_alloc_generation, page_type_flag, 1, last_page);

    /* Large objects don't share pages with other objects. */
    gc_assert(page_bytes_used(first_page) == 0);

    /* Set up the pages. */
    page_table[first_page].allocated = page_type_flag;
    page_table[first_page].gen = gc_alloc_generation;
    page_table[first_page].large_object = 1;
    set_page_scan_start_offset(first_page, 0);

    byte_cnt = 0;

    /* Calc. the number of bytes used in this page. This is not
     * always the number of new bytes, unless it was free. */
    more = 0;
    if ((bytes_used = nbytes) > GENCGC_CARD_BYTES) {
        bytes_used = GENCGC_CARD_BYTES;
        more = 1;
    }
    set_page_bytes_used(first_page, bytes_used);
    byte_cnt += bytes_used;

    next_page = first_page+1;

    /* All the rest of the pages should be free. We need to set their
     * scan_start_offset pointer to the start of the region, and set
     * the bytes_used. */
    while (more) {
        gc_assert(page_free_p(next_page));
        gc_assert(page_bytes_used(next_page) == 0);
        page_table[next_page].allocated = page_type_flag;
        page_table[next_page].gen = gc_alloc_generation;
        page_table[next_page].large_object = 1;

        set_page_scan_start_offset(next_page, npage_bytes(next_page-first_page));

        /* Calculate the number of bytes used in this page. */
        more = 0;
        bytes_used = nbytes - byte_cnt;
        if (bytes_used > GENCGC_CARD_BYTES) {
            bytes_used = GENCGC_CARD_BYTES;
            more = 1;
        }
        set_page_bytes_used(next_page, bytes_used);
        page_table[next_page].write_protected=0;
        page_table[next_page].dont_move=0;
        byte_cnt += bytes_used;
        next_page++;
    }

    gc_assert(byte_cnt == (size_t)nbytes);

    bytes_allocated += nbytes;
    generations[gc_alloc_generation].bytes_allocated += nbytes;

    /* Add the region to the new_areas if requested. */
    if (BOXED_PAGE_FLAG & page_type_flag)
        add_new_area(first_page, 0, nbytes);

    /* Bump up last_free_page */
    if (last_page+1 > last_free_page) {
        last_free_page = last_page+1;
        set_alloc_pointer((lispobj)(page_address(last_free_page)));
    }
    ret = thread_mutex_unlock(&free_pages_lock);
    gc_assert(ret == 0);

#ifdef READ_PROTECT_FREE_PAGES
    os_protect(page_address(first_page),
               npage_bytes(1+last_page-first_page),
               OS_VM_PROT_ALL);
#endif

    zero_dirty_pages(first_page, last_page);

    return page_address(first_page);
}

static page_index_t gencgc_alloc_start_page = -1;

void
gc_heap_exhausted_error_or_lose (sword_t available, sword_t requested)
{
    struct thread *thread = arch_os_get_current_thread();
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
        /* FIXME: assert free_pages_lock held */
        (void)thread_mutex_unlock(&free_pages_lock);
#if !(defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD))
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
        if (SymbolValue(INTERRUPTS_ENABLED,thread) == NIL)
            corruption_warning_and_maybe_lose
                ("Signalling HEAP-EXHAUSTED in a WITHOUT-INTERRUPTS.");
        /* available and requested should be double word aligned, thus
           they can passed as fixnums and shifted later. */
        funcall2(StaticSymbolFunction(HEAP_EXHAUSTED_ERROR), available, requested);
        lose("HEAP-EXHAUSTED-ERROR fell through");
    }
}

page_index_t
gc_find_freeish_pages(page_index_t *restart_page_ptr, sword_t bytes,
                      int page_type_flag)
{
    page_index_t most_bytes_found_from = 0, most_bytes_found_to = 0;
    page_index_t first_page, last_page, restart_page = *restart_page_ptr;
    os_vm_size_t nbytes = bytes;
    os_vm_size_t nbytes_goal = nbytes;
    os_vm_size_t bytes_found = 0;
    os_vm_size_t most_bytes_found = 0;
    boolean small_object = nbytes < GENCGC_CARD_BYTES;
    /* FIXME: assert(free_pages_lock is held); */

    if (nbytes_goal < gencgc_alloc_granularity)
        nbytes_goal = gencgc_alloc_granularity;

    /* Toggled by gc_and_save for heap compaction, normally -1. */
    if (gencgc_alloc_start_page != -1) {
        restart_page = gencgc_alloc_start_page;
    }

    /* FIXME: This is on bytes instead of nbytes pending cleanup of
     * long from the interface. */
    gc_assert(bytes>=0);
    /* Search for a page with at least nbytes of space. We prefer
     * not to split small objects on multiple pages, to reduce the
     * number of contiguous allocation regions spaning multiple
     * pages: this helps avoid excessive conservativism.
     *
     * For other objects, we guarantee that they start on their own
     * page boundary.
     */
    first_page = restart_page;
    while (first_page < page_table_pages) {
        bytes_found = 0;
        if (page_free_p(first_page)) {
                gc_assert(0 == page_bytes_used(first_page));
                bytes_found = GENCGC_CARD_BYTES;
        } else if (small_object &&
                   (page_table[first_page].allocated == page_type_flag) &&
                   (!page_table[first_page].large_object) &&
                   (page_table[first_page].gen == gc_alloc_generation) &&
                   (!page_table[first_page].write_protected) &&
                   (!page_table[first_page].dont_move)) {
            bytes_found = GENCGC_CARD_BYTES - page_bytes_used(first_page);
            if (bytes_found < nbytes) {
                if (bytes_found > most_bytes_found)
                    most_bytes_found = bytes_found;
                first_page++;
                continue;
            }
        } else {
            first_page++;
            continue;
        }

        gc_assert(!page_table[first_page].write_protected);
        for (last_page = first_page+1;
             ((last_page < page_table_pages) &&
              page_free_p(last_page) &&
              (bytes_found < nbytes_goal));
             last_page++) {
            bytes_found += GENCGC_CARD_BYTES;
            gc_assert(0 == page_bytes_used(last_page));
            gc_assert(!page_table[last_page].write_protected);
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
    *restart_page_ptr = most_bytes_found_from;
    return most_bytes_found_to-1;
}

/* Allocate bytes.  All the rest of the special-purpose allocation
 * functions will eventually call this  */

void *
gc_alloc_with_region(sword_t nbytes,int page_type_flag, struct alloc_region *my_region,
                     int quick_p)
{
    void *new_free_pointer;

    if (nbytes>=LARGE_OBJECT_SIZE)
        return gc_alloc_large(nbytes, page_type_flag, my_region);

    /* Check whether there is room in the current alloc region. */
    new_free_pointer = (char*)my_region->free_pointer + nbytes;

    /* fprintf(stderr, "alloc %d bytes from %p to %p\n", nbytes,
       my_region->free_pointer, new_free_pointer); */

    if (new_free_pointer <= my_region->end_addr) {
        /* If so then allocate from the current alloc region. */
        void *new_obj = my_region->free_pointer;
        my_region->free_pointer = new_free_pointer;

        /* Unless a `quick' alloc was requested, check whether the
           alloc region is almost empty. */
        if (!quick_p &&
            addr_diff(my_region->end_addr,my_region->free_pointer) <= 32) {
            /* If so, finished with the current region. */
            gc_alloc_update_page_tables(page_type_flag, my_region);
            /* Set up a new region. */
            gc_alloc_new_region(32 /*bytes*/, page_type_flag, my_region);
        }

        return((void *)new_obj);
    }

    /* Else not enough free space in the current region: retry with a
     * new region. */

    gc_alloc_update_page_tables(page_type_flag, my_region);
    gc_alloc_new_region(nbytes, page_type_flag, my_region);
    return gc_alloc_with_region(nbytes, page_type_flag, my_region,0);
}

/* Copy a large object. If the object is in a large object region then
 * it is simply promoted, else it is copied. If it's large enough then
 * it's copied to a large object region.
 *
 * Bignums and vectors may have shrunk. If the object is not copied
 * the space needs to be reclaimed, and the page_tables corrected. */
static lispobj
general_copy_large_object(lispobj object, sword_t nwords, boolean boxedp)
{
    lispobj *new;
    page_index_t first_page;

    CHECK_COPY_PRECONDITIONS(object, nwords);

    if ((nwords > 1024*1024) && gencgc_verbose) {
        FSHOW((stderr, "/general_copy_large_object: %d bytes\n",
               nwords*N_WORD_BYTES));
    }

    /* Check whether it's a large object. */
    first_page = find_page_index((void *)object);
    gc_assert(first_page >= 0);

    if (page_table[first_page].large_object) {
        /* Promote the object. Note: Unboxed objects may have been
         * allocated to a BOXED region so it may be necessary to
         * change the region to UNBOXED. */
        os_vm_size_t remaining_bytes;
        os_vm_size_t bytes_freed;
        page_index_t next_page;
        page_bytes_t old_bytes_used;

        /* FIXME: This comment is somewhat stale.
         *
         * Note: Any page write-protection must be removed, else a
         * later scavenge_newspace may incorrectly not scavenge these
         * pages. This would not be necessary if they are added to the
         * new areas, but let's do it for them all (they'll probably
         * be written anyway?). */

        gc_assert(page_starts_contiguous_block_p(first_page));
        next_page = first_page;
        remaining_bytes = nwords*N_WORD_BYTES;

        while (remaining_bytes > GENCGC_CARD_BYTES) {
            gc_assert(page_table[next_page].gen == from_space);
            gc_assert(page_table[next_page].large_object);
            gc_assert(page_scan_start_offset(next_page) ==
                      npage_bytes(next_page-first_page));
            gc_assert(page_bytes_used(next_page) == GENCGC_CARD_BYTES);
            /* Should have been unprotected by unprotect_oldspace()
             * for boxed objects, and after promotion unboxed ones
             * should not be on protected pages at all. */
            gc_assert(!page_table[next_page].write_protected);

            if (boxedp)
                gc_assert(page_boxed_p(next_page));
            else {
                gc_assert(page_allocated_no_region_p(next_page));
                page_table[next_page].allocated = UNBOXED_PAGE_FLAG;
            }
            page_table[next_page].gen = new_space;

            remaining_bytes -= GENCGC_CARD_BYTES;
            next_page++;
        }

        /* Now only one page remains, but the object may have shrunk so
         * there may be more unused pages which will be freed. */

        /* Object may have shrunk but shouldn't have grown - check. */
        gc_assert(page_bytes_used(next_page) >= remaining_bytes);

        page_table[next_page].gen = new_space;

        if (boxedp)
            gc_assert(page_boxed_p(next_page));
        else
            page_table[next_page].allocated = UNBOXED_PAGE_FLAG;

        /* Adjust the bytes_used. */
        old_bytes_used = page_bytes_used(next_page);
        set_page_bytes_used(next_page, remaining_bytes);

        bytes_freed = old_bytes_used - remaining_bytes;

        /* Free any remaining pages; needs care. */
        next_page++;
        while ((old_bytes_used == GENCGC_CARD_BYTES) &&
               (page_table[next_page].gen == from_space) &&
               /* FIXME: It is not obvious to me why this is necessary
                * as a loop condition: it seems to me that the
                * scan_start_offset test should be sufficient, but
                * experimentally that is not the case. --NS
                * 2011-11-28 */
               (boxedp ?
                page_boxed_p(next_page) :
                page_allocated_no_region_p(next_page)) &&
               page_table[next_page].large_object &&
               (page_scan_start_offset(next_page) ==
                npage_bytes(next_page - first_page))) {
            /* Checks out OK, free the page. Don't need to both zeroing
             * pages as this should have been done before shrinking the
             * object. These pages shouldn't be write-protected, even if
             * boxed they should be zero filled. */
            gc_assert(!page_table[next_page].write_protected);

            old_bytes_used = page_bytes_used(next_page);
            page_table[next_page].allocated = FREE_PAGE_FLAG;
            set_page_bytes_used(next_page, 0);
            bytes_freed += old_bytes_used;
            next_page++;
        }

        if ((bytes_freed > 0) && gencgc_verbose) {
            FSHOW((stderr,
                   "/general_copy_large_object bytes_freed=%"OS_VM_SIZE_FMT"\n",
                   bytes_freed));
        }

        generations[from_space].bytes_allocated -= nwords*N_WORD_BYTES
            + bytes_freed;
        generations[new_space].bytes_allocated += nwords*N_WORD_BYTES;
        bytes_allocated -= bytes_freed;

        /* Add the region to the new_areas if requested. */
        if (boxedp)
            add_new_area(first_page,0,nwords*N_WORD_BYTES);

        return(object);

    } else {
        /* Allocate space. */
        new = gc_general_alloc(nwords*N_WORD_BYTES,
                               (boxedp ? BOXED_PAGE_FLAG : UNBOXED_PAGE_FLAG),
                               ALLOC_QUICK);

        /* Copy the object. */
        memcpy(new,native_pointer(object),nwords*N_WORD_BYTES);

        /* Return Lisp pointer of new object. */
        return make_lispobj(new, lowtag_of(object));
    }
}

lispobj
copy_large_object(lispobj object, sword_t nwords)
{
    return general_copy_large_object(object, nwords, 1);
}

lispobj
copy_large_unboxed_object(lispobj object, sword_t nwords)
{
    return general_copy_large_object(object, nwords, 0);
}

/* to copy unboxed objects */
lispobj
copy_unboxed_object(lispobj object, sword_t nwords)
{
    return gc_general_copy_object(object, nwords, UNBOXED_PAGE_FLAG);
}

/*
 * weak pointers
 */

/* XX This is a hack adapted from cgc.c. These don't work too
 * efficiently with the gencgc as a list of the weak pointers is
 * maintained within the objects which causes writes to the pages. A
 * limited attempt is made to avoid unnecessary writes, but this needs
 * a re-think. */
/* FIXME: now that we have non-Lisp hashtables in the GC, it might make sense
 * to stop chaining weak pointers through a slot in the object, as a remedy to
 * the above concern. It would also shorten the object by 2 words. */
static sword_t
scav_weak_pointer(lispobj *where, lispobj object)
{
    /* Since we overwrite the 'next' field, we have to make
     * sure not to do so for pointers already in the list.
     * Instead of searching the list of weak_pointers each
     * time, we ensure that next is always NULL when the weak
     * pointer isn't in the list, and not NULL otherwise.
     * Since we can't use NULL to denote end of list, we
     * use a pointer back to the same weak_pointer.
     */
    struct weak_pointer * wp = (struct weak_pointer*)where;

    if (NULL == wp->next && weak_pointer_breakable_p(wp)) {
        wp->next = weak_pointers;
        weak_pointers = wp;
        if (NULL == wp->next)
            wp->next = wp;
    }

    /* Do not let GC scavenge the value slot of the weak pointer.
     * (That is why it is a weak pointer.) */

    return WEAK_POINTER_NWORDS;
}


lispobj *
search_read_only_space(void *pointer)
{
    lispobj *start = (lispobj *) READ_ONLY_SPACE_START;
    lispobj *end = read_only_space_free_pointer;
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return gc_search_space(start, pointer);
}

lispobj *
search_static_space(void *pointer)
{
    lispobj *start = (lispobj *)STATIC_SPACE_START;
    lispobj *end = static_space_free_pointer;
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return gc_search_space(start, pointer);
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

#ifndef GENCGC_IS_PRECISE
// Return the starting address of the object containing 'addr'
// if and only if the object is one which would be evacuated from 'from_space'
// were it allowed to be either discarded as garbage or moved.
// 'addr_page_index' is the page containing 'addr' and must not be -1.
// Return 0 if there is no such object - that is, if addr is past the
// end of the used bytes, or its pages are not in 'from_space' etc.
static lispobj*
conservative_root_p(void *addr, page_index_t addr_page_index)
{
    /* quick check 1: Address is quite likely to have been invalid. */
    struct page* page = &page_table[addr_page_index];
    if (page->gen != from_space ||
#ifdef LISP_FEATURE_SEGREGATED_CODE
        (!is_lisp_pointer((lispobj)addr) && page->allocated != CODE_PAGE_FLAG) ||
#endif
        ((uword_t)addr & (GENCGC_CARD_BYTES - 1)) > page_bytes_used(addr_page_index) ||
        (page->large_object && page->dont_move))
        return 0;
    gc_assert(!(page->allocated & OPEN_REGION_PAGE_FLAG));

#ifdef LISP_FEATURE_SEGREGATED_CODE
    /* quick check 2: Unless the page can hold code, the pointer's lowtag must
     * correspond to the widetag of the object. The object header can safely
     * be read even if it turns out that the pointer is not valid,
     * because the pointer was in bounds for the page.
     * Note that this can falsely pass if looking at the interior of an unboxed
     * array that masquerades as a Lisp object header by pure luck.
     * But if this doesn't pass, there's no point in proceeding to the
     * definitive test which involves searching for the containing object. */

    if (page->allocated != CODE_PAGE_FLAG) {
        lispobj* obj = native_pointer((lispobj)addr);
        if (lowtag_of((lispobj)addr) == LIST_POINTER_LOWTAG) {
            if (!is_cons_half(obj[0]) || !is_cons_half(obj[1]))
                return 0;
        } else {
            unsigned char widetag = widetag_of(*obj);
            if (!other_immediate_lowtag_p(widetag) ||
                lowtag_of((lispobj)addr) != lowtag_for_widetag[widetag>>2])
                return 0;
        }
    }
#endif

    /* Filter out anything which can't be a pointer to a Lisp object
     * (or, as a special case which also requires dont_move, a return
     * address referring to something in a CodeObject). This is
     * expensive but important, since it vastly reduces the
     * probability that random garbage will be bogusly interpreted as
     * a pointer which prevents a page from moving. */
    lispobj* object_start = search_dynamic_space(addr);
    if (!object_start) return 0;

    /* If the containing object is a code object and 'addr' points
     * anywhere beyond the boxed words,
     * presume it to be a valid unboxed return address. */
    if (instruction_ptr_p(addr, object_start))
        return object_start;

    /* Large object pages only contain ONE object, and it will never
     * be a CONS.  However, arrays and bignums can be allocated larger
     * than necessary and then shrunk to fit, leaving what look like
     * (0 . 0) CONSes at the end.  These appear valid to
     * properly_tagged_descriptor_p(), so pick them off here. */
    if (((lowtag_of((lispobj)addr) == LIST_POINTER_LOWTAG) &&
         page_table[addr_page_index].large_object)
        || !properly_tagged_descriptor_p(addr, object_start))
        return 0;

    return object_start;
}
#endif

/* Adjust large bignum and vector objects. This will adjust the
 * allocated region if the size has shrunk, and move unboxed objects
 * into unboxed pages. The pages are not promoted here, and the
 * promoted region is not added to the new_regions; this is really
 * only designed to be called from preserve_pointer(). Shouldn't fail
 * if this is missed, just may delay the moving of objects to unboxed
 * pages, and the freeing of pages. */
static void
maybe_adjust_large_object(page_index_t first_page)
{
    lispobj* where = (lispobj*)page_address(first_page);
    page_index_t next_page;

    uword_t remaining_bytes;
    uword_t bytes_freed;
    uword_t old_bytes_used;

    int page_type_flag;

    /* Check whether it's a vector or bignum object. */
    lispobj widetag = widetag_of(where[0]);
    if (widetag == SIMPLE_VECTOR_WIDETAG)
        page_type_flag = BOXED_PAGE_FLAG;
    else if (specialized_vector_widetag_p(widetag) || widetag == BIGNUM_WIDETAG)
        page_type_flag = UNBOXED_PAGE_FLAG;
    else
        return;

    /* Find its current size. */
    sword_t nwords = sizetab[widetag](where);

    /* Note: Any page write-protection must be removed, else a later
     * scavenge_newspace may incorrectly not scavenge these pages.
     * This would not be necessary if they are added to the new areas,
     * but lets do it for them all (they'll probably be written
     * anyway?). */

    gc_assert(page_starts_contiguous_block_p(first_page));

    next_page = first_page;
    remaining_bytes = nwords*N_WORD_BYTES;
    while (remaining_bytes > GENCGC_CARD_BYTES) {
        gc_assert(page_table[next_page].gen == from_space);
        // We can't assert that page_table[next_page].allocated is correct,
        // because unboxed objects are initially allocated on boxed pages.
        gc_assert(page_allocated_no_region_p(next_page));
        gc_assert(page_table[next_page].large_object);
        gc_assert(page_scan_start_offset(next_page) ==
                  npage_bytes(next_page-first_page));
        gc_assert(page_bytes_used(next_page) == GENCGC_CARD_BYTES);

        // This affects only one object, since large objects don't share pages.
        page_table[next_page].allocated = page_type_flag;

        /* Shouldn't be write-protected at this stage. Essential that the
         * pages aren't. */
        gc_assert(!page_table[next_page].write_protected);
        remaining_bytes -= GENCGC_CARD_BYTES;
        next_page++;
    }

    /* Now only one page remains, but the object may have shrunk so
     * there may be more unused pages which will be freed. */

    /* Object may have shrunk but shouldn't have grown - check. */
    gc_assert(page_bytes_used(next_page) >= remaining_bytes);

    page_table[next_page].allocated = page_type_flag;

    /* Adjust the bytes_used. */
    old_bytes_used = page_bytes_used(next_page);
    set_page_bytes_used(next_page, remaining_bytes);

    bytes_freed = old_bytes_used - remaining_bytes;

    /* Free any remaining pages; needs care. */
    next_page++;
    while ((old_bytes_used == GENCGC_CARD_BYTES) &&
           (page_table[next_page].gen == from_space) &&
           page_allocated_no_region_p(next_page) &&
           page_table[next_page].large_object &&
           (page_scan_start_offset(next_page) ==
            npage_bytes(next_page - first_page))) {
        /* It checks out OK, free the page. We don't need to bother zeroing
         * pages as this should have been done before shrinking the
         * object. These pages shouldn't be write protected as they
         * should be zero filled. */
        gc_assert(!page_table[next_page].write_protected);

        old_bytes_used = page_bytes_used(next_page);
        page_table[next_page].allocated = FREE_PAGE_FLAG;
        set_page_bytes_used(next_page, 0);
        bytes_freed += old_bytes_used;
        next_page++;
    }

    if ((bytes_freed > 0) && gencgc_verbose) {
        FSHOW((stderr,
               "/maybe_adjust_large_object() freed %d\n",
               bytes_freed));
    }

    generations[from_space].bytes_allocated -= bytes_freed;
    bytes_allocated -= bytes_freed;

    return;
}

#ifdef PIN_GRANULARITY_LISPOBJ
/* After scavenging of the roots is done, we go back to the pinned objects
 * and look within them for pointers. While heap_scavenge() could certainly
 * do this, it would potentially lead to extra work, since we can't know
 * whether any given object has been examined at least once, since there is
 * no telltale forwarding-pointer. The easiest thing to do is defer all
 * pinned objects to a subsequent pass, as is done here.
 */
static void
scavenge_pinned_ranges()
{
    int i;
    lispobj key;
    for_each_hopscotch_key(i, key, pinned_objects) {
        lispobj* obj = native_pointer(key);
        lispobj header = *obj;
        // Never invoke scavenger on a simple-fun, just code components.
        if (is_cons_half(header))
            scavenge(obj, 2);
        else if (widetag_of(header) != SIMPLE_FUN_WIDETAG)
            scavtab[widetag_of(header)](obj, header);
    }
}

/* Create an array of fixnum to consume the space between 'from' and 'to' */
static void deposit_filler(uword_t from, uword_t to)
{
    if (to > from) {
        lispobj* where = (lispobj*)from;
        sword_t nwords = (to - from) >> WORD_SHIFT;
        where[0] = SIMPLE_ARRAY_WORD_WIDETAG;
        where[1] = make_fixnum(nwords - 2);
    }
}

/* Zero out the byte ranges on small object pages marked dont_move,
 * carefully skipping over objects in the pin hashtable.
 * TODO: by recording an additional bit per page indicating whether
 * there is more than one pinned object on it, we could avoid qsort()
 * except in the case where there is more than one. */
static void
wipe_nonpinned_words()
{
    void gc_heapsort_uwords(uword_t*, int);
    // Loop over the keys in pinned_objects and pack them densely into
    // the same array - pinned_objects.keys[] - but skip any simple-funs.
    // Admittedly this is abstraction breakage.
    int limit = hopscotch_max_key_index(pinned_objects);
    int n_pins = 0, i;
    for (i = 0; i <= limit; ++i) {
        lispobj key = pinned_objects.keys[i];
        if (key) {
            lispobj* obj = native_pointer(key);
            // No need to check for is_cons_half() - it will be false
            // on a simple-fun header, and that's the correct answer.
            if (widetag_of(*obj) != SIMPLE_FUN_WIDETAG)
                pinned_objects.keys[n_pins++] = (uword_t)obj;
        }
    }
    // Store a sentinel at the end. Even if n_pins = table capacity (unlikely),
    // it is safe to write one more word, because the hops[] array immediately
    // follows the keys[] array in memory.  At worst, 2 elements of hops[]
    // are clobbered, which is irrelevant since the table has already been
    // rendered unusable by stealing its key array for a different purpose.
    pinned_objects.keys[n_pins] = 0;
    // Don't touch pinned_objects.count in case the reset function uses it
    // to decide how to resize for next use (which it doesn't, but could).
    gc_n_stack_pins = n_pins;
    // Order by ascending address, stopping short of the sentinel.
    gc_heapsort_uwords(pinned_objects.keys, n_pins);
#if 0
    printf("Sorted pin list:\n");
    for (i = 0; i < n_pins; ++i) {
      lispobj* obj = (lispobj*)pinned_objects.keys[i];
      if (!is_cons_half(*obj))
           printf("%p: %5d words\n", obj, (int)sizetab[widetag_of(*obj)](obj));
      else printf("%p: CONS\n", obj);
    }
#endif
    // Each entry in the pinned objects demarcates two ranges to be cleared:
    // - the range preceding it back to either the page start, or prior object.
    // - the range after it, up to the lesser of page bytes used or next object.
    uword_t preceding_object = 0;
    uword_t this_page_end = 0;
#define page_base_address(x) (x&~(GENCGC_CARD_BYTES-1))
    for (i = 0; i < n_pins; ++i) {
        // Handle the preceding range. If this object is on the same page as
        // its predecessor, then intervening bytes were already zeroed.
        // If not, then start a new page and do some bookkeeping.
        lispobj* obj = (lispobj*)pinned_objects.keys[i];
        uword_t this_page_base = page_base_address((uword_t)obj);
        /* printf("i=%d obj=%p base=%p\n", i, obj, (void*)this_page_base); */
        if (this_page_base > page_base_address(preceding_object)) {
            deposit_filler(this_page_base, (lispobj)obj);
            // Move the page to newspace
            page_index_t page = find_page_index(obj);
            int used = page_bytes_used(page);
            this_page_end = this_page_base + used;
            /* printf("    Clearing %p .. %p (limit=%p)\n",
               (void*)this_page_base, obj, (void*)this_page_end); */
            generations[new_space].bytes_allocated += used;
            generations[page_table[page].gen].bytes_allocated -= used;
            page_table[page].gen = new_space;
            page_table[page].has_pins = 0;
        }
        // Handle the following range.
        lispobj word = *obj;
        size_t nwords = is_cons_half(word) ? 2 : sizetab[widetag_of(word)](obj);
        uword_t range_start = (uword_t)(obj + nwords);
        uword_t range_end = this_page_end;
        // There is always an i+1'th key due to the sentinel value.
        if (page_base_address(pinned_objects.keys[i+1]) == this_page_base)
            range_end = pinned_objects.keys[i+1];
        /* printf("    Clearing %p .. %p\n", (void*)range_start, (void*)range_end); */
        deposit_filler(range_start, range_end);
        preceding_object = (uword_t)obj;
    }
}

/* Add 'object' to the hashtable, and if the object is a code component,
 * then also add all of the embedded simple-funs.
 * The rationale for the extra work on code components is that without it,
 * every test of pinned_p() on an object would have to check if the pointer
 * is to a simple-fun - entailing an extra read of the header - and mapping
 * to its code component if so.  Since more calls to pinned_p occur than to
 * pin_object, the extra burden should be on this function.
 * Experimentation bears out that this is the better technique.
 * Also, we wouldn't often expect code components in the collected generation
 * so the extra work here is quite minimal, even if it can generally add to
 * the number of keys in the hashtable.
 */
static void
pin_object(lispobj* base_addr)
{
    lispobj object = compute_lispobj(base_addr);
    if (!hopscotch_containsp(&pinned_objects, object)) {
        hopscotch_insert(&pinned_objects, object, 1);
        struct code* maybe_code = (struct code*)native_pointer(object);
        if (widetag_of(maybe_code->header) == CODE_HEADER_WIDETAG) {
          for_each_simple_fun(i, fun, maybe_code, 0, {
              hopscotch_insert(&pinned_objects,
                               make_lispobj(fun, FUN_POINTER_LOWTAG),
                               1);
          })
        }
    }
}
#else
#  define scavenge_pinned_ranges()
#  define wipe_nonpinned_words()
#endif

/* Take a possible pointer to a Lisp object and mark its page in the
 * page_table so that it will not be relocated during a GC.
 *
 * This involves locating the page it points to, then backing up to
 * the start of its region, then marking all pages dont_move from there
 * up to the first page that's not full or has a different generation
 *
 * It is assumed that all the page static flags have been cleared at
 * the start of a GC.
 *
 * It is also assumed that the current gc_alloc() region has been
 * flushed and the tables updated. */

// TODO: there's probably a way to be a little more efficient here.
// As things are, we start by finding the object that encloses 'addr',
// then we see if 'addr' was a "valid" Lisp pointer to that object
// - meaning we expect the correct lowtag on the pointer - except
// that for code objects we don't require a correct lowtag
// and we allow a pointer to anywhere in the object.
//
// It should be possible to avoid calling search_dynamic_space
// more of the time. First, check if the page pointed to might hold code.
// If it does, then we continue regardless of the pointer's lowtag
// (because of the special allowance). If the page definitely does *not*
// hold code, then we require up front that the lowtake make sense,
// by doing the same checks that are in properly_tagged_descriptor_p.
//
// Problem: when code is allocated from a per-thread region,
// does it ensure that the occupied pages are flagged as having code?

#if defined(__GNUC__) && defined(MEMORY_SANITIZER)
#define NO_SANITIZE_MEMORY __attribute__((no_sanitize_memory))
#else
#define NO_SANITIZE_MEMORY
#endif

static void NO_SANITIZE_MEMORY
preserve_pointer(void *addr)
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
  /* Immobile space MUST be lower than dynamic space,
     or else this test needs to be revised */
    if (addr < (void*)IMMOBILE_SPACE_END) {
        extern void immobile_space_preserve_pointer(void*);
        immobile_space_preserve_pointer(addr);
        return;
    }
#endif
    page_index_t addr_page_index = find_page_index(addr);

#ifdef GENCGC_IS_PRECISE
    /* If we're in precise gencgc (non-x86oid as of this writing) then
     * we are only called on valid object pointers in the first place,
     * so we just have to do a bounds-check against the heap, a
     * generation check, and the already-pinned check. */
    if (addr_page_index == -1
        || (page_table[addr_page_index].gen != from_space)
        || page_table[addr_page_index].dont_move)
        return;
#else
    lispobj *object_start;
    if (addr_page_index == -1
        || (object_start = conservative_root_p(addr, addr_page_index)) == 0)
        return;
#endif

    /* (Now that we know that addr_page_index is in range, it's
     * safe to index into page_table[] with it.) */
    unsigned int region_allocation = page_table[addr_page_index].allocated;

    /* Find the beginning of the region.  Note that there may be
     * objects in the region preceding the one that we were passed a
     * pointer to: if this is the case, we will write-protect all the
     * previous objects' pages too.     */

#if 0
    /* I think this'd work just as well, but without the assertions.
     * -dan 2004.01.01 */
    page_index_t first_page = find_page_index(page_scan_start(addr_page_index))
#else
    page_index_t first_page = addr_page_index;
    while (!page_starts_contiguous_block_p(first_page)) {
        --first_page;
        /* Do some checks. */
        gc_assert(page_bytes_used(first_page) == GENCGC_CARD_BYTES);
        gc_assert(page_table[first_page].gen == from_space);
        gc_assert(page_table[first_page].allocated == region_allocation);
    }
#endif

    /* Adjust any large objects before promotion as they won't be
     * copied after promotion. */
    if (page_table[first_page].large_object) {
        maybe_adjust_large_object(first_page);
        /* It may have moved to unboxed pages. */
        region_allocation = page_table[first_page].allocated;
    }

    /* Now work forward until the end of this contiguous area is found,
     * marking all pages as dont_move. */
    page_index_t i;
    for (i = first_page; ;i++) {
        gc_assert(page_table[i].allocated == region_allocation);

        /* Mark the page static. */
        page_table[i].dont_move = 1;

        /* It is essential that the pages are not write protected as
         * they may have pointers into the old-space which need
         * scavenging. They shouldn't be write protected at this
         * stage. */
        gc_assert(!page_table[i].write_protected);

        /* Check whether this is the last page in this contiguous block.. */
        if (page_ends_contiguous_block_p(i, from_space))
            break;
    }

#ifdef PIN_GRANULARITY_LISPOBJ
    /* Do not do this for multi-page objects.  Those pages do not need
     * object wipeout anyway. */
    if (i == first_page) { // single-page object
        pin_object(object_start);
        page_table[i].has_pins = 1;
    }
#endif

    /* Check that the page is now static. */
    gc_assert(page_table[addr_page_index].dont_move != 0);
}


#define IN_REGION_P(a,kind) (kind##_region.start_addr<=a && a<=kind##_region.free_pointer)
#ifdef LISP_FEATURE_SEGREGATED_CODE
#define IN_BOXED_REGION_P(a) IN_REGION_P(a,boxed)||IN_REGION_P(a,code)
#else
#define IN_BOXED_REGION_P(a) IN_REGION_P(a,boxed)
#endif

/* If the given page is not write-protected, then scan it for pointers
 * to younger generations or the top temp. generation, if no
 * suspicious pointers are found then the page is write-protected.
 *
 * Care is taken to check for pointers to the current gc_alloc()
 * region if it is a younger generation or the temp. generation. This
 * frees the caller from doing a gc_alloc_update_page_tables(). Actually
 * the gc_alloc_generation does not need to be checked as this is only
 * called from scavenge_generation() when the gc_alloc generation is
 * younger, so it just checks if there is a pointer to the current
 * region.
 *
 * We return 1 if the page was write-protected, else 0. */
static int
update_page_write_prot(page_index_t page)
{
    generation_index_t gen = page_table[page].gen;
    sword_t j;
    int wp_it = 1;
    void **page_addr = (void **)page_address(page);
    sword_t num_words = page_bytes_used(page) / N_WORD_BYTES;

    /* Shouldn't be a free page. */
    gc_assert(!page_free_p(page));
    gc_assert(page_bytes_used(page) != 0);

    if (!ENABLE_PAGE_PROTECTION) return 0;

    /* Skip if it's already write-protected, pinned, or unboxed */
    if (page_table[page].write_protected
        /* FIXME: What's the reason for not write-protecting pinned pages? */
        || page_table[page].dont_move
        || page_unboxed_p(page))
        return (0);

    /* Scan the page for pointers to younger generations or the
     * top temp. generation. */

    /* This is conservative: any word satisfying is_lisp_pointer() is
     * assumed to be a pointer. To do otherwise would require a family
     * of scavenge-like functions. */
    for (j = 0; j < num_words; j++) {
        void *ptr = *(page_addr+j);
        page_index_t index;
        lispobj __attribute__((unused)) header;

        if (!is_lisp_pointer((lispobj)ptr))
            continue;
        /* Check that it's in the dynamic space */
        if ((index = find_page_index(ptr)) != -1) {
            if (/* Does it point to a younger or the temp. generation? */
                (!page_free_p(index)
                 && (page_bytes_used(index) != 0)
                 && ((page_table[index].gen < gen)
                     || (page_table[index].gen == SCRATCH_GENERATION)))

                /* Or does it point within a current gc_alloc() region? */
                || (IN_BOXED_REGION_P(ptr) || IN_REGION_P(ptr,unboxed))) {
                wp_it = 0;
                break;
            }
        }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        else if ((index = find_immobile_page_index(ptr)) >= 0 &&
                 other_immediate_lowtag_p(header = *native_pointer((lispobj)ptr))) {
            // This is *possibly* a pointer to an object in immobile space,
            // given that above two conditions were satisfied.
            // But unlike in the dynamic space case, we need to read a byte
            // from the object to determine its generation, which requires care.
            // Consider an unboxed word that looks like a pointer to a word that
            // looks like fun-header-widetag. We can't naively back up to the
            // underlying code object since the alleged header might not be one.
            int obj_gen = gen; // Make comparison fail if we fall through
            if (lowtag_of((lispobj)ptr) != FUN_POINTER_LOWTAG) {
                obj_gen = __immobile_obj_generation(native_pointer((lispobj)ptr));
            } else if (widetag_of(header) == SIMPLE_FUN_WIDETAG) {
                lispobj* code = fun_code_header((lispobj)ptr - FUN_POINTER_LOWTAG);
                // This is a heuristic, since we're not actually looking for
                // an object boundary. Precise scanning of 'page' would obviate
                // the guard conditions here.
                if ((lispobj)code >= IMMOBILE_VARYOBJ_SUBSPACE_START
                    && widetag_of(*code) == CODE_HEADER_WIDETAG)
                    obj_gen = __immobile_obj_generation(code);
            }
            // A bogus generation number implies a not-really-pointer,
            // but it won't cause misbehavior.
            if (obj_gen < gen || obj_gen == SCRATCH_GENERATION) {
                wp_it = 0;
                break;
           }
        }
#endif
    }

    if (wp_it == 1) {
        /* Write-protect the page. */
        /*FSHOW((stderr, "/write-protecting page %d gen %d\n", page, gen));*/

        os_protect((void *)page_addr,
                   GENCGC_CARD_BYTES,
                   OS_VM_PROT_READ|OS_VM_PROT_EXECUTE);

        /* Note the page as protected in the page tables. */
        page_table[page].write_protected = 1;
    }

    return (wp_it);
}

/* Is this page holding a normal (non-hashtable) large-object
 * simple-vector? */
static inline boolean large_simple_vector_p(page_index_t page) {
    if (!page_table[page].large_object)
        return 0;
    lispobj object = *(lispobj *)page_address(page);
    return widetag_of(object) == SIMPLE_VECTOR_WIDETAG &&
        (HeaderValue(object) & 0xFF) == subtype_VectorNormal;

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
 *
 * Enabling SC_GEN_CK scavenges the write-protected pages and checks
 * that none were written, which they shouldn't be as they should have
 * no pointers to younger generations. This breaks down for weak
 * pointers as the objects contain a link to the next and are written
 * if a weak pointer is scavenged. Still it's a useful check. */
static void
scavenge_generations(generation_index_t from, generation_index_t to)
{
    page_index_t i;
    page_index_t num_wp = 0;

#define SC_GEN_CK 0
#if SC_GEN_CK
    /* Clear the write_protected_cleared flags on all pages. */
    for (i = 0; i < page_table_pages; i++)
        page_table[i].write_protected_cleared = 0;
#endif

    for (i = 0; i < last_free_page; i++) {
        generation_index_t generation = page_table[i].gen;
        if (page_boxed_p(i)
            && (page_bytes_used(i) != 0)
            && (generation != new_space)
            && (generation >= from)
            && (generation <= to)) {
            page_index_t last_page,j;
            int write_protected=1;

            /* This should be the start of a region */
            gc_assert(page_starts_contiguous_block_p(i));

            if (large_simple_vector_p(i)) {
                /* Scavenge only the unprotected pages of a
                 * large-object vector, other large objects could be
                 * handled as well, but vectors are easier to deal
                 * with and are more likely to grow to very large
                 * sizes where avoiding scavenging the whole thing is
                 * worthwile */
                if (!page_table[i].write_protected) {
                    scavenge((lispobj*)page_address(i) + 2,
                             GENCGC_CARD_BYTES / N_WORD_BYTES - 2);
                    update_page_write_prot(i);
                }
                for (last_page = i + 1; ; last_page++) {
                    lispobj* start = (lispobj*)page_address(last_page);
                    write_protected = page_table[last_page].write_protected;
                    if (page_ends_contiguous_block_p(last_page, generation)) {
                        if (!write_protected) {
                            scavenge(start, page_bytes_used(last_page) / N_WORD_BYTES);
                            update_page_write_prot(last_page);
                        }
                        break;
                    }
                    if (!write_protected) {
                        scavenge(start, GENCGC_CARD_BYTES / N_WORD_BYTES);
                        update_page_write_prot(last_page);
                    }
                }
            } else {
                /* Now work forward until the end of the region */
                for (last_page = i; ; last_page++) {
                    write_protected =
                        write_protected && page_table[last_page].write_protected;
                    if (page_ends_contiguous_block_p(last_page, generation))
                        break;
                }
                if (!write_protected) {
                    heap_scavenge((lispobj*)page_address(i),
                                  (lispobj*)(page_address(last_page)
                                             + page_bytes_used(last_page)));

                    /* Now scan the pages and write protect those that
                     * don't have pointers to younger generations. */
                    if (ENABLE_PAGE_PROTECTION) {
                        for (j = i; j <= last_page; j++) {
                            num_wp += update_page_write_prot(j);
                        }
                    }
                    if ((gencgc_verbose > 1) && (num_wp != 0)) {
                        FSHOW((stderr,
                               "/write protected %d pages within generation %d\n",
                               num_wp, generation));
                    }
                }
            }
            i = last_page;
        }
    }

#if SC_GEN_CK
    /* Check that none of the write_protected pages in this generation
     * have been written to. */
    for (i = 0; i < page_table_pages; i++) {
        if (!page_free_p(i)
            && (page_bytes_used(i) != 0)
            && (page_table[i].gen == generation)
            && (page_table[i].write_protected_cleared != 0)) {
            FSHOW((stderr, "/scavenge_generation() %d\n", generation));
            FSHOW((stderr,
                   "/page bytes_used=%d scan_start_offset=%lu dont_move=%d\n",
                    page_bytes_used(i),
                    scan_start_offset(page_table[i]),
                    page_table[i].dont_move));
            lose("write to protected page %d in scavenge_generation()\n", i);
        }
    }
#endif
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
 * dont_move in which case they may have been promoted and still have
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

#ifdef LISP_FEATURE_IMMOBILE_SPACE
extern unsigned int immobile_scav_queue_count;
extern void
  update_immobile_nursery_bits(),
  scavenge_immobile_roots(generation_index_t,generation_index_t),
  scavenge_immobile_newspace(),
  sweep_immobile_space(int raise),
  write_protect_immobile_space();
#else
#define immobile_scav_queue_count 0
#endif

/* Do one full scan of the new space generation. This is not enough to
 * complete the job as new objects may be added to the generation in
 * the process which are not scavenged. */
static void
scavenge_newspace_generation_one_scan(generation_index_t generation)
{
    page_index_t i;

    FSHOW((stderr,
           "/starting one full scan of newspace generation %d\n",
           generation));
    for (i = 0; i < last_free_page; i++) {
        /* Note that this skips over open regions when it encounters them. */
        if (page_boxed_p(i)
            && (page_bytes_used(i) != 0)
            && (page_table[i].gen == generation)
            && (!page_table[i].write_protected
                /* (This may be redundant as write_protected is now
                 * cleared before promotion.) */
                || page_table[i].dont_move)) {
            page_index_t last_page;
            int all_wp=1;

            /* The scavenge will start at the scan_start_offset of
             * page i.
             *
             * We need to find the full extent of this contiguous
             * block in case objects span pages.
             *
             * Now work forward until the end of this contiguous area
             * is found. A small area is preferred as there is a
             * better chance of its pages being write-protected. */
            for (last_page = i; ;last_page++) {
                /* If all pages are write-protected and movable,
                 * then no need to scavenge */
                all_wp=all_wp && page_table[last_page].write_protected &&
                    !page_table[last_page].dont_move;

                /* Check whether this is the last page in this
                 * contiguous block */
                if (page_ends_contiguous_block_p(last_page, generation))
                    break;
            }

            /* Do a limited check for write-protected pages.  */
            if (!all_wp) {
                new_areas_ignore_page = last_page;
                heap_scavenge(page_scan_start(i),
                              (lispobj*)(page_address(last_page)
                                         + page_bytes_used(last_page)));
            }
            i = last_page;
        }
    }
    FSHOW((stderr,
           "/done with one full scan of newspace generation %d\n",
           generation));
}

/* Do a complete scavenge of the newspace generation. */
static void
scavenge_newspace_generation(generation_index_t generation)
{
    size_t i;

    /* the new_areas array currently being written to by gc_alloc() */
    struct new_area (*current_new_areas)[] = &new_areas_1;
    size_t current_new_areas_index;

    /* the new_areas created by the previous scavenge cycle */
    struct new_area (*previous_new_areas)[] = NULL;
    size_t previous_new_areas_index;

    /* Flush the current regions updating the tables. */
    gc_alloc_update_all_page_tables(0);

    /* Turn on the recording of new areas by gc_alloc(). */
    new_areas = current_new_areas;
    new_areas_index = 0;

    /* Don't need to record new areas that get scavenged anyway during
     * scavenge_newspace_generation_one_scan. */
    record_new_objects = 1;

    /* Start with a full scavenge. */
    scavenge_newspace_generation_one_scan(generation);

    /* Record all new areas now. */
    record_new_objects = 2;

    /* Give a chance to weak hash tables to make other objects live.
     * FIXME: The algorithm implemented here for weak hash table gcing
     * is O(W^2+N) as Bruno Haible warns in
     * http://www.haible.de/bruno/papers/cs/weak/WeakDatastructures-writeup.html
     * see "Implementation 2". */
    scav_weak_hash_tables();

    /* Flush the current regions updating the tables. */
    gc_alloc_update_all_page_tables(0);

    /* Grab new_areas_index. */
    current_new_areas_index = new_areas_index;

    /*FSHOW((stderr,
             "The first scan is finished; current_new_areas_index=%d.\n",
             current_new_areas_index));*/

    while (current_new_areas_index > 0 || immobile_scav_queue_count) {
        /* Move the current to the previous new areas */
        previous_new_areas = current_new_areas;
        previous_new_areas_index = current_new_areas_index;

        /* Scavenge all the areas in previous new areas. Any new areas
         * allocated are saved in current_new_areas. */

        /* Allocate an array for current_new_areas; alternating between
         * new_areas_1 and 2 */
        if (previous_new_areas == &new_areas_1)
            current_new_areas = &new_areas_2;
        else
            current_new_areas = &new_areas_1;

        /* Set up for gc_alloc(). */
        new_areas = current_new_areas;
        new_areas_index = 0;

#ifdef LISP_FEATURE_IMMOBILE_SPACE
        scavenge_immobile_newspace();
#endif
        /* Check whether previous_new_areas had overflowed. */
        if (previous_new_areas_index >= NUM_NEW_AREAS) {

            /* New areas of objects allocated have been lost so need to do a
             * full scan to be sure! If this becomes a problem try
             * increasing NUM_NEW_AREAS. */
            if (gencgc_verbose) {
                SHOW("new_areas overflow, doing full scavenge");
            }

            /* Don't need to record new areas that get scavenged
             * anyway during scavenge_newspace_generation_one_scan. */
            record_new_objects = 1;

            scavenge_newspace_generation_one_scan(generation);

            /* Record all new areas now. */
            record_new_objects = 2;

            scav_weak_hash_tables();

            /* Flush the current regions updating the tables. */
            gc_alloc_update_all_page_tables(0);

        } else {

            /* Work through previous_new_areas. */
            for (i = 0; i < previous_new_areas_index; i++) {
                page_index_t page = (*previous_new_areas)[i].page;
                size_t offset = (*previous_new_areas)[i].offset;
                size_t size = (*previous_new_areas)[i].size;
                gc_assert(size % N_WORD_BYTES == 0);
                lispobj *start = (lispobj*)(page_address(page) + offset);
                heap_scavenge(start, (lispobj*)((char*)start + size));
            }

            scav_weak_hash_tables();

            /* Flush the current regions updating the tables. */
            gc_alloc_update_all_page_tables(0);
        }

        current_new_areas_index = new_areas_index;

        /*FSHOW((stderr,
                 "The re-scan has finished; current_new_areas_index=%d.\n",
                 current_new_areas_index));*/
    }

    /* Turn off recording of areas allocated by gc_alloc(). */
    record_new_objects = 0;

#if SC_NS_GEN_CK
    {
        page_index_t i;
        /* Check that none of the write_protected pages in this generation
         * have been written to. */
        for (i = 0; i < page_table_pages; i++) {
            if (!page_free_p(i)
                && (page_bytes_used(i) != 0)
                && (page_table[i].gen == generation)
                && (page_table[i].write_protected_cleared != 0)
                && (page_table[i].dont_move == 0)) {
                lose("write protected page %d written to in scavenge_newspace_generation\ngeneration=%d dont_move=%d\n",
                     i, generation, page_table[i].dont_move);
            }
        }
    }
#endif
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
    char *page_addr = 0;
    uword_t region_bytes = 0;

    for (i = 0; i < last_free_page; i++) {
        if (!page_free_p(i)
            && (page_bytes_used(i) != 0)
            && (page_table[i].gen == from_space)) {

            /* Remove any write-protection. We should be able to rely
             * on the write-protect flag to avoid redundant calls. */
            if (page_table[i].write_protected) {
                page_table[i].write_protected = 0;
                page_addr = page_address(i);
                if (!region_addr) {
                    /* First region. */
                    region_addr = page_addr;
                    region_bytes = GENCGC_CARD_BYTES;
                } else if (region_addr + region_bytes == page_addr) {
                    /* Region continue. */
                    region_bytes += GENCGC_CARD_BYTES;
                } else {
                    /* Unprotect previous region. */
                    os_protect(region_addr, region_bytes, OS_VM_PROT_ALL);
                    /* First page in new region. */
                    region_addr = page_addr;
                    region_bytes = GENCGC_CARD_BYTES;
                }
            }
        }
    }
    if (region_addr) {
        /* Unprotect last region. */
        os_protect(region_addr, region_bytes, OS_VM_PROT_ALL);
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
        while ((first_page < last_free_page)
               && (page_free_p(first_page)
                   || (page_bytes_used(first_page) == 0)
                   || (page_table[first_page].gen != from_space)))
            first_page++;

        if (first_page >= last_free_page)
            break;

        /* Find the last page of this region. */
        last_page = first_page;

        do {
            /* Free the page. */
            bytes_freed += page_bytes_used(last_page);
            generations[page_table[last_page].gen].bytes_allocated -=
                page_bytes_used(last_page);
            page_table[last_page].allocated = FREE_PAGE_FLAG;
            set_page_bytes_used(last_page, 0);
            /* Should already be unprotected by unprotect_oldspace(). */
            gc_assert(!page_table[last_page].write_protected);
            last_page++;
        }
        while ((last_page < last_free_page)
               && !page_free_p(last_page)
               && (page_bytes_used(last_page) != 0)
               && (page_table[last_page].gen == from_space));

#ifdef READ_PROTECT_FREE_PAGES
        os_protect(page_address(first_page),
                   npage_bytes(last_page-first_page),
                   OS_VM_PROT_NONE);
#endif
        first_page = last_page;
    } while (first_page < last_free_page);

    bytes_allocated -= bytes_freed;
    return bytes_freed;
}

#if 0
/* Print some information about a pointer at the given address. */
static void
print_ptr(lispobj *addr)
{
    /* If addr is in the dynamic space then out the page information. */
    page_index_t pi1 = find_page_index((void*)addr);

    if (pi1 != -1)
        fprintf(stderr,"  %p: page %d  alloc %d  gen %d  bytes_used %d  offset %lu  dont_move %d\n",
                addr,
                pi1,
                page_table[pi1].allocated,
                page_table[pi1].gen,
                page_bytes_used(pi1),
                scan_start_offset(page_table[pi1]),
                page_table[pi1].dont_move);
    fprintf(stderr,"  %x %x %x %x (%x) %x %x %x %x\n",
            *(addr-4),
            *(addr-3),
            *(addr-2),
            *(addr-1),
            *(addr-0),
            *(addr+1),
            *(addr+2),
            *(addr+3),
            *(addr+4));
}
#endif

static int
is_in_stack_space(lispobj ptr)
{
    /* For space verification: Pointers can be valid if they point
     * to a thread stack space.  This would be faster if the thread
     * structures had page-table entries as if they were part of
     * the heap space. */
    struct thread *th;
    for_each_thread(th) {
        if ((th->control_stack_start <= (lispobj *)ptr) &&
            (th->control_stack_end >= (lispobj *)ptr)) {
            return 1;
        }
    }
    return 0;
}

// NOTE: This function can produces false failure indications,
// usually related to dynamic space pointing to the stack of a
// dead thread, but there may be other reasons as well.
static void
verify_range(lispobj *start, size_t words)
{
    extern int valid_lisp_pointer_p(lispobj);
    int is_in_readonly_space =
        (READ_ONLY_SPACE_START <= (uword_t)start &&
         start < read_only_space_free_pointer);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    int is_in_immobile_space =
        (IMMOBILE_SPACE_START <= (uword_t)start &&
         start < immobile_space_free_pointer);
#endif

    lispobj *end = start + words;
    size_t count;
    for ( ; start < end ; start += count) {
        count = 1;
        lispobj thing = *start;
        lispobj __attribute__((unused)) pointee;

        if (is_lisp_pointer(thing)) {
            page_index_t page_index = find_page_index((void*)thing);
            sword_t to_readonly_space =
                (READ_ONLY_SPACE_START <= thing &&
                 thing < (lispobj)read_only_space_free_pointer);
            sword_t to_static_space =
                (STATIC_SPACE_START <= thing &&
                 thing < (lispobj)static_space_free_pointer);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
            sword_t to_immobile_space =
                (IMMOBILE_SPACE_START <= thing &&
                 thing < (lispobj)immobile_fixedobj_free_pointer) ||
                (IMMOBILE_VARYOBJ_SUBSPACE_START <= thing &&
                 thing < (lispobj)immobile_space_free_pointer);
#endif

            /* Does it point to the dynamic space? */
            if (page_index != -1) {
                /* If it's within the dynamic space it should point to a used page. */
                if (page_free_p(page_index))
                    lose ("Ptr %p @ %p sees free page.\n", thing, start);
                if ((thing & (GENCGC_CARD_BYTES-1)) >= page_bytes_used(page_index))
                    lose ("Ptr %p @ %p sees unallocated space.\n", thing, start);
                /* Check that it doesn't point to a forwarding pointer! */
                if (*native_pointer(thing) == 0x01) {
                    lose("Ptr %p @ %p sees forwarding ptr.\n", thing, start);
                }
                /* Check that its not in the RO space as it would then be a
                 * pointer from the RO to the dynamic space. */
                if (is_in_readonly_space) {
                    lose("ptr to dynamic space %p from RO space %x\n",
                         thing, start);
                }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
                // verify all immobile space -> dynamic space pointers
                if (is_in_immobile_space && !valid_lisp_pointer_p(thing)) {
                    lose("Ptr %p @ %p sees junk.\n", thing, start);
                }
#endif
                /* Does it point to a plausible object? This check slows
                 * it down a lot (so it's commented out).
                 *
                 * "a lot" is serious: it ate 50 minutes cpu time on
                 * my duron 950 before I came back from lunch and
                 * killed it.
                 *
                 *   FIXME: Add a variable to enable this
                 * dynamically. */
                /*
                if (!valid_lisp_pointer_p((lispobj *)thing) {
                    lose("ptr %p to invalid object %p\n", thing, start);
                }
                */
#ifdef LISP_FEATURE_IMMOBILE_SPACE
            } else if (to_immobile_space) {
                // the object pointed to must not have been discarded as garbage
                if (!other_immediate_lowtag_p(*native_pointer(thing))
                    || immobile_filler_p(native_pointer(thing)))
                    lose("Ptr %p @ %p sees trashed object.\n", (void*)thing, start);
                // verify all pointers to immobile space
                if (!valid_lisp_pointer_p(thing))
                    lose("Ptr %p @ %p sees junk.\n", thing, start);
#endif
            } else {
                extern char __attribute__((unused)) funcallable_instance_tramp;
                /* Verify that it points to another valid space. */
                if (!to_readonly_space && !to_static_space
                    && !is_in_stack_space(thing)) {
                    lose("Ptr %p @ %p sees junk.\n", thing, start);
                }
            }
            continue;
        }
        int widetag = widetag_of(thing);
        if (is_lisp_immediate(thing) || widetag == NO_TLS_VALUE_MARKER_WIDETAG) {
            /* skip immediates */
        } else if (!(other_immediate_lowtag_p(widetag)
                     && lowtag_for_widetag[widetag>>2])) {
            lose("Unhandled widetag %p at %p\n", widetag, start);
        } else if (unboxed_obj_widetag_p(widetag)) {
            count = sizetab[widetag](start);
        } else switch(widetag) {
                    /* boxed or partially boxed objects */
            // FIXME: x86-64 can have partially unboxed FINs. The raw words
            // are at the moment valid fixnums by blind luck.
            case INSTANCE_WIDETAG:
                if (instance_layout(start)) {
                    sword_t nslots = instance_length(thing) | 1;
                    instance_scan(verify_range, start+1, nslots,
                                  LAYOUT(instance_layout(start))->bitmap);
                    count = 1 + nslots;
                }
                break;
            case CODE_HEADER_WIDETAG:
                {
                struct code *code = (struct code *) start;
                sword_t nheader_words = code_header_words(code->header);
                /* Scavenge the boxed section of the code data block */
                verify_range(start + 1, nheader_words - 1);

                /* Scavenge the boxed section of each function
                 * object in the code data block. */
                for_each_simple_fun(i, fheaderp, code, 1, {
                    verify_range(SIMPLE_FUN_SCAV_START(fheaderp),
                                 SIMPLE_FUN_SCAV_NWORDS(fheaderp)); });
                count = nheader_words + code_instruction_words(code->code_size);
                break;
                }
#ifdef LISP_FEATURE_IMMOBILE_CODE
            case FDEFN_WIDETAG:
                verify_range(start + 1, 2);
                pointee = fdefn_raw_referent((struct fdefn*)start);
                verify_range(&pointee, 1);
                count = CEILING(sizeof (struct fdefn)/sizeof(lispobj), 2);
                break;
#endif
        }
    }
}
static uword_t verify_space(lispobj start, lispobj* end) {
    verify_range((lispobj*)start, end-(lispobj*)start);
    return 0;
}

static void verify_dynamic_space();

static void
verify_gc(void)
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
#  ifdef __linux__
    // Try this verification if marknsweep was compiled with extra debugging.
    // But weak symbols don't work on macOS.
    extern void __attribute__((weak)) check_varyobj_pages();
    if (&check_varyobj_pages) check_varyobj_pages();
#  endif
    verify_space(IMMOBILE_SPACE_START, immobile_fixedobj_free_pointer);
    verify_space(IMMOBILE_VARYOBJ_SUBSPACE_START, immobile_space_free_pointer);
#endif
    struct thread *th;
    for_each_thread(th) {
        verify_space((lispobj)th->binding_stack_start,
                     (lispobj*)get_binding_stack_pointer(th));
    }
    verify_space(READ_ONLY_SPACE_START, read_only_space_free_pointer);
    verify_space(STATIC_SPACE_START, static_space_free_pointer);
    verify_dynamic_space();
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

    for (i = 0; i < last_free_page; i++) {
        if (!page_free_p(i)
            && (page_bytes_used(i) != 0)
            && ((1 << page_table[i].gen) & genmask)) {
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
                     (lispobj*)(page_bytes_used(last_page) + page_address(last_page)),
                     extra);
            if (result) return result;

            i = last_page;
        }
    }
    return 0;
}
static void verify_generation(generation_index_t generation)
{
    walk_generation((uword_t(*)(lispobj*,lispobj*,uword_t))verify_space,
                    generation, 0);
}

/* Check that all the free space is zero filled. */
static void
verify_zero_fill(void)
{
    page_index_t page;

    for (page = 0; page < last_free_page; page++) {
        if (page_free_p(page)) {
            /* The whole page should be zero filled. */
            sword_t *start_addr = (sword_t *)page_address(page);
            sword_t i;
            for (i = 0; i < (sword_t)GENCGC_CARD_BYTES/N_WORD_BYTES; i++) {
                if (start_addr[i] != 0) {
                    lose("free page not zero at %p\n", start_addr + i);
                }
            }
        } else {
            sword_t free_bytes = GENCGC_CARD_BYTES - page_bytes_used(page);
            if (free_bytes > 0) {
                sword_t *start_addr =
                    (sword_t *)(page_address(page) + page_bytes_used(page));
                sword_t size = free_bytes / N_WORD_BYTES;
                sword_t i;
                for (i = 0; i < size; i++) {
                    if (start_addr[i] != 0) {
                        lose("free region not zero at %p\n", start_addr + i);
                    }
                }
            }
        }
    }
}

/* External entry point for verify_zero_fill */
void
gencgc_verify_zero_fill(void)
{
    /* Flush the alloc regions updating the tables. */
    gc_alloc_update_all_page_tables(1);
    SHOW("verifying zero fill");
    verify_zero_fill();
}

static void
verify_dynamic_space(void)
{
    verify_generation(-1);
    if (gencgc_enable_verify_zero_fill)
        verify_zero_fill();
}

/* Write-protect all the dynamic boxed pages in the given generation. */
static void
write_protect_generation_pages(generation_index_t generation)
{
    page_index_t start;

    gc_assert(generation < SCRATCH_GENERATION);

    for (start = 0; start < last_free_page; start++) {
        if (protect_page_p(start, generation)) {
            void *page_start;
            page_index_t last;

            /* Note the page as protected in the page tables. */
            page_table[start].write_protected = 1;

            for (last = start + 1; last < last_free_page; last++) {
                if (!protect_page_p(last, generation))
                  break;
                page_table[last].write_protected = 1;
            }

            page_start = page_address(start);

            os_protect(page_start,
                       npage_bytes(last - start),
                       OS_VM_PROT_READ | OS_VM_PROT_EXECUTE);

            start = last;
        }
    }

    if (gencgc_verbose > 1) {
        FSHOW((stderr,
               "/write protected %d of %d pages in generation %d\n",
               count_write_protect_generation_pages(generation),
               count_generation_pages(generation),
               generation));
    }
}

#ifndef GENCGC_IS_PRECISE
static void
preserve_context_registers (void (*proc)(os_context_register_t), os_context_t *c)
{
#ifdef LISP_FEATURE_SB_THREAD
    void **ptr;
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

    for (i = 0; i < last_free_page; i++) {
        if (page_table[i].dont_move &&
            /* dont_move is cleared lazily, so test the 'gen' field as well. */
            page_table[i].gen == from_space) {
            if (page_table[i].has_pins) {
                // do not move to newspace after all, this will be word-wiped
                continue;
            }
            page_table[i].gen = new_space;
            /* And since we're moving the pages wholesale, also adjust
             * the generation allocation counters. */
            int used = page_bytes_used(i);
            generations[new_space].bytes_allocated += used;
            generations[from_space].bytes_allocated -= used;
        }
    }
}

#if defined(__GNUC__) && defined(ADDRESS_SANITIZER)
#define NO_SANITIZE_ADDRESS __attribute__((no_sanitize_address))
#else
#define NO_SANITIZE_ADDRESS
#endif

/* Garbage collect a generation. If raise is 0 then the remains of the
 * generation are not raised to the next generation. */
static void NO_SANITIZE_ADDRESS
garbage_collect_generation(generation_index_t generation, int raise)
{
    page_index_t i;
    struct thread *th;

    gc_assert(generation <= HIGHEST_NORMAL_GENERATION);

    /* The oldest generation can't be raised. */
    gc_assert((generation != HIGHEST_NORMAL_GENERATION) || (raise == 0));

    /* Check if weak hash tables were processed in the previous GC. */
    gc_assert(weak_hash_tables == NULL);

    /* Initialize the weak pointer list. */
    weak_pointers = NULL;

    /* When a generation is not being raised it is transported to a
     * temporary generation (NUM_GENERATIONS), and lowered when
     * done. Set up this new generation. There should be no pages
     * allocated to it yet. */
    if (!raise) {
         gc_assert(generations[SCRATCH_GENERATION].bytes_allocated == 0);
    }

    /* Set the global src and dest. generations */
    from_space = generation;
    if (raise)
        new_space = generation+1;
    else
        new_space = SCRATCH_GENERATION;

    /* Change to a new space for allocation, resetting the alloc_start_page */
    gc_alloc_generation = new_space;
#ifdef LISP_FEATURE_SEGREGATED_CODE
    bzero(generations[new_space].alloc_start_page_,
          sizeof generations[new_space].alloc_start_page_);
#else
    generations[new_space].alloc_start_page = 0;
    generations[new_space].alloc_unboxed_start_page = 0;
    generations[new_space].alloc_large_start_page = 0;
#endif

#ifdef PIN_GRANULARITY_LISPOBJ
    hopscotch_reset(&pinned_objects);
#endif
    /* Before any pointers are preserved, the dont_move flags on the
     * pages need to be cleared. */
    /* FIXME: consider moving this bitmap into its own range of words,
     * out of the page table. Then we can just bzero() it.
     * This will also obviate the extra test at the comment
     * "dont_move is cleared lazily" in move_pinned_pages_to_newspace().
     */
    for (i = 0; i < last_free_page; i++)
        if(page_table[i].gen==from_space) {
            page_table[i].dont_move = 0;
        }

    /* Un-write-protect the old-space pages. This is essential for the
     * promoted pages as they may contain pointers into the old-space
     * which need to be scavenged. It also helps avoid unnecessary page
     * faults as forwarding pointers are written into them. They need to
     * be un-protected anyway before unmapping later. */
    if (ENABLE_PAGE_PROTECTION)
        unprotect_oldspace();

    /* Scavenge the stacks' conservative roots. */

    /* there are potentially two stacks for each thread: the main
     * stack, which may contain Lisp pointers, and the alternate stack.
     * We don't ever run Lisp code on the altstack, but it may
     * host a sigcontext with lisp objects in it */

    /* what we need to do: (1) find the stack pointer for the main
     * stack; scavenge it (2) find the interrupt context on the
     * alternate stack that might contain lisp values, and scavenge
     * that */

    /* we assume that none of the preceding applies to the thread that
     * initiates GC.  If you ever call GC from inside an altstack
     * handler, you will lose. */

#ifndef GENCGC_IS_PRECISE
    /* And if we're saving a core, there's no point in being conservative. */
    if (conservative_stack) {
        for_each_thread(th) {
            void **ptr;
            void **esp=(void **)-1;
            if (th->state == STATE_DEAD)
                continue;
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

            /* In addition to pointers on the stack, also preserve the
             * return PC, the only value from the context that we need
             * in addition to the SP.  The return PC gets saved by the
             * foreign call wrapper, and removed from the control stack
             * into a register. */
            preserve_pointer(th->pc_around_foreign_call);

            /* And on platforms with interrupts: scavenge ctx registers. */

            /* Disabled on Windows, because it does not have an explicit
             * stack of `interrupt_contexts'.  The reported CSP has been
             * chosen so that the current context on the stack is
             * covered by the stack scan.  See also set_csp_from_context(). */
#  ifndef LISP_FEATURE_WIN32
            if (th != arch_os_get_current_thread()) {
                long k = fixnum_value(
                    SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX,th));
                while (k > 0)
                    preserve_context_registers((void(*)(os_context_register_t))preserve_pointer,
                                               th->interrupt_contexts[--k]);
            }
#  endif
# elif defined(LISP_FEATURE_SB_THREAD)
            sword_t i,free;
            if(th==arch_os_get_current_thread()) {
                /* Somebody is going to burn in hell for this, but casting
                 * it in two steps shuts gcc up about strict aliasing. */
                esp = (void **)((void *)&raise);
            } else {
                void **esp1;
                free=fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX,th));
                for(i=free-1;i>=0;i--) {
                    os_context_t *c=th->interrupt_contexts[i];
                    esp1 = (void **) *os_context_register_addr(c,reg_SP);
                    if (esp1>=(void **)th->control_stack_start &&
                        esp1<(void **)th->control_stack_end) {
                        if(esp1<esp) esp=esp1;
                        preserve_context_registers((void(*)(os_context_register_t))preserve_pointer,
                                                   c);
                    }
                }
            }
# else
            esp = (void **)((void *)&raise);
# endif
            if (!esp || esp == (void*) -1)
                lose("garbage_collect: no SP known for thread %x (OS %x)",
                     th, th->os_thread);
            for (ptr = ((void **)th->control_stack_end)-1; ptr >= esp;  ptr--) {
                preserve_pointer(*ptr);
            }
        }
    }
#else
    /* Non-x86oid systems don't have "conservative roots" as such, but
     * the same mechanism is used for objects pinned for use by alien
     * code. */
    for_each_thread(th) {
        lispobj pin_list = SymbolTlValue(PINNED_OBJECTS,th);
        while (pin_list != NIL) {
            preserve_pointer((void*)(CONS(pin_list)->car));
            pin_list = CONS(pin_list)->cdr;
        }
    }
#endif

#if QSHOW
    if (gencgc_verbose > 1) {
        sword_t num_dont_move_pages = count_dont_move_pages();
        fprintf(stderr,
                "/non-movable pages due to conservative pointers = %ld (%lu bytes)\n",
                num_dont_move_pages,
                npage_bytes(num_dont_move_pages));
    }
#endif

    /* Now that all of the pinned (dont_move) pages are known, and
     * before we start to scavenge (and thus relocate) objects,
     * relocate the pinned pages to newspace, so that the scavenger
     * will not attempt to relocate their contents. */
    move_pinned_pages_to_newspace();

    /* Scavenge all the rest of the roots. */

#ifdef GENCGC_IS_PRECISE
    /*
     * If not x86, we need to scavenge the interrupt context(s) and the
     * control stack.
     */
    {
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

    /* Scavenge the Lisp functions of the interrupt handlers, taking
     * care to avoid SIG_DFL and SIG_IGN. */
    for (i = 0; i < NSIG; i++) {
        union interrupt_handler handler = interrupt_handlers[i];
        if (!ARE_SAME_HANDLER(handler.c, SIG_IGN) &&
            !ARE_SAME_HANDLER(handler.c, SIG_DFL)) {
            scavenge((lispobj *)(interrupt_handlers + i), 1);
        }
    }
    /* Scavenge the binding stacks. */
    {
        struct thread *th;
        for_each_thread(th) {
            scav_binding_stack((lispobj*)th->binding_stack_start,
                               (lispobj*)get_binding_stack_pointer(th));
#ifdef LISP_FEATURE_SB_THREAD
            /* do the tls as well */
            sword_t len;
            len=(SymbolValue(FREE_TLS_INDEX,0) >> WORD_SHIFT) -
                (sizeof (struct thread))/(sizeof (lispobj));
            scavenge((lispobj *) (th+1),len);
#endif
        }
    }

    /* Scavenge static space. */
    if (gencgc_verbose > 1) {
        FSHOW((stderr,
               "/scavenge static space: %d bytes\n",
               (uword_t)static_space_free_pointer - STATIC_SPACE_START));
    }
    heap_scavenge((lispobj*)STATIC_SPACE_START, static_space_free_pointer);

    /* All generations but the generation being GCed need to be
     * scavenged. The new_space generation needs special handling as
     * objects may be moved in - it is handled separately below. */
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    scavenge_immobile_roots(generation+1, SCRATCH_GENERATION);
#endif
    scavenge_generations(generation+1, PSEUDO_STATIC_GENERATION);

#ifdef LISP_FEATURE_SB_TRACEROOT
    if (gc_object_watcher) scavenge(&gc_object_watcher, 1);
#endif
    scavenge_pinned_ranges();
    /* The Lisp start function is stored in the core header, not a static
     * symbol. It is passed to gc_and_save() in this C variable */
    if (lisp_init_function) scavenge(&lisp_init_function, 1);

    /* Finally scavenge the new_space generation. Keep going until no
     * more objects are moved into the new generation */
    scavenge_newspace_generation(new_space);

    /* FIXME: I tried reenabling this check when debugging unrelated
     * GC weirdness ca. sbcl-0.6.12.45, and it failed immediately.
     * Since the current GC code seems to work well, I'm guessing that
     * this debugging code is just stale, but I haven't tried to
     * figure it out. It should be figured out and then either made to
     * work or just deleted. */

#define RESCAN_CHECK 0
#if RESCAN_CHECK
    /* As a check re-scavenge the newspace once; no new objects should
     * be found. */
    {
        os_vm_size_t old_bytes_allocated = bytes_allocated;
        os_vm_size_t bytes_allocated;

        /* Start with a full scavenge. */
        scavenge_newspace_generation_one_scan(new_space);

        /* Flush the current regions, updating the tables. */
        gc_alloc_update_all_page_tables(1);

        bytes_allocated = bytes_allocated - old_bytes_allocated;

        if (bytes_allocated != 0) {
            lose("Rescan of new_space allocated %d more bytes.\n",
                 bytes_allocated);
        }
    }
#endif

    scan_weak_hash_tables();
    scan_weak_pointers();
    wipe_nonpinned_words();
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    // Do this last, because until wipe_nonpinned_words() happens,
    // not all page table entries have the 'gen' value updated,
    // which we need to correctly find all old->young pointers.
    sweep_immobile_space(raise);
#endif

    /* Flush the current regions, updating the tables. */
    gc_alloc_update_all_page_tables(0);
#ifdef PIN_GRANULARITY_LISPOBJ
    hopscotch_log_stats(&pinned_objects, "pins");
#endif

    /* Free the pages in oldspace, but not those marked dont_move. */
    free_oldspace();

    /* If the GC is not raising the age then lower the generation back
     * to its normal generation number */
    if (!raise) {
        for (i = 0; i < last_free_page; i++)
            if ((page_bytes_used(i) != 0)
                && (page_table[i].gen == SCRATCH_GENERATION))
                page_table[i].gen = generation;
        gc_assert(generations[generation].bytes_allocated == 0);
        generations[generation].bytes_allocated =
            generations[SCRATCH_GENERATION].bytes_allocated;
        generations[SCRATCH_GENERATION].bytes_allocated = 0;
    }

    /* Reset the alloc_start_page for generation. */
#ifdef LISP_FEATURE_SEGREGATED_CODE
    bzero(generations[generation].alloc_start_page_,
          sizeof generations[generation].alloc_start_page_);
#else
    generations[generation].alloc_start_page = 0;
    generations[generation].alloc_unboxed_start_page = 0;
    generations[generation].alloc_large_start_page = 0;
#endif

    if (generation >= verify_gens) {
        if (gencgc_verbose) {
            SHOW("verifying");
        }
        verify_gc();
    }

    /* Set the new gc trigger for the GCed generation. */
    generations[generation].gc_trigger =
        generations[generation].bytes_allocated
        + generations[generation].bytes_consed_between_gc;

    if (raise)
        generations[generation].num_gc = 0;
    else
        ++generations[generation].num_gc;

}

/* Update last_free_page, then SymbolValue(ALLOCATION_POINTER). */
sword_t
update_dynamic_space_free_pointer(void)
{
    page_index_t last_page = -1, i;

    for (i = 0; i < last_free_page; i++)
        if (!page_free_p(i) && (page_bytes_used(i) != 0))
            last_page = i;

    last_free_page = last_page+1;

    set_alloc_pointer((lispobj)(page_address(last_free_page)));
    return 0; /* dummy value: return something ... */
}

static void
remap_page_range (page_index_t from, page_index_t to)
{
    /* There's a mysterious Solaris/x86 problem with using mmap
     * tricks for memory zeroing. See sbcl-devel thread
     * "Re: patch: standalone executable redux".
     */
#if defined(LISP_FEATURE_SUNOS)
    zero_and_mark_pages(from, to);
#else
    const page_index_t
            release_granularity = gencgc_release_granularity/GENCGC_CARD_BYTES,
                   release_mask = release_granularity-1,
                            end = to+1,
                   aligned_from = (from+release_mask)&~release_mask,
                    aligned_end = (end&~release_mask);

    if (aligned_from < aligned_end) {
        zero_pages_with_mmap(aligned_from, aligned_end-1);
        if (aligned_from != from)
            zero_and_mark_pages(from, aligned_from-1);
        if (aligned_end != end)
            zero_and_mark_pages(aligned_end, end-1);
    } else {
        zero_and_mark_pages(from, to);
    }
#endif
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

/* GC all generations newer than last_gen, raising the objects in each
 * to the next older generation - we finish when all generations below
 * last_gen are empty.  Then if last_gen is due for a GC, or if
 * last_gen==NUM_GENERATIONS (the scratch generation?  eh?) we GC that
 * too.  The valid range for last_gen is: 0,1,...,NUM_GENERATIONS.
 *
 * We stop collecting at gencgc_oldest_gen_to_gc, even if this is less than
 * last_gen (oh, and note that by default it is NUM_GENERATIONS-1) */
void
collect_garbage(generation_index_t last_gen)
{
    generation_index_t gen = 0, i;
    int raise, more = 0;
    int gen_to_wp;
    /* The largest value of last_free_page seen since the time
     * remap_free_pages was called. */
    static page_index_t high_water_mark = 0;

    FSHOW((stderr, "/entering collect_garbage(%d)\n", last_gen));
    log_generation_stats(gc_logfile, "=== GC Start ===");

    gc_active_p = 1;

    if (last_gen > HIGHEST_NORMAL_GENERATION+1) {
        FSHOW((stderr,
               "/collect_garbage: last_gen = %d, doing a level 0 GC\n",
               last_gen));
        last_gen = 0;
    }

    /* Flush the alloc regions updating the tables. */
    gc_alloc_update_all_page_tables(1);

    /* Verify the new objects created by Lisp code. */
    if (pre_verify_gen_0) {
        FSHOW((stderr, "pre-checking generation 0\n"));
        verify_generation(0);
    }

    if (gencgc_verbose > 1)
        print_generation_stats();

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    /* Immobile space generation bits are lazily updated for gen0
       (not touched on every object allocation) so do it now */
    update_immobile_nursery_bits();
#endif

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

        if (gencgc_verbose > 1) {
            FSHOW((stderr,
                   "starting GC of generation %d with raise=%d alloc=%d trig=%d GCs=%d\n",
                   gen,
                   raise,
                   generations[gen].bytes_allocated,
                   generations[gen].gc_trigger,
                   generations[gen].num_gc));
        }

        /* If an older generation is being filled, then update its
         * memory age. */
        if (raise == 1) {
            generations[gen+1].cum_sum_bytes_allocated +=
                generations[gen+1].bytes_allocated;
        }

        garbage_collect_generation(gen, raise);

        /* Reset the memory age cum_sum. */
        generations[gen].cum_sum_bytes_allocated = 0;

        if (gencgc_verbose > 1) {
            FSHOW((stderr, "GC of generation %d finished:\n", gen));
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
                lose("trying to write-protect gen. %d when gen. %d nonempty\n",
                     gen_to_wp, i);
        }
        write_protect_generation_pages(gen_to_wp);
    }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    write_protect_immobile_space();
#endif

    /* Set gc_alloc() back to generation 0. The current regions should
     * be flushed after the above GCs. */
    gc_assert(boxed_region.free_pointer == boxed_region.start_addr);
    gc_alloc_generation = 0;

    /* Save the high-water mark before updating last_free_page */
    if (last_free_page > high_water_mark)
        high_water_mark = last_free_page;

    update_dynamic_space_free_pointer();

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
        n = snprintf(buf, sizeof buf, MESSAGE, auto_gc_trigger);
        ignore_value(write(2, buf, n));
#undef MESSAGE
    }

    /* If we did a big GC (arbitrarily defined as gen > 1), release memory
     * back to the OS.
     */
    if (gen > small_generation_limit) {
        if (last_free_page > high_water_mark)
            high_water_mark = last_free_page;
        remap_free_pages(0, high_water_mark);
        high_water_mark = 0;
    }

    gc_active_p = 0;
    large_allocation = 0;

#ifdef LISP_FEATURE_SB_TRACEROOT
    if (gc_object_watcher) {
        extern void gc_prove_liveness(void(*)(), lispobj, int, uword_t*, int);
        gc_prove_liveness(preserve_context_registers,
                          gc_object_watcher,
                          gc_n_stack_pins, pinned_objects.keys,
                          gc_traceroot_criterion);
    }
#endif

    log_generation_stats(gc_logfile, "=== GC End ===");
    SHOW("returning from collect_garbage");
}

/* Initialization of gencgc metadata is split into three steps:
 * 1. gc_init() - allocation of a fixed-address space via mmap(),
 *    failing which there's no reason to go on. (safepoint only)
 * 2. gc_allocate_ptes() - page table entries
 * 3. gencgc_pickup_dynamic() - calculation of scan start offsets
 * Steps (2) and (3) are combined in self-build because there is
 * no PAGE_TABLE_CORE_ENTRY_TYPE_CODE core entry. */
void
gc_init(void)
{
#if defined(LISP_FEATURE_SB_SAFEPOINT)
    alloc_gc_page();
#endif
}

void gc_allocate_ptes()
{
    page_index_t i;

    /* Compute the number of pages needed for the dynamic space.
     * Dynamic space size should be aligned on page size. */
    page_table_pages = dynamic_space_size/GENCGC_CARD_BYTES;
    gc_assert(dynamic_space_size == npage_bytes(page_table_pages));

    /* Default nursery size to 5% of the total dynamic space size,
     * min 1Mb. */
    bytes_consed_between_gcs = dynamic_space_size/(os_vm_size_t)20;
    if (bytes_consed_between_gcs < (1024*1024))
        bytes_consed_between_gcs = 1024*1024;

    /* The page_table must be allocated using "calloc" to initialize
     * the page structures correctly. There used to be a separate
     * initialization loop (now commented out; see below) but that was
     * unnecessary and did hurt startup time. */
    printf("Will alloc 50000 page table entries\n");
    page_table = calloc(50000, sizeof(struct page));
    gc_assert(page_table);
    free(page_table);
    printf("Will alloc %d bytes for %d page tables\n", page_table_pages * sizeof(struct page), page_table_pages);
    page_table = os_allocate(page_table_pages * sizeof(struct page));
    gc_assert(page_table);

    hopscotch_init();
#ifdef PIN_GRANULARITY_LISPOBJ
    hopscotch_create(&pinned_objects, HOPSCOTCH_HASH_FUN_DEFAULT, 0 /* hashset */,
                     32 /* logical bin count */, 0 /* default range */);
#endif

    scavtab[WEAK_POINTER_WIDETAG] = scav_weak_pointer;

    /* The page structures are initialized implicitly when page_table
     * is allocated with "calloc" above. Formerly we had the following
     * explicit initialization here (comments converted to C99 style
     * for readability as C's block comments don't nest):
     *
     * // Initialize each page structure.
     * for (i = 0; i < page_table_pages; i++) {
     *     // Initialize all pages as free.
     *     page_table[i].allocated = FREE_PAGE_FLAG;
     *     page_table[i].bytes_used = 0;
     *
     *     // Pages are not write-protected at startup.
     *     page_table[i].write_protected = 0;
     * }
     *
     * Without this loop the image starts up much faster when dynamic
     * space is large -- which it is on 64-bit platforms already by
     * default -- and when "calloc" for large arrays is implemented
     * using copy-on-write of a page of zeroes -- which it is at least
     * on Linux. In this case the pages that page_table_pages is stored
     * in are mapped and cleared not before the corresponding part of
     * dynamic space is used. For example, this saves clearing 16 MB of
     * memory at startup if the page size is 4 KB and the size of
     * dynamic space is 4 GB.
     * FREE_PAGE_FLAG must be 0 for this to work correctly which is
     * asserted below: */
    {
      /* Compile time assertion: If triggered, declares an array
       * of dimension -1 forcing a syntax error. The intent of the
       * assignment is to avoid an "unused variable" warning. */
      char assert_free_page_flag_0[(FREE_PAGE_FLAG) ? -1 : 1];
      assert_free_page_flag_0[0] = assert_free_page_flag_0[0];
    }

    bytes_allocated = 0;

    /* Initialize the generations. */
    for (i = 0; i < NUM_GENERATIONS; i++) {
        generations[i].alloc_start_page = 0;
        generations[i].alloc_unboxed_start_page = 0;
        generations[i].alloc_large_start_page = 0;
        generations[i].bytes_allocated = 0;
        generations[i].gc_trigger = 2000000;
        generations[i].num_gc = 0;
        generations[i].cum_sum_bytes_allocated = 0;
        /* the tune-able parameters */
        generations[i].bytes_consed_between_gc
            = bytes_consed_between_gcs/(os_vm_size_t)HIGHEST_NORMAL_GENERATION;
        generations[i].number_of_gcs_before_promotion = 1;
        generations[i].minimum_age_before_gc = 0.75;
    }

    /* Initialize gc_alloc. */
    gc_alloc_generation = 0;
    gc_set_region_empty(&boxed_region);
    gc_set_region_empty(&unboxed_region);
#ifdef LISP_FEATURE_SEGREGATED_CODE
    gc_set_region_empty(&code_region);
#endif

    last_free_page = 0;
}

/*  Pick up the dynamic space from after a core load.
 *
 *  The ALLOCATION_POINTER points to the end of the dynamic space.
 */

static void
gencgc_pickup_dynamic(void)
{
    page_index_t page = 0;
    char *alloc_ptr = (char *)get_alloc_pointer();
    lispobj *prev=(lispobj *)page_address(page);
    generation_index_t gen = PSEUDO_STATIC_GENERATION;

    bytes_allocated = 0;

    do {
        lispobj *first,*ptr= (lispobj *)page_address(page);

        if (!gencgc_partial_pickup || !page_free_p(page)) {
          page_bytes_t bytes_used = GENCGC_CARD_BYTES;
          /* It is possible, though rare, for the saved page table
           * to contain free pages below alloc_ptr. */
          page_table[page].gen = gen;
          if (gencgc_partial_pickup)
              bytes_used = page_bytes_used(page);
          else
              set_page_bytes_used(page, GENCGC_CARD_BYTES);
          page_table[page].large_object = 0;
          page_table[page].write_protected = 0;
          page_table[page].write_protected_cleared = 0;
          page_table[page].dont_move = 0;
          set_page_need_to_zero(page, 1);

          bytes_allocated += bytes_used;
        }

        if (!gencgc_partial_pickup) {
#ifdef LISP_FEATURE_SEGREGATED_CODE
            // Make the most general assumption: any page *might* contain code.
            page_table[page].allocated = CODE_PAGE_FLAG;
#else
            page_table[page].allocated = BOXED_PAGE_FLAG;
#endif
            first = gc_search_space3(ptr, prev, (ptr+2));
            if(ptr == first)
                prev=ptr;
            set_page_scan_start_offset(page, page_address(page) - (char*)prev);
        }
        page++;
    } while (page_address(page) < alloc_ptr);

    last_free_page = page;

    generations[gen].bytes_allocated = bytes_allocated;

    gc_alloc_update_all_page_tables(1);
    if (ENABLE_PAGE_PROTECTION)
        write_protect_generation_pages(gen);
}

void
gc_initialize_pointers(void)
{
    /* !page_table_pages happens once only in self-build and not again */
    if (!page_table_pages)
        gc_allocate_ptes();
    gencgc_pickup_dynamic();
}


/* alloc(..) is the external interface for memory allocation. It
 * allocates to generation 0. It is not called from within the garbage
 * collector as it is only external uses that need the check for heap
 * size (GC trigger) and to disable the interrupts (interrupts are
 * always disabled during a GC).
 *
 * The vops that call alloc(..) assume that the returned space is zero-filled.
 * (E.g. the most significant word of a 2-word bignum in MOVE-FROM-UNSIGNED.)
 *
 * The check for a GC trigger is only performed when the current
 * region is full, so in most cases it's not needed. */

static inline lispobj *
general_alloc_internal(sword_t nbytes, int page_type_flag, struct alloc_region *region,
                       struct thread *thread)
{
#ifndef LISP_FEATURE_WIN32
    lispobj alloc_signal;
#endif
    void *new_obj;
    void *new_free_pointer;
    os_vm_size_t trigger_bytes = 0;

    gc_assert(nbytes > 0);

    /* Check for alignment allocation problems. */
    gc_assert((((uword_t)region->free_pointer & LOWTAG_MASK) == 0)
              && ((nbytes & LOWTAG_MASK) == 0));

#if !(defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD))
    /* Must be inside a PA section. */
    gc_assert(get_pseudo_atomic_atomic(thread));
#endif

    if ((os_vm_size_t) nbytes > large_allocation)
        large_allocation = nbytes;

    /* maybe we can do this quickly ... */
    new_free_pointer = (char*)region->free_pointer + nbytes;
    if (new_free_pointer <= region->end_addr) {
        new_obj = (void*)(region->free_pointer);
        region->free_pointer = new_free_pointer;
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
        if (SymbolValue(GC_PENDING,thread) == NIL) {
            /* set things up so that GC happens when we finish the PA
             * section */
            SetSymbolValue(GC_PENDING,T,thread);
            if (SymbolValue(GC_INHIBIT,thread) == NIL) {
#ifdef LISP_FEATURE_SB_SAFEPOINT
                thread_register_gc_trigger();
#else
                set_pseudo_atomic_interrupted(thread);
#ifdef GENCGC_IS_PRECISE
                /* PPC calls alloc() from a trap
                 * look up the most context if it's from a trap. */
                {
                    os_context_t *context =
                        thread->interrupt_data->allocation_trap_context;
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
    new_obj = gc_alloc_with_region(nbytes, page_type_flag, region, 0);

#ifndef LISP_FEATURE_WIN32
    /* for sb-prof, and not supported on Windows yet */
    alloc_signal = SymbolValue(ALLOC_SIGNAL,thread);
    if ((alloc_signal & FIXNUM_TAG_MASK) == 0) {
        if ((sword_t) alloc_signal <= 0) {
            SetSymbolValue(ALLOC_SIGNAL, T, thread);
            raise(SIGPROF);
        } else {
            SetSymbolValue(ALLOC_SIGNAL,
                           alloc_signal - (1 << N_FIXNUM_TAG_BITS),
                           thread);
        }
    }
#endif

    return (new_obj);
}

lispobj *
general_alloc(sword_t nbytes, int page_type_flag)
{
    struct thread *thread = arch_os_get_current_thread();
    /* Select correct region, and call general_alloc_internal with it.
     * For other then boxed allocation we must lock first, since the
     * region is shared. */
#ifdef LISP_FEATURE_SEGREGATED_CODE
    if (page_type_flag == BOXED_PAGE_FLAG) {
#else
    if (BOXED_PAGE_FLAG & page_type_flag) {
#endif
#ifdef LISP_FEATURE_SB_THREAD
        struct alloc_region *region = (thread ? &(thread->alloc_region) : &boxed_region);
#else
        struct alloc_region *region = &boxed_region;
#endif
        return general_alloc_internal(nbytes, page_type_flag, region, thread);
#ifdef LISP_FEATURE_SEGREGATED_CODE
    } else if (page_type_flag == UNBOXED_PAGE_FLAG ||
               page_type_flag == CODE_PAGE_FLAG) {
        struct alloc_region *region =
            page_type_flag == CODE_PAGE_FLAG ? &code_region : &unboxed_region;
#else
    } else if (UNBOXED_PAGE_FLAG == page_type_flag) {
        struct alloc_region *region = &unboxed_region;
#endif
        lispobj * obj;
        int result;
        result = thread_mutex_lock(&allocation_lock);
        gc_assert(!result);
        obj = general_alloc_internal(nbytes, page_type_flag, region, thread);
        result = thread_mutex_unlock(&allocation_lock);
        gc_assert(!result);
        return obj;
    } else {
        lose("bad page type flag: %d", page_type_flag);
    }
}

lispobj AMD64_SYSV_ABI *
alloc(sword_t nbytes)
{
#ifdef LISP_FEATURE_SB_SAFEPOINT_STRICTLY
    struct thread *self = arch_os_get_current_thread();
    int was_pseudo_atomic = get_pseudo_atomic_atomic(self);
    if (!was_pseudo_atomic)
        set_pseudo_atomic_atomic(self);
#else
    gc_assert(get_pseudo_atomic_atomic(arch_os_get_current_thread()));
#endif

    lispobj *result = general_alloc(nbytes, BOXED_PAGE_FLAG);

#ifdef LISP_FEATURE_SB_SAFEPOINT_STRICTLY
    if (!was_pseudo_atomic)
        clear_pseudo_atomic_atomic(self);
#endif

    return result;
}

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

int
gencgc_handle_wp_violation(void* fault_addr)
{
    page_index_t page_index = find_page_index(fault_addr);

#if QSHOW_SIGNALS
    FSHOW((stderr,
           "heap WP violation? fault_addr=%p, page_index=%"PAGE_INDEX_FMT"\n",
           fault_addr, page_index));
#endif

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

    } else {
        int ret;
        ret = thread_mutex_lock(&free_pages_lock);
        gc_assert(ret == 0);
        if (page_table[page_index].write_protected) {
            /* Unprotect the page. */
            os_protect(page_address(page_index), GENCGC_CARD_BYTES, OS_VM_PROT_ALL);
            page_table[page_index].write_protected_cleared = 1;
            page_table[page_index].write_protected = 0;
        } else if (!ignore_memoryfaults_on_unprotected_pages) {
            /* The only acceptable reason for this signal on a heap
             * access is that GENCGC write-protected the page.
             * However, if two CPUs hit a wp page near-simultaneously,
             * we had better not have the second one lose here if it
             * does this test after the first one has already set wp=0
             */
            if(page_table[page_index].write_protected_cleared != 1) {
                void lisp_backtrace(int frames);
                lisp_backtrace(10);
                fprintf(stderr,
                        "Fault @ %p, page %"PAGE_INDEX_FMT" not marked as write-protected:\n"
                        "  boxed_region.first_page: %"PAGE_INDEX_FMT","
                        "  boxed_region.last_page %"PAGE_INDEX_FMT"\n"
                        "  page.scan_start_offset: %"OS_VM_SIZE_FMT"\n"
                        "  page.bytes_used: %u\n"
                        "  page.allocated: %d\n"
                        "  page.write_protected: %d\n"
                        "  page.write_protected_cleared: %d\n"
                        "  page.generation: %d\n",
                        fault_addr,
                        page_index,
                        boxed_region.first_page,
                        boxed_region.last_page,
                        page_scan_start_offset(page_index),
                        page_bytes_used(page_index),
                        page_table[page_index].allocated,
                        page_table[page_index].write_protected,
                        page_table[page_index].write_protected_cleared,
                        page_table[page_index].gen);
                if (!continue_after_memoryfault_on_unprotected_pages)
                    lose("Feh.\n");
            }
        }
        ret = thread_mutex_unlock(&free_pages_lock);
        gc_assert(ret == 0);
        /* Don't worry, we can handle it. */
        return 1;
    }
}
/* This is to be called when we catch a SIGSEGV/SIGBUS, determine that
 * it's not just a case of the program hitting the write barrier, and
 * are about to let Lisp deal with it. It's basically just a
 * convenient place to set a gdb breakpoint. */
void
unhandled_sigmemoryfault(void *addr)
{}

static void
update_thread_page_tables(struct thread *th)
{
    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->alloc_region);
#if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)
    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->sprof_alloc_region);
#endif
}

/* GC is single-threaded and all memory allocations during a
   collection happen in the GC thread, so it is sufficient to update
   all the the page tables once at the beginning of a collection and
   update only page tables of the GC thread during the collection. */
void gc_alloc_update_all_page_tables(int for_all_threads)
{
    /* Flush the alloc regions updating the tables. */
    struct thread *th;
    if (for_all_threads) {
        for_each_thread(th) {
            update_thread_page_tables(th);
        }
    }
    else {
        th = arch_os_get_current_thread();
        if (th) {
            update_thread_page_tables(th);
        }
    }
#ifdef LISP_FEATURE_SEGREGATED_CODE
    gc_alloc_update_page_tables(CODE_PAGE_FLAG, &code_region);
#endif
    gc_alloc_update_page_tables(UNBOXED_PAGE_FLAG, &unboxed_region);
    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &boxed_region);
}

void
gc_set_region_empty(struct alloc_region *region)
{
    region->first_page = 0;
    region->last_page = -1;
    region->start_addr = page_address(0);
    region->free_pointer = page_address(0);
    region->end_addr = page_address(0);
}

static void
zero_all_free_pages()
{
    page_index_t i;

    for (i = 0; i < last_free_page; i++) {
        if (page_free_p(i)) {
#ifdef READ_PROTECT_FREE_PAGES
            os_protect(page_address(i),
                       GENCGC_CARD_BYTES,
                       OS_VM_PROT_ALL);
#endif
            zero_pages(i, i);
        }
    }
}

/* Things to do before doing a final GC before saving a core (without
 * purify).
 *
 * + Pages in large_object pages aren't moved by the GC, so we need to
 *   unset that flag from all pages.
 * + The pseudo-static generation isn't normally collected, but it seems
 *   reasonable to collect it at least when saving a core. So move the
 *   pages to a normal generation.
 */
static void
prepare_for_final_gc ()
{
    page_index_t i;

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    extern void prepare_immobile_space_for_final_gc();
    prepare_immobile_space_for_final_gc ();
#endif
    for (i = 0; i < last_free_page; i++) {
        page_table[i].large_object = 0;
        if (page_table[i].gen == PSEUDO_STATIC_GENERATION) {
            int used = page_bytes_used(i);
            page_table[i].gen = HIGHEST_NORMAL_GENERATION;
            generations[PSEUDO_STATIC_GENERATION].bytes_allocated -= used;
            generations[HIGHEST_NORMAL_GENERATION].bytes_allocated += used;
        }
    }
}

/* Set this switch to 1 for coalescing of strings dumped to fasl,
 * or 2 for coalescing of those,
 * plus literal strings in code compiled to memory. */
char gc_coalesce_string_literals = 0;

/* Do a non-conservative GC, and then save a core with the initial
 * function being set to the value of 'lisp_init_function' */
void
gc_and_save(char *filename, boolean prepend_runtime, boolean c_linkable_core,
            boolean save_runtime_options, boolean compressed,
            int compression_level, int application_type)
{
    FILE *file;
    void *runtime_bytes = NULL;
    size_t runtime_size;
    extern void coalesce_similar_objects();
    extern struct lisp_startup_options lisp_startup_options;
    boolean verbose = !lisp_startup_options.noinform;

    /* The filename might come from Lisp, and be moved by the now
     * non-conservative GC. */
    filename = strdup(filename);

    /* Ignore stack contents when preparing to save. */
    conservative_stack = 0;

    /* Collect twice: once into relatively high memory, and then back
     * into low memory. This compacts the retained data into the lower
     * pages, minimizing the size of the core file.
     */
    prepare_for_final_gc();
    gencgc_alloc_start_page = last_free_page;
    collect_garbage(HIGHEST_NORMAL_GENERATION+1);

    // We always coalesce copyable numbers. Addional coalescing is done
    // only on request, in which case a message is shown (unless verbose=0).
    if (gc_coalesce_string_literals && verbose) {
        printf("[coalescing similar vectors... ");
        fflush(stdout);
    }
    coalesce_similar_objects();
    if (gc_coalesce_string_literals && verbose)
        printf("done]\n");

    prepare_for_final_gc();
    gencgc_alloc_start_page = -1;
    collect_garbage(HIGHEST_NORMAL_GENERATION+1);

    if (c_linkable_core) {
#ifdef LISP_FEATURE_SB_ELF_CORE
        /* The dumper doesn't know that pages need to be zeroed before use. */
        zero_all_free_pages();
        save_elf_core(filename, lisp_init_function);
#else
        lose("Attempt to save ELF core without enabling :SB-ELF-CORE\n");
#endif
    } else {
        file = prepare_to_save(filename, prepend_runtime, &runtime_bytes,
                               &runtime_size);
        if (file == NULL)
            return;

        if (prepend_runtime)
            save_runtime_to_filehandle(file, runtime_bytes, runtime_size,
                                       application_type);

        /* The dumper doesn't know that pages need to be zeroed before use. */
        zero_all_free_pages();
        save_to_filehandle(file, filename, lisp_init_function,
                           prepend_runtime, save_runtime_options,
                           compressed ? compression_level : COMPRESSION_LEVEL_NONE);
    }

    /* Oops. Save still managed to fail. Since we've mangled the stack
     * beyond hope, there's not much we can do.
     * (beyond FUNCALLing lisp_init_function, but I suspect that's
     * going to be rather unsatisfactory too... */
    lose("Attempt to save core after non-conservative GC failed.\n");
}
