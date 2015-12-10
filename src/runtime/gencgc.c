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
#include "genesis/vector.h"
#include "genesis/weak-pointer.h"
#include "genesis/fdefn.h"
#include "genesis/simple-fun.h"
#include "save.h"
#include "genesis/hash-table.h"
#include "genesis/instance.h"
#include "genesis/layout.h"
#include "gencgc.h"
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
#include "genesis/cons.h"
#endif

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

/* Should we use page protection to help avoid the scavenging of pages
 * that don't have pointers to younger generations? */
boolean enable_page_protection = 1;

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

/* Should we check for bad pointers after gc_free_heap is called
 * from Lisp PURIFY? */
boolean verify_after_free_heap = 0;

/* Should we print a note when code objects are found in the dynamic space
 * during a heap verify? */
boolean verify_dynamic_code_check = 0;

#ifdef LISP_FEATURE_X86
/* Should we check code objects for fixup errors after they are transported? */
boolean check_code_fixups = 0;
#endif

/* Should we check that newly allocated regions are zero filled? */
boolean gencgc_zero_check = 0;

/* Should we check that the free space is zero filled? */
boolean gencgc_enable_verify_zero_fill = 0;

/* Should we check that free pages are zero filled during gc_free_heap
 * called after Lisp PURIFY? */
boolean gencgc_zero_check_during_free_heap = 0;

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

in_use_marker_t *page_table_dontmove_dwords;
size_t page_table_dontmove_dwords_size_in_bytes;

/* In GC cards that have conservative pointers to them, should we wipe out
 * dwords in there that are not used, so that they do not act as false
 * root to other things in the heap from then on? This is a new feature
 * but in testing it is both reliable and no noticeable slowdown. */
int do_wipe_p = 1;

/* a value that we use to wipe out unused words in GC cards that
 * live alongside conservatively to pointed words. */
const lispobj wipe_with = 0;

static inline boolean page_allocated_p(page_index_t page) {
    return (page_table[page].allocated != FREE_PAGE_FLAG);
}

static inline boolean page_no_region_p(page_index_t page) {
    return !(page_table[page].allocated & OPEN_REGION_PAGE_FLAG);
}

static inline boolean page_allocated_no_region_p(page_index_t page) {
    return ((page_table[page].allocated & (UNBOXED_PAGE_FLAG | BOXED_PAGE_FLAG))
            && page_no_region_p(page));
}

static inline boolean page_free_p(page_index_t page) {
    return (page_table[page].allocated == FREE_PAGE_FLAG);
}

static inline boolean page_boxed_p(page_index_t page) {
    return (page_table[page].allocated & BOXED_PAGE_FLAG);
}

static inline boolean page_boxed_no_region_p(page_index_t page) {
    return page_boxed_p(page) && page_no_region_p(page);
}

static inline boolean page_unboxed_p(page_index_t page) {
    /* Both flags set == boxed code page */
    return ((page_table[page].allocated & UNBOXED_PAGE_FLAG)
            && !page_boxed_p(page));
}

static inline boolean protect_page_p(page_index_t page, generation_index_t generation) {
    return (page_boxed_no_region_p(page)
            && (page_table[page].bytes_used != 0)
            && !page_table[page].dont_move
            && (page_table[page].gen == generation));
}

/* To map addresses to page structures the address of the first page
 * is needed. */
void *heap_base = NULL;

/* Calculate the start address for the given page number. */
inline void *
page_address(page_index_t page_num)
{
    return (heap_base + (page_num * GENCGC_CARD_BYTES));
}

/* Calculate the address where the allocation region associated with
 * the page starts. */
static inline void *
page_scan_start(page_index_t page_index)
{
    return page_address(page_index)-page_table[page_index].scan_start_offset;
}

/* True if the page starts a contiguous block. */
static inline boolean
page_starts_contiguous_block_p(page_index_t page_index)
{
    return page_table[page_index].scan_start_offset == 0;
}

/* True if the page is the last page in a contiguous block. */
static inline boolean
page_ends_contiguous_block_p(page_index_t page_index, generation_index_t gen)
{
    return (/* page doesn't fill block */
            (page_table[page_index].bytes_used < GENCGC_CARD_BYTES)
            /* page is last allocated page */
            || ((page_index + 1) >= last_free_page)
            /* next page free */
            || page_free_p(page_index + 1)
            /* next page contains no data */
            || (page_table[page_index + 1].bytes_used == 0)
            /* next page is in different generation */
            || (page_table[page_index + 1].gen != gen)
            /* next page starts its own contiguous block */
            || (page_starts_contiguous_block_p(page_index + 1)));
}

/* Find the page index within the page_table for the given
 * address. Return -1 on failure. */
inline page_index_t
find_page_index(void *addr)
{
    if (addr >= heap_base) {
        page_index_t index = ((pointer_sized_uint_t)addr -
                              (pointer_sized_uint_t)heap_base) / GENCGC_CARD_BYTES;
        if (index < page_table_pages)
            return (index);
    }
    return (-1);
}

static os_vm_size_t
npage_bytes(page_index_t npages)
{
    gc_assert(npages>=0);
    return ((os_vm_size_t)npages)*GENCGC_CARD_BYTES;
}

/* Check that X is a higher address than Y and return offset from Y to
 * X in bytes. */
static inline os_vm_size_t
void_diff(void *x, void *y)
{
    gc_assert(x >= y);
    return (pointer_sized_uint_t)x - (pointer_sized_uint_t)y;
}

/* a structure to hold the state of a generation
 *
 * CAUTION: If you modify this, make sure to touch up the alien
 * definition in src/code/gc.lisp accordingly. ...or better yes,
 * deal with the FIXME there...
 */
struct generation {

    /* the first page that gc_alloc() checks on its next call */
    page_index_t alloc_start_page;

    /* the first page that gc_alloc_unboxed() checks on its next call */
    page_index_t alloc_unboxed_start_page;

    /* the first page that gc_alloc_large (boxed) considers on its next
     * call. (Although it always allocates after the boxed_region.) */
    page_index_t alloc_large_start_page;

    /* the first page that gc_alloc_large (unboxed) considers on its
     * next call. (Although it always allocates after the
     * current_unboxed_region.) */
    page_index_t alloc_large_unboxed_start_page;

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
        if (page_allocated_p(i)
            && (page_table[i].gen == generation)
            && (page_table[i].write_protected == 1))
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
        if (page_allocated_p(i)
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
        if (page_allocated_p(i)
            && (page_table[i].dont_move != 0)) {
            ++count;
        }
    }
    return count;
}
#endif /* QSHOW */

/* Work through the pages and add up the number of bytes used for the
 * given generation. */
static os_vm_size_t
count_generation_bytes_allocated (generation_index_t gen)
{
    page_index_t i;
    os_vm_size_t result = 0;
    for (i = 0; i < last_free_page; i++) {
        if (page_allocated_p(i)
            && (page_table[i].gen == gen))
            result += page_table[i].bytes_used;
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

extern void
write_generation_stats(FILE *file)
{
    generation_index_t i;

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
#define FPU_STATE_SIZE 27
    int fpu_state[FPU_STATE_SIZE];
#elif defined(LISP_FEATURE_PPC)
#define FPU_STATE_SIZE 32
    long long fpu_state[FPU_STATE_SIZE];
#elif defined(LISP_FEATURE_SPARC)
    /*
     * 32 (single-precision) FP registers, and the FP state register.
     * But Sparc V9 has 32 double-precision registers (equivalent to 64
     * single-precision, but can't be accessed), so we leave enough room
     * for that.
     */
#define FPU_STATE_SIZE (((32 + 32 + 1) + 1)/2)
    long long fpu_state[FPU_STATE_SIZE];
#elif defined(LISP_FEATURE_ARM)
    #define FPU_STATE_SIZE 8
    long long fpu_state[FPU_STATE_SIZE];
#elif defined(LISP_FEATURE_ARM64)
    #define FPU_STATE_SIZE 64
    long fpu_state[FPU_STATE_SIZE];
#endif

    /* This code uses the FP instructions which may be set up for Lisp
     * so they need to be saved and reset for C. */
    fpu_save(fpu_state);

    /* Print the heap stats. */
    fprintf(file,
            " Gen StaPg UbSta LaSta LUbSt Boxed Unboxed LB   LUB  !move  Alloc  Waste   Trig    WP  GCs Mem-age\n");

    for (i = 0; i < SCRATCH_GENERATION; i++) {
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
                "   %1d: %5ld %5ld %5ld %5ld",
                i,
                generations[i].alloc_start_page,
                generations[i].alloc_unboxed_start_page,
                generations[i].alloc_large_start_page,
                generations[i].alloc_large_unboxed_start_page);
        fprintf(file,
                " %5"PAGE_INDEX_FMT" %5"PAGE_INDEX_FMT" %5"PAGE_INDEX_FMT
                " %5"PAGE_INDEX_FMT" %5"PAGE_INDEX_FMT,
                boxed_cnt, unboxed_cnt, large_boxed_cnt,
                large_unboxed_cnt, pinned_cnt);
        fprintf(file,
                " %8"OS_VM_SIZE_FMT
                " %5"OS_VM_SIZE_FMT
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

    fpu_restore(fpu_state);
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

    os_invalidate(addr, length);
    new_addr = os_validate(addr, length);
    if (new_addr == NULL || new_addr != addr) {
        lose("remap_free_pages: page moved, 0x%08x ==> 0x%08x",
             start, new_addr);
    }

    for (i = start; i <= end; i++) {
        page_table[i].need_to_zero = 0;
    }
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
        page_table[i].need_to_zero = 0;
}

/* Zero the pages from START to END (inclusive), except for those
 * pages that are known to already zeroed. Mark all pages in the
 * ranges as non-zeroed.
 */
static void
zero_dirty_pages(page_index_t start, page_index_t end) {
    page_index_t i, j;

    for (i = start; i <= end; i++) {
        if (!page_table[i].need_to_zero) continue;
        for (j = i+1; (j <= end) && (page_table[j].need_to_zero); j++);
        zero_pages(i, j-1);
        i = j;
    }

    for (i = start; i <= end; i++) {
        page_table[i].need_to_zero = 1;
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

/* We are only using two regions at present. Both are for the current
 * newspace generation. */
struct alloc_region boxed_region;
struct alloc_region unboxed_region;

/* The generation currently being allocated to. */
static generation_index_t gc_alloc_generation;

static inline page_index_t
generation_alloc_start_page(generation_index_t generation, int page_type_flag, int large)
{
    if (large) {
        if (UNBOXED_PAGE_FLAG == page_type_flag) {
            return generations[generation].alloc_large_unboxed_start_page;
        } else if (BOXED_PAGE_FLAG & page_type_flag) {
            /* Both code and data. */
            return generations[generation].alloc_large_start_page;
        } else {
            lose("bad page type flag: %d", page_type_flag);
        }
    } else {
        if (UNBOXED_PAGE_FLAG == page_type_flag) {
            return generations[generation].alloc_unboxed_start_page;
        } else if (BOXED_PAGE_FLAG & page_type_flag) {
            /* Both code and data. */
            return generations[generation].alloc_start_page;
        } else {
            lose("bad page_type_flag: %d", page_type_flag);
        }
    }
}

static inline void
set_generation_alloc_start_page(generation_index_t generation, int page_type_flag, int large,
                                page_index_t page)
{
    if (large) {
        if (UNBOXED_PAGE_FLAG == page_type_flag) {
            generations[generation].alloc_large_unboxed_start_page = page;
        } else if (BOXED_PAGE_FLAG & page_type_flag) {
            /* Both code and data. */
            generations[generation].alloc_large_start_page = page;
        } else {
            lose("bad page type flag: %d", page_type_flag);
        }
    } else {
        if (UNBOXED_PAGE_FLAG == page_type_flag) {
            generations[generation].alloc_unboxed_start_page = page;
        } else if (BOXED_PAGE_FLAG & page_type_flag) {
            /* Both code and data. */
            generations[generation].alloc_start_page = page;
        } else {
            lose("bad page type flag: %d", page_type_flag);
        }
    }
}

const int n_dwords_in_card = GENCGC_CARD_BYTES / N_WORD_BYTES / 2;
in_use_marker_t *
dontmove_dwords(page_index_t page)
{
    if (page_table[page].has_dontmove_dwords)
        return &page_table_dontmove_dwords[page * n_dwords_in_card];
    return NULL;
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
    os_vm_size_t bytes_found;
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
    bytes_found=(GENCGC_CARD_BYTES - page_table[first_page].bytes_used)
            + npage_bytes(last_page-first_page);

    /* Set up the alloc_region. */
    alloc_region->first_page = first_page;
    alloc_region->last_page = last_page;
    alloc_region->start_addr = page_table[first_page].bytes_used
        + page_address(first_page);
    alloc_region->free_pointer = alloc_region->start_addr;
    alloc_region->end_addr = alloc_region->start_addr + bytes_found;

    /* Set up the pages. */

    /* The first page may have already been in use. */
    if (page_table[first_page].bytes_used == 0) {
        page_table[first_page].allocated = page_type_flag;
        page_table[first_page].gen = gc_alloc_generation;
        page_table[first_page].large_object = 0;
        page_table[first_page].scan_start_offset = 0;
        // wiping should have free()ed and :=NULL
        gc_assert(dontmove_dwords(first_page) == NULL);
    }

    gc_assert(page_table[first_page].allocated == page_type_flag);
    page_table[first_page].allocated |= OPEN_REGION_PAGE_FLAG;

    gc_assert(page_table[first_page].gen == gc_alloc_generation);
    gc_assert(page_table[first_page].large_object == 0);

    for (i = first_page+1; i <= last_page; i++) {
        page_table[i].allocated = page_type_flag;
        page_table[i].gen = gc_alloc_generation;
        page_table[i].large_object = 0;
        /* This may not be necessary for unboxed regions (think it was
         * broken before!) */
        page_table[i].scan_start_offset =
            void_diff(page_address(i),alloc_region->start_addr);
        page_table[i].allocated |= OPEN_REGION_PAGE_FLAG ;
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
    if (page_table[first_page].bytes_used) {
        first_page++;
    }

    zero_dirty_pages(first_page, last_page);

    /* we can do this after releasing free_pages_lock */
    if (gencgc_zero_check) {
        word_t *p;
        for (p = (word_t *)alloc_region->start_addr;
             p < (word_t *)alloc_region->end_addr; p++) {
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
        orig_first_page_bytes_used = page_table[first_page].bytes_used;

        gc_assert(alloc_region->start_addr ==
                  (page_address(first_page)
                   + page_table[first_page].bytes_used));

        /* All the pages used need to be updated */

        /* Update the first page. */

        /* If the page was free then set up the gen, and
         * scan_start_offset. */
        if (page_table[first_page].bytes_used == 0)
            gc_assert(page_starts_contiguous_block_p(first_page));
        page_table[first_page].allocated &= ~(OPEN_REGION_PAGE_FLAG);

        gc_assert(page_table[first_page].allocated & page_type_flag);
        gc_assert(page_table[first_page].gen == gc_alloc_generation);
        gc_assert(page_table[first_page].large_object == 0);

        byte_cnt = 0;

        /* Calculate the number of bytes used in this page. This is not
         * always the number of new bytes, unless it was free. */
        more = 0;
        if ((bytes_used = void_diff(alloc_region->free_pointer,
                                    page_address(first_page)))
            >GENCGC_CARD_BYTES) {
            bytes_used = GENCGC_CARD_BYTES;
            more = 1;
        }
        page_table[first_page].bytes_used = bytes_used;
        byte_cnt += bytes_used;


        /* All the rest of the pages should be free. We need to set
         * their scan_start_offset pointer to the start of the
         * region, and set the bytes_used. */
        while (more) {
            page_table[next_page].allocated &= ~(OPEN_REGION_PAGE_FLAG);
            gc_assert(page_table[next_page].allocated & page_type_flag);
            gc_assert(page_table[next_page].bytes_used == 0);
            gc_assert(page_table[next_page].gen == gc_alloc_generation);
            gc_assert(page_table[next_page].large_object == 0);

            gc_assert(page_table[next_page].scan_start_offset ==
                      void_diff(page_address(next_page),
                                alloc_region->start_addr));

            /* Calculate the number of bytes used in this page. */
            more = 0;
            if ((bytes_used = void_diff(alloc_region->free_pointer,
                                        page_address(next_page)))>GENCGC_CARD_BYTES) {
                bytes_used = GENCGC_CARD_BYTES;
                more = 1;
            }
            page_table[next_page].bytes_used = bytes_used;
            byte_cnt += bytes_used;

            next_page++;
        }

        region_size = void_diff(alloc_region->free_pointer,
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
        if (page_table[first_page].bytes_used == 0)
            page_table[first_page].allocated = FREE_PAGE_FLAG;
    }

    /* Unallocate any unused pages. */
    while (next_page <= alloc_region->last_page) {
        gc_assert(page_table[next_page].bytes_used == 0);
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
    page_bytes_t orig_first_page_bytes_used;
    os_vm_size_t byte_cnt;
    os_vm_size_t bytes_used;
    int ret;

    ret = thread_mutex_lock(&free_pages_lock);
    gc_assert(ret == 0);

    first_page = generation_alloc_start_page(gc_alloc_generation, page_type_flag, 1);
    if (first_page <= alloc_region->last_page) {
        first_page = alloc_region->last_page+1;
    }

    last_page=gc_find_freeish_pages(&first_page,nbytes, page_type_flag);

    gc_assert(first_page > alloc_region->last_page);

    set_generation_alloc_start_page(gc_alloc_generation, page_type_flag, 1, last_page);

    /* Set up the pages. */
    orig_first_page_bytes_used = page_table[first_page].bytes_used;

    /* If the first page was free then set up the gen, and
     * scan_start_offset. */
    if (page_table[first_page].bytes_used == 0) {
        page_table[first_page].allocated = page_type_flag;
        page_table[first_page].gen = gc_alloc_generation;
        page_table[first_page].scan_start_offset = 0;
        page_table[first_page].large_object = 1;
    }

    gc_assert(page_table[first_page].allocated == page_type_flag);
    gc_assert(page_table[first_page].gen == gc_alloc_generation);
    gc_assert(page_table[first_page].large_object == 1);

    byte_cnt = 0;

    /* Calc. the number of bytes used in this page. This is not
     * always the number of new bytes, unless it was free. */
    more = 0;
    if ((bytes_used = nbytes+orig_first_page_bytes_used) > GENCGC_CARD_BYTES) {
        bytes_used = GENCGC_CARD_BYTES;
        more = 1;
    }
    page_table[first_page].bytes_used = bytes_used;
    byte_cnt += bytes_used;

    next_page = first_page+1;

    /* All the rest of the pages should be free. We need to set their
     * scan_start_offset pointer to the start of the region, and set
     * the bytes_used. */
    while (more) {
        gc_assert(page_free_p(next_page));
        gc_assert(page_table[next_page].bytes_used == 0);
        page_table[next_page].allocated = page_type_flag;
        page_table[next_page].gen = gc_alloc_generation;
        page_table[next_page].large_object = 1;

        page_table[next_page].scan_start_offset =
            npage_bytes(next_page-first_page) - orig_first_page_bytes_used;

        /* Calculate the number of bytes used in this page. */
        more = 0;
        bytes_used=(nbytes+orig_first_page_bytes_used)-byte_cnt;
        if (bytes_used > GENCGC_CARD_BYTES) {
            bytes_used = GENCGC_CARD_BYTES;
            more = 1;
        }
        page_table[next_page].bytes_used = bytes_used;
        page_table[next_page].write_protected=0;
        page_table[next_page].dont_move=0;
        byte_cnt += bytes_used;
        next_page++;
    }

    gc_assert((byte_cnt-orig_first_page_bytes_used) == (size_t)nbytes);

    bytes_allocated += nbytes;
    generations[gc_alloc_generation].bytes_allocated += nbytes;

    /* Add the region to the new_areas if requested. */
    if (BOXED_PAGE_FLAG & page_type_flag)
        add_new_area(first_page,orig_first_page_bytes_used,nbytes);

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
                gc_assert(0 == page_table[first_page].bytes_used);
                bytes_found = GENCGC_CARD_BYTES;
        } else if (small_object &&
                   (page_table[first_page].allocated == page_type_flag) &&
                   (page_table[first_page].large_object == 0) &&
                   (page_table[first_page].gen == gc_alloc_generation) &&
                   (page_table[first_page].write_protected == 0) &&
                   (page_table[first_page].dont_move == 0)) {
            bytes_found = GENCGC_CARD_BYTES - page_table[first_page].bytes_used;
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

        gc_assert(page_table[first_page].write_protected == 0);
        for (last_page = first_page+1;
             ((last_page < page_table_pages) &&
              page_free_p(last_page) &&
              (bytes_found < nbytes_goal));
             last_page++) {
            bytes_found += GENCGC_CARD_BYTES;
            gc_assert(0 == page_table[last_page].bytes_used);
            gc_assert(0 == page_table[last_page].write_protected);
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
    new_free_pointer = my_region->free_pointer + nbytes;

    /* fprintf(stderr, "alloc %d bytes from %p to %p\n", nbytes,
       my_region->free_pointer, new_free_pointer); */

    if (new_free_pointer <= my_region->end_addr) {
        /* If so then allocate from the current alloc region. */
        void *new_obj = my_region->free_pointer;
        my_region->free_pointer = new_free_pointer;

        /* Unless a `quick' alloc was requested, check whether the
           alloc region is almost empty. */
        if (!quick_p &&
            void_diff(my_region->end_addr,my_region->free_pointer) <= 32) {
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
general_copy_large_object(lispobj object, word_t nwords, boolean boxedp)
{
    int tag;
    lispobj *new;
    page_index_t first_page;

    gc_assert(is_lisp_pointer(object));
    gc_assert(from_space_p(object));
    gc_assert((nwords & 0x01) == 0);

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
            gc_assert(page_table[next_page].scan_start_offset ==
                      npage_bytes(next_page-first_page));
            gc_assert(page_table[next_page].bytes_used == GENCGC_CARD_BYTES);
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
        gc_assert(page_table[next_page].bytes_used >= remaining_bytes);

        page_table[next_page].gen = new_space;

        if (boxedp)
            gc_assert(page_boxed_p(next_page));
        else
            page_table[next_page].allocated = UNBOXED_PAGE_FLAG;

        /* Adjust the bytes_used. */
        old_bytes_used = page_table[next_page].bytes_used;
        page_table[next_page].bytes_used = remaining_bytes;

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
               (page_table[next_page].scan_start_offset ==
                npage_bytes(next_page - first_page))) {
            /* Checks out OK, free the page. Don't need to both zeroing
             * pages as this should have been done before shrinking the
             * object. These pages shouldn't be write-protected, even if
             * boxed they should be zero filled. */
            gc_assert(page_table[next_page].write_protected == 0);

            old_bytes_used = page_table[next_page].bytes_used;
            page_table[next_page].allocated = FREE_PAGE_FLAG;
            page_table[next_page].bytes_used = 0;
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
        /* Get tag of object. */
        tag = lowtag_of(object);

        /* Allocate space. */
        new = gc_general_alloc(nwords*N_WORD_BYTES,
                               (boxedp ? BOXED_PAGE_FLAG : UNBOXED_PAGE_FLAG),
                               ALLOC_QUICK);

        /* Copy the object. */
        memcpy(new,native_pointer(object),nwords*N_WORD_BYTES);

        /* Return Lisp pointer of new object. */
        return ((lispobj) new) | tag;
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
 * code and code-related objects
 */
/*
static lispobj trans_fun_header(lispobj object);
static lispobj trans_boxed(lispobj object);
*/

/* Scan a x86 compiled code object, looking for possible fixups that
 * have been missed after a move.
 *
 * Two types of fixups are needed:
 * 1. Absolute fixups to within the code object.
 * 2. Relative fixups to outside the code object.
 *
 * Currently only absolute fixups to the constant vector, or to the
 * code area are checked. */
#ifdef LISP_FEATURE_X86
void
sniff_code_object(struct code *code, os_vm_size_t displacement)
{
    sword_t nheader_words, ncode_words, nwords;
    os_vm_address_t constants_start_addr = NULL, constants_end_addr, p;
    os_vm_address_t code_start_addr, code_end_addr;
    os_vm_address_t code_addr = (os_vm_address_t)code;
    int fixup_found = 0;

    if (!check_code_fixups)
        return;

    FSHOW((stderr, "/sniffing code: %p, %lu\n", code, displacement));

    ncode_words = fixnum_word_value(code->code_size);
    nheader_words = HeaderValue(*(lispobj *)code);
    nwords = ncode_words + nheader_words;

    constants_start_addr = code_addr + 5*N_WORD_BYTES;
    constants_end_addr = code_addr + nheader_words*N_WORD_BYTES;
    code_start_addr = code_addr + nheader_words*N_WORD_BYTES;
    code_end_addr = code_addr + nwords*N_WORD_BYTES;

    /* Work through the unboxed code. */
    for (p = code_start_addr; p < code_end_addr; p++) {
        void *data = *(void **)p;
        unsigned d1 = *((unsigned char *)p - 1);
        unsigned d2 = *((unsigned char *)p - 2);
        unsigned d3 = *((unsigned char *)p - 3);
        unsigned d4 = *((unsigned char *)p - 4);
#if QSHOW
        unsigned d5 = *((unsigned char *)p - 5);
        unsigned d6 = *((unsigned char *)p - 6);
#endif

        /* Check for code references. */
        /* Check for a 32 bit word that looks like an absolute
           reference to within the code adea of the code object. */
        if ((data >= (void*)(code_start_addr-displacement))
            && (data < (void*)(code_end_addr-displacement))) {
            /* function header */
            if ((d4 == 0x5e)
                && (((unsigned)p - 4 - 4*HeaderValue(*((unsigned *)p-1))) ==
                    (unsigned)code)) {
                /* Skip the function header */
                p += 6*4 - 4 - 1;
                continue;
            }
            /* the case of PUSH imm32 */
            if (d1 == 0x68) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/code ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                FSHOW((stderr, "/PUSH $0x%.8x\n", data));
            }
            /* the case of MOV [reg-8],imm32 */
            if ((d3 == 0xc7)
                && (d2==0x40 || d2==0x41 || d2==0x42 || d2==0x43
                    || d2==0x45 || d2==0x46 || d2==0x47)
                && (d1 == 0xf8)) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/code ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                FSHOW((stderr, "/MOV [reg-8],$0x%.8x\n", data));
            }
            /* the case of LEA reg,[disp32] */
            if ((d2 == 0x8d) && ((d1 & 0xc7) == 5)) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/code ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                FSHOW((stderr,"/LEA reg,[$0x%.8x]\n", data));
            }
        }

        /* Check for constant references. */
        /* Check for a 32 bit word that looks like an absolute
           reference to within the constant vector. Constant references
           will be aligned. */
        if ((data >= (void*)(constants_start_addr-displacement))
            && (data < (void*)(constants_end_addr-displacement))
            && (((unsigned)data & 0x3) == 0)) {
            /*  Mov eax,m32 */
            if (d1 == 0xa1) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                FSHOW((stderr,"/MOV eax,0x%.8x\n", data));
            }

            /*  the case of MOV m32,EAX */
            if (d1 == 0xa3) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                FSHOW((stderr, "/MOV 0x%.8x,eax\n", data));
            }

            /* the case of CMP m32,imm32 */
            if ((d1 == 0x3d) && (d2 == 0x81)) {
                fixup_found = 1;
                FSHOW((stderr,
                       "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                       p, d6, d5, d4, d3, d2, d1, data));
                /* XX Check this */
                FSHOW((stderr, "/CMP 0x%.8x,immed32\n", data));
            }

            /* Check for a mod=00, r/m=101 byte. */
            if ((d1 & 0xc7) == 5) {
                /* Cmp m32,reg */
                if (d2 == 0x39) {
                    fixup_found = 1;
                    FSHOW((stderr,
                           "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                           p, d6, d5, d4, d3, d2, d1, data));
                    FSHOW((stderr,"/CMP 0x%.8x,reg\n", data));
                }
                /* the case of CMP reg32,m32 */
                if (d2 == 0x3b) {
                    fixup_found = 1;
                    FSHOW((stderr,
                           "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                           p, d6, d5, d4, d3, d2, d1, data));
                    FSHOW((stderr, "/CMP reg32,0x%.8x\n", data));
                }
                /* the case of MOV m32,reg32 */
                if (d2 == 0x89) {
                    fixup_found = 1;
                    FSHOW((stderr,
                           "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                           p, d6, d5, d4, d3, d2, d1, data));
                    FSHOW((stderr, "/MOV 0x%.8x,reg32\n", data));
                }
                /* the case of MOV reg32,m32 */
                if (d2 == 0x8b) {
                    fixup_found = 1;
                    FSHOW((stderr,
                           "/abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                           p, d6, d5, d4, d3, d2, d1, data));
                    FSHOW((stderr, "/MOV reg32,0x%.8x\n", data));
                }
                /* the case of LEA reg32,m32 */
                if (d2 == 0x8d) {
                    fixup_found = 1;
                    FSHOW((stderr,
                           "abs const ref @%x: %.2x %.2x %.2x %.2x %.2x %.2x (%.8x)\n",
                           p, d6, d5, d4, d3, d2, d1, data));
                    FSHOW((stderr, "/LEA reg32,0x%.8x\n", data));
                }
            }
        }
    }

    /* If anything was found, print some information on the code
     * object. */
    if (fixup_found) {
        FSHOW((stderr,
               "/compiled code object at %x: header words = %d, code words = %d\n",
               code, nheader_words, ncode_words));
        FSHOW((stderr,
               "/const start = %x, end = %x\n",
               constants_start_addr, constants_end_addr));
        FSHOW((stderr,
               "/code start = %x, end = %x\n",
               code_start_addr, code_end_addr));
    }
}
#endif

#ifdef LISP_FEATURE_X86
void
gencgc_apply_code_fixups(struct code *old_code, struct code *new_code)
{
    sword_t nheader_words, ncode_words, nwords;
    os_vm_address_t constants_start_addr, constants_end_addr;
    os_vm_address_t code_start_addr, code_end_addr;
    os_vm_address_t code_addr = (os_vm_address_t)new_code;
    os_vm_address_t old_addr = (os_vm_address_t)old_code;
    os_vm_size_t displacement = code_addr - old_addr;
    lispobj fixups = NIL;
    struct vector *fixups_vector;

    ncode_words = fixnum_word_value(new_code->code_size);
    nheader_words = HeaderValue(*(lispobj *)new_code);
    nwords = ncode_words + nheader_words;
    /* FSHOW((stderr,
             "/compiled code object at %x: header words = %d, code words = %d\n",
             new_code, nheader_words, ncode_words)); */
    constants_start_addr = code_addr + 5*N_WORD_BYTES;
    constants_end_addr = code_addr + nheader_words*N_WORD_BYTES;
    code_start_addr = code_addr + nheader_words*N_WORD_BYTES;
    code_end_addr = code_addr + nwords*N_WORD_BYTES;
    /*
    FSHOW((stderr,
           "/const start = %x, end = %x\n",
           constants_start_addr,constants_end_addr));
    FSHOW((stderr,
           "/code start = %x; end = %x\n",
           code_start_addr,code_end_addr));
    */

    /* The first constant should be a pointer to the fixups for this
       code objects. Check. */
    fixups = new_code->constants[0];

    /* It will be 0 or the unbound-marker if there are no fixups (as
     * will be the case if the code object has been purified, for
     * example) and will be an other pointer if it is valid. */
    if ((fixups == 0) || (fixups == UNBOUND_MARKER_WIDETAG) ||
        !is_lisp_pointer(fixups)) {
        /* Check for possible errors. */
        if (check_code_fixups)
            sniff_code_object(new_code, displacement);

        return;
    }

    fixups_vector = (struct vector *)native_pointer(fixups);

    /* Could be pointing to a forwarding pointer. */
    /* FIXME is this always in from_space?  if so, could replace this code with
     * forwarding_pointer_p/forwarding_pointer_value */
    if (is_lisp_pointer(fixups) &&
        (find_page_index((void*)fixups_vector) != -1) &&
        (fixups_vector->header == 0x01)) {
        /* If so, then follow it. */
        /*SHOW("following pointer to a forwarding pointer");*/
        fixups_vector =
            (struct vector *)native_pointer((lispobj)fixups_vector->length);
    }

    /*SHOW("got fixups");*/

    if (widetag_of(fixups_vector->header) == SIMPLE_ARRAY_WORD_WIDETAG) {
        /* Got the fixups for the code block. Now work through the vector,
           and apply a fixup at each address. */
        sword_t length = fixnum_value(fixups_vector->length);
        sword_t i;
        for (i = 0; i < length; i++) {
            long offset = fixups_vector->data[i];
            /* Now check the current value of offset. */
            os_vm_address_t old_value = *(os_vm_address_t *)(code_start_addr + offset);

            /* If it's within the old_code object then it must be an
             * absolute fixup (relative ones are not saved) */
            if ((old_value >= old_addr)
                && (old_value < (old_addr + nwords*N_WORD_BYTES)))
                /* So add the dispacement. */
                *(os_vm_address_t *)(code_start_addr + offset) =
                    old_value + displacement;
            else
                /* It is outside the old code object so it must be a
                 * relative fixup (absolute fixups are not saved). So
                 * subtract the displacement. */
                *(os_vm_address_t *)(code_start_addr + offset) =
                    old_value - displacement;
        }
    } else {
        /* This used to just print a note to stderr, but a bogus fixup seems to
         * indicate real heap corruption, so a hard hailure is in order. */
        lose("fixup vector %p has a bad widetag: %d\n",
             fixups_vector, widetag_of(fixups_vector->header));
    }

    /* Check for possible errors. */
    if (check_code_fixups) {
        sniff_code_object(new_code,displacement);
    }
}
#endif

static lispobj
trans_boxed_large(lispobj object)
{
    lispobj header;
    uword_t length;

    gc_assert(is_lisp_pointer(object));

    header = *((lispobj *) native_pointer(object));
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return copy_large_object(object, length);
}

/* Doesn't seem to be used, delete it after the grace period. */
#if 0
static lispobj
trans_unboxed_large(lispobj object)
{
    lispobj header;
    uword_t length;

    gc_assert(is_lisp_pointer(object));

    header = *((lispobj *) native_pointer(object));
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return copy_large_unboxed_object(object, length);
}
#endif

/*
 * weak pointers
 */

/* XX This is a hack adapted from cgc.c. These don't work too
 * efficiently with the gencgc as a list of the weak pointers is
 * maintained within the objects which causes writes to the pages. A
 * limited attempt is made to avoid unnecessary writes, but this needs
 * a re-think. */
#define WEAK_POINTER_NWORDS \
    CEILING((sizeof(struct weak_pointer) / sizeof(lispobj)), 2)

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

    if (NULL == wp->next) {
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
    lispobj *end = (lispobj *) SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0);
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return (gc_search_space(start,
                            (((lispobj *)pointer)+2)-start,
                            (lispobj *) pointer));
}

lispobj *
search_static_space(void *pointer)
{
    lispobj *start = (lispobj *)STATIC_SPACE_START;
    lispobj *end = (lispobj *)SymbolValue(STATIC_SPACE_FREE_POINTER,0);
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return (gc_search_space(start,
                            (((lispobj *)pointer)+2)-start,
                            (lispobj *) pointer));
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
    return (gc_search_space(start,
                            (((lispobj *)pointer)+2)-start,
                            (lispobj *)pointer));
}

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)

/* Is there any possibility that pointer is a valid Lisp object
 * reference, and/or something else (e.g. subroutine call return
 * address) which should prevent us from moving the referred-to thing?
 * This is called from preserve_pointers() */
static int
possibly_valid_dynamic_space_pointer_s(lispobj *pointer,
                                       page_index_t addr_page_index,
                                       lispobj **store_here)
{
    lispobj *start_addr;

    /* Find the object start address. */
    start_addr = search_dynamic_space(pointer);

    if (start_addr == NULL) {
        return 0;
    }
    if (store_here) {
        *store_here = start_addr;
    }

    /* If the containing object is a code object, presume that the
     * pointer is valid, simply because it could be an unboxed return
     * address. */
    if (widetag_of(*start_addr) == CODE_HEADER_WIDETAG)
        return 1;

    /* Large object pages only contain ONE object, and it will never
     * be a CONS.  However, arrays and bignums can be allocated larger
     * than necessary and then shrunk to fit, leaving what look like
     * (0 . 0) CONSes at the end.  These appear valid to
     * looks_like_valid_lisp_pointer_p(), so pick them off here. */
    if (page_table[addr_page_index].large_object &&
        (lowtag_of((lispobj)pointer) == LIST_POINTER_LOWTAG))
        return 0;

    return looks_like_valid_lisp_pointer_p((lispobj)pointer, start_addr);
}

#endif  // defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)

static int
valid_conservative_root_p(void *addr, page_index_t addr_page_index,
                          lispobj **begin_ptr)
{
#ifdef GENCGC_IS_PRECISE
    /* If we're in precise gencgc (non-x86oid as of this writing) then
     * we are only called on valid object pointers in the first place,
     * so we just have to do a bounds-check against the heap, a
     * generation check, and the already-pinned check. */
    if ((addr_page_index == -1)
        || (page_table[addr_page_index].gen != from_space)
        || (page_table[addr_page_index].dont_move != 0))
        return 0;
#else
    /* quick check 1: Address is quite likely to have been invalid. */
    if ((addr_page_index == -1)
        || page_free_p(addr_page_index)
        || (page_table[addr_page_index].bytes_used == 0)
        || (page_table[addr_page_index].gen != from_space))
        return 0;
    gc_assert(!(page_table[addr_page_index].allocated&OPEN_REGION_PAGE_FLAG));

    /* quick check 2: Check the offset within the page.
     *
     */
    if (((uword_t)addr & (GENCGC_CARD_BYTES - 1)) >
        page_table[addr_page_index].bytes_used)
        return 0;

    /* Filter out anything which can't be a pointer to a Lisp object
     * (or, as a special case which also requires dont_move, a return
     * address referring to something in a CodeObject). This is
     * expensive but important, since it vastly reduces the
     * probability that random garbage will be bogusly interpreted as
     * a pointer which prevents a page from moving. */
    if (!possibly_valid_dynamic_space_pointer_s(addr, addr_page_index,
                                                begin_ptr))
        return 0;
#endif

    return 1;
}

boolean
in_dontmove_dwordindex_p(page_index_t page_index, int dword_in_page)
{
    in_use_marker_t *marker;
    marker = dontmove_dwords(page_index);
    if (marker)
        return marker[dword_in_page];
    return 0;
}
boolean
in_dontmove_nativeptr_p(page_index_t page_index, lispobj *native_ptr)
{
    if (dontmove_dwords(page_index)) {
        lispobj *begin = page_address(page_index);
        int dword_in_page = (native_ptr - begin) / 2;
        return in_dontmove_dwordindex_p(page_index, dword_in_page);
    } else {
        return 0;
    }
}

/* Adjust large bignum and vector objects. This will adjust the
 * allocated region if the size has shrunk, and move unboxed objects
 * into unboxed pages. The pages are not promoted here, and the
 * promoted region is not added to the new_regions; this is really
 * only designed to be called from preserve_pointer(). Shouldn't fail
 * if this is missed, just may delay the moving of objects to unboxed
 * pages, and the freeing of pages. */
static void
maybe_adjust_large_object(lispobj *where)
{
    page_index_t first_page;
    page_index_t next_page;
    sword_t nwords;

    uword_t remaining_bytes;
    uword_t bytes_freed;
    uword_t old_bytes_used;

    int boxed;

    /* Check whether it's a vector or bignum object. */
    switch (widetag_of(where[0])) {
    case SIMPLE_VECTOR_WIDETAG:
        boxed = BOXED_PAGE_FLAG;
        break;
    case BIGNUM_WIDETAG:
    case SIMPLE_BASE_STRING_WIDETAG:
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
    case SIMPLE_CHARACTER_STRING_WIDETAG:
#endif
    case SIMPLE_BIT_VECTOR_WIDETAG:
    case SIMPLE_ARRAY_NIL_WIDETAG:
    case SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG:
    case SIMPLE_ARRAY_UNSIGNED_BYTE_4_WIDETAG:
    case SIMPLE_ARRAY_UNSIGNED_BYTE_7_WIDETAG:
    case SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG:
    case SIMPLE_ARRAY_UNSIGNED_BYTE_15_WIDETAG:
    case SIMPLE_ARRAY_UNSIGNED_BYTE_16_WIDETAG:

    case SIMPLE_ARRAY_UNSIGNED_FIXNUM_WIDETAG:

    case SIMPLE_ARRAY_UNSIGNED_BYTE_31_WIDETAG:
    case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG
    case SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG
    case SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
    case SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
    case SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG:
#endif

    case SIMPLE_ARRAY_FIXNUM_WIDETAG:

#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
    case SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG
    case SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG:
#endif
    case SIMPLE_ARRAY_SINGLE_FLOAT_WIDETAG:
    case SIMPLE_ARRAY_DOUBLE_FLOAT_WIDETAG:
#ifdef SIMPLE_ARRAY_LONG_FLOAT_WIDETAG
    case SIMPLE_ARRAY_LONG_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
    case SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
    case SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
    case SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG:
#endif
        boxed = UNBOXED_PAGE_FLAG;
        break;
    default:
        return;
    }

    /* Find its current size. */
    nwords = (sizetab[widetag_of(where[0])])(where);

    first_page = find_page_index((void *)where);
    gc_assert(first_page >= 0);

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
        gc_assert(page_allocated_no_region_p(next_page));
        gc_assert(page_table[next_page].large_object);
        gc_assert(page_table[next_page].scan_start_offset ==
                  npage_bytes(next_page-first_page));
        gc_assert(page_table[next_page].bytes_used == GENCGC_CARD_BYTES);

        page_table[next_page].allocated = boxed;

        /* Shouldn't be write-protected at this stage. Essential that the
         * pages aren't. */
        gc_assert(!page_table[next_page].write_protected);
        remaining_bytes -= GENCGC_CARD_BYTES;
        next_page++;
    }

    /* Now only one page remains, but the object may have shrunk so
     * there may be more unused pages which will be freed. */

    /* Object may have shrunk but shouldn't have grown - check. */
    gc_assert(page_table[next_page].bytes_used >= remaining_bytes);

    page_table[next_page].allocated = boxed;
    gc_assert(page_table[next_page].allocated ==
              page_table[first_page].allocated);

    /* Adjust the bytes_used. */
    old_bytes_used = page_table[next_page].bytes_used;
    page_table[next_page].bytes_used = remaining_bytes;

    bytes_freed = old_bytes_used - remaining_bytes;

    /* Free any remaining pages; needs care. */
    next_page++;
    while ((old_bytes_used == GENCGC_CARD_BYTES) &&
           (page_table[next_page].gen == from_space) &&
           page_allocated_no_region_p(next_page) &&
           page_table[next_page].large_object &&
           (page_table[next_page].scan_start_offset ==
            npage_bytes(next_page - first_page))) {
        /* It checks out OK, free the page. We don't need to both zeroing
         * pages as this should have been done before shrinking the
         * object. These pages shouldn't be write protected as they
         * should be zero filled. */
        gc_assert(page_table[next_page].write_protected == 0);

        old_bytes_used = page_table[next_page].bytes_used;
        page_table[next_page].allocated = FREE_PAGE_FLAG;
        page_table[next_page].bytes_used = 0;
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

/*
 * Why is this restricted to protected objects only?
 * Because the rest of the page has been scavenged already,
 * and since that leaves forwarding pointers in the unprotected
 * areas you cannot scavenge it again until those are gone.
 */
void
scavenge_pages_with_conservative_pointers_to_them_protected_objects_only()
{
    page_index_t i;
    for (i = 0; i < last_free_page; i++) {
        if (!dontmove_dwords(i)) {
            continue;
        }
        lispobj *begin = page_address(i);
        unsigned int dword;

        lispobj *scavme_begin = NULL;
        for (dword = 0; dword < GENCGC_CARD_BYTES / N_WORD_BYTES / 2; dword++) {
            if (in_dontmove_dwordindex_p(i, dword)) {
                if (!scavme_begin) {
                    scavme_begin = begin + dword * 2;
                }
            } else {
                // contiguous area stopped
                if (scavme_begin) {
                    scavenge(scavme_begin, (begin + dword * 2) - scavme_begin);
                }
                scavme_begin = NULL;
            }
        }
        if (scavme_begin) {
            scavenge(scavme_begin, (begin + dword * 2) - scavme_begin);
        }
    }
}

int verbosefixes = 0;
void
do_the_wipe()
{
    page_index_t i;
    lispobj *begin;
    int words_wiped = 0;
    int lisp_pointers_wiped = 0;
    int pages_considered = 0;
    int n_pages_cannot_wipe = 0;

    for (i = 0; i < last_free_page; i++) {
        if (!page_table[i].dont_move) {
            continue;
        }
        pages_considered++;
        if (!dontmove_dwords(i)) {
            n_pages_cannot_wipe++;
            continue;
        }
        begin = page_address(i);
        unsigned int dword;
        for (dword = 0; dword < GENCGC_CARD_BYTES / N_WORD_BYTES / 2; dword++) {
            if (!in_dontmove_dwordindex_p(i, dword)) {
                if (is_lisp_pointer(*(begin + dword * 2))) {
                    lisp_pointers_wiped++;
                }
                if (is_lisp_pointer(*(begin + dword * 2 + 1))) {
                    lisp_pointers_wiped++;
                }
                *(begin + dword * 2) = wipe_with;
                *(begin + dword * 2 + 1) = wipe_with;
                words_wiped += 2;
            }
        }
        page_table[i].has_dontmove_dwords = 0;

        // move the page to newspace
        generations[new_space].bytes_allocated += page_table[i].bytes_used;
        generations[page_table[i].gen].bytes_allocated -= page_table[i].bytes_used;
        page_table[i].gen = new_space;
    }
#ifndef LISP_FEATURE_WIN32
    madvise(page_table_dontmove_dwords, page_table_dontmove_dwords_size_in_bytes, MADV_DONTNEED);
#endif
    if ((verbosefixes >= 1 && lisp_pointers_wiped > 0) || verbosefixes >= 2) {
        fprintf(stderr, "gencgc: wiped %d words (%d lisp_pointers) in %d pages, cannot wipe %d pages \n"
                , words_wiped, lisp_pointers_wiped, pages_considered, n_pages_cannot_wipe);
    }
}

void
set_page_consi_bit(page_index_t pageindex, lispobj *mark_which_pointer)
{
    struct page *page = &page_table[pageindex];

    if (!do_wipe_p)
      return;

    gc_assert(mark_which_pointer);
    if (!page->has_dontmove_dwords) {
        page->has_dontmove_dwords = 1;
        bzero(dontmove_dwords(pageindex),
              sizeof(in_use_marker_t) * n_dwords_in_card);
    }
    int size = (sizetab[widetag_of(mark_which_pointer[0])])(mark_which_pointer);
    if (size == 1 &&
        (fixnump(*mark_which_pointer) ||
         is_lisp_pointer(*mark_which_pointer) ||
         lowtag_of(*mark_which_pointer) == 9 ||
         lowtag_of(*mark_which_pointer) == 2)) {
        size = 2;
    }
    if (size % 2 != 0) {
        fprintf(stderr, "WIPE ERROR !dword, size %d, lowtag %d, world 0x%lld\n",
                size,
                lowtag_of(*mark_which_pointer),
                (long long)*mark_which_pointer);
    }
    gc_assert(size % 2 == 0);
    lispobj *begin = page_address(pageindex);
    int begin_dword = (mark_which_pointer - begin) / 2;
    int dword;
    in_use_marker_t *marker = dontmove_dwords(pageindex);
    for (dword = begin_dword; dword < begin_dword + size / 2; dword++) {
        marker[dword] = 1;
    }
}

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

static void
preserve_pointer(void *addr)
{
    page_index_t addr_page_index = find_page_index(addr);
    page_index_t first_page;
    page_index_t i;
    unsigned int region_allocation;
    lispobj *begin_ptr = NULL;

    if (!valid_conservative_root_p(addr, addr_page_index, &begin_ptr))
        return;

    /* (Now that we know that addr_page_index is in range, it's
     * safe to index into page_table[] with it.) */
    region_allocation = page_table[addr_page_index].allocated;

    /* Find the beginning of the region.  Note that there may be
     * objects in the region preceding the one that we were passed a
     * pointer to: if this is the case, we will write-protect all the
     * previous objects' pages too.     */

#if 0
    /* I think this'd work just as well, but without the assertions.
     * -dan 2004.01.01 */
    first_page = find_page_index(page_scan_start(addr_page_index))
#else
    first_page = addr_page_index;
    while (!page_starts_contiguous_block_p(first_page)) {
        --first_page;
        /* Do some checks. */
        gc_assert(page_table[first_page].bytes_used == GENCGC_CARD_BYTES);
        gc_assert(page_table[first_page].gen == from_space);
        gc_assert(page_table[first_page].allocated == region_allocation);
    }
#endif

    /* Adjust any large objects before promotion as they won't be
     * copied after promotion. */
    if (page_table[first_page].large_object) {
        maybe_adjust_large_object(page_address(first_page));
        /* It may have moved to unboxed pages. */
        region_allocation = page_table[first_page].allocated;
    }

    /* Now work forward until the end of this contiguous area is found,
     * marking all pages as dont_move. */
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

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    /* Do not do this for multi-page objects.  Those pages do not need
     * object wipeout anyway.
     */
    if (i == first_page) {
        /* We need the pointer to the beginning of the object
         * We might have gotten it above but maybe not, so make sure
         */
        if (begin_ptr == NULL) {
            possibly_valid_dynamic_space_pointer_s(addr, first_page,
                                                   &begin_ptr);
        }
        set_page_consi_bit(first_page, begin_ptr);
    }
#endif

    /* Check that the page is now static. */
    gc_assert(page_table[addr_page_index].dont_move != 0);
}

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
    sword_t num_words = page_table[page].bytes_used / N_WORD_BYTES;

    /* Shouldn't be a free page. */
    gc_assert(page_allocated_p(page));
    gc_assert(page_table[page].bytes_used != 0);

    /* Skip if it's already write-protected, pinned, or unboxed */
    if (page_table[page].write_protected
        /* FIXME: What's the reason for not write-protecting pinned pages? */
        || page_table[page].dont_move
        || page_unboxed_p(page))
        return (0);

    /* Scan the page for pointers to younger generations or the
     * top temp. generation. */

    /* This is conservative: any word satisfying is_lisp_pointer() is
     * assumed to be a pointer despite that it might be machine code
     * or part of an unboxed array */
    for (j = 0; j < num_words; j++) {
        void *ptr = *(page_addr+j);
        page_index_t index;

        /* Check that it's in the dynamic space */
        if (is_lisp_pointer((lispobj)ptr) && (index = find_page_index(ptr)) != -1)
            if (/* Does it point to a younger or the temp. generation? */
                (page_allocated_p(index)
                 && (page_table[index].bytes_used != 0)
                 && ((page_table[index].gen < gen)
                     || (page_table[index].gen == SCRATCH_GENERATION)))

                /* Or does it point within a current gc_alloc() region? */
                || ((boxed_region.start_addr <= ptr)
                    && (ptr <= boxed_region.free_pointer))
                || ((unboxed_region.start_addr <= ptr)
                    && (ptr <= unboxed_region.free_pointer))) {
                wp_it = 0;
                break;
            }
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
            && (page_table[i].bytes_used != 0)
            && (generation != new_space)
            && (generation >= from)
            && (generation <= to)) {
            page_index_t last_page,j;
            int write_protected=1;

            /* This should be the start of a region */
            gc_assert(page_starts_contiguous_block_p(i));

            /* Now work forward until the end of the region */
            for (last_page = i; ; last_page++) {
                write_protected =
                    write_protected && page_table[last_page].write_protected;
                if (page_ends_contiguous_block_p(last_page, generation))
                    break;
            }
            if (!write_protected) {
                scavenge(page_address(i),
                         ((uword_t)(page_table[last_page].bytes_used
                                          + npage_bytes(last_page-i)))
                         /N_WORD_BYTES);

                /* Now scan the pages and write protect those that
                 * don't have pointers to younger generations. */
                if (enable_page_protection) {
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
            i = last_page;
        }
    }

#if SC_GEN_CK
    /* Check that none of the write_protected pages in this generation
     * have been written to. */
    for (i = 0; i < page_table_pages; i++) {
        if (page_allocated_p(i)
            && (page_table[i].bytes_used != 0)
            && (page_table[i].gen == generation)
            && (page_table[i].write_protected_cleared != 0)) {
            FSHOW((stderr, "/scavenge_generation() %d\n", generation));
            FSHOW((stderr,
                   "/page bytes_used=%d scan_start_offset=%lu dont_move=%d\n",
                    page_table[i].bytes_used,
                    page_table[i].scan_start_offset,
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
            && (page_table[i].bytes_used != 0)
            && (page_table[i].gen == generation)
            && ((page_table[i].write_protected == 0)
                /* (This may be redundant as write_protected is now
                 * cleared before promotion.) */
                || (page_table[i].dont_move == 1))) {
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
                sword_t nwords = (((uword_t)
                               (page_table[last_page].bytes_used
                                + npage_bytes(last_page-i)
                                + page_table[i].scan_start_offset))
                               / N_WORD_BYTES);
                new_areas_ignore_page = last_page;

                scavenge(page_scan_start(i), nwords);

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
    gc_alloc_update_all_page_tables();

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
    gc_alloc_update_all_page_tables();

    /* Grab new_areas_index. */
    current_new_areas_index = new_areas_index;

    /*FSHOW((stderr,
             "The first scan is finished; current_new_areas_index=%d.\n",
             current_new_areas_index));*/

    while (current_new_areas_index > 0) {
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
            gc_alloc_update_all_page_tables();

        } else {

            /* Work through previous_new_areas. */
            for (i = 0; i < previous_new_areas_index; i++) {
                page_index_t page = (*previous_new_areas)[i].page;
                size_t offset = (*previous_new_areas)[i].offset;
                size_t size = (*previous_new_areas)[i].size / N_WORD_BYTES;
                gc_assert((*previous_new_areas)[i].size % N_WORD_BYTES == 0);
                scavenge(page_address(page)+offset, size);
            }

            scav_weak_hash_tables();

            /* Flush the current regions updating the tables. */
            gc_alloc_update_all_page_tables();
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
            if (page_allocated_p(i)
                && (page_table[i].bytes_used != 0)
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
    void *region_addr = 0;
    void *page_addr = 0;
    uword_t region_bytes = 0;

    for (i = 0; i < last_free_page; i++) {
        if (page_allocated_p(i)
            && (page_table[i].bytes_used != 0)
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
                   || (page_table[first_page].bytes_used == 0)
                   || (page_table[first_page].gen != from_space)))
            first_page++;

        if (first_page >= last_free_page)
            break;

        /* Find the last page of this region. */
        last_page = first_page;

        do {
            /* Free the page. */
            bytes_freed += page_table[last_page].bytes_used;
            generations[page_table[last_page].gen].bytes_allocated -=
                page_table[last_page].bytes_used;
            page_table[last_page].allocated = FREE_PAGE_FLAG;
            page_table[last_page].bytes_used = 0;
            /* Should already be unprotected by unprotect_oldspace(). */
            gc_assert(!page_table[last_page].write_protected);
            last_page++;
        }
        while ((last_page < last_free_page)
               && page_allocated_p(last_page)
               && (page_table[last_page].bytes_used != 0)
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
                page_table[pi1].bytes_used,
                page_table[pi1].scan_start_offset,
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

static void
verify_space(lispobj *start, size_t words)
{
    int is_in_dynamic_space = (find_page_index((void*)start) != -1);
    int is_in_readonly_space =
        (READ_ONLY_SPACE_START <= (uword_t)start &&
         (uword_t)start < SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0));

    while (words > 0) {
        size_t count = 1;
        lispobj thing = *(lispobj*)start;

        if (is_lisp_pointer(thing)) {
            page_index_t page_index = find_page_index((void*)thing);
            sword_t to_readonly_space =
                (READ_ONLY_SPACE_START <= thing &&
                 thing < SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0));
            sword_t to_static_space =
                (STATIC_SPACE_START <= thing &&
                 thing < SymbolValue(STATIC_SPACE_FREE_POINTER,0));

            /* Does it point to the dynamic space? */
            if (page_index != -1) {
                /* If it's within the dynamic space it should point to a used
                 * page. XX Could check the offset too. */
                if (page_allocated_p(page_index)
                    && (page_table[page_index].bytes_used == 0))
                    lose ("Ptr %p @ %p sees free page.\n", thing, start);
                /* Check that it doesn't point to a forwarding pointer! */
                if (*((lispobj *)native_pointer(thing)) == 0x01) {
                    lose("Ptr %p @ %p sees forwarding ptr.\n", thing, start);
                }
                /* Check that its not in the RO space as it would then be a
                 * pointer from the RO to the dynamic space. */
                if (is_in_readonly_space) {
                    lose("ptr to dynamic space %p from RO space %x\n",
                         thing, start);
                }
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
                if (!possibly_valid_dynamic_space_pointer_s((lispobj *)thing, page_index, NULL)) {
                    lose("ptr %p to invalid object %p\n", thing, start);
                }
                */
            } else {
                extern void funcallable_instance_tramp;
                /* Verify that it points to another valid space. */
                if (!to_readonly_space && !to_static_space
                    && (thing != (lispobj)&funcallable_instance_tramp)
                    && !is_in_stack_space(thing)) {
                    lose("Ptr %p @ %p sees junk.\n", thing, start);
                }
            }
        } else {
            if (!(fixnump(thing))) {
                /* skip fixnums */
                switch(widetag_of(*start)) {

                    /* boxed objects */
                case SIMPLE_VECTOR_WIDETAG:
                case RATIO_WIDETAG:
                case COMPLEX_WIDETAG:
                case SIMPLE_ARRAY_WIDETAG:
                case COMPLEX_BASE_STRING_WIDETAG:
#ifdef COMPLEX_CHARACTER_STRING_WIDETAG
                case COMPLEX_CHARACTER_STRING_WIDETAG:
#endif
                case COMPLEX_VECTOR_NIL_WIDETAG:
                case COMPLEX_BIT_VECTOR_WIDETAG:
                case COMPLEX_VECTOR_WIDETAG:
                case COMPLEX_ARRAY_WIDETAG:
                case CLOSURE_HEADER_WIDETAG:
                case FUNCALLABLE_INSTANCE_HEADER_WIDETAG:
                case VALUE_CELL_HEADER_WIDETAG:
                case SYMBOL_HEADER_WIDETAG:
                case CHARACTER_WIDETAG:
#if N_WORD_BITS == 64
                case SINGLE_FLOAT_WIDETAG:
#endif
                case UNBOUND_MARKER_WIDETAG:
                case FDEFN_WIDETAG:
                    count = 1;
                    break;

                case INSTANCE_HEADER_WIDETAG:
                    {
                        sword_t ntotal = instance_length(thing);
                        lispobj layout = instance_layout(start);
                        if (!layout) {
                            count = 1;
                            break;
                        }
#ifdef LISP_FEATURE_INTERLEAVED_RAW_SLOTS
                        instance_scan_interleaved(verify_space,
                                                  start, ntotal,
                                                  native_pointer(layout));
#else
                        lispobj nuntagged;
                        nuntagged = ((struct layout *)
                                     native_pointer(layout))->n_untagged_slots;
                        verify_space(start + 1,
                                     ntotal - fixnum_value(nuntagged));
#endif
                        count = ntotal + 1;
                        break;
                    }
                case CODE_HEADER_WIDETAG:
                    {
                        lispobj object = *start;
                        struct code *code;
                        sword_t nheader_words, ncode_words, nwords;
                        lispobj fheaderl;
                        struct simple_fun *fheaderp;

                        code = (struct code *) start;

                        /* Check that it's not in the dynamic space.
                         * FIXME: Isn't is supposed to be OK for code
                         * objects to be in the dynamic space these days? */
                        /* It is for byte compiled code, but there's
                         * no byte compilation in SBCL anymore. */
                        if (is_in_dynamic_space
                            /* Only when enabled */
                            && verify_dynamic_code_check) {
                            FSHOW((stderr,
                                   "/code object at %p in the dynamic space\n",
                                   start));
                        }

                        ncode_words = fixnum_word_value(code->code_size);
                        nheader_words = HeaderValue(object);
                        nwords = ncode_words + nheader_words;
                        nwords = CEILING(nwords, 2);
                        /* Scavenge the boxed section of the code data block */
                        verify_space(start + 1, nheader_words - 1);

                        /* Scavenge the boxed section of each function
                         * object in the code data block. */
                        fheaderl = code->entry_points;
                        while (fheaderl != NIL) {
                            fheaderp =
                                (struct simple_fun *) native_pointer(fheaderl);
                            gc_assert(widetag_of(fheaderp->header) ==
                                      SIMPLE_FUN_HEADER_WIDETAG);
                            verify_space(SIMPLE_FUN_SCAV_START(fheaderp),
                                         SIMPLE_FUN_SCAV_NWORDS(fheaderp));
                            fheaderl = fheaderp->next;
                        }
                        count = nwords;
                        break;
                    }

                    /* unboxed objects */
                case BIGNUM_WIDETAG:
#if N_WORD_BITS != 64
                case SINGLE_FLOAT_WIDETAG:
#endif
                case DOUBLE_FLOAT_WIDETAG:
#ifdef COMPLEX_LONG_FLOAT_WIDETAG
                case LONG_FLOAT_WIDETAG:
#endif
#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
                case COMPLEX_SINGLE_FLOAT_WIDETAG:
#endif
#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
                case COMPLEX_DOUBLE_FLOAT_WIDETAG:
#endif
#ifdef COMPLEX_LONG_FLOAT_WIDETAG
                case COMPLEX_LONG_FLOAT_WIDETAG:
#endif
#ifdef SIMD_PACK_WIDETAG
                case SIMD_PACK_WIDETAG:
#endif
                case SIMPLE_BASE_STRING_WIDETAG:
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
                case SIMPLE_CHARACTER_STRING_WIDETAG:
#endif
                case SIMPLE_BIT_VECTOR_WIDETAG:
                case SIMPLE_ARRAY_NIL_WIDETAG:
                case SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG:
                case SIMPLE_ARRAY_UNSIGNED_BYTE_4_WIDETAG:
                case SIMPLE_ARRAY_UNSIGNED_BYTE_7_WIDETAG:
                case SIMPLE_ARRAY_UNSIGNED_BYTE_8_WIDETAG:
                case SIMPLE_ARRAY_UNSIGNED_BYTE_15_WIDETAG:
                case SIMPLE_ARRAY_UNSIGNED_BYTE_16_WIDETAG:

                case SIMPLE_ARRAY_UNSIGNED_FIXNUM_WIDETAG:

                case SIMPLE_ARRAY_UNSIGNED_BYTE_31_WIDETAG:
                case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG
                case SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG
                case SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
                case SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
                case SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG:
#endif

                case SIMPLE_ARRAY_FIXNUM_WIDETAG:

#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
                case SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG
                case SIMPLE_ARRAY_SIGNED_BYTE_64_WIDETAG:
#endif
                case SIMPLE_ARRAY_SINGLE_FLOAT_WIDETAG:
                case SIMPLE_ARRAY_DOUBLE_FLOAT_WIDETAG:
#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
                case SIMPLE_ARRAY_LONG_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG
                case SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG
                case SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG
                case SIMPLE_ARRAY_COMPLEX_LONG_FLOAT_WIDETAG:
#endif
                case SAP_WIDETAG:
                case WEAK_POINTER_WIDETAG:
#ifdef NO_TLS_VALUE_MARKER_WIDETAG
                case NO_TLS_VALUE_MARKER_WIDETAG:
#endif
                    count = (sizetab[widetag_of(*start)])(start);
                    break;

                default:
                    lose("Unhandled widetag %p at %p\n",
                         widetag_of(*start), start);
                }
            }
        }
        start += count;
        words -= count;
    }
}

static void
verify_gc(void)
{
    /* FIXME: It would be nice to make names consistent so that
     * foo_size meant size *in* *bytes* instead of size in some
     * arbitrary units. (Yes, this caused a bug, how did you guess?:-)
     * Some counts of lispobjs are called foo_count; it might be good
     * to grep for all foo_size and rename the appropriate ones to
     * foo_count. */
    sword_t read_only_space_size =
        (lispobj*)SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0)
        - (lispobj*)READ_ONLY_SPACE_START;
    sword_t static_space_size =
        (lispobj*)SymbolValue(STATIC_SPACE_FREE_POINTER,0)
        - (lispobj*)STATIC_SPACE_START;
    struct thread *th;
    for_each_thread(th) {
    sword_t binding_stack_size =
        (lispobj*)get_binding_stack_pointer(th)
            - (lispobj*)th->binding_stack_start;
        verify_space(th->binding_stack_start, binding_stack_size);
    }
    verify_space((lispobj*)READ_ONLY_SPACE_START, read_only_space_size);
    verify_space((lispobj*)STATIC_SPACE_START   , static_space_size);
}

static void
verify_generation(generation_index_t generation)
{
    page_index_t i;

    for (i = 0; i < last_free_page; i++) {
        if (page_allocated_p(i)
            && (page_table[i].bytes_used != 0)
            && (page_table[i].gen == generation)) {
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
                if (page_ends_contiguous_block_p(last_page, generation))
                    break;

            verify_space(page_address(i),
                         ((uword_t)
                          (page_table[last_page].bytes_used
                           + npage_bytes(last_page-i)))
                         / N_WORD_BYTES);
            i = last_page;
        }
    }
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
            sword_t size = 1024;
            sword_t i;
            for (i = 0; i < size; i++) {
                if (start_addr[i] != 0) {
                    lose("free page not zero at %x\n", start_addr + i);
                }
            }
        } else {
            sword_t free_bytes = GENCGC_CARD_BYTES - page_table[page].bytes_used;
            if (free_bytes > 0) {
                sword_t *start_addr = (sword_t *)((uword_t)page_address(page)
                                          + page_table[page].bytes_used);
                sword_t size = free_bytes / N_WORD_BYTES;
                sword_t i;
                for (i = 0; i < size; i++) {
                    if (start_addr[i] != 0) {
                        lose("free region not zero at %x\n", start_addr + i);
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
    gc_alloc_update_all_page_tables();
    SHOW("verifying zero fill");
    verify_zero_fill();
}

static void
verify_dynamic_space(void)
{
    generation_index_t i;

    for (i = 0; i <= HIGHEST_NORMAL_GENERATION; i++)
        verify_generation(i);

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

            page_start = (void *)page_address(start);

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

#if defined(LISP_FEATURE_SB_THREAD) && (defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
static void
preserve_context_registers (os_context_t *c)
{
    void **ptr;
    /* On Darwin the signal context isn't a contiguous block of memory,
     * so just preserve_pointering its contents won't be sufficient.
     */
#if defined(LISP_FEATURE_DARWIN)||defined(LISP_FEATURE_WIN32)
#if defined LISP_FEATURE_X86
    preserve_pointer((void*)*os_context_register_addr(c,reg_EAX));
    preserve_pointer((void*)*os_context_register_addr(c,reg_ECX));
    preserve_pointer((void*)*os_context_register_addr(c,reg_EDX));
    preserve_pointer((void*)*os_context_register_addr(c,reg_EBX));
    preserve_pointer((void*)*os_context_register_addr(c,reg_ESI));
    preserve_pointer((void*)*os_context_register_addr(c,reg_EDI));
    preserve_pointer((void*)*os_context_pc_addr(c));
#elif defined LISP_FEATURE_X86_64
    preserve_pointer((void*)*os_context_register_addr(c,reg_RAX));
    preserve_pointer((void*)*os_context_register_addr(c,reg_RCX));
    preserve_pointer((void*)*os_context_register_addr(c,reg_RDX));
    preserve_pointer((void*)*os_context_register_addr(c,reg_RBX));
    preserve_pointer((void*)*os_context_register_addr(c,reg_RSI));
    preserve_pointer((void*)*os_context_register_addr(c,reg_RDI));
    preserve_pointer((void*)*os_context_register_addr(c,reg_R8));
    preserve_pointer((void*)*os_context_register_addr(c,reg_R9));
    preserve_pointer((void*)*os_context_register_addr(c,reg_R10));
    preserve_pointer((void*)*os_context_register_addr(c,reg_R11));
    preserve_pointer((void*)*os_context_register_addr(c,reg_R12));
    preserve_pointer((void*)*os_context_register_addr(c,reg_R13));
    preserve_pointer((void*)*os_context_register_addr(c,reg_R14));
    preserve_pointer((void*)*os_context_register_addr(c,reg_R15));
    preserve_pointer((void*)*os_context_pc_addr(c));
#else
    #error "preserve_context_registers needs to be tweaked for non-x86 Darwin"
#endif
#endif
#if !defined(LISP_FEATURE_WIN32)
    for(ptr = ((void **)(c+1))-1; ptr>=(void **)c; ptr--) {
        preserve_pointer(*ptr);
    }
#endif
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
            /* dont_move is cleared lazily, so validate the space as well. */
            page_table[i].gen == from_space) {
            if (dontmove_dwords(i) && do_wipe_p) {
                // do not move to newspace after all, this will be word-wiped
                continue;
            }
            page_table[i].gen = new_space;
            /* And since we're moving the pages wholesale, also adjust
             * the generation allocation counters. */
            generations[new_space].bytes_allocated += page_table[i].bytes_used;
            generations[from_space].bytes_allocated -= page_table[i].bytes_used;
        }
    }
}

/* Garbage collect a generation. If raise is 0 then the remains of the
 * generation are not raised to the next generation. */
static void
garbage_collect_generation(generation_index_t generation, int raise)
{
    uword_t bytes_freed;
    page_index_t i;
    uword_t static_space_size;
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
    generations[new_space].alloc_start_page = 0;
    generations[new_space].alloc_unboxed_start_page = 0;
    generations[new_space].alloc_large_start_page = 0;
    generations[new_space].alloc_large_unboxed_start_page = 0;

    /* Before any pointers are preserved, the dont_move flags on the
     * pages need to be cleared. */
    for (i = 0; i < last_free_page; i++)
        if(page_table[i].gen==from_space) {
            page_table[i].dont_move = 0;
            gc_assert(dontmove_dwords(i) == NULL);
        }

    /* Un-write-protect the old-space pages. This is essential for the
     * promoted pages as they may contain pointers into the old-space
     * which need to be scavenged. It also helps avoid unnecessary page
     * faults as forwarding pointers are written into them. They need to
     * be un-protected anyway before unmapping later. */
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

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
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
                    preserve_context_registers(th->interrupt_contexts[--k]);
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
                        preserve_context_registers(c);
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
            struct cons *list_entry =
                (struct cons *)native_pointer(pin_list);
            preserve_pointer(list_entry->car);
            pin_list = list_entry->cdr;
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

#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
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
            sword_t len= (lispobj *)get_binding_stack_pointer(th) -
                th->binding_stack_start;
            scavenge((lispobj *) th->binding_stack_start,len);
#ifdef LISP_FEATURE_SB_THREAD
            /* do the tls as well */
            len=(SymbolValue(FREE_TLS_INDEX,0) >> WORD_SHIFT) -
                (sizeof (struct thread))/(sizeof (lispobj));
            scavenge((lispobj *) (th+1),len);
#endif
        }
    }

    /* The original CMU CL code had scavenge-read-only-space code
     * controlled by the Lisp-level variable
     * *SCAVENGE-READ-ONLY-SPACE*. It was disabled by default, and it
     * wasn't documented under what circumstances it was useful or
     * safe to turn it on, so it's been turned off in SBCL. If you
     * want/need this functionality, and can test and document it,
     * please submit a patch. */
#if 0
    if (SymbolValue(SCAVENGE_READ_ONLY_SPACE) != NIL) {
        uword_t read_only_space_size =
            (lispobj*)SymbolValue(READ_ONLY_SPACE_FREE_POINTER) -
            (lispobj*)READ_ONLY_SPACE_START;
        FSHOW((stderr,
               "/scavenge read only space: %d bytes\n",
               read_only_space_size * sizeof(lispobj)));
        scavenge( (lispobj *) READ_ONLY_SPACE_START, read_only_space_size);
    }
#endif

    /* Scavenge static space. */
    static_space_size =
        (lispobj *)SymbolValue(STATIC_SPACE_FREE_POINTER,0) -
        (lispobj *)STATIC_SPACE_START;
    if (gencgc_verbose > 1) {
        FSHOW((stderr,
               "/scavenge static space: %d bytes\n",
               static_space_size * sizeof(lispobj)));
    }
    scavenge( (lispobj *) STATIC_SPACE_START, static_space_size);

    /* All generations but the generation being GCed need to be
     * scavenged. The new_space generation needs special handling as
     * objects may be moved in - it is handled separately below. */
    scavenge_generations(generation+1, PSEUDO_STATIC_GENERATION);

    scavenge_pages_with_conservative_pointers_to_them_protected_objects_only();

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
        gc_alloc_update_all_page_tables();

        bytes_allocated = bytes_allocated - old_bytes_allocated;

        if (bytes_allocated != 0) {
            lose("Rescan of new_space allocated %d more bytes.\n",
                 bytes_allocated);
        }
    }
#endif

    scan_weak_hash_tables();
    scan_weak_pointers();
    do_the_wipe();

    /* Flush the current regions, updating the tables. */
    gc_alloc_update_all_page_tables();

    /* Free the pages in oldspace, but not those marked dont_move. */
    bytes_freed = free_oldspace();

    /* If the GC is not raising the age then lower the generation back
     * to its normal generation number */
    if (!raise) {
        for (i = 0; i < last_free_page; i++)
            if ((page_table[i].bytes_used != 0)
                && (page_table[i].gen == SCRATCH_GENERATION))
                page_table[i].gen = generation;
        gc_assert(generations[generation].bytes_allocated == 0);
        generations[generation].bytes_allocated =
            generations[SCRATCH_GENERATION].bytes_allocated;
        generations[SCRATCH_GENERATION].bytes_allocated = 0;
    }

    /* Reset the alloc_start_page for generation. */
    generations[generation].alloc_start_page = 0;
    generations[generation].alloc_unboxed_start_page = 0;
    generations[generation].alloc_large_start_page = 0;
    generations[generation].alloc_large_unboxed_start_page = 0;

    if (generation >= verify_gens) {
        if (gencgc_verbose) {
            SHOW("verifying");
        }
        verify_gc();
        verify_dynamic_space();
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
        if (page_allocated_p(i) && (page_table[i].bytes_used != 0))
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
remap_free_pages (page_index_t from, page_index_t to, int forcibly)
{
    page_index_t first_page, last_page;

    if (forcibly)
        return remap_page_range(from, to);

    for (first_page = from; first_page <= to; first_page++) {
        if (page_allocated_p(first_page) ||
            (page_table[first_page].need_to_zero == 0))
            continue;

        last_page = first_page + 1;
        while (page_free_p(last_page) &&
               (last_page <= to) &&
               (page_table[last_page].need_to_zero == 1))
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
    gc_alloc_update_all_page_tables();

    /* Verify the new objects created by Lisp code. */
    if (pre_verify_gen_0) {
        FSHOW((stderr, "pre-checking generation 0\n"));
        verify_generation(0);
    }

    if (gencgc_verbose > 1)
        print_generation_stats();

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
    if ((gen_to_wp > 0) && enable_page_protection) {
        /* Check that they are all empty. */
        for (i = 0; i < gen_to_wp; i++) {
            if (generations[i].bytes_allocated)
                lose("trying to write-protect gen. %d when gen. %d nonempty\n",
                     gen_to_wp, i);
        }
        write_protect_generation_pages(gen_to_wp);
    }

    /* Set gc_alloc() back to generation 0. The current regions should
     * be flushed after the above GCs. */
    gc_assert((boxed_region.free_pointer - boxed_region.start_addr) == 0);
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

    if(gencgc_verbose)
        fprintf(stderr,"Next gc when %"OS_VM_SIZE_FMT" bytes have been consed\n",
                auto_gc_trigger);

    /* If we did a big GC (arbitrarily defined as gen > 1), release memory
     * back to the OS.
     */
    if (gen > small_generation_limit) {
        if (last_free_page > high_water_mark)
            high_water_mark = last_free_page;
        remap_free_pages(0, high_water_mark, 0);
        high_water_mark = 0;
    }

    gc_active_p = 0;
    large_allocation = 0;

    log_generation_stats(gc_logfile, "=== GC End ===");
    SHOW("returning from collect_garbage");
}

/* This is called by Lisp PURIFY when it is finished. All live objects
 * will have been moved to the RO and Static heaps. The dynamic space
 * will need a full re-initialization. We don't bother having Lisp
 * PURIFY flush the current gc_alloc() region, as the page_tables are
 * re-initialized, and every page is zeroed to be sure. */
void
gc_free_heap(void)
{
    page_index_t page, last_page;

    if (gencgc_verbose > 1) {
        SHOW("entering gc_free_heap");
    }

    for (page = 0; page < page_table_pages; page++) {
        /* Skip free pages which should already be zero filled. */
        if (page_allocated_p(page)) {
            void *page_start;
            for (last_page = page;
                 (last_page < page_table_pages) && page_allocated_p(last_page);
                 last_page++) {
                /* Mark the page free. The other slots are assumed invalid
                 * when it is a FREE_PAGE_FLAG and bytes_used is 0 and it
                 * should not be write-protected -- except that the
                 * generation is used for the current region but it sets
                 * that up. */
                page_table[page].allocated = FREE_PAGE_FLAG;
                page_table[page].bytes_used = 0;
                page_table[page].write_protected = 0;
            }

#ifndef LISP_FEATURE_WIN32 /* Pages already zeroed on win32? Not sure
                            * about this change. */
            page_start = (void *)page_address(page);
            os_protect(page_start, npage_bytes(last_page-page), OS_VM_PROT_ALL);
            remap_free_pages(page, last_page-1, 1);
            page = last_page-1;
#endif
        } else if (gencgc_zero_check_during_free_heap) {
            /* Double-check that the page is zero filled. */
            sword_t *page_start;
            page_index_t i;
            gc_assert(page_free_p(page));
            gc_assert(page_table[page].bytes_used == 0);
            page_start = (sword_t *)page_address(page);
            for (i=0; i<(long)(GENCGC_CARD_BYTES/sizeof(sword_t)); i++) {
                if (page_start[i] != 0) {
                    lose("free region not zero at %x\n", page_start + i);
                }
            }
        }
    }

    bytes_allocated = 0;

    /* Initialize the generations. */
    for (page = 0; page < NUM_GENERATIONS; page++) {
        generations[page].alloc_start_page = 0;
        generations[page].alloc_unboxed_start_page = 0;
        generations[page].alloc_large_start_page = 0;
        generations[page].alloc_large_unboxed_start_page = 0;
        generations[page].bytes_allocated = 0;
        generations[page].gc_trigger = 2000000;
        generations[page].num_gc = 0;
        generations[page].cum_sum_bytes_allocated = 0;
    }

    if (gencgc_verbose > 1)
        print_generation_stats();

    /* Initialize gc_alloc(). */
    gc_alloc_generation = 0;

    gc_set_region_empty(&boxed_region);
    gc_set_region_empty(&unboxed_region);

    last_free_page = 0;
    set_alloc_pointer((lispobj)((char *)heap_base));

    if (verify_after_free_heap) {
        /* Check whether purify has left any bad pointers. */
        FSHOW((stderr, "checking after free_heap\n"));
        verify_gc();
    }
}

void
gc_init(void)
{
    page_index_t i;

#if defined(LISP_FEATURE_SB_SAFEPOINT)
    alloc_gc_page();
#endif

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
    page_table = calloc(page_table_pages, sizeof(struct page));
    gc_assert(page_table);
    size_t total_size = sizeof(in_use_marker_t) * n_dwords_in_card *
      page_table_pages;
    /* We use mmap directly here so that we can use a minimum of
       system calls per page during GC.
       All we need here now is a madvise(DONTNEED) at the end of GC. */
    page_table_dontmove_dwords = os_validate(NULL, total_size);
    /* We do not need to zero, in fact we shouldn't.  Pages actually
       used are zeroed before use. */

    gc_assert(page_table_dontmove_dwords);
    page_table_dontmove_dwords_size_in_bytes = total_size;

    gc_init_tables();
    scavtab[WEAK_POINTER_WIDETAG] = scav_weak_pointer;
    transother[SIMPLE_ARRAY_WIDETAG] = trans_boxed_large;

    heap_base = (void*)DYNAMIC_SPACE_START;

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

    /* Initialize the generations.
     *
     * FIXME: very similar to code in gc_free_heap(), should be shared */
    for (i = 0; i < NUM_GENERATIONS; i++) {
        generations[i].alloc_start_page = 0;
        generations[i].alloc_unboxed_start_page = 0;
        generations[i].alloc_large_start_page = 0;
        generations[i].alloc_large_unboxed_start_page = 0;
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
    void *alloc_ptr = (void *)get_alloc_pointer();
    lispobj *prev=(lispobj *)page_address(page);
    generation_index_t gen = PSEUDO_STATIC_GENERATION;

    bytes_allocated = 0;

    do {
        lispobj *first,*ptr= (lispobj *)page_address(page);

        if (!gencgc_partial_pickup || page_allocated_p(page)) {
          /* It is possible, though rare, for the saved page table
           * to contain free pages below alloc_ptr. */
          page_table[page].gen = gen;
          page_table[page].bytes_used = GENCGC_CARD_BYTES;
          page_table[page].large_object = 0;
          page_table[page].write_protected = 0;
          page_table[page].write_protected_cleared = 0;
          page_table[page].dont_move = 0;
          page_table[page].need_to_zero = 1;

          bytes_allocated += GENCGC_CARD_BYTES;
        }

        if (!gencgc_partial_pickup) {
            page_table[page].allocated = BOXED_PAGE_FLAG;
            first=gc_search_space(prev,(ptr+2)-prev,ptr);
            if(ptr == first)
                prev=ptr;
            page_table[page].scan_start_offset =
                page_address(page) - (void *)prev;
        }
        page++;
    } while (page_address(page) < alloc_ptr);

    last_free_page = page;

    generations[gen].bytes_allocated = bytes_allocated;

    gc_alloc_update_all_page_tables();
    write_protect_generation_pages(gen);
}

void
gc_initialize_pointers(void)
{
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

    gc_assert(nbytes>0);

    /* Check for alignment allocation problems. */
    gc_assert((((uword_t)region->free_pointer & LOWTAG_MASK) == 0)
              && ((nbytes & LOWTAG_MASK) == 0));

#if !(defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD))
    /* Must be inside a PA section. */
    gc_assert(get_pseudo_atomic_atomic(thread));
#endif

    if (nbytes > large_allocation)
        large_allocation = nbytes;

    /* maybe we can do this quickly ... */
    new_free_pointer = region->free_pointer + nbytes;
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
    if (nbytes >= bytes_consed_between_gcs)
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
    if (BOXED_PAGE_FLAG & page_type_flag) {
#ifdef LISP_FEATURE_SB_THREAD
        struct alloc_region *region = (thread ? &(thread->alloc_region) : &boxed_region);
#else
        struct alloc_region *region = &boxed_region;
#endif
        return general_alloc_internal(nbytes, page_type_flag, region, thread);
    } else if (UNBOXED_PAGE_FLAG == page_type_flag) {
        lispobj * obj;
        gc_assert(0 == thread_mutex_lock(&allocation_lock));
        obj = general_alloc_internal(nbytes, page_type_flag, &unboxed_region, thread);
        gc_assert(0 == thread_mutex_unlock(&allocation_lock));
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
                        "  page.bytes_used: %"PAGE_BYTES_FMT"\n"
                        "  page.allocated: %d\n"
                        "  page.write_protected: %d\n"
                        "  page.write_protected_cleared: %d\n"
                        "  page.generation: %d\n",
                        fault_addr,
                        page_index,
                        boxed_region.first_page,
                        boxed_region.last_page,
                        page_table[page_index].scan_start_offset,
                        page_table[page_index].bytes_used,
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

void gc_alloc_update_all_page_tables(void)
{
    /* Flush the alloc regions updating the tables. */
    struct thread *th;
    for_each_thread(th) {
        gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->alloc_region);
#if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)
        gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->sprof_alloc_region);
#endif
    }
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

    do_wipe_p = 0;
    for (i = 0; i < last_free_page; i++) {
        page_table[i].large_object = 0;
        if (page_table[i].gen == PSEUDO_STATIC_GENERATION) {
            int used = page_table[i].bytes_used;
            page_table[i].gen = HIGHEST_NORMAL_GENERATION;
            generations[PSEUDO_STATIC_GENERATION].bytes_allocated -= used;
            generations[HIGHEST_NORMAL_GENERATION].bytes_allocated += used;
        }
    }
}


/* Do a non-conservative GC, and then save a core with the initial
 * function being set to the value of the static symbol
 * SB!VM:RESTART-LISP-FUNCTION */
void
gc_and_save(char *filename, boolean prepend_runtime,
            boolean save_runtime_options, boolean compressed,
            int compression_level, int application_type)
{
    FILE *file;
    void *runtime_bytes = NULL;
    size_t runtime_size;

    file = prepare_to_save(filename, prepend_runtime, &runtime_bytes,
                           &runtime_size);
    if (file == NULL)
       return;

    conservative_stack = 0;

    /* The filename might come from Lisp, and be moved by the now
     * non-conservative GC. */
    filename = strdup(filename);

    /* Collect twice: once into relatively high memory, and then back
     * into low memory. This compacts the retained data into the lower
     * pages, minimizing the size of the core file.
     */
    prepare_for_final_gc();
    gencgc_alloc_start_page = last_free_page;
    collect_garbage(HIGHEST_NORMAL_GENERATION+1);

    prepare_for_final_gc();
    gencgc_alloc_start_page = -1;
    collect_garbage(HIGHEST_NORMAL_GENERATION+1);

    if (prepend_runtime)
        save_runtime_to_filehandle(file, runtime_bytes, runtime_size,
                                   application_type);

    /* The dumper doesn't know that pages need to be zeroed before use. */
    zero_all_free_pages();
    save_to_filehandle(file, filename, SymbolValue(RESTART_LISP_FUNCTION,0),
                       prepend_runtime, save_runtime_options,
                       compressed ? compression_level : COMPRESSION_LEVEL_NONE);
    /* Oops. Save still managed to fail. Since we've mangled the stack
     * beyond hope, there's not much we can do.
     * (beyond FUNCALLing RESTART_LISP_FUNCTION, but I suspect that's
     * going to be rather unsatisfactory too... */
    lose("Attempt to save core after non-conservative GC failed.\n");
}
