/*
 * GENerational Conservative Garbage Collector for SBCL x86
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

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include "sbcl.h"
#include "runtime.h"
#include "os.h"
#include "interr.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"
#include "arch.h"
#include "fixnump.h"
#include "gc.h"
#include "gc-internal.h"
#include "thread.h"
#include "genesis/vector.h"
#include "genesis/weak-pointer.h"
#include "genesis/simple-fun.h"

/* assembly language stub that executes trap_PendingInterrupt */
void do_pending_interrupt(void);

/* forward declarations */
long gc_find_freeish_pages(long *restart_page_ptr, long nbytes, int unboxed);
static void  gencgc_pickup_dynamic(void);
boolean interrupt_maybe_gc_int(int, siginfo_t *, void *);


/*
 * GC parameters
 */

/* the number of actual generations. (The number of 'struct
 * generation' objects is one more than this, because one object
 * serves as scratch when GC'ing.) */
#define NUM_GENERATIONS 6

/* Should we use page protection to help avoid the scavenging of pages
 * that don't have pointers to younger generations? */
boolean enable_page_protection = 1;

/* Should we unmap a page and re-mmap it to have it zero filled? */
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
/* comment from cmucl-2.4.8: This can waste a lot of swap on FreeBSD
 * so don't unmap there.
 *
 * The CMU CL comment didn't specify a version, but was probably an
 * old version of FreeBSD (pre-4.0), so this might no longer be true.
 * OTOH, if it is true, this behavior might exist on OpenBSD too, so
 * for now we don't unmap there either. -- WHN 2001-04-07 */
boolean gencgc_unmap_zero = 0;
#else
boolean gencgc_unmap_zero = 1;
#endif

/* the minimum size (in bytes) for a large object*/
unsigned large_object_size = 4 * PAGE_BYTES;


/*
 * debugging
 */



/* the verbosity level. All non-error messages are disabled at level 0;
 * and only a few rare messages are printed at level 1. */
#ifdef QSHOW
unsigned gencgc_verbose = 1;
#else
unsigned gencgc_verbose = 0;
#endif

/* FIXME: At some point enable the various error-checking things below
 * and see what they say. */

/* We hunt for pointers to old-space, when GCing generations >= verify_gen.
 * Set verify_gens to NUM_GENERATIONS to disable this kind of check. */
int verify_gens = NUM_GENERATIONS;

/* Should we do a pre-scan verify of generation 0 before it's GCed? */
boolean pre_verify_gen_0 = 0;

/* Should we check for bad pointers after gc_free_heap is called
 * from Lisp PURIFY? */
boolean verify_after_free_heap = 0;

/* Should we print a note when code objects are found in the dynamic space
 * during a heap verify? */
boolean verify_dynamic_code_check = 0;

/* Should we check code objects for fixup errors after they are transported? */
boolean check_code_fixups = 0;

/* Should we check that newly allocated regions are zero filled? */
boolean gencgc_zero_check = 0;

/* Should we check that the free space is zero filled? */
boolean gencgc_enable_verify_zero_fill = 0;

/* Should we check that free pages are zero filled during gc_free_heap
 * called after Lisp PURIFY? */
boolean gencgc_zero_check_during_free_heap = 0;

/*
 * GC structures and variables
 */

/* the total bytes allocated. These are seen by Lisp DYNAMIC-USAGE. */
unsigned long bytes_allocated = 0;
extern unsigned long bytes_consed_between_gcs; /* gc-common.c */
unsigned long auto_gc_trigger = 0;

/* the source and destination generations. These are set before a GC starts
 * scavenging. */
long from_space;
long new_space;


/* An array of page structures is statically allocated.
 * This helps quickly map between an address its page structure.
 * NUM_PAGES is set from the size of the dynamic space. */
struct page page_table[NUM_PAGES];

/* To map addresses to page structures the address of the first page
 * is needed. */
static void *heap_base = NULL;

#if N_WORD_BITS == 32
 #define SIMPLE_ARRAY_WORD_WIDETAG SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG
#elif N_WORD_BITS == 64
 #define SIMPLE_ARRAY_WORD_WIDETAG SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG
#endif

/* Calculate the start address for the given page number. */
inline void *
page_address(long page_num)
{
    return (heap_base + (page_num * PAGE_BYTES));
}

/* Find the page index within the page_table for the given
 * address. Return -1 on failure. */
inline long
find_page_index(void *addr)
{
    long index = addr-heap_base;

    if (index >= 0) {
	index = ((unsigned long)index)/PAGE_BYTES;
	if (index < NUM_PAGES)
	    return (index);
    }

    return (-1);
}

/* a structure to hold the state of a generation */
struct generation {

    /* the first page that gc_alloc() checks on its next call */
    long alloc_start_page;

    /* the first page that gc_alloc_unboxed() checks on its next call */
    long alloc_unboxed_start_page;

    /* the first page that gc_alloc_large (boxed) considers on its next
     * call. (Although it always allocates after the boxed_region.) */
    long alloc_large_start_page;

    /* the first page that gc_alloc_large (unboxed) considers on its
     * next call. (Although it always allocates after the
     * current_unboxed_region.) */
    long alloc_large_unboxed_start_page;

    /* the bytes allocated to this generation */
    long bytes_allocated;

    /* the number of bytes at which to trigger a GC */
    long gc_trigger;

    /* to calculate a new level for gc_trigger */
    long bytes_consed_between_gc;

    /* the number of GCs since the last raise */
    int num_gc;

    /* the average age after which a GC will raise objects to the
     * next generation */
    int trigger_age;

    /* the cumulative sum of the bytes allocated to this generation. It is
     * cleared after a GC on this generations, and update before new
     * objects are added from a GC of a younger generation. Dividing by
     * the bytes_allocated will give the average age of the memory in
     * this generation since its last GC. */
    long cum_sum_bytes_allocated;

    /* a minimum average memory age before a GC will occur helps
     * prevent a GC when a large number of new live objects have been
     * added, in which case a GC could be a waste of time */
    double min_av_mem_age;
};
/* the number of actual generations. (The number of 'struct
 * generation' objects is one more than this, because one object
 * serves as scratch when GC'ing.) */
#define NUM_GENERATIONS 6

/* an array of generation structures. There needs to be one more
 * generation structure than actual generations as the oldest
 * generation is temporarily raised then lowered. */
struct generation generations[NUM_GENERATIONS+1];

/* the oldest generation that is will currently be GCed by default.
 * Valid values are: 0, 1, ... (NUM_GENERATIONS-1)
 *
 * The default of (NUM_GENERATIONS-1) enables GC on all generations.
 *
 * Setting this to 0 effectively disables the generational nature of
 * the GC. In some applications generational GC may not be useful
 * because there are no long-lived objects.
 *
 * An intermediate value could be handy after moving long-lived data
 * into an older generation so an unnecessary GC of this long-lived
 * data can be avoided. */
unsigned int  gencgc_oldest_gen_to_gc = NUM_GENERATIONS-1;

/* The maximum free page in the heap is maintained and used to update
 * ALLOCATION_POINTER which is used by the room function to limit its
 * search of the heap. XX Gencgc obviously needs to be better
 * integrated with the Lisp code. */
static long  last_free_page;

/* This lock is to prevent multiple threads from simultaneously
 * allocating new regions which overlap each other.  Note that the
 * majority of GC is single-threaded, but alloc() may be called from
 * >1 thread at a time and must be thread-safe.  This lock must be
 * seized before all accesses to generations[] or to parts of
 * page_table[] that other threads may want to see */

static lispobj free_pages_lock=0;


/*
 * miscellaneous heap functions
 */

/* Count the number of pages which are write-protected within the
 * given generation. */
static long
count_write_protect_generation_pages(int generation)
{
    long i;
    long count = 0;

    for (i = 0; i < last_free_page; i++)
	if ((page_table[i].allocated != FREE_PAGE_FLAG)
	    && (page_table[i].gen == generation)
	    && (page_table[i].write_protected == 1))
	    count++;
    return count;
}

/* Count the number of pages within the given generation. */
static long
count_generation_pages(int generation)
{
    long i;
    long count = 0;

    for (i = 0; i < last_free_page; i++)
	if ((page_table[i].allocated != 0)
	    && (page_table[i].gen == generation))
	    count++;
    return count;
}

#ifdef QSHOW
static long
count_dont_move_pages(void)
{
    long i;
    long count = 0;
    for (i = 0; i < last_free_page; i++) {
	if ((page_table[i].allocated != 0) && (page_table[i].dont_move != 0)) {
	    ++count;
	}
    }
    return count;
}
#endif /* QSHOW */

/* Work through the pages and add up the number of bytes used for the
 * given generation. */
static long
count_generation_bytes_allocated (int gen)
{
    long i;
    long result = 0;
    for (i = 0; i < last_free_page; i++) {
	if ((page_table[i].allocated != 0) && (page_table[i].gen == gen))
	    result += page_table[i].bytes_used;
    }
    return result;
}

/* Return the average age of the memory in a generation. */
static double
gen_av_mem_age(int gen)
{
    if (generations[gen].bytes_allocated == 0)
	return 0.0;

    return
	((double)generations[gen].cum_sum_bytes_allocated)
	/ ((double)generations[gen].bytes_allocated);
}

void fpu_save(int *);		/* defined in x86-assem.S */
void fpu_restore(int *);	/* defined in x86-assem.S */
/* The verbose argument controls how much to print: 0 for normal
 * level of detail; 1 for debugging. */
static void
print_generation_stats(int verbose) /* FIXME: should take FILE argument */
{
    int i, gens;
    int fpu_state[27];

    /* This code uses the FP instructions which may be set up for Lisp
     * so they need to be saved and reset for C. */
    fpu_save(fpu_state);

    /* number of generations to print */
    if (verbose)
	gens = NUM_GENERATIONS+1;
    else
	gens = NUM_GENERATIONS;

    /* Print the heap stats. */
    fprintf(stderr,
	    "   Gen Boxed Unboxed LB   LUB  !move  Alloc  Waste   Trig    WP  GCs Mem-age\n");

    for (i = 0; i < gens; i++) {
	int j;
	int boxed_cnt = 0;
	int unboxed_cnt = 0;
	int large_boxed_cnt = 0;
	int large_unboxed_cnt = 0;
	int pinned_cnt=0;

	for (j = 0; j < last_free_page; j++)
	    if (page_table[j].gen == i) {

		/* Count the number of boxed pages within the given
		 * generation. */
		if (page_table[j].allocated & BOXED_PAGE_FLAG) {
		    if (page_table[j].large_object)
			large_boxed_cnt++;
		    else
			boxed_cnt++;
		}
		if(page_table[j].dont_move) pinned_cnt++;
		/* Count the number of unboxed pages within the given
		 * generation. */
		if (page_table[j].allocated & UNBOXED_PAGE_FLAG) {
		    if (page_table[j].large_object)
			large_unboxed_cnt++;
		    else
			unboxed_cnt++;
		}
	    }

	gc_assert(generations[i].bytes_allocated
		  == count_generation_bytes_allocated(i));
	fprintf(stderr,
		"   %1d: %5d %5d %5d %5d %5d %8d %5d %8d %4d %3d %7.4f\n",
		i,
		boxed_cnt, unboxed_cnt, large_boxed_cnt, large_unboxed_cnt,
		pinned_cnt,
		generations[i].bytes_allocated,
		(count_generation_pages(i)*PAGE_BYTES
		 - generations[i].bytes_allocated),
		generations[i].gc_trigger,
		count_write_protect_generation_pages(i),
		generations[i].num_gc,
		gen_av_mem_age(i));
    }
    fprintf(stderr,"   Total bytes allocated=%ld\n", bytes_allocated);

    fpu_restore(fpu_state);
}

/*
 * allocation routines
 */

/*
 * To support quick and inline allocation, regions of memory can be
 * allocated and then allocated from with just a free pointer and a
 * check against an end address.
 *
 * Since objects can be allocated to spaces with different properties
 * e.g. boxed/unboxed, generation, ages; there may need to be many
 * allocation regions.
 *
 * Each allocation region may be start within a partly used page. Many
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
static int gc_alloc_generation;

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
gc_alloc_new_region(long nbytes, int unboxed, struct alloc_region *alloc_region)
{
    long first_page;
    long last_page;
    long bytes_found;
    long i;

    /*
    FSHOW((stderr,
	   "/alloc_new_region for %d bytes from gen %d\n",
	   nbytes, gc_alloc_generation));
    */

    /* Check that the region is in a reset state. */
    gc_assert((alloc_region->first_page == 0)
	      && (alloc_region->last_page == -1)
	      && (alloc_region->free_pointer == alloc_region->end_addr));
    get_spinlock(&free_pages_lock,(long) alloc_region);
    if (unboxed) {
	first_page =
	    generations[gc_alloc_generation].alloc_unboxed_start_page;
    } else {
	first_page =
	    generations[gc_alloc_generation].alloc_start_page;
    }
    last_page=gc_find_freeish_pages(&first_page,nbytes,unboxed);
    bytes_found=(PAGE_BYTES - page_table[first_page].bytes_used)
	    + PAGE_BYTES*(last_page-first_page);

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
	if (unboxed)
	    page_table[first_page].allocated = UNBOXED_PAGE_FLAG;
	else
	    page_table[first_page].allocated = BOXED_PAGE_FLAG;
	page_table[first_page].gen = gc_alloc_generation;
	page_table[first_page].large_object = 0;
	page_table[first_page].first_object_offset = 0;
    }

    if (unboxed)
	gc_assert(page_table[first_page].allocated == UNBOXED_PAGE_FLAG);
    else
	gc_assert(page_table[first_page].allocated == BOXED_PAGE_FLAG);
    page_table[first_page].allocated |= OPEN_REGION_PAGE_FLAG; 

    gc_assert(page_table[first_page].gen == gc_alloc_generation);
    gc_assert(page_table[first_page].large_object == 0);

    for (i = first_page+1; i <= last_page; i++) {
	if (unboxed)
	    page_table[i].allocated = UNBOXED_PAGE_FLAG;
	else
	    page_table[i].allocated = BOXED_PAGE_FLAG;
	page_table[i].gen = gc_alloc_generation;
	page_table[i].large_object = 0;
	/* This may not be necessary for unboxed regions (think it was
	 * broken before!) */
	page_table[i].first_object_offset =
	    alloc_region->start_addr - page_address(i);
	page_table[i].allocated |= OPEN_REGION_PAGE_FLAG ;
    }
    /* Bump up last_free_page. */
    if (last_page+1 > last_free_page) {
	last_free_page = last_page+1;
	SetSymbolValue(ALLOCATION_POINTER,
		       (lispobj)(((char *)heap_base) + last_free_page*PAGE_BYTES),
		       0);
    }
    release_spinlock(&free_pages_lock);
    
    /* we can do this after releasing free_pages_lock */
    if (gencgc_zero_check) {
	long *p;
	for (p = (long *)alloc_region->start_addr;
	     p < (long *)alloc_region->end_addr; p++) {
	    if (*p != 0) {
		/* KLUDGE: It would be nice to use %lx and explicit casts
		 * (long) in code like this, so that it is less likely to
		 * break randomly when running on a machine with different
		 * word sizes. -- WHN 19991129 */
		lose("The new region at %x is not zero.", p);
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
static long new_areas_ignore_page;
struct new_area {
    long  page;
    long  offset;
    long  size;
};
static struct new_area (*new_areas)[];
static long new_areas_index;
long max_new_areas;

/* Add a new area to new_areas. */
static void
add_new_area(long first_page, long offset, long size)
{
    unsigned new_area_start,c;
    long i;

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

    new_area_start = PAGE_BYTES*first_page + offset;

    /* Search backwards for a prior area that this follows from. If
       found this will save adding a new area. */
    for (i = new_areas_index-1, c = 0; (i >= 0) && (c < 8); i--, c++) {
	unsigned area_end =
	    PAGE_BYTES*((*new_areas)[i].page)
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
gc_alloc_update_page_tables(int unboxed, struct alloc_region *alloc_region)
{
    long more;
    long first_page;
    long next_page;
    long bytes_used;
    long orig_first_page_bytes_used;
    long region_size;
    long byte_cnt;


    first_page = alloc_region->first_page;

    /* Catch an unused alloc_region. */
    if ((first_page == 0) && (alloc_region->last_page == -1))
	return;

    next_page = first_page+1;

    get_spinlock(&free_pages_lock,(long) alloc_region);
    if (alloc_region->free_pointer != alloc_region->start_addr) {
	/* some bytes were allocated in the region */
	orig_first_page_bytes_used = page_table[first_page].bytes_used;

	gc_assert(alloc_region->start_addr == (page_address(first_page) + page_table[first_page].bytes_used));

	/* All the pages used need to be updated */

	/* Update the first page. */

	/* If the page was free then set up the gen, and
	 * first_object_offset. */
	if (page_table[first_page].bytes_used == 0)
	    gc_assert(page_table[first_page].first_object_offset == 0);
	page_table[first_page].allocated &= ~(OPEN_REGION_PAGE_FLAG);

	if (unboxed)
	    gc_assert(page_table[first_page].allocated == UNBOXED_PAGE_FLAG);
	else
	    gc_assert(page_table[first_page].allocated == BOXED_PAGE_FLAG);
	gc_assert(page_table[first_page].gen == gc_alloc_generation);
	gc_assert(page_table[first_page].large_object == 0);

	byte_cnt = 0;

	/* Calculate the number of bytes used in this page. This is not
	 * always the number of new bytes, unless it was free. */
	more = 0;
	if ((bytes_used = (alloc_region->free_pointer - page_address(first_page)))>PAGE_BYTES) {
	    bytes_used = PAGE_BYTES;
	    more = 1;
	}
	page_table[first_page].bytes_used = bytes_used;
	byte_cnt += bytes_used;


	/* All the rest of the pages should be free. We need to set their
	 * first_object_offset pointer to the start of the region, and set
	 * the bytes_used. */
	while (more) {
	    page_table[next_page].allocated &= ~(OPEN_REGION_PAGE_FLAG);
	    if (unboxed)
		gc_assert(page_table[next_page].allocated==UNBOXED_PAGE_FLAG);
	    else
		gc_assert(page_table[next_page].allocated == BOXED_PAGE_FLAG);
	    gc_assert(page_table[next_page].bytes_used == 0);
	    gc_assert(page_table[next_page].gen == gc_alloc_generation);
	    gc_assert(page_table[next_page].large_object == 0);

	    gc_assert(page_table[next_page].first_object_offset ==
		      alloc_region->start_addr - page_address(next_page));

	    /* Calculate the number of bytes used in this page. */
	    more = 0;
	    if ((bytes_used = (alloc_region->free_pointer
			       - page_address(next_page)))>PAGE_BYTES) {
		bytes_used = PAGE_BYTES;
		more = 1;
	    }
	    page_table[next_page].bytes_used = bytes_used;
	    byte_cnt += bytes_used;

	    next_page++;
	}

	region_size = alloc_region->free_pointer - alloc_region->start_addr;
	bytes_allocated += region_size;
	generations[gc_alloc_generation].bytes_allocated += region_size;

	gc_assert((byte_cnt- orig_first_page_bytes_used) == region_size);

	/* Set the generations alloc restart page to the last page of
	 * the region. */
	if (unboxed)
	    generations[gc_alloc_generation].alloc_unboxed_start_page =
		next_page-1;
	else
	    generations[gc_alloc_generation].alloc_start_page = next_page-1;

	/* Add the region to the new_areas if requested. */
	if (!unboxed)
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
    release_spinlock(&free_pages_lock);
    /* alloc_region is per-thread, we're ok to do this unlocked */
    gc_set_region_empty(alloc_region);
}

static inline void *gc_quick_alloc(long nbytes);

/* Allocate a possibly large object. */
void *
gc_alloc_large(long nbytes, int unboxed, struct alloc_region *alloc_region)
{
    long first_page;
    long last_page;
    long orig_first_page_bytes_used;
    long byte_cnt;
    long more;
    long bytes_used;
    long next_page;

    get_spinlock(&free_pages_lock,(long) alloc_region);

    if (unboxed) {
	first_page =
	    generations[gc_alloc_generation].alloc_large_unboxed_start_page;
    } else {
	first_page = generations[gc_alloc_generation].alloc_large_start_page;
    }
    if (first_page <= alloc_region->last_page) {
	first_page = alloc_region->last_page+1;
    }

    last_page=gc_find_freeish_pages(&first_page,nbytes,unboxed);

    gc_assert(first_page > alloc_region->last_page);
    if (unboxed)
	generations[gc_alloc_generation].alloc_large_unboxed_start_page =
	    last_page;
    else
	generations[gc_alloc_generation].alloc_large_start_page = last_page;

    /* Set up the pages. */
    orig_first_page_bytes_used = page_table[first_page].bytes_used;

    /* If the first page was free then set up the gen, and
     * first_object_offset. */
    if (page_table[first_page].bytes_used == 0) {
	if (unboxed)
	    page_table[first_page].allocated = UNBOXED_PAGE_FLAG;
	else
	    page_table[first_page].allocated = BOXED_PAGE_FLAG;
	page_table[first_page].gen = gc_alloc_generation;
	page_table[first_page].first_object_offset = 0;
	page_table[first_page].large_object = 1;
    }

    if (unboxed)
	gc_assert(page_table[first_page].allocated == UNBOXED_PAGE_FLAG);
    else
	gc_assert(page_table[first_page].allocated == BOXED_PAGE_FLAG);
    gc_assert(page_table[first_page].gen == gc_alloc_generation);
    gc_assert(page_table[first_page].large_object == 1);

    byte_cnt = 0;

    /* Calc. the number of bytes used in this page. This is not
     * always the number of new bytes, unless it was free. */
    more = 0;
    if ((bytes_used = nbytes+orig_first_page_bytes_used) > PAGE_BYTES) {
	bytes_used = PAGE_BYTES;
	more = 1;
    }
    page_table[first_page].bytes_used = bytes_used;
    byte_cnt += bytes_used;

    next_page = first_page+1;

    /* All the rest of the pages should be free. We need to set their
     * first_object_offset pointer to the start of the region, and
     * set the bytes_used. */
    while (more) {
	gc_assert(page_table[next_page].allocated == FREE_PAGE_FLAG);
	gc_assert(page_table[next_page].bytes_used == 0);
	if (unboxed)
	    page_table[next_page].allocated = UNBOXED_PAGE_FLAG;
	else
	    page_table[next_page].allocated = BOXED_PAGE_FLAG;
	page_table[next_page].gen = gc_alloc_generation;
	page_table[next_page].large_object = 1;

	page_table[next_page].first_object_offset =
	    orig_first_page_bytes_used - PAGE_BYTES*(next_page-first_page);

	/* Calculate the number of bytes used in this page. */
	more = 0;
	if ((bytes_used=(nbytes+orig_first_page_bytes_used)-byte_cnt) > PAGE_BYTES) {
	    bytes_used = PAGE_BYTES;
	    more = 1;
	}
	page_table[next_page].bytes_used = bytes_used;
	page_table[next_page].write_protected=0;
	page_table[next_page].dont_move=0;
	byte_cnt += bytes_used;
	next_page++;
    }

    gc_assert((byte_cnt-orig_first_page_bytes_used) == nbytes);

    bytes_allocated += nbytes;
    generations[gc_alloc_generation].bytes_allocated += nbytes;

    /* Add the region to the new_areas if requested. */
    if (!unboxed)
	add_new_area(first_page,orig_first_page_bytes_used,nbytes);

    /* Bump up last_free_page */
    if (last_page+1 > last_free_page) {
	last_free_page = last_page+1;
	SetSymbolValue(ALLOCATION_POINTER,
		       (lispobj)(((char *)heap_base) + last_free_page*PAGE_BYTES),0);
    }
    release_spinlock(&free_pages_lock);

    return((void *)(page_address(first_page)+orig_first_page_bytes_used));
}

long
gc_find_freeish_pages(long *restart_page_ptr, long nbytes, int unboxed)
{
    long first_page;
    long last_page;
    long region_size;
    long restart_page=*restart_page_ptr;
    long bytes_found;
    long num_pages;
    long large_p=(nbytes>=large_object_size);
    gc_assert(free_pages_lock);

    /* Search for a contiguous free space of at least nbytes. If it's
     * a large object then align it on a page boundary by searching
     * for a free page. */

    do {
	first_page = restart_page;
	if (large_p)		
	    while ((first_page < NUM_PAGES)
		   && (page_table[first_page].allocated != FREE_PAGE_FLAG))
		first_page++;
	else
	    while (first_page < NUM_PAGES) {
		if(page_table[first_page].allocated == FREE_PAGE_FLAG)
		    break;
		if((page_table[first_page].allocated ==
		    (unboxed ? UNBOXED_PAGE_FLAG : BOXED_PAGE_FLAG)) &&
		   (page_table[first_page].large_object == 0) &&
		   (page_table[first_page].gen == gc_alloc_generation) &&
		   (page_table[first_page].bytes_used < (PAGE_BYTES-32)) &&
		   (page_table[first_page].write_protected == 0) &&
		   (page_table[first_page].dont_move == 0)) {
		    break;
		}
		first_page++;
	    }
	
	if (first_page >= NUM_PAGES) {
	    fprintf(stderr,
		    "Argh! gc_find_free_space failed (first_page), nbytes=%d.\n",
		    nbytes);
	    print_generation_stats(1);
	    lose(NULL);
	}

	gc_assert(page_table[first_page].write_protected == 0);

	last_page = first_page;
	bytes_found = PAGE_BYTES - page_table[first_page].bytes_used;
	num_pages = 1;
	while (((bytes_found < nbytes) 
		|| (!large_p && (num_pages < 2)))
	       && (last_page < (NUM_PAGES-1))
	       && (page_table[last_page+1].allocated == FREE_PAGE_FLAG)) {
	    last_page++;
	    num_pages++;
	    bytes_found += PAGE_BYTES;
	    gc_assert(page_table[last_page].write_protected == 0);
	}

	region_size = (PAGE_BYTES - page_table[first_page].bytes_used)
	    + PAGE_BYTES*(last_page-first_page);

	gc_assert(bytes_found == region_size);
	restart_page = last_page + 1;
    } while ((restart_page < NUM_PAGES) && (bytes_found < nbytes));

    /* Check for a failure */
    if ((restart_page >= NUM_PAGES) && (bytes_found < nbytes)) {
	fprintf(stderr,
		"Argh! gc_find_freeish_pages failed (restart_page), nbytes=%d.\n",
		nbytes);
	print_generation_stats(1);
	lose(NULL);
    }
    *restart_page_ptr=first_page;
    return last_page;
}

/* Allocate bytes.  All the rest of the special-purpose allocation
 * functions will eventually call this  */

void *
gc_alloc_with_region(long nbytes,int unboxed_p, struct alloc_region *my_region,
		     int quick_p)
{
    void *new_free_pointer;

    if(nbytes>=large_object_size)
	return gc_alloc_large(nbytes,unboxed_p,my_region);

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
	    (my_region->end_addr - my_region->free_pointer) <= 32) {
	    /* If so, finished with the current region. */
	    gc_alloc_update_page_tables(unboxed_p, my_region);
	    /* Set up a new region. */
	    gc_alloc_new_region(32 /*bytes*/, unboxed_p, my_region);
	}

	return((void *)new_obj);
    }

    /* Else not enough free space in the current region: retry with a
     * new region. */

    gc_alloc_update_page_tables(unboxed_p, my_region);
    gc_alloc_new_region(nbytes, unboxed_p, my_region);
    return gc_alloc_with_region(nbytes,unboxed_p,my_region,0);
}

/* these are only used during GC: all allocation from the mutator calls
 * alloc() -> gc_alloc_with_region() with the appropriate per-thread 
 * region */

void *
gc_general_alloc(long nbytes,int unboxed_p,int quick_p)
{
    struct alloc_region *my_region = 
      unboxed_p ? &unboxed_region : &boxed_region;
    return gc_alloc_with_region(nbytes,unboxed_p, my_region,quick_p);
}

static inline void *
gc_quick_alloc(long nbytes)
{
    return gc_general_alloc(nbytes,ALLOC_BOXED,ALLOC_QUICK);
}

static inline void *
gc_quick_alloc_large(long nbytes)
{
    return gc_general_alloc(nbytes,ALLOC_BOXED,ALLOC_QUICK);
}

static inline void *
gc_alloc_unboxed(long nbytes)
{
    return gc_general_alloc(nbytes,ALLOC_UNBOXED,0);
}

static inline void *
gc_quick_alloc_unboxed(long nbytes)
{
    return gc_general_alloc(nbytes,ALLOC_UNBOXED,ALLOC_QUICK);
}

static inline void *
gc_quick_alloc_large_unboxed(long nbytes)
{
    return gc_general_alloc(nbytes,ALLOC_UNBOXED,ALLOC_QUICK);
}

/*
 * scavenging/transporting routines derived from gc.c in CMU CL ca. 18b
 */

extern long (*scavtab[256])(lispobj *where, lispobj object);
extern lispobj (*transother[256])(lispobj object);
extern long (*sizetab[256])(lispobj *where);

/* Copy a large boxed object. If the object is in a large object
 * region then it is simply promoted, else it is copied. If it's large
 * enough then it's copied to a large object region.
 *
 * Vectors may have shrunk. If the object is not copied the space
 * needs to be reclaimed, and the page_tables corrected. */
lispobj
copy_large_object(lispobj object, long nwords)
{
    int tag;
    lispobj *new;
    long first_page;

    gc_assert(is_lisp_pointer(object));
    gc_assert(from_space_p(object));
    gc_assert((nwords & 0x01) == 0);


    /* Check whether it's in a large object region. */
    first_page = find_page_index((void *)object);
    gc_assert(first_page >= 0);

    if (page_table[first_page].large_object) {

	/* Promote the object. */

	long remaining_bytes;
	long next_page;
	long bytes_freed;
	long old_bytes_used;

	/* Note: Any page write-protection must be removed, else a
	 * later scavenge_newspace may incorrectly not scavenge these
	 * pages. This would not be necessary if they are added to the
	 * new areas, but let's do it for them all (they'll probably
	 * be written anyway?). */

	gc_assert(page_table[first_page].first_object_offset == 0);

	next_page = first_page;
	remaining_bytes = nwords*N_WORD_BYTES;
	while (remaining_bytes > PAGE_BYTES) {
	    gc_assert(page_table[next_page].gen == from_space);
	    gc_assert(page_table[next_page].allocated == BOXED_PAGE_FLAG);
	    gc_assert(page_table[next_page].large_object);
	    gc_assert(page_table[next_page].first_object_offset==
		      -PAGE_BYTES*(next_page-first_page));
	    gc_assert(page_table[next_page].bytes_used == PAGE_BYTES);

	    page_table[next_page].gen = new_space;

	    /* Remove any write-protection. We should be able to rely
	     * on the write-protect flag to avoid redundant calls. */
	    if (page_table[next_page].write_protected) {
		os_protect(page_address(next_page), PAGE_BYTES, OS_VM_PROT_ALL);
		page_table[next_page].write_protected = 0;
	    }
	    remaining_bytes -= PAGE_BYTES;
	    next_page++;
	}

	/* Now only one page remains, but the object may have shrunk
	 * so there may be more unused pages which will be freed. */

	/* The object may have shrunk but shouldn't have grown. */
	gc_assert(page_table[next_page].bytes_used >= remaining_bytes);

	page_table[next_page].gen = new_space;
	gc_assert(page_table[next_page].allocated == BOXED_PAGE_FLAG);

	/* Adjust the bytes_used. */
	old_bytes_used = page_table[next_page].bytes_used;
	page_table[next_page].bytes_used = remaining_bytes;

	bytes_freed = old_bytes_used - remaining_bytes;

	/* Free any remaining pages; needs care. */
	next_page++;
	while ((old_bytes_used == PAGE_BYTES) &&
	       (page_table[next_page].gen == from_space) &&
	       (page_table[next_page].allocated == BOXED_PAGE_FLAG) &&
	       page_table[next_page].large_object &&
	       (page_table[next_page].first_object_offset ==
		-(next_page - first_page)*PAGE_BYTES)) {
	    /* Checks out OK, free the page. Don't need to bother zeroing
	     * pages as this should have been done before shrinking the
	     * object. These pages shouldn't be write-protected as they
	     * should be zero filled. */
	    gc_assert(page_table[next_page].write_protected == 0);

	    old_bytes_used = page_table[next_page].bytes_used;
	    page_table[next_page].allocated = FREE_PAGE_FLAG;
	    page_table[next_page].bytes_used = 0;
	    bytes_freed += old_bytes_used;
	    next_page++;
	}

	generations[from_space].bytes_allocated -= N_WORD_BYTES*nwords +
	  bytes_freed;
	generations[new_space].bytes_allocated += N_WORD_BYTES*nwords;
	bytes_allocated -= bytes_freed;

	/* Add the region to the new_areas if requested. */
	add_new_area(first_page,0,nwords*N_WORD_BYTES);

	return(object);
    } else {
	/* Get tag of object. */
	tag = lowtag_of(object);

	/* Allocate space. */
	new = gc_quick_alloc_large(nwords*N_WORD_BYTES);

	memcpy(new,native_pointer(object),nwords*N_WORD_BYTES);

	/* Return Lisp pointer of new object. */
	return ((lispobj) new) | tag;
    }
}

/* to copy unboxed objects */
lispobj
copy_unboxed_object(lispobj object, long nwords)
{
    long tag;
    lispobj *new;

    gc_assert(is_lisp_pointer(object));
    gc_assert(from_space_p(object));
    gc_assert((nwords & 0x01) == 0);

    /* Get tag of object. */
    tag = lowtag_of(object);

    /* Allocate space. */
    new = gc_quick_alloc_unboxed(nwords*N_WORD_BYTES);

    memcpy(new,native_pointer(object),nwords*N_WORD_BYTES);

    /* Return Lisp pointer of new object. */
    return ((lispobj) new) | tag;
}

/* to copy large unboxed objects
 *
 * If the object is in a large object region then it is simply
 * promoted, else it is copied. If it's large enough then it's copied
 * to a large object region.
 *
 * Bignums and vectors may have shrunk. If the object is not copied
 * the space needs to be reclaimed, and the page_tables corrected.
 *
 * KLUDGE: There's a lot of cut-and-paste duplication between this
 * function and copy_large_object(..). -- WHN 20000619 */
lispobj
copy_large_unboxed_object(lispobj object, long nwords)
{
    int tag;
    lispobj *new;
    long first_page;

    gc_assert(is_lisp_pointer(object));
    gc_assert(from_space_p(object));
    gc_assert((nwords & 0x01) == 0);

    if ((nwords > 1024*1024) && gencgc_verbose)
	FSHOW((stderr, "/copy_large_unboxed_object: %d bytes\n", nwords*N_WORD_BYTES));

    /* Check whether it's a large object. */
    first_page = find_page_index((void *)object);
    gc_assert(first_page >= 0);

    if (page_table[first_page].large_object) {
	/* Promote the object. Note: Unboxed objects may have been
	 * allocated to a BOXED region so it may be necessary to
	 * change the region to UNBOXED. */
	long remaining_bytes;
	long next_page;
	long bytes_freed;
	long old_bytes_used;

	gc_assert(page_table[first_page].first_object_offset == 0);

	next_page = first_page;
	remaining_bytes = nwords*N_WORD_BYTES;
	while (remaining_bytes > PAGE_BYTES) {
	    gc_assert(page_table[next_page].gen == from_space);
	    gc_assert((page_table[next_page].allocated == UNBOXED_PAGE_FLAG)
		      || (page_table[next_page].allocated == BOXED_PAGE_FLAG));
	    gc_assert(page_table[next_page].large_object);
	    gc_assert(page_table[next_page].first_object_offset==
		      -PAGE_BYTES*(next_page-first_page));
	    gc_assert(page_table[next_page].bytes_used == PAGE_BYTES);

	    page_table[next_page].gen = new_space;
	    page_table[next_page].allocated = UNBOXED_PAGE_FLAG;
	    remaining_bytes -= PAGE_BYTES;
	    next_page++;
	}

	/* Now only one page remains, but the object may have shrunk so
	 * there may be more unused pages which will be freed. */

	/* Object may have shrunk but shouldn't have grown - check. */
	gc_assert(page_table[next_page].bytes_used >= remaining_bytes);

	page_table[next_page].gen = new_space;
	page_table[next_page].allocated = UNBOXED_PAGE_FLAG;

	/* Adjust the bytes_used. */
	old_bytes_used = page_table[next_page].bytes_used;
	page_table[next_page].bytes_used = remaining_bytes;

	bytes_freed = old_bytes_used - remaining_bytes;

	/* Free any remaining pages; needs care. */
	next_page++;
	while ((old_bytes_used == PAGE_BYTES) &&
	       (page_table[next_page].gen == from_space) &&
	       ((page_table[next_page].allocated == UNBOXED_PAGE_FLAG)
		|| (page_table[next_page].allocated == BOXED_PAGE_FLAG)) &&
	       page_table[next_page].large_object &&
	       (page_table[next_page].first_object_offset ==
		-(next_page - first_page)*PAGE_BYTES)) {
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

	if ((bytes_freed > 0) && gencgc_verbose)
	    FSHOW((stderr,
		   "/copy_large_unboxed bytes_freed=%d\n",
		   bytes_freed));

	generations[from_space].bytes_allocated -= nwords*N_WORD_BYTES + bytes_freed;
	generations[new_space].bytes_allocated += nwords*N_WORD_BYTES;
	bytes_allocated -= bytes_freed;

	return(object);
    }
    else {
	/* Get tag of object. */
	tag = lowtag_of(object);

	/* Allocate space. */
	new = gc_quick_alloc_large_unboxed(nwords*N_WORD_BYTES);

        /* Copy the object. */
        memcpy(new,native_pointer(object),nwords*N_WORD_BYTES);

	/* Return Lisp pointer of new object. */
	return ((lispobj) new) | tag;
    }
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
void
sniff_code_object(struct code *code, unsigned displacement)
{
    long nheader_words, ncode_words, nwords;
    void *p;
    void *constants_start_addr, *constants_end_addr;
    void *code_start_addr, *code_end_addr;
    int fixup_found = 0;

    if (!check_code_fixups)
	return;

    ncode_words = fixnum_value(code->code_size);
    nheader_words = HeaderValue(*(lispobj *)code);
    nwords = ncode_words + nheader_words;

    constants_start_addr = (void *)code + 5*N_WORD_BYTES;
    constants_end_addr = (void *)code + nheader_words*N_WORD_BYTES;
    code_start_addr = (void *)code + nheader_words*N_WORD_BYTES;
    code_end_addr = (void *)code + nwords*N_WORD_BYTES;

    /* Work through the unboxed code. */
    for (p = code_start_addr; p < code_end_addr; p++) {
	void *data = *(void **)p;
	unsigned d1 = *((unsigned char *)p - 1);
	unsigned d2 = *((unsigned char *)p - 2);
	unsigned d3 = *((unsigned char *)p - 3);
	unsigned d4 = *((unsigned char *)p - 4);
#ifdef QSHOW
	unsigned d5 = *((unsigned char *)p - 5);
	unsigned d6 = *((unsigned char *)p - 6);
#endif

	/* Check for code references. */
	/* Check for a 32 bit word that looks like an absolute
	   reference to within the code adea of the code object. */
	if ((data >= (code_start_addr-displacement))
	    && (data < (code_end_addr-displacement))) {
	    /* function header */
	    if ((d4 == 0x5e)
		&& (((unsigned)p - 4 - 4*HeaderValue(*((unsigned *)p-1))) == (unsigned)code)) {
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
	if ((data >= (constants_start_addr-displacement))
	    && (data < (constants_end_addr-displacement))
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

void
gencgc_apply_code_fixups(struct code *old_code, struct code *new_code)
{
    long nheader_words, ncode_words, nwords;
    void *constants_start_addr, *constants_end_addr;
    void *code_start_addr, *code_end_addr;
    lispobj fixups = NIL;
    unsigned displacement = (unsigned)new_code - (unsigned)old_code;
    struct vector *fixups_vector;

    ncode_words = fixnum_value(new_code->code_size);
    nheader_words = HeaderValue(*(lispobj *)new_code);
    nwords = ncode_words + nheader_words;
    /* FSHOW((stderr,
	     "/compiled code object at %x: header words = %d, code words = %d\n",
	     new_code, nheader_words, ncode_words)); */
    constants_start_addr = (void *)new_code + 5*N_WORD_BYTES;
    constants_end_addr = (void *)new_code + nheader_words*N_WORD_BYTES;
    code_start_addr = (void *)new_code + nheader_words*N_WORD_BYTES;
    code_end_addr = (void *)new_code + nwords*N_WORD_BYTES;
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
	fixups_vector = (struct vector *)native_pointer((lispobj)fixups_vector->length);
    }

    /*SHOW("got fixups");*/

    if (widetag_of(fixups_vector->header) == SIMPLE_ARRAY_WORD_WIDETAG) {
	/* Got the fixups for the code block. Now work through the vector,
	   and apply a fixup at each address. */
	long length = fixnum_value(fixups_vector->length);
	long i;
	for (i = 0; i < length; i++) {
	    unsigned offset = fixups_vector->data[i];
	    /* Now check the current value of offset. */
	    unsigned old_value =
		*(unsigned *)((unsigned)code_start_addr + offset);

	    /* If it's within the old_code object then it must be an
	     * absolute fixup (relative ones are not saved) */
	    if ((old_value >= (unsigned)old_code)
		&& (old_value < ((unsigned)old_code + nwords*N_WORD_BYTES)))
		/* So add the dispacement. */
		*(unsigned *)((unsigned)code_start_addr + offset) =
		    old_value + displacement;
	    else
		/* It is outside the old code object so it must be a
		 * relative fixup (absolute fixups are not saved). So
		 * subtract the displacement. */
		*(unsigned *)((unsigned)code_start_addr + offset) =
		    old_value - displacement;
	}
    } else {
        fprintf(stderr, "widetag of fixup vector is %d\n", widetag_of(fixups_vector->header));
    }

    /* Check for possible errors. */
    if (check_code_fixups) {
	sniff_code_object(new_code,displacement);
    }
}


static lispobj
trans_boxed_large(lispobj object)
{
    lispobj header;
    unsigned long length;

    gc_assert(is_lisp_pointer(object));

    header = *((lispobj *) native_pointer(object));
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return copy_large_object(object, length);
}


static lispobj
trans_unboxed_large(lispobj object)
{
    lispobj header;
    unsigned long length;


    gc_assert(is_lisp_pointer(object));

    header = *((lispobj *) native_pointer(object));
    length = HeaderValue(header) + 1;
    length = CEILING(length, 2);

    return copy_large_unboxed_object(object, length);
}


/*
 * vector-like objects
 */


/* FIXME: What does this mean? */
int gencgc_hash = 1;

static int
scav_vector(lispobj *where, lispobj object)
{
    unsigned long kv_length;
    lispobj *kv_vector;
    unsigned long length = 0; /* (0 = dummy to stop GCC warning) */
    lispobj *hash_table;
    lispobj empty_symbol;
    unsigned long *index_vector = NULL; /* (NULL = dummy to stop GCC warning) */
    unsigned long *next_vector = NULL; /* (NULL = dummy to stop GCC warning) */
    unsigned long *hash_vector = NULL; /* (NULL = dummy to stop GCC warning) */
    lispobj weak_p_obj;
    unsigned next_vector_length = 0;

    /* FIXME: A comment explaining this would be nice. It looks as
     * though SB-VM:VECTOR-VALID-HASHING-SUBTYPE is set for EQ-based
     * hash tables in the Lisp HASH-TABLE code, and nowhere else. */
    if (HeaderValue(object) != subtype_VectorValidHashing)
	return 1;

    if (!gencgc_hash) {
	/* This is set for backward compatibility. FIXME: Do we need
	 * this any more? */
	*where =
	    (subtype_VectorMustRehash<<N_WIDETAG_BITS) | SIMPLE_VECTOR_WIDETAG;
	return 1;
    }

    kv_length = fixnum_value(where[1]);
    kv_vector = where + 2;  /* Skip the header and length. */
    /*FSHOW((stderr,"/kv_length = %d\n", kv_length));*/

    /* Scavenge element 0, which may be a hash-table structure. */
    scavenge(where+2, 1);
    if (!is_lisp_pointer(where[2])) {
	lose("no pointer at %x in hash table", where[2]);
    }
    hash_table = (lispobj *)native_pointer(where[2]);
    /*FSHOW((stderr,"/hash_table = %x\n", hash_table));*/
    if (widetag_of(hash_table[0]) != INSTANCE_HEADER_WIDETAG) {
	lose("hash table not instance (%x at %x)", hash_table[0], hash_table);
    }

    /* Scavenge element 1, which should be some internal symbol that
     * the hash table code reserves for marking empty slots. */
    scavenge(where+3, 1);
    if (!is_lisp_pointer(where[3])) {
	lose("not empty-hash-table-slot symbol pointer: %x", where[3]);
    }
    empty_symbol = where[3];
    /* fprintf(stderr,"* empty_symbol = %x\n", empty_symbol);*/
    if (widetag_of(*(lispobj *)native_pointer(empty_symbol)) !=
	SYMBOL_HEADER_WIDETAG) {
	lose("not a symbol where empty-hash-table-slot symbol expected: %x",
	     *(lispobj *)native_pointer(empty_symbol));
    }

    /* Scavenge hash table, which will fix the positions of the other
     * needed objects. */
    scavenge(hash_table, 16);

    /* Cross-check the kv_vector. */
    if (where != (lispobj *)native_pointer(hash_table[9])) {
	lose("hash_table table!=this table %x", hash_table[9]);
    }

    /* WEAK-P */
    weak_p_obj = hash_table[10];

    /* index vector */
    {
	lispobj index_vector_obj = hash_table[13];

	if (is_lisp_pointer(index_vector_obj) &&
	    (widetag_of(*(lispobj *)native_pointer(index_vector_obj)) ==
		 SIMPLE_ARRAY_WORD_WIDETAG)) {
	    index_vector = ((lispobj *)native_pointer(index_vector_obj)) + 2;
	    /*FSHOW((stderr, "/index_vector = %x\n",index_vector));*/
	    length = fixnum_value(((lispobj *)native_pointer(index_vector_obj))[1]);
	    /*FSHOW((stderr, "/length = %d\n", length));*/
	} else {
	    lose("invalid index_vector %x", index_vector_obj);
	}
    }

    /* next vector */
    {
	lispobj next_vector_obj = hash_table[14];

	if (is_lisp_pointer(next_vector_obj) &&
	    (widetag_of(*(lispobj *)native_pointer(next_vector_obj)) ==
	     SIMPLE_ARRAY_WORD_WIDETAG)) {
	    next_vector = ((lispobj *)native_pointer(next_vector_obj)) + 2;
	    /*FSHOW((stderr, "/next_vector = %x\n", next_vector));*/
	    next_vector_length = fixnum_value(((lispobj *)native_pointer(next_vector_obj))[1]);
	    /*FSHOW((stderr, "/next_vector_length = %d\n", next_vector_length));*/
	} else {
	    lose("invalid next_vector %x", next_vector_obj);
	}
    }

    /* maybe hash vector */
    {
	/* FIXME: This bare "15" offset should become a symbolic
	 * expression of some sort. And all the other bare offsets
	 * too. And the bare "16" in scavenge(hash_table, 16). And
	 * probably other stuff too. Ugh.. */
	lispobj hash_vector_obj = hash_table[15];

	if (is_lisp_pointer(hash_vector_obj) &&
	    (widetag_of(*(lispobj *)native_pointer(hash_vector_obj)) ==
	     SIMPLE_ARRAY_WORD_WIDETAG)){
	    hash_vector = ((lispobj *)native_pointer(hash_vector_obj)) + 2;
	    /*FSHOW((stderr, "/hash_vector = %x\n", hash_vector));*/
	    gc_assert(fixnum_value(((lispobj *)native_pointer(hash_vector_obj))[1])
		      == next_vector_length);
	} else {
	    hash_vector = NULL;
	    /*FSHOW((stderr, "/no hash_vector: %x\n", hash_vector_obj));*/
	}
    }

    /* These lengths could be different as the index_vector can be a
     * different length from the others, a larger index_vector could help
     * reduce collisions. */
    gc_assert(next_vector_length*2 == kv_length);

    /* now all set up.. */

    /* Work through the KV vector. */
    {
	long i;
	for (i = 1; i < next_vector_length; i++) {
	    lispobj old_key = kv_vector[2*i];

#if N_WORD_BITS == 32
	    unsigned long old_index = (old_key & 0x1fffffff)%length;
#elif N_WORD_BITS == 64
	    unsigned long old_index = (old_key & 0x1fffffffffffffff)%length;
#endif

	    /* Scavenge the key and value. */
	    scavenge(&kv_vector[2*i],2);

	    /* Check whether the key has moved and is EQ based. */
	    {
		lispobj new_key = kv_vector[2*i];
#if N_WORD_BITS == 32
		unsigned long new_index = (new_key & 0x1fffffff)%length;
#elif N_WORD_BITS == 64
		unsigned long new_index = (new_key & 0x1fffffffffffffff)%length;
#endif

		if ((old_index != new_index) &&
		    ((!hash_vector) || (hash_vector[i] == 0x80000000)) &&
		    ((new_key != empty_symbol) ||
		     (kv_vector[2*i] != empty_symbol))) {

		     /*FSHOW((stderr,
			    "* EQ key %d moved from %x to %x; index %d to %d\n",
			    i, old_key, new_key, old_index, new_index));*/

		    if (index_vector[old_index] != 0) {
			 /*FSHOW((stderr, "/P1 %d\n", index_vector[old_index]));*/

			/* Unlink the key from the old_index chain. */
			if (index_vector[old_index] == i) {
			    /*FSHOW((stderr, "/P2a %d\n", next_vector[i]));*/
			    index_vector[old_index] = next_vector[i];
			    /* Link it into the needing rehash chain. */
			    next_vector[i] = fixnum_value(hash_table[11]);
			    hash_table[11] = make_fixnum(i);
			    /*SHOW("P2");*/
			} else {
			    unsigned prior = index_vector[old_index];
			    unsigned next = next_vector[prior];

			    /*FSHOW((stderr, "/P3a %d %d\n", prior, next));*/

			    while (next != 0) {
				 /*FSHOW((stderr, "/P3b %d %d\n", prior, next));*/
				if (next == i) {
				    /* Unlink it. */
				    next_vector[prior] = next_vector[next];
				    /* Link it into the needing rehash
				     * chain. */
				    next_vector[next] =
					fixnum_value(hash_table[11]);
				    hash_table[11] = make_fixnum(next);
				    /*SHOW("/P3");*/
				    break;
				}
				prior = next;
				next = next_vector[next];
			    }
			}
		    }
		}
	    }
	}
    }
    return (CEILING(kv_length + 2, 2));
}



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

static long
scav_weak_pointer(lispobj *where, lispobj object)
{
    struct weak_pointer *wp = weak_pointers;
    /* Push the weak pointer onto the list of weak pointers.
     * Do I have to watch for duplicates? Originally this was
     * part of trans_weak_pointer but that didn't work in the
     * case where the WP was in a promoted region.
     */

    /* Check whether it's already in the list. */
    while (wp != NULL) {
	if (wp == (struct weak_pointer*)where) {
	    break;
	}
	wp = wp->next;
    }
    if (wp == NULL) {
	/* Add it to the start of the list. */
	wp = (struct weak_pointer*)where;
	if (wp->next != weak_pointers) {
	    wp->next = weak_pointers;
	} else {
	    /*SHOW("avoided write to weak pointer");*/
	}
	weak_pointers = wp;
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
    return (search_space(start, 
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
    return (search_space(start, 
			 (((lispobj *)pointer)+2)-start, 
			 (lispobj *) pointer));
}

/* a faster version for searching the dynamic space. This will work even
 * if the object is in a current allocation region. */
lispobj *
search_dynamic_space(void *pointer)
{
    long page_index = find_page_index(pointer);
    lispobj *start;

    /* The address may be invalid, so do some checks. */
    if ((page_index == -1) ||
	(page_table[page_index].allocated == FREE_PAGE_FLAG))
	return NULL;
    start = (lispobj *)((void *)page_address(page_index)
			+ page_table[page_index].first_object_offset);
    return (search_space(start, 
			 (((lispobj *)pointer)+2)-start, 
			 (lispobj *)pointer));
}

/* Is there any possibility that pointer is a valid Lisp object
 * reference, and/or something else (e.g. subroutine call return
 * address) which should prevent us from moving the referred-to thing?
 * This is called from preserve_pointers() */
static int
possibly_valid_dynamic_space_pointer(lispobj *pointer)
{
    lispobj *start_addr;

    /* Find the object start address. */
    if ((start_addr = search_dynamic_space(pointer)) == NULL) {
	return 0;
    }

    /* We need to allow raw pointers into Code objects for return
     * addresses. This will also pick up pointers to functions in code
     * objects. */
    if (widetag_of(*start_addr) == CODE_HEADER_WIDETAG) {
	/* XXX could do some further checks here */
	return 1;
    }

    /* If it's not a return address then it needs to be a valid Lisp
     * pointer. */
    if (!is_lisp_pointer((lispobj)pointer)) {
	return 0;
    }

    /* Check that the object pointed to is consistent with the pointer
     * low tag.
     */
    switch (lowtag_of((lispobj)pointer)) {
    case FUN_POINTER_LOWTAG:
	/* Start_addr should be the enclosing code object, or a closure
	 * header. */
	switch (widetag_of(*start_addr)) {
	case CODE_HEADER_WIDETAG:
	    /* This case is probably caught above. */
	    break;
	case CLOSURE_HEADER_WIDETAG:
	case FUNCALLABLE_INSTANCE_HEADER_WIDETAG:
	    if ((unsigned)pointer !=
		((unsigned)start_addr+FUN_POINTER_LOWTAG)) {
		if (gencgc_verbose)
		    FSHOW((stderr,
			   "/Wf2: %x %x %x\n",
			   pointer, start_addr, *start_addr));
		return 0;
	    }
	    break;
	default:
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "/Wf3: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;
	}
	break;
    case LIST_POINTER_LOWTAG:
	if ((unsigned)pointer !=
	    ((unsigned)start_addr+LIST_POINTER_LOWTAG)) {
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "/Wl1: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;
	}
	/* Is it plausible cons? */
	if ((is_lisp_pointer(start_addr[0])
	    || (fixnump(start_addr[0]))
	    || (widetag_of(start_addr[0]) == CHARACTER_WIDETAG)
#if N_WORD_BITS == 64
	    || (widetag_of(start_addr[0]) == SINGLE_FLOAT_WIDETAG)
#endif
	    || (widetag_of(start_addr[0]) == UNBOUND_MARKER_WIDETAG))
	   && (is_lisp_pointer(start_addr[1])
	       || (fixnump(start_addr[1]))
	       || (widetag_of(start_addr[1]) == CHARACTER_WIDETAG)
#if N_WORD_BITS == 64
	       || (widetag_of(start_addr[1]) == SINGLE_FLOAT_WIDETAG)
#endif
	       || (widetag_of(start_addr[1]) == UNBOUND_MARKER_WIDETAG)))
	    break;
	else {
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "/Wl2: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;
	}
    case INSTANCE_POINTER_LOWTAG:
	if ((unsigned)pointer !=
	    ((unsigned)start_addr+INSTANCE_POINTER_LOWTAG)) {
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "/Wi1: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;
	}
	if (widetag_of(start_addr[0]) != INSTANCE_HEADER_WIDETAG) {
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "/Wi2: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;
	}
	break;
    case OTHER_POINTER_LOWTAG:
	if ((unsigned)pointer !=
	    ((int)start_addr+OTHER_POINTER_LOWTAG)) {
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "/Wo1: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;
	}
	/* Is it plausible?  Not a cons. XXX should check the headers. */
	if (is_lisp_pointer(start_addr[0]) || ((start_addr[0] & 3) == 0)) {
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "/Wo2: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;
	}
	switch (widetag_of(start_addr[0])) {
	case UNBOUND_MARKER_WIDETAG:
	case CHARACTER_WIDETAG:
#if N_WORD_BITS == 64
	case SINGLE_FLOAT_WIDETAG:
#endif
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "*Wo3: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;

	    /* only pointed to by function pointers? */
	case CLOSURE_HEADER_WIDETAG:
	case FUNCALLABLE_INSTANCE_HEADER_WIDETAG:
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "*Wo4: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;

	case INSTANCE_HEADER_WIDETAG:
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "*Wo5: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;

	    /* the valid other immediate pointer objects */
	case SIMPLE_VECTOR_WIDETAG:
	case RATIO_WIDETAG:
	case COMPLEX_WIDETAG:
#ifdef COMPLEX_SINGLE_FLOAT_WIDETAG
	case COMPLEX_SINGLE_FLOAT_WIDETAG:
#endif
#ifdef COMPLEX_DOUBLE_FLOAT_WIDETAG
	case COMPLEX_DOUBLE_FLOAT_WIDETAG:
#endif
#ifdef COMPLEX_LONG_FLOAT_WIDETAG
	case COMPLEX_LONG_FLOAT_WIDETAG:
#endif
	case SIMPLE_ARRAY_WIDETAG:
	case COMPLEX_BASE_STRING_WIDETAG:
#ifdef COMPLEX_CHARACTER_STRING_WIDETAG
	case COMPLEX_CHARACTER_STRING_WIDETAG:
#endif
	case COMPLEX_VECTOR_NIL_WIDETAG:
	case COMPLEX_BIT_VECTOR_WIDETAG:
	case COMPLEX_VECTOR_WIDETAG:
	case COMPLEX_ARRAY_WIDETAG:
	case VALUE_CELL_HEADER_WIDETAG:
	case SYMBOL_HEADER_WIDETAG:
	case FDEFN_WIDETAG:
	case CODE_HEADER_WIDETAG:
	case BIGNUM_WIDETAG:
#if N_WORD_BITS != 64
	case SINGLE_FLOAT_WIDETAG:
#endif
	case DOUBLE_FLOAT_WIDETAG:
#ifdef LONG_FLOAT_WIDETAG
	case LONG_FLOAT_WIDETAG:
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
#ifdef  SIMPLE_ARRAY_UNSIGNED_BYTE_29_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_29_WIDETAG:
#endif
	case SIMPLE_ARRAY_UNSIGNED_BYTE_31_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
#ifdef  SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG:
#endif
#ifdef  SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_63_WIDETAG:
#endif
#ifdef  SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG:
	case SIMPLE_ARRAY_UNSIGNED_BYTE_64_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG
	case SIMPLE_ARRAY_SIGNED_BYTE_8_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG
	case SIMPLE_ARRAY_SIGNED_BYTE_16_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
	case SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
	case SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG
	case SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG:
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
	case SAP_WIDETAG:
	case WEAK_POINTER_WIDETAG:
	    break;

	default:
	    if (gencgc_verbose)
		FSHOW((stderr,
		       "/Wo6: %x %x %x\n",
		       pointer, start_addr, *start_addr));
	    return 0;
	}
	break;
    default:
	if (gencgc_verbose)
	    FSHOW((stderr,
		   "*W?: %x %x %x\n",
		   pointer, start_addr, *start_addr));
	return 0;
    }

    /* looks good */
    return 1;
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
    long first_page;
    long nwords;

    long remaining_bytes;
    long next_page;
    long bytes_freed;
    long old_bytes_used;

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
#ifdef  SIMPLE_ARRAY_UNSIGNED_BYTE_29_WIDETAG
    case SIMPLE_ARRAY_UNSIGNED_BYTE_29_WIDETAG:
#endif
    case SIMPLE_ARRAY_UNSIGNED_BYTE_31_WIDETAG:
    case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG
    case SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG:
#endif
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
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
    case SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
    case SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG
    case SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG:
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

    gc_assert(page_table[first_page].first_object_offset == 0);

    next_page = first_page;
    remaining_bytes = nwords*N_WORD_BYTES;
    while (remaining_bytes > PAGE_BYTES) {
	gc_assert(page_table[next_page].gen == from_space);
	gc_assert((page_table[next_page].allocated == BOXED_PAGE_FLAG)
		  || (page_table[next_page].allocated == UNBOXED_PAGE_FLAG));
	gc_assert(page_table[next_page].large_object);
	gc_assert(page_table[next_page].first_object_offset ==
		  -PAGE_BYTES*(next_page-first_page));
	gc_assert(page_table[next_page].bytes_used == PAGE_BYTES);

	page_table[next_page].allocated = boxed;

	/* Shouldn't be write-protected at this stage. Essential that the
	 * pages aren't. */
	gc_assert(!page_table[next_page].write_protected);
	remaining_bytes -= PAGE_BYTES;
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
    while ((old_bytes_used == PAGE_BYTES) &&
	   (page_table[next_page].gen == from_space) &&
	   ((page_table[next_page].allocated == UNBOXED_PAGE_FLAG)
	    || (page_table[next_page].allocated == BOXED_PAGE_FLAG)) &&
	   page_table[next_page].large_object &&
	   (page_table[next_page].first_object_offset ==
	    -(next_page - first_page)*PAGE_BYTES)) {
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
    long addr_page_index = find_page_index(addr);
    long first_page;
    long i;
    unsigned region_allocation;

    /* quick check 1: Address is quite likely to have been invalid. */
    if ((addr_page_index == -1)
	|| (page_table[addr_page_index].allocated == FREE_PAGE_FLAG)
	|| (page_table[addr_page_index].bytes_used == 0)
	|| (page_table[addr_page_index].gen != from_space)
	/* Skip if already marked dont_move. */
	|| (page_table[addr_page_index].dont_move != 0))
	return;
    gc_assert(!(page_table[addr_page_index].allocated&OPEN_REGION_PAGE_FLAG));
    /* (Now that we know that addr_page_index is in range, it's
     * safe to index into page_table[] with it.) */
    region_allocation = page_table[addr_page_index].allocated;

    /* quick check 2: Check the offset within the page.
     *
     */
    if (((unsigned)addr & (PAGE_BYTES - 1)) > page_table[addr_page_index].bytes_used)
	return;

    /* Filter out anything which can't be a pointer to a Lisp object
     * (or, as a special case which also requires dont_move, a return
     * address referring to something in a CodeObject). This is
     * expensive but important, since it vastly reduces the
     * probability that random garbage will be bogusly interpreted as
     * a pointer which prevents a page from moving. */
    if (!(possibly_valid_dynamic_space_pointer(addr)))
	return;

    /* Find the beginning of the region.  Note that there may be
     * objects in the region preceding the one that we were passed a
     * pointer to: if this is the case, we will write-protect all the
     * previous objects' pages too.     */

#if 0
    /* I think this'd work just as well, but without the assertions.
     * -dan 2004.01.01 */
    first_page=
	find_page_index(page_address(addr_page_index)+
			page_table[addr_page_index].first_object_offset);
#else 
    first_page = addr_page_index;
    while (page_table[first_page].first_object_offset != 0) {
	--first_page;
	/* Do some checks. */
	gc_assert(page_table[first_page].bytes_used == PAGE_BYTES);
	gc_assert(page_table[first_page].gen == from_space);
	gc_assert(page_table[first_page].allocated == region_allocation);
    }
#endif

    /* Adjust any large objects before promotion as they won't be
     * copied after promotion. */
    if (page_table[first_page].large_object) {
	maybe_adjust_large_object(page_address(first_page));
	/* If a large object has shrunk then addr may now point to a
	 * free area in which case it's ignored here. Note it gets
	 * through the valid pointer test above because the tail looks
	 * like conses. */
	if ((page_table[addr_page_index].allocated == FREE_PAGE_FLAG)
	    || (page_table[addr_page_index].bytes_used == 0)
	    /* Check the offset within the page. */
	    || (((unsigned)addr & (PAGE_BYTES - 1))
		> page_table[addr_page_index].bytes_used)) {
	    FSHOW((stderr,
		   "weird? ignore ptr 0x%x to freed area of large object\n",
		   addr));
	    return;
	}
	/* It may have moved to unboxed pages. */
	region_allocation = page_table[first_page].allocated;
    }

    /* Now work forward until the end of this contiguous area is found,
     * marking all pages as dont_move. */
    for (i = first_page; ;i++) {
	gc_assert(page_table[i].allocated == region_allocation);

	/* Mark the page static. */
	page_table[i].dont_move = 1;

	/* Move the page to the new_space. XX I'd rather not do this
	 * but the GC logic is not quite able to copy with the static
	 * pages remaining in the from space. This also requires the
	 * generation bytes_allocated counters be updated. */
	page_table[i].gen = new_space;
	generations[new_space].bytes_allocated += page_table[i].bytes_used;
	generations[from_space].bytes_allocated -= page_table[i].bytes_used;

	/* It is essential that the pages are not write protected as
	 * they may have pointers into the old-space which need
	 * scavenging. They shouldn't be write protected at this
	 * stage. */
	gc_assert(!page_table[i].write_protected);

	/* Check whether this is the last page in this contiguous block.. */
	if ((page_table[i].bytes_used < PAGE_BYTES)
	    /* ..or it is PAGE_BYTES and is the last in the block */
	    || (page_table[i+1].allocated == FREE_PAGE_FLAG)
	    || (page_table[i+1].bytes_used == 0) /* next page free */
	    || (page_table[i+1].gen != from_space) /* diff. gen */
	    || (page_table[i+1].first_object_offset == 0))
	    break;
    }

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
update_page_write_prot(long page)
{
    int gen = page_table[page].gen;
    long j;
    int wp_it = 1;
    void **page_addr = (void **)page_address(page);
    long num_words = page_table[page].bytes_used / N_WORD_BYTES;

    /* Shouldn't be a free page. */
    gc_assert(page_table[page].allocated != FREE_PAGE_FLAG);
    gc_assert(page_table[page].bytes_used != 0);

    /* Skip if it's already write-protected, pinned, or unboxed */
    if (page_table[page].write_protected
	|| page_table[page].dont_move
	|| (page_table[page].allocated & UNBOXED_PAGE_FLAG))
	return (0);

    /* Scan the page for pointers to younger generations or the
     * top temp. generation. */

    for (j = 0; j < num_words; j++) {
	void *ptr = *(page_addr+j);
	long index = find_page_index(ptr);

	/* Check that it's in the dynamic space */
	if (index != -1)
	    if (/* Does it point to a younger or the temp. generation? */
		((page_table[index].allocated != FREE_PAGE_FLAG)
		 && (page_table[index].bytes_used != 0)
		 && ((page_table[index].gen < gen)
		     || (page_table[index].gen == NUM_GENERATIONS)))

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
		   PAGE_BYTES,
		   OS_VM_PROT_READ|OS_VM_PROT_EXECUTE);

	/* Note the page as protected in the page tables. */
	page_table[page].write_protected = 1;
    }

    return (wp_it);
}

/* Scavenge a generation.
 *
 * This will not resolve all pointers when generation is the new
 * space, as new objects may be added which are not checked here - use
 * scavenge_newspace generation.
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
scavenge_generation(int generation)
{
    long i;
    int num_wp = 0;

#define SC_GEN_CK 0
#if SC_GEN_CK
    /* Clear the write_protected_cleared flags on all pages. */
    for (i = 0; i < NUM_PAGES; i++)
	page_table[i].write_protected_cleared = 0;
#endif

    for (i = 0; i < last_free_page; i++) {
	if ((page_table[i].allocated & BOXED_PAGE_FLAG)
	    && (page_table[i].bytes_used != 0)
	    && (page_table[i].gen == generation)) {
	    long last_page,j;
	    int write_protected=1;

	    /* This should be the start of a region */
	    gc_assert(page_table[i].first_object_offset == 0);

	    /* Now work forward until the end of the region */
	    for (last_page = i; ; last_page++) {
		write_protected =
		    write_protected && page_table[last_page].write_protected;
		if ((page_table[last_page].bytes_used < PAGE_BYTES)
		    /* Or it is PAGE_BYTES and is the last in the block */
		    || (!(page_table[last_page+1].allocated & BOXED_PAGE_FLAG))
		    || (page_table[last_page+1].bytes_used == 0)
		    || (page_table[last_page+1].gen != generation)
		    || (page_table[last_page+1].first_object_offset == 0))
		    break;
	    }
	    if (!write_protected) {
		scavenge(page_address(i), 
			 (page_table[last_page].bytes_used +
			  (last_page-i)*PAGE_BYTES)/N_WORD_BYTES);
		
		/* Now scan the pages and write protect those that
		 * don't have pointers to younger generations. */
		if (enable_page_protection) {
		    for (j = i; j <= last_page; j++) {
			num_wp += update_page_write_prot(j);
		    }
		}
	    }
	    i = last_page;
	}
    }
    if ((gencgc_verbose > 1) && (num_wp != 0)) {
	FSHOW((stderr,
	       "/write protected %d pages within generation %d\n",
	       num_wp, generation));
    }

#if SC_GEN_CK
    /* Check that none of the write_protected pages in this generation
     * have been written to. */
    for (i = 0; i < NUM_PAGES; i++) {
	if ((page_table[i].allocation != FREE_PAGE_FLAG)
	    && (page_table[i].bytes_used != 0)
	    && (page_table[i].gen == generation)
	    && (page_table[i].write_protected_cleared != 0)) {
	    FSHOW((stderr, "/scavenge_generation() %d\n", generation));
	    FSHOW((stderr,
		   "/page bytes_used=%d first_object_offset=%d dont_move=%d\n",
		    page_table[i].bytes_used,
		    page_table[i].first_object_offset,
		    page_table[i].dont_move));
	    lose("write to protected page %d in scavenge_generation()", i);
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
scavenge_newspace_generation_one_scan(int generation)
{
    long i;

    FSHOW((stderr,
	   "/starting one full scan of newspace generation %d\n",
	   generation));
    for (i = 0; i < last_free_page; i++) {
	/* Note that this skips over open regions when it encounters them. */
	if ((page_table[i].allocated & BOXED_PAGE_FLAG)
	    && (page_table[i].bytes_used != 0)
	    && (page_table[i].gen == generation)
	    && ((page_table[i].write_protected == 0)
		/* (This may be redundant as write_protected is now
		 * cleared before promotion.) */
		|| (page_table[i].dont_move == 1))) {
	    long last_page;
	    int all_wp=1;

	    /* The scavenge will start at the first_object_offset of page i.
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
		if ((page_table[last_page].bytes_used < PAGE_BYTES)
		    /* Or it is PAGE_BYTES and is the last in the block */
		    || (!(page_table[last_page+1].allocated & BOXED_PAGE_FLAG))
		    || (page_table[last_page+1].bytes_used == 0)
		    || (page_table[last_page+1].gen != generation)
		    || (page_table[last_page+1].first_object_offset == 0))
		    break;
	    }

	    /* Do a limited check for write-protected pages.  */
	    if (!all_wp) {
		long size;
		
		size = (page_table[last_page].bytes_used
			+ (last_page-i)*PAGE_BYTES
			- page_table[i].first_object_offset)/N_WORD_BYTES;
		new_areas_ignore_page = last_page;
		
		scavenge(page_address(i) +
			 page_table[i].first_object_offset,
			 size);
		
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
scavenge_newspace_generation(int generation)
{
    long i;

    /* the new_areas array currently being written to by gc_alloc() */
    struct new_area (*current_new_areas)[] = &new_areas_1;
    long current_new_areas_index;

    /* the new_areas created by the previous scavenge cycle */
    struct new_area (*previous_new_areas)[] = NULL;
    long previous_new_areas_index;

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
	    if (gencgc_verbose)
		SHOW("new_areas overflow, doing full scavenge");

	    /* Don't need to record new areas that get scavenge anyway
	     * during scavenge_newspace_generation_one_scan. */
	    record_new_objects = 1;

	    scavenge_newspace_generation_one_scan(generation);

	    /* Record all new areas now. */
	    record_new_objects = 2;

	    /* Flush the current regions updating the tables. */
	    gc_alloc_update_all_page_tables();

	} else {

	    /* Work through previous_new_areas. */
	    for (i = 0; i < previous_new_areas_index; i++) {
		long page = (*previous_new_areas)[i].page;
		long offset = (*previous_new_areas)[i].offset;
		long size = (*previous_new_areas)[i].size / N_WORD_BYTES;
		gc_assert((*previous_new_areas)[i].size % N_WORD_BYTES == 0);
		scavenge(page_address(page)+offset, size);
	    }

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
    /* Check that none of the write_protected pages in this generation
     * have been written to. */
    for (i = 0; i < NUM_PAGES; i++) {
	if ((page_table[i].allocation != FREE_PAGE_FLAG)
	    && (page_table[i].bytes_used != 0)
	    && (page_table[i].gen == generation)
	    && (page_table[i].write_protected_cleared != 0)
	    && (page_table[i].dont_move == 0)) {
	    lose("write protected page %d written to in scavenge_newspace_generation\ngeneration=%d dont_move=%d",
		 i, generation, page_table[i].dont_move);
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
    long i;

    for (i = 0; i < last_free_page; i++) {
	if ((page_table[i].allocated != FREE_PAGE_FLAG)
	    && (page_table[i].bytes_used != 0)
	    && (page_table[i].gen == from_space)) {
	    void *page_start;

	    page_start = (void *)page_address(i);

	    /* Remove any write-protection. We should be able to rely
	     * on the write-protect flag to avoid redundant calls. */
	    if (page_table[i].write_protected) {
		os_protect(page_start, PAGE_BYTES, OS_VM_PROT_ALL);
		page_table[i].write_protected = 0;
	    }
	}
    }
}

/* Work through all the pages and free any in from_space. This
 * assumes that all objects have been copied or promoted to an older
 * generation. Bytes_allocated and the generation bytes_allocated
 * counter are updated. The number of bytes freed is returned. */
static long
free_oldspace(void)
{
    long bytes_freed = 0;
    long first_page, last_page;

    first_page = 0;

    do {
	/* Find a first page for the next region of pages. */
	while ((first_page < last_free_page)
	       && ((page_table[first_page].allocated == FREE_PAGE_FLAG)
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

	    /* Remove any write-protection. We should be able to rely
	     * on the write-protect flag to avoid redundant calls. */
	    {
		void  *page_start = (void *)page_address(last_page);
	
		if (page_table[last_page].write_protected) {
		    os_protect(page_start, PAGE_BYTES, OS_VM_PROT_ALL);
		    page_table[last_page].write_protected = 0;
		}
	    }
	    last_page++;
	}
	while ((last_page < last_free_page)
	       && (page_table[last_page].allocated != FREE_PAGE_FLAG)
	       && (page_table[last_page].bytes_used != 0)
	       && (page_table[last_page].gen == from_space));

	/* Zero pages from first_page to (last_page-1).
	 *
	 * FIXME: Why not use os_zero(..) function instead of
	 * hand-coding this again? (Check other gencgc_unmap_zero
	 * stuff too. */
	if (gencgc_unmap_zero) {
	    void *page_start, *addr;

	    page_start = (void *)page_address(first_page);

	    os_invalidate(page_start, PAGE_BYTES*(last_page-first_page));
	    addr = os_validate(page_start, PAGE_BYTES*(last_page-first_page));
	    if (addr == NULL || addr != page_start) {
		lose("free_oldspace: page moved, 0x%08x ==> 0x%08x",page_start,
		     addr);
	    }
	} else {
	    long *page_start;

	    page_start = (long *)page_address(first_page);
	    memset(page_start, 0,PAGE_BYTES*(last_page-first_page));
	}

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
    long pi1 = find_page_index((void*)addr);

    if (pi1 != -1)
	fprintf(stderr,"  %x: page %d  alloc %d  gen %d  bytes_used %d  offset %d  dont_move %d\n",
		(unsigned long) addr,
		pi1,
		page_table[pi1].allocated,
		page_table[pi1].gen,
		page_table[pi1].bytes_used,
		page_table[pi1].first_object_offset,
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

extern long undefined_tramp;

static void
verify_space(lispobj *start, size_t words)
{
    int is_in_dynamic_space = (find_page_index((void*)start) != -1);
    int is_in_readonly_space =
	(READ_ONLY_SPACE_START <= (unsigned)start &&
	 (unsigned)start < SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0));

    while (words > 0) {
	size_t count = 1;
	lispobj thing = *(lispobj*)start;

	if (is_lisp_pointer(thing)) {
	    long page_index = find_page_index((void*)thing);
	    long to_readonly_space =
		(READ_ONLY_SPACE_START <= thing &&
		 thing < SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0));
	    long to_static_space =
		(STATIC_SPACE_START <= thing &&
		 thing < SymbolValue(STATIC_SPACE_FREE_POINTER,0));

	    /* Does it point to the dynamic space? */
	    if (page_index != -1) {
		/* If it's within the dynamic space it should point to a used
		 * page. XX Could check the offset too. */
		if ((page_table[page_index].allocated != FREE_PAGE_FLAG)
		    && (page_table[page_index].bytes_used == 0))
		    lose ("Ptr %x @ %x sees free page.", thing, start);
		/* Check that it doesn't point to a forwarding pointer! */
		if (*((lispobj *)native_pointer(thing)) == 0x01) {
		    lose("Ptr %x @ %x sees forwarding ptr.", thing, start);
		}
		/* Check that its not in the RO space as it would then be a
		 * pointer from the RO to the dynamic space. */
		if (is_in_readonly_space) {
		    lose("ptr to dynamic space %x from RO space %x",
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
		if (!possibly_valid_dynamic_space_pointer((lispobj *)thing)) {
		    lose("ptr %x to invalid object %x", thing, start); 
		}
		*/
	    } else {
		/* Verify that it points to another valid space. */
		if (!to_readonly_space && !to_static_space
		    && (thing != (unsigned)&undefined_tramp)) {
		    lose("Ptr %x @ %x sees junk.", thing, start);
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
		case INSTANCE_HEADER_WIDETAG:
		case FDEFN_WIDETAG:
		    count = 1;
		    break;

		case CODE_HEADER_WIDETAG:
		    {
			lispobj object = *start;
			struct code *code;
			long nheader_words, ncode_words, nwords;
			lispobj fheaderl;
			struct simple_fun *fheaderp;

			code = (struct code *) start;

			/* Check that it's not in the dynamic space.
			 * FIXME: Isn't is supposed to be OK for code
			 * objects to be in the dynamic space these days? */
			if (is_in_dynamic_space
			    /* It's ok if it's byte compiled code. The trace
			     * table offset will be a fixnum if it's x86
			     * compiled code - check.
			     *
			     * FIXME: #^#@@! lack of abstraction here..
			     * This line can probably go away now that
			     * there's no byte compiler, but I've got
			     * too much to worry about right now to try
			     * to make sure. -- WHN 2001-10-06 */
			    && fixnump(code->trace_table_offset)
			    /* Only when enabled */
			    && verify_dynamic_code_check) {
			    FSHOW((stderr,
				   "/code object at %x in the dynamic space\n",
				   start));
			}

			ncode_words = fixnum_value(code->code_size);
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
			    gc_assert(widetag_of(fheaderp->header) == SIMPLE_FUN_HEADER_WIDETAG);
			    verify_space(&fheaderp->name, 1);
			    verify_space(&fheaderp->arglist, 1);
			    verify_space(&fheaderp->type, 1);
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
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_29_WIDETAG
		case SIMPLE_ARRAY_UNSIGNED_BYTE_29_WIDETAG:
#endif
		case SIMPLE_ARRAY_UNSIGNED_BYTE_31_WIDETAG:
		case SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG:
#ifdef SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG
		case SIMPLE_ARRAY_UNSIGNED_BYTE_60_WIDETAG:
#endif
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
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG
		case SIMPLE_ARRAY_SIGNED_BYTE_30_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG
		case SIMPLE_ARRAY_SIGNED_BYTE_32_WIDETAG:
#endif
#ifdef SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG
		case SIMPLE_ARRAY_SIGNED_BYTE_61_WIDETAG:
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
		    count = (sizetab[widetag_of(*start)])(start);
		    break;

		default:
		    gc_abort();
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
    long read_only_space_size =
	(lispobj*)SymbolValue(READ_ONLY_SPACE_FREE_POINTER,0)
	- (lispobj*)READ_ONLY_SPACE_START;
    long static_space_size =
	(lispobj*)SymbolValue(STATIC_SPACE_FREE_POINTER,0)
	- (lispobj*)STATIC_SPACE_START;
    struct thread *th;
    for_each_thread(th) {
    long binding_stack_size =
	    (lispobj*)SymbolValue(BINDING_STACK_POINTER,th)
	    - (lispobj*)th->binding_stack_start;
	verify_space(th->binding_stack_start, binding_stack_size);
    }
    verify_space((lispobj*)READ_ONLY_SPACE_START, read_only_space_size);
    verify_space((lispobj*)STATIC_SPACE_START   , static_space_size);
}

static void
verify_generation(int  generation)
{
    int i;

    for (i = 0; i < last_free_page; i++) {
	if ((page_table[i].allocated != FREE_PAGE_FLAG)
	    && (page_table[i].bytes_used != 0)
	    && (page_table[i].gen == generation)) {
	    long last_page;
	    int region_allocation = page_table[i].allocated;

	    /* This should be the start of a contiguous block */
	    gc_assert(page_table[i].first_object_offset == 0);

	    /* Need to find the full extent of this contiguous block in case
	       objects span pages. */

	    /* Now work forward until the end of this contiguous area is
	       found. */
	    for (last_page = i; ;last_page++)
		/* Check whether this is the last page in this contiguous
		 * block. */
		if ((page_table[last_page].bytes_used < PAGE_BYTES)
		    /* Or it is PAGE_BYTES and is the last in the block */
		    || (page_table[last_page+1].allocated != region_allocation)
		    || (page_table[last_page+1].bytes_used == 0)
		    || (page_table[last_page+1].gen != generation)
		    || (page_table[last_page+1].first_object_offset == 0))
		    break;

	    verify_space(page_address(i), (page_table[last_page].bytes_used
					   + (last_page-i)*PAGE_BYTES)/N_WORD_BYTES);
	    i = last_page;
	}
    }
}

/* Check that all the free space is zero filled. */
static void
verify_zero_fill(void)
{
    long page;

    for (page = 0; page < last_free_page; page++) {
	if (page_table[page].allocated == FREE_PAGE_FLAG) {
	    /* The whole page should be zero filled. */
	    long *start_addr = (long *)page_address(page);
	    long size = 1024;
	    long i;
	    for (i = 0; i < size; i++) {
		if (start_addr[i] != 0) {
		    lose("free page not zero at %x", start_addr + i);
		}
	    }
	} else {
	    long free_bytes = PAGE_BYTES - page_table[page].bytes_used;
	    if (free_bytes > 0) {
		long *start_addr = (long *)((unsigned)page_address(page)
					  + page_table[page].bytes_used);
		long size = free_bytes / N_WORD_BYTES;
		long i;
		for (i = 0; i < size; i++) {
		    if (start_addr[i] != 0) {
			lose("free region not zero at %x", start_addr + i);
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
    long i;

    for (i = 0; i < NUM_GENERATIONS; i++)
	verify_generation(i);

    if (gencgc_enable_verify_zero_fill)
	verify_zero_fill();
}

/* Write-protect all the dynamic boxed pages in the given generation. */
static void
write_protect_generation_pages(int generation)
{
    long i;

    gc_assert(generation < NUM_GENERATIONS);

    for (i = 0; i < last_free_page; i++)
	if ((page_table[i].allocated == BOXED_PAGE_FLAG)
	    && (page_table[i].bytes_used != 0)
	    && !page_table[i].dont_move
	    && (page_table[i].gen == generation))  {
	    void *page_start;

	    page_start = (void *)page_address(i);

	    os_protect(page_start,
		       PAGE_BYTES,
		       OS_VM_PROT_READ | OS_VM_PROT_EXECUTE);

	    /* Note the page as protected in the page tables. */
	    page_table[i].write_protected = 1;
	}

    if (gencgc_verbose > 1) {
	FSHOW((stderr,
	       "/write protected %d of %d pages in generation %d\n",
	       count_write_protect_generation_pages(generation),
	       count_generation_pages(generation),
	       generation));
    }
}

/* Garbage collect a generation. If raise is 0 then the remains of the
 * generation are not raised to the next generation. */
static void
garbage_collect_generation(int generation, int raise)
{
    unsigned long bytes_freed;
    unsigned long i;
    unsigned long static_space_size;
    struct thread *th;
    gc_assert(generation <= (NUM_GENERATIONS-1));

    /* The oldest generation can't be raised. */
    gc_assert((generation != (NUM_GENERATIONS-1)) || (raise == 0));

    /* Initialize the weak pointer list. */
    weak_pointers = NULL;

    /* When a generation is not being raised it is transported to a
     * temporary generation (NUM_GENERATIONS), and lowered when
     * done. Set up this new generation. There should be no pages
     * allocated to it yet. */
    if (!raise) {
	 gc_assert(generations[NUM_GENERATIONS].bytes_allocated == 0);
    }

    /* Set the global src and dest. generations */
    from_space = generation;
    if (raise)
	new_space = generation+1;
    else
	new_space = NUM_GENERATIONS;

    /* Change to a new space for allocation, resetting the alloc_start_page */
    gc_alloc_generation = new_space;
    generations[new_space].alloc_start_page = 0;
    generations[new_space].alloc_unboxed_start_page = 0;
    generations[new_space].alloc_large_start_page = 0;
    generations[new_space].alloc_large_unboxed_start_page = 0;

    /* Before any pointers are preserved, the dont_move flags on the
     * pages need to be cleared. */
    for (i = 0; i < last_free_page; i++)
	if(page_table[i].gen==from_space)
	    page_table[i].dont_move = 0;

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
    for_each_thread(th) {
	void **ptr;
	void **esp=(void **)-1;
#ifdef LISP_FEATURE_SB_THREAD
	long i,free;
	if(th==arch_os_get_current_thread()) {
	    esp = (void **) &raise;
	} else {
	    void **esp1;
	    free=fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX,th));
	    for(i=free-1;i>=0;i--) {
		os_context_t *c=th->interrupt_contexts[i];
		esp1 = (void **) *os_context_register_addr(c,reg_ESP);
		if(esp1>=th->control_stack_start&& esp1<th->control_stack_end){
		    if(esp1<esp) esp=esp1;
		    for(ptr = (void **)(c+1); ptr>=(void **)c; ptr--) {
			preserve_pointer(*ptr);
		    }
		}
	    }
	}
#else
	esp = (void **) &raise;
#endif
	for (ptr = (void **)th->control_stack_end; ptr > esp;  ptr--) {
	    preserve_pointer(*ptr);
	}
    }

#ifdef QSHOW
    if (gencgc_verbose > 1) {
	long num_dont_move_pages = count_dont_move_pages();
	fprintf(stderr,
		"/non-movable pages due to conservative pointers = %d (%d bytes)\n",
		num_dont_move_pages,
		num_dont_move_pages * PAGE_BYTES);
    }
#endif

    /* Scavenge all the rest of the roots. */

    /* Scavenge the Lisp functions of the interrupt handlers, taking
     * care to avoid SIG_DFL and SIG_IGN. */
    for_each_thread(th) {
	struct interrupt_data *data=th->interrupt_data;
    for (i = 0; i < NSIG; i++) {
	    union interrupt_handler handler = data->interrupt_handlers[i];
	if (!ARE_SAME_HANDLER(handler.c, SIG_IGN) &&
	    !ARE_SAME_HANDLER(handler.c, SIG_DFL)) {
		scavenge((lispobj *)(data->interrupt_handlers + i), 1);
	    }
	}
    }
    /* Scavenge the binding stacks. */
 {
     struct thread *th;
     for_each_thread(th) {
	 long len= (lispobj *)SymbolValue(BINDING_STACK_POINTER,th) -
	     th->binding_stack_start;
	 scavenge((lispobj *) th->binding_stack_start,len);
#ifdef LISP_FEATURE_SB_THREAD
	 /* do the tls as well */
	 len=fixnum_value(SymbolValue(FREE_TLS_INDEX,0)) -
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
	unsigned long read_only_space_size =
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
    for (i = 0; i < NUM_GENERATIONS; i++) {
	if ((i != generation) && (i != new_space)) {
	    scavenge_generation(i);
	}
    }

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
	long old_bytes_allocated = bytes_allocated;
	long bytes_allocated;

	/* Start with a full scavenge. */
	scavenge_newspace_generation_one_scan(new_space);

	/* Flush the current regions, updating the tables. */
	gc_alloc_update_all_page_tables();

	bytes_allocated = bytes_allocated - old_bytes_allocated;

	if (bytes_allocated != 0) {
	    lose("Rescan of new_space allocated %d more bytes.",
		 bytes_allocated);
	}
    }
#endif

    scan_weak_pointers();

    /* Flush the current regions, updating the tables. */
    gc_alloc_update_all_page_tables();

    /* Free the pages in oldspace, but not those marked dont_move. */
    bytes_freed = free_oldspace();

    /* If the GC is not raising the age then lower the generation back
     * to its normal generation number */
    if (!raise) {
	for (i = 0; i < last_free_page; i++)
	    if ((page_table[i].bytes_used != 0)
		&& (page_table[i].gen == NUM_GENERATIONS))
		page_table[i].gen = generation;
	gc_assert(generations[generation].bytes_allocated == 0);
	generations[generation].bytes_allocated =
	    generations[NUM_GENERATIONS].bytes_allocated;
	generations[NUM_GENERATIONS].bytes_allocated = 0;
    }

    /* Reset the alloc_start_page for generation. */
    generations[generation].alloc_start_page = 0;
    generations[generation].alloc_unboxed_start_page = 0;
    generations[generation].alloc_large_start_page = 0;
    generations[generation].alloc_large_unboxed_start_page = 0;

    if (generation >= verify_gens) {
	if (gencgc_verbose)
	    SHOW("verifying");
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
long
update_x86_dynamic_space_free_pointer(void)
{
    long last_page = -1;
    long i;

    for (i = 0; i < last_free_page; i++)
	if ((page_table[i].allocated != FREE_PAGE_FLAG)
	    && (page_table[i].bytes_used != 0))
	    last_page = i;

    last_free_page = last_page+1;

    SetSymbolValue(ALLOCATION_POINTER,
		   (lispobj)(((char *)heap_base) + last_free_page*PAGE_BYTES),0);
    return 0; /* dummy value: return something ... */
}

/* GC all generations newer than last_gen, raising the objects in each
 * to the next older generation - we finish when all generations below
 * last_gen are empty.  Then if last_gen is due for a GC, or if
 * last_gen==NUM_GENERATIONS (the scratch generation?  eh?) we GC that
 * too.  The valid range for last_gen is: 0,1,...,NUM_GENERATIONS.
 *
 * We stop collecting at gencgc_oldest_gen_to_gc, even if this is less than
 * last_gen (oh, and note that by default it is NUM_GENERATIONS-1) */
 
void
collect_garbage(unsigned last_gen)
{
    int gen = 0;
    int raise;
    int gen_to_wp;
    long i;

    FSHOW((stderr, "/entering collect_garbage(%d)\n", last_gen));

    if (last_gen > NUM_GENERATIONS) {
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
	print_generation_stats(0);

    do {
	/* Collect the generation. */

	if (gen >= gencgc_oldest_gen_to_gc) {
	    /* Never raise the oldest generation. */
	    raise = 0;
	} else {
	    raise =
		(gen < last_gen)
		|| (generations[gen].num_gc >= generations[gen].trigger_age);
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
	    print_generation_stats(0);
	}

	gen++;
    } while ((gen <= gencgc_oldest_gen_to_gc)
	     && ((gen < last_gen)
		 || ((gen <= gencgc_oldest_gen_to_gc)
		     && raise
		     && (generations[gen].bytes_allocated
			 > generations[gen].gc_trigger)
		     && (gen_av_mem_age(gen)
			 > generations[gen].min_av_mem_age))));

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
		lose("trying to write-protect gen. %d when gen. %d nonempty",
		     gen_to_wp, i);
	}
	write_protect_generation_pages(gen_to_wp);
    }

    /* Set gc_alloc() back to generation 0. The current regions should
     * be flushed after the above GCs. */
    gc_assert((boxed_region.free_pointer - boxed_region.start_addr) == 0);
    gc_alloc_generation = 0;

    update_x86_dynamic_space_free_pointer();
    auto_gc_trigger = bytes_allocated + bytes_consed_between_gcs;
    if(gencgc_verbose)
	fprintf(stderr,"Next gc when %ld bytes have been consed\n",
		auto_gc_trigger);
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
    long page;

    if (gencgc_verbose > 1)
	SHOW("entering gc_free_heap");

    for (page = 0; page < NUM_PAGES; page++) {
	/* Skip free pages which should already be zero filled. */
	if (page_table[page].allocated != FREE_PAGE_FLAG) {
	    void *page_start, *addr;

	    /* Mark the page free. The other slots are assumed invalid
	     * when it is a FREE_PAGE_FLAG and bytes_used is 0 and it
	     * should not be write-protected -- except that the
	     * generation is used for the current region but it sets
	     * that up. */
	    page_table[page].allocated = FREE_PAGE_FLAG;
	    page_table[page].bytes_used = 0;

	    /* Zero the page. */
	    page_start = (void *)page_address(page);

	    /* First, remove any write-protection. */
	    os_protect(page_start, PAGE_BYTES, OS_VM_PROT_ALL);
	    page_table[page].write_protected = 0;

	    os_invalidate(page_start,PAGE_BYTES);
	    addr = os_validate(page_start,PAGE_BYTES);
	    if (addr == NULL || addr != page_start) {
		lose("gc_free_heap: page moved, 0x%08x ==> 0x%08x",
		     page_start,
		     addr);
	    }
	} else if (gencgc_zero_check_during_free_heap) {
	    /* Double-check that the page is zero filled. */
	    long *page_start, i;
	    gc_assert(page_table[page].allocated == FREE_PAGE_FLAG);
	    gc_assert(page_table[page].bytes_used == 0);
	    page_start = (long *)page_address(page);
	    for (i=0; i<1024; i++) {
		if (page_start[i] != 0) {
		    lose("free region not zero at %x", page_start + i);
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
	print_generation_stats(0);

    /* Initialize gc_alloc(). */
    gc_alloc_generation = 0;

    gc_set_region_empty(&boxed_region);
    gc_set_region_empty(&unboxed_region);

    last_free_page = 0;
    SetSymbolValue(ALLOCATION_POINTER, (lispobj)((char *)heap_base),0);

    if (verify_after_free_heap) {
	/* Check whether purify has left any bad pointers. */
	if (gencgc_verbose)
	    SHOW("checking after free_heap\n");
	verify_gc();
    }
}

void
gc_init(void)
{
    long i;

    gc_init_tables();
    scavtab[SIMPLE_VECTOR_WIDETAG] = scav_vector;
    scavtab[WEAK_POINTER_WIDETAG] = scav_weak_pointer;
    transother[SIMPLE_ARRAY_WIDETAG] = trans_boxed_large;

    heap_base = (void*)DYNAMIC_SPACE_START;

    /* Initialize each page structure. */
    for (i = 0; i < NUM_PAGES; i++) {
	/* Initialize all pages as free. */
	page_table[i].allocated = FREE_PAGE_FLAG;
	page_table[i].bytes_used = 0;

	/* Pages are not write-protected at startup. */
	page_table[i].write_protected = 0;
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
	generations[i].bytes_consed_between_gc = 2000000;
	generations[i].trigger_age = 1;
	generations[i].min_av_mem_age = 0.75;
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
    long page = 0;
    long alloc_ptr = SymbolValue(ALLOCATION_POINTER,0);
    lispobj *prev=(lispobj *)page_address(page);

    do {
	lispobj *first,*ptr= (lispobj *)page_address(page);
	page_table[page].allocated = BOXED_PAGE_FLAG;
	page_table[page].gen = 0;
	page_table[page].bytes_used = PAGE_BYTES;
	page_table[page].large_object = 0;

	first=search_space(prev,(ptr+2)-prev,ptr);
	if(ptr == first)  prev=ptr; 
	page_table[page].first_object_offset =
	    (void *)prev - page_address(page);
	page++;
    } while (page_address(page) < alloc_ptr);

    generations[0].bytes_allocated = PAGE_BYTES*page;
    bytes_allocated = PAGE_BYTES*page;

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

char *
alloc(long nbytes)
{
    struct thread *th=arch_os_get_current_thread();
    struct alloc_region *region=
#ifdef LISP_FEATURE_SB_THREAD
	th ? &(th->alloc_region) : &boxed_region; 
#else
        &boxed_region; 
#endif
    void *new_obj;
    void *new_free_pointer;
    gc_assert(nbytes>0);
    /* Check for alignment allocation problems. */
    gc_assert((((unsigned)region->free_pointer & LOWTAG_MASK) == 0)
	      && ((nbytes & LOWTAG_MASK) == 0));
#if 0
    if(all_threads)
	/* there are a few places in the C code that allocate data in the
	 * heap before Lisp starts.  This is before interrupts are enabled,
	 * so we don't need to check for pseudo-atomic */
#ifdef LISP_FEATURE_SB_THREAD
	if(!SymbolValue(PSEUDO_ATOMIC_ATOMIC,th)) {
	    register u32 fs;
	    fprintf(stderr, "fatal error in thread 0x%x, pid=%d\n",
		    th,getpid());
	    __asm__("movl %fs,%0" : "=r" (fs)  : );
	    fprintf(stderr, "fs is %x, th->tls_cookie=%x \n",
		    debug_get_fs(),th->tls_cookie);
	    lose("If you see this message before 2004.01.31, mail details to sbcl-devel\n");
	}
#else
    gc_assert(SymbolValue(PSEUDO_ATOMIC_ATOMIC,th));
#endif
#endif
    
    /* maybe we can do this quickly ... */
    new_free_pointer = region->free_pointer + nbytes;
    if (new_free_pointer <= region->end_addr) {
	new_obj = (void*)(region->free_pointer);
	region->free_pointer = new_free_pointer;
	return(new_obj);	/* yup */
    }
    
    /* we have to go the long way around, it seems.  Check whether 
     * we should GC in the near future
     */
    if (auto_gc_trigger && bytes_allocated > auto_gc_trigger) {
	/* set things up so that GC happens when we finish the PA
	 * section.  We only do this if there wasn't a pending handler
	 * already, in case it was a gc.  If it wasn't a GC, the next
	 * allocation will get us back to this point anyway, so no harm done
	 */
	struct interrupt_data *data=th->interrupt_data;
	if(!data->pending_handler) 
	    maybe_defer_handler(interrupt_maybe_gc_int,data,0,0,0);
    }
    new_obj = gc_alloc_with_region(nbytes,0,region,0);
    return (new_obj);
}

/*
 * shared support for the OS-dependent signal handlers which
 * catch GENCGC-related write-protect violations
 */

void unhandled_sigmemoryfault(void);

/* Depending on which OS we're running under, different signals might
 * be raised for a violation of write protection in the heap. This
 * function factors out the common generational GC magic which needs
 * to invoked in this case, and should be called from whatever signal
 * handler is appropriate for the OS we're running under.
 *
 * Return true if this signal is a normal generational GC thing that
 * we were able to handle, or false if it was abnormal and control
 * should fall through to the general SIGSEGV/SIGBUS/whatever logic. */

int
gencgc_handle_wp_violation(void* fault_addr)
{
    long  page_index = find_page_index(fault_addr);

#ifdef QSHOW_SIGNALS
    FSHOW((stderr, "heap WP violation? fault_addr=%x, page_index=%d\n",
	   fault_addr, page_index));
#endif

    /* Check whether the fault is within the dynamic space. */
    if (page_index == (-1)) {

	/* It can be helpful to be able to put a breakpoint on this
	 * case to help diagnose low-level problems. */
	unhandled_sigmemoryfault();

	/* not within the dynamic space -- not our responsibility */
	return 0;

    } else {
	if (page_table[page_index].write_protected) {
	    /* Unprotect the page. */
	    os_protect(page_address(page_index), PAGE_BYTES, OS_VM_PROT_ALL);
	    page_table[page_index].write_protected_cleared = 1;
	    page_table[page_index].write_protected = 0;
	} else {  
	    /* The only acceptable reason for this signal on a heap
	     * access is that GENCGC write-protected the page.
	     * However, if two CPUs hit a wp page near-simultaneously,
	     * we had better not have the second one lose here if it
	     * does this test after the first one has already set wp=0
	     */
	    if(page_table[page_index].write_protected_cleared != 1) 
		lose("fault in heap page not marked as write-protected");
	}
	/* Don't worry, we can handle it. */
	return 1;
    }
}
/* This is to be called when we catch a SIGSEGV/SIGBUS, determine that
 * it's not just a case of the program hitting the write barrier, and
 * are about to let Lisp deal with it. It's basically just a
 * convenient place to set a gdb breakpoint. */
void
unhandled_sigmemoryfault()
{}

void gc_alloc_update_all_page_tables(void)
{
    /* Flush the alloc regions updating the tables. */
    struct thread *th;
    for_each_thread(th) 
        gc_alloc_update_page_tables(0, &th->alloc_region);
    gc_alloc_update_page_tables(1, &unboxed_region);
    gc_alloc_update_page_tables(0, &boxed_region);
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

