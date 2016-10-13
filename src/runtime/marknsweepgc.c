/*
 * Extension to GENCGC which provides for pages of objects
 * that are static in placement but subject to reclamation.
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
 * TODO:
 *  1. Space accounting (GET-BYTES-CONSED etc)
 *  2. Heuristic for auto-trigger. (Can't yet because no space accounting)
 *     Currently happens with regular GC trigger mechanism.
 *  3. Specify space size on startup
 *  4. De-fragment the space on save-lisp-and-die,
 *     but possibly also in out-of-space conditions.
 *     Fragmentation is typically not more than 5%, so this is not a huge issue.
 */

#include "gc.h"
#include "gc-internal.h"
#include "genesis/vector.h"

#include <stdlib.h>
#include <stdio.h>

#define FIRST_VARYOBJ_PAGE (IMMOBILE_FIXEDOBJ_SUBSPACE_SIZE/(int)IMMOBILE_CARD_BYTES)
#define WORDS_PER_PAGE ((int)IMMOBILE_CARD_BYTES/N_WORD_BYTES)
#define DOUBLEWORDS_PER_PAGE (WORDS_PER_PAGE/2)

#undef DEBUG
#undef VERIFY_PAGE_GENS

#ifdef DEBUG
#  define dprintf(arg) fprintf arg
FILE * logfile;
#else
#  define dprintf(arg)
#endif

low_page_index_t max_used_fixedobj_page, max_used_varyobj_page;

// This table is for objects fixed in size, as opposed to variable-sized.
// (Immobile objects are naturally fixed in placement)
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
} *fixedobj_pages;

unsigned int* immobile_scav_queue;
int immobile_scav_queue_head;
// Number of items enqueued; can exceed QCAPACITY on overflow.
// If overflowed, the queue is unusable until reset.
unsigned int immobile_scav_queue_count;
#define QCAPACITY (IMMOBILE_CARD_BYTES/sizeof(int))

#define gens attr.parts.gens_

// These are the high 2 bits of 'flags'
#define WRITE_PROTECT         0x80
#define WRITE_PROTECT_CLEARED 0x40

// Packing and unpacking attributes
// the low two flag bits are for write-protect status
#define MAKE_ATTR(spacing,size,flags) (((spacing)<<8)|((size)<<16)|flags)
#define OBJ_SPACING(attr) ((attr>>8) & 0xFF)

// Ignore the write-protect bits and the generations when comparing attributes
#define ATTRIBUTES_MATCH_P(page_attr,specified_attr) \
  ((page_attr & 0xFFFF3F) == specified_attr)
#define SET_WP_FLAG(index,flag) \
  fixedobj_pages[index].attr.parts.flags = (fixedobj_pages[index].attr.parts.flags & 0x3F) | flag

#define page_obj_align(i) fixedobj_pages[i].attr.parts.obj_align
#define page_obj_size(i) fixedobj_pages[i].attr.parts.obj_size
#define set_page_full(i) fixedobj_pages[i].free_index = IMMOBILE_CARD_BYTES
#define page_full_p(i) (fixedobj_pages[i].free_index >= (int)IMMOBILE_CARD_BYTES)
#define fixedobj_page_wp(i) (fixedobj_pages[i].attr.parts.flags & WRITE_PROTECT)

/// Variable-length pages:

// Array of inverted write-protect flags, 1 bit per page.
unsigned int* varyobj_page_touched_bits;
static int n_bitmap_elts; // length of array measured in 'int's

// Array of offsets backwards in double-lispwords from the page end
// to the lowest-addressed object touching the page. This offset can
// point to a hole, but we prefer that it not. If the offset is zero,
// the page has no object other than possibly a hole resulting
// from a freed object.
unsigned short* varyobj_page_scan_start_offset;

// Array of page generation masks
unsigned char* varyobj_page_header_gens;
// Holes to be stuffed back into the managed free list.
lispobj varyobj_holes;

#define VARYOBJ_PAGE_GENS(x) varyobj_page_header_gens[x-FIRST_VARYOBJ_PAGE]
#define varyobj_page_touched(x) \
  ((varyobj_page_touched_bits[(x-FIRST_VARYOBJ_PAGE)/32] >> (x&31)) & 1)

#ifdef VERIFY_PAGE_GENS
void check_fixedobj_page(low_page_index_t);
void check_varyobj_pages();
#endif

// Object header:  generation byte --|    |-- widetag
//                                   v    v
//                       0xzzzzzzzz GGzzzzww
//         arbitrary data  --------   ---- length in words
//
// There is a hard constraint on NUM_GENERATIONS, which is currently 8.
// (0..5=normal, 6=pseudostatic, 7=scratch)
// It could be as high as 16 for 32-bit words (wherein scratch=gen15)
// or 32 for 64-bits words (wherein scratch=gen31).
// In each case, the VISITED flag bit weight would need to be modified.
// Shifting a 1 bit left by the contents of the generation byte
// must not overflow a register.

#ifdef LISP_FEATURE_LITTLE_ENDIAN
static inline void assign_generation(lispobj* obj, generation_index_t gen)
{
    ((generation_index_t*)obj)[3] = gen;
}
// Turn a grey node black.
static inline void set_visited(lispobj* obj)
{
#ifdef DEBUG
    gc_assert(__immobile_obj_gen_bits(obj) == new_space);
#endif
    ((generation_index_t*)obj)[3] |= IMMOBILE_OBJ_VISITED_FLAG;
}
#else
#error "Need to define assign_generation() for big-endian"
#endif

static inline void *
low_page_address(low_page_index_t page_num)
{
    return ((void*)IMMOBILE_SPACE_START + (page_num * IMMOBILE_CARD_BYTES));
}

//// Variable-length utilities

/* Calculate the address where the first object touching this page starts. */
static inline lispobj*
varyobj_scan_start(low_page_index_t page_index)
{
    return (lispobj*)((char*)low_page_address(page_index+1)
                      - varyobj_page_scan_start_offset[page_index - FIRST_VARYOBJ_PAGE]
                        * (2 * N_WORD_BYTES));
}

/* Return the generation mask for objects headers on 'page_index'
   including at most one object that starts before the page but ends on
   or after it.
   If the scan start is within the page, i.e. less than DOUBLEWORDS_PER_PAGE
   (note that the scan start is measured relative to the page end) then
   we don't need to OR in the generation byte from an extra object,
   as all headers on the page are accounted for in the page generation mask.
   Also an empty page (where scan start is zero) avoids looking
   at the next page's first object by accident via the same test. */
unsigned char varyobj_page_gens_augmented(low_page_index_t page_index)
{
  return (varyobj_page_scan_start_offset[page_index - FIRST_VARYOBJ_PAGE] <= DOUBLEWORDS_PER_PAGE
          ? 0 : (1<<__immobile_obj_generation(varyobj_scan_start(page_index))))
         | VARYOBJ_PAGE_GENS(page_index);
}

//// Fixed-length object allocator

/* Return the index of an immobile page that is probably not totally full,
   starting with 'hint_page' and wrapping around.
   'attributes' determine an eligible page.
   *IMMOBILE-SPACE-FREE-POINTER* is updated to point beyond the found page
   if it previously did not. */

static int get_freeish_page(int hint_page, int attributes)
{
  int page = hint_page;
  lispobj new_free_pointer, old_free_pointer, actual_old;
  struct symbol * free_pointer_sym;
  int page_attr_packed;
  unsigned char best_genmask = 0xff;
  int best_page = -1;

  // Speed of this could be improved by keeping a linked list of pages
  // with any space available, headed by a field in the page struct.
  // This is totally lock-free / wait-free though, so it's really not
  // too shabby, because it never has to deal with a page-table mutex.
  do {
      page_attr_packed = fixedobj_pages[page].attr.packed;
      if (page_attr_packed == 0)
          if ((page_attr_packed =
               __sync_val_compare_and_swap(&fixedobj_pages[page].attr.packed,
                                           0, attributes)) == 0) {
              // Atomically assign MAX(old_free_pointer, new_free_pointer)
              // into the free pointer.
              free_pointer_sym = SYMBOL(IMMOBILE_FIXEDOBJ_FREE_POINTER);
              new_free_pointer = (lispobj)low_page_address(page+1);
              old_free_pointer = free_pointer_sym->value;
              while (new_free_pointer > old_free_pointer) {
                  actual_old =
                    __sync_val_compare_and_swap(&free_pointer_sym->value,
                                                old_free_pointer,
                                                new_free_pointer);
                  if (actual_old == old_free_pointer)
                      break;
                  old_free_pointer = actual_old;
              }
              return page;
          }
      if (ATTRIBUTES_MATCH_P(page_attr_packed, attributes)
          && !page_full_p(page)) {
          if (fixedobj_pages[page].gens <= 1) { // instant win
            return page;
          } else if (fixedobj_pages[page].gens < best_genmask) {
            best_genmask = fixedobj_pages[page].gens;
            best_page = page;
          }
      }
      if (++page >= FIRST_VARYOBJ_PAGE) page = 0;
  } while (page != hint_page);
  if (best_page >= 0)
      return best_page;
  lose("No more immobile pages available");
}

// Unused, but possibly will be for some kind of collision-avoidance scheme
// on claiming of new free pages.
long immobile_alloc_collisions;

/* Beginning at page index *hint, attempt to find space
   for an object on a page with page_attributes. Write its header word
   and return a C (native) pointer. The start page MUST have the proper
   characteristisc, but might be totally full.

   Precondition: Lisp has established a pseudo-atomic section. */

/* There is a slightly different algorithm that would probably be faster
   than what is currently implemented:
   - hint should be the address of a word that you try to claim
     as an object header; it moves from high-to-low instead of low-to-high.
     It's easier to compute the page base than the last valid object start
     if there are some wasted words at the end due to page size not being
     a perfect multiple of object size.
   - you do a CAS into that word, and either suceed or fail
   - if you succeed, subtract the object spacing and compare
     to the page's base address, which can be computed by
     masking. if the next address is above or equal to the page start,
     store it in the hint, otherwise mark the page full */

lispobj alloc_immobile_obj(int page_attributes, lispobj header, int* hint)
{
  int page;
  lispobj word;
  char * page_data, * obj_ptr, * next_obj_ptr, * limit, * next_free;
  int spacing_in_bytes = OBJ_SPACING(page_attributes) << WORD_SHIFT;

  page = *hint;
#ifdef DEBUG
  gc_assert(low_page_address(page) < (void*)SYMBOL(IMMOBILE_FIXEDOBJ_FREE_POINTER)->value);
#endif
  do {
      page_data = low_page_address(page);
      obj_ptr = page_data + fixedobj_pages[page].free_index;
      limit = page_data + IMMOBILE_CARD_BYTES - spacing_in_bytes;
      while (obj_ptr <= limit) {
          word = *(lispobj*)obj_ptr;
          next_obj_ptr = obj_ptr + spacing_in_bytes;
          if (fixnump(word) // a fixnum marks free space
              && __sync_bool_compare_and_swap((lispobj*)obj_ptr,
                                              word, header)) {
              // The value formerly in the header word was the offset to
              // the next hole. Use it to update the freelist pointer.
              // Just slam it in.
              fixedobj_pages[page].free_index = next_obj_ptr + word - page_data;
              return (lispobj)obj_ptr;
          }
          // If some other thread updated the free_index
          // to a larger value, use that. (See example below)
          next_free = page_data + fixedobj_pages[page].free_index;
          obj_ptr = next_free > next_obj_ptr ? next_free : next_obj_ptr;
      }
      set_page_full(page);
      page = get_freeish_page(page+1 >= FIRST_VARYOBJ_PAGE ? 0 : page+1,
                              page_attributes);
      *hint = page;
  } while (1);
}

/*
Example: Conside the freelist initially pointing to word index 6
Threads A, and B, and C each want to claim index 6.
- Thread A wins and then is switched out immediately after the CAS.
- Thread B fails to claim cell 6, claims cell 12 instead.
- Thread C fails to claim a cell and is switched out immediately
  after the CAS.
- Thread B writes the index of the next hole, cell 18 into the
  page's freelist cell.
- Thread A wakes up and writes 12 into the freelist cell.
- Thread C wakes up sees 12 for next_offset. 12 is greater than 6,
  so it sets its next probe location to 12.
  It fails the fixnump(header) test.
- Thread C sees that next_offset is still 12,
  so it skips by the page's object spacing instead, and will continue
  to do so until hitting the end of the page.
*/

//// The collector

void update_immobile_nursery_bits()
{
  low_page_index_t page;
  lispobj fixedobj_free_ptr = SYMBOL(IMMOBILE_FIXEDOBJ_FREE_POINTER)->value;
  lispobj varyobj_free_ptr = SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value;

  // Find the high water marks for this GC scavenge phase
  // [avoid passing exactly IMMOBILE_SPACE_END, which has no page index]
  max_used_fixedobj_page = find_immobile_page_index((void*)(fixedobj_free_ptr-1));
  max_used_varyobj_page = find_immobile_page_index((void*)(varyobj_free_ptr-1));

  immobile_scav_queue = (unsigned int*)low_page_address(max_used_varyobj_page+1);
  gc_assert((IMMOBILE_SPACE_END - (uword_t)immobile_scav_queue) / sizeof(int)
            >= QCAPACITY);

  // Unprotect the in-use ranges. Any page could be written during scavenge
  os_protect((os_vm_address_t)IMMOBILE_SPACE_START,
             fixedobj_free_ptr - IMMOBILE_SPACE_START,
             OS_VM_PROT_ALL);
  os_protect((os_vm_address_t)(IMMOBILE_VARYOBJ_SUBSPACE_START),
             varyobj_free_ptr - IMMOBILE_VARYOBJ_SUBSPACE_START,
             OS_VM_PROT_ALL);

  for (page=0; page <= max_used_fixedobj_page ; ++page) {
      // any page whose free index changed contains nursery objects
      if (fixedobj_pages[page].free_index >> WORD_SHIFT !=
          fixedobj_pages[page].prior_gc_free_word_index)
          fixedobj_pages[page].gens |= 1;
#ifdef VERIFY_PAGE_GENS
      check_fixedobj_page(page);
#endif
  }
#ifdef VERIFY_PAGE_GENS
  check_varyobj_pages();
#endif
}

#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
#define MAXIMUM_STRING_WIDETAG SIMPLE_CHARACTER_STRING_WIDETAG
#else
#define MAXIMUM_STRING_WIDETAG SIMPLE_BASE_STRING_WIDETAG
#endif

static inline boolean unboxed_obj_p(int widetag)
{
    // This is not an exhaustive test for unboxed objects,
    // but it's enough to avoid some unnecessary scavenging.
    return (widetag >= SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG
            && widetag <= MAXIMUM_STRING_WIDETAG
            && widetag != SIMPLE_VECTOR_WIDETAG);
}

/* Turn a white object grey. Also enqueue the object for re-scan */
void
promote_immobile_obj(lispobj *ptr) // a native pointer
{
    if (widetag_of(*ptr) == SIMPLE_FUN_HEADER_WIDETAG)
        ptr = (lispobj*)code_obj_from_simple_fun((struct simple_fun*)ptr);
    int pointerish = !unboxed_obj_p(widetag_of(*ptr));
    assign_generation(ptr, (pointerish ? 0 : IMMOBILE_OBJ_VISITED_FLAG) | new_space);
    low_page_index_t page_index = find_immobile_page_index(ptr);

    if (page_index >= FIRST_VARYOBJ_PAGE) {
        VARYOBJ_PAGE_GENS(page_index) |= 1<<new_space;
    } else {
        fixedobj_pages[page_index].gens |= 1<<new_space;
    }
    // Do nothing if either we don't need to look for pointers in this object,
    // or the work queue has already overflowed, causing a full scan.
    if (!pointerish || immobile_scav_queue_count > QCAPACITY) return;

    // count is either less than or equal to QCAPACITY.
    // If equal, just bump the count to signify overflow.
    if (immobile_scav_queue_count < QCAPACITY) {
        immobile_scav_queue[immobile_scav_queue_head] =
            (uword_t)ptr & 0xFFFFFFFF; // Drop the high bits
        immobile_scav_queue_head = (immobile_scav_queue_head + 1) & (QCAPACITY - 1);
    }
    ++immobile_scav_queue_count;
}

/* If 'addr' points to an immobile object, then make the object
   live by promotion. But if the object is not in the generation
   being collected, do nothing */
void immobile_space_preserve_pointer(void* addr)
{
    low_page_index_t page_index = find_immobile_page_index(addr);
    if (page_index < 0)
        return;

    lispobj* header_addr;
    if (page_index >= FIRST_VARYOBJ_PAGE) {
        // Restrict addr to lie below IMMOBILE_SPACE_FREE_POINTER.
        // This way, if the gens byte is nonzero but there is
        // a final array acting as filler on the remainder of the
        // final page, we won't accidentally find that.
        lispobj* start;
        if ((lispobj)addr >= SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value
            || !(varyobj_page_gens_augmented(page_index) & (1<<from_space))
            || (start = varyobj_scan_start(page_index)) > (lispobj*)addr)
            return;
        header_addr = gc_search_space(start,
                                      native_pointer((lispobj)addr)+2 - start,
                                      (lispobj*)addr);
        if (!header_addr || immobile_filler_p(header_addr))
            return;
        gc_assert(other_immediate_lowtag_p(*header_addr));
    } else if (fixedobj_pages[page_index].gens & (1<<from_space)) {
        int obj_spacing = (page_obj_align(page_index) << WORD_SHIFT);
        int obj_index = ((uword_t)addr & (IMMOBILE_CARD_BYTES-1)) / obj_spacing;
        dprintf((logfile,"Pointer %p is to immobile page %d, object %d\n",
                 addr, page_index, obj_index));
        char* page_start_addr = (char*)((uword_t)addr & ~(IMMOBILE_CARD_BYTES-1));
        header_addr = (lispobj*)(page_start_addr + obj_index * obj_spacing);
        if (fixnump(*header_addr))
            return;
    } else {
        return;
    }
    if (__immobile_obj_gen_bits(header_addr) == from_space) {
        dprintf((logfile,"immobile obj @ %p is conservatively live\n", addr));
        promote_immobile_obj(header_addr);
    }
}

// Loop over the newly-live objects, scavenging them for pointers.
// As with the ordinary gencgc algorithm, this uses almost no stack.
static void full_scavenge_immobile_newspace()
{
    page_index_t page;
    unsigned char bit = 1<<new_space;

    // Fixed-size object pages.

    for (page = 0; page <= max_used_fixedobj_page; ++page) {
        if (!(fixedobj_pages[page].gens & bit)) continue;
        // Skip amount within the loop is in bytes.
        int obj_spacing = page_obj_align(page) << WORD_SHIFT;
        int n_words     = page_obj_size(page);
        lispobj* obj    = low_page_address(page);
        lispobj* limit  = (lispobj*)((char*)obj +
                                     IMMOBILE_CARD_BYTES - obj_spacing);
        for ( ; obj <= limit ; obj = (lispobj*)((char*)obj + obj_spacing) ) {
            if (!fixnump(*obj) && __immobile_obj_gen_bits(obj) == new_space) {
                set_visited(obj);
                scavenge(obj, n_words);
            }
        }
    }

    // Variable-size object pages

    page = FIRST_VARYOBJ_PAGE - 1; // Subtract 1 because of pre-increment
    while (1) {
        // Find the next page with anything in newspace.
        do {
            if (++page > max_used_varyobj_page) return;
        } while ((VARYOBJ_PAGE_GENS(page) & bit) == 0);
        lispobj* obj = varyobj_scan_start(page);
        do {
            lispobj* limit = (lispobj*)low_page_address(page) + WORDS_PER_PAGE;
            int widetag, n_words;
            for ( ; obj < limit ; obj += n_words ) {
                n_words = sizetab[widetag = widetag_of(*obj)](obj);
                if (__immobile_obj_gen_bits(obj) == new_space) {
                    set_visited(obj);
                    scavenge(obj, n_words);
                }
            }
            page = find_immobile_page_index(obj);
            // Bail out if exact absolute end of immobile space was reached.
            if (page < 0) return;
            // If 'page' should be scanned, then pick up where we left off,
            // without recomputing 'obj' but setting a higher 'limit'.
        } while (VARYOBJ_PAGE_GENS(page) & bit);
    }
}

/// Repeatedly scavenge immobile newspace work queue until we find no more
/// reachable objects within. (They might be in dynamic space though).
/// If queue overflow already happened, then a worst-case full scan is needed.
/// If it didn't, we try to drain the queue, hoping that overflow does
/// not happen while doing so.
/// The approach taken is more subtle than just dequeuing each item,
/// scavenging, and letting the outer 'while' loop take over.
/// That would be ok, but could cause more full scans than necessary.
/// Instead, since each entry in the queue is useful information
/// in the non-overflow condition, perform all the work indicated thereby,
/// rather than considering the queue discardable as soon as overflow happens.
/// Essentially we just have to capture the valid span of enqueued items,
/// because the queue state is inconsistent when 'count' exceeds 'capacity'.
void scavenge_immobile_newspace()
{
  while (immobile_scav_queue_count) {
      if (immobile_scav_queue_count > QCAPACITY) {
          immobile_scav_queue_count = 0;
          full_scavenge_immobile_newspace();
      } else {
          int queue_index_from = (immobile_scav_queue_head - immobile_scav_queue_count)
                               & (QCAPACITY - 1);
          int queue_index_to   = immobile_scav_queue_head;
          int i = queue_index_from;
          // The termination condition can't be expressed as an inequality,
          // since the indices might be reversed due to wraparound.
          // To express as equality entails forcing at least one iteration
          // since the ending index might be the starting index.
          do {
              lispobj* obj = (lispobj*)(uword_t)immobile_scav_queue[i];
              i = (1 + i) & (QCAPACITY-1);
              // Only decrement the count if overflow did not happen.
              // The first iteration of this loop will decrement for sure,
              // but subsequent iterations might not.
              if (immobile_scav_queue_count <= QCAPACITY)
                  --immobile_scav_queue_count;
              set_visited(obj);
              scavenge(obj, sizetab[widetag_of(*obj)](obj));
          } while (i != queue_index_to);
      }
  }
}

// Return a page >= page_index having potential old->young pointers,
// or -1 if there isn't one.
static int next_varyobj_root_page(unsigned int page_index,
                                  unsigned char genmask)
{
    int map_index = (page_index - FIRST_VARYOBJ_PAGE) / 32;
    int bit_index = page_index & 31;
    if (map_index >= n_bitmap_elts) return -1;
    // Look only at bits of equal or greater weight than bit_index.
    unsigned int word = (0xFFFFFFFF << bit_index) & varyobj_page_touched_bits[map_index];
    while (1) {
        if (word) {
            bit_index = ffs(word) - 1;
            page_index = FIRST_VARYOBJ_PAGE + map_index * 32 + bit_index;
            if (varyobj_page_gens_augmented(page_index) & genmask)
                return page_index;
            else {
                word ^= (1<<bit_index);
                continue;
            }
        }
        if (++map_index >= n_bitmap_elts) return -1;
        word = varyobj_page_touched_bits[map_index];
    }
}

void
scavenge_immobile_roots(generation_index_t min_gen, generation_index_t max_gen)
{
    // When raise = 1, new_space is also among the root generations.
    // To distinguish new_space objects that were already seen - meaning their
    // contained pointers were walked - versus not seen,
    // we mark the objects scavenged on this pass as visited.
    // This essentialy distinguishes grey from black in the tri-color scheme.
    int mark_visited = new_space == min_gen;

    // example: scavenging gens 2..6, the mask of root gens is #b1111100
    int genmask = ((1 << (max_gen - min_gen + 1)) - 1) << min_gen;

    low_page_index_t page;
    for (page = 0; page <= max_used_fixedobj_page ; ++page) {
        if (fixedobj_page_wp(page) || !(fixedobj_pages[page].gens & genmask))
            continue;
        int obj_spacing = page_obj_align(page) << WORD_SHIFT;
        int n_words = page_obj_size(page);
        lispobj* obj = low_page_address(page);
        lispobj* limit = (lispobj*)((char*)obj +
                                    IMMOBILE_CARD_BYTES - obj_spacing);
        int gen;
        // Immobile space can only contain objects with a header word,
        // no conses, so any fixnum where a header could be is not a live
        // object. The scan loop is expanded twice, once for mark_visited
        // and once for not, to avoid an extra test when 'raise=0'
#define SCAN(mark_it) \
        do { \
            if (!fixnump(*obj) && (genmask >> (gen=__immobile_obj_gen_bits(obj)) & 1)) { \
                if (gen == new_space) { mark_it; } \
                scavenge(obj, n_words); \
            } \
        } while ((obj = (lispobj*)((char*)obj + obj_spacing)) <= limit)
        if (mark_visited) SCAN(set_visited(obj)); else SCAN();
    }
#undef SCAN

    // Variable-length object pages
    page = next_varyobj_root_page(FIRST_VARYOBJ_PAGE, genmask);
    while (page >= 0) {
        lispobj* obj = varyobj_scan_start(page);
        do {
            lispobj* limit = (lispobj*)low_page_address(page) + WORDS_PER_PAGE;
            int widetag, n_words, gen;
#define SCAN(mark_it) \
            for ( ; obj < limit ; obj += n_words ) { \
                n_words = sizetab[widetag = widetag_of(*obj)](obj); \
                if (genmask >> (gen=__immobile_obj_gen_bits(obj)) & 1) { \
                    if (gen == new_space) { mark_it; } \
                    scavenge(obj, n_words); \
                 } \
            }
            if (mark_visited) SCAN(set_visited(obj)) else SCAN();
            page = find_immobile_page_index(obj);
        } while (page > 0
                 && (VARYOBJ_PAGE_GENS(page) & genmask)
                 && varyobj_page_touched(page));
        page = next_varyobj_root_page(1+page, genmask);
    }
#undef SCAN
    scavenge_immobile_newspace();
}

#include "genesis/layout.h"
#define LAYOUT_SIZE (sizeof (struct layout)/N_WORD_BYTES)

// As long as Lisp doesn't have any native allocators (vops and whatnot)
// it doesn't need to access these values.
int layout_page_hint, symbol_page_hint, fdefn_page_hint;

// For the three different page characteristics that we need,
// claim a page that works for those characteristics.
void set_immobile_space_hints()
{
  // The allocator doesn't check whether each 'hint' points to an
  // expected kind of page, so we have to ensure up front that
  // allocations start on different pages. i.e. You can point to
  // a totally full page, but you can't point to a wrong page.
  // It doesn't work to just assign these to consecutive integers
  // without also updating the page attributes.

  // Object sizes must be multiples of 2 because the n_words value we pass
  // to scavenge() is gotten from the page attributes, and scavenge asserts
  // that the ending address is aligned to a doubleword boundary as expected.

  // LAYOUTs are 256-byte-aligned so that the low byte contains no information.
  // This makes it possible to recover a layout pointer from an instance header
  // by simply changing the low byte to instance-pointer-lowtag.
  // As a test of objects using larger-than-required alignment,
  // the 64-bit implementation uses 256-byte alignment for layouts,
  // even though the header can store all bits of the layout pointer.
  // The 32-bit implementation would also need somewhere different to store
  // the generation byte of each layout, which is a minor annoyance.
  layout_page_hint = get_freeish_page(0, MAKE_ATTR(256/N_WORD_BYTES, // spacing
                                                   CEILING(LAYOUT_SIZE,2),
                                                   0));
  symbol_page_hint = get_freeish_page(0, MAKE_ATTR(CEILING(SYMBOL_SIZE,2),
                                                   CEILING(SYMBOL_SIZE,2),
                                                   0));
  fdefn_page_hint = get_freeish_page(0, MAKE_ATTR(CEILING(FDEFN_SIZE,2),
                                                  CEILING(FDEFN_SIZE,2),
                                                  0));
}

static void protect_space()
{
    int max;

    set_immobile_space_hints();
    // find the highest page in use
    for (max = FIRST_VARYOBJ_PAGE-1 ; max >= 0 ; --max)
        if (fixedobj_pages[max].attr.parts.obj_size)
            break;
    max_used_fixedobj_page = max; // this is a page index, not the number of pages.
    SYMBOL(IMMOBILE_FIXEDOBJ_FREE_POINTER)->value =
        IMMOBILE_SPACE_START + IMMOBILE_CARD_BYTES*(1+max);

    for (max = (IMMOBILE_SPACE_SIZE/IMMOBILE_CARD_BYTES)-1 ;
         max >= FIRST_VARYOBJ_PAGE ; --max)
        if (VARYOBJ_PAGE_GENS(max))
            break;
    max_used_varyobj_page = max; // this is a page index, not the number of pages.

    dprintf((logfile, "Protect space: set max used page to %d. end=%lx\n",
             max, IMMOBILE_SPACE_START + IMMOBILE_CARD_BYTES*(1+max)));

    // Now find contiguous ranges of pages that are protectable,
    // minimizing the number of system calls as much as possible.
    int i, start = -1, end = -1; // inclusive bounds on page indices
    for (i = max_used_fixedobj_page ; i >= 0 ; --i) {
        if (fixedobj_page_wp(i)) {
            if (end < 0) end = i;
            start = i;
        }
        if (end >= 0 && (!fixedobj_page_wp(i) || i == 0)) {
            os_protect(low_page_address(start),
                       IMMOBILE_CARD_BYTES * (1 + end - start),
                       OS_VM_PROT_READ|OS_VM_PROT_EXECUTE);
            start = end = -1;
        }
    }
#define varyobj_page_wp(x) !varyobj_page_touched(x)
    for (i = max_used_varyobj_page ; i >= FIRST_VARYOBJ_PAGE ; --i) {
        if (varyobj_page_wp(i)) {
            if (end < 0) end = i;
            start = i;
        }
        if (end >= 0 && (!varyobj_page_wp(i) || i == FIRST_VARYOBJ_PAGE)) {
            os_protect(low_page_address(start),
                       IMMOBILE_CARD_BYTES * (1 + end - start),
                       OS_VM_PROT_READ|OS_VM_PROT_EXECUTE);
            start = end = -1;
        }
    }
#undef varyobj_page_wp
}

// Scan range between start and end (exclusive) for old-to-young pointers.
// 'keep_gen' is the value of the generation byte of objects that were
// candidates to become garbage, but remain live after this gc.
// It will necessarily have the VISITED flag on.
// 'new_gen' is the generation number that those objects will have
// after collection, which is either the same generation or one higher,
// depending on the 'raise' flag for this GC cycle.
static int
range_points_to_younger_p(lispobj* obj, lispobj* end,
                          int gen, int keep_gen, int new_gen)
{
#ifdef DEBUG
  lispobj* __attribute__((unused)) saved_obj = obj, __attribute__((unused)) header = *obj;
#endif
    do {
        lispobj thing = *obj;
        if (is_lisp_pointer(thing)) {
            int to_page = find_page_index((void*)thing),
                to_gen = 255;
            if (to_page >= 0) { // points to ordinary dynamic space
                to_gen = page_table[to_page].gen;
                if (to_gen == PSEUDO_STATIC_GENERATION+1) // scratch gen
                    to_gen = new_gen; // is actually this
            } else if (immobile_space_p(thing)) {
                // Processing the code-entry-points slot of a code component
                // requires the general variant of immobile_obj_gen_bits
                // because the pointed-to object is a simple-fun.
                to_gen = immobile_obj_gen_bits(native_pointer(thing));
                if (to_gen == keep_gen) // keep gen
                    to_gen = new_gen; // is actually this
            }
            if (to_gen < gen) {
                return 1; // yes, points to younger
            }
        }
    } while (++obj < end);
    return 0; // no, does not point to younger
}

// Scan a fixed-size object for old-to-young pointers.
// Since fixed-size objects are boxed and on known boundaries,
// we never start in the middle of random bytes, so the answer is exact.
static inline boolean
fixedobj_points_to_younger_p(lispobj* obj, int n_words,
                             int gen, int keep_gen, int new_gen)
{
    return range_points_to_younger_p(obj+1, obj+n_words,
                                     gen, keep_gen, new_gen);
}

static boolean
varyobj_points_to_younger_p(lispobj* obj, int gen, int keep_gen, int new_gen,
                            os_vm_address_t page_begin,
                            os_vm_address_t page_end) // upper (exclusive) bound
{
    lispobj *begin, *end, word = *obj;
    unsigned char widetag = widetag_of(word);
    if (widetag == CODE_HEADER_WIDETAG) { // usual case. Like scav_code_header()
        lispobj entry_point; /* tagged pointer to entry point */
        struct simple_fun *function_ptr; /* untagged pointer to entry point */
        for (entry_point = ((struct code*)obj)->entry_points;
             entry_point != NIL;
             entry_point = function_ptr->next) {
            function_ptr = (struct simple_fun *) native_pointer(entry_point);
            begin = SIMPLE_FUN_SCAV_START(function_ptr);
            end   = begin + SIMPLE_FUN_SCAV_NWORDS(function_ptr);
            if (page_begin > (os_vm_address_t)begin) begin = (lispobj*)page_begin;
            if (page_end   < (os_vm_address_t)end)   end   = (lispobj*)page_end;
            if (end > begin
                && range_points_to_younger_p(begin, end, gen, keep_gen, new_gen))
                return 1;
        }
        begin = obj;
        end = obj + code_header_words(word); // exclusive bound
    } else if (widetag == SIMPLE_VECTOR_WIDETAG) {
        sword_t length = fixnum_value(((struct vector *)obj)->length);
        begin = obj;
        end = obj + CEILING(length + 2, 2);
    } else if (widetag >= SIMPLE_ARRAY_UNSIGNED_BYTE_2_WIDETAG &&
               widetag <= MAXIMUM_STRING_WIDETAG) {
        return 0;
    } else {
        lose("Unexpected widetag @ %p", obj);
    }
    // Fallthrough: scan words from begin to end
    if (page_begin > (os_vm_address_t)begin) begin = (lispobj*)page_begin;
    if (page_end   < (os_vm_address_t)end)   end   = (lispobj*)page_end;
    if (end > begin && range_points_to_younger_p(begin, end, gen, keep_gen, new_gen))
        return 1;
    return 0;
}

// If 'page' does not contain any objects that points to an object
// younger than themselves, then return true.
// This is called on pages that do not themselves contain objects of
// the generation being collected, but might contain pointers
// to younger generations, which we detect by a cleared WP status bit.
// The bit is cleared on any write, though, even of a non-pointer,
// so this unfortunately has to be tested much more often than we'd like.
static inline boolean can_wp_fixedobj_page(page_index_t page, int keep_gen, int new_gen)
{
    int obj_spacing = page_obj_align(page) << WORD_SHIFT;
    int obj_size_words = page_obj_size(page);
    lispobj* obj = low_page_address(page);
    lispobj* limit = (lispobj*)((char*)obj + IMMOBILE_CARD_BYTES - obj_spacing);
    for ( ; obj <= limit ; obj = (lispobj*)((char*)obj + obj_spacing) )
        if (!fixnump(*obj) && // an object header
            fixedobj_points_to_younger_p(obj, obj_size_words,
                                         __immobile_obj_generation(obj),
                                         keep_gen, new_gen))
            return 0;
    return 1;
}

// To scan _only_ 'page' is impossible in general, but we can act like only
// one page was scanned by backing up to the first object whose end is on
// or after it, and then restricting points_to_younger within the boundaries.
// Doing it this way is probably much better than conservatively assuming
// that any word satisfying is_lisp_pointer() is a pointer.
static inline boolean can_wp_varyobj_page(page_index_t page, int keep_gen, int new_gen)
{
    lispobj *begin = (lispobj*)low_page_address(page);
    lispobj *end   = begin + WORDS_PER_PAGE;
    lispobj *obj   = varyobj_scan_start(page);
    for ( ; obj < end ; obj += sizetab[widetag_of(*obj)](obj) ) {
        gc_assert(other_immediate_lowtag_p(*obj));
        if (!immobile_filler_p(obj) &&
            varyobj_points_to_younger_p(obj,
                                        __immobile_obj_generation(obj),
                                        keep_gen, new_gen,
                                        (os_vm_address_t)begin,
                                        (os_vm_address_t)end))
            return 0;
    }
    return 1;
}

/*
  Sweep immobile space by zeroing the memory of trashed objects
  and linking them into the freelist.

  Possible improvements:
  - If an entire page becomes nothing but holes, we could bzero it
    instead of object-at-a-time clearing. But it's not known to be
    so until after the sweep, so it would entail two passes per page,
    one to mark holes and one to zero them.
  - And perhaps bzero could be used on ranges of holes, because
    in that case each hole's pointer to the next hole is zero as well.
*/

#define SETUP_GENS()                                                   \
  /* Only care about pages with something in old or new space. */      \
  int relevant_genmask = (1 << from_space) | (1 << new_space);         \
  /* Objects whose gen byte is 'keep_gen' are alive. */                \
  int keep_gen = IMMOBILE_OBJ_VISITED_FLAG | new_space;                \
  /* Objects whose gen byte is 'from_space' are trash. */              \
  int discard_gen = from_space;                                        \
  /* Moving non-garbage into either 'from_space' or 'from_space+1' */  \
  generation_index_t new_gen = from_space + (raise!=0)

// The new value of the page generation mask is computed as follows:
// If 'raise' = 1 then:
//     Nothing resides in 'from_space', and 'from_space+1' gains new objects
//     if and only if any objects on the page were retained.
// If 'raise' = 0 then:
//     Nothing resides in the scratch generation, and 'from_space'
//     has objects if and only if any objects were retained.
#define COMPUTE_NEW_MASK(var, old) \
  int var = old & ~(1<<from_space); \
  if ( raise ) \
    var |= 1<<(from_space+1) & any_kept; \
  else \
    var = (var & ~(1<<new_space)) | (1<<from_space & any_kept)

static void
sweep_fixedobj_pages(int raise)
{
    char *page_base;
    lispobj *obj, *limit, *hole;
    // This will be needed for space accounting.
    // threads might fail to consume all the space on a page.
    // By storing in the page table the count of holes that really existed
    // at the start of the prior GC, and subtracting from that the number
    // that exist now, we know how much usable space was obtained (per page).
    int n_holes = 0;
    int word_idx;

    SETUP_GENS();

    low_page_index_t page;
    for (page = 0; page <= max_used_fixedobj_page; ++page) {
        // On pages that won't need manipulation of the freelist,
        // we try to do less work than for pages that need it.
        if (!(fixedobj_pages[page].gens & relevant_genmask)) {
            // Scan for old->young pointers, and WP if there are none.
            if (!fixedobj_page_wp(page) && fixedobj_pages[page].gens > 1
                && can_wp_fixedobj_page(page, keep_gen, new_gen))
                SET_WP_FLAG(page, WRITE_PROTECT);
            continue;
        }
        int obj_spacing = page_obj_align(page) << WORD_SHIFT;
        int obj_size_words = page_obj_size(page);
        page_base = low_page_address(page);
        limit = (lispobj*)(page_base + IMMOBILE_CARD_BYTES - obj_spacing);
        obj = (lispobj*)page_base;
        hole = NULL;
        int any_kept = 0; // was anything moved to the kept generation
        n_holes = 0;

        // wp_it is 1 if we should try to write-protect it now.
        // If already write-protected, skip the tests.
        int wp_it = !fixedobj_page_wp(page);
        int gen;
        for ( ; obj <= limit ; obj = (lispobj*)((char*)obj + obj_spacing) ) {
            if (fixnump(*obj)) { // was already a hole
            trash_it:
                // re-link it into the new freelist
                if (hole)
                    // store the displacement from the end of the object
                    // at prev_hole to the start of this object.
                    *hole = (lispobj)((char*)obj - ((char*)hole + obj_spacing));
                else // this is the first seen hole on the page
                    // record the byte offset to that hole
                  fixedobj_pages[page].free_index = (char*)obj - page_base;
                hole = obj;
                n_holes ++;
            } else if ((gen = __immobile_obj_gen_bits(obj)) == discard_gen) { // trash
                for (word_idx=obj_size_words-1 ; word_idx > 0 ; --word_idx)
                    obj[word_idx] = 0;
                goto trash_it;
            } else if (gen == keep_gen) {
                assign_generation(obj, gen = new_gen);
#ifdef DEBUG
                gc_assert(!fixedobj_points_to_younger_p(obj, obj_size_words,
                                                        gen, keep_gen, new_gen));
#endif
                any_kept = -1;
            } else if (wp_it && fixedobj_points_to_younger_p(obj, obj_size_words,
                                                             gen, keep_gen, new_gen))
              wp_it = 0;
        }
        if ( hole ) // terminate the chain of holes
            *hole = (lispobj)((char*)obj - ((char*)hole + obj_spacing));
        fixedobj_pages[page].prior_gc_free_word_index =
          fixedobj_pages[page].free_index >> WORD_SHIFT;

        COMPUTE_NEW_MASK(mask, fixedobj_pages[page].gens);
        if ( mask ) {
            fixedobj_pages[page].gens = mask;
            if (wp_it) {
                SET_WP_FLAG(page, WRITE_PROTECT);
                dprintf((logfile, "Lowspace: set WP on page %d\n", page));
            }
        } else {
            dprintf((logfile,"page %d is all garbage\n", page));
            fixedobj_pages[page].attr.packed = 0;
        }
#ifdef DEBUG
        verify_fixedobj_page(page);
#endif
        dprintf((logfile,"page %d: %d holes\n", page, n_holes));
    }
}

// Scan for freshly trashed objects and turn them into filler.
// Lisp is responsible for consuming the free space
// when it next allocates a variable-size object.
static void
sweep_varyobj_pages(int raise)
{
    SETUP_GENS();

    lispobj* free_pointer = (lispobj*)SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value;
    low_page_index_t page;
    for (page = FIRST_VARYOBJ_PAGE; page <= max_used_varyobj_page; ++page) {
        int genmask = VARYOBJ_PAGE_GENS(page);
        if (!(genmask & relevant_genmask)) { // Has nothing in oldspace or newspace.
            // Scan for old->young pointers, and WP if there are none.
            if (varyobj_page_touched(page)
                && varyobj_page_gens_augmented(page) > 1
                && can_wp_varyobj_page(page, keep_gen, new_gen))
                varyobj_page_touched_bits[(page - FIRST_VARYOBJ_PAGE)/32] &= ~(1<<(page & 31));
            continue;
        }
        lispobj* page_base = (lispobj*)low_page_address(page);
        lispobj* limit = page_base + WORDS_PER_PAGE;
        if (limit > free_pointer) limit = free_pointer;
        // An object whose tail is on this page or which spans this
        // page would have been dealt with by the page holding its header,
        // so only objects whose headers are on this page
        // are candidates to become garbage now.
        lispobj* obj = varyobj_scan_start(page);
        if (obj < page_base) {
            obj += sizetab[widetag_of(*obj)](obj);
            // obj can't hop over this page. If it did, there would be no
            // headers on the page, and genmask would have been zero.
            gc_assert(obj < limit);
        }
        int any_kept = 0; // was anything moved to the kept generation
        // wp_it is 1 if we should try to write-protect it now.
        // If already write-protected, skip the tests.
        int wp_it = varyobj_page_touched(page);
        int size;
        for ( ; obj < limit ; obj += size ) {
            lispobj word = *obj;
            size = sizetab[widetag_of(word)](obj);
            int gen;
            if (immobile_filler_p(obj)) { // do nothing
            } else if ((gen = __immobile_obj_gen_bits(obj)) == discard_gen) {
                if (size < 4)
                    lose("immobile object @ %p too small to free", obj);
                else { // Create a filler object.
                    struct code* code  = (struct code*)obj;
                    code->header       = 2<<N_WIDETAG_BITS | CODE_HEADER_WIDETAG;
                    code->code_size    = make_fixnum((size - 2) * N_WORD_BYTES);
                    code->entry_points = NIL;
                    code->debug_info   = varyobj_holes;
                    varyobj_holes      = (lispobj)code;
                }
            } else if (gen == keep_gen) {
                assign_generation(obj, gen = new_gen);
#ifdef DEBUG
                gc_assert(!varyobj_points_to_younger_p(obj, gen, keep_gen, new_gen,
                                                       (os_vm_address_t)page_base,
                                                       (os_vm_address_t)limit));
#endif
                any_kept = -1;
            } else if (wp_it &&
                       varyobj_points_to_younger_p(obj, gen, keep_gen, new_gen,
                                                   (os_vm_address_t)page_base,
                                                   (os_vm_address_t)limit))
                wp_it = 0;
        }
        COMPUTE_NEW_MASK(mask, VARYOBJ_PAGE_GENS(page));
        VARYOBJ_PAGE_GENS(page) = mask;
        if ( mask && wp_it )
            varyobj_page_touched_bits[(page - FIRST_VARYOBJ_PAGE)/32] &= ~(1 << (page & 31));
    }
}

// TODO: (Maybe this won't work. Not sure yet.) rather than use the
// same 'raise' concept as in gencgc, each immobile object can store bits
// indicating whether it has survived any GC at its current generation.
// If it has, then it gets promoted next time, rather than all or nothing
// being promoted from the generation getting collected.
void
sweep_immobile_space(int raise)
{
  gc_assert(immobile_scav_queue_count == 0);
  sweep_fixedobj_pages(raise);
  sweep_varyobj_pages(raise);
  protect_space();
  immobile_scav_queue = NULL;
  immobile_scav_queue_head = 0;
}

void gc_init_immobile()
{
#ifdef DEBUG
    logfile = stderr;
#endif
    int n_fixedobj_pages = FIRST_VARYOBJ_PAGE;
    int n_varyobj_pages = (IMMOBILE_SPACE_SIZE - IMMOBILE_FIXEDOBJ_SUBSPACE_SIZE)
                          / IMMOBILE_CARD_BYTES;
    fixedobj_pages = calloc(n_fixedobj_pages, sizeof(struct fixedobj_page));
    gc_assert(fixedobj_pages);

    n_bitmap_elts = (n_varyobj_pages + 31) / 32;
    int request = n_bitmap_elts * sizeof (int)
                + n_varyobj_pages * (sizeof (short)+sizeof (char));
    char* varyobj_page_tables = malloc(request);
    gc_assert(varyobj_page_tables);
    memset(varyobj_page_tables, 0, request);
    varyobj_page_touched_bits = (unsigned int*)varyobj_page_tables;
    varyobj_page_scan_start_offset = (unsigned short*)(varyobj_page_touched_bits + n_bitmap_elts);
    varyobj_page_header_gens = (unsigned char*)(varyobj_page_scan_start_offset + n_varyobj_pages);
}

/* Because the immobile page table is not dumped into a core image,
   we have to reverse-engineer the characteristics of each page,
   which means figuring out what the object spacing should be.
   This is not difficult, but is a bit of a kludge */

static inline int immobile_obj_spacing(lispobj header_word, lispobj *obj,
                                       int actual_size)
{
  lispobj this_layout, layout_layout;

  // 64-bit build does not need to align layouts on 256-byte boundary.
  // But this is a proof-of-concept that should work on 32-bit build,
  // which would need the alignment if compact instance headers are used.
  if (widetag_of(header_word)==INSTANCE_HEADER_WIDETAG) {
    this_layout = instance_layout(obj);
    layout_layout = instance_layout(native_pointer(this_layout));
    // If this object's layout is layout-of-layout, then this is a layout,
    // hence this page must have object spacing of 256 bytes.
    if (this_layout == layout_layout)
        return 256 / N_WORD_BYTES;
  }
  return actual_size; // in words
}

// Set the characteristics of each used page at image startup time.
void immobile_space_coreparse(uword_t address, uword_t len)
{
    int n_pages, word_idx, page;

    n_pages = (len + IMMOBILE_CARD_BYTES - 1) / IMMOBILE_CARD_BYTES;
    if (address == IMMOBILE_SPACE_START) {
        for (page = 0 ; page < n_pages ; ++page) {
            lispobj* page_data = low_page_address(page);
            for (word_idx = 0 ; word_idx < WORDS_PER_PAGE ; ++word_idx) {
                lispobj* obj = page_data + word_idx;
                lispobj header = *obj;
                if (!fixnump(header)) {
                    gc_assert(other_immediate_lowtag_p(*obj));
                    fixedobj_pages[page].attr.parts.obj_size
                        = sizetab[widetag_of(header)](obj);
                    fixedobj_pages[page].attr.parts.obj_align
                        = immobile_obj_spacing(header, obj,
                                               fixedobj_pages[page].attr.parts.obj_size);
                    fixedobj_pages[page].attr.parts.flags = WRITE_PROTECT;
                    fixedobj_pages[page].gens |= 1 << __immobile_obj_gen_bits(obj);
                    break;
                }
            }
        }
    } else if (address == IMMOBILE_VARYOBJ_SUBSPACE_START) {
        lispobj* obj = (lispobj*)address;
        lispobj* limit = (lispobj*)(address + len);
        int n_words;
        for ( ; obj < limit ; obj += n_words ) {
            n_words = sizetab[widetag_of(*obj)](obj);
            if (obj[1] == 0 && (obj[0] == INSTANCE_HEADER_WIDETAG ||
                                obj[0] == 0)) {
                if (obj[0]) {
                    // Round up to the next immobile page.
                    lispobj page_end = CEILING((lispobj)obj, IMMOBILE_CARD_BYTES);
                    n_words = (lispobj*)page_end - obj;
                    obj[0] = SIMPLE_ARRAY_FIXNUM_WIDETAG;
                    obj[1] = make_fixnum(n_words - 2);
                } else {
                    // There are trailing zeros to fill the core file page.
                    // This happens when the next object is exactly aligned
                    // to an immobile page. There is no padding object.
                    gc_assert(((lispobj)obj & (IMMOBILE_CARD_BYTES-1)) == 0);
                }
                limit = obj;
                break;
            }
            if (immobile_filler_p(obj)) {
                // Holes were chained through the debug_info slot at save.
                // Just update the head of the chain.
                varyobj_holes = (lispobj)obj;
                continue;
            }
            int first_page = find_immobile_page_index(obj);
            int last_page = find_immobile_page_index(obj+n_words-1);
            // Only the page with this object header gets a bit in its gen mask.
            VARYOBJ_PAGE_GENS(first_page) |= 1<<__immobile_obj_gen_bits(obj);
            // For each page touched by this object, set the page's
            // scan_start_offset, unless it was already set.
            int page;
            for (page = first_page ; page <= last_page ; ++page) {
                if (!varyobj_page_scan_start_offset[page - FIRST_VARYOBJ_PAGE]) {
                    long offset = (char*)low_page_address(page+1) - (char*)obj;
                    varyobj_page_scan_start_offset[page - FIRST_VARYOBJ_PAGE]
                        = offset >> (WORD_SHIFT + 1);
                }
            }
        }
        SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value = (lispobj)limit;
        protect_space();
    } else {
        lose("unknown immobile subspace");
    }
}

// Demote pseudo-static to highest normal generation
// so that all objects become eligible for collection.
void prepare_immobile_space_for_final_gc()
{
    int page;
    char* page_base;
    char* page_end = (char*)SYMBOL(IMMOBILE_FIXEDOBJ_FREE_POINTER)->value;

    // The list of holes need not be saved.
    SYMBOL(IMMOBILE_FREELIST)->value = NIL;

    for (page_base = (char*)IMMOBILE_SPACE_START, page = 0 ;
         page_base < page_end ;
         page_base += IMMOBILE_CARD_BYTES, ++page) {
        unsigned char mask = fixedobj_pages[page].gens;
        if (mask & 1<<PSEUDO_STATIC_GENERATION) {
            int obj_spacing = page_obj_align(page) << WORD_SHIFT;
            lispobj* obj = (lispobj*)page_base;
            lispobj* limit = (lispobj*)(page_base + IMMOBILE_CARD_BYTES - obj_spacing);
            for ( ; obj <= limit ; obj = (lispobj*)((char*)obj + obj_spacing) ) {
                if (!fixnump(*obj)
                    && __immobile_obj_gen_bits(obj) == PSEUDO_STATIC_GENERATION)
                    assign_generation(obj, HIGHEST_NORMAL_GENERATION);
            }
            fixedobj_pages[page].gens = (mask & ~(1<<PSEUDO_STATIC_GENERATION))
                                        | 1<<HIGHEST_NORMAL_GENERATION;
        }
    }

    lispobj* obj = (lispobj*)IMMOBILE_VARYOBJ_SUBSPACE_START;
    lispobj* limit = (lispobj*)SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value;
    for ( ; obj < limit ; obj += sizetab[widetag_of(*obj)](obj) ) {
        if (__immobile_obj_gen_bits(obj) == PSEUDO_STATIC_GENERATION)
            assign_generation(obj, HIGHEST_NORMAL_GENERATION);
    }
    int max_page = find_immobile_page_index(limit-1);
    for ( page = FIRST_VARYOBJ_PAGE ; page <= max_page ; ++page ) {
        int mask = VARYOBJ_PAGE_GENS(page);
        if (mask & (1<<PSEUDO_STATIC_GENERATION)) {
            VARYOBJ_PAGE_GENS(page) = (mask & ~(1<<PSEUDO_STATIC_GENERATION))
                                      | 1<<HIGHEST_NORMAL_GENERATION;
        }
    }
}

// Now once again promote all objects to pseudo-static just prior to save.
// 'coreparse' makes all pages in regular dynamic space pseudo-static.
// But since immobile objects store their generation, it must be done at save,
// or else it would have to be done on image restart
// which would require writing to a lot of pages for no reason.
void prepare_immobile_space_for_save()
{
    int page;
    char *page_base;
    char* page_end = (char*)SYMBOL(IMMOBILE_FIXEDOBJ_FREE_POINTER)->value;

    for (page_base = (char*)IMMOBILE_SPACE_START, page = 0 ;
         page_base < page_end ;
         page_base += IMMOBILE_CARD_BYTES, ++page) {
        int obj_spacing = page_obj_align(page) << WORD_SHIFT;
        if (obj_spacing) {
            lispobj* obj = (lispobj*)page_base;
            lispobj* limit = (lispobj*)(page_base + IMMOBILE_CARD_BYTES - obj_spacing);
            for ( ; obj <= limit ; obj = (lispobj*)((char*)obj + obj_spacing) ) {
                if (!fixnump(*obj))
                    assign_generation(obj, PSEUDO_STATIC_GENERATION);
            }
        }
    }
    lispobj* obj = (lispobj*)IMMOBILE_VARYOBJ_SUBSPACE_START;
    lispobj* limit = (lispobj*)SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value;
    for ( varyobj_holes = 0 ;  obj < limit ; obj += sizetab[widetag_of(*obj)](obj) ) {
        if (immobile_filler_p(obj)) {
            struct code* code  = (struct code*)obj;
            code->debug_info = varyobj_holes;
            varyobj_holes    = (lispobj)code;
            // 0-fill the unused space.
            int nwords = sizetab[widetag_of(*obj)](obj);
            memset(code->constants, 0,
                   (nwords * N_WORD_BYTES) - offsetof(struct code, constants));
        } else
            assign_generation(obj, PSEUDO_STATIC_GENERATION);
    }
    if ((lispobj)limit & (IMMOBILE_CARD_BYTES-1)) { // Last page is partially used.
        gc_assert(*limit == SIMPLE_ARRAY_FIXNUM_WIDETAG);
        // Write an otherwise illegal object at the free pointer.
        limit[0] = INSTANCE_HEADER_WIDETAG; // 0 payload length
        limit[1] = 0; // no layout
    }
}

//// Interface

int immobile_space_handle_wp_violation(void* fault_addr)
{
    low_page_index_t page_index = find_immobile_page_index(fault_addr);
    if (page_index < 0) return 0;

    if (page_index >= FIRST_VARYOBJ_PAGE) {
        // The free pointer can move up or down. Attempting to insist that a WP
        // fault not occur above the free pointer (plus some slack) is not
        // threadsafe, so allow it anywhere. More strictness could be imparted
        // by tracking the max value attained by the free pointer.
        __sync_or_and_fetch(&varyobj_page_touched_bits[(page_index-FIRST_VARYOBJ_PAGE)/32],
                            1 << (page_index & 31));
    } else {
        // FIXME: a single bitmap of touched bits would make more sense,
        // and the _CLEARED flag doesn't achieve much if anything.
        if (!(fixedobj_pages[page_index].attr.parts.flags
              & (WRITE_PROTECT|WRITE_PROTECT_CLEARED)))
            return 0;
        SET_WP_FLAG(page_index, WRITE_PROTECT_CLEARED);
    }
    os_protect((os_vm_address_t)((lispobj)fault_addr & ~(IMMOBILE_CARD_BYTES-1)),
               IMMOBILE_CARD_BYTES, OS_VM_PROT_ALL);
    return 1;
}

// Find the object that encloses pointer.
lispobj *
search_immobile_space(void *pointer)
{
    lispobj *start;

    if ((lispobj)pointer >= IMMOBILE_SPACE_START
        && (lispobj)pointer < SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value) {
        low_page_index_t page_index = find_immobile_page_index(pointer);
        if ((lispobj)pointer >= IMMOBILE_VARYOBJ_SUBSPACE_START) {
            start = (lispobj*)varyobj_scan_start(page_index);
            if (start > (lispobj*)pointer) return NULL;
            return (gc_search_space(start,
                                    (((lispobj*)pointer)+2)-start,
                                    (lispobj*)pointer));
        } else if ((lispobj)pointer < SYMBOL(IMMOBILE_FIXEDOBJ_FREE_POINTER)->value) {
            char *page_base = (char*)((lispobj)pointer & ~(IMMOBILE_CARD_BYTES-1));
            int spacing = page_obj_align(page_index) << WORD_SHIFT;
            int index = ((char*)pointer - page_base) / spacing;
            char *begin = page_base + spacing * index;
            char *end = begin + (page_obj_size(page_index) << WORD_SHIFT);
            if ((char*)pointer < end) return (lispobj*)begin;
        }

    }
    return NULL;
}

// For coalescing holes, we need to scan backwards, which is done by
// looking backwards for a page that contains the start of a
// block of objects one of which must abut 'obj'.
lispobj* find_preceding_object(lispobj* obj)
{
  int page = find_immobile_page_index(obj);
  while (1) {
      int offset = varyobj_page_scan_start_offset[page - FIRST_VARYOBJ_PAGE];
      if (offset) { // 0 means the page is empty.
          lispobj* start = varyobj_scan_start(page);
          if (start < obj) { // Scan from here forward
              while (1) {
                  lispobj* end = start + sizetab[widetag_of(*start)](start);
                  if (end == obj) return start;
                  gc_assert(end < obj);
                  start = end;
              }
          }
      }
      if (page == FIRST_VARYOBJ_PAGE) {
          gc_assert(obj == low_page_address(FIRST_VARYOBJ_PAGE));
          return 0; // Predecessor does not exist
      }
      --page;
  }
}

#include "genesis/vector.h"
#include "genesis/instance.h"
lispobj alloc_layout(lispobj layout_layout, lispobj slots)
{
    struct vector* v = (struct vector*)native_pointer(slots);
    if (fixnum_value(v->length) != (LAYOUT_SIZE - INSTANCE_DATA_START - 1))
        lose("bad arguments to alloc_layout");
    struct instance* l = (struct instance*)
      alloc_immobile_obj(MAKE_ATTR(256 / N_WORD_BYTES,
                                   CEILING(LAYOUT_SIZE,2),
                                   0),
#ifdef LISP_FEATURE_COMPACT_INSTANCE_HEADER
                         (layout_layout << 32) |
#endif
                         (LAYOUT_SIZE-1)<<8 | INSTANCE_HEADER_WIDETAG,
                         &layout_page_hint);
#ifndef LISP_FEATURE_COMPACT_INSTANCE_HEADER
    l->slots[0] = layout_layout;
#endif
    memcpy(&l->slots[INSTANCE_DATA_START],
           v->data,
           (LAYOUT_SIZE-1)*N_WORD_BYTES);

    // Possible efficiency win: make the "wasted" bytes after the layout into a
    // simple unboxed array so that heap-walking can skip in one step.
    // Probably only a performance issue for MAP-ALLOCATED-OBJECTS,
    // since scavenging know to skip by the object alignment anyway.
    return (lispobj)l | INSTANCE_POINTER_LOWTAG;
}

#include "genesis/symbol.h"
lispobj alloc_sym(lispobj name, int kind)
{
    // In case we want different kinds of symbol pages (as was the hope)
    // to keep special variables apart from random trash.
    // i.e. symbols whose page is often written versus symbols
    // that exist only as monikers. This would minimize the number
    // of different pages that become touched between GC cycles.
    int* hint = &symbol_page_hint;
    struct symbol* s = (struct symbol*)
      alloc_immobile_obj(MAKE_ATTR(CEILING(SYMBOL_SIZE,2), // spacing
                                   CEILING(SYMBOL_SIZE,2), // size
                                   kind),
                         (SYMBOL_SIZE-1)<<8 | SYMBOL_HEADER_WIDETAG,
                         hint);
    s->value = UNBOUND_MARKER_WIDETAG;
    s->hash = 0;
    s->info = NIL;
    s->name = name;
    s->package = NIL;
    return (lispobj)s | OTHER_POINTER_LOWTAG;
}

#include "genesis/fdefn.h"
lispobj alloc_fdefn(lispobj name)
{
    struct fdefn* f = (struct fdefn*)
      alloc_immobile_obj(MAKE_ATTR(CEILING(FDEFN_SIZE,2), // spacing
                                   CEILING(FDEFN_SIZE,2), // size
                                   0),
                         (FDEFN_SIZE-1)<<8 | FDEFN_WIDETAG,
                         &fdefn_page_hint);
    f->name = name;
    f->fun = NIL;
    f->raw_addr = 0;
    return (lispobj)f | OTHER_POINTER_LOWTAG;
}

#ifdef VERIFY_PAGE_GENS
void check_fixedobj_page(int page)
{
  // Every page should have a 'gens' mask which exactly reflects
  // the aggregate over all objects on that page. Verify that invariant,
  // checking all pages, not just the ones below the free pointer.
  int genmask, obj_size, obj_spacing, i, all_ok = 1;
  lispobj *obj, *limit, header;
  int sees_younger = 0;

  obj_size = page_obj_size(page);
  obj_spacing = page_obj_align(page);
  obj = low_page_address(page);
  limit = obj + WORDS_PER_PAGE - obj_spacing;
  genmask = 0;
  if (obj_size == 0) {
      for (i=0; i<WORDS_PER_PAGE; ++i)
        gc_assert(obj[i]==0);
      gc_assert(fixedobj_pages[page].gens ==0);
      return;
  }
  while (obj <= limit) {
      header = *obj;
        if (!fixnump(header)) {
          if (__immobile_obj_gen_bits(obj) & IMMOBILE_OBJ_VISITED_FLAG) {
            fprintf(stderr, "Obj header @ %p = %p\n", obj, (void*)header);
            all_ok = 0;
          } else {
            int gen = __immobile_obj_gen_bits(obj);
            gc_assert(0 <= gen && gen <= 6);
            genmask |= 1<<gen;
            if (fixedobj_points_to_younger_p(obj, page_obj_size(page),
                                             gen, 0xff, 0xff))
              sees_younger = 1;
          }
        }
        obj += obj_spacing;
  }
  // It's not wrong if the gen0 bit is set spuriously, but it should only
  // happen at most once, on the first GC after image startup.
  // At all other times, the invariant should hold that if the freelist
  // indicated that space was available, and the new pointer differs,
  // then some gen0 object exists on the page.
  // The converse is true because of pseudo-atomicity of the allocator:
  // if some thread claimed a hole, then it also updated the freelist.
  // If it died before doing the latter, then the object allegedly created
  // was never really live, so won't contain any pointers.
  if (fixedobj_pages[page].gens != genmask
      && fixedobj_pages[page].gens != (genmask|1)) {
    fprintf(stderr, "Page #x%x @ %p: stored mask=%x actual=%x\n",
            page, low_page_address(page),
            fixedobj_pages[page].gens, genmask);
    all_ok = 0;
  }
  if (fixedobj_page_wp(page) && sees_younger) {
    fprintf(stderr, "Page #x%x @ %p: WP is wrong\n",
            page, low_page_address(page));
    all_ok = 0;
  }
  gc_assert(all_ok);
}

int n_immobile_objects;
int *immobile_objects, *immobile_objects_limit;

int comparator_eq(const void* a, const void* b) {
  return *(int*)a - *(int*)b;
}

// Find the largest item less than or equal.
// (useful for finding the object that contains a given pointer)
int comparator_le(const void* a, const void* b) {
  int diff = *(int*)a - *(int*)b;
  if (diff <= 0) return diff;
  // If looking to the right would see an item strictly greater
  // than the sought key, or there is nothing to the right,
  // then deem this an exact match.
  if (b == (void*)immobile_objects_limit || ((int*)b)[1] > *(int*)a) return 0;
  return 1;
}

// Find the smallest item greater than or equal.
// useful for finding the lowest item at or after a page base address.
int comparator_ge(const void* a, const void* b) {
  int diff = *(int*)a - *(int*)b;
  if (diff >= 0) return diff;
  // If looking to the left would see an item strictly less
  // than the sought key, or there is nothing to the left
  // then deem this an exact match.
  if (b == (void*)immobile_objects || ((int*)b)[-1] < *(int*)a) return 0;
  return -1;
}

void check_varyobj_pages()
{
  // 1. Check that a linear scan sees only valid object headers,
  //    and that it terminates exactly at IMMOBILE_CODE_FREE_POINTER.
  lispobj* obj = (lispobj*)IMMOBILE_VARYOBJ_SUBSPACE_START;
  lispobj* end = (lispobj*)SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value;
  low_page_index_t end_page = find_immobile_page_index((char*)end-1);

  n_immobile_objects = 0;
  while (obj < end) {
    lispobj word = *obj;
    gc_assert(other_immediate_lowtag_p(word));
    int n_words = sizetab[widetag_of(word)](obj);
    obj += n_words;
    ++n_immobile_objects;
  }
  gc_assert(obj == end);

  // 2. Check that all scan_start_offsets are plausible.
  // Begin by collecting all object header locations into an array;
  immobile_objects = calloc(n_immobile_objects, sizeof (lispobj));
  immobile_objects_limit = immobile_objects + n_immobile_objects - 1;
  obj = (lispobj*)IMMOBILE_VARYOBJ_SUBSPACE_START;
  int i = 0;
  while (obj < end) {
    immobile_objects[i++] = (lispobj)obj;
    lispobj word = *obj;
    int n_words = sizetab[widetag_of(word)](obj);
    obj += n_words;
  }
  // Check that each page's scan start is a known immobile object
  // and that it is the right object.
  low_page_index_t page;
  for (page = FIRST_VARYOBJ_PAGE; page <= end_page; ++page) {
    lispobj page_addr = (lispobj)low_page_address(page);
    int* found_below = bsearch(&page_addr, immobile_objects, n_immobile_objects,
                                sizeof (int), comparator_le);
    int* found_above = bsearch(&page_addr, immobile_objects, n_immobile_objects,
                                sizeof (int), comparator_ge);
    int stored_scan_start = (int)varyobj_scan_start(page);
    lispobj* scan_start_obj = (lispobj*)(long)*found_below;
    if ((int)scan_start_obj != stored_scan_start) {
      //printf("page %d: found-below=%p stored=%p\n", page, scan_start_obj, stored_scan_start);
      while (immobile_filler_p(scan_start_obj)) {
        int nwords = sizetab[widetag_of(*scan_start_obj)](scan_start_obj);
        //        printf("skipping %d words to %p\n", nwords, scan_start_obj + nwords);
        scan_start_obj += nwords;
        // the stored scan start does not guarantee that it points
        // to a non-hole; we only assert that it *probably* does not.
        // As such, when computing the "correct" value, we allow
        // any value in between the legal bounding values for it.
        if ((int)scan_start_obj == stored_scan_start)
          break;
        // If you hit the free pointer, or run off the page,
        // then the page is completely empty.
        if (scan_start_obj == (lispobj*)SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value
            || scan_start_obj >= (lispobj*)low_page_address(page+1)) {
          scan_start_obj = low_page_address(page+1);
          break;
        }
      }
    }
    if ((int)scan_start_obj != stored_scan_start)
      lose("page %d: stored_scan_start=%p does not match found %p\n",
           page, stored_scan_start, *found_below);
    if (found_below != found_above) {
      // the object below must touch this page.
      // if it didn't, there should be a higher object below.
      lispobj* below = (lispobj*)(long)*found_below;
      int n_words = sizetab[widetag_of(*below)](below);
      lispobj* end = below + n_words;
      gc_assert(end > (lispobj*)page_addr);
    }
  }
  free(immobile_objects);

  // 3. The generation mask for each page is exactly the union
  //    of generation numbers of object headers on the page.
  for (page = FIRST_VARYOBJ_PAGE; page <= end_page; ++page) {
      if (!varyobj_page_scan_start_offset[page - FIRST_VARYOBJ_PAGE])
        continue; // page is all holes or never used
      obj = varyobj_scan_start(page);
      lispobj word = *obj;
      int n_words = sizetab[widetag_of(word)](obj);
      // Skip the first object if it doesn't start on this page.
      if (obj < (lispobj*)low_page_address(page)) obj += n_words;
      lispobj* limit = (lispobj*)low_page_address(page) + WORDS_PER_PAGE;
      lispobj* freeptr = (lispobj*)SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value;
      if (limit > freeptr) limit = freeptr;
      int mask = 0;
      for ( ; obj < limit ; obj += sizetab[widetag_of(*obj)](obj) ) {
          int gen = __immobile_obj_gen_bits(obj);
          if (immobile_filler_p(obj)) {
              gc_assert(gen == 0);
          } else {
              gc_assert(0 <= gen && gen <= 6);
              mask |= 1 << gen;
          }
          if (widetag_of(*obj) == CODE_HEADER_WIDETAG) {
              lispobj entry_point; /* tagged pointer to entry point */
              struct simple_fun *function_ptr; /* untagged pointer to entry point */
              for (entry_point = ((struct code*)obj)->entry_points;
                   entry_point != NIL;
                   entry_point = function_ptr->next) {
                  function_ptr = (struct simple_fun *) native_pointer(entry_point);
                  gc_assert_verbose(is_lisp_pointer(entry_point),
                                    "Code %p entry point %p is not a lisp pointer.",
                                    obj, (void*)entry_point);
                  gc_assert(widetag_of(function_ptr->header)==SIMPLE_FUN_HEADER_WIDETAG);
              }
          }
      }
      gc_assert(mask == VARYOBJ_PAGE_GENS(page));
  }
  // 4. If a page's dirty bit is clear, it contains no old->young pointers.
  for (page = FIRST_VARYOBJ_PAGE; page <= end_page; ++page) {
    if (!varyobj_page_touched(page)) {
      lispobj* page_begin = low_page_address(page);
      lispobj* page_end = page_begin + WORDS_PER_PAGE;
      // Assert that there are no old->young pointers.
      obj = varyobj_scan_start(page);
      if (obj < page_begin) obj += sizetab[widetag_of(*obj)](obj);
      // Never scan past the free pointer.
      // FIXME: It is is supposed to work to scan past the free pointer
      // on the last page, but the allocator needs to plop an array header there,
      // and sometimes it doesn't.
      lispobj* varyobj_free_ptr = (lispobj*)(SYMBOL(IMMOBILE_SPACE_FREE_POINTER)->value);
      if (page_end > varyobj_free_ptr) page_end = varyobj_free_ptr;
      for ( ; obj < page_end ; obj += sizetab[widetag_of(*obj)](obj) ) {
        if (!immobile_filler_p(obj)
            && varyobj_points_to_younger_p(obj, __immobile_obj_gen_bits(obj),
                                           0xff, 0xff,
                                           (char*)page_begin, (char*)page_end))
          lose("page WP bit on page %d is wrong\n", page);
      }
    }
  }
}
#endif
