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
 */

// Work around a bug in some llvm/clang versions affecting the memcpy
// call in defrag_immobile_space:
//
// When compiled with _FORTIFY_SOURCE nonzero, as seems to be the
// default, memcpy is a macro that expands to
// __builtin_memcpy_chk(dst, source, size, __builtin_object_size(...)).
//
// Now usually if the compiler knows that it does not know
// __builtin_object_size for the source of the copy, the
// __builtin_memcpy_chk call becomes plain old memcpy. But in the
// buggy case, the compiler is convinced that it does know the
// size. This shows up clearly in the disassembly, where the library
// routine receives a source size that was erroneously determined to
// be a compile-time constant 0. Thus the assertion failure is that
// you are reading from a range with 0 bytes in it.
//
// Defining _FORTIFY_LEVEL 0 disables the above macro and thus works
// around the problem. Since it is unclear which clang versions are
// affected, only apply the workaround for the known-bad version.
#if (defined(__clang__) && (__clang_major__ == 6) && (__clang_minor__ == 0))
#define _FORTIFY_SOURCE 0
#endif

#include "code.h"
#include "core.h" // for CORE_PAGE_GENERATION
#include "gc.h"
#include "genesis/brothertree.h"
#include "genesis/cons.h"
#include "genesis/gc-tables.h"
#include "genesis/hash-table.h"
#include "genesis/instance.h"
#include "genesis/package.h"
#include "genesis/static-symbols.h"
#include "genesis/symbol.h"
#include "genesis/vector.h"
#include "immobile-space.h"
#include "pseudo-atomic.h"
#include "search.h"
#include "align.h"
#include "validate.h"
#include "var-io.h"
#include <stdlib.h>
#include <stdio.h>
#include "../../tlsf-bsd/tlsf/tlsf.h"

#define WORDS_PER_PAGE ((int)IMMOBILE_CARD_BYTES/N_WORD_BYTES)
#define DOUBLEWORDS_PER_PAGE (WORDS_PER_PAGE/2)

// In case of problems while debugging, this is selectable.
#define DEFRAGMENT_FIXEDOBJ_SUBSPACE 1

#undef DEBUG

#ifdef DEBUG
#  define dprintf(arg) fprintf arg
FILE * logfile;
#else
#  define dprintf(arg)
#endif

static void defrag_immobile_space(bool verbose);

uword_t immobile_space_lower_bound, immobile_space_max_offset;
uword_t immobile_range_1_max_offset, immobile_range_2_min_offset;
unsigned int text_space_size = TEXT_SPACE_SIZE;

// This table is for objects fixed in size, as opposed to variable-sized.
// (Immobile objects are naturally fixed in placement)
struct fixedobj_page *fixedobj_pages;
lispobj* immobile_scav_queue;
int immobile_scav_queue_head;
// Number of items enqueued; can exceed QCAPACITY on overflow.
// If overflowed, the queue is unusable until reset.
unsigned int immobile_scav_queue_count;
#define QCAPACITY 1024

#define gens attr.parts.gens_

// These are the high 2 bits of 'flags'
#define WRITE_PROTECT         0x80
#define WRITE_PROTECT_CLEARED 0x40

// Packing and unpacking attributes
// the low two flag bits are for write-protect status
#define MAKE_ATTR(spacing) ((spacing)<<8)
#define OBJ_SPACING(attr) ((attr>>8) & 0xFF)

// Ignore the write-protect bits and the generations when comparing attributes
#define ATTRIBUTES_MATCH_P(page_attr,specified_attr) \
  ((page_attr & 0xFFFF3F) == specified_attr)
#define SET_WP_FLAG(index,flag) \
  fixedobj_pages[index].attr.parts.flags = (fixedobj_pages[index].attr.parts.flags & 0x3F) | flag

#define set_page_full(i) fixedobj_pages[i].free_index = IMMOBILE_CARD_BYTES
#define page_full_p(i) (fixedobj_pages[i].free_index >= (int)IMMOBILE_CARD_BYTES)
#define fixedobj_page_wp(i) (fixedobj_pages[i].attr.parts.flags & WRITE_PROTECT)

/* Compute the last (potential) object on a fixed-size object page
 * starting at 'base' and stepping by 'spacing_bytes' bytes */
static inline lispobj* compute_fixedobj_limit(void* base, int spacing_bytes) {
    return (lispobj*)((char*)base + IMMOBILE_CARD_BYTES - spacing_bytes);
}
#define NEXT_FIXEDOBJ(where, spacing_bytes) \
    (where = (lispobj*)((char*)where + spacing_bytes))

/// Variable-length pages:

unsigned char* text_page_genmask;
// scan-start-offset, measured in bytes from page base address.
// one per page *excluding* all pseudostatic pages.
// Unlike with dynamic-space, the scan start for a text page
// is an address not lower than the base page.
unsigned short int* tlsf_page_sso;
// Array of inverted write-protect flags, 1 bit per page.
unsigned int* text_page_touched_bits;
static int n_bitmap_elts; // length of array measured in 'int's
// List of FILLER_WIDETAG objects to be stuffed back into the TLSF-managed pool
// chained through the word after their header.
lispobj codeblob_freelist;

bool immobile_card_protected_p(void* addr)
{
    low_page_index_t page;
    page = find_text_page_index(addr);
    if (page >= 0) return !((text_page_touched_bits[page/32] >> (page&31)) & 1);
    page = find_fixedobj_page_index(addr);
    if (page >= 0) return fixedobj_page_wp(page);
    lose("immobile_card_protected_p(%p)", addr);
}

void* tlsf_control;

#define text_page_touched(x) ((text_page_touched_bits[x/32] >> (x&31)) & 1)
// These bits are in the same place in the code header as the ones
// in 'tlsf.c' but shifted by 8 more so that they refer to the header
// word as a whole.
static const unsigned block_header_free_bit = 1 << 8;
static const unsigned block_header_prev_free_bit = 1 << 9;
/* Indicates oversized allocation in a header bit so that we can eliminate
 * 2 words of padding when saving a core (not done yet)
 * This seems to occur for fewer than .5% of all allocations,
 * accounting for under .01% addditional total space used.
 * So there's really no need to do anything about it.
static const unsigned block_header_oversized = 1 << 10;
*/
void *tlsf_alloc_codeblob(tlsf_t tlsf, int requested_nwords)
{
    // The size we request is 1 word less, because the allocator's block header
    // counts as part of the resulting object as far as Lisp is concerned.
    int size = (requested_nwords - 1) << WORD_SHIFT;
    void* tlsf_result = tlsf_malloc(tlsf, size);
    if (!tlsf_result) return 0;
    struct code* c = (void*)((lispobj*)tlsf_result - 1);
    gc_assert(!((uintptr_t)c & LOWTAG_MASK));
    assign_widetag(c, CODE_HEADER_WIDETAG);
    c->boxed_size = c->debug_info = c->fixups = 0;
    int nwords = code_total_nwords(c);
    ((lispobj*)c)[nwords-1] = 0; // trailer word with the simple-fun table
    lispobj* end = (lispobj*)c + nwords;
    if (end > text_space_highwatermark) text_space_highwatermark = end;
    // Adjust the scan start if this became the lowest addressable in-use block on its page
    low_page_index_t tlsf_page = ((char*)c - (char*)tlsf_mem_start) / IMMOBILE_CARD_BYTES;
    int offset = (uword_t)c & (IMMOBILE_CARD_BYTES-1);
    if (offset < tlsf_page_sso[tlsf_page]) tlsf_page_sso[tlsf_page] = offset;
    text_page_genmask[find_text_page_index(c)] |= 1;
#if 0
    if (code_total_nwords(c) > requested_nwords)
        fprintf(stderr, "NOTE: asked for %d words but got %d\n",
                requested_nwords, code_total_nwords(c));
#endif
    return c;
}

void tlsf_unalloc_codeblob(tlsf_t tlsf, struct code* code)
{
    int nwords = code_total_nwords(code);
    lispobj* end = (lispobj*)code + nwords;
    /* If the HWM is the end of the object being freed, adjust the HWM. There are 2 cases:
     * 1. if the previous block is free, then the start of the previous physical block
     *    is the new high water mark. The rightmost blocks in this picture get conbined.
     *    The block to the left of the already-free one is definitely used.
     *    +-------+------+----------+
     *    | used  | free | freeing  | <- current HWM
     *    +-------+------+----------+
     *            ^ new HWM
     *
     * 2. previous block is in-use: this object's address is the new HWM
     */
    if (end == text_space_highwatermark) {
        if (code->header & block_header_prev_free_bit)
            /* The word prior to 'code' is the pointer to the previous physical block_header_t.
             * The high water mark is 1 word beyond the previous physical block due to the
             * discrepancy between block_header_t and where a block logically begins. */
            text_space_highwatermark = 1 + (lispobj*)((lispobj*)code)[-1];
        else
            text_space_highwatermark = (lispobj*)code;
        gc_assert(!((uword_t)text_space_highwatermark & LOWTAG_MASK));
    }
    // See if the page scan start needs to change
    low_page_index_t tlsf_page = ((char*)code - (char*)tlsf_mem_start) / IMMOBILE_CARD_BYTES;
    int offset = (uword_t)code & (IMMOBILE_CARD_BYTES-1);
    if (offset == tlsf_page_sso[tlsf_page]) {
        lispobj* next = end;
        if (*next & block_header_free_bit) {
            next += code_total_nwords((struct code*)next);
            gc_assert(!(*next & block_header_free_bit)); // adjacent free blocks can't occur
        }
        // If the next used block is on the same page, then it becomes the page scan start
        // even if it the ending sentinel block (which counts as "used").
        if ((((uword_t)next ^ (uword_t)code) / IMMOBILE_CARD_BYTES) == 0) {
            tlsf_page_sso[tlsf_page] = (uword_t)next & (IMMOBILE_CARD_BYTES-1);
        } else {
            tlsf_page_sso[tlsf_page] = USHRT_MAX;
            // the tlsf_page index is based on tlsf_mem_start, but gemmask[] is based on TEXT_SPACE_START
            int text_page = find_text_page_index(code);
            text_page_genmask[text_page] = 0;
        }
    }
    // point to the user data, not the header, when calling free
    tlsf_free(tlsf, (lispobj*)code + 1);
}

//// Fixed-length object allocator

/* Return the index of an immobile page that is probably not totally full,
   starting with 'hint_page' and wrapping around.
   'attributes' determine an eligible page.
   *FIXEDOBJ-SPACE-FREE-POINTER* is updated to point beyond the found page
   if it previously did not. */

static int get_freeish_page(int hint_page, int attributes)
{
  int page = hint_page;
  lispobj *new_free_pointer, *old_free_pointer, *actual_old;
  int page_attr_packed;
  unsigned char best_genmask = 0xff;
  int best_page = -1;
  const int npages = FIXEDOBJ_SPACE_SIZE / IMMOBILE_CARD_BYTES;

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
              new_free_pointer = fixedobj_page_address(page+1);
              old_free_pointer = fixedobj_free_pointer;
              while (new_free_pointer > old_free_pointer) {
                  actual_old =
                    __sync_val_compare_and_swap(&fixedobj_free_pointer,
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
          // Try to avoid new objects on pages with any pseudo-static objects,
          // because then touching the young object forces scanning the page,
          // which is unfortunate if most things on it were untouched.
          if (fixedobj_pages[page].gens < (1<<PSEUDO_STATIC_GENERATION)) {
            // instant win
            return page;
          } else if (fixedobj_pages[page].gens < best_genmask) {
            best_genmask = fixedobj_pages[page].gens;
            best_page = page;
          }
      }
      if (++page >= npages) page = 0;
  } while (page != hint_page);
  if (best_page >= 0)
      return best_page;
  lose("No more immobile pages available");
}

/// Size class is specified by lisp now
#define MAX_ALLOCATOR_SIZE_CLASSES 10
uint32_t fixedobj_page_hint[MAX_ALLOCATOR_SIZE_CLASSES];

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

lispobj
alloc_immobile_fixedobj(int size_class, int spacing_words, uword_t header)
{
  size_class = fixnum_value(size_class);
  spacing_words = fixnum_value(spacing_words);
  header = fixnum_value(header);

  unsigned int page;
  lispobj word;
  int page_attributes = MAKE_ATTR(spacing_words);
  int spacing_in_bytes = spacing_words << WORD_SHIFT;
  const int npages = FIXEDOBJ_SPACE_SIZE / IMMOBILE_CARD_BYTES;

  page = fixedobj_page_hint[size_class];
  if (!page) {
      page = get_freeish_page(0, page_attributes);
      __sync_val_compare_and_swap(&fixedobj_page_hint[size_class], 0, page);
  }

  /* BUG: This assertion is itself buggy and has to be commented out
   * if running with extra debug assertions.
   * It's only OK in single-threaded code, but consider two threads:
   *   Thread A                          Thread B
   *   --------                          --------
   *   1. change attributes  of
   *      page 483 (e.g.) from 0
   *      to something
   *                                     2. observe that page 483 has
   *                                        desired page_attributes,
   *                                        and return it from get_freeish_page
   *                                     3. read the now-obsolete value of
   *                                        fixedobj_free_pointer at the dcheck.
   *   4. bump the free pointer to
   *      the end of page 483
   *      and return that page
   *                                     5. FAIL the dcheck
   *   5. pass the dcheck */
  // gc_dcheck(fixedobj_page_address(page) < (void*)fixedobj_free_pointer);
  do {
      char *page_data = fixedobj_page_address(page);
      char *limit = page_data + IMMOBILE_CARD_BYTES - spacing_in_bytes;
      char *obj_ptr = page_data + fixedobj_pages[page].free_index;
      while (obj_ptr <= limit) {
          word = *(lispobj*)obj_ptr;
          char *next_obj_ptr = obj_ptr + spacing_in_bytes;
          if (fixnump(word) // a fixnum marks free space
              && __sync_bool_compare_and_swap((lispobj*)obj_ptr,
                                              word, header)) {
              // The value formerly in the header word was the offset to
              // the next hole. Use it to update the freelist pointer.
              // Just slam it in
              int new_free_index = next_obj_ptr + word - page_data;
              fixedobj_pages[page].free_index = new_free_index;
              // Indicate in the generation mask that there is a generation0 object
              __sync_fetch_and_or(&fixedobj_pages[page].attr.parts.gens_, 1);
              return compute_lispobj((lispobj*)obj_ptr);
          }
          // If some other thread updated the free_index
          // to a larger value, use that. (See example below)
          char *next_free = page_data + fixedobj_pages[page].free_index;
          obj_ptr = next_free > next_obj_ptr ? next_free : next_obj_ptr;
      }
      set_page_full(page);
      int old_page = page;
      page = get_freeish_page(page+1 >= npages ? 0 : page+1,
                              page_attributes);
      // try to update the hint
      __sync_val_compare_and_swap(&fixedobj_page_hint[size_class],
                                  old_page, page);
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

// Find the high water marks for this GC scavenge phase
// (avoid passing exactly IMMOBILE_SPACE_END, which has no page index)
#define calc_max_used_fixedobj_page() find_fixedobj_page_index(fixedobj_free_pointer-1)
#define calc_max_used_text_page() find_text_page_index(text_space_highwatermark-1)

/* Turn a white object grey. Also enqueue the object for re-scan if required */
void
enliven_immobile_obj(lispobj *ptr, int rescan) // a native pointer
{
    gc_assert(widetag_of(ptr) != SIMPLE_FUN_WIDETAG); // can't enliven interior pointer
    gc_assert(widetag_of(ptr) != FILLER_WIDETAG);
    gc_assert(immobile_obj_gen_bits(ptr) == from_space);
    int pointerish = !leaf_obj_widetag_p(widetag_of(ptr));
    int bits = (pointerish ? 0 : IMMOBILE_OBJ_VISITED_FLAG);
    // enlivening makes the object appear as if written, so that
    // scav_code_blob won't skip it, thus ensuring we transitively
    // scavenge + enliven newspace objects.
    if (widetag_of(ptr) == CODE_HEADER_WIDETAG)
        bits |= OBJ_WRITTEN_FLAG;
    assign_generation(ptr, bits | new_space);
    low_page_index_t page_index = find_fixedobj_page_index(ptr);
    bool is_text = 0;

    if (page_index < 0) {
        page_index = find_text_page_index(ptr);
        gc_assert(page_index >= 0);
        text_page_genmask[page_index] |= 1<<new_space;
        is_text = 1;
    } else {
        fixedobj_pages[page_index].gens |= 1<<new_space;
    }
    // If called from preserve_pointer(), then we haven't scanned immobile
    // roots yet, so we only need ensure that this object's page's WP bit
    // is cleared so that the page is not skipped during root scan.
    if (!rescan) {
        if (pointerish) {
            if (is_text)
                text_page_touched_bits[page_index/32] |= 1U << (page_index & 31);
            else
                SET_WP_FLAG(page_index, WRITE_PROTECT_CLEARED);
        }
        return; // No need to enqueue.
    }

    // TODO: check that objects on protected root pages are not enqueued

    // Do nothing if either we don't need to look for pointers in this object,
    // or the work queue has already overflowed, causing a full scan.
    if (!pointerish || immobile_scav_queue_count > QCAPACITY) return;

    // count is either less than or equal to QCAPACITY.
    // If equal, just bump the count to signify overflow.
    if (immobile_scav_queue_count < QCAPACITY) {
        immobile_scav_queue[immobile_scav_queue_head] = (lispobj)ptr;
        immobile_scav_queue_head = (immobile_scav_queue_head + 1) & (QCAPACITY - 1);
    }
    ++immobile_scav_queue_count;
}

static uint32_t* loaded_codeblob_offsets;
static int loaded_codeblob_offsets_len;

// Find the lowest addressed object on specified page, or 0 if there isn't one
lispobj* text_page_scan_start(low_page_index_t page) {
    char* pagebase = text_page_address(page);
    if (pagebase < (char*)tlsf_mem_start) {
        uint32_t* data = loaded_codeblob_offsets;
        int index = bsearch_greatereql_uint32((int)(pagebase-(char*)TEXT_SPACE_START),
                                              data, loaded_codeblob_offsets_len);
        lispobj* start = 0;
        // I don't think index could ever be -1 ("not found"), could it?
        if (index >= 0) start = (lispobj*)(TEXT_SPACE_START+data[index]);
        // But it is possible for a page to have no scan start (nothing starts on it)
        return (start && (char*)start < pagebase+IMMOBILE_CARD_BYTES) ? start : 0;
    }
    if (pagebase > (char*)text_space_highwatermark) return 0;
    int tlsf_page = (pagebase - (char*)tlsf_mem_start) / IMMOBILE_CARD_BYTES;
    unsigned short sso = tlsf_page_sso[tlsf_page];
    return (sso < IMMOBILE_CARD_BYTES) ? (lispobj*)(pagebase + sso) : 0;
}

lispobj* search_immobile_code(char* ptr) {
    if (ptr < (char*)TEXT_SPACE_START) return 0;
    lispobj* candidate = 0;
    if (ptr < (char*)tlsf_mem_start) {
        uint32_t* data = loaded_codeblob_offsets;
        int index = bsearch_lesseql_uint32((int)(ptr-(char*)TEXT_SPACE_START),
                                           data, loaded_codeblob_offsets_len);
        if (index >= 0) candidate = (lispobj*)(TEXT_SPACE_START+data[index]);
    } else if (ptr < (char*)text_space_highwatermark) {
        lispobj node = brothertree_find_lesseql((uword_t)ptr,
                                                SYMBOL(IMMOBILE_CODEBLOB_TREE)->value);
        if (node != NIL) candidate = (lispobj*)((struct binary_node*)INSTANCE(node))->uw_key;
    }
    if (candidate && widetag_of(candidate) == CODE_HEADER_WIDETAG) {
        int nwords = code_total_nwords((struct code*)candidate);
        if (ptr < (char*)(candidate+nwords)) return candidate;
    }
    return 0;
}

/* If 'addr' points to an immobile object, then make the object
   live by promotion. But if the object is not in the generation
   being collected, do nothing */
bool immobile_space_preserve_pointer(void* addr)
{
    unsigned char genmask = compacting_p() ? 1<<from_space : 0xff;
    lispobj* object_start;
    int valid = 0;
    low_page_index_t page_index;

    if ((page_index = find_fixedobj_page_index(addr)) >= FIXEDOBJ_RESERVED_PAGES
        && ((fixedobj_pages[page_index].gens & genmask) != 0)) {
        int obj_spacing = fixedobj_page_obj_align(page_index);
        int obj_index = ((uword_t)addr & (IMMOBILE_CARD_BYTES-1)) / obj_spacing;
        dprintf((logfile,"Pointer %p is to immobile page %d, object %d\n",
                 addr, page_index, obj_index));
        char* page_start_addr = PTR_ALIGN_DOWN(addr, IMMOBILE_CARD_BYTES);
        object_start = (lispobj*)(page_start_addr + obj_index * obj_spacing);
        valid = !fixnump(*object_start)
            && properly_tagged_descriptor_p(addr, object_start);
    } else if (compacting_p() && (lispobj*)addr < tlsf_mem_start) {
        // Can ignore this pointer if it's point to pseudostatic text
        return 0;
    } else if ((object_start = search_immobile_code(addr)) != 0) {
        valid = instruction_ptr_p(addr, object_start)
                || properly_tagged_descriptor_p(addr, object_start);
    }
    if (valid && (!compacting_p() ||
                  immobile_obj_gen_bits(object_start) == from_space)) {
        dprintf((logfile,"immobile obj @ %p (<- %p) is conservatively live\n",
                 object_start, addr));
        if (compacting_p())
            enliven_immobile_obj(object_start, 0);
        else
            gc_mark_obj(compute_lispobj(object_start));
        return 1;
    }
    return 0;
}

// Loop over the newly-live objects, scavenging them for pointers.
// As with the ordinary gencgc algorithm, this uses almost no stack.
static void full_scavenge_immobile_newspace()
{
    page_index_t page;
    unsigned char bit = 1<<new_space;

    // Fixed-size object pages.

    low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
    for (page = FIXEDOBJ_RESERVED_PAGES; page <= max_used_fixedobj_page; ++page) {
        if (!(fixedobj_pages[page].gens & bit)) continue;
        // Skip amount within the loop is in bytes.
        int obj_spacing = fixedobj_page_obj_align(page);
        lispobj* obj    = fixedobj_page_address(page);
        lispobj* limit  = compute_fixedobj_limit(obj, obj_spacing);
        do {
            if (!fixnump(*obj) && immobile_obj_gen_bits(obj) == new_space) {
                set_visited(obj);
                lispobj header = *obj;
                scavtab[header_widetag(header)](obj, header);
            }
        } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
    }

    // Variable-size object pages

    low_page_index_t max_used_text_page = calc_max_used_text_page();
    page = -1; // -1 because of pre-increment
    while (1) {
        // Find the next page with anything in newspace.
        do {
            if (++page > max_used_text_page) return;
        } while ((text_page_genmask[page] & bit) == 0);
        lispobj* obj = text_page_scan_start(page);
        if (!obj) continue; // page contains nothing - can this happen?
        do {
            lispobj* limit = (lispobj*)text_page_address(page) + WORDS_PER_PAGE;
            if (limit > text_space_highwatermark) limit = text_space_highwatermark;
            sword_t n_words;
            for ( ; obj < limit ; obj += n_words ) {
                lispobj header = *obj;
                if (immobile_obj_gen_bits(obj) == new_space) {
                    set_visited(obj);
                    n_words = scavtab[header_widetag(header)](obj, header);
                } else {
                    n_words = headerobj_size2(obj, header);
                }
            }
            gc_assert(obj <= text_space_highwatermark);
            // Bail out if exact absolute end of immobile space was reached.
            if (obj == text_space_highwatermark) break;
            // If 'page' should be scanned, then pick up where we left off,
            // without recomputing 'obj' but setting a higher 'limit'.
            page = find_text_page_index(obj);
        } while (text_page_genmask[page] & bit);
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
              // FIXME: should not enqueue already-visited objects,
              // but a gc_assert() that it wasn't visited fails.
              if (!(immobile_obj_gen_bits(obj) & IMMOBILE_OBJ_VISITED_FLAG)) {
                set_visited(obj);
                lispobj header = *obj;
                scavtab[header_widetag(header)](obj, header);
              }
          } while (i != queue_index_to);
      }
  }
}

void
scavenge_immobile_roots(generation_index_t min_gen, generation_index_t max_gen)
{
    // example: scavenging gens 2..6, the mask of root gens is #b1111100
    int genmask = ((1 << (max_gen - min_gen + 1)) - 1) << min_gen;

    low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
    low_page_index_t page;
    for (page = FIXEDOBJ_RESERVED_PAGES; page <= max_used_fixedobj_page ; ++page) {
        if (fixedobj_page_wp(page) || !(fixedobj_pages[page].gens & genmask))
            continue;
        int obj_spacing = fixedobj_page_obj_align(page);
        lispobj* obj = fixedobj_page_address(page);
        lispobj* limit = compute_fixedobj_limit(obj, obj_spacing);
        int gen;
        // Immobile space can only contain objects with a header word,
        // no conses, so any fixnum where a header could be is not a live
        // object.
        do {
            if (!fixnump(*obj) && (genmask >> (gen=immobile_obj_gen_bits(obj)) & 1)) {
                if (gen == new_space) { set_visited(obj); }
                lispobj header = *obj;
                scavtab[header_widetag(header)](obj, header);
            }
        } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
    }

    // Text pages
    low_page_index_t max_used_text_page = calc_max_used_text_page();
    page = 0;
    while (page <= max_used_text_page) {
        if (!text_page_touched(page) || !(text_page_genmask[page] & genmask)) {
            ++page;
            continue;
        }
        lispobj* obj = text_page_scan_start(page);
        if (!obj) { ++page; continue; }
        do {
            lispobj* limit = (lispobj*)text_page_address(page) + WORDS_PER_PAGE;
            sword_t n_words;
            int gen;
            if (limit > text_space_highwatermark) limit = text_space_highwatermark;
            for ( ; obj < limit ; obj += n_words ) {
                lispobj header = *obj;
                // scav_code_blob will do nothing if the object isn't
                // marked as written.
                if (genmask >> (gen=immobile_obj_gen_bits(obj)) & 1) {
                    if (gen == new_space) { set_visited(obj); }
                    n_words = scavtab[header_widetag(header)](obj, header);
                } else {
                    n_words = headerobj_size2(obj, header);
                }
            }
            if (obj == text_space_highwatermark) { page = -1; break; }
            page = find_text_page_index(obj);
        } while (page > 0
                 && (text_page_genmask[page] & genmask)
                 && text_page_touched(page));
        if (page < 0) break;
    }
    if (sb_sprof_enabled) {
        // Make another pass over all code and enliven all of 'from_space'
        lispobj* where = (lispobj*)TEXT_SPACE_START;
        lispobj* limit = text_space_highwatermark;
        while (where < limit) {
            if (widetag_of(where) == CODE_HEADER_WIDETAG
                && immobile_obj_gen_bits(where) == from_space
                && code_serialno((struct code*)where) != 0)
                enliven_immobile_obj(where, 1);
            where += headerobj_size(where);
        }
    }
    scavenge_immobile_newspace();
}

void write_protect_immobile_space()
{
    immobile_scav_queue_head = 0;

    if (!ENABLE_PAGE_PROTECTION)
        return;

    // Now find contiguous ranges of pages that are protectable,
    // minimizing the number of system calls as much as possible.
    int i, start = -1, end = -1; // inclusive bounds on page indices
    low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
    for (i = max_used_fixedobj_page ; i >= 0 ; --i) {
        if (fixedobj_page_wp(i)) {
            if (end < 0) end = i;
            start = i;
        }
        if (end >= 0 && (!fixedobj_page_wp(i) || i == 0)) {
            os_protect(fixedobj_page_address(start),
                       IMMOBILE_CARD_BYTES * (1 + end - start),
                       OS_VM_PROT_READ|OS_VM_PROT_EXECUTE);
            start = end = -1;
        }
    }
}

static inline generation_index_t
pointee_gen(lispobj thing, int keep_gen, int new_gen)
{
    int to_page = find_page_index((void*)thing);
    // the default return value should be larger than any real generation number.
    int gen = 127; // generation_index_t is a signed char
    if (to_page >= 0) { // points to ordinary dynamic space
        gen = page_table[to_page].gen;
        if (gen == PSEUDO_STATIC_GENERATION+1) // scratch gen
            gen = new_gen; // is actually this
    } else if (immobile_space_p(thing)) {
        gen = immobile_obj_gen_bits(base_pointer(thing));
        if (gen == keep_gen) // keep gen
            gen = new_gen; // is actually this
    }
    return gen;
}

// 'keep_gen' is the value of the generation byte of objects that were
// candidates to become garbage, but remain live after this gc.
// It will necessarily have the VISITED flag on.
// 'new_gen' is the generation number that those objects will have
// after collection, which is either the same generation or one higher,
// depending on the 'raise' flag for this GC cycle.
static int
younger_p(lispobj thing, int gen, int keep_gen, int new_gen)
{
    return is_lisp_pointer(thing) && pointee_gen(thing, keep_gen, new_gen) < gen;
}

// Scan range between start and end (exclusive) for old-to-young pointers.
static int
range_points_to_younger_p(lispobj* obj, lispobj* end,
                          int gen, int keep_gen, int new_gen)
{
#ifdef DEBUG
  lispobj* __attribute__((unused)) saved_obj = obj, __attribute__((unused)) header = *obj;
#endif
    do {
        lispobj thing = *obj;
        if (is_lisp_pointer(thing) && pointee_gen(thing, keep_gen, new_gen) < gen)
            return 1; // yes, points to younger
    } while (++obj < end);
    return 0; // no, does not point to younger
}

// Scan a fixed-size object for old-to-young pointers.
// Since fixed-size objects are boxed and on known boundaries,
// we never start in the middle of random bytes, so the answer is exact.
static bool
fixedobj_points_to_younger_p(lispobj* obj, int n_words,
                             int gen, int keep_gen, int new_gen)
{
  lispobj layout;

  switch (widetag_of(obj)) {
  case SYMBOL_WIDETAG:
    {
    struct symbol* sym = (void*)obj;
    // Check value, info, function, linkage cell.
    // Don't need to check the symbol-name, which must be older.
    if (younger_p(sym->value, gen, keep_gen, new_gen)) return 1;
    if (younger_p(sym->info, gen, keep_gen, new_gen)) return 1;
    if (younger_p(sym->fdefn, gen, keep_gen, new_gen)) return 1;
#ifdef LISP_FEATURE_LINKAGE_SPACE
    if (younger_p(linkage_cell_taggedptr(symbol_linkage_index(sym)),
                  gen, keep_gen, new_gen)) return 1;
#endif
    }
    return 0;
  case INSTANCE_WIDETAG:
    layout = instance_layout(obj);
    if (!layout) return 0; // object can't have pointers in it yet
    if (younger_p(layout, gen, keep_gen, new_gen))
        return 1;
    struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
    gc_assert(bitmap.nwords == 1);
    if (bitmap.bits[0] != (sword_t)-1) {
        sword_t mask = bitmap.bits[0];
        lispobj* where = obj + 1;
        lispobj* limit = obj + n_words;
        for ( ; where < limit ; ++where, mask >>= 1 )
            if ((mask & 1) != 0 && younger_p(*where, gen, keep_gen, new_gen))
                return 1;
        return 0;
    }
    // FALLTHROUGH_INTENDED
  }
  return range_points_to_younger_p(obj+1, obj+n_words, gen, keep_gen, new_gen);
}

/// The next two functions are analogous to 'update_page_write_prot()'
/// but they differ in that they are "precise" - random code bytes that look
/// like pointers are not accidentally treated as pointers.

// If 'page' does not contain any objects that points to an object
// younger than themselves, then return true.
// This is called on pages that do not themselves contain objects of
// the generation being collected, but might contain pointers
// to younger generations, which we detect by a cleared WP status bit.
// The bit is cleared on any write, though, even of a non-pointer,
// so this unfortunately has to be tested much more often than we'd like.
static inline bool can_wp_fixedobj_page(page_index_t page, int keep_gen, int new_gen)
{
    int obj_spacing = fixedobj_page_obj_align(page);
    lispobj* obj = fixedobj_page_address(page);
    lispobj* limit = compute_fixedobj_limit(obj, obj_spacing);
    do {
        if (!fixnump(*obj) && // an object header
            fixedobj_points_to_younger_p(obj,
                                         headerobj_size(obj),
                                         immobile_obj_generation(obj),
                                         keep_gen, new_gen))
            return 0;
    } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
    return 1;
}

// Return 1 if any header on 'page' is in the remembered set.
static inline bool can_wp_text_page(page_index_t page)
{
    lispobj *begin = text_page_address(page);
    lispobj *end   = begin + WORDS_PER_PAGE;
    lispobj *obj   = text_page_scan_start(page);
    for ( ; obj < end ; obj += headerobj_size(obj) ) {
        gc_assert(other_immediate_lowtag_p(*obj));
        if (widetag_of(obj) == CODE_HEADER_WIDETAG && header_rememberedp(*obj))
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

    SETUP_GENS();

    low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
    low_page_index_t page;
    for (page = FIXEDOBJ_RESERVED_PAGES; page <= max_used_fixedobj_page; ++page) {
        // On pages that won't need manipulation of the freelist,
        // we try to do less work than for pages that need it.
        if (!(fixedobj_pages[page].gens & relevant_genmask)) {
            // Scan for old->young pointers, and WP if there are none.
            if (ENABLE_PAGE_PROTECTION && !fixedobj_page_wp(page)
                && fixedobj_pages[page].gens > 1
                && can_wp_fixedobj_page(page, keep_gen, new_gen)) {
                SET_WP_FLAG(page, WRITE_PROTECT);
                dprintf((logfile, "set WP(1) on fixedobj page %d (mask=%#x)\n",
                         page, fixedobj_pages[page].gens));
            }
            continue;
        }
        int obj_spacing = fixedobj_page_obj_align(page);
        page_base = fixedobj_page_address(page);
        limit = compute_fixedobj_limit(page_base, obj_spacing);
        obj = (lispobj*)page_base;
        hole = NULL;
        int any_kept = 0; // was anything moved to the kept generation
        n_holes = 0;

        // wp_it is 1 if we should try to write-protect it now.
        // If already write-protected, skip the tests.
        int wp_it = ENABLE_PAGE_PROTECTION && !fixedobj_page_wp(page);
        int gen;
        do {
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
            } else if ((gen = immobile_obj_gen_bits(obj)) == discard_gen) { // trash
                memset(obj, 0, obj_spacing);
                goto trash_it;
            } else if (gen == keep_gen) {
                assign_generation(obj, gen = new_gen);
#ifdef DEBUG
                gc_assert(!fixedobj_points_to_younger_p(obj,
                                                        headerobj_size(obj),
                                                        gen, keep_gen, new_gen));
#endif
                any_kept = -1;
            } else if (wp_it && fixedobj_points_to_younger_p(obj,
                                                             headerobj_size(obj),
                                                             gen, keep_gen, new_gen))
              wp_it = 0;
        } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
        if ( hole ) // terminate the chain of holes
            *hole = (lispobj)((char*)obj - ((char*)hole + obj_spacing));

        COMPUTE_NEW_MASK(mask, fixedobj_pages[page].gens);
        if ( mask ) {
            fixedobj_pages[page].gens = mask;
            if (wp_it) {
                SET_WP_FLAG(page, WRITE_PROTECT);
                dprintf((logfile, "set WP(2) on fixedobj page %d\n", page));
            }
        } else {
            dprintf((logfile,"page %d is all garbage\n", page));
            fixedobj_pages[page].attr.packed = 0;
        }
#ifdef DEBUG
        check_fixedobj_page(page, keep_gen, new_gen);
#endif
        dprintf((logfile,"page %d: %d holes\n", page, n_holes));
    }
    memset(fixedobj_page_hint, 0, sizeof fixedobj_page_hint);
}

// Scan for freshly trashed objects and turn them into filler.
// Lisp is responsible for consuming the free space
// when it next allocates a variable-size object.
static void
sweep_text_pages(int raise)
{
    lispobj *freelist = 0, *freelist_tail = 0;
    SETUP_GENS();

    low_page_index_t max_used_text_page = calc_max_used_text_page();
    lispobj* free_pointer = text_space_highwatermark;
    low_page_index_t page;
    for (page = 0; page <= max_used_text_page; ++page) {
        int genmask = text_page_genmask[page];
        if (!(genmask & relevant_genmask)) { // Has nothing in oldspace or newspace.
            // Scan for old->young pointers, and WP if there are none.
            if (ENABLE_PAGE_PROTECTION && text_page_touched(page)
                && text_page_genmask[page] > 1
                && can_wp_text_page(page)) {
                text_page_touched_bits[page/32] &= ~(1U<<(page & 31));
            }
            continue;
        }
        lispobj* obj = text_page_scan_start(page);
        gc_assert(obj);
        lispobj* page_base = text_page_address(page);
        lispobj* limit = page_base + WORDS_PER_PAGE;
        if (limit > free_pointer) limit = free_pointer;
        int any_kept = 0; // was anything moved to the kept generation
        // wp_it is 1 if we should try to write-protect it now.
        // If already write-protected, skip the tests.
        int wp_it = ENABLE_PAGE_PROTECTION && text_page_touched(page);
        sword_t size;
        int gen;

        for ( ; obj < limit ; obj += size ) {
            lispobj word = *obj;
            size = object_size2(obj, word);
            if (header_widetag(word) == FILLER_WIDETAG) { // ignore
            } else if ((gen = immobile_obj_gen_bits(obj)) == discard_gen) {
                gc_assert(header_widetag(word) == CODE_HEADER_WIDETAG);
                assign_widetag(obj, FILLER_WIDETAG);
                // ASSUMPTION: little-endian
                ((char*)obj)[2] = 0; // clear the TRACED flag
                ((char*)obj)[3] = 0; // clear the WRITTEN flag and the generation
                // Building the list in ascending order means less work later on
                // because the HWM will get adjusted once only, at the end.
                // Descending order would decrease the HWM for each deallocation.
                if (freelist) freelist_tail[1] = (lispobj)obj; else freelist = obj;
                freelist_tail = obj;
            } else if (gen == keep_gen) {
                assign_generation(obj, gen = new_gen);
#ifdef DEBUG
                gc_assert(!text_points_to_younger_p(obj, gen, keep_gen, new_gen,
                                                       (os_vm_address_t)page_base,
                                                       (os_vm_address_t)limit));
#endif
                any_kept = -1;
            } else if (wp_it && header_rememberedp(*obj))
                wp_it = 0;
        }
        COMPUTE_NEW_MASK(mask, text_page_genmask[page]);
        text_page_genmask[page] = mask;
        if ( mask && wp_it )
            text_page_touched_bits[page/32] &= ~(1U << (page & 31));
    }
    // Stuff the new freelist onto the front of codeblob_freelist
    if (freelist_tail) {
        freelist_tail[1] = codeblob_freelist;
        codeblob_freelist = (lispobj)freelist;
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
  sweep_text_pages(raise);
}

void gc_init_immobile()
{
#ifdef DEBUG
    logfile = stderr;
#endif
    int n_fixedobj_pages = FIXEDOBJ_SPACE_SIZE / IMMOBILE_CARD_BYTES;
    int n_text_pages = text_space_size / IMMOBILE_CARD_BYTES;
    fixedobj_pages = calloc(n_fixedobj_pages, sizeof(struct fixedobj_page));
    gc_assert(fixedobj_pages);

    n_bitmap_elts = ALIGN_UP(n_text_pages, 32) / 32;
    text_page_touched_bits = (unsigned int*)calloc(n_bitmap_elts, sizeof (int));
    gc_assert(text_page_touched_bits);
    // The conservative value for 'touched' is 1.
    memset(text_page_touched_bits, 0xff, n_bitmap_elts * sizeof (int));
    text_page_genmask = calloc(n_text_pages, 1);
    // Scav queue is arbitrarily located.
    immobile_scav_queue = malloc(QCAPACITY * sizeof(lispobj));
    tlsf_control = malloc(tlsf_size());
    tlsf_create(tlsf_control);
}

// Signify that scan_start is initially not reliable
static int page_attributes_valid;

// Set the characteristics of each used page at image startup time.
void immobile_space_coreparse(uword_t fixedobj_len,
                              __attribute__((unused)) uword_t text_len)
{
    int n_pages, word_idx, page;
    generation_index_t gen = CORE_PAGE_GENERATION;

    gc_init_immobile();

    if (gen != PSEUDO_STATIC_GENERATION) {
        // Change every object's generation. This is only for debugging.
        lispobj *where, *end;
        where = fixedobj_page_address(0);
        end = (lispobj*)((char*)where + fixedobj_len);
        while (where < end) {
            if (!fixnump(*where)) assign_generation(where, gen);
            where += object_size(where);
        }
        where = (lispobj*)TEXT_SPACE_START;
        while (where < text_space_highwatermark) {
            if (widetag_of(where) != FILLER_WIDETAG) assign_generation(where, gen);
            where += object_size(where);
        }
        // If the regression suite is run with core pages in gen0 (to more aggressively
        // test code page transporting), we don't want to cause failures in 'script.test.sh'
        // and some other things that look for an exact match on textual output.
        if (gencgc_verbose)
            fprintf(stderr, "WARNING: demoted immobile objects to gen%d\n", gen);
    }

    n_pages = fixedobj_len / IMMOBILE_CARD_BYTES;
    for (page = 0; page <= FIXEDOBJ_RESERVED_PAGES; ++page) {
        // set page attributes that can't match anything in get_freeish_page()
        fixedobj_pages[page].attr.parts.obj_align = 1;
        if (gen != 0 && ENABLE_PAGE_PROTECTION)
            fixedobj_pages[page].attr.parts.flags = WRITE_PROTECT;
        fixedobj_pages[page].gens |= 1 << gen;
    }
    for (page = FIXEDOBJ_RESERVED_PAGES ; page < n_pages ; ++page) {
        lispobj* page_data = fixedobj_page_address(page);
        for (word_idx = 0 ; word_idx < WORDS_PER_PAGE ; ++word_idx) {
            lispobj* obj = page_data + word_idx;
            lispobj header = *obj;
            if (!fixnump(header)) {
                gc_assert(other_immediate_lowtag_p(*obj));
                sword_t size = object_size2(obj, header);
                fixedobj_pages[page].attr.parts.obj_align = size;
                fixedobj_pages[page].gens |= 1 << immobile_obj_gen_bits(obj);
                if (gen != 0 && ENABLE_PAGE_PROTECTION)
                    fixedobj_pages[page].attr.parts.flags = WRITE_PROTECT;
                break;
            }
        }
    }
    if (!TEXT_SPACE_START) {
        // Don't use the space. Free pointer was initialized to the nominal
        // base address. Set the start to that also so that map-objects-in-range
        // sees no used space, and find_text_page_index() returns -1;
        TEXT_SPACE_START = (uword_t)text_space_highwatermark;
        text_space_size = 0;
        page_attributes_valid = 1; // make search_immobile_space() work right
        return;
    }
    // coreparse() already set text_space_highwatermark
    gc_assert(text_space_highwatermark != 0 /* would be zero if not mmapped yet */
              && text_space_highwatermark <= (lispobj*)(TEXT_SPACE_START + text_len));
    gc_assert(PTR_ALIGN_UP(text_space_highwatermark, IMMOBILE_CARD_BYTES) == text_space_highwatermark);
    tlsf_mem_start = text_space_highwatermark;
    struct vector* v = VECTOR(SYMBOL(IMMOBILE_CODEBLOB_VECTOR)->value);
    gc_assert(widetag_of((lispobj*)v) == SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG);
    // The vector itself is either in R/O space, or pseudo-static in dynamic space
    // depending on :PURIFY
    if(gencgc_verbose) fprintf(stderr, "pseudostatic codeblob vector is %p\n", v);
    loaded_codeblob_offsets = (void*)v->data;
    loaded_codeblob_offsets_len = vector_len(v);
    // Don't use text_len to compute n_pages because that measures backend pages (32k),
    // but we're trying to compute _allocator_ pages.
    n_pages = ((uword_t)text_space_highwatermark-TEXT_SPACE_START) / IMMOBILE_CARD_BYTES;
    // Set the generation mask only on pages that contain some object header
    // (not merely the interior of an object)
    for (page = 0; page < n_pages ; ++page)
        if (text_page_scan_start(page)) text_page_genmask[page] |= 1<<gen;

    // Create a TLSF pool
    char *tlsf_memory_end = (char*)TEXT_SPACE_START + text_space_size;
    int tlsf_memory_size = tlsf_memory_end - (char*)tlsf_mem_start;
    tlsf_add_pool(tlsf_control, tlsf_mem_start, tlsf_memory_size);
    int n_tlsf_pages = tlsf_memory_size / IMMOBILE_CARD_BYTES;
    tlsf_page_sso = malloc(n_tlsf_pages * sizeof (short int));
    memset(tlsf_page_sso, 0xff, n_tlsf_pages * sizeof (short int));

    // Set the WP bits for pages occupied by the core file.
    // (There can be no inter-generation pointers.)
    if (gen != 0 && ENABLE_PAGE_PROTECTION) {
        low_page_index_t page;
        for (page = 0 ; page <= n_pages ; ++page)
            text_page_touched_bits[page/32] &= ~(1U<<(page & 31));
    }
    page_attributes_valid = 1;
}

void deport_codeblob_offsets_from_heap()
{
    /* This vector is needed for the final GC to compute page scan starts
     * but we don't want to preserve it in a lisp space (dynamic or R/O)
     * during that GC.
     * So replicate it off-heap; it'll get made anew after defrag. */
    int nbytes = sizeof (uint32_t) * loaded_codeblob_offsets_len;
    lispobj* vector_copy = malloc(nbytes);
    loaded_codeblob_offsets = memcpy(vector_copy, loaded_codeblob_offsets, nbytes);
    SYMBOL(IMMOBILE_CODEBLOB_VECTOR)->value = NIL;
}

// Change all objects to generation 0
void prepare_immobile_space_for_final_gc()
{
    int page;
    char* page_base;
    char* page_end = (char*)fixedobj_free_pointer;

    gc_assert(!gc_managed_heap_space_p((lispobj)loaded_codeblob_offsets));
    for (page = 0, page_base = fixedobj_page_address(page) ;
         page_base < page_end ;
         page_base += IMMOBILE_CARD_BYTES, ++page) {
        lispobj* obj = (lispobj*)page_base;
        lispobj* limit = (lispobj*)((uword_t)obj + IMMOBILE_CARD_BYTES);
        // Never set the generation mask on an empty page!
        int spacing = fixedobj_page_obj_align(page) >> WORD_SHIFT;
        if (spacing) {
            for ( ; obj < limit ; obj += spacing ) {
                if (other_immediate_lowtag_p(*obj) && immobile_obj_gen_bits(obj) != 0)
                    assign_generation(obj, 0);
            }
            fixedobj_pages[page].gens = 1; // only generation 0 is present
        }
    }

    lispobj* obj = (lispobj*)TEXT_SPACE_START;
    lispobj* limit = text_space_highwatermark;
    int npages = (ALIGN_UP((uword_t)limit, IMMOBILE_CARD_BYTES) - TEXT_SPACE_START)
                 / IMMOBILE_CARD_BYTES;
    memset(text_page_genmask, 0, npages);
    for ( ; obj < limit ; obj += headerobj_size(obj) ) {
        if (widetag_of(obj) != FILLER_WIDETAG) {
            assign_generation(obj, 0);
            text_page_genmask[find_text_page_index(obj)] = 1;
        }
    }
    SYMBOL(IMMOBILE_CODEBLOB_TREE)->value = NIL;
}

uword_t* code_component_order;

int compute_codeblob_offsets_nwords(int* pcount)
{
    int count = 0;
    lispobj* obj = (lispobj*)TEXT_SPACE_START;
    for ( ;  obj < text_space_highwatermark ; obj += headerobj_size(obj) ) ++count;
    if (pcount) *pcount = count;
    int n_data_words = ALIGN_UP(count, 2) >> 1;
    return 2 + ALIGN_UP(n_data_words, 2);
}

/* Defragment the immobile space, and then promote all objects to gen6.
 * 'coreparse' causes all pages in dynamic space to be pseudo-static, but
 * each immobile object stores its own generation, so this must be done at
 * save time, or else it would require touching every object on startup */
void prepare_immobile_space_for_save(bool verbose)
{
    if (verbose) {
        printf("[defragmenting immobile space... ");
        fflush(stdout);
    }

#ifdef LISP_FEATURE_X86_64
    defrag_immobile_space(verbose);
#endif

    lispobj* obj = (lispobj*)FIXEDOBJ_SPACE_START;
    lispobj* limit = fixedobj_free_pointer;
    while (obj < limit) {
        if (other_immediate_lowtag_p(*obj))
            assign_generation(obj, PSEUDO_STATIC_GENERATION);
        obj += object_size(obj);
    }

    for ( obj = (lispobj*)TEXT_SPACE_START ;  obj < text_space_highwatermark ; obj += headerobj_size(obj) ) {
        *obj &= ~(uword_t)block_header_prev_free_bit;
        assign_generation(obj, PSEUDO_STATIC_GENERATION);
    }
    // Create a vector of code offsets
    int codeblob_count;
    int vector_nwords = compute_codeblob_offsets_nwords(&codeblob_count);
    struct vector* v;
    if (read_only_space_free_pointer > (lispobj*)READ_ONLY_SPACE_START) {
        v = (void*)read_only_space_free_pointer;
        read_only_space_free_pointer += vector_nwords;
        if (read_only_space_free_pointer > (lispobj*)READ_ONLY_SPACE_END)
            lose("Didn't reserve enough R/O space?");
    } else {
        v = gc_general_alloc(unboxed_region, vector_nwords<<WORD_SHIFT,
                             PAGE_TYPE_UNBOXED);
        // might have used large-object pages, no region
        ensure_region_closed(unboxed_region, PAGE_TYPE_UNBOXED);
        // gc_general_alloc generally avoids pre-zeroizing, so ensure zero-fill to the end
        // of dynamic space as we've aready performed zero_all_free_ranges().
        uword_t vector_end = (uword_t)((lispobj*)v + vector_nwords),
                aligned_end = ALIGN_UP(vector_end, BACKEND_PAGE_BYTES);
        memset((char*)vector_end, 0, aligned_end-vector_end);
    }
    v->header = SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG;
    v->length_ = make_fixnum(codeblob_count);
    SYMBOL(IMMOBILE_CODEBLOB_VECTOR)->value = make_lispobj(v, OTHER_POINTER_LOWTAG);
    uint32_t* data = (uint32_t*)v->data;
    int i = 0;
    for ( obj = (lispobj*)TEXT_SPACE_START ; obj < text_space_highwatermark ; obj += headerobj_size(obj) ) {
        data[i++] = (int)((uword_t)obj - TEXT_SPACE_START);
    }
    // Write a filler if needed to align tlsf_mem_start
    lispobj* aligned_hwm = PTR_ALIGN_UP(text_space_highwatermark, IMMOBILE_CARD_BYTES);
    if (text_space_highwatermark < aligned_hwm) {
        *text_space_highwatermark = make_filler_header(aligned_hwm - text_space_highwatermark);
        text_space_highwatermark = aligned_hwm;
    }
    if (verbose) printf("done]\n");
}

//// Interface

int immobile_space_handle_wp_violation(void* fault_addr)
{
    low_page_index_t fixedobj_page_index = find_fixedobj_page_index(fault_addr);
    if (fixedobj_page_index < 0)
      return 0; // unhandled

    os_protect(PTR_ALIGN_DOWN(fault_addr, IMMOBILE_CARD_BYTES),
               IMMOBILE_CARD_BYTES, OS_VM_PROT_ALL);

    // FIXME: the _CLEARED flag doesn't achieve much if anything.
    if (!(fixedobj_pages[fixedobj_page_index].attr.parts.flags
          & (WRITE_PROTECT|WRITE_PROTECT_CLEARED)))
        return 0;
    SET_WP_FLAG(fixedobj_page_index, WRITE_PROTECT_CLEARED);

    return 1;
}

/// For defragmentation

static struct tempspace {
  char* start;
  int n_bytes;
} fixedobj_tempspace, text_tempspace;

// Find the object that encloses pointer.
lispobj *
search_immobile_space(void *pointer)
{
    if ((void*)TEXT_SPACE_START <= pointer && pointer < (void*)text_space_highwatermark) {
        if (!page_attributes_valid)lose("Can't search");
        return search_immobile_code(pointer);
    } else if ((void*)FIXEDOBJ_SPACE_START <= pointer
               && pointer < (void*)fixedobj_free_pointer) {
        low_page_index_t page_index = find_fixedobj_page_index(pointer);
        char *page_base = PTR_ALIGN_DOWN(pointer, IMMOBILE_CARD_BYTES);
        // Page attributes are inadequate to represent the size of objects
        // on the reserved page (of which there is only 1 at present).
        // In addition, there are actually differently sized objects on the page.
        // We don't really care about dissimilar sizes, since it is not used as
        // a root for scavenging. It resembles READ-ONLY space in that regard.
        if (page_attributes_valid && page_index >= FIXEDOBJ_RESERVED_PAGES) {
            int spacing = fixedobj_page_obj_align(page_index);
            if (spacing == 0) return NULL;
            int index = ((char*)pointer - page_base) / spacing;
            lispobj *obj = (void*)(page_base + spacing * index);
            char* end;
            /* When defragmenting, there are forwarding pointers in object headers
             * so the sizing functions don't work. Following the forwarding pointer
             * isn't right, because it points to where the object _will_ be,
             * but it isn't actually there. So we have to conservatively assume
             * that the object size is the object alignment.
             * It all other situations, it is OK to call the sizing function. */
            if (fixedobj_tempspace.start) // defragmenting
                end = (char*)obj + spacing;
            else
                end = (char*)(obj + object_size(obj));
            if ((char*)pointer < end) return obj;
        } else {
            return gc_search_space((lispobj*)page_base, pointer);
        }
    }
    return NULL;
}

//// Defragmentation

// Given an address in the target core, return the equivalent
// physical address to read or write during defragmentation
static lispobj* tempspace_addr(void* address)
{
    gc_assert(immobile_space_p((lispobj)address));
    if (find_fixedobj_page_index(address) >= 0) {
        if (fixedobj_tempspace.n_bytes == 0) return address;
        int byte_index = (char*)address - (char*)FIXEDOBJ_SPACE_START;
        gc_assert(byte_index < fixedobj_tempspace.n_bytes);
        return (void*)(fixedobj_tempspace.start + byte_index);
    } else { // text subspace
        int byte_index = (char*)address - (char*)TEXT_SPACE_START;
        gc_assert(byte_index < text_tempspace.n_bytes);
        return (void*)(text_tempspace.start + byte_index);
    }
}

static void copy_back(uword_t space_start, struct tempspace* tempspace,
                      lispobj** pfree_ptr)
{
    lispobj* old_free_ptr = *pfree_ptr;
    int old_usage = (uword_t)old_free_ptr - space_start;
    int new_usage = tempspace->n_bytes;
    memcpy((char*)space_start, tempspace->start, tempspace->n_bytes);
    uword_t end = space_start + new_usage;
    if (new_usage < old_usage) memset((char*)end, 0, old_usage - new_usage);
    *pfree_ptr = (lispobj*)end;
}

static inline bool tempspace_p(char* addr)
{
    return (addr >= fixedobj_tempspace.start &&
            addr < fixedobj_tempspace.start + fixedobj_tempspace.n_bytes)
        || (addr >= text_tempspace.start &&
            addr < text_tempspace.start + text_tempspace.n_bytes);
}
static inline bool known_space_p(lispobj ptr)
{
    return find_page_index((char*)ptr) >= 0
        || tempspace_p((char*)ptr)
        || immobile_space_p(ptr)
        || (STATIC_SPACE_OBJECTS_START <= ptr && ptr < STATIC_SPACE_END);
}
static bool forwardable_ptr_p(lispobj ptr)
{
    return is_lisp_pointer(ptr) &&
           known_space_p(ptr) &&
           forwarding_pointer_p(native_pointer(ptr));
}

static void adjust_words(lispobj *where, sword_t n_words)
{
    int i;
    for (i=0;i<n_words;++i) {
        lispobj ptr = where[i];
        if (forwardable_ptr_p(ptr))
            where[i] = forwarding_pointer_value(native_pointer(ptr));
    }
}

static void adjust_closure_entrypoint(lispobj* slot)
{
    lispobj raw_addr = *slot;
    // closure tramp and fin tramp don't have a simple-fun header.
    // Do not examine the word where the header would be,
    // since it could confuse adjust_words() by having a bit pattern
    // resembling a FP. (It doesn't, but better safe than sorry)
    if (asm_routines_start <= raw_addr && raw_addr < asm_routines_end) return;
    lispobj simple_fun = fun_taggedptr_from_self(raw_addr);
    adjust_words(&simple_fun, 1);
    *slot = fun_self_from_taggedptr(simple_fun);
}

/* Fix the layout of OBJ, storing it back to the object,
 * and return the layout's address in tempspace. */
static struct layout* fix_object_layout(lispobj* obj)
{
#ifndef LISP_FEATURE_COMPACT_INSTANCE_HEADER
    // Without compact instance headers, there are no layouts in immobile-space,
    return LAYOUT(layout_of(obj));
#endif
    gc_assert(instanceoid_widetag_p(widetag_of(obj)));
    lispobj layout = layout_of(obj);
    if (layout == 0) return 0;
    if (forwarding_pointer_p(native_pointer(layout))) { // usually
        layout = forwarding_pointer_value(native_pointer(layout));
        layout_of(obj) = layout;
    }
    struct layout* native_layout = (struct layout*)tempspace_addr(LAYOUT(layout));
    gc_assert(header_widetag(native_layout->header) == INSTANCE_WIDETAG);
    gc_assert(layoutp(make_lispobj(native_layout, INSTANCE_POINTER_LOWTAG)));
    return native_layout;
}

static void apply_absolute_fixups(lispobj, struct code*);

static void __attribute__((unused)) adjust_linkage_cell(int linkage_index)
{
    if (!linkage_index) return;
    char* entrypoint = (char*)linkage_space[linkage_index];
    if (!entrypoint || find_page_index(entrypoint)>=0) return;
    lispobj* base = (lispobj*)(entrypoint - 2*N_WORD_BYTES);
    gc_assert(forwarding_pointer_p(base)); // all of immobile text is forwarded
    lispobj fp = forwarding_pointer_value(base);
    gc_assert(lowtag_of(fp) == FUN_POINTER_LOWTAG &&
              widetag_of(tempspace_addr(native_pointer(fp))) == SIMPLE_FUN_WIDETAG);
    linkage_space[linkage_index] = (lispobj)(native_pointer(fp) + 2);
}

/// It's tricky to try to use the scavtab[] functions for fixing up moved
/// objects, because scavenger functions might invoke transport functions.
/// The best approach is to do an explicit switch over all object types.
static void fixup_space(lispobj* where, size_t n_words)
{
    lispobj* end = where + n_words;
    int widetag;
    sword_t size;
    struct code* code;

    while (where < end) {
        gc_assert(!forwarding_pointer_p(where));
        lispobj header_word = *where;
        if (!is_header(header_word)) {
            adjust_words(where, 2); // A cons. (It can only be filler?)
            where += 2;
            continue;
        }
        size = headerobj_size2(where, header_word);
        widetag = header_widetag(header_word);
        switch (widetag) {
        default:
          if (!leaf_obj_widetag_p(widetag))
            lose("Unhandled widetag in fixup_space: %p", (void*)header_word);
          break;
        case INSTANCE_WIDETAG:
        case FUNCALLABLE_INSTANCE_WIDETAG:
          {
          lispobj* slots = where+1;
          int i;
          struct bitmap bitmap = get_layout_bitmap(fix_object_layout(where));
          for(i=0; i<(size-1); ++i)
              if (bitmap_logbitp(i, bitmap)) adjust_words(slots+i, 1);
          }
          break;
        case CODE_HEADER_WIDETAG:
          // Fixup the constant pool.
          code = (struct code*)where;
          adjust_words(where+2, code_header_words(code)-2);
#ifdef LISP_FEATURE_X86_64
          apply_absolute_fixups(code->fixups, code);
#endif
          break;
        case CLOSURE_WIDETAG:
          adjust_closure_entrypoint(where+1);
          adjust_words(where+2, size-2);
          break;
        case FDEFN_WIDETAG: {
          struct fdefn *fdefn = (void*)where;
          adjust_words(&fdefn->name, 1); // name can be a symbol
          adjust_words(&fdefn->fun, 1);
#ifdef LISP_FEATURE_LINKAGE_SPACE
          adjust_linkage_cell(fdefn_linkage_index(fdefn));
#endif
          break;
        }
        case SYMBOL_WIDETAG: {
          // - info, name, package can not point to an immobile object
          struct symbol* s = (struct symbol*)where;
          adjust_words(&s->value, 1);
          adjust_words(&s->fdefn, 1);
#ifdef LISP_FEATURE_LINKAGE_SPACE
          adjust_linkage_cell(symbol_linkage_index(s));
#endif
          break;
        }
        // Special case because we might need to mark hashtables
        // as needing rehash.
        case SIMPLE_VECTOR_WIDETAG:
          if (vector_flagp(header_word, VectorAddrHashing)) {
              struct vector* kv_vector = (struct vector*)where;
              lispobj* data = kv_vector->data;
              gc_assert(vector_len(kv_vector) >= 5);
              bool needs_rehash = 0;
              unsigned int hwm = KV_PAIRS_HIGH_WATER_MARK(data);
              unsigned int i;
              // FIXME: we're disregarding the hash vector.
              for (i = 1; i <= hwm; ++i) {
                  lispobj ptr = data[2*i];
                  if (forwardable_ptr_p(ptr)) {
                      data[2*i] = forwarding_pointer_value(native_pointer(ptr));
                      needs_rehash = 1;
                  }
                  ptr = data[2*i+1];
                  if (forwardable_ptr_p(ptr))
                      data[2*i+1] = forwarding_pointer_value(native_pointer(ptr));
              }
              if (needs_rehash)
                  KV_PAIRS_REHASH(data) |= make_fixnum(1);
              break;
          }
        // INTENTIONAL FALLTHROUGH
        // All the other array header widetags.
        case SIMPLE_ARRAY_WIDETAG:
#ifdef COMPLEX_CHARACTER_STRING_WIDETAG
        case COMPLEX_CHARACTER_STRING_WIDETAG:
#endif
        case COMPLEX_BASE_STRING_WIDETAG:
        case COMPLEX_BIT_VECTOR_WIDETAG:
        case COMPLEX_VECTOR_WIDETAG:
        case COMPLEX_ARRAY_WIDETAG:
        // And the other entirely boxed objects.
        case VALUE_CELL_WIDETAG:
        case WEAK_POINTER_WIDETAG:
        case RATIO_WIDETAG:
        case COMPLEX_RATIONAL_WIDETAG:
          // Use the sizing functions for generality.
          // Symbols can contain strange header bytes,
          // and vectors might have a padding word, etc.
          adjust_words(where+1, size-1);
          break;
        }
        where += size;
    }
}

uword_t* immobile_space_reloc_index;
uword_t* immobile_space_relocs;

// Take and return an untagged pointer, or 0 if the object did not survive GC.
static lispobj* get_load_address(lispobj* old)
{
    if (forwarding_pointer_p(old))
        return native_pointer(forwarding_pointer_value(old));
    gc_assert(widetag_of(old) == FILLER_WIDETAG);
    return 0;
}

#if DEFRAGMENT_FIXEDOBJ_SUBSPACE
#define N_SYMBOL_KINDS 4

// Return an integer 0..3 telling which block of symbols to relocate 'sym' into.
//  0=uninterned, 1=keyword, 2=special var, 3=other
static int classify_symbol(lispobj* obj)
{
    struct symbol* symbol = (struct symbol*)obj;
    if (symbol_package_id(symbol) == PACKAGE_ID_NONE) return 0;
    if (symbol_package_id(symbol) == PACKAGE_ID_KEYWORD) return 1;
    struct vector* symbol_name = VECTOR(decode_symbol_name(symbol->name));
    if (vector_len(symbol_name) >= 2 &&
        schar(symbol_name, 0) == '*' &&
        schar(symbol_name, vector_len(symbol_name)-1) == '*')
        return 2;
    return 3;
}

static inline char* compute_defrag_start_address()
{
    // The first fixedobj page contains some essential layouts,
    // the addresses of which might be wired in by code generation.
    // As such they must never move.
    return (char*)FIXEDOBJ_SPACE_START + 2*IMMOBILE_CARD_BYTES;

}

static int calc_n_fixedobj_pages(int n_objects, int words_per_object)
{
  words_per_object = ALIGN_UP(words_per_object, 2);
  int objects_per_page = WORDS_PER_PAGE / words_per_object;
  return (n_objects + objects_per_page - 1) / objects_per_page;
}

/// Copy a fixed-size object somewhere. "somewhere" could be either a fixed-
/// or variable-sized object page. The new copy never crosses page boundaries.
/// The original object may reside in either text or fixedobj space.
static void place_fixedobj(lispobj* obj, int size_in_bytes,
                           char *alloc_ptrs[64], char *symbol_alloc_ptr[])
{
      lispobj word = *obj;
      int widetag = header_widetag(word);
      char** alloc_ptr;
      if (widetag == SYMBOL_WIDETAG)
          alloc_ptr = &symbol_alloc_ptr[classify_symbol(obj)];
      else if (!(alloc_ptr = &alloc_ptrs[widetag>>2]))
          lose("Unexpected widetag %x", widetag);
      char* new = *alloc_ptr;
      char* page_end =
          PTR_ALIGN_DOWN(new, IMMOBILE_CARD_BYTES) + IMMOBILE_CARD_BYTES;
      // compute space remaining, possibly negative,
      // if this object were placed on the page.
      int space_avail = (page_end - new) - size_in_bytes;
      if (space_avail < 0) new = page_end;
      gc_assert(!*tempspace_addr(new)); // better not clobber some other object
      memcpy(tempspace_addr(new), obj, size_in_bytes);
      set_forwarding_pointer(obj, make_lispobj(new, LOWTAG_FOR_WIDETAG(widetag)));
      *alloc_ptr = new + size_in_bytes;
}

/// Like above, but specifically for layouts;
struct size_class {
    char* alloc_ptr;
    /* Initially this is a count of the number of layouts in the size class.
     * After computing the total number of layout pages needed, this becomes a count
     * of how many objects can be allocated starting from 'alloc_ptr' without
     * overflowing the page. When zero, we grab the next available page. */
    int count;
};

/* Defragmentation needs more size classes than allocation because a
 * restarted core can not discern the original (coarser) size class
 * in which a layout was allocated. It can only use the actual size.
 * 2n+8 words for N from 0..20 gives a range from 8 to 48 words */
#define MAX_LAYOUT_DEFRAG_SIZE_CLASSES 21
static inline int layout_size_class_nwords(int index) {
    return 8 + 2*index ;
}
static inline int nwords_to_layout_size_class(unsigned int nwords) {
    // the smallest layout size class is 8 words
    int index = nwords <= 8 ? 0 : (nwords - 8)/2;
    if (index >= MAX_LAYOUT_DEFRAG_SIZE_CLASSES)
        lose("Oversized layout: can't defragment");
    return index;
}

static void place_layout(lispobj* obj,
                         struct size_class size_classes[],
                         char** alloc_ptr,
                         __attribute__((unused)) char *alloc_ptr_limit)
{
    // Layouts may occur in different sizes.
    int nwords = 1 + (instance_length(*obj) | 1);
    int size_class_index = nwords_to_layout_size_class(nwords);
    if (size_classes[size_class_index].count == 0) {
        gc_assert(*alloc_ptr <= alloc_ptr_limit);
        size_classes[size_class_index].alloc_ptr = *alloc_ptr;
        size_classes[size_class_index].count = (IMMOBILE_CARD_BYTES>>WORD_SHIFT) / nwords;
        *alloc_ptr += IMMOBILE_CARD_BYTES;
    } else {
        gc_assert(size_classes[size_class_index].alloc_ptr != 0);
        gc_assert(size_classes[size_class_index].count > 0);
    }
    size_classes[size_class_index].count--;
    char* new = size_classes[size_class_index].alloc_ptr;
    gc_assert(!*tempspace_addr(new));
    memcpy(tempspace_addr(new), obj, nwords*N_WORD_BYTES);
    set_forwarding_pointer(obj, make_lispobj(new, INSTANCE_POINTER_LOWTAG));
    // Use nbytes for the defragmentation size class when bumping the pointer,
    // and not the object spacing for the page on which obj resides.
    size_classes[size_class_index].alloc_ptr +=
        layout_size_class_nwords(size_class_index) << WORD_SHIFT;
}

static void defrag_immobile_space(bool verbose)
{
    int i;

    uword_t *components = code_component_order;

    // Count the number of symbols and layouts that will be relocated
    int obj_type_histo[64];
    struct { int size, count; } sym_kind_histo[N_SYMBOL_KINDS];
    memset(obj_type_histo, 0, sizeof obj_type_histo);
    memset(sym_kind_histo, 0, sizeof sym_kind_histo);
    struct size_class layout_size_class[MAX_LAYOUT_DEFRAG_SIZE_CLASSES];
    memset(layout_size_class, 0, sizeof layout_size_class);

#if DEFRAGMENT_FIXEDOBJ_SUBSPACE
    // Find the starting address of fixed-size objects that will undergo defrag.
    // Never move the first pages of LAYOUTs created by genesis.
    // so that codegen can wire in layout of function with less pain.
    char* defrag_base = compute_defrag_start_address();
    low_page_index_t page_index = find_fixedobj_page_index(defrag_base);
    low_page_index_t max_used_fixedobj_page = calc_max_used_fixedobj_page();
    for ( ; page_index <= max_used_fixedobj_page ; ++page_index) {
        int obj_spacing = fixedobj_page_obj_align(page_index);
        if (obj_spacing) {
            lispobj* obj = fixedobj_page_address(page_index);
            lispobj* limit = compute_fixedobj_limit(obj, obj_spacing);
            do {
                lispobj word = *obj;
                if (!fixnump(word)) {
                    int widetag = header_widetag(word);
                    int size = sizetab[widetag](obj);
                    ++obj_type_histo[widetag/4];
                    switch (widetag) {
                    case SYMBOL_WIDETAG:
                        {
                        int kind = classify_symbol(obj);
                        ++sym_kind_histo[kind].count;
                        if (!sym_kind_histo[kind].size)
                            sym_kind_histo[kind].size = size;
                        gc_assert(sym_kind_histo[kind].size == size);
                        }
                        break;
                    case INSTANCE_WIDETAG:
                        gc_assert(layoutp(make_lispobj(obj, INSTANCE_POINTER_LOWTAG)));
                        int class_index = nwords_to_layout_size_class(size);
                        ++layout_size_class[class_index].count;
                        break;
                    default:
                        lose("Unexpected header %lx on fixedobj page @ %p", word, obj);
                    }
                }
            } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
        }
    }

    // Calculate space needed for fixedobj pages after defrag.
    // page order is: layouts, symbols.
    int n_layout_pages = 0;
    int class_index;
    for (class_index = 0; class_index < MAX_LAYOUT_DEFRAG_SIZE_CLASSES; ++class_index) {
        int count = layout_size_class[class_index].count;
        n_layout_pages += calc_n_fixedobj_pages(count, layout_size_class_nwords(class_index));
        layout_size_class[class_index].count = 0;
    }

    char* layout_alloc_ptr = defrag_base;
    char* symbol_alloc_ptrs[N_SYMBOL_KINDS+1];
    symbol_alloc_ptrs[0]    = layout_alloc_ptr + n_layout_pages * IMMOBILE_CARD_BYTES;
    for (i=0; i<N_SYMBOL_KINDS ; ++i)
      symbol_alloc_ptrs[i+1] =
          symbol_alloc_ptrs[i] + calc_n_fixedobj_pages(
              sym_kind_histo[i].count, sym_kind_histo[i].size) * IMMOBILE_CARD_BYTES;
    fixedobj_tempspace.n_bytes = symbol_alloc_ptrs[N_SYMBOL_KINDS] - (char*)FIXEDOBJ_SPACE_START;
    fixedobj_tempspace.start = calloc(fixedobj_tempspace.n_bytes, 1);

    // Copy pages below the defrag base into the temporary copy.
    memcpy(fixedobj_tempspace.start, (char*)FIXEDOBJ_SPACE_START,
           (lispobj)defrag_base - FIXEDOBJ_SPACE_START);
#endif

    // Compute where each code component will be moved to.
    int n_code_components = 0;
    int n_code_bytes = 0;

    if (components) {
        for (i=0 ; components[i*2] ; ++i) {
            lispobj* addr = (lispobj*)components[i*2];
            gc_assert(lowtag_of((lispobj)addr) == OTHER_POINTER_LOWTAG);
            addr = native_pointer((lispobj)addr);
            int widetag = widetag_of(addr);
            gc_assert(widetag == CODE_HEADER_WIDETAG || widetag == FILLER_WIDETAG);
            lispobj new_vaddr = 0;
            // A code component can become garbage in the final GC
            // (defrag happens after the last GC) leaving a filler object
            // which was in components[] because it was live before GC.
            if (widetag == CODE_HEADER_WIDETAG) {
                ++n_code_components;
                new_vaddr = TEXT_SPACE_START + n_code_bytes;
                n_code_bytes += sizetab[widetag](addr) << WORD_SHIFT;
            }
            components[i*2+1] = new_vaddr;
        }
    }
    text_tempspace.n_bytes = n_code_bytes;
    text_tempspace.start = calloc(text_tempspace.n_bytes, 1);

    if (verbose)
        printf("(inst,code,sym)=%d+%d+%d... ",
               obj_type_histo[INSTANCE_WIDETAG/4],
               obj_type_histo[CODE_HEADER_WIDETAG/4] +  n_code_components,
               obj_type_histo[SYMBOL_WIDETAG/4]);

    if (components) {
        // Permute text space into tempspace and deposit forwarding pointers.
        lispobj new_vaddr;
        for (i=0 ; components[i*2] ; ++i) {
            if ((new_vaddr = components[i*2+1]) == 0) continue;
            lispobj* addr = native_pointer(components[i*2]);
            gc_assert(widetag_of(addr) == CODE_HEADER_WIDETAG);
            memcpy(tempspace_addr((void*)new_vaddr), addr,
                   headerobj_size(addr) << WORD_SHIFT);
            int displacement = new_vaddr - (lispobj)addr;
            for_each_simple_fun(index, fun, (struct code*)addr, 1, {
                struct simple_fun *new_fun =
                    (struct simple_fun*)((char*)fun + displacement);
                // Fix the 'self' slot for the new logical address,
                // storing via the current (temporary) address.
                ((struct simple_fun*)tempspace_addr(new_fun))->self =
                    fun_self_from_baseptr(new_fun);
                set_forwarding_pointer((lispobj*)fun,
                                       make_lispobj(new_fun, FUN_POINTER_LOWTAG));
            });
            // Fix any absolute jump tables
            lispobj* jump_table =
                code_jumptable_start((struct code*)tempspace_addr((void*)new_vaddr));
            int count = jumptable_count(jump_table);
            int i;
            for (i = 1; i < count; ++i) if (jump_table[i]) jump_table[i] += displacement;
            set_forwarding_pointer(addr, make_lispobj((void*)new_vaddr,
                                                      OTHER_POINTER_LOWTAG));
        }
    }

#if DEFRAGMENT_FIXEDOBJ_SUBSPACE
    char* alloc_ptrs[64];
    memset(alloc_ptrs, 0, sizeof alloc_ptrs);
    alloc_ptrs[INSTANCE_WIDETAG/4] = layout_alloc_ptr;

    // Permute fixed-sized object pages and deposit forwarding pointers.
    for ( page_index = find_fixedobj_page_index(defrag_base) ;
          page_index <= max_used_fixedobj_page ; ++page_index) {
        int obj_spacing = fixedobj_page_obj_align(page_index);
        if (!obj_spacing) continue;
        lispobj* obj = fixedobj_page_address(page_index);
        lispobj* limit = compute_fixedobj_limit(obj, obj_spacing);
        do {
            if (fixnump(*obj)) continue;
            if (widetag_of(obj) == INSTANCE_WIDETAG) {
                place_layout(obj, layout_size_class,
                             &layout_alloc_ptr, symbol_alloc_ptrs[0]);
            } else {
                place_fixedobj(obj, obj_spacing, alloc_ptrs, symbol_alloc_ptrs);
            }
         } while (NEXT_FIXEDOBJ(obj, obj_spacing) <= limit);
    }
#endif  /* DEFRAGMENT_FIXEDOBJ_SUBSPACE */

    if (immobile_space_reloc_index) {
#ifdef LISP_FEATURE_X86_64
        // Fix displacements in JMP, CALL, and LEA instructions in code objects.
        // There are 2 arrays in use:
        //  - the relocs[] array contains the offset of any machine instruction
        //    that needs to be altered on account of space relocation. Offsets are
        //    relative to base address of code, not the code-instructions.
        //    After the offset is the lisp object referred to by the instruction.
        //  - the reloc_index[] array identifes the code component each reloc belongs to.
        //    It is an array of pairs:
        //      comp_ptr1, index1, comp_ptr2, index2 ... comp_ptrN, indexN, 0, index_max
        //    The index following a component specifies the starting index within the
        //    relocs[] array of the first reloc belonging to the respective component.
        //    The ending reloc can be deduced from the next component's first reloc.
        for (i = 0 ; immobile_space_reloc_index[i*2] ; ++i) {
            lispobj code = immobile_space_reloc_index[i*2] - OTHER_POINTER_LOWTAG;
            lispobj load_addr;
            if (code >= READ_ONLY_SPACE_START && code < READ_ONLY_SPACE_END)
                load_addr = code; // This code can not be moved or GCed.
            else
                load_addr = (lispobj)get_load_address((lispobj*)code);
            if (!load_addr) continue;  // Skip code that was dropped by GC
            int reloc_index     = immobile_space_reloc_index[i*2+1];
            int end_reloc_index = immobile_space_reloc_index[i*2+3];
            for ( ; reloc_index < end_reloc_index ; reloc_index += 2) {
                int offset = immobile_space_relocs[reloc_index];
                char* inst_addr = (char*)code + offset;
                __attribute__((unused)) lispobj obj = immobile_space_relocs[reloc_index+1];
                gc_assert(obj == 0 || obj == NIL);
                // Adjust the displacement by (old - new). Jump target can't move.
                // If the instruction to fix has moved, then adjust for
                // its new address, and perform the fixup in tempspace.
                // Otherwise perform the fixup where the instruction is now.
                // (It only wouldn't move if it's asm code in readonly space)
                char* fixup_loc =
                    immobile_space_p((lispobj)inst_addr) ?
                    (char*)tempspace_addr(inst_addr - code + load_addr) : inst_addr;
                UNALIGNED_STORE32(fixup_loc,
                                  UNALIGNED_LOAD32(fixup_loc) + (code - load_addr));
            }
        }
#endif
        free(immobile_space_relocs);
        free(immobile_space_reloc_index);
    }

    // Fix Lisp pointers in static, immobile, and dynamic spaces
    fixup_space((lispobj*)STATIC_SPACE_OBJECTS_START,
                static_space_free_pointer - (lispobj*)STATIC_SPACE_OBJECTS_START);

    // Objects in immobile space are physically at 'tempspace',
    // but logically at their natural address. Perform fixups
    // at their current physical address.
#if DEFRAGMENT_FIXEDOBJ_SUBSPACE
    fixup_space((lispobj*)fixedobj_tempspace.start,
                fixedobj_tempspace.n_bytes >> WORD_SHIFT);
#else
    fixup_space((lispobj*)FIXEDOBJ_SPACE_START,
                FIXEDOBJ_SPACE_SIZE >> WORD_SHIFT);
#endif
    fixup_space((lispobj*)text_tempspace.start,
                text_tempspace.n_bytes >> WORD_SHIFT);

    // Dynamic space
    // We can safely ignore allocation region boundaries.
    fixup_space((lispobj*)DYNAMIC_SPACE_START,
                ((uword_t)dynamic_space_highwatermark() - DYNAMIC_SPACE_START) >> WORD_SHIFT);

    // Copy the spaces back where they belong.
#if DEFRAGMENT_FIXEDOBJ_SUBSPACE
    copy_back(FIXEDOBJ_SPACE_START, &fixedobj_tempspace, &fixedobj_free_pointer);
#endif
#ifdef LISP_FEATURE_IMMOBILE_CODE
    copy_back(TEXT_SPACE_START, &text_tempspace, &text_space_highwatermark);
    free(components);
#endif

    page_attributes_valid = 0;
#if 0
    // It's easy to mess things up, so assert correctness before saving a core.
    printf("verifying defrag\n");
    verify_heap(&verbose, VERIFY_POST_GC|VERIFY_AGGRESSIVE);
#endif
    free(fixedobj_tempspace.start);
    free(text_tempspace.start);
}
#endif

// Fixup immediate values that encode Lisp object addresses
// in immobile space. Process only the absolute fixups.
#ifdef LISP_FEATURE_X86_64
static void apply_absolute_fixups(lispobj fixups, struct code* code)
{
    struct varint_unpacker unpacker;
    varint_unpacker_init(&unpacker, fixups);
    skip_data_stream(&unpacker); // first data stream comprises the linkage indices
    char* instructions = code_text_start(code);
    int prev_loc = 0, loc;
    // The unpacker will produce successive values followed by a zero. There may
    // be a second data stream for the relative fixups which we ignore.
    while (varint_unpack(&unpacker, &loc) && loc != 0) {
        // For extra compactness, each loc is relative to the prior,
        // so that the magnitudes are smaller.
        loc += prev_loc;
        prev_loc = loc;
        void* fixup_where = instructions + loc;
        lispobj ptr = (lispobj)UNALIGNED_LOAD32(fixup_where);
        lispobj* header_addr;
        long fpval;

        if (is_lisp_pointer(ptr)) { // reference to a SYMBOL or LAYOUT
            lispobj fixed = follow_fp(ptr);
            if (fixed != ptr) UNALIGNED_STORE32(fixup_where, fixed);
            continue;
        }
        // Call to asm routine or linkage table entry using "CALL [#xNNNN]" form.
        // This fixup is only for whole-heap relocation on startup.
        if (asm_routines_start <= ptr && ptr < asm_routines_end) {
            continue;
        }
        if (find_fixedobj_page_index((void*)ptr) >= 0) {
            header_addr = search_immobile_space((void*)ptr);
            gc_assert(header_addr);
            if (!forwarding_pointer_p(header_addr))
                continue;
            fpval = forwarding_pointer_value(header_addr);
            int widetag = widetag_of(tempspace_addr(native_pointer(fpval)));
            // Must be an interior pointer to a symbol value slot
            if (widetag != SYMBOL_WIDETAG) lose("Expected symbol @ %p", header_addr);
        } else {
            lose("strange absolute fixup");
        }
        UNALIGNED_STORE32(fixup_where,
                          ptr - (lispobj)header_addr + (lispobj)native_pointer(fpval));
    }
}
#endif

void dump_immobile_fixedobjs(lispobj* where, lispobj* end, FILE*f)
{
    uword_t prev_page_base = 0;
    for ( ; where < end ; where += object_size(where) ) {
        if (ALIGN_DOWN((uword_t)where, IMMOBILE_CARD_BYTES) != prev_page_base) {
            low_page_index_t page = find_fixedobj_page_index(where);
            fprintf(f, "page @ %p: gens=%x free=%x%s\n",
                    (void*)ALIGN_DOWN((uword_t)where, IMMOBILE_CARD_BYTES),
                    fixedobj_pages[page].attr.parts.gens_,
                    fixedobj_pages[page].free_index,
                    (fixedobj_pages[page].attr.parts.flags & 0x80)?" WP":"");
            prev_page_base = ALIGN_DOWN((uword_t)where, IMMOBILE_CARD_BYTES);
        }
        fprintf(f, "%"OBJ_FMTX": %"OBJ_FMTX"\n", (uword_t)where, *where);
    }
}

void dump_immobile_text(lispobj* where, lispobj* end, FILE*f)
{
    int prevpage = -1;
    for ( ; where < end ; where += object_size(where) ) {
        int page = find_text_page_index(where);
        if (page!=prevpage) {
            fprintf(f, "page %d @ %p ss=%p%s\n",
                    page, text_page_address(page),
                    text_page_scan_start(page),
                    text_page_touched(page)?"":" WP");
            prevpage = page;
        }
        fprintf(f, "%p: %"OBJ_FMTX"\n", where, *where);
    }
}

#if 0
 void verify_text_scan_starts()
{
    lispobj* where = (lispobj*)TEXT_SPACE_START;
    lispobj* limit = text_space_highwatermark;
    int prevpage = -1;
    for ( ; where < limit ; where += object_size(where) ) {
        if (*where & block_header_free_bit) continue;
        int page = find_text_page_index(where);
        if (page > prevpage) { // this object had better be the scan start
            gc_assert(where == text_page_scan_start(page));
            prevpage = page;
        }
    }
    int page, max;
    max = find_text_page_index(text_space_highwatermark);
    for (page=0;page<=max;++page)
      if (text_page_scan_start(page)==0) {
        if (text_page_genmask[page]) {
          fprintf(stderr, "wrong genmask on page %d\n", page);
        }
        gc_assert(text_page_genmask[page]==0);
      }
}
#endif

void* expropriate_memory_from_tlsf(size_t amount)
{
  extern void* tlsf_pool_shrink(void*,void*,size_t);
  char* end = (char*)TEXT_SPACE_START + text_space_size;
  char* start = tlsf_pool_shrink(tlsf_control, end, amount);
#ifdef TLSF_CONFIG_DEBUG
  tlsf_check(tlsf_control);
  tlsf_check_pool(tlsf_mem_start);
  //fprintf(stderr, "TLSF integrity checks passed\n");
#endif
  return start;
}
