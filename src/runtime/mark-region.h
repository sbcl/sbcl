#ifndef MARK_REGION_H
#define MARK_REGION_H
#ifdef LISP_FEATURE_MARK_REGION_GC
#include "lispobj.h"
#include "core.h"

/* Set line size so that every line byte corresponds to one mark
 * bitmap byte. */
#define LINE_SIZE (8 << N_LOWTAG_BITS)
#define LINES_PER_PAGE (GENCGC_PAGE_BYTES / LINE_SIZE)

extern uword_t *allocation_bitmap;
extern _Atomic(uword_t) *mark_bitmap;
extern unsigned char *line_bytemap;
typedef intptr_t line_index_t;

/* Line arithmetic */
static inline char *line_address(line_index_t line) {
  return (char*)(DYNAMIC_SPACE_START + (line * LINE_SIZE));
}

static inline line_index_t address_line(void *address) {
  return ((uintptr_t)address - DYNAMIC_SPACE_START) / LINE_SIZE;
}

static inline line_index_t page_to_line(page_index_t p) { return p*LINES_PER_PAGE; }

#define for_lines_in_page(l, p) \
  for (line_index_t l = page_to_line(p), limit = l+LINES_PER_PAGE; l < limit; l++)

/* Line metadata */
/* Two highest bits are unused, then a bit for "fresh" lines which need
 * the allocation bitmap to be materialised, then a bit for line
 * marking, then four lowest bits hold (generation + 1). A 0 means a
 * free line. */
#define MARK_GEN(l) ((l) | 16)
#define FRESHEN_GEN(l) ((l) | 32)
#define UNMARK_GEN(l) ((l) & 15)
#define UNFRESHEN_GEN(l) ((l) & 31)
#define ENCODE_GEN(g) ((g) + 1)
#define DECODE_GEN(l) (UNMARK_GEN(l) - 1)
#define IS_MARKED(l) ((l) & 16)
#define IS_FRESH(l) ((l) & 32)
#define COPY_MARK(from, to) (((from) & 0x30) | (to))

/* All the GC functions. */

/* I/O */
extern void mrgc_init();
extern void load_corefile_bitmaps(int fd, core_entry_elt_t n_ptes);
static inline uword_t bitmap_size(uword_t n_ptes) {
    const int bitmap_bytes_per_page = GENCGC_PAGE_BYTES / (CONS_SIZE*N_WORD_BYTES) / 8;
    return n_ptes * bitmap_bytes_per_page;
}

/* Allocation */
struct allocator_state {
  /* This is an int and not page_index_t to make struct allocator_state
   * be 8 bytes large, avoiding needing libatomic for 16-byte atomic loads. */
  int page;
  /* We try not to allocate small objects from free pages in order to
   * reduce fragmentation, and to keep more free pages for large
   * objects. */
  bool allow_free_pages;
};

extern void pre_search_for_small_space(sword_t nbytes, int page_type,
                                       struct allocator_state *state, page_index_t end);
extern bool try_allocate_small_from_pages(sword_t nbytes, struct alloc_region *region,
                                          int page_type, generation_index_t gen,
                                          struct allocator_state *start, page_index_t end);
extern bool try_allocate_small_after_region(sword_t nbytes,
                                            struct alloc_region *region);
extern page_index_t try_allocate_large(uword_t nbytes,
                                       int page_type, generation_index_t gen,
                                       struct allocator_state *start, page_index_t end,
                                       uword_t *largest_hole);
extern void CPU_SPLIT_DECL mr_update_closed_region(struct alloc_region *region, generation_index_t gen);

/* Bitmaps */
extern bool allocation_bit_marked(void *pointer);
extern void set_allocation_bit_mark(void *pointer);
extern bool line_marked(void *pointer);
extern lispobj *search_dynamic_space(void *pointer);

/* Liveness */
extern void mr_preserve_ambiguous(uword_t address);
extern void mr_preserve_range(lispobj *from, sword_t nwords);
extern void mr_preserve_leaf(lispobj obj);
extern void mr_preserve_object(lispobj obj);
extern void mr_trace_bump_range(lispobj* start, lispobj *end);
//extern bool pointer_survived_gc_yet(lispobj object);

/* Running the GC */
extern void mr_pre_gc(generation_index_t generation);
extern void mr_collect_garbage(bool raise);
extern void zero_all_free_ranges();
extern void prepare_lines_for_final_gc();
#endif
#endif
