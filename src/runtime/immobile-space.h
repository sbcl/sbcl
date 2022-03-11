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

#ifndef _IMMOBILE_SPACE_H_
#define _IMMOBILE_SPACE_H_

#ifdef LISP_FEATURE_IMMOBILE_SPACE
#include <limits.h>
#include "core.h"

// 1 page is reserved for some constant arrays.
// Right now it is just the array that maps widetag to layout
#define FIXEDOBJ_RESERVED_PAGES 1

extern void prepare_immobile_space_for_final_gc(void);
extern void prepare_immobile_space_for_save(boolean verbose);
extern boolean immobile_space_preserve_pointer(void*);
extern void update_immobile_nursery_bits(void);
extern void scavenge_immobile_roots(generation_index_t,generation_index_t);
extern void scavenge_immobile_newspace(void);
extern void sweep_immobile_space(int raise);
extern void write_protect_immobile_space(void);
extern unsigned int immobile_scav_queue_count;
typedef int low_page_index_t;

extern unsigned int* varyobj_page_touched_bits;
extern uword_t asm_routines_start, asm_routines_end;

static inline void *
fixedobj_page_address(low_page_index_t page_num)
{
    return (void*)(FIXEDOBJ_SPACE_START + (page_num * IMMOBILE_CARD_BYTES));
}
static inline void *
varyobj_page_address(low_page_index_t page_num)
{
    return (void*)(VARYOBJ_SPACE_START + (page_num * IMMOBILE_CARD_BYTES));
}

struct varyobj_page {
    // Generation mask for objects which start on this page.
    // An object which ends on but does not start on this page
    // does not set the respective bit.
    unsigned int generations: 8,
    // Offset backwards in double-lispwords from the page end to the
    // lowest-addressed object touching the page. This offset can point to
    // a hole, but we prefer that it not. If the offset is zero, the page
    // has no object other than possibly a hole resulting from a freed object.
    // The entire space size defaults to just over 100MiB,
    // so 24 bits is more than adequate to point back to any word.
                 scan_start_offset: 24;
};

extern struct varyobj_page *varyobj_pages;
/* Calculate the address where the first object touching this page starts. */
static inline lispobj*
varyobj_scan_start(low_page_index_t page_index)
{
    return (lispobj*)((char*)varyobj_page_address(page_index+1)
                      - varyobj_pages[page_index].scan_start_offset * (2 * N_WORD_BYTES));
}

static inline low_page_index_t find_fixedobj_page_index(void *addr)
{
  if (addr >= (void*)FIXEDOBJ_SPACE_START) {
      // Must use full register size here to avoid truncation of quotient
      // and bogus result!
      page_index_t index =
          ((uintptr_t)addr - (uintptr_t)FIXEDOBJ_SPACE_START) / IMMOBILE_CARD_BYTES;
      if (index < (int)(FIXEDOBJ_SPACE_SIZE/IMMOBILE_CARD_BYTES))
          return index;
  }
  return -1;
}
static inline low_page_index_t find_varyobj_page_index(void *addr)
{
  if (addr >= (void*)VARYOBJ_SPACE_START) {
      // Must use full register size here to avoid truncation of quotient
      // and bogus result!
      size_t offset = (uintptr_t)addr - (uintptr_t)VARYOBJ_SPACE_START;
      if (offset >= varyobj_space_size)
          return -1;
      return offset / IMMOBILE_CARD_BYTES;
  }
  return -1;
}

static inline boolean immobile_space_p(lispobj obj)
{
/* To test the two immobile ranges, we first check that a pointer is within
 * the outer bounds, and then that is not in the excluded middle (if any).
 * This requires only 1 comparison to weed out dynamic-space pointers,
 * vs doing the more obvious 2 tests, provided that dynamic space starts
 * above 4GB. range_1_max == range_2_min if there is no discontinuity. */
    uword_t offset = obj - immobile_space_lower_bound;
    if (offset >= immobile_space_max_offset) return 0;
    return !(immobile_range_1_max_offset <= offset
             && offset < immobile_range_2_min_offset);
}

extern boolean immobile_card_protected_p(void*);

#else

static inline boolean immobile_space_p(lispobj __attribute__((unused)) obj) { return 0; }
#define immobile_obj_gen_bits(dummy) 0
#define prepare_immobile_space_for_final_gc()
#define prepare_immobile_space_for_save(dummy)
#define immobile_space_preserve_pointer(dummy) 0
#define scavenge_immobile_roots(dummy1,dummy2)
#define scavenge_immobile_newspace(dummy)
#define sweep_immobile_space(dummy)
#define update_immobile_nursery_bits()
#define write_protect_immobile_space()
#define immobile_scav_queue_count 0
#define immobile_card_protected_p(dummy) (0)

#endif

#endif // _IMMOBILE_SPACE_H_
