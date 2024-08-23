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

#include <stdbool.h>
#ifdef LISP_FEATURE_IMMOBILE_SPACE
#include <limits.h> // why?
#include "globals.h" // for FIXEDOBJ_SPACE_START and TEXT_SPACE_START
#include "gc-assert.h"

// 1 page is reserved for some constant arrays.
// Right now it is just the array that maps widetag to layout
#define FIXEDOBJ_RESERVED_PAGES 1

extern void prepare_immobile_space_for_final_gc(void);
extern void prepare_immobile_space_for_save(bool verbose);
extern bool immobile_space_preserve_pointer(void*);
extern void scavenge_immobile_roots(generation_index_t,generation_index_t);
extern void scavenge_immobile_newspace(void);
extern void sweep_immobile_space(int raise);
extern void write_protect_immobile_space(void);
extern unsigned int immobile_scav_queue_count;
typedef int low_page_index_t;

extern unsigned int* text_page_touched_bits;
extern uword_t asm_routines_start, asm_routines_end;

static inline void *
fixedobj_page_address(low_page_index_t page_num)
{
    return (void*)(FIXEDOBJ_SPACE_START + (page_num * IMMOBILE_CARD_BYTES));
}
static inline void *
text_page_address(low_page_index_t page_num)
{
    return (void*)(TEXT_SPACE_START + (page_num * IMMOBILE_CARD_BYTES));
}

extern unsigned char* text_page_genmask;
extern unsigned short int* tlsf_page_sso;

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
static inline low_page_index_t find_text_page_index(void *addr)
{
  if (addr >= (void*)TEXT_SPACE_START) {
      // Must use full register size here to avoid truncation of quotient
      // and bogus result!
      size_t offset = (uintptr_t)addr - (uintptr_t)TEXT_SPACE_START;
      if (offset >= text_space_size)
          return -1;
      return offset / IMMOBILE_CARD_BYTES;
  }
  return -1;
}

static inline bool immobile_space_p(lispobj obj)
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

extern bool immobile_card_protected_p(void*);

extern void enliven_immobile_obj(lispobj*,int);

#define IMMOBILE_OBJ_VISITED_FLAG    0x10

// Immobile object header word:
//                 generation byte --|    |-- widetag
//                                   v    v
//                       0xzzzzzzzz GGzzzzww
//         arbitrary data  --------   ---- length in words

// There is a hard constraint on NUM_GENERATIONS, which is currently 8.
// (0..5=normal, 6=pseudostatic, 7=scratch)
// Shifting a 1 bit left by the contents of the generation byte
// must not overflow a register.

// Mask off the VISITED flag to get the generation number
#define immobile_obj_generation(x) (immobile_obj_gen_bits(x) & 0xf)

#ifdef LISP_FEATURE_LITTLE_ENDIAN
// Return the generation bits which means the generation number
// in the 4 low bits (there's 1 excess bit) and the VISITED flag.
static inline int immobile_obj_gen_bits(lispobj* obj) // native pointer
{
    // When debugging, assert that we're called only on a headered object
    // whose header contains a generation byte.
    gc_dcheck(!embedded_obj_p(widetag_of(obj)));
    return ((generation_index_t*)obj)[3] & 0x1F;
}
// Turn a grey node black.
static inline void set_visited(lispobj* obj)
{
    extern generation_index_t new_space;
    gc_dcheck(widetag_of(obj) != SIMPLE_FUN_WIDETAG);
    gc_dcheck(immobile_obj_gen_bits(obj) == new_space);
    ((generation_index_t*)obj)[3] |= IMMOBILE_OBJ_VISITED_FLAG;
}
static inline void assign_generation(lispobj* obj, generation_index_t gen)
{
    gc_dcheck(widetag_of(obj) != SIMPLE_FUN_WIDETAG);
    generation_index_t* ptr = (generation_index_t*)obj + 3;
    // Clear the VISITED flag, assign a new generation, preserving the three
    // high bits which include the OBJ_WRITTEN flag as well as two
    // opaque flag bits for use by Lisp.
    *ptr = (*ptr & 0xE0) | gen;
}
#else
#error "Need to define immobile_obj_gen_bits() for big-endian"
#endif /* little-endian */

#else

static inline bool immobile_space_p(lispobj __attribute__((unused)) obj) { return 0; }
#define immobile_obj_gen_bits(dummy) 0
#define prepare_immobile_space_for_final_gc()
#define prepare_immobile_space_for_save(dummy)
static inline int immobile_space_preserve_pointer(__attribute__((unused)) void* p) { return  0; }
#define scavenge_immobile_roots(dummy1,dummy2)
#define scavenge_immobile_newspace(dummy)
#define sweep_immobile_space(dummy)
#define write_protect_immobile_space()
#define immobile_scav_queue_count 0
#define immobile_card_protected_p(dummy) (0)

#endif

#endif // _IMMOBILE_SPACE_H_
