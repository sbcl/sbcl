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

void prepare_immobile_space_for_save(lispobj init_function, boolean verbose);

typedef int low_page_index_t;

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
#endif

#endif // _IMMOBILE_SPACE_H_
