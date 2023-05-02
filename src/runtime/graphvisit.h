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

#ifndef _GRAPHVISIT_H_
#define _GRAPHVISIT_H_

#include "hopscotch.h"
struct grvisit_context {
  struct hopscotch_table* seen;
  void (*action)(lispobj, void*);
  void* data;
  int depth;
  int maxdepth;
};

extern struct grvisit_context*
visit_heap_from_static_roots(struct hopscotch_table* reached,
                             void (*action)(lispobj, void*),
                             void* data);
#endif
