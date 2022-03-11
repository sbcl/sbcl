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

/// Opposite head/tail convention for immobile space queue.
/// (Enqueue at the tail, dequeue at the head).  Sorry. :-(
struct Qblock {
  int count;
  int tail;
  struct Qblock* next;
  lispobj elements[1];
};

#if 1
#define QBLOCK_BYTES GENCGC_PAGE_BYTES
// 1+ because struct QBlock has space for a single element within it
#define QBLOCK_CAPACITY (1+(QBLOCK_BYTES-sizeof(struct Qblock))/sizeof(lispobj))
#else
#define QBLOCK_CAPACITY 12 /* artificially low, for testing */
#endif
