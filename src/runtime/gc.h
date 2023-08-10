/*
 * garbage collection
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

#ifndef _GC_H_
#define _GC_H_

#include "sbcl.h"
#include "gc-assert.h"
#include "gc-typedefs.h"
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

#if defined LISP_FEATURE_SB_THREAD && !defined LISP_FEATURE_SB_SAFEPOINT
# define THREADS_USING_GCSIGNAL 1
#endif

#if defined LISP_FEATURE_GENERATIONAL && !defined LISP_FEATURE_C_STACK_IS_CONTROL_STACK
# define GENCGC_IS_PRECISE 1
#else
# define GENCGC_IS_PRECISE 0
#endif

extern void gc_init(void);
extern void collect_garbage(generation_index_t last_gen);

extern void set_auto_gc_trigger(os_vm_size_t usage);
extern void clear_auto_gc_trigger(void);

extern bool maybe_gc(os_context_t *context);
void gc_heap_exhausted_error_or_lose (sword_t available, sword_t requested) never_returns;

extern bool gc_active_p;
extern int sb_sprof_enabled;

extern os_vm_size_t bytes_consed_between_gcs;

#define VERIFY_VERBOSE    1
#define VERIFY_PRE_GC     2
#define VERIFY_POST_GC    4
/* AGGRESSIVE = always call valid_lisp_pointer_p() on pointers. */
#define VERIFY_AGGRESSIVE 8
#define VERIFY_TAGS       16
/* QUICK = skip most tests. This is intended for use when GC is believed
 * to be correct per se (i.e. not for debugging GC), and so the verify
 * pass executes more quickly */
#define VERIFY_QUICK      32
/* FINAL = warn about pointers from heap space to non-heap space.
 * Such pointers would normally be ignored and do not get flagged as failure.
 * This can be used in conjunction with QUICK, AGGRESSIVE, or neither. */
#define VERIFY_FINAL      64
#define VERIFY_DONT_LOSE  128

/* VERIFYING_foo indicates internal state, not a caller's option */
/* GENERATIONAL implies formatted objects, but there are ranges of objects
 * that are not generational (static space)
 * so there are no page protection checks performed for pointers from objects
 * in such ranges */
#define VERIFYING_GENERATIONAL 256
/* UNFORMATTED implies that this is not a range of objects
 * but rather a range of pointers such as a binding stack, TLS,
 * lisp signal handler array, or other similar array */
#define VERIFYING_UNFORMATTED 512

#define MAX_ERR_OBJS 5
struct verify_state {
    lispobj* object_addr;
    lispobj object_header;
    uword_t flags;
    generation_index_t object_gen;
    generation_index_t min_pointee_gen;
    _Atomic(int) nerrors;
    lispobj err_objs[5];
};
void hexdump_spaces(struct verify_state*, char *reason);
int verify_heap(lispobj*, int flags);
int hexdump_and_verify_heap(lispobj*, int flags);

page_index_t gc_find_freeish_pages(page_index_t *restart_page_ptr, sword_t nbytes,
                                   int page_type, generation_index_t gen);

extern void tlsf_dump_pool(void*, void*, char *pathname);

extern generation_index_t gencgc_oldest_gen_to_gc;
extern page_index_t gencgc_alloc_start_page;
extern bool conservative_stack;
extern lispobj lisp_init_function;

#include "immobile-space.h" // provides dummy stubs if #-immobile-space
#ifdef LISP_FEATURE_MARK_REGION_GC
#include "pmrgc-impl.h"
#include "mark-region.h"
#elif defined LISP_FEATURE_GENCGC
#include "gencgc-impl.h"
#else
#error "GC selector not defined"
#endif

#endif /* _GC_H_ */
