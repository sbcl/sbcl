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
#include "os.h"
#include <stdint.h>

typedef intptr_t page_index_t;

// This decl should probably be be in gencgc-internal,
// except it can't be: collect_garbage() receives a generation number.
typedef signed char generation_index_t;

extern void gc_init(void);
extern void collect_garbage(generation_index_t last_gen);

extern void set_auto_gc_trigger(os_vm_size_t usage);
extern void clear_auto_gc_trigger(void);

extern boolean maybe_gc(os_context_t *context);

extern boolean gc_active_p;
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
 * that are not generational - static, readonly, and metaspace -
 * so there are no page protection checks performed for pointers from objects
 * in such ranges */
#define VERIFYING_GENERATIONAL 256
/* UNFORMATTED implies that this is not a range of objects
 * but rather a range of pointers such as a binding stack, TLS,
 * lisp signal handler array, or other similar array */
#define VERIFYING_UNFORMATTED 512

int verify_heap(int flags);
#ifdef LISP_FEATURE_GENCGC
#define MAX_ERR_OBJS 5
struct verify_state {
    lispobj* object_addr;
    lispobj object_header;
    uword_t flags;
    generation_index_t object_gen;
    generation_index_t min_pointee_gen;
    int nerrors;
    lispobj err_objs[5];
};
void dump_spaces(struct verify_state*, char *reason);
#endif

#endif /* _GC_H_ */
