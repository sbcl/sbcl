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

#ifdef LISP_FEATURE_GENCGC
#define PAGE_BYTES GENCGC_PAGE_BYTES
#else
#define PAGE_BYTES BACKEND_PAGE_BYTES
#endif

typedef signed long page_index_t;
typedef signed char generation_index_t;

extern void gc_init(void);
extern void gc_initialize_pointers(void);
extern void collect_garbage(generation_index_t last_gen);
extern void gc_init_tables(void);


#include "os.h"

extern void set_auto_gc_trigger(os_vm_size_t usage);
extern void clear_auto_gc_trigger(void);

#include "fixnump.h"

extern boolean maybe_gc(os_context_t *context);

extern unsigned long bytes_consed_between_gcs;

#endif /* _GC_H_ */
