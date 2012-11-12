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
#include <stdint.h>

#define PAGE_BYTES BACKEND_PAGE_BYTES

typedef intptr_t page_index_t;
#ifdef LISP_FEATURE_WIN32
#define PAGE_INDEX_FMT "Id"
#else
#define PAGE_INDEX_FMT "ld"
#endif

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

extern os_vm_size_t bytes_consed_between_gcs;

#endif /* _GC_H_ */
