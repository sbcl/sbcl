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

extern void gc_init(void);
extern void gc_initialize_pointers(void);
extern void collect_garbage(unsigned last_gen);
extern void gc_init_tables(void);


#include "os.h"

extern void set_auto_gc_trigger(os_vm_size_t usage);
extern void clear_auto_gc_trigger(void);

extern int maybe_gc_pending;

static inline int fixnump(lispobj obj) {
    return((obj & 
	    (LOWTAG_MASK & 
	     (~(EVEN_FIXNUM_LOWTAG|ODD_FIXNUM_LOWTAG)))) 
	   == 0);
}

#endif /* _GC_H_ */
