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

/* Note: CMU CL had two different argument conventions for
 * collect_garbage(..), depending on whether gencgc was in use. SBCL
 * should have only one, which is automatic right now (20000814) since
 * we only support gencgc, but should also be maintained if someone
 * adds another GC, or ports one of the other CMU CL GCs like gengc. */
extern void collect_garbage(unsigned last_gen);

#ifndef ibmrt

#include "os.h"

extern void set_auto_gc_trigger(os_vm_size_t usage);
extern void clear_auto_gc_trigger(void);

#endif ibmrt

#endif _GC_H_
