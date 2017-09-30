#ifndef _PPC_DARWIN_OS_H
#define _PPC_DARWIN_OS_H

#include "darwin-os.h"

typedef unsigned int os_context_register_t;
#include "arch-os-generic.inc"

/* On OS X 10.5, the field names for the thread state have changed and
 * now are prepended with __. Use some #define hackery to deal with
 * this.
 */
#if __DARWIN_UNIX03

#define PPC_DARWIN_REGIFY(foo) __ ## foo

typedef ppc_thread_state_t ppc_ss_struct_t;

#else

#define PPC_DARWIN_REGIFY(foo) foo

typedef ppc_saved_state_t ppc_ss_struct_t;

#endif /* __DARWIN_UNIX03 */

#endif /* _PPC_DARWIN_OS_H */
