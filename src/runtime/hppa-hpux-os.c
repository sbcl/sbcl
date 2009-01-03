/*
 * This is the HPPA HPUX incarnation of arch-dependent OS-dependent
 * routines. See also "hppa-os.c".
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

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include "sbcl.h"
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include <sys/socket.h>
#include <sys/utsname.h>

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include "validate.h"
size_t os_vm_page_size;

#ifdef LISP_FEATURE_SB_THREAD
#error "Define threading support functions"
#else
int arch_os_thread_init(struct thread *thread) {
    return 1;                  /* success */
}
int arch_os_thread_cleanup(struct thread *thread) {
    return 1;                  /* success */
}
#endif

/* for hpux read /usr/include/machine/save_state.h
 * os_context_register_addr() may not be used
 * to modify registers without setting a state-flag too */
os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
  return (os_context_register_t *)
         ((unsigned int)(&((ucontext_t *) context)->uc_mcontext.ss_wide.ss_64)) + (offset * 2) + 1;
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    /* Why do I get all the silly ports? -- CSR, 2002-08-11 */
    return ((unsigned int) &((ucontext_t *) context)->uc_mcontext.ss_wide.ss_64.ss_pcoq_head + 4);
}

os_context_register_t *
os_context_npc_addr(os_context_t *context)
{
    return ((unsigned int) &((ucontext_t *) context)->uc_mcontext.ss_wide.ss_64.ss_pcoq_tail + 4);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &(((ucontext_t *)context)->uc_subcontext.__uc_sigmask);
}

void
os_restore_fp_control(os_context_t *context)
{
    /* FIXME: Probably do something. */
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    /* FIXME: Maybe this is OK. */
    sanctify_for_execution(address,length);
}
