/*
 * This is the SPARC BSD incarnation of arch-dependent OS-dependent
 * routines. See also "linux-os.c".
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

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include "validate.h"

#ifdef LISP_FEATURE_SB_THREAD
#error "Define threading support functions"
#else
int arch_os_thread_init(struct thread *thread) {
   return 1;                   /* success */
}
int arch_os_thread_cleanup(struct thread *thread) {
   return 1;                   /* success */
}
#endif

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
   if (offset == 0) {
       static int zero;
       zero = 0;
       return &zero;
   } else if (offset < 16) {
       return &context->uc_mcontext.__gregs[offset+3];
   } else if (offset < 32) {
       /* FIXME: You know, this (int *) stuff looks decidedly
          dubious */
       int *sp = (int*) _UC_MACHINE_SP(context);
       return &(sp[offset-16]);
   } else {
       return 0;
   }
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
   return &(context->uc_mcontext.__gregs[_REG_PC]);
}

os_context_register_t *
os_context_npc_addr(os_context_t *context)
{
   return &(context->uc_mcontext.__gregs[_REG_nPC]);
}

#ifdef SOLARIS
sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
   return &(context->uc_sigmask);
}
#endif

unsigned long
os_context_fp_control(os_context_t *context)
{
   return (context->uc_mcontext.__fpregs.__fpu_fsr);
}

void os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
   /* see sparc-assem.S */
   sparc_flush_icache(address, length);
}
