/*
 * This is the SPARC Linux incarnation of arch-dependent OS-dependent
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
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "sbcl.h"
#include <sys/socket.h>
#include <sys/utsname.h>

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include "validate.h"
size_t os_vm_page_size;

#if defined GENCGC              /* unlikely ... */
#include "gencgc.h"
#endif

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
  /* printf("Offset: %d,", offset);
     printf("Context: %p\n", context);
     printf("PC: %x,", context->si_regs.pc);
     printf("NPC: %x\n", context->si_regs.npc); */
  if (offset == 0) {
    static int zero;
    zero = 0;
    /* printf("Returning: %p pointing to %p\n", &zero, zero); */
    return &zero;
  } else if (offset < 16) {
    /* printf("Returning: %p pointing to %p\n", &context->si_regs.u_regs[offset], context->si_regs.u_regs[offset]); */
    return &context->si_regs.u_regs[offset];
  } else if (offset < 32) {
    int *sp = (int*) context->si_regs.u_regs[14]; /* Stack Pointer ?? */
    /* printf("SP: %p\n", sp);
       printf("Returning: %p pointing to %p\n", &(sp[offset-16]), sp[offset-16]); */
    return &(sp[offset-16]);
  } else
    return 0;
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
  return &(context->si_regs.pc);
}

os_context_register_t *
os_context_npc_addr(os_context_t *context)
{
  return &(context->si_regs.npc);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
  return &(context->si_mask);
}

void os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
  /* FIXME.  There's a bit of stuff in the CMUCL version. It may or
     may not be needed */
}
