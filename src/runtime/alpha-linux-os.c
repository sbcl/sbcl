/*
 * This is the x86 Linux incarnation of arch-dependent OS-dependent
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

/* These header files were lifted wholesale from linux-os.c, some may
 * be redundant. -- Dan Barlow ca. 2001-05-01 */
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

#if defined GENCGC		/* unlikely ... */
#include "gencgc.h"
#endif

sigcontext_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    return &context->uc_mcontext.sc_regs[offset];
}

sigcontext_register_t *
os_context_fpregister_addr(os_context_t *context, int offset)
{
    return &context->uc_mcontext.sc_fpregs[offset];
}

sigcontext_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &((context->uc_mcontext).sc_pc);
}
sigcontext_register_t *
os_context_sp_addr(os_context_t *context)
{
    lose("This was supposed to be an x86-only operation");
    return 0;
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

void os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    /* XXX this really shouldn't be empty

<dhd> dan_b: asm volatile ("call_pal imb")
<dhd> or just "imb"
<dhd> also : : "memory" 

     */
}
