/*
 * This is the Compaq/Digital Alpha Linux incarnation of
 * arch-dependent OS-dependent routines. See also "linux-os.c".  */

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
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <asm/fpu.h>

#include "validate.h"
size_t os_vm_page_size;


os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    return &context->uc_mcontext.sc_regs[offset];
}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return &context->uc_mcontext.sc_fpregs[offset];
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &((context->uc_mcontext).sc_pc);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    return ieee_fpcr_to_swcr((context->uc_mcontext).sc_fpcr);
}

void
os_restore_fp_control(os_context_t *context)
{
    /* FIXME (in two parts):
       Firstly, what happens in alpha linux inside the signal handler?
       Does the floating point control state get cleared as in other
       Linuxes?
    
       Secondly, how do we put it back if so? It will probably involve
       something to do with
    
       context->uc_mcontext.sc_fpcr

       (maybe a simple assembly statement will be enough)
    */ 
}

void os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    asm volatile ("imb" : : : "memory" );
}
