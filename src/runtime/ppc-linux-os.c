/*
 * This is the IBM/Motorola/Apple/whoever Linux incarnation of
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

#include "validate.h"
size_t os_vm_page_size;

struct thread *arch_os_get_current_thread() {
    return all_threads;
}
int arch_os_thread_init(struct thread *thread) {
    return 1;			/* success */
}
int arch_os_thread_cleanup(struct thread *thread) {
    return 1;			/* success */
}

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    return &((context->uc_mcontext.regs)->gpr[offset]);
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &((context->uc_mcontext.regs)->nip);
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return &((context->uc_mcontext.regs)->link);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    /* So this may look like nice, well behaved code. However, closer
       inspection reveals that gpr is simply the general purpose
       registers, and PT_FPSCR is an offset that is larger than 32
       (the number of ppc registers), but that happens to get the
       right answer. -- CSR, 2002-07-11 */
    return context->uc_mcontext.regs->gpr[PT_FPSCR]; 
}

void 
os_restore_fp_control(os_context_t *context)
{
    unsigned long control;
    double d;
    
    control = os_context_fp_control(context) & 
	/* FIXME: Should we preserve the user's requested rounding mode?

	Note that doing 
	
	~(FLOAT_STICKY_BITS_MASK | FLOAT_EXCEPTIONS_BYTE_MASK)
	
	here leads to infinite SIGFPE for invalid operations, as
	there are bits in the control register that need to be
	cleared that are let through by that mask. -- CSR, 2002-07-16 */

	FLOAT_TRAPS_BYTE_MASK;
    
    d = *((double *) &control);
    asm volatile ("mtfsf 0xff,%0" : : "f" (d));
}

void 
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    /* see ppc-arch.c */
    ppc_flush_icache(address,length);
}

