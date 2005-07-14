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
#include <asm/fpu.h>

#include "validate.h"
size_t os_vm_page_size;

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
    /* FIXME: 0x7E0000 is defined as something useful in constants.h,
       but without the UL, which would probably lead to 32/64-bit
       errors if we simply used it here.  Ugh.  CSR, 2003-09-15 */
    arch_set_fp_control(os_context_fp_control(context) & ~(0x7e0000UL) &
                        /* KLUDGE: for some reason that I don't
                        understand, by the time we get here the
                        "enable denormalized traps" bit in the fp
                        control word is set.  Since we really don't
                        want to tra every time someone types
                        LEAST-POSITIVE-SINGLE-FLOAT into the repl,
                        mask that bit out.  -- CSR, 2003-09-15 */
                        ~(0x1UL<<6));
}

void os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    asm volatile ("imb" : : : "memory" );
}
