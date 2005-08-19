/*
 * This is the MIPS Linux incarnation of arch-dependent OS-dependent
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
#include <sys/socket.h>
#include <sys/utsname.h>

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

/* for cacheflush() */
#include <sys/cachectl.h>

/* for BD_CAUSE */
#include <asm/mipsregs.h>

#include "validate.h"

size_t os_vm_page_size;

int
arch_os_thread_init(struct thread *thread)
{
#ifdef LISP_FEATURE_SB_THREAD
#warning "Check threading support functions"
#endif
    return 1;                  /* success */
}

int
arch_os_thread_cleanup(struct thread *thread)
{
#ifdef LISP_FEATURE_SB_THREAD
#warning "Check threading support functions"
#endif
    return 1;                  /* success */
}

os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    return &(((struct sigcontext *)&(context->uc_mcontext))->sc_regs[offset]);
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    /* Why do I get all the silly ports? -- CSR, 2002-08-11 */
    return &(((struct sigcontext *)&(context->uc_mcontext))->sc_pc);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &(context->uc_sigmask);
}

unsigned int
os_context_fp_control(os_context_t *context)
{
    /* FIXME: Probably do something. */
    return 0;
}

void
os_restore_fp_control(os_context_t *context)
{
    /* FIXME: Probably do something. */
}

unsigned int
os_context_bd_cause(os_context_t *context)
{
    /* We need to see if whatever happened, happened because of a
       branch delay event */
    /* FIXME: However, I'm not convinced that the values that Linux
       puts in this slot are actually right; specifically, attempting
       to compile sbcl with sbcl-0.7.7.7 lead to an "infinite SIGTRAP
       loop" where a (BREAK 16) not in a branch delay slot would have
       CAUSEF_BD filled. So, we comment

        return (((struct sigcontext *) &(context->uc_mcontext))->sc_cause
                & CAUSEF_BD);

       out and return 0 always.  -- CSR, 2002-09-02 */
    /* Unfortunately, returning 0 fails for taken branches because
       os_context_bd_cause is also used to find out if a branch
       emulation is needed.  We work around that by checking if the
       current instruction is a jump or a branch.  */
    unsigned int inst = *((unsigned int *)(unsigned int)(*os_context_pc_addr(context)));

    switch (inst >> 26) {
    case 0x0: /* immediate jumps */
        switch (inst & 0x3f) {
        case 0x08:
        case 0x09:
            return CAUSEF_BD;
        }
        break;
    /* branches and register jumps */
    case 0x1:
    case 0x2:
    case 0x3:
    case 0x4:
    case 0x5:
    case 0x6:
    case 0x7:
        return CAUSEF_BD;
    }
    return 0;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    if (cacheflush(address, length, ICACHE) == -1)
        perror("cacheflush");
}
