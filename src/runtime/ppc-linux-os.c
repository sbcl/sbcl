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
#include <sys/prctl.h>

#include "validate.h"
#include "ppc-linux-mcontext.h"

int arch_os_thread_init(struct thread *thread) {
    /* For some reason, PPC Linux appears to default to not generating
     * floating point exceptions.  PR_SET_FPEXC is a PPC-specific
     * option new in kernel 2.4.21 and 2.5.32 that allows us to
     * configure this.  Should we need to run on an older kenel, the
     * equivalent trick is to get into a signal-handling context and
     * modify the saved machine state register.
     *
     * PR_FP_EXC_PRECISE may be more accurate than we need,
     * particularly if we move to the x86oid trick of inserting
     * explicit synchronization for floating-point exception
     * delivery.  If we wish to move to such a model, the other two
     * exception delivery modes that we could use are PR_FP_EXC_ASYNC
     * and PR_FP_EXC_NONRECOV, and exception delivery can be forced
     * by any access to the FPSCR.  -- AB, 2010-May-23 */
    prctl(PR_SET_FPEXC, PR_FP_EXC_PRECISE, 0, 0);

    return 1;                   /* success */
}
int arch_os_thread_cleanup(struct thread *thread) {
    return 1;                   /* success */
}

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
#if defined(GLIBC231_STYLE_UCONTEXT)
    return &((context->uc_mcontext.regs)->gpr[offset]);
#elif defined(GLIBC232_STYLE_UCONTEXT)
    return &((context->uc_mcontext.uc_regs->gregs)[offset]);
#endif
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
#if defined(GLIBC231_STYLE_UCONTEXT)
    return &((context->uc_mcontext.regs)->nip);
#elif defined(GLIBC232_STYLE_UCONTEXT)
    return &((context->uc_mcontext.uc_regs->gregs)[PT_NIP]);
#endif
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
#if defined(GLIBC231_STYLE_UCONTEXT)
    return &((context->uc_mcontext.regs)->link);
#elif defined(GLIBC232_STYLE_UCONTEXT)
    return &((context->uc_mcontext.uc_regs->gregs)[PT_LNK]);
#endif
}

os_context_register_t *
os_context_ctr_addr(os_context_t *context)
{
    /* Like os_context_fp_control() and os_context_lr_addr(), this
     * uses an index beyond the declared end of the array in order to
     * find the correct register value in the context. */
#if defined(GLIBC231_STYLE_UCONTEXT)
    /* FIXME: This probably should be ->ctr instead of ->gpr[PT_CTR]. */
    return &((context->uc_mcontext.regs)->gpr[PT_CTR]);
#elif defined(GLIBC232_STYLE_UCONTEXT)
    return &((context->uc_mcontext.uc_regs)->gregs[PT_CTR]);
#endif
}

os_context_register_t *
os_context_cr_addr(os_context_t *context)
{
    /* Like os_context_fp_control() and os_context_lr_addr(), this
     * uses an index beyond the declared end of the array in order to
     * find the correct register value in the context. */
#if defined(GLIBC231_STYLE_UCONTEXT)
    /* FIXME: This probably should be ->ccr instead of ->gpr[PT_CCR]. */
    return &((context->uc_mcontext.regs)->gpr[PT_CCR]);
#elif defined(GLIBC232_STYLE_UCONTEXT)
    return &((context->uc_mcontext.uc_regs)->gregs[PT_CCR]);
#endif
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
#if defined(GLIBC231_STYLE_UCONTEXT)
    return &context->uc_sigmask;
#elif defined(GLIBC232_STYLE_UCONTEXT)
    return &context->uc_sigmask;
#endif
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    /* So this may look like nice, well behaved code. However, closer
       inspection reveals that gpr is simply the general purpose
       registers, and PT_FPSCR is an offset that is larger than 32
       (the number of ppc registers), but that happens to get the
       right answer. -- CSR, 2002-07-11 */
#if defined(GLIBC231_STYLE_UCONTEXT)
    return context->uc_mcontext.regs->gpr[PT_FPSCR];
#elif defined(GLIBC232_STYLE_UCONTEXT)
    return context->uc_mcontext.uc_regs->gregs[PT_FPSCR];
#endif
}

void
os_restore_fp_control(os_context_t *context)
{
    /* KLUDGE: mtfsf has to be run against a float register, so we
     * construct the float we need to use as an integer, then cast
     * a pointer to its storage to a double and load that.  For
     * this to work, control must be the same width as a double,
     * 64 bits.  And why aren't we using a union here, anyway? */
    unsigned long long control;
    double d;

    /* FIXME: We are only preserving enabled traps and rounding
     * mode here.  Do we also want to preserve "fast mode"? */
    control = os_context_fp_control(context) &
        (FLOAT_TRAPS_BYTE_MASK | FLOAT_ROUNDING_MODE_MASK);

    d = *((double *) &control);
    asm volatile ("mtfsf 0xff,%0" : : "f" (d));
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    /* see ppc-arch.c */
    ppc_flush_icache(address,length);
}

// "cc -S" on this file shows that the C compiler is responsible for making
// a substitution for these functions with an additional argument in front.
// I don't want to know how to do that from Lisp.
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
int _stat(const char *pathname, struct stat *sb) {return stat(pathname, sb); }
int _lstat(const char *pathname, struct stat *sb) { return lstat(pathname, sb); }
int _fstat(int fd, struct stat *sb) { return fstat(fd, sb); }
