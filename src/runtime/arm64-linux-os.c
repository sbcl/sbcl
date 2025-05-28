/*
 * This is the ARM Linux incarnation of arch-dependent OS-dependent
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
#include "genesis/sbcl.h"
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
#include <errno.h>

#include "validate.h"

int arch_os_thread_init(struct thread *thread) {
    stack_t sigstack;
    /* Signal handlers are normally run on the main stack, but we've
     * swapped stacks, require that the control stack contain only
     * boxed data, and expands upwards while the C stack expands
     * downwards. */
    sigstack.ss_sp    = calc_altstack_base(thread);
    sigstack.ss_flags = 0;
    sigstack.ss_size  = calc_altstack_size(thread);
    if(sigaltstack(&sigstack,0)<0)
        lose("Cannot sigaltstack: %s",strerror(errno));

    return 1;                   /* success */
}
int arch_os_thread_cleanup(struct thread *thread) {
    if (thread->breakpoint_misc)
        os_deallocate((os_vm_address_t) thread->breakpoint_misc, getpagesize());
    return 1;                   /* success */
}

os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t *)&(context->uc_mcontext.regs[offset]);
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return os_context_register_addr(context, reg_LR);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &(context->uc_sigmask);
}

os_context_register_t *
os_context_flags_addr(os_context_t *context)
{
    return (os_context_register_t *)&(context->uc_mcontext.pstate);
}

void
os_restore_fp_control(os_context_t *context)
{
    /* FIXME: Implement. */
}

os_context_register_t   *
os_context_float_register_addr(os_context_t *context, int offset)
{
    /* KLUDGE: neither glibc nor the kernel can settle down on a name,
       the kernel used __reserved, now glibc uses __glibc_reserved1.
       Hardcode it until they make up their mind */
    return (os_context_register_t*)
        &((struct fpsimd_context *)((uintptr_t)&context->uc_mcontext + 288))->vregs[offset];
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    os_vm_address_t end_address
        = (os_vm_address_t)(((uintptr_t) address) + length);
    __clear_cache(address, end_address);
}

// To avoid "Missing required foreign symbol" errors in os_link_runtime()
// the executable must actually depend on libm. It would not require libm,
// despite -lm in the link step, if there is no reference to a libm symbol
// observable to the linker. Any one symbol suffices to resolve all of them.
#include <math.h>
const long libm_anchor = (long)acos;

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
int _stat(const char *pathname, struct stat *sb) {return stat(pathname, sb); }
int _lstat(const char *pathname, struct stat *sb) { return lstat(pathname, sb); }
int _fstat(int fd, struct stat *sb) { return fstat(fd, sb); }
