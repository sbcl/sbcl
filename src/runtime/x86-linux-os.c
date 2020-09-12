/*
 * The x86 Linux incarnation of arch-dependent OS-dependent routines.
 * See also "linux-os.c".
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
#include <stddef.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

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
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <asm/ldt.h>
#include <sys/syscall.h>
#include <sys/mman.h>
#include <linux/version.h>
#include "thread.h"             /* dynamic_values_bytes */

static inline int set_thread_area(struct user_desc *u_info)
{
    return syscall(SYS_set_thread_area, u_info);
}

#include "validate.h"

int arch_os_thread_init(struct thread *thread) {
    stack_t sigstack;
#ifdef LISP_FEATURE_SB_THREAD
    struct user_desc desc = {
        0, (unsigned long) thread, dynamic_values_bytes,
        1, 0, 0, 1, 0, 1
    };

    static int entry_number = -1;

    desc.entry_number = entry_number;

    if (set_thread_area(&desc) != 0) {
        return 0;
    }

    entry_number = desc.entry_number;

    __asm__ __volatile__ ("movw %w0, %%fs" : : "q"
                          ((entry_number << 3) + 3));

    if(entry_number < 0) return 0;
#endif

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp    = calc_altstack_base(thread);
    sigstack.ss_flags = 0;
    sigstack.ss_size  = calc_altstack_size(thread);
    if(sigaltstack(&sigstack,0)<0)
        lose("Cannot sigaltstack: %s",strerror(errno));
#endif
    return 1;
}

struct thread *debug_get_fs() {
    register uint32_t fs;
    __asm__ __volatile__ ("movl %%fs,%0" : "=r" (fs)  : );
    return (struct thread *)fs;
}

/* free any arch/os-specific resources used by thread, which is now
 * defunct.  Not called on live threads
 */

int arch_os_thread_cleanup(struct thread *thread) {
    return 1;
}



/* KLUDGE: As of kernel 2.2.14 on Red Hat 6.2, there's code in the
 * <sys/ucontext.h> file to define symbolic names for offsets into
 * gregs[], but it's conditional on __USE_GNU and not defined, so
 * we need to do this nasty absolute index magic number thing
 * instead. */
os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case reg_EAX: return &context->uc_mcontext.gregs[11];
    case reg_ECX: return &context->uc_mcontext.gregs[10];
    case reg_EDX: return &context->uc_mcontext.gregs[9];
    case reg_EBX: return &context->uc_mcontext.gregs[8];
    case reg_ESP: return &context->uc_mcontext.gregs[7];
    case reg_EBP: return &context->uc_mcontext.gregs[6];
    case reg_ESI: return &context->uc_mcontext.gregs[5];
    case reg_EDI: return &context->uc_mcontext.gregs[4];
    default: return 0;
    }
    return &context->uc_mcontext.gregs[offset];
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[14]; /*  REG_EIP */
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[17]; /* REG_UESP */
}

os_context_register_t *
os_context_fp_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[6]; /* REG_EBP */
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    return ((((context->uc_mcontext.fpregs->cw) & 0xffff) ^ 0x3f) |
            (((context->uc_mcontext.fpregs->sw) & 0xffff) << 16));
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

void
os_restore_fp_control(os_context_t *context)
{
    if (context->uc_mcontext.fpregs)
        asm ("fldcw %0" : : "m" (context->uc_mcontext.fpregs->cw));
}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t*)&context->uc_mcontext.fpregs->_st[offset];
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

// To avoid "Missing required foreign symbol" errors in os_link_runtime()
// the executable must actually depend on libm. It would not require libm,
// despite -lm in the link step, if there is no reference to a libm symbol
// observable to the linker. Any one symbol suffices to resolve all of them.
#include <math.h>
const long libm_anchor = (long)acos;
