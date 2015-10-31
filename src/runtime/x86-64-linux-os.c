/*
 * The x86-64 Linux incarnation of arch-dependent OS-dependent
 * routines.  See also "linux-os.c".
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

#define _GNU_SOURCE /* for REG_RAX etc. from sys/ucontext */

#include <stdio.h>
#include <stddef.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>

#include <sys/ucontext.h>

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
#include <linux/unistd.h>
#include <sys/mman.h>
#include <linux/version.h>
#include "thread.h"             /* dynamic_values_bytes */

#include "validate.h"
size_t os_vm_page_size;

int arch_os_thread_init(struct thread *thread) {
    stack_t sigstack;
#ifdef LISP_FEATURE_SB_THREAD
#ifdef LISP_FEATURE_GCC_TLS
    current_thread = thread;
#else
    pthread_setspecific(specials,thread);
#endif
#endif
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp=((void *) thread)+dynamic_values_bytes;
    sigstack.ss_flags=0;
    sigstack.ss_size = 32*SIGSTKSZ;
    if(sigaltstack(&sigstack,0)<0) {
        lose("Cannot sigaltstack: %s\n",strerror(errno));
    }
#endif
    return 1;
}

/* free any arch/os-specific resources used by thread, which is now
 * defunct.  Not called on live threads
 */

int arch_os_thread_cleanup(struct thread *thread) {
    return 1;
}


os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
#define RCASE(name) case reg_ ## name: return \
 (os_context_register_t*)&context->uc_mcontext.gregs[REG_ ## name];
    switch(offset) {
        RCASE(RAX)
        RCASE(RCX)
        RCASE(RDX)
        RCASE(RBX)
        RCASE(RSP)
        RCASE(RBP)
        RCASE(RSI)
        RCASE(RDI)
        RCASE(R8)
        RCASE(R9)
        RCASE(R10)
        RCASE(R11)
        RCASE(R12)
        RCASE(R13)
        RCASE(R14)
        RCASE(R15)
      default:
        if(offset<NGREG)
            return (os_context_register_t*)&context->uc_mcontext.gregs[offset/2+4];
        else return 0;
    }
    return (os_context_register_t*)&context->uc_mcontext.gregs[offset];
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return (os_context_register_t*)&context->uc_mcontext.gregs[REG_RIP]; /*  REG_EIP */
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return (os_context_register_t*)&context->uc_mcontext.gregs[REG_RSP];
}

os_context_register_t *
os_context_fp_addr(os_context_t *context)
{
    return (os_context_register_t*)&context->uc_mcontext.gregs[REG_RBP];
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    return (os_context_register_t*)&context->uc_mcontext.gregs[REG_RSP];

}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t*)&context->uc_mcontext.fpregs->_xmm[offset];
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

void
os_restore_fp_control(os_context_t *context)
{
    if (context->uc_mcontext.fpregs) {
        /* reset exception flags and restore control flags on SSE2 FPU */
        unsigned int temp = (context->uc_mcontext.fpregs->mxcsr) & ~0x3F;
        asm ("ldmxcsr %0" : : "m" (temp));
        /* same for x87 FPU. */
        asm ("fldcw %0" : : "m" (context->uc_mcontext.fpregs->cwd));
    }
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

