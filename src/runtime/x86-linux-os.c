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

#if defined GENCGC
#include "gencgc.h"
#endif

/* KLUDGE: As of kernel 2.2.14 on Red Hat 6.2, there's code in the
 * <sys/ucontext.h> file to define symbolic names for offsets into
 * gregs[], but it's conditional on __USE_GNU and not defined, so
 * we need to do this nasty absolute index magic number thing
 * instead. */
os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case  0: return &context->uc_mcontext.gregs[11]; /* EAX */
    case  2: return &context->uc_mcontext.gregs[10]; /* ECX */
    case  4: return &context->uc_mcontext.gregs[9]; /* EDX */
    case  6: return &context->uc_mcontext.gregs[8]; /* EBX */
    case  8: return &context->uc_mcontext.gregs[7]; /* ESP */
    case 10: return &context->uc_mcontext.gregs[6]; /* EBP */
    case 12: return &context->uc_mcontext.gregs[5]; /* ESI */
    case 14: return &context->uc_mcontext.gregs[4]; /* EDI */
    default: return 0;
    }
    return &context->uc_mcontext.gregs[offset];
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[14];
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[17];
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    /* probably the code snippet
     * #ifdef __linux__
     *    SET_FPU_CONTROL_WORD(context->__fpregs_mem.cw);
     * #endif 
     * is relevant to implementing this correctly */

    /* Note that currently this is not called, as there is an analogous
     * stub in lisp-land (x86-vm.lisp), also returning 0, with the old
     * lisp fp-control code. This is here more as a signpost of a possible
     * way of restoring functionality, and if it is the way to go would
     * need to be included for other architectures as well. */
    return 0;
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

