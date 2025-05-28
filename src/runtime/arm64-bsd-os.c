/*
 * This is the ARM BSD incarnation of arch-dependent OS-dependent
 * routines. See also "bsd-os.c".
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

#ifndef LISP_FEATURE_DARWIN
#include <machine/sysarch.h>
#endif

#include <sys/types.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include "validate.h"

int arch_os_thread_cleanup(struct thread *thread) {
    if (thread->breakpoint_misc)
        os_deallocate((os_vm_address_t) thread->breakpoint_misc, getpagesize());
    return 1;                   /* success */
}

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
        lose("Cannot sigaltstack: %s\n",strerror(errno));

    return 1;                   /* success */
}


#if defined(LISP_FEATURE_OPENBSD)

os_context_register_t   *
os_context_register_addr(os_context_t *context, int regno)
{
    switch (regno) {
        case reg_LR:    return (&context->sc_lr);
        case reg_NSP:   return (&context->sc_sp);
        default:        return (&context->sc_x[regno]);
    }
}

os_context_register_t   *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return NULL;
}

void
os_restore_fp_control(os_context_t *context)
{
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return os_context_register_addr(context, reg_LR);
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    os_vm_address_t end_address
        = (os_vm_address_t)(((uintptr_t) address) + length);
    __clear_cache(address, end_address);
}
os_context_register_t *
os_context_flags_addr(os_context_t *context)
{
    return (os_context_register_t*)(&context->sc_spsr);
}

#elif defined(LISP_FEATURE_NETBSD)
os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t *)&(context->uc_mcontext.__gregs[offset]);
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return os_context_register_addr(context, reg_LR);
}

void
os_restore_fp_control(os_context_t *context)
{
    /* FIXME: Implement. */
}

os_context_register_t   *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t*)
        &context->uc_mcontext.__fregs.__qregs[offset];
}

os_context_register_t *
os_context_flags_addr(os_context_t *context)
{
    return (os_context_register_t *)&(context->uc_mcontext.__gregs[_REG_SPSR]);
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    __builtin___clear_cache(address, address + length);
}
#elif defined LISP_FEATURE_FREEBSD
os_context_register_t   *
os_context_register_addr(os_context_t *context, int offset)
{
      switch (offset) {
        case reg_LR:    return (&context->uc_mcontext.mc_gpregs.gp_lr);
        case reg_NSP:   return (&context->uc_mcontext.mc_gpregs.gp_sp);
        default:        return (&context->uc_mcontext.mc_gpregs.gp_x[offset]);
    }
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return os_context_register_addr(context, reg_LR);
}

void
os_restore_fp_control(os_context_t *context)
{
    /* FIXME: Implement. */
}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t*) &context->uc_mcontext.mc_fpregs.fp_q[offset];
}

os_context_register_t *
os_context_flags_addr(os_context_t *context)
{
    return (os_context_register_t*)(&context->uc_mcontext.mc_gpregs.gp_spsr);
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    __builtin___clear_cache(address, address + length);
}
#elif defined (LISP_FEATURE_DARWIN)
os_context_register_t *
os_context_register_addr(os_context_t *context, int regno)
{
    switch (regno) {
        case reg_LR:    return (os_context_register_t*)(&context->uc_mcontext->__ss.__lr);
        case reg_NSP:   return (os_context_register_t*)(&context->uc_mcontext->__ss.__sp);
        default:        return (os_context_register_t*)(&context->uc_mcontext->__ss.__x[regno]);
    }
}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
    return (os_context_register_t*)(&context->uc_mcontext->__ns.__v[offset]);
}

unsigned int
os_context_fp_control(os_context_t *context)
{
    return context->uc_mcontext->__ns.__fpsr | context->uc_mcontext->__ns.__fpcr;
}

void
os_context_set_fp_control(os_context_t *context, unsigned int value)
{
    context->uc_mcontext->__ns.__fpsr = value;
    context->uc_mcontext->__ns.__fpcr = value;
}

void
os_restore_fp_control(os_context_t *context)
{
}

os_context_register_t *
os_context_lr_addr(os_context_t *context)
{
    return os_context_register_addr(context, reg_LR);
}

os_context_register_t *
os_context_flags_addr(os_context_t *context)
{
    return (os_context_register_t*)(&context->uc_mcontext->__ss.__cpsr);
}
#endif
