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

#include "sbcl.h"
#include "runtime.h"
#include "arch.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "lispregs.h"
#include "signal.h"
#include "alloc.h"
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"

void arch_init(void)
{
    return;
}

os_vm_address_t
arch_get_bad_addr(int signam, siginfo_t *siginfo, os_context_t *context)
{
    return (os_vm_address_t)siginfo->si_addr;
}

void arch_skip_instruction(os_context_t *context)
{
#warning "Implement arch_skip_instruction"
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
#warning "Implement."
}

boolean arch_pseudo_atomic_atomic(os_context_t *context)
{
#warning "Implement."
}

void arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
#warning "Implement."
}

void arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
#warning "Implement."
}

unsigned int arch_install_breakpoint(void *pc)
{
#warning "Implement."
    return 0;
}

void arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
#warning "Implement."
}

void arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
#warning "Implement."
}

void
arch_handle_breakpoint(os_context_t *context)
{
    handle_breakpoint(context);
}

void
arch_handle_fun_end_breakpoint(os_context_t *context)
{
    *os_context_pc_addr(context) = (int) handle_fun_end_breakpoint(context);
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
#warning "Implement."
}

void
sigtrap_handler(int signal, siginfo_t *info, os_context_t *context)
{
#warning "Implement."
}

void
arch_install_interrupt_handlers(void)
{
    undoably_install_low_level_interrupt_handler(SIGTRAP, sigtrap_handler);
}
