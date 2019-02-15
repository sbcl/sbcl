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
    *os_context_pc_addr(context) += 4;
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char*)*os_context_pc_addr(context);
}

boolean arch_pseudo_atomic_atomic(os_context_t *context)
{
    /* FIXME: this foreign_function_call_active test is dubious at
     * best. If a foreign call is made in a pseudo atomic section
     * (?) or more likely a pseudo atomic section is in a foreign
     * call then an interrupt is executed immediately. Maybe it
     * has to do with C code not maintaining pseudo atomic
     * properly. MG - 2005-08-10
     *
     * The foreign_function_call_active used to live at each call-site
     * to arch_pseudo_atomic_atomic, but this seems clearer.
     * --NS 2007-05-15 */
#ifdef LISP_FEATURE_GENCGC
    return get_pseudo_atomic_atomic(arch_os_get_current_thread());
#else
    return (!foreign_function_call_active)
        && (NIL != SymbolValue(PSEUDO_ATOMIC_ATOMIC,0));
#endif
}

void arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, (lispobj)do_pending_interrupt, 0);
}

void arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, 0, 0);
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
