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
#include "getallocptr.h"

os_vm_address_t
arch_get_bad_addr(int signam, siginfo_t *siginfo, os_context_t *context)
{
    return (os_vm_address_t)siginfo->si_addr;
}

void arch_skip_instruction(os_context_t *context)
{
    /* KLUDGE: Other platforms check for trap codes and skip inlined
     * trap/error parameters.  We should too. */

    /* Note that we're doing integer arithmetic here, not pointer. So
     * the value that the return value of os_context_pc_addr() points
     * to will be incremented by 4, not 16.
     */
    *os_context_pc_addr(context) += 4;
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char*)(*os_context_pc_addr(context) + 5);
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
    return get_pseudo_atomic_atomic(get_sb_vm_thread());
#else
    return (!foreign_function_call_active)
        && (NIL != SymbolValue(PSEUDO_ATOMIC_ATOMIC,0));
#endif
}

void arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    set_pseudo_atomic_interrupted(get_sb_vm_thread());
}

void arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
    clear_pseudo_atomic_interrupted(get_sb_vm_thread());
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
    *os_context_pc_addr(context) = (long) handle_fun_end_breakpoint(context);
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    unsigned char register_offset =
      *((unsigned char *)(*os_context_pc_addr(context)) + 5);
    handle_single_step_trap(context, trap, register_offset);
    /* KLUDGE: arch_skip_instruction() only skips one instruction, and
     * there is a following word to deal with as well, so skip
     * twice. */
    arch_skip_instruction(context);
    arch_skip_instruction(context);
}

void
sigtrap_handler(int signal, siginfo_t *info, os_context_t *context)
{
    uint32_t trap_instruction = *((uint32_t *)*os_context_pc_addr(context));

    if (trap_instruction != 0x100073) {
        lose("Unrecognized trap instruction %08x in sigtrap_handler()",
             trap_instruction);
    }

    uint32_t code = *((uint32_t *)(4 + *os_context_pc_addr(context)));

    if (code == trap_PendingInterrupt) {
      arch_skip_instruction(context);
    }

    handle_trap(context, code);
}

void
arch_install_interrupt_handlers(void)
{
    ll_install_handler(SIGTRAP, sigtrap_handler);
}

/* Linkage table */
void arch_write_linkage_table_entry(int index, void *target_addr, int datap)
{
    // allocate successive entries downward
    char *reloc_addr =
        (char*)LINKAGE_TABLE_SPACE_END - (index + 1) * LINKAGE_TABLE_ENTRY_SIZE;
    *(uword_t*)reloc_addr = (uword_t)target_addr;
}

lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs) {
    return ((lispobj(*)(lispobj, lispobj *, int, struct thread*))SYMBOL(CALL_INTO_LISP)->value)
      (fun, args, nargs, get_sb_vm_thread());
}
