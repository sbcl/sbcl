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
    return SymbolValue(PSEUDO_ATOMIC_ATOMIC, 0) != NIL;
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
    u32 trap_instruction = *((u32 *)*os_context_pc_addr(context));

    if (trap_instruction != 0x100073) {
        lose("Unrecognized trap instruction %08lx in sigtrap_handler()",
             trap_instruction);
    }

    u32 code = *((u32 *)(4 + *os_context_pc_addr(context)));

    if (code == trap_PendingInterrupt) {
      arch_skip_instruction(context);
    }

    handle_trap(context, code);
}

void
arch_install_interrupt_handlers(void)
{
    undoably_install_low_level_interrupt_handler(SIGTRAP, sigtrap_handler);
}

#ifdef LISP_FEATURE_LINKAGE_TABLE

/* Linkage tables
 *
 * Linkage entry size is 8 or 20, because we need 2 instructions for the 32-bit case and we need 3 instructions and an 8 byte address in the 64-bit case.
 */

#define LINKAGE_TEMP_REG reg_NL7

void arch_write_linkage_table_entry(char *reloc_addr, void *target_addr, int datap)
{
    if (datap) {
      *(unsigned long *)reloc_addr = (unsigned long)target_addr;
      return;
    }
    int* inst_ptr;
    unsigned inst;

    inst_ptr = (int*) reloc_addr;

#ifndef LISP_FEATURE_64_BIT
    /*
      lui   reg, %hi(address)
      jr    reg, %lo(address)
    */
    unsigned int addr = (unsigned int)target_addr;
    unsigned int hi = ((addr + 0x800) >> 12);
    int lo = addr - (hi << 12);

    inst = 0x37 | LINKAGE_TEMP_REG << 7 | hi << 12;
    *inst_ptr++ = inst;

    inst = 0x67 | LINKAGE_TEMP_REG << 15 | lo << 20;
    *inst_ptr++ = inst;
#else
    /*
      auipc reg, 0
      load reg, 12(reg)
      jr  reg
      address
    */

    inst = 0x17 | LINKAGE_TEMP_REG << 7;
    *inst_ptr++ = inst;

    inst = 0x3 | LINKAGE_TEMP_REG << 7 | WORD_SHIFT << 12 | LINKAGE_TEMP_REG << 15 | 12 << 20;
    *inst_ptr++ = inst;

    inst = 0x67 | LINKAGE_TEMP_REG << 15;
    *inst_ptr++ = inst;

    *(unsigned long *)inst_ptr++ = (unsigned long)target_addr;
#endif

    os_flush_icache((os_vm_address_t) reloc_addr, (char*) inst_ptr - reloc_addr);
}
#endif
