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
#include "monitor.h"

os_vm_address_t arch_get_bad_addr(int sig, siginfo_t *code, os_context_t *context)
{
    return (os_vm_address_t)code->si_addr;
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
    return (unsigned char *)(*os_context_pc_addr(context) + 5);
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
    extern void do_pending_interrupt();
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, (lispobj)do_pending_interrupt, 0);
}

void arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, 0, 0);
}

unsigned int arch_install_breakpoint(void *pc)
{
    /* FIXME: Implement. */

    return 0;
}

void arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    /* FIXME: Implement. */
}

void arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    /* FIXME: Implement. */
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
        *((unsigned char *)(*os_context_pc_addr(context))+5);
    handle_single_step_trap(context, trap, register_offset);
    /* KLUDGE: arch_skip_instruction() only skips one instruction, and
     * there is a following word to deal with as well, so skip
     * twice. */
    arch_skip_instruction(context);
    arch_skip_instruction(context);
}



/* Linkage tables
 *
 * Linkage entry size is 16, because we need 4 instructions.
 */

#define LINKAGE_TEMP_REG        reg_NFP

void arch_write_linkage_table_entry(int index, void *target_addr, int datap)
{
  // allocate successive entries downward
  char *reloc_addr =
      (char*)LINKAGE_TABLE_SPACE_END - (index + 1) * LINKAGE_TABLE_ENTRY_SIZE;
  if (datap) {
    *(unsigned long *)reloc_addr = (unsigned long)target_addr;
    return;
  }
  /*
    ldr reg, [pc, #4]
    bx  reg
    nop
    address

    BX is needed for thumb interworking, without it it could take just two words with
    ldr pc, [pc, #-4]
    address
  */
  int* inst_ptr;
  unsigned inst;

  inst_ptr = (int*) reloc_addr;

  // ldr reg, [pc, #4]
  inst = 0xe59f0000 | LINKAGE_TEMP_REG << 12 | 4;
  *inst_ptr++ = inst;

  // bx reg
  inst = 0xe12fff10 | LINKAGE_TEMP_REG;
  *inst_ptr++ = inst;

  // nop aka mov r0, r0
  inst = 0xe1a00000;
  *inst_ptr++ = inst;

  // address
  *inst_ptr++ = (int)target_addr;

  os_flush_icache((os_vm_address_t) reloc_addr, (char*) inst_ptr - reloc_addr);
}
