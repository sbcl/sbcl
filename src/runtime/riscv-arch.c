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

#include "genesis/sbcl.h"
#include "runtime.h"
#include "arch.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "lispregs.h"
#include <signal.h>
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"
#include "pseudo-atomic.h"

os_vm_address_t
arch_get_bad_addr(int signam, siginfo_t *siginfo, os_context_t *context)
{
    return (os_vm_address_t)siginfo->si_addr;
}

void arch_skip_instruction(os_context_t *context)
{
    /* KLUDGE: Other platforms check for trap codes and skip inlined
     * trap/error parameters.  We should too. */
    OS_CONTEXT_PC(context) += 8;
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char*)(OS_CONTEXT_PC(context) + 5);
}

bool arch_pseudo_atomic_atomic(struct thread *thread) {
    return get_pseudo_atomic_atomic(thread);
}

void arch_set_pseudo_atomic_interrupted(struct thread *thread) {
    set_pseudo_atomic_interrupted(thread);
}

void arch_clear_pseudo_atomic_interrupted(struct thread *thread) {
    clear_pseudo_atomic_interrupted(thread);
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
    OS_CONTEXT_PC(context) = (long) handle_fun_end_breakpoint(context);
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    unsigned char register_offset =
      *((unsigned char *)(OS_CONTEXT_PC(context)) + 5);
    handle_single_step_trap(context, trap, register_offset);
    arch_skip_instruction(context);
}

int riscv_user_emulation;

static void
sigtrap_handler(int signal, siginfo_t *info, os_context_t *context)
{
    uint32_t trap_instruction = *(uint32_t *)OS_CONTEXT_PC(context);

    static int sigaction_workaround;
    if (riscv_user_emulation && !sigaction_workaround) {
        sigset_t curmask;
        thread_sigmask(SIG_BLOCK, 0, &curmask);
        if (*(unsigned long int*)&curmask == 0) {
            char msg[] = "WARNING: broken sigaction() workaround enabled\n";
            ignore_value(write(2, msg, sizeof msg-1));
            sigaction_workaround = 1;
        } else {
            sigaction_workaround = -1;
        }
    }
    if (sigaction_workaround == 1) thread_sigmask(SIG_BLOCK, &blockable_sigset, 0);

    if (trap_instruction != 0x100073) {
        lose("Unrecognized trap instruction %08x in sigtrap_handler()",
             trap_instruction);
    }

    unsigned char code = *((unsigned char *)(4 + OS_CONTEXT_PC(context)));

    handle_trap(context, code);
}

void
arch_install_interrupt_handlers(void)
{
    ll_install_handler(SIGTRAP, sigtrap_handler);
}

/* Linkage tables
 *
 * Linkage entry size is 8 or 20, because we need 2 instructions for the 32-bit case and we need 3 instructions and an 8 byte address in the 64-bit case.
 */

#define LINKAGE_TEMP_REG reg_LIP // Lisp needs to save before entry.

void arch_write_linkage_table_entry(int index, void *target_addr, int datap)
{
    // allocate successive entries downward
    char *reloc_addr =
        (char*)ALIEN_LINKAGE_SPACE_END - (index + 1) * ALIEN_LINKAGE_TABLE_ENTRY_SIZE;
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

lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs) {
    return ((lispobj(*)(lispobj, lispobj *, int, struct thread*))SYMBOL(CALL_INTO_LISP)->value)
      (fun, args, nargs, get_sb_vm_thread());
}
