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
#include <unistd.h>

#define BREAK_INST 0x002A0000
#define BREAK_MASK 0xFFFF8000

os_vm_address_t
arch_get_bad_addr(int signam, siginfo_t *siginfo, os_context_t *context)
{
    return (os_vm_address_t)siginfo->si_addr;
}

void arch_skip_instruction(os_context_t *context)
{
    uint32_t trap_instruction = *(uint32_t *)OS_CONTEXT_PC(context);
    unsigned code = trap_instruction & 0xFF;
    if(code == trap_FunEndBreakpoint || code == trap_Breakpoint)
        OS_CONTEXT_PC(context) += 4;
    else
        OS_CONTEXT_PC(context) += 8;
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char*)(OS_CONTEXT_PC(context));
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
    unsigned int *ptr = (unsigned int *)pc;
    unsigned int result = *ptr;

    *ptr = BREAK_INST | trap_Breakpoint;

    os_flush_icache((os_vm_address_t) ptr, sizeof(unsigned int));
    return result;
}

void arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    *(unsigned int *) pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));
}

static int sign_extend_u(int16_t value, int n_bits) {
    int shift = 32 - n_bits;
    return (value << shift) >> shift;
}

static bool is_load_store_pc_rel(unsigned int inst) {
    unsigned int opcode = (inst >> 22) & 0x3FF;
    switch(opcode) {
        case 0b0010100000: // LD.B
        case 0b0010100001: // LD.H
        case 0b0010100010: // LD.W
        case 0b0010100011: // LD.D
        case 0b0010101000: // LD.BU
        case 0b0010101001: // LD.HU
        case 0b0010101010: // LD.WU
        case 0b0010100100: // ST.B
        case 0b0010100101: // ST.H
        case 0b0010100110: // ST.W
        case 0b0010100111: // ST.D
            return true;
        default:
            return false;
    }
}

#define BREAKPOINT_TEMP_REG reg_T7

void arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned int *pc = (unsigned int *)OS_CONTEXT_PC(context);
    unsigned int *next_pc = pc;

    if (((orig_inst >> 26) & 0x3F) >= 0b010110 && ((orig_inst >> 26) & 0x3F) <= 0b011011) {
        int rj = (orig_inst >> 5) & 0x1F;
        int rd = (orig_inst >> 0) & 0x1F;
        int16_t imm16 = orig_inst & 0xFFFF;
        int offset = sign_extend_u(imm16, 16);

        uword_t val_j = *os_context_register_addr(context, rj);
        uword_t val_d = *os_context_register_addr(context, rd);

        int take_branch = 0;
        switch ((orig_inst >> 26) & 0x3F) {
            case 0b010110: take_branch = (val_j == val_d); break;          // BEQ
            case 0b010111: take_branch = (val_j != val_d); break;          // BNE
            case 0b011000: take_branch = ((sword_t)val_j < (sword_t)val_d); break; // BLT
            case 0b011001: take_branch = ((sword_t)val_j >= (sword_t)val_d); break; // BGE
            case 0b011010: take_branch = (val_j < val_d); break;           // BLTU
            case 0b011011: take_branch = (val_j >= val_d); break;          // BGEU
            default: lose("Unknown conditional branch");
        }

        if (take_branch)
            next_pc += offset;
        else
            next_pc += 1;
    }

    // JIRL
    else if (((orig_inst >> 26) & 0x3F) == 0b010011) {
        int rj = (orig_inst >> 5) & 0x1F;
        int rd = (orig_inst >> 0) & 0x1F;
        int16_t imm16 = orig_inst & 0xFFFF;
        int offset = sign_extend_u(imm16, 16);

        uword_t target = *os_context_register_addr(context, rj) + offset;

        if (rd != 0)
            *os_context_register_addr(context, rd) = (uword_t)(pc + 2);
        next_pc = (unsigned int *)target;
    }

    // Load/Store PC-relative
    else if (is_load_store_pc_rel(orig_inst)) {
        int rd = orig_inst & 0x1F;
        int rj = (orig_inst >> 5) & 0x1F;
        int16_t si12 = (orig_inst >> 10) & 0xFFF;
        int offset = sign_extend_u(si12, 12);
        uword_t addr = *os_context_register_addr(context, rj) + offset;

        switch ((orig_inst >> 22) & 0x3FF) {
            case 0b0010100000: *os_context_register_addr(context, rd) = (sword_t)*(int8_t *)addr; break;   // LD.B
            case 0b0010100001: *os_context_register_addr(context, rd) = (sword_t)*(int16_t *)addr; break;  // LD.H
            case 0b0010100010: *os_context_register_addr(context, rd) = (sword_t)*(int32_t *)addr; break;  // LD.W
            case 0b0010100011: *os_context_register_addr(context, rd) = *(uword_t *)addr; break;          // LD.D
            case 0b0010101000: *os_context_register_addr(context, rd) = *(uint8_t *)addr; break;          // LD.BU
            case 0b0010101001: *os_context_register_addr(context, rd) = *(uint16_t *)addr; break;         // LD.HU
            case 0b0010101010: *os_context_register_addr(context, rd) = *(uint32_t *)addr; break;         // LD.WU
            case 0b0010100100: *(int8_t *)addr = (int8_t)*os_context_register_addr(context, rd); break;  // ST.B
            case 0b0010100101: *(int16_t *)addr = (int16_t)*os_context_register_addr(context, rd); break; // ST.H
            case 0b0010100110: *(int32_t *)addr = (int32_t)*os_context_register_addr(context, rd); break; // ST.W
            case 0b0010100111: *(uword_t *)addr = *os_context_register_addr(context, rd); break;          // ST.D
            default: lose("Unknown load/store instruction");
        }

        next_pc += 1;
    }

    // Trampoline
    else {
        size_t size = getpagesize();
        struct thread *th = get_sb_vm_thread();
        unsigned int *trampoline = (unsigned int *)th->breakpoint_misc;

        if (!trampoline) {
            trampoline = mmap(NULL, size,
                              PROT_READ | PROT_WRITE,
                              MAP_PRIVATE | MAP_ANONYMOUS,
                              -1, 0);
            th->breakpoint_misc = trampoline;
        } else {
            os_protect((os_vm_address_t)trampoline, size, PROT_READ | PROT_WRITE);
        }

        unsigned int *inst_ptr = trampoline;

        next_pc += 1;

        *inst_ptr++ = orig_inst;

        *inst_ptr++ = 0x1C000000 | BREAKPOINT_TEMP_REG;
        *inst_ptr++ = 0x28C00000 | BREAKPOINT_TEMP_REG << 5 | BREAKPOINT_TEMP_REG | 12 << 10;
        *inst_ptr++ = 0x4C000000  | BREAKPOINT_TEMP_REG << 5;

        *(uword_t *)inst_ptr++ = (uword_t)next_pc;

        OS_CONTEXT_PC(context) = (uword_t)trampoline;
        os_flush_icache((os_vm_address_t)trampoline, (char*)inst_ptr - (char*)trampoline);
        os_protect((os_vm_address_t)trampoline, size, PROT_READ | PROT_EXEC);
        return;
    }

    OS_CONTEXT_PC(context) = (uword_t)next_pc;
}


void
arch_handle_breakpoint(os_context_t *context)
{
    handle_breakpoint(context);
}

void
arch_handle_fun_end_breakpoint(os_context_t *context)
{
    OS_CONTEXT_PC(context) = (uword_t)handle_fun_end_breakpoint(context);
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    handle_single_step_trap(context, trap, reg_LEXENV);
    arch_skip_instruction(context);
}


static void
sigtrap_handler(int signal, siginfo_t *info, os_context_t *context)
{

    uint32_t trap_instruction;
    memcpy(&trap_instruction, (void *)OS_CONTEXT_PC(context), sizeof trap_instruction);
    unsigned code;
    unsigned code_1 = trap_instruction & 0xFF;

    if ((trap_instruction & BREAK_MASK) != BREAK_INST) {
        lose("Unrecognized trap instruction %08x in sigtrap_handler()",
             trap_instruction);
    }
    if (code_1 == trap_Breakpoint || code_1 == trap_FunEndBreakpoint)
        code = code_1;
    else
        code = *((unsigned char *)(4 + OS_CONTEXT_PC(context)));

    handle_trap(context, code);
}

void
arch_install_interrupt_handlers(void)
{
    ll_install_handler(SIGTRAP, sigtrap_handler);
}

#define LINKAGE_TEMP_REG reg_LIP


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

    inst = 0x16000000 | LINKAGE_TEMP_REG | hi << 5;
    *inst_ptr++ = inst;

    inst = 0x4C000000 | LINKAGE_TEMP_REG << 5 | lo << 10;
    *inst_ptr++ = inst;
#else
    /*
      pcaddu12i reg, 0
      load reg, 12(reg)
      jirl  reg
      address
    */

    inst = 0x1C000000 | LINKAGE_TEMP_REG;
    *inst_ptr++ = inst;

    inst = 0x28C00000 | LINKAGE_TEMP_REG << 5 | LINKAGE_TEMP_REG | 12 << 10;
    *inst_ptr++ = inst;

    inst = 0x4C000000  | LINKAGE_TEMP_REG << 5;
    *inst_ptr++ = inst;

    *(unsigned long *)inst_ptr++ = (unsigned long)target_addr;
#endif

    os_flush_icache((os_vm_address_t) reloc_addr, (char*) inst_ptr - reloc_addr);
}

lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs) {
    return ((lispobj(*)(lispobj, lispobj *, int, struct thread*))SYMBOL(CALL_INTO_LISP)->value)
      (fun, args, nargs, get_sb_vm_thread());
}
