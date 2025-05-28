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

os_vm_address_t arch_get_bad_addr(int sig, siginfo_t *code, os_context_t *context)
{
    return (os_vm_address_t)code->si_addr;
}

void arch_skip_instruction(os_context_t *context)
{
    uint32_t trap_instruction = *(uint32_t *)OS_CONTEXT_PC(context);
    unsigned code = trap_instruction >> 5 & 0xFF;
    OS_CONTEXT_PC(context) += 4;
    switch (code)
    {
    case trap_Error:
    case trap_Cerror:
        skip_internal_error(context);
    default:
        break;
    }
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char *)OS_CONTEXT_PC(context);
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
    THREAD_JIT_WP(0);
    unsigned int *ptr = (unsigned int *)pc;
    unsigned int result = *ptr;

    *ptr = (0x6a1 << 21) | (trap_Breakpoint << 5);

    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));
    THREAD_JIT_WP(1);

    return result;
}

void arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    THREAD_JIT_WP(0);
    *(unsigned int *) pc = orig_inst;

    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));
    THREAD_JIT_WP(1);
}

#define N_BIT 31
#define Z_BIT 30
#define C_BIT 29
#define V_BIT 28

static bool
condition_holds(os_context_t *context, unsigned int cond)
{
    int flags = *os_context_flags_addr(context);
    bool result;
    // Evaluate base condition.
    switch (cond) {
    case 0b000: result = ((flags >> Z_BIT) & 1);
    case 0b001: result = ((flags >> C_BIT) & 1);
    case 0b010: result = ((flags >> N_BIT) & 1);
    case 0b011: result = ((flags >> V_BIT) & 1);
    case 0b100: result = ((flags >> V_BIT) & 1) && ~((flags >> Z_BIT) & 1);
    case 0b101: result = ((flags >> N_BIT) == (flags >> V_BIT));
    case 0b110: result = ((flags >> N_BIT) == (flags >> V_BIT)) && !((flags >> Z_BIT) & 1);
    case 0b111: result = 1;
    }

    // Condition flag values in the set '111x' indicate always true
    // Otherwise, invert condition if necessary.
    if ((cond & 0b1) && (cond != 0b1111))
        result = !result;

    return result;
}

static sword_t sign_extend(uword_t word, int n_bits) {
  return (sword_t)(word<<(N_WORD_BITS-n_bits)) >> (N_WORD_BITS-n_bits);
}

#define BREAKPOINT_TEMP_REG reg_NL9

/* In order to do the displaced instruction, we do the following:
 *
 * - For instructions which depend on the current PC, such as
 *   PC-relative loads or branch instructions, we try to simulate the
 *   instruction directly and continue execution normally.
 *
 * - Otherwise, we copy the instruction into a trampoline which
 *   branches back to the right place and continue execution at the
 *   trampoline.
 */
void arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned int *pc = (unsigned int *)OS_CONTEXT_PC(context);
    unsigned int *next_pc = pc;

    if ((orig_inst >> 24) == 0b01010100) {
        // Cond branch
        if (condition_holds(context, orig_inst & 0b1111))
            next_pc += sign_extend((orig_inst >> 5) & ~(1 << 19), 19);
        else
            next_pc += 1;
    }
    else if (((orig_inst >> 26) & 0b11111) == 0b000101)
        // Uncond branch: B, BL
        next_pc += sign_extend(orig_inst & ~(1 << 26), 26);
    else if (((orig_inst >> 25) & 0b1111111) == 0b1101011) {
        int rt;
        // Uncond branch register
        switch ((orig_inst >> 21) & 0b1111) {
        case 0b00: // BR
        case 0b01: // BLR
            rt = (orig_inst >> 5) & 0b11111;
            break;
        case 0b10: // RET
            rt = reg_LR;
            break;
        default:
            lose("Unrecognized instruction.");
            break;
        }
        next_pc = (unsigned int*)*os_context_register_addr(context, rt);
    }
    else if (((orig_inst >> 25) & 0b111111) == 0b011010) {
        // Compare branch imm
        bool size_is_64 = (orig_inst >> 31) & 0b1;
        bool op = (orig_inst >> 24) & 0b1;
        int offset = sign_extend((orig_inst >> 5) & ~(1 << 19), 19);
        int rt = orig_inst & 0b11111;
        if (!size_is_64) lose("Size must be 64 bits.");
        if (*os_context_register_addr(context, rt) ^ op)
            next_pc += offset;
        else
            next_pc += 1;
    }
    else if (((orig_inst >> 25) & 0b111111) == 0b011011) {
        // Test branch imm
        bool b5 = (orig_inst >> 31) & 0b1;
        bool op = (orig_inst >> 24) & 0b1;
        bool b40 = (orig_inst >> 19) & 0b11111;
        int bit_pos = (b5 << 6) | b40;
        int offset = sign_extend((orig_inst >> 5) & ~(1 << 14), 14);
        int rt = orig_inst & 0b11111;
        if (!b5) lose("b5 must be 64 bits.");
        if (((*os_context_register_addr(context, rt) >> bit_pos) & 0b1) ^ op)
            next_pc += offset;
        else
            next_pc += 1;
    }
    else if (((orig_inst >> 31) & 0b1) == 0b0) {
        // LDR (literal)
        bool size_is_64 = (orig_inst >> 30) & 0b1;
        int rt = orig_inst & 0b11111;
        int offset = sign_extend((orig_inst >> 5) & ~(1 << 19), 19);
        if (!size_is_64) lose("Size must be 64 bits.");
        *os_context_register_addr(context, rt) = *((lispobj*)(pc + offset));
        next_pc += 1;
    }
    else if (((orig_inst >> 24) & 0b11111) == 0b10000) {
        // ADR(P)
        bool op = (orig_inst >> 31) & 0b1;
        int rd = orig_inst & 0b11111;
        int imm = sign_extend(((orig_inst >> 5) & ~(1 << 19)) |
                              ((orig_inst >> 29) & ~(1 << 2)), 21);
        if (op) // ADRP
            *os_context_register_addr(context, rd) = ((uword_t)pc & ~(1 << 12)) + (imm << 12);
        else // ADR
            *os_context_register_addr(context, rd) = (uword_t)pc + imm;
        next_pc += 1;
    }
    else {
        // Do orig_inst by copying it into a trampoline.
        size_t size = getpagesize();
        // Allocate the thread-local trampoline on-demand.
        struct thread *th = get_sb_vm_thread();
        unsigned int *trampoline = (unsigned int*)th->breakpoint_misc;

        if (!trampoline) {
          trampoline = mmap(NULL, size,
                            PROT_READ | PROT_WRITE,
                            MAP_PRIVATE | MAP_ANONYMOUS,
                            -1, 0);
          th->breakpoint_misc = trampoline;
        }
        else
          os_protect((os_vm_address_t)trampoline, size, PROT_READ | PROT_WRITE);

        unsigned int *inst_ptr = trampoline;
        unsigned int inst;

        next_pc += 1;

        *inst_ptr++ = orig_inst;

        /*
          ldr reg,=address
          br  reg
          address
        */

        // ldr reg, =address
        inst = 0x58000000 | 2 << 5 | BREAKPOINT_TEMP_REG;
        *inst_ptr++ = inst;

        // br reg
        inst = 0xD61F0000 | BREAKPOINT_TEMP_REG << 5;
        *inst_ptr++ = inst;

        // address
        *(unsigned long *)inst_ptr++ = (unsigned long)next_pc;
        OS_CONTEXT_PC(context) = (uword_t)trampoline;
        os_flush_icache((os_vm_address_t) trampoline, (char*) inst_ptr - (char*)trampoline);
        os_protect((os_vm_address_t)trampoline, size, PROT_READ | PROT_EXEC);
        return;
    }

    // If we get here, the instruction has been simulated and we just continue execution at the next pc.
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
    OS_CONTEXT_PC(context) = (uword_t) handle_fun_end_breakpoint(context);
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    handle_single_step_trap(context, trap, reg_LEXENV);
    arch_skip_instruction(context);
}

static void
sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    uint32_t trap_instruction = *(uint32_t *)OS_CONTEXT_PC(context);
    unsigned code = trap_instruction >> 5 & 0xFF;
    if ((trap_instruction >> 21) != 0x6A1) {
        lose("Unrecognized trap instruction %08x in sigtrap_handler() (PC: %p)",
             trap_instruction, (void*)OS_CONTEXT_PC(context));
    }

    handle_trap(context, code);
}

void sigill_handler(int signal, siginfo_t *siginfo, os_context_t *context);

#ifndef LISP_FEATURE_DARWIN
void
sigill_handler(int signal, siginfo_t *siginfo, os_context_t *context) {
    fake_foreign_function_call(context);
    lose("Unhandled SIGILL at %p.", (void*)OS_CONTEXT_PC(context));
}
#endif

void arch_install_interrupt_handlers()
{
    ll_install_handler(SIGTRAP, sigtrap_handler);
    ll_install_handler(SIGILL, sigill_handler);
}


/* Linkage tables
 *
 * Linkage entry size is 16, because we need 2 instructions and an 8 byte address.
 */

#define LINKAGE_TEMP_REG reg_NL9

void arch_write_linkage_table_entry(int index, void *target_addr, int datap)
{
  THREAD_JIT_WP(0);
  char *reloc_addr = (char*)ALIEN_LINKAGE_SPACE_START + index * ALIEN_LINKAGE_TABLE_ENTRY_SIZE;

  if (datap) {
    *(unsigned long *)reloc_addr = (unsigned long)target_addr;
    goto DONE;
  }
  /*
    ldr reg,=address
    br  reg
    address
  */
  int* inst_ptr;
  unsigned inst;

  inst_ptr = (int*) reloc_addr;

  // ldr reg, =address
  inst = 0x58000000 | 2 << 5 | LINKAGE_TEMP_REG;
  *inst_ptr++ = inst;

  // br reg
  inst = 0xD61F0000 | LINKAGE_TEMP_REG << 5;
  *inst_ptr++ = inst;

  // address
  *(unsigned long *)inst_ptr++ = (unsigned long)target_addr;

  os_flush_icache((os_vm_address_t) reloc_addr, (char*) inst_ptr - reloc_addr);

 DONE:
  THREAD_JIT_WP(1);
}

void gcbarrier_patch_code(void* where, int nbits)
{
    // Patch in the 'imms' value for UBFM
    unsigned* pc = where;
    unsigned int mask = ~0x0000FC00; // 6 bits at position 10
    *pc = (*pc & mask) | ((GENCGC_CARD_SHIFT + nbits - 1) << 10);
}

os_vm_address_t coreparse_alloc_space(int space_id, int attr,
                                      os_vm_address_t addr, os_vm_size_t size)
{
    __attribute__((unused)) int extra_request = 0;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (space_id == IMMOBILE_TEXT_CORE_SPACE_ID) {
        extra_request = ALIEN_LINKAGE_SPACE_SIZE;
        size += extra_request;
        addr -= extra_request; // try to put text space start where expected
    }
#endif
    if (size == 0) return addr;

#ifdef LISP_FEATURE_SB_SAFEPOINT
    if (space_id == STATIC_CORE_SPACE_ID) {
        // Allocate space for the safepoint page.
        addr = os_alloc_gc_space(space_id, attr, addr - BACKEND_PAGE_BYTES, size + BACKEND_PAGE_BYTES) + BACKEND_PAGE_BYTES;
    }
    else
#endif
        addr = os_alloc_gc_space(space_id, attr, addr, size);

    if (!addr) lose("Can't allocate %#"OBJ_FMTX" bytes for space %d", size, space_id);

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (space_id == IMMOBILE_TEXT_CORE_SPACE_ID) {
      ALIEN_LINKAGE_SPACE_START = (uword_t)addr;
      addr += extra_request;
    }
#endif
    return addr;
}

long sb_udivmodti4(__uint128_t x, unsigned long y, unsigned long *rem) {
    if (rem)
        *rem = x % y;
    return x / y;
}
