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
    OS_CONTEXT_PC(context) = *os_context_npc_addr(context);
    /* Note that we're doing integer arithmetic here, not pointer. So
     * the value that the return value of os_context_npc_addr() points
     * to will be incremented by 4, not 16.
     */
    *os_context_npc_addr(context) += 4;
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char *)(OS_CONTEXT_PC(context) + 4);
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
    *ptr = trap_Breakpoint;

    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));

    return result;
}

void arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    *(unsigned int *)pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));
}

/*
 * Perform the instruction that we overwrote with a breakpoint.  As we
 * don't have a single-step facility, this means we have to:
 * - put the instruction back
 * - put a second breakpoint at the following instruction,
 *   set after_breakpoint and continue execution.
 *
 * When the second breakpoint is hit (very shortly thereafter, we hope)
 * sigtrap_handler gets called again, but follows the AfterBreakpoint
 * arm, which
 * - puts a bpt back in the first breakpoint place (running across a
 *   breakpoint shouldn't cause it to be uninstalled)
 * - replaces the second bpt with the instruction it was meant to be
 * - carries on
 *
 * Clear?
 */
static unsigned int *skipped_break_addr, displaced_after_inst;

void arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned int *pc = (unsigned int *)OS_CONTEXT_PC(context);
    unsigned int *npc = (unsigned int *)(*os_context_npc_addr(context));

  /*  orig_sigmask = context->sigmask;
      sigemptyset(&context->sigmask); */
  /* FIXME!!! */
  /* FILLBLOCKSET(&context->uc_sigmask);*/

    *pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned int));
    skipped_break_addr = pc;
    displaced_after_inst = *npc;
    *npc = trap_AfterBreakpoint;
    os_flush_icache((os_vm_address_t) npc, sizeof(unsigned int));

}

static int is_btst_reg_self(unsigned int inst)
{
    // Refer to EMIT-FORMAT-3-REG for the bit packing.
    int src1 = (inst >> 14) && 0x1f;
    int src2 = (inst >>  0) && 0x1f;
    /* Fix both src fields to the known value of 0x1f by ORing in a constant
     * and check whether it matches BTST %rN, %rn ("bit test")
     * which is the same as the ANDCC instruction.
     * The constant is #b1111100000000011111 = #x7C01F
     *                   -----         -----
     *                   src1          src2
     */
    return (inst | 0x7C01F) == 0x808FC01F && src1 == src2;
}

static int pseudo_atomic_trap_p(os_context_t *context)
{
    unsigned int* pc = (unsigned int*) OS_CONTEXT_PC(context);

    // As to the choice of bit patterns, see KLUDGE at
    // (defconstant pseudo-atomic-trap) in sparc/parms.lisp
    return pc[0] ==
#ifdef LISP_FEATURE_LINUX
        0x93D02040 // TNE 64. pseudo-atomic-trap = #x40
#else
        0x93D02010 // TNE 16. pseudo-atomic-trap = #x10
#endif
        && is_btst_reg_self(pc[-1]);
}

void
arch_handle_breakpoint(os_context_t *context)
{
    handle_breakpoint(context);
}

void
arch_handle_fun_end_breakpoint(os_context_t *context)
{
    OS_CONTEXT_PC(context) = (int) handle_fun_end_breakpoint(context);
    *os_context_npc_addr(context) = OS_CONTEXT_PC(context) + 4;
}

void
arch_handle_after_breakpoint(os_context_t *context)
{
    *skipped_break_addr = trap_Breakpoint;
    os_flush_icache((os_vm_address_t)skipped_break_addr, sizeof(unsigned int));
    skipped_break_addr = NULL;
    *(unsigned long *)OS_CONTEXT_PC(context) = displaced_after_inst;
    /* context->sigmask = orig_sigmask; */
    os_flush_icache((os_vm_address_t)OS_CONTEXT_PC(context), sizeof(unsigned int));
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    unsigned int code = *(uint32_t *)OS_CONTEXT_PC(context);
    int register_offset = code >> 8 & 0x1f;
    handle_single_step_trap(context, trap, register_offset);
    arch_skip_instruction(context);
}

static void handle_allocation_trap(os_context_t *context, unsigned int *pc)
{
    unsigned int or_inst;
    int rs1;
    int size;
    int immed;
    extern void mixed_region_rollback(sword_t);

    if (foreign_function_call_active)
      lose("Allocation trap inside foreign code.");

    struct thread* thread = get_sb_vm_thread();
    if (gencgc_alloc_profiler && thread->state_word.sprof_enable)
        record_backtrace_from_context(context, thread);

    or_inst = pc[-1];

    /*
     * The instruction before this trap instruction had better be an OR
     * instruction!
     */
    if (!(((or_inst >> 30) == 2) && (((or_inst >> 19) & 0x1f) == 2)))
        lose("Allocation trap @ %p not preceded by an OR instruction: 0x%08x",
             pc, or_inst);

    /*
     * An OR instruction.  RS1 is the register we want to allocate to.
     * RS2 (or an immediate) is the size.
     */
    int rd = (or_inst >> 25) & 0x1f; // just a 1 bit flag essentially
    rs1 = (or_inst >> 14) & 0x1f;
    immed = (or_inst >> 13) & 1;

    if (immed == 1)
        size = or_inst & 0x1fff;
    else
        size = *os_context_register_addr(context, or_inst & 0x1f);
    mixed_region_rollback(size);

    fake_foreign_function_call(context);

    /*
     * Allocate some memory, store the memory address in rs1.
     */
    lispobj* memory;
    struct interrupt_data *data = &thread_interrupt_data(thread);
    data->allocation_trap_context = context;
    extern lispobj *alloc(sword_t), *alloc_list(sword_t);
    memory = (rd & 1) ? alloc_list(size) : alloc(size);
    data->allocation_trap_context = 0;

    *os_context_register_addr(context, rs1) = (lispobj)memory;
    arch_skip_instruction(context);

    undo_fake_foreign_function_call(context);
}

static void sigill_handler(int signal, siginfo_t *siginfo,
                           os_context_t *context)
{
    if (siginfo->si_code == ILL_ILLOPC) {
        int trap;
        unsigned int inst;
        unsigned int* pc = (unsigned int*) siginfo->si_addr;

        if (!gc_managed_heap_space_p((lispobj)pc))
          lose("Illegal instruction not in lisp: %p [%x]\n", pc, *pc);

        inst = *pc;
        trap = inst & 0xff;
        if (trap == trap_Allocation) handle_allocation_trap(context, pc);
        else handle_trap(context,trap);
    }
    else if (siginfo->si_code == ILL_ILLTRP) {
        if (pseudo_atomic_trap_p(context)) {
            /* A trap instruction from a pseudo-atomic. */
            arch_clear_pseudo_atomic_interrupted(get_sb_vm_thread());
            arch_skip_instruction(context);
            interrupt_handle_pending(context);
        }
        else {
            interrupt_internal_error(context, 0);
        }
    }
    else {
        interrupt_handle_now(signal, siginfo, context);
    }
}

void arch_install_interrupt_handlers()
{
    ll_install_handler(SIGILL, sigill_handler);
}



/* This a naive port from CMUCL/sparc, which was mostly stolen from the
 * CMUCL/x86 version, with adjustments for sparc
 *
 * Linkage entry size is 16, because we need at least 3 instruction to
 * implement a jump:
 *
 *      sethi %hi(addr), %g4
 *      jmpl  [%g4 + %lo(addr)], %g5
 *      nop
 *
 * The Sparc V9 ABI seems to use 8 words for its jump tables.  Maybe
 * we should do the same?
 */

/*
 * Define the registers to use in the linkage jump table. Can be the
 * same. Some care must be exercised when choosing these. It has to be
 * a register that is not otherwise being used. reg_L0 is a good
 * choice. call_into_c trashes reg_L0 without preserving it, so we can
 * trash it in the linkage jump table.
 */
#define LINKAGE_TEMP_REG        reg_L0
#define LINKAGE_ADDR_REG        reg_L0

/*
 * Insert the necessary jump instructions at the given address.
 */
void
arch_write_linkage_table_entry(int index, void *target_addr, int datap)
{
  char *reloc_addr = (char*)ALIEN_LINKAGE_SPACE_START + index * ALIEN_LINKAGE_TABLE_ENTRY_SIZE;
  if (datap) {
    *(unsigned long *)reloc_addr = (unsigned long)target_addr;
    return;
  }
  /*
   * Make JMP to function entry.
   *
   * The instruction sequence is:
   *
   *        sethi %hi(addr), temp_reg
   *        jmp   %temp_reg + %lo(addr), %addr_reg
   *        nop
   *        nop
   *
   */

  // 'volatile' works around
  //   "warning: writing 4 bytes into a region of size 0 [-Wstringop-overflow=]"
  volatile int* inst_ptr;
  unsigned long hi;                   /* Top 22 bits of address */
  unsigned long lo;                   /* Low 10 bits of address */
  unsigned int inst;

  inst_ptr = (int*) reloc_addr;

  /*
   * Split the target address into hi and lo parts for the sethi
   * instruction.  hi is the top 22 bits.  lo is the low 10 bits.
   */
  hi = (unsigned long) target_addr;
  lo = hi & 0x3ff;
  hi >>= 10;

  /*
   * sethi %hi(addr), temp_reg
   */

  inst = (0 << 30) | (LINKAGE_TEMP_REG << 25) | (4 << 22) | hi;
  *inst_ptr++ = inst;

  /*
   * jmpl [temp_reg + %lo(addr)], addr_reg
   */

  inst = (2U << 30) | (LINKAGE_ADDR_REG << 25) | (0x38 << 19)
    | (LINKAGE_TEMP_REG << 14) | (1 << 13) | lo;
  *inst_ptr++ = inst;

  /* nop (really sethi 0, %g0) */

  inst = (0 << 30) | (0 << 25) | (4 << 22) | 0;

  *inst_ptr++ = inst;
  *inst_ptr++ = inst;

  os_flush_icache((os_vm_address_t) reloc_addr, (char*) inst_ptr - reloc_addr);
}
