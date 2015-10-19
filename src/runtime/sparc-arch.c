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

#ifdef LISP_FEATURE_LINUX
extern int linux_sparc_siginfo_bug;
#endif

void arch_init(void)
{
    return;
}

os_vm_address_t arch_get_bad_addr(int sig, siginfo_t *code, os_context_t *context)
{
#if 1 /* New way. */
    return (os_vm_address_t)code->si_addr;
#else /* Old way, almost certainly predates sigaction(2)-style handlers */
    unsigned int badinst;
    unsigned int *pc;
    int rs1;

    pc = (unsigned int *)(*os_context_pc_addr(context));

    /* On the sparc, we have to decode the instruction. */

    /* Make sure it's not the pc thats bogus, and that it was lisp code */
    /* that caused the fault. */
    if ((unsigned long) pc & 3) {
      /* Unaligned */
      return NULL;
    }
    if ((pc < READ_ONLY_SPACE_START ||
         pc >= READ_ONLY_SPACE_START+READ_ONLY_SPACE_SIZE) &&
        (pc < current_dynamic_space ||
         pc >= current_dynamic_space + dynamic_space_size)) {
      return NULL;
    }

    badinst = *pc;

    if ((badinst >> 30) != 3)
        /* All load/store instructions have op = 11 (binary) */
        return 0;

    rs1 = (badinst>>14)&0x1f;

    if (badinst & (1<<13)) {
        /* r[rs1] + simm(13) */
        int simm13 = badinst & 0x1fff;

        if (simm13 & (1<<12))
            simm13 |= -1<<13;

        return (os_vm_address_t)
            (*os_context_register_addr(context, rs1)+simm13);
    }
    else {
        /* r[rs1] + r[rs2] */
        int rs2 = badinst & 0x1f;

        return (os_vm_address_t)
            (*os_context_register_addr(context, rs1) +
             *os_context_register_addr(context, rs2));
    }
#endif
}

void arch_skip_instruction(os_context_t *context)
{
    *os_context_pc_addr(context) = *os_context_npc_addr(context);
    /* Note that we're doing integer arithmetic here, not pointer. So
     * the value that the return value of os_context_npc_addr() points
     * to will be incremented by 4, not 16.
     */
    *os_context_npc_addr(context) += 4;
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char *)(*os_context_pc_addr(context) + 4);
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
    return (!foreign_function_call_active)
        && ((*os_context_register_addr(context,reg_ALLOC)) & 4);
}

void arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    *os_context_register_addr(context,reg_ALLOC) |=  1;
}

void arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
    *os_context_register_addr(context,reg_ALLOC) &= ~1;
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
static sigset_t orig_sigmask;

void arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned int *pc = (unsigned int *)(*os_context_pc_addr(context));
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

static int pseudo_atomic_trap_p(os_context_t *context)
{
    unsigned int* pc;
    unsigned int badinst;
    int result;


    pc = (unsigned int*) *os_context_pc_addr(context);
    badinst = *pc;
    result = 0;

    /* Check to see if the current instruction is a pseudo-atomic-trap */
    if (((badinst >> 30) == 2) && (((badinst >> 19) & 0x3f) == 0x3a)
        && (((badinst >> 13) & 1) == 1) && ((badinst & 0x7f) == PSEUDO_ATOMIC_TRAP))
        {
            unsigned int previnst;
            previnst = pc[-1];
            /*
             * Check to see if the previous instruction was an andcc alloc-tn,
             * 3, zero-tn instruction.
             */
            if (((previnst >> 30) == 2) && (((previnst >> 19) & 0x3f) == 0x11)
                && (((previnst >> 14) & 0x1f) == reg_ALLOC)
                && (((previnst >> 25) & 0x1f) == reg_ZERO)
                && (((previnst >> 13) & 1) == 1)
                && ((previnst & 0x1fff) == 3))
                {
                    result = 1;
                }
            else
                {
                    fprintf(stderr, "Oops!  Got a PSEUDO-ATOMIC-TRAP without a preceeding andcc!\n");
                }
        }
    return result;
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
    *os_context_npc_addr(context) = *os_context_pc_addr(context) + 4;
}

void
arch_handle_after_breakpoint(os_context_t *context)
{
    *skipped_break_addr = trap_Breakpoint;
    os_flush_icache(skipped_break_addr, sizeof(unsigned int));
    skipped_break_addr = NULL;
    *(unsigned long *) os_context_pc_addr(context) = displaced_after_inst;
    /* context->sigmask = orig_sigmask; */
    os_flush_icache((os_vm_address_t) os_context_pc_addr(context), sizeof(unsigned int));
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    unsigned int code = *((u32 *)(*os_context_pc_addr(context)));
    int register_offset = code >> 5 & 0x1f;
    handle_single_step_trap(context, trap, register_offset);
    arch_skip_instruction(context);
}

#ifdef LISP_FEATURE_GENCGC
void
arch_handle_allocation_trap(os_context_t *context)
{
    unsigned int* pc;
    unsigned int or_inst;
    int rs1;
    int size;
    int immed;
    int context_index;
    boolean were_in_lisp;
    char* memory;

    if (foreign_function_call_active)
      lose("Allocation trap inside foreign code.");

    pc = (unsigned int*) *os_context_pc_addr(context);
    or_inst = pc[-1];

    /*
     * The instruction before this trap instruction had better be an OR
     * instruction!
     */
    if (!(((or_inst >> 30) == 2) && (((or_inst >> 19) & 0x1f) == 2)))
        lose("Allocation trap not preceded by an OR instruction: 0x%08x",
             or_inst);

    /*
     * An OR instruction.  RS1 is the register we want to allocate to.
     * RS2 (or an immediate) is the size.
     */
    rs1 = (or_inst >> 14) & 0x1f;
    immed = (or_inst >> 13) & 1;

    if (immed == 1)
        size = or_inst & 0x1fff;
    else {
        size = or_inst & 0x1f;
        size = *os_context_register_addr(context, size);
    }

    fake_foreign_function_call(context);

    /*
     * Allocate some memory, store the memory address in rs1.
     */
    {
        struct interrupt_data *data =
            arch_os_get_current_thread()->interrupt_data;
        data->allocation_trap_context = context;
        memory = alloc(size);
        data->allocation_trap_context = 0;
    }
    *os_context_register_addr(context, rs1) = memory;

    undo_fake_foreign_function_call(context);
}
#endif

static void sigill_handler(int signal, siginfo_t *siginfo,
                           os_context_t *context)
{
    if ((siginfo->si_code) == ILL_ILLOPC
#ifdef LISP_FEATURE_LINUX
        || (linux_sparc_siginfo_bug && (siginfo->si_code == 2))
#endif
        ) {
        int trap;
        unsigned int inst;
        unsigned int* pc = (unsigned int*) siginfo->si_addr;

        inst = *pc;
        trap = inst & 0x1f;
        handle_trap(context,trap);
    }
    else if ((siginfo->si_code) == ILL_ILLTRP
#ifdef LISP_FEATURE_LINUX
             || (linux_sparc_siginfo_bug && (siginfo->si_code) == 192)
#endif
             ) {
        if (pseudo_atomic_trap_p(context)) {
            /* A trap instruction from a pseudo-atomic.  We just need
               to fixup up alloc-tn to remove the interrupted flag,
               skip over the trap instruction, and then handle the
               pending interrupt(s). */
            arch_clear_pseudo_atomic_interrupted(context);
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
    undoably_install_low_level_interrupt_handler(SIGILL, sigill_handler);
}


#ifdef LISP_FEATURE_LINKAGE_TABLE

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
arch_write_linkage_table_jmp(void* reloc_addr, void *target_addr)
{
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
  int* inst_ptr;
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

  os_flush_icache((os_vm_address_t) reloc_addr, (char*) inst_ptr - (char*) reloc_addr);
}

void
arch_write_linkage_table_ref(void * reloc_addr, void *target_addr)
{
    *(unsigned long *)reloc_addr = (unsigned long)target_addr;
}

#endif
