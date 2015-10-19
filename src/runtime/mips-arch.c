/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

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

#include "genesis/constants.h"

#define INSN_LEN sizeof(unsigned int)

void
arch_init(void)
{
    return;
}

os_vm_address_t
arch_get_bad_addr(int signam, siginfo_t *siginfo, os_context_t *context)
{
    /* Classic CMUCL comment:

       Finding the bad address on the mips is easy. */
    return (os_vm_address_t)siginfo->si_addr;
}

static inline unsigned int
os_context_register(os_context_t *context, int offset)
{
    return (unsigned int)(*os_context_register_addr(context, offset));
}

static inline unsigned int
os_context_pc(os_context_t *context)
{
    return (unsigned int)(*os_context_pc_addr(context));
}

static inline unsigned int
os_context_insn(os_context_t *context)
{
    if (os_context_bd_cause(context))
        return *(unsigned int *)(os_context_pc(context) + INSN_LEN);
    else
        return *(unsigned int *)(os_context_pc(context));
}

boolean
arch_insn_with_bdelay_p(unsigned int insn)
{
    switch (insn >> 26) {
    case 0x0:
        switch (insn & 0x3f) {
        /* register jumps */
        case 0x08:
        case 0x09:
            return 1;
        }
        break;
    /* branches and immediate jumps */
    case 0x1:
        switch ((insn >> 16) & 0x1f) {
        case 0x00:
        case 0x01:
        case 0x02:
        case 0x03:
        case 0x10:
        case 0x11:
        case 0x12:
        case 0x13:
            return 1;
        }
        break;
    case 0x2:
    case 0x3:
    case 0x4:
    case 0x5:
    case 0x6:
    case 0x7:
        return 1;
    case 0x10:
    case 0x11:
    case 0x12:
        switch ((insn >> 21) & 0x1f) {
         /* CP0/CP1/CP2 branches */
        case 0x08:
            return 1;
        }
        break;
    /* branch likely (MIPS II) */
    case 0x14:
    case 0x15:
    case 0x16:
    case 0x17:
        return 1;
    }
    return 0;
}

/* Find the next instruction in the control flow. For a instruction
   with branch delay slot, this is the branch/jump target if the branch
   is taken, and PC + 8 if it is not taken. For other instructions it
   is PC + 4. */
static unsigned int
next_insn_addr(os_context_t *context, unsigned int inst)
{
    unsigned int opcode = inst >> 26;
    unsigned int r1 = (inst >> 21) & 0x1f;
    unsigned int r2 = (inst >> 16) & 0x1f;
    unsigned int r3 = (inst >> 11) & 0x1f;
    unsigned int disp = ((inst&(1<<15)) ? inst | (-1 << 16) : inst&0x7fff) << 2;
    unsigned int jtgt = (os_context_pc(context) & ~0x0fffffff) | (inst&0x3ffffff) << 2;
    unsigned int tgt = os_context_pc(context);

    switch(opcode) {
    case 0x0: /* jr, jalr */
        switch(inst & 0x3f) {
        case 0x08: /* jr */
            tgt = os_context_register(context, r1);
            break;
        case 0x09: /* jalr */
            tgt = os_context_register(context, r1);
            *os_context_register_addr(context, r3)
                = os_context_pc(context) + INSN_LEN;
            break;
        default:
            tgt += INSN_LEN;
            break;
        }
        break;
    case 0x1: /* bltz, bgez, bltzal, bgezal, ... */
        switch(r2) {
        case 0x00: /* bltz */
        case 0x02: /* bltzl */
            if(os_context_register(context, r1) < 0)
                tgt += disp;
            else
                tgt += INSN_LEN;
            break;
        case 0x01: /* bgez */
        case 0x03: /* bgezl */
            if(os_context_register(context, r1) >= 0)
                tgt += disp;
            else
                tgt += INSN_LEN;
            break;
        case 0x10: /* bltzal */
        case 0x12: /* bltzall */
            if(os_context_register(context, r1) < 0) {
                tgt += disp;
                *os_context_register_addr(context, 31)
                    = os_context_pc(context) + INSN_LEN;
            } else
                tgt += INSN_LEN;
            break;
        case 0x11: /* bgezal */
        case 0x13: /* bgezall */
            if(os_context_register(context, r1) >= 0) {
                tgt += disp;
                *os_context_register_addr(context, 31)
                    = os_context_pc(context) + INSN_LEN;
            } else
                tgt += INSN_LEN;
            break;
        default:
            tgt += INSN_LEN;
            break;
        }
        break;
    case 0x2: /* j */
        tgt = jtgt;
        break;
    case 0x3: /* jal */
        tgt = jtgt;
        *os_context_register_addr(context, 31)
            = os_context_pc(context) + INSN_LEN;
        break;
    case 0x4: /* beq */
    case 0x14: /* beql */
        if(os_context_register(context, r1)
           == os_context_register(context, r2))
            tgt += disp;
        else
            tgt += INSN_LEN;
        break;
    case 0x5: /* bne */
    case 0x15: /* bnel */
        if(os_context_register(context, r1)
           != os_context_register(context, r2))
            tgt += disp;
        else
            tgt += INSN_LEN;
        break;
    case 0x6: /* blez */
    case 0x16: /* blezl */
        if(os_context_register(context, r1)
           <= os_context_register(context, r2))
            tgt += disp;
        else
            tgt += INSN_LEN;
        break;
    case 0x7: /* bgtz */
    case 0x17: /* bgtzl */
        if(os_context_register(context, r1)
           > os_context_register(context, r2))
            tgt += disp;
        else
            tgt += INSN_LEN;
        break;
    case 0x10:
    case 0x11:
    case 0x12:
        switch (r1) {
         /* CP0/CP1/CP2 branches */
        case 0x08:
            /* FIXME */
            tgt += INSN_LEN;
            break;
        }
        break;
    default:
        tgt += INSN_LEN;
        break;
    }
    return tgt;
}

void
arch_skip_instruction(os_context_t *context)
{
    /* Skip the offending instruction. Don't use os_context_insn here,
       since in case of a branch we want the branch insn, not the delay
       slot. */
    *os_context_pc_addr(context)
        = (os_context_register_t)
            next_insn_addr(context,
                *(unsigned int *)(os_context_pc(context)));
}

unsigned char *
arch_internal_error_arguments(os_context_t *context)
{
    if (os_context_bd_cause(context))
        return (unsigned char *)(os_context_pc(context) + (INSN_LEN * 2));
    else
        return (unsigned char *)(os_context_pc(context) + INSN_LEN);
}

boolean
arch_pseudo_atomic_atomic(os_context_t *context)
{
    return os_context_register(context, reg_ALLOC) & 1;
}

void
arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    *os_context_register_addr(context, reg_NL4) |= -1LL<<31;
}

void
arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
    *os_context_register_addr(context, reg_NL4) &= ~(-1LL<<31);
}

unsigned int
arch_install_breakpoint(void *pc)
{
    unsigned int *ptr = (unsigned int *)pc;
    unsigned int insn;

    /* Don't install over a branch/jump with delay slot. */
    if (arch_insn_with_bdelay_p(*ptr))
        ptr++;

    insn = *ptr;
    *ptr = (trap_Breakpoint << 6) | 0xd;
    os_flush_icache((os_vm_address_t)ptr, INSN_LEN);

    return insn;
}

static inline unsigned int
arch_install_after_breakpoint(void *pc)
{
    unsigned int *ptr = (unsigned int *)pc;
    unsigned int insn;

    /* Don't install over a branch/jump with delay slot. */
    if (arch_insn_with_bdelay_p(*ptr))
        ptr++;

    insn = *ptr;
    *ptr = (trap_AfterBreakpoint << 6) | 0xd;
    os_flush_icache((os_vm_address_t)ptr, INSN_LEN);

    return insn;
}

void
arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    unsigned int *ptr = (unsigned int *)pc;

    /* We may remove from a branch delay slot. */
    if (arch_insn_with_bdelay_p(*ptr))
        ptr++;

    *ptr = orig_inst;
    os_flush_icache((os_vm_address_t)ptr, INSN_LEN);
}

/* Perform the instruction that we overwrote with a breakpoint. As we
   don't have a single-step facility, this means we have to:
   - put the instruction back
   - put a second breakpoint at the following instruction,
     set after_breakpoint and continue execution.

   When the second breakpoint is hit (very shortly thereafter, we hope)
   sigtrap_handler gets called again, but follows the AfterBreakpoint
   arm, which
   - puts a bpt back in the first breakpoint place (running across a
     breakpoint shouldn't cause it to be uninstalled)
   - replaces the second bpt with the instruction it was meant to be
   - carries on

   Clear? */

static unsigned int *skipped_break_addr, displaced_after_inst;
static sigset_t orig_sigmask;

void
arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned int *pc = (unsigned int *)os_context_pc(context);
    unsigned int *next_pc;

    orig_sigmask = *os_context_sigmask_addr(context);
    sigaddset_blockable(os_context_sigmask_addr(context));

    /* Put the original instruction back. */
    arch_remove_breakpoint(pc, orig_inst);
    skipped_break_addr = pc;

    /* Figure out where it goes. */
    next_pc = (unsigned int *)next_insn_addr(context, *pc);
    displaced_after_inst = arch_install_after_breakpoint(next_pc);
}

void
arch_handle_breakpoint(os_context_t *context)
{
    handle_breakpoint(context);
}

void
arch_handle_fun_end_breakpoint(os_context_t *context)
{
    *os_context_pc_addr(context)
        = (os_context_register_t)(unsigned int)
        handle_fun_end_breakpoint(context);
}

void
arch_handle_after_breakpoint(os_context_t *context)
{
    arch_install_breakpoint(skipped_break_addr);
    arch_remove_breakpoint((unsigned int *)os_context_pc(context),
                           displaced_after_inst);
    *os_context_sigmask_addr(context) = orig_sigmask;
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    unsigned int code = *((u32 *)(os_context_pc(context)));
    int register_offset = code >> 11 & 0x1f;
    handle_single_step_trap(context, trap, register_offset);
    arch_skip_instruction(context);
}

static void
sigtrap_handler(int signal, siginfo_t *info, os_context_t *context)
{
    unsigned int code = (os_context_insn(context) >> 6) & 0xfffff;
    /* FIXME: This magic number is pseudo-atomic-trap from parms.lisp.
     * Genesis should provide the proper #define, but it specialcases
     * pseudo-atomic-trap to work around some oddity on SPARC.
     * Eventually this should go into handle_trap. */
    if (code==0x10) {
        arch_clear_pseudo_atomic_interrupted(context);
        arch_skip_instruction(context);
        interrupt_handle_pending(context);
    } else
        handle_trap(context,code & 0x1f);
}

static void
sigfpe_handler(int signal, siginfo_t *info, os_context_t *context)
{
    interrupt_handle_now(signal, info, context);
}

unsigned int
arch_get_fp_control(void)
{
    register unsigned int ret asm("$2");

    __asm__ __volatile__ ("cfc1 %0, $31" : "=r" (ret));

    return ret;
}

void
arch_set_fp_control(unsigned int fp)
{
    __asm__ __volatile__ ("ctc1 %0, $31" :: "r" (fp));
}

void
arch_install_interrupt_handlers(void)
{
    undoably_install_low_level_interrupt_handler(SIGTRAP,sigtrap_handler);
}

#ifdef LISP_FEATURE_LINKAGE_TABLE

/* Linkage tables for MIPS

   Linkage entry size is 16, because we need 4 instructions to implement
   a jump. The entry size constant is defined in parms.lisp.

   Define the register to use in the linkage jump table. For MIPS this
   has to be the PIC call register $25 aka t9 aka reg_ALLOC. */
#define LINKAGE_TEMP_REG        reg_ALLOC

/* Insert the necessary jump instructions at the given address. */
void
arch_write_linkage_table_jmp(void* reloc_addr, void *target_addr)
{
  /* Make JMP to function entry. The instruction sequence is:
       lui    $25, 0, %hi(addr)
       addiu  $25, $25, %lo(addr)
       jr     $25
        nop */
  unsigned int *insn = (unsigned int *)reloc_addr;
  unsigned int addr = (unsigned int)target_addr;
  unsigned int hi = ((addr + 0x8000) >> 16) & 0xffff;
  unsigned int lo = addr & 0xffff;

  *insn++ = (15 << 26) | (LINKAGE_TEMP_REG << 16) | hi;
  *insn++ = ((9 << 26) | (LINKAGE_TEMP_REG << 21)
                 | (LINKAGE_TEMP_REG << 16) | lo);
  *insn++ = (LINKAGE_TEMP_REG << 21) | 8;
  *insn = 0;

  os_flush_icache((os_vm_address_t)reloc_addr, LINKAGE_TABLE_ENTRY_SIZE);
}

void
arch_write_linkage_table_ref(void *reloc_addr, void *target_addr)
{
    *(unsigned int *)reloc_addr = (unsigned int)target_addr;
}

#endif
