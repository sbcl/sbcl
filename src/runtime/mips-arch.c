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
#include "monitor.h"

#include "genesis/constants.h"

void
arch_init()
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
        return *(unsigned int *)(os_context_pc(context) + 4);
    else
        return *(unsigned int *)(os_context_pc(context));
}

/* This function is somewhat misnamed, it actually just jumps to the
   correct target address without attempting to execute the delay slot.
   For other instructions it just increments the returned PC value. */
static unsigned int
emulate_branch(os_context_t *context, unsigned int inst)
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
                = os_context_pc(context) + 4;
            break;
        default:
            tgt += 4;
            break;
        }
        break;
    case 0x1: /* bltz, bgez, bltzal, bgezal */
        switch((inst >> 16) & 0x1f) {
        case 0x00: /* bltz */
            if(os_context_register(context, r1) < 0)
                tgt += disp;
            break;
        case 0x01: /* bgez */
            if(os_context_register(context, r1) >= 0)
                tgt += disp;
            break;
        case 0x10: /* bltzal */
            if(os_context_register(context, r1) < 0)
                tgt += disp;
            *os_context_register_addr(context, 31)
                = os_context_pc(context) + 4;
            break;
        case 0x11: /* bgezal */
            if(os_context_register(context, r1) >= 0)
                tgt += disp;
            *os_context_register_addr(context, 31)
                = os_context_pc(context) + 4;
            break;
        default: /* conditional branches/traps for > MIPS I, ignore for now. */
            break;
        }
        break;
    case 0x4: /* beq */
        if(os_context_register(context, r1)
           == os_context_register(context, r2))
            tgt += disp;
        break;
    case 0x5: /* bne */
        if(os_context_register(context, r1)
           != os_context_register(context, r2))
            tgt += disp;
        break;
    case 0x6: /* blez */
        if(os_context_register(context, r1)
           <= os_context_register(context, r2))
            tgt += disp;
        break;
    case 0x7: /* bgtz */
        if(os_context_register(context, r1)
           > os_context_register(context, r2))
            tgt += disp;
        break;
    case 0x2: /* j */
        tgt = jtgt;
        break;
    case 0x3: /* jal */
        tgt = jtgt;
        *os_context_register_addr(context, 31)
            = os_context_pc(context) + 4;
        break;
    default:
        tgt += 4;
        break;
    }
    return tgt;
}

void
arch_skip_instruction(os_context_t *context)
{
    /* Skip the offending instruction.  Don't use os_context_insn here,
       since in case of a branch we want the branch insn, not the delay
       slot.  */
      *os_context_pc_addr(context)
          = emulate_branch(context,
              *(unsigned int *)(os_context_pc(context)));
}

unsigned char *
arch_internal_error_arguments(os_context_t *context)
{
    if (os_context_bd_cause(context))
        return (unsigned char *)(os_context_pc(context) + 8);
    else
        return (unsigned char *)(os_context_pc(context) + 4);
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

unsigned long
arch_install_breakpoint(void *pc)
{
    unsigned int *ptr = (unsigned int *)pc;
    unsigned long result;

    /* Don't install over a branch/jump.  */
    switch (*ptr >> 26) {
    case 0x0: /* immediate jumps */
        switch (*ptr & 0x3f) {
        case 0x08:
        case 0x09:
            ptr++;
        }
        break;
    /* branches and register jumps */
    case 0x1:
    case 0x2:
    case 0x3:
    case 0x4:
    case 0x5:
    case 0x6:
    case 0x7:
        ptr++;
    }

    result = (unsigned long) *ptr;
    *ptr = (trap_Breakpoint << 16) | 0xd;
    os_flush_icache((os_vm_address_t)ptr, sizeof(unsigned int));

    return result;
}

void
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    unsigned int *ptr = (unsigned int *)pc;

    *ptr = (unsigned int) orig_inst;
    os_flush_icache((os_vm_address_t)ptr, sizeof(unsigned int));
}

static unsigned int *skipped_break_addr, displaced_after_inst;
static sigset_t orig_sigmask;

void
arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned int *pc = (unsigned int *)os_context_pc(context);
    unsigned int *break_pc, *next_pc;
    unsigned int next_inst;

    orig_sigmask = *os_context_sigmask_addr(context);
    sigaddset_blockable(os_context_sigmask_addr(context));

    /* Figure out where the breakpoint is, and what happens next. */
    if (os_context_bd_cause(context)) {
        break_pc = pc+1;
        next_inst = *pc;
    } else {
        break_pc = pc;
        next_inst = orig_inst;
    }

    /* Put the original instruction back. */
    arch_remove_breakpoint(break_pc, orig_inst);
    skipped_break_addr = break_pc;

    /* Figure out where it goes. */
    next_pc = (unsigned int *)emulate_branch(context, next_inst);

    displaced_after_inst = arch_install_breakpoint(next_pc);
}

static void
sigill_handler(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);

    fake_foreign_function_call(context);
    monitor_or_something();
}

static void
sigtrap_handler(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    unsigned int code = (os_context_insn(context) >> 16) & 0x1f;

    switch (code) {
    case trap_Halt:
        fake_foreign_function_call(context);
        lose("%%primitive halt called; the party is over.\n");

    case trap_PendingInterrupt:
        arch_skip_instruction(context);
        interrupt_handle_pending(context);
        break;

    case trap_Error:
    case trap_Cerror:
        interrupt_internal_error(signal, info, context, code == trap_Cerror);
        break;

    case trap_Breakpoint:
        handle_breakpoint(signal, info, context);
        break;

    case trap_FunEndBreakpoint:
        *os_context_pc_addr(context)
            = (os_context_register_t)(unsigned int)
                handle_fun_end_breakpoint(signal, info, context);
        break;

    case trap_AfterBreakpoint:
        arch_remove_breakpoint(os_context_pc_addr(context), displaced_after_inst);
        displaced_after_inst = arch_install_breakpoint(skipped_break_addr);
        *os_context_sigmask_addr(context) = orig_sigmask;
        break;

    case 0x10:
        /* Clear the pseudo-atomic flag */
        *os_context_register_addr(context, reg_NL4) &= ~(-1LL<<31);
        arch_skip_instruction(context);
        interrupt_handle_pending(context);
        return;

    default:
        interrupt_handle_now(signal, info, context);
        break;
    }
}

#define FIXNUM_VALUE(lispobj) (((int)lispobj) >> N_FIXNUM_TAG_BITS)

static void
sigfpe_handler(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    unsigned int bad_inst = os_context_insn(context);
    unsigned int op, rs, rt, rd, funct, dest = 32;
    int immed;
    int result;

    op = (bad_inst >> 26) & 0x3f;
    rs = (bad_inst >> 21) & 0x1f;
    rt = (bad_inst >> 16) & 0x1f;
    rd = (bad_inst >> 11) & 0x1f;
    funct = bad_inst & 0x3f;
    immed = (((int)(bad_inst & 0xffff)) << 16) >> 16;

    switch (op) {
    case 0x0: /* SPECIAL */
        switch (funct) {
        case 0x20: /* ADD */
            result = FIXNUM_VALUE(os_context_register(context, rs))
                + FIXNUM_VALUE(os_context_register(context, rt));
            dest = rd;
            break;

        case 0x22: /* SUB */
            result = FIXNUM_VALUE(os_context_register(context, rs))
                - FIXNUM_VALUE(os_context_register(context, rt));
            dest = rd;
            break;

        default:
            interrupt_handle_now(signal, info, context);
            return;
        }
        break;

    case 0x8: /* ADDI */
        result = FIXNUM_VALUE(os_context_register(context,rs))
                    + (immed >> N_FIXNUM_TAG_BITS);
        dest = rt;
        break;

    default:
        interrupt_handle_now(signal, info, context);
        return;
    }

    dynamic_space_free_pointer =
        (lispobj *)(unsigned int)*os_context_register_addr(context,reg_ALLOC);

    *os_context_register_addr(context,dest) = alloc_number(result);

    *os_context_register_addr(context, reg_ALLOC) =
        (unsigned int) dynamic_space_free_pointer;

    arch_skip_instruction(context);
}

void
arch_install_interrupt_handlers()
{
    undoably_install_low_level_interrupt_handler(SIGILL,sigill_handler);
    undoably_install_low_level_interrupt_handler(SIGTRAP,sigtrap_handler);
    undoably_install_low_level_interrupt_handler(SIGFPE,sigfpe_handler);
}

extern lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs);

lispobj
funcall0(lispobj function)
{
    lispobj *args = current_control_stack_pointer;

    return call_into_lisp(function, args, 0);
}

lispobj
funcall1(lispobj function, lispobj arg0)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 1;
    args[0] = arg0;

    return call_into_lisp(function, args, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 2;
    args[0] = arg0;
    args[1] = arg1;

    return call_into_lisp(function, args, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 3;
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;

    return call_into_lisp(function, args, 3);
}
