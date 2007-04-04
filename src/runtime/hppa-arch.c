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

/* Copied from sparc-arch.c.  Not all of these are necessary, probably */
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

os_vm_address_t arch_get_bad_addr(int signal, siginfo_t *siginfo, os_context_t *context)
{
    return siginfo->si_addr;
#if 0
#ifdef hpux
    struct save_state *state;
    os_vm_address_t addr;

    state = (struct save_state *)(&(scp->sc_sl.sl_ss));

    if (state == NULL)
        return NULL;

    /* Check the instruction address first. */
    addr = (os_vm_address_t)((unsigned long)scp->sc_pcoq_head & ~3);
    if (addr < (os_vm_address_t)0x1000)
        return addr;

    /* Otherwise, it must have been a data fault. */
    return (os_vm_address_t)state->ss_cr21;
#else
    struct hp800_thread_state *state;
    os_vm_address_t addr;

    state = (struct hp800_thread_state *)(scp->sc_ap);

    if (state == NULL)
        return NULL;

    /* Check the instruction address first. */
    addr = scp->sc_pcoqh & ~3;
    if (addr < 0x1000)
        return addr;

    /* Otherwise, it must have been a data fault. */
    return state->cr21;
#endif
#endif
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char *)((*os_context_pc_addr(context) & ~3) + 4);
}

boolean arch_pseudo_atomic_atomic(os_context_t *context)
{
    return ((*os_context_register_addr(context,reg_ALLOC)) & 4);
}

void arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    *os_context_register_addr(context,reg_ALLOC) |=  1;
}

/* FIXME: untested */
void arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
    *os_context_register_addr(context,reg_ALLOC) &= ~1;
}

void arch_skip_instruction(os_context_t *context)
{
    ((char *) *os_context_pc_addr(context)) = ((char *) *os_context_npc_addr(context));
    ((char *) *os_context_npc_addr(context)) += 4;
}

unsigned int arch_install_breakpoint(void *pc)
{
    unsigned int *ulpc = (unsigned int *)pc;
    unsigned int orig_inst = *ulpc;

    *ulpc = trap_Breakpoint;
    os_flush_icache((os_vm_address_t)pc, sizeof(*ulpc));
    return orig_inst;
}

void arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    unsigned int *ulpc = (unsigned int *)pc;

    *ulpc = orig_inst;
    os_flush_icache((os_vm_address_t)pc, sizeof(*ulpc));
}

void arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    /* FIXME: Fill this in */
#if 0
#ifdef hpux
    /* We change the next-pc to point to a breakpoint instruction, restore */
    /* the original instruction, and exit.  We would like to be able to */
    /* sigreturn, but we can't, because this is hpux. */
    unsigned int *pc = (unsigned int *)(SC_PC(scp) & ~3);

    NextPc = SC_NPC(scp);
    SC_NPC(scp) = (unsigned int)SingleStepTraps | (SC_NPC(scp)&3);

    BreakpointAddr = pc;
    *pc = orig_inst;
    os_flush_icache((os_vm_address_t)pc, sizeof(unsigned int));
#else
    /* We set the recovery counter to cover one instruction, put the */
    /* original instruction back in, and then resume.  We will then trap */
    /* after executing that one instruction, at which time we can put */
    /* the breakpoint back in. */

    ((struct hp800_thread_state *)scp->sc_ap)->cr0 = 1;
    scp->sc_ps |= 0x10;
    *(unsigned int *)SC_PC(scp) = orig_inst;

    sigreturn(scp);
#endif
#endif
}

#ifdef hpux
static void restore_breakpoint(struct sigcontext *scp)
{
    /* We just single-stepped over an instruction that we want to replace */
    /* with a breakpoint.  So we put the breakpoint back in, and tweek the */
    /* state so that we will continue as if nothing happened. */

    if (NextPc == NULL)
        lose("SingleStepBreakpoint trap at strange time.\n");

    if ((SC_PC(scp)&~3) == (unsigned int)SingleStepTraps) {
        /* The next instruction was not nullified. */
        SC_PC(scp) = NextPc;
        if ((SC_NPC(scp)&~3) == (unsigned int)SingleStepTraps + 4) {
            /* The instruction we just stepped over was not a branch, so */
            /* we need to fix it up.  If it was a branch, it will point to */
            /* the correct place. */
            SC_NPC(scp) = NextPc + 4;
        }
    }
    else {
        /* The next instruction was nullified, so we want to skip it. */
        SC_PC(scp) = NextPc + 4;
        SC_NPC(scp) = NextPc + 8;
    }
    NextPc = NULL;

    if (BreakpointAddr) {
        *BreakpointAddr = trap_Breakpoint;
        os_flush_icache((os_vm_address_t)BreakpointAddr,
                        sizeof(unsigned int));
        BreakpointAddr = NULL;
    }
}
#endif

void
arch_handle_breakpoint(os_context_t *context)
{
    /*sigsetmask(scp->sc_mask); */
    handle_breakpoint(context);
}

void
arch_handle_fun_end_breakpoint(os_context_t *context)
{
    /*sigsetmask(scp->sc_mask); */
    unsigned long pc;
    pc = (unsigned long)
        handle_fun_end_breakpoint(context);
    *os_context_pc_addr(context) = pc;
    *os_context_npc_addr(context) = pc + 4;
}

static void
sigtrap_handler(int signal, siginfo_t *siginfo, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    unsigned int bad_inst;

#if 0
    printf("sigtrap_handler, pc=0x%08x, alloc=0x%08x\n", scp->sc_pcoqh,
           SC_REG(scp,reg_ALLOC));
#endif

    bad_inst = *(unsigned int *)(*os_context_pc_addr(context) & ~3);
    if (bad_inst & 0xfc001fe0)
        interrupt_handle_now(signal, siginfo, context);
    else {
        int im5 = bad_inst & 0x1f;
        handle_trap(context, trap);
    }
}

static void sigfpe_handler(int signal, siginfo_t *siginfo, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    unsigned int badinst;
    int opcode, r1, r2, t;
    long op1, op2, res;

#if 0
    printf("sigfpe_handler, pc=0x%08x, alloc=0x%08x\n", scp->sc_pcoqh,
           SC_REG(scp,reg_ALLOC));
#endif

    switch (siginfo->si_code) {
    case FPE_INTOVF: /*I_OVFLO: */
        badinst = *(unsigned int *)(*os_context_pc_addr(context) & ~3);
        opcode = badinst >> 26;

        if (opcode == 2) {
            /* reg/reg inst. */
            r1 = (badinst >> 16) & 0x1f;
            op1 = fixnum_value(*os_context_register_addr(context, r1));
            r2 = (badinst >> 21) & 0x1f;
            op2 = fixnum_value(*os_context_register_addr(context, r2));
            t = badinst & 0x1f;

            switch ((badinst >> 5) & 0x7f) {
            case 0x70:
                /* Add and trap on overflow. */
                res = op1 + op2;
                break;

            case 0x60:
                /* Subtract and trap on overflow. */
                res = op1 - op2;
                break;

            default:
                goto not_interesting;
            }
        }
        else if ((opcode & 0x37) == 0x25 && (badinst & (1<<11))) {
            /* Add or subtract immediate. */
            op1 = ((badinst >> 3) & 0xff) | ((-badinst&1)<<8);
            r2 = (badinst >> 16) & 0x1f;
            op2 = fixnum_value(*os_context_register_addr(context, r1));
            t = (badinst >> 21) & 0x1f;
            if (opcode == 0x2d)
                res = op1 + op2;
            else
                res = op1 - op2;
        }
        else
            goto not_interesting;

        /* ?? What happens here if we hit the end of dynamic space? */
        dynamic_space_free_pointer = (lispobj *) *os_context_register_addr(context, reg_ALLOC);
        *os_context_register_addr(context, t) = alloc_number(res);
        *os_context_register_addr(context, reg_ALLOC)
            = (unsigned long) dynamic_space_free_pointer;
        arch_skip_instruction(context);

        break;

    case 0: /* I_COND: ?? Maybe tagged add?? FIXME */
        badinst = *(unsigned int *)(*os_context_pc_addr(context) & ~3);
        if ((badinst&0xfffff800) == (0xb000e000|reg_ALLOC<<21|reg_ALLOC<<16)) {
            /* It is an ADDIT,OD i,ALLOC,ALLOC instruction that trapped. */
            /* That means that it is the end of a pseudo-atomic.  So do the */
            /* add stripping off the pseudo-atomic-interrupted bit, and then */
            /* tell the machine-independent code to process the pseudo- */
            /* atomic. */
            int immed = (badinst>>1)&0x3ff;
            if (badinst & 1)
                immed |= -1<<10;
            *os_context_register_addr(context, reg_ALLOC) += (immed-1);
            arch_skip_instruction(context);
            interrupt_handle_pending(context);
            break;
        }
        /* else drop-through. */
    default:
    not_interesting:
        interrupt_handle_now(signal, siginfo, context);
    }
}

/* Merrily cut'n'pasted from sigfpe_handler.  On Linux, until
   2.4.19-pa4 (hopefully), the overflow_trap wasn't implemented,
   resulting in a SIGBUS instead. We adapt the sigfpe_handler here, in
   the hope that it will do as a replacement until the new kernel sees
   the light of day. Since the instructions that we need to fix up
   tend not to be doing unaligned memory access, this should be a safe
   workaround.  -- CSR, 2002-08-17 */
static void sigbus_handler(int signal, siginfo_t *siginfo, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    unsigned int badinst;
    int opcode, r1, r2, t;
    long op1, op2, res;

    badinst = *(unsigned int *)(*os_context_pc_addr(context) & ~3);
    /* First, test for the pseudo-atomic instruction */
    if ((badinst & 0xfffff800) == (0xb000e000 |
                                   reg_ALLOC<<21 |
                                   reg_ALLOC<<16)) {
        /* It is an ADDIT,OD i,ALLOC,ALLOC instruction that trapped.
           That means that it is the end of a pseudo-atomic.  So do
           the add stripping off the pseudo-atomic-interrupted bit,
           and then tell the machine-independent code to process the
           pseudo-atomic. */
        int immed = (badinst>>1) & 0x3ff;
        if (badinst & 1)
            immed |= -1<<10;
        *os_context_register_addr(context, reg_ALLOC) += (immed-1);
        arch_skip_instruction(context);
        interrupt_handle_pending(context);
        return;
    } else {
        opcode = badinst >> 26;
        if (opcode == 2) {
            /* reg/reg inst. */
            r1 = (badinst >> 16) & 0x1f;
            op1 = fixnum_value(*os_context_register_addr(context, r1));
            r2 = (badinst >> 21) & 0x1f;
            op2 = fixnum_value(*os_context_register_addr(context, r2));
            t = badinst & 0x1f;

            switch ((badinst >> 5) & 0x7f) {
            case 0x70:
                /* Add and trap on overflow. */
                res = op1 + op2;
                break;

            case 0x60:
                /* Subtract and trap on overflow. */
                res = op1 - op2;
                break;

            default:
                goto not_interesting;
            }
        } else if ((opcode & 0x37) == 0x25 && (badinst & (1<<11))) {
            /* Add or subtract immediate. */
            op1 = ((badinst >> 3) & 0xff) | ((-badinst&1)<<8);
            r2 = (badinst >> 16) & 0x1f;
            op2 = fixnum_value(*os_context_register_addr(context, r1));
            t = (badinst >> 21) & 0x1f;
            if (opcode == 0x2d)
                res = op1 + op2;
            else
                res = op1 - op2;
        }
        else
            goto not_interesting;

        /* ?? What happens here if we hit the end of dynamic space? */
        dynamic_space_free_pointer = (lispobj *) *os_context_register_addr(context, reg_ALLOC);
        *os_context_register_addr(context, t) = alloc_number(res);
        *os_context_register_addr(context, reg_ALLOC)
            = (unsigned long) dynamic_space_free_pointer;
        arch_skip_instruction(context);

        return;

    not_interesting:
        interrupt_handle_now(signal, siginfo, context);
    }
}


void arch_install_interrupt_handlers(void)
{
    undoably_install_low_level_interrupt_handler(SIGTRAP,sigtrap_handler);
    undoably_install_low_level_interrupt_handler(SIGFPE,sigfpe_handler);
    /* FIXME: beyond 2.4.19-pa4 this shouldn't be necessary. */
    undoably_install_low_level_interrupt_handler(SIGBUS,sigbus_handler);
}
