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

static inline unsigned int
os_context_pc(os_context_t *context)
{
    return (unsigned int)(*os_context_pc_addr(context));
}

os_vm_address_t arch_get_bad_addr(int signal, siginfo_t *siginfo, os_context_t *context)
{
    return (os_vm_address_t)siginfo->si_addr;
#if 0
#ifdef LISP_FEATURE_HPUX
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

#if defined(LISP_FEATURE_HPUX)
      // FIX-lav: use accessor macro instead
      return (!foreign_function_call_active) &&
             *(&((ucontext_t *) context)->uc_mcontext.ss_wide.ss_64.ss_gr7) & 4;
#else
      return (!foreign_function_call_active) &&
          ((*os_context_register_addr(context,reg_ALLOC)) & 4);
#endif
}

void arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
#if defined(LISP_FEATURE_HPUX)
    *(&((ucontext_t *) context)->uc_mcontext.ss_wide.ss_64.ss_gr7) |= 1;
/* on hpux do we need to watch out for the barbarian ? */
    *((os_context_register_t *) &((ucontext_t *) context)->uc_mcontext.ss_flags)
     |= SS_MODIFIEDWIDE;
#else
    *os_context_register_addr(context,reg_ALLOC) |= 1;
#endif
}

/* FIXME: untested */
void arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
#if defined(LISP_FEATURE_HPUX)
    *(&((ucontext_t *) context)->uc_mcontext.ss_wide.ss_64.ss_gr7) &= ~1;
    *((os_context_register_t *) &((ucontext_t *) context)->uc_mcontext.ss_flags)
     |= SS_MODIFIEDWIDE;
#else
    *os_context_register_addr(context,reg_ALLOC) &= ~1;
#endif
}

void arch_skip_instruction(os_context_t *context)
{
    *((unsigned int *) os_context_pc_addr(context)) = *((unsigned int *) os_context_npc_addr(context));
    *((unsigned int *) os_context_npc_addr(context)) += 4;
#ifdef LISP_FEATURE_HPUX
    *((os_context_register_t *) &((ucontext_t *) context)->uc_mcontext.ss_flags)
     |= SS_MODIFIEDWIDE;
#endif
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
  fprintf(stderr, "arch_do_displaced_inst() WARNING: stub.\n");
    /* FIXME: Fill this in */
#if 0
#ifdef LISP_FEATURE_HPUX
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

#ifdef LISP_FEATURE_HPUX
#if 0
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
#ifdef LISP_FEATURE_HPUX
    *((os_context_register_t *) &((ucontext_t *) context)->uc_mcontext.ss_flags)
     |= SS_MODIFIEDWIDE;
#endif
}


//FIX-lav: this whole is copied from mips
void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    unsigned int code = *((u32 *)(os_context_pc(context)));
    int register_offset = code >> 11 & 0x1f;
    handle_single_step_trap(context, trap, register_offset);
    arch_skip_instruction(context);
}

static void
sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    unsigned int bad_inst;

    bad_inst = *(unsigned int *)(*os_context_pc_addr(context) & ~3);
    if (bad_inst & 0xfc001fe0)
        interrupt_handle_now(signal, siginfo, context);
    else {
        int im5 = bad_inst & 0x1f;
        handle_trap(context, im5);
    }
}

static void
sigill_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
  unsigned int bad_inst;

  bad_inst = *(unsigned int *)(*os_context_pc_addr(context) & ~3);
  if (bad_inst == 9) { /* pending-interrupt */
    arch_clear_pseudo_atomic_interrupted(context);
    arch_skip_instruction(context);
    interrupt_handle_pending(context);
  } else {
    handle_trap(context,bad_inst);
  }
}

static void sigfpe_handler(int signal, siginfo_t *siginfo,
                           os_context_t *context)
{
    unsigned badinst = *(unsigned *)(*os_context_pc_addr(context) & ~3);

#ifdef LISP_FEATURE_LINUX
    if (!siginfo->si_code && /* Linux 3.14 seems to set si_code to 0 for this case */
#else
    if (siginfo->si_code == FPE_COND &&
#endif
        (badinst&0xfffff800) == (0xb000e000|reg_ALLOC<<21|reg_ALLOC<<16)) {
        /* It is an ADDIT,OD i,ALLOC,ALLOC instruction that trapped.
         * That means that it is the end of a pseudo-atomic.  So do the
         * add stripping off the pseudo-atomic-interrupted bit, and then
         * tell the machine-independent code to process the pseudo-
         * atomic. We cant skip the instruction because it holds
         * extra-bytes that we must add to reg_alloc in context.
         * It is so because we optimized away 'addi ,extra-bytes reg_alloc'
         */
        int immed = (badinst>>1)&0x3ff;
        if (badinst & 1)
            immed |= -1<<10;
        *os_context_register_addr(context, reg_ALLOC) += (immed-1);
        arch_skip_instruction(context);
        interrupt_handle_pending(context);
    } else {
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
static void sigbus_handler(int signal, siginfo_t *siginfo,
                           os_context_t *context)
{

    unsigned badinst = *(unsigned *)(*os_context_pc_addr(context) & ~3);

    /* First, test for the pseudo-atomic instruction */
    if ((badinst & 0xfffff800) == (0xb000e000 | reg_ALLOC<<21 |
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
    } else {
        interrupt_handle_now(signal, siginfo, context);
    }
}

#ifdef LISP_FEATURE_HPUX
static void
ignore_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
}
#endif

/* this routine installs interrupt handlers that will
 * bypass the lisp interrupt handlers */
void arch_install_interrupt_handlers(void)
{
    undoably_install_low_level_interrupt_handler(SIGTRAP,sigtrap_handler);
    undoably_install_low_level_interrupt_handler(SIGILL,sigill_handler);
    undoably_install_low_level_interrupt_handler(SIGFPE,sigfpe_handler);
    /* FIXME: beyond 2.4.19-pa4 this shouldn't be necessary. */
    undoably_install_low_level_interrupt_handler(SIGBUS,sigbus_handler);
#ifdef LISP_FEATURE_HPUX
    undoably_install_low_level_interrupt_handler(SIGXCPU,ignore_handler);
    undoably_install_low_level_interrupt_handler(SIGXFSZ,ignore_handler);
#endif
}
