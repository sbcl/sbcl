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

/* Note that although superficially it appears that we use
 * os_context_t like we ought to, we actually just assume its a
 * ucontext in places.  Naughty */

#include <stdio.h>
#include <string.h>

#include "sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"
#include "alloc.h"
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"

extern char call_into_lisp_LRA[], call_into_lisp_end[];

extern size_t os_vm_page_size;
#define BREAKPOINT_INST 0x80


void
arch_init(void)
{
    /* This must be called _after_ os_init(), so that we know what the
     * page size is. */

    if (mmap((os_vm_address_t) call_into_lisp_LRA_page,os_vm_page_size,
             OS_VM_PROT_ALL,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED,-1,0)
        == (os_vm_address_t) -1)
        perror("mmap");

    /* call_into_lisp_LRA is a collection of trampolines written in asm -
     * see alpha-assem.S.  We copy it to call_into_lisp_LRA_page where
     * VOPs and things can find it. (I don't know why they can't find it
     * where it was to start with.) */
    bcopy(call_into_lisp_LRA,(void *)call_into_lisp_LRA_page,os_vm_page_size);

    os_flush_icache((os_vm_address_t)call_into_lisp_LRA_page,
                    os_vm_page_size);
    return;
}

os_vm_address_t
arch_get_bad_addr (int sig, siginfo_t *code, os_context_t *context)
{
    unsigned int badinst;

    /* Instructions are 32 bit quantities. */
    unsigned int *pc ;
    /*  fprintf(stderr,"arch_get_bad_addr %d %p %p\n",
        sig, code, context); */
    pc= (unsigned int *)(*os_context_pc_addr(context));

    if (((unsigned long)pc) & 3) {
        return NULL;            /* In what case would pc be unaligned?? */
    }

    if ( (pc < READ_ONLY_SPACE_START ||
          pc >= READ_ONLY_SPACE_START+READ_ONLY_SPACE_SIZE) &&
         (pc < current_dynamic_space ||
          pc >= current_dynamic_space + dynamic_space_size))
        return NULL;

    return context->uc_mcontext.sc_traparg_a0;
}

void
arch_skip_instruction(os_context_t *context)
{
    /* This may be complete rubbish, as (at least for traps) pc points
     * _after_ the instruction that caused us to be here anyway.
     */
    char **pcptr;
    pcptr = (char **) os_context_pc_addr(context);
    *pcptr += 4;
}

unsigned char *
arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char *)(*os_context_pc_addr(context)+4);
}

boolean
arch_pseudo_atomic_atomic(os_context_t *context)
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
        && ((*os_context_register_addr(context,reg_ALLOC)) & 1);
}

void arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    /* On coming out of an atomic section, we subtract 1 from
     * reg_Alloc, then try to store something at that address.  So,
     * to signal that it was interrupted and a signal should be handled,
     * we set bit 63 of reg_ALLOC here so that the end-of-atomic code
     * will raise SIGSEGV (no ram mapped there).  We catch the signal
     * (see the appropriate *-os.c) and call interrupt_handle_pending()
     * for the saved signal instead */

    *os_context_register_addr(context,reg_ALLOC) |=  (1L<<63);
}

void arch_clear_pseudo_atomic_interrupted(os_context_t *context)
{
    *os_context_register_addr(context, reg_ALLOC) &= ~(1L<<63);
}

unsigned int arch_install_breakpoint(void *pc)
{
    unsigned int *ptr = (unsigned int *)pc;
    unsigned int result = *ptr;
    *ptr = BREAKPOINT_INST;

    os_flush_icache((os_vm_address_t)ptr, sizeof(unsigned int));

    return result;
}

void arch_remove_breakpoint(void *pc, unsigned int orig_inst)
{
    unsigned int *ptr = (unsigned int *)pc;
    *ptr = orig_inst;
    os_flush_icache((os_vm_address_t)pc, sizeof(unsigned int));
}

static unsigned int *skipped_break_addr, displaced_after_inst,
     after_breakpoint;


/* This returns a PC value.  Lisp code is all in the 32-bit-addressable
 * space, so we should be ok with an unsigned int. */
unsigned int
emulate_branch(os_context_t *context, unsigned int orig_inst)
{
    int op = orig_inst >> 26;
    int reg_a = (orig_inst >> 21) & 0x1f;
    int reg_b = (orig_inst >> 16) & 0x1f;
    int disp =
        (orig_inst&(1<<20)) ?
        orig_inst | (-1 << 21) :
        orig_inst&0x1fffff;
    int next_pc = *os_context_pc_addr(context);
    int branch = 0; /* was NULL;               */

    switch(op) {
    case 0x1a: /* jmp, jsr, jsr_coroutine, ret */
        *os_context_register_addr(context,reg_a) =
            *os_context_pc_addr(context);
        *os_context_pc_addr(context) =
            *os_context_register_addr(context,reg_b)& ~3;
        break;
    case 0x30: /* br */
        *os_context_register_addr(context,reg_a)=*os_context_pc_addr(context);
        branch = 1;
        break;
    case 0x31: /* fbeq */
        if (*(os_context_float_register_addr(context,reg_a))==0) branch = 1;
        break;
    case 0x32: /* fblt */
        if (*os_context_float_register_addr(context,reg_a)<0) branch = 1;
        break;
    case 0x33: /* fble */
        if (*os_context_float_register_addr(context,reg_a)<=0) branch = 1;
        break;
    case 0x34: /* bsr */
        *os_context_register_addr(context,reg_a)=*os_context_pc_addr(context);
        branch = 1;
        break;
    case 0x35: /* fbne */
        if (*os_context_register_addr(context,reg_a)!=0) branch = 1;
        break;
    case 0x36: /* fbge */
        if (*os_context_float_register_addr(context,reg_a)>=0) branch = 1;
        break;
    case 0x37: /* fbgt */
        if (*os_context_float_register_addr(context,reg_a)>0) branch = 1;
        break;
    case 0x38: /* blbc */
        if ((*os_context_register_addr(context,reg_a)&1) == 0) branch = 1;
        break;
    case 0x39: /* beq */
        if (*os_context_register_addr(context,reg_a)==0) branch = 1;
        break;
    case 0x3a: /* blt */
        if (*os_context_register_addr(context,reg_a)<0) branch = 1;
        break;
    case 0x3b: /* ble */
        if (*os_context_register_addr(context,reg_a)<=0) branch = 1;
        break;
    case 0x3c: /* blbs */
        if ((*os_context_register_addr(context,reg_a)&1)!=0) branch = 1;
        break;
    case 0x3d: /* bne */
        if (*os_context_register_addr(context,reg_a)!=0) branch = 1;
        break;
    case 0x3e: /* bge */
        if (*os_context_register_addr(context,reg_a)>=0) branch = 1;
        break;
    case 0x3f: /* bgt */
        if (*os_context_register_addr(context,reg_a)>0) branch = 1;
        break;
    }
    if (branch)
        next_pc += disp*4;
    return next_pc;
}

static sigset_t orig_sigmask;

/* Perform the instruction that we overwrote with a breakpoint.  As we
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

void arch_do_displaced_inst(os_context_t *context,unsigned int orig_inst)
{
    /* Apparent off-by-one errors ahoy.  If you consult the Alpha ARM,
     * it will tell you that after a BPT, the saved PC is the address
     * of the instruction _after_ the instruction that caused the trap.
     *
     * However, we decremented PC by 4 before calling the Lisp-level
     * handler that calls this routine (see alpha-arch.c line 322 and
     * friends) so when we get to this point PC is actually pointing
     * at the BPT instruction itself.  This is good, because this is
     * where we want to restart execution when we do that */

    unsigned int *pc=(unsigned int *)(*os_context_pc_addr(context));
    unsigned int *next_pc;
    int op = orig_inst >> 26;;

    orig_sigmask = *os_context_sigmask_addr(context);
    sigaddset_blockable(os_context_sigmask_addr(context));

    /* Put the original instruction back. */
    *pc = orig_inst;
    os_flush_icache((os_vm_address_t)pc, sizeof(unsigned int));
    skipped_break_addr = pc;

    /* Figure out where we will end up after running the displaced
     * instruction */
    if (op == 0x1a || (op&0xf) == 0x30) /* a branch */
        /* The cast to long is just to shut gcc up. */
        next_pc = (unsigned int *)((long)emulate_branch(context,orig_inst));
    else
        next_pc = pc+1;

    /* Set the after breakpoint. */
    displaced_after_inst = *next_pc;
    *next_pc = BREAKPOINT_INST;
    after_breakpoint=1;
    os_flush_icache((os_vm_address_t)next_pc, sizeof(unsigned int));
}

void
arch_handle_breakpoint(os_context_t *context)
{
        *os_context_pc_addr(context) -=4;
        handle_breakpoint(context);
}

void
arch_handle_fun_end_breakpoint(os_context_t *context)
{
    *os_context_pc_addr(context) -=4;
    *os_context_pc_addr(context) =
        (int)handle_fun_end_breakpoint(context);
}

void
arch_handle_single_step_trap(os_context_t *context, int trap)
{
    unsigned int code = *((u32 *) (*os_context_pc_addr(context)));
    int register_offset = code >> 5 & 0x1f;
    handle_single_step_trap(context, trap, register_offset);
    arch_skip_instruction(context);
}

static void
sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    unsigned int code;

    /* this is different from how CMUCL does it.  CMUCL used "call_pal
     * PAL_gentrap", which doesn't do anything on Linux (unless NL0
     * contains certain specific values).  We use "bugchk" instead.
     * It's (for our purposes) just the same as bpt but has a
     * different opcode so we can test whether we're dealing with a
     * breakpoint or a "system service" */

    if ((*(unsigned int*)(*os_context_pc_addr(context)-4))==BREAKPOINT_INST) {
        if (after_breakpoint) {
            /* see comments above arch_do_displaced_inst.  This is where
             * we reinsert the breakpoint that we removed earlier */

            *os_context_pc_addr(context) -=4;
            *skipped_break_addr = BREAKPOINT_INST;
            os_flush_icache((os_vm_address_t)skipped_break_addr,
                            sizeof(unsigned int));
            skipped_break_addr = NULL;
            *(unsigned int *)*os_context_pc_addr(context) =
                displaced_after_inst;
            os_flush_icache((os_vm_address_t)*os_context_pc_addr(context), sizeof(unsigned int));
            *os_context_sigmask_addr(context)= orig_sigmask;
            after_breakpoint=0; /* false */
            return;
        } else
            code = trap_Breakpoint;
    } else
        /* a "system service" */
    code=*((u32 *)(*os_context_pc_addr(context)));
    handle_trap(context, code);
}

unsigned long
arch_get_fp_control()
{
    return ieee_get_fp_control();
}

void
arch_set_fp_control(unsigned long fp)
{
    ieee_set_fp_control(fp);
}


void arch_install_interrupt_handlers()
{
    undoably_install_low_level_interrupt_handler(SIGTRAP, sigtrap_handler);
}
