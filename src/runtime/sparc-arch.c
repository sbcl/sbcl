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

#include "runtime.h"
#include "arch.h"
#include "sbcl.h"
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
extern int early_kernel;
#endif

void arch_init(void)
{
    return;
}

os_vm_address_t arch_get_bad_addr(int sig, siginfo_t *code, os_context_t *context)
{
    unsigned long badinst;
    unsigned long *pc;
    int rs1; 

    pc = (unsigned long *)(*os_context_pc_addr(context));

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
         pc >= current_dynamic_space + DYNAMIC_SPACE_SIZE)) {
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
}

void arch_skip_instruction(os_context_t *context)
{
    ((char *) *os_context_pc_addr(context)) = ((char *) *os_context_npc_addr(context));
    ((char *) *os_context_npc_addr(context)) += 4;
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char *)(*os_context_pc_addr(context) + 4);
}

boolean arch_pseudo_atomic_atomic(os_context_t *context)
{
    return ((*os_context_register_addr(context,reg_ALLOC)) & 4);
}

void arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    *os_context_register_addr(context,reg_ALLOC) |=  1;
}

unsigned long arch_install_breakpoint(void *pc)
{
    unsigned long *ptr = (unsigned long *)pc;
    unsigned long result = *ptr;
    *ptr = trap_Breakpoint;
  
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
    
    return result;
}

void arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    *(unsigned long *)pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
}

static unsigned long *skipped_break_addr, displaced_after_inst;
static sigset_t orig_sigmask;

void arch_do_displaced_inst(os_context_t *context, unsigned int orig_inst)
{
    unsigned long *pc = (unsigned long *)(*os_context_pc_addr(context));
    unsigned long *npc = (unsigned long *)(*os_context_npc_addr(context));

  /*  orig_sigmask = context->sigmask;
      sigemptyset(&context->sigmask); */
  /* FIXME!!! */
  /* FILLBLOCKSET(&context->uc_sigmask);*/

    *pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
    skipped_break_addr = pc;
    displaced_after_inst = *npc;
    *npc = trap_AfterBreakpoint;
    os_flush_icache((os_vm_address_t) npc, sizeof(unsigned long));
    
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

static void sigill_handler(int signal, siginfo_t *siginfo, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
#ifdef LISP_FEATURE_LINUX
    /* FIXME: Check that this is necessary -- CSR, 2002-07-15 */
    os_restore_fp_control(context);
#endif
    sigprocmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);
    
    if ((siginfo->si_code) == ILL_ILLOPC
#ifdef LISP_FEATURE_LINUX
	|| (early_kernel && (siginfo->si_code == 2))
#endif
	) {
	int trap;
	unsigned int inst;
	unsigned int* pc = (unsigned int*) siginfo->si_addr;

	inst = *pc;
	trap = inst & 0x3fffff;
	
	switch (trap) {
	case trap_PendingInterrupt:
	    arch_skip_instruction(context);
	    interrupt_handle_pending(context);
	    break;
	    
	case trap_Halt:
	    fake_foreign_function_call(context);
	    lose("%%primitive halt called; the party is over.\n");
	    
	case trap_Error:
	case trap_Cerror:
	    interrupt_internal_error(signal, siginfo, context, trap == trap_Cerror);
	    break;
	    
	case trap_Breakpoint:
	    handle_breakpoint(signal, siginfo, context);
	    break;
	    
	case trap_FunEndBreakpoint:
	    *os_context_pc_addr(context) = (int) handle_fun_end_breakpoint(signal, siginfo, context);
	    *os_context_npc_addr(context) = *os_context_pc_addr(context) + 4;
	    break;
	    
	case trap_AfterBreakpoint:
	    *skipped_break_addr = trap_Breakpoint;
	    skipped_break_addr = NULL;
	    *(unsigned long *) os_context_pc_addr(context) = displaced_after_inst;
	    /* context->sigmask = orig_sigmask; */
	    os_flush_icache((os_vm_address_t) os_context_pc_addr(context), sizeof(unsigned long));
	    break;
	    
	default:
	    interrupt_handle_now(signal, siginfo, context);
	    break;
	}
    }
    else if ((siginfo->si_code) == ILL_ILLTRP
#ifdef LISP_FEATURE_LINUX
	     || (early_kernel && (siginfo->si_code) == 192)
#endif
	     ) {
	if (pseudo_atomic_trap_p(context)) {
	    /* A trap instruction from a pseudo-atomic.  We just need
	       to fixup up alloc-tn to remove the interrupted flag,
	       skip over the trap instruction, and then handle the
	       pending interrupt(s). */
	    *os_context_register_addr(context, reg_ALLOC) &= ~7;
	    arch_skip_instruction(context);
	    interrupt_handle_pending(context);
	}
	else {
	    interrupt_internal_error(signal, siginfo, context, 0);
	}
    }
    else {
	interrupt_handle_now(signal, siginfo, context);
    }
}

static void sigemt_handler(int signal, siginfo_t *siginfo, void *void_context)
{
    unsigned long badinst;
    boolean subtract, immed;
    int rd, rs1, op1, rs2, op2, result;
    os_context_t *context = arch_os_get_context(&void_context);
#ifdef LISP_FEATURE_LINUX
    os_restore_fp_control(context);
#endif
    
    badinst = *(unsigned long *)os_context_pc_addr(context);
    if ((badinst >> 30) != 2 || ((badinst >> 20) & 0x1f) != 0x11) {
	/* It wasn't a tagged add.  Pass the signal into lisp. */
	interrupt_handle_now(signal, siginfo, context);
	return;
    }
    
    fprintf(stderr, "SIGEMT trap handler with tagged op instruction!\n");
    
    /* Extract the parts of the inst. */
    subtract = badinst & (1<<19);
    rs1 = (badinst>>14) & 0x1f;
    op1 = *os_context_register_addr(context, rs1);
    
    /* If the first arg is $ALLOC then it is really a signal-pending note */
    /* for the pseudo-atomic noise. */
    if (rs1 == reg_ALLOC) {
	/* Perform the op anyway. */
	op2 = badinst & 0x1fff;
	if (op2 & (1<<12))
	    op2 |= -1<<13;
	if (subtract)
	    result = op1 - op2;
	else
	    result = op1 + op2;
	*os_context_register_addr(context, reg_ALLOC) = result & ~7;
	arch_skip_instruction(context);
	interrupt_handle_pending(context);
	return;
    }
    
    if ((op1 & 3) != 0) {
	/* The first arg wan't a fixnum. */
	interrupt_internal_error(signal, siginfo, context, 0);
	return;
    }
    
    if (immed = badinst & (1<<13)) {
	op2 = badinst & 0x1fff;
	if (op2 & (1<<12))
	    op2 |= -1<<13;
    }
    else {
	rs2 = badinst & 0x1f;
	op2 = *os_context_register_addr(context, rs2);
    }
    
    if ((op2 & 3) != 0) {
	/* The second arg wan't a fixnum. */
	interrupt_internal_error(signal, siginfo, context, 0);
	return;
    }
    
    rd = (badinst>>25) & 0x1f;
    if (rd != 0) {
	/* Don't bother computing the result unless we are going to use it. */
	if (subtract)
	    result = (op1>>2) - (op2>>2);
	else
	    result = (op1>>2) + (op2>>2);
	
	dynamic_space_free_pointer =
	    (lispobj *) *os_context_register_addr(context, reg_ALLOC);
	
	*os_context_register_addr(context, rd) = alloc_number(result);
	
	*os_context_register_addr(context, reg_ALLOC) =
	    (unsigned long) dynamic_space_free_pointer;
    }
    
    arch_skip_instruction(context);
}

void arch_install_interrupt_handlers()
{
    undoably_install_low_level_interrupt_handler(SIGILL , sigill_handler);
    undoably_install_low_level_interrupt_handler(SIGEMT, sigemt_handler);
}

void get_spinlock(lispobj *word, int value) {
    /* FIXME: dummy definition */
    *word = value;
}


extern lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs);

lispobj funcall0(lispobj function)
{
    lispobj *args = current_control_stack_pointer;

    return call_into_lisp(function, args, 0);
}

lispobj funcall1(lispobj function, lispobj arg0)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 1;
    args[0] = arg0;

    return call_into_lisp(function, args, 1);
}

lispobj funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 2;
    args[0] = arg0;
    args[1] = arg1;

    return call_into_lisp(function, args, 2);
}

lispobj funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 3;
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;

    return call_into_lisp(function, args, 3);
}

