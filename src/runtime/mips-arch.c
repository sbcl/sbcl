/*

 $Header$

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

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

void arch_init()
{
    return;
}

os_vm_address_t arch_get_bad_addr(int signam, siginfo_t *siginfo, os_context_t *context)
{
    /* Classic CMUCL comment:

       Finding the bad address on the mips is easy. */
    return (os_vm_address_t) siginfo->si_addr;
}

unsigned long 
emulate_branch(os_context_t *context, unsigned long inst)
{
    long opcode = inst >> 26;
    long r1 = (inst >> 21) & 0x1f;
    long r2 = (inst >> 16) & 0x1f;
    long bdisp = (inst&(1<<15)) ? inst | (-1 << 16) : inst&0xffff;
    long jdisp = (inst&(1<<25)) ? inst | (-1 << 26) : inst&0xffff;
    long disp = 0;

    switch(opcode) {
    case 0x1: /* bltz, bgez, bltzal, bgezal */
	switch((inst >> 16) & 0x1f) {
	case 0x00: /* bltz */
	    if(*os_context_register_addr(context, r1) < 0)
		disp = bdisp;
	    break;
	case 0x01: /* bgez */
	    if(*os_context_register_addr(context, r1) >= 0)
		disp = bdisp;
	    break;
	case 0x10: /* bltzal */
	    if(*os_context_register_addr(context, r1) < 0)
		disp = bdisp;
	    *os_context_register_addr(context, 31) = *os_context_pc_addr(context) + 4;
	    break;
	case 0x11: /* bgezal */
	    if(*os_context_register_addr(context, r1) >= 0)
		disp = bdisp;
	    *os_context_register_addr(context, 31) = *os_context_pc_addr(context) + 4;
	    break;
	}
	break;
    case 0x4: /* beq */
	if(*os_context_register_addr(context, r1)
	   == *os_context_register_addr(context, r2))
	    disp = bdisp;
	break;
    case 0x5: /* bne */
	if(*os_context_register_addr(context, r1) 
	   != *os_context_register_addr(context, r2))
	    disp = bdisp;
	break;
    case 0x6: /* ble */
	if(*os_context_register_addr(context, r1)
	   /* FIXME: One has to assume that the CMUCL gods of old have
              got the sign issues right... but it might be worth
              checking, someday */
	   <= *os_context_register_addr(context, r2))
	    disp = bdisp;
	break;
    case 0x7: /* bgtz */
	if(*os_context_register_addr(context, r1)
	   >= *os_context_register_addr(context, r2))
	    disp = bdisp;
	break;
    case 0x2: /* j */
	disp = jdisp;
	break;
    case 0x3: /* jal */
	disp = jdisp;
	*os_context_register_addr(context, 31) = *os_context_pc_addr(context) + 4;
	break;
    }
    return (*os_context_pc_addr(context) + disp * 4);
}

void arch_skip_instruction(os_context_t *context)
{
    /* Skip the offending instruction */
    if (os_context_bd_cause(context))
        *os_context_pc_addr(context) = 
	    emulate_branch(context, 
			   *(unsigned long *) *os_context_pc_addr(context));
    else
        *os_context_pc_addr(context) += 4;

    os_flush_icache((os_vm_address_t) *os_context_pc_addr(context), sizeof(unsigned long));
}

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
    if (os_context_bd_cause(context))
	return (unsigned char *)(*os_context_pc_addr(context) + 8);
    else
	return (unsigned char *)(*os_context_pc_addr(context) + 4);
}

boolean arch_pseudo_atomic_atomic(os_context_t *context)
{
    return *os_context_register_addr(context, reg_ALLOC) & 1;
}

#define PSEUDO_ATOMIC_INTERRUPTED_BIAS 0x7f000000

void arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    *os_context_register_addr(context, reg_NL4) |= 1<<31;
}

unsigned long arch_install_breakpoint(void *pc)
{
    unsigned long *ptr = (unsigned long *)pc;
    unsigned long result = *ptr;
    *ptr = (trap_Breakpoint << 16) | 0xd;

    os_flush_icache((os_vm_address_t)ptr, sizeof(unsigned long));

    return result;
}

void arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    *(unsigned long *)pc = orig_inst;

    os_flush_icache((os_vm_address_t)pc, sizeof(unsigned long));
}

static unsigned long *skipped_break_addr, displaced_after_inst;
static sigset_t orig_sigmask;

void arch_do_displaced_inst(os_context_t *context,
			    unsigned int orig_inst)
{
    unsigned long *pc = (unsigned long *)*os_context_pc_addr(context);
    unsigned long *break_pc, *next_pc;
    unsigned long next_inst;
    int opcode;

    orig_sigmask = *os_context_sigmask_addr(context);
    sigaddset_blockable(os_context_sigmask_addr(context));

    /* Figure out where the breakpoint is, and what happens next. */
    if (os_context_bd_cause(context)) {
	break_pc = pc+1;
	next_inst = *pc;
    }
    else {
	break_pc = pc;
	next_inst = orig_inst;
    }

    /* Put the original instruction back. */
    *break_pc = orig_inst;
    os_flush_icache((os_vm_address_t)break_pc, sizeof(unsigned long));
    skipped_break_addr = break_pc;

    /* Figure out where it goes. */
    opcode = next_inst >> 26;
    if (opcode == 1 || ((opcode & 0x3c) == 0x4) || ((next_inst & 0xf00e0000) == 0x80000000)) {
        
        next_pc = emulate_branch(context, next_inst);
    }
    else
	next_pc = pc+1;

    displaced_after_inst = *next_pc;
    *next_pc = (trap_AfterBreakpoint << 16) | 0xd;
    os_flush_icache((os_vm_address_t)next_pc, sizeof(unsigned long));
}

static void sigtrap_handler(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    sigset_t *mask;
    int code;
    /* Don't disallow recursive breakpoint traps.  Otherwise, we can't */
    /* use debugger breakpoints anywhere in here. */
    mask = os_context_sigmask_addr(context);
    sigsetmask(mask);
    code = ((*(int *) (*os_context_pc_addr(context))) >> 16) & 0x1f;

    switch (code) {
    case trap_PendingInterrupt:
	arch_skip_instruction(context);
	interrupt_handle_pending(context);
	break;
	
    case trap_Halt:
	fake_foreign_function_call(context);
	lose("%%primitive halt called; the party is over.\n");
	
    case trap_Error:
    case trap_Cerror:
	interrupt_internal_error(signal, info, context, code==trap_Cerror);
	break;
	
    case trap_Breakpoint:
	handle_breakpoint(signal, info, context);
	break;
	
    case trap_FunEndBreakpoint:
	*os_context_pc_addr(context) = (int)handle_fun_end_breakpoint(signal, info, context);
	break;
	
    case trap_AfterBreakpoint:
	*skipped_break_addr = (trap_Breakpoint << 16) | 0xd;
	os_flush_icache((os_vm_address_t)skipped_break_addr,
			sizeof(unsigned long));
	skipped_break_addr = NULL;
	*(unsigned long *)(*os_context_pc_addr(context)) = displaced_after_inst;
	os_flush_icache((os_vm_address_t) *os_context_pc_addr(context), sizeof(unsigned long));
	*os_context_sigmask_addr(context) = orig_sigmask;
	break;

    case 0x10:
	/* Clear the flag */
	*os_context_register_addr(context, reg_NL4) &= 0x7fffffff;
	arch_skip_instruction(context);
	interrupt_handle_pending(context);
	return;
	
    default:
	interrupt_handle_now(signal, info, context);
	break;
    }
}

/* FIXME: We must have one of these somewhere. Also, export
   N-FIXNUM-TAG-BITS from Lispland and use it rather than 2 here. */
#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

void sigfpe_handler(int signal, siginfo_t *info, void *void_context)
{
    unsigned long bad_inst;
    unsigned int op, rs, rt, rd, funct, dest;
    int immed;
    long result;
    os_context_t *context = arch_os_get_context(&void_context);

    if (os_context_bd_cause(context))
        bad_inst = *(unsigned long *)(*os_context_pc_addr(context) + 4);
    else
        bad_inst = *(unsigned long *)(*os_context_pc_addr(context));

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
	    /* FIXME: Hopefully, this whole section can just go away,
               with the rewrite of pseudo-atomic and the deletion of
               overflow VOPs */
	    /* Check to see if this is really a pa_interrupted hit */
	    if (rs == reg_ALLOC && rt == reg_NL4) {
		*os_context_register_addr(context, reg_ALLOC)
		    += (*os_context_register_addr(context, reg_NL4)
			- PSEUDO_ATOMIC_INTERRUPTED_BIAS);
		arch_skip_instruction(context);
		interrupt_handle_pending(context);
		return;
	    }
	    result = FIXNUM_VALUE(*os_context_register_addr(context, rs))
		+ FIXNUM_VALUE(*os_context_register_addr(context, rt));
	    dest = rd;
	    break;
	    
	case 0x22: /* SUB */
	    result = FIXNUM_VALUE(*os_context_register_addr(context, rs))
		- FIXNUM_VALUE(*os_context_register_addr(context, rt));
	    dest = rd;
	    break;
	    
	default:
	    dest = 32;
	    break;
	}
	break;
	
    case 0x8: /* ADDI */
	result = FIXNUM_VALUE(*os_context_register_addr(context,rs)) + (immed>>2);
	dest = rt;
	break;
	
    default:
	dest = 32;
	break;
    }
    
    if (dest < 32) {
        dynamic_space_free_pointer =
            (lispobj *) *os_context_register_addr(context,reg_ALLOC);

        *os_context_register_addr(context,dest) = alloc_number(result);

	*os_context_register_addr(context, reg_ALLOC) =
	    (unsigned long) dynamic_space_free_pointer;
	
        arch_skip_instruction(context);
	
    }
    else
        interrupt_handle_now(signal, info, context);
}

void arch_install_interrupt_handlers()
{    
    undoably_install_low_level_interrupt_handler(SIGTRAP,sigtrap_handler);
    undoably_install_low_level_interrupt_handler(SIGFPE,sigfpe_handler);
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

