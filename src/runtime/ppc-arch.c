#include <stdio.h>

#include "sbcl.h"
#include "arch.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "lispregs.h"
#include "signal.h"
#include "interrupt.h"
#include "interr.h"

  /* The header files may not define PT_DAR/PT_DSISR.  This definition
     is correct for all versions of ppc linux >= 2.0.30

     As of DR2.1u4, MkLinux doesn't pass these registers to signal
     handlers correctly; a patch is necessary in order to (partially)
     correct this.

     Even with the patch, the DSISR may not have its 'write' bit set
     correctly (it tends not to be set if the fault was caused by
     something other than a protection violation.)
     
     Caveat callers.  */

#ifndef PT_DAR
#define PT_DAR		41
#endif

#ifndef PT_DSISR
#define PT_DSISR	42
#endif

void arch_init() {
}

os_vm_address_t 
arch_get_bad_addr(int sig, siginfo_t *code, os_context_t *context)
{
    unsigned int *pc =  (unsigned int *)(*os_context_pc_addr(context));
    os_vm_address_t addr;
    
    
    /* Make sure it's not the pc thats bogus, and that it was lisp code */
    /* that caused the fault. */
    if ((((unsigned long)pc) & 3) != 0 ||
	((pc < READ_ONLY_SPACE_START ||
	  pc >= READ_ONLY_SPACE_START+READ_ONLY_SPACE_SIZE) &&
	 ((lispobj *)pc < current_dynamic_space || 
	  (lispobj *)pc >= current_dynamic_space + DYNAMIC_SPACE_SIZE)))
	return 0;
    
    
    addr = (os_vm_address_t) (*os_context_register_addr(context,PT_DAR));
    return addr;
}
      

void 
arch_skip_instruction(os_context_t *context)
{
    ((char*)*os_context_pc_addr(context)) +=4; 
}

unsigned char *
arch_internal_error_arguments(os_context_t *context)
{
    return (unsigned char *)(*os_context_pc_addr(context)+4);
}


boolean 
arch_pseudo_atomic_atomic(os_context_t *context)
{
    return ((*os_context_register_addr(context,reg_ALLOC)) & 4);
}

#define PSEUDO_ATOMIC_INTERRUPTED_BIAS 0x7f000000

void 
arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    *os_context_register_addr(context,reg_NL3) 
	+= PSEUDO_ATOMIC_INTERRUPTED_BIAS;
}

unsigned long 
arch_install_breakpoint(void *pc)
{
    unsigned long *ptr = (unsigned long *)pc;
    unsigned long result = *ptr;
    *ptr = (3<<26) | (5 << 21) | trap_Breakpoint;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
    return result;
}

void 
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
    *(unsigned long *)pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
}

static unsigned long *skipped_break_addr, displaced_after_inst;
static sigset_t orig_sigmask;

void 
arch_do_displaced_inst(os_context_t *context,unsigned int orig_inst)
{
    /* not sure how we ensure that we get the breakpoint reinstalled
     * after doing this -dan */
    unsigned long *pc = (unsigned long *)(*os_context_pc_addr(context));
    
    orig_sigmask = *os_context_sigmask_addr(context);
    sigaddset_blockable(os_context_sigmask_addr(context));
    
    *pc = orig_inst;
    os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
    skipped_break_addr = pc;
}

static void 
sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    u32 code;
    sigset_t *mask;
#ifdef LISP_FEATURE_LINUX
    os_restore_fp_control(context);
#endif
    mask=(os_context_sigmask_addr(context));
    sigsetmask(mask); 
    code=*((u32 *)(*os_context_pc_addr(context)));
    if (code == ((3 << 26) | (16 << 21) | (reg_ALLOC << 16))) {
	/* twlti reg_ALLOC,0 - check for deferred interrupt */
	*os_context_register_addr(context,reg_ALLOC) 
	    -= PSEUDO_ATOMIC_INTERRUPTED_BIAS;
	arch_skip_instruction(context);
	/* interrupt or GC was requested in PA; now we're done with the
	   PA section we may as well get around to it */
	interrupt_handle_pending(context);
	return;
	
    }
    if ((code >> 16) == ((3 << 10) | (6 << 5))) {
	/* twllei reg_ZERO,N will always trap if reg_ZERO = 0 */
	int trap = code & 0x1f;
	
	switch (trap) {
	case trap_Halt:
	    fake_foreign_function_call(context);
	    lose("%%primitive halt called; the party is over.\n");
	    
	case trap_Error:
	case trap_Cerror:
	    interrupt_internal_error(signal, code, context, trap == trap_Cerror);
	    break;
	    
	case trap_PendingInterrupt:
	  /* when do we run this branch instead of the twlti code above? */
	    arch_skip_instruction(context);
	    interrupt_handle_pending(context);
	    break;
	    
	case trap_Breakpoint:
	    handle_breakpoint(signal, code, context);
	    break;
	    
	case trap_FunEndBreakpoint:
	    *os_context_pc_addr(context)
		=(int)handle_fun_end_breakpoint(signal, code, context);
	    break;
	    
	case trap_AfterBreakpoint:
	    *skipped_break_addr = trap_Breakpoint;
	    skipped_break_addr = NULL;
	    *(unsigned long *)*os_context_pc_addr(context) 
		= displaced_after_inst;
	    *os_context_sigmask_addr(context)= orig_sigmask;
 
	    os_flush_icache((os_vm_address_t) *os_context_pc_addr(context),
			    sizeof(unsigned long));
	    break;
	    
	default:
	    interrupt_handle_now(signal, code, context);
	    break;
	}
#ifdef LISP_FEATURE_DARWIN
	sigreturn(context);
#endif
	return;
    }
    if (((code >> 26) == 3) && (((code >> 21) & 31) == 24)) {
	interrupt_internal_error(signal, code, context, 0);
#ifdef LISP_FEATURE_DARWIN
	sigreturn(context);
#endif
	return;
    }
    
    interrupt_handle_now(signal, code, context);
#ifdef LISP_FEATURE_DARWIN
    /* Work around G5 bug */
    sigreturn(context);
#endif
}


void arch_install_interrupt_handlers()
{
    undoably_install_low_level_interrupt_handler(SIGILL,sigtrap_handler);
    undoably_install_low_level_interrupt_handler(SIGTRAP,sigtrap_handler);
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

void
ppc_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
  os_vm_address_t end = (os_vm_address_t) ((int)(address+length+(32-1)) &~(32-1));
  extern void ppc_flush_cache_line(os_vm_address_t);

  while (address < end) {
    ppc_flush_cache_line(address);
    address += 32;
  }
}

#ifdef LISP_FEATURE_LINKAGE_TABLE

/* Linkage tables for PowerPC
 *
 * Linkage entry size is 16, because we need at least 4 instructions to
 * implement a jump.
 */

/*
 * Define the registers to use in the linkage jump table. Can be the
 * same. Some care must be exercised when choosing these. It has to be
 * a register that is not otherwise being used. reg_NFP is a good
 * choice. call_into_c trashes reg_NFP without preserving it, so we can
 * trash it in the linkage jump table.
 */
#define LINKAGE_TEMP_REG        reg_NFP
#define LINKAGE_ADDR_REG        reg_NFP

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
   *        addis 13, 0, (hi part of addr)
   *        ori   13, 13, (low part of addr)
   *        mtctr 13
   *        bctr
   *        
   */
  int* inst_ptr;
  unsigned long hi;                   /* Top 16 bits of address */
  unsigned long lo;                   /* Low 16 bits of address */
  unsigned int inst;

  inst_ptr = (int*) reloc_addr;

  /*
   * Split the target address into hi and lo parts for the sethi
   * instruction.  hi is the top 22 bits.  lo is the low 10 bits.
   */
  hi = (unsigned long) target_addr;
  lo = hi & 0xffff;
  hi >>= 16;

  /*
   * addis 13, 0, (hi part)
   */
      
  inst = (15 << 26) | (LINKAGE_TEMP_REG << 21) | (0 << 16) | hi;
  *inst_ptr++ = inst;

  /*
   * ori 13, 13, (lo part)
   */

  inst = (24 << 26) | (LINKAGE_TEMP_REG << 21) | (LINKAGE_TEMP_REG << 16) | lo;
  *inst_ptr++ = inst;
  
  /*
   * mtctr 13
   */

  inst = (31 << 26) | (LINKAGE_TEMP_REG << 21) | (9 << 16) | (467 << 1);
  *inst_ptr++ = inst;

  /*
   * bctr
   */

  inst = (19 << 26) | (20 << 21) | (528 << 1);
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
