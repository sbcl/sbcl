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
#include <asm/pal.h>		/* for PAL_gentrap */

#include "runtime.h"
#include "sbcl.h"
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

void arch_init(void)
{
    /* this must be called _after_ os_init, so we know what the page size is */
    if(mmap((os_vm_address_t) call_into_lisp_LRA_page,os_vm_page_size,
	    OS_VM_PROT_ALL,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED,-1,0)
       == (os_vm_address_t) -1)
	perror("mmap");
    
    /* call_into_lisp_LRA is a collection of trampolines written in asm -
     * see alpha-assem.S.  We copy it to call_into_lisp_LRA_page where
     * VOPs and things can find it (I don't know why they can't find it 
     * where it was to start with). */
    bcopy(call_into_lisp_LRA,(void *)call_into_lisp_LRA_page,os_vm_page_size);

    os_flush_icache((os_vm_address_t)call_into_lisp_LRA_page,
		    os_vm_page_size);
    return;
}

os_vm_address_t 
arch_get_bad_addr (int sig, siginfo_t *code, os_context_t *context)
{
  unsigned int badinst;

  /* instructions are 32 bit quantities */
  unsigned int *pc ;
  /*  fprintf(stderr,"arch_get_bad_addr %d %p %p\n",
      sig, code, context); */
  pc= (unsigned int *)(*os_context_pc_addr(context));

  if(((unsigned long)pc) & 3) 
      return NULL;		/* in what case would pc be unaligned? */

  if( (pc < READ_ONLY_SPACE_START ||
       pc >= READ_ONLY_SPACE_START+READ_ONLY_SPACE_SIZE) && 
      (pc < current_dynamic_space ||
       pc >= current_dynamic_space + DYNAMIC_SPACE_SIZE))
    return NULL;

  badinst = *pc;

  if(((badinst>>27)!=0x16)      /* STL or STQ */
     && ((badinst>>27)!=0x13))  /* STS or STT */
    return NULL;                /* Otherwise forget about address */
  
  return (os_vm_address_t)
    (*os_context_register_addr(context,((badinst>>16)&0x1f))
     +(badinst&0xffff));
}

void arch_skip_instruction(os_context_t *context)
{
    /* this may be complete rubbish, as (at least for traps) pc points
     * _after_ the instruction that caused us to be here anyway
     */
    ((char*)*os_context_pc_addr(context)) +=4; }

unsigned char *arch_internal_error_arguments(os_context_t *context)
{
  return (unsigned char *)(*os_context_pc_addr(context)+4);
}

boolean arch_pseudo_atomic_atomic(os_context_t *context)
{
  return ((*os_context_register_addr(context,reg_ALLOC)) & 1);
}

void arch_set_pseudo_atomic_interrupted(os_context_t *context)
{
    /* On coming out of an atomic section, we subtract 1 from
     * reg_Alloc, then try to store something at that address.  On
     * OSF/1 we add 1 to reg_Alloc here so that the end-of-atomic code
     * will raise SIGTRAP for "unaligned access".  Linux catches
     * unaligned accesses in the kernel and fixes them up[1], so there
     * we toggle bit 63 instead.  The resulting address is somewhere
     * out in no-man's land, so we get SIGSEGV when we try to access
     * it.  We catch whichever signal it is (see the appropriate
     * *-os.c) and call interrupt_handle_pending() from it */

    /* [1] This behaviour can be changed with osf_setsysinfo, but cmucl
     * didn't use that */

#ifdef linux
  *os_context_register_addr(context,reg_ALLOC) |=  (1L<<63);
#else
  *os_context_register_addr(context,reg_ALLOC) |=  2;
#endif
}

/* XXX but is the caller of this storing all 64 bits? */
unsigned long arch_install_breakpoint(void *pc)
{
    unsigned int *ptr = (unsigned int *)pc;
    unsigned long result = (unsigned long) *ptr;
    *ptr = BREAKPOINT_INST;
    *(ptr+1)=trap_Breakpoint;
    
    os_flush_icache((os_vm_address_t)ptr, sizeof(unsigned long));
    
    return result;
}

void arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
  /* was (unsigned int) but gcc complains.  Changed to mirror
     install_breakpoint above */
  unsigned long *ptr=(unsigned long *)pc;
  *ptr = orig_inst;
  os_flush_icache((os_vm_address_t)pc, sizeof(unsigned long));
}

static unsigned int *skipped_break_addr, displaced_after_inst,
     after_breakpoint;


/* Returns a PC value.  Lisp code is all in the 32-bit-addressable
   space,so we should be ok with an unsigned int */

unsigned int
emulate_branch(os_context_t *context,unsigned long orig_inst)
{
  int op = orig_inst >> 26;
  int reg_a = (orig_inst >> 21) & 0x1f;
  int reg_b = (orig_inst >> 16) & 0x1f;
  int fn = orig_inst & 0xffff;
  int disp = (orig_inst&(1<<20)) ? orig_inst | (-1 << 21) : orig_inst&0x1fffff;
  int next_pc = *os_context_pc_addr(context);
  int branch = 0; /* was NULL;	       */

  switch(op) {
  case 0x1a: /* jmp, jsr, jsr_coroutine, ret */
    *os_context_register_addr(context,reg_a)=*os_context_pc_addr(context);
    *os_context_pc_addr(context)=*os_context_register_addr(context,reg_b)& ~3;
    break;
  case 0x30: /* br */
    *os_context_register_addr(context,reg_a)=*os_context_pc_addr(context);
    branch = 1;
    break;
  case 0x31: /* fbeq */
    if(*(os_context_fpregister_addr(context,reg_a))==0) branch = 1;
    break;
  case 0x32: /* fblt */
    if(*os_context_fpregister_addr(context,reg_a)<0) branch = 1;
    break;
  case 0x33: /* fble */
    if(*os_context_fpregister_addr(context,reg_a)<=0) branch = 1;
    break;
  case 0x34: /* bsr */
    *os_context_register_addr(context,reg_a)=*os_context_pc_addr(context);
    branch = 1;
    break;
  case 0x35: /* fbne */
    if(*os_context_register_addr(context,reg_a)!=0) branch = 1;
    break;
  case 0x36: /* fbge */
    if(*os_context_fpregister_addr(context,reg_a)>=0) branch = 1;
    break;
  case 0x37: /* fbgt */
    if(*os_context_fpregister_addr(context,reg_a)>0) branch = 1;
    break;
  case 0x38: /* blbc */
    if((*os_context_register_addr(context,reg_a)&1) == 0) branch = 1;
    break;
  case 0x39: /* beq */
    if(*os_context_register_addr(context,reg_a)==0) branch = 1;
    break;
  case 0x3a: /* blt */
    if(*os_context_register_addr(context,reg_a)<0) branch = 1;
    break;
  case 0x3b: /* ble */
    if(*os_context_register_addr(context,reg_a)<=0) branch = 1;
    break;
  case 0x3c: /* blbs */
    if((*os_context_register_addr(context,reg_a)&1)!=0) branch = 1;
    break;
  case 0x3d: /* bne */
    if(*os_context_register_addr(context,reg_a)!=0) branch = 1;
    break;
  case 0x3e: /* bge */
    if(*os_context_register_addr(context,reg_a)>=0) branch = 1;
    break;
  case 0x3f: /* bgt */
    if(*os_context_register_addr(context,reg_a)>0) branch = 1;
    break;
  }
  if(branch) next_pc += disp*4;
  return next_pc;
}

static sigset_t orig_sigmask;

void arch_do_displaced_inst(os_context_t *context,unsigned int orig_inst)
{
  unsigned int *pc=(unsigned int *)(*os_context_pc_addr(context));
  unsigned int *next_pc;
  unsigned int next_inst;
  int op = orig_inst >> 26;;
  fprintf(stderr,"arch_do_displaced_inst depends on sigreturn, which is not implemented and will\nalways fail\n");
  orig_sigmask = *os_context_sigmask_addr(context);
  sigaddset_blockable(os_context_sigmask_addr(context));

  /* Figure out where the displaced inst is going */
  if(op == 0x1a || (op&0xf) == 0x30) /* branch...ugh */
    /* the cast to long is just to shut gcc up */
    next_pc = (unsigned int *)((long)emulate_branch(context,orig_inst));
  else
    next_pc = pc+1;

  /* Put the original instruction back. */
  *pc = orig_inst;
  os_flush_icache((os_vm_address_t)pc, sizeof(unsigned long));
  skipped_break_addr = pc;

  /* set the after breakpoint */
  displaced_after_inst = *next_pc;
  *next_pc = BREAKPOINT_INST;
  after_breakpoint=1;
  os_flush_icache((os_vm_address_t)next_pc, sizeof(unsigned long));

  ldb_monitor("sigreturn is not implemented and just failed");
  sigreturn(context);
}

#define AfterBreakpoint 100

static void sigill_handler(int signal, siginfo_t *siginfo, os_context_t *context) {
    fake_foreign_function_call(context);
    ldb_monitor();
}

static void sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    /* Don't disallow recursive breakpoint traps.  Otherwise, we can't */
    /* use debugger breakpoints anywhere in here. */

    sigset_t *mask=(os_context_sigmask_addr(context));
    unsigned int code;
    fprintf(stderr,"sigtrap_handler:signal %d context=%p ",signal,context);
    sigsetmask(mask); 

    /* this is different from how CMUCL does it.  CMUCL used
     * "call_pal PAL_gentrap", which doesn't do anything on Linux
     * so screwed up our offsets in odd ways.  We use "bpt" instead
     */

    /* probably we should
    assert(*(unsigned int*)(*os_context_pc_addr(context)-4) == BREAKPOINT_INST)
    but I've not decided a good way to handle it if it turns out not to be
    */
    code=*((u32 *)(*os_context_pc_addr(context)));
    fprintf(stderr,"pc=%lx  code=%d, inst=%x\n",
	    *os_context_pc_addr(context),  code,
	    *(unsigned int*)(*os_context_pc_addr(context)-4));
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
	interrupt_internal_error(signal, siginfo, context, code==trap_Cerror);
	break;

      case trap_Breakpoint:
        *os_context_pc_addr(context) -=4;
	handle_breakpoint(signal, siginfo, context);
	break;

      case trap_FunctionEndBreakpoint:
        *os_context_pc_addr(context) -=4;
	*os_context_pc_addr(context) = (int)handle_function_end_breakpoint(signal, siginfo, context);
	break;

      case AfterBreakpoint:
        *os_context_pc_addr(context) -=4;
	*skipped_break_addr = BREAKPOINT_INST;
	os_flush_icache((os_vm_address_t)skipped_break_addr,
			sizeof(unsigned long));
	skipped_break_addr = NULL;
	*(unsigned int *)*os_context_pc_addr(context) = displaced_after_inst;
	os_flush_icache((os_vm_address_t)*os_context_pc_addr(context), sizeof(unsigned long));
        *os_context_sigmask_addr(context)= orig_sigmask;
        after_breakpoint=0; /* NULL; */
	break;

      default:
	interrupt_handle_now(signal, siginfo, context);
	break;
    }
}

#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

static void sigfpe_handler(int signal, int code, os_context_t *context)
{
}

void arch_install_interrupt_handlers()
{
    undoably_install_low_level_interrupt_handler(SIGILL,  sigill_handler);
    undoably_install_low_level_interrupt_handler(SIGTRAP, sigtrap_handler);
    undoably_install_low_level_interrupt_handler(SIGFPE,  sigfpe_handler);
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


/* This is apparently called by emulate_branch, but isn't defined.  So */
/* just do nothing and hope it works... */

void cacheflush(void)
{
    /* hoping probably isn't _actually_ enough.  we should call_pal imb,
       according to the arch ref manual
    */
}
