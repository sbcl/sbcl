/*
 * The x86 Linux incarnation of arch-dependent OS-dependent routines.
 * See also "linux-os.c".
 */

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
#include <stddef.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/types.h>
#include <unistd.h>

#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "sbcl.h"
#include <sys/socket.h>
#include <sys/utsname.h>

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <asm/ldt.h>
#include <linux/unistd.h>
#include <sys/mman.h>
#include "thread.h"		/* dynamic_values_bytes */

_syscall3(int, modify_ldt, int, func, void *, ptr, unsigned long, bytecount );

#include "validate.h"
size_t os_vm_page_size;

u32 local_ldt_copy[LDT_ENTRIES*LDT_ENTRY_SIZE/sizeof(u32)];

/* XXX this could be conditionally compiled based on some
 * "debug-friendly" flag.  But it doesn't really make stuff slower,
 * just the runtime gets fractionally larger */

void debug_get_ldt()
{ 
    int n=__modify_ldt (0, local_ldt_copy, sizeof local_ldt_copy);
    printf("%d bytes in ldt: print/x local_ldt_copy\n", n);
}

int arch_os_thread_init(struct thread *thread) {
    stack_t sigstack;
#ifdef LISP_FEATURE_SB_THREAD
    /* this must be called from a function that has an exclusive lock
     * on all_threads
     */
    struct modify_ldt_ldt_s ldt_entry = {
	1, 0, 0, /* index, address, length filled in later */
	1, MODIFY_LDT_CONTENTS_DATA, 0, 0, 0, 1
    }; 
    /* get next free ldt entry */
    int n=__modify_ldt(0,local_ldt_copy,sizeof local_ldt_copy);
    if(n) {
	u32 *p;
	for(n=0,p=local_ldt_copy;*p;p+=LDT_ENTRY_SIZE/sizeof(u32))
	    n++;
    }
    ldt_entry.entry_number=n;
    ldt_entry.base_addr=(unsigned long) thread;
    ldt_entry.limit=dynamic_values_bytes;
    ldt_entry.limit_in_pages=0;
    if (__modify_ldt (1, &ldt_entry, sizeof (ldt_entry)) != 0) 
	/* modify_ldt call failed: something magical is not happening */
	return -1;
    __asm__ __volatile__ ("movw %w0, %%fs" : : "q" 
			  ((n << 3) /* selector number */
			   + (1 << 2) /* TI set = LDT */
			   + 3)); /* privilege level */
    thread->tls_cookie=n;
    if(n<0) return 0;
#endif
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp=((void *) thread)+dynamic_values_bytes;
    sigstack.ss_flags=0;
    sigstack.ss_size = 32*SIGSTKSZ;
    sigaltstack(&sigstack,0);
#endif
    return 1;
}

/* if you can't do something like this (maybe because you're using a 
 * register for thread base that is only available in Lisp code)
 * you'll just have to find_thread_by_pid(getpid())
 */
struct thread *arch_os_get_current_thread() {
#ifdef LISP_FEATURE_SB_THREAD
    register struct thread *me=0;
    if(all_threads)
	__asm__ ("movl %%fs:%c1,%0" : "=r" (me)
		 : "i" (offsetof (struct thread,this)));
    return me;
#else
    return all_threads;
#endif
}

/* free any arch/os-specific resources used by thread, which is now
 * defunct.  Not called on live threads
 */

int arch_os_thread_cleanup(struct thread *thread) {
    struct modify_ldt_ldt_s ldt_entry = {
	0, 0, 0, 
	0, MODIFY_LDT_CONTENTS_DATA, 0, 0, 0, 0
    }; 

    ldt_entry.entry_number=thread->tls_cookie;
    if (__modify_ldt (1, &ldt_entry, sizeof (ldt_entry)) != 0) 
	/* modify_ldt call failed: something magical is not happening */
	return 0;
    return 1;
}



/* KLUDGE: As of kernel 2.2.14 on Red Hat 6.2, there's code in the
 * <sys/ucontext.h> file to define symbolic names for offsets into
 * gregs[], but it's conditional on __USE_GNU and not defined, so
 * we need to do this nasty absolute index magic number thing
 * instead. */
os_context_register_t *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case  0: return &context->uc_mcontext.gregs[11]; /* EAX */
    case  2: return &context->uc_mcontext.gregs[10]; /* ECX */
    case  4: return &context->uc_mcontext.gregs[9]; /* EDX */
    case  6: return &context->uc_mcontext.gregs[8]; /* EBX */
    case  8: return &context->uc_mcontext.gregs[7]; /* ESP */
    case 10: return &context->uc_mcontext.gregs[6]; /* EBP */
    case 12: return &context->uc_mcontext.gregs[5]; /* ESI */
    case 14: return &context->uc_mcontext.gregs[4]; /* EDI */
    default: return 0;
    }
    return &context->uc_mcontext.gregs[offset];
}

os_context_register_t *
os_context_pc_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[14]; /*  REG_EIP */
}

os_context_register_t *
os_context_sp_addr(os_context_t *context)
{				
    return &context->uc_mcontext.gregs[17]; /* REG_UESP */
}

os_context_register_t *
os_context_fp_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[6]; /* REG_EBP */
}

unsigned long
os_context_fp_control(os_context_t *context)
{
    return ((((context->uc_mcontext.fpregs->cw) & 0xffff) ^ 0x3f) |
	    (((context->uc_mcontext.fpregs->sw) & 0xffff) << 16));
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

void
os_restore_fp_control(os_context_t *context)
{
    asm ("fldcw %0" : : "m" (context->uc_mcontext.fpregs->cw));
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

