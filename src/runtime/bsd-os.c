/*
 * OS-dependent routines for BSD-ish systems
 *
 * This file (along with os.h) exports an OS-independent interface to
 * the operating system VM facilities. This interface looks a lot like
 * the Mach interface (but simpler in some places). For some operating
 * systems, a subset of these functions will have to be emulated.
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
#include <sys/param.h>
#include <sys/file.h>
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "sbcl.h"
#include "thread.h"

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include "validate.h"


vm_size_t os_vm_page_size;

void os_init(void)
{
    os_vm_page_size = getpagesize();
}

int *os_context_pc_addr(os_context_t *context)
{
#if defined __FreeBSD__
    return CONTEXT_ADDR_FROM_STEM(eip);
#elif defined __OpenBSD__
    return CONTEXT_ADDR_FROM_STEM(pc);
#elif defined DARWIN
    return &context->uc_mcontext->ss.srr0;
#else
#error unsupported BSD variant
#endif
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    /* (Unlike most of the other context fields that we access, the
     * signal mask field is a field of the basic, outermost context
     * struct itself both in FreeBSD 4.0 and in OpenBSD 2.6.) */
#if defined __FreeBSD__ || defined DARWIN
    return &context->uc_sigmask;
#elif defined __OpenBSD__
    return &context->sc_mask;
#else
#error unsupported BSD variant
#endif
}

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_ANON;

    if (addr)
	flags |= MAP_FIXED;

    addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (addr == MAP_FAILED) {
	perror("mmap");
	return NULL;
    }

    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap(addr, len) == -1)
	perror("munmap");
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    addr = mmap(addr, len,
		OS_VM_PROT_ALL,
		MAP_PRIVATE | MAP_FILE | MAP_FIXED,
		fd, (off_t) offset);

    if (addr == MAP_FAILED) {
	perror("mmap");
	lose("unexpected mmap(..) failure");
    }

    return addr;
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect(address, length, prot) == -1) {
	perror("mprotect");
    }
}

static boolean
in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
    char* beg = (char*) sbeg;
    char* end = (char*) sbeg + slen;
    char* adr = (char*) a;
    return (adr >= beg && adr < end);
}

boolean
is_valid_lisp_addr(os_vm_address_t addr)
{
    struct thread *th;
    if(in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE) ||
       in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE) ||
       in_range_p(addr, DYNAMIC_SPACE_START  , DYNAMIC_SPACE_SIZE))
	return 1;
    for_each_thread(th) {
	if((th->control_stack_start <= addr) && (addr < th->control_stack_end))
	    return 1;
	if(in_range_p(addr, th->binding_stack_start, BINDING_STACK_SIZE))
	    return 1;
    }
    return 0;
}

/*
 * any OS-dependent special low-level handling for signals
 */

#if defined LISP_FEATURE_GENCGC

/*
 * The GENCGC needs to be hooked into whatever signal is raised for
 * page fault on this OS.
 */
static void
memory_fault_handler(int signal, siginfo_t *siginfo, void *void_context)
{
    /* The way that we extract low level information like the fault
     * address is not specified by POSIX. */
#if defined __FreeBSD__
    void *fault_addr = siginfo->si_addr;
#elif defined __OpenBSD__
    void *fault_addr = siginfo->si_addr;
#elif defined DARWIN
    void *fault_addr = siginfo->si_addr;
#else
#error unsupported BSD variant
#endif
    os_context_t *context = arch_os_get_context(&void_context);
    if (!gencgc_handle_wp_violation(fault_addr)) 
        if(!handle_control_stack_guard_triggered(context,fault_addr))
	    /* FIXME is this context or void_context?  not that it */
	    /* makes a difference currently except on linux/sparc */
	    interrupt_handle_now(signal, siginfo, void_context);
}
void
os_install_interrupt_handlers(void)
{
    SHOW("os_install_interrupt_handlers()/bsd-os/defined(GENCGC)");
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
						 memory_fault_handler);
    SHOW("leaving os_install_interrupt_handlers()");
}

#else /* Currently Darwin only */

static void
sigsegv_handler(int signal, siginfo_t *info, void* void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    unsigned int pc =  (unsigned int *)(*os_context_pc_addr(context));
    os_vm_address_t addr;
    
    addr = arch_get_bad_addr(signal,info,context);
    if(!interrupt_maybe_gc(signal, info, context))
	if(!handle_control_stack_guard_triggered(context,addr))
	    interrupt_handle_now(signal, info, context);
}

void
os_install_interrupt_handlers(void)
{
    SHOW("os_install_interrupt_handlers()/bsd-os/!defined(GENCGC)");
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
						 sigsegv_handler);
}

#endif /* defined GENCGC */

/* threads */

/* no threading in any *BSD variant on any CPU (yet? in sbcl-0.8.0 anyway) */
#ifdef LISP_FEATURE_SB_THREAD
#error "Define threading support functions"
#else
int arch_os_thread_init(struct thread *thread) {
  stack_t sigstack;
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp=((void *) thread)+dynamic_values_bytes;
    sigstack.ss_flags=0;
    sigstack.ss_size = 32*SIGSTKSZ;
    sigaltstack(&sigstack,0);
#endif
    return 1;                  /* success */
}
int arch_os_thread_cleanup(struct thread *thread) {
    return 1;                  /* success */
}
#endif
