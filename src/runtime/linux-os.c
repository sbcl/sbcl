/*
 * the Linux incarnation of OS-dependent routines.  See also
 * $(sbcl_arch)-linux-os.c
 *
 * This file (along with os.h) exports an OS-independent interface to
 * the operating system VM facilities. Surprise surprise, this
 * interface looks a lot like the Mach interface (but simpler in some
 * places). For some operating systems, a subset of these functions
 * will have to be emulated.
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
#include <sys/socket.h>
#include <sys/utsname.h>

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <linux/version.h>

#include "validate.h"
#include "thread.h"
size_t os_vm_page_size;

#include "gc.h"

int linux_sparc_siginfo_bug = 0;

void os_init(void)
{
    /* Conduct various version checks: do we have enough mmap(), is
     * this a sparc running 2.2, can we do threads? */
    {
        struct utsname name;
	int major_version;
	int minor_version;
	char *p;
	uname(&name);
	p=name.release;  
	major_version = atoi(p);
	p=strchr(p,'.')+1;
	minor_version = atoi(p);
	if (major_version<2) {
	    lose("linux kernel version too old: major version=%d (can't run in version < 2.0.0)",
		 major_version);
	}
#ifdef LISP_FEATURE_SB_THREAD
	if ((major_version <2) || (major_version==2 && minor_version < 4)) {
	    lose("linux kernel 2.4 required for thread-enabled SBCL");
	}
#endif
#ifdef LISP_FEATURE_SPARC
	if ((major_version <2) || (major_version==2 && minor_version < 4)) {
	    FSHOW((stderr,"linux kernel %d.%d predates 2.4;\n enabling workarounds for SPARC kernel bugs in signal handling.\n", major_version,minor_version));
	    linux_sparc_siginfo_bug = 1;
	}
#endif
    }

    os_vm_page_size = getpagesize();
    /* This could just as well be in arch_init(), but it's not. */
#ifdef __i386__
    /* FIXME: This used to be here.  However, I have just removed it
       with no apparent ill effects (it may be that earlier kernels
       started up a process with a different set of traps, or
       something?) Find out what this was meant to do, and reenable it
       or delete it if possible. -- CSR, 2002-07-15 */
    /* SET_FPU_CONTROL_WORD(0x1372|4|8|16|32);  no interrupts */
#endif
}


#ifdef LISP_FEATURE_ALPHA
/* The Alpha is a 64 bit CPU.  SBCL is a 32 bit application.  Due to all
 * the places that assume we can get a pointer into a fixnum with no 
 * information loss, we have to make sure it allocates all its ram in the
 * 0-2Gb region.  */

static void * under_2gb_free_pointer=DYNAMIC_1_SPACE_END;
#endif

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags =  MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE;
    os_vm_address_t actual ;

    if (addr) 
	flags |= MAP_FIXED;
#ifdef LISP_FEATURE_ALPHA
    else {
	flags |= MAP_FIXED;
	addr=under_2gb_free_pointer;
    }
#endif	
    actual = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);
    if (actual == MAP_FAILED ||	(addr && (addr!=actual))) {
	perror("mmap");
	return 0;		/* caller should check this */
    }

#ifdef LISP_FEATURE_ALPHA

    len=(len+(os_vm_page_size-1))&(~(os_vm_page_size-1));
    under_2gb_free_pointer+=len;
#endif

    return actual;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap(addr,len) == -1) {
	perror("munmap");
    }
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

/* FIXME: Now that FOO_END, rather than FOO_SIZE, is the fundamental
 * description of a space, we could probably punt this and just do
 * (FOO_START <= x && x < FOO_END) everywhere it's called. */
static boolean
in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
    char* beg = (char*)((long)sbeg);
    char* end = (char*)((long)sbeg) + slen;
    char* adr = (char*)a;
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
void
sigsegv_handler(int signal, siginfo_t *info, void* void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    void* fault_addr = (void*)context->uc_mcontext.cr2;
    if (!gencgc_handle_wp_violation(fault_addr)) 
	if(!handle_control_stack_guard_triggered(context,fault_addr))
	    interrupt_handle_now(signal, info, void_context);
}

#else

static void
sigsegv_handler(int signal, siginfo_t *info, void* void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    os_vm_address_t addr;

    addr = arch_get_bad_addr(signal,info,context);
    if (addr != NULL && 
	*os_context_register_addr(context,reg_ALLOC) & (1L<<63)){
	
	/* Alpha stuff: This is the end of a pseudo-atomic section
	 * during which a signal was received.  We must deal with the
	 * pending interrupt (see also interrupt.c,
	 * ../code/interrupt.lisp)
	 */
	/* (how we got here: when interrupting, we set bit 63 in
	 * reg_Alloc.  At the end of the atomic section we tried to
	 * write to reg_ALLOC, got a SIGSEGV (there's nothing mapped
	 * there) so ended up here
	 */
	*os_context_register_addr(context,reg_ALLOC) -= (1L<<63);
	interrupt_handle_pending(context);
    } else {
	if(!interrupt_maybe_gc(signal, info, context))
	    if(!handle_control_stack_guard_triggered(context,addr))
		interrupt_handle_now(signal, info, context);
    }
}
#endif

void sigcont_handler(int signal, siginfo_t *info, void *void_context)
{
    /* we need to have a handler installed for this signal so that
     * sigwaitinfo() for it actually returns at the appropriate time
     */
}

void
os_install_interrupt_handlers(void)
{
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
						 sigsegv_handler);
    undoably_install_low_level_interrupt_handler(SIGCONT,
						 sigcont_handler);
}

