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

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/proc.h>
#include "validate.h"
vm_size_t os_vm_page_size;

#if defined GENCGC
#include "gencgc.h"
#endif

/* The different BSD variants have diverged in exactly where they
 * store signal context information, but at least they tend to use the
 * same stems to name the structure fields, so by using this macro we
 * can share a fair amount of code between different variants. */
#if defined __FreeBSD__
#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext.mc_ ## stem
#elif defined __OpenBSD__
#define CONTEXT_ADDR_FROM_STEM(stem) &context->sc_ ## stem
#else
#error unsupported BSD variant
#endif

void
os_init(void)
{
    os_vm_page_size = getpagesize();
}

/* KLUDGE: There is strong family resemblance in the signal context
 * stuff in FreeBSD and OpenBSD, but in detail they're different in
 * almost every line of code. It would be nice to find some way to
 * factor out the commonality better; failing that, it might be best
 * just to split this generic-BSD code into one variant for each BSD. */
   
int *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case  0:
	return CONTEXT_ADDR_FROM_STEM(eax);
    case  2:
	return CONTEXT_ADDR_FROM_STEM(ecx);
    case  4:
	return CONTEXT_ADDR_FROM_STEM(edx);
    case  6:
	return CONTEXT_ADDR_FROM_STEM(ebx);
    case  8:
	return CONTEXT_ADDR_FROM_STEM(esp);
    case 10:
	return CONTEXT_ADDR_FROM_STEM(ebp);
    case 12:
	return CONTEXT_ADDR_FROM_STEM(esi);
    case 14:
	return CONTEXT_ADDR_FROM_STEM(edi);
    default:
	return 0;
    }
}

int *
os_context_pc_addr(os_context_t *context)
{
#if defined __FreeBSD__
    return CONTEXT_ADDR_FROM_STEM(eip);
#elif defined __OpenBSD__
    return CONTEXT_ADDR_FROM_STEM(pc);
#else
#error unsupported BSD variant
#endif
}

int *
os_context_sp_addr(os_context_t *context)
{
    return CONTEXT_ADDR_FROM_STEM(esp);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    /* (Unlike most of the other context fields that we access, the
     * signal mask field is a field of the basic, outermost context
     * struct itself both in FreeBSD 4.0 and in OpenBSD 2.6.) */
#if defined __FreeBSD__
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

/* FIXME: If this can be a no-op on BSD/x86, then it 
 * deserves a more precise name.
 *
 * (Perhaps os_prepare_data_area_to_be_executed()?) */
void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
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
    return in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE)
	|| in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE   )
	|| in_range_p(addr, DYNAMIC_SPACE_START  , DYNAMIC_SPACE_SIZE  )
	|| in_range_p(addr, CONTROL_STACK_START  , CONTROL_STACK_SIZE  )
	|| in_range_p(addr, BINDING_STACK_START  , BINDING_STACK_SIZE  );
}

/*
 * any OS-dependent special low-level handling for signals
 */

#if !defined GENCGC

void
os_install_interrupt_handlers(void)
{
    SHOW("os_install_interrupt_handlers()/bsd-os/!defined(GENCGC)");
}

#else

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

#endif /* !defined GENCGC */
