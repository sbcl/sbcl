/*
 * the Linux incarnation of OS-dependent routines
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

/*
 * $Header$
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
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

#include "x86-validate.h"
size_t os_vm_page_size;

#if defined GENCGC
#include "gencgc.h"
#endif

void os_init(void)
{
    /* Early versions of Linux don't support the mmap(..) functionality
     * that we need. */
    {
        struct utsname name;
	int major_version;
	uname(&name);
	major_version = atoi(name.release);
	if (major_version < 2) {
	    lose("linux major version=%d (can't run in version < 2.0.0)",
		 major_version);
	}
    }

    os_vm_page_size = getpagesize();

    SET_FPU_CONTROL_WORD(0x1372|4|8|16|32); /* no interrupts */
}

/* KLUDGE: As of kernel 2.2.14 on Red Hat 6.2, there's code in the
 * <sys/ucontext.h> file to define symbolic names for offsets into
 * gregs[], but it's conditional on __USE_GNU and not defined, so
 * we need to do this nasty absolute index magic number thing
 * instead. */
int *
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
}
int *
os_context_pc_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[14];
}
int *
os_context_sp_addr(os_context_t *context)
{
    return &context->uc_mcontext.gregs[17];
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    return &context->uc_sigmask;
}

/* In Debian CMU CL ca. 2.4.9, it was possible to get an infinite
 * cascade of errors from do_mmap(..). This variable is a counter to
 * prevent that; when it counts down to zero, an error in do_mmap
 * causes the low-level monitor to be called. */
int n_do_mmap_ignorable_errors = 3;

/* Return 0 for success. */
static int
do_mmap(os_vm_address_t *addr, os_vm_size_t len, int flags)
{
    /* We *must* have the memory where we want it. */
    os_vm_address_t old_addr=*addr;

    *addr = mmap(*addr, len, OS_VM_PROT_ALL, flags, -1, 0);
    if (*addr == MAP_FAILED ||
	((old_addr != NULL) && (*addr != old_addr))) {
        FSHOW((stderr,
	       "error in allocating memory from the OS\n"
	       "(addr=%lx, len=%lx, flags=%lx)\n",
	       (long) addr,
	       (long) len,
	       (long) flags));
	if (n_do_mmap_ignorable_errors > 0) {
	    --n_do_mmap_ignorable_errors;
	} else {
	    lose("too many errors in allocating memory from the OS");
	}
	perror("mmap");
	return 1;
    }
    return 0;
}

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    if (addr) {
	int flags = MAP_PRIVATE | MAP_ANONYMOUS | MAP_FIXED;
	os_vm_address_t base_addr = addr;
	do {
	    /* KLUDGE: It looks as though this code allocates memory
	     * in chunks of size no larger than 'magic', but why? What
	     * is the significance of 0x1000000 here? Also, can it be
	     * right that if the first few 'do_mmap' calls succeed,
	     * then one fails, we leave the memory allocated by the
	     * first few in place even while we return a code for
	     * complete failure? -- WHN 19991020
	     *
	     * Peter Van Eynde writes (20000211)
	     *     This was done because the kernel would only check for
	     *   overcommit for every allocation seperately. So if you
	     *   had 16MB of free mem+swap you could allocate 16M. And
	     *   again, and again, etc. 
	     *     This in [Linux] 2.X could be bad as they changed the memory
	     *   system. A side effect was/is (I don't really know) that
	     *   programs with a lot of memory mappings run slower. But
	     *   of course for 2.2.2X we now have the NO_RESERVE flag that
	     *   helps...
	     *
	     * FIXME: The logic is also flaky w.r.t. failed
	     * allocations. If we make one or more successful calls to
	     * do_mmap(..) before one fails, then we've allocated
	     * memory, and we should ensure that it gets deallocated
	     * sometime somehow. If this function's response to any
	     * failed do_mmap(..) is to give up and return NULL (as in
	     * sbcl-0.6.7), then any failed do_mmap(..) after any
	     * successful do_mmap(..) causes a memory leak. */
	    int magic = 0x1000000;
	    if (len <= magic) {
		if (do_mmap(&addr, len, flags)) {
		    return NULL;
		}
		len = 0;
	    } else {
		if (do_mmap(&addr, magic, flags)) {
		    return NULL;
		}
		addr += magic;
		len = len - magic;
	    }
	} while (len > 0);
	return base_addr;
    } else {
	int flags = MAP_PRIVATE | MAP_ANONYMOUS;
	if (do_mmap(&addr, len, flags)) {
	    return NULL;
	} else {
	    return addr;
	}
    }
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

    if(addr == MAP_FAILED) {
	perror("mmap");
	lose("unexpected mmap(..) failure");
    }

    return addr;
}

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
    char* beg = (char*)sbeg;
    char* end = (char*)sbeg + slen;
    char* adr = (char*)a;
    return (adr >= beg && adr < end);
}

boolean
is_valid_lisp_addr(os_vm_address_t addr)
{
    return
	in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE) ||
	in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE) ||
	in_range_p(addr, DYNAMIC_SPACE_START  , DYNAMIC_SPACE_SIZE) ||
	in_range_p(addr, CONTROL_STACK_START  , CONTROL_STACK_SIZE) ||
	in_range_p(addr, BINDING_STACK_START  , BINDING_STACK_SIZE);
}

/*
 * any OS-dependent special low-level handling for signals
 */

#if !defined GENCGC

void
os_install_interrupt_handlers(void)
{}

#else

/*
 * The GENCGC needs to be hooked into whatever signal is raised for
 * page fault on this OS.
 */
void
sigsegv_handler(int signal, siginfo_t *info, void* void_context)
{
    os_context_t *context = (os_context_t*)void_context;
    void* fault_addr = (void*)context->uc_mcontext.cr2;
    if (!gencgc_handle_wp_violation(fault_addr)) {
	interrupt_handle_now(signal, info, void_context);
    }
}
void
os_install_interrupt_handlers(void)
{
    interrupt_install_low_level_handler(SIGSEGV, sigsegv_handler);
}

#endif
