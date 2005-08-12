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
#include "sbcl.h"
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "runtime.h"
#include "genesis/static-symbols.h"
#include "genesis/fdefn.h"
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
#include "gc.h"
#if defined LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#endif

size_t os_vm_page_size;

#ifdef LISP_FEATURE_SB_THREAD
#include <sys/syscall.h>
#include <unistd.h>
#include <errno.h>

/* values taken from the kernel's linux/futex.h.  This header file
   doesn't exist in userspace, which is our excuse for not grovelling
   them automatically */
#define FUTEX_WAIT (0)
#define FUTEX_WAKE (1)
#define FUTEX_FD (2)
#define FUTEX_REQUEUE (3)

#define sys_futex sbcl_sys_futex
static inline int sys_futex (void *futex, int op, int val, struct timespec *rel)
{
    return syscall (SYS_futex, futex, op, val, rel);
}

int
futex_wait(int *lock_word, int oldval)
{
    int t= sys_futex(lock_word,FUTEX_WAIT,oldval, 0);
    return t;
}

int
futex_wake(int *lock_word, int n)
{
    return sys_futex(lock_word,FUTEX_WAKE,n,0);
}
#endif


int linux_sparc_siginfo_bug = 0;
int linux_no_threads_p = 0;

void
os_init(void)
{
    /* Conduct various version checks: do we have enough mmap(), is
     * this a sparc running 2.2, can we do threads? */
#ifdef LISP_FEATURE_SB_THREAD
    int *futex=0;
#endif
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
    if (!(major_version>2 || minor_version >= 4)) {
#ifdef LISP_FEATURE_SPARC
        FSHOW((stderr,"linux kernel %d.%d predates 2.4;\n enabling workarounds for SPARC kernel bugs in signal handling.\n", major_version,minor_version));
        linux_sparc_siginfo_bug = 1;
#endif
    }
#ifdef LISP_FEATURE_SB_THREAD
    futex_wait(futex,-1);
    if(errno==ENOSYS)  linux_no_threads_p = 1;
    if(linux_no_threads_p)
        fprintf(stderr,"Linux with NPTL support (e.g. kernel 2.6 or newer) required for \nthread-enabled SBCL.  Disabling thread support.\n\n");
#endif
    os_vm_page_size = getpagesize();
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
    os_vm_address_t actual;

#ifdef LISP_FEATURE_ALPHA
    if (!addr) {
        addr=under_2gb_free_pointer;
    }
#endif
    actual = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);
    if (actual == MAP_FAILED || (addr && (addr!=actual))) {
        perror("mmap");
        return 0;               /* caller should check this */
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
    os_vm_address_t actual;

    actual = mmap(addr, len, OS_VM_PROT_ALL, MAP_PRIVATE | MAP_FIXED,
                  fd, (off_t) offset);
    if (actual == MAP_FAILED || (addr && (addr != actual))) {
        perror("mmap");
        lose("unexpected mmap(..) failure");
    }

    return actual;
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect(address, length, prot) == -1) {
        perror("mprotect");
    }
}

boolean
is_valid_lisp_addr(os_vm_address_t addr)
{
    struct thread *th;
    size_t ad = (size_t) addr;

    if ((READ_ONLY_SPACE_START <= ad && ad < READ_ONLY_SPACE_END)
        || (STATIC_SPACE_START <= ad && ad < STATIC_SPACE_END)
#if defined LISP_FEATURE_GENCGC
        || (DYNAMIC_SPACE_START <= ad && ad < DYNAMIC_SPACE_END)
#else
        || (DYNAMIC_0_SPACE_START <= ad && ad < DYNAMIC_0_SPACE_END)
        || (DYNAMIC_1_SPACE_START <= ad && ad < DYNAMIC_1_SPACE_END)
#endif
        )
        return 1;
    for_each_thread(th) {
        if((size_t)(th->control_stack_start) <= ad
           && ad < (size_t)(th->control_stack_end))
            return 1;
        if((size_t)(th->binding_stack_start) <= ad
           && ad < (size_t)(th->binding_stack_start + BINDING_STACK_SIZE))
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
sigsegv_handler(int signal, siginfo_t *info, void* void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    void* fault_addr = (void*)info->si_addr;
    if (!gencgc_handle_wp_violation(fault_addr))
        if(!handle_guard_page_triggered(context,fault_addr))
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
            arrange_return_to_lisp_function(context, SymbolFunction(MEMORY_FAULT_ERROR));
#else
            interrupt_handle_now(signal, info, context);
#endif
}

#else

static void
sigsegv_handler(int signal, siginfo_t *info, void* void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    os_vm_address_t addr = arch_get_bad_addr(signal,info,context);

#ifdef LISP_FEATURE_ALPHA
    /* Alpha stuff: This is the end of a pseudo-atomic section during
       which a signal was received.  We must deal with the pending
       interrupt (see also interrupt.c, ../code/interrupt.lisp)

       (how we got here: when interrupting, we set bit 63 in reg_ALLOC.
       At the end of the atomic section we tried to write to reg_ALLOC,
       got a SIGSEGV (there's nothing mapped there) so ended up here. */
    if (addr != NULL &&
        *os_context_register_addr(context,reg_ALLOC) & (1L<<63)){
        *os_context_register_addr(context,reg_ALLOC) -= (1L<<63);
        interrupt_handle_pending(context);
        return;
    }
#endif

    if(!interrupt_maybe_gc(signal, info, context))
        if(!handle_guard_page_triggered(context,addr))
            interrupt_handle_now(signal, info, context);
}
#endif

void
os_install_interrupt_handlers(void)
{
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
                                                 sigsegv_handler);
#ifdef LISP_FEATURE_SB_THREAD
    undoably_install_low_level_interrupt_handler(SIG_INTERRUPT_THREAD,
                                                 interrupt_thread_handler);
    undoably_install_low_level_interrupt_handler(SIG_STOP_FOR_GC,
                                                 sig_stop_for_gc_handler);
#endif
}
