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
#include <unistd.h>
#include <utime.h>
#include <assert.h>
#include <errno.h>
#include "sbcl.h"
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "interr.h"
#include "lispregs.h"
#include "thread.h"
#include "runtime.h"
#include "genesis/static-symbols.h"
#include "genesis/fdefn.h"

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include "validate.h"
#include "gc-internal.h"



#ifdef __NetBSD__
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <string.h>
#include <sys/stat.h> /* For the stat-family wrappers. */
#include <dirent.h>   /* For the opendir()/readdir() wrappers */
#include <sys/socket.h> /* For the socket() wrapper */
static void netbsd_init();
static os_vm_size_t max_allocation_size;
#endif /* __NetBSD__ */

#if defined LISP_FEATURE_FREEBSD
#include <sys/sysctl.h>
#ifdef LISP_FEATURE_SB_FUTEX
#include <sys/umtx.h>
#endif

static void freebsd_init();
#endif /* __FreeBSD__ */

#ifdef __DragonFly__
#include <sys/sysctl.h>

static void dragonfly_init();
#endif /* __DragonFly__ */

#ifdef __OpenBSD__
#include <sys/types.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <sys/sysctl.h>
#include <dlfcn.h>
#ifdef LISP_FEATURE_X86
#include <machine/cpu.h>
#endif

static void openbsd_init();
#endif

void os_init()
{
#ifdef __NetBSD__
    netbsd_init();
#elif defined(LISP_FEATURE_FREEBSD)
    freebsd_init();
#elif defined(__OpenBSD__)
    openbsd_init();
#elif defined(LISP_FEATURE_DARWIN)
    darwin_init();
#elif defined(__DragonFly__)
    dragonfly_init();
#endif
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    /* (Unlike most of the other context fields that we access, the
     * signal mask field is a field of the basic, outermost context
     * struct itself both in FreeBSD 4.0 and in OpenBSD 2.6.) */
#if defined(LISP_FEATURE_FREEBSD) || defined(__NetBSD__) || defined(LISP_FEATURE_DARWIN) \
    || defined(__DragonFly__)
    return &context->uc_sigmask;
#elif defined (__OpenBSD__)
    return &context->sc_mask;
#else
#error unsupported BSD variant
#endif
}

os_vm_address_t
os_validate(int attributes, os_vm_address_t addr, os_vm_size_t len, int executable, int jit)
{
    int protection;
    int flags = 0;

    if (attributes & IS_GUARD_PAGE)
        protection = OS_VM_PROT_NONE;
    else
#ifndef LISP_FEATURE_DARWIN_JIT
        protection = OS_VM_PROT_ALL;
#else
    if (jit) {
        if (jit == 2)
            protection = OS_VM_PROT_ALL;
        else
            protection = OS_VM_PROT_READ | OS_VM_PROT_WRITE;
        flags = MAP_JIT;
    }
    else if (executable) {
        protection = OS_VM_PROT_READ | OS_VM_PROT_EXECUTE;
    }
    else {
        protection = OS_VM_PROT_READ | OS_VM_PROT_WRITE;
    }
#endif

    attributes &= ~IS_GUARD_PAGE;

#ifndef LISP_FEATURE_DARWIN // Do not use MAP_FIXED, because the OS is sane.

    /* The *BSD family of OSes seem to ignore 'addr' when it is outside
     * of some range which I could not figure out.  Sometimes it seems like the
     * condition is that any address below 4GB can't be requested without MAP_FIXED,
     * but on the other hand, asking for 0x1000 without MAP_FIXED works fine.
     * So we are forced to use MAP_FIXED even for movable mappings. Thus, the logic
     * below does not work as intended because:
     * (1) We can't detect when MAP_FIXED destroyed an existing mapping.
     *     But we can avoid the destruction by using MAP_EXCL when that flag exists,
     *     which it does not always.
     * (2) Passing MAP_FIXED when we do not require a fixed address gets MAP_FAILURE
     *     for mappings that we would have been willing to relocate.
     *     So relocation is effectively disabled.
     * (3) To mitigate the problem of point (2) we remove MAP_FIXED for the
     *     dynamic space which seems to mostly work, but might cause an opposite
     *     problem: we relocate the heap when perhaps we need not have.
     *     That is, even if the OS _could_ _have_ given the address requested,
     *     it randomly decided not to, thus forcing extra work upon us.

     * For reference,
     * FreeBSD says:
       If MAP_EXCL is not specified, a successful MAP_FIXED request replaces any
       previous mappings for the process' pages ...

     * OpenBSD says:
       Except for MAP_FIXED mappings, the system will never replace existing mappings. */

    // ALLOCATE_LOW seems never to get what we want
    if (!(attributes & MOVABLE) || (attributes & ALLOCATE_LOW)) {
        flags = MAP_FIXED;
    }
    if (attributes & IS_THREAD_STRUCT) {
#if defined(LISP_FEATURE_OPENBSD) && defined(MAP_STACK)
        /* OpenBSD requires MAP_STACK for pages used as stack.
         * Note that FreeBSD has a MAP_STACK with different behavior. */
        flags = MAP_STACK;
#endif
    }
#endif

#ifdef MAP_EXCL // not defined in OpenBSD, NetBSD, DragonFlyBSD
    if (flags & MAP_FIXED) flags |= MAP_EXCL;
#endif
    flags |= MAP_PRIVATE | MAP_ANON;

#ifdef __NetBSD__
    if (addr) {
        os_vm_address_t curaddr = addr;

        while (len > 0) {
            os_vm_address_t resaddr;
            os_vm_size_t curlen = MIN(max_allocation_size, len);

            resaddr = mmap(curaddr, curlen, protection, flags, -1, 0);

            if (resaddr == (os_vm_address_t) - 1) {
                perror("mmap");

                while (curaddr > addr) {
                    curaddr -= max_allocation_size;
                    munmap(curaddr, max_allocation_size);
                }

                return NULL;
            }

            curaddr += curlen;
            len -= curlen;
        }
    } else
#endif
    {
        os_vm_address_t requested = addr;
        addr = mmap(addr, len, protection, flags, -1, 0);
        if (requested && requested != addr && !(attributes & MOVABLE)) {
            return 0;
        }

    }

    if (addr == MAP_FAILED) {
#ifdef LISP_FEATURE_OPENBSD
        if (errno == ENOTSUP)
            fprintf(stderr, "RWX mmap not supported, is the current filesystem"
                    " mounted with wxallowed?\n");
        else
#endif
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

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect(address, length, prot) == -1) {

        perror("mprotect");
#ifdef LISP_FEATURE_DARWIN_JIT
        lose("%p %lu", address, length);
#endif
    }
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
memory_fault_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    void *fault_addr = arch_get_bad_addr(signal, siginfo, context);

#if defined(LISP_FEATURE_RESTORE_TLS_SEGMENT_REGISTER_FROM_CONTEXT)
    FSHOW_SIGNAL((stderr, "/ TLS: restoring fs: %p in memory_fault_handler\n",
                  *CONTEXT_ADDR_FROM_STEM(fs)));
    os_restore_tls_segment_register(context);
#endif

    FSHOW((stderr, "Memory fault at: %p, PC: %p\n", fault_addr, *os_context_pc_addr(context)));

#ifdef LISP_FEATURE_SB_SAFEPOINT
    if (handle_safepoint_violation(context, fault_addr)) return;
#endif

    if (gencgc_handle_wp_violation(fault_addr)) return;

    if (!handle_guard_page_triggered(context,fault_addr))
            lisp_memory_fault_error(context, fault_addr);
}

#if defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
void
mach_error_memory_fault_handler(int signal, siginfo_t *siginfo,
                                os_context_t *context) {
    lose("Unhandled memory fault. Exiting.");
}
#endif

void
os_install_interrupt_handlers(void)
{
    SHOW("os_install_interrupt_handlers()/bsd-os/defined(GENCGC)");
    if (INSTALL_SIG_MEMORY_FAULT_HANDLER) {
#if defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
    ll_install_handler(SIG_MEMORY_FAULT, mach_error_memory_fault_handler);
#else
    ll_install_handler(SIG_MEMORY_FAULT,
#if defined(LISP_FEATURE_FREEBSD) && !defined(__GLIBC__)
                                                 (__siginfohandler_t *)
#endif
                                                 memory_fault_handler);
#endif

#ifdef LISP_FEATURE_DARWIN
    /* Unmapped pages get this and not SIGBUS. */
    ll_install_handler(SIGSEGV, memory_fault_handler);
#endif

    }
}

#else /* Currently PPC/Darwin/Cheney only */

static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    os_vm_address_t addr;

    addr = arch_get_bad_addr(signal, info, context);
    if (cheneygc_handle_wp_violation(context, addr)) return;

    if (!handle_guard_page_triggered(context, addr))
            interrupt_handle_now(signal, info, context);
}

void
os_install_interrupt_handlers(void)
{
    ll_install_handler(SIG_MEMORY_FAULT, sigsegv_handler);
}

#endif /* defined GENCGC */

#ifdef __NetBSD__
static void netbsd_init()
{
    struct rlimit rl;

    /* NetBSD counts mmap()ed space against the process's data size limit,
     * so yank it up. This might be a nasty thing to do? */
    getrlimit (RLIMIT_DATA, &rl);
    if (rl.rlim_cur < rl.rlim_max) {
        rl.rlim_cur = rl.rlim_max;
        if (setrlimit (RLIMIT_DATA, &rl) < 0) {
            fprintf (stderr,
                     "RUNTIME WARNING: unable to raise process data size limit:\n\
%s.\n\
The system may fail to start.\n",
                     strerror(errno));
        }
    }
    max_allocation_size = (os_vm_size_t)((rl.rlim_cur / 2) &
      ~(32 * 1024 * 1024));

#ifdef LISP_FEATURE_X86
    {
        size_t len;
        int sse;

        len = sizeof(sse);
        if (sysctlbyname("machdep.sse", &sse, &len,
                         NULL, 0) == 0 && sse != 0) {
            /* Use the SSE detector */
            fast_bzero_pointer = fast_bzero_detect;
        }
     }
#endif /* LISP_FEATURE_X86 */
}

/* Various routines in NetBSD's C library are compatibility wrappers
   for old versions. Programs must be processed by the C toolchain in
   order to get up-to-date definitions of such routines. */
/* The stat-family, opendir, and readdir are used only in sb-posix, as
   of 2007-01-16. -- RMK */
int
_stat(const char *path, struct stat *sb)
{
    return stat(path, sb);
}
int
_lstat(const char *path, struct stat *sb)
{
    return lstat(path, sb);
}
int
_fstat(int fd, struct stat *sb)
{
    return fstat(fd, sb);
}

DIR *
_opendir(const char *filename)
{
    return opendir(filename);
}
struct dirent *
_readdir(DIR *dirp)
{
    return readdir(dirp);
}

int
_utime(const char *file, const struct utimbuf *timep)
{
    return utime(file, timep);
}

/* Used in sb-bsd-sockets. */
int
_socket(int domain, int type, int protocol)
{
    return socket(domain, type, protocol);
}
#endif /* __NetBSD__ */

#if defined(LISP_FEATURE_FREEBSD)
#ifndef __GLIBC__
extern int getosreldate(void);
#endif

int sig_memory_fault;

static void freebsd_init()
{
    /* Memory fault signal on FreeBSD was changed from SIGBUS to
     * SIGSEGV. */
#ifdef __GLIBC__
        sig_memory_fault = SIGSEGV;
#else
    if (getosreldate() < 700004)
        sig_memory_fault = SIGBUS;
    else
        sig_memory_fault = SIGSEGV;
#endif

    /* Quote from sbcl-devel (NIIMI Satoshi): "Some OSes, like FreeBSD
     * 4.x with GENERIC kernel, does not enable SSE support even on
     * SSE capable CPUs". Detect this situation and skip the
     * fast_bzero sse/base selection logic that's normally done in
     * x86-assem.S.
     */
#ifdef LISP_FEATURE_X86
    {
        size_t len;
        int instruction_sse;

        len = sizeof(instruction_sse);
        if (sysctlbyname("hw.instruction_sse", &instruction_sse, &len,
                         NULL, 0) == 0 && instruction_sse != 0) {
            /* Use the SSE detector */
            fast_bzero_pointer = fast_bzero_detect;
        }
    }
#endif /* LISP_FEATURE_X86 */
}

#ifdef LISP_FEATURE_SB_FUTEX
int
futex_wait(int *lock_word, long oldval, long sec, unsigned long usec)
{
    struct timespec timeout;
    int ret;

    if (sec < 0)
        ret = _umtx_op((void *)lock_word, UMTX_OP_WAIT, oldval, 0, 0);
    else {
        timeout.tv_sec = sec;
        timeout.tv_nsec = usec * 1000;
        ret = _umtx_op((void *)lock_word, UMTX_OP_WAIT, oldval, (void*)sizeof timeout, &timeout);
    }
    if (ret == 0) return 0;
    // technically we would not need to check any of the error codes if the lisp side
    // could just avoid looping if there is no time remaining.
    if (errno == ETIMEDOUT) return 1;
    if (errno == EINTR) return 2;
    return -1;
}

int
futex_wake(int *lock_word, int n)
{
    return _umtx_op((void *)lock_word, UMTX_OP_WAKE, n, 0, 0);
}
#endif
#endif /* __FreeBSD__ */

#ifdef __DragonFly__
static void dragonfly_init()
{
#ifdef LISP_FEATURE_X86
    size_t len;
    int instruction_sse;

    len = sizeof(instruction_sse);
    if (sysctlbyname("hw.instruction_sse", &instruction_sse, &len,
                     NULL, 0) == 0 && instruction_sse != 0) {
        /* Use the SSE detector */
        fast_bzero_pointer = fast_bzero_detect;
    }
#endif /* LISP_FEATURE_X86 */
}


#ifdef LISP_FEATURE_SB_FUTEX
int
futex_wait(int *lock_word, long oldval, long sec, unsigned long usec)
{
    int ret;

    if (sec < 0)
        ret = umtx_sleep(lock_word, oldval, 0);
    else {
        int count = usec + 1000000 * sec;
        ret = umtx_sleep(lock_word, oldval, count);
    }

    if (ret == 0) return 0;
    else {
        switch (errno) {
        case EWOULDBLOCK: // Operation timed out
            return 1;
        case EINTR:
            return 2;
        default: // Such as EINVAL or EBUSY
            return -1;
        }
    }
}

int
futex_wake(int *lock_word, int n)
{
    return umtx_wakeup(lock_word, n);
}
#endif
#endif /* __DragonFly__ */

#ifdef LISP_FEATURE_DARWIN
/* defined in darwin-os.c instead */
#elif defined(LISP_FEATURE_FREEBSD)
#ifndef KERN_PROC_PATHNAME
#define KERN_PROC_PATHNAME 12
#endif

char *os_get_runtime_executable_path()
{
    char path[PATH_MAX + 1];

    size_t len = PATH_MAX + 1;
    int mib[4];

    mib[0] = CTL_KERN;
    mib[1] = KERN_PROC;
    mib[2] = KERN_PROC_PATHNAME;
    mib[3] = -1;
    if (sysctl(mib, 4, &path, &len, NULL, 0) != 0)
        return NULL;

    return copied_string(path);
}
#elif defined(LISP_FEATURE_DRAGONFLY) || defined(LISP_FEATURE_NETBSD)
char *os_get_runtime_executable_path()
{
    char path[PATH_MAX + 1];
    int size = readlink("/proc/curproc/"
#ifdef LISP_FEATURE_NETBSD
                        "exe"
#else
                        "file"
#endif
                        , path, sizeof(path) - 1);
    if (size < 0)
        return NULL;
    path[size] = '\0';

    if (strcmp(path, "unknown") == 0)
        return NULL;
    return copied_string(path);
}
#else /* Not DARWIN or FREEBSD or NETBSD or DragonFly */
char *
os_get_runtime_executable_path()
{
    return NULL;
}
#endif

#ifdef __OpenBSD__

int openbsd_use_fxsave = 0;

void
openbsd_init()
{
#ifdef LISP_FEATURE_X86
    int mib[2];
    size_t size;
#endif
    /*
     * Show a warning if it looks like the memory available after
     * allocating the spaces won't be at least this much.
     */
#ifdef LISP_FEATURE_64_BIT
    const int wantfree = 64 * 1024 * 1024;
#else
    const int wantfree = 32 * 1024 * 1024;
#endif
    struct rlimit rl;

#ifdef LISP_FEATURE_X86
    /* Save the machdep.osfxsr sysctl for use by os_restore_fp_control() */
    mib[0] = CTL_MACHDEP;
    mib[1] = CPU_OSFXSR;
    size = sizeof (openbsd_use_fxsave);
    sysctl(mib, 2, &openbsd_use_fxsave, &size, NULL, 0);
    if (openbsd_use_fxsave)
        /* Use the SSE detector */
        fast_bzero_pointer = fast_bzero_detect;
#endif

    /* OpenBSD, like NetBSD, counts mmap()ed space against the
     * process's data size limit. If the soft limit is lower than the
     * hard limit then try to yank it up, this lets users in the
     * "staff" or "daemon" login classes run sbcl with larger dynamic
     * space sizes.
     */
    getrlimit (RLIMIT_DATA, &rl);
    if (rl.rlim_cur < rl.rlim_max) {
        rl.rlim_cur = rl.rlim_max;
        if (setrlimit (RLIMIT_DATA, &rl) < 0) {
            fprintf (stderr,
                     "RUNTIME WARNING: unable to raise process data size limit:\n\
  %s.\n\
The system may fail to start.\n",
                     strerror(errno));
        }
    }

    /*
     * Display a (hopefully) helpful warning if it looks like we won't
     * be able to allocate enough memory.
     */
    getrlimit (RLIMIT_DATA, &rl);
    if (dynamic_space_size + READ_ONLY_SPACE_SIZE + STATIC_SPACE_SIZE +
        LINKAGE_TABLE_SPACE_SIZE + wantfree > rl.rlim_cur)
        fprintf (stderr,
                 "RUNTIME WARNING: data size resource limit may be too low,\n"
                 "  try decreasing the dynamic space size with --dynamic-space-size\n"
                 "  or raising the datasize or datasize-max limits in /etc/login.conf\n");
}

/* OpenBSD's dlsym() relies on the gcc bulitin
 * __builtin_return_address(0) returning an address in the
 * executable's text segment, but when called from lisp it will return
 * an address in the dynamic space.  Work around this by calling this
 * wrapper function instead. Note that tail-call optimization will
 * defeat this, disable it by saving the dlsym() return value in a
 * volatile variable.
*/
void *
os_dlsym(void *handle, const char *symbol)
{
    void * volatile ret = dlsym(handle, symbol);
    return ret;
}

#endif
