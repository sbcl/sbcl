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
#if defined LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#endif

os_vm_size_t os_vm_page_size;

#ifdef __NetBSD__
#include <sys/resource.h>
#include <sys/sysctl.h>
#include <string.h>
#include <sys/stat.h> /* For the stat-family wrappers. */
#include <dirent.h>   /* For the opendir()/readdir() wrappers */
#include <sys/socket.h> /* For the socket() wrapper */
static void netbsd_init();
#endif /* __NetBSD__ */

#ifdef __FreeBSD__
#include <sys/sysctl.h>
#if defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_SB_PTHREAD_FUTEX)
#include <sys/umtx.h>
#endif

static void freebsd_init();
#endif /* __FreeBSD__ */

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

void
os_init(char *argv[], char *envp[])
{
    os_vm_page_size = getpagesize();

#ifdef __NetBSD__
    netbsd_init();
#elif defined(__FreeBSD__)
    freebsd_init();
#elif defined(__OpenBSD__)
    openbsd_init();
#endif
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    /* (Unlike most of the other context fields that we access, the
     * signal mask field is a field of the basic, outermost context
     * struct itself both in FreeBSD 4.0 and in OpenBSD 2.6.) */
#if defined(__FreeBSD__)  || defined(__NetBSD__) || defined(LISP_FEATURE_DARWIN)
    return &context->uc_sigmask;
#elif defined (__OpenBSD__)
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
        lose("unexpected mmap(..) failure\n");
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

    if (in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE) ||
        in_range_p(addr, STATIC_SPACE_START, STATIC_SPACE_SIZE) ||
        in_range_p(addr, DYNAMIC_SPACE_START, dynamic_space_size))
        return 1;
    for_each_thread(th) {
        if (((os_vm_address_t)th->control_stack_start <= addr) &&
            (addr < (os_vm_address_t)th->control_stack_end))
            return 1;
        if (in_range_p(addr, (lispobj) th->binding_stack_start,
                       BINDING_STACK_SIZE))
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
memory_fault_handler(int signal, siginfo_t *siginfo, os_context_t *context)
{
    void *fault_addr = arch_get_bad_addr(signal, siginfo, context);

#if defined(LISP_FEATURE_RESTORE_TLS_SEGMENT_REGISTER_FROM_CONTEXT)
    FSHOW_SIGNAL((stderr, "/ TLS: restoring fs: %p in memory_fault_handler\n",
                  *CONTEXT_ADDR_FROM_STEM(fs)));
    os_restore_tls_segment_register(context);
#endif

    FSHOW((stderr, "Memory fault at: %p, PC: %p\n", fault_addr, *os_context_pc_addr(context)));

    if (!gencgc_handle_wp_violation(fault_addr))
        if(!handle_guard_page_triggered(context,fault_addr))
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
#if defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
                                                 mach_error_memory_fault_handler);
#else
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
#ifdef LISP_FEATURE_FREEBSD
                                                 (__siginfohandler_t *)
#endif
                                                 memory_fault_handler);
#endif

#ifdef LISP_FEATURE_SB_THREAD
    undoably_install_low_level_interrupt_handler(SIG_STOP_FOR_GC,
                                                 sig_stop_for_gc_handler);
#endif
    SHOW("leaving os_install_interrupt_handlers()");
}

#else /* Currently PPC/Darwin/Cheney only */

static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
#if 0
    unsigned int pc =  (unsigned int *)(*os_context_pc_addr(context));
#endif
    os_vm_address_t addr;

    addr = arch_get_bad_addr(signal, info, context);
    if (!cheneygc_handle_wp_violation(context, addr))
        if (!handle_guard_page_triggered(context, addr))
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

#ifdef __NetBSD__
static void netbsd_init()
{
    struct rlimit rl;
    int mib[2], osrev;
    size_t len;

    /* Are we running on a sufficiently functional kernel? */
    mib[0] = CTL_KERN;
    mib[1] = KERN_OSREV;

    len = sizeof(osrev);
    sysctl(mib, 2, &osrev, &len, NULL, 0);

    /* If we're older than 2.0... */
    if (osrev < 200000000) {
        fprintf(stderr, "osrev = %d (needed at least 200000000).\n", osrev);
        lose("NetBSD kernel too old to run sbcl.\n");
    }

    /* NetBSD counts mmap()ed space against the process's data size limit,
     * so yank it up. This might be a nasty thing to do? */
    getrlimit (RLIMIT_DATA, &rl);
    /* Amazingly for such a new port, the provenance and meaning of
       this number are unknown.  It might just mean REALLY_BIG_LIMIT,
       or possibly it should be calculated from dynamic space size.
       -- CSR, 2004-04-08 */
    rl.rlim_cur = 1073741824;
    if (setrlimit (RLIMIT_DATA, &rl) < 0) {
        fprintf (stderr,
                 "RUNTIME WARNING: unable to raise process data size limit:\n\
  %s.\n\
The system may fail to start.\n",
                 strerror(errno));
    }
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

/* Used in sb-bsd-sockets. */
int
_socket(int domain, int type, int protocol)
{
    return socket(domain, type, protocol);
}
#endif /* __NetBSD__ */

#ifdef __FreeBSD__
extern int getosreldate(void);

int sig_memory_fault;

static void freebsd_init()
{
    /* Memory fault signal on FreeBSD was changed from SIGBUS to
     * SIGSEGV. */
    if (getosreldate() < 700004)
        sig_memory_fault = SIGBUS;
    else
        sig_memory_fault = SIGSEGV;

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

#if defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_SB_PTHREAD_FUTEX) \
    && !defined(LISP_FEATURE_SB_LUTEX)
int
futex_wait(int *lock_word, long oldval, long sec, unsigned long usec)
{
    struct timespec timeout;
    int ret;

    if (sec < 0)
        ret = umtx_wait((void *)lock_word, oldval, NULL);
    else {
        timeout.tv_sec = sec;
        timeout.tv_nsec = usec * 1000;
        ret = umtx_wait((void *)lock_word, oldval, &timeout);
    }

    switch (ret) {
    case 0:
        return 0;
    case ETIMEDOUT:
        return 1;
    case EINTR:
        return 2;
    default:
        /* EWOULDBLOCK and others, need to check the lock */
        return -1;
    }
}

int
futex_wake(int *lock_word, int n)
{
    return umtx_wake((void *)lock_word, n);
}
#endif
#endif /* __FreeBSD__ */

#ifdef LISP_FEATURE_DARWIN
/* defined in ppc-darwin-os.c instead */
#elif defined(LISP_FEATURE_FREEBSD)
#ifndef KERN_PROC_PATHNAME
#define KERN_PROC_PATHNAME 12
#endif

char *
os_get_runtime_executable_path(int external)
{
    char path[PATH_MAX + 1];

    if (getosreldate() >= 600024) {
        /* KERN_PROC_PATHNAME is available */
        size_t len = PATH_MAX + 1;
        int mib[4];

        mib[0] = CTL_KERN;
        mib[1] = KERN_PROC;
        mib[2] = KERN_PROC_PATHNAME;
        mib[3] = -1;
        if (sysctl(mib, 4, &path, &len, NULL, 0) != 0)
            return NULL;
    } else {
        int size;
        size = readlink("/proc/curproc/file", path, sizeof(path) - 1);
        if (size < 0)
            return NULL;
        path[size] = '\0';
    }
    if (strcmp(path, "unknown") == 0)
        return NULL;
    return copied_string(path);
}
#elif defined(LISP_FEATURE_NETBSD) || defined(LISP_FEATURE_OPENBSD)
char *
os_get_runtime_executable_path(int external)
{
    struct stat sb;
    if (!external && stat("/proc/curproc/file", &sb) == 0)
        return copied_string("/proc/curproc/file");
    return NULL;
}
#else /* Not DARWIN or FREEBSD or NETBSD or OPENBSD */
char *
os_get_runtime_executable_path(int external)
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
#ifdef LISP_FEATURE_X86_64
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
