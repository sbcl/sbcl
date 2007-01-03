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

static void netbsd_init();
#endif /* __NetBSD__ */

#ifdef __FreeBSD__
#include <sys/sysctl.h>

static void freebsd_init();
#endif /* __FreeBSD__ */

void
os_init(char *argv[], char *envp[])
{
    os_vm_page_size = getpagesize();

#ifdef __NetBSD__
    netbsd_init();
#elif defined(__FreeBSD__)
    freebsd_init();
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
    if(in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE) ||
       in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE) ||
       in_range_p(addr, DYNAMIC_SPACE_START  , dynamic_space_size))
        return 1;
    for_each_thread(th) {
        if(((os_vm_address_t)th->control_stack_start <= addr) &&
           (addr < (os_vm_address_t)th->control_stack_end))
            return 1;
        if(in_range_p(addr, (lispobj)th->binding_stack_start,
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
memory_fault_handler(int signal, siginfo_t *siginfo, void *void_context
#if defined(LISP_FEATURE_FREEBSD) && defined(LISP_FEATURE_X86_64)
/* FreeBSD/amd64 stores fault address only in undocumented 4th arg. */
                     ,void *fault_addr
#endif
    )
{
    os_context_t *context = arch_os_get_context(&void_context);
#if defined(LISP_FEATURE_FREEBSD) && defined(LISP_FEATURE_X86_64)
    /* KLUDGE: Store fault address into si_addr for compatibilities. */
    siginfo->si_addr = fault_addr;
#else
    void *fault_addr = arch_get_bad_addr(signal, siginfo, context);
#endif

#if defined(LISP_FEATURE_RESTORE_TLS_SEGMENT_REGISTER_FROM_CONTEXT)
    FSHOW_SIGNAL((stderr, "/ TLS: restoring fs: %p in memory_fault_handler\n",
                  *CONTEXT_ADDR_FROM_STEM(fs)));
    os_restore_tls_segment_register(context);
#endif

    FSHOW((stderr, "Memory fault at: %p, PC: %p\n", fault_addr, *os_context_pc_addr(context)));

    if (!gencgc_handle_wp_violation(fault_addr))
        if(!handle_guard_page_triggered(context,fault_addr)) {
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
            lisp_memory_fault_error(context, fault_addr);
#else
            if (!interrupt_maybe_gc_int(signal, siginfo, context)) {
                interrupt_handle_now(signal, siginfo, context);
            }
#if defined(LISP_FEATURE_DARWIN)
            /* Work around G5 bug; fix courtesy gbyers */
            DARWIN_FIX_CONTEXT(context);
#endif
#endif
        }
}

#if defined(LISP_FEATURE_MACH_EXCEPTION_HANDLER)
void
mach_error_memory_fault_handler(int signal, siginfo_t *siginfo, void *void_context) {
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
#ifdef SIG_MEMORY_FAULT2
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT2,
#ifdef LISP_FEATURE_FREEBSD
                                                 (__siginfohandler_t *)
#endif
                                                 memory_fault_handler);
#endif
#endif

#ifdef LISP_FEATURE_SB_THREAD
    undoably_install_low_level_interrupt_handler(SIG_INTERRUPT_THREAD,
                                                 interrupt_thread_handler);
    undoably_install_low_level_interrupt_handler(SIG_STOP_FOR_GC,
                                                 sig_stop_for_gc_handler);
#ifdef SIG_RESUME_FROM_GC
    undoably_install_low_level_interrupt_handler(SIG_RESUME_FROM_GC,
                                                 sig_stop_for_gc_handler);
#endif
#endif
    SHOW("leaving os_install_interrupt_handlers()");
}

#else /* Currently PPC/Darwin/Cheney only */

static void
sigsegv_handler(int signal, siginfo_t *info, void* void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    unsigned int pc =  (unsigned int *)(*os_context_pc_addr(context));
    os_vm_address_t addr;

    addr = arch_get_bad_addr(signal,info,context);
    if(!interrupt_maybe_gc(signal, info, context))
        if(!handle_guard_page_triggered(context,addr))
            interrupt_handle_now(signal, info, context);
    /* Work around G5 bug; fix courtesy gbyers */
    DARWIN_FIX_CONTEXT(context);
}

void
os_install_interrupt_handlers(void)
{
    SHOW("os_install_interrupt_handlers()/bsd-os/!defined(GENCGC)");
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
                                                 sigsegv_handler);
#ifdef SIG_MEMORY_FAULT2
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT2,
                                                 sigsegv_handler);
#endif
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

/* The stat() routines in NetBSD's C library are compatibility
   wrappers for some very old version of the stat buffer structure.
   Programs must be processed by the C toolchain in order to get an
   up-to-date definition of the stat() routine.  These wrappers are
   used only in sb-posix, as of 2006-10-15. -- RMK */
int _stat(const char *path, struct stat *sb) {
  return (stat(path, sb));
}

int _lstat(const char *path, struct stat *sb) {
  return (lstat(path, sb));
}

int _fstat(int fd, struct stat *sb) {
  return (fstat(fd, sb));
}


#endif /* __NetBSD__ */

#ifdef __FreeBSD__
static void freebsd_init()
{
    /* Quote from sbcl-devel (NIIMI Satoshi): "Some OSes, like FreeBSD
     * 4.x with GENERIC kernel, does not enable SSE support even on
     * SSE capable CPUs". Detect this situation and skip the
     * fast_bzero sse/base selection logic that's normally done in
     * x86-assem.S.
     */
#ifdef LISP_FEATURE_X86
    size_t len;
    int instruction_sse;

    len = sizeof(instruction_sse);
    if (sysctlbyname("hw.instruction_sse", &instruction_sse, &len, NULL, 0) == 0
        && instruction_sse != 0) {
        /* Use the SSE detector */
        fast_bzero_pointer = fast_bzero_detect;
    }
#endif /* LISP_FEATURE_X86 */
}
#endif /* __FreeBSD__ */

#ifdef LISP_FEATURE_DARWIN
/* defined in ppc-darwin-os.c instead */
#elif defined(LISP_FEATURE_FREEBSD)
#ifndef KERN_PROC_PATHNAME
#define KERN_PROC_PATHNAME 12
#endif

extern int getosreldate(void);

char *
os_get_runtime_executable_path()
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
#elif defined(LISP_FEATURE_NETBSD)
char *
os_get_runtime_executable_path()
{
    struct stat sb;
    char *path = strdup("/proc/curproc/file");
    if (path && ((stat(path, &sb)) == 0))
        return path;
    else {
        fprintf(stderr, "Couldn't stat /proc/curproc/file; is /proc mounted?\n");
        return NULL;
    }
}
#else /* Not DARWIN or FREEBSD or NETBSD */
char *
os_get_runtime_executable_path()
{
    return NULL;
}
#endif
