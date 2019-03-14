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
#include <errno.h>

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <linux/version.h>

#include "validate.h"
#include "thread.h"
#include "gc-internal.h"
#include <fcntl.h>
#ifdef LISP_FEATURE_SB_WTIMER
# include <sys/timerfd.h>
#endif

#ifdef LISP_FEATURE_X86
/* Prototype for personality(2). Done inline here since the header file
 * for this isn't available on old versions of glibc. */
int personality (unsigned long);
#define ADDR_NO_RANDOMIZE 0x0040000
#else
#include <sys/personality.h>
#endif

size_t os_vm_page_size;

#if defined(LISP_FEATURE_SB_THREAD) && defined(LISP_FEATURE_SB_FUTEX) && !defined(LISP_FEATURE_SB_PTHREAD_FUTEX)
#include <sys/syscall.h>
#include <unistd.h>
#include <errno.h>

/* values taken from the kernel's linux/futex.h.  This header file
   doesn't exist in userspace, which is our excuse for not grovelling
   them automatically */
#define FUTEX_WAIT 0
#define FUTEX_WAKE 1
/* This is also copied from linux/futex.h so that a binary compiled on
 * a not so recent Linux system can still take advantage of private
 * futexes when available.*/
#define FUTEX_WAIT_PRIVATE (0+128)
#define FUTEX_WAKE_PRIVATE (1+128)
#define FUTEX_FD (2)
#define FUTEX_REQUEUE (3)

/* Not static so that Lisp may query it. */
boolean futex_private_supported_p;

static inline int
futex_wait_op()
{
    return (futex_private_supported_p ? FUTEX_WAIT_PRIVATE : FUTEX_WAIT);
}

static inline int
futex_wake_op()
{
    return (futex_private_supported_p ? FUTEX_WAKE_PRIVATE : FUTEX_WAKE);
}

static inline int sys_futex(void *futex, int op, int val, struct timespec *rel)
{
    return syscall(SYS_futex, futex, op, val, rel);
}

static void
futex_init()
{
    int x = 0;
    sys_futex(&x, FUTEX_WAIT, 1, 0);
    if (errno == ENOSYS)
        lose("This version of SBCL is compiled with threading support, but your kernel\n"
             "is too old to support this. Please use a more recent kernel or\n"
             "a version of SBCL without threading support.\n");
    sys_futex(&x, FUTEX_WAIT_PRIVATE, 1, 0);
    if (errno == EWOULDBLOCK) {
        futex_private_supported_p = 1;
    } else {
        futex_private_supported_p = 0;
        SHOW("No futex private suppport\n");
    }
}

/* Try to guess the name of the mutex for this futex, based on knowing
 * the two pertinent Lisp object types (WAITQUEUE and MUTEX) that use a futex.
 * Callable from a C debugger */
#include "genesis/vector.h"
char* futex_name(int *lock_word)
{
    // If there is a Lisp string at lock_word-1 or -2, return that.
    // Otherwise return NULL.
    lispobj name = *(lock_word - 1);
    struct vector* v = (struct vector*)native_pointer(name);
    if (lowtag_of(name) == OTHER_POINTER_LOWTAG && widetag_of(&v->header) == SIMPLE_BASE_STRING_WIDETAG)
        return (char*)(v->data);
    name = *(lock_word - 2);
    v = (struct vector*)native_pointer(name);
    if (lowtag_of(name) == OTHER_POINTER_LOWTAG && widetag_of(&v->header) == SIMPLE_BASE_STRING_WIDETAG)
        return (char*)(v->data);
    return 0;
}

int
futex_wait(int *lock_word, int oldval, long sec, unsigned long usec)
{
  struct timespec timeout;
  int t;

  if (sec<0) {
      t = sys_futex(lock_word, futex_wait_op(), oldval, 0);
  }
  else {
      timeout.tv_sec = sec;
      timeout.tv_nsec = usec * 1000;
      t = sys_futex(lock_word, futex_wait_op(), oldval, &timeout);
  }
  if (t==0)
      return 0;
  else if (errno==ETIMEDOUT)
      return 1;
  else if (errno==EINTR)
      return 2;
  else
      /* EWOULDBLOCK and others, need to check the lock */
      return -1;
}

int
futex_wake(int *lock_word, int n)
{
    return sys_futex(lock_word, futex_wake_op(),n,0);
}
#endif


int linux_sparc_siginfo_bug = 0;

#ifdef LISP_FEATURE_SB_THREAD
int
isnptl (void)
{
  size_t n = confstr (_CS_GNU_LIBPTHREAD_VERSION, NULL, 0);
  if (n > 0) {
      char *buf = alloca (n);
      confstr (_CS_GNU_LIBPTHREAD_VERSION, buf, n);
      if (strstr (buf, "NPTL")) {
          return 1;
      }
  }
  return 0;
}
#endif

static void getuname(int *major_version, int* minor_version, int *patch_version)
{
    /* Conduct various version checks: do we have enough mmap(), is
     * this a sparc running 2.2, can we do threads? */
    struct utsname name;
    char *p;
    uname(&name);

    p=name.release;
    *major_version = atoi(p);
    *minor_version = *patch_version = 0;
    p=strchr(p,'.');
    if (p != NULL) {
            *minor_version = atoi(++p);
            p=strchr(p,'.');
            if (p != NULL)
                    *patch_version = atoi(++p);
    }
}

void os_init(char __attribute__((unused)) *argv[],
             char __attribute__((unused)) *envp[])
{
    int major_version, minor_version, patch_version;
    getuname(&major_version, &minor_version, &patch_version);
    if (major_version<2) {
        lose("linux kernel version too old: major version=%d (can't run in version < 2.0.0)\n",
             major_version);
    }
    if (!(major_version>2 || minor_version >= 4)) {
#ifdef LISP_FEATURE_SPARC
        FSHOW((stderr,"linux kernel %d.%d predates 2.4;\n enabling workarounds for SPARC kernel bugs in signal handling.\n", major_version,minor_version));
        linux_sparc_siginfo_bug = 1;
#endif
    }
#ifdef LISP_FEATURE_SB_THREAD
#if defined(LISP_FEATURE_SB_FUTEX) && !defined(LISP_FEATURE_SB_PTHREAD_FUTEX)
    futex_init();
#endif
    if(! isnptl()) {
       lose("This version of SBCL only works correctly with the NPTL threading\n"
            "library. Please use a newer glibc, use an older SBCL, or stop using\n"
            "LD_ASSUME_KERNEL\n");
    }
#endif

    /* Don't use getpagesize(), since it's not constant across Linux
     * kernel versions on some architectures (for example PPC). FIXME:
     * possibly the same should be done on other architectures too.
     */
    os_vm_page_size = BACKEND_PAGE_BYTES;

#ifdef LISP_FEATURE_X86
    /* Use SSE detector.  Recent versions of Linux enable SSE support
     * on SSE capable CPUs.  */
    /* FIXME: Are there any old versions that does not support SSE?  */
    fast_bzero_pointer = fast_bzero_detect;
#endif
}

#if (defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)) \
     && (!defined(DISABLE_ASLR) || DISABLE_ASLR)
# define ALLOW_PERSONALITY_CHANGE 1
#else
# define ALLOW_PERSONALITY_CHANGE 0
#endif

int os_preinit(char *argv[], char *envp[])
{
#if ALLOW_PERSONALITY_CHANGE
    if (getenv("SBCL_IS_RESTARTING")) {
        /* We restarted due to previously enabled ASLR.  Now,
         * reenable it for fork()'ed children. */
        int pers = personality(0xffffffffUL);
        personality(pers & ~ADDR_NO_RANDOMIZE);
        unsetenv("SBCL_IS_RESTARTING");
        return 0; // ensure_spaces() will win or not. Not much to do here.
    }
#endif
    /* See if we can allocate read-only, static, and linkage table spaces
     * at their required addresses. If we can, then there's no need to try
     * the re-exec trick since dynamic space is relocatable. */
    if (allocate_hardwired_spaces(0)) // soft failure mode
        return 1; // indicate that we already allocated hardwired spaces

    /* KLUDGE: Disable memory randomization on new Linux kernels
     * by setting a personality flag and re-executing. (We need
     * to re-execute, since the memory maps that can conflict with
     * the SBCL spaces have already been done at this point).
     */
    int major_version, minor_version, patch_version;
    getuname(&major_version, &minor_version, &patch_version);

#if ALLOW_PERSONALITY_CHANGE
    if ((major_version == 2
         /* Some old kernels will apparently lose unsupported personality flags
          * on exec() */
         && ((minor_version == 6 && patch_version >= 11)
             || (minor_version > 6)
             /* This is what RHEL 3 reports */
             || (minor_version == 4 && patch_version > 20)))
        || major_version >= 3)
    {
        int pers = personality(0xffffffffUL);
        if (!(pers & ADDR_NO_RANDOMIZE)) {
            int retval = personality(pers | ADDR_NO_RANDOMIZE);
            /* Allegedly some Linux kernels (the reported case was
             * "hardened Linux 2.6.7") won't set the new personality,
             * but nor will they return -1 for an error. So as a
             * workaround query the new personality...
             */
            int newpers = personality(0xffffffffUL);
            /* ... and don't re-execute if either the setting resulted
             * in an error or if the value didn't change. Otherwise
             * this might result in an infinite loop.
             */

            if (!getenv("SBCL_IS_RESTARTING") &&
                retval != -1 && newpers != pers) {
                /* Use /proc/self/exe instead of trying to figure out
                 * the executable path from PATH and argv[0], since
                 * that's unreliable. We follow the symlink instead of
                 * executing the file directly in order to prevent top
                 * from displaying the name of the process as "exe". */
                char runtime[PATH_MAX+1];
                int i = readlink("/proc/self/exe", runtime, PATH_MAX);
                if (i != -1) {
                    environ = envp;
                    setenv("SBCL_IS_RESTARTING", "T", 1);
                    runtime[i] = '\0';
                    execv(runtime, argv);
                }
            }
            /* Either changing the personality or execve() failed.
             * Just get on with life and hope for the best. */
        }
    }
#endif
    return 0;
}


#ifdef LISP_FEATURE_ALPHA
/* The Alpha is a 64 bit CPU.  SBCL is a 32 bit application.  Due to all
 * the places that assume we can get a pointer into a fixnum with no
 * information loss, we have to make sure it allocates all its ram in the
 * 0-2Gb region.  */

static void * under_2gb_free_pointer;
os_set_cheneygc_spaces(uword_t space0_start, uword_t space1_start)
{
    uword_t max;
    max = (space1_start > space0_start) ? space1_start : space0_start;
    under_2gb_free_pointer = max + dynamic_space_size;
}

#endif

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect(address, length, prot)) {
        if (errno == ENOMEM) {
            lose("An mprotect call failed with ENOMEM. This probably means that the maximum amount\n"
                 "of separate memory mappings was exceeded. To fix the problem, either increase\n"
                 "the maximum with e.g. 'echo 262144 > /proc/sys/vm/max_map_count' or recompile\n"
                 "SBCL with a larger value for GENCGC-CARD-BYTES in\n"
                 "'src/compiler/target/backend-parms.lisp'.");
        } else {
            perror("mprotect");
        }
    }
}

/*
 * any OS-dependent special low-level handling for signals
 */

/*
 * The GC needs to be hooked into whatever signal is raised for
 * page fault on this OS.
 */
static void
fallback_sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    // This calls corruption_warning_and_maybe_lose.
    lisp_memory_fault_error(context, arch_get_bad_addr(signal, info, context));
}

void (*sbcl_fallback_sigsegv_handler)  // Settable by user.
       (int, siginfo_t*, os_context_t*) = fallback_sigsegv_handler;

static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    os_vm_address_t addr = arch_get_bad_addr(signal, info, context);

#ifdef LISP_FEATURE_ALPHA
    /* Alpha stuff: This is the end of a pseudo-atomic section during
       which a signal was received.  We must deal with the pending
       interrupt (see also interrupt.c, ../code/interrupt.lisp)

       (how we got here: when interrupting, we set bit 63 in reg_ALLOC.
       At the end of the atomic section we tried to write to reg_ALLOC,
       got a SIGSEGV (there's nothing mapped there) so ended up here. */
    if (addr != NULL &&
        *os_context_register_addr(context, reg_ALLOC) & (1L<<63)) {
        *os_context_register_addr(context, reg_ALLOC) -= (1L<<63);
        interrupt_handle_pending(context);
        return;
    }
#endif

#ifdef LISP_FEATURE_SB_SAFEPOINT
    if (!handle_safepoint_violation(context, addr))
#endif

#ifdef LISP_FEATURE_GENCGC
    if (!gencgc_handle_wp_violation(addr))
#else
    if (!cheneygc_handle_wp_violation(context, addr))
#endif
        if (!handle_guard_page_triggered(context, addr))
            sbcl_fallback_sigsegv_handler(signal, info, context);
}

void
os_install_interrupt_handlers(void)
{
    if (INSTALL_SIG_MEMORY_FAULT_HANDLER) {
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
                                                 sigsegv_handler);
    }

    /* OAOOM c.f. sunos-os.c.
     * Should we have a reusable function gc_install_interrupt_handlers? */
#ifdef LISP_FEATURE_SB_THREAD
# ifdef LISP_FEATURE_SB_SAFEPOINT
#  ifdef LISP_FEATURE_SB_THRUPTION
    undoably_install_low_level_interrupt_handler(SIGPIPE, thruption_handler);
#  endif
# else
    undoably_install_low_level_interrupt_handler(SIG_STOP_FOR_GC,
                                                 sig_stop_for_gc_handler);
# endif
#endif
}

char *
os_get_runtime_executable_path(int __attribute__((unused)) external)
{
    char path[PATH_MAX + 1];
    int size;

    size = readlink("/proc/self/exe", path, sizeof(path)-1);
    if (size < 0)
        return NULL;
    else
        path[size] = '\0';

    return copied_string(path);
}

#ifdef LISP_FEATURE_SB_WTIMER
/*
 * Waitable timer implementation for the safepoint-based (SIGALRM-free)
 * timer facility using timerfd_create().
 */
int
os_create_wtimer()
{
    int fd = timerfd_create(CLOCK_MONOTONIC, 0);
    if (fd == -1)
        lose("os_create_wtimer: timerfd_create");

    /* Cannot count on TFD_CLOEXEC availability, so do it manually: */
    if (fcntl(fd, F_SETFD, FD_CLOEXEC) == -1)
        lose("os_create_wtimer: fcntl");

    return fd;
}

int
os_wait_for_wtimer(int fd)
{
    unsigned char buf[8];
    int n = read(fd, buf, sizeof(buf));
    if (n == -1) {
        if (errno == EINTR)
            return -1;
        lose("os_wtimer_listen failed");
    }
    if (n != sizeof(buf))
        lose("os_wtimer_listen read too little");
    return 0;
}

void
os_close_wtimer(int fd)
{
    if (close(fd) == -1)
        lose("os_close_wtimer failed");
}

void
os_set_wtimer(int fd, int sec, int nsec)
{
    struct itimerspec spec = { {0,0}, {0,0} };
    spec.it_value.tv_sec = sec;
    spec.it_value.tv_nsec = nsec;
    if (timerfd_settime(fd, 0, &spec, 0) == -1)
        lose("timerfd_settime");
}

void
os_cancel_wtimer(int fd)
{
    os_set_wtimer(fd, 0, 0);
}
#endif
