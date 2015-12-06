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
#include "gc.h"
#if defined LISP_FEATURE_GENCGC
#include "gencgc-internal.h"
#else
#include "cheneygc-internal.h"
#endif
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

/* This variable was in real use for a few months, basically for
 * storing autodetected information about whether the Linux
 * installation was recent enough to support SBCL threads, and make
 * some run-time decisions based on that. But this turned out to be
 * unstable, so now we just flat-out refuse to start on the old installations
 * when thread support has been compiled in.
 *
 * Unfortunately, in the meanwhile Slime started depending on this
 * variable for deciding which communication style to use. So even
 * though this variable looks unused, it shouldn't be deleted until
 * it's no longer used in the versions of Slime that people are
 * likely to download first. -- JES, 2006-06-07
 */
int linux_no_threads_p = 0;

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

void
os_init(char *argv[], char *envp[])
{
    /* Conduct various version checks: do we have enough mmap(), is
     * this a sparc running 2.2, can we do threads? */
    struct utsname name;
    int major_version;
    int minor_version;
    int patch_version;
    char *p;
    uname(&name);

    p=name.release;
    major_version = atoi(p);
    minor_version = patch_version = 0;
    p=strchr(p,'.');
    if (p != NULL) {
            minor_version = atoi(++p);
            p=strchr(p,'.');
            if (p != NULL)
                    patch_version = atoi(++p);
    }

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

    /* KLUDGE: Disable memory randomization on new Linux kernels
     * by setting a personality flag and re-executing. (We need
     * to re-execute, since the memory maps that can conflict with
     * the SBCL spaces have already been done at this point).
     *
     * Since randomization is currently implemented only on x86 kernels,
     * don't do this trick on other platforms.
     */
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
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
            /* Either changing the personality or execve() failed. Either
             * way we might as well continue, and hope that the random
             * memory maps are ok this time around.
             */
            fprintf(stderr, "WARNING:\
\nCouldn't re-execute SBCL with proper personality flags (/proc isn't mounted? setuid?)\
\nTrying to continue anyway.\n");
        } else if (getenv("SBCL_IS_RESTARTING")) {
            /* We restarted due to previously enabled ASLR.  Now,
             * reenable it for fork()'ed children. */
            int pers = personality(0xffffffffUL);
            personality(pers & ~ADDR_NO_RANDOMIZE);

            unsetenv("SBCL_IS_RESTARTING");
        }
    }
#ifdef LISP_FEATURE_X86
    /* Use SSE detector.  Recent versions of Linux enable SSE support
     * on SSE capable CPUs.  */
    /* FIXME: Are there any old versions that does not support SSE?  */
    fast_bzero_pointer = fast_bzero_detect;
#endif
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
    os_vm_address_t actual;

#ifdef LISP_FEATURE_ALPHA
    if (!addr) {
        addr=under_2gb_free_pointer;
    }
#endif

    if (addr)
        flags |= MAP_FIXED;

    actual = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);
    if (actual == MAP_FAILED) {
        perror("mmap");
        return 0;               /* caller should check this */
    }

    if (addr && (addr!=actual)) {
        fprintf(stderr, "mmap: wanted %lu bytes at %p, actually mapped at %p\n",
                (unsigned long) len, addr, actual);
        return 0;
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
        lose("unexpected mmap(..) failure\n");
    }

    return actual;
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect(address, length, prot) == -1) {
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

/*
 * The GC needs to be hooked into whatever signal is raised for
 * page fault on this OS.
 */
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
            lisp_memory_fault_error(context, addr);
}

void
os_install_interrupt_handlers(void)
{
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
                                                 sigsegv_handler);

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
os_get_runtime_executable_path(int external)
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
