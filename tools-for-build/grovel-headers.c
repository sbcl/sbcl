/*
 * Rummage through the system header files using the C compiler itself
 * as a parser, extracting stuff like preprocessor constants and the
 * sizes and signedness of basic system types, and write it out as
 * Lisp code.
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * While most of SBCL is derived from the CMU CL system, many
 * utilities for the build process (like this one) were written from
 * scratch after the fork from CMU CL.
 *
 * This software is in the public domain and is provided with
 * absolutely no warranty. See the COPYING and CREDITS files for
 * more information.
 */

#include "genesis/sbcl.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#ifdef _WIN32
  /* KLUDGE: From src/runtime/runtime.h, avoid double definition of
     boolean.  We really should clean up our act on this one. */
  #define boolean rpcndr_boolean
  #define WIN32_LEAN_AND_MEAN
  #include <windows.h>
  #include <ntstatus.h>
  #include <shlobj.h>
  #include <wincrypt.h>
  #include <winsock2.h>
  #undef boolean
#else
  #include <poll.h>
  #include <sys/select.h>
  #include <sys/times.h>
  #include <sys/wait.h>
  #include <sys/ioctl.h>
#if defined __HAIKU__ || defined __DragonFly__ || defined LISP_FEATURE_ANDROID
  #include <termios.h>
#else
  #include <sys/termios.h>
#endif
  #include <sys/time.h>
  #include <dlfcn.h>
#endif

#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <time.h>

#ifdef LISP_FEATURE_BSD
  #include <sys/param.h>
  #include <sys/sysctl.h>
#endif

#ifdef LISP_FEATURE_SB_THREAD
# ifdef LISP_FEATURE_WIN32
#  include "pthreads_win32.h"
# elif defined LISP_FEATURE_OS_THREAD_STACK
#  include <limits.h> // PTHREAD_STACK_MIN is possibly in here
#  include <pthread.h> // instead of in here
# endif
#endif

#include "wrap.h"
#include "gc-typedefs.h" // for page_index_t
#include "os.h" // for os_vm_size_t

#if defined LISP_FEATURE_WIN32 && defined LISP_FEATURE_64_BIT
# define CAST_SIZEOF (unsigned long)
#else
# define CAST_SIZEOF
#endif

#define DEFTYPE(lispname,cname) { cname foo; \
    printf("(define-alien-type " lispname " (%s %lu))\n", \
           (((foo=-1)<0) ? "signed" : "unsigned"), (8LU * CAST_SIZEOF (sizeof foo))); }

#define DEFSTRUCT(lispname,cname,body) { cname bar; \
    printf("(define-alien-type nil\n  (struct %s", #lispname); \
    body; \
    printf("))\n"); }
#define DEFSLOT(lispname,cname) \
    printf("\n          (%s (%s %lu))", \
           #lispname, \
           (((bar.cname=-1)<0) ? "signed" : "unsigned"), \
           (8LU * CAST_SIZEOF (sizeof bar.cname)))

#define DEFCONSTANT(lispname,cname) \
  if (cname<0) defconstant_neg(lispname, cname); else defconstant(lispname,cname)

void defconstant(char* lisp_name, unsigned long unix_number) {
    printf("(defconstant %s %lu) ; #x%lx\n", lisp_name, unix_number, unix_number);
}
void defconstant_neg(char* lisp_name, long unix_number) {
    printf("(defconstant %s %ld)\n", lisp_name, unix_number);
}

#ifdef __HAIKU__
// Haiku defines negative error numbers. I don't think that's allowed for any
// of the Posix-specified numbers such as ENOENT, as per
// https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/errno.h.html#tag_13_10
// "The <errno.h> header shall define the following macros which shall expand to integer
//  constant expressions with type int, distinct positive values (except as noted below) ...
//  [ENOENT]" etc
// But all the constants seem to be based off of INT_MIN.
//    #define B_GENERAL_ERROR_BASE INT_MIN
//    #define B_STORAGE_ERROR_BASE (B_GENERAL_ERROR_BASE + 0x6000)
//    #define B_ENTRY_NOT_FOUND (B_STORAGE_ERROR_BASE + 3)
//    #define B_TO_POSIX_ERROR(error) (error)
//    #define ENOENT B_TO_POSIX_ERROR(B_ENTRY_NOT_FOUND)
// The header correctly has errno as a signed int:
//    extern int *_errnop(void);
//    #define errno (*(_errnop()))
// but printing those as though they were unsigned long causes them to get sign-extended
// and show up as huge positive numbers.  So we have a platform-specific variant of
// deferrno() which treats them as 'int' to preserve the negative sign, which has the right
// behavior because the system calls do actually return 'int'.
void deferrno(char* lisp_name, int unix_number)
{
    printf("(defconstant %s %d)\n", lisp_name, unix_number);
}
#else
void deferrno(char* lisp_name, unsigned long unix_number)
{
    defconstant(lisp_name, unix_number);
}
#endif

void defsignal(char* lisp_name, unsigned long unix_number)
{
    defconstant(lisp_name, unix_number);
}

int
main(int argc, char __attribute__((unused)) *argv[])
{
    /* don't need no steenking command line arguments */
    if (1 != argc) {
        fprintf(stderr, "argh! command line argument(s)\n");
        exit(1);
    }

    /* don't need no steenking hand-editing */
    printf(
";;;; This is an automatically generated file, please do not hand-edit it.\n\
;;;; See the program \"grovel-headers.c\".\n\
\n\
");
#ifdef _WIN32
    #include "grovel-headers-win32.inc"
#else
    printf("(in-package \"SB-ALIEN\")\n\n");

    printf (";;;flags for dlopen()\n");

    defconstant ("rtld-lazy", RTLD_LAZY);
    defconstant ("rtld-now", RTLD_NOW);
    defconstant ("rtld-global", RTLD_GLOBAL);

    printf("\n(in-package \"SB-UNIX\")\n\n");

#if defined LISP_FEATURE_OS_THREAD_STACK
    defconstant("pthread-min-stack", PTHREAD_STACK_MIN);
    printf("\n");
#endif

    printf(";;; select()\n");
    defconstant("fd-setsize", FD_SETSIZE);

    printf(";;; poll()\n");
    defconstant("pollin", POLLIN);
    defconstant("pollout", POLLOUT);
    defconstant("pollpri", POLLPRI);
    defconstant("pollhup", POLLHUP);
    defconstant("pollnval", POLLNVAL);
    defconstant("pollerr", POLLERR);
    DEFTYPE("nfds-t", nfds_t);
    printf(";;; types, types, types\n");
    DEFTYPE("clock-t", clock_t);
    DEFTYPE("dev-t",   dev_t);
    DEFTYPE("gid-t",   gid_t);
    DEFTYPE("ino-t",   ino_t);
    DEFTYPE("mode-t",  mode_t);
    DEFTYPE("nlink-t", nlink_t);
    DEFTYPE("off-t",   off_t);
    DEFTYPE("size-t",  size_t);
    DEFTYPE("ssize-t", ssize_t);
    DEFTYPE("time-t",  time_t);
#if !defined(LISP_FEATURE_OS_PROVIDES_SUSECONDS_T)
    /* Similar kludge in sb-posix. */
    DEFTYPE("suseconds-t", long);
#else
    DEFTYPE("suseconds-t", suseconds_t);
#endif
    DEFTYPE("uid-t",   uid_t);
    printf(";; Types in src/runtime/wrap.h. See that file for explantion.\n");
    printf(";; Don't use these types for anything other than the stat wrapper.\n");
    DEFTYPE("wst-ino-t", wst_ino_t);
    DEFTYPE("wst-dev-t", wst_dev_t);
    DEFTYPE("wst-off-t", wst_off_t);
    DEFTYPE("wst-blksize-t", wst_blksize_t);
    DEFTYPE("wst-blkcnt-t", wst_blkcnt_t);
    DEFTYPE("wst-nlink-t", wst_nlink_t);
    DEFTYPE("wst-uid-t", wst_uid_t);
    DEFTYPE("wst-gid-t", wst_gid_t);
    printf("\n");

    printf(";;; fcntl.h (or unistd.h on OpenBSD and NetBSD)\n");
    defconstant("r_ok", R_OK);
    defconstant("w_ok", W_OK);
    defconstant("x_ok", X_OK);
    defconstant("f_ok", F_OK);
    printf("\n");

    printf(";;; fcntlbits.h\n");
    defconstant("o_rdonly",  O_RDONLY);
    defconstant("o_wronly",  O_WRONLY);
    defconstant("o_rdwr",    O_RDWR);
    defconstant("o_accmode", O_ACCMODE);
    defconstant("o_creat",   O_CREAT);
    defconstant("o_excl",    O_EXCL);
    defconstant("o_noctty",  O_NOCTTY);
    defconstant("o_trunc",   O_TRUNC);
    defconstant("o_append",  O_APPEND);
#ifdef LISP_FEATURE_LARGEFILE
    defconstant("o_largefile", O_LARGEFILE);
#endif

    printf(";;;\n");
    defconstant("s-ifmt",  S_IFMT);
    defconstant("s-ififo", S_IFIFO);
    defconstant("s-ifchr", S_IFCHR);
    defconstant("s-ifdir", S_IFDIR);
    defconstant("s-ifblk", S_IFBLK);
    defconstant("s-ifreg", S_IFREG);
    printf("\n");

    defconstant("s-iflnk",  S_IFLNK);
    defconstant("s-ifsock", S_IFSOCK);
    printf("\n");

    printf(";;; error numbers\n");
    deferrno("ebadf", EBADF);
    deferrno("enoent", ENOENT);
    deferrno("eintr", EINTR);
    deferrno("eagain", EAGAIN);
    deferrno("eio", EIO);
    deferrno("eexist", EEXIST);
    deferrno("eloop", ELOOP);
    deferrno("epipe", EPIPE);
    deferrno("espipe", ESPIPE);
    deferrno("ewouldblock", EWOULDBLOCK);
    printf("\n");

    defconstant("sc-nprocessors-onln", _SC_NPROCESSORS_ONLN);

    printf(";;; for waitpid() in run-program.lisp\n");
#ifdef WCONTINUED
    defconstant("wcontinued", WCONTINUED);
#else
    defconstant("wcontinued", 0);
#endif

    defconstant("wnohang", WNOHANG);
    defconstant("wuntraced", WUNTRACED);
    printf("\n");

    printf(";;; various ioctl(2) flags\n");
    defconstant("tiocgpgrp",  TIOCGPGRP);
    printf("\n");

    printf(";;; signals\n");
    defconstant("sizeof-sigset_t", sizeof (sigset_t));
    defconstant("sig_block", SIG_BLOCK);
    defconstant("sig_unblock", SIG_UNBLOCK);
    defconstant("sig_setmask", SIG_SETMASK);
    defsignal("sigalrm", SIGALRM);
    defsignal("sigbus", SIGBUS);
    defsignal("sigchld", SIGCHLD);
    defsignal("sigcont", SIGCONT);
#ifdef SIGEMT
    defsignal("sigemt", SIGEMT);
#endif
    defsignal("sigfpe", SIGFPE);
    defsignal("sighup", SIGHUP);
    defsignal("sigill", SIGILL);
    defsignal("sigint", SIGINT);
#ifdef SIGIO
    defsignal("sigio", SIGIO);
#endif
    defsignal("sigkill", SIGKILL);
    defsignal("sigpipe", SIGPIPE);
    defsignal("sigprof", SIGPROF);
    defsignal("sigquit", SIGQUIT);
    defsignal("sigsegv", SIGSEGV);
#ifdef SIGSTKFLT
    defsignal("sigstkflt", SIGSTKFLT);
#endif
    defsignal("sigstop", SIGSTOP);
#ifdef SIGSYS
    defsignal("sigsys", SIGSYS);
#endif
    defsignal("sigterm", SIGTERM);
    defsignal("sigtrap", SIGTRAP);
    defsignal("sigtstp", SIGTSTP);
    defsignal("sigttin", SIGTTIN);
    defsignal("sigttou", SIGTTOU);
    defsignal("sigurg", SIGURG);
    defsignal("sigusr1", SIGUSR1);
    defsignal("sigusr2", SIGUSR2);
    defsignal("sigvtalrm", SIGVTALRM);
    defsignal("sigwinch", SIGWINCH);
#ifdef SIGXCPU
    defsignal("sigxcpu", SIGXCPU);
#endif
#ifdef SIGXFSZ
    defsignal("sigxfsz", SIGXFSZ);
#endif

    defconstant("itimer-real", ITIMER_REAL);
    defconstant("itimer-virtual", ITIMER_VIRTUAL);
    defconstant("itimer-prof", ITIMER_PROF);

   /* Floating point exception codes. Some of these
    * are missing on Darwin. */
#ifdef FPE_INTOVF
    defconstant("fpe-intovf", FPE_INTOVF);
#else
    defconstant("fpe-intovf", -1);
#endif
#ifdef FPE_INTDIV
    defconstant("fpe-intdiv", FPE_INTDIV);
#else
    defconstant("fpe-intdiv", -1);
#endif
    defconstant("fpe-fltdiv", FPE_FLTDIV);
    defconstant("fpe-fltovf", FPE_FLTOVF);
    defconstant("fpe-fltund", FPE_FLTUND);
    defconstant("fpe-fltres", FPE_FLTRES);
    defconstant("fpe-fltinv", FPE_FLTINV);
#ifdef FPE_FLTSUB
    defconstant("fpe-fltsub", FPE_FLTSUB);
#else
    defconstant("fpe-fltsub", -1);
#endif
#endif // !WIN32
    printf("\n");

#if !defined(LISP_FEATURE_AVOID_CLOCK_GETTIME)
#ifdef LISP_FEATURE_UNIX
    DEFCONSTANT("clock-realtime", CLOCK_REALTIME);
    DEFCONSTANT("clock-monotonic", CLOCK_MONOTONIC);
    DEFCONSTANT("clock-process-cputime-id", CLOCK_PROCESS_CPUTIME_ID);
#endif
#ifdef LISP_FEATURE_LINUX
#ifdef CLOCK_REALTIME_ALARM
    defconstant("clock-realtime-alarm", CLOCK_REALTIME_ALARM);
#endif
    defconstant("clock-realtime-coarse", CLOCK_REALTIME_COARSE);
#ifdef CLOCK_TAI
    defconstant("clock-tai", CLOCK_TAI); // International Atomic Time.
#endif
    defconstant("clock-monotonic-coarse", CLOCK_MONOTONIC_COARSE);
    defconstant("clock-monotonic-raw", CLOCK_MONOTONIC_RAW);
#ifdef CLOCK_BOOTTIME
    defconstant("clock-boottime", CLOCK_BOOTTIME);
#endif
#ifdef CLOCK_BOOTTIME_ALARM
    defconstant("clock-boottime-alarn", CLOCK_BOOTTIME_ALARM);
#endif
    DEFCONSTANT("clock-thread-cputime-id", CLOCK_THREAD_CPUTIME_ID);
#endif
#endif
    printf(";;; structures\n");
    DEFSTRUCT(timeval, struct timeval,
        DEFSLOT(tv-sec, tv_sec);
        DEFSLOT(tv-usec, tv_usec));
    // There is no way to detect musl libc (https://wiki.musl-libc.org/faq)
    // I'm really just guessing, but maybe 32-bit ARM has padding too
#if defined LISP_FEATURE_LINUX && defined LISP_FEATURE_LITTLE_ENDIAN \
  && (defined LISP_FEATURE_X86 || defined LISP_FEATURE_ARM)
    struct timespec dummy_ts;
    if (sizeof dummy_ts > sizeof dummy_ts.tv_sec + sizeof dummy_ts.tv_nsec) {
        fprintf(stderr, "WARNING: Assuming physical layout of timespec\n");
        printf("(define-alien-type nil\n\
  (struct timespec\n\
          (tv-sec (signed 64))\n\
          (tv-nsec (signed 32))\n\
          (padding (signed 32))))\n");
    } else
#endif
    DEFSTRUCT(timespec, struct timespec,
        DEFSLOT(tv-sec, tv_sec);
        DEFSLOT(tv-nsec, tv_nsec));
    defconstant("sizeof-timespec", sizeof (struct timespec));
    defconstant("sizeof-timeval", sizeof (struct timeval));
    printf("\n");

#ifdef LISP_FEATURE_ANDROID
    defconstant("path-max", PATH_MAX);
    printf("\n");
#endif

#ifdef LISP_FEATURE_BSD
    printf(";;; sysctl(3) names\n");
    printf("(in-package \"SB-IMPL\")\n");
    defconstant("ctl-kern", CTL_KERN);
    defconstant("ctl-hw", CTL_HW);
    defconstant("ctl-maxname", CTL_MAXNAME);
    defconstant("kern-ostype", KERN_OSTYPE);
    defconstant("kern-osrelease", KERN_OSRELEASE);
    defconstant("hw-model", HW_MODEL);
    defconstant("hw-pagesize", HW_PAGESIZE);

    defconstant("path-max", PATH_MAX);
    printf("\n");
#endif

    printf("(in-package \"SB-KERNEL\")\n\n");
#ifdef LISP_FEATURE_GENERATIONAL
    printf(";;; GENCGC related\n");
    DEFTYPE("page-index-t", page_index_t);
    DEFTYPE("generation-index-t", generation_index_t);
    printf("\n");
#endif

    printf(";;; Our runtime types\n");
    DEFTYPE("os-vm-size-t", os_vm_size_t);

    return 0;
}
