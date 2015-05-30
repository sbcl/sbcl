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

#include "genesis/config.h"

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#ifdef _WIN32
  /* KLUDGE: From src/runtime/runtime.h, avoid double definition of
     boolean.  We really should clean up our act on this one. */
  #define boolean rpcndr_boolean
  #define WIN32_LEAN_AND_MEAN
  #include <windows.h>
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
#ifdef LISP_FEATURE_ANDROID
  #include <termios.h>
#else
  #include <sys/termios.h>
  #include <langinfo.h>
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

#ifdef LISP_FEATURE_HPUX
#include <sys/bsdtty.h> /* for TIOCGPGRP */
#endif

#ifdef LISP_FEATURE_BSD
  #include <sys/param.h>
  #include <sys/sysctl.h>
#endif

#ifdef _WIN32
  #include "pthreads_win32.h"
#endif

#include "wrap.h"
#include "gc.h"

#define DEFTYPE(lispname,cname) { cname foo; \
    printf("(define-alien-type " lispname " (%s %d))\n", (((foo=-1)<0) ? "signed" : "unsigned"), (8 * (sizeof foo))); }

#define DEFSTRUCT(lispname,cname,body) { cname bar; \
    printf("(define-alien-type nil\n  (struct %s", #lispname); \
    body; \
    printf("))\n"); }
#define DEFSLOT(lispname,cname) \
    printf("\n          (%s (%s %d))", \
           #lispname, \
           (((bar.cname=-1)<0) ? "signed" : "unsigned"), \
           (8 * (sizeof bar.cname)))

void
defconstant(char* lisp_name, unsigned long unix_number)
{
    printf("(defconstant %s %lu) ; #x%lx\n",
           lisp_name, unix_number, unix_number);
}

void deferrno(char* lisp_name, unsigned long unix_number)
{
    defconstant(lisp_name, unix_number);
}

void defsignal(char* lisp_name, unsigned long unix_number)
{
    defconstant(lisp_name, unix_number);
}

int
main(int argc, char *argv[])
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
    printf("(in-package \"SB!WIN32\")\n\n");

    defconstant ("input-record-size", sizeof (INPUT_RECORD));

    defconstant ("MAX_PATH", MAX_PATH);

    printf(";;; CSIDL\n");

    defconstant ("CSIDL_DESKTOP", CSIDL_DESKTOP);
    defconstant ("CSIDL_INTERNET", CSIDL_INTERNET);
    defconstant ("CSIDL_PROGRAMS", CSIDL_PROGRAMS);
    defconstant ("CSIDL_CONTROLS", CSIDL_CONTROLS);
    defconstant ("CSIDL_PRINTERS", CSIDL_PRINTERS);
    defconstant ("CSIDL_PERSONAL", CSIDL_PERSONAL);
    defconstant ("CSIDL_FAVORITES", CSIDL_FAVORITES);
    defconstant ("CSIDL_STARTUP", CSIDL_STARTUP);
    defconstant ("CSIDL_RECENT", CSIDL_RECENT);
    defconstant ("CSIDL_SENDTO", CSIDL_SENDTO);
    defconstant ("CSIDL_BITBUCKET", CSIDL_BITBUCKET);
    defconstant ("CSIDL_STARTMENU", CSIDL_STARTMENU);
    defconstant ("CSIDL_DESKTOPDIRECTORY", CSIDL_DESKTOPDIRECTORY);
    defconstant ("CSIDL_DRIVES", CSIDL_DRIVES);
    defconstant ("CSIDL_NETWORK", CSIDL_NETWORK);
    defconstant ("CSIDL_NETHOOD", CSIDL_NETHOOD);
    defconstant ("CSIDL_FONTS", CSIDL_FONTS);
    defconstant ("CSIDL_TEMPLATES", CSIDL_TEMPLATES);
    defconstant ("CSIDL_COMMON_STARTMENU", CSIDL_COMMON_STARTMENU);
    defconstant ("CSIDL_COMMON_PROGRAMS", CSIDL_COMMON_PROGRAMS);
    defconstant ("CSIDL_COMMON_STARTUP", CSIDL_COMMON_STARTUP);
    defconstant ("CSIDL_COMMON_DESKTOPDIRECTORY", CSIDL_COMMON_DESKTOPDIRECTORY);
    defconstant ("CSIDL_APPDATA", CSIDL_APPDATA);
    defconstant ("CSIDL_PRINTHOOD", CSIDL_PRINTHOOD);
    defconstant ("CSIDL_LOCAL_APPDATA", CSIDL_LOCAL_APPDATA);
    defconstant ("CSIDL_ALTSTARTUP", CSIDL_ALTSTARTUP);
    defconstant ("CSIDL_COMMON_ALTSTARTUP", CSIDL_COMMON_ALTSTARTUP);
    defconstant ("CSIDL_COMMON_FAVORITES", CSIDL_COMMON_FAVORITES);
    defconstant ("CSIDL_INTERNET_CACHE", CSIDL_INTERNET_CACHE);
    defconstant ("CSIDL_COOKIES", CSIDL_COOKIES);
    defconstant ("CSIDL_HISTORY", CSIDL_HISTORY);
    defconstant ("CSIDL_COMMON_APPDATA", CSIDL_COMMON_APPDATA);
    defconstant ("CSIDL_WINDOWS", CSIDL_WINDOWS);
    defconstant ("CSIDL_SYSTEM", CSIDL_SYSTEM);
    defconstant ("CSIDL_PROGRAM_FILES", CSIDL_PROGRAM_FILES);
    defconstant ("CSIDL_MYPICTURES", CSIDL_MYPICTURES);
    defconstant ("CSIDL_PROFILE", CSIDL_PROFILE);
    defconstant ("CSIDL_SYSTEMX86", CSIDL_SYSTEMX86);
    defconstant ("CSIDL_PROGRAM_FILESX86", CSIDL_PROGRAM_FILESX86);
    defconstant ("CSIDL_PROGRAM_FILES_COMMON", CSIDL_PROGRAM_FILES_COMMON);
    defconstant ("CSIDL_PROGRAM_FILES_COMMONX86", CSIDL_PROGRAM_FILES_COMMONX86);
    defconstant ("CSIDL_COMMON_TEMPLATES", CSIDL_COMMON_TEMPLATES);
    defconstant ("CSIDL_COMMON_DOCUMENTS", CSIDL_COMMON_DOCUMENTS);
    defconstant ("CSIDL_COMMON_ADMINTOOLS", CSIDL_COMMON_ADMINTOOLS);
    defconstant ("CSIDL_ADMINTOOLS", CSIDL_ADMINTOOLS);
    defconstant ("CSIDL_CONNECTIONS", CSIDL_CONNECTIONS);
    defconstant ("CSIDL_COMMON_MUSIC", CSIDL_COMMON_MUSIC);
    defconstant ("CSIDL_COMMON_PICTURES", CSIDL_COMMON_PICTURES);
    defconstant ("CSIDL_COMMON_VIDEO", CSIDL_COMMON_VIDEO);
    defconstant ("CSIDL_RESOURCES", CSIDL_RESOURCES);
    defconstant ("CSIDL_RESOURCES_LOCALIZED", CSIDL_RESOURCES_LOCALIZED);
    defconstant ("CSIDL_COMMON_OEM_LINKS", CSIDL_COMMON_OEM_LINKS);
    defconstant ("CSIDL_CDBURN_AREA", CSIDL_CDBURN_AREA);
    defconstant ("CSIDL_COMPUTERSNEARME", CSIDL_COMPUTERSNEARME);
    defconstant ("CSIDL_FLAG_DONT_VERIFY", CSIDL_FLAG_DONT_VERIFY);
    defconstant ("CSIDL_FLAG_CREATE", CSIDL_FLAG_CREATE);
    defconstant ("CSIDL_FLAG_MASK", CSIDL_FLAG_MASK);

    printf(";;; Exceptions\n");
    defconstant("+exception-access-violation+", EXCEPTION_ACCESS_VIOLATION);
    defconstant("+exception-array-bounds-exceeded+", EXCEPTION_ARRAY_BOUNDS_EXCEEDED);
    defconstant("+exception-breakpoint+", EXCEPTION_BREAKPOINT);
    defconstant("+exception-datatype-misalignment+", EXCEPTION_DATATYPE_MISALIGNMENT);
    defconstant("+exception-flt-denormal-operand+", EXCEPTION_FLT_DENORMAL_OPERAND);
    defconstant("+exception-flt-divide-by-zero+", EXCEPTION_FLT_DIVIDE_BY_ZERO);
    defconstant("+exception-flt-inexact-result+", EXCEPTION_FLT_INEXACT_RESULT);
    defconstant("+exception-flt-invalid-operation+", EXCEPTION_FLT_INVALID_OPERATION);
    defconstant("+exception-flt-overflow+", EXCEPTION_FLT_OVERFLOW);
    defconstant("+exception-flt-stack-check+", EXCEPTION_FLT_STACK_CHECK);
    defconstant("+exception-flt-underflow+", EXCEPTION_FLT_UNDERFLOW);
    defconstant("+exception-illegal-instruction+", EXCEPTION_ILLEGAL_INSTRUCTION);
    defconstant("+exception-in-page-error+", EXCEPTION_IN_PAGE_ERROR);
    defconstant("+exception-int-divide-by-zero+", EXCEPTION_INT_DIVIDE_BY_ZERO);
    defconstant("+exception-int-overflow+", EXCEPTION_INT_OVERFLOW);
    defconstant("+exception-invalid-disposition+", EXCEPTION_INVALID_DISPOSITION);
    defconstant("+exception-noncontinuable-exception+", EXCEPTION_NONCONTINUABLE_EXCEPTION);
    defconstant("+exception-priv-instruction+", EXCEPTION_PRIV_INSTRUCTION);
    defconstant("+exception-single-step+", EXCEPTION_SINGLE_STEP);
    defconstant("+exception-stack-overflow+", EXCEPTION_STACK_OVERFLOW);
    defconstant("+dbg-printexception-c+", DBG_PRINTEXCEPTION_C);

    defconstant("+exception-maximum-parameters+", EXCEPTION_MAXIMUM_PARAMETERS);

    printf(";;; FormatMessage\n");

    defconstant("format-message-allocate-buffer", FORMAT_MESSAGE_ALLOCATE_BUFFER);
    defconstant("format-message-from-system", FORMAT_MESSAGE_FROM_SYSTEM);
    defconstant("format-message-max-width-mask", FORMAT_MESSAGE_MAX_WIDTH_MASK);
    defconstant("format-message-ignore-inserts", FORMAT_MESSAGE_IGNORE_INSERTS);

    printf(";;; Errors\n");

    printf(";;; Errors\n");

    defconstant("ERROR_ENVVAR_NOT_FOUND", ERROR_ENVVAR_NOT_FOUND);
    defconstant("ERROR_ALREADY_EXISTS", ERROR_ALREADY_EXISTS);
    defconstant("ERROR_FILE_EXISTS", ERROR_FILE_EXISTS);
    defconstant("ERROR_FILE_NOT_FOUND", ERROR_FILE_NOT_FOUND);
    defconstant("ERROR_ACCESS_DENIED", ERROR_ACCESS_DENIED);

    printf(";;; GetComputerName\n");

    defconstant ("MAX_COMPUTERNAME_LENGTH", MAX_COMPUTERNAME_LENGTH);
    defconstant ("ERROR_BUFFER_OVERFLOW", ERROR_BUFFER_OVERFLOW);

    printf(";;; Windows Types\n");
    DEFTYPE("int-ptr", INT_PTR);
    DEFTYPE("dword",   DWORD);
    DEFTYPE("bool",    BOOL);
    DEFTYPE("uint",    UINT);
    DEFTYPE("ulong",   ULONG);

    printf(";;; File Desired Access\n");
    defconstant ("FILE_GENERIC_READ", FILE_GENERIC_READ);
    defconstant ("FILE_GENERIC_WRITE", FILE_GENERIC_WRITE);
    defconstant ("FILE_GENERIC_EXECUTE", FILE_GENERIC_EXECUTE);
    defconstant ("FILE_SHARE_READ", FILE_SHARE_READ);
    defconstant ("FILE_SHARE_WRITE", FILE_SHARE_WRITE);
    defconstant ("FILE_SHARE_DELETE", FILE_SHARE_DELETE);

    printf(";;; File Creation Dispositions\n");
    defconstant("CREATE_NEW", CREATE_NEW);
    defconstant("CREATE_ALWAYS", CREATE_ALWAYS);
    defconstant("OPEN_EXISTING", OPEN_EXISTING);
    defconstant("OPEN_ALWAYS", OPEN_ALWAYS);
    defconstant("TRUNCATE_EXISTING", TRUNCATE_EXISTING);

    printf(";;; Desired Access\n");
    defconstant("ACCESS_GENERIC_READ", GENERIC_READ);
    defconstant("ACCESS_GENERIC_WRITE", GENERIC_WRITE);
    defconstant("ACCESS_GENERIC_EXECUTE", GENERIC_EXECUTE);
    defconstant("ACCESS_GENERIC_ALL", GENERIC_ALL);
    defconstant("ACCESS_FILE_APPEND_DATA", FILE_APPEND_DATA);
    defconstant("ACCESS_DELETE", DELETE);

    printf(";;; Handle Information Flags\n");
    defconstant("HANDLE_FLAG_INHERIT", HANDLE_FLAG_INHERIT);
    defconstant("HANDLE_FLAG_PROTECT_FROM_CLOSE", HANDLE_FLAG_PROTECT_FROM_CLOSE);

    printf(";;; Standard Handle Keys\n");
    defconstant("STD_INPUT_HANDLE", STD_INPUT_HANDLE);
    defconstant("STD_OUTPUT_HANDLE", STD_OUTPUT_HANDLE);
    defconstant("STD_ERROR_HANDLE", STD_ERROR_HANDLE);

    printf(";;; WinCrypt\n");
    defconstant("crypt-verifycontext", CRYPT_VERIFYCONTEXT);
    defconstant("crypt-silent", CRYPT_SILENT);
    defconstant("prov-rsa-full", PROV_RSA_FULL);

    /* FIXME: SB-UNIX and SB-WIN32 really need to be untangled. */
    printf("(in-package \"SB!UNIX\")\n\n");
    printf(";;; Unix-like constants and types on Windows\n");
    defconstant("o_rdonly", _O_RDONLY);
    defconstant("o_wronly", _O_WRONLY);
    defconstant("o_rdwr",   _O_RDWR);
    defconstant("o_creat",  _O_CREAT);
    defconstant("o_trunc",  _O_TRUNC);
    defconstant("o_append", _O_APPEND);
    defconstant("o_excl",   _O_EXCL);
    defconstant("o_binary", _O_BINARY);
    defconstant("o_noinherit", _O_NOINHERIT);

    defconstant("enoent", ENOENT);
    defconstant("eexist", EEXIST);
    defconstant("eintr", EINTR);
    defconstant("eagain", EAGAIN);
    defconstant("ebadf", EBADF);

    defconstant("s-ifmt",  S_IFMT);
    defconstant("s-ifdir", S_IFDIR);
    defconstant("s-ifreg", S_IFREG);

    DEFTYPE("ino-t",  ino_t);
    DEFTYPE("time-t", time_t);
    DEFTYPE("off-t",  off_t);
    DEFTYPE("size-t", size_t);
    DEFTYPE("mode-t", mode_t);

    DEFTYPE("wst-dev-t", wst_dev_t);
    DEFTYPE("wst-ino-t", wst_ino_t);
    DEFTYPE("wst-off-t", wst_off_t);
    DEFTYPE("wst-blksize-t", wst_blksize_t);
    DEFTYPE("wst-blkcnt-t", wst_blkcnt_t);
    DEFTYPE("wst-nlink-t", wst_nlink_t);
    DEFTYPE("wst-uid-t", wst_uid_t);
    DEFTYPE("wst-gid-t", wst_gid_t);

    /* KLUDGE */
    defconstant("fd-setsize", 1024);
    printf("\n");
#else
    printf("(in-package \"SB!ALIEN\")\n\n");

    printf (";;;flags for dlopen()\n");

    defconstant ("rtld-lazy", RTLD_LAZY);
    defconstant ("rtld-now", RTLD_NOW);
    defconstant ("rtld-global", RTLD_GLOBAL);

    printf("(in-package \"SB!UNIX\")\n\n");

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
#ifndef LISP_FEATURE_ANDROID
    printf(";;; langinfo\n");
    defconstant("codeset", CODESET);
#endif
    printf(";;; types, types, types\n");
    DEFTYPE("clock-t", clock_t);
    DEFTYPE("dev-t",   dev_t);
    DEFTYPE("gid-t",   gid_t);
    DEFTYPE("ino-t",   ino_t);
    DEFTYPE("mode-t",  mode_t);
    DEFTYPE("nlink-t", nlink_t);
    DEFTYPE("off-t",   off_t);
    DEFTYPE("size-t",  size_t);
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
    deferrno("espipe", ESPIPE);
    deferrno("ewouldblock", EWOULDBLOCK);
    printf("\n");

    printf(";;; for wait3(2) in run-program.lisp\n");
    defconstant("wnohang", WNOHANG);
    defconstant("wuntraced", WUNTRACED);
    printf("\n");

    printf(";;; various ioctl(2) flags\n");
    defconstant("tiocgpgrp",  TIOCGPGRP);
    defconstant("tiocspgrp",  TIOCSPGRP);
    defconstant("tiocgwinsz", TIOCGWINSZ);
    defconstant("tiocswinsz", TIOCSWINSZ);
    /* KLUDGE: These are referenced by old CMUCL-derived code, but
     * Linux doesn't define them.
     *
     * I think these are the BSD names, but I don't know what the
     * corresponding SysV/Linux names are. As a point of reference,
     * CMUCL doesn't have these defined either (although the defining
     * forms *do* exist in src/code/unix.lisp), so I don't feel nearly
     * so bad about not hunting them down. Insight into renamed
     * obscure ioctl(2) flags appreciated. --njf, 2002-08-26
     *
     * I note that the first one I grepped for, TIOCSIGSEND, is
     * referenced in SBCL conditional on #+HPUX. Maybe the porters of
     * Oxbridge know more about things like that? And even if they
     * don't, one benefit of the Rhodes crusade to heal the worthy
     * ports should be that afterwards, if we grep for something like
     * this in CVS and it's not there, we can lightheartedly nuke it.
     * -- WHN 2002-08-30 */
    /*
      defconstant("tiocsigsend", TIOCSIGSEND);
      defconstant("tiocflush", TIOCFLUSH);
      defconstant("tiocgetp", TIOCGETP);
      defconstant("tiocsetp", TIOCSETP);
      defconstant("tiocgetc", TIOCGETC);
      defconstant("tiocsetc", TIOCSETC);
      defconstant("tiocgltc", TIOCGLTC);
      defconstant("tiocsltc", TIOCSLTC);
    */
    printf("\n");

    printf(";;; signals\n");
    defconstant("sig-dfl", (unsigned long)SIG_DFL);
    defconstant("sig-ign", (unsigned long)SIG_IGN);

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
    defsignal("sigio", SIGIO);
    defsignal("sigiot", SIGIOT);
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
#ifdef SIGWAITING
    defsignal("sigwaiting", SIGWAITING);
#endif
    defsignal("sigwinch", SIGWINCH);
#ifdef SIGXCPU
    defsignal("sigxcpu", SIGXCPU);
#endif
#ifdef SIGXFSZ
    defsignal("sigxfsz", SIGXFSZ);
#endif

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

    printf(";;; structures\n");
    DEFSTRUCT(timeval, struct timeval,
        DEFSLOT(tv-sec, tv_sec);
        DEFSLOT(tv-usec, tv_usec));
    DEFSTRUCT(timespec, struct timespec,
        DEFSLOT(tv-sec, tv_sec);
        DEFSLOT(tv-nsec, tv_nsec));
    printf("\n");

#ifdef LISP_FEATURE_ANDROID
    defconstant("path-max", PATH_MAX);
    printf("\n");
#endif

#ifdef LISP_FEATURE_BSD
    printf(";;; sysctl(3) names\n");
    printf("(in-package \"SB!IMPL\")\n");
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

    printf("(in-package \"SB!KERNEL\")\n\n");
#ifdef LISP_FEATURE_GENCGC
    printf(";;; GENCGC related\n");
    DEFTYPE("page-index-t", page_index_t);
    DEFTYPE("generation-index-t", generation_index_t);
    printf("\n");
#endif

    printf(";;; Our runtime types\n");
    DEFTYPE("os-vm-size-t", os_vm_size_t);

    return 0;
}
