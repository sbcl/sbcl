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

#include <stdio.h>
#include <sys/types.h>
#include <sys/times.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <sys/termios.h>
#include <fcntl.h>
#include <unistd.h>
#include <signal.h>

#include "genesis/config.h"

#define DEFTYPE(lispname,cname) { cname foo; \
    printf("(define-alien-type " lispname " (%s %d))\n", (((foo=-1)<0) ? "sb!alien:signed" : "unsigned"), (8 * (sizeof foo))); }

void
defconstant(char* lisp_name, long unix_number)
{
    printf("(defconstant %s %ld) ; #x%lx\n",
	   lisp_name, unix_number, unix_number);
}

#define DEFSIGNAL(name) defconstant(#name, name)

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

    printf("(in-package \"SB!UNIX\")\n\n");

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
    DEFTYPE("uid-t",   uid_t);
    printf("\n");

    printf(";;; fcntl.h (or unistd.h on OpenBSD)\n");
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

    printf(";;; for wait3(2) in run-program.lisp\n");
    defconstant("wnohang", WNOHANG);
    defconstant("wuntraced", WUNTRACED);
    printf("\n");

    printf(";;; various ioctl(2) flags\n");
    defconstant("tiocnotty",  TIOCNOTTY);
    defconstant("tiocgwinsz", TIOCGWINSZ);
    defconstant("tiocswinsz", TIOCSWINSZ);
    defconstant("tiocgpgrp",  TIOCGPGRP);
    defconstant("tiocspgrp",  TIOCSPGRP);
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
    DEFSIGNAL(SIGALRM);
    DEFSIGNAL(SIGBUS);
    DEFSIGNAL(SIGCHLD);
    DEFSIGNAL(SIGCONT);
/* FIXME: Maybe #ifdef SIGEMT would be a smarter conditional? */
#if (!(defined LISP_FEATURE_LINUX) || !((defined LISP_FEATURE_PPC) || (defined LISP_FEATURE_X86)))
    DEFSIGNAL(SIGEMT);
#endif
    DEFSIGNAL(SIGFPE);
    DEFSIGNAL(SIGHUP);
    DEFSIGNAL(SIGILL);
    DEFSIGNAL(SIGINT);
    DEFSIGNAL(SIGIO);
    DEFSIGNAL(SIGIOT);
    DEFSIGNAL(SIGKILL);
    DEFSIGNAL(SIGPIPE);
    DEFSIGNAL(SIGPROF);
    DEFSIGNAL(SIGQUIT);
    DEFSIGNAL(SIGSEGV);
#if ((defined LISP_FEATURE_LINUX) && (defined LISP_FEATURE_X86))
    DEFSIGNAL(SIGSTKFLT);
#endif
    DEFSIGNAL(SIGSTOP);
#if (!((defined LISP_FEATURE_LINUX) && (defined LISP_FEATURE_X86))) 
    DEFSIGNAL(SIGSYS);
#endif
    DEFSIGNAL(SIGTERM);
    DEFSIGNAL(SIGTRAP);
    DEFSIGNAL(SIGTSTP);
    DEFSIGNAL(SIGTTIN);
    DEFSIGNAL(SIGTTOU);
    DEFSIGNAL(SIGURG);
    DEFSIGNAL(SIGUSR1);
    DEFSIGNAL(SIGUSR2);
    DEFSIGNAL(SIGVTALRM);
#ifdef LISP_FEATURE_SUNOS
    DEFSIGNAL(SIGWAITING);
#endif
    DEFSIGNAL(SIGWINCH);
#ifndef LISP_FEATURE_HPUX
    DEFSIGNAL(SIGXCPU);
    DEFSIGNAL(SIGXFSZ);
#endif
    return 0;
}
