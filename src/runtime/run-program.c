/*
 * support for the Lisp function RUN-PROGRAM and friends
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

#include "sbcl.h"

#ifndef LISP_FEATURE_WIN32

#include <stdlib.h>
#include <sys/file.h>
#include <sys/types.h>
#include <signal.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <unistd.h>

#include <sys/ioctl.h>
#include <termios.h>


/* borrowed from detachtty's detachtty.c, in turn borrowed from APUE
 * example code found at
 * http://www.yendor.com/programming/unix/apue/pty/main.c

-brkint

 */

int set_noecho(int fd)
{
    struct termios  stermios;

    if (tcgetattr(fd, &stermios) < 0) return 0;

    stermios.c_lflag &= ~(  ECHO | /* ECHOE |  ECHOK | */  ECHONL);
    stermios.c_oflag |= (ONLCR);
    stermios.c_iflag &= ~(BRKINT);
    stermios.c_iflag |= (ICANON|ICRNL);

    stermios.c_cc[VERASE]=0177;
    if (tcsetattr(fd, TCSANOW, &stermios) < 0) return 0;
    return 1;
}

extern char **environ;
int spawn(char *program, char *argv[], int sin, int sout, int serr,
          int search, char *envp[], char *pty_name, int wait)
{
    int pid = fork();
    int fd;
    sigset_t sset;

    if (pid != 0)
        return pid;

    /* Put us in our own process group, but only if we need not
     * share stdin with our parent. In the latter case we claim
     * control of the terminal. */
    if (sin >= 0) {
#if defined(LISP_FEATURE_HPUX)
      setsid();
#elif defined(LISP_FEATURE_DARWIN)
      setpgid(0, getpid());
#elif defined(SVR4) || defined(__linux__) || defined(__osf__)
      setpgrp();
#else
      setpgrp(0, getpid());
#endif
    } else {
      tcsetpgrp(0, getpgrp());
    }

    /* unblock signals */
    sigemptyset(&sset);
    sigprocmask(SIG_SETMASK, &sset, NULL);

    /* If we are supposed to be part of some other pty, go for it. */
    if (pty_name) {
#if !defined(LISP_FEATURE_HPUX) && !defined(SVR4)
        fd = open("/dev/tty", O_RDWR, 0);
        if (fd >= 0) {
            ioctl(fd, TIOCNOTTY, 0);
            close(fd);
        }
#endif
        fd = open(pty_name, O_RDWR, 0);
        dup2(fd, 0);
        set_noecho(0);
        dup2(fd, 1);
        dup2(fd, 2);
        close(fd);
    } else{
    /* Set up stdin, stdout, and stderr */
    if (sin >= 0)
        dup2(sin, 0);
    if (sout >= 0)
        dup2(sout, 1);
    if (serr >= 0)
        dup2(serr, 2);
    }
    /* Close all other fds. */
#ifdef SVR4
    for (fd = sysconf(_SC_OPEN_MAX)-1; fd >= 3; fd--)
        close(fd);
#else
    for (fd = getdtablesize()-1; fd >= 3; fd--)
        close(fd);
#endif

    environ = envp;
    /* Exec the program. */
    if (search)
      execvp(program, argv);
    else
      execv(program, argv);

    exit (1);
}
#else  /* !LISP_FEATURE_WIN32 */

#  include <windows.h>
#  include <process.h>
#  include <stdio.h>
#  include <stdlib.h>
#  include <fcntl.h>
#  include <io.h>

#define   READ_HANDLE  0
#define   WRITE_HANDLE 1

/* These functions do not attempt to deal with wchar_t variations. */

/* Get the value of _environ maintained by MSVCRT */
char **msvcrt_environ ( void ) {
    return ( _environ );
}

/* Set up in, out, err pipes and spawn a program, waiting or otherwise. */
HANDLE spawn (
    const char *program,
    const char *const *argv,
    int in,
    int out,
    int err,
    int search,
    char *envp,
    char *ptyname,
    int wait
    )
{
    int stdout_backup, stdin_backup, stderr_backup, wait_mode;
    HANDLE hProcess;
    HANDLE hReturn;

    /* Duplicate and save the original stdin/out/err handles. */
    stdout_backup = _dup (  _fileno ( stdout ) );
    stdin_backup  = _dup (  _fileno ( stdin  ) );
    stderr_backup = _dup (  _fileno ( stderr ) );

    /* If we are not using stdin/out/err
     * then duplicate the new pipes to current stdin/out/err handles.
     *
     * Default std fds are used if in, out or err parameters
     * are -1. */

    hReturn = (HANDLE)-1;
    hProcess = (HANDLE)-1;
    if ( ( out >= 0 ) && ( out != _fileno ( stdout ) ) ) {
        if ( _dup2 ( out, _fileno ( stdout ) ) != 0 ) goto error_exit;
    }
    if ( ( in >= 0 ) && ( in != _fileno ( stdin ) ) ) {
        if ( _dup2 ( in,  _fileno ( stdin )  ) != 0 ) goto error_exit_out;
    }
    if ( ( err >= 0 ) && ( err != _fileno ( stderr ) ) ) {
        if ( _dup2 ( err, _fileno ( stderr ) ) != 0 ) goto error_exit_in;
    }

    /* Set the wait mode. */
    if ( 0 == wait ) {
        wait_mode = P_NOWAIT;
    } else {
        wait_mode = P_WAIT;
    }

    /* Spawn process given on the command line*/
    if (search)
        hProcess = (HANDLE) spawnvp ( wait_mode, program, argv );
    else
        hProcess = (HANDLE) spawnv ( wait_mode, program, argv );

    /* Now that the process is launched, replace the original
     * in/out/err handles and close the backups. */

    if ( _dup2 ( stderr_backup, _fileno ( stderr ) ) != 0 ) goto error_exit;
 error_exit_in:
    if ( _dup2 ( stdin_backup,  _fileno ( stdin )  ) != 0 ) goto error_exit;
 error_exit_out:
    if ( _dup2 ( stdout_backup, _fileno ( stdout ) ) != 0 ) goto error_exit;

    hReturn = hProcess;

 error_exit:
    close ( stdout_backup );
    close ( stdin_backup  );
    close ( stderr_backup );

    return hReturn;

}


#endif /* !LISP_FEATURE_WIN32 */
