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
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <errno.h>

#ifdef LISP_FEATURE_OPENBSD
#include <util.h>
#endif

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

#if defined(LISP_FEATURE_OPENBSD)

int
set_pty(char *pty_name)
{
    int fd;

    if ((fd = open(pty_name, O_RDWR, 0)) == -1 ||
        login_tty(fd) == -1)
        return (0);
    return (set_noecho(STDIN_FILENO));
}

#else /* !LISP_FEATURE_OPENBSD */

int
set_pty(char *pty_name)
{
    int fd;

#if !defined(LISP_FEATURE_HPUX) && !defined(SVR4)
    fd = open("/dev/tty", O_RDWR, 0);
    if (fd >= 0) {
        ioctl(fd, TIOCNOTTY, 0);
        close(fd);
    }
#endif
    if ((fd = open(pty_name, O_RDWR, 0)) == -1)
        return (-1);
    dup2(fd, 0);
    set_noecho(0);
    dup2(fd, 1);
    dup2(fd, 2);
    close(fd);
    return (0);
}

#endif /* !LISP_FEATURE_OPENBSD */

int wait_for_exec(int pid, int channel[2]) {
    if ((-1 != pid) && (-1 != channel[1])) {
        int child_errno = 0;
        int bytes = sizeof(int);
        int n;
        char *p = (char*)&child_errno;
        close(channel[1]);
        /* Try to read child errno from channel. */
        while ((bytes > 0) &&
               (n = read(channel[0], p, bytes))) {
            if (-1 == n) {
                if (EINTR == errno) {
                    continue;
                } else {
                    break;
                }
            } else {
                bytes -= n;
                p += n;
            }
        }
        close(channel[0]);
        if (child_errno) {
            int status;
            waitpid(pid, &status, 0);
            /* Our convention to tell Lisp that it was the exec or
               chdir that failed, not the fork. */
            /* FIXME: there are other values waitpid(2) can return. */
            if (WIFEXITED(status)) {
                pid = -WEXITSTATUS(status);
            }
            errno = child_errno;
        }
    }
    return pid;
}

extern char **environ;
int spawn(char *program, char *argv[], int sin, int sout, int serr,
          int search, char *envp[], char *pty_name,
          int channel[2],
          char *pwd)
{
    pid_t pid;
    int fd;
    sigset_t sset;
    int failure_code = 2;

    channel[0] = -1;
    channel[1] = -1;
    if (!pipe(channel)) {
        if (-1==fcntl(channel[1], F_SETFD,  FD_CLOEXEC)) {
            close(channel[1]);
            channel[1] = -1;
        }
    }

    pid = fork();
    if (pid) {
        return pid;
    }
    close (channel[0]);

    /* Put us in our own process group, but only if we need not
     * share stdin with our parent. In the latter case we claim
     * control of the terminal. */
    if (sin >= 0) {
#if defined(LISP_FEATURE_HPUX) || defined(LISP_FEATURE_OPENBSD)
      setsid();
#elif defined(LISP_FEATURE_DARWIN)
      setpgid(0, getpid());
#elif defined(SVR4) || defined(__linux__) || defined(__osf__) || defined(__GLIBC__)
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
    if (pty_name)
        set_pty(pty_name);
    else {
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
        if (fd != channel[1]) close(fd);
#else
    for (fd = getdtablesize()-1; fd >= 3; fd--)
        if (fd != channel[1]) close(fd);
#endif

    if (pwd && chdir(pwd) < 0) {
       failure_code = 3;
    } else {
        if (envp) {
            environ = envp;
        }
        /* Exec the program. */
        if (search)
            execvp(program, argv);
        else
            execv(program, argv);
    }

    /* When exec or chdir fails and channel is available, send the errno value. */
    if (-1 != channel[1]) {
        int our_errno = errno;
        int bytes = sizeof(int);
        int n;
        char *p = (char*)&our_errno;
        while ((bytes > 0) &&
               (n = write(channel[1], p, bytes))) {
            if (-1 == n) {
                if (EINTR == errno) {
                    continue;
                } else {
                    break;
                }
            } else {
                bytes -= n;
                p += n;
            }
        }
        close(channel[1]);
    }
    _exit(failure_code);
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
    int wait,
    char *pwd
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

    /* Change working directory if supplied. */
    if (pwd) {
        if (chdir(pwd) < 0) {
            goto error_exit;
        }
    }

    /* Spawn process given on the command line*/
    if (search)
        hProcess = (HANDLE) spawnvp ( wait_mode, program, (char* const* )argv );
    else
        hProcess = (HANDLE) spawnv ( wait_mode, program, (char* const* )argv );

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
