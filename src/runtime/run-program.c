/*
 * Unix support for the Lisp function RUN-PROGRAM and friends
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
#include <dirent.h>
#include "interr.h" // for lose()

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

#if !defined(SVR4) && !defined(__HAIKU__)
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

void closefrom_fallback(int lowfd)
{
    int fd, maxfd;

#ifdef SVR4
    maxfd = sysconf(_SC_OPEN_MAX)-1;
#else
    maxfd = getdtablesize()-1;
#endif

    for (fd = maxfd; fd >= lowfd; fd--)
        close(fd);
}

int closefrom_fddir(char *dir, int lowfd)
{
    DIR *d;
    struct dirent *ent;
    int fd;

    /* This may fail if e.g. the program is running in
     * a chroot that does not include /proc, or potentially
     * on old kernel versions. */
    d = opendir(dir);
    if (!d) return -1;

    for (ent = readdir(d); ent; ent = readdir(d)) {
        /* atoi will return bogus values for certain inputs, but lowfd will
         * prevent us from closing anything we care about. */
        fd = atoi(ent->d_name);
        if (fd >= 0 && fd >= lowfd)
            close(fd);
    }
    closedir(d);
    return 0;
}

void closefds_from(int lowfd, int* dont_close)
{
    if (dont_close) {
        /* dont_close is a sorted simple-array of tagged ints */
        uword_t length = fixnum_value(((uword_t*)dont_close)[-1]);
        uword_t i;
        for (i = 0; i < length; i++)
        {
            int fd = dont_close[i];
            int close_fd;

            /* Close the gaps between the fds */
            for (close_fd = lowfd; close_fd < fd; close_fd++)
            {
                close(close_fd);
            }
            lowfd = fd+1;
        }
    }

#if defined(LISP_FEATURE_OPENBSD) || defined(LISP_FEATURE_NETBSD)       \
    || defined(LISP_FEATURE_DRAGONFLY) || defined(LISP_FEATURE_FREEBSD) \
    || defined(LISP_FEATURE_SUNOS)
    closefrom(lowfd);
#else
    int fds_closed = 0;

/* readdir() uses malloc, which is prone to deadlocking
#ifdef LISP_FEATURE_LINUX
    if (!fds_closed)
        fds_closed = !closefrom_fddir("/proc/self/fd/", lowfd);
#endif
#ifdef LISP_FEATURE_DARWIN
    if (!fds_closed)
        fds_closed = !closefrom_fddir("/dev/fd/", lowfd);
#endif
*/

    if (!fds_closed)
        closefrom_fallback(lowfd);
#endif
}

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
          char *pwd, int* dont_close)
{
    pid_t pid;
    sigset_t sset;
    int failure_code = 2;

    channel[0] = -1;
    channel[1] = -1;
    // Surely we can do better than to lose()
    if (pipe(channel)) lose("can't run-program");

    pid = fork();
    if (pid) {
        return pid;
    }
    close (channel[0]);

    /* Put us in our own process group, but only if we need not
     * share stdin with our parent. In the latter case we claim
     * control of the terminal. */
    if (sin >= 0) {
#ifdef LISP_FEATURE_OPENBSD
      setsid();
#elif defined(LISP_FEATURE_DARWIN)
      setpgid(0, getpid());
#elif defined SVR4 || defined __linux__ || defined __osf__ || defined __GLIBC__ || defined __HAIKU__
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
    /* Close all other fds. First arrange for the pipe fd to be the
     * lowest free fd, then close every open fd above that. */
    channel[1] = dup2(channel[1], 3);
    closefds_from(4, dont_close);

    if (-1 != channel[1]) {
        if (-1==fcntl(channel[1], F_SETFD,  FD_CLOEXEC)) {
            close(channel[1]);
            channel[1] = -1;
        }
    }

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

