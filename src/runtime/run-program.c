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

#include <stdlib.h>
#include <sys/file.h>
#include <sys/types.h>
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

int spawn(char *program, char *argv[], char *envp[], char *pty_name,
	  int stdin, int stdout, int stderr)
{
    int pid = fork();
    int fd;

    if (pid != 0)
	return pid;

    /* Put us in our own process group. */
#if defined(hpux)
    setsid();
#elif defined(SVR4) || defined(__linux__) || defined(__osf__)
    setpgrp();
#else
    setpgrp(0, getpid());
#endif

    /* If we are supposed to be part of some other pty, go for it. */
    if (pty_name) {
#if !defined(hpux) && !defined(SVR4)
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
    if (stdin >= 0)
	dup2(stdin, 0);
    if (stdout >= 0)
	dup2(stdout, 1);
    if (stderr >= 0)
	dup2(stderr, 2);
    }
    /* Close all other fds. */
#ifdef SVR4
    for (fd = sysconf(_SC_OPEN_MAX)-1; fd >= 3; fd--)
	close(fd);
#else
    for (fd = getdtablesize()-1; fd >= 3; fd--)
	close(fd);
#endif

    /* Exec the program. */
    execve(program, argv, envp);

    /* It didn't work, so try /bin/sh. */
    argv[0] = program;
    argv[-1] = "sh";
    execve("/bin/sh", argv-1, envp);

    /* The exec didn't work, flame out. */
    exit(1);
}
