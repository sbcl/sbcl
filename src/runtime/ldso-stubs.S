/*
 * stubs for C-linkage library functions used by the runtime
 *
 * These are needed because the locations of the libraries are
 * filled in by the dynamic linker ld.so at runtime.
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

	.file	"ldso-stubs.S"
	.version	"01.01"
gcc2_compiled.:
        .text

#define LDSO_STUBIFY(fct) \
	.align 16 ; \
.globl ldso_stub__ ## fct ; \
	.type	 ldso_stub__ ## fct,@function ; \
ldso_stub__ ## fct: ; \
	jmp fct ; \
.L ## fct ## e1: ; \
	.size	 ldso_stub__ ## fct,.L ## fct ## e1-ldso_stub__ ## fct ;

 LDSO_STUBIFY(accept)
 LDSO_STUBIFY(access)
 LDSO_STUBIFY(acos)
 LDSO_STUBIFY(acosh)
 LDSO_STUBIFY(asin)
 LDSO_STUBIFY(asinh)
 LDSO_STUBIFY(atanh)
 LDSO_STUBIFY(bind)
 LDSO_STUBIFY(cfgetispeed)
 LDSO_STUBIFY(cfgetospeed)
 LDSO_STUBIFY(cfsetispeed)
 LDSO_STUBIFY(cfsetospeed)
 LDSO_STUBIFY(chmod)
 LDSO_STUBIFY(chown)
 LDSO_STUBIFY(close)
 LDSO_STUBIFY(closedir)
 LDSO_STUBIFY(connect)
 LDSO_STUBIFY(cosh)
 LDSO_STUBIFY(creat)
 LDSO_STUBIFY(dlclose)
 LDSO_STUBIFY(dlerror)
 LDSO_STUBIFY(dlopen)
 LDSO_STUBIFY(dlsym)
 LDSO_STUBIFY(dup)
 LDSO_STUBIFY(dup2)
 LDSO_STUBIFY(execve)
 LDSO_STUBIFY(exit)
 LDSO_STUBIFY(fchmod)
 LDSO_STUBIFY(fchown)
 LDSO_STUBIFY(fcntl)
 LDSO_STUBIFY(fork)
 LDSO_STUBIFY(free)
 LDSO_STUBIFY(fstat)
 LDSO_STUBIFY(fsync)
 LDSO_STUBIFY(ftruncate)
 LDSO_STUBIFY(getcwd)
 LDSO_STUBIFY(getdtablesize)
 LDSO_STUBIFY(getegid)
 LDSO_STUBIFY(getenv)
 LDSO_STUBIFY(getgid)
 LDSO_STUBIFY(gethostid)
 LDSO_STUBIFY(gethostbyaddr)
 LDSO_STUBIFY(gethostbyname)
 LDSO_STUBIFY(gethostname)
 LDSO_STUBIFY(getitimer)
 LDSO_STUBIFY(getpagesize)
 LDSO_STUBIFY(getpeername)
 LDSO_STUBIFY(getpgrp)
 LDSO_STUBIFY(getpid)
 LDSO_STUBIFY(getppid)
 LDSO_STUBIFY(getrusage)
 LDSO_STUBIFY(getsockname)
 LDSO_STUBIFY(gettimeofday)
 LDSO_STUBIFY(getuid)
 LDSO_STUBIFY(hypot)
 LDSO_STUBIFY(ioctl)
 LDSO_STUBIFY(isatty)
 LDSO_STUBIFY(kill)
 LDSO_STUBIFY(killpg)
 LDSO_STUBIFY(link)
 LDSO_STUBIFY(listen)
 LDSO_STUBIFY(log1p)
 LDSO_STUBIFY(lseek)
 LDSO_STUBIFY(lstat)
 LDSO_STUBIFY(malloc)
 LDSO_STUBIFY(memmove)
 LDSO_STUBIFY(mkdir)
 LDSO_STUBIFY(open)
 LDSO_STUBIFY(opendir)
 LDSO_STUBIFY(pipe)
 LDSO_STUBIFY(pow)
 LDSO_STUBIFY(read)
 LDSO_STUBIFY(readdir)
 LDSO_STUBIFY(readlink)
 LDSO_STUBIFY(recv)
 LDSO_STUBIFY(rename)
 LDSO_STUBIFY(rmdir)
 LDSO_STUBIFY(select)
 LDSO_STUBIFY(send)
 LDSO_STUBIFY(setitimer)
 LDSO_STUBIFY(setpgrp)
 LDSO_STUBIFY(sigblock)
 LDSO_STUBIFY(sigpause)
 LDSO_STUBIFY(sigsetmask)
 LDSO_STUBIFY(sinh)
 LDSO_STUBIFY(socket)
 LDSO_STUBIFY(stat)
 LDSO_STUBIFY(strerror)
 LDSO_STUBIFY(symlink)
 LDSO_STUBIFY(sync)
 LDSO_STUBIFY(tanh)
 LDSO_STUBIFY(tcdrain)
 LDSO_STUBIFY(tcflow)
 LDSO_STUBIFY(tcflush)
 LDSO_STUBIFY(tcgetattr)
 LDSO_STUBIFY(tcsendbreak)
 LDSO_STUBIFY(tcsetattr)
 LDSO_STUBIFY(truncate)
 LDSO_STUBIFY(ttyname)
 LDSO_STUBIFY(tzname)
 LDSO_STUBIFY(unlink)
 LDSO_STUBIFY(utimes)
 LDSO_STUBIFY(wait3)	
 LDSO_STUBIFY(write)
