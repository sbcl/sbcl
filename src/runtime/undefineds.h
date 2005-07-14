/*
 * routines that must be linked into the core for Lisp to work
 *
 * but note this is only the BSD version, as per the FIXME
 *
 * FIXME: It's tedious and error-prone having to edit both this file and
 * the analogous ldso-stubs.S file when we change the references to
 * functions, enough so that it would probably be good to rewrite
 * both files in terms of a shared list of function names.
 * E.g. the function names could be in shared-function-names.h
 *   SHARED_FUNCTION(cos)
 *   SHARED_FUNCTION(sinh)
 *   SHARED_FUNCTION(strlen)
 * etc. and the per-OS files could look like
 *   #define SHARED_FUNCTION(f) ....
 *   #include "shared-function-names.h"
 *   ...then going on to do OS-specific things
 * "Once and only once."
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

/* Pick up all the syscalls. */
F(accept)
F(access)
F(acct)
F(bind)
F(brk)
#if defined(hpux) \
     || defined(SVR4) \
     || defined(__FreeBSD__) \
     || defined(__OpenBSD__) \
     || defined(__NetBSD__)
F(cfgetospeed)
F(cfsetospeed)
F(cfgetispeed)
F(cfsetispeed)
#endif
F(chdir)
F(chmod)
F(chown)
F(chroot)
F(close)
F(connect)
F(creat)
F(dup)
F(dup2)
F(execve)
F(exit)
F(fchmod)
F(fchown)
F(fcntl)
#if !defined(hpux) && !defined(SVR4)
F(flock)
#endif
F(fork)
F(fstat)
F(fsync)
F(ftruncate)
#if !defined(hpux) && !defined(SVR4) || defined(SOLARIS25) || defined(irix)
F(getdtablesize)
#endif
F(getegid)
F(geteuid)
F(getgid)
F(getgroups)
F(gethostname)
F(getitimer)
#if !defined(hpux) && !defined(SVR4) || defined(SOLARIS25)
F(getpagesize)
#endif
F(getpeername)
F(getpgrp)
F(getpid)
F(getppid)
#if !defined(SVR4)  ||  defined(SOLARIS25)
F(getpriority)
#endif
F(getrlimit)
#if !defined(SOLARIS) ||  defined(SOLARIS25)
F(getrusage)
#endif
F(getsockname)
F(getsockopt)
F(gettimeofday)
F(getuid)
F(ioctl)
F(kill)
#if !defined(SOLARIS) || defined(SOLARIS25)
F(killpg)
#endif
F(link)
F(listen)
F(lseek)
F(lstat)
F(mkdir)
F(mknod)
F(mmap)
F(mount)
F(munmap)
F(open)
F(pipe)
F(profil)
F(ptrace)
#ifdef mach
F(quota)
#endif
F(read)
F(readlink)
F(readv)
#ifndef SVR4
F(reboot)
#endif
F(recv)
F(recvfrom)
F(recvmsg)
F(rename)
F(rmdir)
F(sbrk)
F(select)
F(send)
F(sendmsg)
F(sendto)
F(setgroups)
#if !defined(SUNOS) && !(defined(SOLARIS) ||  defined(SOLARIS25))
F(sethostid)
#endif
#if !defined(SVR4) ||  defined(SOLARIS25)
F(sethostname)
#endif
F(setitimer)
F(setpgrp)
F(setsid)
#if !defined(SVR4) ||  defined(SOLARIS25)
F(setpriority)
#endif
F(setrlimit)
F(setsockopt)
F(settimeofday)
F(shutdown)
#if !defined(hpux) && !defined(SVR4) && !defined(LISP_FEATURE_X86)
F(sigreturn)
#endif
#if !defined(SVR4)
F(sigsetmask)
#endif
#if !defined(SVR4) && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__)
F(sigstack)
F(sigvec)
#endif
F(socket)
F(socketpair)
F(stat)
#if !defined(SVR4) && !defined(__OpenBSD__)
F(swapon)
#endif
F(symlink)
F(sync)
F(syscall)
#if defined(hpux) || defined(SVR4) || defined(__linux__)
F(closedir)
F(opendir)
#if defined(readdir)
#undef reddir
#endif
F(readdir)
#endif
#if defined(hpux) \
     || defined(SVR4) \
     || defined(__FreeBSD__) \
     || defined(__OpenBSD__) \
     || defined(__NetBSD__) \
     || defined(__linux__)
F(tcgetattr)
F(tcsetattr)
F(tcsendbreak)
F(tcdrain)
F(tcflush)
F(tcflow)
#endif
#if defined(SOLARIS)
F(times)
#endif
F(truncate)
F(umask)
#if !defined(SUNOS) \
     && !defined(parisc) \
     && !defined(SOLARIS) \
     && !defined(__OpenBSD__) \
     && !defined(__FreeBSD__) \
     && !defined(__NetBSD__)
F(umount)
#endif
F(unlink)
#ifndef hpux
F(utimes)
#endif
#ifndef irix
F(vfork)
#endif
#if !defined(osf1) && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__)
F(vhangup)
#endif
F(wait)
#if !defined(SOLARIS) ||  defined(SOLARIS25)
F(wait3)
#endif
F(write)
F(writev)

/* math routines */
F(cos)
F(sin)
F(tan)
F(acos)
F(asin)
F(atan)
F(atan2)
F(sinh)
F(cosh)
F(tanh)
F(asinh)
F(acosh)
F(atanh)
F(exp)
#ifndef hpux
F(expm1)
#endif
F(log)
F(log10)
#ifndef hpux
F(log1p)
#endif
F(pow)
#ifndef hpux
F(cbrt)
#endif
#ifndef LISP_FEATURE_X86
F(sqrt)
#endif
F(hypot)

/* string things */
F(strlen)

/* network support */
F(gethostbyname)
F(gethostbyaddr)

/* other miscellaneous things */
/* FIXME: NetBSD plays some renaming games, and so cannot simply
   reference symbols here.  "NetBSD needs to get fixed here too PEM
   2004-03-27" */
#if defined(SVR4) || defined(__FreeBSD__)
F(setpgid)
F(getpgid)
D(timezone)
#if !defined(__FreeBSD__)
D(altzone)
D(daylight)
#endif
D(tzname)
#endif
F(getcwd)
F(ttyname)

#ifdef irix
F(_getpty)
#endif

F(dlopen)
F(dlsym)
F(dlclose)
F(dlerror)
