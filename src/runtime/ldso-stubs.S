/*
 * stubs for C-linkage library functions which we need to refer to 
 * from Lisp 
 *
 * (But note this is only the Linux version, as per the FIXME
 * note in the BSD version in undefineds.h.)
 *
 * These stubs exist for the benefit of Lisp code that needs to refer 
 * to foreign symbols when dlsym() is not available (i.e. when dumping
 * cold-sbcl.core, when we may be running in a host that's not SBCL,
 * or on platforms that don't have it at all). If the runtime is
 * dynamically linked, library functions won't be linked into it, so
 * the map file won't show them. So, we need a bunch of stubs that
 * nm(1) _can_ see.  
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

#if defined __i386__
	
#define LDSO_STUBIFY(fct)                       \
	.align 16 ;                             \
.globl ldso_stub__ ## fct ;                     \
	.type	 ldso_stub__ ## fct,@function ; \
ldso_stub__ ## fct: ;                           \
	jmp fct ;                               \
.L ## fct ## e1: ;                              \
	.size	 ldso_stub__ ## fct,.L ## fct ## e1-ldso_stub__ ## fct ;

#elif defined alpha
	
	/* I _hope_ this is correct - I haven't checked in the manual
 	 * yet.  It works to the point of building and passing tests,
	 * at any rate     - dan 2001.05.10 */
#define LDSO_STUBIFY(fct)                       \
.globl ldso_stub__ ## fct ;                     \
	.type	 ldso_stub__ ## fct,@function ; \
ldso_stub__ ## fct: ;                           \
	jmp fct ;                               \
.L ## fct ## e1: ;                              \
	.size	 ldso_stub__ ## fct,.L ## fct ## e1-ldso_stub__ ## fct ;
	
#else
#error unsupported CPU architecture
#endif
	
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
 LDSO_STUBIFY(sigsetmask)
 LDSO_STUBIFY(sinh)
 LDSO_STUBIFY(socket)
 LDSO_STUBIFY(stat)
 LDSO_STUBIFY(strerror)
 LDSO_STUBIFY(strlen)
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

/*
 * These aren't needed on the X86 because they're microcoded into the
 * FPU, so the Lisp VOPs can implement them directly without having to
 * call C code.
 *
 * Note: There might be some other functions in this category as well.
 * E.g. I notice tanh() and acos() in the list above.. -- WHN 2001-06-07
 */
#if !defined __i386__
 LDSO_STUBIFY(sin)
 LDSO_STUBIFY(cos) 
 LDSO_STUBIFY(tan)      
 LDSO_STUBIFY(atan) 
 LDSO_STUBIFY(atan2) 
 LDSO_STUBIFY(exp)      
 LDSO_STUBIFY(log) 
 LDSO_STUBIFY(log10) 
 LDSO_STUBIFY(sqrt) 
#endif
#if defined alpha
 LDSO_STUBIFY(ieee_get_fp_control)
 LDSO_STUBIFY(ieee_set_fp_control)
#endif
