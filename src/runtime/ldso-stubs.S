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
 LDSO_STUBIFY(chdir)
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
 LDSO_STUBIFY(getdtablesize)
 LDSO_STUBIFY(getegid)
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
 LDSO_STUBIFY(getwd)
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
 LDSO_STUBIFY(setregid)
 LDSO_STUBIFY(setreuid)
 LDSO_STUBIFY(sigblock)
 LDSO_STUBIFY(sigpause)
 LDSO_STUBIFY(sigsetmask)
 LDSO_STUBIFY(sinh)
 LDSO_STUBIFY(socket)
 LDSO_STUBIFY(stat)
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

/*  LDSO_STUBIFY(abort) */
/*  LDSO_STUBIFY(abs) */
/*  LDSO_STUBIFY(accept) */
/*  LDSO_STUBIFY(access) */
/*  LDSO_STUBIFY(acct) */
/*  LDSO_STUBIFY(acos) */
/*  LDSO_STUBIFY(acosh) */
/*  LDSO_STUBIFY(acoshl) */
/*  LDSO_STUBIFY(acosl) */
/*  LDSO_STUBIFY(addmntent) */
/*  LDSO_STUBIFY(adjtime) */
/*  LDSO_STUBIFY(adjtimex) */
/*  LDSO_STUBIFY(alarm) */
/*  LDSO_STUBIFY(alphasort) */
/*  LDSO_STUBIFY(altzone) */
/*  LDSO_STUBIFY(asctime) */
/*  LDSO_STUBIFY(asin) */
/*  LDSO_STUBIFY(asinh) */
/*  LDSO_STUBIFY(asinhl) */
/*  LDSO_STUBIFY(asinl) */
/*  LDSO_STUBIFY(atan) */
/*  LDSO_STUBIFY(atan2) */
/*  LDSO_STUBIFY(atan2l) */
/*  LDSO_STUBIFY(atanh) */
/*  LDSO_STUBIFY(atanhl) */
/*  LDSO_STUBIFY(atanl) */
/*  LDSO_STUBIFY(atexit) */
/*  LDSO_STUBIFY(atof) */
/*  LDSO_STUBIFY(atoi) */
/*  LDSO_STUBIFY(atol) */
/*  LDSO_STUBIFY(authdes_create) */
/*  LDSO_STUBIFY(authnone_create) */
/*  LDSO_STUBIFY(authunix_create) */
/*  LDSO_STUBIFY(authunix_create_default) */
/*  LDSO_STUBIFY(basename) */
/*  LDSO_STUBIFY(bcmp) */
/*  LDSO_STUBIFY(bcopy) */
/*  LDSO_STUBIFY(bind) */
/*  LDSO_STUBIFY(bindresvport) */
/*  LDSO_STUBIFY(brk) */
/*  LDSO_STUBIFY(bsearch) */
/*  LDSO_STUBIFY(bzero) */
/*  LDSO_STUBIFY(calloc) */
/*  LDSO_STUBIFY(callrpc) */
/*  LDSO_STUBIFY(catclose) */
/*  LDSO_STUBIFY(catgets) */
/*  LDSO_STUBIFY(catopen) */
/*  LDSO_STUBIFY(cbc_crypt) */
/*  LDSO_STUBIFY(cbrt) */
/*  LDSO_STUBIFY(cbrtl) */
/*  LDSO_STUBIFY(ceil) */
/*  LDSO_STUBIFY(ceill) */
/*  LDSO_STUBIFY(cfgetispeed) */
/*  LDSO_STUBIFY(cfgetospeed) */
/*  LDSO_STUBIFY(cfmakeraw) */
/*  LDSO_STUBIFY(cfree) */
/*  LDSO_STUBIFY(cfsetispeed) */
/*  LDSO_STUBIFY(cfsetospeed) */
/*  LDSO_STUBIFY(chdir) */
/*  LDSO_STUBIFY(chmod) */
/*  LDSO_STUBIFY(chown) */
/*  LDSO_STUBIFY(chroot) */
/*  LDSO_STUBIFY(clearerr) */
/*  LDSO_STUBIFY(clnt_broadcast) */
/*  LDSO_STUBIFY(clnt_create) */
/*  LDSO_STUBIFY(clnt_pcreateerror) */
/*  LDSO_STUBIFY(clnt_perrno) */
/*  LDSO_STUBIFY(clnt_perror) */
/*  LDSO_STUBIFY(clnt_spcreateerror) */
/*  LDSO_STUBIFY(clnt_sperrno) */
/*  LDSO_STUBIFY(clnt_sperror) */
/*  LDSO_STUBIFY(clntraw_create) */
/*  LDSO_STUBIFY(clnttcp_create) */
/*  LDSO_STUBIFY(clntudp_bufcreate) */
/*  LDSO_STUBIFY(clntudp_create) */
/*  LDSO_STUBIFY(clock) */
/*  LDSO_STUBIFY(close) */
/*  LDSO_STUBIFY(closedir) */
/*  LDSO_STUBIFY(closelog) */
/*  LDSO_STUBIFY(confstr) */
/*  LDSO_STUBIFY(connect) */
/*  LDSO_STUBIFY(copysign) */
/*  LDSO_STUBIFY(copysignl) */
/*  LDSO_STUBIFY(cos) */
/*  LDSO_STUBIFY(cosh) */
/*  LDSO_STUBIFY(coshl) */
/*  LDSO_STUBIFY(cosl) */
/*  LDSO_STUBIFY(creat) */
/*  LDSO_STUBIFY(crypt) */
/*  LDSO_STUBIFY(ctermid) */
/*  LDSO_STUBIFY(ctime) */
/*  LDSO_STUBIFY(ctime_r) */
/*  LDSO_STUBIFY(cuserid) */
/*  LDSO_STUBIFY(daylight) */
/*  LDSO_STUBIFY(des_setparity) */
/*  LDSO_STUBIFY(difftime) */
/*  LDSO_STUBIFY(dirfd) */
/*  LDSO_STUBIFY(div) */
/*  LDSO_STUBIFY(dlclose) */
/*  LDSO_STUBIFY(dlerror) */
/*  LDSO_STUBIFY(dlopen) */
/*  LDSO_STUBIFY(dlsym) */
/*  LDSO_STUBIFY(dn_comp) */
/*  LDSO_STUBIFY(dn_expand) */
/*  LDSO_STUBIFY(dn_skipname) */
/*  LDSO_STUBIFY(drand48) */
/*  LDSO_STUBIFY(drem) */
/*  LDSO_STUBIFY(dup) */
/*  LDSO_STUBIFY(dup2) */
/*  LDSO_STUBIFY(ecb_crypt) */
/*  LDSO_STUBIFY(ecvt) */
/*  LDSO_STUBIFY(encrypt) */
/*  LDSO_STUBIFY(endgrent) */
/*  LDSO_STUBIFY(endhostent) */
/*  LDSO_STUBIFY(endmntent) */
/*  LDSO_STUBIFY(endnetent) */
/*  LDSO_STUBIFY(endprotoent) */
/*  LDSO_STUBIFY(endpwent) */
/*  LDSO_STUBIFY(endrpcent) */
/*  LDSO_STUBIFY(endservent) */
/*  LDSO_STUBIFY(endsgent) */
/*  LDSO_STUBIFY(endspent) */
/*  LDSO_STUBIFY(endusershell) */
/*  LDSO_STUBIFY(endutent) */
/*  LDSO_STUBIFY(erand48) */
/*  LDSO_STUBIFY(erf) */
/*  LDSO_STUBIFY(erfc) */
/*  LDSO_STUBIFY(erfcl) */
/*  LDSO_STUBIFY(erfl) */
/*  LDSO_STUBIFY(execl) */
/*  LDSO_STUBIFY(execle) */
/*  LDSO_STUBIFY(execlp) */
/*  LDSO_STUBIFY(execv) */
/*  LDSO_STUBIFY(execve) */
/*  LDSO_STUBIFY(execvp) */
/*  LDSO_STUBIFY(exit) */
/*  LDSO_STUBIFY(exp) */
/*  LDSO_STUBIFY(expl) */
/*  LDSO_STUBIFY(expm1) */
/*  LDSO_STUBIFY(expm1l) */
/*  LDSO_STUBIFY(fabs) */
/*  LDSO_STUBIFY(fabsl) */
/*  LDSO_STUBIFY(fchdir) */
/*  LDSO_STUBIFY(fchmod) */
/*  LDSO_STUBIFY(fchown) */
/*  LDSO_STUBIFY(fclose) */
/*  LDSO_STUBIFY(fcntl) */
/*  LDSO_STUBIFY(fcvt) */
/*  LDSO_STUBIFY(fdopen) */
/*  LDSO_STUBIFY(feof) */
/*  LDSO_STUBIFY(ferror) */
/*  LDSO_STUBIFY(fflush) */
/*  LDSO_STUBIFY(ffs) */
/*  LDSO_STUBIFY(fgetc) */
/*  LDSO_STUBIFY(fgetgrent) */
/*  LDSO_STUBIFY(fgetpos) */
/*  LDSO_STUBIFY(fgetpwent) */
/*  LDSO_STUBIFY(fgets) */
/*  LDSO_STUBIFY(fgetsgent) */
/*  LDSO_STUBIFY(fgetspent) */
/*  LDSO_STUBIFY(fileno) */
/*  LDSO_STUBIFY(finite) */
/*  LDSO_STUBIFY(flock) */
/*  LDSO_STUBIFY(flockfile) */
/*  LDSO_STUBIFY(floor) */
/*  LDSO_STUBIFY(floorl) */
/*  LDSO_STUBIFY(fmod) */
/*  LDSO_STUBIFY(fmodl) */
/*  LDSO_STUBIFY(fnmatch) */
/*  LDSO_STUBIFY(fopen) */
/*  LDSO_STUBIFY(fork) */
/*  LDSO_STUBIFY(fp_nquery) */
/*  LDSO_STUBIFY(fp_query) */
/*  LDSO_STUBIFY(fp_resstat) */
/*  LDSO_STUBIFY(fpathconf) */
/*  LDSO_STUBIFY(fpclassifyd) */
/*  LDSO_STUBIFY(fpclassifyf) */
/*  LDSO_STUBIFY(fpclassifyl) */
/*  LDSO_STUBIFY(fprintf) */
/*  LDSO_STUBIFY(fputc) */
/*  LDSO_STUBIFY(fputs) */
/*  LDSO_STUBIFY(fread) */
/*  LDSO_STUBIFY(free) */
/*  LDSO_STUBIFY(freopen) */
/*  LDSO_STUBIFY(frexp) */
/*  LDSO_STUBIFY(frexpl) */
/*  LDSO_STUBIFY(fscanf) */
/*  LDSO_STUBIFY(fseek) */
/*  LDSO_STUBIFY(fsetpos) */
/*  LDSO_STUBIFY(fstat) */
/*  LDSO_STUBIFY(fstatfs) */
/*  LDSO_STUBIFY(fsync) */
/*  LDSO_STUBIFY(ftell) */
/*  LDSO_STUBIFY(ftime) */
/*  LDSO_STUBIFY(ftok) */
/*  LDSO_STUBIFY(ftruncate) */
/*  LDSO_STUBIFY(ftrylockfile) */
/*  LDSO_STUBIFY(ftw) */
/*  LDSO_STUBIFY(funlockfile) */
/*  LDSO_STUBIFY(fwrite) */
/*  LDSO_STUBIFY(gcvt) */
/*  LDSO_STUBIFY(get_current_dir_name) */
/*  LDSO_STUBIFY(get_myaddress) */
/*  LDSO_STUBIFY(getc) */
/*  LDSO_STUBIFY(getchar) */
/*  LDSO_STUBIFY(getcwd) */
/*  LDSO_STUBIFY(getdelim) */
/*  LDSO_STUBIFY(getdents) */
/*  LDSO_STUBIFY(getdirentries) */
/*  LDSO_STUBIFY(getdomainname) */
/*  LDSO_STUBIFY(getdtablesize) */
/*  LDSO_STUBIFY(getegid) */
    LDSO_STUBIFY(getenv)
/*  LDSO_STUBIFY(geteuid) */
/*  LDSO_STUBIFY(getgid) */
/*  LDSO_STUBIFY(getgrent) */
/*  LDSO_STUBIFY(getgrgid) */
/*  LDSO_STUBIFY(getgrnam) */
/*  LDSO_STUBIFY(getgroups) */
/*  LDSO_STUBIFY(gethostbyaddr) */
/*  LDSO_STUBIFY(gethostbyname) */
/*  LDSO_STUBIFY(gethostent) */
/*  LDSO_STUBIFY(gethostid) */
/*  LDSO_STUBIFY(gethostname) */
/*  LDSO_STUBIFY(getitimer) */
/*  LDSO_STUBIFY(getlogin) */
/*  LDSO_STUBIFY(getmntent) */
/*  LDSO_STUBIFY(getnetbyaddr) */
/*  LDSO_STUBIFY(getnetbyname) */
/*  LDSO_STUBIFY(getnetent) */
/*  LDSO_STUBIFY(getopt) */
/*  LDSO_STUBIFY(getopt_long) */
/*  LDSO_STUBIFY(getopt_long_only) */
/*  LDSO_STUBIFY(getpagesize) */
/*  LDSO_STUBIFY(getpass) */
/*  LDSO_STUBIFY(getpeername) */
/*  LDSO_STUBIFY(getpgid) */
/*  LDSO_STUBIFY(getpgrp) */
/*  LDSO_STUBIFY(getpid) */
/*  LDSO_STUBIFY(getppid) */
/*  LDSO_STUBIFY(getpriority) */
/*  LDSO_STUBIFY(getprotobyname) */
/*  LDSO_STUBIFY(getprotobynumber) */
/*  LDSO_STUBIFY(getprotoent) */
/*  LDSO_STUBIFY(getpw) */
/*  LDSO_STUBIFY(getpwent) */
/*  LDSO_STUBIFY(getpwnam) */
/*  LDSO_STUBIFY(getpwuid) */
/*  LDSO_STUBIFY(getrlimit) */
/*  LDSO_STUBIFY(getrpcbyname) */
/*  LDSO_STUBIFY(getrpcbynumber) */
/*  LDSO_STUBIFY(getrpcent) */
/*  LDSO_STUBIFY(getrpcport) */
/*  LDSO_STUBIFY(getrusage) */
/*  LDSO_STUBIFY(gets) */
/*  LDSO_STUBIFY(getservbyname) */
/*  LDSO_STUBIFY(getservbyport) */
/*  LDSO_STUBIFY(getservent) */
/*  LDSO_STUBIFY(getsgent) */
/*  LDSO_STUBIFY(getsgnam) */
/*  LDSO_STUBIFY(getsid) */
/*  LDSO_STUBIFY(getsockname) */
/*  LDSO_STUBIFY(getsockopt) */
/*  LDSO_STUBIFY(getspent) */
/*  LDSO_STUBIFY(getspnam) */
/*  LDSO_STUBIFY(gettimeofday) */
/*  LDSO_STUBIFY(getuid) */
/*  LDSO_STUBIFY(getusershell) */
/*  LDSO_STUBIFY(getutent) */
/*  LDSO_STUBIFY(getutid) */
/*  LDSO_STUBIFY(getutline) */
/*  LDSO_STUBIFY(getw) */
/*  LDSO_STUBIFY(getwd) */
/*  LDSO_STUBIFY(glob) */
/*  LDSO_STUBIFY(globfree) */
/*  LDSO_STUBIFY(gmtime) */
/*  LDSO_STUBIFY(gmtime_r) */
/*  LDSO_STUBIFY(gsignal) */
/*  LDSO_STUBIFY(hasmntopt) */
/*  LDSO_STUBIFY(hcreate) */
/*  LDSO_STUBIFY(hdestroy) */
/*  LDSO_STUBIFY(herror) */
/*  LDSO_STUBIFY(hostalias) */
/*  LDSO_STUBIFY(hsearch) */
/*  LDSO_STUBIFY(htonl) */
/*  LDSO_STUBIFY(htons) */
/*  LDSO_STUBIFY(hypot) */
/*  LDSO_STUBIFY(hypotl) */
/*  LDSO_STUBIFY(idle) */
/*  LDSO_STUBIFY(index) */
/*  LDSO_STUBIFY(inet_addr) */
/*  LDSO_STUBIFY(inet_aton) */
/*  LDSO_STUBIFY(inet_lnaof) */
/*  LDSO_STUBIFY(inet_makeaddr) */
/*  LDSO_STUBIFY(inet_netof) */
/*  LDSO_STUBIFY(inet_network) */
/*  LDSO_STUBIFY(inet_nsap_addr) */
/*  LDSO_STUBIFY(inet_nsap_ntoa) */
/*  LDSO_STUBIFY(inet_ntoa) */
/*  LDSO_STUBIFY(infnan) */
/*  LDSO_STUBIFY(init_des) */
/*  LDSO_STUBIFY(initgroups) */
/*  LDSO_STUBIFY(initstate) */
/*  LDSO_STUBIFY(insque) */
/*  LDSO_STUBIFY(ioctl) */
/*  LDSO_STUBIFY(ioperm) */
/*  LDSO_STUBIFY(iopl) */
/*  LDSO_STUBIFY(ipc) */
/*  LDSO_STUBIFY(isalnum) */
/*  LDSO_STUBIFY(isalpha) */
/*  LDSO_STUBIFY(isascii) */
/*  LDSO_STUBIFY(isatty) */
/*  LDSO_STUBIFY(isblank) */
/*  LDSO_STUBIFY(iscntrl) */
/*  LDSO_STUBIFY(isdigit) */
/*  LDSO_STUBIFY(isgraph) */
/*  LDSO_STUBIFY(isinf) */
/*  LDSO_STUBIFY(isinfl) */
/*  LDSO_STUBIFY(islower) */
/*  LDSO_STUBIFY(isnan) */
/*  LDSO_STUBIFY(isnanl) */
/*  LDSO_STUBIFY(isprint) */
/*  LDSO_STUBIFY(ispunct) */
/*  LDSO_STUBIFY(isspace) */
/*  LDSO_STUBIFY(isupper) */
/*  LDSO_STUBIFY(isxdigit) */
/*  LDSO_STUBIFY(j0) */
/*  LDSO_STUBIFY(j0l) */
/*  LDSO_STUBIFY(j1) */
/*  LDSO_STUBIFY(j1l) */
/*  LDSO_STUBIFY(jn) */
/*  LDSO_STUBIFY(jnl) */
/*  LDSO_STUBIFY(jrand48) */
/*  LDSO_STUBIFY(kill) */
/*  LDSO_STUBIFY(killpg) */
/*  LDSO_STUBIFY(labs) */
/*  LDSO_STUBIFY(lckpwdf) */
/*  LDSO_STUBIFY(lcong48) */
/*  LDSO_STUBIFY(ldexp) */
/*  LDSO_STUBIFY(ldexpl) */
/*  LDSO_STUBIFY(ldiv) */
/*  LDSO_STUBIFY(lfind) */
/*  LDSO_STUBIFY(lgamma) */
/*  LDSO_STUBIFY(lgammal) */
/*  LDSO_STUBIFY(libc_nls_init) */
/*  LDSO_STUBIFY(link) */
/*  LDSO_STUBIFY(listen) */
/*  LDSO_STUBIFY(llseek) */
/*  LDSO_STUBIFY(localeconv) */
/*  LDSO_STUBIFY(localtime) */
/*  LDSO_STUBIFY(localtime_r) */
/*  LDSO_STUBIFY(lockf) */
/*  LDSO_STUBIFY(log) */
/*  LDSO_STUBIFY(log10) */
/*  LDSO_STUBIFY(log10l) */
/*  LDSO_STUBIFY(log1p) */
/*  LDSO_STUBIFY(log1pl) */
/*  LDSO_STUBIFY(log2l) */
/*  LDSO_STUBIFY(logl) */
/*  LDSO_STUBIFY(longjmp) */
/*  LDSO_STUBIFY(lrand48) */
/*  LDSO_STUBIFY(lsearch) */
/*  LDSO_STUBIFY(lseek) */
/*  LDSO_STUBIFY(lstat) */
/*  LDSO_STUBIFY(mallinfo) */
/*  LDSO_STUBIFY(malloc) */
/*  LDSO_STUBIFY(malloc_stats) */
/*  LDSO_STUBIFY(malloc_trim) */
/*  LDSO_STUBIFY(malloc_usable_size) */
/*  LDSO_STUBIFY(mallopt) */
/*  LDSO_STUBIFY(mblen) */
/*  LDSO_STUBIFY(mbstowcs) */
/*  LDSO_STUBIFY(mbtowc) */
/*  LDSO_STUBIFY(memalign) */
/*  LDSO_STUBIFY(memccpy) */
/*  LDSO_STUBIFY(memchr) */
/*  LDSO_STUBIFY(memcmp) */
/*  LDSO_STUBIFY(memcpy) */
/*  LDSO_STUBIFY(memfrob) */
/*  LDSO_STUBIFY(memmem) */
/*  LDSO_STUBIFY(memmove) */
/*  LDSO_STUBIFY(memset) */
/*  LDSO_STUBIFY(mkdir) */
/*  LDSO_STUBIFY(mkfifo) */
/*  LDSO_STUBIFY(mknod) */
/*  LDSO_STUBIFY(mkstemp) */
/*  LDSO_STUBIFY(mktemp) */
/*  LDSO_STUBIFY(mktime) */
/*  LDSO_STUBIFY(mlock) */
/*  LDSO_STUBIFY(mlockall) */
/*  LDSO_STUBIFY(mmap) */
/*  LDSO_STUBIFY(modf) */
/*  LDSO_STUBIFY(modfl) */
/*  LDSO_STUBIFY(mount) */
/*  LDSO_STUBIFY(mprotect) */
/*  LDSO_STUBIFY(mrand48) */
/*  LDSO_STUBIFY(mremap) */
/*  LDSO_STUBIFY(msgctl) */
/*  LDSO_STUBIFY(msgget) */
/*  LDSO_STUBIFY(msgrcv) */
/*  LDSO_STUBIFY(msgsnd) */
/*  LDSO_STUBIFY(msync) */
/*  LDSO_STUBIFY(munlock) */
/*  LDSO_STUBIFY(munlockall) */
/*  LDSO_STUBIFY(munmap) */
/*  LDSO_STUBIFY(nice) */
/*  LDSO_STUBIFY(nl_langinfo) */
/*  LDSO_STUBIFY(nrand48) */
/*  LDSO_STUBIFY(ntohl) */
/*  LDSO_STUBIFY(ntohs) */
/*  LDSO_STUBIFY(obstack_free) */
/*  LDSO_STUBIFY(on_exit) */
/*  LDSO_STUBIFY(open) */
/*  LDSO_STUBIFY(opendir) */
/*  LDSO_STUBIFY(openlog) */
/*  LDSO_STUBIFY(optarg) */
/*  LDSO_STUBIFY(opterr) */
/*  LDSO_STUBIFY(optind) */
/*  LDSO_STUBIFY(optopt) */
/*  LDSO_STUBIFY(p_cdname) */
/*  LDSO_STUBIFY(p_cdnname) */
/*  LDSO_STUBIFY(p_class) */
/*  LDSO_STUBIFY(p_fqname) */
/*  LDSO_STUBIFY(p_option) */
/*  LDSO_STUBIFY(p_query) */
/*  LDSO_STUBIFY(p_rr) */
/*  LDSO_STUBIFY(p_time) */
/*  LDSO_STUBIFY(p_type) */
/*  LDSO_STUBIFY(parse_printf_format) */
/*  LDSO_STUBIFY(pathconf) */
/*  LDSO_STUBIFY(pause) */
/*  LDSO_STUBIFY(pclose) */
/*  LDSO_STUBIFY(perror) */
/*  LDSO_STUBIFY(pipe) */
/*  LDSO_STUBIFY(pmap_getmaps) */
/*  LDSO_STUBIFY(pmap_getport) */
/*  LDSO_STUBIFY(pmap_rmtcall) */
/*  LDSO_STUBIFY(pmap_set) */
/*  LDSO_STUBIFY(pmap_unset) */
/*  LDSO_STUBIFY(popen) */
/*  LDSO_STUBIFY(pow) */
/*  LDSO_STUBIFY(pow10) */
/*  LDSO_STUBIFY(pow10l) */
/*  LDSO_STUBIFY(pow2) */
/*  LDSO_STUBIFY(pow2l) */
/*  LDSO_STUBIFY(powl) */
/*  LDSO_STUBIFY(prev_fstat) */
/*  LDSO_STUBIFY(prev_lstat) */
/*  LDSO_STUBIFY(prev_mknod) */
/*  LDSO_STUBIFY(prev_stat) */
/*  LDSO_STUBIFY(prev_ustat) */
/*  LDSO_STUBIFY(printf) */
/*  LDSO_STUBIFY(psignal) */
/*  LDSO_STUBIFY(pthread_cond_signal) */
/*  LDSO_STUBIFY(pthread_cond_wait) */
/*  LDSO_STUBIFY(pthread_mutex_lock) */
/*  LDSO_STUBIFY(pthread_mutex_unlock) */
/*  LDSO_STUBIFY(pthread_once) */
/*  LDSO_STUBIFY(pthread_yield) */
/*  LDSO_STUBIFY(ptrace) */
/*  LDSO_STUBIFY(putc) */
/*  LDSO_STUBIFY(putchar) */
/*  LDSO_STUBIFY(putenv) */
/*  LDSO_STUBIFY(putlong) */
/*  LDSO_STUBIFY(putpwent) */
/*  LDSO_STUBIFY(puts) */
/*  LDSO_STUBIFY(putsgent) */
/*  LDSO_STUBIFY(putshort) */
/*  LDSO_STUBIFY(putspent) */
/*  LDSO_STUBIFY(pututline) */
/*  LDSO_STUBIFY(putw) */
/*  LDSO_STUBIFY(qsort) */
/*  LDSO_STUBIFY(raise) */
/*  LDSO_STUBIFY(rand) */
/*  LDSO_STUBIFY(random) */
/*  LDSO_STUBIFY(rcmd) */
/*  LDSO_STUBIFY(re_comp) */
/*  LDSO_STUBIFY(re_compile_fastmap) */
/*  LDSO_STUBIFY(re_compile_pattern) */
/*  LDSO_STUBIFY(re_error_msg) */
/*  LDSO_STUBIFY(re_exec) */
/*  LDSO_STUBIFY(re_match) */
/*  LDSO_STUBIFY(re_match_2) */
/*  LDSO_STUBIFY(re_search) */
/*  LDSO_STUBIFY(re_search_2) */
/*  LDSO_STUBIFY(re_set_registers) */
/*  LDSO_STUBIFY(re_set_syntax) */
/*  LDSO_STUBIFY(re_syntax_options) */
/*  LDSO_STUBIFY(read) */
/*  LDSO_STUBIFY(readdir) */
/*  LDSO_STUBIFY(readdir_r) */
/*  LDSO_STUBIFY(readlink) */
/*  LDSO_STUBIFY(readv) */
/*  LDSO_STUBIFY(realloc) */
/*  LDSO_STUBIFY(realpath) */
/*  LDSO_STUBIFY(reboot) */
/*  LDSO_STUBIFY(recv) */
/*  LDSO_STUBIFY(recvfrom) */
/*  LDSO_STUBIFY(recvmsg) */
/*  LDSO_STUBIFY(regcomp) */
/*  LDSO_STUBIFY(regerror) */
/*  LDSO_STUBIFY(regexec) */
/*  LDSO_STUBIFY(regfree) */
/*  LDSO_STUBIFY(register_printf_function) */
/*  LDSO_STUBIFY(remove) */
/*  LDSO_STUBIFY(remque) */
/*  LDSO_STUBIFY(rename) */
/*  LDSO_STUBIFY(res_init) */
/*  LDSO_STUBIFY(res_isourserver) */
/*  LDSO_STUBIFY(res_mkquery) */
/*  LDSO_STUBIFY(res_nameinquery) */
/*  LDSO_STUBIFY(res_queriesmatch) */
/*  LDSO_STUBIFY(res_query) */
/*  LDSO_STUBIFY(res_querydomain) */
/*  LDSO_STUBIFY(res_randomid) */
/*  LDSO_STUBIFY(res_search) */
/*  LDSO_STUBIFY(res_send) */
/*  LDSO_STUBIFY(rewind) */
/*  LDSO_STUBIFY(rewinddir) */
/*  LDSO_STUBIFY(rexec) */
/*  LDSO_STUBIFY(rindex) */
/*  LDSO_STUBIFY(rint) */
/*  LDSO_STUBIFY(rmdir) */
/*  LDSO_STUBIFY(rpc_createerr) */
/*  LDSO_STUBIFY(rresvport) */
/*  LDSO_STUBIFY(rtime) */
/*  LDSO_STUBIFY(ruserok) */
/*  LDSO_STUBIFY(rx_cache_bound) */
/*  LDSO_STUBIFY(rx_id_instruction_table) */
/*  LDSO_STUBIFY(rx_id_translation) */
/*  LDSO_STUBIFY(rx_slowmap) */
/*  LDSO_STUBIFY(rx_version_string) */
/*  LDSO_STUBIFY(sbrk) */
/*  LDSO_STUBIFY(scandir) */
/*  LDSO_STUBIFY(scanf) */
/*  LDSO_STUBIFY(seed48) */
/*  LDSO_STUBIFY(seekdir) */
/*  LDSO_STUBIFY(select) */
/*  LDSO_STUBIFY(semctl) */
/*  LDSO_STUBIFY(semget) */
/*  LDSO_STUBIFY(semop) */
/*  LDSO_STUBIFY(send) */
/*  LDSO_STUBIFY(sendmsg) */
/*  LDSO_STUBIFY(sendto) */
/*  LDSO_STUBIFY(set_new_handler) */
/*  LDSO_STUBIFY(setbuf) */
/*  LDSO_STUBIFY(setbuffer) */
/*  LDSO_STUBIFY(setdomainname) */
/*  LDSO_STUBIFY(setegid) */
/*  LDSO_STUBIFY(setenv) */
/*  LDSO_STUBIFY(seteuid) */
/*  LDSO_STUBIFY(setfsgid) */
/*  LDSO_STUBIFY(setfsuid) */
/*  LDSO_STUBIFY(setgid) */
/*  LDSO_STUBIFY(setgrent) */
/*  LDSO_STUBIFY(setgroups) */
/*  LDSO_STUBIFY(sethostent) */
/*  LDSO_STUBIFY(sethostid) */
/*  LDSO_STUBIFY(sethostname) */
/*  LDSO_STUBIFY(setitimer) */
/*  LDSO_STUBIFY(setkey) */
/*  LDSO_STUBIFY(setlinebuf) */
/*  LDSO_STUBIFY(setlocale) */
/*  LDSO_STUBIFY(setlogmask) */
/*  LDSO_STUBIFY(setmntent) */
/*  LDSO_STUBIFY(setnetent) */
/*  LDSO_STUBIFY(setpgid) */
/*  LDSO_STUBIFY(setpgrp) */
/*  LDSO_STUBIFY(setpriority) */
/*  LDSO_STUBIFY(setprotoent) */
/*  LDSO_STUBIFY(setpwent) */
/*  LDSO_STUBIFY(setregid) */
/*  LDSO_STUBIFY(setreuid) */
/*  LDSO_STUBIFY(setrlimit) */
/*  LDSO_STUBIFY(setrpcent) */
/*  LDSO_STUBIFY(setservent) */
/*  LDSO_STUBIFY(setsgent) */
/*  LDSO_STUBIFY(setsid) */
/*  LDSO_STUBIFY(setsockopt) */
/*  LDSO_STUBIFY(setspent) */
/*  LDSO_STUBIFY(setstate) */
/*  LDSO_STUBIFY(settimeofday) */
/*  LDSO_STUBIFY(setuid) */
/*  LDSO_STUBIFY(setusershell) */
/*  LDSO_STUBIFY(setutent) */
/*  LDSO_STUBIFY(setvbuf) */
/*  LDSO_STUBIFY(sgetsgent) */
/*  LDSO_STUBIFY(sgetspent) */
/*  LDSO_STUBIFY(shmat) */
/*  LDSO_STUBIFY(shmctl) */
/*  LDSO_STUBIFY(shmdt) */
/*  LDSO_STUBIFY(shmget) */
/*  LDSO_STUBIFY(shutdown) */
/*  LDSO_STUBIFY(sigaction) */
/*  LDSO_STUBIFY(sigaddset) */
/*  LDSO_STUBIFY(sigblock) */
/*  LDSO_STUBIFY(sigdelset) */
/*  LDSO_STUBIFY(sigemptyset) */
/*  LDSO_STUBIFY(sigfillset) */
/*  LDSO_STUBIFY(siggetmask) */
/*  LDSO_STUBIFY(siginterrupt) */
/*  LDSO_STUBIFY(sigismember) */
/*  LDSO_STUBIFY(siglongjmp) */
/*  LDSO_STUBIFY(signal) */
/*  LDSO_STUBIFY(signgam) */
/*  LDSO_STUBIFY(signgaml) */
/*  LDSO_STUBIFY(sigpause) */
/*  LDSO_STUBIFY(sigpending) */
/*  LDSO_STUBIFY(sigprocmask) */
/*  LDSO_STUBIFY(sigreturn) */
/*  LDSO_STUBIFY(sigsetmask) */
/*  LDSO_STUBIFY(sigsuspend) */
/*  LDSO_STUBIFY(sin) */
/*  LDSO_STUBIFY(sinh) */
/*  LDSO_STUBIFY(sinhl) */
/*  LDSO_STUBIFY(sinl) */
/*  LDSO_STUBIFY(sleep) */
/*  LDSO_STUBIFY(snprintf) */
/*  LDSO_STUBIFY(socket) */
/*  LDSO_STUBIFY(socketcall) */
/*  LDSO_STUBIFY(socketpair) */
/*  LDSO_STUBIFY(sprintf) */
/*  LDSO_STUBIFY(sqrt) */
/*  LDSO_STUBIFY(sqrtl) */
/*  LDSO_STUBIFY(srand) */
/*  LDSO_STUBIFY(srand48) */
/*  LDSO_STUBIFY(srandom) */
/*  LDSO_STUBIFY(sscanf) */
/*  LDSO_STUBIFY(ssignal) */
/*  LDSO_STUBIFY(stat) */
/*  LDSO_STUBIFY(statfs) */
/*  LDSO_STUBIFY(stderr) */
/*  LDSO_STUBIFY(stdin) */
/*  LDSO_STUBIFY(stdout) */
/*  LDSO_STUBIFY(stime) */
/*  LDSO_STUBIFY(stpcpy) */
/*  LDSO_STUBIFY(stpncpy) */
/*  LDSO_STUBIFY(strcasecmp) */
/*  LDSO_STUBIFY(strcat) */
/*  LDSO_STUBIFY(strchr) */
/*  LDSO_STUBIFY(strcmp) */
/*  LDSO_STUBIFY(strcoll) */
/*  LDSO_STUBIFY(strcpy) */
/*  LDSO_STUBIFY(strcspn) */
/*  LDSO_STUBIFY(strdup) */
/*  LDSO_STUBIFY(strerror) */
/*  LDSO_STUBIFY(strfry) */
/*  LDSO_STUBIFY(strftime) */
/*  LDSO_STUBIFY(strlen) */
/*  LDSO_STUBIFY(strncasecmp) */
/*  LDSO_STUBIFY(strncat) */
/*  LDSO_STUBIFY(strncmp) */
/*  LDSO_STUBIFY(strncpy) */
/*  LDSO_STUBIFY(strpbrk) */
/*  LDSO_STUBIFY(strptime) */
/*  LDSO_STUBIFY(strrchr) */
/*  LDSO_STUBIFY(strsep) */
/*  LDSO_STUBIFY(strsignal) */
/*  LDSO_STUBIFY(strspn) */
/*  LDSO_STUBIFY(strstr) */
/*  LDSO_STUBIFY(strtod) */
/*  LDSO_STUBIFY(strtof) */
/*  LDSO_STUBIFY(strtok) */
/*  LDSO_STUBIFY(strtol) */
/*  LDSO_STUBIFY(strtold) */
/*  LDSO_STUBIFY(strtoq) */
/*  LDSO_STUBIFY(strtoul) */
/*  LDSO_STUBIFY(strtouq) */
/*  LDSO_STUBIFY(strxfrm) */
/*  LDSO_STUBIFY(svc_exit) */
/*  LDSO_STUBIFY(svc_fdset) */
/*  LDSO_STUBIFY(svc_getreq) */
/*  LDSO_STUBIFY(svc_getreqset) */
/*  LDSO_STUBIFY(svc_register) */
/*  LDSO_STUBIFY(svc_run) */
/*  LDSO_STUBIFY(svc_sendreply) */
/*  LDSO_STUBIFY(svc_unregister) */
/*  LDSO_STUBIFY(svcerr_auth) */
/*  LDSO_STUBIFY(svcerr_decode) */
/*  LDSO_STUBIFY(svcerr_noproc) */
/*  LDSO_STUBIFY(svcerr_noprog) */
/*  LDSO_STUBIFY(svcerr_progvers) */
/*  LDSO_STUBIFY(svcerr_systemerr) */
/*  LDSO_STUBIFY(svcerr_weakauth) */
/*  LDSO_STUBIFY(svcraw_create) */
/*  LDSO_STUBIFY(svctcp_create) */
/*  LDSO_STUBIFY(svcudp_bufcreate) */
/*  LDSO_STUBIFY(svcudp_create) */
/*  LDSO_STUBIFY(swab) */
/*  LDSO_STUBIFY(swapoff) */
/*  LDSO_STUBIFY(swapon) */
/*  LDSO_STUBIFY(symlink) */
/*  LDSO_STUBIFY(sync) */
/*  LDSO_STUBIFY(sys_errlist) */
/*  LDSO_STUBIFY(sys_nerr) */
/*  LDSO_STUBIFY(sys_siglist) */
/*  LDSO_STUBIFY(syscall) */
/*  LDSO_STUBIFY(syscall_flock) */
/*  LDSO_STUBIFY(syscall_readv) */
/*  LDSO_STUBIFY(syscall_writev) */
/*  LDSO_STUBIFY(sysconf) */
/*  LDSO_STUBIFY(sysinfo) */
/*  LDSO_STUBIFY(syslog) */
/*  LDSO_STUBIFY(system) */
/*  LDSO_STUBIFY(tan) */
/*  LDSO_STUBIFY(tanh) */
/*  LDSO_STUBIFY(tanhl) */
/*  LDSO_STUBIFY(tanl) */
/*  LDSO_STUBIFY(tcdrain) */
/*  LDSO_STUBIFY(tcflow) */
/*  LDSO_STUBIFY(tcflush) */
/*  LDSO_STUBIFY(tcgetattr) */
/*  LDSO_STUBIFY(tcgetpgrp) */
/*  LDSO_STUBIFY(tcsendbreak) */
/*  LDSO_STUBIFY(tcsetattr) */
/*  LDSO_STUBIFY(tcsetpgrp) */
/*  LDSO_STUBIFY(tdelete) */
/*  LDSO_STUBIFY(tell) */
/*  LDSO_STUBIFY(telldir) */
/*  LDSO_STUBIFY(tempnam) */
/*  LDSO_STUBIFY(tfind) */
/*  LDSO_STUBIFY(time) */
/*  LDSO_STUBIFY(timegm) */
/*  LDSO_STUBIFY(times) */
/*  LDSO_STUBIFY(timezone) */
/*  LDSO_STUBIFY(tmpfile) */
/*  LDSO_STUBIFY(tmpnam) */
/*  LDSO_STUBIFY(toascii) */
/*  LDSO_STUBIFY(tolower) */
/*  LDSO_STUBIFY(toupper) */
/*  LDSO_STUBIFY(truncate) */
/*  LDSO_STUBIFY(tsearch) */
/*  LDSO_STUBIFY(ttyname) */
/*  LDSO_STUBIFY(ttyname_r) */
/*  LDSO_STUBIFY(twalk) */
/*  LDSO_STUBIFY(tzname) */
/*  LDSO_STUBIFY(tzset) */
/*  LDSO_STUBIFY(ulckpwdf) */
/*  LDSO_STUBIFY(ulimit) */
/*  LDSO_STUBIFY(umask) */
/*  LDSO_STUBIFY(umount) */
/*  LDSO_STUBIFY(uname) */
/*  LDSO_STUBIFY(ungetc) */
/*  LDSO_STUBIFY(unlink) */
/*  LDSO_STUBIFY(unsetenv) */
/*  LDSO_STUBIFY(uselib) */
/*  LDSO_STUBIFY(usleep) */
/*  LDSO_STUBIFY(ustat) */
/*  LDSO_STUBIFY(utime) */
/*  LDSO_STUBIFY(utimes) */
/*  LDSO_STUBIFY(utmpname) */
/*  LDSO_STUBIFY(valloc) */
/*  LDSO_STUBIFY(vasprintf) */
/*  LDSO_STUBIFY(vfork) */
/*  LDSO_STUBIFY(vfprintf) */
/*  LDSO_STUBIFY(vfscanf) */
/*  LDSO_STUBIFY(vhangup) */
/*  LDSO_STUBIFY(vm86) */
/*  LDSO_STUBIFY(vprintf) */
/*  LDSO_STUBIFY(vscanf) */
/*  LDSO_STUBIFY(vsnprintf) */
/*  LDSO_STUBIFY(vsprintf) */
/*  LDSO_STUBIFY(vsscanf) */
/*  LDSO_STUBIFY(vsyslog) */
/*  LDSO_STUBIFY(wait) */
/*  LDSO_STUBIFY(wait3) */
/*  LDSO_STUBIFY(wait4) */
/*  LDSO_STUBIFY(waitpid) */
/*  LDSO_STUBIFY(wctomb) */
/*  LDSO_STUBIFY(write) */
/*  LDSO_STUBIFY(writev) */
/*  LDSO_STUBIFY(y0) */
/*  LDSO_STUBIFY(y0l) */
/*  LDSO_STUBIFY(y1) */
/*  LDSO_STUBIFY(y1l) */
/*  LDSO_STUBIFY(yn) */
/*  LDSO_STUBIFY(ynl) */
/*  LDSO_STUBIFY(yp_all) */
/*  LDSO_STUBIFY(yp_bind) */
/*  LDSO_STUBIFY(yp_first) */
/*  LDSO_STUBIFY(yp_get_default_domain) */
/*  LDSO_STUBIFY(yp_maplist) */
/*  LDSO_STUBIFY(yp_master) */
/*  LDSO_STUBIFY(yp_match) */
/*  LDSO_STUBIFY(yp_next) */
/*  LDSO_STUBIFY(yp_order) */
/*  LDSO_STUBIFY(yp_unbind) */
/*  LDSO_STUBIFY(yperr_string) */
/*  LDSO_STUBIFY(ypprot_err) */
