/*
 * OS-dependent routines for BSD-ish systems
 *
 * This file (along with os.h) exports an OS-independent interface to
 * the operating system VM facilities. This interface looks a lot like
 * the Mach interface (but simpler in some places). For some operating
 * systems, a subset of these functions will have to be emulated.
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

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "lispregs.h"
#include "sbcl.h"

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/proc.h>
#include "validate.h"
vm_size_t os_vm_page_size;

#if defined GENCGC
#include "gencgc.h"
#endif

/* The different BSD variants have diverged in exactly where they
 * store signal context information, but at least they tend to use the
 * same stems to name the structure fields, so by using this macro we
 * can share a fair amount of code between different variants. */
#if defined __FreeBSD__
#define CONTEXT_ADDR_FROM_STEM(stem) &context->uc_mcontext.mc_ ## stem
#elif defined __OpenBSD__
#define CONTEXT_ADDR_FROM_STEM(stem) &context->sc_ ## stem
#else
#error unsupported BSD variant
#endif

void
os_init(void)
{
    os_vm_page_size = getpagesize();
}

/* KLUDGE: There is strong family resemblance in the signal context
 * stuff in FreeBSD and OpenBSD, but in detail they're different in
 * almost every line of code. It would be nice to find some way to
 * factor out the commonality better; failing that, it might be best
 * just to split this generic-BSD code into one variant for each BSD. */
   
int *
os_context_register_addr(os_context_t *context, int offset)
{
    switch(offset) {
    case  0:
	return CONTEXT_ADDR_FROM_STEM(eax);
    case  2:
	return CONTEXT_ADDR_FROM_STEM(ecx);
    case  4:
	return CONTEXT_ADDR_FROM_STEM(edx);
    case  6:
	return CONTEXT_ADDR_FROM_STEM(ebx);
    case  8:
	return CONTEXT_ADDR_FROM_STEM(esp);
    case 10:
	return CONTEXT_ADDR_FROM_STEM(ebp);
    case 12:
	return CONTEXT_ADDR_FROM_STEM(esi);
    case 14:
	return CONTEXT_ADDR_FROM_STEM(edi);
    default:
	return 0;
    }
}

int *
os_context_pc_addr(os_context_t *context)
{
#if defined __FreeBSD__
    return CONTEXT_ADDR_FROM_STEM(eip);
#elif defined __OpenBSD__
    return CONTEXT_ADDR_FROM_STEM(pc);
#else
#error unsupported BSD variant
#endif
}

int *
os_context_sp_addr(os_context_t *context)
{
    return CONTEXT_ADDR_FROM_STEM(esp);
}

sigset_t *
os_context_sigmask_addr(os_context_t *context)
{
    /* (Unlike most of the other context fields that we access, the
     * signal mask field is a field of the basic, outermost context
     * struct itself both in FreeBSD 4.0 and in OpenBSD 2.6.) */
#if defined __FreeBSD__
    return &context->uc_sigmask;
#elif defined __OpenBSD__
    return &context->sc_mask;
#else
#error unsupported BSD variant
#endif
}

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    int flags = MAP_PRIVATE | MAP_ANON;

    if (addr)
	flags |= MAP_FIXED;

    addr = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (addr == MAP_FAILED) {
	perror("mmap");
	return NULL;
    }

    return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap(addr, len) == -1)
	perror("munmap");
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    addr = mmap(addr, len,
		OS_VM_PROT_ALL,
		MAP_PRIVATE | MAP_FILE | MAP_FIXED,
		fd, (off_t) offset);

    if (addr == MAP_FAILED) {
	perror("mmap");
	lose("unexpected mmap(..) failure");
    }

    return addr;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect(address, length, prot) == -1) {
	perror("mprotect");
    }
}

static boolean
in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
    char* beg = (char*) sbeg;
    char* end = (char*) sbeg + slen;
    char* adr = (char*) a;
    return (adr >= beg && adr < end);
}

boolean
is_valid_lisp_addr(os_vm_address_t addr)
{
    return in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE)
	|| in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE   )
	|| in_range_p(addr, DYNAMIC_SPACE_START  , DYNAMIC_SPACE_SIZE  )
	|| in_range_p(addr, CONTROL_STACK_START  , CONTROL_STACK_SIZE  )
	|| in_range_p(addr, BINDING_STACK_START  , BINDING_STACK_SIZE  );
}

/*
 * any OS-dependent special low-level handling for signals
 */

#if !defined GENCGC

void
os_install_interrupt_handlers(void)
{}

#else

/*
 * The GENCGC needs to be hooked into whatever signal is raised for
 * page fault on this OS.
 */
static void
memory_fault_handler(int signal, siginfo_t *siginfo, void *void_context)
{
    /* The way that we extract low level information like the fault
     * address is not specified by POSIX. */
#if defined __FreeBSD__
    void *fault_addr = siginfo->si_addr;
#elif defined __OpenBSD__
    void *fault_addr = siginfo->si_addr;
#else
#error unsupported BSD variant
#endif
    if (!gencgc_handle_wp_violation(fault_addr)) {
	interrupt_handle_now(signal, siginfo, void_context);
    }
}
void
os_install_interrupt_handlers(void)
{
#if defined __FreeBSD__
    interrupt_install_low_level_handler(SIGBUS, memory_fault_handler);
#elif defined __OpenBSD__
    interrupt_install_low_level_handler(SIGSEGV, memory_fault_handler);
#else
#error unsupported BSD variant
#endif
}

#endif /* !defined GENCGC */

/*
 * stuff to help work with dynamically linked libraries
 */

/* feh!
 *
 * DL_WORKAROUND enables "stubbing" of various functions from libc et
 * al. This is necessary when using dynamic linking in FreeBSD, as the
 * symbols in the dynamic libraries will not have known addresses (in
 * sbcl.nm).
 *
 * FIXME: This flag should be set in Config.bsd */
#if defined __FreeBSD__
#define DL_WORKAROUND 1
#elif defined __OpenBSD__
/* SBCL doesn't (yet?) work at all with dynamic libs on OpenBSD, so we
 * wouldn't get any use out of these stubs. -- WHN 20001001 */
#define DL_WORKAROUND 0
#else
#error unsupported BSD variant
#endif

#if DL_WORKAROUND
#include <unistd.h>
#include <dlfcn.h>
#include <math.h>
#include <sys/types.h>
#include <dirent.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <time.h>
#include <sys/resource.h>
#include <signal.h>
#include <fcntl.h>

void *ldso_stub__dlopen(const char *path, int mode)
{
  return dlopen(path, mode);
}

void *ldso_stub__dlsym(void *handle, const char *symbol)
{
  return dlsym(handle, symbol);
}

const char *ldso_stub__dlerror(void)
{
  return dlerror();
}
int ldso_stub__access(const char *path, int mode)
{
  return access(path, mode);
}

double ldso_stub__acos(double x)
{
  return acos(x);
}

double ldso_stub__acosh(double x)
{
  return acosh(x);
}

double ldso_stub__asin(double x)
{
  return asin(x);
}

double ldso_stub__asinh(double x)
{
  return asin(x);
}

double ldso_stub__atanh(double x)
{
  return atanh(x);
}


int ldso_stub__chdir(const char *path)
{
  return chdir(path);
}

int ldso_stub__close(int d)
{
  return close(d);
}

int ldso_stub__closedir(DIR *dirp)
{
  return closedir(dirp);
}

double ldso_stub__cosh(double x)
{
  return cosh(x);
}

void ldso_stub__exit(int status)
{
  exit(status);
}

void ldso_stub__free(void *ptr)
{
  free(ptr);
}

int ldso_stub__fstat(int fd, struct stat *sb)
{
  return fstat(fd, sb);
}

int ldso_stub__fsync(int fd)
{
  return fsync(fd);
}

char *ldso_stub__getenv(const char *name)
{
  return getenv(name);
}

int ldso_stub__gethostname(char *name, int namelen)
{
  return gethostname(name, namelen);
}

pid_t ldso_stub__getpid(void)
{
  return getpid();
}

int ldso_stub__getrusage(int who, struct rusage *rusage)
{
  return getrusage(who, rusage);
}

int ldso_stub__gettimeofday(struct timeval *tp, struct timezone *tzp)
{
  return gettimeofday(tp, tzp);
}

uid_t ldso_stub__getuid(void)
{
  return getuid();
}

char *ldso_stub__getwd(char *buf)
{
  return getwd(buf);
}

double ldso_stub__hypot(double x, double y)
{
  return hypot(x, y);
}

int ldso_stub__kill(pid_t pid, int sig)
{
  return kill(pid, sig);
}

int ldso_stub__killpg(pid_t pgrp, int sig)
{
  return killpg(pgrp, sig);
}

off_t ldso_stub__lseek(int fildes, off_t offset, int whence)
{
  return lseek(fildes, offset, whence);
}

int ldso_stub__lstat(const char *path, struct stat *sb)
{
  return lstat(path, sb);
}

void *ldso_stub__malloc(size_t size)
{
  return malloc(size);
}

int ldso_stub__mkdir(const char *path, mode_t mode)
{
  return mkdir(path, mode);
}

int ldso_stub__open(const char *path, int flags, mode_t mode)
{
  return open(path, flags, mode);
}

DIR *ldso_stub__opendir(const char *filename)
{
  return opendir(filename);
}

double ldso_stub__pow(double x, double y)
{
  return pow(x, y);
}

ssize_t ldso_stub__read(int d, void *buf, size_t nbytes)
{
  return read(d, buf, nbytes);
}

struct dirent *ldso_stub__readdir(DIR *dirp)
{
  return readdir(dirp);
}

int ldso_stub__readlink(const char *path, char *buf, int bufsiz)
{
  return readlink(path, buf, bufsiz);
}

int ldso_stub__rename(const char *from, const char *to)
{
  return rename(from, to);
}

int ldso_stub__select(int nfds, fd_set *readfs, fd_set *writefds, 
		      fd_set *exceptfds, struct timeval *timeout)
{
  return select(nfds, readfs, writefds, exceptfds, timeout);
}

int ldso_stub__sigblock(int mask)
{
  return sigblock(mask);
}

int ldso_stub__sigpause(int sigmask)
{
  return sigpause(sigmask);
}

int ldso_stub__sigsetmask(int mask)
{
  return sigsetmask(mask);
}

double ldso_stub__sinh(double x)
{
  return sin(x);
}

int ldso_stub__stat(const char *path, struct stat *sb)
{
  return stat(path, sb);
}

double ldso_stub__tanh(double x)
{
  return tanh(x);
}

/* tzname */

int ldso_stub__unlink(const char *path)
{
  return unlink(path);
}

ssize_t ldso_stub__write(int d, const void *buf, size_t nbytes)
{
  return write(d, buf, nbytes);
}

pid_t ldso_stub__wait3(int *status, int options, struct rusage *rusage)
{
  return wait3(status, options, rusage);
}

#endif /* DL_WORKAROUND */
