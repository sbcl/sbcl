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

#ifndef SBCL_INCLUDED_WIN32_OS_H
#define SBCL_INCLUDED_WIN32_OS_H

#define WIN32_LEAN_AND_MEAN
#include <winsock2.h>
#include <windows.h>

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include "target-arch-os.h"
#include "target-arch.h"

#ifdef LISP_FEATURE_SB_THREAD
#include "pthreads_win32.h"
/* prevent inclusion of a mingw semaphore.h */
#define CANNOT_USE_POSIX_SEM_T
typedef sem_t os_sem_t;
#else
typedef void *siginfo_t;
typedef int sigset_t;
#endif

typedef LPVOID os_vm_address_t;
typedef uword_t os_vm_size_t;
typedef intptr_t os_vm_offset_t;
typedef int os_vm_prot_t;

/* These are used as bitfields, but Win32 doesn't work that way, so we do a translation. */
#define OS_VM_PROT_READ    1
#define OS_VM_PROT_WRITE   2
#define OS_VM_PROT_EXECUTE 4

#define os_open_core(file,mode) win32_open_for_mmap(file)
#define HAVE_os_open_core

#define os_fopen_runtime(file,mode) win32_fopen_runtime()
#define HAVE_os_fopen_runtime

extern int os_number_of_processors;
#define HAVE_os_number_of_processors

extern int win32_open_for_mmap(const char* file);
extern FILE* win32_fopen_runtime();

#define OUR_TLS_INDEX 63
#define SIG_MEMORY_FAULT SIGSEGV

#define SIG_STOP_FOR_GC (SIGRTMIN+1)
#define SIG_DEQUEUE (SIGRTMIN+2)
#define SIG_THREAD_EXIT (SIGRTMIN+3)

struct lisp_exception_frame {
    struct lisp_exception_frame *next_frame;
    void *handler;
    lispobj *bindstack_pointer;
};

void wos_install_interrupt_handlers(struct lisp_exception_frame *handler);
char *dirname(char *path);

void os_invalidate_free(os_vm_address_t addr, os_vm_size_t len);

boolean win32_maybe_interrupt_io(void* thread);

struct thread;
void** os_get_csp(struct thread* th);


#endif  /* SBCL_INCLUDED_WIN32_OS_H */
