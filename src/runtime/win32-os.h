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
#include <ntstatus.h>

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

// 64-bit uses whatever TLS index the kernels gives us which we store in
// 'sbcl_thread_tls_index' to hold our thread-local value of the pointer
// to struct thread.
// 32-bit uses a quasi-arbitrary fixed TLS index that we try to claim on startup.
// of the process.
#ifdef LISP_FEATURE_64_BIT
extern DWORD sbcl_thread_tls_index;
#define OUR_TLS_INDEX sbcl_thread_tls_index
#else
#define OUR_TLS_INDEX 63
#endif
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

boolean win32_maybe_interrupt_io(void* thread);
void os_revalidate_bzero(os_vm_address_t addr,  os_vm_size_t len);

int sb_pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset);

#endif  /* SBCL_INCLUDED_WIN32_OS_H */
