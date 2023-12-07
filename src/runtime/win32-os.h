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

#include "pthreads_win32.h"

#ifdef LISP_FEATURE_64_BIT
/* LPVOID can't be legally used in pointer arithmetic.
 * (so all the arithmetic in 'validate.h' is technically illegal).
 * But I can't test 32-bit so let it stay as was */
typedef char* os_vm_address_t;
#else
typedef LPVOID os_vm_address_t;
#endif
typedef uword_t os_vm_size_t;
typedef intptr_t os_vm_offset_t;
typedef int os_vm_prot_t;

/* These are used as bitfields, but Win32 doesn't work that way, so we do a translation. */
#define OS_VM_PROT_READ    1
#define OS_VM_PROT_WRITE   2
#define OS_VM_PROT_EXECUTE 4

extern int os_number_of_processors;
#define HAVE_os_number_of_processors

extern ULONG win32_stack_guarantee;
extern DWORD win32_page_size;

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

struct lisp_exception_frame {
    struct lisp_exception_frame *next_frame;
    void *handler;
    lispobj *bindstack_pointer;
};

void wos_install_interrupt_handlers(struct lisp_exception_frame *handler);
char *dirname(char *path);

bool win32_maybe_interrupt_io(void* thread);

int sb_pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset);

#endif  /* SBCL_INCLUDED_WIN32_OS_H */
