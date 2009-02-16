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
#include <sys/types.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "target-arch-os.h"
#include "target-arch.h"

typedef LPVOID os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

typedef void *siginfo_t;

/* These are used as bitfields, but Win32 doesn't work that way, so we do a translation. */
#define OS_VM_PROT_READ    1
#define OS_VM_PROT_WRITE   2
#define OS_VM_PROT_EXECUTE 4

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

