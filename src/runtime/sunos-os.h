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

#include <signal.h>
#include <unistd.h>
#include <sys/fcntl.h>
#include <sys/mman.h>
#include <ucontext.h>

#include "target-arch-os.h"
#include "target-arch.h"

/* FIXME: Stolen from CMUCL. Investigate. */
typedef unsigned long os_vm_address_t;
typedef long os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

/* typedef struct ucontext os_context_t;*/

#define OS_VM_PROT_READ    PROT_READ
#define OS_VM_PROT_WRITE   PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define SIG_MEMORY_FAULT SIGSEGV

#define SIG_STOP_FOR_GC (SIGUSR2)
