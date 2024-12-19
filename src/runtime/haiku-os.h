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
#include <sys/mman.h>
#include <signal.h>
#include <string.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef LISP_FEATURE_SB_THREAD
#include <OS.h>
#endif

// Needs to be defined before including target-arch.h
typedef caddr_t os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#include "target-arch-os.h"
#include "target-arch.h"
#define linuxversion(a, b, c) (((a)<<16)+((b)<<8)+(c))

#define SIG_MEMORY_FAULT SIGSEGV

/* Note that this must be higher than the highest numbered
 * synchronously generated signal that we handle (that is SIGSEGV),
 * due to Linux signal handling pecularities. See thread "Signal
 * delivery order" from 2009-03-14 on kernel-devel@vger.kernel.org. */
#define SIG_STOP_FOR_GC (SIGUSR2)
