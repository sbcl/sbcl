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

#ifdef __FreeBSD__
#include <osreldate.h>
#endif

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/signal.h>

typedef caddr_t os_vm_address_t;
typedef vm_size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;
typedef int os_context_register_t;

#if defined __OpenBSD__
/* name defined for compatibility between OpenBSD 3.1 sigaltstack(2) and
 * Linux sigaltstack(2) */
typedef struct sigaltstack stack_t;
#elif defined __FreeBSD__
/* FreeBSD 4.6 already has stack_t defined. */
#endif

#if defined __FreeBSD__
/* Note: The man page for sigaction(2) in FreeBSD 4.0 says that this
 * is an mcontext_t, but according to comments by Raymond Wiker in the
 * original FreeBSD port of SBCL, that's wrong, it's actually a
 * ucontext_t. */

#include <sys/ucontext.h>
typedef ucontext_t os_context_t;
/* As the sbcl-devel message from Raymond Wiker 2000-12-01, FreeBSD
 * (unlike Linux and OpenBSD) doesn't let us tweak the CPU's single
 * step flag bit by messing with the flags stored in a signal context,
 * so we need to implement single stepping in a more roundabout way. */
#define CANNOT_GET_TO_SINGLE_STEP_FLAG
#define SIG_MEMORY_FAULT SIGBUS
#elif defined __OpenBSD__
typedef struct sigcontext os_context_t;
#define SIG_MEMORY_FAULT SIGSEGV
#elif defined DARWIN
  /* man pages claim that the third argument is a sigcontext struct,
     but ucontext_t is defined, matches sigcontext where sensible,
     offers better access to mcontext, and is of course the SUSv2-
     mandated type of the third argument, so we use that instead.
     If Apple is going to break ucontext_t out of spite, I'm going
     to be cross with them ;) -- PRM */

#include <ucontext.h>
typedef ucontext_t os_context_t;
#define SIG_MEMORY_FAULT SIGBUS
#else
#error unsupported BSD variant
#endif

#include "target-arch-os.h"
#include "target-arch.h"

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC
