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

#if defined __FreeBSD__
/* Note: The man page for sigaction(2) in FreeBSD 4.0 says that this
 * is an mcontext_t, but according to comments by Raymond Wiker in the
 * original FreeBSD port of SBCL, that's wrong, it's actually a
 * ucontext_t. */
typedef ucontext_t os_context_t;
#elif defined __OpenBSD__
typedef struct sigcontext os_context_t;
#else
#error unsupported BSD variant
#endif

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	4096
