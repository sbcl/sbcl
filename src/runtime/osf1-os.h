#include <sys/types.h>
#include <sys/mman.h>
#include "target-arch-os.h"
#include "target-arch.h"

typedef caddr_t os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define SIG_MEMORY_FAULT SIGSEGV

typedef long os_context_register_t ;

#ifndef NSIG                    /* osf1 -D_XOPEN_SOURCE_EXTENDED omits this */
#define NSIG (SIGMAX+1)
#endif
