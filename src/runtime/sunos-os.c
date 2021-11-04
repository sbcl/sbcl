#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/file.h>
#include <limits.h>

#include <unistd.h>
#include <errno.h>
#include <sys/param.h>

#include "sbcl.h"
#include "os.h"
#include "arch.h"
#include "interr.h"
#include "interrupt.h"
#include "globals.h"
#include "validate.h"
#include "target-arch-os.h"

#ifdef LISP_FEATURE_X86
#include "genesis/static-symbols.h"
#include "genesis/fdefn.h"
#endif

#ifdef LISP_FEATURE_GENCGC
#include "gc-internal.h"
#endif

void os_init() {}

os_vm_address_t os_validate(int attributes, os_vm_address_t addr, os_vm_size_t len,
            int __attribute__((unused)) execute, int __attribute__((unused)) jit)
{
    int protection = attributes & IS_GUARD_PAGE ? OS_VM_PROT_NONE : OS_VM_PROT_ALL;
    attributes &= ~IS_GUARD_PAGE;
    int flags = MAP_PRIVATE | MAP_NORESERVE | MAP_ANON;
    if (addr)
        flags |= MAP_FIXED;

    addr = mmap(addr, len, protection, flags, -1, 0);

    if (addr == MAP_FAILED) {
        perror("mmap");
        /* While it is generally hard to recover from out-of-memory
         * situations, we require callers to decide on the right course
         * of action here.  Consider thread creation: Failure to mmap
         * here is common if users have started too many threads, and
         * often we can recover from that and treat it as an ordinary
         * error. */
        return 0;
    }

    return addr;
}

void os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if(munmap((void*) addr, len) == -1)
        perror("munmap");
}


void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if(mprotect((void*)address, length, prot) == -1) {
        perror("mprotect");
    }
}

#if defined LISP_FEATURE_GENCGC

void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    void* fault_addr = (void*)info->si_addr;

    if (gencgc_handle_wp_violation(fault_addr)) return;

    if (!handle_guard_page_triggered(context, fault_addr))
            lisp_memory_fault_error(context, fault_addr);
}

#else

static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    os_vm_address_t addr = arch_get_bad_addr(signal, info, context);

    if (cheneygc_handle_wp_violation(context, addr)) return;

    if (!handle_guard_page_triggered(context,addr))
            lisp_memory_fault_error(context, addr);
}

#endif

void
os_install_interrupt_handlers()
{
    ll_install_handler(SIG_MEMORY_FAULT, sigsegv_handler);
}

char *os_get_runtime_executable_path()
{
    char path[PATH_MAX + 1];
    int size = readlink("/proc/self/path/a.out", path, sizeof(path) - 1);
    if (size < 0)
        return NULL;
    path[size] = '\0';

    if (strcmp(path, "unknown") == 0)
        return NULL;
    return copied_string(path);
}
