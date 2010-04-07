#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/file.h>

#include <unistd.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/utsname.h>

#include "sbcl.h"
#include "os.h"
#include "arch.h"
#include "interr.h"
#include "interrupt.h"
#include "globals.h"
#include "validate.h"
#include "target-arch-os.h"

#ifdef LISP_FEATURE_GENCGC
#error gencgc not ported to hpux
#endif

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
#error C_STACK_IS_CONTROL_STACK isnt supported
#endif

size_t os_vm_page_size;

void
os_init(char *argv[], char *envp[])
{
    os_vm_page_size = getpagesize();
}

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
    os_vm_address_t actual;
    int flags = MAP_PRIVATE | MAP_ANONYMOUS;
    if (addr) flags |= MAP_FIXED;

    actual = mmap(addr, len, OS_VM_PROT_ALL, flags, -1, 0);

    if (actual == MAP_FAILED) {
        perror("mmap");
        lose("os_validate(): mmap() failure\n");
    }

    if (addr && (addr!=actual)) {
        fprintf(stderr, "mmap: wanted %lu bytes at %p, actually mapped at %p\n",
                (unsigned long) len, addr, actual);
        return 0;
    }

    return actual;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap(addr,len) == -1) {
        perror("munmap");
        lose("os_invalidate(): mmap() failure\n");
    }
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
    os_vm_address_t actual;
    actual = mmap(addr, len,
                OS_VM_PROT_ALL,
                MAP_PRIVATE | MAP_FILE | MAP_FIXED,
                fd, (off_t) offset);
    if (actual == MAP_FAILED || (addr && (addr != actual))) {
        perror("mmap");
        lose("os_map(): mmap() failure\n");
    }
    return actual;
}

void
os_protect(os_vm_address_t addr, os_vm_size_t len, os_vm_prot_t prot)
{
    if (mprotect(addr, len, prot) == -1) {
        perror("mprotect");
    }
}

boolean
is_valid_lisp_addr(os_vm_address_t addr)
{
    struct thread *th;
    size_t ad = (size_t) addr;

    if ((READ_ONLY_SPACE_START <= ad && ad < READ_ONLY_SPACE_END)
        || (STATIC_SPACE_START <= ad && ad < STATIC_SPACE_END)
        || (DYNAMIC_0_SPACE_START <= ad && ad < DYNAMIC_0_SPACE_END)
        || (DYNAMIC_1_SPACE_START <= ad && ad < DYNAMIC_1_SPACE_END)
        )
        return 1;
    for_each_thread(th) {
        if((size_t)(th->control_stack_start) <= ad
           && ad < (size_t)(th->control_stack_end))
            return 1;
        if((size_t)(th->binding_stack_start) <= ad
           && ad < (size_t)(th->binding_stack_start + BINDING_STACK_SIZE))
            return 1;
    }
    return 0;
}

/*
 * any OS-dependent special low-level handling for signals
 */

static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    os_vm_address_t addr = arch_get_bad_addr(signal, info, context);

    if (!cheneygc_handle_wp_violation(context, addr))
        if (!handle_guard_page_triggered(context, addr))
            lisp_memory_fault_error(context, addr);
    *((os_context_register_t *) &((ucontext_t *) context)->uc_mcontext.ss_flags)
     |= SS_MODIFIEDWIDE;
}

void
os_install_interrupt_handlers(void)
{
    undoably_install_low_level_interrupt_handler(SIG_MEMORY_FAULT,
                                                 sigsegv_handler);
}

char *
os_get_runtime_executable_path(int external)
{
    return NULL;
}

/* when inside call_into_lisp, we will first jump to the stub
 * and then the stub will jump into the lisp function. Then
 * the lisp function will return to the stub function and
 * the stub will return to the call_into_lisp function.
 */
void *return_from_lisp_stub;
void
setup_return_from_lisp_stub (void *addr)
{
  return_from_lisp_stub = addr;
}
