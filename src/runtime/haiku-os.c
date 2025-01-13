#include "os.h"
#include "interr.h"
#include "interrupt.h"
#include "arch.h" // for arch_get_bad_addr
#include "interrupt.h" // for sig_stop_for_gc_handler
#include "gc.h" // for gencgc_handle_wp_violation
#include <image.h>
#include <stdio.h>

os_vm_address_t
os_alloc_gc_space(int __attribute__((unused)) space_id,
                  int attributes, os_vm_address_t addr, os_vm_size_t len)
{
    int protection = attributes & IS_GUARD_PAGE ? OS_VM_PROT_NONE : OS_VM_PROT_ALL;
    attributes &= ~IS_GUARD_PAGE;
    int flags =  MAP_PRIVATE | MAP_NORESERVE | MAP_ANONYMOUS;
    if (addr) {
        flags |= MAP_FIXED;
    }
    os_vm_address_t actual;

#ifdef MAP_32BIT
    if (attributes & ALLOCATE_LOW)
        flags |= MAP_32BIT;
#endif
    actual = mmap(addr, len, protection, flags, -1, 0);
    if (actual == MAP_FAILED) {
        fprintf(stderr, "mmap: FAILED__ wanted %lu bytes at %p, actually mapped at %p\n",
                (unsigned long) len, addr, actual);
        perror("mmap");
        return 0;               /* caller should check this */
    }

    // If requested addr was 0, the MOVABLE attribute means nothing.
    if (addr && !(attributes & MOVABLE) && (addr != actual)) {
        fprintf(stderr, "mmap: wanted %lu bytes at %p, actually mapped at %p\n",
                (unsigned long) len, addr, actual);
        return 0;
    }

    return actual;
}

char *os_get_runtime_executable_path()
{
    int cookie = 0;
    image_info info;
    int status = _get_next_image_info(0, &cookie, &info, sizeof(info));
    return (status == 0) ? copied_string(info.name) : 0;
}

void os_init() {}

static void
sigsegv_handler(int signal, siginfo_t *info, os_context_t *context)
{
    /*fprintf(stderr, "SIGSEGV: pc=%p addr=%p\n",
           context->uc_mcontext.rip, info->si_addr);*/
    os_vm_address_t addr = arch_get_bad_addr(signal, info, context);
    if (gencgc_handle_wp_violation(context, addr)) return;

    if (!handle_guard_page_triggered(context, addr)) {
        //interrupt_handle_now(signal, info, context);
        lisp_memory_fault_error(context, addr);
    }
}

void
os_install_interrupt_handlers(void)
{
    if (INSTALL_SIG_MEMORY_FAULT_HANDLER) {
        ll_install_handler(SIGSEGV, sigsegv_handler);
    }
}
