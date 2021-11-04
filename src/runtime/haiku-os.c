#include "os.h"
#include "interr.h"
#include "interrupt.h"
#include "arch.h" // for arch_get_bad_addr
#include "interrupt.h" // for sig_stop_for_gc_handler
#include <image.h>
#include <stdio.h>

os_vm_address_t
os_validate(int attributes, os_vm_address_t addr, os_vm_size_t len,
            int __attribute__((unused)) execute, int __attribute__((unused)) jit)
{
    int protection = attributes & IS_GUARD_PAGE ? OS_VM_PROT_NONE : OS_VM_PROT_ALL;
    attributes &= ~IS_GUARD_PAGE;
    // There's no MAP_NORESERVE flag? How do we inform the OS not to commit
    // the whole range to swap?
    int flags =  MAP_PRIVATE | MAP_ANONYMOUS;
    os_vm_address_t actual;

#ifdef MAP_32BIT
    if (attributes & ALLOCATE_LOW)
        flags |= MAP_32BIT;
#endif
    actual = mmap(addr, len, protection, flags, -1, 0);
    if (actual == MAP_FAILED) {
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

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
    if (munmap(addr,len) == -1) {
        perror("munmap");
    }
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
    if (mprotect(address, length, prot)) {
        perror("mprotect");
    }
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
    if (gencgc_handle_wp_violation(addr)) return;

    if (!handle_guard_page_triggered(context, addr))
            interrupt_handle_now(signal, info, context);
}

void
os_install_interrupt_handlers(void)
{
    ll_install_handler(SIGSEGV, sigsegv_handler);
}

int pthread_getattr_np(pthread_t thread, pthread_attr_t *attr)
{
    lose("pthread_getattr_np unimplemented");
}

int pthread_attr_setstack(pthread_attr_t *attr,
                          void *stackaddr, size_t stacksize)
{
  lose("pthread_attr_setstack unimplemented");
}

int pthread_attr_getstack(const pthread_attr_t *attr,
                          void **stackaddr, size_t *stacksize)
{
  lose("pthread_attr_getstack unimplemented");
}
