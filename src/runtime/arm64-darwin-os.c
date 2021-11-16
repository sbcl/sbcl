#include "thread.h"
#include "gc-internal.h"
#include "gc-private.h"
void set_thread_stack(void *address) {
    /* KLUDGE: There is no interface to change the stack location of
       the initial thread, and without that backtrace(3) returns zero
       frames, which breaks some graphical applications on High Sierra
    */
    pthread_t thread = pthread_self();
    void *stackaddr = pthread_get_stackaddr_np(thread);
    size_t stacksize = pthread_get_stacksize_np(thread);

    if (__PTHREAD_SIZE__ >= 22*8 &&
        ((void **)thread->__opaque)[20] == stackaddr &&
        ((size_t *)thread->__opaque)[21] == stacksize) {
        ((void **)thread->__opaque)[20] = address;
        ((size_t *)thread->__opaque)[21] = thread_control_stack_size;
        ((size_t *)thread->__opaque)[23] = (thread_control_stack_size + vm_page_size);
    }
}

void jit_patch(lispobj* address, lispobj value) {
    THREAD_JIT(0);
    *address = value;
    THREAD_JIT(1);
}

void jit_patch_int(int* address, int value) {
    THREAD_JIT(0);
    *address = value;
    THREAD_JIT(1);
}

void jit_patch_uint(unsigned* address, unsigned value) {
    THREAD_JIT(0);
    *address = value;
    THREAD_JIT(1);
}

void jit_patch_uchar(unsigned char* address, unsigned char value) {
    THREAD_JIT(0);
    *address = value;
    THREAD_JIT(1);
}

void jit_memcpy(void* dst, void* src, size_t n) {
    THREAD_JIT(0);
    memcpy(dst, src, n);
    THREAD_JIT(1);
}
void jit_patch_code(lispobj code, lispobj value, unsigned long index) {
    THREAD_JIT(0);
    gc_card_mark[addr_to_card_index(code)] = 0;
    SET_WRITTEN_FLAG(native_pointer(code));
    native_pointer(code)[index] = value;
    THREAD_JIT(1);
}


void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    os_vm_address_t end_address
        = (os_vm_address_t)(((uintptr_t) address) + length);
    __clear_cache(address, end_address);
}
