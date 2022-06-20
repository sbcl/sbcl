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

void jit_copy_code_insts(lispobj dst, lispobj* src)
{
    lispobj* aligned_src = (lispobj*)(src + ((uword_t)src & N_WORD_BYTES)); // align up
    struct code* code = (struct code*)(dst-OTHER_POINTER_LOWTAG);
    int nwords = code_total_nwords(code);
    gc_assert(code_total_nwords((struct code*)aligned_src));
    THREAD_JIT(0);
    // Leave the header word alone
    memcpy(&code->boxed_size, aligned_src + 1, (nwords-1)<<WORD_SHIFT);
    for_each_simple_fun(i, fun, code, 1, { fun->self = fun_self_from_baseptr(fun); })
    THREAD_JIT(1);
    free(src);
    // FINISH-FIXUPS didn't call SB-VM:SANCTIFY-FOR-EXECUTION
    // because the copy of the code on which it operates was only temporary.
    __clear_cache(code, (lispobj*)code + nwords);
}

void jit_copy_code_constants(lispobj lispcode, lispobj constants)
{
    struct code* code = (void*)(lispcode - OTHER_POINTER_LOWTAG);
    gc_assert(header_widetag(code->header) == CODE_HEADER_WIDETAG);
    struct vector* v = VECTOR(constants);
    gc_assert(header_widetag(v->header) == SIMPLE_VECTOR_WIDETAG);
    gc_assert(find_page_index((void*)code) >= 0);
    THREAD_JIT(0);
    sigset_t mask;
    sigemptyset(&mask);
    sigaddset(&mask, SIG_STOP_FOR_GC);
    thread_sigmask(SIG_BLOCK, &mask, 0);
    SET_WRITTEN_FLAG((lispobj*)code);
    memcpy(&code->constants, v->data, vector_len(v) * N_WORD_BYTES);
    thread_sigmask(SIG_UNBLOCK, &mask, 0);
    THREAD_JIT(1);
}

void jit_memcpy(void* dst, void* src, size_t n) {
    THREAD_JIT(0);
    memcpy(dst, src, n);
    THREAD_JIT(1);
}

void jit_patch_code(lispobj code, lispobj value, unsigned long index) {
    /* It is critical that we NOT touch the mark table if the object is off-heap.
     * With soft protection, it's doesn't matter - it's merely suboptimal - but arm64
     * uses physical protection for now, and a page fault on a page that is erroneously
     * marked (i.e. not write-protected, allegedly) would be an error.
     * Disallow GC in between setting the WRITTEN flag and doing the assigmment */
    if (find_page_index((void*)code) >= 0) {
        THREAD_JIT(0);
        sigset_t mask;
        sigemptyset(&mask);
        sigaddset(&mask, SIG_STOP_FOR_GC);
        thread_sigmask(SIG_BLOCK, &mask, 0);
        gc_card_mark[addr_to_card_index(code)] = CARD_MARKED;
        SET_WRITTEN_FLAG(native_pointer(code));
        native_pointer(code)[index] = value;
        thread_sigmask(SIG_UNBLOCK, &mask, 0);
        THREAD_JIT(1);
    } else { // Off-heap code objects can't be executed (or GC'd)
        SET_WRITTEN_FLAG(native_pointer(code));
        native_pointer(code)[index] = value;
    }
}


void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
    os_vm_address_t end_address
        = (os_vm_address_t)(((uintptr_t) address) + length);
    __clear_cache(address, end_address);
}
