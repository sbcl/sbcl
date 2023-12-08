#include "thread.h"
#include "gc.h"
#include "code.h"
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
    THREAD_JIT_WP(0);
    *address = value;
    THREAD_JIT_WP(1);
}

void jit_copy_code_insts(lispobj dst, lispobj* src)
{
    lispobj* aligned_src = (lispobj*)(src + ((uword_t)src & N_WORD_BYTES)); // align up
    struct code* code = (struct code*)(dst-OTHER_POINTER_LOWTAG);
    int nwords = code_total_nwords(code);
    gc_assert(code_total_nwords((struct code*)aligned_src));
    THREAD_JIT_WP(0);
    // Leave the header word alone
    memcpy(&code->boxed_size, aligned_src + 1, (nwords-1)<<WORD_SHIFT);
    for_each_simple_fun(i, fun, code, 1, { fun->self = fun_self_from_baseptr(fun); })
    THREAD_JIT_WP(1);
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

    sigset_t mask;
    block_blockable_signals(&mask);
    THREAD_JIT_WP(0);
    gc_card_mark[addr_to_card_index(code)] = CARD_MARKED;
    SET_WRITTEN_FLAG((lispobj*)code);
    memcpy(&code->constants, v->data, vector_len(v) * N_WORD_BYTES);
    THREAD_JIT_WP(1);
    thread_sigmask(SIG_SETMASK, &mask, 0);
}

void jit_memcpy(void* dst, void* src, size_t n) {
    THREAD_JIT_WP(0);
    memcpy(dst, src, n);
    THREAD_JIT_WP(1);
}

void jit_patch_code(lispobj code, lispobj value, unsigned long index) {
    /* It is better not to the touch a card mark if the object is off-heap,
     * though it's not terribly important any more */
    if (find_page_index((void*)code) >= 0) {
        sigset_t mask;
        // Disallow GC in between setting the WRITTEN flag and doing the assigmment
        block_blockable_signals(&mask);
        THREAD_JIT_WP(0);

        gc_card_mark[addr_to_card_index(code)] = CARD_MARKED;
        SET_WRITTEN_FLAG(native_pointer(code));
        native_pointer(code)[index] = value;

        THREAD_JIT_WP(1);
        thread_sigmask(SIG_SETMASK, &mask, 0);
    } else { // Off-heap code objects can't be executed (or GC'd)
             // umm, so why do they exist? I wish I could remember...
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

void
sigill_handler(int signal, siginfo_t *siginfo, os_context_t *context) {
    int esr;
    if (siginfo->si_code == ILL_ILLTRP &&
        ((esr = context->uc_mcontext->__es.__esr)>>26 & 0x3f) == 0x2C) {
        int code = 0;
        if (esr & 1 << 4) {
            code = FPE_FLTRES;
        }
        else if (esr & 1 << 3) {
            code = FPE_FLTUND;
        } else if (esr & 1 << 2) {
            code = FPE_FLTOVF;
        } else if (esr & 1 << 1) {
            code = FPE_FLTDIV;
        } else if (esr & 1) {
            code = FPE_FLTINV;
        }
/*        if (esr & 1 << 7) {
          Input Denormal Floating-point exception trapped bit.
          No FPE_ constant for it.
        }
*/
        siginfo->si_code = code;
        return interrupt_handle_now(SIGFPE, siginfo, context);
    }

    fake_foreign_function_call(context);
    lose("Unhandled SIGILL at %p.", (void*)OS_CONTEXT_PC(context));
}
