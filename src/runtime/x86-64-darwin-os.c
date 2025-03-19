// Not sure which headers are really needed ...
#include "thread.h"
#include <sys/_types.h>
#include <sys/ucontext.h>
#include <pthread.h>

#if __DARWIN_UNIX03
#include <sys/_structs.h>
typedef struct __darwin_ucontext darwin_ucontext;

#define rip __rip
#define rsp __rsp
#define rbp __rbp
#define rax __rax
#define rbx __rbx
#define rcx __rcx
#define rdx __rdx
#define rsi __rsi
#define rdi __rdi
#define r8 __r8
#define r9 __r9
#define faultvaddr __faultvaddr
#define ss __ss
#define es __es
#define fs __fs
#define rflags __rflags

#define fpu_fcw __fpu_fcw
#define fpu_mxcsr __fpu_mxcsr

#else

typedef struct ucontext darwin_ucontext;

#endif

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

void
os_restore_fp_control(os_context_t *context)
{
    /* KLUDGE: The x87 FPU control word is some nasty bitfield struct
     * thing.  Rather than deal with that, just grab it as a 16-bit
     * integer. */
    unsigned short fpu_control_word =
        *((unsigned short *)&context->uc_mcontext->fs.fpu_fcw);
    /* reset exception flags and restore control flags on SSE2 FPU */
    unsigned int temp = (context->uc_mcontext->fs.fpu_mxcsr) & ~0x3F;
    asm ("ldmxcsr %0" : : "m" (temp));
    /* same for x87 FPU. */
    asm ("fldcw %0" : : "m" (fpu_control_word));
}

os_context_register_t *
os_context_float_register_addr(os_context_t *context, int offset)
{
  return (os_context_register_t *)((&context->uc_mcontext->__fs.__fpu_xmm0) + offset);
}

#ifdef LISP_FEATURE_PARTIAL_SW_INT_AVOIDANCE
void synchronous_trap(char* savearea, lispobj* sp_at_interrupt)
{
    struct { // unobvious order determined by gcc linux headers
        lispobj r8, r9, r10, r11, r12, r13, r14, r15,
                rdi, rsi, rbp, rbx, rdx, rax, rcx, rsp;
    } *gprsave = (void*)(savearea + 16*32); // 16 32-byte YMM regs

    os_context_t context, *c;
    memset(&context, 0, sizeof context);
    c = &context;

    // Copy the pushed regs into a machine context
    CONTEXT_SLOT(c, rax) = gprsave->rax;
    CONTEXT_SLOT(c, rbx) = gprsave->rbx;
    CONTEXT_SLOT(c, rcx) = gprsave->rcx;
    CONTEXT_SLOT(c, rdx) = gprsave->rdx;
    CONTEXT_SLOT(c, rsp) = (uword_t)sp_at_interrupt;
    // skip rbp here
    CONTEXT_SLOT(c, rsi) = gprsave->rsi;
    CONTEXT_SLOT(c, rdi) = gprsave->rdi;
    CONTEXT_SLOT(c, r8)  = gprsave->r8;
    CONTEXT_SLOT(c, r9)  = gprsave->r9;
    CONTEXT_SLOT(c, r10) = gprsave->r10;
    CONTEXT_SLOT(c, r11) = gprsave->r11;
    CONTEXT_SLOT(c, r12) = gprsave->r12;
    CONTEXT_SLOT(c, r13) = gprsave->r13;
    CONTEXT_SLOT(c, r14) = gprsave->r14;
    CONTEXT_SLOT(c, r15) = gprsave->r15;

    // Take the return-PC to the user code which is 1 word down from exactly where
    // the stack-pointer was at the simulated INT3, then add 1 because a real INT
    // instructions leaves the PC pointing after it.
    long pc_at_interrupt = sp_at_interrupt[-1];
    CONTEXT_SLOT(c, rip) = 1 + pc_at_interrupt;
    // The first instruction of the asm routine was to push EFLAGS
    CONTEXT_SLOT(c, rflags) = sp_at_interrupt[-2];
    // The next instruction was to push RBP
    CONTEXT_SLOT(c, rbp) = sp_at_interrupt[-3];

    sigset_t curmask;
    thread_sigmask(SIG_BLOCK, &blockable_sigset, &curmask);

    interrupt_handle_pending(&context); // GC

    thread_sigmask(SIG_SETMASK, &curmask, 0);
}
#endif
