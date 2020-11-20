#ifdef LISP_FEATURE_SB_THREAD
#include <mach/mach_init.h>
#endif

#include "thread.h"
#include "validate.h"
#include "runtime.h"
#include "interrupt.h"
#include "x86-64-darwin-os.h"
#include "x86-64-arch.h"
#include "genesis/fdefn.h"
#include "gc-internal.h"
#include "arch.h"

#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/mach_types.h>
#include <mach/sync_policy.h>
#include <mach/machine/thread_state.h>
#include <mach/machine/thread_status.h>
#include <sys/_types.h>
#include <sys/ucontext.h>
#include <pthread.h>
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#if __DARWIN_UNIX03
#include <sys/_structs.h>
#endif

#if __DARWIN_UNIX03

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

#ifdef x86_AVX_STATE64_COUNT
typedef _STRUCT_MCONTEXT_AVX64 darwin_mcontext;
#else
typedef _STRUCT_MCONTEXT64 darwin_mcontext;
#endif

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER

void sigill_handler(int signal, siginfo_t *siginfo, os_context_t *context);
void sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context);
void memory_fault_handler(int signal, siginfo_t *siginfo,
                          os_context_t *context);

void
undefined_alien_handler(int signal, siginfo_t *siginfo, os_context_t *context) {
    arrange_return_to_lisp_function
        (context, StaticSymbolFunction(UNDEFINED_ALIEN_VARIABLE_ERROR));
}

/* This executes in the faulting thread as part of the signal
 * emulation.  It is passed a context with the uc_mcontext field
 * pointing to a valid block of memory. */
void build_fake_signal_context(darwin_ucontext *context,
                               x86_thread_state64_t *thread_state,
                               x86_float_state64_t *float_state) {
    block_blockable_signals(&context->uc_sigmask);
    context->uc_mcontext->ss = *thread_state;
    context->uc_mcontext->fs = *float_state;
}

/* This executes in the faulting thread as part of the signal
 * emulation.  It is effectively the inverse operation from above. */
void update_thread_state_from_context(x86_thread_state64_t *thread_state,
                                      x86_float_state64_t *float_state,
                                      darwin_ucontext  *context) {
    *thread_state = context->uc_mcontext->ss;
    *float_state = context->uc_mcontext->fs;
    thread_sigmask(SIG_SETMASK, &context->uc_sigmask, NULL);
}

boolean will_exhaust_stack(struct thread * th, x86_thread_state64_t *context, int size) {
    __uint64_t sp = context->rsp - size;

    if(sp < (__uint64_t)(CONTROL_STACK_HARD_GUARD_PAGE(th) + os_vm_page_size)) {
        lose("Control stack exhausted during signal emulation: PC: %llx",
             context->rip);
    }

    if(sp < (__uint64_t)(CONTROL_STACK_GUARD_PAGE(th) + os_vm_page_size) &&
       th->state_word.control_stack_guard_page_protected) {
        /* We hit the end of the control stack: disable guard page
         * protection so the error handler has some headroom, protect the
         * previous page so that we can catch returns from the guard page
         * and restore it. */
        lower_thread_control_stack_guard_page(th);
        context->rsp = (__uint64_t)(CONTROL_STACK_GUARD_PAGE(th) + os_vm_page_size);
        return 1;
    }
    return 0;
}

/* Modify a context to push new data on its stack. */
void push_context(uint64_t data, x86_thread_state64_t *context)
{
    uint64_t* stack_pointer = (uint64_t*)context->rsp - 1;

    context->rsp = (__uint64_t) stack_pointer;
    *stack_pointer = data;
}

void align_context_stack(x86_thread_state64_t *context)
{
    /* 16-byte align the stack. */
    context->rsp &= ~15;
}

void *stack_allocate(struct thread * th, x86_thread_state64_t *context, size_t size)
{

    context->rsp = context->rsp - size;
    return (void*)context->rsp;
}

void signal_emulation_wrapper(darwin_mcontext *mcontext,
                              int signal,
                              os_vm_address_t addr,
                              void (*handler)(int, siginfo_t *, void *))
{
    siginfo_t siginfo;
    darwin_ucontext context;

    siginfo.si_signo = signal;
    siginfo.si_addr = addr;

    context.uc_mcontext = (_STRUCT_MCONTEXT64 *)mcontext;

    /* when BSD signals are fired, they mask they signals in sa_mask
       which always seem to be the blockable_sigset, for us, so we
       need to:
       1) save the current sigmask
       2) block blockable signals
       3) call the signal handler
       4) restore the sigmask */

    block_blockable_signals(&context.uc_sigmask);

    handler(signal, &siginfo, &context);

    thread_sigmask(SIG_SETMASK, &context.uc_sigmask, NULL);

    /* Trap to restore the signal context. */
    asm volatile (".quad 0xffffffffffff0b0f"
                  : : "a" (mcontext));
}

void call_signal_emulator_in_context(x86_thread_state64_t *context,
                                     darwin_mcontext *mcontext,
                                     int signal,
                                     os_vm_address_t addr,
                                     void* handler)
{

    align_context_stack(context);
    push_context(context->rip, context);
    context->rdi = (uint64_t) mcontext;
    context->rsi = signal;
    context->rdx = (uint64_t) addr;
    context->rcx = (uint64_t) handler;

    context->rip = (uint64_t) signal_emulation_wrapper;
}

/* Call CONTROL_STACK_EXHAUSTED_ERROR directly, without emulating
   signals. It doesn't need any signal contexts and it's better to use
   as little stack as possible. */
void call_stack_exhausted_in_context(x86_thread_state64_t *context)
{
    align_context_stack(context);
    push_context(context->rip, context);
    context->rdi = (uint64_t) StaticSymbolFunction(CONTROL_STACK_EXHAUSTED_ERROR);
    context->rip = (uint64_t) funcall0;
}

#if defined DUMP_CONTEXT
void dump_context(x86_thread_state64_t *context)
{
    int i;
    uint64_t *stack_pointer;

    printf("rax: %08lx  rcx: %08lx  rdx: %08lx  rbx: %08lx\n",
           context->rax, context->rcx, context->rdx, context->rbx);
    printf("rsp: %08lx  rbp: %08lx  rsi: %08lx  rdi: %08lx\n",
           context->rsp, context->rbp, context->rsi, context->rdi);
    printf("rip: %08lx  eflags: %08lx\n",
           context->rip, context->rflags);
    printf("cs: %04hx  ds: %04hx  es: %04hx  "
           "ss: %04hx  fs: %04hx  gs: %04hx\n",
           context->cs, context->ds, context->rs,
           context->ss, context->fs, context->gs);

    stack_pointer = (uint64_t *)context->rsp;
    for (i = 0; i < 48; i+=4) {
        printf("%08x:  %08x %08x %08x %08x\n",
               context->rsp + (i * 4),
               stack_pointer[i],
               stack_pointer[i+1],
               stack_pointer[i+2],
               stack_pointer[i+3]);
    }
}
#endif

kern_return_t
catch_exception_raise(mach_port_t exception_port,
                      mach_port_t thread,
                      mach_port_t task,
                      exception_type_t exception,
                      exception_data_t code_vector,
                      mach_msg_type_number_t code_count)
{
    kern_return_t ret = KERN_SUCCESS, dealloc_ret;
    int signal, rip_offset = 0;
    void (*handler)(int, siginfo_t *, os_context_t *);

    x86_thread_state64_t thread_state;
    mach_msg_type_number_t thread_state_count = x86_THREAD_STATE64_COUNT;

#ifdef x86_AVX_STATE64_COUNT
    mach_msg_type_number_t float_state_count = avx_supported? x86_AVX_STATE64_COUNT : x86_FLOAT_STATE64_COUNT;
    int float_state_flavor = avx_supported? x86_AVX_STATE64 : x86_FLOAT_STATE64;
#else
    mach_msg_type_number_t float_state_count = x86_FLOAT_STATE64_COUNT;
    int float_state_flavor = x86_FLOAT_STATE64;
#endif

    x86_exception_state64_t exception_state;
    mach_msg_type_number_t exception_state_count = x86_EXCEPTION_STATE64_COUNT;

    x86_thread_state64_t backup_thread_state;

    os_vm_address_t addr;

    thread_get_state(thread, x86_EXCEPTION_STATE64,
                     (thread_state_t)&exception_state, &exception_state_count);

    if (code_count && exception == EXC_BAD_ACCESS && code_vector[0] == EXC_I386_GPFLT) {
        /* This can happen for addresses larger than 48 bits,
           resulting in bogus faultvaddr. */
        addr = NULL;
    } else {
        addr = (void*)exception_state.faultvaddr;
    }

    /* Just need to unprotect the page and do some bookkeeping, no need
     * to run it from the faulting thread.
     * And because the GC uses signals to stop the world it might
     * interfere with that bookkeeping, because there's a window
     * before block_blockable_signals is performed. */
    if (exception == EXC_BAD_ACCESS && gencgc_handle_wp_violation(addr)) {
        goto do_not_handle;
    }

    struct thread *th;

    FSHOW((stderr,"/entering catch_exception_raise with exception: %d\n", exception));
    if (mach_port_get_context(mach_task_self(), exception_port, (mach_vm_address_t *)&th)
        != KERN_SUCCESS) {
        lose("Can't find the thread for an exception %u", exception_port);
    }
    thread_get_state(thread, x86_THREAD_STATE64,
                     (thread_state_t)&thread_state, &thread_state_count);

    boolean stack_unprotected = 0;

    switch (exception) {

    case EXC_BAD_ACCESS:
        signal = SIGBUS;

        if(addr >= CONTROL_STACK_RETURN_GUARD_PAGE(th) &&
           addr < CONTROL_STACK_RETURN_GUARD_PAGE(th) + os_vm_page_size) {
            /* We're returning from the guard page: reprotect it, and
             * unprotect this one. This works even if we somehow missed
             * the return-guard-page, and hit it on our way to new
             * exhaustion instead. */
            reset_thread_control_stack_guard_page(th);
            goto do_not_handle;
        }

        /* note the os_context hackery here.  When the signal handler returns,
         * it won't go back to what it was doing ... */
        if(addr >= CONTROL_STACK_GUARD_PAGE(th) &&
           addr < CONTROL_STACK_GUARD_PAGE(th) + os_vm_page_size) {
            /* We hit the end of the control stack: disable guard page
             * protection so the error handler has some headroom, protect the
             * previous page so that we can catch returns from the guard page
             * and restore it. */
            lower_thread_control_stack_guard_page(th);
            stack_unprotected = 1;
        }
        else if (addr >= undefined_alien_address &&
                 addr < undefined_alien_address + os_vm_page_size) {
            handler = undefined_alien_handler;
        } else {
            handler = memory_fault_handler;
        }
        break;
    case EXC_BAD_INSTRUCTION:

        if (*((uint64_t *)thread_state.rip) == 0xffffffffffff0b0f) {
            /* Fake sigreturn. See the end of signal_emulation_wrapper() */

            /* Apply any modifications done to the context, */

            darwin_mcontext *mcontext = (darwin_mcontext *) thread_state.rax;

            thread_set_state(thread, x86_THREAD_STATE64,
                             (thread_state_t) &mcontext->ss, thread_state_count);
            thread_set_state(thread, float_state_flavor,
                             (thread_state_t) &mcontext->fs, float_state_count);
            goto do_not_handle;
        } else if (*((unsigned short *)thread_state.rip) == 0x0b0f) {
            signal = SIGTRAP;
            rip_offset = 2;
            handler = sigtrap_handler;
        } else {
            signal = SIGILL;
            handler = sigill_handler;
        }

        break;
    case EXC_BREAKPOINT:
        if (single_stepping) {
            signal = SIGTRAP;
            /* Clear TF or the signal emulation wrapper won't proceed
               with single stepping enabled. */
            thread_state.rflags &= ~0x100;
            handler = sigtrap_handler;
            break;
        } else if (*(unsigned char*)(thread_state.rip-1) == 0xCC) {
            signal = SIGTRAP;
            handler = sigtrap_handler;
            break;
        }
    default:
        ret = KERN_INVALID_RIGHT;
        goto do_not_handle;
    }

    backup_thread_state = thread_state;

    /* The ABI has a 128-byte red zone. */
    stack_allocate(th, &thread_state, 128);

    if (will_exhaust_stack(th, &thread_state,
                           /* Won't be passing much to stack_exhausted_error */
                           stack_unprotected ? N_WORD_BYTES*4 :
                           ALIGN_UP(sizeof(darwin_mcontext) +
                                    N_WORD_BYTES, /* return address */
                                    N_WORD_BYTES*2))
        || stack_unprotected) {
        call_stack_exhausted_in_context(&thread_state);
    } else {

        darwin_mcontext *mcontext = stack_allocate(th, &thread_state, sizeof(darwin_mcontext));

        backup_thread_state.rip += rip_offset;
        mcontext->ss = backup_thread_state;

        thread_get_state(thread, float_state_flavor, (thread_state_t) &mcontext->fs, &float_state_count);

        call_signal_emulator_in_context(&thread_state,
                                        mcontext,
                                        signal,
                                        addr,
                                        handler);
    }

    thread_set_state(thread, x86_THREAD_STATE64,
                     (thread_state_t)&thread_state, thread_state_count);
  do_not_handle:

    dealloc_ret = mach_port_deallocate (mach_task_self(), thread);
    if (dealloc_ret) {
        lose("mach_port_deallocate (thread) failed with return_code %d", dealloc_ret);
    }

    dealloc_ret = mach_port_deallocate (mach_task_self(), task);
    if (dealloc_ret) {
        lose("mach_port_deallocate (task) failed with return_code %d", dealloc_ret);
    }

    return ret;
}
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
