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
typedef struct __darwin_mcontext64 darwin_mcontext;

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
typedef struct mcontext darwin_mcontext;

#endif

#ifdef LISP_FEATURE_SB_THREAD
pthread_mutex_t mach_exception_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER

void sigill_handler(int signal, siginfo_t *siginfo, os_context_t *context);
void sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context);
void memory_fault_handler(int signal, siginfo_t *siginfo,
                          os_context_t *context);

/* This executes in the faulting thread as part of the signal
 * emulation.  It is passed a context with the uc_mcontext field
 * pointing to a valid block of memory. */
void build_fake_signal_context(darwin_ucontext *context,
                               x86_thread_state64_t *thread_state,
                               x86_float_state64_t *float_state) {
    thread_sigmask(0, NULL, &context->uc_sigmask);
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

/* Modify a context to push new data on its stack. */
void push_context(u64 data, x86_thread_state64_t *context)
{
    u64 *stack_pointer;

    stack_pointer = (u64*) context->rsp;
    *(--stack_pointer) = data;
    context->rsp = (u64) stack_pointer;
}

void align_context_stack(x86_thread_state64_t *context)
{
    /* 16byte align the stack (provided that the stack is, as it
     * should be, 8byte aligned. */
    while (context->rsp & 15) push_context(0, context);
}

/* Stack allocation starts with a context that has a mod-4 ESP value
 * and needs to leave a context with a mod-16 ESP that will restore
 * the old ESP value and other register state when activated.  The
 * first part of this is the recovery trampoline, which loads ESP from
 * EBP, pops EBP, and returns. */
asm(".globl _stack_allocation_recover; \
    .align 4; \
 _stack_allocation_recover: \
    lea -48(%rbp), %rsp; \
    pop %rsi; \
    pop %rdi; \
    pop %rdx; \
    pop %rcx; \
    pop %r8; \
    pop %r9; \
    pop %rbp; \
    ret;");

void open_stack_allocation(x86_thread_state64_t *context)
{
    void stack_allocation_recover(void);

    push_context(context->rip, context);
    push_context(context->rbp, context);
    context->rbp = context->rsp;

    push_context(context->r9, context);
    push_context(context->r8, context);
    push_context(context->rcx, context);
    push_context(context->rdx, context);
    push_context(context->rsi, context);
    push_context(context->rdi, context);

    context->rip = (u64) stack_allocation_recover;

    align_context_stack(context);
}

/* Stack allocation of data starts with a context with a mod-16 ESP
 * value and reserves some space on it by manipulating the ESP
 * register. */
void *stack_allocate(x86_thread_state64_t *context, size_t size)
{
    /* round up size to 16byte multiple */
    size = (size + 15) & -16;

    context->rsp = ((u64)context->rsp) - size;

    return (void *)context->rsp;
}

/* Arranging to invoke a C function is tricky, as we have to assume
 * cdecl calling conventions (caller removes args) and x86/darwin
 * alignment requirements.  The simplest way to arrange this,
 * actually, is to open a new stack allocation.
 * WARNING!!! THIS DOES NOT PRESERVE REGISTERS! */
void call_c_function_in_context(x86_thread_state64_t *context,
                                void *function,
                                int nargs,
                                ...)
{
    va_list ap;
    int i;
    u64 *stack_pointer;

    /* Set up to restore stack on exit. */
    open_stack_allocation(context);

    /* Have to keep stack 16byte aligned on x86/darwin. */
    for (i = (1 & -nargs); i; i--) {
        push_context(0, context);
    }

    context->rsp = ((u64)context->rsp) - nargs * 8;
    stack_pointer = (u64 *)context->rsp;

    va_start(ap, nargs);
    if (nargs > 0) context->rdi = va_arg(ap, u64);
    if (nargs > 1) context->rsi = va_arg(ap, u64);
    if (nargs > 2) context->rdx = va_arg(ap, u64);
    if (nargs > 3) context->rcx = va_arg(ap, u64);
    if (nargs > 4) context->r8 = va_arg(ap, u64);
    if (nargs > 5) context->r9 = va_arg(ap, u64);
    for (i = 6; i < nargs; i++) {
        stack_pointer[i] = va_arg(ap, u64);
    }
    va_end(ap);

    push_context(context->rip, context);
    context->rip = (u64) function;
}

void signal_emulation_wrapper(x86_thread_state64_t *thread_state,
                              x86_float_state64_t *float_state,
                              int signal,
                              siginfo_t *siginfo,
                              void (*handler)(int, siginfo_t *, void *))
{

    darwin_ucontext context;
    darwin_mcontext regs;

    context.uc_mcontext = &regs;

    /* when BSD signals are fired, they mask they signals in sa_mask
       which always seem to be the blockable_sigset, for us, so we
       need to:
       1) save the current sigmask
       2) block blockable signals
       3) call the signal handler
       4) restore the sigmask */

    build_fake_signal_context(&context, thread_state, float_state);

    block_blockable_signals(0);

    handler(signal, siginfo, &context);

    update_thread_state_from_context(thread_state, float_state, &context);

    /* Trap to restore the signal context. */
    asm volatile (".quad 0xffffffffffff0b0f"
                  : : "a" (thread_state), "b" (float_state));
}

#if defined DUMP_CONTEXT
void dump_context(x86_thread_state64_t *context)
{
    int i;
    u64 *stack_pointer;

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

    stack_pointer = (u64 *)context->rsp;
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

void
control_stack_exhausted_handler(int signal, siginfo_t *siginfo,
                                os_context_t *context) {
    extern void unblock_signals_in_context_and_maybe_warn(os_context_t*);
    unblock_signals_in_context_and_maybe_warn(context);
    arrange_return_to_lisp_function
        (context, StaticSymbolFunction(CONTROL_STACK_EXHAUSTED_ERROR));
}

void
undefined_alien_handler(int signal, siginfo_t *siginfo, os_context_t *context) {
    arrange_return_to_lisp_function
        (context, StaticSymbolFunction(UNDEFINED_ALIEN_VARIABLE_ERROR));
}

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
    siginfo_t* siginfo;
    void (*handler)(int, siginfo_t *, os_context_t *);

#ifdef LISP_FEATURE_SB_THREAD
    thread_mutex_lock(&mach_exception_lock);
#endif

    x86_thread_state64_t thread_state;
    mach_msg_type_number_t thread_state_count = x86_THREAD_STATE64_COUNT;

#ifdef x86_AVX_STATE64_COUNT
    x86_avx_state64_t float_state;
    mach_msg_type_number_t float_state_count = avx_supported? x86_AVX_STATE64_COUNT : x86_FLOAT_STATE64_COUNT;
    x86_avx_state64_t *target_float_state;
    int float_state_flavor = avx_supported? x86_AVX_STATE64 : x86_FLOAT_STATE64;
#else
    x86_float_state64_t float_state;
    mach_msg_type_number_t float_state_count = x86_FLOAT_STATE64_COUNT;
    x86_float_state64_t *target_float_state;
    int float_state_flavor = x86_FLOAT_STATE64;
#endif

    x86_exception_state64_t exception_state;
    mach_msg_type_number_t exception_state_count = x86_EXCEPTION_STATE64_COUNT;

    x86_thread_state64_t backup_thread_state;
    x86_thread_state64_t *target_thread_state;

    os_vm_address_t addr;

    struct thread *th;

    FSHOW((stderr,"/entering catch_exception_raise with exception: %d\n", exception));
    if (mach_port_get_context(mach_task_self(), exception_port, (mach_vm_address_t *)&th)
        != KERN_SUCCESS) {
        lose("Can't find the thread for an exception %p", exception_port);
    }
    thread_get_state(thread, x86_THREAD_STATE64,
                     (thread_state_t)&thread_state, &thread_state_count);
    thread_get_state(thread, float_state_flavor,
                     (thread_state_t)&float_state, &float_state_count);
    thread_get_state(thread, x86_EXCEPTION_STATE64,
                     (thread_state_t)&exception_state, &exception_state_count);

    if (code_count && exception == EXC_BAD_ACCESS && code_vector[0] == EXC_I386_GPFLT) {
        /* This can happen for addresses larger than 48 bits,
           resulting in bogus faultvaddr. */
        addr = NULL;
    } else {
        addr = (void*)exception_state.faultvaddr;
    }
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
            handler = control_stack_exhausted_handler;
        }
        else if (addr >= undefined_alien_address &&
                 addr < undefined_alien_address + os_vm_page_size) {
            handler = undefined_alien_handler;
        } else {
            handler = memory_fault_handler;
        }
        break;
    case EXC_BAD_INSTRUCTION:

        if (*((u64 *)thread_state.rip) == 0xffffffffffff0b0f) {
            /* fake sigreturn. */

            /* When we get here, thread_state.rax is a pointer to a
             * thread_state to restore. */
            /* thread_state = *((thread_state_t *)thread_state.rax); */

            thread_set_state(thread, x86_THREAD_STATE64,
                             (thread_state_t) thread_state.rax, thread_state_count);
            thread_set_state(thread, float_state_flavor,
                             (thread_state_t) thread_state.rbx, float_state_count);
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
        }
    default:
        ret = KERN_INVALID_RIGHT;
        goto do_not_handle;
    }

    backup_thread_state = thread_state;

    /* The ABI has a 128-byte red zone. */
    stack_allocate(&thread_state, 128);

    open_stack_allocation(&thread_state);
    /* Reserve a 256 byte zone for signal handlers
     * to use on the interrupted thread stack.
     */
    stack_allocate(&thread_state, 256);

    /* Save thread state */
    target_thread_state = stack_allocate(&thread_state, sizeof(*target_thread_state));
    (*target_thread_state) = backup_thread_state;

    target_thread_state->rip += rip_offset;
    /* Save float state */
    target_float_state = stack_allocate(&thread_state, sizeof(*target_float_state));
    (*target_float_state) = float_state;

    /* Set up siginfo */
    siginfo = stack_allocate(&thread_state, sizeof(*siginfo));

    siginfo->si_signo = signal;
    siginfo->si_addr = addr;

    call_c_function_in_context(&thread_state,
                               signal_emulation_wrapper,
                               5,
                               target_thread_state,
                               target_float_state,
                               signal,
                               siginfo,
                               handler);
    thread_set_state(thread, x86_THREAD_STATE64,
                     (thread_state_t)&thread_state, thread_state_count);
    thread_set_state(thread, float_state_flavor,
                     (thread_state_t)&float_state, float_state_count);
  do_not_handle:
#ifdef LISP_FEATURE_SB_THREAD
    thread_mutex_unlock(&mach_exception_lock);
#endif

    dealloc_ret = mach_port_deallocate (mach_task_self(), thread);
    if (dealloc_ret) {
        lose("mach_port_deallocate (thread) failed with return_code %d\n", dealloc_ret);
    }

    dealloc_ret = mach_port_deallocate (mach_task_self(), task);
    if (dealloc_ret) {
        lose("mach_port_deallocate (task) failed with return_code %d\n", dealloc_ret);
    }

    return ret;
}
#endif

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
