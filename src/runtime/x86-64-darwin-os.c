#ifdef LISP_FEATURE_SB_THREAD
#include <architecture/i386/table.h>
#include <i386/user_ldt.h>
#include <mach/mach_init.h>
#endif

#include "thread.h"
#include "validate.h"
#include "runtime.h"
#include "interrupt.h"
#include "x86-64-darwin-os.h"
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

#else

typedef struct ucontext darwin_ucontext;
typedef struct mcontext darwin_mcontext;

#endif

#ifdef LISP_FEATURE_SB_THREAD
pthread_mutex_t mach_exception_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER

kern_return_t mach_thread_init(mach_port_t thread_exception_port);

void sigill_handler(int signal, siginfo_t *siginfo, os_context_t *context);
void sigtrap_handler(int signal, siginfo_t *siginfo, os_context_t *context);
void memory_fault_handler(int signal, siginfo_t *siginfo,
                          os_context_t *context);

/* exc_server handles mach exception messages from the kernel and
 * calls catch exception raise. We use the system-provided
 * mach_msg_server, which, I assume, calls exc_server in a loop.
 *
 */
extern boolean_t exc_server();

/* This executes in the faulting thread as part of the signal
 * emulation.  It is passed a context with the uc_mcontext field
 * pointing to a valid block of memory. */
void build_fake_signal_context(darwin_ucontext *context,
                               x86_thread_state64_t *thread_state,
                               x86_float_state64_t *float_state) {
    pthread_sigmask(0, NULL, &context->uc_sigmask);
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
    pthread_sigmask(SIG_SETMASK, &context->uc_sigmask, NULL);
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

    /* CLH: FIXME **NOTE: HACK ALERT!** Ideally, we would allocate
     * context and regs on the stack as local variables, but this
     * causes problems for the lisp debugger. When it walks the stack
     * for a back trace, it sees the 1) address of the local variable
     * on the stack and thinks that is a frame pointer to a lisp
     * frame, and, 2) the address of the sap that we alloc'ed in
     * dynamic space and thinks that is a return address, so it,
     * heuristicly (and wrongly), chooses that this should be
     * interpreted as a lisp frame instead of as a C frame.
     * We can work around this in this case by os_validating the
     * context (and regs just for symmetry).
     */

    darwin_ucontext  *context;
    darwin_mcontext *regs;

    context = (darwin_ucontext *) os_validate(0, sizeof(darwin_ucontext));
    regs = (darwin_mcontext*) os_validate(0, sizeof(darwin_mcontext));
    context->uc_mcontext = regs;

    /* when BSD signals are fired, they mask they signals in sa_mask
       which always seem to be the blockable_sigset, for us, so we
       need to:
       1) save the current sigmask
       2) block blockable signals
       3) call the signal handler
       4) restore the sigmask */

    build_fake_signal_context(context, thread_state, float_state);

    block_blockable_signals(0, 0);

    handler(signal, siginfo, context);

    update_thread_state_from_context(thread_state, float_state, context);

    os_invalidate((os_vm_address_t)context, sizeof(darwin_ucontext));
    os_invalidate((os_vm_address_t)regs, sizeof(darwin_mcontext));

    /* Trap to restore the signal context. */
    asm volatile ("mov %0, %%rax; mov %1, %%rbx; .quad 0xffffffffffff0b0f"
                  : : "r" (thread_state), "r" (float_state));
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
    kern_return_t ret;
    int signal;
    siginfo_t* siginfo;

#ifdef LISP_FEATURE_SB_THREAD
    thread_mutex_lock(&mach_exception_lock);
#endif

    x86_thread_state64_t thread_state;
    mach_msg_type_number_t thread_state_count = x86_THREAD_STATE64_COUNT;

    x86_float_state64_t float_state;
    mach_msg_type_number_t float_state_count = x86_FLOAT_STATE64_COUNT;

    x86_exception_state64_t exception_state;
    mach_msg_type_number_t exception_state_count = x86_EXCEPTION_STATE64_COUNT;

    x86_thread_state64_t backup_thread_state;
    x86_thread_state64_t *target_thread_state;
    x86_float_state64_t *target_float_state;

    os_vm_address_t addr;

    struct thread *th = (struct thread*) exception_port;

    FSHOW((stderr,"/entering catch_exception_raise with exception: %d\n", exception));

    switch (exception) {

    case EXC_BAD_ACCESS:
        signal = SIGBUS;
        ret = thread_get_state(thread,
                               x86_THREAD_STATE64,
                               (thread_state_t)&thread_state,
                               &thread_state_count);
        ret = thread_get_state(thread,
                               x86_FLOAT_STATE64,
                               (thread_state_t)&float_state,
                               &float_state_count);
        ret = thread_get_state(thread,
                               x86_EXCEPTION_STATE64,
                               (thread_state_t)&exception_state,
                               &exception_state_count);
        addr = (void*)exception_state.faultvaddr;


        /* note the os_context hackery here.  When the signal handler returns,
         * it won't go back to what it was doing ... */
        if(addr >= CONTROL_STACK_GUARD_PAGE(th) &&
           addr < CONTROL_STACK_GUARD_PAGE(th) + os_vm_page_size) {
            /* We hit the end of the control stack: disable guard page
             * protection so the error handler has some headroom, protect the
             * previous page so that we can catch returns from the guard page
             * and restore it. */
            lower_thread_control_stack_guard_page(th);

            backup_thread_state = thread_state;
            open_stack_allocation(&thread_state);
            /* Reserve a 256 byte zone for signal handlers
             * to use on the interrupted thread stack.
             */
            stack_allocate(&thread_state, 256);

            /* Save thread state */
            target_thread_state =
                stack_allocate(&thread_state, sizeof(*target_thread_state));
            (*target_thread_state) = backup_thread_state;

            /* Save float state */
            target_float_state =
                stack_allocate(&thread_state, sizeof(*target_float_state));
            (*target_float_state) = float_state;

            /* Set up siginfo */
            siginfo = stack_allocate(&thread_state, sizeof(*siginfo));
            /* what do we need to put in our fake siginfo?  It looks like
             * the x86 code only uses si_signo and si_adrr. */
            siginfo->si_signo = signal;
            siginfo->si_addr = (void*)exception_state.faultvaddr;

            call_c_function_in_context(&thread_state,
                                       signal_emulation_wrapper,
                                       5,
                                       target_thread_state,
                                       target_float_state,
                                       signal,
                                       siginfo,
                                       control_stack_exhausted_handler);
        }
        else if(addr >= CONTROL_STACK_RETURN_GUARD_PAGE(th) &&
                addr < CONTROL_STACK_RETURN_GUARD_PAGE(th) + os_vm_page_size) {
            /* We're returning from the guard page: reprotect it, and
             * unprotect this one. This works even if we somehow missed
             * the return-guard-page, and hit it on our way to new
             * exhaustion instead. */
            reset_thread_control_stack_guard_page(th);
        }
        else if (addr >= undefined_alien_address &&
                 addr < undefined_alien_address + os_vm_page_size) {
            backup_thread_state = thread_state;
            open_stack_allocation(&thread_state);
            stack_allocate(&thread_state, 256);

            /* Save thread state */
            target_thread_state =
                stack_allocate(&thread_state, sizeof(*target_thread_state));
            (*target_thread_state) = backup_thread_state;

            target_float_state =
                stack_allocate(&thread_state, sizeof(*target_float_state));
            (*target_float_state) = float_state;

            /* Set up siginfo */
            siginfo = stack_allocate(&thread_state, sizeof(*siginfo));
            /* what do we need to put in our fake siginfo?  It looks like
             * the x86 code only uses si_signo and si_adrr. */
            siginfo->si_signo = signal;
            siginfo->si_addr = (void*)exception_state.faultvaddr;

            call_c_function_in_context(&thread_state,
                                       signal_emulation_wrapper,
                                       5,
                                       target_thread_state,
                                       target_float_state,
                                       signal,
                                       siginfo,
                                       undefined_alien_handler);
        } else {

            backup_thread_state = thread_state;
            open_stack_allocation(&thread_state);
            stack_allocate(&thread_state, 256);

            /* Save thread state */
            target_thread_state =
                stack_allocate(&thread_state, sizeof(*target_thread_state));
            (*target_thread_state) = backup_thread_state;

            target_float_state =
                stack_allocate(&thread_state, sizeof(*target_float_state));
            (*target_float_state) = float_state;

            /* Set up siginfo */
            siginfo = stack_allocate(&thread_state, sizeof(*siginfo));
            /* what do we need to put in our fake siginfo?  It looks like
             * the x86 code only uses si_signo and si_adrr. */
            siginfo->si_signo = signal;
            siginfo->si_addr = (void*)exception_state.faultvaddr;

            call_c_function_in_context(&thread_state,
                                       signal_emulation_wrapper,
                                       5,
                                       target_thread_state,
                                       target_float_state,
                                       signal,
                                       siginfo,
                                       memory_fault_handler);
        }
        ret = thread_set_state(thread,
                               x86_THREAD_STATE64,
                               (thread_state_t)&thread_state,
                               thread_state_count);

        ret = thread_set_state(thread,
                               x86_FLOAT_STATE64,
                               (thread_state_t)&float_state,
                               float_state_count);
#ifdef LISP_FEATURE_SB_THREAD
        thread_mutex_unlock(&mach_exception_lock);
#endif
        return KERN_SUCCESS;

    case EXC_BAD_INSTRUCTION:

        ret = thread_get_state(thread,
                               x86_THREAD_STATE64,
                               (thread_state_t)&thread_state,
                               &thread_state_count);
        ret = thread_get_state(thread,
                               x86_FLOAT_STATE64,
                               (thread_state_t)&float_state,
                               &float_state_count);
        ret = thread_get_state(thread,
                               x86_EXCEPTION_STATE64,
                               (thread_state_t)&exception_state,
                               &exception_state_count);
        if (0xffffffffffff0b0f == *((u64 *)thread_state.rip)) {
            /* fake sigreturn. */

            /* When we get here, thread_state.rax is a pointer to a
             * thread_state to restore. */
            /* thread_state = *((thread_state_t *)thread_state.rax); */

            ret = thread_set_state(thread,
                                   x86_THREAD_STATE64,
                                   (thread_state_t) thread_state.rax,
                                   /* &thread_state, */
                                   thread_state_count);

            ret = thread_set_state(thread,
                                   x86_FLOAT_STATE64,
                                   (thread_state_t) thread_state.rbx,
                                   /* &thread_state, */
                                   float_state_count);
        } else {

            backup_thread_state = thread_state;
            open_stack_allocation(&thread_state);
            stack_allocate(&thread_state, 256);

            /* Save thread state */
            target_thread_state =
                stack_allocate(&thread_state, sizeof(*target_thread_state));
            (*target_thread_state) = backup_thread_state;

            target_float_state =
                stack_allocate(&thread_state, sizeof(*target_float_state));
            (*target_float_state) = float_state;

            /* Set up siginfo */
            siginfo = stack_allocate(&thread_state, sizeof(*siginfo));
            /* what do we need to put in our fake siginfo?  It looks like
             * the x86 code only uses si_signo and si_adrr. */
            if (*((unsigned short *)target_thread_state->rip) == 0x0b0f) {
                signal = SIGTRAP;
                siginfo->si_signo = signal;
                siginfo->si_addr = (void*)exception_state.faultvaddr;
                target_thread_state->rip += 2;
                call_c_function_in_context(&thread_state,
                                           signal_emulation_wrapper,
                                           5,
                                           target_thread_state,
                                           target_float_state,
                                           signal,
                                           siginfo,
                                           sigtrap_handler);
            } else {
                signal = SIGILL;
                siginfo->si_signo = signal;
                siginfo->si_addr = (void*)exception_state.faultvaddr;

                call_c_function_in_context(&thread_state,
                                           signal_emulation_wrapper,
                                           5,
                                           target_thread_state,
                                           target_float_state,
                                           signal,
                                           siginfo,
                                           sigill_handler);
            }
            ret = thread_set_state(thread,
                                   x86_THREAD_STATE64,
                                   (thread_state_t)&thread_state,
                                   thread_state_count);
            ret = thread_set_state(thread,
                                   x86_FLOAT_STATE64,
                                   (thread_state_t)&float_state,
                                   float_state_count);
        }
#ifdef LISP_FEATURE_SB_THREAD
        thread_mutex_unlock(&mach_exception_lock);
#endif
        return KERN_SUCCESS;

    default:
#ifdef LISP_FEATURE_SB_THREAD
        thread_mutex_unlock(&mach_exception_lock);
#endif
        return KERN_INVALID_RIGHT;
    }
}

void *
mach_exception_handler(void *port)
{
  mach_msg_server(exc_server, 2048, (mach_port_t) port, 0);
  /* mach_msg_server should never return, but it should dispatch mach
   * exceptions to our catch_exception_raise function
   */
  lose("mach_msg_server returned");
}

/* Sets up the thread that will listen for mach exceptions. note that
   the exception handlers will be run on this thread. This is
   different from the BSD-style signal handling situation in which the
   signal handlers run in the relevant thread directly. */

mach_port_t mach_exception_handler_port_set = MACH_PORT_NULL;

pthread_t
setup_mach_exception_handling_thread()
{
    kern_return_t ret;
    pthread_t mach_exception_handling_thread = NULL;
    pthread_attr_t attr;

    /* allocate a mach_port for this process */
    ret = mach_port_allocate(mach_task_self(),
                             MACH_PORT_RIGHT_PORT_SET,
                             &mach_exception_handler_port_set);

    /* create the thread that will receive the mach exceptions */

    FSHOW((stderr, "Creating mach_exception_handler thread!\n"));

    pthread_attr_init(&attr);
    pthread_create(&mach_exception_handling_thread,
                   &attr,
                   mach_exception_handler,
                   (void*) mach_exception_handler_port_set);
    pthread_attr_destroy(&attr);

    return mach_exception_handling_thread;
}

/* tell the kernel that we want EXC_BAD_ACCESS exceptions sent to the
   exception port (which is being listened to do by the mach
   exception handling thread). */
kern_return_t
mach_thread_init(mach_port_t thread_exception_port)
{
    kern_return_t ret;
    /* allocate a named port for the thread */

    FSHOW((stderr, "Allocating mach port %x\n", thread_exception_port));

    ret = mach_port_allocate_name(mach_task_self(),
                                  MACH_PORT_RIGHT_RECEIVE,
                                  thread_exception_port);
    if (ret) {
        lose("mach_port_allocate_name failed with return_code %d\n", ret);
    }

    /* establish the right for the thread_exception_port to send messages */
    ret = mach_port_insert_right(mach_task_self(),
                                 thread_exception_port,
                                 thread_exception_port,
                                 MACH_MSG_TYPE_MAKE_SEND);
    if (ret) {
        lose("mach_port_insert_right failed with return_code %d\n", ret);
    }

    ret = thread_set_exception_ports(mach_thread_self(),
                                     EXC_MASK_BAD_ACCESS | EXC_MASK_BAD_INSTRUCTION,
                                     thread_exception_port,
                                     EXCEPTION_DEFAULT,
                                     THREAD_STATE_NONE);
    if (ret) {
        lose("thread_set_exception_port failed with return_code %d\n", ret);
    }

    ret = mach_port_move_member(mach_task_self(),
                                thread_exception_port,
                                mach_exception_handler_port_set);
    if (ret) {
        lose("mach_port_ failed with return_code %d\n", ret);
    }

    return ret;
}

void
setup_mach_exceptions() {
    setup_mach_exception_handling_thread();
    mach_thread_init(THREAD_STRUCT_TO_EXCEPTION_PORT(all_threads));
}

pid_t
mach_fork() {
    pid_t pid = fork();
    if (pid == 0) {
        setup_mach_exceptions();
        return pid;
    } else {
        return pid;
    }
}

#endif
