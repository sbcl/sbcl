

#ifdef LISP_FEATURE_SB_THREAD
#include <architecture/i386/table.h>
#include <i386/user_ldt.h>
#include <mach/mach_init.h>
#endif

#include "thread.h"
#include "validate.h"
#include "runtime.h"
#include "interrupt.h"
#include "x86-darwin-os.h"
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

#ifdef LISP_FEATURE_SB_THREAD

pthread_mutex_t modify_ldt_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t mach_exception_lock = PTHREAD_MUTEX_INITIALIZER;

void set_data_desc_size(data_desc_t* desc, unsigned long size)
{
    desc->limit00 = (size - 1) & 0xffff;
    desc->limit16 = ((size - 1) >> 16) &0xf;
}

void set_data_desc_addr(data_desc_t* desc, void* addr)
{
    desc->base00 = (unsigned int)addr & 0xffff;
    desc->base16 = ((unsigned int)addr & 0xff0000) >> 16;
    desc->base24 = ((unsigned int)addr & 0xff000000) >> 24;
}

#endif

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
kern_return_t mach_thread_init(mach_port_t thread_exception_port);
#endif

int arch_os_thread_init(struct thread *thread) {
#ifdef LISP_FEATURE_SB_THREAD
    int n;
    sel_t sel;

    data_desc_t ldt_entry = { 0, 0, 0, DESC_DATA_WRITE,
                              3, 1, 0, DESC_DATA_32B, DESC_GRAN_BYTE, 0 };

    set_data_desc_addr(&ldt_entry, thread);
    set_data_desc_size(&ldt_entry, dynamic_values_bytes);

    thread_mutex_lock(&modify_ldt_lock);
    n = i386_set_ldt(LDT_AUTO_ALLOC, (union ldt_entry*) &ldt_entry, 1);

    if (n < 0) {
        perror("i386_set_ldt");
        lose("unexpected i386_set_ldt(..) failure\n");
    }
    thread_mutex_unlock(&modify_ldt_lock);

    FSHOW_SIGNAL((stderr, "/ TLS: Allocated LDT %x\n", n));
    sel.index = n;
    sel.rpl = USER_PRIV;
    sel.ti = SEL_LDT;

    __asm__ __volatile__ ("mov %0, %%fs" : : "r"(sel));

    thread->tls_cookie=n;
    pthread_setspecific(specials,thread);
#endif
#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
    mach_thread_init(THREAD_STRUCT_TO_EXCEPTION_PORT(thread));
#endif

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    stack_t sigstack;

    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    sigstack.ss_sp=((void *) thread)+dynamic_values_bytes;
    sigstack.ss_flags=0;
    sigstack.ss_size = 32*SIGSTKSZ;
    sigaltstack(&sigstack,0);
#endif
    return 1;                  /* success */
}

int arch_os_thread_cleanup(struct thread *thread) {
#if defined(LISP_FEATURE_SB_THREAD)
    int n = thread->tls_cookie;

    /* Set the %%fs register back to 0 and free the the ldt
     * by setting it to NULL.
     */
    FSHOW_SIGNAL((stderr, "/ TLS: Freeing LDT %x\n", n));

    __asm__ __volatile__ ("mov %0, %%fs" : : "r"(0));
    thread_mutex_lock(&modify_ldt_lock);
    i386_set_ldt(n, NULL, 1);
    thread_mutex_unlock(&modify_ldt_lock);
#endif
    return 1;                  /* success */
}

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER

void sigill_handler(int signal, siginfo_t *siginfo, void *void_context);
void sigtrap_handler(int signal, siginfo_t *siginfo, void *void_context);
void memory_fault_handler(int signal, siginfo_t *siginfo, void *void_context);

/* exc_server handles mach exception messages from the kernel and
 * calls catch exception raise. We use the system-provided
 * mach_msg_server, which, I assume, calls exc_server in a loop.
 *
 */
extern boolean_t exc_server();

/* This executes in the faulting thread as part of the signal
 * emulation.  It is passed a context with the uc_mcontext field
 * pointing to a valid block of memory. */
void build_fake_signal_context(struct ucontext *context,
                               x86_thread_state32_t *thread_state,
                               x86_float_state32_t *float_state) {
    pthread_sigmask(0, NULL, &context->uc_sigmask);
    context->uc_mcontext->ss = *thread_state;
    context->uc_mcontext->fs = *float_state;
}

/* This executes in the faulting thread as part of the signal
 * emulation.  It is effectively the inverse operation from above. */
void update_thread_state_from_context(x86_thread_state32_t *thread_state,
                                      x86_float_state32_t *float_state,
                                      struct ucontext *context) {
    *thread_state = context->uc_mcontext->ss;
    *float_state = context->uc_mcontext->fs;
    pthread_sigmask(SIG_SETMASK, &context->uc_sigmask, NULL);
}

/* Modify a context to push new data on its stack. */
void push_context(u32 data, x86_thread_state32_t *context)
{
    u32 *stack_pointer;

    stack_pointer = (u32*) context->esp;
    *(--stack_pointer) = data;
    context->esp = (unsigned int) stack_pointer;
}

void align_context_stack(x86_thread_state32_t *context)
{
    /* 16byte align the stack (provided that the stack is, as it
     * should be, 4byte aligned. */
    while (context->esp & 15) push_context(0, context);
}

/* Stack allocation starts with a context that has a mod-4 ESP value
 * and needs to leave a context with a mod-16 ESP that will restore
 * the old ESP value and other register state when activated.  The
 * first part of this is the recovery trampoline, which loads ESP from
 * EBP, pops EBP, and returns. */
asm("_stack_allocation_recover: movl %ebp, %esp; popl %ebp; ret;");

void open_stack_allocation(x86_thread_state32_t *context)
{
    void stack_allocation_recover(void);

    push_context(context->eip, context);
    push_context(context->ebp, context);
    context->ebp = context->esp;
    context->eip = (unsigned int) stack_allocation_recover;

    align_context_stack(context);
}

/* Stack allocation of data starts with a context with a mod-16 ESP
 * value and reserves some space on it by manipulating the ESP
 * register. */
void *stack_allocate(x86_thread_state32_t *context, size_t size)
{
    /* round up size to 16byte multiple */
    size = (size + 15) & -16;

    context->esp = ((u32)context->esp) - size;

    return (void *)context->esp;
}

/* Arranging to invoke a C function is tricky, as we have to assume
 * cdecl calling conventions (caller removes args) and x86/darwin
 * alignment requirements.  The simplest way to arrange this,
 * actually, is to open a new stack allocation.
 * WARNING!!! THIS DOES NOT PRESERVE REGISTERS! */
void call_c_function_in_context(x86_thread_state32_t *context,
                                void *function,
                                int nargs,
                                ...)
{
    va_list ap;
    int i;
    u32 *stack_pointer;

    /* Set up to restore stack on exit. */
    open_stack_allocation(context);

    /* Have to keep stack 16byte aligned on x86/darwin. */
    for (i = (3 & -nargs); i; i--) {
        push_context(0, context);
    }

    context->esp = ((u32)context->esp) - nargs * 4;
    stack_pointer = (u32 *)context->esp;

    va_start(ap, nargs);
    for (i = 0; i < nargs; i++) {
        //push_context(va_arg(ap, u32), context);
        stack_pointer[i] = va_arg(ap, u32);
    }
    va_end(ap);

    push_context(context->eip, context);
    context->eip = (unsigned int) function;
}

void signal_emulation_wrapper(x86_thread_state32_t *thread_state,
                              x86_float_state32_t *float_state,
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

    struct ucontext *context;
    struct mcontext *regs;

    context = (struct ucontext*) os_validate(0, sizeof(struct ucontext));
    regs = (struct mcontext*) os_validate(0, sizeof(struct mcontext));
    context->uc_mcontext = regs;

    /* when BSD signals are fired, they mask they signals in sa_mask
       which always seem to be the blockable_sigset, for us, so we
       need to:
       1) save the current sigmask
       2) block blockable signals
       3) call the signal handler
       4) restore the sigmask */

    build_fake_signal_context(context, thread_state, float_state);

    block_blockable_signals();

    handler(signal, siginfo, context);

    update_thread_state_from_context(thread_state, float_state, context);

    os_invalidate((os_vm_address_t)context, sizeof(struct ucontext));
    os_invalidate((os_vm_address_t)regs, sizeof(struct mcontext));

    /* Trap to restore the signal context. */
    asm volatile ("movl %0, %%eax; movl %1, %%ebx; .long 0xffff0b0f"
                  : : "r" (thread_state), "r" (float_state));
}

#if defined DUMP_CONTEXT
void dump_context(x86_thread_state32_t *context)
{
    int i;
    u32 *stack_pointer;

    printf("eax: %08lx  ecx: %08lx  edx: %08lx  ebx: %08lx\n",
           context->eax, context->ecx, context->edx, context->ebx);
    printf("esp: %08lx  ebp: %08lx  esi: %08lx  edi: %08lx\n",
           context->esp, context->ebp, context->esi, context->edi);
    printf("eip: %08lx  eflags: %08lx\n",
           context->eip, context->eflags);
    printf("cs: %04hx  ds: %04hx  es: %04hx  "
           "ss: %04hx  fs: %04hx  gs: %04hx\n",
           context->cs, context->ds, context->es,
           context->ss, context->fs, context->gs);

    stack_pointer = (u32 *)context->esp;
    for (i = 0; i < 48; i+=4) {
        printf("%08x:  %08x %08x %08x %08x\n",
               context->esp + (i * 4),
               stack_pointer[i],
               stack_pointer[i+1],
               stack_pointer[i+2],
               stack_pointer[i+3]);
    }
}
#endif

void
control_stack_exhausted_handler(int signal, siginfo_t *siginfo, void *void_context) {
    os_context_t *context = arch_os_get_context(&void_context);

    arrange_return_to_lisp_function
        (context, SymbolFunction(CONTROL_STACK_EXHAUSTED_ERROR));
}

void
undefined_alien_handler(int signal, siginfo_t *siginfo, void *void_context) {
    os_context_t *context = arch_os_get_context(&void_context);

    arrange_return_to_lisp_function
        (context, SymbolFunction(UNDEFINED_ALIEN_VARIABLE_ERROR));
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

    thread_mutex_lock(&mach_exception_lock);

    x86_thread_state32_t thread_state;
    mach_msg_type_number_t thread_state_count = x86_THREAD_STATE32_COUNT;

    x86_float_state32_t float_state;
    mach_msg_type_number_t float_state_count = x86_FLOAT_STATE32_COUNT;

    x86_exception_state32_t exception_state;
    mach_msg_type_number_t exception_state_count = x86_EXCEPTION_STATE32_COUNT;

    x86_thread_state32_t backup_thread_state;
    x86_thread_state32_t *target_thread_state;
    x86_float_state32_t *target_float_state;

    os_vm_address_t addr;

    struct thread *th = (struct thread*) exception_port;

    FSHOW((stderr,"/entering catch_exception_raise with exception: %d\n", exception));

    switch (exception) {

    case EXC_BAD_ACCESS:
        signal = SIGBUS;
        ret = thread_get_state(thread,
                               x86_THREAD_STATE32,
                               (thread_state_t)&thread_state,
                               &thread_state_count);
        ret = thread_get_state(thread,
                               x86_FLOAT_STATE32,
                               (thread_state_t)&float_state,
                               &float_state_count);
        ret = thread_get_state(thread,
                               x86_EXCEPTION_STATE32,
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
            protect_control_stack_guard_page_thread(0, th);
            protect_control_stack_return_guard_page_thread(1, th);

            backup_thread_state = thread_state;
            open_stack_allocation(&thread_state);

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
            protect_control_stack_guard_page_thread(1, th);
            protect_control_stack_return_guard_page_thread(0, th);
        }
        else if (addr >= undefined_alien_address &&
                 addr < undefined_alien_address + os_vm_page_size) {
            backup_thread_state = thread_state;
            open_stack_allocation(&thread_state);

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
                               x86_THREAD_STATE32,
                               (thread_state_t)&thread_state,
                               thread_state_count);

        ret = thread_set_state(thread,
                               x86_FLOAT_STATE32,
                               (thread_state_t)&float_state,
                               float_state_count);
        thread_mutex_unlock(&mach_exception_lock);
        return KERN_SUCCESS;

    case EXC_BAD_INSTRUCTION:

        ret = thread_get_state(thread,
                               x86_THREAD_STATE32,
                               (thread_state_t)&thread_state,
                               &thread_state_count);
        ret = thread_get_state(thread,
                               x86_FLOAT_STATE32,
                               (thread_state_t)&float_state,
                               &float_state_count);
        ret = thread_get_state(thread,
                               x86_EXCEPTION_STATE32,
                               (thread_state_t)&exception_state,
                               &exception_state_count);
        if (0xffff0b0f == *((u32 *)thread_state.eip)) {
            /* fake sigreturn. */

            /* When we get here, thread_state.eax is a pointer to a
             * thread_state to restore. */
            /* thread_state = *((thread_state_t *)thread_state.eax); */

            ret = thread_set_state(thread,
                                   x86_THREAD_STATE32,
                                   (thread_state_t) thread_state.eax,
                                   /* &thread_state, */
                                   thread_state_count);

            ret = thread_set_state(thread,
                                   x86_FLOAT_STATE32,
                                   (thread_state_t) thread_state.ebx,
                                   /* &thread_state, */
                                   float_state_count);
        } else {

            backup_thread_state = thread_state;
            open_stack_allocation(&thread_state);

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
            if (*((unsigned short *)target_thread_state->eip) == 0x0b0f) {
                signal = SIGTRAP;
                siginfo->si_signo = signal;
                siginfo->si_addr = (void*)exception_state.faultvaddr;
                target_thread_state->eip += 2;
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
                                   x86_THREAD_STATE32,
                                   (thread_state_t)&thread_state,
                                   thread_state_count);
            ret = thread_set_state(thread,
                                   x86_FLOAT_STATE32,
                                   (thread_state_t)&float_state,
                                   float_state_count);
        }
        thread_mutex_unlock(&mach_exception_lock);
        return KERN_SUCCESS;

    default:
        thread_mutex_unlock(&mach_exception_lock);
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
  abort();
}

#endif

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER

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
