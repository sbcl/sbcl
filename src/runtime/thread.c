/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * This software is derived from the CMU CL system, which was
 * written at Carnegie Mellon University and released into the
 * public domain. The software is in the public domain and is
 * provided with absolutely no warranty. See the COPYING and CREDITS
 * files for more information.
 */

#include "sbcl.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifndef LISP_FEATURE_WIN32
#include <sched.h>
#endif
#include <stddef.h>
#include <errno.h>
#include <sys/types.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/wait.h>
#endif

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/mach_types.h>
#endif

#include "runtime.h"
#include "validate.h"           /* for BINDING_STACK_SIZE etc */
#include "thread.h"
#include "arch.h"
#include "target-arch-os.h"
#include "os.h"
#include "globals.h"
#include "dynbind.h"
#include "genesis/cons.h"
#include "genesis/fdefn.h"
#include "interr.h"             /* for lose() */
#include "alloc.h"
#include "gc-internal.h"
#include "cpputil.h"
#include "pseudo-atomic.h"
#include "interrupt.h"
#include "lispregs.h"

#ifdef LISP_FEATURE_SB_THREAD

#ifdef LISP_FEATURE_OPENBSD
#include <pthread_np.h>
#endif

#ifdef LISP_FEATURE_SUNOS
#include <thread.h>
#endif

#ifdef LISP_FEATURE_WIN32
# define IMMEDIATE_POST_MORTEM
#endif

#ifdef LISP_FEATURE_DARWIN
#define DELAY_THREAD_POST_MORTEM 5
#define LOCK_CREATE_THREAD
#endif

#endif

#if defined(LISP_FEATURE_FREEBSD) || defined(LISP_FEATURE_DRAGONFLY)
#define LOCK_CREATE_THREAD
#endif

#ifdef LISP_FEATURE_SB_THREAD
struct thread_post_mortem {
#ifdef DELAY_THREAD_POST_MORTEM
    struct thread_post_mortem *next;
#endif
    os_thread_t os_thread;
    pthread_attr_t *os_attr;
    os_vm_address_t os_address;
};

#ifdef DELAY_THREAD_POST_MORTEM
static int pending_thread_post_mortem_count = 0;
pthread_mutex_t thread_post_mortem_lock = PTHREAD_MUTEX_INITIALIZER;
#endif
static struct thread_post_mortem * volatile pending_thread_post_mortem = 0;
#endif

int dynamic_values_bytes=TLS_SIZE*sizeof(lispobj);  /* same for all threads */
struct thread *all_threads;
extern struct interrupt_data * global_interrupt_data;

#ifdef LISP_FEATURE_SB_THREAD
pthread_mutex_t all_threads_lock = PTHREAD_MUTEX_INITIALIZER;
#ifdef LOCK_CREATE_THREAD
static pthread_mutex_t create_thread_lock = PTHREAD_MUTEX_INITIALIZER;
#endif
#ifdef LISP_FEATURE_GCC_TLS
__thread struct thread *current_thread;
#endif
pthread_key_t lisp_thread = 0;
#endif

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
extern lispobj call_into_lisp_first_time(lispobj fun, lispobj *args, int nargs)
# ifdef LISP_FEATURE_X86_64
    __attribute__((sysv_abi))
# endif
    ;
#endif

static void
link_thread(struct thread *th)
{
    if (all_threads) all_threads->prev=th;
    th->next=all_threads;
    th->prev=0;
    all_threads=th;
}

#ifdef LISP_FEATURE_SB_THREAD
static void
unlink_thread(struct thread *th)
{
    if (th->prev)
        th->prev->next = th->next;
    else
        all_threads = th->next;
    if (th->next)
        th->next->prev = th->prev;
}

#ifndef LISP_FEATURE_SB_SAFEPOINT
/* Only access thread state with blockables blocked. */
lispobj
thread_state(struct thread *thread)
{
    lispobj state;
    sigset_t old;
    block_blockable_signals(NULL, &old);
    os_sem_wait(thread->state_sem, "thread_state");
    state = thread->state;
    os_sem_post(thread->state_sem, "thread_state");
    thread_sigmask(SIG_SETMASK, &old, NULL);
    return state;
}

void
set_thread_state(struct thread *thread, lispobj state)
{
    int i, waitcount = 0;
    sigset_t old;
    block_blockable_signals(NULL, &old);
    os_sem_wait(thread->state_sem, "set_thread_state");
    if (thread->state != state) {
        if ((STATE_STOPPED==state) ||
            (STATE_DEAD==state)) {
            waitcount = thread->state_not_running_waitcount;
            thread->state_not_running_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(thread->state_not_running_sem, "set_thread_state (not running)");
        }
        if ((STATE_RUNNING==state) ||
            (STATE_DEAD==state)) {
            waitcount = thread->state_not_stopped_waitcount;
            thread->state_not_stopped_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(thread->state_not_stopped_sem, "set_thread_state (not stopped)");
        }
        thread->state = state;
    }
    os_sem_post(thread->state_sem, "set_thread_state");
    thread_sigmask(SIG_SETMASK, &old, NULL);
}

void
wait_for_thread_state_change(struct thread *thread, lispobj state)
{
    sigset_t old;
    os_sem_t *wait_sem;
    block_blockable_signals(NULL, &old);
  start:
    os_sem_wait(thread->state_sem, "wait_for_thread_state_change");
    if (thread->state == state) {
        switch (state) {
        case STATE_RUNNING:
            wait_sem = thread->state_not_running_sem;
            thread->state_not_running_waitcount++;
            break;
        case STATE_STOPPED:
            wait_sem = thread->state_not_stopped_sem;
            thread->state_not_stopped_waitcount++;
            break;
        default:
            lose("Invalid state in wait_for_thread_state_change: "OBJ_FMTX"\n", state);
        }
    } else {
        wait_sem = NULL;
    }
    os_sem_post(thread->state_sem, "wait_for_thread_state_change");
    if (wait_sem) {
        os_sem_wait(wait_sem, "wait_for_thread_state_change");
        goto start;
    }
    thread_sigmask(SIG_SETMASK, &old, NULL);
}
#endif /* sb-safepoint */
#endif /* sb-thread */

static int
initial_thread_trampoline(struct thread *th)
{
    lispobj function;
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    lispobj *args = NULL;
#endif
#ifdef LISP_FEATURE_SB_THREAD
    pthread_setspecific(lisp_thread, (void *)1);
#endif
#if defined(THREADS_USING_GCSIGNAL) && (defined(LISP_FEATURE_PPC) || defined(LISP_FEATURE_ARM64))
    /* SIG_STOP_FOR_GC defaults to blocked on PPC? */
    unblock_gc_signals(0,0);
#endif
    function = th->no_tls_value_marker;
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER_WIDETAG;
    if(arch_os_thread_init(th)==0) return 1;
    link_thread(th);
    th->os_thread=thread_self();
#ifndef LISP_FEATURE_WIN32
    protect_control_stack_hard_guard_page(1, NULL);
#endif
    protect_binding_stack_hard_guard_page(1, NULL);
    protect_alien_stack_hard_guard_page(1, NULL);
#ifndef LISP_FEATURE_WIN32
    protect_control_stack_guard_page(1, NULL);
#endif
    protect_binding_stack_guard_page(1, NULL);
    protect_alien_stack_guard_page(1, NULL);

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    return call_into_lisp_first_time(function,args,0);
#else
    return funcall0(function);
#endif
}

#ifdef LISP_FEATURE_SB_THREAD

# if defined(IMMEDIATE_POST_MORTEM)

/*
 * If this feature is set, we are running on a stack managed by the OS,
 * and no fancy delays are required for anything.  Just do it.
 */
static void
schedule_thread_post_mortem(struct thread *corpse)
{
    pthread_detach(pthread_self());
    gc_assert(!pthread_attr_destroy(corpse->os_attr));
    free(corpse->os_attr);
#if defined(LISP_FEATURE_WIN32)
    os_invalidate_free(corpse->os_address, THREAD_STRUCT_SIZE);
#else
    os_invalidate(corpse->os_address, THREAD_STRUCT_SIZE);
#endif
}

# else

/* THREAD POST MORTEM CLEANUP
 *
 * Memory allocated for the thread stacks cannot be reclaimed while
 * the thread is still alive, so we need a mechanism for post mortem
 * cleanups. FIXME: We actually have three, for historical reasons as
 * the saying goes. Do we really need three? Nikodemus guesses that
 * not anymore, now that we properly call pthread_attr_destroy before
 * freeing the stack. */

static struct thread_post_mortem *
plan_thread_post_mortem(struct thread *corpse)
{
    if (corpse) {
        struct thread_post_mortem *post_mortem = malloc(sizeof(struct thread_post_mortem));
        gc_assert(post_mortem);
        post_mortem->os_thread = corpse->os_thread;
        post_mortem->os_attr = corpse->os_attr;
        post_mortem->os_address = corpse->os_address;
#ifdef DELAY_THREAD_POST_MORTEM
        post_mortem->next = NULL;
#endif
        return post_mortem;
    } else {
        /* FIXME: When does this happen? */
        return NULL;
    }
}

static void
perform_thread_post_mortem(struct thread_post_mortem *post_mortem)
{
#ifdef CREATE_POST_MORTEM_THREAD
    pthread_detach(pthread_self());
#endif
    if (post_mortem) {
        gc_assert(!pthread_join(post_mortem->os_thread, NULL));
        gc_assert(!pthread_attr_destroy(post_mortem->os_attr));
        free(post_mortem->os_attr);
        os_invalidate(post_mortem->os_address, THREAD_STRUCT_SIZE);
        free(post_mortem);
    }
}

static void
schedule_thread_post_mortem(struct thread *corpse)
{
    struct thread_post_mortem *post_mortem = NULL;
    if (corpse) {
        post_mortem = plan_thread_post_mortem(corpse);

#ifdef DELAY_THREAD_POST_MORTEM
        pthread_mutex_lock(&thread_post_mortem_lock);
        /* First stick the new post mortem to the end of the queue. */
        if (pending_thread_post_mortem) {
            struct thread_post_mortem *next = pending_thread_post_mortem;
            while (next->next) {
                next = next->next;
            }
            next->next = post_mortem;
        } else {
            pending_thread_post_mortem = post_mortem;
        }
        /* Then, if there are enough things in the queue, clean up one
         * from the head -- or increment the count, and null out the
         * post_mortem we have. */
        if (pending_thread_post_mortem_count > DELAY_THREAD_POST_MORTEM) {
            post_mortem = pending_thread_post_mortem;
            pending_thread_post_mortem = post_mortem->next;
        } else {
            pending_thread_post_mortem_count++;
            post_mortem = NULL;
        }
        pthread_mutex_unlock(&thread_post_mortem_lock);
        /* Finally run, the cleanup, if any. */
        perform_thread_post_mortem(post_mortem);
#elif defined(CREATE_POST_MORTEM_THREAD)
        gc_assert(!pthread_create(&thread, NULL, perform_thread_post_mortem, post_mortem));
#else
        post_mortem = (struct thread_post_mortem *)
            swap_lispobjs((lispobj *)(void *)&pending_thread_post_mortem,
                          (lispobj)post_mortem);
        perform_thread_post_mortem(post_mortem);
#endif
    }
}

# endif /* !IMMEDIATE_POST_MORTEM */

/* Note: scribble must be stack-allocated */
static void
init_new_thread(struct thread *th, init_thread_data *scribble, int guardp)
{
    int lock_ret;

    pthread_setspecific(lisp_thread, (void *)1);
    if(arch_os_thread_init(th)==0) {
        /* FIXME: handle error */
        lose("arch_os_thread_init failed\n");
    }

    th->os_thread=thread_self();
    if (guardp)
        protect_control_stack_guard_page(1, NULL);
    protect_binding_stack_guard_page(1, NULL);
    protect_alien_stack_guard_page(1, NULL);
    /* Since GC can only know about this thread from the all_threads
     * list and we're just adding this thread to it, there is no
     * danger of deadlocking even with SIG_STOP_FOR_GC blocked (which
     * it is not). */
#ifdef LISP_FEATURE_SB_SAFEPOINT
    *th->csp_around_foreign_call = (lispobj)scribble;
#endif
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);
    link_thread(th);
    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    /* Kludge: Changed the order of some steps between the safepoint/
     * non-safepoint versions of this code.  Can we unify this more?
     */
#ifdef LISP_FEATURE_SB_SAFEPOINT
    gc_state_lock();
    gc_state_wait(GC_NONE);
    gc_state_unlock();
    push_gcing_safety(&scribble->safety);
#endif
}

static void
undo_init_new_thread(struct thread *th, init_thread_data *scribble)
{
    int lock_ret;

    /* Kludge: Changed the order of some steps between the safepoint/
     * non-safepoint versions of this code.  Can we unify this more?
     */
#ifdef LISP_FEATURE_SB_SAFEPOINT
    block_blockable_signals(0, 0);
    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->alloc_region);
#if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)
    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->sprof_alloc_region);
#endif
    pop_gcing_safety(&scribble->safety);
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);
    unlink_thread(th);
    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);
#else
    /* Block GC */
    block_blockable_signals(0, 0);
    set_thread_state(th, STATE_DEAD);

    /* SIG_STOP_FOR_GC is blocked and GC might be waiting for this
     * thread, but since we are already dead it won't wait long. */
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->alloc_region);
#if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)
    gc_alloc_update_page_tables(BOXED_PAGE_FLAG, &th->sprof_alloc_region);
#endif
    unlink_thread(th);
    pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);
#endif

    arch_os_thread_cleanup(th);

#ifndef LISP_FEATURE_SB_SAFEPOINT
    os_sem_destroy(th->state_sem);
    os_sem_destroy(th->state_not_running_sem);
    os_sem_destroy(th->state_not_stopped_sem);
#endif

#if defined(LISP_FEATURE_WIN32)
    free((os_vm_address_t)th->interrupt_data);
#else
    os_invalidate((os_vm_address_t)th->interrupt_data,
                  (sizeof (struct interrupt_data)));
#endif

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
    mach_lisp_thread_destroy(th);
#endif

#if defined(LISP_FEATURE_WIN32)
    int i;
    for (i = 0; i<
             (int) (sizeof(th->private_events.events)/
                    sizeof(th->private_events.events[0])); ++i) {
      CloseHandle(th->private_events.events[i]);
    }
    TlsSetValue(OUR_TLS_INDEX,NULL);
#endif

    /* Undo the association of the current pthread to its `struct thread',
     * such that we can call arch_os_get_current_thread() later in this
     * thread and cleanly get back NULL. */
#ifdef LISP_FEATURE_GCC_TLS
    current_thread = NULL;
#else
    pthread_setspecific(specials, NULL);
#endif

    schedule_thread_post_mortem(th);
}

/* this is the first thing that runs in the child (which is why the
 * silly calling convention).  Basically it calls the user's requested
 * lisp function after doing arch_os_thread_init and whatever other
 * bookkeeping needs to be done
 */
int
new_thread_trampoline(struct thread *th)
{
    int result;
    init_thread_data scribble;

    FSHOW((stderr,"/creating thread %lu\n", thread_self()));
    check_deferrables_blocked_or_lose(0);
#ifndef LISP_FEATURE_SB_SAFEPOINT
    check_gc_signals_unblocked_or_lose(0);
#endif

    lispobj function = th->no_tls_value_marker;
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER_WIDETAG;
    init_new_thread(th, &scribble, 1);
    result = funcall0(function);
    undo_init_new_thread(th, &scribble);

    FSHOW((stderr,"/exiting thread %lu\n", thread_self()));
    return result;
}

static struct thread *create_thread_struct(lispobj);

void
attach_os_thread(init_thread_data *scribble)
{
    os_thread_t os = pthread_self();
    odxprint(misc, "attach_os_thread: attaching to %p", os);

    struct thread *th = create_thread_struct(NIL);
    block_deferrable_signals(0, &scribble->oldset);
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER_WIDETAG;
    /* We don't actually want a pthread_attr here, but rather than add
     * `if's to the post-mostem, let's just keep that code happy by
     * keeping it initialized: */
    pthread_attr_init(th->os_attr);

#ifndef LISP_FEATURE_WIN32
    /* On windows, arch_os_thread_init will take care of finding the
     * stack. */
    void *stack_addr;
    size_t stack_size;
#ifdef LISP_FEATURE_OPENBSD
    stack_t stack;
    pthread_stackseg_np(os, &stack);
    stack_size = stack.ss_size;
    stack_addr = (void*)((size_t)stack.ss_sp - stack_size);
#elif defined LISP_FEATURE_SUNOS
  stack_t stack;
  thr_stksegment(&stack);
  stack_size = stack.ss_size;
  stack_addr = (void*)((size_t)stack.ss_sp - stack_size);
#elif defined(LISP_FEATURE_DARWIN)
    stack_addr = pthread_get_stackaddr_np(os);
    stack_size = pthread_get_stacksize_np(os);
#else
    pthread_attr_t attr;
#ifdef LISP_FEATURE_FREEBSD
    pthread_attr_get_np(os, &attr);
#else
    int pthread_getattr_np(pthread_t, pthread_attr_t *);
    pthread_getattr_np(os, &attr);
#endif
    pthread_attr_getstack(&attr, &stack_addr, &stack_size);
#endif

    th->control_stack_start = stack_addr;
    th->control_stack_end = (void *) (((uintptr_t) stack_addr) + stack_size);
#endif

    init_new_thread(th, scribble, 0);

    /* We will be calling into Lisp soon, and the functions being called
     * recklessly ignore the comment in target-thread which says that we
     * must be careful to not cause GC while initializing a new thread.
     * Since we first need to create a fresh thread object, it's really
     * tempting to just perform such unsafe allocation though.  So let's
     * at least try to suppress GC before consing, and hope that it
     * works: */
    bind_variable(GC_INHIBIT, T, th);

    uword_t stacksize
        = (uword_t) th->control_stack_end - (uword_t) th->control_stack_start;
    odxprint(misc, "attach_os_thread: attached %p as %p (0x%lx bytes stack)",
             os, th, (long) stacksize);
}

void
detach_os_thread(init_thread_data *scribble)
{
    struct thread *th = arch_os_get_current_thread();
    odxprint(misc, "detach_os_thread: detaching");

    undo_init_new_thread(th, scribble);

    odxprint(misc, "deattach_os_thread: detached");
    pthread_setspecific(lisp_thread, (void *)0);
    thread_sigmask(SIG_SETMASK, &scribble->oldset, 0);
}

void
callback_wrapper_trampoline(
#if !(defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
    /* On the x86oid backends, the assembly wrapper happens to not pass
     * in ENTER_ALIEN_CALLBACK explicitly for safepoints.  However, the
     * platforms with precise GC are tricky enough already, and I want
     * to minimize the read-time conditionals.  For those platforms, I'm
     * only replacing funcall3 with callback_wrapper_trampoline while
     * keeping the arguments unchanged. --DFL */
    lispobj __attribute__((__unused__)) fun,
#endif
    lispobj arg0, lispobj arg1, lispobj arg2)
{
#if defined(LISP_FEATURE_WIN32)
    pthread_np_notice_thread();
#endif
    struct thread* th = arch_os_get_current_thread();
    if (!th) {                  /* callback invoked in non-lisp thread */
        init_thread_data scribble;
        attach_os_thread(&scribble);
        funcall3(StaticSymbolFunction(ENTER_FOREIGN_CALLBACK), arg0,arg1,arg2);
        detach_os_thread(&scribble);
        return;
    }

#ifdef LISP_FEATURE_WIN32
    /* arg2 is the pointer to a return value, which sits on the stack */
    th->carried_base_pointer = (os_context_register_t) *(((void**)arg2)-1);
#endif

#ifdef LISP_FEATURE_SB_SAFEPOINT
    WITH_GC_AT_SAFEPOINTS_ONLY()
#endif
    {
       funcall3(SymbolValue(ENTER_ALIEN_CALLBACK, 0), arg0, arg1, arg2);

    }
}
#endif /* LISP_FEATURE_SB_THREAD */

static void
free_thread_struct(struct thread *th)
{
#if defined(LISP_FEATURE_WIN32)
    if (th->interrupt_data) {
        os_invalidate_free((os_vm_address_t) th->interrupt_data,
                      (sizeof (struct interrupt_data)));
    }
    os_invalidate_free((os_vm_address_t) th->os_address,
                  THREAD_STRUCT_SIZE);
#else
    if (th->interrupt_data)
        os_invalidate((os_vm_address_t) th->interrupt_data,
                      (sizeof (struct interrupt_data)));
    os_invalidate((os_vm_address_t) th->os_address,
                  THREAD_STRUCT_SIZE);
#endif
}

#ifdef LISP_FEATURE_SB_THREAD
/* FIXME: should be MAX_INTERRUPTS -1 ? */
const unsigned int tls_index_start =
  MAX_INTERRUPTS + sizeof(struct thread)/sizeof(lispobj);
#endif

/* this is called from any other thread to create the new one, and
 * initialize all parts of it that can be initialized from another
 * thread
 */

static struct thread *
create_thread_struct(lispobj initial_function) {
    union per_thread_data *per_thread;
    struct thread *th=0;        /*  subdue gcc */
    void *spaces=0;
    void *aligned_spaces=0;
#if defined(LISP_FEATURE_SB_THREAD) || defined(LISP_FEATURE_WIN32)
    unsigned int i;
#endif

    /* May as well allocate all the spaces at once: it saves us from
     * having to decide what to do if only some of the allocations
     * succeed. SPACES must be appropriately aligned, since the GC
     * expects the control stack to start at a page boundary -- and
     * the OS may have even more rigorous requirements. We can't rely
     * on the alignment passed from os_validate, since that might
     * assume the current (e.g. 4k) pagesize, while we calculate with
     * the biggest (e.g. 64k) pagesize allowed by the ABI. */
    spaces=os_validate(0, THREAD_STRUCT_SIZE);
    if(!spaces)
        return NULL;
    /* Aligning up is safe as THREAD_STRUCT_SIZE has
     * THREAD_ALIGNMENT_BYTES padding. */
    aligned_spaces = (void *)((((uword_t)(char *)spaces)
                               + THREAD_ALIGNMENT_BYTES-1)
                              &~(uword_t)(THREAD_ALIGNMENT_BYTES-1));
    void* csp_page=
        (aligned_spaces+
         thread_control_stack_size+
         BINDING_STACK_SIZE+
         ALIEN_STACK_SIZE);
    per_thread=(union per_thread_data *)
        (csp_page + THREAD_CSP_PAGE_SIZE);

#ifdef LISP_FEATURE_SB_THREAD
    for(i = 0; i < (dynamic_values_bytes / sizeof(lispobj)); i++)
        per_thread->dynamic_values[i] = NO_TLS_VALUE_MARKER_WIDETAG;
#endif

    th=&per_thread->thread;
    th->os_address = spaces;
    th->control_stack_start = aligned_spaces;
    th->binding_stack_start=
        (lispobj*)((void*)th->control_stack_start+thread_control_stack_size);
    th->control_stack_end = th->binding_stack_start;
    th->control_stack_guard_page_protected = T;
    th->alien_stack_start=
        (lispobj*)((void*)th->binding_stack_start+BINDING_STACK_SIZE);
    set_binding_stack_pointer(th,th->binding_stack_start);
    th->this=th;
    th->os_thread=0;

#ifdef LISP_FEATURE_SB_SAFEPOINT
# ifdef LISP_FEATURE_WIN32
    th->carried_base_pointer = 0;
# endif
# ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    th->pc_around_foreign_call = 0;
# endif
    th->csp_around_foreign_call = csp_page;
#endif

#ifdef LISP_FEATURE_SB_THREAD
    /* Contrary to the "allocate all the spaces at once" comment above,
     * the os_attr is allocated separately.  We cannot put it into the
     * nonpointer data, because it's used for post_mortem and freed
     * separately */
    th->os_attr=malloc(sizeof(pthread_attr_t));
# ifndef LISP_FEATURE_SB_SAFEPOINT
    struct nonpointer_thread_data *nonpointer_data
      = (void *) &per_thread->dynamic_values[TLS_SIZE];

    th->state_sem=&nonpointer_data->state_sem;
    th->state_not_running_sem=&nonpointer_data->state_not_running_sem;
    th->state_not_stopped_sem=&nonpointer_data->state_not_stopped_sem;
    os_sem_init(th->state_sem, 1);
    os_sem_init(th->state_not_running_sem, 0);
    os_sem_init(th->state_not_stopped_sem, 0);
    th->state_not_running_waitcount = 0;
    th->state_not_stopped_waitcount = 0;
# endif

#endif
    th->state=STATE_RUNNING;
#ifdef ALIEN_STACK_GROWS_DOWNWARD
    th->alien_stack_pointer=((void *)th->alien_stack_start
                             + ALIEN_STACK_SIZE-N_WORD_BYTES);
#else
    th->alien_stack_pointer=((void *)th->alien_stack_start);
#endif

#ifdef LISP_FEATURE_SB_THREAD
    th->pseudo_atomic_bits=0;
#elif defined LISP_FEATURE_GENCGC
    clear_pseudo_atomic_atomic(th);
    clear_pseudo_atomic_interrupted(th);
#endif

#ifdef LISP_FEATURE_GENCGC
    gc_set_region_empty(&th->alloc_region);
# if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)
    gc_set_region_empty(&th->sprof_alloc_region);
# endif
#endif
#ifdef LISP_FEATURE_SB_THREAD
    /* This parallels the same logic in globals.c for the
     * single-threaded foreign_function_call_active, KLUDGE and
     * all. */
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    th->foreign_function_call_active = 0;
#else
    th->foreign_function_call_active = 1;
#endif
#endif

#ifndef LISP_FEATURE_SB_THREAD
    /* the tls-points-into-struct-thread trick is only good for threaded
     * sbcl, because unithread sbcl doesn't have tls.  So, we copy the
     * appropriate values from struct thread here, and make sure that
     * we use the appropriate SymbolValue macros to access any of the
     * variable quantities from the C runtime.  It's not quite OAOOM,
     * it just feels like it */
    SetSymbolValue(BINDING_STACK_START,(lispobj)th->binding_stack_start,th);
    SetSymbolValue(CONTROL_STACK_START,(lispobj)th->control_stack_start,th);
    SetSymbolValue(CONTROL_STACK_END,(lispobj)th->control_stack_end,th);
#if defined(LISP_FEATURE_X86) || defined (LISP_FEATURE_X86_64)
    SetSymbolValue(ALIEN_STACK_POINTER,(lispobj)th->alien_stack_pointer,th);
#endif
#endif
    bind_variable(CURRENT_CATCH_BLOCK,make_fixnum(0),th);
    bind_variable(CURRENT_UNWIND_PROTECT_BLOCK,make_fixnum(0),th);
    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX,make_fixnum(0),th);
    bind_variable(INTERRUPT_PENDING, NIL,th);
    bind_variable(INTERRUPTS_ENABLED,T,th);
    bind_variable(ALLOW_WITH_INTERRUPTS,T,th);
    bind_variable(GC_PENDING,NIL,th);
    bind_variable(ALLOC_SIGNAL,NIL,th);
#ifdef PINNED_OBJECTS
    bind_variable(PINNED_OBJECTS,NIL,th);
#endif
#ifdef LISP_FEATURE_SB_THREAD
    bind_variable(STOP_FOR_GC_PENDING,NIL,th);
#endif
#if defined(LISP_FEATURE_SB_SAFEPOINT)
    bind_variable(GC_SAFE,NIL,th);
    bind_variable(IN_SAFEPOINT,NIL,th);
#endif
#ifdef LISP_FEATURE_SB_THRUPTION
    bind_variable(THRUPTION_PENDING,NIL,th);
    bind_variable(RESTART_CLUSTERS,NIL,th);
#endif
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    access_control_stack_pointer(th)=th->control_stack_start;
#endif

#if defined(LISP_FEATURE_WIN32)
    th->interrupt_data = (struct interrupt_data *)
        calloc((sizeof (struct interrupt_data)),1);
#else
    th->interrupt_data = (struct interrupt_data *)
        os_validate(0,(sizeof (struct interrupt_data)));
#endif
    if (!th->interrupt_data) {
        free_thread_struct(th);
        return 0;
    }
    th->interrupt_data->pending_handler = 0;
    th->interrupt_data->gc_blocked_deferrables = 0;
#ifdef GENCGC_IS_PRECISE
    th->interrupt_data->allocation_trap_context = 0;
#endif
    th->no_tls_value_marker=initial_function;

#if defined(LISP_FEATURE_WIN32)
    for (i = 0; i<sizeof(th->private_events.events)/
           sizeof(th->private_events.events[0]); ++i) {
      th->private_events.events[i] = CreateEvent(NULL,FALSE,FALSE,NULL);
    }
    th->synchronous_io_handle_and_flag = 0;
#endif
    th->stepping = 0;
    return th;
}

void create_initial_thread(lispobj initial_function) {
    struct thread *th=create_thread_struct(initial_function);
#ifdef LISP_FEATURE_SB_THREAD
    pthread_key_create(&lisp_thread, 0);
#endif
    if(th) {
        initial_thread_trampoline(th); /* no return */
    } else lose("can't create initial thread\n");
}

#ifdef LISP_FEATURE_SB_THREAD

#ifndef __USE_XOPEN2K
extern int pthread_attr_setstack (pthread_attr_t *__attr, void *__stackaddr,
                                  size_t __stacksize);
#endif

boolean create_os_thread(struct thread *th,os_thread_t *kid_tid)
{
    /* The new thread inherits the restrictive signal mask set here,
     * and enables signals again when it is set up properly. */
    sigset_t oldset;
    boolean r=1;
    int retcode = 0, initcode;

    FSHOW_SIGNAL((stderr,"/create_os_thread: creating new thread\n"));

    /* Blocking deferrable signals is enough, no need to block
     * SIG_STOP_FOR_GC because the child process is not linked onto
     * all_threads until it's ready. */
    block_deferrable_signals(0, &oldset);

#ifdef LOCK_CREATE_THREAD
    retcode = pthread_mutex_lock(&create_thread_lock);
    gc_assert(retcode == 0);
    FSHOW_SIGNAL((stderr,"/create_os_thread: got lock\n"));
#endif

    if((initcode = pthread_attr_init(th->os_attr)) ||
       /* call_into_lisp_first_time switches the stack for the initial
        * thread. For the others, we use this. */
#if defined(LISP_FEATURE_WIN32)
       (pthread_attr_setstacksize(th->os_attr, thread_control_stack_size)) ||
#else
# if defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
       (pthread_attr_setstack(th->os_attr,th->control_stack_start,
                              thread_control_stack_size)) ||
# else
       (pthread_attr_setstack(th->os_attr,th->alien_stack_start,
                              ALIEN_STACK_SIZE)) ||
# endif
#endif
       (retcode = pthread_create
        (kid_tid,th->os_attr,(void *(*)(void *))new_thread_trampoline,th))) {
        FSHOW_SIGNAL((stderr, "init = %d\n", initcode));
        FSHOW_SIGNAL((stderr, "pthread_create returned %d, errno %d\n",
                      retcode, errno));
        if(retcode < 0) {
            perror("create_os_thread");
        }
        r=0;
    }

#ifdef LOCK_CREATE_THREAD
    retcode = pthread_mutex_unlock(&create_thread_lock);
    gc_assert(retcode == 0);
    FSHOW_SIGNAL((stderr,"/create_os_thread: released lock\n"));
#endif
    thread_sigmask(SIG_SETMASK,&oldset,0);
    return r;
}

os_thread_t create_thread(lispobj initial_function) {
    struct thread *th, *thread = arch_os_get_current_thread();
    os_thread_t kid_tid = 0;

    /* Must defend against async unwinds. */
    if (SymbolValue(INTERRUPTS_ENABLED, thread) != NIL)
        lose("create_thread is not safe when interrupts are enabled.\n");

    /* Assuming that a fresh thread struct has no lisp objects in it,
     * linking it to all_threads can be left to the thread itself
     * without fear of gc lossage. initial_function violates this
     * assumption and must stay pinned until the child starts up. */
    th = create_thread_struct(initial_function);
    if (th && !create_os_thread(th,&kid_tid)) {
        free_thread_struct(th);
        kid_tid = 0;
    }
    return kid_tid;
}

/* stopping the world is a two-stage process.  From this thread we signal
 * all the others with SIG_STOP_FOR_GC.  The handler for this signal does
 * the usual pseudo-atomic checks (we don't want to stop a thread while
 * it's in the middle of allocation) then waits for another SIG_STOP_FOR_GC.
 */
/*
 * (With SB-SAFEPOINT, see the definitions in safepoint.c instead.)
 */
#ifndef LISP_FEATURE_SB_SAFEPOINT

/* To avoid deadlocks when gc stops the world all clients of each
 * mutex must enable or disable SIG_STOP_FOR_GC for the duration of
 * holding the lock, but they must agree on which. */
void gc_stop_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    int status, lock_ret;
#ifdef LOCK_CREATE_THREAD
    /* KLUDGE: Stopping the thread during pthread_create() causes deadlock
     * on FreeBSD. */
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:waiting on create_thread_lock\n"));
    lock_ret = pthread_mutex_lock(&create_thread_lock);
    gc_assert(lock_ret == 0);
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:got create_thread_lock\n"));
#endif
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:waiting on lock\n"));
    /* keep threads from starting while the world is stopped. */
    lock_ret = pthread_mutex_lock(&all_threads_lock);      \
    gc_assert(lock_ret == 0);

    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:got lock\n"));
    /* stop all other threads by sending them SIG_STOP_FOR_GC */
    for(p=all_threads; p; p=p->next) {
        gc_assert(p->os_thread != 0);
        FSHOW_SIGNAL((stderr,"/gc_stop_the_world: thread=%lu, state=%x\n",
                      p->os_thread, thread_state(p)));
        if((p!=th) && ((thread_state(p)==STATE_RUNNING))) {
            FSHOW_SIGNAL((stderr,"/gc_stop_the_world: suspending thread %lu\n",
                          p->os_thread));
            /* We already hold all_thread_lock, P can become DEAD but
             * cannot exit, ergo it's safe to use pthread_kill. */
            status=pthread_kill(p->os_thread,SIG_STOP_FOR_GC);
            if (status==ESRCH) {
                /* This thread has exited. */
                gc_assert(thread_state(p)==STATE_DEAD);
            } else if (status) {
                lose("cannot send suspend thread=%lu: %d, %s\n",
                     p->os_thread,status,strerror(status));
            }
        }
    }
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:signals sent\n"));
    for(p=all_threads;p;p=p->next) {
        if (p!=th) {
            FSHOW_SIGNAL
                ((stderr,
                  "/gc_stop_the_world: waiting for thread=%lu: state=%x\n",
                  p->os_thread, thread_state(p)));
            wait_for_thread_state_change(p, STATE_RUNNING);
            if (p->state == STATE_RUNNING)
                lose("/gc_stop_the_world: unexpected state");
        }
    }
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:end\n"));
}

void gc_start_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    int lock_ret;
    /* if a resumed thread creates a new thread before we're done with
     * this loop, the new thread will get consed on the front of
     * all_threads, but it won't have been stopped so won't need
     * restarting */
    FSHOW_SIGNAL((stderr,"/gc_start_the_world:begin\n"));
    for(p=all_threads;p;p=p->next) {
        gc_assert(p->os_thread!=0);
        if (p!=th) {
            lispobj state = thread_state(p);
            if (state != STATE_DEAD) {
                if(state != STATE_STOPPED) {
                    lose("gc_start_the_world: wrong thread state is %d\n",
                         fixnum_value(state));
                }
                FSHOW_SIGNAL((stderr, "/gc_start_the_world: resuming %lu\n",
                              p->os_thread));
                set_thread_state(p, STATE_RUNNING);
            }
        }
    }

    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);
#ifdef LOCK_CREATE_THREAD
    lock_ret = pthread_mutex_unlock(&create_thread_lock);
    gc_assert(lock_ret == 0);
#endif

    FSHOW_SIGNAL((stderr,"/gc_start_the_world:end\n"));
}

#endif /* !LISP_FEATURE_SB_SAFEPOINT */
#endif /* !LISP_FEATURE_SB_THREAD */

int
thread_yield()
{
#ifdef LISP_FEATURE_SB_THREAD
    return sched_yield();
#else
    return 0;
#endif
}

int
wake_thread(os_thread_t os_thread)
{
#if defined(LISP_FEATURE_WIN32)
    return kill_safely(os_thread, 1);
#elif !defined(LISP_FEATURE_SB_THRUPTION)
    return kill_safely(os_thread, SIGPIPE);
#else
    return wake_thread_posix(os_thread);
#endif
}

/* If the thread id given does not belong to a running thread (it has
 * exited or never even existed) pthread_kill _may_ fail with ESRCH,
 * but it is also allowed to just segfault, see
 * <http://udrepper.livejournal.com/16844.html>.
 *
 * Relying on thread ids can easily backfire since ids are recycled
 * (NPTL recycles them extremely fast) so a signal can be sent to
 * another process if the one it was sent to exited.
 *
 * For these reasons, we must make sure that the thread is still alive
 * when the pthread_kill is called and return if the thread is
 * exiting.
 *
 * Note (DFL, 2011-06-22): At the time of writing, this function is only
 * used for INTERRUPT-THREAD, hence the wake_thread special-case for
 * Windows is OK. */
int
kill_safely(os_thread_t os_thread, int signal)
{
    FSHOW_SIGNAL((stderr,"/kill_safely: %lu, %d\n", os_thread, signal));
    {
#ifdef LISP_FEATURE_SB_THREAD
        sigset_t oldset;
        struct thread *thread;
        /* Frequent special case: resignalling to self.  The idea is
         * that leave_region safepoint will acknowledge the signal, so
         * there is no need to take locks, roll thread to safepoint
         * etc. */
        /* Kludge (on safepoint builds): At the moment, this isn't just
         * an optimization; rather it masks the fact that
         * gc_stop_the_world() grabs the all_threads mutex without
         * releasing it, and since we're not using recursive pthread
         * mutexes, the pthread_mutex_lock() around the all_threads loop
         * would go wrong.  Why are we running interruptions while
         * stopping the world though?  Test case is (:ASYNC-UNWIND
         * :SPECIALS), especially with s/10/100/ in both loops. */
        if (os_thread == pthread_self()) {
            pthread_kill(os_thread, signal);
#ifdef LISP_FEATURE_WIN32
            check_pending_thruptions(NULL);
#endif
            return 0;
        }

        /* pthread_kill is not async signal safe and we don't want to be
         * interrupted while holding the lock. */
        block_deferrable_signals(0, &oldset);
        pthread_mutex_lock(&all_threads_lock);
        for (thread = all_threads; thread; thread = thread->next) {
            if (thread->os_thread == os_thread) {
                int status = pthread_kill(os_thread, signal);
                if (status)
                    lose("kill_safely: pthread_kill failed with %d\n", status);
#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THRUPTION)
                wake_thread_win32(thread);
#endif
                break;
            }
        }
        pthread_mutex_unlock(&all_threads_lock);
        thread_sigmask(SIG_SETMASK,&oldset,0);
        if (thread)
            return 0;
        else
            return -1;
#elif defined(LISP_FEATURE_WIN32)
        return 0;
#else
        int status;
        if (os_thread != 0)
            lose("kill_safely: who do you want to kill? %d?\n", os_thread);
        /* Dubious (as in don't know why it works) workaround for the
         * signal sometimes not being generated on darwin. */
#ifdef LISP_FEATURE_DARWIN
        {
            sigset_t oldset;
            sigprocmask(SIG_BLOCK, &deferrable_sigset, &oldset);
            status = raise(signal);
            sigprocmask(SIG_SETMASK,&oldset,0);
        }
#else
        status = raise(signal);
#endif
        if (status == 0) {
            return 0;
        } else {
            lose("cannot raise signal %d, %d %s\n",
                 signal, status, strerror(errno));
        }
#endif
    }
}
