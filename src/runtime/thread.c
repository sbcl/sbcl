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
#include <signal.h>
#include <stddef.h>
#include <errno.h>
#include <sys/types.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/wait.h>
#endif

#include "runtime.h"
#include "validate.h"           /* for CONTROL_STACK_SIZE etc */
#include "alloc.h"
#include "thread.h"
#include "arch.h"
#include "target-arch-os.h"
#include "os.h"
#include "globals.h"
#include "dynbind.h"
#include "genesis/cons.h"
#include "genesis/fdefn.h"
#include "interr.h"             /* for lose() */
#include "gc-internal.h"

#ifdef LISP_FEATURE_WIN32
/*
 * Win32 doesn't have SIGSTKSZ, and we're not switching stacks anyway,
 * so define it arbitrarily
 */
#define SIGSTKSZ 1024
#endif

#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_SB_THREAD)
#define QUEUE_FREEABLE_THREAD_STACKS
#endif

#define ALIEN_STACK_SIZE (1*1024*1024) /* 1Mb size chosen at random */

struct freeable_stack {
#ifdef QUEUE_FREEABLE_THREAD_STACKS
    struct freeable_stack *next;
#endif
    os_thread_t os_thread;
    os_vm_address_t stack;
};


#ifdef QUEUE_FREEABLE_THREAD_STACKS
static struct freeable_stack * volatile freeable_stack_queue = 0;
static int freeable_stack_count = 0;
pthread_mutex_t freeable_stack_lock = PTHREAD_MUTEX_INITIALIZER;
#else
static struct freeable_stack * volatile freeable_stack = 0;
#endif

int dynamic_values_bytes=4096*sizeof(lispobj);  /* same for all threads */
struct thread * volatile all_threads;
extern struct interrupt_data * global_interrupt_data;

#ifdef LISP_FEATURE_LINUX
extern int linux_no_threads_p;
#endif

#ifdef LISP_FEATURE_SB_THREAD
pthread_mutex_t all_threads_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
extern lispobj call_into_lisp_first_time(lispobj fun, lispobj *args, int nargs);
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
#endif

static int
initial_thread_trampoline(struct thread *th)
{
    lispobj function;
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    lispobj *args = NULL;
#endif
    function = th->no_tls_value_marker;
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER_WIDETAG;
    if(arch_os_thread_init(th)==0) return 1;
    link_thread(th);
    th->os_thread=thread_self();
#ifndef LISP_FEATURE_WIN32
    protect_control_stack_guard_page(1);
#endif

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    return call_into_lisp_first_time(function,args,0);
#else
    return funcall0(function);
#endif
}

#define THREAD_STRUCT_SIZE (THREAD_CONTROL_STACK_SIZE + BINDING_STACK_SIZE + \
                            ALIEN_STACK_SIZE + dynamic_values_bytes + \
                            32 * SIGSTKSZ)

#ifdef LISP_FEATURE_SB_THREAD

#ifdef QUEUE_FREEABLE_THREAD_STACKS

queue_freeable_thread_stack(struct thread *thread_to_be_cleaned_up)
{
     if (thread_to_be_cleaned_up) {
        pthread_mutex_lock(&freeable_stack_lock);
        if (freeable_stack_queue) {
            struct freeable_stack *new_freeable_stack = 0, *next;
            next = freeable_stack_queue;
            while (next->next) {
                next = next->next;
            }
            new_freeable_stack = (struct freeable_stack *)
                os_validate(0, sizeof(struct freeable_stack));
            new_freeable_stack->next = NULL;
            new_freeable_stack->os_thread = thread_to_be_cleaned_up->os_thread;
            new_freeable_stack->stack = (os_vm_address_t)
                thread_to_be_cleaned_up->control_stack_start;
            next->next = new_freeable_stack;
            freeable_stack_count++;
        } else {
            struct freeable_stack *new_freeable_stack = 0;
            new_freeable_stack = (struct freeable_stack *)
                os_validate(0, sizeof(struct freeable_stack));
            new_freeable_stack->next = NULL;
            new_freeable_stack->os_thread = thread_to_be_cleaned_up->os_thread;
            new_freeable_stack->stack = (os_vm_address_t)
                thread_to_be_cleaned_up->control_stack_start;
            freeable_stack_queue = new_freeable_stack;
            freeable_stack_count++;
        }
        pthread_mutex_unlock(&freeable_stack_lock);
    }
}

static void
free_freeable_stacks() {
    if (freeable_stack_queue && (freeable_stack_count > 8)) {
        struct freeable_stack* old;
        pthread_mutex_lock(&freeable_stack_lock);
        old = freeable_stack_queue;
        freeable_stack_queue = old->next;
        freeable_stack_count--;
        gc_assert(pthread_join(old->os_thread, NULL) == 0);
        fprintf(stderr, "freeing thread %x stack\n", old->os_thread);
        os_invalidate(old->stack, THREAD_STRUCT_SIZE);
        os_invalidate((os_vm_address_t)old, sizeof(struct freeable_stack));
        pthread_mutex_unlock(&freeable_stack_lock);
    }
}

#else
static void
free_thread_stack_later(struct thread *thread_to_be_cleaned_up)
{
    struct freeable_stack *new_freeable_stack = 0;
    if (thread_to_be_cleaned_up) {
        new_freeable_stack = (struct freeable_stack *)
            os_validate(0, sizeof(struct freeable_stack));
        new_freeable_stack->os_thread = thread_to_be_cleaned_up->os_thread;
        new_freeable_stack->stack = (os_vm_address_t)
            thread_to_be_cleaned_up->control_stack_start;
    }
    new_freeable_stack = (struct freeable_stack *)
        swap_lispobjs((lispobj *)(void *)&freeable_stack,
                      (lispobj)new_freeable_stack);
    if (new_freeable_stack) {
        FSHOW((stderr,"/reaping %p\n", (void*) new_freeable_stack->os_thread));
        /* #if !defined(LISP_FEATURE_DARWIN) */
        /* Under NPTL pthread_join really waits until the thread
         * exists and the stack can be safely freed. This is sadly not
         * mandated by the pthread spec. */
        gc_assert(pthread_join(new_freeable_stack->os_thread, NULL) == 0);
        os_invalidate(new_freeable_stack->stack, THREAD_STRUCT_SIZE);
        os_invalidate((os_vm_address_t) new_freeable_stack,
                      sizeof(struct freeable_stack));
        /* #endif */
    }
}
#endif

/* this is the first thing that runs in the child (which is why the
 * silly calling convention).  Basically it calls the user's requested
 * lisp function after doing arch_os_thread_init and whatever other
 * bookkeeping needs to be done
 */
int
new_thread_trampoline(struct thread *th)
{
    lispobj function;
    int result;
    FSHOW((stderr,"/creating thread %lu\n", thread_self()));
    function = th->no_tls_value_marker;
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER_WIDETAG;
    if(arch_os_thread_init(th)==0) {
        /* FIXME: handle error */
        lose("arch_os_thread_init failed\n");
    }

    th->os_thread=thread_self();
    protect_control_stack_guard_page(1);
    /* Since GC can only know about this thread from the all_threads
     * list and we're just adding this thread to it there is no danger
     * of deadlocking even with SIG_STOP_FOR_GC blocked (which it is
     * not). */
    pthread_mutex_lock(&all_threads_lock);
    link_thread(th);
    pthread_mutex_unlock(&all_threads_lock);

    result = funcall0(function);
    th->state=STATE_DEAD;

    /* SIG_STOP_FOR_GC is blocked and GC might be waiting for this
     * thread, but since we are already dead it won't wait long. */
    pthread_mutex_lock(&all_threads_lock);
    gc_alloc_update_page_tables(0, &th->alloc_region);
    unlink_thread(th);
    pthread_mutex_unlock(&all_threads_lock);

    if(th->tls_cookie>=0) arch_os_thread_cleanup(th);
    os_invalidate((os_vm_address_t)th->interrupt_data,
                  (sizeof (struct interrupt_data)));

#ifdef QUEUE_FREEABLE_THREAD_STACKS
    queue_freeable_thread_stack(th);
#else
    free_thread_stack_later(th);
#endif

    FSHOW((stderr,"/exiting thread %p\n", thread_self()));
    return result;
}

#endif /* LISP_FEATURE_SB_THREAD */

static void
free_thread_struct(struct thread *th)
{
    if (th->interrupt_data)
        os_invalidate((os_vm_address_t) th->interrupt_data,
                      (sizeof (struct interrupt_data)));
    os_invalidate((os_vm_address_t) th->control_stack_start,
                  THREAD_STRUCT_SIZE);
}

/* this is called from any other thread to create the new one, and
 * initialize all parts of it that can be initialized from another
 * thread
 */

static struct thread *
create_thread_struct(lispobj initial_function) {
    union per_thread_data *per_thread;
    struct thread *th=0;        /*  subdue gcc */
    void *spaces=0;
#ifdef LISP_FEATURE_SB_THREAD
    int i;
#endif

    /* may as well allocate all the spaces at once: it saves us from
     * having to decide what to do if only some of the allocations
     * succeed */
    spaces=os_validate(0, THREAD_STRUCT_SIZE);
    if(!spaces)
         return NULL;
    per_thread=(union per_thread_data *)
        (spaces+
         THREAD_CONTROL_STACK_SIZE+
         BINDING_STACK_SIZE+
         ALIEN_STACK_SIZE);

#ifdef LISP_FEATURE_SB_THREAD
    for(i = 0; i < (dynamic_values_bytes / sizeof(lispobj)); i++)
        per_thread->dynamic_values[i] = NO_TLS_VALUE_MARKER_WIDETAG;
    if (all_threads == 0) {
        if(SymbolValue(FREE_TLS_INDEX,0)==UNBOUND_MARKER_WIDETAG) {
            SetSymbolValue
                (FREE_TLS_INDEX,
                 /* FIXME: should be MAX_INTERRUPTS -1 ? */
                 make_fixnum(MAX_INTERRUPTS+
                             sizeof(struct thread)/sizeof(lispobj)),
                 0);
            SetSymbolValue(TLS_INDEX_LOCK,make_fixnum(0),0);
        }
#define STATIC_TLS_INIT(sym,field) \
  ((struct symbol *)(sym-OTHER_POINTER_LOWTAG))->tls_index= \
  make_fixnum(THREAD_SLOT_OFFSET_WORDS(field))

        STATIC_TLS_INIT(BINDING_STACK_START,binding_stack_start);
        STATIC_TLS_INIT(BINDING_STACK_POINTER,binding_stack_pointer);
        STATIC_TLS_INIT(CONTROL_STACK_START,control_stack_start);
        STATIC_TLS_INIT(CONTROL_STACK_END,control_stack_end);
        STATIC_TLS_INIT(ALIEN_STACK,alien_stack_pointer);
#if defined(LISP_FEATURE_X86) || defined (LISP_FEATURE_X86_64)
        STATIC_TLS_INIT(PSEUDO_ATOMIC_ATOMIC,pseudo_atomic_atomic);
        STATIC_TLS_INIT(PSEUDO_ATOMIC_INTERRUPTED,pseudo_atomic_interrupted);
#endif
#undef STATIC_TLS_INIT
    }
#endif

    th=&per_thread->thread;
    th->control_stack_start = spaces;
    th->binding_stack_start=
        (lispobj*)((void*)th->control_stack_start+THREAD_CONTROL_STACK_SIZE);
    th->control_stack_end = th->binding_stack_start;
    th->alien_stack_start=
        (lispobj*)((void*)th->binding_stack_start+BINDING_STACK_SIZE);
    th->binding_stack_pointer=th->binding_stack_start;
    th->this=th;
    th->os_thread=0;
    th->state=STATE_RUNNING;
#ifdef LISP_FEATURE_STACK_GROWS_DOWNWARD_NOT_UPWARD
    th->alien_stack_pointer=((void *)th->alien_stack_start
                             + ALIEN_STACK_SIZE-N_WORD_BYTES);
#else
    th->alien_stack_pointer=((void *)th->alien_stack_start);
#endif
#if defined(LISP_FEATURE_X86) || defined (LISP_FEATURE_X86_64)
    th->pseudo_atomic_interrupted=0;
    th->pseudo_atomic_atomic=0;
#endif
#ifdef LISP_FEATURE_GENCGC
    gc_set_region_empty(&th->alloc_region);
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
    SetSymbolValue(BINDING_STACK_POINTER,(lispobj)th->binding_stack_pointer,th);
    SetSymbolValue(ALIEN_STACK,(lispobj)th->alien_stack_pointer,th);
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC,(lispobj)th->pseudo_atomic_atomic,th);
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED,th->pseudo_atomic_interrupted,th);
#else
    current_binding_stack_pointer=th->binding_stack_pointer;
    current_control_stack_pointer=th->control_stack_start;
#endif
#endif
    bind_variable(CURRENT_CATCH_BLOCK,make_fixnum(0),th);
    bind_variable(CURRENT_UNWIND_PROTECT_BLOCK,make_fixnum(0),th);
    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX,make_fixnum(0),th);
    bind_variable(INTERRUPT_PENDING, NIL,th);
    bind_variable(INTERRUPTS_ENABLED,T,th);
    bind_variable(GC_PENDING,NIL,th);
#ifdef LISP_FEATURE_SB_THREAD
    bind_variable(STOP_FOR_GC_PENDING,NIL,th);
#endif

    th->interrupt_data = (struct interrupt_data *)
        os_validate(0,(sizeof (struct interrupt_data)));
    if (!th->interrupt_data) {
        free_thread_struct(th);
        return 0;
    }
    th->interrupt_data->pending_handler = 0;
    th->no_tls_value_marker=initial_function;
    return th;
}

void create_initial_thread(lispobj initial_function) {
    struct thread *th=create_thread_struct(initial_function);
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
    pthread_attr_t attr;
    sigset_t newset,oldset;
    boolean r=1;
    int retcode, initcode, sizecode, addrcode;

    FSHOW_SIGNAL((stderr,"/create_os_thread: creating new thread\n"));

    sigemptyset(&newset);
    /* Blocking deferrable signals is enough, no need to block
     * SIG_STOP_FOR_GC because the child process is not linked onto
     * all_threads until it's ready. */
    sigaddset_deferrable(&newset);
    thread_sigmask(SIG_BLOCK, &newset, &oldset);

#if defined(LISP_FEATURE_DARWIN)
#define CONTROL_STACK_ADJUST 8192 /* darwin wants page-aligned stacks */
#else
#define CONTROL_STACK_ADJUST 16
#endif

    if((initcode = pthread_attr_init(&attr)) ||
       /* FIXME: why do we even have this in the first place? */
       (pthread_attr_setstack(&attr,th->control_stack_start,
                              THREAD_CONTROL_STACK_SIZE-CONTROL_STACK_ADJUST)) ||
#undef CONTROL_STACK_ADJUST
       (retcode = pthread_create
        (kid_tid,&attr,(void *(*)(void *))new_thread_trampoline,th))) {
        FSHOW_SIGNAL((stderr, "init, size, addr = %d, %d, %d\n", initcode, sizecode, addrcode));
        FSHOW_SIGNAL((stderr, printf("pthread_create returned %d, errno %d\n", retcode, errno)));
        FSHOW_SIGNAL((stderr, "wanted stack size %d, min stack size %d\n",
                      THREAD_CONTROL_STACK_SIZE-16, PTHREAD_STACK_MIN));
        if(retcode < 0) {
            perror("create_os_thread");
        }

        r=0;
    }
    thread_sigmask(SIG_SETMASK,&oldset,0);
    return r;
}

os_thread_t create_thread(lispobj initial_function) {
    struct thread *th;
    os_thread_t kid_tid;

#ifdef LISP_FEATURE_LINUX
    if(linux_no_threads_p) return 0;
#endif

    /* Assuming that a fresh thread struct has no lisp objects in it,
     * linking it to all_threads can be left to the thread itself
     * without fear of gc lossage. initial_function violates this
     * assumption and must stay pinned until the child starts up. */
    th = create_thread_struct(initial_function);
    if(th==0) return 0;

    if (create_os_thread(th,&kid_tid)) {
        return kid_tid;
    } else {
        free_thread_struct(th);
        return 0;
    }
}

/* Send the signo to os_thread, retry if the rt signal queue is
 * full. */
static int kill_thread_safely(os_thread_t os_thread, int signo)
{
    int r;
    /* The man page does not mention EAGAIN as a valid return value
     * for either pthread_kill or kill. But that's theory, this is
     * practice. By waiting here we assume that the delivery of this
     * signal is not necessary for the delivery of the signals in the
     * queue. In other words, we _assume_ there are no deadlocks. */
    while ((r=pthread_kill(os_thread,signo))==EAGAIN) {
        /* wait a bit then try again in the hope of the rt signal
         * queue not being full */
        FSHOW_SIGNAL((stderr,"/rt signal queue full\n"));
        /* FIXME: some kind of backoff (random, exponential) would be
         * nice. */
        sleep(1);
    }
    return r;
}

int signal_interrupt_thread(os_thread_t os_thread)
{
    int status = kill_thread_safely(os_thread, SIG_INTERRUPT_THREAD);
    if (status == 0) {
        return 0;
    } else if (status == ESRCH) {
        return -1;
    } else {
        lose("cannot send SIG_INTERRUPT_THREAD to thread=%lu: %d, %s\n",
             os_thread, status, strerror(status));
    }
}

/* stopping the world is a two-stage process.  From this thread we signal
 * all the others with SIG_STOP_FOR_GC.  The handler for this signal does
 * the usual pseudo-atomic checks (we don't want to stop a thread while
 * it's in the middle of allocation) then waits for another SIG_STOP_FOR_GC.
 */

/* To avoid deadlocks when gc stops the world all clients of each
 * mutex must enable or disable SIG_STOP_FOR_GC for the duration of
 * holding the lock, but they must agree on which. */
void gc_stop_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    int status;
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:waiting on lock, thread=%lu\n",
                  th->os_thread));
    /* keep threads from starting while the world is stopped. */
    pthread_mutex_lock(&all_threads_lock); \
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:got lock, thread=%lu\n",
                  th->os_thread));
    /* stop all other threads by sending them SIG_STOP_FOR_GC */
    for(p=all_threads; p; p=p->next) {
        gc_assert(p->os_thread != 0);
        FSHOW_SIGNAL((stderr,"/gc_stop_the_world: p->state: %x\n", p->state));
        if((p!=th) && ((p->state==STATE_RUNNING))) {
            FSHOW_SIGNAL((stderr,"/gc_stop_the_world: suspending %x, os_thread %x\n",
                          p, p->os_thread));
            status=kill_thread_safely(p->os_thread,SIG_STOP_FOR_GC);
            if (status==ESRCH) {
                /* This thread has exited. */
                gc_assert(p->state==STATE_DEAD);
            } else if (status) {
                lose("cannot send suspend thread=%lu: %d, %s\n",
                     p->os_thread,status,strerror(status));
            }
        }
    }
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:signals sent\n"));
    /* wait for the running threads to stop or finish */
    for(p=all_threads;p;) {
        FSHOW_SIGNAL((stderr,"/gc_stop_the_world: th: %p, p: %p\n", th, p));
        if((p!=th) && (p->state==STATE_RUNNING)) {
            sched_yield();
        } else {
            p=p->next;
        }
    }
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:end\n"));
}

void gc_start_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    int status;
    /* if a resumed thread creates a new thread before we're done with
     * this loop, the new thread will get consed on the front of
     * all_threads, but it won't have been stopped so won't need
     * restarting */
    FSHOW_SIGNAL((stderr,"/gc_start_the_world:begin\n"));
    for(p=all_threads;p;p=p->next) {
        gc_assert(p->os_thread!=0);
        if((p!=th) && (p->state!=STATE_DEAD)) {
            if(p->state!=STATE_SUSPENDED) {
                lose("gc_start_the_world: wrong thread state is %d\n",
                     fixnum_value(p->state));
            }
            FSHOW_SIGNAL((stderr, "/gc_start_the_world: resuming %lu\n",
                          p->os_thread));
            p->state=STATE_RUNNING;

#if defined(SIG_RESUME_FROM_GC)
            status=kill_thread_safely(p->os_thread,SIG_RESUME_FROM_GC);
#else
            status=kill_thread_safely(p->os_thread,SIG_STOP_FOR_GC);
#endif
            if (status) {
                lose("cannot resume thread=%lu: %d, %s\n",
                     p->os_thread,status,strerror(status));
            }
        }
    }
    /* If we waited here until all threads leave STATE_SUSPENDED, then
     * SIG_STOP_FOR_GC wouldn't need to be a rt signal. That has some
     * performance implications, but does away with the 'rt signal
     * queue full' problem. */

#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_SB_THREAD)
    /* we must wait for all threads to leave suspended state else we
     * risk signal accumulation and lose any meaning of
     * thread->state */
    for(p=all_threads;p;) {
        if((p!=th) && (p->state==STATE_SUSPENDED)) {
            sched_yield();
        } else {
            p=p->next;
        }
    }
#endif

    pthread_mutex_unlock(&all_threads_lock);
    FSHOW_SIGNAL((stderr,"/gc_start_the_world:end\n"));
}
#endif
