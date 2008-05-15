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

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/mach_types.h>
#endif

#include "runtime.h"
#include "validate.h"           /* for BINDING_STACK_SIZE etc */
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
#define DELAY_THREAD_POST_MORTEM 5
#define LOCK_CREATE_THREAD
#endif

#ifdef LISP_FEATURE_FREEBSD
#define CREATE_CLEANUP_THREAD
#define LOCK_CREATE_THREAD
#endif

#define ALIEN_STACK_SIZE (1*1024*1024) /* 1Mb size chosen at random */

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

int dynamic_values_bytes=TLS_SIZE*sizeof(lispobj);  /* same for all threads */
struct thread * volatile all_threads;
extern struct interrupt_data * global_interrupt_data;

#ifdef LISP_FEATURE_SB_THREAD
pthread_mutex_t all_threads_lock = PTHREAD_MUTEX_INITIALIZER;
#ifdef LOCK_CREATE_THREAD
static pthread_mutex_t create_thread_lock = PTHREAD_MUTEX_INITIALIZER;
#endif
#ifdef LISP_FEATURE_GCC_TLS
__thread struct thread *current_thread;
#endif
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

#define THREAD_STRUCT_SIZE (thread_control_stack_size + BINDING_STACK_SIZE + \
                            ALIEN_STACK_SIZE + dynamic_values_bytes +        \
                            32 * SIGSTKSZ +                                  \
                            THREAD_ALIGNMENT_BYTES)

#ifdef LISP_FEATURE_SB_THREAD
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

/* this is the first thing that runs in the child (which is why the
 * silly calling convention).  Basically it calls the user's requested
 * lisp function after doing arch_os_thread_init and whatever other
 * bookkeeping needs to be done
 */
int
new_thread_trampoline(struct thread *th)
{
    lispobj function;
    int result, lock_ret;

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
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);
    link_thread(th);
    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    result = funcall0(function);

    /* Block GC */
    block_blockable_signals();
    th->state=STATE_DEAD;

    /* SIG_STOP_FOR_GC is blocked and GC might be waiting for this
     * thread, but since we are already dead it won't wait long. */
    lock_ret = pthread_mutex_lock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    gc_alloc_update_page_tables(0, &th->alloc_region);
    unlink_thread(th);
    pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);

    if(th->tls_cookie>=0) arch_os_thread_cleanup(th);
    os_invalidate((os_vm_address_t)th->interrupt_data,
                  (sizeof (struct interrupt_data)));

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
    FSHOW((stderr, "Deallocating mach port %x\n", THREAD_STRUCT_TO_EXCEPTION_PORT(th)));
    mach_port_move_member(mach_task_self(),
                          THREAD_STRUCT_TO_EXCEPTION_PORT(th),
                          MACH_PORT_NULL);
    mach_port_deallocate(mach_task_self(),
                         THREAD_STRUCT_TO_EXCEPTION_PORT(th));
    mach_port_destroy(mach_task_self(),
                      THREAD_STRUCT_TO_EXCEPTION_PORT(th));
#endif

    schedule_thread_post_mortem(th);
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
    os_invalidate((os_vm_address_t) th->os_address,
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
    void *aligned_spaces=0;
#ifdef LISP_FEATURE_SB_THREAD
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
    aligned_spaces = (void *)((((unsigned long)(char *)spaces)
                               + THREAD_ALIGNMENT_BYTES-1)
                              &~(unsigned long)(THREAD_ALIGNMENT_BYTES-1));
    per_thread=(union per_thread_data *)
        (aligned_spaces+
         thread_control_stack_size+
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
        STATIC_TLS_INIT(PSEUDO_ATOMIC_BITS,pseudo_atomic_bits);
#endif
#undef STATIC_TLS_INIT
    }
#endif

    th=&per_thread->thread;
    th->os_address = spaces;
    th->control_stack_start = aligned_spaces;
    th->binding_stack_start=
        (lispobj*)((void*)th->control_stack_start+thread_control_stack_size);
    th->control_stack_end = th->binding_stack_start;
    th->alien_stack_start=
        (lispobj*)((void*)th->binding_stack_start+BINDING_STACK_SIZE);
    th->binding_stack_pointer=th->binding_stack_start;
    th->this=th;
    th->os_thread=0;
    th->os_attr=malloc(sizeof(pthread_attr_t));
    th->state=STATE_RUNNING;
#ifdef LISP_FEATURE_STACK_GROWS_DOWNWARD_NOT_UPWARD
    th->alien_stack_pointer=((void *)th->alien_stack_start
                             + ALIEN_STACK_SIZE-N_WORD_BYTES);
#else
    th->alien_stack_pointer=((void *)th->alien_stack_start);
#endif
#if defined(LISP_FEATURE_X86) || defined (LISP_FEATURE_X86_64)
    th->pseudo_atomic_bits=0;
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
    SetSymbolValue(PSEUDO_ATOMIC_BITS,(lispobj)th->pseudo_atomic_bits,th);
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
    bind_variable(ALLOW_WITH_INTERRUPTS,T,th);
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

    th->stepping = NIL;
    return th;
}

#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
mach_port_t setup_mach_exception_handling_thread();
kern_return_t mach_thread_init(mach_port_t thread_exception_port);

#endif

void create_initial_thread(lispobj initial_function) {
    struct thread *th=create_thread_struct(initial_function);
    if(th) {
#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
        setup_mach_exception_handling_thread();
#endif
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
    sigset_t newset,oldset;
    boolean r=1;
    int retcode = 0, initcode;

    FSHOW_SIGNAL((stderr,"/create_os_thread: creating new thread\n"));

#ifdef LOCK_CREATE_THREAD
    retcode = pthread_mutex_lock(&create_thread_lock);
    gc_assert(retcode == 0);
    FSHOW_SIGNAL((stderr,"/create_os_thread: got lock\n"));
#endif
    sigemptyset(&newset);
    /* Blocking deferrable signals is enough, no need to block
     * SIG_STOP_FOR_GC because the child process is not linked onto
     * all_threads until it's ready. */
    sigaddset_deferrable(&newset);
    thread_sigmask(SIG_BLOCK, &newset, &oldset);

    if((initcode = pthread_attr_init(th->os_attr)) ||
       /* call_into_lisp_first_time switches the stack for the initial thread. For the
        * others, we use this. */
       (pthread_attr_setstack(th->os_attr,th->control_stack_start,thread_control_stack_size)) ||
       (retcode = pthread_create
        (kid_tid,th->os_attr,(void *(*)(void *))new_thread_trampoline,th))) {
        FSHOW_SIGNAL((stderr, "init = %d\n", initcode));
        FSHOW_SIGNAL((stderr, printf("pthread_create returned %d, errno %d\n", retcode, errno)));
        FSHOW_SIGNAL((stderr, "wanted stack size %d, min stack size %d\n",
                      cstack_size, PTHREAD_STACK_MIN));
        if(retcode < 0) {
            perror("create_os_thread");
        }
        r=0;
    }

    thread_sigmask(SIG_SETMASK,&oldset,0);
#ifdef LOCK_CREATE_THREAD
    retcode = pthread_mutex_unlock(&create_thread_lock);
    gc_assert(retcode == 0);
    FSHOW_SIGNAL((stderr,"/create_os_thread: released lock\n"));
#endif
    return r;
}

os_thread_t create_thread(lispobj initial_function) {
    struct thread *th;
    os_thread_t kid_tid;

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
int
kill_thread_safely(os_thread_t os_thread, int signo)
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
    int status, lock_ret;
#ifdef LOCK_CREATE_THREAD
    /* KLUDGE: Stopping the thread during pthread_create() causes deadlock
     * on FreeBSD. */
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:waiting on create_thread_lock, thread=%lu\n",
                  th->os_thread));
    lock_ret = pthread_mutex_lock(&create_thread_lock);
    gc_assert(lock_ret == 0);
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:got create_thread_lock, thread=%lu\n",
                  th->os_thread));
#endif
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:waiting on lock, thread=%lu\n",
                  th->os_thread));
    /* keep threads from starting while the world is stopped. */
    lock_ret = pthread_mutex_lock(&all_threads_lock);      \
    gc_assert(lock_ret == 0);

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
    int status, lock_ret;
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

    lock_ret = pthread_mutex_unlock(&all_threads_lock);
    gc_assert(lock_ret == 0);
#ifdef LOCK_CREATE_THREAD
    lock_ret = pthread_mutex_unlock(&create_thread_lock);
    gc_assert(lock_ret == 0);
#endif

    FSHOW_SIGNAL((stderr,"/gc_start_the_world:end\n"));
}
#endif

int
thread_yield()
{
#ifdef LISP_FEATURE_SB_THREAD
    return sched_yield();
#else
    return 0;
#endif
}
