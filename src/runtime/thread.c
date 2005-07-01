#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sched.h>
#include <signal.h>
#include <stddef.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "sbcl.h"
#include "runtime.h"
#include "validate.h"		/* for CONTROL_STACK_SIZE etc */
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

#define ALIEN_STACK_SIZE (1*1024*1024) /* 1Mb size chosen at random */

int dynamic_values_bytes=4096*sizeof(lispobj);	/* same for all threads */
struct thread *all_threads;
volatile lispobj all_threads_lock;
extern struct interrupt_data * global_interrupt_data;
extern int linux_no_threads_p;

/* When trying to get all_threads_lock one should make sure that
 * sig_stop_for_gc is not blocked. Else there would be a possible
 * deadlock: gc locks it, other thread blocks signals, gc sends stop
 * request to other thread and waits, other thread blocks on lock. */
void check_sig_stop_for_gc_can_arrive_or_lose()
{
    /* Get the current sigmask, by blocking the empty set. */
    sigset_t empty,current;
    sigemptyset(&empty);
    thread_sigmask(SIG_BLOCK, &empty, &current);
    if (sigismember(&current,SIG_STOP_FOR_GC))
        lose("SIG_STOP_FOR_GC is blocked\n");
    if (SymbolValue(INTERRUPTS_ENABLED,arch_os_get_current_thread()) == NIL)
        lose("interrupts disabled\n");
    if (arch_pseudo_atomic_atomic(NULL))
        lose("n pseudo atomic\n");
}

#ifdef QSHOW_SIGNALS
#define FSHOW_SIGNAL FSHOW
#else
#define FSHOW_SIGNAL(args)
#endif

#define GET_ALL_THREADS_LOCK(name) \
    { \
        sigset_t _newset,_oldset; \
        sigemptyset(&_newset); \
        sigaddset_blockable(&_newset); \
        sigdelset(&_newset,SIG_STOP_FOR_GC); \
        thread_sigmask(SIG_BLOCK, &_newset, &_oldset); \
        check_sig_stop_for_gc_can_arrive_or_lose(); \
        FSHOW_SIGNAL((stderr,"/%s:waiting on lock=%ld, thread=%ld\n",name, \
               all_threads_lock,arch_os_get_current_thread()->os_thread)); \
        get_spinlock(&all_threads_lock,(long)arch_os_get_current_thread()); \
        FSHOW_SIGNAL((stderr,"/%s:got lock, thread=%ld\n", \
               name,arch_os_get_current_thread()->os_thread));

#define RELEASE_ALL_THREADS_LOCK(name) \
        FSHOW_SIGNAL((stderr,"/%s:released lock\n",name)); \
        release_spinlock(&all_threads_lock); \
        thread_sigmask(SIG_SETMASK,&_oldset,0); \
    }

int
initial_thread_trampoline(struct thread *th)
{
    lispobj function;
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    lispobj *args = NULL;
#endif

    function = th->unbound_marker;
    th->unbound_marker = UNBOUND_MARKER_WIDETAG;
    if(arch_os_thread_init(th)==0) return 1;

    if(th->os_thread < 1) lose("th->os_thread not set up right");
    th->state=STATE_RUNNING;
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    return call_into_lisp_first_time(function,args,0);
#else
    return funcall0(function);
#endif
}

#ifdef LISP_FEATURE_SB_THREAD

/* this is the first thing that runs in the child (which is why the
 * silly calling convention).  Basically it calls the user's requested
 * lisp function after doing arch_os_thread_init and whatever other
 * bookkeeping needs to be done
 */
int
new_thread_trampoline(struct thread *th)
{
    lispobj function;
    function = th->unbound_marker;
    th->unbound_marker = UNBOUND_MARKER_WIDETAG;
    if(arch_os_thread_init(th)==0) return 1;

    /* wait here until our thread is linked into all_threads: see below */
    while(th->os_thread<1) sched_yield();

    th->state=STATE_RUNNING;
    return funcall0(function);
}
#endif /* LISP_FEATURE_SB_THREAD */

/* this is called from any other thread to create the new one, and
 * initialize all parts of it that can be initialized from another
 * thread
 */

struct thread * create_thread_struct(lispobj initial_function) {
    union per_thread_data *per_thread;
    struct thread *th=0;	/*  subdue gcc */
    void *spaces=0;

    /* may as well allocate all the spaces at once: it saves us from
     * having to decide what to do if only some of the allocations
     * succeed */
    spaces=os_validate(0,
		       THREAD_CONTROL_STACK_SIZE+
		       BINDING_STACK_SIZE+
		       ALIEN_STACK_SIZE+
		       dynamic_values_bytes+
		       32*SIGSTKSZ);
    if(!spaces)
	 return NULL;
    per_thread=(union per_thread_data *)
	(spaces+
	 THREAD_CONTROL_STACK_SIZE+
	 BINDING_STACK_SIZE+
	 ALIEN_STACK_SIZE);

    if(all_threads) {
	memcpy(per_thread,arch_os_get_current_thread(),
	       dynamic_values_bytes);
    } else {
#ifdef LISP_FEATURE_SB_THREAD
	int i;
	for(i=0;i<(dynamic_values_bytes/sizeof(lispobj));i++)
	    per_thread->dynamic_values[i]=UNBOUND_MARKER_WIDETAG;
	if(SymbolValue(FREE_TLS_INDEX,0)==UNBOUND_MARKER_WIDETAG)
	    SetSymbolValue
		(FREE_TLS_INDEX,
		 make_fixnum(MAX_INTERRUPTS+
			     sizeof(struct thread)/sizeof(lispobj)),
		 0);
#define STATIC_TLS_INIT(sym,field) \
  ((struct symbol *)(sym-OTHER_POINTER_LOWTAG))->tls_index= \
  make_fixnum(THREAD_SLOT_OFFSET_WORDS(field))

	STATIC_TLS_INIT(BINDING_STACK_START,binding_stack_start);
	STATIC_TLS_INIT(BINDING_STACK_POINTER,binding_stack_pointer);
	STATIC_TLS_INIT(CONTROL_STACK_START,control_stack_start);
	STATIC_TLS_INIT(CONTROL_STACK_END,control_stack_end);
	STATIC_TLS_INIT(ALIEN_STACK,alien_stack_pointer);
#ifdef LISP_FEATURE_X86
	STATIC_TLS_INIT(PSEUDO_ATOMIC_ATOMIC,pseudo_atomic_atomic);
	STATIC_TLS_INIT(PSEUDO_ATOMIC_INTERRUPTED,pseudo_atomic_interrupted);
#endif
#undef STATIC_TLS_INIT
#endif
    }

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
    th->interrupt_fun=NIL;
    th->interrupt_fun_lock=0;
    th->state=STATE_STARTING;
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

    th->interrupt_data = (struct interrupt_data *)
        os_validate(0,(sizeof (struct interrupt_data)));
    if(all_threads)
	memcpy(th->interrupt_data,
	       arch_os_get_current_thread()->interrupt_data,
	       sizeof (struct interrupt_data));
    else
	memcpy(th->interrupt_data,global_interrupt_data,
	       sizeof (struct interrupt_data));

    th->unbound_marker=initial_function;
    return th;
}

void link_thread(struct thread *th,os_thread_t kid_tid)
{
    if (all_threads) all_threads->prev=th;
    th->next=all_threads;
    th->prev=0;
    all_threads=th;
    /* note that th->os_thread is 0 at this time.  We rely on
     * all_threads_lock to ensure that we don't have >1 thread with
     * os_thread=0 on the list at once
     */
    protect_control_stack_guard_page(th,1);
    /* child will not start until this is set */
    th->os_thread=kid_tid;
}

void create_initial_thread(lispobj initial_function) {
    struct thread *th=create_thread_struct(initial_function);
    os_thread_t kid_tid=thread_self();
    if(th && kid_tid>0) {
	link_thread(th,kid_tid);
	initial_thread_trampoline(all_threads); /* no return */
    } else lose("can't create initial thread");
}

#ifdef LISP_FEATURE_SB_THREAD

boolean create_os_thread(struct thread *th,os_thread_t *kid_tid)
{
    /* The new thread inherits the restrictive signal mask set here,
     * and enables signals again when it is set up properly. */
    pthread_attr_t attr;
    sigset_t newset,oldset;
    boolean r=1;
    sigemptyset(&newset);
    sigaddset_blockable(&newset);
    thread_sigmask(SIG_BLOCK, &newset, &oldset);
    
    if((pthread_attr_init(&attr)) ||
       (pthread_attr_setstack(&attr,th->control_stack_start,
                              THREAD_CONTROL_STACK_SIZE-16)) ||
       (pthread_create
        (kid_tid,&attr,(void *(*)(void *))new_thread_trampoline,th)))
        r=0;
    thread_sigmask(SIG_SETMASK,&oldset,0);
    return r;
}

struct thread *create_thread(lispobj initial_function) {
    struct thread *th;
    os_thread_t kid_tid=0;
    boolean success;

    if(linux_no_threads_p) return 0;

    th=create_thread_struct(initial_function);
    if(th==0) return 0;

    /* we must not be interrupted here after a successful
     * create_os_thread, because the kid will be waiting for its
     * thread struct to be linked */
    GET_ALL_THREADS_LOCK("create_thread")

    success=create_os_thread(th,&kid_tid);
    if (success)
	link_thread(th,kid_tid);
    else
	os_invalidate((os_vm_address_t) th->control_stack_start,
		      ((sizeof (lispobj))
		       * (th->control_stack_end-th->control_stack_start)) +
		      BINDING_STACK_SIZE+ALIEN_STACK_SIZE+dynamic_values_bytes+
		      32*SIGSTKSZ);

    RELEASE_ALL_THREADS_LOCK("create_thread")

    if (success)
        return th;
    else
        return 0;
}
#endif

#if defined LISP_FEATURE_SB_THREAD
/* This is not needed unless #+SB-THREAD, as there's a trivial null
 * unithread definition. */

/* called from lisp from the thread object finalizer */
void reap_dead_thread(struct thread *th)
{
    if(th->state!=STATE_DEAD)
        lose("thread %lx is not joinable, state=%d\n",th,th->state);
#ifdef LISP_FEATURE_GENCGC
    {
        sigset_t newset,oldset;
        sigemptyset(&newset);
        sigaddset_blockable(&newset);
        thread_sigmask(SIG_BLOCK, &newset, &oldset);
        gc_alloc_update_page_tables(0, &th->alloc_region);
        release_spinlock(&all_threads_lock);
        thread_sigmask(SIG_SETMASK,&oldset,0);
    }
#endif
    GET_ALL_THREADS_LOCK("reap_dead_thread")
    FSHOW((stderr,"/reap_dead_thread: reaping %ld\n",th->os_thread));
    if(th->prev)
        th->prev->next=th->next;
    else all_threads=th->next;
    if(th->next)
        th->next->prev=th->prev;
    RELEASE_ALL_THREADS_LOCK("reap_dead_thread")
    if(th->tls_cookie>=0) arch_os_thread_cleanup(th);
    gc_assert(pthread_join(th->os_thread,NULL)==0);
    os_invalidate((os_vm_address_t) th->control_stack_start,
                  ((sizeof (lispobj))
                   * (th->control_stack_end-th->control_stack_start)) +
                  BINDING_STACK_SIZE+ALIEN_STACK_SIZE+dynamic_values_bytes+
                  32*SIGSTKSZ);
}

int interrupt_thread(struct thread *th, lispobj function)
{
    /* A thread may also become dead after this test. */
    if ((th->state != STATE_DEAD)) {
        /* In clone_threads, if A and B both interrupt C at
         * approximately the same time, it does not matter: the
         * second signal will be masked until the handler has
         * returned from the first one.  In pthreads though, we
         * can't put the knowledge of what function to call into
         * the siginfo, so we have to store it in the destination
         * thread, and do it in such a way that A won't clobber
         * B's interrupt.  Hence this stupid linked list.
         *
         * This does depend on SIG_INTERRUPT_THREAD being queued
         * (as POSIX RT signals are): we need to keep
         * interrupt_fun data for exactly as many signals as are
         * going to be received by the destination thread.
         */
        struct cons *c=alloc_cons(function,NIL);
        int kill_status;
        /* interrupt_thread_handler locks this spinlock with
         * interrupts blocked and it does so for the sake of
         * arrange_return_to_lisp_function, so we must also block
         * them. */
        sigset_t newset,oldset;
        sigemptyset(&newset);
        sigaddset_blockable(&newset);
        thread_sigmask(SIG_BLOCK, &newset, &oldset);
        get_spinlock(&th->interrupt_fun_lock,
                     (long)arch_os_get_current_thread());
        kill_status=thread_kill(th->os_thread,SIG_INTERRUPT_THREAD);
        if(kill_status==0) {
            ((struct cons *)native_pointer(c))->cdr=th->interrupt_fun;
            th->interrupt_fun=c;
        }
        release_spinlock(&th->interrupt_fun_lock);
        thread_sigmask(SIG_SETMASK,&oldset,0);
        return (kill_status ? -1 : 0);
    }
    errno=EPERM; return -1;
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
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:waiting on lock, thread=%ld\n",
                  th->os_thread));
    /* keep threads from starting while the world is stopped. */
    get_spinlock(&all_threads_lock,(long)th);
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:got lock, thread=%ld\n",
                  th->os_thread));
    /* stop all other threads by sending them SIG_STOP_FOR_GC */
    for(p=all_threads; p; p=p->next) {
        while(p->state==STATE_STARTING) sched_yield();
        if((p!=th) && (p->state==STATE_RUNNING)) {
            FSHOW_SIGNAL((stderr,"/gc_stop_the_world:sending sig_stop to %ld\n",
                          p->os_thread));
            if(thread_kill(p->os_thread,SIG_STOP_FOR_GC)==-1) {
                /* we can't kill the thread; assume because it died
                 * since we last checked */
                p->state=STATE_DEAD;
                FSHOW_SIGNAL((stderr,"/gc_stop_the_world:assuming %ld dead\n",
                   p->os_thread));
            }
        }
    }
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:signals sent\n"));
    /* wait for the running threads to stop or finish */
    for(p=all_threads;p;) {
        gc_assert(p->os_thread!=0);
        gc_assert(p->state!=STATE_STARTING);
        if((p==th) || (p->state==STATE_SUSPENDED) ||
           (p->state==STATE_DEAD)) {
            p=p->next;
        }
    }
    FSHOW_SIGNAL((stderr,"/gc_stop_the_world:end\n"));
}

void gc_start_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    /* if a resumed thread creates a new thread before we're done with
     * this loop, the new thread will get consed on the front of
     * all_threads, but it won't have been stopped so won't need
     * restarting */
    FSHOW_SIGNAL((stderr,"/gc_start_the_world:begin\n"));
    for(p=all_threads;p;p=p->next) {
        gc_assert(p->os_thread!=0);
	if((p!=th) && (p->state!=STATE_DEAD)) {
            if(p->state!=STATE_SUSPENDED) {
                lose("gc_start_the_world: wrong thread state is %ld\n",
                     fixnum_value(p->state));
            }
            thread_kill(p->os_thread,SIG_STOP_FOR_GC);
        }
    }
    /* we must wait for all threads to leave stopped state else we
     * risk signal accumulation and lose any meaning of
     * thread->state */
    for(p=all_threads;p;) {
        if((p==th) || (p->state!=STATE_SUSPENDED)) {
            p=p->next;
        }
    }
    release_spinlock(&all_threads_lock);
    FSHOW_SIGNAL((stderr,"/gc_start_the_world:end\n"));
}
#endif
