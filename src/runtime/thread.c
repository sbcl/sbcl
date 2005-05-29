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
volatile lispobj thread_start_lock;
extern struct interrupt_data * global_interrupt_data;
extern int linux_no_threads_p;

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

    if(th->pid < 1) lose("th->pid not set up right");
    th->state=STATE_RUNNING;
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    return call_into_lisp_first_time(function,args,0);
#else
    return funcall0(function);
#endif
}

/* this is the first thing that clone() runs in the child (which is
 * why the silly calling convention).  Basically it calls the user's
 * requested lisp function after doing arch_os_thread_init and
 * whatever other bookkeeping needs to be done
 */

#ifdef LISP_FEATURE_SB_THREAD
int
new_thread_trampoline(struct thread *th)
{
    lispobj function;
    function = th->unbound_marker;
    th->unbound_marker = UNBOUND_MARKER_WIDETAG;
    if(arch_os_thread_init(th)==0) return 1;	

    /* wait here until our thread is linked into all_threads: see below */
    while(th->pid<1) sched_yield();

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
    th->pid=0;
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

    th->interrupt_data =
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

void link_thread(struct thread *th,pid_t kid_pid)
{
    sigset_t newset,oldset;
    sigemptyset(&newset);
    sigaddset_blockable(&newset);
    sigprocmask(SIG_BLOCK, &newset, &oldset); 

    get_spinlock(&all_threads_lock,kid_pid);
    th->next=all_threads;
    all_threads=th;
    /* note that th->pid is 0 at this time.  We rely on all_threads_lock
     * to ensure that we don't have >1 thread with pid=0 on the list at once
     */
    protect_control_stack_guard_page(th->pid,1);
    th->pid=kid_pid;		/* child will not start until this is set */
    release_spinlock(&all_threads_lock);

    sigprocmask(SIG_SETMASK,&oldset,0);
}

void create_initial_thread(lispobj initial_function) {
    struct thread *th=create_thread_struct(initial_function);
    pid_t kid_pid=getpid();
    if(th && kid_pid>0) {
	link_thread(th,kid_pid);
	initial_thread_trampoline(all_threads); /* no return */
    } else lose("can't create initial thread");
}

#ifdef LISP_FEATURE_SB_THREAD
pid_t create_thread(lispobj initial_function) {
    struct thread *th;
    pid_t kid_pid=0;

    if(linux_no_threads_p) return 0;
    th=create_thread_struct(initial_function);
    if(th==0) return 0;
#ifdef QSHOW_SIGNALS
    SHOW("create_thread:waiting on lock");
#endif
    get_spinlock(&thread_start_lock,arch_os_get_current_thread()->pid);
#ifdef QSHOW_SIGNALS
    SHOW("create_thread:got lock");
#endif
    kid_pid=clone(new_thread_trampoline,
		  (((void*)th->control_stack_start)+
		   THREAD_CONTROL_STACK_SIZE-16),
		  CLONE_FILES|SIG_THREAD_EXIT|CLONE_VM,th);
    
    if(kid_pid>0) {
	link_thread(th,kid_pid);
        /* wait here until our thread is started: see new_thread_trampoline */
        while(th->state==STATE_STARTING) sched_yield();
        /* it's started and initialized, it's safe to gc */
        release_spinlock(&thread_start_lock);
#ifdef QSHOW_SIGNALS
        SHOW("create_thread:released lock");
#endif
	return th->pid;
    } else {
        release_spinlock(&thread_start_lock);
#ifdef QSHOW_SIGNALS
        SHOW("create_thread:released lock(failure)");
#endif
	os_invalidate((os_vm_address_t) th->control_stack_start,
		      ((sizeof (lispobj))
		       * (th->control_stack_end-th->control_stack_start)) +
		      BINDING_STACK_SIZE+ALIEN_STACK_SIZE+dynamic_values_bytes+
		      32*SIGSTKSZ);
	return 0;
    }
}
#endif

struct thread *find_thread_by_pid(pid_t pid) 
{
    struct thread *th;
    for_each_thread(th)
	if(th->pid==pid) return th;
    return 0;
}

#if defined LISP_FEATURE_SB_THREAD
/* This is not needed unless #+SB-THREAD, as there's a trivial null
 * unithread definition. */

void mark_dead_threads() 
{
    pid_t kid;
    int status;
    while(1) {
	kid=waitpid(-1,&status,__WALL|WNOHANG);
	if(kid<=0) break;
	if(WIFEXITED(status) || WIFSIGNALED(status)) {
	    struct thread *th=find_thread_by_pid(kid);
	    if(th) th->state=STATE_DEAD;
	}
    }
}

void reap_dead_threads() 
{
    struct thread *th,*next,*prev=0;
    th=all_threads;
    while(th) {
	next=th->next;
	if(th->state==STATE_DEAD) {
	    funcall1(SymbolFunction(HANDLE_THREAD_EXIT),make_fixnum(th->pid));
#ifdef LISP_FEATURE_GENCGC
	    gc_alloc_update_page_tables(0, &th->alloc_region);
#endif
	    get_spinlock(&all_threads_lock,th->pid);
	    if(prev) prev->next=next;
	    else all_threads=next;
	    release_spinlock(&all_threads_lock);
	    if(th->tls_cookie>=0) arch_os_thread_cleanup(th); 
	    os_invalidate((os_vm_address_t) th->control_stack_start,
			  ((sizeof (lispobj))
			   * (th->control_stack_end-th->control_stack_start)) +
			  BINDING_STACK_SIZE+ALIEN_STACK_SIZE+dynamic_values_bytes+
			  32*SIGSTKSZ);
	} else 
	    prev=th;
	th=next;
    }
}

int interrupt_thread(pid_t pid, lispobj function)
{
    union sigval sigval;
    struct thread *th;
    sigval.sival_int=function;
    for_each_thread(th) 
	if((th->pid==pid) && (th->state != STATE_DEAD))
	    return sigqueue(pid, SIG_INTERRUPT_THREAD, sigval);
    errno=EPERM; return -1;
}

int signal_thread_to_dequeue (pid_t pid)
{
    return kill (pid, SIG_DEQUEUE);
}


/* stopping the world is a two-stage process.  From this thread we signal 
 * all the others with SIG_STOP_FOR_GC.  The handler for this signal does
 * the usual pseudo-atomic checks (we don't want to stop a thread while 
 * it's in the middle of allocation) then waits for another SIG_STOP_FOR_GC.
 */

void gc_stop_the_world()
{
#ifdef QSHOW_SIGNALS
    SHOW("gc_stop_the_world:begin");
#endif
    struct thread *p,*th=arch_os_get_current_thread();
    /* keep threads from starting while the world is stopped. */
    get_spinlock(&thread_start_lock,th->pid);
#ifdef QSHOW_SIGNALS
    SHOW("gc_stop_the_world:locked");
#endif
    /* stop all other threads by sending them SIG_STOP_FOR_GC */
    for(p=all_threads; p; p=p->next) {
        if((p!=th) && (p->pid!=0) && (p->state==STATE_RUNNING)) {
            p->state=STATE_STOPPING;
            if(kill(p->pid,SIG_STOP_FOR_GC)==-1) {
                /* we can't kill the process; assume because it
                 * died already (and its parent is dead so never
                 * saw the SIGCHLD) */
                p->state=STATE_DEAD;
            }
        }
    }
#ifdef QSHOW_SIGNALS
    SHOW("gc_stop_the_world:signals sent");
#endif
    /* wait for the running threads to stop */
    for(p=all_threads;p;) {
        if((p==th) || (p->pid==0) || (p->state==STATE_STARTING) ||
           (p->state==STATE_DEAD) || (p->state==STATE_STOPPED)) {
            p=p->next;
        }
    }
#ifdef QSHOW_SIGNALS
    SHOW("gc_stop_the_world:end");
#endif
}

void gc_start_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    /* if a resumed thread creates a new thread before we're done with
     * this loop, the new thread will get consed on the front of
     * all_threads, but it won't have been stopped so won't need
     * restarting; there can be threads just starting from before
     * gc_stop_the_world, though */
#ifdef QSHOW_SIGNALS
    SHOW("gc_start_the_world:begin");
#endif
    for(p=all_threads;p;p=p->next) {
	if((p!=th) && (p->pid!=0) && (p->state!=STATE_STARTING) &&
           (p->state!=STATE_DEAD)) {
            if(p->state!=STATE_STOPPED) {
                lose("gc_start_the_world: wrong thread state is %ld\n",
                     fixnum_value(p->state));
            }
            kill(p->pid,SIG_STOP_FOR_GC);
        }
    }
    /* we must wait for all threads to leave stopped state else we
     * risk signal accumulation and lose any meaning of
     * thread->state */
    for(p=all_threads;p;) {
        gc_assert(p->state!=STATE_STOPPING);
        if((p==th) || (p->pid==0) || (p->state!=STATE_STOPPED)) {
            p=p->next;
        }
    }
    release_spinlock(&thread_start_lock);
#ifdef QSHOW_SIGNALS
    SHOW("gc_start_the_world:end");
#endif
}
#endif
