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

int
initial_thread_trampoline(struct thread *th)
{
    lispobj function;
    lispobj *args = NULL;
    function = th->unbound_marker;
    th->unbound_marker = UNBOUND_MARKER_WIDETAG;
    if(arch_os_thread_init(th)==0) return 1;

    if(!th->os_thread) lose("th->os_thread not set up right");
    th->state=STATE_RUNNING;
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    return call_into_lisp_first_time(function,args,0);
#else
    return funcall0(function);
#endif
}

#ifdef LISP_FEATURE_SB_THREAD
void mark_thread_dead(struct thread *th) {
    /* I hope it's safe for a thread to detach itself inside a 
     * cancellation cleanup */
    pthread_detach(th->os_thread);
    th->state=STATE_DEAD;
}

/* this is the first thing that clone() runs in the child (which is
 * why the silly calling convention).  Basically it calls the user's
 * requested lisp function after doing arch_os_thread_init and
 * whatever other bookkeeping needs to be done
 */
int
new_thread_trampoline(struct thread *th)
{
    lispobj function,ret;
    function = th->unbound_marker;
    th->unbound_marker = UNBOUND_MARKER_WIDETAG;
    if(arch_os_thread_init(th)==0) return 1;	

    /* wait here until our thread is linked into all_threads: see below */
    while(th->os_thread<1) sched_yield();
    pthread_cleanup_push(mark_thread_dead,th);
    th->state=STATE_RUNNING;
    ret = funcall0(function);
    pthread_cleanup_pop(1);
    return ret;
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
		       32*SIGSTKSZ
		       );
    if(!spaces) goto cleanup;
    per_thread=(union per_thread_data *)
	(spaces+
	 THREAD_CONTROL_STACK_SIZE+
	 BINDING_STACK_SIZE+
	 ALIEN_STACK_SIZE);

    th=&per_thread->thread;
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
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
	STATIC_TLS_INIT(PSEUDO_ATOMIC_ATOMIC,pseudo_atomic_atomic);
	STATIC_TLS_INIT(PSEUDO_ATOMIC_INTERRUPTED,pseudo_atomic_interrupted);
#endif
#undef STATIC_TLS_INIT
#endif
    }

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
    th->state=STATE_STOPPED;
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

    th->interrupt_data=os_validate(0,(sizeof (struct interrupt_data)));
    if(all_threads) 
	memcpy(th->interrupt_data,
	       arch_os_get_current_thread()->interrupt_data,
	       sizeof (struct interrupt_data));
    else 
	memcpy(th->interrupt_data,global_interrupt_data,
	       sizeof (struct interrupt_data));

    th->unbound_marker=initial_function;
    return th;
 cleanup:
    /* if(th && th->tls_cookie>=0) os_free_tls_pointer(th); */
    if(spaces) os_invalidate(spaces,
			     THREAD_CONTROL_STACK_SIZE+BINDING_STACK_SIZE+
			     ALIEN_STACK_SIZE+dynamic_values_bytes);
    return 0;
}

void link_thread(struct thread *th,os_thread_t kid_pid)
{
    sigset_t newset,oldset;
    sigemptyset(&newset);
    sigaddset_blockable(&newset);
    pthread_sigmask(SIG_BLOCK, &newset, &oldset); 

    get_spinlock(&all_threads_lock,kid_pid);
    th->next=all_threads;
    all_threads=th;
    /* note that th->os_thread is 0 at this time.  We rely on
     * all_threads_lock to ensure that we don't have >1 thread with
     * os_thread=0 on the list at once
     */
    protect_control_stack_guard_page(th->os_thread,1);
    release_spinlock(&all_threads_lock);

    pthread_sigmask(SIG_SETMASK,&oldset,0);
    th->os_thread=kid_pid;		/* child will not start until this is set */
}

void create_initial_thread(lispobj initial_function) {
    struct thread *th=create_thread_struct(initial_function);
    os_thread_t kid_pid=thread_self();
    if(th && kid_pid>0) {
	link_thread(th,kid_pid);
	initial_thread_trampoline(all_threads); /* no return */
    } else lose("can't create initial thread");
}


os_thread_t create_thread(lispobj initial_function) {
    struct thread *th=create_thread_struct(initial_function);
    os_thread_t kid_pid=0;
#ifndef USE_LINUX_CLONE
     pthread_attr_t attr;
#endif

    if(th==0) return 0;
#ifdef USE_LINUX_CLONE
    kid_pid=clone(new_thread_trampoline,
		  (((void*)th->control_stack_start)+
		   THREAD_CONTROL_STACK_SIZE-16),
		  CLONE_FILES|SIG_THREAD_EXIT|CLONE_VM,th);
#else
     if((pthread_attr_init(&attr)) ||
	(pthread_attr_setstack(&attr,th->control_stack_start,
			       THREAD_CONTROL_STACK_SIZE-16)) ||
	(pthread_create(&kid_pid,&attr,new_thread_trampoline,th)))
	 kid_pid=0;
#endif
    if(kid_pid>0) {
	link_thread(th,kid_pid);
	return th->os_thread;
    } else {
	os_invalidate((os_vm_address_t) th->control_stack_start,
		      ((sizeof (lispobj))
		       * (th->control_stack_end-th->control_stack_start)) +
		      BINDING_STACK_SIZE+ALIEN_STACK_SIZE+dynamic_values_bytes+
		      32*SIGSTKSZ);
	return 0;
    }
}


struct thread *find_thread_by_os_thread(os_thread_t pid) 
{
    struct thread *th;
    for_each_thread(th)
	if(th->os_thread==pid) return th;
    return 0;
}

#if defined LISP_FEATURE_SB_THREAD
/* This is not needed unless #+SB-THREAD, as there's a trivial null
 * unithread definition. */
void mark_dead_threads() 
{
#ifdef USE_LINUX_CLONE
    os_thread_t kid;
    int status;
    while(1) {
	kid=waitpid(-1,&status,__WALL|WNOHANG);
	if(kid<=0) break;
	if(WIFEXITED(status) || WIFSIGNALED(status)) {
	    struct thread *th=find_thread_by_pid(kid);
	    if(th) th->state=STATE_DEAD;
	}
    }
#endif
    /* no cleanup needed for pthreads: the thread marks itself
     * dead in a cancellation cleanup handler */
}

void reap_dead_threads() 
{
    struct thread *th,*next,*prev=0;
    th=all_threads;
    while(th) {
	next=th->next;
	if(th->state==STATE_DEAD) {
	    funcall1(SymbolFunction(HANDLE_THREAD_EXIT),make_fixnum(th->os_thread));
#ifdef LISP_FEATURE_GENCGC
	    gc_alloc_update_page_tables(0, &th->alloc_region);
#endif
	    get_spinlock(&all_threads_lock,th->os_thread);
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
/* These are not needed unless #+SB-THREAD, and since sigwaitinfo()
 * doesn't seem to be easily available everywhere (OpenBSD...) it's
 * more trouble than it's worth to compile it when not needed. */

/* These won't be any use for pthreads: if we can't use pthread sync
   things somehow, we need to find another way to do these (or make
   our requirement "pthreads + futex"...).
*/

void block_sigcont(void)
{
    /* don't allow ourselves to receive SIGCONT while we're in the
     * "ambiguous" state of being on the queue but not actually stopped.
     */
    sigset_t newset;
    sigemptyset(&newset);
    sigaddset(&newset,SIG_DEQUEUE);
    pthread_sigmask(SIG_BLOCK, &newset, 0); 
}

void unblock_sigcont_and_sleep(void)
{
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set,SIG_DEQUEUE);
    do {
	errno=0;
	sigwaitinfo(&set,0);
    }while(errno==EINTR);
    pthread_sigmask(SIG_UNBLOCK,&set,0);
}


int interrupt_thread(os_thread_t pid, lispobj function)
{
    struct thread *th;
#ifdef USE_LINUX_CLONE
    union sigval sigval;
    sigval.sival_int=function;
#endif
    for_each_thread(th) 
	if((th->os_thread==pid) && (th->state != STATE_DEAD)) {
#ifdef USE_LINUX_CLONE	    
	    return sigqueue(pid, SIG_INTERRUPT_THREAD, sigval);
#else
	    /* In clone_threads, if A and B both interrupt C at approximately 
	     * the same time, it does not matter: the second signal will be
	     * masked until the handler has returned from the first one.
	     * In pthreads though, we can't put the knowledge of what function
	     * to call into the siginfo, so we have to store it in the 
	     * destination thread, and do it in such a way that A won't 
	     * clobber B's interrupt.  Hence this stupid linked list.
	     *
	     * This does depend on SIG_INTERRUPT_THREAD being queued
	     * (as POSIX RT signals are): we need to keep
	     * interrupt_fun data for exactly as many signals as are
	     * going to be received by the destination thread.
	     */
	    struct cons * c;
	    get_spinlock(&th->interrupt_fun_lock,arch_os_get_current_thread());
	    c=alloc_cons(function,th->interrupt_fun);
	    th->interrupt_fun=c;
	    release_spinlock(&th->interrupt_fun_lock);
	    return kill_thread(th->os_thread,SIG_INTERRUPT_THREAD);
#endif
	}
    errno=EPERM; return -1;
}

int signal_thread_to_dequeue (os_thread_t pid)
{
#ifdef USE_LINUX_CLONE
    return kill (pid, SIG_DEQUEUE);
#else
    return pthread_kill(pid, SIG_DEQUEUE);
#endif

}


/* stopping the world is a two-stage process.  From this thread we signal 
 * all the others with SIG_STOP_FOR_GC.  The handler for this signal does
 * the usual pseudo-atomic checks (we don't want to stop a thread while 
 * it's in the middle of allocation) then kills _itself_ with SIGSTOP.
 */

void gc_stop_the_world()
{
    /* stop all other threads by sending them SIG_STOP_FOR_GC */
    struct thread *p,*th=arch_os_get_current_thread();
    os_thread_t old_pid;
    int finished;
    do {
	finished=1;
	for(p=all_threads,old_pid=p->os_thread; p; p=p->next) {
	    if(p==th) continue;
	    if(p->state==STATE_RUNNING) {
		p->state=STATE_STOPPING;
		if(kill_thread(p->os_thread,SIG_STOP_FOR_GC)==-1) {
		    /* we can't kill the process; assume because it
		     * died already (and its parent is dead so never
		     * saw the SIGCHLD) */
		    p->state=STATE_DEAD;
		}
	    }
	    if((p->state!=STATE_STOPPED) &&
	       (p->state!=STATE_DEAD)) {
		finished=0;
	    }
	}
	if(old_pid!=all_threads->os_thread) {
	    finished=0;
	}
    } while(!finished);
}

void gc_start_the_world()
{
    struct thread *p,*th=arch_os_get_current_thread();
    /* if a resumed thread creates a new thread before we're done with
     * this loop, the new thread will get consed on the front of *
     * all_threads_lock, but it won't have been stopped so won't need
     * restarting */
    for(p=all_threads;p;p=p->next) {
	if((p==th) || (p->state==STATE_DEAD)) continue;
	p->state=STATE_RUNNING;
	kill_thread(p->os_thread,SIG_STOP_FOR_GC);
    }
}
#endif /* sb_thread */
