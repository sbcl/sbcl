#include <stdlib.h>
#include <stdio.h>
#include <sched.h>
#ifndef CLONE_PARENT		/* lameass glibc 2.2  doesn't define this */
#define CLONE_PARENT 0x00008000	/* even though the manpage documents it */
#endif
#include "runtime.h"
#include "sbcl.h"
#include "validate.h"		/* for CONTROL_STACK_SIZE etc */
#include "thread.h"
#include "arch.h"
#include "target-arch-os.h"
#include "os.h"
#include "globals.h"
#define ALIEN_STACK_SIZE (1*1024*1024) /* 1Mb size chosen at random */
int dynamic_values_bytes=4096*sizeof(lispobj);	/* same for all threads */
struct thread *all_threads;



/* this is the first thing that clone() runs in the child (which is
 * why the silly calling convention).  Basically it calls the user's
 * requested lisp function after doing arch_os_thread_init and
 * whatever other bookkeeping needs to be done
 */

/* set go to 0 to stop the thread before it starts.  Convenient if you
* want to attach a debugger to it before it does anything */

volatile int go=1;		
int
new_thread_trampoline(struct thread *th)
{
    lispobj function = th->unbound_marker;
    th->unbound_marker = UNBOUND_MARKER_WIDETAG;

    FSHOW((stderr, "/pausing 0x%lx(%d) before new_thread_trampoline(0x%lx)\n",
	   (unsigned long)th,getpid(),(unsigned long)function));
    if(go==0) {
	while(go==0) ;
	FSHOW((stderr, "/continue\n"));
    }
    if(arch_os_thread_init(th)==0) 
	return 1;		/* failure.  no, really */
    else 
	return funcall0(function);
}

/* this is called from any other thread to create the new one, and
 * initialize all parts of it that can be initialized from another 
 * thread 
 */

struct thread *create_thread(lispobj initial_function) {
    /* XXX This function or some of it needs to lock all_threads
     */
    lispobj trampoline_argv[2];
    union per_thread_data *per_thread;
    struct thread *th=0;	/*  subdue gcc */
    void *spaces=0;

    /* may as well allocate all the spaces at once: it saves us from
     * having to decide what to do if only some of the allocations
     * succeed */
    spaces=os_validate(0,
		       THREAD_CONTROL_STACK_SIZE+BINDING_STACK_SIZE+
		       ALIEN_STACK_SIZE+dynamic_values_bytes);
    if(!spaces) goto cleanup;
    per_thread=(union per_thread_data *)
	(spaces+THREAD_CONTROL_STACK_SIZE+BINDING_STACK_SIZE+ALIEN_STACK_SIZE);

    th=&per_thread->thread;
    if(all_threads) {
	memcpy(per_thread,arch_os_get_current_thread(),
	       dynamic_values_bytes);
    } else {
	int i;
	for(i=0;i<(dynamic_values_bytes/sizeof(lispobj));i++)
	    per_thread->dynamic_values[i]=UNBOUND_MARKER_WIDETAG;
	if(SymbolValue(FREE_TLS_INDEX,0)==UNBOUND_MARKER_WIDETAG) 
	    SetSymbolValue(FREE_TLS_INDEX,
			   make_fixnum(sizeof(struct thread)/sizeof(lispobj)),
			   0);
	((struct symbol *)(BINDING_STACK_START-OTHER_POINTER_LOWTAG))
	    ->tls_index=make_fixnum(1);
	((struct symbol *)(BINDING_STACK_POINTER-OTHER_POINTER_LOWTAG))
	    ->tls_index=make_fixnum(2);
	((struct symbol *)(CONTROL_STACK_START-OTHER_POINTER_LOWTAG))
	    ->tls_index=make_fixnum(3);
	((struct symbol *)(ALIEN_STACK-OTHER_POINTER_LOWTAG))
	    ->tls_index=make_fixnum(5);
    }

    th->control_stack_start = spaces;
    th->binding_stack_start=
	(lispobj*)((void*)th->control_stack_start+THREAD_CONTROL_STACK_SIZE);
    th->alien_stack_start=
	(lispobj*)((void*)th->binding_stack_start+BINDING_STACK_SIZE);
    th->binding_stack_pointer=th->binding_stack_start;
    th->alien_stack_pointer=((void *)th->alien_stack_start
			     + ALIEN_STACK_SIZE-4); /* naked 4.  FIXME */
    gc_set_region_empty(&th->alloc_region);
    
    bind_variable(CURRENT_CATCH_BLOCK,make_fixnum(0),th);
    bind_variable(CURRENT_UNWIND_PROTECT_BLOCK,make_fixnum(0),th); 
    bind_variable(PSEUDO_ATOMIC_ATOMIC,make_fixnum(0),th);
    bind_variable(PSEUDO_ATOMIC_INTERRUPTED,make_fixnum(0),th);
    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX,make_fixnum(0),th);
    bind_variable(INTERRUPT_PENDING, NIL,th);
    bind_variable(INTERRUPTS_ENABLED,T,th);

    th->next=all_threads;
#if defined(LISP_FEATURE_X86) && defined (LISP_FEATURE_LINUX)

    th->unbound_marker=initial_function;
    th->pid=
	clone(new_thread_trampoline,th->binding_stack_start-2,
	      (((getpid()!=parent_pid)?CLONE_PARENT:0)
	       |CLONE_SIGHAND|CLONE_VM),th);
    fprintf(stderr,"child pid is %d\n",th->pid);
    if(th->pid<=0) goto cleanup;
#else
#error this stuff presently only works on x86 Linux
#endif
    all_threads=th;
    return th;
 cleanup:
    /* if(th && th->tls_cookie>=0) os_free_tls_pointer(th); */
    if(spaces) os_invalidate(spaces,
			     THREAD_CONTROL_STACK_SIZE+BINDING_STACK_SIZE+
			     ALIEN_STACK_SIZE+dynamic_values_bytes);
    return 0;
}


struct thread *find_thread_by_pid(pid_t pid) 
{
    struct thread *th;
    for_each_thread(th)
	if(th->pid==pid) return th;
    return 0;
}
