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
#include "os.h"
#include "globals.h"
#define ALIEN_STACK_SIZE (1*1024*1024) /* 1Mb size chosen at random */
#if 0
#define THREAD_CONTROL_STACK_SIZE (2*1024*1024)	/* must be 2^n */
#endif
int dynamic_values_bytes=256*sizeof(lispobj);	/* same for all threads */
struct thread *all_threads;

struct thread *init_thread(lispobj initial_function) {
    /* XXX This function or some of it needs to lock all_threads
     */

    struct thread *th=calloc(sizeof(struct thread),1);
    void *spaces=0;

    /* may as well allocate all of thse at once: it saves us from
     * having to decide what to do if only some of the allocations
     * succeed */
       
    spaces=os_validate(0,
		       THREAD_CONTROL_STACK_SIZE+BINDING_STACK_SIZE+
		       ALIEN_STACK_SIZE+dynamic_values_bytes);
    if(!spaces) goto cleanup;
    th->control_stack_start = spaces;
    th->binding_stack_start=
	(lispobj*)((void*)th->control_stack_start+THREAD_CONTROL_STACK_SIZE);
    th->alien_stack_start=
	(lispobj*)((void*)th->binding_stack_start+BINDING_STACK_SIZE);
    th->dynamic_values_start=
	(lispobj*)((void*)th->alien_stack_start+ALIEN_STACK_SIZE);
    if(all_threads) {
	memcpy(th->dynamic_values_start,
	       arch_os_get_current_thread()->dynamic_values_start,
	       dynamic_values_bytes);
    } else {
	int i;
	for(i=0;i<(dynamic_values_bytes/sizeof(lispobj));i++)
	    th->dynamic_values_start[i]=UNBOUND_MARKER_WIDETAG;
    }

    /* yes, really we are keeping raw machine words in these lisp
     * symbol value slots */

    /* obviously we'll have to do the dynamic-value-offset thing here
     * as soon as we want two threads to actually execute lisp at
     * once */
    ((struct symbol *)native_pointer(ALIEN_STACK))->value
	=LOW_WORD(th->dynamic_values_start-1);
#ifdef BINDING_STACK_POINTER
    SetSymbolValue(BINDING_STACK_POINTER, LOW_WORD(th->binding_stack_start),th);
#endif

    th->next=all_threads;

    th->tls_cookie=os_set_tls_pointer(th);
    if(th->tls_cookie<0) goto cleanup;

#if defined(LISP_FEATURE_X86) && defined (LISP_FEATURE_LINUX)
    *(th->binding_stack_start-1) = LOW_WORD(th->dynamic_values_start);
    th->pid=
	clone(funcall0,th->binding_stack_start-2,
	      ((getpid()!=parent_pid)?CLONE_PARENT:0)
	      |CLONE_SIGHAND|CLONE_VM,initial_function);
    fprintf(stderr,"child pid is %d\n",th->pid);
    if(!th->pid) goto cleanup;
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
    if(th) free(th);
    
    return 0;
}

struct thread *find_thread_by_pid(pid_t pid) 
{
    struct thread *th;
    for_each_thread(th)
	if(th->pid==pid) return th;
    return 0;
}
