/*
 * main() entry point for a stand-alone SBCL image
 */

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

#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <signal.h>
#include <sys/ptrace.h>
#include <sched.h>
#include <errno.h>

#if defined(SVR4) || defined(__linux__)
#include <time.h>
#endif

#include "signal.h"

#include "runtime.h"
#include "sbcl.h"
#include "alloc.h"
#include "vars.h"
#include "globals.h"
#include "os.h"
#include "interrupt.h"
#include "arch.h"
#include "gc.h"
#include "interr.h"
#include "monitor.h"
#include "validate.h"
#include "core.h"
#include "save.h"
#include "lispregs.h"
#include "thread.h"

#ifdef irix
#include <string.h>
#include "interr.h"
#endif

/* SIGINT handler that invokes the monitor (for when Lisp isn't up to it) */
static void
sigint_handler(int signal, siginfo_t *info, void *void_context)
{
    lose("\nSIGINT hit at 0x%08lX\n", 
	 (unsigned long) *os_context_pc_addr(void_context));
}

/* (This is not static, because we want to be able to call it from
 * Lisp land.) */
void
sigint_init(void)
{
    SHOW("entering sigint_init()");
    install_handler(SIGINT, sigint_handler);
    SHOW("leaving sigint_init()");
}

/*
 * helper functions for dealing with command line args
 */

void *
successful_malloc(size_t size)
{
    void* result = malloc(size);
    if (0 == result) {
	lose("malloc failure");
    } else {
	return result;
    }
    return (void *) NULL; /* dummy value: return something ... */
}

char *
copied_string(char *string)
{
    return strcpy(successful_malloc(1+strlen(string)), string);
}

char *
copied_existing_filename_or_null(char *filename)
{
    struct stat filename_stat;
    if (stat(filename, &filename_stat)) { /* if failure */
	return 0;
    } else {
        return copied_string(filename);
    }
}

/* Convert a null-terminated array of null-terminated strings (e.g.
 * argv or envp) into a Lisp list of Lisp strings. */
static lispobj
alloc_string_list(char *array_ptr[])
{
    if (*array_ptr) {
	return alloc_cons(alloc_string(*array_ptr),
			  alloc_string_list(1 + array_ptr));
    } else {
	return NIL;
    }
}

int gc_thread_pid;

int
main(int argc, char *argv[], char *envp[])
{
    /* the name of the core file we're to execute. Note that this is
     * a malloc'ed string which should be freed eventually. */
    char *core = 0;

    /* other command line options */
    boolean noinform = 0;
    boolean end_runtime_options = 0;

    lispobj initial_function;

    /* KLUDGE: os_vm_page_size is set by os_init(), and on some
     * systems (e.g. Alpha) arch_init() needs need os_vm_page_size, so
     * it must follow os_init(). -- WHN 2000-01-26 */
    os_init();
    arch_init();
    gc_init();
    validate();

    /* Parse our part of the command line (aka "runtime options"),
     * stripping out those options that we handle. */
    {
	int argi = 1;
	while (argi < argc) {
	    char *arg = argv[argi];
	    if (0 == strcmp(arg, "--noinform")) {
		noinform = 1;
		++argi;
	    } else if (0 == strcmp(arg, "--core")) {
		if (core) {
		    lose("more than one core file specified");
		} else {
		    ++argi;
		    if (argi >= argc) {
			lose("missing filename for --core argument");
		    }
		    core = copied_string(argv[argi]);
		    ++argi;
		}
	    } else if (0 == strcmp(arg, "--end-runtime-options")) {
		end_runtime_options = 1;
		++argi;
		break;
	    } else {
		/* This option was unrecognized as a runtime option,
		 * so it must be a toplevel option or a user option,
		 * so we must be past the end of the runtime option
		 * section. */
		break;
	    }
	}
	/* This is where we strip out those options that we handle. We
	 * also take this opportunity to make sure that we don't find
	 * an out-of-place "--end-runtime-options" option. */
	{
	    char *argi0 = argv[argi];
	    int argj = 1;
	    while (argi < argc) {
		char *arg = argv[argi++];
		/* If we encounter --end-runtime-options for the first
		 * time after the point where we had to give up on
		 * runtime options, then the point where we had to
		 * give up on runtime options must've been a user
		 * error. */
		if (!end_runtime_options &&
		    0 == strcmp(arg, "--end-runtime-options")) {
		    lose("bad runtime option \"%s\"", argi0);
		}
		argv[argj++] = arg;
	    }
	    argv[argj] = 0;
	    argc = argj;
	}
    }

    /* If no core file was specified, look for one. */
    if (!core) {
	char *sbcl_home = getenv("SBCL_HOME");
	if (sbcl_home) {
	    char *lookhere;
	    char *stem = "/sbcl.core";
	    lookhere = (char *) calloc(strlen(sbcl_home) +
				       strlen(stem) +
				       1,
				       sizeof(char));
	    sprintf(lookhere, "%s%s", sbcl_home, stem);
	    core = copied_existing_filename_or_null(lookhere);
	    free(lookhere);
	} else {
	    core = copied_existing_filename_or_null("/usr/lib/sbcl.core");
	    if (!core) {
		core =
		    copied_existing_filename_or_null("/usr/local/lib/sbcl.core");
	    }
	}
	if (!core) {
	    lose("can't find core file");
	}
    }

    if (!noinform) {
	printf(
"This is SBCL %s, an implementation of ANSI Common Lisp.\n\
\n\
SBCL is derived from the CMU CL system created at Carnegie Mellon University.\n\
Besides software and documentation originally created at Carnegie Mellon\n\
University, SBCL contains some software originally from the Massachusetts\n\
Institute of Technology, Symbolics Incorporated, and Xerox Corporation, and\n\
material contributed by volunteers since the release of CMU CL into the\n\
public domain. See the CREDITS file in the distribution for more information.\n\
\n\
SBCL is a free software system, provided as is, with absolutely no warranty.\n\
It is mostly in the public domain, but also includes some software copyrighted\n\
  Massachusetts Institute of Technology, 1986;\n\
  Symbolics, Inc., 1989, 1990, 1991, 1992; and\n\
  Xerox Corporation, 1985, 1986, 1987, 1988, 1989, 1990\n\
used under BSD-style licenses allowing copying only under certain conditions.\n\
See the COPYING file in the distribution for more information.\n\
\n\
More information about SBCL is available at <http://sbcl.sourceforge.net/>.\n\
", SBCL_VERSION_STRING);
	fflush(stdout);
    }

#ifdef MACH
    mach_init();
#endif
#if defined(SVR4) || defined(__linux__)
    tzset();
#endif

    define_var("nil", NIL, 1);
    define_var("t", T, 1);

    set_lossage_handler(monitor_or_something);

    globals_init();

    initial_function = load_core_file(core);
    if (initial_function == NIL) {
	lose("couldn't find initial function");
    }
    SHOW("freeing core");
    free(core);

    gc_initialize_pointers();

    interrupt_init();

    arch_install_interrupt_handlers();
    os_install_interrupt_handlers();


    /* Convert remaining argv values to something that Lisp can grok. */
    SHOW("setting POSIX-ARGV symbol value");
    SetSymbolValue(POSIX_ARGV, alloc_string_list(argv),0);

    /* Install a handler to pick off SIGINT until the Lisp system gets
     * far enough along to install its own handler. */
    sigint_init();

    FSHOW((stderr, "/funcalling initial_function=0x%lx\n", initial_function));
    create_thread(initial_function);
    /* first lisp thread should be the foregound process, not us */
    if(setpgid(all_threads->pid,0)==-1)
	perror("setpgid child");
    if(tcsetpgrp(0,all_threads->pid) == -1)
	perror("tcsetpgrp child");
 
    gc_thread_pid=getpid();
    parent_loop();
}

static void parent_sighandler(int signum) 
{
    fprintf(stderr,"parent thread got signal %d , maybe_gc_pending=%d\n",
	    signum, maybe_gc_pending);
}

static void parent_do_garbage_collect(void)
{    
    int waiting_threads=0;
    struct thread *th;
    int status,p;

    for_each_thread(th) {
	fprintf(stderr,"attaching to %d ...",th->pid); 
	if(ptrace(PTRACE_ATTACH,th->pid,0,0))
	    perror("PTRACE_ATTACH");
	else waiting_threads++;
    }
    stop_the_world=1;

    do {
	/* not sure if we have to wait for PTRACE_ATTACH to finish
	 * before we can send PTRACE_CONT, so let's play it safe
	 */
	while(waiting_threads>0) {
	    if((p=waitpid(-1,&status, WUNTRACED|__WALL))>0) {
		if(WIFEXITED(status) || WIFSIGNALED(status)) 
		    destroy_thread(find_thread_by_pid(p));
		else {
		    fprintf(stderr, "wait returned pid %d\n",p);
		    waiting_threads--;
		}
	    }
	}
	for_each_thread(th) {
	    if(SymbolTlValue(PSEUDO_ATOMIC_ATOMIC,th)) {
		/* restart the child, sending it a signal that will cause it 
		 * to go into interrupt_handle_pending as soon as it's
		 * finished being pseudo_atomic */
		fprintf(stderr, "%d was pseudo-atomic, letting it resume\n",
			th->pid);
		if(ptrace(PTRACE_CONT,th->pid,0,SIGCONT))
		    perror("PTRACE_CONT");
		waiting_threads++;
	    }
	}
    } while (waiting_threads>0);
		
    collect_garbage(maybe_gc_pending-1);
    maybe_gc_pending=0;
    stop_the_world=0;
    for_each_thread(th) 
	if(ptrace(PTRACE_DETACH,th->pid,0,0))
	    perror("PTRACE_DETACH");
}

static void /* noreturn */ parent_loop(void)
{
    struct sigaction sa;
    sigset_t sigset;
    sigemptyset(&sigset);

    sigemptyset(&sigset);
    sigaddset(&sigset, SIGALRM);
    sigaddset(&sigset, SIGCHLD);
    sigprocmask(SIG_UNBLOCK,&sigset,0);
    sa.sa_handler=parent_sighandler;
    sa.sa_mask=sigset;
    sa.sa_flags= SA_RESTART;
    sigaction(SIGALRM, &sa, 0);
    sigaction(SIGCHLD, &sa, 0);

    /* renounce sin, the world, and a controlling tty */
    if(setpgid(0,0)==-1) perror("setpgid parent");

    while(all_threads) {
	int status;
	pid_t pid=0;
	fprintf(stderr,"parent thread waiting for a signal\n"); 
	pause();
	if(maybe_gc_pending) {
	    /* someone asked for a garbage collection */
	    parent_do_garbage_collect();
	} 
	status=0;
	while(all_threads && (pid=waitpid(-1,&status,__WALL|WNOHANG))) {
	    struct thread *th;
	    if(WIFEXITED(status) || WIFSIGNALED(status)) {
		fprintf(stderr,"waitpid : child %d exited \n", pid);
		th=find_thread_by_pid(pid);
		/* FIXME lock all_threads */
		if(th) destroy_thread(th);
	    }else 
		fprintf(stderr,"waitpid : child %d stopped? %d %d\n", pid,
			WIFSTOPPED(status), WSTOPSIG(status));
	}
    }
}

