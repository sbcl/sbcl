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
#include <stdlib.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>

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
#if defined GENCGC
#include "gencgc.h"
#endif
#include "core.h"
#include "save.h"
#include "lispregs.h"

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
		    core = copied_string(argv[argi]);
		    if (argi >= argc) {
			lose("missing filename for --core argument");
		    }
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
	    lookhere = (char *) calloc(strlen("/sbcl.core") + strlen(sbcl_home) + 1,
					sizeof(char));
	    sprintf(lookhere, "%s/sbcl.core", sbcl_home);
	    core = copied_existing_filename_or_null(lookhere);
	    free(lookhere);
	} else {
	    core = copied_existing_filename_or_null("/usr/lib/sbcl.core");
	    if (!core) {
		core = copied_existing_filename_or_null("/usr/local/lib/sbcl.core");
	    }
	}
	if (!core) {
	    lose("can't find core file");
	}
    }

    if (!noinform) {
	printf(
"This is SBCL " SBCL_VERSION_STRING ", an implementation of ANSI Common Lisp.\n\
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
");
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

#if 0
    os_init();
    gc_init();
    validate();
#endif
    globals_init();

    initial_function = load_core_file(core);
    if (initial_function == NIL) {
	lose("couldn't find initial function");
    }
    SHOW("freeing core");
    free(core);

#if defined GENCGC
    gencgc_pickup_dynamic();
#else
#endif

#ifdef BINDING_STACK_POINTER
    SetSymbolValue(BINDING_STACK_POINTER, BINDING_STACK_START);
#endif
#if defined INTERNAL_GC_TRIGGER && !defined __i386__
    SetSymbolValue(INTERNAL_GC_TRIGGER, make_fixnum(-1));
#endif

    interrupt_init();

    arch_install_interrupt_handlers();
    os_install_interrupt_handlers();

#ifdef PSEUDO_ATOMIC_ATOMIC
    /* Turn on pseudo atomic for when we call into Lisp. */
    SHOW("turning on pseudo atomic");
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(1));
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(0));
#endif

    /* Convert remaining argv values to something that Lisp can grok. */
    SHOW("setting POSIX-ARGV symbol value");
    SetSymbolValue(POSIX_ARGV, alloc_string_list(argv));

    /* Install a handler to pick off SIGINT until the Lisp system gets
     * far enough along to install its own handler. */
    sigint_init();

    FSHOW((stderr, "/funcalling initial_function=0x%lx\n", initial_function));
    funcall0(initial_function);

    /* initial_function() is not supposed to return. */
    lose("Lisp initial_function gave up control.");
    return 0; /* dummy value: return something */
}

