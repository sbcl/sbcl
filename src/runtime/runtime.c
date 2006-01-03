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

#include "sbcl.h"

#include <stdio.h>
#include <string.h>
#ifndef LISP_FEATURE_WIN32
#include <libgen.h>
#endif
#include <sys/types.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/wait.h>
#endif
#include <stdlib.h>
#include <unistd.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <signal.h>
#ifndef LISP_FEATURE_WIN32
#include <sched.h>
#endif
#include <errno.h>
#include <locale.h>

#if defined(SVR4) || defined(__linux__)
#include <time.h>
#endif

#include "signal.h"

#include "runtime.h"
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

#include "genesis/static-symbols.h"
#include "genesis/symbol.h"


#ifdef irix
#include <string.h>
#include "interr.h"
#endif

#ifndef SBCL_HOME
#define SBCL_HOME "/usr/local/lib/sbcl/"
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
        lose("malloc failure\n");
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
 * argv or envp) into a Lisp list of Lisp base-strings. */
static lispobj
alloc_base_string_list(char *array_ptr[])
{
    if (*array_ptr) {
        return alloc_cons(alloc_base_string(*array_ptr),
                          alloc_base_string_list(1 + array_ptr));
    } else {
        return NIL;
    }
}

/* miscellaneous chattiness */

void
print_help()
{
    puts(
"SBCL is a Common Lisp programming environment. Ordinarily you shouldn't\n\
need command line options when you invoke it interactively: you can just\n\
start it and work with the customary Lisp READ-EVAL-PRINT loop.\n\
\n\
One option idiom which is sometimes useful interactively (e.g. when\n\
exercising a test case for a bug report) is\n\
  sbcl --sysinit /dev/null --userinit /dev/null\n\
to keep SBCL from reading any initialization files at startup. And some\n\
people like to suppress the default startup message:\n\
  sbcl --noinform\n\
\n\
Other options can be useful when you're running SBCL noninteractively,\n\
e.g. from a script, or if you have a strange system configuration, so\n\
that SBCL can't by default find one of the files it needs. For\n\
information on such options, see the sbcl(1) man page.\n\
\n\
More information on SBCL can be found on its man page, or at\n\
<http://sbcl.sf.net/>.\n");
}

void
print_version()
{
    printf("SBCL %s\n", SBCL_VERSION_STRING);
}

void
print_banner()
{
    printf(
"This is SBCL %s, an implementation of ANSI Common Lisp.\n\
More information about SBCL is available at <http://www.sbcl.org/>.\n\
\n\
SBCL is free software, provided as is, with absolutely no warranty.\n\
It is mostly in the public domain; some portions are provided under\n\
BSD-style licenses.  See the CREDITS and COPYING files in the\n\
distribution for more information.\n\
", SBCL_VERSION_STRING);
}


int
main(int argc, char *argv[], char *envp[])
{
#ifdef LISP_FEATURE_WIN32
    /* Exception handling support structure. Evil Win32 hack. */
    struct lisp_exception_frame exception_frame;
#endif

    /* the name of the core file we're to execute. Note that this is
     * a malloc'ed string which should be freed eventually. */
    char *core = 0;
    char **sbcl_argv = 0;

    /* other command line options */
    boolean noinform = 0;
    boolean end_runtime_options = 0;

    lispobj initial_function;

    interrupt_init();
    block_blockable_signals();

    setlocale(LC_ALL, "");

    /* KLUDGE: os_vm_page_size is set by os_init(), and on some
     * systems (e.g. Alpha) arch_init() needs need os_vm_page_size, so
     * it must follow os_init(). -- WHN 2000-01-26 */
    os_init(argv, envp);
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
                    lose("more than one core file specified\n");
                } else {
                    ++argi;
                    if (argi >= argc) {
                        lose("missing filename for --core argument\n");
                    }
                    core = copied_string(argv[argi]);
                    ++argi;
                }
            } else if (0 == strcmp(arg, "--help")) {
                /* I think this is the (or a) usual convention: upon
                 * seeing "--help" we immediately print our help
                 * string and exit, ignoring everything else. */
                print_help();
                exit(0);
            } else if (0 == strcmp(arg, "--version")) {
                /* As in "--help" case, I think this is expected. */
                print_version();
                exit(0);
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
            /* (argc - argi) for the arguments, one for the binary,
               and one for the terminating NULL. */
            sbcl_argv = successful_malloc((2 + argc - argi) * sizeof(char *));
            sbcl_argv[0] = argv[0];
            while (argi < argc) {
                char *arg = argv[argi++];
                /* If we encounter --end-runtime-options for the first
                 * time after the point where we had to give up on
                 * runtime options, then the point where we had to
                 * give up on runtime options must've been a user
                 * error. */
                if (!end_runtime_options &&
                    0 == strcmp(arg, "--end-runtime-options")) {
                    lose("bad runtime option \"%s\"\n", argi0);
                }
                sbcl_argv[argj++] = arg;
            }
            sbcl_argv[argj] = 0;
        }
    }

    /* If no core file was specified, look for one. */
    if (!core) {
        char *sbcl_home = getenv("SBCL_HOME");
        char *lookhere;
        char *stem = "/sbcl.core";
        if(!sbcl_home) sbcl_home = SBCL_HOME;
        lookhere = (char *) calloc(strlen(sbcl_home) +
                                   strlen(stem) +
                                   1,
                                   sizeof(char));
        sprintf(lookhere, "%s%s", sbcl_home, stem);
        core = copied_existing_filename_or_null(lookhere);
        free(lookhere);
        if (!core) {
            lose("can't find core file\n");
        }
    }
    /* Make sure that SBCL_HOME is set, no matter where the core was
     * found */
    if (!getenv("SBCL_HOME")) {
        char *envstring, *copied_core, *dir;
        char *stem = "SBCL_HOME=";
        copied_core = copied_string(core);
#ifndef LISP_FEATURE_WIN32
        dir = dirname(copied_core);
#else /* LISP_FEATURE_WIN32 */
        dir = "";
#endif
        envstring = (char *) calloc(strlen(stem) +
                                    strlen(dir) +
                                    1,
                                    sizeof(char));
        sprintf(envstring, "%s%s", stem, dir);
        putenv(envstring);
        free(copied_core);
    }

    if (!noinform) {
        print_banner();
        fflush(stdout);
    }

#if defined(SVR4) || defined(__linux__)
    tzset();
#endif

    define_var("nil", NIL, 1);
    define_var("t", T, 1);

    set_lossage_handler(monitor_or_something);

    globals_init();

    initial_function = load_core_file(core);
    if (initial_function == NIL) {
        lose("couldn't find initial function\n");
    }
    SHOW("freeing core");
    free(core);

    gc_initialize_pointers();

    arch_install_interrupt_handlers();
#ifndef LISP_FEATURE_WIN32
    os_install_interrupt_handlers();
#else
/*     wos_install_interrupt_handlers(handler); */
    wos_install_interrupt_handlers(&exception_frame);
#endif

    /* Convert remaining argv values to something that Lisp can grok. */
    SHOW("setting POSIX-ARGV symbol value");
    SetSymbolValue(POSIX_ARGV, alloc_base_string_list(sbcl_argv),0);
    free(sbcl_argv);

    FSHOW((stderr, "/funcalling initial_function=0x%lx\n",
          (unsigned long)initial_function));
#ifdef LISP_FEATURE_WIN32
    fprintf(stderr, "\n\
This is experimental prerelease support for the Windows platform: use\n\
at your own risk.  \"Your Kitten of Death awaits!\"\n");
    fflush(stdout);
    fflush(stderr);
#endif
    create_initial_thread(initial_function);
    lose("CATS.  CATS ARE NICE.\n");
    return 0;
}
