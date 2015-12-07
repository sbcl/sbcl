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
#include <ctype.h>
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
#include "runtime.h"
#ifndef LISP_FEATURE_WIN32
#include <sched.h>
#endif
#include <errno.h>
#include <locale.h>
#include <limits.h>

#if defined(SVR4) || defined(__linux__)
#include <time.h>
#endif

#if !(defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD))
#include "signal.h"
#endif

#include "runtime.h"
#include "vars.h"
#include "globals.h"
#include "os.h"
#include "interr.h"
#include "alloc.h"
#include "interrupt.h"
#include "arch.h"
#include "gc.h"
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
#define SBCL_HOME SBCL_PREFIX"/lib/sbcl/"
#endif

#ifdef LISP_FEATURE_HPUX
extern void *return_from_lisp_stub;
#include "genesis/closure.h"
#include "genesis/simple-fun.h"
#endif


/* SIGINT handler that invokes the monitor (for when Lisp isn't up to it) */
static void
sigint_handler(int signal, siginfo_t *info, os_context_t *context)
{
    lose("\nSIGINT hit at 0x%08lX\n",
         (unsigned long) *os_context_pc_addr(context));
}

/* (This is not static, because we want to be able to call it from
 * Lisp land.) */
void
sigint_init(void)
{
    SHOW("entering sigint_init()");
    install_handler(SIGINT, sigint_handler, 1);
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

#ifndef LISP_FEATURE_WIN32
char *
copied_realpath(const char *pathname)
{
    char *messy, *tidy;
    size_t len;

    /* realpath() supposedly can't be counted on to always return
     * an absolute path, so we prepend the cwd to relative paths */
    messy = NULL;
    if (pathname[0] != '/') {
        messy = successful_malloc(PATH_MAX + 1);
        if (getcwd(messy, PATH_MAX + 1) == NULL) {
            free(messy);
            return NULL;
        }
        len = strlen(messy);
        snprintf(messy + len, PATH_MAX + 1 - len, "/%s", pathname);
    }

    tidy = successful_malloc(PATH_MAX + 1);
    if (realpath((messy ? messy : pathname), tidy) == NULL) {
        free(messy);
        free(tidy);
        return NULL;
    }

    return tidy;
}
#endif /* LISP_FEATURE_WIN32 */

/* miscellaneous chattiness */

void
print_help()
{
    puts(
"Usage: sbcl [runtime-options] [toplevel-options] [user-options]\n\
Common runtime options:\n\
  --help                     Print this message and exit.\n\
  --version                  Print version information and exit.\n\
  --core <filename>          Use the specified core file instead of the default.\n\
  --dynamic-space-size <MiB> Size of reserved dynamic space in megabytes.\n\
  --control-stack-size <MiB> Size of reserved control stack in megabytes.\n\
\n\
Common toplevel options:\n\
  --sysinit <filename>       System-wide init-file to use instead of default.\n\
  --userinit <filename>      Per-user init-file to use instead of default.\n\
  --no-sysinit               Inhibit processing of any system-wide init-file.\n\
  --no-userinit              Inhibit processing of any per-user init-file.\n\
  --disable-debugger         Invoke sb-ext:disable-debugger.\n\
  --noprint                  Run a Read-Eval Loop without printing results.\n\
  --script [<filename>]      Skip #! line, disable debugger, avoid verbosity.\n\
  --quit                     Exit with code 0 after option processing.\n\
  --non-interactive          Sets both --quit and --disable-debugger.\n\
Common toplevel options that are processed in order:\n\
  --eval <form>              Form to eval when processing this option.\n\
  --load <filename>          File to load when processing this option.\n\
\n\
User options are not processed by SBCL. All runtime options must\n\
appear before toplevel options, and all toplevel options must\n\
appear before user options.\n\
\n\
For more information please refer to the SBCL User Manual, which\n\
should be installed along with SBCL, and is also available from the\n\
website <http://www.sbcl.org/>.\n");
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
"
#ifdef LISP_FEATURE_WIN32
"\n\
WARNING: the Windows port is fragile, particularly for multithreaded\n\
code.  Unfortunately, the development team currently lacks the time\n\
and resources this platform demands.\n\
"
#endif
, SBCL_VERSION_STRING);
}

/* Look for a core file to load, first in the directory named by the
 * SBCL_HOME environment variable, then in a hardcoded default
 * location.  Returns a malloced copy of the core filename. */
char *
search_for_core ()
{
    char *sbcl_home = getenv("SBCL_HOME");
    char *lookhere;
    char *stem = "/sbcl.core";
    char *core;

    if (!(sbcl_home && *sbcl_home)) sbcl_home = SBCL_HOME;
    lookhere = (char *) calloc(strlen(sbcl_home) +
                               strlen(stem) +
                               1,
                               sizeof(char));
    sprintf(lookhere, "%s%s", sbcl_home, stem);
    core = copied_existing_filename_or_null(lookhere);

    if (!core) {
        lose("can't find core file at %s\n", lookhere);
    }

    free(lookhere);

    return core;
}

/* Try to find the path to an executable from argv[0], this is only
 * used when os_get_runtime_executable_path() returns NULL */
#ifdef LISP_FEATURE_WIN32
char *
search_for_executable(const char *argv0)
{
    return NULL;
}
#else /* LISP_FEATURE_WIN32 */
char *
search_for_executable(const char *argv0)
{
    char *search, *start, *end, *buf;

    /* If argv[0] contains a slash then it's probably an absolute path
     * or relative to the current directory, so check if it exists. */
    if (strchr(argv0, '/') != NULL && access(argv0, F_OK) == 0)
        return copied_realpath(argv0);

    /* Bail on an absolute path which doesn't exist */
    if (argv0[0] == '/')
        return NULL;

    /* Otherwise check if argv[0] exists relative to any directory in PATH */
    search = getenv("PATH");
    if (search == NULL)
        return NULL;
    search = copied_string(search);
    buf = successful_malloc(PATH_MAX + 1);
    for (start = search; (end = strchr(start, ':')) != NULL; start = end + 1) {
        *end = '\0';
        snprintf(buf, PATH_MAX + 1, "%s/%s", start, argv0);
        if (access(buf, F_OK) == 0) {
            free(search);
            search = copied_realpath(buf);
            free(buf);
            return search;
        }
    }
    /* The above for-loop fails to process the last part of PATH if PATH does
     * not end with ':'. We may consider appending an extra ':' to the end of
     * SEARCH.  -- houjingyi 2013-05-24 */
    if (start != NULL && *start != '\0') {
        snprintf(buf, PATH_MAX + 1, "%s/%s", start, argv0);
        if (access(buf, F_OK) == 0) {
            free(search);
            search = copied_realpath(buf);
            free(buf);
            return search;
        }
    }

    free(search);
    free(buf);
    return NULL;
}
#endif /* LISP_FEATURE_WIN32 */

size_t
parse_size_arg(char *arg, char *arg_name)
{
  char *tail, *power_name;
  size_t power, res;

  res = strtoul(arg, &tail, 0);

  if (arg == tail) {
    lose("%s argument is not a number: %s", arg_name, arg);
  } else if (tail[0]) {
    int i, size;
    power_name = copied_string(tail);
    size = strlen(power_name);
    for (i=0; i<size; i++)
      power_name[i] = toupper(power_name[i]);
  } else {
    power = 20;
    power_name = NULL;
  }
  if (power_name) {
    if ((0==strcmp("KB", power_name)) ||
        (0==strcmp("KIB", power_name))) {
      power = 10;
    } else if ((0==strcmp("MB", power_name)) ||
               (0==strcmp("MIB", power_name))) {
      power = 20;
    } else if ((0==strcmp("GB", power_name)) ||
               (0==strcmp("GIB", power_name))) {
      power = 30;
    } else {
      lose("%s argument has an unknown suffix: %s", arg_name, tail);
    }
    free(power_name);
  }
  if ((res <= 0) ||
      (res >= (SIZE_MAX >> power))) {
    lose("%s argument is out of range: %s", arg_name, arg);
  }
  res <<= power;
  return res;
}

char **posix_argv;
char **full_posix_argv;
char *core_string;

struct runtime_options *runtime_options;

char *saved_runtime_path = NULL;
#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD)
void pthreads_win32_init();
#endif

void print_locale_variable(const char *name)
{
  char *value = getenv(name);

  if (value) {
    fprintf(stderr, "\n  %s=%s", name, value);
  }
}

void setup_locale()
{
  if(setlocale(LC_ALL, "") == NULL) {
#ifndef LISP_FEATURE_WIN32

    fprintf(stderr, "WARNING: Setting locale failed.\n");
    fprintf(stderr, "  Check the following variables for correct values:");

    if (setlocale(LC_CTYPE, "") == NULL) {
      print_locale_variable("LC_ALL");
      print_locale_variable("LC_CTYPE");
      print_locale_variable("LANG");
    }

    if (setlocale(LC_MESSAGES, "") == NULL) {
      print_locale_variable("LC_MESSAGES");
    }
    if (setlocale(LC_COLLATE, "") == NULL) {
      print_locale_variable("LC_COLLATE");
    }
    if (setlocale(LC_MONETARY, "") == NULL) {
      print_locale_variable("LC_MONETARY");
    }
    if (setlocale(LC_NUMERIC, "") == NULL) {
      print_locale_variable("LC_NUMERIC");
    }
    if (setlocale(LC_TIME, "") == NULL) {
      print_locale_variable("LC_TIME");
    }
    fprintf(stderr, "\n");

#endif
  }
}
void print_environment(int argc, char *argv[])
{
    int n = 0;
    printf("; Commandline arguments:\n");
    while (n < argc) {
        printf(";  %2d: \"%s\"\n", n, argv[n]);
        ++n;
    }
    n = 0;
    printf(";\n; Environment:\n");
    while (ENVIRON[n]) {
        printf(";  %2d: \"%s\"\n", n, ENVIRON[n]);
        ++n;
    }
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
    os_vm_offset_t embedded_core_offset = 0;
    char *runtime_path = 0;

    /* other command line options */
    boolean noinform = 0;
    boolean end_runtime_options = 0;
    boolean disable_lossage_handler_p = 0;
    boolean debug_environment_p = 0;

    lispobj initial_function;
    const char *sbcl_home = getenv("SBCL_HOME");

#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD)
    os_preinit();
    pthreads_win32_init();
#endif

    interrupt_init();
    block_blockable_signals(0, 0);

    runtime_options = NULL;

    /* Save the argv[0] derived runtime path in case
     * os_get_runtime_executable_path(1) isn't able to get an
     * externally-usable path later on. */
    saved_runtime_path = search_for_executable(argv[0]);

    /* Check early to see if this executable has an embedded core,
     * which also populates runtime_options if the core has runtime
     * options */
    runtime_path = os_get_runtime_executable_path(0);
    if (runtime_path || saved_runtime_path) {
        os_vm_offset_t offset = search_for_embedded_core(
            runtime_path ? runtime_path : saved_runtime_path);
        if (offset != -1) {
            embedded_core_offset = offset;
            core = (runtime_path ? runtime_path :
                    copied_string(saved_runtime_path));
        } else {
            if (runtime_path)
                free(runtime_path);
        }
    }


    /* Parse our part of the command line (aka "runtime options"),
     * stripping out those options that we handle. */
    if (runtime_options != NULL) {
        dynamic_space_size = runtime_options->dynamic_space_size;
        thread_control_stack_size = runtime_options->thread_control_stack_size;
        full_posix_argv = sbcl_argv = argv;
    } else {
        int argi = 1;

        runtime_options = successful_malloc(sizeof(struct runtime_options));
        full_posix_argv = argv;

        while (argi < argc) {
            char *arg = argv[argi];
            if (0 == strcmp(arg, "--script")) {
                /* This is both a runtime and a toplevel option. As a
                 * runtime option, it is equivalent to --noinform.
                 * This exits, and does not increment argi, so that
                 * TOPLEVEL-INIT sees the option. */
                noinform = 1;
                end_runtime_options = 1;
                disable_lossage_handler_p = 1;
                lose_on_corruption_p = 1;
                break;
            } else if (0 == strcmp(arg, "--noinform")) {
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
            } else if (0 == strcmp(arg, "--dynamic-space-size")) {
                ++argi;
                if (argi >= argc)
                    lose("missing argument for --dynamic-space-size");
                  dynamic_space_size = parse_size_arg(argv[argi++],
                                                      "--dynamic-space-size");
#               ifdef MAX_DYNAMIC_SPACE_END
                if (!((DYNAMIC_SPACE_START <
                       DYNAMIC_SPACE_START+dynamic_space_size) &&
                      (DYNAMIC_SPACE_START+dynamic_space_size <=
                       MAX_DYNAMIC_SPACE_END))) {
                  char* suffix = "";
                  char* size = argv[argi-1];
                  if (!strchr(size, 'B') && !strchr(size, 'b')) {
                    suffix = " [MB]";
                  }
                  lose("--dynamic-space-size argument %s%s is too large, max %lu KB",
                       size, suffix,
                       (MAX_DYNAMIC_SPACE_END-DYNAMIC_SPACE_START) / 1024);
                }
#               endif
            } else if (0 == strcmp(arg, "--control-stack-size")) {
                ++argi;
                if (argi >= argc)
                    lose("missing argument for --control-stack-size");
                errno = 0;
                thread_control_stack_size = parse_size_arg(argv[argi++], "--control-stack-size");
            } else if (0 == strcmp(arg, "--debug-environment")) {
                debug_environment_p = 1;
                ++argi;
            } else if (0 == strcmp(arg, "--disable-ldb")) {
                disable_lossage_handler_p = 1;
                ++argi;
            } else if (0 == strcmp(arg, "--lose-on-corruption")) {
                lose_on_corruption_p = 1;
                ++argi;
            } else if (0 == strcmp(arg, "--end-runtime-options")) {
                end_runtime_options = 1;
                ++argi;
                break;
            } else if (0 == strcmp(arg, "--merge-core-pages")) {
                ++argi;
                merge_core_pages = 1;
            } else if (0 == strcmp(arg, "--no-merge-core-pages")) {
                ++argi;
                merge_core_pages = 0;
            } else if (0 == strcmp(arg, "--default-merge-core-pages")) {
                ++argi;
                merge_core_pages = -1;
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

    /* Align down to multiple of page_table page size, and to the appropriate
     * stack alignment. */
    dynamic_space_size &= ~(sword_t)(PAGE_BYTES-1);
#ifdef LISP_FEATURE_GENCGC
    dynamic_space_size &= ~(sword_t)(GENCGC_CARD_BYTES-1);
#endif
    thread_control_stack_size &= ~(sword_t)(CONTROL_STACK_ALIGNMENT_BYTES-1);

    /* Preserve the runtime options for possible future core saving */
    runtime_options->dynamic_space_size = dynamic_space_size;
    runtime_options->thread_control_stack_size = thread_control_stack_size;

    /* KLUDGE: os_vm_page_size is set by os_init(), and on some
     * systems (e.g. Alpha) arch_init() needs need os_vm_page_size, so
     * it must follow os_init(). -- WHN 2000-01-26 */
    os_init(argv, envp);
    /* os_init may re-execute the runtime, don't print anything before
     * that, otherwise it will be duplicated. */
    if (debug_environment_p) {
        print_environment(argc, argv);
    }
    dyndebug_init();
    arch_init();
    validate();
    gc_init();

    setup_locale();

    /* If no core file was specified, look for one. */
    if (!core) {
        core = search_for_core();
    }

    /* Make sure that SBCL_HOME is set and not the empty string,
       unless loading an embedded core. */
    if (!(sbcl_home && *sbcl_home) && embedded_core_offset == 0) {
        char *envstring, *copied_core, *dir;
        char *stem = "SBCL_HOME=";
        copied_core = copied_string(core);
        dir = dirname(copied_core);
        envstring = (char *) calloc(strlen(stem) +
                                    strlen(dir) +
                                    1,
                                    sizeof(char));
        sprintf(envstring, "%s%s", stem, dir);
        putenv(envstring);
        free(copied_core);
    }

    if (!noinform && embedded_core_offset == 0) {
        print_banner();
        fflush(stdout);
    }

    if (embedded_core_offset == 0) {
        /* Here we make a last attempt at recognizing an embedded core,
         * so that a file with an embedded core is a valid argument to
         * --core.  We take care that any decisions on special behaviour
         * (suppressed banner, embedded options) have already been made
         * before we reach this block, so that there is no observable
         * difference between "embedded" and "bare" images given to
         * --core. */
        os_vm_offset_t offset = search_for_embedded_core(core);
        if (offset != -1)
            embedded_core_offset = offset;
    }

#if defined(SVR4) || defined(__linux__) || defined(__NetBSD__)
    tzset();
#endif

    define_var("nil", NIL, 1);
    define_var("t", T, 1);

    if (!disable_lossage_handler_p)
        enable_lossage_handler();

    globals_init();

    initial_function = load_core_file(core, embedded_core_offset);
    if (initial_function == NIL) {
        lose("couldn't find initial function\n");
    }
#ifdef LISP_FEATURE_SB_DYNAMIC_CORE
    os_link_runtime();
#endif
#ifdef LISP_FEATURE_HPUX
    /* -1 = CLOSURE_FUN_OFFSET, 23 = SIMPLE_FUN_CODE_OFFSET, we are
     * not in LANGUAGE_ASSEMBLY so we cant reach them. */
    return_from_lisp_stub = (void *) ((char *)*((unsigned long *)
                 ((char *)initial_function + -1)) + 23);
#endif

    gc_initialize_pointers();

    arch_install_interrupt_handlers();
#ifndef LISP_FEATURE_WIN32
    os_install_interrupt_handlers();
#else
/*     wos_install_interrupt_handlers(handler); */
    wos_install_interrupt_handlers(&exception_frame);
#endif

    /* Pass core filename and the processed argv into Lisp. They'll
     * need to be processed further there, to do locale conversion.
     */
    core_string = core;
    posix_argv = sbcl_argv;

    FSHOW((stderr, "/funcalling initial_function=0x%lx\n",
          (unsigned long)initial_function));
    create_initial_thread(initial_function);
    lose("unexpected return from initial thread in main()\n");
    return 0;
}
