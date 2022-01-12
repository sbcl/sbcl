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
#else
#include <shellapi.h>
#endif
#include <errno.h>
#include <locale.h>
#include <limits.h>

#include <time.h>

#ifndef LISP_FEATURE_WIN32
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

struct timespec lisp_init_time;

static char libpath[] = "../lib/sbcl";
char *sbcl_runtime_home;
char *sbcl_runtime;


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

static char *
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
static char *
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
        if (messy)
            free(messy);
        free(tidy);
        return NULL;
    }

    if (messy)
        free(messy);

    return tidy;
}
#endif /* LISP_FEATURE_WIN32 */

/* miscellaneous chattiness */

static void
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
  --tls-limit                Maximum number of thread-local symbols.\n\
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

static void
print_version()
{
    printf("SBCL %s\n", SBCL_VERSION_STRING);
}

static void
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

/* Look for a core file to load, first in the directory named by the
 * SBCL_HOME environment variable, then in a hardcoded default
 * location.  Returns a malloced copy of the core filename. */
static char *
search_for_core ()
{
    char *env_sbcl_home = getenv("SBCL_HOME");
    char *lookhere;
    char *stem = "/sbcl.core";
    char *core;
    struct stat filename_stat;

    if (!(env_sbcl_home && *env_sbcl_home) ||
        stat(env_sbcl_home, &filename_stat))
        env_sbcl_home = sbcl_runtime_home;
    lookhere = (char *) calloc(strlen(env_sbcl_home) +
                               strlen(libpath) +
                               strlen(stem) +
                               1,
                               sizeof(char));
    sprintf(lookhere, "%s%s%s", env_sbcl_home, libpath, stem);
    core = copied_existing_filename_or_null(lookhere);

    if (core) {
        free(lookhere);
    } else {
        free(lookhere);
        core = copied_existing_filename_or_null ("sbcl.core");
        if (!core) {
            lookhere = (char *) calloc(strlen(env_sbcl_home) +
                                       strlen(stem) +
                                       1,
                                       sizeof(char));
            sprintf(lookhere, "%s%s", env_sbcl_home, stem);
            core = copied_existing_filename_or_null (lookhere);
            if (core) {
                free(lookhere);
            } else {
                return NULL;
            }
        }
    }

    return core;
}

/* Try to find the path to an executable from argv[0], this is only
 * used when os_get_runtime_executable_path() returns NULL */
#ifdef LISP_FEATURE_WIN32
static char *
search_for_executable(const char *argv0)
{
    return NULL;
}
#else /* LISP_FEATURE_WIN32 */
static char *
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

static size_t
parse_size_arg(char *arg, char *arg_name)
{
  char *tail;
  size_t power = 20, res;

  res = strtoul(arg, &tail, 0);

  if (arg == tail) {
    lose("%s argument is not a number: %s", arg_name, arg);
  } else if (tail[0]) {
    if (!strcasecmp("KB", tail) || !strcasecmp("KIB", tail)) {
      power = 10;
    } else if (!strcasecmp("MB", tail) || !strcasecmp("MIB", tail)) {
      power = 20;
    } else if (!strcasecmp("GB", tail) || !strcasecmp("GIB", tail)) {
      power = 30;
    } else if (!strcasecmp("TB", tail) || !strcasecmp("TIB", tail)) {
      power = 40;
    } else {
      lose("%s argument has an unknown suffix: %s", arg_name, tail);
    }
  }
  if ((res <= 0) ||
      (res >= (SIZE_MAX >> power))) {
    lose("%s argument is out of range: %s", arg_name, arg);
  }
  res <<= power;
  return res;
}

#ifdef LISP_FEATURE_WIN32
    wchar_t
#else
    char
#endif
    **posix_argv;

char *core_string;

static void print_environment(int argc, char *argv[])
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

char * sb_realpath (char *);
char *dir_name(char *path) {
    if (path == NULL)
        return NULL;

    char* result;
    char slashchar =
#ifdef LISP_FEATURE_WIN32
        '\\';
#else
    '/';
#endif

    char *slash = strrchr(path, slashchar);

    if (slash) {
        int prefixlen = slash - path + 1; // keep the slash in the prefix
        result = successful_malloc(prefixlen + 1);
        memcpy(result, path, prefixlen);
        result[prefixlen] = 0;
        return result;
    } else {
        return NULL;
    }
}


extern void write_protect_immobile_space();
struct lisp_startup_options lisp_startup_options;

struct cmdline_options {
    char *core;
    char **argv;
    boolean disable_lossage_handler_p;
    int merge_core_pages;
};

static int is_memsize_arg(char *argv[], int argi, int argc, int *merge_core_pages)
{
    char *arg = argv[argi];
    if (!strcmp(arg, "--dynamic-space-size")) {
        if ((argi+1) >= argc) lose("missing argument for --dynamic-space-size");
        dynamic_space_size = parse_size_arg(argv[argi+1],
                                            "--dynamic-space-size");
#ifdef MAX_DYNAMIC_SPACE_END
        if (!((DYNAMIC_SPACE_START <
                       DYNAMIC_SPACE_START+dynamic_space_size) &&
                      (DYNAMIC_SPACE_START+dynamic_space_size <=
                       MAX_DYNAMIC_SPACE_END))) {
            char* suffix = "";
            char* size = argv[argi-1];
            if (!strchr(size, 'B') && !strchr(size, 'b')) suffix = " [MB]";
            lose("--dynamic-space-size argument %s%s is too large, max %lu KB",
                 size, suffix, (MAX_DYNAMIC_SPACE_END-DYNAMIC_SPACE_START) / 1024);
        }
#endif
        return 2;
    }
    if (!strcmp(arg, "--control-stack-size")) {
        if ((argi+1) >= argc) lose("missing argument for --control-stack-size");
        thread_control_stack_size = parse_size_arg(argv[argi+1], "--control-stack-size");
        return 2;
    }
    if (!strcmp(arg, "--tls-limit")) {
        // this is not named "tls-size" because "size" is not the
        // best measurement for how many symbols to allow
        if ((argi+1) >= argc) lose("missing argument for --tls-limit");
        dynamic_values_bytes = N_WORD_BYTES * atoi(argv[argi+1]);
        return 2;
    }
    if (!strcmp(arg, "--merge-core-pages")) {
        *merge_core_pages = 1;
        return 1;
    }
    if (!strcmp(arg, "--no-merge-core-pages")) {
        *merge_core_pages = 0;
        return 1;
    }
    return 0;
}

static struct cmdline_options
parse_argv(struct memsize_options memsize_options,
           int argc, char *argv[], char *core)
{
#ifdef LISP_FEATURE_WIN32
    wchar_t
#else
        char
#endif
        **sbcl_argv = 0;
    /* other command line options */
    boolean disable_lossage_handler_p
#if defined(LISP_FEATURE_SB_LDB)
        = 0;
#else
        = 1;
#endif
    boolean debug_environment_p = 0;
    int merge_core_pages = -1;

    int argi = 1;
    int n_consumed;
    if (memsize_options.present_in_core) {
        /* Our arg parsing isn't (and can't be) integrated with the application's,
         * but we really want users to be able to override the heap size.
         * So don't parse most options, but _do_ parse memory size options and/or
         * core page merging options, wherever they occur, and strip them out.
         * Any args that remain are passed through to Lisp.
         *
         * This does have a small semantic glitch: If your executable accepts
         * flags such as "--my-opt" "--merge-core-pages" where "--merge-core-pages"
         * is literally (and perversely) the value the user gives to "--my-opt",
         * that's just too bad! The somewhat conventional "--" option will stop
         * parsing SBCL options and pass everything else through including the "--".
         * The rationale for passing "--" through is that we're trying to be
         * as uninvasive as possible. Let's hope that nobody needs to put a "--"
         * to the left of any of the memory size options */
        dynamic_space_size = memsize_options.dynamic_space_size;
        thread_control_stack_size = memsize_options.thread_control_stack_size;
        dynamic_values_bytes = memsize_options.thread_tls_bytes;
#ifndef LISP_FEATURE_WIN32
        sbcl_argv = successful_malloc((argc + 1) * sizeof(char *));
        sbcl_argv[0] = argv[0];
        int stop_parsing = 0; // have we seen '--'
        int output_index = 1;
        while (argi < argc) {
            if (stop_parsing) // just copy it over
                sbcl_argv[output_index++] = argv[argi++];
            else if (!strcmp(argv[argi], "--")) // keep it, but parse nothing else
                sbcl_argv[output_index++] = argv[argi++], stop_parsing = 1;
            else if ((n_consumed = is_memsize_arg(argv, argi, argc, &merge_core_pages)))
                argi += n_consumed; // eat it
            else // default action - copy it
                sbcl_argv[output_index++] = argv[argi++];
        }
        sbcl_argv[output_index] = 0;
#else
        int wargc;
        sbcl_argv = CommandLineToArgvW(GetCommandLineW(), &wargc);
        // Somebody who wishes this to work for #+win32 should feel free to do the same...
#endif
    } else {
        boolean end_runtime_options = 0;
        /* Parse our any of the command-line options that we handle from C,
         * stopping at the first one that we don't, and leave the rest */
        while (argi < argc) {
            char *arg = argv[argi];
            if (0 == strcmp(arg, "--script")) {
                /* This is both a runtime and a toplevel option. As a
                 * runtime option, it is equivalent to --noinform.
                 * This exits, and does not increment argi, so that
                 * TOPLEVEL-INIT sees the option. */
                lisp_startup_options.noinform = 1;
                end_runtime_options = 1;
                disable_lossage_handler_p = 1;
                lose_on_corruption_p = 1;
                break;
            } else if (0 == strcmp(arg, "--noinform")) {
                lisp_startup_options.noinform = 1;
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
            } else if ((n_consumed = is_memsize_arg(argv, argi, argc, &merge_core_pages))) {
                argi += n_consumed;
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
#ifndef LISP_FEATURE_WIN32
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
                    lose("bad runtime option \"%s\"", argi0);
                }
                sbcl_argv[argj++] = arg;
            }
#else
            /* The runtime options are processed as chars above, which may
             * not always work but may be good enough for now, as it
             * has been for a long time. */
            int wargc;
            wchar_t** wargv;
            wargv = CommandLineToArgvW(GetCommandLineW(), &wargc);
            sbcl_argv = successful_malloc((((argi < wargc) ? (wargc - argi) : 0) + 2)
                                          * sizeof(wchar_t *));
            sbcl_argv[0] = wargv[0];
            while (argi < wargc) {
                wchar_t *warg = wargv[argi++];
                if (!end_runtime_options &&
                    0 == wcscmp(warg, L"--end-runtime-options")) {
                    lose("bad runtime option \"%s\"", argi0);
                }
                sbcl_argv[argj++] = warg;
            }
#endif
            sbcl_argv[argj] = 0;
        }
    }
    if (debug_environment_p) {
        print_environment(argc, argv);
    }

    struct cmdline_options o;
    o.core = core;
    o.argv = sbcl_argv;
    o.disable_lossage_handler_p = disable_lossage_handler_p;
    o.merge_core_pages = merge_core_pages;
    return o;
}

int
initialize_lisp(int argc, char *argv[], char *envp[])
{
#ifdef LISP_FEATURE_WIN32
    /* Exception handling support structure. Evil Win32 hack. */
    struct lisp_exception_frame exception_frame;
#endif
#ifdef LISP_FEATURE_UNIX
    clock_gettime(
#ifdef LISP_FEATURE_LINUX
        CLOCK_MONOTONIC_COARSE
#else
        CLOCK_MONOTONIC
#endif
        , &lisp_init_time);
#endif

    /* the name of the core file we're to execute. Note that this is
     * a malloc'ed string which should be freed eventually. */
    char *core = 0;

    os_vm_offset_t embedded_core_offset = 0;

    lispobj initial_function;
    struct memsize_options memsize_options;
    memsize_options.present_in_core = 0;

    boolean have_hardwired_spaces = os_preinit(argv, envp);

    interrupt_init();
#ifdef LISP_FEATURE_UNIX
    /* Not sure why anyone sends signals to this process so early.
     * But win32 models the signal mask as part of 'struct thread'
     * which doesn't exist yet, so don't do this */
    block_blockable_signals(0);
#endif

    /* Check early to see if this executable has an embedded core,
     * which also populates runtime_options if the core has runtime
     * options */
    if (!(sbcl_runtime = os_get_runtime_executable_path()))
        sbcl_runtime = search_for_executable(argv[0]);

    if (!(sbcl_runtime_home = dir_name(argv[0])))
      if (!(sbcl_runtime_home = dir_name(sbcl_runtime)))
        sbcl_runtime_home = libpath;

    if (sbcl_runtime) {
        os_vm_offset_t offset = search_for_embedded_core(sbcl_runtime, &memsize_options);
        if (offset != -1) {
            embedded_core_offset = offset;
            core = sbcl_runtime;
        }
    }

    struct cmdline_options options = parse_argv(memsize_options, argc, argv, core);

    /* Align down to multiple of page_table page size, and to the appropriate
     * stack alignment. */
    dynamic_space_size &= ~(sword_t)(BACKEND_PAGE_BYTES-1);
#ifdef LISP_FEATURE_GENCGC
    dynamic_space_size &= ~(sword_t)(GENCGC_PAGE_BYTES-1);
#endif
    thread_control_stack_size &= ~(sword_t)(CONTROL_STACK_ALIGNMENT_BYTES-1);

    os_init();
    dyndebug_init();
    // FIXME: if the 'have' flag is 0 and you've disabled disabling of ASLR
    // then we haven't done an exec(), nor unmapped the mappings that were obtained
    // already obtained (if any) so it is unhelpful to try again here.
    allocate_lisp_dynamic_space(have_hardwired_spaces);
    gc_init();

    /* If no core file was specified, look for one. */
    core = options.core;
    if (!core && !(core = search_for_core())) {
      /* Try resolving symlinks */
      if (sbcl_runtime) {
        free(sbcl_runtime_home);
        char* real = sb_realpath(sbcl_runtime);
        if (!real)
          goto lose;
        sbcl_runtime_home = dir_name(real);
        free(real);
        if (!sbcl_runtime_home)
          goto lose;
        if(!(core = search_for_core()))
          goto lose;
      } else {
      lose:
        lose("Can't find sbcl.core");
      }
    }

    if (embedded_core_offset)
        lisp_startup_options.noinform = 1;

    if (!lisp_startup_options.noinform) {
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
        os_vm_offset_t offset = search_for_embedded_core(core, 0);
        if (offset != -1)
            embedded_core_offset = offset;
    }

    globals_init();

    /* Doing this immediately after the core has been located
     * and before any random malloc() calls occur improves the chance
     * of mapping dynamic space at our preferred address (if movable).
     * If not movable, it was already mapped in allocate_spaces(). */
    initial_function = load_core_file(core, embedded_core_offset,
                                      options.merge_core_pages);
    if (initial_function == NIL) {
        lose("couldn't find initial function");
    }

#if defined(SVR4) || defined(__linux__) || defined(__NetBSD__) || defined(__HAIKU__)
    tzset();
#endif

    define_var("nil", NIL, 1);
    define_var("t", T, 1);

    if (!options.disable_lossage_handler_p)
        enable_lossage_handler();

    os_link_runtime();
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    /* Delayed until after dynamic space has been mapped, fixups made,
     * and/or immobile-space linkage entries written,
     * since it was too soon earlier to handle write faults. */
    write_protect_immobile_space();
#endif

    arch_install_interrupt_handlers();
#ifndef LISP_FEATURE_WIN32
    os_install_interrupt_handlers();
# ifdef LISP_FEATURE_SB_SAFEPOINT
    ll_install_handler(SIGURG, thruption_handler);
# elif defined LISP_FEATURE_SB_THREAD
    ll_install_handler(SIG_STOP_FOR_GC, sig_stop_for_gc_handler);
# endif
#else
/*     wos_install_interrupt_handlers(handler); */
    wos_install_interrupt_handlers(&exception_frame);
#endif

    /* Pass core filename and the processed argv into Lisp. They'll
     * need to be processed further there, to do locale conversion.
     */
    core_string = core;
    posix_argv = options.argv;

    FSHOW((stderr, "/funcalling initial_function=0x%lx\n",
          (unsigned long)initial_function));
    create_main_lisp_thread(initial_function);
    return 0;
}
