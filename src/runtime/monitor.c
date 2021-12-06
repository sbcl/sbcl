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
#include <sys/types.h>
#include <stdlib.h>
#include <sys/time.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/resource.h>
#endif
#include <signal.h>
#include <unistd.h>

#include "runtime.h"
#include "parse.h"
#include "vars.h"

#include "print.h"
#include "arch.h"
#include "interr.h"
#include "search.h"
#include "purify.h"
#include "globals.h"
#include "lispregs.h"
#include "interrupt.h"
#include "thread.h"
#include "genesis/static-symbols.h"
#include "genesis/primitive-objects.h"
#include "genesis/gc-tables.h"
#include "gc-internal.h"


/* When we need to do command input, we use this stream, which is not
 * in general stdin, so that things will "work" (as well as being
 * thrown into ldb can be considered "working":-) even in a process
 * where standard input has been redirected to a file or pipe.
 *
 * (We could set up output to go to a special ldb_out stream for the
 * same reason, but there's been no pressure for that so far.)
 *
 * The enter-the-ldb-monitor function is responsible for setting up
 * this stream. */
static FILE *ldb_in = 0;
static int ldb_in_fd = -1;

typedef int cmd(char **ptr);

static cmd call_cmd, dump_cmd, print_cmd, quit_cmd, help_cmd;
static cmd flush_cmd, regs_cmd, exit_cmd;
static cmd print_context_cmd, pte_cmd, search_cmd;
static cmd backtrace_cmd, purify_cmd, catchers_cmd;
static cmd grab_sigs_cmd;
static cmd kill_cmd;

static struct cmd {
    char *cmd, *help;
    int (*fn)(char **ptr);
} supported_cmds[] = {
    {"help", "Display this help information.", help_cmd},
    {"?", "(an alias for help)", help_cmd},
    {"backtrace", "Backtrace up to N frames.", backtrace_cmd},
    {"call", "Call FUNCTION with ARG1, ARG2, ...", call_cmd},
    {"catchers", "Print a list of all the active catchers.", catchers_cmd},
    {"context", "Print interrupt context number I.", print_context_cmd},
    {"dump", "Dump memory starting at ADDRESS for COUNT words.", dump_cmd},
    {"d", "(an alias for dump)", dump_cmd},
    {"exit", "Exit this instance of the monitor.", exit_cmd},
    {"flush", "Flush all temp variables.", flush_cmd},
    /* (Classic CMU CL had a "gc" command here, which seems like a
     * reasonable idea, but the code was stale (incompatible with
     * gencgc) so I just flushed it. -- WHN 20000814 */
    {"grab-signals", "Set the signal handlers to call LDB.", grab_sigs_cmd},
    {"kill", "Kill ourself with signal number N (useful if running under gdb)",
     kill_cmd},
    {"purify", "Purify. (Caveat purifier!)", purify_cmd},
    {"print", "Print object at ADDRESS.", print_cmd},
    {"p", "(an alias for print)", print_cmd},
    {"pte", "Page table entry for address", pte_cmd},
    {"quit", "Quit.", quit_cmd},
    {"regs", "Display current Lisp registers.", regs_cmd},
    {"search", "Search heap for object.", search_cmd},
    {NULL, NULL, NULL}
};

static int
visible(unsigned char c)
{
    if (c < ' ' || c > '~')
        return ' ';
    else
        return c;
}

static boolean valid_widetag_p(unsigned char widetag) {
    // TODO: ensure that widetag is defined (not "unused") and is for a headered object
    // (i.e. is not CHARACTER_WIDETAG and not some other things)
    return other_immediate_lowtag_p(widetag);
}
static int NO_SANITIZE_MEMORY
dump_cmd(char **ptr)
{
    static char *lastaddr = 0;
    static int lastcount = 20;

    char *addr = lastaddr;
    int count = lastcount, displacement;
    int force = 0, decode = 0;

    if (more_p(ptr)) {
        // you can't both "force" and "decode" - only one or the other,
        // or neither
        if (!strncmp(*ptr, "-f ", 3)) {
          force = 1;
          *ptr += 3;
        } else if (!strncmp(*ptr, "-d ", 3)) {
          decode = 1;
          *ptr += 3;
        }
        if (!parse_addr(ptr, !force, &addr)) return 0;

        if (more_p(ptr) && !parse_number(ptr, &count)) return 0;
    }

    if (count == 0) {
        printf("COUNT must be non-zero.\n");
        return 0;
    }

    lastcount = count;

    if (count > 0)
        displacement = N_WORD_BYTES;
    else {
        displacement = -N_WORD_BYTES;
        count = -count;
    }

    boolean aligned = ((uword_t)addr & LOWTAG_MASK) == 0;
    if (decode && (!aligned || displacement < 0)) {
        printf("Sorry, can only decode if aligned and stepping forward\n");
        decode = 0;
    }
    lispobj* next_object = decode ? (lispobj*)addr : 0;

    while (count-- > 0) {
        printf("%p: ", (os_vm_address_t) addr);
        if (force || gc_managed_addr_p((lispobj)addr)) {
            unsigned long *lptr = (unsigned long *)addr;
            unsigned char *cptr = (unsigned char *)addr;

#if N_WORD_BYTES == 8
            printf("0x%016lx | %c%c%c%c%c%c%c%c",
                   lptr[0],
                   visible(cptr[0]), visible(cptr[1]),
                   visible(cptr[2]), visible(cptr[3]),
                   visible(cptr[4]), visible(cptr[5]),
                   visible(cptr[6]), visible(cptr[7]));
#else
            unsigned short *sptr = (unsigned short *)addr;
            printf("0x%08lx   0x%04x 0x%04x   "
                   "0x%02x 0x%02x 0x%02x 0x%02x    "
                   "%c%c"
                   "%c%c",
                   lptr[0], sptr[0], sptr[1],
                   cptr[0], cptr[1], cptr[2], cptr[3],
                   visible(cptr[0]), visible(cptr[1]),
                   visible(cptr[2]), visible(cptr[3]));
#endif
#ifdef LISP_FEATURE_GENCGC
            if (aligned) {
                lispobj ptr = *(lispobj*)addr;
                int gen;
                if (is_lisp_pointer(ptr) && gc_managed_heap_space_p(ptr)
                    && (gen = gc_gen_of(ptr, 99)) != 99) // say that static is 99
                    if (gen != 99) printf(" | %d", gen);
            }
#endif
            if (decode && addr == (char*)next_object) {
                lispobj word = *(lispobj*)addr;
                // ensure validity of widetag because crashing with
                // "no size function" would be worse than doing nothing
                if (word != 0 && !is_lisp_pointer(word)
                    && valid_widetag_p(header_widetag(word))) {
                    printf(" %s", widetag_names[header_widetag(word)>>2]);
                    next_object += sizetab[header_widetag(word)](next_object);
                } else if (!is_header(word)) {
                    next_object += CONS_SIZE;
                } else { // disable decoder if weirdness observed
                    decode = 0;
                }
            }
            printf("\n");
        }
        else
            printf("invalid Lisp-level address\n");

        addr += displacement;
    }

    lastaddr = addr;
    return 0;
}

static int
print_cmd(char **ptr)
{
    lispobj obj;
    if (parse_lispobj(ptr, &obj)) print(obj);
    return 0;
}

static int
pte_cmd(char **ptr)
{
    extern void gc_show_pte(lispobj);
    lispobj obj;
    if (parse_lispobj(ptr, &obj)) gc_show_pte(obj);
    return 0;
}

static int
kill_cmd(char **ptr)
{
#ifndef LISP_FEATURE_WIN32
    int sig;
    if (parse_number(ptr, &sig)) kill(getpid(), sig);
#endif
    return 0;
}

static int
regs_cmd(char __attribute__((unused)) **ptr)
{
    struct thread __attribute__((unused)) *thread=get_sb_vm_thread();

    printf("CSP\t=\t%p   ", access_control_stack_pointer(thread));
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
    printf("CFP\t=\t%p   ", access_control_frame_pointer(thread));
#endif

#ifdef reg_BSP
    printf("BSP\t=\t%p\n", get_binding_stack_pointer(thread));
#else
    /* printf("BSP\t=\t%p\n", (void*)SymbolValue(BINDING_STACK_POINTER)); */
    printf("\n");
#endif

#ifdef LISP_FEATURE_GENCGC
    /* printf("DYNAMIC\t=\t%p\n", (void*)DYNAMIC_SPACE_START); */
#else
    printf("STATIC\t=\t%p   ", static_space_free_pointer);
    printf("RDONLY\t=\t%p   ", read_only_space_free_pointer);
    printf("DYNAMIC\t=\t%p\n", (void*)current_dynamic_space);
#endif

#ifndef ALLOCATION_POINTER
    printf("ALLOC\t=\t%p\n", (void*)dynamic_space_free_pointer);
#else
    printf("ALLOC\t=\t%p\n", (void*)SymbolValue(ALLOCATION_POINTER, thread));
#endif

#ifndef LISP_FEATURE_GENCGC
    printf("TRIGGER\t=\t%p\n", (void*)current_auto_gc_trigger);
#endif
    return 0;
}

static int
call_cmd(char **ptr)
{
    lispobj thing;
    parse_lispobj(ptr, &thing);
    lispobj function, args[3];
    lispobj result = NIL;

    int numargs;

    if (lowtag_of(thing) == OTHER_POINTER_LOWTAG) {
        lispobj *obj = native_pointer(thing);
        switch (widetag_of(obj)) {
          case SYMBOL_WIDETAG:
              function = symbol_function((struct symbol*)obj);
              if (function == NIL) {
                  printf("Symbol 0x%08lx is undefined.\n", (long unsigned)thing);
                  return 0;
              }
              break;
          case FDEFN_WIDETAG:
              function = FDEFN(thing)->fun;
              if (function == NIL) {
                  printf("Fdefn 0x%08lx is undefined.\n", (long unsigned)thing);
                  return 0;
              }
              break;
          default:
              printf("0x%08lx is not a function pointer, symbol, "
                     "or fdefn object.\n",
                     (long unsigned)thing);
              return 0;
        }
    }
    else if (lowtag_of(thing) != FUN_POINTER_LOWTAG) {
        printf("0x%08lx is not a function pointer, symbol, or fdefn object.\n",
               (long unsigned)thing);
        return 0;
    }
    else
        function = thing;

    numargs = 0;
    while (more_p(ptr)) {
        if (numargs >= 3) {
            printf("too many arguments (no more than 3 supported)\n");
            return 0;
        }
        parse_lispobj(ptr, &args[numargs++]);
    }

    switch (numargs) {
      case 0:
          result = funcall0(function);
          break;
      case 1:
          result = funcall1(function, args[0]);
          break;
      case 2:
          result = funcall2(function, args[0], args[1]);
          break;
      case 3:
          result = funcall3(function, args[0], args[1], args[2]);
          break;
      default:
          lose("unsupported arg count made it past validity check?!");
    }

    print(result);
    return 0;
}

static int
flush_cmd(char __attribute__((unused)) **ptr)
{
    flush_vars();
    return 0;
}

static int
quit_cmd(char __attribute__((unused)) **ptr)
{
    char buf[10];

    printf("Really quit? [y] ");
    fflush(stdout);
    if (fgets(buf, sizeof(buf), ldb_in)) {
        if (buf[0] == 'y' || buf[0] == 'Y' || buf[0] == '\n')
            exit(1);
    } else {
        printf("\nUnable to read response, assuming y.\n");
        exit(1);
    }
    return 0;
}

static int
help_cmd(char __attribute__((unused)) **ptr)
{
    struct cmd *cmd;

    for (cmd = supported_cmds; cmd->cmd != NULL; cmd++)
        if (cmd->help != NULL)
            printf("%s\t%s\n", cmd->cmd, cmd->help);
    return 0;
}

static int
exit_cmd(char __attribute__((unused)) **ptr)
{
    return 1; // 'done' flag
}

static int
purify_cmd(char __attribute__((unused)) **ptr)
{
    purify(NIL, NIL);
    return 0;
}

static void
print_context(os_context_t *context)
{
    int i;

    for (i = 0; i < NREGS; i++) {
        printf("%s:\t", lisp_register_names[i]);
        brief_print((lispobj)(*os_context_register_addr(context,i)));

    }
#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_PPC)
    printf("DAR:\t\t 0x%08lx\n", (unsigned long)(*os_context_register_addr(context, 41)));
    printf("DSISR:\t\t 0x%08lx\n", (unsigned long)(*os_context_register_addr(context, 42)));
#endif
#ifndef REG_PC
    printf("PC:\t\t  0x%08lx\n",
           (unsigned long)(*os_context_pc_addr(context)));
#endif
}

static int
print_context_cmd(char **ptr)
{
    int free_ici;
    struct thread *thread=get_sb_vm_thread();

    free_ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,thread));

    if (more_p(ptr)) {
        int index;

        if (!parse_number(ptr, &index)) return 0;

        if ((index >= 0) && (index < free_ici)) {
            printf("There are %d interrupt contexts.\n", free_ici);
            printf("printing context %d\n", index);
            print_context(nth_interrupt_context(index, thread));
        } else {
            printf("There aren't that many/few contexts.\n");
            printf("There are %d interrupt contexts.\n", free_ici);
        }
    } else {
        if (free_ici == 0)
            printf("There are no interrupt contexts!\n");
        else {
            printf("There are %d interrupt contexts.\n", free_ici);
            printf("printing context %d\n", free_ici - 1);
            print_context(nth_interrupt_context(free_ici - 1, thread));
        }
    }
    return 0;
}

static int
backtrace_cmd(char **ptr)
{
    void lisp_backtrace(int frames);
    int n;

    if (more_p(ptr)) {
        if (!parse_number(ptr, &n)) return 0;
    } else
        n = 100;

    printf("Backtrace:\n");
    lisp_backtrace(n);
    return 0;
}

static int search_cmd(char **ptr)
{
    char *addr;
    if (!parse_addr(ptr, 1, &addr)) return 0;
    lispobj *obj = search_all_gc_spaces((void*)addr);
    if(obj)
        printf("#x%"OBJ_FMTX"\n", compute_lispobj(obj));
    else
        printf("Not found\n");
    return 0;
}

static int
catchers_cmd(char __attribute__((unused)) **ptr)
{
    struct catch_block *catch = (struct catch_block *)
        read_TLS(CURRENT_CATCH_BLOCK, get_sb_vm_thread());

    if (catch == NULL)
        printf("There are no active catchers!\n");
    else {
        while (catch != NULL) {
            printf("%p:\n\tuwp  : %p\n\tfp   : %p\n\t"
                   "code : %p\n\tentry: %p\n\ttag: ",
                   catch,
                   catch->uwp,
                   catch->cfp,
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || defined(LISP_FEATURE_ARM64)
                   component_ptr_from_pc((void*)catch->entry_pc),
#else
                   catch->code,
#endif
                   (void*)(catch->entry_pc));
            brief_print((lispobj)catch->tag);
            catch = catch->previous_catch;
        }
    }
    return 0;
}

/* SIGINT handler that invokes the monitor (for when Lisp isn't up to it) */
static void
sigint_handler(int __attribute__((unused)) signal,
               siginfo_t __attribute__((unused)) *info,
               void *context)
{
    extern void ldb_monitor();
    fprintf(stderr, "\nSIGINT hit at %p\n", (void*)*os_context_pc_addr(context));
    ldb_monitor();
    fprintf(stderr, "Returning to lisp (if you're lucky).\n");
}

static int
grab_sigs_cmd(char __attribute__((unused)) **ptr)
{
#ifdef LISP_FEATURE_WIN32
    fprintf(stderr, "sorry no can do\n"); fflush(stderr);
#else
    printf("Grabbing SIGINT.\n");
    struct sigaction sa;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO;
    sa.sa_sigaction = sigint_handler;
    sigaction(SIGINT, &sa, 0);
#endif
    return 0;
}

void
ldb_monitor(void)
{
    struct cmd *cmd, *found;
    char buf[256];
    char *line, *ptr, *token;
    int ambig;

    printf("Welcome to LDB, a low-level debugger for the Lisp runtime environment.\n");
    if (!ldb_in) {
#ifndef LISP_FEATURE_WIN32
        ldb_in = fopen("/dev/tty","r+");
        if (ldb_in == NULL) {
            perror("Error opening /dev/tty");
            ldb_in = stdin;
        }
#else
        ldb_in = stdin;
#endif
        ldb_in_fd = fileno(ldb_in);
    }

    while (1) {
        printf("ldb> ");
        fflush(stdout);
        line = fgets(buf, sizeof(buf), ldb_in);
        if (line == NULL) {
            exit(1);
        }
        ptr = line;
        if ((token = parse_token(&ptr)) == NULL)
            continue;
        ambig = 0;
        found = NULL;
        for (cmd = supported_cmds; cmd->cmd != NULL; cmd++) {
            if (strcmp(token, cmd->cmd) == 0) {
                found = cmd;
                ambig = 0;
                break;
            }
            else if (strncmp(token, cmd->cmd, strlen(token)) == 0) {
                if (found)
                    ambig = 1;
                else
                    found = cmd;
            }
        }
        if (ambig)
            printf("``%s'' is ambiguous.\n", token);
        else if (found == NULL)
            printf("unknown command: ``%s''\n", token);
        else {
            reset_printer();
            int done = (*found->fn)(&ptr);
            if (done) return;
        }
    }
}

/* what we do when things go badly wrong at a low level */
void
monitor_or_something()
{
    ldb_monitor();
}
