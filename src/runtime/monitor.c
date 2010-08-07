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
#include <setjmp.h>
#include <sys/time.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/resource.h>
#endif
#include <signal.h>
#include <unistd.h>

#include "runtime.h"
#include "parse.h"
#include "vars.h"

/* Almost all of this file can be skipped if we're not supporting LDB. */
#if defined(LISP_FEATURE_SB_LDB)

#include "print.h"
#include "arch.h"
#include "interr.h"
#include "gc.h"
#include "search.h"
#include "purify.h"
#include "globals.h"
#include "lispregs.h"
#include "interrupt.h"
#include "thread.h"
#include "genesis/static-symbols.h"
#include "genesis/primitive-objects.h"



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

typedef void cmd(char **ptr);

static cmd dump_cmd, print_cmd, quit_cmd, help_cmd;
static cmd flush_cmd, search_cmd, regs_cmd, exit_cmd;
static cmd print_context_cmd;
static cmd backtrace_cmd, purify_cmd, catchers_cmd;
static cmd grab_sigs_cmd;
static cmd kill_cmd;

static struct cmd {
    char *cmd, *help;
    void (*fn)(char **ptr);
} supported_cmds[] = {
    {"help", "Display this help information.", help_cmd},
    {"?", "(an alias for help)", help_cmd},
    {"backtrace", "Backtrace up to N frames.", backtrace_cmd},
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
    {"quit", "Quit.", quit_cmd},
    {"regs", "Display current Lisp registers.", regs_cmd},
    {"search", "Search for TYPE starting at ADDRESS for a max of COUNT words.", search_cmd},
    {"s", "(an alias for search)", search_cmd},
    {NULL, NULL, NULL}
};

static jmp_buf curbuf;

static int
visible(unsigned char c)
{
    if (c < ' ' || c > '~')
        return ' ';
    else
        return c;
}

static void
dump_cmd(char **ptr)
{
    static char *lastaddr = 0;
    static int lastcount = 20;

    char *addr = lastaddr;
    int count = lastcount, displacement;

    if (more_p(ptr)) {
        addr = parse_addr(ptr);

        if (more_p(ptr))
            count = parse_number(ptr);
    }

    if (count == 0) {
        printf("COUNT must be non-zero.\n");
        return;
    }

    lastcount = count;

    if (count > 0)
        displacement = 4;
    else {
        displacement = -4;
        count = -count;
    }

    while (count-- > 0) {
#ifndef LISP_FEATURE_ALPHA
        printf("0x%08lX: ", (unsigned long) addr);
#else
        printf("0x%08X: ", (u32) addr);
#endif
        if (is_valid_lisp_addr((os_vm_address_t)addr)) {
#ifndef LISP_FEATURE_ALPHA
            unsigned long *lptr = (unsigned long *)addr;
#else
            u32 *lptr = (u32 *)addr;
#endif
            unsigned short *sptr = (unsigned short *)addr;
            unsigned char *cptr = (unsigned char *)addr;

            printf("0x%08lx   0x%04x 0x%04x   "
                   "0x%02x 0x%02x 0x%02x 0x%02x    "
                   "%c%c"
                   "%c%c\n",
                   lptr[0], sptr[0], sptr[1],
                   cptr[0], cptr[1], cptr[2], cptr[3],
                   visible(cptr[0]), visible(cptr[1]),
                   visible(cptr[2]), visible(cptr[3]));
        }
        else
            printf("invalid Lisp-level address\n");

        addr += displacement;
    }

    lastaddr = addr;
}

static void
print_cmd(char **ptr)
{
    lispobj obj = parse_lispobj(ptr);

    print(obj);
}

static void
kill_cmd(char **ptr)
{
#ifndef LISP_FEATURE_WIN32
    kill(getpid(), parse_number(ptr));
#endif
}

static void
regs_cmd(char **ptr)
{
    struct thread *thread=arch_os_get_current_thread();

    printf("CSP\t=\t0x%08lx   ", (unsigned long)access_control_stack_pointer(thread));
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
    printf("CFP\t=\t0x%08lx   ", (unsigned long)access_control_frame_pointer(thread));
#endif

#ifdef reg_BSP
    printf("BSP\t=\t0x%08lx\n", (unsigned long)get_binding_stack_pointer(thread));
#else
    /* printf("BSP\t=\t0x%08lx\n",
           (unsigned long)SymbolValue(BINDING_STACK_POINTER)); */
    printf("\n");
#endif

#ifdef LISP_FEATURE_GENCGC
    /* printf("DYNAMIC\t=\t0x%08lx\n", DYNAMIC_SPACE_START); */
#else
    printf("STATIC\t=\t0x%08lx   ",
           (unsigned long)SymbolValue(STATIC_SPACE_FREE_POINTER, thread));
    printf("RDONLY\t=\t0x%08lx   ",
           (unsigned long)SymbolValue(READ_ONLY_SPACE_FREE_POINTER, thread));
    printf("DYNAMIC\t=\t0x%08lx\n", (unsigned long)current_dynamic_space);
#endif

#ifdef reg_ALLOC
    printf("ALLOC\t=\t0x%08lx\n", (unsigned long)dynamic_space_free_pointer);
#else
    printf("ALLOC\t=\t0x%08lx\n",
           (unsigned long)SymbolValue(ALLOCATION_POINTER, thread));
#endif

#ifndef LISP_FEATURE_GENCGC
    printf("TRIGGER\t=\t0x%08lx\n", (unsigned long)current_auto_gc_trigger);
#endif
}

static void
search_cmd(char **ptr)
{
    static int lastval = 0, lastcount = 0;
    static lispobj *start = 0, *end = 0;
    int val, count;
    lispobj *addr, obj;

    if (more_p(ptr)) {
        val = parse_number(ptr);
        if (val < 0 || val > 0xff) {
            printf("can only search for single bytes\n");
            return;
        }
        if (more_p(ptr)) {
            addr = (lispobj *)native_pointer((long)parse_addr(ptr));
            if (more_p(ptr)) {
                count = parse_number(ptr);
            }
            else {
                /* Specified value and address, but no count. Only one. */
                count = -1;
            }
        }
        else {
            /* Specified a value, but no address, so search same range. */
            addr = start;
            count = lastcount;
        }
    }
    else {
        /* Specified nothing, search again for val. */
        val = lastval;
        addr = end;
        count = lastcount;
    }

    lastval = val;
    start = end = addr;
    lastcount = count;

    printf("searching for 0x%x at 0x%08lX\n", val, (unsigned long)end);

    while (search_for_type(val, &end, &count)) {
        printf("found 0x%x at 0x%08lX:\n", val, (unsigned long)end);
        obj = *end;
        addr = end;
        end += 2;
        if (widetag_of(obj) == SIMPLE_FUN_HEADER_WIDETAG) {
            print((long)addr | FUN_POINTER_LOWTAG);
        } else if (other_immediate_lowtag_p(obj)) {
            print((lispobj)addr | OTHER_POINTER_LOWTAG);
        } else {
            print((lispobj)addr);
        } if (count == -1) {
            return;
        }
    }
}

/* (There used to be call_cmd() here, to call known-at-cold-init-time
 * Lisp functions from ldb, but it bitrotted and was deleted in
 * sbcl-0.7.5.1. See older CVS versions if you want to resuscitate
 * it.) */

static void
flush_cmd(char **ptr)
{
    flush_vars();
}

static void
quit_cmd(char **ptr)
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
}

static void
help_cmd(char **ptr)
{
    struct cmd *cmd;

    for (cmd = supported_cmds; cmd->cmd != NULL; cmd++)
        if (cmd->help != NULL)
            printf("%s\t%s\n", cmd->cmd, cmd->help);
}

static int done;

static void
exit_cmd(char **ptr)
{
    done = 1;
}

static void
purify_cmd(char **ptr)
{
    purify(NIL, NIL);
}

static void
print_context(os_context_t *context)
{
    int i;

    for (i = 0; i < NREGS; i++) {
        printf("%s:\t", lisp_register_names[i]);
#ifdef LISP_FEATURE_X86
        brief_print((lispobj)(*os_context_register_addr(context,
                                                        i*2)));
#else
        brief_print((lispobj)(*os_context_register_addr(context,i)));
#endif
    }
#ifdef LISP_FEATURE_DARWIN
    printf("DAR:\t\t 0x%08lx\n", (unsigned long)(*os_context_register_addr(context, 41)));
    printf("DSISR:\t\t 0x%08lx\n", (unsigned long)(*os_context_register_addr(context, 42)));
#endif
    printf("PC:\t\t  0x%08lx\n",
           (unsigned long)(*os_context_pc_addr(context)));
}

static void
print_context_cmd(char **ptr)
{
    int free_ici;
    struct thread *thread=arch_os_get_current_thread();

    free_ici = fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX,thread));

    if (more_p(ptr)) {
        int index;

        index = parse_number(ptr);

        if ((index >= 0) && (index < free_ici)) {
            printf("There are %d interrupt contexts.\n", free_ici);
            printf("printing context %d\n", index);
            print_context(thread->interrupt_contexts[index]);
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
            print_context(thread->interrupt_contexts[free_ici - 1]);
        }
    }
}

static void
backtrace_cmd(char **ptr)
{
    void lisp_backtrace(int frames);
    int n;

    if (more_p(ptr))
        n = parse_number(ptr);
    else
        n = 100;

    printf("Backtrace:\n");
    lisp_backtrace(n);
}

static void
catchers_cmd(char **ptr)
{
    struct catch_block *catch;
    struct thread *thread=arch_os_get_current_thread();

    catch = (struct catch_block *)SymbolValue(CURRENT_CATCH_BLOCK,thread);

    if (catch == NULL)
        printf("There are no active catchers!\n");
    else {
        while (catch != NULL) {
            printf("0x%08lX:\n\tuwp: 0x%08lX\n\tfp: 0x%08lX\n\t"
                   "code: 0x%08lX\n\tentry: 0x%08lX\n\ttag: ",
                   (unsigned long)catch,
                   (unsigned long)(catch->current_uwp),
                   (unsigned long)(catch->current_cont),
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
                   (unsigned long)component_ptr_from_pc((void*)catch->entry_pc)
                       + OTHER_POINTER_LOWTAG,
#else
                   (unsigned long)(catch->current_code),
#endif
                   (unsigned long)(catch->entry_pc));
            brief_print((lispobj)catch->tag);
            catch = catch->previous_catch;
        }
    }
}

static void
grab_sigs_cmd(char **ptr)
{
    extern void sigint_init(void);

    printf("Grabbing signals.\n");
    sigint_init();
}

static void
sub_monitor(void)
{
    struct cmd *cmd, *found;
    char buf[256];
    char *line, *ptr, *token;
    int ambig;

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

    while (!done) {
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
            (*found->fn)(&ptr);
        }
    }
}

void
ldb_monitor()
{
    jmp_buf oldbuf;

    bcopy(curbuf, oldbuf, sizeof(oldbuf));

    printf("Welcome to LDB, a low-level debugger for the Lisp runtime environment.\n");

    setjmp(curbuf);

    sub_monitor();

    done = 0;

    bcopy(oldbuf, curbuf, sizeof(curbuf));
}

void
throw_to_monitor()
{
    longjmp(curbuf, 1);
}

#endif /* defined(LISP_FEATURE_SB_LDB) */

/* what we do when things go badly wrong at a low level */
void
monitor_or_something()
{
#if defined(LISP_FEATURE_SB_LDB)
    ldb_monitor();
#else
     fprintf(stderr,
"The system is too badly corrupted or confused to continue at the Lisp\n\
level. If the system had been compiled with the SB-LDB feature, we'd drop\n\
into the LDB low-level debugger now. But there's no LDB in this build, so\n\
we can't really do anything but just exit, sorry.\n");
    exit(1);
#endif
}
