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
#include <sys/types.h>
#include <stdlib.h>
#include <setjmp.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include <unistd.h>

#include "runtime.h"
#include "sbcl.h"
#include "monitor.h"

/* Almost all of this file can be skipped if we're not supporting LDB. */
#if defined(LISP_FEATURE_SB_LDB)

#include "globals.h"
#include "vars.h"
#include "parse.h"
#include "os.h"
#include "interrupt.h"
#include "lispregs.h"
#include "print.h"
#include "arch.h"
#include "interr.h"
#include "gc.h"
#include "search.h"
#include "purify.h"

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

static cmd call_cmd, dump_cmd, print_cmd, quit_cmd, help_cmd;
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
#ifndef alpha
        printf("0x%08lX: ", (unsigned long) addr);
#else
        printf("0x%08X: ", (u32) addr);
#endif
        if (is_valid_lisp_addr((os_vm_address_t)addr)) {
#ifndef alpha
            unsigned long *lptr = (unsigned long *)addr;
#else
            u32 *lptr = (u32 *)addr;
#endif
            unsigned short *sptr = (unsigned short *)addr;
            unsigned char *cptr = (unsigned char *)addr;

            printf("0x%08lx   0x%04x 0x%04x   0x%02x 0x%02x 0x%02x 0x%02x    %c%c%c%c\n", lptr[0], sptr[0], sptr[1], cptr[0], cptr[1], cptr[2], cptr[3], visible(cptr[0]), visible(cptr[1]), visible(cptr[2]), visible(cptr[3]));
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
    kill(getpid(), parse_number(ptr));
}

static void
regs_cmd(char **ptr)
{
    printf("CSP\t=\t0x%08lX\n", (unsigned long)current_control_stack_pointer);
    printf("FP\t=\t0x%08lX\n", (unsigned long)current_control_frame_pointer);
#if !defined(__i386__)
    printf("BSP\t=\t0x%08X\n", (unsigned long)current_binding_stack_pointer);
#endif
#ifdef __i386__
    printf("BSP\t=\t0x%08lx\n",
	   (unsigned long)SymbolValue(BINDING_STACK_POINTER));
#endif

    printf("DYNAMIC\t=\t0x%08lx\n", (unsigned long)DYNAMIC_SPACE_START);
#if defined(__i386__)
    printf("ALLOC\t=\t0x%08lx\n",
	   (unsigned long)SymbolValue(ALLOCATION_POINTER));
    printf("TRIGGER\t=\t0x%08lx\n",
	   (unsigned long)SymbolValue(INTERNAL_GC_TRIGGER));
#else
    printf("ALLOC\t=\t0x%08X\n",
	   (unsigned long)dynamic_space_free_pointer);
    printf("TRIGGER\t=\t0x%08lx\n", (unsigned long)current_auto_gc_trigger);
#endif
    printf("STATIC\t=\t0x%08lx\n",
	   (unsigned long)SymbolValue(STATIC_SPACE_FREE_POINTER));
    printf("RDONLY\t=\t0x%08lx\n",
	   (unsigned long)SymbolValue(READ_ONLY_SPACE_FREE_POINTER));

#ifdef MIPS
    printf("FLAGS\t=\t0x%08x\n", current_flags_register);
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
        if (TypeOf(obj) == type_FunctionHeader)
            print((long)addr | type_FunctionPointer);
        else if (LowtagOf(obj) == type_OtherImmediate0 || LowtagOf(obj) == type_OtherImmediate1)
            print((lispobj)addr | type_OtherPointer);
        else
            print((lispobj)addr);
        if (count == -1)
            return;
    }
}

static void
call_cmd(char **ptr)
{
    lispobj thing = parse_lispobj(ptr), function, result = 0, cons, args[3];
    int numargs;

    if (LowtagOf(thing) == type_OtherPointer) {
	switch (TypeOf(*(lispobj *)(thing-type_OtherPointer))) {
	  case type_SymbolHeader:
	    for (cons = SymbolValue(INITIAL_FDEFN_OBJECTS);
		 cons != NIL;
		 cons = CONS(cons)->cdr) {
		if (FDEFN(CONS(cons)->car)->name == thing) {
		    thing = CONS(cons)->car;
		    goto fdefn;
		}
	    }
	    printf("Symbol 0x%08lx is undefined.\n", (long unsigned)thing);
	    return;

	  case type_Fdefn:
	  fdefn:
	    function = FDEFN(thing)->function;
	    if (function == NIL) {
		printf("Fdefn 0x%08lx is undefined.\n", (long unsigned)thing);
		return;
	    }
	    break;
	  default:
	    printf("0x%08lx is not a function pointer, symbol, "
		   "or fdefn object.\n",
		   (long unsigned)thing);
	    return;
	}
    }
    else if (LowtagOf(thing) != type_FunctionPointer) {
        printf("0x%08lx is not a function pointer, symbol, or fdefn object.\n",
	       (long unsigned)thing);
        return;
    }
    else
	function = thing;

    numargs = 0;
    while (more_p(ptr)) {
	if (numargs >= 3) {
	    printf("too many arguments (no more than 3 supported)\n");
	    return;
	}
	args[numargs++] = parse_lispobj(ptr);
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
}

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
    fgets(buf, sizeof(buf), ldb_in);
    if (buf[0] == 'y' || buf[0] == 'Y' || buf[0] == '\n')
        exit(0);
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
#ifdef __i386__
		brief_print((lispobj)(*os_context_register_addr(context,
								i*2)));
#else
		brief_print((lispobj)(*os_context_register_addr(context,i)));
#endif
	}
	printf("PC:\t\t  0x%08lx\n",
	       (unsigned long)(*os_context_pc_addr(context)));
}

static void
print_context_cmd(char **ptr)
{
	int free;

	free = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
	
        if (more_p(ptr)) {
		int index;

		index = parse_number(ptr);

		if ((index >= 0) && (index < free)) {
			printf("There are %d interrupt contexts.\n", free);
			printf("printing context %d\n", index);
			print_context(lisp_interrupt_contexts[index]);
		} else {
			printf("There aren't that many/few contexts.\n");
			printf("There are %d interrupt contexts.\n", free);
		}
	} else {
		if (free == 0)
			printf("There are no interrupt contexts!\n");
		else {
			printf("There are %d interrupt contexts.\n", free);
			printf("printing context %d\n", free - 1);
			print_context(lisp_interrupt_contexts[free - 1]);
		}
	}
}

static void
backtrace_cmd(char **ptr)
{
    void backtrace(int frames);
    int n;

    if (more_p(ptr))
	n = parse_number(ptr);
    else
	n = 100;

    printf("Backtrace:\n");
    backtrace(n);
}

static void
catchers_cmd(char **ptr)
{
    struct catch_block *catch;

    catch = (struct catch_block *)SymbolValue(CURRENT_CATCH_BLOCK);

    if (catch == NULL)
        printf("There are no active catchers!\n");
    else {
        while (catch != NULL) {
#ifndef __i386__
            printf("0x%08lX:\n\tuwp: 0x%08lX\n\tfp: 0x%08lX\n\tcode: 0x%08lx\n\tentry: 0x%08lx\n\ttag: ",
		   (unsigned long)catch, (unsigned long)(catch->current_uwp),
		   (unsigned long)(catch->current_cont),
		   catch->current_code,
		   catch->entry_pc);
#else
            printf("0x%08lX:\n\tuwp: 0x%08lX\n\tfp: 0x%08lX\n\tcode: 0x%08lx\n\tentry: 0x%08lx\n\ttag: ",
		   (unsigned long)catch, (unsigned long)(catch->current_uwp),
		   (unsigned long)(catch->current_cont),
		   (unsigned long)component_ptr_from_pc((void*)catch->entry_pc) +
		   type_OtherPointer,
		   (unsigned long)catch->entry_pc);
#endif
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
	ldb_in = fopen("/dev/tty","r+");
	ldb_in_fd = fileno(ldb_in);
    }

    while (!done) {
        printf("ldb> ");
        fflush(stdout);
        line = fgets(buf, sizeof(buf), ldb_in);
        if (line == NULL) {
	    if (isatty(ldb_in_fd)) {
		putchar('\n');
	        continue;
	    }
	    else {
		fprintf(stderr, "\nEOF on something other than a tty.\n");
		exit(0);
	    }
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

    printf("LDB monitor\n");

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
    fprintf(stderr, "There's no LDB in this build; exiting.\n");
    exit(1);
#endif
}
