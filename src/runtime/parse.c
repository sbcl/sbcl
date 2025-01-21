/* parsing for LDB monitor */

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
#include <stdlib.h>
#include <ctype.h>

#include "genesis/sbcl.h"
#ifdef LISP_FEATURE_WIN32
#include "pthreads_win32.h"
#else
#include <signal.h>
#endif
#include "runtime.h"

#include "globals.h"
#include "vars.h"
#include "parse.h"
#include "os.h"
#include "interrupt.h"
#include "lispregs.h"
#include "validate.h"
#include "arch.h"
#include "search.h"
#include "thread.h"

#include "genesis/closure.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"

static void skip_ws(char **ptr)
{
    while (**ptr <= ' ' && **ptr != '\0')
        (*ptr)++;
}

static bool string_to_long(char *token, uword_t *value)
{
    int base, digit;
    uword_t num;
    char *ptr;

    if (token == 0)
        return 0;

    if (token[0] == '0')
        if (token[1] == 'x' || token[1] == 'X') {
            base = 16;
            token += 2;
        }
        else {
            base = 8;
            token++;
        }
    else if (token[0] == '#') {
        switch (token[1]) {
            case 'x':
            case 'X':
                base = 16;
                token += 2;
                break;
            case 'o':
            case 'O':
                base = 8;
                token += 2;
                break;
            default:
                return 0;
        }
    }
    else
        base = 10;

    num = 0;
    ptr = token;
    while (*ptr != '\0') {
        if (*ptr >= 'a' && *ptr <= 'f')
            digit = *ptr + 10 - 'a';
        else if (*ptr >= 'A' && *ptr <= 'F')
            digit = *ptr + 10 - 'A';
        else if (*ptr >= '0' && *ptr <= '9')
            digit = *ptr - '0';
        else
            return 0;
        if (digit < 0 || digit >= base)
            return 0;

        ptr++;
        num = num * base + digit;
    }

    *value = num;
    return 1;
}

static bool lookup_variable(char *name, lispobj *result)
{
    struct var *var = lookup_by_name(name);

    if (var == NULL)
        return 0;
    else {
        *result = var_value(var);
        return 1;
    }
}


bool more_p(char **ptr)
{
    skip_ws(ptr);

    if (**ptr == '\0')
        return 0;
    else
        return 1;
}

char *parse_token(char **ptr)
{
    char *token;

    skip_ws(ptr);

    if (**ptr == '\0')
        return NULL;

    token = *ptr;

    while (**ptr > ' ')
        (*ptr)++;

    if (**ptr != '\0') {
        **ptr = '\0';
        (*ptr)++;
    }

    return token;
}

/// Return 1 for successful parse
int parse_number(char **ptr, int *output)
{
    char *token = parse_token(ptr);
    uword_t result;

    if (token == NULL) {
        printf("expected a number\n");
        return 0;
    }
    if (string_to_long(token, &result)) {
        // FIXME: consumers of this interface expect to receive an 'int',
        // but string_to_long yields a uword_t.
        // Should enforce the smaller range.
        *output = result;
        return 1;
    }
    printf("invalid number: ``%s''\n", token);
    return 0;
}

int parse_addr(char **ptr, bool safely, char **output, FILE* errstream)
{
    char *token = parse_token(ptr);
    lispobj result;

    if (token == NULL) {
        fprintf(errstream, "expected an address\n");
        return 0;
    }
    if (token[0] == '$') {
        if (!lookup_variable(token+1, &result)) {
            fprintf(errstream, "unknown variable: ``%s''\n", token);
            return 0;
        }
        result &= ~7; // LOWTAG_MASK maybe?
    } else {
        uword_t value;
        if (!string_to_long(token, &value)) {
            fprintf(errstream, "invalid number: ``%s''\n", token);
            return 0;
        }
        result = (value & ~3); // what is ~3 for - word alignment?
    }

    if (safely && !gc_managed_addr_p(result)) {
        fprintf(errstream, "invalid Lisp-level address: %p\n", (void *)result);
        return 0;
    }

    *output = (char *)result;
    return 1;
}

static lispobj lookup_symbol(char *name)
{
    uword_t ranges[][2] = {
      { STATIC_SPACE_OBJECTS_START, (uword_t)static_space_free_pointer },
#ifdef LISP_FEATURE_IMMOBILE_SPACE
      { FIXEDOBJ_SPACE_START, (uword_t)fixedobj_free_pointer },
#endif
#ifdef LISP_FEATURE_GENERATIONAL
      { DYNAMIC_SPACE_START, dynamic_space_highwatermark() }
#else
      { (uword_t)current_dynamic_space, (uword_t)get_alloc_pointer() }
#endif
    };

    lispobj *headerptr;
    unsigned i;
    for (i=0; i<(sizeof ranges/(2*sizeof (uword_t))); ++i)
        if ((headerptr = search_for_symbol(name, ranges[i][0], ranges[i][1], 0)))
            return make_lispobj(headerptr, OTHER_POINTER_LOWTAG);
    // Try again, case-insensitively
    for (i=0; i<(sizeof ranges/(2*sizeof (uword_t))); ++i)
        if ((headerptr = search_for_symbol(name, ranges[i][0], ranges[i][1], 1)))
            return make_lispobj(headerptr, OTHER_POINTER_LOWTAG);
    return 0;
}

static int parse_regnum(char *s)
{
    // Well this is confusing as heck! Registers r0..r9 in the AARCH64 vm definition
    // are machine regs 10 through 19. So caveat emptor if you use this syntax.
    if ((s[1] == 'R') || (s[1] == 'r')) {
        int regnum;

        if (s[2] == '\0')
            return -1;

        /* skip the $R part and call atoi on the number */
        regnum = atoi(s + 2);
        if ((regnum >= 0) && (regnum < NREGS))
            return regnum;
        else
            return -1;
    } else {
        int i;

        for (i = 0; i < NREGS ; i++)
            if (strcasecmp(s + 1, lisp_register_names[i]) == 0)
#ifdef LISP_FEATURE_X86
                return i*2;
#else
        return i;
#endif

        return -1;
    }
}

int parse_lispobj(char **ptr, lispobj *output)
{
    struct thread *thread = get_sb_vm_thread();
    char *token = parse_token(ptr);
    uword_t pointer;
    lispobj result;
    uword_t value;

    if (token == NULL) {
        printf("expected an object\n");
        return 0;
    }
    if (token[0] == '$') {
        if (isalpha(token[1])) {
            int free_ici;
            int regnum;
            os_context_t *context;

            free_ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,thread));

            if (free_ici == 0) {
                printf("Variable ``%s'' is not valid -- there is no current interrupt context.\n", token);
                return 0;
            }

            context = nth_interrupt_context(free_ici - 1, thread);

            regnum = parse_regnum(token);
            if (regnum < 0) {
                printf("bogus register: ``%s''\n", token);
                return 0;
            }

            result = *os_context_register_addr(context, regnum);
        } else if (!lookup_variable(token+1, &result)) {
            printf("unknown variable: ``%s''\n", token);
            return 0;
        }
    } else if (token[0] == '@') {
        if (string_to_long(token+1, &pointer)) {
            // More wtf - are we trying to round down to a double-lispword
            // or to a lispword? I don't know.
            // This '@' input syntax is just plain useless anyway.
            pointer &= ~3;
            if (gc_managed_addr_p(pointer))
                result = *(lispobj *)pointer;
            else {
                printf("invalid Lisp-level address: ``%s''\n", token+1);
                return 0;
            }
        }
        else {
            printf("invalid address: ``%s''\n", token+1);
            return 0;
        }
    }
    else if (string_to_long(token, &value))
        result = value;
    else if ((result = lookup_symbol(token)) != 0)
        ;
    else {
        printf("invalid Lisp object: ``%s''\n", token);
        return 0;
    }

    *output = result;
    return 1;
}
