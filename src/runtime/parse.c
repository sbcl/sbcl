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

/*
 * $Header$
 */

#include <stdio.h>
#include <ctype.h>
#include <signal.h>

#include "runtime.h"
#include "sbcl.h"
#include "globals.h"
#include "vars.h"
#include "parse.h"
#include "os.h"
#include "interrupt.h"
#include "lispregs.h"
#include "monitor.h"
#include "arch.h"
#include "search.h"

static void skip_ws(char **ptr)
{
    while (**ptr <= ' ' && **ptr != '\0')
        (*ptr)++;
}

static boolean string_to_long(char *token, long *value)
{
    int base, digit;
    long num;
    char *ptr;

    if (token == 0)
        return 0;

    if (token[0] == '0')
        if (token[1] == 'x') {
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

static boolean lookup_variable(char *name, lispobj *result)
{
    struct var *var = lookup_by_name(name);

    if (var == NULL)
        return 0;
    else {
        *result = var_value(var);
        return 1;
    }
}


boolean more_p(ptr)
char **ptr;
{
    skip_ws(ptr);

    if (**ptr == '\0')
        return 0;
    else
        return 1;
}

char *parse_token(ptr)
char **ptr;
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

#if 0
static boolean number_p(token)
char *token;
{
    char *okay;

    if (token == NULL)
        return 0;

    okay = "abcdefABCDEF987654321d0";

    if (token[0] == '0')
        if (token[1] == 'x' || token[1] == 'X')
            token += 2;
        else {
            token++;
            okay += 14;
        }
    else if (token[0] == '#') {
        switch (token[1]) {
            case 'x':
            case 'X':
                break;
            case 'o':
            case 'O':
                okay += 14;
                break;
            default:
                return 0;
        }
    }
    else
        okay += 12;

    while (*token != '\0')
        if (index(okay, *token++) == NULL)
            return 0;
    return 1;
}
#endif

long parse_number(ptr)
char **ptr;
{
    char *token = parse_token(ptr);
    long result;

    if (token == NULL) {
        printf("expected a number\n");
        throw_to_monitor();
    }
    else if (string_to_long(token, &result))
        return result;
    else {
        printf("invalid number: ``%s''\n", token);
        throw_to_monitor();
    }
    return 0;
}

char *parse_addr(ptr)
char **ptr;
{
    char *token = parse_token(ptr);
    long result;

    if (token == NULL) {
        printf("expected an address\n");
        throw_to_monitor();
    }
    else if (token[0] == '$') {
        if (!lookup_variable(token+1, (lispobj *)&result)) {
            printf("unknown variable: ``%s''\n", token);
            throw_to_monitor();
        }
        result &= ~7;
    }
    else {
        if (!string_to_long(token, &result)) {
            printf("invalid number: ``%s''\n", token);
            throw_to_monitor();
        }
        result &= ~3;
    }

    if (!is_valid_lisp_addr((os_vm_address_t)result)) {
        printf("invalid Lisp-level address: 0x%lx\n", result);
        throw_to_monitor();
    }

    return (char *)result;
}

static boolean lookup_symbol(char *name, lispobj *result)
{
    int count;
    lispobj *headerptr;

    /* Search static space. */
    headerptr = static_space;
    count = ((lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER) -
	     static_space);
    if (search_for_symbol(name, &headerptr, &count)) {
        *result = (lispobj)headerptr | type_OtherPointer;
        return 1;
    }

    /* Search dynamic space. */
    headerptr = current_dynamic_space;
#if !defined(ibmrt) && !defined(__i386__)
    count = current_dynamic_space_free_pointer - current_dynamic_space;
#else
    count = (lispobj *)SymbolValue(ALLOCATION_POINTER) - current_dynamic_space;
#endif
    if (search_for_symbol(name, &headerptr, &count)) {
        *result = (lispobj)headerptr | type_OtherPointer;
        return 1;
    }

    return 0;
}

static int
parse_regnum(char *s)
{
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
#ifdef __i386__
				return i*2;
#else
				return i;
#endif
		
		return -1;
	}
}

lispobj parse_lispobj(ptr)
char **ptr;
{
    char *token = parse_token(ptr);
    long pointer;
    lispobj result;

    if (token == NULL) {
        printf("expected an object\n");
        throw_to_monitor();
    } else if (token[0] == '$') {
	if (isalpha(token[1])) {
		int free;
		int regnum;
		os_context_t *context;

		free = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;

		if (free == 0) {
			printf("Variable ``%s'' is not valid -- there is no current interrupt context.\n", token);
			throw_to_monitor();
		}

		context = lisp_interrupt_contexts[free - 1];

		regnum = parse_regnum(token);
		if (regnum < 0) {
			printf("bogus register: ``%s''\n", token);
			throw_to_monitor();
		}

		result = *os_context_register_addr(context, regnum);
	} else if (!lookup_variable(token+1, &result)) {
            printf("unknown variable: ``%s''\n", token);
            throw_to_monitor();
        }
    } else if (token[0] == '@') {
        if (string_to_long(token+1, &pointer)) {
            pointer &= ~3;
            if (is_valid_lisp_addr((os_vm_address_t)pointer))
                result = *(lispobj *)pointer;
            else {
                printf("invalid Lisp-level address: ``%s''\n", token+1);
                throw_to_monitor();
            }
        }
        else {
            printf("invalid address: ``%s''\n", token+1);
            throw_to_monitor();
        }
    }
    else if (string_to_long(token, (long *)&result))
        ;
    else if (lookup_symbol(token, &result))
        ;
    else {
        printf("invalid Lisp object: ``%s''\n", token);
        throw_to_monitor();
    }

    return result;
}
