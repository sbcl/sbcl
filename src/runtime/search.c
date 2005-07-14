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

#include <string.h>

#include "sbcl.h"
#include "runtime.h"
#include "os.h"
#include "search.h"
#include "thread.h"
#include "genesis/primitive-objects.h"

boolean search_for_type(int type, lispobj **start, int *count)
{
    lispobj obj, *addr;

    while ((*count == -1 || (*count > 0)) &&
           is_valid_lisp_addr((os_vm_address_t)*start)) {
        obj = **start;
        addr = *start;
        if (*count != -1)
            *count -= 2;

        if (widetag_of(obj) == type)
            return 1;

        (*start) += 2;
    }
    return 0;
}

boolean search_for_symbol(char *name, lispobj **start, int *count)
{
    struct symbol *symbol;
    struct vector *symbol_name;

    while (search_for_type(SYMBOL_HEADER_WIDETAG, start, count)) {
        symbol = (struct symbol *)native_pointer((lispobj)*start);
        if (lowtag_of(symbol->name) == OTHER_POINTER_LOWTAG) {
            symbol_name = (struct vector *)native_pointer(symbol->name);
            if (is_valid_lisp_addr((os_vm_address_t)symbol_name) &&
                /* FIXME: Broken with more than one type of string
                   (i.e. even broken given (VECTOR NIL) */
                widetag_of(symbol_name->header) == SIMPLE_BASE_STRING_WIDETAG &&
                strcmp((char *)symbol_name->data, name) == 0)
                return 1;
        }
        (*start) += 2;
    }
    return 0;
}
