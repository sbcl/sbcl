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

#include "runtime.h"
#include "sbcl.h"
#include "os.h"
#include "search.h"

boolean search_for_type(int type, lispobj **start, int *count)
{
    lispobj obj, *addr;

    while ((*count == -1 || (*count > 0)) &&
	   is_valid_lisp_addr((os_vm_address_t)*start)) {
        obj = **start;
        addr = *start;
        if (*count != -1)
            *count -= 2;

        if (TypeOf(obj) == type)
            return 1;

        (*start) += 2;
    }
    return 0;
}

boolean search_for_symbol(char *name, lispobj **start, int *count)
{
    struct symbol *symbol;
    struct vector *symbol_name;

    while (search_for_type(type_SymbolHeader, start, count)) {
        symbol = (struct symbol *)native_pointer((lispobj)*start);
	if (LowtagOf(symbol->name) == type_OtherPointer) {
            symbol_name = (struct vector *)native_pointer(symbol->name);
            if (is_valid_lisp_addr((os_vm_address_t)symbol_name) &&
		TypeOf(symbol_name->header) == type_SimpleString &&
		strcmp((char *)symbol_name->data, name) == 0)
                return 1;
	}
        (*start) += 2;
    }
    return 0;
}
