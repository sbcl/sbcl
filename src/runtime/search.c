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
    lispobj obj;

    while ((*count == -1 || (*count > 0)) &&
           is_valid_lisp_addr((os_vm_address_t)*start)) {
        obj = **start;
        if (*count != -1)
            *count -= 2;

        if (widetag_of(obj) == type)
            return 1;

        (*start) += 2;
    }
    return 0;
}

static int strcmp_ucs4_ascii(uint32_t* a, char* b)
{
  int i = 0;

  // Lisp terminates UCS4 strings with NULL bytes - probably to no avail
  // since null-terminated UCS4 isn't a common convention for any foreign ABI -
  // but length has been pre-checked, so hitting an ASCII null is a win.
  while (a[i] == ((unsigned char*)b)[i])
    if (b[i] == 0)
      return 0;
    else
      ++i;
  return a[i] - b[i]; // same return convention as strcmp()
}

boolean search_for_symbol(char *name, lispobj **start, int *count)
{
    struct symbol *symbol;
    struct vector *symbol_name;
    int namelen = strlen(name);

    while (search_for_type(SYMBOL_HEADER_WIDETAG, start, count)) {
        symbol = (struct symbol *)native_pointer((lispobj)*start);
        if (lowtag_of(symbol->name) == OTHER_POINTER_LOWTAG) {
            symbol_name = (struct vector *)native_pointer(symbol->name);
            if (is_valid_lisp_addr((os_vm_address_t)symbol_name) &&
                /* FIXME: Broken with more than one type of string
                   (i.e. even broken given (VECTOR NIL) */
                ((widetag_of(symbol_name->header) == SIMPLE_BASE_STRING_WIDETAG
                  && fixnum_value(symbol_name->length) == namelen
                  && !strcmp((char *)symbol_name->data, name))
#ifdef LISP_FEATURE_SB_UNICODE
                 || (widetag_of(symbol_name->header) == SIMPLE_CHARACTER_STRING_WIDETAG
                     && fixnum_value(symbol_name->length) == namelen
                     && !strcmp_ucs4_ascii((uint32_t*)symbol_name->data, name))
#endif
                 ))
                return 1;
        }
        (*start) += 2;
    }
    return 0;
}
