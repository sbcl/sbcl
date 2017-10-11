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
#include "gc-internal.h"
#include "genesis/primitive-objects.h"
#include "genesis/hash-table.h"
#include "genesis/package.h"

lispobj *
search_read_only_space(void *pointer)
{
    lispobj *start = (lispobj *) READ_ONLY_SPACE_START;
    lispobj *end = read_only_space_free_pointer;
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return gc_search_space(start, pointer);
}

lispobj *
search_static_space(void *pointer)
{
    lispobj *start = (lispobj *)STATIC_SPACE_START;
    lispobj *end = static_space_free_pointer;
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return gc_search_space(start, pointer);
}

boolean search_for_type(int type, lispobj **start, int *count)
{
    lispobj obj;

    while ((*count == -1 || (*count > 0)) &&
           gc_managed_addr_p((lispobj)*start)) {
        obj = **start;
        if (*count != -1)
            *count -= 2;

        if (widetag_of(obj) == type)
            return 1;

        (*start) += 2;
    }
    return 0;
}

static int __attribute__((unused)) strcmp_ucs4_ascii(uint32_t* a, char* b)
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

lispobj* search_for_symbol(char *name, lispobj start, lispobj end)
{
    lispobj* where = (lispobj*)start;
    lispobj* limit = (lispobj*)end;
    struct symbol *symbol;
    lispobj namelen = make_fixnum(strlen(name));

    while (where < limit) {
        lispobj word = *where;
        if (widetag_of(word) == SYMBOL_WIDETAG &&
            lowtag_of((symbol = (struct symbol *)where)->name) == OTHER_POINTER_LOWTAG) {
            struct vector *symbol_name = VECTOR(symbol->name);
            if (gc_managed_addr_p((lispobj)symbol_name) &&
                ((widetag_of(symbol_name->header) == SIMPLE_BASE_STRING_WIDETAG
                  && symbol_name->length == namelen
                  && !strcmp((char *)symbol_name->data, name))
#ifdef LISP_FEATURE_SB_UNICODE
                 || (widetag_of(symbol_name->header) == SIMPLE_CHARACTER_STRING_WIDETAG
                     && symbol_name->length == namelen
                     && !strcmp_ucs4_ascii((uint32_t*)symbol_name->data, name))
#endif
                 ))
                return where;
        }
        where += OBJECT_SIZE(word, where);
    }
    return 0;
}

static boolean sym_stringeq(lispobj sym, const char *string, int len)
{
    struct vector* name = VECTOR(SYMBOL(sym)->name);
    return widetag_of(name->header) == SIMPLE_BASE_STRING_WIDETAG
        && name->length == make_fixnum(len)
        && !memcmp(name->data, string, len);
}

static lispobj* search_package_symbols(lispobj package, char* symbol_name,
                                       unsigned int* hint)
{
    // Since we don't have Lisp's string hash algorithm in C, we can only
    // scan linearly, using the 'hint' as a starting point.
    struct package* pkg = (struct package*)(package - INSTANCE_POINTER_LOWTAG);
    int table_selector = *hint & 1, iteration;
    for (iteration = 1; iteration <= 2; ++iteration) {
        struct instance* symbols = (struct instance*)
          native_pointer(table_selector ? pkg->external_symbols : pkg->internal_symbols);
        gc_assert(widetag_of(symbols->header) == INSTANCE_WIDETAG);
        struct vector* cells = VECTOR(symbols->slots[INSTANCE_DATA_START]);
        gc_assert(widetag_of(cells->header) == SIMPLE_VECTOR_WIDETAG);
        lispobj namelen = strlen(symbol_name);
        int cells_length = fixnum_value(cells->length);
        int index = *hint >> 1;
        if (index >= cells_length)
            index = 0; // safeguard against vector shrinkage
        int initial_index = index;
        do {
            lispobj thing = cells->data[index];
            if (lowtag_of(thing) == OTHER_POINTER_LOWTAG
                && widetag_of(SYMBOL(thing)->header) == SYMBOL_WIDETAG
                && sym_stringeq(thing, symbol_name, namelen)) {
                *hint = (index << 1) | table_selector;
                return (lispobj*)SYMBOL(thing);
            }
            index = (index + 1) % cells_length;
        } while (index != initial_index);
        table_selector = table_selector ^ 1;
    }
    return 0;
}

lispobj* find_symbol(char* symbol_name, char* package_name, unsigned int* hint)
{
    // Use SB-KERNEL::SUB-GC to get a hold of the SB-KERNEL package,
    // which contains the symbol *PACKAGE-NAMES*.
    static unsigned int kernelpkg_hint;
    lispobj kernel_package = SYMBOL(FDEFN(SUB_GC_FDEFN)->name)->package;
    lispobj* package_names = search_package_symbols(kernel_package, "*PACKAGE-NAMES*",
                                                    &kernelpkg_hint);
    lispobj namelen = strlen(package_name);
    struct hash_table* names = (struct hash_table*)
      native_pointer(((struct symbol*)package_names)->value);
    struct vector* cells = (struct vector*)native_pointer(names->table);
    int i;
    // Search *PACKAGE-NAMES* for the package
    for (i=2; i<fixnum_value(cells->length); i += 2) {
        lispobj element = cells->data[i];
        if (is_lisp_pointer(element)) {
            struct vector* string = (struct vector*)native_pointer(element);
            if (widetag_of(string->header) == SIMPLE_BASE_STRING_WIDETAG
                && string->length == make_fixnum(namelen)
                && !memcmp(string->data, package_name, namelen)) {
                element = cells->data[i+1];
                if (lowtag_of(element) == LIST_POINTER_LOWTAG)
                    element = CONS(element)->car;
                return search_package_symbols(element, symbol_name, hint);
            }
        }
    }
    return 0;
}
