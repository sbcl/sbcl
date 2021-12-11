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
#include <ctype.h>

#include "sbcl.h"
#include "runtime.h"
#include "os.h"
#include "search.h"
#include "thread.h"
#include "gc-internal.h"
#include "genesis/primitive-objects.h"
#include "genesis/hash-table.h"
#include "genesis/package.h"
#include "getallocptr.h" // for get_alloc_pointer()

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
    lispobj *start = (lispobj *)STATIC_SPACE_OBJECTS_START;
    lispobj *end = static_space_free_pointer;
    if ((pointer < (void *)start) || (pointer >= (void *)end))
        return NULL;
    return gc_search_space(start, pointer);
}

lispobj *search_all_gc_spaces(void *pointer)
{
    lispobj *start;
    if (((start = search_dynamic_space(pointer)) != NULL) ||
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        ((start = search_immobile_space(pointer)) != NULL) ||
#endif
        ((start = search_static_space(pointer)) != NULL) ||
        ((start = search_read_only_space(pointer)) != NULL))
        return start;
    return NULL;
}

static int __attribute__((unused)) strcmp_ucs4_ascii(uint32_t* a, unsigned char* b,
                                                     boolean ignore_case)
{
    int i = 0;

  // Lisp terminates UCS4 strings with NULL bytes - probably to no avail
  // since null-terminated UCS4 isn't a common convention for any foreign ABI -
  // but length has been pre-checked, so hitting an ASCII null is a win.
    if (ignore_case) {
        while (toupper(a[i]) == toupper(b[i]))
            if (b[i] == 0)
                return 0;
            else
                ++i;
    } else {
        while (a[i] == b[i])
            if (b[i] == 0)
                return 0;
            else
                ++i;
    }
    return a[i] - b[i]; // same return convention as strcmp()
}

struct symbol_search {
    char *name;
    boolean ignore_case;
};
static uword_t search_symbol_aux(lispobj* start, lispobj* end, uword_t arg)
{
    struct symbol_search* ss = (struct symbol_search*)arg;
    return (uword_t)search_for_symbol(ss->name, (lispobj)start, (lispobj)end, ss->ignore_case);
}
lispobj* search_for_symbol(char *name, lispobj start, lispobj end, boolean ignore_case)
{
    lispobj* where = (lispobj*)start;
    lispobj* limit = (lispobj*)end;
    lispobj namelen = make_fixnum(strlen(name));

#ifdef LISP_FEATURE_GENCGC
    // This function was never safe to use on pages that were dirtied with unboxed words.
    // It has become even less safe now that don't prezero most pages during GC,
    // because we will certainly encounter remnants of forwarding pointers etc.
    // So if the specified range is all of dynamic space, defer to the space walker.
    if (start == DYNAMIC_SPACE_START && end == (uword_t)get_alloc_pointer()) {
        struct symbol_search ss = {name, ignore_case};
        return (lispobj*)walk_generation(search_symbol_aux, -1, (uword_t)&ss);
    }
#endif
    while (where < limit) {
        lispobj word = *where;
        struct vector *string;
        if (header_widetag(word) == SYMBOL_WIDETAG &&
            (string = symbol_name((struct symbol*)where)) != 0 &&
            string->length_ == namelen) {
            if (gc_managed_addr_p((lispobj)string) &&
                ((widetag_of(&string->header) == SIMPLE_BASE_STRING_WIDETAG
                  && !(ignore_case ? strcasecmp : strcmp)((char *)string->data, name))
#ifdef LISP_FEATURE_SB_UNICODE
                 || (widetag_of(&string->header) == SIMPLE_CHARACTER_STRING_WIDETAG
                     && !strcmp_ucs4_ascii((uint32_t*)string->data,
                                           (unsigned char*)name, ignore_case))
#endif
                 ))
                return where;
        }
        where += OBJECT_SIZE(word, where);
    }
    return 0;
}

/// This unfortunately entails a heap scan,
/// but it's quite fast if the symbol is found in immobile space.
#ifdef LISP_FEATURE_SB_THREAD
struct symbol* lisp_symbol_from_tls_index(lispobj tls_index)
{
    lispobj* where = 0;
    lispobj* end = 0;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    where = (lispobj*)FIXEDOBJ_SPACE_START;
    end = fixedobj_free_pointer;
#endif
    while (1) {
        while (where < end) {
            lispobj header = *where;
            int widetag = header_widetag(header);
            if (widetag == SYMBOL_WIDETAG &&
                tls_index_of(((struct symbol*)where)) == tls_index)
                return (struct symbol*)where;
            where += OBJECT_SIZE(header, where);
        }
        if (where >= (lispobj*)DYNAMIC_SPACE_START)
            break;
        where = (lispobj*)DYNAMIC_SPACE_START;
        end = (lispobj*)get_alloc_pointer();
    }
    return 0;
}
#endif

static boolean sym_stringeq(lispobj sym, const char *string, int len)
{
    struct vector* name = symbol_name(SYMBOL(sym));
    return widetag_of(&name->header) == SIMPLE_BASE_STRING_WIDETAG
        && vector_len(name) == len
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
        gc_assert(widetag_of(&symbols->header) == INSTANCE_WIDETAG);
        struct vector* cells = VECTOR(symbols->slots[INSTANCE_DATA_START]);
        gc_assert(widetag_of(&cells->header) == SIMPLE_VECTOR_WIDETAG);
        lispobj namelen = strlen(symbol_name);
        int cells_length = vector_len(cells);
        int index = *hint >> 1;
        if (index >= cells_length)
            index = 0; // safeguard against vector shrinkage
        int initial_index = index;
        do {
            lispobj thing = cells->data[index];
            if (lowtag_of(thing) == OTHER_POINTER_LOWTAG
                && widetag_of(&SYMBOL(thing)->header) == SYMBOL_WIDETAG
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

lispobj* find_symbol(char* symbol_name, lispobj package, unsigned int* hint)
{
    return package ? search_package_symbols(package, symbol_name, hint) : 0;
}
