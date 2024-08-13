/*
 * Coalescing of constant vectors for SAVE-LISP-AND-DIE
 */

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

#include <stdbool.h>
#include "genesis/sbcl.h"
#include "gc.h"
#include "genesis/vector.h"
#include "genesis/gc-tables.h"
#include "genesis/instance.h"
#include "genesis/symbol.h"
#include "immobile-space.h"
#include "hopscotch.h"
#include "code.h"
#include "genesis/static-symbols.h"
#include "validate.h"
#include "var-io.h"

static bool gcable_pointer_p(lispobj pointer)
{
#ifdef LISP_FEATURE_CHENEYGC
   return pointer >= (lispobj)current_dynamic_space
       && pointer < (lispobj)get_alloc_pointer();
#endif
#ifdef LISP_FEATURE_GENERATIONAL
   return find_page_index((void*)pointer) >= 0 || immobile_space_p(pointer);
#endif
}

static bool coalescible_number_p(lispobj* where)
{
    int widetag = widetag_of(where);
    return widetag == BIGNUM_WIDETAG
        // Ratios and complex integers containing pointers to bignums don't work.
        || ((widetag == RATIO_WIDETAG || widetag == COMPLEX_RATIONAL_WIDETAG)
            && fixnump(where[1]) && fixnump(where[2]))
#ifndef LISP_FEATURE_64_BIT
        || widetag == SINGLE_FLOAT_WIDETAG
#endif
        || widetag == DOUBLE_FLOAT_WIDETAG
        || widetag == COMPLEX_SINGLE_FLOAT_WIDETAG
        || widetag == COMPLEX_DOUBLE_FLOAT_WIDETAG;
}

/// Return true of fixnums, bignums, strings, symbols.
/// Strings are considered eql-comparable,
/// because they're coalesced before comparing.
static bool eql_comparable_p(lispobj obj)
{
    if (fixnump(obj) || obj == NIL) return 1;
    if (lowtag_of(obj) != OTHER_POINTER_LOWTAG) return 0;
    int widetag = widetag_of(native_pointer(obj));
    return widetag == BIGNUM_WIDETAG
        || widetag == SYMBOL_WIDETAG
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
        || widetag == SIMPLE_CHARACTER_STRING_WIDETAG
#endif
        || widetag == SIMPLE_BASE_STRING_WIDETAG;
}

static bool vector_isevery(bool (*pred)(lispobj), struct vector* v)
{
    int i;
    for (i = vector_len(v)-1; i >= 0; --i)
        if (!pred(v->data[i])) return 0;
    return 1;
}

/* FIXME: we should actually be even more careful about coalescing objects
 * that appear as keys in hash-tables.  While we do take the precaution of
 * updating the need-to-rehash indicator, we might create keys that compare
 * the same under the table's comparator.  It seems like doing that could
 * cause various kinds of weirdness in some applications. Nobody has reported
 * misbehavior in the 3 years or so that coalescing has been the default,
 * so it doesn't seem horribly bad, but does seem a bit broken */
static void coalesce_obj(lispobj* where, struct hopscotch_table* ht)
{
    lispobj ptr = *where;
    if (lowtag_of(ptr) != OTHER_POINTER_LOWTAG || !gc_managed_heap_space_p(ptr))
        return;

    extern char gc_coalesce_string_literals;
    // gc_coalesce_string_literals represents the "aggressiveness" level.
    // If 1, then we share vectors tagged as +VECTOR-SHAREABLE+,
    // but if >1, those and also +VECTOR-SHAREABLE-NONSTD+.
    int mask = gc_coalesce_string_literals > 1
      ? (VECTOR_SHAREABLE|VECTOR_SHAREABLE_NONSTD)<<ARRAY_FLAGS_POSITION
      : (VECTOR_SHAREABLE                        )<<ARRAY_FLAGS_POSITION;

    lispobj* obj = native_pointer(ptr);
    lispobj header = *obj;
    int widetag = header_widetag(header);

    if ((((header & mask) != 0) // optimistically assume it's a vector
         && ((widetag == SIMPLE_VECTOR_WIDETAG
              && vector_isevery(eql_comparable_p, (struct vector*)obj))
             || specialized_vector_widetag_p(widetag)))
        || coalescible_number_p(obj)) {
        if (widetag == SIMPLE_VECTOR_WIDETAG) {
            struct vector* v = (void*)obj;
            sword_t n_elts = vector_len(v), i;
            for (i = 0 ; i < n_elts ; ++i) coalesce_obj(v->data+i, ht);
        }
        int index = hopscotch_get(ht, (uword_t)obj, 0);
        if (!index) // Not found
            hopscotch_insert(ht, (uword_t)obj, 1);
        else {
            ptr = make_lispobj((void*)ht->keys[index-1],
                               OTHER_POINTER_LOWTAG);
            // Check for no read-only to dynamic-space pointer
            if ((uintptr_t)where >= READ_ONLY_SPACE_START &&
                (uintptr_t)where < READ_ONLY_SPACE_END &&
                gcable_pointer_p(ptr))
                lose("Coalesce produced RO->DS ptr");
            *where = ptr;
        }
    }
}

/* FIXME: there are 10+ variants of the skeleton of an object traverser.
 * Pick one and try to make it customizable. I tried a callback-based approach,
 * but it's way too slow. Next best thing is a ".inc" file which defines the shape
 * of the function, with pieces inserted by #define.
 *
 * (1) gc-common's table-based mechanism
 * (2) gencgc's verify_range()
 * (3) immobile space {fixedobj,text}_points_to_younger_p()
 *     and fixup_space() for defrag. [and the table-based thing is used too]
 * (4) fullcgc's trace_object()
 * (5) coreparse's relocate_space()
 * (6) traceroot's find_ref() and build_refs() which itself has two modes
 * (7) sanity_check_loaded_core() which is quite possibly the prettiest yet
 * (8) purify()
 * (9) coalesce_range()
 * plus the Lisp variant:
 * (10) do-referenced-object which thank goodness is common to 2 uses
 * and if you want to count 'print.c' as another, there's that.
 * There's also cheneygc's print_garbage() which uses the dispatch tables.
 * And now there's update_writeprotection() which is also ad-hoc.
 */

static uword_t coalesce_range(lispobj* where, lispobj* limit, uword_t arg)
{
    struct hopscotch_table* ht = (struct hopscotch_table*)arg;
    sword_t nwords, i;

    where = next_object(where, 0, limit);
    while (where) {
        lispobj word = *where;
        if (is_header(word)) {
            int widetag = header_widetag(word);
            nwords = sizetab[widetag](where);
            lispobj *next = next_object(where, nwords, limit);
            if (leaf_obj_widetag_p(widetag)) {
              // Ignore this object.
              where = next;
              continue;
            }
            sword_t coalesce_nwords = nwords;
            if (instanceoid_widetag_p(widetag)) {
                lispobj layout = layout_of(where);
                struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
                for (i=0; i<(nwords-1); ++i)
                    if (bitmap_logbitp(i, bitmap)) coalesce_obj(where+1+i, ht);
                where = next;
                continue;
            }
            switch (widetag) {
            case SYMBOL_WIDETAG:
            {
                struct symbol* symbol = (void*)where;
                lispobj name = decode_symbol_name(symbol->name);
                coalesce_obj(&name, ht);
                set_symbol_name(symbol, name);
                where = next;
                continue;
            }
            case CODE_HEADER_WIDETAG:
                coalesce_nwords = code_header_words((struct code*)where);
                break;
            }
            for(i=1; i<coalesce_nwords; ++i)
                coalesce_obj(where+i, ht);
            where = next;
        } else {
            nwords = 2;
            coalesce_obj(where+0, ht);
            coalesce_obj(where+1, ht);
            where = next_object(where, 2, limit);
        }
    }
    return 0;
}

/* Do as good as job as we can to de-duplicate strings
 * This doesn't need to scan stacks or anything fancy.
 * It's not wrong to fail to coalesce things that could have been */
void coalesce_similar_objects()
{
    struct hopscotch_table ht;
    uword_t arg = (uword_t)&ht;

    hopscotch_create(&ht, HOPSCOTCH_VECTOR_HASH, 0, 1<<17, 0);
    coalesce_range((lispobj*)READ_ONLY_SPACE_START, read_only_space_free_pointer, arg);
    lispobj* the_symbol_nil = (lispobj*)(NIL - LIST_POINTER_LOWTAG - N_WORD_BYTES);
    coalesce_range(the_symbol_nil, ALIGN_UP(SYMBOL_SIZE,2) + the_symbol_nil, arg);
    coalesce_range((lispobj*)STATIC_SPACE_OBJECTS_START, static_space_free_pointer, arg);
    if (PERMGEN_SPACE_START)
        coalesce_range((lispobj*)PERMGEN_SPACE_START, permgen_space_free_pointer, arg);

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    coalesce_range((lispobj*)FIXEDOBJ_SPACE_START, fixedobj_free_pointer, arg);
    coalesce_range((lispobj*)TEXT_SPACE_START, text_space_highwatermark, arg);
#endif
#ifdef LISP_FEATURE_GENERATIONAL
    walk_generation(coalesce_range, -1, arg);
#else
    coalesce_range(current_dynamic_space, get_alloc_pointer(), arg);
#endif
    hopscotch_destroy(&ht);
}
