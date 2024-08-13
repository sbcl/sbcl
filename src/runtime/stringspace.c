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

#include "code.h"
#include "gc.h"
#include "genesis/gc-tables.h"
#include "genesis/instance.h"
#include "genesis/static-symbols.h"
#include "genesis/symbol.h"
#include "genesis/vector.h"
#include "globals.h"
#include "validate.h"
#include <stdio.h>
#include <stdlib.h>

#ifdef LISP_FEATURE_DARWIN_JIT

void prepare_readonly_space(__attribute__((unused)) int purify,
                            __attribute__((unused)) int print) {}
void move_rospace_to_dynamic(__attribute__((unused)) int print) {}

#else

/* This visitor is mostly like all the others, but only bothers with words that
 * can possibly be adjusted to point to or from readonly space.
 * So there's no dealing with fdefns at all, or closure-fun, funcallable-instance-fun,
 * or a few other things. */
static void visit_pointer_words(lispobj* object, lispobj (*func)(lispobj, uword_t), uword_t arg)
{
#define FIX(what) { lispobj ptr = what; \
    if (is_lisp_pointer(ptr) && gc_managed_heap_space_p(ptr)) { lispobj new = func(ptr, arg); if (new != ptr) what = new; } }

    if (is_cons_half(*object)) {
        FIX(object[0]);
        FIX(object[1]);
        return;
    }
    int widetag = widetag_of(object);
    if (leaf_obj_widetag_p(widetag)) return;
    if (instanceoid_widetag_p(widetag)) {
        lispobj layout = layout_of(object);
        if (layout) {
            struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
            int nslots = instanceoid_length(*object), i;
            for (i=0; i<nslots; ++i) if (bitmap_logbitp(i, bitmap)) FIX(object[i+1]);
        }
    } else if (widetag == SIMPLE_VECTOR_WIDETAG) {
        struct vector* v = (void*)object;
        sword_t len = vector_len(v), i;
        int rehash = 0;
        for (i=0; i<len; ++i) {
            lispobj old = v->data[i];
            if (!(is_lisp_pointer(old) && gc_managed_heap_space_p(old))) continue;
            lispobj new = func(old, arg);
            if (new != old) v->data[i] = new, rehash = 1;
        }
        // The 'rehash' bit on address-sensitive hashing vectors needs to be set if
        // an address-sensitive key moved, because it won't move again in final GC.
        if (vector_flagp(*object, VectorAddrHashing) && rehash) v->data[1] = make_fixnum(1);
    } else if (widetag == SYMBOL_WIDETAG) {
        struct symbol*s = (void*)object;
        FIX(s->value);
        FIX(s->info); // how could this ever be readonly ???
        // name was already reassigned when moving dynamic to R/O, but when moving
        // R/O back to dynamic it was not done yet.
        lispobj name = decode_symbol_name(s->name);
        gc_assert(is_lisp_pointer(name));
        set_symbol_name(s, func(name, arg));
    } else if (widetag == FDEFN_WIDETAG) {
        // nothing to do
    } else if (widetag == CODE_HEADER_WIDETAG) {
        int boxedlen = code_header_words((struct code*)object), i;
        // first 4 slots are header, boxedlen, fixups, debuginfo
        for (i=2; i<boxedlen; ++i) FIX(object[i]);
    } else {
        sword_t len = object_size(object), i;
        for (i=1; i<len; ++i) FIX(object[i]);
    }
#undef FIX
}

static int readonly_unboxed_obj_p(lispobj* obj)
{
    if (forwarding_pointer_p(obj) || is_cons_half(*obj)) return 0;
    int widetag = widetag_of(obj);
    switch (widetag) {
#ifndef LISP_FEATURE_64_BIT
    case SINGLE_FLOAT_WIDETAG:
#endif
    case BIGNUM_WIDETAG: case DOUBLE_FLOAT_WIDETAG:
    case COMPLEX_SINGLE_FLOAT_WIDETAG: case COMPLEX_DOUBLE_FLOAT_WIDETAG:
    case SAP_WIDETAG: case SIMPLE_ARRAY_NIL_WIDETAG:
#ifdef SIMD_PACK_WIDETAG
    case SIMD_PACK_WIDETAG:
#endif
#ifdef SIMD_PACK_256_WIDETAG
    case SIMD_PACK_256_WIDETAG:
#endif
        return 1;
    case RATIO_WIDETAG: case COMPLEX_RATIONAL_WIDETAG:
        return fixnump(obj[1]) && fixnump(obj[2]);
    }
    if ((widetag > SIMPLE_VECTOR_WIDETAG && widetag < COMPLEX_BASE_STRING_WIDETAG)) {
        if (!vector_len((struct vector*)obj)) return 1; // length 0 vectors can't be stored into
        // any vector marked shareable is a winner
        if (*obj & (VECTOR_SHAREABLE|VECTOR_SHAREABLE_NONSTD)<<ARRAY_FLAGS_POSITION) return 1;
    }
    if (widetag == SIMPLE_VECTOR_WIDETAG) {
        struct vector*v = (void*)obj;
        sword_t length = vector_len(v);
        if (!length) return 1; // length 0 vectors can't be stored into
        if (*obj & (VECTOR_SHAREABLE|VECTOR_SHAREABLE_NONSTD)<<ARRAY_FLAGS_POSITION) {
            // If every element is non-pointer, then it can go in readonly space
            sword_t i;
            for (i=0; i<length; ++i)
                if (v->data[i] != NIL && is_lisp_pointer(v->data[i])) return 0;
            return 1;
        }
    }
    return 0;
}

// CAUTION: don't think you can use this generally. It can only compute sizes
// of objects that do not change when forwarded. Instances can grow when forwarded
// by gencgc due to adding a stable hash slot.
static sword_t careful_object_size(lispobj* obj) {
    return object_size(forwarding_pointer_p(obj)
                       ? native_pointer(forwarding_pointer_value(obj))
                       : obj);
}

static void walk_range(lispobj* start, lispobj* end, void (*func)(lispobj*,uword_t), uword_t arg)
{
    lispobj* where = next_object(start, 0, end);
    while (where) {
        func(where, arg);
        where = next_object(where, careful_object_size(where), end);
    }
}

struct pair {
    void (*func)(lispobj*,uword_t);
    uword_t data;
};

static uword_t walk_range_wrapper(lispobj* where, lispobj* limit, uword_t arg)
{
    struct pair* pair = (void*)arg;
    walk_range(where, limit, pair->func, pair->data);
    return 0;
}

/* Call 'fun' on every object in every space, passing it object and 'arg' */
static void walk_all_gc_spaces(void (*fun)(lispobj*,uword_t), uword_t arg)
{
    walk_range((lispobj*)NIL_SYMBOL_SLOTS_START, (lispobj*)NIL_SYMBOL_SLOTS_END, fun, arg);
    walk_range((lispobj*)STATIC_SPACE_OBJECTS_START, static_space_free_pointer, fun, arg);
    if (PERMGEN_SPACE_START)
        walk_range((lispobj*)PERMGEN_SPACE_START, permgen_space_free_pointer, fun, arg);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    walk_range((lispobj*)FIXEDOBJ_SPACE_START, fixedobj_free_pointer, fun, arg);
#endif
    if (TEXT_SPACE_START)
        walk_range((lispobj*)TEXT_SPACE_START, text_space_highwatermark, fun, arg);
    struct pair pair = {fun, arg};
    walk_generation(walk_range_wrapper, -1, (uword_t)&pair);
}

static lispobj readonlyize(lispobj* obj)
{
    sword_t nwords = object_size(obj);
    lispobj* copy = read_only_space_free_pointer;
    lispobj* new_freeptr = copy + nwords;
    if ((uword_t)new_freeptr > READ_ONLY_SPACE_END)
        lose("overran R/O space? ptr=%p end=%p", read_only_space_free_pointer,
             (void*)READ_ONLY_SPACE_END);
    read_only_space_free_pointer = new_freeptr;
    memcpy(copy, obj, nwords << WORD_SHIFT);
    lispobj new = make_lispobj(copy, OTHER_POINTER_LOWTAG);
    set_forwarding_pointer(obj, new);
    // gc_assert(careful_object_size(obj) == nwords);
    return new;
}

// We have to take the unused arg for function signature compatibility.
// Lambdas in C are so crappy.
static void ensure_symbol_name_forwarded(lispobj* obj,
                                         __attribute__((unused)) uword_t arg) {
    if (widetag_of(obj) == SYMBOL_WIDETAG) {
        struct symbol* s = (void*)obj;
        lispobj* name = native_pointer(decode_symbol_name(s->name));
        bool fp = forwarding_pointer_p(name);
        if (fp || (*name & ((VECTOR_SHAREABLE|VECTOR_SHAREABLE_NONSTD)<<ARRAY_FLAGS_POSITION)))
            set_symbol_name(s, fp ? forwarding_pointer_value(name) : readonlyize(name));
    }
}

static lispobj follow_ro_fp(lispobj ptr, __attribute__((unused)) uword_t arg) {
    return follow_fp(ptr); // just ignore arg
}
static void follow_rospace_ptrs(lispobj* obj, __attribute__((unused)) uword_t arg) {
    if (!forwarding_pointer_p(obj)) visit_pointer_words(obj, follow_ro_fp, 0);
}
static void insert_filler(lispobj* obj, __attribute__((unused)) uword_t arg) {
    if (forwarding_pointer_p(obj)) {
        sword_t nwords = object_size(native_pointer(forwarding_pointer_value(obj)));
        // TODO: huge fillers aren't really used elsewhere. Can this exceed the
        // bits allotted for the size field? Insert more than one filler if needed.
        *obj = make_filler_header(nwords);
    }
}

void prepare_readonly_space(int purify, int print)
{
    gc_assert((uword_t)read_only_space_free_pointer == READ_ONLY_SPACE_START);
    if (!purify) {
        // I guess some of the architectures need a readonly page now because reasons,
        // even though we're somewhat more careful in coreparse to avoid reading memory
        // at READ_ONLY_SPACE_START if the free_pointer isn't higher than the start.
        int string_space_size = BACKEND_PAGE_BYTES;
        READ_ONLY_SPACE_START =
            (uword_t)os_alloc_gc_space(READ_ONLY_CORE_SPACE_ID,
                                       MOVABLE, (char*)DYNAMIC_SPACE_START - string_space_size,
                                       string_space_size);
        READ_ONLY_SPACE_END = READ_ONLY_SPACE_START + string_space_size;
        read_only_space_free_pointer = (lispobj*)READ_ONLY_SPACE_START;
        return;
    }
    int sum_sizes = 2;
    // 1. Sum of the sizes of immutable objects, which can only be in dynamic space
    if (print) fprintf(stderr, "purify: calculating size ... ");
    page_index_t first, last;
    for ( first = 0; first < next_free_page; first = 1+last ) {
        last = contiguous_block_final_page(first);
        lispobj* where = (lispobj*)page_address(first);
        lispobj* limit = (lispobj*)page_limit(last);
        where = next_object(where, 0, limit);
        sword_t nwords;
        while (where) {
            nwords = object_size(where);
            if ( readonly_unboxed_obj_p(where) ) sum_sizes += nwords;
            where = next_object(where, nwords, limit);
        }
    }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    extern int compute_codeblob_offsets_nwords(int*);
    sum_sizes += compute_codeblob_offsets_nwords(NULL);
#endif
    sum_sizes += 2; // for the SIMPLE_VECTOR inserted below as a delimiter
    sum_sizes <<= WORD_SHIFT;
    if (print) fprintf(stderr, "%d bytes\n", sum_sizes);
    int string_space_size = ALIGN_UP(sum_sizes, BACKEND_PAGE_BYTES);
    // Try to place readonly just below dynamic space, but it doesn't really matter where
    READ_ONLY_SPACE_START =
        (uword_t)os_alloc_gc_space(READ_ONLY_CORE_SPACE_ID,
                                   MOVABLE, (char*)DYNAMIC_SPACE_START - string_space_size,
                                   string_space_size);
    READ_ONLY_SPACE_END = READ_ONLY_SPACE_START + sum_sizes;
    read_only_space_free_pointer = (lispobj*)READ_ONLY_SPACE_START;

    // 2. Forward all symbol names so that they're placed contiguously
    // Could be even more clever and sort lexicographically for determistic core
    if (print) fprintf(stderr, "purify: forwarding symbol names\n");
    walk_all_gc_spaces(ensure_symbol_name_forwarded, 0);

    // Add a random delimiter object between symbol-names and everything else.
    // APROPOS-LIST uses this to detect the end of the strings.
    *read_only_space_free_pointer = SIMPLE_VECTOR_WIDETAG; // length 0
    read_only_space_free_pointer += 2;

    // 3. Forward everything else
    if (print) fprintf(stderr, "purify: forwarding other data\n");
    for ( first = 0; first < next_free_page; first = 1+last ) {
        last = contiguous_block_final_page(first);
        lispobj* where = (lispobj*)page_address(first);
        lispobj* limit = page_limit(last);
        for ( where = next_object(where, 0, limit) ; where ;
              where = next_object(where, careful_object_size(where), limit) )
            if (readonly_unboxed_obj_p(where)) readonlyize(where);
    }

    // 4. Update all objects in all spaces to point to r/o copy of anything that moved
    if (print) fprintf(stderr, "purify: fixing all pointers\n");
    walk_all_gc_spaces(follow_rospace_ptrs, 0);
    walk_all_gc_spaces(insert_filler, 0);
}

/* Now for the opposite of 'purify' - moving back from R/O to dynamic */

static lispobj follow_shadow_fp(lispobj ptr, uword_t arg) {
    if (!readonly_space_p(ptr)) return ptr;
    lispobj* shadow_space = (lispobj*)arg;
    lispobj* base = native_pointer(ptr);
    int displacement = base - (lispobj*)READ_ONLY_SPACE_START;
    return shadow_space[displacement];
}

static void undo_rospace_ptrs(lispobj* obj, uword_t arg) {
    visit_pointer_words(obj, follow_shadow_fp, arg);
}

void move_rospace_to_dynamic(__attribute__((unused)) int print)
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    extern void deport_codeblob_offsets_from_heap();
    deport_codeblob_offsets_from_heap();
#endif
    int space_usage = (uword_t)read_only_space_free_pointer - READ_ONLY_SPACE_START;
    if (!space_usage) return;
    lispobj* shadow_base = calloc(space_usage, 1);
    gc_assert(shadow_base);
    lispobj* shadow_cursor = shadow_base;
    // Forward everything in R/O to dynamic space. Record FPs outside of the objects
    // since the space is not writable and we're about to unmap.
    lispobj *where = (lispobj*)READ_ONLY_SPACE_START;
    sword_t nwords;
    for ( ; where < read_only_space_free_pointer ; where += nwords, shadow_cursor += nwords ) {
        nwords = headerobj_size(where);
        lispobj *new = gc_general_alloc(unboxed_region, nwords*N_WORD_BYTES, PAGE_TYPE_BOXED);
        SET_ALLOCATED_BIT(new);
        memcpy(new, where, nwords*N_WORD_BYTES);
        *shadow_cursor = make_lispobj(new, OTHER_POINTER_LOWTAG);
    }
    ensure_region_closed(unboxed_region, PAGE_TYPE_BOXED);
    os_deallocate((void*)READ_ONLY_SPACE_START, READ_ONLY_SPACE_END - READ_ONLY_SPACE_START);
    walk_all_gc_spaces(undo_rospace_ptrs, (uword_t)shadow_base);
    // Set it empty
    read_only_space_free_pointer = (lispobj*)READ_ONLY_SPACE_START;
}

/* This kludge is only for the hide-packages test after it invokes move_rospace_to_dynamic().
 * I wish there were a better way to write that test than to hack up the GC
 * so badly that it can't run with its heap verifications enabled. */
void test_dirty_all_gc_cards()
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    int n_text_pages = text_space_size / IMMOBILE_CARD_BYTES;
    int n_bitmap_elts = ALIGN_UP(n_text_pages, 32) / 32;
    memset(text_page_touched_bits, 0xFF, sizeof (int)*n_bitmap_elts);
    lispobj* where = (lispobj*)TEXT_SPACE_START;
    // OBJ_WRITTEN_FLAG is confusing. The '<<24' puts it in the generation byte.
    for ( ; where < text_space_highwatermark ; where += object_size(where) )
        if (widetag_of(where) == CODE_HEADER_WIDETAG) *where |= (OBJ_WRITTEN_FLAG << 24);
#endif
#ifdef LISP_FEATURE_SOFT_CARD_MARKS // just touch all mark bits, no harm done
    memset(gc_card_mark, 0, 1<<gc_card_table_nbits);
#endif
    page_index_t first = 0;
    while (first < next_free_page) {
        page_index_t last = contiguous_block_final_page(first);
        lispobj* where = (lispobj*)page_address(first);
        lispobj* limit = page_limit(last);
        for (where = next_object(where, 0, limit) ; where ;
             where = next_object(where, object_size(where), limit) )
            if (widetag_of(where) == CODE_HEADER_WIDETAG) {
#ifndef LISP_FEATURE_SOFT_CARD_MARKS // only touch code pages, others got WP faults
                gc_card_mark[addr_to_card_index(where)] = 1;
#endif
                *where |= (OBJ_WRITTEN_FLAG << 24);
            }
        first = 1+last;
    }
    pre_verify_gen_0 = 1;
}

#endif
