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
#include "gc-private.h"
#include "genesis/primitive-objects.h"
#include "genesis/hash-table.h"
#include "genesis/package.h"
#include "genesis/split-ordered-list.h"
#include "genesis/brothertree.h"
#include "forwarding-ptr.h"

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
    if (start == DYNAMIC_SPACE_START && end == dynamic_space_highwatermark()) {
        struct symbol_search ss = {name, ignore_case};
        return (lispobj*)walk_generation(search_symbol_aux, -1, (uword_t)&ss);
    }
#endif
    while (where < limit) {
        struct vector *string;
        if (widetag_of(where) == SYMBOL_WIDETAG &&
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
        where += object_size(where);
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
            where += object_size2(where, header);
        }
        if (where >= (lispobj*)DYNAMIC_SPACE_START)
            break;
        where = (lispobj*)DYNAMIC_SPACE_START;
        end = (lispobj*)dynamic_space_highwatermark();
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
        struct instance* table = (struct instance*)
          native_pointer(table_selector ? pkg->external_symbols : pkg->internal_symbols);
        gc_assert(widetag_of(&table->header) == INSTANCE_WIDETAG);
        gc_assert(listp(table->slots[INSTANCE_DATA_START])); // KLUDGE
        struct cons* cells = (void*)native_pointer(table->slots[INSTANCE_DATA_START]);
        gc_assert(simple_vector_p(cells->cdr));
        struct vector* v = VECTOR(cells->cdr);
        lispobj namelen = strlen(symbol_name);
        int vector_length = vector_len(v);
        int index = *hint >> 1;
        if (index >= vector_length)
            index = 0; // safeguard against vector shrinkage
        int initial_index = index;
        do {
            lispobj thing = v->data[index];
            if (lowtag_of(thing) == OTHER_POINTER_LOWTAG
                && widetag_of(&SYMBOL(thing)->header) == SYMBOL_WIDETAG
                && sym_stringeq(thing, symbol_name, namelen)) {
                *hint = (index << 1) | table_selector;
                return (lispobj*)SYMBOL(thing);
            }
            index = (index + 1) % vector_length;
        } while (index != initial_index);
        table_selector = table_selector ^ 1;
    }
    return 0;
}

lispobj* find_symbol(char* symbol_name, lispobj package, unsigned int* hint)
{
    return package ? search_package_symbols(package, symbol_name, hint) : 0;
}

static inline boolean fringe_node_p(struct binary_node* node)
{
    int len = ((unsigned int)node->header >> INSTANCE_LENGTH_SHIFT) & INSTANCE_LENGTH_MASK;
    return len <= (int)(1+INSTANCE_DATA_START);
}

/* I anticipate using the brothertree search algorithms to find code
 * while GC has already potentially moved some of the tree nodes,
 * thus the use of follow_fp() before dereferencing a node pointer */
uword_t brothertree_find_eql(uword_t key, lispobj tree)
{
    while (tree != NIL) {
        tree = follow_fp(tree);
        lispobj layout = follow_fp(instance_layout(INSTANCE(tree)));
        if (layout_depth2_id(LAYOUT(layout)) == BROTHERTREE_UNARY_NODE_LAYOUT_ID) {
            tree = ((struct unary_node*)INSTANCE(tree))->child;
        } else {
            struct binary_node* node = (void*)INSTANCE(tree);
            if (node->uw_key == key) return tree;
            lispobj l = NIL, r = NIL;
            // unless a fringe node, read the left and right pointers
            if (!fringe_node_p(node)) l = node->_left, r = node->_right;
            if (key < node->uw_key) tree = l; else tree = r;
        }
    }
    return 0;
}

uword_t brothertree_find_lesseql(uword_t key, lispobj tree)
{
    lispobj best = NIL;
    while (tree != NIL) {
        tree = follow_fp(tree);
        lispobj layout = follow_fp(instance_layout(INSTANCE(tree)));
        if (layout_depth2_id(LAYOUT(layout)) == BROTHERTREE_UNARY_NODE_LAYOUT_ID) {
            tree = ((struct unary_node*)INSTANCE(tree))->child;
        } else {
            struct binary_node* node = (void*)INSTANCE(tree);
            if (node->uw_key == key) return tree;
            lispobj l = NIL, r = NIL;
            // unless a fringe node, read the left and right pointers
            if (!fringe_node_p(node)) l = node->_left, r = node->_right;
            if (key < node->uw_key) tree = l; else { best = tree; tree = r; }
        }
    }
    return best;
}

uword_t brothertree_find_greatereql(uword_t key, lispobj tree)
{
    lispobj best = NIL;
    while (tree != NIL) {
        tree = follow_fp(tree);
        lispobj layout = follow_fp(instance_layout(INSTANCE(tree)));
        if (layout_depth2_id(LAYOUT(layout)) == BROTHERTREE_UNARY_NODE_LAYOUT_ID) {
            tree = ((struct unary_node*)INSTANCE(tree))->child;
        } else {
            struct binary_node* node = (void*)INSTANCE(tree);
            if (node->uw_key == key) return tree;
            lispobj l = NIL, r = NIL;
            // unless a fringe node, read the left and right pointers
            if (!fringe_node_p(node)) l = node->_left, r = node->_right;
            if (key > node->uw_key) tree = r; else { best = tree; tree = l; }
        }
    }
    return best;
}

#define BSEARCH_ALGORITHM_IMPL \
    int low = 0; \
    int high = nelements - 1; \
    while (low <= high) { \
        /* Many authors point out that this is a bug if overflow occurs \
         * and it can be avoided by using low+(high-low)/2 or similar. \
         * But we will never have so many code blobs that overflow occurs. */ \
        int mid = (low + high) / 2; \
        uword_t probe = array[mid]; \
        if (probe < item) low = mid + 1; \
        else if (probe > item) high = mid - 1; \
        else return mid; \
    } \

/* Binary search a sorted vector (of code base addresses).
 * This might be useful for generations other than 0,
 * because we only need to rebuild the vector in GC, which is
 * easily done; and it's much denser than a tree */
int bsearch_lesseql_uword(uword_t item, uword_t* array, int nelements)
{
    BSEARCH_ALGORITHM_IMPL
    if (high >= 0) return high;
    return -1;
}

int bsearch_greatereql_uword(uword_t item, uword_t* array, int nelements)
{
    BSEARCH_ALGORITHM_IMPL
    if (low < nelements) return low;
    return -1;
}

#ifdef LISP_FEATURE_64_BIT
/// As above, but using space-relative pointers which halve the storage requirement
int bsearch_lesseql_uint32(uint32_t item, uint32_t* array, int nelements)
{
    BSEARCH_ALGORITHM_IMPL
    if (high >= 0) return high;
    return -1;
}

int bsearch_greatereql_uint32(uint32_t item, uint32_t* array, int nelements)
{
    BSEARCH_ALGORITHM_IMPL
    if (low < nelements) return low;
    return -1;
}
#endif

/* Find in an address-based split-ordered list
 * Unlike the lisp algorithm, this does not "assist" a pending deletion
 * by completing it with compare-and-swap - this loop simply ignores
 * any deleted nodes that haven't been snipped out yet. */
struct split_ordered_list_node*
split_ordered_list_find(struct split_ordered_list* solist,
                        lispobj key)
{
    struct cons* bins_and_shift = CONS(solist->bins);
    struct vector* bins = VECTOR(bins_and_shift->car);
    int shift = fixnum_value(bins_and_shift->cdr);
    // see MULTIPLICATIVE-HASH in src/code/solist.lisp
#ifdef LISP_FEATURE_64_BIT
    lispobj prod = 11400714819323198485UL * key;
#else
    lispobj prod = 2654435769U * key;
#endif
    lispobj full_hash = (prod >> (1+N_FIXNUM_TAG_BITS)) | 1;
    int bin_index = full_hash >> shift;
    lispobj nodeptr = bins->data[bin_index];
    while ((nodeptr & WIDETAG_MASK) == UNBOUND_MARKER_WIDETAG) {
        nodeptr = bins->data[--bin_index];
    }
    struct split_ordered_list_node* node = (void*)native_pointer(nodeptr);
    lispobj hash_as_fixnum = make_fixnum(full_hash);
    while (1) {
        if (node->node_hash == hash_as_fixnum) { // possible hit
            if (node->so_key == key && // looking good
                lowtag_of(node->_node_next) != 0) { // node is not deleted, great
                return node;
            }
        }
        if (node->node_hash > hash_as_fixnum ||
            node->_node_next == LFLIST_TAIL_ATOM) return NULL;
        node = (void*)native_pointer(node->_node_next);
    }
}
