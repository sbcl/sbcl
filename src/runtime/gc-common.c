/*
 * Garbage Collection common functions for scavenging, moving and sizing
 * objects.  These are for use with both GC (stop & copy GC) and GENCGC
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

/*
 * For a review of garbage collection techniques (e.g. generational
 * GC) and terminology (e.g. "scavenging") see Paul R. Wilson,
 * "Uniprocessor Garbage Collection Techniques". As of 20000618, this
 * had been accepted for _ACM Computing Surveys_ and was available
 * as a PostScript preprint through
 *   <http://www.cs.utexas.edu/users/oops/papers.html>
 * as
 *   <ftp://ftp.cs.utexas.edu/pub/garbage/bigsurv.ps>.
 */

#include <stdio.h>
#include <signal.h>
#include <string.h>
#include "sbcl.h"
#include "runtime.h"
#include "os.h"
#include "interr.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"
#include "arch.h"
#include "gc.h"
#include "hopscotch.h"
#include "code.h"
#include "genesis/primitive-objects.h"
#include "genesis/static-symbols.h"
#include "genesis/layout.h"
#include "genesis/hash-table.h"
#define WANT_SCAV_TRANS_SIZE_TABLES
#include "gc-internal.h"
#include "gc-private.h"
#include "forwarding-ptr.h"
#include "var-io.h"
#include "search.h"
#include "murmur_hash.h"

#ifdef LISP_FEATURE_SPARC
#define LONG_FLOAT_SIZE 4
#elif defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
#define LONG_FLOAT_SIZE 3
#endif

os_vm_size_t dynamic_space_size = DEFAULT_DYNAMIC_SPACE_SIZE;
os_vm_size_t thread_control_stack_size = DEFAULT_CONTROL_STACK_SIZE;

sword_t (*const scavtab[256])(lispobj *where, lispobj object);
uword_t gc_copied_nwords;

/* If sb_sprof_enabled was used and the data are not in the final form
 * (in the *SAMPLES* instance) then all code remains live.
 * This is a weaker constraint than 'pin_all_dynamic_space_code'
 * because the latter implies that all code is not potential garbage and not
 * movable, whereas this only implies not potential garbage */
int sb_sprof_enabled;

// "Transport" functions are responsible for deciding where to copy an object
// and how many bytes to copy (usually the sizing function is inlined into the
// trans function, so we don't call a sizetab[] entry), allocating memory,
// and calling memcpy. These functions generally do not deposit forwarding
// pointers into the original object - that would be up to the scav_x_pointer
// function - however, there are some exceptions:
//  - trans_list() is responsible for leaving FPs for all cons cells that
//    it copied while laying out a chain of consecutive conses.
//  - trans_code() is responsible for leaving FPs for both the code object
//    AND all embedded functions.
static lispobj (*transother[64])(lispobj object);
sword_t (*sizetab[256])(lispobj *where);
struct weak_pointer *weak_pointer_chain = WEAK_POINTER_CHAIN_END;
struct cons *weak_vectors;

os_vm_size_t bytes_consed_between_gcs = 12*1024*1024;

#ifdef LISP_FEATURE_PPC64
// unevenly spaced pointer lowtags
static void (*scav_ptr[16])(lispobj *where, lispobj object); /* forward decl */
#define PTR_SCAVTAB_INDEX(ptr) (ptr & 15)
#else
// evenly spaced pointer lowtags
static void (*scav_ptr[4])(lispobj *where, lispobj object); /* forward decl */
#define PTR_SCAVTAB_INDEX(ptr) ((uint32_t)ptr>>(N_LOWTAG_BITS-2))&3
#endif

/* Fixup the pointer in 'object' which is stored at *addr.
 * That is, rewrite *addr if (and only if) 'object' got moved.
 *
 * As a precondition of calling this, 'object' must satisfy is_lisp_pointer().
 *
 * For GENCGC only:
 * - With 32-bit words, is_lisp_pointer(object) returns true if addr
 *   contains a broken heart marker (0x01), so we need a sanity check
 *   as the last case.
 * - With 64-bit words, is_lisp_pointer(object) is false when addr
 *   contains a broken heart marker (0x01), so this won't get called.
 */
static inline void scav1(lispobj* addr, lispobj object)
{
#ifdef LISP_FEATURE_CHENEYGC
    if (from_space_p(object)) {
        if (forwarding_pointer_p(native_pointer(object)))
            *addr = forwarding_pointer_value(native_pointer(object));
        else
            scav_ptr[PTR_SCAVTAB_INDEX(object)](addr, object);
    }
#else
    page_index_t page;
    /* In theory we can test forwarding_pointer_p on anything, but it's probably
     * better to avoid reading more memory than needed. Hence the pre-check
     * for a from_space object. But, rather than call from_space_p() which always
     * checks for object pinning, it's a performance boost to treat from_space_p()
     * as a leaky abstraction - reading one word at *native_pointer(object) is easier
     * than looking in a hashset, and 9 times out of 10 times we need to read it anyway.
     * And if the object was already forwarded, we never need pinned_p. */
    if ((page = find_page_index((void*)object)) >= 0) {
        if (page_table[page].gen == from_space) {
            if (forwarding_pointer_p(native_pointer(object)))
                *addr = forwarding_pointer_value(native_pointer(object));
            else if (!pinned_p(object, page))
                scav_ptr[PTR_SCAVTAB_INDEX(object)](addr, object);
        }
    }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    // Test immobile_space_p() only if object was definitely not in dynamic space
    else if (immobile_space_p(object)) {
        lispobj *ptr = base_pointer(object);
        if (immobile_obj_gen_bits(ptr) == from_space)
            enliven_immobile_obj(ptr, 1);
    }
#endif
#if (N_WORD_BITS == 32) && defined(LISP_FEATURE_GENCGC)
    else if (object == 1)
          lose("unexpected forwarding pointer in scavenge @ %p", addr);
#endif
#endif
}

inline void gc_scav_pair(lispobj where[2])
{
    lispobj object = where[0];
    if (is_lisp_pointer(object))
        scav1(where, object);
    object = where[1];
    if (is_lisp_pointer(object))
        scav1(where+1, object);
}

// Scavenge a block of memory from 'start' to 'end'
// that may contain object headers.
void heap_scavenge(lispobj *start, lispobj *end)
{
    lispobj *object_ptr;

    for (object_ptr = start; object_ptr < end;) {
        lispobj object = *object_ptr;
        if (other_immediate_lowtag_p(object))
            /* It's some sort of header object or another. */
            object_ptr += (scavtab[header_widetag(object)])(object_ptr, object);
        else {  // it's a cons
            gc_scav_pair(object_ptr);
            object_ptr += 2;
        }
    }
    // This assertion is usually the one that fails when something
    // is subtly wrong with the heap, so definitely always do it.
    if (object_ptr != end)
        lose("heap_scavenge failure: Final object pointer %p, start %p, end %p",
             object_ptr, start, end);
}

// Scavenge a block of memory from 'start' extending for 'n_words'
// that must not contain any object headers.
sword_t scavenge(lispobj *start, sword_t n_words)
{
    gc_dcheck(compacting_p());
    lispobj *end = start + n_words;
    lispobj *object_ptr;
    for (object_ptr = start; object_ptr < end; object_ptr++) {
        lispobj object = *object_ptr;
        if (is_lisp_pointer(object)) scav1(object_ptr, object);
    }
    return n_words;
}

/* Fix pointers in a range of memory from 'start' to 'end' where no raw words
 * are allowed. Object headers and immediates are ignored, except that:
 *   - compact instance headers fix the layout
 *   - filler_widetag causes skipping of its payload.
 * Recompute 'dirty' and return the new value as the logical OR of its initial
 * value (determined by whether the card is sticky-marked) and whether
 * any old->young pointer is seen.
 *
 * In case of card-spanning objects, the 'start' and 'end' parameters might not
 * exactly delimit objects boundaries. */
#ifdef LISP_FEATURE_GENCGC
int descriptors_scavenge(lispobj *start, lispobj* end,
                         generation_index_t gen, int dirty)
{
    lispobj *where;
    for (where = start; where < end; where++) {
        lispobj ptr = *where;
        int pointee_gen = 8;
        // Nothing in the root set is forwardable. When I forgot to handle fillers,
        // this assertion failed often, because we'd try to process old garbage.
        gc_dcheck(ptr != 1);
        if (is_lisp_pointer(ptr)) {
            // A mostly copy-and-paste from scav1()
            page_index_t page = find_page_index((void*)ptr);
            if (page >= 0) { // Avoid falling into immobile_space_p if 'ptr' is to dynamic space
                pointee_gen = page_table[page].gen;
                if (pointee_gen == from_space) {
                    pointee_gen = new_space;
                    if (forwarding_pointer_p(native_pointer(ptr))) {
                        *where = forwarding_pointer_value(native_pointer(ptr));
                    } else if (!pinned_p(ptr, page)) {
                        scav_ptr[PTR_SCAVTAB_INDEX(ptr)](where, ptr);
                    }
                }
            }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
            // do this only if object is definitely not in dynamic space.
            else if (immobile_space_p(ptr)) {
              immobile_obj: ;
                lispobj *base = base_pointer(ptr);
                int genbits = immobile_obj_gen_bits(base);
                // The VISITED bit is masked out. Don't re-enliven visited.
                // (VISITED = black in the traditional tri-color marking scheme)
                pointee_gen = genbits & 0xf;
                if (pointee_gen == from_space) pointee_gen = new_space;
                if (genbits == from_space) enliven_immobile_obj(base, 1);
            }
#endif
        }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        else if (instanceoid_widetag_p(ptr & WIDETAG_MASK) && ((ptr >>= 32) != 0))
            goto immobile_obj;
#endif
        else {
            // Advancing by the filler payload count causes 'where' to be exactly
            // at the next object after the loop increments it as usual.
            if (header_widetag(ptr) == FILLER_WIDETAG) where += ptr >> N_WIDETAG_BITS;
            continue;
        }
        // Dear lord, I hate the numbering scheme with SCRATCH_GENERATION higher than everything
        if (pointee_gen < gen || pointee_gen == (1+PSEUDO_STATIC_GENERATION)) dirty = 1;
    }
    return dirty;
}
#endif

/* If 'fun' is provided, then call it on each livened object,
 * otherwise use scav1() */
void scav_binding_stack(lispobj* where, lispobj* end, void (*fun)(lispobj))
{
    /* The binding stack consists of pairs of words, each holding a value and
     * either a TLS index (if threads), or symbol (if no threads).
     * Here we scavenge only the entries' values.
     *
     * Were the TLS index scavenged, it can never cause a symbol to move,
     * let alone be considered live. So we are bug-for-bug compatible regardless
     * of +/- sb-thread if a symbol would otherwise be garbage at this point.
     * As a practical matter, it is technically impossible for a symbol's only
     * reason for livenesss to be the binding stack. Nonetheless it is best to
     * enforce behavioral consistency whether or not the platform has threads.
     *
     * Bindings of compile-time known symbols is fairly easy to reason about.
     * Code headers point to symbols, therefore code objects retains symbols.
     * The edge case of a PROGV binding of a freshly made symbol (via MAKE-SYMBOL)
     * is interesting. Indeed we preserve the symbol because PROGV places a reference
     * on the control stack, thereby either pinning or scavenging as the case may be.
     * If it did not, we would need a map from TLS index to symbol, updated if
     * a symbol moves, allowing death of a symbol only when no binding entry
     * mentions that index. Such complication is unnecessary at present.
     */
    struct binding* binding = (struct binding*)where;
    if (fun) { // call the specified function
        for ( ; (lispobj*)binding < end; ++binding )
            if (is_lisp_pointer(binding->value))
                fun(binding->value);

    } else {   // call scav1
        for ( ; (lispobj*)binding < end; ++binding )
            if (is_lisp_pointer(binding->value))
                scav1(&binding->value, binding->value);
    }
}
void scan_binding_stack()
{
#ifndef LISP_FEATURE_SB_THREAD
    struct thread* th;
    for_each_thread(th) { /* 'all' is exactly one */
        struct binding *binding = (struct binding*)th->binding_stack_start;
        lispobj *end = (lispobj*)get_binding_stack_pointer(th);
        for ( ; (lispobj*)binding < end; ++binding ) {
            if (is_lisp_pointer(binding->symbol) &&
                forwarding_pointer_p(native_pointer(binding->symbol)))
                binding->symbol =
                  forwarding_pointer_value(native_pointer(binding->symbol));
        }
    }
#endif
}

extern int pin_all_dynamic_space_code;
static struct code *
trans_code(struct code *code)
{
#ifdef LISP_FEATURE_GENCGC
    gc_dcheck(!pin_all_dynamic_space_code);
#endif
    /* if object has already been transported, just return pointer */
    if (forwarding_pointer_p((lispobj *)code)) {
        return (struct code *)native_pointer(forwarding_pointer_value((lispobj*)code));
    }

    gc_dcheck(widetag_of(&code->header) == CODE_HEADER_WIDETAG);

    /* prepare to transport the code vector */
    lispobj l_code = make_lispobj(code, OTHER_POINTER_LOWTAG);
    lispobj l_new_code = copy_possibly_large_object(l_code,
                                           code_total_nwords(code),
                                           code_region, PAGE_TYPE_CODE);

#ifdef LISP_FEATURE_GENCGC
    if (l_new_code == l_code)
        return code;
#endif

    set_forwarding_pointer((lispobj *)code, l_new_code);

    struct code *new_code = (struct code *) native_pointer(l_new_code);
    sword_t displacement = l_new_code - l_code;

#if defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64 || \
    defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64
    // Fixup absolute jump tables. These aren't recorded in code->fixups
    // because we don't need to denote an arbitrary set of places in the code.
    // The count alone suffices. A GC immediately after creating the code
    // could cause us to observe some 0 words here. Those should be ignored.
    lispobj* jump_table = code_jumptable_start(new_code);
    int count = jumptable_count(jump_table);
    int i;
    for (i = 1; i < count; ++i)
        if (jump_table[i]) jump_table[i] += displacement;
#endif
    for_each_simple_fun(i, new_fun, new_code, 1, {
        // Calculate the old raw function pointer
        struct simple_fun* old_fun = (struct simple_fun*)((char*)new_fun - displacement);
        if (fun_self_from_baseptr(old_fun) == old_fun->self) {
            new_fun->self = fun_self_from_baseptr(new_fun);
            set_forwarding_pointer((lispobj*)old_fun,
                                   make_lispobj(new_fun, FUN_POINTER_LOWTAG));
        }
    })
    gencgc_apply_code_fixups(code, new_code);
#ifdef LISP_FEATURE_GENCGC
    /* Cheneygc doesn't need this os_flush_icache, it flushes the whole
       spaces once when all copying is done. */
    os_flush_icache(code_text_start(new_code), code_text_size(new_code));
#endif
    return new_code;
}

static sword_t
scav_fun_pointer(lispobj *where, lispobj object)
{
    gc_dcheck(functionp(object));

    lispobj* fun = (void*)(object - FUN_POINTER_LOWTAG);
    lispobj copy;
    int widetag = widetag_of(fun);
    // object may be a simple-fun header, a funcallable instance, or closure
    if (widetag == SIMPLE_FUN_WIDETAG) {
        uword_t offset = (HeaderValue(*fun) & FUN_HEADER_NWORDS_MASK) * N_WORD_BYTES;
        /* Transport the whole code object */
        struct code *code = trans_code((struct code *) ((uword_t) fun - offset));
        copy = make_lispobj((char*)code + offset, FUN_POINTER_LOWTAG);
    } else {
        int page_type = PAGE_TYPE_MIXED;
        void* region = mixed_region;
        if (widetag == FUNCALLABLE_INSTANCE_WIDETAG) {
            /* funcallable-instance might have all descriptor slots
             * except for the trampoline, which points to an asm routine.
             * This is not true for self-contained trampoline GFs though. */
            struct layout* layout = (void*)native_pointer(funinstance_layout(FUNCTION(object)));
            if (layout && (layout->flags & STRICTLY_BOXED_FLAG)) // 'flags' is a raw slot
                page_type = PAGE_TYPE_BOXED, region = boxed_region;
        } else {
            /* Closures can always go on strictly boxed pages even though the
             * underlying function is (possibly) an untagged pointer.
             * When a closure is scavenged as a root, it can't need to fix the
             * value in closure->fun because that function has to be _older_ than
             * (or the same gen as) the closure. So if it's older, then it's not
             * in from_space. But what if it's the same? It's still not in
             * from_space, because the closure and its function have
             * generation >= (1+from_space) to be generational roots */
            page_type = PAGE_TYPE_BOXED, region = boxed_region;
        }
        copy = gc_copy_object(object, 1+SHORT_BOXED_NWORDS(*fun), region, page_type);
    }
    if (copy != object) { // large code won't undergo physical copy
        set_forwarding_pointer(fun, copy);
        *where = copy;
    }
    CHECK_COPY_POSTCONDITIONS(copy, FUN_POINTER_LOWTAG);
    return 1;
}

static lispobj
trans_code_header(lispobj object)
{
    struct code *ncode = trans_code((struct code *) native_pointer(object));
    return make_lispobj(ncode, OTHER_POINTER_LOWTAG);
}

static sword_t
size_code_header(lispobj *where)
{
    return code_total_nwords((struct code*)where);
}

#ifdef RETURN_PC_WIDETAG
static sword_t
scav_return_pc_header(lispobj *where, lispobj object)
{
    lose("attempted to scavenge a return PC header where=%p object=%"OBJ_FMTX,
         where, object);
    return 0; /* bogus return value to satisfy static type checking */
}

static lispobj
trans_return_pc_header(lispobj object)
{
    struct simple_fun *return_pc = (struct simple_fun *) native_pointer(object);
    uword_t offset = HeaderValue(return_pc->header) * N_WORD_BYTES;

    /* Transport the whole code object */
    struct code *code = trans_code((struct code *) ((uword_t) return_pc - offset));

    return make_lispobj((char*)code + offset, OTHER_POINTER_LOWTAG);
}
#endif /* RETURN_PC_WIDETAG */

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || defined(LISP_FEATURE_ARM64)
/* Closures hold a pointer to the raw simple-fun entry address instead of the
 * tagged object so that CALL [RAX+const] can be used to invoke it. */
static sword_t
scav_closure(lispobj *where, lispobj header)
{
    struct closure *closure = (struct closure *)where;
    if (closure->fun) {
        lispobj fun = fun_taggedptr_from_self(closure->fun);
        lispobj newfun = fun;
        scavenge(&newfun, 1);
        if (newfun != fun) // Don't write unnecessarily
            closure->fun = fun_self_from_taggedptr(newfun);
    }
    int payload_words = SHORT_BOXED_NWORDS(header);
    // Payload includes 'fun' which was just looked at, so subtract it.
    scavenge(1 + &closure->fun, payload_words - 1);
    return 1 + payload_words;
}
#endif

/*
 * instances
 */

int n_unboxed_instances;

/* If there are no raw words then we want to use a BOXED page if the instance
 * length is small. Or if there are only raw words and immediates, then use UNBOXED.
 * A bitmap of 0 implies that this instance could never be subject to CHANGE-CLASS,
 * so it will never gain tagged slots. If all slots are immediates,
 * then both constraints are satisfied; prefer UNBOXED.
 *
 * If the page is unboxed, the layout has to be pseudostatic.
 * As to the restriction on instance length: refer to the comment above
 * scan_contiguous_boxed_cards in gencgc.
 */
static inline lispobj copy_instance(lispobj object)
{
    // Object is an un-forwarded object in from_space
    lispobj header = *(lispobj*)(object - INSTANCE_POINTER_LOWTAG);
    int original_length = instance_length(header);

    void* region = mixed_region;
    int page_type = PAGE_TYPE_MIXED;

#ifdef LISP_FEATURE_GENCGC
    struct layout* layout = (void*)native_pointer(instance_layout(INSTANCE(object)));
    struct bitmap bitmap;
    if (layout) {
        generation_index_t layout_gen = 0;
# ifdef LISP_FEATURE_IMMOBILE_SPACE
        if (find_fixedobj_page_index(layout))
            layout_gen = immobile_obj_generation((lispobj*)layout);
# else
        page_index_t p = find_page_index(layout);
        if (p >= 0) layout_gen = page_table[p].gen;
# endif
        if (layout_gen == PSEUDO_STATIC_GENERATION
            && (bitmap = get_layout_bitmap(layout)).bits[0] == 0
            && bitmap.nwords == 1)
            page_type = PAGE_TYPE_UNBOXED, region = unboxed_region;
        else if (original_length < (int)GENCGC_PAGE_WORDS &&
                 (layout->flags & STRICTLY_BOXED_FLAG)) // 'flags' is a raw slot
            page_type = PAGE_TYPE_BOXED, region = boxed_region;
    }
#endif

    lispobj copy;
    // KLUDGE: reading both flags at once doesn't really work
    // unless either we know what the opaque values are:
    //  8 = stable-hash-required-flag
    //  9 = hash-slot-present-flag
    // Maybe the C compiler is sufficiently smart to turn a boolean expression
    // involving "&&" into this simple expression?
    if (((header >> 8) & 3) == 1) { // state = hashed + not moved
        /* If odd, add 1 (making even). Rounding that to odd and then
         * adding 1 for the header will effectively add 2 words.
         * Otherwise, don't add anything because a padding slot exists */
        int new_length = original_length + (original_length & 1);
        copy = gc_copy_object_resizing(object, 1 + (new_length|1),
                                       region, page_type,
                                       1 + (original_length|1));
        lispobj *base = native_pointer(copy);
        /* store the old address as the hash value */
#ifdef LISP_FEATURE_64_BIT
        uint64_t hash = murmur3_fmix64(object);
#else
        uint32_t hash = murmur3_fmix32(object);
#endif
        hash = (hash << (N_FIXNUM_TAG_BITS+1)) >> 1;
        base[original_length+1] = hash;
        /* If the object was enlarged by 2 words, then clear the final word.
         * Git rev 5f435a2b66 removed prezeroizing during GC, and that
         * last word was not the target of a memcpy */
        if (original_length & 1) base[original_length+2] = 0;
        *base |= 1 << flag_HashSlotPresent;
#if 0
        printf("stable hash: obj=%p L=%d -> %p L=%d (recalc=%d)\n",
               (void*)object, original_length, (void*)copy, new_length,
               instance_length(*base));
#endif
    } else {
        copy = gc_copy_object(object, 1 + (original_length|1), region, page_type);
    }
    set_forwarding_pointer(native_pointer(object), copy);
    return copy;
}

static sword_t
scav_instance_pointer(lispobj *where, lispobj object)
{
    gc_dcheck(instancep(object));
    lispobj copy = copy_instance(object);
    *where = copy;

    struct instance* node = INSTANCE(copy);
    lispobj layout = instance_layout((lispobj*)node);
    if (layout) {
        if (forwarding_pointer_p((lispobj*)LAYOUT(layout)))
            layout = forwarding_pointer_value((lispobj*)LAYOUT(layout));
        if (lockfree_list_node_layout_p(LAYOUT(layout))) {
            // Copy chain of lockfree list nodes to consecutive memory addresses,
            // just like trans_list does.  A logically deleted node will break the chain,
            // as its 'next' will not satisfy instancep(), but that's ok.
            while (instancep(object = node->slots[INSTANCE_DATA_START]) // node.next
                   && from_space_p(object)
                   && !forwarding_pointer_p(native_pointer(object))) {
                copy = copy_instance(object);
                node->slots[INSTANCE_DATA_START] = copy;
                node = INSTANCE(copy);
                // We don't have to stop upon seeing an instance with a different layout.
                // The only other object in the 'next' chain could be *TAIL-ATOM* if we reach
                // the end. It's possible that all of the tests in the 'while' loop are met
                // by *TAIL-ATOM* though probably not, because it is in pseudo-static space
                // which is never from_space. But even we iterate one more time, it's fine,
                // despite tail-atom's layout not having the custom GC bit set - it has the
                // 'next' pointer in the required slot. So even if we somehow copy it in
                // this loop (vs when scavenging the symbol referencing it), the iteration
                // after this will stop because forwarding_pointer_p will be true next time
                // due to the fact that *TAIL-ATOM* is its own 'next' (behaving like NIL).
            }
        }
    }

    return 1;
}


/*
 * lists and conses
 */

static inline lispobj
trans_list(lispobj object)
{
    /* Copy 'object'. */
    struct cons *copy = (struct cons *)
        gc_general_alloc(cons_region, sizeof(struct cons), PAGE_TYPE_CONS);
    NOTE_TRANSPORTING(object, copy, CONS_SIZE);
    lispobj new_list_pointer = make_lispobj(copy, LIST_POINTER_LOWTAG);
    copy->car = CONS(object)->car;
    /* Grab the cdr: set_forwarding_pointer will clobber it in GENCGC  */
    lispobj cdr = CONS(object)->cdr;
    set_forwarding_pointer((lispobj *)CONS(object), new_list_pointer);

    /* Try to linearize the list in the cdr direction to help reduce
     * paging. */
    while (listp(cdr) && from_space_p(cdr)) {
        lispobj* native_cdr = (lispobj*)CONS(cdr);
        if (forwarding_pointer_p(native_cdr)) {  // Might as well fix now.
            cdr = forwarding_pointer_value(native_cdr);
            break;
        }
        /* Copy 'cdr'. */
        struct cons *cdr_copy = (struct cons*)
            gc_general_alloc(cons_region, sizeof(struct cons), PAGE_TYPE_CONS);
        NOTE_TRANSPORTING(cdr, cdr_copy, CONS_SIZE);
        cdr_copy->car = ((struct cons*)native_cdr)->car;
        /* Grab the cdr before it is clobbered. */
        lispobj next = ((struct cons*)native_cdr)->cdr;
        /* Set cdr of the predecessor, and store an FP. */
        set_forwarding_pointer(native_cdr,
                               copy->cdr = make_lispobj(cdr_copy,
                                                        LIST_POINTER_LOWTAG));
        copy = cdr_copy;
        cdr = next;
    }
    copy->cdr = cdr;
    return new_list_pointer;
}

static sword_t
scav_list_pointer(lispobj *where, lispobj object)
{
    gc_dcheck(listp(object));

    lispobj copy = trans_list(object);
    gc_dcheck(copy != object);

    CHECK_COPY_POSTCONDITIONS(copy, LIST_POINTER_LOWTAG);

    *where = copy;
    return 1;
}

/*
 * scavenging and transporting other pointers
 */

static sword_t
scav_other_pointer(lispobj *where, lispobj object)
{
    gc_dcheck(lowtag_of(object) == OTHER_POINTER_LOWTAG);

    /* Object is a pointer into from space - not FP. */
    lispobj *first_pointer = (lispobj *)(object - OTHER_POINTER_LOWTAG);
    int tag = widetag_of(first_pointer);
    lispobj copy = transother[other_immediate_lowtag_p(tag)?tag>>2:0](object);

    // If the object was large, then instead of transporting it,
    // gencgc might simply promote the pages and return the same pointer.
    // That decision is made in copy_possibly_large_object().
    if (copy != object) {
        set_forwarding_pointer(first_pointer, copy);
        *where = copy;
    }
    CHECK_COPY_POSTCONDITIONS(copy, OTHER_POINTER_LOWTAG);
    return 1;
}

/*
 * immediate, boxed, and unboxed objects
 */

 /* The immediate object scavenger basically wants to be "scav_cons",
  * and so returns 2. To see why it's right, observe that scavenge() will
  * not invoke a scavtab entry on any object except for one satisfying
  * is_lisp_pointer(). So if a scavtab[] function got here,
  * then it must be via heap_scavenge(). But heap_scavenge() should only
  * dispatch via scavtab[] if it thought it saw an object header.
  * So why do we act like it saw a cons? Because conses can contain an
  * immediate object that satisfies both other_immediate_lowtag_p()
  * and is_lisp_immediate(), namely, the objects specifically mentioned at
  * is_cons_half(). So heap_scavenge() is nearly testing is_cons_half()
  * but even more efficiently, by ignoring the unusual immediate widetags
  * until we get to scav_immediate.
  *
  * And just to hammer the point home: we won't blow past the end of a specific
  * range of words when scavenging a binding or control stack or anything else,
  * because scavenge() skips immediate objects all by itself,
  * or rather it skips anything not satisfying is_lisp_pointer().
  *
  * As to the unbound marker, see rev. 09c78105eabc6bf2b339f421d4ed1df4678003db
  * which says that we might see it in conses for reasons somewhat unknown.
  */
static sword_t
scav_immediate(lispobj *where, lispobj object)
{
    object = *++where;
    if (is_lisp_pointer(object)) scav1(where, object);
    return 2;
}

static sword_t
size_immediate(lispobj __attribute__((unused)) *where)
{
    return 1;
}

//// General boxed object scav/trans/size functions

#define DEF_SCAV_BOXED(suffix, sizer) \
  static sword_t __attribute__((unused)) \
  scav_##suffix(lispobj *where, lispobj header) { \
      return 1 + scavenge(where+1, sizer(header)); \
  } \
  static sword_t size_##suffix(lispobj *where) { return 1 + sizer(*where); }

DEF_SCAV_BOXED(boxed, BOXED_NWORDS)
DEF_SCAV_BOXED(short_boxed, SHORT_BOXED_NWORDS)
DEF_SCAV_BOXED(tiny_boxed, TINY_BOXED_NWORDS)

static lispobj trans_boxed(lispobj object) {
    return gc_copy_object(object, 1 + BOXED_NWORDS(*native_pointer(object)),
                          boxed_region, PAGE_TYPE_BOXED);
}
static lispobj trans_tiny_mixed(lispobj object) {
    return gc_copy_object(object, 1 + TINY_BOXED_NWORDS(*native_pointer(object)),
                          mixed_region, PAGE_TYPE_MIXED);
}

static sword_t scav_symbol(lispobj *where, lispobj header) {
#ifdef LISP_FEATURE_COMPACT_SYMBOL
    struct symbol* s = (void*)where;
    scavenge(&s->value, 2); // picks up the value and info slots
    lispobj name = decode_symbol_name(s->name);
    lispobj new = name;
    scavenge(&new, 1);
    if (new != name) set_symbol_name(s, new);
    // The normal length indicated in the header would be (SYMBOL_SIZE-1) since
    // SYMBOL_SIZE counts the header as 1 word. If the indicated size is SYMBOL_SIZE,
    // then there's an extra slot.  (The extra slot provides quick access to
    // the special-operator handler function in the fast evaluator.)
    int indicated_nwords = (header>>N_WIDETAG_BITS) & 0xFF;
    // We've already processed the {hash, value, info, name}, so subtract 4 words.
    // In truth, the hash was ignored, though it might be a good place to store
    // some pointer data. 64 bits of hash is way more than enough.
    scavenge(&s->fdefn, indicated_nwords - 4);
    return 1 + (indicated_nwords|1); // round to odd, then add 1 for the header
#else
    return scav_tiny_boxed(where, header);
#endif
}

static inline int array_header_nwords(lispobj header) {
    unsigned char rank = (header >> ARRAY_RANK_POSITION);
    ++rank; // wraparound from 255 to 0
    int nwords = sizeof (struct array)/N_WORD_BYTES + (rank-1);
    return ALIGN_UP(nwords, 2);
}
static sword_t scav_array(lispobj *where, lispobj header) {
    struct array* a = (void*)where;
    scav1(&a->data, a->data);
    // displaced_p sounds like it would be T or NIL, but it is overloaded
    // to hold information for situations where array A is displaced to an
    // expressly adjustable array B which gets adjusted to become too small
    // to contain all the elements of A.
    scav1(&a->displaced_p, a->displaced_p);
    scav1(&a->displaced_from, a->displaced_from);
    return array_header_nwords(header);
}
static lispobj trans_array(lispobj object) {
    // VECTOR is a lie but I'm using it only to subtract the lowtag
    return gc_copy_object(object, array_header_nwords(VECTOR(object)->header),
                          boxed_region, PAGE_TYPE_BOXED);
}
static sword_t size_array(lispobj *where) { return array_header_nwords(*where); }

static sword_t
scav_instance(lispobj *where, lispobj header)
{
    int nslots = instance_length(header); // un-padded length
    int total_nwords = 1 + (nslots | 1);

    // First things first: fix or enliven the layout pointer as necessary,
    // writing it back if and only if it changed.
    lispobj layoutptr = instance_layout(where);
    if (!layoutptr) return total_nwords; // instance can't point to any data yet
    struct layout *layout = LAYOUT(layoutptr);
#ifdef LISP_FEATURE_METASPACE
    scav1(&layout->friend, layout->friend);
#else
    lispobj old = layoutptr;
    scav1(&layoutptr, layoutptr);
    if (layoutptr != old) instance_layout(where) = layoutptr;
#endif
    layout = LAYOUT(layoutptr); // in case it was adjusted in !METASPACE
    struct bitmap bitmap = get_layout_bitmap(layout);
    sword_t mask = bitmap.bits[0]; // there's always at least 1 bitmap word

    if (bitmap.nwords == 1) {
        if ((uword_t)mask == (uword_t)-1 << INSTANCE_DATA_START) {
            // Easy case: all slots are tagged words
            scavenge(where+1+INSTANCE_DATA_START, nslots-INSTANCE_DATA_START);
            return total_nwords;
        }
        if (mask == 0) return total_nwords; // trivial case: no tagged words
    }

    // Specially scavenge the 'next' slot of a lockfree list node. If the node is
    // pending deletion, 'next' will satisfy fixnump() but is in fact a pointer.
    // GC doesn't care too much about the deletion algorithm, but does have to
    // ensure liveness of the pointee, which may move unless pinned.
    if (lockfree_list_node_layout_p(layout)) {
        struct instance* node = (struct instance*)where;
        lispobj next = node->slots[INSTANCE_DATA_START];
        if (fixnump(next) && next) { // ignore initially 0 heap words
            lispobj descriptor = next | INSTANCE_POINTER_LOWTAG;
            scav1(&descriptor, descriptor);
            // Fix the pointer but of course leave it in mid-deletion (untagged) state.
            if (descriptor != (next | INSTANCE_POINTER_LOWTAG))
                node->slots[INSTANCE_DATA_START] = descriptor & ~LOWTAG_MASK;
        }
    }

    ++where; // skip over the header
    lispobj obj;
    unsigned int end_word_index = bitmap.nwords - 1;
    if (end_word_index) { // > 1 word
        unsigned int bitmap_word_index = 0;
        // 'mask' was preloaded with the bitmap.bits[0]
        do {
            // I suspect that mutating a structure layout with raw slots
            // could cause this assertion to fail, but at least we'll catch
            // that the user did something dangerous, exiting with an error
            // rather than causing heap corruption.
            if (nslots < N_WORD_BITS) lose("Mutated structure layout %p", (void*)layout);
            nslots -= N_WORD_BITS;
            lispobj* limit = where + N_WORD_BITS;
            do {
                if ((mask & 1) && is_lisp_pointer(obj = *where)) scav1(where, obj);
                mask >>= 1;
            } while (++where < limit);
            mask = bitmap.bits[++bitmap_word_index];
        } while (bitmap_word_index != end_word_index);
    }
    // Scan at most N_WORD_BITS more using the final word of the mask.
    // But there may be 0 slots remaining, or more than N_WORD_BITS slots remaining.
    int count = nslots <= N_WORD_BITS ? nslots : N_WORD_BITS;
    lispobj* limit = where + count;
    for ( ; where < limit ; mask >>= 1, ++where )
        if ((mask & 1) && is_lisp_pointer(obj = *where)) scav1(where, obj);
    nslots -= count;

    // Finally, see if the mask has its top bit on and there are more slots
    if (mask < 0 && nslots != 0) scavenge(where, nslots);
    return total_nwords;
}

static sword_t size_instance(lispobj *where) {
    return 1 + (instance_length(*where) | 1);
}

static sword_t
scav_funinstance(lispobj *where, lispobj header)
{
    int nslots = HeaderValue(header) & SHORT_HEADER_MAX_WORDS;
    // First things first: fix or enliven the layout pointer as necessary,
    // writing it back if and only if it changed.
    lispobj layoutptr = funinstance_layout(where);
    if (!layoutptr) return 1 + (nslots | 1); // skip, instance can't point to data
#ifdef LISP_FEATURE_METASPACE
    struct layout * layout = LAYOUT(layoutptr);
    scav1(&layout->friend, layout->friend);
#else
    lispobj old = layoutptr;
    scav1(&layoutptr, layoutptr);
    if (layoutptr != old) funinstance_layout(where) = layoutptr;
#endif
    // Do a similar thing as scav_instance but without any special cases.
    struct bitmap bitmap = get_layout_bitmap(LAYOUT(layoutptr));
    gc_assert(bitmap.nwords == 1);
    sword_t mask = bitmap.bits[0];
    ++where;
    lispobj* limit = where + nslots;
    lispobj obj;
    for ( ; where < limit ; mask >>= 1, ++where )
        if ((mask & 1) && is_lisp_pointer(obj = *where)) scav1(where, obj);
    return 1 + (nslots | 1);
}

/* Bignums use the highest bit of the header word as the GC mark bit.
 *
 * If assertions are enabled, the number of words taken up is double
 * what it would ordinarily be, which is a gross overstatement of the
 * the number of words actually needed for sanity-check bits,
 * i.e. ALIGN_UP(CEILING(nwords,N_WORD_BITS),2)
 * but the allocator is simplified by just doubling the space,
 * and it doesn't matter because this is only for testing */
static inline size_t bignum_nwords(lispobj header) {
#ifdef LISP_FEATURE_BIGNUM_ASSERTIONS
    // FIXME: how did I arrive at this constant? (being less than the other by some bits)
    int ndigits = ((unsigned int)header >> 8) & 0x7fffff;
    return 2 * ndigits + 2;
#else
    // NOTE: a better name for this constant would be BIGNUM_LENGTH_MASK,
    // because hypothetically we could want a max length of #x7FFFC
    // which would have a mask of #x7FFFF to not lose the low bits.
    size_t ndigits = (header >> N_WIDETAG_BITS) & MAXIMUM_BIGNUM_LENGTH;
    return 1 + (ndigits | 1); // round-to-odd + account for the header
#endif
}
static inline sword_t size_bignum(lispobj *where) {
    return bignum_nwords(*where);
}
static sword_t scav_bignum(lispobj __attribute__((unused)) *where, lispobj header) {
    return bignum_nwords(header);
}
static lispobj trans_bignum(lispobj object)
{
    gc_dcheck(lowtag_of(object) == OTHER_POINTER_LOWTAG);
    return copy_possibly_large_object(object, bignum_nwords(*native_pointer(object)),
                                      unboxed_region, PAGE_TYPE_UNBOXED);
}

#ifndef LISP_FEATURE_X86_64
lispobj fdefn_callee_lispobj(struct fdefn* fdefn) {
    lispobj raw_addr = (lispobj)fdefn->raw_addr;
    if (!raw_addr || points_to_asm_code_p(raw_addr))
        // technically this should return the address of the code object
        // containing asm routines, but it's fine to return 0.
        return 0;
    return raw_addr - FUN_RAW_ADDR_OFFSET;
}
#endif

static sword_t
scav_fdefn(lispobj *where, lispobj __attribute__((unused)) object)
{
    struct fdefn *fdefn = (struct fdefn *)where;
    scavenge(where + 1, 2); // 'name' and 'fun'
    lispobj obj = fdefn_callee_lispobj(fdefn);
    lispobj new = obj;
    scavenge(&new, 1);
    if (new != obj) fdefn->raw_addr += (sword_t)(new - obj);
    // Payload length is not computed from the header
    return FDEFN_SIZE;
}
static lispobj trans_fdefn(lispobj object) {
    return gc_copy_object(object, FDEFN_SIZE, mixed_region, PAGE_TYPE_MIXED);
}
static sword_t size_fdefn(lispobj __attribute__((unused)) *where) {
    return FDEFN_SIZE;
}

static sword_t
scav_unboxed(lispobj __attribute__((unused)) *where, lispobj object)
{
    sword_t length = HeaderValue(object) + 1;
    return ALIGN_UP(length, 2);
}

static lispobj
trans_unboxed(lispobj object)
{
    gc_dcheck(lowtag_of(object) == OTHER_POINTER_LOWTAG);
    sword_t length = HeaderValue(*native_pointer(object)) + 1;
    return copy_unboxed_object(object, ALIGN_UP(length, 2));
}

static lispobj
trans_ratio_or_complex(lispobj object)
{
    gc_dcheck(lowtag_of(object) == OTHER_POINTER_LOWTAG);
    lispobj* x = native_pointer(object);
    lispobj a = x[1];
    lispobj b = x[2];

    /* A zero ratio or complex means it was just allocated by fixed-alloc and
       a bignum can still be written there. Not a problem with a conservative GC
       since it will be pinned down. */
    if (fixnump(a) && fixnump(b)
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        && a && b
#endif
        )
 {
      return copy_unboxed_object(object, 4);
    }
    return gc_copy_object(object, 4, boxed_region, PAGE_TYPE_BOXED);
}

/* vector-like objects */
static lispobj
trans_vector_t(lispobj object)
{
    gc_dcheck(lowtag_of(object) == OTHER_POINTER_LOWTAG);

    struct vector*v = VECTOR(object);
    sword_t length = vector_len(v);
    unsigned int mask = (VECTOR_ALLOC_MIXED_REGION_BIT
                         | (flag_VectorWeak << ARRAY_FLAGS_POSITION));
    void* region = boxed_region;
    int page_type = PAGE_TYPE_BOXED;
    if (!length)
        page_type = PAGE_TYPE_UNBOXED, region = unboxed_region;
    else if (v->header & mask)
        page_type = PAGE_TYPE_MIXED, region = mixed_region;
    return copy_possibly_large_object(object, ALIGN_UP(length + 2, 2), region, page_type);
}

static sword_t
size_vector_t(lispobj *where)
{
    sword_t length = vector_len(((struct vector*)where));
    return ALIGN_UP(length + 2, 2);
}

static inline uword_t NWORDS(uword_t x, uword_t n_bits)
{
    /* A good compiler should be able to constant-fold this whole thing,
       even with the conditional. */
    if(n_bits <= N_WORD_BITS) {
        uword_t elements_per_word = N_WORD_BITS/n_bits;

        return ALIGN_UP(x, elements_per_word)/elements_per_word;
    }
    else {
        /* FIXME: should have some sort of assertion that N_WORD_BITS
           evenly divides n_bits */
        return x * (n_bits/N_WORD_BITS);
    }
}

#ifdef LISP_FEATURE_UBSAN
// If specialized vectors point to a vector of bits in their first
// word after the header, they can't be relocated to unboxed pages.
#define SPECIALIZED_VECTOR_ARGS mixed_region, PAGE_TYPE_MIXED
#else
#define SPECIALIZED_VECTOR_ARGS unboxed_region, PAGE_TYPE_UNBOXED
#endif

static inline void check_shadow_bits(__attribute((unused)) lispobj* v) {
#ifdef LISP_FEATURE_UBSAN
    if (is_lisp_pointer(v[1])) {
        scavenge(v + 1, 1); // shadow bits
        if (vector_len((struct vector*)native_pointer(v[1])) < vector_len((struct vector*)v))
          lose("messed up shadow bits for %p\n", v);
    } else if (v[1]) {
        char *origin_pc = (char*)(v[1]>>4);
        lispobj* code = component_ptr_from_pc(origin_pc);
        if (code) scavenge((lispobj*)&code, 1);
        /* else if (widetag_of(v)==SIMPLE_VECTOR_WIDETAG)
           lose("can't find code containing %p (vector=%p)", origin_pc, v); */
    }
#endif
}

#define DEF_SPECIALIZED_VECTOR(name, nwords) \
  static sword_t __attribute__((unused)) scav_##name(\
      lispobj *where, lispobj __attribute__((unused)) header) { \
    check_shadow_bits(where); \
    sword_t length = vector_len(((struct vector*)where)); \
    return ALIGN_UP(nwords + 2, 2); \
  } \
  static lispobj __attribute__((unused)) trans_##name(lispobj object) { \
    gc_dcheck(lowtag_of(object) == OTHER_POINTER_LOWTAG); \
    sword_t length = vector_len(VECTOR(object)); \
    return copy_possibly_large_object(object, ALIGN_UP(nwords + 2, 2), \
                             SPECIALIZED_VECTOR_ARGS); \
  } \
  static sword_t __attribute__((unused)) size_##name(lispobj *where) { \
    sword_t length = vector_len(((struct vector*)where)); \
    return ALIGN_UP(nwords + 2, 2); \
  }

DEF_SPECIALIZED_VECTOR(vector_nil, 0*length)
DEF_SPECIALIZED_VECTOR(vector_bit, NWORDS(length,1))
/* NOTE: base strings contain one more element of data (a terminating '\0'
 * to help interface with C functions) than indicated by the length slot.
 * UCS4 strings do not get a terminator element */
DEF_SPECIALIZED_VECTOR(base_string, NWORDS((length+1), 8))
#define DEF_SCAV_TRANS_SIZE_UB(nbits) \
  DEF_SPECIALIZED_VECTOR(vector_unsigned_byte_##nbits, NWORDS(length, nbits))
DEF_SCAV_TRANS_SIZE_UB(2)
DEF_SCAV_TRANS_SIZE_UB(4)
DEF_SCAV_TRANS_SIZE_UB(8)
DEF_SCAV_TRANS_SIZE_UB(16)
DEF_SCAV_TRANS_SIZE_UB(32)
DEF_SCAV_TRANS_SIZE_UB(64)
DEF_SCAV_TRANS_SIZE_UB(128)
#ifdef LONG_FLOAT_SIZE
DEF_SPECIALIZED_VECTOR(vector_long_float, length * LONG_FLOAT_SIZE)
DEF_SPECIALIZED_VECTOR(vector_complex_long_float, length * (2 * LONG_FLOAT_SIZE))
#endif

sword_t
scav_weak_pointer(lispobj *where, lispobj __attribute__((unused)) object)
{
    struct weak_pointer * wp = (struct weak_pointer*)where;
    /* If wp->next is non-NULL then it's already in the weak pointer chain.
     * If it is, then even if wp->value is now known to be live,
     * we can't fix (or don't need to fix) the slot, because removing
     * from a singly-linked-list is an O(n) operation */
    if (!wp->next) {
        lispobj pointee = wp->value;
        // A broken weak-pointer's value slot has unbound-marker
        // which does not satisfy is_lisp_pointer().
        int breakable = is_lisp_pointer(pointee) && (from_space_p(pointee)
#ifdef LISP_FEATURE_IMMOBILE_SPACE
         || (immobile_space_p(pointee) &&
             immobile_obj_gen_bits(base_pointer(pointee)) == from_space)
#endif
            );
        if (breakable) { // Pointee could potentially be garbage.
            // But it might already have been deemed live and forwarded.
            if (forwarding_pointer_p(native_pointer(pointee))) {
                wp->value = forwarding_pointer_value(native_pointer(pointee));
                return WEAK_POINTER_NWORDS;
            }
            add_to_weak_pointer_chain(wp);
        }
    }
    return WEAK_POINTER_NWORDS;
}

void smash_weak_pointers(void)
{
    struct weak_pointer *wp, *next_wp;
    for (wp = weak_pointer_chain; wp != WEAK_POINTER_CHAIN_END; wp = next_wp) {
        gc_assert(widetag_of(&wp->header) == WEAK_POINTER_WIDETAG);
        next_wp = wp->next;
        wp->next = NULL;

        lispobj val = wp->value;
        /* A weak pointer is placed onto the list only if it points to an object
         * that could potentially die. So first of all, 'val' must be a pointer,
         * and secondly it must not be in the assumed live set, which is checked
         * below by way of falling into lose(), which shouldn't happen */
        gc_assert(is_lisp_pointer(val));

        /* Now, we need to check whether the object has been forwarded. If
         * it has been, the weak pointer is still good and needs to be
         * updated. Otherwise, the weak pointer needs to be broken. */
        TEST_WEAK_CELL(wp->value, val, UNBOUND_MARKER_WIDETAG)
#ifdef LISP_FEATURE_GENCGC
        // Large objects are "moved" by touching the page table gen field.
        // Do nothing if the target of this weak pointer had that happen.
        else if (new_space_p(val)) { }
#endif
        else
            lose("unbreakable pointer %p", wp);
    }
    weak_pointer_chain = WEAK_POINTER_CHAIN_END;

    struct cons* vectors = weak_vectors;
    while (vectors) {
        struct vector* vector = (struct vector*)vectors->car;
        vectors = (struct cons*)vectors->cdr;
        ensure_non_ptr_word_writable(&vector->header);
        UNSET_WEAK_VECTOR_VISITED(vector);
        sword_t len = vector_len(vector);
        sword_t i;
        for (i = 0; i<len; ++i) {
            lispobj val = vector->data[i];
            // Ignore non-pointers
            if (is_lisp_pointer(val)) {
                TEST_WEAK_CELL(vector->data[i], val, NIL);
            }
        }
    }
    weak_vectors = 0;
}


/* Hash tables */

#if N_WORD_BITS == 32
#define EQ_HASH_MASK 0x1fffffff
#elif N_WORD_BITS == 64
#define EQ_HASH_MASK 0x1fffffffffffffff
#endif

/* Compute the EQ-hash of KEY. This must match POINTER-HASH in
 * target-hash-table.lisp.  */
#define EQ_HASH(key) ((key) & EQ_HASH_MASK)

/* List of weak hash tables chained through their NEXT-WEAK-HASH-TABLE
 * slot. Set to NULL at the end of a collection.
 *
 * This is not optimal because, when a table is tenured, it won't be
 * processed automatically; only the yougest generation is GC'd by
 * default. On the other hand, all applications will need an
 * occasional full GC anyway, so it's not that bad either.  */
struct hash_table *weak_hash_tables = NULL;
struct hopscotch_table weak_objects; // other than weak pointers

/* Return true if OBJ has already survived the current GC. */
static inline int pointer_survived_gc_yet(lispobj obj)
{
#ifdef LISP_FEATURE_CHENEYGC
    // This is the most straightforward definition.
    return (!from_space_p(obj) || forwarding_pointer_p(native_pointer(obj)));
#else
    /* Check for a pointer to dynamic space before considering immobile space.
       Based on the relative size of the spaces, this should be a win because
       if the object is in the dynamic space and not the 'from' generation
       we don't want to test immobile_space_p() at all.
       Additionally, pinned_p() is both more expensive and less likely than
       forwarding_pointer_p(), so we want to reverse those conditions, which
       would not be possible with pinned_p() buried inside from_space_p(). */
    page_index_t page_index = find_page_index((void*)obj);
    if (page_index >= 0)
        return page_table[page_index].gen != from_space ||
               forwarding_pointer_p(native_pointer(obj)) ||
               pinned_p(obj, page_index);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (immobile_space_p(obj))
        return immobile_obj_gen_bits(base_pointer(obj)) != from_space;
#endif
    return 1;
#endif
}

#define HT_ENTRY_LIVENESS_FUN_ARRAY_NAME weak_ht_alivep_funs
#include "weak-hash-pred.inc"

/* Return the beginning of data in ARRAY (skipping the header and the
 * length) or NULL if it isn't an array of the specified widetag after
 * all. */
static inline void *get_array_data(lispobj array, int widetag)
{
    if (is_lisp_pointer(array) && widetag_of(native_pointer(array)) == widetag)
        return &(VECTOR(array)->data[0]);
    lose("bad type: %"OBJ_FMTX" should have widetag %x", array, widetag);
}

extern uword_t gc_private_cons(uword_t, uword_t);

void add_to_weak_vector_list(lispobj* vector, lispobj header)
{
    if (!(header & WEAK_VECTOR_VISITED_BIT)) {
        weak_vectors = (struct cons*)gc_private_cons((uword_t)vector,
                                                     (uword_t)weak_vectors);
        NON_FAULTING_STORE(*vector |= WEAK_VECTOR_VISITED_BIT, vector);
    }
}

static inline void add_trigger(lispobj triggering_object, lispobj* plivened_object)
{
    if (is_lisp_pointer(*plivened_object)) // Nonpointer objects are ignored
        hopscotch_put(&weak_objects, triggering_object,
                      gc_private_cons((uword_t)plivened_object,
                                       hopscotch_get(&weak_objects,
                                                     triggering_object, 0)));
}

int debug_weak_ht = 0;
static inline void add_kv_triggers(lispobj* pair, int weakness)
{
    if (debug_weak_ht) {
        const char *const strings[3] = {"key","val","key-or-val"}; // {1, 2, 3}
        fprintf(stderr, "weak %s: <%"OBJ_FMTX",%"OBJ_FMTX">\n",
                strings[weakness-1], pair[0], pair[1]);
    }
    if (weakness & 1) add_trigger(pair[0], &pair[1]);
    if (weakness & 2) add_trigger(pair[1], &pair[0]);
}

/* This is essentially a set join operation over the set of all live objects
 * against the set of watched objects.
 * The question is which way to perform the join for maximum efficiency:
 *
 * 1. as each object is transported, test if it's in the trigger table
 * or
 * 2. scan the trigger table periodically checking whether each object is live.
 *
 * (1) performs a constant amount more work (a hashtable lookup) per forwarding
 *     and also has to maintain some kind of queue of the objects whose trigger
 *     condition was met
 * (2) has no hashtable lookup entailed by transporting, but performs a number
 *     of extra survived_gc_yet() calls periodically corresponding to triggering
 *     objects that and are not known to be live.
 *
 * There are far more transported objects than key objects in the weak object
 * table, so despite that (1) has lower computational complexity,
 * it seems like (2) works well in practice due to the cheaper implementation.
 * Obviously we could benchmark and compare, but seeing as how this already
 * fixes the scaling problem in a huge way, it's not an important question.
 */

/* Call 'predicate' on each triggering object, and if it returns 1, then call
 * 'mark' on each livened object, or use scav1() if 'mark' is null */
boolean test_weak_triggers(int (*predicate)(lispobj), void (*mark)(lispobj))
{
    extern void gc_private_free(struct cons*);
    int old_count = weak_objects.count;
    int index;
    lispobj trigger_obj;

    if (!old_count) return 0;
    if (debug_weak_ht)
        printf("begin scan_weak_pairs: count=%d\n", old_count);

    struct cons **values = (struct cons**)weak_objects.values;
    if (!predicate)
        predicate = pointer_survived_gc_yet;

    for_each_hopscotch_key(index, trigger_obj, weak_objects) {
        gc_assert(is_lisp_pointer(trigger_obj));
        if (predicate(trigger_obj)) {
            struct cons* chain;
            if (debug_weak_ht) {
                fprintf(stderr, "weak object %"OBJ_FMTX" livens", trigger_obj);
                for ( chain = values[index] ; chain ; chain = (struct cons*)chain->cdr )
                    fprintf(stderr, " *%"OBJ_FMTX"=%"OBJ_FMTX,
                            chain->car, *(lispobj*)chain->car);
                fputc('\n', stderr);
            }
            chain = values[index];
            gc_assert(chain);
            for ( ; chain ; chain = (struct cons*)chain->cdr ) {
                lispobj *plivened_obj = (lispobj*)chain->car;
                // Don't scavenge the cell in place! We lack the information
                // required to set the rehash flag on address-sensitive keys.
                lispobj livened_obj = *plivened_obj;
                if (mark)
                    mark(livened_obj);
                else
                    scav1(&livened_obj, livened_obj);
            }
            gc_private_free(values[index]);
            hopscotch_delete(&weak_objects, trigger_obj);
            if (!weak_objects.count) {
                hopscotch_reset(&weak_objects);
                if (debug_weak_ht)
                    fprintf(stderr, "no more weak pairs\n");
                return 1;
            }
            gc_assert(weak_objects.count > 0);
        } else {
            if (debug_weak_ht)
                fprintf(stderr, "weak object %"OBJ_FMTX" still dead\n", trigger_obj);
        }
    }
    if (debug_weak_ht)
        printf("end scan_weak_pairs: count=%d\n", weak_objects.count);
    return weak_objects.count != old_count;
}

int finalizer_thread_runflag = 1;
#ifdef LISP_FEATURE_SB_THREAD
#ifdef LISP_FEATURE_WIN32
CRITICAL_SECTION finalizer_mutex;
CONDITION_VARIABLE finalizer_condvar;
void finalizer_thread_wait () {
    EnterCriticalSection(&finalizer_mutex);
    if (finalizer_thread_runflag)
        SleepConditionVariableCS(&finalizer_condvar, &finalizer_mutex, INFINITE);
    LeaveCriticalSection(&finalizer_mutex);
}
void finalizer_thread_wake () {
    WakeAllConditionVariable(&finalizer_condvar);
}
void finalizer_thread_stop () {
    EnterCriticalSection(&finalizer_mutex);
    finalizer_thread_runflag = 0;
    WakeAllConditionVariable(&finalizer_condvar);
    LeaveCriticalSection(&finalizer_mutex);
}
#else
pthread_mutex_t finalizer_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t finalizer_condvar = PTHREAD_COND_INITIALIZER;
void finalizer_thread_wait () {
    ignore_value(mutex_acquire(&finalizer_mutex));
    if (finalizer_thread_runflag)
        pthread_cond_wait(&finalizer_condvar, &finalizer_mutex);
    ignore_value(mutex_release(&finalizer_mutex));
}
void finalizer_thread_wake() {
    pthread_cond_broadcast(&finalizer_condvar);
}
void finalizer_thread_stop() {
    ignore_value(mutex_acquire(&finalizer_mutex));
    finalizer_thread_runflag = 0;
    pthread_cond_broadcast(&finalizer_condvar);
    ignore_value(mutex_release(&finalizer_mutex));
}
#endif
#endif

void gc_common_init()
{
#ifdef LISP_FEATURE_WIN32
    InitializeCriticalSection(&finalizer_mutex);
    InitializeConditionVariable(&finalizer_condvar);
#endif
    hopscotch_init();
    hopscotch_create(&weak_objects, HOPSCOTCH_HASH_FUN_DEFAULT, N_WORD_BYTES,
                     32 /* logical bin count */, 0 /* default range */);
}

static inline boolean stable_eql_hash_p(lispobj obj)
{
    return lowtag_of(obj) == OTHER_POINTER_LOWTAG
        && widetag_of((lispobj*)(obj-OTHER_POINTER_LOWTAG)) <= SYMBOL_WIDETAG;
}

/* EQUAL and EQUALP tables always have hash vectors, so GC always knows
 * for any given key whether it was hashed by address.
 * EQ never has a hash vector (except if there is a user-defined hash function)
 * but always hashes by the pointer bits (which could be an address or immediate).
 * EQL never has a hash vector (same exception), but can hash some objects
 * by their contents, not their address. This macro determines for a key whether
 * its pointer bits force a rehash. In the case where we would call
 * stable_eql_hash_p(), skip the call if 'rehash' is already 1.
 */
#define SHOULD_REHASH(oldkey, newkey, hashvec, hv_index) \
  ((newkey != oldkey) && \
   (!hashvec ? rehash || !eql_hashing || !stable_eql_hash_p(newkey) : \
    hashvec[hv_index] == MAGIC_HASH_VECTOR_VALUE))


/* Scavenge the "real" entries in the hash-table kv vector. The vector element
 * at index 0 bounds the scan. The element at length-1 (the hash table itself)
 * was scavenged already.
 *
 * We can disregard any entry in which both key and value are immediates.
 * This effectively ignores empty pairs, as well as makes fixnum -> fixnum table
 * more efficient.
 */
#define SCAV_ENTRIES(entry_alivep, defer)                                      \
    boolean __attribute__((unused)) any_deferred = 0;                          \
    boolean rehash = 0;                                                        \
    unsigned hwm = KV_PAIRS_HIGH_WATER_MARK(data);                             \
    unsigned i;                                                                \
    for (i = 1; i <= hwm; i++) {                                               \
        lispobj key = data[2*i], value = data[2*i+1];                          \
        if (at_least_one_pointer_p(key,value)) {                               \
          if (!entry_alivep) { defer; any_deferred = 1; } else {               \
            /* Scavenge the key and value. */                                  \
            scav_entry(&data[2*i]);                                            \
            /* mark the table for rehash if address-based key moves */         \
            if (SHOULD_REHASH(key, data[2*i], hashvals, i)) rehash = 1;        \
    }}}                                                                        \
    /* Though at least partly writable, vector element 1 could be on a write-protected page. */ \
    if (rehash) \
      NON_FAULTING_STORE(KV_PAIRS_REHASH(data) |= make_fixnum(1), &data[1])

static void scan_nonweak_kv_vector(struct vector *kv_vector, void (*scav_entry)(lispobj*))
{
    lispobj* data = kv_vector->data;

    if (!vector_flagp(kv_vector->header, VectorAddrHashing)) {
        // All keys were hashed address-insensitively
        return (void)scavenge(data + 2, KV_PAIRS_HIGH_WATER_MARK(data) * 2);
    }
    // Read the hash vector (or NIL) from the last element. If the last element
    // satisfies instancep() then this vector belongs to a weak table,
    // and the KV vector has had its weakness removed temporarily to simplify
    // rehashing, which occurs only when rehashing without growing.
    // When growing a weak table, the KV vector is not created as weak initially;
    // its last element points to the hash-vector as for any strong KV vector.
    sword_t kv_length = vector_len(kv_vector);
    lispobj kv_supplement = data[kv_length-1];
    boolean eql_hashing = 0; // whether this table is an EQL table
    if (instancep(kv_supplement)) {
        struct hash_table* ht = (struct hash_table*)native_pointer(kv_supplement);
        eql_hashing = hashtable_kind(ht) == 1;
        kv_supplement = ht->hash_vector;
    } else if (kv_supplement == T) { // EQL hashing on a non-weak table
        eql_hashing = 1;
        kv_supplement = NIL;
    }
    uint32_t *hashvals = 0;
    if (kv_supplement != NIL) {
        hashvals = get_array_data(kv_supplement, SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG);
        gc_assert(2 * vector_len(VECTOR(kv_supplement)) + 1 == kv_length);
    }
    SCAV_ENTRIES(1, );
}

boolean scan_weak_hashtable(struct hash_table *hash_table,
                            int (*predicate)(lispobj,lispobj),
                            void (*scav_entry)(lispobj*))
{
    lispobj *data = get_array_data(hash_table->pairs, SIMPLE_VECTOR_WIDETAG);
    if (data == NULL)
        lose("invalid kv_vector %"OBJ_FMTX, hash_table->pairs);
    sword_t kv_length = vector_len(VECTOR(hash_table->pairs));

    uint32_t *hashvals = 0;
    if (hash_table->hash_vector != NIL) {
        hashvals = get_array_data(hash_table->hash_vector,
                                     SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG);
        gc_assert(vector_len(VECTOR(hash_table->hash_vector)) ==
                  vector_len(VECTOR(hash_table->next_vector)));
    }

     /* next_vector and hash_vector have a 1:1 size relation.
      * kv_vector is twice that plus the supplemental cell.
      * The index vector length is arbitrary - it can be smaller or larger
      * than the maximum number of table entries. */
    gc_assert(2 * vector_len(VECTOR(hash_table->next_vector)) + 1 == kv_length);

    int weakness = hashtable_weakness(hash_table);
    boolean eql_hashing = hashtable_kind(hash_table) == 1;
    /* Work through the KV vector. */
    SCAV_ENTRIES(predicate(key, value), add_kv_triggers(&data[2*i], weakness));
    if (!any_deferred && debug_weak_ht)
        fprintf(stderr,
                "will skip rescan of weak ht: %d/%d items\n",
                (int)KV_PAIRS_HIGH_WATER_MARK(data), (int)(kv_length/2));

    return any_deferred;
}

sword_t
scav_vector_t(lispobj *where, lispobj header)
{
    sword_t length = vector_len((struct vector*)where);

    check_shadow_bits(where);
    /* SB-VM:VECTOR-HASHING-FLAG is set for all hash tables in the
     * Lisp HASH-TABLE code to indicate need for special GC support.
     * But note that if the vector is a hashing vector that is neither
     * weak nor contains any key hashed by its address, then it is basically
     * a regular vector, except that GC needs only to look at cells below
     * the high-water-mark, plus the SUPPLEMENT slot at the end */
    if (vector_flags_zerop(header)) { // ordinary simple-vector
 normal:
        scavenge(where + 2, length);
        goto done;
    }

    if (vector_is_weak_not_hashing_p(header)) {
        add_to_weak_vector_list(where, header);
        goto done;
    }

    lispobj* data = where + 2;
    // Verify that the rehash stamp is a fixnum
    gc_assert(fixnump(data[1]));

    /* Scavenge element (length-1), which may be a hash-table structure. */
    scavenge(&data[length-1], 1);
    if (!vector_flagp(header, VectorWeak)) {
        scan_nonweak_kv_vector((struct vector*)where, gc_scav_pair);
        goto done;
    }

    lispobj ltable = data[length-1];
    if (!is_lisp_pointer(ltable)) {
        /* This'll happen when REHASH clears the header of old-kv-vector
         * and fills it with zero, but some other thread simulatenously
         * sets the header in %%PUTHASH.
         */
        /* And it's ok if the table backpointer hasn't been stored yet,
         * if the high-water-mark is 0 */
        if (KV_PAIRS_HIGH_WATER_MARK(data))
            fprintf(stderr,
                "Warning: no pointer at %p in hash table: this indicates "
                "non-fatal corruption caused by concurrent access to a "
                "hash-table from multiple threads. Any accesses to "
                "hash-tables shared between threads should be protected "
                "by locks.\n", &data[length-1]);
        goto normal;
    }
    struct hash_table *hash_table  = (struct hash_table *)native_pointer(ltable);
    if (widetag_of(&hash_table->header) != INSTANCE_WIDETAG) {
        lose("hash table not instance (%"OBJ_FMTX" at %p)",
             hash_table->header,
             hash_table);
    }
    gc_assert(hashtable_weakp(hash_table));

    /* Scavenge slots of hash table, which will fix the positions of the other
     * needed objects. */
    scav_instance((lispobj *)hash_table, hash_table->header);

    /* Cross-check the kv_vector. */
    if (where != native_pointer(hash_table->pairs))
        lose("hash_table table!=this table %"OBJ_FMTX, hash_table->pairs);

    if (hash_table->next_weak_hash_table == NIL) {
        int weakness = hashtable_weakness(hash_table);
        boolean defer = 1;
        /* Key-AND-Value means that no scavenging can/will be performed as
         * a consequence of visiting the table. Each entry is looked at once
         * only, after _all_ other work is done, and then it's either live
         * or it isn't based on whether both halves are live. So the initial
         * value of 'defer = 1' is correct. For all other weakness kinds,
         * we might be able to skip rescan depending on whether all entries
         * are actually live right now, as opposed to provisionally live */
        if (weakness != WEAKNESS_KEY_AND_VALUE)
            defer = scan_weak_hashtable(hash_table,
                                        weak_ht_alivep_funs[weakness],
                                        gc_scav_pair);
        /* There is a down-side to *not* pushing the table into the list,
         * but it should not matter too much: if we attempt to scavenge more
         * than once (when and only when the newspace areas overflow),
         * then we don't know that we already did it, and we'll do it again.
         * This is the same as occurs on all other objects */
        if (defer) {
            NON_FAULTING_STORE(hash_table->next_weak_hash_table
                               = (lispobj)weak_hash_tables,
                               &hash_table->next_weak_hash_table);
            weak_hash_tables = hash_table;
        }
    }
 done:
    return (ALIGN_UP(length + 2, 2));
}

/* Walk through the chain whose first element is *FIRST and remove
 * dead weak entries.
 * Return the new value for 'should rehash'.
 *
 * This operation might have to touch a hash-table that is currently
 * on a write-protected page, as follows:
 *    hash-table in gen5 (WRITE-PROTECTED) -> pair vector in gen5 (NOT WRITE-PROTECTED)
 *    -> younger k/v in gen1 that are deemed not-alive.
 * That's all fine, but now we have to store into the table for two reasons:
 *  1. to adjust the count
 *  2. to store the list of reusable cells
 * The former store is a non-pointer, but the latter may create an old->young pointer,
 * because the list of cells for reuse is freshly consed (and therefore young).
 * Moreover, when updating 'smashed_cells', that slot might not even be on the same
 * hardware page as the table header (if a page-spanning object) so it might be
 * unwritable even if words 0 through <something> are writable.
 * Employing the NON_FAULTING_STORE macro might make sense for the non-pointer slot,
 * except that it's potentially a lot more unprotects and reprotects.
 * Better to just get it done once.
 */
static inline boolean
cull_weak_hash_table_bucket(struct hash_table *hash_table,
                            uint32_t bucket, uint32_t index,
                            lispobj *kv_vector,
                            uint32_t *next_vector, uint32_t *hash_vector,
                            int (*alivep_test)(lispobj,lispobj),
                            void (*fix_pointers)(lispobj[2]),
                            boolean save_culled_values,
                            boolean rehash)
{
    const lispobj empty_symbol = UNBOUND_MARKER_WIDETAG;
    int eql_hashing = hashtable_kind(hash_table) == 1;
    for ( ; index ; index = next_vector[index] ) {
        lispobj key = kv_vector[2 * index];
        lispobj value = kv_vector[2 * index + 1];
        // Lisp might not have gotten around to pruning a chain
        // containing previously culled items.
        if (key == empty_symbol && value == empty_symbol) continue;
        // If the pair doesn't have both halves empty,
        // then it mustn't have either half empty.
        // FIXME: this looks like a potential data race - do we definitely store
        // the key and value before inserting into a chain? Probably.
        gc_assert(key != empty_symbol);
        gc_assert(value != empty_symbol);
        if (!alivep_test(key, value)) {
            gc_assert(hash_table->_count > 0);
            if (save_culled_values) {
                lispobj val = kv_vector[2 * index + 1];
                gc_assert(!is_lisp_pointer(val));
                struct cons *cons = (struct cons*)
                  gc_general_alloc(cons_region, sizeof(struct cons), PAGE_TYPE_CONS);
                // Lisp code which manipulates the culled_values slot must use
                // compare-and-swap, but C code need not, because GC runs in one
                // thread and has stopped the Lisp world.
                cons->cdr = hash_table->culled_values;
                cons->car = val;
                lispobj list = make_lispobj(cons, LIST_POINTER_LOWTAG);
                notice_pointer_store(&hash_table->culled_values);
                hash_table->culled_values = list;
                // ensure this cons doesn't get smashed into (0 . 0) by full gc
                if (!compacting_p()) gc_mark_obj(list);
            }
            kv_vector[2 * index] = empty_symbol;
            kv_vector[2 * index + 1] = empty_symbol;
            ensure_non_ptr_word_writable(&hash_table->_count);
            hash_table->_count -= make_fixnum(1);

            // Push (index . bucket) onto the table's GC culled cell list.
            // If each of 'index' and 'bucket' can be represented in 14 bits,
            // then pack them in a fixnum. Otherwise a cons. This makes the code
            // essentially identical regardless of word size while in most cases
            // consuming only 1 cons per culled item.
            struct cons *cons;
            if ((index & ~0x3FFF) | (bucket & ~0x3FFF)) { // large values
                cons = (struct cons*)
                  gc_general_alloc(cons_region, 2 * sizeof(struct cons), PAGE_TYPE_CONS);
                cons->car = make_lispobj(cons + 1, LIST_POINTER_LOWTAG);
                cons[1].car = make_fixnum(index);  // which cell became free
                cons[1].cdr = make_fixnum(bucket); // which chain was it in
                if (!compacting_p()) gc_mark_obj(cons->car);
            } else { // small values
                cons = (struct cons*)
                  gc_general_alloc(cons_region, sizeof(struct cons), PAGE_TYPE_CONS);
                cons->car = ((index << 14) | bucket) << N_FIXNUM_TAG_BITS;
            }
            cons->cdr = hash_table->smashed_cells;
            // Lisp code must atomically pop the list whereas this C code
            // always wins and does not need compare-and-swap.
            notice_pointer_store(&hash_table->smashed_cells);
            hash_table->smashed_cells = make_lispobj(cons, LIST_POINTER_LOWTAG);
            // ensure this cons doesn't get smashed into (0 . 0) by full gc
            if (!compacting_p()) gc_mark_obj(hash_table->smashed_cells);

        } else {
            if (fix_pointers) { // Follow FPs as necessary
                lispobj key = kv_vector[2 * index];
                fix_pointers(&kv_vector[2 * index]);
                if (SHOULD_REHASH(key, kv_vector[2 * index], hash_vector, index))
                    rehash = 1;
            }
        }
    }
    return rehash;
}

static void
cull_weak_hash_table (struct hash_table *hash_table,
                      int (*alivep_test)(lispobj,lispobj),
                      void (*fix_pointers)(lispobj[2]))
{
    sword_t i;

    lispobj *kv_vector = get_array_data(hash_table->pairs, SIMPLE_VECTOR_WIDETAG);
    uint32_t *index_vector = get_array_data(hash_table->index_vector,
                                            SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG);
    sword_t n_buckets = vector_len(VECTOR(hash_table->index_vector));
    uint32_t *next_vector = get_array_data(hash_table->next_vector,
                                           SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG);
    uint32_t *hash_vector = 0;
    if (hash_table->hash_vector != NIL)
        hash_vector = get_array_data(hash_table->hash_vector,
                                     SIMPLE_ARRAY_UNSIGNED_BYTE_32_WIDETAG);

    boolean rehash = 0;
    boolean save_culled_values = (hash_table->flags & make_fixnum(4)) != 0;
    // I'm slightly confused as to why we can't (or don't) compute the
    // 'should rehash' flag while scavenging the weak k/v vector.
    // I believe the explanation is this: for weak-key-AND-value tables, the vector
    // is never scavenged. It just ends up here after all other scavenging is done.
    // We then need to fix the still-live pointers, which entails possibly setting the
    // 'rehash' flag. It would not make sense to treat the other 3 flavors of
    // weakness any differently.
    for (i = 0; i < n_buckets; i++) {
        if (cull_weak_hash_table_bucket(hash_table, i, index_vector[i],
                                        kv_vector, next_vector, hash_vector,
                                        alivep_test, fix_pointers,
                                        save_culled_values, rehash))
            rehash = 1;
    }
    /* If an EQ-based key has moved, mark the hash-table for rehash */
    if (rehash)
        NON_FAULTING_STORE(KV_PAIRS_REHASH(kv_vector) |= make_fixnum(1), &kv_vector[1]);
}

/* Fix one <k,v> pair in a weak hashtable.
 * Do not call scavenge(), just follow forwarding pointers */
static void pair_follow_fps(lispobj ht_entry[2])
{
    lispobj obj = ht_entry[0];
    /* Define a macro to safely test forwarding_pointer_p().
     * This could, in general, use 'from_space_p', however, from_space_p is more
     * strict than "can we determine that it's OK to call forwarding_pointer_p".
     * For gencgc it's OK to call forwarded_pointer_p even if the object is
     * not in from_space, and so the test need not involve pinned_p */
#ifdef LISP_FEATURE_CHENEYGC
#define IS_FORWARDED(x) (is_lisp_pointer(x) && from_space_p(x) && \
                        forwarding_pointer_p(native_pointer(x)))
#else
#define IS_FORWARDED(x) (is_lisp_pointer(x) && find_page_index((void*)x) >= 0 && \
                        forwarding_pointer_p(native_pointer(x)))
#endif
    if (IS_FORWARDED(obj))
        ht_entry[0] = forwarding_pointer_value(native_pointer(obj));
    obj = ht_entry[1];
    if (IS_FORWARDED(obj))
        ht_entry[1] = forwarding_pointer_value(native_pointer(obj));
#undef IS_FORWARDED
}

/* Remove dead entries from weak hash tables. */
void cull_weak_hash_tables(int (*alivep[4])(lispobj,lispobj))
{
    struct hash_table *table, *next;

    for (table = weak_hash_tables; table != NULL; table = next) {
        next = (struct hash_table *)table->next_weak_hash_table;
        NON_FAULTING_STORE(table->next_weak_hash_table = NIL,
                           &table->next_weak_hash_table);
        int weakness = hashtable_weakness(table);
        gc_assert((weakness & ~3) == 0);
        cull_weak_hash_table(table, alivep[weakness], compacting_p() ? pair_follow_fps : 0);
    }
    weak_hash_tables = NULL;
    /* Reset weak_objects only if the count is nonzero.
     * If test_weak_triggers() caused the count to hit zero, then it already
     * performed a reset. Consecutive resets with no intervening insert are
     * technically ok, but it's best to avoid halving the size twice,
     * which is what an extra reset would do if it saw no inserts. */
    if (weak_objects.count)
        hopscotch_reset(&weak_objects);
#ifdef LISP_FEATURE_GENCGC
    // Close the region used when pushing items to the finalizer queue
    ensure_region_closed(cons_region, PAGE_TYPE_CONS);
#endif
}


/*
 * initialization
 */

static sword_t scav_lose(lispobj *where, lispobj object)
{
    lose("no scavenge function for object %p (widetag %#x)",
         (void*)object, widetag_of(where));

    return 0; /* bogus return value to satisfy static type checking */
}

static lispobj
trans_lose(lispobj object)
{
    lose("no transport function for object %p (widetag %#x)",
         (void*)object, widetag_of(native_pointer(object)));
    return NIL; /* bogus return value to satisfy static type checking */
}

static sword_t
size_lose(lispobj *where)
{
    lose("no size function for object at %p (widetag %#x)",
         (void*)where, widetag_of(where));
    return 1; /* bogus return value to satisfy static type checking */
}

/*
 * initialization
 */

sword_t scav_code_header(lispobj *object, lispobj header);
#include "genesis/gc-tables.h"

/* Find the code object for the given pc, or return NULL on
   failure. */
lispobj *
component_ptr_from_pc(char *pc)
{
    lispobj *object = search_all_gc_spaces(pc);

    if (object != NULL && widetag_of(object) == CODE_HEADER_WIDETAG)
        return object;

    return NULL;
}

/// Return the name of the simple-fun containing 'pc',
/// and if 'pfun' is non-null then store the base pointer
/// to the function in *pfun. This is an easier-to-use interface than
/// just returning the function, because accessing the name requires
/// knowing the function's index into the entry-point vector.
lispobj simple_fun_name_from_pc(char *pc, lispobj** pfun)
{
    struct code* code = (void*)component_ptr_from_pc(pc);
    if (!code) return 0; // not in lisp heap
    char *insts = code_text_start(code);
    unsigned int* offsets = code_fun_table(code) - 1;
    int i;
    // Scanning backwards makes the stopping condition easy:
    // the first function lower than 'pc' is the right answer.
    for (i=code_n_funs(code)-1; i>=0; --i) {
        struct simple_fun* fun = (void*)(insts + offsets[-i]);
        if ((char*)fun < pc) {
            if (pfun) *pfun = (lispobj*)fun;
            return code->constants[i*CODE_SLOTS_PER_SIMPLE_FUN];
        }
    }
    return 0; // oops, how did this happen?
}

#ifdef LISP_FEATURE_UBSAN
// ubsan tracks memory origin by a not-exactly-gc-safe way
// that kinda works, as long as gc_search_space() doesn't crash,
// which it shouldn't if carefully visiting objects.
#define SEARCH_SPACE_FOLLOWS_FORWARDING_POINTERS 1
#else
#define SEARCH_SPACE_FOLLOWS_FORWARDING_POINTERS 0
#endif
/* Scan an area looking for an object which encloses the given pointer.
 * Return the object start on success, or NULL on failure. */
lispobj *
gc_search_space3(void *pointer, lispobj *start, void *limit)
{
    if (pointer < (void*)start || pointer >= limit) return NULL;

    size_t count;
#if SEARCH_SPACE_FOLLOWS_FORWARDING_POINTERS
    /* CAUTION: this code is _significantly_ slower than the production version
       due to the extra checks for forwarding.  Only use it if debugging */
    for ( ; (void*)start < limit ; start += count) {
        lispobj *forwarded_start;
        if (forwarding_pointer_p(start))
            forwarded_start = native_pointer(forwarding_pointer_value(start));
        else
            forwarded_start = start;
        lispobj thing = *forwarded_start;
        count = OBJECT_SIZE(thing, forwarded_start);
        /* Check whether the pointer is within this object. */
        if (pointer < (void*)(start+count)) return start;
    }
#else
    for ( ; (void*)start < limit ; start += count) {
        lispobj thing = *start;
        count = OBJECT_SIZE(thing, start);
        /* Check whether the pointer is within this object. */
        if (pointer < (void*)(start+count)) return start;
    }
#endif
    return NULL;
}


// Binary-search the simple-funs in 'code' looking for an exact
// match to 'fun'. ('fun' can not be greater than or equal to the start
// of instructions for the returned function - it must be exactly
// an untagged pointer to the base address). Return -1 if not found.
// We don't actually need this to be fast, because it's used only
// when debugging. (Conservative stack scan relaxes the exactness
// contraint, and precise stack scan does not use this at all)
int simple_fun_index(struct code* code, struct simple_fun *fun)
{
    int min = 0;
    int max = code_n_funs(code) - 1;
    char *instruction_area = code_text_start(code);
    unsigned int* offsets = code_fun_table(code) - 1;
    while (min <= max) {
        int midpoint = (min + max) >> 1;
        char *guess = instruction_area + offsets[-midpoint];
        if (guess == (char*)fun) return midpoint;
        if (guess > (char*)fun) max = midpoint - 1; else min = midpoint + 1;
    }
    return -1;
}

/* Helper for valid_lisp_pointer_p (below) and
 * conservative_root_p (gencgc).
 *
 * pointer is the pointer to check validity of,
 * and start_addr is the address of the enclosing object.
 *
 * This is actually quite simple to check: because the heap state is assumed
 * consistent, and 'start_addr' is known good, having come from
 * gc_search_space(), only the 'pointer' argument is dubious.
 * So make 'start_addr' into a tagged pointer and see if that matches 'pointer'.
 * If it does, then 'pointer' is valid.
 */
int
properly_tagged_p_internal(lispobj pointer, lispobj *start_addr)
{
    // If a headerless object, confirm that 'pointer' is a list pointer.
    // Given the precondition that the heap is in a valid state,
    // it may be assumed that is_cons_half() is satisfied by both the
    // car and cdr.
    lispobj word = *start_addr;
    if (!is_header(word))
        return make_lispobj(start_addr, LIST_POINTER_LOWTAG) == pointer;

    int widetag = header_widetag(word);
    int lowtag = LOWTAG_FOR_WIDETAG(widetag);
    if (lowtag && make_lispobj(start_addr, lowtag) == pointer)
        return 1; // instant win

    // debug_info must be assigned prior to examining a code object for simple-funs.
    // This is how we distinguish objects that are partyway through construction.
    // It would be wrong to read garbage bytes from the simple-fun table.
    if (widetag == CODE_HEADER_WIDETAG && ((struct code*)start_addr)->debug_info) {
        if (functionp(pointer)) {
            lispobj* potential_fun = FUNCTION(pointer);
            if (widetag_of(potential_fun) == SIMPLE_FUN_WIDETAG &&
                simple_fun_index((struct code*)start_addr,
                                 (struct simple_fun*)potential_fun) >= 0)
                return 1;
        }
#ifdef RETURN_PC_WIDETAG
        /* LRA objects are similar to simple-funs in that they are
         * embedded objects. We can't actually do as precise a test
         * as for simple-funs, since we don't know where the LRAs are.
         * Nonetheless, the check of header validity should produce
         * very few false positives */
        if (lowtag_of(pointer) == OTHER_POINTER_LOWTAG) {
            lispobj *potential_lra = native_pointer(pointer);
            if ((widetag_of(potential_lra) == RETURN_PC_WIDETAG) &&
                ((potential_lra - HeaderValue(potential_lra[0])) == start_addr)) {
                return 1; /* It's as good as we can verify. */
            }
        }
#endif
    }
    return 0; // no good
}

/* META: Note the ambiguous word "validate" in the comment below.
 * This means "Decide whether <x> is valid".
 * But when you see os_validate() elsewhere, that doesn't mean to ask
 * whether something is valid, it says to *make* it valid.
 * I think it would be nice if we could avoid using the word in the
 * sense in which os_validate() uses it, which would entail renaming
 * a bunch of stuff, which is harder than just explaining why
 * the comments can be deceptive */

/* Used by the debugger to validate possibly bogus pointers before
 * calling MAKE-LISP-OBJ on them.
 *
 * FIXME: We would like to make this perfect, because if the debugger
 * constructs a reference to a bugs lisp object, and it ends up in a
 * location scavenged by the GC all hell breaks loose.
 *
 * Whereas conservative_root_p has to be conservative
 * and return true for all valid pointers, this could actually be eager
 * and lie about a few pointers without bad results... but that should
 * be reflected in the name.
 */
int
valid_lisp_pointer_p(lispobj pointer)
{
    lispobj *start = search_all_gc_spaces((void*)pointer);
    if (start != NULL)
        return properly_tagged_descriptor_p((void*)pointer, start);
    return 0;
}

static boolean can_invoke_post_gc(__attribute__((unused)) struct thread* th,
                                  sigset_t *context_sigmask)
{
#ifdef LISP_FEATURE_SB_THREAD
    lispobj obj = th->lisp_thread;
    /* Ok, I seriously doubt that this can happen now. Don't we create
     * the 'struct thread' with a pointer to its SB-THREAD:THREAD right away?
     * I thought so. But if I'm mistaken, give up. */
    if (!obj) return 0;
    struct thread_instance* lispthread = (void*)(obj - INSTANCE_POINTER_LOWTAG);

    /* If the SB-THREAD:THREAD has a 0 for its 'struct thread', give up.
     * This is the same as the THREAD-ALIVE-P test.  Maybe a thread that is
     * in the process of un-setting that slot performed this GC. */
    if (!lispthread->primitive_thread) return 0;

    /* I don't know why we aren't in general willing to run post-GC with some or all
     * deferrable signals blocked. "Obviously" the idea is to run post-GC code only in
     * a thread that is in a nominally pristine state, as opposed to one that is doing
     * any manner of monkey business. But was there some specific issue related to
     * running post-GC with signals blocked? Nontrivial post-GC code is bad anyway,
     * so how could trivial code be adversely affected? i.e. if your post-GC code
     * can't run with some signals blocked, then it shouldn't be your post-GC code. */
#endif
    return !deferrables_blocked_p(context_sigmask);
}

boolean
maybe_gc(os_context_t *context)
{
    lispobj gc_happened;
    __attribute__((unused)) struct thread *thread = get_sb_vm_thread();
    boolean were_in_lisp = !foreign_function_call_active_p(thread);

    if (were_in_lisp) {
        fake_foreign_function_call(context);
    }

    /* SUB-GC may return without GCing if *GC-INHIBIT* is set, in
     * which case we will be running with no gc trigger barrier
     * thing for a while.  But it shouldn't be long until the end
     * of WITHOUT-GCING.
     *
     * FIXME: It would be good to protect the end of dynamic space for
     * CheneyGC and signal a storage condition from there.
     */

    /* Restore the signal mask from the interrupted context before
     * calling into Lisp if interrupts are enabled. Why not always?
     *
     * Suppose there is a WITHOUT-INTERRUPTS block far, far out. If an
     * interrupt hits while in SUB-GC, it is deferred and the
     * os_context_sigmask of that interrupt is set to block further
     * deferrable interrupts (until the first one is
     * handled). Unfortunately, that context refers to this place and
     * when we return from here the signals will not be blocked.
     *
     * A kludgy alternative is to propagate the sigmask change to the
     * outer context.
     */
#ifndef LISP_FEATURE_SB_SAFEPOINT
    check_gc_signals_unblocked_or_lose(os_context_sigmask_addr(context));
    unblock_gc_signals();
#endif
    FSHOW((stderr, "/maybe_gc: calling SUB_GC\n"));
    /* FIXME: Nothing must go wrong during GC else we end up running
     * the debugger, error handlers, and user code in general in a
     * potentially unsafe place. Running out of the control stack or
     * the heap in SUB-GC are ways to lose. Of course, deferrables
     * cannot be unblocked because there may be a pending handler, or
     * we may even be in a WITHOUT-INTERRUPTS. */
    gc_happened = funcall1(StaticSymbolFunction(SUB_GC), 0);
    FSHOW((stderr, "/maybe_gc: gc_happened=%s\n",
           (gc_happened == NIL)
           ? "NIL"
           : ((gc_happened == T)
              ? "T"
              : "0")));
    /* gc_happened can take three values: T, NIL, 0.
     *
     * T means that the thread managed to trigger a GC, and post-gc
     * must be called.
     *
     * NIL means that the thread is within without-gcing, and no GC
     * has occurred.
     *
     * Finally, 0 means that *a* GC has occurred, but it wasn't
     * triggered by this thread; success, but post-gc doesn't have
     * to be called.
     */
    if ((gc_happened == T) &&
        /* See if interrupts are enabled or it's possible to enable
         * them. POST-GC has a similar check, but we don't want to
         * unlock deferrables in that case and get a pending interrupt
         * here. */
        ((read_TLS(INTERRUPTS_ENABLED,thread) != NIL) ||
         (read_TLS(ALLOW_WITH_INTERRUPTS,thread) != NIL))) {
#ifndef LISP_FEATURE_WIN32
        sigset_t *context_sigmask = os_context_sigmask_addr(context);
        if (can_invoke_post_gc(thread, context_sigmask)) {
            /* The gist of this is that we make it appear that return-from-signal
             * happened, and the calling code decided to take a detour through the
             * post-GC code. Except that we do it while the interrupt context
             * is still on the stack */
            thread_sigmask(SIG_SETMASK, context_sigmask, 0);
#ifndef LISP_FEATURE_SB_SAFEPOINT
            check_gc_signals_unblocked_or_lose(0);
#endif
#endif
            FSHOW((stderr, "/maybe_gc: calling POST_GC\n"));
            funcall0(StaticSymbolFunction(POST_GC));
#ifndef LISP_FEATURE_WIN32
        } else {
            FSHOW((stderr, "/maybe_gc: punting on POST_GC due to blockage\n"));
        }
#endif
    }

    if (were_in_lisp) {
        undo_fake_foreign_function_call(context);
    } else {
        /* Otherwise done by undo_fake_foreign_function_call. And
         something later wants them to be blocked. What a nice
         interface.*/
        block_blockable_signals(0);
    }

    FSHOW((stderr, "/maybe_gc: returning\n"));
    return (gc_happened != NIL);
}

#define BYTES_ZERO_BEFORE_END (1<<12)

/* There used to be a similar function called SCRUB-CONTROL-STACK in
 * Lisp and another called zero_stack() in cheneygc.c, but since it's
 * shorter to express in, and more often called from C, I keep only
 * the C one after fixing it. -- MG 2009-03-25 */

/* Zero the unused portion of the control stack so that old objects
 * are not kept alive because of uninitialized stack variables.
 *
 * "To summarize the problem, since not all allocated stack frame
 * slots are guaranteed to be written by the time you call an another
 * function or GC, there may be garbage pointers retained in your dead
 * stack locations. The stack scrubbing only affects the part of the
 * stack from the SP to the end of the allocated stack." - ram, on
 * cmucl-imp, Tue, 25 Sep 2001
 *
 * So, as an (admittedly lame) workaround, from time to time we call
 * scrub-control-stack to zero out all the unused portion. This is
 * supposed to happen when the stack is mostly empty, so that we have
 * a chance of clearing more of it: callers are currently (2002.07.18)
 * REPL, SUB-GC and sig_stop_for_gc_handler. */

/* Take care not to tread on the guard page and the hard guard page as
 * it would be unkind to sig_stop_for_gc_handler. Touching the return
 * guard page is not dangerous. For this to work the guard page must
 * be zeroed when protected. */

/* FIXME: I think there is no guarantee that once
 * BYTES_ZERO_BEFORE_END bytes are zero the rest are also zero. This
 * may be what the "lame" adjective in the above comment is for. In
 * this case, exact gc may lose badly. */
void
scrub_control_stack()
{
    scrub_thread_control_stack(get_sb_vm_thread());
}

void
scrub_thread_control_stack(struct thread *th)
{
    os_vm_address_t guard_page_address = CONTROL_STACK_GUARD_PAGE(th);
    os_vm_address_t hard_guard_page_address = CONTROL_STACK_HARD_GUARD_PAGE(th);
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* On these targets scrubbing from C is a bad idea, so we punt to
     * a routine in $ARCH-assem.S. */
    extern void arch_scrub_control_stack(struct thread *, os_vm_address_t, os_vm_address_t)
#ifdef LISP_FEATURE_X86_64
    __attribute__((sysv_abi))
#endif
    ;
    arch_scrub_control_stack(th, guard_page_address, hard_guard_page_address);
#else
    lispobj *sp = access_control_stack_pointer(th);
 scrub:
    if ((((os_vm_address_t)sp < (hard_guard_page_address + os_vm_page_size)) &&
         ((os_vm_address_t)sp >= hard_guard_page_address)) ||
        (((os_vm_address_t)sp < (guard_page_address + os_vm_page_size)) &&
         ((os_vm_address_t)sp >= guard_page_address) &&
         th->state_word.control_stack_guard_page_protected))
        return;
#ifdef LISP_FEATURE_STACK_GROWS_DOWNWARD_NOT_UPWARD
    do {
        *sp = 0;
    } while (((uword_t)sp--) & (BYTES_ZERO_BEFORE_END - 1));
    if ((os_vm_address_t)sp < (hard_guard_page_address + os_vm_page_size))
        return;
    do {
        if (*sp)
            goto scrub;
    } while (((uword_t)sp--) & (BYTES_ZERO_BEFORE_END - 1));
#else
    do {
        *sp = 0;
    } while (((uword_t)++sp) & (BYTES_ZERO_BEFORE_END - 1));
    if ((os_vm_address_t)sp >= hard_guard_page_address)
        return;
    do {
        if (*sp)
            goto scrub;
    } while (((uword_t)++sp) & (BYTES_ZERO_BEFORE_END - 1));
#endif
#endif /* LISP_FEATURE_C_STACK_IS_CONTROL_STACK */
}

#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)

void
scavenge_control_stack(struct thread *th)
{
    if (!compacting_p()) {
        long nwords = (lispobj*)access_control_stack_pointer(th) - th->control_stack_start;
        gc_mark_range(th->control_stack_start, nwords);
        return;
    }
    lispobj *object_ptr;

    /* In order to properly support dynamic-extent allocation of
     * non-CONS objects, the control stack requires special handling.
     * Rather than calling scavenge() directly, grovel over it fixing
     * broken hearts, scavenging pointers to oldspace, and pitching a
     * fit when encountering unboxed data.  This prevents stray object
     * headers from causing the scavenger to blow past the end of the
     * stack (an error case checked in scavenge()).  We don't worry
     * about treating unboxed words as boxed or vice versa, because
     * the compiler isn't allowed to store unboxed objects on the
     * control stack.  -- AB, 2011-Dec-02 */

    /* FIXME: I believe that this loop could be replaced by scavenge(),
     * as it can not "... blow past the end" on header words,
     * the way that heap_scavenge() might */
    for (object_ptr = th->control_stack_start;
         object_ptr < access_control_stack_pointer(th);
         object_ptr++) {

        lispobj object = *object_ptr;
#ifdef LISP_FEATURE_GENCGC
        if (forwarding_pointer_p(object_ptr))
            lose("unexpected forwarding pointer in scavenge_control_stack: %p, start=%p, end=%p",
                 object_ptr, th->control_stack_start, access_control_stack_pointer(th));
#endif
        if (is_lisp_pointer(object) && from_space_p(object)) {
            /* It currently points to old space. Check for a
             * forwarding pointer. */
            lispobj *ptr = native_pointer(object);
            if (forwarding_pointer_p(ptr)) {
                /* Yes, there's a forwarding pointer. */
                *object_ptr = forwarding_pointer_value(ptr);
            } else {
                /* Scavenge that pointer. */
                long n_words_scavenged =
                    (scavtab[header_widetag(object)])(object_ptr, object);
                gc_assert(n_words_scavenged == 1);
            }
        } else if (scavtab[header_widetag(object)] == scav_lose) {
            lose("unboxed object in scavenge_control_stack: %p->%"OBJ_FMTX", start=%p, end=%p",
                 object_ptr, object, th->control_stack_start, access_control_stack_pointer(th));
        }
    }
}

/* Scavenging Interrupt Contexts */

static int boxed_registers[] = BOXED_REGISTERS;

/* The GC has a notion of an "interior pointer" register, an unboxed
 * register that typically contains a pointer to inside an object
 * referenced by another pointer.  The most obvious of these is the
 * program counter, although many compiler backends define a "Lisp
 * Interior Pointer" register known to the runtime as reg_LIP, and
 * various CPU architectures have other registers that also partake of
 * the interior-pointer nature.  As the code for pairing an interior
 * pointer value up with its "base" register, and fixing it up after
 * scavenging is complete is horribly repetitive, a few macros paper
 * over the monotony.  --AB, 2010-Jul-14 */

/* These macros are only ever used over a lexical environment which
 * defines a pointer to an os_context_t called context, thus we don't
 * bother to pass that context in as a parameter. */

/* Define how to access a given interior pointer. */
#define ACCESS_INTERIOR_POINTER_pc \
    *os_context_pc_addr(context)
#define ACCESS_INTERIOR_POINTER_lip \
    *os_context_register_addr(context, reg_LIP)
#define ACCESS_INTERIOR_POINTER_lr \
    *os_context_lr_addr(context)
#define ACCESS_INTERIOR_POINTER_npc \
    *os_context_npc_addr(context)
#define ACCESS_INTERIOR_POINTER_ctr \
    *os_context_ctr_addr(context)

#define INTERIOR_POINTER_VARS(name) \
    uword_t name##_offset;    \
    int name##_register_pair

#define PAIR_INTERIOR_POINTER(name)                             \
    pair_interior_pointer(context,                              \
                          ACCESS_INTERIOR_POINTER_##name,       \
                          &name##_offset,                       \
                          &name##_register_pair,                \
                          #name)

/* One complexity here is that if a paired register is not found for
 * an interior pointer, then that pointer does not get updated.
 * Originally, there was some commentary about using an index of -1
 * when calling os_context_register_addr() on SPARC referring to the
 * program counter, but the real reason is to allow an interior
 * pointer register to point to the runtime, read-only space, or
 * static space without problems. */
#define FIXUP_INTERIOR_POINTER(name)                                    \
    do {                                                                \
        /* fprintf(stderr, "Fixing interior ptr "#name"\n"); */         \
        if (name##_register_pair >= 0) {                                \
            ACCESS_INTERIOR_POINTER_##name =                            \
                (*os_context_register_addr(context,                     \
                                           name##_register_pair)        \
                 & ~LOWTAG_MASK)                                        \
                + name##_offset;                                        \
        }                                                               \
    } while (0)

#ifdef LISP_FEATURE_PPC64
// reg_CODE holds a native pointer on PPC64
#define plausible_base_register(val,reg) (is_lisp_pointer(val)||reg==reg_CODE)
#else
#define plausible_base_register(val,reg) (is_lisp_pointer(val))
#endif

static void
pair_interior_pointer(os_context_t *context, uword_t pointer,
                      uword_t *saved_offset, int *register_pair,
                      char *regname)
{
    unsigned int i;

    /*
     * I (RLT) think this is trying to find the boxed register that is
     * closest to the LIP address, without going past it.  Usually, it's
     * reg_CODE or reg_LRA.  But sometimes, nothing can be found.
     */
    /* 0x7FFFFFFF on 32-bit platforms;
       0x7FFFFFFFFFFFFFFF on 64-bit platforms */
    *saved_offset = (((uword_t)1) << (N_WORD_BITS - 1)) - 1;
    *register_pair = -1;
    for (i = 0; i < (sizeof(boxed_registers) / sizeof(int)); i++) {
        uword_t offset;

        int regindex = boxed_registers[i];
        uword_t regval = *os_context_register_addr(context, regindex);

        /* An interior pointer is never relative to a non-pointer
         * register (an oversight in the original implementation).
         * The simplest argument for why this is true is to consider
         * the fixnum that happens by coincide to be the word-index in
         * memory of the header for some object plus two.  This is
         * happenstance would cause the register containing the fixnum
         * to be selected as the register_pair if the interior pointer
         * is to anywhere after the first two words of the object.
         * The fixnum won't be changed during GC, but the object might
         * move, thus destroying the interior pointer.  --AB,
         * 2010-Jul-14 */

        // Note this can produce weird pairings that seem not to adversely
        // affect anything. For instance if reg_LIP points lower than anything
        // in a boxed register except for let's say register A0 which is
        // currently NIL, then we'll say that LIP was based on A0 + huge offset.
        // NIL doesn't move, so we won't alter LIP. But what if A0 had something
        // random in it and lower than LIP? It will pair with LIP and then
        // adjust LIP to point to garbage.  This is "harmless" because
        // we never actually treat LIP as an exact root.

        if (plausible_base_register(regval, regindex) &&
            ((regval & ~LOWTAG_MASK) <= pointer)) {
            offset = pointer - (regval & ~LOWTAG_MASK);
            if (offset < *saved_offset) {
                *saved_offset = offset;
                *register_pair = regindex;
            }
        }
    }
#if 0
    if (*register_pair >= 0)
        fprintf(stderr, "pair_interior_ptr: %-3s=%p based on %s=%p + %x\n",
                regname, (void*)pointer,
                lisp_register_names[*register_pair],
                (void*)(((uword_t)*os_context_register_addr(context, *register_pair))
                        & ~LOWTAG_MASK),
                (int)*saved_offset);
    else
        fprintf(stderr, "pair_interior_ptr: %-3s=%#lx not based\n", regname, pointer);
#endif
}

static void
scavenge_interrupt_context(os_context_t * context)
{
    unsigned int i;

    /* FIXME: The various #ifdef noise here is precisely that: noise.
     * Is it possible to fold it into the macrology so that we have
     * one set of #ifdefs and then INTERIOR_POINTER_VARS /et alia/
     * compile out for the registers that don't exist on a given
     * platform? */

#ifdef reg_CODE
    INTERIOR_POINTER_VARS(pc);
#endif

#ifdef reg_LIP
    INTERIOR_POINTER_VARS(lip);
#endif
#ifdef ARCH_HAS_LINK_REGISTER
    INTERIOR_POINTER_VARS(lr);
#endif
#ifdef ARCH_HAS_NPC_REGISTER
    INTERIOR_POINTER_VARS(npc);
#endif
#if defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64
    INTERIOR_POINTER_VARS(ctr);
#endif

#ifdef reg_CODE
    PAIR_INTERIOR_POINTER(pc);
#endif

#ifdef reg_LIP
    PAIR_INTERIOR_POINTER(lip);
#endif

#ifdef ARCH_HAS_LINK_REGISTER
#ifndef reg_CODE
    /* If LR has code in it don't pair it with anything else, since
       there's reg_CODE and it may match something bogus. It will be pinned by pin_stack. */
    int code_in_lr = 0;

    if (dynamic_space_code_from_pc((char *)*os_context_register_addr(context, reg_LR))) {
        code_in_lr = 1;
    }
#endif
    {
      PAIR_INTERIOR_POINTER(lr);
    }
#endif

#ifdef ARCH_HAS_NPC_REGISTER
    PAIR_INTERIOR_POINTER(npc);
#endif
#if defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64
    PAIR_INTERIOR_POINTER(ctr);
#endif

    /* Scavenge all boxed registers in the context. */
    for (i = 0; i < (sizeof(boxed_registers) / sizeof(int)); i++) {
        os_context_register_t *boxed_reg;
        lispobj datum;

        /* We can't "just" cast os_context_register_addr() to a
         * pointer to lispobj and pass it to scavenge, because some
         * systems can have a wider register width than we use for
         * lisp objects, and on big-endian systems casting a pointer
         * to a narrower target type doesn't work properly.
         * Therefore, we copy the value out to a temporary lispobj
         * variable, scavenge there, and copy the value back in.
         *
         * FIXME: lispobj is unsigned, os_context_register_t may be
         * signed or unsigned, are we truncating or sign-extending
         * values here that shouldn't be modified?  Possibly affects
         * any architecture that has 32-bit and 64-bit variants where
         * we run in 32-bit mode on 64-bit hardware when the OS is set
         * up for 64-bit from the start.  Or an environment with
         * 32-bit addresses and 64-bit registers. */

        boxed_reg = os_context_register_addr(context, boxed_registers[i]);
        datum = *boxed_reg;
        if (compacting_p()) scavenge(&datum, 1); else gc_mark_obj(datum);
        *boxed_reg = datum;
    }

    /* Now that the scavenging is done, repair the various interior
     * pointers. */
#ifdef reg_CODE
    FIXUP_INTERIOR_POINTER(pc);
#endif

#ifdef reg_LIP
    FIXUP_INTERIOR_POINTER(lip);
#endif
#ifdef ARCH_HAS_LINK_REGISTER

#ifndef reg_CODE
    if(!code_in_lr)
#endif
    {
        FIXUP_INTERIOR_POINTER(lr);
    }
#endif
#ifdef ARCH_HAS_NPC_REGISTER
    FIXUP_INTERIOR_POINTER(npc);
#endif
#if defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64
    FIXUP_INTERIOR_POINTER(ctr);
#endif
}

void
scavenge_interrupt_contexts(struct thread *th)
{
    int i, index;
    os_context_t *context;

    index = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th));

#if defined(DEBUG_PRINT_CONTEXT_INDEX)
    printf("Number of active contexts: %d\n", index);
#endif

    for (i = 0; i < index; i++) {
        context = nth_interrupt_context(i, th);
        scavenge_interrupt_context(context);
    }
}
#endif /* x86oid targets */

/* Our own implementation of heapsort, because some C libraries have a qsort()
 * that calls malloc() apparently, which we MUST NOT do. */

typedef uword_t* heap;

#define swap(a,i,j) { uword_t temp=a[i];a[i]=a[j];a[j]=temp; }
static void sift_down(heap array, int start, int end)
{
     int root = start;
     while (root * 2 + 1 <= end) {
       int child = root * 2 + 1;
       if (child + 1 <= end && array[child] < array[child+1])
           ++child;
       if (array[root] < array[child]) {
           swap(array, root, child);
           root = child;
       } else {
           return;
       }
     }
}

static void heapify(heap array, int length)
{
    int start = (length - 2) / 2;
    while (start >= 0) {
        sift_down(array, start, length-1);
        --start;
    }
}

void gc_heapsort_uwords(heap array, int length)
{
    heapify(array, length);
    int end = length - 1;
    while (end > 0) {
        swap(array, end, 0);
        --end;
        sift_down(array, 0, end);
    }
}

/// External function for calling from Lisp.
uword_t primitive_object_size(lispobj ptr) {
    lispobj* addr = native_pointer(ptr);
    return OBJECT_SIZE(*addr,addr) * N_WORD_BYTES;
}
