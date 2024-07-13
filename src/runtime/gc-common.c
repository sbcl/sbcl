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
#include "genesis/sbcl.h"
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
#include "align.h"
#include "genesis/primitive-objects.h"
#include "genesis/binding.h"
#include "genesis/hash-table.h"
#include "genesis/split-ordered-list.h"
#include "genesis/static-symbols.h"
#include "var-io.h"
#include "search.h"
#include "murmur_hash.h"
#include "incremental-compact.h"

os_vm_size_t dynamic_space_size = DEFAULT_DYNAMIC_SPACE_SIZE;
os_vm_size_t thread_control_stack_size = DEFAULT_CONTROL_STACK_SIZE;

sword_t (*const scavtab[256])(lispobj *where, lispobj object);
uword_t gc_copied_nwords, gc_in_situ_live_nwords;

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
 *   contains FORWARDING_HEADER (0x01), so we need a guard condition
 *   as the last case, to make error detection possible.
 * - With 64-bit words, is_lisp_pointer(object) is false when addr
 *   contains FORWARDING_HEADER, so this function won't get called.
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
    /* In theory we can test forwarding_pointer_p on anything, but it's probably
     * better to avoid reading more memory than needed. Hence the pre-check
     * for a from_space object. But, rather than call from_space_p() which always
     * checks for object pinning, it's a performance boost to treat from_space_p()
     * as a leaky abstraction - reading one word at *native_pointer(object) is easier
     * than looking in a hashset, and 9 times out of 10 times we need to read it anyway.
     * And if the object was already forwarded, we never need pinned_p.
     *
     * Based on some instrumentation added to this function, I determined
     * that it makes sense to read the 'gen' even if find_page_index() returns -1,
     * because approximately 25% of all calls to scav1() *do* find that the object
     * is in from_space. The guard condition on page_index is not needed
     * because it is legal to access page_table at index -1.
     * Therefore, when the object is in from_space, we incur one fewer branch */

    __attribute__((unused)) page_index_t page = find_page_index((void*)object);
#ifndef LISP_FEATURE_MARK_REGION_GC
    if (page_table[page].gen == from_space) {
#else
      /* The incremental compactor only calls scavenge (then scav1) with
       * pointers of the right generation. */
      {
#endif
            if (forwarding_pointer_p(native_pointer(object)))
                *addr = forwarding_pointer_value(native_pointer(object));
            else if (!pinned_p(object, page))
                scav_ptr[PTR_SCAVTAB_INDEX(object)](addr, object);
    }
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    // Test immobile_space_p() only if object was definitely not in dynamic space
    else if (page < 0 && immobile_space_p(object)) {
        lispobj *ptr = base_pointer(object);
        if (immobile_obj_gen_bits(ptr) == from_space)
            enliven_immobile_obj(ptr, 1);
    }
#endif
#if (N_WORD_BITS == 32) && defined(LISP_FEATURE_GENERATIONAL)
    else if (object == FORWARDING_HEADER)
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

static sword_t scav_lose(lispobj *where, lispobj object)
{
    lose("no scavenge function for object %p (widetag %#x)",
         (void*)object, widetag_of(where));

    return 0; /* bogus return value to satisfy static type checking */
}

FILE *gc_activitylog_file;
FILE *gc_activitylog()
{
    char *pathname = "gc-action.log";
    if (!gc_activitylog_file) {
        gc_activitylog_file = fopen(pathname, "w");
        fprintf(stderr, "opened %s\n", pathname);
    }
    return gc_activitylog_file;
}

// Scavenge a block of memory from 'start' to 'end'
// that may contain object headers.
void heap_scavenge(lispobj *start, lispobj *end)
{
    lispobj *object_ptr;

    for (object_ptr = start; object_ptr < end;) {
        lispobj object = *object_ptr;
        if (GC_LOGGING) fprintf(gc_activitylog(), "o %p\n", object_ptr);
        if (other_immediate_lowtag_p(object)) {
#ifdef GC_DEBUG
            /* This check for scav_lose() isn't strictly necessary,
             * but a failure here is often clearer than ending up in
             * scav_lose without knowing the [start,end] */
            if (scavtab[header_widetag(object)] == scav_lose) lose("Losing @ %p", object_ptr);
#endif
            /* It's some sort of header object or another. */
            object_ptr += (scavtab[header_widetag(object)])(object_ptr, object);
        } else {  // it's a cons
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
            // Advancing by the filler payload count (= total - 1) causes 'where' to
            // align exactly to the next object after the loop steps by 1 as usual.
            if (header_widetag(ptr) == FILLER_WIDETAG)
                where += filler_total_nwords(ptr)-1;
            continue;
        }
        // Dear lord, I hate the numbering scheme with SCRATCH_GENERATION higher than everything
        if (pointee_gen < gen || pointee_gen == (1+PSEUDO_STATIC_GENERATION)) dirty = 1;
    }
    return dirty;
}

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
    gc_dcheck(!pin_all_dynamic_space_code);
    /* if object has already been transported, just return pointer */
    if (forwarding_pointer_p((lispobj *)code)) {
        return (struct code *)native_pointer(forwarding_pointer_value((lispobj*)code));
    }

    gc_dcheck(widetag_of(&code->header) == CODE_HEADER_WIDETAG);

    /* prepare to transport the code vector */
    long nwords = code_total_nwords(code);
#ifndef LISP_FEATURE_64_BIT
    /* 32-bit can allocate large code to ordinary (small object) pages
     * because it uses code regions larger than LARGE_OBJECT_SIZE.
     * This makes large code accidentally fit into the region.
     * We can correct that problem here by closing the code region.
     * This is simpler than (or more abstract than) calling gc_alloc_large()
     * because whatever post-copying actions gc_copy_object() performs,
     * we still want, such as the NOTE_TRANSPORTING macro invocation */
    long nbytes = nwords << WORD_SHIFT;
    if (nbytes >= LARGE_OBJECT_SIZE && !page_single_obj_p(find_page_index(code)))
        ensure_region_closed(code_region, PAGE_TYPE_CODE);
#endif
    lispobj l_code = make_lispobj(code, OTHER_POINTER_LOWTAG);
    lispobj l_new_code
        = copy_potential_large_object(l_code, nwords, code_region, PAGE_TYPE_CODE);

    if (l_new_code == l_code) return code;

    set_forwarding_pointer((lispobj *)code, l_new_code);

    struct code *new_code = (struct code *) native_pointer(l_new_code);
    sword_t displacement = l_new_code - l_code;

#if defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64 || \
    defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64 || defined LISP_FEATURE_ARM64
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
    os_flush_icache(code_text_start(new_code), code_text_size(new_code));
    return new_code;
}

#ifdef LISP_FEATURE_64_BIT
# define layout_flags(x) x->sw_flags
#else
# define layout_flags(x) x->uw_flags
#endif

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
        int page_type = PAGE_TYPE_SMALL_MIXED;
        void* region = small_mixed_region;
        if (widetag == FUNCALLABLE_INSTANCE_WIDETAG) {
            /* funcallable-instance might have all descriptor slots
             * except for the trampoline, which points to an asm routine.
             * This is not true for self-contained trampoline GFs though. */
#ifdef LISP_FEATURE_EXECUTABLE_FUNINSTANCES
            page_type = PAGE_TYPE_CODE, region = code_region;
#else
            struct layout* layout = (void*)native_pointer(funinstance_layout(FUNCTION(object)));
            if (layout && (layout_flags(layout) & STRICTLY_BOXED_FLAG))
                page_type = PAGE_TYPE_BOXED, region = boxed_region;
#endif
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
        gc_assert(copy != object);
#ifdef LISP_FEATURE_EXECUTABLE_FUNINSTANCES
        if (widetag == FUNCALLABLE_INSTANCE_WIDETAG) {
            struct funcallable_instance* old = (void*)native_pointer(object);
            struct funcallable_instance* new = (void*)native_pointer(copy);
            if (old->trampoline == (lispobj)&old->instword1)
                new->trampoline = (lispobj)&new->instword1;
        }
#endif
        set_forwarding_pointer(fun, copy);
    }
    if (copy != object) *where = copy;
    CHECK_COPY_POSTCONDITIONS(copy, FUN_POINTER_LOWTAG);
    return 1;
}

static lispobj trans_code_blob(lispobj object)
{
    return make_lispobj(trans_code((struct code *)native_pointer(object)),
                        OTHER_POINTER_LOWTAG);
}

static sword_t size_code_blob(lispobj *where)
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

    void* region = small_mixed_region;
    int page_type = PAGE_TYPE_SMALL_MIXED;

    struct layout* layout = (void*)native_pointer(instance_layout(INSTANCE(object)));
    struct bitmap bitmap;
    const int words_per_card = GENCGC_CARD_BYTES>>WORD_SHIFT;
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
        else if (original_length < words_per_card &&
                 (layout_flags(layout) & STRICTLY_BOXED_FLAG))
            page_type = PAGE_TYPE_BOXED, region = boxed_region;
    }

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
        int old_nwords = 1 + (original_length|1);
        int new_length = original_length + (original_length & 1);
        copy = gc_copy_object_resizing(object, 1 + (new_length|1),
                                       region, page_type, old_nwords);
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
        set_forwarding_pointer_resized(native_pointer(object), copy, old_nwords);
    } else {
        int nwords = 1 + (original_length|1);
        copy = gc_copy_object(object, nwords, region, page_type);
        set_forwarding_pointer(native_pointer(object), copy);
    }
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
            while (instancep(object = ((struct list_node*)node)->_node_next)
                   && from_space_p(object)
                   && !forwarding_pointer_p(native_pointer(object))) {
                copy = copy_instance(object);
                ((struct list_node*)node)->_node_next = copy;
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
    // That decision is made in copy_potential_large_object().
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

static sword_t size_filler(lispobj *where) {  return filler_total_nwords(*where); }
static sword_t scav_filler(__attribute__((unused)) lispobj *where, lispobj object) {
    return filler_total_nwords(object);
}

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

#ifdef LISP_FEATURE_PPC64
/* Dead conses are clobbered to contain all 1s. The first word of such a cons
 * does NOT look like a valid cons half due to unusual lowtag arrangement */
static sword_t scav_consfiller(lispobj *where, lispobj object)
{
    gc_assert(object == (uword_t)-1);
    gc_assert(where[1] == (uword_t)-1);
    return 2;
}
static sword_t size_consfiller(lispobj *where)
{
    gc_assert(where[0] == (uword_t)-1 && where[1] == (uword_t)-1);
    return 2;
}
#endif

//// General boxed object scav/trans/size functions

#define DEF_SCAV_BOXED(suffix, sizer) \
  static sword_t __attribute__((unused)) \
  scav_##suffix(lispobj *where, lispobj header) { \
      return 1 + scavenge(where+1, sizer(header)); \
  } \
  static sword_t size_##suffix(lispobj *where) { return 1 + sizer(*where); }

DEF_SCAV_BOXED(boxed, BOXED_NWORDS)
DEF_SCAV_BOXED(short_boxed, SHORT_BOXED_NWORDS)

static lispobj trans_boxed(lispobj object) {
    return gc_copy_object(object, 1 + BOXED_NWORDS(*native_pointer(object)),
                          boxed_region, PAGE_TYPE_BOXED);
}

/* Symbol */

#ifdef LISP_FEATURE_LINKAGE_SPACE
static void scav_linkage_cell(int linkage_index)
{
    if (!linkage_index) return;
    lispobj entrypoint = linkage_space[linkage_index];
    if (!entrypoint) return;
    lispobj taggedptr = fun_taggedptr_from_self(entrypoint);
    lispobj new = taggedptr;
    scav1(&new, new);
    if (new != taggedptr) linkage_space[linkage_index] = new + (entrypoint - taggedptr);
}
#endif
void scav_code_linkage_cells(__attribute__((unused)) struct code* c)
{
#ifdef LISP_FEATURE_LINKAGE_SPACE
    const unsigned int smallvec_elts =
        (GENCGC_PAGE_BYTES - offsetof(struct vector,data)) / N_WORD_BYTES;
    lispobj integer = barrier_load(&c->fixups);
    if (!integer) return; // do no work for leaf codeblobs
    lispobj name_map = barrier_load(&SYMBOL(LINKAGE_NAME_MAP)->value);
    gc_assert(simple_vector_p(name_map));
    struct vector* outer_vector = VECTOR(name_map);
    struct varint_unpacker unpacker;
    varint_unpacker_init(&unpacker, integer);
    int prev_index = 0, index;
    while (varint_unpack(&unpacker, &index) && index != 0) {
        index += prev_index;
        prev_index = index;
        int index_high = (unsigned int)index / smallvec_elts;
        int index_low = (unsigned int)index % smallvec_elts;
        lispobj smallvec = barrier_load(&outer_vector->data[index_high]);
        gc_assert(other_pointer_p(smallvec));
        struct vector* inner_vector = VECTOR(smallvec);
        // Ensure liveness of function name
        scavenge(&inner_vector->data[index_low], 1);
    }
#endif
}

static sword_t scav_symbol(lispobj *where,
                           __attribute__((unused)) lispobj header) {
    struct symbol* s = (void*)where;
#ifdef LISP_FEATURE_64_BIT
# ifdef LISP_FEATURE_LINKAGE_SPACE
    // Skip the hash slot, it isn't a tagged descriptor
    scavenge(&s->value, 3); // value, fdefn, info
    scav_linkage_cell(symbol_linkage_index(s));
# else
    /* The first 4 slots of a symbol are all boxed words, but vary in meaning
     * based on #+relocatable-static-space. Scanning the hash is harmless - though
     * unnecessary - at present, since it is of descriptor nature, be it fixnum,
     * or NIL in the case of NIL. Though there is little or no benefit to gaining
     * 1 bit, we could make hash a raw slot in which case we'd have to use care
     * to avoid reading it. trace-object.inc uses three separate operations */
    scavenge(where + 1, 4);
# endif
    lispobj name = decode_symbol_name(s->name);
    lispobj new = name;
    scavenge(&new, 1);
    if (new != name) set_symbol_name(s, new);
#else
    scavenge(&s->fdefn, 4); // fdefn, value, info, name
#endif
    return ALIGN_UP(SYMBOL_SIZE, 2);
}
static lispobj trans_symbol(lispobj object) {
    return gc_copy_object(object, ALIGN_UP(SYMBOL_SIZE,2),
                          small_mixed_region, PAGE_TYPE_SMALL_MIXED);
}
static sword_t size_symbol(lispobj __attribute__((unused)) *where) {
    return ALIGN_UP(SYMBOL_SIZE,2);
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
    lispobj old = layoutptr;
    scav1(&layoutptr, layoutptr);
    if (layoutptr != old) instance_layout(where) = layoutptr;
    struct layout *layout = LAYOUT(layoutptr);
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
        struct list_node* node = (struct list_node*)where;
        lispobj next = node->_node_next;
        if (fixnump(next) && next) { // ignore initially 0 heap words
            lispobj descriptor = next | INSTANCE_POINTER_LOWTAG;
            scav1(&descriptor, descriptor);
            // Fix the pointer but of course leave it in mid-deletion (untagged) state.
            if (descriptor != (next | INSTANCE_POINTER_LOWTAG))
                node->_node_next = descriptor & ~LOWTAG_MASK;
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
    // This handles compact or non-compact layouts with indifference.
    lispobj old = layoutptr;
    scav1(&layoutptr, layoutptr);
    if (layoutptr != old) funinstance_layout(where) = layoutptr;
    struct funcallable_instance* fin = (void*)where;
    lispobj* firstword = &fin->function;
    // This will visit the layout again if it appears as an ordinary descriptor slot.
    // It's not a problem for gencgc to process a slot twice.
    lispobj* lastword = where + (nslots+1); // exclusive upper bound
    scavenge(firstword, lastword - firstword);
    return 1 + (nslots | 1);
}

/* If assertions are enabled, the number of words taken up is double
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
    return copy_potential_large_object(object, bignum_nwords(*native_pointer(object)),
                                      unboxed_region, PAGE_TYPE_UNBOXED);
}

#ifndef LISP_FEATURE_X86_64
lispobj decode_fdefn_rawfun(struct fdefn* fdefn) {
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
#ifdef LISP_FEATURE_LINKAGE_SPACE
    scavenge(where + 1, 3); // name, padding, function
    scav_linkage_cell(fdefn_linkage_index(fdefn));
#else
    scavenge(where + 1, 2); // 'name' and 'fun'
    lispobj obj = decode_fdefn_rawfun(fdefn);
    lispobj new = obj;
    scavenge(&new, 1);
    if (new != obj) fdefn->raw_addr += (sword_t)(new - obj);
#endif
    // Payload length is not computed from the header
    return FDEFN_SIZE;
}
static lispobj trans_fdefn(lispobj object) {
    return gc_copy_object(object, FDEFN_SIZE, small_mixed_region, PAGE_TYPE_SMALL_MIXED);
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
        page_type = PAGE_TYPE_SMALL_MIXED, region = small_mixed_region;
    return copy_potential_large_object(object, ALIGN_UP(length + 2, 2), region, page_type);
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
#define SPECIALIZED_VECTOR_ARGS small_mixed_region, PAGE_TYPE_SMALL_MIXED
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
    return copy_potential_large_object(object, ALIGN_UP(nwords + 2, 2), \
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

/* Weak-pointer has two variants. If the header data indicate 0 payload words,
 * then it's a vector of lispobj with a widetag outside the the range of vector widetags.
 * Otherwise, it contains exactly 1 referent.
 * We might also wish to support a third variant which would implement an ephemeron
 * (https://en.wikipedia.org/wiki/Ephemeron)
 * that otherwise can only be simulated very inefficiently in SBCL as a weak hash-table
 * containing a single key */
static sword_t scav_weakptr(lispobj *where, lispobj __attribute__((unused)) object)
{
    if (weakptr_vectorp((struct weak_pointer*)where)) {
        add_to_weak_vector_list(where, *where); // treat it like weak simple-vector
        return size_vector_t(where);
    }
    struct weak_pointer * wp = (struct weak_pointer*)where;
    /* If wp->next is non-NULL then it's already in the weak pointer chain.
     * If it is, then even if wp->value is now known to be live,
     * we can't fix (or don't need to fix) the slot, because removing
     * from a singly-linked-list is an O(n) operation */
    if (!in_weak_pointer_list(wp)) {
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
            if (forwarding_pointer_p(native_pointer(pointee)))
                wp->value = forwarding_pointer_value(native_pointer(pointee));
            else
                add_to_weak_pointer_chain(wp);
        }
    }
    return ALIGN_UP(WEAK_POINTER_SIZE, 2);
}
static lispobj trans_weakptr(lispobj object) {
    lispobj* where = (lispobj*)(object - OTHER_POINTER_LOWTAG);
    if (weakptr_vectorp((struct weak_pointer*)where)) // See trans_vector_t
        return copy_potential_large_object(object,
                                           size_vector_t(where),
                                           small_mixed_region, PAGE_TYPE_SMALL_MIXED);
    else
        return gc_copy_object(object,
                              ALIGN_UP(WEAK_POINTER_SIZE, 2),
                              small_mixed_region, PAGE_TYPE_SMALL_MIXED);
}
static sword_t size_weakptr(lispobj *where) {
    return weakptr_vectorp((struct weak_pointer*)where) ? size_vector_t(where)
      : ALIGN_UP(WEAK_POINTER_SIZE, 2);
}

void smash_weak_pointers(void)
{
    struct weak_pointer *wp, *next_wp;
    for (wp = weak_pointer_chain; wp != WEAK_POINTER_CHAIN_END; wp = next_wp) {
        gc_assert(widetag_of(&wp->header) == WEAK_POINTER_WIDETAG);
        next_wp = get_weak_pointer_next(wp);
        reset_weak_pointer_next(wp);

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
        // Large objects are "moved" by touching the page table gen field.
        // Do nothing if the target of this weak pointer had that happen.
        else if (new_space_p(val)) { }
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

#define HT_ENTRY_LIVENESS_FUN_ARRAY_NAME weak_ht_alivep_funs
static inline bool pointer_survived_gc_yet(lispobj obj) {
    return taggedptr_alivep_impl(obj);
}
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
bool test_weak_triggers(bool (*predicate)(lispobj), void (*mark)(lispobj))
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
#else
pthread_mutex_t finalizer_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t finalizer_condvar = PTHREAD_COND_INITIALIZER;
#endif
void finalizer_thread_wait () {
    ignore_value(mutex_acquire(&finalizer_mutex));
    /* Sleep only if we should be running but there is no post-GC hook to run.
     * Finalizers per se do not have an assurance of quick execution. Namely,
     * there is a race between deciding to wait and testing whether a finalizer
     * should run. That's OK, though in fact it would be fairly simple to
     * examine FINALIZERS_TRIGGERED as part of the condition here */
    if (finalizer_thread_runflag && SYMBOL(RUN_GC_HOOKS)->value == 0)
        CONDITION_VAR_WAIT(&finalizer_condvar, &finalizer_mutex);
    ignore_value(mutex_release(&finalizer_mutex));
}
void finalizer_thread_wake (int run_hooks) {
    if (run_hooks) {
        ignore_value(mutex_acquire(&finalizer_mutex));
        // This has to be atomic because the finalizer thread doesn't acquire
        // the mutex when decrementing
        __sync_add_and_fetch(&SYMBOL(RUN_GC_HOOKS)->value, make_fixnum(1));
        CONDITION_VAR_WAKE_ALL(&finalizer_condvar);
        ignore_value(mutex_release(&finalizer_mutex));
    } else { // just poke the finalizer thread and hope it runs something
        CONDITION_VAR_WAKE_ALL(&finalizer_condvar);
    }
}
void finalizer_thread_stop () {
    ignore_value(mutex_acquire(&finalizer_mutex));
    finalizer_thread_runflag = 0;
    CONDITION_VAR_WAKE_ALL(&finalizer_condvar);
    ignore_value(mutex_release(&finalizer_mutex));
}
#endif

#ifdef TRACE_MMAP_SYSCALLS
FILE* mmgr_debug_logfile;
void set_page_type_impl(struct page* pte, int newval)
{
    if (newval != pte->type) /* too "noisy" without this pre-test */
        fprintf(mmgr_debug_logfile, "pg @ %p : was %x is %x\n",
                page_address(pte-page_table), pte->type, newval);
    pte->type = newval;
}
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

static inline bool stable_eql_hash_p(lispobj obj)
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


/* The standard pointer tagging scheme admits an optimization that cuts the number
 * of instructions down when testing for either 'a' or 'b' (or both) being a tagged
 * pointer, because the bitwise OR of two pointers is considered a pointer.
 * This trick is inadmissible for the PPC64 lowtag arrangement.
 *
 * NOTE: this is allowed to return a false positive. e.g. take the fixnum 1
 * (internally #b10) and single-float-widetag = 0x19. (Assume 64-bit words)
 * The bitwise OR is (#b11001 | #b10) = #b11011 which looks like a pointer */
#ifdef LISP_FEATURE_PPC64
#define at_least_one_pointer_p(a,b) (is_lisp_pointer(a) || is_lisp_pointer(b))
#else
#define at_least_one_pointer_p(a,b) (is_lisp_pointer(a|b))
#endif

/* Scavenge the "real" entries in the hash-table kv vector. The vector element
 * at index 0 bounds the scan. The element at length-1 (the hash table itself)
 * was scavenged already.
 *
 * We can disregard any entry in which both key and value are immediates.
 * This effectively ignores empty pairs, as well as makes fixnum -> fixnum table
 * more efficient.
 */
#define SCAV_ENTRIES(entry_alivep, defer)                                      \
    bool __attribute__((unused)) any_deferred = 0;                             \
    bool rehash = 0;                                                           \
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
    bool eql_hashing = 0; // whether this table is an EQL table
    if (instancep(kv_supplement)) {
        struct hash_table* ht = (struct hash_table*)native_pointer(kv_supplement);
        eql_hashing = hashtable_kind(ht) == HASHTABLE_KIND_EQL;
        kv_supplement = ht->hash_vector;
    } else if (kv_supplement == LISP_T) { // EQL hashing on a non-weak table
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

bool scan_weak_hashtable(struct hash_table *hash_table,
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
    bool eql_hashing = hashtable_kind(hash_table) == HASHTABLE_KIND_EQL;
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

    /* Scavenge element (length-1), which may be a hash-table structure
     * or a vector of hashes, depending on the table kind/weakness */
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

    if ((lispobj)hash_table->next_weak_hash_table == NIL) {
        int weakness = hashtable_weakness(hash_table);
        bool defer = 1;
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
            NON_FAULTING_STORE(hash_table->next_weak_hash_table = weak_hash_tables,
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
static inline bool
cull_weak_hash_table_bucket(struct hash_table *hash_table,
                            uint32_t bucket, uint32_t index,
                            lispobj *kv_vector,
                            uint32_t *next_vector, uint32_t *hash_vector,
                            int (*alivep_test)(lispobj,lispobj),
                            void (*fix_pointers)(lispobj[2]),
                            bool rehash)
{
    const lispobj empty_symbol = UNBOUND_MARKER_WIDETAG;
    int eql_hashing = hashtable_kind(hash_table) == HASHTABLE_KIND_EQL;
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
            if (debug_weak_ht)
                fprintf(stderr, "<%"OBJ_FMTX",%"OBJ_FMTX"> is dead\n", key, value);
            gc_assert(hash_table->_count > 0);
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
#ifdef LISP_FEATURE_MARK_REGION_GC
                mr_preserve_leaf(cons->car);
#else
                if (!compacting_p()) gc_mark_obj(cons->car);
#endif
            } else { // small values
                cons = (struct cons*)
                  gc_general_alloc(cons_region, sizeof(struct cons), PAGE_TYPE_CONS);
                cons->car = ((index << 14) | bucket) << N_FIXNUM_TAG_BITS;
            }
            cons->cdr = hash_table->smashed_cells;
#ifdef LISP_FEATURE_MARK_REGION_GC
            /* We just created a pointer that the incremental compactor
             * doesn't know about yet, so maybe log it. */
            log_slot(cons->cdr, &cons->cdr, (lispobj*)cons, SOURCE_NORMAL);
#endif
            // Lisp code must atomically pop the list whereas this C code
            // always wins and does not need compare-and-swap.
            notice_pointer_store(hash_table, &hash_table->smashed_cells);
            hash_table->smashed_cells = make_lispobj(cons, LIST_POINTER_LOWTAG);
            // ensure this cons doesn't get smashed into (0 . 0) by full gc
#ifdef LISP_FEATURE_MARK_REGION_GC
            mr_preserve_leaf(hash_table->smashed_cells);
#else
            if (!compacting_p()) gc_mark_obj(hash_table->smashed_cells);
#endif

        } else {
            if (debug_weak_ht)
                fprintf(stderr, "<%"OBJ_FMTX",%"OBJ_FMTX"> is alive\n", key, value);
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

    bool rehash = 0;
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
                                        rehash))
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
        next = table->next_weak_hash_table;
        NON_FAULTING_STORE(table->next_weak_hash_table = (void*)NIL,
                           &table->next_weak_hash_table);
        int weakness = hashtable_weakness(table);
        gc_assert((weakness & ~3) == 0);
        if (debug_weak_ht)
            fprintf(stderr, "Culling %p with weakness %d\n", table, weakness);
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
    // Close the region used when pushing into hash_table->smashed_cells
    ensure_region_closed(cons_region, PAGE_TYPE_CONS);
}


/*
 * initialization
 */

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

sword_t scav_code_blob(lispobj *object, lispobj header);
#define WANT_SCAV_TRANS_SIZE_TABLES
#include "genesis/gc-tables.h"

/* Find the code object for the given pc, or return NULL on
   failure. */
lispobj *
component_ptr_from_pc(char *pc)
{
    /* This will safely look in one or both codeblob trees and/or the
     * sorted array of immobile text pages. Failing those, it'll perform
     * the usual linear scan of generation 1 and up pages. In any case
     * it should be perfectly threadsafe because the trees are made of immutable
     * nodes, and linear scan only operates on pages that can't be
     * concurrently manipulated */
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
gc_search_space3(void *pointer, lispobj * const start, void *limit)
{
    if (pointer < (void*)start || pointer >= limit) return NULL;

    size_t count;
    lispobj* where = start;

#if SEARCH_SPACE_FOLLOWS_FORWARDING_POINTERS
    /* CAUTION: this code is _significantly_ slower than the production version
       due to the extra checks for forwarding.
       Also it is BROKEN. DO NOT ENABLE THE #define UNLESS/UNTIL FIXED. */
    for ( ; (void*)where < limit ; where += count) {
        lispobj *copy = where;
        if (forwarding_pointer_p(where))
            copy = native_pointer(forwarding_pointer_value(where));
        // BUG: the size of a forwarded object may exceed the size of the original
        // due to the addition of a stable hash slot.
        count = object_size(copy);
        /* Check whether the pointer is within this object. */
        if (pointer < (void*)(where+count)) return where;
    }
#else
    where = next_object(where, 0, limit);
    while (where) {
        count = object_size(where);
        /* Check whether the pointer is within this object. */
        if (pointer < (void*)(where+count)) return where;
        where = next_object(where, count, limit);
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

/* Helper for valid_tagged_pointer_p (below) and
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
            lispobj* potential_fun = (void*)FUNCTION(pointer);
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

/* Used by the debugger to validate possibly bogus pointers before
 * calling MAKE-LISP-OBJ on them.
 *
 * If the debugger constructs places a reference to a non-object
 * into a boxed register, things could end very badly.
 */
int
valid_tagged_pointer_p(lispobj pointer)
{
    /* We don't have a general way to ask a specific GC implementation
     * whether 'pointer' is definitely the tagged pointer to an object -
     * all we have is "search for a containing object" and then a decision
     * whether pointer is the tagged pointer to that.
     * But searching is actually too complex an operation for some easy
     * cases when we could answer the question more simply, e.g. if the alleged
     * pointer can not possibly be valid because of widetag/lowtag mismatch.
     * I think it would be legal to start with that test here, because this
     * function does not accept interior pointers. (Simple-fun pointers are
     * interior to the containing code, but are properly tagged pointers
     * to the base of the function to which they point) */
    page_index_t page = find_page_index((void*)pointer);
    if (page >= 0 &&
        (page_table[page].type & PAGE_TYPE_MASK) == PAGE_TYPE_BOXED) {
        int wordindex = (pointer & (GENCGC_PAGE_BYTES-1)) >> WORD_SHIFT;
        return (wordindex < page_table[page].words_used_)
            /* strictly boxed pages can only contain headers and immediates */
            && is_header(*native_pointer(pointer))
            && make_lispobj(native_pointer(pointer),
                            LOWTAG_FOR_WIDETAG(*native_pointer(pointer) & WIDETAG_MASK))
               == pointer;
    }
    lispobj *start = search_all_gc_spaces((void*)pointer);
    if (start != NULL)
        return properly_tagged_descriptor_p((void*)pointer, start);
    return 0;
}

static bool can_invoke_post_gc(__attribute__((unused)) struct thread* th,
                                  sigset_t *context_sigmask)
{
#ifdef LISP_FEATURE_SB_THREAD
    /* TODO: with #+sb-thread, running post-GC actions is as simple as bumping the
     * value in the static symbol *RUN-GC-HOOKS* and waking the finalizer thread,
     * which is done in a C function. Therefore all this complicated logic around whether
     * Lisp can/should execute user code is for nothing- the finalizer is always alive,
     * and either executing a thunk of user code, or idle */
    lispobj obj = th->lisp_thread;
    /* Ok, I seriously doubt that this can happen now. Don't we create
     * the 'struct thread' with a pointer to its SB-THREAD:THREAD right away?
     * I thought so. But if I'm mistaken, give up. */
    if (!obj) return 0;
    struct thread_instance* lispthread = (void*)(obj - INSTANCE_POINTER_LOWTAG);

    /* If the SB-THREAD:THREAD has a 0 for its 'struct thread', give up.
     * This is the same as the THREAD-ALIVE-P test.  Maybe a thread that is
     * in the process of un-setting that slot performed this GC. */
    if (!lispthread->uw_primitive_thread) return 0;

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

bool maybe_gc(os_context_t *context)
{
    lispobj gc_happened;
    __attribute__((unused)) struct thread *thread = get_sb_vm_thread();
    bool were_in_lisp = !foreign_function_call_active_p(thread);

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
    unblock_gc_stop_signal();
#endif
    /* FIXME: Nothing must go wrong during GC else we end up running
     * the debugger, error handlers, and user code in general in a
     * potentially unsafe place. Running out of the control stack or
     * the heap in SUB-GC are ways to lose. Of course, deferrables
     * cannot be unblocked because there may be a pending handler, or
     * we may even be in a WITHOUT-INTERRUPTS. */
    gc_happened = funcall1(StaticSymbolFunction(SUB_GC), 0);
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
    if ((gc_happened == LISP_T) &&
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
            funcall0(StaticSymbolFunction(POST_GC));
#ifndef LISP_FEATURE_WIN32
        } else {
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
#ifndef LISP_FEATURE_MARK_REGION_GC
    if (!compacting_p()) {
        long nwords = (lispobj*)access_control_stack_pointer(th) - th->control_stack_start;
        gc_mark_range(th->control_stack_start, nwords);
        return;
    }
#endif
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

    for (object_ptr = th->control_stack_start;
         object_ptr < access_control_stack_pointer(th);
         object_ptr++) {
        lispobj word = *object_ptr;
        if (word == FORWARDING_HEADER)
            lose("unexpected forwarding pointer in scavenge_control_stack: %p, start=%p, end=%p",
                 object_ptr, th->control_stack_start, access_control_stack_pointer(th));
        else if (is_lisp_pointer(word)) {
#ifdef LISP_FEATURE_MARK_REGION_GC
          mr_preserve_object(word);
#else
          scav1(object_ptr, word);
#endif
        }
#ifdef LISP_FEATURE_PPC64
        /* For ppc64, ~0 does not satisfy is_lisp_pointer() or is_lisp_immediate(),
         * but it can be ignored. It nominally satisfies is_lisp_pointer() on other
         * architectures, and gets ignored because it does not point to the heap. */
        else if (is_lisp_immediate(word) || word == ~(uword_t)0) { } // ignore
#else
        else if (is_lisp_immediate(word)) { } // ignore
#endif
        else if (scavtab[header_widetag(word)] == scav_lose) {
            lose("unboxed object in scavenge_control_stack: %p->%"OBJ_FMTX", start=%p, end=%p",
                 object_ptr, word, th->control_stack_start, access_control_stack_pointer(th));
        }
    }
}

#ifdef reg_CODE
/* Scavenging Interrupt Contexts */

static int boxed_registers[] = BOXED_REGISTERS;

// Nothing uses os_context_pc_addr any more, except ACCESS_INTERIOR_POINTER_pc.
// I didn't see a good way to remove that one.
extern os_context_register_t* os_context_pc_addr(os_context_t*);

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

#ifdef reg_LRA
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

    /* Platforms without LRA pin on-stack code. Furthermore, the PC
       must not be paired, as even on platforms with $CODE, there is
       nothing valid to pair PC with immediately upon function
       return. */
#ifdef reg_LRA
    PAIR_INTERIOR_POINTER(pc);
#endif

#ifdef reg_LIP
    PAIR_INTERIOR_POINTER(lip);
#endif

#ifdef ARCH_HAS_LINK_REGISTER
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
#ifdef reg_LRA
    FIXUP_INTERIOR_POINTER(pc);
#endif

#ifdef reg_LIP
    FIXUP_INTERIOR_POINTER(lip);
#endif
#ifdef ARCH_HAS_LINK_REGISTER

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
#endif /* !REG_CODE */
#endif /* x86oid targets */

/* Finalizer table based on Split-Ordered Lists */
static void push_in_ordinary_list(struct symbol* list_holder, lispobj element)
{
    struct cons* cons = gc_general_alloc(cons_region, 2*N_WORD_BYTES, PAGE_TYPE_CONS);
    cons->car = element;
    lispobj old = list_holder->value;
    cons->cdr = old;
    lispobj new = make_lispobj(cons, LIST_POINTER_LOWTAG);
    lispobj __attribute__((unused)) actual =
        __sync_val_compare_and_swap(&list_holder->value, old, new);
    gc_assert(actual == old);
#ifdef LISP_FEATURE_MARK_REGION_GC
    set_allocation_bit_mark(cons);
#else
    if (!compacting_p()) gc_mark_obj(new);
#endif
}
static void push_in_alist(struct symbol* list_holder, lispobj key, lispobj val)
{
    struct cons* cons = gc_general_alloc(cons_region, 2*N_WORD_BYTES, PAGE_TYPE_CONS);
    cons->car = key;
    cons->cdr = val;
    lispobj pair = make_lispobj(cons, LIST_POINTER_LOWTAG);
#ifdef LISP_FEATURE_MARK_REGION_GC
    set_allocation_bit_mark(cons);
#else
    if (!compacting_p()) gc_mark_obj(pair);
#endif
    push_in_ordinary_list(list_holder, pair);
}

/* Scan the finalizer table and take action on each node as follows:
 * - nodes already marked for deletion, and dummy nodes, are ignored
 * - transported keys are moved to the "rehash" list and then logically deleted
 * - dead keys are moved to the "triggered" list and then logically deleted
 */
static inline bool obj_alivep(lispobj* obj_base) {
    extern bool fullcgc_lispobj_livep(lispobj);
    lispobj obj = compute_lispobj(obj_base);
#ifdef LISP_FEATURE_MARK_REGION_GC
    /* We manipulate allocation bits, as we handle finalizers after
     * sweeping, so that compaction can run before finalizers are
     * scanned. Compaction needs to run before as scan_finalizers
     * needs to rehash when forwarding pointers are encountered. */
    return allocation_bit_marked(native_pointer(obj));
#else
    return compacting_p() ? pointer_survived_gc_yet(obj) : fullcgc_lispobj_livep(obj);
#endif
}

void scan_finalizers()
{
#ifndef LISP_FEATURE_WEAK_VECTOR_READBARRIER
    lispobj finalizer_store = SYMBOL(FINALIZER_STORE)->value;
    gc_assert(lowtag_of(finalizer_store) == INSTANCE_POINTER_LOWTAG);
    struct split_ordered_list* solist = (void*)native_pointer(finalizer_store);
    // SO-HEAD can not possibly be marked for deletion, therefore %NODE-NEXT
    // returns a valid node.
    lispobj node = ((struct solist_node*)native_pointer(solist->head))->_node_next;
    while (node != LFLIST_TAIL_ATOM) {
        // At each iteration, 'this' is the node whose disposition we're pondering,
        struct solist_node* this = (void*)INSTANCE(node);
        // To determine if 'this' is pending deletion, read the bits of its 'next'
        lispobj next = this->_node_next;
        if (fixnump(next)) { // node is already logically deleted, pending physical deletion
            gc_assert(!so_dummy_node_p(this));
            node = next | INSTANCE_POINTER_LOWTAG;
            continue;
        }
        if (so_dummy_node_p(this)) {
            // nothing to do
        } else if (forwarding_pointer_p((lispobj*)this->so_key)) {
            // live object moved
            push_in_alist(SYMBOL(FINALIZER_REHASHLIST),
                          forwarding_pointer_value((lispobj*)this->so_key),
                          this->so_data);
            // FIXME: use sync_fetch_and_and or does it not matter since world is stopped?
            this->_node_next = next & ~LOWTAG_MASK; // logically delete
            // 'key' is left alone but is no longer considered a reference because the
            // node is logically deleted. This preserves a partial order if there are
            // hash collisions as key is the tiebreaker.
            --solist->uw_count;
        } else if (!obj_alivep((lispobj*)this->so_key)) {
            push_in_ordinary_list(SYMBOL(FINALIZERS_TRIGGERED), this->so_data);
            this->_node_next = next & ~LOWTAG_MASK; // logically delete
            --solist->uw_count;
        }
        node = next;
    }
    // Close the region
    ensure_region_closed(cons_region, PAGE_TYPE_CONS);
#endif
}

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
    return object_size(addr) * N_WORD_BYTES;
}

#ifdef LISP_FEATURE_GENERATIONAL
/* We hunt for pointers to old-space, when GCing generations >= verify_gen.
 * Set verify_gens to HIGHEST_NORMAL_GENERATION + 2 to disable this kind of
 * check. */
generation_index_t verify_gens = HIGHEST_NORMAL_GENERATION + 2;

/* Should we do a pre-scan of the heap before it's GCed? */
int pre_verify_gen_0 = 0;

int hexdump_enabled = 0;
static int hexdump_counter;
#define HEXDUMP_PATH_TEMPLATE "/var/tmp/heap-%d-%d.txt"

char *page_card_mark_string(page_index_t page, char *result)
{
    long card = addr_to_card_index(page_address(page));
    if (cardseq_all_marked_nonsticky(card))
        result[0] = '*', result[1] = 0;
    else if (!cardseq_any_marked(card))
        result[0] = '-', result[1] = 0;
    else {
        int i;
        for(i=0; i<CARDS_PER_PAGE; ++i)
        switch (gc_card_mark[card+i] & MARK_BYTE_MASK) {
        case CARD_MARKED: result[i] = '*'; break;
        case CARD_UNMARKED: result[i] = '-'; break;
#ifdef LISP_FEATURE_SOFT_CARD_MARKS
        case STICKY_MARK: result[i] = 'S'; break;
#else
        case WP_CLEARED_AND_MARKED: result[i] = 'd'; break; // "d" is for dirty
#endif
        default: result[i] = '?'; break; // illegal value
        }
        result[CARDS_PER_PAGE] = 0;
    }
    return result;
}

static int dump_completely_p(lispobj* obj, struct verify_state* state)
{
    int i;
    if (!state) {
        page_index_t pg = find_page_index(obj);
        if (pg >= 10470 && pg <= 10485) return 1; // (as an example)
        return 0;
    }
    for (i=0; i<MAX_ERR_OBJS; ++i)
        if (state->err_objs[i] == (uword_t)obj) return 1;
    return 0;
}

static void hexdump_control_stacks(__attribute__((unused)) void* approximate_stackptr,
                                   __attribute__((unused)) FILE *stream)
{
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    struct thread* th;
    for_each_thread(th) {
        if (th->state_word.state == STATE_DEAD) continue;
        lispobj* stackptr;
        if (th == get_sb_vm_thread()) {
            stackptr = approximate_stackptr;
        } else {
            int ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,th))-1;
            os_context_t *c = nth_interrupt_context(ici, th);
            stackptr = (lispobj*) *os_context_register_addr(c,reg_SP);
        }
        gc_assert(((uword_t)stackptr & (LOWTAG_MASK>>1)) == 0); // lispword-aligned
        lispobj* where = th->control_stack_end;
        fprintf(stream, "\nThread @ %p\n", th);
        for (--where; where >= stackptr; --where) {
            lispobj word = *where;
            if (!fixnump(word) && gc_managed_addr_p(word))
                fprintf(stream, "  %p: %"OBJ_FMTX"\n", where, word);
        }
    }
#endif
}

extern void dump_immobile_fixedobjs(lispobj* where, lispobj* end, FILE*f);
extern void dump_immobile_text(lispobj* where, lispobj* end, FILE*f);

/* Dump spaces as human-readable text (hexadecimal) */
void hexdump_spaces(struct verify_state* state, char *reason, char* pathname)
{
    FILE *f = fopen(pathname, "w");

#ifdef LISP_FEATURE_IMMOBILE_SPACE
    fprintf(f, "Fixedobj space:\n");
    dump_immobile_fixedobjs((lispobj*)FIXEDOBJ_SPACE_START, fixedobj_free_pointer, f);
    fprintf(f, "Text space (tlsf mem @ %p):\n", tlsf_mem_start);
    dump_immobile_text((lispobj*)TEXT_SPACE_START, text_space_highwatermark, f);
#endif

    fprintf(f, "Dynamic space:\n");
    page_index_t firstpage = 0, lastpage;
    while (firstpage < next_free_page) {
        lastpage = firstpage;
        while (!page_ends_contiguous_block_p(lastpage, page_table[firstpage].gen))
            lastpage++;
        if (!page_bytes_used(firstpage)) {
            firstpage = 1+lastpage;
            continue;
        }
        lispobj* base = (lispobj*)page_address(firstpage);
        lispobj* limit = (lispobj*)page_address(lastpage) + page_words_used(lastpage);
        fprintf(f, "range %d:%d (%p:%p) t%d g%d ",
                (int)firstpage, (int)lastpage, base, limit,
                page_table[firstpage].type, page_table[firstpage].gen);
        page_index_t p;
        for (p = firstpage; p <= lastpage; ++p) {
            char marks[1+CARDS_PER_PAGE];
            putc((p == firstpage) ? '(' : ' ', f);
            fprintf(f, "%s", page_card_mark_string(p, marks));
        }
        fprintf(f, ")\n");
        lispobj *where = next_object(base, 0, limit);
        while (where){
            sword_t nwords = object_size(where);
            /* If your'e having trouble with a subset of objects, and you can get
             * a reliable reproducer, this predicate can decide which objects to
             * output in full. Generally you don't need that much output */
            if (widetag_of(where) == FILLER_WIDETAG) {
                lispobj* end = where + filler_total_nwords(*where);
                fprintf(f, " %06x: fill to %p\n", (int)(uword_t)where & 0xffffff, end);
            } else if (dump_completely_p(where, state)) {
                sword_t i;
                for(i=0;i<nwords;++i) {
                    uword_t word = where[i];
                    if (i==0)
                        fprintf(f, " %06x: ", (int)(uword_t)(where+i) & 0xffffff);
                    else
                        fprintf(f, "   %04x: ", (int)(uword_t)(where+i) & 0xffff);
                    if (word == NIL) fprintf(f, "nil"); else fprintf(f, "%" OBJ_FMTX, word);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
                    if (i == 0 && header_widetag(word) == INSTANCE_WIDETAG) word >>= 32;
#endif
                    if (is_lisp_pointer(word)
                        && (find_page_index((void*)word)>=0 || immobile_space_p(word)))
                        fprintf(f, " (g%d)", gc_gen_of(word, 0xff));
                    fprintf(f,"\n");
                }
            } else {
                int min_gen = 8;
                int prefix = ' ';
                if (widetag_of(where)==CODE_HEADER_WIDETAG && header_rememberedp(*where))
                    prefix = '#';
                else if (card_dirtyp(addr_to_card_index(where)))
                    prefix = '|';
                fprintf(f, "%c%06x: %"OBJ_FMTX, prefix, (int)(uword_t)where & 0xffffff, *where);
                int i;
                int boxed_nwords = nwords;
                // This is just a heuristic guess of pointee generation.
                // For code it's (mostly) right, for other things it's slightly less right
                // because we're really not respecting the tagged or raw nature of each word.
                if (widetag_of(where)==CODE_HEADER_WIDETAG)
                    boxed_nwords = code_header_words((struct code*)where);
                for (i=0; i<boxed_nwords; ++i) {
                    uword_t word = where[i];
                    page_index_t pointee_page;
                    if (is_lisp_pointer(word) && (pointee_page=find_page_index((void*)word))>=0
                        && page_table[pointee_page].gen < min_gen)
                        min_gen = page_table[pointee_page].gen;
                }
                if (min_gen != 8)
                    fprintf(f, " (>g%d)\n", min_gen);
                else
                  fprintf(f, "\n");
            }
            where = next_object(where, nwords, limit);
        }
        fprintf(f,"--\n");
        firstpage = 1+lastpage;
    }
    hexdump_control_stacks(&reason, f);
    fclose(f);
    fprintf(stderr, "%s: wrote [%s]\n", reason, pathname);
}

int hexdump_and_verify_heap(lispobj* cur_thread_approx_stackptr, int flags)
{
    if (hexdump_enabled) {
        char path[100];
        ++hexdump_counter;
        sprintf(path, HEXDUMP_PATH_TEMPLATE, getpid(), hexdump_counter);
        hexdump_spaces(0, flags & VERIFY_POST_GC ? "post-GC" : "pre-GC", path);
    }
#if 0
    if (hexdump_counter >= 9) {
        char pathname[128];
        sprintf(pathname, "gc-%d-%d-%d-%s.bin",
                getpid(), n_gcs, from_space,
                flags & VERIFY_POST_GC ? "post" : "pre");
        save_gc_crashdump(pathname, cur_thread_approx_stackptr);
        fprintf(stderr, "Wrote [%s]\n", pathname);
    }
#endif
    return verify_heap(cur_thread_approx_stackptr, flags);
}
#endif

/* These are do-nothing wrappers for now */
lispobj *lisp_component_ptr_from_pc(char *pc) {
    lispobj *result = component_ptr_from_pc(pc);
    return result;
}
int lisp_valid_tagged_pointer_p(lispobj pointer) {
    int result = valid_tagged_pointer_p(pointer);
    return result;
}

#ifdef LISP_FEATURE_GENERATIONAL
// For the standalone ldb monitor
void recompute_gen_bytes_allocated() {
    page_index_t page;
    int gen;
    for (gen=0; gen<NUM_GENERATIONS; ++gen)
        generations[gen].bytes_allocated = 0;
    for (page=0; page<next_free_page; ++page)
        generations[page_table[page].gen].bytes_allocated += page_bytes_used(page);
    bytes_allocated = 0;
    for (gen=0; gen<NUM_GENERATIONS; ++gen)
        bytes_allocated += generations[gen].bytes_allocated;
}
#endif

#ifdef LISP_FEATURE_DARWIN_JIT
_Atomic(char) *page_execp;
#include "sys_mmap.inc"
#include <errno.h>
/* darwin-jit has another reason to remap besides just zeroing, namely,
 * changing betwee RWX|JIT and RW-, so we don't ever want to call
 * zero_range_with_mmap because among other things it doesn't know
 * to change the bit that reflects how the range was mapped */
void remap_page_range(int option, page_index_t from, page_index_t to)
{
    int executable = option == 1;
    int noreserve = (option == 2) ? MAP_NORESERVE : 0; // for "de-commit"
    void* base = page_address(from);
    sword_t length = npage_bytes(to + 1 - from);
    void* new_addr;
    /* It's horrible that mprotect() can't do this but as you can see, the JIT bit
     * is not part of the protections but rather the flags.
     * It's even more horrible that passing MAP_FIXED to replace the mapping
     * fails with EINVAL unles you unmap first, making this highly vulnerable
     * to the bug described in zero-with-mmap-bug.txt
     */
    sbcl_munmap(base, length);
    if (executable)
        new_addr = sbcl_mmap(base, length, OS_VM_PROT_ALL, MAP_ANON|MAP_PRIVATE|MAP_JIT, -1, 0);
    else {
        new_addr = sbcl_mmap(base, length, PROT_READ|PROT_WRITE,
                             MAP_ANON|MAP_PRIVATE|noreserve, -1, 0);
    }
    if (new_addr != base) lose("remap: page moved, %p ==> %p errno=%d", base, new_addr, errno);
    page_index_t p;
    for (p = from; p <= to; ++p) {
        set_page_executable(p, executable);
        set_page_need_to_zero(p, 0);
    }
}
#endif

/*
Regarding page zero-filling, if the next operation would be memcpy(),
then zeroing is a total waste of time and we should skip it.

The most simple case seems to be ALLOCATE-CODE-OBJECT because we can treat pages
of code consistently in terms of whether the newly allocated page is for Lisp or
for the garbage collector's transport function. The object is basically an unboxed
object, so there are no "wild pointers" in it, at least until the boxed size is
written. So we can skip prezeroing because the bulk of the object will be subject
to memcpy() from either an octet vector produced by the assember, or a fasl stream.
We only need to prezero the boxed words. GC avoids prezeroing of its code pages.

The next simplest case seems to be unboxed pages - Lisp can never directly request
an unboxed page (at least in the current design), so any unboxed page is for GC,
and will always be used for a transport function. We can skip zeroing those pages,
because even if GC does not fill the page completely, it can not be used for other
object allocations from Lisp.

Boxed pages are the problem. Except for pages which are 100% used by GC, they  might
later by consumed in part by Lisp. Unfortunately we don't know whether it will be
100% used until it's 100% used. So we can't skip zeroing.
However, in general, we should try to convert Lisp allocators to be aware of
the issue of zeroing rather than relying on C to do it, as this will relieve
a pain point (a so-called "impedence mismatch") when trying to plug in other
allocators that do not intrinsically give you zero-initialized memory.

The cases can be broken down as follows:
 - unboxed objects can always be zeroed at leisure in Lisp. This is hard only because
   Lisp does not distinguish in the slow path allocator whether it is asking for boxed
   or unboxed memory, so even if we made the Lisp code perform explicit zero-filling of
   strings and numeric vectors, the allocation macros needs to be enhanced
   to inform C of the fact that zeroing will happen in Lisp whenever we have to go to
   the slow path; and we'll need unboxed thread-local regions of course.

 - structure objects almost always have all slots written immediately after
   allocation, so they don't necessarily demand prezeroing, but we have to think about
   to the scope of the pseudatomic wrapping. One of the following must pertain:
   * Widen the pseudo-atomic scope so that initialization happens within it,
     never permitting GC to see old garbage, OR
   * Store the layout last rather than first, and say that until the layout is stored,
     GC might see garbage, treating any bit pattern as a conservative pointer.
     (because there are two separate issues: ignoring old values, and ensuring that
     newly written slots are perceived as enlivening what they point to)
   * Add some bits indicating how many slots of the object are initialized.
     This seems impractical

 - general arrays present the largest problem - the choice of when to zero should be
   based on whether the object is large or not and whether one of :initial-element
   or :initial-contents were specified. If the initial-element is NIL, then the initial
   zero-fill was a waste.

 - closures and everything else except arrays are basically structure-like
   and have the same issue. Fixed-sized objects are simple though - e.g. value-cells
   can move the store of the 1 word payload inside pseudo-atomic if it isn't already.
   Thusly, any value-cell could go on a non-prezeroed page.

In general, deciding when to zero-initialize to attain maximum performance is nontrivial.
See "Why Nothing Matters: The Impact of Zeroing"
https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/zero-oopsla-2011.pdf

 */

/* Zero the memory at ADDR for LENGTH bytes, but use mmap/munmap instead
 * of zeroing it ourselves, i.e. in practice give the memory back to the
 * OS. Generally done after a large GC.
 */
#if !defined LISP_FEATURE_DARWIN_JIT && !defined LISP_FEATURE_WIN32
static void __attribute__((unused))
zero_range_with_mmap(os_vm_address_t addr, os_vm_size_t length) {
#ifdef LISP_FEATURE_LINUX
    // We use MADV_DONTNEED only on Linux due to differing semantics from BSD.
    // Linux treats it as a demand that the memory be 0-filled, or refreshed
    // from a file that backs the range. BSD takes it as a hint that you don't
    // care if the memory has to brought in from swap when next accessed,
    // i.e. it's not a request to make a user-visible alteration to memory.
    // So in theory this can bring a page in from the core file, if we happen
    // to hit a page that resides in the portion of memory mapped by coreparse.
    // In practice this should not happen because objects from a core file can't
    // become garbage. Except in save-lisp-and-die they can, and we must be
    // cautious not to resurrect bytes that originally came from the file.
    if ((os_vm_address_t)addr >= anon_dynamic_space_start) {
        if (madvise(addr, length, MADV_DONTNEED) != 0)
            lose("madvise failed");
    } else { // See doc/internals-notes/zero-with-mmap-bug.txt
        // Trying to see how often this happens.
        // fprintf(stderr, "zero_range_with_mmap: fallback to memset()\n");
        memset(addr, 0, length);
    }
#elif defined LISP_FEATURE_DARWIN // and NOT darwin-jit
    // Replace the mapping using MAP_FIXED (even though the man page says "Use of this
    // option is discouraged") which avoids succumbing to the vulnerability of unmap/remap.
    // Other BSD variants can do this but I don't know which they are.
    void *new_addr = mmap(addr, length, OS_VM_PROT_ALL,
                          MAP_ANON|MAP_PRIVATE|MAP_FIXED|MAP_NORESERVE,
                          -1, 0);
    if (new_addr != addr) lose("zero_range_with_mmap: page moved, %p ==> %p", addr, new_addr);
#else
    // As described above, this branch has a bug! We want to hold the reservation
    // on the address range, but de-commmit the storage,
    // and I don't think a generic POSIX system can do that.
    void *new_addr;
    os_deallocate(addr, length);
    new_addr = os_alloc_gc_space(DYNAMIC_CORE_SPACE_ID, NOT_MOVABLE, addr, length);
    if (new_addr == NULL || new_addr != addr) {
        lose("zero_range_with_mmap: page moved, %p ==> %p",
             addr, new_addr);
    }
#endif
}
#endif

/* Zero the pages from START to END (inclusive). Generally done just after
 * a new region has been allocated.
 */
static inline void memset_page_range(int byte, page_index_t start, page_index_t end) {
    if (start <= end)
        memset(page_address(start), byte, npage_bytes(1+end-start));
}

/* Ensure that pages from START to END (inclusive) are ready for use,
 * which entails one or more of the following:
 * - ensuring that the OS commits to backing them (#+win32 only)
 * - proper mapping for read/write (#+darwin-jit only)
 * - ensuring zero-fill if need be. Not all page types need zero-fill.
 *   Otherwise, pages which are already zero-filled are skipped.
 *   For each newly zeroed page, clear the need_to_zero flag.
 */
#if defined LISP_FEATURE_RISCV && defined LISP_FEATURE_LINUX // KLUDGE
int mmap_does_not_zero;
#endif
void prepare_pages(__attribute__((unused)) bool commit,
                   page_index_t start, page_index_t end,
                   int page_type, generation_index_t generation) {
    gc_assert(end >= start);
#ifdef LISP_FEATURE_WIN32
    if (commit)
        os_commit_memory(page_address(start), npage_bytes(end+1-start));
#endif
    // If allocating mixed pages to gen0 (or scratch which becomes gen0) then
    // this allocation is potentially going to be extended by lisp (if it happens to
    // pick up the tail of the page as its next available region)
    // and we really have to zeroize the page. Otherwise, if not mixed or allocating
    // memory that is entirely within GC, then lisp will never use parts of the page.
    // So we can avoid pre-zeroing all codes pages, all unboxed pages,
    // all strictly boxed pages, and all mixed pages allocated to gen>=1.

    page_index_t i;
#ifdef LISP_FEATURE_DARWIN_JIT
    /* Ensure that the whole range is mapped properly for page_type. If so
     * then fall into the regular logic that avoids zero-filling when possible.
     * Otherwise, remap the range even if partially ok */
    char logior = 0, logand = 1;
    for (i = start; i <= end; i++)
        logior |= page_execp[i], logand &= page_execp[i];
    if ((logior != logand) || (logior != is_code(page_type)))
        return remap_page_range(is_code(page_type), start, end);
#endif
    /* FIXME: There is a bug if BACKEND_PAGE_BYTES exceeds GENCGC_PAGE_BYTES,
     * because it can inaccurately reflect the need_to_zero state of GC pages
     * overlapping the last "backend" page mapped from the core file.
     * I really don't care. See rev e476a4dfc93d8c08 though. */
    bool usable_by_lisp =
        generation == 0 || (generation == SCRATCH_GENERATION && from_space == 0);
    if (page_type == PAGE_TYPE_MIXED && usable_by_lisp) {
        for (i = start; i <= end; i++)
            if (page_need_to_zero(i)) {
                memset_page_range(0, i, i);
                set_page_need_to_zero(i, 0);
            }
    }
}

/*
 * Supposing the OS can only operate on ranges of a certain granularity
 * (which we call 'gencgc_release_granularity'), then given any page rage,
 * align the lower bound up and the upper down to match the granularity.
 *
 *     |-->| OS page | OS page |<--|
 *
 * If the interior of the aligned range is nonempty,
 * perform three operations: unmap/remap, fill before, fill after.
 * Otherwise, just one operation to fill the whole range.
 */
#if !defined LISP_FEATURE_WIN32 && !defined LISP_FEATURE_DARWIN_JIT
const os_vm_size_t gencgc_release_granularity = BACKEND_PAGE_BYTES;
static void
release_page_range (page_index_t from, page_index_t to)
{
    /* There's a mysterious Solaris/x86 problem with using mmap
     * tricks for memory zeroing. See sbcl-devel thread
     * "Re: patch: standalone executable redux".
     */
    /* I have no idea what the issue with Haiku is, but using memset
     * works where the unmap,map technique does not. Yet using the remap
     * trick plus a post-check that the pages were correctly zeroed finds
     * no problem at that time. So what's failing later and why??? */
#if defined LISP_FEATURE_SUNOS || defined LISP_FEATURE_HAIKU
    memset_page_range(0, from, to);
#else
    size_t granularity = gencgc_release_granularity;
    // page_address "works" even if 'to' == page_table_pages-1
    char* start = page_address(from);
    char* end   = page_address(to+1);
    char* aligned_start = PTR_ALIGN_UP(start, granularity);
    char* aligned_end   = PTR_ALIGN_DOWN(end, granularity);

    /* NOTE: this is largely pointless because gencgc-release-granularity
     * is everywhere defined to be EXACTLY +backend-page-bytes+
     * which by definition is the quantum at which we'll unmap/map.
     * Maybe we should remove the needless complexity? */
    if (aligned_start < aligned_end) {
        zero_range_with_mmap(aligned_start, aligned_end-aligned_start);
        memset(start, 0, aligned_start - start);
        memset(aligned_end, 0, end - aligned_end);
    } else {
        memset_page_range(0, from, to);
    }
#endif
    page_index_t i;
    for (i = from; i <= to; i++) set_page_need_to_zero(i, 0);
}
#endif

// "Release" (i.e. try to give the OS back physical memory for) any wholly unused pages
void remap_free_pages (page_index_t from, page_index_t to)
{
    page_index_t first_page, last_page;

    for (first_page = from; first_page <= to; first_page++) {
        if (!page_free_p(first_page) || !page_need_to_zero(first_page))
            continue;

        last_page = first_page + 1;
        while (page_free_p(last_page) &&
               (last_page <= to) &&
               (page_need_to_zero(last_page)))
            last_page++;

#ifdef LISP_FEATURE_WIN32
        gc_assert(VirtualFree(page_address(first_page), npage_bytes(last_page-first_page),
                              MEM_DECOMMIT));
#elif defined LISP_FEATURE_DARWIN_JIT
        remap_page_range(2, first_page, last_page-1); // change to non-executable noreserve
#else
        release_page_range(first_page, last_page-1);
#endif

        first_page = last_page;
    }
}

#if !(defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64)
// This function pertains only to the CPUs for which instructions are 4-byte-aligned
lispobj *
dynamic_space_code_from_pc(char *pc)
{
    /* Only look at untagged pointers, otherwise they won't be in the PC.
     * (which is a valid precondition for fixed-length 4-byte instructions,
     * not variable-length) */
    if((uword_t)pc % 4 == 0 && is_code(page_table[find_page_index(pc)].type)) {
        lispobj *object = search_dynamic_space(pc);
        if (object != NULL && widetag_of(object) == CODE_HEADER_WIDETAG)
            return object;
    }

    return NULL;
}
#endif

#ifdef LISP_FEATURE_DEBUG_GC_BARRIERS

static bool card_markedp(void* addr)
{
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (immobile_space_p((lispobj)addr))
        return !immobile_card_protected_p(addr);
#endif
    return gc_card_mark[addr_to_card_index(addr)] != CARD_UNMARKED;
}

extern void check_barrier (lispobj young, lispobj old, int wp) {
    generation_index_t old_gen = gc_gen_of(old, -1);
    if (old_gen == -1 || (!wp && card_markedp(native_pointer(old))))
        return;
    generation_index_t young_gen = gc_gen_of(young, -1);
    if (young_gen == -1)
        return;
    if (old_gen > young_gen) {
        lose("check_barrier: young gen %d %lx, old gen %d %lx", young_gen, young, old_gen, old);

    }
}
#endif

// Return a native representation of the perturbed h0 supplied as a fixnum.
unsigned prefuzz_ht_hash(lispobj h0)
{
#ifdef LISP_FEATURE_64_BIT
    /* Cautiously compute in the Lisp representation
     * to ensure total consistency with the Lisp code.
     * e.g. (SB-IMPL::EQ-HASH -1s0) => -2323857407723175924
     * All of the shifts are to the right, so we needn't consider
     * overflow but we do need to kill the tag bit(s).
     * The sum can wrap, but that's OK because it gets chopped at the end */
#define fixnum_ashr(val,count) ((val>>count)&~(uword_t)FIXNUM_TAG_MASK)
    sword_t sum = (h0 ^ make_fixnum(0x39516A7))
      + fixnum_ashr(h0, 3)
      + fixnum_ashr(h0, 12)
      + fixnum_ashr(h0, 20);
    // the mask looks wrong for 32-bit, but I'm not trying to debug 32-bit.
    return fixnum_value(sum & (make_fixnum(((uword_t)1<<31)-1)));
#else
    lose("Unimplemented");
#endif
}

#ifdef LISP_FEATURE_MARK_REGION_GC
static void maybe_fix_hash_table(struct hash_table* ht, bool fix_bad)
{
    extern int verify_lisp_hashtable(struct hash_table* ht, FILE* file);
    int errors = verify_lisp_hashtable(ht, 0);
    if (!errors) return;
    if (fix_bad) {
        lispobj* data = VECTOR(ht->pairs)->data;
        char m[] = "GC: marking table for rehash\n";
        write(2, m, sizeof m-1); // (avoid possible stdio deadlock)
        data[1] |= make_fixnum(1);
        return;
    } else {
      lose("table %p should be marked for rehash", ht);
    }
}

/* Assert that every address-based key in 'ht' is findable
 * unless the table is already marked as demanding a rehash. */
static void verify_hash_table_if_possible(struct hash_table* ht, bool fix_bad)
{
    if (!ht->_count) return; // nothing to do
    uword_t flags = ht->uw_flags;
    if (flags & 2) return;  // user-defined hash-function: no good
    struct vector* kvv = (void*)native_pointer(ht->pairs);
    /* The least-significant-bit of the rehash state is "need-to-rehash"
     * The next bit is "rehashing". These act like a spinlock on the chains.
     * Rehash does not preclude further key movement apart from the key argument
     * to GET/PUT/REMHASH. We'd like to - but can't - verify anything if rehashing
     * because the chains are ill-defined.  If GC further messed the table up by
     * failing to mark it as needing rehash while in rehash then we're screwed */
    if (kvv->data[1] & make_fixnum(3)) {
        if (kvv->data[1] & make_fixnum(2)) {
            char msg[100];
            int n = snprintf(msg, sizeof msg, "CAN NOT VERIFY: table %p count=%ld\n",
                             ht, fixnum_value(ht->_count));
            write(2, msg, n);
        }
        return;
    }
    maybe_fix_hash_table(ht, fix_bad);
}

static uword_t verify_tables_in_range(lispobj* start, lispobj* end, uword_t fix_bad)
{
    lispobj* where = next_object(start, 0, end); /* find first marked object */
    lispobj layout;
    while (where) {
        if (widetag_of(where) == INSTANCE_WIDETAG &&
            (layout = instance_layout(where)) != 0 &&
            layout_depth2_id(LAYOUT(layout)) == HASH_TABLE_LAYOUT_ID)
            verify_hash_table_if_possible((struct hash_table*)where, fix_bad);
        sword_t nwords = object_size(where);
        where = next_object(where, nwords, end);
    }
    return 0;
}

void verify_hash_tables(bool fix_bad)
{
    walk_generation(verify_tables_in_range, -1, fix_bad);
}
#endif

/* This limit is adequate for testing, but a better way to handle it
 * would be to size the remset at half the objects in core permgen.
 * If that limit is reached, then don't remember individual objects
 * but instead flag all of permgen as needing to be scavenged. */
#define REMSET_GLOBAL_MAX 20000
lispobj permgen_remset[REMSET_GLOBAL_MAX];
int permgen_remset_count;

static void remset_append1(lispobj x)
{
    int n = permgen_remset_count;
    if (n == REMSET_GLOBAL_MAX) lose("global remset overflow");
    permgen_remset[n] = x;
    ++permgen_remset_count;
}

void remset_union(lispobj remset)
{
    while (remset) {
        struct vector* v = VECTOR(remset);
        int count = fixnum_value(v->data[0]);
        int i;
        for (i=0; i<count; ++i) remset_append1(v->data[i+2]);
        remset = v->data[1];
    }
}

void remember_all_permgen()
{
    permgen_bounds[1] = PERMGEN_SPACE_START;
    memset(permgen_remset, 0, permgen_remset_count*N_WORD_BYTES);
    permgen_remset_count = 0;
}

void illegal_linkage_space_call() {
    lose("jumped via obsolete linkage entry");
}

void scavenge_elf_linkage_space()
{
    // ELF space linkage cells, if present, are roots for GC.
    lispobj modified_vector = SYMBOL(ELF_LINKAGE_CELL_MODIFIED)->value;
    if (modified_vector == NIL) return;
    struct vector* v = VECTOR(modified_vector);
    uword_t* bits = v->data;
    unsigned int nbits = vector_len(v);
    unsigned int nwords = (nbits + N_WORD_BITS-1)/N_WORD_BITS;
    unsigned int wordindex;
    int ind_major = 0, ind_minor;
    for (wordindex = 0; wordindex < nwords; ++wordindex, ind_major += N_WORD_BITS) {
        uword_t word = bits[wordindex];
        for ( ind_minor = 0 ; word != 0 ; word >>= 1, ++ind_minor ) {
            // Unmodified cells can be ignored
            if (!(word & 1)) continue;
            int linkage_index = ind_major + ind_minor;
            lispobj entrypoint = elf_linkage_space[linkage_index];
            /* Entrypoint can't become illegal_linkage_space_call because only
             * the cells associated with the non-ELF linkage space get swept
             * (i.e. smashed in the manner of weak objects). */
            if (!entrypoint) continue;
            lispobj taggedptr = fun_taggedptr_from_self(entrypoint);
            lispobj new = taggedptr;
            scav1(&new, new);
            if (new != taggedptr)
                elf_linkage_space[linkage_index] = new + (entrypoint - taggedptr);
        }
    }
}

void sweep_linkage_space()
{
    // Erase linkage cells whose name got NILed in the weak vector clearing pass
    struct vector* outer = VECTOR(SYMBOL(LINKAGE_NAME_MAP)->value);
    int outer_len = vector_len(outer), index1 = 0, linkage_index = 0;
    for ( ; index1 < outer_len ; ++index1) {
        lispobj v = outer->data[index1];
        if (!v) break;
        struct vector* inner = VECTOR(v);
        int index2 = 0, limit = vector_len(inner);
        for ( ; index2 < limit ; ++index2, ++linkage_index )
            if (inner->data[index2] == NIL && linkage_space[linkage_index])
                linkage_space[linkage_index] = (uword_t)illegal_linkage_space_call;
    }
}
