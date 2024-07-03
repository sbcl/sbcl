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

#include "gc.h"
#include "genesis/gc-tables.h"
#include "genesis/closure.h"
#include "genesis/instance.h"
#include "genesis/vector.h"
#include "genesis/hash-table.h"
#include "genesis/static-symbols.h"
#include "genesis/symbol.h"
#include "code.h"
#include "immobile-space.h"
#include "queue.h"
#include "os.h"
#include "validate.h"
#include "var-io.h"

#include <stdio.h>
#ifndef LISP_FEATURE_WIN32
#define HAVE_GETRUSAGE 1
#include <sys/resource.h> // for getrusage()
#endif

struct unbounded_queue {
  struct Qblock* head_block;
  struct Qblock* tail_block;
  struct Qblock* recycler;
  long tot_count; // Not used
} scav_queue;

/* Initialized to number of pages in page table
 * and decremented before use. */
static page_index_t free_page;

/* The whole-page allocator works backwards from the end of dynamic space.
 * If it collides with 'next_free_page', then you lose.
 * TODO: It would be reasonably simple to have this request more memory from
 * the OS instead of failing on overflow */
static void* get_free_page() {
    --free_page;
    if (free_page < next_free_page)
        lose("Needed more space to GC");
    set_page_type(page_table[free_page], PAGE_TYPE_UNBOXED);
    char* mem = page_address(free_page);
    prepare_pages(1, free_page, free_page, PAGE_TYPE_UNBOXED, -1);
    return mem;
}

static void gc_enqueue(lispobj object)
{
    gc_dcheck(is_lisp_pointer(object));
    gc_dcheck(widetag_of(native_pointer(object)) != SIMPLE_FUN_WIDETAG);
    struct Qblock* block = scav_queue.tail_block;
    if (block->count == QBLOCK_CAPACITY) {
        struct Qblock* next;
        next = scav_queue.recycler;
        if (next) {
            scav_queue.recycler = next->next;
        } else {
            next = (struct Qblock*)get_free_page();
        }
        block = block->next = next;
        block->next = 0;
        block->tail = block->count = 0;
        scav_queue.tail_block = block;
    }
    block->elements[block->tail] = object;
    if (++block->tail == QBLOCK_CAPACITY) block->tail = 0;
    ++block->count;
}

static lispobj gc_dequeue()
{
    struct Qblock* block = scav_queue.head_block;
    gc_assert(block->count);
    int index = block->tail - block->count;
    lispobj object = block->elements[index + (index<0 ? QBLOCK_CAPACITY : 0)];
    if (--block->count == 0) {
        struct Qblock* next = block->next;
        if (next) {
            scav_queue.head_block = next;
            block->next = scav_queue.recycler;
            scav_queue.recycler = block;
        }
    }
    return object;
}

static inline sword_t dword_index(uword_t ptr, uword_t base) {
    return (ptr - base) >> (1+WORD_SHIFT);
}

/* The "canonical" pointer to an object is usually just the object itself.
 * This is true even for SIMPLE-FUN- we don't need to regard only the code base
 * as canonical. The exception is that LRAs can't be marked because they can't
 * be discovered and marked when marking their containing code */
static inline lispobj canonical_ptr(lispobj pointer)
{
#ifdef RETURN_PC_WIDETAG
  /* NO_TLS_VALUE is all 1s, and so it might look like it has OTHER_POINTER_LOWTAG
   * depending on the architecture (the word size, etc), but there is no memory
   * at 0xff...ff so definitely don't call widetag_of - that won't fly! */
    if (lowtag_of(pointer)==OTHER_POINTER_LOWTAG
        && pointer != NO_TLS_VALUE_MARKER
        && widetag_of(native_pointer(pointer)) == RETURN_PC_WIDETAG)
        return fun_code_tagged(native_pointer(pointer));
#endif
    return pointer;
}

sword_t fixedobj_index_bit_bias, text_index_bit_bias;
uword_t *fullcgcmarks;
static size_t markbits_size;
static inline sword_t ptr_to_bit_index(lispobj pointer) {
    if (pointer == NIL) return -1;
    page_index_t p = find_page_index((void*)pointer);
    if (p >= 0) return dword_index(pointer, DYNAMIC_SPACE_START);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    p = find_fixedobj_page_index((void*)pointer);
    if (p >= 0) return dword_index(pointer, FIXEDOBJ_SPACE_START) + fixedobj_index_bit_bias;
    p = find_text_page_index((void*)pointer);
    if (p >= 0) return dword_index(pointer, TEXT_SPACE_START) + text_index_bit_bias;
#endif
    return -1;
}
#define interesting_pointer_p(x) ptr_to_bit_index(x)>=0

/* Return true if OBJ has already survived the current GC. */
static inline bool pointer_survived_gc_yet(lispobj pointer)
{
    sword_t mark_index = ptr_to_bit_index(canonical_ptr(pointer));
    if (mark_index < 0) return 1; // "uninteresting" objects always survive GC
    return (fullcgcmarks[mark_index / N_WORD_BITS] >> (mark_index % N_WORD_BITS)) & 1;
}

bool fullcgc_lispobj_livep(lispobj pointer) {
    return pointer_survived_gc_yet(pointer);
}

void dump_marked_objects() {
    fprintf(stderr, "Marked objects:\n");
    page_index_t first = 0;
    int n = 0;
    while (first < next_free_page) {
        page_index_t last = contiguous_block_final_page(first);
        lispobj* where = (lispobj*)page_address(first);
        lispobj* limit = (lispobj*)page_address(last) + page_words_used(last);
        while (where < limit) {
            lispobj obj = compute_lispobj(where);
            if (pointer_survived_gc_yet(obj)) {
                ++n;
                fprintf(stderr, " %"OBJ_FMTX"\n", obj);
            }
            where += object_size(where);
        }
        first = 1 + last;
    }
    fprintf(stderr, "Total: %d\n", n);
}

/* If stray pointer detection is being performed, then all weak references
 * (weak pointers, weak hash tables) are treated as strong.
 * This informs the consumer whether it is possible to reach an object
 * that satisfies the test given the current heap state */
lispobj stray_pointer_source_obj;
int (*stray_pointer_detector_fn)(lispobj); // return value is unused
static void __mark_obj(lispobj pointer)
{
    lispobj* base;

    pointer = canonical_ptr(pointer);
    sword_t mark_index = ptr_to_bit_index(pointer);
    if (mark_index < 0) {
        if (stray_pointer_detector_fn) stray_pointer_detector_fn(pointer);
        return; // uninteresting pointer
    }
    uword_t wordindex = mark_index / N_WORD_BITS;
    uword_t bit = (uword_t)1 << (mark_index % N_WORD_BITS);
    if (fullcgcmarks[wordindex] & bit) return; // already marked
    if (lowtag_of(pointer) == FUN_POINTER_LOWTAG
        && embedded_obj_p(widetag_of((lispobj*)FUNCTION(pointer)))) {
        lispobj* code = (void*)fun_code_header(FUNCTION(pointer));
        mark_index -= ((char*)FUNCTION(pointer) - (char*)code) >> (1+WORD_SHIFT);
        pointer = make_lispobj(code, OTHER_POINTER_LOWTAG);
        base = code;
        wordindex = mark_index / N_WORD_BITS;
        bit = (uword_t)1 << (mark_index % N_WORD_BITS);
        if (fullcgcmarks[wordindex] & bit) return; // already marked
    } else
        base = native_pointer(pointer);
    fullcgcmarks[wordindex] |= bit;
    // FIXME: restore the code for #ifdef LISP_FEATURE_UBSAN
    if (widetag_of(base) == CODE_HEADER_WIDETAG) {
        struct code* code = (void*)base;
        /* mark all simple-funs which speeds up pointer_survived_gc_yet.
         * Just add the offset in dwords from base to each fun to compute
         * the mark bit index (rather than calling ptr_to_bit_index) */
        for_each_simple_fun(i, fun, code, 0, {
            unsigned int offset = ((char*)fun - (char*)base) >> (1+WORD_SHIFT);
            uword_t funmark = mark_index + offset;
            fullcgcmarks[funmark / N_WORD_BITS] |= (uword_t)1 << (funmark % N_WORD_BITS);
        })
    }
    if (listp(pointer) || !leaf_obj_widetag_p(widetag_of(base))) gc_enqueue(pointer);
}

inline void gc_mark_obj(lispobj thing) {
    if (is_lisp_pointer(thing)) __mark_obj(thing);
}

static inline void mark_pair(lispobj* where)
{
    gc_mark_obj(where[0]);
    gc_mark_obj(where[1]);
}

void gc_mark_range(lispobj* where, long count) {
    long i;
    for(i=0; i<count; ++i) gc_mark_obj(where[i]);
}

#define TRACE_NAME trace_object
#define ACTION(x, dummy1, dummy2) gc_mark_obj(x)
#define STRENGTHEN_WEAK_REFS stray_pointer_detector_fn
#define HT_ENTRY_LIVENESS_FUN_ARRAY_NAME alivep_funs
#include "trace-object.inc"

void prepare_for_full_mark_phase()
{
    free_page = page_table_pages;
    struct Qblock* block = (struct Qblock*)get_free_page();
    scav_queue.head_block = block;
    scav_queue.tail_block = block;
    scav_queue.recycler   = 0;
    block->next = 0;
    block->tail = block->count = 0;
    /* Consume as many bits as cover the entire dynamic space regardless
     * of its current usage.  Same for the other spaces.
     * This previously tried to be clever about using only as many bits for
     * dynamic space as correspond to the current high water mark, which was
     * an ill-conceived idea, because cull_weak_hash_tables() can consume
     * dynamic space when processing finalizers. So it marks an object live,
     * but that object's mark bit could be past the reserved range of dynamic
     * space mark bits, thus accidentally marking some _other_ thing live.
     * And heaven forbid that other object isn't supposed to be live,
     * you're in for a heap of trouble (pun intended) */
    sword_t nbits_dynamic = dynamic_space_size / (2*N_WORD_BYTES);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    sword_t nbits_fixedobj = FIXEDOBJ_SPACE_SIZE / (2*N_WORD_BYTES);
    sword_t nbits_text = text_space_size / (2*N_WORD_BYTES);
    fixedobj_index_bit_bias = nbits_dynamic;
    text_index_bit_bias = fixedobj_index_bit_bias + nbits_fixedobj;
    sword_t nbytes = (nbits_dynamic + nbits_fixedobj + nbits_text) / 8;
#else
    sword_t nbytes = nbits_dynamic / 8;
#endif
    markbits_size = ALIGN_UP(nbytes, GETPAGESIZE);
    fullcgcmarks = (void*)os_allocate(markbits_size);
}

void scav_static_range(lispobj* where, lispobj* end)
{
    while (where < end) {
        lispobj obj = compute_lispobj(where);
        gc_enqueue(obj);
        where += listp(obj) ? 2 : headerobj_size(where);
    }
}

void execute_full_mark_phase()
{
#ifdef HAVE_GETRUSAGE
    struct rusage before, after;
    getrusage(RUSAGE_SELF, &before);
#endif
    trace_object((lispobj*)NIL_SYMBOL_SLOTS_START);
    scav_static_range((lispobj*)STATIC_SPACE_OBJECTS_START, static_space_free_pointer);
    scav_static_range((lispobj*)PERMGEN_SPACE_START, permgen_space_free_pointer);
#ifndef LISP_FEATURE_IMMOBILE_SPACE
    // if NO immobile-space, then text space is equivalent to static space
    scav_static_range((lispobj*)TEXT_SPACE_START, text_space_highwatermark);
#endif
    gc_mark_obj(lisp_package_vector);
    gc_mark_obj(lisp_init_function);
    gc_mark_obj(alloc_profile_data);
    do {
        lispobj ptr = gc_dequeue();
        gc_dcheck(ptr != 0);
        stray_pointer_source_obj = ptr;
        if (!listp(ptr))
            trace_object(native_pointer(ptr));
        else
            mark_pair((lispobj*)(ptr - LIST_POINTER_LOWTAG));
    } while (scav_queue.head_block->count ||
             (test_weak_triggers(pointer_survived_gc_yet, gc_mark_obj) &&
              scav_queue.head_block->count));
    stray_pointer_source_obj = 0;

#ifdef HAVE_GETRUSAGE
    getrusage(RUSAGE_SELF, &after);
#define timediff(b,a,field) \
    (double)((a.field.tv_sec-b.field.tv_sec)*1000000 + \
             (a.field.tv_usec-b.field.tv_usec)) / 1000000.0
    if (gencgc_verbose)
        fprintf(stderr,
                "[Mark phase: %d pages used, ET=%f+%f sys+usr]\n",
                (int)(page_table_pages - free_page),
                timediff(before, after, ru_stime), timediff(before, after, ru_utime));
#endif
}

static void local_smash_weak_pointers()
{
    struct weak_pointer *wp, *next_wp;
    for (wp = weak_pointer_chain; wp != WEAK_POINTER_CHAIN_END; wp = next_wp) {
        gc_assert(widetag_of(&wp->header) == WEAK_POINTER_WIDETAG);
        next_wp = get_weak_pointer_next(wp);
        reset_weak_pointer_next(wp);
        lispobj pointee = wp->value;
        gc_assert(is_lisp_pointer(pointee));
        if (!pointer_survived_gc_yet(pointee))
            wp->value = UNBOUND_MARKER_WIDETAG;
    }
    weak_pointer_chain = WEAK_POINTER_CHAIN_END;

    struct cons* vectors = weak_vectors;
    while (vectors) {
        struct vector* vector = (struct vector*)vectors->car;
        vectors = (struct cons*)vectors->cdr;
        UNSET_WEAK_VECTOR_VISITED(vector);
        sword_t len = vector_len(vector);
        sword_t i;
        for (i = 0; i<len; ++i) {
            lispobj obj = vector->data[i];
            // Ignore non-pointers
            if (is_lisp_pointer(obj) && !pointer_survived_gc_yet(obj))
                vector->data[i] = NIL;
        }
    }
    weak_vectors = 0;
}

#ifndef LISP_FEATURE_IMMOBILE_SPACE
#undef immobile_obj_gen_bits
#define immobile_obj_gen_bits(x) (lose("No page index?"),0)
#else
static void sweep_fixedobj_pages()
{
    low_page_index_t page;
    uword_t space_base = FIXEDOBJ_SPACE_START;
    sword_t bitmap_index_bias = fixedobj_index_bit_bias;
    for (page = FIXEDOBJ_RESERVED_PAGES ; ; ++page) {
        lispobj *obj = fixedobj_page_address(page);
        if (obj >= fixedobj_free_pointer)
            break;
        int obj_spacing = fixedobj_page_obj_align(page);
        if (!obj_spacing)
            continue;
        int nwords = obj_spacing >> WORD_SHIFT;
        lispobj *limit = (lispobj*)((char*)obj + IMMOBILE_CARD_BYTES - obj_spacing);
        for ( ; obj <= limit ; obj = (lispobj*)((char*)obj + obj_spacing) ) {
            lispobj header = *obj;
            if (fixnump(header)) continue; // is a hole
            uword_t index = bitmap_index_bias + (((uword_t)obj - space_base) >> (1+WORD_SHIFT));
            uword_t livep =
                fullcgcmarks[index / N_WORD_BITS] & ((uword_t)1 << (index % N_WORD_BITS));
            if (!livep) memset(obj, 0, nwords * N_WORD_BYTES);
        }
    }
}
#endif

/* Overwrite exactly 1 object with non-pointer words of some sort.
 * This eliminates tenured garbage in pseudo-static-generation,
 * and does NOT strive to to write as few words as possible,
 * unlike deposit_filler() which tries to be efficient */
static void clobber_headered_object(lispobj* addr, sword_t nwords)
{
    page_index_t page = find_page_index(addr);
    if (page < 0) { // code space
#ifdef LISP_FEATURE_IMMOBILE_SPACE
        extern lispobj codeblob_freelist;
        if (widetag_of(addr) == CODE_HEADER_WIDETAG) {
            // OAOO violation - like sweep_immobile_text()
            assign_widetag(addr, FILLER_WIDETAG);
            ((char*)addr)[2] = 0; // clear the TRACED flag
            ((char*)addr)[3] = 0; // clear the WRITTEN flag and the generation
            // add to list only if it is above tlsf_mem_start
            // (below it will never by utilized by the TLSF allocator)
            if (addr >= tlsf_mem_start) {
                addr[1] = codeblob_freelist; // push into to-be-freed list
                codeblob_freelist = (lispobj)addr;
            }
        }
#endif
    } else if (is_code(page_table[page].type)) {
        // Code pages don't want (0 . 0) fillers, otherwise heap checking
        // gets an error: "object @ 0x..... is non-code on code page"
        addr[0] = make_filler_header(nwords);
        addr[1] = 0;
    } else {
        memset(addr, 0, nwords * N_WORD_BYTES);
    }
}

static uword_t sweep(lispobj* where, lispobj* end,
                     __attribute__((unused)) uword_t arg)
{
    sword_t nwords;
    uword_t space_base = DYNAMIC_SPACE_START;
    sword_t bitmap_index_bias = 0;
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (find_page_index(where) < 0) {
        gc_assert(find_text_page_index(where) >= 0);
        space_base = TEXT_SPACE_START;
        bitmap_index_bias = text_index_bit_bias;
    }
#endif
    for ( ; where < end ; where += nwords ) {
        lispobj word = *where;
        uword_t index = bitmap_index_bias + (((uword_t)where - space_base) >> (1+WORD_SHIFT));
        uword_t livep =
            fullcgcmarks[index / N_WORD_BITS] & ((uword_t)1 << (index % N_WORD_BITS));
        if (is_header(word)) {
            nwords = headerobj_size2(where, word);
            if (!livep && header_widetag(word) != FILLER_WIDETAG)
                clobber_headered_object(where, nwords);
        } else {
            nwords = 2;
            if (!livep) where[0] = where[1] = (uword_t)-1;
        }
    }
    return 0;
}

#ifndef LISP_FEATURE_MARK_REGION_GC
static uword_t sweep_possibly_large(lispobj* where, lispobj* end,
                                    __attribute__((unused)) uword_t arg)
{
    extern void free_large_object(lispobj*, lispobj*);
    if (page_single_obj_p(find_page_index(where))) {
        if (!pointer_survived_gc_yet((lispobj)where)) free_large_object(where, end);
    } else
        sweep(where, end, arg);
    return 0;
}
#endif

void dispose_markbits() {
    os_deallocate((void*)fullcgcmarks, markbits_size);
    fullcgcmarks = 0; markbits_size = 0;
    page_index_t page;
    // Give back all private-use pages and indicate need-to-zero
    for (page = free_page; page < page_table_pages; ++page) {
        gc_assert((page_table[page].type & PAGE_TYPE_MASK) == PAGE_TYPE_UNBOXED);
        gc_assert(!page_bytes_used(page));
        set_page_need_to_zero(page, 1);
        set_page_type(page_table[page], FREE_PAGE_FLAG);
      }
}

void execute_full_sweep_phase()
{
#ifdef LISP_FEATURE_MARK_REGION_GC
    lose("Can't do sweep");
#else
    long words_zeroed[1+PSEUDO_STATIC_GENERATION]; // One count per generation

    local_smash_weak_pointers();
    gc_dispose_private_pages();
    cull_weak_hash_tables(alivep_funs);
    scan_finalizers();

    memset(words_zeroed, 0, sizeof words_zeroed);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    sweep_fixedobj_pages();
    sweep((lispobj*)TEXT_SPACE_START, text_space_highwatermark,
          (uword_t)words_zeroed);
    // Recompute generation masks for text space
    int npages = (ALIGN_UP((uword_t)text_space_highwatermark, IMMOBILE_CARD_BYTES)
                  - TEXT_SPACE_START) / IMMOBILE_CARD_BYTES;
    memset(text_page_genmask, 0, npages);
    lispobj* where = (lispobj*)TEXT_SPACE_START;
    for ( ; where < text_space_highwatermark ; where += object_size(where) )
        if (widetag_of(where) == CODE_HEADER_WIDETAG)
            text_page_genmask[find_text_page_index(where)]
                |= (1 << immobile_obj_gen_bits(where));
#endif
    walk_generation(sweep_possibly_large, -1, (uword_t)words_zeroed);
    if (gencgc_verbose) {
        fprintf(stderr, "[Sweep phase: ");
        int i;
        for(i=6;i>=0;--i)
            fprintf(stderr, "%ld%s", words_zeroed[i], i?"+":"");
        fprintf(stderr, " words zeroed]\n");
    }
    dispose_markbits();
#endif
}
