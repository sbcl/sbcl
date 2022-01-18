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
#include "gc-internal.h"
#include "gc-private.h"
#include "gencgc-private.h"
#include "genesis/gc-tables.h"
#include "genesis/closure.h"
#include "genesis/cons.h"
#include "genesis/instance.h"
#include "genesis/vector.h"
#include "genesis/layout.h"
#include "genesis/hash-table.h"
#include "code.h"
#include "immobile-space.h"
#include "queue.h"

#include <stdio.h>
#ifndef LISP_FEATURE_WIN32
#define HAVE_GETRUSAGE 1
#endif
#if HAVE_GETRUSAGE
#include <sys/resource.h> // for getrusage()
#endif


/* Most headered objects use MARK_BIT to record liveness.
 * Bignums always use the leftmost bit regardless of word size.
 * Fdefns use 0x4000 which overlaps the 'written' bit in the generation byte,
 * but 'written' is not used except for code objects, so this is fine.
 *
 * Bit 31 of the header is the mark bit for all remaining object types.
 * This avoids clash with the layout pointer of instances and functions,
 * the TLS index of symbols, and various other bits.
 * The mark bit occupies the same byte as the generation number
 * in immobile space, but doesn't conflict with that usage.
 */
#define MARK_BIT ((uword_t)1 << 31)
#define FDEFN_MARK_BIT 0x4000
#ifdef LISP_FEATURE_64_BIT
#define BIGNUM_MARK_BIT ((uword_t)1 << 63)
#else
#define BIGNUM_MARK_BIT MARK_BIT
#endif

#define interesting_pointer_p(x) \
  (find_page_index((void*)x) >= 0 || immobile_space_p(x))

#ifdef DEBUG
#  define dprintf(arg) printf arg
FILE * logfile;
#else
#  define dprintf(arg)
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
    page_table[free_page].type = PAGE_TYPE_UNBOXED;
    char* mem = page_address(free_page);
    zero_dirty_pages(free_page, free_page, 0);
    return mem;
}

/* The suballocator doles out blocks of bits for marking conses live.
 * Example: If pages are 32768 bytes, and Lisp words are 8 bytes,
 * then one GC page can hold 2K cons cells.
 * One byte marks 8 conses (1 bit per cons), 256 bytes mark 2048 conses.
 * 128 blocks of 256 bytes fit on a 32K GC page. */
char *suballocator_free_ptr, *suballocator_end_ptr;

static void* allocate_cons_mark_bits() {
    int nbytes = GENCGC_PAGE_BYTES / (2 * N_WORD_BYTES) / 8;
    if (suballocator_free_ptr + nbytes > suballocator_end_ptr) {
        suballocator_free_ptr = get_free_page();
        suballocator_end_ptr = suballocator_free_ptr + GENCGC_PAGE_BYTES;
    }
    void* mem = suballocator_free_ptr;
    suballocator_free_ptr += nbytes;
    return mem;
}

static void gc_enqueue(lispobj object)
{
    gc_dcheck(is_lisp_pointer(object));
    struct Qblock* block = scav_queue.tail_block;
    if (block->count == QBLOCK_CAPACITY) {
        struct Qblock* next;
        next = scav_queue.recycler;
        if (next) {
            scav_queue.recycler = next->next;
            next->next = 0;
            dprintf(("Popped recycle list\n"));
        } else {
            next = (struct Qblock*)get_free_page();
            dprintf(("Alloc'd new block\n"));
        }
        block = block->next = next;
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
            dprintf(("Qblock emptied - returned to recycle list\n"));
        } else {
            dprintf(("Qblock emptied - NOT returned to recycle list\n"));
        }
    }
    return object;
}

/* The 'mark_bits' hashtable maps a page address to a block of mark bits
 * for headerless objects (conses) */
struct hopscotch_table mark_bits;

static inline uword_t compute_page_key(lispobj cons) {
    return ALIGN_DOWN(cons, GENCGC_PAGE_BYTES);
}
static inline int compute_dword_number(lispobj cons) {
    return (cons & (GENCGC_PAGE_BYTES - 1)) >> (1+WORD_SHIFT);
}

static inline int cons_markedp(lispobj pointer) {
    unsigned char* bits = (unsigned char*)
        hopscotch_get(&mark_bits, compute_page_key(pointer), 0);
    if (!bits) return 0;
    int index = compute_dword_number(pointer);
    return (bits[index / 8] >> (index % 8)) & 1;
}

/* Return true if OBJ has already survived the current GC. */
static inline int pointer_survived_gc_yet(lispobj pointer)
{
    if (!interesting_pointer_p(pointer))
        return 1;
    if (listp(pointer))
        return cons_markedp(pointer);
    lispobj header = *native_pointer(pointer);
    int widetag = header_widetag(header);
    switch (widetag) {
    case BIGNUM_WIDETAG: return (header & BIGNUM_MARK_BIT) != 0;
    case FDEFN_WIDETAG : return (header & FDEFN_MARK_BIT) != 0;
    }
    if (embedded_obj_p(widetag))
        header = *fun_code_header(native_pointer(pointer));
    return (header & MARK_BIT) != 0;
}

void __mark_obj(lispobj pointer)
{
    gc_dcheck(is_lisp_pointer(pointer));
    if (!interesting_pointer_p(pointer))
        return;
    if (!listp(pointer)) {
        lispobj* base = native_pointer(pointer);
        lispobj header = *base;
        int widetag = header_widetag(header);
        if (widetag == BIGNUM_WIDETAG) {
            *base |= BIGNUM_MARK_BIT;
            return; // don't enqueue - no pointers
        } else {
            if (embedded_obj_p(widetag)) {
                base = fun_code_header(base);
                pointer = make_lispobj(base, OTHER_POINTER_LOWTAG);
                header = *base;
            }
            uword_t markbit = (widetag == FDEFN_WIDETAG) ? FDEFN_MARK_BIT : MARK_BIT;
            if (header & markbit) return; // already marked
            *base |= markbit;
        }
#ifdef LISP_FEATURE_UBSAN
        if (specialized_vector_widetag_p(widetag) && is_lisp_pointer(base[1]))
            gc_mark_obj(base[1]);
        else if (widetag == SIMPLE_VECTOR_WIDETAG && fixnump(base[1])) {
            char *origin_pc = (char*)(base[1]>>4);
            lispobj* code = component_ptr_from_pc(origin_pc);
            if (code) gc_mark_obj(make_lispobj(code, OTHER_POINTER_LOWTAG));
            /* else lose("can't find code containing %p (vector=%p)", origin_pc, base); */
        }
#endif
        if (leaf_obj_widetag_p(widetag)) return;
    } else {
        uword_t key = compute_page_key(pointer);
        int index = compute_dword_number(pointer);
        unsigned char* bits = (unsigned char*)hopscotch_get(&mark_bits, key, 0);
        if (!bits) {
            bits = allocate_cons_mark_bits();
            hopscotch_insert(&mark_bits, key, (sword_t)bits);
        } else if (bits[index / 8] & (1 << (index % 8))) {
            return;
        }
        // Mark the cons
        bits[index / 8] |= 1 << (index % 8);
    }
    gc_enqueue(pointer);
}

inline void gc_mark_obj(lispobj thing) {
    if (is_lisp_pointer(thing))
        __mark_obj(thing);
}

static inline void mark_pair(lispobj* where)
{
    gc_mark_obj(where[0]);
    gc_mark_obj(where[1]);
}

void gc_mark_range(lispobj* where, long count) {
    long i;
    for(i=0; i<count; ++i)
        gc_mark_obj(where[i]);
}

#define HT_ENTRY_LIVENESS_FUN_ARRAY_NAME alivep_funs
#include "weak-hash-pred.inc"

static void trace_using_layout(lispobj layout, lispobj* where, int nslots)
{
    // Apart from the allowance for untagged pointers in lockfree list nodes,
    // this contains almost none of the special cases that gencgc does.
    if (!layout) return;
#ifdef LISP_FEATURE_METASPACE
    gc_mark_obj(LAYOUT(layout)->friend);
#else
    gc_mark_obj(layout);
#endif
    if (lockfree_list_node_layout_p(LAYOUT(layout))) { // allow untagged 'next'
        struct instance* node = (struct instance*)where;
        lispobj next = node->slots[INSTANCE_DATA_START];
        // ignore if 0
        if (fixnump(next) && next) __mark_obj(next|INSTANCE_POINTER_LOWTAG);
    }
    struct bitmap bitmap = get_layout_bitmap(LAYOUT(layout));
    int i;
    lispobj* slots = where+1;
    for (i=0; i<nslots; ++i)
        if (bitmap_logbitp(i, bitmap) && is_lisp_pointer(slots[i]))
            __mark_obj(slots[i]);
}

static void trace_object(lispobj* where)
{
    lispobj header = *where;
    int widetag = header_widetag(header);

    if (instanceoid_widetag_p(widetag))
        return trace_using_layout(layout_of(where), where,
                                  instanceoid_length(header));
    sword_t scan_from = 1;
    sword_t scan_to = sizetab[widetag](where);
    sword_t i;
    struct weak_pointer *weakptr;
    switch (widetag) {
    case SIMPLE_VECTOR_WIDETAG:
#ifdef LISP_FEATURE_UBSAN
        if (is_lisp_pointer(where[1])) gc_mark_obj(where[1]);
#endif
        // non-weak hashtable kv vectors are trivial in fullcgc. Keys don't move
        // so the table will not need rehash as a result of gc.
        // Ergo, those may be treated just like ordinary simple vectors.
        // However, weakness remains as a special case.
        if (vector_flagp(header, VectorWeak)) {
            if (!vector_flagp(header, VectorHashing)) {
                add_to_weak_vector_list(where, header);
                return;
            }
            // Ok, we're looking at a weak hash-table.
            struct vector* v = (struct vector*)where;
            lispobj lhash_table = v->data[vector_len(v)-1];
            gc_dcheck(instancep(lhash_table));
            __mark_obj(lhash_table);
            struct hash_table* hash_table
              = (struct hash_table *)native_pointer(lhash_table);
            gc_assert(hashtable_weakp(hash_table));
            // An object can only be removed from the queue once.
            // Therefore the 'next' pointer has got to be nil.
            gc_assert(hash_table->next_weak_hash_table == NIL);
            int weakness = hashtable_weakness(hash_table);
            boolean defer = 1;
            if (weakness != WEAKNESS_KEY_AND_VALUE)
                defer = scan_weak_hashtable(hash_table, alivep_funs[weakness],
                                            mark_pair);
            if (defer) {
                hash_table->next_weak_hash_table = (lispobj)weak_hash_tables;
                weak_hash_tables = hash_table;
            }
            return;
        }
        break;
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64) || defined (LISP_FEATURE_ARM64)
    /* on x86[-64], closure->fun is a fixnum-qua-pointer. Convert it to a lisp
     * pointer to mark it, but not on platforms where it's already a descriptor */
    case CLOSURE_WIDETAG:
        gc_mark_obj(fun_taggedptr_from_self(((struct closure*)where)->fun));
        scan_from = 2;
        break; // scan slots normally
#endif
    case CODE_HEADER_WIDETAG:
        scan_to = code_header_words((struct code*)where);
#ifdef LISP_FEATURE_UNTAGGED_FDEFNS
        struct code* code = (struct code*)where;
        lispobj* fdefns_start = code->constants
                                + code_n_funs(code) * CODE_SLOTS_PER_SIMPLE_FUN;
        lispobj* fdefns_end  = fdefns_start + code_n_named_calls(code);
        lispobj* limit = where + scan_to;
        where = where + scan_from;
        while (where < limit) {
            lispobj word = *where;
            if (where >= fdefns_start && where < fdefns_end) word |= OTHER_POINTER_LOWTAG;
            gc_mark_obj(word);
            ++where;
        }
        return;
#endif
        break;
#ifdef LISP_FEATURE_COMPACT_SYMBOL
    case SYMBOL_WIDETAG:
        {
        struct symbol* s = (void*)where;
        gc_mark_obj(decode_symbol_name(s->name));
        gc_mark_obj(s->value);
        gc_mark_obj(s->info);
        gc_mark_obj(s->fdefn);
        // process the unnamed slot of augmented symbols
        if ((s->header & 0xFF00) == (SYMBOL_SIZE<<8)) gc_mark_obj(*(1+&s->fdefn));
        }
        return;
#endif
    case FDEFN_WIDETAG:
        gc_mark_obj(fdefn_callee_lispobj((struct fdefn*)where));
        scan_to = 3;
        break;
    case WEAK_POINTER_WIDETAG:
        weakptr = (struct weak_pointer*)where;
        if (is_lisp_pointer(weakptr->value) && interesting_pointer_p(weakptr->value))
            add_to_weak_pointer_chain(weakptr);
        return;
    default:
        if (leaf_obj_widetag_p(widetag)) return;
    }
    for(i=scan_from; i<scan_to; ++i)
        gc_mark_obj(where[i]);
}

void prepare_for_full_mark_phase()
{
    // FIXME: Estimate how large to create mark_bits based on dynamic space size.
    // Guess 8 words per object, and X% of the objects are conses.
    // The problem is guessing how localized the conses are: guess that N conses
    // will reside on fraction*N different pages, which guides us as to how many
    // hash table entries are needed.
    hopscotch_create(&mark_bits, HOPSCOTCH_HASH_FUN_DEFAULT,
                     N_WORD_BYTES, /* table values are machine words */
                     65536, /* initial size */
                     0);

    free_page = page_table_pages;
    suballocator_free_ptr = suballocator_end_ptr = 0;
    struct Qblock* block = (struct Qblock*)get_free_page();
    dprintf(("Queue block holds %d objects\n", (int)QBLOCK_CAPACITY));
    scav_queue.head_block = block;
    scav_queue.tail_block = block;
    scav_queue.recycler   = 0;
    gc_assert(!scav_queue.head_block->count);
}

void execute_full_mark_phase()
{
#if HAVE_GETRUSAGE
    struct rusage before, after;
    getrusage(RUSAGE_SELF, &before);
#endif
    trace_object((lispobj*)NIL_SYMBOL_SLOTS_START);
    lispobj* where = (lispobj*)STATIC_SPACE_OBJECTS_START;
    lispobj* end = static_space_free_pointer;
    while (where < end) {
        lispobj obj = compute_lispobj(where);
        gc_enqueue(obj);
        where += listp(obj) ? 2 : sizetab[widetag_of(where)](where);
    }
#ifdef LISP_FEATURE_METASPACE
    where = (lispobj*)METASPACE_START;
    end = (lispobj*)READ_ONLY_SPACE_END;
    while (where < end) {
        lispobj obj = compute_lispobj(where);
        gc_enqueue(obj);
        where += listp(obj) ? 2 : sizetab[widetag_of(where)](where);
    }
#endif
    gc_mark_obj(lisp_package_vector);
    do {
        lispobj ptr = gc_dequeue();
        gc_dcheck(ptr != 0);
        if (!listp(ptr))
            trace_object(native_pointer(ptr));
        else
            mark_pair((lispobj*)(ptr - LIST_POINTER_LOWTAG));
    } while (scav_queue.head_block->count ||
             (test_weak_triggers(pointer_survived_gc_yet, gc_mark_obj) &&
              scav_queue.head_block->count));

#if HAVE_GETRUSAGE
    getrusage(RUSAGE_SELF, &after);
#define timediff(b,a,field) \
    (double)((a.field.tv_sec-b.field.tv_sec)*1000000 + \
             (a.field.tv_usec-b.field.tv_usec)) / 1000000.0
    if (gencgc_verbose)
        fprintf(stderr,
                "[Mark phase: %d pages used, HT-count=%d, ET=%f+%f sys+usr]\n",
                (int)(page_table_pages - free_page), mark_bits.count,
                timediff(before, after, ru_stime), timediff(before, after, ru_utime));
#endif
}

static void local_smash_weak_pointers()
{
    struct weak_pointer *wp, *next_wp;
    for (wp = weak_pointer_chain; wp != WEAK_POINTER_CHAIN_END; wp = next_wp) {
        gc_assert(widetag_of(&wp->header) == WEAK_POINTER_WIDETAG);
        next_wp = wp->next;
        wp->next = NULL;
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

__attribute__((unused)) static char *fillerp(lispobj* where)
{
    page_index_t page;
    if (where[0] | where[1])
        return "cons";
    if ((page = find_page_index(where)) >= 0 && page_single_obj_p(page))
        return "cons (largeobj filler)";
    return "cons (filler)";
}

static FILE *sweeplog;
static int sweep_mode = 1;

# define NOTE_GARBAGE(gen,addr,nwords,tally,erase) \
  { tally[gen] += nwords; \
    if (sweep_mode & 2) /* print before erasing */ \
     fprintf(sweeplog, "%5d %d #x%"OBJ_FMTX": %"OBJ_FMTX" %"OBJ_FMTX"\n", \
             (int)nwords, gen, compute_lispobj(addr), \
             addr[0], addr[1]); \
    if (sweep_mode & 1) { erase; } }

#ifndef LISP_FEATURE_IMMOBILE_SPACE
#undef immobile_obj_gen_bits
#define immobile_obj_gen_bits(x) (lose("No page index?"),0)
#else
static void sweep_fixedobj_pages(long *zeroed)
{
    low_page_index_t page;

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
            uword_t markbit = (header_widetag(header) == FDEFN_WIDETAG) ? FDEFN_MARK_BIT : MARK_BIT;
            if (fixnump(header)) { // is a hole
            } else if (header & markbit) { // live object
                *obj = header ^ markbit;
            } else {
                NOTE_GARBAGE(immobile_obj_gen_bits(obj), obj, nwords, zeroed,
                             memset(obj, 0, nwords * N_WORD_BYTES));
            }
        }
    }
    // The reserved fixedobj page has the vector of primitive object layouts.
    lispobj* obj = fixedobj_page_address(0);
    lispobj* limit = fixedobj_page_address(FIXEDOBJ_RESERVED_PAGES);
    while (obj < limit) {
        lispobj header = *obj;
        uword_t markbit = (header_widetag(header) == FDEFN_WIDETAG) ? FDEFN_MARK_BIT : MARK_BIT;
        if (header & markbit) *obj = header ^ markbit;
        obj += sizetab[widetag_of(obj)](obj);
    }
}
#endif

static uword_t sweep(lispobj* where, lispobj* end, uword_t arg)
{
    long *zeroed = (long*)arg; // one count per generation
    sword_t nwords;

    // TODO: consecutive dead objects on same page should be merged.
    for ( ; where < end ; where += nwords ) {
        lispobj word = *where;
        if (is_header(word)) {
            nwords = sizetab[header_widetag(word)](where);
            lispobj markbit = MARK_BIT;
            switch (header_widetag(word)) {
            case BIGNUM_WIDETAG: markbit = BIGNUM_MARK_BIT; break;
            case FDEFN_WIDETAG : markbit = FDEFN_MARK_BIT; break;
            }
            if (word & markbit)
                *where = word ^ markbit;
            else {
                // Turn the object into either a (0 . 0) cons
                // or an unboxed filler depending on size.
                if (nwords <= 2) // could be SAP, SIMPLE-ARRAY-NIL, 1-word bignum, etc
                    goto cons;
                struct code* code  = (struct code*)where;
                // Keep in sync with the definition of filler_obj_p()
                if (!filler_obj_p((lispobj*)code)) {
                    page_index_t page = find_page_index(where);
                    int gen = page >= 0 ? page_table[page].gen
                      : immobile_obj_gen_bits(where);
                    NOTE_GARBAGE(gen, where, nwords, zeroed, {
                        code->boxed_size = 0;
                        code->header = (nwords << CODE_HEADER_SIZE_SHIFT)
                                     | CODE_HEADER_WIDETAG;
                        /* This memset() is needed for strictly boxed pages,
                         * but probably not for other page types. No big deal.
                         * fullcgc isn't really used much */
                        memset(where+2, 0, (nwords - 2) * N_WORD_BYTES);
                    })
                }
            }
        } else {
            nwords = 2;
            if (!cons_markedp((lispobj)where)) {
                if (where[0] | where[1]) {
               cons:
                    gc_dcheck(!immobile_space_p((lispobj)where));
                    NOTE_GARBAGE(page_table[find_page_index(where)].gen,
                                 where, 2, zeroed,
                                 where[0] = where[1] = 0);
                }
            }
        }
    }
    return 0;
}

// sweep_mode: 1 = erase, 2 = print, 3 = both
void toggle_print_garbage(char *filename, int enable)
{
    if (enable) {
        if (sweeplog) {
          fprintf(stderr,"Erasing previous sweep log file\n");
          fclose(sweeplog);
        }
        sweeplog = fopen(filename, "w");
        sweep_mode = enable < 0 ? 2 : 3;
        fprintf(stderr, "Set sweep mode to %d\n", sweep_mode);
    } else {
        fclose(sweeplog);
        fprintf(stderr, "Sweep log closed\n");
        sweep_mode = 1;
    }
}

void execute_full_sweep_phase()
{
    long words_zeroed[1+PSEUDO_STATIC_GENERATION]; // One count per generation

    local_smash_weak_pointers();
    gc_dispose_private_pages();
    cull_weak_hash_tables(alivep_funs);

    memset(words_zeroed, 0, sizeof words_zeroed);
#ifdef LISP_FEATURE_IMMOBILE_SPACE
    if (sweeplog) fprintf(sweeplog, "-- fixedobj space --\n");
    sweep_fixedobj_pages(words_zeroed);
    if (sweeplog) fprintf(sweeplog, "-- varyobj space --\n");
    sweep((lispobj*)VARYOBJ_SPACE_START, varyobj_free_pointer,
          (uword_t)words_zeroed);
#endif
    if (sweeplog) fprintf(sweeplog, "-- dynamic space --\n");
    walk_generation(sweep, -1, (uword_t)words_zeroed);
    if (gencgc_verbose) {
        fprintf(stderr, "[Sweep phase: ");
        int i;
        for(i=6;i>=0;--i)
            fprintf(stderr, "%ld%s", words_zeroed[i], i?"+":"");
        fprintf(stderr, " words zeroed]\n");
    }
    hopscotch_destroy(&mark_bits);
    if (sweeplog)
        fflush(sweeplog);

    free_page = next_free_page;
    while (free_page < page_table_pages) {
        page_table[free_page++].type = FREE_PAGE_FLAG;
    }
}
