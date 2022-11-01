#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <inttypes.h>
#include "sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "gc.h"
#include "gc-internal.h"
#include "gc-private.h"
#include "gencgc-private.h"
#include "lispregs.h"
#include "genesis/arena.h"
#include "thread.h" // for mutex_acquire() stub if no pthreads

extern void acquire_gc_page_table_lock(), release_gc_page_table_lock();
extern lispobj * component_ptr_from_pc(char *pc);

// Arena memory block. At least one is associated with each arena,
// and potentially more than one, depending on allowance for growth.
struct arena_memblk {
    char* freeptr;
    char* limit;
    struct arena_memblk* next;
    uword_t padding; // always 0
};

void switch_to_arena(lispobj arena_taggedptr,
                     __attribute__((unused)) lispobj* ra) // return address
{
    struct arena* arena = (void*)native_pointer(arena_taggedptr);
    struct thread* th = get_sb_vm_thread();
#if 0
    struct code* c = (void*)component_ptr_from_pc((void*)ra);
    int id = c ? code_serialno(c) : 0;
    fprintf(stderr, "arena switch %s %p. caller=(PC=%lx ID=#x%x)\n",
            (arena ? "to" : "from"),
            (arena ? arena : (void*)th->arena),
            (uword_t)ra, id);
#endif
    arena_state* savearea = &th->arena_savearea;
    struct thread_instance *lispthread = (void*)native_pointer(th->lisp_thread);
    if (!arena) { // finished with the arena
        gc_assert(th->arena); // must have been an arena in use
        // Compute the number of bytes that could have been used
        int remaining = 0;
        if (th->mixed_tlab.start_addr)
            remaining += (char*)th->mixed_tlab.free_pointer - (char*)th->mixed_tlab.start_addr;
        if (th->cons_tlab.start_addr)
            remaining += (char*)th->cons_tlab.free_pointer - (char*)th->cons_tlab.start_addr;
        struct arena* in_use_arena = (void*)native_pointer(th->arena);
        __sync_fetch_and_add(&in_use_arena->uw_bytes_wasted, remaining);
        lispthread->arena_cookie = // Capture the timestamp of last use (a/k/a cookie)
            ((struct arena*)native_pointer(th->arena))->cookie;
        // Copy the TLABs to the thread's arena save area
        // so that allocation might pick up where it left off.
        savearea->arena = th->arena;
        savearea->mixed = th->mixed_tlab;
        savearea->cons = th->cons_tlab;
        // Indicate that the tlabs have no space remaining.
        gc_set_region_empty(&th->mixed_tlab);
        gc_set_region_empty(&th->cons_tlab);

    } else { // switching from the dynamic space to an arena
        if (th->arena)
            lose("arena error: can't switch from %p to %p", (void*)th->arena, arena);
        acquire_gc_page_table_lock();
        // See if this arena has ever been switched to,
        // and if not, then add it into 'arena_chain'.
        if (!arena->link) {
            arena->link = arena_chain ? arena_chain : NIL;
            arena_chain = arena_taggedptr;
#ifdef LISP_FEATURE_SB_THREAD
            // And ensure that this arena has a mutex for growing it
#ifndef LISP_FEATURE_WIN32
            pthread_mutex_t* arena_mutex = malloc(sizeof (pthread_mutex_t));
            pthread_mutex_init(arena_mutex, 0);
#else
            CRITICAL_SECTION* arena_mutex = malloc(sizeof(CRITICAL_SECTION));
            InitializeCriticalSection(arena_mutex);
#endif
            arena->uw_pthr_mutex = (uword_t)arena_mutex;
#endif
        }
        // Close only the non-system regions
        if (th->mixed_tlab.start_addr) gc_close_region(&th->mixed_tlab, PAGE_TYPE_MIXED);
        if (th->cons_tlab.start_addr) gc_close_region(&th->cons_tlab, PAGE_TYPE_CONS);
        release_gc_page_table_lock();
        /* If the last arena that this thread worked with is the same as 'arena'
         * and the cookie in 'arena' matches the thread's cached copy,
         * then restore the TLABs to their prior state. This way we aren't forced to
         * discard memory every time a thread-pool worker dequeues a task.
         * It can usually pick up where it was in the arena, as long as
         * it is acting on the same arena as before */
        if (savearea->arena == arena_taggedptr && arena->cookie == lispthread->arena_cookie) {
            th->mixed_tlab = savearea->mixed;
            th->cons_tlab = savearea->cons;
        }
        memset(&th->arena_savearea, 0, sizeof th->arena_savearea);
    }
    th->arena = arena_taggedptr;
}

/* lisp defstructs manifested as C structs have all fields as 'unsigned word'
 * but the mutex is a pthread_mutex_t* which is annoying to cast all the time */
#define ARENA_MUTEX_ACQUIRE(a) ignore_value(mutex_acquire((void*)a->uw_pthr_mutex))
#define ARENA_MUTEX_RELEASE(a) ignore_value(mutex_release((void*)a->uw_pthr_mutex))

static void* memblk_claim_subrange(struct arena* a, struct arena_memblk* mem,
                                   sword_t nbytes, int filler)
{
    char* where = mem->freeptr;
    while (1) {
        char* new_freeptr = where + nbytes;
        if (new_freeptr > mem->limit) {
            ARENA_MUTEX_ACQUIRE(a);
            // It's possible somebody already extended this and we're late to notice,
            // due to a->current_block being speculatively loaded w/o the mutex.
            struct arena_memblk* actual_current_block = (void*)a->uw_current_block;
            if (mem != actual_current_block) { // oops - looking at the wrong memblk
                mem = actual_current_block;
                // fall into the mutex release and restart below
            }  else {
                // really add an extension block
                if (a->uw_extension_count == a->uw_max_extensions) { // can't extend further
                    ignore_value(mutex_release((void*)a->uw_pthr_mutex));
                    lose("Fatal: arena memory exhausted");
                }
                char* new_mem = malloc(a->uw_growth_amount);
                if (new_mem == 0) {
                    ignore_value(mutex_release((void*)a->uw_pthr_mutex));
                    lose("Fatal: arena memory exhausted and could not obtain more memory");
                }
                struct arena_memblk* extension= (void*)new_mem;
                extension->freeptr = new_mem + sizeof (struct arena_memblk);
                extension->limit = new_mem + a->uw_growth_amount;
                extension->next = NULL;
                extension->padding = 0;
                a->uw_length += a->uw_growth_amount; // tally up the total length
                mem->next = extension;
                // Other threads can start using the new block already
                a->uw_current_block = (lispobj)extension;
                mem = extension;
            }
            ARENA_MUTEX_RELEASE(a);
            // C compiler should self-tail-call in optimized build
            return memblk_claim_subrange(a, mem, nbytes, filler);
        }
        char* old = __sync_val_compare_and_swap(&mem->freeptr, where, new_freeptr);
        if (old == where) break;
        where = old;
    }
    memset(where, filler, nbytes);
    return where;
}
static void* claim_new_subrange(struct arena* a, sword_t nbytes, int filler) {
    return memblk_claim_subrange(a, (void*)a->uw_current_block, nbytes, filler);
}

lispobj* handle_arena_alloc(struct thread* th, struct alloc_region* region,
                            int page_type, sword_t nbytes)
{
    /* Todo: can we keep this code in Lisp and make it parameterizable?
     * Calling into C has to spill all FPRs which is just too much.
     *  - if the TLAB is for type CONS and has less than K1 (arb) bytes more,
     *    or non-cons and has less than K2 (arb) bytes more,
     *    then discard that whole TLAB and start a new one.
     *  - AND allocate the current request directly from the arena.
     */
    int avail = (char*)region->end_addr - (char*)region->free_pointer;
    int min_keep = (page_type == PAGE_TYPE_CONS) ? 4*CONS_SIZE*N_WORD_BYTES : 128;
    int filler = (page_type == PAGE_TYPE_CONS) ? 255 : 0;
    /* Precondition: free space in the TLAB was not enough to satisfy the request.
     * We want to do exactly one claim_new_subrange operation. There are 2 cases:
     *
     *  1. amount remaining in the TLAB is not significant. Just toss it out.
     *     Refill the TLAB with the default quantum plus the user's actual request.
     *     Then carve out the new object from the beginning of the TLAB.
     *
     *  2. amount remaining in the TLAB is worth keeping.
     *     Don't refill the TLAB; just make the new object as a discrete claim.
     */
    if (avail < min_keep) { // case 1
        long total_request = nbytes + 8192;
        region->start_addr = claim_new_subrange((void*)native_pointer(th->arena),
                                                total_request, filler);
        region->end_addr = (char*)region->start_addr + total_request;
        lispobj* object = region->start_addr;
        region->free_pointer = (char*)region->start_addr + nbytes;
        return object;
    }
    // case 2
    return claim_new_subrange((void*)native_pointer(th->arena), nbytes, filler);
}

long arena_bytes_used(lispobj arena_taggedptr)
{
    long sum = 0;
    struct arena* arena = (void*)native_pointer(arena_taggedptr);
    ARENA_MUTEX_ACQUIRE(arena);
    struct arena_memblk* block = (void*)arena->uw_first_block;
    do {
        sum += block->freeptr - (char*)block;
    } while ((block = block->next) != NULL);
    ARENA_MUTEX_RELEASE(arena);
    return sum;
}
/* Release successor blocks of this arena, keeping only the first */
void arena_release_memblks(lispobj arena_taggedptr)
{
    struct arena* arena = (void*)native_pointer(arena_taggedptr);
    ARENA_MUTEX_ACQUIRE(arena);
    struct arena_memblk* first = (void*)arena->uw_first_block;
    struct arena_memblk* block = first->next;
    while (block) {
        struct arena_memblk* next = block->next;
        free(block);
        block = next;
    }
    arena->uw_current_block = arena->uw_first_block;
    first->next = NULL;
    ARENA_MUTEX_RELEASE(arena);
}

// theoretically want a mutex guarding the global, but will we do this in real life?
// FIXME: need to munmap all blocks and free() the arena-growth mutex
void unlink_gc_arena(lispobj arena) // arena is a tagged pointer
{
    struct arena* prev = 0;
    lispobj current = arena_chain;
    if (!current) return;
    do {
        struct arena* this = (struct arena*)(current-INSTANCE_POINTER_LOWTAG);
        lispobj next = this->link;
        if (arena == current) {
            if (!prev) { // assign into the global var
              // the global never takes on the value NIL though
              arena_chain = (next == NIL) ? 0 : next;
            } else {
              prev->link = next;
            }
            return;
        }
        prev = this;
        current = next;
    } while (arena != NIL);
}

int scavenge_arenas = 1;
void gc_scavenge_arenas()
{
    /* Always scavenge the 'cookie' in each arena since that's a heap object.
     * It has to be, so that threads don't point to an object that can disappear.
     * The cookie could probably be a raw word, but it'd be subject to wraparound. */
    lispobj chain = arena_chain;
    if (chain) {
        do {
            struct arena* a = (void*)native_pointer(chain);
            fprintf(stderr, "arena %p has cookie %"OBJ_FMTX"\n", a, a->cookie);
            scavenge(&a->cookie, 1);
            chain = a->link;
        } while (chain != NIL);
    }
    if (!scavenge_arenas) {
      fprintf(stderr, "GC will NOT scavenge arena contents\n");
      return;
    }
    /*
     * If there are arenas in use, then treat them as roots.
     * TODO: devise a way to avoid scanning all of all arenas.
     */
    // scavenge(&arena_chain, 1); // don't need this. the arena struct isn't on the heap
    chain = arena_chain;
    if (chain) {
        do {
            // Trace all objects below the free pointer
            struct arena* a = (void*)native_pointer(chain);
            struct arena_memblk* block = (void*)a->uw_first_block;
            while (block) {
                // The block is its own lower bound for scavenge.
                // Its first 4 words look like fixnums, so no need to skip 'em.
                fprintf(stderr, "Arena @ %p: scavenging %p..%p\n",
                        a, block, block->freeptr);
                heap_scavenge((lispobj*)block, (lispobj*)block->freeptr);
                block = block->next;
            }
            chain = a->link;
        } while (chain != NIL);
    }
}

static struct result {
  struct vector* v;
  int count;
} searchresult;

static lispobj find_containing_arena(lispobj ptr) {
    if (!is_lisp_pointer(ptr) || !arena_chain) return 0;
    lispobj chain = arena_chain;
    do {
        struct arena* arena = (void*)INSTANCE(chain);
        struct arena_memblk* block = (void*)arena->uw_first_block;
        while (block) {
            if ((lispobj)block <= ptr && (char*)ptr < block->freeptr) return chain;
            block = block->next;
        }
        chain = arena->link;
    } while (chain != NIL);
    return 0;
}

static void add_to_result(struct result* res, lispobj val)
{
    if (res->count >= vector_len(res->v)) {
      fprintf(stderr, "WARNING: out of buffer space\n");
      return;
    }
    res->v->data[res->count] = val;
    ++res->count;
}

static lispobj target_arena;
static inline boolean interesting_arena_pointer_p(lispobj ptr)
{
    lispobj arena = find_containing_arena(ptr);
    if (!arena) return 0; // uninteresting
    // If 'ptr' is exactly to some arena _regardless_ of which arena
    // we're actually interested in, then 'ptr' is not interesting.
    if (ptr == arena) return 0;
    // If the arena we're interested in ('target_arena') is not the arena to which
    // 'ptr' points, ignore it. target_arena 0 implies interest in all arenas.
    if (target_arena != 0 && target_arena != arena) return 0;
    return 1;
}

static void scan_thread_words(lispobj* start, lispobj* end, struct result* res,
                              __attribute__((unused)) char* legend,
                              struct thread* th, int *printed)
{
    gc_assert(arena_chain);
    int precise = res != NULL;
    lispobj* where = start;
    for ( ; where < end ; ++where) {
        lispobj word = *where;
        if (interesting_arena_pointer_p(word)) {
            if (!*printed) { // print thread identifier once only
              fprintf(stderr, "in thread %p:\n", th);
              *printed = 1;
            }
            /*
            fprintf(stderr, "word @ %p -> %lx %s (%s)\n", where, word, precise?"precise":"ambiguous",
            legend);*/
            if (precise) add_to_result(res, (lispobj)where);
        }
    }
}

extern void gc_stop_the_world(), gc_start_the_world();
extern void prepare_for_full_mark_phase(), execute_full_mark_phase(), dispose_markbits();
extern int (*stray_pointer_detector_fn)(lispobj);
extern lispobj stray_pointer_source_obj;

static int points_to_arena_interior(lispobj ptr) {
    if (!interesting_arena_pointer_p(ptr)) return 0;
    // So this pointer points an arena of interest
    if (searchresult.count >= vector_len(searchresult.v)) {
        fprintf(stderr, "WARNING: out of buffer space\n");
    } else {
        int ct = searchresult.count;
        searchresult.v->data[ct] = stray_pointer_source_obj;
        searchresult.count++;
    }
    return 0; // Returned value does nothing now.
}

int find_dynspace_to_arena_ptrs(lispobj arena, lispobj result_buffer)
{
    target_arena = arena;
    // check for suspcious pointers to arena from thread roots
    searchresult.v = VECTOR(result_buffer);
    stray_pointer_detector_fn = points_to_arena_interior;

    gc_stop_the_world();
    prepare_for_full_mark_phase();
    fprintf(stderr, "Checking threads...\n");
    struct thread* th;
    int printed;
    for_each_thread(th) {
        if (th->state_word.state == STATE_DEAD) continue;
        stray_pointer_source_obj = (lispobj)th;
        printed = 0;
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        if (th == get_sb_vm_thread()) {
          scan_thread_words(&result_buffer, th->control_stack_end, 0, "stack", th, &printed);
        } else {
          int ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX, th));
          if (ici != 1) lose("can't find interrupt context");
          lispobj sp = *os_context_register_addr(nth_interrupt_context(0, th), reg_SP);
          scan_thread_words((lispobj*)sp, th->control_stack_end, 0, "stack", th, &printed);
        }
#endif
        scan_thread_words((lispobj*)th->binding_stack_start,
                          (lispobj*)get_binding_stack_pointer(th), 0,
                          "bindings", th,  &printed);
        lispobj* from = &th->lisp_thread;
        lispobj* to = (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th);
        scan_thread_words(from,to, 0, "TLS", th, &printed);
        stray_pointer_source_obj = 0;
    }
    fprintf(stderr, "Checking dynamic space...\n");
    execute_full_mark_phase();
    dispose_markbits();
    gc_start_the_world();
    searchresult.v = 0;
    int result = searchresult.count;
    searchresult.count = 0;
    target_arena = 0;
    return result;
}
