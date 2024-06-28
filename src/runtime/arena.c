#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <inttypes.h>
#include "genesis/sbcl.h"
#include "runtime.h"
#include "globals.h"
#include "gc.h"
#include "lispregs.h"
#include "genesis/arena.h"
#include "genesis/gc-tables.h"
#include "thread.h"
#include "genesis/instance.h"
#include "graphvisit.h"

extern lispobj * component_ptr_from_pc(char *pc);

// Arena memory block. At least one is associated with each arena,
// and potentially more than one, depending on allowance for growth.
struct arena_memblk {
    char* freeptr;
    char* limit;
    struct arena_memblk* next;
    // The first memblk contains a 'struct arena' and a pthread mutex
    // and the arena_memblk itself. Other memblks only have the memblk.
    // Either way, Lisp objects commence at the 'allocator_base'.
    char* allocator_base;
};

#if 1
# define ARENA_GET_OS_MEMORY(size) malloc(size)
# define ARENA_DISPOSE_MEMORY(addr,size) free(addr)
#else
/* The use-case for this is to reliably detect access to deleted arenas
 * instead of "possibly" depending on what free() did.
 * But malloc() + free() generally give you more control over the
 * release rate back to the OS if you use TCMalloc */
void* ARENA_GET_OS_MEMORY(size_t size) {
    void* result = os_allocate(size);
    fprintf(stderr, "os_allocate -> %p\n", result);
    return result;
}
void ARENA_DISPOSE_MEMORY(void* addr, size_t size)
{
    fprintf(stderr, "about to os_deallocate %p + %lx\n", addr, size);
    os_deallocate(addr,size);
}
#endif

#define CHUNK_ALIGN 4096

lispobj sbcl_new_arena(size_t size)
{
    // First 3 objects in the arena:
    //   Arena
    //   Mutex
    //   Memblk
    struct arena* arena = ARENA_GET_OS_MEMORY(size);
    memset(arena, 0, sizeof *arena);
    arena->header = ((sizeof (struct arena) / N_WORD_BYTES) << INSTANCE_LENGTH_SHIFT)
      | INSTANCE_WIDETAG;
    struct arena_memblk* block =
      (void*)((char*)arena + ALIGN_UP(sizeof (struct arena), 2*N_WORD_BYTES));
    // arenas require threads, but the header for the mutex definition
    // might not have been included if #-sb-thread
#ifdef LISP_FEATURE_SB_THREAD
#ifdef LISP_FEATURE_WIN32
    CRITICAL_SECTION *mutex = (void*)block;
    InitializeCriticalSection(mutex);
#else
    pthread_mutex_t* mutex = (void*)block;
    pthread_mutex_init(mutex, 0);
#endif
    block = (void*)((char*)block + ALIGN_UP(sizeof *mutex, 2*N_WORD_BYTES));
#endif
    char* mem_base = (char*)block + sizeof *block;
    // Prevent user allocations from starting at an address that is not
    // a multiple of 4k. In this manner it is possible to mprotect
    // all user allocations instead of having to skip the first batch
    // so that the arena struct always remains accessible.
    char* aligned_mem_base = PTR_ALIGN_UP(mem_base, CHUNK_ALIGN);
    block->freeptr = block->allocator_base = aligned_mem_base;
    char *limit = (char*)arena + size;
    char *aligned_limit = PTR_ALIGN_DOWN(limit, CHUNK_ALIGN);
    block->limit = aligned_limit;
    block->next = NULL;
    arena->uw_original_size = size;
    arena->uw_length = size;
    arena->uw_current_block = arena->uw_first_block = (uword_t)block;
    return make_lispobj(arena, INSTANCE_POINTER_LOWTAG);
}

static __attribute__((unused)) inline void* arena_mutex(struct arena* a) {
    return (void*)((char*)a + ALIGN_UP(sizeof (struct arena), 2*N_WORD_BYTES));
}

#define ARENA_MUTEX_ACQUIRE(a) ignore_value(mutex_acquire(arena_mutex(a)))
#define ARENA_MUTEX_RELEASE(a) ignore_value(mutex_release(arena_mutex(a)))

/* Release successor blocks of this arena, keeping only the first */
/* TODO: in debug mode, assert that no threads are using the arena */
void arena_release_memblks(lispobj arena_taggedptr)
{
    struct arena* arena = (void*)native_pointer(arena_taggedptr);
    // Avoid a race with GC in scavenge_arenas(). We're messing up the chain
    // of blocks and can't have anything else looking at them.
    __attribute__((unused)) lispobj old_hidden
      = __sync_val_compare_and_swap(&arena->hidden, NIL, LISP_T);
    gc_assert(old_hidden == NIL);
    ARENA_MUTEX_ACQUIRE(arena);
    struct arena_memblk* first = (void*)arena->uw_first_block;
    struct arena_memblk* block = first->next;
    while (block) {
        struct arena_memblk* next = block->next;
        ARENA_DISPOSE_MEMORY(block, arena->uw_growth_amount);
        block = next;
    }
    arena->uw_current_block = arena->uw_first_block;
    first->freeptr = first->allocator_base;
    first->next = NULL;
    // Release huge-object blocks
    block = (void*)arena->uw_huge_objects;
    while (block) {
        struct arena_memblk* next = block->next;
        free(block);
        block = next;
    }
    arena->uw_huge_objects = 0;
    arena->uw_length = arena->uw_original_size;
    arena->hidden = NIL;
    ARENA_MUTEX_RELEASE(arena);
}

void sbcl_delete_arena(lispobj arena_taggedptr)
{
    arena_release_memblks(arena_taggedptr);
    struct arena* arena = (void*)native_pointer(arena_taggedptr);
#ifdef LISP_FEATURE_WIN32
    DeleteCriticalSection(arena_mutex(arena));
#elif defined LISP_FEATURE_SB_THREAD
    pthread_mutex_destroy(arena_mutex(arena));
#endif
    if (arena->link) {
        acquire_gc_page_table_lock(); // Page table lock guards the arena chain
        // The usual singly-linked-list deletion algorithm with no initial dummy node.
        if (arena_taggedptr == arena_chain) { // was head of the chain
            lispobj next = arena->link;
            // Arena chain becomes 0 (not NIL) if there are no arenas.
            arena_chain = next == NIL ? 0 : next;
        } else {
            struct arena* prev = (void*)native_pointer(arena_chain);
            while (prev->link != arena_taggedptr) {
                if (!prev->link || prev->link == NIL) lose("Arena chain corrupted");
                prev = (void*)native_pointer(prev->link);
            }
            prev->link = arena->link;
        }
        release_gc_page_table_lock();
    }
    ARENA_DISPOSE_MEMORY(arena, arena->uw_original_size);
}

static page_index_t close_heap_region(struct alloc_region* r, int page_type) {
    page_index_t result = -1;
    if (r->start_addr) {
        result = find_page_index(r->start_addr);
        gc_close_region(r, page_type);
    }
    return result;
}

void switch_to_arena(lispobj arena_taggedptr)
{
    struct arena* arena = (void*)native_pointer(arena_taggedptr);
    struct thread* th = get_sb_vm_thread();
    struct extra_thread_data *extra_data = thread_extra_data(th);
    if (arena) { // switching from the dynamic space to an arena
        if (th->arena)
            lose("arena error: can't switch from %p to %p", (void*)th->arena,
                 (void*)arena_taggedptr);
        // Page table lock guards the arena chain, as well as the page table
        acquire_gc_page_table_lock();
        // See if this arena has ever been switched to,
        // and if not, then add it into 'arena_chain'.
        if (!arena->link) {
            arena->link = arena_chain ? arena_chain : NIL;
            arena_chain = arena_taggedptr;
        }
        // Close only the non-system regions
        extra_data->mixed_page_hint = close_heap_region(&th->mixed_tlab, PAGE_TYPE_MIXED);
        extra_data->cons_page_hint = close_heap_region(&th->cons_tlab, PAGE_TYPE_CONS);
        release_gc_page_table_lock();
#if 0 // this causes a data race, the very thing it's trying to avoid
        int arena_index = fixnum_value(arena->index);
        /* If this thread has potentially used this arena previously, see if
         * the TLAB pointers can be restored based on token validity */
        if (arena_index <= extra_data->arena_count) {
            // Note that arena indices start at 1 so subtract 1 to get an array index
            arena_state* state = &extra_data->arena_savearea[arena_index-1];
            if (state->token == arena->uw_token) { // arena was not rewound since last use
                th->mixed_tlab = state->mixed;
                th->cons_tlab  = state->cons;
            }
            memset(state, 0, sizeof (arena_state));
        }
#endif
    } else { // finished with the arena
        gc_assert(th->arena); // must have been an arena in use
#if 0
        struct arena* old_arena = (void*)native_pointer(th->arena);
        int arena_index = fixnum_value(old_arena->index);
        // Ensure that this thread has enough space in its save area for the arena index.
        if (arena_index > extra_data->arena_count) {
            /* Theoretically using realloc() here might work, but realloc does not zero-fill
             * the new portion, and it has to be zero-filled to avoid seeing random tokens */
            arena_state* new = calloc(arena_index, sizeof (arena_state));
            if (extra_data->arena_count > 0) {
                memcpy(new, extra_data->arena_savearea,
                       extra_data->arena_count * sizeof (arena_state));
                free(extra_data->arena_savearea);
            }
            extra_data->arena_count = arena_index;
            extra_data->arena_savearea = new;
        }
        arena_state* state = &extra_data->arena_savearea[arena_index-1];
        // Copy the TLABs to the thread's arena save area
        state->token = old_arena->uw_token;
        state->mixed = th->mixed_tlab;
        state->cons  = th->cons_tlab;
#endif
        // Indicate that the tlabs have no space remaining.
        gc_set_region_empty(&th->mixed_tlab);
        gc_set_region_empty(&th->cons_tlab);
    }
    th->arena = arena_taggedptr;
}

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
            } else { // potentially extend
                // An object occupying 1/512th or more of an extension is separately allocated.
                int oversized = nbytes > (sword_t)(a->uw_growth_amount >> 9);
                long request = oversized ? nbytes : (sword_t)a->uw_growth_amount;
                if (a->uw_length + request > a->uw_size_limit) { // limit reached
                    ARENA_MUTEX_RELEASE(a);
                    lose("Fatal: won't add arena %s block. Length=%lx request=%lx max=%lx",
                         oversized ? "huge-object" : "extension",
                         (long)a->uw_length, (long)request, (long)a->uw_size_limit);
                }
                // For a huge object, ensure that there will be adequate space after aligning
                // the bounds. Don't worry about it for a regular extension block though
                // because there's no chance that the current request won't fit.
                // Moreover, use malloc() for oversized objects instead of the macro
                // because we don't actually remember the originally requested size
                // to pass to ARENA_DISPOSE_MEMORY in arena_release_memblks
                long actual_request;
                char* new_mem;
                if (oversized) {
                    actual_request = request + 3*CHUNK_ALIGN;
                    new_mem = malloc(actual_request);
                } else {
                    actual_request = request;
                    new_mem = ARENA_GET_OS_MEMORY(actual_request);
                }
                if (new_mem == 0) {
                    ARENA_MUTEX_RELEASE(a);
                    lose("Fatal: arena memory exhausted and could not obtain more memory");
                }
                struct arena_memblk* extension= (void*)new_mem;
                char* mem_base = new_mem + sizeof (struct arena_memblk);
                // Alignment serves the needs of hide/unhide because mprotect()
                // operates only on boundaries as dictated by the OS and/or CPU.
                char* aligned_mem_base = PTR_ALIGN_UP(mem_base, CHUNK_ALIGN);
                extension->freeptr = extension->allocator_base = aligned_mem_base;
                char* limit = new_mem + actual_request;
                char* aligned_limit = PTR_ALIGN_DOWN(limit, CHUNK_ALIGN);
                extension->limit = aligned_limit;
                extension->next = NULL;
                // tally up the total length
                // For huge objects, count only the directly requested space,
                // not the "bookends" (alignment chunks)
                a->uw_length += request;
                if (oversized) {
                    long usable_space = aligned_limit - aligned_mem_base;
                    // This should assert() because it's very bad, but assertions can be
                    // disabled, so explicitly lose() if it happens.
                    if (nbytes > usable_space) lose("alignment glitch");
                    extension->next = (void*)a->uw_huge_objects;
                    a->uw_huge_objects = (uword_t)extension;
                    // A huge object needs to be zero-filled. It probably was, because it
                    // probably just got mmapped() but there's no way to know.
                    memset(aligned_mem_base, 0, nbytes);
                    ARENA_MUTEX_RELEASE(a);
                    return aligned_mem_base;
                }
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
    /* What's the point of a different filler for CONS? Well, once upon a time
     * I figured that an ambiguous pointer having LIST-POINTER-LOWTAG could be excluded
     * if it points to (uword_t)-1 because no valid cons can hold those bits in the CAR.
     * However, mark-region always zero-fills unused lines, and so the compiler takes
     * advantage of that by using a :DWORD operand when storing NIL to the CDR of a
     * fresh cons. Since the two GCs have different behaviors here, as does the compiler,
     * any unused ranges in an arenas must resemble an unused range in the heap */
#ifdef LISP_FEATURE_MARK_REGION_GC
    int filler = 0;
#else
    int filler = (page_type == PAGE_TYPE_CONS) ? 255 : 0;
#endif
    /* Precondition: free space in the TLAB was not enough to satisfy the request.
     * We want to do exactly one claim_new_subrange operation. There are 2 cases:
     *
     *  1. amount remaining in the TLAB is not significant. Just toss it out.
     *     Refill the TLAB with the default quantum plus the user's actual request.
     *     Then carve out the new object from the beginning of the TLAB.
     *     Exception: if 'nbytes' exceeds 64KiB then always treat it as case 2.
     *     This avoids an anomaly in the situation where 'nbytes' so large that it
     *     can not be obtained from the current arena block, but instead uses malloc()
     *     directly. In such case, attempting to refill the TLAB from the same
     *     block is futile.
     *
     *  2. amount remaining in the TLAB is worth keeping.
     *     Don't refill the TLAB; just make the new object as a discrete claim.
     */
    if (avail < min_keep && nbytes <= 65536) { // case 1
        struct arena* in_use_arena = (void*)native_pointer(th->arena);
        __sync_fetch_and_add(&in_use_arena->uw_bytes_wasted, avail);
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
        sum += block->freeptr - block->allocator_base;
    } while ((block = block->next) != NULL);
    for ( block = (void*)arena->uw_huge_objects ; block ; block = block->next) {
        sum += block->limit - block->allocator_base;
    }
    ARENA_MUTEX_RELEASE(arena);
    return sum;
}

int scavenge_arenas = 1;
void gc_scavenge_arenas()
{
    if (!scavenge_arenas) {
      if (gencgc_verbose) fprintf(stderr, "GC will NOT scavenge arena contents\n");
      return;
    }
    lispobj chain = arena_chain;
    /*
     * If there are arenas in use, then treat them as roots.
     * TODO: devise a way to avoid scanning all of all arenas.
     */
    // scavenge(&arena_chain, 1); // don't need this. the arena struct isn't on the heap
    chain = arena_chain;
    if (chain) {
        do {
            // Trace all objects below the free pointer, unless hidden
            struct arena* a = (void*)native_pointer(chain);
            if (a->hidden == NIL) {
                struct arena_memblk* block = (void*)a->uw_first_block;
                do {
                    if (gencgc_verbose)
                        fprintf(stderr, "Arena @ %p: scavenging %p..%p\n",
                                a, block->allocator_base, block->freeptr);
#ifdef LISP_FEATURE_MARK_REGION_GC
                    mr_trace_bump_range((lispobj*)block->allocator_base, (lispobj*)block->freeptr);
#else
                    heap_scavenge((lispobj*)block->allocator_base, (lispobj*)block->freeptr);
#endif
                } while ((block = block->next) != NULL);
                for ( block = (void*)a->uw_huge_objects ; block ; block = block->next ) {
                    lispobj* obj = (lispobj*)block->allocator_base;
#ifdef LISP_FEATURE_MARK_REGION_GC
                    // fprintf(stderr, "arena huge object @ %p: %lx %lx\n", obj, obj[0], obj[1]);
                    // probably could call trace_other_object directly here?
                    lispobj* end = obj + object_size(obj);
                    mr_trace_bump_range(obj, end);
#else
                    lispobj header = *obj;
                    scavtab[header_widetag(header)](obj, header);
#endif
                }
            }
            chain = a->link;
        } while (chain != NIL);
    }
}

static struct result {
  struct vector* v;
  int count;
} searchresult;

lispobj find_containing_arena(lispobj ptr) {
    if (!arena_chain) return 0;
    lispobj chain = arena_chain;
    do {
        struct arena* arena = (void*)INSTANCE(chain);
        struct arena_memblk* block = (void*)arena->uw_first_block;
        do {
            if ((lispobj)block <= ptr && (char*)ptr < block->freeptr) return chain;
        } while ((block = block->next) != NULL);
        chain = arena->link;
    } while (chain != NIL);
    return 0;
}

static lispobj target_arena;
static inline int interesting_arena_pointer_p(lispobj ptr)
{
    lispobj arena = is_lisp_pointer(ptr) ? find_containing_arena(ptr) : 0;
    if (!arena) return 0; // uninteresting
    // If 'ptr' is exactly to some arena _regardless_ of which arena
    // we're actually interested in, then 'ptr' is not interesting.
    if (ptr == arena) return 0;
    // If the arena we're interested in ('target_arena') is not the arena to which
    // 'ptr' points, ignore it. target_arena 0 implies interest in all arenas.
    if (target_arena != 0 && target_arena != arena) return 0;
    return 1;
}

extern void gc_stop_the_world(), gc_start_the_world();
extern void prepare_for_full_mark_phase(), execute_full_mark_phase(), dispose_markbits();
extern int (*stray_pointer_detector_fn)(lispobj);
extern lispobj stray_pointer_source_obj;

static void add_to_result(lispobj val)
{
    if (searchresult.count >= vector_len(searchresult.v)) {
        fprintf(stderr, "WARNING: out of buffer space\n");
    } else {
        int ct = searchresult.count;
        searchresult.v->data[ct] = val;
        searchresult.count++;
    }
}

static int record_if_points_to_arena_interior(lispobj ptr) {
    if (!interesting_arena_pointer_p(ptr)) return 0;
    add_to_result(stray_pointer_source_obj);
    return 0; // Returned value does nothing now.
}

/* Be aware that reading an arbitrary word and interpreting it is somewhat fragile. */
static int valid_arena_obj_ptr_p(lispobj ptr)
{
    if (listp(ptr)) return is_cons_half(ptr);
    return is_header(*native_pointer(ptr));
}

static void __attribute__((unused))
scan_thread_control_stack(lispobj* start, lispobj* end, lispobj lispthread)
{
    lispobj* where = start;
    for ( ; where < end ; ++where) {
        lispobj word = *where;
        if (is_lisp_pointer(word) && interesting_arena_pointer_p(word)
            && valid_arena_obj_ptr_p(word))
            printf("lispthread %p, word @ %p -> %p\n",
                   (void*)lispthread, where, (void*)word);
    }
}

static void scan_thread_words(lispobj* start, lispobj* end)
{
    gc_assert(arena_chain);
    lispobj* where = start;
    for ( ; where < end ; ++where) {
        lispobj word = *where;
        if (interesting_arena_pointer_p(word)) add_to_result((lispobj)where);
    }
}

int find_dynspace_to_arena_ptrs(lispobj arena, lispobj result_buffer)
{
    gc_stop_the_world();
    if (!arena_chain) {
        fprintf(stderr, "No arenas to examine\n");
        gc_start_the_world();
        return 0;
    }
    target_arena = arena;
    // check for suspcious pointers to arena from thread roots
    searchresult.v = VECTOR(result_buffer);
    stray_pointer_detector_fn = record_if_points_to_arena_interior;

    prepare_for_full_mark_phase();
    fprintf(stderr, "Checking threads...\n");
    struct thread* th;
    for_each_thread(th) {
        if (th->state_word.state == STATE_DEAD) continue;
        stray_pointer_source_obj = (lispobj)th;
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        /* This produces false positives, don't return potential pointers, but instead print them.
         * Using the output, you can try to probe the suspect memory with SB-VM:HEXDUMP */
        if (th == get_sb_vm_thread()) {
            scan_thread_control_stack(&arena, // = the approximate stack pointer
                                      th->control_stack_end,
                                      th->lisp_thread);
        } else {
#ifdef LISP_FEATURE_SB_SAFEPOINT
            lispobj *sp = os_get_csp(th);
#else
            int ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX, th));
            if (ici == 0) lose("can't find interrupt context");
            lispobj sp = *os_context_register_addr(nth_interrupt_context(ici-1, th), reg_SP);
#endif
            scan_thread_control_stack((lispobj*)sp, th->control_stack_end, th->lisp_thread);
        }
#endif
        scan_thread_words((lispobj*)th->binding_stack_start,
                          (lispobj*)get_binding_stack_pointer(th));
        lispobj* from = &th->lisp_thread;
        lispobj* to = (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th);
        scan_thread_words(from, to);
    }
    fprintf(stderr, "Checking dynamic space...\n");
    execute_full_mark_phase();
    dispose_markbits();
    gc_start_the_world();
    searchresult.v = 0;
    int result = searchresult.count;
    stray_pointer_detector_fn = 0;
    searchresult.count = 0;
    target_arena = 0;
    return result;
}

static int count_arena_objects(lispobj arena, int *pnchunks)
{
    struct arena* a = (void*)native_pointer(arena);
    struct arena_memblk* blk = (void*)a->uw_first_block;
    int nobjects = 0;
    int nchunks = 0;
    do {
        ++nchunks;
        lispobj* where = (void*)ALIGN_UP(((uword_t)blk + sizeof (struct arena_memblk)),
                                         CHUNK_ALIGN);
        lispobj* limit = (void*)blk->freeptr;
        while (where < limit) {
            if (*where == (uword_t)-1) { // filler
                where += 2;
            } else {
                ++nobjects;
                where += object_size(where);
            }
        }
        blk = blk->next;
    } while (blk);
    *pnchunks = nchunks;
    return nobjects;
}

void arena_mprotect(lispobj arena, int option)
{
#ifndef LISP_FEATURE_WIN32
    if (option) { // protecting
        int nchunks;
        int count = count_arena_objects(arena, &nchunks); // mainly as a sanity-check
        fprintf(stderr, "arena_mprotect %p: %d objects in %d chunk(s)\n",
                (void*)arena, count, nchunks);
    }
    // PROT_EXEC is not needed. Code blobs always go to dynamic or immobile space
    int prot = option ? PROT_NONE : (PROT_READ|PROT_WRITE /*|PROT_EXEC*/);
    struct arena* a = (void*)native_pointer(arena);
    struct arena_memblk* blk = (void*)a->uw_first_block;
    do {
        char* base = PTR_ALIGN_UP((char*)blk + sizeof (struct arena_memblk), CHUNK_ALIGN);
        char* limit = (void*)blk->limit;
        mprotect(base, limit-base, prot);
        // the block itself is not within [base,limit] and so can be read even if prot==PROT_NONE
        blk = blk->next;
    } while (blk);
    // TODO: mprotect huge-object blocks
#endif
}

lispobj arena_find_containing_object(lispobj arena, char* ptr)
{
    struct arena* a = (void*)native_pointer(arena);
    struct arena_memblk* blk = (void*)a->uw_first_block;
    do {
        lispobj* where = (void*)ALIGN_UP(((uword_t)blk + sizeof (struct arena_memblk)),
                                         CHUNK_ALIGN);
        lispobj* limit = (void*)blk->freeptr;
        while (where < limit) {
            if (*where == (uword_t)-1) { // filler
                where += 2;
            } else {
                sword_t objsize = object_size(where);
                if (ptr >= (char*)where && ptr < (char*)where + objsize)
                    return compute_lispobj(where);
                where += objsize;
            }
        }
        blk = blk->next;
    } while (blk);
    return 0;
}

int diagnose_arena_fault(os_context_t* context, char *addr)
{
#ifndef LISP_FEATURE_WIN32
    if (!arena_chain) return 0; // not handled
    lispobj arena = find_containing_arena((lispobj)addr);
    struct thread* th = get_sb_vm_thread();
    struct thread_instance* instance = (void*)native_pointer(th->lisp_thread);
    lispobj name = instance->_name;
    char *c_string = 0;
    if (name != NIL) {
        struct vector* string = (void*)native_pointer(name);
        if (header_widetag(string->header) == SIMPLE_BASE_STRING_WIDETAG)
            c_string = (char*)string->data;
    }
    fprintf(stderr, "trying diagnose_arena_fault(%p,'%s')\n",
            addr, c_string);
    if (!arena) {
        fprintf(stderr, "fault is not in an arena\n");
        fflush(stderr);
        return 0; // not handled
    }
    int hidden = ((struct arena*)native_pointer(arena))->hidden == LISP_T;
    fprintf(stderr, "fault in arena %p [%s]\n", (void*)arena, (hidden ? "HIDDEN" : "visible"));
    fflush(stderr);
    if (!hidden) return 0;
    arena_mprotect(arena, 0); // unprotect it and find the object
    fprintf(stderr, "unprotected OK\n");
    fflush(stderr);
    lispobj obj = arena_find_containing_object(arena, addr);
    if (!obj) {
        fprintf(stderr, "could not find containing lispobj\n");
        fflush(stderr);
        return 0;
    }
    fprintf(stderr, "access of object @ %p\n", (void*)obj);
    fflush(stderr);
    lisp_memory_fault_error(context, addr);
#endif
    return 1;
}

struct visitor {
    lispobj arena;
    long nwords;
};

static void visit(lispobj obj, void* arg) {
    struct visitor* v = arg;
    if (find_containing_arena(obj) == v->arena) v->nwords += object_size(native_pointer(obj));
}
size_t count_arena_live_bytes(lispobj arena) {
    struct hopscotch_table h;
    struct visitor v;
    v.arena = arena;
    v.nwords = 0;
    struct grvisit_context* c =
        visit_heap_from_static_roots(&h, visit, &v);
    hopscotch_destroy(&h);
    free(c);
    return v.nwords * N_WORD_BYTES;
}
