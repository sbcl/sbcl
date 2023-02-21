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
#include "genesis/gc-tables.h"
#include "thread.h"

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
    memset(mem_base, 0xCC, aligned_mem_base - mem_base);
    block->freeptr = aligned_mem_base;
    char *limit = (char*)arena + size;
    char *aligned_limit = PTR_ALIGN_DOWN(limit, CHUNK_ALIGN);
    block->limit = aligned_limit;
    block->next = NULL;
    block->padding = 0;
    arena->uw_original_size = size;
    arena->uw_length = size;
    arena->uw_current_block = arena->uw_first_block = (uword_t)block;
    return make_lispobj(arena, INSTANCE_POINTER_LOWTAG);
}

static inline void* arena_mutex(struct arena* a) {
    return (void*)((char*)a + ALIGN_UP(sizeof (struct arena), 2*N_WORD_BYTES));
}

#define ARENA_MUTEX_ACQUIRE(a) ignore_value(mutex_acquire(arena_mutex(a)))
#define ARENA_MUTEX_RELEASE(a) ignore_value(mutex_release(arena_mutex(a)))

/* Release successor blocks of this arena, keeping only the first */
/* TODO: in debug mode, assert that no threads are using the arena */
void arena_release_memblks(lispobj arena_taggedptr)
{
    struct arena* arena = (void*)native_pointer(arena_taggedptr);
    ARENA_MUTEX_ACQUIRE(arena);
    struct arena_memblk* first = (void*)arena->uw_first_block;
    struct arena_memblk* block = first->next;
    while (block) {
        struct arena_memblk* next = block->next;
        ARENA_DISPOSE_MEMORY(block, arena->uw_growth_amount);
        block = next;
    }
    arena->uw_current_block = arena->uw_first_block;
    char* mem_base = (char*)first + sizeof (struct arena_memblk);
    first->freeptr = PTR_ALIGN_UP(mem_base, CHUNK_ALIGN);
    first->next = NULL;
    arena->uw_extension_count = 0;
    arena->uw_length = arena->uw_original_size;
    ARENA_MUTEX_RELEASE(arena);
}

void AMD64_SYSV_ABI sbcl_delete_arena(lispobj arena_taggedptr)
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

void AMD64_SYSV_ABI switch_to_arena(lispobj arena_taggedptr,
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
    struct extra_thread_data *extra_data = thread_extra_data(th);
    if (arena) { // switching from the dynamic space to an arena
        if (th->arena)
            lose("arena error: can't switch from %p to %p", (void*)th->arena, arena);
        // Page table lock guards the arena chain, as well as the page table
        acquire_gc_page_table_lock();
        // See if this arena has ever been switched to,
        // and if not, then add it into 'arena_chain'.
        if (!arena->link) {
            arena->link = arena_chain ? arena_chain : NIL;
            arena_chain = arena_taggedptr;
        }
        // Close only the non-system regions
        if (th->mixed_tlab.start_addr) gc_close_region(&th->mixed_tlab, PAGE_TYPE_MIXED);
        if (th->cons_tlab.start_addr) gc_close_region(&th->cons_tlab, PAGE_TYPE_CONS);
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
            }  else {
                // really add an extension block
                if (a->uw_extension_count == a->uw_max_extensions) { // can't extend further
                    ARENA_MUTEX_RELEASE(a);
                    lose("Fatal: arena memory exhausted");
                }
                char* new_mem = ARENA_GET_OS_MEMORY(a->uw_growth_amount);
                if (new_mem == 0) {
                    ARENA_MUTEX_RELEASE(a);
                    lose("Fatal: arena memory exhausted and could not obtain more memory");
                }
                struct arena_memblk* extension= (void*)new_mem;
                char* mem_base = new_mem + sizeof (struct arena_memblk);
                char* aligned_mem_base = PTR_ALIGN_UP(mem_base, CHUNK_ALIGN);
                extension->freeptr = aligned_mem_base;
                char* limit = new_mem + a->uw_growth_amount;
                char* aligned_limit = PTR_ALIGN_DOWN(limit, CHUNK_ALIGN);
                extension->limit = aligned_limit;
                extension->next = NULL;
                extension->padding = 0;
                a->uw_length += a->uw_growth_amount; // tally up the total length
                a->uw_extension_count++;
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
        sum += block->freeptr - (char*)block;
    } while ((block = block->next) != NULL);
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
                    // The block is its own lower bound for scavenge.
                    // Its first 4 words look like fixnums, so no need to skip 'em.
                    fprintf(stderr, "Arena @ %p: scavenging %p..%p\n",
                            a, block, block->freeptr);
                    heap_scavenge((lispobj*)block, (lispobj*)block->freeptr);
                } while ((block = block->next) != NULL);
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
static inline boolean interesting_arena_pointer_p(lispobj ptr)
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
    gc_assert(arena_chain);
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
    target_arena = arena;
    // check for suspcious pointers to arena from thread roots
    searchresult.v = VECTOR(result_buffer);
    stray_pointer_detector_fn = record_if_points_to_arena_interior;

    gc_stop_the_world();
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
          int ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX, th));
          if (ici != 1) lose("can't find interrupt context");
          lispobj sp = *os_context_register_addr(nth_interrupt_context(0, th), reg_SP);
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
    lispobj name = instance->name;
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
    return 1;
#endif
}
