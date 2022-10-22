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

struct arena {
  uword_t header;
#ifndef LISP_FEATURE_COMPACT_INSTANCE_HEADER
  lispobj layout_ptr;
#endif
  char *base;
  uword_t length;
  char *freeptr;
  lispobj cookie; // "random" opaque value
  // You can infer whether an arena is in the global chain based on whether
  // 'next' is 0 or a valid lisp pointer (possibly NIL).
  uword_t link;
};

// Tagged lisp pointer to a 'struct arena' (which is also a lisp DEFSTRUCT)
// The chain terminates with NIL.
lispobj arena_chain;

const int TLAB_NWORDS = 3;

extern void acquire_gc_page_table_lock(), release_gc_page_table_lock();

extern lispobj * component_ptr_from_pc(char *pc);
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
    __attribute__((unused)) lispobj* save = th->arena_savearea;
    struct thread_instance *lispthread = (void*)native_pointer(th->lisp_thread);
    if (!arena) { // finished with the arena
        gc_assert(th->arena); // must have been an arena in use
        lispthread->arena_cookie = // Capture the timestamp of last use (a/k/a cookie)
            ((struct arena*)native_pointer(th->arena))->cookie;
        // Copy the TLABs to the thread's arena save area
        // so that allocation might pick up where it left off.
#if 0
        *save = th->arena;
        memcpy(save+1, &th->mixed_tlab, TLAB_NWORDS);
        memcpy(save+4, &th->cons_tlab, TLAB_NWORDS);
#endif
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
            // fprintf(stderr, "arena %p added to chain\n", arena);
        }
        // Close only the non-system regions
        if (th->mixed_tlab.start_addr) gc_close_region(&th->mixed_tlab, PAGE_TYPE_MIXED);
        if (th->cons_tlab.start_addr) gc_close_region(&th->cons_tlab, PAGE_TYPE_CONS);
        release_gc_page_table_lock();
#if 0
        /* If the last arena that this thread worked with is the same as 'arena'
         * and the cookie in 'arena' matches the thread's cached copy,
         * then restore the TLABS to their prior state. This way we aren't forced to
         * discard memory every time a thread-pool worker dequeues a task.
         * It can usually pick up where it was in the arena, as long as
         * it is acting on the same arena as before */
        if (*save == arena_taggedptr && arena->cookie == lispthread->arena_cookie) {
            // fprintf(stderr, "thread %p: restoring previously claimed arena ptr\n", th);
            memcpy(&th->mixed_tlab, save+1, TLAB_NWORDS);
            memcpy(&th->cons_tlab, save+4, TLAB_NWORDS);
        } else {
            fprintf(stderr, "thread %p: NOT restoring arena ptr\n", th);
        }
        memset(save, 0, sizeof th->arena_savearea);
#endif
    }
    th->arena = arena_taggedptr;
}

static void* claim_new_subrange(struct arena* a, sword_t nbytes, int filler)
{
    char* where = a->freeptr;
    while (1) {
        char* new_freeptr = where + nbytes;
        if (new_freeptr > a->base + a->length) {
            lose("Fatal: arena memory exhausted");
        }
        char* old = __sync_val_compare_and_swap(&a->freeptr, where, new_freeptr);
        if (old == where) break;
        where = old;
    }
    memset(where, filler, nbytes);
    return where;
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
  char *name = 0;
  if (region == &th->sys_cons_tlab) name = "cons";
  else if (region == &th->sys_mixed_tlab) name = "mixed";
  if (name) lose("thread %p: won't arena-allocate system %s TLAB", th, name);

    int avail = (char*)region->end_addr - (char*)region->free_pointer;
    int min_keep = (page_type == PAGE_TYPE_CONS) ? 4*CONS_SIZE*N_WORD_BYTES : 128;
    int filler = (page_type == PAGE_TYPE_CONS) ? 255 : 0;
    if (avail < min_keep) {
        //fprintf(stderr, "refilling region\n");
        int request = 8192;
        region->start_addr = claim_new_subrange((void*)native_pointer(th->arena),
                                                request, filler);
        region->end_addr = (char*)region->start_addr + request;
        region->free_pointer = region->start_addr;
        //fprintf(stderr, "assign new region: %p..%p\n", region->start_addr, region->end_addr);
    }
    return claim_new_subrange((void*)native_pointer(th->arena), nbytes, filler);
}

// theoretically want a mutex guarding the global, but will we do this in real life?
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
            fprintf(stderr, "arena %p has cookie %lx\n", a, a->cookie);
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
            fprintf(stderr, "Arena: scavenging %p..%p\n", (lispobj*)a->base, (lispobj*)a->freeptr);
            heap_scavenge((lispobj*)a->base, (lispobj*)a->freeptr);
            chain = a->link;
        } while (chain != NIL);
    }
}

static struct result {
  struct vector* v;
  int count;
} searchresult;

static int points_to_arena(lispobj ptr) {
    if (!is_lisp_pointer(ptr) || !arena_chain) return 0;
    lispobj chain = arena_chain;
    do {
        struct arena* arena = (void*)INSTANCE(chain);
        if (arena->base <= (char*)ptr && (char*)ptr < arena->freeptr) return 1;
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

static boolean is_arena_structure(lispobj word)
{
    lispobj chain = arena_chain;
    do {
        if (word == chain) return 1;
        struct arena* a = (void*)native_pointer(chain);
        chain = a->link;
    } while (chain != NIL);
    return 0;
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
        if (points_to_arena(word) && !is_arena_structure(word)) {
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
    if (!points_to_arena(ptr) || is_arena_structure(ptr)) return 0;
    if (searchresult.count >= vector_len(searchresult.v)) {
        fprintf(stderr, "WARNING: out of buffer space\n");
    } else {
        int ct = searchresult.count;
        searchresult.v->data[ct] = ptr;
        searchresult.count++;
    }
    return 1;
}
      
// This is newer heap->arena pointer-finder based on fullcgc
// but it doesn't completely work yet.
int find_dynspace_to_arena_ptrs(lispobj result_buffer)
{
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
        if (th == get_sb_vm_thread()) {
          scan_thread_words(&result_buffer, th->control_stack_end, 0, "stack", th, &printed);
        } else {
          int ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX, th));
          if (ici != 1) lose("can't find interrupt context");
          lispobj sp = *os_context_register_addr(nth_interrupt_context(0, th), reg_SP);
          scan_thread_words((lispobj*)sp, th->control_stack_end, 0, "stack", th, &printed);
        }
        scan_thread_words((lispobj*)th->binding_stack_start,
                          (lispobj*)get_binding_stack_pointer(th), 0,
                          "bindings", th,  &printed);
#ifdef LISP_FEATURE_SB_THREAD
        lispobj* from = &th->lisp_thread;
        lispobj* to = (lispobj*)(SymbolValue(FREE_TLS_INDEX,0) + (char*)th);
        scan_thread_words(from,to, 0, "TLS", th, &printed);
#endif
        stray_pointer_source_obj = 0;
    }
    fprintf(stderr, "Checking dynamic space...\n");
    // print heap->arena pointers as a side-effect of marking
    execute_full_mark_phase();
    dispose_markbits();
    gc_start_the_world();
    searchresult.v = 0;
    int result = searchresult.count;
    searchresult.count = 0;
    return result;
}
