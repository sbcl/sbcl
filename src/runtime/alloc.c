/*
 * C half of code-component allocator for Lisp with gencgc.
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

#include <stdlib.h>
#include <stdio.h>
#include "gc.h"
#include "pseudo-atomic.h"
#include "genesis/gc-tables.h"
#include "genesis/vector.h"
#include "arch.h" // why is this where funcall2 is declared???
#include "genesis/symbol.h"
#include "code.h"

lispobj* atomic_bump_static_space_free_ptr(int nbytes)
{
    gc_assert((nbytes & LOWTAG_MASK) == 0);
    lispobj* claimed_ptr = static_space_free_pointer;
#ifdef LISP_FEATURE_X86_64
    // Must not clobber some constant data on the final page of static space
    lispobj* limit = static_space_trailer_start;
#else
    lispobj* limit = (lispobj*)STATIC_SPACE_END;
#endif
    do {
        lispobj* new = (lispobj*)((char*)claimed_ptr + nbytes);
        // Fail if space exhausted or bogusly wrapped around
        if (new > limit || new < claimed_ptr) return 0;
        lispobj* actual_old = __sync_val_compare_and_swap(&static_space_free_pointer,
                                                          claimed_ptr, new);
        if (actual_old == claimed_ptr) return claimed_ptr;
        claimed_ptr = actual_old;
    } while (1);
}

// Work space for the deterministic allocation profiler.
// Only supported on x86-64, but the variables are always referenced
// to reduce preprocessor conditionalization.
os_vm_address_t alloc_profile_buffer; // array of counters
static size_t profile_buffer_size;
lispobj alloc_profile_data;           // SIMPLE-VECTOR of <code-component,PC>
// This variable is read from Lisp - see src/compiler/generic/core
int alloc_profiling;              // enabled flag

#ifdef LISP_FEATURE_WIN32
CRITICAL_SECTION alloc_profiler_lock; // threads are mandatory for win32
#elif defined LISP_FEATURE_SB_THREAD
pthread_mutex_t alloc_profiler_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

unsigned int max_alloc_point_counters;

void allocation_profiler_start()
{
    int __attribute__((unused)) ret = mutex_acquire(&alloc_profiler_lock);
    gc_assert(ret);
    if (!alloc_profiling && simple_vector_p(alloc_profile_data)) {
        max_alloc_point_counters = vector_len(VECTOR(alloc_profile_data))/2;
        size_t size = N_WORD_BYTES * max_alloc_point_counters;
        os_vm_address_t old_buffer = 0;
        if (size != profile_buffer_size) {
            profile_buffer_size = size;
            old_buffer = alloc_profile_buffer;
            alloc_profile_buffer = os_allocate(size);
            printf("using %d cells (0x%"OBJ_FMTX" bytes) for profile buffer @ %p\n",
                   max_alloc_point_counters, (lispobj)size, alloc_profile_buffer);
        }
        alloc_profiling = 1;
        int n = 0;
        struct thread* th;
        for_each_thread(th) {
            th->profile_data = (uword_t*)alloc_profile_buffer;
            ++n;
        }
        printf("allocation profiler: %d thread%s\n", n, n>1?"s":"");
        if (old_buffer) {
            // Thread-safely switching buffers would entail lazy reclamation
            // of the old one. Just don't use the interface functions
            // when any thread might be looking at the old buffer.
            printf("WARNING: Unsafely changed alloc profile buffer\n");
            os_deallocate(alloc_profile_buffer, profile_buffer_size);
        }
    } else {
        fprintf(stderr, alloc_profiling ?
                "allocation profiler already started\n" :
                "profile metadata not created\n");
    }
    ret = mutex_release(&alloc_profiler_lock);
    gc_assert(ret);
    fflush(stdout);
}

// This is not exactly threadsafe. Don't try anything fancy.
void allocation_profiler_stop()
{
    int __attribute__((unused)) ret = mutex_acquire(&alloc_profiler_lock);
    gc_assert(ret);
    if (alloc_profiling) {
        alloc_profiling = 0;
        struct thread* th;
        for_each_thread(th) {
            th->profile_data = 0;
        }
    } else {
        fprintf(stderr, "allocation profiler not started\n");
    }
    ret = mutex_release(&alloc_profiler_lock);
    gc_assert(ret);
}

/** slow path allocation from lisp **/

// Code allocation is always serialized
#ifdef LISP_FEATURE_WIN32
CRITICAL_SECTION code_allocator_lock; // threads are mandatory for win32
#elif defined LISP_FEATURE_SB_THREAD
pthread_mutex_t code_allocator_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

int close_region_nfillers;
uword_t close_region_tot_bytes_wasted;

typedef struct { struct alloc_region* r; int type; } close_region_arg;
void sync_close_regions(int block_signals, int options,
                        close_region_arg* a, int count)
{
    sigset_t savedmask;
    __attribute__((unused)) int result;
    if (block_signals) block_blockable_signals(&savedmask);
    if (options & LOCK_CODE_ALLOCATOR) {
        result = mutex_acquire(&code_allocator_lock);
        gc_dcheck(result);
    }
    if (options & LOCK_PAGE_TABLE) acquire_gc_page_table_lock();
    int i;
    for (i=0; i<count; ++i) {
        page_index_t p = find_page_index(a[i].r->start_addr);
        if (p < 0) continue;
#ifndef LISP_FEATURE_MARK_REGION_GC
        /* Potentially use up all remaining bytes in the TLAB before closing.
         * Pages below the alloc_start for the page type cannot possibly be used,
         * but we didn't properly account for that space, which has a bad effect
         * on the GC auto-trigger. Wasting but counting it works better.
         * But (FIXME) - why does it _NOT_ _WORK_ to include the test of 'p<alloc_start' here?
         * With that test in, I still see heap exhaustions, but without the test
         * - so using up the remainder of the TLAB always - we do NOT get exhaustions.
         * It can't be a race, because we're holding the mutex */
        if ((options & CONSUME_REMAINDER) /* && p < get_alloc_start_page(a[i].type) */ ) {
            extern void deposit_filler(char*, char*);
            char* freeptr = a[i].r->free_pointer;
            char* new_end =
                (a[i].type == PAGE_TYPE_CONS) ?
                PTR_ALIGN_DOWN(freeptr, GENCGC_PAGE_BYTES) + CONS_PAGE_USABLE_BYTES
                : PTR_ALIGN_UP(freeptr, GENCGC_PAGE_BYTES);
            // fillers may not be needed. This anticipates non-zero-filed pages though.
            deposit_filler(freeptr, new_end);
            a[i].r->free_pointer = new_end;
            close_region_nfillers++;
            close_region_tot_bytes_wasted += new_end - freeptr;
        }
#endif
        ensure_region_closed(a[i].r, a[i].type);
    }
    if (options & LOCK_PAGE_TABLE) release_gc_page_table_lock();
    if (options & LOCK_CODE_ALLOCATOR) {
        result = mutex_release(&code_allocator_lock);
        gc_dcheck(result);
    }
    if (block_signals) thread_sigmask(SIG_SETMASK, &savedmask, 0);
}


#define N_THREAD_TLABS(array) sizeof array / sizeof (close_region_arg)

/* These two exported "close_x" functions are called from Lisp prior to
 * heap-walking. They must never get interrupted by STOP_FOR_GC while holding
 * either the free page lock or code allocation lock.
 * Normally this is guaranteed by pseudo-atomic, but in the interest of simplicity,
 * these are plain foreign calls without aid of a vop. */
void close_current_thread_tlab() {
    __attribute__((unused)) struct thread *self = get_sb_vm_thread();
    /* If the compiler doesn't use the cons region, neither will alloc_list().
     * i.e. we'll never see the cons region used with PAGE_TYPE_MIXED.
     * Thus the invariants about page type correctness hold when closing */
    close_region_arg argv[] = {
      { THREAD_ALLOC_REGION(self,mixed), PAGE_TYPE_MIXED },
      { THREAD_ALLOC_REGION(self,cons), PAGE_TYPE_CONS },
#ifdef LISP_FEATURE_SB_THREAD
      { THREAD_ALLOC_REGION(self,sys_mixed), PAGE_TYPE_MIXED },
      { THREAD_ALLOC_REGION(self,sys_cons), PAGE_TYPE_CONS }
#endif
    };
    sync_close_regions(1, LOCK_PAGE_TABLE, argv, N_THREAD_TLABS(argv));
}
void close_code_region() {
    close_region_arg argv = { code_region, PAGE_TYPE_CODE };
    sync_close_regions(1, LOCK_PAGE_TABLE|LOCK_CODE_ALLOCATOR, &argv, 1);
}
/* When this is called by unregister_thread() with STOP_FOR_GC blocked,
 * it needs to aquire the page table lock but not the code allocator lock.
 * It is also called at the start of GC to close each non-dead thread's regions,
 * in which case no locks are needed since all other lisp threads are stopped. */
void gc_close_thread_regions(__attribute__((unused)) struct thread* th,
                             int locking) {
    close_region_arg argv[] = {
#if defined LISP_FEATURE_SB_THREAD || defined LISP_FEATURE_X86_64
      { &th->mixed_tlab, PAGE_TYPE_MIXED },
      { &th->cons_tlab, PAGE_TYPE_CONS },
      { &th->sys_mixed_tlab, PAGE_TYPE_MIXED },
      { &th->sys_cons_tlab, PAGE_TYPE_CONS }
#else
      { main_thread_mixed_region, PAGE_TYPE_MIXED },
      { main_thread_cons_region, PAGE_TYPE_CONS },
#endif
    };
    sync_close_regions(0, locking, argv, N_THREAD_TLABS(argv));
}

extern lispobj* lisp_alloc(int, struct alloc_region *, sword_t,
                           int, struct thread *);

/* alloc() and alloc_list() are external interfaces for memory allocation.
 * They allocate to generation 0 and are not called from within the garbage
 * collector as it is only external uses that need the check for heap
 * size (GC trigger) and to disable the interrupts (interrupts are
 * always disabled during a GC). */

#if defined(LISP_FEATURE_SYSTEM_TLABS) || defined(LISP_FEATURE_X86_64)

/* The asm routines have been modified so that alloc() and alloc_list()
 * each receive the size and a single-bit flag selecting the system or user TLAB.
 */
#define DEFINE_LISP_ENTRYPOINT(name, largep, TLAB, page_type) \
NO_SANITIZE_MEMORY lispobj *name(sword_t nbytes, int sys) { \
    struct thread *self = get_sb_vm_thread(); \
    return lisp_alloc(largep | sys, \
                      sys ? &self->sys_##TLAB##_tlab : THREAD_ALLOC_REGION(self,TLAB), \
                      nbytes, page_type, self); }

DEFINE_LISP_ENTRYPOINT(alloc, (nbytes >= LARGE_OBJECT_SIZE), mixed, PAGE_TYPE_MIXED)
DEFINE_LISP_ENTRYPOINT(alloc_list, 0, cons, PAGE_TYPE_CONS)

#else

#define DEFINE_LISP_ENTRYPOINT(name, largep, tlab, page_type) \
NO_SANITIZE_MEMORY lispobj *name(sword_t nbytes) { \
    struct thread *self = get_sb_vm_thread(); \
    return lisp_alloc(largep, THREAD_ALLOC_REGION(self,tlab), nbytes, page_type, self); }

DEFINE_LISP_ENTRYPOINT(alloc, nbytes >= LARGE_OBJECT_SIZE, mixed, PAGE_TYPE_MIXED)
#ifdef LISP_FEATURE_USE_CONS_REGION
// for this variant of alloc_list to work properly, the allocation vops have to know
// when to use the cons_tlab slot. Otherwise we would inadvertently allocate a CONS page
// for the mixed_tlab region, which would cause all kinds of problems.
DEFINE_LISP_ENTRYPOINT(alloc_list, 0, cons, PAGE_TYPE_CONS)
#else
// Lists will get moved to CONS pages when copied.
DEFINE_LISP_ENTRYPOINT(alloc_list, 0, mixed, PAGE_TYPE_MIXED)
#endif

#endif

lispobj alloc_code_object(unsigned total_words, unsigned boxed)
{
    struct thread *th = get_sb_vm_thread();
    // x86-64 uses pseudo-atomic. Others should too, but instead use WITHOUT-GCING
#ifndef LISP_FEATURE_X86_64
    if (read_TLS(GC_INHIBIT, th) == NIL)
        lose("alloc_code_object called with GC enabled.");
#endif

    sword_t nbytes = total_words * N_WORD_BYTES;
    /* Allocations of code are all serialized. We might also acquire
     * free_pages_lock depending on availability of space in the region */
    __attribute__((unused)) int result = mutex_acquire(&code_allocator_lock);
    gc_assert(result);
    struct code *code =
        (void*)lisp_alloc(nbytes >= LARGE_OBJECT_SIZE, code_region, nbytes, PAGE_TYPE_CODE, th);
    result = mutex_release(&code_allocator_lock);
    gc_assert(result);
    THREAD_JIT_WP(0);

    code->header = ((uword_t)total_words << CODE_HEADER_SIZE_SHIFT) | CODE_HEADER_WIDETAG;
    // GC mustn't see uninitialized data. And one word past the boxed words (which holds the
    // count of following words containing absolute jump addresses) must also be pre-zeroed.
    memset((lispobj*)code + 2, 0, (1 + boxed - 2) * N_WORD_BYTES);
    // 'boxed_size' is an untagged word expressing the number of *bytes* in the boxed section
    // (so CODE-INSTRUCTIONS can simply add rather than shift and add).
    code->boxed_size = boxed * N_WORD_BYTES;

    ((lispobj*)code)[total_words-1] = 0; // zeroize the simple-fun table count
    THREAD_JIT_WP(1);
    return make_lispobj(code, OTHER_POINTER_LOWTAG);
}

#ifdef LISP_FEATURE_SYSTEM_TLABS
#define PREPARE_LIST_ALLOCATION() \
    struct alloc_region *region = sys ? &self->sys_cons_tlab : &self->cons_tlab; \
    int partial_request = (self->arena && !sys) ? \
                          nbytes : (char*)region->end_addr - (char*)region->free_pointer; \
    gc_assert(nbytes >= (sword_t)partial_request); \
    if (partial_request == 0) partial_request = CONS_PAGE_USABLE_BYTES
#else /* no system tlabs */
#define PREPARE_LIST_ALLOCATION() \
    struct alloc_region *region = THREAD_ALLOC_REGION(self, cons); \
    int partial_request = (char*)region->end_addr - (char*)region->free_pointer; \
    gc_assert(nbytes > (sword_t)partial_request); \
    if (partial_request == 0) partial_request = CONS_PAGE_USABLE_BYTES
#endif

#ifdef LISP_FEATURE_X86_64
NO_SANITIZE_MEMORY lispobj alloc_funinstance(sword_t nbytes)
{
    struct thread *th = get_sb_vm_thread();
    __attribute__((unused)) int result = mutex_acquire(&code_allocator_lock);
    gc_assert(result);
    void* mem = lisp_alloc(0, code_region, nbytes, PAGE_TYPE_CODE, th);
    result = mutex_release(&code_allocator_lock);
    gc_assert(result);
    memset(mem, 0, nbytes);
    return (lispobj)mem;
}

/* Make a list that couldn't be inline-allocated. Break it up into contiguous
 * blocks of conses not to exceed one GC page each. */
NO_SANITIZE_MEMORY lispobj
make_list(sword_t nelts, lispobj element, int sys) {
    // Technically this overflow handler could permit garbage collection
    // between separate allocation. For now the entire thing is pseudo-atomic.
    struct thread *self = get_sb_vm_thread();
    sword_t nbytes = nelts << WORD_SHIFT; // fixnum input
    PREPARE_LIST_ALLOCATION();
    lispobj result, *tail = &result;
    do {
        if (nbytes < partial_request) partial_request = nbytes;
        struct cons* c = (void*)lisp_alloc(sys, region, partial_request, PAGE_TYPE_CONS, self);
        if (!c) { gc_assert(self->arena); return 0; }
        *tail = make_lispobj((void*)c, LIST_POINTER_LOWTAG);
        int ncells = partial_request >> (1+WORD_SHIFT);
        nbytes -= N_WORD_BYTES * 2 * ncells;
        struct cons* limit = c + ncells;
        while (c < limit) {
            c->car = element; c->cdr = make_lispobj(c+1, LIST_POINTER_LOWTAG);
            ++c;
        }
        tail = &((c-1)->cdr);
        partial_request = CONS_PAGE_USABLE_BYTES;
    } while (nbytes);
    *tail = NIL;
    return result;
}
#endif

/* Convert a &MORE context to a list. Split it up like make_list if we have to */
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
NO_SANITIZE_MEMORY lispobj
listify_rest_arg(sword_t nelts, lispobj* context, int sys) {
    // same comment as above in make_list() applies about the scope of pseudo-atomic
    struct thread *self = get_sb_vm_thread();
    sword_t nbytes = nelts << WORD_SHIFT; // fixnum input
    PREPARE_LIST_ALLOCATION();
    lispobj result, *tail = &result;
    do {
        if (nbytes < partial_request) partial_request = nbytes;
        struct cons* c = (void*)lisp_alloc(sys, region, partial_request, PAGE_TYPE_CONS, self);
        if (!c) { gc_assert(self->arena); return 0; }
        *tail = make_lispobj((void*)c, LIST_POINTER_LOWTAG);
        int ncells = partial_request >> (1+WORD_SHIFT);
        nbytes -= N_WORD_BYTES * 2 * ncells;
        while (ncells--) {
            c->car = *context--;
            c->cdr = make_lispobj(c+1, LIST_POINTER_LOWTAG);
            c++;
        }
        tail = &((c-1)->cdr);
        partial_request = CONS_PAGE_USABLE_BYTES;
    } while (nbytes);
    *tail = NIL;
    return result;
}
#else
/* Let's assume that all the rest of the architectures work similarly.
 * There may be minor variations in how both args get passed */
NO_SANITIZE_MEMORY lispobj listify_rest_arg(lispobj* context, sword_t context_bytes
#ifdef LISP_FEATURE_SYSTEM_TLABS
                                            , int sys
#endif
    )
{
#ifndef LISP_FEATURE_SYSTEM_TLABS
    int sys = 0;
#endif
    // same comment as above in make_list() applies about the scope of pseudo-atomic
    struct thread *self = get_sb_vm_thread();
    sword_t nbytes = context_bytes * CONS_SIZE;
    PREPARE_LIST_ALLOCATION();
    lispobj result, *tail = &result;
    do {
        if (nbytes < partial_request) partial_request = nbytes;
        struct cons* c = (void*)lisp_alloc(sys, region, partial_request, PAGE_TYPE_CONS, self);
        *tail = make_lispobj((void*)c, LIST_POINTER_LOWTAG);
        int ncells = partial_request >> (1+WORD_SHIFT);
        nbytes -= N_WORD_BYTES * 2 * ncells;
        while (ncells--) {
            c->car = *context++;
            c->cdr = make_lispobj(c+1, LIST_POINTER_LOWTAG);
            c++;
        }
        tail = &((c-1)->cdr);
        partial_request = CONS_PAGE_USABLE_BYTES;
    } while (nbytes);
    *tail = NIL;
    return result;
}
#endif

/* Return the average age of the memory in a generation. */
double generation_average_age(generation_index_t gen_index)
{
    struct generation* gen = &generations[gen_index];
    if (gen->bytes_allocated == 0)
        return 0.0;

    return (double)gen->cum_sum_bytes_allocated / (double)gen->bytes_allocated;
}

extern void gc_gen_report_to_file(int filedes, FILE *file);
void write_generation_stats(FILE *file) { gc_gen_report_to_file(-1, file); }

void log_generation_stats(char *logfile, char *header)
{
    if (logfile) {
        FILE * log = fopen(logfile, "a");
        if (log) {
            fprintf(log, "%s\n", header);
            write_generation_stats(log);
            fclose(log);
        } else {
            fprintf(stderr, "Could not open gc logfile: %s\n", logfile);
            fflush(stderr);
        }
    }
}

void print_generation_stats(void) { write_generation_stats(stderr); }

void write_heap_exhaustion_report(FILE *file, long available, long requested,
                             struct thread __attribute__((unused)) *thread)
{
    fprintf(file,
            "Heap exhausted during %s: %ld bytes available, %ld requested.\n",
            gc_active_p ? "garbage collection" : "allocation",
            available,
            requested);
    write_generation_stats(file);
    fprintf(file, "GC control variables:\n");
    fprintf(file, "   *GC-INHIBIT* = %s\n   *GC-PENDING* = %s\n",
            read_TLS(GC_INHIBIT,thread)==NIL ? "false" : "true",
            (read_TLS(GC_PENDING, thread) == LISP_T) ?
            "true" : ((read_TLS(GC_PENDING, thread) == NIL) ?
                      "false" : "in progress"));
#ifdef LISP_FEATURE_SB_THREAD
    fprintf(file, "   *STOP-FOR-GC-PENDING* = %s\n",
            read_TLS(STOP_FOR_GC_PENDING,thread)==NIL ? "false" : "true");
#endif
    fprintf(file, "Collection trigger variables:\n");
    fprintf(file, "   dynamic_space_size = %zd\n   bytes_allocated = %zd\n   auto_gc_trigger = %zd\n   bytes_consed_between_gcs = %zd\n",
            dynamic_space_size,
            bytes_allocated,
            auto_gc_trigger,
            bytes_consed_between_gcs);
}

/** failure reporting **/

char * gc_logfile = NULL;

void report_heap_exhaustion(long available, long requested, struct thread *th)
{
    if (gc_logfile) {
        FILE * log = fopen(gc_logfile, "a");
        if (log) {
            write_heap_exhaustion_report(log, available, requested, th);
            fclose(log);
        } else {
            fprintf(stderr, "Could not open gc logfile: %s\n", gc_logfile);
            fflush(stderr);
        }
    }
    /* Always to stderr as well. */
    write_heap_exhaustion_report(stderr, available, requested, th);
}

void gc_heap_exhausted_error_or_lose (sword_t available, sword_t requested)
{
    struct thread *thread = get_sb_vm_thread();
    /* Write basic information before doing anything else: if we don't
     * call to lisp this is a must, and even if we do there is always
     * the danger that we bounce back here before the error has been
     * handled, or indeed even printed.
     */
    report_heap_exhaustion(available, requested, thread);
    if (gc_active_p || (available == 0)) {
        /* If we are in GC, or totally out of memory there is no way
         * to sanely transfer control to the lisp-side of things.
         */
        lose("Heap exhausted, game over.");
    }
    else {
        release_gc_page_table_lock();
#ifndef LISP_FEATURE_WIN32
        gc_assert(get_pseudo_atomic_atomic(thread));
        clear_pseudo_atomic_atomic(thread);
        if (get_pseudo_atomic_interrupted(thread))
            do_pending_interrupt();
#endif
        /* Another issue is that signalling HEAP-EXHAUSTED error leads
         * to running user code at arbitrary places, even in a
         * WITHOUT-INTERRUPTS which may lead to a deadlock without
         * running out of the heap. So at this point all bets are
         * off. */
        if (read_TLS(INTERRUPTS_ENABLED,thread) == NIL)
            corruption_warning_and_maybe_lose
                ("Signalling HEAP-EXHAUSTED in a WITHOUT-INTERRUPTS.");
        /* available and requested should be double word aligned, thus
           they can passed as fixnums and shifted later. */
        funcall2(StaticSymbolFunction(HEAP_EXHAUSTED_ERROR), available, requested);
        lose("HEAP-EXHAUSTED-ERROR fell through");
    }
}

/* Apparently SIGSTKSZ can be defined differently depending on _GNU_SOURCE.
 * It would be fine if different ways of arriving at the constant produced
 * the same answer, but they dont:
 *   #include <signal.h>
 *   #include <stdio.h>
 *   #include <unistd.h>
 *   int main() { printf("%d %d\n", sysconf (_SC_SIGSTKSZ), SIGSTKSZ); return 0; }
 * Output:
 * 20480 16384
 *
 * As such, it's best that all uses of the preprocessor macro be confined to one place
 * because free_thread_struct explicitly passes a size when freeing.
 */

#ifdef LISP_FEATURE_WIN32
#define ALT_STACK_SIZE 0
#else
#define ALT_STACK_SIZE 32 * SIGSTKSZ
#endif

/* As a helpful reminder of how this calculation arises, the summands should
 * correspond, in the correct order, to the picture below */
#define THREAD_STRUCT_SIZE \
  (THREAD_ALIGNMENT_BYTES + \
   thread_control_stack_size + BINDING_STACK_SIZE + ALIEN_STACK_SIZE + \
   THREAD_CSP_PAGE_SIZE + \
   (THREAD_HEADER_SLOTS*N_WORD_BYTES) + dynamic_values_bytes + \
   sizeof (struct extra_thread_data) + ALT_STACK_SIZE)

/* this is called from any other thread to create the new one, and
 * initialize all parts of it that can be initialized from another
 * thread
 *
 * The allocated memory will be laid out as depicted below.
 * Left-to-right is in order of lowest to highest address:
 *
 *      ______ spaces as obtained from OS
 *     /   ___ aligned_spaces
 *    /   /
 *  (0) (1)       (2)       (3)       (4)    (5)          (6)
 *   |   | CONTROL | BINDING |  ALIEN  | Trap | thread     |          |
 *   |   |  STACK  |  STACK  |  STACK  | page | structure  | altstack |
 *   |...|------------------------------------------------------------|
 *          2MiB       1MiB     1MiB               (*)         (**)
 *
 *  |              Lisp TLS             |   (**) altstack         |
 *  |-----------------------------------|----------|--------------|
 *  | thread + struct + dynamically     |   extra  |   sigstack   |
 *  | header   thread   assigned TLS    |   data   |              |
 *  +---------+-------------------------|----------+--------------|
 *  |         | <--- TLS_SIZE words --> |   ~1kb   | 32*SIGSTKSZ  |
 *            ^ thread base
 *
 *   (1) = control stack start. default size shown
 *   (2) = binding stack start. size = BINDING_STACK_SIZE
 *   (3) = alien stack start.   size = ALIEN_STACK_SIZE
 *   (4) = C safepoint page.    size = BACKEND_PAGE_BYTES or 0
 *   (5) = per_thread_data.     size = (THREAD_HEADER_SLOTS+TLS_SIZE) words
 *   (6) = arbitrarily-sized "extra" data and signal stack.
 *
 *   (0) and (1) may coincide; (4) and (5) may coincide
 *
 *   - Lisp TLS overlaps 'struct thread' so that the first N (~30) words
 *     have preassigned TLS indices.
 *
 *   - "extra" data are not in 'struct thread' because placing them there
 *     makes it tough to calculate addresses in 'struct thread' from Lisp.
 *     (Every 'struct thread' slot has a known size)
 *
 * On sb-safepoint builds one page before the thread base is used for the foreign calls safepoint.
 */

struct thread *
alloc_thread_struct(void* spaces) {
    /* Allocate the thread structure in one fell swoop as there is no way to recover
     * from failing to obtain contiguous memory. Note that the OS may have a smaller
     * alignment granularity than BACKEND_PAGE_BYTES so we may have to adjust the
     * result to make it conform to our guard page alignment requirement. */
    bool is_recycled = 0;
    if (spaces) {
        // If reusing memory from a previously exited thread, start by removing
        // some old junk from the stack. This is imperfect since we only clear a little
        // at the top, but doing so enables diagnosing some garbage-retention issues
        // using a fine-toothed comb. It would not be possible at all to diagnose
        // if any newly started thread could refer a dead thread's heap objects.
        is_recycled = 1;
    } else {
        spaces = os_alloc_gc_space(THREAD_STRUCT_CORE_SPACE_ID, MOVABLE,
                                   NULL, THREAD_STRUCT_SIZE);
        if (!spaces) return NULL;
    }
    /* Aligning up is safe as THREAD_STRUCT_SIZE has
     * THREAD_ALIGNMENT_BYTES padding. */
    char *aligned_spaces = PTR_ALIGN_UP(spaces, THREAD_ALIGNMENT_BYTES);
    char* csp_page = aligned_spaces + thread_control_stack_size +
                     BINDING_STACK_SIZE + ALIEN_STACK_SIZE;

    // Refer to the ASCII art in the block comment above
    struct thread *th = (void*)(csp_page + THREAD_CSP_PAGE_SIZE
                                + THREAD_HEADER_SLOTS*N_WORD_BYTES);

#ifdef LISP_FEATURE_SB_SAFEPOINT
    // Out of caution I'm supposing that the last thread to use this memory
    // might have left this page as read-only. Could it? I have no idea.
    os_protect(csp_page, THREAD_CSP_PAGE_SIZE, OS_VM_PROT_READ|OS_VM_PROT_WRITE);
#endif

#ifdef LISP_FEATURE_SB_THREAD
    memset(th, 0, sizeof *th);
    lispobj* ptr = (lispobj*)(th + 1);
    lispobj* end = (lispobj*)((char*)th + dynamic_values_bytes);
    memset(ptr, NO_TLS_VALUE_MARKER & 0xFF, (char*)end-(char*)ptr);
    th->tls_size = dynamic_values_bytes;
#endif

    __attribute((unused)) lispobj* tls = (lispobj*)th;

    th->os_address = spaces;
    th->control_stack_start = (lispobj*)aligned_spaces;
    th->binding_stack_start=
        (lispobj*)((char*)th->control_stack_start+thread_control_stack_size);
    th->control_stack_end = th->binding_stack_start;

    if (is_recycled) {
#if GENCGC_IS_PRECISE
    /* Clear the entire control stack. Without this I was able to induce a GC failure
     * in a test which hammered on thread creation for hours. The control stack is
     * scavenged before the heap, so a stale word could point to the start (or middle)
     * of an object using a bad lowtag, for whatever object formerly was there.
     * Then a wrong transport function would be called and (if it worked at all) would
     * place a wrongly tagged FP into a word that might not be the base of an object.
     * Assume for simplicity (as is true) that stacks grow upward if GENCGC_IS_PRECISE.
     * This could just call scrub_thread_control_stack but the comment there says that
     * it's a lame algorithm and only mostly right - it stops after (1<<12) words
     * and checks if the next is nonzero, looping again if it isn't.
     * There's no reason not to be exactly right here instead of probably right */
        memset((char*)th->control_stack_start, 0,
               // take off 2 pages because of the soft and hard guard pages
               thread_control_stack_size - 2*os_vm_page_size);
#else
    /* This is a little wasteful of cycles to pre-zero the pthread overhead (which in glibc
     * resides at the highest stack addresses) comprising about 5kb, below which is the lisp
     * stack. We don't need to zeroize above the lisp stack end, but we don't know exactly
     * where that will be.  Zeroizing more than necessary is conservative, and helps ensure
     * that garbage retention from reused stacks does not pose a huge problem. */
        memset((char*)th->control_stack_end - 16384, 0, 16384);
#endif
    }

    th->state_word.control_stack_guard_page_protected = 1;
    th->state_word.alien_stack_guard_page_protected = 1;
    th->state_word.binding_stack_guard_page_protected = 1;
    th->alien_stack_start=
        (lispobj*)((char*)th->binding_stack_start+BINDING_STACK_SIZE);
    set_binding_stack_pointer(th,th->binding_stack_start);
    th->this = th;
    th->os_thread = 0;
    // Once allocated, the allocation profiling buffer sticks around.
    // If present and enabled, assign into the new thread.
    extern int alloc_profiling;
    th->profile_data = (uword_t*)(alloc_profiling ? alloc_profile_buffer : 0);

    struct extra_thread_data *extra_data = thread_extra_data(th);
    // thread_interrupt_data(th) gets cleared by this memset
    memset(extra_data, 0, sizeof *extra_data);

#if THREADS_USING_GCSIGNAL
    os_sem_init(&extra_data->state_sem, 1);
    os_sem_init(&extra_data->state_not_running_sem, 0);
    os_sem_init(&extra_data->state_not_stopped_sem, 0);
#endif
#if defined LISP_FEATURE_UNIX && defined LISP_FEATURE_SB_THREAD
    os_sem_init(&extra_data->sprof_sem, 0);
#endif
    th->sprof_data = 0;

    th->state_word.state = STATE_RUNNING;
    th->state_word.sprof_enable = 0;
    th->state_word.user_thread_p = 1;

    lispobj* alien_stack_end = (lispobj*)((char*)th->alien_stack_start + ALIEN_STACK_SIZE);
#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64
    // Alien-stack-pointer is predecremented upon use
    th->alien_stack_pointer = alien_stack_end;
#else
    // I do not know the convention for alien-stack-pointer
    th->alien_stack_pointer = alien_stack_end - 1;
#endif

#ifdef HAVE_THREAD_PSEUDO_ATOMIC_BITS_SLOT
    memset(&th->pseudo_atomic_bits, 0, sizeof th->pseudo_atomic_bits);
#elif defined LISP_FEATURE_GENERATIONAL
    clear_pseudo_atomic_atomic(th);
    clear_pseudo_atomic_interrupted(th);
#endif

    INIT_THREAD_REGIONS(th);
#ifdef LISP_FEATURE_SB_THREAD
    /* This parallels the same logic in globals.c for the
     * single-threaded foreign_function_call_active, KLUDGE and
     * all. */
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    th->ffcall_active_p = 0;
#elif !defined(LISP_FEATURE_ARM64) // uses control_stack_start
    th->ffcall_active_p = 1;
#endif
#endif

#ifndef LISP_FEATURE_SB_THREAD
    /* the tls-points-into-struct-thread trick is only good for threaded
     * sbcl, because unithread sbcl doesn't have tls.  So, we copy the
     * appropriate values from struct thread here, and make sure that
     * we use the appropriate SymbolValue macros to access any of the
     * variable quantities from the C runtime.  It's not quite OAOOM,
     * it just feels like it */
    SetSymbolValue(BINDING_STACK_START,(lispobj)th->binding_stack_start,th);
    SetSymbolValue(CONTROL_STACK_START,(lispobj)th->control_stack_start,th);
    SetSymbolValue(CONTROL_STACK_END,(lispobj)th->control_stack_end,th);
#if defined(LISP_FEATURE_X86) || defined (LISP_FEATURE_X86_64)
    SetSymbolValue(ALIEN_STACK_POINTER,(lispobj)th->alien_stack_pointer,th);
#endif
#endif
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    access_control_stack_pointer(th)=th->control_stack_start;
    access_control_frame_pointer(th)=0;
#endif

#if defined LISP_FEATURE_PPC64
    /* Storing a 0 into code coverage mark bytes or GC card mark bytes
     * can be done from the low byte of the thread base register.
     * The thread alignment is BACKEND_PAGE_BYTES (from thread.h), but seeing as this is
     * a similar-but-different requirement, it pays to double-check */
    if ((lispobj)th & 0xFF) lose("Thread struct not at least 256-byte-aligned");
#endif

#ifdef LISP_FEATURE_SB_THREAD
// This macro is the same as "write_TLS(sym,val,th)" but can't be spelled thus.
// 'sym' would get substituted prior to token pasting, so you end up with a bad
// token "(*)_tlsindex" because all symbols are #defined to "(*)" so that #ifdef
// remains meaningful to the preprocessor, while use of 'sym' itself yields
// a deliberate syntax error if you try to compile an expression involving it.
#  define INITIALIZE_TLS(sym,val) write_TLS_index(sym##_tlsindex, val, th, _ignored_)
#else
#  define INITIALIZE_TLS(sym,val) SYMBOL(sym)->value = val
#endif
#include "genesis/thread-init.inc"
    th->no_tls_value_marker = NO_TLS_VALUE_MARKER;

#if defined(LISP_FEATURE_WIN32)
    int i;
    for (i = 0; i<NUM_PRIVATE_EVENTS; ++i)
        thread_private_events(th,i) = CreateEvent(NULL,FALSE,FALSE,NULL);
    thread_extra_data(th)->synchronous_io_handle_and_flag = 0;
#endif
    th->stepping = 0;
    return th;
}

void free_thread_struct(struct thread *th)
{
    struct extra_thread_data *extra_data = thread_extra_data(th);
    if (extra_data->arena_savearea) free(extra_data->arena_savearea);
    os_deallocate((os_vm_address_t) th->os_address, THREAD_STRUCT_SIZE);
}

#ifdef LISP_FEATURE_UNIX
/* (Technically, we still allocate an altstack even on Windows.  Since
 * Windows has a contiguous stack with an automatic guard page of
 * user-configurable size instead of an alternative stack though, the
 * SBCL-allocated altstack doesn't actually apply and won't be used.) */
int
on_altstack_p(struct thread *th, void *esp)
{
    void *start = (char *)th+dynamic_values_bytes;
    void *end = (char *)start + 32*SIGSTKSZ;
    return start <= esp && esp < end;
}
#endif
void* calc_altstack_end(struct thread* thread) {
    return (char*)thread->os_address + THREAD_STRUCT_SIZE;
}
