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
    do {
        lispobj* new = (lispobj*)((char*)claimed_ptr + nbytes);
        // Fail if space exhausted or bogusly wrapped around
        if (new > (lispobj*)STATIC_SPACE_END || new < claimed_ptr) return 0;
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

// The asm routines have been modified so that alloc() and alloc_list()
// each receive the size an a single-bit flag affecting locality of the result.
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
    // 'boxed_size' is an untagged word expressing the number of *bytes* in the boxed section
    // (so CODE-INSTRUCTIONS can simply add rather than shift and add).
    code->boxed_size = boxed * N_WORD_BYTES;
    code->debug_info = 0;
    code->fixups = 0;
    lispobj* p = &code->constants[0], *end = (lispobj*)code + boxed;
    /* Must intialize to an arbitrary non-pointer value so that GC doesn't crash after the
     * size is assigned (at some point prior to storing the constants) */
    for ( ; p < end ; ++p) *p = 0;
    *p = 0; // 'p' now points to the jump table count word which must be 0
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
make_list(lispobj element, sword_t nbytes, int sys) {
    // Technically this overflow handler could permit garbage collection
    // between separate allocation. For now the entire thing is pseudo-atomic.
    struct thread *self = get_sb_vm_thread();
    PREPARE_LIST_ALLOCATION();
    lispobj result, *tail = &result;
    do {
        if (nbytes < partial_request) partial_request = nbytes;
        struct cons* c = (void*)lisp_alloc(sys, region, partial_request, PAGE_TYPE_CONS, self);
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
listify_rest_arg(lispobj* context, sword_t nbytes, int sys) {
    // same comment as above in make_list() applies about the scope of pseudo-atomic
    struct thread *self = get_sb_vm_thread();
    PREPARE_LIST_ALLOCATION();
    lispobj result, *tail = &result;
    do {
        if (nbytes < partial_request) partial_request = nbytes;
        struct cons* c = (void*)lisp_alloc(sys, region, partial_request, PAGE_TYPE_CONS, self);
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
