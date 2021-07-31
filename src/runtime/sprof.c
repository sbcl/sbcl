#include <signal.h>
#include <stdio.h>
#include <errno.h>
#include "thread.h"
#include "murmur_hash.h"
#include "gc-assert.h"
#include "arch.h" // why is component_ptr_from_pc declared here???
#include "gc-internal.h"
#include "gc.h"
#include "lispregs.h"
#if !defined LISP_FEATURE_X86 && !defined LISP_FEATURE_X86_64
#include "callframe.inc"
#endif
#include "genesis/compiled-debug-fun.h"

#ifdef MEMORY_SANITIZER
#include <sanitizer/msan_interface.h>
#endif

#include <limits.h>
#include <fcntl.h>
#include <unistd.h>
/* Basic approach:
 * each thread allocates a storage for samples (traces) and a hash-table
 * to groups matching samples together. Collisions in the table are resolved
 * by chaining.
 *
 * At each sample collection:
 * 1. gather up to TRACE_BUFFER_LEN program-counter locations as the stack trace
 * 2. if it exceeds MAX_RECORDED_TRACE_LEN locations, condense it to
      the first and last frames with an elision marker in between.
 * 3. compute a hash of the trace
 * 4. if trace is present in hash-table
 *     then increment the count
 *     else
 *       ensure trace has stable PC locations
 *       add or update
 * Each PC location is tentatively recorded as raw PC location.
 * To ensure location stability:
 *    Check whether each raw PC loc is within lisp code and not pseudostatic.
 *    For each which is movable, express the PC as a code serial# and offset.
 */

#define ELEMENT_SIZE               8

#define TRACE_BUFFER_LEN         300
#define MAX_RECORDED_TRACE_LEN    62
#define N_BUCKETS 0x10000
#define HASH_MASK (N_BUCKETS-1)
#ifdef LISP_FEATURE_64_BIT
  // Disable sampling if buffer has grown to 8 million elements, which is 64MiB.
  // This is enough to store 128k traces if each trace has its maximum of 62 frames
  // which most of them won't. ((62+2) * 8 * 128 * 1024) = 64MiB
#  define CAPACITY_MAX 8*1024*1024
#else
  // Disable sampling if buffer has grown to 1 million elements, which is 8MiB.
#  define CAPACITY_MAX 1024*1024
#endif
#define INITIAL_FREE_POINTER 2

// Lisp and C both accesses this.
// The structure is 2 elements long, each element being 8 bytes.
struct sprof_data {
    // Element 0
    uint32_t *buckets; // power-of-2-sized bucket array
#ifndef LISP_FEATURE_64_BIT
    uint32_t padding0;
#endif
    // Element 1
    // index into next available element of trace_buffer.
    // elements are always 8 bytes regardless of machine word size.
    uint32_t free_pointer;
    uint32_t capacity;
};

// Number of elements that are not part of the locs[] array.
// An 'element' is 8 bytes.
#define TRACE_PREFIX_ELEMENTS 2
struct trace {
    uint32_t next; // next trace with identical hash
    uint32_t multiplicity; // number of times hit
#ifdef LISP_FEATURE_64_BIT
#define trace_len(trace) ((int32_t)((trace)->header))
    uword_t header; // upper 32 bits are hash val, lower 32 are length
    // each entry has the most-significant-bit on if the value represents
    // <code-serial#, pc-offset> in  the high and low 4 byte.
    // If it's just an address, the uppermost bit is clear.
    // As far as I know, no architecture will have code as such a large
    // addresses as to make the representation ambiguous.
    uword_t locs[TRACE_BUFFER_LEN];
#else
#define trace_len(trace) trace->len
    sword_t len;
    uword_t hash;
    // One entry is 2 lispwords. If word1 is 0, then word0 is a PC.
    // Otherwise word0 is the code serial# and word1 is the pc offset.
    // This could probably be reduced to 6 bytes per element by using 3 bytes to store
    // a serial# and 3 bytes to store the offset within code.
    // But then there would be access alignment issues to worry about.
    struct loc { uint32_t word0, word1; } locs[TRACE_BUFFER_LEN];
#endif
};

static inline struct trace* sprof_data_trace(struct sprof_data* data, uint32_t index) {
    return (void*)((char*)data + index*ELEMENT_SIZE);
}

static int in_stack_range(uword_t pc, struct thread* thread)
{
    return pc >= (uword_t)thread->control_stack_start
        && pc < (uword_t)thread->control_stack_end;
}

// Use the WORD-MIX algorithm from src/code/string-hash
static inline uword_t word_mix(uword_t x, uword_t y)
{
    uword_t mul = 3622009729038463111LL & UINT_MAX;
    uword_t xor = 608948948376289905LL & UINT_MAX;
    sword_t xy = x * mul + y;
    return xor ^ xy ^ (xy >> 5);
}

#ifdef LISP_FEATURE_64_BIT
static uint32_t compute_hash(uword_t* elements, int len) {
    int i;
    uword_t hash = len;
    for (i = 0; i < len; i++) {
        uword_t pc = elements[i];
        hash = word_mix(hash, pc);
    }
    return (uint32_t)murmur3_fmix64(hash);
}
#else
static uint32_t compute_hash(struct loc* elements, int len) {
    int i;
    uword_t hash = len;
    for (i = 0; i < len; i++) {
        uword_t pc = elements[i].word0; // this is either a PC or the code serial#
        hash = word_mix(hash, pc);
    }
    return murmur3_fmix32(hash);
}
#endif

static inline void store_trace_header(struct trace* trace, uint32_t hash, uint32_t len)
{
#ifdef LISP_FEATURE_64_BIT
    trace->header = ((uword_t)hash<<32) | len;
#else
    trace->hash = hash;
    trace->len = len;
#endif
}

static int trace_equal(struct trace* a, struct trace* b)
{
    int i;
#ifdef LISP_FEATURE_64_BIT
    if (a->header != b->header) return 0;
    for (i=0; i<trace_len(a); ++i) if (a->locs[i] != b->locs[i]) return 0;
#else
    if (a->hash != b->hash || a->len != b->len) return 0;
    for (i=0; i<trace_len(a); ++i)
        if (a->locs[i].word0 != b->locs[i].word0 ||
            a->locs[i].word1 != b->locs[i].word1) return 0;
#endif
    return 1;
}

static uint32_t* hash_get(struct sprof_data* data, struct trace* trace, uint32_t hash)
{
    int index = hash & HASH_MASK;
    uint32_t entry = data->buckets[index];
    while (entry) {
        struct trace* key = sprof_data_trace(data, entry);
        if (trace_equal(key, trace)) return &key->multiplicity;
        entry = key->next;
    }
    return 0;
}

static uint32_t* hash_insert(struct sprof_data* data, struct trace* trace, uint32_t hash)
{
    // allocate a permanent copy. +2 is for the fixed overhead elements
    int n_elements = TRACE_PREFIX_ELEMENTS + trace_len(trace);
    struct trace* copy = sprof_data_trace(data, data->free_pointer);
    memcpy(copy, trace, n_elements * ELEMENT_SIZE);
    copy->multiplicity = 0;
    // insert into chain for this hash
    int index = hash & HASH_MASK;
    copy->next = data->buckets[index];
    data->buckets[index] = data->free_pointer;
    // consume the buffer elements
    data->free_pointer += n_elements;
    return &copy->multiplicity;
}

static int unstable_program_counter_p(uword_t addr)
{
#ifdef LISP_FEATURE_CHENEYGC
    return (DYNAMIC_0_SPACE_START <= addr &&
            addr < DYNAMIC_0_SPACE_START + dynamic_space_size)
        || (DYNAMIC_1_SPACE_START <= addr &&
            addr < DYNAMIC_1_SPACE_START + dynamic_space_size);
#else
    /* I think that it's a reasonable assumption that code in immobile
     * space will not be garbage-collected during the profiling run.
     * Similar issue for fdefns which contain an executable instruction
     * as well as closure-calling trampolines and builtin-trampoline GFs.
     * It might be neat to mark some objects with a bit saying
     * never to move them if they appeared in a trace.
     * When would the bit get cleared though? I don't know */
    page_index_t page = find_page_index((void*)addr);
    return page >= 0 && page_table[page].gen != PSEUDO_STATIC_GENERATION;
#endif
}

#ifdef LISP_FEATURE_64_BIT
#define STORE_PC(tr, indx, val) (tr).locs[indx] = val
#define STORE_REL_PC(tr, indx, ser, offs) \
  (tr).locs[indx] = ((uword_t)1 << 63) | ((offs) << 32) | (ser)
#else
#define STORE_PC(tr, indx, val) (tr).locs[indx].word0 = val; (tr).locs[indx].word1 = 0
#define STORE_REL_PC(tr, indx, ser, offs) \
  (tr).locs[indx].word0 = ser; (tr).locs[indx].word1 = offs
#endif

/* Represent 'trace' using code_serialno + offset for some PC locations.
 * Locations in foreign and pseudo-static code may remain as-is.
 * Return 1 if the trace was affected by stabilizing it. */
static int NO_SANITIZE_MEMORY stabilize(struct trace* trace)
{
    int len = trace_len(trace);
    int changedp = 0;
    int i;
    uword_t pc;
    for(i=0; i<len; ++i) {
#ifdef LISP_FEATURE_64_BIT
        pc = trace->locs[i];
#else
        pc = trace->locs[i].word0;
#endif
        if (unstable_program_counter_p(pc)) {
            struct code *code = (void*)component_ptr_from_pc((char*)pc);
            if (code) {
                STORE_REL_PC(*trace, i, code_serialno(code), (pc - (uword_t)code));
                changedp = 1;
            } else {
                // Can't have any unstable locations in the result
                STORE_PC(*trace, i, (uword_t)-1);
            }
        }
    }
    return changedp;
}

static int NO_SANITIZE_MEMORY
gather_trace_from_context(struct thread* thread, os_context_t* context,
                          struct trace* trace, int limit)
{
    uword_t pc = *os_context_pc_addr(context);
    int len = 1;
    STORE_PC(*trace, 0, pc);
#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64
    uword_t* fp = (uword_t*)os_context_frame_pointer(context);
    uword_t* sp = (uword_t*)*os_context_sp_addr(context);
    if (fp >= sp && fp < thread->control_stack_end) { // plausible frame-pointer
        // TODO: This should be replaced with more sophisticated backtrace routine
        // that understands foreign code compiled without frame pointers.
        // It's no different from what we have now though.
        for(;;) {
            uword_t prev_fp = *fp;
            uword_t prev_pc = fp[1];
#ifdef LISP_FEATURE_64_BIT
            // If this can't possibly be a valid program counter,
            // change it to the "unknown" value.
            if ((sword_t)prev_pc < 0) prev_pc = (sword_t)-1;
#endif
            STORE_PC(*trace, len, prev_pc);
            if (++len == limit) break;
            // Ensure that the next FP and PC are reasonable.
            if (prev_fp <= (uword_t)fp || prev_fp >= (uword_t)thread->control_stack_end
                || in_stack_range(prev_pc, thread)) break;
            fp = (uword_t*)prev_fp;
        }
    }
#else
    if (gc_managed_heap_space_p(pc) && component_ptr_from_pc((void*)pc)) {
        // If the PC was in lisp code, then the frame register is probably correct,
        // and so it's probably the case that 'frame->saved_lra' is a tagged PC
        // in the caller. Unfortunately it is not 100% reliable in a signal handler,
        // so we don't try to walk back more than one frame.
        struct call_frame* frame = (void*)(*os_context_register_addr(context, reg_CFP));
        if (in_stack_range((uword_t)frame, thread)
            && lowtag_of(frame->saved_lra) == OTHER_POINTER_LOWTAG
            && component_ptr_from_pc((void*)frame->saved_lra)) {
            STORE_PC(*trace, len, frame->saved_lra);
            ++len;
        }
    }
#endif
    return len;
}

extern struct compiled_debug_fun*
debug_function_from_pc (struct code* code, void *pc);

static int gather_trace_from_frame(struct thread* thread, uword_t* fp,
                                   struct trace* trace, int limit)
{
    int len = 0;

#if defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64
    if (fp >= thread->control_stack_start && fp < thread->control_stack_end) {
        for(;;) {
            uword_t prev_fp = *fp;
            uword_t prev_pc = fp[1];
#ifdef LISP_FEATURE_64_BIT
            // If this can't possibly be a valid program counter,
            // change it to the "unknown" value.
            if ((sword_t)prev_pc < 0) prev_pc = (sword_t)-1;
#endif
            STORE_PC(*trace, len, prev_pc);
            if (++len == limit) break;
            // Ensure that the next FP and PC are reasonable.
            if (prev_fp <= (uword_t)fp || prev_fp >= (uword_t)thread->control_stack_end
                || in_stack_range(prev_pc, thread)) break;
            fp = (uword_t*)prev_fp;
        }
    }
#else
    struct call_info info;
    memset(&info, 0, sizeof info);
    info.frame = (struct call_frame *)access_control_frame_pointer(thread);
    // Only try to step 2 frames because anything more and we may randomly crash.
    // If you don't believe this claim, then just try inserting a lisp_backtrace(20)
    // into the start of lisp_alloc() and watch that about half the time
    // you get a nice backtrace, and half the time you get a crash.
    if (lisp_frame_previous(thread, &info) && info.code) {
        struct compiled_debug_fun *df;
        df = (void*)debug_function_from_pc((struct code *)info.code,
                                    (void*)((uword_t)info.code + info.pc));
        if (df) {
            STORE_PC(*trace, 0, (uword_t)info.code + info.pc);
            ++len;
            if (lisp_frame_previous(thread, &info) && info.code) {
                STORE_PC(*trace, 0, (uword_t)info.code + info.pc);
                ++len;
            }
        }
    }
#endif
    return len;
}

#define LOCKED_BY_SELF  1
#define LOCKED_BY_OTHER 2
#define LOCK_CONTESTED  (LOCKED_BY_SELF|LOCKED_BY_OTHER)

static void* initialize_sprof_data(struct thread* thread)
{
    void* buckets = (void*)os_allocate(N_BUCKETS * sizeof (uint32_t));
    if (!buckets) return 0;
    int capacity = 128*1024; // arbitrary starting size for traces, 128K elements = 1MiB
    struct sprof_data *data = (void*)os_allocate(capacity * ELEMENT_SIZE);
    if (!data) { os_deallocate(buckets, N_BUCKETS * sizeof (uint32_t)); return 0; }
    data->capacity = capacity;
    data->free_pointer = INITIAL_FREE_POINTER; // next available element
    data->buckets = buckets;
    thread->sprof_data = (lispobj)data;
    return data;
}

static struct sprof_data* enlarge_buffer(struct sprof_data* current,
                                         uint32_t new_capacity)
{
    char * new_buffer = os_allocate(new_capacity * ELEMENT_SIZE);
    memcpy(new_buffer, current, current->free_pointer * ELEMENT_SIZE);
    os_invalidate((void*)current, current->capacity * ELEMENT_SIZE);
    current = (struct sprof_data*)new_buffer;
    current->capacity = new_capacity;
    return current;
}

#define SPROF_LOCK(th) thread_extra_data(th)->sprof_lock

#ifdef LISP_FEATURE_SB_THREAD
/* If this thread acquired an uncontested lock (old == LOCKED_BY_SELF), release it.
 * If this thread didn't acquire the lock (old == 0 or old == 2), do nothing.
 * The only interesting case is LOCK_CONTESTED */
#define RELEASE_LOCK(th) \
  int oldval = __sync_val_compare_and_swap(&SPROF_LOCK(th), LOCKED_BY_SELF, 0); \
  if (oldval == LOCK_CONTESTED) { \
        oldval = __sync_val_compare_and_swap(&SPROF_LOCK(th), LOCK_CONTESTED, LOCKED_BY_OTHER); \
        gc_assert(oldval == LOCK_CONTESTED); \
        os_sem_post(&thread_extra_data(th)->sprof_sem, "sprof"); \
    }
#else
#define RELEASE_LOCK(th) SPROF_LOCK(th) = 0
#endif

int sb_sprof_trace_ct;
int sb_sprof_trace_ct_max;

/* this could get false msan positives because Lisp don't mark stack words as clean
   so anything may appear as unwritten from C depending on whether any C code
   ever marked them. So it was basically down to luck whether this worked or not */
static int NO_SANITIZE_MEMORY
collect_backtrace(struct thread* th, int contextp, void* context_or_fp)
{
    int oldcount = __sync_fetch_and_add(&sb_sprof_trace_ct, 1);
    if (oldcount >= sb_sprof_trace_ct_max) {
        __sync_fetch_and_sub(&sb_sprof_trace_ct, 1);
        return -1; // sample limit exceeded
    }
    struct trace trace;
    int len;
    if (contextp)
        len = gather_trace_from_context(th, context_or_fp, &trace, TRACE_BUFFER_LEN);
    else
        len = gather_trace_from_frame(th, context_or_fp, &trace, TRACE_BUFFER_LEN);
    if (len < 1) return len;
    if (len > MAX_RECORDED_TRACE_LEN) {
        // change excessively long trace to "hot_end ... elision_marker ... cold_end"
        int midpoint = MAX_RECORDED_TRACE_LEN/2;
        int suffix = midpoint-1;
        STORE_PC(trace, midpoint, (uword_t)-1);
        memmove(&trace.locs[midpoint+1], &trace.locs[len-suffix], N_WORD_BYTES*suffix);
        len = MAX_RECORDED_TRACE_LEN;
    }
    // Hash before trying to insert so that potentially the conversion of unstable
    // PCs to stable PCs can be skipped, if there is a hash match.
    uword_t hash = compute_hash(trace.locs, len);
    store_trace_header(&trace, hash, len);

    // Try to acquire the lock
    if (__sync_val_compare_and_swap(&SPROF_LOCK(th), 0, LOCKED_BY_SELF)!=0)
        return -2; // already locked

    struct sprof_data* data = (void*)th->sprof_data;
    if (!data) data = initialize_sprof_data(th);
    uint32_t* pcount;
    if ((pcount = hash_get(data, &trace, hash)) == NULL) {
        if (stabilize(&trace)) { // changed ?
            hash = compute_hash(trace.locs, len); // revise the hash
            store_trace_header(&trace, hash, len);
            pcount = hash_get(data, &trace, hash);
        }
        if (!pcount) { // still not found, insert it
            uint32_t n_elements = TRACE_PREFIX_ELEMENTS + len;
            uint32_t capacity = data->capacity;
            if (data->free_pointer + n_elements > capacity) {
                // If we're at maximum capacity, bail out
                if (capacity == CAPACITY_MAX) return 0;
                // Before enlarging the buffer, check whether anyone is trying
                // to read it; if so, just bail out.
                // This is not to avoid a race - that's taken care of by the
                // cmpxchg - but it's preferable to drop the current sample
                // versus make a bunch more system call while there is a waiter.
                if (SPROF_LOCK(th) & LOCKED_BY_OTHER) return 0;
                data = enlarge_buffer(data, 2*capacity);
                th->sprof_data = (lispobj)data;
            }
            pcount = hash_insert(data, &trace, hash);
        }
    }
    ++*pcount;
    return 1;
}

static void diagnose_failure(struct thread* thread) {
    // MAX-SAMPLES bounds the memory growth within a constant factor for one thread,
    // but if multithreaded, each thread could allocate a buffer and grow it an
    // arbitrary number of times. The automatic disable tries to avoid an explosion
    // in memory consumption.
    struct sprof_data* data = (void*)thread->sprof_data;
    if (data && data->capacity == CAPACITY_MAX) {
        // disable the profiler in this thread
        thread->state_word.sprof_enable = 0;
#ifdef LISP_FEATURE_SB_THREAD
        char msg[100];
        int msglen = sprintf(msg,
                             "WARNING: pthread %p disabled sprof sampler to limit memory use\n",
                             (void*)pthread_self());
        ignore_value(write(2, msg, msglen));
#endif
    }
}

void record_backtrace_from_context(void *context, struct thread* thread) {
    int success = collect_backtrace(thread, 1, context) == 1;
    // Release the lock. This synchronizes with acquire_sprof_data_lock()
    // which atomically adds LOCKED_BY_OTHER to the lock field.
    // If that happens first, then the cmpxchg will fail, and we'll do
    // a sem_post here. If this happens first, then the thread wishing to
    // acquire the data will see that 'old' is 0, and it will be happy.
    RELEASE_LOCK(thread);
    if (!success) diagnose_failure(thread);
}

/* The SIGPROF handler. This used to be deferrable via the can_handle_now_test()
 * check in interrupt.c, which would return false during GC, because Lisp binds
 * *INTERRUPTS-ENABLED* to NIL in the thread which performs the GC; and all other
 * threads are in their stop_for_gc handler which blocks async signals including
 * SIGPROF, as per the sa_mask in the sigaction() call that assigned the handler.
 * But now that SIGPROF is never deferred, we have to be careful around GC.
 * There's a complicated solution and an easy solution. The complicated is to have
 * component_ptr_from_pc() fail safely if called, so that taking a sample is fine
 * provided that all PC locations are pseudo-static - in that case we do not use
 * component_ptr_from_pc() in the signal handler.
 * The easy out is just to drop the sample; so that's what we do, and versus
 * blocking/unblocking SIGPROF in collect_garbage(), it avoids 2 system calls. */
void sigprof_handler(int sig, __attribute__((unused)) siginfo_t* info,
                     void *context)
{
    if (gc_active_p) return; // no mem barrier needed to read this
    int _saved_errno = errno;
    struct thread* thread = get_sb_vm_thread();
    // We can only profile Lisp threads.
    if (thread) {
        if (thread->state_word.sprof_enable)
            record_backtrace_from_context(context, thread);
        else
            // Block further signals it on return from the handler.
            // This won't actually work if there are nested handlers on the stack,
            // but that's OK, we'll just try again to block it if it occurs.
            sigaddset(os_context_sigmask_addr(context), sig);
    }
    errno = _saved_errno;
}

#if !(defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64 || defined LISP_FEATURE_SPARC)
void allocator_record_backtrace(void* frame_ptr, struct thread* thread)
{
    int success = collect_backtrace(thread, 0, frame_ptr) == 1;
    RELEASE_LOCK(thread);
    if (!success) diagnose_failure(thread);
}
#endif

/// Ensuring mutual exclusivity with the SIGPROF handler,
/// return the profiling data for 'thread', or 0 if none.
uword_t acquire_sprof_data(struct thread* thread)
{
    int old = __sync_fetch_and_or(&SPROF_LOCK(thread), LOCKED_BY_OTHER);
#ifdef LISP_FEATURE_SB_THREAD
    if (old != 0) {
        gc_assert(old == LOCKED_BY_SELF); // could not be LOCKED_BY_OTHER
        // A profiled thread will sem_post() on return from its profiling signal handler
        // if it observes that both lock bits were on.
        // This should be called with the thread's interruption mutex held so that the thread
        // whose data are being acquired can't exit and delete its sprof_sem.
        os_sem_wait(&thread_extra_data(thread)->sprof_sem, "sprof");
        gc_assert(SPROF_LOCK(thread) == LOCKED_BY_OTHER);
    }
#else
    gc_assert(old == 0);
#endif
    // sync cas prevents reading before setting the lock
    uword_t retval = __sync_val_compare_and_swap(&thread->sprof_data, 0, 0);
    // if data were allocated, then set the field to 0
    if (retval) {
        __sync_val_compare_and_swap(&thread->sprof_data, retval, 0);
#ifdef MEMORY_SANITIZER
        // Traces were recorded with the sanitizer disabled, so we either need to
        // read the memory from lisp in SAFETY 0 which disables UNINITIALIZED-LOAD-TRAP,
        // or simply mark the memory as clean.
        int freeptr = ((struct sprof_data*)retval)->free_pointer;
        __msan_unpoison((void*)retval, freeptr * ELEMENT_SIZE);
#endif
    }
    __sync_fetch_and_and(&SPROF_LOCK(thread), 0);
    // This this thread owns that thread's data. ('This' and 'that' could be the same)
    return retval;
}
