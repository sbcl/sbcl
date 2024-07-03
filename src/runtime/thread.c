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

#ifdef __linux__
#define _GNU_SOURCE // for pthread_setname_np()
#endif
#include "genesis/sbcl.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifndef LISP_FEATURE_WIN32
#include <sched.h>
#endif
#include <stddef.h>
#include <errno.h>
#include <sys/types.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/wait.h>
#endif

#include "runtime.h"
#include "validate.h"           /* for BINDING_STACK_SIZE etc */
#include "thread.h"
#include "genesis/thread.h"
#include "arch.h"
#include "target-arch-os.h"
#include "os.h"
#include "globals.h"
#include "genesis/cons.h"
#include "genesis/symbol.h"
#include "genesis/instance.h"
#include "genesis/vector.h"
#include "interr.h"             /* for lose() */
#include "gc.h"
#include "pseudo-atomic.h"
#include "interrupt.h"
#include "lispregs.h"
#include "atomiclog.inc"

#ifdef LISP_FEATURE_SB_THREAD

#if defined LISP_FEATURE_OPENBSD || defined LISP_FEATURE_FREEBSD || defined LISP_FEATURE_DRAGONFLY
#include <pthread_np.h>
#endif

#ifdef LISP_FEATURE_SUNOS
#include <thread.h>
#endif
#endif

int dynamic_values_bytes = 4096 * sizeof(lispobj);  // same for all threads
// exposed to lisp for pthread_create if not C_STACK_IS_CONTROL_STACK
os_vm_size_t thread_alien_stack_size = ALIEN_STACK_SIZE;
struct thread *all_threads;

#ifdef LISP_FEATURE_SB_THREAD

#ifdef LISP_FEATURE_GCC_TLS
__thread struct thread *current_thread;
#elif !defined LISP_FEATURE_WIN32
pthread_key_t current_thread = 0;
#endif

#ifdef LISP_FEATURE_WIN32
CRITICAL_SECTION all_threads_lock;
static CRITICAL_SECTION recyclebin_lock;
static CRITICAL_SECTION in_gc_lock;
#else
pthread_mutex_t all_threads_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t recyclebin_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t in_gc_lock = PTHREAD_MUTEX_INITIALIZER;
#endif

#endif

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
extern lispobj call_into_lisp_first_time(lispobj fun, lispobj *args, int nargs);
#endif

static void
link_thread(struct thread *th)
{
    if (all_threads) all_threads->prev=th;
    th->next=all_threads;
    th->prev=0;
    all_threads=th;
}

#ifdef LISP_FEATURE_SB_THREAD
static void
unlink_thread(struct thread *th)
{
    if (th->prev)
        th->prev->next = th->next;
    else
        all_threads = th->next;
    if (th->next)
        th->next->prev = th->prev;
}

/* Not safe in general, but if your thread names are all
 * simple-base-string and won't move, this is slightly ok */
char* vm_thread_name(struct thread* th)
{
    if (!th) return "non-lisp";
    struct thread_instance *lispthread = (void*)INSTANCE(th->lisp_thread);
    lispobj name = lispthread->_name;
    if (simple_base_string_p(name)) return vector_sap(name);
    return "?";
}

#define get_thread_state(thread) \
 (int)__sync_val_compare_and_swap(&thread->state_word.state, -1, -1)

#ifndef LISP_FEATURE_SB_SAFEPOINT

void
set_thread_state(struct thread *thread,
                 char state,
                 bool signals_already_blocked) // for foreign thread
{
    struct extra_thread_data *semaphores = thread_extra_data(thread);
    int i, waitcount = 0;
    sigset_t old;
    // If we've already masked the blockable signals we can avoid two syscalls here.
    if (!signals_already_blocked)
        block_blockable_signals(&old);
    os_sem_wait(&semaphores->state_sem);
    if (thread->state_word.state != state) {
        if ((STATE_STOPPED==state) ||
            (STATE_DEAD==state)) {
            waitcount = semaphores->state_not_running_waitcount;
            semaphores->state_not_running_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(&semaphores->state_not_running_sem);
        }
        if ((STATE_RUNNING==state) ||
            (STATE_DEAD==state)) {
            waitcount = semaphores->state_not_stopped_waitcount;
            semaphores->state_not_stopped_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(&semaphores->state_not_stopped_sem);
        }
        thread->state_word.state = state;
    }
    os_sem_post(&semaphores->state_sem);
    if (!signals_already_blocked)
        thread_sigmask(SIG_SETMASK, &old, NULL);
}

// Wait until "thread's" state is something other than 'undesired_state'
// and return whatever the new state is.
int thread_wait_until_not(int undesired_state,
                          struct thread *thread)
{
    struct extra_thread_data *semaphores = thread_extra_data(thread);
    sigset_t old;
    os_sem_t *wait_sem;
    block_blockable_signals(&old);
  start:
    os_sem_wait(&semaphores->state_sem);
    /* "The following functions synchronize memory with respect to other threads:
     *  ... pthread_mutex_lock() ... "
     * https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html#tag_04_11
     * But we still have to ensure no compiler reordering.
     */
    int ending_state = get_thread_state(thread);
    if (ending_state == undesired_state) {
        switch (undesired_state) {
        case STATE_RUNNING:
            wait_sem = &semaphores->state_not_running_sem;
            semaphores->state_not_running_waitcount++;
            break;
        case STATE_STOPPED:
            wait_sem = &semaphores->state_not_stopped_sem;
            semaphores->state_not_stopped_waitcount++;
            break;
        default:
            lose("thread_wait_until_not: invalid argument %x", ending_state);
        }
    } else {
        wait_sem = NULL;
    }
    os_sem_post(&semaphores->state_sem);
    if (wait_sem) {
        os_sem_wait(wait_sem);
        goto start;
    }
    thread_sigmask(SIG_SETMASK, &old, NULL);
    return ending_state;
}
#endif /* sb-safepoint */
#endif /* sb-thread */

#ifdef LISP_FEATURE_WIN32
#define sb_GetTID() GetCurrentThreadId()
#elif defined __linux__
// gettid() was added in glibc 2.30 but we support older glibc
int sb_GetTID() { return syscall(SYS_gettid); }
#elif defined __DragonFly__
#include <sys/lwp.h>
lwpid_t sb_GetTID() { return lwp_gettid(); }
#elif defined __FreeBSD__
#include <sys/thr.h>
int sb_GetTID()
{
    long id;
    thr_self(&id);
    // man thr_self(2) says: the thread identifier is an integer in the range
    // from PID_MAX + 2 (100001) to INT_MAX. So casting to int is safe.
    return (int)id;
}
#elif defined __OpenBSD__
int sb_GetTID()
{
    return getthrid();
}
#elif defined __APPLE__ && defined LISP_FEATURE_SB_THREAD
int sb_GetTID() {
    return pthread_mach_thread_np(pthread_self());
}
#else
#define sb_GetTID() 0
#endif

/* Our futex-based lisp mutex needs an OS-assigned unique ID.
 * Why not use pthread_self? I think the reason is that that on linux,
 * the TID is 4 bytes, and the futex lock word is 4 bytes.
 * If the unique ID needed 8 bytes, there could be spurious aliasing
 * that would make the code behave incorrectly. */
static int get_nonzero_tid()
{
    int tid = sb_GetTID();
#ifdef LISP_FEATURE_SB_FUTEX
    // If no futexes, don't need or want to assert that the TID is valid.
    // (macOS etc)
    gc_assert(tid != 0);
#endif
    return tid;
}

// Because creation is synchronized by *MAKE-THREAD-LOCK*
// we only need a single 'attributes' object.
#if defined LISP_FEATURE_SB_THREAD && !defined LISP_FEATURE_WIN32
pthread_attr_t new_lisp_thread_attr;
#define init_shared_attr_object() (pthread_attr_init(&new_lisp_thread_attr)==0)
#else
#define init_shared_attr_object() (1)
#endif
struct thread *alloc_thread_struct(void*);

#ifdef LISP_FEATURE_WIN32
#define ASSOCIATE_OS_THREAD(thread) \
    DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), \
                    GetCurrentProcess(), (LPHANDLE)&thread->os_thread, 0, TRUE, \
                    DUPLICATE_SAME_ACCESS)
#elif defined LISP_FEATURE_GS_SEG
#include <asm/prctl.h>
#include <sys/prctl.h>
extern int arch_prctl(int code, unsigned long *addr);
#define ASSOCIATE_OS_THREAD(thread) arch_prctl(ARCH_SET_GS, (uword_t*)thread), \
      thread->os_thread = thread_self()
#else
#define ASSOCIATE_OS_THREAD(thread) thread->os_thread = thread_self()
#endif

#ifndef LISP_FEATURE_SB_THREAD
# define ASSIGN_CURRENT_THREAD(dummy)
#elif defined LISP_FEATURE_GCC_TLS
# define ASSIGN_CURRENT_THREAD(x) current_thread = x
#elif !defined LISP_FEATURE_WIN32
# define ASSIGN_CURRENT_THREAD(x) pthread_setspecific(current_thread, x)
#else
# define ASSIGN_CURRENT_THREAD(x) TlsSetValue(OUR_TLS_INDEX, x)
#endif

#ifdef LISP_FEATURE_WIN32
// Need a function callable from assembly code, where the inline one won't do.
void* read_current_thread() {
  return get_sb_vm_thread();
}
#endif

#if defined LISP_FEATURE_DARWIN && defined LISP_FEATURE_SB_THREAD
extern pthread_key_t ignore_stop_for_gc;
#endif

#if !defined COLLECT_GC_STATS && !defined STANDALONE_LDB && \
  defined LISP_FEATURE_LINUX && defined LISP_FEATURE_SB_THREAD && defined LISP_FEATURE_64_BIT
#define COLLECT_GC_STATS
#endif
#ifdef COLLECT_GC_STATS
__attribute__((unused)) static struct timespec gc_start_time;
__attribute__((unused)) static long stw_elapsed,
    stw_min_duration = LONG_MAX, stw_max_duration, stw_sum_duration,
    gc_min_duration = LONG_MAX, gc_max_duration, gc_sum_duration;
int show_gc_stats, n_gcs_done;
static void summarize_gc_stats(void) {
    // TODO: also collect things like number of root pages,bytes scanned
    // and number of pages,bytes copied on average per GC cycle.
    if (show_gc_stats && n_gcs_done)
        fprintf(stderr,
                "\nGC: stw_delay=%ld,%ld,%ld \u00B5s (min,avg,max) pause=%ld,%ld,%ld \u00B5s (sum=%ld) over %d GCs\n",
                stw_min_duration/1000, stw_sum_duration/n_gcs_done/1000, stw_max_duration/1000,
                gc_min_duration/1000, gc_sum_duration/n_gcs_done/1000, gc_max_duration/1000,
                gc_sum_duration/1000, n_gcs_done);
}
void reset_gc_stats() { // after sb-posix:fork
    stw_min_duration = LONG_MAX; stw_max_duration = stw_sum_duration = 0;
    gc_min_duration = LONG_MAX; gc_max_duration = gc_sum_duration = 0;
    n_gcs_done = 0;
    show_gc_stats = 1; // won't show if never called reset
}
#endif

#ifdef ATOMIC_LOGGING
#define THREAD_NAME_MAP_MAX 20 /* KLUDGE */
struct {
  pthread_t thread;
  char *name; // strdup'ed
} thread_name_map[THREAD_NAME_MAP_MAX];
int thread_name_map_count;

char* thread_name_from_pthread(pthread_t pointer){
    int i;
    for(i=0; i<thread_name_map_count; ++i)
        if (thread_name_map[i].thread == pointer) return thread_name_map[i].name;
    return 0;
}
#endif

void create_main_lisp_thread(lispobj function) {
#ifdef LISP_FEATURE_WIN32
    InitializeCriticalSection(&all_threads_lock);
    InitializeCriticalSection(&recyclebin_lock);
    InitializeCriticalSection(&in_gc_lock);
#endif
    struct thread *th = alloc_thread_struct(0);
    if (!th || arch_os_thread_init(th)==0 || !init_shared_attr_object())
        lose("can't create initial thread");
    th->state_word.sprof_enable = 1;
#if defined LISP_FEATURE_SB_THREAD && !defined LISP_FEATURE_GCC_TLS && !defined LISP_FEATURE_WIN32
    pthread_key_create(&current_thread, 0);
#endif
#if defined LISP_FEATURE_DARWIN && defined LISP_FEATURE_SB_THREAD
    pthread_key_create(&ignore_stop_for_gc, 0);
#endif
#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    __attribute__((unused)) lispobj *args = NULL;
#endif
    ASSOCIATE_OS_THREAD(th);
    ASSIGN_CURRENT_THREAD(th);
#if defined THREADS_USING_GCSIGNAL && \
    (defined LISP_FEATURE_PPC || defined LISP_FEATURE_PPC64 || defined LISP_FEATURE_ARM64 || defined LISP_FEATURE_RISCV)
    /* SIG_STOP_FOR_GC defaults to blocked on PPC? */
    unblock_gc_stop_signal();
#endif
    link_thread(th);
    th->os_kernel_tid = get_nonzero_tid();

#ifndef LISP_FEATURE_WIN32
    protect_control_stack_hard_guard_page(1, NULL);
#endif
    protect_binding_stack_hard_guard_page(1, NULL);
    protect_alien_stack_hard_guard_page(1, NULL);
#ifndef LISP_FEATURE_WIN32
    protect_control_stack_guard_page(1, NULL);
#endif
    protect_binding_stack_guard_page(1, NULL);
    protect_alien_stack_guard_page(1, NULL);

#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_X86_64)
    set_thread_stack(th->control_stack_end);
#endif

#ifdef COLLECT_GC_STATS
    atexit(summarize_gc_stats);
#endif
    /* WIN32 has a special stack arrangement, calling
     * call_into_lisp_first_time will put the new stack in the middle
     * of the current stack */
#if !(defined(LISP_FEATURE_WIN32) && !defined(OS_THREAD_STACK)) \
    && (defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
    call_into_lisp_first_time(function,args,0);
#else
    funcall0(function);
#endif
    // If we end up returning, clean up the initial thread.
#ifdef LISP_FEATURE_SB_THREAD
    unlink_thread(th);
#else
    all_threads = NULL;
#endif
    arch_os_thread_cleanup(th);
    ASSIGN_CURRENT_THREAD(NULL);
}

#ifdef LISP_FEATURE_SB_THREAD

void free_thread_struct(struct thread *th)
{
    struct extra_thread_data *extra_data = thread_extra_data(th);
    if (extra_data->arena_savearea) free(extra_data->arena_savearea);
    os_deallocate((os_vm_address_t) th->os_address, THREAD_STRUCT_SIZE);
}

/* Note: scribble must be stack-allocated */
static void
init_new_thread(struct thread *th,
                init_thread_data __attribute__((unused)) *scribble,
                int guardp)
{
    ASSIGN_CURRENT_THREAD(th);
    if(arch_os_thread_init(th)==0) {
        /* FIXME: handle error */
        lose("arch_os_thread_init failed");
    }

#define GUARD_CONTROL_STACK 1
#define GUARD_BINDING_STACK 2
#define GUARD_ALIEN_STACK   4

#ifndef LISP_FEATURE_WIN32
    if (guardp & GUARD_CONTROL_STACK)
        protect_control_stack_guard_page(1, NULL);
#endif
    if (guardp & GUARD_BINDING_STACK)
        protect_binding_stack_guard_page(1, NULL);
    if (guardp & GUARD_ALIEN_STACK)
        protect_alien_stack_guard_page(1, NULL);

    /* Since GC can only know about this thread from the all_threads
     * list and we're just adding this thread to it, there is no
     * danger of deadlocking even with SIG_STOP_FOR_GC blocked (which
     * it is not). */
#ifdef LISP_FEATURE_SB_SAFEPOINT
    csp_around_foreign_call(th) = (lispobj)scribble;
#endif
    __attribute__((unused)) int lock_ret = mutex_acquire(&all_threads_lock);
    gc_assert(lock_ret);
    link_thread(th);
    ignore_value(mutex_release(&all_threads_lock));

    /* Kludge: Changed the order of some steps between the safepoint/
     * non-safepoint versions of this code.  Can we unify this more?
     */
#ifdef LISP_FEATURE_SB_SAFEPOINT
    WITH_GC_STATE_LOCK {
        gc_state_wait(GC_NONE);
    }
    push_gcing_safety(&scribble->safety);
#endif
}

lispobj remset_transfer_list;

static void
unregister_thread(struct thread *th,
                  init_thread_data __attribute__((unused)) *scribble)
{
    block_blockable_signals(0);
#ifdef LISP_FEATURE_PERMGEN
    lispobj my_remset = th->remset;
    if (my_remset) {
        lispobj tail = remset_transfer_list;
        while (1) {
            VECTOR(my_remset)->data[1] = tail;
            lispobj actual_old = __sync_val_compare_and_swap(
                &remset_transfer_list, tail, my_remset);
            if (actual_old == tail) break;
            tail = actual_old;
        }
        th->remset = 0;
    }
#endif
    gc_close_thread_regions(th, LOCK_PAGE_TABLE|CONSUME_REMAINDER);
#ifdef LISP_FEATURE_SB_SAFEPOINT
    pop_gcing_safety(&scribble->safety);
#else
    /* This state change serves to "acknowledge" any stop-the-world
     * signal received while the STOP_FOR_GC signal is blocked */
    set_thread_state(th, STATE_DEAD, 1);
#endif
    /* SIG_STOP_FOR_GC is blocked and GC might be waiting for this
     * thread, but since we are either exiting lisp code as a lisp
     * thread that is dying, or exiting lisp code to return to
     * former status as a C thread, it won't wait long. */
    __attribute__((unused)) int lock_ret = mutex_acquire(&all_threads_lock);
    gc_assert(lock_ret);
    unlink_thread(th);
    lock_ret = mutex_release(&all_threads_lock);
    gc_assert(lock_ret);

    arch_os_thread_cleanup(th);

    __attribute__((unused)) struct extra_thread_data *semaphores = thread_extra_data(th);
#ifdef LISP_FEATURE_UNIX
    os_sem_destroy(&semaphores->sprof_sem);
#endif
#ifndef LISP_FEATURE_SB_SAFEPOINT
    os_sem_destroy(&semaphores->state_sem);
    os_sem_destroy(&semaphores->state_not_running_sem);
    os_sem_destroy(&semaphores->state_not_stopped_sem);
#endif

#if defined(LISP_FEATURE_WIN32)
    int i;
    for (i = 0; i<NUM_PRIVATE_EVENTS; ++i)
        CloseHandle(thread_private_events(th,i));
#endif

    /* Undo the association of the current pthread to its `struct thread',
     * such that we can call get_sb_vm_thread() later in this
     * thread and cleanly get back NULL. */
    /* FIXME: what if, after we blocked signals, someone uses INTERRUPT-THREAD
     * on this thread? It's no longer a lisp thread; I suspect the signal
     * will be redirected to a lisp thread.
     * Can anything else go wrong with other signals? Nothing else should
     * direct signals specifically to this thread. Per-process signals are ok
     * because the kernel picks a thread in which a signal isn't blocked */
    ASSIGN_CURRENT_THREAD(NULL);
}

/* this is the first thing that runs in the child (which is why the
 * silly calling convention).  Basically it calls the user's requested
 * lisp function after doing arch_os_thread_init and whatever other
 * bookkeeping needs to be done
 */
#ifdef LISP_FEATURE_WIN32
__stdcall unsigned int new_thread_trampoline(LPVOID arg)
#else
void* new_thread_trampoline(void* arg)
#endif
{
    struct thread* th = arg;
    ASSOCIATE_OS_THREAD(th);

#ifdef LISP_FEATURE_SB_SAFEPOINT
    init_thread_data scribble;
    // This "scribble" thing is really quite pointless because the original sigset_t
    // was passed in the thread's startup info (unless no signals at all were blocked).
    // And when terminating, why does anyone care what the signal mask was???
    // Well, there's a big "however": '&scribble' is no mere pass-by-reference arg-
    // it is actually used as an approximation of the C stack pointer.
#define SCRIBBLE &scribble
#else
#define SCRIBBLE 0
#endif
    // 'th->lisp_thread' remains valid despite not being in all_threads
    // due to the pinning via *STARTING-THREADS*.
    struct thread_instance *lispthread = (void*)native_pointer(th->lisp_thread);
    if (lispthread->_ephemeral_p == LISP_T) th->state_word.user_thread_p = 0;

#ifdef ATOMIC_LOGGING
      char* string = strdup((char*)VECTOR(name)->data); // FIXME: no such var as 'name'
      int index = __sync_fetch_and_add(&thread_name_map_count, 1);
      gc_assert(index < THREAD_NAME_MAP_MAX);
      thread_name_map[index].thread = pthread_self();
      thread_name_map[index].name = string;
#endif

    struct vector* startup_info = VECTOR(lispthread->startup_info); // 'lispthread' is pinned
    gc_assert(header_widetag(startup_info->header) == SIMPLE_VECTOR_WIDETAG);
    lispobj startfun = startup_info->data[0]; // 'startup_info' is pinned
    gc_assert(functionp(startfun));
    // GC can benefit from knowing the _effective_ end of the ambiguous root range.
    // Nothing at a higher address than &arg needs to be scanned for ambiguous roots.
    // For x86 + linux this optimization skips over about 800 words in the stack scan,
    // and for x86-64 it skip about 550 words as observed via:
    // fprintf(stderr, "%d non-lisp stack words\n",
    //                 (int)((lispobj*)th->control_stack_end - (lispobj*)&arg));
    // ADDRESS_SANITIZER doesn't allow this optimization.
    // Both of these assertions fail with the sanitizer enabled:
    //    gc_assert(th->control_stack_start <= (lispobj*)&arg
    //              && (lispobj*)&arg <= th->control_stack_end);
    //    gc_assert(th->control_stack_start <= (lispobj*)&startup_info
    //              && (lispobj*)&startup_info <= th->control_stack_end);
    // It seems to subvert the "&" and "*" operators in a way that only it understands,
    // while the stack pointer register is unperturbed.
    // (gencgc takes '&raise' for the current thread, but it disables the sanitizers)
    //
    // A stop-for-GC signal that hits after init_new_thread() releases the all_threads lock
    // and returns control here needs to see in the interrupt context a stack pointer
    // strictly below the computed th->control_stack_end. So make sure the value we pick
    // is strictly above any value of SP that the interrupt context could have.
#if defined LISP_FEATURE_C_STACK_IS_CONTROL_STACK && !defined ADDRESS_SANITIZER \
    && !defined LISP_FEATURE_SB_SAFEPOINT
    th->control_stack_end = (lispobj*)&arg + 1;
#endif
    th->os_kernel_tid = get_nonzero_tid();
    init_new_thread(th, SCRIBBLE, 0);
    // Passing the untagged pointer ensures 2 things:
    // - that the pinning mechanism works as designed, and not just by accident.
    // - that the initial stack does not contain a lisp pointer after it is not needed.
    //   (a regression test asserts that not even a THREAD instance is on the stack)
    funcall1(startfun, (lispobj)lispthread); // both pinned
    // Close the GC region and unlink from all_threads
    unregister_thread(th, SCRIBBLE);

    return 0;
}


// This receives a VECTOR-SAP
void sb_set_os_thread_name(char* name)
{
    __attribute__((unused)) struct vector* v = (void*)(name - offsetof(struct vector,data));
    /* Potentially set the externally-visible name of this thread,
     * and for a whole pile of crazy, look at get_max_thread_name_length_impl() in
     * https://github.com/llvm-mirror/llvm/blob/394ea6522c69c2668bf328fc923e1a11cd785265/lib/Support/Unix/Threading.inc
     * which among other things, suggests that Linux might not even have the syscall */
#ifdef LISP_FEATURE_LINUX
    /* "The thread name is a meaningful C language string, whose length is
     *  restricted to 16 characters, including the terminating null byte ('\0').
     *  The pthread_setname_np() function can fail with the following error:
     *  ERANGE The length of the string ... exceeds the allowed limit." */
    if (vector_len(v) <= 15) pthread_setname_np(pthread_self(), name);
#endif
#ifdef LISP_FEATURE_NETBSD
    /* This constant is an upper bound on the length including the NUL.
     * Exceeding it will fail the call. It happens to be 32.
     * Also, don't want to printf-format a name containing a '%' */
    if (vector_len(v) < PTHREAD_MAX_NAMELEN_NP) pthread_setname_np(pthread_self(), "%s", name);
#endif
#if defined LISP_FEATURE_FREEBSD || defined LISP_FEATURE_OPENBSD
    /* Some places document that the length limit is either 16 or 32,
     * but my testing showed that 12.1 seems to accept any length */
    pthread_set_name_np(pthread_self(), name);
#endif
#if defined LISP_FEATURE_DARWIN && !defined LISP_FEATURE_AVOID_PTHREAD_SETNAME_NP
    if (vector_len(v) < 64) pthread_setname_np(name);
#endif
}

#ifdef LISP_FEATURE_OS_THREAD_STACK
extern void* funcall1_switching_stack(void*, void *(*fun)(void *));

void* new_thread_trampoline_switch_stack(void* th) {
    return funcall1_switching_stack(th, new_thread_trampoline);
}
#endif

static struct thread* recyclebin_threads;
static struct thread* get_recyclebin_item()
{
    struct thread* result = 0;
    __attribute__((unused)) int rc = mutex_acquire(&recyclebin_lock);
    gc_assert(rc);
    if (recyclebin_threads) {
        result = recyclebin_threads;
        recyclebin_threads = result->next;
    }
    ignore_value(mutex_release(&recyclebin_lock));
    return result ? result->os_address : 0;
}
static void put_recyclebin_item(struct thread* th)
{
    __attribute__((unused)) int rc = mutex_acquire(&recyclebin_lock);
    gc_assert(rc);
    th->next = recyclebin_threads;
    recyclebin_threads = th;
    ignore_value(mutex_release(&recyclebin_lock));
}
void empty_thread_recyclebin()
{
    if (!recyclebin_threads) return;
    sigset_t old;
    block_deferrable_signals(&old);
    // no big deal if already locked (recursive GC?)
    if (TryEnterCriticalSection(&recyclebin_lock)) {
        struct thread* this = recyclebin_threads;
        while (this) {
            struct thread* next = this->next;
            free_thread_struct(this);
            this = next;
        }
        recyclebin_threads = 0;
        ignore_value(mutex_release(&recyclebin_lock));
    }
    thread_sigmask(SIG_SETMASK, &old, 0);
}

static void attach_os_thread(init_thread_data *scribble)
{
#ifndef LISP_FEATURE_WIN32 // native threads have no signal maskk
    block_deferrable_signals(&scribble->oldset);
#endif
    void* recycled_memory = get_recyclebin_item();
    struct thread *th = alloc_thread_struct(recycled_memory);

#ifndef LISP_FEATURE_SB_SAFEPOINT
    /* new-lisp-thread-trampoline doesn't like when the GC signal is blocked */
    /* FIXME: could be done using a single call to pthread_sigmask
       together with blocking the deferrable signals above. */
    unblock_gc_stop_signal();
#endif

    th->os_kernel_tid = get_nonzero_tid();
    /* win32: While ASSOCIATE_OS_THREAD performs a relatively expensive DuplicateHandle(),
     * simplicity here is preferable to the complexity entailed by memoizing the handle
     * in a TLS slot and registering a waiter on the foreign thread to close to handle.
     * In contrast to the previous approach, the new handle is closed in detach_os_thread(),
     * and if C calls lisp again in this thread... then lather, rinse, repeat.
     * A benchmark based on 'fcb-threads.impure' shows that we're still 8x faster
     * at callback entry than the code as it was prior to git rev 91f86339b4 */
    ASSOCIATE_OS_THREAD(th);

#if !defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
    /* On windows, arch_os_thread_init will take care of finding the
     * stack. */
    void *stack_addr;
    size_t stack_size;
# ifdef LISP_FEATURE_OPENBSD
    stack_t stack;
    pthread_stackseg_np(th->os_thread, &stack);
    stack_size = stack.ss_size;
    stack_addr = (void*)((size_t)stack.ss_sp - stack_size);
# elif defined LISP_FEATURE_SUNOS
    stack_t stack;
    thr_stksegment(&stack);
    stack_size = stack.ss_size;
    stack_addr = (void*)((size_t)stack.ss_sp - stack_size);
# elif defined(LISP_FEATURE_DARWIN)
    stack_size = pthread_get_stacksize_np(th->os_thread);
    stack_addr = (char*)pthread_get_stackaddr_np(th->os_thread) - stack_size;
# else
    pthread_attr_t attr;
    pthread_attr_init(&attr);
#   if defined LISP_FEATURE_FREEBSD || defined LISP_FEATURE_DRAGONFLY
    pthread_attr_get_np(th->os_thread, &attr);
#   else
    int pthread_getattr_np(pthread_t, pthread_attr_t *);
    pthread_getattr_np(th->os_thread, &attr);
#   endif
    pthread_attr_getstack(&attr, &stack_addr, &stack_size);
    pthread_attr_destroy(&attr);
# endif
    th->control_stack_start = stack_addr;
    th->control_stack_end = (void *) (((uintptr_t) stack_addr) + stack_size);
#endif

    /* We don't protect the control stack when adopting a foreign thread
     * because we wouldn't know where to put the guard */
    init_new_thread(th, scribble,
                    /* recycled memory already had mprotect() done,
                     * so avoid 2 syscalls when possible */
                    recycled_memory ? 0 : GUARD_BINDING_STACK|GUARD_ALIEN_STACK);
}

static void detach_os_thread(init_thread_data *scribble)
{
    struct thread *th = get_sb_vm_thread();

#if defined(LISP_FEATURE_WIN32)
    CloseHandle((HANDLE)th->os_thread);
#endif

    unregister_thread(th, scribble);

    /* We have to clear a STOP_FOR_GC signal if pending. Consider:
     *  - on entry to unregister_thread, we block all signals
     *  - simultaneously some other thread decides that it needs to initiate a GC
     *  - that thread observes that this thread exists in all_threads and sends
     *    STOP_FOR_GC, so it becomes pending but undeliverable in this thread
     *  - immediately after blocking signals, we change state to DEAD,
     *    which allows the GCing thread to ignore this thread
     *    (it sees the state change criterion as having been satisfied)
     *  - the GCing thread releases the all_threads lock
     *  - this thread acquires the lock and removes itself from all_threads,
     *    and indicates that it is no longer a lisp thread
     *  - but STOP_FOR_GC is pending because it was in the blocked set.
     * Bad things happen unless we clear the pending GC signal.
     */
#if !defined LISP_FEATURE_SB_SAFEPOINT
    sigset_t pending;
    sigpending(&pending);
    if (sigismember(&pending, SIG_STOP_FOR_GC)) {
#ifdef LISP_FEATURE_DARWIN
        /* sigwait is not reliable on macOS, but sigsuspend is. It unfortunately
         * requires that the signal be delivered, so set a flag to ignore it.
         * If you don't believe the preceding statement, try enabling the other
         * branch of this #ifdef and running fcb-threads.impure.lisp which will
         * sporadically fail with "Can't handle sig31 in non-lisp thread".
         * So either sigpending was sometimes lying (hence we didn't try to clear
         * the signal), or else sigwait did not dequeue the signal. Clearly the
         * latter must be true, because if only the former were true, then we
         * would also see the test fail with sigsuspend */
        sigset_t blockmask;
        sigfillset(&blockmask);
        sigdelset(&blockmask, SIG_STOP_FOR_GC);
        pthread_setspecific(ignore_stop_for_gc, (void*)1);
        /* sigsuspend takes the mask of signals to block */
        sigsuspend(&blockmask);
        pthread_setspecific(ignore_stop_for_gc, 0);
        sigpending(&pending);
        if (sigismember(&pending, SIG_STOP_FOR_GC)) lose("clear stop-for-GC did not work");
#else
        __attribute__((unused)) int sig, rc;
        /* sigwait takes the mask of signals to allow through */
        rc = sigwait(&gc_sigset, &sig);
        gc_assert(rc == 0 && sig == SIG_STOP_FOR_GC);
#endif
    }
#endif
    put_recyclebin_item(th);
#ifndef LISP_FEATURE_WIN32 // native threads have no signal mask
    thread_sigmask(SIG_SETMASK, &scribble->oldset, 0);
#endif
}

#if defined(LISP_FEATURE_X86_64) && !defined(LISP_FEATURE_WIN32)
extern void funcall_alien_callback(lispobj arg1, lispobj arg2, lispobj arg0,
                                   struct thread* thread)
  __attribute__((sysv_abi));
#endif

/* This function's address is assigned into a static symbol's value slot,
 * so it has to look like a fixnum. lp#1991485 */
void __attribute__((aligned(8)))
callback_wrapper_trampoline(
#if !(defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
    /* On the x86oid backends, the assembly wrapper happens to not pass
     * in ENTER_ALIEN_CALLBACK explicitly for safepoints.  However, the
     * platforms with precise GC are tricky enough already, and I want
     * to minimize the read-time conditionals.  For those platforms, I'm
     * only replacing funcall3 with callback_wrapper_trampoline while
     * keeping the arguments unchanged. --DFL */
    lispobj __attribute__((__unused__)) fun,
#endif
    lispobj arg0, lispobj arg1, lispobj arg2)
{
    struct thread* th = get_sb_vm_thread();
    if (!th) {                  /* callback invoked in non-lisp thread */
        init_thread_data scribble;
        attach_os_thread(&scribble);

        WITH_GC_AT_SAFEPOINTS_ONLY()
        {
            funcall3(StaticSymbolFunction(ENTER_FOREIGN_CALLBACK), arg0,arg1,arg2);
        }
        detach_os_thread(&scribble);
        return;
    }

#ifdef LISP_FEATURE_WIN32
    /* arg2 is the pointer to a return value, which sits on the stack */
    thread_extra_data(th)->carried_base_pointer = (os_context_register_t) *(((void**)arg2)-1);
#endif

    WITH_GC_AT_SAFEPOINTS_ONLY()
    {
#if defined(LISP_FEATURE_X86_64) && !defined(LISP_FEATURE_WIN32)
        funcall_alien_callback(arg1, arg2, arg0, th);
#else
        funcall3(StaticSymbolFunction(ENTER_ALIEN_CALLBACK), arg0,arg1,arg2);
#endif
    }
}

#endif /* LISP_FEATURE_SB_THREAD */

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
 *   |   | CONTROL | BINDING |  ALIEN  |  CSP | thread     |          |
 *   |   |  STACK  |  STACK  |  STACK  | PAGE | structure  | altstack |
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
    bool zeroize_stack = 0;
    if (spaces) {
        // If reusing memory from a previously exited thread, start by removing
        // some old junk from the stack. This is imperfect since we only clear a little
        // at the top, but doing so enables diagnosing some garbage-retention issues
        // using a fine-toothed comb. It would not be possible at all to diagnose
        // if any newly started thread could refer a dead thread's heap objects.
        zeroize_stack = 1;
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
#ifdef THREAD_T_NIL_CONSTANTS_SLOT
    tls[THREAD_T_NIL_CONSTANTS_SLOT] = (NIL << 32) | LISP_T;
#endif
#ifdef LISP_FEATURE_LINKAGE_SPACE
    tls[THREAD_LINKAGE_TABLE_SLOT] = (lispobj)linkage_space;
    tls[THREAD_ALIEN_LINKAGE_TABLE_BASE_SLOT] = (lispobj)ALIEN_LINKAGE_SPACE_START;
#endif
#if defined LISP_FEATURE_X86_64 && defined LISP_FEATURE_LINUX
    tls[THREAD_MSAN_XOR_CONSTANT_SLOT] = 0x500000000000;
#endif
#ifdef LAYOUT_OF_FUNCTION
    tls[THREAD_FUNCTION_LAYOUT_SLOT] = LAYOUT_OF_FUNCTION << 32;
#endif
#ifdef THREAD_TEXT_CARD_MARKS_SLOT
    extern unsigned int* text_page_touched_bits;
    tls[THREAD_TEXT_SPACE_ADDR_SLOT] = TEXT_SPACE_START;
    tls[THREAD_TEXT_CARD_COUNT_SLOT] = text_space_size / IMMOBILE_CARD_BYTES;
    tls[THREAD_TEXT_CARD_MARKS_SLOT] = (lispobj)text_page_touched_bits;
#endif

    th->os_address = spaces;
    th->control_stack_start = (lispobj*)aligned_spaces;
    th->binding_stack_start=
        (lispobj*)((char*)th->control_stack_start+thread_control_stack_size);
    th->control_stack_end = th->binding_stack_start;

    if (zeroize_stack) {
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
    th->alien_stack_start=
        (lispobj*)((char*)th->binding_stack_start+BINDING_STACK_SIZE);
    set_binding_stack_pointer(th,th->binding_stack_start);
    th->this = th;
    th->os_kernel_tid = 0;
    th->os_thread = 0;
    // Once allocated, the allocation profiling buffer sticks around.
    // If present and enabled, assign into the new thread.
    extern int alloc_profiling;
    th->profile_data = (uword_t*)(alloc_profiling ? alloc_profile_buffer : 0);

    struct extra_thread_data *extra_data = thread_extra_data(th);
    memset(extra_data, 0, sizeof *extra_data);

#if defined LISP_FEATURE_SB_THREAD && !defined LISP_FEATURE_SB_SAFEPOINT
    os_sem_init(&extra_data->state_sem, 1);
    os_sem_init(&extra_data->state_not_running_sem, 0);
    os_sem_init(&extra_data->state_not_stopped_sem, 0);
#endif
#if defined LISP_FEATURE_UNIX && defined LISP_FEATURE_SB_THREAD
    os_sem_init(&extra_data->sprof_sem, 0);
#endif
    extra_data->sprof_lock = 0;
    th->sprof_data = 0;

    th->state_word.state = STATE_RUNNING;
    th->state_word.sprof_enable = 0;
    th->state_word.user_thread_p = 1;

    lispobj* alien_stack_end = (lispobj*)((char*)th->alien_stack_start + ALIEN_STACK_SIZE);
#ifdef ALIEN_STACK_GROWS_UPWARD
    th->alien_stack_pointer = alien_stack_start;
#elif defined LISP_FEATURE_X86 || defined LISP_FEATURE_X86_64
    /* It is not necessary to subtract a word from the starting stack pointer
     * because consuming C stack space pre-decrements before using the pointer.
     * So this choice has nothing to do with saving a word of memory, and everything
     * to do with my lack of understanding of the conventions for non-x86. */
    th->alien_stack_pointer = alien_stack_end;
#else
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

    thread_interrupt_data(th).pending_handler = 0;
    thread_interrupt_data(th).gc_blocked_deferrables = 0;
#if HAVE_ALLOCATION_TRAP_CONTEXT
    thread_interrupt_data(th).allocation_trap_context = 0;
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
    th->card_table = (lispobj)gc_card_mark;
    return th;
}
#ifdef LISP_FEATURE_SB_THREAD
#ifdef LISP_FEATURE_WIN32
uword_t create_thread(struct thread* th)
{
    unsigned int tid;
    struct extra_thread_data *data = thread_extra_data(th);
    data->blocked_signal_set = deferrable_sigset;
    // It's somewhat customary in the win32 API to start threads as suspended.
    th->os_thread =
      _beginthreadex(NULL, thread_control_stack_size, new_thread_trampoline, th,
                     CREATE_SUSPENDED | STACK_SIZE_PARAM_IS_A_RESERVATION, &tid);
    bool success = th->os_thread != 0;
    if (success) {
        th->os_kernel_tid = tid;
        ResumeThread((HANDLE)th->os_thread);
    }
    return success;
}
#endif

int try_acquire_gc_lock() { return TryEnterCriticalSection(&in_gc_lock); }
int release_gc_lock() { return mutex_release(&in_gc_lock); }

static __attribute__((unused)) struct timespec stw_begin_realtime, stw_begin_cputime;
long timespec_diff(struct timespec* begin, struct timespec* end)
{
#ifdef LISP_FEATURE_64_BIT
    return (end->tv_sec - begin->tv_sec) * 1000000000L + (end->tv_nsec - begin->tv_nsec) ;
#else
    return (end->tv_sec - begin->tv_sec) * 1000000L + (end->tv_nsec - begin->tv_nsec) / 1000;
#endif
}
#ifdef MEASURE_STOP_THE_WORLD_PAUSE
void thread_accrue_stw_time(struct thread* th,
                            struct timespec* begin_real,
                            struct timespec* begin_cpu)
{
    /* A non-Lisp thread calling into Lisp via DEFINE-ALIEN-CALLABLE
     * can receive SIG_STOP_FOR_GC as soon as it has a 'struct thread'
     * and _before_ a thread instance has been consed */
    if (th->lisp_thread) {
        struct timespec now;
        clock_gettime(CLOCK_MONOTONIC, &now);
        unsigned long elapsed = timespec_diff(begin_real, &now);
        struct thread_instance* ti = (void*)INSTANCE(th->lisp_thread);
        if (elapsed > ti->uw_max_stw_pause) ti->uw_max_stw_pause = elapsed;
        ti->uw_sum_stw_pause += elapsed;
        ++ti->uw_ct_stw_pauses;
        if (begin_cpu) {
#ifdef CLOCK_THREAD_CPUTIME_ID
          clock_gettime(CLOCK_THREAD_CPUTIME_ID, &now);
          ti->uw_gc_virtual_time += timespec_diff(begin_cpu, &now);
#endif
        }
    }
}
#endif

/* stopping the world is a two-stage process.  From this thread we signal
 * all the others with SIG_STOP_FOR_GC.  The handler for this signal does
 * the usual pseudo-atomic checks (we don't want to stop a thread while
 * it's in the middle of allocation) then waits for another SIG_STOP_FOR_GC.
 */
/*
 * (With SB-SAFEPOINT, see the definitions in safepoint.c instead.)
 */
#if !defined LISP_FEATURE_SB_SAFEPOINT && !defined STANDALONE_LDB

/* To avoid deadlocks when gc stops the world all clients of each
 * mutex must enable or disable SIG_STOP_FOR_GC for the duration of
 * holding the lock, but they must agree on which.
 * [The preceding remark is probably wrong - STOP_FOR_GC is a signal
 * that is directed to a thread, so the "wrong" thread would never
 * respond to someone else's STOP_FOR_GC. I'm leaving the comment
 * just case someone can decipher it and decide to delete it]
 *
 * A note about ESRCH: tchnically ESRCH can happen if an OS thread ceases
 * to exist, while the thread library has a representation of the thread
 * because pthread_join() wasn't invoked on it yet.
 * ESRCH can't oocur for us because:
 * - if a thread was still linked in all_threads at the acquire of all_threads lock,
 *   then that thread can't make progress in its termination code, because it's
 *   waiting on the lock. If it changed its state to DEAD, but we perceived it as
 *   RUNNING, it now must be blocked on the all_threads_lock and it can't disappear.
 * - ESRCH is not guaranteed to be returned anyway, and Linux man page doesn't even
 *   list it as a possible outcome of pthread_kill.
 * Also, there used to be assertion that "thread_state(p)==STATE_DEAD)" on ESRCH
 * error, but that's saying that there is still memory backing 'struct thread'
 * (so that dereferencing was valid), but if dereferencing was valid, then the thread
 * can't have died (i.e. if ESRCH could be returned, then that implies that
 * the memory shouldn't be there) */

void gc_stop_the_world()
{
#ifdef MEASURE_STOP_THE_WORLD_PAUSE
    /* The thread performing stop-the-world does not use sig_stop_for_gc_handler on itself,
     * so it would not accrue time spent stopped. Force it to, by considering it "paused"
     * from the moment it wants to stop all other threads. */
    clock_gettime(CLOCK_MONOTONIC, &stw_begin_realtime);
#endif
#ifdef CLOCK_THREAD_CPUTIME_ID
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &stw_begin_cputime);
#endif
    struct thread *th, *me = get_sb_vm_thread();
    int rc;

    /* Keep threads from registering with GC while the world is stopped. */
    rc = mutex_acquire(&all_threads_lock);
    gc_assert(rc);

    /* stop all other threads by sending them SIG_STOP_FOR_GC */
    for_each_thread(th) {
        if (th != me) {
            gc_assert(th->os_thread != 0);
            struct extra_thread_data *semaphores = thread_extra_data(th);
            os_sem_wait(&semaphores->state_sem);
            int state = get_thread_state(th);
            if (state == STATE_RUNNING) {
                rc = pthread_kill(th->os_thread,SIG_STOP_FOR_GC);
                /* This used to bogusly check for ESRCH.
                 * I changed the ESRCH case to just fall into lose() */
                if (rc) lose("cannot suspend thread %p: %d, %s",
                     // KLUDGE: assume that os_thread can be cast as pointer.
                     // See comment in 'interr.h' about that.
                     (void*)th->os_thread, rc, strerror(rc));
            }
            os_sem_post(&semaphores->state_sem);
        }
    }
    for_each_thread(th) {
        if (th != me) {
            __attribute__((unused)) int state = thread_wait_until_not(STATE_RUNNING, th);
            gc_assert(state != STATE_RUNNING);
        }
    }
    event0("/gc_stop_the_world:end");
}

/* pthread_kill is not guaranteed to be reentrant, prevent
 * gc_stop_the_world from interrupting another pthread_kill */
int sb_thread_kill (pthread_t thread, int sig) {
    sigset_t old;
    block_blockable_signals(&old);
    int ret = pthread_kill(thread, sig);
    thread_sigmask(SIG_SETMASK, &old, NULL);
    return ret;
}


void gc_start_the_world()
{
#ifdef COLLECT_GC_STATS
    struct timespec gc_end_time;
    clock_gettime(CLOCK_MONOTONIC, &gc_end_time);
    long gc_elapsed = (gc_end_time.tv_sec - gc_start_time.tv_sec)*1000000000L
                      + (gc_end_time.tv_nsec - gc_start_time.tv_nsec);
    if (stw_elapsed < 0 || gc_elapsed < 0) {
        char errmsg[] = "GC: Negative times?\n";
        ignore_value(write(2, errmsg, sizeof errmsg-1));
    } else {
        stw_sum_duration += stw_elapsed;
        if (stw_elapsed < stw_min_duration) stw_min_duration = stw_elapsed;
        if (stw_elapsed > stw_max_duration) stw_max_duration = stw_elapsed;
        gc_sum_duration += gc_elapsed;
        if (gc_elapsed < gc_min_duration) gc_min_duration = gc_elapsed;
        if (gc_elapsed > gc_max_duration) gc_max_duration = gc_elapsed;
        ++n_gcs_done;
    }
#endif
    struct thread *th, *me = get_sb_vm_thread();
    __attribute__((unused)) int lock_ret;
    /* if a resumed thread creates a new thread before we're done with
     * this loop, the new thread will be suspended waiting to acquire
     * the all_threads lock */
    for_each_thread(th) {
        gc_assert(th->os_thread);
        if (th != me) {
          /* I don't know if a normal load is fine here. I think we can't read
           * any value other than what was already observed?
           * No harm in being cautious though with regard to compiler reordering */
            int state = get_thread_state(th);
            if (state != STATE_DEAD) {
                if(state != STATE_STOPPED)
                    lose("gc_start_the_world: bad thread state %x", state);
                set_thread_state(th, STATE_RUNNING, 0);
            }
        }
    }

    lock_ret = mutex_release(&all_threads_lock);
    gc_assert(lock_ret);
#ifdef MEASURE_STOP_THE_WORLD_PAUSE
    thread_accrue_stw_time(me, &stw_begin_realtime, &stw_begin_cputime);
#endif
}

#endif /* !LISP_FEATURE_SB_SAFEPOINT */
#elif !defined STANDALONE_LDB
// no threads
void gc_stop_the_world() {}
void gc_start_the_world() {}
#endif /* !LISP_FEATURE_SB_THREAD */

int
thread_yield()
{
#ifdef LISP_FEATURE_SB_THREAD
    return sched_yield();
#else
    return 0;
#endif
}

#ifdef LISP_FEATURE_SB_SAFEPOINT
/* If the thread id given does not belong to a running thread (it has
 * exited or never even existed) pthread_kill _may_ fail with ESRCH,
 * but it is also allowed to just segfault, see
 * <http://udrepper.livejournal.com/16844.html>.
 *
 * Relying on thread ids can easily backfire since ids are recycled
 * (NPTL recycles them extremely fast) so a signal can be sent to
 * another process if the one it was sent to exited.
 *
 * For these reasons, we must make sure that the thread is still alive
 * when the pthread_kill is called and return if the thread is
 * exiting.
 *
 * Note (DFL, 2011-06-22): At the time of writing, this function is only
 * used for INTERRUPT-THREAD, hence the wake_thread special-case for
 * Windows is OK. */
void wake_thread(struct thread_instance* lispthread)
{
#ifdef LISP_FEATURE_WIN32
        /* META: why is this comment about safepoint builds mentioning
         * gc_stop_the_world() ? Never the twain shall meet. */

        /* Kludge (on safepoint builds): At the moment, this isn't just
         * an optimization; rather it masks the fact that
         * gc_stop_the_world() grabs the all_threads mutex without
         * releasing it, and since we're not using recursive pthread
         * mutexes, the pthread_mutex_lock() around the all_threads loop
         * would go wrong.  Why are we running interruptions while
         * stopping the world though?  Test case is (:ASYNC-UNWIND
         * :SPECIALS), especially with s/10/100/ in both loops. */

        /* Frequent special case: resignalling to self.  The idea is
         * that leave_region safepoint will acknowledge the signal, so
         * there is no need to take locks, roll thread to safepoint
         * etc. */
        struct thread* thread = (void*)lispthread->uw_primitive_thread;
        if (thread == get_sb_vm_thread()) {
            sb_pthr_kill(thread, 1); // can't fail
            check_pending_thruptions(NULL);
            return;
        }
        // block_deferrables + mutex_lock looks very unnecessary here,
        // but without them, make-target-contrib hangs in bsd-sockets.
        sigset_t oldset;
        block_deferrable_signals(&oldset);
        mutex_acquire(&all_threads_lock);
        sb_pthr_kill(thread, 1); // can't fail
# ifdef LISP_FEATURE_SB_SAFEPOINT
        wake_thread_impl(lispthread);
# endif
        mutex_release(&all_threads_lock);
        thread_sigmask(SIG_SETMASK,&oldset,0);
#elif defined LISP_FEATURE_SB_SAFEPOINT
    wake_thread_impl(lispthread);
#else
    pthread_kill(lispthread->uw_os_thread, SIGURG);
#endif
}
#endif

#ifdef LISP_FEATURE_ULTRAFUTEX
extern int futex_wake(int *lock_word, int n);
void lispmutex_wake_waiter()
{
    struct lispmutex* m = (void*)INSTANCE(read_TLS(CURRENT_MUTEX, get_sb_vm_thread()));
    // The lock word is in the least-significant half of the state word if 64-bit.
    // See the definition of MUTEX-STATE-ADDRESS which adds 4 if #+big-endian.
    int* word =
#ifdef LISP_FEATURE_BIG_ENDIAN
                     1 +
#endif
        (int*)&m->uw_state;
    *word = 0; // slam 0 in, meaning uncontested
    futex_wake(word, 1);
}
#endif
