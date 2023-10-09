/*
 * control.c
 * @copyright (c) 2007-2014, Tohoku University.
 * @author UENO Katsuhiro
 */

#ifdef HAVE_GENESIS_CONFIG
#include "genesis/config.h"
#include "genesis/thread.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"
#include "genesis/constants.h"
#include "interr.h"
#include <stdio.h>
#include <string.h>
#endif
#include "smlsharp.h"
#include <stdlib.h>
#include "heap.h"
#include <signal.h>

int gc_verbose = 1;

#ifndef WITHOUT_MULTITHREAD
struct control {
  //_Atomic(unsigned int) state;
        //unsigned int* statepointer;
        _Atomic(unsigned int) flags;
        struct control *next;  /* singly-linked list */
        // Deletion of the vm_thread (the 'struct thread' and all associated
        // memory) can be done only while holding this lock.
        // Also the thread is not allowed to completely exit (i.e. cease to be
        // a valid pthread identifier) until clearing the vm_thread field,
        // which is also required to be done under the lock.
        pthread_mutex_t state_lock;
        struct thread *vm_thread;
};
#endif /* !WITHOUT_MULTITHREAD */

#define PHASE_MASK       0x0fU
#define INACTIVE_FLAG    0x10U
#define PHASE(state)     ((state) & PHASE_MASK)
#define ACTIVE(phase)    (phase)
#define INACTIVE(phase)  ((phase) | INACTIVE_FLAG)
#define IS_ACTIVE(state) (!((state) & INACTIVE_FLAG))

#define CANCELED_FLAG    0x01U
#define PTHREAD_FLAG     0x02U

#ifdef WITHOUT_MULTITHREAD
#undef IS_ACTIVE
#define IS_ACTIVE(x) 1
#endif /* WITHOUT_MULTITHREAD */

struct sml_user {
#ifndef WITHOUT_MASSIVETHREADS
        struct control control;
#endif /* !WITHOUT_MASSIVETHREADS */
        struct frame_stack_range {
                /* If bottom is empty, this is a dummy range; this must be
                 * skipped during stack frame scan.  A dummy range is used
                 * to run a sequence of top-level code fragments efficiently.
                 * See sml_run_toplevels. */
                void *bottom, *top;
                struct frame_stack_range *next;
        } *frame_stack;
        void *arbdata;
        void *exn_object;
};

struct sml_worker {
#ifndef WITHOUT_MULTITHREAD
        struct control control;
#endif /* !WITHOUT_MULTITHRED */
        union sml_alloc *thread_local_heap;
        struct sml_user *user;
#ifndef WITHOUT_MASSIVETHREADS
        /* newly created mutators (but not yet in global mutators list) */
        _Atomic(struct control *) new_users;
#endif /* WITHOUT_MASSIVETHREADS */
};

#ifndef WITHOUT_MULTITHREAD
static _Atomic(struct control *) workers;
static enum sml_sync_phase new_worker_phase = ASYNC;
static sml_spinlock_t worker_creation_lock = SPIN_LOCK_INIT;
union sml_alloc* get_worker_aps() {
  return ((struct  sml_worker*)workers)->thread_local_heap;
}
#endif /* !WITHOUT_MULTITHREAD */

#ifndef WITHOUT_MASSIVETHREADS
static struct control *users; /* owned by collector only */
#endif /* !WITHOUT_MASSIVETHREADS */

_Atomic(unsigned int) sml_check_flag;

#define FLAG_GC      1U
#define FLAG_SIGNAL  (~(-1U >> 1))

int use_gcsignal = 1;
#ifndef WITHOUT_CONCURRENCY
#define USE_SYNC_SEM 1
#ifdef USE_SYNC_SEM
#include <semaphore.h>
os_sem_t gc_sync_semaphore;
#elif defined USE_SYNC_PIPE
static int sync_pipe[2];
#else
static pthread_mutex_t sync_wait_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t sync_wait_cond = PTHREAD_COND_INITIALIZER;
#endif
static _Atomic(unsigned int) sync_counter;
#endif /* !WITHOUT_CONCURRENCY */

#include <unistd.h>
#ifndef WITHOUT_MULTITHREAD
static void cancel(void *p)
{
        /* Thread cancellation is performed in C code since SML# code
         * never cancel any thread.  We assume that thread is always
         * canceled synchronously due to, for example, pthread_cancel or
         * pthread_exit.  POSIX thread provides capability of asynchronous
         * cancellation, which would break mutator--collector handshaking,
         * but no clever programmer use it ;p).
         */
        fetch_or(relaxed, &((struct control *)p)->flags, CANCELED_FLAG);
        TPRINTF(1, "set CANCELED_FLAG");
}
#endif /* WITHOUT_MULTITHREAD */

worker_tlv_alloc(struct sml_worker *, current_worker, cancel);

void sml_current_worker_set_thread(struct thread* thread, int set_aps) {
    struct sml_worker *worker = worker_tlv_get(current_worker);
    worker->control.vm_thread = thread;
    if (thread) {
        spin_lock(&worker_creation_lock);
        thread->gc_phase = ACTIVE(new_worker_phase);
        spin_unlock(&worker_creation_lock);
    }
    if (set_aps) {
    }
}
void sml_current_user_set_arbdata(void* data) {
    struct sml_worker *worker = worker_tlv_get(current_worker);
    struct sml_user *user = worker->user;
    fprintf(stderr, "current SML worker = %p, user = %p\n", worker, user);
    user->arbdata = data;
}
void* get_sml_user_arbdata(struct sml_user* user) { return user->arbdata; }

#ifndef WITHOUT_MASSIVETHREADS
user_tlv_alloc(struct sml_user *, current_user, cancel);
#endif /* !WITHOUT_MASSIVETHREADS */

static _Atomic(struct sml_worker *) signal_receiver;
static void (*signal_handler)(void);

int
sml_set_signal_handler(void (*handler)(void))
{
        struct sml_worker *worker = worker_tlv_get(current_worker), *old = NULL;
        if (!cmpswap_relaxed(&signal_receiver, &old, worker) && old != worker)
                return 1;
        signal_handler = handler;
        return 0;
}

int
sml_send_signal()
{
        if (load_relaxed(&signal_receiver) == NULL)
                return 1;
        fetch_or(release, &sml_check_flag, FLAG_SIGNAL);
        return 0;
}

#if !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY
static pthread_mutex_t stop_the_world_lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t stop_the_world_cond = PTHREAD_COND_INITIALIZER;
static _Atomic(unsigned int) stop_the_world_flag;
#endif /* !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY
int
sml_stop_the_world()
{
        mutex_lock(&stop_the_world_lock);
        if (load_relaxed(&stop_the_world_flag)) {
                /* do nothing if another thread stops the world */
                while (load_relaxed(&stop_the_world_flag))
                        cond_wait(&stop_the_world_cond, &stop_the_world_lock);
                mutex_unlock(&stop_the_world_lock);
                return 0;
        }
        store_relaxed(&stop_the_world_flag, 1);
        mutex_unlock(&stop_the_world_lock);
        return 1;
}
#endif /* !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY */

#if !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY
void
sml_run_the_world()
{
        mutex_lock(&stop_the_world_lock);
        store_relaxed(&stop_the_world_flag, 0);
        cond_broadcast(&stop_the_world_cond);
        mutex_unlock(&stop_the_world_lock);
}
#endif /* !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY */

#ifndef WITHOUT_MULTITHREAD
static void
control_insert(_Atomic(struct control *) *list, struct control *item)
{
        struct control *old = load_relaxed(list);
        do {
                item->next = old;
        } while (!cmpswap_release(list, &old, item));
}
#endif /* !WITHOUT_MULTITHREAD */

#ifndef WITHOUT_MULTITHREAD
extern __thread struct thread *current_thread;
// pointer to control state (stored in the SBCL 'struct thread')
#define pCTRL_STATE(c) &(c).vm_thread->gc_phase
static void
control_init(struct control *control, unsigned int state)
{
        struct thread* vm_thread = control->vm_thread;
        if (!vm_thread) vm_thread = control->vm_thread = current_thread;
        if (vm_thread) atomic_init(&vm_thread->gc_phase, state);
        atomic_init(&control->flags, 0);
}
#endif /* !WITHOUT_MULTITHREAD */

#ifndef WITHOUT_MULTITHREAD
static void
activate(struct control *control)
{
        unsigned int old;

        /* all updates by other threads must happen before here */
        old = fetch_and(acquire, pCTRL_STATE(*control), ~INACTIVE_FLAG);
        while (IS_ACTIVE(old)) {
                sched_yield();
                old = fetch_and(acquire, pCTRL_STATE(*control), ~INACTIVE_FLAG);
        }
        assert(IS_ACTIVE(load_relaxed(pCTRL_STATE(*control))));
}
#endif /* !WITHOUT_MULTITHREAD */

#ifndef WITHOUT_MASSIVETHREADS
static void
activate_myth(struct control *control)
{
        unsigned int old;

        /* all updates by other threads must happen before here */
        old = fetch_and(acquire, &control->state, ~INACTIVE_FLAG);
        while (IS_ACTIVE(old)) {
                if (!(load_relaxed(&control->flags) & PTHREAD_FLAG))
                        myth_yield();
                old = fetch_and(acquire, &control->state, ~INACTIVE_FLAG);
        }
        assert(IS_ACTIVE(load_relaxed(&control->state)));
}
#endif /* !WITHOUT_MULTITHREAD */

#ifndef WITHOUT_MASSIVETHREADS
static void
user_register(struct sml_user *user, struct sml_worker *worker)
{
        control_init(&user->control, ACTIVE(ASYNC));
        control_insert(&worker->new_users, &user->control);
        if (load_relaxed(&worker->control.flags) & PTHREAD_FLAG) {
                atomic_init(&user->control.flags, PTHREAD_FLAG);
                worker->user = user;
        } else {
                user_tlv_set(current_user, user);
        }
}
#endif /* WITHOUT_MASSIVETHREADS */

#ifdef WITHOUT_MULTITHREAD
static void
worker_register(struct sml_worker *worker ATTR_UNUSED)
{
        worker_tlv_set(current_worker, worker);
}
#endif /* WITHOUT_MULTITHREAD */

#ifndef WITHOUT_MULTITHREAD
static void
worker_register(struct sml_worker *worker)
{
        control_init(&worker->control, ACTIVE(ASYNC));
#ifndef WITHOUT_MASSIVETHREADS
        if (!myth_is_myth_worker())
                atomic_init(&worker->control.flags, PTHREAD_FLAG);
#endif /* !WITHOUT_MASSIVETHREADS */
        spin_lock(&worker_creation_lock);
        if (worker->control.vm_thread)
            atomic_init(pCTRL_STATE(worker->control), ACTIVE(new_worker_phase));
        else {
            fprintf(stderr, "WARNING: no sb-vm:thread for GC worker yet\n");
        }
        control_insert(&workers, &worker->control);
        spin_unlock(&worker_creation_lock);
        worker_tlv_set(current_worker, worker);
}
#endif /* WITHOUT_MULTITHREAD */

static struct sml_user *
user_new()
{
        struct sml_user *user;
        user = xmalloc(sizeof(struct sml_user), "user");
        user->arbdata = NULL;
        user->frame_stack = NULL;
        user->exn_object = NULL;
        return user;
}

static struct sml_worker *
worker_new()
{
        struct sml_worker *worker;
        worker = xmalloc(sizeof(struct sml_worker), "worker");
        worker->thread_local_heap = sml_heap_worker_init();
        worker->user = NULL;
        worker->control.vm_thread = NULL;
        pthread_mutex_init(&worker->control.state_lock, 0);
#ifndef WITHOUT_MASSIVETHREADS
        atomic_init(&worker->new_users, NULL);
#endif /* WITHOUT_MASSIVETHREADS */
        fprintf(stderr, "GC: new worker @ %p\n", worker);
        return worker;
}

static void
user_destroy(struct sml_user *user)
{
        xfree(user, "user");
}

static void
worker_destroy(struct sml_worker *worker)
{
        sml_heap_worker_destroy(worker->thread_local_heap);
#ifdef WITHOUT_MASSIVETHREADS
        user_destroy(worker->user);
#endif /* WITHOUT_MASSIVETHREADS */
        pthread_mutex_destroy(&worker->control.state_lock);
        xfree(worker, "worker");
}

#ifndef WITHOUT_CONCURRENCY
extern char* cur_thread_name();
static void
decr_sync_counter_relaxed()
{
        unsigned int oldval = fetch_sub(relaxed, &sync_counter, 1);
        TPRINTF(1, "dec_sync_relaxed: old=%x", oldval);
#if USE_SYNC_SEM
        os_sem_post(&gc_sync_semaphore);
#else
        if (oldval == 1) {
                mutex_lock(&sync_wait_lock);
                cond_signal(&sync_wait_cond);
                mutex_unlock(&sync_wait_lock);
        }
#endif
}
#endif /* !WITHOUT_CONCURRENCY */

#ifndef WITHOUT_CONCURRENCY
static void
decr_sync_counter_release()
{
        unsigned int oldval = fetch_sub(release, &sync_counter, 1);
        TPRINTF(1, "dec_sync_release: old=%x", oldval);
#if USE_SYNC_SEM
        os_sem_post(&gc_sync_semaphore);
#else
        if (oldval == 1) {
                mutex_lock(&sync_wait_lock);
                cond_signal(&sync_wait_cond);
                mutex_unlock(&sync_wait_lock);
        }
#endif
}
#endif /* !WITHOUT_CONCURRENCY */

#if !defined WITHOUT_CONCURRENCY && defined WITHOUT_MASSIVETHREADS
static void
user_sync2(struct sml_user *user)
{
        sml_heap_user_sync2(user);
}
#endif /* !defined WITHOUT_CONCURRENCY && defined WITHOUT_MASSIVETHREADS */

#ifndef WITHOUT_MASSIVETHREADS
static void
user_sync2(struct sml_user *user)
{
        sml_heap_user_sync2(user);
        /* all updates by this thread must happen before here */
        decr_sync_counter_release();
}
#endif /* !WITHOUT_MASSIVETHREADS */

#ifndef WITHOUT_CONCURRENCY
static void
user_sync2_with(struct sml_user *user, void *frame_pointer)
{
        void *old_frame_top = user->frame_stack->top;
        if (!old_frame_top)
                user->frame_stack->top = frame_pointer;
        user_sync2(user);
        user->frame_stack->top = old_frame_top;
}
#endif /* WITHOUT_CONCURRENCY */

#ifndef WITHOUT_CONCURRENCY
static void
worker_sync1(struct sml_worker *worker ATTR_UNUSED)
{
        decr_sync_counter_relaxed();
}
#endif /* WITHOUT_CONCURRENCY */

#ifndef WITHOUT_CONCURRENCY
static void
worker_sync2(struct sml_worker *worker)
{
        sml_heap_worker_sync2(worker->thread_local_heap);
        extern void sbcl_thread_sync2(struct thread*);
        sbcl_thread_sync2(worker->control.vm_thread);
        /* all updates by this thread must happen before here */
        decr_sync_counter_release();
}
#endif /* WITHOUT_CONCURRENCY */

#ifdef WITHOUT_MASSIVETHREADS
static void
user_leave(struct sml_user *user ATTR_UNUSED)
{
}
#endif /* WITHOUT_MASSIVETHREADS */

#ifndef WITHOUT_MASSIVETHREADS
static void
user_leave(struct sml_user *user)
{
        unsigned int old;
        assert(IS_ACTIVE(load_relaxed(&user->control.state)));
        /* SYNC2 -> ASYNC */
        old = swap(release, &user->control.state, INACTIVE(ASYNC));
        if (old == ACTIVE(PRESYNC2))
                user_sync2(user);
}
#endif /* WITHOUT_MASSIVETHREADS */

#ifdef WITHOUT_MULTITHREAD
static void
worker_leave(struct sml_worker *worker ATTR_UNUSED)
{
}
#endif /* WITHOUT_MULTITHREAD */

#if !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY
static void
worker_leave(struct sml_worker *worker)
{
        assert(load_relaxed(&worker->control.state) == ACTIVE(ASYNC));
        /* all updates by this thread must happen before here */
        store_release(&worker->control.state, INACTIVE(ASYNC));
        if (load_relaxed(&stop_the_world_flag)) {
                mutex_lock(&worker->control.inactive_wait_lock);
                cond_signal(&worker->control.inactive_wait_cond);
                mutex_unlock(&worker->control.inactive_wait_lock);
        }
}
#endif /* !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY */

#ifndef WITHOUT_CONCURRENCY
static void
worker_leave(struct sml_worker *worker)
{
        unsigned int old;
        assert(IS_ACTIVE(load_relaxed(pCTRL_STATE(worker->control))));
        /* all updates by this thread must happen before here */
        /* PRESYNC1 -> SYNC1 or PRESYNC2 -> SYNC2 */
        old = fetch_or(release, pCTRL_STATE(worker->control), INACTIVE_FLAG | 1);
        if (old == ACTIVE(PRESYNC1)) {
                worker_sync1(worker);
        } else if (old == ACTIVE(PRESYNC2)) {
#ifdef WITHOUT_MASSIVETHREADS
                user_sync2(worker->user);
#endif /* WITHOUT_MASSIVETHREADS */
                worker_sync2(worker);
        }
#ifndef WITHOUT_MASSIVETHREADS
        if (!(load_relaxed(&worker->control.flags) & PTHREAD_FLAG))
                worker->user = NULL;
#endif /* !WITHOUT_MASSIVETHREADS */
}
#endif /* WITHOUT_CONCURRENCY */

SML_PRIMITIVE void
sml_leave()
{
  // transfer of control from SML to C.
  // As long as the user has left the managed context, if the collector asks for
  // co-operation from user code (to get roots or change phase), the collector will
  // perform the action on behalf of the user thread.
        struct sml_worker *worker = worker_tlv_get(current_worker);
        assert(worker->user->frame_stack->top == NULL);
        worker->user->frame_stack->top = CALLER_FRAME_END_ADDRESS();
        user_leave(worker->user);
        worker_leave(worker);
}

void *
sml_leave_internal(void *frame_pointer)
{
        struct sml_worker *worker = worker_tlv_get(current_worker);
        void *old_frame_top = worker->user->frame_stack->top;
        TPRINTF(0, "leaving managed context. FP=%p old=%p", frame_pointer, old_frame_top);
        if (!old_frame_top)
                worker->user->frame_stack->top = frame_pointer;
        user_leave(worker->user);
        worker_leave(worker);
        return old_frame_top;
}

#ifdef WITHOUT_MASSIVETHREADS
static void
user_enter(struct sml_user *user ATTR_UNUSED)
{
}
#endif /* WITHOUT_MASSIVETHREADS */

#ifndef WITHOUT_MASSIVETHREADS
static void
user_enter(struct sml_user *user)
{
        activate_myth(&user->control);

        if (load_relaxed(&user->control.state) == ACTIVE(PRESYNC2)) {
                store_relaxed(&user->control.state, ACTIVE(ASYNC));
                user_sync2(user);
        }
}
#endif /* !WITHOUT_MASSIVETHREADS */

#ifdef WITHOUT_MULTITHREAD
static struct sml_worker *
worker_enter(struct sml_worker *worker, struct sml_user *user ATTR_UNUSED)
{
        return worker;
}
#endif /* WITHOUT_MULTITHREAD */

#if !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY
static struct sml_worker *
worker_enter(struct sml_worker *worker, struct sml_user *user ATTR_UNUSED)
{
        struct control *control = &worker->control;
        unsigned int old;

        /* lock; all updates so far must be acquired */
        old = INACTIVE(ASYNC);
        if (cmpswap_weak_acquire(&control->state, &old, ACTIVE(ASYNC)))
                return;

        mutex_lock(&control->inactive_wait_lock);
        pthread_cleanup_push(cleanup_mutex_unlock,
                             &control->inactive_wait_lock);
        old = INACTIVE(ASYNC);
        while (!cmpswap_weak_acquire(&control->state, &old, ACTIVE(ASYNC))) {
                cond_wait(&control->inactive_wait_cond,
                          &control->inactive_wait_lock);
                old = INACTIVE(ASYNC);
        }
        pthread_cleanup_pop(1);

        return worker;
}
#endif /* !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY */

#if !defined WITHOUT_CONCURRENCY && defined WITHOUT_MASSIVETHREADS
static struct sml_worker *
worker_enter(struct sml_worker *worker, struct sml_user *user ATTR_UNUSED)
{
        activate(&worker->control);
        return worker;
}
#endif /* !defined WITHOUT_CONCURRENCY && defined WITHOUT_MASSIVETHREADS */

#ifndef WITHOUT_MASSIVETHREADS
static struct sml_worker *
worker_enter(struct sml_worker *worker, struct sml_user *user)
{
        if (worker) {
                activate(&worker->control);
        } else {
                worker = worker_new();
                worker_register(worker);
        }
        worker->user = user;
        return worker;
}
#endif /* !WITHOUT_MASSIVETHREADS */

SML_PRIMITIVE void
sml_enter()
{
        struct sml_user *user = NULL;
        struct sml_worker *worker;
#ifndef WITHOUT_MASSIVETHREADS
        if (myth_is_myth_worker()) {
                user = user_tlv_get(current_user);
        } else {
                worker = worker_tlv_get(current_worker);
                user = worker->user;
        }
#endif /* WITHOUT_MASSIVETHREADS */
        user_enter(user);
        worker = worker_tlv_get(current_worker);
        worker = worker_enter(worker, user);
        assert(worker->user->frame_stack->top == CALLER_FRAME_END_ADDRESS());
        worker->user->frame_stack->top = NULL;
        TPRINTF(0, "re-entered Lisp");
}

void
sml_enter_internal(void *old_frame_top)
{
  TPRINTF(0, "re-entering managed context with old_frame_top=%p", old_frame_top);
        struct sml_user *user = NULL;
        struct sml_worker *worker = worker_tlv_get(current_worker);
#ifndef WITHOUT_MASSIVETHREADS
        if (load_relaxed(&worker->control.flags) & PTHREAD_FLAG)
                user = worker->user;
        else
                user = user_tlv_get(current_user);
#endif /* WITHOUT_MASSIVETHREADS */
        user_enter(user);
        worker = worker_tlv_get(current_worker);
        worker = worker_enter(worker, user);
        worker->user->frame_stack->top = old_frame_top;
}

#if defined WITHOUT_MASSIVETHREADS && !defined WITHOUT_CONCURRENCY
static void
user_sync2_check(struct sml_user *user, void *frame_pointer)
{
        user_sync2_with(user, frame_pointer);
}
#endif /* WITHOUT_MASSIVETHREADS && !WITHOUT_CONCURRENCY */

#ifndef WITHOUT_MASSIVETHREADS
static void
user_sync2_check(struct sml_user *user, void *frame_pointer)
{
        unsigned int state = load_relaxed(&user->control.state);
        if (state == ACTIVE(PRESYNC2)) {
                store_relaxed(&user->control.state, ACTIVE(ASYNC));
                user_sync2_with(user, frame_pointer);
        }
}
#endif /* WITHOUT_MASSIVETHREADS */

#ifdef WITHOUT_MULTITHREAD
void
sml_check_internal(void *frame_pointer ATTR_UNUSED)
{
}
#endif /* WITHOUT_MULTITHREAD */

char *phase_names[8] = {"?", "ASYNC", "PRESYNC1", "SYNC1", "PRESYNC2", "SYNC2", "", "MARK"};
char *phase_names_inactive[8] =
{"-?","-<ASYNC>", "-<PRESYNC1>", "-<SYNC1>", "-<PRESYNC2>", "-<SYNC2>", "", "-<MARK>"};

#ifndef WITHOUT_CONCURRENCY
extern FILE* get_log_file_for_thread();
extern void backtrace_to_file(FILE*f);

/*void poll_until_phase(long dummy, enum sml_sync_phase this_phase,
                      enum sml_sync_phase desired_phase) {
  enum sml_sync_phase newphase;
  struct timespec sleeptime;
  TPRINTF(0, "==== waiting for %s phase ====", phase_name(desired_phase));
  FILE* f = get_log_file_for_thread();
  if (!f) f = stderr;
  fprintf(f, "Waiting for phase %s:\n", phase_name(desired_phase));
  backtrace_to_file(f);
  int i = 0;
  sleeptime.tv_sec = 0;
  sleeptime.tv_nsec = (1000*1000*1000)/10000;
  for(;;) {
    ++i;
    sml_check_internal(&dummy);
    newphase = sml_current_phase();
    if (newphase != this_phase) {
      TPRINTF(0, "detected phase change to %s (sleep iteration %d)", phase_name(newphase), i);
      this_phase = newphase;
      if (this_phase == desired_phase) break;
    } else {
      nanosleep(&sleeptime,0);
      if (sleeptime.tv_nsec < (1000*1000*1000)/10) sleeptime.tv_nsec *= 2;
    }
  }
  TPRINTF(0, "==== resuming ===");
  fprintf(f, "Resuming\n\n");
}*/

ATTR_UNUSED static int heapdump_counter;

__thread unsigned int last_observed_gc_state;
void
sml_check_internal(void *frame_pointer)
{
        struct sml_worker *worker = worker_tlv_get(current_worker);
        unsigned int state = load_relaxed(pCTRL_STATE(worker->control));

        assert(IS_ACTIVE(state));

        if (state != last_observed_gc_state) {
          TPRINTF(2, "check_internal: phase chage %x to %x", last_observed_gc_state, state);
          struct thread* th = current_thread;
          if (th) {
            TPRINTF(2, "new_obj=%d stack_store=%d heap_store=%d barrier=%d spinwait=%d",
                    th->ct_new_objects, th->ct_stack_obj_stores, th->ct_heap_obj_stores,
                    th->ct_store_barriers, th->ct_spinlock_yields);
            th->ct_new_objects = th->ct_stack_obj_stores = th->ct_heap_obj_stores
                               = th->ct_store_barriers = th->ct_spinlock_yields = 0;
          }
          last_observed_gc_state = state;
        }

        switch(state) {
        case ACTIVE(PRESYNC1):
#if 0
                if (enable_async_gc && getenv("SMLGC_SYNC1_DUMP")) {
                    ++heapdump_counter;
                    char pathbuf[100];
                    int n = snprintf(pathbuf, sizeof pathbuf, "heapsnap%d.txt", heapdump_counter);
                    (void)n;
                    hexdump_sml_heap_to_file(pathbuf);
                    TPRINTF(0, "Dumped heap to '%s'", pathbuf);
                }
#endif
                store_relaxed(pCTRL_STATE(worker->control), ACTIVE(SYNC1));
                worker_sync1(worker);
                // if (!enable_async_gc) poll_until_phase(0, ACTIVE(SYNC1), ASYNC);
                break;
        case ACTIVE(PRESYNC2):
                store_relaxed(pCTRL_STATE(worker->control), ACTIVE(SYNC2));
                user_sync2_check(worker->user, frame_pointer);
                worker_sync2(worker);
                break;
        }
}

void cooperate_with_gc(uword_t stackptr_at_interrupt) {
    TPRINTF(1, "co-operate with GC");
    sml_check_internal((void*)stackptr_at_interrupt);
}

#endif /* !WITHOUT_CONCURRENCY */

void sml_call_with_cleanup(void(*)(void), void(*)(void*,void*,void*), void*);
struct signal_cleanup_arg {
        void *frame_pointer;
        void (*signal_handler)(void);
};

#ifndef LISP_FEATURE_SBCL
static void
signal_cleanup(void *arg, void *u, void *e)
{
        struct signal_cleanup_arg *a = arg;
        signal_handler = a->signal_handler;
        sml_enter_internal(a->frame_pointer);
        sml_unsave_exn(e);
}
#endif

#ifndef LISP_FEATURE_SBCL
SML_PRIMITIVE void
sml_check(unsigned int flag)
{
        void *frame_pointer = CALLER_FRAME_END_ADDRESS();
        assert(worker_tlv_get(current_worker)->user->frame_stack->top
               == NULL);

        if ((flag & FLAG_SIGNAL)
            && load_relaxed(&signal_receiver) == worker_tlv_get(current_worker)
            && signal_handler != NULL) {
                struct signal_cleanup_arg a;
                fetch_and(acquire, &sml_check_flag, ~FLAG_SIGNAL);
                a.frame_pointer = sml_leave_internal(frame_pointer);
                a.signal_handler = signal_handler;
                signal_handler = NULL;
                sml_call_with_cleanup(a.signal_handler, signal_cleanup, &a);
        }

        sml_check_internal(frame_pointer);
}
#endif

#ifdef WITHOUT_MULTITHREAD
enum sml_sync_phase
sml_current_phase()
{
        return ASYNC;
}
#endif /* WITHOUT_MULTITHREAD */

#ifndef WITHOUT_MULTITHREAD
enum sml_sync_phase
sml_current_phase()
{
        struct sml_worker *worker = worker_tlv_get(current_worker);
        /* Thanks to memory coherency, control.state always indicates
         * the current status of this mutator regardless of the fact that
         * both the mutator and collector updates it.
         * If worker->control.state is SYNC1, then the thread is in SYNC1
         * at this instant.
         */
        return PHASE(load_relaxed(pCTRL_STATE(worker->control)));
}
/*void* sml_current_phase_pointer() {
        struct sml_worker *worker = worker_tlv_get(current_worker);
        return &worker->control.state;
}*/
#endif /* !WITHOUT_MULTITHREAD */

SML_PRIMITIVE void
sml_save()
{
        struct sml_worker *worker = worker_tlv_get(current_worker);
        struct sml_user *user = worker->user;
        assert(IS_ACTIVE(load_relaxed(pCTRL_STATE(worker->control))));
        assert(user->frame_stack->top == NULL);
        user->frame_stack->top = CALLER_FRAME_END_ADDRESS();
}

SML_PRIMITIVE void
sml_unsave()
{
        struct sml_worker *worker = worker_tlv_get(current_worker);
        struct sml_user *user = worker->user;
        assert(IS_ACTIVE(load_relaxed(pCTRL_STATE(worker->control))));
        assert(user->frame_stack->top == CALLER_FRAME_END_ADDRESS());
        user->frame_stack->top = NULL;
}

SML_PRIMITIVE void
sml_save_exn(void *obj)
{
        struct sml_worker *worker = worker_tlv_get(current_worker);
        struct sml_user *user = worker->user;
        if (user->exn_object)
                sml_fatal(0, "unhandled exception during exception handling");
        user->exn_object = obj;
}

SML_PRIMITIVE void *
sml_unsave_exn(void *p)
{
        struct sml_worker *worker;
        struct sml_user *user;
        void *ret;
        if (!p)
                return NULL;
        worker = worker_tlv_get(current_worker);
        user = worker->user;
        ret = user->exn_object;
        assert(ret != NULL);
        user->exn_object = NULL;
        return ret;
}

/* for debug */
#ifndef NDEBUG
int
sml_saved()
{
        struct sml_worker *worker = worker_tlv_get(current_worker);
        struct sml_user *user = worker->user;
#ifndef WITHOUT_MASSIVETHREADS
        if (!(load_relaxed(&worker->control.flags) & PTHREAD_FLAG))
                user = user_tlv_get(current_user);
#endif /* WITHOUT_MASSIVETHREADS */
        return user->frame_stack->top != NULL;
}
#endif /* NDEBUG */

static struct timespec wallclock;
void
sml_control_init()
{
        worker_tlv_init(current_worker);
#ifndef WITHOUT_MASSIVETHREADS
        user_tlv_init(current_user);
#endif /* !WITHOUT_MASSIVETHREADS */
#ifdef USE_SYNC_SEM
        os_sem_init(&gc_sync_semaphore, 0);
#elif defined USE_SYNC_PIPE
        pipe(sync_pipe);
        assert(sync_pipe[0] != 0 || sync_pipe[1] != 0);
#endif
        atomic_init(&sml_check_flag, 0);
        clock_gettime(CLOCK_MONOTONIC, &wallclock);
}

SML_PRIMITIVE void
sml_start(void *arg)
{
        struct frame_stack_range *range = arg;
        struct sml_user *user = NULL;
        struct sml_worker *worker;
        range->bottom = CALLER_FRAME_END_ADDRESS();
        range->top = NULL;

        fprintf(stderr, "sml_start arg=%p range=%p:%p\n", arg, range->bottom, range->top);
#ifndef WITHOUT_MASSIVETHREADS
        worker = worker_tlv_get(current_worker);
        if (worker) {
                user = load_relaxed(&worker->control.flags) & PTHREAD_FLAG
                        ? worker->user : user_tlv_get(current_user);
        } else {
                user = myth_is_myth_worker()
                        ? user_tlv_get(current_user) : NULL;
        }
        if (user) {
                user_enter(user);
                worker = worker_tlv_get(current_worker);
                worker = worker_enter(worker, user);
        } else {
                user = user_new();
                worker = worker_tlv_get(current_worker);
                worker = worker_enter(worker, user);
                user_register(user, worker);
        }
#else /* !WITHOUT_MASSIVETHREADS */
        worker = worker_tlv_get(current_worker);
        if (worker) {
                worker = worker_enter(worker, user);
        } else {
                worker = worker_new();
                worker->user = user_new();
                worker_register(worker);
        }
#endif /* !WITHOUT_MASSIVETHREADS */

        range->next = worker->user->frame_stack;
        worker->user->frame_stack = range;
}

SML_PRIMITIVE void
sml_end()
{
        struct sml_worker *worker = worker_tlv_get(current_worker);
        struct sml_user *user = worker->user;

        fprintf(stderr, "sml_end: vmthread = %p\n", worker->control.vm_thread);
        assert(IS_ACTIVE(load_relaxed(pCTRL_STATE(worker->control))));
        if (user->frame_stack->bottom == CALLER_FRAME_END_ADDRESS()) {
        } else {
          // shouldn't matter, because user_leave does nothing with the stack
          // if no massivethreads.
          fprintf(stderr, "sml_end: frame stack bottom not right? oh well (%p vs %p)\n",
                  user->frame_stack->bottom, CALLER_FRAME_END_ADDRESS());
        }

        user->frame_stack = user->frame_stack->next;
        user_leave(worker->user);
        worker_leave(worker);
}

#ifndef WITHOUT_MASSIVETHREADS
static struct control **
users_gc(struct control **p)
{
        struct control *control;

        while ((control = *p)) {
                if (load_acquire(&control->flags) & CANCELED_FLAG) {
                        *p = control->next;
                        user_destroy((struct sml_user *)control);
                } else {
                        p = &control->next;
                }
        }
        return p;
}
#endif /* WITHOUT_MASSIVETHREADS */

#ifndef WITHOUT_MASSIVETHREADS
static void
gather_users(struct sml_worker *worker)
{
        struct control **new_users;

        new_users = users_gc(&users);

        for (; worker; worker = (struct sml_worker *)worker->control.next) {
                *new_users = swap(acquire, &worker->new_users, NULL);
                new_users = users_gc(new_users);
        }
}
#endif /* !WITHOUT_MASSIVETHREADS */

#ifndef WITHOUT_MULTITHREAD
static void
control_gc()
{
        struct control *first_worker, **p;
        struct control ATTR_UNUSED **new_users;
        struct sml_worker *w;

#ifndef WITHOUT_MASSIVETHREADS
        new_users = users_gc(&users);
#endif /* !WITHOUT_MASSIVETHREADS */

        first_worker = load_acquire(&workers);
        TPRINTF(0, "control_gc: first_worker=%p", first_worker);
        if (!first_worker)
                return;

        /* destroy canceled workers except for the first one */
        p = &first_worker->next;
        while ((w = (struct sml_worker *)*p)) {
                if (load_acquire(&w->control.flags) & CANCELED_FLAG) {
#ifndef WITHOUT_MASSIVETHREADS
                        *new_users = load_acquire(&w->new_users);
                        new_users = users_gc(new_users);
#endif /* !WITHOUT_MASSIVETHREADS */
                        *p = w->control.next;
                        worker_destroy(w);
                } else {
#ifndef WITHOUT_MASSIVETHREADS
                        *new_users = swap(acquire, &w->new_users, NULL);
                        new_users = users_gc(new_users);
#endif /* !WITHOUT_MASSIVETHREADS */
                        p = &w->control.next;
                }
        }

        /* if the first one is canceled, destroy it if we can occupy it */
        if ((load_acquire(&first_worker->flags) & CANCELED_FLAG)
            && cmpswap_acquire(&workers, &first_worker, first_worker->next)) {
                w = (struct sml_worker *)first_worker;
#ifndef WITHOUT_MASSIVETHREADS
                *new_users = load_acquire(&w->new_users);
                new_users = users_gc(new_users);
#endif /* !WITHOUT_MASSIVETHREADS */
                worker_destroy(w);
        }
}
#endif /* !WITHOUT_MULTITHREAD */

#if defined WITHOUT_MULTITHREAD && !defined WITHOUT_CONCURRENCY
void
sml_detach()
{
        struct sml_worker *worker = worker_tlv_get_or_init(current_worker);
        if (worker) {
                if (user->frame_stack != NULL)
                        sml_fatal(0, "sml_detach: ML code is running");
                user_destroy(worker->user);
                worker_destroy(worker);
        }
        worker_tlv_set(current_worker, NULL);
}
#endif /* WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#ifndef WITHOUT_MULTITHREAD
void
sml_detach()
{
    TPRINTF(1, "sml_detach");
    // why would a thread with no 'struct sml_worker' ever get detached?
        struct sml_worker *worker = worker_tlv_get_or_init(current_worker);
        struct sml_user *user = worker ? worker->user : NULL;

#ifndef WITHOUT_MASSIVETHREADS
        if (myth_is_myth_worker()) {
                user = user_tlv_get_or_init(current_user);
                user_tlv_set(current_user, NULL);
        }
        if (user)
                cancel(user);
#endif /* WITHOUT_MASSIVETHREADS */

        if (user && user->frame_stack != NULL)
                sml_fatal(0, "sml_detach: ML code is running");

        cancel(worker);
        worker_tlv_set(current_worker, NULL);
}
#endif /* !WITHOUT_MULTITHREAD */

#ifndef WITHOUT_CONCURRENCY
long this_gc_phase_time[4], cumulative_phase_time[4];
static void
change_phase(struct control *list,
             enum sml_sync_phase old, enum sml_sync_phase new)
{
  {
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  int index;
  switch (old) {
  case ASYNC: index = 0; break;
  case SYNC1: index = 1; break;
  case SYNC2: index = 2; break;
  case MARK:  index = 3; break;
  default: lose("bad phase change");
  }
  long delta = (now.tv_sec - wallclock.tv_sec)*1000000
               + (now.tv_nsec - wallclock.tv_nsec)/1000;
  this_gc_phase_time[index] = delta;
  wallclock = now;
  if (smlgc_verbose) printf("change phase %s -> %s\n", phase_name(old), phase_name(new));
  if (old==MARK) {
    cumulative_phase_time[0] += this_gc_phase_time[0];
    cumulative_phase_time[1] += this_gc_phase_time[1];
    cumulative_phase_time[2] += this_gc_phase_time[2];
    cumulative_phase_time[3] += this_gc_phase_time[3];
    long this_sum = this_gc_phase_time[0] + this_gc_phase_time[1] +
                    this_gc_phase_time[2] + this_gc_phase_time[3];
    long tot_sum = cumulative_phase_time[0] + cumulative_phase_time[1] +
                   cumulative_phase_time[2] + cumulative_phase_time[3];
    int this_pct_async = 100 * this_gc_phase_time[0] / this_sum;
    int this_pct_sync2 = 100 * this_gc_phase_time[2] / this_sum;
    int this_pct_mark  = 100 * this_gc_phase_time[3] / this_sum;
    // The percent time in sync1 is negligible. Just absorb whatever
    // amount makes the sum of percents 100
    int this_pct_sync1 = 100 - (this_pct_async + this_pct_sync2 + this_pct_mark);
    int tot_pct_async = 100 * cumulative_phase_time[0] / tot_sum;
    int tot_pct_sync2 = 100 * cumulative_phase_time[2] / tot_sum;
    int tot_pct_mark  = 100 * cumulative_phase_time[3] / tot_sum;
    int tot_pct_sync1 = 100 - (tot_pct_async + tot_pct_sync2 + tot_pct_mark);
    printf("Phase time(asy/syn1/syn2/mrk): cur=%ld+%ld+%ld+%ld (%d/%d/%d/%d) tot=(%d/%d/%d/%d)\n",
           this_gc_phase_time[0], this_gc_phase_time[1],
           this_gc_phase_time[2], this_gc_phase_time[3],
           this_pct_async, this_pct_sync1, this_pct_sync2, this_pct_mark,
           tot_pct_async, tot_pct_sync1, tot_pct_sync2, tot_pct_mark);
  }
  }
  TPRINTF(2, "change-phase %s -> %s", phase_name(old), phase_name(new));
        struct control *control;
        unsigned int state ATTR_UNUSED;

        for (control = list; control; control = control->next) {
                state = fetch_xor(relaxed, pCTRL_STATE(*control), old ^ new);
                assert(PHASE(state) == old);
        }
}
#endif /* !WITHOUT_CONCURRENCY */

#ifndef WITHOUT_CONCURRENCY
#if USE_SYNC_SEM
static void wait_for_sync(int count) {
    TPRINTF(1, "waiting on sync semaphore (count=%d)", count);
    do os_sem_wait(&gc_sync_semaphore); while (--count);
}
#endif

static void raise_gcsignal(struct control* control, ATTR_UNUSED char *reason) {
        if (!use_gcsignal) return;
        struct thread* vmthread = control->vm_thread;
        if (vmthread->os_kernel_tid == 0) {
            lose("vmthread %p exists but has no OS thread", vmthread);
        }
        TPRINTF(1, "signaling ACTIVE thread %p for %s", vmthread, reason);
        pthread_kill(vmthread->os_thread, SIG_STOP_FOR_GC);
}

static void
sync1(struct control *workers)
{
        struct control *control;
        unsigned int old, new, count = 0;

        assert(load_acquire(&sync_counter) == 0);
        fetch_or(relaxed, &sml_check_flag, FLAG_GC);

        for (control = workers; control; control = control->next) {
                pthread_mutex_lock(&control->state_lock);
                assert(control->vm_thread);
                //if (!vmthread) lose("can't phase change control %p - no vm_thread", control);
                old = INACTIVE(PRESYNC1);
                new = INACTIVE(SYNC1);
                if (cmpswap_relaxed(pCTRL_STATE(*control), &old, new)) {
                        TPRINTF(1, "SYNC1 on behalf of INACTIVE %p", control);
                        worker_sync1((struct sml_worker *)control);
                } else {
                        raise_gcsignal(control, "sync1");
                }
                pthread_mutex_unlock(&control->state_lock);
                count++;
        }

#if USE_SYNC_SEM
        fetch_add(relaxed, &sync_counter, count);
        wait_for_sync(count);
#else
        if (fetch_add(relaxed, &sync_counter, count) + count != 0) {
                mutex_lock(&sync_wait_lock);
                while (!(load_relaxed(&sync_counter) == 0))
                        cond_wait(&sync_wait_cond, &sync_wait_lock);
                mutex_unlock(&sync_wait_lock);
        }
#endif

        fetch_and(relaxed, &sml_check_flag, ~FLAG_GC);

        sml_heap_collector_sync1();
}
#endif /* !WITHOUT_CONCURRENCY */

#ifndef WITHOUT_CONCURRENCY
#ifndef WITHOUT_MASSIVETHREADS
static void
sync2_1()
{
        struct control *control;
        unsigned int old, new, count = 0;

        fetch_or(relaxed, &sml_check_flag, FLAG_GC);

        for (control = users; control; control = control->next) {
                old = INACTIVE(PRESYNC2);
                new = ACTIVE(ASYNC);
                /* all updates so far must happen before here */
                if (cmpswap_acquire(&control->state, &old, new)) {
                        user_sync2((struct sml_user *)control);
                        /* all updates by this thread must happen before here */
                        store_release(&control->state, INACTIVE(ASYNC));
                }
                count++;
        }

        /* all updates so far must happen before here */
        if (fetch_add(acquire, &sync_counter, count) + count != 0) {
                mutex_lock(&sync_wait_lock);
                while (!(load_acquire(&sync_counter) == 0))
                        cond_wait(&sync_wait_cond, &sync_wait_lock);
                mutex_unlock(&sync_wait_lock);
        }

        fetch_and(relaxed, &sml_check_flag, ~FLAG_GC);
}
#endif

static void
sync2(struct control *workers)
{
        struct control *control;
        unsigned int old, new, count = 0;

        assert(load_acquire(&sync_counter) == 0);
        sml_heap_collector_sync2();

        fetch_or(relaxed, &sml_check_flag, FLAG_GC);

        for (control = workers; control; control = control->next) {
                pthread_mutex_lock(&control->state_lock);
                assert(control->vm_thread);
                // if (!vmthread) lose("can't phase change control %p - no vm_thread", control);
                old = INACTIVE(PRESYNC2);
                new = ACTIVE(SYNC2);
                /* all updates so far must happen before here */
                if (cmpswap_acquire(pCTRL_STATE(*control), &old, new)) {
                        TPRINTF(1, "SYNC2 on behalf of INACTIVE %p", control);
                        struct sml_worker *w = (struct sml_worker *)control;
#ifdef WITHOUT_MASSIVETHREADS
                        user_sync2(w->user);
#endif /* WITHOUT_MASSIVETHREADS */
                        worker_sync2(w);
                        /* all updates by this thread must happen before here */
                        store_release(pCTRL_STATE(*control), INACTIVE(SYNC2));
                } else {
                        raise_gcsignal(control, "sync2");
                }
                pthread_mutex_unlock(&control->state_lock);
                count++;
        }

        /* all updates so far must happen before here */
#if USE_SYNC_SEM
        fetch_add(relaxed, &sync_counter, count);
        wait_for_sync(count);
#else
        if (fetch_add(acquire, &sync_counter, count) + count != 0) {
                mutex_lock(&sync_wait_lock);
                while (!(load_acquire(&sync_counter) == 0))
                        cond_wait(&sync_wait_cond, &sync_wait_lock);
                mutex_unlock(&sync_wait_lock);
        }
#endif

        fetch_and(relaxed, &sml_check_flag, ~FLAG_GC);
}
#endif /* WITHOUT_CONCURRENCY */

#ifdef WITHOUT_MULTITHREAD
void
sml_gc()
{
        struct sml_worker *worker = worker_tlv_get(current_worker);

        sml_heap_worker_sync2(worker->thread_local_heap);
        sml_heap_collector_sync2();
        sml_heap_user_sync2(worker->user);
        sml_heap_collector_mark();
        sml_run_finalizer();
        sml_heap_collector_async();
}
#endif /* WITHOUT_MULTITHREAD */

#if !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY
void
sml_gc()
{
        /* stop-the-world garbage collection */
        struct control *control;

        mutex_lock(&thread_creation_lock);
        ASSERT(load_relaxed(&stop_the_world_flag));

        fetch_or(relaxed, &sml_check_flag, FLAG_GC);

        for (control = workers; control; control = control->next)
                activate(control);

        fetch_and(relaxed, &sml_check_flag, ~FLAG_GC);

        for (control = workers; control; control = control->next) {
                struct sml_worker *worker = (struct sml_worker *)control;
                sml_heap_worker_sync2(worker->thread_local_heap);
        }
        sml_heap_collector_sync2();
        for (control = workers; control; control = control->next) {
                struct sml_worker *worker = (struct sml_worker *)control;
                sml_heap_user_sync2(worker->user);
        }
        sml_heap_collector_mark();
        sml_run_finalizer();
        sml_heap_collector_async();

        control_gc(&workers, worker_destroy);

        for (control = workers; control; control = control->next) {
                mutex_lock(&control->inactive_wait_lock);
                store_release(&control->state, INACTIVE(ASYNC));
                cond_signal(&control->inactive_wait_cond);
                mutex_unlock(&control->inactive_wait_lock);
        }

        mutex_unlock(&control_blocks_lock);
}
#endif /* !defined WITHOUT_MULTITHREAD && defined WITHOUT_CONCURRENCY */

#ifndef WITHOUT_CONCURRENCY
extern pthread_rwlock_t valid_obj_lock;
void
sml_gc()
{
  struct timespec now;
  extern struct timespec lisp_init_time;
  clock_gettime(CLOCK_MONOTONIC, &now);
  int sec = now.tv_sec - lisp_init_time.tv_sec;
  long millisec = (now.tv_nsec - lisp_init_time.tv_nsec) / 1000000;
  float fsec = (float)sec + (float)millisec/1000.0;
  fprintf(stderr, "[%f] GC cycle %d\n", fsec, get_gc_cycle_number());
  TPRINTF(1, "start of cycle %d", get_gc_cycle_number());
        struct control *current_workers;
        control_gc();

        assert(load_relaxed(&sync_counter) == 0);

        /* SYNC1: turn on snooping and snapshot barriers */

        /* all new worker threads must be in SYNC1. */
        spin_lock(&worker_creation_lock);
        new_worker_phase = SYNC1;
        current_workers = load_relaxed(&workers);
        spin_unlock(&worker_creation_lock);

        /* Setting the refState to REPEAT now, before tracing, is the right thing
         * because it causes mutators to record loads through weak objects
         * without checking the referent color. */
        if (smlgc_verbose) printf("Setting refState_REPEAT\n");
        atomic_store(&weakRefState, weakRefState_REPEAT);
        change_phase(current_workers, ASYNC, PRESYNC1);

        sync1(current_workers);

        /* SYNC2: rootset & allocation pointer snapshot */

        /* all new worker threads must be in SYNC2. */
        spin_lock(&worker_creation_lock);
        new_worker_phase = SYNC2;
        current_workers = load_relaxed(&workers);
        spin_unlock(&worker_creation_lock);

#ifndef WITHOUT_MASSIVETHREADS
        /* users must precede workers */
        gather_users((struct sml_worker *)current_workers);
        change_phase(users, ASYNC, PRESYNC2);
        sync2_1();
#endif /* !WITHOUT_MASSIVETHREADS */
        change_phase(current_workers, SYNC1, PRESYNC2);

        sync2(current_workers);

        /* MARK: turn off snooping barrier */

        /* all new worker threads must be in MARK. */
        spin_lock(&worker_creation_lock);
        new_worker_phase = MARK;
        current_workers = load_relaxed(&workers);
        spin_unlock(&worker_creation_lock);

        change_phase(current_workers, SYNC2, MARK);

        if (smlgc_verbose) fprintf(stderr, "START MARK\n");
        sml_heap_collector_mark();

        /* Mutators can operate with the store barrier off, but can not freely reference
         * weak objects yet. We need to clear the dead ones first. The marking bitmap
         * is intact, so mutators can and must utilize the bitmap to decide which weak
         * refs are live.
         * Weak objects newly allocated after the marking cycle can only point to black
         * objects because there is no way to hold a white or gray reference.
         * Unfortunately, if we were to change phase to ASYNC now, it would cause confusion
         * as to whether large objects are allocated black, which would make
         * heap_check_alive potentially return no when it meant yes. */

        /* ASYNC: turn off snapshot barrier */
        spin_lock(&worker_creation_lock);
        new_worker_phase = ASYNC;
        current_workers = load_relaxed(&workers);
        spin_unlock(&worker_creation_lock);

        pthread_rwlock_wrlock(&valid_obj_lock);
        change_phase(current_workers, MARK, ASYNC);

        sml_heap_collector_after_mark();
#ifdef LISP_FEATURE_SBCL
        extern void finalizer_thread_wake();
        SYMBOL(GC_EPOCH)->value = make_fixnum(get_gc_cycle_number());
        finalizer_thread_wake();
#else
        sml_run_finalizer();
#endif
        sml_heap_collector_async();
        pthread_rwlock_unlock(&valid_obj_lock);
}

#endif /* !WITHOUT_MULTITHREAD && !WITHOUT_CONCURRENCY */

#ifndef LISP_FEATURE_SBCL
static void *
frame_enum_ptr(void *frame_end, void (*trace)(void **, void *), void *data)
{
        void *codeaddr = FRAME_CODE_ADDRESS(frame_end);
        void *frame_begin, **slot;
        const struct sml_frame_layout *layout = sml_lookup_frametable(codeaddr);
        uint16_t num_roots, i;

        /* assume that the stack grows downwards. */
        frame_begin = (void**)frame_end + layout->frame_size;
        num_roots = layout->num_roots;

        for (i = 0; i < num_roots; i++) {
                slot = (void**)frame_end + layout->root_offsets[i];
                if (*slot)
                        trace(slot, data);
        }

        return NEXT_FRAME(frame_begin);
}

void
sml_stack_enum_ptr(struct sml_user *user,
                   void (*trace)(void **, void *), void *data)
{
        const struct frame_stack_range *range;
        void *fp, *next;

        if (user->exn_object)
                trace(&user->exn_object, data);

        for (range = user->frame_stack; range; range = range->next) {
                /* skip dummy ranges */
                if (range->bottom == NULL)
                        continue;
                assert(range->top != NULL);
                fp = range->top;
                for (;;) {
                        next = frame_enum_ptr(fp, trace, data);
                        if (fp == range->bottom)
                                break;
                        fp = next;
                }
        }
}
#endif

#ifdef WITHOUT_MULTITHREAD
void
sml_exit(int status)
{
        sml_finish();
        exit(status);
}
#endif /* WITHOUT_MULTITHREAD */

#ifndef WITHOUT_MULTITHREAD
void
sml_exit(int status)
{
        struct control *control;

        control = (struct control *)worker_tlv_get_or_init(current_worker);
        if (control)
                cancel(control);

#ifndef WITHOUT_MASSIVETHREADS
        control = myth_is_myth_worker()
                ? (struct control *)user_tlv_get_or_init(current_user)
                : NULL;
        if (control)
                cancel(control);
#endif /* WITHOUT_MASSIVETHREADS */

        sml_heap_stop();

        /* disallow thread creation and termination */
        spin_lock(&worker_creation_lock);

        control = load_relaxed(&workers);
#ifndef WITHOUT_MASSIVETHREADS
        control = users;
#endif /* WITHOUT_MASSIVETHREADS */

        /* If there is no thread other than this thread, release all
         * resources allocated by the runtime.  Otherwise, terminate the
         * process immediately without any cleanup. */
        if (control == NULL)
                sml_finish();

        exit(status);
}

#endif /* !WITHOUT_MULTITHREAD */

int worker_is_inactive() {
  struct sml_worker*w = (void*)workers;
  unsigned int state = load_relaxed(pCTRL_STATE(w->control));
  return state & INACTIVE_FLAG;
}
void* get_user_stack_hot_end() {
  return ((struct sml_worker*)workers)->user->frame_stack->top;
}

void smlgc_unregister_lisp_thread(struct thread* thread)
{
    struct sml_worker *worker = worker_tlv_get(current_worker);
    // stop-for-GC *must* be blocked. Consider: we just acquired the lock,
    // if GC wants to ask us to cooperate via the signal, GC needs to acquire
    // the lock in order to decide if we're alive. -> Deadlock.
    pthread_mutex_lock(&worker->control.state_lock);
    // This says to treat the pthread _as_ _if_ it no longer exists
    // (technically pthread_t is opaque and can't be tested for 0 or
    // other "known invalid" bit pattern)
    worker->control.vm_thread->os_kernel_tid = 0;
    //worker->control.vm_thread = NULL;
    pthread_mutex_unlock(&worker->control.state_lock);
    struct alloc_ptr* control_ap = (void*)worker->thread_local_heap;
    assert(control_ap[4].blocksize_bytes == 1<<4);
    int nbytes = 9*sizeof (struct alloc_ptr);
    memcpy(&control_ap[4], &thread->ap4, nbytes);
    memset(&thread->ap4, 0, nbytes);
}

#include "segment.inc"
void* find_owning_control(struct segment* seg, int blocksize_log2)
{
    struct control* c = workers;
    for ( ; c ; c = c->next ) {
      struct thread* th = c->vm_thread;
      if (th && blocksize_log2 >= 4) {
        struct alloc_ptr* ap = &th->ap4;
        ap += (blocksize_log2 - 4);
        if (segment_addr(ap->freebit.ptr) == seg) return c;
      } else {
        struct alloc_ptr* ap = (void*)((struct sml_worker*)c)->thread_local_heap;
        ap += blocksize_log2;
        if (segment_addr(ap->freebit.ptr) == seg) return c;
      }
    }
    return 0;
}
char* vm_thread_name(struct thread* th);
char* owning_thread_name(struct segment* seg, int blocksize_log2)
{
    struct control* c = find_owning_control(seg, blocksize_log2);
    if (!c) return 0;
    if (c->vm_thread)
      return vm_thread_name(c->vm_thread);
    else
      return "GC";
}
