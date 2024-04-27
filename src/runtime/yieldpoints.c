#define _GNU_SOURCE // for pthread_setname_np
#include <pthread.h>
#include <signal.h>
#include <stdio.h>
#include <limits.h> // INT_MAX
#include <errno.h>
#include "thread.h"
#include "interr.h"
#include "gc-assert.h"
#include "core.h"
#include "atomiclog.inc"

typedef enum { SUSPEND=1, ASYNC_SIGNAL=2 } yield_reason;

static int debug = 0;

typedef enum { UNKNOWN, IN_LISP, LISP_TO_C, C_TO_LISP } yieldpoint_kind;
int yp_kind_hit[4];

static __thread char threadname[20];
char *pthreadname(struct thread* th) {
    pthread_getname_np(th->os_thread, threadname, 20);
    //sprintf(threadname, "%p", th);
    return threadname;
}

char *thread_yieldpoint_trap_page_base(struct thread*th ) {
    return (char*)th - THREAD_HEADER_SLOTS*N_WORD_BYTES - THREAD_YIELDPOINT_PAGE_SIZE;
}
typedef enum { READ_WRITE, READ_ONLY, NO_ACCESS } yieldpoint_page_acc;
void yieldpoint_trap_toggle(struct thread* th,
                            yieldpoint_page_acc access)
{
    int prot_mode[3] = { OS_VM_PROT_ALL, PROT_READ, PROT_NONE };
    char* page_base = thread_yieldpoint_trap_page_base(th);
    // fprintf(stderr, "YP page protect: %d @ %p\n", THREAD_YIELDPOINT_PAGE_SIZE, page_base);
    mprotect(page_base, THREAD_YIELDPOINT_PAGE_SIZE, prot_mode[access]);
    th->yieldpoint_page_access = access;
}

#define INITIATE_GC 0x100
#define GC_CONTROLLER_QUIT 0x200

extern pthread_rwlock_t all_threads_lock;
pthread_mutex_t gc_condition_lock = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t gc_controller_cond = PTHREAD_COND_INITIALIZER;
pthread_cond_t gc_completion_cond = PTHREAD_COND_INITIALIZER;
pthread_t gc_controller;
pthread_t rdb_thread;
int gc_notification_reason;
os_sem_t gc_sync_sem;
long gc_cycle_count;

static lispobj* yieldpoint_trap_addr(struct thread* th) {
    return (lispobj*)((char*)th - 128);
}
static lispobj get_stackptr_at_foreign_call(struct thread* th) {
    return *yieldpoint_trap_addr(th);
}
static void clear_stackptr_at_foreign_call(struct thread* th) {
    *yieldpoint_trap_addr(th) = 0;
}
void gc_inhibitor_control(int flag) {
    if (flag)
      pthread_rwlock_rdlock(&all_threads_lock);
    else
      pthread_rwlock_unlock(&all_threads_lock);
}

extern int futex_wait(int*, int, long, unsigned long);
extern int futex_wake(int*, int);
// FIXME: uint32_t as per https://man7.org/linux/man-pages/man2/futex.2.html
// but our os.c files all use 'int'
static int32_t world_is_stopped;

static void force_tlab_slowpath(struct alloc_region* r, lispobj* spill)
{
    void* real_end = __sync_fetch_and_or(&r->end_addr, 0);
    /* By stuffing in 0 for 'end' the thread's allocator will fail its next comparison
     * against the TLAB limit, reverting to the slow path allocator, at which point it
     * observes the stop request.  The only synchronization needed with the mutator
     * is to ensure we've got an up-to-date copy of the actual end before clobbering it */
    while (1) {
        *spill = (lispobj)real_end;
        if (real_end == REGION_END_EMPTY_VALUE) break;
        void* oldval = __sync_val_compare_and_swap(&r->end_addr, real_end, 0);
        if (oldval == real_end) break;
        real_end = oldval;
    }
}
static void restore_tlab(struct alloc_region* r, lispobj* spill)
{
    if (r->end_addr == 0) {
        lispobj actual_end = *spill;
        gc_assert(actual_end != 0);
        r->end_addr = (void*)actual_end;
    }
    *spill = 0;
}

#define TELNET_SERVER 1
#if TELNET_SERVER
#define MAXTHREADS 50
static struct {
    struct thread* th;
    char in_foreign_code;
    char gc_waiting_on_spinlock;
    //char did_sem_post;
}  known_threads[MAXTHREADS];
static int n_known_threads;
static int sem_waits;
static int get_n_currently_waiting_on() { return sem_waits; }
#endif

// Acquire the locks in this order:
//   condition_lock, mutators lock, per-thread spinlock (one at a time)
void gc_stop_the_world() {
  event0("begin stw");
    struct thread* me = get_sb_vm_thread();
    struct thread* th;
    /* The global "stopped" flag is set *before* doing anything else
     * because as we're arming the trap for each thread in turn, any thread
     * that hits a C-to-Lisp yieldpoint should block at the futex_wait call
     * as though the world is already stopped */
    world_is_stopped = 1;
    sem_waits = 0;
    //    int sem_waits = 0;
    // TODO: for all threads _except_ the finalizer, stop them; then ask the finalizer
    // thread to run the pre-GC actions (clobber the hash-caches, etc) and stop itself.
    for_each_thread(th) {
#if TELNET_SERVER
        int ti = n_known_threads++;
        if (ti > MAXTHREADS) lose("Too many cooks");
        known_threads[ti].th = th;
#endif
        // Thread can't change its state to DEAD without acquiring the rwlock for reading
        // Therefore if non-dead, it will participate in stop-for-gc by waiting for a
        // stop-the-world trap, and it will respond with sem_post.
        if (th == me) continue;
        //
        known_threads[ti].gc_waiting_on_spinlock = 1;
        acquire_lock(&th->yieldpoint_spinlock);
        if (th->state_word.state == STATE_DEAD) {
            known_threads[ti].gc_waiting_on_spinlock = 0;
            release_lock(&th->yieldpoint_spinlock);
            continue;
        }
        // th->stepping.suspend = -1;
        if (debug) fprintf(stderr, "GC: will stop thread %p (%s)\n", th, pthreadname(th));
        __sync_fetch_and_or(&th->interrupt_reason, SUSPEND);
        force_tlab_slowpath(&th->cons_tlab, &th->spill_cons_tlab_end);
        force_tlab_slowpath(&th->sys_cons_tlab, &th->spill_sys_cons_tlab_end);
        force_tlab_slowpath(&th->mixed_tlab, &th->spill_mixed_tlab_end);
        force_tlab_slowpath(&th->sys_mixed_tlab, &th->spill_sys_mixed_tlab_end);
        yieldpoint_trap_toggle(th, READ_ONLY);
        lispobj stackptr = get_stackptr_at_foreign_call(th);
        yieldpoint_trap_toggle(th, NO_ACCESS);
        release_lock(&th->yieldpoint_spinlock);
        known_threads[ti].gc_waiting_on_spinlock = 0;
        event2("stopping %p, sp=%lx", th, stackptr);
        if (stackptr) {
            known_threads[ti].in_foreign_code = 1;
            /* Assume that the next instruction after a Lisp-to-C yieldpoint
             * is a call into C which pushes the return address on the stack.
             * This gets us the return PC "for free" relative to the #+sb-safepoint
             * technique which uses LEA [RIP+n] to compute and store the PC. */
            if ((lispobj*)stackptr - 1 >= th->control_stack_start)
                stackptr -= N_WORD_BYTES;
            gc_assert(stackptr >= (lispobj)th->control_stack_start &&
                      stackptr < (lispobj)th->control_stack_end);
            th->control_stack_pointer = (lispobj*)stackptr;
            if (debug)
              fprintf(stderr, "thread %p stackptr = %p = end-%dw\n",
                      th, th->control_stack_pointer,
                      (int)(th->control_stack_end - th->control_stack_pointer));
        }
        if (stackptr == 0) ++sem_waits; // the thread is in Lisp
    }
    if (debug) fprintf(stderr, "GC: sem_wait for %d\n", sem_waits);
    event1("waiting on %d threads", sem_waits);
    // Wait for all threads executing Lisp (i.e. not foreign code) to stop
    while (sem_waits--) sem_wait(&gc_sync_sem);
    // Assert that every threads' stack pointer is known
    for_each_thread(th) {
        if (th->control_stack_pointer != 0 ||
            read_TLS(FREE_INTERRUPT_CONTEXT_INDEX, th)) {
        } else {
            lose("Thread %p context is unknown", th);
        }
        restore_tlab(&th->cons_tlab, &th->spill_cons_tlab_end);
        restore_tlab(&th->sys_cons_tlab, &th->spill_sys_cons_tlab_end);
        restore_tlab(&th->mixed_tlab, &th->spill_mixed_tlab_end);
        restore_tlab(&th->sys_mixed_tlab, &th->spill_sys_mixed_tlab_end);
    }
    n_known_threads = 0;
    memset(known_threads, 0, sizeof known_threads);
}

void gc_start_the_world() {
    struct thread* th;
    for_each_thread(th) {
        th->control_stack_pointer = 0;
        __sync_fetch_and_and(&th->interrupt_reason, ~SUSPEND);
        //th->stepping.suspend = 0;
    }
    world_is_stopped = 0;
    futex_wake(&world_is_stopped, INT_MAX);
    if (debug) fprintf(stderr, "GC: world restarted\n");
}

void show_yp_kind_hit() {
  fprintf(stderr, "YP hit: %d %d %d %d\n", yp_kind_hit[0], yp_kind_hit[1], yp_kind_hit[2], yp_kind_hit[3]);
}

/*
Finalizer thread should wake on any of the following events:
- finalizer item queued
- interrupt-thread action
- other deferred async signal
- post-gc hooks
The first 3 are working, the last is not. I need a way to tell
it to run the hooks */

extern int finalizer_thread_runflag;
void finalizer_thread_wait () {
    /* pthread_cond_wait can never return EINTR which is what made
     * SB-THREAD:INTERRUPT-THREAD unreliable on the finalizer.
     * Direct use of a futex gives nicer behavior */
    int c = finalizer_thread_runflag;
    if (c) futex_wait(&finalizer_thread_runflag, c, -1, 0);

}
void finalizer_thread_wake () {
    futex_wake(&finalizer_thread_runflag, 1);
}
void finalizer_thread_stop () {
    finalizer_thread_runflag = 0;
    futex_wake(&finalizer_thread_runflag, 1);
}

extern void yieldpoint_patch_asm_routines(int);
void* collector_main(__attribute__((unused)) void* dummy)
{
#ifdef LISP_FEATURE_LINUX
    pthread_setname_np(pthread_self(), "collectorMain");
#endif
    pthread_mutex_lock(&gc_condition_lock);
    for (;;) {
        pthread_cond_wait(&gc_controller_cond, &gc_condition_lock);
        int reason = gc_notification_reason;
        if (reason & GC_CONTROLLER_QUIT) break;
        if (!(gc_notification_reason & INITIATE_GC)) continue; // sleep some more
        if (debug) fprintf(stderr, "GC main wants to run\n");
        /* One or more threads that we need to try to suspend might each be
         * trying to acquire the lock to signal a GC request, so never hold
         * the condition lock while trying to initiat a collection */
        pthread_mutex_unlock(&gc_condition_lock);
        /* No thread can die nor be created; and no thread can inhibit GC. */
        pthread_rwlock_wrlock(&all_threads_lock);
        // Patching is needed in order for threads to reach a yieldpoint quicker,
        // since asm routines have NOPs where yieldpoints should be.
        //yieldpoint_patch_asm_routines(1);
        gc_stop_the_world();
        int generation = gc_notification_reason & 7;
        extern void collect_garbage(generation_index_t);
        //show_yp_kind_hit();
        collect_garbage(generation);
        gc_notification_reason = 0;
        ++gc_cycle_count;
        // TODO: enqueue a finalizer thread action to run post-GC hooks
        gc_start_the_world();
        pthread_rwlock_unlock(&all_threads_lock);
        finalizer_thread_wake();
        pthread_mutex_lock(&gc_condition_lock);
        // Manually requested GC should wait on this condition
        pthread_cond_broadcast(&gc_completion_cond);
        extern void empty_thread_recyclebin();
        empty_thread_recyclebin();
        // can be un-patched at any time
        //yieldpoint_patch_asm_routines(0);
    }
    pthread_mutex_unlock(&gc_condition_lock);
    return 0;
}

#if TELNET_SERVER
#include <sys/socket.h>
#include <netinet/in.h>
int listener;
void* debugging_service_main(__attribute__((unused)) void* dummy)
{
#if 0
  {sigset_t mask;
    pthread_sigmask(SIG_SETMASK, 0, &mask);
     char buf[100]; sigset_tostring(&mask, buf, sizeof buf);
      printf("service thread has mask %s\n", buf);
      sigset_tostring(&deferrable_sigset, buf, sizeof buf);
      printf("deferrable is %s\n", buf);
      sigset_tostring(&blockable_sigset, buf, sizeof buf);
      printf("blockable  is %s\n", buf);}
#endif
    struct sockaddr_in sin;
    memset(&sin, 0, sizeof sin);
    sin.sin_addr.s_addr = INADDR_ANY;
    sin.sin_port = 0;
    listener = socket(AF_INET, SOCK_STREAM, 0);
    if (bind(listener, &sin, sizeof sin)) {perror("bind");exit(1);}
    if (listen(listener, 5)) {perror("listen");exit(1);}
    socklen_t addrlen = sizeof sin;
    if (getsockname(listener, &sin, &addrlen)) {perror("getsockname");exit(1);}
    char buf[64];
    int n = snprintf(buf, sizeof buf, "Pid %d listening on port %d\n", getpid(), ntohs(sin.sin_port));
    write(2, buf, n);
    int peer;
    FILE* stream = 0;
    while (1) {
      peer = accept(listener, &sin, &addrlen);
      if (peer < 0) lose("wtf?");
      stream = fdopen(peer, "w");
      setlinebuf(stream);
      fprintf(stream, "nthreads=%d waiting_on=%d\n", n_known_threads, get_n_currently_waiting_on());
      fprintf(stream, " pthread vm_thread tid State Lip/C spinlock_wait\n");
      {int i;for (i=0;i<n_known_threads;++i) {
          struct thread* th  = known_threads[i].th;
          if (!th) {
            fprintf(stream, "%d: bad\n", i);
          } else {
            int sw = th->state_word.state;
            lispobj mutex = read_TLS(CURRENT_MUTEX, th);
            fprintf(stream, "%d: %lx %p %lx %s %s %d ",
                    i, th->os_thread, th, th->os_kernel_tid,
                    (sw==STATE_DEAD?"DEAD":sw==STATE_RUNNING?"RUN ":sw==STATE_STOPPED?"STOP":"????"),
                    known_threads[i].in_foreign_code?"C   ":"Lisp",
                    known_threads[i].gc_waiting_on_spinlock);
            if (mutex != 0) {
              struct lispmutex* m = (void*)native_pointer(mutex);
              lispobj name = m->name;
              fprintf(stderr, "mutex=%lx (state=%lx owner=%lx name=%s)",
                      mutex, m->uw_state, m->_owner,
                      (other_pointer_p(name) && simple_base_string_p(name)) ? (char*)(name+1) : "?");
            }
            putc('\n', stderr);
          }
        }
      }
      while (1) {
        char line[80];
        write(peer, "> ", 2);
        int n = read(peer, line, sizeof line);
        if (n<=0){ fclose(stream); break; }
        printf("got n=%d command [%s]\n", n, line);
        char cmd = line[0];
        if (cmd == 'q') break;
        write(peer, "m'kay\n",6);
      }
      printf("remote debug service connected\n");
      close(peer);
    }
}
#endif

void safepoint_init() { // backward-compatible function name
    os_sem_init(&gc_sync_sem, 0);
    sigset_t oldmask;
    // Don't want SIGCHLD, SIGARLM, etc
    pthread_sigmask(SIG_BLOCK, &blockable_sigset, &oldmask);
    pthread_create(&gc_controller, NULL, collector_main, 0);
    if (debug) fprintf(stderr, "GC controller thread is %p\n", (void*)gc_controller);
#ifdef TELNET_SERVER
    pthread_create(&rdb_thread, NULL, debugging_service_main, 0);
    pthread_setname_np(rdb_thread, "debug");
#endif
    pthread_sigmask(SIG_SETMASK, &oldmask, 0);
}

void request_garbage_collection(int generation)
{
    if (generation == -1) { // auto-triggered request
        // a read barrier is not important here
        if (gc_notification_reason) return; // request already pending
        pthread_mutex_lock(&gc_condition_lock);
        gc_notification_reason |= INITIATE_GC;
        pthread_cond_broadcast(&gc_controller_cond);
        pthread_mutex_unlock(&gc_condition_lock);
    } else {
        pthread_mutex_lock(&gc_condition_lock);
        /* Take the higher of competing args to SB-EXT:GC if two user threads
         * nearly simultaneously invoke it */
        int pending = gc_notification_reason & 7;
        if (pending > generation) generation = pending;
        gc_notification_reason = INITIATE_GC | generation;
        long starting_count = gc_cycle_count;
        /* The Lisp side will deal with the case of explicitly requesting a collection
         * while inside without-gcing. That's kinda stupid and it's unclear why we would
         * want to support that, other than regression tests assert that it works */
        pthread_cond_broadcast(&gc_controller_cond);
        while (gc_cycle_count == starting_count)
            pthread_cond_wait(&gc_completion_cond, &gc_condition_lock);
        pthread_mutex_unlock(&gc_condition_lock);
    }
}

extern void
store_signal_data_for_later (struct interrupt_data *data, void *handler,
                             int signal,
                             siginfo_t *info, os_context_t *context);

/* Unlike maybe_now_maybe_later, this handler never handles a signal right away
 * but instead always defers it to a yieldpoint.
 * It would be neat if we could figure out which Lisp thread is in the foreground
 * so SIGINT could be directed correctly the first time instead of having to
 * potentially re-signal it in Lisp */
void defer_to_yieldpoint(int sig, siginfo_t* info, void* context)
{
    int _saved_errno = errno;
    struct thread* th = get_sb_vm_thread();
    store_signal_data_for_later(&thread_interrupt_data(th), (void*)1,
                                sig, info, context);
    lispobj enabled = read_TLS(INTERRUPTS_ENABLED, th);
#if 1
    {
    char b[100];
    int n = snprintf(b, sizeof b, "%s: Deferring: %s (pc=%lx) %s\n",
                     pthreadname(th), strsignal(sig),
                     os_context_pc(context),
                     enabled==NIL?"DISABLED":enabled==LISP_T?"ENABLED":"?");

      write(2,b,n);
      //libunwind_backtrace(th, context);
    }
#endif
    if (enabled == NIL) {
        write_TLS(INTERRUPTS_ENABLED, 0, th); // 0 = "signal deferred"
        /* WITHOUT-INTERRUPTS will check whether a signal got deferred,
         * and will explicitly handle it without a yieldpoint trap */
    } else if (enabled == LISP_T) {
        /* If the lock is already taken, then either the GC controller is frobbing it,
         * or this thread itself is in alloc_cooperate. In either case we don't need
         * to arm the trap. The worst that happens is a deferred interrupt
         * gets handled a little later */
        __sync_fetch_and_or(&th->interrupt_reason, ASYNC_SIGNAL);
        if (try_acquire_lock(&th->yieldpoint_spinlock)) {
            yieldpoint_trap_toggle(th, NO_ACCESS);
            release_lock(&th->yieldpoint_spinlock);
        }
    } else {
        lose("Received async interrupt with enable neither T nor NIL");
    }
    errno = _saved_errno;
}

static void publish_context(struct thread* thread, os_context_t* context)
{
    /* Do dynamic binding of the active interrupt context index
     * and save the context in the context array. */
    int context_index =
        fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX, thread));
    if (context_index >= MAX_INTERRUPTS)
        lose("maximum interrupt nesting depth (%d) exceeded", MAX_INTERRUPTS);

    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX, make_fixnum(context_index + 1), thread);
    nth_interrupt_context(context_index, thread) = context;
}

static void unpublish_context(struct thread* thread)
{
    // Never leave stale pointers in the signal context array
    int ici = fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,thread));
    nth_interrupt_context(ici - 1, thread) = NULL;
    /* Undo dynamic binding of FREE_INTERRUPT_CONTEXT_INDEX */
    unbind(thread);
}

#if 0
void sigtrap_handler(int __attribute__((unused)) signal,
                     siginfo_t __attribute__((unused)) *info,
                     os_context_t *context)
{
    struct thread*th = get_sb_vm_thread();
    if (!th->stepping.suspend) lose("Why are you closed?");
    publish_context(th, context);
    sem_post(&gc_sync_sem);
    if (debug) {
      char buf[100];
      int n = snprintf(buf, sizeof buf, "%s: posted\n", pthreadname(th));
      write(2, buf, n);
    }
    do { futex_wait(&world_is_stopped, 1, -1, 0); } while (world_is_stopped);
    if (debug) fprintf(stderr, "%s: back from futex_wait\n", pthreadname(th));
    unpublish_context(th);
}
#endif

static void suspend_for_gc(struct thread* th)
{
    if (debug)fprintf(stderr, "%s: need to suspend in allocator\n", pthreadname(th));
    __sync_fetch_and_add(&yp_kind_hit[0], 1);
    ucontext_t context; // don't really need this whole context, but that's fine
    getcontext(&context);
    publish_context(th, &context);
    th->state_word.state = STATE_STOPPED;
    sem_post(&gc_sync_sem);
    do { futex_wait(&world_is_stopped, 1, -1, 0); } while (world_is_stopped);
    th->state_word.state = STATE_RUNNING;
    if (debug) fprintf(stderr, "%s: back from futex_wait in allocator\n", pthreadname(th));
    unpublish_context(th);
}

void maybe_suspend_for_gc(struct thread* th)
{
    if (th->interrupt_reason & SUSPEND) suspend_for_gc(th);
}

/* Return a copy of the thread's region. We operate on the copy so that it is not
 * succeptible to having the 'end' slot clobbered while in the call sequence of lisp_alloc. */
struct alloc_region alloc_cooperate_before(struct alloc_region* r, struct thread* th)
{
    struct alloc_region result;
    while (1) {
        // acquire_lock(&th->yieldpoint_spinlock);
        result = *r;
        // release_lock(&th->yieldpoint_spinlock);
        if (result.end_addr) return result;
        gc_assert(th->interrupt_reason & SUSPEND);
        suspend_for_gc(th);
    }
}

static lispobj* region_end_spill_slot(struct thread* th, struct alloc_region* r)
{
    if (r == &th->cons_tlab) return &th->spill_cons_tlab_end;
    if (r == &th->sys_cons_tlab) return &th->spill_sys_cons_tlab_end;
    if (r == &th->mixed_tlab) return &th->spill_mixed_tlab_end;
    if (r == &th->sys_mixed_tlab) return &th->spill_sys_mixed_tlab_end;
    lose("Unknown TLAB %p for thread %p", r, th);
}

/* Restore the thread's TLAB from the shadow copy. If GC already forced the region
 * into a slow path fallback, we have replicate that effect. It won't work to yield
 * now because we'll lose the object whose space was just allocated but whose bits
 * were not written */
void alloc_cooperate_after(struct alloc_region* dst, struct alloc_region* src,
                           struct thread* th)
{
    acquire_lock(&th->yieldpoint_spinlock);
    bool yield = !dst->end_addr;
    *dst = *src;
    lispobj* spill_slot = region_end_spill_slot(th, dst);
    if (yield)
       force_tlab_slowpath(dst, spill_slot);
    else
       gc_assert(*spill_slot == 0);
    release_lock(&th->yieldpoint_spinlock);
#if 0
    if (yield) //start_addr != src->start_addr || dst->end_addr != src->end_addr)
      fprintf(stderr, "cooperate_after detected detected change of region: TLAB=%p:%p (free=%p) shadow=%p:%p (free=%p) yield=%d\n",
              dst->start_addr, dst->end_addr, dst->free_pointer,
              src->start_addr, src->end_addr, src->free_pointer, yield);
#endif
}

#ifdef LISP_FEATURE_X86_64
static yieldpoint_kind infer_kind(unsigned char* pc) {
    if (pc[0] == 0x45 && pc[1] == 0x84) return IN_LISP;   // TEST BYTE
    if (pc[0] == 0x45 && pc[1] == 0x85) return IN_LISP;   // TEST DWORD
    if (pc[0] == 0x66 && pc[1] == 0x45) return IN_LISP;   // MOVNTDQA
    if (pc[0] == 0xC4 && pc[1] == 0x42) return IN_LISP;   // VMOVNTDQA
    if (pc[0] == 0x49 && pc[1] == 0x89) return LISP_TO_C; // MOV mem, RSP
    if (pc[0] == 0x49 && pc[1] == 0x31) return C_TO_LISP; // XOR mem, RSP
    return UNKNOWN;
}
#endif

static void yieldpoint_trap(struct thread* th, os_context_t *context)
{
    unsigned char* pc = (void*)os_context_pc(context);
    yieldpoint_kind kind = infer_kind(pc);
    if (debug) {
        char buf[100];
        int n = snprintf(buf, sizeof buf, "%s: YP trap %d %d pc=%p\n",
                         pthreadname(th), (int)th->interrupt_reason, kind, pc);
        write(2, buf, n);
        sb_dump_mcontext("foo", context);
    }
    event3("YP trap %x %d %p", th->interrupt_reason, kind, pc);
    if (kind == UNKNOWN) lose("unrecognized yieldpoint @ %p", pc);
    __sync_fetch_and_add(&yp_kind_hit[kind], 1);
    while (1) {
        int reason = th->interrupt_reason;
        if (!reason) {
            /* Disarm the trap but only if a double-check of the trap reason
             * confirms that it need not be armed. A yieldpoint trap can't happen
             * in alloc_cooperate, so this can't deadlock with itself */
            acquire_lock(&th->yieldpoint_spinlock);
            // FIXME: atomic load probably needed here? especially for non-x86
            reason = th->interrupt_reason;
            if (!reason) yieldpoint_trap_toggle(th, READ_WRITE);
            release_lock(&th->yieldpoint_spinlock);
            if (!reason) return;
        }
        /* Always cooperate with GC before doing anything else. */
        if (reason & SUSPEND) {
            int context_published = 0;
            th->state_word.state = STATE_STOPPED;
            if (kind == C_TO_LISP) {
                /* Trapped trying to get back to Lisp from a C call. Do not sem_post,
                 * just wait until world isn't stopped. */
            } else {
                publish_context(th, context);
                context_published = 1;
                sem_post(&gc_sync_sem);
                if (debug) {
                  char buf[100];
                  int n = snprintf(buf, sizeof buf, "%s: posted\n", pthreadname(th));
                  write(2, buf, n);
                }
            }
            do { futex_wait(&world_is_stopped, 1, -1, 0); } while (world_is_stopped);
            th->state_word.state = STATE_RUNNING;
            if (debug) fprintf(stderr, "%s: back from futex_wait\n", pthreadname(th));
            if (context_published) unpublish_context(th);
            reason &= ~SUSPEND;
        }
        if (!reason) continue; // try to return now
        gc_assert(reason & ASYNC_SIGNAL);
        // Make the yieldpoint instruction act like it was a CALL to a Lisp asm routine
        static char* asm_trampoline;
        if (!asm_trampoline) { // Lookup and/or memoize the routine to call
            asm_trampoline = get_asm_routine_by_name("HANDLE-DEFERRED-SIGNAL", 0);
            gc_assert(asm_trampoline);
            if (debug) fprintf(stderr, "YP asm tramp is %p\n", asm_trampoline);
        }
        /* Disarm the trap, ensuring that we're "in Lisp" regardless of the kind of yieldpoint,
         * because we don't want to handle async signals from C. (The Lisp signal handler
         * is ordinary Lisp code that can cons, triggering a garbage collection)
         * But also consider the following theoretically possible sequence:
         * GC is initiated, arms this thread's trap, completes, is immediately re-initiated
         * and arms this thread's trap. If we unilaterally disarm it, then we will miss the
         * second stop request that arrived in rapid-fire succession */
        acquire_lock(&th->yieldpoint_spinlock);
        if (th->interrupt_reason & SUSPEND) { // dang, not again?
            release_lock(&th->yieldpoint_spinlock);
            continue;
        }
        yieldpoint_trap_toggle(th, READ_WRITE);
        clear_stackptr_at_foreign_call(th);
        release_lock(&th->yieldpoint_spinlock);
        os_context_register_t return_to = (os_context_register_t)pc;
        /* If returning from foreign code (at a C-to-Lisp yieldpoint), step beyond the
         * yieldpoint. Having cleared the stack-pointer slot in the thread structure,
         * the collector understands that this thread is effectively in Lisp
         * regardless of the kind of yieldpoint trap */
        if (kind == C_TO_LISP) return_to += 4; // size of the XOR instruction
        /* Simulate a CALL that returns to the yieldpoint (or one instruction after).
         * God forbid this store hits the stack guard page.
         * Maybe just don't handle the async signal in that case? */
        lispobj* sp = (lispobj*)*os_context_sp_addr(context);
        --sp;
        fprintf(stderr, "YP trap: delivering signal to thread %p, pushing old PC %lx\n", th, return_to);
        *sp = return_to;
        *os_context_sp_addr(context) = (os_context_register_t)sp;
        /* If yet another GC stop request arrives, the next yieldpoint will be in the
         * prologue of the Lisp function that handles the deferred signal */
        OS_CONTEXT_PC(context) = (os_context_register_t)asm_trampoline;
        return;
    }
}

int handle_safepoint_violation(os_context_t *context, os_vm_address_t fault_address) {
    struct thread*th = get_sb_vm_thread();
    if (th && yieldpoint_trap_addr(th) == (lispobj*)fault_address) {
        yieldpoint_trap(th, context);
        return 1;
    }
    return 0;
}
int show_interrupt_data() {
    struct thread*th = get_sb_vm_thread();
    struct interrupt_data *data = &thread_interrupt_data(th);
    siginfo_t *inf = &data->pending_info;
    char string1[100], string2[100];
    sigset_t oldmask;
    int result = data->pending_signal;
    pthread_sigmask(SIG_SETMASK, 0, &oldmask);
    sigset_tostring(&oldmask, string1, 100);
    sigset_tostring(&data->pending_mask, string2, 100);
    printf("sig=%d no=%d errno=%d code=%d pid=%d uid=%d st=%d cur_mask=%s pend_mask=%s\n",
           data->pending_signal, inf->si_signo, inf->si_errno, inf->si_code, inf->si_pid, inf->si_uid,
           inf->si_status, string1, string2);
    return result;
}
int get_pending_signal_number() {
    struct thread*th = get_sb_vm_thread();
    return thread_interrupt_data(th).pending_signal;
}
void flush_pending_signal_and_restore() {
    struct thread*th = get_sb_vm_thread();
    struct interrupt_data *data = &thread_interrupt_data(th);
    //siginfo_t *inf = &data->pending_info;
    data->pending_handler = 0;
    data->pending_signal = 0;
    __sync_fetch_and_and(&th->interrupt_reason, ~ASYNC_SIGNAL);
    pthread_sigmask(SIG_SETMASK, &data->pending_mask, 0);
}

extern void interrupt_handle_now_handler(int, siginfo_t*, void*);
extern void sigprof_handler(int, siginfo_t*, void*);
extern void (*interrupt_low_level_handlers[NSIG]) (int, siginfo_t*, os_context_t*);

 /* This is called from Lisp. */
void install_handler(int signal, lispobj handler)
{
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);

    printf("HL handler %d (%s)\n", signal, strsignal(signal));
    if (interrupt_low_level_handlers[signal]) {
        // When there's a low-level handler, we must leave it alone.
        // Give it the lisp function to call if it decides to forward a signal.
        // SIG_IGN and SIG_DFL don't always do what you think in such case.
        lisp_sig_handlers[signal] = functionp(handler) ? handler : 0;
    } else if (signal == SIGPROF) {
        if (handler) sa.sa_sigaction = sigprof_handler;
        else         sa.sa_handler   = SIG_DFL;
        // The handler is signal-safe, but because it uses component_ptr_from_pc(),
        // it must block GC, lest crashes occur from dereferencing wild pointers.
        // FIXME: skip collect a profiling sample if GC is in progress which can happen
        // if we transited through a lisp-to-c yieldpoint
        sa.sa_mask = blockable_sigset;
        sa.sa_flags = SA_SIGINFO | SA_RESTART;
        sigaction(signal, &sa, NULL);
        return;
    } else {
        // Our "abstract" values for SIG_DFL and SIG_IGN are 0 and 1
        // respectively which are probably the real values from signal.h
        // but this way way don't need to put them in grovel-headers.c
        if (handler==0 || handler==1) {
            sa.sa_handler = handler ? SIG_IGN : SIG_DFL;
            // assign the OS level action before clearing the lisp function.
            // (If a signal were to be delivered to the C trampoline when the lisp
            // function is NIL, we'd get the effect of :IGNORE regardless
            // of what the default action should be)
            sigaction(signal, &sa, NULL);
            lisp_sig_handlers[signal] = 0;
            return;
        }
        sa.sa_flags = SA_SIGINFO;
        if (signal != SIGURG) sa.sa_flags |= SA_RESTART;
        if (sigismember(&deferrable_sigset, signal)) {
            sa.sa_sigaction = defer_to_yieldpoint;
        } else {
            sa.sa_sigaction = interrupt_handle_now_handler;
            sa.sa_flags |= SA_NODEFER;
        }
        sa.sa_mask = blockable_sigset;
        // ensure the C handler sees a lisp function before doing sigaction()
        lisp_sig_handlers[signal] = handler;
        sigaction(signal, &sa, NULL);
    }
}
