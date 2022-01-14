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
#include "sbcl.h"

#ifdef LISP_FEATURE_SB_SAFEPOINT /* entire file */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifndef LISP_FEATURE_WIN32
#include <sched.h>
#endif
#include <signal.h>
#include <stddef.h>
#include <errno.h>
#include <sys/types.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/wait.h>
#endif
#include "runtime.h"
#include "validate.h"
#include "thread.h"
#include "arch.h"
#include "target-arch-os.h"
#include "os.h"
#include "globals.h"
#include "dynbind.h"
#include "genesis/cons.h"
#include "genesis/fdefn.h"
#include "interr.h"
#include "alloc.h"
#include "gc-internal.h"
#include "getallocptr.h"
#include "interrupt.h"
#include "lispregs.h"

const char* gc_phase_names[GC_NPHASES] = {
    "GC_NONE",
    "GC_FLIGHT",
    "GC_MESSAGE",
    "GC_INVOKED",
    "GC_QUIET",
    "GC_SETTLED",
    "GC_COLLECT"
};

/* States and transitions:
 *
 * GC_NONE: Free running code.
 *
 * GC_NONE -> GC_FLIGHT: unmap_gc_page(), arming the GSP trap.
 *
 * GC_FLIGHT: GC triggered normally, waiting for post-allocation
 * safepoint trap.
 *
 * GC_FLIGHT -> GC_MESSAGE: gc_notify_early(), arming the per-thread
 * CSP traps.
 *
 * GC_MESSAGE: Waiting for lisp threads to stop (WITHOUT-GCING threads
 * will resume at GC_INVOKED).
 *
 * GC_MESSAGE -> GC_INVOKED: map_gc_page(), disarming the GSP trap.
 *
 * GC_INVOKED: Waiting for WITHOUT-GCING threads to leave
 * WITHOUT-GCING.
 *
 * GC_INVOKED -> GC_QUIET: nothing changes.
 *
 * GC_QUIET: GCing threads race to stop the world (and melt with you).
 *
 * GC_QUIET -> GC_SETTLED: unmap_gc_page(), gc_notify_final(), arming
 * GSP and CSP traps again.
 *
 * GC_SETTLED: Waiting for remaining lisp threads to stop.
 *
 * GC_SETTLED -> GC_COLLECT: map_gc_page(), disarming the GSP trap.
 *
 * GC_COLLECT: World is stopped, save for one thread in SUB-GC / FLET
 * PERFORM-GC, running the garbage collector.
 *
 * GC_COLLECT -> GC_NONE: gc_none(), clearing CSP traps and possibly
 * GC_PENDING.
 *
 * GC_NONE: Free running code.
 *
 * Note that the system may not actually stop in every state for a GC.
 * For example, a system with only one thread directly invoking
 * SB-EXT:GC will advance quickly from GC_NONE to GC_COLLECT, simply
 * because no other threads exist to prevent it.  That same scenario
 * with a thread inside WITHOUT-GCING sitting in alien code at the
 * time will move to GC_INVOKED and then wait for the WITHOUT-GCING
 * thread to finish up, then proceed to GC_COLLECT. */

#ifdef LISP_FEATURE_SB_THREAD
#define CURRENT_THREAD_VAR(name) \
    struct thread *name = get_sb_vm_thread()
#define THREAD_STOP_PENDING(th) \
    read_TLS(STOP_FOR_GC_PENDING, th)
#define SET_THREAD_STOP_PENDING(th,state) \
    write_TLS(STOP_FOR_GC_PENDING,state,th)
#define WITH_ALL_THREADS_LOCK \
    mutex_acquire(&all_threads_lock); \
    RUN_BODY_ONCE(all_threads_lock, mutex_release(&all_threads_lock))
#else
#define CURRENT_THREAD_VAR(name)
#define THREAD_STOP_PENDING(th) NIL
#define SET_THREAD_STOP_PENDING(th,state)
#define WITH_ALL_THREADS_LOCK
#endif

#if !defined(LISP_FEATURE_WIN32)
/* win32-os.c covers these, but there is no unixlike-os.c, so the normal
 * definition goes here.  Fixme: (Why) don't these work for Windows?
 */
void
map_gc_page()
{
    odxprint(misc, "map_gc_page");
    os_protect((void *) GC_SAFEPOINT_PAGE_ADDR,
               BACKEND_PAGE_BYTES,
               OS_VM_PROT_READ);
}

void
unmap_gc_page()
{
    odxprint(misc, "unmap_gc_page");
    os_protect((void *) GC_SAFEPOINT_PAGE_ADDR, BACKEND_PAGE_BYTES, OS_VM_PROT_NONE);
}
#endif /* !LISP_FEATURE_WIN32 */

static struct gc_state {
#ifdef LISP_FEATURE_WIN32
    /* Per-process lock for gc_state */
    CRITICAL_SECTION lock;;
    /* Conditions: one per phase */
    CONDITION_VARIABLE phase_cond[GC_NPHASES];
#else
    pthread_mutex_t lock;
    pthread_cond_t phase_cond[GC_NPHASES];
#endif

    /* For each [current or future] phase, a number of threads not yet ready to
     * leave it */
    int phase_wait[GC_NPHASES];

    /* Master thread controlling the topmost stop/gc/start sequence */
    struct thread* master;
    struct thread* collector;

    /* Current GC phase */
    gc_phase_t phase;
} gc_state
#ifdef LISP_FEATURE_UNIX
           = { PTHREAD_MUTEX_INITIALIZER,
               { PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER,
                 PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER, PTHREAD_COND_INITIALIZER,
                 PTHREAD_COND_INITIALIZER },
               { 0, 0, 0, 0, 0, 0, 0 }, NULL, NULL, GC_NONE }
#endif
    ;

void safepoint_init()
{
# ifdef LISP_FEATURE_WIN32
    int i;
    extern void alloc_gc_page(void);
    alloc_gc_page();
    for (i=GC_NONE; i<GC_NPHASES; ++i)
        InitializeConditionVariable(&gc_state.phase_cond[i]);
    InitializeCriticalSection(&gc_state.lock);
#else
    os_validate(NOT_MOVABLE, GC_SAFEPOINT_PAGE_ADDR, BACKEND_PAGE_BYTES, 0, 0);
#endif
    gc_state.phase = GC_NONE;
}

void
gc_state_lock()
{
    odxprint(safepoints,"GC state to be locked");
#ifdef LISP_FEATURE_SB_THREAD
    int result = mutex_acquire(&gc_state.lock);
    gc_assert(result);
#endif
    if (gc_state.master) {
        fprintf(stderr,"GC state lock glitch [%p] in thread %p phase %d (%s)\n",
                gc_state.master,get_sb_vm_thread(),gc_state.phase,
                gc_phase_names[gc_state.phase]);
        odxprint(safepoints,"GC state lock glitch [%p]",gc_state.master);
    }
    gc_assert(!gc_state.master);
    gc_state.master = get_sb_vm_thread();
    odxprint(safepoints,"GC state locked in phase %d (%s)",
             gc_state.phase, gc_phase_names[gc_state.phase]);
}

void
gc_state_unlock()
{
    odxprint(safepoints,"GC state to be unlocked in phase %d (%s)",
             gc_state.phase, gc_phase_names[gc_state.phase]);
    gc_assert(get_sb_vm_thread()==gc_state.master);
    gc_state.master = NULL;
#ifdef LISP_FEATURE_SB_THREAD
    int result = mutex_release(&gc_state.lock);
    gc_assert(result);
#endif
    odxprint(safepoints,"%s","GC state unlocked");
}

void
gc_state_wait(gc_phase_t phase)
{
    struct thread* self = get_sb_vm_thread();
    odxprint(safepoints,"Waiting for %d (%s) -> %d (%s) [%d holders]",
             gc_state.phase, gc_phase_names[gc_state.phase],
             phase, gc_phase_names[phase],
             gc_state.phase_wait[gc_state.phase]);
    gc_assert(gc_state.master == self);
    gc_state.master = NULL;
    while(gc_state.phase != phase && !(phase == GC_QUIET && (gc_state.phase > GC_QUIET))) {
#ifdef LISP_FEATURE_WIN32
        SleepConditionVariableCS(&gc_state.phase_cond[phase], &gc_state.lock, INFINITE);
#elif defined LISP_FEATURE_SB_THREAD
        pthread_cond_wait(&gc_state.phase_cond[phase],&gc_state.lock);
#else
        lose("gc_state_wait() blocks, but we're #-SB-THREAD");
#endif
    }
    gc_assert(gc_state.master == NULL);
    gc_state.master = self;
}

int
gc_cycle_active(void)
{
    return gc_state.phase != GC_NONE;
}

static void
set_csp_from_context(struct thread *self, os_context_t *ctx)
{
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    void **sp = (void **) *os_context_register_addr(ctx, reg_SP);
    /* On POSIX platforms, it is sufficient to investigate only the part
     * of the stack that was live before the interrupt, because in
     * addition, we consider interrupt contexts explicitly.  On Windows,
     * however, we do not keep an explicit stack of exception contexts,
     * and instead arrange for the conservative stack scan to also cover
     * the context implicitly.  The obvious way to do that is to start
     * at the context itself: */
#ifdef LISP_FEATURE_WIN32
    gc_assert((void **) ctx < sp);
    sp = (void**) ctx;
#endif
    gc_assert((void **)self->control_stack_start
              <= sp && sp
              < (void **)self->control_stack_end);
#else
    /* Note that the exact value doesn't matter much here, since
     * platforms with precise GC use get_csp() only as a boolean -- the
     * precise GC already keeps track of the stack pointer itself.
     * That said, we're either in a foreign function call or have
     * called fake_foreign_function_call(), and having accurate values
     * here makes the debugging experience easier and less
     * disconcerting. */
    void **sp = (void **) access_control_stack_pointer(self);
#endif
    csp_around_foreign_call(self) = (lispobj) sp;
}


static inline gc_phase_t gc_phase_next(gc_phase_t old) {
    return (old+1) % GC_NPHASES;
}

static inline boolean
thread_blocks_gc(struct thread *thread)
{
    return read_TLS(GC_INHIBIT,thread)==T;
}
/* set_thread_csp_access -- alter page permissions for not-in-Lisp
   flag (Lisp Stack Top) of the thread `th'. The flag may be modified
   if `writable' is true.

   Return true if there is a non-null value in the flag.

   When a thread enters C code or leaves it, a per-thread location is
   modified. That machine word serves as a not-in-Lisp flag; for
   convenience, when in C, it's filled with a topmost stack location
   that may contain Lisp data. When thread is in Lisp, the word
   contains NULL.

   GENCGC uses each thread's flag value for conservative garbage collection.

   There is a full VM page reserved for this word; page permissions
   are switched to read-only for race-free examine + wait + use
   scenarios. */
static inline boolean
set_thread_csp_access(struct thread* th, boolean writable)
{
    os_protect((char*)th - (THREAD_HEADER_SLOTS*N_WORD_BYTES) - THREAD_CSP_PAGE_SIZE,
               THREAD_CSP_PAGE_SIZE,
               writable? (OS_VM_PROT_READ|OS_VM_PROT_WRITE)
               : (OS_VM_PROT_READ));
    return csp_around_foreign_call(th) != 0;
}

static inline void gc_notify_early()
{
    struct thread *self = get_sb_vm_thread(), *p;
    odxprint(safepoints,"%s","global notification");
    gc_assert(gc_state.phase == GC_MESSAGE);
    /* We're setting up the per-thread traps to make sure that all
     * lisp-side threads get stopped (if they are WITHOUT-GCING then
     * they can resume once the GSP trap is disarmed), and all
     * alien-side threads that are inside WITHOUT-GCING get their
     * chance to run until they exit WITHOUT-GCING. */
    WITH_ALL_THREADS_LOCK {
        for_each_thread(p) {
            /* This thread is already on a waitcount somewhere. */
            if (p==self)
                continue;
            /* If there's a collector thread then it is already on a
             * waitcount somewhere.  And it may-or-may-not be this
             * thread. */
            if (p==gc_state.collector)
                continue;
            odxprint(safepoints,"notifying thread %p csp %p",p,csp_around_foreign_call(p));
            boolean was_in_lisp = !set_thread_csp_access(p,0);
            if (was_in_lisp) {
                /* Threads "in-lisp" block leaving GC_MESSAGE, as we
                 * need them to hit their CSP or the GSP, and we unmap
                 * the GSP when transitioning to GC_INVOKED. */
                gc_state.phase_wait[GC_MESSAGE]++;
                SET_THREAD_STOP_PENDING(p,T);
            } else if (thread_blocks_gc(p)) {
                /* Threads "in-alien" don't block leaving GC_MESSAGE,
                 * as the CSP trap is sufficient to catch them, but
                 * any thread that is WITHOUT-GCING prevents exit from
                 * GC_INVOKED. */
                gc_state.phase_wait[GC_INVOKED]++;
                SET_THREAD_STOP_PENDING(p,T);
            }
        }
    }
}

static inline void gc_notify_final()
{
    struct thread *p;
    odxprint(safepoints,"%s","global notification");
    gc_assert(gc_state.phase == GC_SETTLED);
    gc_state.phase_wait[GC_SETTLED]=0;
    /* All remaining lisp threads, except for the collector, now need
     * to be stopped, so that the collector can run the GC.  Any
     * thread already stopped shows up as being "in-alien", so we
     * don't bother with them here. */
    WITH_ALL_THREADS_LOCK {
        for_each_thread(p) {
            if (p == gc_state.collector)
                continue;
            odxprint(safepoints,"notifying thread %p csp %p",p,csp_around_foreign_call(p));
            boolean was_in_lisp = !set_thread_csp_access(p,0);
            if (was_in_lisp) {
                gc_state.phase_wait[GC_SETTLED]++;
                SET_THREAD_STOP_PENDING(p,T);
            }
        }
    }
}

static inline void gc_done()
{
    CURRENT_THREAD_VAR(self);
    struct thread *p;
    boolean inhibit = (read_TLS(GC_INHIBIT,self)==T);

    odxprint(safepoints,"%s","global denotification");
    WITH_ALL_THREADS_LOCK {
        for_each_thread(p) {
            if (inhibit && (read_TLS(GC_PENDING,p)==T))
                write_TLS(GC_PENDING,NIL,p);
            set_thread_csp_access(p,1);
        }
    }
}

static inline void gc_handle_phase()
{
    odxprint(safepoints,"Entering phase %d (%s)",
             gc_state.phase, gc_phase_names[gc_state.phase]);
    switch (gc_state.phase) {
    case GC_FLIGHT:
        unmap_gc_page();
        break;
    case GC_MESSAGE:
        gc_notify_early();
        break;
    case GC_INVOKED:
        map_gc_page();
        break;
    case GC_SETTLED:
        gc_notify_final();
        unmap_gc_page();
        break;
    case GC_COLLECT:
        map_gc_page();
        break;
    case GC_NONE:
        gc_done();
        break;
    default:
        break;
    }
}


/* become ready to leave the <old> phase, but unready to leave the <new> phase;
 * `old' can be GC_NONE, it means this thread weren't blocking any state.  `cur'
 * can be GC_NONE, it means this thread wouldn't block GC_NONE, but still wait
 * for it. */
static inline void gc_advance(gc_phase_t cur, gc_phase_t old) {
    odxprint(safepoints,"GC advance request %d (%s) -> %d (%s) in phase %d (%s)",
             old, gc_phase_names[old], cur, gc_phase_names[cur],
             gc_state.phase, gc_phase_names[gc_state.phase]);
    if (cur == old)
        return;
    if (cur == gc_state.phase)
        return;
    if (old < gc_state.phase)
        old = GC_NONE;
    if (old != GC_NONE) {
        gc_state.phase_wait[old]--;
        odxprint(safepoints,"%d holders of phase %d (%s) without me",gc_state.phase_wait[old],old,gc_phase_names[old]);
    }
    if (cur != GC_NONE) {
        gc_state.phase_wait[cur]++;
        odxprint(safepoints,"%d holders of phase %d (%s) with me",gc_state.phase_wait[cur],cur,gc_phase_names[cur]);
    }
    /* roll forth as long as there's no waiters */
    while (gc_state.phase_wait[gc_state.phase]==0
           && gc_state.phase != cur) {
        gc_state.phase = gc_phase_next(gc_state.phase);
        odxprint(safepoints,"no blockers, direct advance to %d (%s)",gc_state.phase,gc_phase_names[gc_state.phase]);
        gc_handle_phase();
#ifdef LISP_FEATURE_WIN32
        WakeAllConditionVariable(&gc_state.phase_cond[gc_state.phase]);
#elif defined LISP_FEATURE_SB_THREAD
        pthread_cond_broadcast(&gc_state.phase_cond[gc_state.phase]);
#endif
    }
    odxprint(safepoints,"going to wait for %d threads",gc_state.phase_wait[gc_state.phase]);
    gc_state_wait(cur);
}

void
thread_register_gc_trigger()
{
    odxprint(misc, "/thread_register_gc_trigger");
    struct thread *self = get_sb_vm_thread();
    WITH_GC_STATE_LOCK {
        if (gc_state.phase == GC_NONE &&
            read_TLS(IN_SAFEPOINT,self)!=T &&
            !thread_blocks_gc(self)) {
            /* A thread (this thread), while doing allocation, has
             * determined that we need to run the garbage collector.
             * But it's in the middle of initializing an object, so we
             * advance to GC_FLIGHT, arming the GSP trap with the idea
             * that there is a GSP trap check once the allocated
             * object is initialized.  Any thread that has GC_PENDING
             * set and GC_INHIBIT clear can take over from here (see
             * thread_in_lisp_raised()), but some thread must. */
            gc_advance(GC_FLIGHT,GC_NONE);
        }
    }
}

#ifdef LISP_FEATURE_SB_SAFEPOINT
static inline int
thread_may_thrupt(os_context_t *ctx)
{
    struct thread * self = get_sb_vm_thread();
    /* Thread may be interrupted if all of these are true:
     * 1) Deferrables are unblocked in the context of the signal that
     *    went into the safepoint.  -- Otherwise the surrounding code
     *    didn't want to be interrupted by a signal, so presumably it didn't
     *    want to be INTERRUPT-THREADed either.
     *    (See interrupt_handle_pending for an exception.)
     * 2) On POSIX: There is no pending signal.  This is important even
     *    after checking the sigmask, since we could be in the
     *    handle_pending trap following re-enabling of interrupts.
     *    Signals are unblocked in that case, but the signal is still
     *    pending; we want to run GC before handling the signal and
     *    therefore entered this safepoint.  But the thruption would call
     *    ALLOW-WITH-INTERRUPTS, and could re-enter the handle_pending
     *    trap, leading to recursion.
     * 3) INTERRUPTS_ENABLED is non-nil.
     * 4) No GC pending; it takes precedence.
     * Note that we are in a safepoint here, which is always outside of PA. */

    if (read_TLS(INTERRUPTS_ENABLED, self) == NIL)
        return 0;

    if (read_TLS(GC_PENDING, self) != NIL)
        return 0;

    if (THREAD_STOP_PENDING(self) != NIL)
        return 0;

#ifdef LISP_FEATURE_WIN32
    if (deferrables_blocked_p(&thread_extra_data(self)->blocked_signal_set))
        return 0;
#else
    /* ctx is NULL if the caller wants to ignore the sigmask. */
    if (ctx && deferrables_blocked_p(os_context_sigmask_addr(ctx)))
        return 0;
    if (read_TLS(INTERRUPT_PENDING, self) != NIL)
        return 0;
#endif

    return 1;
}

// returns 0 if skipped, 1 otherwise
int
check_pending_thruptions(os_context_t *ctx)
{
    struct thread *p = get_sb_vm_thread();

#ifdef LISP_FEATURE_WIN32
    sigset_t oldset;
    /* On Windows, wake_thread/kill_safely does not set THRUPTION_PENDING
     * in the self-kill case; instead we do it here while also clearing the
     * "signal". */
    if (thread_extra_data(p)->pending_signal_set)
        if (__sync_fetch_and_and(&thread_extra_data(p)->pending_signal_set,0))
            write_TLS(THRUPTION_PENDING, T, p);
#endif

    if (!thread_may_thrupt(ctx))
        return 0;
    if (read_TLS(THRUPTION_PENDING, p) == NIL)
        return 0;
    write_TLS(THRUPTION_PENDING, NIL, p);

#ifdef LISP_FEATURE_WIN32
    oldset = thread_extra_data(p)->blocked_signal_set;
    thread_extra_data(p)->blocked_signal_set = deferrable_sigset;
#else
    sigset_t oldset;
    block_deferrable_signals(&oldset);
#endif

    int was_in_lisp = ctx && !foreign_function_call_active_p(p);

    if (was_in_lisp) {
        fake_foreign_function_call(ctx);
    }

    DX_ALLOC_SAP(context_sap, ctx);
    WITH_GC_AT_SAFEPOINTS_ONLY() {
        funcall1(StaticSymbolFunction(RUN_INTERRUPTION), context_sap);
    }

    if (was_in_lisp)
        undo_fake_foreign_function_call(ctx);

#ifdef LISP_FEATURE_WIN32
    thread_extra_data(p)->blocked_signal_set = oldset;
    if (ctx) ctx->sigmask = oldset;
#else
    thread_sigmask(SIG_SETMASK, &oldset, 0);
#endif

    return 1;
}
#endif

int
on_stack_p(struct thread *th, void *esp)
{
    return (void *)th->control_stack_start
        <= esp && esp
        < (void *)th->control_stack_end;
}

#ifndef LISP_FEATURE_WIN32
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

void
assert_on_stack(struct thread *th, void *esp)
{
    if (on_stack_p(th, esp))
        return;
#ifndef LISP_FEATURE_WIN32
    if (on_altstack_p(th, esp))
        lose("thread %p: esp on altstack: %p", th, esp);
#endif
    lose("thread %p: bogus esp: %p (range=%p..%p)", th, esp,
         th->control_stack_start, th->control_stack_end);
}

/// Similar to the one in gc-common, but without the sigmask test.
static boolean can_invoke_post_gc(struct thread* th)
{
    lispobj obj = th->lisp_thread;
    if (!obj) return 0;
    struct thread_instance* lispthread = (void*)(obj - INSTANCE_POINTER_LOWTAG);
    if (!lispthread->primitive_thread) return 0;
    return 1;
}

// returns 0 if skipped, 1 otherwise
int check_pending_gc(__attribute((unused)) os_context_t *ctx)
{
    odxprint(misc, "check_pending_gc");
    struct thread * self = get_sb_vm_thread();
    int done = 0;
    sigset_t sigset;

    if ((read_TLS(IN_SAFEPOINT,self) == T) &&
        ((read_TLS(GC_INHIBIT,self) == NIL) &&
         (read_TLS(GC_PENDING,self) == NIL))) {
        write_TLS(IN_SAFEPOINT,NIL,self);
    }
    if (!thread_blocks_gc(self) && (read_TLS(IN_SAFEPOINT, self) == NIL)) {
        if (read_TLS(GC_PENDING, self) == T) {
            lispobj gc_happened = NIL;

            bind_variable(IN_SAFEPOINT,T,self);
            block_deferrable_signals(&sigset);
            if(read_TLS(GC_PENDING,self)==T)
                gc_happened = funcall1(StaticSymbolFunction(SUB_GC), 0);
            unbind(self);
            thread_sigmask(SIG_SETMASK,&sigset,NULL);
            if (gc_happened == T) {
                /* POST_GC wants to enable interrupts */
                if ((read_TLS(INTERRUPTS_ENABLED,self) == T ||
                     read_TLS(ALLOW_WITH_INTERRUPTS,self) == T)
                    && can_invoke_post_gc(self))
                    funcall0(StaticSymbolFunction(POST_GC));
                done = 1;
            }
        }
    }
    return done;
}


void thread_in_lisp_raised(os_context_t *ctxptr)
{
    struct thread *self = get_sb_vm_thread();
    boolean check_gc_and_thruptions = 0;
    odxprint(safepoints,"%s","thread_in_lisp_raised");

    /* Either we just hit the GSP trap, or we took a PIT stop and
     * there is a stop-for-GC or thruption pending. */
    WITH_GC_STATE_LOCK {
        if (gc_state.phase == GC_FLIGHT &&
            read_TLS(GC_PENDING,self)==T &&
            !thread_blocks_gc(self) && read_TLS(IN_SAFEPOINT,self)!=T) {
            /* Some thread (possibly even this one) that does not have
             * GC_INHIBIT set has noticed that a GC is warranted and
             * advanced the phase to GC_FLIGHT, arming the GSP trap,
             * which this thread has hit.  This thread doesn't have
             * GC_INHIBIT set, and has also noticed that a GC is
             * warranted.  It doesn't matter which thread pushes
             * things forwards at this point, just that it happens.
             * This thread is now a candidate for running the GC, so
             * we advance to GC_QUIET, where the only threads still
             * running are competing to run the GC. */
            set_csp_from_context(self, ctxptr);
            gc_advance(GC_QUIET,GC_FLIGHT);
            set_thread_csp_access(self,1);
            /* If a thread has already reached gc_stop_the_world(),
             * just wait until the world starts again. */
            if (gc_state.collector) {
                gc_advance(GC_NONE,GC_QUIET);
            } else {
                /* ??? Isn't this already T? */
                write_TLS(GC_PENDING,T,self);
            }
            csp_around_foreign_call(self) = 0;
            check_gc_and_thruptions = 1;
        } else {
            /* This thread isn't a candidate for running the GC
             * (yet?), so we can't advance past GC_FLIGHT, so wait for
             * the next phase, GC_MESSAGE, before we do anything. */
            if (gc_state.phase == GC_FLIGHT) {
                gc_state_wait(GC_MESSAGE);
            }
            if (!thread_blocks_gc(self)) {
                /* This thread doesn't have GC_INHIBIT set, so sit
                 * tight and wait for the GC to be over.  The current
                 * phase is GC_MESSAGE, GC_INVOKED, GC_QUIET, or
                 * GC_SETTLED. */
                SET_THREAD_STOP_PENDING(self,NIL);
                set_thread_csp_access(self,1);
                set_csp_from_context(self, ctxptr);
                if (gc_state.phase <= GC_SETTLED)
                    gc_advance(GC_NONE,gc_state.phase);
                else
                    gc_state_wait(GC_NONE);
                csp_around_foreign_call(self) = 0;
                check_gc_and_thruptions = 1;
            } else {
                /* This thread has GC_INHIBIT set, meaning that it's
                 * within a WITHOUT-GCING, so advance from wherever we
                 * are (GC_MESSAGE) to GC_INVOKED so that we can
                 * continue running.  When we leave the WITHOUT-GCING
                 * we'll take a PIT stop and wind up in the case
                 * above...  Or we'll call gc_stop_the_world(). */
                gc_advance(GC_INVOKED,gc_state.phase);
                SET_THREAD_STOP_PENDING(self,T);
                /* Why do we not want to run thruptions here? */
            }
        }
    }
    /* If we still need to GC, and it's not inhibited, call into
     * SUB-GC.  Phase is either GC_QUIET or GC_NONE. */
    if (check_gc_and_thruptions) {
        check_pending_gc(ctxptr);
#ifdef LISP_FEATURE_SB_SAFEPOINT
        while(check_pending_thruptions(ctxptr));
#endif
    }
}

void thread_in_safety_transition(os_context_t *ctxptr)
{
    struct thread *self = get_sb_vm_thread();
    boolean was_in_alien;

    odxprint(safepoints,"%s","GC safety transition");
    WITH_GC_STATE_LOCK {
        was_in_alien = set_thread_csp_access(self,1);
        if (was_in_alien) {
            /* This is an alien->lisp or alien->alien transition. */
            if (thread_blocks_gc(self)) {
                /* gc_notify_early() accounted for this thread as not
                 * being able to leave GC_INVOKED when it armed our
                 * CSP trap, but some other threads may still be
                 * holding things back at GC_MESSAGE, so wait for
                 * GC_INVOKED before continuing.  Don't advance, the
                 * threads preventing exit from GC_MESSAGE have that
                 * privilege. */
                gc_state_wait(GC_INVOKED);
            } else {
                /* This thread isn't within a WITHOUT-GCING, so just
                 * wait until the GC is done before continuing. */
                gc_state_wait(GC_NONE);
            }
        } else {
            /* This is a lisp->alien or lisp->lisp transition. */
            if (!thread_blocks_gc(self)) {
                /* This thread doesn't have GC_INHIBIT set, so sit
                 * tight and wait for the GC to be over.  This is
                 * virtually the same logic as the similar case in
                 * thread_in_lisp_raised(). */
                SET_THREAD_STOP_PENDING(self,NIL);
                set_csp_from_context(self, ctxptr);
                if (gc_state.phase <= GC_SETTLED)
                    gc_advance(GC_NONE,gc_state.phase);
                else
                    gc_state_wait(GC_NONE);
                csp_around_foreign_call(self) = 0;
            } else {
                /* This thread has GC_INHIBIT set, meaning that it's
                 * within a WITHOUT-GCING, so advance from wherever we
                 * are (GC_MESSAGE) to GC_INVOKED so that we can
                 * continue running.  When we leave the WITHOUT-GCING
                 * we'll take a PIT stop and wind up in the case
                 * above...  Or we'll call gc_stop_the_world().  This
                 * logic is identical to the similar case in
                 * thread_in_lisp_raised(). */
                gc_advance(GC_INVOKED,gc_state.phase);
                SET_THREAD_STOP_PENDING(self,T);
            }
        }
    }
#ifdef LISP_FEATURE_SB_SAFEPOINT
    if (was_in_alien) {
        while(check_pending_thruptions(ctxptr));
    }
#endif
}

#ifdef LISP_FEATURE_WIN32
void thread_interrupted(os_context_t *ctxptr)
{
    struct thread *self = get_sb_vm_thread();
    boolean gc_active, was_in_alien;

    odxprint(safepoints,"%s","pending interrupt trap");
    WITH_GC_STATE_LOCK {
        gc_active = gc_cycle_active();
        if (gc_active) {
            was_in_alien = set_thread_csp_access(self,1);
        }
    }
    if (gc_active) {
        if (was_in_alien) {
            thread_in_safety_transition(ctxptr);
        } else {
            thread_in_lisp_raised(ctxptr);
        }
    }
    check_pending_gc(ctxptr);
#ifdef LISP_FEATURE_SB_SAFEPOINT
    while(check_pending_thruptions(ctxptr));
#endif
}
#endif

void
gc_stop_the_world()
{
    struct thread* self = get_sb_vm_thread();
    odxprint(safepoints, "stop the world");
    WITH_GC_STATE_LOCK {
        /* This thread is the collector, and needs special handling in
         * gc_notify_early() and gc_notify_final() because of it. */
        gc_state.collector = self;
        /* And we need to control advancement past GC_QUIET. */
        gc_state.phase_wait[GC_QUIET]++;

        /* So, we won the race to get to gc_stop_the_world().  Now we
         * need to get to GC_COLLECT, where we're the only thread
         * running, so that we can run the collector.  What we do
         * depends on what's already been done. */
        switch(gc_state.phase) {
        case GC_NONE:
            gc_advance(GC_QUIET,gc_state.phase);
        case GC_FLIGHT:
        case GC_MESSAGE:
        case GC_INVOKED:
            if ((gc_state.phase == GC_MESSAGE)
                || (gc_state.phase == GC_INVOKED)) {
                /* If the phase was GC_MESSAGE or GC_INVOKED, we were
                 * accounted as "in alien", and are on the GC_INVOKED
                 * waitcount, or we were "in lisp" but in WITHOUT-GCING,
                 * which led to us putting OURSELVES on the GC_INVOKED
                 * waitcount. */
                gc_advance(GC_QUIET, GC_INVOKED);
            } else {
                gc_state_wait(GC_QUIET);
            }
        case GC_QUIET:
            /* Some number of threads were trying to get to GC_QUIET.
             * But this thread is sufficient to be able to leave
             * GC_QUIET. */
            gc_state.phase_wait[GC_QUIET]=1;
            /* Advance through GC_SETTLED to GC_COLLECT, stopping the
             * other threads that were racing to stop the world. */
            gc_advance(GC_COLLECT,GC_QUIET);
            break;
        case GC_COLLECT:
            break;
        default:
            lose("Stopping the world in unexpected state %d",gc_state.phase);
            break;
        }
        set_thread_csp_access(self,1);
    }
    SET_THREAD_STOP_PENDING(self,NIL);
}


void gc_start_the_world()
{
    odxprint(safepoints,"%s","start the world");
    WITH_GC_STATE_LOCK {
        gc_state.collector = NULL;
        gc_advance(GC_NONE,GC_COLLECT);
    }
}


#ifdef LISP_FEATURE_SB_SAFEPOINT
/* wake_thread(thread) -- ensure a thruption delivery to
 * `thread'. */

# ifdef LISP_FEATURE_WIN32

void
wake_thread_io(struct thread * thread)
{
    SetEvent(thread_private_events(thread,1));
    win32_maybe_interrupt_io(thread);
}

void wake_thread_impl(struct thread_instance *lispthread)
{
    struct thread* thread = (void*)lispthread->primitive_thread;
    wake_thread_io(thread);

    if (read_TLS(THRUPTION_PENDING,thread)==T)
        return;

    write_TLS(THRUPTION_PENDING,T,thread);

    if ((read_TLS(GC_PENDING,thread)==T)
        ||(THREAD_STOP_PENDING(thread)==T)
        )
        return;

    wake_thread_io(thread);
    mutex_release(&all_threads_lock);

    WITH_GC_STATE_LOCK {
        if (gc_state.phase == GC_NONE) {
            gc_advance(GC_INVOKED,GC_NONE);
            gc_advance(GC_NONE,GC_INVOKED);
        }
    }

    mutex_acquire(&all_threads_lock);
    return;
}
# else
void wake_thread_impl(struct thread_instance *lispthread)
{
    struct thread *thread = (void*)lispthread->primitive_thread;
    struct thread *self = get_sb_vm_thread();

    /* Must not and need not attempt to signal ourselves while we're the
     * STW initiator. */
    if (thread == self) {
        write_TLS(THRUPTION_PENDING,T,self);
        while (check_pending_thruptions(0 /* ignore the sigmask */))
            ;
        return;
    }

    /* We are not in a signal handler here, so need to block signals
     * manually. */
    sigset_t oldset;
    block_deferrable_signals(&oldset);

    WITH_GC_STATE_LOCK {
        if (gc_state.phase == GC_NONE) {
            odxprint(safepoints, "wake_thread_posix: invoking");
            gc_advance(GC_INVOKED,GC_NONE);
            {
                /* I do not know whether WITH_ALL_THREADS_LOCK was only to avoid
                 * hitting wild pointers in the loop over threads (gone now)
                 * or whether it _also_ had an effect on the safepoint state.
                 * Out of caution I'm leaving it in despite removing the loop */

                /* only if in foreign code, notify using signal */
                WITH_ALL_THREADS_LOCK {
                    do {
                            odxprint(safepoints, "wake_thread_posix: found");
                            write_TLS(THRUPTION_PENDING,T,thread);
                            if (read_TLS(GC_PENDING,thread) == T
                                || THREAD_STOP_PENDING(thread) == T)
                                break;

                            if (os_get_csp(thread)) {
                                odxprint(safepoints, "wake_thread_posix: kill");
                                /* ... and in foreign code.  Push it into a safety
                                 * transition. */
                                int status = pthread_kill((pthread_t)lispthread->os_thread, SIGURG);
                                if (status)
                                    lose("wake_thread_posix: pthread_kill failed with %d",
                                         status);
                            }
                    } while(0);
                }
            }
            gc_advance(GC_NONE,GC_INVOKED);
        } else {
            odxprint(safepoints, "wake_thread_posix: passive");
            write_TLS(THRUPTION_PENDING, T, thread);
        }
    }
    thread_sigmask(SIG_SETMASK, &oldset, 0);
}
#endif /* !LISP_FEATURE_WIN32 */
#endif /* LISP_FEATURE_SB_SAFEPOINT */

void* os_get_csp(struct thread* th)
{
    FSHOW_SIGNAL((stderr, "Thread %p has CSP %p, stack [%p,%p]\n",
                  th,
                  (void*)csp_around_foreign_call(th),
                  th->control_stack_start,
                  th->control_stack_end));
    return (void*)csp_around_foreign_call(th);
}


#ifndef LISP_FEATURE_WIN32

# ifdef LISP_FEATURE_SB_SAFEPOINT
/* This is basically what 'low_level_maybe_now_maybe_later' was (which doesn't exist),
 * but with a different name, and different way of deciding to defer the signal */
void thruption_handler(__attribute__((unused)) int signal,
                       __attribute__((unused)) siginfo_t *info,
                       os_context_t *ctx)
{
    struct thread *self = get_sb_vm_thread();

    void *transition_sp = os_get_csp(self);
    if (!transition_sp)
        /* In Lisp code.  Do not run thruptions asynchronously.  The
         * next safepoint will take care of it. */
        return;

#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    if (!foreign_function_call_active_p(self))
        lose("csp && !ffca");
#endif

    /* In C code.  As a rule, we assume that running thruptions is OK. */
    csp_around_foreign_call(self) = 0;
    thread_in_lisp_raised(ctx);
    csp_around_foreign_call(self) = (intptr_t) transition_sp;
}
# endif

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
/* Trap trampolines are in target-assem.S so that they pick up the
 * trap instruction selection features automatically. */
extern lispobj
handle_global_safepoint_violation(lispobj fun, lispobj *args, int nargs);
extern lispobj
handle_csp_safepoint_violation(lispobj fun, lispobj *args, int nargs);
#endif

int
handle_safepoint_violation(os_context_t *ctx, os_vm_address_t fault_address)
{
    struct thread *self = get_sb_vm_thread();

    FSHOW_SIGNAL((stderr, "fault_address = %p, sp = %p, &csp = %p\n",
                  fault_address,
                  GC_SAFEPOINT_TRAP_ADDR, csp_around_foreign_call(self)));


    if (fault_address == (os_vm_address_t) GC_SAFEPOINT_TRAP_ADDR) {
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        /* We're on the altstack and don't want to run Lisp code. */
        arrange_return_to_c_function(ctx, handle_global_safepoint_violation, 0);
#else
        if (foreign_function_call_active_p(self)) lose("GSP trap in C?");
        fake_foreign_function_call(ctx);
        thread_in_lisp_raised(ctx);
        undo_fake_foreign_function_call(ctx);
#endif
        return 1;
    }

    if ((1+THREAD_HEADER_SLOTS)+(lispobj*)fault_address == (lispobj*)self) {
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        arrange_return_to_c_function(ctx, handle_csp_safepoint_violation, 0);
#else
        if (!foreign_function_call_active_p(self)) lose("CSP trap in Lisp?");
        thread_in_safety_transition(ctx);
#endif
        return 1;
    }

    /* not a safepoint */
    return 0;
}
#endif /* LISP_FEATURE_WIN32 */

#endif /* LISP_FEATURE_SB_SAFEPOINT -- entire file */
