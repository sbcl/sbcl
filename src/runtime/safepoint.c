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
#ifdef LISP_FEATURE_MACH_EXCEPTION_HANDLER
#include <mach/mach.h>
#include <mach/mach_error.h>
#include <mach/mach_types.h>
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
#include "pseudo-atomic.h"
#include "interrupt.h"
#include "lispregs.h"

#if !defined(LISP_FEATURE_WIN32)
/* win32-os.c covers these, but there is no unixlike-os.c, so the normal
 * definition goes here.  Fixme: (Why) don't these work for Windows?
 */
void
alloc_gc_page()
{
    os_validate(GC_SAFEPOINT_PAGE_ADDR, 4);
}

void
map_gc_page()
{
    odxprint(misc, "map_gc_page");
    os_protect((void *) GC_SAFEPOINT_PAGE_ADDR,
               4,
               OS_VM_PROT_READ | OS_VM_PROT_WRITE);
}

void
unmap_gc_page()
{
    odxprint(misc, "unmap_gc_page");
    os_protect((void *) GC_SAFEPOINT_PAGE_ADDR, 4, OS_VM_PROT_NONE);
}
#endif /* !LISP_FEATURE_WIN32 */

/* Planned state progressions:
 *
 * none -> flight:
 *
 *     unmap_gc_page(). No blockers (GC_NONE can be left at any * moment).
 *
 * flight -> message:
 *
 *     happens when a master thread enters its trap.
 *
 *     The only blocker for flight mode is the master thread itself
 *     (GC_FLIGHT can't be left until the master thread traps).
 *
 * message -> invoked:
 *
 *     happens after each (other) thread is notified, i.e. it will
 *     eventually stop (already stopped). map_gc_page().
 *
 *     Each thread with empty CSP disagrees to leave GC_MESSAGE phase.
 *
 * invoked -> collect:
 *
 *     happens when every gc-inhibitor comes to completion (that's
 *     normally pending interrupt trap).
 *
 *     NB gc_stop_the_world, if it happens in non-master thread, "takes
 *     over" as a master, also deregistering itself as a blocker
 *     (i.e. it's ready to leave GC_INVOKED, but now it objects to
 *     leaving GC_COLLECT; this "usurpation" doesn't require any change
 *     to GC_COLLECT counter: for the counter, it's immaterial _which_
 *     thread is waiting).
 *
 * collect -> none:
 *
 *     happens at gc_start_the_world (that should always happen in the
 *     master).
 *
 *     Any thread waiting until GC end now continues.
 */
struct gc_state {
    /* Flag: conditions are initialized */
    boolean initialized;

    /* Per-process lock for gc_state */
    pthread_mutex_t lock;

    /* Conditions: one per phase */
    pthread_cond_t phase_cond[GC_NPHASES];

    /* For each [current or future] phase, a number of threads not yet ready to
     * leave it */
    int phase_wait[GC_NPHASES];

    /* Master thread controlling the topmost stop/gc/start sequence */
    struct thread* master;
    struct thread* collector;

    /* Current GC phase */
    gc_phase_t phase;
};

static struct gc_state gc_state = {
    .lock = PTHREAD_MUTEX_INITIALIZER,
    .phase = GC_NONE,
};

void
gc_state_lock()
{
    odxprint(safepoints,"GC state [%p] to be locked",gc_state.lock);
    gc_assert(0==pthread_mutex_lock(&gc_state.lock));
    if (gc_state.master) {
        fprintf(stderr,"GC state lock glitch [%p] in thread %p phase %d\n",
                gc_state.master,arch_os_get_current_thread(),gc_state.phase);
        odxprint(safepoints,"GC state lock glitch [%p]",gc_state.master);
    }
    gc_assert(!gc_state.master);
    gc_state.master = arch_os_get_current_thread();
    if (!gc_state.initialized) {
        int i;
        for (i=GC_NONE; i<GC_NPHASES; ++i)
            pthread_cond_init(&gc_state.phase_cond[i],NULL);
        gc_state.initialized = 1;
    }
    odxprint(safepoints,"GC state [%p] locked in phase %d",gc_state.lock, gc_state.phase);
}

void
gc_state_unlock()
{
    odxprint(safepoints,"GC state to be unlocked in phase %d",gc_state.phase);
    gc_assert(arch_os_get_current_thread()==gc_state.master);
    gc_state.master = NULL;
    gc_assert(0==pthread_mutex_unlock(&gc_state.lock));
    odxprint(safepoints,"%s","GC state unlocked");
}

void
gc_state_wait(gc_phase_t phase)
{
    struct thread* self = arch_os_get_current_thread();
    odxprint(safepoints,"Waiting for %d -> %d [%d holders]",
             gc_state.phase,phase,gc_state.phase_wait[gc_state.phase]);
    gc_assert(gc_state.master == self);
    gc_state.master = NULL;
    while(gc_state.phase != phase && !(phase == GC_QUIET && (gc_state.phase > GC_QUIET)))
        pthread_cond_wait(&gc_state.phase_cond[phase],&gc_state.lock);
    gc_assert(gc_state.master == NULL);
    gc_state.master = self;
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
     * precise GC already keeps track of the stack pointer itself. */
    void **sp = (void **) 0xEEEEEEEE;
#endif
    *self->csp_around_foreign_call = (lispobj) sp;
}


static inline gc_phase_t gc_phase_next(gc_phase_t old) {
    return (old+1) % GC_NPHASES;
}

static inline gc_phase_t thread_gc_phase(struct thread* p)
{
    boolean inhibit = (SymbolTlValue(GC_INHIBIT,p)==T)||
        (SymbolTlValue(IN_WITHOUT_GCING,p)==IN_WITHOUT_GCING);

    boolean inprogress =
        (SymbolTlValue(GC_PENDING,p)!=T&& SymbolTlValue(GC_PENDING,p)!=NIL);

    return
        inprogress ? (gc_state.collector && (gc_state.collector != p)
                      ? GC_NONE : GC_QUIET)
        : (inhibit ? GC_INVOKED : GC_NONE);
}

static inline void thread_gc_promote(struct thread* p, gc_phase_t cur, gc_phase_t old) {
    if (old != GC_NONE)
        gc_state.phase_wait[old]--;
    if (cur != GC_NONE) {
        gc_state.phase_wait[cur]++;
    }
    if (cur != GC_NONE)
        SetTlSymbolValue(STOP_FOR_GC_PENDING,T,p);
}

/* set_thread_csp_access -- alter page permissions for not-in-Lisp
   flag (Lisp Stack Top) of the thread `p'. The flag may be modified
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
set_thread_csp_access(struct thread* p, boolean writable)
{
    os_protect((os_vm_address_t) p->csp_around_foreign_call,
               THREAD_CSP_PAGE_SIZE,
               writable? (OS_VM_PROT_READ|OS_VM_PROT_WRITE)
               : (OS_VM_PROT_READ));
    return !!*p->csp_around_foreign_call;
}

static inline void gc_notify_early()
{
    struct thread *self = arch_os_get_current_thread(), *p;
    odxprint(safepoints,"%s","global notification");
    pthread_mutex_lock(&all_threads_lock);
    for_each_thread(p) {
        if (p==self)
            continue;
        odxprint(safepoints,"notifying thread %p csp %p",p,*p->csp_around_foreign_call);
        if (!set_thread_csp_access(p,0)) {
            thread_gc_promote(p, gc_state.phase, GC_NONE);
        } else {
            thread_gc_promote(p, thread_gc_phase(p), GC_NONE);
        }
    }
    pthread_mutex_unlock(&all_threads_lock);
}

static inline void gc_notify_final()
{
    struct thread *p;
    odxprint(safepoints,"%s","global notification");
    gc_state.phase_wait[gc_state.phase]=0;
    pthread_mutex_lock(&all_threads_lock);
    for_each_thread(p) {
        if (p == gc_state.collector)
            continue;
        odxprint(safepoints,"notifying thread %p csp %p",p,*p->csp_around_foreign_call);
        if (!set_thread_csp_access(p,0)) {
            thread_gc_promote(p, gc_state.phase, GC_NONE);
        }
    }
    pthread_mutex_unlock(&all_threads_lock);
}

static inline void gc_done()
{
    struct thread *self = arch_os_get_current_thread(), *p;
    boolean inhibit = (SymbolTlValue(GC_INHIBIT,self)==T);

    odxprint(safepoints,"%s","global denotification");
    pthread_mutex_lock(&all_threads_lock);
    for_each_thread(p) {
        if (inhibit && (SymbolTlValue(GC_PENDING,p)==T))
            SetTlSymbolValue(GC_PENDING,NIL,p);
        set_thread_csp_access(p,1);
    }
    pthread_mutex_unlock(&all_threads_lock);
}

static inline void gc_handle_phase()
{
    odxprint(safepoints,"Entering phase %d",gc_state.phase);
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
    odxprint(safepoints,"GC advance request %d -> %d in phase %d",old,cur,gc_state.phase);
    if (cur == old)
        return;
    if (cur == gc_state.phase)
        return;
    if (old < gc_state.phase)
        old = GC_NONE;
    if (old != GC_NONE) {
        gc_state.phase_wait[old]--;
        odxprint(safepoints,"%d holders of phase %d without me",gc_state.phase_wait[old],old);
    }
    if (cur != GC_NONE) {
        gc_state.phase_wait[cur]++;
        odxprint(safepoints,"%d holders of phase %d with me",gc_state.phase_wait[cur],cur);
    }
    /* roll forth as long as there's no waiters */
    while (gc_state.phase_wait[gc_state.phase]==0
           && gc_state.phase != cur) {
        gc_state.phase = gc_phase_next(gc_state.phase);
        odxprint(safepoints,"no blockers, direct advance to %d",gc_state.phase);
        gc_handle_phase();
        pthread_cond_broadcast(&gc_state.phase_cond[gc_state.phase]);
    }
    odxprint(safepoints,"going to wait for %d threads",gc_state.phase_wait[gc_state.phase]);
    gc_state_wait(cur);
}

void
thread_register_gc_trigger()
{
    odxprint(misc, "/thread_register_gc_trigger");
    struct thread *self = arch_os_get_current_thread();
    gc_state_lock();
    if (gc_state.phase == GC_NONE &&
        SymbolTlValue(IN_SAFEPOINT,self)!=T &&
        thread_gc_phase(self)==GC_NONE) {
        gc_advance(GC_FLIGHT,GC_NONE);
    }
    gc_state_unlock();
}

static inline int
thread_may_gc()
{
    /* Thread may gc if all of these are true:
     * 1) GC_INHIBIT == NIL  (outside of protected part of without-gcing)
     * 2) GC_PENDING != :in-progress    (outside of recursion protection)
     * Note that we are in a safepoint here, which is always outside of PA. */

    struct thread *self = arch_os_get_current_thread();
    return (SymbolValue(GC_INHIBIT, self) == NIL
            && (SymbolTlValue(GC_PENDING, self) == T ||
                SymbolTlValue(GC_PENDING, self) == NIL));
}

#ifdef LISP_FEATURE_SB_THRUPTION
static inline int
thread_may_thrupt(os_context_t *ctx)
{
    struct thread * self = arch_os_get_current_thread();
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

    if (SymbolValue(INTERRUPTS_ENABLED, self) == NIL)
        return 0;

    if (SymbolValue(GC_PENDING, self) != NIL)
        return 0;

    if (SymbolValue(STOP_FOR_GC_PENDING, self) != NIL)
        return 0;

#ifdef LISP_FEATURE_WIN32
    if (deferrables_blocked_p(&self->os_thread->blocked_signal_set))
        return 0;
#else
    /* ctx is NULL if the caller wants to ignore the sigmask. */
    if (ctx && deferrables_blocked_p(os_context_sigmask_addr(ctx)))
        return 0;
    if (SymbolValue(INTERRUPT_PENDING, self) != NIL)
        return 0;
#endif

    if (SymbolValue(RESTART_CLUSTERS, self) == NIL)
        /* This special case prevents TERMINATE-THREAD from hitting
         * during INITIAL-THREAD-FUNCTION before it's ready.  Curiously,
         * deferrables are already unblocked there.  Further
         * investigation may be in order. */
        return 0;

    return 1;
}

// returns 0 if skipped, 1 otherwise
int
check_pending_thruptions(os_context_t *ctx)
{
    struct thread *p = arch_os_get_current_thread();

#ifdef LISP_FEATURE_WIN32
    pthread_t pself = p->os_thread;
    sigset_t oldset;
    /* On Windows, wake_thread/kill_safely does not set THRUPTION_PENDING
     * in the self-kill case; instead we do it here while also clearing the
     * "signal". */
    if (pself->pending_signal_set)
        if (__sync_fetch_and_and(&pself->pending_signal_set,0))
            SetSymbolValue(THRUPTION_PENDING, T, p);
#endif

    if (!thread_may_thrupt(ctx))
        return 0;
    if (SymbolValue(THRUPTION_PENDING, p) == NIL)
        return 0;
    SetSymbolValue(THRUPTION_PENDING, NIL, p);

#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    int was_in_lisp = !foreign_function_call_active_p(p);
    if (was_in_lisp) {
        if (!ctx)
            lose("self-kill bug");
        fake_foreign_function_call(ctx);
    }
#endif

#ifdef LISP_FEATURE_WIN32
    oldset = pself->blocked_signal_set;
    pself->blocked_signal_set = deferrable_sigset;
    if (ctx) fake_foreign_function_call(ctx);
#else
    sigset_t oldset;
    block_deferrable_signals(0, &oldset);
#endif

    funcall0(StaticSymbolFunction(RUN_INTERRUPTION));

#ifdef LISP_FEATURE_WIN32
    if (ctx) undo_fake_foreign_function_call(ctx);
    pself->blocked_signal_set = oldset;
    if (ctx) ctx->sigmask = oldset;
#else
    thread_sigmask(SIG_SETMASK, &oldset, 0);
#endif

#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    if (was_in_lisp)
        undo_fake_foreign_function_call(ctx);
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
    void *start = (void *)th+dynamic_values_bytes;
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
    lose("thread %p: bogus esp: %p", th, esp);
}

// returns 0 if skipped, 1 otherwise
int
check_pending_gc(os_context_t *ctx)
{
    odxprint(misc, "check_pending_gc");
    struct thread * self = arch_os_get_current_thread();
    int done = 0;
    sigset_t sigset;

    if ((SymbolValue(IN_SAFEPOINT,self) == T) &&
        ((SymbolValue(GC_INHIBIT,self) == NIL) &&
         (SymbolValue(GC_PENDING,self) == NIL))) {
        SetSymbolValue(IN_SAFEPOINT,NIL,self);
    }
    if (thread_may_gc() && (SymbolValue(IN_SAFEPOINT, self) == NIL)) {
        if ((SymbolTlValue(GC_PENDING, self) == T)) {
            lispobj gc_happened = NIL;

            bind_variable(IN_SAFEPOINT,T,self);
            block_deferrable_signals(NULL,&sigset);
            if(SymbolTlValue(GC_PENDING,self)==T)
                gc_happened = funcall0(StaticSymbolFunction(SUB_GC));
            unbind(self);
            thread_sigmask(SIG_SETMASK,&sigset,NULL);
            if (gc_happened == T) {
                /* POST_GC wants to enable interrupts */
                if (SymbolValue(INTERRUPTS_ENABLED,self) == T ||
                    SymbolValue(ALLOW_WITH_INTERRUPTS,self) == T) {
                    odxprint(misc, "going to call POST_GC");
                    funcall0(StaticSymbolFunction(POST_GC));
                }
                done = 1;
            }
        }
    }
    return done;
}


void thread_in_lisp_raised(os_context_t *ctxptr)
{
    struct thread *self = arch_os_get_current_thread();
    gc_phase_t phase;
    odxprint(safepoints,"%s","thread_in_lisp_raised");
    gc_state_lock();

    if (gc_state.phase == GC_FLIGHT &&
        SymbolTlValue(GC_PENDING,self)==T &&
        thread_gc_phase(self)==GC_NONE &&
        thread_may_gc() && SymbolTlValue(IN_SAFEPOINT,self)!=T) {
        set_csp_from_context(self, ctxptr);
        gc_advance(GC_QUIET,GC_FLIGHT);
        set_thread_csp_access(self,1);
        if (gc_state.collector) {
            gc_advance(GC_NONE,GC_QUIET);
        } else {
            *self->csp_around_foreign_call = 0;
            SetTlSymbolValue(GC_PENDING,T,self);
        }
        gc_state_unlock();
        check_pending_gc(ctxptr);
#ifdef LISP_FEATURE_SB_THRUPTION
        while(check_pending_thruptions(ctxptr));
#endif
        return;
    }
    if (gc_state.phase == GC_FLIGHT) {
        gc_state_wait(GC_MESSAGE);
    }
    phase = thread_gc_phase(self);
    if (phase == GC_NONE) {
        SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,self);
        set_thread_csp_access(self,1);
        set_csp_from_context(self, ctxptr);
        if (gc_state.phase <= GC_SETTLED)
            gc_advance(phase,gc_state.phase);
        else
            gc_state_wait(phase);
        *self->csp_around_foreign_call = 0;
        gc_state_unlock();
        check_pending_gc(ctxptr);
#ifdef LISP_FEATURE_SB_THRUPTION
        while(check_pending_thruptions(ctxptr));
#endif
    } else {
        gc_advance(phase,gc_state.phase);
        SetTlSymbolValue(STOP_FOR_GC_PENDING,T,self);
        gc_state_unlock();
    }
}

void thread_in_safety_transition(os_context_t *ctxptr)
{
    struct thread *self = arch_os_get_current_thread();

    odxprint(safepoints,"%s","GC safety transition");
    gc_state_lock();
    if (set_thread_csp_access(self,1)) {
        gc_state_wait(thread_gc_phase(self));
        gc_state_unlock();
#ifdef LISP_FEATURE_SB_THRUPTION
        while(check_pending_thruptions(ctxptr));
#endif
    } else {
        gc_phase_t phase = thread_gc_phase(self);
        if (phase == GC_NONE) {
            SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,self);
            set_csp_from_context(self, ctxptr);
            if (gc_state.phase <= GC_SETTLED)
                gc_advance(phase,gc_state.phase);
            else
                gc_state_wait(phase);
            *self->csp_around_foreign_call = 0;
        } else {
            gc_advance(phase,gc_state.phase);
            SetTlSymbolValue(STOP_FOR_GC_PENDING,T,self);
        }
        gc_state_unlock();
    }
}

void thread_interrupted(os_context_t *ctxptr)
{
    struct thread *self = arch_os_get_current_thread();

    odxprint(safepoints,"%s","pending interrupt trap");
    gc_state_lock();
    if (gc_state.phase != GC_NONE) {
        if (set_thread_csp_access(self,1)) {
            gc_state_unlock();
            thread_in_safety_transition(ctxptr);
        } else {
            gc_state_unlock();
            thread_in_lisp_raised(ctxptr);
        }
    } else {
        gc_state_unlock();
    }
    check_pending_gc(ctxptr);
#ifdef LISP_FEATURE_SB_THRUPTION
    while(check_pending_thruptions(ctxptr));
#endif
}

void
gc_stop_the_world()
{
    struct thread* self = arch_os_get_current_thread();
    odxprint(safepoints, "stop the world");
    gc_state_lock();
    gc_state.collector = self;
    gc_state.phase_wait[GC_QUIET]++;

    switch(gc_state.phase) {
    case GC_NONE:
        gc_advance(GC_QUIET,gc_state.phase);
    case GC_FLIGHT:
    case GC_MESSAGE:
    case GC_INVOKED:
        gc_state_wait(GC_QUIET);
    case GC_QUIET:
        gc_state.phase_wait[GC_QUIET]=1;
        gc_advance(GC_COLLECT,GC_QUIET);
        break;
    case GC_COLLECT:
        break;
    default:
        lose("Stopping the world in unexpected state %d",gc_state.phase);
        break;
    }
    set_thread_csp_access(self,1);
    gc_state_unlock();
    SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,self);
}


void gc_start_the_world()
{
    odxprint(safepoints,"%s","start the world");
    gc_state_lock();
    gc_state.collector = NULL;
    SetSymbolValue(IN_WITHOUT_GCING,IN_WITHOUT_GCING,
                     arch_os_get_current_thread());
    gc_advance(GC_NONE,GC_COLLECT);
    gc_state_unlock();
}


#ifdef LISP_FEATURE_SB_THRUPTION
/* wake_thread(thread) -- ensure a thruption delivery to
 * `thread'. */

# ifdef LISP_FEATURE_WIN32

void
wake_thread_io(struct thread * thread)
{
    SetEvent(thread->private_events.events[1]);
    win32_maybe_interrupt_io(thread);
}

void
wake_thread_win32(struct thread *thread)
{
    struct thread *self = arch_os_get_current_thread();

    wake_thread_io(thread);

    if (SymbolTlValue(THRUPTION_PENDING,thread)==T)
        return;

    SetTlSymbolValue(THRUPTION_PENDING,T,thread);

    if ((SymbolTlValue(GC_PENDING,thread)==T)||
        (SymbolTlValue(STOP_FOR_GC_PENDING,thread)==T))
        return;

    wake_thread_io(thread);
    pthread_mutex_unlock(&all_threads_lock);

    gc_state_lock();
    if (gc_state.phase == GC_NONE) {
        gc_advance(GC_INVOKED,GC_NONE);
        gc_advance(GC_NONE,GC_INVOKED);
    }
    gc_state_unlock();

    pthread_mutex_lock(&all_threads_lock);
    return;
}
# else
int
wake_thread_posix(os_thread_t os_thread)
{
    int found = 0;
    struct thread *thread;
    struct thread *self = arch_os_get_current_thread();

    /* Must not and need not attempt to signal ourselves while we're the
     * STW initiator. */
    if (self->os_thread == os_thread) {
        SetTlSymbolValue(THRUPTION_PENDING,T,self);
        WITH_GC_AT_SAFEPOINTS_ONLY()
            while (check_pending_thruptions(0 /* ignore the sigmask */))
                ;
        return 0;
    }

    /* We are not in a signal handler here, so need to block signals
     * manually. */
    sigset_t oldset;
    block_deferrable_signals(0, &oldset);

    gc_state_lock();
    if (gc_state.phase == GC_NONE) {
        odxprint(safepoints, "wake_thread_posix: invoking");
        gc_advance(GC_INVOKED,GC_NONE);
        {
            /* only if in foreign code, notify using signal */
            pthread_mutex_lock(&all_threads_lock);
            for_each_thread (thread)
                if (thread->os_thread == os_thread) {
                    /* it's still alive... */
                    found = 1;

                    odxprint(safepoints, "wake_thread_posix: found");
                    SetTlSymbolValue(THRUPTION_PENDING,T,thread);
                    if (SymbolTlValue(GC_PENDING,thread) == T
                        || SymbolTlValue(STOP_FOR_GC_PENDING,thread) == T)
                        break;

                    if (os_get_csp(thread)) {
                        odxprint(safepoints, "wake_thread_posix: kill");
                        /* ... and in foreign code.  Push it into a safety
                         * transition. */
                        int status = pthread_kill(os_thread, SIGPIPE);
                        if (status)
                            lose("wake_thread_posix: pthread_kill failed with %d\n",
                                 status);
                    }
                    break;
                }
            pthread_mutex_unlock(&all_threads_lock);
        }
        gc_advance(GC_NONE,GC_INVOKED);
    } else {
        odxprint(safepoints, "wake_thread_posix: passive");
        /* We are not able to wake the thread up actively, but maybe
         * some other thread will take care of it.  Kludge: Unless it is
         * in foreign code.  Let's at least try to get our return value
         * right. */
        pthread_mutex_lock(&all_threads_lock);
        for_each_thread (thread)
            if (thread->os_thread == os_thread) {
                SetTlSymbolValue(THRUPTION_PENDING,T,thread);
                found = 1;
                break;
            }
        pthread_mutex_unlock(&all_threads_lock);
    }
    gc_state_unlock();

    odxprint(safepoints, "wake_thread_posix leaving, found=%d", found);
    thread_sigmask(SIG_SETMASK, &oldset, 0);
    return found ? 0 : -1;
}
#endif /* !LISP_FEATURE_WIN32 */
#endif /* LISP_FEATURE_SB_THRUPTION */

void**
os_get_csp(struct thread* th)
{
    FSHOW_SIGNAL((stderr, "Thread %p has CSP *(%p) == %p, stack [%p,%p]\n",
                  th,
                  th->csp_around_foreign_call,
                  *(void***)th->csp_around_foreign_call,
                  th->control_stack_start,
                  th->control_stack_end));
    return *(void***)th->csp_around_foreign_call;
}


#ifndef LISP_FEATURE_WIN32

# ifdef LISP_FEATURE_SB_THRUPTION
void
thruption_handler(int signal, siginfo_t *info, os_context_t *ctx)
{
    struct thread *self = arch_os_get_current_thread();

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
    *self->csp_around_foreign_call = 0;
    thread_in_lisp_raised(ctx);
    *self->csp_around_foreign_call = transition_sp;
}
# endif

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK

/* Designed to be of the same type as call_into_lisp.  Ignores its
 * arguments. */
lispobj
handle_global_safepoint_violation(lispobj fun, lispobj *args, int nargs)
{
#if trap_GlobalSafepoint != 0x1a
# error trap_GlobalSafepoint mismatch
#endif
    asm("int3; .byte 0x1a;");
    return 0;
}

lispobj
handle_csp_safepoint_violation(lispobj fun, lispobj *args, int nargs)
{
#if trap_CspSafepoint != 0x1b
# error trap_CspSafepoint mismatch
#endif
    asm("int3; .byte 0x1b;");
    return 0;
}

#endif /* C_STACK_IS_CONTROL_STACK */

int
handle_safepoint_violation(os_context_t *ctx, os_vm_address_t fault_address)
{
    FSHOW_SIGNAL((stderr, "fault_address = %p, sp = %p, &csp = %p\n",
                  fault_address,
                  GC_SAFEPOINT_PAGE_ADDR,
                  arch_os_get_current_thread()->csp_around_foreign_call));

    struct thread *self = arch_os_get_current_thread();

    if (fault_address == (os_vm_address_t) GC_SAFEPOINT_PAGE_ADDR) {
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

    if (fault_address == (os_vm_address_t) self->csp_around_foreign_call) {
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

#if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)
void
signal_handler_callback(lispobj run_handler, int signo, void *info, void *ctx)
{
    init_thread_data scribble;
    void *args[2];
    DX_ALLOC_SAP(args_sap, args);

    args[0] = info;
    args[1] = ctx;

    attach_os_thread(&scribble);

    odxprint(misc, "callback from signal handler thread for: %d\n", signo);
    funcall3(StaticSymbolFunction(SIGNAL_HANDLER_CALLBACK),
             run_handler, make_fixnum(signo), args_sap);

    detach_os_thread(&scribble);
    return;
}
#endif

#endif /* LISP_FEATURE_SB_SAFEPOINT -- entire file */
