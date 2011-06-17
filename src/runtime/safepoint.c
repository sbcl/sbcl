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

/* Temporarily, this macro is a wrapper for FSHOW_SIGNAL.  Ultimately,
 * it will be restored to its full win32 branch functionality, where it
 * provides a very useful tracing mechanism that is configurable at
 * runtime. */
#define odxprint_show(what, fmt, args...)                       \
     do {                                                       \
         struct thread *__self = arch_os_get_current_thread();  \
         FSHOW_SIGNAL((stderr, "[%p/%p:%s] " fmt "\n",          \
                       __self,                                  \
                       __self->os_thread,                       \
                       #what,                                   \
                       ##args));                                \
     } while (0)

#if QSHOW_SIGNALS
# define odxprint odxprint_show
#else
# define odxprint(what, fmt, args...) do {} while (0)
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

    gc_assert(!os_get_csp(p));

    if (!thread_may_thrupt(ctx))
        return 0;
    if (SymbolValue(THRUPTION_PENDING, p) == NIL)
        return 0;
    SetSymbolValue(THRUPTION_PENDING, NIL, p);

    sigset_t oldset;
    block_deferrable_signals(0, &oldset);

    funcall0(StaticSymbolFunction(RUN_INTERRUPTION));

    pthread_sigmask(SIG_SETMASK, &oldset, 0);
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
            unbind_variable(IN_SAFEPOINT,self);
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

/* Several ideas on interthread signalling should be
   tried. Implementation below was chosen for its moderate size and
   relative simplicity.

   Mutex is the only (conventional) system synchronization primitive
   used by it. Some of the code below looks weird with this
   limitation; rwlocks, Windows Event Objects, or perhaps pthread
   barriers could be used to improve clarity.

   No condvars here: our pthreads_win32 is great, but it doesn't
   provide wait morphing optimization; let's avoid extra context
   switches and extra contention. */

struct gc_dispatcher {

    /* Held by the first thread that decides to signal all others, for
       the entire period while common GC safepoint page is
       unmapped. This thread is called `STW (stop-the-world)
       initiator' below. */
    pthread_mutex_t mx_gpunmapped;

    /* Held by STW initiator while it updates th_stw_initiator and
       takes other locks in this structure */
    pthread_mutex_t mx_gptransition;

    /* Held by STW initiator until the world should be started (GC
       complete, thruptions delivered). */
    pthread_mutex_t mx_gcing;

    /* Held by a SUB-GC's gc_stop_the_world() when thread in SUB-GC
       holds the GC Lisp-level mutex, but _couldn't_ become STW
       initiator (i.e. another thread is already stopping the
       world). */
    pthread_mutex_t mx_subgc;

    /* First thread (at this round) that decided to stop the world */
    struct thread *th_stw_initiator;

    /* Thread running SUB-GC under the `supervision' of STW
       initiator */
    struct thread *th_subgc;

    /* Stop counter. Nested gc-stop-the-world and gc-start-the-world
       work without thundering herd. */
    int stopped;

    /* Thruption flag: Iff true, current STW initiator is delivering
       thruptions and not GCing. */
    boolean thruption;

} gc_dispatcher = {
    /* mutexes lazy initialized, other data initially zeroed */
    .mx_gpunmapped = PTHREAD_MUTEX_INITIALIZER,
    .mx_gptransition = PTHREAD_MUTEX_INITIALIZER,
    .mx_gcing = PTHREAD_MUTEX_INITIALIZER,
    .mx_subgc = PTHREAD_MUTEX_INITIALIZER,
};


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


/* maybe_become_stw_initiator -- if there is no stop-the-world action
   in progress, begin it by unmapping GC page, and record current
   thread as STW initiator.

   `thruption' flag affects some subtleties of stop/start methods:
   waiting for other threads allowing GC; setting and clearing
   STOP_FOR_GC_PENDING, GC_PENDING, THRUPTION_PENDING, etc.

   Return true if current thread becomes a GC initiator, or already
   _is_ a STW initiator.

   Unlike gc_stop_the_world and gc_start_the_world (that should be
   used in matching pairs), maybe_become_stw_initiator is idempotent
   within a stop-restart cycle. With this call, a thread may `reserve
   the right' to stop the world as early as it wants. */

static inline boolean
maybe_become_stw_initiator(boolean thruption)
{
    struct thread* self = arch_os_get_current_thread();

    /* Double-checked locking. Possible word tearing on some
       architectures, FIXME FIXME, but let's think of it when GENCGC
       and threaded SBCL is ported to them. */
    if (!gc_dispatcher.th_stw_initiator) {
        odxprint(misc,"NULL STW BEFORE GPTRANSITION");
        pthread_mutex_lock(&gc_dispatcher.mx_gptransition);
        /* We hold mx_gptransition. Is there no STW initiator yet? */
        if (!gc_dispatcher.th_stw_initiator) {
            odxprint(misc,"NULL STW IN GPTRANSITION, REPLACING");
            /* Then we are... */
            gc_dispatcher.th_stw_initiator = self;
            gc_dispatcher.thruption = thruption;

            /* hold mx_gcing until we restart the world */
            pthread_mutex_lock(&gc_dispatcher.mx_gcing);

            /* and mx_gpunmapped until we remap common GC page */
            pthread_mutex_lock(&gc_dispatcher.mx_gpunmapped);

            /* we unmap it; other threads running Lisp code will now
               trap. */
            unmap_gc_page();

            /* stop counter; the world is not stopped yet. */
            gc_dispatcher.stopped = 0;
        }
        pthread_mutex_unlock(&gc_dispatcher.mx_gptransition);
    }
    return gc_dispatcher.th_stw_initiator == self;
}


/* maybe_let_the_world_go -- if current thread is a STW initiator,
   unlock internal GC structures, and return true. */
static inline boolean
maybe_let_the_world_go()
{
    struct thread* self = arch_os_get_current_thread();
    if (gc_dispatcher.th_stw_initiator == self) {
        pthread_mutex_lock(&gc_dispatcher.mx_gptransition);
        if (gc_dispatcher.th_stw_initiator == self) {
            gc_dispatcher.th_stw_initiator = NULL;
        }
        pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
        pthread_mutex_unlock(&gc_dispatcher.mx_gptransition);
        return 1;
    } else {
        return 0;
    }
}


/* gc_stop_the_world -- become STW initiator (waiting for other GCs to
   complete if necessary), and make sure all other threads are either
   stopped or gc-safe (i.e. running foreign calls).

   If GC initiator already exists, gc_stop_the_world() either waits
   for its completion, or cooperates with it: e.g. concurrent pending
   thruption handler allows (SUB-GC) to complete under its
   `supervision'.

   Code sections bounded by gc_stop_the_world and gc_start_the_world
   may be nested; inner calls don't stop or start threads,
   decrementing or incrementing the stop counter instead. */
void
gc_stop_the_world()
{
    struct thread* self = arch_os_get_current_thread(), *p;
    boolean thruption;
    if (SymbolTlValue(GC_INHIBIT,self)!=T) {
        /* If GC is enabled, this thread may wait for current STW
           initiator without causing deadlock. */
        if (!maybe_become_stw_initiator(0)) {
            pthread_mutex_lock(&gc_dispatcher.mx_gcing);
            maybe_become_stw_initiator(0);
            pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
        }
        /* Now _this thread_ should be STW initiator */
        gc_assert(self == gc_dispatcher.th_stw_initiator);
    } else {
        /* GC inhibited; e.g. we are inside SUB-GC */
        if (!maybe_become_stw_initiator(0)) {
            /* Some trouble. Inside SUB-GC, holding the Lisp-side
               mutex, but some other thread is stopping the world. */
            if (gc_dispatcher.thruption) {
                /* Thruption. Wait until it's delivered */
                pthread_mutex_lock(&gc_dispatcher.mx_gcing);
                /* Warning: mx_gcing is held recursively. */
                gc_assert(maybe_become_stw_initiator(0));
                pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
            } else {
                /* In SUB-GC, holding mutex; other thread wants to
                   GC. */
                if (gc_dispatcher.th_subgc == self) {
                    /* There is an outer gc_stop_the_world() by _this_
                       thread, running subordinately to initiator.
                       Just increase stop counter. */
                    ++gc_dispatcher.stopped;
                    return;
                }
                /* Register as subordinate collector thread: take
                   mx_subgc */
                pthread_mutex_lock(&gc_dispatcher.mx_subgc);
                ++gc_dispatcher.stopped;

                /* Unlocking thread's own thread_qrl() designates
                   `time to examine me' to other threads. */
                pthread_mutex_unlock(thread_qrl(self));

                /* STW (GC) initiator thread will see our thread needs
                   to finish GC. It will stop the world and itself,
                   and unlock its qrl. */
                pthread_mutex_lock(thread_qrl(gc_dispatcher.th_stw_initiator));
                return;
            }
        }
    }
    thruption = gc_dispatcher.thruption; /* Thruption or GC? */
    if (!gc_dispatcher.stopped++) {
        /* Outermost stop: signal other threads */
        pthread_mutex_lock(&all_threads_lock);
        /* Phase 1: ensure all threads are aware of the need to stop,
           or locked in the foreign code. */
        for_each_thread(p) {
            pthread_mutex_t *p_qrl = thread_qrl(p);
            if (p==self)
                continue;

            /* Read-protect p's flag */
            if (!set_thread_csp_access(p,0)) {
                odxprint(safepoints,"taking qrl %p of %p", p_qrl, p);
                /* Thread is in Lisp, so it should trap (either in
                   Lisp or in Lisp->FFI transition). Trap handler
                   unlocks thread_qrl(p); when it happens, we're safe
                   to examine that thread. */
                pthread_mutex_lock(p_qrl);
                odxprint(safepoints,"taken qrl %p of %p", p_qrl, p);
                /* Mark thread for the future: should we collect, or
                   wait for its final permission? */
                if (SymbolTlValue(GC_INHIBIT,p)!=T) {
                    SetTlSymbolValue(GC_SAFE,T,p);
                } else {
                    SetTlSymbolValue(GC_SAFE,NIL,p);
                }
                pthread_mutex_unlock(p_qrl);
            } else {
                /* In C; we just disabled writing. */
                if (!thruption) {
                    if (SymbolTlValue(GC_INHIBIT,p)==T) {
                        /* GC inhibited there */
                        SetTlSymbolValue(STOP_FOR_GC_PENDING,T,p);
                        /* Enable writing.  Such threads trap by
                           pending thruption when WITHOUT-GCING
                           section ends */
                        set_thread_csp_access(p,1);
                        SetTlSymbolValue(GC_SAFE,NIL,p);
                    } else {
                        /* Thread allows concurrent GC. It runs in C
                           (not a mutator), its in-Lisp flag is
                           read-only (so it traps on return). */
                        SetTlSymbolValue(GC_SAFE,T,p);
                    }
                }
            }
        }
        /* All threads are ready (GC_SAFE == T) or notified (GC_SAFE == NIL). */
        map_gc_page();
        pthread_mutex_unlock(&gc_dispatcher.mx_gpunmapped);
        /* Threads with GC inhibited -- continued */
        odxprint(safepoints,"after remapping GC page %p",self);

        SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,self);
        if (!thruption) {
            struct thread* priority_gc = NULL;
            for_each_thread(p) {
                if (p==self)
                    continue;
                if (SymbolTlValue(GC_SAFE,p)!=T) {
                    /* Wait for thread to `park'. NB it _always_ does
                       it with a pending interrupt trap, so CSP locking is
                       not needed */
                    odxprint(safepoints,"waiting final parking %p (qrl %p)",p, thread_qrl(p));
                    WITH_STATE_SEM(p) {
                        pthread_mutex_lock(thread_qrl(p));
                        if (SymbolTlValue(GC_INHIBIT,p)==T) {
                            /* Concurrent GC invoked manually */
                            gc_assert(!priority_gc); /* Should be at most one at a time */
                            priority_gc = p;
                        }
                        pthread_mutex_unlock(thread_qrl(p));
                    }
                }
                if (!os_get_csp(p))
                    lose("gc_stop_the_world: no SP in parked thread: %p", p);
            }
            if (priority_gc) {
                /* This thread is managing the entire process, so it
                   has to allow manually-invoked GC to complete */
                if (!set_thread_csp_access(self,1)) {
                    /* Create T.O.S. */
                    *self->csp_around_foreign_call = (lispobj)__builtin_frame_address(0);
                    /* Unlock myself */
                    pthread_mutex_unlock(thread_qrl(self));
                    /* Priority GC should take over, holding
                       mx_subgc until it's done. */
                    pthread_mutex_lock(&gc_dispatcher.mx_subgc);
                    /* Lock myself */
                    pthread_mutex_lock(thread_qrl(self));
                    *self->csp_around_foreign_call = 0;
                    SetTlSymbolValue(GC_PENDING,NIL,self);
                    pthread_mutex_unlock(&gc_dispatcher.mx_subgc);
                } else {
                    /* Unlock myself */
                    pthread_mutex_unlock(thread_qrl(self));
                    /* Priority GC should take over, holding
                       mx_subgc until it's done. */
                    pthread_mutex_lock(&gc_dispatcher.mx_subgc);
                    /* Lock myself */
                    pthread_mutex_lock(thread_qrl(self));
                    /* Unlock sub-gc */
                    pthread_mutex_unlock(&gc_dispatcher.mx_subgc);
                }
            }
        }
    }
}


/* gc_start_the_world() -- restart all other threads if the call
   matches the _outermost_ gc_stop_the_world(), or decrement the stop
   counter. */
void
gc_start_the_world()
{
    struct thread* self = arch_os_get_current_thread(), *p;
    boolean thruption = gc_dispatcher.thruption;
    if (gc_dispatcher.th_stw_initiator != self) {
        odxprint(misc,"Unmapper %p self %p",gc_dispatcher.th_stw_initiator,self);
        gc_assert (gc_dispatcher.th_subgc == self);
        if (--gc_dispatcher.stopped == 1) {
            gc_dispatcher.th_subgc = NULL;
            pthread_mutex_unlock(&gc_dispatcher.mx_subgc);
            /* GC initiator may continue now */
            pthread_mutex_unlock(thread_qrl(gc_dispatcher.th_stw_initiator));
        }
        return;
    }

    gc_assert(gc_dispatcher.th_stw_initiator == self);

    if (!--gc_dispatcher.stopped) {
        for_each_thread(p) {
            if (!thruption) {
                SetTlSymbolValue(STOP_FOR_GC_PENDING,NIL,p);
                SetTlSymbolValue(GC_PENDING,NIL,p);
            }
            if (
#ifdef LISP_FEATURE_SB_THRUPTION
                SymbolTlValue(THRUPTION_PENDING,p)!=T
#else
                1 /* trivially no thruption pending */
#endif
                || SymbolTlValue(INTERRUPTS_ENABLED,p)!=T)
                set_thread_csp_access(p,1);
        }
        pthread_mutex_unlock(&all_threads_lock);
        /* Release everyone */
        maybe_let_the_world_go();
    }
}


/* in_race_p() -- return TRUE if no other thread is inside SUB-GC with
   GC-PENDING :IN-PROGRESS. Used to prevent deadlock between manual
   SUB-GC, auto-gc and thruption. */
static inline boolean
in_race_p()
{
    struct thread* self = arch_os_get_current_thread(), *p;
    boolean result = 0;
    pthread_mutex_lock(&all_threads_lock);
    for_each_thread(p) {
        if (p!=self &&
            SymbolTlValue(GC_PENDING,p)!=T &&
            SymbolTlValue(GC_PENDING,p)!=NIL) {
            result = 1;
            break;
        }
    }
    pthread_mutex_unlock(&all_threads_lock);
    if (result) {
        map_gc_page();
        pthread_mutex_unlock(&gc_dispatcher.mx_gpunmapped);
        maybe_let_the_world_go();
    }
    return result;
}

static void
set_csp_from_context(struct thread *self, os_context_t *ctx)
{
    void **sp = (void **) *os_context_register_addr(ctx, reg_SP);
    gc_assert((void **)self->control_stack_start
              <= sp && sp
              < (void **)self->control_stack_end);
    *self->csp_around_foreign_call = (lispobj) sp;
}

void
thread_pitstop(os_context_t *ctxptr)
{
    struct thread* self = arch_os_get_current_thread();
    boolean inhibitor = (SymbolTlValue(GC_INHIBIT,self)==T);

    odxprint(safepoints,"pitstop [%p]", ctxptr);
    if (inhibitor) {
        SetTlSymbolValue(STOP_FOR_GC_PENDING,T,self);
        /* Free qrl to let know we're ready... */
        WITH_STATE_SEM(self) {
            pthread_mutex_unlock(thread_qrl(self));
            pthread_mutex_lock(&gc_dispatcher.mx_gpunmapped);
            pthread_mutex_lock(thread_qrl(self));
            pthread_mutex_unlock(&gc_dispatcher.mx_gpunmapped);
        }
        /* Enable FF-CSP recording (not hurt: will gc at pit-stop, and
           pit-stop always waits for GC end) */
        set_thread_csp_access(self,1);
    } else {
        if (self == gc_dispatcher.th_stw_initiator && gc_dispatcher.stopped) {
            set_thread_csp_access(self,1);
            check_pending_gc(ctxptr);
            return;
        }
        if ((SymbolTlValue(GC_PENDING,self)!=NIL) &&
            maybe_become_stw_initiator(0) && !in_race_p()) {
            gc_stop_the_world();
            set_thread_csp_access(self,1);
            check_pending_gc(ctxptr);
            gc_start_the_world();
        } else {
            /* An innocent thread which is not an initiator _and_ is
               not objecting. */
            odxprint(safepoints,"pitstop yielding [%p]", ctxptr);
            if (!set_thread_csp_access(self,1)) {
                if (os_get_csp(self))
                    lose("thread_pitstop: would lose csp");
                set_csp_from_context(self, ctxptr);
                pthread_mutex_unlock(thread_qrl(self));
                pthread_mutex_lock(&gc_dispatcher.mx_gcing);
                *self->csp_around_foreign_call = 0;
                pthread_mutex_lock(thread_qrl(self));
                pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
            } else {
                pthread_mutex_lock(&gc_dispatcher.mx_gcing);
                set_thread_csp_access(self,1);
                WITH_GC_AT_SAFEPOINTS_ONLY() {
                    pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
#ifdef LISP_FEATURE_SB_THRUPTION
                    while (check_pending_thruptions(ctxptr))
                        ;
#endif
                }
                return;
            }
        }
    }
#ifdef LISP_FEATURE_SB_THRUPTION
    while(check_pending_thruptions(ctxptr));
#endif
}

static inline void
thread_edge(os_context_t *ctxptr)
{
    struct thread *self = arch_os_get_current_thread();
    set_thread_csp_access(self,1);
    if (os_get_csp(self)) {
        if (!self->pc_around_foreign_call)
            return;             /* trivialize */
        odxprint(safepoints,"edge leaving [%p]", ctxptr);
        if (SymbolTlValue(GC_INHIBIT,self)!=T) {
#ifdef LISP_FEATURE_SB_THRUPTION
            if (SymbolTlValue(THRUPTION_PENDING,self)==T &&
                SymbolTlValue(INTERRUPTS_ENABLED,self)==T) {
                pthread_mutex_lock(&gc_dispatcher.mx_gcing);
                set_thread_csp_access(self,1);
                WITH_GC_AT_SAFEPOINTS_ONLY() {
                    pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
                    while (check_pending_thruptions(ctxptr))
                        ;
                }
            } else
#endif
            {
                pthread_mutex_lock(&gc_dispatcher.mx_gcing);
                odxprint(safepoints,"edge leaving [%p] took gcing", ctxptr);
                pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
                odxprint(safepoints,"edge leaving [%p] released gcing", ctxptr);
            }
        }
    } else {
        /* Entering. */
        odxprint(safepoints,"edge entering [%p]", ctxptr);
#ifdef LISP_FEATURE_SB_THRUPTION
        while(check_pending_thruptions(ctxptr))
            ;
#endif
        if (os_get_csp(self))
            lose("thread_edge: would lose csp");
        set_csp_from_context(self, ctxptr);
        if (SymbolTlValue(GC_INHIBIT,self)!=T) {
            pthread_mutex_unlock(thread_qrl(self));
            pthread_mutex_lock(&gc_dispatcher.mx_gcing);
            *self->csp_around_foreign_call = 0;
            pthread_mutex_lock(thread_qrl(self));
            pthread_mutex_unlock(&gc_dispatcher.mx_gcing);
        } else {
            SetTlSymbolValue(STOP_FOR_GC_PENDING,T,self);
            pthread_mutex_unlock(thread_qrl(self));
            pthread_mutex_lock(&gc_dispatcher.mx_gpunmapped);
            *self->csp_around_foreign_call = 0;
            pthread_mutex_lock(thread_qrl(self));
            pthread_mutex_unlock(&gc_dispatcher.mx_gpunmapped);
        }
    }
}


/* thread_register_gc_trigger --

   Called by GENCGC in each thread where GC_PENDING becomes T because
   allocated memory size has crossed the threshold in
   auto_gc_trigger. For the new collective GC sequence, its first call
   marks a process-wide beginning of GC.
*/
void
thread_register_gc_trigger()
{
    odxprint(misc, "/thread_register_gc_trigger");
    struct thread* self = arch_os_get_current_thread();
    /* This function should be called instead of former
       set_pseudo_atomic_interrupted(), e.g. never with true
       GC_INHIBIT */
    gc_assert(SymbolTlValue(GC_INHIBIT,self)!=T);

    /* unmap GC page, signal other threads... */
    maybe_become_stw_initiator(0);
}



#ifdef LISP_FEATURE_SB_THRUPTION
/* wake_thread(thread) -- ensure a thruption delivery to
 * `thread'. */

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

    if (!maybe_become_stw_initiator(1) || in_race_p()) {
        /* we are not able to wake the thread up, but the STW initiator
         * will take care of it (kludge: unless it is in foreign code).
         * Let's at least try to get our return value right. */
        pthread_mutex_lock(&all_threads_lock);
        for_each_thread (thread)
            if (thread->os_thread == os_thread) {
                found = 1;
                break;
            }
        pthread_mutex_unlock(&all_threads_lock);
        goto cleanup;
    }
    gc_stop_the_world();

    /* we hold the all_threads lock */
    for_each_thread (thread)
        if (thread->os_thread == os_thread) {
            /* it's still alive... */
            found = 1;

            SetTlSymbolValue(THRUPTION_PENDING,T,thread);
            if (SymbolTlValue(GC_PENDING,thread) == T
                || SymbolTlValue(STOP_FOR_GC_PENDING,thread) == T)
                break;

            if (os_get_csp(thread)) {
                /* ... and in foreign code.  Push it into a safety
                 * transition. */
                int status = pthread_kill(os_thread, SIGPIPE);
                if (status)
                    lose("wake_thread_posix: pthread_kill failed with %d\n",
                         status);
            }
            break;
        }

    /* If it was alive but in Lisp, the pit stop takes care of thruptions. */
    gc_start_the_world();

cleanup:
    pthread_sigmask(SIG_SETMASK, &oldset, 0);
    return found ? 0 : -1;
}
#endif /* LISP_FEATURE_SB_THRUPTION */

void
thread_in_safety_transition(os_context_t *ctx)
{
    FSHOW_SIGNAL((stderr, "thread_in_safety_transition\n"));
    thread_edge(ctx);
}

void
thread_in_lisp_raised(os_context_t *ctx)
{
    FSHOW_SIGNAL((stderr, "thread_in_lisp_raised\n"));
    thread_pitstop(ctx);
}

void
thread_interrupted(os_context_t *ctx)
{
    FSHOW_SIGNAL((stderr, "thread_interrupted\n"));
    thread_pitstop(ctx);
}

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

    if (!os_get_csp(self))
        /* In Lisp code.  Do not run thruptions asynchronously.  The
         * next safepoint will take care of it. */
        return;

    /* In C code.  As a rule, we assume that running thruptions is OK. */
    fake_foreign_function_call(ctx);
    thread_in_safety_transition(ctx);
    undo_fake_foreign_function_call(ctx);
}
# endif

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

int
handle_safepoint_violation(os_context_t *ctx, os_vm_address_t fault_address)
{
    FSHOW_SIGNAL((stderr, "fault_address = %p, sp = %p, &csp = %p\n",
                  fault_address,
                  GC_SAFEPOINT_PAGE_ADDR,
                  arch_os_get_current_thread()->csp_around_foreign_call));

    struct thread *self = arch_os_get_current_thread();

    if (fault_address == (os_vm_address_t) GC_SAFEPOINT_PAGE_ADDR) {
        /* We're on the altstack and don't want to run Lisp code. */
        arrange_return_to_c_function(ctx, handle_global_safepoint_violation, 0);
        return 1;
    }

    if (fault_address == (os_vm_address_t) self->csp_around_foreign_call) {
        arrange_return_to_c_function(ctx, handle_csp_safepoint_violation, 0);
        return 1;
    }

    /* not a safepoint */
    return 0;
}
#endif /* LISP_FEATURE_WIN32 */

void
callback_wrapper_trampoline(lispobj arg0, lispobj arg1, lispobj arg2)
{
    struct thread* th = arch_os_get_current_thread();
    if (!th)
        lose("callback invoked in non-lisp thread.  Sorry, that is not supported yet.");

    WITH_GC_AT_SAFEPOINTS_ONLY()
        funcall3(SymbolValue(ENTER_ALIEN_CALLBACK, 0), arg0, arg1, arg2);
}

#endif /* LISP_FEATURE_SB_SAFEPOINT -- entire file */
