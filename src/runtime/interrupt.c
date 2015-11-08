/*
 * interrupt-handling magic
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


/* As far as I can tell, what's going on here is:
 *
 * In the case of most signals, when Lisp asks us to handle the
 * signal, the outermost handler (the one actually passed to UNIX) is
 * either interrupt_handle_now(..) or maybe_now_maybe_later(..).
 * In that case, the Lisp-level handler is stored in interrupt_handlers[..]
 * and interrupt_low_level_handlers[..] is cleared.
 *
 * However, some signals need special handling, e.g.
 *
 * o the SIGSEGV (for e.g. Linux) or SIGBUS (for e.g. FreeBSD) used by the
 *   garbage collector to detect violations of write protection,
 *   because some cases of such signals (e.g. GC-related violations of
 *   write protection) are handled at C level and never passed on to
 *   Lisp. For such signals, we still store any Lisp-level handler
 *   in interrupt_handlers[..], but for the outermost handle we use
 *   the value from interrupt_low_level_handlers[..], instead of the
 *   ordinary interrupt_handle_now(..) or interrupt_handle_later(..).
 *
 * o the SIGTRAP (Linux/Alpha) which Lisp code uses to handle breakpoints,
 *   pseudo-atomic sections, and some classes of error (e.g. "function
 *   not defined").  This never goes anywhere near the Lisp handlers at all.
 *   See runtime/alpha-arch.c and code/signal.lisp
 *
 * - WHN 20000728, dan 20010128 */

#include "sbcl.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/wait.h>
#endif
#include <errno.h>

#include "runtime.h"
#include "arch.h"
#include "os.h"
#include "interrupt.h"
#include "globals.h"
#include "lispregs.h"
#include "validate.h"
#include "interr.h"
#include "gc.h"
#include "alloc.h"
#include "dynbind.h"
#include "pseudo-atomic.h"
#include "genesis/fdefn.h"
#include "genesis/simple-fun.h"
#include "genesis/cons.h"

/* When we catch an internal error, should we pass it back to Lisp to
 * be handled in a high-level way? (Early in cold init, the answer is
 * 'no', because Lisp is still too brain-dead to handle anything.
 * After sufficient initialization has been completed, the answer
 * becomes 'yes'.) */
boolean internal_errors_enabled = 0;

#ifndef LISP_FEATURE_WIN32
static
void (*interrupt_low_level_handlers[NSIG]) (int, siginfo_t*, os_context_t*);
#endif
union interrupt_handler interrupt_handlers[NSIG];

/* Under Linux on some architectures, we appear to have to restore the
 * FPU control word from the context, as after the signal is delivered
 * we appear to have a null FPU control word. */
#if defined(RESTORE_FP_CONTROL_FROM_CONTEXT)
#define RESTORE_FP_CONTROL_WORD(context,void_context)           \
    os_context_t *context = arch_os_get_context(&void_context); \
    os_restore_fp_control(context);
#else
#define RESTORE_FP_CONTROL_WORD(context,void_context)           \
    os_context_t *context = arch_os_get_context(&void_context);
#endif

/* Foreign code may want to start some threads on its own.
 * Non-targetted, truly asynchronous signals can be delivered to
 * basically any thread, but invoking Lisp handlers in such foregign
 * threads is really bad, so let's resignal it.
 *
 * This should at least bring attention to the problem, but it cannot
 * work for SIGSEGV and similar. It is good enough for timers, and
 * maybe all deferrables. */

#if defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_WIN32)
static void
add_handled_signals(sigset_t *sigset)
{
    int i;
    for(i = 1; i < NSIG; i++) {
        if (!(ARE_SAME_HANDLER(interrupt_low_level_handlers[i], SIG_DFL)) ||
            !(ARE_SAME_HANDLER(interrupt_handlers[i].c, SIG_DFL))) {
            sigaddset(sigset, i);
        }
    }
}

void block_signals(sigset_t *what, sigset_t *where, sigset_t *old);
#endif

static boolean
maybe_resignal_to_lisp_thread(int signal, os_context_t *context)
{
#if defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_WIN32)
    if (!pthread_getspecific(lisp_thread)) {
        if (!(sigismember(&deferrable_sigset,signal))) {
            corruption_warning_and_maybe_lose
                ("Received signal %d in non-lisp thread %lu, resignalling to a lisp thread.",
                 signal,
                 pthread_self());
        }
        {
            sigset_t sigset;
            sigemptyset(&sigset);
            add_handled_signals(&sigset);
            block_signals(&sigset, 0, 0);
            block_signals(&sigset, os_context_sigmask_addr(context), 0);
            kill(getpid(), signal);
        }
        return 1;
    } else
#endif
        return 0;
}

/* These are to be used in signal handlers. Currently all handlers are
 * called from one of:
 *
 * interrupt_handle_now_handler
 * maybe_now_maybe_later
 * unblock_me_trampoline
 * low_level_handle_now_handler
 * low_level_maybe_now_maybe_later
 * low_level_unblock_me_trampoline
 *
 * This gives us a single point of control (or six) over errno, fp
 * control word, and fixing up signal context on sparc.
 *
 * The SPARC/Linux platform doesn't quite do signals the way we want
 * them done. The third argument in the handler isn't filled in by the
 * kernel properly, so we fix it up ourselves in the
 * arch_os_get_context(..) function. -- CSR, 2002-07-23
 */
#define SAVE_ERRNO(signal,context,void_context)                 \
    {                                                           \
        int _saved_errno = errno;                               \
        RESTORE_FP_CONTROL_WORD(context,void_context);          \
        if (!maybe_resignal_to_lisp_thread(signal, context))    \
        {

#define RESTORE_ERRNO                                           \
        }                                                       \
        errno = _saved_errno;                                   \
    }

static void run_deferred_handler(struct interrupt_data *data,
                                 os_context_t *context);
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
static void store_signal_data_for_later (struct interrupt_data *data,
                                         void *handler, int signal,
                                         siginfo_t *info,
                                         os_context_t *context);


/* Generic signal related utilities. */

void
get_current_sigmask(sigset_t *sigset)
{
    /* Get the current sigmask, by blocking the empty set. */
    thread_sigmask(SIG_BLOCK, 0, sigset);
}

void
block_signals(sigset_t *what, sigset_t *where, sigset_t *old)
{
    if (where) {
        int i;
        if (old)
            sigcopyset(old, where);
        for(i = 1; i < NSIG; i++) {
            if (sigismember(what, i))
                sigaddset(where, i);
        }
    } else {
        thread_sigmask(SIG_BLOCK, what, old);
    }
}

void
unblock_signals(sigset_t *what, sigset_t *where, sigset_t *old)
{
    if (where) {
        int i;
        if (old)
            sigcopyset(old, where);
        for(i = 1; i < NSIG; i++) {
            if (sigismember(what, i))
                sigdelset(where, i);
        }
    } else {
        thread_sigmask(SIG_UNBLOCK, what, old);
    }
}

static void
print_sigset(sigset_t *sigset)
{
  int i;
  for(i = 1; i < NSIG; i++) {
    if (sigismember(sigset, i))
      fprintf(stderr, "Signal %d masked\n", i);
  }
}

/* Return 1 is all signals is sigset2 are masked in sigset, return 0
 * if all re unmasked else die. Passing NULL for sigset is a shorthand
 * for the current sigmask. */
boolean
all_signals_blocked_p(sigset_t *sigset, sigset_t *sigset2,
                                const char *name)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    int i;
    boolean has_blocked = 0, has_unblocked = 0;
    sigset_t current;
    if (sigset == 0) {
        get_current_sigmask(&current);
        sigset = &current;
    }
    for(i = 1; i < NSIG; i++) {
        if (sigismember(sigset2, i)) {
            if (sigismember(sigset, i))
                has_blocked = 1;
            else
                has_unblocked = 1;
        }
    }
    if (has_blocked && has_unblocked) {
        print_sigset(sigset);
        lose("some %s signals blocked, some unblocked\n", name);
    }
    if (has_blocked)
        return 1;
    else
        return 0;
#endif
}


/* Deferrables, blockables, gc signals. */

void
sigaddset_deferrable(sigset_t *s)
{
    sigaddset(s, SIGHUP);
    sigaddset(s, SIGINT);
    sigaddset(s, SIGTERM);
    sigaddset(s, SIGQUIT);
    sigaddset(s, SIGPIPE);
    sigaddset(s, SIGALRM);
    sigaddset(s, SIGURG);
    sigaddset(s, SIGTSTP);
    sigaddset(s, SIGCHLD);
    sigaddset(s, SIGIO);
#ifndef LISP_FEATURE_HPUX
    sigaddset(s, SIGXCPU);
    sigaddset(s, SIGXFSZ);
#endif
    sigaddset(s, SIGVTALRM);
    sigaddset(s, SIGPROF);
    sigaddset(s, SIGWINCH);
}

void
sigaddset_blockable(sigset_t *sigset)
{
    sigaddset_deferrable(sigset);
    sigaddset_gc(sigset);
}

void
sigaddset_gc(sigset_t *sigset)
{
#ifdef THREADS_USING_GCSIGNAL
    sigaddset(sigset,SIG_STOP_FOR_GC);
#endif
}

/* initialized in interrupt_init */
sigset_t deferrable_sigset;
sigset_t blockable_sigset;
sigset_t gc_sigset;

#endif

#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
boolean
deferrables_blocked_p(sigset_t *sigset)
{
    return all_signals_blocked_p(sigset, &deferrable_sigset, "deferrable");
}
#endif

void
check_deferrables_unblocked_or_lose(sigset_t *sigset)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    if (deferrables_blocked_p(sigset))
        lose("deferrables blocked\n");
#endif
}

void
check_deferrables_blocked_or_lose(sigset_t *sigset)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    if (!deferrables_blocked_p(sigset))
        lose("deferrables unblocked\n");
#endif
}

#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
boolean
blockables_blocked_p(sigset_t *sigset)
{
    return all_signals_blocked_p(sigset, &blockable_sigset, "blockable");
}
#endif

void
check_blockables_unblocked_or_lose(sigset_t *sigset)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    if (blockables_blocked_p(sigset))
        lose("blockables blocked\n");
#endif
}

void
check_blockables_blocked_or_lose(sigset_t *sigset)
{
#if !defined(LISP_FEATURE_WIN32)
    /* On Windows, there are no actual signals, but since the win32 port
     * tracks the sigmask and checks it explicitly, some functions are
     * still required to keep the mask set up properly.  (After all, the
     * goal of the sigmask emulation is to not have to change all the
     * call sites in the first place.)
     *
     * However, this does not hold for all signals equally: While
     * deferrables matter ("is interrupt-thread okay?"), it is not worth
     * having to set up blockables properly (which include the
     * non-existing GC signals).
     *
     * Yet, as the original comment explains it:
     *   Adjusting FREE-INTERRUPT-CONTEXT-INDEX* and other aspecs of
     *   fake_foreign_function_call machinery are sometimes useful here[...].
     *
     * So we merely skip this assertion.
     *   -- DFL, trying to expand on a comment by AK.
     */
    if (!blockables_blocked_p(sigset))
        lose("blockables unblocked\n");
#endif
}

#ifndef LISP_FEATURE_SB_SAFEPOINT
#if !defined(LISP_FEATURE_WIN32)
boolean
gc_signals_blocked_p(sigset_t *sigset)
{
    return all_signals_blocked_p(sigset, &gc_sigset, "gc");
}
#endif

void
check_gc_signals_unblocked_or_lose(sigset_t *sigset)
{
#if !defined(LISP_FEATURE_WIN32)
    if (gc_signals_blocked_p(sigset))
        lose("gc signals blocked\n");
#endif
}

void
check_gc_signals_blocked_or_lose(sigset_t *sigset)
{
#if !defined(LISP_FEATURE_WIN32)
    if (!gc_signals_blocked_p(sigset))
        lose("gc signals unblocked\n");
#endif
}
#endif

void
block_deferrable_signals(sigset_t *where, sigset_t *old)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    block_signals(&deferrable_sigset, where, old);
#endif
}

void
block_blockable_signals(sigset_t *where, sigset_t *old)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    block_signals(&blockable_sigset, where, old);
#endif
}

#ifndef LISP_FEATURE_SB_SAFEPOINT
void
block_gc_signals(sigset_t *where, sigset_t *old)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    block_signals(&gc_sigset, where, old);
#endif
}
#endif

void
unblock_deferrable_signals(sigset_t *where, sigset_t *old)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    if (interrupt_handler_pending_p())
        lose("unblock_deferrable_signals: losing proposition\n");
#ifndef LISP_FEATURE_SB_SAFEPOINT
    check_gc_signals_unblocked_or_lose(where);
#endif
    unblock_signals(&deferrable_sigset, where, old);
#endif
}

void
unblock_blockable_signals(sigset_t *where, sigset_t *old)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    unblock_signals(&blockable_sigset, where, old);
#endif
}

#ifndef LISP_FEATURE_SB_SAFEPOINT
void
unblock_gc_signals(sigset_t *where, sigset_t *old)
{
#ifndef LISP_FEATURE_WIN32
    unblock_signals(&gc_sigset, where, old);
#endif
}
#endif

void
unblock_signals_in_context_and_maybe_warn(os_context_t *context)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    sigset_t *sigset = os_context_sigmask_addr(context);
#ifndef LISP_FEATURE_SB_SAFEPOINT
    if (all_signals_blocked_p(sigset, &gc_sigset, "gc")) {
        corruption_warning_and_maybe_lose(
"Enabling blocked gc signals to allow returning to Lisp without risking\n\
gc deadlocks. Since GC signals are only blocked in signal handlers when \n\
they are not safe to interrupt at all, this is a pretty severe occurrence.\n");
        unblock_gc_signals(sigset, 0);
    }
#endif
    if (!interrupt_handler_pending_p()) {
        unblock_deferrable_signals(sigset, 0);
    }
#endif
}


inline static void
check_interrupts_enabled_or_lose(os_context_t *context)
{
    struct thread *thread=arch_os_get_current_thread();
    if (SymbolValue(INTERRUPTS_ENABLED,thread) == NIL)
        lose("interrupts not enabled\n");
    if (arch_pseudo_atomic_atomic(context))
        lose ("in pseudo atomic section\n");
}

/* Save sigset (or the current sigmask if 0) if there is no pending
 * handler, because that means that deferabbles are already blocked.
 * The purpose is to avoid losing the pending gc signal if a
 * deferrable interrupt async unwinds between clearing the pseudo
 * atomic and trapping to GC.*/
#ifndef LISP_FEATURE_SB_SAFEPOINT
void
maybe_save_gc_mask_and_block_deferrables(sigset_t *sigset)
{
#ifndef LISP_FEATURE_WIN32
    struct thread *thread = arch_os_get_current_thread();
    struct interrupt_data *data = thread->interrupt_data;
    sigset_t oldset;
    /* Obviously, this function is called when signals may not be
     * blocked. Let's make sure we are not interrupted. */
    block_blockable_signals(0, &oldset);
#ifndef LISP_FEATURE_SB_THREAD
    /* With threads a SIG_STOP_FOR_GC and a normal GC may also want to
     * block. */
    if (data->gc_blocked_deferrables)
        lose("gc_blocked_deferrables already true\n");
#endif
    if ((!data->pending_handler) &&
        (!data->gc_blocked_deferrables)) {
        FSHOW_SIGNAL((stderr,"/setting gc_blocked_deferrables\n"));
        data->gc_blocked_deferrables = 1;
        if (sigset) {
            /* This is the sigmask of some context. */
            sigcopyset(&data->pending_mask, sigset);
            sigaddset_deferrable(sigset);
            thread_sigmask(SIG_SETMASK,&oldset,0);
            return;
        } else {
            /* Operating on the current sigmask. Save oldset and
             * unblock gc signals. In the end, this is equivalent to
             * blocking the deferrables. */
            sigcopyset(&data->pending_mask, &oldset);
            thread_sigmask(SIG_UNBLOCK, &gc_sigset, 0);
            return;
        }
    }
    thread_sigmask(SIG_SETMASK,&oldset,0);
#endif
}
#endif

/* Are we leaving WITH-GCING and already running with interrupts
 * enabled, without the protection of *GC-INHIBIT* T and there is gc
 * (or stop for gc) pending, but we haven't trapped yet? */
int
in_leaving_without_gcing_race_p(struct thread *thread)
{
    return ((SymbolValue(IN_WITHOUT_GCING,thread) != NIL) &&
            (SymbolValue(INTERRUPTS_ENABLED,thread) != NIL) &&
            (SymbolValue(GC_INHIBIT,thread) == NIL) &&
            ((SymbolValue(GC_PENDING,thread) != NIL)
#if defined(LISP_FEATURE_SB_THREAD)
             || (SymbolValue(STOP_FOR_GC_PENDING,thread) != NIL)
#endif
             ));
}

/* Check our baroque invariants. */
void
check_interrupt_context_or_lose(os_context_t *context)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    struct thread *thread = arch_os_get_current_thread();
    struct interrupt_data *data = thread->interrupt_data;
    int interrupt_deferred_p = (data->pending_handler != 0);
    int interrupt_pending = (SymbolValue(INTERRUPT_PENDING,thread) != NIL);
    sigset_t *sigset = os_context_sigmask_addr(context);
    /* On PPC pseudo_atomic_interrupted is cleared when coming out of
     * handle_allocation_trap. */
#if defined(LISP_FEATURE_GENCGC) && !defined(GENCGC_IS_PRECISE)
    int interrupts_enabled = (SymbolValue(INTERRUPTS_ENABLED,thread) != NIL);
    int gc_inhibit = (SymbolValue(GC_INHIBIT,thread) != NIL);
    int gc_pending = (SymbolValue(GC_PENDING,thread) == T);
    int pseudo_atomic_interrupted = get_pseudo_atomic_interrupted(thread);
    int in_race_p = in_leaving_without_gcing_race_p(thread);
    /* In the time window between leaving the *INTERRUPTS-ENABLED* NIL
     * section and trapping, a SIG_STOP_FOR_GC would see the next
     * check fail, for this reason sig_stop_for_gc handler does not
     * call this function. */
    if (interrupt_deferred_p) {
        if (!(!interrupts_enabled || pseudo_atomic_interrupted || in_race_p))
            lose("Stray deferred interrupt.\n");
    }
    if (gc_pending)
        if (!(pseudo_atomic_interrupted || gc_inhibit || in_race_p))
            lose("GC_PENDING, but why?\n");
#if defined(LISP_FEATURE_SB_THREAD)
    {
        int stop_for_gc_pending =
            (SymbolValue(STOP_FOR_GC_PENDING,thread) != NIL);
        if (stop_for_gc_pending)
            if (!(pseudo_atomic_interrupted || gc_inhibit || in_race_p))
                lose("STOP_FOR_GC_PENDING, but why?\n");
        if (pseudo_atomic_interrupted)
            if (!(gc_pending || stop_for_gc_pending || interrupt_deferred_p))
                lose("pseudo_atomic_interrupted, but why?\n");
    }
#else
    if (pseudo_atomic_interrupted)
        if (!(gc_pending || interrupt_deferred_p))
            lose("pseudo_atomic_interrupted, but why?\n");
#endif
#endif
    if (interrupt_pending && !interrupt_deferred_p)
        lose("INTERRUPT_PENDING but not pending handler.\n");
    if ((data->gc_blocked_deferrables) && interrupt_pending)
        lose("gc_blocked_deferrables and interrupt pending\n.");
    if (data->gc_blocked_deferrables)
        check_deferrables_blocked_or_lose(sigset);
    if (interrupt_pending || interrupt_deferred_p ||
        data->gc_blocked_deferrables)
        check_deferrables_blocked_or_lose(sigset);
    else {
        check_deferrables_unblocked_or_lose(sigset);
#ifndef LISP_FEATURE_SB_SAFEPOINT
        /* If deferrables are unblocked then we are open to signals
         * that run lisp code. */
        check_gc_signals_unblocked_or_lose(sigset);
#endif
    }
#endif
}

/*
 * utility routines used by various signal handlers
 */

static void
build_fake_control_stack_frames(struct thread *th,os_context_t *context)
{
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK

    lispobj oldcont;

    /* Build a fake stack frame or frames */

#if !defined(LISP_FEATURE_ARM) && !defined(LISP_FEATURE_ARM64)
    access_control_frame_pointer(th) =
        (lispobj *)(uword_t)
        (*os_context_register_addr(context, reg_CSP));
    if ((lispobj *)(uword_t)
        (*os_context_register_addr(context, reg_CFP))
        == access_control_frame_pointer(th)) {
        /* There is a small window during call where the callee's
         * frame isn't built yet. */
        if (lowtag_of(*os_context_register_addr(context, reg_CODE))
            == FUN_POINTER_LOWTAG) {
            /* We have called, but not built the new frame, so
             * build it for them. */
            access_control_frame_pointer(th)[0] =
                *os_context_register_addr(context, reg_OCFP);
            access_control_frame_pointer(th)[1] =
                *os_context_register_addr(context, reg_LRA);
            access_control_frame_pointer(th) += 2;
            /* Build our frame on top of it. */
            oldcont = (lispobj)(*os_context_register_addr(context, reg_CFP));
        }
        else {
            /* We haven't yet called, build our frame as if the
             * partial frame wasn't there. */
            oldcont = (lispobj)(*os_context_register_addr(context, reg_OCFP));
        }
    } else
#elif defined (LISP_FEATURE_ARM)
        access_control_frame_pointer(th) =
            SymbolValue(CONTROL_STACK_POINTER, th);
#elif defined (LISP_FEATURE_ARM64)
    access_control_frame_pointer(th) =
        (lispobj *)(uword_t) (*os_context_register_addr(context, reg_CSP));
#endif
    /* We can't tell whether we are still in the caller if it had to
     * allocate a stack frame due to stack arguments. */
    /* This observation provoked some past CMUCL maintainer to ask
     * "Can anything strange happen during return?" */
    {
        /* normal case */
        oldcont = (lispobj)(*os_context_register_addr(context, reg_CFP));
    }

    access_control_stack_pointer(th) = access_control_frame_pointer(th) + 3;

    access_control_frame_pointer(th)[0] = oldcont;
    access_control_frame_pointer(th)[1] = NIL;
    access_control_frame_pointer(th)[2] =
        (lispobj)(*os_context_register_addr(context, reg_CODE));
#endif
}

/* Stores the context for gc to scavange and builds fake stack
 * frames. */
void
fake_foreign_function_call(os_context_t *context)
{
    int context_index;
    struct thread *thread=arch_os_get_current_thread();

    /* context_index incrementing must not be interrupted */
    check_blockables_blocked_or_lose(0);

    /* Get current Lisp state from context. */
#if defined(LISP_FEATURE_ARM) && !defined(LISP_FEATURE_GENCGC)
    dynamic_space_free_pointer = SymbolValue(ALLOCATION_POINTER, thread);
#endif
#ifdef reg_ALLOC
#ifdef LISP_FEATURE_SB_THREAD
    thread->pseudo_atomic_bits =
#else
    dynamic_space_free_pointer =
        (lispobj *)(uword_t)
#endif
            (*os_context_register_addr(context, reg_ALLOC));
/*     fprintf(stderr,"dynamic_space_free_pointer: %p\n", */
/*             dynamic_space_free_pointer); */
#if defined(LISP_FEATURE_ALPHA) || defined(LISP_FEATURE_MIPS)
    if ((sword_t)dynamic_space_free_pointer & 1) {
        lose("dead in fake_foreign_function_call, context = %x\n", context);
    }
#endif
/* why doesnt PPC and SPARC do something like this: */
#if defined(LISP_FEATURE_HPPA)
    if ((sword_t)dynamic_space_free_pointer & 4) {
        lose("dead in fake_foreign_function_call, context = %x, d_s_f_p = %x\n", context, dynamic_space_free_pointer);
    }
#endif
#endif
#ifdef reg_BSP
    set_binding_stack_pointer(thread,
        *os_context_register_addr(context, reg_BSP));
#endif

#if defined(LISP_FEATURE_ARM)
    /* Stash our control stack pointer */
    bind_variable(INTERRUPTED_CONTROL_STACK_POINTER,
                  SymbolValue(CONTROL_STACK_POINTER, thread),
                  thread);
#endif

    build_fake_control_stack_frames(thread,context);

    /* Do dynamic binding of the active interrupt context index
     * and save the context in the context array. */
    context_index =
        fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX,thread));

    if (context_index >= MAX_INTERRUPTS) {
        lose("maximum interrupt nesting depth (%d) exceeded\n", MAX_INTERRUPTS);
    }

    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX,
                  make_fixnum(context_index + 1),thread);

    thread->interrupt_contexts[context_index] = context;

#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
    /* x86oid targets don't maintain the foreign function call flag at
     * all, so leave them to believe that they are never in foreign
     * code. */
    foreign_function_call_active_p(thread) = 1;
#endif
}

/* blocks all blockable signals.  If you are calling from a signal handler,
 * the usual signal mask will be restored from the context when the handler
 * finishes.  Otherwise, be careful */
void
undo_fake_foreign_function_call(os_context_t *context)
{
    struct thread *thread=arch_os_get_current_thread();
    /* Block all blockable signals. */
    block_blockable_signals(0, 0);

    foreign_function_call_active_p(thread) = 0;

    /* Undo dynamic binding of FREE_INTERRUPT_CONTEXT_INDEX */
    unbind(thread);

#if defined(LISP_FEATURE_ARM)
    /* Restore our saved control stack pointer */
    SetSymbolValue(CONTROL_STACK_POINTER,
                   SymbolValue(INTERRUPTED_CONTROL_STACK_POINTER,
                               thread),
                   thread);
    unbind(thread);
#endif

#if defined(reg_ALLOC) && !defined(LISP_FEATURE_SB_THREAD)
    /* Put the dynamic space free pointer back into the context. */
    *os_context_register_addr(context, reg_ALLOC) =
        (uword_t) dynamic_space_free_pointer
        | (*os_context_register_addr(context, reg_ALLOC)
           & LOWTAG_MASK);
    /*
      ((uword_t)(*os_context_register_addr(context, reg_ALLOC))
      & ~LOWTAG_MASK)
      | ((uword_t) dynamic_space_free_pointer & LOWTAG_MASK);
    */
#endif
#if defined(reg_ALLOC) && defined(LISP_FEATURE_SB_THREAD)
    /* Put the pseudo-atomic bits and dynamic space free pointer back
     * into the context (p-a-bits for p-a, and dynamic space free
     * pointer for ROOM). */
    *os_context_register_addr(context, reg_ALLOC) =
        (uword_t) dynamic_space_free_pointer
        | (thread->pseudo_atomic_bits & LOWTAG_MASK);
    /* And clear them so we don't get bit later by call-in/call-out
     * not updating them. */
    thread->pseudo_atomic_bits = 0;
#endif
#if defined(LISP_FEATURE_ARM) && !defined(LISP_FEATURE_GENCGC)
    SetSymbolValue(ALLOCATION_POINTER, dynamic_space_free_pointer, thread);
#endif
}

/* a handler for the signal caused by execution of a trap opcode
 * signalling an internal error */
void
interrupt_internal_error(os_context_t *context, boolean continuable)
{
    DX_ALLOC_SAP(context_sap, context);

    fake_foreign_function_call(context);

    if (!internal_errors_enabled) {
        describe_internal_error(context);
        /* There's no good way to recover from an internal error
         * before the Lisp error handling mechanism is set up. */
        lose("internal error too early in init, can't recover\n");
    }

#ifndef LISP_FEATURE_SB_SAFEPOINT
    unblock_gc_signals(0, 0);
#endif

#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);
#endif

#if defined(LISP_FEATURE_LINUX) && defined(LISP_FEATURE_MIPS)
    /* Workaround for blocked SIGTRAP. */
    {
        sigset_t newset;
        sigemptyset(&newset);
        sigaddset(&newset, SIGTRAP);
        thread_sigmask(SIG_UNBLOCK, &newset, 0);
    }
#endif

    SHOW("in interrupt_internal_error");
#if QSHOW == 2
    /* Display some rudimentary debugging information about the
     * error, so that even if the Lisp error handler gets badly
     * confused, we have a chance to determine what's going on. */
    describe_internal_error(context);
#endif
    funcall2(StaticSymbolFunction(INTERNAL_ERROR), context_sap,
             continuable ? T : NIL);

    undo_fake_foreign_function_call(context); /* blocks signals again */
    if (continuable)
        arch_skip_instruction(context);
}

boolean
interrupt_handler_pending_p(void)
{
    struct thread *thread = arch_os_get_current_thread();
    struct interrupt_data *data = thread->interrupt_data;
    return (data->pending_handler != 0);
}

void
interrupt_handle_pending(os_context_t *context)
{
    /* There are three ways we can get here. First, if an interrupt
     * occurs within pseudo-atomic, it will be deferred, and we'll
     * trap to here at the end of the pseudo-atomic block. Second, if
     * the GC (in alloc()) decides that a GC is required, it will set
     * *GC-PENDING* and pseudo-atomic-interrupted if not *GC-INHIBIT*,
     * and alloc() is always called from within pseudo-atomic, and
     * thus we end up here again. Third, when calling GC-ON or at the
     * end of a WITHOUT-GCING, MAYBE-HANDLE-PENDING-GC will trap to
     * here if there is a pending GC. Fourth, ahem, at the end of
     * WITHOUT-INTERRUPTS (bar complications with nesting).
     *
     * A fourth way happens with safepoints: In addition to a stop for
     * GC that is pending, there are thruptions.  Both mechanisms are
     * mostly signal-free, yet also of an asynchronous nature, so it makes
     * sense to let interrupt_handle_pending take care of running them:
     * It gets run precisely at those places where it is safe to process
     * pending asynchronous tasks. */

    struct thread *thread = arch_os_get_current_thread();
    struct interrupt_data *data = thread->interrupt_data;

    if (arch_pseudo_atomic_atomic(context)) {
        lose("Handling pending interrupt in pseudo atomic.");
    }

    FSHOW_SIGNAL((stderr, "/entering interrupt_handle_pending\n"));

    check_blockables_blocked_or_lose(0);
#ifndef LISP_FEATURE_SB_SAFEPOINT
    /*
     * (On safepoint builds, there is no gc_blocked_deferrables nor
     * SIG_STOP_FOR_GC.)
     */
    /* If GC/SIG_STOP_FOR_GC struck during PA and there was no pending
     * handler, then the pending mask was saved and
     * gc_blocked_deferrables set. Hence, there can be no pending
     * handler and it's safe to restore the pending mask.
     *
     * Note, that if gc_blocked_deferrables is false we may still have
     * to GC. In this case, we are coming out of a WITHOUT-GCING or a
     * pseudo atomic was interrupt be a deferrable first. */
    if (data->gc_blocked_deferrables) {
        if (data->pending_handler)
            lose("GC blocked deferrables but still got a pending handler.");
        if (SymbolValue(GC_INHIBIT,thread)!=NIL)
            lose("GC blocked deferrables while GC is inhibited.");
        /* Restore the saved signal mask from the original signal (the
         * one that interrupted us during the critical section) into
         * the os_context for the signal we're currently in the
         * handler for. This should ensure that when we return from
         * the handler the blocked signals are unblocked. */
#ifndef LISP_FEATURE_WIN32
        sigcopyset(os_context_sigmask_addr(context), &data->pending_mask);
#endif
        data->gc_blocked_deferrables = 0;
    }
#endif

    if (SymbolValue(GC_INHIBIT,thread)==NIL) {
        void *original_pending_handler = data->pending_handler;

#ifdef LISP_FEATURE_SB_SAFEPOINT
        /* handles the STOP_FOR_GC_PENDING case, plus THRUPTIONS */
        if (SymbolValue(STOP_FOR_GC_PENDING,thread) != NIL
# ifdef LISP_FEATURE_SB_THRUPTION
             || (SymbolValue(THRUPTION_PENDING,thread) != NIL
                 && SymbolValue(INTERRUPTS_ENABLED, thread) != NIL)
# endif
            )
            /* We ought to take this chance to do a pitstop now. */
            thread_in_lisp_raised(context);
#elif defined(LISP_FEATURE_SB_THREAD)
        if (SymbolValue(STOP_FOR_GC_PENDING,thread) != NIL) {
            /* STOP_FOR_GC_PENDING and GC_PENDING are cleared by
             * the signal handler if it actually stops us. */
            arch_clear_pseudo_atomic_interrupted(context);
            sig_stop_for_gc_handler(SIG_STOP_FOR_GC,NULL,context);
        } else
#endif
         /* Test for T and not for != NIL since the value :IN-PROGRESS
          * is used in SUB-GC as part of the mechanism to supress
          * recursive gcs.*/
        if (SymbolValue(GC_PENDING,thread) == T) {

            /* Two reasons for doing this. First, if there is a
             * pending handler we don't want to run. Second, we are
             * going to clear pseudo atomic interrupted to avoid
             * spurious trapping on every allocation in SUB_GC and
             * having a pending handler with interrupts enabled and
             * without pseudo atomic interrupted breaks an
             * invariant. */
            if (data->pending_handler) {
                bind_variable(ALLOW_WITH_INTERRUPTS, NIL, thread);
                bind_variable(INTERRUPTS_ENABLED, NIL, thread);
            }

            arch_clear_pseudo_atomic_interrupted(context);

            /* GC_PENDING is cleared in SUB-GC, or if another thread
             * is doing a gc already we will get a SIG_STOP_FOR_GC and
             * that will clear it.
             *
             * If there is a pending handler or gc was triggerred in a
             * signal handler then maybe_gc won't run POST_GC and will
             * return normally. */
            if (!maybe_gc(context))
                lose("GC not inhibited but maybe_gc did not GC.");

            if (data->pending_handler) {
                unbind(thread);
                unbind(thread);
            }
        } else if (SymbolValue(GC_PENDING,thread) != NIL) {
            /* It's not NIL or T so GC_PENDING is :IN-PROGRESS. If
             * GC-PENDING is not NIL then we cannot trap on pseudo
             * atomic due to GC (see if(GC_PENDING) logic in
             * cheneygc.c an gengcgc.c), plus there is a outer
             * WITHOUT-INTERRUPTS SUB_GC, so how did we end up
             * here? */
            lose("Trapping to run pending handler while GC in progress.");
        }

        check_blockables_blocked_or_lose(0);

        /* No GC shall be lost. If SUB_GC triggers another GC then
         * that should be handled on the spot. */
        if (SymbolValue(GC_PENDING,thread) != NIL)
            lose("GC_PENDING after doing gc.");
#ifdef THREADS_USING_GCSIGNAL
        if (SymbolValue(STOP_FOR_GC_PENDING,thread) != NIL)
            lose("STOP_FOR_GC_PENDING after doing gc.");
#endif
        /* Check two things. First, that gc does not clobber a handler
         * that's already pending. Second, that there is no interrupt
         * lossage: if original_pending_handler was NULL then even if
         * an interrupt arrived during GC (POST-GC, really) it was
         * handled. */
        if (original_pending_handler != data->pending_handler)
            lose("pending handler changed in gc: %x -> %x.",
                 original_pending_handler, data->pending_handler);
    }

#ifndef LISP_FEATURE_WIN32
    /* There may be no pending handler, because it was only a gc that
     * had to be executed or because Lisp is a bit too eager to call
     * DO-PENDING-INTERRUPT. */
    if ((SymbolValue(INTERRUPTS_ENABLED,thread) != NIL) &&
        (data->pending_handler))  {
        /* No matter how we ended up here, clear both
         * INTERRUPT_PENDING and pseudo atomic interrupted. It's safe
         * because we checked above that there is no GC pending. */
        SetSymbolValue(INTERRUPT_PENDING, NIL, thread);
        arch_clear_pseudo_atomic_interrupted(context);
        /* Restore the sigmask in the context. */
        sigcopyset(os_context_sigmask_addr(context), &data->pending_mask);
        run_deferred_handler(data, context);
    }
#ifdef LISP_FEATURE_SB_THRUPTION
    if (SymbolValue(THRUPTION_PENDING,thread)==T)
        /* Special case for the following situation: There is a
         * thruption pending, but a signal had been deferred.  The
         * pitstop at the top of this function could only take care
         * of GC, and skipped the thruption, so we need to try again
         * now that INTERRUPT_PENDING and the sigmask have been
         * reset. */
        while (check_pending_thruptions(context))
            ;
#endif
#endif
#ifdef LISP_FEATURE_GENCGC
    if (get_pseudo_atomic_interrupted(thread))
        lose("pseudo_atomic_interrupted after interrupt_handle_pending\n");
#endif
    /* It is possible that the end of this function was reached
     * without never actually doing anything, the tests in Lisp for
     * when to call receive-pending-interrupt are not exact. */
    FSHOW_SIGNAL((stderr, "/exiting interrupt_handle_pending\n"));
}


void
interrupt_handle_now(int signal, siginfo_t *info, os_context_t *context)
{
    boolean were_in_lisp;
    union interrupt_handler handler;

    check_blockables_blocked_or_lose(0);

#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    if (sigismember(&deferrable_sigset,signal))
        check_interrupts_enabled_or_lose(context);
#endif

    handler = interrupt_handlers[signal];

    if (ARE_SAME_HANDLER(handler.c, SIG_IGN)) {
        return;
    }

    were_in_lisp = !foreign_function_call_active_p(arch_os_get_current_thread());
    if (were_in_lisp)
    {
        fake_foreign_function_call(context);
    }

    FSHOW_SIGNAL((stderr,
                  "/entering interrupt_handle_now(%d, info, context)\n",
                  signal));

    if (ARE_SAME_HANDLER(handler.c, SIG_DFL)) {

        /* This can happen if someone tries to ignore or default one
         * of the signals we need for runtime support, and the runtime
         * support decides to pass on it. */
        lose("no handler for signal %d in interrupt_handle_now(..)\n", signal);

    } else if (lowtag_of(handler.lisp) == FUN_POINTER_LOWTAG) {
        /* Once we've decided what to do about contexts in a
         * return-elsewhere world (the original context will no longer
         * be available; should we copy it or was nobody using it anyway?)
         * then we should convert this to return-elsewhere */

        /* CMUCL comment said "Allocate the SAPs while the interrupts
         * are still disabled.".  I (dan, 2003.08.21) assume this is
         * because we're not in pseudoatomic and allocation shouldn't
         * be interrupted.  In which case it's no longer an issue as
         * all our allocation from C now goes through a PA wrapper,
         * but still, doesn't hurt.
         *
         * Yeah, but non-gencgc platforms don't really wrap allocation
         * in PA. MG - 2005-08-29  */


#ifndef LISP_FEATURE_SB_SAFEPOINT
        /* Leave deferrable signals blocked, the handler itself will
         * allow signals again when it sees fit. */
        unblock_gc_signals(0, 0);
#else
        WITH_GC_AT_SAFEPOINTS_ONLY()
#endif
        { // the block is needed for WITH_GC_AT_SAFEPOINTS_ONLY() to work
            DX_ALLOC_SAP(context_sap, context);
            DX_ALLOC_SAP(info_sap, info);

            FSHOW_SIGNAL((stderr,"/calling Lisp-level handler\n"));

            funcall3(handler.lisp,
                     make_fixnum(signal),
                     info_sap,
                     context_sap);
        }
    } else {
        /* This cannot happen in sane circumstances. */

        FSHOW_SIGNAL((stderr,"/calling C-level handler\n"));

#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
        /* Allow signals again. */
        thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);
        (*handler.c)(signal, info, context);
#endif
    }

    if (were_in_lisp)
    {
        undo_fake_foreign_function_call(context); /* block signals again */
    }

    FSHOW_SIGNAL((stderr,
                  "/returning from interrupt_handle_now(%d, info, context)\n",
                  signal));
}

/* This is called at the end of a critical section if the indications
 * are that some signal was deferred during the section.  Note that as
 * far as C or the kernel is concerned we dealt with the signal
 * already; we're just doing the Lisp-level processing now that we
 * put off then */
static void
run_deferred_handler(struct interrupt_data *data, os_context_t *context)
{
    /* The pending_handler may enable interrupts and then another
     * interrupt may hit, overwrite interrupt_data, so reset the
     * pending handler before calling it. Trust the handler to finish
     * with the siginfo before enabling interrupts. */
    void (*pending_handler) (int, siginfo_t*, os_context_t*) =
        data->pending_handler;

    data->pending_handler=0;
    FSHOW_SIGNAL((stderr, "/running deferred handler %p\n", pending_handler));
    (*pending_handler)(data->pending_signal,&(data->pending_info), context);
}

#ifndef LISP_FEATURE_WIN32
boolean
maybe_defer_handler(void *handler, struct interrupt_data *data,
                    int signal, siginfo_t *info, os_context_t *context)
{
    struct thread *thread=arch_os_get_current_thread();

    check_blockables_blocked_or_lose(0);

    if (SymbolValue(INTERRUPT_PENDING,thread) != NIL)
        lose("interrupt already pending\n");
    if (thread->interrupt_data->pending_handler)
        lose("there is a pending handler already (PA)\n");
    if (data->gc_blocked_deferrables)
        lose("maybe_defer_handler: gc_blocked_deferrables true\n");
    check_interrupt_context_or_lose(context);
    /* If interrupts are disabled then INTERRUPT_PENDING is set and
     * not PSEDUO_ATOMIC_INTERRUPTED. This is important for a pseudo
     * atomic section inside a WITHOUT-INTERRUPTS.
     *
     * Also, if in_leaving_without_gcing_race_p then
     * interrupt_handle_pending is going to be called soon, so
     * stashing the signal away is safe.
     */
    if ((SymbolValue(INTERRUPTS_ENABLED,thread) == NIL) ||
        in_leaving_without_gcing_race_p(thread)) {
        FSHOW_SIGNAL((stderr,
                      "/maybe_defer_handler(%x,%d): deferred (RACE=%d)\n",
                      (unsigned int)handler,signal,
                      in_leaving_without_gcing_race_p(thread)));
        store_signal_data_for_later(data,handler,signal,info,context);
        SetSymbolValue(INTERRUPT_PENDING, T,thread);
        check_interrupt_context_or_lose(context);
        return 1;
    }
    /* a slightly confusing test. arch_pseudo_atomic_atomic() doesn't
     * actually use its argument for anything on x86, so this branch
     * may succeed even when context is null (gencgc alloc()) */
    if (arch_pseudo_atomic_atomic(context)) {
        FSHOW_SIGNAL((stderr,
                      "/maybe_defer_handler(%x,%d): deferred(PA)\n",
                      (unsigned int)handler,signal));
        store_signal_data_for_later(data,handler,signal,info,context);
        arch_set_pseudo_atomic_interrupted(context);
        check_interrupt_context_or_lose(context);
        return 1;
    }
    FSHOW_SIGNAL((stderr,
                  "/maybe_defer_handler(%x,%d): not deferred\n",
                  (unsigned int)handler,signal));
    return 0;
}

static void
store_signal_data_for_later (struct interrupt_data *data, void *handler,
                             int signal,
                             siginfo_t *info, os_context_t *context)
{
    if (data->pending_handler)
        lose("tried to overwrite pending interrupt handler %x with %x\n",
             data->pending_handler, handler);
    if (!handler)
        lose("tried to defer null interrupt handler\n");
    data->pending_handler = handler;
    data->pending_signal = signal;
    if(info)
        memcpy(&(data->pending_info), info, sizeof(siginfo_t));

    FSHOW_SIGNAL((stderr, "/store_signal_data_for_later: signal: %d\n",
                  signal));

    if(!context)
        lose("Null context");

    /* the signal mask in the context (from before we were
     * interrupted) is copied to be restored when run_deferred_handler
     * happens. Then the usually-blocked signals are added to the mask
     * in the context so that we are running with blocked signals when
     * the handler returns */
    sigcopyset(&(data->pending_mask),os_context_sigmask_addr(context));
    sigaddset_deferrable(os_context_sigmask_addr(context));
}

static void
maybe_now_maybe_later(int signal, siginfo_t *info, void *void_context)
{
    SAVE_ERRNO(signal,context,void_context);
    struct thread *thread = arch_os_get_current_thread();
    struct interrupt_data *data = thread->interrupt_data;
    if(!maybe_defer_handler(interrupt_handle_now,data,signal,info,context))
        interrupt_handle_now(signal, info, context);
    RESTORE_ERRNO;
}

static void
low_level_interrupt_handle_now(int signal, siginfo_t *info,
                               os_context_t *context)
{
    /* No FP control fixage needed, caller has done that. */
    check_blockables_blocked_or_lose(0);
    check_interrupts_enabled_or_lose(context);
    (*interrupt_low_level_handlers[signal])(signal, info, context);
    /* No Darwin context fixage needed, caller does that. */
}

static void
low_level_maybe_now_maybe_later(int signal, siginfo_t *info, void *void_context)
{
    SAVE_ERRNO(signal,context,void_context);
    struct thread *thread = arch_os_get_current_thread();
    struct interrupt_data *data = thread->interrupt_data;

    if(!maybe_defer_handler(low_level_interrupt_handle_now,data,
                            signal,info,context))
        low_level_interrupt_handle_now(signal, info, context);
    RESTORE_ERRNO;
}
#endif

#ifdef THREADS_USING_GCSIGNAL

/* This function must not cons, because that may trigger a GC. */
void
sig_stop_for_gc_handler(int signal, siginfo_t *info, os_context_t *context)
{
    struct thread *thread=arch_os_get_current_thread();
    boolean was_in_lisp;

    /* Test for GC_INHIBIT _first_, else we'd trap on every single
     * pseudo atomic until gc is finally allowed. */
    if (SymbolValue(GC_INHIBIT,thread) != NIL) {
        FSHOW_SIGNAL((stderr, "sig_stop_for_gc deferred (*GC-INHIBIT*)\n"));
        SetSymbolValue(STOP_FOR_GC_PENDING,T,thread);
        return;
    } else if (arch_pseudo_atomic_atomic(context)) {
        FSHOW_SIGNAL((stderr,"sig_stop_for_gc deferred (PA)\n"));
        SetSymbolValue(STOP_FOR_GC_PENDING,T,thread);
        arch_set_pseudo_atomic_interrupted(context);
        maybe_save_gc_mask_and_block_deferrables
            (os_context_sigmask_addr(context));
        return;
    }

    FSHOW_SIGNAL((stderr, "/sig_stop_for_gc_handler\n"));

    /* Not PA and GC not inhibited -- we can stop now. */

    was_in_lisp = !foreign_function_call_active_p(arch_os_get_current_thread());

    if (was_in_lisp) {
        /* need the context stored so it can have registers scavenged */
        fake_foreign_function_call(context);
    }

    /* Not pending anymore. */
    SetSymbolValue(GC_PENDING,NIL,thread);
    SetSymbolValue(STOP_FOR_GC_PENDING,NIL,thread);

    /* Consider this: in a PA section GC is requested: GC_PENDING,
     * pseudo_atomic_interrupted and gc_blocked_deferrables are set,
     * deferrables are blocked then pseudo_atomic_atomic is cleared,
     * but a SIG_STOP_FOR_GC arrives before trapping to
     * interrupt_handle_pending. Here, GC_PENDING is cleared but
     * pseudo_atomic_interrupted is not and we go on running with
     * pseudo_atomic_interrupted but without a pending interrupt or
     * GC. GC_BLOCKED_DEFERRABLES is also left at 1. So let's tidy it
     * up. */
    if (thread->interrupt_data->gc_blocked_deferrables) {
        FSHOW_SIGNAL((stderr,"cleaning up after gc_blocked_deferrables\n"));
        clear_pseudo_atomic_interrupted(thread);
        sigcopyset(os_context_sigmask_addr(context),
                   &thread->interrupt_data->pending_mask);
        thread->interrupt_data->gc_blocked_deferrables = 0;
    }

    if(thread_state(thread)!=STATE_RUNNING) {
        lose("sig_stop_for_gc_handler: wrong thread state: %ld\n",
             fixnum_value(thread->state));
    }

    set_thread_state(thread,STATE_STOPPED);
    FSHOW_SIGNAL((stderr,"suspended\n"));

    /* While waiting for gc to finish occupy ourselves with zeroing
     * the unused portion of the control stack to reduce conservatism.
     * On hypothetic platforms with threads and exact gc it is
     * actually a must. */
    scrub_control_stack();

    wait_for_thread_state_change(thread, STATE_STOPPED);
    FSHOW_SIGNAL((stderr,"resumed\n"));

    if(thread_state(thread)!=STATE_RUNNING) {
        lose("sig_stop_for_gc_handler: wrong thread state on wakeup: %ld\n",
             fixnum_value(thread_state(thread)));
    }

    if (was_in_lisp) {
        undo_fake_foreign_function_call(context);
    }
}

#endif

void
interrupt_handle_now_handler(int signal, siginfo_t *info, void *void_context)
{
    SAVE_ERRNO(signal,context,void_context);
#ifndef LISP_FEATURE_WIN32
    if ((signal == SIGILL) || (signal == SIGBUS)
#if !(defined(LISP_FEATURE_LINUX) || defined(LISP_FEATURE_ANDROID))
        || (signal == SIGEMT)
#endif
        )
        corruption_warning_and_maybe_lose("Signal %d received (PC: %p)", signal,
                                          *os_context_pc_addr(context));
#endif
    interrupt_handle_now(signal, info, context);
    RESTORE_ERRNO;
}

/* manipulate the signal context and stack such that when the handler
 * returns, it will call function instead of whatever it was doing
 * previously
 */

#if (defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
extern int *context_eflags_addr(os_context_t *context);
#endif

extern lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs);
extern void post_signal_tramp(void);
extern void call_into_lisp_tramp(void);

void
arrange_return_to_c_function(os_context_t *context,
                             call_into_lisp_lookalike funptr,
                             lispobj function)
{
#if !(defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_SAFEPOINT))
    check_gc_signals_unblocked_or_lose
        (os_context_sigmask_addr(context));
#endif
#if !(defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
    void * fun=native_pointer(function);
    void *code = &(((struct simple_fun *) fun)->code);
#endif

    /* Build a stack frame showing `interrupted' so that the
     * user's backtrace makes (as much) sense (as usual) */

    /* fp state is saved and restored by call_into_lisp */
    /* FIXME: errno is not restored, but since current uses of this
     * function only call Lisp code that signals an error, it's not
     * much of a problem. In other words, running out of the control
     * stack between a syscall and (GET-ERRNO) may clobber errno if
     * something fails during signalling or in the handler. But I
     * can't see what can go wrong as long as there is no CONTINUE
     * like restart on them. */
#ifdef LISP_FEATURE_X86
    /* Suppose the existence of some function that saved all
     * registers, called call_into_lisp, then restored GP registers and
     * returned.  It would look something like this:

     push   ebp
     mov    ebp esp
     pushfl
     pushal
     push   $0
     push   $0
     pushl  {address of function to call}
     call   0x8058db0 <call_into_lisp>
     addl   $12,%esp
     popal
     popfl
     leave
     ret

     * What we do here is set up the stack that call_into_lisp would
     * expect to see if it had been called by this code, and frob the
     * signal context so that signal return goes directly to call_into_lisp,
     * and when that function (and the lisp function it invoked) returns,
     * it returns to the second half of this imaginary function which
     * restores all registers and returns to C

     * For this to work, the latter part of the imaginary function
     * must obviously exist in reality.  That would be post_signal_tramp
     */

    u32 *sp=(u32 *)*os_context_register_addr(context,reg_ESP);

#if defined(LISP_FEATURE_DARWIN)
    u32 *register_save_area = (u32 *)os_validate(0, 0x40);

    FSHOW_SIGNAL((stderr, "/arrange_return_to_lisp_function: preparing to go to function %x, sp: %x\n", function, sp));
    FSHOW_SIGNAL((stderr, "/arrange_return_to_lisp_function: context: %x, &context %x\n", context, &context));

    /* 1. os_validate (malloc/mmap) register_save_block
     * 2. copy register state into register_save_block
     * 3. put a pointer to register_save_block in a register in the context
     * 4. set the context's EIP to point to a trampoline which:
     *    a. builds the fake stack frame from the block
     *    b. frees the block
     *    c. calls the function
     */

    *register_save_area = *os_context_pc_addr(context);
    *(register_save_area + 1) = function;
    *(register_save_area + 2) = *os_context_register_addr(context,reg_EDI);
    *(register_save_area + 3) = *os_context_register_addr(context,reg_ESI);
    *(register_save_area + 4) = *os_context_register_addr(context,reg_EDX);
    *(register_save_area + 5) = *os_context_register_addr(context,reg_ECX);
    *(register_save_area + 6) = *os_context_register_addr(context,reg_EBX);
    *(register_save_area + 7) = *os_context_register_addr(context,reg_EAX);
    *(register_save_area + 8) = *context_eflags_addr(context);

    *os_context_pc_addr(context) =
      (os_context_register_t) funptr;
    *os_context_register_addr(context,reg_ECX) =
      (os_context_register_t) register_save_area;
#else

    /* return address for call_into_lisp: */
    *(sp-15) = (u32)post_signal_tramp;
    *(sp-14) = function;        /* args for call_into_lisp : function*/
    *(sp-13) = 0;               /*                           arg array */
    *(sp-12) = 0;               /*                           no. args */
    /* this order matches that used in POPAD */
    *(sp-11)=*os_context_register_addr(context,reg_EDI);
    *(sp-10)=*os_context_register_addr(context,reg_ESI);

    *(sp-9)=*os_context_register_addr(context,reg_ESP)-8;
    /* POPAD ignores the value of ESP:  */
    *(sp-8)=0;
    *(sp-7)=*os_context_register_addr(context,reg_EBX);

    *(sp-6)=*os_context_register_addr(context,reg_EDX);
    *(sp-5)=*os_context_register_addr(context,reg_ECX);
    *(sp-4)=*os_context_register_addr(context,reg_EAX);
    *(sp-3)=*context_eflags_addr(context);
    *(sp-2)=*os_context_register_addr(context,reg_EBP);
    *(sp-1)=*os_context_pc_addr(context);

#endif

#elif defined(LISP_FEATURE_X86_64)
    u64 *sp=(u64 *)*os_context_register_addr(context,reg_RSP);

    /* return address for call_into_lisp: */
    *(sp-18) = (u64)post_signal_tramp;

    *(sp-17)=*os_context_register_addr(context,reg_R15);
    *(sp-16)=*os_context_register_addr(context,reg_R14);
    *(sp-15)=*os_context_register_addr(context,reg_R13);
    *(sp-14)=*os_context_register_addr(context,reg_R12);
    *(sp-13)=*os_context_register_addr(context,reg_R11);
    *(sp-12)=*os_context_register_addr(context,reg_R10);
    *(sp-11)=*os_context_register_addr(context,reg_R9);
    *(sp-10)=*os_context_register_addr(context,reg_R8);
    *(sp-9)=*os_context_register_addr(context,reg_RDI);
    *(sp-8)=*os_context_register_addr(context,reg_RSI);
    /* skip RBP and RSP */
    *(sp-7)=*os_context_register_addr(context,reg_RBX);
    *(sp-6)=*os_context_register_addr(context,reg_RDX);
    *(sp-5)=*os_context_register_addr(context,reg_RCX);
    *(sp-4)=*os_context_register_addr(context,reg_RAX);
    *(sp-3)=*context_eflags_addr(context);
    *(sp-2)=*os_context_register_addr(context,reg_RBP);
    *(sp-1)=*os_context_pc_addr(context);

    *os_context_register_addr(context,reg_RDI) =
        (os_context_register_t)function; /* function */
    *os_context_register_addr(context,reg_RSI) = 0;        /* arg. array */
    *os_context_register_addr(context,reg_RDX) = 0;        /* no. args */
#else
    struct thread *th=arch_os_get_current_thread();
    build_fake_control_stack_frames(th,context);
#endif

#ifdef LISP_FEATURE_X86

#if !defined(LISP_FEATURE_DARWIN)
    *os_context_pc_addr(context) = (os_context_register_t)funptr;
    *os_context_register_addr(context,reg_ECX) = 0;
    *os_context_register_addr(context,reg_EBP) = (os_context_register_t)(sp-2);
#ifdef __NetBSD__
    *os_context_register_addr(context,reg_UESP) =
        (os_context_register_t)(sp-15);
#else
    *os_context_register_addr(context,reg_ESP) = (os_context_register_t)(sp-15);
#endif /* __NETBSD__ */
#endif /* LISP_FEATURE_DARWIN */

#elif defined(LISP_FEATURE_X86_64)
    *os_context_pc_addr(context) = (os_context_register_t)funptr;
    *os_context_register_addr(context,reg_RCX) = 0;
    *os_context_register_addr(context,reg_RBP) = (os_context_register_t)(sp-2);
    *os_context_register_addr(context,reg_RSP) = (os_context_register_t)(sp-18);
#else
    /* this much of the calling convention is common to all
       non-x86 ports */
    *os_context_pc_addr(context) = (os_context_register_t)(unsigned long)code;
    *os_context_register_addr(context,reg_NARGS) = 0;
#ifdef reg_LIP
    *os_context_register_addr(context,reg_LIP) =
        (os_context_register_t)(unsigned long)code;
#endif
    *os_context_register_addr(context,reg_CFP) =
        (os_context_register_t)(unsigned long)access_control_frame_pointer(th);
#endif
#ifdef ARCH_HAS_NPC_REGISTER
    *os_context_npc_addr(context) =
        4 + *os_context_pc_addr(context);
#endif
#if defined(LISP_FEATURE_SPARC) || defined(LISP_FEATURE_ARM) || defined(LISP_FEATURE_ARM64)
    *os_context_register_addr(context,reg_CODE) =
        (os_context_register_t)(fun + FUN_POINTER_LOWTAG);
#endif
    FSHOW((stderr, "/arranged return to Lisp function (0x%lx)\n",
           (long)function));
}

void
arrange_return_to_lisp_function(os_context_t *context, lispobj function)
{
#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_X86)
    arrange_return_to_c_function(context, call_into_lisp_tramp, function);
#else
    arrange_return_to_c_function(context, call_into_lisp, function);
#endif
}

// These have undefined_alien_function tramp in x-assem.S
#if !(defined(LISP_FEATURE_X86_64) || defined(LISP_FEATURE_ARM) || defined(LISP_FEATURE_ARM64))
/* KLUDGE: Theoretically the approach we use for undefined alien
 * variables should work for functions as well, but on PPC/Darwin
 * we get bus error at bogus addresses instead, hence this workaround,
 * that has the added benefit of automatically discriminating between
 * functions and variables.
 */
void
undefined_alien_function(void)
{
    funcall0(StaticSymbolFunction(UNDEFINED_ALIEN_FUN_ERROR));
}
#endif

void lower_thread_control_stack_guard_page(struct thread *th)
{
    protect_control_stack_guard_page(0, th);
    protect_control_stack_return_guard_page(1, th);
    th->control_stack_guard_page_protected = NIL;
    fprintf(stderr, "INFO: Control stack guard page unprotected\n");
}

void reset_thread_control_stack_guard_page(struct thread *th)
{
    memset(CONTROL_STACK_GUARD_PAGE(th), 0, os_vm_page_size);
    protect_control_stack_guard_page(1, th);
    protect_control_stack_return_guard_page(0, th);
    th->control_stack_guard_page_protected = T;
    fprintf(stderr, "INFO: Control stack guard page reprotected\n");
}

/* Called from the REPL, too. */
void reset_control_stack_guard_page(void)
{
    struct thread *th=arch_os_get_current_thread();
    if (th->control_stack_guard_page_protected == NIL) {
        reset_thread_control_stack_guard_page(th);
    }
}

void lower_control_stack_guard_page(void)
{
    lower_thread_control_stack_guard_page(arch_os_get_current_thread());
}

boolean
handle_guard_page_triggered(os_context_t *context,os_vm_address_t addr)
{
    struct thread *th=arch_os_get_current_thread();

    if(addr >= CONTROL_STACK_HARD_GUARD_PAGE(th) &&
       addr < CONTROL_STACK_HARD_GUARD_PAGE(th) + os_vm_page_size) {
        lose("Control stack exhausted, fault: %p, PC: %p",
             addr, *os_context_pc_addr(context));
    }
    else if(addr >= CONTROL_STACK_GUARD_PAGE(th) &&
            addr < CONTROL_STACK_GUARD_PAGE(th) + os_vm_page_size) {
        /* We hit the end of the control stack: disable guard page
         * protection so the error handler has some headroom, protect the
         * previous page so that we can catch returns from the guard page
         * and restore it. */
        if (th->control_stack_guard_page_protected == NIL)
            lose("control_stack_guard_page_protected NIL");
        lower_control_stack_guard_page();
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        /* For the unfortunate case, when the control stack is
         * exhausted in a signal handler. */
        unblock_signals_in_context_and_maybe_warn(context);
#endif
        arrange_return_to_lisp_function
            (context, StaticSymbolFunction(CONTROL_STACK_EXHAUSTED_ERROR));
        return 1;
    }
    else if(addr >= CONTROL_STACK_RETURN_GUARD_PAGE(th) &&
            addr < CONTROL_STACK_RETURN_GUARD_PAGE(th) + os_vm_page_size) {
        /* We're returning from the guard page: reprotect it, and
         * unprotect this one. This works even if we somehow missed
         * the return-guard-page, and hit it on our way to new
         * exhaustion instead. */
        if (th->control_stack_guard_page_protected != NIL)
            lose("control_stack_guard_page_protected not NIL");
        reset_control_stack_guard_page();
        return 1;
    }
    else if(addr >= BINDING_STACK_HARD_GUARD_PAGE(th) &&
            addr < BINDING_STACK_HARD_GUARD_PAGE(th) + os_vm_page_size) {
        lose("Binding stack exhausted");
    }
    else if(addr >= BINDING_STACK_GUARD_PAGE(th) &&
            addr < BINDING_STACK_GUARD_PAGE(th) + os_vm_page_size) {
        protect_binding_stack_guard_page(0, NULL);
        protect_binding_stack_return_guard_page(1, NULL);
        fprintf(stderr, "INFO: Binding stack guard page unprotected\n");

        /* For the unfortunate case, when the binding stack is
         * exhausted in a signal handler. */
        unblock_signals_in_context_and_maybe_warn(context);
        arrange_return_to_lisp_function
            (context, StaticSymbolFunction(BINDING_STACK_EXHAUSTED_ERROR));
        return 1;
    }
    else if(addr >= BINDING_STACK_RETURN_GUARD_PAGE(th) &&
            addr < BINDING_STACK_RETURN_GUARD_PAGE(th) + os_vm_page_size) {
        protect_binding_stack_guard_page(1, NULL);
        protect_binding_stack_return_guard_page(0, NULL);
        fprintf(stderr, "INFO: Binding stack guard page reprotected\n");
        return 1;
    }
    else if(addr >= ALIEN_STACK_HARD_GUARD_PAGE(th) &&
            addr < ALIEN_STACK_HARD_GUARD_PAGE(th) + os_vm_page_size) {
        lose("Alien stack exhausted");
    }
    else if(addr >= ALIEN_STACK_GUARD_PAGE(th) &&
            addr < ALIEN_STACK_GUARD_PAGE(th) + os_vm_page_size) {
        protect_alien_stack_guard_page(0, NULL);
        protect_alien_stack_return_guard_page(1, NULL);
        fprintf(stderr, "INFO: Alien stack guard page unprotected\n");

        /* For the unfortunate case, when the alien stack is
         * exhausted in a signal handler. */
        unblock_signals_in_context_and_maybe_warn(context);
        arrange_return_to_lisp_function
            (context, StaticSymbolFunction(ALIEN_STACK_EXHAUSTED_ERROR));
        return 1;
    }
    else if(addr >= ALIEN_STACK_RETURN_GUARD_PAGE(th) &&
            addr < ALIEN_STACK_RETURN_GUARD_PAGE(th) + os_vm_page_size) {
        protect_alien_stack_guard_page(1, NULL);
        protect_alien_stack_return_guard_page(0, NULL);
        fprintf(stderr, "INFO: Alien stack guard page reprotected\n");
        return 1;
    }
    else if (addr >= undefined_alien_address &&
             addr < undefined_alien_address + os_vm_page_size) {
        arrange_return_to_lisp_function
            (context, StaticSymbolFunction(UNDEFINED_ALIEN_VARIABLE_ERROR));
        return 1;
    }
    else return 0;
}

/*
 * noise to install handlers
 */

#ifndef LISP_FEATURE_WIN32
/* In Linux 2.4 synchronous signals (sigtrap & co) can be delivered if
 * they are blocked, in Linux 2.6 the default handler is invoked
 * instead that usually coredumps. One might hastily think that adding
 * SA_NODEFER helps, but until ~2.6.13 if SA_NODEFER is specified then
 * the whole sa_mask is ignored and instead of not adding the signal
 * in question to the mask. That means if it's not blockable the
 * signal must be unblocked at the beginning of signal handlers.
 *
 * It turns out that NetBSD's SA_NODEFER doesn't DTRT in a different
 * way: if SA_NODEFER is set and the signal is in sa_mask, the signal
 * will be unblocked in the sigmask during the signal handler.  -- RMK
 * X-mas day, 2005
 */
static volatile int sigaction_nodefer_works = -1;

#define SA_NODEFER_TEST_BLOCK_SIGNAL SIGABRT
#define SA_NODEFER_TEST_KILL_SIGNAL SIGUSR1

static void
sigaction_nodefer_test_handler(int signal, siginfo_t *info, void *void_context)
{
    sigset_t current;
    int i;
    get_current_sigmask(&current);
    /* There should be exactly two blocked signals: the two we added
     * to sa_mask when setting up the handler.  NetBSD doesn't block
     * the signal we're handling when SA_NODEFER is set; Linux before
     * 2.6.13 or so also doesn't block the other signal when
     * SA_NODEFER is set. */
    for(i = 1; i < NSIG; i++)
        if (sigismember(&current, i) !=
            (((i == SA_NODEFER_TEST_BLOCK_SIGNAL) || (i == signal)) ? 1 : 0)) {
            FSHOW_SIGNAL((stderr, "SA_NODEFER doesn't work, signal %d\n", i));
            sigaction_nodefer_works = 0;
        }
    if (sigaction_nodefer_works == -1)
        sigaction_nodefer_works = 1;
}

static void
see_if_sigaction_nodefer_works(void)
{
    struct sigaction sa, old_sa;

    sa.sa_flags = SA_SIGINFO | SA_NODEFER;
    sa.sa_sigaction = sigaction_nodefer_test_handler;
    sigemptyset(&sa.sa_mask);
    sigaddset(&sa.sa_mask, SA_NODEFER_TEST_BLOCK_SIGNAL);
    sigaddset(&sa.sa_mask, SA_NODEFER_TEST_KILL_SIGNAL);
    sigaction(SA_NODEFER_TEST_KILL_SIGNAL, &sa, &old_sa);
    /* Make sure no signals are blocked. */
    {
        sigset_t empty;
        sigemptyset(&empty);
        thread_sigmask(SIG_SETMASK, &empty, 0);
    }
    kill(getpid(), SA_NODEFER_TEST_KILL_SIGNAL);
    while (sigaction_nodefer_works == -1);
    sigaction(SA_NODEFER_TEST_KILL_SIGNAL, &old_sa, NULL);
}

#undef SA_NODEFER_TEST_BLOCK_SIGNAL
#undef SA_NODEFER_TEST_KILL_SIGNAL

#if defined(LISP_FEATURE_SB_SAFEPOINT_STRICTLY) && !defined(LISP_FEATURE_WIN32)

static void *
signal_thread_trampoline(void *pthread_arg)
{
    int signo = (int) pthread_arg;
    os_context_t fake_context;
    siginfo_t fake_info;
#ifdef LISP_FEATURE_PPC
    mcontext_t uc_regs;
#endif

    memset(&fake_info, 0, sizeof(fake_info));
    memset(&fake_context, 0, sizeof(fake_context));
#ifdef LISP_FEATURE_PPC
    memset(&uc_regs, 0, sizeof(uc_regs));
    fake_context.uc_mcontext.uc_regs = &uc_regs;
#endif

    *os_context_pc_addr(&fake_context) = &signal_thread_trampoline;
#ifdef ARCH_HAS_STACK_POINTER /* aka x86(-64) */
    *os_context_sp_addr(&fake_context) = __builtin_frame_address(0);
#endif

    signal_handler_callback(interrupt_handlers[signo].lisp,
                            signo, &fake_info, &fake_context);
    return 0;
}

static void
sigprof_handler_trampoline(int signal, siginfo_t *info, void *void_context)
{
    SAVE_ERRNO(signal,context,void_context);
    struct thread *self = arch_os_get_current_thread();

    /* alloc() is not re-entrant and still uses pseudo atomic (even though
     * inline allocation does not).  In this case, give up. */
    if (get_pseudo_atomic_atomic(self))
        goto cleanup;

    struct alloc_region tmp = self->alloc_region;
    self->alloc_region = self->sprof_alloc_region;
    self->sprof_alloc_region = tmp;

    interrupt_handle_now_handler(signal, info, void_context);

    /* And we're back.  We know that the SIGPROF handler never unwinds
     * non-locally, and can simply swap things back: */

    tmp = self->alloc_region;
    self->alloc_region = self->sprof_alloc_region;
    self->sprof_alloc_region = tmp;

cleanup:
    ; /* Dear C compiler, it's OK to have a label here. */
    RESTORE_ERRNO;
}

static void
spawn_signal_thread_handler(int signal, siginfo_t *info, void *void_context)
{
    SAVE_ERRNO(signal,context,void_context);

    pthread_attr_t attr;
    pthread_t th;

    if (pthread_attr_init(&attr))
        goto lost;
    if (pthread_attr_setstacksize(&attr, thread_control_stack_size))
        goto lost;
    if (pthread_create(&th, &attr, &signal_thread_trampoline, (void*) signal))
        goto lost;
    if (pthread_attr_destroy(&attr))
        goto lost;

    RESTORE_ERRNO;
    return;

lost:
    lose("spawn_signal_thread_handler");
}
#endif

static void
unblock_me_trampoline(int signal, siginfo_t *info, void *void_context)
{
    SAVE_ERRNO(signal,context,void_context);
    sigset_t unblock;

    sigemptyset(&unblock);
    sigaddset(&unblock, signal);
    thread_sigmask(SIG_UNBLOCK, &unblock, 0);
    interrupt_handle_now(signal, info, context);
    RESTORE_ERRNO;
}

static void
low_level_unblock_me_trampoline(int signal, siginfo_t *info, void *void_context)
{
    SAVE_ERRNO(signal,context,void_context);
    sigset_t unblock;

    sigemptyset(&unblock);
    sigaddset(&unblock, signal);
    thread_sigmask(SIG_UNBLOCK, &unblock, 0);
    (*interrupt_low_level_handlers[signal])(signal, info, context);
    RESTORE_ERRNO;
}

static void
low_level_handle_now_handler(int signal, siginfo_t *info, void *void_context)
{
    SAVE_ERRNO(signal,context,void_context);
    (*interrupt_low_level_handlers[signal])(signal, info, context);
    RESTORE_ERRNO;
}

void
undoably_install_low_level_interrupt_handler (int signal,
                                              interrupt_handler_t handler)
{
    struct sigaction sa;

    if (0 > signal || signal >= NSIG) {
        lose("bad signal number %d\n", signal);
    }

    if (ARE_SAME_HANDLER(handler, SIG_DFL))
        sa.sa_sigaction = (void (*)(int, siginfo_t*, void*))handler;
    else if (sigismember(&deferrable_sigset,signal))
        sa.sa_sigaction = low_level_maybe_now_maybe_later;
    else if (!sigaction_nodefer_works &&
             !sigismember(&blockable_sigset, signal))
        sa.sa_sigaction = low_level_unblock_me_trampoline;
    else
        sa.sa_sigaction = low_level_handle_now_handler;

#ifdef LISP_FEATURE_SB_THRUPTION
    /* It's in `deferrable_sigset' so that we block&unblock it properly,
     * but we don't actually want to defer it.  And if we put it only
     * into blockable_sigset, we'd have to special-case it around thread
     * creation at least. */
    if (signal == SIGPIPE)
        sa.sa_sigaction = low_level_handle_now_handler;
#endif

    sigcopyset(&sa.sa_mask, &blockable_sigset);
    sa.sa_flags = SA_SIGINFO | SA_RESTART
        | (sigaction_nodefer_works ? SA_NODEFER : 0);
#if defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
    if(signal==SIG_MEMORY_FAULT) {
        sa.sa_flags |= SA_ONSTACK;
# ifdef LISP_FEATURE_SB_SAFEPOINT
        sigaddset(&sa.sa_mask, SIGRTMIN);
        sigaddset(&sa.sa_mask, SIGRTMIN+1);
# endif
    }
#endif

    sigaction(signal, &sa, NULL);
    interrupt_low_level_handlers[signal] =
        (ARE_SAME_HANDLER(handler, SIG_DFL) ? 0 : handler);
}
#endif

/* This is called from Lisp. */
uword_t
install_handler(int signal, void handler(int, siginfo_t*, os_context_t*),
                int synchronous)
{
#ifndef LISP_FEATURE_WIN32
    struct sigaction sa;
    sigset_t old;
    union interrupt_handler oldhandler;

    FSHOW((stderr, "/entering POSIX install_handler(%d, ..)\n", signal));

    block_blockable_signals(0, &old);

    FSHOW((stderr, "/interrupt_low_level_handlers[signal]=%p\n",
           interrupt_low_level_handlers[signal]));
    if (interrupt_low_level_handlers[signal]==0) {
        if (ARE_SAME_HANDLER(handler, SIG_DFL) ||
            ARE_SAME_HANDLER(handler, SIG_IGN))
            sa.sa_sigaction = (void (*)(int, siginfo_t*, void*))handler;
#ifdef LISP_FEATURE_SB_SAFEPOINT_STRICTLY
        else if (signal == SIGPROF)
            sa.sa_sigaction = sigprof_handler_trampoline;
        else if (!synchronous)
            sa.sa_sigaction = spawn_signal_thread_handler;
#endif
        else if (sigismember(&deferrable_sigset, signal))
            sa.sa_sigaction = maybe_now_maybe_later;
        else if (!sigaction_nodefer_works &&
                 !sigismember(&blockable_sigset, signal))
            sa.sa_sigaction = unblock_me_trampoline;
        else
            sa.sa_sigaction = interrupt_handle_now_handler;

        sigcopyset(&sa.sa_mask, &blockable_sigset);
        sa.sa_flags = SA_SIGINFO | SA_RESTART |
            (sigaction_nodefer_works ? SA_NODEFER : 0);
        sigaction(signal, &sa, NULL);
    }

    oldhandler = interrupt_handlers[signal];
    interrupt_handlers[signal].c = handler;

    thread_sigmask(SIG_SETMASK, &old, 0);

    FSHOW((stderr, "/leaving POSIX install_handler(%d, ..)\n", signal));

    return (uword_t)oldhandler.lisp;
#else
    /* Probably-wrong Win32 hack */
    return 0;
#endif
}

/* This must not go through lisp as it's allowed anytime, even when on
 * the altstack. */
void
sigabrt_handler(int signal, siginfo_t *info, os_context_t *context)
{
    lose("SIGABRT received.\n");
}

void
interrupt_init(void)
{
#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    int i;
    SHOW("entering interrupt_init()");
#ifndef LISP_FEATURE_WIN32
    see_if_sigaction_nodefer_works();
#endif
    sigemptyset(&deferrable_sigset);
    sigemptyset(&blockable_sigset);
    sigemptyset(&gc_sigset);
    sigaddset_deferrable(&deferrable_sigset);
    sigaddset_blockable(&blockable_sigset);
    sigaddset_gc(&gc_sigset);
#endif

#ifndef LISP_FEATURE_WIN32
    /* Set up high level handler information. */
    for (i = 0; i < NSIG; i++) {
        interrupt_handlers[i].c =
            /* (The cast here blasts away the distinction between
             * SA_SIGACTION-style three-argument handlers and
             * signal(..)-style one-argument handlers, which is OK
             * because it works to call the 1-argument form where the
             * 3-argument form is expected.) */
            (void (*)(int, siginfo_t*, os_context_t*))SIG_DFL;
    }
    undoably_install_low_level_interrupt_handler(SIGABRT, sigabrt_handler);
#endif
    SHOW("returning from interrupt_init()");
}

#ifndef LISP_FEATURE_WIN32
int
siginfo_code(siginfo_t *info)
{
    return info->si_code;
}
os_vm_address_t current_memory_fault_address;

void
lisp_memory_fault_error(os_context_t *context, os_vm_address_t addr)
{
   /* FIXME: This is lossy: if we get another memory fault (eg. from
    * another thread) before lisp has read this, we lose the information.
    * However, since this is mostly informative, we'll live with that for
    * now -- some address is better then no address in this case.
    */
    current_memory_fault_address = addr;
    /* To allow debugging memory faults in signal handlers and such. */
    corruption_warning_and_maybe_lose("Memory fault at %p (pc=%p, sp=%p)",
                                      addr,
                                      *os_context_pc_addr(context),
#ifdef ARCH_HAS_STACK_POINTER
                                      *os_context_sp_addr(context)
#else
                                      0
#endif
                                      );
    unblock_signals_in_context_and_maybe_warn(context);
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    arrange_return_to_lisp_function(context,
                                    StaticSymbolFunction(MEMORY_FAULT_ERROR));
#else
    funcall0(StaticSymbolFunction(MEMORY_FAULT_ERROR));
#endif
}
#endif

static void
unhandled_trap_error(os_context_t *context)
{
    DX_ALLOC_SAP(context_sap, context);
    fake_foreign_function_call(context);
#ifndef LISP_FEATURE_SB_SAFEPOINT
    unblock_gc_signals(0, 0);
#endif

#if !defined(LISP_FEATURE_WIN32) || defined(LISP_FEATURE_SB_THREAD)
    thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);
#endif
    funcall1(StaticSymbolFunction(UNHANDLED_TRAP_ERROR), context_sap);
    lose("UNHANDLED-TRAP-ERROR fell through");
}

/* Common logic for trapping instructions. How we actually handle each
 * case is highly architecture dependent, but the overall shape is
 * this. */
void
handle_trap(os_context_t *context, int trap)
{
    switch(trap) {
#if !(defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD))
    case trap_PendingInterrupt:
        FSHOW((stderr, "/<trap pending interrupt>\n"));
        arch_skip_instruction(context);
        interrupt_handle_pending(context);
        break;
#endif
    case trap_Error:
    case trap_Cerror:
#ifdef trap_InvalidArgCount
    case trap_InvalidArgCount:
#endif
        FSHOW((stderr, "/<trap error/cerror %d>\n", trap));
        interrupt_internal_error(context, trap==trap_Cerror);
        break;
    case trap_Breakpoint:
        arch_handle_breakpoint(context);
        break;
    case trap_FunEndBreakpoint:
        arch_handle_fun_end_breakpoint(context);
        break;
#ifdef trap_AfterBreakpoint
    case trap_AfterBreakpoint:
        arch_handle_after_breakpoint(context);
        break;
#endif
#ifdef trap_SingleStepAround
    case trap_SingleStepAround:
    case trap_SingleStepBefore:
        arch_handle_single_step_trap(context, trap);
        break;
#endif
#ifdef trap_GlobalSafepoint
    case trap_GlobalSafepoint:
        fake_foreign_function_call(context);
        thread_in_lisp_raised(context);
        undo_fake_foreign_function_call(context);
        arch_skip_instruction(context);
        break;
    case trap_CspSafepoint:
        fake_foreign_function_call(context);
        thread_in_safety_transition(context);
        undo_fake_foreign_function_call(context);
        arch_skip_instruction(context);
        break;
#endif
#if defined(LISP_FEATURE_SPARC) && defined(LISP_FEATURE_GENCGC)
    case trap_Allocation:
        arch_handle_allocation_trap(context);
        arch_skip_instruction(context);
        break;
#endif
    case trap_Halt:
        fake_foreign_function_call(context);
        lose("%%PRIMITIVE HALT called; the party is over.\n");
    default:
        unhandled_trap_error(context);
    }
}
