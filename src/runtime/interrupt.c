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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>

#include "sbcl.h"
#include "runtime.h"
#include "arch.h"
#include "os.h"
#include "interrupt.h"
#include "globals.h"
#include "lispregs.h"
#include "validate.h"
#include "monitor.h"
#include "gc.h"
#include "alloc.h"
#include "dynbind.h"
#include "interr.h"
#include "genesis/fdefn.h"
#include "genesis/simple-fun.h"
#include "genesis/cons.h"



static void run_deferred_handler(struct interrupt_data *data, void *v_context);
static void store_signal_data_for_later (struct interrupt_data *data,
                                         void *handler, int signal,
                                         siginfo_t *info,
                                         os_context_t *context);
boolean interrupt_maybe_gc_int(int signal, siginfo_t *info, void *v_context);

void
sigaddset_deferrable(sigset_t *s)
{
    sigaddset(s, SIGHUP);
    sigaddset(s, SIGINT);
    sigaddset(s, SIGQUIT);
    sigaddset(s, SIGPIPE);
    sigaddset(s, SIGALRM);
    sigaddset(s, SIGURG);
    sigaddset(s, SIGTSTP);
    sigaddset(s, SIGCHLD);
    sigaddset(s, SIGIO);
    sigaddset(s, SIGXCPU);
    sigaddset(s, SIGXFSZ);
    sigaddset(s, SIGVTALRM);
    sigaddset(s, SIGPROF);
    sigaddset(s, SIGWINCH);
    sigaddset(s, SIGUSR1);
    sigaddset(s, SIGUSR2);
#ifdef LISP_FEATURE_SB_THREAD
    sigaddset(s, SIG_INTERRUPT_THREAD);
#endif
}

void
sigaddset_blockable(sigset_t *s)
{
    sigaddset_deferrable(s);
#ifdef LISP_FEATURE_SB_THREAD
    sigaddset(s, SIG_STOP_FOR_GC);
#endif
}

/* initialized in interrupt_init */
static sigset_t deferrable_sigset;
static sigset_t blockable_sigset;

void
check_blockables_blocked_or_lose()
{
    /* Get the current sigmask, by blocking the empty set. */
    sigset_t empty,current;
    int i;
    sigemptyset(&empty);
    thread_sigmask(SIG_BLOCK, &empty, &current);
    for(i = 1; i < NSIG; i++) {
        if (sigismember(&blockable_sigset, i) && !sigismember(&current, i))
            lose("blockable signal %d not blocked",i);
    }
}

inline static void
check_interrupts_enabled_or_lose(os_context_t *context)
{
    struct thread *thread=arch_os_get_current_thread();
    if (SymbolValue(INTERRUPTS_ENABLED,thread) == NIL)
        lose("interrupts not enabled");
    if (
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
        (!foreign_function_call_active) &&
#endif
        arch_pseudo_atomic_atomic(context))
        lose ("in pseudo atomic section");
}

/* When we catch an internal error, should we pass it back to Lisp to
 * be handled in a high-level way? (Early in cold init, the answer is
 * 'no', because Lisp is still too brain-dead to handle anything.
 * After sufficient initialization has been completed, the answer
 * becomes 'yes'.) */
boolean internal_errors_enabled = 0;

static void (*interrupt_low_level_handlers[NSIG]) (int, siginfo_t*, void*);
union interrupt_handler interrupt_handlers[NSIG];

/* At the toplevel repl we routinely call this function.  The signal
 * mask ought to be clear anyway most of the time, but may be non-zero
 * if we were interrupted e.g. while waiting for a queue.  */

void
reset_signal_mask(void)
{
    sigset_t new;
    sigemptyset(&new);
    thread_sigmask(SIG_SETMASK,&new,0);
}

void
block_blockable_signals(void)
{
    thread_sigmask(SIG_BLOCK, &blockable_sigset, 0);
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

    current_control_frame_pointer =
        (lispobj *)(unsigned long)
            (*os_context_register_addr(context, reg_CSP));
    if ((lispobj *)(unsigned long)
            (*os_context_register_addr(context, reg_CFP))
        == current_control_frame_pointer) {
        /* There is a small window during call where the callee's
         * frame isn't built yet. */
        if (lowtag_of(*os_context_register_addr(context, reg_CODE))
            == FUN_POINTER_LOWTAG) {
            /* We have called, but not built the new frame, so
             * build it for them. */
            current_control_frame_pointer[0] =
                *os_context_register_addr(context, reg_OCFP);
            current_control_frame_pointer[1] =
                *os_context_register_addr(context, reg_LRA);
            current_control_frame_pointer += 8;
            /* Build our frame on top of it. */
            oldcont = (lispobj)(*os_context_register_addr(context, reg_CFP));
        }
        else {
            /* We haven't yet called, build our frame as if the
             * partial frame wasn't there. */
            oldcont = (lispobj)(*os_context_register_addr(context, reg_OCFP));
        }
    }
    /* We can't tell whether we are still in the caller if it had to
     * allocate a stack frame due to stack arguments. */
    /* This observation provoked some past CMUCL maintainer to ask
     * "Can anything strange happen during return?" */
    else {
        /* normal case */
        oldcont = (lispobj)(*os_context_register_addr(context, reg_CFP));
    }

    current_control_stack_pointer = current_control_frame_pointer + 8;

    current_control_frame_pointer[0] = oldcont;
    current_control_frame_pointer[1] = NIL;
    current_control_frame_pointer[2] =
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
    check_blockables_blocked_or_lose();

    /* Get current Lisp state from context. */
#ifdef reg_ALLOC
    dynamic_space_free_pointer =
        (lispobj *)(unsigned long)
            (*os_context_register_addr(context, reg_ALLOC));
#if defined(LISP_FEATURE_ALPHA)
    if ((long)dynamic_space_free_pointer & 1) {
        lose("dead in fake_foreign_function_call, context = %x", context);
    }
#endif
#endif
#ifdef reg_BSP
    current_binding_stack_pointer =
        (lispobj *)(unsigned long)
            (*os_context_register_addr(context, reg_BSP));
#endif

    build_fake_control_stack_frames(thread,context);

    /* Do dynamic binding of the active interrupt context index
     * and save the context in the context array. */
    context_index =
        fixnum_value(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX,thread));

    if (context_index >= MAX_INTERRUPTS) {
        lose("maximum interrupt nesting depth (%d) exceeded", MAX_INTERRUPTS);
    }

    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX,
                  make_fixnum(context_index + 1),thread);

    thread->interrupt_contexts[context_index] = context;

    /* no longer in Lisp now */
    foreign_function_call_active = 1;
}

/* blocks all blockable signals.  If you are calling from a signal handler,
 * the usual signal mask will be restored from the context when the handler
 * finishes.  Otherwise, be careful */
void
undo_fake_foreign_function_call(os_context_t *context)
{
    struct thread *thread=arch_os_get_current_thread();
    /* Block all blockable signals. */
    block_blockable_signals();

    /* going back into Lisp */
    foreign_function_call_active = 0;

    /* Undo dynamic binding of FREE_INTERRUPT_CONTEXT_INDEX */
    unbind(thread);

#ifdef reg_ALLOC
    /* Put the dynamic space free pointer back into the context. */
    *os_context_register_addr(context, reg_ALLOC) =
        (unsigned long) dynamic_space_free_pointer;
#endif
}

/* a handler for the signal caused by execution of a trap opcode
 * signalling an internal error */
void
interrupt_internal_error(int signal, siginfo_t *info, os_context_t *context,
                         boolean continuable)
{
    lispobj context_sap;

    fake_foreign_function_call(context);

    if (!internal_errors_enabled) {
        describe_internal_error(context);
        /* There's no good way to recover from an internal error
         * before the Lisp error handling mechanism is set up. */
        lose("internal error too early in init, can't recover");
    }

    /* Allocate the SAP object while the interrupts are still
     * disabled. */
    context_sap = alloc_sap(context);

    thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);

    SHOW("in interrupt_internal_error");
#ifdef QSHOW
    /* Display some rudimentary debugging information about the
     * error, so that even if the Lisp error handler gets badly
     * confused, we have a chance to determine what's going on. */
    describe_internal_error(context);
#endif
    funcall2(SymbolFunction(INTERNAL_ERROR), context_sap,
             continuable ? T : NIL);

    undo_fake_foreign_function_call(context); /* blocks signals again */
    if (continuable)
        arch_skip_instruction(context);
}

void
interrupt_handle_pending(os_context_t *context)
{
    struct thread *thread;
    struct interrupt_data *data;

    check_blockables_blocked_or_lose();

    thread=arch_os_get_current_thread();
    data=thread->interrupt_data;

#if defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64)
    /* If pseudo_atomic_interrupted is set then the interrupt is going
     * to be handled now, ergo it's safe to clear it. */
    arch_clear_pseudo_atomic_interrupted(context);
#endif

    if (SymbolValue(GC_INHIBIT,thread)==NIL) {
#ifdef LISP_FEATURE_SB_THREAD
        if (SymbolValue(STOP_FOR_GC_PENDING,thread) != NIL) {
            /* another thread has already initiated a gc, this attempt
             * might as well be cancelled */
            SetSymbolValue(GC_PENDING,NIL,thread);
            SetSymbolValue(STOP_FOR_GC_PENDING,NIL,thread);
            sig_stop_for_gc_handler(SIG_STOP_FOR_GC,NULL,context);
        } else
#endif
        if (SymbolValue(GC_PENDING,thread) != NIL) {
            /* GC_PENDING is cleared in SUB-GC, or if another thread
             * is doing a gc already we will get a SIG_STOP_FOR_GC and
             * that will clear it. */
            interrupt_maybe_gc_int(0,NULL,context);
        }
        check_blockables_blocked_or_lose();
    }

    /* we may be here only to do the gc stuff, if interrupts are
     * enabled run the pending handler */
    if (!((SymbolValue(INTERRUPTS_ENABLED,thread) == NIL) ||
          (
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
           (!foreign_function_call_active) &&
#endif
           arch_pseudo_atomic_atomic(context)))) {

        /* There may be no pending handler, because it was only a gc
         * that had to be executed or because pseudo atomic triggered
         * twice for a single interrupt. For the interested reader,
         * that may happen if an interrupt hits after the interrupted
         * flag is cleared but before pseduo-atomic is set and a
         * pseudo atomic is interrupted in that interrupt. */
        if (data->pending_handler) {

            /* If we're here as the result of a pseudo-atomic as opposed
             * to WITHOUT-INTERRUPTS, then INTERRUPT_PENDING is already
             * NIL, because maybe_defer_handler sets
             * PSEUDO_ATOMIC_INTERRUPTED only if interrupts are enabled.*/
            SetSymbolValue(INTERRUPT_PENDING, NIL,thread);

            /* restore the saved signal mask from the original signal (the
             * one that interrupted us during the critical section) into the
             * os_context for the signal we're currently in the handler for.
             * This should ensure that when we return from the handler the
             * blocked signals are unblocked */
            sigcopyset(os_context_sigmask_addr(context), &data->pending_mask);

            sigemptyset(&data->pending_mask);
            /* This will break on sparc linux: the deferred handler really wants
             * to be called with a void_context */
            run_deferred_handler(data,(void *)context);
        }
    }
}

/*
 * the two main signal handlers:
 *   interrupt_handle_now(..)
 *   maybe_now_maybe_later(..)
 *
 * to which we have added interrupt_handle_now_handler(..).  Why?
 * Well, mostly because the SPARC/Linux platform doesn't quite do
 * signals the way we want them done.  The third argument in the
 * handler isn't filled in by the kernel properly, so we fix it up
 * ourselves in the arch_os_get_context(..) function; however, we only
 * want to do this when we first hit the handler, and not when
 * interrupt_handle_now(..) is being called from some other handler
 * (when the fixup will already have been done). -- CSR, 2002-07-23
 */

void
interrupt_handle_now(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = (os_context_t*)void_context;
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
    boolean were_in_lisp;
#endif
    union interrupt_handler handler;
    check_blockables_blocked_or_lose();
    if (sigismember(&deferrable_sigset,signal))
        check_interrupts_enabled_or_lose(context);

#ifdef LISP_FEATURE_LINUX
    /* Under Linux on some architectures, we appear to have to restore
       the FPU control word from the context, as after the signal is
       delivered we appear to have a null FPU control word. */
    os_restore_fp_control(context);
#endif
    handler = interrupt_handlers[signal];

    if (ARE_SAME_HANDLER(handler.c, SIG_IGN)) {
        return;
    }

#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
    were_in_lisp = !foreign_function_call_active;
    if (were_in_lisp)
#endif
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
        lose("no handler for signal %d in interrupt_handle_now(..)", signal);

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

        lispobj info_sap,context_sap = alloc_sap(context);
        info_sap = alloc_sap(info);
        /* Leave deferrable signals blocked, the handler itself will
         * allow signals again when it sees fit. */
#ifdef LISP_FEATURE_SB_THREAD
        {
            sigset_t unblock;
            sigemptyset(&unblock);
            sigaddset(&unblock, SIG_STOP_FOR_GC);
            thread_sigmask(SIG_UNBLOCK, &unblock, 0);
        }
#endif

        FSHOW_SIGNAL((stderr,"/calling Lisp-level handler\n"));

        funcall3(handler.lisp,
                 make_fixnum(signal),
                 info_sap,
                 context_sap);
    } else {

        FSHOW_SIGNAL((stderr,"/calling C-level handler\n"));

        /* Allow signals again. */
        thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);

        (*handler.c)(signal, info, void_context);
    }

#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
    if (were_in_lisp)
#endif
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
run_deferred_handler(struct interrupt_data *data, void *v_context) {
    /* The pending_handler may enable interrupts and then another
     * interrupt may hit, overwrite interrupt_data, so reset the
     * pending handler before calling it. Trust the handler to finish
     * with the siginfo before enabling interrupts. */
    void (*pending_handler) (int, siginfo_t*, void*)=data->pending_handler;
    data->pending_handler=0;
    (*pending_handler)(data->pending_signal,&(data->pending_info), v_context);
}

boolean
maybe_defer_handler(void *handler, struct interrupt_data *data,
                    int signal, siginfo_t *info, os_context_t *context)
{
    struct thread *thread=arch_os_get_current_thread();

    check_blockables_blocked_or_lose();

    if (SymbolValue(INTERRUPT_PENDING,thread) != NIL)
        lose("interrupt already pending");
    /* If interrupts are disabled then INTERRUPT_PENDING is set and
     * not PSEDUO_ATOMIC_INTERRUPTED. This is important for a pseudo
     * atomic section inside a WITHOUT-INTERRUPTS.
     */
    if (SymbolValue(INTERRUPTS_ENABLED,thread) == NIL) {
        store_signal_data_for_later(data,handler,signal,info,context);
        SetSymbolValue(INTERRUPT_PENDING, T,thread);
        FSHOW_SIGNAL((stderr,
                      "/maybe_defer_handler(%x,%d),thread=%lu: deferred\n",
                      (unsigned int)handler,signal,
                      (unsigned long)thread->os_thread));
        return 1;
    }
    /* a slightly confusing test.  arch_pseudo_atomic_atomic() doesn't
     * actually use its argument for anything on x86, so this branch
     * may succeed even when context is null (gencgc alloc()) */
    if (
#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64)
        /* FIXME: this foreign_function_call_active test is dubious at
         * best. If a foreign call is made in a pseudo atomic section
         * (?) or more likely a pseudo atomic section is in a foreign
         * call then an interrupt is executed immediately. Maybe it
         * has to do with C code not maintaining pseudo atomic
         * properly. MG - 2005-08-10 */
        (!foreign_function_call_active) &&
#endif
        arch_pseudo_atomic_atomic(context)) {
        store_signal_data_for_later(data,handler,signal,info,context);
        arch_set_pseudo_atomic_interrupted(context);
        FSHOW_SIGNAL((stderr,
                      "/maybe_defer_handler(%x,%d),thread=%lu: deferred(PA)\n",
                      (unsigned int)handler,signal,
                      (unsigned long)thread->os_thread));
        return 1;
    }
    FSHOW_SIGNAL((stderr,
                  "/maybe_defer_handler(%x,%d),thread=%lu: not deferred\n",
                  (unsigned int)handler,signal,
                  (unsigned long)thread->os_thread));
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
    if(context) {
        /* the signal mask in the context (from before we were
         * interrupted) is copied to be restored when
         * run_deferred_handler happens.  Then the usually-blocked
         * signals are added to the mask in the context so that we are
         * running with blocked signals when the handler returns */
        sigcopyset(&(data->pending_mask),os_context_sigmask_addr(context));
        sigaddset_deferrable(os_context_sigmask_addr(context));
    }
}

static void
maybe_now_maybe_later(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    struct thread *thread=arch_os_get_current_thread();
    struct interrupt_data *data=thread->interrupt_data;
#ifdef LISP_FEATURE_LINUX
    os_restore_fp_control(context);
#endif
    if(maybe_defer_handler(interrupt_handle_now,data,signal,info,context))
        return;
    interrupt_handle_now(signal, info, context);
#ifdef LISP_FEATURE_DARWIN
    /* Work around G5 bug */
    DARWIN_FIX_CONTEXT(context);
#endif
}

static void
low_level_interrupt_handle_now(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = (os_context_t*)void_context;

#ifdef LISP_FEATURE_LINUX
    os_restore_fp_control(context);
#endif
    check_blockables_blocked_or_lose();
    check_interrupts_enabled_or_lose(context);
    interrupt_low_level_handlers[signal](signal, info, void_context);
#ifdef LISP_FEATURE_DARWIN
    /* Work around G5 bug */
    DARWIN_FIX_CONTEXT(context);
#endif
}

static void
low_level_maybe_now_maybe_later(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    struct thread *thread=arch_os_get_current_thread();
    struct interrupt_data *data=thread->interrupt_data;
#ifdef LISP_FEATURE_LINUX
    os_restore_fp_control(context);
#endif
    if(maybe_defer_handler(low_level_interrupt_handle_now,data,
                           signal,info,context))
        return;
    low_level_interrupt_handle_now(signal, info, context);
#ifdef LISP_FEATURE_DARWIN
    /* Work around G5 bug */
    DARWIN_FIX_CONTEXT(context);
#endif
}

#ifdef LISP_FEATURE_SB_THREAD

void
sig_stop_for_gc_handler(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    struct thread *thread=arch_os_get_current_thread();
    sigset_t ss;

    if ((arch_pseudo_atomic_atomic(context) ||
         SymbolValue(GC_INHIBIT,thread) != NIL)) {
        SetSymbolValue(STOP_FOR_GC_PENDING,T,thread);
        if (SymbolValue(GC_INHIBIT,thread) == NIL)
            arch_set_pseudo_atomic_interrupted(context);
        FSHOW_SIGNAL((stderr,"thread=%lu sig_stop_for_gc deferred\n",
                      thread->os_thread));
    } else {
        /* need the context stored so it can have registers scavenged */
        fake_foreign_function_call(context);

        sigfillset(&ss); /* Block everything. */
        thread_sigmask(SIG_BLOCK,&ss,0);

        if(thread->state!=STATE_RUNNING) {
            lose("sig_stop_for_gc_handler: wrong thread state: %ld\n",
                 fixnum_value(thread->state));
        }
        thread->state=STATE_SUSPENDED;
        FSHOW_SIGNAL((stderr,"thread=%lu suspended\n",thread->os_thread));

        sigemptyset(&ss); sigaddset(&ss,SIG_STOP_FOR_GC);
        /* It is possible to get SIGCONT (and probably other
         * non-blockable signals) here. */
        while (sigwaitinfo(&ss,0) != SIG_STOP_FOR_GC);
        FSHOW_SIGNAL((stderr,"thread=%lu resumed\n",thread->os_thread));
        if(thread->state!=STATE_RUNNING) {
            lose("sig_stop_for_gc_handler: wrong thread state on wakeup: %ld\n",
                 fixnum_value(thread->state));
        }

        undo_fake_foreign_function_call(context);
    }
}
#endif

void
interrupt_handle_now_handler(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    interrupt_handle_now(signal, info, context);
#ifdef LISP_FEATURE_DARWIN
    DARWIN_FIX_CONTEXT(context);
#endif
}

/*
 * stuff to detect and handle hitting the GC trigger
 */

#ifndef LISP_FEATURE_GENCGC
/* since GENCGC has its own way to record trigger */
static boolean
gc_trigger_hit(int signal, siginfo_t *info, os_context_t *context)
{
    if (current_auto_gc_trigger == NULL)
        return 0;
    else{
        void *badaddr=arch_get_bad_addr(signal,info,context);
        return (badaddr >= (void *)current_auto_gc_trigger &&
                badaddr <((void *)current_dynamic_space + DYNAMIC_SPACE_SIZE));
    }
}
#endif

/* manipulate the signal context and stack such that when the handler
 * returns, it will call function instead of whatever it was doing
 * previously
 */

#if (defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
extern int *context_eflags_addr(os_context_t *context);
#endif

extern lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs);
extern void post_signal_tramp(void);
void
arrange_return_to_lisp_function(os_context_t *context, lispobj function)
{
#if !(defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
    void * fun=native_pointer(function);
    void *code = &(((struct simple_fun *) fun)->code);
#endif

    /* Build a stack frame showing `interrupted' so that the
     * user's backtrace makes (as much) sense (as usual) */

    /* FIXME: what about restoring fp state? */
    /* FIXME: what about restoring errno? */
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
    *os_context_pc_addr(context) = (os_context_register_t)call_into_lisp;
    *os_context_register_addr(context,reg_ECX) = 0;
    *os_context_register_addr(context,reg_EBP) = (os_context_register_t)(sp-2);
#ifdef __NetBSD__
    *os_context_register_addr(context,reg_UESP) =
        (os_context_register_t)(sp-15);
#else
    *os_context_register_addr(context,reg_ESP) = (os_context_register_t)(sp-15);
#endif
#elif defined(LISP_FEATURE_X86_64)
    *os_context_pc_addr(context) = (os_context_register_t)call_into_lisp;
    *os_context_register_addr(context,reg_RCX) = 0;
    *os_context_register_addr(context,reg_RBP) = (os_context_register_t)(sp-2);
    *os_context_register_addr(context,reg_RSP) = (os_context_register_t)(sp-18);
#else
    /* this much of the calling convention is common to all
       non-x86 ports */
    *os_context_pc_addr(context) = (os_context_register_t)(unsigned long)code;
    *os_context_register_addr(context,reg_NARGS) = 0;
    *os_context_register_addr(context,reg_LIP) =
        (os_context_register_t)(unsigned long)code;
    *os_context_register_addr(context,reg_CFP) =
        (os_context_register_t)(unsigned long)current_control_frame_pointer;
#endif
#ifdef ARCH_HAS_NPC_REGISTER
    *os_context_npc_addr(context) =
        4 + *os_context_pc_addr(context);
#endif
#ifdef LISP_FEATURE_SPARC
    *os_context_register_addr(context,reg_CODE) =
        (os_context_register_t)(fun + FUN_POINTER_LOWTAG);
#endif
}

#ifdef LISP_FEATURE_SB_THREAD

/* FIXME: this function can go away when all lisp handlers are invoked
 * via arrange_return_to_lisp_function. */
void
interrupt_thread_handler(int num, siginfo_t *info, void *v_context)
{
    os_context_t *context = (os_context_t*)arch_os_get_context(&v_context);
    /* let the handler enable interrupts again when it sees fit */
    sigaddset_deferrable(os_context_sigmask_addr(context));
    arrange_return_to_lisp_function(context, SymbolFunction(RUN_INTERRUPTION));
}

#endif

/* KLUDGE: Theoretically the approach we use for undefined alien
 * variables should work for functions as well, but on PPC/Darwin
 * we get bus error at bogus addresses instead, hence this workaround,
 * that has the added benefit of automatically discriminating between
 * functions and variables.
 */
void
undefined_alien_function() {
    funcall0(SymbolFunction(UNDEFINED_ALIEN_FUNCTION_ERROR));
}

boolean
handle_guard_page_triggered(os_context_t *context,os_vm_address_t addr)
{
    struct thread *th=arch_os_get_current_thread();

    /* note the os_context hackery here.  When the signal handler returns,
     * it won't go back to what it was doing ... */
    if(addr >= CONTROL_STACK_GUARD_PAGE(th) &&
       addr < CONTROL_STACK_GUARD_PAGE(th) + os_vm_page_size) {
        /* We hit the end of the control stack: disable guard page
         * protection so the error handler has some headroom, protect the
         * previous page so that we can catch returns from the guard page
         * and restore it. */
        protect_control_stack_guard_page(0);
        protect_control_stack_return_guard_page(1);

        arrange_return_to_lisp_function
            (context, SymbolFunction(CONTROL_STACK_EXHAUSTED_ERROR));
        return 1;
    }
    else if(addr >= CONTROL_STACK_RETURN_GUARD_PAGE(th) &&
            addr < CONTROL_STACK_RETURN_GUARD_PAGE(th) + os_vm_page_size) {
        /* We're returning from the guard page: reprotect it, and
         * unprotect this one. This works even if we somehow missed
         * the return-guard-page, and hit it on our way to new
         * exhaustion instead. */
        protect_control_stack_guard_page(1);
        protect_control_stack_return_guard_page(0);
        return 1;
    }
    else if (addr >= undefined_alien_address &&
             addr < undefined_alien_address + os_vm_page_size) {
        arrange_return_to_lisp_function
          (context, SymbolFunction(UNDEFINED_ALIEN_VARIABLE_ERROR));
        return 1;
    }
    else return 0;
}

#ifndef LISP_FEATURE_GENCGC
/* This function gets called from the SIGSEGV (for e.g. Linux, NetBSD, &
 * OpenBSD) or SIGBUS (for e.g. FreeBSD) handler. Here we check
 * whether the signal was due to treading on the mprotect()ed zone -
 * and if so, arrange for a GC to happen. */
extern unsigned long bytes_consed_between_gcs; /* gc-common.c */

boolean
interrupt_maybe_gc(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context=(os_context_t *) void_context;

    if(!foreign_function_call_active && gc_trigger_hit(signal, info, context)){
        struct thread *thread=arch_os_get_current_thread();
        clear_auto_gc_trigger();
        /* Don't flood the system with interrupts if the need to gc is
         * already noted. This can happen for example when SUB-GC
         * allocates or after a gc triggered in a WITHOUT-GCING. */
        if (SymbolValue(GC_PENDING,thread) == NIL) {
            if (SymbolValue(GC_INHIBIT,thread) == NIL) {
                if (arch_pseudo_atomic_atomic(context)) {
                    /* set things up so that GC happens when we finish
                     * the PA section */
                    SetSymbolValue(GC_PENDING,T,thread);
                    arch_set_pseudo_atomic_interrupted(context);
                } else {
                    interrupt_maybe_gc_int(signal,info,void_context);
                }
            } else {
                SetSymbolValue(GC_PENDING,T,thread);
            }
        }
        return 1;
    }
    return 0;
}

#endif

/* this is also used by gencgc, in alloc() */
boolean
interrupt_maybe_gc_int(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context=(os_context_t *) void_context;
    struct thread *thread=arch_os_get_current_thread();

    fake_foreign_function_call(context);

    /* SUB-GC may return without GCing if *GC-INHIBIT* is set, in
     * which case we will be running with no gc trigger barrier
     * thing for a while.  But it shouldn't be long until the end
     * of WITHOUT-GCING.
     *
     * FIXME: It would be good to protect the end of dynamic space
     * and signal a storage condition from there.
     */

    /* Restore the signal mask from the interrupted context before
     * calling into Lisp if interrupts are enabled. Why not always?
     *
     * Suppose there is a WITHOUT-INTERRUPTS block far, far out. If an
     * interrupt hits while in SUB-GC, it is deferred and the
     * os_context_sigmask of that interrupt is set to block further
     * deferrable interrupts (until the first one is
     * handled). Unfortunately, that context refers to this place and
     * when we return from here the signals will not be blocked.
     *
     * A kludgy alternative is to propagate the sigmask change to the
     * outer context.
     */
    if(SymbolValue(INTERRUPTS_ENABLED,thread)!=NIL)
        thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);
#ifdef LISP_FEATURE_SB_THREAD
    else {
        sigset_t new;
        sigemptyset(&new);
        sigaddset(&new,SIG_STOP_FOR_GC);
        thread_sigmask(SIG_UNBLOCK,&new,0);
    }
#endif
    funcall0(SymbolFunction(SUB_GC));

    undo_fake_foreign_function_call(context);
    return 1;
}


/*
 * noise to install handlers
 */

/* In Linux 2.4 synchronous signals (sigtrap & co) can be delivered if
 * they are blocked, in Linux 2.6 the default handler is invoked
 * instead that usually coredumps. One might hastily think that adding
 * SA_NODEFER helps, but until ~2.6.13 if SA_NODEFER is specified then
 * the whole sa_mask is ignored and instead of not adding the signal
 * in question to the mask. That means if it's not blockable the
 * signal must be unblocked at the beginning of signal handlers.
 */
static volatile int sigaction_nodefer_works = -1;

static void
sigaction_nodefer_test_handler(int signal, siginfo_t *info, void *void_context)
{
    sigset_t empty, current;
    int i;
    sigemptyset(&empty);
    sigprocmask(SIG_BLOCK, &empty, &current);
    for(i = 1; i < NSIG; i++)
        if (sigismember(&current, i) != ((i == SIGABRT) ? 1 : 0)) {
            FSHOW_SIGNAL((stderr, "SA_NODEFER doesn't work, signal %d\n", i));
            sigaction_nodefer_works = 0;
        }
    if (sigaction_nodefer_works == -1)
        sigaction_nodefer_works = 1;
}

static void
see_if_sigaction_nodefer_works()
{
    struct sigaction sa;

    sa.sa_flags = SA_SIGINFO | SA_NODEFER;
    sa.sa_sigaction = sigaction_nodefer_test_handler;
    sigemptyset(&sa.sa_mask);
    sigaddset(&sa.sa_mask, SIGABRT);
    /* We can use any signal for which a handler will be installed
     * later. Let's go with SIGINT because gdb barfs on SIGTRAP on
     * Darwin. */
    sigaction(SIGINT, &sa, NULL);
    /* Make sure no signals are blocked. */
    {
        sigset_t empty;
        sigemptyset(&empty);
        sigprocmask(SIG_SETMASK, &empty, 0);
    }
    kill(getpid(), SIGINT);
    while (sigaction_nodefer_works == -1);
}

static void
unblock_me_trampoline(int signal, siginfo_t *info, void *void_context)
{
    sigset_t unblock;
    sigemptyset(&unblock);
    sigaddset(&unblock, signal);
    thread_sigmask(SIG_UNBLOCK, &unblock, 0);
    interrupt_handle_now_handler(signal, info, void_context);
}

static void
low_level_unblock_me_trampoline(int signal, siginfo_t *info, void *void_context)
{
    sigset_t unblock;
    sigemptyset(&unblock);
    sigaddset(&unblock, signal);
    thread_sigmask(SIG_UNBLOCK, &unblock, 0);
    (*interrupt_low_level_handlers[signal])(signal, info, void_context);
}

void
undoably_install_low_level_interrupt_handler (int signal,
                                              void handler(int,
                                                           siginfo_t*,
                                                           void*))
{
    struct sigaction sa;

    if (0 > signal || signal >= NSIG) {
        lose("bad signal number %d", signal);
    }

    if (ARE_SAME_HANDLER(handler, SIG_DFL))
        sa.sa_sigaction = handler;
    else if (sigismember(&deferrable_sigset,signal))
        sa.sa_sigaction = low_level_maybe_now_maybe_later;
    else if (!sigaction_nodefer_works &&
             !sigismember(&blockable_sigset, signal))
        sa.sa_sigaction = low_level_unblock_me_trampoline;
    else
        sa.sa_sigaction = handler;

    sigcopyset(&sa.sa_mask, &blockable_sigset);
    sa.sa_flags = SA_SIGINFO | SA_RESTART |
        (sigaction_nodefer_works ? SA_NODEFER : 0);
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    if((signal==SIG_MEMORY_FAULT)
#ifdef SIG_INTERRUPT_THREAD
       || (signal==SIG_INTERRUPT_THREAD)
#endif
       )
        sa.sa_flags |= SA_ONSTACK;
#endif

    sigaction(signal, &sa, NULL);
    interrupt_low_level_handlers[signal] =
        (ARE_SAME_HANDLER(handler, SIG_DFL) ? 0 : handler);
}

/* This is called from Lisp. */
unsigned long
install_handler(int signal, void handler(int, siginfo_t*, void*))
{
    struct sigaction sa;
    sigset_t old, new;
    union interrupt_handler oldhandler;

    FSHOW((stderr, "/entering POSIX install_handler(%d, ..)\n", signal));

    sigemptyset(&new);
    sigaddset(&new, signal);
    thread_sigmask(SIG_BLOCK, &new, &old);

    FSHOW((stderr, "/interrupt_low_level_handlers[signal]=%x\n",
           (unsigned int)interrupt_low_level_handlers[signal]));
    if (interrupt_low_level_handlers[signal]==0) {
        if (ARE_SAME_HANDLER(handler, SIG_DFL) ||
            ARE_SAME_HANDLER(handler, SIG_IGN))
            sa.sa_sigaction = handler;
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

    return (unsigned long)oldhandler.lisp;
}

void
interrupt_init()
{
    int i;
    SHOW("entering interrupt_init()");
    see_if_sigaction_nodefer_works();
    sigemptyset(&deferrable_sigset);
    sigemptyset(&blockable_sigset);
    sigaddset_deferrable(&deferrable_sigset);
    sigaddset_blockable(&blockable_sigset);

    /* Set up high level handler information. */
    for (i = 0; i < NSIG; i++) {
        interrupt_handlers[i].c =
            /* (The cast here blasts away the distinction between
             * SA_SIGACTION-style three-argument handlers and
             * signal(..)-style one-argument handlers, which is OK
             * because it works to call the 1-argument form where the
             * 3-argument form is expected.) */
            (void (*)(int, siginfo_t*, void*))SIG_DFL;
    }

    SHOW("returning from interrupt_init()");
}
