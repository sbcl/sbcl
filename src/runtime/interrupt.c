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

#include <stdio.h>

#include <signal.h>
#ifdef mach /* KLUDGE: #ifdef on lowercase symbols? Ick. -- WHN 19990904 */
#ifdef mips
#include <mips/cpu.h>
#endif
#endif

#include "runtime.h"
#include "arch.h"
#include "sbcl.h"
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

void sigaddset_blockable(sigset_t *s)
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
}

/* When we catch an internal error, should we pass it back to Lisp to
 * be handled in a high-level way? (Early in cold init, the answer is
 * 'no', because Lisp is still too brain-dead to handle anything.
 * After sufficient initialization has been completed, the answer
 * becomes 'yes'.) */
boolean internal_errors_enabled = 0;

os_context_t *lisp_interrupt_contexts[MAX_INTERRUPTS];

/* As far as I can tell, what's going on here is:
 *
 * In the case of most signals, when Lisp asks us to handle the
 * signal, the outermost handler (the one actually passed to UNIX) is
 * either interrupt_handle_now(..) or interrupt_handle_later(..).
 * In that case, the Lisp-level handler is stored in interrupt_handlers[..]
 * and interrupt_low_level_handlers[..] is cleared.
 *
 * However, some signals need special handling, e.g. the SIGSEGV (for
 * Linux) or SIGBUS (for FreeBSD) used by the garbage collector to
 * detect violations of write protection, because some cases of such
 * signals (e.g. GC-related violations of write protection) are
 * handled at C level and never passed on to Lisp. For such signals,
 * we still store any Lisp-level handler in interrupt_handlers[..],
 * but for the outermost handle we use the value from
 * interrupt_low_level_handlers[..], instead of the ordinary
 * interrupt_handle_now(..) or interrupt_handle_later(..).
 *
 * -- WHN 20000728 */
void (*interrupt_low_level_handlers[NSIG]) (int, siginfo_t*, void*) = {0};
union interrupt_handler interrupt_handlers[NSIG];

/* signal number, siginfo_t, and old mask information for pending signal
 *
 * pending_signal=0 when there is no pending signal. */
static int pending_signal = 0;
static siginfo_t pending_info;
static sigset_t pending_mask;

static boolean maybe_gc_pending = 0;

/*
 * utility routines used by various signal handlers
 */

void
fake_foreign_function_call(os_context_t *context)
{
    int context_index;
#ifndef __i386__
    lispobj oldcont;
#endif

    /* Get current Lisp state from context. */
#ifdef reg_ALLOC
    dynamic_space_free_pointer =
	(lispobj *)(*os_context_register_addr(context, reg_ALLOC));
#ifdef alpha
    if ((long)dynamic_space_free_pointer & 1) {
	lose("dead in fake_foreign_function_call, context = %x", context);
    }
#endif
#endif
#ifdef reg_BSP
    current_binding_stack_pointer =
	(lispobj *)(*os_context_register_addr(context, reg_BSP));
#endif

#ifndef __i386__
    /* Build a fake stack frame. */
    current_control_frame_pointer =
	(lispobj *)(*os_context_register_addr(context, reg_CSP));
    if ((lispobj *)(*os_context_register_addr(context, reg_CFP))
	== current_control_frame_pointer) {
        /* There is a small window during call where the callee's
         * frame isn't built yet. */
        if (LowtagOf(*os_context_register_addr(context, reg_CODE))
	    == type_FunctionPointer) {
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
    /* ### We can't tell whether we are still in the caller if it had
     * to reg_ALLOCate the stack frame due to stack arguments. */
    /* ### Can anything strange happen during return? */
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

    /* Do dynamic binding of the active interrupt context index
     * and save the context in the context array. */
    context_index = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
    /* FIXME: Ick! Why use abstract "make_fixnum" in some places if
     * you're going to convert from fixnum by bare >>2 in other
     * places? Use fixnum_value(..) here, and look for other places
     * which do bare >> and << for fixnum_value and make_fixnum. */

    if (context_index >= MAX_INTERRUPTS) {
        lose("maximum interrupt nesting depth (%d) exceeded",
	     MAX_INTERRUPTS);
    }

    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX,
		  make_fixnum(context_index + 1));

    lisp_interrupt_contexts[context_index] = context;

    /* no longer in Lisp now */
    foreign_function_call_active = 1;
}

void
undo_fake_foreign_function_call(os_context_t *context)
{
    /* Block all blockable signals. */
    sigset_t block;
    sigemptyset(&block);
    sigaddset_blockable(&block);
    sigprocmask(SIG_BLOCK, &block, 0);

    /* going back into Lisp */
    foreign_function_call_active = 0;

    /* Undo dynamic binding. */
    /* ### Do I really need to unbind_to_here()? */
    /* FIXME: Is this to undo the binding of
     * FREE_INTERRUPT_CONTEXT_INDEX? If so, we should say so. And
     * perhaps yes, unbind_to_here() really would be clearer and less
     * fragile.. */
    unbind();

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
    lispobj context_sap = 0;

    fake_foreign_function_call(context);

    /* Allocate the SAP object while the interrupts are still
     * disabled. */
    if (internal_errors_enabled) {
	context_sap = alloc_sap(context);
    }

    sigprocmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);

    if (internal_errors_enabled) {
        SHOW("in interrupt_internal_error");
#if QSHOW
	/* Display some rudimentary debugging information about the
	 * error, so that even if the Lisp error handler gets badly
	 * confused, we have a chance to determine what's going on. */
	describe_internal_error(context);
#endif
	funcall2(SymbolFunction(INTERNAL_ERROR), context_sap,
		 continuable ? T : NIL);
    } else {
	describe_internal_error(context);
	/* There's no good way to recover from an internal error
	 * before the Lisp error handling mechanism is set up. */
	lose("internal error too early in init, can't recover");
    }
    undo_fake_foreign_function_call(context);
    if (continuable) {
	arch_skip_instruction(context);
    }
}

void
interrupt_handle_pending(os_context_t *context)
{
#ifndef __i386__
    boolean were_in_lisp = !foreign_function_call_active;
#endif

    SetSymbolValue(INTERRUPT_PENDING, NIL);

    if (maybe_gc_pending) {
	maybe_gc_pending = 0;
#ifndef __i386__
	if (were_in_lisp)
#endif
	{
	    fake_foreign_function_call(context);
	}
	funcall0(SymbolFunction(MAYBE_GC));
#ifndef __i386__
	if (were_in_lisp)
#endif
	{
	    undo_fake_foreign_function_call(context);
        }
    }

    /* FIXME: How come we unconditionally copy from pending_mask into
     * the context, and then test whether pending_signal is set? If
     * pending_signal wasn't set, how could pending_mask be valid? */
    memcpy(os_context_sigmask_addr(context), &pending_mask, sizeof(sigset_t));
    sigemptyset(&pending_mask);
    if (pending_signal) {
	int signal = pending_signal;
	siginfo_t info;
	memcpy(&info, &pending_info, sizeof(siginfo_t));
	pending_signal = 0;
	interrupt_handle_now(signal, &info, context);
    }
}

/*
 * the two main signal handlers:
 *   interrupt_handle_now(..)
 *   maybe_now_maybe_later(..)
 */

void
interrupt_handle_now(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = (os_context_t*)void_context;
#ifndef __i386__
    boolean were_in_lisp;
#endif
    union interrupt_handler handler;

    /* FIXME: The CMU CL we forked off of had this Linux-only
     * operation here. Newer CMU CLs (e.g. 18c) have hairier
     * Linux/i386-only logic here. SBCL seems to be more reliable
     * without anything here. However, if we start supporting code
     * which sets the rounding mode, then we may want to do something
     * special to force the rounding mode back to some standard value
     * here, so that ISRs can have a standard environment. (OTOH, if
     * rounding modes are under user control, then perhaps we should
     * leave this up to the user.)
     *
     * For now we just suppress this code completely (just like the
     * parallel code in maybe_now_maybe_later).
     * #ifdef __linux__
     *    SET_FPU_CONTROL_WORD(context->__fpregs_mem.cw);
     * #endif
     */

    handler = interrupt_handlers[signal];

    if (ARE_SAME_HANDLER(handler.c, SIG_IGN)) {
	return;
    }

#ifndef __i386__
    were_in_lisp = !foreign_function_call_active;
    if (were_in_lisp)
#endif
    {
        fake_foreign_function_call(context);
    }

#ifdef QSHOW_SIGNALS
    FSHOW((stderr, "in interrupt_handle_now(%d, info, context)\n", signal));
#endif

    if (ARE_SAME_HANDLER(handler.c, SIG_DFL)) {

	/* This can happen if someone tries to ignore or default one
	 * of the signals we need for runtime support, and the runtime
	 * support decides to pass on it. */
	lose("no handler for signal %d in interrupt_handle_now(..)", signal);

    } else if (LowtagOf(handler.lisp) == type_FunctionPointer) {

        /* Allocate the SAPs while the interrupts are still disabled.
	 * (FIXME: Why? This is the way it was done in CMU CL, and it
	 * even had the comment noting that this is the way it was
	 * done, but no motivation..) */
        lispobj context_sap = alloc_sap(context);
        lispobj info_sap = alloc_sap(info);

        /* Allow signals again. */
        sigprocmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);

#ifdef QSHOW_SIGNALS
	SHOW("calling Lisp-level handler");
#endif

        funcall3(handler.lisp,
		 make_fixnum(signal),
		 info_sap,
		 context_sap);
    } else {

#ifdef QSHOW_SIGNALS
	SHOW("calling C-level handler");
#endif

        /* Allow signals again. */
        sigprocmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);
	
        (*handler.c)(signal, info, void_context);
    }

#ifndef __i386__
    if (were_in_lisp)
#endif
    {
        undo_fake_foreign_function_call(context);
    }
}

static void
maybe_now_maybe_later(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = (os_context_t*)void_context;

    /* FIXME: See Debian cmucl 2.4.17, and mail from DTC on the CMU CL
     * mailing list 23 Oct 1999, for changes in FPU handling at
     * interrupt time which should be ported into SBCL. Also see the
     * analogous logic at the head of interrupt_handle_now for
     * more related FIXME stuff. 
     *
     * For now, we just suppress this code completely.
     * #ifdef __linux__
     *    SET_FPU_CONTROL_WORD(context->__fpregs_mem.cw);
     * #endif
     */

    if (SymbolValue(INTERRUPTS_ENABLED) == NIL) {

	/* FIXME: This code is exactly the same as the code in the
	 * other leg of the if(..), and should be factored out into
	 * a shared function. */
        pending_signal = signal;
	memcpy(&pending_info, info, sizeof(siginfo_t));
        memcpy(&pending_mask,
	       os_context_sigmask_addr(context),
	       sizeof(sigset_t));
	sigaddset_blockable(os_context_sigmask_addr(context));

        SetSymbolValue(INTERRUPT_PENDING, T);

    } else if (
#ifndef __i386__
	       (!foreign_function_call_active) &&
#endif
	       arch_pseudo_atomic_atomic(context)) {

	/* FIXME: It would probably be good to replace these bare
	 * memcpy(..) calls with calls to cpy_siginfo_t and
	 * cpy_sigset_t, so that we only have to get the sizeof
	 * expressions right in one place, and after that static type
	 * checking takes over. */
        pending_signal = signal;
	memcpy(&pending_info, info, sizeof(siginfo_t));
	memcpy(&pending_mask,
	       os_context_sigmask_addr(context),
	       sizeof(sigset_t));
	sigaddset_blockable(os_context_sigmask_addr(context));

	arch_set_pseudo_atomic_interrupted(context);

    } else {
        interrupt_handle_now(signal, info, context);
    }
}

/*
 * stuff to detect and handle hitting the GC trigger
 */

#ifndef INTERNAL_GC_TRIGGER
static boolean
gc_trigger_hit(int signal, siginfo_t *info, os_context_t *context)
{
    if (current_auto_gc_trigger == NULL)
	return 0;
    else{
	lispobj *badaddr=(lispobj *)arch_get_bad_addr(signal,
						      info,
						      context);

	return (badaddr >= current_auto_gc_trigger &&
		badaddr < DYNAMIC_SPACE_START + DYNAMIC_SPACE_SIZE);
    }
}
#endif

#ifndef __i386__
boolean
interrupt_maybe_gc(int signal, siginfo_t *info, os_context_t *context)
{
    if (!foreign_function_call_active
#ifndef INTERNAL_GC_TRIGGER
	&& gc_trigger_hit(signal, info, context)
#endif
	) {
#ifndef INTERNAL_GC_TRIGGER
	clear_auto_gc_trigger();
#endif

	if (arch_pseudo_atomic_atomic(context)) {
	    maybe_gc_pending = 1;
	    if (pending_signal == 0) {
		/* FIXME: This copy-pending_mask-then-sigaddset_blockable
		 * idiom occurs over and over. It should be factored out
		 * into a function with a descriptive name. */
		memcpy(&pending_mask,
		       os_context_sigmask_addr(context),
		       sizeof(sigset_t));
		sigaddset_blockable(os_context_sigmask_addr(context));
	    }
	    arch_set_pseudo_atomic_interrupted(context);
	}
	else {
	    fake_foreign_function_call(context);
	    funcall0(SymbolFunction(MAYBE_GC));
	    undo_fake_foreign_function_call(context);
	}

	return 1;
    } else {
	return 0;
    }
}
#endif

/*
 * noise to install handlers
 */

/* Install a special low-level handler for signal; or if handler is
 * SIG_DFL, remove any special handling for signal. */
void
interrupt_install_low_level_handler (int signal,
				     void handler(int, siginfo_t*, void*))
{
    struct sigaction sa;

    sa.sa_sigaction = handler;
    sigemptyset(&sa.sa_mask);
    sigaddset_blockable(&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO | SA_RESTART;

    sigaction(signal, &sa, NULL);
    interrupt_low_level_handlers[signal] =
	(ARE_SAME_HANDLER(handler,SIG_DFL) ? 0 : handler);
}

/* This is called from Lisp. */
unsigned long
install_handler(int signal, void handler(int, siginfo_t*, void*))
{
    struct sigaction sa;
    sigset_t old, new;
    union interrupt_handler oldhandler;

    FSHOW((stderr, "entering POSIX install_handler(%d, ..)\n", signal));

    sigemptyset(&new);
    sigaddset(&new, signal);
    sigprocmask(SIG_BLOCK, &new, &old);

    sigemptyset(&new);
    sigaddset_blockable(&new);

    FSHOW((stderr, "interrupt_low_level_handlers[signal]=%d\n",
	   interrupt_low_level_handlers[signal]));
    if (interrupt_low_level_handlers[signal]==0) {
	if (ARE_SAME_HANDLER(handler, SIG_DFL) ||
	    ARE_SAME_HANDLER(handler, SIG_IGN)) {
	    sa.sa_sigaction = handler;
	} else if (sigismember(&new, signal)) {
	    sa.sa_sigaction = maybe_now_maybe_later;
	} else {
	    sa.sa_sigaction = interrupt_handle_now;
	}

	sigemptyset(&sa.sa_mask);
	sigaddset_blockable(&sa.sa_mask);
	sa.sa_flags = SA_SIGINFO | SA_RESTART;

	sigaction(signal, &sa, NULL);
    }

    oldhandler = interrupt_handlers[signal];
    interrupt_handlers[signal].c = handler;

    sigprocmask(SIG_SETMASK, &old, 0);

    FSHOW((stderr, "leaving POSIX install_handler(%d, ..)\n", signal));

    return (unsigned long)oldhandler.lisp;
}

void
interrupt_init(void)
{
    int i;

    for (i = 0; i < NSIG; i++) {
        interrupt_handlers[i].c =
	    /* (The cast here blasts away the distinction between
	     * SA_SIGACTION-style three-argument handlers and
	     * signal(..)-style one-argument handlers, which is OK
	     * because it works to call the 1-argument form where the
	     * 3-argument form is expected.) */
	    (void (*)(int, siginfo_t*, void*))SIG_DFL;
    }
}
