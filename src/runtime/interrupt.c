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
#include <stdlib.h>
#include <string.h>
#include <signal.h>

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
#include "genesis/simple-fun.h"
#include "genesis/fdefn.h"
#include "genesis/symbol.h"
#include "genesis/static-symbols.h"

void sigaddset_blockable(sigset_t *s)
{
    sigaddset(s, SIGHUP);
    sigaddset(s, SIGINT);
    sigaddset(s, SIGQUIT);
    sigaddset(s, SIGPIPE);
    sigaddset(s, SIGALRM);
    sigaddset(s, SIGURG);
    sigaddset(s, SIGFPE);
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


void (*interrupt_low_level_handlers[NSIG]) (int, siginfo_t*, void*) = {0};
union interrupt_handler interrupt_handlers[NSIG];

/* signal number, siginfo_t, and old mask information for pending signal
 *
 * pending_signal=0 when there is no pending signal. */
static int pending_signal = 0;
static siginfo_t pending_info;
static sigset_t pending_mask;

boolean maybe_gc_pending = 0;

/*
 * utility routines used by various signal handlers
 */

void 
build_fake_control_stack_frames(os_context_t *context)
{
#ifndef LISP_FEATURE_X86
    
    lispobj oldcont;

    /* Build a fake stack frame or frames */

    current_control_frame_pointer =
	(lispobj *)(*os_context_register_addr(context, reg_CSP));
    if ((lispobj *)(*os_context_register_addr(context, reg_CFP))
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

void
fake_foreign_function_call(os_context_t *context)
{
    int context_index;

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

    build_fake_control_stack_frames(context);

    /* Do dynamic binding of the active interrupt context index
     * and save the context in the context array. */
    context_index = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
    /* FIXME: Ick! Why use abstract "make_fixnum" in some places if
     * you're going to convert from fixnum by bare >>2 in other
     * places? Use fixnum_value(..) here, and look for other places
     * which do bare >> and << for fixnum_value and make_fixnum. */

    if (context_index >= MAX_INTERRUPTS) {
        lose("maximum interrupt nesting depth (%d) exceeded", MAX_INTERRUPTS);
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
    /* dan (2001.08.10) thinks the above supposition is probably correct */
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

/* This function handles pending interrupts.  Note that in C/kernel
 * terms we dealt with the signal already; we just haven't decided
 * whether to call a Lisp handler or do a GC or something like that.
 * If it helps, you can think of pending_{signal,mask,info} as a
 * one-element queue of signals that we have acknowledged but not
 * processed */

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

    /* FIXME: This isn't very clear. It would be good to reverse
     * engineer it and rewrite the code more clearly, or write a clear
     * explanation of what's going on in the comments, or both.
     *
     * WHN's question 1a: How come we unconditionally copy from
     * pending_mask into the context, and then test whether
     * pending_signal is set?
     * 
     * WHN's question 1b: If pending_signal wasn't set, how could
     * pending_mask be valid?
     * 
     * Dan Barlow's reply (sbcl-devel 2001-03-13): And the answer is -
     * or appears to be - because interrupt_maybe_gc set it that way
     * (look in the #ifndef __i386__ bit). We can't GC during a
     * pseudo-atomic, so we set maybe_gc_pending=1 and
     * arch_set_pseudo_atomic_interrupted(..) When we come out of
     * pseudo_atomic we're marked as interrupted, so we call
     * interrupt_handle_pending, which does the GC using the pending
     * context (it needs a context so that it has registers to use as
     * GC roots) then notices there's no actual interrupt handler to
     * call, so doesn't. That's the second question [1b] answered,
     * anyway. Why we still need to copy the pending_mask into the
     * context given that we're now done with the context anyway, I
     * couldn't say. */
#if 0
    memcpy(os_context_sigmask_addr(context), &pending_mask, 
	   4 /* sizeof(sigset_t) */ );
#endif
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
#ifndef __i386__
    boolean were_in_lisp;
#endif
    union interrupt_handler handler;

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
    
#ifndef __i386__
    were_in_lisp = !foreign_function_call_active;
    if (were_in_lisp)
#endif
    {
        fake_foreign_function_call(context);
    }

#ifdef QSHOW_SIGNALS
    FSHOW((stderr,
	   "/entering interrupt_handle_now(%d, info, context)\n",
	   signal));
#endif

    if (ARE_SAME_HANDLER(handler.c, SIG_DFL)) {

	/* This can happen if someone tries to ignore or default one
	 * of the signals we need for runtime support, and the runtime
	 * support decides to pass on it. */
	lose("no handler for signal %d in interrupt_handle_now(..)", signal);

    } else if (lowtag_of(handler.lisp) == FUN_POINTER_LOWTAG) {

        /* Allocate the SAPs while the interrupts are still disabled.
	 * (FIXME: Why? This is the way it was done in CMU CL, and it
	 * even had the comment noting that this is the way it was
	 * done, but no motivation..) */
        lispobj info_sap,context_sap = alloc_sap(context);
        info_sap = alloc_sap(info);
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

#ifdef QSHOW_SIGNALS
    FSHOW((stderr,
	   "/returning from interrupt_handle_now(%d, info, context)\n",
	   signal));
#endif
}

static void
maybe_now_maybe_later(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);

#ifdef LISP_FEATURE_LINUX
    os_restore_fp_control(context);
#endif 
    
    /* see comments at top of code/signal.lisp for what's going on here
     * with INTERRUPTS_ENABLED/INTERRUPT_HANDLE_NOW 
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


void
interrupt_handle_now_handler(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context = arch_os_get_context(&void_context);
    interrupt_handle_now(signal, info, context);
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

/* and similarly for the control stack guard page */

boolean handle_control_stack_guard_triggered(os_context_t *context,void *addr)
{
    /* note the os_context hackery here.  When the signal handler returns, 
     * it won't go back to what it was doing ... */
    if(addr>=(void *)CONTROL_STACK_GUARD_PAGE && 
       addr<(void *)(CONTROL_STACK_GUARD_PAGE+os_vm_page_size)) {
	void *fun;
	void *code;
	
	/* we hit the end of the control stack.  disable protection
	 * temporarily so the error handler has some headroom */
	protect_control_stack_guard_page(0);
	
	fun = (void *)
	    native_pointer((lispobj) SymbolFunction(CONTROL_STACK_EXHAUSTED_ERROR));
	code = &(((struct simple_fun *) fun)->code);

	/* Build a stack frame showing `interrupted' so that the
	 * user's backtrace makes (as much) sense (as usual) */
	build_fake_control_stack_frames(context);
	/* signal handler will "return" to this error-causing function */
	*os_context_pc_addr(context) = code;
#ifdef LISP_FEATURE_X86
	*os_context_register_addr(context,reg_ECX) = 0; 
#else
	/* this much of the calling convention is common to all
	   non-x86 ports */
	*os_context_register_addr(context,reg_NARGS) = 0; 
	*os_context_register_addr(context,reg_LIP) = code;
	*os_context_register_addr(context,reg_CFP) = 
	    current_control_frame_pointer;
#endif
#ifdef ARCH_HAS_NPC_REGISTER
	*os_context_npc_addr(context) =
	    4 + *os_context_pc_addr(context);
#endif
#ifdef LISP_FEATURE_SPARC
	/* Bletch.  This is a feature of the SPARC calling convention,
	   which sadly I'm not going to go into in large detail here,
	   as I don't know it well enough.  Suffice to say that if the
	   line 

	   (INST MOVE CODE-TN FUNCTION) 

	   in compiler/sparc/call.lisp is changed, then this bit can
	   probably go away.  -- CSR, 2002-07-24 */
	*os_context_register_addr(context,reg_CODE) = 
	    fun + FUN_POINTER_LOWTAG;
#endif
	return 1;
    }
    else return 0;
}

#ifndef LISP_FEATURE_X86
/* This function gets called from the SIGSEGV (for e.g. Linux or
 * OpenBSD) or SIGBUS (for e.g. FreeBSD) handler. Here we check
 * whether the signal was due to treading on the mprotect()ed zone -
 * and if so, arrange for a GC to happen. */
boolean
interrupt_maybe_gc(int signal, siginfo_t *info, void *void_context)
{
    os_context_t *context=(os_context_t *) void_context;

    if (!foreign_function_call_active
#ifndef LISP_FEATURE_GENCGC 
	/* nb: GENCGC on non-x86?  I really don't think so.  This
	 * happens every time */
	&& gc_trigger_hit(signal, info, context)
#endif
	) {
#ifndef LISP_FEATURE_GENCGC 
	clear_auto_gc_trigger();
#endif

	if (arch_pseudo_atomic_atomic(context)) {
	    /* don't GC during an atomic operation.  Instead, copy the 
	     * signal mask somewhere safe.  interrupt_handle_pending
	     * will detect pending_signal==0 and know to do a GC with the
	     * signal context instead of calling a Lisp-level handler */
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
	    lispobj *old_free_space=current_dynamic_space;
	    fake_foreign_function_call(context);
	    funcall0(SymbolFunction(MAYBE_GC));
	    undo_fake_foreign_function_call(context);
	    if(current_dynamic_space==old_free_space) 
		/* MAYBE-GC (as the name suggest) might not.  If it
		 * doesn't, it won't reset the GC trigger either, so we
		 * have to do it ourselves.  Put it near the end of
		 * dynamic space so we're not running into it continually
		 */
		set_auto_gc_trigger(DYNAMIC_SPACE_SIZE
				    -(u32)os_vm_page_size);
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

/*
 * what low-level signal handlers looked like before
 * undoably_install_low_level_interrupt_handler() got involved
 */
struct low_level_signal_handler_state {
    int was_modified;
    void (*handler)(int, siginfo_t*, void*);
} old_low_level_signal_handler_states[NSIG];

void
uninstall_low_level_interrupt_handlers_atexit(void)
{
    int signal;
    for (signal = 0; signal < NSIG; ++signal) {
	struct low_level_signal_handler_state
	    *old_low_level_signal_handler_state =
	    old_low_level_signal_handler_states + signal;
	if (old_low_level_signal_handler_state->was_modified) {
	    struct sigaction sa;
	    sa.sa_sigaction = old_low_level_signal_handler_state->handler;
	    sigemptyset(&sa.sa_mask);
	    sa.sa_flags = SA_SIGINFO | SA_RESTART; 
	    sigaction(signal, &sa, NULL);
	}
    }
}

/* Undoably install a special low-level handler for signal; or if
 * handler is SIG_DFL, remove any special handling for signal.
 *
 * The "undoably" aspect is because we also arrange with atexit() for
 * the handler to be restored to its old value. This is for tidiness:
 * it shouldn't matter much ordinarily, but it does remove a window
 * where e.g. memory fault signals (SIGSEGV or SIGBUS, which in
 * ordinary operation of SBCL are sent to the generational garbage
 * collector, then possibly onward to Lisp code) or SIGINT (which is
 * ordinarily passed to Lisp code) could otherwise be handled
 * bizarrely/brokenly because the Lisp code would try to deal with
 * them using machinery (like stream output buffers) which has already
 * been dismantled. */
void
undoably_install_low_level_interrupt_handler (int signal,
					      void handler(int,
							   siginfo_t*,
							   void*))
{
    struct sigaction sa;
    struct low_level_signal_handler_state *old_low_level_signal_handler_state =
	old_low_level_signal_handler_states + signal;

    if (0 > signal || signal >= NSIG) {
	lose("bad signal number %d", signal);
    }

    sa.sa_sigaction = handler;
    sigemptyset(&sa.sa_mask);
    sigaddset_blockable(&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO | SA_RESTART;
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    /* Signal handlers are run on the control stack, so if it is exhausted
     * we had better use an alternate stack for whatever signal tells us
     * we've exhausted it */
    if(signal==SIG_MEMORY_FAULT) {
	stack_t sigstack;
	sigstack.ss_sp=(void *) ALTERNATE_SIGNAL_STACK_START;
	sigstack.ss_flags=0;
	sigstack.ss_size = SIGSTKSZ;
	sigaltstack(&sigstack,0);
	sa.sa_flags|=SA_ONSTACK;
    }
#endif
    
    /* In the case of interrupt handlers which are modified more than
     * once, we only save the original unmodified copy. */
    if (!old_low_level_signal_handler_state->was_modified) {
	struct sigaction *old_handler =
	    (struct sigaction*) &old_low_level_signal_handler_state->handler;
	old_low_level_signal_handler_state->was_modified = 1;
	sigaction(signal, &sa, old_handler);
    } else {
	sigaction(signal, &sa, NULL);
    }

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
    sigprocmask(SIG_BLOCK, &new, &old);

    sigemptyset(&new);
    sigaddset_blockable(&new);

    FSHOW((stderr, "/interrupt_low_level_handlers[signal]=%d\n",
	   interrupt_low_level_handlers[signal]));
    if (interrupt_low_level_handlers[signal]==0) {
	if (ARE_SAME_HANDLER(handler, SIG_DFL) ||
	    ARE_SAME_HANDLER(handler, SIG_IGN)) {
	    sa.sa_sigaction = handler;
	} else if (sigismember(&new, signal)) {
	    sa.sa_sigaction = maybe_now_maybe_later;
	} else {
	    sa.sa_sigaction = interrupt_handle_now_handler;
	}

	sigemptyset(&sa.sa_mask);
	sigaddset_blockable(&sa.sa_mask);
	sa.sa_flags = SA_SIGINFO | SA_RESTART;

	sigaction(signal, &sa, NULL);
    }

    oldhandler = interrupt_handlers[signal];
    interrupt_handlers[signal].c = handler;

    sigprocmask(SIG_SETMASK, &old, 0);

    FSHOW((stderr, "/leaving POSIX install_handler(%d, ..)\n", signal));

    return (unsigned long)oldhandler.lisp;
}

void
interrupt_init(void)
{
    int i;

    SHOW("entering interrupt_init()");

    /* Set up for recovery from any installed low-level handlers. */
    atexit(&uninstall_low_level_interrupt_handlers_atexit);

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
