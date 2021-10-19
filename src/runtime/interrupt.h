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

#if !defined(_INCLUDE_INTERRUPT_H_)
#define _INCLUDE_INTERRUPT_H_

#include "runtime.h"
#include <string.h>
#include "genesis/static-symbols.h"

extern void sigset_tostring(const sigset_t *sigset, char* result, int result_length);

/* Set all blockable signals into *s. */
extern void sigaddset_blockable(sigset_t *s);

extern sigset_t deferrable_sigset;
extern sigset_t blockable_sigset;
extern sigset_t gc_sigset;
extern sigset_t thread_start_sigset;

extern boolean deferrables_blocked_p(sigset_t *sigset);

extern void check_deferrables_blocked_or_lose(sigset_t *sigset);

extern void check_deferrables_unblocked_or_lose(sigset_t *sigset);
extern void check_gc_signals_unblocked_or_lose(sigset_t *sigset);

extern void block_deferrable_signals(sigset_t *old);
extern void block_blockable_signals(sigset_t *old);

extern void unblock_deferrable_signals(sigset_t *where);
extern void unblock_gc_signals(void);

extern void maybe_save_gc_mask_and_block_deferrables(sigset_t *sigset);

/* maximum signal nesting depth
 *
 * FIXME: In CMUCL this was 4096, and it was first scaled down to 256
 * and then 32, until finally Stumpwm broke: it is possible to receive
 * interrupts in sufficiently quick succession that handler nesting
 * can become preposterous. Scaling this back up to 1024 is a bandaid:
 * for a real fix we should consider the following things:
 *
 *   We should almost certainly always use
 *   arrange_return_to_lisp_function, though it needs to be thought
 *   about arguments, and it needs to be able to pass a frobbable
 *   context to the callee...
 *
 *   There are cases when nesting handlers is exactly what we want:
 *   eg. SIGINT.
 *
 *   There are cases when we probably want to drop duplicate signals
 *   on the floor if they arrive before the previous one has been handled.
 *   Eg. SIGPROF.
 *
 *   There are cases when we probably want to handle duplicate signals
 *   after the previous handler has returned, not before. Eg. SIGALARM.
 *
 * -- NS 2007-01-29
 */
extern lispobj lisp_sig_handlers[NSIG];

struct interrupt_data {
    /* signal information for pending signal.  pending_signal=0 when there
     * is no pending signal. */
    void (*pending_handler) (int, siginfo_t*, os_context_t*) ;
    int pending_signal;
    siginfo_t pending_info;
    sigset_t pending_mask;
    /* Was pending mask saved for gc request? True if GC_PENDING or
     * SIG_STOP_FOR_GC happened in a pseudo atomic with GC_INHIBIT NIL
     * and with no pending handler. Both deferrable interrupt handlers
     * and gc are careful not to clobber each other's pending_mask. */
    boolean gc_blocked_deferrables;
#if GENCGC_IS_PRECISE
    /* On PPC when consing wants to turn to alloc(), it does so via a
     * trap. When alloc() wants to save the sigmask it consults
     * allocation_trap_context. It does not look up the most recent
     * context, because alloc() can be called from other places
     * too. */
    os_context_t *allocation_trap_context;
#endif
};

typedef lispobj (*call_into_lisp_lookalike)(
    lispobj fun, lispobj *args, int nargs);

extern boolean interrupt_handler_pending_p(void);
extern void interrupt_init(void);
extern void fake_foreign_function_call(os_context_t* context);
extern void undo_fake_foreign_function_call(os_context_t* context);
extern void arrange_return_to_c_function(
    os_context_t *, call_into_lisp_lookalike, lispobj);
extern void arrange_return_to_lisp_function(os_context_t *, lispobj);
extern void interrupt_handle_now(int, siginfo_t*, os_context_t*);
extern void interrupt_handle_pending(os_context_t*);
extern void interrupt_internal_error(os_context_t*, boolean continuable);
extern boolean handle_guard_page_triggered(os_context_t *,os_vm_address_t);

#ifdef DO_PENDING_INTERRUPT
#define do_pending_interrupt ((void(*)(void))SYMBOL(DO_PENDING_INTERRUPT)->value)
#elif defined(LISP_FEATURE_GENCGC)
/* assembly language stub that executes trap_PendingInterrupt */
extern void do_pending_interrupt(void);
#endif

#ifdef LISP_FEATURE_SB_THREAD
extern void sig_stop_for_gc_handler(int, siginfo_t*, os_context_t*);
#endif
typedef void (*interrupt_handler_t)(int, siginfo_t *, os_context_t *);
extern void ll_install_handler(int signal, interrupt_handler_t handler);

/* The void* casting here avoids having to mess with the various types
 * of function argument lists possible for signal handlers:
 * SA_SIGACTION handlers have one signature, and the default old-style
 * signal(..) handlers have another, and attempting to represent them
 * "cleanly" with union types is in fact a mess. */
#define ARE_SAME_HANDLER(x, y) ((void*)(x) == (void*)(y))

extern void handle_trap(os_context_t *context, int trap);

#ifndef LISP_FEATURE_WIN32
extern void lisp_memory_fault_error(os_context_t *context,
                                    os_vm_address_t addr);
#endif

#include "thread.h"

extern void lower_thread_control_stack_guard_page(struct thread *th);
extern void reset_thread_control_stack_guard_page(struct thread *th);

#if defined(LISP_FEATURE_SB_SAFEPOINT) && !defined(LISP_FEATURE_WIN32)
# ifdef LISP_FEATURE_SB_SAFEPOINT
void thruption_handler(int signal, siginfo_t *info, os_context_t *context);
# endif
#endif

#endif
