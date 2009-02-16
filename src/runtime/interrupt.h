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

#include <signal.h>
#include <string.h>

/*
 * This is a workaround for some slightly silly Linux/GNU Libc
 * behaviour: glibc defines sigset_t to support 1024 signals, which is
 * more than the kernel.  This is usually not a problem, but becomes
 * one when we want to save a signal mask from a ucontext, and restore
 * it later into another ucontext: the ucontext is allocated on the
 * stack by the kernel, so copying a libc-sized sigset_t into it will
 * overflow and cause other data on the stack to be corrupted */
/* FIXME: do not rely on NSIG being a multiple of 8 */
#define REAL_SIGSET_SIZE_BYTES ((NSIG/8))

extern sigset_t deferrable_sigset;
extern sigset_t blockable_sigset;

extern void check_deferrables_blocked_or_lose(void);
extern void check_blockables_blocked_or_lose(void);
extern void check_gc_signals_unblocked_or_lose(void);
extern void unblock_gc_signals(void);

static inline void
sigcopyset(sigset_t *new, sigset_t *old)
{
    memcpy(new, old, REAL_SIGSET_SIZE_BYTES);
}

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
#define MAX_INTERRUPTS 1024

union interrupt_handler {
    lispobj lisp;
    void (*c)(int, siginfo_t*, void*);
};

extern union interrupt_handler interrupt_handlers[NSIG];

struct interrupt_data {
    /* signal information for pending signal.  pending_signal=0 when there
     * is no pending signal. */
    void (*pending_handler) (int, siginfo_t*, void*) ;
    int pending_signal;
    siginfo_t pending_info;
    sigset_t pending_mask;
};


extern void interrupt_init(void);
extern void fake_foreign_function_call(os_context_t* context);
extern void undo_fake_foreign_function_call(os_context_t* context);
extern void arrange_return_to_lisp_function(os_context_t *, lispobj);
extern void interrupt_handle_now(int, siginfo_t*, os_context_t*);
extern void interrupt_handle_pending(os_context_t*);
extern void interrupt_internal_error(os_context_t*, boolean continuable);
extern boolean handle_guard_page_triggered(os_context_t *,os_vm_address_t);
extern boolean maybe_defer_handler(void *handler, struct interrupt_data *data,
                                   int signal, siginfo_t *info,
                                   os_context_t *context);
#if defined LISP_FEATURE_GENCGC
/* assembly language stub that executes trap_PendingInterrupt */
extern void do_pending_interrupt(void);
#endif

#ifdef LISP_FEATURE_SB_THREAD
extern void interrupt_thread_handler(int, siginfo_t*, void*);
extern void sig_stop_for_gc_handler(int, siginfo_t*, void*);
#endif
typedef void (*interrupt_handler_t)(int, siginfo_t *, void *);
extern void undoably_install_low_level_interrupt_handler (
                        int signal,
                        interrupt_handler_t handler);
extern unsigned long install_handler(int signal,
                                     interrupt_handler_t handler);

extern union interrupt_handler interrupt_handlers[NSIG];

/* Set all deferrable signals into *s. */
extern void sigaddset_deferrable(sigset_t *s);
/* Set all blockable signals into *s. */
extern void sigaddset_blockable(sigset_t *s);

extern void block_blockable_signals(void);
extern void unblock_deferrable_signals(void);

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

#endif
