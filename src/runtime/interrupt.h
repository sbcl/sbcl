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

/* maximum signal nesting depth
 *
 * Note: In CMU CL, this was 4096, but there was no explanation given,
 * and it's hard to see why we'd need that many nested interrupts, so
 * I've scaled it back (to 256) to see what happens. -- WHN 20000730 

 * Nothing happened, so let's creep it back a bit further -- dan 20030411 */
#define MAX_INTERRUPTS 32

union interrupt_handler {
    lispobj lisp;
    void (*c)(int, siginfo_t*, void*);
};

struct interrupt_data {
    void (*interrupt_low_level_handlers[NSIG]) (int, siginfo_t*, void*) ;
    union interrupt_handler interrupt_handlers[NSIG];

    /* signal information for pending signal.  pending_signal=0 when there 
     * is no pending signal. */
    void (*pending_handler) (int, siginfo_t*, void*) ;
    int pending_signal ;
    siginfo_t pending_info;
    sigset_t pending_mask;
};


extern void interrupt_init();
extern void fake_foreign_function_call(os_context_t* context);
extern void undo_fake_foreign_function_call(os_context_t* context);
extern void interrupt_handle_now(int, siginfo_t*, void*);
extern void interrupt_handle_pending(os_context_t*);
extern void interrupt_internal_error(int, siginfo_t*, os_context_t*,
				     boolean continuable);
extern boolean handle_control_stack_guard_triggered(os_context_t *,void *);
extern boolean interrupt_maybe_gc(int, siginfo_t*, void*);
#ifdef LISP_FEATURE_SB_THREAD
extern void interrupt_thread_handler(int, siginfo_t*, void*);
extern void sig_stop_for_gc_handler(int, siginfo_t*, void*);
#endif
extern void undoably_install_low_level_interrupt_handler (int signal,
							  void
							  handler(int,
								  siginfo_t*,
								  void*));
extern unsigned long install_handler(int signal,
				     void handler(int, siginfo_t*, void*));

extern union interrupt_handler interrupt_handlers[NSIG];

/* Set all blockable signals into *s. */
void sigaddset_blockable(sigset_t *s);

/* The void* casting here avoids having to mess with the various types
 * of function argument lists possible for signal handlers:
 * SA_SIGACTION handlers have one signature, and the default old-style
 * signal(..) handlers have another, and attempting to represent them
 * "cleanly" with union types is in fact a mess. */
#define ARE_SAME_HANDLER(x, y) ((void*)(x) == (void*)(y))

#endif

