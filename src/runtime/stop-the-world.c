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

#include "genesis/thread.h"
#include "arch.h"
#include "gc-assert.h"
#include "gc.h"
#include "pseudo-atomic.h"
#include "interrupt.h"
#include "lispregs.h"
#include "atomiclog.inc"

#ifdef LISP_FEATURE_SB_THREAD

#ifdef LISP_FEATURE_SUNOS
#include <thread.h>
#endif
#endif

#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
#include <stdatomic.h>
atomic_bool stopping_the_world = 0;

#endif

#ifdef LISP_FEATURE_SB_THREAD

#define get_thread_state(thread) \
 (int)__sync_val_compare_and_swap(&thread->state_word.state, -1, -1)

#if THREADS_USING_GCSIGNAL
void
set_thread_state(struct thread *thread,
                 char state,
                 bool signals_already_blocked) // for foreign thread
{
    struct extra_thread_data *semaphores = thread_extra_data(thread);
    int i, waitcount = 0;
    sigset_t old;
    // If we've already masked the blockable signals we can avoid two syscalls here.
    if (!signals_already_blocked)
        block_blockable_signals(&old);
    os_sem_wait(&semaphores->state_sem);
    if (thread->state_word.state != state) {
        if ((STATE_STOPPED==state) ||
            (STATE_DEAD==state)) {
            waitcount = semaphores->state_not_running_waitcount;
            semaphores->state_not_running_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(&semaphores->state_not_running_sem);
        }
        if ((STATE_RUNNING==state) ||
            (STATE_DEAD==state)) {
            waitcount = semaphores->state_not_stopped_waitcount;
            semaphores->state_not_stopped_waitcount = 0;
            for (i=0; i<waitcount; i++)
                os_sem_post(&semaphores->state_not_stopped_sem);
        }
        thread->state_word.state = state;
    }
    os_sem_post(&semaphores->state_sem);
    if (!signals_already_blocked)
        thread_sigmask(SIG_SETMASK, &old, NULL);
}

// Wait until "thread's" state is something other than 'undesired_state'
// and return whatever the new state is.
int thread_wait_until_not(int undesired_state,
                          struct thread *thread)
{
    struct extra_thread_data *semaphores = thread_extra_data(thread);
    sigset_t old;
    os_sem_t *wait_sem;
    block_blockable_signals(&old);
  start:
    os_sem_wait(&semaphores->state_sem);
    /* "The following functions synchronize memory with respect to other threads:
     *  ... pthread_mutex_lock() ... "
     * https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap04.html#tag_04_11
     * But we still have to ensure no compiler reordering.
     */
    int ending_state = get_thread_state(thread);
    if (ending_state == undesired_state) {
        switch (undesired_state) {
        case STATE_RUNNING:
            wait_sem = &semaphores->state_not_running_sem;
            semaphores->state_not_running_waitcount++;
            break;
        case STATE_STOPPED:
            wait_sem = &semaphores->state_not_stopped_sem;
            semaphores->state_not_stopped_waitcount++;
            break;
        default:
            lose("thread_wait_until_not: invalid argument %x", ending_state);
        }
    } else {
        wait_sem = NULL;
    }
    os_sem_post(&semaphores->state_sem);
    if (wait_sem) {
        os_sem_wait(wait_sem);
        goto start;
    }
    thread_sigmask(SIG_SETMASK, &old, NULL);
    return ending_state;
}

/* This function must not cons, because that may trigger a GC. */
void
sig_stop_for_gc_handler(int __attribute__((unused)) signal,
                        siginfo_t __attribute__((unused)) *info,
                        os_context_t *context)
{

#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
    /* The stop signal was already processed by
       handle_foreign_call_trigger */
    if (!atomic_load(&stopping_the_world))
        return;
#endif

    struct thread *thread = get_sb_vm_thread();
    bool was_in_lisp;
    /* Test for GC_INHIBIT _first_, else we'd trap on every single
     * pseudo atomic until gc is finally allowed. */
    if (read_TLS(GC_INHIBIT,thread) != NIL) {
        event0("stop_for_gc deferred for *GC-INHIBIT*");
        write_TLS(STOP_FOR_GC_PENDING, LISP_T, thread);
        return;
    } else if (arch_pseudo_atomic_atomic(thread)) {
        event0("stop_for_gc deferred for PA");
        write_TLS(STOP_FOR_GC_PENDING, LISP_T, thread);
        arch_set_pseudo_atomic_interrupted(thread);
        maybe_save_gc_mask_and_block_deferrables(context);
        return;
    }
    event0("stop_for_gc");

    if (!thread->state_word.control_stack_guard_page_protected) {
        protect_control_stack_return_guard_page(0, thread);
    }

    /* Not PA and GC not inhibited -- we can stop now. */

    was_in_lisp = !foreign_function_call_active_p(thread);

    if (was_in_lisp) {
        /* need the context stored so it can have registers scavenged */
        fake_foreign_function_call(context);
    }

    /* Not pending anymore. */
    write_TLS(GC_PENDING,NIL,thread);
    write_TLS(STOP_FOR_GC_PENDING,NIL,thread);

    /* Consider this: in a PA section GC is requested: GC_PENDING,
     * pseudo_atomic_interrupted and gc_blocked_deferrables are set,
     * deferrables are blocked then pseudo_atomic_atomic is cleared,
     * but a SIG_STOP_FOR_GC arrives before trapping to
     * interrupt_handle_pending. Here, GC_PENDING is cleared but
     * pseudo_atomic_interrupted is not and we go on running with
     * pseudo_atomic_interrupted but without a pending interrupt or
     * GC. GC_BLOCKED_DEFERRABLES is also left at 1. So let's tidy it
     * up. */
    if (thread_interrupt_data(thread).gc_blocked_deferrables) {
        event0("cleaning up after gc_blocked_deferrables");
        clear_pseudo_atomic_interrupted(thread);
        struct interrupt_data *interrupt_data = &thread_interrupt_data(thread);
        sigcopyset(os_context_sigmask_addr(context), &interrupt_data->pending_mask);
        interrupt_data->gc_blocked_deferrables = 0;
    }

    /* No need to use an atomic memory load here - this thead "owns" its state
     * for now, and nobody else touches it, the sole exception being that GC
     * sets it to RUNNING. The loads inside thread_wait_until_not()
     * are slightly more interesting from that perspective */
    if (thread->state_word.state != STATE_RUNNING)
        lose("stop_for_gc: bad thread state: %x", (int)thread->state_word.state);

#ifdef MEASURE_STOP_THE_WORLD_PAUSE
    struct timespec t_beginpause;
    clock_gettime(CLOCK_MONOTONIC, &t_beginpause);
#endif

    /* We say that the thread is "stopped" as of now, but the blocking operation
     * occurs below at thread_wait_until_not(STATE_STOPPED). Note that sem_post()
     * is expressly permitted in signal handlers, and set_thread_state uses it */
    set_thread_state(thread, STATE_STOPPED, 0);
    event0("suspended");

    /* While waiting for gc to finish occupy ourselves with zeroing
     * the unused portion of the control stack to reduce conservatism.
     * On the platforms with threads and exact gc it is
     * actually a must. */
    scrub_control_stack();

    /* Now we wait on a semaphore, which, to be pedantic, is not specified as async-safe.
     * Normally the way to implement a "suspend" operation is to issue any blocking
     * syscall such as sigsuspend() or select(). Apparently every OS + C runtime that
     * we wish to support has no problem with sem_wait() here in the signal handler. */

    int my_state = thread_wait_until_not(STATE_STOPPED, thread);
    thread_accrue_stw_time(thread, &t_beginpause, 0);

    if (!thread->state_word.control_stack_guard_page_protected) {
        protect_control_stack_return_guard_page(1, thread);
    }

#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
    /* Might have been blocked in handle_foreign_call_trigger */
    sigdelset(os_context_sigmask_addr(context), SIG_STOP_FOR_GC);
#endif    

    event0("resumed");

    /* The state can't go from STOPPED to DEAD because it's this thread is reading
     * its own state, hence it must be running.
     * (If we tried to observe a different thread, it could appear to change from
     * STOPPED to DEAD, skipping RUNNING, because if you blink you might miss it) */
    if (my_state != STATE_RUNNING)
        lose("stop_for_gc: bad state on wakeup: %x", my_state);

    if (was_in_lisp) {
        undo_fake_foreign_function_call(context);
    }
}

#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL

lispobj set_thread_foreign_call_trigger(struct thread* th, bool writable)
{
    os_protect((char*)th - THREAD_CSP_PAGE_SIZE,
               THREAD_CSP_PAGE_SIZE,
               writable? (OS_VM_PROT_READ|OS_VM_PROT_WRITE)
               : (OS_VM_PROT_READ));
    return csp_around_foreign_call(th);
}

int
handle_foreign_call_trigger (os_context_t *context, os_vm_address_t fault_address)
{
    struct thread *th = get_sb_vm_thread();

    if ((lispobj*)fault_address == &csp_around_foreign_call(th)) {
        int exiting = csp_around_foreign_call(th) != 0;

        if (read_TLS(GC_INHIBIT,th) == NIL) {
            if (exiting) {
                /* gc_stop_the_world has left this thread untouched,
                   wait for gc_start_the_world */
                pthread_mutex_t *exit_lock = &thread_extra_data(th)->foreign_exit_lock;
                gc_assert(mutex_acquire(exit_lock));
                gc_assert(mutex_release(exit_lock));
            }
            else {
                /* gc_stop_the_world has either already sent a signal
                   or will send it soon, wait for it. */
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
                th->control_stack_pointer = (lispobj*)*os_context_register_addr(context, reg_SP);
#endif
                /* sigsuspend appears to be broken on macOS, call the
                   handler directly and then ignore it outside of stop_the_world */
                sig_stop_for_gc_handler(0, NULL, context);
            }
        } else { /* Inside without-gcing */
            /* Unprotect the page to be able to proceed */
            set_thread_foreign_call_trigger(th, 1);
            /* Stop upon exiting from without-gcing */
            if (exiting) {
                pthread_mutex_t *exit_lock = &thread_extra_data(th)->foreign_exit_lock;
                write_TLS(STOP_FOR_GC_PENDING, LISP_T, th);
                gc_assert(mutex_acquire(exit_lock));
                gc_assert(mutex_release(exit_lock));
            } else {
                if (read_TLS(STOP_FOR_GC_PENDING, th) == NIL) {
                    /* Block SIG_STOP_FOR_GC and exit,
                     * interrupt_handle_pending will reset it */
                    sigaddset(os_context_sigmask_addr(context),SIG_STOP_FOR_GC);
                    write_TLS(STOP_FOR_GC_PENDING, LISP_T, th);
                }

            }
        }
        return 1;
    }
    return 0;
}
#endif
#endif

/* stopping the world is a two-stage process.  From this thread we signal
 * all the others with SIG_STOP_FOR_GC.  The handler for this signal does
 * the usual pseudo-atomic checks (we don't want to stop a thread while
 * it's in the middle of allocation) then waits for another SIG_STOP_FOR_GC.
 */
/*
 * (With SB-SAFEPOINT, see the definitions in safepoint.c instead.)
 */
#if HAVE_GC_STW_SIGNAL

/* To avoid deadlocks when gc stops the world all clients of each
 * mutex must enable or disable SIG_STOP_FOR_GC for the duration of
 * holding the lock, but they must agree on which.
 * [The preceding remark is probably wrong - STOP_FOR_GC is a signal
 * that is directed to a thread, so the "wrong" thread would never
 * respond to someone else's STOP_FOR_GC. I'm leaving the comment
 * just case someone can decipher it and decide to delete it]
 *
 * A note about ESRCH: tchnically ESRCH can happen if an OS thread ceases
 * to exist, while the thread library has a representation of the thread
 * because pthread_join() wasn't invoked on it yet.
 * ESRCH can't oocur for us because:
 * - if a thread was still linked in all_threads at the acquire of all_threads lock,
 *   then that thread can't make progress in its termination code, because it's
 *   waiting on the lock. If it changed its state to DEAD, but we perceived it as
 *   RUNNING, it now must be blocked on the all_threads_lock and it can't disappear.
 * - ESRCH is not guaranteed to be returned anyway, and Linux man page doesn't even
 *   list it as a possible outcome of pthread_kill.
 * Also, there used to be assertion that "thread_state(p)==STATE_DEAD)" on ESRCH
 * error, but that's saying that there is still memory backing 'struct thread'
 * (so that dereferencing was valid), but if dereferencing was valid, then the thread
 * can't have died (i.e. if ESRCH could be returned, then that implies that
 * the memory shouldn't be there) */

static __attribute__((unused)) struct timespec stw_begin_realtime, stw_begin_cputime;
void gc_stop_the_world()
{
#ifdef MEASURE_STOP_THE_WORLD_PAUSE
    /* The thread performing stop-the-world does not use sig_stop_for_gc_handler on itself,
     * so it would not accrue time spent stopped. Force it to, by considering it "paused"
     * from the moment it wants to stop all other threads. */
    clock_gettime(CLOCK_MONOTONIC, &stw_begin_realtime);
# ifdef CLOCK_THREAD_CPUTIME_ID
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, &stw_begin_cputime);
# endif
#endif
    struct thread *th, *me = get_sb_vm_thread();
    int rc;

    /* Keep threads from registering with GC while the world is stopped. */
    rc = mutex_acquire(&all_threads_lock);
    gc_assert(rc);

#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
    atomic_store(&stopping_the_world, 1);
#endif

    /* stop all other threads by sending them SIG_STOP_FOR_GC */
    for_each_thread(th) {
        if (th != me) {
            gc_assert(th->os_thread != 0);
            struct extra_thread_data *semaphores = thread_extra_data(th);
            bool foreign = 0;
#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
            pthread_mutex_t *exit_lock = &semaphores->foreign_exit_lock;
            gc_assert(mutex_acquire(exit_lock));
            lispobj csp = set_thread_foreign_call_trigger(th, 0);
            foreign = csp != 0;
#endif

            os_sem_wait(&semaphores->state_sem);
            int state = get_thread_state(th);
            if (state == STATE_RUNNING) {
                if (!foreign) {
                    rc = pthread_kill(th->os_thread,SIG_STOP_FOR_GC);
                    /* This used to bogusly check for ESRCH.
                     * I changed the ESRCH case to just fall into lose() */
                    if (rc) lose("cannot suspend thread %p: %d, %s",
                                 // KLUDGE: assume that os_thread can be cast as pointer.
                                 // See comment in 'interr.h' about that.
                                 (void*)th->os_thread, rc, strerror(rc));

                }
#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
                else {
                    if (read_TLS(GC_INHIBIT,th) != NIL) {
                        /* It's a foreign call but inside without-gcing,
                           handle_foreign_call_trigger will set
                           *stop-for-gc-pending*, need to unlock the exit
                           lock to reach the end of without-gcing* */
                        semaphores->gc_inhibited = 2;
                        gc_assert(mutex_release(exit_lock));
                    } else {
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
                        th->control_stack_pointer = (lispobj*)csp;
#endif
                        semaphores->gc_inhibited = 1;
                    }
                }
#endif
            }
            os_sem_post(&semaphores->state_sem);
        }
    }
    for_each_thread(th) {
        if (th != me) {
#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
            if (thread_extra_data(th)->gc_inhibited != 1)
#endif
            {
                thread_wait_until_not(STATE_RUNNING, th);
            }
        }
    }
#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
    atomic_store(&stopping_the_world, 0);
#endif
    event0("/gc_stop_the_world:end");
}

void gc_start_the_world()
{
#ifdef COLLECT_GC_STATS
    struct timespec gc_end_time;
    clock_gettime(CLOCK_MONOTONIC, &gc_end_time);
    long gc_elapsed = (gc_end_time.tv_sec - gc_start_time.tv_sec)*1000000000L
                      + (gc_end_time.tv_nsec - gc_start_time.tv_nsec);
    if (stw_elapsed < 0 || gc_elapsed < 0) {
        char errmsg[] = "GC: Negative times?\n";
        ignore_value(write(2, errmsg, sizeof errmsg-1));
    } else {
        stw_sum_duration += stw_elapsed;
        if (stw_elapsed < stw_min_duration) stw_min_duration = stw_elapsed;
        if (stw_elapsed > stw_max_duration) stw_max_duration = stw_elapsed;
        gc_sum_duration += gc_elapsed;
        if (gc_elapsed < gc_min_duration) gc_min_duration = gc_elapsed;
        if (gc_elapsed > gc_max_duration) gc_max_duration = gc_elapsed;
        ++n_gcs_done;
    }
#endif
    struct thread *th, *me = get_sb_vm_thread();
    __attribute__((unused)) int lock_ret;
    /* if a resumed thread creates a new thread before we're done with
     * this loop, the new thread will be suspended waiting to acquire
     * the all_threads lock */
    for_each_thread(th) {
        gc_assert(th->os_thread);
        if (th != me) {
            /* I don't know if a normal load is fine here. I think we can't read
             * any value other than what was already observed?
             * No harm in being cautious though with regard to compiler reordering */
            int state = get_thread_state(th);
#ifdef LISP_FEATURE_NONSTOP_FOREIGN_CALL
            bool foreign = csp_around_foreign_call(th);
            set_thread_foreign_call_trigger(th, 1);

            if (thread_extra_data(th)->gc_inhibited != 2) // already released
                gc_assert(mutex_release(&thread_extra_data(th)->foreign_exit_lock));

            thread_extra_data(th)->gc_inhibited = 0;

            if (foreign) {
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
                th->control_stack_pointer = 0;
#endif
            }
            else
#endif
            {
                if (state != STATE_DEAD) {
                    if (state != STATE_STOPPED)
                        lose("gc_start_the_world: bad thread state %x", state);
                    set_thread_state(th, STATE_RUNNING, 0);
                }
            }
        }
    }

    lock_ret = mutex_release(&all_threads_lock);
    gc_assert(lock_ret);
    thread_accrue_stw_time(me, &stw_begin_realtime, &stw_begin_cputime);
}

#endif /* !LISP_FEATURE_SB_SAFEPOINT */

#else
// no threads
void gc_stop_the_world() {}
void gc_start_the_world() {}
#endif /* !LISP_FEATURE_SB_THREAD */
