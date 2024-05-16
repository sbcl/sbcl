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
 * o the SIGTRAP which Lisp code may uses to handle breakpoints,
 *   pseudo-atomic sections, and some classes of error (e.g. "function
 *   not defined").  This never goes anywhere near the Lisp handlers at all.
 *   See src/code/signal.lisp
 *
 * - WHN 20000728, dan 20010128 */

#include "genesis/sbcl.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <sys/types.h>
#ifndef LISP_FEATURE_WIN32
#include <sys/wait.h>
#endif
#include <errno.h>
#ifdef MEASURE_STOP_THE_WORLD_PAUSE
#include <time.h>
#endif

#include "runtime.h"
#include "arch.h"
#include "code.h"
#include "os.h"
#include "interrupt.h"
#include "globals.h"
#include "lispregs.h"
#include "validate.h"
#include "interr.h"
#include "gc.h"
#include "genesis/sap.h"
#include "pseudo-atomic.h"
#include "genesis/symbol.h"
#include "genesis/cons.h"
#include "genesis/vector.h"
#include "genesis/thread.h"
#include "atomiclog.inc"

#ifdef ATOMIC_LOGGING
uword_t *eventdata;
int n_logevents;
#endif

#ifdef ADDRESS_SANITIZER
#include <sanitizer/asan_interface.h>
#endif

/*
 * This is a workaround for some slightly silly Linux/GNU Libc
 * behaviour: glibc defines sigset_t to support 1024 signals, which is
 * more than the kernel.  This is usually not a problem, but becomes
 * one when we want to save a signal mask from a ucontext, and restore
 * it later into another ucontext: the ucontext is allocated on the
 * stack by the kernel, so copying a libc-sized sigset_t into it will
 * overflow and cause other data on the stack to be corrupted */
/* See https://sourceware.org/bugzilla/show_bug.cgi?id=1780 */

#ifdef LISP_FEATURE_WIN32
# define REAL_SIGSET_SIZE_BYTES (4)
#else
/* FIXME: do not rely on NSIG being a multiple of 8.
 * In fact it is *not* a multiple of 8 - it it 65 on x86-64-linux */
# define REAL_SIGSET_SIZE_BYTES ((NSIG/8))
#endif

#ifdef LISP_FEATURE_NETBSD
#define OS_SA_NODEFER 0
#else
#define OS_SA_NODEFER SA_NODEFER
#endif

static inline void sigcopyset(sigset_t *to, sigset_t *from) {
#ifdef ADDRESS_SANITIZER
    sigemptyset(to);
#endif
    memcpy(to, from, REAL_SIGSET_SIZE_BYTES);
}

/* When we catch an internal error, should we pass it back to Lisp to
 * be handled in a high-level way? (Early in cold init, the answer is
 * 'no', because Lisp is still too brain-dead to handle anything.
 * After sufficient initialization has been completed, the answer
 * becomes 'yes'.) */
int internal_errors_enabled = 0; // read in cold-init

// SIGRTMAX is not usable in an array size declaration because it might be
// a variable expression, so use NSIG which is at least as large as SIGRTMAX.
#ifndef LISP_FEATURE_WIN32
static
void (*interrupt_low_level_handlers[NSIG]) (int, siginfo_t*, os_context_t*);
struct sigaction old_ll_sigactions[NSIG];
#endif
lispobj lisp_sig_handlers[NSIG];

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

/* The code below wants to iterate from 1 through SIGRTMAX inclusive, not NSIG,
 * because NSIG is the maximum capacity of a sigset, whereas SIGRTMAX is a
 * possibly smaller value indicating the number of signal numbers that could
 * really be in use. For systems which have distinctly different values
 * (discounting the deliberate off-by-one nature), sigismember() may return -1
 * for signal numbers in excess of SIGRTMAX, and we would generally take
 * -1 to mean boolean 'true' unless it is carefully checked for.
 * The following program illustrates the issue:
 *    #include <signal.h>
 *    #include <stdio.h>
 *    void main() {
 *      sigset_t ss;
 *      sigemptyset(&ss);
 *      printf("%d %d %d\n", SIGRTMAX, NSIG, sigismember(&ss, 41));
 *    }
 *  ./sigismembertest => 40 65 -1
 */
#ifdef SIGRTMAX
#  define MAX_SIGNUM SIGRTMAX
#else
#  define MAX_SIGNUM (NSIG-1)
#endif

// For each bit present in 'source', add it to 'dest'.
// This is the same as "sigorset(dest, dest, source)" if _GNU_SOURCE is defined.
static void sigmask_logior(sigset_t *dest, const sigset_t *source)
{
    int i;
    for(i = 1; i <= MAX_SIGNUM; i++) {
        if (sigismember(source, i)) sigaddset(dest, i);
    }
}

// For each bit present in 'source', remove it from 'dest'
// (logically AND with logical complement).
static void sigmask_logandc(sigset_t *dest, const sigset_t *source)
{
    int i;
    for(i = 1; i <= MAX_SIGNUM; i++) {
        if (sigismember(source, i)) sigdelset(dest, i);
    }
}

/* Foreign code may want to start some threads on its own.
 * Non-targetted, truly asynchronous signals can be delivered to
 * basically any thread, but invoking Lisp handlers in such foreign
 * threads is really bad, so let's resignal it.
 *
 * This should at least bring attention to the problem, but it cannot
 * work for SIGSEGV and similar. It is good enough for timers, and
 * maybe all deferrables. */

#if defined LISP_FEATURE_DARWIN && defined LISP_FEATURE_SB_THREAD
pthread_key_t ignore_stop_for_gc;
#endif

#ifdef LISP_FEATURE_WIN32
#define resignal_to_lisp_thread(dummy1,dummy2) {}
#else
static void
resignal_to_lisp_thread(int signal, os_context_t *context)
{
    if (!sigismember(&deferrable_sigset,signal)) {
        corruption_warning_and_maybe_lose
#ifdef LISP_FEATURE_SB_THREAD
            ("Received signal %d @ %lx in non-lisp"THREAD_ID_LABEL", resignaling to a lisp thread.",
             signal, os_context_pc(context), THREAD_ID_VALUE);
#else
            ("Received signal %d in non-lisp thread, resignaling to a lisp thread.", signal);
#endif
    }
    sigset_t sigset;
    sigemptyset(&sigset);
    int i;
    for(i = 1; i < NSIG; i++) {
        // This use of SIG_DFL is a bit disingenous. It actually means "is it 0",
        // because the low_level_handlers array is never explicitly initialized
        // with SIG_DFL in each element, but SIG_DFL happens to be 0.
        if (!ARE_SAME_HANDLER(interrupt_low_level_handlers[i], SIG_DFL)
            || lisp_sig_handlers[i]) {
            sigaddset(&sigset, i);
        }
    }
    thread_sigmask(SIG_BLOCK, &sigset, 0);
    // This arranges for every handled signal to be blocked on return from this
    // handler invocation, presumably because that avoids further detours through
    // this thread's handler for signals that we don't want it to handle.
    sigmask_logior(os_context_sigmask_addr(context), &sigset);
    kill(getpid(), signal);
}
#endif

#if INSTALL_SIG_MEMORY_FAULT_HANDLER && defined(THREAD_SANITIZER)
/* Under TSAN, any delivered signal blocks all other signals regardless of the
 * 'sa_mask' given to sigaction(). This is courtesy of an interceptor -
 * https://github.com/llvm-mirror/compiler-rt/blob/bcc227ee4af1ef3e63033b35dcb1d5627a3b2941/lib/tsan/rtl/tsan_interceptors.cc#L1972
 *
 * So among other things, SIGSEGV is blocked on receipt of any random signal
 * of interest (SIGPROF, SIGALRM, SIGPIPE, ...) that might call Lisp code.
 * Therefore, if any handler re-enters Lisp, there is a high likelihood
 * of SIGSEGV being delivered while blocked. Unfortunately, the OS treats
 * blocked SIGSEGV exactly as if the specified disposition were SIG_DFL,
 * which results in process termination and a core dump.
 *
 * Our trick is to unblock SIGSEGV early in every handler,
 * so not to face sudden death if it happens to invoke Lisp.
 */
#  define UNBLOCK_SIGSEGV() \
  { sigset_t mask; sigemptyset(&mask); \
    sigaddset(&mask, SIG_MEMORY_FAULT); /* usually SIGSEGV */ \
    thread_sigmask(SIG_UNBLOCK, &mask, 0); }
#else
#  define UNBLOCK_SIGSEGV() {}
#endif

/* These are to be used in signal handlers. Currently all handlers are
 * called from one of:
 *
 * interrupt_handle_now_handler
 * maybe_now_maybe_later
 * low_level_handle_now_handler
 *
 * This gives us a single point of control (or three) over errno, fp
 * control word, and fixing up signal context on sparc.
 *
 * The SPARC/Linux platform doesn't quite do signals the way we want
 * them done. The third argument in the handler isn't filled in by the
 * kernel properly, so we fix it up ourselves in the
 * arch_os_get_context(..) function. -- CSR, 2002-07-23
 */
#ifdef ATOMIC_LOGGING
void dump_eventlog()
{
    int i = 0;
    uword_t *e = eventdata;
    char buf[1024];
    int nc, nc1; // number of chars in buffer
    // Define buflen to be smaller than 'buf' so that we can prefix it
    // with thread pointer and suffix it with a newline
    // without too much hassle.
#define buflen (sizeof buf-20)
    nc = snprintf(buf, buflen, "Event log: used %d elements of %d max\n", n_logevents, EVENTBUFMAX);
    write(2, buf, nc);
    while (i<n_logevents) {
        char *fmt = (char*)e[i+1];
        uword_t prefix = e[i];
        int nargs = prefix & 7;
        void* thread_pointer = (void*)(prefix & ~7);
        extern char* thread_name_from_pthread(void*);
        char* name = thread_name_from_pthread(thread_pointer);
        if (name) nc = sprintf(buf, "%s: ", name); else nc = sprintf(buf, "%p: ", thread_pointer);
        switch (nargs) {
        default: printf("busted event log"); return;
        case 0: nc1 = snprintf(buf+nc, buflen, fmt, 0); break; // the 0 inhibits a warning
        case 1: nc1 = snprintf(buf+nc, buflen, fmt, e[i+2]); break;
        case 2: nc1 = snprintf(buf+nc, buflen, fmt, e[i+2], e[i+3]); break;
        case 3: nc1 = snprintf(buf+nc, buflen, fmt, e[i+2], e[i+3], e[i+4]); break;
        case 4: nc1 = snprintf(buf+nc, buflen, fmt, e[i+2], e[i+3], e[i+4], e[i+5]); break;
        case 5: nc1 = snprintf(buf+nc, buflen, fmt, e[i+2], e[i+3], e[i+4], e[i+5], e[i+6]); break;
        case 6: nc1 = snprintf(buf+nc, buflen, fmt, e[i+2], e[i+3], e[i+4], e[i+5], e[i+6],
                               e[i+7]); break;
        }
#undef buflen
        buf[nc+nc1] = '\n';
        write(2, buf, 1+nc+nc1);
        i += nargs + 2;
    }
}
void sigdump_eventlog(int __attribute__((unused)) signal,
                      siginfo_t __attribute__((unused)) *info,
                      os_context_t *context)
{
    dump_eventlog();
}

static void record_signal(int sig, void* context)
{
    event2("got signal %d @ pc=%p", sig, os_context_pc(context));
}
#define RECORD_SIGNAL(sig,ctxt) if(sig!=SIGSEGV)record_signal(sig,ctxt);
#else
#define RECORD_SIGNAL(sig,ctxt)
#endif

#ifdef LISP_FEATURE_WIN32
# define should_handle_in_this_thread(c) (1)
#else
# define should_handle_in_this_thread(c) lisp_thread_p(c)
#endif
#define SAVE_ERRNO(signal,context,void_context)                 \
    {                                                           \
        int _saved_errno = errno;                               \
        RECORD_SIGNAL(signal,void_context);                     \
        UNBLOCK_SIGSEGV();                                      \
        RESTORE_FP_CONTROL_WORD(context,void_context);          \
        if (should_handle_in_this_thread(context)) {

#define RESTORE_ERRNO                                           \
        } else resignal_to_lisp_thread(signal,void_context);    \
        errno = _saved_errno;                                   \
    }

static void run_deferred_handler(struct interrupt_data *data,
                                 os_context_t *context);

/* Generic signal related utilities. */

// Stringify sigset into the supplied result buffer.
void
sigset_tostring(const sigset_t *sigset, char* result, int result_length)
{
    int i;
    int len = 0;
    if (!sigset) { strcpy(result,"nil"); return; }
    if (*(uint32_t*)sigset == 0xFFFFFFFF) { strcpy(result,"All"); return; }
    result[0] = '{';
    len = 1;
    for (i = 1; i <= MAX_SIGNUM; i++)
        if (sigismember(sigset, i)) {
            // ensure room for (generously) 3 digits + comma + null, or give up
            if (len > result_length - 5) {
                strcpy(result, "too many to list");
                return;
            }
            len += sprintf(result+len, "%s%d", len>1?",":"", i);
        }
    result[len] = '}';
    result[len+1] = 0;
}


/* Deferrables, blockables, gc signals. */

#ifdef LISP_FEATURE_SB_SAFEPOINT
static void sigaddset_deferrable(sigset_t *s) {
    sigaddset(s, SIGURG);
}
static void sigaddset_async(sigset_t *s) {
#else
static void sigaddset_deferrable(sigset_t *s) {
#endif
    sigaddset(s, SIGHUP);
    sigaddset(s, SIGINT);
    sigaddset(s, SIGTERM);
    sigaddset(s, SIGQUIT);
    sigaddset(s, SIGALRM);
    sigaddset(s, SIGURG);
    sigaddset(s, SIGTSTP);
    sigaddset(s, SIGCHLD);
#ifdef SIGIO
    sigaddset(s, SIGIO);
#else
    sigaddset(s, SIGPOLL);
#endif
#ifndef LISP_FEATURE_BACKTRACE_ON_SIGNAL
    sigaddset(s, SIGXCPU);
#endif
    sigaddset(s, SIGXFSZ);
#if !(defined SIG_STOP_FOR_GC && SIG_STOP_FOR_GC == SIGVTALRM)
    sigaddset(s, SIGVTALRM);
#endif
#if !(defined SIG_STOP_FOR_GC && SIG_STOP_FOR_GC == SIGWINCH)
    sigaddset(s, SIGWINCH);
#endif
}

static void
sigaddset_gc(sigset_t __attribute__((unused)) *sigset)
{
#ifdef THREADS_USING_GCSIGNAL
    sigaddset(sigset,SIG_STOP_FOR_GC);
#endif
}

void
sigaddset_blockable(sigset_t *sigset)
{
#ifdef LISP_FEATURE_SB_SAFEPOINT
    sigaddset_async(sigset);
#else
    sigaddset_deferrable(sigset);
    sigaddset_gc(sigset);
#endif
    // SIGPIPE is *NOT* an asynchronous signal. In normal usage you receive this signal
    // synchronously in response to a system call. As such we do not place it in the
    // deferrable set, but it _is_ blockable. If you're doing something wherein SIGPIPE
    // interrupts a pseudo-atomic section, then you're doing something wrong for sure.
    sigaddset(sigset, SIGPIPE);
    // SIGPROF does not need to be deferred- our new handler is signal-safe, and trying to
    // hide non-async-safety of a SIGPROF handler behind a deferral mechanism is horrible.
    sigaddset(sigset, SIGPROF);
}

/* initialized in interrupt_init */
sigset_t deferrable_sigset;
sigset_t blockable_sigset;
sigset_t thread_start_sigset;
/* gc_sigset will have exactly 1 bit on, for SIG_STOP_FOR_GC, or no bits on.
 * We always use SIGUSR2 as SIG_STOP_FOR_GC, though in days past it may have
 * varied by OS. Also, long ago, there was a different signal to resume after
 * suspension, but now we use a semaphore for that, which is technically
 * on shaky ground, but seems to work. e.g. consider an implementation of
 * of sem_wait that requires a call to malloc; it could fail badly for us */
sigset_t gc_sigset;

/* Return 1 if almost all deferrable signals are blocked, 0 if not,
 * and fail if there is a mixture. Explicitly ignore SIGALRM which we now
 * allow to be always blocked in a thread and/or manipulated.
 * Also don't bother with ones guarded by #ifdef in sigaddset_deferrable
 * (SIGIO, SIGPOLL, SIGXCPU).
 * The intent is to perform a best-effort check that the runtime's assumptions
 * are not egregiously violated, not to enforce proper use of each and every signal.
 * (Who would add a SIGTSTP handler that is not completely async safe anyway?)
 */
bool deferrables_blocked_p(sigset_t *sigset)
{
    sigset_t current;
    if (sigset == 0) {
        thread_sigmask(SIG_BLOCK, 0, &current);
        sigset = &current;
    }
#ifdef LISP_FEATURE_SB_SAFEPOINT
    /* The only signal whose mask bit we manipulate is SIGURG.
     * All other deferrable signals remain permanently in a blocked state.
     * Therefore the answer to the question of whether deferrables
     * are blocked is simply whether SIGURG is blocked */
    return sigismember(sigset, SIGURG);
#else
    /* SIGPROF must not be here. Some people use an async-signal-safe profiler
     * which not only doesn't rely on signal deferral, but wants to manipulate the
     * blocked/unblocked bit completely independently of SBCL's requirements.
     * Such usage would have needed to either modify the global deferrable_sigset
     * at runtime, or locally patch it out of sigaddset_deferrable.
     * I'd prefer to remove external access to deferrable_sigset which suggests that
     * this predicate should be insensitive to whether SIGPROF is deferrable.
     * The actual deferral mechanmism still works, because remember, this test does
     * not affect behavior of correct code - it is just to decide whether we understand
     * the signal mask to be in a valid state, but it was overly restrictive.
     *
     * Also SIGXCPU and SIGPWR are conspicuously absent. SB-THREAD:INTERRUPT-THREAD
     * used SIGPWR long ago, but I don't know why it was never in deferrable_sigset.
     */
    const int expected_mask = 0x3ff
#if (defined SIG_STOP_FOR_GC && SIG_STOP_FOR_GC == SIGVTALRM)
                              - (1<<1)
#endif
#if (defined SIG_STOP_FOR_GC && SIG_STOP_FOR_GC == SIGWINCH)
                              - (1<<0)
#endif
                              ;

    int mask = (sigismember(sigset, SIGHUP)    << 9)
             | (sigismember(sigset, SIGINT)    << 8)
             | (sigismember(sigset, SIGTERM)   << 7)
             | (sigismember(sigset, SIGQUIT)   << 6)
             | (sigismember(sigset, SIGURG)    << 5)
             | (sigismember(sigset, SIGTSTP)   << 4)
             | (sigismember(sigset, SIGCHLD)   << 3)
             | (sigismember(sigset, SIGXFSZ)   << 2)
#if !(defined SIG_STOP_FOR_GC && SIG_STOP_FOR_GC == SIGVTALRM)
             | (sigismember(sigset, SIGVTALRM) << 1)
#endif
#if !(defined SIG_STOP_FOR_GC && SIG_STOP_FOR_GC == SIGWINCH)
             | (sigismember(sigset, SIGWINCH)  << 0)
#endif
               ;
    if (mask == expected_mask) return 1;
    if (!mask) return 0;
    char buf[3*64]; // assuming worst case 64 signals present in sigset
    sigset_tostring(sigset, buf, sizeof buf);
    lose("deferrable signals partially blocked: {%s}", buf);
#endif
}

void
check_deferrables_unblocked_or_lose(sigset_t *sigset)
{
    if (deferrables_blocked_p(sigset))
        lose("deferrables blocked");
}

void
check_deferrables_blocked_or_lose(sigset_t *sigset)
{
    if (!deferrables_blocked_p(sigset))
        lose("deferrables unblocked");
}

#ifdef LISP_FEATURE_RISCV
int sigaction_does_not_mask;
#endif
static void assert_blockables_blocked()
{
#ifdef LISP_FEATURE_RISCV
    if (sigaction_does_not_mask) return; // assert nothing
#endif
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
    sigset_t mask;
    thread_sigmask(SIG_BLOCK, 0, &mask);
    /* Test a representative bit from each set of signals of interest:
     *  (1) stop-for-GC
     *  (2) other blockable asynchronous signals
     *  (3) other blockable synchronous signals (SIGPIPE)
     * If the representative bit is in the mask, say the whole set is.
     * Since this is just a check of an invariant which correct execution
     * will always adhere to, there is not much additional advantage to
     * looking at all the signal bits, unlike the situation where the
     * mask test is used as a predicate to decide on control flow.
     */
    if (!(
#ifdef THREADS_USING_GCSIGNAL
        sigismember(&mask, SIG_STOP_FOR_GC) && // (1)
#endif
        sigismember(&mask, SIGHUP) &&          // (2)
        sigismember(&mask, SIGPIPE)))          // (3)
        lose("blockables unblocked");
#endif
}

#ifndef LISP_FEATURE_SB_SAFEPOINT
void
check_gc_signals_unblocked_or_lose(sigset_t *sigset)
{
    sigset_t current;
    if (!sigset) {
        thread_sigmask(SIG_BLOCK, 0, &current);
        sigset = &current;
    }
    if (sigismember(sigset, SIG_STOP_FOR_GC))
        lose("gc signals blocked");
}
#endif

void
block_deferrable_signals(sigset_t *old)
{
    thread_sigmask(SIG_BLOCK, &deferrable_sigset, old);
}

void
block_blockable_signals(sigset_t *old)
{
    thread_sigmask(SIG_BLOCK, &blockable_sigset, old);
}

// Do one of two things depending on whether the specified 'where'
// is non-null or null.
// 1. If non-null, then alter the mask in *where, removing deferrable signals
//    from it without affecting the thread's mask from perspective of the OS.
// 2. If null, the actually change the current thread's signal mask.
void
unblock_deferrable_signals(sigset_t *where)
{
    if (interrupt_handler_pending_p())
        lose("unblock_deferrable_signals: losing proposition");
#ifndef LISP_FEATURE_SB_SAFEPOINT
    // If 'where' is null, check_gc_signals_unblocked_or_lose() will
    // fetch the current signal mask (from the OS) and check that.
    check_gc_signals_unblocked_or_lose(where);
#endif
    sigset_t localmask, *sigset;
    if (get_sb_vm_thread()->state_word.user_thread_p) {
        sigset = &deferrable_sigset;
    } else {
        /* ASSUMPTION: system threads never want to receive SIGALRM.
         * Actually, if we get here in the finalizer thread, things are
         * in bad shape - stack exhaustion or something */
        localmask = deferrable_sigset;
        sigdelset(&localmask, SIGALRM);
        sigset = &localmask;
    }
    if (where)
        sigmask_logandc(where, sigset);
    else
        thread_sigmask(SIG_UNBLOCK, sigset, 0);
}

#ifndef LISP_FEATURE_SB_SAFEPOINT
// This function previously had an #ifdef guard precluding doing anything for
// win32, which was redundant because SB_SAFEPOINT is always defined for win32.
void unblock_gc_stop_signal(void) {
    thread_sigmask(SIG_UNBLOCK, &gc_sigset, 0);
}
#endif

void
unblock_signals_in_context_and_maybe_warn(os_context_t *context)
{
    sigset_t *sigset = os_context_sigmask_addr(context);
#ifndef LISP_FEATURE_SB_SAFEPOINT
    if (sigismember(sigset, SIG_STOP_FOR_GC)) {
        corruption_warning_and_maybe_lose(
"Enabling blocked gc signals to allow returning to Lisp without risking\n\
gc deadlocks. Since GC signals are only blocked in signal handlers when \n\
they are not safe to interrupt at all, this is a pretty severe occurrence.\n");
        sigdelset(sigset, SIG_STOP_FOR_GC);
    }
#endif
    if (!interrupt_handler_pending_p()) {
        unblock_deferrable_signals(sigset);
    }
}


/* Note that the comment from rev aa0ed5a420 seems back-ass-wards.
 * "if there is no pending signal .. because that means deferrable are blocked"?
 * How would NO pending signal imply that deferrables are blocked? */

/* Save sigset (or the current sigmask if 0) if there is no pending
 * handler, because that means that deferabbles are already blocked.
 * The purpose is to avoid losing the pending gc signal if a
 * deferrable interrupt async unwinds between clearing the pseudo
 * atomic and trapping to GC.*/
#ifndef LISP_FEATURE_SB_SAFEPOINT
void maybe_save_gc_mask_and_block_deferrables(os_context_t *context)
{
    struct thread *thread = get_sb_vm_thread();
    struct interrupt_data *data = &thread_interrupt_data(thread);
    sigset_t oldset;
    /* Obviously, this function is called when signals may not be
     * blocked. Let's make sure we are not interrupted. */
    block_blockable_signals(&oldset);
#ifndef LISP_FEATURE_SB_THREAD
    /* With threads a SIG_STOP_FOR_GC and a normal GC may also want to
     * block. */
    if (data->gc_blocked_deferrables)
        lose("gc_blocked_deferrables already true");
#endif
    if ((!data->pending_handler) &&
        (!data->gc_blocked_deferrables)) {
        data->gc_blocked_deferrables = 1;
        if (context) {
            /* When there is a sigcontext, do the following:
             * - 'pending_mask' stores the mask from just prior to receipt of the signal
             * - On return to the interrupt point, the signal mask is restored
             *   to what it was PLUS all deferrable signals
             * - On return from this function, no change to the current mask */
            sigset_t *sigset = os_context_sigmask_addr(context);
            sigcopyset(&data->pending_mask, sigset);
            sigaddset_deferrable(sigset);
            thread_sigmask(SIG_SETMASK,&oldset,0);
            return;
        } else {
            /* Operating on the current sigmask. Save oldset and
             * unblock gc signals. In the end, this is equivalent to
             * blocking the deferrables and also SIGPIPE and SIGPROF neither of which
             * is usually deferred, for reasons cited in sigaddset_blockable */
            sigcopyset(&data->pending_mask, &oldset);
            unblock_gc_stop_signal();
            return;
        }
    }
    thread_sigmask(SIG_SETMASK,&oldset,0);
}
#endif

/* Are we leaving WITH-GCING and already running with interrupts
 * enabled, without the protection of *GC-INHIBIT* T and there is gc
 * (or stop for gc) pending, but we haven't trapped yet? */
int
in_leaving_without_gcing_race_p(struct thread __attribute__((unused)) *thread)
{
    return ((read_TLS(IN_WITHOUT_GCING,thread) != NIL) &&
            (read_TLS(INTERRUPTS_ENABLED,thread) != NIL) &&
            (read_TLS(GC_INHIBIT,thread) == NIL) &&
            ((read_TLS(GC_PENDING,thread) != NIL)
#if defined(LISP_FEATURE_SB_THREAD)
             || (read_TLS(STOP_FOR_GC_PENDING,thread) != NIL)
#endif
             ));
}

/* Check our baroque invariants. */
static void
check_interrupt_context_or_lose(os_context_t *context)
{
    struct thread *thread = get_sb_vm_thread();
    struct interrupt_data *data = &thread_interrupt_data(thread);
    int interrupt_deferred_p = (data->pending_handler != 0);
    int interrupt_pending = (read_TLS(INTERRUPT_PENDING,thread) != NIL);
    sigset_t *sigset = os_context_sigmask_addr(context);
    /* On PPC pseudo_atomic_interrupted is cleared when coming out of
     * handle_allocation_trap. */
#if defined LISP_FEATURE_GENERATIONAL && !GENCGC_IS_PRECISE
    int interrupts_enabled = (read_TLS(INTERRUPTS_ENABLED,thread) != NIL);
    int gc_inhibit = (read_TLS(GC_INHIBIT,thread) != NIL);
    int gc_pending = (read_TLS(GC_PENDING,thread) == LISP_T);
    int pseudo_atomic_interrupted = get_pseudo_atomic_interrupted(thread);
    int in_race_p = in_leaving_without_gcing_race_p(thread);
    int safepoint_active = 0;
#if defined(LISP_FEATURE_SB_SAFEPOINT)
    /* Don't try to take the gc state lock if there's a chance that
     * we're already holding it (thread_register_gc_trigger() is
     * called from PA, gc_stop_the_world() and gc_start_the_world()
     * are called from WITHOUT-GCING, all other takers of the lock
     * have deferrables blocked). */
    if (!(interrupt_pending || pseudo_atomic_interrupted || gc_inhibit)) {
        WITH_GC_STATE_LOCK {
            safepoint_active = gc_cycle_active();
        }
    }
#endif
    /* In the time window between leaving the *INTERRUPTS-ENABLED* NIL
     * section and trapping, a SIG_STOP_FOR_GC would see the next
     * check fail, for this reason sig_stop_for_gc handler does not
     * call this function. */
    if (interrupt_deferred_p) {
        if (!(!interrupts_enabled || pseudo_atomic_interrupted || in_race_p))
            lose("Stray deferred interrupt.");
    }
    if (gc_pending)
        if (!(pseudo_atomic_interrupted || gc_inhibit || in_race_p || safepoint_active))
            lose("GC_PENDING, but why?");
#if defined(LISP_FEATURE_SB_THREAD)
    {
        int stop_for_gc_pending =
            (read_TLS(STOP_FOR_GC_PENDING,thread) != NIL);
        if (stop_for_gc_pending)
            if (!(pseudo_atomic_interrupted || gc_inhibit || in_race_p || safepoint_active))
                lose("STOP_FOR_GC_PENDING, but why?");
        if (pseudo_atomic_interrupted)
            if (!(gc_pending || stop_for_gc_pending || interrupt_deferred_p))
                lose("pseudo_atomic_interrupted, but why?");
    }
#else
    if (pseudo_atomic_interrupted)
        if (!(gc_pending || interrupt_deferred_p))
            lose("pseudo_atomic_interrupted, but why?");
#endif
#endif
    if (interrupt_pending && !interrupt_deferred_p)
        lose("INTERRUPT_PENDING but not pending handler.");
    if ((data->gc_blocked_deferrables) && interrupt_pending)
        lose("gc_blocked_deferrables and interrupt pending.");
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
}

/*
 * utility routines used by various signal handlers
 */
#ifdef LISP_FEATURE_ARM64
static void
build_fake_control_stack_frames(struct thread *th, os_context_t *context)
{

    lispobj oldcont;
    /* Ignore the two words above CSP, which can be used without adjusting CSP */
    lispobj* csp = (lispobj *)(uword_t) (*os_context_register_addr(context, reg_CSP)) + 2;
    access_control_frame_pointer(th) = (lispobj *)(uword_t) csp;

    oldcont = (lispobj)(*os_context_register_addr(context, reg_CFP));

    access_control_frame_pointer(th)[1] = os_context_pc(context);
    access_control_frame_pointer(th)[0] = oldcont;
    access_control_stack_pointer(th) = csp + 2;
}
#else
static void
build_fake_control_stack_frames(struct thread __attribute__((unused)) *th,
                                os_context_t __attribute__((unused)) *context)
{
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK

    lispobj oldcont;

    /* Build a fake stack frame or frames */

#if !defined(LISP_FEATURE_ARM)
    access_control_frame_pointer(th) =
        (lispobj *)(uword_t)
        (*os_context_register_addr(context, reg_CSP));
    if ((lispobj *)(uword_t)
        (*os_context_register_addr(context, reg_CFP))
        == access_control_frame_pointer(th)) {
        /* There is a small window during call where the callee's
         * frame isn't built yet. */
        if (functionp(*os_context_register_addr(context, reg_CODE))) {
            /* We have called, but not built the new frame, so
             * build it for them. */
            access_control_frame_pointer(th)[0] =
                *os_context_register_addr(context, reg_OCFP);
            access_control_frame_pointer(th)[1] =
#ifdef reg_LRA
              *os_context_register_addr(context, reg_LRA);
#else
              *os_context_register_addr(context, reg_RA);
#endif
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
        access_control_frame_pointer(th) = (lispobj*) SymbolValue(CONTROL_STACK_POINTER, th);
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
#ifdef reg_CODE
    access_control_frame_pointer(th)[1] = NIL;
    access_control_frame_pointer(th)[2] =
        (lispobj)(*os_context_register_addr(context, reg_CODE));
#else
    access_control_frame_pointer(th)[1] = os_context_pc(context);
#endif
#endif
}
#endif

/* Stores the context for gc to scavenge and builds fake stack
 * frames. */
void fake_foreign_function_call_noassert(os_context_t *context)
{
    int context_index;
    struct thread *thread = get_sb_vm_thread();

#ifdef reg_BSP
    set_binding_stack_pointer(thread,
       // registers can be wider than uword_t (on some 64-bit machines compiling to 32-bit code)
       (uword_t)*os_context_register_addr(context, reg_BSP));
#endif

#if defined(LISP_FEATURE_ARM)
    /* Stash our control stack pointer */
    bind_variable(INTERRUPTED_CONTROL_STACK_POINTER,
                  SymbolValue(CONTROL_STACK_POINTER, thread),
                  thread);
#endif

    /* Do dynamic binding of the active interrupt context index
     * and save the context in the context array. */
    context_index =
        fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,thread));

    if (context_index >= MAX_INTERRUPTS)
        lose("maximum interrupt nesting depth (%d) exceeded", MAX_INTERRUPTS);

    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX,
                  make_fixnum(context_index + 1),thread);

    nth_interrupt_context(context_index, thread) = context;

    build_fake_control_stack_frames(thread, context);

#if !defined(LISP_FEATURE_X86) && !defined(LISP_FEATURE_X86_64) &&  \
  !(defined(LISP_FEATURE_ARM64) && defined(LISP_FEATURE_SB_THREAD))
    /* x86oid targets don't maintain the foreign function call flag at
     * all, so leave them to believe that they are never in foreign
     * code.

     And ARM64 uses control_stack_pointer, which is set in
     build_fake_control_stack_frames. */

    foreign_function_call_active_p(thread) = 1;
#endif
}
void fake_foreign_function_call(os_context_t *context)
{
    /* context_index incrementing must not be interrupted */
    assert_blockables_blocked();
    fake_foreign_function_call_noassert(context);
}

/* blocks all blockable signals.  If you are calling from a signal handler,
 * the usual signal mask will be restored from the context when the handler
 * finishes.  Otherwise, be careful */
void
undo_fake_foreign_function_call(os_context_t __attribute__((unused)) *context)
{
    struct thread *thread = get_sb_vm_thread();
    /* Block all blockable signals. */
    block_blockable_signals(0);

    foreign_function_call_active_p(thread) = 0;

#ifdef LISP_FEATURE_SB_THREAD
    // Never leave stale pointers in the signal context array
    nth_interrupt_context(fixnum_value(read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,thread)) - 1, thread) = NULL;
#endif
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
}

/* a handler for the signal caused by execution of a trap opcode
 * signalling an internal error */
void
interrupt_internal_error(os_context_t *context, bool continuable)
{
    DX_ALLOC_SAP(context_sap, context);

    fake_foreign_function_call(context);

    if (!internal_errors_enabled) {
        describe_internal_error(context);
        /* There's no good way to recover from an internal error
         * before the Lisp error handling mechanism is set up. */
        lose("internal error too early in init, can't recover");
    }

    thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);

#if defined(LISP_FEATURE_LINUX) && defined(LISP_FEATURE_MIPS)
    /* Workaround for blocked SIGTRAP. */
    {
        sigset_t newset;
        sigemptyset(&newset);
        sigaddset(&newset, SIGTRAP);
        thread_sigmask(SIG_UNBLOCK, &newset, 0);
    }
#endif

    /* Display some rudimentary debugging information about the
     * error, so that even if the Lisp error handler gets badly
     * confused, we have a chance to determine what's going on. */
    // describe_internal_error(context); // uncomment me for debugging

    funcall2(StaticSymbolFunction(INTERNAL_ERROR), context_sap,
             continuable ? LISP_T : NIL);

    undo_fake_foreign_function_call(context); /* blocks signals again */
    if (continuable)
        arch_skip_instruction(context);
}

bool interrupt_handler_pending_p(void)
{
    struct interrupt_data *data = &thread_interrupt_data(get_sb_vm_thread());
    return (data->pending_handler != 0);
}

void
interrupt_handle_pending(os_context_t *context)
{
#ifdef ADDRESS_SANITIZER
    __asan_unpoison_memory_region(context, sizeof *context);
#endif
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

    struct thread *thread = get_sb_vm_thread();
    struct interrupt_data *data = &thread_interrupt_data(thread);

    if (arch_pseudo_atomic_atomic(thread)) {
        lose("Handling pending interrupt in pseudo atomic.");
    }

    assert_blockables_blocked();
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
        if (read_TLS(GC_INHIBIT,thread)!=NIL)
            lose("GC blocked deferrables while GC is inhibited.");
        /* Restore the saved signal mask from the original signal (the
         * one that interrupted us during the critical section) into
         * the os_context for the signal we're currently in the
         * handler for. This should ensure that when we return from
         * the handler the blocked signals are unblocked. */
        sigcopyset(os_context_sigmask_addr(context), &data->pending_mask);
        data->gc_blocked_deferrables = 0;
    }
#endif

    if (read_TLS(GC_INHIBIT,thread)==NIL) {
        void *original_pending_handler = data->pending_handler;

#ifdef LISP_FEATURE_SB_SAFEPOINT
        /* handles the STOP_FOR_GC_PENDING case, plus THRUPTIONS */
        if (read_TLS(STOP_FOR_GC_PENDING,thread) != NIL
             || (read_TLS(THRUPTION_PENDING,thread) != NIL
                 && read_TLS(INTERRUPTS_ENABLED, thread) != NIL)) {
            /* We ought to take this chance to do a pitstop now. */
            fake_foreign_function_call(context);
            thread_in_lisp_raised(context);
            undo_fake_foreign_function_call(context);
        }
#elif defined(LISP_FEATURE_SB_THREAD)
        if (read_TLS(STOP_FOR_GC_PENDING,thread) != NIL) {
            /* STOP_FOR_GC_PENDING and GC_PENDING are cleared by
             * the signal handler if it actually stops us. */
            arch_clear_pseudo_atomic_interrupted(thread);
            sig_stop_for_gc_handler(SIG_STOP_FOR_GC,NULL,context);
        } else
#endif
         /* Test for T and not for != NIL since the value :IN-PROGRESS
          * used to be used in SUB-GC as part of the mechanism to
          * supress recursive gcs.*/
        if (read_TLS(GC_PENDING,thread) == LISP_T) {

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

            arch_clear_pseudo_atomic_interrupted(thread);

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
        } else if (read_TLS(GC_PENDING,thread) != NIL) {
            /* It's not NIL or T so GC_PENDING is :IN-PROGRESS. If
             * GC-PENDING is not NIL then we cannot trap on pseudo
             * atomic due to GC (see if(GC_PENDING) logic in
             * cheneygc.c an gengcgc.c), plus there is a outer
             * WITHOUT-INTERRUPTS SUB_GC, so how did we end up
             * here? */
            lose("Trapping to run pending handler while GC in progress.");
        }

        assert_blockables_blocked();

        /* No GC shall be lost. If SUB_GC triggers another GC then
         * that should be handled on the spot. */
        if (read_TLS(GC_PENDING,thread) != NIL)
            lose("GC_PENDING after doing gc.");
#ifdef THREADS_USING_GCSIGNAL
        if (read_TLS(STOP_FOR_GC_PENDING,thread) != NIL)
            lose("STOP_FOR_GC_PENDING after doing gc.");
#endif
        /* Check two things. First, that gc does not clobber a handler
         * that's already pending. Second, that there is no interrupt
         * lossage: if original_pending_handler was NULL then even if
         * an interrupt arrived during GC (POST-GC, really) it was
         * handled. */
        if (original_pending_handler != data->pending_handler)
            lose("pending handler changed in gc: %p -> %p, signal = %d.",
                 original_pending_handler, data->pending_handler, data->pending_signal);
    }

#ifndef LISP_FEATURE_WIN32
    /* There may be no pending handler, because it was only a gc that
     * had to be executed or because Lisp is a bit too eager to call
     * DO-PENDING-INTERRUPT. */
    if ((read_TLS(INTERRUPTS_ENABLED,thread) != NIL) &&
        (data->pending_handler))  {
        /* No matter how we ended up here, clear both
         * INTERRUPT_PENDING and pseudo atomic interrupted. It's safe
         * because we checked above that there is no GC pending. */
        write_TLS(INTERRUPT_PENDING, NIL, thread);
        arch_clear_pseudo_atomic_interrupted(thread);
        /* Restore the sigmask in the context. */
        sigcopyset(os_context_sigmask_addr(context), &data->pending_mask);
        run_deferred_handler(data, context);
    }
#ifdef LISP_FEATURE_SB_SAFEPOINT
    if (read_TLS(THRUPTION_PENDING,thread)==LISP_T)
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
#ifdef LISP_FEATURE_GENERATIONAL
    if (get_pseudo_atomic_interrupted(thread))
        lose("pseudo_atomic_interrupted after interrupt_handle_pending");
#endif
    /* It is possible that the end of this function was reached
     * without actually doing anything, the tests in Lisp for
     * when to call receive-pending-interrupt are not exact. */
}


void
interrupt_handle_now(int signal, siginfo_t *info, os_context_t *context)
{
    bool were_in_lisp;
    lispobj handler = lisp_sig_handlers[signal];

    if (!functionp(handler)) return;

    assert_blockables_blocked();

    struct thread* thread = get_sb_vm_thread();
    if (sigismember(&deferrable_sigset,signal)) {
        if (read_TLS(INTERRUPTS_ENABLED,thread) == NIL) lose("interrupts not enabled");
        if (arch_pseudo_atomic_atomic(thread)) lose ("in pseudo atomic section");
    }

    were_in_lisp = !foreign_function_call_active_p(thread);
    if (were_in_lisp)
    {
        // Use the variant of fake_ffc that doesn't do another pthread_sigmask syscall,
        // as we've just asserted that signals are blocked.
        fake_foreign_function_call_noassert(context);
    }

        /* Once we've decided what to do about contexts in a
         * return-elsewhere world (the original context will no longer
         * be available; should we copy it or was nobody using it anyway?)
         * then we should convert this to return-elsewhere */

#if !defined(LISP_FEATURE_SB_SAFEPOINT) && defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
        /* Leave deferrable signals blocked, the handler itself will
         * allow signals again when it sees fit. */
        /* handler.lisp will hide from the GC, will be enabled in the handler itself.
         * Not a problem for the conservative GC. */
        unblock_gc_stop_signal();
#endif

        WITH_GC_AT_SAFEPOINTS_ONLY()
        { // the block is needed for WITH_GC_AT_SAFEPOINTS_ONLY() to work
            DX_ALLOC_SAP(context_sap, context);
            DX_ALLOC_SAP(info_sap, info);

            funcall3(handler,
                     make_fixnum(signal),
                     info_sap,
                     context_sap);
        }

    if (were_in_lisp)
    {
        undo_fake_foreign_function_call(context); /* block signals again */
    }
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
    (*pending_handler)(data->pending_signal,&(data->pending_info), context);
}

#ifndef LISP_FEATURE_WIN32
static void
store_signal_data_for_later (struct interrupt_data *data, void *handler,
                             int signal,
                             siginfo_t *info, os_context_t *context)
{
    if (!context || !handler || data->pending_handler)
        lose("can't defer signal: context=%p handler=%p pending=%p",
             context, handler, data->pending_handler);
    data->pending_handler = handler;
    data->pending_signal = signal;
    if (info)
        memcpy(&data->pending_info, info, sizeof *info);
    else
        memset(&data->pending_info, 0, sizeof *info);
    /* the signal mask in the context (from before we were
     * interrupted) is copied to be restored when run_deferred_handler
     * happens. Then the usually-blocked signals are added to the mask
     * in the context so that we are running with blocked signals when
     * the handler returns */
    sigcopyset(&data->pending_mask, os_context_sigmask_addr(context));
    sigaddset_deferrable(os_context_sigmask_addr(context));
}

/* What's going on ?
 *
 *  0: fp=0x7fa86423e7a0 pc=0x27608c Foreign function (null)
 *  1: fp=0x7fa86423e7b0 pc=0x2767aa Foreign function (null)
 *  2: fp=0x7fa86423e890 pc=0x2761af Foreign function (null)
 *  3: fp=0x7fa86423e970 pc=0x27882f Foreign function (null)       ; maybe_now_maybe_later
 *  4: fp=0x7fa86423f5c0 pc=0x7fa8646bc750 Foreign function (null) ; WHICH SIGNAL ?
 *  5: fp=0x7fa86423f630 pc=0x2713ce Foreign function (null)       ; verify_range
 *  6: fp=0x7fa86423f6c0 pc=0x2703d3 Foreign function (null)       ; verify_heap
 *  7: fp=0x7fa86423f780 pc=0x26e168 Foreign function collect_garbage
 *  8: fp=0x7fa86423f7f0 pc=0x270298 Foreign function gc_and_save
 *  9: fp=0x7fa86423f848 pc=0x52f93bf2 <??? type 45>::GC-AND-SAVE
 * 10: fp=0x7fa86423f950 pc=0x52d32a7b <??? type 45>::SAVE-LISP-AND-DIE
 * 11: fp=0x7fa86423fa00 pc=0x52a34179 <??? type 45>::SAVE-LISP-AND-DIE
 *
 * fatal error encountered in SBCL pid 9436 tid 9436:
 * interrupt already pending
 */

static bool
can_handle_now(void *handler, struct interrupt_data *data,
               int signal, siginfo_t *info, os_context_t *context)
{
#ifdef DEBUG
    // All this might prove is that you set sa_mask correctly when calling
    // sigaction. That's not worth an extra system call.
    assert_blockables_blocked();
#endif

    struct thread *thread = get_sb_vm_thread();

    if (read_TLS(INTERRUPT_PENDING,thread) != NIL)
        lose("interrupt already pending when sig%d received, pc=%p", signal,
             (void*)os_context_pc(context));
    if (thread_interrupt_data(thread).pending_handler)
        lose("there is a pending handler already (PA)");
    if (data->gc_blocked_deferrables)
        lose("can_handle_now: gc_blocked_deferrables true");

    int answer = 1;
    /* If interrupts are disabled then INTERRUPT_PENDING is set and
     * not PSEDUO_ATOMIC_INTERRUPTED. This is important for a pseudo
     * atomic section inside a WITHOUT-INTERRUPTS.
     *
     * Also, if in_leaving_without_gcing_race_p then
     * interrupt_handle_pending is going to be called soon, so
     * stashing the signal away is safe.
     */
    if ((read_TLS(INTERRUPTS_ENABLED,thread) == NIL) ||
        in_leaving_without_gcing_race_p(thread)) {
        event3("can_handle_now(%p,%d): deferred (RACE=%d)", handler, signal,
               in_leaving_without_gcing_race_p(thread));
        store_signal_data_for_later(data,handler,signal,info,context);
        write_TLS(INTERRUPT_PENDING, LISP_T, thread);
        answer = 0;
    }
    /* a slightly confusing test. arch_pseudo_atomic_atomic() doesn't
     * actually use its argument for anything on x86, so this branch
     * may succeed even when context is null (gencgc alloc()) */
    else if (arch_pseudo_atomic_atomic(thread)) {
        event2("can_handle_now(%p,%d): deferred (PA)", handler, signal);
        store_signal_data_for_later(data,handler,signal,info,context);
        arch_set_pseudo_atomic_interrupted(thread);
        answer = 0;
    }

    check_interrupt_context_or_lose(context);

    return answer;
}

static void
maybe_now_maybe_later(int signal, siginfo_t *info, void *void_context)
{
    SAVE_ERRNO(signal,context,void_context);
    struct thread *thread = get_sb_vm_thread();
    struct interrupt_data *data = &thread_interrupt_data(thread);
    if (can_handle_now(interrupt_handle_now, data, signal, info, context))
        interrupt_handle_now(signal, info, context);
    RESTORE_ERRNO;
}
#endif

#ifdef THREADS_USING_GCSIGNAL

/* This function must not cons, because that may trigger a GC. */
void
sig_stop_for_gc_handler(int __attribute__((unused)) signal,
                        siginfo_t __attribute__((unused)) *info,
                        os_context_t *context)
{
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
#ifdef MEASURE_STOP_THE_WORLD_PAUSE
    extern void thread_accrue_stw_time(struct thread*,struct timespec*,struct timespec*);
    thread_accrue_stw_time(thread, &t_beginpause, 0);
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

#endif

void
interrupt_handle_now_handler(int signal, siginfo_t *info, void *void_context)
{
    SAVE_ERRNO(signal,context,void_context);
#ifndef LISP_FEATURE_WIN32
    if ((signal == SIGILL) || (signal == SIGBUS)
#if !(defined LISP_FEATURE_LINUX || defined LISP_FEATURE_ANDROID || defined LISP_FEATURE_HAIKU)
        || (signal == SIGEMT)
#endif
        )
        corruption_warning_and_maybe_lose("Signal %d received (PC: %p)", signal,
                                          os_context_pc(context));
#endif
    interrupt_handle_now(signal, info, context);
    RESTORE_ERRNO;
}

/* manipulate the signal context and stack such that when the handler
 * returns, it will call function instead of whatever it was doing
 * previously
 */

#if (defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
extern int *os_context_flags_addr(os_context_t *context);
#endif

extern lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs);
extern void post_signal_tramp(void);
extern void call_into_lisp_tramp(void);

void
arrange_return_to_c_function(os_context_t *context,
                             call_into_lisp_lookalike funptr,
                             lispobj function)
{
#ifndef LISP_FEATURE_SB_SAFEPOINT
    check_gc_signals_unblocked_or_lose(os_context_sigmask_addr(context));
#endif
#if !(defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
    void * fun=native_pointer(function);
    void *code = &(((struct simple_fun *) fun)->insts);
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

#ifndef LISP_FEATURE_DARWIN
    uint32_t *sp=(uint32_t *)*os_context_register_addr(context,reg_ESP);
#endif

#if defined(LISP_FEATURE_DARWIN)
    uint32_t *register_save_area = (uint32_t *)os_allocate(0x40);

    /* 1. allocate (malloc/mmap) register_save_block
     * 2. copy register state into register_save_block
     * 3. put a pointer to register_save_block in a register in the context
     * 4. set the context's EIP to point to a trampoline which:
     *    a. builds the fake stack frame from the block
     *    b. frees the block
     *    c. calls the function
     */

    *register_save_area = os_context_pc(context);
    *(register_save_area + 1) = function;
    *(register_save_area + 2) = *os_context_register_addr(context,reg_EDI);
    *(register_save_area + 3) = *os_context_register_addr(context,reg_ESI);
    *(register_save_area + 4) = *os_context_register_addr(context,reg_EDX);
    *(register_save_area + 5) = *os_context_register_addr(context,reg_ECX);
    *(register_save_area + 6) = *os_context_register_addr(context,reg_EBX);
    *(register_save_area + 7) = *os_context_register_addr(context,reg_EAX);
    *(register_save_area + 8) = *os_context_flags_addr(context);

    set_os_context_pc(context, (os_context_register_t) funptr);
    *os_context_register_addr(context,reg_ECX) =
      (os_context_register_t) register_save_area;
#else

    /* return address for call_into_lisp: */
    *(sp-15) = (uint32_t)post_signal_tramp;
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
    *(sp-3)=*os_context_flags_addr(context);
    *(sp-2)=*os_context_register_addr(context,reg_EBP);
    *(sp-1)=os_context_pc(context);

#endif

#elif defined(LISP_FEATURE_X86_64)
    uword_t *sp=(uword_t *)*os_context_register_addr(context,reg_RSP);

    /* return address for call_into_lisp: */
    *(sp-18) = (uint64_t)post_signal_tramp;

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
    *(sp-3)=*os_context_flags_addr(context);
    *(sp-2)=*os_context_register_addr(context,reg_RBP);
    *(sp-1)=os_context_pc(context);

    *os_context_register_addr(context,reg_RDI) =
        (os_context_register_t)function; /* function */
    *os_context_register_addr(context,reg_RSI) = 0;        /* arg. array */
    *os_context_register_addr(context,reg_RDX) = 0;        /* no. args */
#else
    struct thread *th = get_sb_vm_thread();
    build_fake_control_stack_frames(th,context);
#endif

#ifdef LISP_FEATURE_X86

#if !defined(LISP_FEATURE_DARWIN)
    set_os_context_pc(context, (os_context_register_t)funptr);
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
    set_os_context_pc(context, (os_context_register_t)funptr);
    *os_context_register_addr(context,reg_RCX) = 0;
    *os_context_register_addr(context,reg_RBP) = (os_context_register_t)(sp-2);
    *os_context_register_addr(context,reg_RSP) = (os_context_register_t)(sp-18);
#else
    /* this much of the calling convention is common to all
       non-x86 ports */
    set_os_context_pc(context, (os_context_register_t)(unsigned long)code);
    *os_context_register_addr(context,reg_NARGS) = 0;
#ifdef reg_LIP
    *os_context_register_addr(context,reg_LIP) =
        (os_context_register_t)(unsigned long)code;
#endif
    *os_context_register_addr(context,reg_CFP) =
        (os_context_register_t)(unsigned long)access_control_frame_pointer(th);
#endif
#ifdef ARCH_HAS_NPC_REGISTER
    *os_context_npc_addr(context) = 4 + os_context_pc(context);
#endif
#if defined(LISP_FEATURE_SPARC) || defined(LISP_FEATURE_ARM) || defined(LISP_FEATURE_RISCV)
    *os_context_register_addr(context,reg_CODE) =
        (os_context_register_t)((char*)fun + FUN_POINTER_LOWTAG);
#endif
}

void
arrange_return_to_lisp_function(os_context_t *context, lispobj function)
{
#if defined(LISP_FEATURE_DARWIN) && defined(LISP_FEATURE_X86)
    arrange_return_to_c_function(context,
                                 (call_into_lisp_lookalike)call_into_lisp_tramp,
                                 function);
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

#ifndef LISP_FEATURE_WIN32
void lower_thread_control_stack_guard_page(struct thread *th)
{
    protect_control_stack_guard_page(0, th);
    protect_control_stack_return_guard_page(1, th);
    th->state_word.control_stack_guard_page_protected = 0;
    fprintf(stderr, "INFO: Control stack guard page unprotected\n");
}

void reset_thread_control_stack_guard_page(struct thread *th)
{
    memset(CONTROL_STACK_GUARD_PAGE(th), 0, os_vm_page_size);
    protect_control_stack_guard_page(1, th);
    protect_control_stack_return_guard_page(0, th);
    th->state_word.control_stack_guard_page_protected = 1;
    fprintf(stderr, "INFO: Control stack guard page reprotected\n");
}
#endif

bool handle_guard_page_triggered(os_context_t *context,os_vm_address_t addr)
{
    struct thread *th = get_sb_vm_thread();

#ifndef LISP_FEATURE_WIN32
    if(addr >= CONTROL_STACK_HARD_GUARD_PAGE(th) &&
       addr < CONTROL_STACK_HARD_GUARD_PAGE(th) + os_vm_page_size) {
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
        /* fake_foreign_function_call wants to write to the stack. */
        protect_control_stack_hard_guard_page(0, th);
#endif
        fake_foreign_function_call(context);
        lose("Control stack exhausted, fault: %p, PC: %p",
             addr, (void*)os_context_pc(context));
    }
    else if(addr >= CONTROL_STACK_GUARD_PAGE(th) &&
            addr < CONTROL_STACK_GUARD_PAGE(th) + os_vm_page_size) {
        /* We hit the end of the control stack: disable guard page
         * protection so the error handler has some headroom, protect the
         * previous page so that we can catch returns from the guard page
         * and restore it. */
        if (gc_active_p) {
            fake_foreign_function_call(context);
            lose("Control stack exhausted with gc_active_p, fault: %p, PC: %p",
                 addr, (void*)os_context_pc(context));
        }
        if (arch_pseudo_atomic_atomic(th)) {
            fake_foreign_function_call(context);
            lose("Control stack exhausted while pseudo-atomic, fault: %p, PC: %p",
                 addr, (void*)os_context_pc(context));
        }
        if (lose_on_corruption_p) {
#ifndef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
            /* fake_foreign_function_call wants to write to the stack. */
            protect_control_stack_guard_page(0, th);
#endif
            fake_foreign_function_call(context);
            lose("Control stack exhausted, fault: %p, PC: %p",
                 addr, (void*)os_context_pc(context));
        }
        if (!th->state_word.control_stack_guard_page_protected)
            lose("control_stack_guard_page_protected NIL");
        lower_thread_control_stack_guard_page(th);
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
        if (th->state_word.control_stack_guard_page_protected)
            lose("control_stack_guard_page_protected not NIL");
        reset_thread_control_stack_guard_page(th);
        return 1;
    }
    else
#endif // !LISP_FEATURE_WIN32
    if(addr >= BINDING_STACK_HARD_GUARD_PAGE(th) &&
       addr < BINDING_STACK_HARD_GUARD_PAGE(th) + os_vm_page_size) {
        lose("Binding stack exhausted");
    }
    else if(addr >= BINDING_STACK_GUARD_PAGE(th) &&
            addr < BINDING_STACK_GUARD_PAGE(th) + os_vm_page_size) {
        protect_binding_stack_guard_page(0, NULL);
        protect_binding_stack_return_guard_page(1, NULL);

        if (lose_on_corruption_p) {
            fake_foreign_function_call(context);
            lose("Binding stack exhausted");
        }

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

#ifndef LISP_FEATURE_WIN32
extern void restore_sbcl_signals () {
    int signal;
    for (signal = 0; signal < NSIG; signal++) {
        interrupt_handler_t handler = interrupt_low_level_handlers[signal];
        if ((void*)handler != (void*)SIG_DFL) {
            ll_install_handler(signal, handler);
        }
    }
}

static void
low_level_handle_now_handler(int signal, siginfo_t *info, void *void_context)
{
    /* We forgo SAVE_ERRNO / RESTORE_ERRNO here because those can resignal to a
     * different thread. It never makes sense with synchronous signals such as SIGILL,
     * SIGTRAP, SIGFPE, SIGSEGV which are necessarily thread-specific; nor SIGABRT
     * when raised by assert(). Some cases might warrant trying both the "old"
     * and "our" handler, but the handler does not return an indicator of whether
     * it did anything, which makes handler chaining impractical */
    int saved_errno = errno;
    RECORD_SIGNAL(signal,void_context);
    UNBLOCK_SIGSEGV();
    RESTORE_FP_CONTROL_WORD(context,void_context);
    if (lisp_thread_p(void_context)) {
        interrupt_low_level_handlers[signal](signal, info, context);
    }
#if defined LISP_FEATURE_DARWIN && defined LISP_FEATURE_SB_THREAD
    else if (signal == SIG_STOP_FOR_GC && pthread_getspecific(ignore_stop_for_gc)) {
        /* Clearing stop-for-GC on macOS seems to require that the signal
         * be delivered and then ignored in code. */
    }
#endif
    else if (old_ll_sigactions[signal].sa_handler == SIG_IGN) {
        // drop it
    } else if (old_ll_sigactions[signal].sa_handler != SIG_DFL) {
        (old_ll_sigactions[signal].sa_sigaction)(signal, info, context);
    } else {
#ifdef LISP_FEATURE_SB_THREAD
        lose("Can't handle sig%d in non-lisp thread %p @ %p",
             signal,
             // Casting to void* is a kludge - "technically" you can't assume that
             // pthread_t is integer-sized. It could be a struct.
             (void*)pthread_self(), (void*)os_context_pc(context));
#endif
    }
    errno = saved_errno;
}

/* Install a handler for a synchronous signal. These are predominantly
 * SIG{SEGV, ILL, TRAP, FPE, ABRT}. Low-level handlers might or might not
 * involve calling Lisp.
 * As well there are two asynchronous signals installed via this function:
 * - STOP_FOR_GC is low-level, but might defer the signal through
 *   an intricate bunch of decisions about the state of the world.
 * - SIGURG without :SB-SAFEPOINT is a high-level (Lisp) handler,
 *   but with :SB-SAFEPOINT is low-level handler that uses different
 *   criteria for when to defer. */
void
ll_install_handler (int signal, interrupt_handler_t handler)
{
    struct sigaction sa;

    if (0 > signal || signal >= NSIG
#ifdef LISP_FEATURE_SB_SAFEPOINT
    /* SIGURG is in `deferrable_sigset' so that we block&unblock it properly,
     * but we don't actually want to defer it, at least not here.
     * (It might get deferred until a safepoint). And if we put it only
     * into blockable_sigset, we'd have to special-case it around thread
     * creation at least. */
        || (signal != SIGURG && sigismember(&deferrable_sigset,signal))
#else
        || sigismember(&deferrable_sigset,signal)
#endif
        || (void*)handler == (void*)SIG_DFL)
        lose("ll_install_handler: bad args: sig=%d, fn=%p", signal, handler);

    sa.sa_sigaction = low_level_handle_now_handler;
    sa.sa_mask = blockable_sigset;
    sa.sa_flags = SA_SIGINFO | SA_RESTART | OS_SA_NODEFER;
#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
    if (signal==SIG_MEMORY_FAULT) sa.sa_flags |= SA_ONSTACK;
#endif

    sigaction(signal, &sa, &old_ll_sigactions[signal]);
    interrupt_low_level_handlers[signal] = handler;
}
#endif

extern void sigprof_handler(int, siginfo_t*, void*);

/* This is called from Lisp. */
void install_handler(int signal, lispobj handler)
{
#ifndef LISP_FEATURE_WIN32
    struct sigaction sa;
    memset(&sa, 0, sizeof sa);

    if (interrupt_low_level_handlers[signal]) {
        // When there's a low-level handler, we must leave it alone.
        // Give it the lisp function to call if it decides to forward a signal.
        // SIG_IGN and SIG_DFL don't always do what you think in such case.
        lisp_sig_handlers[signal] = functionp(handler) ? handler : 0;
    } else if (signal == SIGPROF) {
        if (handler) sa.sa_sigaction = sigprof_handler;
        else         sa.sa_handler   = SIG_DFL;
        // The handler is signal-safe, but because it uses component_ptr_from_pc(),
        // it must block GC, lest crashes occur from dereferencing wild pointers.
        sa.sa_mask = blockable_sigset;
        sa.sa_flags = SA_SIGINFO | SA_RESTART;
        sigaction(signal, &sa, NULL);
        return;
    } else {
        // Our "abstract" values for SIG_DFL and SIG_IGN are 0 and 1
        // respectively which are probably the real values from signal.h
        // but this way way don't need to put them in grovel-headers.c
        if (handler==0 || handler==1) {
            sa.sa_handler = handler ? SIG_IGN : SIG_DFL;
            // assign the OS level action before clearing the lisp function.
            // (If a signal were to be delivered to the C trampoline when the lisp
            // function is NIL, we'd get the effect of :IGNORE regardless
            // of what the default action should be)
            sigaction(signal, &sa, NULL);
            lisp_sig_handlers[signal] = 0;
            return;
        }
        if (sigismember(&deferrable_sigset, signal))
            sa.sa_sigaction = maybe_now_maybe_later;
        else
            sa.sa_sigaction = interrupt_handle_now_handler;

        sa.sa_mask = blockable_sigset;
        sa.sa_flags = SA_SIGINFO | SA_RESTART | OS_SA_NODEFER;
        // ensure the C handler sees a lisp function before doing sigaction()
        lisp_sig_handlers[signal] = handler;
        sigaction(signal, &sa, NULL);
    }
#endif
}

/* This must not go through lisp as it's allowed anytime, even when on
 * the altstack. */
void
sigabrt_handler(int __attribute__((unused)) signal,
                siginfo_t __attribute__((unused)) *info,
                os_context_t *context)
{
    /* Save the interrupt context. No need to undo it, since lose()
     * shouldn't return. */
    fake_foreign_function_call(context);
    lose("SIGABRT received.");
}

void
interrupt_init(void)
{
#ifdef ATOMIC_LOGGING
    // If fetch_and_add gives us an index that is less than EVENTBUFMAX,
    // we assume that there is room to record an event with up to 8 arguments
    // which means the prefix, the format string, and the arguments.
    eventdata = calloc(EVENTBUFMAX+10, N_WORD_BYTES);
    void sigdump_eventlog(int, siginfo_t*, os_context_t*);
    // pick anything not used. SIGPWR is also a good choice
    ll_install_handler(SIGINFO, sigdump_eventlog);
#endif
    int __attribute__((unused)) i;
    sigemptyset(&deferrable_sigset);
    sigemptyset(&blockable_sigset);
    sigemptyset(&gc_sigset);
    sigaddset_deferrable(&deferrable_sigset);
    sigaddset_blockable(&blockable_sigset);
    sigaddset_gc(&gc_sigset);

    sigaddset_deferrable(&thread_start_sigset);
    /* sigprof_handler may interrupt a thread that doesn't have
     current_thread set up yet, which can be a thread-local variable,
     and sigprof_handler will try to allocate it, but thread-local
     initialization is not guaranteed to be async safe. */
    sigaddset(&thread_start_sigset, SIGPROF);


#ifdef LISP_FEATURE_BACKTRACE_ON_SIGNAL
    // Use this only if you know what you're doing
    void backtrace_lisp_threads(int, siginfo_t*, os_context_t*);
    ll_install_handler(SIGXCPU, backtrace_lisp_threads);
#endif

#ifndef LISP_FEATURE_WIN32
    ll_install_handler(SIGABRT, sigabrt_handler);
#endif
}

#ifndef LISP_FEATURE_WIN32
int
siginfo_code(siginfo_t *info)
{
    return info->si_code;
}

void
lisp_memory_fault_error(os_context_t *context, os_vm_address_t addr)
{

    /* If it's a store to read-only space, it's not "corruption", so don't say that.
     * Lisp will change its wording of the memory-fault-error string */

    if (!readonly_space_p((uword_t)addr)) {
        /* To allow debugging memory faults in signal handlers and such. */
#ifdef ARCH_HAS_STACK_POINTER
        char* pc = (char*)os_context_pc(context);
        struct code* code = (struct code*)component_ptr_from_pc(pc);
        unsigned int offset = code ? pc - (char*)code : 0;
        if (offset)
            corruption_warning(
                "Memory fault at %p (pc=%p [code %p+0x%X ID 0x%x], fp=%p, sp=%p)" THREAD_ID_LABEL,
                addr, pc, code, offset, code_serialno(code),
                os_context_frame_pointer(context),
                *os_context_sp_addr(context), THREAD_ID_VALUE);
        else
            corruption_warning(
                "Memory fault at %p (pc=%p, fp=%p, sp=%p)" THREAD_ID_LABEL,
                addr, pc, os_context_frame_pointer(context),
                *os_context_sp_addr(context), THREAD_ID_VALUE);
#else
        corruption_warning("Memory fault at %p (pc=%p)",
                           addr, (void*)os_context_pc(context));
#endif
        /* If we lose on corruption, provide LDB with debugging information. */
        fake_foreign_function_call(context);
        maybe_lose();
    } else {
        fake_foreign_function_call(context);
    }

#ifdef LISP_FEATURE_C_STACK_IS_CONTROL_STACK
#  if !(defined(LISP_FEATURE_X86) || defined(LISP_FEATURE_X86_64))
#    error memory fault emulation needs validating for this architecture
#  endif
    /* We're on the altstack, and don't want to run Lisp code here, so
     * we need to return from this signal handler.  But when we get to
     * Lisp we'd like to have a signal context (with correct values in
     * it) to present to the debugger, along with knowledge of what
     * the faulting address was.  To get a signal context on the main
     * stack, we arrange to return to a trap instruction.  To get the
     * correct program counter in the context, we save it on the stack
     * here and restore it to the context in the trap handler.  To
     * pass the fault address, we save it on the stack here and pick
     * it up in the trap handler.  And the stack pointer manipulation
     * works as long as the on-stack side only pops items in its trap
     * handler. */
    extern void memory_fault_emulation_trap(void);
    undo_fake_foreign_function_call(context);
    void **sp = (void **)*os_context_sp_addr(context);
    *--sp = (void *)os_context_pc(context);
    *--sp = addr;
#  ifdef LISP_FEATURE_X86
    /* KLUDGE: x86-linux sp_addr doesn't affect the CPU on return */
    *((void **)os_context_register_addr(context, reg_ESP)) = sp;
#  else
    *((void **)os_context_sp_addr(context)) = sp;
#  endif
    set_os_context_pc(context, (os_context_register_t)memory_fault_emulation_trap);
    /* We exit here, letting the signal handler return, picking up at
     * memory_fault_emulation_trap (in target-assem.S), which will
     * trap, and the handler calls the function below, where we
     * restore our state to parallel what a non-x86oid would have, and
     * then run the common code for handling the error in Lisp. */
}

void
handle_memory_fault_emulation_trap(os_context_t *context)
{
    void **sp = (void **)*os_context_sp_addr(context);
    void *addr = *sp++;
    set_os_context_pc(context, (os_context_register_t)*sp++);
#  ifdef LISP_FEATURE_X86
    /* KLUDGE: x86-linux sp_addr doesn't affect the CPU on return */
    *((void **)os_context_register_addr(context, reg_ESP)) = sp;
#  else
    *os_context_sp_addr(context) = (os_context_register_t)sp;
#  endif
    fake_foreign_function_call(context);
#endif /* C_STACK_IS_CONTROL_STACK */
    /* On x86oids, we're in handle_memory_fault_emulation_trap().
     * On real computers, we're still in lisp_memory_fault_error(). */

    thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);

    DX_ALLOC_SAP(context_sap, context);
    DX_ALLOC_SAP(fault_address_sap, addr);
    funcall2(StaticSymbolFunction(MEMORY_FAULT_ERROR),
             context_sap, fault_address_sap);
    undo_fake_foreign_function_call(context);
}
#endif /* !LISP_FEATURE_WIN32 */

static void
unhandled_trap_error(os_context_t *context)
{
    DX_ALLOC_SAP(context_sap, context);
    fake_foreign_function_call(context);

    thread_sigmask(SIG_SETMASK, os_context_sigmask_addr(context), 0);

    funcall1(StaticSymbolFunction(UNHANDLED_TRAP_ERROR), context_sap);
    lose("UNHANDLED-TRAP-ERROR fell through");
}

/* Common logic for trapping instructions. How we actually handle each
 * case is highly architecture dependent, but the overall shape is
 * this. */
void
handle_trap(os_context_t *context, int trap)
{
    if (trap >= trap_Error) {
        trap = trap_Error;
    }
    switch(trap) {
#ifndef LISP_FEATURE_WIN32
    case trap_PendingInterrupt:
        arch_skip_instruction(context);
        interrupt_handle_pending(context);
        break;
#endif
    case trap_Error:
    case trap_Cerror:
#ifdef trap_InvalidArgCount
    case trap_InvalidArgCount:
#endif
#ifdef trap_UninitializedLoad
# define CONTINUABLE_P (trap==trap_Cerror || trap==trap_UninitializedLoad)
    case trap_UninitializedLoad:
#else
# define CONTINUABLE_P (trap==trap_Cerror)
#endif
        interrupt_internal_error(context, CONTINUABLE_P);
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
#if defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK) && !defined(LISP_FEATURE_WIN32)
    case trap_MemoryFaultEmulation:
        handle_memory_fault_emulation_trap(context);
        break;
#endif
    case trap_Halt:
        fake_foreign_function_call(context);
        lose("%%PRIMITIVE HALT called; the party is over.");
    default:
        unhandled_trap_error(context);
    }
}

#ifndef LISP_FEATURE_WIN32
// Return 1 if the signal was previously in the blocked set.
int sb_toggle_sigprof(os_context_t* context, int block) {
    if (context) {
        // This case is used with INTERRUPT-THREAD to unmask SIGPROF in any thread
        // other than the current thread.
        gc_assert(!block);

        // Alter the mask on return from the _outermost_ signal context, which
        // should usually be the supplied context, but not if nesting happened.
        if (read_TLS(FREE_INTERRUPT_CONTEXT_INDEX,get_sb_vm_thread()) > 0) {
            context = nth_interrupt_context(0, get_sb_vm_thread());
            gc_assert(context);
        }

        sigset_t *mask = os_context_sigmask_addr(context);
        int was_blocked = sigismember(mask, SIGPROF);
        if (block) sigaddset(mask, SIGPROF); else sigdelset(mask, SIGPROF);
        return was_blocked;
    } else {
        sigset_t sigset, old;
        sigemptyset(&sigset);
        sigaddset(&sigset, SIGPROF);
        thread_sigmask(block ? SIG_BLOCK : SIG_UNBLOCK, &sigset, &old);
        return sigismember(&old, SIGPROF);
    }
}
#endif
