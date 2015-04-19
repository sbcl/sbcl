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

/* FIXME: Aren't symbols with underscore prefixes supposed to be
 * reserved for system libraries? Perhaps rename stuff like this
 * to names like INCLUDED_SBCL_RUNTIME_H. */
#ifndef _SBCL_RUNTIME_H_
#define _SBCL_RUNTIME_H_

#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD)
# include "pthreads_win32.h"
#else
# include <signal.h>
# ifdef LISP_FEATURE_SB_THREAD
#  include <pthread.h>
# endif
#endif

#include <stdint.h>

#if defined(LISP_FEATURE_SB_THREAD)
#define thread_self() pthread_self()
#define thread_kill pthread_kill

#ifdef LISP_FEATURE_WIN32
#define thread_sigmask _sbcl_pthread_sigmask
#else
#define thread_sigmask pthread_sigmask
#endif

#define thread_mutex_lock(l) pthread_mutex_lock(l)
#define thread_mutex_unlock(l) pthread_mutex_unlock(l)
#else
#define thread_self() 0
#define thread_kill kill_safely
#define thread_sigmask sigprocmask
#define thread_mutex_lock(l) 0
#define thread_mutex_unlock(l) 0
#endif

#if defined(LISP_FEATURE_WIN32) && defined(LISP_FEATURE_SB_THREAD)
void os_preinit();
#endif

#if defined(LISP_FEATURE_SB_SAFEPOINT)

typedef enum {
    GC_NONE=0,
    GC_FLIGHT,
    GC_MESSAGE,
    GC_INVOKED,
    GC_QUIET,
    GC_SETTLED,
    GC_COLLECT,
    GC_NPHASES
}  gc_phase_t;

void map_gc_page();
void unmap_gc_page();
int check_pending_interrupts();
void gc_state_lock();
void gc_state_wait(gc_phase_t);
void gc_state_unlock();

#endif

/*
 * The next few defines serve as configuration -- edit them inline if
 * you are a developer and want to affect FSHOW behaviour.
 */

/* Block blockable interrupts for each SHOW, if not 0.
 * (On Windows, this setting has no effect.)
 *
 * In principle, this is a "configuration option", but I am not aware of
 * any reason why or when it would be advantageous to disable it. */
#define QSHOW_SIGNAL_SAFE 1

/* Enable extra-verbose low-level debugging output for signals? (You
 * probably don't want this unless you're trying to debug very early
 * cold boot on a new machine, or one where you've just messed up
 * signal handling.)
 *
 * Note: It may be that doing this is fundamentally unsound, since it
 * causes output from signal handlers, and the i/o libraries aren't
 * necessarily reentrant. But it can still be very convenient for
 * figuring out what's going on when you have a signal handling
 * problem.
 *
 * Possible values are:
 *   0 -- Never show signal-related output.  There is absolutely no
 *        run-time overhead from FSHOW_SIGNAL in this case.
 *
 *   1 -- (recommended)
 *        Show signal-related output only if selected at run-time
 *        (otherwise almost no run-time overhead).
 *
 *   2 -- Unconditionally show signal-related output.
 *        Very significant overhead.
 *
 * For reasons of tradition, we default to 0 on POSIX and 1 on Windows
 * through :SB-QSHOW.
 *
 * With option 1, set up environment variable SBCL_DYNDEBUG to include
 * "fshow" or "fshow_signal" before starting SBCL to enable output.
 *
 * There is no particular advantage to option 2 except that you do not
 * need to set environment variables in this case.
 */
#ifdef LISP_FEATURE_SB_QSHOW
# define QSHOW_SIGNALS 1
#else
# define QSHOW_SIGNALS 0
#endif

/* Enable low-level debugging output, if not zero. Defaults to enabled
 * if QSHOW_SIGNALS, disabled otherwise. Change it to 1 or 2 if you want
 * low-level debugging output but not the whole signal mess. */
#define QSHOW QSHOW_SIGNALS

/*
 * Configuration options end here -- the following defines do not
 * generally need customization.
 */

#define odxprint(topic, fmt, ...)                       \
    do                                                  \
        if (dyndebug_config.dyndebug_##topic)           \
            odxprint_fun(fmt "\n", ##__VA_ARGS__);      \
    while (0)

void odxprint_fun(const char *fmt, ...);
void fshow_fun(void *ignored, const char *fmt, ...);

/* Flags defined in a structure to avoid code duplication between
 * declaration and definition. */
extern struct dyndebug_config {
    int dyndebug_fshow;
    int dyndebug_fshow_signal;
    int dyndebug_gencgc_verbose;
    int dyndebug_safepoints;
    int dyndebug_seh;
    int dyndebug_misc;
    int dyndebug_pagefaults;
    int dyndebug_backtrace_when_lost;
    int dyndebug_sleep_when_lost;
    int dyndebug_io;
    int dyndebug_runtime_link;
} dyndebug_config;

#ifdef LISP_FEATURE_GENCGC
extern int gencgc_verbose;
#endif

void dyndebug_init(void);

#if QSHOW_SIGNAL_SAFE == 1 && !defined(LISP_FEATURE_WIN32)

extern sigset_t blockable_sigset;

#define QSHOW_BLOCK                                             \
        sigset_t oldset;                                        \
        thread_sigmask(SIG_BLOCK, &blockable_sigset, &oldset)
#define QSHOW_UNBLOCK thread_sigmask(SIG_SETMASK,&oldset,0)
#else
#define QSHOW_BLOCK
#define QSHOW_UNBLOCK
#endif

/* The following macros duplicate the expansion of odxprint, because the
 * extra level of parentheses around `args' prevents us from
 * implementing FSHOW in terms of odxprint directly.  (They also differ
 * in a newline.)
 */

#if QSHOW
# define FSHOW(args) \
    do if (dyndebug_config.dyndebug_fshow) fshow_fun args; while (0)
# define SHOW(string) FSHOW((stderr, "/%s\n", string))
#else
# define FSHOW(args)
# define SHOW(string)
#endif

#if QSHOW_SIGNALS
# define FSHOW_SIGNAL(args)                                             \
    do if (dyndebug_config.dyndebug_fshow_signal) fshow_fun args; while (0)
#else
# define FSHOW_SIGNAL(args)
#endif

/* KLUDGE: These are in theory machine-dependent and OS-dependent, but
 * in practice the "foo int" definitions work for all the machines
 * that SBCL runs on as of 0.6.7. If we port to the Alpha or some
 * other non-32-bit machine we'll probably need real machine-dependent
 * and OS-dependent definitions again. */
/* even on alpha, int happens to be 4 bytes.  long is longer. */
/* FIXME: these names really shouldn't reflect their length and this
   is not quite right for some of the FFI stuff */
#if defined(LISP_FEATURE_WIN32)&&defined(LISP_FEATURE_X86_64)
typedef unsigned long long u64;
typedef signed long long s64;
#else
typedef unsigned long u64;
typedef signed long s64;
#endif
typedef unsigned int u32;
typedef signed int s32;

/* this is an integral type the same length as a machine pointer */
typedef uintptr_t pointer_sized_uint_t;

#ifdef _WIN64
#define AMD64_SYSV_ABI __attribute__((sysv_abi))
#else
#define AMD64_SYSV_ABI
#endif

#include <sys/types.h>

#if defined(LISP_FEATURE_SB_THREAD)
typedef pthread_t os_thread_t;
#else
typedef pid_t os_thread_t;
#endif

typedef uintptr_t uword_t;
typedef intptr_t  sword_t;

/* FIXME: we do things this way because of the alpha32 port.  once
   alpha64 has arrived, all this nastiness can go away */
#if 64 == N_WORD_BITS
#define LOW_WORD(c) ((pointer_sized_uint_t)c)
#define OBJ_FMTX "lx"
typedef uintptr_t lispobj;
#else
#define OBJ_FMTX "x"
#define LOW_WORD(c) ((long)(c) & 0xFFFFFFFFL)
/* fake it on alpha32 */
typedef unsigned int lispobj;
#endif

static inline int
lowtag_of(lispobj obj)
{
    return obj & LOWTAG_MASK;
}

static inline int
widetag_of(lispobj obj)
{
    return obj & WIDETAG_MASK;
}

static inline uword_t
HeaderValue(lispobj obj)
{
  return obj >> N_WIDETAG_BITS;
}

static inline uword_t instance_length(lispobj header)
{
  return (header >> N_WIDETAG_BITS);
}
static inline lispobj instance_layout(lispobj* instance_ptr) // native ptr
{
  return instance_ptr[1]; // the word following the header is the layout
}

static inline struct cons *
CONS(lispobj obj)
{
  return (struct cons *)(obj - LIST_POINTER_LOWTAG);
}

static inline struct symbol *
SYMBOL(lispobj obj)
{
  return (struct symbol *)(obj - OTHER_POINTER_LOWTAG);
}

static inline struct fdefn *
FDEFN(lispobj obj)
{
  return (struct fdefn *)(obj - OTHER_POINTER_LOWTAG);
}

/* Is the Lisp object obj something with pointer nature (as opposed to
 * e.g. a fixnum or character or unbound marker)? */
static inline int
is_lisp_pointer(lispobj obj)
{
#if N_WORD_BITS == 64
    return (obj & 3) == 3;
#else
    return obj & 1;
#endif
}

#include "fixnump.h"

/* Is the Lisp object obj something with immediate nature (e.g. a
 * fixnum or character or unbound marker)? */
static inline int
is_lisp_immediate(lispobj obj)
{
    return (fixnump(obj)
            || (widetag_of(obj) == CHARACTER_WIDETAG)
#if N_WORD_BITS == 64
            || (widetag_of(obj) == SINGLE_FLOAT_WIDETAG)
#endif
            || (widetag_of(obj) == UNBOUND_MARKER_WIDETAG));
}

/* Convert from a lispobj with type bits to a native (ordinary
 * C/assembly) pointer to the beginning of the object. */
static inline lispobj *
native_pointer(lispobj obj)
{
    return (lispobj *) ((pointer_sized_uint_t) (obj & ~LOWTAG_MASK));
}

/* inverse operation: create a suitably tagged lispobj from a native
 * pointer or integer.*/
static inline lispobj
make_lispobj(void *o, int low_tag)
{
    return LOW_WORD(o) | low_tag;
}

#define MAKE_FIXNUM(n) (n << N_FIXNUM_TAG_BITS)
static inline lispobj
make_fixnum(sword_t n)
{
    return MAKE_FIXNUM(n);
}

static inline sword_t
fixnum_value(lispobj n)
{
    return n >> N_FIXNUM_TAG_BITS;
}

static inline sword_t
fixnum_word_value(lispobj n)
{
    /* Convert bytes into words, double-word aligned. */
    sword_t x = ((n >> N_FIXNUM_TAG_BITS) + LOWTAG_MASK) & ~LOWTAG_MASK;

    return x >> WORD_SHIFT;
}

#if defined(LISP_FEATURE_WIN32)
/* KLUDGE: Avoid double definition of boolean by rpcndr.h included via
 * shlobj.h.
 *
 * FIXME: We should probably arrange to use the rpcndr.h boolean on Windows,
 * or get rid of our own boolean type.  If the boolean type is only used in
 * the runtime, and never passed to Lisp, then it doesn't matter which one
 * we use.
 */
#define boolean rpcndr_boolean
#include <shlobj.h>
#undef boolean
#endif
typedef int boolean;

static inline boolean
other_immediate_lowtag_p(lispobj header)
{
    /* These lowtags are spaced 4 apart throughout the lowtag space. */
    return (lowtag_of(header) & 3) == OTHER_IMMEDIATE_0_LOWTAG;
}

/* KLUDGE: As far as I can tell there's no ANSI C way of saying
 * "this function never returns". This is the way that you do it
 * in GCC later than version 2.5 or so. */
#if defined(__GNUC__)
#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 5)
#define never_returns __attribute__ ((noreturn))
#else
#define never_returns
#endif
#else
#define never_returns
#endif

extern void *successful_malloc (size_t size);
extern char *copied_string (char *string);

#if defined(LISP_FEATURE_SB_THREAD) && !defined(LISP_FEATURE_SB_SAFEPOINT)
# define THREADS_USING_GCSIGNAL 1
#endif

/* Now that SPARC has precise GENCGC, several places that used to be
 * #ifdef PCC need adjustment.  Clearly, "PPC or SPARC" is as unhelpful
 * a test as its reverse, "x86 or x86-64".  However, the feature
 * commonly used to differentiate between those two worlds is
 * C_STACK_IS_CONTROL_STACK, and clearly (or at least in my humble
 * opinion), at some point we'd like to have precise GC on x86 while
 * still sharing the C stack, so stack usage ought not imply GC
 * conservativeness.  So let's have a helper feature that makes the code
 * a bit more future-proof, even if it is itself currently defined in
 * the naive way: */
#if defined(LISP_FEATURE_GENCGC) && !defined(LISP_FEATURE_C_STACK_IS_CONTROL_STACK)
# define GENCGC_IS_PRECISE 1
#endif

void *os_dlsym_default(char *name);

#endif /* _SBCL_RUNTIME_H_ */
