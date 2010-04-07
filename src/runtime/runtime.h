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

#if defined(LISP_FEATURE_SB_THREAD)
#define thread_self() pthread_self()
#define thread_kill pthread_kill
#define thread_sigmask pthread_sigmask
#define thread_mutex_lock(l) pthread_mutex_lock(l)
#define thread_mutex_unlock(l) pthread_mutex_unlock(l)
#else
#define thread_self() 0
#define thread_kill kill_safely
#define thread_sigmask sigprocmask
#define thread_mutex_lock(l) 0
#define thread_mutex_unlock(l) 0
#endif

/* Block blockable interrupts for each SHOW, if not 0. */
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
 * problem. */
#define QSHOW_SIGNALS 0
/* Enable low-level debugging output, if not zero. Defaults to enabled
 * if QSHOW_SIGNALS, disabled otherwise. Change it to 1 if you want
 * low-level debugging output but not the whole signal mess. */
#define QSHOW QSHOW_SIGNALS

#if QSHOW

#if QSHOW_SIGNAL_SAFE == 1 && !defined(LISP_FEATURE_WIN32)

#include <signal.h>
extern sigset_t blockable_sigset;

#define QSHOW_BLOCK                                             \
        sigset_t oldset;                                        \
        thread_sigmask(SIG_BLOCK, &blockable_sigset, &oldset);
#define QSHOW_UNBLOCK thread_sigmask(SIG_SETMASK,&oldset,0);
#else
#define QSHOW_BLOCK
#define QSHOW_UNBLOCK
#endif

#ifdef LISP_FEATURE_SB_THREAD
#define QSHOW_PREFIX fprintf(stderr, "%lu ", pthread_self());
#else
#define QSHOW_PREFIX
#endif

#define FSHOW(args)                                             \
    do {                                                        \
        QSHOW_BLOCK                                             \
        QSHOW_PREFIX                                            \
        fprintf args;                                           \
        QSHOW_UNBLOCK                                           \
    } while (0)
#define SHOW(string) FSHOW((stderr, "/%s\n", string))

#else

#define FSHOW(args)
#define SHOW(string)

#endif

#if QSHOW_SIGNALS
#define FSHOW_SIGNAL FSHOW
#else
#define FSHOW_SIGNAL(args)
#endif

/* KLUDGE: These are in theory machine-dependent and OS-dependent, but
 * in practice the "foo int" definitions work for all the machines
 * that SBCL runs on as of 0.6.7. If we port to the Alpha or some
 * other non-32-bit machine we'll probably need real machine-dependent
 * and OS-dependent definitions again. */
/* even on alpha, int happens to be 4 bytes.  long is longer. */
/* FIXME: these names really shouldn't reflect their length and this
   is not quite right for some of the FFI stuff */
typedef unsigned long u64;
typedef signed long s64;
typedef unsigned int u32;
typedef signed int s32;

/* this is an integral type the same length as a machine pointer */
typedef unsigned long pointer_sized_uint_t ;

#include <sys/types.h>

#if defined(LISP_FEATURE_SB_THREAD)
#include <pthread.h>
typedef pthread_t os_thread_t;
#else
typedef pid_t os_thread_t;
#endif

/* FIXME: we do things this way because of the alpha32 port.  once
   alpha64 has arrived, all this nastiness can go away */
#if 64 == N_WORD_BITS
#define LOW_WORD(c) ((pointer_sized_uint_t)c)
typedef unsigned long lispobj;
#else
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

static inline unsigned long
HeaderValue(lispobj obj)
{
  return obj >> N_WIDETAG_BITS;
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
    return obj & 1;
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

static inline lispobj
make_fixnum(long n)
{
    return n << N_FIXNUM_TAG_BITS;
}

static inline long
fixnum_value(lispobj n)
{
    return n >> N_FIXNUM_TAG_BITS;
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
    switch (lowtag_of(header)) {
    case OTHER_IMMEDIATE_0_LOWTAG:
    case OTHER_IMMEDIATE_1_LOWTAG:
#ifdef OTHER_IMMEDIATE_2_LOWTAG
    case OTHER_IMMEDIATE_2_LOWTAG:
#endif
#ifdef OTHER_IMMEDIATE_3_LOWTAG
    case OTHER_IMMEDIATE_3_LOWTAG:
#endif
        return 1;
    default:
        return 0;
    }
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

#define RUNTIME_OPTIONS_MAGIC 0x31EBF355
/* 1 for magic, 1 for boolean, 2 for struct runtime_options fields */
#define RUNTIME_OPTIONS_WORDS (1 + 1 + 2)

struct runtime_options {
    size_t dynamic_space_size;
    size_t thread_control_stack_size;
};

/* saved runtime path computed from argv[0] */
extern char *saved_runtime_path;

#endif /* _SBCL_RUNTIME_H_ */
