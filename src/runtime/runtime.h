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

#ifndef _SBCL_RUNTIME_H_
#define _SBCL_RUNTIME_H_

#include "lispobj.h"

#ifdef LISP_FEATURE_WIN32
# include "pthreads_win32.h"
#else
# include <signal.h>
# ifdef LISP_FEATURE_SB_THREAD
#  include <pthread.h>
# endif
#endif

#include <stdint.h>
#include <inttypes.h>

#if defined(LISP_FEATURE_SB_THREAD)

#ifdef LISP_FEATURE_WIN32
#define thread_sigmask sb_pthread_sigmask
// wrap CriticalSection operators in a function returning 1 to satisfy assertions
static inline int cs_mutex_lock(void* l) { EnterCriticalSection(l); return 1; }
static inline int cs_mutex_unlock(void* l) { LeaveCriticalSection(l); return 1; }
#define mutex_acquire(l) cs_mutex_lock(l)
#define mutex_release(l) cs_mutex_unlock(l)
#define CONDITION_VAR_WAIT(x,y) SleepConditionVariableCS(x,y,INFINITE)
#define CONDITION_VAR_WAKE_ALL(x) WakeAllConditionVariable(x)
#else
#define thread_self() pthread_self()
#define thread_equal(a,b) pthread_equal(a,b)
#define thread_sigmask pthread_sigmask
#define mutex_acquire(l) !pthread_mutex_lock(l)
#define mutex_release(l) !pthread_mutex_unlock(l)
#define TryEnterCriticalSection(l) !pthread_mutex_trylock(l)
#define CONDITION_VAR_WAIT(x,y) pthread_cond_wait(x,y)
#define CONDITION_VAR_WAKE_ALL(x) pthread_cond_broadcast(x)
#endif

#else
// not SB_THREAD
#define thread_self() 0
#define thread_equal(a,b) ((a)==(b))
#define thread_sigmask sigprocmask
#define mutex_acquire(l) 1
#define mutex_release(l) 1
#define TryEnterCriticalSection(l) 1
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
void gc_state_lock();
void gc_state_wait(gc_phase_t);
int gc_cycle_active(void);
void gc_state_unlock();

#define WITH_GC_STATE_LOCK \
    gc_state_lock(); \
    RUN_BODY_ONCE(gc_state_lock, gc_state_unlock())

#endif

/*
 * Configuration options end here -- the following defines do not
 * generally need customization.
 */

/* Flags defined in a structure to avoid code duplication between
 * declaration and definition. */
extern struct dyndebug_config {
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

void dyndebug_init(void);

#include <sys/types.h>

#define OBJ_FMTX PRIxPTR

#include "align.h"

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

extern void *checked_malloc (size_t size);
extern char *copied_string (char *string);

void *os_dlsym_default(char *name); // Why not in 'os.h' ?

/* Even with just -O1, gcc optimizes the jumps in this "loop" away
 * entirely, giving the ability to define WITH-FOO-style macros. */
#define RUN_BODY_ONCE(prefix, finally_do)               \
    int prefix##done = 0;                               \
    for (; !prefix##done; finally_do, prefix##done = 1)

// casting to void is no longer enough to suppress a warning about unused
// results of libc functions declared with warn_unused_result.
// from http://git.savannah.gnu.org/cgit/gnulib.git/tree/lib/ignore-value.h
#if 3 < __GNUC__ + (4 <= __GNUC_MINOR__)
# define ignore_value(x) \
      (__extension__ ({ __typeof__ (x) __x = (x); (void) __x; }))
#else
# define ignore_value(x) ((void) (x))
#endif

#if defined(__GNUC__) && defined(ADDRESS_SANITIZER)
#define NO_SANITIZE_ADDRESS __attribute__((no_sanitize_address))
#else
#define NO_SANITIZE_ADDRESS
#endif

#if defined(__GNUC__) && defined(MEMORY_SANITIZER)
#define NO_SANITIZE_MEMORY __attribute__((no_sanitize_memory))
#else
#define NO_SANITIZE_MEMORY
#endif

#endif /* _SBCL_RUNTIME_H_ */
