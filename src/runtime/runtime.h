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
#else
#define thread_self() pthread_self()
#define thread_equal(a,b) pthread_equal(a,b)
#define thread_sigmask pthread_sigmask
#define mutex_acquire(l) !pthread_mutex_lock(l)
#define mutex_release(l) !pthread_mutex_unlock(l)
#define TryEnterCriticalSection(l) !pthread_mutex_trylock(l)
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
 * The next few defines serve as configuration -- edit them inline if
 * you are a developer and want to affect FSHOW behaviour.
 */

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

#ifdef _WIN64
#define AMD64_SYSV_ABI __attribute__((sysv_abi))
#else
#define AMD64_SYSV_ABI
#endif

#include <sys/types.h>

#define OBJ_FMTX PRIxPTR

static inline int
lowtag_of(lispobj obj)
{
    return obj & LOWTAG_MASK;
}

static inline int
widetag_of(lispobj* obj)
{
#ifdef LISP_FEATURE_BIG_ENDIAN
    return ((unsigned char*)obj)[N_WORD_BYTES-1];
#else
    return *(unsigned char*)obj;
#endif
}
static inline int
header_widetag(lispobj obj)
{
    return obj & WIDETAG_MASK;
}

static inline uword_t
HeaderValue(lispobj obj)
{
  return obj >> N_WIDETAG_BITS;
}

static inline int listp(lispobj obj) {
    return lowtag_of(obj) == LIST_POINTER_LOWTAG;
}
static inline int instancep(lispobj obj) {
    return lowtag_of(obj) == INSTANCE_POINTER_LOWTAG;
}
static inline int functionp(lispobj obj) {
    return lowtag_of(obj) == FUN_POINTER_LOWTAG;
}
static inline int other_pointer_p(lispobj obj) {
    return lowtag_of(obj) == OTHER_POINTER_LOWTAG;
}
static inline int simple_vector_p(lispobj obj) {
    return other_pointer_p(obj) &&
           widetag_of((lispobj*)(obj-OTHER_POINTER_LOWTAG)) == SIMPLE_VECTOR_WIDETAG;
}

/* Is the Lisp object obj something with pointer nature (as opposed to
 * e.g. a fixnum or character or unbound marker)? */
static inline int
is_lisp_pointer(lispobj obj)
{
#ifdef LISP_FEATURE_PPC64
    return (obj & 5) == 4;
#elif N_WORD_BITS == 64
    return (obj & 3) == 3;
#else
    return obj & 1;
#endif
}

/* The standard pointer tagging scheme admits an optimization that cuts the number
 * of instructions down when testing for either 'a' or 'b' (or both) being a tagged
 * pointer, because the bitwise OR of two pointers is considered a pointer.
 * This trick is inadmissible for the PPC64 lowtag arrangement */
#ifdef LISP_FEATURE_PPC64
#define at_least_one_pointer_p(a,b) (is_lisp_pointer(a) || is_lisp_pointer(b))
#else
#define at_least_one_pointer_p(a,b) (is_lisp_pointer(a|b))
#endif

#include "fixnump.h"

/* Is the Lisp object obj something with immediate nature (e.g. a
 * fixnum or character or unbound marker)? */
static inline int
is_lisp_immediate(lispobj obj)
{
    int widetag;
    return (fixnump(obj)
            || ((widetag = header_widetag(obj)) == CHARACTER_WIDETAG)
#if N_WORD_BITS == 64
            || (widetag == SINGLE_FLOAT_WIDETAG)
#endif
            || (widetag == UNBOUND_MARKER_WIDETAG));
}

/* Convert from a lispobj with type bits to a native (ordinary
 * C/assembly) pointer to the beginning of the object. */
static inline lispobj *
native_pointer(lispobj obj)
{
    return (lispobj *) ((uintptr_t) (obj & ~LOWTAG_MASK));
}

/* inverse operation: create a suitably tagged lispobj from a native
 * pointer or integer.*/
static inline lispobj
make_lispobj(void *o, int low_tag)
{
    return (lispobj)o | low_tag;
}

#define make_fixnum(n) ((uword_t)(n) << N_FIXNUM_TAG_BITS)

static inline sword_t
fixnum_value(lispobj n)
{
    return (sword_t)n >> N_FIXNUM_TAG_BITS;
}

#include "align.h"

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

// other_immediate_lowtag_p is the least strict of the tests for whether a word
// is potentially an object header, merely checking whether the bits fit the general
// pattern of header widetags without regard for whether some headered object type
// could in fact have those exact low bits. Specifically, this falsely returns 1
// for UNBOUND_MARKER_WIDETAG, CHARACTER_WIDETAG, and on 64-bit machines,
// SINGLE_FLOAT_WIDETAG; as well as unallocated and unused widetags (e.g. LRA on x86)
// none of which denote the start of a headered object.
// The ambiguous cases are for words would start a cons - the three mentioned above.
// Other cases (NO_TLS_VALUE_MARKER_WIDETAG and other things) do not cause a problem
// in practice because they can't be the first word of a lisp object.
static inline boolean
other_immediate_lowtag_p(lispobj header)
{
    /* These lowtags are spaced 4 apart throughout the lowtag space. */
    return (lowtag_of(header) & 3) == OTHER_IMMEDIATE_0_LOWTAG;
}

// widetag_lowtag encodes in the sign bit whether the byte corresponds
// to a headered object, and in the low bits the lowtag of a tagged pointer
// pointing to this object, be it headered or a cons.
extern unsigned char widetag_lowtag[256];
#define LOWTAG_FOR_WIDETAG(x) (widetag_lowtag[x] & LOWTAG_MASK)

// is_header() and is_cons_half() are logical complements when invoked
// on the first word of any lisp object. However, given a word which is
// only *potentially* the first word of a lisp object, they can both be false.
// In ambiguous root detection, is_cons_half() is to be used, as it is the more
// stringent check. The set of valid bit patterns in the low byte of the car
// of a cons is smaller than the set of patterns accepted by !is_header().
static inline int is_header(lispobj potential_header_word) {
    return widetag_lowtag[potential_header_word & WIDETAG_MASK] & 0x80;
}

static inline int
is_cons_half(lispobj obj)
{
    if (fixnump(obj) || is_lisp_pointer(obj)) return 1;
    int widetag = header_widetag(obj);
    return widetag == CHARACTER_WIDETAG ||
#if N_WORD_BITS == 64
           widetag == SINGLE_FLOAT_WIDETAG ||
#endif
           widetag == UNBOUND_MARKER_WIDETAG;
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
#else
# define GENCGC_IS_PRECISE 0
#endif

void *os_dlsym_default(char *name);

struct lisp_startup_options {
    boolean noinform;
};
extern struct lisp_startup_options lisp_startup_options;

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

/// Object sizing macros. One of these days I'd like to rationalize
/// all our headers making them more intuitive as to which need be
/// included to get access to various things.
/// For the time being, this file makes as much sense as anything else.

/// These sizing macros return the number of *payload* words,
/// exclusive of the object header word. Payload length is always
/// an odd number so that total word count is an even number.

/* Each size category is designed to allow 1 bit for a GC mark bit,
 * possibly some flag bits, and the payload length in words.
 * There are three size categories for most non-vector objects,
 * differing in how many flag bits versus size bits there are.
 * The GC mark bit is always in bit index 31 of the header regardless of
 * machine word size.  Bit index 31 is chosen for consistency between 32-bit
 * and 64-bit machines. It is a natural choice for 32-bit headers by avoiding
 * intererence with other header fields. It is also chosen for 64-bit headers
 * because the upper 32 bits of headers for some objects are already occupied
 * by other data: symbol TLS index, instance layout, etc.
 */

/* The largest payload count is expressed in 23 bits. These objects
 * can't reside in immobile space as there is no room for generation bits.
 * All sorts of objects fall into this category, but mostly due to inertia.
 * There are no non-vector boxed objects whose size should be so large.
 * Header:   size |    tag
 *          -----   ------
 *        23 bits | 8 bits
 */
#define BOXED_NWORDS(obj) ((HeaderValue(obj) & 0x7FFFFF) | 1)

/* Medium-sized payload count is expressed in 15 bits. Objects in this category
 * may reside in immobile space: CLOSURE, FUNCALLABLE-INSTANCE.
 * The single data bit is used as a closure's NAMED flag.
 *
 * Header:  gen# |  data |     size |    tag
 *         -----   -----    -------   ------
 *        8 bits | 1 bit |  15 bits | 8 bits
 */
#define SHORT_BOXED_NWORDS(obj) ((HeaderValue(obj) & SHORT_HEADER_MAX_WORDS) | 1)

/* Tiny payload count is expressed in 8 bits. Objects in this size category
 * can reside in immobile space: SYMBOL, FDEFN.
 * Header:  gen# | flags |   size |    tag
 *         -----   ------  ------   ------
 *        8 bits   8 bits  8 bits | 8 bits
 * FDEFN  flag bits: 1 bit for statically-linked
 * SYMBOL flag bits: 1 bit for present in initial core image
 */
#define TINY_BOXED_NWORDS(obj) ((HeaderValue(obj) & 0xFF) | 1)

#endif /* _SBCL_RUNTIME_H_ */
