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

/*#define QSHOW */ /* Enable low-level debugging output? */

#ifdef QSHOW
#define FSHOW(args) fprintf args
#define SHOW(string) FSHOW((stderr, "/%s\n", string))
#else
#define FSHOW(args)
#define SHOW(string)
#endif

/* Enable extra-verbose low-level debugging output for signals? (You
 * probably don't want this unless you're trying to debug very early
 * cold boot on a new machine, or one where you've just messed up
 * signal handling.)
 *
 * Note: It may be that doing this is fundamentally unsound, since it
 * causes output from signal handlers, and the i/o libraries aren't
 * necessarily reentrant. But it can still be very convenient for
 * figuring out what's going on when you have a signal handling
 * problem.. */
#define QSHOW_SIGNALS 0

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
lowtag_of(lispobj obj) {
    return obj & LOWTAG_MASK;
}

static inline int
widetag_of(lispobj obj) {
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

/* Convert from a lispobj with type bits to a native (ordinary
 * C/assembly) pointer to the beginning of the object. */
static inline lispobj *
native_pointer(lispobj obj)
{
    return (lispobj *) ((pointer_sized_uint_t) (obj & ~LOWTAG_MASK));
}
/* inverse operation: create a suitably tagged lispobj from a native
 * pointer or integer.  Needs to be a macro due to the tedious C type
 * system */
#define make_lispobj(o,low_tag) ((lispobj)(LOW_WORD(o)|low_tag))

/* FIXME: There seems to be no reason that make_fixnum and fixnum_value
 * can't be implemented as (possibly inline) functions. */
#define make_fixnum(n) ((lispobj)((n)<<N_FIXNUM_TAG_BITS))
#define fixnum_value(n) (((long)n)>>N_FIXNUM_TAG_BITS)

/* Too bad ANSI C doesn't define "bool" as C++ does.. */
typedef int boolean;

/* This only works for static symbols. */
/* FIXME: should be called StaticSymbolFunction, right? */
#define SymbolFunction(sym) \
    (((struct fdefn *)(native_pointer(SymbolValue(sym,0))))->fun)

/* KLUDGE: As far as I can tell there's no ANSI C way of saying
 * "this function never returns". This is the way that you do it
 * in GCC later than version 2.7 or so. If you are using some
 * compiler that doesn't understand this, you could could just
 * change it to "typedef void never_returns" and nothing would
 * break, though you might get a few more bytes of compiled code or
 * a few more compiler warnings. -- WHN 2000-10-21 */
typedef volatile void never_returns;

#endif /* _SBCL_RUNTIME_H_ */
