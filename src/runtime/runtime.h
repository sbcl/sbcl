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

#define QSHOW 0 /* Enable low-level debugging output? */
#if QSHOW
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

/* FIXME: There seems to be no reason that LowtagOf can't be defined
 * as a (possibly inline) function instead of a macro. It would also
 * be reasonable to rename the constants in ALL CAPS. */

#define lowtag_Bits 3
#define lowtag_Mask ((1<<lowtag_Bits)-1)
#define LowtagOf(obj) ((obj)&lowtag_Mask)
#define type_Bits 8
#define type_Mask ((1<<type_Bits)-1)

/* FIXME: There seems to be no reason that TypeOf, HeaderValue,
 * Pointerp, PTR, CONS, SYMBOL, and FDEFN can't be defined
 * as (possibly inline) functions instead of macros. */

#define TypeOf(obj) ((obj)&type_Mask)
#define HeaderValue(obj) ((unsigned long) ((obj)>>type_Bits))

#define Pointerp(obj) ((obj) & 0x01)
#define PTR(obj) ((unsigned long)((obj)&~lowtag_Mask))

#define CONS(obj) ((struct cons *)((obj)-type_ListPointer))
#define SYMBOL(obj) ((struct symbol *)((obj)-type_OtherPointer))
#define FDEFN(obj) ((struct fdefn *)((obj)-type_OtherPointer))

/* KLUDGE: These are in theory machine-dependent and OS-dependent, but
 * in practice the "foo int" definitions work for all the machines
 * that SBCL runs on as of 0.6.7. If we port to the Alpha or some
 * other non-32-bit machine we'll probably need real machine-dependent
 * and OS-dependent definitions again. */
#if ((defined alpha) && !(defined linux))
#error No u32,s32 definitions for this platform.  Write some.
#else
/* int happens to be 4 bytes on linux/alpha.  long is longer. */
typedef unsigned int u32;
typedef signed int s32;
#define LOW_WORD(c) ((long)(c) & 0xFFFFFFFFL)
#endif

typedef u32 lispobj;

/* FIXME: There seems to be no reason that make_fixnum and fixnum_value
 * can't be implemented as (possibly inline) functions. */
#define make_fixnum(n) ((lispobj)((n)<<2))
#define fixnum_value(n) (((long)n)>>2)

/* Too bad ANSI C doesn't define "bool" as C++ does.. */
typedef int boolean;

/* FIXME: There seems to be no reason that SymbolValue, SetSymbolValue,
 * and SymbolFunction can't be defined as (possibly inline) functions
 * instead of macros. */

#define SymbolValue(sym) \
    (((struct symbol *)((sym)-type_OtherPointer))->value)
#define SetSymbolValue(sym,val) \
    (((struct symbol *)((sym)-type_OtherPointer))->value = (val))

/* This only works for static symbols. */
/* FIXME: should be called StaticSymbolFunction, right? */
#define SymbolFunction(sym) \
    (((struct fdefn *)(SymbolValue(sym)-type_OtherPointer))->function)

/* KLUDGE: As far as I can tell there's no ANSI C way of saying
 * "this function never returns". This is the way that you do it
 * in GCC later than version 2.7 or so. If you are using some 
 * compiler that doesn't understand this, you could could just
 * change it to "typedef void never_returns" and nothing would
 * break, you might just get a few more bytes of compiled code or
 * a few more compiler warnings. -- WHN 2000-10-21 */
typedef volatile void never_returns;

#endif /* _SBCL_RUNTIME_H_ */
