/*
 * routines that must be linked into the core for Lisp to work
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

#ifdef sun
#ifndef MACH
#if !defined(SUNOS) && !defined(SOLARIS)
#define SUNOS
#endif
#endif
#endif

typedef int func();

extern func
#define F(x) x,
#if !(defined(irix) || defined(SOLARIS))
/* XXXfixme next line probably wrong; was previous behavior */
#define D(x) x,
#else
#define D(x)
#endif
#include "undefineds.h"
#undef F
#undef D
exit; /* just some function known to exist */

#if defined(SOLARIS) || defined(irix)

#ifdef irix
int errno; /* hack to be sure works with newer libc without having to redump */
           /* causes libc to be relocated to match cmucl rather than vice
            * versa */
#endif

extern int
#define F(x)
#define D(x) x,
#include "undefineds.h"
#undef F
#undef D
errno;                          /* a variable known to exist */

int reference_random_symbols(void) {
   int a;
#define F(x) x();
#define D(x) a+=x;
#include "undefineds.h"
#undef F
#undef D
   return a;
   }

#else

func *reference_random_symbols[] = {
#define F(x) x,
   /* XXXfixme Next line is probably wrong but was previous behavior. */
#define D(x) x,
#include "undefineds.h"
#undef F
#undef D
   exit                         /* a function known to exist */
};

#endif
