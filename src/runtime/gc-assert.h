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

#ifndef _GC_ASSERT_H_
#define _GC_ASSERT_H_

#include "interr.h" /* for lose() */

#define gc_abort()                                                     \
  lose("GC invariant lost, file \"%s\", line %d", __FILE__, __LINE__)

/// Enable extra debug-only checks if DEBUG
#ifdef DEBUG
# define gc_dcheck(ex) gc_assert(ex)
#else
# define gc_dcheck(ex) ((void)0)
#endif

/// Disable all assertions if NDEBUG
#ifdef NDEBUG
# define gc_assert(ex) ((void)0)
#else
# define gc_assert(ex)                                                 \
do {                                                                   \
    if (!(ex)) gc_abort();                                             \
} while (0)
#endif

#endif
