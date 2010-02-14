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

#ifndef _FIXNUMP_H
#define _FIXNUMP_H

static inline int fixnump(lispobj obj)
{
    return((obj & FIXNUM_TAG_MASK) == 0);
}

#endif
