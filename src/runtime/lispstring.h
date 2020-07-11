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

#ifndef _LISPSTRING_H_
#define _LISPSTRING_H_

static inline boolean string_widetag_p(int widetag)
{
    // element type of NIL can just go to hell
    return widetag == SIMPLE_BASE_STRING_WIDETAG
#ifdef SIMPLE_CHARACTER_STRING_WIDETAG
        || widetag == SIMPLE_CHARACTER_STRING_WIDETAG
#endif
        ;
}

// This does not accept (SIMPLE-ARRAY NIL (*))
// (You'd have a pretty bad time trying making a symbol like that)
static inline unsigned int schar(struct vector* string, int index)
{
    if (widetag_of(&string->header) == SIMPLE_BASE_STRING_WIDETAG)
        return ((char*)string->data)[index];
    else
        return ((unsigned int*)string->data)[index];
}

#endif
