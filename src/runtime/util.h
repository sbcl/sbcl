/*
 * miscellaneous utilities
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

/*
 * a utility to accumulate a zero-terminated array of void* values
 *
 * (Ah, lovely C, makes it such a delight to accumulate a collection
 * whose length isn't known in advance.. but it's probably more fun
 * than trying to teach the SBCL debugger to walk g++ stack frames, not to
 * mention dealing with g++'s lovely in-which-file-do-templates-expand
 * issues; or than trying to use Lisp for all accumulation and having to
 * hassle about FFIing all the details of opendir/readdir/closedir
 * and so forth.)
 *
 * We more or less simulate C++-style ctors and dtors.
 */
typedef struct
voidacc { /* the accumulator itself, to be treated as an opaque data type */
/*private:*/
    void **result;
    int n_avail;
    int n_used;
} voidacc;
int voidacc_ctor(voidacc*); /* the ctor, returning 0 for success */
int voidacc_acc(voidacc*, void*); /* Accumulate an element into result,
                                   * returning 0 for success. */
void** voidacc_give_away_result(voidacc*); /* giving away ownership */
void voidacc_dtor(voidacc*); /* the dtor */
