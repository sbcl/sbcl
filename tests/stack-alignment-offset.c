/* Compiled and run by foreign-stack-alignment.lisp
 *
 * stack_alignment_offset(int) returns the offset of the first argument from a
 * given alignment. run (1) from main, to obtain the good value with no
 * lisp involved (2) from lisp both with and without callbacks to see that
 * we have not messed the alignment.
 *
 * trampoline(int(*)()) is here so that we can get callbacks on the
 * stack too.
 */

/* This software is part of the SBCL system. See the README file for
 * more information.
 *
 * While most of SBCL is derived from the CMU CL system, the test
 * files (like this one) were written from scratch after the fork
 * from CMU CL.
 *
 * This software is in the public domain and is provided with
 * absolutely no warranty. See the COPYING and CREDITS files for
 * more information.
 */

#include <stdio.h>
#include <stdint.h> // for uintptr_t
#include <stdlib.h> // for atoi

/* <nikodemus> bwahahahaaa!
 * <Xophe> oh dear.  He's finally flipped
 * <lisppaste> nikodemus pasted "stack_alignment_offset" at
 *             http://paste.lisp.org/display/13231
 * <Xophe> heh
 * <Xophe> along with a big / * This code is really twisted * / comment :-)
 * <antifuchs> gods.
 */
extern int
stack_alignment_offset (int alignment)
{
    return ((unsigned int)(uintptr_t)&alignment) % alignment;
}

extern int
trampoline (int(*callback)(void))
{
    return callback();
}

int main (int argc, char** argv)
{
    if (argc != 2) {
        printf("wrong number of arguments: %d\n", argc-1);
        return 1;
    }

    printf("%d\n", stack_alignment_offset(atoi(argv[1])));
    return 0;
}
