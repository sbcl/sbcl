/*
 * Test for the endianness of the target platform (needed for MIPS
 * support, at the very least, as systems with either endianness exist
 * in the wild).
 */

/*
 * This software is part of the SBCL system. See the README file for
 * more information.
 *
 * While most of SBCL is derived from the CMU CL system, many
 * utilities for the build process (like this one) were written from
 * scratch after the fork from CMU CL.
 *
 * This software is in the public domain and is provided with
 * absolutely no warranty. See the COPYING and CREDITS files for
 * more information.
 */

#include <stdio.h>
#include <stdlib.h>

int main (int __attribute__((unused)) argc, char __attribute__((unused)) *argv[]) {
    int foo = 0x20212223;
    char *bar = (char *) &foo;
    switch(*bar) {
    case ' ':
        printf(" :big-endian");
        break;
    case '#':
        printf(" :little-endian");
        break;
    default:
        /* FIXME: How do we do sane error processing in Unix?  This
           program will be called from a script, in a manner somewhat
           like:

               tools-for-build/determine-endianness >> $ltf

           but what if we have a too-smart C compiler that actually
           gets us down to this branch?  I suppose that if we have a C
           compiler that is that smart, we're doomed to miscompile the
           runtime anyway, so we won't get here.  Still, it might be
           good to have "set -e" in the various scripts so that we can
           exit with an error here and have it be caught by the build
           tools.  -- CSR, 2002-11-24
        */
        exit(1);
    }
    exit(0);
}
