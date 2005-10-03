/*
 * Find the offset of uc_mcontext in a ucontext structure, to enable
 * building on both (glibc-2.3.1 and earlier) and (glibc-2.3.2 and
 * later), after the glibc developers broke source code compatibility.
 * (see also Debian bugs #207806 and #209074)
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
#include <stddef.h>
#include <stdlib.h>
#include <sys/ucontext.h>

int main (int argc, char *argv[]) {

    if(argc != 1) {
        fprintf(stderr,"%s: command line arguments provided.  Don't do that.\n", argv[0]);
        exit(1);
    }

    printf("\
/* This is an automatically-generated file; please do not edit it.\n\
   See the program tools-for-build/where-is-mcontext.c.\n\
 */\n\n");

    printf("\
#ifndef PPC_LINUX_MCONTEXT_H\n\
#define PPC_LINUX_MCONTEXT_H\n\n");

    if (offsetof(ucontext_t,uc_mcontext) > 40) {
        printf("#define GLIBC232_STYLE_UCONTEXT\n\n");
    } else {
        printf("#define GLIBC231_STYLE_UCONTEXT\n\n");
    }
    printf("\
#endif /* PPC_LINUX_MCONTEXT_H */\n");
    exit(0);
}

