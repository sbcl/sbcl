/*
 * Rummage through the system header files using the C compiler itself
 * as a parser, extracting stuff like preprocessor constants and the
 * sizes and signedness of basic system types, and write it out as
 * Lisp code.
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
#include <sys/types.h>
#include <sys/times.h>
#include <sys/stat.h>
#include <fcntl.h>

#define DEFTYPE(lispname,cname) { cname foo; \
    printf("(def-alien-type "##lispname##" (%s %d))\n", (((foo=-1)<0) ? "sb!alien:signed" : "unsigned"), (8 * (sizeof foo))); }

void
defconstant(char* lisp_name, long unix_number)
{
    printf("(defconstant %s %ld) ; #x%lx\n",
	   lisp_name, unix_number, unix_number);
}

int
main(int argc, char *argv[])
{
    /* don't need no steenking command line arguments */
    if (1 != argc) {
	fprintf(stderr, "argh! command line argument(s)\n");
	exit(1);
    }

    /* don't need no steenking hand-editing */
    printf(
";;;; This is an automatically generated file, please do not hand-edit it.
;;;; See the program \"grovel_headers.c\".

");

    printf("(in-package \"SB!UNIX\")\n\n");

    printf(";;; types, types, types\n");
    DEFTYPE("dev-t",   dev_t);
    DEFTYPE("ino-t",   ino_t);
    DEFTYPE("mode-t",  mode_t);
    DEFTYPE("nlink-t", nlink_t);
    DEFTYPE("uid-t",   uid_t);
    DEFTYPE("gid-t",   gid_t);
    DEFTYPE("clock-t", clock_t);
    DEFTYPE("off-t",   off_t);
    DEFTYPE("time-t",  time_t);
    printf("\n");

    printf(";;; fcntl.h\n");
    defconstant("r_ok", R_OK);
    defconstant("w_ok", W_OK);
    defconstant("x_ok", X_OK);
    defconstant("f_ok", F_OK);
    printf("\n");

    printf(";;; fcntlbits.h\n");
    defconstant("o_rdonly",  O_RDONLY);
    defconstant("o_wronly",  O_WRONLY);
    defconstant("o_rdwr",    O_RDWR);
    defconstant("o_accmode", O_ACCMODE);
    defconstant("o_creat",   O_CREAT);
    defconstant("o_excl",    O_EXCL);
    defconstant("o_noctty",  O_NOCTTY);
    defconstant("o_trunc",   O_TRUNC);
    defconstant("o_append",  O_APPEND);
    printf(";;;\n");
    defconstant( "s-ifmt",  S_IFMT);
    defconstant( "s-ififo", S_IFIFO);
    defconstant( "s-ifchr", S_IFCHR);
    defconstant( "s-ifdir", S_IFDIR);
    defconstant( "s-ifblk", S_IFBLK);
    defconstant( "s-ifreg", S_IFREG);
    printf("\n");
  
    defconstant( "s-iflnk",  S_IFLNK);
    defconstant( "s-ifsock", S_IFSOCK);
    printf("\n");

    return 0;
}
