/* get the sizes and signedness of basic system types. Doing this by
 * hand is basically just too tedious.

 * In the current system this doesn't get built or run automatically,
 * because I (Dan) am lazy and do not want to think too hard about the
 * interaction between generated source files, build trees, and CVS.
 * You have to build it yourself when porting to a new architecture -
 * which I'd guess doesn't happen too often anyway

 * The output from this is generally in code/$(architecture)-$(os)-types.h */

#include <sys/types.h>
#include <sys/times.h>
#include <sys/stat.h>
#include <fcntl.h>

#define DEFTYPE(lispname,cname) { cname foo; \
    printf("(def-alien-type "##lispname##" (%s %d))\n", (((foo=-1)<0) ? "sb!alien:signed" : "unsigned"), (8 * (sizeof foo))); }

#define DEFCONSTANT(lispname,cname) { printf("(defconstant %s %d) ;; 0x%x\n",lispname,cname,cname);} 

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

    DEFTYPE("dev-t",   dev_t);
    DEFTYPE("ino-t",   ino_t);
    DEFTYPE("mode-t",  mode_t);
    DEFTYPE("nlink-t", nlink_t);
    DEFTYPE("uid-t",   uid_t);
    DEFTYPE("gid-t",   gid_t);
    DEFTYPE("clock-t", clock_t);
    DEFTYPE("off-t",   off_t);
    DEFTYPE("time-t",  time_t);

    /* fcntl.h */
    DEFCONSTANT("r_ok", R_OK);
    DEFCONSTANT("w_ok", W_OK);
    DEFCONSTANT("x_ok", X_OK);
    DEFCONSTANT("f_ok", F_OK);

    /* fcntlbits.h */
    DEFCONSTANT("o_rdonly",  O_RDONLY);
    DEFCONSTANT("o_wronly",  O_WRONLY);
    DEFCONSTANT("o_rdwr",    O_RDWR);
    DEFCONSTANT("o_accmode", O_ACCMODE);
    DEFCONSTANT("o_creat",   O_CREAT);
    DEFCONSTANT("o_excl",    O_EXCL);
    DEFCONSTANT("o_noctty",  O_NOCTTY);
    DEFCONSTANT("o_trunc",   O_TRUNC);
    DEFCONSTANT("o_append",  O_APPEND);
    /**/
    DEFCONSTANT( "s-ifmt",  S_IFMT);
    DEFCONSTANT( "s-ififo", S_IFIFO);
    DEFCONSTANT( "s-ifchr", S_IFCHR);
    DEFCONSTANT( "s-ifdir", S_IFDIR);
    DEFCONSTANT( "s-ifblk", S_IFBLK);
    DEFCONSTANT( "s-ifreg", S_IFREG);
  
    DEFCONSTANT( "s-iflnk",  S_IFLNK);
    DEFCONSTANT( "s-ifsock", S_IFSOCK);

    return 0;
}
