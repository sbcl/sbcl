/*
 * a utility for experimenting with mmap()-at-absolute-address-ranges
 * as a crude way of checking whether it's reasonable for SBCL to
 * reserve those address ranges for itself
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

#include <stdio.h>
#include <sys/types.h>
#include <sys/mman.h>

long
hexparse(char *s)
{
    long result;
    if (1 != sscanf(s, "%lx", &result)) {
        fprintf(stderr, "can't parse \"%s\" as hexadecimal integer\n", s);
        exit(1);
    }
    return result;
}

int
main(int argc, char *argv[])
{
    char *addr;
    char *requested_addr;

    /* FIXME: It would be nice to make the no-command-line-arguments
     * case of this program automatically check all the spaces that
     * SBCL likes to map. Then we could execute this program as a
     * sanity check in make-target-2.sh before we try to execute sbcl
     * itself. */
    if (argc != 3) {
        fprintf(stderr, "usage: %s $addr $size\n", argv[0]);
        exit(1);
    }

    requested_addr = (char*)hexparse(argv[1]);
    addr = mmap(requested_addr,
                hexparse(argv[2]),
                0x7,
                MAP_PRIVATE | MAP_ANON | MAP_FIXED,
                -1,
                0);

    /* FIXME: It would be nice to make this a stronger test. E.g.
     * besides just trying to mmap() the area, we could check that the
     * area is not already mapped (perhaps by checking that attempts
     * to reference the to-be-mapped area cause SIGSEGV or SIGBUS).
     * (At least on OpenBSD, "A successful mmap deletes any previous
     * mapping in the allocated address range.") */
    if (addr != requested_addr) {
        perror("mmap");
    }

    exit(0);
}
