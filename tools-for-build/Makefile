# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

-include genesis/Makefile.features # for grovel-headers
-include Config

# Do not append to CFLAGS otherwise -I can override the -I below.
CFLAGS:=-I../src/runtime $(CFLAGS)
LDFLAGS:=$(LDFLAGS)
LDLIBS:=$(OS_LIBS)

all: grovel-headers determine-endianness where-is-mcontext mmap-rwx avx2

perfecthash: ../src/runtime/perfecthash.c
	$(CC) -DDEFINE_MAIN $(LINKFLAGS) $(CPPFLAGS) $(CFLAGS) -o $@ $^

clean:
	rm -f *.o grovel-headers determine-endianness where-is-mcontext mmap-rwx avx2 perfecthash
	rm -f *.exe
	rm -fr *.dSYM
