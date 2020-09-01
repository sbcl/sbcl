# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

# it is illogical that this makefile pulls in Makefile.features,
# which is presumed not to exist, and which when it does exist causes
# confusion as to what flags should be added in for a generic C compile.
# Why do we do this???
-include genesis/Makefile.features
-include Config

CPPFLAGS+=-I../src/runtime
LDFLAGS:=$(LDFLAGS)
LDLIBS:=$(OS_LIBS)

all: grovel-headers determine-endianness where-is-mcontext mmap-rwx

clean:
	rm -f *.o grovel-headers determine-endianness where-is-mcontext mmap-rwx
	rm -f *.exe
