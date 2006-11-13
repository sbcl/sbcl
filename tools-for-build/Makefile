# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

-include genesis/Makefile.features
-include Config

CPPFLAGS:=-I../src/runtime
LDFLAGS:=$(LDFLAGS) $(OS_LIBS)

all: grovel-headers determine-endianness where-is-mcontext \
        modify-ldt-struct-name sigaction-sa-nodefer-works-test

clean:
	rm -f *.o grovel-headers determine-endianness where-is-mcontext \
                modify-ldt-structure-name sigaction-sa-nodefer-works-test
