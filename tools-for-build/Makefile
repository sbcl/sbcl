# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

-include ../src/runtime/Config

CPPFLAGS=-I../src/runtime

all: grovel-headers determine-endianness where-is-mcontext

clean: 
	rm -f *.o grovel-headers determine-endianness where-is-mcontext
