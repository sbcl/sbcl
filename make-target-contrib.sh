#!/bin/sh

# This is a script to be run as part of make.sh. The only time you'd
# probably want to run it by itself is if you're trying to
# cross-compile the system or if you're doing some kind of
# troubleshooting.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

SBCL="`pwd`/src/runtime/sbcl --noinform --core `pwd`/output/sbcl.core --userinit /dev/null --sysinit /dev/null --disable-debugger"
SBCL_BUILDING_CONTRIB=1
export SBCL SBCL_BUILDING_CONTRIB
for i in contrib/*; do
    test -d $i || continue;
    # export INSTALL_DIR=$SBCL_HOME/`basename $i `
    make -C $i test 
done
