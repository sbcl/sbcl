#!/bin/sh
set -e

# This is a script to be run as part of make.sh. The only time you'd
# want to run it by itself is if you're trying to cross-compile the
# system or if you're doing some kind of troubleshooting.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

echo //entering make-target-2.sh

LANG=C
LC_ALL=C
export LANG LC_ALL

# Do warm init stuff, e.g. building and loading CLOS, and stuff which
# can't be done until CLOS is running.
#
# Note that it's normal for the newborn system to think rather hard at
# the beginning of this process (e.g. using nearly 100Mb of virtual memory
# and >30 seconds of CPU time on a 450MHz CPU), and unless you built the
# system with the :SB-SHOW feature enabled, it does it rather silently,
# without trying to tell you about what it's doing. So unless it hangs
# for much longer than that, don't worry, it's likely to be normal.
echo //doing warm init - compilation phase
./src/runtime/sbcl \
--core output/cold-sbcl.core \
--lose-on-corruption \
--no-sysinit --no-userinit < make-target-2.lisp
echo //doing warm init - load and dump phase
./src/runtime/sbcl \
--core output/cold-sbcl.core \
--lose-on-corruption \
--no-sysinit --no-userinit < make-target-2-load.lisp
