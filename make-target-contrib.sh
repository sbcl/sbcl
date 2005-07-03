#!/bin/sh

# This is a script to be run as part of make.sh. The only time you'd
# probably want to run it by itself is if you're cross-compiling the
# system or doing some kind of troubleshooting.

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

LANG=C
export LANG

. ./find-gnumake.sh
find_gnumake

# usually SBCL_HOME refers to the installed root of SBCL, not the
# build directory.  Right now, however, where there are dependencies
# between contrib packages, we want the _uninstalled_ versions to be
# found
SBCL_HOME=`pwd`/contrib
export SBCL_HOME

SBCL="`pwd`/src/runtime/sbcl --noinform --core `pwd`/output/sbcl.core --userinit /dev/null --sysinit /dev/null --disable-debugger"
SBCL_BUILDING_CONTRIB=1
export SBCL SBCL_BUILDING_CONTRIB

# deleting things here lets us not worry about interaction with stale
# fasls.  This is not good, but is better than :FORCE on each asdf
# operation, because that causes multiple builds of base systems such
# as SB-RT and SB-GROVEL, but FIXME: there's probably a better
# solution.  -- CSR, 2003-05-30

find contrib/ \( -name '*.fasl' -o -name 'foo.c' -o -name 'a.out' -o -name 'alien.so' -o -name '*.o' \) \
  -print | xargs rm -f

mkdir -p contrib/systems
rm -f contrib/systems/*

for i in contrib/*/*.asd; do
    ln -sf ../../$i contrib/systems/
done

for i in contrib/*; do
    test -d $i && test -f $i/Makefile || continue;
    # export INSTALL_DIR=$SBCL_HOME/`basename $i `
    test -f $i/test-passed && rm $i/test-passed 
    $GNUMAKE -C $i test && touch $i/test-passed
done

# Sometimes people used to see the "No tests failed." output from the last
# DEFTEST in contrib self-tests and think that's all that is. So...
HEADER_HAS_BEEN_PRINTED=false
for dir in contrib/*
do
  if [ -d "$dir" -a -f "$dir/Makefile" -a ! -f "$dir/test-passed" ]; then
      if $HEADER_HAS_BEEN_PRINTED; then
	  echo > /dev/null
      else
	  echo "Failed contribs:"
	  HEADER_HAS_BEEN_PRINTED=true
      fi
      echo "  `basename $dir`"
  fi
done
