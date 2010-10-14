#!/bin/sh
set -e

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

echo //entering make-target-contrib.sh

LANG=C
LC_ALL=C
export LANG LC_ALL

. ./find-gnumake.sh
find_gnumake

. ./sbcl-pwd.sh
sbcl_pwd

SBCL_HOME="$SBCL_PWD/contrib"
export SBCL_HOME
if [ "$OSTYPE" = "cygwin" ] ; then
    SBCL_PWD=`echo $SBCL_PWD | sed s/\ /\\\\\\\\\ /g`
fi

SBCL="$SBCL_PWD/src/runtime/sbcl --noinform --core $SBCL_PWD/output/sbcl.core \
--lose-on-corruption --disable-debugger --no-sysinit --no-userinit"
SBCL_BUILDING_CONTRIB=1
export SBCL SBCL_BUILDING_CONTRIB

# deleting things here lets us not worry about interaction with stale
# fasls.  This is not good, but is better than :FORCE on each asdf
# operation, because that causes multiple builds of base systems such
# as SB-RT and SB-GROVEL, but FIXME: there's probably a better
# solution.  -- CSR, 2003-05-30

find contrib/ \( -name '*.fasl' -o \
                 -name '*.FASL' -o \
                 -name 'foo.c' -o \
                 -name 'FOO.C' -o \
                 -name 'a.out' -o \
                 -name 'A.OUT' -o \
                 -name 'alien.so' -o \
                 -name 'ALIEN.SO' -o \
                 -name '*.o' -o \
                 -name '*.O' \) \
  -print | xargs rm -f

find output -name 'building-contrib.*' -print | xargs rm -f

# Ignore all source registries.
CL_SOURCE_REGISTRY='(:source-registry :ignore-inherited-configuration)'
export CL_SOURCE_REGISTRY

for i in contrib/*; do
    test -d $i && test -f $i/Makefile || continue;
    # export INSTALL_DIR=$SBCL_HOME/`basename $i `
    test -f $i/test-passed && rm $i/test-passed
    # hack to get exit codes right.
    if $GNUMAKE -C $i test 2>&1 && touch $i/test-passed ; then
	:
    else
	exit $?
    fi | tee output/building-contrib.`basename $i` 
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
          cat <<EOF

WARNING! Some of the contrib modules did not build successfully or pass
their self-tests. Failed contribs:"
EOF
          HEADER_HAS_BEEN_PRINTED=true
      fi
      echo "  `basename $dir`"
  fi
done

if [ $HEADER_HAS_BEEN_PRINTED = true ]; then
  exit 1
fi
