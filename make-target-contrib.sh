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

# Just doing CC=${CC:-cc} may be enough, but it needs to be checked
# that cc is available on all platforms.
if [ -z "$CC" ]; then
    if [ -x "`command -v cc`" ]; then
        CC=cc
    else
        CC=gcc
    fi
fi

unset EXTRA_CFLAGS # avoid any potential interference 
export CC LANG LC_ALL

# Load our build configuration
. output/build-config

## All programs spawned by make-target-contrib.sh that use this
## variable or anything derived from it are started with CWD
## contrib/<contrib_name>/. Keeping this a relative pathname to the
## toplevel source directory makes the shell and make portions of the
## build system robust against funny stuff in PWD.
SBCL_TOP="../../"

SBCL_HOME="$SBCL_TOP/obj/sbcl-home"
export SBCL_HOME SBCL_TOP

SBCL="$SBCL_TOP/src/runtime/sbcl --noinform --core $SBCL_TOP/output/sbcl.core \
--lose-on-corruption --disable-debugger --no-sysinit --no-userinit"
SBCL_BUILDING_CONTRIB=1
export SBCL SBCL_BUILDING_CONTRIB

# deleting things here lets us not worry about interaction with stale
# fasls.  This is not good, but is better than :FORCE on each asdf
# operation, because that causes multiple builds of base systems such
# as SB-RT and SB-GROVEL, but FIXME: there's probably a better
# solution.  -- CSR, 2003-05-30
if [ -z "$DONT_CLEAN_SBCL_CONTRIB" ] ; then
  rm -fr obj/sbcl-home/contrib/
  rm -fr obj/asdf-cache/
fi

find output -name 'building-contrib.*' -print | xargs rm -f

# Ignore all source registries.
if [ -z "$*" ]; then
    $GNUMAKE $SBCL_MAKE_JOBS -C contrib
else
    for x in "$@"; do
        $GNUMAKE -C contrib $x.fasl
    done
fi

# Otherwise report expected failures:
HEADER_HAS_BEEN_PRINTED=false
for dir in `cd ./obj/asdf-cache/ ; echo *`; do
  f="obj/asdf-cache/$dir/test-passed.test-report"
  if test -f "$f" && grep -i fail "$f" >/dev/null; then
      if ! $HEADER_HAS_BEEN_PRINTED; then
          cat <<EOF

Note: Test suite failures which are expected for this combination of
platform and features have been ignored:
EOF
          HEADER_HAS_BEEN_PRINTED=true
      fi
      echo "  contrib/$dir"
      (unset IFS; while read line; do echo "    $line"; done <"$f")
  fi
done

if [ -z "$*" ]; then
    contribs_to_build="`cd ./contrib ; echo *`"
else
    contribs_to_build="$*"
fi

# Sometimes people used to see the "No tests failed." output from the last
# DEFTEST in contrib self-tests and think that's all that is. So...
HEADER_HAS_BEEN_PRINTED=false
for dir in $contribs_to_build
do
  if [ -d "contrib/$dir" -a -f "contrib/$dir/Makefile" -a ! -f "obj/asdf-cache/$dir/test-passed.test-report" ]; then
      if $HEADER_HAS_BEEN_PRINTED; then
          echo > /dev/null
      else
          cat <<EOF

WARNING! Some of the contrib modules did not build successfully or pass
their self-tests. Failed contribs:"
EOF
          HEADER_HAS_BEEN_PRINTED=true
      fi
      echo "  $dir"
  fi
done

if [ $HEADER_HAS_BEEN_PRINTED = true ]; then
  exit 1
fi
