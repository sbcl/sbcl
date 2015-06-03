#!/bin/sh
set -e

LANG=C
LC_ALL=C
export LANG LC_ALL

# "When we build software, it's a good idea to have a reliable method
# for getting an executable from it. We want any two reconstructions
# starting from the same source to end up in the same result. That's
# just a basic intellectual premise."
#     -- Christian Queinnec, in _Lisp In Small Pieces_, p. 313

# This software is part of the SBCL system. See the README file for
# more information.
#
# This software is derived from the CMU CL system, which was
# written at Carnegie Mellon University and released into the
# public domain. The software is in the public domain and is
# provided with absolutely no warranty. See the COPYING and CREDITS
# files for more information.

# If you're cross-compiling, make-config.sh should "do the right
# thing" when run on the target machine, with the minor caveat that
# any --xc-host parameter should be suitable for the host machine
# instead of the target.
sh make-config.sh "$@" || exit $?

. output/prefix.def
. output/build-config

build_started=`date`
echo "//Starting build: $build_started"
# Apparently option parsing succeeded. Print out the results.
echo "//Options: --prefix='$SBCL_PREFIX' --xc-host='$SBCL_XC_HOST'"

# Enforce the source policy for no bogus whitespace
tools-for-build/canonicalize-whitespace

# The make-host-*.sh scripts are run on the cross-compilation host,
# and the make-target-*.sh scripts are run on the target machine. In
# ordinary compilation, we just do these phases consecutively on the
# same machine, but if you wanted to cross-compile from one machine
# which supports Common Lisp to another which does not (yet:-) support
# Common Lisp, you could do something like this:
#   Create copies of the source tree on both the host and the target.
#   Read the make-config.sh script carefully and emulate it by hand
#     on both machines (e.g. creating "target"-named symlinks to
#     identify the target architecture).
#   On the host system:
#     SBCL_XC_HOST=<whatever> sh make-host-1.sh
#   Copy src/runtime/genesis/*.h from the host system to the target
#     system.
#   On the target system:
#     sh make-target-1.sh
#   Copy src/runtime/sbcl.nm and output/stuff-groveled-from-headers.lisp
#     from the target system to the host system.
#   On the host system:
#     SBCL_XC_HOST=<whatever> sh make-host-2.sh
#   Copy output/cold-sbcl.core from the host system to the target system.
#   On the target system:
#     sh make-target-2.sh
#     sh make-target-contrib.sh
# Or, if you can set up the files somewhere shared (with NFS, AFS, or
# whatever) between the host machine and the target machine, the basic
# procedure above should still work, but you can skip the "copy" steps.
# If you can use rsync on the host machine, you can call make-config.sh
# with:
# --host-location=user@host-machine:<rsync path to host sbcl directory>
# and the make-target-*.sh scripts will take care of transferring the
# necessary files.
time sh make-host-1.sh
time sh make-target-1.sh
time sh make-host-2.sh
time sh make-target-2.sh
time sh make-target-contrib.sh

NCONTRIBS=`find contrib -name Makefile -print | wc -l`
NPASSED=`find obj/asdf-cache -name test-passed.test-report -print | wc -l`
echo
echo "The build seems to have finished successfully, including $NPASSED (out of $NCONTRIBS)"
echo "contributed modules. If you would like to run more extensive tests on"
echo "the new SBCL, you can try:"
echo
echo "  cd tests && sh ./run-tests.sh"
echo
echo "  (All tests should pass on x86/Linux, x86/FreeBSD4, and ppc/Darwin. On"
echo "  other platforms some failures are currently expected; patches welcome"
echo "  as always.)"
echo
echo "To build documentation:"
echo
echo "  cd doc/manual && make"
echo
echo "To install SBCL (more information in INSTALL):"
echo
echo "  sh install.sh"

# This is probably the best place to ensure people will see this.
if test -n "$legacy_xc_spec"
then
    echo <<EOF
******************************************************************************
**
**  Old-style XC-host specification detected: '$SBCL_XC_HOST'
**
**  Since 1.0.41.45 SBCL expects the XC-host to be specified using
**  the --xc-host='myhost' command line option, not with a positional
**  argument. The legacy style still works, but will not be supported
**  indefinitely. Please update your build procedure.
**
******************************************************************************
EOF
fi

build_finished=`date`
echo
echo "//build started:  $build_started"
echo "//build finished: $build_finished"
