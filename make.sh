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

print_help="no"

# The classic form here was to use --userinit $DEVNULL --sysinit
# $DEVNULL, but that doesn't work on Win32 because SBCL doesn't handle
# device names properly. We still need $DEVNULL to be NUL on Win32
# because it's used elsewhere (such as canonicalize-whitespace), so we
# need an alternate solution for the init file overrides. --no-foos
# have now been available long enough that this should not stop anyone
# from building.
if [ "$OSTYPE" = "cygwin" -o "$OSTYPE" = "msys" ]
then
    SBCL_PREFIX="$PROGRAMFILES/sbcl"
else
    SBCL_PREFIX="/usr/local"
fi
SBCL_XC_HOST="sbcl --disable-debugger --no-userinit --no-sysinit"
export SBCL_XC_HOST

# Parse command-line options.
bad_option() {
    echo $1
    echo "Enter \"$0 --help\" for list of valid options."
    exit 1
}

some_options=false
for option
do
  optarg_ok=true
  # Split --foo=bar into --foo and bar.
  case $option in
      *=*)
        # For ease of scripting treat skip valued options with empty
        # values.
        optarg=`expr "X$option" : '[^=]*=\(.*\)'` || optarg_ok=false
        option=`expr "X$option" : 'X\([^=]*=\).*'`
        ;;
      *)
        optarg=""
        ;;
  esac

  case $option in
      --help | -help | -h)
        print_help="yes" ;;
      --prefix=)
        $optarg_ok && SBCL_PREFIX=$optarg
        ;;
      --xc-host=)
        $optarg_ok && SBCL_XC_HOST=$optarg
        ;;
      --dynamic-space-size=)
        $optarg_ok && SBCL_DYNAMIC_SPACE_SIZE=$optarg
	;;
      -*)
        bad_option "Unknown command-line option to $0: \"$option\""
        ;;
      *)
        if $some_options
        then
            bad_option "Unknown command-line option to $0: \"$option\""
        else
            legacy_xc_spec=$option
        fi
        ;;
  esac
  some_options=true
done

# Previously XC host was provided as a positional argument. 
if test -n "$legacy_xc_spec"
then
    SBCL_XC_HOST="$legacy_xc_spec"
fi

if test "$print_help" = "yes"
then
  cat <<EOF
\`make.sh' drives the SBCL build.

Usage: $0 [OPTION]...

  Important: make.sh does not currently control the entirety of the
  build: configuration file customize-target-features.lisp and certain
  environment variables play a role as well. see file INSTALL for
  details.

Options:
  -h, --help           Display this help and exit.

  --prefix=<path>      Specify the install location.

      Script install.sh installs SBCL under the specified prefix
      path: runtime as prefix/bin/sbcl, additional files under
      prefix/lib/sbcl, and documentation under prefix/share.

      This option also affects the binaries: built-in default for
      SBCL_HOME is: prefix/lib/sbcl/

      Default prefix is: /usr/local

  --dynamic-space-size=<size> Specify default dynamic-space size.

      If not provided, the default is platform-specific. <size> is
      taken to be megabytes unless explicitly suffixed with Gb in
      order to specify the size in gigabytes.

  --xc-host=<string>   Specify the Common Lisp compilation host.

      The string provided should be a command to invoke the
      cross-compilation Lisp system in such a way, that it reads
      commands from standard input, and terminates when it reaches end
      of file on standard input.

      Examples:

       "sbcl --disable-debugger --no-sysinit --no-userinit"
                  Use an existing SBCL binary as a cross-compilation
                  host even though you have stuff in your
                  initialization files which makes it behave in such a
                  non-standard way that it keeps the build from
                  working. Also disable the debugger instead of
                  waiting endlessly for a programmer to help it out
                  with input on *DEBUG-IO*. (This is the default.)

       "sbcl"
                  Use an existing SBCL binary as a cross-compilation
                  host, including your initialization files and
                  building with the debugger enabled. Not recommended
                  for casual users.

       "lisp -noinit -batch"
                  Use an existing CMU CL binary as a cross-compilation
                  host when you have weird things in your .cmucl-init
                  file.
EOF
  exit
fi

build_started=`date`
echo "//Starting build: $build_started"
# Apparently option parsing succeeded. Print out the results.
echo "//Options: --prefix='$SBCL_PREFIX' --xc-host='$SBCL_XC_HOST'"

mkdir -p output
# Save prefix for make and install.sh.
echo "SBCL_PREFIX='$SBCL_PREFIX'" > output/prefix.def
echo "$SBCL_DYNAMIC_SPACE_SIZE" > output/dynamic-space-size.txt

# FIXME: Tweak this script, and the rest of the system, to support
# a second bootstrapping pass in which the cross-compilation host is
# known to be SBCL itself, so that the cross-compiler can do some
# optimizations (especially specializable arrays) that it doesn't
# know how to implement how in a portable way. (Or maybe that wouldn't
# require a second pass, just testing at build-the-cross-compiler time
# whether the cross-compilation host returns suitable values from
# UPGRADED-ARRAY-ELEMENT-TYPE?)

if [ "$OSTYPE" = "cygwin" -o "$OSTYPE" = "msys" ] ; then
    DEVNULL=NUL
else
    DEVNULL=/dev/null
fi
export DEVNULL

. ./find-gnumake.sh
find_gnumake

# If you're cross-compiling, you should probably just walk through the
# make-config.sh script by hand doing the right thing on both the host
# and target machines.
sh make-config.sh

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
time sh make-host-1.sh
time sh make-target-1.sh
time sh make-host-2.sh
time sh make-target-2.sh
time sh make-target-contrib.sh

NCONTRIBS=`find contrib -name Makefile -print | wc -l`
NPASSED=`find contrib -name test-passed -print | wc -l`
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
