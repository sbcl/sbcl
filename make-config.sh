#!/bin/sh
set -e

# The make-config.sh script uses information about the target machine
# to set things up for compilation. It's vaguely like a stripped-down
# version of autoconf. It's intended to be run as part of make.sh. The
# only time you'd want to run it by itself is if you're trying to
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

print_help="no"

if [ "$OSTYPE" = "cygwin" -o "$OSTYPE" = "msys" ]
then
    SBCL_PREFIX="$PROGRAMFILES/sbcl"
else
    SBCL_PREFIX="/usr/local"
fi
SBCL_XC_HOST="sbcl --no-userinit --no-sysinit"

# Parse command-line options.
bad_option() {
    echo $1
    echo "Enter \"$0 --help\" for list of valid options."
    exit 1
}

WITH_FEATURES=""
WITHOUT_FEATURES=""
FANCY_FEATURES=":sb-core-compression :sb-xref-for-internals :sb-after-xc-core"

perform_host_lisp_check=no
fancy=false
some_options=false
for option
do
  optarg_ok=true
  # Split --foo=bar into --foo and bar.
  case $option in
      *=*)
        # For ease of scripting skip valued options with empty
        # values.
        optarg=`expr "X$option" : '[^=]*=\(.*\)'` || optarg_ok=false
        option=`expr "X$option" : 'X\([^=]*=\).*'`
        ;;
      --with*)
        optarg=`expr "X$option" : 'X--[^-]*-\(.*\)'` \
            || bad_option "Malformed feature toggle: $option"
        option=`expr "X$option" : 'X\(--[^-]*\).*'`
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
      --arch=)
        $oparg_ok && SBCL_ARCH=$optarg
        ;;
      --xc-host=)
        $optarg_ok && SBCL_XC_HOST=$optarg
        ;;
      --host-location=)
        $optarg_ok && SBCL_HOST_LOCATION=$optarg
        ;;
      --target-location=)
        $optarg_ok && SBCL_TARGET_LOCATION=$optarg
        ;;
      --dynamic-space-size=)
        $optarg_ok && SBCL_DYNAMIC_SPACE_SIZE=$optarg
	;;
      --with)
        WITH_FEATURES="$WITH_FEATURES :$optarg"
        ;;
      --without)
        WITHOUT_FEATURES="$WITHOUT_FEATURES :$optarg"
	;;
      --fancy)
        WITH_FEATURES="$WITH_FEATURES $FANCY_FEATURES"
        # Lower down we add :sb-thread for platforms where it can be built.
        fancy=true
        ;;
      --check-host-lisp)
        perform_host_lisp_check=yes
        ;;
      -*)
        bad_option "Unknown command-line option to $0: \"$option\""
        ;;
      *)
        if $some_options
        then
            bad_option "Unknown command-line option to $0: \"$option\""
        else
            SBCL_XC_HOST=$option
        fi
        ;;
  esac
  some_options=true
done

if (test -f customize-target-features.lisp && \
    (test -n "$WITH_FEATURES" || test -n "$WITHOUT_FEATURES"))
then
    # Actually there's no reason why it would not work, but it would
    # be confusing to say --with-thread only to have it turned off by
    # customize-target-features.lisp...
    echo "ERROR: Both customize-target-features.lisp, and feature-options"
    echo "to make.sh present -- cannot use both at the same time."
    exit 1
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

  --dynamic-space-size=<size> Default dynamic-space size for target.

      This specifies the default dynamic-space size for the SBCL
      being built. If you need to control the dynamic-space size
      of the host SBCL, use the --xc-host option.

      If not provided, the default is platform-specific. <size> is
      taken to be megabytes unless explicitly suffixed with Gb in
      order to specify the size in gigabytes.

  --with-<feature>     Build with specified feature.
  --without-<feature>  Build wihout the specfied feature.

  --fancy              Build with several optional features:

                           $FANCY_FEATURES

                       Plus threading on platforms which support it.

  --arch=<string>      Specify the architecture to build for.

      Mainly for doing x86 builds on x86-64.

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

  --host-location=<string> Location of the source directory on compilation host

      The string is passed to the command rsync to transfer the
      necessary files between the target and host directories during
      the make-target-*.sh steps of cross-compilation (cf. make.sh)

      Examples:

       user@host-machine:/home/user/sbcl
                  Transfer the files to/from directory /home/user/sbcl
                  on host-machine.

EOF
  exit 1
fi

mkdir -p output
echo "SBCL_TEST_HOST=\"$SBCL_XC_HOST\"" > output/build-config
. output/build-config # may come out differently due to escaping

if [ $perform_host_lisp_check = yes ]
then
    if echo '(lisp-implementation-type)' | $SBCL_TEST_HOST; then
        :
    else
        echo "No working host Common Lisp implementation."
        echo 'See ./INSTALL, the "SOURCE DISTRIBUTION" section'
        exit 1
    fi
fi

# Running make.sh with different options without clean.sh in the middle
# can break things.
sh clean.sh

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

. ./find-gnumake.sh
find_gnumake

./generate-version.sh

# Now that we've done our option parsing and found various
# dependencies, write them out to a file to be sourced by other
# scripts.

echo "GNUMAKE=\"$GNUMAKE\"; export GNUMAKE" >> output/build-config
echo "SBCL_XC_HOST=\"$SBCL_XC_HOST\"; export SBCL_XC_HOST" >> output/build-config
if [ -n "$SBCL_HOST_LOCATION" ]; then
    echo "SBCL_HOST_LOCATION=\"$SBCL_HOST_LOCATION\"; export SBCL_HOST_LOCATION" >> output/build-config
fi
if [ -n "$SBCL_TARGET_LOCATION" ]; then
    echo "SBCL_TARGET_LOCATION=\"$SBCL_TARGET_LOCATION\"; export SBCL_TARGET_LOCATION" >> output/build-config
fi

# And now, sorting out the per-target dependencies...

case `uname` in
    Linux)
        sbcl_os="linux"
        ;;
    *BSD)
        case `uname` in
            FreeBSD)
                sbcl_os="freebsd"
                ;;
            GNU/kFreeBSD)
                sbcl_os="gnu-kfreebsd"
                ;;
            OpenBSD)
                sbcl_os="openbsd"
                ;;
            NetBSD)
                sbcl_os="netbsd"
                ;;
            *)
                echo unsupported BSD variant: `uname`
                exit 1
                ;;
        esac
        ;;
    DragonFly)
	sbcl_os="dragonflybsd"
	;;
    Darwin)
        sbcl_os="darwin"
        ;;
    SunOS)
        sbcl_os="sunos"
        ;;
    CYGWIN* | WindowsNT | MINGW* | MSYS*)
        sbcl_os="win32"
        ;;
    Haiku)
        sbcl_os="haiku"
        ;;
    *)
        echo unsupported OS type: `uname`
        exit 1
        ;;
esac

link_or_copy() {
   if [ "$sbcl_os" = "win32" ] ; then
      # Use preprocessor or makefile includes instead of copying if
      # possible, to avoid unexpected use of the original, unchanged
      # files when re-running only make-target-1 during development.
      if echo "$1" | egrep '[.][ch]$'; then
         echo "#include \"$1\"" >"$2"
      elif echo "$1" | egrep '^Config[.]'; then
         echo "include $1" >"$2"
      else
         cp -r "$1" "$2"
      fi
   else
       ln -s "$1" "$2"
   fi
}

remove_dir_safely() {
   if [ "$sbcl_os" = "win32" ] ; then
        if [ -d "$1" ] ; then
            rm -rf "$1"
        elif [ -e "$1" ] ; then
            echo "I'm afraid to remove non-directory $1."
            exit 1
        fi
    else
        if [ -h "$1" ] ; then
            rm "$1"
        elif [ -w "$1" ] ; then
            echo "I'm afraid to replace non-symlink $1 with a symlink."
            exit 1
        fi
    fi
}

echo //entering make-config.sh

echo //ensuring the existence of output/ directory
if [ ! -d output ] ; then mkdir output; fi

echo //guessing default target CPU architecture from host architecture
case `uname -m` in
    *86) guessed_sbcl_arch=x86 ;;
    i86pc) guessed_sbcl_arch=x86 ;;
    *x86_64) guessed_sbcl_arch=x86-64 ;;
    amd64) guessed_sbcl_arch=x86-64 ;;
    sparc*) guessed_sbcl_arch=sparc ;;
    sun*) guessed_sbcl_arch=sparc ;;
    *ppc) guessed_sbcl_arch=ppc ;;
    ppc64) guessed_sbcl_arch=ppc ;;
    ppc64le) guessed_sbcl_arch=ppc64 ;; # is ok because there was never 32-bit LE
    Power*Macintosh) guessed_sbcl_arch=ppc ;;
    ibmnws) guessed_sbcl_arch=ppc ;;
    mips*) guessed_sbcl_arch=mips ;;
    arm64) guessed_sbcl_arch=arm64 ;;
    *arm*) guessed_sbcl_arch=arm ;;
    aarch64) guessed_sbcl_arch=arm64 ;;
    riscv32) guessed_sbcl_arch=riscv xlen=32;;
    riscv64) guessed_sbcl_arch=riscv xlen=64;;
    *)
        # If we're not building on a supported target architecture, we
        # we have no guess, but it's not an error yet, since maybe
        # target architecture will be specified explicitly below.
        guessed_sbcl_arch=''
        ;;
esac

# Under Solaris, uname -m returns "i86pc" even if CPU is amd64.
if [ "$sbcl_os" = "sunos" ] && [ `isainfo -k` = "amd64" ]; then
    guessed_sbcl_arch=x86-64
fi

# Under Darwin, uname -m returns "i386" even if CPU is x86_64.
if [ "$sbcl_os" = "darwin" ] && [ "`/usr/sbin/sysctl -n hw.optional.x86_64`" = "1" ]; then
    guessed_sbcl_arch=x86-64
fi

# Under NetBSD, uname -m returns "evbarm" even if CPU is arm64.
if [ "$sbcl_os" = "netbsd" ] && [ `uname -p` = "aarch64" ]; then
    guessed_sbcl_arch=arm64
fi

echo //setting up CPU-architecture-dependent information
if test -n "$SBCL_ARCH"
then
    # Normalize it.
    SBCL_ARCH=`echo $SBCL_ARCH | tr '[A-Z]' '[a-z]' | tr _ -`
    case $SBCL_ARCH in
        riscv*)
            case $SBCL_ARCH in
                riscv32) SBCL_ARCH=riscv xlen=32;;
                riscv64) SBCL_ARCH=riscv xlen=64;;
                *)
                    echo "Please choose between riscv32 and riscv64."
                    exit 1
            esac
    esac
fi
sbcl_arch=${SBCL_ARCH:-$guessed_sbcl_arch}
echo sbcl_arch=\"$sbcl_arch\"
if [ "$sbcl_arch" = "" ] ; then
    echo "can't guess target SBCL architecture, please specify --arch=<name>"
    exit 1
fi
if $fancy
then
    # If --fancy, enable threads on platforms where they can be built.
    case $sbcl_arch in
        x86|x86-64|ppc|arm64|riscv)
	    if [ "$sbcl_os" = "dragonflybsd" ]
	    then
		echo "No threads on this platform."
	    else
		WITH_FEATURES="$WITH_FEATURES :sb-thread"
		echo "Enabling threads due to --fancy."
	    fi
            ;;
        *)
            echo "No threads on this platform."
            ;;
    esac
else
    case $sbcl_arch in
        x86|x86-64)
            case $sbcl_os in
                linux|darwin)
                    WITH_FEATURES="$WITH_FEATURES :sb-thread"
            esac
    esac
    case $sbcl_arch in
        arm64)
            WITH_FEATURES="$WITH_FEATURES :sb-thread"
    esac
fi

case "$sbcl_os" in
    netbsd)
        # default to using paxctl to disable mprotect restrictions
        if [ "x$(sysctl -n security.pax.mprotect.enabled 2>/dev/null)" = x1 -a \
             "x$SBCL_PAXCTL" = x ]; then
            echo "SBCL_PAXCTL=\"/usr/sbin/paxctl +m\"; export SBCL_PAXCTL" \
                 >> output/build-config
        fi
        ;;
    openbsd)
        # openbsd 6.0 and newer restrict mmap of RWX pages
        if [ `uname -r | tr -d .` -gt 60 ]; then
            rm -f tools-for-build/mmap-rwx
            LDFLAGS="$LDFLAGS -Wl,-zwxneeded" $GNUMAKE -C tools-for-build mmap-rwx -I ../src/runtime
            if ! ./tools-for-build/mmap-rwx; then
                echo "Can't mmap() RWX pages!"
                echo "Is the current filesystem mounted with wxallowed?"
                exit 1
            fi
        fi
    ;;
esac

ltf=`pwd`/local-target-features.lisp-expr
echo //initializing $ltf
echo ';;;; This is a machine-generated file.' > $ltf
echo ';;;; Please do not edit it by hand.' >> $ltf
echo ';;;; See make-config.sh.' >> $ltf
echo "(lambda (features) (set-difference (union features (list :${sbcl_arch}$WITH_FEATURES " >> $ltf

echo //setting up OS-dependent information

original_dir=`pwd`
cd ./src/runtime/
rm -f Config target-arch-os.h target-arch.h target-os.h target-lispregs.h
rm -f sbcl.mk sbcl.o libsbcl.a
# KLUDGE: these two logically belong in the previous section
# ("architecture-dependent"); it seems silly to enforce this in terms
# of the shell script, though. -- CSR, 2002-02-03
link_or_copy $sbcl_arch-arch.h target-arch.h
link_or_copy $sbcl_arch-lispregs.h target-lispregs.h
case "$sbcl_os" in
    linux)
        printf ' :unix :linux :elf' >> $ltf
        case "$sbcl_arch" in
          arm64 | ppc64 | x86 | x86-64)
	        printf ' :gcc-tls' >> $ltf
        esac
        case "$sbcl_arch" in
          arm | arm64 | ppc | ppc64 | x86 | x86-64)
	        printf ' :use-sys-mmap' >> $ltf
        esac

        # If you add other platforms here, don't forget to edit
        # src/runtime/Config.foo-linux too.
        case "$sbcl_arch" in
	    mips | arm | x86 | x86-64)
		printf ' :largefile' >> $ltf
		;;
        esac

        link_or_copy Config.$sbcl_arch-linux Config
        link_or_copy $sbcl_arch-linux-os.h target-arch-os.h
        link_or_copy linux-os.h target-os.h
        ;;
    haiku)
        printf ' :unix :haiku :elf :int4-breakpoints' >> $ltf
        link_or_copy Config.$sbcl_arch-haiku Config
        link_or_copy $sbcl_arch-haiku-os.h target-arch-os.h
        link_or_copy haiku-os.h target-os.h
        ;;
    *bsd)
        printf ' :unix :bsd :elf' >> $ltf
        # FIXME: can we enable :gcc-tls across all variants?
        link_or_copy $sbcl_arch-bsd-os.h target-arch-os.h
        link_or_copy bsd-os.h target-os.h
        case "$sbcl_os" in
            *freebsd)
                printf ' :freebsd' >> $ltf
                printf ' :gcc-tls' >> $ltf
                if [ $sbcl_os = "gnu-kfreebsd" ]; then
                    printf ' :gnu-kfreebsd' >> $ltf
                fi
                link_or_copy Config.$sbcl_arch-$sbcl_os Config
                ;;
            openbsd)
                printf ' :openbsd' >> $ltf
                link_or_copy Config.$sbcl_arch-openbsd Config
                ;;
            netbsd)
                printf ' :netbsd' >> $ltf
                link_or_copy Config.$sbcl_arch-netbsd Config
                ;;
            dragonflybsd)
                printf ' :dragonfly' >> $ltf
                link_or_copy Config.$sbcl_arch-dragonfly Config
                ;;
            *)
                echo unsupported BSD variant: `uname`
                exit 1
                ;;
        esac
        ;;
    darwin)
        printf ' :unix :bsd :darwin :mach-o' >> $ltf
        if [ $sbcl_arch = "x86" ]; then
            printf ' :mach-exception-handler' >> $ltf
        fi
        if [ $sbcl_arch = "x86-64" ]; then
            printf ' :mach-exception-handler' >> $ltf
            darwin_version=`uname -r`
            darwin_version_major=${DARWIN_VERSION_MAJOR:-${darwin_version%%.*}}

            if (( 8 < $darwin_version_major )); then
	        printf ' :inode64' >> $ltf
            fi
        fi
        if [ $sbcl_arch = "arm64" ]; then
            printf ' :darwin-jit :gcc-tls' >> $ltf
        fi
        link_or_copy $sbcl_arch-darwin-os.h target-arch-os.h
        link_or_copy bsd-os.h target-os.h
        link_or_copy Config.$sbcl_arch-darwin Config
        ;;
    sunos)
        printf ' :unix :sunos :elf' >> $ltf
        if [ $sbcl_arch = "x86-64" ]; then
            printf ' :largefile' >> $ltf
        fi
        link_or_copy Config.$sbcl_arch-sunos Config
        link_or_copy $sbcl_arch-sunos-os.h target-arch-os.h
        link_or_copy sunos-os.h target-os.h
        ;;
    win32)
        printf ' :win32' >> $ltf
        #
        # Optional features -- We enable them by default, but the build
        # ought to work perfectly without them:
        #
        printf ' :sb-qshow' >> $ltf
        #
        # Required features -- Some of these used to be optional, but
        # building without them is no longer considered supported:
        #
        # (Of course it doesn't provide dlopen, but there is
        # roughly-equivalent magic nevertheless:)
        printf ' :os-provides-dlopen' >> $ltf
        printf ' :sb-thread :sb-safepoint' >> $ltf
        #
        link_or_copy Config.$sbcl_arch-win32 Config
        link_or_copy $sbcl_arch-win32-os.h target-arch-os.h
        link_or_copy win32-os.h target-os.h
        ;;
    *)
        echo unsupported OS type: `uname`
        exit 1
        ;;
esac
cd "$original_dir"

# FIXME: Things like :c-stack-grows-..., etc, should be
# *derived-target-features* or equivalent, so that there was a nicer
# way to specify them then sprinkling them in this file. They should
# still be tweakable by advanced users, though, but probably not
# appear in *features* of target. #+/- should be adjusted to take
# them in account as well. At minimum the nicer specification stuff,
# though:
#
# (define-feature :dlopen (features)
#   (union '(:bsd :linux :darwin :sunos) features))
#
# (define-feature :c-stack-grows-downwards-not-upwards (features)
#   (member :x86 features))

case "$sbcl_arch" in
  x86)
    if [ "$sbcl_os" = "win32" ]; then
        # of course it doesn't provide dlopen, but there is
        # roughly-equivalent magic nevertheless.
        printf ' :os-provides-dlopen' >> $ltf
    fi
    if [ "$sbcl_os" = "openbsd" ]; then
        rm -f src/runtime/openbsd-sigcontext.h
        sh tools-for-build/openbsd-sigcontext.sh > src/runtime/openbsd-sigcontext.h
    fi
    ;;
  x86-64)
    printf ' :sb-simd-pack :sb-simd-pack-256 :avx2' >> $ltf # not mandatory
    case "$sbcl_os" in
    linux | darwin | *bsd)
        printf ' :immobile-space :immobile-code :compact-instance-header' >> $ltf
    esac
    ;;
  ppc)
    if [ "$sbcl_os" = "linux" ]; then
        # Use a C program to detect which kind of glibc we're building on,
        # to bandage across the break in source compatibility between
        # versions 2.3.1 and 2.3.2
        #
        # FIXME: integrate to grovel-features, mayhaps
	$GNUMAKE -C tools-for-build where-is-mcontext -I ../src/runtime
	tools-for-build/where-is-mcontext > src/runtime/ppc-linux-mcontext.h || (echo "error running where-is-mcontext"; exit 1)
    elif [ "$sbcl_os" = "darwin" ]; then
        # We provide a dlopen shim, so a little lie won't hurt
	printf ' :os-provides-dlopen' >> $ltf
        # The default stack ulimit under darwin is too small to run PURIFY.
        # Best we can do is complain and exit at this stage
	if [ "`ulimit -s`" = "512" ]; then
            echo "Your stack size limit is too small to build SBCL."
            echo "See the limit(1) or ulimit(1) commands and the README file."
            exit 1
	fi
    fi
    ;;
  ppc64)
    # there is no glibc bug that requires the 'where-is-mcontext' hack.
    # (Sufficiently new glibc uses the correct definition, which is the same as
    # 2.3.1, so define our constant for that)
    echo '#define GLIBC231_STYLE_UCONTEXT 1' > src/runtime/ppc-linux-mcontext.h
   ;;
  riscv)
    if [ "$xlen" = "64" ]; then
        printf ' :64-bit' >> $ltf
    elif [ "$xlen" = "32" ]; then
        :
    else
        echo 'Architecture word width unspecified. (Either 32-bit or 64-bit.)'
        exit 1
    fi
    ;;
  sparc)
    # Test the compiler in order to see if we are building on Sun
    # toolchain as opposed to GNU binutils, and write the appropriate
    # FUNCDEF macro for assembler. No harm in running this on sparc-linux
    # as well.
    sh tools-for-build/sparc-funcdef.sh > src/runtime/sparc-funcdef.h
    if [ "$sbcl_os" = "netbsd" ] || [ "$sbcl_os" = "sunos" ] || [ "$sbcl_os" = "linux" ]; then
        printf ' :gencgc' >> $ltf
    else
        echo '***'
        echo '*** You are running SPARC on other than SunOS, NetBSD, or Linux.  Since'
        echo '*** GENCGC is untested on this combination, make-config.sh'
        echo '*** is falling back to CHENEYGC.  Please consider adjusting'
        echo '*** parms.lisp to build with GENCGC instead.'
        echo '***'
        printf ' :cheneygc' >> $ltf
    fi
    ;;
esac

# Use a little C program to try to guess the endianness.  Ware
# cross-compilers!
#
# FIXME: integrate to grovel-features, mayhaps
$GNUMAKE -C tools-for-build determine-endianness -I ../src/runtime
tools-for-build/determine-endianness >> $ltf

export sbcl_os sbcl_arch
sh tools-for-build/grovel-features.sh >> $ltf

echo //finishing $ltf
printf " %s" "`cat crossbuild-runner/backends/${sbcl_arch}/features`" >> $ltf
echo ")) (list$WITHOUT_FEATURES)))" >> $ltf

# FIXME: The version system should probably be redone along these lines:
#
# echo //setting up version information.
# versionfile=version.txt
# cp base-version.txt $versionfile
# echo " (built `date -u` by `whoami`@`hostname`)" >> $versionfile
# echo 'This is a machine-generated file and should not be edited by hand.' >> $versionfile

# Make a unique ID for this build (to discourage people from
# mismatching sbcl and *.core files).
if [ `uname` = "SunOS" ] ; then
  # use /usr/xpg4/bin/id instead of /usr/bin/id
  PATH=/usr/xpg4/bin:$PATH
fi
echo '"'`hostname`-`id -un`-`date +%Y-%m-%d-%H-%M-%S`'"' > output/build-id.inc

if [ -n "$SBCL_HOST_LOCATION" ]; then
    echo //setting up host configuration
    rsync --delete-after -a output/ "$SBCL_HOST_LOCATION/output/"
    rsync -a local-target-features.lisp-expr version.lisp-expr "$SBCL_HOST_LOCATION/"
fi
