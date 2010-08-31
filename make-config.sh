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

case `uname` in
    Linux)
        sbcl_os="linux"
        ;;
    OSF1)
        # it's changed name twice since it was called OSF/1: clearly
        # the marketers forgot to tell the engineers about Digital Unix
        # _or_ OSF/1 ...
        sbcl_os="osf1"
        ;;
    *BSD)
        case `uname` in
            FreeBSD)
                sbcl_os="freebsd"
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
    Darwin)
        sbcl_os="darwin"
        ;;
    SunOS)
        sbcl_os="sunos"
        ;;
    CYGWIN* | WindowsNT | MINGW*)
        sbcl_os="win32"
        ;;
    HP-UX)
        sbcl_os="hpux"
        ;;
    *)
        echo unsupported OS type: `uname`
        exit 1
        ;;
esac

link_or_copy() {
   if [ "$sbcl_os" = "win32" ] ; then
       cp -r "$1" "$2"
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

ltf=`pwd`/local-target-features.lisp-expr
echo //initializing $ltf
echo ';;;; This is a machine-generated file.' > $ltf
echo ';;;; Please do not edit it by hand.' >> $ltf
echo ';;;; See make-config.sh.' >> $ltf
printf '(' >> $ltf

echo //guessing default target CPU architecture from host architecture
case `uname -m` in
    *86) guessed_sbcl_arch=x86 ;;
    i86pc) guessed_sbcl_arch=x86 ;;
    *x86_64) guessed_sbcl_arch=x86-64 ;;
    amd64) guessed_sbcl_arch=x86-64 ;;
    [Aa]lpha) guessed_sbcl_arch=alpha ;;
    sparc*) guessed_sbcl_arch=sparc ;;
    sun*) guessed_sbcl_arch=sparc ;;
    *ppc) guessed_sbcl_arch=ppc ;;
    ppc64) guessed_sbcl_arch=ppc ;;
    Power*Macintosh) guessed_sbcl_arch=ppc ;;
    parisc) guessed_sbcl_arch=hppa ;;
    9000/800) guessed_sbcl_arch=hppa ;;
    mips*) guessed_sbcl_arch=mips ;;
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

echo //setting up CPU-architecture-dependent information
sbcl_arch=${SBCL_ARCH:-$guessed_sbcl_arch}
echo sbcl_arch=\"$sbcl_arch\"
if [ "$sbcl_arch" = "" ] ; then
    echo "can't guess target SBCL architecture, need SBCL_ARCH environment var"
    exit 1
fi
printf ":%s" "$sbcl_arch" >> $ltf

echo //setting up OS-dependent information

# Under Darwin x86-64, guess whether Darwin 9+ or below.
if [ "$sbcl_os" = "darwin" ] && [ "$sbcl_arch" = "x86-64" ]; then
    darwin_version=`uname -r`
    darwin_version_major=${DARWIN_VERSION_MAJOR:-${darwin_version%%.*}}
    if (( 8 < $darwin_version_major )); then
	printf ' :inode64 :darwin9-or-better' >> $ltf
    fi
fi

original_dir=`pwd`
cd ./src/runtime/
rm -f Config target-arch-os.h target-arch.h target-os.h target-lispregs.h
# KLUDGE: these two logically belong in the previous section
# ("architecture-dependent"); it seems silly to enforce this in terms
# of the shell script, though. -- CSR, 2002-02-03
link_or_copy $sbcl_arch-arch.h target-arch.h
link_or_copy $sbcl_arch-lispregs.h target-lispregs.h
case "$sbcl_os" in
    linux)
        printf ' :unix' >> $ltf
        printf ' :elf' >> $ltf
        printf ' :linux' >> $ltf

        # If you add other platforms here, don't forget to edit
        # src/runtime/Config.foo-linux too.
        case "$sbcl_arch" in
	    mips)
		printf ' :largefile' >> $ltf
		;;
            x86 | x86-64)
		printf ' :sb-thread :largefile' >> $ltf
		;;
        esac

        if [ $sbcl_arch = "x86-64" ]; then
            link_or_copy Config.x86_64-linux Config
        else
            link_or_copy Config.$sbcl_arch-linux Config
        fi
        link_or_copy $sbcl_arch-linux-os.h target-arch-os.h
        link_or_copy linux-os.h target-os.h
        ;;
    osf1)
        printf ' :unix' >> $ltf
        printf ' :elf' >> $ltf
        printf ' :osf1' >> $ltf
        link_or_copy Config.$sbcl_arch-osf1 Config
        link_or_copy $sbcl_arch-osf1-os.h target-arch-os.h
        link_or_copy osf1-os.h target-os.h
        ;;
    hpux)
        printf ' :unix' >> $ltf
        printf ' :elf' >> $ltf
        printf ' :hpux' >> $ltf
        link_or_copy Config.$sbcl_arch-hpux Config
        link_or_copy $sbcl_arch-hpux-os.h target-arch-os.h
        link_or_copy hpux-os.h target-os.h
        ;;
    *bsd)
        printf ' :unix' >> $ltf
        printf ' :bsd' >> $ltf
        link_or_copy $sbcl_arch-bsd-os.h target-arch-os.h
        link_or_copy bsd-os.h target-os.h
        case "$sbcl_os" in
            freebsd)
                printf ' :elf' >> $ltf
                printf ' :freebsd' >> $ltf
                printf ' :gcc-tls' >> $ltf
                if [ $sbcl_arch = "x86" ]; then
                    printf ' :restore-tls-segment-register-from-context' >> $ltf
                fi
                link_or_copy Config.$sbcl_arch-freebsd Config
                ;;
            openbsd)
                printf ' :elf' >> $ltf
                printf ' :openbsd' >> $ltf
                link_or_copy Config.$sbcl_arch-openbsd Config
                ;;
            netbsd)
                printf ' :netbsd' >> $ltf
                printf ' :elf' >> $ltf
                link_or_copy Config.$sbcl_arch-netbsd Config
                ;;
            *)
                echo unsupported BSD variant: `uname`
                exit 1
                ;;
        esac
        ;;
    darwin)
        printf ' :unix' >> $ltf
        printf ' :mach-o' >> $ltf
        printf ' :bsd' >> $ltf
        printf ' :darwin' >> $ltf
        if [ $sbcl_arch = "x86" ]; then
            printf ' :mach-exception-handler :sb-lutex :restore-fs-segment-register-from-tls :ud2-breakpoints' >> $ltf
        fi
        if [ $sbcl_arch = "x86-64" ]; then
            printf ' :mach-exception-handler :sb-lutex :ud2-breakpoints' >> $ltf
        fi
        link_or_copy $sbcl_arch-darwin-os.h target-arch-os.h
        link_or_copy bsd-os.h target-os.h
        link_or_copy Config.$sbcl_arch-darwin Config
        ;;
    sunos)
        printf ' :unix' >> $ltf
        printf ' :elf' >> $ltf
        printf ' :sunos' >> $ltf
        if [ $sbcl_arch = "x86" ] || [ $sbcl_arch = "amd64" ]; then
            printf ' :sb-lutex' >> $ltf
        fi
        link_or_copy Config.$sbcl_arch-sunos Config
        link_or_copy $sbcl_arch-sunos-os.h target-arch-os.h
        link_or_copy sunos-os.h target-os.h
        ;;
    win32)
        printf ' :win32' >> $ltf
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
# appear in *features* of target. #!+/- should be adjusted to take
# them in account as well. At minimum the nicer specification stuff,
# though:
#
# (define-feature :dlopen (features)
#   (union '(:bsd :linux :darwin :sunos) features))
#
# (define-feature :c-stack-grows-downwards-not-upwards (features)
#   (member :x86 features))

# KLUDGE: currently the x86 only works with the generational garbage
# collector (indicated by the presence of :GENCGC in *FEATURES*) and
# alpha, sparc and ppc with the stop'n'copy collector (indicated by
# the absence of :GENCGC in *FEATURES*). This isn't a great
# separation, but for now, rather than have :GENCGC in
# base-target-features.lisp-expr, we add it into local-target-features
# if we're building for x86. -- CSR, 2002-02-21 Then we do something
# similar with :STACK-GROWS-FOOWARD, too. -- WHN 2002-03-03
if [ "$sbcl_arch" = "x86" ]; then
    printf ' :gencgc :stack-grows-downward-not-upward :c-stack-is-control-stack' >> $ltf
    printf ' :compare-and-swap-vops :unwind-to-frame-and-call-vop :raw-instance-init-vops' >> $ltf
    printf ' :stack-allocatable-closures :stack-allocatable-vectors' >> $ltf
    printf ' :stack-allocatable-lists :stack-allocatable-fixed-objects' >> $ltf
    printf ' :alien-callbacks :cycle-counter :inline-constants ' >> $ltf
    printf ' :memory-barrier-vops' >> $ltf
    case "$sbcl_os" in
    linux | freebsd | netbsd | openbsd | sunos | darwin | win32)
        printf ' :linkage-table' >> $ltf
    esac
    if [ "$sbcl_os" = "win32" ]; then
        # of course it doesn't provide dlopen, but there is
        # roughly-equivalent magic nevertheless.
        printf ' :os-provides-dlopen' >> $ltf
    fi
    if [ "$sbcl_os" = "openbsd" ]; then
        rm -f src/runtime/openbsd-sigcontext.h
        sh tools-for-build/openbsd-sigcontext.sh > src/runtime/openbsd-sigcontext.h
    fi
elif [ "$sbcl_arch" = "x86-64" ]; then
    printf ' :gencgc :stack-grows-downward-not-upward :c-stack-is-control-stack :linkage-table' >> $ltf
    printf ' :compare-and-swap-vops :unwind-to-frame-and-call-vop :raw-instance-init-vops' >> $ltf
    printf ' :stack-allocatable-closures :stack-allocatable-vectors' >> $ltf
    printf ' :stack-allocatable-lists :stack-allocatable-fixed-objects' >> $ltf
    printf ' :alien-callbacks :cycle-counter :complex-float-vops' >> $ltf
    printf ' :float-eql-vops :inline-constants :memory-barrier-vops' >> $ltf
elif [ "$sbcl_arch" = "mips" ]; then
    printf ' :linkage-table' >> $ltf
    printf ' :stack-allocatable-closures :stack-allocatable-vectors' >> $ltf
    printf ' :stack-allocatable-lists :stack-allocatable-fixed-objects' >> $ltf
    printf ' :alien-callbacks' >> $ltf
    # Use a little C program to try to guess the endianness.  Ware
    # cross-compilers!
    #
    # FIXME: integrate to grovel-features, mayhaps
    $GNUMAKE -C tools-for-build determine-endianness -I ../src/runtime
    tools-for-build/determine-endianness >> $ltf
elif [ "$sbcl_arch" = "ppc" ]; then
    printf ' :gencgc :stack-allocatable-closures :stack-allocatable-lists' >> $ltf
    printf ' :linkage-table :raw-instance-init-vops :memory-barrier-vops' >> $ltf
    printf ' :compare-and-swap-vops' >> $ltf
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
	printf " :os-provides-dlopen :alien-callbacks" >> $ltf
        # The default stack ulimit under darwin is too small to run PURIFY.
        # Best we can do is complain and exit at this stage
	if [ "`ulimit -s`" = "512" ]; then
            echo "Your stack size limit is too small to build SBCL."
            echo "See the limit(1) or ulimit(1) commands and the README file."
            exit 1
	fi
    fi
elif [ "$sbcl_arch" = "sparc" ]; then
    # Test the compiler in order to see if we are building on Sun
    # toolchain as opposed to GNU binutils, and write the appropriate
    # FUNCDEF macro for assembler. No harm in running this on sparc-linux
    # as well.
    sh tools-for-build/sparc-funcdef.sh > src/runtime/sparc-funcdef.h
    if [ "$sbcl_os" = "sunos" ] || [ "$sbcl_os" = "linux" ]; then
        printf ' :linkage-table' >> $ltf
    fi
    printf ' :stack-allocatable-closures :stack-allocatable-lists' >> $ltf
elif [ "$sbcl_arch" = "alpha" ]; then
    printf ' :stack-allocatable-closures :stack-allocatable-lists' >> $ltf
elif [ "$sbcl_arch" = "hppa" ]; then
    printf ' :stack-allocatable-vectors :stack-allocatable-fixed-objects' >> $ltf
    printf ' :stack-allocatable-lists' >> $ltf
else
    # Nothing need be done in this case, but sh syntax wants a placeholder.
    echo > /dev/null
fi

export sbcl_os sbcl_arch
sh tools-for-build/grovel-features.sh >> $ltf

echo //finishing $ltf
echo ')' >> $ltf

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
echo '"'`hostname`-`id -un`-`date +%Y-%m-%d-%H-%M-%S`'"' > output/build-id.tmp
