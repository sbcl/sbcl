#!/bin/sh

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
    *86|x86_64) guessed_sbcl_arch=x86 ;; 
    [Aa]lpha) guessed_sbcl_arch=alpha ;;
    sparc*) guessed_sbcl_arch=sparc ;;
    sun*) guessed_sbcl_arch=sparc ;;
    ppc) guessed_sbcl_arch=ppc ;;
    Power*Macintosh) guessed_sbcl_arch=ppc ;;
    parisc) guessed_sbcl_arch=hppa ;;
    mips) guessed_sbcl_arch=mips ;;
    *)
        # If we're not building on a supported target architecture, we
	# we have no guess, but it's not an error yet, since maybe
	# target architecture will be specified explicitly below.
	guessed_sbcl_arch=''
	;;
esac

echo //setting up CPU-architecture-dependent information
sbcl_arch=${SBCL_ARCH:-$guessed_sbcl_arch}
echo sbcl_arch=\"$sbcl_arch\"
if [ "$sbcl_arch" = "" ] ; then
    echo "can't guess target SBCL architecture, need SBCL_ARCH environment var"
    exit 1
fi
printf ":%s" "$sbcl_arch" >> $ltf 

for d in src/compiler src/assembly; do
    echo //setting up symlink $d/target
    original_dir=`pwd`
    cd $d
    if [ -h target ] ; then
	rm target
    elif [ -w target ] ; then
	echo "I'm afraid to replace non-symlink $d/target with a symlink."
	exit 1
    fi
    if [ -d $sbcl_arch ] ; then
	ln -s $sbcl_arch target
    else
	echo "missing sbcl_arch directory $PWD/$sbcl_arch"
	exit 1
    fi
    cd $original_dir
done

echo //setting up symlink src/compiler/assembly
if [ -h src/compiler/assembly ] ; then
    rm src/compiler/assembly
elif [ -w src/compiler/assembly ] ; then
    echo "I'm afraid to replace non-symlink compiler/assembly with a symlink."
    exit 1
fi
ln -s ../assembly src/compiler/assembly

echo //setting up OS-dependent information
original_dir=`pwd`
cd src/runtime/
rm -f Config target-arch-os.h target-arch.h target-os.h target-lispregs.h
# KLUDGE: these two logically belong in the previous section
# ("architecture-dependent"); it seems silly to enforce this in terms
# of the shell script, though. -- CSR, 2002-02-03
ln -s $sbcl_arch-arch.h target-arch.h
ln -s $sbcl_arch-lispregs.h target-lispregs.h
case `uname` in 
    Linux)
	printf ' :linux' >> $ltf
	sbcl_os="linux"
	if [ "`uname -m`" = "x86_64" ]; then
	    ln -s Config.x86_64-linux Config
	else
	    ln -s Config.$sbcl_arch-linux Config
	fi
	ln -s $sbcl_arch-linux-os.h target-arch-os.h
	ln -s linux-os.h target-os.h
	;;
    OSF1)			
        # it's changed name twice since it was called OSF/1: clearly
        # the marketers forgot to tell the engineers about Digital Unix
        # _or_ OSF/1 ...
	printf ' :osf1' >> $ltf
	sbcl_os="osf1"
        ln -s Config.$sbcl_arch-osf1 Config
	ln -s $sbcl_arch-osf1-os.h target-arch-os.h
	ln -s osf1-os.h target-os.h
	;;
    *BSD)
	printf ' :bsd' >> $ltf
	ln -s $sbcl_arch-bsd-os.h target-arch-os.h
	ln -s bsd-os.h target-os.h
	case `uname` in
	    FreeBSD)
		printf ' :freebsd' >> $ltf
		sbcl_os="freebsd"
		ln -s Config.$sbcl_arch-freebsd Config
		;;
	    OpenBSD)
		printf ' :openbsd' >> $ltf
		sbcl_os="openbsd"
		ln -s Config.$sbcl_arch-openbsd Config
		;;
	    *)
		echo unsupported BSD variant: `uname`
		exit 1
		;;
	esac
	;;
    Darwin)
	printf ' :bsd' >> $ltf
	sbcl_os="darwin"
	ln -s $sbcl_arch-darwin-os.h target-arch-os.h
	ln -s bsd-os.h target-os.h
	printf ' :darwin' >> $ltf
	ln -s Config.$sbcl_arch-darwin Config
	;;
    SunOS)
        printf ' :sunos' >> $ltf
	sbcl_os="sunos"
	ln -s Config.$sbcl_arch-sunos Config
	ln -s $sbcl_arch-sunos-os.h target-arch-os.h
	ln -s sunos-os.h target-os.h
	;;
    *)
	echo unsupported OS type: `uname`
	exit 1
	;;
esac
cd $original_dir

# KLUDGE: currently the x86 only works with the generational garbage
# collector (indicated by the presence of :GENCGC in *FEATURES*) and
# alpha, sparc and ppc with the stop'n'copy collector (indicated by
# the absence of :GENCGC in *FEATURES*). This isn't a great
# separation, but for now, rather than have :GENCGC in
# base-target-features.lisp-expr, we add it into local-target-features
# if we're building for x86. -- CSR, 2002-02-21 Then we do something
# similar with :STACK-GROWS-FOOWARD, too. -- WHN 2002-03-03
if [ "$sbcl_arch" = "x86" ] ; then
    printf ' :gencgc :stack-grows-downward-not-upward :c-stack-is-control-stack' >> $ltf
elif [ "$sbcl_arch" = "mips" ] ; then
    # Use a little C program to try to guess the endianness.  Ware
    # cross-compilers!
    $GNUMAKE -C tools-for-build determine-endianness
    tools-for-build/determine-endianness >> $ltf
elif [ "$sbcl_arch" = "ppc" -a "$sbcl_os" = "linux" ]; then
    # Use a C program to detect which kind of glibc we're building on,
    # to bandage across the break in source compatibility between
    # versions 2.3.1 and 2.3.2
    $GNUMAKE -C tools-for-build where-is-mcontext
    tools-for-build/where-is-mcontext > src/runtime/ppc-linux-mcontext.h
elif [ "$sbcl_arch" = "ppc" -a "$sbcl_os" = "darwin" ]; then
    # The default stack ulimit under darwin is too small to run PURIFY.
    # Best we can do is complain and exit at this stage
    if [ "`ulimit -s`" = "512" ]; then
        echo "Your stack size limit is too small to build SBCL."
        echo "See the limit(1) or ulimit(1) commands and the README file."
        exit 1
    fi
elif [ "$sbcl_arch" = "sparc" ]; then
    # Test the compiler in order to see if we are building on Sun 
    # toolchain as opposed to GNU binutils, and write the appropriate
    # FUNCDEF macro for assembler. No harm in running this on sparc-linux 
    # as well.
    sh tools-for-build/sparc-funcdef.sh > src/runtime/sparc-funcdef.h
else
    # Nothing need be done in this case, but sh syntax wants a placeholder.
    echo > /dev/null
fi

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
echo '"'`hostname`-`whoami`-`date +%F-%H-%M-%S`'"' > output/build-id.tmp

