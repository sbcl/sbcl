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

ltf=`pwd`/local-target-features.lisp-expr
echo //initializing $ltf
echo '; This is a machine-generated file and should not be edited by hand.' > $ltf
echo -n '(' >> $ltf

echo '//setting up "target"-named symlinks to designate target architecture'
# Currently supported: x86 alpha
sbcl_arch=x86
echo -n ":$sbcl_arch" >> $ltf 
for d in src/compiler src/assembly; do
    echo //setting up symlink $d/target
    original_dir=`pwd`
    cd $d
    if [ -L target ] ; then
	rm target
    elif [ -e target ] ; then
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

echo //setting up OS-dependent information
original_dir=`pwd`
cd src/runtime/
rm -f Config
if [ `uname` = Linux ]; then
    echo -n ' :linux' >> $ltf
    ln -s Config.$sbcl_arch-linux Config
    ( cd ../code && ln -sf $sbcl_arch-linux-types.lisp target-os-types.lisp )
elif uname | grep BSD; then
    echo -n ' :bsd' >> $ltf
    ( cd ../code && ln -sf $sbcl_arch-bsd-types.lisp target-os-types.lisp )
    if [ `uname` = FreeBSD ]; then
	echo -n ' :freebsd' >> $ltf
	ln -s Config.$sbcl_arch-freebsd Config
    elif [ `uname` = OpenBSD ]; then
	echo -n ' :openbsd' >> $ltf
	ln -s Config.$sbcl_arch-openbsd Config
    else
	echo unsupported BSD variant: `uname`
	exit 1
    fi
else
    echo unsupported OS type: `uname`
    exit 1
fi
cd $original_dir

echo //finishing $ltf
echo ')' >> $ltf

# FIXME: The version system should probably be redone along these lines:
#
# echo //setting up version information.
# versionfile=version.txt
# cp base-version.txt $versionfile
# echo " (built `date -u` by `whoami`@`hostname`)" >> $versionfile
# echo 'This is a machine-generated file and should not be edited by hand.' >> $versionfile
