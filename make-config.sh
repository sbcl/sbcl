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
sbcl_arch=x86 # (the only possibility supported, at least as of sbcl-0.6.7)
echo -n ":x86" >> $ltf # (again, the only possibility supported)
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
cd src/runtime/
rm -f Config
if [ `uname` = Linux ]; then
    echo -n ' :linux' >> $ltf
    ln -s Config.x86-linux Config
elif uname | grep BSD; then
    if [ `uname` = FreeBSD ]; then
	echo -n ' :freebsd' >> $ltf
    elif [ `uname` = OpenBSD ]; then
	echo -n ' :openbsd' >> $ltf
    else
	echo unsupported BSD variant: `uname`
	exit 1
    fi
    echo -n ' :bsd' >> $ltf
    ln -s Config.x86-bsd Config
else
    echo unsupported OS type: `uname`
    exit 1
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
