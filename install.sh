#!/bin/sh

# Install SBCL files into the usual places.

ensure_dirs () 
{
    for j in $*; do 
	test -d $j || mkdir $j
    done;
}

INSTALL_ROOT=${INSTALL_ROOT-/usr/local}
SBCL_SOURCE=`pwd`
if [ -n "$SBCL_HOME" -a "$INSTALL_ROOT/lib/sbcl" != "$SBCL_HOME" ];then
   echo SBCL_HOME environment variable is set, and conflicts with INSTALL_ROOT.
   echo Aborting installation.  Unset one or reset the other, then try again
   echo INSTALL_ROOT="$INSTALL_ROOT"
   echo SBCL_HOME="$SBCL_HOME"
   exit 1
fi
SBCL_HOME=$INSTALL_ROOT/lib/sbcl
export SBCL_HOME
ensure_dirs $INSTALL_ROOT $INSTALL_ROOT/bin $INSTALL_ROOT/lib \
    $INSTALL_ROOT/man $INSTALL_ROOT/man/man1 \
    $SBCL_HOME $SBCL_HOME/systems

test -e $INSTALL_ROOT/bin/sbcl && \
    cp $INSTALL_ROOT/bin/sbcl $INSTALL_ROOT/bin/sbcl.old
test -e $SBCL_HOME/sbcl.core && \
    cp $SBCL_HOME/sbcl.core $SBCL_HOME/sbcl.core.old

cp src/runtime/sbcl $INSTALL_ROOT/bin/
cp output/sbcl.core $SBCL_HOME/sbcl.core
cp doc/sbcl.1 $INSTALL_ROOT/man/man1/

# installing contrib 

SBCL="`pwd`/src/runtime/sbcl --noinform --core `pwd`/output/sbcl.core --userinit /dev/null --sysinit /dev/null --disable-debugger"
SBCL_BUILDING_CONTRIB=1
export SBCL SBCL_BUILDING_CONTRIB

gnumake=${GNUMAKE:-gmake}
for i in contrib/*; do
    test -d $i && test -f $i/test-passed || continue;
    export INSTALL_DIR=$SBCL_HOME/`basename $i `
    ensure_dirs $INSTALL_DIR && $gnumake -C $i install
done
