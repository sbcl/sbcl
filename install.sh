#!/bin/sh

# Install SBCL files into the usual places.

ensure_dirs () 
{
    for j in $*; do 
	test -d $j || mkdir -p $j
    done;
}

INSTALL_ROOT=${INSTALL_ROOT-/usr/local}
MAN_DIR=${MAN_DIR-$INSTALL_ROOT/share/man}
SBCL_SOURCE=`pwd`
if [ -n "$SBCL_HOME" -a "$INSTALL_ROOT/lib/sbcl" != "$SBCL_HOME" ];then
   echo SBCL_HOME environment variable is set, and conflicts with INSTALL_ROOT.
   echo Aborting installation.  Unset one or reset the other, then try again
   echo INSTALL_ROOT="$INSTALL_ROOT"
   echo SBCL_HOME="$SBCL_HOME"
   exit 1
fi
SBCL_HOME=$INSTALL_ROOT/lib/sbcl
export SBCL_HOME INSTALL_ROOT
ensure_dirs $BUILD_ROOT$INSTALL_ROOT $BUILD_ROOT$INSTALL_ROOT/bin \
    $BUILD_ROOT$INSTALL_ROOT/lib  \
    $BUILD_ROOT$MAN_DIR $BUILD_ROOT$MAN_DIR/man1 \
    $BUILD_ROOT$SBCL_HOME $BUILD_ROOT$SBCL_HOME/systems \
    $BUILD_ROOT$SBCL_HOME/site-systems

# move old versions out of the way.  Safer than copying: don't want to
# break any running instances that have these files mapped
test -f $BUILD_ROOT$INSTALL_ROOT/bin/sbcl && \
    mv $BUILD_ROOT$INSTALL_ROOT/bin/sbcl $BUILD_ROOT$INSTALL_ROOT/bin/sbcl.old
test -f $BUILD_ROOT$SBCL_HOME/sbcl.core && \
    mv $BUILD_ROOT$SBCL_HOME/sbcl.core $BUILD_ROOT$SBCL_HOME/sbcl.core.old

cp src/runtime/sbcl $BUILD_ROOT$INSTALL_ROOT/bin/
cp output/sbcl.core $BUILD_ROOT$SBCL_HOME/sbcl.core
cp doc/sbcl.1 doc/sbcl-asdf-install.1 $BUILD_ROOT$MAN_DIR/man1/

# installing contrib 

SBCL="`pwd`/src/runtime/sbcl --noinform --core `pwd`/output/sbcl.core --userinit /dev/null --sysinit /dev/null --disable-debugger"
SBCL_BUILDING_CONTRIB=1
export SBCL SBCL_BUILDING_CONTRIB

. ./find-gnumake.sh
find_gnumake

for i in contrib/*; do
    test -d $i && test -f $i/test-passed || continue;
    INSTALL_DIR=$SBCL_HOME/`basename $i `
    export INSTALL_DIR
    ensure_dirs $BUILD_ROOT$INSTALL_DIR && $GNUMAKE -C $i install
done
