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
INFO_DIR=${INFO_DIR-$INSTALL_ROOT/share/info}
DOC_DIR=${DOC_DIR-$INSTALL_ROOT/share/doc/sbcl}

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
    $BUILD_ROOT$INFO_DIR $BUILD_ROOT$DOC_DIR \
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

echo
echo "SBCL has been installed:"
echo " binary $BUILD_ROOT$INSTALL_ROOT/bin/sbcl"
echo " core and contribs in $BUILD_ROOT$INSTALL_ROOT/lib/sbcl/"

# Installing manual & misc bits of documentation
#
# Locations based on FHS 2.3.
# See: <http://www.pathname.com/fhs/pub/fhs-2.3.html>
#
# share/       architecture independent read-only things
# share/man/   manpages, should be the same as man/
# share/info/  info files
# share/doc/   misc documentation

echo
echo "Documentation:"

# man
cp doc/sbcl.1 $BUILD_ROOT$MAN_DIR/man1/ && echo " man $BUILD_ROOT$MAN_DIR/man1/sbcl.1"

# info
INFO_FILE=doc/manual/sbcl.info
test -f $INFO_FILE && cp $INFO_FILE $BUILD_ROOT$INFO_DIR/ \
    && echo -n " info $BUILD_ROOT$INFO_DIR/sbcl.info" \
    && ( install-info $BUILD_ROOT$INFO_DIR/sbcl.info > /dev/null 2>&1 \
         || echo -n " (could not add to system catalog)" ) \
    && echo

# pdf
PDF_FILE=doc/manual/sbcl.pdf
test -f $PDF_FILE && cp $PDF_FILE $BUILD_ROOT$DOC_DIR/ \
    && echo " pdf $BUILD_ROOT$DOC_DIR/sbcl.pdf"

# html
HTMLS=doc/manual/sbcl
test -d $HTMLS && cp -r $HTMLS $BUILD_ROOT$DOC_DIR/html \
    && echo " html $BUILD_ROOT$DOC_DIR/html/index.html"

for f in BUGS SUPPORT CREDITS COPYING NEWS
do
  cp $f $BUILD_ROOT$DOC_DIR/
done
