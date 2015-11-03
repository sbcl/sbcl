#!/bin/sh
set -e

# Install SBCL files into the usual places.

ensure_dirs ()
{
    for j in "$@"; do
         test -d "$j" || mkdir -p "$j"
    done;
}

. output/prefix.def
DEFAULT_INSTALL_ROOT=$SBCL_PREFIX

if [ "$OSTYPE" = "cygwin" -o "$OSTYPE" = "msys" ] ; then
    RUNTIME=sbcl.exe
    OLD_RUNTIME=sbcl.exe.old
else
    RUNTIME=sbcl
    OLD_RUNTIME=sbcl.old
fi
INSTALL_ROOT=${INSTALL_ROOT-$DEFAULT_INSTALL_ROOT}
MAN_DIR=${MAN_DIR-"$INSTALL_ROOT"/share/man}
INFO_DIR=${INFO_DIR-"$INSTALL_ROOT"/share/info}
DOC_DIR=${DOC_DIR-"$INSTALL_ROOT"/share/doc/sbcl}

# Does the environment look sane?
if [ -n "$SBCL_HOME" -a "$INSTALL_ROOT/lib/sbcl" != "$SBCL_HOME" ];then
   echo SBCL_HOME environment variable is set, and conflicts with INSTALL_ROOT.
   echo Aborting installation.  Unset one or reset the other, then try again
   echo INSTALL_ROOT="$INSTALL_ROOT"
   echo SBCL_HOME="$SBCL_HOME"
   exit 1
fi

# Before doing anything else, make sure we have an SBCL to install
if [ -f src/runtime/$RUNTIME ]; then
    if [ -f output/sbcl.core ]; then
        true
    else
        echo "output/sbcl.core not found, aborting installation."
        exit 1
    fi
else
    echo "src/runtime/$RUNTIME not found, aborting installation."
    exit 1
fi

SBCL_HOME="$INSTALL_ROOT"/lib/sbcl
export SBCL_HOME INSTALL_ROOT
ensure_dirs "$BUILD_ROOT$INSTALL_ROOT" "$BUILD_ROOT$INSTALL_ROOT"/bin \
    "$BUILD_ROOT$INSTALL_ROOT"/lib  \
    "$BUILD_ROOT$MAN_DIR" "$BUILD_ROOT$MAN_DIR"/man1 \
    "$BUILD_ROOT$INFO_DIR" "$BUILD_ROOT$DOC_DIR" \
    "$BUILD_ROOT$DOC_DIR"/html \
    "$BUILD_ROOT$SBCL_HOME"

# move old versions out of the way.  Safer than copying: don't want to
# break any running instances that have these files mapped
test -f "$BUILD_ROOT$INSTALL_ROOT"/bin/$RUNTIME && \
 mv "$BUILD_ROOT$INSTALL_ROOT"/bin/$RUNTIME \
    "$BUILD_ROOT$INSTALL_ROOT"/bin/$OLD_RUNTIME
test -f "$BUILD_ROOT$SBCL_HOME"/sbcl.core && \
    mv "$BUILD_ROOT$SBCL_HOME"/sbcl.core "$BUILD_ROOT$SBCL_HOME"/sbcl.core.old

cp src/runtime/$RUNTIME "$BUILD_ROOT$INSTALL_ROOT"/bin/
cp output/sbcl.core "$BUILD_ROOT$SBCL_HOME"/sbcl.core

# installing contrib

. ./sbcl-pwd.sh
sbcl_pwd

SBCL="$SBCL_PWD/src/runtime/sbcl --noinform --core $SBCL_PWD/output/sbcl.core --no-userinit --no-sysinit --disable-debugger"
SBCL_BUILDING_CONTRIB=1
export SBCL SBCL_BUILDING_CONTRIB SBCL_PWD

. ./find-gnumake.sh
find_gnumake

for i in `cd obj/asdf-cache ; echo *`; do
    test -d obj/asdf-cache/$i && test -f obj/asdf-cache/$i/test-passed.test-report || continue;
    INSTALL_DIR="$SBCL_HOME/contrib/"
    export INSTALL_DIR
    ensure_dirs "$BUILD_ROOT$INSTALL_DIR" && $GNUMAKE -C contrib/$i install < /dev/null
done

echo
echo "SBCL has been installed:"
echo " binary $BUILD_ROOT$INSTALL_ROOT/bin/$RUNTIME"
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
cp doc/sbcl.1 "$BUILD_ROOT$MAN_DIR"/man1/ && echo " man $BUILD_ROOT$MAN_DIR/man1/sbcl.1"

# info
for info in doc/manual/*.info
do
  test -e $info && cp $info "$BUILD_ROOT$INFO_DIR"/ \
      && BN=`basename $info` \
      && DIRFAIL=`install-info --info-dir="$BUILD_ROOT$INFO_DIR" \
        "$BUILD_ROOT$INFO_DIR"/$BN > /dev/null 2>&1 \
           || echo "(could not add to system catalog)"` \
      && echo " info $BUILD_ROOT$INFO_DIR/`basename $info` [$BUILD_ROOT$INFO_DIR/dir] $DIRFAIL"
done

for info in doc/manual/*.info-*
do
  test -e $info && cp $info "$BUILD_ROOT$INFO_DIR"/ \
      && echo " info $BUILD_ROOT$INFO_DIR/`basename $info`"
done

# pdf
for pdf in doc/manual/*.pdf
do
  test -e $pdf && cp $pdf "$BUILD_ROOT$DOC_DIR"/ \
      && echo " pdf $BUILD_ROOT$DOC_DIR/`basename $pdf`"
done

# html
for html in doc/manual/sbcl doc/manual/asdf
do
  test -d $html && cp -R -L $html "$BUILD_ROOT$DOC_DIR"/html \
      && echo " html $BUILD_ROOT$DOC_DIR/html/`basename $html`/index.html"
done

for html in doc/manual/sbcl.html doc/manual/asdf.html
do
  test -e $html && cp $html "$BUILD_ROOT$DOC_DIR"/ \
      && echo " html $BUILD_ROOT$DOC_DIR/`basename $html`"
done

for f in BUGS CREDITS COPYING NEWS
do
  test -e $f && cp $f "$BUILD_ROOT$DOC_DIR"/
done
