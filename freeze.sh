#! /bin/bash

# Sourceforge username
SFUSER=${SFUSER:-$USER}

set -ex

usage() {
    if ! [ -z "$1" ]
    then
        echo $1
    fi
    cat <<EOF

usage: $0 VERSION

  This script performs a number of sanity checks on the current repository, and creates
  a lightweight tag for the revision in \$PWD/release.sbcl (or does it?).

  ENVIRONMENT:

  SFUSER: Sourceforge username. Defaults to \$USER
  SBCL_RELEASE_DIR: Absolute path to directory containing all the build
    artifacts. If not defined, a new build directory is created in \$PWD.
  CCL: Path to Clozure Common Lisp, suitable as an argument to make.sh
EOF
    exit 1
}

if [ -z "$1" ]
then
    usage "No version number."
else
    VERSION="$1-rc"; shift
    echo $VERSION | perl -pe 'die "Invalid version number: $_\n" if !/^\d+\.\d+\.\d+-rc$/'
fi

if ! [ -z "$@" ]
then
    usage "Extra command-line arguments: $@"
fi

if [ -z "$CCL" ]
then
    usage "CCL environment variable not set"
fi

SBCL_RELEASE_DIR=${SBCL_RELEASE_DIR:-$(mktemp -d $PWD/sbcl-release-dir-$(date +%Y%m%d)-XXXXXXXXX)}
SBCL_DIR=$SBCL_RELEASE_DIR/sbcl-$VERSION
GIT_DIR=$PWD/release.sbcl
LOGFILE=$SBCL_RELEASE_DIR/log.txt

if [ ! -d $SBCL_DIR ]; then
    cd $GIT_DIR

    sbcl_directory="$(cd "$(dirname $0)"; pwd)"

    echo "Checking that the tree is clean."
    if ! [ $(git status --porcelain | wc -l) = 0 ]
    then
        echo "There are uncommitted / unpushed changes in this checkout!"
        git status
        exit 1
    fi

    ## Find the current version using git describe
    git_describe_version="$(git describe | grep -E '^sbcl-[0-9]\.[0-9]+\.[0-9]+' | sed -E 's/^sbcl-([0-9]\.[0-9]+\.[0-9]+)-.*$/\1/')"
    ## Check NEWS

    echo "Checking NEWS"
    if ! grep  "^changes relative to sbcl-$git_describe_version:" NEWS > /dev/null
    then
        echo "NEWS not in correct format!"
        exit 1
    fi

    ## Make draft release notes

    if [ ! -f $SBCL_RELEASE_DIR/sbcl-$VERSION-release-notes.txt ]; then
        awk "BEGIN { state = 0 }
     /^changes relative to sbcl-/ { state = 1 }
     /^changes in sbcl-/ { state = 0 }
     { if(state == 1) print \$0 }" < $GIT_DIR/NEWS > $SBCL_RELEASE_DIR/sbcl-$VERSION-release-notes.txt
    fi

    ## Tag

    # I'd like to use the same tag each time, but I can't convince
    # myself that that will do the right thing when pushed, and I don't
    # want to break all our mirrors for this.

    # echo "Tagging as release_candidate"
    # git tag release_candidate

    git clone $GIT_DIR $SBCL_DIR
fi

# check self-build (without float oracle)

## Build x86-64 binary for bootstrap.

if [ ! -d $SBCL_RELEASE_DIR/bin ]; then
    echo "Building bootstrap x86-64 binary"
    cd $SBCL_DIR
    nice -20 ./make.sh >$LOGFILE 2>&1

    cd tests
    nice -20 sh ./run-tests.sh >>$LOGFILE 2>&1
    mkdir -p $SBCL_RELEASE_DIR/bin
    cp $SBCL_DIR/src/runtime/sbcl $SBCL_RELEASE_DIR/bin/sbcl
    cp $SBCL_DIR/output/sbcl.core $SBCL_RELEASE_DIR/bin/sbcl.core
fi

## Build x86-64 release candidate binary.

if [ ! -d $SBCL_RELEASE_DIR/sbcl-$VERSION-x86-64-linux ]; then
    echo "Building release candidate x86-64 binary"
    cd $SBCL_DIR
    sh clean.sh
    nice -20 ./make.sh "$SBCL_RELEASE_DIR/bin/sbcl --core $SBCL_RELEASE_DIR/bin/sbcl.core --no-userinit" >> $LOGFILE 2>&1
    cd doc && sh ./make-doc.sh
    cd $SBCL_RELEASE_DIR

    ln -s $SBCL_DIR $SBCL_RELEASE_DIR/sbcl-$VERSION-x86-64-linux
    sh $SBCL_DIR/binary-distribution.sh sbcl-$VERSION-x86-64-linux
    bzip2 sbcl-$VERSION-x86-64-linux-binary.tar
    sh $SBCL_DIR/html-distribution.sh sbcl-$VERSION
    bzip2 sbcl-$VERSION-documentation-html.tar

    mv $SBCL_DIR/obj/from-xc obj_from-xc_sbcl
fi

# check build from ccl

if [ ! -d $SBCL_RELEASE_DIR/obj_from-xc_ccl ]; then
   cd $SBCL_DIR
   sh clean.sh
   nice -20 ./make.sh "$CCL" >> $LOGFILE 2>&1
   cd $SBCL_RELEASE_DIR

   mv $SBCL_DIR/obj/from-xc obj_from-xc_ccl
fi

# TODO: check binary-equality between ccl, sbcl objs

# TODO: check build from clisp, abcl

# upload rc build

if [ ! -f $SBCL_RELEASE_DIR/uploaded ]; then

  read -n 1 -p "Ok to upload? " A; echo
  if [ $A \!= "y" ]; then
    exit 1
  fi

  cd $SBCL_RELEASE_DIR
cat > $SBCL_RELEASE_DIR/sftp-batch <<EOF
cd /home/frs/project/s/sb/sbcl/sbcl-rc
put sbcl-$VERSION-x86-64-linux-binary.tar.bz2
put sbcl-$VERSION-documentation-html.tar.bz2
put sbcl-$VERSION-release-notes.txt
EOF
  sftp -b $SBCL_RELEASE_DIR/sftp-batch $SFUSER,sbcl@frs.sourceforge.net
  touch uploaded
fi

# TODO: check self-crossbuild on x86, arm, ppc


# TODO: find Fix Committed lp bugs for NEWS frobbery
