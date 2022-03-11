#!/bin/bash

# Sourceforge username
SFUSER=${SFUSER:-$USER}

set -ex

## Parse command line arguments

usage () {
    if ! [ -z "$1" ]
    then
        echo $1
    fi
    cat <<EOF

usage: $0 [-s] VERSION-NUMBER [REV]

  This script frobs NEWS, makes a "release" commit + tag of the
  repository in \$PWD/release.sbcl .

  It then clones the repository to a new clean directory and does the
  following:
  - Builds source tarball
  - Builds a SBCL for self-build
  - Builds x86-64 binaries
  - Uploads binaries
  - Pushes the repository
  - Builds and uploads the documentation

  If -s is given, then use the gpg-sign mechanism of "git tag". You
  will need to have your gpg secret key handy.

  if REV is given, it is the git revision to build into a release.
  Default is origin/master.

  ENVIRONMENT:

  SFUSER: Sourceforge username. Defaults to \$USER
  SBCL_RELEASE_DIR: Absolute path to directory containing all the build
    artifacts. If not defined, a new build directory is created in \$PWD.

EOF
    exit 1
}

if [ "-s" = "$1" ] || [ "--sign" = "$1" ]
then
    sign="-s"; shift
else
    sign=""
fi

## Verify version number

if [ -z "$1" ]
then
    usage "No version number."
else
    VERSION=$1; shift
    echo $VERSION | perl -pe 'die "Invalid version number: $_\n" if !/^\d+\.\d+\.\d+$/'
fi

if [ -z "$1" ]
then
    rev=origin/master
else
    rev=$1; shift
    type=$(git cat-file -t "$rev" 2> /dev/null || echo "unknown")
    if ([ "tag" != "$type" ] && [ "commit" != "$type" ])
    then
        usage "$rev is $type, not a tag or a commit."
    fi
fi

if ! [ -z "$@" ]
then
    usage "Extra command-line arguments: $@"
fi

SBCL_RELEASE_DIR=${SBCL_RELEASE_DIR:-$(mktemp -d $PWD/sbcl-release-dir-$(date +%Y%m%d)-XXXXXXXXX)}
SBCL_DIR=$SBCL_RELEASE_DIR/sbcl-$VERSION
GIT_DIR=$PWD/release.sbcl
LOGFILE=$SBCL_RELEASE_DIR/log.txt

## Frob the git repository, and clone the repo to a clean build directory.

if [ ! -d $SBCL_DIR ]; then
  cd $GIT_DIR

  sbcl_directory="$(cd "$(dirname $0)"; pwd)"

  branch_name="release-$(date '+%s')"
  original_branch="$(git describe --all --contains HEAD)"

  echo "Checking that the tree is clean."
  if ! [ $(git status --porcelain | wc -l) = 0 ]
  then
      echo "There are uncommitted / unpushed changes in this checkout!"
      git status
      exit 1
  fi

  ## Perform the necessary changes to the NEWS file:

  echo "Munging NEWS"
  sed -i.orig "/^changes relative to sbcl-.*:/ s/changes/changes in sbcl-$VERSION/ " NEWS
  rm -f NEWS.orig
  if ! grep "^changes in sbcl-$VERSION relative to" NEWS > /dev/null
  then
      echo "NEWS munging failed!"
      exit 1
  fi

  ## Commit

  cd "$sbcl_directory"

  echo "Committing release version."
  git add NEWS
  git commit -m "$VERSION: will be tagged as \"sbcl-$VERSION\""

  ## Make release notes

  if [ ! -f $SBCL_RELEASE_DIR/sbcl-$VERSION-release-notes.txt ]; then
    awk "BEGIN { state = 0 }
     /^changes in sbcl-/ { state = 0 } 
     /^changes in sbcl-$VERSION/ { state = 1 }
     { if(state == 1) print \$0 }" < $GIT_DIR/NEWS > $SBCL_RELEASE_DIR/sbcl-$VERSION-release-notes.txt
  fi

  ## Tag

  tag="sbcl-$VERSION"
  echo "Tagging as $tag"
  git tag $sign -F $SBCL_RELEASE_DIR/sbcl-$VERSION-release-notes.txt "$tag"

  git clone $GIT_DIR $SBCL_DIR
fi

## Make the source tarball.

if [ ! \( -f $SBCL_RELEASE_DIR/sbcl-$VERSION-source.tar -o -f $SBCL_RELEASE_DIR/sbcl-$VERSION-source.tar.bz2 \) ]; then
  cd $SBCL_DIR
  $SBCL_DIR/generate-version.sh
  mkdir -p CVS
  sh ./distclean.sh
  rm -rf .git

  cd $SBCL_RELEASE_DIR
  sh sbcl-$VERSION/source-distribution.sh sbcl-$VERSION

  mv $SBCL_DIR $SBCL_DIR.git

  tar xvf sbcl-$VERSION-source.tar
fi

## Build x86-64 binary for bootstrap.

if [ ! -d $SBCL_RELEASE_DIR/bin ]; then
  cd $SBCL_DIR
  nice -20 ./make.sh >$LOGFILE 2>&1

  cd tests
  nice -20 sh ./run-tests.sh >>$LOGFILE 2>&1
  mkdir -p $SBCL_RELEASE_DIR/bin
  cp $SBCL_DIR/src/runtime/sbcl $SBCL_RELEASE_DIR/bin/sbcl
  cp $SBCL_DIR/output/sbcl.core $SBCL_RELEASE_DIR/bin/sbcl.core
fi

## Build x86-64 release binary.

if [ ! -d $SBCL_RELEASE_DIR/sbcl-$VERSION-x86-64-linux ]; then
  cd $SBCL_DIR
  sh clean.sh
  nice -20 ./make.sh "$SBCL_RELEASE_DIR/bin/sbcl --core $SBCL_RELEASE_DIR/bin/sbcl.core --no-userinit" >> $LOGFILE 2>&1
  cd doc && sh ./make-doc.sh
  cd $SBCL_RELEASE_DIR

  ln -s $SBCL_DIR $SBCL_RELEASE_DIR/sbcl-$VERSION-x86-64-linux
  sh $SBCL_DIR/binary-distribution.sh sbcl-$VERSION-x86-64-linux
  sh $SBCL_DIR/html-distribution.sh sbcl-$VERSION
fi

## Build x86 release binary.

#if [ ! -d $SBCL_RELEASE_DIR/sbcl-$VERSION-x86-linux ]; then
#  cd $SBCL_DIR
#  sh clean.sh
#  export SBCL_ARCH=x86
#  export PATH=/scratch/src/release/x86-gcc-wrapper:$PATH
#  nice -20 ./make.sh "$SBCL_RELEASE_DIR/bin/sbcl --core $SBCL_RELEASE_DIR/bin/s#bcl.core --no-userinit" >> $LOGFILE 2>&1
#  cd tests
#  nice -20 sh ./run-tests.sh >>$LOGFILE 2>&

#  cd $SBCL_RELEASE_DIR
#  ln -s $SBCL_DIR $SBCL_RELEASE_DIR/sbcl-$VERSION-x86-linux
#  sh $SBCL_DIR/binary-distribution.sh sbcl-$VERSION-x86-linux
#fi

## Checksum

if [ ! -f $SBCL_RELEASE_DIR/sbcl-$VERSION-$SFUSER ]; then
  cd $SBCL_RELEASE_DIR
  echo "The SHA256 checksums of the following distribution files are:" > sbcl-$VERSION-$SFUSER
  echo >> sbcl-$VERSION-$SFUSER
  sha256sum sbcl-$VERSION*.tar >> sbcl-$VERSION-$SFUSER
  bzip2 sbcl-$VERSION*.tar
fi

## Bug closing email

if [ ! -f $SBCL_RELEASE_DIR/sbcl-$VERSION-bugmail.txt ]; then
  cd $SBCL_RELEASE_DIR
  echo Bugs fixed by sbcl-$VERSION release > sbcl-$VERSION-bugmail.txt
 for bugnum in $(egrep -o "#[1-9][0-9][0-9][0-9][0-9][0-9]+" sbcl-$VERSION-release-notes.txt | sed s/#// | sort -n); do 
    printf "\n bug %s\n status fixreleased" $bugnum >> sbcl-$VERSION-bugmail.txt
  done
  echo >> sbcl-$VERSION-bugmail.txt
fi

## Sign

if [ ! -f $SBCL_RELEASE_DIR/sbcl-$VERSION-$SFUSER.asc ]; then
  cd $SBCL_RELEASE_DIR
  gpg -sta $SBCL_RELEASE_DIR/sbcl-$VERSION-$SFUSER
fi

## Upload to sf.net

if [ ! -f $SBCL_RELEASE_DIR/uploaded ]; then

  read -n 1 -p "Ok to upload? " A; echo  
  if [ $A \!= "y" ]; then
    exit 1
  fi

  cd $SBCL_RELEASE_DIR
cat > $SBCL_RELEASE_DIR/sftp-batch <<EOF
cd /home/frs/project/s/sb/sbcl/sbcl
mkdir $VERSION
chmod 775 $VERSION
cd $VERSION
put sbcl-$VERSION-$SFUSER.asc
put sbcl-$VERSION-x86-64-linux-binary.tar.bz2
put sbcl-$VERSION-source.tar.bz2
put sbcl-$VERSION-documentation-html.tar.bz2
put sbcl-$VERSION-release-notes.txt
put sbcl-$VERSION-release-notes.txt README
EOF
  sftp -b $SBCL_RELEASE_DIR/sftp-batch $SFUSER,sbcl@frs.sourceforge.net 
  touch uploaded
fi

## Push

if [ ! -f $SBCL_RELEASE_DIR/sbcl-git-pushed ]; then
  cd $GIT_DIR
  git diff origin || true
  
  read -n 1 -p "Ok? " A; echo  

  if [ $A = "y" ]; then
    git push
    git push --tags
    touch $SBCL_RELEASE_DIR/sbcl-git-pushed
  else
    exit 1
  fi
fi

## Build + push documentation

if [ ! -f $SBCL_RELEASE_DIR/sbcl-page-uploaded ]; then
  cp -af $GIT_DIR/../sbcl-page $SBCL_RELEASE_DIR
  cd $SBCL_RELEASE_DIR/sbcl-page/sbcl
  git fetch
  cd $SBCL_RELEASE_DIR/sbcl-page
  git pull
  git submodule update
  cp $SBCL_DIR/NEWS .
  perl -i -pe "s/(:x86-64 :linux :available) \".*\"/\$1 \"$VERSION\"/" \
    platform-support-platforms.lisp

  export LC_CTYPE=en_GB.utf8

  ## FIXME: this depends on the sbcl-$VERSION tag being visible on sf
  ## git, not just the local machine (because of the submodule), which
  ## means that the release.sbcl should have as its origin sf not
  ## another local copy.
  nice -20 make manual generate-pages SBCL_TAG=sbcl-$VERSION >>$LOGFILE 2>&1

  COMMIT=false

  git diff || COMMIT=true  
  links -dump platform-table.html

  read -n 1 -p "Ok? " A; echo  

  if [ $A = "y" ]; then
    if [ $COMMIT ]; then
      git commit -a -m "Update for $VERSION"
      git push
    fi
    make upload-pages upload-manual SBCL_TAG=sbcl-$VERSION
    touch $SBCL_RELEASE_DIR/sbcl-page-uploaded
  else
    exit 1
  fi
fi

set +x

echo TODO:
echo 
echo perform administrative tasks:
echo 
echo \* visit https://sourceforge.net/projects/sbcl/files/
echo \* select sbcl -> $VERSION -> "view details" for the source
echo \ \ tarball, "Select all" and Save
echo \* mail sbcl-announce
echo \* check and send sbcl-$VERSION-bugmail.txt to edit@bugs.launchpad.net
echo \ \ '(sign: C-c RET s p)'
