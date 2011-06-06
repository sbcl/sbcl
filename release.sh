#! /bin/sh

set -e

usage () {
    if ! [ -z "$1" ]
    then
        echo $1
    fi
    cat <<EOF

usage: $0 [-s] VERSION-NUMBER [REV]

  This script frobs NEWS, makes a "release" commit, builds, runs tests
  and creates an annotated tag for the release with VERSION-NUMBER.

  If -s is given, then use the gpg-sign mechanism of "git tag". You
  will need to have your gpg secret key handy.

  if REV is given, it is the git revision to build into a release.
  Default is origin/master.

  No changes will be pushed upstream. This script will tell you how to
  do this when it finishes.

EOF
    exit 1
}

if [ "-s" = "$1" ] || [ "--sign" = "$1" ]
then
    sign="-s"; shift
else
    sign=""
fi

if [ -z "$1" ]
then
    usage "No version number."
else
    version=$1; shift
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

sbcl_directory="$(cd "$(dirname $0)"; pwd)"
tmpfile=$(mktemp -t sbcl-build-$(date +%Y%m%d)-XXXXXXXXX)
tmpdir="$(mktemp -d -t sbcl-build-tree-$(date +%Y%m%d)-XXXXXXXXX)"

## Check for messy work dirs:

echo "Fetching updates."
git fetch

branch_name="release-$(date '+%s')"
original_branch="$(git describe --all --contains HEAD)"
trap "cd \"$sbcl_directory\" ; git checkout $original_branch" EXIT
git checkout -b $branch_name $rev

echo "Checking that the tree is clean."
if ! [ $(git status --porcelain | wc -l) = 0 ]
then
    echo "There are uncommitted / unpushed changes in this checkout!"
    git status
    exit 1
fi

## Perform the necessary changes to the NEWS file:

echo "Munging NEWS"
sed -i.orig "/^changes relative to sbcl-.*:/ s/changes/changes in sbcl-$version/ " NEWS
rm -f NEWS.orig
if ! grep "^changes in sbcl-$version relative to" NEWS > /dev/null
then
    echo "NEWS munging failed!"
    exit 1
fi

cd "$sbcl_directory"

echo "Committing release version."
git add NEWS
git commit -m "$version: will be tagged as \"sbcl-$version\""

relnotes=$tmpdir/sbcl-$version-release-notes.txt
awk "BEGIN { state = 0 }
 /^changes in sbcl-/ { state = 0 }
 /^changes in sbcl-$version/ { state = 1 }
 { if(state == 1) print \$0 }" < NEWS > $relnotes

tag="sbcl-$version"
echo "Tagging as $tag"
git tag $sign -F $relnotes "$tag"

SBCL_BUILDING_RELEASE_FROM=HEAD
export SBCL_BUILDING_RELEASE_FROM
echo "Building SBCL, log: $tmpfile"
./make.sh >$tmpfile 2>&1

if [ "SBCL $version" != "$(./src/runtime/sbcl --version)" ]
then
    echo "Built version number doesn't match requested one:" &>2
    echo &>2
    echo "    $(./src/runtime/sbcl --version)" &>2
    exit 1
fi

built_version=$(./src/runtime/sbcl --version | awk '{print $2}')

echo "Running tests, log: $tmpfile"
cd tests
sh ./run-tests.sh >>$tmpfile 2>&1
cd ..

cp ./src/runtime/sbcl "$tmpdir"/sbcl-$version-bin
cp ./output/sbcl.core "$tmpdir"/sbcl-$version.core

echo "Self-building, log: $tmpfile"
./make.sh --xc-host="$tmpdir/sbcl-$version-bin --core $tmpdir/sbcl-$version.core --no-userinit --no-sysinit --disable-debugger" >>$tmpfile 2>&1

echo "Building docs, log: $tmpfile"
cd doc && sh ./make-doc.sh >$tmpfile 2>&1

cd ..

rm -f "$tmpdir"/sbcl-$version-bin "$tmpdir"/sbcl-$version.core

cp -a "$sbcl_directory" "$tmpdir"/sbcl-$version

echo "Building tarballs, log $tmpfile"
ln -s "$tmpdir"/sbcl-$version "$tmpdir"/sbcl-$version-x86-linux
cd "$tmpdir"/
sh sbcl-$version/binary-distribution.sh sbcl-$version-x86-linux >$tmpfile 2>&1
sh sbcl-$version/html-distribution.sh sbcl-$version >$tmpfile 2>&1
cd sbcl-$version
sh ./distclean.sh >$tmpfile 2>&1
cd ..
sh sbcl-$version/source-distribution.sh sbcl-$version >$tmpfile 2>&1

echo "The SHA256 checksums of the following distribution files are:" > sbcl-$version-crhodes
echo >> sbcl-$version-crhodes
sha256sum sbcl-$version*.tar >> sbcl-$version-crhodes
bzip2 "$tmpdir"/sbcl-$version*.tar

echo "Building bugmail."
bugmail=sbcl-$version-bugmail.txt
echo Bugs fixed by sbcl-$version release > $bugmail
for bugnum in $(egrep -o "#[1-9][0-9][0-9][0-9][0-9][0-9]+" $relnotes | sed s/#// | sort -n)
do 
  printf "\n bug %s\n status fixreleased" $bugnum >> $bugmail
done
echo >> $bugmail

echo SBCL distribution has been prepared in "$tmpdir"
echo TODO:
echo
echo "Sanity check: git show $tag"
echo
echo "git merge $branch_name && git push && git push --tags"
echo "git branch -d $branch_name"
echo "cd \"$tmpdir\""
echo gpg -sta sbcl-$version-crhodes
echo sftp crhodes,sbcl@frs.sourceforge.net
echo \* cd /home/frs/project/s/sb/sbcl/sbcl
echo \* mkdir $version
echo \* chmod 775 $version
echo \* cd $version
echo \* put sbcl-$version-crhodes.asc
echo \* put sbcl-$version-x86-linux-binary.tar.bz2
echo \* put sbcl-$version-source.tar.bz2
echo \* put sbcl-$version-documentation-html.tar.bz2
echo \* put sbcl-$version-release-notes.txt
echo 
echo perform administrative tasks:
echo 
echo \* https://sourceforge.net/project/admin/?group_id=1373
echo \* In the File Manager interface, click on the release notes file
echo \ \ and tick the release notes box.
echo \* In the File Manager interface, click on the source tarball and
echo \ \ select as default download for all OSes.
echo \* mail sbcl-announce
echo \* check and send sbcl-$version-bugmail.txt to edit@bugs.launchpad.net
echo \ \ '(sign: C-c RET s p)'
echo \* update \#lisp IRC topic
echo \* update sbcl website

