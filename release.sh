#! /bin/sh

set -ex

cd "$1"

sbcl_directory="$(pwd)"

cd "$sbcl_directory"

tmpfile=$(mktemp --tmpdir sbcl-build-$(date +%Y%m%d)-XXXXXXXXX)

./make.sh >$tmpfile 2>&1

./src/runtime/sbcl --version | grep '^SBCL [1-9][0-9]*\.[0-9]\+\.[1-9][0-9]*$'

version=$(./src/runtime/sbcl --version | awk '{print $2}')
grep "^changes in sbcl-$version relative to" NEWS

cd tests
sh ./run-tests.sh >>$tmpfile 2>&1
cd ..

cp ./src/runtime/sbcl /tmp/sbcl-$version
cp ./output/sbcl.core /tmp/sbcl-$version.core

./make.sh "/tmp/sbcl-$version --core /tmp/sbcl-$version.core" > /tmp/sbcl-$version-build-log 2>&1
cd doc && sh ./make-doc.sh

cd ..

rm /tmp/sbcl-$version /tmp/sbcl-$version.core

cp -a "$sbcl_directory" /tmp/sbcl-$version

ln -s /tmp/sbcl-$version /tmp/sbcl-$version-x86-linux
cd /tmp/
sh sbcl-$version/binary-distribution.sh sbcl-$version-x86-linux
sh sbcl-$version/html-distribution.sh sbcl-$version
cd sbcl-$version
sh ./distclean.sh
cd ..
sh sbcl-$version/source-distribution.sh sbcl-$version

awk "BEGIN { state = 0 }
 /^changes in sbcl-/ { state = 0 } 
 /^changes in sbcl-$version/ { state = 1 }
 { if(state == 1) print \$0 }" < sbcl-$version/NEWS > sbcl-$version-release-notes.txt

echo "The SHA256 checksums of the following distribution files are:" > sbcl-$version-crhodes
echo >> sbcl-$version-crhodes
sha256sum sbcl-$version*.tar >> sbcl-$version-crhodes
bzip2 /tmp/sbcl-$version*.tar

echo Bugs fixed by sbcl-$version release > sbcl-$version-bugmail.txt
for bugnum in $(egrep -o "#[1-9][0-9][0-9][0-9][0-9][0-9]+" sbcl-$version-release-notes.txt | sed s/#// | sort -n)
do 
  printf "\n bug %s\n status fixreleased" $bugnum >> sbcl-$version-bugmail.txt
done
echo >> sbcl-$version-bugmail.txt

set +x

echo TODO:
echo
echo cvs commit -m "\"$version: will be tagged as sbcl_$(echo $version | sed 's/\./_/g')\""
echo cvs tag sbcl_$(echo $version | sed 's/\./_/g')
echo gpg -sta /tmp/sbcl-$version-crhodes
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
