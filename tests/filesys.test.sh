#!/bin/sh

# This software is part of the SBCL system. See the README file for
# more information.
#
# While most of SBCL is derived from the CMU CL system, the test
# files (like this one) were written from scratch after the fork
# from CMU CL.
# 
# This software is in the public domain and is provided with
# absolutely no warranty. See the COPYING and CREDITS files for
# more information.

# Test DIRECTORY and TRUENAME.
testdir=`pwd`"/filesys-test-$$"
mkdir $testdir
echo this is a test > $testdir/test-1.tmp
echo this is a test > $testdir/test-2.tmp
cd $testdir
ln -s test-1.tmp link-1
ln -s `pwd`/test-2.tmp link-2
ln -s i-do-not-exist link-3
ln -s link-4 link-4
ln -s link-5 link-6
ln -s `pwd`/link-6 link-5
expected_truenames=\
"'(#p\"$testdir/link-3\"\
   #p\"$testdir/link-4\"\
   #p\"$testdir/link-5\"\
   #p\"$testdir/link-6\"\
   #p\"$testdir/test-1.tmp\"\
   #p\"$testdir/test-2.tmp\")"
$SBCL <<EOF
  (in-package :cl-user)
  (let* ((directory (directory "./*.*"))
         (truenames (sort directory #'string< :key #'pathname-name)))
    (format t "~&TRUENAMES=~S~%" truenames)
    (finish-output)
    (assert (equal truenames $expected_truenames)))
  (assert (equal (truename "test-1.tmp") #p"$testdir/test-1.tmp"))
  (assert (equal (truename "link-1")     #p"$testdir/test-1.tmp"))
  (assert (equal (truename "link-2")     #p"$testdir/test-2.tmp"))
  (assert (equal (truename "link-3")     #p"$testdir/link-3"))
  (assert (equal (truename "link-4")     #p"$testdir/link-4"))
  (assert (equal (truename "link-5")     #p"$testdir/link-5"))
  (assert (equal (truename "link-6")     #p"$testdir/link-6"))
  (sb-ext:quit :unix-status 52)
EOF
if [ $? != 52 ]; then
    echo DIRECTORY/TRUENAME test part 1 failed, unexpected SBCL return code=$?
    exit 1
fi
cd ..
$SBCL <<EOF
  (in-package :cl-user)
  (let* ((directory (directory "$testdir/*.*"))
         (truenames (sort directory #'string< :key #'pathname-name)))
    (format t "~&TRUENAMES=~S~%" truenames)
    (finish-output)
    (assert (equal truenames $expected_truenames)))
  (assert (equal (truename "$testdir/test-1.tmp") #p"$testdir/test-1.tmp"))
  (assert (equal (truename "$testdir/link-1")     #p"$testdir/test-1.tmp"))
  (assert (equal (truename "$testdir/link-2")     #p"$testdir/test-2.tmp"))
  (assert (equal (truename "$testdir/link-3")     #p"$testdir/link-3"))
  (assert (equal (truename "$testdir/link-4")     #p"$testdir/link-4"))
  (assert (equal (truename "$testdir/link-5")     #p"$testdir/link-5"))
  (assert (equal (truename "$testdir/link-6")     #p"$testdir/link-6"))
  (sb-ext:quit :unix-status 52)
EOF
if [ $? != 52 ]; then
    echo DIRECTORY/TRUENAME test part 2 failed, unexpected SBCL return code=$?
    exit 1
fi
rm -r $testdir

# success convention for script
exit 104
