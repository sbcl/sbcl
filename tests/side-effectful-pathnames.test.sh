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

original_pwd=`pwd`

# LOADing and COMPILEing files with logical pathnames
testdir=`pwd`"/side-effectful-pathnames-test-$$"
testfilestem="load-test"
StudlyCapsStem="Load-Test"
testfilename="$testdir/$testfilestem.lisp"
mkdir $testdir
cat >$testfilename <<EOF
  (in-package :cl-user)
  (defparameter *loaded* :yes)
EOF
$SBCL <<EOF
  (in-package :cl-user)
  (setf (logical-pathname-translations "TEST")
        (list (list "**;*.*.*" "$testdir/**/*.*")))
  (format t "/translations=~S~%" (logical-pathname-translations "TEST"))
  (let* ((untranslated "test:$StudlyCapsStem.lisp")
         (ignore-me (format t "untranslated=~S~%" untranslated))
         (translation (namestring (translate-logical-pathname untranslated)))
         (expected-translation "$testdir/$testfilestem.lisp"))
    (format t "translation=~S~%" translation)
    (format t "expected-translation=~S~%" expected-translation)
    (assert (string= translation expected-translation)))
  (format t "about to LOAD ~S~%" "TEST:$StudlyCapsStem")
  (load "TEST:$StudlyCapsStem")
  (assert (eq *loaded* :yes))
  (let ((compiled-file-name (namestring (compile-file "TEST:$StudlyCapsStem")))
        (expected-file-name "$testdir/$testfilestem.fasl"))
    (format t "compiled-file-name=~S~%" compiled-file-name)
    (format t "expected-file-name=~S~%" expected-file-name)
    (assert (string= compiled-file-name expected-file-name)))
  (sb-ext:quit :unix-status 52)
EOF
if [ $? != 52 ]; then
    echo LOAD/COMPILE test failed, unexpected Lisp return code=$?
    exit 1
fi
# We don't need the test directory any more.
rm -r $testdir

# In the flaky1 branch, Dan Barlow pointed out that
# ENSURE-DIRECTORIES-EXIST failed for these relative pathname
# operations when the mysterious special case handling of "" pathnames
# was removed from UNIX-STAT. Let's make sure that it works now.
#
# Set up an empty directory to work with.
testdir=${TMPDIR:-/tmp}/sbcl-mkdir-test-$$
if ! rm -rf $testdir ; then
  echo "$testdir already exists and could not be deleted"
  exit 1;
fi
mkdir $testdir
cd $testdir
#
# Provoke failure.
$SBCL <<EOF
(let ((rel-name #p"foo/bar/")
      (abs-name (merge-pathnames #p"baz/quux/" (truename "."))))
  (and
   (ensure-directories-exist abs-name)
   (ensure-directories-exist rel-name)
   (sb-ext:quit :unix-status 52)))
EOF
if [ $? != 52 ]; then
    echo ENSURE-DIRECTORIES-EXIST test failed, unexpected SBCL return code=$?
    find $testdir -print
    exit 1
fi
if [ ! -d $testdir/foo/bar ] ; then
    echo test failed: $testdir/foo/bar is not a directory
    find $testdir -print
    exit 1
fi;
if [ ! -d $testdir/baz/quux ] ; then
    echo test failed: $testdir/baz/quux is not a directory
    find $testdir -print
    exit 1
fi;
#
# We succeeded, life is good. Now we don't need the test directory
# any more; and come back home.
rm -r $testdir
cd $original_pwd

# success convention for script
exit 104
