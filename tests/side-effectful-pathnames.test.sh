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
  (load "TEST:$StudlyCapsStem")
  (assert (eq *loaded* :yes))
  (let ((compiled-file-name (namestring (compile-file "TEST:$StudlyCapsStem")))
        (expected-file-name "$testdir/$testfilestem.x86f"))
    (format t "compiled-file-name=~S~%" compiled-file-name)
    (format t "expected-file-name=~S~%" expected-file-name)
    (assert (string= compiled-file-name expected-file-name)))
  (sb-ext:quit :unix-status 52)
EOF
if [ $? != 52 ]; then
    echo test failed: $?
    exit 1
fi
rm -r $testdir

# success
exit 104
