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

. ./subr.sh

use_test_subdirectory
testdir="`pwd -P`" # resolve symbolic links in the directory.

# LOADing and COMPILEing files with logical pathnames
testfilestem="load-test"
StudlyCapsStem="Load-Test"
testfilename="$testfilestem.lisp"
cat >$testfilename <<EOF
  (in-package :cl-user)
  (defparameter *loaded* :yes)
EOF
run_sbcl <<EOF
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
  (sb-ext:quit :unix-status $EXIT_LISP_WIN)
EOF
check_status_maybe_lose "LOAD/COMPILE" $?

# In the flaky1 branch, Dan Barlow pointed out that
# ENSURE-DIRECTORIES-EXIST failed for these relative pathname
# operations when the mysterious special case handling of "" pathnames
# was removed from UNIX-STAT. Let's make sure that it works now.
#
# Set up an empty directory to work with.
testdir="${TMPDIR:-/tmp}/sbcl-mkdir-test-$$"
if ! rm -rf "$testdir" ; then
  echo "$testdir already exists and could not be deleted"
  exit 1;
fi
mkdir "$testdir"
cd "$testdir"
#
# Provoke failure.
run_sbcl <<EOF
(let ((rel-name #p"foo/bar/")
      (abs-name (merge-pathnames #p"baz/quux/" (truename "."))))
  (and
   (equalp (ensure-directories-exist abs-name) abs-name)
   (equalp (ensure-directories-exist rel-name) rel-name)
   (sb-ext:quit :unix-status 52)))
EOF
check_status_maybe_lose "ENSURE-DIRECTORIES-EXIST" $?
if [ ! -d "$testdir/foo/bar" ] ; then
    echo test failed: "$testdir/foo/bar" is not a directory
    find "$testdir" -print
    exit 1
fi;
if [ ! -d "$testdir/baz/quux" ] ; then
    echo test failed: "$testdir/baz/quux" is not a directory
    find "$testdir" -print
    exit 1
fi;
#
# We succeeded, life is good. Now we don't need the test directory
# any more; and come back home.
cd "$SBCL_PWD"
rm -r "$testdir"

exit $EXIT_TEST_WIN
