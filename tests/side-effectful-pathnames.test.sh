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
    (declare (ignore ignore-me))
    (format t "translation=~S~%" translation)
    (format t "expected-translation=~S~%" expected-translation)
    (assert (string= translation expected-translation)))
  (format t "about to LOAD ~S~%" "TEST:$StudlyCapsStem")
  (load "TEST:$StudlyCapsStem")
  (assert (eq *loaded* :yes))
  ;; COMPILE-FILE returns truenames unless SB-C::*MERGE-PATHNAMES* is set it NIL
  ;; so that you don't see pathnames returned that are immediately useless
  ;; on another machine, in which case we should just explicitly call TRUENAME
  ;; to get the expected result for this test, which is of course meaningless,
  ;; because my 'pwd' is here:
  ;;  /google/src/cloud/..somewhere...EA0643B31763702B4088D15FEA8C1700_1/google3/third_party/lisp/sbcl
  ;; but it "wants" to see:
  ;;  /build/work/bb4efcf5cc6f5221b1a246e4fe09dd16eed5/google3/tmp/side-effectful-pathnames-test-4187/load-test.fasl"
  ;; even though a different execution's might as well be here (or anywhere):
  ;;  /build/work/698d0e844a200a4c30cf75e6233b55b215ea/google3/runfiles/google3/third_party/lisp/sbcl/src/tests
  ;; So is it better to have a kludged passing test, or a failing test?
  ;; And is it better to change the test's expectation, or "force" a valid result?
  ;; I don't know the answer to those questions, and I don't care.
  (let ((compiled-file-name (namestring (truename (compile-file "TEST:$StudlyCapsStem"))))
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
   (sb-ext:exit :code 52)))
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
