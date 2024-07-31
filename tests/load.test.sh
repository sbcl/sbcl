#!/bin/sh
. ./subr.sh

use_test_subdirectory

set -e
mkdir subdir1
echo '(defvar *hi* "hi")' > subdir1/file1.lisp

run_sbcl <<EOF
  (setq *default-pathname-defaults*
    (merge-pathnames (make-pathname :directory '(:relative "subdir1"))))
  (load "file1") ; should merge the dir and default the .lisp type
  (assert (string= *hi* "hi"))
EOF
exit $EXIT_TEST_WIN
