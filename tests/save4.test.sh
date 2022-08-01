export TEST_BASEDIR=${TMPDIR:-/tmp}
. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

# test for lp#1983248 - gc_close_region on an unopen region
run_sbcl <<EOF
(defparameter *keepme* (make-array 15500 :fill-pointer 0))
(let ((s (find-symbol "*COMPILE-TO-MEMORY-SPACE*" "SB-C")))
  (when s
    (set s :immobile)
    (dotimes (i (array-dimension *keepme* 0))
      (vector-push-extend (compile nil \`(lambda () ,i)) *keepme*))))
(save-lisp-and-die "$tmpcore")
EOF

check_status_maybe_lose "LOTS-OF-CODE" $? 0 "(saved OK)"

exit $EXIT_TEST_WIN
