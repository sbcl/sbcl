. ./subr.sh

this_file=`pwd`/save10.test.sh
use_test_subdirectory

tmpcore=$TEST_FILESTEM.core
set -e

run_sbcl <<EOF
  (defvar *x* (list (sb-kernel:%make-lisp-obj sb-vm:fun-pointer-lowtag)
                    (sb-kernel:%make-lisp-obj sb-vm:instance-pointer-lowtag)))
  (save-lisp-and-die "$tmpcore")
EOF

exit $EXIT_TEST_WIN
