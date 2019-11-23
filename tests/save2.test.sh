export TEST_BASEDIR=${TMPDIR:-/tmp}
. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

# This test asserts that the :TOPLEVEL function is allowed to return with
# an arbitrary value that does _not_ propagate into the exit status.
run_sbcl <<EOF
  (save-lisp-and-die "$tmpcore" :toplevel (lambda () 42))
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit \
    --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})"
check_status_maybe_lose "SAVE-LISP-AND-DIE :TOPLEVEL" $? 0 "(saved core ran)"

exit $EXIT_TEST_WIN
