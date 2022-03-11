export TEST_BASEDIR=${TMPDIR:-/tmp}
. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

# In sbcl-0.9.8 saving cores with callbacks didn't work on gencgc platforms
run_sbcl <<EOF
  (defun bar ()
    (format t "~&Callbacks not supported, skipping~%")
    (exit :code 42))
  (when (member :alien-callbacks sb-impl:+internal-features+)
    (fmakunbound 'bar)
    (sb-alien:define-alien-callable foo int () 42)
    (defun bar () (exit :code (alien-funcall (sb-alien:alien-callable-function 'foo)))))
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit --noprint \
    --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})" \
    <<EOF
  (bar)
EOF
check_status_maybe_lose "Callbacks after SAVE-LISP-AND-DIE" $? \
    42 "(callback function ran)"

exit $EXIT_TEST_WIN
