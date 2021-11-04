export TEST_BASEDIR=${TMPDIR:-/tmp}
. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

# In sbcl-0.7.7 SAVE-LISP-AND-DIE didn't work at all because of
# flakiness caused by consing/GC/purify twice-and-at-least-twice
# mismatch grot.
#
# "serves yall right for fiddling with too much stuff"
#   -- Eric Marsden, <http://tunes.org/~nef/logs/lisp/02.09.15>
#
# diagnosed and fixed by Dan Barlow in sbcl-0.7.7.29
run_sbcl <<EOF
  (defun foo (x) (+ x 11))
  (setq *features* (union *features* sb-impl:+internal-features+))
  ;; The basic smoke test includes a test that immobile-space defragmentation
  ;; supports calls to "static" functions - those called without reference
  ;; to an fdefn, from a caller in dynamic space.
  ;; dynamic space should be the default for compilation to memory,
  ;; but maybe someone changed it :immobile, so bind it to be certain.
  (let (#+immobile-code (sb-c::*compile-to-memory-space* :dynamic))
     (defvar *afun* (compile nil '(lambda (x) (- (length x))))))
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit --noprint \
    --eval "(setf sb-ext:*evaluator-mode* :${TEST_SBCL_EVALUATOR_MODE:-compile})" \
    <<EOF
  (exit :code (foo 10))
EOF
check_status_maybe_lose "Basic SAVE-LISP-AND-DIE" $? 21 "(saved core ran)"

exit $EXIT_TEST_WIN
