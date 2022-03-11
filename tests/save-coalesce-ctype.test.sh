export TEST_BASEDIR=${TMPDIR:-/tmp}
. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

run_sbcl <<EOF
  (defvar *x* (cons (sb-kernel::make-unknown-type :specifier 'hash-table) (sb-kernel:specifier-type 'hash-table)))
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit --noprint \
    <<EOF
  (exit :code (if (eq (car *x*) (cdr *x*)) 1 0))
EOF
check_status_maybe_lose "coalesce-ctypes SAVE-LISP-AND-DIE" $? 0 "(saved core ran)"

exit $EXIT_TEST_WIN
