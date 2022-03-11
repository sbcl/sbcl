# Don't try to run sbcl from /tmp on openbsd as it's unlikely to be
# mounted with wxallowed
if [ "$SBCL_SOFTWARE_TYPE" != OpenBSD ]; then
    export TEST_BASEDIR=${TMPDIR:-/tmp}
fi
. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

# Regression test for https://bugs.launchpad.net/sbcl/+bug/1857920
# saving a finalizer would crash you into ldb.
# (Sadly, saved finalizers mean nothing to gencgc since it will never
# actually free the object which the finalizer watches)

run_sbcl <<EOF
  (defvar *x* (cons 1 2))
  (finalize *x* (lambda () (print 'ran)))
  (save-lisp-and-die "$tmpcore" :executable t)
EOF
set -e
./"$tmpcore" --disable-ldb --no-userinit --no-sysinit
rm "$tmpcore"
exit $EXIT_TEST_WIN
