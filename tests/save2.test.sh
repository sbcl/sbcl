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

run_sbcl --eval '(save-lisp-and-die "'$tmpcore'" :toplevel (lambda () (format t "Ahoy-hoy.~%")))'
result1=`run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit`
result2=`run_sbcl_with_core "$tmpcore" --merge-core-pages --noinform --no-userinit --no-sysinit`
# Both invocations should produce the "Ahoy-hoy." but result2 didn't
# because of busted arg parsing in git rev f0a7f17516
# result2 has to be string-quoted in case it contains junk (due to not parsing
# --noinform after incorrectly parsing --merge-core-pages)
if [ ${result1} != "Ahoy-hoy." -o "${result2}" != ${result1} ]
then
  echo FAIL: merge-core-pages
  exit 0
fi
# Passing doesn't actually mean we utilized madvise,
# just that we didn't choke on the argument.
echo PASS: merge-core-pages

exit $EXIT_TEST_WIN
