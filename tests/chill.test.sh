. ./subr.sh

run_sbcl <<EOF
#+sparc (exit :code 2) ;skip
(exit :code 0)
EOF
status=$?
if [ $status != 0 ]; then # test can't be executed
    # we don't have a way to exit shell tests with "inapplicable" as the result
    exit $EXIT_TEST_WIN
fi

set -e
# The function SB-C:LOCATION-NUMBER does not get defined for all backends,
# and may get dropped by the tree shaker.
run_sbcl --load ../src/cold/chill.lisp \
  --eval '(assert (eq :external (nth-value 1 (find-symbol "LOCATION-NUMBER" "SB-C"))))' \
  --quit

exit $EXIT_TEST_WIN
