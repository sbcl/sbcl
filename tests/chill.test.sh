. ./subr.sh

set -e
# The function SB-C:LOCATION-NUMBER does not get defined for all backends,
# and may get dropped by the tree shaker.
run_sbcl --load ../src/cold/chill.lisp \
  --eval '(assert (eq :external (nth-value 1 (find-symbol "LOCATION-NUMBER" "SB-C"))))' \
  --quit

exit $EXIT_TEST_WIN
