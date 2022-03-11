. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core
tmpoutput=$TEST_FILESTEM.txt

# test suppression of banner in executable cores
run_sbcl <<EOF
  (save-lisp-and-die "$tmpcore" :executable t)
EOF
chmod u+x "$tmpcore"
# FIXME: Shouldn't this test use "run_sbcl_with_core" ?
./"$tmpcore" > "$tmpoutput" --no-userinit --no-sysinit --noprint <<EOF
  (exit :code 71)
EOF
status=$?
if [ $status != 71 ]; then
  echo "failure in banner suppression: $status"
  exit 1
elif [ -s "$tmpoutput" ]; then
  echo "failure in banner suppression: nonempty output:"
  echo ---
  cat "$tmpoutput"
  echo ---
  exit 1
elif [ -f "$tmpoutput" ]; then
  echo "/Executable suppressed banner, good."
else
  echo "failure in banner suppression: $tmpoutput was not created or something funny happened."
  exit 1
fi

exit $EXIT_TEST_WIN
