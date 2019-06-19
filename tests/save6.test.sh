. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

# Regression test for https://bugs.launchpad.net/sbcl/+bug/411925
# saving runtime options _from_ executable cores
run_sbcl <<EOF
  (save-lisp-and-die "$tmpcore" :executable t)
EOF
chmod u+x "$tmpcore"
./"$tmpcore" --no-userinit --no-sysinit --noprint <<EOF
  (save-lisp-and-die "$tmpcore" :executable t :save-runtime-options t)
EOF
chmod u+x "$tmpcore"
./"$tmpcore" --no-userinit --no-sysinit --noprint --version --eval '(exit)' <<EOF
  (when (equal *posix-argv* '("./$tmpcore" "--version" "--eval" "(exit)"))
    (exit :code 42))
EOF
status=$?
rm "$tmpcore"
if [ $status != 42 ]; then
    echo "saving runtime options from executable failed"
    exit 1
fi

exit $EXIT_TEST_WIN
