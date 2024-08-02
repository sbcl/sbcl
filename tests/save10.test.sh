. ./subr.sh

this_file=`pwd`/save10.test.sh
use_test_subdirectory

tmpcore=$TEST_FILESTEM.core
set -e

# This test asserts that preparing read-only space prior to saving a core file
# does not attempt to rewrite pointers that appear to point to unknown memory.
# So the test attempts to fabricate such pointers. However it's a bad idea
# to store into the car of a cons the value
#  (sb-kernel:%make-lisp-obj sb-vm:instance-pointer-lowtag)
# as that cons is indistinguishable from a forwarded cons in 32-bit builds,
# where INSTANCE_POINTER_LOWTAG is 1 and forwarding_pointer_p(x) is '*x == 1'
run_sbcl <<EOF
  (defvar *x* (list (sb-kernel:%make-lisp-obj sb-vm:fun-pointer-lowtag)
                    (sb-kernel:%make-lisp-obj
                     (logior #x100 sb-vm:instance-pointer-lowtag))))
  (save-lisp-and-die "$tmpcore")
EOF

exit $EXIT_TEST_WIN
