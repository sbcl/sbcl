export TEST_BASEDIR=${TMPDIR:-/tmp}
. ./subr.sh

use_test_subdirectory

tmpcore=$TEST_FILESTEM.core

# This test has to do with incorrect GC of funcallable instances
run_sbcl <<EOF
  (require :sb-bsd-sockets)
  (save-lisp-and-die "$tmpcore")
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit \
    --eval "(require :sb-posix)" --quit
check_status_maybe_lose "SAVE-LISP-AND-DIE" $? 0 "(saved core ran)"

# Verify that for funcallable instances which were moved into the
# immobile varyobj space by SAVE-LISP-AND-DIE, setting the layout
# updates the GC card touched bit.
# Going through instance-obsolescence stuff makes things mostly work
# by accident, because (SETF %FUNCALLABLE-INSTANCE-INFO) touches
# the card dirty bit when reassigning the CLOS slot vector.
# We need to simulate a GC interrupt after altering the layout,
# but prior to affecting the slot vector.
run_sbcl <<EOF
  (defclass subgf (standard-generic-function) (a)
    (:metaclass sb-mop:funcallable-standard-class))
  (defgeneric myfun (a)
    (:generic-function-class subgf)
    (:method ((self integer)) 'hey-integer))
  (defun assign-layout ()
    #+gencgc (setf (extern-alien "verify_gens" char) 0)
    (defclass subgf (standard-generic-function) (a b) ; add a slot
      (:metaclass sb-mop:funcallable-standard-class))
    (defclass subgf (standard-generic-function) (a) ; remove a slot
      (:metaclass sb-mop:funcallable-standard-class))
    (let ((nl (sb-kernel:wrapper-friend (sb-kernel:find-layout 'subgf)))) ; new layout
      (assert (not (eq (sb-kernel:%fun-layout #'myfun) nl)))
      (sb-kernel:%set-fun-layout #'myfun nl)
      (gc)))
  (save-lisp-and-die "$tmpcore" :toplevel #'assign-layout)
EOF
run_sbcl_with_core "$tmpcore" --noinform --no-userinit --no-sysinit
check_status_maybe_lose "SET-FIN-LAYOUT" $? 0 "(saved core ran)"

exit $EXIT_TEST_WIN
