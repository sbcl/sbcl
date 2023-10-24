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
./"$tmpcore" --disable-ldb --no-userinit --no-sysinit --noprint
echo "Saved finalizer smoke test: PASS"

# Assert that >1 finalizer on an object are correctly saved/restored.
run_sbcl <<EOF
(defvar *x* "hi")
(defun a () (print 1))
(defun b () (print 2))
(defun c () (print 3))
(finalize *x* #'a)
(finalize *x* #'b :dont-save t)
(finalize *x* #'c)
(save-lisp-and-die "$tmpcore")
EOF
# this would crash in finalizers-reinit
run_sbcl_with_core "$tmpcore" --noinform --disable-ldb --no-userinit --no-sysinit --noprint <<EOF
(sb-sys:with-pinned-objects (*x*)
  (let ((list (sb-lockless:so-data
               (sb-lockless:so-find sb-impl::**finalizer-store**
                (sb-kernel:%make-lisp-obj
                 (logandc2 (sb-kernel:get-lisp-obj-address *x*)
                           sb-vm:lowtag-mask))))))
    (assert (= (length list) 2))
    (assert (find #'a list))
    (assert (find #'c list))))
EOF
rm "$tmpcore"
echo "One obj, three finalizers: PASS"

# Assert that :DONT-SAVE finalizers which were triggered during the DEINITIALIZE flow
# and placed in the triggered list do not act as though they were saved.
# i.e. They're ineligible for running on restart, but also - as a design choice -
# not run during saving of the core even when the object died.
run_sbcl <<EOF
(progn (sb-ext:finalize (make-array 1) (lambda () (format t "RAN~%")) :dont-save t) nil)
(save-lisp-and-die "$tmpcore" :executable t)
EOF
result=`./"$tmpcore" --disable-ldb --no-userinit --no-sysinit --noprint \
  --eval '(sleep .0125)' --eval '(quit)'`
if [ "x$result" = "x" ]
then
  echo "Triggered DONT-SAVE finalizer: PASS"
else
  echo "Triggered DONT-SAVE finalizer: FAIL"
  exit 1
fi
rm "$tmpcore"
exit $EXIT_TEST_WIN
