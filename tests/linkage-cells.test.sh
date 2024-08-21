# If this test were in a .lisp file then the FILE-INFO would hold on
# to all function names and so we couldn't show that GC works.
. ./subr.sh
run_sbcl <<EOF
;; This test fails on mark-region. It appears to smash weak vectors more lazily,
;; which doesn't make sense unless the weak vector is actually livening its contents
;; because the symbol G is most certainly in the youngest generation and eligible
;; to become garbage.
;;; I don't know how to make a shell test exit with an EXPECTED failure.
#-(and x86-64 linux gencgc) (sb-sys:os-exit 0) ; should be "test inapplicable"
(declaim (ftype function g bar))
(defun f (x) (g x))
(compile 'f)
(defvar *li* (sb-vm::fname-linkage-index 'g))
;;(print *li*)
(defvar *wp* (make-weak-pointer (sb-vm::linkage-addr->name *li* :index)))
(fmakunbound 'f)
(unintern 'g)
(setq * nil ** nil *** nil
      + nil ++ nil +++ nil
      / nil // nil /// nil)
(gc) ; even changing this to :FULL does not fix the mark-region failure
(unless (null (sb-vm::linkage-addr->name *li* :index))
  ;; Do not  extract WEAK-POINTER-VALUE in this thread, lest it be referenced
  ;; from the control stack.
  (sb-thread:join-thread
   (sb-thread:make-thread
    (lambda (wp)
      (let ((obj (weak-pointer-value wp)))
        (format t "~&Searching for path to ~S (g~D)~%" obj
                (sb-kernel:generation-of obj))))
    :arguments *wp*))
  (search-roots *wp* :criterion :static :print t))
(assert (null (sb-vm::linkage-addr->name *li* :index)))
(defun foo () (bar))
(compile 'foo)
;; BAR should get the same linkage index as G had
(assert (= (sb-vm::fname-linkage-index 'bar) *li*))
EOF
if [ $? -eq 0 ]
then
  exit $EXIT_TEST_WIN
fi
