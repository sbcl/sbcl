#-x86-64 (invoke-restart 'run-tests::skip-file)

(defvar *lookups* nil)

(with-scratch-file (source "lisp")
  (with-open-file (stream source :direction :output :if-does-not-exist :create)
    (write-string "
(defun f1 (x)
  (declare (notinline g1))
  (g1 x))

(defun f2 (x)
  (when (fboundp 'g2) (funcall 'g2 x)))

(defun f3 (x) (g3 x))

(defun f4 (x)
  (when (fboundp 'g3) (g4 x)))" stream))
  (let ((fasl (handler-bind ((style-warning #'muffle-warning))
                (compile-file source))))
    (delete-file source)
    (sb-int:encapsulate 'sb-int:ensure-linkage-index 'trace
       (compile nil
        '(lambda (realfun fname &optional quiet)
           (when (member fname '(g1 g2 g3 g4))
             (push (cons fname quiet) *lookups*))
           (funcall realfun fname quiet))))
    (load fasl)
    (delete-file fasl)
    (sb-int:unencapsulate 'sb-int:ensure-linkage-index 'trace)
    (assert (equal (nreverse *lookups*)
                   '((g1 . t) ; g1 and g2 are looked up silently
                     (g2 . t)
                     (g3 . nil) ; g3 and g4 will check for a defined callee
                     (g4 . nil))))))
