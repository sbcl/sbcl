;;; Test from git rev cf65b9804d28c5e6ee2fa53cbac143c2f87f108c
;;; which wasn't fully testing what it purported to, which is that
;;; the fopcompiler signals a warning about undefined variables.
;;; (There is no way to assert that by merely placing it in a cload file)
(defvar *value-passer*)
(defun trythis (x) (setf *value-passer* x))
(defvar *was-fopcompile-called* nil)
(sb-int:encapsulate 'sb-c::fopcompile 'test
  (compile nil '(lambda (realfun &rest args)
                 (setf *was-fopcompile-called* t)
                 (apply realfun args))))
(with-test (:name :fopcompile-undefined-var)
  (with-scratch-file (fasl "fasl")
    (with-scratch-file (lisp "lisp")
      (with-open-file (f lisp :direction :output)
        (prin1 '(trythis fopcompile-test-undef-var) f))
      (multiple-value-bind (fasl warningp errorp)
          (compile-file lisp :output-file fasl)
        (sb-int:unencapsulate 'sb-c::fopcompile 'test)
        (assert *was-fopcompile-called*)
        (assert (and fasl warningp errorp))))
    (setf (symbol-value 'fopcompile-test-undef-var) 1)
    (assert (eql (let ((*value-passer*)) (load fasl) *value-passer*)
                 1))))
