
;;; Regression tests for bugs cited in
;;; https://groups.google.com/g/sbcl-devel/c/4XTJ9hEUngM/m/B5-iQxdTAAAJ

;;; 1. loading an externalized literal prior to seeing a %DEFSTRUCT
;;;    and %TARGET-DEFSTRUCT for the type.
;;     Presence of raw slots is irrelevant.
(with-test (:name :literal-before-defstruct)
  (with-scratch-file (srcname "lisp")
    (with-scratch-file (fasl "fasl")
      (with-open-file (src srcname :direction :output)
        (print '(in-package "STRUCT") src)
        (print '(eval-when (:compile-toplevel :load-toplevel)
                 (defstruct charstruc (c #\a :type character)))
               src)
        (print '(eval-when (:compile-toplevel)
                 (defmethod make-load-form ((x charstruc) &optional e)
                   (make-load-form-saving-slots x :environment e)))
               src)
        ;; Write as a string because #S() won't work until CHARSTRUC is compiled
        (write-string "(defparameter *s* #s(charstruc :c #\\z))" src))
      (make-package "STRUCT" :use '("CL"))
      (compile-file srcname :output-file fasl :verbose nil)
      (delete-package "STRUCT")
      (make-package "STRUCT" :use '("CL"))
      (load fasl))))

;;; 2. referencing a layout and then defining the structure
;;;    could fail if there are raw slots.
(with-test (:name :no-spurious-redef-warning)
  (with-scratch-file (srcname "lisp")
    (with-scratch-file (fasl "fasl")
      (with-open-file (src srcname :direction :output)
        (let ((defstruct
                  `(defstruct (big (:predicate nil))
                     (first t)
                     ,@(loop for i below sb-vm:n-word-bits
                             collect `(,(sb-int:symbolicate "RAW" i)
                                        0 :type cl:double-float))
                     ,@(loop for i below 5
                             collect `(,(sb-int:symbolicate "MORE" i) nil)))))
          (print '(in-package "STRUCT") src)
          (print `(eval-when (:compile-toplevel) ,defstruct) src) ; for compiling a type-check
          (print `(defun bigp (x) (typep x 'big)) src)
          (print defstruct src)))
      ;; Reusing the same package as from test #1
      ;; (make-package "STRUCT" :use '("CL"))
      (compile-file srcname :output-file fasl :verbose nil)
      (delete-package "STRUCT")
      (make-package "STRUCT" :use '("CL"))
      (load fasl))))
