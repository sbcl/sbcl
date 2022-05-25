
;;; Regression test for bug cited in
;;; https://groups.google.com/g/sbcl-devel/c/4XTJ9hEUngM/m/B5-iQxdTAAAJ
;;; where referencing a layout and then defining the structure
;;; could fail if there are raw slots.
(with-test (:name :no-spurious-redef-warning)
  (with-scratch-file (srcname "lisp")
    (with-scratch-file (fasl "fasl")
      (with-open-file (src srcname :direction :output)
        (let ((defstruct
                  `(defstruct (big (:predicate nil))
                     (first t)
                     ,@(loop for i below sb-vm:n-word-bits
                             collect `(,(sb-int:symbolicate "RAW" (write-to-string i))
                                        0 :type cl:double-float))
                     ,@(loop for i below 5
                             collect `(,(sb-int:symbolicate "MORE" (write-to-string i)) nil)))))
          (print '(in-package "STRUCT") src)
          (print `(eval-when (:compile-toplevel) ,defstruct) src) ; for compiling a type-check
          (print `(defun bigp (x) (typep x 'big)) src)
          (print defstruct src)))
      (make-package "STRUCT" :use '("CL"))
      (compile-file srcname :output-file fasl)
      (delete-package "STRUCT")
      (make-package "STRUCT" :use '("CL"))
      (load fasl))))
