(require 'sb-posix)

(defpackage multiple-file-compile
  (:use :cl :cl-user)
  (:export :foo :bar))

(defun compile-forms-as-files (forms)
  (declare (type list forms))
  (let* ((dir (pathname (concatenate 'string (scratch-file-name) "/"))))
    (sb-posix:mkdir dir #b111111111)
    (let ((lisps
            (loop for f in forms
                  collect
                  (merge-pathnames (pathname-name (pathname (scratch-file-name "lisp")))
                                   dir)))
          (fasl (merge-pathnames (pathname-name (pathname (scratch-file-name "fasl")))
                                 dir)))
      (mapc #'(lambda (lisp-file form)
                (with-open-file (f lisp-file :direction :output)
                  (prin1 form f)))
            lisps forms)
      (let ((res (progn
                   (sb-c:compile-files lisps :output-file fasl)
                   (load fasl)
                   (sb-c:compile-files lisps :output-file fasl :block-compile t)
                   (load fasl)
                   (multiple-file-compile:bar 2 3)
                   t)))
        (mapc 'delete-file (directory (merge-pathnames "*.*" dir)))
        (sb-posix:rmdir dir)
        res))))

(with-test (:name :compile-multiple-files)
  (compile-forms-as-files
   `((defpackage multiple-file-compile
       (:use :cl :cl-user)
       (:export :foo :bar))
     (progn
       (in-package :multiple-file-compile)
       (defun multiple-file-compile:foo (a b)
         (+ a b)))
     (progn
       (in-package :multiple-file-compile)
       (defun multiple-file-compile:bar (a b)
         (* a (multiple-file-compile:foo a b)))))))
