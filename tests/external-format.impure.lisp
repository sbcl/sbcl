;;;; This file is for testing external-format functionality, using
;;;; test machinery which might have side effects (e.g.  executing
;;;; DEFUN, writing files).  Note that the tests here reach into
;;;; unexported functionality, and should not be used as a guide for
;;;; users.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;; 
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(defmacro do-external-formats ((xf &optional result) &body body)
  (let ((nxf (gensym)))
    `(dolist (,nxf sb-impl::*external-formats* ,result)
       (let ((,xf (first (first ,nxf))))
	 ,@body))))

(do-external-formats (xf)
  (with-open-file (s "/dev/null" :direction :input :external-format xf)
    (assert (eq (read-char s nil s) s))))

(let ((s (open "external-format-test.lisp" :direction :output
	       :if-exists :supersede :external-format :latin-1)))
  (unwind-protect
       (progn
	 (write-string ";;; ABCD" s)
	 (write-char (code-char 233) s)
	 (terpri s)
	 (close s)
	 (compile-file "external-format-test.lisp" :external-format :utf-8))
    (delete-file s)
    (let ((p (probe-file (compile-file-pathname "external-format-test.lisp"))))
      (when p
	(delete-file p)))))

(sb-ext:quit :unix-status 104)
    