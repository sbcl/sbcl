;;;; miscellaneous operations on functions, returning functions, or
;;;; primarily useful for functional programming

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defun identity (thing)
  "This function simply returns what was passed to it."
  thing)

(defun complement (function)
  "Return a new function that returns T whenever FUNCTION returns NIL and
   NIL whenever FUNCTION returns non-NIL."
  ;; KLUDGE: constraint propagation is unable to detect that NTH gets
  ;; called only on indices known to be less than the predetermined N.
  (macrolet ((arg (n) `(fast-&rest-nth ,n arguments)))
    (lambda (&rest arguments)
      (not (let ((n (length arguments)))
             (if (> n 3)
                 (apply function arguments)
                 (case n
                  (1 (funcall function (arg 0)))
                  (2 (funcall function (arg 0) (arg 1)))
                  (3 (funcall function (arg 0) (arg 1) (arg 2)))
                  (t (funcall function)))))))))

(defun constantly (value)
  "Return a function that always returns VALUE."
  (lambda (&rest arguments)
    (declare (ignore arguments))
    (declare (optimize (speed 3) (safety 0) (debug 0)))
    value))

;;; some commonly-occurring CONSTANTLY forms
(macrolet ((def-constantly-fun (name constant-expr)
             `(progn
                (declaim (ftype (sfunction * (eql ,constant-expr)) ,name))
                (setf (symbol-function ',name)
                      (constantly ,constant-expr)))))
  (def-constantly-fun constantly-t t)
  (def-constantly-fun constantly-nil nil)
  (def-constantly-fun constantly-0 0))
