(cl:in-package :cl-user)

(declaim (optimize (debug 3) (speed 2) (space 1)))

;;; Until version 0.6.9 or so, SBCL's version of Python couldn't this
;;; correctly, due to the bug patched by Rob MacLachlan on the
;;; cmucl-imp list 2000-06-21, and apply to SBCL by Martin Atzmueller.
;;; (The effectiveness of the test also depends on the implicit
;;; function typing of Python (where DEFUN is like DECLAIM FTYPE),
;;; which violates the ANSI spec, and should be fixed. Once that
;;; unrelated bug is fixed, this code will no longer test the type
;;; inference behavior it's intended to test.)
(defun emptyvalues (&rest rest) (declare (ignore rest)) (values))
(defstruct foo x y)
(defun bar ()
  (let ((res (emptyvalues)))
    (unless (typep res 'foo)
      'expected-value)))
(assert (eq (bar) 'expected-value))

(sb-ext:quit :unix-status 104) ; success
