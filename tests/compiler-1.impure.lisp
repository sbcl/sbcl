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

(declaim (ftype (function (real) (values integer single-float)) valuesify))
(defun valuesify (x)
  (values (round x)
	  (coerce x 'single-float)))
(defun exercise-valuesify (x)
  (multiple-value-bind (i f) (valuesify x)
    (declare (type integer i))
    (declare (type single-float f))
    (+ i f)))
(assert (= (exercise-valuesify 1.25) 2.25))

(sb-ext:quit :unix-status 104) ; success
