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

(in-package :cl-user)

;;; Array initialization has complicated defaulting for :ELEMENT-TYPE,
;;; and both compile-time and run-time logic takes a whack at it.
(let ((testcases '(;; Bug 126, confusion between high-level default string
		   ;; initial element #\SPACE and low-level default array
		   ;; element #\NULL, is gone.
		   (#\null (make-array 11 :element-type 'character))
		   (#\space (make-string 11 :initial-element #\space))
		   (#\* (make-string 11 :initial-element #\*))
		   (#\null (make-string 11))
		   (#\null (make-string 11 :initial-element #\null))
		   (#\x (make-string 11 :initial-element #\x))
		   ;; And the other tweaks made when fixing bug 126 didn't
		   ;; mess things up too badly either.
		   (0 (make-array 11))
		   (nil (make-array 11 :initial-element nil))
		   (12 (make-array 11 :initial-element 12))
		   (0 (make-array 11 :element-type '(unsigned-byte 4)))
		   (12 (make-array 11
				   :element-type '(unsigned-byte 4)
				   :initial-element 12)))))
  (dolist (testcase testcases)
    (destructuring-bind (expected-result form) testcase
      (unless (eql expected-result (aref (eval form) 3))
        (error "expected ~S in EVAL ~S" expected-result form))
      (unless (eql expected-result
		   (aref (funcall (compile nil `(lambda () ,form))) 3))
        (error "expected ~S in FUNCALL COMPILE ~S" expected-result form)))))
