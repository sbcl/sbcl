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
		   (#\null (make-array 11 :element-type 'character) simple-string)
		   (#\space (make-string 11 :initial-element #\space) string)
		   (#\* (make-string 11 :initial-element #\*))
		   (#\null (make-string 11))
		   (#\null (make-string 11 :initial-element #\null))
		   (#\x (make-string 11 :initial-element #\x))
		   ;; And the other tweaks made when fixing bug 126 didn't
		   ;; mess things up too badly either.
		   (0 (make-array 11) simple-vector)
		   (nil (make-array 11 :initial-element nil))
		   (12 (make-array 11 :initial-element 12))
		   (0 (make-array 11 :element-type '(unsigned-byte 4)) (simple-array (unsigned-byte 4) (*)))
		   (12 (make-array 11
				   :element-type '(unsigned-byte 4)
				   :initial-element 12)))))
  (dolist (testcase testcases)
    (destructuring-bind (expected-result form &optional type) testcase
      (unless (eql expected-result (aref (eval form) 3))
        (error "expected ~S in EVAL ~S" expected-result form))
      (unless (eql expected-result
		   (aref (funcall (compile nil `(lambda () ,form))) 3))
        (error "expected ~S in FUNCALL COMPILE ~S" expected-result form))
      ;; also do some testing of compilation and verification that
      ;; errors are thrown appropriately.
      (unless (eql expected-result
		   (funcall (compile nil `(lambda () (aref ,form 3)))))
	(error "expected ~S in COMPILED-AREF ~S" expected-result form))
      (when type
	(unless (eql expected-result
		     (funcall (compile nil `(lambda () (let ((x ,form))
							 (declare (type ,type x))
							 (aref x 3))))))
	  (error "expected ~S in COMPILED-DECLARED-AREF ~S" expected-result form)))
      (when (ignore-errors (aref (eval form) 12))
	(error "error not thrown in EVAL ~S" form))
      (when (ignore-errors (aref (funcall (compile nil `(lambda () ,form))) 12))
	(error "error not thrown in FUNCALL COMPILE ~S"))
      (when (ignore-errors (funcall (compile nil `(lambda () (aref ,form 12)))))
	(error "error not thrown in COMPILED-AREF ~S" form))
      (when type
	(when (ignore-errors (funcall
			      (compile nil `(lambda () (let ((x ,form))
							 (declare (type ,type x))
							 (aref x 12))))))
	  (error "error not thrown in COMPILED-DECLARED-AREF ~S" form))))))

;;; On the SPARC, until sbcl-0.7.7.20, there was a bug in array
;;; references for small vector elements (spotted by Raymond Toy); the
;;; bug persisted on the PPC until sbcl-0.7.8.20.
(let (vector)
  (loop for i below 64
	for list = (make-list 64 :initial-element 1)
	do (setf (nth i list) 0)
	do (setf vector (make-array 64 :element-type 'bit 
				       :initial-contents list))
	do (assert (= (funcall 
		       (compile nil 
				`(lambda (rmdr)
				  (declare (type (simple-array bit (*)) rmdr)
				           (optimize (speed 3) (safety 0)))
				  (aref rmdr ,i)))
		       vector)
		      0))))

;;; Following refactoring of sequence functions to detect bad type
;;; specifiers, REVERSE was left broken on vectors with fill pointers.
(let ((a (make-array 10
		     :fill-pointer 5
		     :element-type 'character
		     :initial-contents "abcdefghij")))
  (assert (string= (reverse a) "edcba")))
