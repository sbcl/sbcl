;;;; tests related to sequences

;;;; This file is impure because we want to be able to use DEFUN.

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

;;; helper functions for exercising SEQUENCE code on data of many
;;; specialized types, and in many different optimization scenarios
(defun for-every-seq-1 (base-seq snippet)
  (dolist (seq-type '(list
		      (simple-array t 1)
		      (vector t)
		      (simple-array character 1)
		      (vector character)
		      (simple-array (signed-byte 4) 1)
		      (vector (signed-byte 4))))
    (flet ((entirely (eltype)
	     (every (lambda (el) (typep el eltype)) base-seq)))
      (dolist (declaredness '(nil t))
	(dolist (optimization '(((speed 3) (space 0))
				((speed 2) (space 2))
				((speed 1) (space 2))
				((speed 0) (space 1))))
	  (let* ((seq (if (eq seq-type 'list)
			  (coerce base-seq 'list)
			  (destructuring-bind (type-first &rest type-rest)
			      seq-type
			    (ecase type-first
			      (simple-array
			       (destructuring-bind (eltype one) type-rest
				 (assert (= one 1))
				 (if (entirely eltype)
				     (coerce base-seq seq-type)
				     (return))))
			      (vector
			       (destructuring-bind (eltype) type-rest
				 (if (entirely eltype)
				     (replace (make-array (length base-seq)
							  :element-type eltype
							  :adjustable t)
					      base-seq)
				     (return))))))))
		 (lambda-expr `(lambda (seq)
				 ,@(when declaredness
				     `((declare (type ,seq-type seq))))
				 (declare (optimize ,@optimization))
				 ,snippet)))
	    (multiple-value-bind (fun warnings-p failure-p)
		(compile nil lambda-expr)
	      (when (or warnings-p failure-p)
		(error "~@<failed compilation:~2I ~_WARNINGS-P=~S ~_FAILURE-P=~S ~_LAMBDA-EXPR=~S~:@>" lambda-expr))
	      (unless (funcall fun seq)
		(error "~@<failed test:~2I ~_BASE-SEQ=~S ~_SNIPPET=~S ~_SEQ-TYPE=~S ~_DECLAREDNESS=~S ~_OPTIMIZATION=~S~:@>"
		       base-seq
		       snippet
		       seq-type
		       declaredness
		       optimization)))))))))
(defun for-every-seq (base-seq snippets)
  (dolist (snippet snippets)
    (for-every-seq-1 base-seq snippet)))
		
;;; tests of FIND, POSITION, FIND-IF, and POSITION-IF (and a few for
;;; deprecated FIND-IF-NOT and POSITION-IF-NOT too)
(for-every-seq #()
  '((null (find 1 seq))
    (null (find 1 seq :from-end t))
    (null (position 1 seq :key #'abs))
    (null (position nil seq :test (constantly t)))
    (null (position nil seq :test nil))
    (null (position nil seq :test-not nil))
    (null (find-if #'1+ seq :key #'log))
    (null (position-if #'identity seq :from-end t))
    (null (find-if-not #'packagep seq))
    (null (position-if-not #'packagep seq :key nil))))
(for-every-seq #(1)
  '((null (find 2 seq))
    (find 2 seq :key #'1+)
    (find 1 seq :from-end t)
    (null (find 0 seq :from-end t))
    (eql 0 (position 1 seq :key #'abs))
    (null (position nil seq :test 'equal))
    (eql 1 (find-if #'1- seq :key #'log))
    (eql 0 (position-if #'identity seq :from-end t))
    (null (find-if-not #'sin seq))
    (eql 0 (position-if-not #'packagep seq :key 'identity))))
(for-every-seq #(1 2 3 2 1)
  '((find 3 seq)
    (find 3 seq :from-end 'yes)
    (eql 0 (position 0 seq :key '1-))
    (eql 4 (position 0 seq :key '1- :from-end t))
    (eql 2 (position 4 seq :key '1+))
    (eql 2 (position 4 seq :key '1+ :from-end t))
    (eql 1 (position 2 seq))
    (eql 3 (position 2 seq :key nil :from-end t))
    (eql 2 (position 3 seq :test '=))
    (eql 0 (position 3 seq :test-not 'equalp))
    (eql 2 (position 3 seq :test 'equal :from-end t))
    (null (position 4 seq :test #'eql))
    (null (find-if #'packagep seq))
    (eql 1 (find-if #'plusp seq))
    (eql 3 (position-if #'plusp seq :key #'1- :from-end t))
    (eql 1 (position-if #'evenp seq))
    (eql 3 (position-if #'evenp seq :from-end t))
    (null (find-if-not #'plusp seq))
    (eql 0 (position-if-not #'evenp seq))))
(for-every-seq "string test"
  '((null (find 0 seq))
    (null (find #\D seq :key #'char-upcase))
    (find #\E seq :key #'char-upcase)
    (null (find #\e seq :key #'char-upcase))
    (eql 3 (position #\i seq))
    (eql 0 (position #\s seq :key #'char-downcase))
    (eql 1 (position #\s seq :key #'char-downcase :test #'char/=))
    (eql 9 (position #\s seq :from-end t :test #'char=))
    (eql 10 (position #\s seq :from-end t :test #'char/=))
    (eql 4 (position #\N seq :from-end t :key 'char-upcase :test #'char-equal))
    (eql 5 (position-if (lambda (c) (equal #\g c)) seq))
    (eql 5 (position-if (lambda (c) (equal #\g c)) seq :from-end t))
    (find-if #'characterp seq)
    (find-if #'(lambda (c) (typep c 'base-char)) seq :from-end t)
    (null (find-if 'upper-case-p seq))))

;;; success
(quit :unix-status 104)
