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
				     (let ((initial-element
					    (cond ((subtypep eltype 'character)
						   #\!)
						  ((subtypep eltype 'number)
						   0)
						  (t #'error))))
				       (replace (make-array
						 (+ (length base-seq)
						    (random 3))
						 :element-type eltype
						 :fill-pointer
						 (length base-seq)
						 :initial-element
						 initial-element)
						base-seq))
				     (return))))))))
		 (lambda-expr `(lambda (seq)
				 ,@(when declaredness
				     `((declare (type ,seq-type seq))))
				 (declare (optimize ,@optimization))
				 ,snippet)))
	    (format t "~&~S~%" lambda-expr)
	    (multiple-value-bind (fun warnings-p failure-p)
		(compile nil lambda-expr)
	      (when (or warnings-p failure-p)
		(error "~@<failed compilation:~2I ~_LAMBDA-EXPR=~S ~_WARNINGS-P=~S ~_FAILURE-P=~S~:@>"
		       lambda-expr warnings-p failure-p))
	      (format t "~&~S ~S ~S ~S ~S~%"
		      base-seq snippet seq-type declaredness optimization)
	      (format t "~&(TYPEP SEQ 'SIMPLE-ARRAY)=~S~%"
		      (typep seq 'simple-array))
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

;;; a wrapper to hide declared type information from the compiler, so
;;; we don't get stopped by compiler warnings about e.g. compiling
;;; (POSITION 1 #() :KEY #'ABS) when #() has been coerced to a string.
(defun indiscriminate (fun)
  (lambda (&rest rest) (apply fun rest)))
  
;;; asymmetric test arg order example from ANSI FIND definition page
(assert (eql #\space ; original example, depends on ASCII character ordering
	     (find #\d "here are some letters that can be looked at"
		   :test #'char>)))
(assert (eql #\e ; modified example, depends only on standard a-z ordering
	     (find #\f "herearesomeletters" :test #'char>)))
(assert (eql 4 ; modified more, avoids charset technicalities completely
	     (find 5 '(6 4) :test '>)))

;;; tests of FIND, POSITION, FIND-IF, and POSITION-IF (and a few for
;;; deprecated FIND-IF-NOT and POSITION-IF-NOT too)
(for-every-seq #()
  '((null (find 1 seq))
    (null (find 1 seq :from-end t))
    (null (position 1 seq :key (indiscriminate #'abs)))
    (null (position nil seq :test (constantly t)))
    (null (position nil seq :test nil))
    (null (position nil seq :test-not nil))
    (null (find-if #'1+ seq :key (indiscriminate #'log)))
    (null (position-if #'identity seq :from-end t))
    (null (find-if-not #'packagep seq))
    (null (position-if-not #'packagep seq :key nil))))
(for-every-seq #(1)
  '((null (find 2 seq))
    ;; Get the argument ordering for asymmetric tests like #'> right.
    ;; (bug reported and fixed by Alexey Dejneka sbcl-devel 2001-10-17)
    (eql 1 (find 2 seq :test #'>))
    (find 2 seq :key #'1+)
    (find 1 seq :from-end t)
    (null (find 1 seq :from-end t :start 1))
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
    (eql 1 (position 1.5 seq :test #'<))
    (eql 0 (position 0 seq :key '1-))
    (eql 4 (position 0 seq :key '1- :from-end t))
    (eql 2 (position 4 seq :key '1+))
    (eql 2 (position 4 seq :key '1+ :from-end t))
    (eql 1 (position 2 seq))
    (eql 1 (position 2 seq :start 1))
    (null (find 2 seq :start 1 :end 1))
    (eql 3 (position 2 seq :start 2))
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
    (eql 2 (position-if #'plusp seq :from-end nil :key '1- :start 2))
    (eql 3 (position-if #'plusp seq :from-end t :key '1- :start 2))
    (null (position-if #'plusp seq :from-end t :key '1- :start 2 :end 2))
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
    (find-if (lambda (c) (typep c 'base-char)) seq :from-end t)
    (null (find-if 'upper-case-p seq))))
	 
;;; SUBSEQ
(let ((avec (make-array 10
			:fill-pointer 4
			:initial-contents '(0 1 2 3 iv v vi vii iix ix))))
  ;; These first five always worked AFAIK.
  (assert (equalp (subseq avec 0 3) #(0 1 2)))
  (assert (equalp (subseq avec 3 3) #()))
  (assert (equalp (subseq avec 1 3) #(1 2)))
  (assert (equalp (subseq avec 1) #(1 2 3)))
  (assert (equalp (subseq avec 1 4) #(1 2 3)))
  ;; SBCL bug found ca. 2002-05-01 by OpenMCL's correct handling of
  ;; SUBSEQ, CSR's driving portable cross-compilation far enough to
  ;; reach the SUBSEQ calls in assem.lisp, and WHN's sleazy
  ;; translation of old CMU CL new-assem.lisp into sufficiently grotty
  ;; portable Lisp that it passed suitable illegal values to SUBSEQ to
  ;; exercise the bug:-|
  ;;
  ;; SUBSEQ should check its END value against logical LENGTH, not
  ;; physical ARRAY-DIMENSION 0.
  ;;
  ;; fixed in sbcl-0.7.4.22 by WHN
  (assert (null (ignore-errors (aref (subseq avec 1 5) 0)))))

;;; FILL
(defun test-fill-typecheck (x)
  (declare (optimize (safety 3) (space 2) (speed 1)))
  (fill (make-string 10) x))

(assert (string= (test-fill-typecheck #\@) "@@@@@@@@@@"))
;;; BUG 186, fixed in sbcl-0.7.5.5
(assert (null (ignore-errors (test-fill-typecheck 4097))))

;;; MAKE-SEQUENCE, COERCE, CONCATENATE, MERGE, MAP and requested
;;; result type (BUGs 46a, 46b, 66)
(macrolet ((assert-type-error (form)
	     `(assert (typep (nth-value 1 (ignore-errors ,form)) 
			     'type-error))))
  (dolist (type-stub '((simple-vector) 
		       (vector *) 
		       (vector (signed-byte 8))
		       (vector (unsigned-byte 16))
		       (vector (signed-byte 32))
		       (simple-bit-vector)))
    (declare (optimize safety))
    (format t "~&~S~%" type-stub)
    ;; MAKE-SEQUENCE
    (assert (= (length (make-sequence `(,@type-stub) 10)) 10))
    (assert (= (length (make-sequence `(,@type-stub 10) 10)) 10))
    (assert-type-error (make-sequence `(,@type-stub 10) 11))
    ;; COERCE
    (assert (= (length (coerce '(0 0 0) `(,@type-stub))) 3))
    (assert (= (length (coerce #(0 0 0) `(,@type-stub 3))) 3))
    (assert-type-error (coerce #*111 `(,@type-stub 4)))
    ;; CONCATENATE
    (assert (= (length (concatenate `(,@type-stub) #(0 0 0) #*111)) 6))
    (assert (equalp (concatenate `(,@type-stub) #(0 0 0) #*111)
		   (coerce #(0 0 0 1 1 1) `(,@type-stub))))
    (assert (= (length (concatenate `(,@type-stub 6) #(0 0 0) #*111)) 6))
    (assert (equalp (concatenate `(,@type-stub 6) #(0 0 0) #*111)
		   (coerce #(0 0 0 1 1 1) `(,@type-stub 6))))
    (assert-type-error (concatenate `(,@type-stub 5) #(0 0 0) #*111))
    ;; MERGE
    (assert (= (length (merge `(,@type-stub) #(0 1 0) #*111 #'>)) 6))
    (assert (equalp (merge `(,@type-stub) #(0 1 0) #*111 #'>)
		   (coerce #(1 1 1 0 1 0) `(,@type-stub))))
    (assert (= (length (merge `(,@type-stub 6) #(0 1 0) #*111 #'>)) 6))
    (assert (equalp (merge `(,@type-stub 6) #(0 1 0) #*111 #'>)
		   (coerce #(1 1 1 0 1 0) `(,@type-stub 6))))
    (assert-type-error (merge `(,@type-stub 4) #(0 1 0) #*111 #'>))
    ;; MAP
    (assert (= (length (map `(,@type-stub) #'logxor #(0 0 1 1) '(0 1 0 1))) 4))
    (assert (equalp (map `(,@type-stub) #'logxor #(0 0 1 1) '(0 1 0 1))
		   (coerce #(0 1 1 0) `(,@type-stub))))
    (assert (= (length (map `(,@type-stub 4) #'logxor #(0 0 1 1) '(0 1 0 1))) 
	       4))
    (assert (equalp (map `(,@type-stub 4) #'logxor #(0 0 1 1) '(0 1 0 1))
		   (coerce #(0 1 1 0) `(,@type-stub 4))))
    (assert-type-error (map `(,@type-stub 5) #'logxor #(0 0 1 1) '(0 1 0 1))))
  ;; some more CONCATENATE tests for strings
  (locally 
      (declare (optimize safety))
    (assert (string= (concatenate 'string "foo" " " "bar") "foo bar"))
    (assert (string= (concatenate '(string 7) "foo" " " "bar") "foo bar"))
    (assert-type-error (concatenate '(string 6) "foo" " " "bar"))
    (assert (string= (concatenate '(string 6) "foo" #(#\b #\a #\r)) "foobar"))
    (assert-type-error (concatenate '(string 7) "foo" #(#\b #\a #\r))))
  ;; SIMPLE-ARRAY isn't allowed as a vector type specifier
  (locally
      (declare (optimize safety))
    (assert-type-error (concatenate 'simple-array "foo" "bar"))
    (assert-type-error (map 'simple-array #'identity '(1 2 3)))
    (assert-type-error (coerce '(1 2 3) 'simple-array))
    (assert-type-error (merge 'simple-array '(1 3) '(2 4) '<))
    ;; but COERCE has an exemption clause:
    (assert (string= "foo" (coerce "foo" 'simple-array)))
    ;; ... though not in all cases.
    (assert-type-error (coerce '(#\f #\o #\o) 'simple-array))))

;;; As pointed out by Raymond Toy on #lisp IRC, MERGE had some issues
;;; with user-defined types until sbcl-0.7.8.11
(deftype list-typeoid () 'list)
(assert (equal '(1 2 3 4) (merge 'list-typeoid '(1 3) '(2 4) '<)))
;;; and also with types that weren't precicely LIST
(assert (equal '(1 2 3 4) (merge 'cons '(1 3) '(2 4) '<)))

;;; success
(quit :unix-status 104)
