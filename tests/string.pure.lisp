;;;; miscellaneous tests of STRING-related stuff

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

(in-package "CL-USER")

;;; basic non-destructive case operations
(assert (string= (string-upcase     "This is a test.") "THIS IS A TEST."))
(assert (string= (string-downcase   "This is a test.") "this is a test."))
(assert (string= (string-capitalize "This is a test.") "This Is A Test."))
(assert (string= (string-upcase "Is this 900-Sex-hott, please?" :start 3)
		 "Is THIS 900-SEX-HOTT, PLEASE?"))
(assert (string= (string-downcase "Is this 900-Sex-hott, please?"
				  :start 10 :end 16)
		 "Is this 900-sex-hott, please?"))
(assert (string= (string-capitalize "Is this 900-Sex-hott, please?")
		 "Is This 900-Sex-Hott, Please?"))

;;; The non-destructive case operations accept string designators, not
;;; just strings.
(assert (string= (string-upcase '|String designator|) "STRING DESIGNATOR"))
(assert (string= (string-downcase #\S) "s"))
(assert (string= (string-downcase #\.) "."))
(assert (string= (string-capitalize 'ya-str-desig :end 5) "Ya-StR-DESIG"))

;;; basic destructive case operations
(let ((nstring (make-array 5 :element-type 'character :fill-pointer 0)))
  (vector-push-extend #\c nstring)
  (vector-push-extend #\a nstring)
  (vector-push-extend #\t nstring)
  (nstring-upcase nstring)
  (assert (string= nstring "CAT"))
  (setf (fill-pointer nstring) 2)
  (nstring-downcase nstring :start 1)
  (setf (fill-pointer nstring) 3)
  (assert (string= nstring "CaT"))
  (nstring-capitalize nstring)
  (assert (string= nstring "Cat")))

;;; (VECTOR NIL)s are strings.  Tests for that and issues uncovered in
;;; the process.
(assert (typep (make-array 1 :element-type nil) 'string))
(assert (not (typep (make-array 2 :element-type nil) 'base-string)))
(assert (typep (make-string 3 :element-type nil) 'simple-string))
(assert (not (typep (make-string 4 :element-type nil) 'simple-base-string)))

(assert (subtypep (class-of (make-array 1 :element-type nil))
		  (find-class 'string)))
(assert (subtypep (class-of (make-array 2 :element-type nil :fill-pointer 1))
		  (find-class 'string)))

(assert (string= "" (make-array 0 :element-type nil)))
(assert (string/= "a" (make-array 0 :element-type nil)))
(assert (string= "" (make-array 5 :element-type nil :fill-pointer 0)))

(assert (= (sxhash "")
	   (sxhash (make-array 0 :element-type nil))
	   (sxhash (make-array 5 :element-type nil :fill-pointer 0))
	   (sxhash (make-string 0 :element-type nil))))
(assert (subtypep (type-of (make-array 2 :element-type nil)) 'simple-string))
(assert (subtypep (type-of (make-array 4 :element-type nil :fill-pointer t))
		  'string))

(assert (eq (intern "") (intern (make-array 0 :element-type nil))))
(assert (eq (intern "")
	    (intern (make-array 5 :element-type nil :fill-pointer 0))))

(assert (raises-error? (make-string 5 :element-type t)))
(assert (raises-error? (let () (make-string 5 :element-type t))))

;; MISC.574
(assert (= (funcall (lambda (a)
		      (declare (optimize (speed 3) (safety 1)
					 (debug 1) (space 2))
			       (fixnum a))
		      (string<= (coerce "e99mo7yAJ6oU4" 'base-string)
				(coerce "aaABAAbaa" 'base-string)
				:start1 a))
		    9)
	   9))
