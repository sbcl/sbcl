;;;; miscellaneous compiler tests with side effects (e.g. DEFUN
;;;; changing FDEFINITIONs and globaldb stuff)

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

(cl:in-package :cl-user)

(declaim (optimize (debug 3) (speed 2) (space 1)))

;;; Until version 0.6.9 or so, SBCL's version of Python couldn't do
;;; this correctly, due to the bug patched by Rob MacLachlan on the
;;; cmucl-imp list 2000-06-21, and applied to SBCL by Martin Atzmueller.
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

;;; An early version (sbcl-0.6.11.33) of code to check FTYPEs from DEFUN
;;; against DECLAIMed FTYPEs blew up when an FTYPE was DECLAIMed
;;; to be pure FUNCTION, because the internal representation of
;;; FUNCTION itself (as opposed to subtypes of FUNCTION, such as
;;; (FUNCTION () T)) is a BUILT-IN-CLASS object, not a FUN-TYPE
;;; object.
(declaim (ftype function i-am-just-a-function))
(defun i-am-just-a-function (x y) (+ x y 1))

;;; Stig E Sandoe reported in cclan-Bugs-431263 that SBCL couldn't
;;; compile this. sbcl-0.6.12.26 died in CIRCULAR-LIST-P with "The
;;; value \"EST\" is not of type LIST." Dan Barlow fixed it.
(defvar +time-zones+
  '((5 "EDT" . "EST") (6 "CDT" . "CST") (7 "MDT" .
"MST") (8 "PDT" . "PST")
    (0 "GMT" . "GDT") (-2 "MET" . "MET DST"))
  "*The string representations of the time zones.")

;;; The old CMU CL Python compiler assumed that it was safe to infer
;;; function types (including return types) from function definitions
;;; and then use them to optimize code later. This is of course bad
;;; when functions are redefined. The problem was fixed in
;;; sbcl-0.6.12.57.
(defun foo (x)
  (if (plusp x)
      1.0
      0))
(defun bar (x)
  (typecase (foo x)
    (fixnum :fixnum)
    (real :real)
    (string :string)
    (t :t)))
(assert (eql (bar 11) :real))
(assert (eql (bar -11) :fixnum))
(setf (symbol-function 'foo) #'identity)
(assert (eql (bar 11) :fixnum))
(assert (eql (bar -11.0) :real))
(assert (eql (bar "this is a test") :string))
(assert (eql (bar (make-hash-table)) :t))

;;; bug reported by Brian Spilsbury sbcl-devel 2001-09-30, fixed by
;;; Alexey Dejneka patch sbcl-devel 2001-10-02
(defun pixarray-element-size (pixarray)
  (let ((eltype (array-element-type pixarray)))
    (cond ((eq eltype 'bit) 1)
          ((and (listp eltype)
                (eq (first eltype) 'unsigned-byte))
           (second eltype))
          (t
           (error "Invalid pixarray: ~S." pixarray)))))
(assert (eql 1 (pixarray-element-size #*110)))

;;; bug 31 turned out to be a manifestation of non-ANSI array type
;;; handling, fixed by CSR in sbcl-0.7.3.8.
(defun array-element-type-handling (x)
  (declare (type (vector cons) x))
  (when (consp (aref x 0))
    (aref x 0)))
(assert (eq (array-element-type-handling
	     (make-array 3 :element-type t :initial-element 0))
	    nil))

;;; bug 220: type check inserted after all arguments in MV-CALL caused
;;; failure of stack analysis
(defun bug220-helper ()
  13)
(assert (equal (multiple-value-call #'list
                 (the integer (bug220-helper))
                 nil)
               '(13 nil)))

(sb-ext:quit :unix-status 104) ; success

