;;;; miscellaneous side-effectful tests of CLOS

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

(defpackage "FOO"
  (:use "CL"))
(in-package "FOO")

;;; It should be possible to do DEFGENERIC and DEFMETHOD referring to
;;; structure types defined earlier in the file.
(defstruct struct-a x y)
(defstruct struct-b x y z)
(defmethod wiggle ((a struct-a))
  (+ (struct-a-x a)
     (struct-a-y a)))
(defgeneric jiggle ((arg t)))
(defmethod jiggle ((a struct-a))
  (- (struct-a-x a)
     (struct-a-y a)))
(defmethod jiggle ((b struct-b))
  (- (struct-b-x b)
     (struct-b-y b)
     (struct-b-z b)))
(assert (= (wiggle (make-struct-a :x 6 :y 5))
           (jiggle (make-struct-b :x 19 :y 6 :z 2))))

;;; Compiling DEFGENERIC should prevent "undefined function" style
;;; warnings from code within the same file.
(defgeneric gf-defined-in-this-file ((x number) (y number)))
(defun function-using-gf-defined-in-this-file (x y n)
  (unless (minusp n)
    (gf-defined-in-this-file x y)))

;;; Until Martin Atzmueller ported Pierre Mai's CMU CL fixes in
;;; sbcl-0.6.12.25, the implementation of NO-APPLICABLE-METHOD was
;;; broken in such a way that the code here would signal an error.
(defgeneric zut-n-a-m (a b c))
(defmethod no-applicable-method ((zut-n-a-m (eql #'zut-n-a-m)) &rest args)
  (format t "~&No applicable method for ZUT-N-A-M ~S, yet.~%" args))
(zut-n-a-m 1 2 3)

;;; bug reported and fixed by Alexey Dejneka sbcl-devel 2001-09-10:
;;; This DEFGENERIC shouldn't cause an error.
(defgeneric ad-gf (a) (:method :around (x) x))

;;; DEFGENERIC and DEFMETHOD shouldn't accept &REST when it's not
;;; followed by a variable:
;;; e.g. (DEFMETHOD FOO ((X T) &REST) NIL) should signal an error.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro expect-error (&body body)
    `(multiple-value-bind (res condition)
      (ignore-errors (progn ,@body))
      (declare (ignore res))
      (typep condition 'error))))

(assert (expect-error
         (macroexpand-1
          '(defmethod foo0 ((x t) &rest) nil))))

(assert (expect-error (defgeneric foo1 (x &rest))))
(assert (expect-error (defgeneric foo2 (x a &rest))))

(defgeneric foo3 (x &rest y))
(defmethod foo3 ((x t) &rest y) nil)
(defmethod foo4 ((x t) &key y &rest z) nil)
(defgeneric foo4 (x &key y &rest z))

(assert (expect-error (defgeneric foo5 (x &rest))))
(assert (expect-error (macroexpand-1 '(defmethod foo6 (x &rest)))))

;;; structure-class tests setup
(defclass structure-class-foo1 () () (:metaclass cl:structure-class))
(defclass structure-class-foo2 (structure-class-foo1)
  () (:metaclass cl:structure-class))

;;; standard-class tests setup
(defclass standard-class-foo1 () () (:metaclass cl:standard-class))
(defclass standard-class-foo2 (standard-class-foo1)
  () (:metaclass cl:standard-class))

(assert (typep (class-of (make-instance 'structure-class-foo1))
               'structure-class))
(assert (typep (make-instance 'structure-class-foo1) 'structure-class-foo1))
(assert (typep (make-instance 'standard-class-foo1) 'standard-class-foo1))

;;; DEFGENERIC's blow-away-old-methods behavior is specified to have
;;; special hacks to distinguish between defined-with-DEFGENERIC-:METHOD
;;; methods and defined-with-DEFMETHOD methods, so that reLOADing
;;; DEFGENERIC-containing files does the right thing instead of 
;;; randomly slicing your generic functions. (APD made this work
;;; in sbcl-0.7.0.2.)
(defgeneric born-to-be-redefined (x)
  (:method ((x integer))
    'integer))
(defmethod born-to-be-redefined ((x real))
  'real)
(assert (eq (born-to-be-redefined 1) 'integer))
(defgeneric born-to-be-redefined (x))
(assert (eq (born-to-be-redefined 1) 'real)) ; failed until sbcl-0.7.0.2
(defgeneric born-to-be-redefined (x)
  (:method ((x integer))
    'integer))
(defmethod born-to-be-redefined ((x integer))
  'int)
(assert (eq (born-to-be-redefined 1) 'int))
(defgeneric born-to-be-redefined (x))
(assert (eq (born-to-be-redefined 1) 'int))

;;; In the removal of ITERATE from SB-PCL, a bug was introduced
;;; preventing forward-references and also change-class (which
;;; forward-references used interally) from working properly.  One
;;; symptom was reported by Brian Spilsbury (sbcl-devel 2002-04-08),
;;; and another on IRC by Dan Barlow simultaneously.  Better check
;;; that it doesn't happen again.
;;;
;;; First, the forward references:
(defclass a (b) ())
(defclass b () ())
;;; Then change-class
(defclass class-with-slots ()
  ((a-slot :initarg :a-slot :accessor a-slot)
   (b-slot :initarg :b-slot :accessor b-slot)
   (c-slot :initarg :c-slot :accessor c-slot)))
(let ((foo (make-instance 'class-with-slots
			  :a-slot 1
			  :b-slot 2
			  :c-slot 3)))
  (let ((bar (change-class foo 'class-with-slots)))
    (assert (= (a-slot bar) 1))
    (assert (= (b-slot bar) 2))
    (assert (= (c-slot bar) 3))))

;;; some more CHANGE-CLASS testing, now that we have an ANSI-compliant
;;; version (thanks to Espen Johnsen)
(defclass from-class ()
  ((foo :initarg :foo :accessor foo)))
(defclass to-class ()
  ((foo :initarg :foo :accessor foo)
   (bar :initarg :bar :accessor bar)))
(let* ((from (make-instance 'from-class :foo 1))
       (to (change-class from 'to-class :bar 2)))
  (assert (= (foo to) 1))
  (assert (= (bar to) 2)))

;;; Until Pierre Mai's patch (sbcl-devel 2002-06-18, merged in
;;; sbcl-0.7.4.39) the :MOST-SPECIFIC-LAST option had no effect.
(defgeneric bug180 ((x t))
  (:method-combination list :most-specific-last))
(defmethod bug180 list ((x number))
  'number)
(defmethod bug180 list ((x fixnum))
  'fixnum)
(assert (equal (bug180 14) '(number fixnum)))

;;; printing a structure class should not loop indefinitely (or cause
;;; a stack overflow):
(defclass test-printing-structure-class ()
  ((slot :initarg :slot))
  (:metaclass structure-class))
(print (make-instance 'test-printing-structure-class :slot 2))

;;; structure-classes should behave nicely when subclassed
(defclass super-structure ()
  ((a :initarg :a :accessor a-accessor)
   (b :initform 2 :reader b-reader))
  (:metaclass structure-class))
(defclass sub-structure (super-structure)
  ((c :initarg :c :writer c-writer :accessor c-accessor))
  (:metaclass structure-class))
(let ((foo (make-instance 'sub-structure :a 1 :c 3)))
  (assert (= (a-accessor foo) 1))
  (assert (= (b-reader foo) 2))
  (assert (= (c-accessor foo) 3))
  (setf (a-accessor foo) 4)
  (c-writer 5 foo)
  (assert (= (a-accessor foo) 4))
  (assert (= (c-accessor foo) 5)))

;;; At least as of sbcl-0.7.4, PCL has code to support a special
;;; encoding of effective method functions for slot accessors as
;;; FIXNUMs. Given this special casing, it'd be easy for slot accessor
;;; functions to get broken in special ways even though ordinary
;;; generic functions work. As of sbcl-0.7.4 we didn't have any tests
;;; for that possibility. Now we have a few tests:
(defclass fish ()
  ((fin :reader ffin :writer ffin!)
   (tail :reader ftail :writer ftail!)))
(defvar *fish* (make-instance 'fish))
(ffin! 'triangular-fin *fish*)
(defclass cod (fish) ())
(defvar *cod* (make-instance 'cod))
(defparameter *clos-dispatch-side-fx* (make-array 0 :fill-pointer 0))
(defmethod ffin! (new-fin (cod cod))
  (format t "~&about to set ~S fin to ~S~%" cod new-fin)
  (vector-push-extend '(cod) *clos-dispatch-side-fx*)
  (prog1
      (call-next-method)
    (format t "~&done setting ~S fin to ~S~%" cod new-fin)))
(defmethod ffin! :before (new-fin (cod cod))
  (vector-push-extend '(:before cod) *clos-dispatch-side-fx*)
  (format t "~&exploring the CLOS dispatch zoo with COD fins~%"))
(ffin! 'almost-triang-fin *cod*)
(assert (eq (ffin *cod*) 'almost-triang-fin))
(assert (equalp #((:before cod) (cod)) *clos-dispatch-side-fx*))

;;;; success

(sb-ext:quit :unix-status 104)
