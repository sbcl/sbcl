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

;;;; success

(sb-ext:quit :unix-status 104)
