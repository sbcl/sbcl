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
(defgeneric jiggle (arg))
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
(defgeneric gf-defined-in-this-file (x y))
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
(defgeneric foo4 (x &rest z &key y))
(assert (expect-error (defgeneric foo5 (x &rest))))
(assert (expect-error (macroexpand-1 '(defmethod foo6 (x &rest)))))

;;; more lambda-list checking
;;;
;;; DEFGENERIC lambda lists are subject to various limitations, as per
;;; section 3.4.2 of the ANSI spec. Since Alexey Dejneka's patch for
;;; bug 191-b ca. sbcl-0.7.22, these limitations should be enforced.
(labels ((coerce-to-boolean (x)
	   (if x t nil))
	 (%like-or-dislike (expr expected-failure-p)
           (declare (type boolean expected-failure-p))
           (format t "~&trying ~S~%" expr)
           (multiple-value-bind (fun warnings-p failure-p)
	     (compile nil
		      `(lambda ()
                         ,expr))
	     (declare (ignore fun))
	     ;; In principle the constraint on WARNINGS-P below seems
	     ;; reasonable, but in practice we get warnings about
	     ;; undefined functions from the DEFGENERICs, apparently
	     ;; because the DECLAIMs which ordinarily prevent such
	     ;; warnings don't take effect because EVAL-WHEN
	     ;; (:COMPILE-TOPLEVEL) loses its magic when compiled
	     ;; within a LAMBDA. So maybe we can't test WARNINGS-P
	     ;; after all?
             ;;(unless expected-failure-p
	     ;;  (assert (not warnings-p)))
	     (assert (eq (coerce-to-boolean failure-p) expected-failure-p))))
         (like (expr)
           (%like-or-dislike expr nil))
         (dislike (expr)
           (%like-or-dislike expr t)))
  ;; basic sanity
  (dislike '(defgeneric gf-for-ll-test-0 ("a" #p"b")))
  (like    '(defgeneric gf-for-ll-test-1 ()))
  (like    '(defgeneric gf-for-ll-test-2 (x)))
  ;; forbidden default or supplied-p for &OPTIONAL or &KEY arguments
  (dislike '(defgeneric gf-for-ll-test-3 (x &optional (y 0)))) 
  (like    '(defgeneric gf-for-ll-test-4 (x &optional y))) 
  (dislike '(defgeneric gf-for-ll-test-5 (x y &key (z :z z-p)))) 
  (like    '(defgeneric gf-for-ll-test-6 (x y &key z)))
  (dislike '(defgeneric gf-for-ll-test-7 (x &optional (y 0) &key z))) 
  (like    '(defgeneric gf-for-ll-test-8 (x &optional y &key z))) 
  (dislike '(defgeneric gf-for-ll-test-9 (x &optional y &key (z :z)))) 
  (like    '(defgeneric gf-for-ll-test-10 (x &optional y &key z))) 
  (dislike '(defgeneric gf-for-ll-test-11 (&optional &key (k :k k-p))))
  (like    '(defgeneric gf-for-ll-test-12 (&optional &key k)))
  ;; forbidden &AUX
  (dislike '(defgeneric gf-for-ll-test-13 (x y z &optional a &aux g h)))
  (like    '(defgeneric gf-for-ll-test-14 (x y z &optional a)))
  (dislike '(defgeneric gf-for-ll-test-bare-aux-1 (x &aux)))
  (like    '(defgeneric gf-for-ll-test-bare-aux-2 (x)))
  ;; also can't use bogoDEFMETHODish type-qualifier-ish decorations
  ;; on required arguments
  (dislike '(defgeneric gf-for-11-test-15 ((arg t))))
  (like '(defgeneric gf-for-11-test-16 (arg))))

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
(defgeneric bug180 (x)
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

;;; Until sbcl-0.7.6.21, the long form of DEFINE-METHOD-COMBINATION
;;; ignored its options; Gerd Moellmann found and fixed the problem
;;; for cmucl (cmucl-imp 2002-06-18).
(define-method-combination test-mc (x)
  ;; X above being a method-group-specifier
  ((primary () :required t))
  `(call-method ,(first primary)))

(defgeneric gf (obj)
  (:method-combination test-mc 1))

(defmethod gf (obj)
  obj)

;;; Until sbcl-0.7.7.20, some conditions weren't being signalled, and
;;; some others were of the wrong type:
(macrolet ((assert-program-error (form)
	     `(multiple-value-bind (value error)
	          (ignore-errors ,form)
	        (assert (null value))
	        (assert (typep error 'program-error)))))
  (assert-program-error (defclass foo001 () (a b a)))
  (assert-program-error (defclass foo002 () 
			  (a b) 
			  (:default-initargs x 'a x 'b)))
  (assert-program-error (defclass foo003 ()
			  ((a :allocation :class :allocation :class))))
  (assert-program-error (defclass foo004 ()
			  ((a :silly t))))
  ;; and some more, found by Wolfhard Buss and fixed for cmucl by Gerd
  ;; Moellmann in 0.7.8.x:
  (assert-program-error (progn
			  (defmethod odd-key-args-checking (&key (key 42)) key)
			  (odd-key-args-checking 3)))
  (assert (= (odd-key-args-checking) 42))
  (assert (eq (odd-key-args-checking :key t) t)))

;;; DOCUMENTATION's argument-precedence-order wasn't being faithfully
;;; preserved through the bootstrap process until sbcl-0.7.8.39.
;;; (thanks to Gerd Moellmann)
(let ((answer (documentation '+ 'function)))
  (assert (stringp answer))
  (defmethod documentation ((x (eql '+)) y) "WRONG")
  (assert (string= (documentation '+ 'function) answer)))

;;; only certain declarations are permitted in DEFGENERIC
(macrolet ((assert-program-error (form)
	     `(multiple-value-bind (value error)
	          (ignore-errors ,form)
	        (assert (null value))
	        (assert (typep error 'program-error)))))
  (assert-program-error (defgeneric bogus-declaration (x)
			  (declare (special y))))
  (assert-program-error (defgeneric bogus-declaration2 (x)
			  (declare (notinline concatenate)))))
;;; CALL-NEXT-METHOD should call NO-NEXT-METHOD if there is no next
;;; method.
(defmethod no-next-method-test ((x integer)) (call-next-method))
(assert (null (ignore-errors (no-next-method-test 1))))
(defmethod no-next-method ((g (eql #'no-next-method-test)) m &rest args)
  'success)
(assert (eq (no-next-method-test 1) 'success))
(assert (null (ignore-errors (no-next-method-test 'foo))))

;;; regression test for bug 176, following a fix that seems
;;; simultaneously to fix 140 while not exposing 176 (by Gerd
;;; Moellmann, merged in sbcl-0.7.9.12).
(dotimes (i 10)
  (let ((lastname (intern (format nil "C176-~D" (1- i))))
        (name (intern (format nil "C176-~D" i))))
  (eval `(defclass ,name
             (,@(if (= i 0) nil (list lastname)))
           ()))
  (eval `(defmethod initialize-instance :after ((x ,name) &rest any)
           (declare (ignore any))))))
(defclass b176 () (aslot-176))
(defclass c176-0 (b176) ())
(assert (= 1 (setf (slot-value (make-instance 'c176-9) 'aslot-176) 1)))

;;; DEFINE-METHOD-COMBINATION was over-eager at checking for duplicate
;;; primary methods:
(define-method-combination dmc-test-mc (&optional (order :most-specific-first))
  ((around (:around))
   (primary (dmc-test-mc) :order order :required t))
   (let ((form (if (rest primary)
                   `(and ,@(mapcar #'(lambda (method)
                                       `(call-method ,method))
                                   primary))
                   `(call-method ,(first primary)))))
     (if around
         `(call-method ,(first around)
                       (,@(rest around)
                        (make-method ,form)))
         form)))

(defgeneric dmc-test-mc (&key k)
  (:method-combination dmc-test-mc))

(defmethod dmc-test-mc dmc-test-mc (&key k)
	   k)

(dmc-test-mc :k 1)
;;; While I'm at it, DEFINE-METHOD-COMBINATION is defined to return
;;; the NAME argument, not some random method object. So:
(assert (eq (define-method-combination dmc-test-return-foo)
	    'dmc-test-return-foo))
(assert (eq (define-method-combination dmc-test-return-bar :operator and)
	    'dmc-test-return-bar))
(assert (eq (define-method-combination dmc-test-return
		(&optional (order :most-specific-first))
	      ((around (:around))
	       (primary (dmc-test-return) :order order :required t))
	      (let ((form (if (rest primary)
			      `(and ,@(mapcar #'(lambda (method)
						  `(call-method ,method))
					      primary))
			      `(call-method ,(first primary)))))
		(if around
		    `(call-method ,(first around)
		      (,@(rest around)
		       (make-method ,form)))
		    form)))
	    'dmc-test-return))

;;; DEFMETHOD should signal a PROGRAM-ERROR if an incompatible lambda
;;; list is given:
(defmethod incompatible-ll-test-1 (x) x)
(multiple-value-bind (result error)
    (ignore-errors (defmethod incompatible-ll-test-1 (x y) y))
  (assert (null result))
  (assert (typep error 'program-error)))
(multiple-value-bind (result error)
    (ignore-errors (defmethod incompatible-ll-test-1 (x &rest y) y))
  (assert (null result))
  (assert (typep error 'program-error)))
;;; Sneakily using a bit of MOPness to check some consistency
(assert (= (length
	    (sb-pcl:generic-function-methods #'incompatible-ll-test-1)) 1))

(defmethod incompatible-ll-test-2 (x &key bar) bar)
(multiple-value-bind (result error)
    (ignore-errors (defmethod incompatible-ll-test-2 (x) x))
  (assert (null result))
  (assert (typep error 'program-error)))
(defmethod incompatible-ll-test-2 (x &rest y) y)
(assert (= (length
	    (sb-pcl:generic-function-methods #'incompatible-ll-test-2)) 1))
(defmethod incompatible-ll-test-2 ((x integer) &key bar) bar)
(assert (= (length
	    (sb-pcl:generic-function-methods #'incompatible-ll-test-2)) 2))
(assert (equal (incompatible-ll-test-2 t 1 2) '(1 2)))
(assert (eq (incompatible-ll-test-2 1 :bar 'yes) 'yes))

;;; Attempting to instantiate classes with forward references in their
;;; CPL should signal errors (FIXME: of what type?)
(defclass never-finished-class (this-one-unfinished-too) ())
(multiple-value-bind (result error)
    (ignore-errors (make-instance 'never-finished-class))
  (assert (null result))
  (assert (typep error 'error)))
(multiple-value-bind (result error)
    (ignore-errors (make-instance 'this-one-unfinished-too))
  (assert (null result))
  (assert (typep error 'error)))

;;;; success

(sb-ext:quit :unix-status 104)
