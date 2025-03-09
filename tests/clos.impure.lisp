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

#+interpreter (invoke-restart 'run-tests::skip-file)

(load "compiler-test-util.lisp")
(defpackage "CLOS-IMPURE"
  (:use "CL" "SB-EXT" "ASSERTOID" "TEST-UTIL" "COMPILER-TEST-UTIL"))
(in-package "CLOS-IMPURE")

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

(with-test (:name (defmethod defstruct :same-file))
  (assert (= (wiggle (make-struct-a :x 6 :y 5))
             (jiggle (make-struct-b :x 19 :y 6 :z 2)))))

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
(defmethod zut-n-a-m :around (a b (c symbol)) nil)
(defmethod no-applicable-method ((zut-n-a-m (eql #'zut-n-a-m)) &rest args)
  (declare (ignore args))
  :no-applicable-method)
(defmethod sb-pcl:no-primary-method ((zut-n-a-m (eql #'zut-n-a-m)) &rest args)
  (declare (ignore args))
  :no-primary-method)
(with-test (:name no-applicable-method)
  (assert (eq :no-applicable-method (zut-n-a-m 1 2 3))))
(with-test (:name :no-primary-method)
  (assert (eq :no-primary-method (zut-n-a-m 1 2 t))))

;;; bug reported and fixed by Alexey Dejneka sbcl-devel 2001-09-10:
;;; This DEFGENERIC shouldn't cause an error.
(defgeneric ad-gf (a) (:method :around (x) x))

;;; DEFGENERIC and DEFMETHOD shouldn't accept &REST when it's not
;;; followed by a variable:
;;; e.g. (DEFMETHOD FOO ((X T) &REST) NIL) should signal an error.
;;; Of course most of these are totally redundant
;;; now that there is only one function to parse all lambda lists.
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmacro expect-error (form)
    `(multiple-value-bind (res condition)
      (let ((*error-output* (make-broadcast-stream)))
        (ignore-errors (eval ',form))) ; delay until *error-output* is bound
      (declare (ignore res))
      (typep condition 'error))))
(assert (expect-error (defmethod foo0 ((x t) &rest) nil)))
(assert (expect-error (defgeneric foo1 (x &rest))))
(assert (expect-error (defgeneric foo2 (x a &rest))))
(defgeneric foo3 (x &rest y))
(defmethod foo3 ((x t) &rest y) y nil)
(defgeneric foo4 (x &rest z &key y))
(defmethod foo4 ((x t) &rest z &key y) z y nil)
(assert (expect-error (defgeneric foo5 (x &rest))))
(assert (expect-error (defmethod foo6 (x &rest))))

;;; legal method specializers
(defclass bug-525916-1 () ())
(defclass bug-525916-2 () ())
(with-test (:name (defmethod :specializer-syntax :bug-525916))
  (assert (expect-error (defmethod invalid ((arg)) arg)))
  (assert (expect-error (defmethod invalid (nil) 1)))
  (assert (expect-error (defmethod invalid ((arg . bug-525916-1)) arg)))
  (assert (expect-error (defmethod invalid ((arg bug-525916-1 bug-525916-2)) arg))))

;;; more lambda-list checking
;;;
;;; DEFGENERIC lambda lists are subject to various limitations, as per
;;; section 3.4.2 of the ANSI spec. Since Alexey Dejneka's patch for
;;; bug 191-b ca. sbcl-0.7.22, these limitations should be enforced.
(flet ((test-case (operator lambda-list
                   &optional expect-failure-p expect-warnings-p &rest messages)
         (multiple-value-bind
               (fun failure-p warnings style-warnings notes errors)
             (checked-compile `(lambda () (,operator ,(gensym) ,lambda-list))
                              :allow-failure expect-failure-p
                              :allow-warnings expect-warnings-p
                              :allow-style-warnings expect-warnings-p)
           (declare (ignore fun notes))
           (let ((warnings (append style-warnings warnings)))
             (when expect-failure-p (assert failure-p))
             (when expect-warnings-p (assert warnings))
             (assert (= (length messages) (+ (length warnings) (length errors))))
             (loop for message in messages
                for error in (append warnings errors)
                do (assert (search message (princ-to-string error))))))))

  (with-test (:name (defgeneric :lambda-list))
    (mapc
     (lambda (spec) (apply #'test-case 'defgeneric spec))
     '(;; basic sanity
       (("a" #p"b")
        t nil "Required argument is not a symbol: \"a\"")
       (())
       ((x))
       ;; repeated names and keywords
       ((x x)
        t nil "The variable X occurs more than once")
       ((x &rest x)
        t nil "The variable X occurs more than once")
       ((&optional x x)
        t nil "The variable X occurs more than once")
       ((&key x ((:y x)))
        t nil "The variable X occurs more than once")
       ((&key x ((:x y)))
        t nil "The keyword :X occurs more than once")
       ((&key ((:x a)) ((:x b)))
        t nil "The keyword :X occurs more than once")
       ;; illegal variable names
       ((nil)
        t nil "NIL cannot be used")
       ((:pi)
        t nil ":PI is a keyword and cannot be used")
       ((pi)
        t nil "COMMON-LISP:PI names a defined constant")
       ;; forbidden default or supplied-p for &OPTIONAL or &KEY arguments
       ((x &optional (y 0))
        t nil "Invalid &OPTIONAL argument specifier (Y 0)")
       ((x &optional y))
       ((x y &key (z :z z-p))
        t nil "Invalid &KEY argument specifier (Z :Z Z-P)")
       ((x y &key z))
       ((x &optional (y 0) &key z)
        t t
        "&OPTIONAL and &KEY found in the same lambda list"
        "Invalid &OPTIONAL argument specifier (Y 0)")
       ((x &optional y &key z)
        nil t "&OPTIONAL and &KEY found in the same lambda list")
       ((x &optional y &key (z :z))
        t t
        "&OPTIONAL and &KEY found in the same lambda list"
        "Invalid &KEY argument specifier (Z :Z)")
       ((&optional &key (k :k k-p))
        t nil "Invalid &KEY argument specifier (K :K K-P)")
       ((&optional &key k))
       ;; forbidden &AUX
       ((x y z &optional a &aux g h)
        t nil "&AUX is not allowed in a generic function lambda list")
       ((x y z &optional a))
       ((x &aux)
        t nil "&AUX is not allowed in a generic function lambda list")
       ;; also can't use bogoDEFMETHODish type-qualifier-ish decorations
       ;; on required arguments
       (((arg t))
        t nil "Required argument is not a symbol: (ARG T)"))))

  (with-test (:name (defmethod :lambda-list))
    (mapc
     (lambda (spec) (apply #'test-case 'defmethod spec))
     '(;; Invalid specialized required argument
       (((x t t))
        t nil "arg is not a non-NIL symbol or a list of two elements: (X T T)")
       ;; Repeated names and keywords
       (((x t) (x t))
        t nil "The variable X occurs more than once")
       (((x t) &rest x)
        t nil "The variable X occurs more than once")
       ((&optional x x)
        t nil "The variable X occurs more than once")
       ((&key x ((:y x)))
        t nil "The variable X occurs more than once")
       ((&key x ((:x y)))
        t nil "The keyword :X occurs more than once")
       ;; Illegal variable names
       (((nil t))
        t nil "NIL cannot be used")
       (((:pi t))
        t nil ":PI is a keyword and cannot be used")
       (((pi t))
        t nil "COMMON-LISP:PI names a defined constant")))))


;;; Explicit :metaclass option with structure-class and
;;; standard-class.

(defclass structure-class-foo1 () () (:metaclass cl:structure-class))
(defclass structure-class-foo2 (structure-class-foo1)
  () (:metaclass cl:structure-class))
(with-test (:name (defclass :metaclass cl:structure-class))
  (assert (typep (class-of (make-instance 'structure-class-foo1))
                 'structure-class))
  (assert (typep (make-instance 'structure-class-foo1) 'structure-class-foo1))
  (assert (typep (class-of (make-instance 'structure-class-foo2))
                 'structure-class))
  (assert (typep (make-instance 'structure-class-foo2) 'structure-class-foo2)))

(defclass standard-class-foo1 () () (:metaclass cl:standard-class))
(defclass standard-class-foo2 (standard-class-foo1)
  () (:metaclass cl:standard-class))
(with-test (:name (defclass :metaclass cl:standard-class))
  (assert (typep (make-instance 'standard-class-foo1) 'standard-class-foo1))
  (assert (typep (make-instance 'standard-class-foo2) 'standard-class-foo2)))

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
;;; preventing forward-references and also CHANGE-CLASS (which
;;; forward-references used interally) from working properly.
;;;
;;; One symptom was reported by Brian Spilsbury (sbcl-devel
;;; 2002-04-08), and another on IRC by Dan Barlow simultaneously.
;;; Better check that it doesn't happen again.
;;;
;;; Some more CHANGE-CLASS testing (which only applies to the now
;;; ANSI-compliant version) thanks to Espen Johnsen)

;; First, the forward references:
(defclass forward-ref-a (forward-ref-b) ())
(defclass forward-ref-b () ())
;; (a couple more complicated examples found by Paul Dietz' test
;; suite):
(defclass forward-ref-c1 (forward-ref-c2) ())
(defclass forward-ref-c2 (forward-ref-c3) ())

(defclass forward-ref-d1 (forward-ref-d2 forward-ref-d3)
  ())
(defclass forward-ref-d2 (forward-ref-d4 forward-ref-d5)
  ())

;; Then CHANGE-CLASS
(defun change-class-test-case (spec)
  (destructuring-bind (from-class from-initargs
                       to-class   to-initargs
                       expected-slots)
      spec
    (let ((from (typecase from-class
                  (symbol
                   (apply #'make-instance from-class from-initargs))
                  ((cons (eql :class) (cons symbol))
                   (find-class (second from-class))))))
      (flet ((change ()
               (apply #'change-class from to-class to-initargs))
             ;; These local functions make ASSERT produce better error
             ;; messages.
             (slot-value-equal (instance name value expected)
               (declare (ignore instance name))
               (equal value expected))
             (slot-not-bound (instance name)
               (not (slot-boundp instance name))))
        (case expected-slots
          (error
           (assert-error (change)))
          (type-error
           (assert-error (change) type-error))
          (t
           (let ((to (change)))
             (loop :for (name value) :in expected-slots
                :do (case value
                      (:unbound
                       (assert (slot-not-bound to name)))
                      (t
                       (assert
                        (slot-value-equal
                         to name (slot-value to name) value))))))))))))

(defclass change-class.smoke.1 ()
  ((foo :initarg :foo)))
(defclass change-class.smoke.2 (change-class.smoke.1) ())
(defclass change-class.smoke.3 (change-class.smoke.1)
  ((foo :initarg :foo)))
(defclass change-class.smoke.4 ()
  ((foo :initarg :foo) (bar :initarg :bar)))
(defclass change-class.smoke.5 ()
  ((a :initarg :a) (b :initarg :b) (c :initarg :c)))
(defclass change-class.smoke.6 () ())

(with-test (:name (change-class :smoke))
  (mapc
   #'change-class-test-case
   '(;; Test unbound slots.
     (change-class.smoke.1 ()               change-class.smoke.1 ()
      ((foo :unbound)))
     (change-class.smoke.1 ()               change-class.smoke.2 ()
      ((foo :unbound)))
     (change-class.smoke.1 ()               change-class.smoke.3 ()
      ((foo :unbound)))
     (change-class.smoke.1 ()               change-class.smoke.4 ()
      ((foo :unbound) (bar :unbound)))
     (change-class.smoke.4 (:bar 1)         change-class.smoke.1 ()
      ((foo :unbound)))

     ;; Bound slots are retained.
     (change-class.smoke.1 (:foo :baz)      change-class.smoke.1 ()
      ((foo :baz)))
     (change-class.smoke.1 (:foo :baz)      change-class.smoke.2 ()
      ((foo :baz)))
     (change-class.smoke.1 (:foo :baz)      change-class.smoke.3 ()
      ((foo :baz)))
     (change-class.smoke.1 (:foo :baz)      change-class.smoke.4 ()
      ((foo :baz) (bar :unbound)))
     (change-class.smoke.4 (:foo :baz)      change-class.smoke.1 ()
      ((foo :baz)))

     ;; Original test.
     (change-class.smoke.5 (:a 1 :b 2 :c 3) change-class.smoke.5 ()
      ((a 1) (b 2) (c 3)))

     ;; Original test by Espen Johnsen
     (change-class.smoke.1 (:foo 1)         change-class.smoke.4 (:bar 2)
      ((foo 1) (bar 2)))

     ;; Cannot change objects into metaobjects.
     (change-class.smoke.6 ()               class                ()
      error)
     (change-class.smoke.6 ()               generic-function     ()
      error)
     (change-class.smoke.6 ()               method               ()
      error)
     (change-class.smoke.6 ()               slot-definition      ()
      error))))

;; Test for type-checking

(locally (declare (optimize (safety 3))) ; force slot type-checking
  (defclass change-class.type-check.1 ()
    ((foo :initarg :foo :type real)))
  (defclass change-class.type-check.2 ()
    ((foo :initarg :foo :type integer))))

(with-test (:name (change-class :type-check))
  (mapc
   #'change-class-test-case
   '(;; These are allowed.
     (change-class.type-check.1 ()         change-class.type-check.2 ()
      ((foo :unbound)))
     (change-class.type-check.1 (:foo 1)   change-class.type-check.2 ()
      ((foo 1)))
     (change-class.type-check.1 (:foo 1.0) change-class.type-check.2 (:foo 2)
      ((foo 2)))

     ;; These are not allowed.
     (change-class.type-check.1 ()         change-class.type-check.2 (:foo 1.0)
      type-error)
     (change-class.type-check.1 (:foo 1.0) change-class.type-check.2 ()
      type-error)))

  ;; Type-mismatches should be recoverable via USE-VALUE restart.
  (let* ((from (make-instance 'change-class.type-check.1 :foo 1.0))
         (to (handler-bind ((type-error (lambda (condition)
                                          (declare (ignore condition))
                                          (use-value 3))))
               (change-class from 'change-class.type-check.2))))
    (assert (equal (slot-value to 'foo) 3))))

;; Test interaction with initforms and -args

(defclass change-class.initforms.1 ()
  ())
(defclass change-class.initforms.2 ()
  ((foo :initarg :foo)))
(defclass change-class.initforms.3 ()
  ((foo :initarg :foo :initform :bar)))
(defclass change-class.initforms.4 ()
  ((foo :initarg :foo))
  (:default-initargs
   :foo :bar))

(with-test (:name (change-class :initforms))
  (mapc
   #'change-class-test-case
   '(;; Initialization of added slot.
     (change-class.initforms.1 ()          change-class.initforms.3 ()
      ((foo :bar)))
     (change-class.initforms.1 ()          change-class.initforms.3 (:foo :fez)
      ((foo :fez)))
     (change-class.initforms.1 ()          change-class.initforms.4 ()
      ((foo :unbound))) ; default initargs are not used
     (change-class.initforms.1 ()          change-class.initforms.4 (:foo :fez)
      ((foo :fez)))

     ;; Unbound slot remains unbound.
     (change-class.initforms.2 ()          change-class.initforms.3 ()
      ((foo :unbound)))
     (change-class.initforms.2 ()          change-class.initforms.3 (:foo :fez)
      ((foo :fez)))
     (change-class.initforms.2 ()          change-class.initforms.4 ()
      ((foo :unbound)))
     (change-class.initforms.2 ()          change-class.initforms.4 (:foo :fez)
      ((foo :fez)))

     ;; Value is retained.
     (change-class.initforms.2 (:foo :baz) change-class.initforms.3 ()
      ((foo :baz)))
     (change-class.initforms.2 (:foo :baz) change-class.initforms.3 (:foo :fez)
      ((foo :fez)))
     (change-class.initforms.2 (:foo :baz) change-class.initforms.4 ()
      ((foo :baz)))
     (change-class.initforms.2 (:foo :baz) change-class.initforms.4 (:foo :fez)
      ((foo :fez))))))

;; Test for FORWARD-REFERENCED-CLASS

(defclass change-class.forward-referenced.1 () ())
;; CHANGE-CLASS.FORWARD-REFERENCED.2 is only needed to create the
;; FORWARD-REFERENCED-CLASS CHANGE-CLASS.FORWARD-REFERENCED.3.
(defclass change-class.forward-referenced.2 (change-class.forward-referenced.3) ())

(with-test (:name (change-class sb-mop:forward-referenced-class))
  (mapc
   #'change-class-test-case
   '(;; Changing instances of "ordinary classes" to classes which are
     ;; instances of FORWARD-REFERENCED-CLASS is not allowed.
     (change-class.forward-referenced.1          () change-class.forward-referenced.3 ()
      error)

     ;; Changing instances of FORWARD-REFERENCED-CLASS into
     ;; non-CLASSes and in particular non-CLASS metaobjects is not
     ;; allowed.
     ((:class change-class.forward-referenced.3) () change-class.forward-referenced.1 ()
      error)
     ((:class change-class.forward-referenced.3) () generic-function                  ()
      error)
     ((:class change-class.forward-referenced.3) () method                            ()
      error)
     ((:class change-class.forward-referenced.3) () slot-definition                   ()
      error)

     ;; Changing instances of FORWARD-REFERENCED-CLASS into CLASS is
     ;; allowed but destructive. Therefore has to be final test case.
     ((:class change-class.forward-referenced.3) () standard-class                    ()
      ()))))

;; Test for FUNCALLABLE-STANDARD-CLASS

(defclass change-class.funcallable.1 () ())
(defclass change-class.funcallable.2 () ()
  (:metaclass sb-mop:funcallable-standard-class))
(defclass change-class.funcallable.3 () ()
  (:metaclass sb-mop:funcallable-standard-class))

(with-test (:name (change-class sb-mop:funcallable-standard-class))
  (mapc
   #'change-class-test-case
   '(;; Cannot change STANDARD-OBJECT into FUNCALLABLE-STANDARD-OBJECT
     ;; and vice-versa.
     (change-class.funcallable.1 () change-class.funcallable.2 ()
      error)
     (change-class.funcallable.2 () change-class.funcallable.1 ()
      error)
     ;; FUNCALLABLE-STANDARD-OBJECTs should work.
     (change-class.funcallable.2 () change-class.funcallable.2 ()
      ())
     (change-class.funcallable.2 () change-class.funcallable.3 ()
      ()))))

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
(with-test (:name :test-printing-structure-class)
  (write-to-string (make-instance 'test-printing-structure-class :slot 2)))

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
  ; (format t "~&about to set ~S fin to ~S~%" cod new-fin)
  (vector-push-extend '(cod) *clos-dispatch-side-fx*)
  (prog1
      (call-next-method)
    ; (format t "~&done setting ~S fin to ~S~%" cod new-fin)
    ))
(defmethod ffin! :before (new-fin (cod cod))
  (vector-push-extend '(:before cod) *clos-dispatch-side-fx*)
  ;(format t "~&exploring the CLOS dispatch zoo with COD fins~%")
  )
(ffin! 'almost-triang-fin *cod*)
(assert (eq (ffin *cod*) 'almost-triang-fin))
(assert (equalp #((:before cod) (cod)) *clos-dispatch-side-fx*))

;;; Until sbcl-0.7.6.21, the long form of DEFINE-METHOD-COMBINATION
;;; ignored its options; Gerd Moellmann found and fixed the problem
;;; for cmucl (cmucl-imp 2002-06-18).
(define-method-combination test-mc (x)
  ;; X above being a method-group-specifier
  ((primary () :required t))
  x
  `(call-method ,(first primary)))

(defgeneric gf (obj)
  (:method-combination test-mc 1))

(defmethod gf (obj)
  obj)

;;; Until sbcl-0.7.7.20, some conditions weren't being signalled, and
;;; some others were of the wrong type:
(macrolet ((assert-program-error (form)
             `(multiple-value-bind (value error)
                  (ignore-errors (let ((*error-output* (make-broadcast-stream)))
                                   (eval ',form)))
                (unless (and (null value) (typep error 'program-error))
                  (error "~S failed: ~S, ~S" ',form value error)))))
  (assert-program-error (defclass foo001 () (a b a)))
  (assert-program-error (defclass foo002 ()
                          (a b)
                          (:default-initargs x 'a x 'b)))
  (assert-program-error (defclass foo003 ()
                          ((a :allocation :class :allocation :class))))
  (assert-program-error (defclass foo004 ()
                          ((a :silly t))))
  ;; and some more, found by Wolfhard Buss and fixed for cmucl by Gerd
  ;; Moellmann in sbcl-0.7.8.x:
  (assert-program-error (progn
                          (defmethod odd-key-args-checking (&key (key 42)) key)
                          (odd-key-args-checking 3)))
  (assert (= (odd-key-args-checking) 42))
  (assert (eq (odd-key-args-checking :key t) t))
  ;; yet some more, fixed in sbcl-0.7.9.xx
  (assert-program-error (defclass foo005 ()
                          (:metaclass sb-pcl::funcallable-standard-class)
                          (:metaclass 1)))
  (assert-program-error (defclass foo006 ()
                          ((a :reader (setf a)))))
  (assert-program-error (defclass foo007 ()
                          ((a :initarg 1))))
  (assert-program-error (defclass foo008 ()
                          (a :initarg :a)
                          (:default-initargs :a 1)
                          (:default-initargs :a 2)))
  ;; and also BUG 47d, fixed in sbcl-0.8alpha.0.26
  (assert-program-error (defgeneric if (x)))
  ;; DEFCLASS should detect an error if slot names aren't suitable as
  ;; variable names:
  (assert-program-error (defclass foo009 ()
                          ((:a :initarg :a))))
  (assert-program-error (defclass foo010 ()
                          (("a" :initarg :a))))
  (assert-program-error (defclass foo011 ()
                          ((#1a() :initarg :a))))
  (assert-program-error (defclass foo012 ()
                          ((t :initarg :t))))
  (assert-program-error (defclass foo013 () ("a")))
  ;; specialized lambda lists have certain restrictions on ordering,
  ;; repeating keywords, and the like:
  (assert-program-error (defmethod foo014 ((foo t) &rest) nil))
  (assert-program-error (defmethod foo015 ((foo t) &rest x y) nil))
  (assert-program-error (defmethod foo016 ((foo t) &allow-other-keys) nil))
  (assert-program-error (defmethod foo017 ((foo t)
                                           &optional x &optional y) nil))
  (assert-program-error (defmethod foo018 ((foo t) &rest x &rest y) nil))
  (assert-program-error (defmethod foo019 ((foo t) &rest x &optional y) nil))
  (assert-program-error (defmethod foo020 ((foo t) &key x &optional y) nil))
  (assert-program-error (defmethod foo021 ((foo t) &key x &rest y) nil)))

;;; DOCUMENTATION's argument-precedence-order wasn't being faithfully
;;; preserved through the bootstrap process until sbcl-0.7.8.39.
;;; (thanks to Gerd Moellmann)
(with-test (:name (documentation :argument-precedence-order))
  (defun foo022 ()
    "Documentation"
    t)
  (let ((answer (documentation 'foo022 'function)))
    (assert (stringp answer))
    (defmethod documentation ((x (eql 'foo022)) y) "WRONG")
    (assert (string= (documentation 'foo022 'function) answer))))

;;; only certain declarations are permitted in DEFGENERIC
(macrolet ((assert-program-error (form)
             `(multiple-value-bind (value error)
                  (ignore-errors (let ((*error-output* (make-broadcast-stream)))
                                   (eval ',form)))
                (assert (null value))
                (assert (typep error 'program-error)))))
  (assert-program-error (defgeneric bogus-declaration (x)
                          (declare (special y))))
  (assert-program-error (defgeneric bogus-declaration2 (x)
                          (declare (notinline concatenate)))))
;;; CALL-NEXT-METHOD should call NO-NEXT-METHOD if there is no next
;;; method.
(defmethod no-next-method-test ((x integer)) (call-next-method))
(assert-error (no-next-method-test 1) sb-pcl::no-next-method-error)
(defmethod no-next-method ((g (eql #'no-next-method-test)) m &rest args)
  (declare (ignore args))
  'success)
(assert (eq (no-next-method-test 1) 'success))
(assert-error (no-next-method-test 'foo) sb-pcl::no-applicable-method-error)

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

;;; DEFINE-METHOD-COMBINATION should, according to the description in 7.7,
;;; allow you to do everything in the body forms yourself if you specify
;;; exactly one method group whose qualifier-pattern is *
;;;
;;; The specific language is:
;;; "The use of method group specifiers provides a convenient syntax to select
;;; methods, to divide them among the possible roles, and to perform the
;;; necessary error checking. It is possible to perform further filtering of
;;; methods in the body forms by using normal list-processing operations and
;;; the functions method-qualifiers and invalid-method-error. It is permissible
;;; to use setq on the variables named in the method group specifiers and to
;;; bind additional variables. It is also possible to bypass the method group
;;; specifier mechanism and do everything in the body forms. This is
;;; accomplished by writing a single method group with * as its only
;;; qualifier-pattern; the variable is then bound to a list of all of the
;;; applicable methods, in most-specific-first order."
(define-method-combination wam-test-method-combination-a ()
  ((all-methods *))
  (do ((methods all-methods (rest methods))
       (primary nil)
       (around nil))
      ((null methods)
       (let ((primary (nreverse primary))
             (around (nreverse around)))
         (if primary
              (let ((form (if (rest primary)
                             `(call-method ,(first primary) ,(rest primary))
                             `(call-method ,(first primary)))))
                (if around
                    `(call-method ,(first around) (,@(rest around)
                                                   (make-method ,form)))
                    form))
              `(make-method (error "No primary methods")))))
    (let* ((method (first methods))
           (qualifier (first (method-qualifiers method))))
      (cond
        ((equal :around qualifier)
         (push method around))
        ((null qualifier)
         (push method primary))))))

(defgeneric wam-test-mc-a (val)
  (:method-combination wam-test-method-combination-a))
(assert-error (wam-test-mc-a 13))
(defmethod wam-test-mc-a ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))
(assert (= (wam-test-mc-a 13) 13))
(defmethod wam-test-mc-a :around ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))
(assert (= (wam-test-mc-a 13) 26))

;;; DEFINE-METHOD-COMBINATION
;;; When two methods are in the same method group and have the same
;;; specializers, their sort order within the group may be ambiguous. Therefore,
;;; we should throw an error when we have two methods in the same group with
;;; the same specializers /as long as/ we have more than one method group
;;; or our single method group qualifier-pattern is not *. This resolves the
;;; apparent conflict with the above 'It is also possible to bypass' language.
;;;
;;; The language specifying this behavior is:
;;; "Note that two methods with identical specializers, but with different
;;; qualifiers, are not ordered by the algorithm described in Step 2 of the
;;; method selection and combination process described in Section 7.6.6
;;; (Method Selection and Combination). Normally the two methods play different
;;; roles in the effective method because they have different qualifiers, and
;;; no matter how they are ordered in the result of Step 2, the effective
;;; method is the same. If the two methods play the same role and their order
;;; matters, an error is signaled. This happens as part of the qualifier
;;; pattern matching in define-method-combination."
;;;
;;; Note that the spec pretty much equates 'method group' and 'role'.
;; First we ensure that it fails correctly when there is more than one
;; method group
(define-method-combination wam-test-method-combination-b ()
  ((around (:around))
   (primary * :required t))
  (let ((form (if (rest primary)
                  `(call-method ,(first primary) ,(rest primary))
                  `(call-method ,(first primary)))))
    (if around
        `(call-method ,(first around) (,@(rest around)
                                       (make-method ,form)))
        form)))

(defgeneric wam-test-mc-b (val)
  (:method-combination wam-test-method-combination-b))
(defmethod wam-test-mc-b ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))
(assert (= (wam-test-mc-b 13) 13))
(defmethod wam-test-mc-b :around ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))
(assert (= (wam-test-mc-b 14) 28))
(defmethod wam-test-mc-b :somethingelse ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))
(let ((*error-output* (make-broadcast-stream)))
  (assert-error (wam-test-mc-b 15)))

;;; now, ensure that it fails with a single group with a qualifier-pattern
;;; that is not *
(define-method-combination wam-test-method-combination-c ()
  ((methods listp :required t))
  (if (rest methods)
      `(call-method ,(first methods) ,(rest methods))
      `(call-method ,(first methods))))

(defgeneric wam-test-mc-c (val)
  (:method-combination wam-test-method-combination-c))
(assert-error (wam-test-mc-c 13))
(defmethod wam-test-mc-c :foo ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))
(assert (= (wam-test-mc-c 14) 14))
(defmethod wam-test-mc-c :bar ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))
(let ((*error-output* (make-broadcast-stream)))
  (assert-error (wam-test-mc-c 15)))

;;; DEFMETHOD should signal an ERROR if an incompatible lambda list is
;;; given:
(defmethod incompatible-ll-test-1 (x) x)
(assert-error (defmethod incompatible-ll-test-1 (x y) y))
(assert-error (defmethod incompatible-ll-test-1 (x &rest y) y))
;;; Sneakily using a bit of MOPness to check some consistency
(assert (= (length
            (sb-mop:generic-function-methods #'incompatible-ll-test-1)) 1))

(defmethod incompatible-ll-test-2 (x &key bar) bar)
(assert-error (defmethod incompatible-ll-test-2 (x) x))
(defmethod incompatible-ll-test-2 (x &rest y) y)
(assert (= (length
            (sb-mop:generic-function-methods #'incompatible-ll-test-2)) 1))
(defmethod incompatible-ll-test-2 ((x integer) &key bar) bar)
(assert (= (length
            (sb-mop:generic-function-methods #'incompatible-ll-test-2)) 2))

;;; Per Christophe, this is an illegal method call because of 7.6.5
(handler-bind ((style-warning #'muffle-warning))
  (eval '(assert-error (incompatible-ll-test-2 t 1 2))))

(assert (eq (incompatible-ll-test-2 1 :bar 'yes) 'yes))

(defmethod incompatible-ll-test-3 ((x integer)) x)
(remove-method #'incompatible-ll-test-3
               (find-method #'incompatible-ll-test-3
                            nil
                            (list (find-class 'integer))))
(assert-error (defmethod incompatible-ll-test-3 (x y) (list x y)))


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

;;; Classes with :ALLOCATION :CLASS slots should be subclassable (and
;;; weren't for a while in sbcl-0.7.9.xx)
(defclass superclass-with-slot ()
  ((a :allocation :class)))
(defclass subclass-for-class-allocation (superclass-with-slot) ())
(make-instance 'subclass-for-class-allocation)

;;; bug #136: CALL-NEXT-METHOD was being a little too lexical,
;;; resulting in failure in the following:
(defmethod call-next-method-lexical-args ((x integer))
  x)
(defmethod call-next-method-lexical-args :around ((x integer))
  (let ((x (1+ x)))
    (declare (ignorable x))
    (call-next-method)))
(assert (= (call-next-method-lexical-args 3) 3))

;;; DEFINE-METHOD-COMBINATION with arguments was hopelessly broken
;;; until 0.7.9.5x
(defvar *d-m-c-args-test* nil)
(define-method-combination progn-with-lock ()
  ((methods ()))
  (:arguments object)
  `(unwind-protect
    (progn (lock (object-lock ,object))
           ,@(mapcar #'(lambda (method)
                         `(call-method ,method))
                     methods))
    (unlock (object-lock ,object))))
(defun object-lock (obj)
  (push "object-lock" *d-m-c-args-test*)
  obj)
(defun unlock (obj)
  (push "unlock" *d-m-c-args-test*)
  obj)
(defun lock (obj)
  (push "lock" *d-m-c-args-test*)
  obj)
(defgeneric d-m-c-args-test (x)
  (:method-combination progn-with-lock))
(defmethod d-m-c-args-test ((x symbol))
  (push "primary" *d-m-c-args-test*))
(defmethod d-m-c-args-test ((x number))
  (error "foo"))
(assert (equal (d-m-c-args-test t) '("primary" "lock" "object-lock")))
(assert (equal *d-m-c-args-test*
               '("unlock" "object-lock" "primary" "lock" "object-lock")))
(setf *d-m-c-args-test* nil)
(ignore-errors (d-m-c-args-test 1))
(assert (equal *d-m-c-args-test*
               '("unlock" "object-lock" "lock" "object-lock")))

;;; The walker (on which DEFMETHOD depended) didn't know how to handle
;;; SYMBOL-MACROLET properly.  In fact, as of sbcl-0.7.10.20 it still
;;; doesn't, but it does well enough to compile the following without
;;; error (the problems remain in asking for a complete macroexpansion
;;; of an arbitrary form).
(defgeneric bug222 (z))
(symbol-macrolet ((x 1))
  (defmethod bug222 (z)
    (macrolet ((frob (form) `(progn ,form ,x)))
      (frob (princ-to-string x)))))
(assert (= (bug222 t) 1))

;;; also, a test case to guard against bogus environment hacking:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set 'bug222-b 3))
(defgeneric bug222-b (z stream))
;;; this should at the least compile:
(let ((bug222-b 1))
  (defmethod bug222-b (z stream)
    (macrolet ((frob (form)
                 ;; This macro's expander should see the global value of BUG222-B.
                 ;; But forcing it do to so by explicit call of SYMBOL-VALUE
                 ;; would defeat the purpose of the test. So ignore the warning.
                 (declare (muffle-conditions warning))
                 `(progn ,form ,bug222-b)))
      (frob (format stream "~D~%" bug222-b)))))
;;; and it would be nice (though not specified by ANSI) if the answer
;;; were as follows:
(let ((x (make-string-output-stream)))
  (let ((value (bug222-b t x)))
    ;; not specified by ANSI
    #+#.(cl:if (cl:eq sb-ext:*evaluator-mode* :compile) '(and) '(or))
    (assert (= value 3)))
  ;; specified.
  (assert (char= (char (get-output-stream-string x) 0) #\1)))

;;; REINITIALIZE-INSTANCE, in the ctor optimization, wasn't checking
;;; for invalid initargs where it should:
(defclass class234 () ())
(defclass subclass234 (class234) ())
(defvar *bug234* 0)
(defun bug-234 ()
  (reinitialize-instance (make-instance 'class234) :dummy 0))
(defun subbug-234 ()
  (reinitialize-instance (make-instance 'subclass234) :dummy 0))
(assert-error (bug-234) program-error)
(defmethod shared-initialize :after ((i class234) slots &key dummy)
  (declare (ignore dummy))
  (incf *bug234*))
(assert (typep (subbug-234) 'subclass234))
(assert (= *bug234*
           ;; once for MAKE-INSTANCE, once for REINITIALIZE-INSTANCE
           2))

;;; also, some combinations of MAKE-INSTANCE and subclassing missed
;;; new methods (Gerd Moellmann sbcl-devel 2002-12-29):
(defclass class234-b1 () ())
(defclass class234-b2 (class234-b1) ())
(defvar *bug234-b* 0)
(defun bug234-b ()
  (make-instance 'class234-b2))
(compile 'bug234-b)
(bug234-b)
(assert (= *bug234-b* 0))
(defmethod initialize-instance :before ((x class234-b1) &rest args)
  (declare (ignore args))
  (incf *bug234-b*))
(bug234-b)
(assert (= *bug234-b* 1))

;;; we should be able to make classes with uninterned names:
(defclass #:class-with-uninterned-name () ())

;;; SLOT-MISSING should be called when there are missing slots.
(defclass class-with-all-slots-missing () ())
(defmethod slot-missing (class (o class-with-all-slots-missing)
                         slot-name op
                         &optional new-value)
  (declare (ignore new-value))
  (values op 1 2 3))
(defmethod cwasm-sv ((o class-with-all-slots-missing))
  (slot-value o 'baz))
(defmethod cwasm-ssv (nv (o class-with-all-slots-missing))
  (setf (slot-value o 'baz) nv))
(defmethod cwasm-sbp ((o class-with-all-slots-missing))
  (slot-boundp o 'baz))
(defmethod cwasm-smk ((o class-with-all-slots-missing))
  (slot-makunbound o 'baz))

(with-test (:name :slot-value-missing)
  (assert (equal (multiple-value-list
                  (slot-value (make-instance 'class-with-all-slots-missing) 'foo))
                 '(slot-value)))
  (assert (equal (multiple-value-list
                  (funcall (lambda (x) (slot-value x 'bar))
                           (make-instance 'class-with-all-slots-missing)))
                 '(slot-value)))
  (assert (equal (multiple-value-list
                  (cwasm-sv (make-instance 'class-with-all-slots-missing)))
                 '(slot-value))))

(with-test (:name :slot-boundp-missing)
  (assert (equal (multiple-value-list
                  (slot-boundp (make-instance 'class-with-all-slots-missing) 'foo))
                 '(t)))
  (assert (equal (multiple-value-list
                  (funcall (lambda (x) (slot-boundp x 'bar))
                           (make-instance 'class-with-all-slots-missing)))
                 '(t)))
  (assert (equal (multiple-value-list
                  (cwasm-sbp (make-instance 'class-with-all-slots-missing)))
                 '(t))))

(with-test (:name :slot-setf-missing)
  (assert (equal (multiple-value-list
                  (setf (slot-value (make-instance 'class-with-all-slots-missing) 'foo) 10))
                 '(10)))
  (assert (equal (multiple-value-list
                  (funcall (lambda (x) (setf (slot-value x 'bar) 20))
                           (make-instance 'class-with-all-slots-missing)))
                 '(20)))
  (assert (equal (multiple-value-list
                  (cwasm-ssv 30 (make-instance 'class-with-all-slots-missing)))
                 '(30))))

(with-test (:name :slot-makunbound-missing)
  (let ((instance (make-instance 'class-with-all-slots-missing)))
    (assert (equal (multiple-value-list (slot-makunbound instance 'foo))
                   (list instance)))
    (assert (equal (multiple-value-list
                    (funcall (lambda (x) (slot-makunbound x 'bar)) instance))
                   (list instance)))
    (assert (equal (multiple-value-list
                    (cwasm-smk instance))
                   (list instance)))))

(macrolet ((try (which)
             `(assert (eq ((lambda (x)
                             (declare (,which sb-pcl::set-slot-value))
                             (setf (slot-value x 'b) 'baz))
                           (make-instance 'class-with-all-slots-missing))
            ;; SLOT-MISSING's value is specified to be ignored; we
            ;; return NEW-VALUE.
                          'baz))))
  (try inline)
  (try notinline))

;;; we should be able to specialize on anything that names a class.
(defclass name-for-class () ())
(defmethod something-that-specializes ((x name-for-class)) 1)
(setf (find-class 'other-name-for-class) (find-class 'name-for-class))
(defmethod something-that-specializes ((x other-name-for-class)) 2)
(assert (= (something-that-specializes (make-instance 'name-for-class)) 2))
(assert (= (something-that-specializes (make-instance 'other-name-for-class))
           2))

;;; more forward referenced classes stuff
(defclass frc-1 (frc-2) ())
(assert (subtypep 'frc-1 (find-class 'frc-2)))
(assert (subtypep (find-class 'frc-1) 'frc-2))
(assert (not (subtypep (find-class 'frc-2) 'frc-1)))
(defclass frc-2 (frc-3) ((a :initarg :a)))
(assert (subtypep 'frc-1 (find-class 'frc-3)))
(defclass frc-3 () ())
(assert (typep (make-instance 'frc-1 :a 2) (find-class 'frc-1)))
(assert (typep (make-instance 'frc-2 :a 3) (find-class 'frc-2)))

;;; check that we can define classes with two slots of different names
;;; (even if it STYLE-WARNs).
(defclass odd-name-class ()
  ((name :initarg :name)
   (cl-user::name :initarg :name2)))
(handler-bind ((style-warning #'muffle-warning))
(let ((x (make-instance 'odd-name-class :name 1 :name2 2)))
  (assert (= (slot-value x 'name) 1))
  (assert (= (slot-value x 'cl-user::name) 2))))

;;; ALLOCATE-INSTANCE should work on structures, even if defined by
;;; DEFSTRUCT (and not DEFCLASS :METACLASS STRUCTURE-CLASS).
(defstruct allocatable-structure a)
(assert (typep (allocate-instance (find-class 'allocatable-structure))
               'allocatable-structure))

;;; Bug found by Paul Dietz when devising CPL tests: somewhat
;;; amazingly, calls to CPL would work a couple of times, and then
;;; start returning NIL.  A fix was found (relating to the
;;; applicability of constant-dfun optimization) by Gerd Moellmann.
(defgeneric cpl (x)
  (:method-combination list)
  (:method list ((x broadcast-stream)) 'broadcast-stream)
  (:method list ((x integer)) 'integer)
  (:method list ((x number)) 'number)
  (:method list ((x stream)) 'stream)
  (:method list ((x structure-object)) 'structure-object))
(assert (equal (cpl 0) '(integer number)))
(assert (equal (cpl 0) '(integer number)))
(assert (equal (cpl 0) '(integer number)))
(assert (equal (cpl 0) '(integer number)))
(assert (equal (cpl 0) '(integer number)))
(assert (equal (cpl (make-broadcast-stream))
               '(broadcast-stream stream structure-object)))
(assert (equal (cpl (make-broadcast-stream))
               '(broadcast-stream stream structure-object)))
(assert (equal (cpl (make-broadcast-stream))
               '(broadcast-stream stream structure-object)))

;;; Bug in CALL-NEXT-METHOD: assignment to the method's formal
;;; parameters shouldn't affect the arguments to the next method for a
;;; no-argument call to CALL-NEXT-METHOD
(defgeneric cnm-assignment (x)
  (:method (x) x)
  (:method ((x integer)) (setq x 3)
           (list x (call-next-method) (call-next-method x))))
(assert (equal (cnm-assignment 1) '(3 1 3)))

;;; lp#1734771: Even for assignment to parameters (implcitly or
;;; explicitly) specialized to T, or using EQL specializers.
(defgeneric bug-1734771 (x y)
  (:method (x y) (list x y))
  (:method ((x integer) y)
    (incf y)
    (call-next-method))
  (:method ((x symbol) (y t))
    (incf y)
    (call-next-method))
  (:method (x (y (eql nil)))
    (setf y t)
    (opaque-identity y) ; or else "assigned but never read" style-warning
    (call-next-method)))
(with-test (:name (:cnm-assignment :bug-1734771 1))
  (assert (equal (bug-1734771 2 3) '(2 3))))
(with-test (:name (:cnm-assignment :bug-1734771 2))
  (assert (equal (bug-1734771 t 3) '(t 3))))
(with-test (:name (:cnm-assignment :bug-1734771 3))
  (assert (equal (bug-1734771 #\c nil) '(#\c nil))))

(defgeneric bug-1734771-2 (x &optional y)
  (:method (x &optional (y nil y-p)) (list x y y-p))
  (:method ((x integer) &optional (y 0))
    (incf y)
    (call-next-method))
  (:method ((x symbol) &optional (y 0))
    (declare (ignore y))
    (call-next-method)))
(with-test (:name (:cnm-assignment :bug-1734771 4))
  (assert (equal (bug-1734771-2 2) '(2 nil nil))))
(with-test (:name (:cnm-assignment :bug-1734771 5))
  (assert (equal (bug-1734771-2 2 0) '(2 0 t))))
(with-test (:name (:cnm-assignment :bug-1734771 6))
  (assert (equal (bug-1734771-2 t) '(t nil nil))))

;;; Bug reported by Istvan Marko 2003-07-09
(let ((class-name (gentemp)))
  (loop for i from 1 to 9
        for slot-name = (intern (format nil "X~D" i))
        for initarg-name = (intern (format nil "X~D" i) :keyword)
        collect `(,slot-name :initarg ,initarg-name) into slot-descs
        append `(,initarg-name (list 0)) into default-initargs
        finally (eval `(defclass ,class-name ()
                         (,@slot-descs)
                         (:default-initargs ,@default-initargs))))
  (let ((f (checked-compile `(lambda () (make-instance ',class-name)))))
    (assert (typep (funcall f) class-name))))

;;; bug 262: DEFMETHOD failed on a generic function without a lambda
;;; list
(ensure-generic-function 'bug262)
(defmethod bug262 (x y)
  (list x y))
(assert (equal (bug262 1 2) '(1 2)))

;;; salex on #lisp 2003-10-13 reported that type declarations inside
;;; WITH-SLOTS are too hairy to be checked
(defvar *x*)
(with-test (:name (with-slots declare :note :hairy))
  (flet ((ensure-no-notes (form)
           ;; FIXME: it would be better to check specifically for the "type
           ;; is too hairy" note
           (checked-compile `(lambda () ,form) :allow-notes nil)))
    (ensure-no-notes '(with-slots (a) *x*
                       (declare (integer a))
                       a))
    (ensure-no-notes '(with-slots (a) *x*
                       (declare (integer a))
                       (declare (notinline slot-value))
                       a))))

;;; from CLHS 7.6.5.1
(defclass character-class () ((char :initarg :char)))
(defclass picture-class () ((glyph :initarg :glyph)))
(defclass character-picture-class (character-class picture-class) ())

(defmethod width ((c character-class) &key font) font)
(defmethod width ((p picture-class) &key pixel-size) pixel-size)

(assert-error
 (width (make-instance 'character-class :char #\Q)
        :font 'baskerville :pixel-size 10)
 program-error)
(assert-error
 (width (make-instance 'picture-class :glyph #\Q)
        :font 'baskerville :pixel-size 10)
 program-error)
(assert (eq (width (make-instance 'character-picture-class :char #\Q)
                   :font 'baskerville :pixel-size 10)
            'baskerville))

;;; consequences of 7.6.5.1 on EQL-specialized effective methods
;;; (reported by Syll, lp#1760987)
(defgeneric eqls1760987 (x &key k1))
(defmethod eqls1760987 ((x (eql 1)) &key k1 k2)
  (list x k1 k2))
(defmethod eqls1760987 ((x (eql 2)) &key k1 k3)
  (list x k1 k3))
(defmethod eqls1760987 ((x integer) &key k1 k4)
  (list x k1 k4))

(with-test (:name :bug-1760987)
  (assert (equal (eqls1760987 1) '(1 nil nil)))
  (assert (equal (eqls1760987 1 :k1 2) '(1 2 nil)))
  (assert (equal (eqls1760987 1 :k2 2) '(1 nil 2)))
  (assert (equal (eqls1760987 1 :k1 2 :k2 3) '(1 2 3)))
  (assert (equal (eqls1760987 1 :k4 4) '(1 nil nil)))
  (assert (equal (eqls1760987 1 :k1 2 :k4 4) '(1 2 nil)))
  (assert (equal (eqls1760987 1 :k2 2 :k4 4) '(1 nil 2)))
  (assert (equal (eqls1760987 1 :k1 2 :k2 3 :k4 4) '(1 2 3)))
  (assert (equal (eqls1760987 2) '(2 nil nil)))
  (assert (equal (eqls1760987 2 :k1 2) '(2 2 nil)))
  (assert (equal (eqls1760987 2 :k3 2) '(2 nil 2)))
  (assert (equal (eqls1760987 2 :k1 2 :k3 3) '(2 2 3)))
  (assert (equal (eqls1760987 2 :k4 4) '(2 nil nil)))
  (assert (equal (eqls1760987 2 :k1 2 :k4 4) '(2 2 nil)))
  (assert (equal (eqls1760987 2 :k3 2 :k4 4) '(2 nil 2)))
  (assert (equal (eqls1760987 2 :k1 2 :k3 3 :k4 4) '(2 2 3)))
  (assert (equal (eqls1760987 3) '(3 nil nil)))
  (assert (equal (eqls1760987 3 :k1 2) '(3 2 nil)))
  (assert (equal (eqls1760987 3 :k4 2) '(3 nil 2)))
  (assert (equal (eqls1760987 3 :k1 2 :k4 3) '(3 2 3)))
  (assert-error (eqls1760987 1 :k3 5) program-error)
  (assert-error (eqls1760987 1 :k1 2 :k3 5) program-error)
  (assert-error (eqls1760987 1 :k2 2 :k3 5) program-error)
  (assert-error (eqls1760987 1 :k1 2 :k2 3 :k3 5) program-error)
  (assert-error (eqls1760987 2 :k2 5) program-error)
  (assert-error (eqls1760987 2 :k1 2 :k2 5) program-error)
  (assert-error (eqls1760987 2 :k3 2 :k2 5) program-error)
  (assert-error (eqls1760987 2 :k1 2 :k3 3 :k2 5) program-error)
  (assert-error (eqls1760987 3 :k3 5) program-error)
  (assert-error (eqls1760987 3 :k1 2 :k3 5) program-error)
  (assert-error (eqls1760987 3 :k4 2 :k3 5) program-error)
  (assert-error (eqls1760987 3 :k1 2 :k2 3 :k3 5) program-error)
  (assert-error (eqls1760987 3 :k2 5) program-error)
  (assert-error (eqls1760987 3 :k1 2 :k2 5) program-error)
  (assert-error (eqls1760987 3 :k4 2 :k2 5) program-error)
  (assert-error (eqls1760987 3 :k1 2 :k3 3 :k2 5) program-error))

;;; CLHS 7.6.5 should still hold in the presence of auxiliary methods
(defgeneric gf-with-keys-to-check (a &key b)
  (:method ((a integer) &key b) (declare (ignore b)) (1+ a))
  (:method ((a string) &key b) (list a b))
  (:method ((a symbol) &key b c) (declare (ignore b)) (list a c))
  (:method :around ((a integer) &key b) (declare (ignore b)) (1+ (call-next-method))))

(with-test (:name (:check-keyword-args :no-error))
  (assert (= (gf-with-keys-to-check 1) 3))
  (assert (= (gf-with-keys-to-check 1 :b 2) 3))
  (assert (equal (gf-with-keys-to-check "a") '("a" nil)))
  (assert (equal (gf-with-keys-to-check "a" :b 2) '("a" 2)))
  (assert (equal (gf-with-keys-to-check 'a) '(a nil)))
  (assert (equal (gf-with-keys-to-check 'a :b 2) '(a nil)))
  (assert (equal (gf-with-keys-to-check 'a :c 2) '(a 2)))
  (assert (equal (gf-with-keys-to-check 'a :b 2 :c 3) '(a 3))))

(with-test (:name (:check-keyword-args :allow-other-keys :no-error))
  (assert (= (gf-with-keys-to-check 1 :z 3 :allow-other-keys t) 3))
  (assert (= (gf-with-keys-to-check 1 :b 2 :z 3 :allow-other-keys t) 3))
  (assert (equal (gf-with-keys-to-check "a" :z 3 :allow-other-keys t) '("a" nil)))
  (assert (equal (gf-with-keys-to-check "a" :b 2 :z 3 :allow-other-keys t) '("a" 2)))
  (assert (equal (gf-with-keys-to-check 'a :z 3 :allow-other-keys t) '(a nil)))
  (assert (equal (gf-with-keys-to-check 'a :b 2 :z 3 :allow-other-keys t) '(a nil)))
  (assert (equal (gf-with-keys-to-check 'a :c 2 :z 3 :allow-other-keys t) '(a 2)))
  (assert (equal (gf-with-keys-to-check 'a :b 2 :c 3 :allow-other-keys t) '(a 3))))

(with-test (:name (:check-keyword-args :unmatched-keyword :error))
  (assert-error (gf-with-keys-to-check 1 :z 3) program-error)
  (assert-error (gf-with-keys-to-check 1 :b 2 :z 3) program-error)
  (assert-error (gf-with-keys-to-check "a" :z 3) program-error)
  (assert-error (gf-with-keys-to-check "a" :b 2 :z 3) program-error)
  (assert-error (gf-with-keys-to-check 'a :z 3) program-error)
  (assert-error (gf-with-keys-to-check 'a :b 2 :z 3) program-error)
  (assert-error (gf-with-keys-to-check 'a :c 2 :z 3) program-error)
  (assert-error (gf-with-keys-to-check 'a :b 2 :c 3 :z 4) program-error))

(with-test (:name (:check-keyword-args :odd-keyword :error))
  (assert-error (gf-with-keys-to-check 1 :b) program-error)
  (assert-error (gf-with-keys-to-check 1 :b 2 :b) program-error)
  (assert-error (gf-with-keys-to-check "a" :b) program-error)
  (assert-error (gf-with-keys-to-check "a" :b 2 :b) program-error)
  (assert-error (gf-with-keys-to-check 'a :b) program-error)
  (assert-error (gf-with-keys-to-check 'a :b 2 :b) program-error)
  (assert-error (gf-with-keys-to-check 'a :c 2 :b) program-error)
  (assert-error (gf-with-keys-to-check 'a :b 2 :c 3 :b) program-error))

;;; verify that we perform these checks for standardized generic
;;; functions too
(defclass shared-initialize-keyword-check () ())

(with-test (:name (:check-keyword-args shared-initialize :odd-keyword :error))
  (assert-error (shared-initialize (make-instance 'shared-initialize-keyword-check) nil :a)
                program-error))

(with-test (:name (:check-keyword-args shared-initialize :non-keyword :error))
  (assert-error (shared-initialize (make-instance 'shared-initialize-keyword-check) nil '(abc) 1)
                program-error))

;;; verify that we can still detect no primary methods and invalid qualifiers

(defmethod gf-with-keys-and-no-primary-method :around ((x integer) &key b)
  (declare (ignore b))
  (1+ x))

(with-test (:name (:check-keyword-args :no-primary-method :no-keywords :error))
  (assert-error (gf-with-keys-and-no-primary-method 3) sb-pcl::no-primary-method-error))
(with-test (:name (:check-keyword-args :no-primary-method :ok-keyword :error))
  (assert-error (gf-with-keys-and-no-primary-method 3 :b 2) sb-pcl::no-primary-method-error))
(with-test (:name (:check-keyword-args :no-primary-method :bad-keyword :error))
  (assert-error (gf-with-keys-and-no-primary-method 3 :c 2)
                (or program-error sb-pcl::no-primary-method-error)))

(defgeneric gf-with-keys-and-invalid-qualifier (x &key)
  (:method-combination progn))
(defmethod gf-with-keys-and-invalid-qualifier progn ((x integer) &key b)
  (declare (ignore b))
  (print (1+ x) (make-broadcast-stream))) ; don't show noise
;; I'm guessing this is PROGN but spelled wrong purposely, right?
(defmethod gf-with-keys-and-invalid-qualifier prong ((x fixnum) &key c) (print c))

(with-test (:name (:check-keyword-args :invalid-qualifier :no-keywords :error))
  (assert-error (gf-with-keys-and-invalid-qualifier 3)))
(with-test (:name (:check-keyword-args :invalid-qualifier :ok-keyword :error))
  (assert-error (gf-with-keys-and-invalid-qualifier 3 :b 3)))
(with-test (:name (:check-keyword-args :invalid-qualifier :bad-keyword :error))
  (assert-error (gf-with-keys-and-invalid-qualifier 3 :z 4)))
(with-test (:name (:check-keyword-args :no-applicable-invalid-qualifier :no-keywords :no-error))
  (assert (= (gf-with-keys-and-invalid-qualifier (1+ most-positive-fixnum))
             (+ most-positive-fixnum 2))))
(with-test (:name (:check-keyword-args :no-applicable-invalid-qualifier :ok-keyword :no-error))
  (assert (= (gf-with-keys-and-invalid-qualifier (1+ most-positive-fixnum) :b 3)
             (+ most-positive-fixnum 2))))
(with-test (:name (:check-keyword-args :no-applicable-invalid-qualifier :bad-keyword :error))
  (assert-error (gf-with-keys-and-invalid-qualifier (1+ most-positive-fixnum) :c 3)))

;;; class redefinition shouldn't give any warnings, in the usual case
(defclass about-to-be-redefined () ((some-slot :accessor some-slot)))
(handler-bind ((warning #'error))
  (defclass about-to-be-redefined () ((some-slot :accessor some-slot))))

;;; attempts to add accessorish methods to generic functions with more
;;; complex lambda lists should fail
(defgeneric accessoroid (object &key &allow-other-keys))
(assert-error
 (defclass accessoroid-class () ((slot :accessor accessoroid)))
 program-error)

;;; reported by Bruno Haible sbcl-devel 2004-04-15
(defclass shared-slot-and-redefinition ()
  ((size :initarg :size :initform 1 :allocation :class)))
(let ((i (make-instance 'shared-slot-and-redefinition)))
  (defclass shared-slot-and-redefinition ()
    ((size :initarg :size :initform 2 :allocation :class)))
  (assert (= (slot-value i 'size) 1)))

;;; reported by Bruno Haible sbcl-devel 2004-04-15
(defclass superclass-born-to-be-obsoleted () (a))
(defclass subclass-born-to-be-obsoleted (superclass-born-to-be-obsoleted) ())
(defparameter *born-to-be-obsoleted*
  (make-instance 'subclass-born-to-be-obsoleted))
(defparameter *born-to-be-obsoleted-obsoleted* nil)
(defmethod update-instance-for-redefined-class
    ((o subclass-born-to-be-obsoleted) a d pl &key)
  (setf *born-to-be-obsoleted-obsoleted* t))
(make-instances-obsolete 'superclass-born-to-be-obsoleted)
(slot-boundp *born-to-be-obsoleted* 'a)
(assert *born-to-be-obsoleted-obsoleted*)

;;; additional test suggested by Bruno Haible sbcl-devel 2004-04-21
(defclass super-super-obsoleted () (a))
(defclass super-obsoleted-1 (super-super-obsoleted) ())
(defclass super-obsoleted-2 (super-super-obsoleted) ())
(defclass obsoleted (super-obsoleted-1 super-obsoleted-2) ())
(defparameter *obsoleted* (make-instance 'obsoleted))
(defparameter *obsoleted-counter* 0)
(defmethod update-instance-for-redefined-class ((o obsoleted) a d pl &key)
  (incf *obsoleted-counter*))
(make-instances-obsolete 'super-super-obsoleted)
(slot-boundp *obsoleted* 'a)
(assert (= *obsoleted-counter* 1))

;;; yet another MAKE-INSTANCES-OBSOLETE test, this time from Nikodemus
;;; Siivola.  Not all methods for accessing slots are created equal...
(defclass yet-another-obsoletion-super () ((obs :accessor obs-of :initform 0)))
(defclass yet-another-obsoletion-sub (yet-another-obsoletion-super) ())
(defmethod shared-initialize :after ((i yet-another-obsoletion-super)
                                     slots &rest init)
  (declare (ignore init))
  (incf (obs-of i)))

(defvar *yao-super* (make-instance 'yet-another-obsoletion-super))
(defvar *yao-sub* (make-instance 'yet-another-obsoletion-sub))

(assert (= (obs-of *yao-super*) 1))
(assert (= (obs-of *yao-sub*) 1))
(make-instances-obsolete 'yet-another-obsoletion-super)
(assert (= (obs-of *yao-sub*) 2))
(assert (= (obs-of *yao-super*) 2))
(make-instances-obsolete 'yet-another-obsoletion-super)
(assert (= (obs-of *yao-super*) 3))
(assert (= (obs-of *yao-sub*) 3))
(assert (= (slot-value *yao-super* 'obs) 3))
(assert (= (slot-value *yao-sub* 'obs) 3))

;;; one more MIO test: variable slot names
(defclass mio () ((x :initform 42)))
(defvar *mio-slot* 'x)
(defparameter *mio-counter* 0)
(defmethod update-instance-for-redefined-class ((instance mio) new old plist &key)
  (incf *mio-counter*))

(let ((x (make-instance 'mio)))
  (make-instances-obsolete 'mio)
  (slot-value x *mio-slot*))

(let ((x (make-instance 'mio)))
  (make-instances-obsolete 'mio)
  (setf (slot-value x *mio-slot*) 13))

(let ((x (make-instance 'mio)))
  (make-instances-obsolete 'mio)
  (slot-boundp x *mio-slot*))

(let ((x (make-instance 'mio)))
  (make-instances-obsolete 'mio)
  (slot-makunbound x *mio-slot*))

(assert (= 4 *mio-counter*))

;;; :class -> :instance slot allocation transfers of inherited slots,
;;; reported by Bruno Haible
(with-test (:name (defclass :redefinition-class->instance-allocation))
  (let (i)
   (defclass super-with-magic-slot ()
     ((magic :initarg :size :initform 1 :allocation :class)))
   (defclass sub-of-super-with-magic-slot (super-with-magic-slot) ())
   (setq i (make-instance 'sub-of-super-with-magic-slot))
   (defclass super-with-magic-slot ()
     ((magic :initarg :size :initform 2)))
   (assert (= 1 (slot-value i 'magic)))))

;;; MAKE-INSTANCES-OBSOLETE return values
(with-test (:name (make-instances-obsolete :return-values) )
  (defclass one-more-to-obsolete () ())
  (assert (eq 'one-more-to-obsolete
              (make-instances-obsolete 'one-more-to-obsolete)))
  (assert (eq (find-class 'one-more-to-obsolete)
              (make-instances-obsolete (find-class 'one-more-to-obsolete)))))

;;; Sensible error instead of a BUG. Reported by Thomas Burdick.
(with-test (:name (defclass :slot-with-duplicate-accessors))
  (assert-error (defclass slot-with-duplicate-accessors ()
                  ((slot :writer get-slot :reader get-slot)))
                (and error (not sb-int:bug))))

;;; BUG 321: errors in parsing DEFINE-METHOD-COMBINATION arguments
;;; lambda lists.

(define-method-combination w-args ()
  ((method-list *))
  (:arguments arg1 arg2 &aux (extra :extra))
  `(progn ,arg1 ,arg2 ,extra
          ,@(mapcar (lambda (method) `(call-method ,method)) method-list)))
(defgeneric mc-test-w-args (p1 p2 s)
  (:method-combination w-args)
  (:method ((p1 number) (p2 t) s)
    (vector-push-extend (list 'number p1 p2) s))
  (:method ((p1 string) (p2 t) s)
    (vector-push-extend (list 'string p1 p2) s))
  (:method ((p1 t) (p2 t) s) (vector-push-extend (list t p1 p2) s)))
(let ((v (make-array 0 :adjustable t :fill-pointer t)))
  (assert (= (mc-test-w-args 1 2 v) 1))
  (assert (equal (aref v 0) '(number 1 2)))
  (assert (equal (aref v 1) '(t 1 2))))

;;; BUG 276: declarations and mutation.
(defmethod fee ((x fixnum))
  (setq x (/ x 2))
  x)
(assert (= (fee 1) 1/2))
(defmethod fum ((x fixnum))
  (setf x (/ x 2))
  x)
(assert (= (fum 3) 3/2))
(defmethod fii ((x fixnum))
  (declare (special x))
  (setf x (/ x 2))
  x)
(assert (= (fii 1) 1/2))
(defvar *faa*)
(defmethod faa ((*faa* string-stream))
  (setq *faa* (make-broadcast-stream *faa*))
  (write-line "Break, you sucker!" *faa*)
  'ok)
(assert (eq 'ok (faa (make-string-output-stream))))
(defmethod fex ((x fixnum) (y fixnum))
  (multiple-value-setq (x y) (values (/ x y) (/ y x)))
  (list x y))
(assert (equal (fex 5 3) '(5/3 3/5)))

;;; Bug reported by Zach Beane; incorrect return of (function
;;; ',fun-name) in defgeneric
(with-test (:name (defgeneric :return type))
  (flet ((test (form)
           (let ((fun (checked-compile form)))
             (assert (typep (funcall fun) 'generic-function)))))
    (test '(lambda () (flet ((nonsense () nil))
                        (declare (ignorable #'nonsense))
                        (defgeneric nonsense ()))))
    (test '(lambda () (flet ((nonsense-2 () nil))
                        (declare (ignorable #'nonsense-2))
                        (defgeneric nonsense-2 ()
                          (:method () t)))))))

;;; bug reported by Bruno Haible: (setf find-class) using a
;;; forward-referenced class
(defclass fr-sub (fr-super) ())
(setf (find-class 'fr-alt) (find-class 'fr-super))
(assert (eq (find-class 'fr-alt) (find-class 'fr-super)))


;;; ANSI Figure 4-8: all defined classes.  Check that we can define
;;; methods on all of these.
(progn
  (defgeneric method-for-defined-classes (x))
  (dolist (c '(arithmetic-error
               generic-function simple-error array hash-table
               simple-type-error
               bit-vector integer simple-warning
               broadcast-stream list standard-class
               built-in-class logical-pathname standard-generic-function
               cell-error method standard-method
               character method-combination standard-object
               class null storage-condition
               complex number stream
               concatenated-stream package stream-error
               condition package-error string
               cons parse-error string-stream
               control-error pathname structure-class
               division-by-zero print-not-readable structure-object
               echo-stream program-error style-warning
               end-of-file random-state symbol
               error ratio synonym-stream
               file-error rational t
               file-stream reader-error two-way-stream
               float readtable type-error
               floating-point-inexact real unbound-slot
               floating-point-invalid-operation restart unbound-variable
               floating-point-overflow sequence undefined-function
               floating-point-underflow serious-condition vector
               function simple-condition warning))
    (eval `(defmethod method-for-defined-classes ((x ,c)) (princ x))))
  (assert (string= (with-output-to-string (*standard-output*)
                     (method-for-defined-classes #\3))
                   "3")))



;;; When class definition does not complete due to a bad accessor
;;; name, do not cause an error when a new accessor name is provided
;;; during class redefinition

(defun existing-name (object)
  (list object))

(assert-error (defclass redefinition-of-accessor-class ()
                ((slot :accessor existing-name))))

(defclass redefinition-of-accessor-class ()
  ((slot :accessor new-name)))



(load "package-ctor-bug.lisp")
(assert (= (package-ctor-bug:test) 3))
(delete-package "PACKAGE-CTOR-BUG")
(load "package-ctor-bug.lisp")
(assert (= (package-ctor-bug:test) 3))

(with-test (:name (defmethod (setf find-class) integer))
  (handler-bind ((warning #'muffle-warning))
  (mapcar #'eval
          '(
            (deftype defined-type () 'integer)
            (assert-error
             (defmethod method-on-defined-type ((x defined-type)) x))
            (deftype defined-type-and-class () 'integer)
            (setf (find-class 'defined-type-and-class) (find-class 'integer))
            (defmethod method-on-defined-type-and-class
                ((x defined-type-and-class))
              (1+ x))
            (assert (= (method-on-defined-type-and-class 3) 4))))))

;; bug 281
(let (#+nil ; no more sb-pcl::*max-emf-precomputation-methods* as of
            ; sbcl-1.0.41.x
      (sb-pcl::*max-emf-precomputation-methods* 0))
  (handler-bind ((warning #'muffle-warning)) ; "invalid qualifiers"
  (eval '(defgeneric bug-281 (x)
          (:method-combination +)
          (:method ((x symbol)) 1)
          (:method + ((x number)) x))))
  (assert (= 1 (funcall 'bug-281 1)))
  (assert (= 4.2 (funcall 'bug-281 4.2)))
  (multiple-value-bind (val err) (ignore-errors (funcall 'bug-281 'symbol))
    (assert (not val))
    (assert (typep err 'error))))

;;; RESTART-CASE and CALL-METHOD

;;; from Bruno Haible

(defun rc-cm/prompt-for-new-values ()
  (format *debug-io* "~&New values: ")
  (finish-output *debug-io*)
  (list (read *debug-io*)))

(defun rc-cm/add-method-restarts (form method)
  (let ((block (gensym))
        (tag (gensym)))
    `(block ,block
      (tagbody
         ,tag
         (return-from ,block
           (restart-case ,form
             (method-redo ()
               :report (lambda (stream)
                         (format stream "Try calling ~S again." ,method))
               (go ,tag))
             (method-return (l)
               :report (lambda (stream)
                         (format stream "Specify return values for ~S call."
                                 ,method))
               :interactive (lambda () (rc-cm/prompt-for-new-values))
               (return-from ,block (values-list l)))))))))

(defun rc-cm/convert-effective-method (efm)
  (if (consp efm)
      (if (eq (car efm) 'call-method)
          (let ((method-list (third efm)))
            (if (or (typep (first method-list) 'method) (rest method-list))
                ;; Reduce the case of multiple methods to a single one.
                ;; Make the call to the next-method explicit.
                (rc-cm/convert-effective-method
                 `(call-method ,(second efm)
                   ((make-method
                     (call-method ,(first method-list) ,(rest method-list))))))
                ;; Now the case of at most one method.
                (if (typep (second efm) 'method)
                    ;; Wrap the method call in a RESTART-CASE.
                    (rc-cm/add-method-restarts
                     (cons (rc-cm/convert-effective-method (car efm))
                           (rc-cm/convert-effective-method (cdr efm)))
                     (second efm))
                    ;; Normal recursive processing.
                    (cons (rc-cm/convert-effective-method (car efm))
                          (rc-cm/convert-effective-method (cdr efm))))))
          (cons (rc-cm/convert-effective-method (car efm))
                (rc-cm/convert-effective-method (cdr efm))))
      efm))

(define-method-combination standard-with-restarts ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods-sequentially (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let ((form (if (or before after (rest primary))
                    `(multiple-value-prog1
                       (progn
                         ,@(call-methods-sequentially before)
                         (call-method ,(first primary) ,(rest primary)))
                      ,@(call-methods-sequentially (reverse after)))
                    `(call-method ,(first primary)))))
      (when around
        (setq form
              `(call-method ,(first around)
                (,@(rest around) (make-method ,form)))))
      (rc-cm/convert-effective-method form))))

(defgeneric rc-cm/testgf16 (x)
  (:method-combination standard-with-restarts))
(defclass rc-cm/testclass16a () ())
(defclass rc-cm/testclass16b (rc-cm/testclass16a) ())
(defclass rc-cm/testclass16c (rc-cm/testclass16a) ())
(defclass rc-cm/testclass16d (rc-cm/testclass16b rc-cm/testclass16c) ())
(defmethod rc-cm/testgf16 ((x rc-cm/testclass16a))
  (list 'a
        (not (null (find-restart 'method-redo)))
        (not (null (find-restart 'method-return)))))
(defmethod rc-cm/testgf16 ((x rc-cm/testclass16b))
  (cons 'b (call-next-method)))
(defmethod rc-cm/testgf16 ((x rc-cm/testclass16c))
  (cons 'c (call-next-method)))
(defmethod rc-cm/testgf16 ((x rc-cm/testclass16d))
  (cons 'd (call-next-method)))
(assert (equal (rc-cm/testgf16 (make-instance 'rc-cm/testclass16d))
               '(d b c a t t)))

;;; test case from Gerd Moellmann
(define-method-combination r-c/c-m-1 ()
  ((primary () :required t))
  `(restart-case (call-method ,(first primary))))

(defgeneric r-c/c-m-1-gf ()
  (:method-combination r-c/c-m-1)
  (:method () nil))

(assert (null (r-c/c-m-1-gf)))

(handler-bind ((warning #'error))
  (eval '(defclass class-for-ctor/class-slot ()
          ((class-slot :initarg :class-slot :allocation :class))))
  (eval '(let ((c1 (make-instance 'class-for-ctor/class-slot))
               (c2 (make-instance 'class-for-ctor/class-slot :class-slot 1)))
          (assert (equal (list (slot-value c1 'class-slot)
                               (slot-value c2 'class-slot))
                   (list 1 1))))))

;;; tests of ctors on anonymous classes
(defparameter *unnamed* (defclass ctor-unnamed-literal-class () ()))
(setf (class-name *unnamed*) nil)
(setf (find-class 'ctor-unnamed-literal-class) nil)
(defparameter *unnamed2* (defclass ctor-unnamed-literal-class2 () ()))
(defun ctor-unnamed-literal-class ()
  (make-instance '#.*unnamed*))
(compile 'ctor-unnamed-literal-class)
(defun ctor-unnamed-literal-class2 ()
  (make-instance '#.(find-class 'ctor-unnamed-literal-class2)))
(compile 'ctor-unnamed-literal-class2)
(defun ctor-unnamed-literal-class2/symbol ()
  (make-instance 'ctor-unnamed-literal-class2))
(compile 'ctor-unnamed-literal-class2/symbol)
(setf (class-name *unnamed2*) nil)
(setf (find-class 'ctor-unnamed-literal-class2) nil)
(with-test (:name (:ctor :unnamed-before))
  (assert (typep (ctor-unnamed-literal-class) *unnamed*)))
(with-test (:name (:ctor :unnamed-after))
  (assert (typep (ctor-unnamed-literal-class2) *unnamed2*)))
(with-test (:name (:ctor :unnamed-after/symbol))
  (assert-error (ctor-unnamed-literal-class2/symbol)))

;;; classes with slot types shouldn't break if the types don't name
;;; classes (bug #391)
(defclass slot-type-superclass () ((slot :type fixnum)))
(defclass slot-type-subclass (slot-type-superclass)
  ((slot :type (integer 1 5))))
(let ((instance (make-instance 'slot-type-subclass)))
  (setf (slot-value instance 'slot) 3))

;;; ctors where there's a non-standard SHARED-INITIALIZE method and an
;;; initarg which isn't self-evaluating (kpreid on #lisp 2006-01-29)
(defclass kpreid-enode ()
  ((slot :initarg not-a-keyword)))
(defmethod shared-initialize ((o kpreid-enode) slots &key &allow-other-keys)
  (call-next-method))
(defun make-kpreid-enode ()
  (make-instance 'kpreid-enode 'not-a-keyword 3))
(with-test (:name (:ctor :non-keyword-initarg))
  (let ((x (make-kpreid-enode))
        (y (make-kpreid-enode)))
    (= (slot-value x 'slot) (slot-value y 'slot))))

;;; defining a class hierarchy shouldn't lead to spurious classoid
;;; errors on TYPEP questions (reported by Tim Moore on #lisp
;;; 2006-03-10)
(defclass backwards-2 (backwards-1) (a b))
(defclass backwards-3 (backwards-2) ())
(defun typep-backwards-3 (x)
  (typep x 'backwards-3))
(defclass backwards-1 () (a b))
(assert (not (typep-backwards-3 1)))
(assert (not (typep-backwards-3 (make-instance 'backwards-2))))
(assert (typep-backwards-3 (make-instance 'backwards-3)))

(defgeneric remove-method-1 (x)
  (:method ((x integer)) (1+ x)))
(defgeneric remove-method-2 (x)
  (:method ((x integer)) (1- x)))
(assert (eq #'remove-method-1
            (remove-method #'remove-method-1
                           (find-method #'remove-method-2
                                        nil
                                        (list (find-class 'integer))))))
(assert (= (remove-method-1 3) 4))
(assert (= (remove-method-2 3) 2))

;;; ANSI doesn't require these restarts, but now that we have them we
;;; better test them too.
(defclass slot-unbound-restart-test () ((x)))
(let ((test (make-instance 'slot-unbound-restart-test)))
  (assert (not (slot-boundp test 'x)))
  (assert (= 42 (handler-bind ((unbound-slot (lambda (c) (use-value 42 c))))
                  (slot-value test 'x))))
  (assert (not (slot-boundp test 'x)))
  (assert (= 13 (handler-bind ((unbound-slot (lambda (c) (store-value 13 c))))
                  (slot-value test 'x))))
  (assert (= 13 (slot-value test 'x))))
;;; Using class instances as specializers, reported by Pascal Costanza, ref CLHS 7.6.2
(defclass class-as-specializer-test ()
   ())
(eval `(defmethod class-as-specializer-test1 ((x ,(find-class 'class-as-specializer-test)))
         'foo))
(assert (eq 'foo (class-as-specializer-test1 (make-instance 'class-as-specializer-test))))
(funcall (checked-compile `(lambda ()
                             (defmethod class-as-specializer-test2 ((x ,(find-class 'class-as-specializer-test)))
                               'bar))))
(assert (eq 'bar (class-as-specializer-test2 (make-instance 'class-as-specializer-test))))

;;; CHANGE-CLASS and tricky allocation.
(defclass foo-to-be-changed ()
  ((a :allocation :class :initform 1)))
(defclass bar-to-be-changed (foo-to-be-changed) ())
(defvar *bar-to-be-changed* (make-instance 'bar-to-be-changed))
(defclass baz-to-be-changed ()
  ((a :allocation :instance :initform 2)))
(change-class *bar-to-be-changed* 'baz-to-be-changed)
(assert (= (slot-value *bar-to-be-changed* 'a) 1))

;;; proper name and class redefinition
(defvar *to-be-renamed1* (defclass to-be-renamed1 () ()))
(defvar *to-be-renamed2* (defclass to-be-renamed2 () ()))
(setf (find-class 'to-be-renamed1) (find-class 'to-be-renamed2))
(defvar *renamed1* (defclass to-be-renamed1 () ()))
(assert (not (eq *to-be-renamed1* *to-be-renamed2*)))
(assert (not (eq *to-be-renamed1* *renamed1*)))
(assert (not (eq *to-be-renamed2* *renamed1*)))

;;; CLASS-NAME (and various other standardized generic functions) have
;;; their effective methods precomputed; in the process of rearranging
;;; (SETF FIND-CLASS) and FINALIZE-INHERITANCE, this broke.
(defclass class-with-odd-class-name-method ()
  ((a :accessor class-name)))

;;; another case where precomputing (this time on PRINT-OBJECT) and
;;; lazily-finalized classes caused problems.  (report from James Y
;;; Knight sbcl-devel 20-07-2006)

(defclass base-print-object () ())
;;; this has the side-effect of finalizing BASE-PRINT-OBJECT, and
;;; additionally the second specializer (STREAM) changes the cache
;;; structure to require two keys, not just one.
;; warning is "Specializing on the second argument to PRINT-OBJECT ..."
(handler-bind ((warning #'muffle-warning))
(defmethod print-object ((o base-print-object) (s stream))
  nil))
;;; unfinalized as yet
(defclass sub-print-object (base-print-object) ())
;;; the accessor causes an eager finalization
(defclass subsub-print-object (sub-print-object)
  ((a :accessor a)))

;;; triggers a discriminating function (and so cache) recomputation.
;;; The method on BASE-PRINT-OBJECT will cause the system to attempt
;;; to fill the cache for all subclasses of BASE-PRINT-OBJECT which
;;; have valid wrappers; however, in the course of doing so, the
;;; SUB-PRINT-OBJECT class gets finalized, which invalidates the
;;; SUBSUB-PRINT-OBJECT wrapper; if an invalid wrapper gets into a
;;; cache with more than one key, then failure ensues.
(reinitialize-instance #'print-object)

;;; bug in long-form method combination: if there's an applicable
;;; method not part of any method group, we need to call
;;; INVALID-METHOD-ERROR.  (MC27 test case from Bruno Haible)
(define-method-combination mc27 ()
  ((normal ())
   (ignored (:ignore :unused)))
  `(list 'result
    ,@(mapcar #'(lambda (method) `(call-method ,method)) normal)))
(defgeneric test-mc27 (x)
  (:method-combination mc27)
  (:method :ignore ((x number)) (declare (notinline /)) (/ 0)))
(handler-bind ((style-warning #'muffle-warning))
(assert-error (test-mc27 7)))

(define-method-combination mc27prime ()
  ((normal ())
   (ignored (:ignore)))
  `(list 'result ,@(mapcar (lambda (m) `(call-method ,m)) normal)))
(defgeneric test-mc27prime (x)
  (:method-combination mc27prime)
  (:method :ignore ((x number)) (declare (notinline /)) (/ 0)))
(handler-bind ((style-warning #'muffle-warning))
(assert (equal '(result) (test-mc27prime 3))))
(assert-error (test-mc27 t) sb-pcl::no-applicable-method-error) ; still no-applicable-method

;;; more invalid wrappers.  This time for a long-standing bug in the
;;; compiler's expansion for TYPEP on various class-like things, with
;;; user-visible consequences.
(defclass obsolete-again () ())
(defvar *obsolete-again* (make-instance 'obsolete-again))
(defvar *obsolete-again-hash* (sxhash *obsolete-again*))
(make-instances-obsolete (find-class 'obsolete-again))
(assert (not (streamp *obsolete-again*)))
(make-instances-obsolete (find-class 'obsolete-again))
(assert (= (sxhash *obsolete-again*) *obsolete-again-hash*))
(compile (defun is-a-structure-object-p (x) (typep x 'structure-object)))
(make-instances-obsolete (find-class 'obsolete-again))
(assert (not (is-a-structure-object-p *obsolete-again*)))

;;; overeager optimization of slot-valuish things
(defclass listoid ()
  ((caroid :initarg :caroid)
   (cdroid :initarg :cdroid :initform nil)))
(defmethod lengthoid ((x listoid))
  (let ((result 0))
    (loop until (null x)
          do (incf result) (setq x (slot-value x 'cdroid)))
    result))
(with-test (:name ((:setq :method-parameter) slot-value))
  (assert (= (lengthoid (make-instance 'listoid)) 1))
  (assert (= (lengthoid
              (make-instance 'listoid :cdroid
                             (make-instance 'listoid :cdroid
                                            (make-instance 'listoid))))
             3)))



;;;; Tests for argument parsing in fast-method-functions.

(defvar *foo* 0)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (symbol-value 'a) 'invalid))

(defmacro test1 (lambda-list values args &key declarations cnm)
  `(progn
     (fmakunbound 'll-method)
     (fmakunbound 'll-function)
     (defmethod ll-method ,lambda-list
       ,@declarations
       ,@(when cnm
           `((when nil (call-next-method))))
       (list ,@values))
     (defun ll-function ,lambda-list
       ,@declarations
       (list ,@values))
     (dotimes (i 2)
       (assert (equal (ll-method ,@args)
                      (ll-function ,@args))))))

(defmacro test (&rest args)
  `(progn
     (test1 ,@args :cnm nil)
     (test1 ,@args :cnm t)))

;; Just plain arguments

(test (a) (a) (1))
(test (a b c d e f g h i) (a b c d e f g h i) (1 2 3 4 5 6 7 8 9))

(test (*foo*) (*foo* (symbol-value '*foo*)) (1))

(test (a) (a (symbol-value 'a)) (1)
      :declarations ((declare (special a))))

;; Optionals

(test (a &optional b c) (a b c) (1))
(test (a &optional b c) (a b c) (1 2))
(test (a &optional b c) (a b c) (1 2 3))

(test (a &optional (b 'b b-p) (c 'c c-p)) (a b c b-p c-p) (1))
(test (a &optional (b 'b b-p) (c 'c c-p)) (a b c b-p c-p) (1 2))
(test (a &optional (b 'b b-p) (c 'c c-p)) (a b c b-p c-p) (1 2 3))

(test (&optional *foo*) (*foo* (symbol-value '*foo*)) ())
(test (&optional *foo*) (*foo* (symbol-value '*foo*)) (1))

(test (&optional (*foo* 'z foo-p)) (*foo* (symbol-value '*foo*) foo-p) ())
(test (&optional (*foo* 'z foo-p)) (*foo* (symbol-value '*foo*) foo-p) (1))

(test (&optional a) (a (symbol-value 'a)) ()
      :declarations ((declare (special a))))
(test (&optional a) (a (symbol-value 'a)) (1)
      :declarations ((declare (special a))))

(test (&optional (a 'z a-p)) (a (symbol-value 'a) a-p) ()
      :declarations ((declare (special a))))
(test (&optional (a 'z a-p)) (a (symbol-value 'a) a-p) (1)
      :declarations ((declare (special a))))

(defparameter *count* 0)

(test (&optional (a (incf *count*)) (b (incf *count*)))
      (a b *count* (setf *count* 0))
      ())

;; Keywords with some &RESTs thrown in

(dolist (args '((1)
                (1 :b 2)
                (1 :c 3)
                (1 :b 2 :c 3)
                (1 :c 3 :b 2)
                (1 :c 3 :c 1 :b 2 :b 4)))
  (eval `(test (a &key b c) (a b c) ,args))
  (eval `(test (a &key (b 'b b-p) (c 'c c-p))
               (a b c b-p c-p)
               ,args))
  (eval `(test (a &rest rest &key (b 'b b-p) (c 'c c-p))
               (a b c b-p c-p rest)
               ,args))
  (eval `(test (a &rest *foo* &key (b 'b b-p) (c 'c c-p))
               (a b c b-p c-p *foo* (symbol-value '*foo*))
               ,args))
  (eval `(test (a &rest *foo* &key (b 'b b-p) (c 'c c-p))
               (a b c b-p c-p *foo* (symbol-value '*foo*))
               ,args
               :declarations ((declare (special b-p))))))

(dolist (args '(()
                (:*foo* 1)
                (:*foo* 1 :*foo* 2)))
  (eval `(test (&key *foo*) (*foo* (symbol-value '*foo*)) ,args))
  (eval `(test (&key (*foo* 'z foo-p)) (*foo* (symbol-value '*foo*) foo-p)
               ,args))
  (eval `(test (&key ((:*foo* a) 'z foo-p)) (a (symbol-value 'a) foo-p)
               ,args))
  (eval `(test (&key ((:*foo* a) 'z foo-p)) (a (symbol-value 'a) foo-p)
               ,args
               :declarations ((declare (special a))))))

(defparameter *count* 0)

(test (&key (a (incf *count*)) (b (incf *count*)))
      (a b *count* (setf *count* 0))
      ())

(test (&key a b &allow-other-keys) (a b) (:a 1 :b 2 :c 3))

(defmethod clim-style-lambda-list-test (a b &optional c d &key x y)
  (list a b c d x y))

(clim-style-lambda-list-test 1 2)

(setf *count* 0)

(test (&aux (a (incf *count*)) (b (incf *count*)))
      (a b *count* (setf *count* 0))
      ())

;;;; long-form method combination with &rest in :arguments
;;;; (this had a bug what with fixed in 1.0.4.something)
(define-method-combination long-form-with-&rest ()
  ((methods *))
  (:arguments x &rest others)
  `(progn
     ,@(mapcar (lambda (method)
                 `(call-method ,method))
               methods)
     (list ,x (length ,others))))

(defgeneric test-long-form-with-&rest (x &rest others)
  (:method-combination long-form-with-&rest))

(defmethod test-long-form-with-&rest (x &rest others)
  others
  nil)

(with-test (:name (define-method-combination :long-form-with-&rest))
  (assert (equal '(:foo 13)
                 (apply #'test-long-form-with-&rest :foo (make-list 13)))))

;;;; slot-missing for non-standard classes on SLOT-VALUE
;;;;
;;;; FIXME: This is arguably not right, actually: CLHS seems to say
;;;; we should just signal an error at least for built-in classes, but
;;;; for a while we were hitting NO-APPLICABLE-METHOD, which is definitely
;;;; wrong -- so test this for now at least.

(defvar *magic-symbol* (gensym "MAGIC"))

(set *magic-symbol* 42)

(defmethod slot-missing (class instance (slot-name (eql *magic-symbol*)) op
                         &optional new)
  (if (eq 'setf op)
      (setf (symbol-value *magic-symbol*)  new)
      (symbol-value *magic-symbol*)))

(with-test (:name (slot-missing :non-standard-classes))
  (assert (eql 42 (slot-value (cons t t) *magic-symbol*)))
  (assert (eql 13 (setf (slot-value 123 *magic-symbol*) 13)))
  (assert (eql 13 (slot-value 'foobar *magic-symbol*))))

;;;; Built-in structure and condition layouts should have NIL in
;;;; LAYOUT-FOR-PCL-OBJ-P, and classes should have T.

(with-test (:name (sb-pcl::layout-for-pcl-obj-p :builtin))
  (assert (not (sb-pcl::layout-for-pcl-obj-p (sb-pcl::find-layout 'warning))))
  (assert (not (sb-pcl::layout-for-pcl-obj-p (sb-pcl::find-layout 'hash-table))))
  (assert (eq t (sb-pcl::layout-for-pcl-obj-p (sb-pcl::find-layout 'standard-object)))))

;;;; bug 402: PCL used to warn about non-standard declarations
(declaim (declaration bug-402-d))
(defgeneric bug-402-gf (x))
(with-test (:name (defmethod :non-standard-declaration :bug-402))
  (handler-bind ((warning #'error))
    (eval '(defmethod bug-402-gf (x)
            (declare (bug-402-d x))
            x))))

;;;; non-keyword :default-initargs + :before method on shared initialize
;;;; interacted badly with CTOR optimizations
(defclass ctor-default-initarg-problem ()
  ((slot :initarg slotto))
  (:default-initargs slotto 123))
(defmethod shared-initialize :before ((instance ctor-default-initarg-problem) slot-names &rest initargs)
  (format nil "~&Rock on: ~A~%" initargs))
(defun provoke-ctor-default-initarg-problem ()
  (make-instance 'ctor-default-initarg-problem))
(with-test (:name (make-instance :non-keyword-default-initargs
                   shared-initialize :before))
  (handler-bind ((warning #'error))
    (assert (= 123 (slot-value (provoke-ctor-default-initarg-problem) 'slot)))))

;;;; discriminating net on streams used to generate code deletion notes on
;;;; first call
(defgeneric stream-fd (stream direction))
(defmethod stream-fd ((stream sb-sys:fd-stream) direction)
  (declare (ignore direction))
  (sb-sys:fd-stream-fd stream))
(defmethod stream-fd ((stream synonym-stream) direction)
  (stream-fd (symbol-value (synonym-stream-symbol stream)) direction))
(defmethod stream-fd ((stream two-way-stream) direction)
  (ecase direction
    (:input
     (stream-fd
      (two-way-stream-input-stream stream) direction))
    (:output
     (stream-fd
      (two-way-stream-output-stream stream) direction))))
(with-test (:name (:discriminating-name :code-deletion-note))
  (handler-bind ((compiler-note #'error))
    (stream-fd sb-sys:*stdin* :output)
    (stream-fd sb-sys:*stdin* :output)))

(with-test (:name :bug-380)
  (defclass bug-380 ()
    ((slot :accessor bug380-slot)))
  (fmakunbound 'foo-slot)
  (defgeneric foo-slot (x y z))
  (defclass foo ()
    ((slot :accessor foo-slot-value))))

;;; SET and (SETF SYMBOL-VALUE) used to confuse permuation vector
;;; optimizations
(defclass fih ()
  ((x :initform :fih)))
(defclass fah ()
  ((x :initform :fah)))
(declaim (special *fih*))
(defmethod fihfah ((*fih* fih))
  (set '*fih* (make-instance 'fah))
  (list (slot-value *fih* 'x)
        (eval '(slot-value *fih* 'x))))
(defmethod fihfah ((fah fah))
  (declare (special fah))
  (set 'fah (make-instance 'fih))
  (list (slot-value fah 'x)
        (eval '(slot-value fah 'x))))
(with-test (:name :set-of-a-method-specializer)
  (assert (equal '(:fah :fah) (fihfah (make-instance 'fih))))
  (assert (equal '(:fih :fih) (fihfah (make-instance 'fah)))))

(defmethod no-implicit-declarations-for-local-specials ((faax double-float))
  (declare (special faax))
  (set 'faax (when (< faax 0) (- faax)))
  faax)
(with-test (:name :no-implicit-declarations-for-local-specials)
  (assert (not (no-implicit-declarations-for-local-specials 1.0d0))))

(defstruct bug-357-a
  slot1
  (slot2 t)
  (slot3 (coerce pi 'single-float) :type single-float))
(defclass bug-357-b (bug-357-a)
  ((slot2 :initform 't2)
   (slot4 :initform -44)
   (slot5)
   (slot6 :initform t)
   (slot7 :initform (floor (* pi pi)))
   (slot8 :initform 88))
  (:metaclass structure-class))
(defstruct (bug-357-c (:include bug-357-b (slot8 -88) (slot5 :ok)))
  slot9
  (slot10 t)
  (slot11 (floor (exp 3))))
(with-test (:name :bug-357)
  (flet ((slots (x)
           (list (bug-357-c-slot1 x)
                 (bug-357-c-slot2 x)
                 (bug-357-c-slot3 x)
                 (bug-357-c-slot4 x)
                 (bug-357-c-slot5 x)
                 (bug-357-c-slot6 x)
                 (bug-357-c-slot7 x)
                 (bug-357-c-slot8 x)
                 (bug-357-c-slot9 x)
                 (bug-357-c-slot10 x)
                 (bug-357-c-slot11 x))))
    (let ((base (slots (make-bug-357-c))))
      (assert (equal base (slots (make-instance 'bug-357-c))))
      (assert (equal base '(nil t2 3.1415927 -44 :ok t 9 -88 nil t 20))))))

(defclass class-slot-shared-initialize ()
  ((a :allocation :class :initform :ok)))
(with-test (:name :class-slot-shared-initialize)
  (let ((x (make-instance 'class-slot-shared-initialize)))
    (assert (eq :ok (slot-value x 'a)))
    (slot-makunbound x 'a)
    (assert (not (slot-boundp x 'a)))
    (shared-initialize x '(a))
    (assert (slot-boundp x 'a))
    (assert (eq :ok (slot-value x 'a)))))

(declaim (ftype (function (t t t) (values single-float &optional))
                i-dont-want-to-be-clobbered-1
                i-dont-want-to-be-clobbered-2))
(handler-bind ((warning #'muffle-warning))
(defgeneric i-dont-want-to-be-clobbered-1 (x y z))
(defmethod i-dont-want-to-be-clobbered-2 ((x cons) y z)
  y))
(defun i-cause-an-gf-info-update ()
  (i-dont-want-to-be-clobbered-2 t t t))
(with-test (:name (defgeneric :should-clobber-ftype))
  ;; (because it doesn't check the argument or result types)
  (assert (equal '(function (t t t) *)
                 (sb-kernel:type-specifier
                  (sb-int:info :function
                               :type 'i-dont-want-to-be-clobbered-1))))
  (assert (equal '(function (t t t) *)
                 (sb-kernel:type-specifier
                  (sb-int:info :function
                               :type 'i-dont-want-to-be-clobbered-2))))
  (assert (eq :defined-method
              (sb-int:info :function
                           :where-from 'i-dont-want-to-be-clobbered-1)))
  (assert (eq :defined-method
              (sb-int:info :function
                           :where-from 'i-dont-want-to-be-clobbered-2))))

(with-test (:name :bogus-parameter-specializer-name-error)
  (assert (eq :ok
              (handler-case
                 (let ((*error-output* (make-broadcast-stream)))
                   (eval `(defmethod #:fii ((x "a string")) 'string)))
                (sb-int:reference-condition (c)
                  (when (member '(:ansi-cl :macro defmethod)
                                (sb-int:reference-condition-references c)
                                :test #'equal)
                    :ok))))))

(defclass remove-default-initargs-test ()
  ((x :initarg :x :initform 42)))
(defclass remove-default-initatgs-test ()
  ((x :initarg :x :initform 42))
  (:default-initargs :x 0))
(defclass remove-default-initargs-test ()
  ((x :initarg :x :initform 42)))
(with-test (:name :remove-default-initargs)
  (assert (= 42 (slot-value (make-instance 'remove-default-initargs-test)
                            'x))))

;; putting this inside WITH-TEST interferes with recognition of the class name
;; as a specializer
(defclass bug-485019 ()
  ((array :initarg :array)))
(with-test (:name :bug-485019)
  ;; there was a bug in WALK-SETQ, used in method body walking, in the
  ;; presence of declarations on symbol macros.
  (defmethod bug-485019 ((bug-485019 bug-485019))
    (with-slots (array) bug-485019
      (declare (type (or null simple-array) array))
      (setf array (make-array 4)))
    bug-485019)
  (funcall 'bug-485019 (make-instance 'bug-485019)))

;;; The compiler didn't propagate the declarared type before applying
;;; the transform for (SETF SLOT-VALUE), so the generic accessor was used.
(defstruct foo-520366
  slot)
(defun quux-520366 (cont)
  (funcall cont))
(defun bar-520366 (foo-struct)
  (declare (type foo-520366 foo-struct))
  (with-slots (slot) foo-struct
    (tagbody
       (quux-520366 #'(lambda ()
                        (setf slot :value)
                        (go TAG)))
     TAG)))
(with-test (:name :bug-520366)
  (let ((callees (find-named-callees #'bar-520366)))
    (assert (equal '(quux-520366) callees))))

(defgeneric no-applicable-method/retry (x))
(defmethod no-applicable-method/retry ((x string))
  "string")
(with-test (:name :no-applicable-method/retry)
  (assert (equal "cons"
                 (handler-bind ((sb-pcl::no-applicable-method-error
                                 (lambda (c)
                                   (declare (ignore c))
                                   (let ((r (find-restart 'sb-pcl::retry)))
                                     (when r
                                       (eval `(defmethod no-applicable-method/retry ((x cons))
                                                "cons"))
                                       (invoke-restart r))))))
                   (no-applicable-method/retry (cons t t))))))

(defgeneric no-primary-method/retry (x))
(defmethod no-primary-method/retry :before (x) (assert x))
(with-test (:name :no-primary-method/retry)
  (assert (equal "ok!"
                 (handler-bind ((sb-pcl::no-primary-method-error
                                 (lambda (c)
                                   (declare (ignore c))
                                   (let ((r (find-restart 'sb-pcl::retry)))
                                     (when r
                                       (eval `(defmethod no-primary-method/retry (x)
                                                "ok!"))
                                       (invoke-restart r))))))
                   (no-primary-method/retry (cons t t))))))

;;; test that a cacheing strategy for make-instance initargs checking
;;; can handle class redefinitions

(defun cacheing-initargs-redefinitions-make-instances
    (&optional (initarg :slot))
  (declare (notinline make-instance))
  (make-instance 'cacheing-initargs-redefinitions-check)
  (make-instance 'cacheing-initargs-redefinitions-check initarg 3))

(defclass cacheing-initargs-redefinitions-check ()
  ((slot :initarg :slot)))

(with-test (:name (make-instance :initargs-checking-before-redefinition))
  (make-instance 'cacheing-initargs-redefinitions-check)
  (make-instance 'cacheing-initargs-redefinitions-check :slot 3)
  (cacheing-initargs-redefinitions-make-instances :slot)
  (assert-error (cacheing-initargs-redefinitions-make-instances :slot2)))

(defclass cacheing-initargs-redefinitions-check ()
  ((slot :initarg :slot2)))

(with-test (:name (make-instance :initargs-checking-after-redefinition))
  (make-instance 'cacheing-initargs-redefinitions-check)
  (make-instance 'cacheing-initargs-redefinitions-check :slot2 3)
  (cacheing-initargs-redefinitions-make-instances :slot2)
  (assert-error (cacheing-initargs-redefinitions-make-instances :slot)))

(defmethod initialize-instance :after
    ((class cacheing-initargs-redefinitions-check) &key slot)
  (declare (ignore slot))
  nil)

(with-test (:name (make-instance :initargs-checking-new-method-initargs))
  (make-instance 'cacheing-initargs-redefinitions-check)
  (make-instance 'cacheing-initargs-redefinitions-check :slot2 3)
  (cacheing-initargs-redefinitions-make-instances :slot2)
  (let ((thing (cacheing-initargs-redefinitions-make-instances :slot)))
    (assert (not (slot-boundp thing 'slot)))))


;;; defmethod tests

(with-test (:name (defmethod :specializer-builtin-class-alias :bug-618387))
  (let ((alias (gensym)))
    (setf (find-class alias) (find-class 'symbol))
    (eval `(defmethod lp-618387 ((s ,alias))
             (symbol-name s)))
    (assert (equal "FOO" (funcall 'lp-618387 :foo)))))

(with-test (:name (defmethod :pcl-spurious-ignore-warnings))
  (defgeneric no-spurious-ignore-warnings (req &key key))
  (handler-bind ((warning (lambda (x) (error "~A" x))))
    (eval
     '(defmethod no-spurious-ignore-warnings ((req number) &key key)
       (declare (ignore key))
       (check-type req integer))))
  (defgeneric should-get-an-ignore-warning (req &key key))
  (let ((warnings 0))
    (handler-bind ((warning (lambda (c) (setq warnings 1) (muffle-warning c))))
      (eval '(defmethod should-get-an-ignore-warning ((req integer) &key key)
              (check-type req integer))))
    (assert (= warnings 1))))

(handler-bind ((style-warning #'muffle-warning))
  (eval '(defgeneric generic-function-pretty-arglist-optional-and-key (req &optional opt &key key)
  (:method (req &optional opt &key key)
    (list req opt key)))))
(with-test (:name :generic-function-pretty-arglist-optional-and-key)
  (handler-bind ((warning #'error))
    ;; Used to signal a style-warning
    (assert (equal '(req &optional opt &key key)
                   (sb-pcl::generic-function-pretty-arglist
                    #'generic-function-pretty-arglist-optional-and-key)))))

(with-test (:name :bug-894202)
  (assert (eq :good
              (handler-case
                  (let ((name (gensym "FOO"))
                        (decl (gensym "BAR")))
                    (eval `(defgeneric ,name ()
                             (declare (,decl)))))
                (warning ()
                  :good)))))

(with-test (:name :bug-898331)
  (handler-bind ((warning #'error))
    (eval `(defgeneric bug-898331 (request type remaining-segment-requests all-requests)))
    (eval `(defmethod bug-898331 ((request cons) (type (eql :cancel))
                                  remaining-segment-requests
                                  all-segment-requests)
             (declare (ignore all-segment-requests))
             (check-type request t)))))

(with-test (:name (defmethod :bug-1001799))
  ;; compilation of the defmethod used to cause infinite recursion
  (let ((pax (gensym "PAX"))
        (pnr (gensym "PNR"))
        (sup (gensym "SUP"))
        (frob (gensym "FROB"))
        (sb-ext:*evaluator-mode* :compile))
    (eval
     `(progn
        (declaim (optimize (speed 1) (space 1) (safety 3) (debug 3) (compilation-speed 1)))
        (defclass ,pax (,sup)
          ((,pnr :type (or null ,pnr))))
        (defclass ,pnr (,sup)
          ((,pax :type (or null ,pax))))
        (defclass ,sup ()
          ())
        (defmethod ,frob ((pnr ,pnr))
          (slot-value pnr ',pax))
        (declaim (optimize (safety 1) (debug 1)))))))

(defclass bug-1099708 () ((slot-1099708 :initarg :slot-1099708)))
(defun make-1099708-1 ()
  (make-instance 'bug-1099708 :slot-1099708 '#1= (1 2 . #1#)))
(defun make-1099708-2 ()
  (make-instance 'bug-1099708 :slot-1099708 '#2= (1 2 . #2#)))
(with-test (:name :bug-1099708)
  ;; caused infinite equal testing in function name lookup
  (assert (not (eql (slot-value (make-1099708-1) 'slot-1099708)
                    (slot-value (make-1099708-2) 'slot-1099708)))))

(defclass bug-1099708b-list ()
  ((slot-1099708b-list :initarg :slot-1099708b-list)))
(defun make-1099708b-list-1 ()
  (make-instance 'bug-1099708b-list :slot-1099708b-list '(some value)))
(defun make-1099708b-list-2 ()
  (make-instance 'bug-1099708b-list :slot-1099708b-list '(some value)))
(with-test (:name :bug-1099708b-list)
  (assert (eql (slot-value (make-1099708b-list-1) 'slot-1099708b-list)
               (slot-value (make-1099708b-list-1) 'slot-1099708b-list)))
  (assert (eql (slot-value (make-1099708b-list-2) 'slot-1099708b-list)
               (slot-value (make-1099708b-list-2) 'slot-1099708b-list)))
  (assert (not (eql (slot-value (make-1099708b-list-1) 'slot-1099708b-list)
                    (slot-value (make-1099708b-list-2) 'slot-1099708b-list)))))

(defclass bug-1099708b-string ()
  ((slot-1099708b-string :initarg :slot-1099708b-string)))
(defun make-1099708b-string-1 ()
  (make-instance 'bug-1099708b-string :slot-1099708b-string "string"))
(defun make-1099708b-string-2 ()
  (make-instance 'bug-1099708b-string :slot-1099708b-string "string"))
(with-test (:name :bug-1099708b-string)
  (assert (eql (slot-value (make-1099708b-string-1) 'slot-1099708b-string)
               (slot-value (make-1099708b-string-1) 'slot-1099708b-string)))
  (assert (eql (slot-value (make-1099708b-string-2) 'slot-1099708b-string)
               (slot-value (make-1099708b-string-2) 'slot-1099708b-string)))
  (assert (not (eql (slot-value (make-1099708b-string-1) 'slot-1099708b-string)
                    (slot-value (make-1099708b-string-2) 'slot-1099708b-string)))))

(defclass bug-1099708b-bitvector ()
  ((slot-1099708b-bitvector :initarg :slot-1099708b-bitvector)))
(defun make-1099708b-bitvector-1 ()
  (make-instance 'bug-1099708b-bitvector :slot-1099708b-bitvector #*1011))
(defun make-1099708b-bitvector-2 ()
  (make-instance 'bug-1099708b-bitvector :slot-1099708b-bitvector #*1011))
(with-test (:name :bug-1099708b-bitvector)
  (assert (eql (slot-value (make-1099708b-bitvector-1) 'slot-1099708b-bitvector)
               (slot-value (make-1099708b-bitvector-1) 'slot-1099708b-bitvector)))
  (assert (eql (slot-value (make-1099708b-bitvector-2) 'slot-1099708b-bitvector)
               (slot-value (make-1099708b-bitvector-2) 'slot-1099708b-bitvector)))
  (assert (not (eql (slot-value (make-1099708b-bitvector-1) 'slot-1099708b-bitvector)
                    (slot-value (make-1099708b-bitvector-2) 'slot-1099708b-bitvector)))))

(defclass bug-1099708b-pathname ()
  ((slot-1099708b-pathname :initarg :slot-1099708b-pathname)))
#+nil ; after change 58602640ed, I don't see how to make this assert something useful
(with-test (:name :bug-1099708b-pathname)
  (defun make-1099708b-pathname-1 ()
    (make-instance 'bug-1099708b-pathname :slot-1099708b-pathname #p"pn"))
  (defun make-1099708b-pathname-2 ()
    (make-instance 'bug-1099708b-pathname :slot-1099708b-pathname #p"pn"))
  (assert (eql (slot-value (make-1099708b-pathname-1) 'slot-1099708b-pathname)
               (slot-value (make-1099708b-pathname-1) 'slot-1099708b-pathname)))
  (assert (eql (slot-value (make-1099708b-pathname-2) 'slot-1099708b-pathname)
               (slot-value (make-1099708b-pathname-2) 'slot-1099708b-pathname)))
  (assert (not (eql (slot-value (make-1099708b-pathname-1) 'slot-1099708b-pathname)
                    (slot-value (make-1099708b-pathname-2) 'slot-1099708b-pathname)))))

(defclass bug-1099708c-list ()
  ((slot-1099708c-list :initarg :slot-1099708c-list)))
(progn
  (defun make-1099708c-list-1 ()
    (make-instance 'bug-1099708c-list :slot-1099708c-list #1='(some value)))
  (defun make-1099708c-list-2 ()
    (make-instance 'bug-1099708c-list :slot-1099708c-list #1#)))
(with-test (:name :bug-1099708c-list)
  (assert (eql (slot-value (make-1099708c-list-1) 'slot-1099708c-list)
               (slot-value (make-1099708c-list-1) 'slot-1099708c-list)))
  (assert (eql (slot-value (make-1099708c-list-2) 'slot-1099708c-list)
               (slot-value (make-1099708c-list-2) 'slot-1099708c-list)))
  (assert (eql (slot-value (make-1099708c-list-1) 'slot-1099708c-list)
               (slot-value (make-1099708c-list-2) 'slot-1099708c-list))))

;;; bug-1179858

;;; Define a class and force the "fallback" constructor generator to be
;;; used by having a HAIRY-AROUND-OR-NONSTANDARD-PRIMARY-METHOD-P on
;;; SHARED-INITIALIZE.
(defclass bug-1179858 ()
  ((foo :initarg :foo :reader bug-1179858-foo))
  (:default-initargs :foo (error "Should not be evaluated")))
(defmethod shared-initialize :around ((instance bug-1179858) (slot-names t) &key)
  (call-next-method))

(with-test (:name (make-instance :fallback-generator-initarg-handling :bug-1179858))
  ;; Now compile a lambda containing MAKE-INSTANCE to exercise the
  ;; fallback constructor generator. Call the resulting compiled
  ;; function to trigger the bug.
  (checked-compile-and-assert ()
      '(lambda () (make-instance 'bug-1179858 :foo t))
    (() nil :test (constantly t))))

;;; Other brokenness, found while investigating: fallback-generator
;;; handling of non-keyword initialization arguments
(defclass bug-1179858b ()
  ((foo :initarg foo :reader bug-1179858b-foo))
  (:default-initargs foo 14))
(defmethod shared-initialize :around ((instance bug-1179858b) (slot-names t) &key)
  (call-next-method))

(with-test (:name (make-instance :fallback-generator-non-keyword-initarg :bug-1179858))
  (flet ((foo= (n i) (= (bug-1179858b-foo i) n)))
    (assert
     (foo= 14 (funcall (checked-compile '(lambda () (make-instance 'bug-1179858b))))))
    (assert
     (foo= 15 (funcall (checked-compile '(lambda () (make-instance 'bug-1179858b 'foo 15))))))))

(with-test (:name (:cpl-violation-setup :bug-309076))
  (assert-error
   (progn
     (defclass bug-309076-broken-class (standard-class) ()
       (:metaclass sb-mop:funcallable-standard-class))
     (sb-mop:finalize-inheritance (find-class 'bug-309076-broken-class)))))

(defclass bug-309076-class (standard-class) ())
(with-test (:name (:cpl-violation-irrelevant-class :bug-309076))
  (defmethod sb-mop:validate-superclass ((x bug-309076-class) (y standard-class)) t)
  (assert (typep (make-instance 'bug-309076-class) 'bug-309076-class)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-cltl2)
  (defmethod b ()))

(defmacro macro ()
  (let ((a 20))
    (declare (special a))
    (checked-compile-and-assert ()
        (sb-mop:make-method-lambda
         #'b
         (find-method #'b () ())
         '(lambda () (declare (special a)) a)
         nil)
      (('(1) ()) 20))))

(with-test (:name :make-method-lambda-leakage)
  ;; lambda list of X leaks into the invocation of make-method-lambda
  ;; during code-walking performed by make-method-lambda invoked by
  ;; DEFMETHOD
  (sb-cltl2:macroexpand-all '(defmethod x (a) (macro))))

(with-test (:name (defmethod :undefined-function :bug-503095))
  (flet ((test-load (file)
           (handler-bind
               (((or warning error) #'error))
             (load file))))
    (multiple-value-bind (fasl warnings errorsp)
          (compile-file "bug-503095.lisp" :print nil :verbose nil
                        :output-file (scratch-file-name "fasl"))
      (unwind-protect
           (progn (assert (and fasl (not warnings) (not errorsp)))
                  (test-load fasl))
        (and fasl (delete-file fasl))))
    (test-load "bug-503095-2.lisp")))

(defclass a-633911 ()
    ((x-633911 :initform nil
               :accessor x-633911)))
(defclass b-633911 ()
    ((x-633911 :initform nil
               :accessor x-633911)))
(with-test (:name :accessor-and-plain-method)
  (defmethod x-633911 ((b a-633911)) 10)
  (assert (= (x-633911 (make-instance 'a-633911)) 10)))

(with-test (:name (built-in-class :subclass error))
  (flet ((mapper (c)
           (when (and (class-name c) (typep c 'built-in-class))
             (assert-error (eval `(defclass ,(gensym) (,(class-name c))
                                    ()))))))
    (sb-pcl::map-all-classes #'mapper)))

(defclass slot-value-using-class-a () ())
(defclass slot-value-using-class-b () (x))

(with-test (:name :svuc-with-bad-slotd)
  (let* ((a (make-instance 'slot-value-using-class-a))
         (b (make-instance 'slot-value-using-class-b))
         (slotd (car (sb-mop:class-slots (class-of b)))))
    (assert-error (sb-mop:slot-value-using-class (class-of a) a slotd))
    (assert-error (setf (sb-mop:slot-value-using-class (class-of a) a slotd) t))))

;; A test that the *FGENS* cache works.
;; This is not a test in the CLOS problem domain, but in the implementation
;; of discriminating functions.
(defgeneric f (x y) (:method (x y) (+ x y)))
(defgeneric g (x y) (:method (x y) (* x y)))
(with-test (:name :fgens-sharing)
  (let ((count0 (hash-table-count sb-pcl::*fgens*))
        (foo (f 1 2))
        (count1 (hash-table-count sb-pcl::*fgens*))
        (bar (g 1 2))
        (count2 (hash-table-count sb-pcl::*fgens*)))
    (declare (ignore foo bar))
    (assert (= count0 count1 count2))))


;;; Classes shouldn't be their own direct or indirect superclasses or
;;; metaclasses.

(with-test (:name (sb-mop:ensure-class :class-is-direct-superclass
                   :bug-1418883))
  (assert-error
   (defclass class-with-self-as-superclass (class-with-self-as-superclass) ())))

(with-test (:name (sb-mop:ensure-class :superclass-cycle :bug-1418883))
  ;; These have a superclass cycle from the beginning.
  (defclass class-with-superclass-cycle1 (class-with-superclass-cycle2) ())
  (assert-error
   (defclass class-with-superclass-cycle2 (class-with-superclass-cycle1) ())))

(with-test (:name (sb-mop:ensure-class :self-metaclass))
  ;; These have a metaclass cycle from the beginning.
  (assert-error
   (defclass class-with-self-as-metaclass () ()
     (:metaclass class-with-self-as-metaclass))))

(with-test (:name (sb-pcl::update-class :class-becomes-direct-superclass
                   :bug-1418883))
  (defclass class-with-eventual-self-as-superclass () ())
  ;; Update class to introduce superclass.
  (assert-error
   (defclass class-with-eventual-self-as-superclass
       (class-with-eventual-self-as-superclass) ())))

(with-test (:name (sb-pcl::update-class :superclasses-become-cyclic
                   :bug-1418883))
  ;; Nothing wrong with these.
  (defclass class-with-eventual-superclass-cycle1 () ())
  (defclass class-with-eventual-superclass-cycle2
      (class-with-eventual-superclass-cycle1) ())
  ;; Update first class to introduce the superclass cycle.
  (assert-error
   (defclass class-with-eventual-superclass-cycle1
       (class-with-eventual-superclass-cycle2) ())))

(with-test (:name (sb-pcl::update-class :becomes-self-metaclass))
  (defclass class-with-eventual-self-as-metaclass () ())
  ;; Try to update metaclass to self.
  (assert-error
   (defclass class-with-eventual-self-as-metaclass () ()
     (:metaclass class-with-eventual-self-as-metaclass))))

(with-test (:name :function-keywords)
  (defgeneric function-keywords-test (&key))
  (assert (equal (function-keywords
                  (defmethod function-keywords-test (&key a b)
                    (declare (ignore a b))))
                 '(:a :b))))

(with-test (:name :superclass-finalization)
  (let* ((class1 (gensym "CLASS1-"))
         (class2 (gensym "CLASS2-")))
    (eval `(defclass ,class1 () ()))
    (eval `(defclass ,class2 (,class1) ()))
    (let ((instance (make-instance class2)))
      (sb-mop:finalize-inheritance (find-class class1))
      (assert (not (sb-kernel:layout-invalid (sb-kernel:layout-of instance)))))))

(with-test (:name (allocate-instance :on symbol))
  (let ((class (gensym "CLASS-")))
    (eval `(defclass ,class () ()))
    (checked-compile-and-assert ()
        `(lambda () (allocate-instance ',class))
      (() (condition 'sb-pcl::no-applicable-method-error)))))

(defclass unbound-slot-after-allocation=class ()
  ((abc :allocation :class)
   (d :accessor unbound-slot-after-allocation=class)))

(with-test (:name :unbound-slot-after-allocation=class)
  (assert-error (unbound-slot-after-allocation=class
                 (make-instance 'unbound-slot-after-allocation=class))
                unbound-slot))

(with-test (:name :layouf-of-nil)
  (assert (eq (sb-kernel:layout-of nil) (sb-kernel:find-layout 'null))))

(with-test (:name (defmethod :on-classless-type))
  (handler-bind ((timeout (lambda (condition)
                            (declare (ignore condition))
                            (error "Timeout"))))
    (sb-ext:with-timeout 10
      (checked-compile-and-assert (:allow-warnings t)
          `(lambda ()
             (defmethod foo ((bar keyword))))
        (() (condition 'sb-pcl:class-not-found-error))))))

(defclass removing-a-class () ())

(defvar *removing-a-class* (sb-ext:make-weak-pointer (find-class 'removing-a-class)))
(setf (find-class 'removing-a-class) nil)
(sb-mop:remove-direct-subclass (find-class 'standard-object)
                               (sb-ext:weak-pointer-value *removing-a-class*))

(with-test (:name :removing-a-class :fails-on :permgen)
  (sb-ext:gc :full t)
  (assert (not (sb-ext:weak-pointer-value *removing-a-class*))))

(locally (declare (optimize safety))
  (defclass setf-slot-value-restart ()
    ((a :type integer
        :accessor setf-slot-value-restart-a))))

(with-test (:name :setf-slot-value-restart
                  :skipped-on :sparc) ; hangs
  (let ((instance (make-instance 'setf-slot-value-restart)))
    (handler-bind ((type-error
                     (lambda (c) (use-value 1 c))))
      (setf (slot-value instance 'a) 'x))
    (assert (eql (slot-value instance 'a) 1))
    (handler-bind ((type-error
                     (lambda (c) (use-value 2 c))))
      (setf (setf-slot-value-restart-a instance) 'y))
    (assert (eql (setf-slot-value-restart-a instance) 2))))

(with-test (:name :super-class-cycle-forward-referenced)
  (defclass super-class-cycle-forward-referenced-a (super-class-cycle-forward-referenced-b) ())
  (assert (handler-case
              (not (defclass super-class-cycle-forward-referenced-b
                       (super-class-cycle-forward-referenced-a)
                     ()))
            (error (c)
              c)))
  (assert (null (sb-mop:class-direct-subclasses (find-class 'super-class-cycle-forward-referenced-a))))
  (defclass super-class-cycle-forward-referenced-b () ()))
