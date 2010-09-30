;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defpackage :sb-introspect-test
  (:use "SB-INTROSPECT" "CL" "SB-RT"))

(in-package :sb-introspect-test)

(deftest function-lambda-list.1
    (function-lambda-list 'cl-user::one)
  (cl-user::a cl-user::b cl-user::c))

(deftest function-lambda-list.2
    (function-lambda-list 'the)
  (sb-c::value-type sb-c::form))

(deftest function-lambda-list.3
    (function-lambda-list #'(sb-pcl::slow-method cl-user::j (t)))
  (sb-pcl::method-args sb-pcl::next-methods))

(deftest definition-source-plist.1
    (let* ((source (find-definition-source #'cl-user::one))
           (plist (definition-source-plist source)))
      (values (= (definition-source-file-write-date source)
                 (file-write-date "test.lisp"))
              (or (equal (getf plist :test-outer)
                         "OUT")
                  plist)))
  t t)

(deftest definition-source-plist.2
    (let ((plist (definition-source-plist
                     (find-definition-source #'cl-user::four))))
      (values (or (equal (getf plist :test-outer) "OUT")
                  plist)
              (or (equal (getf plist :test-inner) "IN")
                  plist)))
  t t)

(defun matchp (object form-number)
  (let ((ds (sb-introspect:find-definition-source object)))
    (and (pathnamep (sb-introspect:definition-source-pathname ds))
         (= form-number
            (first (sb-introspect:definition-source-form-path ds))))))

(defun matchp-name (type object form-number)
  (let ((ds (car (sb-introspect:find-definition-sources-by-name object type))))
    (and (pathnamep (sb-introspect:definition-source-pathname ds))
         (= form-number
            (first (sb-introspect:definition-source-form-path ds))))))

(defun matchp-length (type object form-numbers)
  (let ((ds (sb-introspect:find-definition-sources-by-name object type)))
    (= (length ds) form-numbers)))

(deftest find-source-stuff.1
    (matchp-name :function 'cl-user::one 2)
  t)

(deftest find-source-stuff.2
    (matchp #'cl-user::one 2)
  t)

(deftest find-source-stuff.3
    (matchp-name :generic-function 'cl-user::two 3)
  t)

(deftest find-source-stuff.4
    (matchp (car (sb-pcl:generic-function-methods #'cl-user::two)) 4)
  t)

(deftest find-source-stuff.5
    (matchp-name :variable 'cl-user::*a* 8)
  t)

(deftest find-source-stuff.6
    (matchp-name :variable 'cl-user::*b* 9)
  t)

(deftest find-source-stuff.7
    (matchp-name :class 'cl-user::a 10)
  t)

(deftest find-source-stuff.8
    (matchp-name :condition 'cl-user::b 11)
  t)

(deftest find-source-stuff.9
    (matchp-name :structure 'cl-user::c 12)
  t)

(deftest find-source-stuff.10
    (matchp-name :function 'cl-user::make-c 12)
  t)

(deftest find-source-stuff.11
    (matchp-name :function 'cl-user::c-e 12)
  t)

(deftest find-source-stuff.12
    (matchp-name :structure 'cl-user::d 13)
  t)

(deftest find-source-stuff.13
    (matchp-name :function 'cl-user::make-d 13)
  t)

(deftest find-source-stuff.14
    (matchp-name :function 'cl-user::d-e 13)
  t)

(deftest find-source-stuff.15
    (matchp-name :package 'cl-user::e 14)
  t)

(deftest find-source-stuff.16
    (matchp-name :symbol-macro 'cl-user::f 15)
  t)

(deftest find-source-stuff.17
    (matchp-name :type 'cl-user::g 16)
  t)

(deftest find-source-stuff.18
    (matchp-name :constant 'cl-user::+h+ 17)
  t)

(deftest find-source-stuff.19
    (matchp-length :method 'cl-user::j 2)
  t)

(deftest find-source-stuff.20
    (matchp-name :macro 'cl-user::l 20)
  t)

(deftest find-source-stuff.21
    (matchp-name :compiler-macro 'cl-user::m 21)
  t)

(deftest find-source-stuff.22
    (matchp-name :setf-expander 'cl-user::n 22)
  t)

(deftest find-source-stuff.23
    (matchp-name :function  '(setf cl-user::o) 23)
  t)

(deftest find-source-stuff.24
    (matchp-name :method  '(setf cl-user::p) 24)
  t)

(deftest find-source-stuff.25
    (matchp-name :macro  'cl-user::q 25)
  t)


(deftest find-source-stuff.26
    (matchp-name :method-combination 'cl-user::r 26)
  t)


(deftest find-source-stuff.27
    (matchp-name :setf-expander 'cl-user::s 27)
  t)

(deftest find-source-stuff.28
    (let ((fin (make-instance 'sb-mop:funcallable-standard-object)))
      (sb-mop:set-funcallable-instance-function fin #'cl-user::one)
      (matchp fin 2))
  t)

(deftest find-source-stuff.29
    (unwind-protect
         (progn
           (sb-profile:profile cl-user::one)
           (matchp-name :function 'cl-user::one 2))
      (sb-profile:unprofile cl-user::one))
  t)

(deftest find-source-stuff.30
    ;; Test finding a type that isn't one
    (not (find-definition-sources-by-name 'fboundp :type))
  t)

;;; Check wrt. interplay of generic functions and their methods.

(defgeneric xuuq (gf.a gf.b          &rest gf.rest &key gf.k-X))
(defmethod  xuuq ((m1.a number) m1.b &rest m1.rest &key gf.k-X m1.k-Y m1.k-Z)
  (declare (ignore m1.a m1.b m1.rest gf.k-X m1.k-Y m1.k-Z))
  'm1)
(defmethod  xuuq ((m2.a string) m2.b &rest m2.rest &key gf.k-X m1.k-Y m2.k-Q)
  (declare (ignore m2.a m2.b m2.rest gf.k-X m1.k-Y m2.k-Q))
  'm2)

;; XUUQ's lambda list should look similiar to
;;
;;    (GF.A GF.B &REST GF.REST &KEY GF.K-X M1.K-Z M1.K-Y M2.K-Q)
;;
(deftest gf-interplay.1
    (multiple-value-bind (required optional restp rest keyp keys allowp
                                auxp aux morep more-context more-count)
        (sb-int:parse-lambda-list (function-lambda-list #'xuuq))
      (and (equal required '(gf.a gf.b))
           (null optional)
           (and restp (eql rest 'gf.rest))
           (and keyp
                (member 'gf.k-X keys)
                (member 'm1.k-Y keys)
                (member 'm1.k-Z keys)
                (member 'm2.k-Q keys))
           (not allowp)
           (and (not auxp) (null aux))
           (and (not morep) (null more-context) (not more-count))))
  t)

;;; Check what happens when there's no explicit DEFGENERIC.

(defmethod kroolz (r1 r2 &optional opt &aux aux)
  (declare (ignore r1 r2 opt aux))
  'kroolz)

(deftest gf-interplay.2
    (equal (function-lambda-list #'kroolz) '(r1 r2 &optional opt))
  t)

;;;; Check correctness of DEFTYPE-LAMBDA-LIST.
(deftype foobar-type
    (&whole w &environment e r1 r2 &optional o &rest rest &key k1 k2 k3)
  (declare (ignore w e r1 r2 o rest k1 k2 k3))
  nil)

(deftest deftype-lambda-list.1
    (deftype-lambda-list 'foobar-type)
  (&whole w &environment e r1 r2 &optional o &rest rest &key k1 k2 k3)
  t)

(deftest deftype-lambda-list.2
    (deftype-lambda-list (gensym))
  nil
  nil)

;; ARRAY is a primitive type with associated translator function.
(deftest deftype-lambda-list.3
    (deftype-lambda-list 'array)
  (&optional (sb-kernel::element-type '*) (sb-kernel::dimensions '*))
  t)

;; VECTOR is a primitive type that is defined by means of DEFTYPE.
(deftest deftype-lambda-list.4
    (deftype-lambda-list 'vector)
  (&optional sb-kernel::element-type sb-kernel::size)
  t)

;;; Test allocation-information

(defun tai (x kind info &key ignore)
  (multiple-value-bind (kind2 info2) (sb-introspect:allocation-information x)
    (unless (eq kind kind2)
      (error "wanted ~S, got ~S" kind kind2))
    (when (not (null ignore))
      (setf info2 (copy-list info2))
      (dolist (key ignore)
        (remf info2 key))
      (setf info (copy-list info))
      (dolist (key ignore)
        (remf info key)))
    (equal info info2)))

(deftest allocation-infromation.1
    (tai nil :heap '(:space :static))
  t)

(deftest allocation-information.2
    (tai t :heap '(:space :static))
  t)

(deftest allocation-information.3
    (tai 42 :immediate nil)
  t)

;;; Skip the whole damn test on GENCGC PPC -- the combination is just
;;; to flaky for this to make too much sense.
#-(and ppc gencgc)
(deftest allocation-information.4
    #+gencgc
    (tai #'cons :heap
         ;; FIXME: This is the canonical GENCGC result. On PPC we sometimes get
         ;; :LARGE T, which doesn't seem right -- but ignore that for now.
         '(:space :dynamic :generation 6 :write-protected t :boxed t :pinned nil :large nil)
         :ignore (list :page #+ppc :large))
    #-gencgc
    (tai :cons :heap
         ;; FIXME: Figure out what's the right cheney-result. SPARC at least
         ;; has exhibited both :READ-ONLY and :DYNAMIC, which seems wrong.
         '()
         :ignore '(:space))
  t)

#+sb-thread
(deftest allocation-information.thread.1
    (let ((x (list 1 2 3)))
      (declare (dynamic-extent x))
      (tai x :stack sb-thread:*current-thread*))
  t)

#+sb-thread
(progn
   (defun thread-tai ()
     (let ((x (list 1 2 3)))
       (declare (dynamic-extent x))
       (let ((child (sb-thread:make-thread
                     (lambda ()
                       (sb-introspect:allocation-information x)))))
         (equal (list :stack sb-thread:*current-thread*)
                (multiple-value-list (sb-thread:join-thread child))))))

   (deftest allocation-information.thread.2
       (thread-tai)
     t)

   (defun thread-tai2 ()
     (let* ((sem (sb-thread:make-semaphore))
            (obj nil)
            (child (sb-thread:make-thread
                    (lambda ()
                      (let ((x (list 1 2 3)))
                        (declare (dynamic-extent x))
                        (setf obj x)
                        (sb-thread:wait-on-semaphore sem)))
                    :name "child")))
       (loop until obj)
       (unwind-protect
            (equal (list :stack child)
                   (multiple-value-list
                    (sb-introspect:allocation-information obj)))
         (sb-thread:signal-semaphore sem)
         (sb-thread:join-thread child))))

   (deftest allocation-information.thread.3
       (thread-tai2)
     t))

;;;; Test FUNCTION-TYPE

(defun type-equal (typespec1 typespec2)
  (or (equal typespec1 typespec2)   ; TYPE= punts on &keywords in FTYPEs.
      (sb-kernel:type= (sb-kernel:values-specifier-type typespec1)
                       (sb-kernel:values-specifier-type typespec2))))

(defmacro interpret (form)
  `(let ((sb-ext:*evaluator-mode* :interpret))
     (eval ',form)))

;; Functions

(declaim (ftype (function (integer &optional string) string) moon))
(defun moon (int &optional suffix)
  (concatenate 'string (princ-to-string int) suffix))

(deftest function-type.1
    (values (type-equal (function-type 'moon) (function-type #'moon))
            (type-equal (function-type #'moon)
                        '(function (integer &optional string)
                          (values string &rest t))))
  t t)

(defun sun (x y &key k1)
  (declare (fixnum x y))
  (declare (boolean k1))
  (declare (ignore x y k1))
  t)

(deftest function-type.2
    (values (type-equal (function-type 'sun) (function-type #'sun))
            ;; Does not currently work due to Bug #384892. (1.0.31.26)
            #+nil
            (type-equal (function-type #'sun)
                        '(function (fixnum fixnum &key (:k1 (member nil t)))
                          (values (member t) &optional))))
  t #+nil t)

;; Local functions

(deftest function-type.5
    (flet ((f (s)
             (declare (symbol s))
             (values (symbol-name s))))
      (type-equal (function-type #'f)
                  '(function (symbol) (values simple-string &optional))))
  t)

;; Closures

(deftest function-type.6
    (let ((x 10))
      (declare (fixnum x))
      (flet ((closure (y)
               (declare (fixnum y))
               (setq x (+ x y))))
        (type-equal (function-type #'closure)
                    '(function (fixnum) (values fixnum &optional)))))
  t)

;; Anonymous functions

(deftest function-type.7
    (type-equal (function-type #'(lambda (x) (declare (fixnum x)) x))
                '(function (fixnum) (values fixnum &optional)))
  t)

;; Interpreted functions

#+sb-eval
(deftest function-type.8
    (type-equal (function-type (interpret (lambda (x) (declare (fixnum x)) x)))
                '(function (&rest t) *))
  t)

;; Generic functions

(defgeneric earth (x y))

(deftest function-type+gfs.1
    (values (type-equal (function-type 'earth) (function-type #'earth))
            (type-equal (function-type 'earth) '(function (t t) *)))
  t t)

;; Implicitly created generic functions.

;; (FUNCTION-TYPE 'MARS) => FUNCTION at the moment. (1.0.31.26)

;; See LP #520695.

(defmethod mars (x y) (+ x y))

#+ nil
(deftest function-type+gfs.2
    (values (type-equal (function-type 'mars) (function-type #'mars))
            (type-equal (function-type 'mars) '(function (t t) *)))
  t t)

;; DEFSTRUCT created functions

;; These do not yet work because SB-KERNEL:%FUN-NAME does not work on
;; functions defined by DEFSTRUCT. (1.0.35.x)

;; See LP #520692.

#+nil
(progn

  (defstruct (struct (:predicate our-struct-p)
                     (:copier copy-our-struct))
    (a 42 :type fixnum))

  (deftest function-type+defstruct.1
      (values (type-equal (function-type 'struct-a)
                          (function-type #'struct-a))
              (type-equal (function-type 'struct-a)
                          '(function (struct) (values fixnum &optional))))
    t t)

  (deftest function-type+defstruct.2
      (values (type-equal (function-type 'our-struct-p)
                          (function-type #'our-struct-p))
              (type-equal (function-type 'our-struct-p)
                          '(function (t) (values (member t nil) &optional))))
    t t)

  (deftest function-type+defstruct.3
      (values (type-equal (function-type 'copy-our-struct)
                          (function-type #'copy-our-struct))
              (type-equal (function-type 'copy-our-struct)
                          '(function (struct) (values struct &optional))))
    t t)

  (defstruct (typed-struct :named (:type list)
                           (:predicate typed-struct-p))
    (a 42 :type fixnum))

  (deftest function-type+defstruct.4
      (values (type-equal (function-type 'typed-struct-a)
                          (function-type #'typed-struct-a))
              (type-equal (function-type 'typed-struct-a)
                          '(function (list) (values fixnum &optional))))
    t t)

  (deftest function-type+defstruct.5
      (values (type-equal (function-type 'typed-struct-p)
                          (function-type #'typed-struct-p))
              (type-equal (function-type 'typed-struct-p)
                          '(function (t) (values (member t nil) &optional))))
    t t)

  ) ; #+nil (progn ...

;; SETF functions

(defun (setf sun) (value x y &key k1)
  (declare (boolean value))
  (declare (fixnum x y))
  (declare (boolean k1))
  (declare (ignore x y k1))
  value)

(deftest function-type+setf.1
    (values (type-equal (function-type '(setf sun))
                        (function-type #'(setf sun)))
            (type-equal (function-type '(setf sun))
                        '(function ((member nil t)
                                    fixnum fixnum
                                    &key (:k1 (member nil t)))
                          *)))
  t t)

;; Misc

(deftest function-type+misc.1
    (flet ((nullary ()))
      (type-equal (function-type #'nullary)
                  '(function () (values null &optional))))
  t)
