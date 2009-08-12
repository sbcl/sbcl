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
    (multiple-value-bind (arglist found?) (deftype-lambda-list 'foobar-type)
          (and found?
               (equal arglist '(&whole w &environment e
                                r1 r2 &optional o &rest rest &key k1 k2 k3))))
  t)

(deftest deftype-lambda-list.2
    (equal (multiple-value-list (deftype-lambda-list (gensym)))
           '(nil nil))
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

(deftest allocation-information.4
    #+gencgc
    (tai #'cons :heap
         ;; FIXME: This is the canonical GENCGC result. On PPC we sometimes get
         ;; :LARGE T, which doesn't seem right -- but ignore that for now.
         '(:space :dynamic :generation 6 :write-protected t :pinned nil :large nil)
         :ignore #+ppc '(:large) #-ppc nil)
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
