;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(defpackage :sb-introspect-test
  (:import-from #:test-util #:deftest)
  (:use "SB-INTROSPECT" "CL"))

(in-package :sb-introspect-test)

;; When running the tests which query for a function type, sb-interpreter
;; can return an answer if there were type declarations for the arguments,
;; except that return type is always unknown. The compiler returns a
;; definitive answer, and sb-eval always answers with just FUNCTION.
(defun expect-wild-return-type-p (f)
  (declare (ignorable f))
  (or #+sb-fasteval (typep f 'sb-kernel:interpreted-function)))

(deftest function-lambda-list.1
    (function-lambda-list 'cl-user::one)
  (cl-user::a cl-user::b cl-user::c)
  nil)

(deftest function-lambda-list.1a
    (function-lambda-list 'cl-user::0-debug)
  ()
  t)

(deftest function-lambda-list.2
    (function-lambda-list 'the)
  (sb-c::value-type sb-c::form)
  nil)

(deftest function-lambda-list.3
    (function-lambda-list #'(sb-pcl::slow-method cl-user::j (t)))
  (sb-pcl::method-args sb-pcl::next-methods)
  nil)

(deftest macro-lambda-list.1
    (equal (function-lambda-list (defmacro macro-lambda-list.1-m (x b)
                                   `(,x ,b)))
           '(x b))
  t)

(defmacro interpret (form)
  `(let ((sb-ext:*evaluator-mode* :interpret))
     (eval ',form)))

#+sb-eval
(deftest macro-lambda-list.2
    (equal (function-lambda-list (interpret (defmacro macro-lambda-list.2-m (x)
                                              x)))
           '(x))
  t)

(deftest macro-lambda-list.3
    (equal (function-lambda-list (defmacro macro-lambda-list.1-m (x &optional (b "abc"))
                                   `(,x ,b)))
           '(x &optional (b "abc")))
  t)

(deftest macro-lambda-list.4
    (equal (function-lambda-list (defmacro macro-lambda-list.1-m (x &key (b "abc"))
                                   `(,x ,b)))
           '(x &key (b "abc")))
  t)

(test-util:with-test (:name definition-source.1 :skipped-on :no-source-locs)
  #+sb-devel
  (assert (consp (find-definition-sources-by-name 'vectorp :vop)))
  (assert (consp (find-definition-sources-by-name 'check-type :macro))))

(deftest definition-source-plist.1
    (let* ((source (find-definition-source #'cl-user::one))
           (plist (definition-source-plist source))
           (pathname (definition-source-pathname source)))
      ;; the full pathname isn't important
      (values (equalp (pathname-name pathname) "TEST")
              (or (= (definition-source-file-write-date source)
                     (file-write-date pathname))
                  ;; this env var can cause the preceding = comparison to fail.
                  (not (null (sb-ext:posix-getenv "SOURCE_DATE_EPOCH"))))
              (or (equal (getf plist :test-outer)
                         "OUT")
                  plist)))
  t t t)

;;; The behavior of :SOURCE-PLIST on nested WITH-COMPILATION-UNIT
;;; is to append. This is documented in source/compiler/main
(test-util:with-test (:name :definition-source-plist.2)
    (let ((plist (definition-source-plist
                     (find-definition-source #'cl-user::four))))
      (assert (equal (getf plist :test-outer) "OUT"))
      (assert (equal (getf plist :test-inner) "IN"))))

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

(test-util:with-test (:name find-source-stuff.3 :skipped-on :no-source-locs)
  (assert (matchp-name :generic-function 'cl-user::two 3)))

(test-util:with-test (:name find-source-stuff.4 :skipped-on :no-source-locs)
  (assert (matchp (car (sb-mop:generic-function-methods #'cl-user::two)) 4)))

(test-util:with-test (:name find-source-stuff.5 :skipped-on :no-source-locs)
  (assert (matchp-name :variable 'cl-user::*a* 8)))

(test-util:with-test (:name find-source-stuff.6 :skipped-on :no-source-locs)
  (assert (matchp-name :variable 'cl-user::*b* 9)))

(test-util:with-test (:name find-source-stuff.7 :skipped-on :no-source-locs)
  (assert (matchp-name :class 'cl-user::a 10)))

(test-util:with-test (:name find-source-stuff.8 :skipped-on :no-source-locs)
  (assert (matchp-name :condition 'cl-user::b 11)))

(test-util:with-test (:name find-source-stuff.9 :skipped-on :no-source-locs)
  (assert (matchp-name :structure 'cl-user::c 12)))

(deftest find-source-stuff.10
    (matchp-name :function 'cl-user::make-c 12)
  t)

(deftest find-source-stuff.11
    (matchp-name :function 'cl-user::c-e 12)
  t)

(test-util:with-test (:name find-source-stuff.12 :skipped-on :no-source-locs)
  (assert (matchp-name :structure 'cl-user::d 13)))

(deftest find-source-stuff.13
    (matchp-name :function 'cl-user::make-d 13)
  t)

(deftest find-source-stuff.14
    (matchp-name :function 'cl-user::d-e 13)
  t)

(test-util:with-test (:name find-source-stuff.15 :skipped-on :no-source-locs)
  (assert (matchp-name :package 'cl-user::e 14)))

(test-util:with-test (:name find-source-stuff.16 :skipped-on :no-source-locs)
  (assert (matchp-name :symbol-macro 'cl-user::f 15)))

(test-util:with-test (:name find-source-stuff.17 :skipped-on :no-source-locs)
  (assert (matchp-name :type 'cl-user::g 16)))

(test-util:with-test (:name find-source-stuff.18 :skipped-on :no-source-locs)
  (assert (matchp-name :constant 'cl-user::+h+ 17)))

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
    (matchp-name :function '(setf cl-user::o) 23)
  t)

(test-util:with-test (:name find-source-stuff.24 :skipped-on :no-source-locs)
  (assert (matchp-name :method '(setf cl-user::p) 24)))

(deftest find-source-stuff.25
    (matchp-name :macro  'cl-user::q 25)
  t)


(test-util:with-test (:name find-source-stuff.26 :skipped-on :no-source-locs)
  (assert (matchp-name :method-combination 'cl-user::r 26)))


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

(deftest find-source-stuff.31
    (matchp-name :function 'cl-user::compile-time-too-fun 28)
  t)

(load "../contrib/sb-introspect/load-test.lisp")
(deftest find-source-stuff.32
    (matchp-name :function 'cl-user::loaded-as-source-fun 3)
  t)

(test-util:with-test (:name find-source-stuff.33 :skipped-on :no-source-locs)
  (assert (matchp-name :variable 'cl-user::**global** 29)))

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
    (multiple-value-bind (llks required optional rest keys aux more)
        (sb-int:parse-lambda-list (function-lambda-list #'xuuq))
      (and (equal required '(gf.a gf.b))
           (null optional)
           (eq (car rest) 'gf.rest)
           (and (sb-int:ll-kwds-keyp llks)
                (member 'gf.k-X keys)
                (member 'm1.k-Y keys)
                (member 'm1.k-Z keys)
                (member 'm2.k-Q keys))
           (not (sb-int:ll-kwds-allowp llks))
           (null aux)
           (null more)))
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
  (declare (ignore w e r1 r2 o rest k1 k2 k3)
           (sb-ext:muffle-conditions sb-kernel:&optional-and-&key-in-lambda-list))
  nil)

(deftest deftype-lambda-list.1
    (deftype-lambda-list 'foobar-type)
  (r1 r2 &optional o &rest rest &key k1 k2 k3)
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

;;; Check correctness of METHOD-COMBINATION-LAMBDA-LIST
(deftest method-combination-lambda-list.1
    (method-combination-lambda-list 'standard)
  nil)

(deftest method-combination-lambda-list.2
    (method-combination-lambda-list
     (sb-mop:find-method-combination #'documentation 'cl-user::r nil))
  (&optional (sb-pcl::order :most-specific-first)))

(declaim (sb-ext:muffle-conditions style-warning))
(define-method-combination long-form-mc (foo &rest args &key bar) ())

(deftest method-combination-lambda-list.3
    (method-combination-lambda-list 'long-form-mc)
  (foo &rest args &key bar))


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


(deftest allocation-information.1
    (tai nil :heap '(:space :static))
  t)

(deftest allocation-information.2
    (tai t :heap '(:space :static))
  t)

#+immobile-space
(deftest allocation-information.2b
    (tai '*print-base* :heap '(:space :immobile))
  t)

(deftest allocation-information.3
    (tai 42 :immediate nil)
  t)

#+x86-64
(deftest allocation-information.3b
    (tai 42s0 :immediate nil)
  t)

(test-util:with-test (:name :allocation-information.4
           ;; Ignored as per the comment above, even though it seems
           ;; unlikely that this is the right condition.
           :fails-on (or :ppc64 (and :sparc :gencgc)))
    (tai (make-list 1) :heap
         `(:space :dynamic :boxed t :large nil)
         :ignore (list :page :pinned :generation :write-protected)))

(setq sb-ext:*evaluator-mode* :compile)
(sb-ext:defglobal *large-obj* nil)

#+(and generational (or riscv x86 x86-64 ppc) (not win32) (not ubsan))
(progn
  (setq *print-array* nil)
  (setq *large-obj* (make-array (* sb-vm:gencgc-page-bytes 4)
                                :element-type '(unsigned-byte 8)))
  (sb-ext:gc :gen 1) ; Array won't move to a large unboxed page until GC'd
  (test-util:with-test (:name allocation-information.5
                        :skipped-on :mark-region-gc) ; doesn't move to an unboxed page
          (tai *large-obj* :heap
               `(:space :dynamic :generation 1 :boxed nil :pinned nil :large t)
               :ignore (list :page :write-protected))
      t))

(defun page-and-gen (thing)
  (let ((props (nth-value 1 (allocation-information thing))))
    (values (getf props :page)
            (getf props :generation))))

(defun assert-large-page/gen/boxedp (thing-name page gen boxedp)
  (sb-ext:gc :gen gen)
  (let ((props (nth-value 1 (allocation-information (symbol-value thing-name)))))
    ;; Don't leave any pointers to THING from the stack.
    ;; Without it, the next test iteration (after next GC) will fail.
    (sb-sys:scrub-control-stack)
    ;; Check that uncopyableness isn't due to pin,
    ;; or else the test proves nothing.
    (and (eq (getf props :pinned :missing) nil)
         (eq (getf props :large) t)
         (= (getf props :page) page)
         (= (getf props :generation) gen)
         (eq (getf props :boxed :missing) boxedp))))

(defun alloc-large-code ()
  ;; large objets have to have to be at least 4 GC pages
  (let ((bytes (* 4 sb-vm:gencgc-page-bytes)))
    ;; For 32-bit in order to force allocation into a large-object page, the currently
    ;; open code region has to be closed. Otherwise the allocation might fit in the region.
    #-64-bit
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "gc_close_region"
                            (function sb-alien:void sb-alien:unsigned sb-alien:unsigned))
     (+ (* 3 3 sb-vm:n-word-bytes) ; KLUDGE: a region is 3 words, and code_region
                                        ; is the third in the array of regions
        (sb-sys:find-dynamic-foreign-symbol-address "gc_alloc_region"))
     7) ; KLUDGE: PAGE_TYPE_CODE
    ;; A legal code object needs >= 4 boxed words. Let's use 8
    (let ((object (sb-c:allocate-code-object nil 8 (- bytes (* 8 sb-vm:n-word-bytes)))))
      (setq *large-obj* object)
      ;; assert that it's large, otherwise the entire test is bogus
      (let ((props (nth-value 1 (allocation-information object))))
        (assert (getf props :large)))
      object)))
(compile 'alloc-large-code)

#+generational
(deftest allocation-information.6
    ;; Remember, all tests run after all toplevel forms have executed,
    ;; so if this were (DEFGLOBAL *LARGE-CODE* ... ) or something,
    ;; the garbage collection explicitly requested for ALLOCATION-INFORMATION.5
    ;; would have already happened, and thus affected this test as well.
    ;; So we need to make the objects within each test,
    ;; while avoiding use of lexical vars that would cause conservative pinning.
    (multiple-value-bind (page gen) (page-and-gen (alloc-large-code))
      (declare (ignorable gen))
      (declare (notinline format))
      (format (make-string-output-stream) "~%")
      (loop for i from 1 to sb-vm:+highest-normal-generation+
            always (assert-large-page/gen/boxedp '*large-obj* page i t)))
  t)
(sb-ext:defglobal *b* nil)
(sb-ext:defglobal *negb* nil)
(sb-ext:defglobal *small-bignum* nil)
(defun get-small-bignum-allocation-information ()
  (setq *small-bignum* (+ (+ *b* (ash 1 100)) *negb*))
  (nth-value 1 (allocation-information *small-bignum*)))

#-(or mark-region-gc gc-stress)
(deftest allocation-information.7
    (locally
      (declare (notinline format))
      ;; Create a bignum using 4 GC pages
      (setq *b* (ash 1 (* sb-vm:gencgc-page-bytes sb-vm:n-byte-bits 4)))
      (setq *negb* (- *b*))
      (and (let ((props (get-small-bignum-allocation-information)))
             ;; *SMALL-BIGNUM* was created as a large boxed object
             (and (eq (getf props :large) t)
                  (eq (getf props :boxed) t)))
           (multiple-value-bind (page gen) (page-and-gen *b*)
             (format (make-string-output-stream) "~%")
             (loop for i from 1 to sb-vm:+highest-normal-generation+
                   always
                   (and (assert-large-page/gen/boxedp '*b* page i nil)
                        (let ((props (nth-value 1 (allocation-information *small-bignum*))))
                          ;; Scrub away the ref to *small-bignum* by making a random call
                          (format (make-broadcast-stream) "~S" props)
                          ;; Assert that *SMALL-BIGNUM* got moved to a small unboxed page
                          (and (not (getf props :pinned :fail))
                               (not (getf props :large :fail))
                               (not (getf props :boxed :fail)))))))))
  t)

#.(if (and (eq sb-ext:*evaluator-mode* :compile) (member :sb-thread *features*))
'(deftest allocation-information.thread.1
    (let ((x (list 1 2 3)))
      (declare (dynamic-extent x))
      (tai x :stack sb-thread:*current-thread*))
  t)
(values))

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
            (type-equal (function-type #'sun)
                        '(function (fixnum fixnum &key (:k1 (member nil t)))
                          (values (member t) &optional))))
  t t)

;; Local functions

(deftest function-type.5
    (flet ((f (s)
             (declare (symbol s))
             (values (symbol-name s))))
      (type-equal (function-type #'f)
                  (if (expect-wild-return-type-p #'f)
                      '(function (symbol) *)
                      '(function (symbol) (values #+sb-unicode simple-string
                                                  #-sb-unicode simple-base-string
                                                  &optional)))))
  t)

;; Closures

(deftest function-type.6
    (let ((x 10))
      (declare (fixnum x))
      (flet ((closure (y)
               (declare (fixnum y))
               (setq x (+ x y))))
        (type-equal (function-type #'closure)
                    (if (expect-wild-return-type-p #'closure)
                        '(function (fixnum) *)
                        '(function (fixnum) (values fixnum &optional))))))
  t)

;; Anonymous functions

(deftest function-type.7
  (let ((f #'(lambda (x) (declare (fixnum x)) x)))
    (type-equal (function-type f)
                (if (expect-wild-return-type-p f)
                    '(function (fixnum) *)
                    '(function (fixnum) (values fixnum &optional)))))
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

  )

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
                          (values (member nil t) &optional))))
  t t)

;; Misc

(deftest function-type+misc.1
    (flet ((nullary ()))
      (type-equal (function-type #'nullary)
                  (if (expect-wild-return-type-p #'nullary)
                      '(function () *)
                      '(function () (values null &optional)))))
  t)

;;; Defstruct accessor, copier, and predicate

(deftest defstruct-fun-sources
  (let ((copier (find-definition-source #'cl-user::copy-three))
        (accessor (find-definition-source #'cl-user::three-four))
        (predicate (find-definition-source #'cl-user::three-p)))
    (values (and (equalp copier accessor)
                 (equalp copier predicate))
            (equal "test.lisp"
                   (file-namestring (definition-source-pathname copier)))
            (equal '(5)
                   (definition-source-form-path copier))))
  t
  t
  t)

(deftest defstruct-fun-sources-by-name
  (let ((copier (car (find-definition-sources-by-name 'cl-user::copy-three :function)))
        (accessor (car (find-definition-sources-by-name 'cl-user::three-four :function)))
        (predicate (car (find-definition-sources-by-name 'cl-user::three-p :function))))
    (values (and (equalp copier accessor)
                 (equalp copier predicate))
            (equal "test.lisp"
                   (file-namestring (definition-source-pathname copier)))
            (equal '(5)
                   (definition-source-form-path copier))))
  t
  t
  t)

(test-util:with-test (:name alien-type.1 :skipped-on :no-source-locs)
  (assert (matchp-name :alien-type 'cl-user::test-alien-type 30)))

(test-util:with-test (:name alien-type.2 :skipped-on :no-source-locs)
  (assert (matchp-name :alien-type 'cl-user::test-alien-struct 31)))

(test-util:with-test (:name alien-variable :skipped-on :no-source-locs)
  (assert (matchp-name :variable 'cl-user::test-alien-var 32)))

(test-util:with-test (:name condition-slot-reader :skipped-on :no-source-locs)
  (matchp-name :method 'cl-user::condition-slot-reader 33))

(test-util:with-test (:name condition-slot-writer :skipped-on :no-source-locs)
  (matchp-name :method 'cl-user::condition-slot-writer 33))

(deftest function-with-a-local-function
    (sb-introspect:definition-source-form-number
     (car (sb-introspect:find-definition-sources-by-name
           'cl-user::with-a-local-function :function)))
  0)

(deftest object-size
    (plusp (sb-introspect::object-size #'print-object))
  t)

;;; ASDF.  I can't even.
(compile 'sb-introspect:map-root)
(defun list-pointees (x simple)
  (sb-int:collect ((result))
    (sb-introspect:map-root (lambda (obj) (result obj))
                            x :simple simple)
    (result)))
(defun count-pointees (x simple)
  (length (list-pointees x simple)))

;;; A closure points to its underlying function, all closed-over values,
;;; and possibly the closure's name.
(deftest map-root-closure-unnamed
    (count-pointees (funcall (compile nil `(lambda (x) (lambda () x))) t) nil)
  2)
;;; (SYMBOL-FUNCTION 'IF) is a named closure over 1 value.
;;; The closed-over value is (:SPECIAL IF), and the name of the closure is
;;; identical, but it also has a docstring.
(deftest map-root-closure-named (count-pointees (symbol-function 'if) nil) 3)

;;; GFs point to their layout, implementation function, and slot vector.
;;; There's also a hash-code which is stored is one of two different ways.
;;; However, in either case we expect only 3 referenced objects,
;;; because due to a strange design choice in MAP-ROOT,
;;; it does not invoke the funarg on fixnums (or characters).
(deftest map-root-gf (count-pointees #'make-load-form nil) 3)

;;; Simple functions should point to their name, type, arglist, info,
;;; and unless SIMPLE is specified to MAP-ROOT, the containing code.
(deftest map-root-function-simple (count-pointees #'car t) 4)
(deftest map-root-function-unsimple (count-pointees #'car nil) 5)

(deftest largest-objects-smoketest
    (let ((*standard-output* (make-broadcast-stream)))
      (sb-introspect::largest-objects))
  nil)

(defun sets-equalp (a b) (and (subsetp a b) (subsetp b a)))
(deftest find-callees
    (sets-equalp (sb-introspect:find-function-callees #'sb-c::find-dominators)
                 (list #'sb-c::dfo-as-needed #'sb-c::number-blocks))
  t)
(deftest find-callers
    (let ((callers (sb-introspect:find-function-callers #'sb-c::dfo-as-needed)))
      (and (>= (length callers) 5)
           (not (null (member #'sb-c::find-dominators callers)))))
  t)
