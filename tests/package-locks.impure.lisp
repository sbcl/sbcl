;;;; package lock tests with side effects

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

#+sb-devel (sb-ext:exit :code 104) ; packages are not locked for devs

(load "compiler-test-util.lisp")

;;;; Our little labrats and a few utilities

(defpackage :test-used)

(defpackage :test-unused)

(defpackage :test-nicknamed)

(defpackage :test-not-nicknamed)

(defpackage :test-aux (:export #:noslot #:noslot2))

(defpackage :test
  (:use :test-used)
  (:shadow #:shadowed)
  (:import-from :cl nil)
  (:export
   #:*special*
   #:*unbound-special*
   #:bound-non-special
   #:car
   #:cdr
   #:class
   #:constant
   #:external
   #:function
   #:macro
   #:noclass
   #:noclass-slot
   #:nocondition
   #:nocondition-slot
   #:nospecial
   #:nostruct
   #:nostruct2
   #:nostruct-slot
   #:nosymbol-macro
   #:notype
   #:num
   #:numfun
   #:shadowed
   #:symbol-macro
   #:unused))

(defvar *uninterned* "UNINTERNED")
(defvar *interned* "INTERNED")

(defun maybe-unintern (name package)
    (let ((s (find-symbol name package)))
      (when s
        (unintern s package))))

(defun set-test-locks (lock-p)
  (dolist (p '(:test :test-aux :test-delete))
    (when (find-package p)
      (if lock-p
          (sb-ext:lock-package p)
          (sb-ext:unlock-package p)))))

(defun reset-test (lock)
  "Reset TEST package to a known state, ensure that TEST-DELETE exists."
  (unless (find-package :test-delete)
    (make-package :test-delete))
  (sb-ext:with-unlocked-packages (:test :test-aux)
    (dolist (s '(test:nosymbol-macro
                 test:noclass test:nostruct test:nostruct2 test:nocondition))
      (makunbound s)
      (unintern s)
      (intern (symbol-name s) :test))
    (rename-package (find-package :test) :test)
    (dolist (nickname (package-local-nicknames :test))
      (remove-package-local-nickname (car nickname) :test))
    (add-package-local-nickname :nicknamed :test-nicknamed :test)
    (unexport (intern "INTERNAL" :test) :test)
    (intern *interned* :test)
    (use-package :test-used :test)
    (export 'test::external :test)
    (unuse-package :test-unused :test)
    (defclass test:class () ())
    (defun test:function () 'test:function)
    (defmacro test:macro () ''test:macro)
    (defparameter test:*special* 'test:*special*)
    (defvar test:*unbound-special*)
    (set 'test:bound-non-special 10)
    (defconstant test:constant 'test:constant)
    (intern "UNUSED" :test)
    (dolist (s '(test:nocondition-slot test:noclass-slot test:nostruct-slot
                 test-aux:noslot test-aux:noslot2))
      (fmakunbound s))
    (ignore-errors (progn
                     (fmakunbound 'test:unused)
                     (makunbound 'test:unused)))
    (maybe-unintern *uninterned* :test)
    (maybe-unintern "NOT-FROM-TEST" :test)
    (defconstant test:num 0)
    (define-symbol-macro test:symbol-macro "SYMBOL-MACRO")
    (defun test:numfun (n) n)
    (defun test:car (cons) (cl:car cons))
    (defun (setf test:cdr) (obj cons) (setf (cl:cdr cons) obj))
    (assert (not (find-symbol *uninterned* :test))))
  (set-test-locks lock))

(defun tmp-fmakunbound (x)
  "FMAKUNDBOUND x, then restore the original binding."
  (let ((f (fdefinition x)))
    (fmakunbound x)
    (ignore-errors (setf (fdefinition x) f))))

(defmacro with-error-info ((string &rest args) &body forms)
  `(handler-bind ((error (lambda (e)
                           (declare (ignorable e))
                           (format t ,string ,@args)
                           (finish-output))))
     (progn ,@forms)))

;;;; Test cases

;;; A collection of forms that are legal both with and without package
;;; locks.
(defvar *legal-forms*
  '(;; package alterations that don't actually mutate the package
    (intern *interned* :test)
    (import 'test:unused :test)
    (shadowing-import 'test:shadowed :test)
    (export 'test:unused :test)
    (unexport 'test::internal :test)
    (let ((p (find-package :test)))
      (rename-package p :test))
    (use-package :test-used :test)
    (unuse-package :test-unused :test)
    (shadow "SHADOWED" :test)
    (let ((s (with-unlocked-packages (:test)
               (let ((s (intern *uninterned* :test)))
                 (unintern s :test)
                 s))))
      (unintern s :test))

    ;; binding and altering value
    (let ((test:function 123))
      (assert (eql test:function 123)))
    (let ((test:*special* :foo))
      (assert (eql test:*special* :foo)))
    (progn
      (setf test:*special* :quux)
      (assert (eql test:*special* :quux)))
    (let ((test:unused :zot))
      (assert (eql test:unused :zot)))

    ;; symbol-macrolet
    (symbol-macrolet ((test:function :sym-ok))
        (assert (eql test:function :sym-ok)))
    (symbol-macrolet ((test:unused :sym-ok2))
        (assert (eql test:unused :sym-ok2)))

    ;; binding as a function
    (flet ((test:*special* () :yes))
      (assert (eql (test:*special*) :yes)))
    (flet ((test:unused () :yes!))
      (assert (eql (test:unused) :yes!)))
    (labels ((test:*special* () :yes))
      (assert (eql (test:*special*) :yes)))
    (labels ((test:unused () :yes!))
      (assert (eql (test:unused) :yes!)))

    ;; binding as a macro
    (macrolet ((test:*special* () :ok))
      (assert (eql (test:*special*) :ok)))
    ))

;;; A collection of forms that cause runtime package lock violations
;;; on TEST, and will also signal an error on LOAD even if first
;;; compiled with COMPILE-FILE with TEST unlocked.
(defvar *illegal-runtime-forms*
  '(;; package alterations
    (intern *uninterned* :test)
    (import 'not-from-test :test)
    (export 'test::internal :test)
    (unexport 'test:external :test)
    (shadowing-import 'not-from-test :test)
    (let ((p (find-package :test)))
      (rename-package p :test '(:test-nick)))
    (use-package :test-unused :test)
    (unuse-package :test-used :test)
    (add-package-local-nickname :not-nicknamed :test-not-nicknamed :test)
    (remove-package-local-nickname :nicknamed :test)
    (shadow 'not-from-test :test)
    (unintern (or (find-symbol *interned* :test) (error "bugo")) :test)
    (delete-package :test-delete)

    ;; redefining or undefining as a function
    (defun test:function () 'foo)
    (setf (fdefinition 'test:function) (lambda () 'bar))
    (setf (symbol-function 'test:function) (lambda () 'quux))
    (tmp-fmakunbound 'test:function)

    ;; defining or undefining as a macro or compiler macro
    (defmacro test:unused () ''foo)
    (setf (macro-function 'test:unused) (constantly 'foo))
    (define-compiler-macro test:unused (&whole form arg)
      (declare (ignore arg))
      form)
    (setf (compiler-macro-function 'test:unused) (constantly 'foo))

    ;; type-specifier or structure
    (progn
      (defstruct test:nostruct test:nostruct-slot)
      ;; test creation as well, since the structure-class won't be
      ;; finalized before that
      (make-nostruct :nostruct-slot :foo))
    (defclass test:noclass ()
      ((slot :initform nil :accessor test:noclass-slot)))
    (deftype test:notype () 'string)
    (define-condition test:nocondition (error)
      ((slot :initform nil :accessor test:nocondition-slot)))

    ;; symbol-macro
    (define-symbol-macro test:nosymbol-macro 'foo)

    ;; declaration proclamation
    (proclaim '(declaration test:unused))

    ;; declare special
    (declaim (special test:nospecial))
    (proclaim '(special test:nospecial))

    ;; declare type
    (declaim (type fixnum test:num))
    (proclaim '(type fixnum test:num))

    ;; declare ftype
    (declaim (ftype (function (fixnum) fixnum) test:numfun))
    (proclaim '(ftype (function (fixnum) fixnum) test:numfun))

    ;; setf expanders
    (defsetf test:car rplaca) ; strictly speaking wrong, but ok as a test
    (defsetf test:car (cons) (new-car)
      `(setf (car ,cons) ,new-car))
    (define-setf-expander test:car (place)
      (multiple-value-bind (dummies vals newval setter getter)
          (get-setf-expansion place)
        (declare (ignore newval setter))
        (let ((store (gensym)))
          (values dummies
                  vals
                  `(,store)
                  `(progn (rplaca ,getter ,store) ,store)
                  `(car ,getter)))))

    ;; setf function names
    (defun (setf test:function) (obj)
      obj)
    (tmp-fmakunbound '(setf test:cdr))

    ;; define-method-combination
    (define-method-combination test:unused)

    ;; setf find-class
    (setf (find-class 'test:class) (find-class 'standard-class))
    ))

;;; Forms that cause violations on two distinct packages.
(defvar *illegal-double-forms*
  '((defclass test:noclass () ((x :accessor test-aux:noslot)))
    (define-condition test:nocondition (error)
      ((x :accessor test-aux:noslot2)))))

;;; A collection of forms that cause compile-time package lock
;;; violations on TEST, and will not signal an error on LOAD if first
;;; compiled by COMPILE-FILE with test unlocked. CAR is the affected
;;; symbol, CDR the form affecting it.
(defvar *illegal-lexical-forms-alist*
  '(;; binding

    ;; binding as a function
    (test:function . (flet ((test:function () :shite))
                       (test:function)))
    (test:function . (labels ((test:function () :shite))
                       (test:function)))
    (test:macro . (flet ((test:macro () :shite))
                    (test:macro)))
    (test:macro . (labels ((test:macro () :shite))
                    (test:macro)))

    ;; macrolet
    (test:function . (macrolet ((test:function () :yuk))
                       (test:function)))
    (test:macro . (macrolet ((test:macro () :yuk))
                    (test:macro)))

    ;; setf name
    (test:function . (flet (((setf test:function) (obj)
                              obj))
                       (setf (test:function) 1)))

    ;; ftype
    ;;
    ;; The legacy interpreter doesn't do anything with ftype declarations
    #+(or :sb-fasteval (not :interpreter))
    (test:function . (locally
                         (declare (ftype function test:function))
                       (cons t t)))

    ;; type
    ;;
    ;; Nor with type declarations
    #+(or :sb-fasteval (not :interpreter))
    (test:num . (locally
                    (declare (type fixnum test:num))
                  (cons t t)))

    ;; special
    (test:nospecial . (locally
                          (declare (special test:nospecial))
                        (cons t t)))

    ;; declare ftype
    #+(or :sb-fasteval (not :interpreter))
    (test:numfun . (locally
                       (declare (ftype (function (fixnum) fixnum) test:numfun))
                     (cons t t)))))

(defvar *illegal-lexical-forms*
  (mapcar #'cdr *illegal-lexical-forms-alist*))

(defvar *illegal-forms* (append *illegal-runtime-forms*
                                *illegal-lexical-forms*
                                *illegal-double-forms*))

;;;; Running the tests

;;; Unlocked. No errors nowhere.
(reset-test nil)

(with-test (:name :unlocked-package)
  (dolist (form (append *legal-forms* *illegal-forms*))
    (with-error-info ("Unlocked form: ~S~%" form)
      (eval form))))

;;; Locked. Errors for all illegal forms, none for legal.
(reset-test t)

(with-test (:name :locked-package/legal-forms)
  (dolist (form *legal-forms*)
    (with-error-info ("locked legal form: ~S~%" form)
      (eval form))))

(with-test (:name :locked-package/illegal-runtime-forms)
  (dolist (form (remove 'declaim (append *illegal-runtime-forms*
                                         *illegal-double-forms*)
                        :key #'first))
    (with-error-info ("locked illegal runtime form: ~S~%" form)
      (let ((fun (checked-compile `(lambda () ,form))))
        (assert-error (funcall fun) sb-ext:package-lock-violation))
      (assert-error (eval form) sb-ext:package-lock-violation))))

(with-test (:name :locked-package/illegal-lexical-forms)
  (loop :for (nil . form) :in *illegal-lexical-forms-alist* :do
     (with-error-info ("compile locked illegal lexical form: ~S~%" form)
       (let ((fun (checked-compile `(lambda () ,form)
                                   :allow-failure t
                                   :allow-warnings 'simple-warning)))
         (assert-error (funcall fun) program-error))
       (assert-error (let ((*error-output* (make-broadcast-stream)))
                       (eval form))
                     ;; Let's not be pedantic here.
                     ;; PACKAGE-LOCK-VIOLATION is right,
                     ;; because the distinction between lexical analysis
                     ;; and running is artificial for interpreted code.
                     (or sb-ext:package-lock-violation program-error)))))

;;; Locked, WITHOUT-PACKAGE-LOCKS
(reset-test t)

(with-test (:name (sb-ext:without-package-locks :locked-package :illegal-runtime-forms))
  (dolist (form (remove 'declaim *illegal-runtime-forms* :key #'first))
    (with-error-info ("without-package-locks illegal runtime form: ~S~%" form)
      (funcall (checked-compile `(lambda () (without-package-locks ,form)))))))

(with-test (:name (sb-ext:without-package-locks :locked-package :illegal-lexical-forms))
 (dolist (form *illegal-lexical-forms*)
   (let ((fun (without-package-locks (checked-compile `(lambda () ,form)))))
     (funcall fun))
   (without-package-locks (eval form))))

;;; Locked, DISABLE-PACKAGE-LOCKS
(reset-test t)

(dolist (pair *illegal-lexical-forms-alist*)
  (destructuring-bind (sym . form) pair
    (with-error-info ("disable-package-locks on illegal form: ~S~%"
                      form)
      (funcall (checked-compile `(lambda ()
                                   (declare (disable-package-locks ,sym))
                                   ,form)))
      (eval `(locally
                 (declare (disable-package-locks ,sym))
               ,form)))))

;;; Locked, one error per "lexically apparent violated package", also
;;; test restarts.
(reset-test t)

(with-test (:name :illegal-runtime-forms)
 (dolist (form *illegal-runtime-forms*)
   (with-error-info ("one error per form ~S~%" form)
     (let ((errorp nil))
       (handler-bind ((package-lock-violation (lambda (e)
                                                (when errorp
                                                  (error "multiple errors ~%~a~% and ~%~a"
                                                         errorp e))
                                                (setf errorp e)
                                                (continue e))))
         (eval form))))))

(with-test (:name :illegal-double-forms)
  (dolist (form *illegal-double-forms*)
    (with-error-info ("two errors per form: ~S~%" form)
      (let ((error-count 0))
        ;; check that we don't get multiple errors from a single form
        (handler-bind ((package-lock-violation (lambda (x)
                                                 (declare (ignorable x))
                                                 (incf error-count)
                                                 (continue x))))
          (eval form)
          (unless (= 2 error-count)
            (error "expected 2 errors per form, got ~A for ~A"
                   error-count form)))))))

;;; COMPILE-FILE when unlocked, LOAD locked -- *illegal-runtime-forms* only
;;;
;;; This is not part of the interface, but it is the behaviour we want
(with-test (:name (compile-file load :locked-package))
  (let* ((tmp (scratch-file-name "lisp"))
         (fasl (compile-file-pathname tmp)))
    (dolist (form *illegal-runtime-forms*)
      (unwind-protect
           (with-simple-restart (next "~S failed, continue with next test" form)
             (reset-test nil)
             (with-open-file (f tmp :direction :output)
               (prin1 form f))
             (multiple-value-bind (file warnings failure-p) (compile-file tmp)
               (declare (ignore file warnings failure-p))
               (set-test-locks t)
               (assert-error (load fasl) sb-ext:package-lock-violation)))
        (when (probe-file tmp)
          (delete-file tmp))
        (when (probe-file fasl)
          (delete-file fasl))))))

;;;; Tests for enable-package-locks declarations
(reset-test t)

(with-test (:name (sb-ext:enable-package-locks))
  (loop :for (sym . form) :in *illegal-lexical-forms-alist* :do
     (let ((fun (checked-compile
                 `(lambda ()
                    (declare (disable-package-locks ,sym))
                    ,form
                    (locally (declare (enable-package-locks ,sym))
                      ,form))
                 :allow-failure t
                 :allow-warnings 'simple-warning)))
       (assert-error (funcall fun) program-error))
     (assert-error
      (let ((*error-output* (make-broadcast-stream)))
        (eval `(locally (declare (disable-package-locks ,sym))
                 ,form
                 (locally (declare (enable-package-locks ,sym))
                   ,form))))
      (or sb-ext:package-lock-violation program-error))))

;;;; See that trace on functions in locked packages doesn't break
;;;; anything.
(assert (trace test:function :break t))
(untrace test:function)

;;;; No bogus violations from defclass with accessors in a locked
;;;; package. Reported by by Francois-Rene Rideau.
(with-test (:name (defclass :accessor :package-locked))
  (assert (package-locked-p :sb-gray))
  (let ((fun (checked-compile
              '(lambda ()
                (defclass fare-class ()
                  ((line-column :initform 0 :reader sb-gray:stream-line-column)))))))
    (multiple-value-bind (class run-errors) (ignore-errors (funcall fun))
      (assert (not run-errors))
      (assert (eq class (find-class 'fare-class))))))

;;;; No bogus violations from DECLARE's done by PCL behind the
;;;; scenes. Reported by David Wragg on sbcl-help.
(reset-test t)

(defmethod pcl-type-declaration-method-bug ((test:*special* stream))
  test:*special*)
(assert (eq *terminal-io* (pcl-type-declaration-method-bug *terminal-io*)))

;; Interpreters don't walk into a method body until it's executed.
#-:interpreter
(assert-error
 (eval
  '(defmethod pcl-type-declaration-method-bug ((test:*special* stream))
    (declare (type stream test:*special*))
    test:*special*))
 program-error)

;;; Bogus package lock violations from LOOP

(with-test (:name (loop :bogus sb-ext:package-lock-violation))
  (assert (equal (loop :for *print-base* :from 2 :to 3 :collect *print-base*)
                 '(2 3))))

;;; Package lock for DEFMACRO -> DEFUN and vice-versa.
(reset-test t)
(with-test (:name :bug-576637)
  (assert-error (eval `(defun test:macro (x) x))
                sb-ext:package-lock-violation)
  (assert (eq 'test:macro (eval `(test:macro))))
  (assert-error (eval `(defmacro test:function (x) x))
                sb-ext:package-lock-violation)
  (assert (eq 'test:function (eval `(test:function)))))

(defpackage :macro-killing-macro-1
  (:use :cl)
  (:lock t)
  (:export #:to-die-for))

(defpackage :macro-killing-macro-2
  (:use :cl :macro-killing-macro-1))

(ctu:file-compile
 `((in-package :macro-killing-macro-1)
   (defmacro to-die-for ()
     :original))
 :load t)

(with-test (:name :defmacro-killing-macro)
  (ignore-errors
    (ctu:file-compile
     `((in-package :macro-killing-macro-2)
       (defmacro to-die-for ()
         :replacement))))
  (assert (eq :original (macroexpand '(macro-killing-macro-1:to-die-for)))))

(with-test (:name :setf-macro-function-killing-macro)
  (ignore-errors
    (ctu:file-compile
     `((in-package :macro-killing-macro-2)
       (eval-when (:compile-toplevel)
         (setf (macro-function 'to-die-for) (constantly :replacement2))))))
  (assert (eq :original (macroexpand '(macro-killing-macro-1:to-die-for)))))

(with-test (:name :compile-time-defun-package-locked)
  ;; Make sure compile-time side-effects of DEFUN are protected against.
  (let ((inline-lambda (function-lambda-expression #'fill-pointer)))
    ;; Make sure it's actually inlined...
    (assert inline-lambda)
    (assert (eq :ok
                (handler-case
                    (ctu:file-compile `((defun fill-pointer (x) x)))
                  (sb-ext:symbol-package-locked-error (e)
                    (when (eq 'fill-pointer
                              (sb-ext:package-locked-error-symbol e))
                      :ok)))))
    (assert (equal inline-lambda
                   (function-lambda-expression #'fill-pointer)))))

(with-test (:name :compile-time-defclass-package-locked)
  ;; Compiling (DEFCLASS FTYPE ...) used to break SBCL, but the package
  ;; locks didn't kick in till later.
  (assert (eq :ok
              (handler-case
                  (ctu:file-compile `((defclass ftype () ())))
                (sb-ext:symbol-package-locked-error (e)
                  (when (eq 'ftype (sb-ext:package-locked-error-symbol e))
                    :ok)))))
  ;; Check for accessor violations as well.
  (assert (eq :ok
              (handler-case
                  (ctu:file-compile `((defclass foo () ((ftype :reader ftype)))))
                (sb-ext:symbol-package-locked-error (e)
                  (when (eq 'ftype (sb-ext:package-locked-error-symbol e))
                    :ok))))))

(with-test (:name :assert-symbol-home-package-unlocked)
  (assert-error (sb-impl::assert-symbol-home-package-unlocked
                 'cl:cons "trying to foo ~S")
                symbol-package-locked-error)
  (assert-error
   (sb-impl::assert-symbol-home-package-unlocked
    'cl:cons "trying to ~*~S ~2:*~A~* as a ~S"
    :foo :bar)
   symbol-package-locked-error))

(with-test (:name :defcostant-locks)
  (assert-error (defconstant test:constant 100)
                symbol-package-locked-error))

(with-test (:name :defstruct-compile-time-locks)
  (assert-error (ctu:file-compile
                 `((defstruct test:nostruct)))
      symbol-package-locked-error)
  (assert-error (ctu:file-compile
                 `((defstruct (a-struct-test.1
                               (:conc-name))
                     test:nostruct)))
      symbol-package-locked-error)
  (assert-error (ctu:file-compile
                 `((defstruct (a-struct-test.2
                               (:predicate test:nostruct)))))
      symbol-package-locked-error)
  (assert-error (ctu:file-compile
                 `((defstruct (a-struct-test.3
                               (:copier test:nostruct)))))
      symbol-package-locked-error)
  (assert-error (ctu:file-compile
                 `((defstruct (a-struct-test.4
                               (:constructor test:nostruct)))))
      symbol-package-locked-error))

(with-test (:name :set-undefined-vars)
  (assert-error (eval '(set 'test:car 10))
                symbol-package-locked-error)
  (assert-error (eval '(setf test:car 10))
                symbol-package-locked-error)
  (assert-error (eval '(setf (symbol-value 'test:car) 10))
                symbol-package-locked-error))

(with-test (:name :set-undefined-vars-warnings)
  (flet ((test (lambda)
           (multiple-value-bind (fun failure warnings)
               (checked-compile lambda :allow-warnings t)
             (assert (and failure warnings))
             (assert-error (funcall fun)
                           symbol-package-locked-error))))
    (test '(lambda () (set 'test:car 10)))
    (test '(lambda () (setf test:car 10)))
    (test '(lambda () (setf (symbol-value 'test:car) 10)))))

(with-test (:name :declare-unbound-special)
  (assert (nth-value 1
                     (checked-compile
                      '(lambda ()
                        (declare (fixnum test:*unbound-special*)))
                      :allow-failure t
                      :allow-warnings t))))

(with-test (:name :declare-bound-non-special)
  (checked-compile '(lambda (test:bound-non-special)
                     (declare (fixnum test:bound-non-special))
                     test:bound-non-special)))

(with-test (:name :unintern-nil)
  (assert-error (unintern nil 'test)
                package-locked-error))

(with-test (:name :progv-unbind)
  (checked-compile-and-assert
   ()
   '(lambda (vars vals)
     (progv vars vals))
   (('(test:*special*) nil) (condition 'symbol-package-locked-error))))


