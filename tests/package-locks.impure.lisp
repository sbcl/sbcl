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

(in-package :cl-user)

(load "assertoid.lisp")
(use-package "ASSERTOID")

#-sb-package-locks
(sb-ext:quit :unix-status 104)

;;;; Our little labrats and a few utilities

(defpackage :test-used)

(defpackage :test-unused)

(defpackage :test-aux (:export #:noslot))

(defpackage :test
  (:use :test-used)
  (:shadow #:shadowed)
  (:export 
   #:*special*
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
   #:unused
   ))

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

(defun reset-test ()
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
    (unexport (intern "INTERNAL" :test) :test)
    (intern *interned* :test)
    (use-package :test-used :test)
    (export 'test::external :test)
    (unuse-package :test-unused :test)
    (defclass test:class () ())
    (defun test:function () 'test:function)
    (defmacro test:macro () ''test:macro)
    (defparameter test:*special* 'test:*special*)
    (defconstant test:constant 'test:constant)
    (intern "UNUSED" :test)
    (dolist (s '(test:nocondition-slot test:noclass-slot test:nostruct-slot
		 test-aux:noslot))
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
    (assert (not (find-symbol *uninterned* :test)))))

(defun tmp-fmakunbound (x)
  "FMAKUNDBOUND x, then restore the original binding."
  (let ((f (fdefinition x)))
    (fmakunbound x)
    (ignore-errors (setf (fdefinition x) f))))

(defmacro with-error-info ((string &rest args) &body forms)
  `(handler-bind ((error (lambda (e)
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
    (shadow 'not-from-test :test)
    (unintern (or (find-symbol *interned* :test) (error "bugo")) :test)
    (delete-package :test-delete)

    ;; defining or undefining as a function
    (defun test:unused () 'foo)
    (setf (fdefinition 'test:unused) (lambda () 'bar))
    (setf (symbol-function 'test:unused) (lambda () 'quux))
    (tmp-fmakunbound 'test:function)

    ;; defining or undefining as a macro or compiler macro
    (defmacro test:unused () ''foo)
    (setf (macro-function 'test:unused) (constantly 'foo))
    (define-compiler-macro test:unused (&whole form arg) 
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
      ((x :accessor test-aux:noslot)))))

;;; A collection of forms that cause compile-time package lock
;;; violations on TEST, and will not signal an error on LOAD if first
;;; compiled by COMPILE-FILE with test unlocked. CAR is the affected
;;; symbol, CDR the form affecting it.
(defvar *illegal-compile-time-forms-alist*
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
    (test:function . (locally
                         (declare (ftype function test:function))
                       (cons t t)))

    ;; type
    (test:num . (locally
                    (declare (type fixnum test:num))
                  (cons t t)))
      
    ;; special
    (test:nospecial . (locally
                          (declare (special test:nospecial))
                        (cons t t)))

    ;; declare ftype
    (test:numfun . (locally
                       (declare (ftype (function (fixnum) fixnum) test:numfun))
                     (cons t t)))))

(defvar *illegal-compile-time-forms* (mapcar #'cdr *illegal-compile-time-forms-alist*))

(defvar *illegal-forms* (append *illegal-runtime-forms*
                                *illegal-compile-time-forms*
                                *illegal-double-forms*))

;;;; Running the tests

;;; Unlocked. No errors nowhere.
(reset-test)
(set-test-locks nil)
(dolist (form (append *legal-forms* *illegal-forms*))
  (with-error-info ("~Unlocked form: ~S~%" form)
    (eval form)))

;;; Locked. Errors for all illegal forms, none for legal.
(reset-test)
(set-test-locks t)
(dolist (form *legal-forms*)
  (with-error-info ("locked legal form: ~S~%" form)
    (eval form)))
(reset-test)
(set-test-locks t)
(dolist (form (append *illegal-runtime-forms* *illegal-double-forms*))
  (with-error-info ("locked illegal runtime form: ~S~%" form)
    (let ((fun (compile nil `(lambda () ,form))))
      (assert (raises-error? (funcall fun) sb-ext:package-lock-violation)))))
(dolist (pair *illegal-compile-time-forms-alist*)
  (let ((form (cdr pair)))
    (with-error-info ("locked illegal compile-time form: ~S~%" form)
      (assert (raises-error? (compile nil `(lambda () ,form)) sb-ext:package-lock-violation)))))

;;; Locked, WITHOUT-PACKAGE-LOCKS for runtime errors.
(reset-test)
(set-test-locks t)
(dolist (form *illegal-runtime-forms*)
  (with-error-info ("without-package-locks illegal runtime form: ~S~%" form)
    (funcall (compile nil `(lambda () (without-package-locks ,form))))))

;;; Locked, WITHOUT-PACKAGE-LOCKS & DISABLE-PACKAGE-LOCKS for compile-time errors.
(reset-test)
(set-test-locks t)
(dolist (pair *illegal-compile-time-forms-alist*)
  (destructuring-bind (sym . form) pair
    (with-error-info ("without-package-locks illegal compile-time form: ~S~%" form)
      (let ((fun (without-package-locks (compile nil `(lambda () ,form)))))
        (funcall fun)))))
(reset-test)
(set-test-locks t)
(dolist (pair *illegal-compile-time-forms-alist*)
  (destructuring-bind (sym . form) pair
    (with-error-info ("disable-package-locks illegal compile-time form: ~S~%" form)
      (funcall (compile nil `(lambda ()
                              (declare (disable-package-locks ,sym))
                              ,form))))))

;;; Locked, one error per "lexically apparent violated package", also
;;; test restarts.
(reset-test)
(set-test-locks t)
(dolist (form (append *illegal-runtime-forms* *illegal-compile-time-forms*))
  (with-error-info ("one error per form: ~S~%")
    (let ((errorp nil))
      (handler-bind ((package-lock-violation (lambda (e)
                                               (when errorp
                                                 (error "multiple errors"))
                                               (setf errorp t)
                                               (continue e))))
        (eval form)))))
(dolist (form *illegal-double-forms*)
  (with-error-info ("two errors per form: ~S~%" form)
    (let ((error-count 0))
      ;; check that we don't get multiple errors from a single form
      (handler-bind ((package-lock-violation (lambda (x)
                                               (declare (ignore x))
                                               (incf error-count)
                                               (continue x))))
	(eval form)
	(unless (= 2 error-count)
	  (error "expected 2 errors per form, got ~A for ~A" 
		 error-count form))))))

;;; COMPILE-FILE when unlocked, LOAD locked -- *illegal-runtime-forms* only
(let* ((tmp "package-locks.tmp.lisp")
       (fasl (compile-file-pathname tmp))
       (n 0))
  (dolist (form *illegal-runtime-forms*)
    (unwind-protect
	 (with-simple-restart (next "~S failed, continue with next test" form)
	   (reset-test)
	   (set-test-locks nil)
	   (with-open-file (f tmp :direction :output)
	     (prin1 form f))
	   (multiple-value-bind (file warnings failure-p) (compile-file tmp)
	     (set-test-locks t)
	     (assert (raises-error? (load fasl) sb-ext:package-lock-violation))))
      (when (probe-file tmp)
	(delete-file tmp))
      (when (probe-file fasl)
	(delete-file fasl)))))

;;;; Tests for enable-package-locks declarations
(reset-test)
(set-test-locks t)
(dolist (pair *illegal-compile-time-forms-alist*)
  (destructuring-bind (sym . form) pair
    (assert (raises-error?
             (compile nil `(lambda ()
                            (declare (disable-package-locks ,sym))
                            ,form
                            (locally (declare (enable-package-locks ,sym))
                              ,form)))
             package-lock-violation))
    (assert (raises-error?
             (eval `(locally (declare (disable-package-locks ,sym))
                     ,form
                     (locally (declare (enable-package-locks ,sym))
                       ,form)))
             package-lock-violation))))

;;;; See that trace on functions in locked packages doesn't break
;;;; anything.
(assert (trace test:function :break t))

;;; WOOT! Done.
(sb-ext:quit :unix-status 104)
