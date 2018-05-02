;;;; testing method combination redefinition

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

(defpackage "CLOS-METHOD-COMBINATION-REDEFINITION"
  (:use "COMMON-LISP" "TEST-UTIL"))

(in-package "CLOS-METHOD-COMBINATION-REDEFINITION")

;;;; long-form method combination redefinition

;;; first, define a method combination
(define-method-combination long-or ()
  ((primary () :required t))
  `(or ,@(reverse (mapcar (lambda (m) `(call-method ,m)) primary))))

;;; GF has standard method combination
(defgeneric long-or-test (x)
  (:method ((x fixnum)) 'fixnum)
  (:method ((x integer)) 'integer)
  (:method ((x number)) 'number))

(with-test (:name (:method-combination standard))
  (assert (eql (long-or-test 3) 'fixnum))
  (assert (eql (long-or-test (1+ most-positive-fixnum)) 'integer))
  (assert (eql (long-or-test 3.2) 'number)))

;;; add the method combination
(defgeneric long-or-test (x)
  (:method ((x fixnum)) 'fixnum)
  (:method ((x integer)) 'integer)
  (:method ((x number)) 'number)
  (:method-combination long-or))

(with-test (:name (:method-combination :long-or-reverse))
  (assert (eql (long-or-test 3) 'number))
  (assert (eql (long-or-test (1+ most-positive-fixnum)) 'number))
  (assert (eql (long-or-test 3.2) 'number)))

;;; redefine the method combination
(define-method-combination long-or ()
  ((primary () :required t))
  `(or ,@(mapcar (lambda (m) `(call-method ,m)) primary)))

(with-test (:name (:method-combination :long-or))
  (assert (eql (long-or-test 3) 'fixnum))
  (assert (eql (long-or-test (1+ most-positive-fixnum)) 'integer))
  (assert (eql (long-or-test 3.2) 'number)))

;;;; short-form method-combination redefiniton

;;; define a method-combination
(define-method-combination div :operator /)

(defgeneric short-div (x)
  (:method-combination div :most-specific-first)
  (:method div ((x number)) 4)
  (:method div ((x fixnum)) 8))

(with-test (:name (:method-combination :short-div))
  (assert (= (short-div 3) 2))
  (assert (= (short-div 3.0) 1/4)))

;;; check that changing method-combination options works
(defgeneric short-div (x)
  (:method-combination div :most-specific-last)
  (:method div ((x number)) 4)
  (:method div ((x fixnum)) 8))

(with-test (:name (:method-combination :short-div :most-specific-last))
  (assert (= (short-div 3) 1/2))
  (assert (= (short-div 3.0) 1/4)))

;;; check that choosing new short-form options works
(define-method-combination div :operator / :identity-with-one-argument t)

(with-test (:name (:method-combination :short-div :identity-with-one-argument))
  (assert (= (short-div 3) 1/2))
  (assert (= (short-div 3.0) 4)))

;;; check that changing method-combination options works (deletion of :most-specific-last)
(defgeneric short-div (x)
  (:method-combination div)
  (:method div ((x number)) 4)
  (:method div ((x fixnum)) 8))

(with-test (:name (:method-combination :short-div :identity-with-one-argument :most-specific-first))
  (assert (= (short-div 3) 2))
  (assert (= (short-div 3.0) 4)))

;;; check that changing operator works
(define-method-combination div :operator -)

(with-test (:name (:method-combination :short-div :operator -))
  (assert (= (short-div 3) 4))
  (assert (= (short-div 3.0) -4)))


;;;; modifying the need for args-lambda-list

;;; define a fancy method combination.  (Happens to implement a finite state machine)
(define-method-combination fsm (default-start)
  ((primary *))
  `(let ((state ',default-start))
     (restart-bind
         (,@(mapcar (lambda (m) `(,(first (method-qualifiers m))
                                   (lambda ()
                                     (setq state (call-method ,m))
                                     (if (and (typep state '(and symbol (not null)))
                                              (find-restart state))
                                         (invoke-restart state)
                                         state))))
                    primary))
       (invoke-restart state))))

(defclass parse-state ()
  ((string :initarg :string)
   (index :initform 0)))

;;; use the finite state machine to recognize strings with an even
;;; number of #\a characters
(defgeneric even-as (state &key &allow-other-keys)
  (:method-combination fsm yes)
  (:method yes (state &key)
    (with-slots ((s string) (i index)) state
      (cond ((= i (length s)) t) ((char= (char s i) #\a) (incf i) 'no) (t (incf i) 'yes))))
  (:method no (state &key)
    (with-slots ((s string) (i index)) state
      (cond ((= i (length s)) nil) ((char= (char s i) #\a) (incf i) 'yes) (t (incf i) 'no)))))

;;; test.  (The non-functional :START argument tests are to contrast
;;; with what happens next)
(with-test (:name (:method-combination :finite-state-machine))
  (assert (even-as (make-instance 'parse-state :string "abcbab")))
  (assert (even-as (make-instance 'parse-state :string "abcbab") :start 'no))
  (assert (not (even-as (make-instance 'parse-state :string "abcbabab"))))
  (assert (not (even-as (make-instance 'parse-state :string "abcbabab") :start 'no))))

;;; generalize: if we allow the call site to specify the initial
;;; state, our FSM method combination is more expressive.
(define-method-combination fsm (default-start)
  ((primary *))
  (:arguments &key start)
  `(let ((state (or ,start ',default-start)))
     (restart-bind
         (,@(mapcar (lambda (m) `(,(first (method-qualifiers m))
                                   (lambda ()
                                     (setq state (call-method ,m))
                                     (if (and (typep state '(and symbol (not null)))
                                              (find-restart state))
                                         (invoke-restart state)
                                         state))))
                    primary))
       (invoke-restart state))))

(with-test (:name (:method-combination :finite-state-machine :redefinition :args-lambda-list))
  (assert (even-as (make-instance 'parse-state :string "abcbab")))
  (assert (not (even-as (make-instance 'parse-state :string "abcbab") :start 'no)))
  (assert (not (even-as (make-instance 'parse-state :string "abcbabab"))))
  (assert (even-as (make-instance 'parse-state :string "abcbabab") :start 'no)))

;;;; changing between short- and long-form method combination
(define-method-combination maximum :operator max)

(defgeneric maxx (x)
  (:method-combination maximum)
  (:method maximum ((x symbol)) 3)
  (:method maximum ((x list)) 4)
  (:method maximum ((x null)) 5))

(with-test (:name (:method-combination :maximum))
  (assert (= (maxx nil) 5))
  (assert (= (maxx '(3 4)) 4))
  (assert (= (maxx t) 3)))

(define-method-combination maximum ()
  ((maximum (maximum)))
  `(min ,@(mapcar (lambda (m) `(call-method ,m)) maximum)))

(with-test (:name (:method-combination :maximum :redefined-long))
  (assert (= (maxx nil) 3))
  (assert (= (maxx '(3 4)) 4))
  (assert (= (maxx t) 3)))

(define-method-combination maximum :operator -)

(with-test (:name (:method-combination :maximum :redefined-short))
  (assert (= (maxx nil) -2))
  (assert (= (maxx '(3 4)) -4))
  (assert (= (maxx t) -3)))
