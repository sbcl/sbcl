;;;; the ASSERTOID macro, asserting something with added generality
;;;; to help in regression tests

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

(cl:defpackage "ASSERTOID"
  (:use "CL")
  (:export "GRAB-CONDITION" "ASSERT-ERROR"
           "HAS-ERROR?" "IS" "ASSERTOID"
           "ASSERT-SIGNAL" "ASSERT-NO-SIGNAL"
           "LEGACY-EVAL-P"
           "EQUAL-MOD-GENSYMS" "CHECK-FUNCTION-EVALUATION-ORDER"
           "ASSERT-TYPE"))

(cl:in-package "ASSERTOID")

(defmacro grab-condition (&body body)
  `(nth-value 1
     (ignore-errors ,@body)))

(defmacro has-error? (form &optional (error-subtype-spec 'error))
  `(typep (nth-value 1 (ignore-errors ,form)) ',error-subtype-spec))

(defmacro assert-error (form &optional (error-subtype-spec 'error))
  `(let ((result (multiple-value-list (ignore-errors ,form))))
     (assert (typep (second result) ',error-subtype-spec)
             ()  "~s returned ~s without signalling ~s"
             ',form
             result
             ',error-subtype-spec)))

(defun %assert-signal (thunk condition-type
                       expected-min-count expected-max-count)
  (declare (ignore condition-type))
  (let ((count 0))
    (prog1
        (funcall thunk (lambda (condition)
                         (incf count)
                         (when (typep condition 'warning)
                           (muffle-warning condition))))
      (assert (<= expected-min-count count expected-max-count)))))

(defmacro assert-signal (form &optional
                              (condition-type 'condition)
                              (expected-min-count 1)
                              (expected-max-count expected-min-count))
  (let ((handle (gensym)))
    `(%assert-signal
      (lambda (,handle)
        (handler-bind ((,condition-type ,handle)) ,form))
      ',condition-type ,expected-min-count ,expected-max-count)))

(defun %assert-no-signal (thunk condition-type)
  (declare (ignore condition-type))
  (let ((signaled-condition))
    (prog1
        (funcall thunk (lambda (condition)
                         (setf signaled-condition condition)
                         (when (typep condition 'warning)
                           (muffle-warning condition))))
      (assert (not signaled-condition)))))

(defmacro assert-no-signal (form &optional (condition-type 'condition))
  (let ((handle (gensym)))
    `(%assert-no-signal
      (lambda (,handle)
        (handler-bind ((,condition-type ,handle)) ,form))
      ',condition-type)))

(defmacro assert-type (lambda type)
  (if (typep type '(cons (eql function)))
      `(assert
        (test-util:type-specifiers-equal
         (sb-kernel:%simple-fun-type
          (test-util:checked-compile ',lambda))
         ',type))
      `(assert
        (test-util:type-specifiers-equal
         (caddr
          (sb-kernel:%simple-fun-type
           (test-util:checked-compile ',lambda)))
         ',(if (typep type '(cons (eql values)))
               type
               `(values ,type &optional))))))

;;; EXPR is an expression to evaluate (both with EVAL and with
;;; COMPILE/FUNCALL). EXTRA-OPTIMIZATIONS is a list of lists of
;;; optimizations to pass to (DECLARE (OPTIMIZE ..)), to cause the
;;; expression to be tested in other than the default optimization
;;; level(s).
;;;
;;; The messiness with the various flavors of EXPECTED stuff is
;;; to handle various issues:
;;;   * Some things are expected to signal errors instead of returning
;;;     ordinary values.
;;;   * Some things are expected to return multiple values.
;;;   * Some things can return any of several values (e.g. generalized
;;;     booleans).
;;; The default is to expect a generalized boolean true.
;;;
;;; Use EXPECTED-LAMBDA to require an answer which satisfies the given
;;; LAMBDA. EXPECTED-EQL, EXPECTED-EQUAL, and EXPECTED-EQUALP are
;;; shorthand for special cases of EXPECTED-LAMBDA.
;;;
;;; Use EXPECTED-ERROR to require an error to be signalled. Use
;;; EXPECTED-ERROR-LAMBDA to require that an error be signalled and
;;; that further it satisfies the given lambda.
(defmacro assertoid (expr
                     &key
                     extra-optimizations
                     (expected-eql nil expected-eql-p)
                     (expected-equal nil expected-equal-p)
                     (expected-equalp nil expected-equalp-p)
                     (expected-lambda (cond
                                       (expected-eql-p
                                        (lambda (x)
                                          (eql x (eval expected-eql))))
                                       (expected-equal-p
                                        (lambda (x)
                                          (equal x (eval expected-equal))))
                                       (expected-equalp-p
                                        (lambda (x)
                                          (equalp x (eval expected-equalp))))
                                       (t
                                        (lambda (x)
                                          x)))
                                      expected-lambda-p)
                     (expected-error-type nil expected-error-type-p)
                     (expected-error-lambda (if expected-error-type
                                                (lambda (condition)
                                                  (typep condition
                                                         expected-error-type))
                                                nil)
                                            expected-error-lambda-p))
  (when (> (count-if #'identity
                     (vector expected-eql-p
                             expected-equal-p
                             expected-equalp-p
                             expected-lambda-p
                             expected-error-type-p
                             expected-error-lambda-p))
           1)
    (error "multiple EXPECTED-FOO arguments"))
  (when expected-error-lambda
    (error "stub: expected-error functionality not supported yet"))
  (let ((eval-expected-lambda (eval expected-lambda)))
    (flet ((frob (evaloid)
                 (let ((result (funcall evaloid expr)))
                   (unless (funcall eval-expected-lambda result)
                     (error "failed assertoid ~S" expr))))
           (compile-as-evaloid (optimizations)
             (lambda (expr)
               (funcall (compile nil
                                 `(lambda ()
                                    (declare (optimize ,@optimizations))
                                    ,expr))))))
      (frob #'eval)
      (frob (compile-as-evaloid ()))
      (dolist (i extra-optimizations)
        (frob (compile-as-evaloid i))))))

;;; examples
(assertoid (= 2 (length (list 1 2))))
(assertoid (= 2 (length (list 1 2)))
           :extra-optimizations (((speed 2) (space 3))
                                 ((speed 1) (space 3))))
(assertoid (cons 1 2)
           :expected-lambda (lambda (x) (equal x '(1 . 2))))
(assertoid (cons (list 1 2) (list 1 2))
           :expected-equal '((1 2) 1 2))
;;; not implemented yet:
#+nil (assertoid (length (eval (find-package :cl)))
                 :expected-error-type 'type-error)

(defmacro is (form)
  (if (consp form)
      (destructuring-bind (op expected real) form
        `(let ((expected-value ,expected)
               (real-value ,real))
           (unless (,op expected-value real-value)
             (error "Wanted ~S, got ~S:~% ~S"
                    expected-value real-value ',form))))
      `(unless ,form
         (error "~S evaluated to NIL" ',form))))

;; Return T if two sexprs are EQUAL, considering uninterned symbols
;; in expression A as EQ to one in B provided that there exists a
;; mapping that makes the forms EQUAL.
;; This is helpful when testing complicated macroexpanders.
;; Note that this is much simpler than unification,
;; because symbols can only be replaced by other symbols.
(defun equal-mod-gensyms (a b &optional (pred #'equal))
  (let ((subst-table (make-hash-table :test 'eq)))
    (labels ((recurse (a b)
               (cond ((and (consp a) (consp b))
                      (and (recurse (car a) (car b))
                           (recurse (cdr a) (cdr b))))
                     ((and (symbolp a) (symbolp b))
                      (multiple-value-bind (replacement found)
                          (gethash a subst-table a)
                        (or (eq replacement b)
                            (and (not found)
                                 (not (symbol-package a))
                                 (setf (gethash a subst-table) b)))))
                     (t ; strings, numbers
                      (funcall pred a b)))))
      (recurse a b))))

(defun legacy-eval-p ()
  (and (eq sb-ext:*evaluator-mode* :interpret)
       (find-package "SB-EVAL")))

(defmacro check-function-evaluation-order (form)
  (let ((evals (gensym "EVALS"))
        expected)
    `(let ((,evals))
       (multiple-value-prog1
           (,(car form)
            ,@(loop for i from 0
                    for arg in (cdr form)
                    collect `(progn
                               (push ,i ,evals)
                               ,arg)
                    do
                    (push i expected)))
         (assert (equal ,evals ',expected)
                 () 'simple-error
                 :format-control "Bad evaluation order of ~s:~% ~s"
                 :format-arguments (list ',form
                                         (reverse ,evals)))))))
