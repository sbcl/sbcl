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

(cl:in-package :cl-user)

(use-package :test-util)

;;; Bug from CLOCC.
(defpackage :p1
  (:use :cl)
  (:export #:code #:code-msg #:%code-msg))
(in-package :p1)
(define-condition code ()
  ((msg :reader code-msg :reader %code-msg :initarg :msg)))

(defpackage :p2
  (:use :cl :p1))
(in-package :p2)
(define-condition code1 (code)
  ((msg :accessor code-msg :initarg :msg)))

(let ((code (make-condition 'code :msg 1)))
  (assert (typep code 'code))
  (assert (eql (code-msg code) 1))
  (assert (eql (%code-msg code) 1)))
(let ((code (make-condition 'code1 :msg 1)))
  (assert (typep code 'code))
  (assert (eql (code-msg code) 1))
  (assert (eql (%code-msg code) 1))
  (setf (code-msg code) 2)
  (assert (eql (code-msg code) 2))
  (assert (eql (%code-msg code) 1)))

(in-package :cl-user)

;;; Check that initializing the condition class metaobject doesn't create
;;; any instances. Reported by Marco Baringer on sbcl-devel Mon, 05 Jul 2004.
(defvar *condition-count* 0)
(define-condition counted-condition () ((slot :initform (incf *condition-count*))))
(defmethod frob-counted-condition ((x counted-condition)) x)
(assert (= 0 *condition-count*))
(assert (typep (sb-mop:class-prototype (find-class 'counted-condition))
               '(and condition counted-condition)))

(define-condition picky-condition () ())

(with-test (:name (:picky-condition compute-restarts))
  (restart-case
      (handler-case
          (error 'picky-condition)
        (picky-condition (c)
          ;; The PICKY-RESTART should be applicable for the
          ;; PICKY-CONDITION and all other cases.
          (assert (eq (restart-name (first (compute-restarts))) 'picky-restart))
          (assert (eq (restart-name (first (compute-restarts c))) 'picky-restart))
          (assert (eq (car (compute-restarts)) (car (compute-restarts c))))
          ;; ANOTHER-PICKY-RESTART should not be applicable for the
          ;; PICKY-CONDITION, but all other cases.
          (assert (not (find 'another-picky-restart (compute-restarts c)
                             :key #'restart-name)))
          (assert (find 'another-picky-restart (compute-restarts)
                        :key #'restart-name))
          :ok))
    (picky-restart ()
      :report "Do nothing."
      :test (lambda (c) (typep c '(or null picky-condition))))
    (another-picky-restart ()
      :report "Do nothing as well"
      :test (lambda (c) (typep c '(not picky-condition))))))

;;; adapted from Helmut Eller on cmucl-imp
(with-test (:name (:picky-condition invoke-restart))
  (assert (eq 'it
              (restart-case
                  (handler-case
                      (error 'picky-condition)
                    (picky-condition (c)
                      (invoke-restart (find-restart 'give-it c))))
                (give-it ()
                  :test (lambda (c) (typep c 'picky-condition))
                  'it)))))

;;; In sbcl-1.0.9, a condition derived from CL:STREAM-ERROR (or
;;; CL:READER-ERROR or or CL:PARSE-ERROR) didn't inherit a usable
;;; PRINT-OBJECT method --- the PRINT-OBJECT code implicitly assumed
;;; that CL:STREAM-ERROR was like a SIMPLE-CONDITION, with args and
;;; format control, which seems to be a preANSIism.
;;;
;;; (The spec for DEFINE-CONDITION says that if :REPORT is not
;;; supplied, "information about how to report this type of condition
;;; is inherited from the PARENT-TYPE." The spec doesn't explicitly
;;; forbid the inherited printer from trying to read slots which
;;; aren't portably specified for the condition, but it doesn't seem
;;; reasonable for the inherited printer to do so. It does seem
;;; reasonable for app code to derive a new condition from
;;; CL:READER-ERROR (perhaps for an error in a readmacro) or
;;; CL:PARSE-ERROR (perhaps for an error in an operator
;;; READ-MY-FAVORITE-DATA-STRUCTURE) or CL:STREAM-ERROR (dunno why
;;; offhand, but perhaps for some Gray-stream-ish reason), not define
;;; a :REPORT method for its new condition, and expect to inherit from
;;; the application's printer all the cruft required for describing
;;; the location of the error in the input.)
(define-condition my-stream-error-1-0-9 (stream-error) ())
(define-condition parse-foo-error-1-0-9 (parse-error) ())
(define-condition read-bar-error-1-0-9 (reader-error) ())
(with-test (:name :printable-conditions)
  (let (;; instances created initializing all the slots specified in
        ;; ANSI CL
        (parse-foo-error-1-0-9 (make-condition 'parse-foo-error-1-0-9
                                               :stream *standard-input*))
        (read-foo-error-1-0-9 (make-condition 'read-bar-error-1-0-9
                                              :stream *standard-input*))
        (my-stream-error-1-0-9 (make-condition 'my-stream-error-1-0-9
                                               :stream *standard-input*)))
    ;; should be printable
    (dolist (c (list
                my-stream-error-1-0-9
                parse-foo-error-1-0-9
                read-foo-error-1-0-9))
      ;; whether escaped or not
      (dolist (*print-escape* '(nil t))
        (write c :stream (make-string-output-stream))))))

;;; Reported by Michael Weber: restart computation in :TEST-FUNCTION used to
;;; cause infinite recursion.
(defun restart-test-finds-restarts ()
  (restart-bind
      ((bar (lambda ()
              (return-from restart-test-finds-restarts 42))
         :test-function
         (lambda (condition)
           (declare (ignore condition))
           (find-restart 'qux))))
    (when (find-restart 'bar)
      (invoke-restart 'bar))))
(assert (not (restart-test-finds-restarts)))

(with-test (:name :bug-896379)
  (let ((*evaluator-mode* :compile))
    (handler-bind ((style-warning #'error))
      (let ((reader (gensym "READER"))
            (name (gensym "FOO-ERROR")))
        (eval `(define-condition ,name (error)
                 ((slot :initarg :slot :reader ,reader))
                 (:report (lambda (c stream)
                            (format stream "Oops: ~S" (,reader c))))))))))

(with-test (:name :define-condition-result)
  (let ((name (gensym "CONDITION")))
    (assert
     (eq (eval `(define-condition ,name () ()))
         name))))

;;; bug-1164970

(define-condition condition-with-default-initargs (condition)
  ()
  (:default-initargs :foo 1))

(with-test (:name (sb-mop:class-direct-default-initargs :for-condition-class
                   :bug-1164970))
  ;; CLASS-DIRECT-DEFAULT-INITARGS used to return nil for all
  ;; condition classes.
  (let ((initargs (sb-mop:class-direct-default-initargs
                   (find-class 'condition-with-default-initargs))))
    (assert (equal (subseq (first initargs) 0 2) '(:foo 1)))))

;;; bug-539517

(defconstant +error-when-called+ (lambda () (error "oops")))

(define-condition condition-with-constant-function-initarg ()
  ((foo :initarg :foo
        :reader condition-with-constant-function-initarg-foo))
  (:default-initargs :foo +error-when-called+))

(with-test (:name (:condition-with-constant-function-initarg :bug-539517))
  ;; The default initarg handling for condition classes used to
  ;; confuse constant functions (thus +ERROR-WHEN-CALLED+) and
  ;; initfunctions. This lead to +ERROR-WHEN-CALLED+ being called as
  ;; if it was an initfunction.
  (assert (functionp
           (condition-with-constant-function-initarg-foo
            (make-condition 'condition-with-constant-function-initarg))))
  (assert (functionp
           (condition-with-constant-function-initarg-foo
            (make-instance 'condition-with-constant-function-initarg)))))

;; Same problem

(define-condition condition-with-constant-function-initform ()
  ((foo :initarg :foo
        :reader condition-with-constant-function-initform-foo
        :initform +error-when-called+)))

(with-test (:name (:condition-with-constant-function-slot-initform))
  (assert (functionp
           (condition-with-constant-function-initform-foo
            (make-condition 'condition-with-constant-function-initform))))
  (assert (functionp
           (condition-with-constant-function-initform-foo
            (make-instance 'condition-with-constant-function-initform)))))

;;; bug-1164969

(defvar bar-counter 0)

(defvar baz-counter 0)

(define-condition condition-with-non-constant-default-initarg ()
  ((bar :initarg :bar
        :reader condition-with-non-constant-default-initarg-bar)
   (baz :initarg :baz
        :reader condition-with-non-constant-default-initarg-baz
        :initform (incf baz-counter)))
  (:default-initargs :bar (incf bar-counter)))
(define-condition condition-with-non-constant-default-initarg ()
  ((bar :initarg :bar
        :reader condition-with-non-constant-default-initarg-bar)
   (baz :initarg :baz
        :reader condition-with-non-constant-default-initarg-baz
        :initform (incf baz-counter)))
  (:default-initargs :bar (incf bar-counter)))

(with-test (:name (:redefining-condition-with-non-constant-default-initarg
                   :bug-1164969))
  ;; Redefining conditions could lead to multiple evaluations of
  ;; initfunctions for slots and default initargs. We need all the
  ;; combinations of make-condition/instance and eval/compile because
  ;; they failed differently.
  (macrolet ((test (case &body body)
               `(progn
                  (setf bar-counter 0
                        baz-counter 0)
                  (let ((instance (progn ,@body)))
                    (assert
                     (= 1 (condition-with-non-constant-default-initarg-bar
                           instance))
                     nil
                     ,(format nil "Assertion failed for default initarg initfunction for ~A"
                              case))
                    (assert
                     (= 1 (condition-with-non-constant-default-initarg-baz
                           instance))
                     nil
                     ,(format nil "Assertion failed for slot initfunction for ~A"
                              case)))
                  (assert (= 1 bar-counter))
                  (assert (= 1 baz-counter)))))

    ;; Go through EVAL to avoid optimizations.
    (test :eval+make-condition
      (eval '(make-condition
              'condition-with-non-constant-default-initarg)))
    (test :eval+make-instance
      (eval '(make-instance
              'condition-with-non-constant-default-initarg)))

    ;; Allow optimizations.
    (test :compile+make-condition
      (make-condition
       'condition-with-non-constant-default-initarg))
    (test :compile+make-instance
      (make-instance
       'condition-with-non-constant-default-initarg))))

;;; bug-1049404

(define-condition condition-with-class-allocation ()
  ((count :accessor condition-with-class-allocation-count
          :initform 0
          :allocation :class)))

(with-test (:name (:condition-with-class-allocation :bug-1049404))
  (loop repeat 5 do
           (incf (condition-with-class-allocation-count
                  (make-condition 'condition-with-class-allocation))))
  (assert (= 5 (condition-with-class-allocation-count
                (make-condition 'condition-with-class-allocation)))))

;;; bug-789497

(with-test (:name (assert :print-intermediate-results :bug-789497))
  (macrolet ((test (bindings expression expected-message)
               `(let ,bindings
                  (handler-case (assert ,expression)
                    (simple-error (condition)
                      (assert (string= (princ-to-string condition)
                                       ,expected-message)))))))
    ;; Constant and variables => no special report.
    (test () nil "The assertion NIL failed.")
    (test ((a nil)) a "The assertion A failed.")
    ;; Special operators => no special report.
    (test ((a nil) (b nil)) (or a b) "The assertion (OR A B) failed.")
    (test ((a nil) (b t)) (and a b) "The assertion (AND A B) failed.")
    ;; Functions with constant and non-constant arguments => include
    ;; non-constant arguments in report.
    (test ((a t)) (not a) "The assertion (NOT A) failed with A = T.")
    (test () (not t) "The assertion (NOT T) failed.")
    (test ((a -1)) (plusp (signum a))
          "The assertion (PLUSP (SIGNUM A)) failed with (SIGNUM A) = -1.")
    ;; Same for local functions.
    (flet ((my-not (x) (not x))
           (my-plusp (x) (plusp x)))
      (test ((a t)) (my-not a) "The assertion (MY-NOT A) failed with A = T.")
      (test () (my-not t) "The assertion (MY-NOT T) failed.")
      (test ((a -1)) (my-plusp (signum a))
            "The assertion (MY-PLUSP (SIGNUM A)) failed with (SIGNUM A) = -1."))))

;; If ASSERT thinks it's checking a function call form, it binds the arguments
;; to local vars so that it can show them on failure. But it would accidentally
;; treat a local macro as a function if its name coincided with a global.
(defun compute (a b) (declare (ignore a b)))
(defun compute-something-else (a b) (list a b))
(defun assert-arg-eval-order (a b)
  (let ((evals))
    (flet ((eval-arg (arg) (setq evals (nconc evals (list arg))) arg))
      (macrolet ((compute (x y z)
                   (declare (ignore y)) ; does not appear in expansion
                   `(compute-something-else ,z ,x)))
        (assert (compute (eval-arg a)
                         (eval-arg (error "bork"))
                         (eval-arg b)))))
    evals))
(with-test (:name :assert-arg-eval-order)
  (assert (equal (assert-arg-eval-order 1 2) '(2 1))))

(with-test (:name (find-restart :recheck-conditions-and-tests :bug-774410))
  (let ((activep t))
    (restart-bind ((switchable-restart
                     (constantly 'irrelevant)
                     :test-function (lambda (condition)
                                      (declare (ignore condition))
                                      activep)))
      (let ((actual-restart (find-restart 'switchable-restart)))
        ;; Active/inactive because of condition-restarts associations.
        (let ((required-condition (make-condition 'condition))
              (wrong-condition (make-condition 'condition)))
          (with-condition-restarts required-condition (list actual-restart)
            (assert (find-restart actual-restart required-condition))
            (assert (not (find-restart actual-restart wrong-condition)))))

        ;; Inactive because of test-function.
        (setf activep nil)
        (assert (not (find-restart actual-restart)))
        (assert (not (find-restart actual-restart (make-condition 'condition))))))))

(macrolet
    ((with-testing-restart ((&key
                               (condition-var (gensym))
                               (condition-restart-p t))
                            &body body)
       `(block block
          (handler-bind ((condition (lambda (,condition-var)
                                      (declare (ignorable ,condition-var))
                                      ,@body)))
            (restart-bind ((testing-restart
                             (lambda () (return-from block :transfer))
                             :test-function (lambda (condition)
                                              (typep condition 'condition))))
              ,(if condition-restart-p
                   `(let ((condition (make-condition 'condition)))
                      (with-condition-restarts condition (car sb-kernel:*restart-clusters*)
                        (signal condition)))
                   `(signal (make-condition 'condition)))
              :no-transfer)))))

  (with-test (:name (invoke-restart :test-function))

    ;; When given a restart name, there is no condition so
    ;; INVOKE-RESTART cannot call the :test-function. SBCL considers
    ;; the restart unsuitable for the requested invocation. See
    ;; comment in INVOKE-RESTART.
    (assert-error (with-testing-restart ()
                    (invoke-restart 'testing-restart))
                  control-error)

    ;; When given a RESTART instance (which could only have been found
    ;; by passing an appropriate condition to FIND-RESTART),
    ;; INVOKE-RESTART does not call the :test-function. Again, see
    ;; comment in INVOKE-RESTART.
    (assert (eq :transfer
                (with-testing-restart (:condition-var condition)
                  (invoke-restart
                   (find-restart 'testing-restart condition)))))

    ;; Some other condition, even if it passes the :test-function,
    ;; only works if the restart has not been associated to the
    ;; original condition.
    (assert (eq :transfer
                (with-testing-restart (:condition-restart-p nil)
                  (invoke-restart
                   (find-restart 'testing-restart (make-condition 'condition))))))
    (with-testing-restart ()
      (assert (not (find-restart 'testing-restart (make-condition 'condition))))))

  (with-test (:name (invoke-restart-interactively :test-function))
    ;; Comments in (INVOKE-RESTART :TEST-FUNCTION) apply here as well.
    (assert-error
     (with-testing-restart ()
       (invoke-restart-interactively 'testing-restart))
     control-error)

    (assert (eq :transfer
                (with-testing-restart (:condition-var condition)
                  (invoke-restart-interactively
                   (find-restart 'testing-restart condition)))))

    (assert (eq :transfer
                (with-testing-restart (:condition-restart-p nil)
                  (invoke-restart-interactively
                   (find-restart 'testing-restart (make-condition 'condition))))))))

(defun case-failure-example (x) (etypecase x (function 1) (symbol 2)))
;; The :report method should not print "wanted one of #'SYMBOL"
(with-test (:name :case-failure-report-pprint-silliness)
  (handler-case (case-failure-example 3)
    (condition (c)
      (let ((str (write-to-string c :escape nil :pretty t)))
        (assert (not (search "#'SYMBOL" str)))))))

(define-condition a-condition-with-a-classy-slot ()
  ((a :allocation :class :initform 'foo)))

(defvar *a-classy-classoid*
  (sb-kernel:find-classoid 'a-condition-with-a-classy-slot))
;; precondition to the test
(assert (= (length (sb-kernel::condition-classoid-class-slots
                    *a-classy-classoid*))
           1))

(define-condition a-condition-with-a-classy-slot ()
  ((b :allocation :class :initform 'baz)
   (a :allocation :class :initform 'foo)))

(with-test (:name :condition-classoid-redef-with-class-slot)
  (assert (= (length (sb-kernel::condition-classoid-class-slots
                      *a-classy-classoid*))
             2)))

(defun some-silly-handler (x) (declare (ignore x)))

;; Note, you should exercise extreme caution when printing *HANDLER-CLUSTERS*
;; from the REPL or you will likely confuse yourself.
;; At any point, the clusters are composed of a chain of dx conses.
;; One way to print them is like this:
;;  (funcall (lambda () (print sb-kernel::*handler-clusters*) nil))

(with-test (:name :handler-bind-expansion-variations)

  ;; exercise all 3 possibilities for the TEST of a clause
  (handler-bind ((error #'print))
    ;; first cluster, first binding, first part
    (let ((test (caaar sb-kernel:*handler-clusters*)))
      (assert (eq test (sb-kernel:find-classoid-cell 'error)))))
  (locally (declare (muffle-conditions style-warning))
    (handler-bind (((satisfies some-silly-fun) #'print))
      (let ((test (caaar sb-kernel:*handler-clusters*)))
        (assert (eq test (sb-int:find-fdefn 'some-silly-fun))))))
  (handler-bind (((or warning error) #'print))
    (let ((test (caaar sb-kernel:*handler-clusters*)))
      (assert (functionp test))))

  ;; exercise all 3 possibilities for the HANDLER of a clause

  (flet ((frob (x) (print x)))
    (handler-bind ((warning
                    (lambda (c) (frob (frob c)))))
      ;; first cluster, first binding, second part
      (let ((fn (cdaar sb-kernel:*handler-clusters*)))
        (assert (functionp fn)))))

  (handler-bind ((warning (eval ''muffle-warning)))
    (let ((fn (cdaar sb-kernel:*handler-clusters*)))
      ;; the function is stored as a symbol
      (assert (eq fn 'muffle-warning))))

  (handler-bind ((warning 'some-silly-handler))
    (let ((fn (cdaar sb-kernel:*handler-clusters*)))
      ;; the function is stored as an fdefn
      (assert (typep fn 'sb-kernel::fdefn))))

  (handler-bind ((warning 'muffle-warning))
    (let ((fn (cdaar sb-kernel:*handler-clusters*)))
      ;; the function is stored directly because it's a builtin.
      (assert (eq fn (symbol-function 'muffle-warning))))))

;; Oddly enough, at least one other Lisp considers this example
;; *not* to fail, quite explicitly - the handler function in the
;; handler-cluster is a lambda that returns a function that is
;; the real handler function.
(defun this-should-fail ()
  (declare (muffle-conditions style-warning))
  (handler-bind ((condition #'some-nonexistent-handler))
    (random 100)))

(with-test (:name :handler-bind-evals-handlers-immediately)
  (assert-error (this-should-fail))
  (defun some-nonexistent-handler (x) x)
  (assert (integerp (this-should-fail)))) ; but not now it shouldn't
