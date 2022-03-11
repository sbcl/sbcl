;;;; tests for the DESCRIBE function

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

(defmacro assert-non-empty-output (&body forms)
  `(assert (plusp (length (with-output-to-string (*standard-output*)
                            ,@forms)))))

(defstruct to-be-described a b)

(defclass forward-describe-class (forward-describe-ref) (a))

(defclass non-standard-generic-function (generic-function) ()
  (:metaclass sb-mop:funcallable-standard-class))
(defmethod sb-mop:generic-function-name ((generic-function non-standard-generic-function))
  'name)

(with-test (:name (describe :empty-gf))
  (assert-no-signal
   (assert-non-empty-output
    (describe (make-instance 'non-standard-generic-function)))
   warning)
  (assert-signal
   (assert-non-empty-output
    (describe (make-instance 'standard-generic-function)))
   warning))

;;; DESCRIBE should run without signalling an error.
(with-test (:name (describe :no-error))
  (assert-non-empty-output (describe (make-to-be-described)))
  (assert-non-empty-output (describe 12))
  (assert-non-empty-output (describe "a string"))
  (assert-non-empty-output (describe 'symbolism))
  (assert-non-empty-output (describe (find-package :cl)))
  (assert-non-empty-output (describe '(a list)))
  (assert-non-empty-output (describe #(a vector))))

(let ((sb-ext:*evaluator-mode* :compile))
  (eval `(let (x) (defun closure-to-describe () (incf x)))))

(with-test (:name (describe :no-error :closure :bug-824974))
  (assert-non-empty-output (describe 'closure-to-describe)))

;;; DESCRIBE shouldn't fail on rank-0 arrays (bug reported and fixed
;;; by Lutz Euler sbcl-devel 2002-12-03)
(with-test (:name (describe :no-error array :rank 0))
  (assert-non-empty-output (describe #0a0))
  (assert-non-empty-output (describe #(1 2 3)))
  (assert-non-empty-output (describe #2a((1 2) (3 4)))))

(defclass cannot-print-this () ())
(defmethod print-object ((object cannot-print-this) stream)
  (error "No go!"))

(with-test (:name (describe :no-error print-object))
  ;; Errors during printing objects used to be suppressed in a way
  ;; that required outer condition handlers to behave in a specific
  ;; way.
  (handler-bind ((error (lambda (condition)
                          (error "~@<~S signaled ~A.~@:>"
                                 'describe condition))))
    (assert-non-empty-output (describe (make-instance 'cannot-print-this)))))

;;; The DESCRIBE-OBJECT methods for built-in CL stuff should do
;;; FRESH-LINE and TERPRI neatly.
(with-test (:name (describe fresh-line terpri))
  (dolist (i (list (make-to-be-described :a 14) 12 "a string"
                   #0a0 #(1 2 3) #2a((1 2) (3 4)) 'sym :keyword
                   (find-package :keyword) (list 1 2 3)
                   nil (cons 1 2) (make-hash-table)
                   (let ((h (make-hash-table)))
                     (setf (gethash 10 h) 100
                           (gethash 11 h) 121)
                     h)
                   (make-condition 'simple-error)
                   (make-condition 'simple-error :format-control "fc")
                   #'car #'make-to-be-described (lambda (x) (+ x 11))
                   (constantly 'foo) #'(setf to-be-described-a)
                   #'describe-object (find-class 'to-be-described)
                   (find-class 'forward-describe-class)
                   (find-class 'forward-describe-ref) (find-class 'cons)))
    (let ((s (with-output-to-string (s)
               (write-char #\x s)
               (describe i s))))
      (macrolet ((check (form)
                   `(or ,form
                        (error "misbehavior in DESCRIBE of ~S:~%   ~S" i ',form))))
        (check (char= #\x (char s 0)))
        ;; one leading #\NEWLINE from FRESH-LINE or the like, no more
        (check (char= #\newline (char s 1)))
        (check (char/= #\newline (char s 2)))
        ;; one trailing #\NEWLINE from TERPRI or the like, no more
        (let ((n (length s)))
          (check (char= #\newline (char s (- n 1))))
          (check (char/= #\newline (char s (- n 2)))))))))

(with-test (:name (describe :argument-precedence-order))
  ;; Argument precedence order information is only interesting for two
  ;; or more required parameters.
  (assert (not (search "Argument precedence order"
                       (with-output-to-string (stream)
                         (describe #'class-name stream)))))
  (assert (search "Argument precedence order"
                  (with-output-to-string (stream)
                    (describe #'add-method stream)))))

(defun lottafun (x y z &rest r &key ((:wat w)) glup)
  (declare (dynamic-extent glup z r w x))
  (declare (ignore x y z r w glup))
  (print 'hi))

(with-test (:name (describe :fun-dx-args))
  ;; though R is DX, it is not useful information to show,
  ;; because the caller doesn't decide how to allocate the &rest list.
  (assert (search "Dynamic-extent arguments: positional=(0 2), keyword=(:WAT :GLUP)"
                  (with-output-to-string (stream)
                    (describe #'lottafun stream)))))

(with-test (:name (describe sb-kernel:funcallable-instance))
  (assert (search "Slots with :INSTANCE allocation"
                  (with-output-to-string (stream)
                    (describe #'class-name stream)))))

(with-test (:name (describe class))
  (assert (search "Class precedence-list:"
                  (with-output-to-string (stream)
                    (describe (find-class 'standard-class) stream)))))

(proclaim '(declaration my-declaration))

(with-test (:name (describe declaration))
  (flet ((test (name expected-description)
           (assert (search expected-description
                           (with-output-to-string (stream)
                             (describe name stream))))))
    (test 'inline "INLINE names a standard declaration.")
    (test 'sb-ext:deprecated "DEPRECATED names an SBCL-specific declaration.")
    (test 'my-declaration "MY-DECLARATION names a user-defined declaration.")))

(with-test (:name (describe array :displaced-to))
  (assert (search "Displaced: no"
                  (with-output-to-string (stream)
                    (describe (make-array 1 :adjustable t) stream))))
  (assert (search "Displaced-to: #<"
                  (with-output-to-string (stream)
                    (describe (make-array 1 :displaced-to (make-array 1))
                              stream)))))

(with-test (:name :bad-second-arg)
  (assert-error
   (describe 'describe
             (opaque-identity
              (make-array 256 :element-type 'character :fill-pointer 0)))
   type-error))
