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

;;; Check that initializing the condition class metaobject doesn't create
;;; any instances. Reported by Marco Baringer on sbcl-devel Mon, 05 Jul 2004.
(defvar *condition-count* 0)
(define-condition counted-condition () ((slot :initform (incf *condition-count*))))
(defmethod frob-counted-condition ((x counted-condition)) x)
(assert (= 0 *condition-count*))
(assert (typep (sb-mop:class-prototype (find-class 'counted-condition))
               '(and condition counted-condition)))

(define-condition picky-condition () ())
(restart-case
    (handler-case
        (error 'picky-condition)
      (picky-condition (c)
        (assert (eq (car (compute-restarts)) (car (compute-restarts c))))))
  (picky-restart ()
    :report "Do nothing."
    :test (lambda (c)
            (typep c '(or null picky-condition)))
    'ok))

;;; adapted from Helmut Eller on cmucl-imp
(assert (eq 'it
            (restart-case
                (handler-case
                    (error 'picky-condition)
                  (picky-condition (c)
                    (invoke-restart (find-restart 'give-it c))))
              (give-it ()
                :test (lambda (c) (typep c 'picky-condition))
                'it))))

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
      (write c :stream (make-string-output-stream)))))

;;; Reported by Michael Weber: restart computation in :TEST-FUNCTION used to
;;; cause infinite recursion.
(defun restart-test-finds-restarts ()
  (restart-bind
      ((bar (lambda ()
              (return-from restart-test-finds-restarts 42))
         :test-function
         (lambda (condition)
           (find-restart 'qux))))
    (when (find-restart 'bar)
      (invoke-restart 'bar))))
(assert (not (restart-test-finds-restarts)))
