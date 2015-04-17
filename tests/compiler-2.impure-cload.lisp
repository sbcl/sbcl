;;;; -*- lisp -*-

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

;;;; recognize self-calls
(declaim (optimize speed))

;;; This is a fopcompilable form that caused FOP stack underflow
;;; because the PROGN and SETQ each failed to push a NIL onto the stack.
;;; >>> DO NOT ADD A (WITH-TEST) TO THIS. <<< It must stay fopcompilable.
(let ((a (progn)) ; lp# 1427050
      (b (setq))
      (b (+ 1 2)))
  (defvar *aaa* a)
  (defvar *bbb* b))


;;;; These three forms should be equivalent.

;;; This used to be a bug in the handling of null-lexenv vs toplevel
;;; policy: LOCALLY and MACROLET hid the toplevel policy from view.

(locally
    (defun foo (n)
      (frob 'foo)
      (if (<= n 0)
          n
          (foo (1- n)))))

(progn
  (defun bar (n)
    (frob 'bar)
    (if (<= n 0)
        n
        (bar (1- n)))))

(macrolet ()
  (defun quux (n)
    (frob 'quux)
    (if (<= n 0)
        n
        (quux (1- n)))))

(defun frob (x)
  (setf (fdefinition x) (constantly 13)))

(defun test ()
  (list (foo 1) (bar 1) (quux 1)))

(assert (equal (test) '(0 0 0)))
(assert (equal (test) '(13 13 13))) ; sanity check

;;; Bug in 1.0.2 and 1.0.3, where the XEP was compiled with the wrong
;;; policy. (Test-case derived from code posted by alexander.ekart in
;;; comp.lang.lisp).

(locally
    (declare (optimize (safety 0)))
  (defun bubblesort (x y)
    (declare (type (simple-array fixnum (*)) x)
             (type fixnum y)
             (optimize (speed 3) (safety 3) (space 0) (debug 0)))
    (aref x y)))

(assert-error (bubblesort (make-array 10) 9))

(define-symbol-macro %trash% what)
(locally
  ;; just in case we get so smart that INFO becomes foldable
  (declare (notinline sb-int:info))
  (assert (eq (sb-int:info :variable :kind '%trash%) :macro))
  (assert (eq (sb-int:info :variable :macro-expansion '%trash%) 'what))
  (assert (sb-int:info :source-location :symbol-macro '%trash%)))
(let ()
  (declare (notinline sb-int:info))
  (defconstant %trash% 9) ; this is non-toplevel
  (multiple-value-bind (val foundp)
      (sb-int:info :variable :macro-expansion '%trash%)
    (assert (and (not val) (not foundp)))))

;;; COMPILE-FILE-POSITION

(macrolet ((line () `(multiple-value-call 'cons (compile-file-position))))
  (defun more-foo (x)
    (if x
        (format nil "Great! ~D" (line)) ; <-- this is line 97
        (format nil "Yikes ~D" (line)))))

(declaim (inline thing))
(defun thing ()
  (format nil "failed to frob a knob at line #~D"
          (compile-file-position))) ; <-- this is line 103

(defmacro more-randomness ()
  '(progn
    (let ()
      (thing))))

(macrolet ()
  (progn
    (defun bork (x)
      (flet ()
        (if x
            (locally (declare (notinline thing))
              (more-randomness))
            (progn (more-randomness))))))) ; <-- this is line 117

(with-test (:name :compile-file-position)
  (assert (string= (more-foo t) "Great! (97 . 32)"))
  (assert (string= (more-foo nil) "Yikes (98 . 31)"))
  (assert (string= (bork t) "failed to frob a knob at line #103"))
  (assert (string= (bork nil) "failed to frob a knob at line #117")))
