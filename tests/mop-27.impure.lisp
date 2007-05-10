;;;; miscellaneous side-effectful tests of the MOP

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

;;; a test of a non-standard specializer class.  Some context: a
;;; (mostly content-free) discussion on comp.lang.lisp around
;;; 2007-05-08 about the merits of Lisp, wherein an F#/OCaml advocate
;;; implies roughly "I've heard that CLOS is slower than pattern
;;; matching"

;;; This implements a generic function type which dispatches on
;;; patterns in its methods.  The implementation below is a simple
;;; interpreter of patterns; compiling the patterns into a
;;; discrimination net, or other optimized dispatch structure, would
;;; be an interesting exercise for the reader.  (As would fixing some
;;; other marked issues).

(defpackage "MOP-27"
  (:use "CL" "SB-MOP"))

(in-package "MOP-27")

(defclass pattern-specializer (specializer)
  ((pattern :initarg pattern :reader pattern)
   (direct-methods :initform nil :reader specializer-direct-methods)))

(defvar *pattern-specializer-table* (make-hash-table :test 'equal))

(defun ensure-pattern-specializer (pattern)
  (or (gethash pattern *pattern-specializer-table*)
      (setf (gethash pattern *pattern-specializer-table*)
            (make-instance 'pattern-specializer 'pattern pattern))))

;;; only one arg for now
(defclass pattern-gf/1 (standard-generic-function) ()
  (:metaclass funcallable-standard-class))

(defmethod compute-discriminating-function ((generic-function pattern-gf/1))
  (lambda (arg)
    (let* ((methods (generic-function-methods generic-function))
           (function (method-interpreting-function methods generic-function)))
      (set-funcallable-instance-function generic-function function)
      (funcall function arg))))

(defun method-interpreting-function (methods gf)
  (lambda (arg)
    (dolist (method methods (no-applicable-method gf (list arg)))
      (when (matchesp arg (pattern (car (method-specializers method))))
        (return (funcall (method-function method) (list arg) nil))))))

(defun matchesp (arg pattern)
  (cond
    ((null pattern) t)
    ((atom pattern) (eql arg pattern))
    (t (and (matchesp (car arg) (car pattern))
            (matchesp (cdr arg) (cdr pattern))))))


;;; protocol functions.  SPECIALIZER-DIRECT-METHODS is implemented by
;;; a reader on the specializer.  FIXME: implement
;;; SPECIALIZER-DIRECT-GENERIC-FUNCTIONS.
(defmethod add-direct-method ((specializer pattern-specializer) method)
  (pushnew method (slot-value specializer 'direct-methods)))
(defmethod remove-direct-method ((specializer pattern-specializer) method)
  (setf (slot-value specializer 'direct-methods)
        (remove method (slot-value specializer 'direct-methods))))

(defgeneric simplify (x)
  (:generic-function-class pattern-gf/1))
;;; KLUDGE: order of definition matters, as we simply traverse
;;; generic-function-methods until a pattern matches our argument.
;;; Additionally, we're not doing anything interesting with regard to
;;; destructuring the pattern for use in the method body; a real
;;; implementation would make it more convenient.
(let ((specializer (ensure-pattern-specializer 'nil)))
  (eval `(defmethod simplify ((x ,specializer)) x)))
(let ((specializer (ensure-pattern-specializer '(* nil 0))))
  (eval `(defmethod simplify ((x ,specializer)) 0)))
(let ((specializer (ensure-pattern-specializer '(* 0 nil))))
  (eval `(defmethod simplify ((x ,specializer)) 0)))

(assert (eql (simplify '(* 0 3)) 0))
(assert (eql (simplify '(* (+ x y) 0)) 0))
(assert (equal (simplify '(+ x y)) '(+ x y)))
