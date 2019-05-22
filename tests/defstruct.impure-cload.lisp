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

(eval-when (:compile-toplevel)
  (load "compiler-test-util.lisp"))

;;; Prior to the change that allowed forward-references to slot types,
;;; the MAKE-S1 constructor would have used a "cached typep" placeholder
;;; for structure types S2 and S3; and MAKE-S2 would have used one for S3.
;;; The placeholder lazily figures out that a symbol references a now-defined
;;; defstruct, and it tries to precompute a way to as-efficiently-as-possible
;;; test for that type, given that it couldn't wire in the test to start with.
;;; Only S3 would have been compiled correctly from the outset because it
;;; makes backwards references and no forward references.
;;;
;;; But now with the DEFSTRUCT improvements, the type checks for the slot
;;; named A all compile to basically the same thing in each MAKE- function,
;;; without use of placeholders nor just-in-time optimization attempts.
;;;
(defstruct s1 (a nil :type (or s1 s2 s3 null)))
(defstruct s2 (a nil :type (or s1 s2 s3 null)))
(defstruct s3 (a nil :type (or s1 s2 s3 null)))

(with-test (:name :defstruct-slot-type-circularity)
  (dolist (symbol '(make-s1 make-s2 make-s3))
    (let ((constants
           (ctu:find-code-constants (symbol-function symbol)
                                    :type 'sb-kernel:layout)))
      (assert (= (length constants) 3)))))
