;;;; nonportable floating point things, useful in LOAD-TIME-VALUE
;;;; forms for referring to floating point objects that will exist on
;;;; the SBCL target but may not when running under an ordinary ANSI
;;;; Common Lisp implementation.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun make-unportable-float (name)
  (flet ((opaque-identity (x) x))
    ;; KLUDGE: "DO NOT CONSTANT FOLD, EVIL COMPILER!"
    (declare (notinline opaque-identity make-single-float make-double-float))
    (ecase name
      (:single-float-negative-zero (make-single-float
                                    (opaque-identity #x-80000000)))
      (:double-float-negative-zero (make-double-float
                                    (opaque-identity #x-80000000)
                                    (opaque-identity #x00000000)))
      #!+long-float
      (:long-float-negative-zero (error "write LONG-FLOAT creation form")))))
