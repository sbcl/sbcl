;;;; weak pointer support

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-IMPL")

(defun make-weak-pointer (object)
  "Allocate and return a weak pointer which points to OBJECT."
  (make-weak-pointer object))

(declaim (inline weak-pointer-value))
(defun weak-pointer-value (weak-pointer)
  "If WEAK-POINTER is valid, return the value of WEAK-POINTER and T.
If the referent of WEAK-POINTER has been garbage collected,
returns the values NIL and NIL."
  (declare (type weak-pointer weak-pointer))
  (let ((value (sb-vm::%weak-pointer-value weak-pointer)))
    (if (sb-vm::unbound-marker-p value)
        (values nil nil)
        (values value t))))
