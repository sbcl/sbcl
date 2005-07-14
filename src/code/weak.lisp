;;;; weak pointer support

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!IMPL")

(defun make-weak-pointer (object)
  #!+sb-doc
  "Allocate and return a weak pointer which points to OBJECT."
  (make-weak-pointer object))

#!-sb-fluid (declaim (inline weak-pointer-value))
(defun weak-pointer-value (weak-pointer)
  #!+sb-doc
  "If WEAK-POINTER is valid, return the value of WEAK-POINTER and T.
If the referent of WEAK-POINTER has been garbage collected,
returns the values NIL and NIL."
  (declare (type weak-pointer weak-pointer))
  ;; We don't need to wrap this with a WITHOUT-GCING, because once we
  ;; have extracted the value, our reference to it will keep the weak
  ;; pointer from becoming broken. We just have to make sure the
  ;; compiler won't reorder these primitives.
  ;;
  ;; FIXME: Might it be a good idea to tweak the DEFKNOWNs for
  ;; %WEAK-POINTER-VALUE and %WEAK-POINTER-BROKEN, so that the
  ;; compiler will never try to reorder them even in code where we
  ;; neglect to frame them in a LET?
  (let ((value (sb!c::%weak-pointer-value weak-pointer))
        (broken (sb!c::%weak-pointer-broken weak-pointer)))
    (values value (not broken))))
