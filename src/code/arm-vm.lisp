;;; This file contains the ARM specific runtime stuff.
;;;
(in-package "SB!VM")

;;; See x86-vm.lisp for a description of this.
;;; FIXME: Why is this present in every ARCH-vm.lisp with the the same definition. Is there something like common-vm?
(define-alien-type os-context-t (struct os-context-t-struct))

;;;; FIXUP-CODE-OBJECT

(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (unless (zerop (rem offset n-word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (sb!sys:without-gcing
   (let ((sap (%primitive sb!kernel::code-instructions code)))
     (ecase kind
       (:absolute
        (setf (sap-ref-32 sap offset) fixup))))))
