;;; This file contains the ARM specific runtime stuff.
;;;
(in-package "SB!VM")

;;; See x86-vm.lisp for a description of this.
;;; FIXME: Why is this present in every ARCH-vm.lisp with the the same definition. Is there something like common-vm?
(define-alien-type os-context-t (struct os-context-t-struct))

;;; Dummy placeholder file.
