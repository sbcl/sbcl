;;; This file contains the ARM specific runtime stuff.
;;;
(in-package "SB!VM")

#-sb-xc-host
(defun machine-type ()
  "Return a string describing the type of the local machine."
  "ARM")

;;;; FIXUP-CODE-OBJECT

(defconstant-eqx +fixup-kinds+ #(:absolute) #'equalp)
(!with-bigvec-or-sap
(defun fixup-code-object (code offset fixup kind flavor)
  (declare (type index offset))
  (declare (ignore flavor))
  (unless (zerop (rem offset sb!assem:+inst-alignment-bytes+))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (let ((sap (code-instructions code)))
    (ecase kind
      (:absolute
       (setf (sap-ref-32 sap offset) fixup))))
  nil))

;;;; "Sigcontext" access functions, cut & pasted from sparc-vm.lisp,
;;;; then modified for ARM.
;;;;
;;;; See also x86-vm for commentary on signed vs unsigned.

#-sb-xc-host (progn
(defun context-float-register (context index format)
  (declare (ignorable context index))
  (warn "stub CONTEXT-FLOAT-REGISTER")
  (coerce 0 format))

(defun %set-context-float-register (context index format new-value)
  (declare (ignore context index))
  (warn "stub %SET-CONTEXT-FLOAT-REGISTER")
  (coerce new-value format))

;;;; INTERNAL-ERROR-ARGS.

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (trap-number (sap-ref-8 pc 4)))
    (declare (type system-area-pointer pc))
    (sb!kernel::decode-internal-error-args (sap+ pc 5) trap-number)))
) ; end PROGN
