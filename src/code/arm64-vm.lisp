;;; This file contains the ARM specific runtime stuff.
;;;
(in-package "SB!VM")

;;; See x86-vm.lisp for a description of this.
;;; FIXME: Why is this present in every ARCH-vm.lisp with the the same definition. Is there something like common-vm?
(define-alien-type os-context-t (struct os-context-t-struct))

(defun machine-type ()
  #!+sb-doc
  "Return a string describing the type of the local machine."
  "ARM64")

;;;; FIXUP-CODE-OBJECT

(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (unless (zerop (rem offset 4))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (without-gcing
    (let ((sap (%primitive code-instructions code)))
      (ecase kind
        (:absolute
         (setf (sap-ref-word sap offset) fixup))
        (:cond-branch
         (setf (ldb (byte 19 5) (sap-ref-32 sap offset))
               (ash (- fixup (+ (sap-int sap) offset)) -2)))
        (:uncond-branch
         (setf (ldb (byte 26 0) (sap-ref-32 sap offset))
               (ash (- fixup (+ (sap-int sap) offset)) -2)))))))

;;;; "Sigcontext" access functions, cut & pasted from sparc-vm.lisp,
;;;; then modified for ARM.
;;;;
;;;; See also x86-vm for commentary on signed vs unsigned.

(define-alien-routine ("os_context_register_addr" context-register-addr)
  (* unsigned-int)
  (context (* os-context-t))
  (index int))

(define-alien-routine ("os_context_pc_addr" context-register-pc-addr)
  (* unsigned-int)
  (context (* os-context-t)))

;;; FIXME: Should this and CONTEXT-PC be INLINE to reduce consing?
;;; (Are they used in anything time-critical, or just the debugger?)
(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (deref (context-register-addr context index)))

(defun %set-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (setf (deref (context-register-addr context index))
        new))

(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (int-sap (deref (context-register-pc-addr context))))

;;;; INTERNAL-ERROR-ARGS.

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (instruction (sap-ref-32 pc 0))
         (length (sap-ref-8 pc 4))
         (vector (make-array length :element-type '(unsigned-byte 8)))
         (index 0)
         (error-number (ldb (byte 8 13) instruction)))
    (declare (type system-area-pointer pc)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (copy-ub8-from-system-area pc 5 vector 0 length)
    (collect ((sc-offsets))
      (loop
       (when (>= index length)
         (return))
       (sc-offsets (sb!c:read-var-integer vector index)))
      (values error-number (sc-offsets)))))
