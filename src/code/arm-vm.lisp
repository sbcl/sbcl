;;; This file contains the ARM specific runtime stuff.
;;;
(in-package "SB!VM")

;;; See x86-vm.lisp for a description of this.
;;; FIXME: Why is this present in every ARCH-vm.lisp with the the same definition. Is there something like common-vm?
(define-alien-type os-context-t (struct os-context-t-struct))

(defun machine-type ()
  #!+sb-doc
  "Return a string describing the type of the local machine."
  "ARM")

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

;;;; "Sigcontext" access functions, cut & pasted from sparc-vm.lisp,
;;;; then modified for ARM.
;;;;
;;;; See also x86-vm for commentary on signed vs unsigned.

(define-alien-routine ("os_context_register_addr" context-register-addr)
  (* unsigned-int)
  (context (* os-context-t))
  (index int))

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
  (int-sap (context-register context pc-offset)))

;;;; INTERNAL-ERROR-ARGS.

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (length (sap-ref-8 pc 5)) ;; Skip trap instruction and kind byte
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system-area-pointer pc)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (copy-ub8-from-system-area pc 6 vector 0 length)
    (let* ((index 0)
           (error-number (sb!c:read-var-integer vector index)))
      (collect ((sc-offsets))
               (loop
                (when (>= index length)
                  (return))
                (sc-offsets (sb!c:read-var-integer vector index)))
               (values error-number (sc-offsets))))))
