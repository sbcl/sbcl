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
  (* unsigned-long)
  (context (* os-context-t))
  (index int))

(define-alien-routine ("os_context_pc_addr" context-register-pc-addr)
  (* unsigned-long)
  (context (* os-context-t)))
(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* unsigned) (context (* os-context-t)) (index int))

;;; FIXME: Should this and CONTEXT-PC be INLINE to reduce consing?
;;; (Are they used in anything time-critical, or just the debugger?)
(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (deref (context-register-addr context index)))

(defun %set-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (setf (deref (context-register-addr context index))
        new))

(defun context-float-register (context index format)
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (sap-ref-single sap 0))
      (double-float
       (sap-ref-double sap 0))
      (complex-single-float
       (complex (sap-ref-single sap 0)
                (sap-ref-single sap 4)))
      (complex-double-float
       (complex (sap-ref-double sap 0)
                (sap-ref-double sap 8))))))

(defun %set-context-float-register (context index format value)
  (let ((sap (alien-sap (context-float-register-addr context index))))
    (ecase format
      (single-float
       (setf (sap-ref-single sap 0) value))
      (double-float
       (setf (sap-ref-double sap 0) value))
      (complex-single-float
       (locally
           (declare (type (complex single-float) value))
         (setf (sap-ref-single sap 0) (realpart value)
               (sap-ref-single sap 4) (imagpart value))))
      (complex-double-float
       (locally
           (declare (type (complex double-float) value))
         (setf (sap-ref-double sap 0) (realpart value)
               (sap-ref-double sap 8) (imagpart value)))))))

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
         (error-number (ldb (byte 8 13) instruction)))
    (declare (type system-area-pointer pc))
    (if (= (ldb (byte 8 5) instruction) invalid-arg-count-trap)
        (values error-number '(#.arg-count-sc))
        (let* ((length (sap-ref-8 pc 4))
               (vector (make-array length :element-type '(unsigned-byte 8)))
               (index 0))
          (declare (type (unsigned-byte 8) length)
                   (type (simple-array (unsigned-byte 8) (*)) vector))
          (copy-ub8-from-system-area pc 5 vector 0 length)
          (collect ((sc-offsets))
            (loop
             (when (>= index length)
               (return))
             (sc-offsets (sb!c:read-var-integer vector index)))
            (values error-number (sc-offsets)))))))
