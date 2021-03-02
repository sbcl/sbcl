;;; This file contains the RISC-V-specific runtime stuff.

(in-package "SB-VM")

(defun machine-type ()
  "Return a string describing the type of the local machine."
  #-64-bit "RV32G" #+64-bit "RV64G")

(defun return-machine-address (scp)
  ;; KLUDGE: Taken from SPARC backend. Why does `8' need to be added
  ;; to the return address? Without it, backtraces get truncated and
  ;; are incorrect. Are the other backends wrong as well by not adding
  ;; 8?
  (+ (context-register scp lip-offset) 8))


;;; CONTEXT-FLOAT-REGISTER
(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* unsigned) (context (* os-context-t)) (index int))

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

;;; INTERNAL-ERROR-ARGS

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (trap-number (sap-ref-8 pc 4)))
    (declare (type system-area-pointer pc))
    (if (= trap-number invalid-arg-count-trap)
        (values #.(error-number-or-lose 'invalid-arg-count-error)
                '(#.arg-count-sc))
        (sb-kernel::decode-internal-error-args (sap+ pc 5) trap-number))))

;;; CONTEXT-CALL-FUNCTION

;;; Undo the effects of XEP-ALLOCATE-FRAME
;;; and point PC to FUNCTION
(defun context-call-function (context function &optional arg-count)
  (declare (ignore context function arg-count))
  (style-warn "Unimplemented."))
