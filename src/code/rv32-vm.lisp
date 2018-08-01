;;; This file contains the RV32-specific runtime stuff.

(in-package "SB-VM")

#-sb-xc-host
(defun machine-type ()
  "Return a string describing the type of the local machine."
  "RV32")

;;; FIXUP-CODE-OBJECT

(defconstant-eqx +fixup-kinds+ #(:dummy) #'equalp)

(!with-bigvec-or-sap
 (defun fixup-code-object (code offset fixup kind flavor)
   (declare (ignore code offset fixup kind flavor))
   nil))

;;; CONTEXT-FLOAT-REGISTER
#-sb-xc-host (progn
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
               (sap-ref-double sap 8) (imagpart value))))))))

;;; INTERNAL-ERROR-ARGS
#-sb-xc-host
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (declare (ignore context))
  nil)

;;; CONTEXT-CALL-FUNCTION
#-sb-xc-host
(defun context-call-function (context function &optional arg-count)
  (declare (ignore context function arg-count)))
