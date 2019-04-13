;;; This file contains the RISC-V-specific runtime stuff.

(in-package "SB-VM")

#-sb-xc-host
(defun machine-type ()
  "Return a string describing the type of the local machine."
  #-64-bit "RV32G" #+64-bit "RV64G")

;;; FIXUP-CODE-OBJECT

(defconstant-eqx +fixup-kinds+ #(:absolute :i-type :s-type :u-type) #'equalp)

(defun u-and-i-inst-immediate (value)
  (let ((hi (ash (+ value (expt 2 11)) -12)))
    (values hi (- value (ash hi 12)))))

(def!type short-immediate () `(signed-byte 12))
(def!type short-immediate-fixnum () `(signed-byte ,(- 12 n-fixnum-tag-bits)))

(def!type u+i-immediate ()
  #-64-bit `(and (signed-byte 32) (unsigned-byte 32))
  #+64-bit `(or (integer #x-80000800 #x7ffff7ff)
                (integer ,(+ (ash 1 64) #x-80000800)
                         ,(1- (ash 1 64)))))

(!with-bigvec-or-sap
  (defun fixup-code-object (code offset fixup kind flavor)
    (declare (type index offset))
    (declare (ignore flavor))
    (unless (zerop (rem offset sb-assem:+inst-alignment-bytes+))
      (error "Unaligned instruction?  offset=#x~X." offset))
    #+64-bit
    (unless (typep fixup 'u+i-immediate)
      (error "Tried to fixup with ~a." fixup))
    (let ((sap (code-instructions code)))
      (multiple-value-bind (u i) (u-and-i-inst-immediate fixup)
        (ecase kind
          (:absolute
           (setf (sap-ref-32 sap offset) fixup))
          (:i-type
           (setf (ldb (byte 12 20) (sap-ref-32 sap offset)) i))
          (:s-type
           (setf (ldb (byte 5 7) (sap-ref-32 sap offset))
                 (ldb (byte 5 0) i))
           (setf (ldb (byte 7 25) (sap-ref-32 sap offset))
                 (ldb (byte 7 5) i)))
          (:u-type
           (setf (ldb (byte 20 12) (sap-ref-32 sap offset)) u)))))
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

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
#-sb-xc-host
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
#-sb-xc-host
(defun context-call-function (context function &optional arg-count)
  (declare (ignore context function arg-count))
  (style-warn "Unimplemented."))
