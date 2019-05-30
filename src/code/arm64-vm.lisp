;;; This file contains the ARM specific runtime stuff.
;;;
(in-package "SB-VM")

#-sb-xc-host
(defun machine-type ()
  "Return a string describing the type of the local machine."
  "ARM64")

;;;; FIXUP-CODE-OBJECT

(defconstant-eqx +fixup-kinds+ #(:absolute :cond-branch :uncond-branch)
  #'equalp)
(!with-bigvec-or-sap
(defun fixup-code-object (code offset fixup kind flavor)
  (declare (type index offset))
  (declare (ignore flavor))
  (unless (zerop (rem offset sb-assem:+inst-alignment-bytes+))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (let ((sap (code-instructions code)))
    (ecase kind
        (:absolute
         (setf (sap-ref-word sap offset) fixup))
        (:cond-branch
         (setf (ldb (byte 19 5) (sap-ref-32 sap offset))
               (ash (- fixup (+ (sap-int sap) offset)) -2)))
        (:uncond-branch
         (setf (ldb (byte 26 0) (sap-ref-32 sap offset))
               (ash (- fixup (+ (sap-int sap) offset)) -2)))))
  nil))

;;;; "Sigcontext" access functions, cut & pasted from sparc-vm.lisp,
;;;; then modified for ARM.
;;;;
;;;; See also x86-vm for commentary on signed vs unsigned.

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
               (sap-ref-double sap 8) (imagpart value)))))))

;;;; INTERNAL-ERROR-ARGS.

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (instruction (sap-ref-32 pc 0))
         (error-number (ldb (byte 8 13) instruction))
         (trap-number (ldb (byte 8 5) instruction)))
    (declare (type system-area-pointer pc))
    (if (= trap-number invalid-arg-count-trap)
        (values error-number '(#.arg-count-sc) trap-number)
        (sb-kernel::decode-internal-error-args (sap+ pc 4) trap-number error-number))))
) ; end PROGN

;;; Undo the effects of XEP-ALLOCATE-FRAME
;;; and point PC to FUNCTION
#-sb-xc-host
(defun context-call-function (context function &optional arg-count)
  (with-pinned-objects (function)
    (with-pinned-context-code-object (context)
      (let* ((fun-addr (get-lisp-obj-address function))
             (entry (+ (sap-ref-word (int-sap fun-addr)
                                     (- (ash simple-fun-self-slot word-shift)
                                        fun-pointer-lowtag))
                       (- (ash simple-fun-insts-offset word-shift)
                          fun-pointer-lowtag))))
        (when arg-count
          (setf (context-register context nargs-offset)
                (get-lisp-obj-address arg-count)))
        (setf (context-register context lexenv-offset) fun-addr
              (context-register context lr-offset) entry)
        (set-context-pc context entry)))))
