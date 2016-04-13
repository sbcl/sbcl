;;; This file contains the MIPS specific runtime stuff.
;;;
(in-package "SB!VM")


(define-alien-type os-context-t (struct os-context-t-struct))
(define-alien-type os-context-register-t unsigned-long-long)


;;;; MACHINE-TYPE

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "MIPS")

;;;; FIXUP-CODE-OBJECT

(defun fixup-code-object (code offset value kind)
  (declare (type index offset))
  (unless (zerop (rem offset n-word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (without-gcing
   (let ((sap (%primitive sb!c::code-instructions code)))
     (ecase kind
       (:jump
        (aver (zerop (ash value -28)))
        (setf (ldb (byte 26 0) (sap-ref-32 sap offset))
              (ash value -2)))
       (:lui
        (setf (ldb (byte 16 0) (sap-ref-32 sap offset))
              (ash (1+ (ash value -15)) -1)))
       (:addi
        (setf (ldb (byte 16 0) (sap-ref-32 sap offset))
              (ldb (byte 16 0) value)))))))


(define-alien-routine ("os_context_pc_addr" context-pc-addr)
    (* os-context-register-t)
  (context (* os-context-t) :in))

(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (int-sap (deref (context-pc-addr context))))

(define-alien-routine ("os_context_register_addr" context-register-addr)
    (* os-context-register-t)
  (context (* os-context-t) :in)
  (index int :in))

(define-alien-routine ("os_context_bd_cause" context-bd-cause-int)
    unsigned-int
  (context (* os-context-t) :in))

;;; FIXME: Should this and CONTEXT-PC be INLINE to reduce consing?
;;; (Are they used in anything time-critical, or just the debugger?)
(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* os-context-register-t)) addr))
    ;; The 32-bit Linux ABI uses 64-bit slots for register storage in
    ;; the context structure.  At least some hardware sign extends
    ;; 32-bit values in these registers, leading to badness in the
    ;; debugger.  Mask it down here.
    (mask-field (byte 32 0) (deref addr))))

(defun %set-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* os-context-register-t)) addr))
    (setf (deref addr) new)))

;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

;;; FIXME: Whether COERCE actually knows how to make a float out of a
;;; long is another question. This stuff still needs testing.
(define-alien-routine ("os_context_fpregister_addr" context-float-register-addr)
    (* os-context-register-t)
  (context (* os-context-t) :in)
  (index int :in))

(defun context-float-register (context index format)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-float-register-addr context index)))
    (declare (type (alien (* os-context-register-t)) addr))
    (coerce (deref addr) format)))

(defun %set-context-float-register (context index format new)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-float-register-addr context index)))
    (declare (type (alien (* os-context-register-t)) addr))
    (setf (deref addr) (coerce new format))))

(define-alien-routine
    ("arch_get_fp_control" floating-point-modes) unsigned-int)

(define-alien-routine
    ("arch_set_fp_control" %floating-point-modes-setter) void (fp unsigned-int :in))

(defun (setf floating-point-modes) (val) (%floating-point-modes-setter val))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    unsigned-int
  (context (* os-context-t) :in))

;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.  This is e.g.
;;; 4       23      254     206     1       0       0       0
;;; |       ~~~~~~~~~~~~~~~~~~~~~~~~~
;;; length         data              (everything is an octet)
;;; (pc + 4)
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (/show0 "entering INTERNAL-ERROR-ARGS, CONTEXT=..")
  (/hexstr context)
  (let ((pc (context-pc context))
        (cause (context-bd-cause-int context)))
    (declare (type system-area-pointer pc))
    (multiple-value-bind (error-number length sc-offsets)
        ;; KLUDGE: This exposure of the branch delay mechanism hurts.
        (sb!disassem:snarf-error-junk pc (if (logbitp 31 cause) 8 4))
      (declare (ignore length))
      (values error-number sc-offsets))))
