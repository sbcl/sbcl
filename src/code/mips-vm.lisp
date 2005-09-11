;;; This file contains the MIPS specific runtime stuff.
;;;
(in-package "SB!VM")


(define-alien-type os-context-t (struct os-context-t-struct))


;;;; MACHINE-TYPE and MACHINE-VERSION

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "MIPS")

;;; support for CL:MACHINE-VERSION defined OAOO elsewhere
(defun get-machine-version ()
  #!+little-endian "little-endian"
  #!-little-endian "big-endian")


;;;; FIXUP-CODE-OBJECT

(defun fixup-code-object (code offset value kind)
  (declare (type index offset))
  (unless (zerop (rem offset n-word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (sb!sys:without-gcing
   (let ((sap (truly-the system-area-pointer
                         (%primitive sb!c::code-instructions code))))
     (ecase kind
       (:jump
        (aver (zerop (ash value -28)))
        (setf (ldb (byte 26 0) (sap-ref-32 sap offset))
              (ash value -2)))
       (:lui
        (setf (sap-ref-16 sap
                          #!+little-endian offset
                          #!-little-endian (+ offset 2))
              (+ (ash value -16)
                 (if (logbitp 15 value) 1 0))))
       (:addi
        (setf (sap-ref-16 sap
                          #!+little-endian offset
                          #!-little-endian (+ offset 2))
              (ldb (byte 16 0) value)))))))


(define-alien-routine ("os_context_pc_addr" context-pc-addr)
    (* unsigned-long-long)
  (context (* os-context-t)))

(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (int-sap (deref (context-pc-addr context))))

(define-alien-routine ("os_context_register_addr" context-register-addr)
    (* unsigned-long-long)
  (context (* os-context-t))
  (index int))

(define-alien-routine ("os_context_bd_cause" context-bd-cause-int)
    unsigned-int
  (context (* os-context-t)))

;;; FIXME: Should this and CONTEXT-PC be INLINE to reduce consing?
;;; (Are they used in anything time-critical, or just the debugger?)
(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* unsigned-long-long)) addr))
    (deref addr)))

(defun %set-context-register (context index new)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-register-addr context index)))
    (declare (type (alien (* unsigned-long-long)) addr))
    (setf (deref addr) new)))

;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

;;; FIXME: Whether COERCE actually knows how to make a float out of a
;;; long is another question. This stuff still needs testing.
(define-alien-routine ("os_context_fpregister_addr" context-float-register-addr)
    (* unsigned-long-long)
  (context (* os-context-t))
  (index int))

(defun context-float-register (context index format)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-float-register-addr context index)))
    (declare (type (alien (* unsigned-long-long)) addr))
    (coerce (deref addr) format)))

(defun %set-context-float-register (context index format new)
  (declare (type (alien (* os-context-t)) context))
  (let ((addr (context-float-register-addr context index)))
    (declare (type (alien (* unsigned-long-long)) addr))
    (setf (deref addr) (coerce new format))))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    unsigned-int
  (context (* os-context-t)))

;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;;
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (/show0 "entering INTERNAL-ERROR-ARGS, CONTEXT=..")
  (/hexstr context)
  (let ((pc (context-pc context))
        (cause (context-bd-cause-int context)))
    (declare (type system-area-pointer pc))
    ;; KLUDGE: This exposure of the branch delay mechanism hurts.
    (when (logbitp 31 cause)
      (setf pc (sap+ pc 4)))
    (args-for-unimp-inst pc)))

(defun args-for-unimp-inst (pc)
  (declare (type system-area-pointer pc))
  (let* ((length (sap-ref-8 pc 4))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (copy-ub8-from-system-area pc 5 vector 0 length)
    (let* ((index 0)
           (error-number (sb!c:read-var-integer vector index)))
      (collect ((sc-offsets))
               (loop
                (when (>= index length)
                  (return))
                (sc-offsets (sb!c:read-var-integer vector index)))
               (values error-number (sc-offsets))))))
