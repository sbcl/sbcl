;;;; SPARC-specific runtime stuff

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB!VM")

;;; See x86-vm.lisp for a description of this.
(define-alien-type os-context-t (struct os-context-t-struct))

;;;; MACHINE-TYPE

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "SPARC")

(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (unless (zerop (rem offset n-word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (without-gcing
   (let ((sap (%primitive code-instructions code)))
     (ecase kind
       (:call
        (error "Can't deal with CALL fixups, yet."))
       (:sethi
        (setf (ldb (byte 22 0) (sap-ref-32 sap offset))
              (ldb (byte 22 10) fixup)))
       (:add
        (setf (ldb (byte 10 0) (sap-ref-32 sap offset))
              (ldb (byte 10 0) fixup)))))))


;;;; "Sigcontext" access functions, cut & pasted from alpha-vm.lisp.
;;;;
;;;; See also x86-vm for commentary on signed vs unsigned.

(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned-int)
  (context (* os-context-t)))

(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (int-sap (deref (context-pc-addr context))))

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

;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

;;; FIXME: Whether COERCE actually knows how to make a float out of a
;;; long is another question. This stuff still needs testing.
#+nil
(define-alien-routine ("os_context_float_register_addr" context-float-register-addr)
  (* long)
  (context (* os-context-t))
  (index int))
#+nil
(defun context-float-register (context index format)
  (declare (type (alien (* os-context-t)) context))
  (coerce (deref (context-float-register-addr context index)) format))
#+nil
(defun %set-context-float-register (context index format new)
  (declare (type (alien (* os-context-t)) context))
  (setf (deref (context-float-register-addr context index))
        (coerce new format)))

;;; Given a signal context, return the floating point modes word in
;;; the same format as returned by FLOATING-POINT-MODES.

;;; Under SunOS, we have a straightforward implementation in C:
#!+sunos
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (unsigned 32)
  (context (* os-context-t)))

;;; Under Linux, we have to contend with utterly broken signal handling.
#!+linux
(defun context-floating-point-modes (context)
  (declare (ignore context))
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)

;;;; INTERNAL-ERROR-ARGS.

;;; Given a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.  This is e.g.
;;; 4       23      254     240     2       0       0       0
;;; |       ~~~~~~~~~~~~~~~~~~~~~~~~~
;;; length         data              (everything is an octet)
;;;  (pc)
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (sb!int::/show0 "entering INTERNAL-ERROR-ARGS")
  (let* ((pc (context-pc context))
         (bad-inst (sap-ref-32 pc 0))
         (op (ldb (byte 2 30) bad-inst))
         (op2 (ldb (byte 3 22) bad-inst))
         (op3 (ldb (byte 6 19) bad-inst)))
    (declare (type system-area-pointer pc))
    (cond ((and (= op #b00) (= op2 #b000))
           (args-for-unimp-inst context))
          ((and (= op #b10) (= (ldb (byte 4 2) op3) #b1000))
           (args-for-tagged-add-inst context bad-inst))
          ((and (= op #b10) (= op3 #b111010))
           (args-for-tcc-inst bad-inst))
          (t
           (values #.(error-number-or-lose 'unknown-error) nil)))))

(defun args-for-unimp-inst (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (length (sap-ref-8 pc 4))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system-area-pointer pc)
             (type (unsigned-byte 8) length)
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

(defun args-for-tagged-add-inst (context bad-inst)
  (declare (type (alien (* os-context-t)) context))
  (let* ((rs1 (ldb (byte 5 14) bad-inst))
         (op1 (make-lisp-obj (context-register context rs1))))
    (if (fixnump op1)
        (if (zerop (ldb (byte 1 13) bad-inst))
            (let* ((rs2 (ldb (byte 5 0) bad-inst))
                   (op2 (make-lisp-obj (context-register context rs2))))
              (if (fixnump op2)
                  (values #.(error-number-or-lose 'unknown-error) nil)
                  (values #.(error-number-or-lose 'object-not-fixnum-error)
                          (list (sb!c::make-sc-offset
                                 descriptor-reg-sc-number
                                 rs2)))))
            (values #.(error-number-or-lose 'unknown-error) nil))
        (values #.(error-number-or-lose 'object-not-fixnum-error)
                (list (sb!c::make-sc-offset descriptor-reg-sc-number
                                            rs1))))))

(defun args-for-tcc-inst (bad-inst)
  (let* ((trap-number (ldb (byte 8 0) bad-inst))
         (reg (ldb (byte 5 8) bad-inst)))
    (values (case trap-number
              (#.object-not-list-trap
               #.(error-number-or-lose 'object-not-list-error))
              (#.object-not-instance-trap
               #.(error-number-or-lose 'object-not-instance-error))
              (t
               #.(error-number-or-lose 'unknown-error)))
            (list (sb!c::make-sc-offset descriptor-reg-sc-number reg)))))

