;;; This file contains the PPC specific runtime stuff.
;;;
(in-package "SB!VM")

(define-alien-type os-context-t (struct os-context-t-struct))


;;;; MACHINE-TYPE

(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "PowerPC")

;;;; FIXUP-CODE-OBJECT

(defun fixup-code-object (code offset fixup kind)
  (declare (type index offset))
  (unless (zerop (rem offset n-word-bytes))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (without-gcing
   (let ((sap (%primitive code-instructions code)))
     (ecase kind
       (:b
        (error "Can't deal with CALL fixups, yet."))
       (:ba
        (setf (ldb (byte 24 2) (sap-ref-32 sap offset))
              (ash fixup -2)))
       (:ha
        (let* ((h (ldb (byte 16 16) fixup))
               (l (ldb (byte 16 0) fixup)))
          ; Compensate for possible sign-extension when the low half
          ; is added to the high.  We could avoid this by ORI-ing
          ; the low half in 32-bit absolute loads, but it'd be
          ; nice to be able to do:
          ;  lis rX,foo@ha
          ;  lwz rY,foo@l(rX)
          ; and lwz/stw and friends all use a signed 16-bit offset.
          (setf (ldb (byte 16 0) (sap-ref-32 sap offset))
                 (if (logbitp 15 l) (ldb (byte 16 0) (1+ h)) h))))
       (:l
        (setf (ldb (byte 16 0) (sap-ref-32 sap offset))
              (ldb (byte 16 0) fixup)))))))


;;;; "Sigcontext" access functions, cut & pasted from x86-vm.lisp then
;;;; hacked for types.

(define-alien-routine ("os_context_pc_addr" context-pc-addr) (* unsigned-long)
  (context (* os-context-t)))

(defun context-pc (context)
  (declare (type (alien (* os-context-t)) context))
  (int-sap (deref (context-pc-addr context))))

(define-alien-routine ("os_context_register_addr" context-register-addr)
  (* unsigned-long)
  (context (* os-context-t))
  (index int))

(defun context-register (context index)
  (declare (type (alien (* os-context-t)) context))
  (deref (context-register-addr context index)))

(define-alien-routine ("os_context_lr_addr" context-lr-addr) (* unsigned-long)
  (context (* os-context-t)))

(defun context-lr (context)
  (declare (type (alien (* os-context-t)) context))
  (int-sap (deref (context-lr-addr context))))

(defun %set-context-register (context index new)
(declare (type (alien (* os-context-t)) context))
(setf (deref (context-register-addr context index))
      new))
;;; This is like CONTEXT-REGISTER, but returns the value of a float
;;; register. FORMAT is the type of float to return.

;;; FIXME: Whether COERCE actually knows how to make a float out of a
;;; long is another question. This stuff still needs testing.
#+nil
(define-alien-routine ("os_context_fpregister_addr" context-float-register-addr)
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
;;;
;;; FIXME: surely this must be accessible somewhere under Darwin?  Or
;;; under NetBSD?
#!+linux
(define-alien-routine ("os_context_fp_control" context-floating-point-modes)
    (unsigned 32)
  (context (* os-context-t)))


;;;; INTERNAL-ERROR-ARGS.

;;; GIVEN a (POSIX) signal context, extract the internal error
;;; arguments from the instruction stream.  This is e.g.

;;; INTERNAL-ERROR-ARGS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;;
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (bad-inst (sap-ref-32 pc 0))
         (op (ldb (byte 16 16) bad-inst))
         (regnum (ldb (byte 5 0) op)))
    (declare (type system-area-pointer pc))
    (cond ((= op (logior (ash 3 10) (ash 6 5)))
           (args-for-unimp-inst context))
          #!-precise-arg-count-error
          ((and (= (ldb (byte 6 10) op) 3)
                (= (ldb (byte 5 5) op) 24))
           (let ((prev (sap-ref-32 (int-sap (- (sap-int pc) 4)) 0)))
             (if (and (= (ldb (byte 6 26) prev) 3)
                      (= (ldb (byte 5 21) prev) 0))
                 (values (ldb (byte 16 0) prev)
                         (list (make-sc-offset any-reg-sc-number
                                               (ldb (byte 5 16) prev))))
                 (values #.(error-number-or-lose
                            'invalid-arg-count-error)
                         (list (make-sc-offset any-reg-sc-number regnum))))))
          #!+precise-arg-count-error
          ((and (= (ldb (byte 6 10) op) 3) ;; twi
                (or (= regnum #.(sc-offset-offset arg-count-sc))
                    (= (ldb (byte 5 5) op) 24))) ;; :ne
           ;; Type errors are encoded as
           ;; twi 0 value-register error-code
           ;; twi :ne temp-register x
           (let ((prev (sap-ref-32 (int-sap (- (sap-int pc) 4)) 0)))
             (if (and (= (ldb (byte 5 5) op) 24) ;; is the condition :ne?
                      (= (ldb (byte 6 26) prev) 3) ;; is it twi?
                      (= (ldb (byte 5 21) prev) 0)) ;; is it non-trapping?
                 (values (ldb (byte 16 0) prev)
                         (list (make-sc-offset any-reg-sc-number
                                               (ldb (byte 5 16) prev))))
                 ;; arg-count errors are encoded as
                 ;; twi {:ne :llt :lgt} nargs arg-count
                 (values #.(error-number-or-lose 'invalid-arg-count-error)
                         '(#.arg-count-sc)))))
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
           (error-number (read-var-integer vector index)))
      (collect ((sc-offsets))
               (loop
                (when (>= index length)
                  (return))
                (sc-offsets (read-var-integer vector index)))
               (values error-number (sc-offsets))))))

