(in-package "SB!VM")

#-sb-xc-host
(defun machine-type ()
  "Returns a string describing the type of the local machine."
  "HPPA")

;;;; FIXUP-CODE-OBJECT
(defconstant-eqx +fixup-kinds+
    #(:absolute :load :load11u :load-short :hi :branch)
  #'equalp)
(!with-bigvec-or-sap
(defun fixup-code-object (code offset value kind flavor)
  (declare (ignore flavor))
  (unless (zerop (rem offset sb!assem:+inst-alignment-bytes+))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (let* ((sap (code-instructions code))
         (inst (sap-ref-32 sap offset)))
    (setf (sap-ref-32 sap offset)
          (ecase kind
             (:absolute
              value)
             (:load
              (logior (mask-field (byte 18 14) value)
                      (if (< value 0)
                        (1+ (ash (ldb (byte 13 0) value) 1))
                        (ash (ldb (byte 13 0) value) 1))))
             (:load11u
              (logior (if (< value 0)
                          (1+ (ash (ldb (byte 10 0) value) 1))
                          (ash (ldb (byte 11 0) value) 1))
                      (mask-field (byte 18 14) inst)))
             (:load-short
              (let ((low-bits (ldb (byte 11 0) value)))
                (aver (<= 0 low-bits (1- (ash 1 4))))
                (logior (ash (dpb (ldb (byte 4 0) value)
                                  (byte 4 1)
                                  (ldb (byte 1 4) value)) 17)
                        (logand inst #xffe0ffff))))
             (:hi
              (logior (ash (ldb (byte 5 13) value) 16)
                      (ash (ldb (byte 2 18) value) 14)
                      (ash (ldb (byte 2 11) value) 12)
                      (ash (ldb (byte 11 20) value) 1)
                      (ldb (byte 1 31) value)
                      (logand inst #xffe00000)))
             (:branch
              (let ((bits (ldb (byte 9 2) value)))
                (aver (zerop (ldb (byte 2 0) value)))
                (logior (ash bits 3)
                        (mask-field (byte 1 1) inst)
                        (mask-field (byte 3 13) inst)
                        (mask-field (byte 11 21) inst)))))))
  nil))

#-sb-xc-host (progn

;;; For now.
(defun context-floating-point-modes (context)
  (declare (ignore context))
  (warn "stub CONTEXT-FLOATING-POINT-MODES")
  0)

;;;; Internal-error-arguments.

;;; INTERNAL-ERROR-ARGUMENTS -- interface.
;;;
;;; Given the sigcontext, extract the internal error arguments from the
;;; instruction stream.
;;;
(defun internal-error-args (context)
  (declare (type (alien (* os-context-t)) context))
  (let* ((pc (context-pc context))
         (trap-number (logand (sap-ref-8 pc 3) #x1f)))
    (declare (type system-area-pointer pc))
    (sb!kernel::decode-internal-error-args (sap+ pc 5) trap-number
                                           (sap-ref-8 pc 4))))
) ; end PROGN
