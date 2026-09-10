;;;; target-only stuff for avx2-insts.lisp
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-X86-64-ASM")

;;; Printer for EVEX/VEX ModRM.r/m register operands.
;;; Does NOT use EVEX R' - R' belongs only to the ModRM.reg field.
(defun print-ymmreg-rm (value stream dstate)
  (let* ((offset (etypecase value
                   ((mod 32) value)
                   (reg (reg-num value))))
         (reg (get-fpr (cond ((dstate-getprop dstate +evex-l1+) :zmm)
                             ((dstate-getprop dstate +vex-l+) :ymm)
                             (t :xmm))
                       offset))
         (name (reg-name reg)))
    (if stream
        (write-string name stream)
        (operand name dstate))))

;;; Uses EVEX V' to form a 5-bit register number.
(defun print-ymmreg-vvvv (value stream dstate)
  (let* ((offset (etypecase value
                   ((mod 32) value)
                   (reg (reg-num value))))
         (offset (if (dstate-getprop dstate +evex-v-prime+)
                     (+ offset 16)
                     offset))
         (reg (get-fpr (cond ((dstate-getprop dstate +evex-l1+) :zmm)
                             ((dstate-getprop dstate +vex-l+) :ymm)
                             (t :xmm))
                       offset))
         (name (reg-name reg)))
    (if stream
        (write-string name stream)
        (operand name dstate))))

(defun print-ymmreg (value stream dstate)
  (let* ((offset (etypecase value
                   ((mod 32) value)
                   (reg (reg-num value))))
         ;; For EVEX, R' provides bit 4 of the reg field (registers 16-31).
         ;; This flag is set by the evex-r-prime prefilter.
         (offset (if (dstate-getprop dstate +evex-r-prime+)
                     (+ offset 16)
                     offset))
         (reg (get-fpr (cond ((dstate-getprop dstate +evex-l1+) :zmm)
                             ((dstate-getprop dstate +vex-l+) :ymm)
                             (t :xmm))
                       offset))
         (name (reg-name reg)))
    (if stream
        (write-string name stream)
        (operand name dstate))))

(defun print-kreg (value stream dstate)
  (let* ((offset (etypecase value
                   ((mod 32) value)
                   (reg (reg-num value))))
         (reg (get-fpr :kreg offset))
         (name (reg-name reg)))
    (if stream
        (write-string name stream)
        (operand name dstate))))

(defun print-kreg/mem (value stream dstate)
  (if (machine-ea-p value)
      (print-mem-ref :ref value :qword stream dstate)
      (print-kreg value stream dstate)))

(defun print-ymmreg/mem (value stream dstate)
  (if (machine-ea-p value)
      (print-mem-ref :ref value nil stream dstate)
      (print-ymmreg-rm value stream dstate)))

;;; Printer for half-width vector operands (e.g. 2x widening conversions).
;;; In 512-bit EVEX mode (+evex-l1+), half width is YMM.
;;; In 256-bit or 128-bit mode, half width is XMM.
(defun print-half-ymmreg-rm (value stream dstate)
  (let* ((offset (etypecase value
                   ((mod 32) value)
                   (reg (reg-num value))))
         (reg (get-fpr (cond ((dstate-getprop dstate +evex-l1+) :ymm)
                             (t :xmm))
                       offset))
         (name (reg-name reg)))
    (if stream
        (write-string name stream)
        (operand name dstate))))

(defun print-half-ymmreg/mem (value stream dstate)
  (if (machine-ea-p value)
      (print-mem-ref :ref value nil stream dstate)
      (print-half-ymmreg-rm value stream dstate)))

;;; Printer for the register-direct source operand of a 2x-widening move
;;; (VPMOV[SZ]X{BW,WD,DQ}): its width is always one step below the
;;; destination's vector length (xmm source for an xmm or ymm dest,
;;; ymm source for a zmm dest).
(defun print-ymmreg-rm-one-size-down (value stream dstate)
  (let* ((offset (etypecase value
                   ((mod 32) value)
                   (reg (reg-num value))))
         (reg (get-fpr (if (dstate-getprop dstate +evex-l1+) :ymm :xmm)
                       offset))
         (name (reg-name reg)))
    (if stream
        (write-string name stream)
        (operand name dstate))))

(defun print-ymmreg/mem-one-size-down (value stream dstate)
  (if (machine-ea-p value)
      (print-mem-ref :ref value nil stream dstate)
      (print-ymmreg-rm-one-size-down value stream dstate)))

(defun invert-4 (dstate value)
  (declare (ignore dstate))
  (logxor value #b1111))

(defun print-vmx/y (value stream dstate)
  (print-mem-ref :ref value :qword stream dstate
                 :index-reg-printer #'print-ymmreg))

(defun print-vmx (value stream dstate)
  (print-mem-ref :ref value :qword stream dstate
                 :index-reg-printer #'print-xmmreg))

(defun print-xmmreg/mem-with-width (value width sized-p stream dstate)
  (declare (type (member :byte :word :dword :qword) width)
           (type boolean sized-p))
  (if (machine-ea-p value)
      (print-mem-ref (if sized-p :sized-ref :ref) value width stream dstate)
      (print-xmmreg value stream dstate)))

(defun print-sized-xmmreg/mem (value stream dstate)
  (print-xmmreg/mem-with-width
   value (inst-operand-size dstate) t stream dstate))

(defun print-sized-byte-xmmreg/mem (value stream dstate)
  (print-xmmreg/mem-with-width value :byte t stream dstate))

(defun print-sized-word-xmmreg/mem (value stream dstate)
  (print-xmmreg/mem-with-width value :word t stream dstate))

(defun print-sized-dword-xmmreg/mem (value stream dstate)
  (print-xmmreg/mem-with-width value :dword t stream dstate))

(defun print-sized-xmmreg/mem-default-qword (value stream dstate)
  (print-xmmreg/mem-with-width
   value (inst-operand-size-default-qword dstate) t stream dstate))

(defun print-opmask-register (value stream dstate)
  (let ((name (format nil "K~d" (logand value 7))))
    (if stream
        (write-string name stream)
        (operand name dstate))))

(defun print-vsib/mem (value stream dstate)
  (if (not (machine-ea-p value))
      (print-ymmreg/mem value stream dstate)
      (let ((base (machine-ea-base value))
            (disp (machine-ea-disp value))
            (index (machine-ea-index value))
            (scale (machine-ea-scale value)))
        (flet ((write-vector-index (reg)
                 (let ((id (reg-id reg)))
                   (format stream "~a~d"
                           (cond ((dstate-getprop dstate +evex-l1+) "ZMM")
                                 ((dstate-getprop dstate +vex-l+) "YMM")
                                 (t "XMM"))
                           (reg-id-num id))))
               (write-disp (disp)
                 (cond
                   ((integerp disp)
                    (unless (zerop disp)
                      (format stream "~@d" disp)))
                   ((label-p disp)
                    (print-label disp stream dstate))
                   (t
                    (princ disp stream)))))
          (when stream
            (write-char #\[ stream)
            (when base
              (print-reg base stream dstate))
            (when index
              (when base (write-char #\+ stream))
              (write-vector-index index)
              (unless (= scale 1)
                (format stream "*~d" scale)))
            (when (or base index)
              (write-disp disp))
            (unless (or base index)
              (write-disp disp))
            (write-char #\] stream))))))

;;; APX EVEX printers
(defun print-apx-gpr-vvvv (value stream dstate)
  (let* ((v-low (logxor value #b1111))
         (reg-num (if (dstate-getprop dstate +evex-v-prime+)
                      (+ v-low 16)
                      v-low)))
    (print-reg-with-width reg-num (inst-operand-size dstate) stream dstate)))

(defun print-apx-gpr-vvvv-default-qword (value stream dstate)
  (let* ((v-low (logxor value #b1111))
         (reg-num (if (dstate-getprop dstate +evex-v-prime+)
                      (+ v-low 16)
                      v-low)))
    (print-reg-with-width reg-num (inst-operand-size-default-qword dstate) stream dstate)))

(defun print-apx-nf (value stream dstate)
  (when (eql value 1)
    (if stream
        (write-string "{nf} " stream)
        (operand "{nf} " dstate))))
