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
                   ((unsigned-byte 4) value)
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
                   ((unsigned-byte 4) value)
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
                   ((unsigned-byte 4) value)
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
                   ((unsigned-byte 4) value)
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

