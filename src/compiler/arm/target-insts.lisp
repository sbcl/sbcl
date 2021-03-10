;;;; that part of the description of the ARM instruction set (for
;;;; ARMv5) which is not needed on the cross-compilation host.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-ARM-ASM")

(defun maybe-add-notes (dstate)
  (let* ((inst (sap-ref-int (dstate-segment-sap dstate)
                            (dstate-cur-offs dstate) n-word-bytes
                            (dstate-byte-order dstate)))
         (op (ldb (byte 8 20) inst))
         (offset (ldb (byte 12 0) inst))
         (rn (ldb (byte 4 16) inst)))
    (cond ((and (= rn null-offset))
           (let ((offset (+ nil-value offset)))
             (case op
               ((88 89) ;; LDR/STR
                (maybe-note-assembler-routine offset nil dstate)
                (maybe-note-static-symbol
                 (logior offset other-pointer-lowtag) dstate))
               (40 ;; ADD
                (maybe-note-static-symbol offset dstate)))))
          (t
           (case op
             (89 ;; LDR
              (case rn
                (#.code-offset
                 (note-code-constant offset dstate))
                (#.pc-offset
                 (let ((value (sap-ref-int (dstate-segment-sap dstate)
                                           (+ (dstate-cur-offs dstate) offset 8)
                                           n-word-bytes
                                           (dstate-byte-order dstate))))
                   (maybe-note-assembler-routine value nil dstate))))))))))

(defun print-condition (value stream dstate)
  (declare (type stream stream)
           (fixnum value)
           (ignore dstate))
  (unless (= value 14) ;; Don't print :al
    (princ (aref +condition-name-vec+ value) stream)))

(defun print-reg (value stream dstate)
  (declare (type stream stream)
           (fixnum value)
           (ignore dstate))
  (princ (aref sb-vm::*register-names* value) stream))

(defun print-float-reg (value stream dstate)
  (declare (type stream stream)
           (list value)
           (ignore dstate))
  (destructuring-bind (double high low) value
    (format stream "~[S~;D~]~a"
            double
            (if (= double 1)
                high
                (logior (ash high 1) low)))))

(defun print-float-sys-reg (value stream dstate)
  (declare (type stream stream)
           (fixnum value)
           (ignore dstate))
  (princ (ecase value
           (#b0000 "FPSID")
           (#b0001 "FPSCR")
           (#b1000 "FPEXC")) stream))

(defun print-shift-type (value stream dstate)
  (declare (type stream stream)
           (fixnum value)
           (ignore dstate))
  (princ (aref #(lsl lsr asr ror) value) stream))

(defun print-immediate-shift (value stream dstate)
  (declare (type stream stream) (ignore dstate))
  (destructuring-bind (amount shift) value
    (declare (fixnum amount shift))
    (cond
      ((and (zerop amount)
            (zerop shift))) ;; No shift
      ((and (zerop amount)
            (= shift 3))
       (princ ", RRX" stream))
      (t
       (princ ", " stream)
       (princ (aref #(lsl lsr asr ror) shift) stream)
       (princ " #" stream)
       (princ amount stream)))))

(defun print-shifter-immediate (value stream dstate)
  (declare (type stream stream)
           (fixnum value))
  (maybe-add-notes dstate)
  (let* ((rotate (ldb (byte 4 8) value))
         (immediate (mask-field (byte 8 0) value))
         (left (mask-field (byte 32 0)
                           (ash immediate (- 32 rotate rotate))))
         (right (ash immediate (- 0 rotate rotate))))
    (princ (logior left right) stream)))

(defun use-label-relative-label (value dstate)
  (declare (type (signed-byte 24) value)
           (type disassem-state dstate))
  (+ 8 (ash value 2) (dstate-cur-addr dstate)))

(defun print-load/store-immediate (value stream dstate)
  (declare (type stream stream))
  (maybe-add-notes dstate)
  (destructuring-bind (p u w offset) value
    (declare (bit p u w) (fixnum offset))
    (if (zerop offset)
        (princ "]" stream)
        (progn
          (princ (if (zerop p) "], #" ", #") stream)
          (when (zerop u)
            (princ "-" stream))
          (princ offset stream)
          (unless (zerop p)
            (princ (if (zerop w) "]" "]!") stream))))))

(defun print-load/store-register (value stream dstate)
  (destructuring-bind (p u w shift-imm shift rm) value
    (when (zerop p)
      (princ "]" stream))
    (princ (if (zerop u) ", -" ", ") stream)
    (print-reg rm stream dstate)
    (print-immediate-shift (list shift-imm shift) stream dstate)
    (unless (zerop p)
      (princ (if (zerop w) "]" "]!") stream))))

(defun print-msr-field-mask (value stream dstate)
  (declare (type stream stream) (ignore dstate))
  (destructuring-bind (spsr-p field-mask) value
    (declare (bit spsr-p) ((unsigned-byte 4) field-mask))
    (if (zerop spsr-p)
        (princ "CPSR_" stream)
        (princ "SPSR_" stream))
    (when (logbitp 0 field-mask) (princ "c" stream))
    (when (logbitp 1 field-mask) (princ "x" stream))
    (when (logbitp 2 field-mask) (princ "s" stream))
    (when (logbitp 3 field-mask) (princ "f" stream))))

;;;; special magic to support decoding internal-error and related traps

(defun debug-trap-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap (debug-trap-code chunk dstate)))
      (case trap
        (#.halt-trap
         (nt "Halt trap"))
        (#.pending-interrupt-trap
         (nt "Pending interrupt trap"))
        (#.breakpoint-trap
         (nt "Breakpoint trap"))
        (#.fun-end-breakpoint-trap
         (nt "Function end breakpoint trap"))
        (#.single-step-around-trap
         (nt "Single step around trap"))
        (#.single-step-before-trap
         (nt "Single step before trap"))
        (t
         (when (or (and (= trap cerror-trap) (progn (nt "cerror trap") t))
                   (>= trap error-trap))
           (handle-break-args #'snarf-error-junk trap stream dstate)))))))
