;;;; This file is for stuff which was in CMU CL's insts.lisp
;;;; file, but which in the SBCL build process can't be compiled
;;;; into code for the cross-compilation host.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-RISCV-ASM")

(defun current-instruction (dstate &optional (offset 0))
  (sap-ref-int (dstate-segment-sap dstate)
               (+ (dstate-cur-offs dstate) offset)
               n-word-bytes
               (dstate-byte-order dstate)))

(defun instruction-opcode (instruction)
  (ldb (byte 7 0) instruction))

(defvar *disassem-use-lisp-reg-names* t
  "If non-NIL, print registers using the Lisp register names.
Otherwise, use the RISC-V register names")

;; FIXME: Can this be a property of DSTATE instead?
(defvar *note-u-inst* (make-array 32 :initial-element nil)
  "A map for the disassembler indicating the target register and value
used in a u-type instruction.  This is used to make annotations about
function addresses and register values.")

(defconstant-eqx lisp-reg-symbols
  #.(map 'vector
         (lambda (name)
           (and name (make-symbol (concatenate 'string "$" name))))
         sb-vm::*register-names*)
  #'equalp)

(defconstant-eqx riscv-reg-symbols
  #.(coerce (loop for n from 0 to 31 collect (make-symbol (format nil "x~d" n)))
            'vector)
  #'equalp)

(defun print-reg (value stream dstate)
  (declare (stream stream) (fixnum value))
  (let ((regname (aref (if *disassem-use-lisp-reg-names*
                           lisp-reg-symbols
                           riscv-reg-symbols)
                       value))
        (instruction (current-instruction dstate)))
    (case (instruction-opcode instruction)
      ;; LUI
      ((#b0110111)
       (note-absolute-u-inst value (ldb (byte 20 12) instruction)))
      ;; AUIPC
      ((#b0010111)
       (note-pc-relative-u-inst value (ldb (byte 20 12) instruction) dstate))
      (t
       ;; Delete any u-inst entry with the same target register.
       #+ (or)
       (setf (aref *note-u-inst* value) nil)))
    (princ regname stream)
    (maybe-note-associated-storage-ref
     value 'registers regname dstate)))

(defun note-absolute-u-inst (rd u-imm)
  (setf (aref *note-u-inst* rd) (coerce-signed u-imm 12)))

(defun note-pc-relative-u-inst (rd u-imm dstate)
  (setf (aref *note-u-inst* rd) (+ (dstate-cur-addr dstate)
                                   (coerce-signed u-imm 12))))

(defconstant-eqx float-reg-symbols
  #.(coerce (loop for n from 0 to 31 collect (make-symbol (format nil "ft~d" n)))
            'vector)
  #'equalp)

(defun print-fp-reg (value stream dstate)
  (declare (stream stream) (fixnum value))
  (let ((regname (aref float-reg-symbols value)))
    (princ regname stream)
    (maybe-note-associated-storage-ref
     value 'float-registers regname dstate)))

(defun reconstruct-s-immediate (value)
  (coerce-signed (dpb (first value) (byte 7 5) (second value)) 12))

(defun print-s-imm (value stream dstate)
  (declare (stream stream) (ignore dstate))
  (princ (reconstruct-s-immediate value) stream))

(defun use-b-label (value dstate)
  (declare (type disassem-state dstate))
  (let* ((value (coerce-signed
                 (logior (ash (first value) 12)
                         (ash (second value) 11)
                         (ash (third value) 5)
                         (ash (fourth value) 1))
                 12))
         (address (+ value (dstate-cur-addr dstate))))
    ;; LRA pointer
    (if (= (logand address lowtag-mask) other-pointer-lowtag)
        (- address (- other-pointer-lowtag n-word-bytes))
        address)))

(defun use-j-label (value dstate)
  (declare (type disassem-state dstate))
  (let* ((value (coerce-signed
                 (logior (ash (first value) 20)
                         (ash (second value) 12)
                         (ash (third value) 11)
                         (ash (fourth value) 1))
                 20))
         (address (+ value (dstate-cur-addr dstate))))
    ;; LRA pointer
    (if (= (logand address lowtag-mask) other-pointer-lowtag)
        (- address (- other-pointer-lowtag n-word-bytes))
        address)))

(defun print-float-fmt (value stream dstate)
  (declare (ignore dstate)
           (stream stream)
           (type (unsigned-byte 3) value))
  (princ (case value
           (#b00 's)
           (#b01 'd)
           (#b10 'q)
           (t '?))
         stream))

(defun print-float-rm (value stream dstate)
  (declare (ignore dstate)
           (stream stream)
           (type (unsigned-byte 3) value))
  (princ (aref #(rne rtz rdn rup rmm unused1 unused2 dynamic)
               value)
         stream))

(defun print-a-ordering (value stream dstate)
  (declare (ignore dstate)
           (stream stream)
           (type (unsigned-byte 2) value))
  (when (logbitp 0 value)
    (princ 'aq stream)
    (princ #\Space stream))
  (when (logbitp 1 value)
    (princ 'rl stream)))

(defun print-fence-ordering (value stream dstate)
  (declare (ignore dstate)
           (stream stream)
           (type (unsigned-byte 4) value))
  (dotimes (index 4)
    (when (logbitp (- 3 index) value)
      (princ (aref #(i o r w) index) stream))))

(defun maybe-augment (rd i-imm)
  (+ (ash (or (aref *note-u-inst* rd) 0) 12)
     (coerce-signed i-imm 12)))

(defun annotate-load-store (register offset dstate)
  (case register
    (#.sb-vm::code-offset
     (note-code-constant offset dstate))
    (#.sb-vm::null-offset
     (let ((offset (+ sb-vm:nil-value offset)))
       (maybe-note-assembler-routine offset nil dstate)
       (maybe-note-static-symbol (logior offset other-pointer-lowtag)
                                 dstate)))
    #+sb-thread
    (#.sb-vm::thread-offset
     (let* ((thread-slots
              (load-time-value
               (primitive-object-slots (sb-vm::primitive-object 'sb-vm::thread))
               t))
            (slot (find (ash offset (- word-shift)) thread-slots
                        :key #'slot-offset)))
       (when slot
         (note (lambda (stream)
                 (format stream "thread.~(~A~)" (slot-name slot)))
               dstate))))
    (t
     (let ((offset (maybe-augment register offset)))
       (maybe-note-assembler-routine offset nil dstate)
       (maybe-note-static-symbol (logior offset other-pointer-lowtag)
                                 dstate)))))

(defun print-load-annotation (value stream dstate)
  (declare (ignore stream))
  (annotate-load-store (first value)
                       (coerce-signed (second value) 12)
                       dstate))

(defun print-store-annotation (value stream dstate)
  (declare (ignore stream))
  (annotate-load-store (first value)
                       (reconstruct-s-immediate (rest value))
                       dstate))

(defun print-jalr-annotation (value stream dstate)
  (declare (ignore stream))
  (destructuring-bind (rs i-imm) value
    (maybe-note-assembler-routine (maybe-augment rs i-imm) t dstate)))

;;;; interrupt instructions

(defun break-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap (trap-code chunk dstate)))
     (case trap
       (#.breakpoint-trap
        (nt "breakpoint trap"))
       (#.pending-interrupt-trap
        (nt "pending interrupt trap"))
       (#.halt-trap
        (nt "halt trap"))
       (#.fun-end-breakpoint-trap
        (nt "function end breakpoint trap"))
       (#.single-step-around-trap
        (nt "single-step trap (around)"))
       (#.single-step-before-trap
        (nt "single-step trap (before)"))
       (#.invalid-arg-count-trap
        (nt "Invalid argument count trap"))
       (t
        (when (or (and (= trap error-trap) (progn (nt "cerror trap") t))
                  (>= trap error-trap))
          (handle-break-args #'snarf-error-junk trap stream dstate)))))))
