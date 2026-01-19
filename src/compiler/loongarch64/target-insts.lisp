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

(in-package "SB-LOONGARCH64-ASM")

(defun current-instruction (dstate &optional (offset 0))
  (sap-ref-int (dstate-segment-sap dstate)
               (+ (dstate-cur-offs dstate) offset)
               n-word-bytes
               (dstate-byte-order dstate)))


(defun instruction-opcode (instruction)
  (ldb (byte 7 25) instruction))


(defvar *disassem-use-lisp-reg-names* t
  "Control register display format:
   t = Use Lisp register name (such as A0)
   nil = Use LoongArch native register names (such as R4)")

(defvar *note-u-inst* (make-array 32 :initial-element nil)
  "A map for the disassembler indicating the target register and value
used in a u-type instruction.  This is used to make annotations about
function addresses and register values.")

(defconstant-eqx lisp-reg-symbols
  (map 'vector
       (lambda (name)
         (and name (make-symbol (concatenate 'string "$" name))))
       sb-vm::*register-names*)
  #'equalp)

(defconstant-eqx loongarch64-reg-symbols
  (coerce (loop for n from 0 to 31 collect (make-symbol (format nil "x~d" n)))
          'vector)
  #'equalp)

(defun print-reg (value stream dstate)
  (declare (type stream stream)
           (fixnum value))
  (let ((regname (aref (if *disassem-use-lisp-reg-names*
                           lisp-reg-symbols
                           loongarch64-reg-symbols)
                       value))
        (instruction (current-instruction dstate)))
    (case (instruction-opcode instruction)
      ((#b0001010)
       (note-absolute-u-inst value (ldb (byte 20 5) instruction)))
      ((#b0001110)
       (note-pc-relative-u-inst value (ldb (byte 20 5) instruction) dstate))
      (t
       ;; Delete any u-inst entry with the same target register.
       #+ (or)
       (setf (aref *note-u-inst* value) nil)))
    (princ regname stream)
    (maybe-note-associated-storage-ref
     value 'registers regname dstate)))

(defun note-absolute-u-inst (rd u-imm)
  (setf (aref *note-u-inst* rd)
        (sign-extend u-imm 20)))

(defun note-pc-relative-u-inst (rd u-imm dstate)
  (setf (aref *note-u-inst* rd) (+ (dstate-cur-addr dstate)
                                   (coerce-signed u-imm 20))))

(defconstant-eqx float-reg-symbols
  (coerce (loop for n from 0 to 31 collect (make-symbol (format nil "ft~d" n)))
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
  (princ value stream))

(defun use-b-label (imm16 dstate)
  (declare (type integer imm16)
           (type disassem-state dstate))
  (let* ((pc (dstate-cur-addr dstate))
         ;; signed immediate is 16 bits
         (offset (ash (coerce-signed imm16 16) 2))
         (addr (+ pc offset))
         ;; lowtag pointer adjustment
         (final (if (= (logand addr lowtag-mask) other-pointer-lowtag)
                    (- addr (- other-pointer-lowtag n-word-bytes))
                    addr)))
    final))


(defun use-bl-label (value dstate)
  (declare (type disassem-state dstate))
  (let* ((value (coerce-signed
                (logior (ash (first value) 16)
                      (ash (second value) 10))
                26))
         (address (+ value (dstate-cur-addr dstate))))
    (if (= (logand address lowtag-mask) other-pointer-lowtag)
        (- address (- other-pointer-lowtag n-word-bytes))
        address)))

(defun use-j-label (offs16 dstate)
  (declare (type integer offs16)
           (type disassem-state dstate))
  (let* ((pc (dstate-cur-addr dstate))
         ;; sign-extend and <<2
         (offset (ash (coerce-signed offs16 16) 2))
         (target (+ pc offset))
         (final (if (= (logand target lowtag-mask)
                       other-pointer-lowtag)
                    (- target (- other-pointer-lowtag n-word-bytes))
                    target)))
    final))


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
  (princ (aref #(rne rz rdn rup rmm unused1 unused2 dynamic)
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

(defun print-dbar-ordering (value stream dstate)
  (declare (type (unsigned-byte 4) value)
           (type stream stream)
           (ignore dstate))
  (let ((parts (loop for i from 3 downto 0
                     when (logbitp i value)
                     collect (aref #(i o r w) i))))
    (if parts
        (format stream "~{~a~}" parts)
        (princ 'none stream))))

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
    (#.sb-vm::tp-offset
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
                       (rest value)
                       dstate))

(defun print-jirl-annotation (value stream dstate)
  (declare (ignore stream))
  (destructuring-bind (rd i-imm) value
    (maybe-note-assembler-routine (maybe-augment rd i-imm) t dstate)))

(defun break-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap (break-code chunk dstate)))
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
