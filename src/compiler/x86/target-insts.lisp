;;;; target-only stuff from CMU CL's src/compiler/x86/insts.lisp
;;;;
;;;; i.e. stuff which was in CMU CL's insts.lisp file, but which in
;;;; the SBCL build process can't be compiled into code for the
;;;; cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-X86-ASM")

;;; Return the operand size based on the prefixes and width bit from
;;; the dstate.
(defun inst-operand-size (dstate)
  (declare (type disassem-state dstate))
  (cond ((dstate-getprop dstate +operand-size-8+) :byte)
        ((dstate-getprop dstate +operand-size-16+) :word)
        (t +default-operand-size+)))

;;; Return the operand size for a "word-sized" operand based on the
;;; prefixes from the dstate.
(defun inst-word-operand-size (dstate)
  (declare (type disassem-state dstate))
  (if (dstate-getprop dstate +operand-size-16+) :word :dword))

;;; Disassembling x86 code needs to take into account little things
;;; like instructions that have a byte/word length bit in their
;;; encoding, prefixes to change the default word length for a single
;;; instruction, and so on.  Unfortunately, there is no easy way with
;;; this disassembler framework to handle prefixes that will work
;;; correctly in all cases, so we copy the x86-64 version which at
;;; least can handle the code output by the compiler.
;;;
;;; Width information for an instruction and whether a segment
;;; override prefix was seen is stored as an inst-prop on the dstate.
;;; The inst-props are cleared automatically after each non-prefix
;;; instruction, must be set by prefilters, and contain a single bit of
;;; data each (presence/absence).

;;; Returns either an integer, meaning a register, or a list of
;;; (BASE-REG OFFSET INDEX-REG INDEX-SCALE), where any component
;;; may be missing or nil to indicate that it's not used or has the
;;; obvious default value (e.g., 1 for the index-scale).
(defun prefilter-reg/mem (dstate mod r/m)
  (declare (type disassem-state dstate)
           (type (unsigned-byte 2) mod)
           (type (unsigned-byte 3) r/m))
  (cond ((= mod #b11)
           ;; registers
           r/m)
        ((= r/m #b100)
           ;; sib byte
           (let ((sib (read-suffix 8 dstate)))
             (declare (type (unsigned-byte 8) sib))
             (let ((base-reg (ldb (byte 3 0) sib))
                   (index-reg (ldb (byte 3 3) sib))
                   (index-scale (ldb (byte 2 6) sib)))
               (declare (type (unsigned-byte 3) base-reg index-reg)
                        (type (unsigned-byte 2) index-scale))
               (let* ((offset
                       (case mod
                         (#b00
                          (if (= base-reg #b101)
                              (read-signed-suffix 32 dstate)
                              nil))
                         (#b01
                          (read-signed-suffix 8 dstate))
                         (#b10
                          (read-signed-suffix 32 dstate)))))
                 (list (if (and (= mod #b00) (= base-reg #b101)) nil base-reg)
                       offset
                       (if (= index-reg #b100) nil index-reg)
                       (ash 1 index-scale))))))
        ((and (= mod #b00) (= r/m #b101))
           (list nil (read-signed-suffix 32 dstate)) )
        ((= mod #b00)
           (list r/m))
        ((= mod #b01)
           (list r/m (read-signed-suffix 8 dstate)))
        (t                            ; (= mod #b10)
           (list r/m (read-signed-suffix 32 dstate)))))

;;; This is a sort of bogus prefilter that just stores the info globally for
;;; other people to use; it probably never gets printed.
(defun prefilter-width (dstate value)
  (declare (type bit value) (type disassem-state dstate))
  (when (zerop value)
    (dstate-setprop dstate +operand-size-8+))
  value)

(defun print-reg-with-width (value width stream dstate)
  (declare (ignore dstate))
  (princ (aref (ecase width
                 ;; Notice that the this array is not the same
                 ;; as SB-VM::+BYTE-REGISTER-NAMES+
                 (:byte #(al cl dl bl ah ch dh bh))
                 (:word sb-vm::+word-register-names+)
                 (:dword sb-vm::+dword-register-names+))
               (if (eq width :byte) value (ash value 1)))
         stream)
  ;; XXX plus should do some source-var notes
  )

(defun print-reg (value stream dstate)
  (declare (type reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value
                        (inst-operand-size dstate)
                        stream
                        dstate))

(defun print-word-reg (value stream dstate)
  (declare (type reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value
                        (inst-word-operand-size dstate)
                        stream
                        dstate))

(defun print-byte-reg (value stream dstate)
  (declare (type reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value :byte stream dstate))

(defun print-addr-reg (value stream dstate)
  (declare (type reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value *default-address-size* stream dstate))

(defun print-reg/mem (value stream dstate)
  (declare (type (or list reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (if (typep value 'reg)
      (print-reg value stream dstate)
      (print-mem-access value stream nil dstate)))

;; Same as print-reg/mem, but prints an explicit size indicator for
;; memory references.
(defun print-sized-reg/mem (value stream dstate)
  (declare (type (or list reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (if (typep value 'reg)
      (print-reg value stream dstate)
      (print-mem-access value stream t dstate)))

(defun print-byte-reg/mem (value stream dstate)
  (declare (type (or list reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (if (typep value 'reg)
      (print-byte-reg value stream dstate)
      (print-mem-access value stream t dstate)))

(defun print-word-reg/mem (value stream dstate)
  (declare (type (or list reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (if (typep value 'reg)
      (print-word-reg value stream dstate)
      (print-mem-access value stream nil dstate)))

(defun print-label (value stream dstate)
  (declare (ignore dstate))
  (princ16 value stream))

(defun maybe-print-segment-override (stream dstate)
  (cond ((dstate-getprop dstate +fs-segment-prefix+)
         (princ "FS:" stream))
        ((dstate-getprop dstate +gs-segment-prefix+)
         (princ "GS:" stream))))

(defun print-mem-access (value stream print-size-p dstate)
  (declare (type list value)
           (type stream stream)
           (type (member t nil) print-size-p)
           (type disassem-state dstate))
  (when print-size-p
    (princ (inst-operand-size dstate) stream)
    (princ '| PTR | stream))
  (maybe-print-segment-override stream dstate)
  (write-char #\[ stream)
  (destructuring-bind (base &optional offset index scale) value
    (when base
      (print-addr-reg base stream dstate))
    (when index
      (when base (write-char #\+ stream))
      (print-addr-reg index stream dstate)
      (when (and scale (/= scale 1))
        (write-char #\* stream)
        (princ scale stream)))
    (when offset
      (cond ((or base index)
             (unless (zerop offset) ; don't show "+0"
               (unless (minusp offset)
                 (write-char #\+ stream))
               (princ offset stream)))
            (t ; memory absolute
             (setq offset (ldb (byte 32 0) offset)) ; never show as negative
             (princ16 offset stream)
             (or (nth-value 1 (note-code-constant offset dstate :absolute))
                 (maybe-note-assembler-routine offset nil dstate))))))
  (write-char #\] stream))

;;;; interrupt instructions

(defun break-control (chunk inst stream dstate)
  ;; Do not parse bytes following a trap instruction unless it belongs to lisp code.
  ;; C++ compilers will emit ud2 for various reasons.
  (when (sb-disassem::dstate-foreign-code-p dstate)
    (return-from break-control))
  (flet ((nt (x) (if stream (note x dstate))))
    (let ((trap (if (eq (sb-disassem::inst-print-name inst) 'ud2)
                    (word-imm-code chunk dstate)
                    (byte-imm-code chunk dstate))))
     (case trap
       (#.breakpoint-trap
        (nt "breakpoint trap"))
       (#.pending-interrupt-trap
        (nt "pending interrupt trap"))
       (#.halt-trap
        (nt "halt trap"))
       (#.fun-end-breakpoint-trap
        (nt "function end breakpoint trap"))
       (t
        (when (or (and (= trap cerror-trap) (progn (nt "cerror trap") t))
                  (>= trap error-trap))
          (handle-break-args #'snarf-error-junk trap stream dstate)))))))
