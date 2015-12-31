;;;; that part of the description of the x86 instruction set (for
;;;; 80386 and above) which can live on the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!X86-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from this package into SB-VM
  (import '(*condition-name-vec* conditional-opcode
            register-p ; FIXME: rename to GPR-P
            make-ea ea-disp width-bits) 'sb!vm)
  ;; Imports from SB-VM into this package
  (import '(sb!vm::*byte-sc-names* sb!vm::*word-sc-names* sb!vm::*dword-sc-names*
            sb!vm::frame-byte-offset
            sb!vm::registers sb!vm::float-registers sb!vm::stack))) ; SB names

(!begin-instruction-definitions)

(setf *disassem-inst-alignment-bytes* 1)

(deftype reg () '(unsigned-byte 3))

(def!constant +default-operand-size+ :dword)

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

(defun offset-next (value dstate)
  (declare (type integer value)
           (type disassem-state dstate))
  (+ (dstate-next-addr dstate) value))

(defparameter *default-address-size*
  ;; Actually, :DWORD is the only one really supported.
  :dword)

(defparameter *byte-reg-names*
  #(al cl dl bl ah ch dh bh))
(defparameter *word-reg-names*
  #(ax cx dx bx sp bp si di))
(defparameter *dword-reg-names*
  #(eax ecx edx ebx esp ebp esi edi))

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

;;; Return the operand size based on the prefixes and width bit from
;;; the dstate.
(defun inst-operand-size (dstate)
  (declare (type disassem-state dstate))
  (cond ((dstate-get-inst-prop dstate 'operand-size-8) :byte)
        ((dstate-get-inst-prop dstate 'operand-size-16) :word)
        (t +default-operand-size+)))

;;; Return the operand size for a "word-sized" operand based on the
;;; prefixes from the dstate.
(defun inst-word-operand-size (dstate)
  (declare (type disassem-state dstate))
  (if (dstate-get-inst-prop dstate 'operand-size-16) :word :dword))

(defun print-reg-with-width (value width stream dstate)
  (declare (ignore dstate))
  (princ (aref (ecase width
                 (:byte *byte-reg-names*)
                 (:word *word-reg-names*)
                 (:dword *dword-reg-names*))
               value)
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
  (cond ((dstate-get-inst-prop dstate 'fs-segment-prefix)
         (princ "FS:" stream))
        ((dstate-get-inst-prop dstate 'gs-segment-prefix)
         (princ "GS:" stream))))

;;; Returns either an integer, meaning a register, or a list of
;;; (BASE-REG OFFSET INDEX-REG INDEX-SCALE), where any component
;;; may be missing or nil to indicate that it's not used or has the
;;; obvious default value (e.g., 1 for the index-scale).
(defun prefilter-reg/mem (value dstate)
  (declare (type list value)
           (type disassem-state dstate))
  (let ((mod (car value))
        (r/m (cadr value)))
    (declare (type (unsigned-byte 2) mod)
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
           (list r/m (read-signed-suffix 32 dstate))))))


;;; This is a sort of bogus prefilter that just stores the info globally for
;;; other people to use; it probably never gets printed.
(defun prefilter-width (value dstate)
  (declare (type bit value)
           (type disassem-state dstate))
  (when (zerop value)
    (dstate-put-inst-prop dstate 'operand-size-8))
  value)

;;; This prefilter is used solely for its side effect, namely to put
;;; the property OPERAND-SIZE-16 into the DSTATE.
(defun prefilter-x66 (value dstate)
  (declare (type (eql #x66) value)
           (ignore value)
           (type disassem-state dstate))
  (dstate-put-inst-prop dstate 'operand-size-16))

;;; This prefilter is used solely for its side effect, namely to put
;;; one of the properties [FG]S-SEGMENT-PREFIX into the DSTATE.
;;; Unlike PREFILTER-X66, this prefilter only catches the low bit of
;;; the prefix byte.
(defun prefilter-seg (value dstate)
  (declare (type bit value)
           (type disassem-state dstate))
  (dstate-put-inst-prop
   dstate (elt '(fs-segment-prefix gs-segment-prefix) value)))

(defun read-address (value dstate)
  (declare (ignore value))              ; always nil anyway
  (read-suffix (width-bits *default-address-size*) dstate))

(defun width-bits (width)
  (ecase width
    (:byte 8)
    (:word 16)
    (:dword 32)
    (:float 32)
    (:double 64)))

) ; EVAL-WHEN

;;;; disassembler argument types

(define-arg-type displacement
  :sign-extend t
  :use-label #'offset-next
  :printer (lambda (value stream dstate)
             (maybe-note-assembler-routine value nil dstate)
             (print-label value stream dstate)))

(define-arg-type accum
  :printer (lambda (value stream dstate)
             (declare (ignore value)
                      (type stream stream)
                      (type disassem-state dstate))
             (print-reg 0 stream dstate)))

(define-arg-type word-accum
  :printer (lambda (value stream dstate)
             (declare (ignore value)
                      (type stream stream)
                      (type disassem-state dstate))
             (print-word-reg 0 stream dstate)))

(define-arg-type reg :printer #'print-reg)

(define-arg-type addr-reg :printer #'print-addr-reg)

(define-arg-type word-reg :printer #'print-word-reg)

(define-arg-type imm-addr
  :prefilter #'read-address
  :printer #'print-label)

(define-arg-type imm-data
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (read-suffix (width-bits (inst-operand-size dstate)) dstate)))

(define-arg-type signed-imm-data
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (let ((width (inst-operand-size dstate)))
                 (read-signed-suffix (width-bits width) dstate))))

(define-arg-type imm-byte
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (read-suffix 8 dstate)))

(define-arg-type signed-imm-byte
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (read-signed-suffix 8 dstate)))

(define-arg-type signed-imm-dword
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (read-signed-suffix 32 dstate)))

(define-arg-type imm-word
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (let ((width (inst-word-operand-size dstate)))
                 (read-suffix (width-bits width) dstate))))

(define-arg-type signed-imm-word
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (let ((width (inst-word-operand-size dstate)))
                 (read-signed-suffix (width-bits width) dstate))))

;;; needed for the ret imm16 instruction
(define-arg-type imm-word-16
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (read-suffix 16 dstate)))

(define-arg-type reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-reg/mem)
(define-arg-type sized-reg/mem
  ;; Same as reg/mem, but prints an explicit size indicator for
  ;; memory references.
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-reg/mem)
(define-arg-type byte-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-byte-reg/mem)
(define-arg-type word-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-word-reg/mem)

;;; added by jrd
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
(defun print-fp-reg (value stream dstate)
  (declare (ignore dstate))
  (format stream "FR~D" value))
(defun prefilter-fp-reg (value dstate)
  ;; just return it
  (declare (ignore dstate))
  value)
) ; EVAL-WHEN
(define-arg-type fp-reg :prefilter #'prefilter-fp-reg
                        :printer #'print-fp-reg)

(define-arg-type width
  :prefilter #'prefilter-width
  :printer (lambda (value stream dstate)
             (declare (ignore value))
             (princ (schar (symbol-name (inst-operand-size dstate)) 0)
                    stream)))

;;; Used to capture the effect of the #x66 operand size override prefix.
(define-arg-type x66 :prefilter #'prefilter-x66)

;;; Used to capture the effect of the #x64 and #x65 segment override
;;; prefixes.
(define-arg-type seg :prefilter #'prefilter-seg)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defparameter *conditions*
  '((:o . 0)
    (:no . 1)
    (:b . 2) (:nae . 2) (:c . 2)
    (:nb . 3) (:ae . 3) (:nc . 3)
    (:eq . 4) (:e . 4) (:z . 4)
    (:ne . 5) (:nz . 5)
    (:be . 6) (:na . 6)
    (:nbe . 7) (:a . 7)
    (:s . 8)
    (:ns . 9)
    (:p . 10) (:pe . 10)
    (:np . 11) (:po . 11)
    (:l . 12) (:nge . 12)
    (:nl . 13) (:ge . 13)
    (:le . 14) (:ng . 14)
    (:nle . 15) (:g . 15)))
(defparameter *condition-name-vec*
  (let ((vec (make-array 16 :initial-element nil)))
    (dolist (cond *conditions*)
      (when (null (aref vec (cdr cond)))
        (setf (aref vec (cdr cond)) (car cond))))
    vec))
) ; EVAL-WHEN

;;; Set assembler parameters. (In CMU CL, this was done with
;;; a call to a macro DEF-ASSEMBLER-PARAMS.)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf sb!assem:*assem-scheduler-p* nil))

(define-arg-type condition-code :printer *condition-name-vec*)

(defun conditional-opcode (condition)
  (cdr (assoc condition *conditions* :test #'eq)))

;;;; disassembler instruction formats

(eval-when (:compile-toplevel :execute)
  (defun swap-if (direction field1 separator field2)
    `(:if (,direction :constant 0)
          (,field1 ,separator ,field2)
          (,field2 ,separator ,field1))))

(define-instruction-format (byte 8 :default-printer '(:name))
  (op    :field (byte 8 0))
  ;; optional fields
  (accum :type 'accum)
  (imm))

;;; Prefix instructions

(define-instruction-format (x66 8)
  (x66   :field (byte 8 0) :type 'x66 :value #x66))

(define-instruction-format (seg 8)
  (seg   :field (byte 7 1) :value #x32)
  (fsgs  :field (byte 1 0) :type 'seg))

(define-instruction-format (simple 8)
  (op    :field (byte 7 1))
  (width :field (byte 1 0) :type 'width)
  ;; optional fields
  (accum :type 'accum)
  (imm))

(define-instruction-format (two-bytes 16 :default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8))))

;;; Same as simple, but with direction bit
(define-instruction-format (simple-dir 8 :include simple)
  (op :field (byte 6 2))
  (dir :field (byte 1 1)))

;;; Same as simple, but with the immediate value occurring by default,
;;; and with an appropiate printer.
(define-instruction-format (accum-imm 8
                                     :include simple
                                     :default-printer '(:name
                                                        :tab accum ", " imm))
  (imm :type 'imm-data))

(define-instruction-format (reg-no-width 8 :default-printer '(:name :tab reg))
  (op    :field (byte 5 3))
  (reg   :field (byte 3 0) :type 'word-reg)
  ;; optional fields
  (accum :type 'word-accum)
  (imm))

;;; adds a width field to reg-no-width
(define-instruction-format (reg 8 :default-printer '(:name :tab reg))
  (op    :field (byte 4 4))
  (width :field (byte 1 3) :type 'width)
  (reg   :field (byte 3 0) :type 'reg)
  ;; optional fields
  (accum :type 'accum)
  (imm)
  )

;;; Same as reg, but with direction bit
(define-instruction-format (reg-dir 8 :include reg)
  (op  :field (byte 3 5))
  (dir :field (byte 1 4)))

(define-instruction-format (reg-reg/mem 16
                                        :default-printer
                                        `(:name :tab reg ", " reg/mem))
  (op      :field (byte 7 1))
  (width   :field (byte 1 0)    :type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
                                :type 'reg/mem)
  (reg     :field (byte 3 11)   :type 'reg)
  ;; optional fields
  (imm))

;;; same as reg-reg/mem, but with direction bit
(define-instruction-format (reg-reg/mem-dir 16
                                        :include reg-reg/mem
                                        :default-printer
                                        `(:name
                                          :tab
                                          ,(swap-if 'dir 'reg/mem ", " 'reg)))
  (op  :field (byte 6 2))
  (dir :field (byte 1 1)))

;;; Same as reg-rem/mem, but uses the reg field as a second op code.
(define-instruction-format (reg/mem 16 :default-printer '(:name :tab reg/mem))
  (op      :fields (list (byte 7 1) (byte 3 11)))
  (width   :field (byte 1 0)    :type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
                                :type 'sized-reg/mem)
  ;; optional fields
  (imm))

;;; Same as reg/mem, but with the immediate value occurring by default,
;;; and with an appropiate printer.
(define-instruction-format (reg/mem-imm 16
                                        :include reg/mem
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (reg/mem :type 'sized-reg/mem)
  (imm     :type 'imm-data))

;;; Same as reg/mem, but with using the accumulator in the default printer
(define-instruction-format
    (accum-reg/mem 16
     :include reg/mem :default-printer '(:name :tab accum ", " reg/mem))
  (reg/mem :type 'reg/mem)              ; don't need a size
  (accum :type 'accum))

;;; Same as reg-reg/mem, but with a prefix of #b00001111
(define-instruction-format (ext-reg-reg/mem 24
                                        :default-printer
                                        `(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op      :field (byte 7 9))
  (width   :field (byte 1 8)    :type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem)
  (reg     :field (byte 3 19)   :type 'reg)
  ;; optional fields
  (imm))

(define-instruction-format (ext-reg-reg/mem-no-width 24
                                        :default-printer
                                        `(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem)
  (reg     :field (byte 3 19)   :type 'reg)
  ;; optional fields
  (imm))

(define-instruction-format (ext-reg/mem-no-width 24
                                        :default-printer
                                        `(:name :tab reg/mem))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op      :fields (list (byte 8 8) (byte 3 19)))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem))

;;; reg-no-width with #x0f prefix
(define-instruction-format (ext-reg-no-width 16
                                        :default-printer '(:name :tab reg))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op    :field (byte 5 11))
  (reg   :field (byte 3 8) :type 'reg))

;;; Same as reg/mem, but with a prefix of #b00001111
(define-instruction-format (ext-reg/mem 24
                                        :default-printer '(:name :tab reg/mem))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op      :fields (list (byte 7 9) (byte 3 19)))
  (width   :field (byte 1 8)    :type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'sized-reg/mem)
  ;; optional fields
  (imm))

(define-instruction-format (ext-reg/mem-imm 24
                                        :include ext-reg/mem
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (imm :type 'imm-data))

(define-instruction-format (ext-reg/mem-no-width+imm8 24
                                        :include ext-reg/mem-no-width
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (imm :type 'imm-byte))

;;;; This section was added by jrd, for fp instructions.

;;; regular fp inst to/from registers/memory
(define-instruction-format (floating-point 16
                                        :default-printer
                                        `(:name :tab reg/mem))
  (prefix :field (byte 5 3) :value #b11011)
  (op     :fields (list (byte 3 0) (byte 3 11)))
  (reg/mem :fields (list (byte 2 14) (byte 3 8)) :type 'reg/mem))

;;; fp insn to/from fp reg
(define-instruction-format (floating-point-fp 16
                                        :default-printer `(:name :tab fp-reg))
  (prefix :field (byte 5 3) :value #b11011)
  (suffix :field (byte 2 14) :value #b11)
  (op     :fields (list (byte 3 0) (byte 3 11)))
  (fp-reg :field (byte 3 8) :type 'fp-reg))

;;; fp insn to/from fp reg, with the reversed source/destination flag.
(define-instruction-format (floating-point-fp-d 16
                            :default-printer
                            `(:name :tab ,(swap-if 'd "ST0" ", " 'fp-reg)))
  (prefix :field (byte 5 3) :value #b11011)
  (suffix :field (byte 2 14) :value #b11)
  (op     :fields (list (byte 2 0) (byte 3 11)))
  (d      :field (byte 1 2))
  (fp-reg :field (byte 3 8) :type 'fp-reg))


;;; (added by (?) pfw)
;;; fp no operand isns
(define-instruction-format (floating-point-no 16 :default-printer '(:name))
  (prefix :field (byte 8  0) :value #b11011001)
  (suffix :field (byte 3 13) :value #b111)
  (op     :field (byte 5  8)))

(define-instruction-format (floating-point-3 16 :default-printer '(:name))
  (prefix :field (byte 5 3) :value #b11011)
  (suffix :field (byte 2 14) :value #b11)
  (op     :fields (list (byte 3 0) (byte 6 8))))

(define-instruction-format (floating-point-5 16 :default-printer '(:name))
  (prefix :field (byte 8  0) :value #b11011011)
  (suffix :field (byte 3 13) :value #b111)
  (op     :field (byte 5  8)))

(define-instruction-format (floating-point-st 16 :default-printer '(:name))
  (prefix :field (byte 8  0) :value #b11011111)
  (suffix :field (byte 3 13) :value #b111)
  (op     :field (byte 5  8)))

(define-instruction-format (string-op 8
                                     :include simple
                                     :default-printer '(:name width)))

(define-instruction-format (short-cond-jump 16)
  (op    :field (byte 4 4))
  (cc    :field (byte 4 0) :type 'condition-code)
  (label :field (byte 8 8) :type 'displacement))

(define-instruction-format (short-jump 16 :default-printer '(:name :tab label))
  (const :field (byte 4 4) :value #b1110)
  (op    :field (byte 4 0))
  (label :field (byte 8 8) :type 'displacement))

(define-instruction-format (near-cond-jump 16)
  (op    :fields (list (byte 8 0) (byte 4 12)) :value '(#b00001111 #b1000))
  (cc    :field (byte 4 8) :type 'condition-code)
  ;; The disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the offset.
  (label :type 'displacement
         :prefilter (lambda (value dstate)
                      (declare (ignore value)) ; always nil anyway
                      (read-signed-suffix 32 dstate))))

(define-instruction-format (near-jump 8 :default-printer '(:name :tab label))
  (op    :field (byte 8 0))
  ;; The disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the address.
  (label :type 'displacement
         :prefilter (lambda (value dstate)
                      (declare (ignore value)) ; always nil anyway
                      (read-signed-suffix 32 dstate))))


(define-instruction-format (cond-set 24
                                     :default-printer '('set cc :tab reg/mem))
  (prefix :field (byte 8 0) :value #b00001111)
  (op    :field (byte 4 12) :value #b1001)
  (cc    :field (byte 4 8) :type 'condition-code)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
           :type 'byte-reg/mem)
  (reg     :field (byte 3 19)   :value #b000))

(define-instruction-format (cond-move 24
                                     :default-printer
                                        '('cmov cc :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op      :field (byte 4 12)   :value #b0100)
  (cc      :field (byte 4 8)    :type 'condition-code)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem)
  (reg     :field (byte 3 19)   :type 'reg))

(define-instruction-format (enter-format 32
                                     :default-printer '(:name
                                                        :tab disp
                                                        (:unless (:constant 0)
                                                          ", " level)))
  (op :field (byte 8 0))
  (disp :field (byte 16 8))
  (level :field (byte 8 24)))

(define-instruction-format (prefetch 24 :default-printer '(:name ", " reg/mem))
  (prefix :field (byte 8 0) :value #b00001111)
  (op :field (byte 8 8) :value #b00011000)
  (reg/mem :fields (list (byte 2 22) (byte 3 16)) :type 'byte-reg/mem)
  (reg :field (byte 3 19) :type 'reg))

;;; Single byte instruction with an immediate byte argument.
(define-instruction-format (byte-imm 16 :default-printer '(:name :tab code))
 (op :field (byte 8 0))
 (code :field (byte 8 8) :reader byte-imm-code))

;;; Two byte instruction with an immediate byte argument.
;;;
(define-instruction-format (word-imm 24 :default-printer '(:name :tab code))
  (op :field (byte 16 0))
  (code :field (byte 8 16) :reader word-imm-code))


;;;; primitive emitters

(define-bitfield-emitter emit-word 16
  (byte 16 0))

(define-bitfield-emitter emit-dword 32
  (byte 32 0))

(define-bitfield-emitter emit-byte-with-reg 8
  (byte 5 3) (byte 3 0))

(define-bitfield-emitter emit-mod-reg-r/m-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))

(define-bitfield-emitter emit-sib-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))

;;;; fixup emitters

(defun emit-absolute-fixup (segment fixup)
  (note-fixup segment :absolute fixup)
  (let ((offset (fixup-offset fixup)))
    (if (label-p offset)
        (emit-back-patch segment
                         4 ; FIXME: n-word-bytes
                         (lambda (segment posn)
                           (declare (ignore posn))
                           (emit-dword segment
                                       (- (+ (component-header-length)
                                             (or (label-position offset)
                                                 0))
                                          other-pointer-lowtag))))
        (emit-dword segment (or offset 0)))))

(defun emit-relative-fixup (segment fixup)
  (note-fixup segment :relative fixup)
  (emit-dword segment (or (fixup-offset fixup) 0)))

;;;; the effective-address (ea) structure

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (aver (eq (sb-name (sc-sb (tn-sc tn))) 'registers))
  (let ((offset (tn-offset tn)))
    (logior (ash (logand offset 1) 2)
            (ash offset -1))))

(defstruct (ea (:constructor make-ea (size &key base index scale disp))
               (:copier nil))
  (size nil :type (member :byte :word :dword))
  (base nil :type (or tn null))
  (index nil :type (or tn null))
  (scale 1 :type (member 1 2 4 8))
  (disp 0 :type (or (unsigned-byte 32) (signed-byte 32) fixup)))
(def!method print-object ((ea ea) stream)
  (cond ((or *print-escape* *print-readably*)
         (print-unreadable-object (ea stream :type t)
           (format stream
                   "~S~@[ base=~S~]~@[ index=~S~]~@[ scale=~S~]~@[ disp=~S~]"
                   (ea-size ea)
                   (ea-base ea)
                   (ea-index ea)
                   (let ((scale (ea-scale ea)))
                     (if (= scale 1) nil scale))
                   (ea-disp ea))))
        (t
         (format stream "~A PTR [" (symbol-name (ea-size ea)))
         (when (ea-base ea)
           (write-string (sb!c::location-print-name (ea-base ea)) stream)
           (when (ea-index ea)
             (write-string "+" stream)))
         (when (ea-index ea)
           (write-string (sb!c::location-print-name (ea-index ea)) stream))
         (unless (= (ea-scale ea) 1)
           (format stream "*~A" (ea-scale ea)))
         (typecase (ea-disp ea)
           (null)
           (integer
            (format stream "~@D" (ea-disp ea)))
           (t
            (format stream "+~A" (ea-disp ea))))
         (write-char #\] stream))))

(defun emit-ea (segment thing reg &optional allow-constants)
  (etypecase thing
    (tn
     (ecase (sb-name (sc-sb (tn-sc thing)))
       (registers
        (emit-mod-reg-r/m-byte segment #b11 reg (reg-tn-encoding thing)))
       (stack
        ;; Convert stack tns into an index off of EBP.
        (let ((disp (frame-byte-offset (tn-offset thing))))
          (cond ((<= -128 disp 127)
                 (emit-mod-reg-r/m-byte segment #b01 reg #b101)
                 (emit-byte segment disp))
                (t
                 (emit-mod-reg-r/m-byte segment #b10 reg #b101)
                 (emit-dword segment disp)))))
       (constant
        (unless allow-constants
          (error
           "Constant TNs can only be directly used in MOV, PUSH, and CMP."))
        (emit-mod-reg-r/m-byte segment #b00 reg #b101)
        (emit-absolute-fixup segment
                             (make-fixup nil
                                         :code-object
                                         (- (* (tn-offset thing) n-word-bytes)
                                            other-pointer-lowtag))))))
    (ea
     (let* ((base (ea-base thing))
            (index (ea-index thing))
            (scale (ea-scale thing))
            (disp (ea-disp thing))
            (mod (cond ((or (null base)
                            (and (eql disp 0)
                                 (not (= (reg-tn-encoding base) #b101))))
                        #b00)
                       ((and (fixnump disp) (<= -128 disp 127))
                        #b01)
                       (t
                        #b10)))
            (r/m (cond (index #b100)
                       ((null base) #b101)
                       (t (reg-tn-encoding base)))))
       (when (and (fixup-p disp)
                  (label-p (fixup-offset disp)))
         (aver (null base))
         (aver (null index))
         (return-from emit-ea (emit-ea segment disp reg allow-constants)))
       (emit-mod-reg-r/m-byte segment mod reg r/m)
       (when (= r/m #b100)
         (let ((ss (1- (integer-length scale)))
               (index (if (null index)
                          #b100
                          (let ((index (reg-tn-encoding index)))
                            (if (= index #b100)
                                (error "can't index off of ESP")
                                index))))
               (base (if (null base)
                         #b101
                         (reg-tn-encoding base))))
           (emit-sib-byte segment ss index base)))
       (cond ((= mod #b01)
              (emit-byte segment disp))
             ((or (= mod #b10) (null base))
              (if (fixup-p disp)
                  (emit-absolute-fixup segment disp)
                  (emit-dword segment disp))))))
    (fixup
     (emit-mod-reg-r/m-byte segment #b00 reg #b101)
     (emit-absolute-fixup segment thing))))

(defun fp-reg-tn-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'float-registers)))

;;; like the above, but for fp-instructions--jrd
(defun emit-fp-op (segment thing op)
  (if (fp-reg-tn-p thing)
      (emit-byte segment (dpb op (byte 3 3) (dpb (tn-offset thing)
                                                 (byte 3 0)
                                                 #b11000000)))
    (emit-ea segment thing op)))

(defun byte-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) *byte-sc-names*)
       t))

(defun byte-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :byte))
    (tn
     (and (member (sc-name (tn-sc thing)) *byte-sc-names*) t))
    (t nil)))

(defun word-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) *word-sc-names*)
       t))

(defun word-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :word))
    (tn (and (member (sc-name (tn-sc thing)) *word-sc-names*) t))
    (t nil)))

(defun dword-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) *dword-sc-names*)
       t))

(defun dword-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :dword))
    (tn
     (and (member (sc-name (tn-sc thing)) *dword-sc-names*) t))
    (t nil)))

(defun register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)))

(defun accumulator-p (thing)
  (and (register-p thing)
       (= (tn-offset thing) 0)))

;;;; utilities

(def!constant +operand-size-prefix-byte+ #b01100110)

(defun maybe-emit-operand-size-prefix (segment size)
  (unless (or (eq size :byte) (eq size +default-operand-size+))
    (emit-byte segment +operand-size-prefix-byte+)))

(defun operand-size (thing)
  (typecase thing
    (tn
     ;; FIXME: might as well be COND instead of having to use #. readmacro
     ;; to hack up the code
     (case (sc-name (tn-sc thing))
       (#.*dword-sc-names*
        :dword)
       (#.*word-sc-names*
        :word)
       (#.*byte-sc-names*
        :byte)
       ;; added by jrd: float-registers is a separate size (?)
       (#.sb!vm::*float-sc-names*
        :float)
       (#.sb!vm::*double-sc-names*
        :double)
       (t
        (error "can't tell the size of ~S ~S" thing (sc-name (tn-sc thing))))))
    (ea
     (ea-size thing))
    (t
     nil)))

(defun matching-operand-size (dst src)
  (let ((dst-size (operand-size dst))
        (src-size (operand-size src)))
    (if dst-size
        (if src-size
            (if (eq dst-size src-size)
                dst-size
                (error "size mismatch: ~S is a ~S and ~S is a ~S."
                       dst dst-size src src-size))
            dst-size)
        (if src-size
            src-size
            (error "can't tell the size of either ~S or ~S" dst src)))))

(defun emit-sized-immediate (segment size value)
  (ecase size
    (:byte
     (emit-byte segment value))
    (:word
     (emit-word segment value))
    (:dword
     (emit-dword segment value))))

;;;; prefixes

(define-instruction x66 (segment)
  (:printer x66 () nil :print-name nil)
  (:emitter
   (bug "#X66 prefix used as a standalone instruction")))

(defun emit-prefix (segment name)
  (ecase name
    ((nil))
    (:lock
     #!+sb-thread
     (emit-byte segment #xf0))
    (:fs
     (emit-byte segment #x64))
    (:gs
     (emit-byte segment #x65))))

(define-instruction fs (segment)
  (:printer seg ((fsgs #b0)) nil :print-name nil)
  (:emitter
   (bug "FS prefix used as a standalone instruction")))

(define-instruction gs (segment)
  (:printer seg ((fsgs #b1)) nil :print-name nil)
  (:emitter
   (bug "GS prefix used as a standalone instruction")))

(define-instruction lock (segment)
  (:printer byte ((op #b11110000)) nil)
  (:emitter
   (bug "LOCK prefix used as a standalone instruction")))

(define-instruction rep (segment)
  (:emitter
   (emit-byte segment #b11110011)))

(define-instruction repe (segment)
  (:printer byte ((op #b11110011)) nil)
  (:emitter
   (emit-byte segment #b11110011)))

(define-instruction repne (segment)
  (:printer byte ((op #b11110010)) nil)
  (:emitter
   (emit-byte segment #b11110010)))

;;;; general data transfer

(define-instruction mov (segment dst src &optional prefix)
  ;; immediate to register
  (:printer reg ((op #b1011) (imm nil :type 'imm-data))
            '(:name :tab reg ", " imm))
  ;; absolute mem to/from accumulator
  (:printer simple-dir ((op #b101000) (imm nil :type 'imm-addr))
            `(:name :tab ,(swap-if 'dir 'accum ", " '("[" imm "]"))))
  ;; register to/from register/memory
  (:printer reg-reg/mem-dir ((op #b100010)))
  ;; immediate to register/memory
  (:printer reg/mem-imm ((op '(#b1100011 #b000))))

  (:emitter
   (emit-prefix segment prefix)
   (let ((size (matching-operand-size dst src)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((register-p dst)
            (cond ((integerp src)
                   (emit-byte-with-reg segment
                                       (if (eq size :byte)
                                           #b10110
                                           #b10111)
                                       (reg-tn-encoding dst))
                   (emit-sized-immediate segment size src))
                  ((and (fixup-p src) (accumulator-p dst))
                   (emit-byte segment
                              (if (eq size :byte)
                                  #b10100000
                                  #b10100001))
                   (emit-absolute-fixup segment src))
                  (t
                   (emit-byte segment
                              (if (eq size :byte)
                                  #b10001010
                                  #b10001011))
                   (emit-ea segment src (reg-tn-encoding dst) t))))
           ((and (fixup-p dst) (accumulator-p src))
            (emit-byte segment (if (eq size :byte) #b10100010 #b10100011))
            (emit-absolute-fixup segment dst))
           ((integerp src)
            (emit-byte segment (if (eq size :byte) #b11000110 #b11000111))
            (emit-ea segment dst #b000)
            (emit-sized-immediate segment size src))
           ((register-p src)
            (emit-byte segment (if (eq size :byte) #b10001000 #b10001001))
            (emit-ea segment dst (reg-tn-encoding src)))
           ((fixup-p src)
            (aver (eq size :dword))
            (emit-byte segment #b11000111)
            (emit-ea segment dst #b000)
            (emit-absolute-fixup segment src))
           (t
            (error "bogus arguments to MOV: ~S ~S" dst src))))))

(defun emit-move-with-extension (segment dst src opcode)
  (aver (register-p dst))
  (let ((dst-size (operand-size dst))
        (src-size (operand-size src)))
    (ecase dst-size
      (:word
       (aver (eq src-size :byte))
       (maybe-emit-operand-size-prefix segment :word)
       (emit-byte segment #b00001111)
       (emit-byte segment opcode)
       (emit-ea segment src (reg-tn-encoding dst)))
      (:dword
       (ecase src-size
         (:byte
          (maybe-emit-operand-size-prefix segment :dword)
          (emit-byte segment #b00001111)
          (emit-byte segment opcode)
          (emit-ea segment src (reg-tn-encoding dst)))
         (:word
          (emit-byte segment #b00001111)
          (emit-byte segment (logior opcode 1))
          (emit-ea segment src (reg-tn-encoding dst))))))))

(define-instruction movsx (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011111)
                             (reg nil :type 'word-reg)
                             (reg/mem nil :type 'sized-reg/mem)))
  (:emitter (emit-move-with-extension segment dst src #b10111110)))

(define-instruction movzx (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011011)
                             (reg nil :type 'word-reg)
                             (reg/mem nil :type 'sized-reg/mem)))
  (:emitter (emit-move-with-extension segment dst src #b10110110)))

(define-instruction push (segment src &optional prefix)
  ;; register
  (:printer reg-no-width ((op #b01010)))
  ;; register/memory
  (:printer reg/mem ((op '(#b1111111 #b110)) (width 1)))
  ;; immediate
  (:printer byte ((op #b01101010) (imm nil :type 'signed-imm-byte))
            '(:name :tab imm))
  (:printer byte ((op #b01101000) (imm nil :type 'imm-word))
            '(:name :tab imm))
  ;; ### segment registers?

  (:emitter
   (emit-prefix segment prefix)
   (cond ((integerp src)
          (cond ((<= -128 src 127)
                 (emit-byte segment #b01101010)
                 (emit-byte segment src))
                (t
                 (emit-byte segment #b01101000)
                 (emit-dword segment src))))
         ((fixup-p src)
          ;; Interpret the fixup as an immediate dword to push.
          (emit-byte segment #b01101000)
          (emit-absolute-fixup segment src))
         (t
          (let ((size (operand-size src)))
            (aver (not (eq size :byte)))
            (maybe-emit-operand-size-prefix segment size)
            (cond ((register-p src)
                   (emit-byte-with-reg segment #b01010 (reg-tn-encoding src)))
                  (t
                   (emit-byte segment #b11111111)
                   (emit-ea segment src #b110 t))))))))

(define-instruction pusha (segment)
  (:printer byte ((op #b01100000)))
  (:emitter
   (emit-byte segment #b01100000)))

(define-instruction pop (segment dst)
  (:printer reg-no-width ((op #b01011)))
  (:printer reg/mem ((op '(#b1000111 #b000)) (width 1)))
  (:emitter
   (let ((size (operand-size dst)))
     (aver (not (eq size :byte)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((register-p dst)
            (emit-byte-with-reg segment #b01011 (reg-tn-encoding dst)))
           (t
            (emit-byte segment #b10001111)
            (emit-ea segment dst #b000))))))

(define-instruction popa (segment)
  (:printer byte ((op #b01100001)))
  (:emitter
   (emit-byte segment #b01100001)))

(define-instruction xchg (segment operand1 operand2)
  ;; Register with accumulator.
  (:printer reg-no-width ((op #b10010)) '(:name :tab accum ", " reg))
  ;; Register/Memory with Register.
  (:printer reg-reg/mem ((op #b1000011)))
  (:emitter
   (let ((size (matching-operand-size operand1 operand2)))
     (maybe-emit-operand-size-prefix segment size)
     (labels ((xchg-acc-with-something (acc something)
                (if (and (not (eq size :byte)) (register-p something))
                    (emit-byte-with-reg segment
                                        #b10010
                                        (reg-tn-encoding something))
                    (xchg-reg-with-something acc something)))
              (xchg-reg-with-something (reg something)
                (emit-byte segment (if (eq size :byte) #b10000110 #b10000111))
                (emit-ea segment something (reg-tn-encoding reg))))
       (cond ((accumulator-p operand1)
              (xchg-acc-with-something operand1 operand2))
             ((accumulator-p operand2)
              (xchg-acc-with-something operand2 operand1))
             ((register-p operand1)
              (xchg-reg-with-something operand1 operand2))
             ((register-p operand2)
              (xchg-reg-with-something operand2 operand1))
             (t
              (error "bogus args to XCHG: ~S ~S" operand1 operand2)))))))

(define-instruction lea (segment dst src)
  (:printer reg-reg/mem ((op #b1000110) (width 1)))
  (:emitter
   (aver (dword-reg-p dst))
   (emit-byte segment #b10001101)
   (emit-ea segment src (reg-tn-encoding dst))))

(define-instruction cmpxchg (segment dst src &optional prefix)
  ;; Register/Memory with Register.
  (:printer ext-reg-reg/mem ((op #b1011000)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (aver (register-p src))
   (emit-prefix segment prefix)
   (let ((size (matching-operand-size src dst)))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b00001111)
     (emit-byte segment (if (eq size :byte) #b10110000 #b10110001))
     (emit-ea segment dst (reg-tn-encoding src)))))

(define-instruction cmpxchg8b (segment mem &optional prefix)
  (:printer ext-reg/mem-no-width ((op '(#xC7 1))))
  (:emitter
   (aver (not (register-p mem)))
   (emit-prefix segment prefix)
   (emit-byte segment #x0F)
   (emit-byte segment #xC7)
   (emit-ea segment mem 1)))

(define-instruction rdrand (segment dst)
  (:printer ext-reg/mem-no-width
            ((op '(#xC7 6))))
  (:emitter
   (aver (register-p dst))
   (maybe-emit-operand-size-prefix segment (operand-size dst))
   (emit-byte segment #x0F)
   (emit-byte segment #xC7)
   (emit-ea segment dst 6)))

(define-instruction pause (segment)
  (:printer two-bytes ((op '(#xf3 #x90))))
  (:emitter
   (emit-byte segment #xf3)
   (emit-byte segment #x90)))

;;;; flag control instructions

;;; CLC -- Clear Carry Flag.
(define-instruction clc (segment)
  (:printer byte ((op #b11111000)))
  (:emitter
   (emit-byte segment #b11111000)))

;;; CLD -- Clear Direction Flag.
(define-instruction cld (segment)
  (:printer byte ((op #b11111100)))
  (:emitter
   (emit-byte segment #b11111100)))

;;; CLI -- Clear Iterrupt Enable Flag.
(define-instruction cli (segment)
  (:printer byte ((op #b11111010)))
  (:emitter
   (emit-byte segment #b11111010)))

;;; CMC -- Complement Carry Flag.
(define-instruction cmc (segment)
  (:printer byte ((op #b11110101)))
  (:emitter
   (emit-byte segment #b11110101)))

;;; LAHF -- Load AH into flags.
(define-instruction lahf (segment)
  (:printer byte ((op #b10011111)))
  (:emitter
   (emit-byte segment #b10011111)))

;;; POPF -- Pop flags.
(define-instruction popf (segment)
  (:printer byte ((op #b10011101)))
  (:emitter
   (emit-byte segment #b10011101)))

;;; PUSHF -- push flags.
(define-instruction pushf (segment)
  (:printer byte ((op #b10011100)))
  (:emitter
   (emit-byte segment #b10011100)))

;;; SAHF -- Store AH into flags.
(define-instruction sahf (segment)
  (:printer byte ((op #b10011110)))
  (:emitter
   (emit-byte segment #b10011110)))

;;; STC -- Set Carry Flag.
(define-instruction stc (segment)
  (:printer byte ((op #b11111001)))
  (:emitter
   (emit-byte segment #b11111001)))

;;; STD -- Set Direction Flag.
(define-instruction std (segment)
  (:printer byte ((op #b11111101)))
  (:emitter
   (emit-byte segment #b11111101)))

;;; STI -- Set Interrupt Enable Flag.
(define-instruction sti (segment)
  (:printer byte ((op #b11111011)))
  (:emitter
   (emit-byte segment #b11111011)))

;;;; arithmetic

(defun emit-random-arith-inst (name segment dst src opcode
                               &optional allow-constants)
  (let ((size (matching-operand-size dst src)))
    (maybe-emit-operand-size-prefix segment size)
    (cond
     ((integerp src)
      (cond ((and (not (eq size :byte)) (<= -128 src 127))
             (emit-byte segment #b10000011)
             (emit-ea segment dst opcode allow-constants)
             (emit-byte segment src))
            ((accumulator-p dst)
             (emit-byte segment
                        (dpb opcode
                             (byte 3 3)
                             (if (eq size :byte)
                                 #b00000100
                                 #b00000101)))
             (emit-sized-immediate segment size src))
            (t
             (emit-byte segment (if (eq size :byte) #b10000000 #b10000001))
             (emit-ea segment dst opcode allow-constants)
             (emit-sized-immediate segment size src))))
     ((register-p src)
      (emit-byte segment
                 (dpb opcode
                      (byte 3 3)
                      (if (eq size :byte) #b00000000 #b00000001)))
      (emit-ea segment dst (reg-tn-encoding src) allow-constants))
     ((register-p dst)
      (emit-byte segment
                 (dpb opcode
                      (byte 3 3)
                      (if (eq size :byte) #b00000010 #b00000011)))
      (emit-ea segment src (reg-tn-encoding dst) allow-constants))
     (t
      (error "bogus operands to ~A" name)))))

(eval-when (:compile-toplevel :execute)
  (defun arith-inst-printer-list (subop)
    `((accum-imm ((op ,(dpb subop (byte 3 2) #b0000010))))
      (reg/mem-imm ((op (#b1000000 ,subop))))
      (reg/mem-imm ((op (#b1000001 ,subop))
                    (imm nil :type signed-imm-byte)))
      (reg-reg/mem-dir ((op ,(dpb subop (byte 3 1) #b000000)))))))

(define-instruction add (segment dst src &optional prefix)
  (:printer-list (arith-inst-printer-list #b000))
  (:emitter
   (emit-prefix segment prefix)
   (emit-random-arith-inst "ADD" segment dst src #b000)))

(define-instruction adc (segment dst src)
  (:printer-list (arith-inst-printer-list #b010))
  (:emitter (emit-random-arith-inst "ADC" segment dst src #b010)))

(define-instruction sub (segment dst src &optional prefix)
  (:printer-list (arith-inst-printer-list #b101))
  (:emitter
   (emit-prefix segment prefix)
   (emit-random-arith-inst "SUB" segment dst src #b101)))

(define-instruction sbb (segment dst src)
  (:printer-list (arith-inst-printer-list #b011))
  (:emitter (emit-random-arith-inst "SBB" segment dst src #b011)))

(define-instruction cmp (segment dst src &optional prefix)
  (:printer-list (arith-inst-printer-list #b111))
  (:emitter
   (emit-prefix segment prefix)
   (emit-random-arith-inst "CMP" segment dst src #b111 t)))

(define-instruction inc (segment dst)
  ;; Register.
  (:printer reg-no-width ((op #b01000)))
  ;; Register/Memory
  (:printer reg/mem ((op '(#b1111111 #b000))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((and (not (eq size :byte)) (register-p dst))
            (emit-byte-with-reg segment #b01000 (reg-tn-encoding dst)))
           (t
            (emit-byte segment (if (eq size :byte) #b11111110 #b11111111))
            (emit-ea segment dst #b000))))))

(define-instruction dec (segment dst)
  ;; Register.
  (:printer reg-no-width ((op #b01001)))
  ;; Register/Memory
  (:printer reg/mem ((op '(#b1111111 #b001))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((and (not (eq size :byte)) (register-p dst))
            (emit-byte-with-reg segment #b01001 (reg-tn-encoding dst)))
           (t
            (emit-byte segment (if (eq size :byte) #b11111110 #b11111111))
            (emit-ea segment dst #b001))))))

(define-instruction neg (segment dst)
  (:printer reg/mem ((op '(#b1111011 #b011))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment dst #b011))))

(define-instruction aaa (segment)
  (:printer byte ((op #b00110111)))
  (:emitter
   (emit-byte segment #b00110111)))

(define-instruction aas (segment)
  (:printer byte ((op #b00111111)))
  (:emitter
   (emit-byte segment #b00111111)))

(define-instruction daa (segment)
  (:printer byte ((op #b00100111)))
  (:emitter
   (emit-byte segment #b00100111)))

(define-instruction das (segment)
  (:printer byte ((op #b00101111)))
  (:emitter
   (emit-byte segment #b00101111)))

(define-instruction mul (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b100))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b100))))

(define-instruction imul (segment dst &optional src1 src2)
  (:printer accum-reg/mem ((op '(#b1111011 #b101))))
  (:printer ext-reg-reg/mem ((op #b1010111)))
  (:printer reg-reg/mem ((op #b0110100) (width 1)
                         (imm nil :type 'signed-imm-word))
            '(:name :tab reg ", " reg/mem ", " imm))
  (:printer reg-reg/mem ((op #b0110101) (width 1)
                         (imm nil :type 'signed-imm-byte))
            '(:name :tab reg ", " reg/mem ", " imm))
  (:emitter
   (flet ((r/m-with-immed-to-reg (reg r/m immed)
            (let* ((size (matching-operand-size reg r/m))
                   (sx (and (not (eq size :byte)) (<= -128 immed 127))))
              (maybe-emit-operand-size-prefix segment size)
              (emit-byte segment (if sx #b01101011 #b01101001))
              (emit-ea segment r/m (reg-tn-encoding reg))
              (if sx
                  (emit-byte segment immed)
                  (emit-sized-immediate segment size immed)))))
     (cond (src2
            (r/m-with-immed-to-reg dst src1 src2))
           (src1
            (if (integerp src1)
                (r/m-with-immed-to-reg dst dst src1)
                (let ((size (matching-operand-size dst src1)))
                  (maybe-emit-operand-size-prefix segment size)
                  (emit-byte segment #b00001111)
                  (emit-byte segment #b10101111)
                  (emit-ea segment src1 (reg-tn-encoding dst)))))
           (t
            (let ((size (operand-size dst)))
              (maybe-emit-operand-size-prefix segment size)
              (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
              (emit-ea segment dst #b101)))))))

(define-instruction div (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b110))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b110))))

(define-instruction idiv (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b111))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b111))))

(define-instruction aad (segment)
  (:printer two-bytes ((op '(#b11010101 #b00001010))))
  (:emitter
   (emit-byte segment #b11010101)
   (emit-byte segment #b00001010)))

(define-instruction aam (segment)
  (:printer two-bytes ((op '(#b11010100 #b00001010))))
  (:emitter
   (emit-byte segment #b11010100)
   (emit-byte segment #b00001010)))

(define-instruction bswap (segment dst)
  (:printer ext-reg-no-width ((op #b11001)))
  (:emitter
   (emit-byte segment #x0f)
   (emit-byte-with-reg segment #b11001 (reg-tn-encoding dst))))

;;; CBW -- Convert Byte to Word. AX <- sign_xtnd(AL)
(define-instruction cbw (segment)
  (:printer two-bytes ((op '(#b01100110 #b10011000))))
  (:emitter
   (maybe-emit-operand-size-prefix segment :word)
   (emit-byte segment #b10011000)))

;;; CWDE -- Convert Word To Double Word Extened. EAX <- sign_xtnd(AX)
(define-instruction cwde (segment)
  (:printer byte ((op #b10011000)))
  (:emitter
   (maybe-emit-operand-size-prefix segment :dword)
   (emit-byte segment #b10011000)))

;;; CWD -- Convert Word to Double Word. DX:AX <- sign_xtnd(AX)
(define-instruction cwd (segment)
  (:printer two-bytes ((op '(#b01100110 #b10011001))))
  (:emitter
   (maybe-emit-operand-size-prefix segment :word)
   (emit-byte segment #b10011001)))

;;; CDQ -- Convert Double Word to Quad Word. EDX:EAX <- sign_xtnd(EAX)
(define-instruction cdq (segment)
  (:printer byte ((op #b10011001)))
  (:emitter
   (maybe-emit-operand-size-prefix segment :dword)
   (emit-byte segment #b10011001)))

(define-instruction xadd (segment dst src &optional prefix)
  ;; Register/Memory with Register.
  (:printer ext-reg-reg/mem ((op #b1100000)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (aver (register-p src))
   (emit-prefix segment prefix)
   (let ((size (matching-operand-size src dst)))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b00001111)
     (emit-byte segment (if (eq size :byte) #b11000000 #b11000001))
     (emit-ea segment dst (reg-tn-encoding src)))))


;;;; logic

(defun emit-shift-inst (segment dst amount opcode)
  (let ((size (operand-size dst)))
    (maybe-emit-operand-size-prefix segment size)
    (multiple-value-bind (major-opcode immed)
        (case amount
          (:cl (values #b11010010 nil))
          (1 (values #b11010000 nil))
          (t (values #b11000000 t)))
      (emit-byte segment
                 (if (eq size :byte) major-opcode (logior major-opcode 1)))
      (emit-ea segment dst opcode)
      (when immed
        (emit-byte segment amount)))))

(define-instruction-format
    (shift-inst 16 :include reg/mem
     :default-printer '(:name :tab reg/mem ", " (:if (varying :positive) 'cl 1)))
  (op :fields (list (byte 6 2) (byte 3 11)))
  (varying :field (byte 1 1)))

(macrolet ((define (name subop)
             `(define-instruction ,name (segment dst amount)
                (:printer shift-inst ((op '(#b110100 ,subop)))) ; shift by CL or 1
                (:printer reg/mem-imm ((op '(#b1100000 ,subop))
                                       (imm nil :type 'imm-byte)))
                (:emitter (emit-shift-inst segment dst amount ,subop)))))
  (define rol #b000)
  (define ror #b001)
  (define rcl #b010)
  (define rcr #b011)
  (define shl #b100)
  (define shr #b101)
  (define sar #b111))

(defun emit-double-shift (segment opcode dst src amt)
  (let ((size (matching-operand-size dst src)))
    (when (eq size :byte)
      (error "Double shifts can only be used with words."))
    (maybe-emit-operand-size-prefix segment size)
    (emit-byte segment #b00001111)
    (emit-byte segment (dpb opcode (byte 1 3)
                            (if (eq amt :cl) #b10100101 #b10100100)))
    #+nil
    (emit-ea segment dst src)
    (emit-ea segment dst (reg-tn-encoding src)) ; pw tries this
    (unless (eq amt :cl)
      (emit-byte segment amt))))

(eval-when (:compile-toplevel :execute)
  (defun double-shift-inst-printer-list (op)
    `((ext-reg-reg/mem ((op ,(logior op #b10)) (width 0)
                        (imm nil :type signed-imm-byte))
         (:name :tab reg/mem ", " reg ", " imm))
      (ext-reg-reg/mem ((op ,(logior op #b10)) (width 1))
         (:name :tab reg/mem ", " reg ", " 'cl)))))

(define-instruction shld (segment dst src amt)
  (:declare (type (or (member :cl) (mod 32)) amt))
  (:printer-list (double-shift-inst-printer-list #b1010000))
  (:emitter
   (emit-double-shift segment #b0 dst src amt)))

(define-instruction shrd (segment dst src amt)
  (:declare (type (or (member :cl) (mod 32)) amt))
  (:printer-list (double-shift-inst-printer-list #b1010100))
  (:emitter
   (emit-double-shift segment #b1 dst src amt)))

(define-instruction and (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b100))
  (:emitter
   (emit-random-arith-inst "AND" segment dst src #b100)))

(define-instruction test (segment this that)
  (:printer accum-imm ((op #b1010100)))
  (:printer reg/mem-imm ((op '(#b1111011 #b000))))
  (:printer reg-reg/mem ((op #b1000010)))
  (:emitter
   (let ((size (matching-operand-size this that)))
     (maybe-emit-operand-size-prefix segment size)
     (flet ((test-immed-and-something (immed something)
              (cond ((accumulator-p something)
                     (emit-byte segment
                                (if (eq size :byte) #b10101000 #b10101001))
                     (emit-sized-immediate segment size immed))
                    (t
                     (emit-byte segment
                                (if (eq size :byte) #b11110110 #b11110111))
                     (emit-ea segment something #b000)
                     (emit-sized-immediate segment size immed))))
            (test-reg-and-something (reg something)
              (emit-byte segment (if (eq size :byte) #b10000100 #b10000101))
              (emit-ea segment something (reg-tn-encoding reg))))
       (cond ((integerp that)
              (test-immed-and-something that this))
             ((integerp this)
              (test-immed-and-something this that))
             ((register-p this)
              (test-reg-and-something this that))
             ((register-p that)
              (test-reg-and-something that this))
             (t
              (error "bogus operands for TEST: ~S and ~S" this that)))))))

(define-instruction or (segment dst src &optional prefix)
  (:printer-list
   (arith-inst-printer-list #b001))
  (:emitter
   (emit-prefix segment prefix)
   (emit-random-arith-inst "OR" segment dst src #b001)))

(define-instruction xor (segment dst src &optional prefix)
  (:printer-list
   (arith-inst-printer-list #b110))
  (:emitter
   (emit-prefix segment prefix)
   (emit-random-arith-inst "XOR" segment dst src #b110)))

(define-instruction not (segment dst)
  (:printer reg/mem ((op '(#b1111011 #b010))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment dst #b010))))

;;;; string manipulation

(define-instruction cmps (segment size)
  (:printer string-op ((op #b1010011)))
  (:emitter
   (maybe-emit-operand-size-prefix segment size)
   (emit-byte segment (if (eq size :byte) #b10100110 #b10100111))))

(define-instruction ins (segment acc)
  (:printer string-op ((op #b0110110)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b01101100 #b01101101)))))

(define-instruction lods (segment acc)
  (:printer string-op ((op #b1010110)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b10101100 #b10101101)))))

(define-instruction movs (segment size)
  (:printer string-op ((op #b1010010)))
  (:emitter
   (maybe-emit-operand-size-prefix segment size)
   (emit-byte segment (if (eq size :byte) #b10100100 #b10100101))))

(define-instruction outs (segment acc)
  (:printer string-op ((op #b0110111)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b01101110 #b01101111)))))

(define-instruction scas (segment acc)
  (:printer string-op ((op #b1010111)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b10101110 #b10101111)))))

(define-instruction stos (segment acc)
  (:printer string-op ((op #b1010101)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b10101010 #b10101011)))))

(define-instruction xlat (segment)
  (:printer byte ((op #b11010111)))
  (:emitter
   (emit-byte segment #b11010111)))


;;;; bit manipulation

(define-instruction bsf (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011110) (width 0)))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (when (eq size :byte)
       (error "can't scan bytes: ~S" src))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b00001111)
     (emit-byte segment #b10111100)
     (emit-ea segment src (reg-tn-encoding dst)))))

(define-instruction bsr (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011110) (width 1)))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (when (eq size :byte)
       (error "can't scan bytes: ~S" src))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b00001111)
     (emit-byte segment #b10111101)
     (emit-ea segment src (reg-tn-encoding dst)))))

(defun emit-bit-test-and-mumble (segment src index opcode)
  (let ((size (operand-size src)))
    (when (eq size :byte)
      (error "can't scan bytes: ~S" src))
    (maybe-emit-operand-size-prefix segment size)
    (emit-byte segment #b00001111)
    (cond ((integerp index)
           (emit-byte segment #b10111010)
           (emit-ea segment src opcode)
           (emit-byte segment index))
          (t
           (emit-byte segment (dpb opcode (byte 3 3) #b10000011))
           (emit-ea segment src (reg-tn-encoding index))))))

(eval-when (:compile-toplevel :execute)
  (defun bit-test-inst-printer-list (subop)
    `((ext-reg/mem-no-width+imm8 ((op (#xBA ,subop))))
      (ext-reg-reg/mem-no-width ((op ,(dpb subop (byte 3 3) #b10000011))
                                 (reg/mem nil :type sized-reg/mem))
                                (:name :tab reg/mem ", " reg)))))

(macrolet ((define (inst opcode-extension)
             `(define-instruction ,inst (segment src index)
                (:printer-list (bit-test-inst-printer-list ,opcode-extension))
                (:emitter (emit-bit-test-and-mumble segment src index
                                                    ,opcode-extension)))))
  (define bt  4)
  (define bts 5)
  (define btr 6)
  (define btc 7))


;;;; control transfer

(define-instruction call (segment where)
  (:printer near-jump ((op #b11101000)))
  (:printer reg/mem ((op '(#b1111111 #b010)) (width 1)))
  (:emitter
   (typecase where
     (label
      (emit-byte segment #b11101000)
      (emit-back-patch segment
                       4
                       (lambda (segment posn)
                         (emit-dword segment
                                     (- (label-position where)
                                        (+ posn 4))))))
     (fixup
      (emit-byte segment #b11101000)
      (emit-relative-fixup segment where))
     (t
      (emit-byte segment #b11111111)
      (emit-ea segment where #b010)))))

(defun emit-byte-displacement-backpatch (segment target)
  (emit-back-patch segment
                   1
                   (lambda (segment posn)
                     (let ((disp (- (label-position target) (1+ posn))))
                       (aver (<= -128 disp 127))
                       (emit-byte segment disp)))))

(define-instruction jmp (segment cond &optional where)
  ;; conditional jumps
  (:printer short-cond-jump ((op #b0111)) '('j cc :tab label))
  (:printer near-cond-jump () '('j cc :tab label))
  ;; unconditional jumps
  (:printer short-jump ((op #b1011)))
  (:printer near-jump ((op #b11101001)) )
  (:printer reg/mem ((op '(#b1111111 #b100)) (width 1)))
  (:emitter
   (cond (where
          (emit-chooser
           segment 6 2
           (lambda (segment posn delta-if-after)
             (let ((disp (- (label-position where posn delta-if-after)
                            (+ posn 2))))
               (when (<= -128 disp 127)
                 (emit-byte segment
                            (dpb (conditional-opcode cond)
                                 (byte 4 0)
                                 #b01110000))
                 (emit-byte-displacement-backpatch segment where)
                 t)))
           (lambda (segment posn)
             (let ((disp (- (label-position where) (+ posn 6))))
               (emit-byte segment #b00001111)
               (emit-byte segment
                          (dpb (conditional-opcode cond)
                               (byte 4 0)
                               #b10000000))
               (emit-dword segment disp)))))
         ((label-p (setq where cond))
          (emit-chooser
           segment 5 0
           (lambda (segment posn delta-if-after)
             (let ((disp (- (label-position where posn delta-if-after)
                            (+ posn 2))))
               (when (<= -128 disp 127)
                 (emit-byte segment #b11101011)
                 (emit-byte-displacement-backpatch segment where)
                 t)))
           (lambda (segment posn)
             (let ((disp (- (label-position where) (+ posn 5))))
               (emit-byte segment #b11101001)
               (emit-dword segment disp)))))
         ((fixup-p where)
          (emit-byte segment #b11101001)
          (emit-relative-fixup segment where))
         (t
          (unless (or (ea-p where) (tn-p where))
                  (error "don't know what to do with ~A" where))
          (emit-byte segment #b11111111)
          (emit-ea segment where #b100)))))

(define-instruction jmp-short (segment label)
  (:emitter
   (emit-byte segment #b11101011)
   (emit-byte-displacement-backpatch segment label)))

(define-instruction ret (segment &optional stack-delta)
  (:printer byte ((op #b11000011)))
  (:printer byte ((op #b11000010) (imm nil :type 'imm-word-16))
            '(:name :tab imm))
  (:emitter
   (cond ((and stack-delta (not (zerop stack-delta)))
          (emit-byte segment #b11000010)
          (emit-word segment stack-delta))
         (t
          (emit-byte segment #b11000011)))))

(define-instruction jecxz (segment target)
  (:printer short-jump ((op #b0011)))
  (:emitter
   (emit-byte segment #b11100011)
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loop (segment target)
  (:printer short-jump ((op #b0010)))
  (:emitter
   (emit-byte segment #b11100010)       ; pfw this was 11100011, or jecxz!!!!
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loopz (segment target)
  (:printer short-jump ((op #b0001)))
  (:emitter
   (emit-byte segment #b11100001)
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loopnz (segment target)
  (:printer short-jump ((op #b0000)))
  (:emitter
   (emit-byte segment #b11100000)
   (emit-byte-displacement-backpatch segment target)))

;;;; conditional move
(define-instruction cmov (segment cond dst src)
  (:printer cond-move ())
  (:emitter
   (aver (register-p dst))
   (let ((size (matching-operand-size dst src)))
     (aver (or (eq size :word) (eq size :dword)))
     (maybe-emit-operand-size-prefix segment size))
   (emit-byte segment #b00001111)
   (emit-byte segment (dpb (conditional-opcode cond) (byte 4 0) #b01000000))
   (emit-ea segment src (reg-tn-encoding dst))))

;;;; conditional byte set

(define-instruction set (segment dst cond)
  (:printer cond-set ())
  (:emitter
   (emit-byte segment #b00001111)
   (emit-byte segment (dpb (conditional-opcode cond) (byte 4 0) #b10010000))
   (emit-ea segment dst #b000)))

;;;; enter/leave

(define-instruction enter (segment disp &optional (level 0))
  (:declare (type (unsigned-byte 16) disp)
            (type (unsigned-byte 8) level))
  (:printer enter-format ((op #b11001000)))
  (:emitter
   (emit-byte segment #b11001000)
   (emit-word segment disp)
   (emit-byte segment level)))

(define-instruction leave (segment)
  (:printer byte ((op #b11001001)))
  (:emitter
   (emit-byte segment #b11001001)))

;;;; prefetch
(define-instruction prefetchnta (segment ea)
  (:printer prefetch ((op #b00011000) (reg #b000)))
  (:emitter
   (aver (typep ea 'ea))
   (aver (eq :byte (ea-size ea)))
   (emit-byte segment #b00001111)
   (emit-byte segment #b00011000)
   (emit-ea segment ea #b000)))

(define-instruction prefetcht0 (segment ea)
  (:printer prefetch ((op #b00011000) (reg #b001)))
  (:emitter
   (aver (typep ea 'ea))
   (aver (eq :byte (ea-size ea)))
   (emit-byte segment #b00001111)
   (emit-byte segment #b00011000)
   (emit-ea segment ea #b001)))

(define-instruction prefetcht1 (segment ea)
  (:printer prefetch ((op #b00011000) (reg #b010)))
  (:emitter
   (aver (typep ea 'ea))
   (aver (eq :byte (ea-size ea)))
   (emit-byte segment #b00001111)
   (emit-byte segment #b00011000)
   (emit-ea segment ea #b010)))

(define-instruction prefetcht2 (segment ea)
  (:printer prefetch ((op #b00011000) (reg #b011)))
  (:emitter
   (aver (typep ea 'ea))
   (aver (eq :byte (ea-size ea)))
   (emit-byte segment #b00001111)
   (emit-byte segment #b00011000)
   (emit-ea segment ea #b011)))

;;;; interrupt instructions

(defun snarf-error-junk (sap offset &optional length-only)
  (let* ((length (sap-ref-8 sap offset))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type system-area-pointer sap)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
           (values 0 (1+ length) nil nil))
          (t
           (copy-ub8-from-system-area sap (1+ offset) vector 0 length)
           (collect ((sc-offsets)
                     (lengths))
             (lengths 1)                ; the length byte
             (let* ((index 0)
                    (error-number (read-var-integer vector index)))
               (lengths index)
               (loop
                 (when (>= index length)
                   (return))
                 (let ((old-index index))
                   (sc-offsets (read-var-integer vector index))
                   (lengths (- index old-index))))
               (values error-number
                       (1+ length)
                       (sc-offsets)
                       (lengths))))))))

#|
(defmacro break-cases (breaknum &body cases)
  (let ((bn-temp (gensym)))
    (collect ((clauses))
      (dolist (case cases)
        (clauses `((= ,bn-temp ,(car case)) ,@(cdr case))))
      `(let ((,bn-temp ,breaknum))
         (cond ,@(clauses))))))
|#

(defun break-control (chunk inst stream dstate)
  (declare (ignore inst))
  (flet ((nt (x) (if stream (note x dstate))))
    (case #!-ud2-breakpoints (byte-imm-code chunk dstate)
          #!+ud2-breakpoints (word-imm-code chunk dstate)
      (#.error-trap
       (nt "error trap")
       (handle-break-args #'snarf-error-junk stream dstate))
      (#.cerror-trap
       (nt "cerror trap")
       (handle-break-args #'snarf-error-junk stream dstate))
      (#.breakpoint-trap
       (nt "breakpoint trap"))
      (#.pending-interrupt-trap
       (nt "pending interrupt trap"))
      (#.halt-trap
       (nt "halt trap"))
      (#.fun-end-breakpoint-trap
       (nt "function end breakpoint trap")))))

(define-instruction break (segment code)
  (:declare (type (unsigned-byte 8) code))
  #!-ud2-breakpoints (:printer byte-imm ((op #b11001100))
                               '(:name :tab code) :control #'break-control)
  #!+ud2-breakpoints (:printer word-imm ((op #b0000101100001111))
                               '(:name :tab code) :control #'break-control)
  (:emitter
   #!-ud2-breakpoints (emit-byte segment #b11001100)
   ;; On darwin, trap handling via SIGTRAP is unreliable, therefore we
   ;; throw a sigill with 0x0b0f instead and check for this in the
   ;; SIGILL handler and pass it on to the sigtrap handler if
   ;; appropriate
   #!+ud2-breakpoints (emit-word segment #b0000101100001111)
   (emit-byte segment code)))

(define-instruction int (segment number)
  (:declare (type (unsigned-byte 8) number))
  (:printer byte-imm ((op #b11001101)))
  (:emitter
   (etypecase number
     ((member 3)
      (emit-byte segment #b11001100))
     ((unsigned-byte 8)
      (emit-byte segment #b11001101)
      (emit-byte segment number)))))

(define-instruction into (segment)
  (:printer byte ((op #b11001110)))
  (:emitter
   (emit-byte segment #b11001110)))

(define-instruction bound (segment reg bounds)
  (:emitter
   (let ((size (matching-operand-size reg bounds)))
     (when (eq size :byte)
       (error "can't bounds-test bytes: ~S" reg))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b01100010)
     (emit-ea segment bounds (reg-tn-encoding reg)))))

(define-instruction iret (segment)
  (:printer byte ((op #b11001111)))
  (:emitter
   (emit-byte segment #b11001111)))

;;;; processor control

(define-instruction hlt (segment)
  (:printer byte ((op #b11110100)))
  (:emitter
   (emit-byte segment #b11110100)))

(define-instruction nop (segment)
  (:printer byte ((op #b10010000)))
  (:emitter
   (emit-byte segment #b10010000)))

(define-instruction wait (segment)
  (:printer byte ((op #b10011011)))
  (:emitter
   (emit-byte segment #b10011011)))

;;;; miscellaneous hackery

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

(define-instruction word (segment word)
  (:emitter
   (emit-word segment word)))

(define-instruction dword (segment dword)
  (:emitter
   (emit-dword segment dword)))

(defun emit-header-data (segment type)
  (emit-back-patch segment
                   4
                   (lambda (segment posn)
                     (emit-dword segment
                                 (logior type
                                         (ash (+ posn
                                                 (component-header-length))
                                              (- n-widetag-bits
                                                 word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  (:emitter
   (emit-header-data segment simple-fun-header-widetag)))

(define-instruction lra-header-word (segment)
  (:emitter
   (emit-header-data segment return-pc-header-widetag)))

;;;; fp instructions
;;;;
;;;; FIXME: This section said "added by jrd", which should end up in CREDITS.
;;;;
;;;; Note: We treat the single-precision and double-precision variants
;;;; as separate instructions.

;;; Load single to st(0).
(define-instruction fld (segment source)
  (:printer floating-point ((op '(#b001 #b000))))
  (:emitter
    (emit-byte segment #b11011001)
    (emit-fp-op segment source #b000)))

;;; Load double to st(0).
(define-instruction fldd (segment source)
  (:printer floating-point ((op '(#b101 #b000))))
  (:printer floating-point-fp ((op '(#b001 #b000))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011001)
     (emit-byte segment #b11011101))
    (emit-fp-op segment source #b000)))

;;; Load long to st(0).
(define-instruction fldl (segment source)
  (:printer floating-point ((op '(#b011 #b101))))
  (:emitter
    (emit-byte segment #b11011011)
    (emit-fp-op segment source #b101)))

;;; Store single from st(0).
(define-instruction fst (segment dest)
  (:printer floating-point ((op '(#b001 #b010))))
  (:emitter
    (cond ((fp-reg-tn-p dest)
           (emit-byte segment #b11011101)
           (emit-fp-op segment dest #b010))
          (t
           (emit-byte segment #b11011001)
           (emit-fp-op segment dest #b010)))))

;;; Store double from st(0).
(define-instruction fstd (segment dest)
  (:printer floating-point ((op '(#b101 #b010))))
  (:printer floating-point-fp ((op '(#b101 #b010))))
  (:emitter
   (cond ((fp-reg-tn-p dest)
          (emit-byte segment #b11011101)
          (emit-fp-op segment dest #b010))
         (t
          (emit-byte segment #b11011101)
          (emit-fp-op segment dest #b010)))))

;;; Arithmetic ops are all done with at least one operand at top of
;;; stack. The other operand is is another register or a 32/64 bit
;;; memory loc.

;;; dtc: I've tried to follow the Intel ASM386 conventions, but note
;;; that these conflict with the Gdb conventions for binops. To reduce
;;; the confusion I've added comments showing the mathamatical
;;; operation and the two syntaxes. By the ASM386 convention the
;;; instruction syntax is:
;;;
;;;      Fop Source
;;; or   Fop Destination, Source
;;;
;;; If only one operand is given then it is the source and the
;;; destination is ST(0). There are reversed forms of the fsub and
;;; fdiv instructions inducated by an 'R' suffix.
;;;
;;; The mathematical operation for the non-reverse form is always:
;;;     destination = destination op source
;;;
;;; For the reversed form it is:
;;;     destination = source op destination
;;;
;;; The instructions below only accept one operand at present which is
;;; usually the source. I've hack in extra instructions to implement
;;; the fops with a ST(i) destination, these have a -sti suffix and
;;; the operand is the destination with the source being ST(0).

;;; Add single:
;;;   st(0) = st(0) + memory or st(i).
(define-instruction fadd (segment source)
  (:printer floating-point ((op '(#b000 #b000))))
  (:emitter
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b000)))

;;; Add double:
;;;   st(0) = st(0) + memory or st(i).
(define-instruction faddd (segment source)
  (:printer floating-point ((op '(#b100 #b000))))
  (:printer floating-point-fp ((op '(#b000 #b000))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b000)))

;;; Add double destination st(i):
;;;   st(i) = st(0) + st(i).
(define-instruction fadd-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b000))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b000)))
;;; with pop
(define-instruction faddp-sti (segment destination)
  (:printer floating-point-fp ((op '(#b110 #b000))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011110)
   (emit-fp-op segment destination #b000)))

;;; Subtract single:
;;;   st(0) = st(0) - memory or st(i).
(define-instruction fsub (segment source)
  (:printer floating-point ((op '(#b000 #b100))))
  (:emitter
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b100)))

;;; Subtract single, reverse:
;;;   st(0) = memory or st(i) - st(0).
(define-instruction fsubr (segment source)
  (:printer floating-point ((op '(#b000 #b101))))
  (:emitter
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b101)))

;;; Subtract double:
;;;   st(0) = st(0) - memory or st(i).
(define-instruction fsubd (segment source)
  (:printer floating-point ((op '(#b100 #b100))))
  (:printer floating-point-fp ((op '(#b000 #b100))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b100)))

;;; Subtract double, reverse:
;;;   st(0) = memory or st(i) - st(0).
(define-instruction fsubrd (segment source)
  (:printer floating-point ((op '(#b100 #b101))))
  (:printer floating-point-fp ((op '(#b000 #b101))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b101)))

;;; Subtract double, destination st(i):
;;;   st(i) = st(i) - st(0).
;;;
;;; ASM386 syntax: FSUB ST(i), ST
;;; Gdb    syntax: fsubr %st,%st(i)
(define-instruction fsub-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b101))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b101)))
;;; with a pop
(define-instruction fsubp-sti (segment destination)
  (:printer floating-point-fp ((op '(#b110 #b101))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011110)
   (emit-fp-op segment destination #b101)))

;;; Subtract double, reverse, destination st(i):
;;;   st(i) = st(0) - st(i).
;;;
;;; ASM386 syntax: FSUBR ST(i), ST
;;; Gdb    syntax: fsub %st,%st(i)
(define-instruction fsubr-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b100))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b100)))
;;; with a pop
(define-instruction fsubrp-sti (segment destination)
  (:printer floating-point-fp ((op '(#b110 #b100))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011110)
   (emit-fp-op segment destination #b100)))

;;; Multiply single:
;;;   st(0) = st(0) * memory or st(i).
(define-instruction fmul (segment source)
  (:printer floating-point ((op '(#b000 #b001))))
  (:emitter
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b001)))

;;; Multiply double:
;;;   st(0) = st(0) * memory or st(i).
(define-instruction fmuld (segment source)
  (:printer floating-point ((op '(#b100 #b001))))
  (:printer floating-point-fp ((op '(#b000 #b001))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b001)))

;;; Multiply double, destination st(i):
;;;   st(i) = st(i) * st(0).
(define-instruction fmul-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b001))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b001)))

;;; Divide single:
;;;   st(0) = st(0) / memory or st(i).
(define-instruction fdiv (segment source)
  (:printer floating-point ((op '(#b000 #b110))))
  (:emitter
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b110)))

;;; Divide single, reverse:
;;;   st(0) = memory or st(i) / st(0).
(define-instruction fdivr (segment source)
  (:printer floating-point ((op '(#b000 #b111))))
  (:emitter
    (emit-byte segment #b11011000)
    (emit-fp-op segment source #b111)))

;;; Divide double:
;;;   st(0) = st(0) / memory or st(i).
(define-instruction fdivd (segment source)
  (:printer floating-point ((op '(#b100 #b110))))
  (:printer floating-point-fp ((op '(#b000 #b110))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b110)))

;;; Divide double, reverse:
;;;   st(0) = memory or st(i) / st(0).
(define-instruction fdivrd (segment source)
  (:printer floating-point ((op '(#b100 #b111))))
  (:printer floating-point-fp ((op '(#b000 #b111))))
  (:emitter
   (if (fp-reg-tn-p source)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment source #b111)))

;;; Divide double, destination st(i):
;;;   st(i) = st(i) / st(0).
;;;
;;; ASM386 syntax: FDIV ST(i), ST
;;; Gdb    syntax: fdivr %st,%st(i)
(define-instruction fdiv-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b111))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b111)))

;;; Divide double, reverse, destination st(i):
;;;   st(i) = st(0) / st(i).
;;;
;;; ASM386 syntax: FDIVR ST(i), ST
;;; Gdb    syntax: fdiv %st,%st(i)
(define-instruction fdivr-sti (segment destination)
  (:printer floating-point-fp ((op '(#b100 #b110))))
  (:emitter
   (aver (fp-reg-tn-p destination))
   (emit-byte segment #b11011100)
   (emit-fp-op segment destination #b110)))

;;; Exchange fr0 with fr(n). (There is no double precision variant.)
(define-instruction fxch (segment source)
  (:printer floating-point-fp ((op '(#b001 #b001))))
  (:emitter
    (aver (and (tn-p source)
               (eq (sb-name (sc-sb (tn-sc source))) 'float-registers)))
    (emit-byte segment #b11011001)
    (emit-fp-op segment source #b001)))

;;; Push 32-bit integer to st0.
(define-instruction fild (segment source)
  (:printer floating-point ((op '(#b011 #b000))))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-fp-op segment source #b000)))

;;; Push 64-bit integer to st0.
(define-instruction fildl (segment source)
  (:printer floating-point ((op '(#b111 #b101))))
  (:emitter
   (emit-byte segment #b11011111)
   (emit-fp-op segment source #b101)))

;;; Store 32-bit integer.
(define-instruction fist (segment dest)
  (:printer floating-point ((op '(#b011 #b010))))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-fp-op segment dest #b010)))

;;; Store and pop 32-bit integer.
(define-instruction fistp (segment dest)
  (:printer floating-point ((op '(#b011 #b011))))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-fp-op segment dest #b011)))

;;; Store and pop 64-bit integer.
(define-instruction fistpl (segment dest)
  (:printer floating-point ((op '(#b111 #b111))))
  (:emitter
   (emit-byte segment #b11011111)
   (emit-fp-op segment dest #b111)))

;;; Store single from st(0) and pop.
(define-instruction fstp (segment dest)
  (:printer floating-point ((op '(#b001 #b011))))
  (:emitter
   (cond ((fp-reg-tn-p dest)
          (emit-byte segment #b11011101)
          (emit-fp-op segment dest #b011))
         (t
          (emit-byte segment #b11011001)
          (emit-fp-op segment dest #b011)))))

;;; Store double from st(0) and pop.
(define-instruction fstpd (segment dest)
  (:printer floating-point ((op '(#b101 #b011))))
  (:printer floating-point-fp ((op '(#b101 #b011))))
  (:emitter
   (cond ((fp-reg-tn-p dest)
          (emit-byte segment #b11011101)
          (emit-fp-op segment dest #b011))
         (t
          (emit-byte segment #b11011101)
          (emit-fp-op segment dest #b011)))))

;;; Store long from st(0) and pop.
(define-instruction fstpl (segment dest)
  (:printer floating-point ((op '(#b011 #b111))))
  (:emitter
    (emit-byte segment #b11011011)
    (emit-fp-op segment dest #b111)))

;;; Decrement stack-top pointer.
(define-instruction fdecstp (segment)
  (:printer floating-point-no ((op #b10110)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110110)))

;;; Increment stack-top pointer.
(define-instruction fincstp (segment)
  (:printer floating-point-no ((op #b10111)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110111)))

;;; Free fp register.
(define-instruction ffree (segment dest)
  (:printer floating-point-fp ((op '(#b101 #b000))))
  (:emitter
   (emit-byte segment #b11011101)
   (emit-fp-op segment dest #b000)))

(define-instruction fabs (segment)
  (:printer floating-point-no ((op #b00001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100001)))

(define-instruction fchs (segment)
  (:printer floating-point-no ((op #b00000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100000)))

(define-instruction frndint(segment)
  (:printer floating-point-no ((op #b11100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111100)))

;;; Initialize NPX.
(define-instruction fninit(segment)
  (:printer floating-point-5 ((op #b00011)))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-byte segment #b11100011)))

;;; Store Status Word to AX.
(define-instruction fnstsw(segment)
  (:printer floating-point-st ((op #b00000)))
  (:emitter
   (emit-byte segment #b11011111)
   (emit-byte segment #b11100000)))

;;; Load Control Word.
;;;
;;; src must be a memory location
(define-instruction fldcw(segment src)
  (:printer floating-point ((op '(#b001 #b101))))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-fp-op segment src #b101)))

;;; Store Control Word.
(define-instruction fnstcw(segment dst)
  (:printer floating-point ((op '(#b001 #b111))))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-fp-op segment dst #b111)))

;;; Store FP Environment.
(define-instruction fstenv(segment dst)
  (:printer floating-point ((op '(#b001 #b110))))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-fp-op segment dst #b110)))

;;; Restore FP Environment.
(define-instruction fldenv(segment src)
  (:printer floating-point ((op '(#b001 #b100))))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-fp-op segment src #b100)))

;;; Save FP State.
(define-instruction fsave(segment dst)
  (:printer floating-point ((op '(#b101 #b110))))
  (:emitter
   (emit-byte segment #b11011101)
   (emit-fp-op segment dst #b110)))

;;; Restore FP State.
(define-instruction frstor(segment src)
  (:printer floating-point ((op '(#b101 #b100))))
  (:emitter
   (emit-byte segment #b11011101)
   (emit-fp-op segment src #b100)))

;;; Clear exceptions.
(define-instruction fnclex(segment)
  (:printer floating-point-5 ((op #b00010)))
  (:emitter
   (emit-byte segment #b11011011)
   (emit-byte segment #b11100010)))

;;; comparison
(define-instruction fcom (segment src)
  (:printer floating-point ((op '(#b000 #b010))))
  (:emitter
   (emit-byte segment #b11011000)
   (emit-fp-op segment src #b010)))

(define-instruction fcomd (segment src)
  (:printer floating-point ((op '(#b100 #b010))))
  (:printer floating-point-fp ((op '(#b000 #b010))))
  (:emitter
   (if (fp-reg-tn-p src)
       (emit-byte segment #b11011000)
     (emit-byte segment #b11011100))
   (emit-fp-op segment src #b010)))

;;; Compare ST1 to ST0, popping the stack twice.
(define-instruction fcompp (segment)
  (:printer floating-point-3 ((op '(#b110 #b011001))))
  (:emitter
   (emit-byte segment #b11011110)
   (emit-byte segment #b11011001)))

;;; unordered comparison
(define-instruction fucom (segment src)
  (:printer floating-point-fp ((op '(#b101 #b100))))
  (:emitter
   (aver (fp-reg-tn-p src))
   (emit-byte segment #b11011101)
   (emit-fp-op segment src #b100)))

(define-instruction ftst (segment)
  (:printer floating-point-no ((op #b00100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100100)))

;;;; 80387 specials

(define-instruction fsqrt(segment)
  (:printer floating-point-no ((op #b11010)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111010)))

(define-instruction fscale(segment)
  (:printer floating-point-no ((op #b11101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111101)))

(define-instruction fxtract(segment)
  (:printer floating-point-no ((op #b10100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110100)))

(define-instruction fsin(segment)
  (:printer floating-point-no ((op #b11110)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111110)))

(define-instruction fcos(segment)
  (:printer floating-point-no ((op #b11111)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111111)))

(define-instruction fprem1(segment)
  (:printer floating-point-no ((op #b10101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110101)))

(define-instruction fprem(segment)
  (:printer floating-point-no ((op #b11000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111000)))

(define-instruction fxam (segment)
  (:printer floating-point-no ((op #b00101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11100101)))

;;; These do push/pop to stack and need special handling
;;; in any VOPs that use them. See the book.

;;; st0 <- st1*log2(st0)
(define-instruction fyl2x(segment)      ; pops stack
  (:printer floating-point-no ((op #b10001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110001)))

(define-instruction fyl2xp1(segment)
  (:printer floating-point-no ((op #b11001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11111001)))

(define-instruction f2xm1(segment)
  (:printer floating-point-no ((op #b10000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110000)))

(define-instruction fptan(segment)      ; st(0) <- 1; st(1) <- tan
  (:printer floating-point-no ((op #b10010)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110010)))

(define-instruction fpatan(segment)     ; POPS STACK
  (:printer floating-point-no ((op #b10011)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11110011)))

;;;; loading constants

(define-instruction fldz(segment)
  (:printer floating-point-no ((op #b01110)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101110)))

(define-instruction fld1(segment)
  (:printer floating-point-no ((op #b01000)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101000)))

(define-instruction fldpi(segment)
  (:printer floating-point-no ((op #b01011)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101011)))

(define-instruction fldl2t(segment)
  (:printer floating-point-no ((op #b01001)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101001)))

(define-instruction fldl2e(segment)
  (:printer floating-point-no ((op #b01010)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101010)))

(define-instruction fldlg2(segment)
  (:printer floating-point-no ((op #b01100)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101100)))

(define-instruction fldln2(segment)
  (:printer floating-point-no ((op #b01101)))
  (:emitter
   (emit-byte segment #b11011001)
   (emit-byte segment #b11101101)))

;;;; Miscellany

(define-instruction cpuid (segment)
  (:printer two-bytes ((op '(#b00001111 #b10100010))))
  (:emitter
   (emit-byte segment #b00001111)
   (emit-byte segment #b10100010)))

(define-instruction rdtsc (segment)
  (:printer two-bytes ((op '(#b00001111 #b00110001))))
  (:emitter
   (emit-byte segment #b00001111)
   (emit-byte segment #b00110001)))

;;;; Late VM definitions
(defun canonicalize-inline-constant (constant)
  (let ((first (car constant)))
    (typecase first
      (single-float (setf constant (list :single-float first)))
      (double-float (setf constant (list :double-float first)))))
  (destructuring-bind (type value) constant
    (ecase type
      ((:byte :word :dword)
         (aver (integerp value))
         (cons type value))
      ((:base-char)
         #!+sb-unicode (aver (typep value 'base-char))
         (cons :byte (char-code value)))
      ((:character)
         (aver (characterp value))
         (cons :dword (char-code value)))
      ((:single-float)
         (aver (typep value 'single-float))
         (cons :dword (ldb (byte 32 0) (single-float-bits value))))
      ((:double-float-bits)
         (aver (integerp value))
         (cons :double-float (ldb (byte 64 0) value)))
      ((:double-float)
         (aver (typep value 'double-float))
         (cons :double-float
               (ldb (byte 64 0) (logior (ash (double-float-high-bits value) 32)
                                        (double-float-low-bits value))))))))

(defun inline-constant-value (constant)
  (let ((label (gen-label))
        (size  (ecase (car constant)
                 ((:byte :word :dword) (car constant))
                 (:double-float :dword))))
    (values label (make-ea size
                           :disp (make-fixup nil :code-object label)))))

(defun emit-constant-segment-header (segment constants optimize)
  (declare (ignore segment constants))
  (loop repeat (if optimize 64 16) do (inst byte #x90)))

(defun size-nbyte (size)
  (ecase size
    (:byte  1)
    (:word  2)
    (:dword 4)
    (:double-float 8)))

(defun sort-inline-constants (constants)
  (stable-sort constants #'> :key (lambda (constant)
                                    (size-nbyte (caar constant)))))

(defun emit-inline-constant (constant label)
  (let ((size (size-nbyte (car constant))))
    (emit-alignment (integer-length (1- size)))
    (emit-label label)
    (let ((val (cdr constant)))
      (loop repeat size
            do (inst byte (ldb (byte 8 0) val))
               (setf val (ash val -8))))))
