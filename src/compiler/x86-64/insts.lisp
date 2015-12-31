;;;; that part of the description of the x86-64 instruction set
;;;; which can live on the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!X86-64-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from this package into SB-VM
  (import '(*condition-name-vec* conditional-opcode
            register-p xmm-register-p ; FIXME: rename REGISTER-P to GPR-P
            make-ea ea-disp) 'sb!vm)
  ;; Imports from SB-VM into this package
  (import '(sb!vm::*byte-sc-names* sb!vm::*word-sc-names*
            sb!vm::*dword-sc-names* sb!vm::*qword-sc-names*
            sb!vm::frame-byte-offset
            sb!vm::registers sb!vm::float-registers sb!vm::stack))) ; SB names

(!begin-instruction-definitions)

;;; Note: In CMU CL, this used to be a call to SET-DISASSEM-PARAMS.
(setf *disassem-inst-alignment-bytes* 1)

;;; This type is used mostly in disassembly and represents legacy
;;; registers only. R8-R15 are handled separately.
(deftype reg () '(unsigned-byte 3))

;;; This includes legacy registers and R8-R15.
(deftype full-reg () '(unsigned-byte 4))

;;; The XMM registers XMM0 - XMM15.
(deftype xmmreg () '(unsigned-byte 4))

;;; Default word size for the chip: if the operand size /= :dword
;;; we need to output #x66 (or REX) prefix
(def!constant +default-operand-size+ :dword)

;;; The default address size for the chip. It could be overwritten
;;; to :dword with a #x67 prefix, but this is never needed by SBCL
;;; and thus not supported by this assembler/disassembler.
(def!constant +default-address-size+ :qword)

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

(defun offset-next (value dstate)
  (declare (type integer value)
           (type disassem-state dstate))
  (+ (dstate-next-addr dstate) value))

(defparameter *byte-reg-names*
  #(al cl dl bl spl bpl sil dil r8b r9b r10b r11b r12b r13b r14b r15b))
(defparameter *high-byte-reg-names*
  #(ah ch dh bh))
(defparameter *word-reg-names*
  #(ax cx dx bx sp bp si di r8w r9w r10w r11w r12w r13w r14w r15w))
(defparameter *dword-reg-names*
  #(eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d))
(defparameter *qword-reg-names*
  #(rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15))

;;; The printers for registers, memory references and immediates need to
;;; take into account the width bit in the instruction, whether a #x66
;;; or a REX prefix was issued, and the contents of the REX prefix.
;;; This is implemented using prefilters to put flags into the slot
;;; INST-PROPERTIES of the DSTATE.  These flags are the following
;;; symbols:
;;;
;;; OPERAND-SIZE-8   The width bit was zero
;;; OPERAND-SIZE-16  The "operand size override" prefix (#x66) was found
;;; REX              A REX prefix was found
;;; REX-W            A REX prefix with the "operand width" bit set was
;;;                  found
;;; REX-R            A REX prefix with the "register" bit set was found
;;; REX-X            A REX prefix with the "index" bit set was found
;;; REX-B            A REX prefix with the "base" bit set was found

;;; Return the operand size depending on the prefixes and width bit as
;;; stored in DSTATE.
(defun inst-operand-size (dstate)
  (declare (type disassem-state dstate))
  (cond ((dstate-get-inst-prop dstate 'operand-size-8) :byte)
        ((dstate-get-inst-prop dstate 'rex-w) :qword)
        ((dstate-get-inst-prop dstate 'operand-size-16) :word)
        (t +default-operand-size+)))

;;; The same as INST-OPERAND-SIZE, but for those instructions (e.g.
;;; PUSH, JMP) that have a default operand size of :qword. It can only
;;; be overwritten to :word.
(defun inst-operand-size-default-qword (dstate)
  (declare (type disassem-state dstate))
  (if (dstate-get-inst-prop dstate 'operand-size-16) :word :qword))

;;; Print to STREAM the name of the general-purpose register encoded by
;;; VALUE and of size WIDTH. For robustness, the high byte registers
;;; (AH, BH, CH, DH) are correctly detected, too, although the compiler
;;; does not use them.
(defun print-reg-with-width (value width stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (princ (if (and (eq width :byte)
                  (<= 4 value 7)
                  (not (dstate-get-inst-prop dstate 'rex)))
             (aref *high-byte-reg-names* (- value 4))
             (aref (ecase width
                     (:byte *byte-reg-names*)
                     (:word *word-reg-names*)
                     (:dword *dword-reg-names*)
                     (:qword *qword-reg-names*))
                   value))
         stream)
  ;; XXX plus should do some source-var notes
  )

(defun print-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value
                        (inst-operand-size dstate)
                        stream
                        dstate))

(defun print-reg-default-qword (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value
                        (inst-operand-size-default-qword dstate)
                        stream
                        dstate))

(defun print-byte-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value :byte stream dstate))

(defun print-addr-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg-with-width value +default-address-size+ stream dstate))

;;; Print a register or a memory reference of the given WIDTH.
;;; If SIZED-P is true, add an explicit size indicator for memory
;;; references.
(defun print-reg/mem-with-width (value width sized-p stream dstate)
  (declare (type (or list full-reg) value)
           (type (member :byte :word :dword :qword) width)
           (type boolean sized-p)
           (type stream stream)
           (type disassem-state dstate))
  (if (typep value 'full-reg)
      (print-reg-with-width value width stream dstate)
      (print-mem-ref (if sized-p :sized-ref :ref) value width stream dstate)))

;;; Print a register or a memory reference. The width is determined by
;;; calling INST-OPERAND-SIZE.
(defun print-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size dstate) nil stream dstate))

;; Same as print-reg/mem, but prints an explicit size indicator for
;; memory references.
(defun print-sized-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size dstate) t stream dstate))

;;; Same as print-sized-reg/mem, but with a default operand size of
;;; :qword.
(defun print-sized-reg/mem-default-qword (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size-default-qword dstate) t stream dstate))

(defun print-sized-byte-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width value :byte t stream dstate))

(defun print-sized-word-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width value :word t stream dstate))

(defun print-sized-dword-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type disassem-state dstate))
  (print-reg/mem-with-width value :dword t stream dstate))

(defun print-label (value stream dstate)
  (declare (ignore dstate))
  (princ16 value stream))

(defun print-xmmreg (value stream dstate)
  (declare (type xmmreg value) (type stream stream) (ignore dstate))
  (format stream "XMM~d" value))

(defun print-xmmreg/mem (value stream dstate)
  (declare (type (or list xmmreg) value)
           (type stream stream)
           (type disassem-state dstate))
  (if (typep value 'xmmreg)
      (print-xmmreg value stream dstate)
      (print-mem-ref :ref value nil stream dstate)))

;;; This prefilter is used solely for its side effects, namely to put
;;; the bits found in the REX prefix into the DSTATE for use by other
;;; prefilters and by printers.
(defun prefilter-wrxb (value dstate)
  (declare (type (unsigned-byte 4) value)
           (type disassem-state dstate))
  (dstate-put-inst-prop dstate 'rex)
  (when (plusp (logand value #b1000))
    (dstate-put-inst-prop dstate 'rex-w))
  (when (plusp (logand value #b0100))
    (dstate-put-inst-prop dstate 'rex-r))
  (when (plusp (logand value #b0010))
    (dstate-put-inst-prop dstate 'rex-x))
  (when (plusp (logand value #b0001))
    (dstate-put-inst-prop dstate 'rex-b))
  value)

;;; The two following prefilters are used instead of prefilter-wrxb when
;;; the bits of the REX prefix need to be treated individually. They are
;;; always used together, so only the first one sets the REX property.
(defun prefilter-rex-w (value dstate)
  (declare (type bit value) (type disassem-state dstate))
  (dstate-put-inst-prop dstate 'rex)
  (when (plusp value)
    (dstate-put-inst-prop dstate 'rex-w)))

(defun prefilter-rex-b (value dstate)
  (declare (type bit value) (type disassem-state dstate))
  (when (plusp value)
    (dstate-put-inst-prop dstate 'rex-b)))

;;; This prefilter is used solely for its side effect, namely to put
;;; the property OPERAND-SIZE-8 into the DSTATE if VALUE is 0.
(defun prefilter-width (value dstate)
  (declare (type bit value) (type disassem-state dstate))
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

;;; A register field that can be extended by REX.R.
(defun prefilter-reg-r (value dstate)
  (declare (type reg value) (type disassem-state dstate))
  (if (dstate-get-inst-prop dstate 'rex-r) (+ value 8) value))

;;; A register field that can be extended by REX.B.
(defun prefilter-reg-b (value dstate)
  (declare (type reg value) (type disassem-state dstate))
  (if (dstate-get-inst-prop dstate 'rex-b) (+ value 8) value))

;;; Returns either an integer, meaning a register, or a list of
;;; (BASE-REG OFFSET INDEX-REG INDEX-SCALE), where any component
;;; may be missing or nil to indicate that it's not used or has the
;;; obvious default value (e.g., 1 for the index-scale). VALUE is a list
;;; of the mod and r/m field of the ModRM byte of the instruction.
;;; Depending on VALUE a SIB byte and/or an offset may be read. The
;;; REX.B bit from DSTATE is used to extend the sole register or the
;;; BASE-REG to a full register, the REX.X bit does the same for the
;;; INDEX-REG.
(defun prefilter-reg/mem (value dstate)
  (declare (type list value)
           (type disassem-state dstate))
  (flet ((extend (bit-name reg)
           (logior (if (dstate-get-inst-prop dstate bit-name) 8 0)
                   reg)))
    (declare (inline extend))
    (let* ((mod (the (unsigned-byte 2) (first value)))
           (r/m (the (unsigned-byte 3) (second value)))
           (full-reg (extend 'rex-b r/m)))
      (cond ((= mod #b11)
             ;; registers
             full-reg)
            ((= r/m #b100) ; SIB byte - rex.b is "don't care"
             (let* ((sib (the (unsigned-byte 8)
                              (read-suffix 8 dstate)))
                    (base-reg (ldb (byte 3 0) sib))
                    (index-reg (extend 'rex-x (ldb (byte 3 3) sib)))
                    (offset
                         (case mod
                               (#b00
                                (if (= base-reg #b101)
                                    (read-signed-suffix 32 dstate)
                                  nil))
                               (#b01
                                (read-signed-suffix 8 dstate))
                               (#b10
                                (read-signed-suffix 32 dstate)))))
               (list (unless (and (= mod #b00) (= base-reg #b101))
                       (extend 'rex-b base-reg))
                     offset
                     (unless (= index-reg #b100) index-reg) ; index can't be RSP
                     (ash 1 (ldb (byte 2 6) sib)))))
            ;; rex.b is not decoded in determining RIP-relative mode
            ((and (= mod #b00) (= r/m #b101))
             (list 'rip (read-signed-suffix 32 dstate)))
            ((= mod #b00)
             (list full-reg))
            ((= mod #b01)
             (list full-reg (read-signed-suffix 8 dstate)))
            (t                            ; (= mod #b10)
             (list full-reg (read-signed-suffix 32 dstate)))))))

(defun read-address (value dstate)
  (declare (ignore value))              ; always nil anyway
  (read-suffix (width-bits (inst-operand-size dstate)) dstate))

(defun width-bits (width)
  (ecase width
    (:byte 8)
    (:word 16)
    (:dword 32)
    (:qword 64)))

(defun print-imm/asm-routine (value stream dstate)
  (maybe-note-assembler-routine value nil dstate)
  (maybe-note-static-symbol value dstate)
  (princ value stream))
) ; EVAL-WHEN

;;;; disassembler argument types

;;; Used to capture the lower four bits of the REX prefix all at once ...
(define-arg-type wrxb :prefilter #'prefilter-wrxb)
;;; ... or individually (not needed for REX.R and REX.X).
(define-arg-type rex-w :prefilter #'prefilter-rex-w)
(define-arg-type rex-b :prefilter #'prefilter-rex-b)

(define-arg-type width
  :prefilter #'prefilter-width
  :printer (lambda (value stream dstate)
             (declare (ignore value))
             (princ (schar (symbol-name (inst-operand-size dstate)) 0)
                    stream)))

;;; Used to capture the effect of the #x66 operand size override prefix.
(define-arg-type x66 :prefilter #'prefilter-x66)

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

(define-arg-type reg
  :prefilter #'prefilter-reg-r
  :printer #'print-reg)

(define-arg-type reg-b
  :prefilter #'prefilter-reg-b
  :printer #'print-reg)

(define-arg-type reg-b-default-qword
  :prefilter #'prefilter-reg-b
  :printer #'print-reg-default-qword)

(define-arg-type imm-addr
  :prefilter #'read-address
  :printer #'print-label)

;;; Normally, immediate values for an operand size of :qword are of size
;;; :dword and are sign-extended to 64 bits. For an exception, see the
;;; argument type definition of SIGNED-IMM-DATA-UPTO-QWORD below.
(define-arg-type signed-imm-data
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (let ((width (width-bits (inst-operand-size dstate))))
                 (when (= width 64)
                   (setf width 32))
                 (read-signed-suffix width dstate))))

(define-arg-type signed-imm-data/asm-routine
  :type 'signed-imm-data
  :printer #'print-imm/asm-routine)

;;; Used by the variant of the MOV instruction with opcode B8 which can
;;; move immediates of all sizes (i.e. including :qword) into a
;;; register.
(define-arg-type signed-imm-data-upto-qword
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (read-signed-suffix
                (width-bits (inst-operand-size dstate))
                dstate)))

(define-arg-type signed-imm-data-upto-qword/asm-routine
  :type 'signed-imm-data-upto-qword
  :printer #'print-imm/asm-routine)


;;; Used by those instructions that have a default operand size of
;;; :qword. Nevertheless the immediate is at most of size :dword.
;;; The only instruction of this kind having a variant with an immediate
;;; argument is PUSH.
(define-arg-type signed-imm-data-default-qword
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (let ((width (width-bits
                             (inst-operand-size-default-qword dstate))))
                 (when (= width 64)
                   (setf width 32))
                 (read-signed-suffix width dstate))))

(define-arg-type signed-imm-byte
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (read-signed-suffix 8 dstate)))

(define-arg-type imm-byte
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (read-suffix 8 dstate)))

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

;;; Arguments of type reg/mem with a fixed size.
(define-arg-type sized-byte-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-byte-reg/mem)
(define-arg-type sized-word-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-word-reg/mem)
(define-arg-type sized-dword-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-dword-reg/mem)

;;; Same as sized-reg/mem, but with a default operand size of :qword.
(define-arg-type sized-reg/mem-default-qword
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-reg/mem-default-qword)

;;; XMM registers
(define-arg-type xmmreg
  :prefilter #'prefilter-reg-r
  :printer #'print-xmmreg)

(define-arg-type xmmreg-b
  :prefilter #'prefilter-reg-b
  :printer #'print-xmmreg)

(define-arg-type xmmreg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-xmmreg/mem)


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

;;; SSE shuffle patterns. The names end in the number of bits of the
;;; immediate byte that are used to encode the pattern and the radix
;;; in which to print the value.
(macrolet ((define-sse-shuffle-arg-type (name format-string)
               `(define-arg-type ,name
                  :type 'imm-byte
                  :printer (lambda (value stream dstate)
                             (declare (type (unsigned-byte 8) value)
                                      (type stream stream)
                                      (ignore dstate))
                             (format stream ,format-string value)))))
  (define-sse-shuffle-arg-type sse-shuffle-pattern-2-2 "#b~2,'0B")
  (define-sse-shuffle-arg-type sse-shuffle-pattern-8-4 "#4r~4,4,'0R"))

;;; Set assembler parameters. (In CMU CL, this was done with
;;; a call to a macro DEF-ASSEMBLER-PARAMS.)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf sb!assem:*assem-scheduler-p* nil))

(define-arg-type condition-code
  :printer *condition-name-vec*)

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

(define-instruction-format (two-bytes 16
                                        :default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8))))

(define-instruction-format (three-bytes 24
                                        :default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8) (byte 8 16))))

;;; Prefix instructions

(define-instruction-format (rex 8)
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb))

(define-instruction-format (x66 8)
  (x66     :field (byte 8 0)    :type 'x66      :value #x66))

;;; A one-byte instruction with a #x66 prefix, used to indicate an
;;; operand size of :word.
(define-instruction-format (x66-byte 16
                                        :default-printer '(:name))
  (x66   :field (byte 8 0) :value #x66)
  (op    :field (byte 8 8)))

;;; A one-byte instruction with a REX prefix, used to indicate an
;;; operand size of :qword. REX.W must be 1, the other three bits are
;;; ignored.
(define-instruction-format (rex-byte 16
                                        :default-printer '(:name))
  (rex   :field (byte 5 3) :value #b01001)
  (op    :field (byte 8 8)))

(define-instruction-format (simple 8)
  (op    :field (byte 7 1))
  (width :field (byte 1 0) :type 'width)
  ;; optional fields
  (accum :type 'accum)
  (imm))

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
  (imm :type 'signed-imm-data))

(define-instruction-format (reg-no-width 8
                                     :default-printer '(:name :tab reg))
  (op    :field (byte 5 3))
  (reg   :field (byte 3 0) :type 'reg-b)
  ;; optional fields
  (accum :type 'accum)
  (imm))

;;; This is reg-no-width with a mandatory REX prefix and accum field,
;;; with the ability to match against REX.W and REX.B individually.
;;; REX.R and REX.X are ignored.
(define-instruction-format (rex-accum-reg 16
                                       :default-printer
                                       '(:name :tab accum ", " reg))
  (rex   :field (byte 4 4) :value #b0100)
  (rex-w :field (byte 1 3) :type 'rex-w)
  (rex-b :field (byte 1 0) :type 'rex-b)
  (op    :field (byte 5 11))
  (reg   :field (byte 3 8) :type 'reg-b)
  (accum :type 'accum))

;;; Same as reg-no-width, but with a default operand size of :qword.
(define-instruction-format (reg-no-width-default-qword 8
                                        :include reg-no-width
                                        :default-printer '(:name :tab reg))
  (reg   :type 'reg-b-default-qword))

;;; Adds a width field to reg-no-width. Note that we can't use
;;; :INCLUDE REG-NO-WIDTH here to save typing because that would put
;;; the WIDTH field last, but the prefilter for WIDTH must run before
;;; the one for IMM to be able to determine the correct size of IMM.
(define-instruction-format (reg 8
                                        :default-printer '(:name :tab reg))
  (op    :field (byte 4 4))
  (width :field (byte 1 3) :type 'width)
  (reg   :field (byte 3 0) :type 'reg-b)
  ;; optional fields
  (accum :type 'accum)
  (imm))

(define-instruction-format (rex-reg 16
                                        :default-printer '(:name :tab reg))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (width   :field (byte 1 11)   :type 'width)
  (op      :field (byte 4 12))
  (reg     :field (byte 3 8)    :type 'reg-b)
  ;; optional fields
  (accum   :type 'accum)
  (imm))

(define-instruction-format (reg-reg/mem 16
                                        :default-printer
                                        `(:name :tab reg ", " reg/mem))
  (op      :field (byte 7 1))
  (width   :field (byte 1 0)    :type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
           :type 'reg/mem :reader reg-r/m-inst-r/m-arg)
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

;;; Same as reg-reg/mem, but uses the reg field as a second op code.
(define-instruction-format (reg/mem 16
                                        :default-printer '(:name :tab reg/mem))
  (op      :fields (list (byte 7 1) (byte 3 11)))
  (width   :field (byte 1 0)    :type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
                                :type 'sized-reg/mem)
  ;; optional fields
  (imm))

;;; Same as reg/mem, but without a width field and with a default
;;; operand size of :qword.
(define-instruction-format (reg/mem-default-qword 16
                                        :default-printer '(:name :tab reg/mem))
  (op      :fields (list (byte 8 0) (byte 3 11)))
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
                                :type 'sized-reg/mem-default-qword))

;;; Same as reg/mem, but with the immediate value occurring by default,
;;; and with an appropiate printer.
(define-instruction-format (reg/mem-imm 16
                                        :include reg/mem
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (reg/mem :type 'sized-reg/mem)
  (imm     :type 'signed-imm-data))

(define-instruction-format (reg/mem-imm/asm-routine 16
                                        :include reg/mem-imm
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (reg/mem :type 'sized-reg/mem)
  (imm     :type 'signed-imm-data/asm-routine))

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
  (reg   :field (byte 3 8) :type 'reg-b))

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
  (imm :type 'signed-imm-data))

(define-instruction-format (ext-reg/mem-no-width+imm8 24
                                        :include ext-reg/mem-no-width
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (imm :type 'imm-byte))

;;;; XMM instructions

;;; All XMM instructions use an extended opcode (#x0F as the first
;;; opcode byte). Therefore in the following "EXT" in the name of the
;;; instruction formats refers to the formats that have an additional
;;; prefix (#x66, #xF2 or #xF3).

;;; Instructions having an XMM register as the destination operand
;;; and an XMM register or a memory location as the source operand.
;;; The size of the operands is implicitly given by the instruction.
(define-instruction-format (xmm-xmm/mem 24
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (x0f     :field (byte 8 0)    :value #x0f)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 19)   :type 'xmmreg)
  ;; optional fields
  (imm))

(define-instruction-format (ext-xmm-xmm/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg)
  (imm))

(define-instruction-format (ext-rex-xmm-xmm/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg)
  (imm))

(define-instruction-format (ext-2byte-xmm-xmm/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op1     :field (byte 8 16))          ; #x38 or #x3a
  (op2     :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg))

(define-instruction-format (ext-rex-2byte-xmm-xmm/mem 48
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op1     :field (byte 8 24))          ; #x38 or #x3a
  (op2     :field (byte 8 32))
  (reg/mem :fields (list (byte 2 46) (byte 3 40))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 43)   :type 'xmmreg))

;;; Same as xmm-xmm/mem etc., but with direction bit.

(define-instruction-format (ext-xmm-xmm/mem-dir 32
                                        :include ext-xmm-xmm/mem
                                        :default-printer
                                        `(:name
                                          :tab
                                          ,(swap-if 'dir 'reg ", " 'reg/mem)))
  (op      :field (byte 7 17))
  (dir     :field (byte 1 16)))

(define-instruction-format (ext-rex-xmm-xmm/mem-dir 40
                                        :include ext-rex-xmm-xmm/mem
                                        :default-printer
                                        `(:name
                                          :tab
                                          ,(swap-if 'dir 'reg ", " 'reg/mem)))
  (op      :field (byte 7 25))
  (dir     :field (byte 1 24)))

;;; Instructions having an XMM register as one operand
;;; and a constant (unsigned) byte as the other.

(define-instruction-format (ext-xmm-imm 32
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)   :value #x0f)
  (op      :field (byte 8 16))
  (/i      :field (byte 3 27))
  (b11     :field (byte 2 30) :value #b11)
  (reg/mem :field (byte 3 24)
           :type 'xmmreg-b)
  (imm     :type 'imm-byte))

(define-instruction-format (ext-rex-xmm-imm 40
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op      :field (byte 8 24))
  (/i      :field (byte 3 35))
  (b11     :field (byte 2 38) :value #b11)
  (reg/mem :field (byte 3 32)
           :type 'xmmreg-b)
  (imm     :type 'imm-byte))

;;; Instructions having an XMM register as one operand and a general-
;;; -purpose register or a memory location as the other operand.

(define-instruction-format (xmm-reg/mem 24
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (x0f     :field (byte 8 0)    :value #x0f)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 19)   :type 'xmmreg)
  (imm))

(define-instruction-format (ext-xmm-reg/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'sized-reg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg)
  (imm))

(define-instruction-format (ext-rex-xmm-reg/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'sized-reg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg)
  (imm))

(define-instruction-format (ext-2byte-xmm-reg/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op1     :field (byte 8 16))
  (op2     :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32)) :type 'sized-reg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg)
  (imm))

;;; Instructions having a general-purpose register as one operand and an
;;; XMM register or a memory location as the other operand.

(define-instruction-format (reg-xmm/mem 24
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (x0f     :field (byte 8 0)    :value #x0f)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 19)   :type 'reg))

(define-instruction-format (ext-reg-xmm/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'reg))

(define-instruction-format (ext-rex-reg-xmm/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 35)   :type 'reg))

;;; Instructions having a general-purpose register or a memory location
;;; as one operand and an a XMM register as the other operand.

(define-instruction-format (ext-reg/mem-xmm 32
                                        :default-printer
                                        '(:name :tab reg/mem ", " reg))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'reg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg)
  (imm))

(define-instruction-format (ext-rex-reg/mem-xmm 40
                                        :default-printer
                                        '(:name :tab reg/mem ", " reg))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)    :value #x0f)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'reg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg)
  (imm))

(define-instruction-format (ext-2byte-reg/mem-xmm 40
                                        :default-printer
                                        '(:name :tab reg/mem ", " reg))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op1     :field (byte 8 16))
  (op2     :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32)) :type 'reg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg)
  (imm))

(define-instruction-format (ext-rex-2byte-reg/mem-xmm 48
                                        :default-printer
                                        '(:name :tab reg/mem ", " reg))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op1     :field (byte 8 24))
  (op2     :field (byte 8 32))
  (reg/mem :fields (list (byte 2 46) (byte 3 40)) :type 'reg/mem)
  (reg     :field (byte 3 43)   :type 'xmmreg)
  (imm))

;;; Instructions having a general-purpose register as one operand and an a
;;; general-purpose register or a memory location as the other operand,
;;; and using a prefix byte.

(define-instruction-format (ext-prefix-reg-reg/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'sized-reg/mem)
  (reg     :field (byte 3 27)   :type 'reg))

(define-instruction-format (ext-rex-prefix-reg-reg/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'sized-reg/mem)
  (reg     :field (byte 3 35)   :type 'reg))

(define-instruction-format (ext-2byte-prefix-reg-reg/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op1     :field (byte 8 16))          ; #x38 or #x3a
  (op2     :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'sized-reg/mem)
  (reg     :field (byte 3 35)   :type 'reg))

(define-instruction-format (ext-rex-2byte-prefix-reg-reg/mem 48
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op1     :field (byte 8 24))          ; #x38 or #x3a
  (op2     :field (byte 8 32))
  (reg/mem :fields (list (byte 2 46) (byte 3 40))
                                :type 'sized-reg/mem)
  (reg     :field (byte 3 43)   :type 'reg))

;; XMM comparison instruction

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *sse-conditions* #(:eq :lt :le :unord :neq :nlt :nle :ord)))

(define-arg-type sse-condition-code
  ;; Inherit the prefilter from IMM-BYTE to READ-SUFFIX the byte.
  :type 'imm-byte
  :printer *sse-conditions*)

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

(define-instruction-format (near-cond-jump 48)
  (op    :fields (list (byte 8 0) (byte 4 12)) :value '(#b00001111 #b1000))
  (cc    :field (byte 4 8) :type 'condition-code)
  (label :field (byte 32 16) :type 'displacement))

(define-instruction-format (near-jump 40 :default-printer '(:name :tab label))
  (op    :field (byte 8 0))
  (label :field (byte 32 8) :type 'displacement))

(define-instruction-format (cond-set 24 :default-printer '('set cc :tab reg/mem))
  (prefix :field (byte 8 0) :value #b00001111)
  (op    :field (byte 4 12) :value #b1001)
  (cc    :field (byte 4 8) :type 'condition-code)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
           :type 'sized-byte-reg/mem)
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

;;; Single byte instruction with an immediate byte argument.
(define-instruction-format (byte-imm 16 :default-printer '(:name :tab code))
 (op :field (byte 8 0))
 (code :field (byte 8 8) :reader byte-imm-code))

;;; Two byte instruction with an immediate byte argument.
;;;
(define-instruction-format (word-imm 24 :default-printer '(:name :tab code))
  (op :field (byte 16 0))
  (code :field (byte 8 16) :reader word-imm-code))

;;; F3 escape map - Needs a ton more work.

(define-instruction-format (F3-escape 24)
  (prefix1 :field (byte 8 0) :value #xF3)
  (prefix2 :field (byte 8 8) :value #x0F)
  (op      :field (byte 8 16)))

(define-instruction-format (rex-F3-escape 32)
  ;; F3 is a legacy prefix which was generalized to select an alternate opcode
  ;; map. Legacy prefixes are encoded in the instruction before a REX prefix.
  (prefix1 :field (byte 8 0)  :value #xF3)
  (rex     :field (byte 4 12) :value 4)    ; "prefix2"
  (wrxb    :field (byte 4 8)  :type 'wrxb)
  (prefix3 :field (byte 8 16) :value #x0F)
  (op      :field (byte 8 24)))

(define-instruction-format (F3-escape-reg-reg/mem 32
                                        :include F3-escape
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (reg/mem :fields (list (byte 2 30) (byte 3 24)) :type 'sized-reg/mem)
  (reg     :field  (byte 3 27) :type 'reg))

(define-instruction-format (rex-F3-escape-reg-reg/mem 40
                                        :include rex-F3-escape
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (reg/mem :fields (list (byte 2 38) (byte 3 32)) :type 'sized-reg/mem)
  (reg     :field  (byte 3 35) :type 'reg))


;;;; primitive emitters

(define-bitfield-emitter emit-word 16
  (byte 16 0))

;; FIXME: a nice enhancement would be to save all sexprs of small functions
;; within the same file, and drop them at the end.
;; Expressly declaimed inline definitions would be saved as usual though.
(declaim (inline emit-dword))
(define-bitfield-emitter emit-dword 32
  (byte 32 0))
(declaim (notinline emit-dword))

;;; Most uses of dwords are as displacements or as immediate values in
;;; 64-bit operations. In these cases they are sign-extended to 64 bits.
;;; EMIT-DWORD is unsuitable there because it accepts values of type
;;; (OR (SIGNED-BYTE 32) (UNSIGNED-BYTE 32)), so we provide a more
;;; restricted emitter here.
(defun emit-signed-dword (segment value)
  (declare (type sb!assem:segment segment)
           (type (signed-byte 32) value))
  (declare (inline emit-dword))
  (emit-dword segment value))

(define-bitfield-emitter emit-qword 64
  (byte 64 0))

(define-bitfield-emitter emit-byte-with-reg 8
  (byte 5 3) (byte 3 0))

(define-bitfield-emitter emit-mod-reg-r/m-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))

(define-bitfield-emitter emit-sib-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))

(define-bitfield-emitter emit-rex-byte 8
  (byte 4 4) (byte 1 3) (byte 1 2) (byte 1 1) (byte 1 0))



;;;; fixup emitters

(defun emit-absolute-fixup (segment fixup &optional quad-p)
  (note-fixup segment (if quad-p :absolute64 :absolute) fixup)
  (let ((offset (fixup-offset fixup)))
    (if (label-p offset)
        (emit-back-patch segment
                         (if quad-p 8 4)
                         (lambda (segment posn)
                           (declare (ignore posn))
                           (let ((val  (- (+ (component-header-length)
                                             (or (label-position offset)
                                                 0))
                                          other-pointer-lowtag)))
                             (if quad-p
                                 (emit-qword segment val)
                                 (emit-signed-dword segment val)))))
        (if quad-p
            (emit-qword segment (or offset 0))
            (emit-signed-dword segment (or offset 0))))))

(defun emit-relative-fixup (segment fixup)
  (note-fixup segment :relative fixup)
  (emit-signed-dword segment (or (fixup-offset fixup) 0)))


;;;; the effective-address (ea) structure

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  ;; ea only has space for three bits of register number: regs r8
  ;; and up are selected by a REX prefix byte which caller is responsible
  ;; for having emitted where necessary already
  (ecase (sb-name (sc-sb (tn-sc tn)))
    (registers
     (let ((offset (mod (tn-offset tn) 16)))
       (logior (ash (logand offset 1) 2)
               (ash offset -1))))
    (float-registers
     (mod (tn-offset tn) 8))))

(defstruct (ea (:constructor make-ea (size &key base index scale disp))
               (:copier nil))
  ;; note that we can represent an EA with a QWORD size, but EMIT-EA
  ;; can't actually emit it on its own: caller also needs to emit REX
  ;; prefix
  (size nil :type (member :byte :word :dword :qword))
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
           (write-string (sb!c:location-print-name (ea-base ea)) stream)
           (when (ea-index ea)
             (write-string "+" stream)))
         (when (ea-index ea)
           (write-string (sb!c:location-print-name (ea-index ea)) stream))
         (unless (= (ea-scale ea) 1)
           (format stream "*~A" (ea-scale ea)))
         (typecase (ea-disp ea)
           (null)
           (integer
            (format stream "~@D" (ea-disp ea)))
           (t
            (format stream "+~A" (ea-disp ea))))
         (write-char #\] stream))))

(defun emit-constant-tn-rip (segment constant-tn reg remaining-bytes)
  ;; AMD64 doesn't currently have a code object register to use as a
  ;; base register for constant access. Instead we use RIP-relative
  ;; addressing. The offset from the SIMPLE-FUN-HEADER to the instruction
  ;; is passed to the backpatch callback. In addition we need the offset
  ;; from the start of the function header to the slot in the CODE-HEADER
  ;; that stores the constant. Since we don't know where the code header
  ;; starts, instead count backwards from the function header.
  (let* ((2comp (component-info *component-being-compiled*))
         (constants (ir2-component-constants 2comp))
         (len (length constants))
         ;; Both CODE-HEADER and SIMPLE-FUN-HEADER are 16-byte aligned.
         ;; If there are an even amount of constants, there will be
         ;; an extra qword of padding before the function header, which
         ;; needs to be adjusted for. XXX: This will break if new slots
         ;; are added to the code header.
         (offset (* (- (+ len (if (evenp len)
                                  1
                                  2))
                       (tn-offset constant-tn))
                    n-word-bytes)))
    ;; RIP-relative addressing
    (emit-mod-reg-r/m-byte segment #b00 reg #b101)
    (emit-back-patch segment
                     4
                     (lambda (segment posn)
                       ;; The addressing is relative to end of instruction,
                       ;; i.e. the end of this dword. Hence the + 4.
                       (emit-signed-dword segment
                                          (+ 4 remaining-bytes
                                             (- (+ offset posn)))))))
  (values))

(defun emit-label-rip (segment fixup reg remaining-bytes)
  (let ((label (fixup-offset fixup)))
    ;; RIP-relative addressing
    (emit-mod-reg-r/m-byte segment #b00 reg #b101)
    (emit-back-patch segment
                     4
                     (lambda (segment posn)
                       (emit-signed-dword segment
                                          (- (label-position label)
                                             (+ posn 4 remaining-bytes))))))
  (values))

(defun emit-ea (segment thing reg &key allow-constants (remaining-bytes 0))
  (etypecase thing
    (tn
     ;; this would be eleganter if we had a function that would create
     ;; an ea given a tn
     (ecase (sb-name (sc-sb (tn-sc thing)))
       ((registers float-registers)
        (emit-mod-reg-r/m-byte segment #b11 reg (reg-tn-encoding thing)))
       (stack
        ;; Convert stack tns into an index off RBP.
        (let ((disp (frame-byte-offset (tn-offset thing))))
          (cond ((<= -128 disp 127)
                 (emit-mod-reg-r/m-byte segment #b01 reg #b101)
                 (emit-byte segment disp))
                (t
                 (emit-mod-reg-r/m-byte segment #b10 reg #b101)
                 (emit-signed-dword segment disp)))))
       (constant
        (unless allow-constants
          ;; Why?
          (error
           "Constant TNs can only be directly used in MOV, PUSH, and CMP."))
        (emit-constant-tn-rip segment thing reg remaining-bytes))))
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
         (return-from emit-ea (emit-ea segment disp reg
                                       :allow-constants allow-constants
                                       :remaining-bytes remaining-bytes)))
       (when (and (= mod 0) (= r/m #b101))
         ;; this is rip-relative in amd64, so we'll use a sib instead
         (setf r/m #b100 scale 1))
       (emit-mod-reg-r/m-byte segment mod reg r/m)
       (when (= r/m #b100)
         (let ((ss (1- (integer-length scale)))
               (index (if (null index)
                          #b100
                          (if (location= index sb!vm::rsp-tn)
                              (error "can't index off of RSP")
                              (reg-tn-encoding index))))
               (base (if (null base)
                         #b101
                         (reg-tn-encoding base))))
           (emit-sib-byte segment ss index base)))
       (cond ((= mod #b01)
              (emit-byte segment disp))
             ((or (= mod #b10) (null base))
              (if (fixup-p disp)
                  (emit-absolute-fixup segment disp)
                  (emit-signed-dword segment disp))))))
    (fixup
     (typecase (fixup-offset thing)
       (label
        (emit-label-rip segment thing reg remaining-bytes))
       (t
        (emit-mod-reg-r/m-byte segment #b00 reg #b100)
        (emit-sib-byte segment 0 #b100 #b101)
        (emit-absolute-fixup segment thing))))))

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

(defun qword-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (member (sc-name (tn-sc thing)) *qword-sc-names*)
       t))

(defun qword-ea-p (thing)
  (typecase thing
    (ea (eq (ea-size thing) :qword))
    (tn
     (and (member (sc-name (tn-sc thing)) *qword-sc-names*) t))
    (t nil)))

;;; Return true if THING is a general-purpose register TN.
(defun register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)))

(defun accumulator-p (thing)
  (and (register-p thing)
       (= (tn-offset thing) 0)))

;;; Return true if THING is an XMM register TN.
(defun xmm-register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'float-registers)))


;;;; utilities

(def!constant +operand-size-prefix-byte+ #b01100110)

(defun maybe-emit-operand-size-prefix (segment size)
  (unless (or (eq size :byte)
              (eq size :qword)          ; REX prefix handles this
              (eq size +default-operand-size+))
    (emit-byte segment +operand-size-prefix-byte+)))

;;; A REX prefix must be emitted if at least one of the following
;;; conditions is true:
;;  1. The operand size is :QWORD and the default operand size of the
;;     instruction is not :QWORD.
;;; 2. The instruction references an extended register.
;;; 3. The instruction references one of the byte registers SIL, DIL,
;;;    SPL or BPL.

;;; Emit a REX prefix if necessary. OPERAND-SIZE is used to determine
;;; whether to set REX.W. Callers pass it explicitly as :DO-NOT-SET if
;;; this should not happen, for example because the instruction's
;;; default operand size is qword. R, X and B are NIL or TNs specifying
;;; registers the encodings of which are extended with the REX.R, REX.X
;;; and REX.B bit, respectively. To determine whether one of the byte
;;; registers is used that can only be accessed using a REX prefix, we
;;; need only to test R and B, because X is only used for the index
;;; register of an effective address and therefore never byte-sized.
;;; For R we can avoid to calculate the size of the TN because it is
;;; always OPERAND-SIZE. The size of B must be calculated here because
;;; B can be address-sized (if it is the base register of an effective
;;; address), of OPERAND-SIZE (if the instruction operates on two
;;; registers) or of some different size (in the instructions that
;;; combine arguments of different sizes: MOVZX, MOVSX, MOVSXD and
;;; several SSE instructions, e.g. CVTSD2SI). We don't distinguish
;;; between general-purpose and floating point registers for this cause
;;; because only general-purpose registers can be byte-sized at all.
(defun maybe-emit-rex-prefix (segment operand-size r x b)
  (declare (type (member nil :byte :word :dword :qword :do-not-set)
                 operand-size)
           (type (or null tn) r x b))
  (labels ((if-hi (r)
             (if (and r (> (tn-offset r)
                           ;; offset of r8 is 16, offset of xmm8 is 8
                           (if (eq (sb-name (sc-sb (tn-sc r)))
                                   'float-registers)
                               7
                               15)))
                 1
                 0))
           (reg-4-7-p (r)
             ;; Assuming R is a TN describing a general-purpose
             ;; register, return true if it references register
             ;; 4 upto 7.
             (<= 8 (tn-offset r) 15)))
    (let ((rex-w (if (eq operand-size :qword) 1 0))
          (rex-r (if-hi r))
          (rex-x (if-hi x))
          (rex-b (if-hi b)))
      (when (or (not (zerop (logior rex-w rex-r rex-x rex-b)))
                (and r
                     (eq operand-size :byte)
                     (reg-4-7-p r))
                (and b
                     (eq (operand-size b) :byte)
                     (reg-4-7-p b)))
        (emit-rex-byte segment #b0100 rex-w rex-r rex-x rex-b)))))

;;; Emit a REX prefix if necessary. The operand size is determined from
;;; THING or can be overwritten by OPERAND-SIZE. This and REG are always
;;; passed to MAYBE-EMIT-REX-PREFIX. Additionally, if THING is an EA we
;;; pass its index and base registers, if it is a register TN, we pass
;;; only itself.
;;; In contrast to EMIT-EA above, neither stack TNs nor fixups need to
;;; be treated specially here: If THING is a stack TN, neither it nor
;;; any of its components are passed to MAYBE-EMIT-REX-PREFIX which
;;; works correctly because stack references always use RBP as the base
;;; register and never use an index register so no extended registers
;;; need to be accessed. Fixups are assembled using an addressing mode
;;; of displacement-only or RIP-plus-displacement (see EMIT-EA), so may
;;; not reference an extended register. The displacement-only addressing
;;; mode requires that REX.X is 0, which is ensured here.
(defun maybe-emit-rex-for-ea (segment thing reg &key operand-size)
  (declare (type (or ea tn fixup) thing)
           (type (or null tn) reg)
           (type (member nil :byte :word :dword :qword :do-not-set)
                 operand-size))
  (let ((ea-p (ea-p thing)))
    (maybe-emit-rex-prefix segment
                           (or operand-size (operand-size thing))
                           reg
                           (and ea-p (ea-index thing))
                           (cond (ea-p (ea-base thing))
                                 ((and (tn-p thing)
                                       (member (sb-name (sc-sb (tn-sc thing)))
                                               '(float-registers registers)))
                                  thing)
                                 (t nil)))))

(defun operand-size (thing)
  (typecase thing
    (tn
     ;; FIXME: might as well be COND instead of having to use #. readmacro
     ;; to hack up the code
     (case (sc-name (tn-sc thing))
       #!+sb-simd-pack
       (#.sb!vm::*oword-sc-names*
        :oword)
       (#.*qword-sc-names*
        :qword)
       (#.*dword-sc-names*
        :dword)
       (#.*word-sc-names*
        :word)
       (#.*byte-sc-names*
        :byte)
       ;; added by jrd: float-registers is a separate size (?)
       ;; The only place in the code where we are called with THING
       ;; being a float-register is in MAYBE-EMIT-REX-PREFIX when it
       ;; checks whether THING is a byte register. Thus our result in
       ;; these cases could as well be :dword and :qword. I leave it as
       ;; :float and :double which is more likely to trigger an aver
       ;; instead of silently doing the wrong thing in case this
       ;; situation should change. Lutz Euler, 2005-10-23.
       (#.sb!vm::*float-sc-names*
        :float)
       (#.sb!vm::*double-sc-names*
        :double)
       (#.sb!vm::*complex-sc-names*
        :complex)
       (t
        (error "can't tell the size of ~S ~S" thing (sc-name (tn-sc thing))))))
    (ea
     (ea-size thing))
    (fixup
     ;; GNA.  Guess who spelt "flavor" correctly first time round?
     ;; There's a strong argument in my mind to change all uses of
     ;; "flavor" to "kind": and similarly with some misguided uses of
     ;; "type" here and there.  -- CSR, 2005-01-06.
     (case (fixup-flavor thing)
       ((:foreign-dataref) :qword)))
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

;;; Except in a very few cases (MOV instructions A1, A3 and B8 - BF)
;;; we expect dword data bytes even when 64 bit work is being done.
;;; But A1 and A3 are currently unused and B8 - BF use EMIT-QWORD
;;; directly, so we emit all quad constants as dwords, additionally
;;; making sure that they survive the sign-extension to 64 bits
;;; unchanged.
(defun emit-sized-immediate (segment size value)
  (ecase size
    (:byte
     (emit-byte segment value))
    (:word
     (emit-word segment value))
    (:dword
     (emit-dword segment value))
    (:qword
     (emit-signed-dword segment value))))

;;;; prefixes

(define-instruction rex (segment)
  (:printer rex () nil :print-name nil)
  (:emitter
   (bug "REX prefix used as a standalone instruction")))

(define-instruction x66 (segment)
  (:printer x66 () nil :print-name nil)
  (:emitter
   (bug "#X66 prefix used as a standalone instruction")))

(defun emit-prefix (segment name)
  (declare (ignorable segment))
  (ecase name
    ((nil))
    (:lock
     #!+sb-thread
     (emit-byte segment #xf0))))

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

;;; This is the part of the MOV instruction emitter that does moving
;;; of an immediate value into a qword register. We go to some length
;;; to achieve the shortest possible encoding.
(defun emit-immediate-move-to-qword-register (segment dst src)
  (declare (type integer src))
  (cond ((typep src '(unsigned-byte 32))
         ;; We use the B8 - BF encoding with an operand size of 32 bits
         ;; here and let the implicit zero-extension fill the upper half
         ;; of the 64-bit destination register. Instruction size: five
         ;; or six bytes. (A REX prefix will be emitted only if the
         ;; destination is an extended register.)
         (maybe-emit-rex-prefix segment :dword nil nil dst)
         (emit-byte-with-reg segment #b10111 (reg-tn-encoding dst))
         (emit-dword segment src))
        (t
         (maybe-emit-rex-prefix segment :qword nil nil dst)
         (cond ((typep src '(signed-byte 32))
                ;; Use the C7 encoding that takes a 32-bit immediate and
                ;; sign-extends it to 64 bits. Instruction size: seven
                ;; bytes.
                (emit-byte segment #b11000111)
                (emit-mod-reg-r/m-byte segment #b11 #b000
                                       (reg-tn-encoding dst))
                (emit-signed-dword segment src))
               ((<= (- (expt 2 64) (expt 2 31))
                    src
                    (1- (expt 2 64)))
                ;; This triggers on positive integers of 64 bits length
                ;; with the most significant 33 bits being 1. We use the
                ;; same encoding as in the previous clause.
                (emit-byte segment #b11000111)
                (emit-mod-reg-r/m-byte segment #b11 #b000
                                       (reg-tn-encoding dst))
                (emit-signed-dword segment (- src (expt 2 64))))
               (t
                ;; We need a full 64-bit immediate. Instruction size:
                ;; ten bytes.
                (emit-byte-with-reg segment #b10111 (reg-tn-encoding dst))
                (emit-qword segment src))))))

(define-instruction mov (segment dst src)
  ;; immediate to register
  (:printer reg ((op #b1011) (imm nil :type 'signed-imm-data/asm-routine))
            '(:name :tab reg ", " imm))
  (:printer rex-reg ((op #b1011)
                     (imm nil :type 'signed-imm-data-upto-qword/asm-routine))
            '(:name :tab reg ", " imm))
  ;; absolute mem to/from accumulator
  (:printer simple-dir ((op #b101000) (imm nil :type 'imm-addr))
            `(:name :tab ,(swap-if 'dir 'accum ", " '("[" imm "]"))))
  ;; register to/from register/memory
  (:printer reg-reg/mem-dir ((op #b100010)))
  ;; immediate to register/memory
  (:printer reg/mem-imm/asm-routine ((op '(#b1100011 #b000))))

  (:emitter
   (let ((size (matching-operand-size dst src)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((register-p dst)
            (cond ((integerp src)
                   (cond ((eq size :qword)
                          (emit-immediate-move-to-qword-register segment
                                                                 dst src))
                         (t
                          (maybe-emit-rex-prefix segment size nil nil dst)
                          (emit-byte-with-reg segment
                                              (if (eq size :byte)
                                                  #b10110
                                                  #b10111)
                                              (reg-tn-encoding dst))
                          (emit-sized-immediate segment size src))))
                  ((and (fixup-p src)
                        (or (eq (fixup-flavor src) :foreign)
                            (eq (fixup-flavor src) :assembly-routine)))
                   (maybe-emit-rex-prefix segment :dword nil nil dst)
                   (emit-byte-with-reg segment #b10111 (reg-tn-encoding dst))
                   (emit-absolute-fixup segment src))
                  (t
                   (maybe-emit-rex-for-ea segment src dst)
                   (emit-byte segment
                              (if (eq size :byte)
                                  #b10001010
                                  #b10001011))
                   (emit-ea segment src (reg-tn-encoding dst)
                            :allow-constants t))))
           ((integerp src)
            ;; C7 only deals with 32 bit immediates even if the
            ;; destination is a 64-bit location. The value is
            ;; sign-extended in this case.
            (maybe-emit-rex-for-ea segment dst nil)
            (emit-byte segment (if (eq size :byte) #b11000110 #b11000111))
            (emit-ea segment dst #b000)
            (emit-sized-immediate segment size src))
           ((register-p src)
            (maybe-emit-rex-for-ea segment dst src)
            (emit-byte segment (if (eq size :byte) #b10001000 #b10001001))
            (emit-ea segment dst (reg-tn-encoding src)))
           ((fixup-p src)
            ;; Generally we can't MOV a fixupped value into an EA, since
            ;; MOV on non-registers can only take a 32-bit immediate arg.
            ;; Make an exception for :FOREIGN fixups (pretty much just
            ;; the runtime asm, since other foreign calls go through the
            ;; the linkage table) and for linkage table references, since
            ;; these should always end up in low memory.
            (aver (or (member (fixup-flavor src)
                              '(:foreign :foreign-dataref :symbol-tls-index))
                      (eq (ea-size dst) :dword)))
            (maybe-emit-rex-for-ea segment dst nil)
            (emit-byte segment #b11000111)
            (emit-ea segment dst #b000)
            (emit-absolute-fixup segment src))
           (t
            (error "bogus arguments to MOV: ~S ~S" dst src))))))

;;; Emit a sign-extending (if SIGNED-P is true) or zero-extending move.
;;; To achieve the shortest possible encoding zero extensions into a
;;; 64-bit destination are assembled as a straight 32-bit MOV (if the
;;; source size is 32 bits) or as MOVZX with a 32-bit destination (if
;;; the source size is 8 or 16 bits). Due to the implicit zero extension
;;; to 64 bits this has the same effect as a MOVZX with 64-bit
;;; destination but often needs no REX prefix.
(defun emit-move-with-extension (segment dst src signed-p)
  (aver (register-p dst))
  (let ((dst-size (operand-size dst))
        (src-size (operand-size src))
        (opcode (if signed-p #b10111110 #b10110110)))
    (macrolet ((emitter (operand-size &rest bytes)
                 `(progn
                   (maybe-emit-rex-for-ea segment src dst
                                          :operand-size ,operand-size)
                   ,@(mapcar (lambda (byte)
                               `(emit-byte segment ,byte))
                             bytes)
                   (emit-ea segment src (reg-tn-encoding dst)))))
      (ecase dst-size
        (:word
         (aver (eq src-size :byte))
         (maybe-emit-operand-size-prefix segment :word)
         (emitter :word #b00001111 opcode))
        ((:dword :qword)
         (unless signed-p
           (setf dst-size :dword))
         (ecase src-size
           (:byte
            (emitter dst-size #b00001111 opcode))
           (:word
            (emitter dst-size #b00001111 (logior opcode 1)))
           (:dword
            (aver (or (not signed-p) (eq dst-size :qword)))
            (emitter dst-size
                     (if signed-p #x63 #x8b))))))))) ; movsxd or straight mov

;; MOV[SZ]X - #x66 or REX selects the destination REG size, wherein :byte isn't
;; a possibility.  The 'width' bit selects a source r/m size of :byte or :word.
(define-instruction-format
    (move-with-extension 24 :include ext-reg-reg/mem
     :default-printer
     '(:name :tab reg ", "
       (:cond ((width :constant 0) (:using #'print-sized-byte-reg/mem reg/mem))
              (t (:using #'print-sized-word-reg/mem reg/mem)))))
  (width :prefilter nil)) ; doesn't affect DSTATE

(define-instruction movsx (segment dst src)
  (:printer move-with-extension ((op #b1011111)))
  (:emitter (emit-move-with-extension segment dst src :signed)))

(define-instruction movzx (segment dst src)
  (:printer move-with-extension ((op #b1011011)))
  (:emitter (emit-move-with-extension segment dst src nil)))

;;; The regular use of MOVSXD is with an operand size of :qword. This
;;; sign-extends the dword source into the qword destination register.
;;; If the operand size is :dword the instruction zero-extends the dword
;;; source into the qword destination register, i.e. it does the same as
;;; a dword MOV into a register.
(define-instruction movsxd (segment dst src)
  (:printer reg-reg/mem ((op #b0110001) (width 1)
                         (reg/mem nil :type 'sized-dword-reg/mem)))
  (:emitter (emit-move-with-extension segment dst src :signed)))

;;; this is not a real amd64 instruction, of course
(define-instruction movzxd (segment dst src)
  ; (:printer reg-reg/mem ((op #x63) (reg nil :type 'reg)))
  (:emitter (emit-move-with-extension segment dst src nil)))

(define-instruction push (segment src)
  ;; register
  (:printer reg-no-width-default-qword ((op #b01010)))
  ;; register/memory
  (:printer reg/mem-default-qword ((op '(#b11111111 #b110))))
  ;; immediate
  (:printer byte ((op #b01101010) (imm nil :type 'signed-imm-byte))
            '(:name :tab imm))
  (:printer byte ((op #b01101000)
                  (imm nil :type 'signed-imm-data-default-qword))
            '(:name :tab imm))
  ;; ### segment registers?

  (:emitter
   (cond ((integerp src)
          (cond ((<= -128 src 127)
                 (emit-byte segment #b01101010)
                 (emit-byte segment src))
                (t
                 ;; A REX-prefix is not needed because the operand size
                 ;; defaults to 64 bits. The size of the immediate is 32
                 ;; bits and it is sign-extended.
                 (emit-byte segment #b01101000)
                 (emit-signed-dword segment src))))
         (t
          (let ((size (operand-size src)))
            (aver (or (eq size :qword) (eq size :word)))
            (maybe-emit-operand-size-prefix segment size)
            (maybe-emit-rex-for-ea segment src nil :operand-size :do-not-set)
            (cond ((register-p src)
                   (emit-byte-with-reg segment #b01010 (reg-tn-encoding src)))
                  (t
                   (emit-byte segment #b11111111)
                   (emit-ea segment src #b110 :allow-constants t))))))))

(define-instruction pop (segment dst)
  (:printer reg-no-width-default-qword ((op #b01011)))
  (:printer reg/mem-default-qword ((op '(#b10001111 #b000))))
  (:emitter
   (let ((size (operand-size dst)))
     (aver (or (eq size :qword) (eq size :word)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil :operand-size :do-not-set)
     (cond ((register-p dst)
            (emit-byte-with-reg segment #b01011 (reg-tn-encoding dst)))
           (t
            (emit-byte segment #b10001111)
            (emit-ea segment dst #b000))))))

;;; Compared to x86 we need to take two particularities into account
;;; here:
;;; * XCHG EAX, EAX can't be encoded as #x90 as the processor interprets
;;;   that opcode as NOP while XCHG EAX, EAX is specified to clear the
;;;   upper half of RAX. We need to use the long form #x87 #xC0 instead.
;;; * The opcode #x90 is not only used for NOP and XCHG RAX, RAX and
;;;   XCHG AX, AX, but also for XCHG RAX, R8 (and the corresponding 32-
;;;   and 16-bit versions). The printer for the NOP instruction (further
;;;   below) matches all these encodings so needs to be overridden here
;;;   for the cases that need to print as XCHG.
;;; Assembler and disassembler chained then map these special cases as
;;; follows:
;;;   (INST NOP)                 ->  90      ->  NOP
;;;   (INST XCHG RAX-TN RAX-TN)  ->  4890    ->  NOP
;;;   (INST XCHG EAX-TN EAX-TN)  ->  87C0    ->  XCHG EAX, EAX
;;;   (INST XCHG AX-TN AX-TN)    ->  6690    ->  NOP
;;;   (INST XCHG RAX-TN R8-TN)   ->  4990    ->  XCHG RAX, R8
;;;   (INST XCHG EAX-TN R8D-TN)  ->  4190    ->  XCHG EAX, R8D
;;;   (INST XCHG AX-TN R8W-TN)   ->  664190  ->  XCHG AX, R8W
;;; The disassembler additionally correctly matches encoding variants
;;; that the assembler doesn't generate, for example 4E90 prints as NOP
;;; and 4F90 as XCHG RAX, R8 (both because REX.R and REX.X are ignored).
(define-instruction xchg (segment operand1 operand2)
  ;; This printer matches all patterns that encode exchanging RAX with
  ;; R8, EAX with R8D, or AX with R8W. These consist of the opcode #x90
  ;; with a REX prefix with REX.B = 1, and possibly the #x66 prefix.
  ;; We rely on the prefix automatism for the #x66 prefix, but
  ;; explicitly match the REX prefix as we need to provide a value for
  ;; REX.B, and to override the NOP printer by virtue of a longer match.
  (:printer rex-accum-reg ((rex-b 1) (op #b10010) (reg #b000)))
  ;; Register with accumulator.
  (:printer reg-no-width ((op #b10010)) '(:name :tab accum ", " reg))
  ;; Register/Memory with Register.
  (:printer reg-reg/mem ((op #b1000011)))
  (:emitter
   (let ((size (matching-operand-size operand1 operand2)))
     (maybe-emit-operand-size-prefix segment size)
     (labels ((xchg-acc-with-something (acc something)
                (if (and (not (eq size :byte))
                         (register-p something)
                         ;; Don't use the short encoding for XCHG EAX, EAX:
                         (not (and (= (tn-offset something) sb!vm::eax-offset)
                                   (eq size :dword))))
                    (progn
                      (maybe-emit-rex-for-ea segment something acc)
                      (emit-byte-with-reg segment
                                          #b10010
                                          (reg-tn-encoding something)))
                    (xchg-reg-with-something acc something)))
              (xchg-reg-with-something (reg something)
                (maybe-emit-rex-for-ea segment something reg)
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

;; It's an error to compile instructions without their labeler and printer defined
;; in the compiler, even though they aren't called.
;; This stems from compile-time use of (MAKE-VALSRC #'f '#'f)
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)

;; If the filtered VALUE (R/M field of LEA) should be treated as a label,
;; return the virtual address, otherwise the value unchanged.
(defun lea-compute-label (value dstate)
  (if (and (listp value) (eq (first value) 'rip))
      (+ (dstate-next-addr dstate) (second value))
      value))

;; Figure out whether LEA should print its EA with just the stuff in brackets,
;; or additionally show the EA as either a label or a hex literal.
(defun lea-print-ea (value stream dstate)
  (let ((width (inst-operand-size dstate)))
    (etypecase value
      (list
       ;; Indicate to PRINT-MEM-REF that this is not a memory access.
       (print-mem-ref :compute value width stream dstate)
       (when (eq (first value) 'rip)
         (let ((addr (+ (dstate-next-addr dstate) (second value))))
           (note (lambda (s) (format s "= #x~x" addr)) dstate))))

      (string
       ;; A label for the EA should not print as itself, but as the decomposed
       ;; addressing mode so that [ADDR] and [RIP+disp] are unmistakable.
       (print-mem-ref :compute (reg-r/m-inst-r/m-arg dchunk-zero dstate)
                      width stream dstate)
       (note (lambda (s) (format s "= ~A" value)) dstate))

      ;; We're robust in allowing VALUE to be an integer (a register),
      ;; though LEA Rx,Ry is an illegal instruction.
      (full-reg
       (print-reg-with-width value width stream dstate)))))

) ; EVAL-WHEN

(define-instruction lea (segment dst src)
  (:printer
   reg-reg/mem
   ((op #b1000110) (width 1)
    (reg/mem nil :use-label #'lea-compute-label :printer #'lea-print-ea)))
  (:emitter
   (aver (or (dword-reg-p dst) (qword-reg-p dst)))
   (maybe-emit-rex-for-ea segment src dst
                          :operand-size (if (dword-reg-p dst) :dword :qword))
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
     (maybe-emit-rex-for-ea segment dst src)
     (emit-byte segment #b00001111)
     (emit-byte segment (if (eq size :byte) #b10110000 #b10110001))
     (emit-ea segment dst (reg-tn-encoding src)))))

(define-instruction cmpxchg16b (segment mem &optional prefix)
  (:printer ext-reg/mem-no-width
            ((op '(#xC7 1))))
  (:emitter
   (aver (not (register-p mem)))
   (emit-prefix segment prefix)
   (maybe-emit-rex-for-ea segment mem nil :operand-size :qword)
   (emit-byte segment #x0F)
   (emit-byte segment #xC7)
   (emit-ea segment mem 1))) ; operand extension

(define-instruction rdrand (segment dst)
  (:printer ext-reg/mem-no-width
            ((op '(#xC7 6))))
  (:emitter
   (aver (register-p dst))
   (maybe-emit-operand-size-prefix segment (operand-size dst))
   (maybe-emit-rex-for-ea segment dst nil)
   (emit-byte segment #x0F)
   (emit-byte segment #xC7)
   (emit-ea segment dst 6)))

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
             (maybe-emit-rex-for-ea segment dst nil)
             (emit-byte segment #b10000011)
             (emit-ea segment dst opcode :allow-constants allow-constants)
             (emit-byte segment src))
            ((accumulator-p dst)
             (maybe-emit-rex-for-ea segment dst nil)
             (emit-byte segment
                        (dpb opcode
                             (byte 3 3)
                             (if (eq size :byte)
                                 #b00000100
                                 #b00000101)))
             (emit-sized-immediate segment size src))
            (t
             (maybe-emit-rex-for-ea segment dst nil)
             (emit-byte segment (if (eq size :byte) #b10000000 #b10000001))
             (emit-ea segment dst opcode :allow-constants allow-constants)
             (emit-sized-immediate segment size src))))
     ((register-p src)
      (maybe-emit-rex-for-ea segment dst src)
      (emit-byte segment
                 (dpb opcode
                      (byte 3 3)
                      (if (eq size :byte) #b00000000 #b00000001)))
      (emit-ea segment dst (reg-tn-encoding src)
               :allow-constants allow-constants))
     ((register-p dst)
      (maybe-emit-rex-for-ea segment src dst)
      (emit-byte segment
                 (dpb opcode
                      (byte 3 3)
                      (if (eq size :byte) #b00000010 #b00000011)))
      (emit-ea segment src (reg-tn-encoding dst)
               :allow-constants allow-constants))
     (t
      (error "bogus operands to ~A" name)))))

(eval-when (:compile-toplevel :execute)
  (defun arith-inst-printer-list (subop)
    `((accum-imm ((op ,(dpb subop (byte 3 2) #b0000010))))
      (reg/mem-imm ((op (#b1000000 ,subop))))
      ;; The redundant encoding #x82 is invalid in 64-bit mode,
      ;; therefore we force WIDTH to 1.
      (reg/mem-imm ((op (#b1000001 ,subop)) (width 1)
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

(define-instruction sub (segment dst src)
  (:printer-list (arith-inst-printer-list #b101))
  (:emitter (emit-random-arith-inst "SUB" segment dst src #b101)))

(define-instruction sbb (segment dst src)
  (:printer-list (arith-inst-printer-list #b011))
  (:emitter (emit-random-arith-inst "SBB" segment dst src #b011)))

(define-instruction cmp (segment dst src)
  (:printer-list (arith-inst-printer-list #b111))
  (:emitter (emit-random-arith-inst "CMP" segment dst src #b111 t)))

;;; The one-byte encodings for INC and DEC are used as REX prefixes
;;; in 64-bit mode so we always use the two-byte form.
(define-instruction inc (segment dst &optional prefix)
  (:printer reg/mem ((op '(#b1111111 #b000))))
  (:emitter
   (emit-prefix segment prefix)
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)
     (emit-byte segment (if (eq size :byte) #b11111110 #b11111111))
     (emit-ea segment dst #b000))))

(define-instruction dec (segment dst &optional prefix)
  (:printer reg/mem ((op '(#b1111111 #b001))))
  (:emitter
   (emit-prefix segment prefix)
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)
     (emit-byte segment (if (eq size :byte) #b11111110 #b11111111))
     (emit-ea segment dst #b001))))

(define-instruction neg (segment dst)
  (:printer reg/mem ((op '(#b1111011 #b011))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment dst #b011))))

(define-instruction mul (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b100))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b100))))

(define-instruction imul (segment dst &optional src1 src2)
  (:printer accum-reg/mem ((op '(#b1111011 #b101))))
  (:printer ext-reg-reg/mem-no-width ((op #b10101111)))
  ;; These next two are like a single format where one bit in the opcode byte
  ;; determines the size of the immediate datum. A REG-REG/MEM-IMM format
  ;; would save one entry in the decoding table, since that bit would become
  ;; "don't care" from a decoding perspective, but we don't have (many) other
  ;; 3-operand opcodes in the general purpose (non-SSE) opcode space.
  (:printer reg-reg/mem ((op #b0110100) (width 1)
                         (imm nil :type 'signed-imm-data))
            '(:name :tab reg ", " reg/mem ", " imm))
  (:printer reg-reg/mem ((op #b0110101) (width 1)
                         (imm nil :type 'signed-imm-byte))
            '(:name :tab reg ", " reg/mem ", " imm))
  (:emitter
   (flet ((r/m-with-immed-to-reg (reg r/m immed)
            (let* ((size (matching-operand-size reg r/m))
                   (sx (and (not (eq size :byte)) (<= -128 immed 127))))
              (maybe-emit-operand-size-prefix segment size)
              (maybe-emit-rex-for-ea segment r/m reg)
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
                  (maybe-emit-rex-for-ea segment src1 dst)
                  (emit-byte segment #b00001111)
                  (emit-byte segment #b10101111)
                  (emit-ea segment src1 (reg-tn-encoding dst)))))
           (t
            (let ((size (operand-size dst)))
              (maybe-emit-operand-size-prefix segment size)
              (maybe-emit-rex-for-ea segment dst nil)
              (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
              (emit-ea segment dst #b101)))))))

(define-instruction div (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b110))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b110))))

(define-instruction idiv (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b111))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b111))))

(define-instruction bswap (segment dst)
  (:printer ext-reg-no-width ((op #b11001)))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-rex-prefix segment size nil nil dst)
     (emit-byte segment #x0f)
     (emit-byte-with-reg segment #b11001 (reg-tn-encoding dst)))))

;;; CBW -- Convert Byte to Word. AX <- sign_xtnd(AL)
(define-instruction cbw (segment)
  (:printer x66-byte ((op #b10011000)))
  (:emitter
   (maybe-emit-operand-size-prefix segment :word)
   (emit-byte segment #b10011000)))

;;; CWDE -- Convert Word To Double Word Extended. EAX <- sign_xtnd(AX)
(define-instruction cwde (segment)
  (:printer byte ((op #b10011000)))
  (:emitter
   (maybe-emit-operand-size-prefix segment :dword)
   (emit-byte segment #b10011000)))

;;; CDQE -- Convert Double Word To Quad Word Extended. RAX <- sign_xtnd(EAX)
(define-instruction cdqe (segment)
  (:printer rex-byte ((op #b10011000)))
  (:emitter
   (maybe-emit-rex-prefix segment :qword nil nil nil)
   (emit-byte segment #b10011000)))

;;; CWD -- Convert Word to Double Word. DX:AX <- sign_xtnd(AX)
(define-instruction cwd (segment)
  (:printer x66-byte ((op #b10011001)))
  (:emitter
   (maybe-emit-operand-size-prefix segment :word)
   (emit-byte segment #b10011001)))

;;; CDQ -- Convert Double Word to Quad Word. EDX:EAX <- sign_xtnd(EAX)
(define-instruction cdq (segment)
  (:printer byte ((op #b10011001)))
  (:emitter
   (maybe-emit-operand-size-prefix segment :dword)
   (emit-byte segment #b10011001)))

;;; CQO -- Convert Quad Word to Octaword. RDX:RAX <- sign_xtnd(RAX)
(define-instruction cqo (segment)
  (:printer rex-byte ((op #b10011001)))
  (:emitter
   (maybe-emit-rex-prefix segment :qword nil nil nil)
   (emit-byte segment #b10011001)))

(define-instruction xadd (segment dst src &optional prefix)
  ;; Register/Memory with Register.
  (:printer ext-reg-reg/mem ((op #b1100000)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (aver (register-p src))
   (emit-prefix segment prefix)
   (let ((size (matching-operand-size src dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst src)
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
      (maybe-emit-rex-for-ea segment dst nil)
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
    (maybe-emit-rex-for-ea segment dst src)
    (emit-byte segment #b00001111)
    (emit-byte segment (dpb opcode (byte 1 3)
                            (if (eq amt :cl) #b10100101 #b10100100)))
    (emit-ea segment dst (reg-tn-encoding src))
    (unless (eq amt :cl)
      (emit-byte segment amt))))

(eval-when (:compile-toplevel :execute)
  (defun double-shift-inst-printer-list (op)
    `((ext-reg-reg/mem-no-width ((op ,(logior op #b100))
                                 (imm nil :type imm-byte))
         (:name :tab reg/mem ", " reg ", " imm))
      (ext-reg-reg/mem-no-width ((op ,(logior op #b101)))
         (:name :tab reg/mem ", " reg ", " 'cl)))))

(define-instruction shld (segment dst src amt)
  (:declare (type (or (member :cl) (mod 64)) amt))
  (:printer-list (double-shift-inst-printer-list #b10100000))
  (:emitter
   (emit-double-shift segment #b0 dst src amt)))

(define-instruction shrd (segment dst src amt)
  (:declare (type (or (member :cl) (mod 64)) amt))
  (:printer-list (double-shift-inst-printer-list #b10101000))
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
                     (maybe-emit-rex-for-ea segment something nil)
                     (emit-byte segment
                                (if (eq size :byte) #b10101000 #b10101001))
                     (emit-sized-immediate segment size immed))
                    (t
                     (maybe-emit-rex-for-ea segment something nil)
                     (emit-byte segment
                                (if (eq size :byte) #b11110110 #b11110111))
                     (emit-ea segment something #b000)
                     (emit-sized-immediate segment size immed))))
            (test-reg-and-something (reg something)
              (maybe-emit-rex-for-ea segment something reg)
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

(define-instruction or (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b001))
  (:emitter
   (emit-random-arith-inst "OR" segment dst src #b001)))

(define-instruction xor (segment dst src)
  (:printer-list
   (arith-inst-printer-list #b110))
  (:emitter
   (emit-random-arith-inst "XOR" segment dst src #b110)))

(define-instruction not (segment dst)
  (:printer reg/mem ((op '(#b1111011 #b010))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment dst #b010))))

;;;; string manipulation

(define-instruction cmps (segment size)
  (:printer string-op ((op #b1010011)))
  (:emitter
   (maybe-emit-operand-size-prefix segment size)
   (maybe-emit-rex-prefix segment size nil nil nil)
   (emit-byte segment (if (eq size :byte) #b10100110 #b10100111))))

(define-instruction ins (segment acc)
  (:printer string-op ((op #b0110110)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b01101100 #b01101101)))))

(define-instruction lods (segment acc)
  (:printer string-op ((op #b1010110)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b10101100 #b10101101)))))

(define-instruction movs (segment size)
  (:printer string-op ((op #b1010010)))
  (:emitter
   (maybe-emit-operand-size-prefix segment size)
   (maybe-emit-rex-prefix segment size nil nil nil)
   (emit-byte segment (if (eq size :byte) #b10100100 #b10100101))))

(define-instruction outs (segment acc)
  (:printer string-op ((op #b0110111)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b01101110 #b01101111)))))

(define-instruction scas (segment acc)
  (:printer string-op ((op #b1010111)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b10101110 #b10101111)))))

(define-instruction stos (segment acc)
  (:printer string-op ((op #b1010101)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b10101010 #b10101011)))))

(define-instruction xlat (segment)
  (:printer byte ((op #b11010111)))
  (:emitter
   (emit-byte segment #b11010111)))


;;;; bit manipulation

(define-instruction bsf (segment dst src)
  (:printer ext-reg-reg/mem-no-width ((op #b10111100)))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (when (eq size :byte)
       (error "can't scan bytes: ~S" src))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src dst)
     (emit-byte segment #b00001111)
     (emit-byte segment #b10111100)
     (emit-ea segment src (reg-tn-encoding dst)))))

(define-instruction bsr (segment dst src)
  (:printer ext-reg-reg/mem-no-width ((op #b10111101)))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (when (eq size :byte)
       (error "can't scan bytes: ~S" src))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src dst)
     (emit-byte segment #b00001111)
     (emit-byte segment #b10111101)
     (emit-ea segment src (reg-tn-encoding dst)))))

(defun emit-bit-test-and-mumble (segment src index opcode)
  (let ((size (operand-size src)))
    (when (eq size :byte)
      (error "can't scan bytes: ~S" src))
    (maybe-emit-operand-size-prefix segment size)
    (cond ((integerp index)
           (maybe-emit-rex-for-ea segment src nil)
           (emit-byte segment #b00001111)
           (emit-byte segment #b10111010)
           (emit-ea segment src opcode)
           (emit-byte segment index))
          (t
           (maybe-emit-rex-for-ea segment src index)
           (emit-byte segment #b00001111)
           (emit-byte segment (dpb opcode (byte 3 3) #b10000011))
           (emit-ea segment src (reg-tn-encoding index))))))

(eval-when (:compile-toplevel :execute)
  (defun bit-test-inst-printer-list (subop)
    `((ext-reg/mem-no-width+imm8 ((op (#xBA ,subop))))
      (ext-reg-reg/mem-no-width ((op ,(dpb subop (byte 3 3) #b10000011))
                                 (reg/mem nil :type sized-reg/mem))
                                (:name :tab reg/mem ", " reg)))))

(macrolet ((define (inst opcode-extension)
             `(define-instruction ,inst (segment src index &optional prefix)
                (:printer-list (bit-test-inst-printer-list ,opcode-extension))
                (:emitter
                 (emit-prefix segment prefix)
                 (emit-bit-test-and-mumble segment src index
                                           ,opcode-extension)))))
  (define bt  4)
  (define bts 5)
  (define btr 6)
  (define btc 7))


;;;; control transfer

(define-instruction call (segment where)
  (:printer near-jump ((op #b11101000)))
  (:printer reg/mem-default-qword ((op '(#b11111111 #b010))))
  (:emitter
   (typecase where
     (label
      (emit-byte segment #b11101000) ; 32 bit relative
      (emit-back-patch segment
                       4
                       (lambda (segment posn)
                         (emit-signed-dword segment
                                            (- (label-position where)
                                               (+ posn 4))))))
     (fixup
      ;; There is no CALL rel64...
      (error "Cannot CALL a fixup: ~S" where))
     (t
      (maybe-emit-rex-for-ea segment where nil :operand-size :do-not-set)
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
  (:printer near-jump ((op #b11101001)))
  (:printer reg/mem-default-qword ((op '(#b11111111 #b100))))
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
               (emit-signed-dword segment disp)))))
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
               (emit-signed-dword segment disp)))))
         ((fixup-p where)
          (emit-byte segment #b11101001)
          (emit-relative-fixup segment where))
         (t
          (unless (or (ea-p where) (tn-p where))
            (error "don't know what to do with ~A" where))
          ;; near jump defaults to 64 bit
          ;; w-bit in rex prefix is unnecessary
          (maybe-emit-rex-for-ea segment where nil :operand-size :do-not-set)
          (emit-byte segment #b11111111)
          (emit-ea segment where #b100)))))

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

(define-instruction jrcxz (segment target)
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
     (aver (or (eq size :word) (eq size :dword) (eq size :qword)))
     (maybe-emit-operand-size-prefix segment size))
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #b00001111)
   (emit-byte segment (dpb (conditional-opcode cond) (byte 4 0) #b01000000))
   (emit-ea segment src (reg-tn-encoding dst) :allow-constants t)))

;;;; conditional byte set

(define-instruction set (segment dst cond)
  (:printer cond-set ())
  (:emitter
   (maybe-emit-rex-for-ea segment dst nil :operand-size :byte)
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
       (nt "function end breakpoint trap"))
      (#.single-step-around-trap
       (nt "single-step trap (around)"))
      (#.single-step-before-trap
       (nt "single-step trap (before)"))
      (#.invalid-arg-count-trap
       (nt "Invalid argument count trap")))))

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
  ;; multi-byte NOP
  (:printer ext-reg/mem-no-width ((op '(#x1f 0))) '(:name))
  (:emitter
   (emit-byte segment #b10010000)))

;;; Emit a sequence of single- or multi-byte NOPs to fill AMOUNT many
;;; bytes with the smallest possible number of such instructions.
(defun emit-long-nop (segment amount)
  (declare (type sb!assem:segment segment)
           (type index amount))
  ;; Pack all instructions into one byte vector to save space.
  (let* ((bytes #.(!coerce-to-specialized
                          #(#x90
                            #x66 #x90
                            #x0f #x1f #x00
                            #x0f #x1f #x40 #x00
                            #x0f #x1f #x44 #x00 #x00
                            #x66 #x0f #x1f #x44 #x00 #x00
                            #x0f #x1f #x80 #x00 #x00 #x00 #x00
                            #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00
                            #x66 #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00)
                          '(unsigned-byte 8)))
         (max-length (isqrt (* 2 (length bytes)))))
    (loop
      (let* ((count (min amount max-length))
             (start (ash (* count (1- count)) -1)))
        (dotimes (i count)
          (emit-byte segment (aref bytes (+ start i)))))
      (if (> amount max-length)
          (decf amount max-length)
          (return)))))

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
                   n-word-bytes
                   (lambda (segment posn)
                     (emit-qword segment
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

;;;; Instructions required to do floating point operations using SSE

;; Return a one- or two-element list of printers for SSE instructions.
;; The one-element list is used in the cases where the REX prefix is
;; really a prefix and thus automatically supported, the two-element
;; list is used when the REX prefix is used in an infix position.
(eval-when (:compile-toplevel :execute)
  (defun sse-inst-printer-list (inst-format-stem prefix opcode
                                &key more-fields printer)
    (let ((fields `(,@(when prefix
                        `((prefix ,prefix)))
                    (op ,opcode)
                    ,@more-fields))
          (inst-formats (if prefix
                            (list (symbolicate "EXT-" inst-format-stem)
                                  (symbolicate "EXT-REX-" inst-format-stem))
                            (list inst-format-stem))))
      (mapcar (lambda (inst-format)
                `(,inst-format ,fields ,@(when printer
                                           (list printer))))
              inst-formats)))
  (defun 2byte-sse-inst-printer-list (inst-format-stem prefix op1 op2
                                       &key more-fields printer)
    (let ((fields `(,@(when prefix
                        `((prefix, prefix)))
                    (op1 ,op1)
                    (op2 ,op2)
                    ,@more-fields))
          (inst-formats (if prefix
                            (list (symbolicate "EXT-" inst-format-stem)
                                  (symbolicate "EXT-REX-" inst-format-stem))
                            (list inst-format-stem))))
      (mapcar (lambda (inst-format)
                `(,inst-format ,fields ,@(when printer
                                           (list printer))))
              inst-formats))))

(defun emit-sse-inst (segment dst src prefix opcode
                      &key operand-size (remaining-bytes 0))
  (when prefix
    (emit-byte segment prefix))
  (if operand-size
      (maybe-emit-rex-for-ea segment src dst :operand-size operand-size)
      (maybe-emit-rex-for-ea segment src dst))
  (emit-byte segment #x0f)
  (emit-byte segment opcode)
  (emit-ea segment src (reg-tn-encoding dst) :remaining-bytes remaining-bytes))

;; 0110 0110:0000 1111:0111 00gg: 11 010 xmmreg:imm8

(defun emit-sse-inst-with-imm (segment dst/src imm
                               prefix opcode /i
                               &key operand-size)
  (aver (<= 0 /i 7))
  (when prefix
    (emit-byte segment prefix))
  ;; dst/src is encoded in the r/m field, not r; REX.B must be
  ;; set to use extended XMM registers
  (maybe-emit-rex-prefix segment operand-size nil nil dst/src)
  (emit-byte segment #x0F)
  (emit-byte segment opcode)
  (emit-byte segment (logior (ash (logior #b11000 /i) 3)
                             (reg-tn-encoding dst/src)))
  (emit-byte segment imm))

(defun emit-sse-inst-2byte (segment dst src prefix op1 op2
                            &key operand-size (remaining-bytes 0))
  (when prefix
    (emit-byte segment prefix))
  (if operand-size
      (maybe-emit-rex-for-ea segment src dst :operand-size operand-size)
      (maybe-emit-rex-for-ea segment src dst))
  (emit-byte segment #x0f)
  (emit-byte segment op1)
  (emit-byte segment op2)
  (emit-ea segment src (reg-tn-encoding dst) :remaining-bytes remaining-bytes))

(macrolet
    ((define-imm-sse-instruction (name opcode /i)
         `(define-instruction ,name (segment dst/src imm)
            (:printer-list
             ',(sse-inst-printer-list 'xmm-imm #x66 opcode
                                      :more-fields `((/i ,/i))))
            (:emitter
             (emit-sse-inst-with-imm segment dst/src imm
                                     #x66 ,opcode ,/i
                                     :operand-size :do-not-set)))))
  (define-imm-sse-instruction pslldq #x73 7)
  (define-imm-sse-instruction psllw-imm #x71 6)
  (define-imm-sse-instruction pslld-imm #x72 6)
  (define-imm-sse-instruction psllq-imm #x73 6)

  (define-imm-sse-instruction psraw-imm #x71 4)
  (define-imm-sse-instruction psrad-imm #x72 4)

  (define-imm-sse-instruction psrldq #x73 3)
  (define-imm-sse-instruction psrlw-imm #x71 2)
  (define-imm-sse-instruction psrld-imm #x72 2)
  (define-imm-sse-instruction psrlq-imm #x73 2))

;;; Emit an SSE instruction that has an XMM register as the destination
;;; operand and for which the size of the operands is implicitly given
;;; by the instruction.
(defun emit-regular-sse-inst (segment dst src prefix opcode
                              &key (remaining-bytes 0))
  (aver (xmm-register-p dst))
  (emit-sse-inst segment dst src prefix opcode
                 :operand-size :do-not-set
                 :remaining-bytes remaining-bytes))

(defun emit-regular-2byte-sse-inst (segment dst src prefix op1 op2
                                    &key (remaining-bytes 0))
  (aver (xmm-register-p dst))
  (emit-sse-inst-2byte segment dst src prefix op1 op2
                       :operand-size :do-not-set
                       :remaining-bytes remaining-bytes))

;;; Instructions having an XMM register as the destination operand
;;; and an XMM register or a memory location as the source operand.
;;; The operand size is implicitly given by the instruction.

(macrolet ((define-regular-sse-inst (name prefix opcode)
             `(define-instruction ,name (segment dst src)
                (:printer-list
                 ',(sse-inst-printer-list 'xmm-xmm/mem prefix opcode))
                (:emitter
                 (emit-regular-sse-inst segment dst src ,prefix ,opcode)))))
  ;; moves
  (define-regular-sse-inst movshdup #xf3 #x16)
  (define-regular-sse-inst movsldup #xf3 #x12)
  (define-regular-sse-inst movddup  #xf2 #x12)
  ;; logical
  (define-regular-sse-inst andpd    #x66 #x54)
  (define-regular-sse-inst andps    nil  #x54)
  (define-regular-sse-inst andnpd   #x66 #x55)
  (define-regular-sse-inst andnps   nil  #x55)
  (define-regular-sse-inst orpd     #x66 #x56)
  (define-regular-sse-inst orps     nil  #x56)
  (define-regular-sse-inst pand     #x66 #xdb)
  (define-regular-sse-inst pandn    #x66 #xdf)
  (define-regular-sse-inst por      #x66 #xeb)
  (define-regular-sse-inst pxor     #x66 #xef)
  (define-regular-sse-inst xorpd    #x66 #x57)
  (define-regular-sse-inst xorps    nil  #x57)
  ;; comparison
  (define-regular-sse-inst comisd   #x66 #x2f)
  (define-regular-sse-inst comiss   nil  #x2f)
  (define-regular-sse-inst ucomisd  #x66 #x2e)
  (define-regular-sse-inst ucomiss  nil  #x2e)
  ;; integer comparison
  (define-regular-sse-inst pcmpeqb  #x66 #x74)
  (define-regular-sse-inst pcmpeqw  #x66 #x75)
  (define-regular-sse-inst pcmpeqd  #x66 #x76)
  (define-regular-sse-inst pcmpgtb  #x66 #x64)
  (define-regular-sse-inst pcmpgtw  #x66 #x65)
  (define-regular-sse-inst pcmpgtd  #x66 #x66)
  ;; max/min
  (define-regular-sse-inst maxpd    #x66 #x5f)
  (define-regular-sse-inst maxps    nil  #x5f)
  (define-regular-sse-inst maxsd    #xf2 #x5f)
  (define-regular-sse-inst maxss    #xf3 #x5f)
  (define-regular-sse-inst minpd    #x66 #x5d)
  (define-regular-sse-inst minps    nil  #x5d)
  (define-regular-sse-inst minsd    #xf2 #x5d)
  (define-regular-sse-inst minss    #xf3 #x5d)
  ;; integer max/min
  (define-regular-sse-inst pmaxsw   #x66 #xee)
  (define-regular-sse-inst pmaxub   #x66 #xde)
  (define-regular-sse-inst pminsw   #x66 #xea)
  (define-regular-sse-inst pminub   #x66 #xda)
  ;; arithmetic
  (define-regular-sse-inst addpd    #x66 #x58)
  (define-regular-sse-inst addps    nil  #x58)
  (define-regular-sse-inst addsd    #xf2 #x58)
  (define-regular-sse-inst addss    #xf3 #x58)
  (define-regular-sse-inst addsubpd #x66 #xd0)
  (define-regular-sse-inst addsubps #xf2 #xd0)
  (define-regular-sse-inst divpd    #x66 #x5e)
  (define-regular-sse-inst divps    nil  #x5e)
  (define-regular-sse-inst divsd    #xf2 #x5e)
  (define-regular-sse-inst divss    #xf3 #x5e)
  (define-regular-sse-inst haddpd   #x66 #x7c)
  (define-regular-sse-inst haddps   #xf2 #x7c)
  (define-regular-sse-inst hsubpd   #x66 #x7d)
  (define-regular-sse-inst hsubps   #xf2 #x7d)
  (define-regular-sse-inst mulpd    #x66 #x59)
  (define-regular-sse-inst mulps    nil  #x59)
  (define-regular-sse-inst mulsd    #xf2 #x59)
  (define-regular-sse-inst mulss    #xf3 #x59)
  (define-regular-sse-inst rcpps    nil  #x53)
  (define-regular-sse-inst rcpss    #xf3 #x53)
  (define-regular-sse-inst rsqrtps  nil  #x52)
  (define-regular-sse-inst rsqrtss  #xf3 #x52)
  (define-regular-sse-inst sqrtpd   #x66 #x51)
  (define-regular-sse-inst sqrtps   nil  #x51)
  (define-regular-sse-inst sqrtsd   #xf2 #x51)
  (define-regular-sse-inst sqrtss   #xf3 #x51)
  (define-regular-sse-inst subpd    #x66 #x5c)
  (define-regular-sse-inst subps    nil  #x5c)
  (define-regular-sse-inst subsd    #xf2 #x5c)
  (define-regular-sse-inst subss    #xf3 #x5c)
  (define-regular-sse-inst unpckhpd #x66 #x15)
  (define-regular-sse-inst unpckhps nil  #x15)
  (define-regular-sse-inst unpcklpd #x66 #x14)
  (define-regular-sse-inst unpcklps nil  #x14)
  ;; integer arithmetic
  (define-regular-sse-inst paddb    #x66 #xfc)
  (define-regular-sse-inst paddw    #x66 #xfd)
  (define-regular-sse-inst paddd    #x66 #xfe)
  (define-regular-sse-inst paddq    #x66 #xd4)
  (define-regular-sse-inst paddsb   #x66 #xec)
  (define-regular-sse-inst paddsw   #x66 #xed)
  (define-regular-sse-inst paddusb  #x66 #xdc)
  (define-regular-sse-inst paddusw  #x66 #xdd)
  (define-regular-sse-inst pavgb    #x66 #xe0)
  (define-regular-sse-inst pavgw    #x66 #xe3)
  (define-regular-sse-inst pmaddwd  #x66 #xf5)
  (define-regular-sse-inst pmulhuw  #x66 #xe4)
  (define-regular-sse-inst pmulhw   #x66 #xe5)
  (define-regular-sse-inst pmullw   #x66 #xd5)
  (define-regular-sse-inst pmuludq  #x66 #xf4)
  (define-regular-sse-inst psadbw   #x66 #xf6)
  (define-regular-sse-inst psllw    #x66 #xf1)
  (define-regular-sse-inst pslld    #x66 #xf2)
  (define-regular-sse-inst psllq    #x66 #xf3)
  (define-regular-sse-inst psraw    #x66 #xe1)
  (define-regular-sse-inst psrad    #x66 #xe2)
  (define-regular-sse-inst psrlw    #x66 #xd1)
  (define-regular-sse-inst psrld    #x66 #xd2)
  (define-regular-sse-inst psrlq    #x66 #xd3)
  (define-regular-sse-inst psubb    #x66 #xf8)
  (define-regular-sse-inst psubw    #x66 #xf9)
  (define-regular-sse-inst psubd    #x66 #xfa)
  (define-regular-sse-inst psubq    #x66 #xfb)
  (define-regular-sse-inst psubsb   #x66 #xe8)
  (define-regular-sse-inst psubsw   #x66 #xe9)
  (define-regular-sse-inst psubusb  #x66 #xd8)
  (define-regular-sse-inst psubusw  #x66 #xd9)
  ;; conversion
  (define-regular-sse-inst cvtdq2pd #xf3 #xe6)
  (define-regular-sse-inst cvtdq2ps nil  #x5b)
  (define-regular-sse-inst cvtpd2dq #xf2 #xe6)
  (define-regular-sse-inst cvtpd2ps #x66 #x5a)
  (define-regular-sse-inst cvtps2dq #x66 #x5b)
  (define-regular-sse-inst cvtps2pd nil  #x5a)
  (define-regular-sse-inst cvtsd2ss #xf2 #x5a)
  (define-regular-sse-inst cvtss2sd #xf3 #x5a)
  (define-regular-sse-inst cvttpd2dq #x66 #xe6)
  (define-regular-sse-inst cvttps2dq #xf3 #x5b)
  ;; integer
  (define-regular-sse-inst packsswb  #x66 #x63)
  (define-regular-sse-inst packssdw  #x66 #x6b)
  (define-regular-sse-inst packuswb  #x66 #x67)
  (define-regular-sse-inst punpckhbw #x66 #x68)
  (define-regular-sse-inst punpckhwd #x66 #x69)
  (define-regular-sse-inst punpckhdq #x66 #x6a)
  (define-regular-sse-inst punpckhqdq #x66 #x6d)
  (define-regular-sse-inst punpcklbw #x66 #x60)
  (define-regular-sse-inst punpcklwd #x66 #x61)
  (define-regular-sse-inst punpckldq #x66 #x62)
  (define-regular-sse-inst punpcklqdq #x66 #x6c))

(macrolet ((define-xmm-shuffle-sse-inst (name prefix opcode n-bits radix)
               (let ((shuffle-pattern
                      (intern (format nil "SSE-SHUFFLE-PATTERN-~D-~D"
                                      n-bits radix))))
                 `(define-instruction ,name (segment dst src pattern)
                    (:printer-list
                     ',(sse-inst-printer-list
                        'xmm-xmm/mem prefix opcode
                        :more-fields `((imm nil :type ,shuffle-pattern))
                        :printer '(:name :tab reg ", " reg/mem ", " imm)))

                    (:emitter
                     (aver (typep pattern '(unsigned-byte ,n-bits)))
                     (emit-regular-sse-inst segment dst src ,prefix ,opcode
                                            :remaining-bytes 1)
                     (emit-byte segment pattern))))))
  (define-xmm-shuffle-sse-inst pshufd  #x66 #x70 8 4)
  (define-xmm-shuffle-sse-inst pshufhw #xf3 #x70 8 4)
  (define-xmm-shuffle-sse-inst pshuflw #xf2 #x70 8 4)
  (define-xmm-shuffle-sse-inst shufpd  #x66 #xc6 2 2)
  (define-xmm-shuffle-sse-inst shufps  nil  #xc6 8 4))

;; MASKMOVDQU (dst is DS:RDI)
(define-instruction maskmovdqu (segment src mask)
  (:printer-list
   (sse-inst-printer-list 'xmm-xmm/mem #x66 #xf7))
  (:emitter
   (aver (xmm-register-p src))
   (aver (xmm-register-p mask))
   (emit-regular-sse-inst segment src mask #x66 #xf7)))

(macrolet ((define-comparison-sse-inst (name prefix opcode
                                        name-prefix name-suffix)
               `(define-instruction ,name (segment op x y)
                  (:printer-list
                   ',(sse-inst-printer-list
                      'xmm-xmm/mem prefix opcode
                      :more-fields '((imm nil :type sse-condition-code))
                      :printer `(,name-prefix imm ,name-suffix
                                 :tab reg ", " reg/mem)))
                  (:emitter
                   (let ((code (position op *sse-conditions*)))
                     (aver code)
                     (emit-regular-sse-inst segment x y ,prefix ,opcode
                                            :remaining-bytes 1)
                     (emit-byte segment code))))))
  (define-comparison-sse-inst cmppd #x66 #xc2 "CMP" "PD")
  (define-comparison-sse-inst cmpps nil  #xc2 "CMP" "PS")
  (define-comparison-sse-inst cmpsd #xf2 #xc2 "CMP" "SD")
  (define-comparison-sse-inst cmpss #xf3 #xc2 "CMP" "SS"))

;;; MOVSD, MOVSS
(macrolet ((define-movsd/ss-sse-inst (name prefix)
             `(define-instruction ,name (segment dst src)
                (:printer-list
                 ',(sse-inst-printer-list 'xmm-xmm/mem-dir
                                          prefix #b0001000))
                (:emitter
                 (cond ((xmm-register-p dst)
                        (emit-sse-inst segment dst src ,prefix #x10
                                       :operand-size :do-not-set))
                       (t
                        (aver (xmm-register-p src))
                        (emit-sse-inst segment src dst ,prefix #x11
                                       :operand-size :do-not-set)))))))
  (define-movsd/ss-sse-inst movsd #xf2)
  (define-movsd/ss-sse-inst movss #xf3))

;;; Packed MOVs
(macrolet ((define-mov-sse-inst (name prefix opcode-from opcode-to
                                      &key force-to-mem reg-reg-name)
               `(progn
                  ,(when reg-reg-name
                     `(define-instruction ,reg-reg-name (segment dst src)
                        (:emitter
                         (aver (xmm-register-p dst))
                         (aver (xmm-register-p src))
                         (emit-regular-sse-inst segment dst src
                                                ,prefix ,opcode-from))))
                  (define-instruction ,name (segment dst src)
                    (:printer-list
                     '(,@(when opcode-from
                           (sse-inst-printer-list
                            'xmm-xmm/mem prefix opcode-from))
                       ,@(sse-inst-printer-list
                          'xmm-xmm/mem prefix opcode-to
                          :printer '(:name :tab reg/mem ", " reg))))
                    (:emitter
                     (cond ,@(when opcode-from
                               `(((xmm-register-p dst)
                                  ,(when force-to-mem
                                     `(aver (not (or (register-p src)
                                                     (xmm-register-p src)))))
                                  (emit-regular-sse-inst
                                   segment dst src ,prefix ,opcode-from))))
                           (t
                            (aver (xmm-register-p src))
                            ,(when force-to-mem
                               `(aver (not (or (register-p dst)
                                               (xmm-register-p dst)))))
                            (emit-regular-sse-inst segment src dst
                                                   ,prefix ,opcode-to))))))))
  ;; direction bit?
  (define-mov-sse-inst movapd #x66 #x28 #x29)
  (define-mov-sse-inst movaps nil  #x28 #x29)
  (define-mov-sse-inst movdqa #x66 #x6f #x7f)
  (define-mov-sse-inst movdqu #xf3 #x6f #x7f)

  ;; streaming
  (define-mov-sse-inst movntdq #x66 nil #xe7 :force-to-mem t)
  (define-mov-sse-inst movntpd #x66 nil #x2b :force-to-mem t)
  (define-mov-sse-inst movntps nil  nil #x2b :force-to-mem t)

  ;; use movhps for movlhps and movlps for movhlps
  (define-mov-sse-inst movhpd #x66 #x16 #x17 :force-to-mem t)
  (define-mov-sse-inst movhps nil  #x16 #x17 :reg-reg-name movlhps)
  (define-mov-sse-inst movlpd #x66 #x12 #x13 :force-to-mem t)
  (define-mov-sse-inst movlps nil  #x12 #x13 :reg-reg-name movhlps)
  (define-mov-sse-inst movupd #x66 #x10 #x11)
  (define-mov-sse-inst movups nil  #x10 #x11))

;;; MOVNTDQA
(define-instruction movntdqa (segment dst src)
  (:printer-list
   (2byte-sse-inst-printer-list '2byte-xmm-xmm/mem #x66 #x38 #x2a))
  (:emitter
   (aver (and (xmm-register-p dst)
              (not (xmm-register-p src))))
   (emit-regular-2byte-sse-inst segment dst src #x66 #x38 #x2a)))

;;; MOVQ
(define-instruction movq (segment dst src)
  (:printer-list
   (append
    (sse-inst-printer-list 'xmm-xmm/mem #xf3 #x7e)
    (sse-inst-printer-list 'xmm-xmm/mem #x66 #xd6
                           :printer '(:name :tab reg/mem ", " reg))))
  (:emitter
   (cond ((xmm-register-p dst)
          (emit-sse-inst segment dst src #xf3 #x7e
                         :operand-size :do-not-set))
         (t
          (aver (xmm-register-p src))
          (emit-sse-inst segment src dst #x66 #xd6
                         :operand-size :do-not-set)))))

;;; Instructions having an XMM register as the destination operand
;;; and a general-purpose register or a memory location as the source
;;; operand. The operand size is calculated from the source operand.

;;; MOVD - Move a 32- or 64-bit value from a general-purpose register or
;;; a memory location to the low order 32 or 64 bits of an XMM register
;;; with zero extension or vice versa.
;;; We do not support the MMX version of this instruction.
(define-instruction movd (segment dst src)
  (:printer-list
   (append
    (sse-inst-printer-list 'xmm-reg/mem #x66 #x6e)
    (sse-inst-printer-list 'xmm-reg/mem #x66 #x7e
                           :printer '(:name :tab reg/mem ", " reg))))
  (:emitter
   (cond ((xmm-register-p dst)
          (emit-sse-inst segment dst src #x66 #x6e))
         (t
          (aver (xmm-register-p src))
          (emit-sse-inst segment src dst #x66 #x7e)))))

(macrolet ((define-extract-sse-instruction (name prefix op1 op2
                                            &key explicit-qword)
             `(define-instruction ,name (segment dst src imm)
                (:printer
                 ,(if op2 (if explicit-qword
                              'ext-rex-2byte-reg/mem-xmm
                              'ext-2byte-reg/mem-xmm)
                      'ext-reg/mem-xmm)
                 ((prefix '(,prefix))
                  ,@(if op2
                        `((op1 '(,op1)) (op2 '(,op2)))
                        `((op '(,op1))))
                  (imm nil :type 'imm-byte))
                 '(:name :tab reg/mem ", " reg ", " imm))
                (:emitter
                 (aver (and (xmm-register-p src) (not (xmm-register-p dst))))
                 ,(if op2
                      `(emit-sse-inst-2byte segment dst src ,prefix ,op1 ,op2
                                            :operand-size ,(if explicit-qword
                                                               :qword
                                                               :do-not-set)
                                            :remaining-bytes 1)
                      `(emit-sse-inst segment dst src ,prefix ,op1
                                      :operand-size ,(if explicit-qword
                                                         :qword
                                                         :do-not-set)
                                      :remaining-bytes 1))
                 (emit-byte segment imm))))

           (define-insert-sse-instruction (name prefix op1 op2)
             `(define-instruction ,name (segment dst src imm)
                (:printer
                 ,(if op2 'ext-2byte-xmm-reg/mem 'ext-xmm-reg/mem)
                 ((prefix '(,prefix))
                  ,@(if op2
                        `((op1 '(,op1)) (op2 '(,op2)))
                        `((op '(,op1))))
                  (imm nil :type 'imm-byte))
                 '(:name :tab reg ", " reg/mem ", " imm))
                (:emitter
                 (aver (and (xmm-register-p dst) (not (xmm-register-p src))))
                 ,(if op2
                      `(emit-sse-inst-2byte segment dst src ,prefix ,op1 ,op2
                                            :operand-size :do-not-set
                                            :remaining-bytes 1)
                      `(emit-sse-inst segment dst src ,prefix ,op1
                                      :operand-size :do-not-set
                                      :remaining-bytes 1))
                 (emit-byte segment imm)))))


  ;; pinsrq not encodable in 64-bit mode
  (define-insert-sse-instruction pinsrb #x66 #x3a #x20)
  (define-insert-sse-instruction pinsrw #x66 #xc4 nil)
  (define-insert-sse-instruction pinsrd #x66 #x3a #x22)
  (define-insert-sse-instruction insertps #x66 #x3a #x21)

  (define-extract-sse-instruction pextrb #x66 #x3a #x14)
  (define-extract-sse-instruction pextrd #x66 #x3a #x16)
  (define-extract-sse-instruction pextrq #x66 #x3a #x16 :explicit-qword t)
  (define-extract-sse-instruction extractps #x66 #x3a #x17))

;; PEXTRW has a new 2-byte encoding in SSE4.1 to allow dst to be
;; a memory address.
(define-instruction pextrw (segment dst src imm)
  (:printer-list
   (append
    (2byte-sse-inst-printer-list '2byte-reg/mem-xmm #x66 #x3a #x15
                                 :more-fields '((imm nil :type imm-byte))
                                 :printer
                                 '(:name :tab reg/mem ", " reg ", " imm))
    (sse-inst-printer-list 'reg/mem-xmm #x66 #xc5
                           :more-fields '((imm nil :type imm-byte))
                           :printer
                           '(:name :tab reg/mem ", " reg ", " imm))))
  (:emitter
   (aver (xmm-register-p src))
   (if (not (register-p dst))
       (emit-sse-inst-2byte segment dst src #x66 #x3a #x15
                            :operand-size :do-not-set :remaining-bytes 1)
       (emit-sse-inst segment dst src #x66 #xc5
                            :operand-size :do-not-set :remaining-bytes 1))
   (emit-byte segment imm)))

(macrolet ((define-integer-source-sse-inst (name prefix opcode &key mem-only)
             `(define-instruction ,name (segment dst src)
                (:printer-list
                 ',(sse-inst-printer-list 'xmm-reg/mem prefix opcode))
                (:emitter
                 (aver (xmm-register-p dst))
                 ,(when mem-only
                    `(aver (not (or (register-p src)
                                    (xmm-register-p src)))))
                 (let ((src-size (operand-size src)))
                   (aver (or (eq src-size :qword) (eq src-size :dword))))
                 (emit-sse-inst segment dst src ,prefix ,opcode)))))
  (define-integer-source-sse-inst cvtsi2sd #xf2 #x2a)
  (define-integer-source-sse-inst cvtsi2ss #xf3 #x2a)
  ;; FIXME: memory operand is always a QWORD
  (define-integer-source-sse-inst cvtpi2pd #x66 #x2a :mem-only t)
  (define-integer-source-sse-inst cvtpi2ps nil  #x2a :mem-only t))

;;; Instructions having a general-purpose register as the destination
;;; operand and an XMM register or a memory location as the source
;;; operand. The operand size is calculated from the destination
;;; operand.

(macrolet ((define-gpr-destination-sse-inst (name prefix opcode &key reg-only)
             `(define-instruction ,name (segment dst src)
                (:printer-list
                 ',(sse-inst-printer-list 'reg-xmm/mem prefix opcode))
                (:emitter
                 (aver (register-p dst))
                 ,(when reg-only
                    `(aver (xmm-register-p src)))
                 (let ((dst-size (operand-size dst)))
                   (aver (or (eq dst-size :qword) (eq dst-size :dword)))
                   (emit-sse-inst segment dst src ,prefix ,opcode
                                  :operand-size dst-size))))))
  (define-gpr-destination-sse-inst cvtsd2si  #xf2 #x2d)
  (define-gpr-destination-sse-inst cvtss2si  #xf3 #x2d)
  (define-gpr-destination-sse-inst cvttsd2si #xf2 #x2c)
  (define-gpr-destination-sse-inst cvttss2si #xf3 #x2c)
  (define-gpr-destination-sse-inst movmskpd  #x66 #x50 :reg-only t)
  (define-gpr-destination-sse-inst movmskps  nil  #x50 :reg-only t)
  (define-gpr-destination-sse-inst pmovmskb  #x66 #xd7 :reg-only t))

;;;; We call these "2byte" instructions due to their two opcode bytes.
;;;; Intel and AMD call them three-byte instructions, as they count the
;;;; 0x0f byte for determining the number of opcode bytes.

;;; Instructions that take XMM-XMM/MEM and XMM-XMM/MEM-IMM arguments.

(macrolet ((regular-2byte-sse-inst (name prefix op1 op2)
             `(define-instruction ,name (segment dst src)
                (:printer-list
                 ',(2byte-sse-inst-printer-list '2byte-xmm-xmm/mem prefix
                                                op1 op2))
                (:emitter
                 (emit-regular-2byte-sse-inst segment dst src ,prefix
                                              ,op1 ,op2))))
           (regular-2byte-sse-inst-imm (name prefix op1 op2)
             `(define-instruction ,name (segment dst src imm)
                (:printer-list
                 ',(2byte-sse-inst-printer-list
                    '2byte-xmm-xmm/mem prefix op1 op2
                    :more-fields '((imm nil :type imm-byte))
                    :printer `(:name :tab reg ", " reg/mem ", " imm)))
                (:emitter
                 (aver (typep imm '(unsigned-byte 8)))
                 (emit-regular-2byte-sse-inst segment dst src ,prefix ,op1 ,op2
                                              :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (regular-2byte-sse-inst pshufb #x66 #x38 #x00)
  (regular-2byte-sse-inst phaddw #x66 #x38 #x01)
  (regular-2byte-sse-inst phaddd #x66 #x38 #x02)
  (regular-2byte-sse-inst phaddsw #x66 #x38 #x03)
  (regular-2byte-sse-inst pmaddubsw #x66 #x38 #x04)
  (regular-2byte-sse-inst phsubw #x66 #x38 #x05)
  (regular-2byte-sse-inst phsubd #x66 #x38 #x06)
  (regular-2byte-sse-inst phsubsw #x66 #x38 #x07)
  (regular-2byte-sse-inst psignb #x66 #x38 #x08)
  (regular-2byte-sse-inst psignw #x66 #x38 #x09)
  (regular-2byte-sse-inst psignd #x66 #x38 #x0a)
  (regular-2byte-sse-inst pmulhrsw #x66 #x38 #x0b)

  (regular-2byte-sse-inst ptest #x66 #x38 #x17)
  (regular-2byte-sse-inst pabsb #x66 #x38 #x1c)
  (regular-2byte-sse-inst pabsw #x66 #x38 #x1d)
  (regular-2byte-sse-inst pabsd #x66 #x38 #x1e)

  (regular-2byte-sse-inst pmuldq #x66 #x38 #x28)
  (regular-2byte-sse-inst pcmpeqq #x66 #x38 #x29)
  (regular-2byte-sse-inst packusdw #x66 #x38 #x2b)

  (regular-2byte-sse-inst pcmpgtq #x66 #x38 #x37)
  (regular-2byte-sse-inst pminsb #x66 #x38 #x38)
  (regular-2byte-sse-inst pminsd #x66 #x38 #x39)
  (regular-2byte-sse-inst pminuw #x66 #x38 #x3a)
  (regular-2byte-sse-inst pminud #x66 #x38 #x3b)
  (regular-2byte-sse-inst pmaxsb #x66 #x38 #x3c)
  (regular-2byte-sse-inst pmaxsd #x66 #x38 #x3d)
  (regular-2byte-sse-inst pmaxuw #x66 #x38 #x3e)
  (regular-2byte-sse-inst pmaxud #x66 #x38 #x3f)

  (regular-2byte-sse-inst pmulld #x66 #x38 #x40)
  (regular-2byte-sse-inst phminposuw #x66 #x38 #x41)

  (regular-2byte-sse-inst aesimc #x66 #x38 #xdb)
  (regular-2byte-sse-inst aesenc #x66 #x38 #xdc)
  (regular-2byte-sse-inst aesenclast #x66 #x38 #xdd)
  (regular-2byte-sse-inst aesdec #x66 #x38 #xde)
  (regular-2byte-sse-inst aesdeclast #x66 #x38 #xdf)

  (regular-2byte-sse-inst pmovsxbw #x66 #x38 #x20)
  (regular-2byte-sse-inst pmovsxbd #x66 #x38 #x21)
  (regular-2byte-sse-inst pmovsxbq #x66 #x38 #x22)
  (regular-2byte-sse-inst pmovsxwd #x66 #x38 #x23)
  (regular-2byte-sse-inst pmovsxwq #x66 #x38 #x24)
  (regular-2byte-sse-inst pmovsxdq #x66 #x38 #x25)

  (regular-2byte-sse-inst pmovzxbw #x66 #x38 #x30)
  (regular-2byte-sse-inst pmovzxbd #x66 #x38 #x31)
  (regular-2byte-sse-inst pmovzxbq #x66 #x38 #x32)
  (regular-2byte-sse-inst pmovzxwd #x66 #x38 #x33)
  (regular-2byte-sse-inst pmovzxwq #x66 #x38 #x34)
  (regular-2byte-sse-inst pmovzxdq #x66 #x38 #x35)

  (regular-2byte-sse-inst-imm roundps #x66 #x3a #x08)
  (regular-2byte-sse-inst-imm roundpd #x66 #x3a #x09)
  (regular-2byte-sse-inst-imm roundss #x66 #x3a #x0a)
  (regular-2byte-sse-inst-imm roundsd #x66 #x3a #x0b)
  (regular-2byte-sse-inst-imm blendps #x66 #x3a #x0c)
  (regular-2byte-sse-inst-imm blendpd #x66 #x3a #x0d)
  (regular-2byte-sse-inst-imm pblendw #x66 #x3a #x0e)
  (regular-2byte-sse-inst-imm palignr #x66 #x3a #x0f)
  (regular-2byte-sse-inst-imm dpps    #x66 #x3a #x40)
  (regular-2byte-sse-inst-imm dppd    #x66 #x3a #x41)

  (regular-2byte-sse-inst-imm mpsadbw #x66 #x3a #x42)
  (regular-2byte-sse-inst-imm pclmulqdq #x66 #x3a #x44)

  (regular-2byte-sse-inst-imm pcmpestrm #x66 #x3a #x60)
  (regular-2byte-sse-inst-imm pcmpestri #x66 #x3a #x61)
  (regular-2byte-sse-inst-imm pcmpistrm #x66 #x3a #x62)
  (regular-2byte-sse-inst-imm pcmpistri #x66 #x3a #x63)

  (regular-2byte-sse-inst-imm aeskeygenassist #x66 #x3a #xdf))

;;; Other SSE instructions

;; Instructions implicitly using XMM0 as a mask
(macrolet ((define-sse-inst-implicit-mask (name prefix op1 op2)
             `(define-instruction ,name (segment dst src mask)
                (:printer-list
                 ',(2byte-sse-inst-printer-list
                    '2byte-xmm-xmm/mem prefix op1 op2
                    :printer '(:name :tab reg ", " reg/mem ", XMM0")))
                (:emitter
                 (aver (xmm-register-p dst))
                 (aver (and (xmm-register-p mask) (= (tn-offset mask) 0)))
                 (emit-regular-2byte-sse-inst segment dst src ,prefix
                                              ,op1 ,op2)))))

  (define-sse-inst-implicit-mask pblendvb #x66 #x38 #x10)
  (define-sse-inst-implicit-mask blendvps #x66 #x38 #x14)
  (define-sse-inst-implicit-mask blendvpd #x66 #x38 #x15))

(define-instruction movnti (segment dst src)
  (:printer ext-reg-reg/mem-no-width ((op #xc3)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (aver (not (or (register-p dst)
                  (xmm-register-p dst))))
   (aver (register-p src))
   (maybe-emit-rex-for-ea segment dst src)
   (emit-byte segment #x0f)
   (emit-byte segment #xc3)
   (emit-ea segment dst (reg-tn-encoding src))))

(define-instruction prefetch (segment type src)
  (:printer ext-reg/mem-no-width ((op '(#x18 0)))
            '("PREFETCHNTA" :tab reg/mem))
  (:printer ext-reg/mem-no-width ((op '(#x18 1)))
            '("PREFETCHT0" :tab reg/mem))
  (:printer ext-reg/mem-no-width ((op '(#x18 2)))
            '("PREFETCHT1" :tab reg/mem))
  (:printer ext-reg/mem-no-width ((op '(#x18 3)))
            '("PREFETCHT2" :tab reg/mem))
  (:emitter
   (aver (not (or (register-p src)
                  (xmm-register-p src))))
   (aver (eq (operand-size src) :byte))
   (let ((type (position type #(:nta :t0 :t1 :t2))))
     (aver type)
     (maybe-emit-rex-for-ea segment src nil)
     (emit-byte segment #x0f)
     (emit-byte segment #x18)
     (emit-ea segment src type))))

(define-instruction clflush (segment src)
  (:printer ext-reg/mem-no-width ((op '(#xae 7))))
  (:emitter
   (aver (not (or (register-p src)
                  (xmm-register-p src))))
   (aver (eq (operand-size src) :byte))
   (maybe-emit-rex-for-ea segment src nil)
   (emit-byte segment #x0f)
   (emit-byte segment #xae)
   (emit-ea segment src 7)))

(macrolet ((define-fence-instruction (name last-byte)
               `(define-instruction ,name (segment)
                  (:printer three-bytes ((op '(#x0f #xae ,last-byte))))
                  (:emitter
                   (emit-byte segment #x0f)
                   (emit-byte segment #xae)
                   (emit-byte segment ,last-byte)))))
  (define-fence-instruction lfence #b11101000)
  (define-fence-instruction mfence #b11110000)
  (define-fence-instruction sfence #b11111000))

(define-instruction pause (segment)
  (:printer two-bytes ((op '(#xf3 #x90))))
  (:emitter
   (emit-byte segment #xf3)
   (emit-byte segment #x90)))

(define-instruction ldmxcsr (segment src)
  (:printer ext-reg/mem-no-width ((op '(#xae 2))))
  (:emitter
   (aver (not (or (register-p src)
                  (xmm-register-p src))))
   (aver (eq (operand-size src) :dword))
   (maybe-emit-rex-for-ea segment src nil)
   (emit-byte segment #x0f)
   (emit-byte segment #xae)
   (emit-ea segment src 2)))

(define-instruction stmxcsr (segment dst)
  (:printer ext-reg/mem-no-width ((op '(#xae 3))))
  (:emitter
   (aver (not (or (register-p dst)
                  (xmm-register-p dst))))
   (aver (eq (operand-size dst) :dword))
   (maybe-emit-rex-for-ea segment dst nil)
   (emit-byte segment #x0f)
   (emit-byte segment #xae)
   (emit-ea segment dst 3)))

(define-instruction popcnt (segment dst src)
  (:printer-list `((f3-escape-reg-reg/mem ((op #xB8)))
                   (rex-f3-escape-reg-reg/mem ((op #xB8)))))
  (:emitter
   (aver (register-p dst))
   (aver (and (register-p dst) (not (eq (operand-size dst) :byte))))
   (aver (not (eq (operand-size src) :byte)))
   (emit-sse-inst segment dst src #xf3 #xb8)))

(define-instruction crc32 (segment dst src)
  (:printer-list
   `(,@(mapcan (lambda (op2)
                 (mapcar (lambda (instfmt)
                           `(,instfmt ((prefix (#xf2)) (op1 (#x38))
                                       (op2 (,op2)))))
                         '(ext-rex-2byte-prefix-reg-reg/mem
                           ext-2byte-prefix-reg-reg/mem)))
               '(#xf0 #xf1))))
  (:emitter
   (let ((dst-size (operand-size dst)))
     (aver (and (register-p dst) (not (or (eq dst-size :word)
                                          (eq dst-size :byte)))))
     (if (eq (operand-size src) :byte)
         (emit-sse-inst-2byte segment dst src #xf2 #x38 #xf0)
         (emit-sse-inst-2byte segment dst src #xf2 #x38 #xf1)))))

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

(defun canonicalize-inline-constant (constant &aux (alignedp nil))
  (let ((first (car constant)))
    (when (eql first :aligned)
      (setf alignedp t)
      (pop constant)
      (setf first (car constant)))
    (typecase first
      (single-float (setf constant (list :single-float first)))
      (double-float (setf constant (list :double-float first)))
      .
      #+sb-xc-host
      ((complex
        ;; It's an error (perhaps) on the host to use simd-pack type.
        ;; [and btw it's disconcerting that this isn't an ETYPECASE.]
        (error "xc-host can't reference complex float")))
      #-sb-xc-host
      (((complex single-float)
        (setf constant (list :complex-single-float first)))
       ((complex double-float)
        (setf constant (list :complex-double-float first)))
       #!+sb-simd-pack
       (simd-pack
        (setq constant
              (list :sse (logior (%simd-pack-low first)
                                 (ash (%simd-pack-high first) 64))))))))
  (destructuring-bind (type value) constant
    (ecase type
      ((:byte :word :dword :qword)
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
         (cons (if alignedp :oword :dword)
               (ldb (byte 32 0) (single-float-bits value))))
      ((:double-float)
         (aver (typep value 'double-float))
         (cons (if alignedp :oword :qword)
               (ldb (byte 64 0) (logior (ash (double-float-high-bits value) 32)
                                        (double-float-low-bits value)))))
      ((:complex-single-float)
         (aver (typep value '(complex single-float)))
         (cons (if alignedp :oword :qword)
               (ldb (byte 64 0)
                    (logior (ash (single-float-bits (imagpart value)) 32)
                            (ldb (byte 32 0)
                                 (single-float-bits (realpart value)))))))
      ((:oword :sse)
         (aver (integerp value))
         (cons :oword value))
      ((:complex-double-float)
         (aver (typep value '(complex double-float)))
         (cons :oword
               (logior (ash (double-float-high-bits (imagpart value)) 96)
                       (ash (double-float-low-bits (imagpart value)) 64)
                       (ash (ldb (byte 32 0)
                                 (double-float-high-bits (realpart value)))
                            32)
                       (double-float-low-bits (realpart value))))))))

(defun inline-constant-value (constant)
  (let ((label (gen-label))
        (size  (ecase (car constant)
                 ((:byte :word :dword :qword) (car constant))
                 ((:oword) :qword))))
    (values label (make-ea size
                           :disp (make-fixup nil :code-object label)))))

(defun emit-constant-segment-header (segment constants optimize)
  (declare (ignore constants))
  (emit-long-nop segment (if optimize 64 16)))

(defun size-nbyte (size)
  (ecase size
    (:byte  1)
    (:word  2)
    (:dword 4)
    (:qword 8)
    (:oword 16)))

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
