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

(in-package "SB!VM")
;;; FIXME: SB!DISASSEM: prefixes are used so widely in this file that
;;; I wonder whether the separation of the disassembler from the
;;; virtual machine is valid or adds value.

;;; Note: In CMU CL, this used to be a call to SET-DISASSEM-PARAMS.
(setf sb!disassem:*disassem-inst-alignment-bytes* 1)

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
           (type sb!disassem:disassem-state dstate))
  (+ (sb!disassem:dstate-next-addr dstate) value))

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
  (declare (type sb!disassem:disassem-state dstate))
  (cond ((sb!disassem:dstate-get-inst-prop dstate 'operand-size-8)
         :byte)
        ((sb!disassem:dstate-get-inst-prop dstate 'rex-w)
         :qword)
        ((sb!disassem:dstate-get-inst-prop dstate 'operand-size-16)
         :word)
        (t
         +default-operand-size+)))

;;; The same as INST-OPERAND-SIZE, but for those instructions (e.g.
;;; PUSH, JMP) that have a default operand size of :qword. It can only
;;; be overwritten to :word.
(defun inst-operand-size-default-qword (dstate)
  (declare (type sb!disassem:disassem-state dstate))
  (if (sb!disassem:dstate-get-inst-prop dstate 'operand-size-16)
      :word
      :qword))

;;; Print to STREAM the name of the general-purpose register encoded by
;;; VALUE and of size WIDTH. For robustness, the high byte registers
;;; (AH, BH, CH, DH) are correctly detected, too, although the compiler
;;; does not use them.
(defun print-reg-with-width (value width stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (princ (if (and (eq width :byte)
                  (<= 4 value 7)
                  (not (sb!disassem:dstate-get-inst-prop dstate 'rex)))
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
           (type sb!disassem:disassem-state dstate))
  (print-reg-with-width value
                        (inst-operand-size dstate)
                        stream
                        dstate))

(defun print-reg-default-qword (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg-with-width value
                        (inst-operand-size-default-qword dstate)
                        stream
                        dstate))

(defun print-byte-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg-with-width value :byte stream dstate))

(defun print-addr-reg (value stream dstate)
  (declare (type full-reg value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg-with-width value +default-address-size+ stream dstate))

;;; Print a register or a memory reference of the given WIDTH.
;;; If SIZED-P is true, add an explicit size indicator for memory
;;; references.
(defun print-reg/mem-with-width (value width sized-p stream dstate)
  (declare (type (or list full-reg) value)
           (type (member :byte :word :dword :qword) width)
           (type boolean sized-p)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (if (typep value 'full-reg)
      (print-reg-with-width value width stream dstate)
    (print-mem-access value (and sized-p width) stream dstate)))

;;; Print a register or a memory reference. The width is determined by
;;; calling INST-OPERAND-SIZE.
(defun print-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size dstate) nil stream dstate))

;; Same as print-reg/mem, but prints an explicit size indicator for
;; memory references.
(defun print-sized-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size dstate) t stream dstate))

;;; Same as print-sized-reg/mem, but with a default operand size of
;;; :qword.
(defun print-sized-reg/mem-default-qword (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width
   value (inst-operand-size-default-qword dstate) t stream dstate))

(defun print-sized-byte-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width value :byte t stream dstate))

(defun print-sized-word-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width value :word t stream dstate))

(defun print-sized-dword-reg/mem (value stream dstate)
  (declare (type (or list full-reg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (print-reg/mem-with-width value :dword t stream dstate))

(defun print-label (value stream dstate)
  (declare (ignore dstate))
  (sb!disassem:princ16 value stream))

(defun print-xmmreg (value stream dstate)
  (declare (type xmmreg value)
           (type stream stream)
           (ignore dstate))
  (format stream "XMM~d" value))

(defun print-xmmreg/mem (value stream dstate)
  (declare (type (or list xmmreg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (if (typep value 'xmmreg)
      (print-xmmreg value stream dstate)
    (print-mem-access value nil stream dstate)))

;; Same as print-xmmreg/mem, but prints an explicit size indicator for
;; memory references.
(defun print-sized-xmmreg/mem (value stream dstate)
  (declare (type (or list xmmreg) value)
           (type stream stream)
           (type sb!disassem:disassem-state dstate))
  (if (typep value 'xmmreg)
      (print-xmmreg value stream dstate)
    (print-mem-access value (inst-operand-size dstate) stream dstate)))

;;; This prefilter is used solely for its side effects, namely to put
;;; the bits found in the REX prefix into the DSTATE for use by other
;;; prefilters and by printers.
(defun prefilter-wrxb (value dstate)
  (declare (type (unsigned-byte 4) value)
           (type sb!disassem:disassem-state dstate))
  (sb!disassem:dstate-put-inst-prop dstate 'rex)
  (when (plusp (logand value #b1000))
    (sb!disassem:dstate-put-inst-prop dstate 'rex-w))
  (when (plusp (logand value #b0100))
    (sb!disassem:dstate-put-inst-prop dstate 'rex-r))
  (when (plusp (logand value #b0010))
    (sb!disassem:dstate-put-inst-prop dstate 'rex-x))
  (when (plusp (logand value #b0001))
    (sb!disassem:dstate-put-inst-prop dstate 'rex-b))
  value)

;;; This prefilter is used solely for its side effect, namely to put
;;; the property OPERAND-SIZE-8 into the DSTATE if VALUE is 0.
(defun prefilter-width (value dstate)
  (declare (type bit value)
           (type sb!disassem:disassem-state dstate))
  (when (zerop value)
    (sb!disassem:dstate-put-inst-prop dstate 'operand-size-8))
  value)

;;; This prefilter is used solely for its side effect, namely to put
;;; the property OPERAND-SIZE-16 into the DSTATE.
(defun prefilter-x66 (value dstate)
  (declare (type (eql #x66) value)
           (ignore value)
           (type sb!disassem:disassem-state dstate))
  (sb!disassem:dstate-put-inst-prop dstate 'operand-size-16))

;;; A register field that can be extended by REX.R.
(defun prefilter-reg-r (value dstate)
  (declare (type reg value)
           (type sb!disassem:disassem-state dstate))
  (if (sb!disassem::dstate-get-inst-prop dstate 'rex-r)
      (+ value 8)
      value))

;;; A register field that can be extended by REX.B.
(defun prefilter-reg-b (value dstate)
  (declare (type reg value)
           (type sb!disassem:disassem-state dstate))
  (if (sb!disassem::dstate-get-inst-prop dstate 'rex-b)
      (+ value 8)
      value))

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
           (type sb!disassem:disassem-state dstate))
  (let ((mod (first value))
        (r/m (second value)))
    (declare (type (unsigned-byte 2) mod)
             (type (unsigned-byte 3) r/m))
    (let ((full-reg (if (sb!disassem:dstate-get-inst-prop dstate 'rex-b)
                        (+ r/m 8)
                        r/m)))
      (declare (type full-reg full-reg))
      (cond ((= mod #b11)
             ;; registers
             full-reg)
            ((= r/m #b100)
             ;; sib byte
             (let ((sib (sb!disassem:read-suffix 8 dstate)))
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
                                    (sb!disassem:read-signed-suffix 32 dstate)
                                  nil))
                               (#b01
                                (sb!disassem:read-signed-suffix 8 dstate))
                               (#b10
                                (sb!disassem:read-signed-suffix 32 dstate)))))
                   (list (unless (and (= mod #b00) (= base-reg #b101))
                           (if (sb!disassem:dstate-get-inst-prop dstate 'rex-b)
                               (+ base-reg 8)
                               base-reg))
                         offset
                         (unless (= index-reg #b100)
                           (if (sb!disassem:dstate-get-inst-prop dstate 'rex-x)
                               (+ index-reg 8)
                               index-reg))
                         (ash 1 index-scale))))))
            ((and (= mod #b00) (= r/m #b101))
             (list 'rip (sb!disassem:read-signed-suffix 32 dstate)))
            ((= mod #b00)
             (list full-reg))
            ((= mod #b01)
           (list full-reg (sb!disassem:read-signed-suffix 8 dstate)))
          (t                            ; (= mod #b10)
           (list full-reg (sb!disassem:read-signed-suffix 32 dstate)))))))

(defun read-address (value dstate)
  (declare (ignore value))              ; always nil anyway
  (sb!disassem:read-suffix (width-bits (inst-operand-size dstate)) dstate))

(defun width-bits (width)
  (ecase width
    (:byte 8)
    (:word 16)
    (:dword 32)
    (:qword 64)))

) ; EVAL-WHEN

;;;; disassembler argument types

;;; Used to capture the lower four bits of the REX prefix.
(sb!disassem:define-arg-type wrxb
  :prefilter #'prefilter-wrxb)

(sb!disassem:define-arg-type width
  :prefilter #'prefilter-width
  :printer (lambda (value stream dstate)
             (declare (ignore value))
             (princ (schar (symbol-name (inst-operand-size dstate)) 0)
                    stream)))

;;; Used to capture the effect of the #x66 operand size override prefix.
(sb!disassem:define-arg-type x66
  :prefilter #'prefilter-x66)

(sb!disassem:define-arg-type displacement
  :sign-extend t
  :use-label #'offset-next
  :printer (lambda (value stream dstate)
             (sb!disassem:maybe-note-assembler-routine value nil dstate)
             (print-label value stream dstate)))

(sb!disassem:define-arg-type accum
  :printer (lambda (value stream dstate)
             (declare (ignore value)
                      (type stream stream)
                      (type sb!disassem:disassem-state dstate))
             (print-reg 0 stream dstate)))

(sb!disassem:define-arg-type reg
  :prefilter #'prefilter-reg-r
  :printer #'print-reg)

(sb!disassem:define-arg-type reg-b
  :prefilter #'prefilter-reg-b
  :printer #'print-reg)

(sb!disassem:define-arg-type reg-b-default-qword
  :prefilter #'prefilter-reg-b
  :printer #'print-reg-default-qword)

(sb!disassem:define-arg-type imm-addr
  :prefilter #'read-address
  :printer #'print-label)

;;; Normally, immediate values for an operand size of :qword are of size
;;; :dword and are sign-extended to 64 bits. For an exception, see the
;;; argument type definition following this one.
(sb!disassem:define-arg-type signed-imm-data
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (let ((width (width-bits (inst-operand-size dstate))))
                 (when (= width 64)
                   (setf width 32))
                 (sb!disassem:read-signed-suffix width dstate))))

;;; Used by the variant of the MOV instruction with opcode B8 which can
;;; move immediates of all sizes (i.e. including :qword) into a
;;; register.
(sb!disassem:define-arg-type signed-imm-data-upto-qword
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (sb!disassem:read-signed-suffix
                (width-bits (inst-operand-size dstate))
                dstate)))

;;; Used by those instructions that have a default operand size of
;;; :qword. Nevertheless the immediate is at most of size :dword.
;;; The only instruction of this kind having a variant with an immediate
;;; argument is PUSH.
(sb!disassem:define-arg-type signed-imm-data-default-qword
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (let ((width (width-bits
                             (inst-operand-size-default-qword dstate))))
                 (when (= width 64)
                   (setf width 32))
                 (sb!disassem:read-signed-suffix width dstate))))

(sb!disassem:define-arg-type signed-imm-byte
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (sb!disassem:read-signed-suffix 8 dstate)))

(sb!disassem:define-arg-type imm-byte
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (sb!disassem:read-suffix 8 dstate)))

;;; needed for the ret imm16 instruction
(sb!disassem:define-arg-type imm-word-16
  :prefilter (lambda (value dstate)
               (declare (ignore value)) ; always nil anyway
               (sb!disassem:read-suffix 16 dstate)))

(sb!disassem:define-arg-type reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-reg/mem)
(sb!disassem:define-arg-type sized-reg/mem
  ;; Same as reg/mem, but prints an explicit size indicator for
  ;; memory references.
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-reg/mem)

;;; Arguments of type reg/mem with a fixed size.
(sb!disassem:define-arg-type sized-byte-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-byte-reg/mem)
(sb!disassem:define-arg-type sized-word-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-word-reg/mem)
(sb!disassem:define-arg-type sized-dword-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-dword-reg/mem)

;;; Same as sized-reg/mem, but with a default operand size of :qword.
(sb!disassem:define-arg-type sized-reg/mem-default-qword
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-reg/mem-default-qword)

;;; XMM registers
(sb!disassem:define-arg-type xmmreg
  :prefilter #'prefilter-reg-r
  :printer #'print-xmmreg)

(sb!disassem:define-arg-type xmmreg-b
  :prefilter #'prefilter-reg-b
  :printer #'print-xmmreg)

(sb!disassem:define-arg-type xmmreg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-xmmreg/mem)

(sb!disassem:define-arg-type sized-xmmreg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-sized-xmmreg/mem)


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

(sb!disassem:define-arg-type condition-code
  :printer *condition-name-vec*)

(defun conditional-opcode (condition)
  (cdr (assoc condition *conditions* :test #'eq)))

;;;; disassembler instruction formats

(eval-when (:compile-toplevel :execute)
  (defun swap-if (direction field1 separator field2)
    `(:if (,direction :constant 0)
          (,field1 ,separator ,field2)
          (,field2 ,separator ,field1))))

(sb!disassem:define-instruction-format (byte 8 :default-printer '(:name))
  (op    :field (byte 8 0))
  ;; optional fields
  (accum :type 'accum)
  (imm))

(sb!disassem:define-instruction-format (two-bytes 16
                                        :default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8))))

(sb!disassem:define-instruction-format (three-bytes 24
                                        :default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8) (byte 8 16))))

;;; A one-byte instruction with a #x66 prefix, used to indicate an
;;; operand size of :word.
(sb!disassem:define-instruction-format (x66-byte 16
                                        :default-printer '(:name))
  (x66   :field (byte 8 0) :value #x66)
  (op    :field (byte 8 8)))

;;; A one-byte instruction with a REX prefix, used to indicate an
;;; operand size of :qword. REX.W must be 1, the other three bits are
;;; ignored.
(sb!disassem:define-instruction-format (rex-byte 16
                                        :default-printer '(:name))
  (rex   :field (byte 5 3) :value #b01001)
  (op    :field (byte 8 8)))

(sb!disassem:define-instruction-format (simple 8)
  (op    :field (byte 7 1))
  (width :field (byte 1 0) :type 'width)
  ;; optional fields
  (accum :type 'accum)
  (imm))

(sb!disassem:define-instruction-format (rex-simple 16)
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (op    :field (byte 7 9))
  (width :field (byte 1 8) :type 'width)
  ;; optional fields
  (accum :type 'accum)
  (imm))

;;; Same as simple, but with direction bit
(sb!disassem:define-instruction-format (simple-dir 8 :include 'simple)
  (op :field (byte 6 2))
  (dir :field (byte 1 1)))

;;; Same as simple, but with the immediate value occurring by default,
;;; and with an appropiate printer.
(sb!disassem:define-instruction-format (accum-imm 8
                                     :include 'simple
                                     :default-printer '(:name
                                                        :tab accum ", " imm))
  (imm :type 'signed-imm-data))

(sb!disassem:define-instruction-format (rex-accum-imm 16
                                     :include 'rex-simple
                                     :default-printer '(:name
                                                        :tab accum ", " imm))
  (imm :type 'signed-imm-data))

(sb!disassem:define-instruction-format (reg-no-width 8
                                     :default-printer '(:name :tab reg))
  (op    :field (byte 5 3))
  (reg   :field (byte 3 0) :type 'reg-b)
  ;; optional fields
  (accum :type 'accum)
  (imm))

(sb!disassem:define-instruction-format (rex-reg-no-width 16
                                     :default-printer '(:name :tab reg))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (op      :field (byte 5 11))
  (reg     :field (byte 3 8)    :type 'reg-b)
  ;; optional fields
  (accum :type 'accum)
  (imm))

;;; Same as reg-no-width, but with a default operand size of :qword.
(sb!disassem:define-instruction-format (reg-no-width-default-qword 8
                                        :include 'reg-no-width
                                        :default-printer '(:name :tab reg))
  (reg   :type 'reg-b-default-qword))

;;; Same as rex-reg-no-width, but with a default operand size of :qword.
(sb!disassem:define-instruction-format (rex-reg-no-width-default-qword 16
                                        :include 'rex-reg-no-width
                                        :default-printer '(:name :tab reg))
  (reg     :type 'reg-b-default-qword))

;;; Adds a width field to reg-no-width. Note that we can't use
;;; :INCLUDE 'REG-NO-WIDTH here to save typing because that would put
;;; the WIDTH field last, but the prefilter for WIDTH must run before
;;; the one for IMM to be able to determine the correct size of IMM.
(sb!disassem:define-instruction-format (reg 8
                                        :default-printer '(:name :tab reg))
  (op    :field (byte 4 4))
  (width :field (byte 1 3) :type 'width)
  (reg   :field (byte 3 0) :type 'reg-b)
  ;; optional fields
  (accum :type 'accum)
  (imm))

(sb!disassem:define-instruction-format (rex-reg 16
                                        :default-printer '(:name :tab reg))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (width   :field (byte 1 11)   :type 'width)
  (op      :field (byte 4 12))
  (reg     :field (byte 3 8)    :type 'reg-b)
  ;; optional fields
  (accum   :type 'accum)
  (imm))

(sb!disassem:define-instruction-format (two-bytes 16
                                        :default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8))))

(sb!disassem:define-instruction-format (reg-reg/mem 16
                                        :default-printer
                                        `(:name :tab reg ", " reg/mem))
  (op      :field (byte 7 1))
  (width   :field (byte 1 0)    :type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
                                :type 'reg/mem)
  (reg     :field (byte 3 11)   :type 'reg)
  ;; optional fields
  (imm))

(sb!disassem:define-instruction-format (rex-reg-reg/mem 24
                                        :default-printer
                                        `(:name :tab reg ", " reg/mem))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (width   :field (byte 1 8)    :type 'width)
  (op      :field (byte 7 9))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem)
  (reg     :field (byte 3 19)   :type 'reg)
  ;; optional fields
  (imm))

;;; same as reg-reg/mem, but with direction bit
(sb!disassem:define-instruction-format (reg-reg/mem-dir 16
                                        :include 'reg-reg/mem
                                        :default-printer
                                        `(:name
                                          :tab
                                          ,(swap-if 'dir 'reg/mem ", " 'reg)))
  (op  :field (byte 6 2))
  (dir :field (byte 1 1)))

(sb!disassem:define-instruction-format (rex-reg-reg/mem-dir 24
                                        :include 'rex-reg-reg/mem
                                        :default-printer
                                        `(:name
                                          :tab
                                          ,(swap-if 'dir 'reg/mem ", " 'reg)))
  (op  :field (byte 6 10))
  (dir :field (byte 1 9)))

(sb!disassem:define-instruction-format (x66-reg-reg/mem-dir 24
                                        :default-printer
                                        `(:name
                                          :tab
                                          ,(swap-if 'dir 'reg/mem ", " 'reg)))
  (x66     :field (byte 8 0)    :type 'x66 :value #x66)
  (op      :field (byte 6 10))
  (dir     :field (byte 1 9))
  (width   :field (byte 1 8)    :type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem)
  (reg     :field (byte 3 19)   :type 'reg))

(sb!disassem:define-instruction-format (x66-rex-reg-reg/mem-dir 32
                                        :default-printer
                                        `(:name
                                          :tab
                                          ,(swap-if 'dir 'reg/mem ", " 'reg)))
  (x66     :field (byte 8 0)    :type 'x66 :value #x66)
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (op      :field (byte 6 18))
  (dir     :field (byte 1 17))
  (width   :field (byte 1 16)   :type 'width)
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'reg/mem)
  (reg     :field (byte 3 27)   :type 'reg))

;;; Same as reg-reg/mem, but uses the reg field as a second op code.
(sb!disassem:define-instruction-format (reg/mem 16
                                        :default-printer '(:name :tab reg/mem))
  (op      :fields (list (byte 7 1) (byte 3 11)))
  (width   :field (byte 1 0)    :type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
                                :type 'sized-reg/mem)
  ;; optional fields
  (imm))

(sb!disassem:define-instruction-format (rex-reg/mem 24
                                        :default-printer '(:name :tab reg/mem))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (op      :fields (list (byte 7 9) (byte 3 19)))
  (width   :field (byte 1 8)    :type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'sized-reg/mem)
  ;; optional fields
  (imm))

;;; Same as reg/mem, but without a width field and with a default
;;; operand size of :qword.
(sb!disassem:define-instruction-format (reg/mem-default-qword 16
                                        :default-printer '(:name :tab reg/mem))
  (op      :fields (list (byte 8 0) (byte 3 11)))
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
                                :type 'sized-reg/mem-default-qword))

(sb!disassem:define-instruction-format (rex-reg/mem-default-qword 24
                                        :default-printer '(:name :tab reg/mem))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (op      :fields (list (byte 8 8) (byte 3 19)))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'sized-reg/mem-default-qword))

;;; Same as reg/mem, but with the immediate value occurring by default,
;;; and with an appropiate printer.
(sb!disassem:define-instruction-format (reg/mem-imm 16
                                        :include 'reg/mem
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (reg/mem :type 'sized-reg/mem)
  (imm     :type 'signed-imm-data))

(sb!disassem:define-instruction-format (rex-reg/mem-imm 24
                                        :include 'rex-reg/mem
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (reg/mem :type 'sized-reg/mem)
  (imm     :type 'signed-imm-data))

;;; Same as reg/mem, but with using the accumulator in the default printer
(sb!disassem:define-instruction-format
    (accum-reg/mem 16
     :include 'reg/mem :default-printer '(:name :tab accum ", " reg/mem))
  (reg/mem :type 'reg/mem)              ; don't need a size
  (accum :type 'accum))

(sb!disassem:define-instruction-format (rex-accum-reg/mem 24
                                        :include 'rex-reg/mem
                                        :default-printer
                                        '(:name :tab accum ", " reg/mem))
  (reg/mem :type 'reg/mem)              ; don't need a size
  (accum   :type 'accum))

;;; Same as reg-reg/mem, but with a prefix of #b00001111
(sb!disassem:define-instruction-format (ext-reg-reg/mem 24
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

(sb!disassem:define-instruction-format (ext-reg-reg/mem-no-width 24
                                        :default-printer
                                        `(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem)
  (reg     :field (byte 3 19)   :type 'reg))

(sb!disassem:define-instruction-format (rex-ext-reg-reg/mem-no-width 32
                                        :default-printer
                                        `(:name :tab reg ", " reg/mem))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (prefix  :field (byte 8 8)    :value #b00001111)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'reg/mem)
  (reg     :field (byte 3 27)   :type 'reg))

(sb!disassem:define-instruction-format (ext-reg/mem-no-width 24
                                        :default-printer
                                        `(:name :tab reg/mem))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op      :fields (list (byte 8 8) (byte 3 19)))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem))

(sb!disassem:define-instruction-format (rex-ext-reg/mem-no-width 32
                                        :default-printer
                                        `(:name :tab reg/mem))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (prefix  :field (byte 8 8)    :value #b00001111)
  (op      :fields (list (byte 8 16) (byte 3 27)))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'reg/mem))

;;; reg-no-width with #x0f prefix
(sb!disassem:define-instruction-format (ext-reg-no-width 16
                                        :default-printer '(:name :tab reg))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op    :field (byte 5 11))
  (reg   :field (byte 3 8) :type 'reg-b))

;;; Same as reg/mem, but with a prefix of #b00001111
(sb!disassem:define-instruction-format (ext-reg/mem 24
                                        :default-printer '(:name :tab reg/mem))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op      :fields (list (byte 7 9) (byte 3 19)))
  (width   :field (byte 1 8)    :type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'sized-reg/mem)
  ;; optional fields
  (imm))

(sb!disassem:define-instruction-format (ext-reg/mem-imm 24
                                        :include 'ext-reg/mem
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (imm :type 'signed-imm-data))

;;;; XMM instructions

;;; All XMM instructions use an extended opcode (#x0F as the first
;;; opcode byte). Therefore in the following "EXT" in the name of the
;;; instruction formats refers to the formats that have an additional
;;; prefix (#x66, #xF2 or #xF3).

;;; Instructions having an XMM register as the destination operand
;;; and an XMM register or a memory location as the source operand.
;;; The size of the operands is implicitly given by the instruction.
(sb!disassem:define-instruction-format (xmm-xmm/mem 24
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (x0f     :field (byte 8 0)    :value #x0f)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 19)   :type 'xmmreg))

(sb!disassem:define-instruction-format (rex-xmm-xmm/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg))

(sb!disassem:define-instruction-format (ext-xmm-xmm/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg))

(sb!disassem:define-instruction-format (ext-rex-xmm-xmm/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg))

;;; Same as xmm-xmm/mem etc., but with direction bit.

(sb!disassem:define-instruction-format (ext-xmm-xmm/mem-dir 32
                                        :include 'ext-xmm-xmm/mem
                                        :default-printer
                                        `(:name
                                          :tab
                                          ,(swap-if 'dir 'reg ", " 'reg/mem)))
  (op      :field (byte 7 17))
  (dir     :field (byte 1 16)))

(sb!disassem:define-instruction-format (ext-rex-xmm-xmm/mem-dir 40
                                        :include 'ext-rex-xmm-xmm/mem
                                        :default-printer
                                        `(:name
                                          :tab
                                          ,(swap-if 'dir 'reg ", " 'reg/mem)))
  (op      :field (byte 7 25))
  (dir     :field (byte 1 24)))

;;; Instructions having an XMM register as one operand
;;; and a constant (unsigned) byte as the other.

(sb!disassem:define-instruction-format (ext-xmm-imm 32
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

(sb!disassem:define-instruction-format (ext-rex-xmm-imm 40
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

(sb!disassem:define-instruction-format (xmm-reg/mem 24
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (x0f     :field (byte 8 0)    :value #x0f)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 19)   :type 'xmmreg))

(sb!disassem:define-instruction-format (rex-xmm-reg/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (rex     :field (byte 4 4)   :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (x0f     :field (byte 8 8)   :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'sized-reg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg))

(sb!disassem:define-instruction-format (ext-xmm-reg/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'sized-reg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg))

(sb!disassem:define-instruction-format (ext-rex-xmm-reg/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'sized-reg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg))

;;; Instructions having a general-purpose register as one operand and an
;;; XMM register or a memory location as the other operand.

(sb!disassem:define-instruction-format (reg-xmm/mem 24
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (x0f     :field (byte 8 0)    :value #x0f)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'sized-xmmreg/mem)
  (reg     :field (byte 3 19)   :type 'reg))

(sb!disassem:define-instruction-format (rex-reg-xmm/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (rex     :field (byte 4 4)   :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (x0f     :field (byte 8 8)   :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'sized-xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'reg))

(sb!disassem:define-instruction-format (ext-reg-xmm/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'sized-xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'reg))

(sb!disassem:define-instruction-format (ext-rex-reg-xmm/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'sized-xmmreg/mem)
  (reg     :field (byte 3 35)   :type 'reg))

;; XMM comparison instruction

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *sse-conditions* #(:eq :lt :le :unord :neq :nlt :nle :ord)))

(sb!disassem:define-arg-type sse-condition-code
  :printer *sse-conditions*)

(sb!disassem:define-instruction-format (xmm-xmm/mem-cmp 32
                                        :default-printer
                                        '(:name " " cc :tab reg ", " reg/mem))
  (x0f     :field (byte 8 0)    :value #x0f)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 19)   :type 'xmmreg)
  (cc      :field (byte 8 24)   :type 'sse-condition-code))

(sb!disassem:define-instruction-format (rex-xmm-xmm/mem-cmp 40
                                        :default-printer
                                        '(:name " " cc :tab reg ", " reg/mem))
  (rex     :field (byte 4 4)   :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg)
  (cc      :field (byte 8 32)   :type 'sse-condition-code))

(sb!disassem:define-instruction-format (ext-xmm-xmm/mem-cmp 40
                                        :default-printer
                                        '(:name " " cc :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg)
  (cc      :field (byte 8 32)   :type 'sse-condition-code))

(sb!disassem:define-instruction-format (ext-rex-xmm-xmm/mem-cmp 48
                                        :default-printer
                                        '(:name " " cc :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg)
  (cc      :field (byte 8 40)   :type 'sse-condition-code))

;;; XMM instructions with 8 bit immediate data

(sb!disassem:define-instruction-format (xmm-xmm/mem-imm 24
                                        :default-printer
                                        '(:name
                                          :tab reg ", " reg/mem ", " imm))
  (x0f     :field (byte 8 0)    :value #x0f)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 19)   :type 'xmmreg)
  (imm     :type 'imm-byte))

(sb!disassem:define-instruction-format (rex-xmm-xmm/mem-imm 32
                                        :default-printer
                                        '(:name
                                          :tab reg ", " reg/mem ", " imm))
  (rex     :field (byte 4 4)    :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg)
  (imm     :type 'imm-byte))

(sb!disassem:define-instruction-format (ext-xmm-xmm/mem-imm 32
                                        :default-printer
                                        '(:name
                                          :tab reg ", " reg/mem ", " imm))
  (prefix  :field (byte 8 0))
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg)
  (imm     :type 'imm-byte))

(sb!disassem:define-instruction-format (ext-rex-xmm-xmm/mem-imm 40
                                        :default-printer
                                        '(:name
                                          :tab reg ", " reg/mem ", " imm))
  (prefix  :field (byte 8 0))
  (rex     :field (byte 4 12)   :value #b0100)
  (wrxb    :field (byte 4 8)    :type 'wrxb)
  (x0f     :field (byte 8 16)   :value #x0f)
  (op      :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg)
  (imm     :type 'imm-byte))

(sb!disassem:define-instruction-format (string-op 8
                                     :include 'simple
                                     :default-printer '(:name width)))

(sb!disassem:define-instruction-format (rex-string-op 16
                                     :include 'rex-simple
                                     :default-printer '(:name width)))

(sb!disassem:define-instruction-format (short-cond-jump 16)
  (op    :field (byte 4 4))
  (cc    :field (byte 4 0) :type 'condition-code)
  (label :field (byte 8 8) :type 'displacement))

(sb!disassem:define-instruction-format (short-jump 16
                                     :default-printer '(:name :tab label))
  (const :field (byte 4 4) :value #b1110)
  (op    :field (byte 4 0))
  (label :field (byte 8 8) :type 'displacement))

(sb!disassem:define-instruction-format (near-cond-jump 16)
  (op    :fields (list (byte 8 0) (byte 4 12)) :value '(#b00001111 #b1000))
  (cc    :field (byte 4 8) :type 'condition-code)
  ;; The disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the offset.
  (label :type 'displacement
         :prefilter (lambda (value dstate)
                      (declare (ignore value)) ; always nil anyway
                      (sb!disassem:read-signed-suffix 32 dstate))))

(sb!disassem:define-instruction-format (near-jump 8
                                     :default-printer '(:name :tab label))
  (op    :field (byte 8 0))
  ;; The disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the address.
  (label :type 'displacement
         :prefilter (lambda (value dstate)
                      (declare (ignore value)) ; always nil anyway
                      (sb!disassem:read-signed-suffix 32 dstate))))


(sb!disassem:define-instruction-format (cond-set 24
                                     :default-printer '('set cc :tab reg/mem))
  (prefix :field (byte 8 0) :value #b00001111)
  (op    :field (byte 4 12) :value #b1001)
  (cc    :field (byte 4 8) :type 'condition-code)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
           :type 'sized-byte-reg/mem)
  (reg     :field (byte 3 19)   :value #b000))

(sb!disassem:define-instruction-format (cond-move 24
                                     :default-printer
                                        '('cmov cc :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)    :value #b00001111)
  (op      :field (byte 4 12)   :value #b0100)
  (cc      :field (byte 4 8)    :type 'condition-code)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem)
  (reg     :field (byte 3 19)   :type 'reg))

(sb!disassem:define-instruction-format (rex-cond-move 32
                                     :default-printer
                                        '('cmov cc :tab reg ", " reg/mem))
  (rex     :field (byte 4 4)   :value #b0100)
  (wrxb    :field (byte 4 0)    :type 'wrxb)
  (prefix  :field (byte 8 8)    :value #b00001111)
  (op      :field (byte 4 20)   :value #b0100)
  (cc      :field (byte 4 16)    :type 'condition-code)
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'reg/mem)
  (reg     :field (byte 3 27)   :type 'reg))

(sb!disassem:define-instruction-format (enter-format 32
                                     :default-printer '(:name
                                                        :tab disp
                                                        (:unless (:constant 0)
                                                          ", " level)))
  (op :field (byte 8 0))
  (disp :field (byte 16 8))
  (level :field (byte 8 24)))

;;; Single byte instruction with an immediate byte argument.
(sb!disassem:define-instruction-format (byte-imm 16
                                     :default-printer '(:name :tab code))
 (op :field (byte 8 0))
 (code :field (byte 8 8)))

;;; Two byte instruction with an immediate byte argument.
;;;
(sb!disassem:define-instruction-format (word-imm 24
                                     :default-printer '(:name :tab code))
  (op :field (byte 16 0))
  (code :field (byte 8 16)))


;;;; primitive emitters

(define-bitfield-emitter emit-word 16
  (byte 16 0))

(define-bitfield-emitter emit-dword 32
  (byte 32 0))

;;; Most uses of dwords are as displacements or as immediate values in
;;; 64-bit operations. In these cases they are sign-extended to 64 bits.
;;; EMIT-DWORD is unsuitable there because it accepts values of type
;;; (OR (SIGNED-BYTE 32) (UNSIGNED-BYTE 32)), so we provide a more
;;; restricted emitter here.
(defun emit-signed-dword (segment value)
  (declare (type segment segment)
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

(defun emit-constant-tn-rip (segment constant-tn reg)
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
                                          (+ 4 (- (+ offset posn)))))))
  (values))

(defun emit-label-rip (segment fixup reg)
  (let ((label (fixup-offset fixup)))
    ;; RIP-relative addressing
    (emit-mod-reg-r/m-byte segment #b00 reg #b101)
    (emit-back-patch segment
                     4
                     (lambda (segment posn)
                       (emit-signed-dword segment (- (label-position label)
                                                     (+ posn 4))))))
  (values))

(defun emit-ea (segment thing reg &optional allow-constants)
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
        (emit-constant-tn-rip segment thing reg))))
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
       (when (and (= mod 0) (= r/m #b101))
         ;; this is rip-relative in amd64, so we'll use a sib instead
         (setf r/m #b100 scale 1))
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
                  (emit-signed-dword segment disp))))))
    (fixup
     (typecase (fixup-offset thing)
       (label
        (emit-label-rip segment thing reg))
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
       (#.*float-sc-names*
        :float)
       (#.*double-sc-names*
        :double)
       (#.*complex-sc-names*
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
  (:printer reg ((op #b1011) (imm nil :type 'signed-imm-data))
            '(:name :tab reg ", " imm))
  (:printer rex-reg ((op #b1011) (imm nil :type 'signed-imm-data-upto-qword))
            '(:name :tab reg ", " imm))
  ;; absolute mem to/from accumulator
  (:printer simple-dir ((op #b101000) (imm nil :type 'imm-addr))
            `(:name :tab ,(swap-if 'dir 'accum ", " '("[" imm "]"))))
  ;; register to/from register/memory
  (:printer reg-reg/mem-dir ((op #b100010)))
  (:printer rex-reg-reg/mem-dir ((op #b100010)))
  (:printer x66-reg-reg/mem-dir ((op #b100010)))
  (:printer x66-rex-reg-reg/mem-dir ((op #b100010)))
  ;; immediate to register/memory
  (:printer reg/mem-imm ((op '(#b1100011 #b000))))
  (:printer rex-reg/mem-imm ((op '(#b1100011 #b000))))

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
                  (t
                   (maybe-emit-rex-for-ea segment src dst)
                   (emit-byte segment
                              (if (eq size :byte)
                                  #b10001010
                                  #b10001011))
                   (emit-ea segment src (reg-tn-encoding dst) t))))
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
            (aver (or (eq (fixup-flavor src) :foreign)
                      (eq (fixup-flavor src) :foreign-dataref)
                      (eq (ea-size dst) :dword)))
            (maybe-emit-rex-for-ea segment dst nil)
            (emit-byte segment #b11000111)
            (emit-ea segment dst #b000)
            (emit-absolute-fixup segment src))
           (t
            (error "bogus arguments to MOV: ~S ~S" dst src))))))

(defun emit-move-with-extension (segment dst src signed-p)
  (aver (register-p dst))
  (let ((dst-size (operand-size dst))
        (src-size (operand-size src))
        (opcode (if signed-p  #b10111110 #b10110110)))
    (ecase dst-size
      (:word
       (aver (eq src-size :byte))
       (maybe-emit-operand-size-prefix segment :word)
       ;; REX prefix is needed if SRC is SIL, DIL, SPL or BPL.
       (maybe-emit-rex-for-ea segment src dst :operand-size :word)
       (emit-byte segment #b00001111)
       (emit-byte segment opcode)
       (emit-ea segment src (reg-tn-encoding dst)))
      ((:dword :qword)
       (ecase src-size
         (:byte
          (maybe-emit-rex-for-ea segment src dst :operand-size dst-size)
          (emit-byte segment #b00001111)
          (emit-byte segment opcode)
          (emit-ea segment src (reg-tn-encoding dst)))
         (:word
          (maybe-emit-rex-for-ea segment src dst :operand-size dst-size)
          (emit-byte segment #b00001111)
          (emit-byte segment (logior opcode 1))
          (emit-ea segment src (reg-tn-encoding dst)))
         (:dword
          (aver (eq dst-size :qword))
          ;; dst is in reg, src is in modrm
          (let ((ea-p (ea-p src)))
            (maybe-emit-rex-prefix segment (if signed-p :qword :dword) dst
                                   (and ea-p (ea-index src))
                                   (cond (ea-p (ea-base src))
                                         ((tn-p src) src)
                                         (t nil)))
            (emit-byte segment (if signed-p #x63 #x8b)) ;movsxd or straight mov
            ;;(emit-byte segment opcode)
            (emit-ea segment src (reg-tn-encoding dst)))))))))

(define-instruction movsx (segment dst src)
  (:printer ext-reg-reg/mem-no-width
            ((op #b10111110) (reg/mem nil :type 'sized-byte-reg/mem)))
  (:printer rex-ext-reg-reg/mem-no-width
            ((op #b10111110) (reg/mem nil :type 'sized-byte-reg/mem)))
  (:printer ext-reg-reg/mem-no-width
            ((op #b10111111) (reg/mem nil :type 'sized-word-reg/mem)))
  (:printer rex-ext-reg-reg/mem-no-width
            ((op #b10111111) (reg/mem nil :type 'sized-word-reg/mem)))
  (:emitter (emit-move-with-extension segment dst src :signed)))

(define-instruction movzx (segment dst src)
  (:printer ext-reg-reg/mem-no-width
            ((op #b10110110) (reg/mem nil :type 'sized-byte-reg/mem)))
  (:printer rex-ext-reg-reg/mem-no-width
            ((op #b10110110) (reg/mem nil :type 'sized-byte-reg/mem)))
  (:printer ext-reg-reg/mem-no-width
            ((op #b10110111) (reg/mem nil :type 'sized-word-reg/mem)))
  (:printer rex-ext-reg-reg/mem-no-width
            ((op #b10110111) (reg/mem nil :type 'sized-word-reg/mem)))
  (:emitter (emit-move-with-extension segment dst src nil)))

;;; The regular use of MOVSXD is with an operand size of :qword. This
;;; sign-extends the dword source into the qword destination register.
;;; If the operand size is :dword the instruction zero-extends the dword
;;; source into the qword destination register, i.e. it does the same as
;;; a dword MOV into a register.
(define-instruction movsxd (segment dst src)
  (:printer reg-reg/mem ((op #b0110001) (width 1)
                         (reg/mem nil :type 'sized-dword-reg/mem)))
  (:printer rex-reg-reg/mem ((op #b0110001) (width 1)
                             (reg/mem nil :type 'sized-dword-reg/mem)))
  (:emitter (emit-move-with-extension segment dst src :signed)))

;;; this is not a real amd64 instruction, of course
(define-instruction movzxd (segment dst src)
  ; (:printer reg-reg/mem ((op #x63) (reg nil :type 'reg)))
  (:emitter (emit-move-with-extension segment dst src nil)))

(define-instruction push (segment src)
  ;; register
  (:printer reg-no-width-default-qword ((op #b01010)))
  (:printer rex-reg-no-width-default-qword ((op #b01010)))
  ;; register/memory
  (:printer reg/mem-default-qword ((op '(#b11111111 #b110))))
  (:printer rex-reg/mem-default-qword ((op '(#b11111111 #b110))))
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
                   (emit-ea segment src #b110 t))))))))

(define-instruction pop (segment dst)
  (:printer reg-no-width-default-qword ((op #b01011)))
  (:printer rex-reg-no-width-default-qword ((op #b01011)))
  (:printer reg/mem-default-qword ((op '(#b10001111 #b000))))
  (:printer rex-reg/mem-default-qword ((op '(#b10001111 #b000))))
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

(define-instruction xchg (segment operand1 operand2)
  ;; Register with accumulator.
  (:printer reg-no-width ((op #b10010)) '(:name :tab accum ", " reg))
  ;; Register/Memory with Register.
  (:printer reg-reg/mem ((op #b1000011)))
  (:printer rex-reg-reg/mem ((op #b1000011)))
  (:emitter
   (let ((size (matching-operand-size operand1 operand2)))
     (maybe-emit-operand-size-prefix segment size)
     (labels ((xchg-acc-with-something (acc something)
                (if (and (not (eq size :byte)) (register-p something))
                    (progn
                      (maybe-emit-rex-for-ea segment acc something)
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

(define-instruction lea (segment dst src)
  (:printer rex-reg-reg/mem ((op #b1000110)))
  (:printer reg-reg/mem ((op #b1000110) (width 1)))
  (:emitter
   (aver (or (dword-reg-p dst) (qword-reg-p dst)))
   (maybe-emit-rex-for-ea segment src dst
                          :operand-size :qword)
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
             (emit-ea segment dst opcode allow-constants)
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
             (emit-ea segment dst opcode allow-constants)
             (emit-sized-immediate segment size src))))
     ((register-p src)
      (maybe-emit-rex-for-ea segment dst src)
      (emit-byte segment
                 (dpb opcode
                      (byte 3 3)
                      (if (eq size :byte) #b00000000 #b00000001)))
      (emit-ea segment dst (reg-tn-encoding src) allow-constants))
     ((register-p dst)
      (maybe-emit-rex-for-ea segment src dst)
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
      (rex-accum-imm ((op ,(dpb subop (byte 3 2) #b0000010))))
      (reg/mem-imm ((op (#b1000000 ,subop))))
      (rex-reg/mem-imm ((op (#b1000000 ,subop))))
      ;; The redundant encoding #x82 is invalid in 64-bit mode,
      ;; therefore we force WIDTH to 1.
      (reg/mem-imm ((op (#b1000001 ,subop)) (width 1)
                    (imm nil :type signed-imm-byte)))
      (rex-reg/mem-imm ((op (#b1000001 ,subop)) (width 1)
                        (imm nil :type signed-imm-byte)))
      (reg-reg/mem-dir ((op ,(dpb subop (byte 3 1) #b000000))))
      (rex-reg-reg/mem-dir ((op ,(dpb subop (byte 3 1) #b000000))))))
  )

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
(define-instruction inc (segment dst)
  (:printer reg/mem ((op '(#b1111111 #b000))))
  (:printer rex-reg/mem ((op '(#b1111111 #b000))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)
     (emit-byte segment (if (eq size :byte) #b11111110 #b11111111))
     (emit-ea segment dst #b000))))

(define-instruction dec (segment dst)
  (:printer reg/mem ((op '(#b1111111 #b001))))
  (:printer rex-reg/mem ((op '(#b1111111 #b001))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)
     (emit-byte segment (if (eq size :byte) #b11111110 #b11111111))
     (emit-ea segment dst #b001))))

(define-instruction neg (segment dst)
  (:printer reg/mem ((op '(#b1111011 #b011))))
  (:printer rex-reg/mem ((op '(#b1111011 #b011))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment dst #b011))))

(define-instruction mul (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b100))))
  (:printer rex-accum-reg/mem ((op '(#b1111011 #b100))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b100))))

(define-instruction imul (segment dst &optional src1 src2)
  (:printer accum-reg/mem ((op '(#b1111011 #b101))))
  (:printer rex-accum-reg/mem ((op '(#b1111011 #b101))))
  (:printer ext-reg-reg/mem-no-width ((op #b10101111)))
  (:printer rex-ext-reg-reg/mem-no-width ((op #b10101111)))
  (:printer reg-reg/mem ((op #b0110100) (width 1)
                         (imm nil :type 'signed-imm-data))
            '(:name :tab reg ", " reg/mem ", " imm))
  (:printer rex-reg-reg/mem ((op #b0110100) (width 1)
                             (imm nil :type 'signed-imm-data))
            '(:name :tab reg ", " reg/mem ", " imm))
  (:printer reg-reg/mem ((op #b0110101) (width 1)
                         (imm nil :type 'signed-imm-byte))
            '(:name :tab reg ", " reg/mem ", " imm))
  (:printer rex-reg-reg/mem ((op #b0110101) (width 1)
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
  (:printer rex-accum-reg/mem ((op '(#b1111011 #b110))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment src nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b110))))

(define-instruction idiv (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b111))))
  (:printer rex-accum-reg/mem ((op '(#b1111011 #b111))))
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

(eval-when (:compile-toplevel :execute)
  (defun shift-inst-printer-list (subop)
    `((reg/mem ((op (#b1101000 ,subop)))
               (:name :tab reg/mem ", 1"))
      (rex-reg/mem ((op (#b1101000 ,subop)))
                   (:name :tab reg/mem ", 1"))
      (reg/mem ((op (#b1101001 ,subop)))
               (:name :tab reg/mem ", " 'cl))
      (rex-reg/mem ((op (#b1101001 ,subop)))
               (:name :tab reg/mem ", " 'cl))
      (reg/mem-imm ((op (#b1100000 ,subop))
                    (imm nil :type imm-byte)))
      (rex-reg/mem-imm ((op (#b1100000 ,subop))
                    (imm nil :type imm-byte))))))

(define-instruction rol (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b000))
  (:emitter
   (emit-shift-inst segment dst amount #b000)))

(define-instruction ror (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b001))
  (:emitter
   (emit-shift-inst segment dst amount #b001)))

(define-instruction rcl (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b010))
  (:emitter
   (emit-shift-inst segment dst amount #b010)))

(define-instruction rcr (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b011))
  (:emitter
   (emit-shift-inst segment dst amount #b011)))

(define-instruction shl (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b100))
  (:emitter
   (emit-shift-inst segment dst amount #b100)))

(define-instruction shr (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b101))
  (:emitter
   (emit-shift-inst segment dst amount #b101)))

(define-instruction sar (segment dst amount)
  (:printer-list
   (shift-inst-printer-list #b111))
  (:emitter
   (emit-shift-inst segment dst amount #b111)))

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
    `(#+nil
      (ext-reg-reg/mem-imm ((op ,(logior op #b100))
                            (imm nil :type signed-imm-byte)))
      (ext-reg-reg/mem ((op ,(logior op #b101)))
         (:name :tab reg/mem ", " 'cl)))))

(define-instruction shld (segment dst src amt)
  (:declare (type (or (member :cl) (mod 32)) amt))
  (:printer-list (double-shift-inst-printer-list #b10100000))
  (:emitter
   (emit-double-shift segment #b0 dst src amt)))

(define-instruction shrd (segment dst src amt)
  (:declare (type (or (member :cl) (mod 32)) amt))
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
  (:printer rex-accum-imm ((op #b1010100)))
  (:printer reg/mem-imm ((op '(#b1111011 #b000))))
  (:printer rex-reg/mem-imm ((op '(#b1111011 #b000))))
  (:printer reg-reg/mem ((op #b1000010)))
  (:printer rex-reg-reg/mem ((op #b1000010)))
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
  (:printer rex-reg/mem ((op '(#b1111011 #b010))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-for-ea segment dst nil)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment dst #b010))))

;;;; string manipulation

(define-instruction cmps (segment size)
  (:printer string-op ((op #b1010011)))
  (:printer rex-string-op ((op #b1010011)))
  (:emitter
   (maybe-emit-operand-size-prefix segment size)
   (maybe-emit-rex-prefix segment size nil nil nil)
   (emit-byte segment (if (eq size :byte) #b10100110 #b10100111))))

(define-instruction ins (segment acc)
  (:printer string-op ((op #b0110110)))
  (:printer rex-string-op ((op #b0110110)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b01101100 #b01101101)))))

(define-instruction lods (segment acc)
  (:printer string-op ((op #b1010110)))
  (:printer rex-string-op ((op #b1010110)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b10101100 #b10101101)))))

(define-instruction movs (segment size)
  (:printer string-op ((op #b1010010)))
  (:printer rex-string-op ((op #b1010010)))
  (:emitter
   (maybe-emit-operand-size-prefix segment size)
   (maybe-emit-rex-prefix segment size nil nil nil)
   (emit-byte segment (if (eq size :byte) #b10100100 #b10100101))))

(define-instruction outs (segment acc)
  (:printer string-op ((op #b0110111)))
  (:printer rex-string-op ((op #b0110111)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b01101110 #b01101111)))))

(define-instruction scas (segment acc)
  (:printer string-op ((op #b1010111)))
  (:printer rex-string-op ((op #b1010111)))
  (:emitter
   (let ((size (operand-size acc)))
     (aver (accumulator-p acc))
     (maybe-emit-operand-size-prefix segment size)
     (maybe-emit-rex-prefix segment size nil nil nil)
     (emit-byte segment (if (eq size :byte) #b10101110 #b10101111)))))

(define-instruction stos (segment acc)
  (:printer string-op ((op #b1010101)))
  (:printer rex-string-op ((op #b1010101)))
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

(define-instruction rep (segment)
  (:emitter
   (emit-byte segment #b11110011)))

(define-instruction repe (segment)
  (:printer byte ((op #b11110011)))
  (:emitter
   (emit-byte segment #b11110011)))

(define-instruction repne (segment)
  (:printer byte ((op #b11110010)))
  (:emitter
   (emit-byte segment #b11110010)))


;;;; bit manipulation

(define-instruction bsf (segment dst src)
  (:printer ext-reg-reg/mem-no-width ((op #b10111100)))
  (:printer rex-ext-reg-reg/mem-no-width ((op #b10111100)))
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
  (:printer rex-ext-reg-reg/mem-no-width ((op #b10111101)))
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
    `((ext-reg/mem-imm ((op (#b1011101 ,subop))
                        (reg/mem nil :type reg/mem)
                        (imm nil :type imm-byte)
                        (width 0)))
      (ext-reg-reg/mem ((op ,(dpb subop (byte 3 2) #b1000001))
                        (width 1))
                       (:name :tab reg/mem ", " reg)))))

(define-instruction bt (segment src index)
  (:printer-list (bit-test-inst-printer-list #b100))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b100)))

(define-instruction btc (segment src index)
  (:printer-list (bit-test-inst-printer-list #b111))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b111)))

(define-instruction btr (segment src index)
  (:printer-list (bit-test-inst-printer-list #b110))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b110)))

(define-instruction bts (segment src index)
  (:printer-list (bit-test-inst-printer-list #b101))
  (:emitter
   (emit-bit-test-and-mumble segment src index #b101)))


;;;; control transfer

(define-instruction call (segment where)
  (:printer near-jump ((op #b11101000)))
  (:printer reg/mem-default-qword ((op '(#b11111111 #b010))))
  (:printer rex-reg/mem-default-qword ((op '(#b11111111 #b010))))
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
  (:printer rex-reg/mem-default-qword ((op '(#b11111111 #b100))))
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
  (:printer rex-cond-move ())
  (:emitter
   (aver (register-p dst))
   (let ((size (matching-operand-size dst src)))
     (aver (or (eq size :word) (eq size :dword) (eq size :qword)))
     (maybe-emit-operand-size-prefix segment size))
   (maybe-emit-rex-for-ea segment src dst)
   (emit-byte segment #b00001111)
   (emit-byte segment (dpb (conditional-opcode cond) (byte 4 0) #b01000000))
   (emit-ea segment src (reg-tn-encoding dst))))

;;;; conditional byte set

(define-instruction set (segment dst cond)
  (:printer cond-set ())
  (:emitter
   (maybe-emit-rex-for-ea segment dst nil)
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
  (let* ((length (sb!sys:sap-ref-8 sap offset))
         (vector (make-array length :element-type '(unsigned-byte 8))))
    (declare (type sb!sys:system-area-pointer sap)
             (type (unsigned-byte 8) length)
             (type (simple-array (unsigned-byte 8) (*)) vector))
    (cond (length-only
           (values 0 (1+ length) nil nil))
          (t
           (sb!kernel:copy-ub8-from-system-area sap (1+ offset)
                                                vector 0 length)
           (collect ((sc-offsets)
                     (lengths))
             (lengths 1)                ; the length byte
             (let* ((index 0)
                    (error-number (sb!c:read-var-integer vector index)))
               (lengths index)
               (loop
                 (when (>= index length)
                   (return))
                 (let ((old-index index))
                   (sc-offsets (sb!c:read-var-integer vector index))
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
  (flet ((nt (x) (if stream (sb!disassem:note x dstate))))
    ;; XXX: {BYTE,WORD}-IMM-CODE below is a macro defined by the
    ;; DEFINE-INSTRUCTION-FORMAT for {BYTE,WORD}-IMM above.  Due to
    ;; the spectacular design for DEFINE-INSTRUCTION-FORMAT (involving
    ;; a call to EVAL in order to define the macros at compile-time
    ;; only) they do not even show up as symbols in the target core.
    (case #!-ud2-breakpoints (byte-imm-code chunk dstate)
          #!+ud2-breakpoints (word-imm-code chunk dstate)
      (#.error-trap
       (nt "error trap")
       (sb!disassem:handle-break-args #'snarf-error-junk stream dstate))
      (#.cerror-trap
       (nt "cerror trap")
       (sb!disassem:handle-break-args #'snarf-error-junk stream dstate))
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
       (nt "single-step trap (before)")))))

(define-instruction break (segment code)
  (:declare (type (unsigned-byte 8) code))
  #!-ud2-breakpoints (:printer byte-imm ((op #b11001100)) '(:name :tab code)
                               :control #'break-control)
  #!+ud2-breakpoints (:printer word-imm ((op #b0000101100001111)) '(:name :tab code)
                               :control #'break-control)
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
  (:emitter
   (emit-byte segment #b10010000)))

(define-instruction wait (segment)
  (:printer byte ((op #b10011011)))
  (:emitter
   (emit-byte segment #b10011011)))

(defun emit-prefix (segment name)
  (declare (ignorable segment))
  (ecase name
    ((nil))
    (:lock
     #!+sb-thread
     (emit-byte segment #xf0))))

;;; FIXME: It would be better to make the disassembler understand the prefix as part
;;; of the instructions...
(define-instruction lock (segment)
  (:printer byte ((op #b11110000)))
  (:emitter
   (bug "LOCK prefix used as a standalone instruction")))

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

(defun emit-sse-inst (segment dst src prefix opcode &key operand-size)
  (when prefix
    (emit-byte segment prefix))
  (if operand-size
      (maybe-emit-rex-for-ea segment src dst :operand-size operand-size)
      (maybe-emit-rex-for-ea segment src dst))
  (emit-byte segment #x0f)
  (emit-byte segment opcode)
  (emit-ea segment src (reg-tn-encoding dst)))

;; 0110 0110:0000 1111:0111 00gg: 11 010 xmmreg:imm8

(defun emit-sse-inst-with-imm (segment dst/src imm
                               prefix opcode /i
                               &key operand-size)
  (aver (<= 0 /i 7))
  (when prefix
    (emit-byte segment prefix))
  (maybe-emit-rex-prefix segment operand-size dst/src nil nil)
  (emit-byte segment #x0F)
  (emit-byte segment opcode)
  (emit-byte segment (logior (ash (logior #b11000 /i) 3)
                             (reg-tn-encoding dst/src)))
  (emit-byte segment imm))

(macrolet
    ((define-imm-sse-instruction (name opcode /i)
         `(define-instruction ,name (segment dst/src imm)
            (:printer ext-rex-xmm-imm ((prefix #x66) (op ,opcode) (/i ,/i)))
            (:printer ext-xmm-imm ((prefix #x66) (op ,opcode) (/i ,/i)))
            (:emitter
             (emit-sse-inst-with-imm segment dst/src imm
                                     #x66 ,opcode ,/i
                                     :operand-size :do-not-set)))))
  (define-imm-sse-instruction pslldq #x73 7)
  (define-imm-sse-instruction psllw #x71 6)
  (define-imm-sse-instruction pslld #x72 6)
  (define-imm-sse-instruction psllq #x73 6)

  (define-imm-sse-instruction psraw-imm #x71 4)
  (define-imm-sse-instruction psrad-imm #x72 4)

  (define-imm-sse-instruction psrldq #x73 3)
  (define-imm-sse-instruction psrlw #x71 2)
  (define-imm-sse-instruction psrld #x72 2)
  (define-imm-sse-instruction psrlq #x73 2))

;;; Emit an SSE instruction that has an XMM register as the destination
;;; operand and for which the size of the operands is implicitly given
;;; by the instruction.
(defun emit-regular-sse-inst (segment dst src prefix opcode)
  (aver (xmm-register-p dst))
  (emit-sse-inst segment dst src prefix opcode
                 :operand-size :do-not-set))

;;; Instructions having an XMM register as the destination operand
;;; and an XMM register or a memory location as the source operand.
;;; The operand size is implicitly given by the instruction.

(macrolet ((define-regular-sse-inst (name prefix opcode)
             `(define-instruction ,name (segment dst src)
                ,@(if prefix
                      `((:printer ext-xmm-xmm/mem
                                  ((prefix ,prefix) (op ,opcode)))
                        (:printer ext-rex-xmm-xmm/mem
                                  ((prefix ,prefix) (op ,opcode))))
                      `((:printer xmm-xmm/mem ((op ,opcode)))
                        (:printer rex-xmm-xmm/mem ((op ,opcode)))))
                (:emitter
                 (emit-regular-sse-inst segment dst src ,prefix ,opcode)))))
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
  (define-regular-sse-inst divpd    #x66 #x5e)
  (define-regular-sse-inst divps    nil  #x5e)
  (define-regular-sse-inst divsd    #xf2 #x5e)
  (define-regular-sse-inst divss    #xf3 #x5e)
  (define-regular-sse-inst mulpd    #x66 #x59)
  (define-regular-sse-inst mulps    nil  #x59)
  (define-regular-sse-inst mulsd    #xf2 #x59)
  (define-regular-sse-inst mulss    #xf3 #x59)
  (define-regular-sse-inst rccps    nil  #x53)
  (define-regular-sse-inst rcpss    #xf3 #x53)
  (define-regular-sse-inst rsqrtps  nil  #x52)
  (define-regular-sse-inst rsqrtss  #xf3 #x52)
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
  (define-regular-sse-inst padduwb  #x66 #xdd)
  (define-regular-sse-inst pavgb    #x66 #xe0)
  (define-regular-sse-inst pavgw    #x66 #xe3)
  (define-regular-sse-inst pmaddwd  #x66 #xf5)
  (define-regular-sse-inst pmulhuw  #x66 #xe4)
  (define-regular-sse-inst pmulhw   #x66 #xe5)
  (define-regular-sse-inst pmullw   #x66 #xd5)
  (define-regular-sse-inst pmuludq  #x66 #xf4)
  (define-regular-sse-inst psadbw   #x66 #xf6)
  (define-regular-sse-inst psraw    #x66 #xe1)
  (define-regular-sse-inst psrad    #x66 #xe2)
  (define-regular-sse-inst psubb    #x66 #xf8)
  (define-regular-sse-inst psubw    #x66 #xf9)
  (define-regular-sse-inst psubd    #x66 #xfa)
  (define-regular-sse-inst psubq    #x66 #xfb)
  (define-regular-sse-inst psubsb   #x66 #xd8)
  (define-regular-sse-inst psubsw   #x66 #xd9)
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
  ;; moves
  (define-regular-sse-inst movntdq #x66 #xe7)
  (define-regular-sse-inst movntpd #x66 #x2b)
  (define-regular-sse-inst movntps nil #x2b)
  ;; integer
  (define-regular-sse-inst packsswb  #x66 #x63)
  (define-regular-sse-inst packssdw  #x66 #x6b)
  (define-regular-sse-inst punpckhbw #x66 #x68)
  (define-regular-sse-inst punpckhwd #x66 #x69)
  (define-regular-sse-inst punpckhdq #x66 #x6a)
  (define-regular-sse-inst punpckhqdq #x66 #x6d)
  (define-regular-sse-inst punpcklbw #x66 #x60)
  (define-regular-sse-inst punpcklwd #x66 #x61)
  (define-regular-sse-inst punpckldq #x66 #x62)
  (define-regular-sse-inst punpcklqdq #x66 #x6c))

(macrolet ((define-xmm-shuffle-sse-inst (name prefix opcode)
               `(define-instruction ,name (segment dst src pattern)
                  ,@(if prefix
                        `((:printer ext-xmm-xmm/mem-imm ; suboptimal
                                    ((prefix ,prefix) (op ,opcode)))
                          (:printer ext-rex-xmm-xmm/mem-imm
                                    ((prefix ,prefix) (op ,opcode))))
                        `((:printer xmm-xmm/mem-imm ((op ,opcode)))
                          (:printer rex-xmm-xmm/mem-imm ((op ,opcode)))))
                  (:emitter
                   (aver (typep pattern '(unsigned-byte 8)))
                   (emit-regular-sse-inst segment dst src ,prefix ,opcode)
                   (emit-byte segment pattern)))))
  (define-xmm-shuffle-sse-inst pshufd  #x66 #x70)
  (define-xmm-shuffle-sse-inst pshufhw #xf3 #x70)
  (define-xmm-shuffle-sse-inst pshuflw #xf2 #x70)
  (define-xmm-shuffle-sse-inst shufpd  #x66 #xc6)
  (define-xmm-shuffle-sse-inst shufps  nil  #xc6))

;; MASKMOVDQU (dst is DS:RDI)
(define-instruction maskmovdqu (segment src mask)
  (:printer ext-xmm-xmm/mem
            ((prefix #x66) (op #xf7)))
  (:printer ext-rex-xmm-xmm/mem
            ((prefix #x66) (op #xf7)))
  (:emitter
   (aver (xmm-register-p src))
   (aver (xmm-register-p mask))
   (emit-regular-sse-inst segment src mask #x66 #xf7)))

(macrolet ((define-xmm-comparison-sse-inst (name prefix opcode &optional name-prefix name-suffix)
               (let ((printer (when name-prefix
                                `'(,name-prefix cc ,name-suffix :tab reg ", " reg/mem))))
                 `(define-instruction ,name (segment op x y)
                    ,@(if prefix
                          `((:printer ext-xmm-xmm/mem-cmp
                                      ((prefix ,prefix) (op ,opcode))
                                      ,@(and printer `(,printer)))
                            (:printer ext-rex-xmm-xmm/mem-cmp
                                      ((prefix ,prefix) (op ,opcode))
                                      ,@(and printer `(,printer))))
                          `((:printer xmm-xmm/mem-cmp ((op ,opcode))
                                      ,@(and printer `(,printer)))
                            (:printer rex-xmm-xmm/mem-cmp ((op ,opcode))
                                      ,@(and printer `(,printer)))))
                    (:emitter
                     (let ((code (position op *sse-conditions*)))
                       (aver code)
                       (emit-regular-sse-inst segment x y ,prefix ,opcode)
                       (emit-byte segment code)))))))
  (define-xmm-comparison-sse-inst cmppd #x66 #xc2 "CMP" "PD")
  (define-xmm-comparison-sse-inst cmpps nil  #xc2 "CMP" "PS")
  (define-xmm-comparison-sse-inst cmpsd #xf2 #xc2 "CMP" "SD")
  (define-xmm-comparison-sse-inst cmpss #xf3 #xc2 "CMP" "SS"))

;;; MOVSD, MOVSS
(macrolet ((define-movsd/ss-sse-inst (name prefix)
             `(define-instruction ,name (segment dst src)
                (:printer ext-xmm-xmm/mem-dir ((prefix ,prefix)
                                               (op #b0001000)))
                (:printer ext-rex-xmm-xmm/mem-dir ((prefix ,prefix)
                                                   (op #b0001000)))
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
                         (emit-regular-sse-inst segment dst src ,prefix ,opcode-from))))
                  (define-instruction ,name (segment dst src)
                    ,@(if prefix
                          `((:printer ext-xmm-xmm/mem
                                      ((prefix ,prefix) (op ,opcode-from)))
                            (:printer ext-rex-xmm-xmm/mem
                                      ((prefix ,prefix) (op ,opcode-from)))
                            (:printer ext-xmm-xmm/mem
                                      ((prefix ,prefix) (op ,opcode-to))
                                      '(:name :tab reg/mem ", " reg))
                            (:printer ext-rex-xmm-xmm/mem
                                      ((prefix ,prefix) (op ,opcode-to))
                                      '(:name :tab reg/mem ", " reg)))
                          `((:printer xmm-xmm/mem
                                      ((op ,opcode-from)))
                            (:printer rex-xmm-xmm/mem
                                      ((op ,opcode-from)))
                            (:printer xmm-xmm/mem
                                      ((op ,opcode-to))
                                      '(:name :tab reg/mem ", " reg))
                            (:printer rex-xmm-xmm/mem
                                      ((op ,opcode-to))
                                      '(:name :tab reg/mem ", " reg))))
                    (:emitter
                     (cond ((xmm-register-p dst)
                            ,(when force-to-mem
                               `(aver (not (or (register-p src)
                                               (xmm-register-p src)))))
                            (emit-regular-sse-inst segment dst src ,prefix ,opcode-from))
                           (t
                            (aver (xmm-register-p src))
                            ,(when force-to-mem
                               `(aver (not (or (register-p dst)
                                               (xmm-register-p dst)))))
                            (emit-regular-sse-inst segment src dst ,prefix ,opcode-to))))))))
  ;; direction bit?
  (define-mov-sse-inst movapd #x66 #x28 #x29)
  (define-mov-sse-inst movaps nil  #x28 #x29)
  (define-mov-sse-inst movdqa #x66 #x6f #x7f)
  (define-mov-sse-inst movdqu #xf3 #x6f #x7f)

  ;; use movhps for movlhps and movlps for movhlps
  (define-mov-sse-inst movhpd #x66 #x16 #x17 :force-to-mem t)
  (define-mov-sse-inst movhps nil  #x16 #x17 :reg-reg-name movlhps)
  (define-mov-sse-inst movlpd #x66 #x12 #x13 :force-to-mem t)
  (define-mov-sse-inst movlps nil  #x12 #x13 :reg-reg-name movhlps)
  (define-mov-sse-inst movupd #x66 #x10 #x11)
  (define-mov-sse-inst movups nil  #x10 #x11))

;;; MOVQ
(define-instruction movq (segment dst src)
  (:printer ext-xmm-xmm/mem ((prefix #xf3) (op #x7e)))
  (:printer ext-rex-xmm-xmm/mem ((prefix #xf3) (op #x7e)))
  (:printer ext-xmm-xmm/mem ((prefix #x66) (op #xd6))
            '(:name :tab reg/mem ", " reg))
  (:printer ext-rex-xmm-xmm/mem ((prefix #x66) (op #xd6))
            '(:name :tab reg/mem ", " reg))
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
  (:printer ext-xmm-reg/mem ((prefix #x66) (op #x6e)))
  (:printer ext-rex-xmm-reg/mem ((prefix #x66) (op #x6e)))
  (:printer ext-xmm-reg/mem ((prefix #x66) (op #x7e))
            '(:name :tab reg/mem ", " reg))
  (:printer ext-rex-xmm-reg/mem ((prefix #x66) (op #x7e))
            '(:name :tab reg/mem ", " reg))
  (:emitter
   (cond ((xmm-register-p dst)
          (emit-sse-inst segment dst src #x66 #x6e))
         (t
          (aver (xmm-register-p src))
          (emit-sse-inst segment src dst #x66 #x7e)))))

(macrolet ((define-integer-source-sse-inst (name prefix opcode &key mem-only)
             `(define-instruction ,name (segment dst src)
                ,@(if prefix
                      `((:printer ext-xmm-reg/mem ((prefix ,prefix) (op ,opcode)))
                        (:printer ext-rex-xmm-reg/mem ((prefix ,prefix) (op ,opcode))))
                      `((:printer xmm-reg/mem ((op ,opcode)))
                        (:printer rex-xmm-reg/mem ((op ,opcode)))))

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
                ,@(if prefix
                      `((:printer ext-reg-xmm/mem ((prefix ,prefix) (op ,opcode)))
                        (:printer ext-rex-reg-xmm/mem ((prefix ,prefix) (op ,opcode))))
                      `((:printer reg-xmm/mem ((op ,opcode)))
                        (:printer rex-reg-xmm/mem ((op ,opcode)))))
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

;;; Other SSE instructions

;; FIXME: is that right!?
(define-instruction movnti (segment dst src)
  (:printer ext-reg-reg/mem-no-width ((op #xc3)))
  (:printer rex-ext-reg-reg/mem-no-width ((op #xc3)))
  (:emitter
   (aver (not (or (register-p dst)
                  (xmm-register-p dst))))
   (aver (register-p src))
   (maybe-emit-rex-for-ea segment src dst)
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
  (:printer rex-ext-reg/mem-no-width ((op '(#x18 0)))
            '("PREFETCHNTA" :tab reg/mem))
  (:printer rex-ext-reg/mem-no-width ((op '(#x18 1)))
            '("PREFETCHT0" :tab reg/mem))
  (:printer rex-ext-reg/mem-no-width ((op '(#x18 2)))
            '("PREFETCHT1" :tab reg/mem))
  (:printer rex-ext-reg/mem-no-width ((op '(#x18 3)))
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
  (:printer rex-ext-reg/mem-no-width ((op '(#xae 7))))
  (:emitter
   (aver (not (or (register-p src)
                  (xmm-register-p src))))
   (aver (eq (operand-size src) :byte))
   (maybe-emit-rex-for-ea segment src nil)
   (emit-byte segment #x0f)
   (emit-byte segment #x18)
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
  (:printer rex-ext-reg/mem-no-width ((op '(#xae 2))))
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
  (:printer rex-ext-reg/mem-no-width ((op '(#xae 3))))
  (:emitter
   (aver (not (or (register-p dst)
                  (xmm-register-p dst))))
   (aver (eq (operand-size dst) :dword))
   (maybe-emit-rex-for-ea segment dst nil)
   (emit-byte segment #x0f)
   (emit-byte segment #xae)
   (emit-ea segment dst 3)))

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
      ((complex single-float)
         (setf constant (list :complex-single-float first)))
      ((complex double-float)
         (setf constant (list :complex-double-float first)))))
  (destructuring-bind (type value) constant
    (ecase type
      ((:byte :word :dword :qword)
         (aver (integerp value))
         (cons type value))
      ((:base-char)
         (aver (base-char-p value))
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

(defun emit-constant-segment-header (constants optimize)
  (declare (ignore constants))
  (loop repeat (if optimize 64 16) do (inst byte #x90)))

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
