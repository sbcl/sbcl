;;; that part of the description of the x86-64 instruction set
;;;; which can live on the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-X86-64-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from this package into SB-VM
  (import '(negate-condition
            plausible-signed-imm32-operand-p
            ea-p ea-base ea-index size-nbyte alias-p
            ea ea-disp rip-relative-ea) "SB-VM")
  (import 'sb-assem::&prefix)
  ;; Imports from SB-VM into this package
  #+sb-simd-pack-256
  (import '(sb-vm::int-avx2-reg sb-vm::double-avx2-reg sb-vm::single-avx2-reg))
  (import '(sb-vm::tn-byte-offset sb-vm::tn-reg sb-vm::reg-name
            sb-vm::frame-byte-offset sb-vm::rip-tn sb-vm::rbp-tn
            sb-vm::gpr-tn-p sb-vm::stack-tn-p sb-c::tn-reads sb-c::tn-writes
            sb-vm::ymm-reg
            sb-vm::linkage-addr->name
            sb-vm::registers sb-vm::float-registers sb-vm::stack))) ; SB names

(defconstant +lock-prefix-present+ #x80)
;;;                                  v---- size prefix presence bit
(defconstant +byte-size-prefix+    #b100)
(defconstant +word-size-prefix+    #b101)
(defconstant +dword-size-prefix+   #b110)
(defconstant +qword-size-prefix+   #b111)
(defun lockp (prefix) (if (logtest +lock-prefix-present+ prefix) :lock))
(defmacro opsize-prefix-present (byte) `(logtest ,byte #b100))
(defmacro opsize-prefix-keyword (byte)
  `(svref #(:byte :word :dword :qword) (logand ,byte #b11)))
(defun pick-operand-size (prefix operand1 &optional operand2)
  (acond ((logtest prefix #b100) (opsize-prefix-keyword prefix))
         (operand2
          (let ((dst-size (operand-size operand1))
                (src-size (operand-size operand2)))
            (cond ((not (or dst-size src-size))
                   (error "can't tell the size of either ~S or ~S" operand1 operand2))
                  ((and dst-size src-size)
                   (aver (eq dst-size src-size))
                   dst-size)
                  (t (or dst-size src-size)))))
         (t (operand-size operand1))))
(defun encode-size-prefix (prefix)
  (case prefix
   (:byte  +byte-size-prefix+)
   (:word  +word-size-prefix+)
   (:dword +dword-size-prefix+)
   (:qword +qword-size-prefix+)))
;;; Prefix is a mandatory operand to the encoder, so if none were
;;; written then this correctly prepends a 0.
(defun sb-assem::extract-prefix-keywords (args &aux (lockp 0) (size 0))
  (loop (acond ((eq (car args) :lock) (setq lockp +lock-prefix-present+))
               ((encode-size-prefix (car args)) (setq size it))
               (t (return (cons (logior lockp size) args))))
        (pop args)))
(defun sb-assem::decode-prefix (args) ; for trace file only
  (let ((b (car args)))
    (if (zerop b)
        (cdr args)
        (cons (append (if (logtest +lock-prefix-present+ b) '(:lock))
                      (if (opsize-prefix-present b) (list (opsize-prefix-keyword b))))
              (cdr args)))))

;;; a REG object discards all information about a TN except its storage base
;;; (a/k/a register class), size class (for GPRs), and encoding.
;;; The disassembler also uses this structure.
(defstruct (reg (:predicate register-p)
                (:copier nil)
                (:constructor !make-reg (id)))
  (id 0 :type (unsigned-byte 8)))
(declaim (freeze-type reg))

;;; Default word size for the chip: if the operand size /= :dword
;;; we need to output #x66 (or REX) prefix
(defconstant +default-operand-size+ :dword)

;;; The default address size for the chip. It could be overwritten
;;; to :dword with a #x67 prefix, but this is never needed by SBCL
;;; and thus not supported by this assembler/disassembler.
(defconstant +default-address-size+ :qword)

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
(defconstant +allow-qword-imm+ #b10000000000)
(defconstant +imm-size-8+      #b01000000000)
(defconstant +operand-size-8+  #b00100000000)
(defconstant +operand-size-16+ #b00010000000)
(defconstant +fs-segment+      #b00001000000)
(defconstant +gs-segment+      #b00000100000)
(defconstant +rex+             #b00000010000)
;;; The next 4 exactly correspond to the bits in the REX prefix itself,
;;; to avoid unpacking and stuffing into inst-properties one at a time.
(defconstant +rex-w+           #b1000)
(defconstant +rex-r+           #b0100)
(defconstant +rex-x+           #b0010)
(defconstant +rex-b+           #b0001)

(defun size-nbyte (size)
  (ecase size
    (:byte  1)
    (:word  2)
    (:dword 4)
    (:qword 8)
    (:oword 16)
    (:hword 32)))

;;; If chopping IMM to 32 bits and sign-extending is equal to the original value,
;;; return the signed result, which the CPU will always extend to 64 bits.
;;; Notably this allows MOST-POSITIVE-WORD to be an immediate constant.
;;; Only use this if the actual operand size is 64 bits, because it will lie to you
;;; if you pass in #xfffffffffffffffc but are operating on a :dword - it returns
;;; a small negative, which encodes to a dword.  Apparently the system assembler
;;; considers this a "feature", and merely truncates, though it does warn.
(defun plausible-signed-imm32-operand-p (imm)
  (typecase imm
    ((signed-byte 32) imm)
    ;; Alternatively, the lower bound #xFFFFFFFF80000000 could
    ;; be spelled as (MASK-FIELD (BYTE 33 31) -1)
    ((integer #.(- (expt 2 64) (expt 2 31)) #.most-positive-word)
     (sb-c::mask-signed-field 32 imm))
    (t nil)))
;;; Like above but for 8 bit signed immediate operands. In this case we need
;;; to know the operand size, because, for example #xffcf is a signed imm8
;;; if the operand size is :word, but it is not if the operand size is larger.
(defun plausible-signed-imm8-operand-p (imm operand-size)
  (cond ((typep imm '(signed-byte 8))
         imm)
        ((eq operand-size :qword)
         ;; Try the imm32 test, and if the result is (signed-byte 8),
         ;; then return it, otherwise return NIL.
         (let ((imm (plausible-signed-imm32-operand-p imm)))
           (when (typep imm '(signed-byte 8))
             imm)))
        (t
         (when (case operand-size
                 (:word (typep imm '(integer #xFF80 #xFFFF)))
                 (:dword (typep imm '(integer #xFFFFFF80 #xFFFFFFFF))))
           (sb-c::mask-signed-field 8 imm)))))

;;;; disassembler argument types

;;; Used to capture the lower four bits of the REX prefix all at once ...
(define-arg-type wrxb
  :prefilter (lambda (dstate value)
               (dstate-setprop dstate (logior +rex+ (logand value #b1111)))
               value))
;;; ... or individually (not needed for REX.R and REX.X).
;;; They are always used together, so only the first one sets the REX property.
(define-arg-type rex-w
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (logior +rex+ (if (plusp value) +rex-w+ 0)))))
(define-arg-type rex-b
  :prefilter (lambda (dstate value)
               (dstate-setprop dstate (if (plusp value) +rex-b+ 0))))

(define-arg-type width
  :prefilter #'prefilter-width
  :printer (lambda (value stream dstate)
             (declare (ignore value))
             (when stream
               (princ (schar (symbol-name (inst-operand-size dstate)) 0)
                      stream))))

;;; Used to capture the effect of the #x66 operand size override prefix.
(define-arg-type x66
  :prefilter (lambda (dstate junk)
               (declare (ignore junk))
               (dstate-setprop dstate +operand-size-16+)))

(define-arg-type displacement
  :sign-extend t
  :use-label (lambda (value dstate)
               (if (sb-disassem::dstate-absolutize-jumps dstate)
                   (+ (dstate-next-addr dstate) value)
                   value))
  :printer #'print-rel32-disp)

(define-arg-type accum
  :printer (lambda (value stream dstate)
             (declare (ignore value)
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
  ;; imm-addr is used only with opcodes #xA0 through #xA3 which take a 64-bit
  ;; address unless overridden to 32-bit by the #x67 prefix that we don't parse.
  ;; i.e. we don't have (INST-ADDR-SIZE DSTATE), so always take it to be 64 bits.
  :prefilter (lambda (dstate) (read-suffix 64 dstate))
  :printer #'print-label)

;;; Normally, immediate values for an operand size of :qword are of size
;;; :dword and are sign-extended to 64 bits.
;;; The exception is that opcode group 0xB8 .. 0xBF allows a :qword immediate.
(define-arg-type signed-imm-data
  :prefilter (lambda (dstate &aux (width (inst-operand-size dstate)))
               (when (and (not (dstate-getprop dstate +allow-qword-imm+))
                          (eq width :qword))
                 (setf width :dword))
               (read-signed-suffix (* (size-nbyte width) n-byte-bits) dstate))
  :printer (lambda (value stream dstate)
             (declare (notinline sb-disassem::inst-name))
             (if (not stream) ; won't have a DSTATE-INST in this case, maybe fix that?
                 (operand value dstate)
                 (let ((opcode (sb-disassem::inst-name (sb-disassem::dstate-inst dstate)))
                       (size (inst-operand-size dstate)))
                   ;; Slight bugs still- MOV to memory of :DWORD could be writing
                   ;; a raw slot or anything. Only a :QWORD could be a symbol, however
                   ;; the size when moving to register is :DWORD.
                   ;; And CMP could be comparing "raw" data from SAP-REF-32, except that
                   ;; in practice it can't because the compiler is too stupid to do it,
                   ;; and will fixnumize the loaded value.
                   ;; If this is too eager in printing random constants as lisp objects,
                   ;; there is another tactic we could use: only consider something a pointer
                   ;; if it is in a location corresponding to an absolute code fixup.
                   (if (and (memq size '(:dword :qword))
                            (memq opcode '(mov cmp push))
                            (maybe-note-static-lispobj value dstate t))
                       (princ16 value stream)
                       (princ value stream))))))

;;; Used by those instructions that have a default operand size of
;;; :qword. Nevertheless the immediate is at most of size :dword.
;;; The only instruction of this kind having a variant with an immediate
;;; argument is PUSH.
(define-arg-type signed-imm-data-default-qword
  :type 'signed-imm-data ; to get the :printer
  :prefilter (lambda (dstate)
               (let ((nbits (* (size-nbyte (inst-operand-size-default-qword dstate))
                               n-byte-bits)))
                 (when (= nbits 64)
                   (setf nbits 32))
                 (read-signed-suffix nbits dstate))))

(define-arg-type signed-imm-byte
  :prefilter (lambda (dstate)
               (read-signed-suffix 8 dstate)))

(define-arg-type imm-byte
  :prefilter (lambda (dstate)
               (read-suffix 8 dstate)))

;;; needed for the ret imm16 instruction
(define-arg-type imm-word-16
  :prefilter (lambda (dstate)
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
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-xmmreg/mem)

(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant-eqx +conditions+
  ;; The first element in each row is the one we disassemble as.
  ;; Always prefer the one without a negation in it if there is a choice.
  '((:o . 0)
    (:no . 1)
    (:b . 2) (:nae . 2) (:c . 2)
    (:ae . 3) (:nb . 3) (:nc . 3)
    (:eq . 4) (:e . 4) (:z . 4)
    (:ne . 5) (:nz . 5)
    (:be . 6) (:na . 6)
    (:a . 7) (:nbe . 7)
    (:s . 8)
    (:ns . 9)
    (:p . 10) (:pe . 10)
    (:po . 11) (:np . 11)
    (:l . 12) (:nge . 12)
    (:ge . 13) (:nl . 13)
    (:le . 14) (:ng . 14)
    (:g . 15) (:nle . 15))
  #'equal)
(defun encoded-condition (condition)
  (cdr (assoc condition +conditions+ :test #'eq))))
(defconstant-eqx +condition-name-vec+
  (let ((vec (make-array 16 :initial-element nil)))
    (dolist (cond +conditions+ vec)
      (when (null (aref vec (cdr cond)))
        (setf (aref vec (cdr cond)) (car cond)))))
  #'equalp)

;;; SSE shuffle patterns. The names end in the number of bits of the
;;; immediate byte that are used to encode the pattern and the radix
;;; in which to print the value.
(macrolet ((define-sse-shuffle-arg-type (name format-string)
               `(define-arg-type ,name
                  :type 'imm-byte
                  :printer (lambda (value stream dstate)
                             (declare (type (unsigned-byte 8) value)
                                      (type (or null stream) stream))
                             (if stream
                                 (format stream ,format-string value)
                                 (operand value dstate))))))
  (define-sse-shuffle-arg-type sse-shuffle-pattern-2-2 "#b~2,'0B")
  (define-sse-shuffle-arg-type sse-shuffle-pattern-8-4 "#4r~4,4,'0R"))

(define-arg-type condition-code :printer +condition-name-vec+)

(defun negate-condition (name)
  (aref +condition-name-vec+ (logxor 1 (encoded-condition name))))

;;;; disassembler instruction formats

(defun swap-if (direction field1 separator field2)
    `(:if (,direction :constant 0)
          (,field1 ,separator ,field2)
          (,field2 ,separator ,field1)))

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

(define-instruction-format (reg-reg/mem 16
                                        :default-printer
                                        `(:name :tab reg ", " reg/mem))
  (op      :field (byte 7 1))
  (width   :field (byte 1 0)    :type 'width)
  (reg/mem :fields (list (byte 2 14) (byte 3 8))
           :type 'reg/mem :reader regrm-inst-r/m)
  (reg     :field (byte 3 11)   :type 'reg :reader regrm-inst-reg)
  ;; optional fields
  (imm))

;;; same as reg-reg/mem, but with direction bit
(define-instruction-format (reg-reg/mem-dir 16
                            :include reg-reg/mem
                            :default-printer
                            `(:name :tab ,(swap-if 'dir 'reg/mem ", " 'reg)))
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
  (prefix  :field (byte 8 0)    :value #x0F)
  (op      :field (byte 7 9))
  (width   :field (byte 1 8)    :type 'width)
  (reg/mem :fields (list (byte 2 22) (byte 3 16)) :type 'reg/mem)
  (reg     :field (byte 3 19)   :type 'reg)
  ;; optional fields
  (imm))

(define-instruction-format (ext-reg-reg/mem-no-width 24
                                        :default-printer
                                        `(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)    :value #x0F)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem)
  (reg     :field (byte 3 19)   :type 'reg)
  ;; optional fields
  (imm))

(define-instruction-format (ext-reg/mem-no-width 24
                                        :default-printer
                                        `(:name :tab reg/mem))
  (prefix  :field (byte 8 0)    :value #x0F)
  (op      :fields (list (byte 8 8) (byte 3 19)))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'reg/mem))

;;; reg-no-width with #x0f prefix
(define-instruction-format (ext-reg-no-width 16
                                        :default-printer '(:name :tab reg))
  (prefix  :field (byte 8 0)    :value #x0F)
  (op    :field (byte 5 11))
  (reg   :field (byte 3 8) :type 'reg-b))


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
  (rex) ; optional
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
  (rex) ; optional
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg)
  (imm))

(define-instruction-format (ext-2byte-xmm-xmm/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex) ; optional
  (x0f     :field (byte 8 8)    :value #x0f)
  (op1     :field (byte 8 16))          ; #x38 or #x3a
  (op2     :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg))

;;; Same as xmm-xmm/mem etc., but with direction bit.

(define-instruction-format (ext-xmm-xmm/mem-dir 32
                                        :include ext-xmm-xmm/mem
                                        :default-printer
                                        `(:name
                                          :tab
                                          ,(swap-if 'dir 'reg ", " 'reg/mem)))
  (op      :field (byte 7 17))
  (dir     :field (byte 1 16)))

;;; Instructions having an XMM register as one operand
;;; and a constant (unsigned) byte as the other.

(define-instruction-format (ext-xmm-imm 32
                                        :default-printer
                                        '(:name :tab reg/mem ", " imm))
  (prefix  :field (byte 8 0))
  (rex) ; optional
  (x0f     :field (byte 8 8)   :value #x0f)
  (op      :field (byte 8 16))
  (/i      :field (byte 3 27))
  (b11     :field (byte 2 30) :value #b11)
  (reg/mem :field (byte 3 24)
           :type 'xmmreg-b)
  (imm     :type 'imm-byte))

;;; Instructions having an XMM register as one operand and a general-
;;; -purpose register or a memory location as the other operand.

(define-instruction-format (xmm-reg/mem 24
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (rex) ; optional
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

(define-instruction-format (ext-2byte-xmm-reg/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex) ; optional
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
  (rex) ; optional
  (x0f     :field (byte 8 0)    :value #x0f)
  (op      :field (byte 8 8))
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 19)   :type 'reg))

(define-instruction-format (ext-reg-xmm/mem 32
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex) ; optional
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'xmmreg/mem)
  (reg     :field (byte 3 27)   :type 'reg))

;;; Instructions having a general-purpose register or a memory location
;;; as one operand and an a XMM register as the other operand.

(define-instruction-format (ext-reg/mem-xmm 32
                                        :default-printer
                                        '(:name :tab reg/mem ", " reg))
  (prefix  :field (byte 8 0))
  (rex) ; optional
  (x0f     :field (byte 8 8)    :value #x0f)
  (op      :field (byte 8 16))
  (reg/mem :fields (list (byte 2 30) (byte 3 24))
                                :type 'reg/mem)
  (reg     :field (byte 3 27)   :type 'xmmreg)
  (imm))

(define-instruction-format (ext-2byte-reg/mem-xmm 40
                                        :default-printer
                                        '(:name :tab reg/mem ", " reg))
  (prefix  :field (byte 8 0))
  (rex) ; optional
  (x0f     :field (byte 8 8)    :value #x0f)
  (op1     :field (byte 8 16))
  (op2     :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32)) :type 'reg/mem)
  (reg     :field (byte 3 35)   :type 'xmmreg)
  (imm))

;;; Instructions having a general-purpose register as one operand and an a
;;; general-purpose register or a memory location as the other operand,
;;; and using a prefix byte.

(define-instruction-format (ext-2byte-prefix-reg-reg/mem 40
                                        :default-printer
                                        '(:name :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0))
  (rex) ; optional
  (x0f     :field (byte 8 8)    :value #x0f)
  (op1     :field (byte 8 16))          ; #x38 or #x3a
  (op2     :field (byte 8 24))
  (reg/mem :fields (list (byte 2 38) (byte 3 32))
                                :type 'sized-reg/mem)
  (reg     :field (byte 3 35)   :type 'reg))

;; XMM comparison instruction

(defconstant-eqx +sse-conditions+
    #(:eq :lt :le :unord :neq :nlt :nle :ord)
  #'equalp)

(define-arg-type sse-condition-code
  ;; Inherit the prefilter from IMM-BYTE to READ-SUFFIX the byte.
  :type 'imm-byte
  :printer +sse-conditions+)

(define-instruction-format (string-op 8
                                     :include simple
                                     :default-printer '(:name width)))

(define-instruction-format (short-cond-jump 16)
  (op    :field (byte 4 4) :value #b0111)
  (cc    :field (byte 4 0) :type 'condition-code)
  (label :field (byte 8 8) :type 'displacement))

(define-instruction-format (short-jump 16 :default-printer '(:name :tab label))
  (const :field (byte 4 4) :value #b1110)
  (op    :field (byte 4 0))
  (label :field (byte 8 8) :type 'displacement))

(define-instruction-format (near-cond-jump 48)
  (op    :fields (list (byte 8 0) (byte 4 12)) :value '(#x0F #b1000))
  (cc    :field (byte 4 8) :type 'condition-code)
  (label :field (byte 32 16) :type 'displacement :reader near-cond-jump-displacement))

(define-instruction-format (near-jump 40 :default-printer '(:name :tab label))
  (op    :field (byte 8 0))
  (label :field (byte 32 8) :type 'displacement :reader near-jump-displacement))

(define-instruction-format (cond-set 24 :default-printer '('set cc :tab reg/mem))
  (prefix :field (byte 8 0) :value #x0F)
  (op    :field (byte 4 12) :value #b1001)
  (cc    :field (byte 4 8) :type 'condition-code)
  (reg/mem :fields (list (byte 2 22) (byte 3 16))
           :type 'sized-byte-reg/mem)
  (reg     :field (byte 3 19)   :value #b000))

(define-instruction-format (cond-move 24
                                     :default-printer
                                        '('cmov cc :tab reg ", " reg/mem))
  (prefix  :field (byte 8 0)    :value #x0F)
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


;;;; primitive emitters

(define-bitfield-emitter emit-word 16
  (byte 16 0))

(declaim (maybe-inline emit-dword))
(define-bitfield-emitter emit-dword 32
  (byte 32 0))

(define-bitfield-emitter emit-qword 64
  (byte 64 0))

;;; Most uses of dwords are as displacements or as immediate values in
;;; 64-bit operations. In these cases they are sign-extended to 64 bits.
;;; EMIT-DWORD is unsuitable there because it accepts values of type
;;; (OR (SIGNED-BYTE 32) (UNSIGNED-BYTE 32)), so we provide a more
;;; restricted emitter here.
(defun emit-signed-dword (segment value)
  (declare (type sb-assem:segment segment)
           (type (signed-byte 32) value))
  #-sb-xc-host (declare (inline emit-dword))
  (emit-dword segment value))

(define-bitfield-emitter emit-mod-reg-r/m-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))

(define-bitfield-emitter emit-sib-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))


;;;; fixup emitters

(defun emit-absolute-fixup (segment fixup &optional quad-p)
  (note-fixup segment (if quad-p :absolute :abs32) fixup)
  (let ((offset (fixup-offset fixup)))
    (if quad-p
        (emit-qword segment offset)
        (emit-signed-dword segment offset))))

(defun emit-relative-fixup (segment fixup)
  (note-fixup segment :rel32 fixup)
  (emit-signed-dword segment (fixup-offset fixup)))


(defmacro emit-bytes (segment &rest bytes)
  `(progn ,@(mapcar (lambda (x) `(emit-byte ,segment ,x)) bytes)))
(defun opcode+size-bit (opcode size)
  (if (eq size :byte) opcode (logior opcode 1)))

;;; A label can refer to things near enough it using the addend.
(defstruct (label+addend (:constructor make-label+addend (label addend))
                         (:predicate nil)
                         (:copier nil))
  (label nil :type label)
  ;; The addend of :CODE computes the tagged code object.
  ;; It could be represented by just a label, except that the assembler can't
  ;; directly reference labels in the boxed header. A label would need a negative
  ;; POSN to represent such a location.
  (addend 0 :type (or (signed-byte 32) (eql :code))))
(declaim (freeze-type label+addend))

;;;; the effective-address (ea) structure
(defstruct (ea (:constructor %ea (segment disp base index scale))
               (:copier nil))
  (segment nil :type (member :cs :gs) :read-only t)
  (base nil :type (or tn null) :read-only t)
  (index nil :type (or tn null) :read-only t)
  (scale 1 :type (member 1 2 4 8) :read-only t)
  (disp 0 :type (or (unsigned-byte 32) (signed-byte 32) fixup
                    label label+addend)
          :read-only t))
(declaim (freeze-type ea))
(defmethod print-object ((ea ea) stream)
  (cond ((or *print-escape* *print-readably*)
         (print-unreadable-object (ea stream :type t)
           (format stream
                   "~@[ base=~S~]~@[ index=~S~]~@[ scale=~S~]~@[ disp=~S~]"
                   (let ((b (ea-base ea))) (if (eq b rip-tn) :RIP b))
                   (ea-index ea)
                   (let ((scale (ea-scale ea)))
                     (if (= scale 1) nil scale))
                   (ea-disp ea))))
        (t
         ;; This is ridiculous. READ won't really read an EA in this syntax.
         (format stream "PTR [")
         (flet ((name (tn) (reg-name (tn-reg tn))))
           (awhen (ea-base ea)
             (write-string (if (eq it rip-tn) "RIP" (name it)) stream)
             (when (ea-index ea)
               (write-string "+" stream)))
           (awhen (ea-index ea)
             (write-string (name it) stream)))
         (unless (= (ea-scale ea) 1)
           (format stream "*~A" (ea-scale ea)))
         (typecase (ea-disp ea)
           (integer
            (format stream "~@D" (ea-disp ea)))
           (t
            (format stream "+~A" (ea-disp ea))))
         (write-char #\] stream))))

;;; BOA constructor for EA has these acceptable forms:
;;;   (EA displacement &OPTIONAL base-register index-register scale)
;;;   (EA base-register &OPTIONAL index-register scale)
;;;
;;; mnemonic device: the syntax is like AT&T "%seg:disp(%rbase,%rindex,scale)"
;;; where the leading "seg" and "disp" are optional.
;;;
;;; Most instructions can determine an EA size based on the size of a register
;;; operand. The few that can't are mem+immediate mode instructions, and
;;; instructions which need explicit differently sized register + EA.
;;; In those cases, the instruction syntax includes a size, as in:
;;;
;;;  AT&T  : testb $0x40(%rax)
;;;  Intel : test byte ptr [rax], 40
;;;  SBCL  : (TEST :BYTE (EA RAX-TN) #x40)
;;;
(defun ea (&rest args) ; seg displacement base index scale
  (declare (dynamic-extent args))
  (let ((seg :cs) disp)
    (let ((first (car args)))
      (case first
        (:gs (setq seg first) (pop args))
        (:cs (pop args))))
    (let ((first (car args)))
      ;; Rather than checking explicitly for all the things that are legal to be
      ;; a displacement (i.e. LABEL, FIXUP, INTEGER), look for (NOT (OR TN NULL).)
      ;; That is, NIL must be a placeholder for absence of base register,
      ;; and not a displacement.
      (unless (typep first '(or tn null))
        (setq disp first)
        (pop args)))
    ;; The minimal EA is either an absolute address or an unindexed base register.
    ;; So gotta have at least one of disp or base. Enforce by doing either of two
    ;; destructuring-binds depending on whether DISP was present.
    (if disp
        (destructuring-bind (&optional base index (scale 1)) args
          (%ea seg disp base index scale))
        (destructuring-bind (base &optional index (scale 1)) args
          (%ea seg 0 base index scale)))))

(defun rip-relative-ea (label &optional addend)
  (%ea :cs (if addend (make-label+addend label addend) label) rip-tn nil 1))

(defun emit-byte-displacement-backpatch (segment target)
  (emit-back-patch segment 1
                   (lambda (segment posn)
                     (emit-byte segment
                                (the (signed-byte 8)
                                  (- (label-position target) (1+ posn)))))))

(defun emit-dword-displacement-backpatch (segment target &optional (n-extra 0))
  ;; N-EXTRA is how many more instruction bytes will follow, to properly compute
  ;; the displacement from the beginning of the next instruction to TARGET.
  (emit-back-patch segment 4
                   (lambda (segment posn)
                     (emit-signed-dword segment (- (label-position target)
                                                   (+ 4 posn n-extra))))))

(defconstant +regclass-gpr+ 0)
(defconstant +regclass-fpr+ 1)
(defconstant +size-class-qword+ 0)
(defconstant +size-class-byte+  3)

;;; Size is :BYTE,:WORD,:DWORD,:QWORD
;;; INDEX is the index in the register file for the class and size.
;;; Byte registers indices 0 through 15 correspond to the uniform register
;;; set defined by the AMD64 architecture, and indices 20, 21, 22, 23 are
;;; for the legacy high byte registers AH,CH,DH,BH respectively.
;;; (indices 16 through 19 are unused)
(defun make-gpr-id (size index)
  (logior (ash (if (eq size :byte) (the (mod 24) index) (the (mod 16) index)) 3)
          (ash (or (position size #(:qword :dword :word :byte))
                   (error "Bad register size ~s" size))
               1)))

(defun make-fpr-id (index size)
  (declare (type (mod 16) index))
  (ecase size
    (:xmm (logior (ash index 3) 1)) ; low bit = FPR, not GPR
    (:ymm (logior (ash index 3) 3))))
(defun is-ymm-id-p (reg-id)
  (= (ldb (byte 3 0) reg-id) 3))

(declaim (inline is-gpr-id-p gpr-id-size-class reg-id-num))
(defun is-gpr-id-p (reg-id)
  (not (logbitp 0 reg-id)))
(defun gpr-id-size-class (reg-id) ; an integer 0..3 identifying the size
  (ldb (byte 2 1) reg-id))
(defun reg-id-num (reg-id)
  (ldb (byte 5 3) reg-id))

;;; Note that SB-VM has its own variation on these predicates
;;; operating on TNs: GPR-TN-P, XMM-TN-P

;;; Return true if THING is a general-purpose register.
(defun gpr-p (thing)
  (and (register-p thing) (is-gpr-id-p (reg-id thing))))

;;; Return true if THING is an XMM register.
(defun xmm-register-p (thing)
  (and (register-p thing) (not (is-gpr-id-p (reg-id thing)))))

;;; Return true if THING is AL, AX, EAX, or RAX
(defun accumulator-p (thing)
  (and (gpr-p thing) (= 0 (reg-id-num (reg-id thing)))))

;;; Any TN that does not reference memory would have already been
;;; rendered as a REG by the operand lowering pass.
(defun mem-ref-p (x) (or (ea-p x) (tn-p x)))

(defun reg-name (reg)
  (let ((id (reg-id reg)))
    (svref (cond ((is-gpr-id-p id)
                  (svref (load-time-value
                          (vector sb-vm::+qword-register-names+
                                  sb-vm::+dword-register-names+
                                  sb-vm::+word-register-names+
                                  sb-vm::+byte-register-names+)
                          t)
                         (gpr-id-size-class id)))
                 ((is-ymm-id-p id)
                  #.(coerce (loop for i below 16 collect (format nil "YMM~D" i))
                            'vector))
                 (t
                  #.(coerce (loop for i below 16 collect (format nil "XMM~D" i))
                            'vector)))
           (reg-id-num id))))

;;; Return the "lossy" encoding of REG (keeping only the 3 low bits).
;;; Assume that the caller emitted a REX appropriately if needed for
;;; this use of REG (we can't actually assert that since we don't know
;;; which REX bit should have been set, nor which bits were set).
;;; However, we can assert absence of REX if so called for.
(declaim (ftype (sfunction (reg sb-assem:segment) (mod 8)) reg-encoding))
(defun reg-encoding (reg segment &aux (id (reg-id reg)))
  (cond ((and (is-gpr-id-p id)
              (eq (gpr-id-size-class id) +size-class-byte+)
              (>= (reg-id-num id) 16))
         (if (eql (segment-encoder-state segment) +rex+)
             (bug "Can't encode ~A with REX prefix" (reg-name reg))
             ; number 16 corresponds to AH which is encoded as 4 and so on
             (+ (reg-id-num id) -16 4)))
        (t
         (logand (reg-id-num id) 7))))

(defun emit-byte+reg (seg byte reg)
  (emit-byte seg (+ byte (reg-encoding reg seg))))

;;; The GPR for any size and register number is a unique atom. Return it.
(defun get-gpr (size number)
  (svref (load-time-value
          (coerce (append
                   (loop for i from 0 below 16 collect (!make-reg (make-gpr-id :qword i)))
                   (loop for i from 0 below 16 collect (!make-reg (make-gpr-id :dword i)))
                   (loop for i from 0 below 16 collect (!make-reg (make-gpr-id :word i)))
                   ;; byte reg vector is #(AL CL ... R15B AH CH DH BH)
                   (loop for i from 0 below 20 collect (!make-reg (make-gpr-id :byte i))))
                  'vector)
          t)
         (+ (if (eq size :byte) (the (mod 20) number) (the (mod 16) number))
            (ecase size
              (:qword  0)
              (:dword 16)
              (:word  32)
              (:byte  48)))))

(defun get-fpr (regset number)
  (ecase regset
    (:xmm
     (svref (load-time-value
             (coerce (loop for i from 0 below 16
                        collect (!make-reg (make-fpr-id i :xmm)))
                     'vector)
             t)
            number))
    (:ymm
     (svref (load-time-value
             (coerce (loop for i from 0 below 16
                        collect (!make-reg (make-fpr-id i :ymm)))
                     'vector)
             t)
            number))))

;;; Given a TN which maps to a GPR, return the corresponding REG.
;;; This is called during operand lowering, and also for emitting an EA
;;; since the EA BASE and INDEX slots reference TNs, not REGs.
(defun tn-reg (tn)
  (declare (type tn tn))
  (aver (eq (sb-name (sc-sb (tn-sc tn))) 'registers))
  (get-gpr (sc-operand-size (tn-sc tn)) (tn-offset tn)))

;;; For each TN that maps to a register, replace it with a REG instance.
;;; This is so that when we resize a register for instructions which specify
;;; an explicit operand size, we do not construct random TNs. If nothing else,
;;; this saves some consing, but also allows removal of 2 or 3 storage classes.
;;; (i.e. We should never need to ask for a vop temporary that is DWORD-REG
;;; because you can't usefully pack 2 dwords in a qword)
(defun sb-assem::perform-operand-lowering (operands)
  (mapcar (lambda (operand)
            (cond ((typep operand '(cons tn (eql :high-byte)))
                   (get-gpr :byte (+ 16 (the (mod 4) (tn-offset (car operand))))))
                  ((not (typep operand 'tn))
                   ;; EA, literal, fixup, keyword, or REG if reg-in-size was used
                   operand)
                  ((eq (sb-name (sc-sb (tn-sc operand))) 'registers)
                   (tn-reg operand))
                  ((memq (sc-name (tn-sc operand))
                         '(ymm-reg
                           int-avx2-reg
                           double-avx2-reg
                           single-avx2-reg))
                   (get-fpr :ymm (tn-offset operand)))
                  ((eq (sb-name (sc-sb (tn-sc operand))) 'float-registers)
                   (get-fpr :xmm (tn-offset operand)))
                  (t ; a stack SC or constant
                   operand)))
          operands))

(defun emit-ea (segment thing reg &key (remaining-bytes 0) xmm-index)
  (when (register-p reg)
    (setq reg (reg-encoding reg segment)))
  (etypecase thing
    (reg
     (emit-mod-reg-r/m-byte segment #b11 reg (reg-encoding thing segment)))
    (tn
     (ecase (sb-name (sc-sb (tn-sc thing)))
       (stack
        (emit-ea segment (ea (frame-byte-offset (tn-offset thing)) rbp-tn) reg))
       ((constant sb-vm::immediate-constant)
        (when (eq (sb-name (sc-sb (tn-sc thing))) 'sb-vm::immediate-constant)
          ;; Assert that THING is definitely present in boxed constants. If not
          ;; you'll get "NIL is not of type INTEGER" (because TN-OFFSET is NIL)
          (let ((val (tn-value thing)))
            (aver (and (symbolp val) (not (static-symbol-p val))))))
        ;; To access the constant at index 5 out of 6 constants, that's simply
        ;; word index -1 from the origin label, and so on.
        (emit-ea segment
                 (rip-relative-ea (segment-origin segment) ; = word index 0
                                  (- (tn-byte-offset thing)
                                     (component-header-length)))
                 reg :remaining-bytes remaining-bytes))))
    (ea
     (when (eq (ea-base thing) rip-tn)
       (aver (null (ea-index thing)))
       (let ((disp (ea-disp thing)))
         (aver (typep disp '(or label label+addend fixup)))
         (emit-mod-reg-r/m-byte segment #b00 reg #b101) ; RIP-relative mode
         (if (typep disp 'fixup)
             (emit-relative-fixup segment disp)
             (multiple-value-bind (label addend)
                 (if (typep disp 'label+addend)
                     (values (label+addend-label disp) (label+addend-addend disp))
                     (values disp 0))
               (when (eq addend :code)
                 (setq addend (- other-pointer-lowtag (component-header-length))))
               ;; To point at ADDEND bytes beyond the label, pretend that the PC
               ;; at which the EA occurs is _smaller_ by that amount.
               (emit-dword-displacement-backpatch
                segment label (- remaining-bytes addend)))))
       (return-from emit-ea))
     (let* ((base (ea-base thing))
            (index (ea-index thing))
            (scale (ea-scale thing))
            (disp (ea-disp thing))
            (base-encoding (when base (reg-encoding (tn-reg base) segment)))
            (mod (cond ((or (null base) (and (eql disp 0) (/= base-encoding #b101)))
                        #b00)
                       ((and (fixnump disp) (<= -128 disp 127))
                        #b01)
                       (t
                        #b10)))
            (r/m (cond (index #b100)
                       ((null base) #b101)
                       (t base-encoding))))
       (when (and (= mod 0) (= r/m #b101))
         ;; this is rip-relative in amd64, so we'll use a sib instead
         (setf r/m #b100 scale 1))
       (emit-mod-reg-r/m-byte segment mod reg r/m)
       (when (= r/m #b100)
         (let ((ss (1- (integer-length scale)))
               (index (if (null index)
                          #b100
                          (if (location= index sb-vm::rsp-tn)
                              (error "can't index off of RSP")
                              (reg-encoding (if xmm-index
                                                (get-fpr :xmm (tn-offset index))
                                                (tn-reg index))
                                            segment))))
               (base (if (null base) #b101 base-encoding)))
           (emit-sib-byte segment ss index base)))
       (cond ((= mod #b01)
              (emit-byte segment disp))
             ((or (= mod #b10) (null base))
              (cond ((not (fixup-p disp))
                     (emit-signed-dword segment disp))
                    ((eq (fixup-flavor disp) :assembly-routine)
                     (note-fixup segment :*abs32 disp)
                     (emit-signed-dword segment 0))
                    (t
                     (emit-absolute-fixup segment disp)))))))))

;;;; utilities

;;; A REX prefix must be emitted if at least one of the following
;;; conditions is true:
;;; 1. Any of the WRXB bits are nonzero, which occurs if:
;;;    (a) The operand size is :QWORD and the default operand size of the
;;;        instruction is not :QWORD, or
;;;    (b) The instruction references a register above 7.
;;; 2. The instruction references one of the byte registers SIL, DIL,
;;;    SPL or BPL in the 'R' or 'B' encoding fields. X can be ignored
;;;    because the index in an EA is never byte-sized.

;;; Emit a REX prefix if necessary. WIDTH is used to determine
;;; whether to set REX.W. Callers pass it explicitly as :DO-NOT-SET if
;;; this should not happen, for example because the instruction's default
;;; operand size is qword.
;;; R, X and B are NIL or REG-IDs specifying registers the encodings of
;;; which may be extended with the REX.R, REX.X and REX.B bit, respectively.
(defun emit-rex-if-needed (segment width-qword r x b)
  (declare (type boolean width-qword)
           (type (or null fixnum) r x b))
  (flet ((encoding-bit3-p (reg-id)
           (and reg-id (logbitp 3 (reg-id-num reg-id))))
         (spl/bpl/sil/dil-p (reg-id)
           (and reg-id
                (is-gpr-id-p reg-id)
                (eq (gpr-id-size-class reg-id) +size-class-byte+)
                (<= 4 (reg-id-num reg-id) 7))))
    (let ((wrxb (logior (if width-qword         #b1000 0)
                        (if (encoding-bit3-p r) #b0100 0)
                        (if (encoding-bit3-p x) #b0010 0)
                        (if (encoding-bit3-p b) #b0001 0))))
      (cond ((or (not (eql wrxb 0))
                 (spl/bpl/sil/dil-p r)
                 (spl/bpl/sil/dil-p b))
             (emit-byte segment (logior #x40 wrxb))
             (setf (segment-encoder-state segment) +rex+))
            (t
             (setf (segment-encoder-state segment) nil))))))

;;; Emit any instruction prefixes as required.
;;; THING is a register or memory operand of some kind. It and REG
;;; are always passed to EMIT-REX-IF-NEEDED.
;;; Additionally, if THING is an EA we pass its index and base registers;
;;; if it is a register TN, we pass only itself, as the 'B' register.
;;; If THING is a stack TN, neither it nor any of its components are passed
;;; to EMIT-REX-IF-NEEDED, which works correctly because stack references
;;; always use RBP as the base register and never use an index register,
;;; so no extended registers need to be accessed.
;;; Fixups are assembled using an addressing mode of displacement-only or
;;; RIP-plus-displacement (see EMIT-EA), so will not reference an extended
;;; register.
(defun emit-prefixes (segment thing reg operand-size &key lock)
  (declare (type (or tn reg ea fixup null) thing)
           (type (or tn reg integer null) reg)
           (type (member :byte :word :dword :qword :do-not-set) operand-size))
  ;; Legacy prefixes are order-insensitive, but let's approximately match the
  ;; output of the system assembler for consistency's sake.
  (when (and (ea-p thing) (eq (ea-segment thing) :gs))
    (emit-byte segment #x65))
  (when (eq operand-size :word)
    (emit-byte segment #x66))
  (ecase lock
    ((nil))
    (:lock (emit-byte segment #xf0))) ; even if #-sb-thread
  (let ((ea-p (ea-p thing)))
    (emit-rex-if-needed segment
                        (eq operand-size :qword) ; REX.W
                        (when (register-p reg)
                          (reg-id reg)) ; REX.R
                        (awhen (and ea-p (ea-index thing)) ; REX.X
                          (reg-id (tn-reg it)))
                        (cond (ea-p ; REX.B
                               (let ((base (ea-base thing)))
                                 (when (and base (neq base rip-tn))
                                   (reg-id (tn-reg base)))))
                              ((register-p thing)
                               (reg-id thing))))))

(defun operand-size (thing)
  (typecase thing
    (tn
     (or (sb-c:sc-operand-size (tn-sc thing))
         (error "can't tell the size of ~S" thing)))
    (reg
     (let ((id (reg-id thing)))
       (when (is-gpr-id-p id)
         (aref #(:qword :dword :word :byte) (gpr-id-size-class id)))))
    (fixup
     ;; GNA.  Guess who spelt "flavor" correctly first time round?
     ;; There's a strong argument in my mind to change all uses of
     ;; "flavor" to "kind": and similarly with some misguided uses of
     ;; "type" here and there.  -- CSR, 2005-01-06.
     (case (fixup-flavor thing)
       ((:foreign-dataref) :qword)))
    (t
     nil)))

;;; Except in a very few cases (MOV instructions A1, A3 and B8 - BF)
;;; we expect dword immediate operands even for 64 bit operations.
;;; Those opcodes call EMIT-QWORD directly. All other uses of :qword
;;; constants should fit in a :dword
(defun emit-imm-operand (segment value size)
  ;; In order by descending popularity
  (ecase size
    (:byte  (emit-byte segment value))
    (:dword (emit-dword segment value))
    (:qword (emit-signed-dword segment value))
    (:word  (emit-word segment value))))

;;;; prefixes

(define-instruction rex (segment)
  (:printer rex () nil :print-name nil))

(define-instruction x66 (segment)
  (:printer x66 () nil :print-name nil))

(define-instruction fs (segment)
  (:printer byte ((op #x64 :prefilter (lambda (dstate value)
                                        (declare (ignore value))
                                        (dstate-setprop dstate +fs-segment+))))
            nil :print-name nil))
(define-instruction gs (segment)
  (:printer byte ((op #x65 :prefilter (lambda (dstate value)
                                        (declare (ignore value))
                                        (dstate-setprop dstate +gs-segment+))))
            nil :print-name nil))

(define-instruction lock (segment)
  (:printer byte ((op #xF0)) nil))

(define-instruction rep (segment)
  (:emitter (emit-byte segment #xF3)))

(define-instruction repe (segment)
  (:printer byte ((op #xF3)) nil)
  (:emitter (emit-byte segment #xF3)))

(define-instruction repne (segment)
  (:printer byte ((op #xF2)) nil)
  (:emitter (emit-byte segment #xF2)))

;;;; general data transfer

;;; Cast THING into a diferent size so that EMIT-PREFIXES can decide whether
;;; it's looking at a :BYTE register that requires a REX prefix.
(defun sized-thing (thing size)
  (if (and (gpr-p thing) (neq (operand-size thing) size))
      (get-gpr size (reg-id-num (reg-id thing)))
      thing))

;;; This function SHOULD NOT BE USED. It is only for compatibility.
(defun sb-vm::reg-in-size (tn size)
  ;; We don't put internal functions through a deprecation cycle.
  ;; This should annoy maintainers enough to remove their misuses of this.
  (warn "Don't use REG-IN-SIZE")
  (sized-thing (tn-reg tn) size))

(define-instruction mov (segment &prefix prefix dst src)
  ;; immediate to register
  (:printer reg ((op #b1011 :prefilter (lambda (dstate value)
                                         (dstate-setprop dstate +allow-qword-imm+)
                                         value))
                 (imm nil :type 'signed-imm-data))
            '(:name :tab reg ", " imm))
  ;; register to/from register/memory
  (:printer reg-reg/mem-dir ((op #b100010)))
  ;; immediate to register/memory
  (:printer reg/mem-imm ((op '(#b1100011 #b000))))
  (:emitter
   (let ((size (pick-operand-size prefix dst src)))
     (emit-mov-instruction segment size (sized-thing dst size) (sized-thing src size)))))

(defun emit-mov-instruction (segment size dst src)
  (cond ((gpr-p dst)
            (cond ((integerp src)
                   ;; We want to encode the immediate using the fewest bytes possible.
                   (let ((imm-size
                          ;; If it's a :qword constant that fits in an unsigned
                          ;; :dword, then use a zero-extended :dword immediate.
                          (if (and (eq size :qword) (typep src '(unsigned-byte 32)))
                              :dword
                              size)))
                     (emit-prefixes segment dst nil imm-size))
                   (acond ((neq size :qword) ; :dword or smaller dst is straightforward
                           (emit-byte+reg segment (if (eq size :byte) #xB0 #xB8) dst)
                           (emit-imm-operand segment src size))
                          ;; This must be move to a :qword register.
                          ((typep src '(unsigned-byte 32))
                           ;; Encode as B8+dst using operand size of 32 bits
                           ;; and implicit zero-extension.
                           ;; Instruction size: 5 if no REX prefix, or 6 with.
                           (emit-byte+reg segment #xB8 dst)
                           (emit-dword segment src))
                          ((plausible-signed-imm32-operand-p src)
                           ;; It's either a signed-byte-32, or a large unsigned
                           ;; value whose 33 high bits are all 1.
                           ;; Encode as C7 which sign-extends a 32-bit imm to 64 bits.
                           ;; Instruction size: 7 bytes.
                           (emit-byte segment #xC7)
                           (emit-mod-reg-r/m-byte segment #b11 #b000
                                                  (reg-encoding dst segment))
                           (emit-signed-dword segment it))
                          (t
                           ;; 64-bit immediate. Instruction size: 10 bytes.
                           (emit-byte+reg segment #xB8 dst)
                           (emit-qword segment src))))
                  ((fixup-p src) ; treat as a 32-bit unsigned integer
                   ;; But imm-to-reg could take a 64-bit operand if needed.
                   (emit-prefixes segment dst nil :dword)
                   (emit-byte+reg segment #xB8 dst)
                   (emit-absolute-fixup segment src))
                  (t
                   (emit-prefixes segment src dst size)
                   (emit-byte segment (opcode+size-bit #x8A size))
                   (emit-ea segment src dst))))
        ((integerp src) ; imm to memory
            ;; C7 only deals with 32 bit immediates even if the
            ;; destination is a 64-bit location. The value is
            ;; sign-extended in this case.
            (let ((imm-size (if (eq size :qword) :dword size))
                  ;; If IMMEDIATE32-P returns NIL, use the original value,
                  ;; which will signal an error in EMIT-IMMEDIATE
                  (imm-val (or (and (eq size :qword)
                                    (plausible-signed-imm32-operand-p src))
                               src)))
              (emit-prefixes segment dst nil size)
              (emit-byte segment (opcode+size-bit #xC6 size))
              ;; The EA could be RIP-relative, thus it is important
              ;; to get :REMAINING-BYTES correct.
              (emit-ea segment dst #b000 :remaining-bytes (size-nbyte imm-size))
              (emit-imm-operand segment imm-val imm-size)))
        ((gpr-p src) ; reg to mem
            (emit-prefixes segment dst src size)
            (emit-byte segment (opcode+size-bit #x88 size))
            (emit-ea segment dst src))
        ((fixup-p src) ; equivalent to 32-bit unsigned integer
            ;; imm-to-mem can not take a 64-bit operand, but could sign-extend
            ;; to 8 bytes, which is probably not what you want.
            (emit-prefixes segment dst nil size)
            (emit-byte segment #xC7)
            (emit-ea segment dst #b000)
            (emit-absolute-fixup segment src))
        (t
            (error "bogus arguments to MOV: ~S ~S" dst src))))

;;; MOVABS is not a mnemonic according to the CPU vendors, but every (dis)assembler
;;; in popular use chooses this mnemonic instead of MOV with an 8-byte operand.
;;; (Even with Intel-compatible syntax, LLVM produces MOVABS).
;;; A possible motive is that it makes round-trip disassembly + reassembly faithful
;;; to the original encoding.  If MOVABS were rendered as MOV on account of the
;;; operand fitting by chance in 4 bytes, then information loss would occur.
;;; On the other hand, information loss occurs with other operands whose immediate
;;; value could fit in 1 byte or 4 bytes, so I don't know that that's the full
;;; reasoning. But in this disassembler anyway, an EA holds only a 32-bit integer
;;; so it doesn't really work to shoehorn this into the MOV instruction emitter.
(define-instruction movabs (segment dst src)
  ;; absolute mem to/from accumulator
  (:printer simple-dir ((op #b101000) (imm nil :type 'imm-addr))
            `(:name :tab ,(swap-if 'dir 'accum ", " '("[" imm "]"))))
  (:emitter
   (multiple-value-bind (reg ea dir-bit)
       (if (gpr-p dst) (values dst src 0) (values src dst 2))
     (aver (and (accumulator-p reg) (typep ea 'word)))
     (let ((size (operand-size reg)))
       (emit-prefixes segment nil nil size)
       (emit-byte segment (logior (opcode+size-bit #xA0 size) dir-bit))
       (emit-qword segment ea)))))

;; MOV[SZ]X - #x66 or REX selects the destination REG size, wherein :byte isn't
;; a possibility.  The 'width' bit selects a source r/m size of :byte or :word.
(define-instruction-format
    (move-with-extension 24 :include ext-reg-reg/mem
     :default-printer
     '(:name :tab reg ", "
       (:cond ((width :constant 0) (:using #'print-sized-byte-reg/mem reg/mem))
              (t (:using #'print-sized-word-reg/mem reg/mem)))))
  (width :prefilter nil)) ; doesn't affect DSTATE

;;; Emit a sign-extending (if SIGNED-P is true) or zero-extending move.
(flet ((emit* (segment sizes dst src signed-p)
         (aver (gpr-p dst))
         (let ((dst-size (cadr sizes)) ; DST-SIZE size governs the OPERAND-SIZE
               (src-size (car sizes))) ; SRC-SIZE is controlled by the opcode
           (aver (> (size-nbyte dst-size) (size-nbyte src-size)))
           (emit-prefixes segment (sized-thing src src-size) dst dst-size)
           (if (eq src-size :dword)
               ;; AMD calls this MOVSXD. If emitted without REX.W, it writes
               ;; only 32 bits. That is discouraged, and we don't do it.
               ;; (As checked by the AVER that dst is strictly larger than src)
               (emit-byte segment #x63)
               (emit-bytes segment #x0F
                           (opcode+size-bit (if signed-p #xBE #xB6) src-size)))
           (emit-ea segment src dst))))

  ;; Mnemonic: Intel specifies [V]PMOV[SZ]sd where 's' and 'd' denote the src
  ;; size and dst size, though data movement is from operand 2 to operand 1.
  ;; This strives to match that, using separate keywords rather than letters.
  (define-instruction movsx (segment sizes dst src)
    (:printer move-with-extension ((op #b1011111)))
    (:printer reg-reg/mem ((op #b0110001) (width 1)
                           (reg/mem nil :type 'sized-dword-reg/mem)))
    (:emitter (emit* segment sizes dst src t)))

  (define-instruction movzx (segment sizes dst src)
    (:printer move-with-extension ((op #b1011011)))
    (:emitter (aver (not (equal sizes '(:dword :qword)))) ; should use MOV instead
              (emit* segment sizes dst src nil))))

(flet ((emit* (segment thing gpr-opcode mem-opcode subcode)
         (let ((size
                ;; Immediate SYMBOL in immobile-space will fail in OPERAND-SIZE
                ;; because SC-OPERAND-SIZE = NIL. Push (make-fixup x :immobile-symbol))
                ;; would work, but requires recording it in code-fixups.
                (cond ((and (tn-p thing)
                            (sc-is thing sb-vm::immediate)
                            (symbolp (tn-value thing)))
                       :qword)
                      ((operand-size thing))
                      (t :qword))))
           (aver (or (eq size :qword) (eq size :word)))
           (emit-prefixes segment thing nil (if (eq size :word) :word :do-not-set))
           (cond ((gpr-p thing)
                  (emit-byte+reg segment gpr-opcode thing))
                 (t
                  (emit-byte segment mem-opcode)
                  (emit-ea segment thing subcode))))))
  (define-instruction push (segment src)
    ;; register
    (:printer reg-no-width-default-qword ((op #b01010)))
    ;; register/memory
    (:printer reg/mem-default-qword ((op '(#xFF 6))))
    ;; immediate
    (:printer byte ((op #b01101010) (imm nil :type 'signed-imm-byte))
              '(:name :tab imm))
    (:printer byte ((op #b01101000)
                    (imm nil :type 'signed-imm-data-default-qword))
              '(:name :tab imm))
    ;; ### segment registers?
    (:emitter
     (cond ((integerp src)
            ;; REX.W is not needed for :qword immediates because the default
            ;; operand size is 64 bits and the immediate value (8 or 32 bits)
            ;; is always sign-extended.
            (binding* ((imm (or (plausible-signed-imm32-operand-p src) src))
                       ((opcode operand-size)
                        (if (typep imm '(signed-byte 8))
                            (values #x6A :byte)
                            (values #x68 :qword))))
              (emit-byte segment opcode)
              (emit-imm-operand segment imm operand-size)))
           ((fixup-p src)
            (emit-byte segment #x68)
            (emit-absolute-fixup segment src))
           (t
            (emit* segment src #x50 #xFF 6)))))

  (define-instruction pop (segment dst)
    (:printer reg-no-width-default-qword ((op #b01011)))
    (:printer reg/mem-default-qword ((op '(#x8F 0))))
    (:emitter (emit* segment dst #x58 #x8F 0))))

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
(define-instruction xchg (segment &prefix prefix operand1 operand2)
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
   ;; Not parsing a :LOCK prefix is ok, because:
   ;;   "If a memory operand is referenced, the processor's locking protocol is automatically
   ;;    implemented for the duration of the exchange operation, regardless of the presence
   ;;    or absence of the LOCK prefix or of the value of the IOPL."
   (let ((size (pick-operand-size prefix operand1 operand2)))
     (labels ((xchg-acc-with-something (acc something)
                (if (and (not (eq size :byte))
                         (gpr-p something)
                         ;; Don't use the short encoding for XCHG EAX, EAX:
                         (not (and (accumulator-p something)
                                   (eq size :dword))))
                    (progn
                      (emit-prefixes segment something acc size)
                      (emit-byte+reg segment #x90 something))
                    (xchg-reg-with-something acc something)))
              (xchg-reg-with-something (reg something)
                (emit-prefixes segment something reg size)
                (emit-byte segment (opcode+size-bit #x86 size))
                (emit-ea segment something reg)))
       (cond ((accumulator-p operand1)
              (xchg-acc-with-something operand1 operand2))
             ((accumulator-p operand2)
              (xchg-acc-with-something operand2 operand1))
             ((gpr-p operand1)
              (xchg-reg-with-something operand1 operand2))
             ((gpr-p operand2)
              (xchg-reg-with-something operand2 operand1))
             (t
              (error "bogus args to XCHG: ~S ~S" operand1 operand2)))))))

(define-instruction lea (segment &prefix prefix dst src)
  (:printer
   reg-reg/mem
   ((op #b1000110) (width 1)
    (reg/mem nil :use-label #'lea-compute-label :printer #'lea-print-ea)))
  (:emitter
   (let ((size (pick-operand-size prefix dst)))
     (aver (member size '(:dword :qword)))
     (emit-prefixes segment src dst size)
     (emit-byte segment #x8D)
     (emit-ea segment src dst))))

(define-instruction cmpxchg (segment &prefix prefix dst src)
  ;; Register/Memory with Register.
  (:printer ext-reg-reg/mem ((op #b1011000)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (let ((size (pick-operand-size prefix dst src)))
     (aver (gpr-p src))
     (emit-prefixes segment dst (sized-thing src size) size :lock (lockp prefix))
     (emit-bytes segment #x0F (opcode+size-bit #xB0 size))
     (emit-ea segment dst src))))

(define-instruction cmpxchg16b (segment &prefix prefix mem)
  (:printer ext-reg/mem-no-width ((op '(#xC7 1))))
  (:emitter
   (aver (not (gpr-p mem)))
   (emit-prefixes segment mem nil :qword :lock (lockp prefix))
   (emit-bytes segment #x0F #xC7)
   (emit-ea segment mem 1)))

(define-instruction rdrand (segment dst)
  (:printer ext-reg/mem-no-width ((op '(#xC7 6))))
  (:emitter
   (aver (gpr-p dst))
   (emit-prefixes segment dst nil (operand-size dst))
   (emit-bytes segment #x0F #xC7)
   (emit-ea segment dst 6)))

;;;; flag control instructions

(macrolet ((def (mnemonic opcode)
             `(define-instruction ,mnemonic (segment)
                (:printer byte ((op ,opcode)))
                (:emitter (emit-byte segment ,opcode)))))
  (def wait  #x9B) ; Wait.
  (def pushf #x9C) ; Push flags.
  (def popf  #x9D) ; Pop flags.
  (def sahf  #x9E) ; Store AH into flags.
  (def lahf  #x9F) ; Load AH from flags.
  (def icebp #xF1) ; ICE breakpoint
  (def hlt   #xF4) ; Halt
  (def cmc   #xF5) ; Complement Carry Flag.
  (def clc   #xF8) ; Clear Carry Flag.
  (def stc   #xF9) ; Set Carry Flag.
  (def cli   #xFA) ; Clear Iterrupt Enable Flag.
  (def sti   #xFB) ; Set Interrupt Enable Flag.
  (def cld   #xFC) ; Clear Direction Flag.
  (def std   #xFD) ; Set Direction Flag.
)

;;;; arithmetic

(flet ((emit* (name segment prefix dst src opcode)
         (let ((size (pick-operand-size prefix dst src)))
           (setq src (sized-thing src size)
                 dst (sized-thing dst size))
           (acond
            ((and (neq size :byte) (plausible-signed-imm8-operand-p src size))
             (emit-prefixes segment dst nil size :lock (lockp prefix))
             (emit-byte segment #x83)
             (emit-ea segment dst opcode :remaining-bytes 1)
             (emit-byte segment it))
            ((or (integerp src)
                 (and (fixup-p src)
                      (memq (fixup-flavor src)
                            '(:layout-id :layout :immobile-symbol :symbol-tls-index
                              :card-table-index-mask))))
             (emit-prefixes segment dst nil size :lock (lockp prefix))
             (cond ((accumulator-p dst)
                    (emit-byte segment
                               (opcode+size-bit (dpb opcode (byte 3 3) #b00000100)
                                                size)))
                   (t
                    (emit-byte segment (opcode+size-bit #x80 size))
                    (emit-ea segment dst opcode)))
             (if (fixup-p src)
                 (emit-absolute-fixup segment src)
                 (let ((imm (or (and (eq size :qword)
                                     (plausible-signed-imm32-operand-p src))
                                src)))
                   (emit-imm-operand segment imm size))))
            (t
             (multiple-value-bind (reg/mem reg dir-bit)
                 (cond ((gpr-p src) (values dst src #b00))
                       ((gpr-p dst) (values src dst #b10))
                       (t (error "bogus operands to ~A" name)))
               (emit-prefixes segment reg/mem reg size :lock (lockp prefix))
               (emit-byte segment
                          (opcode+size-bit (dpb opcode (byte 3 3) dir-bit) size))
               (emit-ea segment reg/mem reg)))))))
  (macrolet ((define (name subop)
               `(define-instruction ,name (segment &prefix prefix dst src)
                  (:printer accum-imm ((op ,(dpb subop (byte 3 2) #b0000010))))
                  (:printer reg/mem-imm ((op '(#b1000000 ,subop))))
                  ;; The redundant encoding #x82 is invalid in 64-bit mode,
                  ;; therefore we force WIDTH to 1.
                  (:printer reg/mem-imm ((op '(#b1000001 ,subop)) (width 1)
                                         (imm nil :type 'signed-imm-byte)))
                  (:printer reg-reg/mem-dir ((op ,(dpb subop (byte 3 1) #b000000))))
                  (:emitter (emit* ,(string name) segment prefix dst src ,subop)))))
    (define add #b000)
    (define adc #b010)
    (define sub #b101)
    (define sbb #b011)
    (define cmp #b111)
    (define and #b100)
    (define or  #b001)
    (define xor #b110)))

(flet ((emit* (segment prefix dst opcode subcode)
         (let ((size (pick-operand-size prefix dst)))
           (emit-prefixes segment (sized-thing dst size) nil size :lock (lockp prefix))
           (emit-byte segment (opcode+size-bit (ash opcode 1) size))
           (emit-ea segment dst subcode))))
  (define-instruction not (segment &prefix prefix dst)
    (:printer reg/mem ((op '(#b1111011 #b010))))
    (:emitter (emit* segment prefix dst #b1111011 #b010)))
  (define-instruction neg (segment &prefix prefix dst)
    (:printer reg/mem ((op '(#b1111011 #b011))))
    (:emitter (emit* segment prefix dst #b1111011 #b011)))
  ;; The one-byte encodings for INC and DEC are used as REX prefixes
  ;; in 64-bit mode so we always use the two-byte form.
  (define-instruction inc (segment &prefix prefix dst)
    (:printer reg/mem ((op '(#b1111111 #b000))))
    (:emitter (emit* segment prefix dst #b1111111 #b000)))
  (define-instruction dec (segment &prefix prefix dst)
    (:printer reg/mem ((op '(#b1111111 #b001))))
    (:emitter (emit* segment prefix dst #b1111111 #b001))))

(define-instruction-format (imul-3-operand 16 :include reg-reg/mem)
  (op    :fields (list (byte 6 2) (byte 1 0)) :value '(#b011010 1))
  (width :field (byte 1 1)
         :prefilter (lambda (dstate value)
                      (unless (eql value 0)
                        (dstate-setprop dstate +imm-size-8+))))
  (imm   :prefilter
         (lambda (dstate)
           (let ((nbytes
                  (if (dstate-getprop dstate +imm-size-8+)
                      1
                      (min 4 (size-nbyte (inst-operand-size dstate))))))
             (read-signed-suffix (* nbytes 8) dstate)))))

(define-instruction imul (segment &prefix prefix dst &optional src imm)
  ;; 1-operand form of IMUL produces a double-precision result in rDX:rAX.
  ;; The default accum-reg/mem printer would be very misleading, as it would print
  ;; something like "IMUL EAX, [mem]" which implies a 32-bit result.
  (:printer accum-reg/mem ((op '(#b1111011 #b101)))
            '(:name :tab (:using #'print-sized-reg/mem reg/mem)))
  (:printer ext-reg-reg/mem-no-width ((op #xAF))) ; 2-operand
  (:printer imul-3-operand () '(:name :tab reg ", " reg/mem ", " imm))
  (:emitter
   (let ((operand-size (pick-operand-size prefix dst src)))
     (cond ((not src) ; 1-operand form affects RDX:RAX or subregisters thereof
            (aver (not imm))
            (emit-prefixes segment dst nil (or operand-size :qword))
            (emit-byte segment (opcode+size-bit #xF6 operand-size))
            (emit-ea segment dst #b101))
           (t
            (aver (neq operand-size :byte))
            ;; If two operands and the second is immediate, it's really 3-operand
            ;; form with the same dst and src, which has to be a register.
            (when (and (integerp src) (not imm))
              (setq imm src src dst))
            (let ((imm-size (if (typep imm '(signed-byte 8)) :byte operand-size)))
              (emit-prefixes segment src dst operand-size)
              (if imm
                  (emit-byte segment (if (eq imm-size :byte) #x6B #x69))
                  (emit-bytes segment #x0F #xAF))
              (emit-ea segment src dst :remaining-bytes (if imm (size-nbyte imm-size) 0))
              (if imm
                  (emit-imm-operand segment imm imm-size))))))))

(flet ((emit* (segment subcode prefix src junk)
         (when junk
           (warn "Do not supply 2 operands to 1-operand MUL,DIV")
           (aver (accumulator-p src))
           (setq src junk))
         (let ((size (pick-operand-size prefix src)))
           (emit-prefixes segment src nil size)
           (emit-byte segment (opcode+size-bit #xF6 size))
           (emit-ea segment src subcode))))
  (define-instruction mul (segment &prefix prefix src &optional junk)
    (:printer accum-reg/mem ((op '(#b1111011 #b100))))
    (:emitter (emit* segment #b100 prefix src junk)))
  (define-instruction div (segment &prefix prefix src &optional junk)
    (:printer accum-reg/mem ((op '(#b1111011 #b110))))
    (:emitter (emit* segment #b110 prefix src junk)))
  (define-instruction idiv (segment &prefix prefix src &optional junk)
    (:printer accum-reg/mem ((op '(#b1111011 #b111))))
    (:emitter (emit* segment #b111 prefix src junk))))

(define-instruction bswap (segment &prefix prefix dst)
  (:printer ext-reg-no-width ((op #b11001)))
  (:emitter
   (let ((size (pick-operand-size prefix dst)))
     (aver (member size '(:dword :qword)))
     (emit-prefixes segment dst nil size)
     (emit-byte segment #x0f)
     (emit-byte+reg segment #xC8 dst))))

;;; CBW -- Convert Byte to Word. AX <- sign_xtnd(AL)
(define-instruction cbw (segment)
  (:printer x66-byte ((op #x98)))
  (:emitter
   (emit-prefixes segment nil nil :word)
   (emit-byte segment #x98)))

;;; CWDE -- Convert Word To Double Word Extended. EAX <- sign_xtnd(AX)
(define-instruction cwde (segment)
  (:printer byte ((op #x98)))
  (:emitter (emit-byte segment #x98)))

;;; CDQE -- Convert Double Word To Quad Word Extended. RAX <- sign_xtnd(EAX)
(define-instruction cdqe (segment)
  (:printer rex-byte ((op #x98)))
  (:emitter
   (emit-prefixes segment nil nil :qword)
   (emit-byte segment #x98)))

;;; CWD -- Convert Word to Double Word. DX:AX <- sign_xtnd(AX)
(define-instruction cwd (segment)
  (:printer x66-byte ((op #x99)))
  (:emitter
   (emit-prefixes segment nil nil :word)
   (emit-byte segment #x99)))

;;; CDQ -- Convert Double Word to Quad Word. EDX:EAX <- sign_xtnd(EAX)
(define-instruction cdq (segment)
  (:printer byte ((op #x99)))
  (:emitter (emit-byte segment #x99)))

;;; CQO -- Convert Quad Word to Octaword. RDX:RAX <- sign_xtnd(RAX)
(define-instruction cqo (segment)
  (:printer rex-byte ((op #x99)))
  (:emitter
   (emit-prefixes segment nil nil :qword)
   (emit-byte segment #x99)))

(define-instruction xadd (segment &prefix prefix dst src)
  ;; Register/Memory with Register.
  (:printer ext-reg-reg/mem ((op #b1100000)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (aver (gpr-p src))
   (let ((size (pick-operand-size prefix dst src)))
     (emit-prefixes segment dst src size :lock (lockp prefix))
     (emit-bytes segment #x0F (opcode+size-bit #xC0 size))
     (emit-ea segment dst src))))


;;;; logic

(define-instruction-format
    (shift-inst 16 :include reg/mem
     :default-printer '(:name :tab reg/mem ", " (:if (variablep :positive) 'cl 1)))
  (op :fields (list (byte 6 2) (byte 3 11)))
  (variablep :field (byte 1 1)))

(flet ((emit* (segment prefix dst amount subcode)
         (let ((size (pick-operand-size prefix dst)))
           (multiple-value-bind (opcode immed)
               (case amount
                 (:cl (values #b11010010 nil))
                 (1   (values #b11010000 nil))
                 (t   (values #b11000000 t)))
             (emit-prefixes segment (sized-thing dst size) nil size)
             (emit-byte segment (opcode+size-bit opcode size))
             (emit-ea segment dst subcode :remaining-bytes (if immed 1 0))
             (when immed
               (emit-byte segment amount))))))
  (macrolet ((define (name subop)
               `(define-instruction ,name (segment &prefix prefix dst amount)
                  (:printer shift-inst ((op '(#b110100 ,subop)))) ; shift by CL or 1
                  (:printer reg/mem-imm ((op '(#b1100000 ,subop))
                                         (imm nil :type 'imm-byte)))
                  (:emitter (emit* segment prefix dst amount ,subop)))))
    (define rol #b000)
    (define ror #b001)
    (define rcl #b010)
    (define rcr #b011)
    (define shl #b100)
    (define shr #b101)
    (define sar #b111)))

(flet ((emit* (segment opcode prefix dst src amt)
         (let ((size (pick-operand-size prefix dst src)))
           (when (eq size :byte)
             (error "Double shift requires word or larger operand"))
           (emit-prefixes segment dst src size)
           (emit-bytes segment #x0F
                       ;; SHLD = A4 or A5; SHRD = AC or AD
                       (dpb opcode (byte 1 3) (if (eq amt :cl) #xA5 #xA4)))
           (emit-ea segment dst src :remaining-bytes (if (eq amt :cl) 0 1))
           (unless (eq amt :cl)
             (emit-byte segment (the (mod 64) amt))))))
  (macrolet ((define (name direction-bit op)
               `(define-instruction ,name (segment &prefix prefix dst src amt)
                  (:printer ext-reg-reg/mem-no-width ((op ,(logior op #b100))
                                                      (imm nil :type 'imm-byte))
                            '(:name :tab reg/mem ", " reg ", " imm))
                  (:printer ext-reg-reg/mem-no-width ((op ,(logior op #b101)))
                            '(:name :tab reg/mem ", " reg ", " 'cl))
                  (:emitter (emit* segment ,direction-bit prefix dst src amt)))))
    (define shld 0 #b10100000)
    (define shrd 1 #b10101000)))

(define-instruction test (segment &prefix prefix this that)
  (:printer accum-imm ((op #b1010100)))
  (:printer reg/mem-imm ((op '(#b1111011 #b000))))
  (:printer reg-reg/mem ((op #b1000010)))
  (:emitter
   (let ((size (pick-operand-size prefix this that)))
     (setq this (sized-thing this size)
           that (sized-thing that size))
     (when (and (gpr-p this) (mem-ref-p that))
       (rotatef this that))
     (emit-prefixes segment this that size)
     ;; gas disallows the constant as the first arg (in at&t syntax)
     ;; but does allow a memory arg as either operand.
     (cond ((integerp this) (error "Inverted arguments to TEST"))
           ((integerp that)
            ;; TEST has no form that sign-extends an 8-bit immediate,
            ;; so all we need to be concerned with is whether a positive
            ;; qword is bitwise equivalent to a signed dword.
            (awhen (and (eq size :qword) (plausible-signed-imm32-operand-p that))
              (setq that it))
            (cond ((accumulator-p this)
                   (emit-byte segment (opcode+size-bit #xA8 size)))
                  (t
                   (emit-byte segment (opcode+size-bit #xF6 size))
                   (emit-ea segment this #b000 :remaining-bytes 1)))
            (emit-imm-operand segment that size))
           (t
            (emit-byte segment (opcode+size-bit #x84 size))
            (emit-ea segment this that))))))

;;;; string manipulation

(flet ((emit* (segment opcode size)
         (emit-prefixes segment nil nil size)
         (emit-byte segment (opcode+size-bit (ash opcode 1) size))))
  (define-instruction movs (segment size)
    (:printer string-op ((op #b1010010)))
    (:emitter (emit* segment #b1010010 size)))

  (define-instruction cmps (segment size)
    (:printer string-op ((op #b1010011)))
    (:emitter (emit* segment #b1010011 size)))

  (define-instruction lods (segment size)
    (:printer string-op ((op #b1010110)))
    (:emitter (emit* segment #b1010110 size)))

  (define-instruction scas (segment size)
    (:printer string-op ((op #b1010111)))
    (:emitter (emit* segment #b1010111 size)))

  (define-instruction stos (segment size)
    (:printer string-op ((op #b1010101)))
    (:emitter (emit* segment #b1010101 size)))

  (define-instruction ins (segment size)
    (:printer string-op ((op #b0110110)))
    (:emitter (emit* segment #b0110110 size)))

  (define-instruction outs (segment size)
    (:printer string-op ((op #b0110111)))
    (:emitter (emit* segment #b0110111 size))))

(define-instruction xlat (segment)
  (:printer byte ((op #b11010111)))
  (:emitter
   (emit-byte segment #b11010111)))


;;;; bit manipulation

(flet ((emit* (segment opcode prefix dst src)
         (let ((size (pick-operand-size prefix dst src)))
           (when (eq size :byte)
             (error "can't scan bytes: ~S" src))
           (emit-prefixes segment src dst size)
           (emit-bytes segment #x0F opcode)
           (emit-ea segment src dst))))

  (define-instruction bsf (segment &prefix prefix dst src)
    (:printer ext-reg-reg/mem-no-width ((op #xBC)))
    (:emitter (emit* segment #xBC prefix dst src)))

  (define-instruction bsr (segment &prefix prefix dst src)
    (:printer ext-reg-reg/mem-no-width ((op #xBD)))
    (:emitter (emit* segment #xBD prefix dst src))))

(flet ((emit* (segment prefix src index opcode)
         (let ((size (pick-operand-size prefix src index)))
           (when (eq size :byte)
             (error "can't test byte: ~S" src))
           (emit-prefixes segment src index size :lock (lockp prefix))
           (cond ((integerp index)
                  (emit-bytes segment #x0F #xBA)
                  (emit-ea segment src opcode :remaining-bytes 1)
                  (emit-byte segment index))
                 (t
                  (emit-bytes segment #x0F (dpb opcode (byte 3 3) #b10000011))
                  (emit-ea segment src index))))))

  (macrolet ((define (inst opcode-extension)
               `(define-instruction ,inst (segment &prefix prefix src index)
                  (:printer ext-reg/mem-no-width+imm8
                            ((op '(#xBA ,opcode-extension))
                             (reg/mem nil :type 'sized-reg/mem)))
                  (:printer ext-reg-reg/mem-no-width
                            ((op ,(dpb opcode-extension (byte 3 3) #b10000011))
                             (reg/mem nil :type 'sized-reg/mem))
                            '(:name :tab reg/mem ", " reg))
                  (:emitter (emit* segment prefix src index ,opcode-extension)))))
    (define bt  4)
    (define bts 5)
    (define btr 6)
    (define btc 7)))


;;;; control transfer

(define-instruction call (segment where)
  (:printer near-jump ((op #xE8)))
  (:printer reg/mem-default-qword ((op '(#xFF #b010))
                                   (reg/mem nil :printer #'print-jmp-ea)))
  (:emitter
   (typecase where
     (label
      (emit-byte segment #xE8) ; 32 bit relative
      (emit-dword-displacement-backpatch segment where))
     (fixup
      (emit-byte segment #xE8)
      (emit-relative-fixup segment where))
     (t
      (emit-prefixes segment where nil :do-not-set)
      (emit-byte segment #xFF)
      (emit-ea segment where #b010)))))

(define-instruction jmp (segment cond &optional where)
  ;; conditional jumps
  (:printer short-cond-jump () '('j cc :tab label))
  (:printer near-cond-jump () '('j cc :tab label))
  ;; unconditional jumps
  (:printer short-jump ((op #b1011)))
  (:printer near-jump ((op #xE9)))
  (:printer reg/mem-default-qword ((op '(#xFF #b100))
                                   (reg/mem nil :printer #'print-jmp-ea)))
  (:emitter
   (flet ((byte-disp-p (source target disp shrinkage) ; T if 1-byte displacement
            ;; If the displacement is (signed-byte 8), then we have the answer.
            ;; Otherwise, if the displacement is positive and could be 1 byte,
            ;; then we check if the post-shrinkage value is in range.
            (or (typep disp '(signed-byte 8))
                (and (> disp 0)
                     (<= (- disp shrinkage) 127)
                     (not (any-alignment-between-p segment source target))))))
     (cond
        (where
          (cond ((fixup-p where)
                 (emit-bytes segment #x0F
                             (dpb (encoded-condition cond)
                                  (byte 4 0)
                                  #b10000000))
                 (emit-relative-fixup segment where))
                (t
                 (emit-chooser
                  segment 6 2 ; emit either 2 or 6 bytes
                  ;; The difference in encoding lengths is 4, therefore this
                  ;; preserves 4-byte alignment ("2 bits" as we put it).
                  (lambda (segment chooser posn delta-if-after)
                    (let ((disp (- (label-position where posn delta-if-after)
                                   (+ posn 2))))
                      (when (byte-disp-p chooser where disp 4)
                        (emit-byte segment
                                   (dpb (encoded-condition cond)
                                        (byte 4 0)
                                        #b01110000))
                        (emit-byte-displacement-backpatch segment where)
                        t)))
                  (lambda (segment posn)
                    (let ((disp (- (label-position where) (+ posn 6))))
                      (emit-bytes segment #x0F
                                  (dpb (encoded-condition cond)
                                       (byte 4 0)
                                       #b10000000))
                      (emit-signed-dword segment disp)))))))
         ((label-p (setq where cond))
          (emit-chooser
           segment 5 0 ; emit either 2 or 5 bytes; no alignment is preserved
           (lambda (segment chooser posn delta-if-after)
             (let ((disp (- (label-position where posn delta-if-after)
                            (+ posn 2))))
               (when (byte-disp-p chooser where disp 3)
                 (emit-byte segment #xEB)
                 (emit-byte-displacement-backpatch segment where)
                 t)))
           (lambda (segment posn)
             (let ((disp (- (label-position where) (+ posn 5))))
               (emit-byte segment #xE9)
               (emit-signed-dword segment disp)))))
         ((fixup-p where)
          (emit-byte segment #xE9)
          (emit-relative-fixup segment where))
         (t
          (unless (typep where '(or reg ea))
            (error "don't know what to do with ~A" where))
          ;; near jump defaults to 64 bit
          ;; w-bit in rex prefix is unnecessary
          (emit-prefixes segment where nil :do-not-set)
          (emit-byte segment #b11111111)
          (emit-ea segment where #b100))))))

(define-instruction ret (segment &optional stack-delta)
  (:printer byte ((op #xC3)))
  (:printer byte ((op #xC2) (imm nil :type 'imm-word-16)) '(:name :tab imm))
  (:emitter
   (cond ((and stack-delta (not (zerop stack-delta)))
          (emit-byte segment #xC2)
          (emit-word segment stack-delta))
         (t
          (emit-byte segment #xC3)))))

(define-instruction jrcxz (segment target)
  (:printer short-jump ((op #b0011)))
  (:emitter
   (emit-byte segment #xE3)
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loop (segment target)
  (:printer short-jump ((op #b0010)))
  (:emitter
   (emit-byte segment #xE2)
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loopz (segment target)
  (:printer short-jump ((op #b0001)))
  (:emitter
   (emit-byte segment #xE1)
   (emit-byte-displacement-backpatch segment target)))

(define-instruction loopnz (segment target)
  (:printer short-jump ((op #b0000)))
  (:emitter
   (emit-byte segment #xE0)
   (emit-byte-displacement-backpatch segment target)))

;;;; conditional move
(define-instruction cmov (segment &prefix prefix cond dst src)
  (:printer cond-move ())
  (:emitter
   (let ((size (pick-operand-size prefix dst src)))
     (aver (gpr-p dst))
     (aver (neq size :byte))
     (emit-prefixes segment src dst size))
   (emit-byte segment #x0F)
   (emit-byte segment (dpb (encoded-condition cond) (byte 4 0) #b01000000))
   (emit-ea segment src dst)))

;;;; conditional byte set

(define-instruction set (segment cond dst) ; argument order is like JMPcc
  (:printer cond-set ())
  (:emitter
   (emit-prefixes segment (sized-thing dst :byte) nil :byte)
   (emit-byte segment #x0F)
   (emit-byte segment (dpb (encoded-condition cond) (byte 4 0) #b10010000))
   (emit-ea segment dst #b000)))

;;;; enter/leave

(define-instruction enter (segment disp &optional (level 0))
  (:declare (type (unsigned-byte 16) disp)
            (type (unsigned-byte 8) level))
  (:printer enter-format ((op #xC8)))
  (:emitter
   (emit-byte segment #xC8)
   (emit-word segment disp)
   (emit-byte segment level)))

(define-instruction leave (segment)
  (:printer byte ((op #xC9)))
  (:emitter (emit-byte segment #xC9)))

;;;; interrupt instructions

;;; The default interrupt instruction is INT3 which signals SIGTRAP.
;;; This makes for a lot of trouble when using gdb to debug lisp, because gdb really wants
;;; to use SIGTRAP for itself. And allegedly there were OSes where SIGTRAP was unreliable
;;; but I have never seen it, other than it being intercepted by gdb.
;;; (Maybe that's what someone meant by "unreliable"?)
;;; So depending on your requirement, SIGILL can be raised instead via either the INTO
;;; instruction which is illegal on amd64, or UD2 for compabitility with 32-bit code.
;;; UD2 is not needed on amd64 but is on 32-bit where INTO is a legal instruction.
;;; However, if trying to debug code which also gets an "actual" SIGILL, this still poses
;;; a problem for gdb. To workaround that we can emit a call to a asm routine which
;;; has essentially the same effect as the signal.
;;; Orthogonal to the preceding choices, INT1 can be used for pseudo-atomic-interrupted
;;; but that doesn't work on all systems.
(define-instruction break (segment &optional (code nil codep))
  (:printer byte-imm ((op #xCC)) :default :print-name 'int3 :control #'break-control)
  (:printer word-imm ((op #x0B0F)) :default :print-name 'ud2 :control #'break-control)
  ;; INTO always signals SIGILL in 64-bit code. Using it avoids conflicting with gdb's
  ;; use of sigtrap and shortens the error break by 1 byte relative to UD2.
  (:printer byte-imm ((op #xCE)) :default :print-name 'into :control #'break-control)
  (:emitter
   #+sw-int-avoidance ; emit CALL [EA] to skip over the trap instruction
   (let ((where (ea (make-fixup 'sb-vm::synchronous-trap :assembly-routine))))
     (emit-prefixes segment where nil :do-not-set)
     (emit-byte segment #xFF)
     (emit-ea segment where #b010))
   #-ud2-breakpoints (emit-byte segment (or #+int4-breakpoints #xCE #xCC))
   #+ud2-breakpoints (emit-word segment #x0B0F)
   (when codep (emit-byte segment (the (unsigned-byte 8) code)))))

(define-instruction int (segment number)
  (:declare (type (unsigned-byte 8) number))
  (:printer byte-imm ((op #xCD)))
  (:emitter
   (etypecase number
     ((member 3 4)
      (emit-byte segment (if (eql number 4) #xCE #xCC)))
     ((unsigned-byte 8)
      (emit-bytes segment #xCD number)))))

(define-instruction ud0 (segment reg rm)
  (:printer ext-reg-reg/mem-no-width ((op #xFF)))
  (:emitter
   (emit-prefixes segment rm reg :dword)
   (emit-bytes segment #x0F #xFF)
   (emit-ea segment rm reg)))

(define-instruction ud1 (segment reg rm)
  (:printer ext-reg-reg/mem-no-width ((op #xB9)))
  (:emitter
   (emit-prefixes segment rm reg :dword)
   (emit-bytes segment #x0F #xB9)
   (emit-ea segment rm reg)))

(define-instruction iret (segment)
  (:printer byte ((op #xCF)))
  (:emitter (emit-byte segment #xCF)))

;;;; processor control

(define-instruction nop (segment)
  (:printer byte ((op #x90)))
  ;; multi-byte NOP
  (:printer ext-reg/mem-no-width ((op '(#x1f 0))) '(:name))
  (:emitter (emit-byte segment #x90)))

;;; Emit a sequence of single- or multi-byte NOPs to fill AMOUNT many
;;; bytes with the smallest possible number of such instructions.
(defun emit-long-nop (segment amount)
  (declare (type sb-assem:segment segment)
           (type index amount))
  ;; Pack all instructions into one byte vector to save space.
  (let* ((bytes #.(coerce
                   #(#x90
                     #x66 #x90
                     #x0f #x1f #x00
                     #x0f #x1f #x40 #x00
                     #x0f #x1f #x44 #x00 #x00
                     #x66 #x0f #x1f #x44 #x00 #x00
                     #x0f #x1f #x80 #x00 #x00 #x00 #x00
                     #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00
                     #x66 #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00)
                   '(vector (unsigned-byte 8))))
         (max-length 9))
    (loop
      (let* ((count
              ;; Disassembly looks better if encodings are 8 bytes or fewer,
              ;; so when 10 to 15 bytes remain, emit two more NOPs of roughly
              ;; equal length rather than say a 9-byte + 1-byte.
              (if (<= 10 amount 15)
                  (ceiling amount 2)
                  (min amount max-length)))
             (start (ash (* count (1- count)) -1)))
        (dotimes (i count)
          (emit-byte segment (aref bytes (+ start i))))
        (when (zerop (decf amount count))
          (return))))))

(define-instruction syscall (segment)
  (:printer two-bytes ((op '(#x0F #x05))))
  (:emitter (emit-bytes segment #x0F #x05)))


;;;; miscellaneous hackery

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

;;; Compute the distance backward to the base address of the code component
;;; containing this simple-fun header, measured in words.
(defun emit-header-data (segment type)
  (emit-back-patch segment
                   n-word-bytes
                   (lambda (segment posn)
                     (emit-qword
                      segment
                      (logior type
                              (ash (+ posn (- (component-header-length)
                                              (segment-header-skew segment)))
                                   (- n-widetag-bits word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  (:emitter
   (emit-header-data segment simple-fun-widetag)))

;;;; Instructions required to do floating point operations using SSE

;;; Return a :PRINTER expression for SSE instructions.
(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun sse-inst-printer (inst-format-stem prefix opcode &key more-fields printer)
    (let ((fields `(,@(when prefix `((prefix ,prefix)))
                    (op ,opcode)
                    ,@more-fields)))
      `((:printer ,(if prefix (symbolicate "EXT-" inst-format-stem) inst-format-stem)
                  ,fields ,@(if printer `(',printer))))))
  (defun 2byte-sse-inst-printer (inst-format-stem prefix op1 op2 &key more-fields printer)
    (let ((fields `(,@(when prefix `((prefix, prefix)))
                    (op1 ,op1)
                    (op2 ,op2)
                    ,@more-fields)))
      `((:printer ,(if prefix (symbolicate "EXT-" inst-format-stem) inst-format-stem)
                  ,fields ,@(if printer `(',printer)))))))

(defun emit-sse-inst (segment dst src prefix opcode
                      &key operand-size (remaining-bytes 0))
  (when prefix
    (emit-byte segment prefix))
  (emit-prefixes segment src dst (or operand-size (operand-size src)))
  (emit-bytes segment #x0f opcode)
  (emit-ea segment src dst :remaining-bytes remaining-bytes))

;; 0110 0110:0000 1111:0111 00gg: 11 010 xmmreg:imm8

(defun emit-sse-inst-with-imm (segment dst/src imm
                               prefix opcode /i
                               &key operand-size)
  (aver (<= 0 /i 7))
  (when prefix
    (emit-byte segment prefix))
  ;; dst/src is encoded in the r/m field, not r; REX.B must be
  ;; set to use extended XMM registers
  (emit-prefixes segment dst/src nil operand-size)
  (emit-byte segment #x0F)
  (emit-byte segment opcode)
  (emit-byte segment (logior (ash (logior #b11000 /i) 3)
                             (reg-encoding dst/src segment)))
  (emit-byte segment imm))

(defun emit-sse-inst-2byte (segment dst src prefix op1 op2
                            &key operand-size (remaining-bytes 0))
  (when prefix
    (emit-byte segment prefix))
  (emit-prefixes segment src dst (or operand-size (operand-size src)))
  (emit-byte segment #x0f)
  (emit-byte segment op1)
  (emit-byte segment op2)
  (emit-ea segment src dst :remaining-bytes remaining-bytes))

(macrolet
    ((define-imm-sse-instruction (name opcode /i)
         `(define-instruction ,name (segment dst/src imm)
            ,@(sse-inst-printer 'xmm-imm #x66 opcode :more-fields `((/i ,/i)))
            (:emitter
             (emit-sse-inst-with-imm segment dst/src imm
                                     #x66 ,opcode ,/i
                                     :operand-size :do-not-set)))))
  ;; FIXME: why did someone decide to invent new mnemonics for the immediate forms?
  ;; Can we put them back to normal?
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
                ,@(sse-inst-printer 'xmm-xmm/mem prefix opcode)
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
                    ,@(sse-inst-printer
                        'xmm-xmm/mem prefix opcode
                        :more-fields `((imm nil :type ',shuffle-pattern))
                        :printer '(:name :tab reg ", " reg/mem ", " imm))

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
  (:emitter
   (aver (xmm-register-p src))
   (aver (xmm-register-p mask))
   (emit-regular-sse-inst segment src mask #x66 #xf7))
  . #.(sse-inst-printer 'xmm-xmm/mem #x66 #xf7))

(macrolet ((define-comparison-sse-inst (name prefix opcode
                                        name-prefix name-suffix)
               `(define-instruction ,name (segment op x y)
                  ,@(sse-inst-printer
                      'xmm-xmm/mem prefix opcode
                      :more-fields '((imm nil :type 'sse-condition-code))
                      :printer `(,name-prefix imm ,name-suffix
                                 :tab reg ", " reg/mem))
                  (:emitter
                   (let ((code (position op +sse-conditions+)))
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
                ,@(sse-inst-printer 'xmm-xmm/mem-dir prefix #b0001000)
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
                    ,@(when opcode-from
                        (sse-inst-printer 'xmm-xmm/mem prefix opcode-from))
                    ,@(sse-inst-printer
                          'xmm-xmm/mem prefix opcode-to
                          :printer '(:name :tab reg/mem ", " reg))
                    (:emitter
                     (cond ,@(when opcode-from
                               `(((xmm-register-p dst)
                                  ,(when force-to-mem
                                     `(aver (not (register-p src))))
                                  (emit-regular-sse-inst
                                   segment dst src ,prefix ,opcode-from))))
                           (t
                            (aver (xmm-register-p src))
                            ,(when force-to-mem
                               `(aver (not (register-p dst))))
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
  (:emitter
   (aver (and (xmm-register-p dst)
              (not (xmm-register-p src))))
   (emit-regular-2byte-sse-inst segment dst src #x66 #x38 #x2a))
  . #.(2byte-sse-inst-printer '2byte-xmm-xmm/mem #x66 #x38 #x2a))

;;; Move a 32-bit value (MOVD) or 64-bit value (MOVQ)
;;; MOVD has encodings for xmm <-> r/m32.
;;; MOVQ has encodings for xmm <-> r/m64 and xmm <-> xmm/m64.
;;; (We do not support the encodings for MMX registers)
;;;
;;; Troubles stem from a difference in interpretation.
;;; AMD's suggests the following mapping of mnemonic to functionality:
;;;  * MOVD = the encoding "66 0f 6e" or "66 0f 7e" (chosen by direction)
;;;  * MOVQ = the encoding "f3 0f 7e" or "66 0f d6" (chosen by direction)
;;; whereas Intel (more mnemonically) documents these as:
;;;  * MOVD = movement of a doubleword
;;;  * MOVQ = movement of a quadword, having 4 encodings
;;;
;;; Additionally, AMD documents that "their" MOVD has encodings for
;;; "xmm <-> r/m64" (as REX.W+MOVD) and they imply that "their" MOVQ does
;;; NOT have encodings for "xmm <-> r/m64" (because that's REX.W+MOVD)
;;; Beneath it all, the binary encodings do the right thing. It's a question
;;; of naming. Assemblers do the right thing in any case.
;;;
;;; As can be seen here, all these operations are named the same by objdump:
;;;      f3 0f 7e 00             movq   (%rax),%xmm0  \ 64-bit-only move
;;;      66 0f d6 00             movq   %xmm0,(%rax)  /
;;;      66 48 0f 6e 00          movq   (%rax),%xmm0  \ REX.W + MOVD
;;;      66 48 0f 7e 00          movq   %xmm0,(%rax)  /
;;;
;;; The shorter encoding is the one that can't access GPRs and
;;; has no 32-bit form, nor requires REX.W bit to extend to 64 bits.
;;; AMD names the latter two lines MOVD. This is a bit of an oxymoron.
;;;
;;; Anyway, the upshot is severalfold:
;;; (1) Our MOVQ emitter needs to accept a 64-bit GPR<->XMM move emitting
;;;     a "66 REX 0f {6e|7e}" encoding. AMD (reluctantly?) concurs in the
;;;     latest doc (24594-Rev.3.26-May 2018) that this form of MOVD is
;;;     "Also known as MOVQ in some developer tools" (page 231)
;;; (2) It's absurd to disassemble a 64-bit move as MOVD in lieu of MOVQ.
;;;     I could find no tool that actually does that, nor should we.
;;;     This is simply saying that we can faithfully round-trip our own asm.
;;; (3) On top of that, we'll _require_ that MOVD only operate on 32 bits,
;;;     and MOVQ on 64 bits. This is stricter than other assemblers.
;;;     Furthermore MOVD does not accept a size prefix such as
;;;     (INST MOVD :QWORD X Y) ; <- What is this, I can't even.
;;;
;;; For further reading:
;;; https://www.gamedev.net/blogs/entry/2250281-demystifying-sse-move-instructions/
;;; which nowhere says "and by the way, MOVD can move 64 bits"
;;;
#-sb-xc-host
(defun print-mov[dq]-opcode  (dchunk inst stream dstate)
  (declare (ignore dchunk inst))
  (when stream
    (princ (if (dstate-getprop dstate +rex-w+) 'movq 'movd) stream)))

(flet ((move-xmm<->gpr (segment dst src size)
         ;; We no longer AVER that the size of the src/dst is exactly :DWORD.
         ;; So you can write (MOVD float0-tn rax-tn) and it will move 32 bits.
         (setq dst (sized-thing dst size)
               src (sized-thing src size))
         (cond ((xmm-register-p dst)
                (emit-sse-inst segment dst src #x66 #x6e
                               :operand-size (and (ea-p src)
                                                  :do-not-set)))
               (t
                (aver (xmm-register-p src))
                (emit-sse-inst segment src dst #x66 #x7e
                               :operand-size (and (ea-p dst)
                                                  :do-not-set))))))
  (define-instruction movd (segment dst src)
    (:emitter (move-xmm<->gpr segment dst src :dword))
    . #.(append (sse-inst-printer 'xmm-reg/mem #x66 #x6e
                 :printer '(#'print-mov[dq]-opcode :tab reg ", " reg/mem))
                (sse-inst-printer 'xmm-reg/mem #x66 #x7e
                 :printer '(#'print-mov[dq]-opcode :tab reg/mem ", " reg))))

  (define-instruction movq (segment dst src)
    (:emitter
     (cond ((or (gpr-p src) (gpr-p dst))
            (move-xmm<->gpr segment dst src :qword))
           (t
            (cond ((xmm-register-p dst)
                   (emit-sse-inst segment dst src #xf3 #x7e
                                  :operand-size :do-not-set))
                  (t
                   (aver (xmm-register-p src))
                   (emit-sse-inst segment src dst #x66 #xd6
                                  :operand-size :do-not-set))))))
    . #.(append (sse-inst-printer 'xmm-xmm/mem #xf3 #x7e)
                (sse-inst-printer 'xmm-xmm/mem #x66 #xd6
                                  :printer '(:name :tab reg/mem ", " reg)))))

(define-instruction rdfsbase (segment dst)
  (:printer ext-xmm-reg/mem ((prefix #xF3) (op #xAE)
                             (reg nil :prefilter nil :printer #("RDFSBASE" "RDGSBASE")))
            '(reg :tab reg/mem))
  (:emitter (emit-sse-inst segment 0 dst #xf3 #xae)))
(define-instruction rdgsbase (segment dst)
  (:emitter (emit-sse-inst segment 1 dst #xf3 #xae)))

;;; Instructions having an XMM register as the destination operand
;;; and a general-purpose register or a memory location as the source
;;; operand. The operand size is calculated from the source operand.

(macrolet ((define-extract-sse-instruction (name prefix op1 op2
                                            &key explicit-qword)
             `(define-instruction ,name (segment dst src imm)
                ;; The printer for PEXTRD changes the opcode name to PEXTRQ
                ;; depending on the REX.W bit.
                ,@(case name
                    (pextrq nil)
                    (pextrd
                     `((:printer ext-2byte-reg/mem-xmm
                                 ((prefix '(,prefix))
                                  (rex nil :prefilter (lambda (dstate)
                                                        (if (dstate-getprop dstate +rex-w+) 1 0))
                                           :printer #("PEXTRD" "PEXTRQ"))
                                  (op1 '(,op1))
                                  (op2 '(,op2))
                                  (imm nil :type 'imm-byte))
                                 '(rex :tab reg/mem ", " reg ", " imm))))

                    (t
                     `((:printer ext-2byte-reg/mem-xmm
                                 ((prefix '(,prefix))
                                  (op1 '(,op1))
                                  (op2 '(,op2))
                                  (imm nil :type 'imm-byte))
                                 '(:name :tab reg/mem ", " reg ", " imm)))))
                (:emitter
                 (aver (and (xmm-register-p src) (not (xmm-register-p dst))))
                 (emit-sse-inst-2byte segment src dst ,prefix ,op1 ,op2
                                      :operand-size ,(if explicit-qword
                                                         :qword
                                                         :do-not-set)
                                      :remaining-bytes 1)
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


  ;; pinsrq not encodable in 64-bit mode ;; FIXME: that's not true
  (define-insert-sse-instruction pinsrb #x66 #x3a #x20)
  (define-insert-sse-instruction pinsrw #x66 #xc4 nil)
  (define-insert-sse-instruction pinsrd #x66 #x3a #x22)

  (define-extract-sse-instruction pextrb #x66 #x3a #x14)
  (define-extract-sse-instruction pextrd #x66 #x3a #x16)
  (define-extract-sse-instruction pextrq #x66 #x3a #x16 :explicit-qword t)
  (define-extract-sse-instruction extractps #x66 #x3a #x17))

;; PEXTRW has a new 2-byte encoding in SSE4.1 to allow dst to be
;; a memory address.
(define-instruction pextrw (segment dst src imm)
  (:emitter
   (aver (xmm-register-p src))
   (if (gpr-p dst)
       (emit-sse-inst segment dst src #x66 #xc5
                            :operand-size :do-not-set :remaining-bytes 1)
       (emit-sse-inst-2byte segment dst src #x66 #x3a #x15
                            :operand-size :do-not-set :remaining-bytes 1))
   (emit-byte segment imm))
  . #.(append
       (2byte-sse-inst-printer '2byte-reg/mem-xmm #x66 #x3a #x15
                                    :more-fields '((imm nil :type 'imm-byte))
                                    :printer '(:name :tab reg/mem ", " reg ", " imm))
       (sse-inst-printer 'reg/mem-xmm #x66 #xc5
                              :more-fields '((imm nil :type 'imm-byte))
                              :printer '(:name :tab reg/mem ", " reg ", " imm))))

(macrolet ((define-integer-source-sse-inst (name prefix opcode &key mem-only)
             `(define-instruction ,name (segment dst src)
                ,@(sse-inst-printer 'xmm-reg/mem prefix opcode)
                (:emitter
                 (aver (xmm-register-p dst))
                 ,(when mem-only
                    `(aver (not (register-p src))))
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

(macrolet ((define-gpr-destination-sse-inst (name prefix opcode
                                                  &key (size '(operand-size dst))
                                                       reg-only)
             `(define-instruction ,name (segment dst src)
                ,@(sse-inst-printer 'reg-xmm/mem prefix opcode)
                (:emitter
                 (aver (gpr-p dst))
                 ,(when reg-only
                    `(aver (xmm-register-p src)))
                 (let ((dst-size ,size))
                   ,@(unless (eq size :do-not-set)
                      '((aver (or (eq dst-size :qword) (eq dst-size :dword)))))
                   (emit-sse-inst segment dst src ,prefix ,opcode
                                  :operand-size dst-size))))))
  (define-gpr-destination-sse-inst cvtsd2si  #xf2 #x2d)
  (define-gpr-destination-sse-inst cvtss2si  #xf3 #x2d)
  (define-gpr-destination-sse-inst cvttsd2si #xf2 #x2c)
  (define-gpr-destination-sse-inst cvttss2si #xf3 #x2c)
  ;; Per the Intel documentation on these instructions:
  ;;  "The default operand size is 64-bit in 64-bit mode."
  ;; We disassemble these incorrectly, but it doesn't matter since the
  ;; result is effectively zero-extended to 64 bits either way.
  (define-gpr-destination-sse-inst movmskpd  #x66 #x50 :reg-only t
    :size :do-not-set)
  (define-gpr-destination-sse-inst movmskps  nil  #x50 :reg-only t
    :size :do-not-set)
  (define-gpr-destination-sse-inst pmovmskb  #x66 #xd7 :reg-only t
    :size :do-not-set))

;;;; We call these "2byte" instructions due to their two opcode bytes.
;;;; Intel and AMD call them three-byte instructions, as they count the
;;;; 0x0f byte for determining the number of opcode bytes.

;;; Instructions that take XMM-XMM/MEM and XMM-XMM/MEM-IMM arguments.

(macrolet ((regular-2byte-sse-inst (name prefix op1 op2)
             `(define-instruction ,name (segment dst src)
                ,@(2byte-sse-inst-printer '2byte-xmm-xmm/mem prefix
                                                op1 op2)
                (:emitter
                 (emit-regular-2byte-sse-inst segment dst src ,prefix
                                              ,op1 ,op2))))
           (regular-2byte-sse-inst-imm (name prefix op1 op2)
             `(define-instruction ,name (segment dst src imm)
                ,@(2byte-sse-inst-printer
                    '2byte-xmm-xmm/mem prefix op1 op2
                    :more-fields '((imm nil :type 'imm-byte))
                    :printer `(:name :tab reg ", " reg/mem ", " imm))
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

  (regular-2byte-sse-inst-imm insertps #x66 #x3a #x21)

  (regular-2byte-sse-inst-imm aeskeygenassist #x66 #x3a #xdf))

;;; Other SSE instructions

;; Instructions implicitly using XMM0 as a mask
(macrolet ((define-sse-inst-implicit-mask (name prefix op1 op2)
             `(define-instruction ,name (segment dst src mask)
                ,@(2byte-sse-inst-printer
                    '2byte-xmm-xmm/mem prefix op1 op2
                    :printer '(:name :tab reg ", " reg/mem ", XMM0"))
                (:emitter
                 (aver (xmm-register-p dst))
                 (aver (and (xmm-register-p mask)
                            (= (reg-id-num (reg-id mask)) 0)))
                 (emit-regular-2byte-sse-inst segment dst src ,prefix
                                              ,op1 ,op2)))))

  (define-sse-inst-implicit-mask pblendvb #x66 #x38 #x10)
  (define-sse-inst-implicit-mask blendvps #x66 #x38 #x14)
  (define-sse-inst-implicit-mask blendvpd #x66 #x38 #x15))

(define-instruction movnti (segment dst src)
  (:printer ext-reg-reg/mem-no-width ((op #xc3)) '(:name :tab reg/mem ", " reg))
  (:emitter
   (aver (not (register-p dst)))
   (aver (gpr-p src))
   (emit-prefixes segment dst src (operand-size src))
   (emit-bytes segment #x0f #xc3)
   (emit-ea segment dst src)))

(flet ((emit* (segment opcode subcode src)
         (aver (not (register-p src)))
         (aver subcode)
         (emit-prefixes segment src nil :byte)
         (emit-byte segment #x0f)
         (emit-byte segment opcode)
         (emit-ea segment src subcode)))

  (define-instruction prefetch (segment type src)
    (:printer ext-reg/mem-no-width ((op '(#x18 0)))
              '("PREFETCHNTA" :tab reg/mem))
    (:printer ext-reg/mem-no-width ((op '(#x18 1)))
              '("PREFETCHT0" :tab reg/mem))
    (:printer ext-reg/mem-no-width ((op '(#x18 2)))
              '("PREFETCHT1" :tab reg/mem))
    (:printer ext-reg/mem-no-width ((op '(#x18 3)))
              '("PREFETCHT2" :tab reg/mem))
    (:emitter (emit* segment #x18 (position type #(:nta :t0 :t1 :t2)) src)))

  (define-instruction clflush (segment src)
    (:printer ext-reg/mem-no-width ((op '(#xae 7))))
    (:emitter (emit* segment #xae 7 src))))

(macrolet ((define-fence-instruction (name last-byte)
               `(define-instruction ,name (segment)
                  (:printer three-bytes ((op '(#x0f #xae ,last-byte))))
                  (:emitter (emit-bytes segment #x0f #xae ,last-byte)))))
  (define-fence-instruction lfence #xE8)
  (define-fence-instruction mfence #xF0)
  (define-fence-instruction sfence #xF8))

(define-instruction pause (segment)
  (:printer two-bytes ((op '(#xf3 #x90))))
  (:emitter (emit-bytes segment #xf3 #x90)))

(flet ((emit* (segment ea subcode)
         (aver (not (register-p ea)))
         (emit-prefixes segment ea nil :dword)
         (emit-bytes segment #x0f #xae)
         (emit-ea segment ea subcode)))

  (define-instruction ldmxcsr (segment src)
    (:printer ext-reg/mem-no-width ((op '(#xae 2))))
    (:emitter (emit* segment src 2)))

  (define-instruction stmxcsr (segment dst)
    (:printer ext-reg/mem-no-width ((op '(#xae 3))))
    (:emitter (emit* segment dst 3))))

(define-instruction popcnt (segment &prefix prefix dst src)
  (:printer ext-xmm-reg/mem ((prefix #xF3) (op #xB8) (reg nil :type 'reg)))
  (:emitter
   (let ((size (pick-operand-size prefix dst src)))
     (aver (neq size :byte))
     ;; FIXME: this disagrees with the prefix order that other assemblers emit
     ;; for "popcnt %ax,%r10w" which is 66 f3 44 0f b8 d0.
     ;; We disassemble it wrongly, but I think ours is a valid encoding.
     (emit-sse-inst segment dst src #xf3 #xb8 :operand-size size))))

(define-instruction tzcnt (segment &prefix prefix dst src)
  (:printer ext-xmm-reg/mem ((prefix #xF3) (op #xBC) (reg nil :type 'reg)))
  (:emitter
   (let ((size (pick-operand-size prefix dst src)))
     (aver (neq size :byte))
     ;; FIXME: same bug as POPCNT - we can't disassemble if size is :WORD
     (emit-sse-inst segment dst src #xf3 #xBC :operand-size size))))

(define-instruction crc32 (segment src-size dst src)
  ;; The low bit of the final opcode byte sets the source size.
  ;; REX.W bit sets the destination size. can't have #x66 prefix and REX.W = 1.
  (:printer ext-2byte-prefix-reg-reg/mem
            ((prefix #xf2) (op1 #x38)
             (op2 #b1111000 :field (byte 7 25)) ; #xF0 ignoring the low bit
             (src-width nil :field (byte 1 24) :prefilter #'prefilter-width)
             (reg nil :printer #'print-d/q-word-reg)))
  (:emitter
   (let ((dst-size (operand-size dst)))
     ;; The following operand size combinations are possible:
     ;;   dst = r32, src = r/m{8, 16, 32}
     ;;   dst = r64, src = r/m{8, 64}
     (aver (and (gpr-p dst)
                (memq src-size (case dst-size
                                 (:dword '(:byte :word :dword))
                                 (:qword '(:byte :qword))))))
     (emit-prefixes segment nil nil (if (eq src-size :word) :word :do-not-set))
     (emit-sse-inst-2byte segment dst src #xf2 #x38
                          (opcode+size-bit #xf0 src-size)
                          ;; :OPERAND-SIZE is ordinarily determined
                          ;; from 'src', so override it to use 'dst'.
                          :operand-size dst-size))))

;;;; Miscellany

(define-instruction cpuid (segment)
  (:printer two-bytes ((op '(#x0F #xA2))))
  (:emitter (emit-bytes segment #x0F #xA2)))

(define-instruction rdtsc (segment)
  (:printer two-bytes ((op '(#x0F #x31))))
  (:emitter (emit-bytes segment #x0f #x31)))

;;;; Intel TSX - some user library (STMX) used to define these,
;;;; but it's not really supported and they actually belong here.

(define-instruction-format
    (xbegin 48 :default-printer '(:name :tab label))
  (op :fields (list (byte 8 0) (byte 8 8)) :value '(#xc7 #xf8))
  (label :field (byte 32 16) :type 'displacement))

(define-instruction-format
    (xabort 24 :default-printer '(:name :tab imm))
  (op :fields (list (byte 8 0) (byte 8 8)) :value '(#xc6 #xf8))
  (imm :field (byte 8 16)))

(define-instruction xbegin (segment &optional where)
  (:printer xbegin ())
  (:emitter
   (emit-bytes segment #xc7 #xf8)
   (if where
       ;; emit 32-bit, signed relative offset for where
       (emit-dword-displacement-backpatch segment where)
       ;; nowhere to jump: simply jump to the next instruction
       (emit-dword segment 0))))

(define-instruction xend (segment)
  (:printer three-bytes ((op '(#x0f #x01 #xd5))))
  (:emitter (emit-bytes segment #x0f #x01 #xd5)))

(define-instruction xabort (segment reason)
  (:printer xabort ())
  (:emitter
   (aver (<= 0 reason #xff))
   (emit-bytes segment #xc6 #xf8 reason)))

(define-instruction xtest (segment)
  (:printer three-bytes ((op '(#x0f #x01 #xd6))))
  (:emitter (emit-bytes segment #x0f #x01 #xd6)))

(define-instruction xacquire (segment) ;; same prefix byte as repne/repnz
  (:emitter
   (emit-byte segment #xf2)))

(define-instruction xrelease (segment) ;; same prefix byte as rep/repe/repz
  (:emitter
   (emit-byte segment #xf3)))

;;;; Late VM definitions

(defun canonicalize-inline-constant (constant &aux (alignedp nil))
  ;; FIXME: It's slightly wonky to conflate alignment with size.
  ;; It works only because the architecture is little-endian, so when we dump a
  ;; single- or double-float as an :oword, the zero padding goes where it should.
  ;; A saner approach would have the alignment not necessarily taken directly from
  ;; the size, but merely defaulted to the size, with the option to overalign,
  ;; or potentially underalign for denser packing if desired.
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
       (setf constant (list :complex-double-float first)))
      #+(and sb-simd-pack (not sb-xc-host))
      (simd-pack
       (setq constant
             (list :sse (logior (%simd-pack-low first)
                                (ash (%simd-pack-high first) 64)))))
      #+(and sb-simd-pack-256 (not sb-xc-host))
      (simd-pack-256
       (setq constant
             (sb-vm::%simd-pack-256-inline-constant first)))))
  (destructuring-bind (type value) constant
    (ecase type
      ((:byte :word :dword :qword)
       (aver (integerp value))
       (cons type value))
      ((:oword :sse)
       (aver (integerp value))
       (cons :oword value))
      ((:hword :avx2)
       (aver (integerp value))
       (cons :hword value))
      ((:single-float)
       (aver (typep value 'single-float))
       (cons (if alignedp :oword :dword)
             (ldb (byte 32 0) (single-float-bits value))))
      ((:double-float)
       (aver (typep value 'double-float))
       (cons (if alignedp :oword :qword)
             (ldb (byte 64 0) (double-float-bits value))))
      ((:complex-single-float)
       (aver (typep value '(complex single-float)))
       (cons (if alignedp :oword :qword)
             (ldb (byte 64 0)
                  (logior (ash (single-float-bits (imagpart value)) 32)
                          (ldb (byte 32 0)
                               (single-float-bits (realpart value)))))))
      ((:complex-double-float)
       (aver (typep value '(complex double-float)))
       (cons :oword
             (logior (ash (ldb (byte 64 0) (double-float-bits (imagpart value))) 64)
                     (ldb (byte 64 0) (double-float-bits (realpart value))))))
      ((:jump-table)
       (cons :jump-table value)))))

(defun inline-constant-value (constant)
  (declare (ignore constant)) ; weird!
  (let ((label (gen-label)))
    (values label (rip-relative-ea label))))

(defun align-of (constant)
  (case (car constant)
    (:jump-table n-word-bytes)
    (t (size-nbyte (car constant)))))

(defun sort-inline-constants (constants)
  ;; Each constant is ((size . bits) . label)
  ;; Jump tables must precede everything else.
  (let ((jump-tables (remove :jump-table constants :test #'neq :key #'caar))
        (rest (remove :jump-table constants :key #'caar)))
    (concatenate 'vector
                 jump-tables
                 (stable-sort rest #'> :key (lambda (x) (align-of (car x)))))))

(defun emit-inline-constant (section constant label)
  ;; See comment at CANONICALIZE-INLINE-CONSTANT about how we are
  ;; careless with the distinction between alignment and size.
  ;; Such slop would not work for big-endian machines.
  (let ((size (align-of constant)))
    (emit section
          `(.align ,(integer-length (1- size)))
          label
          (if (eq (car constant) :jump-table)
              `(.lispword ,@(coerce (cdr constant) 'list))
              ;; Could add pseudo-ops for .WORD, .INT, .OCTA just like gcc has.
              ;; But it works fine to emit as a sequence of bytes
              `(.byte ,@(let ((val (cdr constant)))
                          (loop repeat size
                                collect (prog1 (ldb (byte 8 0) val)
                                          (setf val (ash val -8))))))))))

;;; Return an address which when _dereferenced_ will return ADDR
(defun sb-vm::asm-routine-indirect-address (addr)
  (let ((i (sb-fasl::asm-routine-index-from-addr addr)))
    (declare (ignorable i))
    #-immobile-space (sap-int (sap+ (code-instructions sb-fasl:*assembler-routines*)
                                    (ash i word-shift)))
    ;; When asm routines are in relocatable text space, the vector of indirections
    ;; is stored externally in static space. It's unfortunately overly complicated
    ;; to get the address of that vector in genesis. But it doesn't matter.
    #+immobile-space
    (or
     ;; Accounting for the jump-table-count as the first unboxed word in
     ;; code-instructions, subtract 1 from I to get the correct vector element.
     #-sb-xc-host (sap-int (sap+ (vector-sap sb-fasl::*asm-routine-vector*)
                                 (ash (1- i) word-shift)))
     (error "unreachable"))))

;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.
;;; The code object we're fixing up is pinned whenever this is called.
(symbol-macrolet
    (#-sb-xc-host (sb-vm::lisp-linkage-space-addr
                   (sb-alien:extern-alien "linkage_space" sb-alien:unsigned)))
(defun fixup-code-object (code offset value kind flavor)
  (declare (type index offset))
  (cond ((eq flavor :linkage-cell)
         (setq value (let ((index (ash value word-shift)))
                       (ecase kind
                         #+immobile-space (:rel32 (+ sb-vm::lisp-linkage-space-addr index))
                         (:abs32 index)))))
        ((and (eq flavor :assembly-routine) (eq kind :*abs32))
         (setq value (sb-vm::asm-routine-indirect-address value))))
  (let ((sap (code-instructions code)))
    (case flavor
      (:card-table-index-mask ; the VALUE is nbits, so convert it to an AND mask
       (setf (sap-ref-32 sap offset) (1- (ash 1 value))))
      (:layout-id ; layout IDs are signed quantities on x86-64
       (setf (signed-sap-ref-32 sap offset) value))
      (:alien-data-linkage-index
       (setf (sap-ref-32 sap offset) (* value alien-linkage-table-entry-size)))
      (:alien-code-linkage-index
       (let ((addend (sap-ref-32 sap offset)))
         (setf (sap-ref-32 sap offset) (+ (* value alien-linkage-table-entry-size) addend))))
      (t
       ;; All x86-64 fixup locations contain an implicit addend at the location
       ;; to be fixed up. The addend is always zero for certain <KIND,FLAVOR> pairs,
       ;; but we don't need to assert that.
       (incf value (if (eq kind :absolute)
                       (signed-sap-ref-64 sap offset)
                       (signed-sap-ref-32 sap offset)))
       (ecase kind
         ((:abs32 :*abs32) ; 32 unsigned bits
          (setf (sap-ref-32 sap offset) value))
         (:rel32
          ;; Replace word with the difference between VALUE and current pc.
          ;; JMP/CALL are relative to the next instruction,
          ;; so add 4 bytes for the size of the displacement itself.
          ;; Relative fixups don't exist with movable code,
          ;; so in the #-immobile-code case, there's nothing to assert.
          #+(and immobile-code (not sb-xc-host))
          (unless (immobile-space-obj-p code)
            (error "Can't compute fixup relative to movable object ~S" code))
          (setf (signed-sap-ref-32 sap offset) (- value (+ (sap-int sap) offset 4))))
         (:absolute
          ;; These are used for jump tables and are not recorded in %code-fixups.
          ;; GC knows to adjust the values if code is moved.
          (setf (sap-ref-64 sap offset) value))))))
  nil))

;;; There are 3 data streams in the FIXUPS slot:
;;; 1. linkage table indices
;;; 2. absolute fixups
;;; 3. card table mask fixups
;;; This fuction returns only the latter 2 streams. The first is prepended later.
(defun sb-c::pack-fixups-for-reapplication (fixup-notes &aux abs32-fixups imm-fixups)
  ;; An absolute fixup is stored in the code header's %FIXUPS slot if it
  ;; references an immobile-space (but not static-space) object.
  (dolist (note fixup-notes (sb-c:pack-code-fixup-locs abs32-fixups imm-fixups))
    (let* ((fixup (fixup-note-fixup note))
           (offset (fixup-note-position note))
           (flavor (fixup-flavor fixup)))
      (cond ((eq flavor :card-table-index-mask) (push offset imm-fixups))
            #+(or permgen immobile-space)
            ((and (eq (fixup-note-kind note) :abs32)
                  (memq flavor ; these all point to fixedobj space
                        '(:layout :immobile-symbol :symbol-value)))
             (push offset abs32-fixups))))))

;;; Coverage support

(define-instruction store-coverage-mark (segment mark-index)
  (:emitter
   (assemble (segment)
     (inst mov :byte (rip-relative-ea (segment-origin segment)
                                      ;; skip over jump table word and entries
                                      (+ (* (1+ (component-n-jump-table-entries))
                                            n-word-bytes)
                                         mark-index))
           1))))

(defun sb-assem::%mark-used-labels (operand)
  (when (typep operand 'ea)
    (let ((disp (ea-disp operand)))
      (typecase disp
       (label
        (setf (label-usedp disp) t))
       (label+addend
        (setf (label-usedp (label+addend-label disp)) t))))))

;;; Assembly optimizer support

(defun parse-2-operands (stmt)
  (let* ((operands (stmt-operands stmt))
         (first (pop operands))
         (second (pop operands)))
    (if (atom (gethash (stmt-mnemonic stmt) sb-assem::*inst-encoder*)) ; no prefixes
        (values :qword first second)
        (let ((prefix first)
              (first second)
              (second (pop operands)))
          (values (pick-operand-size prefix first second) first second)))))

(defun smaller-of (size1 size2)
  (if (or (eq size1 :dword) (eq size2 :dword)) :dword :qword))

;;; TODO: all of these start with PARSE-2-OPERANDS. It might be better
;;; if the driving loop could do that.
;;; It's supposed to be backend-agnostic, so maybe it will need to be
;;; two steps: prepare a statement for rule testing, and test a rule
;;; for applicability.

;;; A :QWORD load followed by a shift or a mask that clears the upper 32 bits
;;; can use a :DWORD load.
;;; (Recognizing the 3-instruction pattern of MOV + SAR + AND could also work)
;;; The second instruction could probably be any :dword-sized operation.
(defpattern "load :qword -> :dword" ((mov) (shr and)) (stmt next)
  (binding* (((size1 dst1 src1) (parse-2-operands stmt))
             ((size2 dst2 src2) (parse-2-operands next)))
    (declare (ignore src2))
    (when (and (gpr-tn-p dst1)
               (tn-p dst2)
               (location= dst2 dst1)
               (eq size1 :qword)
               (eq size2 :dword))
      (setf (stmt-operands stmt) `(,+dword-size-prefix+ ,dst1 ,(if (integerp src1)
                                                                   (ldb (byte 32 0) src1)
                                                                   src1)))
      next)))

;;; "AND r, imm1" + "AND r, imm2" -> "AND r, (imm1 & imm2)"
(defpattern "and + and -> and" ((and) (and)) (stmt next)
  (binding* (((size1 dst1 src1) (parse-2-operands stmt))
             ((size2 dst2 src2) (parse-2-operands next)))
    (when (and (gpr-tn-p dst1)
               (location= dst2 dst1)
               (member size1 '(:qword :dword))
               (typep src1 '(signed-byte 32))
               (member size2 '(:dword :qword))
               (typep src2 '(signed-byte 32)))
      (setf (stmt-operands next)
            `(,(encode-size-prefix (smaller-of size1 size2)) ,dst2 ,(logand src1 src2)))
      (add-stmt-labels next (stmt-labels stmt))
      (delete-stmt stmt)
      next)))

;;; In "{AND,OR,...} reg, src ; TEST reg, reg ; {JMP,SET} {:z,:nz,:s,:ns}"
;;; the TEST is unnecessary since ALU operations set the Z and S flags.
;;; Per the processor manual, TEST clears OF and CF, so presumably
;;; there is not a branch-if on either of those flags.
;;; It shouldn't be a problem that removal of TEST leaves more flags affected.
(defpattern "ALU + test" ((add adc sub sbb and or xor neg sar shl shr) (test)) (stmt next)
  (binding* (((size1 dst1 src1) (parse-2-operands stmt))
             ((size2 dst2 src2) (parse-2-operands next))
             (next-next (stmt-next next)))
    (declare (ignore src1))
    (when (and (not (stmt-labels next))
               (gpr-tn-p dst2)
               (location= dst1 dst2) ; they can have different SCs
               (eq dst2 src2)
               next-next
               ;; Zero shifts do not affect the flags
               (not (and (memq (stmt-mnemonic stmt) '(sar shl shr))
                         (memq (car (last (stmt-operands stmt)))
                               '(:cl 0))))
               (memq (stmt-mnemonic next-next) '(jmp set))
               ;; TODO: figure out when it would be correct to omit TEST for the carry flag
               (case (car (stmt-operands next-next))
                 ((:s :ns)
                  (eq size2 size1))
                 ((:ne :e :nz :z)
                  (or (eq size2 size1) (and (eq size1 :dword) (eq size2 :qword))))))
      (delete-stmt next)
      next-next)))

;;; "fixnumize" + "SHR reg, N" where N > n-tag-bits skips the fixnumize.
;;; (could generalize: masking out N bits with AND, following by shifting
;;; out N low bits can eliminate the AND)
(defpattern "fixnumize + shr -> shr" ((and) (sar shr)) (stmt next)
  (binding* (((size1 dst1 src1) (parse-2-operands stmt))
             ((size2 dst2 src2) (parse-2-operands next)))
    (when (and (gpr-tn-p dst1)
               (location= dst2 dst1)
               (eq size1 :qword)
               (eql src1 (lognot fixnum-tag-mask))
               (member size2 '(:dword :qword))
               (typep src2 `(integer ,n-fixnum-tag-bits 63)))
      (add-stmt-labels next (stmt-labels stmt))
      (delete-stmt stmt)
      next)))

;;; SAR + AND can change the shift operand from :QWORD to :DWORD
;;; if the AND operand size is :DWORD and the shift would not see
;;; any of the upper 32 bits.
;;;  e.g. shift of 1: consumes source bit indices 1 .. 63
;;;       shift of 2: consumes source bit indices 2 .. 63
;;;       etc
;;; if mask has N bits, then the highest (inclusive) bit index consumed
;;; from the unshifted source is shift_amount + N - 1.
;;; If that number is <= 31 then we can use a :dword shift.
(defpattern "sar + and -> shr :dword" ((sar) (and)) (stmt next)
  (binding* (((size1 dst1 src1) (parse-2-operands stmt))
             ((size2 dst2 src2) (parse-2-operands next)))
    (when (and (gpr-tn-p dst1)
               (location= dst2 dst1)
               (eq size1 :qword)
               (typep src1 '(integer 1 63))
               (eq size2 :dword)
               (typep src2 '(unsigned-byte 31)))
      (let ((max-dst1-bit-index (+ src1 (integer-length src2) -1)))
        ;; If source bit 31 is in the result, and the operand size gets reduced
        ;; to :DWORD and the shift is signed, it would wrongly replicate the
        ;; sign bit across the :DWORD register.
        ;; It makes me nervous to think about correctness in that case,
        ;; so I'm constraining this to 31 bits, not 32.
        (when (<= max-dst1-bit-index 30)
          (setf (stmt-mnemonic stmt) 'shr
                (stmt-operands stmt) `(,+dword-size-prefix+ ,dst1 ,src1))
          next)))))

(defun reg= (a b) ; Return T if A and B are the same register
  ;; NIL is allowed for base and/or index of an EA.
  (if (not a) (not b) (and b (location= a b))))

(defun ea= (a b) ; Return  T if A and B are the same EA
  (and (eql (ea-scale a) (ea-scale b))
       (reg= (ea-index a) (ea-index b))
       (reg= (ea-base a) (ea-base b))
       (eql (ea-disp a) (ea-disp b))))

(defun alias-p (a b) ; Return T if A and B are the same anything
  ;; There are other ways for A and B to alias: an EA for a stack TN as an
  ;; obvious example, but less obviously EAs whose registers alias.
  ;; At worst, we'll skip instcombine in such situations.
  (etypecase b
    (tn (and (tn-p a) (location= a b)))
    (ea (and (ea-p a) (ea= a b)))
    ((or fixup number reg) nil)))

(defpattern "mov dst,src + mov src,dst elim" ((mov) (mov)) (stmt next)
  (binding* (((size1 dst1 src1) (parse-2-operands stmt))
             ((size2 dst2 src2) (parse-2-operands next)))
    (when (and (eq size1 :qword)
               (eq size2 :qword)
               (alias-p dst2 src1)
               (alias-p dst1 src2))
      #+nil
      (let ((mode (cond ((and (gpr-tn-p src1) (gpr-tn-p src2)) "r->r,r->r")
                        ((gpr-tn-p src1) "r->m,m->r")
                        (t "m->r,r->m"))))
        (format t "~&MOV elim ~a~@{ ~s~}~%" mode src1 dst1 src2 dst2))
      (add-stmt-labels stmt (stmt-labels next))
      (delete-stmt next)
      stmt)))

;;; Return :TAKEN if taking the conditional branch COND1 implies that COND2's
;;; branch will be taken, or :NOT-TAKEN if COND2 will fallthrough,
;;; or NIL it can't be determined.
(defun branch-branch-implication (cond1 cond2)
  (macrolet ((conditions (symbol1 symbol2)
               `(and (eql ,(encoded-condition symbol1) cond1)
                     (eql ,(encoded-condition symbol2) cond2))))
    (cond ((or (eq cond1 cond2) (eq cond2 :always))
           ;;  conditional to same condition  -> jump to target of 2nd jump
           ;;  conditional to :ALWAYS         -> jump to target of 2nd jump
           ;;   (this includes "always" to "always" either way you look at it)
           :taken)
          ((and (fixnump cond1) (fixnump cond2) (eq (logxor cond1 1) cond2))
           ;; A conditional jump to the negation of that condition
           ;; goes to the instruction after the 2nd jump.
           :not-taken)
          ;; I manually examined a sampling of calls to this function
          ;; and did not notice other opportunities to return non-NIL
          ((conditions :a :eq) :not-taken) ; above to equal
          ((conditions :a :ne) :taken) ; above to not-equal
          (t nil))))

;;; Possible enhancement: it should be possible to eliminate more jumps-to-jumps
;;; by knowing something about implication of one condition upon another, e.g.
;;; either JC or JZ jumping to JBE would take the second jump, since JBE is (CF=1 or ZF=1).
(defun sb-assem::perform-jump-to-jump-elimination (starting-stmt label->stmt-map)
  (flet ((jmp-cond (stmt)
           (if (cdr (stmt-operands stmt))
               (encoded-condition (car (stmt-operands stmt)))
               :always))
         (jmp-target (stmt)
           ;; could be an effective address (only if not a conditional jump)
           (let ((maybe-label (car (last (stmt-operands stmt)))))
             (and (label-p maybe-label) maybe-label))))
    (do ((stmt starting-stmt (stmt-next stmt)))
        ((null stmt))
      (when (eq (stmt-mnemonic stmt) 'jmp)
        (let* ((to-label (jmp-target stmt))
               (to-stmt (gethash to-label label->stmt-map)))
          (case (and to-stmt
                     (eq (stmt-mnemonic to-stmt) 'jmp)
                     (jmp-target to-stmt)
                     (branch-branch-implication (jmp-cond stmt) (jmp-cond to-stmt)))
            (:taken
             (setf (car (last (stmt-operands stmt))) (jmp-target to-stmt)))
            (:not-taken
             (let* ((fallthrough (stmt-next to-stmt))
                    (label ; the statement might already be labeled
                     (or (first (ensure-list (stmt-labels fallthrough)))
                         (let ((label (gen-label))) ; maake a new label
                           (setf (gethash label label->stmt-map) fallthrough)
                           (add-stmt-labels fallthrough label)))))
               (setf (car (last (stmt-operands stmt))) label)))))))))
