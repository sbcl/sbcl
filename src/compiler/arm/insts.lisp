;;;; that part of the description of the ARM instruction set (for
;;;; ARMv5) which can live on the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-ARM-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from this package into SB-VM
  (import '(conditional-opcode negate-condition emit-word
            composite-immediate-instruction encodable-immediate
            lsl lsr asr ror cpsr @) "SB-VM")
  ;; Imports from SB-VM into this package
  (import '(sb-vm:nil-value sb-vm::registers sb-vm::null-tn sb-vm::null-offset
            sb-vm::pc-tn sb-vm::pc-offset sb-vm::code-offset)))



(defconstant-eqx +conditions+
  '((:eq . 0)
    (:ne . 1)
    (:cs . 2) (:hs . 2)
    (:cc . 3) (:lo . 3)
    (:mi . 4)
    (:pl . 5)
    (:vs . 6)
    (:vc . 7)
    (:hi . 8)
    (:ls . 9)
    (:ge . 10)
    (:lt . 11)
    (:gt . 12)
    (:le . 13)
    (:al . 14))
  #'equal)
(defconstant-eqx +condition-name-vec+
  #.(let ((vec (make-array 16 :initial-element nil)))
      (dolist (cond +conditions+ vec)
        (when (null (aref vec (cdr cond)))
          (setf (aref vec (cdr cond)) (car cond)))))
  #'equalp)

(defun conditional-opcode (condition)
  (cdr (assoc condition +conditions+ :test #'eq)))
(defun negate-condition (name)
  (let ((code (logxor 1 (conditional-opcode name))))
    (aref +condition-name-vec+ code)))

;;;; disassembler field definitions

(define-arg-type condition-code :printer #'print-condition)

(define-arg-type reg :printer #'print-reg)

(define-arg-type float-reg :printer #'print-float-reg)

(define-arg-type float-sys-reg :printer #'print-float-sys-reg)

(define-arg-type shift-type :printer #'print-shift-type)

(define-arg-type immediate-shift :printer #'print-immediate-shift)

(define-arg-type shifter-immediate :printer #'print-shifter-immediate)

(define-arg-type relative-label
  :sign-extend t
  :use-label #'use-label-relative-label)

(define-arg-type load/store-immediate :printer #'print-load/store-immediate)

(define-arg-type load/store-register :printer #'print-load/store-register)

(define-arg-type msr-field-mask :printer #'print-msr-field-mask)

;;;; disassembler instruction format definitions

(define-instruction-format (dp-shift-immediate 32
                            :default-printer
                            '(:name cond :tab rd ", " rn ", " rm shift))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-8 :field (byte 8 20))
  (rn :field (byte 4 16) :type 'reg)
  (rd :field (byte 4 12) :type 'reg)
  (shift :fields (list (byte 5 7) (byte 2 5)) :type 'immediate-shift)
  (register-shift-p :field (byte 1 4) :value 0)
  (rm :field (byte 4 0) :type 'reg))

(define-instruction-format
    (dp-shift-register 32
     :default-printer
     '(:name cond :tab rd ", " rn ", " rm ", " shift-type " " rs))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-8 :field (byte 8 20))
  (rn :field (byte 4 16) :type 'reg)
  (rd :field (byte 4 12) :type 'reg)
  (rs :field (byte 4 8) :type 'reg)
  (multiply-p :field (byte 1 7) :value 0)
  (shift-type :field (byte 2 5) :type 'shift-type)
  (register-shift-p :field (byte 1 4) :value 1)
  (rm :field (byte 4 0) :type 'reg))

(define-instruction-format (dp-immediate 32
                            :default-printer
                            '(:name cond :tab rd ", " rn ", #" immediate))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-8 :field (byte 8 20))
  (rn :field (byte 4 16) :type 'reg)
  (rd :field (byte 4 12) :type 'reg)
  (immediate :field (byte 12 0) :type 'shifter-immediate))

(define-instruction-format (branch 32 :default-printer '(:name cond :tab target))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-4 :field (byte 4 24))
  (target :field (byte 24 0) :type 'relative-label))

(define-instruction-format
    (load/store-immediate 32
     ;; FIXME: cond should come between LDR/STR and B.
     :default-printer '(:name cond :tab rd ", [" rn load/store-offset))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-3 :field (byte 3 25))
  (load/store-offset :fields (list (byte 1 24)
                                   (byte 1 23)
                                   (byte 1 21)
                                   (byte 12 0))
                     :type 'load/store-immediate)
  (opcode-b :field (byte 1 22))
  (opcode-l :field (byte 1 20))
  (rn :field (byte 4 16) :type 'reg)
  (rd :field (byte 4 12) :type 'reg))

(define-instruction-format
    (load/store-register 32
     ;; FIXME: cond should come between LDR/STR and B.
     :default-printer '(:name cond :tab rd ", [" rn load/store-offset))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-3 :field (byte 3 25))
  (load/store-offset :fields (list (byte 1 24)
                                   (byte 1 23)
                                   (byte 1 21)
                                   (byte 5 7)  ;; shift_imm
                                   (byte 2 5)  ;; shift
                                   (byte 4 0)) ;; Rm
                     :type 'load/store-register)
  (opcode-b :field (byte 1 22))
  (opcode-l :field (byte 1 20))
  (opcode-0 :field (byte 1 4))
  (rn :field (byte 4 16) :type 'reg)
  (rd :field (byte 4 12) :type 'reg))

;;; Not sure if we can coerce our disassembler to print anything resembling this:
;;; <LDM|STM>{cond}<FD|ED|FA|EA|IA|IB|DA|DB> Rn{!},<Rlist>{^} where:
;;;   {cond}  two-character condition mnemonic. See Table 4-2: Condition code
;;;           summary on page 4-5.
;;;   Rn      is an expression evaluating to a valid register number
;;;   <Rlist> is a list of registers and register ranges enclosed in {} (For example,
;;;           {R0,R2-R7,R10}).
;;;   {!}     if present requests write-back (W=1), otherwise W=0
;;;   {^}     if present set S bit to load the CPSR along with the PC, or force transfer
;;;           of user bank when in privileged mode
;;; not to mention the alternative mnemonics PUSH and POP
;;; when using the native stack pointer as base register.
(define-instruction-format
    ;; This is just to show something in the disassembly other than BYTE ...
    (ldm/stm 32 :default-printer '(:name cond :tab bits ", " rn ", " reglist))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-3 :field (byte 3 25))
  (bits :field (byte 4 21)) ; complicated
  (opcode-l :field (byte 1 20))
  (rn :field (byte 4 16) :type 'reg)
  (reglist :field (byte 16 0)
           :printer (lambda (value stream dstate)
                      (declare (ignore dstate))
                      (format stream "{~{R~d~^,~}}"
                              (loop for i below 16
                                    when (logbitp i value) collect i)))))

(define-instruction-format (swi 32
                            :default-printer '(:name cond :tab "#" swi-number))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-4 :field (byte 4 24))
  (swi-number :field (byte 24 0)))

(define-instruction-format (debug-trap 32 :default-printer '(:name :tab code))
  (opcode-32 :field (byte 32 0))
  ;; We use a prefilter in order to read trap codes in order to avoid
  ;; encoding the code within the instruction body (requiring the use of
  ;; a different trap instruction and a SIGILL handler) and in order to
  ;; avoid attempting to include the code in the decoded instruction
  ;; proper (requiring moving to a 40-bit instruction for disassembling
  ;; trap codes, and being affected by endianness issues).
  (code :prefilter (lambda (dstate) (read-suffix 8 dstate))
        :reader debug-trap-code))

(define-instruction-format (msr-immediate 32
                            :default-printer
                            '(:name cond :tab field-mask ", #" immediate))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-5 :field (byte 5 23) :value #b00110)
  (field-mask :fields (list (byte 1 22) (byte 4 16)) :type 'msr-field-mask)
  (opcode-2 :field (byte 2 20) :value #b10)
  (sbo :field (byte 4 12) :value #b1111)
  (immediate :field (byte 12 0) :type 'shifter-immediate))

(define-instruction-format (msr-register 32
                            :default-printer '(:name cond :tab field-mask ", " rm))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-5 :field (byte 5 23) :value #b00010)
  (field-mask :fields (list (byte 1 22) (byte 4 16)) :type 'msr-field-mask)
  (opcode-2 :field (byte 2 20) :value #b10)
  (sbo :field (byte 4 12) :value #b1111)
  (sbz :field (byte 8 4) :value #b00000000)
  (rm :field (byte 4 0) :type 'reg))

(define-instruction-format (multiply-dzsm 32
                            :default-printer '(:name cond :tab rd ", " rs ", " rm))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-8 :field (byte 8 20))
  (rd :field (byte 4 16) :type 'reg)
  (sbz :field (byte 4 12) :value 0)
  (rs :field (byte 4 8) :type 'reg)
  (opcode-4 :field (byte 4 4))
  (rm :field (byte 4 0) :type 'reg))

(define-instruction-format
    (multiply-dnsm 32
     :default-printer '(:name cond :tab rd ", " rs ", " rm ", " num))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-8 :field (byte 8 20))
  (rd :field (byte 4 16) :type 'reg)
  (num :field (byte 4 12) :type 'reg)
  (rs :field (byte 4 8) :type 'reg)
  (opcode-4 :field (byte 4 4))
  (rm :field (byte 4 0) :type 'reg))

(define-instruction-format
    (multiply-ddsm 32
     :default-printer '(:name cond :tab rdlo ", " rdhi ", " rs ", " rm))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-8 :field (byte 8 20))
  (rdhi :field (byte 4 16) :type 'reg)
  (rdlo :field (byte 4 12) :type 'reg)
  (rs :field (byte 4 8) :type 'reg)
  (opcode-4 :field (byte 4 4))
  (rm :field (byte 4 0) :type 'reg))

(define-instruction-format (branch-exchange 32
                            :default-printer '(:name cond :tab rm))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-8 :field (byte 8 20))
  (sbo :field (byte 12 8) :value #xFFF)
  (opcode-4 :field (byte 4 4))
  (rm :field (byte 4 0) :type 'reg))

(define-instruction-format (fp-binary 32
                            :default-printer '(:name cond :tab fd ", " fn ", " fm))
  (cond :field (byte 4 28) :type 'condition-code)
  (opc-1 :field (byte 4 24) :value #b1110)
  (p :field (byte 1 23))
  (q :field (byte 1 21))
  (r :field (byte 1 20))
  (s :field (byte 1 6))
  (fn :fields (list (byte 1 8) (byte 4 16) (byte 1 7)) :type 'float-reg)
  (fd :fields (list (byte 1 8) (byte 4 12) (byte 1 22)) :type 'float-reg)
  (fm :fields (list (byte 1 8) (byte 4 0) (byte 1 5)) :type 'float-reg)
  (opc-2 :field (byte 3 9) :value #b101)
  (size :field (byte 1 8))
  (opc-3 :field (byte 1 4) :value 0))

(define-instruction-format (fp-unary 32
                            :default-printer '(:name cond :tab fd  ", " fm))
  (cond :field (byte 4 28) :type 'condition-code)
  (opc-1 :field (byte 5 23) :value #b11101)
  (opc-2 :field (byte 2 20) :value #b11)
  (opc :field (byte 4 16))
  (fd :fields (list (byte 1 8) (byte 4 12) (byte 1 22)) :type 'float-reg)
  (fm :fields (list (byte 1 8) (byte 4 0) (byte 1 5)) :type 'float-reg)
  (opc-3 :field (byte 3 9) :value #b101)
  (size :field (byte 1 8))
  (n :field (byte 1 7))
  (s :field (byte 1 6) :value 1)
  (opc-4 :field (byte 1 4) :value 0))

(define-instruction-format (fp-unary-one-op 32
                            :default-printer '(:name cond :tab fd))
  (cond :field (byte 4 28) :type 'condition-code)
  (opc-1 :field (byte 5 23) :value #b11101)
  (opc-2 :field (byte 2 20) :value #b11)
  (opc :field (byte 4 16))
  (fd :fields (list (byte 1 8) (byte 4 12) (byte 1 22)) :type 'float-reg)
  (fm :fields (list (byte 1 8) (byte 4 0) (byte 1 5)) :type 'float-reg)
  (opc-3 :field (byte 3 9) :value #b101)
  (size :field (byte 1 8))
  (n :field (byte 1 7))
  (s :field (byte 1 6) :value 1)
  (sbz :field (byte 6 0) :value 0))

(define-instruction-format (fp-srt 32)
  (cond :field (byte 4 28) :type 'condition-code)
  (opc-1 :field (byte 4 24) :value #b1110)
  (opc :field (byte 3 21))
  (l :field (byte 1 20))
  (fn :fields (list (byte 1 8) (byte 1 7) (byte 4 16)) :type 'float-reg)
  (rd :field (byte 4 12)  :type 'reg)
  (opc-3 :field (byte 3 9) :value #b101)
  (size :field (byte 1 8))
  (opc-4 :field (byte 7 0) :value #b0010000))

(define-instruction-format (fp-srt-sys 32)
  (cond :field (byte 4 28) :type 'condition-code)
  (opc-1 :field (byte 4 24) :value #b1110)
  (opc :field (byte 3 21))
  (l :field (byte 1 20))
  (fn :field (byte 4 16) :type 'float-sys-reg)
  (rd :field (byte 4 12)  :type 'reg)
  (opc-3 :field (byte 3 9) :value #b101)
  (opc-4 :field (byte 8 0) :value #b00010000))

(define-instruction-format (fp-trt 32)
  (cond :field (byte 4 28) :type 'condition-code)
  (opc-1 :field (byte 7 21) :value #b1100010)
  (l :field (byte 1 20))
  (rn :field (byte 4 16)  :type 'reg)
  (rd :field (byte 4 12)  :type 'reg)
  (opc-2 :field (byte 3 9) :value #b101)
  (size :field (byte 1 8))
  (opc-3 :field (byte 2 6) :value 0)
  (fm :fields (list (byte 1 8) (byte 4 0) (byte 1 5)) :type 'float-reg)
  (opc-4 :field (byte 1 4) :value 1))

(define-instruction-format (conditional 32 :default-printer '(:name cond))
  (cond :field (byte 4 28) :type 'condition-code)
  (op :field (byte 28 0)))

;;;; primitive emitters

;(define-bitfield-emitter emit-word 16
;  (byte 16 0))

(define-bitfield-emitter emit-word 32
  (byte 32 0))

;;;; miscellaneous hackery

(defun register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)))

(defmacro with-condition-defaulted ((argvar arglist) &body body)
  (let ((internal-emitter (gensym)))
    `(flet ((,internal-emitter ,arglist
              ,@body))
       (if (assoc (car ,argvar) +conditions+)
           (apply #',internal-emitter ,argvar)
           (apply #',internal-emitter :al ,argvar)))))

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

;(define-instruction word (segment word)
;  (:emitter
;   (emit-word segment word)))

(define-instruction word (segment word)
  (:emitter
   (etypecase word
     (fixup
      (note-fixup segment :absolute word)
      (emit-word segment 0))
     (integer
      (emit-word segment word)))))

(defun emit-header-data (segment type)
  (emit-back-patch segment
                   4
                   (lambda (segment posn)
                     (emit-word segment
                                (logior type
                                        (ash (+ posn
                                                (component-header-length))
                                             (- n-widetag-bits
                                                word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  (:emitter
   (emit-header-data segment simple-fun-widetag)))

(define-instruction lra-header-word (segment)
  (:emitter
   (emit-header-data segment return-pc-widetag)))

;;;; Addressing mode 1 support

;;; Addressing mode 1 has some 11 formats.  These are immediate,
;;; register, and nine shift/rotate functions based on one or more
;;; registers.  As the mnemonics used for these functions are not
;;; currently used, we simply define them as constructors for a
;;; shifter-operand structure, similar to the make-ea function in the
;;; x86 backend.

(defstruct shifter-operand
  register
  function-code
  operand)

(defun lsl (register operand)
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 0 31))))

  (make-shifter-operand :register register :function-code 0 :operand operand))

(defun lsr (register operand)
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 1 32))))

  (make-shifter-operand :register register :function-code 1 :operand operand))

(defun asr (register operand)
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 1 32))))

  (make-shifter-operand :register register :function-code 2 :operand operand))

(defun ror (register operand)
  ;; ROR is a special case: the encoding for ROR with an immediate
  ;; shift of 32 (0) is actually RRX.
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 1 31))))

  (make-shifter-operand :register register :function-code 3 :operand operand))

(defun rrx (register)
  ;; RRX is a special case: it is encoded as ROR with an immediate
  ;; shift of 32 (0), and has no operand.
  (aver (register-p register))
  (make-shifter-operand :register register :function-code 3 :operand 0))

(define-condition cannot-encode-immediate-operand (error)
  ((value :initarg :value)))

(defun encodable-immediate (operand)
  ;; 32-bit immediate data is encoded as an 8-bit immediate data value
  ;; and a 4-bit immediate shift count.  The actual value is the
  ;; immediate data rotated right by a number of bits equal to twice
  ;; the shift count.  Note that this means that there are a limited
  ;; number of valid immediate integers and that some integers have
  ;; multiple possible encodings.  In the case of multiple encodings,
  ;; the correct one to use is the one with the lowest shift count.
  ;;
  ;; XXX: Is it possible to determine the correct encoding in constant
  ;; time, rather than time proportional to the final shift count?  Is
  ;; it possible to determine if a given integer is valid without
  ;; attempting to encode it?  Are such solutions cheaper (either time
  ;; or spacewise) than simply attempting to encode it?
  (labels ((try-immediate-encoding (value shift)
             (unless (<= 0 shift 15)
               (return-from encodable-immediate))
             (if (typep value '(unsigned-byte 8))
                 (dpb shift (byte 4 8) value)
                 (try-immediate-encoding (dpb value (byte 30 2)
                                              (ldb (byte 2 30) value))
                                         (1+ shift)))))
    (try-immediate-encoding operand 0)))

(defun encode-shifter-immediate (operand)
  (or
   (encodable-immediate operand)
   (error 'cannot-encode-immediate-operand :value operand)))

(defun encode-shifter-operand (operand)
  (etypecase operand
    (integer
     (dpb 1 (byte 1 25) (encode-shifter-immediate operand)))

    (tn
     (cond
       ((eq 'registers (sb-name (sc-sb (tn-sc operand))))
        ;; For those wondering, this is LSL immediate for 0 bits.
        (tn-offset operand))

       ((eq 'null (sc-name (tn-sc operand)))
        null-offset)

       (t (error "Don't know how to encode TN ~A as a SHIFTER-OPERAND" operand))))

    (shifter-operand
     (let ((Rm (tn-offset (shifter-operand-register operand)))
           (shift-code (shifter-operand-function-code operand))
           (shift-amount (shifter-operand-operand operand)))
       (etypecase shift-amount
         (integer
          (dpb shift-amount (byte 5 7)
               (dpb shift-code (byte 2 5)
                    Rm)))
         (tn
          (dpb (tn-offset shift-amount) (byte 4 8)
               (dpb shift-code (byte 2 5)
                    (dpb 1 (byte 1 4)
                         Rm)))))))))

(defun lowest-set-bit-index (integer-value)
  (max 0 (1- (integer-length (logand integer-value (- integer-value))))))

;; FIXME: it would be idiomatic to use (DEFINE-INSTRUCTION-MACRO COMPOSITE ...)
;; instead of exporting another instruction-generating macro into SB-VM.
;; An invocation would resemble (INST COMPOSITE {ADD|SUB|whatever| ARGS ...)
(defmacro composite-immediate-instruction (op r x y &key fixnumize neg-op invert-y invert-r single-op-op first-op first-no-source temporary)
  ;; Successively applies 8-bit wide chunks of Y to X using OP storing the result in R.
  ;;
  ;; If FIXNUMIZE is true, Y is fixnumized before being used.
  ;; If NEG-OP is given and Y is negative, NEG-OP is used instead of OP.
  ;; If INVERT-Y is given LOGNOT is applied to Y before it being used (but after possibly
  ;; being fixnumized.
  ;; If INVERT-R is given R is bit wise inverted at the end.
  ;; If SINGLE-OP-OP is given and (possibly fixnumized) Y fits into a single ARM immediate
  ;; it is used for a single operation instead of OP.
  ;; If FIRST-OP is given, it is used in the first iteration instead of OP.
  ;; If FIRST-NO-SOURCE is given, there will be ne source register (X) in the first iteration.
  ;; If TEMPORARY is given, it should be a non-descriptor register
  ;; used for the accumulation of a temporary non-descriptor.  Only makes sense with INVERT-R
  (when temporary
    (aver invert-r))
  (let ((bytespec (gensym "bytespec"))
        (value (gensym "value"))
        (transformed (gensym "transformed"))
        (acc (gensym "acc")))
    (labels ((instruction (source-reg op neg-op &optional no-source)
               `(,@(if neg-op
                        `((if (< ,y 0)
                              (inst ,neg-op ,acc ,@(when (not no-source)`(,source-reg))
                                    (mask-field ,bytespec ,value))
                              (inst ,op ,acc ,@(when (not no-source) `(,source-reg))
                                    (mask-field ,bytespec ,value))))
                        `((inst ,op ,acc ,@(when (not no-source) `(,source-reg))
                                (mask-field ,bytespec ,value))))
                  (setf (ldb ,bytespec ,value) 0)))
             (composite ()
               `((let ((,bytespec (byte 8 (logandc1 1 (lowest-set-bit-index ,value)))))
                    ,@(instruction x (or first-op op) neg-op first-no-source))
                  (do ((,bytespec (byte 8 (logandc1 1 (lowest-set-bit-index ,value)))
                                  (byte 8 (logandc1 1 (lowest-set-bit-index ,value)))))
                      ((zerop ,value))
                    ,@(instruction acc op neg-op))
                  ,@(when invert-r
                      `((inst mvn ,r ,acc))))))
      `(let* ((,transformed ,(if fixnumize
                                 `(fixnumize ,y)
                                 `,y))
              (,value (ldb (byte 32 0)
                           ,@(if neg-op
                                 `((if (< ,transformed 0) (- ,transformed) ,transformed))
                                 (if invert-y
                                     `((lognot ,transformed))
                                   `(,transformed)))))
              (,acc (or ,temporary ,r)))
         ,@(if single-op-op
              `((if (encodable-immediate ,transformed)
                    (inst ,single-op-op ,r ,x ,transformed)
                    (progn ,@(composite))))
              (composite))))))


;;;; Addressing mode 2 support

;;; Addressing mode 2 ostensibly has 9 formats.  These are formed from
;;; a cross product of three address calculations and three base
;;; register writeback modes.  As one of the address calculations is a
;;; scaled register calculation identical to the mode 1 register shift
;;; by constant, we reuse the shifter-operand structure and its public
;;; constructors.

(defstruct memory-operand
  base
  offset
  direction
  mode)

;;; The @ macro is used to encode a memory addressing mode.  The
;;; parameters for the base form are a base register, an optional
;;; offset (either an integer, a register tn or a shifter-operand
;;; structure with a constant shift amount, optionally within a unary
;;; - form), and a base register writeback mode (either :offset,
;;; :pre-index, or :post-index).  The alternative form uses a label as
;;; the base register, and accepts only (optionally negated) integers
;;; as offsets, and requires a mode of :offset.
(defun %@ (base offset direction mode)
  (when (label-p base)
    (aver (eq mode :offset))
    (aver (integerp offset)))

  (when (shifter-operand-p offset)
    (aver (integerp (shifter-operand-operand offset))))

  ;; Fix up direction with negative offsets.
  (when (and (not (label-p base))
             (integerp offset)
             (< offset 0))
    (setf offset (- offset))
    (setf direction (if (eq direction :up) :down :up)))

  (make-memory-operand :base base :offset offset
                       :direction direction :mode mode))

(defmacro @ (base &optional (offset 0) (mode :offset))
  (let* ((direction (if (and (consp offset)
                             (eq (car offset) '-)
                             (null (cddr offset)))
                        :down
                        :up))
         (offset (if (eq direction :down) (cadr offset) offset)))
    `(%@ ,base ,offset ,direction ,mode)))

;;;; Data-processing instructions

;;; Data processing instructions have a 4-bit opcode field and a 1-bit
;;; "S" field for updating condition bits.  They are adjacent, so we
;;; roll them into one 5-bit field for convenience.

(define-bitfield-emitter emit-dp-instruction 32
  (byte 4 28) (byte 2 26) (byte 1 25) (byte 5 20)
  (byte 4 16) (byte 4 12) (byte 12 0))

;;; There are 16 data processing instructions, with a breakdown as
;;; follows:
;;;
;;;   1.) Two "move" instructions, with no "source" operand (they have
;;;       destination and shifter operands only).
;;;
;;;   2.) Four "test" instructions, with no "destination" operand.
;;;       These instructions always have their "S" bit set, though it
;;;       is not specified in their mnemonics.
;;;
;;;   3.) Ten "normal" instructions, with all three operands.
;;;
;;; Aside from this, the instructions all have a regular encoding, so
;;; we can use a single macro to define them.

(defmacro define-data-processing-instruction (instruction opcode dest-p src-p)
  `(define-instruction ,instruction (segment &rest args)
     (:printer dp-shift-immediate ((opcode-8 ,opcode)
                                   ,@(unless dest-p '((rd 0)))
                                   ,@(unless src-p '((rn 0))))
               ,@(cond
                  ((not dest-p)
                   '('(:name cond :tab rn ", " rm shift)))
                  ((not src-p)
                   '('(:name cond :tab rd ", " rm shift)))))
     (:printer dp-shift-register ((opcode-8 ,opcode)
                                  ,@(unless dest-p '((rd 0)))
                                  ,@(unless src-p '((rn 0))))
               ,@(cond
                  ((not dest-p)
                   '('(:name cond :tab rn ", " rm ", " shift-type " " rs)))
                  ((not src-p)
                   '('(:name cond :tab rd ", " rm ", " shift-type " " rs)))))
     (:printer dp-immediate ((opcode-8 ,(logior opcode #x20))
                             ,@(unless dest-p '((rd 0)))
                             ,@(unless src-p '((rn 0))))
               ,@(cond
                  ((not dest-p)
                   '('(:name cond :tab rn ", " immediate)))
                  ((not src-p)
                   '('(:name cond :tab rd ", " immediate)))))
     (:emitter
      (with-condition-defaulted (args (condition ,@(if dest-p '(dest))
                                                 ,@(if src-p '(src))
                                                 shifter-operand))
        ,(if dest-p '(aver (register-p dest)))
        ,(if src-p '(aver (register-p src)))
        (let ((shifter-operand (encode-shifter-operand shifter-operand)))
          (emit-dp-instruction segment
                               (conditional-opcode condition)
                               0
                               (ldb (byte 1 25) shifter-operand)
                               ,opcode
                               ,(if src-p '(tn-offset src) 0)
                               ,(if dest-p '(tn-offset dest) 0)
                               (ldb (byte 12 0) shifter-operand)))))))

(define-data-processing-instruction and  #x00 t t)
(define-data-processing-instruction ands #x01 t t)
(define-data-processing-instruction eor  #x02 t t)
(define-data-processing-instruction eors #x03 t t)
(define-data-processing-instruction sub  #x04 t t)
(define-data-processing-instruction subs #x05 t t)
(define-data-processing-instruction rsb  #x06 t t)
(define-data-processing-instruction rsbs #x07 t t)
(define-data-processing-instruction add  #x08 t t)
(define-data-processing-instruction adds #x09 t t)
(define-data-processing-instruction adc  #x0a t t)
(define-data-processing-instruction adcs #x0b t t)
(define-data-processing-instruction sbc  #x0c t t)
(define-data-processing-instruction sbcs #x0d t t)
(define-data-processing-instruction rsc  #x0e t t)
(define-data-processing-instruction rscs #x0f t t)
(define-data-processing-instruction orr  #x18 t t)
(define-data-processing-instruction orrs #x19 t t)
(define-data-processing-instruction bic  #x1c t t)
(define-data-processing-instruction bics #x1d t t)

(define-data-processing-instruction tst  #x11 nil t)
(define-data-processing-instruction teq  #x13 nil t)
(define-data-processing-instruction cmp  #x15 nil t)
(define-data-processing-instruction cmn  #x17 nil t)

(define-data-processing-instruction mov  #x1a t nil)
(define-data-processing-instruction movs #x1b t nil)
(define-data-processing-instruction mvn  #x1e t nil)
(define-data-processing-instruction mvns #x1f t nil)

(define-instruction-format (movw-format 32
                            :default-printer '(:name :tab rd ", #" immediate))
  (cond :field (byte 4 28) :type 'condition-code)
  (opcode-8 :field (byte 8 20))
  (immediate :fields (list (byte 4 16) (byte 12 0))
             :prefilter (lambda (dstate high low)
                          (declare (ignore dstate))
                          (logior (ash high 12) low)))
  (rd :field (byte 4 12) :type 'reg))

(macrolet ((mov-imm-16 (segment rd imm half)
             `(emit-dp-instruction ,segment 14 #b00 #b1
                                   ,(ecase half
                                      (:low  #b10000)
                                      (:high #b10100))
                                   (ldb (byte 4 12) ,imm)
                                   (tn-offset ,rd)
                                   (ldb (byte 12 0) ,imm))))
(define-instruction movw (segment rd imm) ; move wide (zero-extend)
  (:printer movw-format ((opcode-8 #b00110000)))
  (:emitter (mov-imm-16 segment rd imm :low)))
(define-instruction movt (segment rd imm) ; move top bits (and keep bottom)
  (:printer movw-format ((opcode-8 #b00110100)))
  (:emitter (mov-imm-16 segment rd imm :high))))

;;;; Exception-generating instructions

;;; There are two exception-generating instructions.  One, BKPT, is
;;; ostensibly used as a breakpoint instruction, and to communicate
;;; with debugging hardware.  The other, SWI, is intended for use as a
;;; system call interface.  We need both because, at least on some
;;; platforms, the only breakpoint trap that works properly is a
;;; syscall.

(define-bitfield-emitter emit-swi-instruction 32
  (byte 4 28) (byte 4 24) (byte 24 0))

(define-instruction swi (segment &rest args)
  (:printer swi ((opcode-4 #b1111)))
  (:emitter
   (with-condition-defaulted (args (condition code))
     (emit-swi-instruction segment
                           (conditional-opcode condition)
                           #b1111 code))))

(define-bitfield-emitter emit-bkpt-instruction 32
  (byte 4 28) (byte 8 20) (byte 12 8) (byte 4 4) (byte 4 0))

(define-instruction bkpt (segment code)
  (:emitter
   (emit-bkpt-instruction segment #b1110 #b00010010
                          (ldb (byte 12 4) code)
                          #b0111
                          (ldb (byte 4 0) code))))

;;; It turns out that the Linux kernel decodes this particular
;;; officially undefined instruction as a single-instruction SIGTRAP
;;; generation instruction, or breakpoint.
(define-instruction debug-trap (segment)
  (:printer debug-trap ((opcode-32 #+linux #xe7f001f0
                                   #+(or netbsd openbsd) #xe7ffdefe))
            :default :control #'debug-trap-control)
  (:emitter
   (emit-word segment #+linux #xe7f001f0 #+(or netbsd openbsd) #xe7ffdefe)))

;;;; Miscellaneous arithmetic instructions

(define-bitfield-emitter emit-clz-instruction 32
  (byte 4 28) (byte 12 16) (byte 4 12) (byte 8 4) (byte 4 0))

(define-instruction clz (segment &rest args)
  (:printer dp-shift-register ((opcode-8 #b00010110)
                               (rn #b1111)
                               (rs #b1111)
                               (shift-type #b00))
            '(:name cond :tab rd ", " rm))
  (:emitter
   (with-condition-defaulted (args (condition dest src))
     (aver (register-p dest))
     (aver (register-p src))
     (emit-clz-instruction segment (conditional-opcode condition)
                           #b000101101111
                           (tn-offset dest)
                           #b11110001
                           (tn-offset src)))))

;;;; Branch instructions

(define-bitfield-emitter emit-branch-instruction 32
  (byte 4 28) (byte 4 24) (byte 24 0))

(defun emit-branch-back-patch (segment condition opcode dest)
  (emit-back-patch segment 4
                   (lambda (segment posn)
                     (emit-branch-instruction segment
                                              (conditional-opcode condition)
                                              opcode
                                              (ldb (byte 24 2)
                                                   (- (label-position dest)
                                                      (+ posn 8)))))))

(define-instruction b (segment &rest args)
  (:printer branch ((opcode-4 #b1010)))
  (:emitter
   (with-condition-defaulted (args (condition dest))
     (aver (label-p dest))
     (emit-branch-back-patch segment condition #b1010 dest))))

(define-instruction bl (segment &rest args)
  (:printer branch ((opcode-4 #b1011)))
  (:emitter
   (with-condition-defaulted (args (condition dest))
     (aver (label-p dest))
     (emit-branch-back-patch segment condition #b1011 dest))))

(define-bitfield-emitter emit-branch-exchange-instruction 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 4 4) (byte 4 0))

(define-instruction bx (segment &rest args)
  (:printer branch-exchange ((opcode-8 #b00010010)
                             (opcode-4 #b0001)))
  (:emitter
   (with-condition-defaulted (args (condition dest))
     (aver (register-p dest))
     (emit-branch-exchange-instruction segment
                                       (conditional-opcode condition)
                                       #b00010010 #b1111 #b1111
                                       #b1111 #b0001 (tn-offset dest)))))

(define-instruction blx (segment &rest args)
  (:printer branch-exchange ((opcode-8 #b00010010)
                             (opcode-4 #b0011)))
  (:emitter
   (with-condition-defaulted (args (condition dest))
     (aver (register-p dest))
     (emit-branch-exchange-instruction segment
                                       (conditional-opcode condition)
                                       #b00010010 #b1111 #b1111
                                       #b1111 #b0011 (tn-offset dest)))))

;;;; Semaphore instructions

(defun emit-semaphore-instruction (segment opcode condition dest value address)
  (aver (register-p dest))
  (aver (register-p value))
  (aver (memory-operand-p address))
  (aver (zerop (memory-operand-offset address)))
  (aver (eq :offset (memory-operand-mode address)))
  (emit-dp-instruction segment (conditional-opcode condition)
                       #b00 0 opcode (tn-offset (memory-operand-base address))
                       (tn-offset dest)
                       (dpb #b1001 (byte 4 4) (tn-offset value))))

(define-instruction swp (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition dest value address))
     (emit-semaphore-instruction segment #b10000
                                 condition dest value address))))

(define-instruction swpb (segment &rest args)
  (:emitter
   (with-condition-defaulted (args (condition dest value address))
     (emit-semaphore-instruction segment #b10100
                                 condition dest value address))))

;;;; Status-register instructions

(define-instruction mrs (segment &rest args)
  (:printer dp-shift-immediate ((opcode-8 #b0010000)
                                (rn #b1111)
                                (shift '(0 0))
                                (rm 0))
            '(:name cond :tab rd ", CPSR"))
  (:printer dp-shift-immediate ((opcode-8 #b0010100)
                                (rn #b1111)
                                (shift '(0 0))
                                (rm 0))
            '(:name cond :tab rd ", SPSR"))
  (:emitter
   (with-condition-defaulted (args (condition dest reg))
     (aver (register-p dest))
     (aver (member reg '(:cpsr :spsr)))
     (emit-dp-instruction segment (conditional-opcode condition)
                          #b00 0 (if (eq reg :cpsr) #b10000 #b10100)
                          #b1111 (tn-offset dest) 0))))

(defun encode-status-register-fields (fields)
  (let ((fields (string fields)))
    (labels ((frob (mask index)
               (let* ((field (aref fields index))
                      (field-mask (cdr (assoc field
                                              '((#\C . #b0001) (#\X . #b0010)
                                                (#\S . #b0100) (#\F . #b1000))
                                              :test #'char=))))
                 (unless field-mask
                   (error "bad status register field desginator ~S" fields))
                 (if (< (1+ index) (length fields))
                     (frob (logior mask field-mask) (1+ index))
                     (logior mask field-mask)))))
      (frob 0 0))))

(defmacro cpsr (fields)
  (encode-status-register-fields fields))

(defmacro spsr (fields)
  (logior #b10000 (encode-status-register-fields fields)))

(define-instruction msr (segment &rest args)
  (:printer msr-immediate ())
  (:printer msr-register ())
  (:emitter
   (with-condition-defaulted (args (condition field-mask src))
     (aver (or (register-p src)
               (integerp src)))
     (let ((encoded-src (encode-shifter-operand src)))
       (emit-dp-instruction segment (conditional-opcode condition)
                            #b00 (ldb (byte 1 25) encoded-src)
                            (if (logbitp 4 field-mask) #b10110 #b10010)
                            field-mask #b1111
                            (ldb (byte 12 0) encoded-src))))))

;;;; Multiply instructions

(define-bitfield-emitter emit-multiply-instruction 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 4 4) (byte 4 0))

(macrolet
    ((define-multiply-instruction (name field-mapping opcode1 opcode2)
       (let ((arglist (ecase field-mapping
                        (:dzsm '(dest src multiplicand))
                        (:dnsm '(dest src multiplicand num))
                        (:ddsm '(dest-lo dest src multiplicand)))))
         `(define-instruction ,name (segment &rest args)
            (:printer ,(symbolicate 'multiply- field-mapping)
                      ((opcode-8 ,opcode1)
                       (opcode-4 ,opcode2)))
            (:emitter
             (with-condition-defaulted (args (condition ,@arglist))
               ,@(loop
                    for arg in arglist
                    collect `(aver (register-p ,arg)))
               (emit-multiply-instruction segment (conditional-opcode condition)
                                          ,opcode1
                                          (tn-offset dest)
                                          ,(ecase field-mapping
                                             (:dzsm 0)
                                             (:dnsm '(tn-offset num))
                                             (:ddsm '(tn-offset dest-lo)))
                                          (tn-offset src)
                                          ,opcode2
                                          (tn-offset multiplicand))))))))

  (define-multiply-instruction mul  :dzsm #b00000000 #b1001)
  (define-multiply-instruction muls :dzsm #b00000001 #b1001)
  (define-multiply-instruction mla  :dnsm #b00000010 #b1001)
  (define-multiply-instruction mlas :dnsm #b00000011 #b1001)

  (define-multiply-instruction umull  :ddsm #b00001000 #b1001)
  (define-multiply-instruction umulls :ddsm #b00001001 #b1001)
  (define-multiply-instruction umlal  :ddsm #b00001010 #b1001)
  (define-multiply-instruction umlals :ddsm #b00001011 #b1001)

  (define-multiply-instruction smull  :ddsm #b00001100 #b1001)
  (define-multiply-instruction smulls :ddsm #b00001101 #b1001)
  (define-multiply-instruction smlal  :ddsm #b00001110 #b1001)
  (define-multiply-instruction smlals :ddsm #b00001111 #b1001)

  (define-multiply-instruction smlabb :dnsm #b00010000 #b1000)
  (define-multiply-instruction smlatb :dnsm #b00010000 #b1010)
  (define-multiply-instruction smlabt :dnsm #b00010000 #b1100)
  (define-multiply-instruction smlatt :dnsm #b00010000 #b1110)

  (define-multiply-instruction smlalbb :ddsm #b00010100 #b1000)
  (define-multiply-instruction smlaltb :ddsm #b00010100 #b1010)
  (define-multiply-instruction smlalbt :ddsm #b00010100 #b1100)
  (define-multiply-instruction smlaltt :ddsm #b00010100 #b1110)

  (define-multiply-instruction smulbb :dzsm #b00010110 #b1000)
  (define-multiply-instruction smultb :dzsm #b00010110 #b1010)
  (define-multiply-instruction smulbt :dzsm #b00010110 #b1100)
  (define-multiply-instruction smultt :dzsm #b00010110 #b1110)

  (define-multiply-instruction smlawb :dnsm #b00010010 #b1000)
  (define-multiply-instruction smlawt :dnsm #b00010010 #b1100)

  (define-multiply-instruction smulwb :dzsm #b00010010 #b1010)
  (define-multiply-instruction smulwt :dzsm #b00010010 #b1110))

;;;; Load/store instructions

;;; Emit a load/store instruction.  CONDITION is a condition code
;;; name, KIND is :load or :store, WIDTH is :word or :byte, DATA is a
;;; register TN and ADDRESS is either a memory-operand structure or a
;;; stack TN.
(defun emit-load/store-instruction (segment condition kind width data address)
  (flet ((compute-opcode (direction mode)
           (let ((opcode-bits '(:load #b00001 :store #b00000
                                :word #b00000 :byte #b00100
                                :up #b01000 :down #b00000
                                :offset #b10000
                                :pre-index #b10010
                                :post-index #b00000)))
             (reduce #'logior (list kind width direction mode)
                     :key (lambda (value) (getf opcode-bits value))))))
    (etypecase address
      (memory-operand
       (let* ((base (memory-operand-base address))
              (offset (memory-operand-offset address))
              (direction (memory-operand-direction address))
              (mode (memory-operand-mode address))
              (cond-bits (conditional-opcode condition)))
         (cond
           ((label-p base)
            (emit-back-patch
             segment 4
             (lambda (segment posn)
               (let* ((label-delta (- (label-position base)
                                      (+ posn 8)))
                      (offset-delta (if (eq direction :up)
                                        offset
                                        (- offset)))
                      (overall-delta (+ label-delta
                                        offset-delta))
                      (absolute-delta (abs overall-delta)))
                 (aver (typep absolute-delta '(unsigned-byte 12)))
                 (emit-dp-instruction segment cond-bits #b01 0
                                      (compute-opcode (if (< overall-delta 0)
                                                          :down
                                                          :up)
                                                      mode)
                                      pc-offset (tn-offset data)
                                      absolute-delta)))))
           ((integerp offset)
            (aver (typep offset '(unsigned-byte 12)))
            (emit-dp-instruction segment cond-bits #b01 0
                                 (compute-opcode direction mode)
                                 (tn-offset base) (tn-offset data)
                                 offset))
           (t
            (emit-dp-instruction segment cond-bits #b01 1
                                 (compute-opcode direction mode)
                                 (tn-offset base) (tn-offset data)
                                 (encode-shifter-operand offset))))))

      #+(or)
      (tn
       ;; FIXME: This is for stack TN references, and needs must be
       ;; implemented.
       ))))

(macrolet
    ((define-load/store-instruction (name kind width)
       `(define-instruction ,name (segment &rest args)
          (:printer load/store-immediate ((opcode-3 #b010)
                                          (opcode-b ,(ecase width
                                                       (:word 0)
                                                       (:byte 1)))
                                          (opcode-l ,(ecase kind
                                                       (:load 1)
                                                       (:store 0)))))
          (:printer load/store-register ((opcode-3 #b011)
                                         (opcode-0 0)
                                         (opcode-b ,(ecase width
                                                      (:word 0)
                                                      (:byte 1)))
                                         (opcode-l ,(ecase kind
                                                      (:load 1)
                                                      (:store 0)))))
          (:emitter
           (with-condition-defaulted (args (condition reg address))
             (aver (or (register-p reg)
                       ,@(when (eq :store kind)
                               '((and (tn-p reg)
                                  (eq 'null (sc-name (tn-sc reg))))))))
             (emit-load/store-instruction segment condition
                                          ,kind ,width
                                          (if (register-p reg) reg null-tn)
                                          address))))))
  (define-load/store-instruction ldr :load :word)
  (define-load/store-instruction ldrb :load :byte)
  (define-load/store-instruction str :store :word)
  (define-load/store-instruction strb :store :byte))

(define-instruction ldm (segment &rest args) ; load multiple
  (:printer ldm/stm ((opcode-3 #b100) (opcode-l 1))))
(define-instruction stm (segment &rest args) ; store multiple
  (:printer ldm/stm ((opcode-3 #b100) (opcode-l 0))))

;;; Emit a miscellaneous load/store instruction.  CONDITION is a
;;; condition code name, OPCODE1 is the low bit of the first opcode
;;; field, OPCODE2 is the second opcode field, DATA is a register TN
;;; and ADDRESS is either a memory-operand structure or a stack TN.
(defun emit-misc-load/store-instruction (segment condition opcode1
                                         opcode2 data address)
  (flet ((compute-opcode (kind direction mode)
           (let ((opcode-bits '(:register #b00000 :immediate #b00100
                                :up #b01000 :down #b00000
                                :offset #b10000
                                :pre-index #b10010
                                :post-index #b00000)))
             (reduce #'logior (list kind direction mode)
                     :key (lambda (value) (getf opcode-bits value))))))
    (etypecase address
      (memory-operand
       (let* ((base (memory-operand-base address))
              (offset (memory-operand-offset address))
              (direction (memory-operand-direction address))
              (mode (memory-operand-mode address))
              (cond-bits (conditional-opcode condition)))
         (cond
           ((label-p base)
            (emit-back-patch
             segment 4
             (lambda (segment posn)
               (let* ((label-delta (- (label-position base)
                                      (+ posn 8)))
                      (offset-delta (if (eq direction :up)
                                        offset
                                        (- offset)))
                      (overall-delta (+ label-delta
                                        offset-delta))
                      (absolute-delta (abs overall-delta)))
                 (aver (typep absolute-delta '(unsigned-byte 8)))
                 (emit-multiply-instruction segment cond-bits
                                            (logior opcode1
                                                    (compute-opcode :immedaite
                                                                    (if (< overall-delta 0)
                                                                        :down
                                                                        :up)
                                                                    mode))
                                            pc-offset (tn-offset data)
                                            (ldb (byte 4 4) absolute-delta)
                                            opcode2 absolute-delta)))))
           ((integerp offset)
            (aver (typep offset '(unsigned-byte 8)))
            (emit-multiply-instruction segment cond-bits
                                       (logior opcode1
                                               (compute-opcode :immediate direction mode))
                                       (tn-offset base) (tn-offset data)
                                       (ldb (byte 4 4) offset)
                                       opcode2 offset))
           ((register-p offset)
            (emit-multiply-instruction segment cond-bits
                                       (logior opcode1
                                               (compute-opcode :register direction mode))
                                       (tn-offset base) (tn-offset data)
                                       0 opcode2 (tn-offset offset)))
           (t
            (error "bad thing for a miscellaneous load/store address ~S"
                   address)))))

      #+(or)
      (tn
       ;; FIXME: This is for stack TN references, and needs must be
       ;; implemented.
       ))))

(macrolet
    ((define-misc-load/store-instruction (name opcode1 opcode2 double-width)
       `(define-instruction ,name (segment &rest args)
          (:emitter
           (with-condition-defaulted (args (condition reg address))
             (aver (register-p reg))
             ,(when double-width '(aver (evenp (tn-offset reg))))
             (emit-misc-load/store-instruction segment condition
                                               ,opcode1 ,opcode2
                                               reg address))))))
  (define-misc-load/store-instruction strh 0 #b1011 nil)
  (define-misc-load/store-instruction ldrd 0 #b1101 t)
  (define-misc-load/store-instruction strd 0 #b1111 t)

  (define-misc-load/store-instruction ldrh 1 #b1011 nil)
  (define-misc-load/store-instruction ldrsb 1 #b1101 nil)
  (define-misc-load/store-instruction ldrsh 1 #b1111 nil))

;;;; Boxed-object computation instructions (for LRA and CODE)

;;; Compute the address of a CODE object by parsing the header of a
;;; nearby LRA or SIMPLE-FUN.
(define-instruction compute-code (segment code lip object-label temp)
  (:vop-var vop)
  (:emitter
   (emit-back-patch
    segment 12
    (lambda (segment position)
      (assemble (segment vop)
        ;; Calculate the address of the code component.  This is an
        ;; exercise in excess cleverness.  First, we calculate (from
        ;; our program counter only) the address of OBJECT-LABEL plus
        ;; OTHER-POINTER-LOWTAG.  The extra two words are to
        ;; compensate for the offset applied by ARM CPUs when reading
        ;; the program counter.
        (inst sub lip pc-tn (- ;; The 8 below is the displacement
                               ;; from reading the program counter.
                               (+ position 8)
                               (+ (label-position object-label)
                                  other-pointer-lowtag)))
        ;; Next, we read the function header.
        (inst ldr temp (@ lip (- other-pointer-lowtag)))
        ;; And finally we use the header value (a count in words),
        ;; plus the fact that the top two bits of the widetag are
        ;; clear (SIMPLE-FUN-WIDETAG is #x2A and
        ;; RETURN-PC-WIDETAG is #x36) to compute the boxed
        ;; address of the code component.
        (inst sub code lip (lsr temp (- 8 word-shift))))))))

;;; Compute the address of a nearby LRA object by dead reckoning from
;;; the location of the current instruction.
(define-instruction compute-lra (segment dest lip lra-label)
  (:vop-var vop)
  (:emitter
   ;; We can compute the LRA in a single instruction if the overall
   ;; offset puts it to within an 8-bit displacement.  Otherwise, we
   ;; need to load it by parts into LIP until we're down to an 8-bit
   ;; displacement, and load the final 8 bits into DEST.  We may
   ;; safely presume that an overall displacement may be up to 24 bits
   ;; wide (the PPC backend has special provision for branches over 15
   ;; bits, which implies that segments can become large, but a 16
   ;; megabyte segment (24 bits of displacement) is ridiculous), so we
   ;; need to cover a range of up to three octets of displacement.
   (labels ((compute-delta (position &optional magic-value)
              (- (+ (label-position lra-label
                                    (when magic-value position)
                                    magic-value)
                    other-pointer-lowtag)
                 ;; The 8 below is the displacement
                 ;; from reading the program counter.
                 (+ position 8)))

            (load-chunk (segment delta dst src chunk)
              (assemble (segment vop)
                (if (< delta 0)
                    (inst sub dst src chunk)
                    (inst add dst src chunk))))

            (three-instruction-emitter (segment position)
              (let* ((delta (compute-delta position))
                     (absolute-delta (abs delta)))
                (load-chunk segment delta
                            lip pc-tn (mask-field (byte 8 16) absolute-delta))
                (load-chunk segment delta
                            lip lip (mask-field (byte 8 8) absolute-delta))
                (load-chunk segment delta
                            dest lip (mask-field (byte 8 0) absolute-delta))))

            (two-instruction-emitter (segment position)
              (let* ((delta (compute-delta position))
                     (absolute-delta (abs delta)))
                (assemble (segment vop)
                  (load-chunk segment delta
                              lip pc-tn (mask-field (byte 8 8) absolute-delta))
                  (load-chunk segment delta
                              dest lip (mask-field (byte 8 0) absolute-delta)))))

            (one-instruction-emitter (segment position)
              (let* ((delta (compute-delta position))
                     (absolute-delta (abs delta)))
                (assemble (segment vop)
                  (load-chunk segment delta
                              dest pc-tn absolute-delta))))

            (two-instruction-maybe-shrink (segment chooser posn magic-value)
              (declare (ignore chooser))
              (let ((delta (compute-delta posn magic-value)))
                (when (<= (integer-length delta) 8)
                  (emit-back-patch segment 4
                                   #'one-instruction-emitter)
                  t)))

            (three-instruction-maybe-shrink (segment chooser posn magic-value)
              (declare (ignore chooser))
              (let ((delta (compute-delta posn magic-value)))
                (when (<= (integer-length delta) 16)
                  (emit-chooser segment 8 2
                                #'two-instruction-maybe-shrink
                                #'two-instruction-emitter)
                  t))))
     (emit-chooser
      ;; We need to emit up to three instructions, which is 12 octets.
      ;; This preserves a mere two bits of alignment.
      segment 12 2
      #'three-instruction-maybe-shrink
      #'three-instruction-emitter))))

;;; Load a register from a "nearby" LABEL by dead reckoning from the
;;; location of the current instruction.
(define-instruction load-from-label (segment &rest args)
  (:vop-var vop)
  (:emitter
   ;; ISTM this use of an interior-pointer is unnecessary. Since we know the
   ;; displacement of the label from the base of the CODE, we could load
   ;; either from [code+X] or [PC+X] where X is in an unsigned-reg.
   (with-condition-defaulted (args (condition dest lip label))
     ;; We can load the word addressed by a label in a single
     ;; instruction if the overall offset puts it to within a 12-bit
     ;; displacement.  Otherwise, we need to build an address by parts
     ;; into LIP until we're down to a 12-bit displacement, and then
     ;; apply the final 12 bits with LDR.  For now, we'll allow up to 20
     ;; bits of displacement, as that should be easy to implement, and a
     ;; megabyte large code object is already a bit unwieldly.  If
     ;; necessary, we can expand to a 28 bit displacement.
     (labels ((compute-delta (position &optional magic-value)
                (- (label-position label
                                   (when magic-value position)
                                   magic-value)
                   ;; The 8 below is the displacement
                   ;; from reading the program counter.
                   (+ position 8)))

              (load-chunk (segment delta dst src chunk)
                (assemble (segment vop)
                  (if (< delta 0)
                      (inst sub condition dst src chunk)
                      (inst add condition dst src chunk))))

              (two-instruction-emitter (segment position)
                (let* ((delta (compute-delta position))
                       (absolute-delta (abs delta)))
                  (assemble (segment vop)
                    (load-chunk segment delta
                                lip pc-tn (mask-field (byte 8 12) absolute-delta))
                    (inst ldr condition dest (@ lip (mask-field (byte 12 0) delta))))))

              (one-instruction-emitter (segment position)
                (let* ((delta (compute-delta position)))
                  (assemble (segment vop)
                    (inst ldr condition dest (@ pc-tn delta)))))

              (two-instruction-maybe-shrink (segment chooser posn magic-value)
                (declare (ignore chooser))
                (let ((delta (compute-delta posn magic-value)))
                  (when (<= (integer-length delta) 12)
                    (emit-back-patch segment 4
                                     #'one-instruction-emitter)
                    t))))
       (emit-chooser
        ;; We need to emit up to two instructions, which is 8 octets,
        ;; but might wish to emit only one.  This preserves a mere two
        ;; bits of alignment.
        segment 8 2
        #'two-instruction-maybe-shrink
        #'two-instruction-emitter)))))

(define-instruction adr (segment code label &optional (offset 0))
  (:vop-var vop)
  (:emitter
   (emit-back-patch
    segment 4
    (lambda (segment position)
      (assemble (segment vop)
        (let ((offset (+ (- (label-position label)
                            (+ position 8))
                         offset)))
          (if (plusp offset)
              (inst add code pc-tn offset)
              (inst sub code pc-tn (- offset)))))))))

;; data processing floating point instructions
(define-bitfield-emitter emit-fp-dp-instruction 32
  (byte 4 28) ; cond
  (byte 4 24) ; #b1110
  (byte 1 23) ; p
  (byte 1 22) ; D
  (byte 1 21) ; q
  (byte 1 20) ; r
  (byte 4 16) ; Fn || extension op
  (byte 4 12) ; Fd
  (byte 3 9) ; #b101
  (byte 1 8) ; double/single precission
  (byte 1 7) ; N || extension op
  (byte 1 6) ; s
  (byte 1 5) ; M
  (byte 1 4) ; #b0
  (byte 4 0)) ; Fm

(defun low-bit-float-reg (reg-tn)
  (logand 1 (tn-offset reg-tn)))

(defun high-bits-float-reg (reg-tn)
  (ash (tn-offset reg-tn) -1))

(defmacro define-binary-fp-data-processing-instruction (name precision p q r s)
  (let ((precision-flag (ecase precision
                          (:single 0)
                          (:double 1))))
    `(define-instruction ,name (segment &rest args)
       (:printer fp-binary ((p ,p)
                            (q ,q)
                            (r ,r)
                            (s ,s)
                            (size ,precision-flag)))
       (:emitter
        (with-condition-defaulted (args (condition dest op-n op-m))
          (emit-fp-dp-instruction segment
                                  (conditional-opcode condition)
                                  #b1110
                                  ,p
                                  (low-bit-float-reg dest)
                                  ,q
                                  ,r
                                  (high-bits-float-reg op-n)
                                  (high-bits-float-reg dest)
                                  #b101
                                  ,precision-flag
                                  (low-bit-float-reg op-n)
                                  ,s
                                  (low-bit-float-reg op-m)
                                  #b0
                                  (high-bits-float-reg op-m)))))))

(defmacro define-binary-fp-data-processing-instructions (root p q r s)
  `(progn
     (define-binary-fp-data-processing-instruction ,(symbolicate root 's) :single ,p ,q ,r ,s)
     (define-binary-fp-data-processing-instruction ,(symbolicate root 'd) :double ,p ,q ,r ,s)))

(define-binary-fp-data-processing-instructions fmac  0 0 0 0)
(define-binary-fp-data-processing-instructions fnmac 0 0 0 1)
(define-binary-fp-data-processing-instructions fmsc  0 0 1 0)
(define-binary-fp-data-processing-instructions fnmsc 0 0 1 1)
(define-binary-fp-data-processing-instructions fmul  0 1 0 0)
(define-binary-fp-data-processing-instructions fnmul 0 1 0 1)
(define-binary-fp-data-processing-instructions fadd  0 1 1 0)
(define-binary-fp-data-processing-instructions fsub  0 1 1 1)
(define-binary-fp-data-processing-instructions fdiv  1 0 0 0)

;;; op-m-sbz means that it should-be-zero, and only one register is supplied.
(defmacro define-unary-fp-data-processing-instruction (name precision fn n
                                                       &key op-m-sbz)
  (let ((precision-flag (ecase precision
                          (:single 0)
                          (:double 1))))
    `(define-instruction ,name (segment &rest args)
       (:printer ,(if op-m-sbz
                      'fp-unary-one-op
                      'fp-unary)
                 ((size ,precision-flag)
                  (n ,n)
                  (opc ,fn)))
       (:emitter
        (with-condition-defaulted (args (condition dest
                                                   ,@(unless op-m-sbz
                                                       '(op-m))))
          (emit-fp-dp-instruction segment
                                  (conditional-opcode condition)
                                  #b1110
                                  #b1
                                  (low-bit-float-reg dest)
                                  #b1
                                  #b1
                                  ,fn
                                  (high-bits-float-reg dest)
                                  #b101
                                  ,precision-flag
                                  ,n
                                  #b1
                                  ,(if op-m-sbz
                                       0
                                       '(low-bit-float-reg op-m))
                                  #b0
                                  ,(if op-m-sbz
                                       0
                                       '(high-bits-float-reg op-m))))))))

(defmacro define-unary-fp-data-processing-instructions (root fn n &key op-m-sbz)
  `(progn
     (define-unary-fp-data-processing-instruction ,(symbolicate root 's) :single ,fn ,n
       :op-m-sbz ,op-m-sbz)
     (define-unary-fp-data-processing-instruction ,(symbolicate root 'd) :double ,fn ,n
       :op-m-sbz ,op-m-sbz)))

(define-unary-fp-data-processing-instructions fcpy   #b0000 0)
(define-unary-fp-data-processing-instructions fabs   #b0000 1)
(define-unary-fp-data-processing-instructions fneg   #b0001 0)
(define-unary-fp-data-processing-instructions fsqrt  #b0001 1)
(define-unary-fp-data-processing-instructions fcmp   #b0100 0)
(define-unary-fp-data-processing-instructions fcmpe  #b0100 1)
(define-unary-fp-data-processing-instructions fcmpz  #b0101 0  :op-m-sbz t)
(define-unary-fp-data-processing-instructions fcmpez #b0101 1  :op-m-sbz t)
(define-unary-fp-data-processing-instructions fuito  #b1000 0)
(define-unary-fp-data-processing-instructions fsito  #b1000 1)
(define-unary-fp-data-processing-instructions ftoui  #b1100 0)
(define-unary-fp-data-processing-instructions ftouiz #b1100 1)
(define-unary-fp-data-processing-instructions ftosi  #b1101 0)
(define-unary-fp-data-processing-instructions ftosiz #b1101 1)

(define-unary-fp-data-processing-instruction fcvtds :single #b0111 1)
(define-unary-fp-data-processing-instruction fcvtsd :double #b0111 1)

;;; Load/Store Float Instructions

(define-bitfield-emitter emit-fp-ls-instruction 32
  (byte 4 28) ; cond
  (byte 3 25) ; #b110
  (byte 1 24) ; P
  (byte 1 23) ; U
  (byte 1 22) ; D
  (byte 1 21) ; W
  (byte 1 20) ; L
  (byte 4 16) ; Rn
  (byte 4 12) ; Fd
  (byte 3 9) ; #b101
  (byte 1 8) ; double/single precission
  (byte 8 0)) ; offset

;; Define a load/store multiple floating point instruction. PRECISION is
;; :SINGLE for single precision values and :DOUBLE for double precision values.
;; DIRECTION has to be either :LOAD or :STORE.
;; If INC-OFFSET is true, the offset part of the instruction will be incremented by 1
;; indicating in the double case a load/store unknown instruction.
(defmacro define-load-store-multiple-fp-instruction (name precision direction &optional inc-offset)
  (let ((precision-flag (ecase precision
                          (:single 0)
                          (:double 1)))
        (direction-flag (ecase direction
                          (:load 1)
                          (:store 0))))
    `(define-instruction ,name (segment &rest args)
       (:emitter
        (with-condition-defaulted (args (condition address base-reg reg-count))
          (let* ((mode (cond
                         ((consp address)
                          (cdr address))
                         (t :unindexed)))
                 (p (ecase mode
                      ((:unindexed :increment) 0)
                      ((:decrement) 1)))
                 (u (ecase mode
                      ((:unindexed :increment) 1)
                      ((:decrement) 0)))
                 (w (ecase mode
                      ((:unindexed) 0)
                      ((:increment :decrement) 1))))
            (emit-fp-ls-instruction segment
                                    (conditional-opcode condition)
                                    #b110
                                    p
                                    u
                                    (low-bit-float-reg base-reg)
                                    w
                                    ,direction-flag
                                    (tn-offset address)
                                    (high-bits-float-reg base-reg)
                                    #b101
                                    ,precision-flag
                                    ,(ecase precision
                                       (:single 'reg-count)
                                       (:double `(+ (* 2 reg-count)
                                                    ,(if inc-offset 1 0)))))))))))

;; multiple single precision
(define-load-store-multiple-fp-instruction fstms :single :store)
(define-load-store-multiple-fp-instruction fldms :single :load)
;; multiple double precision
(define-load-store-multiple-fp-instruction fstmd :double :store)
(define-load-store-multiple-fp-instruction fldmd :double :load)
;; multiple double precision registers of unknown content (needs up to 2 * reg-count + 1 words of space)
(define-load-store-multiple-fp-instruction fstmx :double :store t)
(define-load-store-multiple-fp-instruction fldmx :double :load t)

;; KLUDGE: this group of pseudo-instructions are fragile (no error
;; handling for the various ways to mis-use them), have no support for
;; predication, and use the somewhat-broken interface for the
;; load-store-multiple-fp instructions above.
(define-instruction-macro load-complex-single (dest memory-operand)
  `(inst fldms (memory-operand-base ,memory-operand) ,dest 2))
(define-instruction-macro load-complex-double (dest memory-operand)
  `(inst fldmd (memory-operand-base ,memory-operand) ,dest 2))
(define-instruction-macro store-complex-single (src memory-operand)
  `(inst fstms (memory-operand-base ,memory-operand) ,src 2))
(define-instruction-macro store-complex-double (src memory-operand)
  `(inst fstmd (memory-operand-base ,memory-operand) ,src 2))

;; Define a load/store one floating point instruction. PRECISION is
;; :SINGLE for single precision values and :DOUBLE for double precision values.
;; DIRECTION has to be either :LOAD or :STORE.
(defmacro define-load-store-one-fp-instruction (name precision direction)
  (let ((precision-flag (ecase precision
                          (:single 0)
                          (:double 1)))
        (direction-flag (ecase direction
                          (:load 1)
                          (:store 0))))
    `(define-instruction ,name (segment &rest args)
       (:emitter
        (with-condition-defaulted (args (condition float-reg memory-operand))
          (let ((base (memory-operand-base memory-operand))
                (offset (memory-operand-offset memory-operand))
                (direction (memory-operand-direction memory-operand)))
            (aver (eq (memory-operand-mode memory-operand) :offset))
            (aver (and (integerp offset)
                       (zerop (logand offset 3))))
            ;; FIXME: Should support LABEL bases.
            (aver (tn-p base))
            (emit-fp-ls-instruction segment
                                    (conditional-opcode condition)
                                    #b110
                                    1
                                    (if (eq direction :up) 1 0)
                                    (low-bit-float-reg float-reg)
                                    0
                                    ,direction-flag
                                    (tn-offset base)
                                    (high-bits-float-reg float-reg)
                                    #b101
                                    ,precision-flag
                                    (ash offset -2))))))))

(define-load-store-one-fp-instruction fsts :single :store)
(define-load-store-one-fp-instruction flds :single :load)
(define-load-store-one-fp-instruction fstd :double :store)
(define-load-store-one-fp-instruction fldd :double :load)


;; single register transfer instructions

(define-bitfield-emitter emit-fp-srt-instruction 32
  (byte 4 28) ; cond
  (byte 4 24) ; #b1110
  (byte 3 21) ; opc
  (byte 1 20) ; L

  (byte 4 16) ; Fn
  (byte 4 12) ; Rd
  (byte 3 9) ; #b101
  (byte 1 8) ; precision

  (byte 1 7) ; N
  (byte 7 0)) ; #b0010000

(define-bitfield-emitter emit-conditional-instruction 32
  (byte 4 28)                           ; cond
  (byte 28 0))                          ; op

;;; This has the same encoding as FMRX R15, FPSCR
(define-instruction fmstat (segment &rest args)
  (:printer conditional
            ((op #xEF1FA10)))
  (:emitter
   (with-condition-defaulted (args (condition))
     (emit-conditional-instruction  segment
                                    (conditional-opcode condition)
                                    #xEF1FA10))))

(defun system-reg-encoding (float-reg)
  (ecase float-reg
    (:fpsid #b0000)
    (:fpscr #b0001)
    (:fpexc #b1000)))

(defmacro define-single-reg-transfer-fp-instruction (name precision direction opcode &optional system-reg)
  (let ((precision-flag (ecase precision
                          (:single 0)
                          (:double 1)))
        (direction-flag (ecase direction
                          (:to-arm 1)
                          (:from-arm 0))))
    `(define-instruction ,name (segment &rest args)
       (:printer ,(if system-reg
                      'fp-srt-sys
                      'fp-srt)
                 ((opc ,opcode)
                  (l ,direction-flag)
                  (size ,precision-flag))
                 ',(if (eq direction :to-arm)
                      '(:name cond :tab rd ", " fn)
                      '(:name cond :tab fn ", " rd)))
       (:emitter
        (with-condition-defaulted (args (condition ,@(if (eq direction :to-arm)
                                                         '(arm-reg float-reg)
                                                         '(float-reg arm-reg))))
          (emit-fp-srt-instruction segment
                                   (conditional-opcode condition)
                                   #b1110
                                   ,opcode
                                   ,direction-flag
                                   ,(if system-reg
                                        '(system-reg-encoding float-reg)
                                        '(high-bits-float-reg float-reg))
                                   (tn-offset arm-reg)
                                   #b101
                                   ,precision-flag
                                   ,(if system-reg
                                        0
                                        '(low-bit-float-reg float-reg))
                                   #b0010000))))))

(define-single-reg-transfer-fp-instruction fmsr :single :from-arm #b000)
(define-single-reg-transfer-fp-instruction fmrs :single :to-arm #b000)
(define-single-reg-transfer-fp-instruction fmdlr :double :from-arm #b000)
(define-single-reg-transfer-fp-instruction fmrdl :double :to-arm #b000)
(define-single-reg-transfer-fp-instruction fmdhr :double :from-arm #b001)
(define-single-reg-transfer-fp-instruction fmrdh :double :to-arm #b001)
(define-single-reg-transfer-fp-instruction fmxr :single :from-arm #b111 t)
(define-single-reg-transfer-fp-instruction fmrx :single :to-arm #b111 t)

(define-bitfield-emitter emit-fp-trt-instruction 32
  (byte 4 28) ; cond
  (byte 7 21) ; #b1100010
  (byte 1 20) ; L
  (byte 4 16) ; Rn
  (byte 4 12) ; Rd
  (byte 3 9) ; #b101
  (byte 1 8) ; precision
  (byte 2 6) ; #b00
  (byte 1 5) ; M
  (byte 1 4) ; #b1
  (byte 4 0)) ; Fm

(defmacro define-two-reg-transfer-fp-instruction (name precision direction)
  (let ((precision-flag (ecase precision
                          (:single 0)
                          (:double 1)))
        (direction-flag (ecase direction
                          (:to-arm 1)
                          (:from-arm 0))))
    `(define-instruction ,name (segment &rest args)
       (:printer fp-trt
                 ((l ,direction-flag)
                  (size ,precision-flag))
                 ',(if (eq direction :to-arm)
                       '(:name cond :tab rd ", " rn ", " fm)
                       '(:name cond :tab fm ", " rd ", " rn )))
       (:emitter
        (with-condition-defaulted (args (condition ,@(if (eq direction :to-arm)
                                                         '(arm-reg-1 arm-reg-2 float-reg)
                                                         '(float-reg arm-reg-1 arm-reg-2))))
          (emit-fp-trt-instruction segment
                                   (conditional-opcode condition)
                                   #b1100010
                                   ,direction-flag
                                   (tn-offset arm-reg-2)
                                   (tn-offset arm-reg-1)
                                   #b101
                                   ,precision-flag
                                   #b00
                                   (low-bit-float-reg float-reg)
                                   #b1
                                   (high-bits-float-reg float-reg)))))))

(define-two-reg-transfer-fp-instruction fmsrr :single :from-arm)
(define-two-reg-transfer-fp-instruction fmrrs :single :to-arm)
(define-two-reg-transfer-fp-instruction fmdrr :double :from-arm)
(define-two-reg-transfer-fp-instruction fmrrd :double :to-arm)

(sb-assem::%def-inst-encoder
 '.layout-id
 (lambda (segment layout)
   (sb-c:note-fixup segment :layout-id (sb-c:make-fixup layout :layout-id))))

(defun sb-vm:fixup-code-object (code offset value kind flavor)
  (declare (type index offset) (ignore flavor))
  (unless (zerop (rem offset sb-assem:+inst-alignment-bytes+))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (let ((sap (code-instructions code)))
    (ecase kind
      (:layout-id
       (aver (typep value '(unsigned-byte 24)))
       (setf (sap-ref-word sap offset)
             (dpb (ldb (byte 8 16) value) (byte 8 0) (sap-ref-word sap offset))
             (sap-ref-word sap (+ offset 4))
             (dpb (ldb (byte 8  8) value) (byte 8 0) (sap-ref-word sap (+ offset 4)))
             (sap-ref-word sap (+ offset 8))
             (dpb (ldb (byte 8  0) value) (byte 8 0) (sap-ref-word sap (+ offset 8)))))
      (:absolute
       (setf (sap-ref-32 sap offset) value))))
  nil)

(define-instruction store-coverage-mark (segment path-index temp)
  (:emitter
   ;; No backpatch is needed to compute the offset into the code header
   ;; because COMPONENT-HEADER-LENGTH is known at this point.
   (let* ((offset (+ (component-header-length)
                     n-word-bytes ; skip over jump table word
                     path-index
                     (- other-pointer-lowtag)))
          (addr
           (@ sb-vm::code-tn
              (etypecase offset
                ((integer 0 4095) offset)
                ((unsigned-byte 31)
                 (inst* segment 'movw temp (logand offset #xffff))
                 (when (ldb-test (byte 16 16) offset)
                   (inst* segment 'movt temp (ldb (byte 16 16) offset)))
                 temp)))))
     (inst* segment 'strb sb-vm::null-tn addr))))
