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

(in-package "SB-ARM64-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from this package into SB-VM
  (import '(conditional-opcode negate-condition
            add-sub-immediate-p fixnum-add-sub-immediate-p
            negative-add-sub-immediate-p
            encode-logical-immediate fixnum-encode-logical-immediate
            ldr-str-offset-encodable ldp-stp-offset-p
            bic-mask extend lsl lsr asr ror @ encode-fp-immediate) "SB-VM")
  ;; Imports from SB-VM into this package
  (import '(sb-vm::*register-names*
            sb-vm::add-sub-immediate
            sb-vm::32-bit-reg sb-vm::single-reg sb-vm::double-reg
            sb-vm::complex-single-reg sb-vm::complex-double-reg
            sb-vm::tmp-tn sb-vm::zr-tn sb-vm::nsp-offset)))



(defconstant-eqx +conditions+
  '((:eq . 0)
    (:ne . 1)
    (:hs . 2) (:cs . 2)
    (:lo . 3) (:cc . 3)
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
  (let ((vec (make-array 16 :initial-element nil)))
    (dolist (cond +conditions+ vec)
      (when (null (aref vec (cdr cond)))
        (setf (aref vec (cdr cond)) (car cond)))))
  #'equalp)

(defun conditional-opcode (condition)
  (cdr (assoc condition +conditions+ :test #'eq)))

(defun negate-condition (condition)
  (aref +condition-name-vec+
        (logxor 1 (conditional-opcode condition))))

;;;; disassembler field definitions

(progn

  (define-arg-type shift :printer #'print-shift)

  (define-arg-type 2-bit-shift :printer #'print-2-bit-shift)

  (define-arg-type wide-shift :printer #'print-wide-shift)

  (define-arg-type extend :printer #'print-extend)

  (define-arg-type ldr-str-extend :printer #'print-ldr-str-extend)

  (define-arg-type scaled-immediate :printer #'print-scaled-immediate)

  (define-arg-type immediate :sign-extend t :printer #'print-immediate)

  (define-arg-type unsigned-immediate :printer #'print-immediate)

  (define-arg-type logical-immediate :printer #'print-logical-immediate)

  (define-arg-type imm-writeback :printer #'print-imm-writeback)

  (define-arg-type pair-imm-writeback :printer #'print-pair-imm-writeback)

  (define-arg-type test-branch-immediate :printer #'print-test-branch-immediate)

  (define-arg-type reg :printer #'print-reg)

  (define-arg-type x-reg :printer #'print-x-reg)

  (define-arg-type x-reg-sp :printer #'print-x-reg-sp)

  (define-arg-type w-reg :printer #'print-w-reg)

  (define-arg-type reg-sp :printer #'print-reg-sp)

  (define-arg-type sized-reg :printer #'print-sized-reg)

  (define-arg-type reg-float-reg :printer #'print-reg-float-reg)

  (define-arg-type float-reg :printer #'print-float-reg)
  (define-arg-type vbhs :printer #'print-vbhs)
  (define-arg-type vhsd :printer #'print-vhsd)
  (define-arg-type vx.t :printer #'print-vx.t)

  (define-arg-type simd-reg :printer #'print-simd-reg)

  (define-arg-type simd-copy-reg :printer #'print-simd-copy-reg)
  (define-arg-type simd-dup-reg :printer #'print-simd-dup-reg)

  (define-arg-type simd-immh-reg :printer #'print-simd-immh-reg)
  (define-arg-type simd-immh-shift-left :printer #'print-simd-immh-shift-left)
  (define-arg-type simd-immh-shift-right :printer #'print-simd-immh-shift-right)
  (define-arg-type simd-modified-imm :printer #'print-simd-modified-imm)
  (define-arg-type simd-reg-cmode :printer #'print-simd-reg-cmode)
  (define-arg-type simd-table-regs :printer #'print-simd-table-regs)
  (define-arg-type simd-b-reg :printer #'print-simd-b-reg)

  (define-arg-type fp-imm :printer #'print-fp-imm)

  (define-arg-type sys-reg :printer #'print-sys-reg)

  (define-arg-type cond :printer #'print-cond)
  (define-arg-type negated-cond :printer #'print-negated-cond)

  (define-arg-type ldr-str-annotation :printer #'annotate-ldr-str-imm)
  (define-arg-type ldr-str-pair-annotation :printer #'annotate-ldr-str-pair)

  (define-arg-type ldr-str-reg-annotation :printer #'annotate-ldr-str-reg)
  (define-arg-type ldr-literal-annotation :printer #'annotate-ldr-literal :sign-extend t)

  (define-arg-type add-sub-imm-annotation :printer #'annotate-add-sub-imm)

  (define-arg-type label :sign-extend t :use-label #'use-label))

;;;; primitive emitters

(define-bitfield-emitter emit-word 32
  (byte 32 0))

(define-bitfield-emitter emit-dword 64
  (byte 64 0))

;;;; miscellaneous hackery

(defun register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'sb-vm::registers)))

(defun fp-register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'sb-vm::float-registers)))

(defun reg-size (tn)
  (if (sc-is tn 32-bit-reg)
      0
      1))

(defun reg-offset (tn)
  (aver (or (register-p tn)
            (fp-register-p tn)))
  (tn-offset tn))

(defun fpr-offset (tn)
  (aver (fp-register-p tn))
  (tn-offset tn))

(defun gpr-offset (tn)
  (aver (register-p tn))
  (tn-offset tn))

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

(define-instruction dword (segment word)
  (:emitter
   (etypecase word
     (fixup
      (note-fixup segment :absolute word)
      (emit-dword segment 0))
     (integer
      (emit-dword segment word)))))

(define-instruction simple-fun-header-word (segment)
  (:emitter
   (emit-back-patch segment
                    8
                    (lambda (segment posn)
                      (emit-dword segment
                                  (logior simple-fun-widetag
                                          (ash (+ posn
                                                  (component-header-length))
                                               (- n-widetag-bits
                                                  word-shift))))))))


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
            (typep operand '(integer 0 63))))

  (make-shifter-operand :register register :function-code 0 :operand operand))

(defun lsr (register operand)
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 0 63))))

  (make-shifter-operand :register register :function-code 1 :operand operand))

(defun asr (register operand)
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 1 63))))

  (make-shifter-operand :register register :function-code 2 :operand operand))

(defun ror (register operand)
  ;; ROR is a special case: the encoding for ROR with an immediate
  ;; shift of 32 (0) is actually RRX.
  (aver (register-p register))
  (aver (or (register-p operand)
            (typep operand '(integer 1 63))))

  (make-shifter-operand :register register :function-code 3 :operand operand))

(defun rrx (register)
  ;; RRX is a special case: it is encoded as ROR with an immediate
  ;; shift of 32 (0), and has no operand.
  (aver (register-p register))
  (make-shifter-operand :register register :function-code 3 :operand 0))

(defstruct (extend
            (:constructor extend (register kind &optional (operand 0))))
  (register nil :type tn)
  kind
  (operand 0 :type (integer 0 63)))

(define-condition cannot-encode-immediate-operand (error)
  ((value :initarg :value))
  (:report
   (lambda (condition stream)
     (format stream "Cannot encode ~S" (slot-value condition 'value)))))

(defun encode-shifted-register (operand)
  (etypecase operand
    (tn
     (values 0 0 operand))
    (shifter-operand
     (values (shifter-operand-function-code operand)
             (shifter-operand-operand operand)
             (shifter-operand-register operand)))))


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
  mode)

;;; The @ function is used to encode a memory addressing mode.  The
;;; parameters for the base form are a base register, an optional
;;; offset (either an integer, a register tn or a shifter-operand
;;; structure with a constant shift amount, optionally within a unary
;;; - form), and a base register writeback mode (either :offset,
;;; :pre-index, or :post-index).  The alternative form uses a label as
;;; the base register, and accepts only (optionally negated) integers
;;; as offsets, and requires a mode of :offset.
(defun @ (base &optional (offset 0) (mode :offset))
  (when (label-p base)
    (aver (eq mode :offset))
    (aver (integerp offset)))

  (when (shifter-operand-p offset)
    (aver (integerp (shifter-operand-operand offset))))

  (make-memory-operand :base base :offset offset
                       :mode mode))


;;;; Data-processing instructions


(defmacro def-emitter (name &body specs)
  (collect ((arg-names) (arg-types))
    (let* ((total-bits 32)
           (overall-mask (ash -1 total-bits))
           (num-bytes (truncate total-bits assembly-unit-bits))
           (bytes (make-array num-bytes :initial-element nil)))
      (dolist (spec-expr specs)
        (destructuring-bind (arg size pos) spec-expr
          (when (ldb-test (byte size pos) overall-mask)
            (error "The byte spec ~S either overlaps another byte spec, or ~
                    extends past the end."
                   spec-expr))
          (setf (ldb (byte size pos) overall-mask) -1)
          (unless (numberp arg)
            (arg-names arg)
            (arg-types `(type (integer ,(ash -1 (1- size))
                                       ,(1- (ash 1 size)))
                              ,arg)))
          (multiple-value-bind (start-byte offset)
              (floor pos assembly-unit-bits)
            (let ((end-byte (floor (1- (+ pos size))
                                   assembly-unit-bits)))
              (flet ((maybe-ash (expr offset)
                       (if (zerop offset)
                           expr
                           `(ash ,expr ,offset))))
                (declare (inline maybe-ash))
                (cond ((zerop size))
                      ((= start-byte end-byte)
                       (push (maybe-ash `(ldb (byte ,size 0) ,arg)
                                        offset)
                             (svref bytes start-byte)))
                      (t
                       (push (maybe-ash
                              `(ldb (byte ,(- assembly-unit-bits offset) 0)
                                    ,arg)
                              offset)
                             (svref bytes start-byte))
                       (do ((index (1+ start-byte) (1+ index)))
                           ((>= index end-byte))
                         (push
                          `(ldb (byte ,assembly-unit-bits
                                      ,(- (* assembly-unit-bits
                                             (- index start-byte))
                                          offset))
                                ,arg)
                          (svref bytes index)))
                       (let ((len (rem (+ size offset)
                                       assembly-unit-bits)))
                         (push
                          `(ldb (byte ,(if (zerop len)
                                           assembly-unit-bits
                                           len)
                                      ,(- (* assembly-unit-bits
                                             (- end-byte start-byte))
                                          offset))
                                ,arg)
                          (svref bytes end-byte))))))))))
      (unless (= overall-mask -1)
        (error "There are holes. ~v,'0b"
               total-bits
               (ldb (byte total-bits 0) overall-mask)))
      (let ((forms nil))
        (dotimes (i num-bytes)
          (let ((pieces (svref bytes i)))
            (aver pieces)
            (push `(emit-byte segment
                              ,(if (cdr pieces)
                                   `(logior ,@pieces)
                                   (car pieces)))
                  forms)))
        `(defun ,(symbolicate "EMIT-" name) (segment ,@(arg-names))
           (declare (type sb-assem:segment segment) ,@(arg-types))
           ,@(ecase *backend-byte-order*
               (:little-endian (nreverse forms))
               (:big-endian forms))
           nil)))))

(defconstant +64-bit-size+ 1)

(def-emitter add-sub-imm
  (size 1 31)
  (op 2 29)
  (#b10001 5 24)
  (shift 2 22)
  (imm 12 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (add-sub 32)
  (op :field (byte 2 29))
  (rn :field (byte 5 5) :type 'reg-sp)
  (rd :field (byte 5 0) :type 'reg-sp))

(define-instruction-format
    (add-sub-imm 32
     :default-printer '(:name :tab rd ", " rn ", " imm shift
                        add-sub-imm-annotation)
     :include add-sub)
  (op2 :field (byte 5 24) :value #b10001)
  (shift :field (byte 2 22) :type '2-bit-shift)
  (imm :field (byte 12 10) :type 'unsigned-immediate)
  (add-sub-imm-annotation :fields (list (byte 5 5) (byte 2 22) (byte 12 10))
                          :type 'add-sub-imm-annotation))

(define-instruction-format
    (adds-subs-imm 32
     :include add-sub-imm
     :default-printer '(:name :tab rd ", " rn ", " imm shift))
  (rd :type 'reg))

(define-instruction-format
    (add-sub-shift-reg 32
     :default-printer '(:name :tab rd ", " rn ", " rm shift)
     :include add-sub)
  (op2 :field (byte 5 24) :value #b01011)
  (op3 :field (byte 1 21) :value #b0)
  (shift :fields (list (byte 2 22) (byte 6 10)) :type 'shift)
  (rm :field (byte 5 16) :type 'reg)
  (rn :type 'reg)
  (rd :type 'reg))

(def-emitter add-sub-shift-reg
  (size 1 31)
  (op 2 29)
  (#b01011 5 24)
  (shift 2 22)
  (#b0 1 21)
  (rm 5 16)
  (imm 6 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format
    (add-sub-ext-reg 32
     :default-printer '(:name :tab rd ", " rn ", " rm extend)
     :include add-sub)
  (op2 :field (byte 8 21) :value #b01011001)
  (extend :fields (list (byte 3 13) (byte 3 10)) :type 'extend)
  (rm :field (byte 5 16) :type 'reg)
  (rd :type 'reg))

(def-emitter add-sub-ext-reg
  (size 1 31)
  (op 2 29)
  (#b01011001 8 21)
  (rm 5 16)
  (option 3 13)
  (imm 3 10)
  (rn 5 5)
  (rd 5 0))

(defun add-sub-immediate-p (x)
  (or (typep x '(unsigned-byte 12))
      (and (typep x '(unsigned-byte 24))
           (not (ldb-test (byte 12 0) x)))))

(defun fixnum-add-sub-immediate-p (x)
  (and (fixnump x)
       (let ((x (fixnumize x)))
         (or (typep x '(unsigned-byte 12))
             (and (typep x '(unsigned-byte 24))
                  (not (ldb-test (byte 12 0) x)))))))

(defun negative-add-sub-immediate-p (x)
  (and (typep x '(integer * -1))
       (let ((x (- x)))
         (or (typep x '(unsigned-byte 12))
             (and (typep x '(unsigned-byte 24))
                  (not (ldb-test (byte 12 0) x)))))))

(defmacro def-add-sub (name op &rest printers)
  `(define-instruction ,name (segment rd rn rm)
     ,@printers
     (:emitter
      (let ((size (reg-size rn)))
       (cond ((or (register-p rm)
                  (shifter-operand-p rm))
              (multiple-value-bind (shift amount rm) (encode-shifted-register rm)
                (emit-add-sub-shift-reg segment size ,op shift (gpr-offset rm)
                                        amount (gpr-offset rn) (gpr-offset rd))))
             ((extend-p rm)
              (let* ((shift (extend-operand rm))
                     (extend (ecase (extend-kind rm)
                               (:uxtb #b00)
                               (:uxth #b001)
                               (:uxtw #b010)
                               ((:lsl :uxtx) #b011)
                               (:sxtb #b100)
                               (:sxth #b101)
                               (:sxtw #b110)
                               (:sxtx #b111)))
                     (rm (extend-register rm)))
                (emit-add-sub-ext-reg segment size ,op
                                      (gpr-offset rm)
                                      extend shift (gpr-offset rn) (gpr-offset rd))))
             (t
              (let ((imm rm)
                    (shift 0))
                (when (and (typep imm '(unsigned-byte 24))
                           (not (zerop imm))
                           (not (ldb-test (byte 12 0) imm)))
                  (setf imm (ash imm -12)
                        shift 1))
                (emit-add-sub-imm segment size ,op shift imm
                                  (gpr-offset rn) (gpr-offset rd)))))))))

(def-add-sub add #b00
  (:printer add-sub-imm ((op #b00)))
  (:printer add-sub-imm ((op #b00) (rd #.nsp-offset) (imm 0))
            '('mov :tab rd ", " rn))
  (:printer add-sub-imm ((op #b00) (rn #.nsp-offset) (imm 0))
            '('mov :tab rd ", " rn))
  (:printer add-sub-ext-reg ((op #b00)))
  (:printer add-sub-shift-reg ((op #b00))))

(def-add-sub adds #b01
  (:printer add-sub-imm ((op #b01) (rd nil :type 'reg)))
  (:printer add-sub-ext-reg ((op #b01) (rd nil :type 'reg)))
  (:printer add-sub-shift-reg ((op #b01)))
  (:printer add-sub-imm ((op #b01) (rd #b11111))
            '('cmn :tab rn ", " imm shift))
  (:printer add-sub-ext-reg ((op #b01) (rd #b11111))
            '('cmn :tab rn ", " rm extend))
  (:printer add-sub-shift-reg ((op #b01) (rd #b11111))
            '('cmn :tab rn ", " rm shift)))

(def-add-sub sub #b10
  (:printer add-sub-imm ((op #b10)))
  (:printer add-sub-ext-reg ((op #b10)))
  (:printer add-sub-shift-reg ((op #b10)))
  (:printer add-sub-shift-reg ((op #b10) (rn #b11111))
            '('neg :tab rd ", " rm shift)))

(def-add-sub subs #b11
  (:printer add-sub-imm ((op #b11)))
  (:printer add-sub-ext-reg ((op #b11)))
  (:printer add-sub-shift-reg ((op #b11)))
  (:printer add-sub-imm ((op #b11) (rd #b11111))
            '('cmp :tab rn ", " imm shift))
  (:printer add-sub-ext-reg ((op #b11) (rd #b11111))
            '('cmp :tab rn ", " rm extend))
  (:printer add-sub-shift-reg ((op #b11) (rd #b11111))
            '('cmp :tab rn ", " rm shift))
  (:printer add-sub-shift-reg ((op #b11) (rn #b11111) (rd #b11111))
            '('cmp :tab rn ", " rm shift))
  (:printer add-sub-shift-reg ((op #b11) (rn #b11111))
            '('negs :tab rd ", " rm shift)))

(define-instruction-macro cmp (rn rm)
  `(let ((rn ,rn)
         (rm ,rm))
     (inst subs (if (sc-is rn 32-bit-reg)
                    (32-bit-reg zr-tn)
                    zr-tn)
           rn rm)))

(define-instruction-macro cmn (rn rm)
  `(let ((rn ,rn)
         (rm ,rm))
     (inst adds (if (sc-is rn 32-bit-reg)
                    (32-bit-reg zr-tn)
                    zr-tn)
           rn rm)))

(define-instruction-macro neg (rd rm)
  `(let ((rd ,rd)
         (rm ,rm))
     (inst sub rd (if (sc-is rd 32-bit-reg)
                      (32-bit-reg zr-tn)
                      zr-tn)
           rm)))

(define-instruction-macro negs (rd rm)
  `(let ((rd ,rd)
         (rm ,rm))
     (inst subs rd (if (sc-is rd 32-bit-reg)
                       (32-bit-reg zr-tn)
                       zr-tn)
           rm)))

(define-instruction-macro add-sub (rd rm immediate)
  `(let ((rd ,rd)
         (rm ,rm)
         (imm ,immediate))
     (if (minusp imm)
         (inst sub rd rm (add-sub-immediate (- imm)))
         (inst add rd rm (add-sub-immediate imm)))))
;;;

(def-emitter add-sub-carry
  (size 1 31)
  (op 2 29)
  (#b11010000 8 21)
  (rm 5 16)
  (#b000000 6 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format
    (add-sub-carry 32 :include add-sub
                      :default-printer '(:name :tab rd ", " rn ", " rm))
  (op2 :field (byte 8 21) :value #b11010000)
  (rm :field (byte 5 16) :type 'reg)
  (op3 :field (byte 6 10) :value 0)
  (rn :type 'reg)
  (rd :type 'reg))

(defmacro def-add-sub-carry (name opc)
  `(define-instruction ,name (segment rd rn rm)
     (:printer add-sub-carry ((op ,opc)))
     (:emitter
      (emit-add-sub-carry segment
                          (reg-size rd)
                          ,opc
                          (gpr-offset rm) (gpr-offset rn) (gpr-offset rd)))))

(def-add-sub-carry adc #b00)
(def-add-sub-carry adcs #b01)
(def-add-sub-carry sbc #b10)
(def-add-sub-carry sbcs #b11)

;;;

(define-instruction-format (logical 32)
  (op :field (byte 2 29))
  (rn :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(def-emitter logical-reg
  (size 1 31)
  (opc 2 29)
  (#b01010 5 24)
  (shift 2 22)
  (n 1 21)
  (rm 5 16)
  (imm 6 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format
    (logical-reg 32
     :include logical
     :default-printer '(:name :tab rd ", " rn ", " rm shift))
  (op2 :field (byte 5 24) :value #b01010)
  (shift :fields (list (byte 2 22) (byte 6 10)) :type 'shift)
  (n :field (byte 1 21) :value 0)
  (rm :field (byte 5 16) :type 'reg))

(def-emitter logical-imm
  (size 1 31)
  (opc 2 29)
  (#b100100 6 23)
  (n 1 22)
  (imr 6 16)
  (ims 6 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format
    (logical-imm 32
     :include logical
     :default-printer '(:name :tab rd  ", " rn ", " imm))
  (op2 :field (byte 6 23) :value #b100100)
  (imm :fields (list (byte 1 22) (byte 6 16) (byte 6 10))
       :type 'logical-immediate)
  (rd :type 'reg-sp))

(defun sequence-of-ones-p (integer)
  (declare (type (unsigned-byte 64) integer))
  (and (plusp integer)
       (let ((ones (logior (1- integer) integer))) ;; turns zeros on the right into ones
         (not (logtest (1+ ones) ;; Turns #b111 into #b1000
                       ones))))) ;; And when ANDed will produce 0

(defun count-trailing-zeros (integer)
  (declare (type (unsigned-byte 64) integer))
  (loop for i below 64
        until (logbitp 0 integer)
        do (setf integer (ash integer -1))
        finally (return i)))

(defun find-pattern (integer &optional (width 64))
  (declare (type (unsigned-byte 64) integer)
           (type (member 32 64) width)
           (optimize speed))
  (loop with pattern = integer
        for size of-type (integer 0 32) = (truncate width 2) then (truncate size 2)
        for try-pattern of-type (unsigned-byte 32) = (ldb (byte size 0) integer)
        while (and (= try-pattern
                      (the (unsigned-byte 32) (ldb (byte size size) integer)))
                   (> size 1))
        do (setf pattern try-pattern)
        finally (return (values (* size 2) pattern))))

(defun fixnum-encode-logical-immediate (integer)
  (and (fixnump integer)
       (encode-logical-immediate (fixnumize integer))))

(defun encode-logical-immediate (integer &optional (width 64))
  (let ((integer (ldb (byte 64 0) integer)))
    (cond ((or (zerop integer)
               (= integer (ldb (byte 64 0) -1)))
           nil)
          (t
           (multiple-value-bind (size pattern) (find-pattern integer width)
             (values (ldb (byte 1 6) size) ;; 64-bit patterns need to set the N bit to 1
                     (cond ((sequence-of-ones-p pattern)
                            ;; Simple case of consecutive ones, just needs shifting right
                            (mod (- size (count-trailing-zeros pattern)) size))
                           ;; Invert the pattern and find consecutive ones there
                           ((not (sequence-of-ones-p (ldb (byte size 0)
                                                          (lognot pattern))))
                            (return-from encode-logical-immediate))
                           (t
                            ;; Rotate the bits on the left so that they are all consecutive
                            (- size (integer-length (ldb (byte size 0) (lognot pattern))))))
                     (logior (1- (logcount pattern))
                             ;; The size is calculated based on the highest set bit of IMMS inverted.
                             ;; Set unused bits to 1 so that the size can be calcuted correctly.
                             (ldb (byte 6 0) (ash -1 (integer-length size))))))))))

(defun rotate-byte (count size pos integer)
  ;; Taken from sb-rotate-byte
  (let ((count (nth-value 1 (round count size)))
        (mask (1- (ash 1 size))))
    (logior (logand integer (lognot (ash mask pos)))
            (let ((field (logand (ash mask pos) integer)))
              (logand (ash mask pos)
                      (if (> count 0)
                          (logior (ash field count)
                                  (ash field (- count size)))
                          (logior (ash field count)
                                  (ash field (+ count size)))))))))

(defun decode-logical-immediate (n immr imms)
  ;; DecodeBitMasks() From the ARM manual
  (declare (type bit n)
           (type (unsigned-byte 6) imms imms))
  (let* ((length (if (zerop n)
                     (1- (integer-length (ldb (byte 6 0) (lognot imms))))
                     6))
         (levels (ldb (byte length 0) -1))
         (s (logand imms levels))
         (r (logand immr levels))
         (bits (ldb (byte (1+ s) 0) -1))
         (pattern (rotate-byte (- r) (ash 1 length) 0 bits))
         (result 0))
    (declare (type (unsigned-byte 64) result))
    (loop for i below 64 by (1+ levels)
          do (setf (ldb (byte (1+ levels) i) result)
                   pattern))
    result))

(defun emit-logical-reg-inst (segment opc n rd rn rm)
  (let* ((shift 0)
         (amount 0))
    (when (shifter-operand-p rm)
      (setf shift (shifter-operand-function-code rm)
            amount (shifter-operand-operand rm)))
    (emit-logical-reg segment
                      (reg-size rd)
                      opc
                      shift n (gpr-offset
                               (if (shifter-operand-p rm)
                                   (shifter-operand-register rm)
                                   rm))
                      amount
                      (gpr-offset rn) (gpr-offset rd))))

(defmacro def-logical-imm-and-reg (name opc &rest printers)
  `(define-instruction ,name (segment rd rn rm)
     ,@printers
     (:emitter
      (if (or (register-p rm)
              (shifter-operand-p rm))
          (emit-logical-reg-inst segment ,opc 0 rd rn rm)
          (let ((size (reg-size rd)))
           (multiple-value-bind (n immr imms)
               (encode-logical-immediate rm (if (= size 1)
                                                64
                                                32))
             (unless n
               (error 'cannot-encode-immediate-operand :value rm))
             (emit-logical-imm segment size ,opc n immr imms (gpr-offset rn) (gpr-offset rd))))))))

(def-logical-imm-and-reg and #b00
  (:printer logical-imm ((op #b00) (n 0)))
  (:printer logical-reg ((op #b00) (n 0))))
(def-logical-imm-and-reg orr #b01
  (:printer logical-imm ((op #b01)))
  (:printer logical-reg ((op #b01)))
  (:printer logical-imm ((op #b01) (rn 31))
            '('mov :tab rd  ", " imm))
  (:printer logical-reg ((op #b01) (rn 31))
                        '('mov :tab rd ", " rm shift)))
(def-logical-imm-and-reg eor #b10
  (:printer logical-imm ((op #b10)))
  (:printer logical-reg ((op #b10))))
(def-logical-imm-and-reg ands #b11
  (:printer logical-imm ((op #b11)))
  (:printer logical-reg ((op #b11)))
  (:printer logical-imm ((op #b11) (rd 31))
            '('tst :tab rn  ", " imm))
  (:printer logical-reg ((op #b11) (rd 31))
            '('tst :tab rn ", " rm shift)))

(define-instruction-macro tst (rn rm)
  `(inst ands zr-tn ,rn ,rm))

(defmacro def-logical-reg (name opc &rest printers)
  `(define-instruction ,name (segment rd rn rm)
     ,@printers
     (:emitter
      (emit-logical-reg-inst segment ,opc 1 rd rn rm))))

(defun bic-mask (x)
  (ldb (byte 64 0) (lognot x)))

(def-logical-reg bic #b00
  (:printer logical-reg ((op #b00) (n 1))))
(def-logical-reg orn #b01
  (:printer logical-reg ((op #b01) (n 1)))
  (:printer logical-reg ((op #b01) (n 1) (rn 31))
            '('mvn :tab rd ", " rm shift)))
(def-logical-reg eon #b10
  (:printer logical-reg ((op #b10) (n 1))))
(def-logical-reg bics #b11
  (:printer logical-reg ((op #b11) (n 1))))

(define-instruction-macro mvn (rd rm)
  `(inst orn ,rd zr-tn ,rm))

;;;

(def-emitter bitfield
  (size 1 31)
  (opc 2 29)
  (#b100110 6 23)
  (n 1 22)
  (imr 6 16)
  (ims 6 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (bitfield 32
                            :default-printer
                            '(:name :tab rd  ", " rn ", " immr ", " imms))
  (op :field (byte 2 29))
  (op2 :field (byte 6 23) :value #b100110)
  (n :field (byte 1 22) :value +64-bit-size+)
  (immr :field (byte 6 16) :type 'unsigned-immediate)
  (imms :field (byte 6 10) :type 'unsigned-immediate)
  (rn :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg)
  (ubfm-alias :fields (list (byte 6 16) (byte 6 10))))


(define-instruction sbfm (segment rd rn immr imms)
  (:printer bitfield ((op 0)))
  (:printer bitfield ((op 0) (imms #b111111))
            '('asr :tab rd  ", " rn ", " immr))
  (:emitter
   (emit-bitfield segment +64-bit-size+ 0 +64-bit-size+
                  immr imms (gpr-offset rn) (gpr-offset rd))))

(define-instruction bfm (segment rd rn immr imms)
  (:printer bitfield ((op 1)))
  (:emitter
   (emit-bitfield segment +64-bit-size+ 1 +64-bit-size+
                  immr imms (gpr-offset rn) (gpr-offset rd))))

(define-instruction ubfm (segment rd rn immr imms)
  (:printer bitfield ((op #b10) (imms #b111111))
            '('lsr :tab rd  ", " rn ", " immr))
  (:printer bitfield ((op #b10))
            ;; This ought to have a better solution.
            ;; The whole disassembler ought to be better...
            '((:using #'print-ubfm-alias-name ubfm-alias)
              :tab rd  ", " rn ", "
              (:using #'print-ubfm-alias ubfm-alias)))
  (:emitter
   (when (and (fixup-p imms) (eq (fixup-flavor imms) :card-table-index-mask))
     (note-fixup segment :ubfm-imms imms)
     (setq imms 0))
   (emit-bitfield segment +64-bit-size+ #b10 +64-bit-size+
                  immr imms (gpr-offset rn) (gpr-offset rd))))

(define-instruction-macro asr (rd rn shift)
  `(let ((rd ,rd)
         (rn ,rn)
         (shift ,shift))
     (if (integerp shift)
         (inst sbfm rd rn shift 63)
         (inst asrv rd rn shift))))

(define-instruction-macro lsr (rd rn shift)
  `(let ((rd ,rd)
         (rn ,rn)
         (shift ,shift))
     (if (integerp shift)
         (inst ubfm rd rn shift 63)
         (inst lsrv rd rn shift))))

(define-instruction-macro lsl (rd rn shift)
  `(let ((rd ,rd)
         (rn ,rn)
         (shift ,shift))
     (if (integerp shift)
         (inst ubfm rd rn
               (mod (- shift) 64)
               (- 63 shift))
         (inst lslv rd rn shift))))

(define-instruction-macro ror (rd rs shift)
  `(let ((rd ,rd)
         (rs ,rs)
         (shift ,shift))
     (if (integerp shift)
         (inst extr rd rs rs shift)
         (inst rorv rd rs shift))))

(define-instruction-macro sxtw (rd rn)
  `(inst sbfm ,rd ,rn 0 31))
;;;

(def-emitter extract
  (size 1 31)
  (#b00100111 8 23)
  (n 1 22)
  (#b0 1 21)
  (rm 5 16)
  (imm 6 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (extract 32)
  (op2 :field (byte 8 23) :value #b00100111)
  (op3 :field (byte 1 21) :value #b0)
  (rm :field (byte 5 16) :type 'reg)
  (imm :field (byte 6 10) :type 'unsigned-immediate)
  (rn :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-instruction extr (segment rd rn rm lsb)
  (:printer extract ()
            '((:cond
                ((rn :same-as rm) 'ror)
                (t :name))
              :tab rd  ", " rn (:unless (:same-as rn) ", " rm) ", " imm))
  (:emitter
   (let ((size (reg-size rd)))
    (emit-extract segment size size
                  (gpr-offset rm)
                  lsb
                  (gpr-offset rn)
                  (gpr-offset rd)))))

;;;

(def-emitter move-wide
  (size 1 31)
  (opc 2 29)
  (#b100101 6 23)
  (hw 2 21)
  (imm 16 5)
  (rd 5 0))

(define-instruction-format (move-wide 32
                            :default-printer '(:name :tab rd  ", " imm shift))
  (op :field (byte 2 29))
  (op2 :field (byte 6 23) :value #b100101)
  (shift :field (byte 2 21) :type 'wide-shift)
  (imm :field (byte 16 5) :type 'unsigned-immediate)
  (rd :field (byte 5 0) :type 'reg))

(define-instruction-macro mov-sp (rd rm)
  `(inst add ,rd ,rm 0))

(define-instruction-macro mov (rd rm)
  `(let ((rd ,rd)
         (rm ,rm))
     (if (integerp rm)
         (sb-vm::load-immediate-word rd rm)
         (inst orr rd zr-tn rm))))

(define-instruction movn (segment rd imm &optional (shift 0))
  (:printer move-wide ((op #b00)))
  (:emitter
   (aver (not (ldb-test (byte 4 0) shift)))
   (emit-move-wide segment (reg-size rd) #b00 (/ shift 16) imm (gpr-offset rd))))

(define-instruction movz (segment rd imm &optional (shift 0))
  (:printer move-wide ((op #b10)))
  (:emitter
   (aver (not (ldb-test (byte 4 0) shift)))
   (emit-move-wide segment +64-bit-size+ #b10 (/ shift 16)
                   (cond ((and (fixup-p imm)
                               (eq (fixup-flavor imm) :symbol-tls-index))
                          (note-fixup segment :move-wide imm)
                          0)
                         (t
                          imm))
                   (gpr-offset rd))))

(define-instruction movk (segment rd imm &optional (shift 0))
  (:printer move-wide ((op #b11)))
  (:emitter
   (aver (not (ldb-test (byte 4 0) shift)))
   (emit-move-wide segment +64-bit-size+ #b11 (/ shift 16) imm (gpr-offset rd))))

;;;

(def-emitter cond-select
  (size 1 31)
  (op 1 30)
  (#b011010100 9 21)
  (rm 5 16)
  (cond 4 12)
  (op2 2 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format
    (cond-select 32
     :default-printer '(:name :tab rd  ", " rn ", " rm ", " cond))
  (op :field (byte 1 30))
  (op3 :field (byte 9 21) :value #b011010100)
  (rm :field (byte 5 16) :type 'reg)
  (cond :field (byte 4 12) :type 'cond)
  (op2 :field (byte 2 10))
  (rn :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(defmacro def-cond-select (name op op2 &rest printers)
  `(define-instruction ,name (segment rd rn rm cond)
     (:printer cond-select ((op ,op)
                            (op2 ,op2)))
     ,@printers
     (:emitter
      (emit-cond-select segment +64-bit-size+ ,op (gpr-offset rm) (conditional-opcode cond)
                        ,op2 (gpr-offset rn) (gpr-offset rd)))))

(def-cond-select csel 0 0)
(def-cond-select csinc 0 1
  (:printer cond-select ((op 0) (op2 1) (rn 31) (rm 31)
                         (cond nil :type 'negated-cond))
            '('cset :tab rd  ", " cond)))
(def-cond-select csinv 1 0
  (:printer cond-select ((op 1) (op2 0) (rn 31) (rm 31)
                         (cond nil :type 'negated-cond))
            '('csetm :tab rd  ", " cond)))
(def-cond-select csneg 1 1)

(define-instruction-macro cset (rd cond)
  `(inst csinc ,rd zr-tn zr-tn (negate-condition ,cond)))

(define-instruction-macro csetm (rd cond)
  `(inst csinv ,rd zr-tn zr-tn (negate-condition ,cond)))
;;;

(def-emitter cond-compare
  (size 1 31)
  (op 1 30)
  (#b111010010 9 21)
  (rm-imm 5 16)
  (cond 4 12)
  (imm-p 1 11)
  (#b0 1 10)
  (rn 5 5)
  (0 1 4)
  (nzcv 4 0))

(define-instruction-format
    (cond-compare 32
     :default-printer '(:name :tab rn ", " rm ", " nzcv ", " cond))
    (op :field (byte 1 30))
    (op2 :field (byte 9 21) :value #b111010010)
    (rm :field (byte 5 16) :type 'reg)
    (cond :field (byte 4 12) :type 'cond)
    (imm-p :field (byte 1 11))
    (op3 :field (byte 1 10) :value 0)
    (rn :field (byte 5 5) :type 'reg)
    (op4 :field (byte 1 4) :value 0)
    (nzcv :field (byte 4 0) :type 'unsigned-immediate))

(defmacro def-cond-compare (name op)
  `(define-instruction ,name (segment rn rm-imm cond &optional (nzcv 0))
     (:printer cond-compare ((op ,op) (imm-p 0)))
     (:printer cond-compare ((op ,op) (imm-p 1)
                             (rm nil :type 'unsigned-immediate)))
     (:emitter
      (emit-cond-compare segment +64-bit-size+ ,op
                         (if (integerp rm-imm)
                             rm-imm
                             (gpr-offset rm-imm))
                         (conditional-opcode cond)
                         (if (integerp rm-imm)
                             1
                             0)
                         (gpr-offset rn) nzcv))))

(def-cond-compare ccmn #b0)
(def-cond-compare ccmp #b1)

;;;

(def-emitter data-processing-1
  (size 1 31)
  (#b101101011000000000 18 13)
  (opcode 3 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (data-processing-1 32
                            :default-printer '(:name :tab rd  ", " rn))
  (size :field (byte 1 31))
  (op2 :field (byte 18 13) :value #b101101011000000000)
  (op :field (byte 3 10))
  (rn :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(defmacro def-data-processing-1 (name opc &optional 32-bit-opcode)
  `(define-instruction ,name (segment rd rn)
     (:printer data-processing-1 ((op ,opc)))
     (:emitter
      (emit-data-processing-1 segment
                              (reg-size rd)
                              ,(if 32-bit-opcode
                                   `(sc-case rd
                                     (32-bit-reg
                                      ,32-bit-opcode)
                                      (t
                                       ,opc))
                                   opc)
                              (gpr-offset rn)
                              (gpr-offset rd)))))

(def-data-processing-1 rbit #b000)
(def-data-processing-1 rev16 #b001)
(def-data-processing-1 clz #b100)
(def-data-processing-1 cls #b101)

(define-instruction rev32 (segment rd rn)
  (:printer data-processing-1 ((size 1) (op #b10)))
  (:emitter
   (emit-data-processing-1 segment
                           (reg-size rd)
                           #b10
                           (gpr-offset rn)
                           (gpr-offset rd))))

(define-instruction rev (segment rd rn)
  (:printer data-processing-1 ((size #b1) (op #b11)))
  (:printer data-processing-1 ((size #b0) (op #b10)))
  (:emitter
   (emit-data-processing-1 segment (reg-size rd)
                           (sc-case rd
                             (32-bit-reg #b10)
                             (t #b11))
                           (gpr-offset rn)
                           (gpr-offset rd))))

;;;

(def-emitter data-processing-2
  (size 1 31)
  (#b0011010110 10 21)
  (rm 5 16)
  (opcode 6 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (data-processing-2 32
                            :default-printer '(:name :tab rd  ", " rn ", " rm))
  (op2 :field (byte 10 21) :value #b0011010110)
  (rm :field (byte 5 16) :type 'reg)
  (op :field (byte 6 10))
  (rn :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))


(defmacro def-data-processing-2 (name opc &optional alias)
  `(define-instruction ,name (segment rd rn rm)
     (:printer data-processing-2 ((op ,opc))
               ,@(and alias
                      `('(',alias :tab rd ", " rn ", " rm))))
     (:emitter
      (emit-data-processing-2 segment (reg-size rd)
                              (gpr-offset rm)
                              ,opc (gpr-offset rn) (gpr-offset rd)))))

(def-data-processing-2 asrv #b001010 asr)
(def-data-processing-2 lslv #b001000 lsl)
(def-data-processing-2 lsrv #b001001 lsr)
(def-data-processing-2 rorv #b001011 ror)


(def-data-processing-2 udiv #b00010)
(def-data-processing-2 sdiv #b00011)

;;;

(def-emitter data-processing-3
  (size 1 31)
  (#b0011011 7 24)
  (op31 3 21)
  (rm 5 16)
  (o0 1 15)
  (ra 5 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (data-processing-3 32
                            :default-printer
                            '(:name :tab rd  ", " rn ", " rm ", " ra))
  (op2 :field (byte 7 24) :value #b0011011)
  (op31 :field (byte 3 21))
  (rm :field (byte 5 16) :type 'reg)
  (o0 :field (byte 1 15))
  (ra :field (byte 5 10) :type 'reg)
  (rn :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(defmacro def-data-processing-3 (name op31 o0 &rest printers)
  `(define-instruction ,name (segment rd rn rm ra)
     (:printer data-processing-3 ((op31 ,op31) (o0 ,o0)))
     ,@printers
     (:emitter
      (emit-data-processing-3 segment
                              (reg-size rd)
                              ,op31
                              (gpr-offset rm)
                              ,o0 (gpr-offset ra) (gpr-offset rn) (gpr-offset rd)))))

(def-data-processing-3 madd #b000 0
  (:printer data-processing-3 ((op31 #b000) (o0 0) (ra 31))
            '('mul :tab rd  ", " rn ", " rm )))

(def-data-processing-3 smaddl #b001 0
  (:printer data-processing-3 ((op31 #b001) (o0 0) (ra 31))
            '('smull :tab rd  ", " rn ", " rm )))
(def-data-processing-3 umaddl #b101 0
  (:printer data-processing-3 ((op31 #b101) (o0 0) (ra 31))
            '('umull :tab rd  ", " rn ", " rm )))

(def-data-processing-3 msub #b000 1)
(def-data-processing-3 smsubl #b001 1)
(def-data-processing-3 umsubl #b101 1)

(define-instruction-macro mul (rd rn rm)
  `(inst madd ,rd ,rn ,rm zr-tn))

(define-instruction smulh (segment rd rn rm)
  (:printer data-processing-3 ((op31 #b010) (o0 0) (ra 31))
            '(:name :tab rd  ", " rn ", " rm))
  (:emitter
   (emit-data-processing-3 segment (reg-size rd) #b010 (gpr-offset rm)
                           0 31 (gpr-offset rn) (gpr-offset rd))))

(define-instruction umulh (segment rd rn rm)
  (:printer data-processing-3 ((op31 #b110) (o0 0) (ra 31))
            '(:name :tab rd  ", " rn ", " rm))
  (:emitter
   (emit-data-processing-3 segment (reg-size rd) #b110 (gpr-offset rm)
                           0 31 (gpr-offset rn) (gpr-offset rd))))
;;;

(define-instruction-format (ldr-str 32)
  (size :field (byte 2 30))
  (op2 :field (byte 3 27) :value #b111)
  (v :field (byte 1 26))
  (op3 :field (byte 2 24) :value #b00)
  (op :field (byte 2 22))
  (rn :field (byte 5 5) :type 'x-reg-sp)
  (rt :fields (list (byte 2 30) (byte 1 23) (byte 5 0)) :type 'reg-float-reg)
  (ldr-str-annotation :type 'ldr-str-annotation))

(def-emitter ldr-str-unsigned-imm
  (size 2 30)
  (#b111 3 27)
  (v 1 26)
  (#b01 2 24)
  (opc 2 22)
  (imm 12 10)
  (rn 5 5)
  (rt 5 0))

(define-instruction-format
    (ldr-str-unsigned-imm 32
     :default-printer '(:name :tab rt  ", [" rn (:unless (just-imm :constant 0) ", " imm) "]"
                        ldr-str-annotation)
     :include ldr-str)
    (op3 :value #b01)
    (just-imm :field (byte 12 10))
    (imm :fields (list (byte 2 30) (byte 1 23) (byte 12 10) (byte 1 26))
         :type 'scaled-immediate)
    (ldr-str-annotation :fields (list (byte 2 30) (byte 1 23) (byte 12 10) (byte 1 26))))

(def-emitter ldr-str-unscaled-imm
  (size 2 30)
  (#b111 3 27)
  (v 1 26)
  (#b00 2 24)
  (opc 2 22)
  (#b0 1 21)
  (imm 9 12)
  (mode 2 10)
  (rn 5 5)
  (rt 5 0))

(define-instruction-format
    (ldr-str-unscaled-imm 32
     :default-printer '(:name :tab rt  ", [" rn imm-writeback ldr-str-annotation)
     :include ldr-str)
  (op4 :field (byte 1 21) :value #b0)
  (imm-writeback :fields (list (byte 9 12) (byte 2 10)) :type 'imm-writeback)
  (ldr-str-annotation :field (byte 9 12)))

(def-emitter ldr-str-reg
  (size 2 30)
  (#b111 3 27)
  (v 1 26)
  (#b00 2 24)
  (opc 2 22)
  (#b1 1 21)
  (rm 5 16)
  (option 3 13)
  (s 1 12)
  (#b10 2 10)
  (rn 5 5)
  (rt 5 0))

(define-instruction-format
    (ldr-str-reg 32
     :default-printer '(:name :tab rt  ", [" rn ", " rm option "]" ldr-str-annotation)
     :include ldr-str)
  (op4 :field (byte 1 21) :value 1)
  (rm :field (byte 5 16) :type 'x-reg)
  (option :fields (list (byte 3 13) (byte 1 12)) :type 'ldr-str-extend)
  (op5 :field (byte 2 10) :value #b10)
  (ldr-str-annotation :field (byte 5 16) :type 'ldr-str-reg-annotation))

(def-emitter ldr-literal
  (opc 2 30)
  (#b011 3 27)
  (v 1 26)
  (#b00 2 24)
  (imm 19 5)
  (rt 5 0))

(define-instruction-format (ldr-literal 32
                            :default-printer '(:name :tab rt ", " label literal-annotation)
                            :include ldr-str)
  (op2 :value #b011)
  (label :field (byte 19 5) :type 'label)
  (rt :fields (list (byte 2 30) (byte 5 0)))
  (literal-annotation :field (byte 19 5) :type 'ldr-literal-annotation))

(defun ldr-str-offset-encodable (offset &optional (size 64))
  (or (typep offset '(signed-byte 9))
      (multiple-value-bind (qout rem) (truncate offset (truncate size 8))
        (and (zerop rem)
             (typep qout '(unsigned-byte 12))))))

(defun emit-load-store (size opc segment dst address)
  (let* ((base (memory-operand-base address))
         (offset (memory-operand-offset address))
         (mode (memory-operand-mode address))
         (index-encoding (or (position mode '(:offset :post-index 0 :pre-index))
                             (error "Bad addressing mode")))
         (fp (fp-register-p dst))
         (v  (if fp
                 1
                 0))
         (size (cond (fp
                      (sc-case dst
                        (complex-double-reg
                         (setf opc (logior #b10 opc))
                         #b00)
                        (t
                         (logior #b10
                                 (fp-reg-type dst)))))
                     (size)
                     ((sc-is dst 32-bit-reg)
                      #b10)
                     (t #b11)))
         (scale (if fp
                    (logior (ash (ldb (byte 1 1) opc) 2)
                            size)
                    size))
         (dst (reg-offset dst)))
    (cond ((and (or
                 (and (fixup-p offset)
                      (eq (fixup-flavor offset) :symbol-tls-index))
                 (and (typep offset 'unsigned-byte)
                      (not (ldb-test (byte scale 0) offset))
                      (typep (ash offset (- scale)) '(unsigned-byte 12))))
                (register-p base)
                (eq mode :offset))
           (emit-ldr-str-unsigned-imm segment size
                                      v opc
                                      (cond ((fixup-p offset)
                                             (note-fixup segment :ldr-str offset)
                                             0)
                                            (t
                                             (ash offset (- scale))))
                                      (gpr-offset base)
                                      dst))
          ((and (eq mode :offset)
                (or (register-p offset)
                    (extend-p offset)))
           (multiple-value-bind
                 (register shift extend)
               (if (extend-p offset)
                   (values (extend-register offset)
                           (if (> (extend-operand offset) 0)
                               1
                               0)
                           (ecase (extend-kind offset)
                             (:uxtw #b010)
                             (:lsl #b011)
                             (:sxtw #b110)
                             (:sxtx #b111)))
                   (values offset 0 #b011))
             (emit-ldr-str-reg segment size
                               v opc
                               (reg-offset register)
                               extend shift
                               (gpr-offset base)
                               dst)))
          ((and (typep offset '(signed-byte 9))
                (register-p base))
           (emit-ldr-str-unscaled-imm segment size v
                                      opc offset
                                      index-encoding
                                      (gpr-offset base) dst))
          (t
           (error "Invalid STR/LDR arguments: ~s ~s" dst address)))))

(defmacro def-load-store (name size opc &rest printers)
  `(define-instruction ,name (segment dst address)
     (:printer ldr-str-unsigned-imm ((size ,size) (op ,opc) (v 0)))
     (:printer ldr-str-reg ((size ,size) (op ,opc) (v 0)))
     (:printer ldr-str-unscaled-imm ((size ,size) (op ,opc) (v 0)))
     ,@printers
     (:emitter
      (emit-load-store ,size ,opc segment dst address))))

(def-load-store strb 0 #b00)
(def-load-store ldrb 0 #b01)
(def-load-store ldrsb 0 #b10)
(def-load-store strh 1 #b00)
(def-load-store ldrh 1 #b01)
(def-load-store ldrsh 1 #b10)
(def-load-store ldrsw #b10 #b10)

(def-load-store str nil #b00
  (:printer ldr-str-unsigned-imm ((op 0)))
  (:printer ldr-str-reg ((op 0)))
  (:printer ldr-str-unscaled-imm ((op 0)))
  ;; 128-bit stores
  (:printer ldr-str-unsigned-imm ((size #b00) (op #b10) (v 1)))
  (:printer ldr-str-reg ((size #b00) (op #b10) (v 1)))
  (:printer ldr-str-unscaled-imm ((size #b00) (op #b10) (v 1))))

(define-instruction ldr (segment dst address)
  (:printer ldr-str-unsigned-imm ((op #b01)))
  (:printer ldr-str-reg ((op #b01)))
  (:printer ldr-str-unscaled-imm ((op #b01)))
  (:printer ldr-literal ())
  ;; 128-bit loads
  (:printer ldr-str-unsigned-imm ((op #b11)))
  (:printer ldr-str-reg ((op #b11)))
  (:printer ldr-str-unscaled-imm ((op #b11)))
  (:emitter
   (typecase address
     (fixup
      (note-fixup segment :pc-relative-ldr-str address)
      (emit-pc-relative segment 1 0 0 (gpr-offset dst))  ; ADRP
      (assemble (segment)
        (inst ldr dst (@ dst))))
     (label
      (emit-back-patch segment 4
                       (lambda (segment posn)
                         (emit-ldr-literal segment
                                           #b01
                                           (if (fp-register-p dst)
                                               1
                                               0)
                                           (ash (- (label-position address) posn) -2)
                                           (reg-offset dst)))))
     (t
      (emit-load-store nil 1 segment dst address)))))

(def-emitter ldr-str-pair
  (opc 2 30)
  (#b101 3 27)
  (v 1 26)
  (#b0 1 25)
  (op2 2 23)
  (l 1 22)
  (imm 7 15)
  (rt2 5 10)
  (rn 5 5)
  (rt 5 0))

(define-instruction-format
    (ldr-str-pair 32
     :default-printer '(:name :tab rt ", " rt2 ", [" rn pair-imm-writeback ldr-str-annotation)
     :include ldr-str)
  (size :field (byte 2 30))
  (op2 :value #b101)
  (v :field (byte 1 26))
  (op3 :field (byte 1 25) :value #b00)
  (l :field (byte 1 22))
  (pair-imm-writeback :fields (list (byte 2 23) (byte 2 30) (byte 7 15) (byte 1 26))
                      :type 'pair-imm-writeback)
  (rt2 :fields (list (byte 2 30) (byte 5 10)) :type 'reg-float-reg)
  (rt :fields (list (byte 2 30) (byte 5 0)))
  (ldr-str-annotation :fields (list (byte 5 5) (byte 7 15)) :type 'ldr-str-pair-annotation))

(defun ldp-stp-offset-p (offset size)
  (multiple-value-bind (quot rem) (truncate offset (ecase size
                                                     (32 4)
                                                     (64 8)
                                                     (128 16)))
    (and (zerop rem)
         (typep quot '(signed-byte 7)))))

(defun emit-ldr-str-pair-inst (l segment rt1 rt2 address)
  (let* ((base (memory-operand-base address))
         (offset (memory-operand-offset address))
         (mode (memory-operand-mode address))
         (fp (cond ((and (fp-register-p rt1)
                         (fp-register-p rt2))
                    (assert (and (eq (tn-sc rt1)
                                     (tn-sc rt2)))
                            (rt1 rt2)
                            "Arguments should have the same FP storage class: ~s ~s."
                            rt1 rt2)
                    t)
                   ((or (fp-register-p rt1)
                        (fp-register-p rt2))
                    (error "Both registers must have the same storage class: ~s ~s."
                           rt1 rt2))))
         (v  (if fp
                 1
                 0))
         (size 3)
         (opc (cond ((not fp)
                     #b10)
                     (t
                      (fp-reg-type rt1)))))
    (when fp
      (setf size (+ opc 2)))
    (aver (not (ldb-test (byte size 0) offset)))
    (emit-ldr-str-pair segment opc v
                       (ecase mode
                         (:post-index #b01)
                         (:pre-index #b11)
                         (:offset #b10))
                       l
                       (ash offset (- size))
                       (reg-offset rt2) (reg-offset base) (reg-offset rt1))))

(define-instruction stp (segment rt1 rt2 address)
  (:printer ldr-str-pair ((l 0)))
  (:emitter
   (emit-ldr-str-pair-inst 0 segment rt1 rt2 address)))

(define-instruction ldp (segment rt1 rt2 address)
  (:printer ldr-str-pair ((l 1)))
  (:emitter
   (emit-ldr-str-pair-inst 1 segment rt1 rt2 address)))

;;;

(def-emitter ldr-str-exclusive
  (size 2 30)
  (#b001000 6 24)
  (o2 1 23)
  (l 1 22)
  (o1 1 21)
  (rs 5 16)
  (o0 1 15)
  (rt2 5 10)
  (rn 5 5)
  (rt 5 0))

(define-instruction-format (ldr-str-exclusive 32)
  (size :field (byte 2 30))
  (op2 :field (byte 6 24) :value #b001000)
  (o2 :field (byte 1 23))
  (l :field (byte 1 22))
  (o1 :field (byte 1 21))
  (rs :field (byte 5 16) :type 'w-reg)
  (o0 :field (byte 1 15))
  (rt2 :field (byte 5 10) :type 'reg)
  (rn :field (byte 5 5) :type 'x-reg-sp)
  (rt :field (byte 5 0) :type 'reg))

(defmacro def-store-exclusive (name o0 o1 o2 rs)
  `(define-instruction ,name (segment ,@(and rs '(rs)) rt rn)
     (:printer ldr-str-exclusive ((o0 ,o0) (o1 ,o1) (o2 ,o2) (l 0))
               '(:name :tab ,@(and rs '(rs ", ")) rt ", [" rn "]"))
     (:emitter
      (emit-ldr-str-exclusive segment (logior #b10 (reg-size rt))
                              ,o2 0 ,o1
                              ,(if rs
                                   '(gpr-offset rs)
                                   31)
                              ,o0
                              31
                              (gpr-offset rn)
                              (gpr-offset rt)))))

(def-store-exclusive stxr 0 0 0 t)
(def-store-exclusive stlxr 1 0 0 t)
(def-store-exclusive stlr 1 0 1 nil)

(defmacro def-load-exclusive (name o0 o1 o2)
  `(define-instruction ,name (segment rt rn)
     (:printer ldr-str-exclusive ((o0 ,o0) (o1 ,o1) (o2 ,o2) (l 1))
               '(:name :tab rt ", [" rn "]"))
     (:emitter
      (emit-ldr-str-exclusive segment (logior #b10 (reg-size rt))
                              ,o2 1 ,o1
                              31
                              ,o0
                              31
                              (gpr-offset rn)
                              (gpr-offset rt)))))

(def-load-exclusive ldxr 0 0 0)
(def-load-exclusive ldaxr 1 0 0)
(def-load-exclusive ldar 1 0 1)

(define-instruction-format (cas 32)
  (size1 :field (byte 1 31) :value 1)
  (size :field (byte 1 30))
  (op2 :field (byte 6 24) :value #b001000)
  (o2 :field (byte 1 23) :value 1)
  (l :field (byte 1 22))
  (o1 :field (byte 1 21) :value 1)
  (rs :fields (list (byte 1 30) (byte 5 16)) :type 'sized-reg)
  (o0 :field (byte 1 15))
  (rt2 :field (byte 5 10) :value #b11111)
  (rn :field (byte 5 5) :type 'x-reg-sp)
  (rt :fields (list (byte 1 30) (byte 5 0)) :type 'sized-reg))

(define-instruction-format (casb 32)
  (size :field (byte 2 30))
  (op2 :field (byte 6 24) :value #b001000)
  (o2 :field (byte 1 23) :value 1)
  (l :field (byte 1 22))
  (o1 :field (byte 1 21) :value 1)
  (rs :field (byte 5 16) :type 'w-reg)
  (o0 :field (byte 1 15))
  (rt2 :field (byte 5 10) :value #b11111)
  (rn :field (byte 5 5) :type 'x-reg-sp)
  (rt :field (byte 5 0) :type 'w-reg))

(defmacro def-cas (name o0 l)
  `(define-instruction ,name (segment rs rt rn)
     (:printer cas ((o0 ,o0) (l ,l))
               '(:name :tab rs ", " rt ", [" rn "]"))
     (:emitter
      (emit-ldr-str-exclusive segment
                              (logior #b10 (reg-size rt))
                              1
                              ,l
                              1
                              (gpr-offset rs)
                              ,o0
                              31
                              (gpr-offset rn)
                              (gpr-offset rt)))))

(def-cas cas 0 0)
(def-cas casa 0 1)
(def-cas casal 1 1)
(def-cas casl 1 0)

(defmacro def-casb (name size o0 l)
  `(define-instruction ,name (segment rs rt rn)
     (:printer casb ((size ,size) (o0 ,o0) (l ,l))
               '(:name :tab rs ", " rt ", [" rn "]"))
     (:emitter
      (emit-ldr-str-exclusive segment
                              ,size
                              1
                              ,l
                              1
                              (gpr-offset rs)
                              ,o0
                              31
                              (gpr-offset rn)
                              (gpr-offset rt)))))
(def-casb casb 0 0 0)
(def-casb casab 0 0 1)
(def-casb casalb 0 1 1)
(def-casb caslb 0 1 0)

(def-casb cash 1 0 0)
(def-casb casah 1 0 1)
(def-casb casalh 1 1 1)
(def-casb caslh 1 1 0)

;;;

(def-emitter ldatomic
  (size 2 30)
  (#b111000 6 24)
  (a 1 23)
  (r 1 22)
  (#b1 1 21)
  (rs 5 16)
  (opc 6 10)
  (rn 5 5)
  (rt 5 0))

(define-instruction-format (ldatomic 32)
  (size1 :field (byte 1 31) :value #b1)
  (size :field (byte 1 30))
  (op2 :field (byte 6 24) :value #b111000)
  (a :field (byte 1 23))
  (r :field (byte 1 22))
  (o1 :field (byte 1 21) :value #b1)
  (rs :fields (list (byte 1 30) (byte 5 16)) :type 'sized-reg)
  (opc :field (byte 6 10))
  (rn :field (byte 5 5) :type 'x-reg-sp)
  (rt :fields (list (byte 1 30) (byte 5 0)) :type 'sized-reg))

(defmacro def-ldatomic (name opc a r)
  `(define-instruction ,name (segment rs rt rn)
     (:printer ldatomic ((a ,a) (r ,r) (opc ,opc))
               '(:name :tab rs ", " rt ", [" rn "]"))
     (:emitter
      (emit-ldatomic segment
                     (logior #b10 (reg-size rt))
                     ,a
                     ,r
                     (gpr-offset rs)
                     ,opc
                     (gpr-offset rn)
                     (gpr-offset rt)))))

(def-ldatomic ldadd 0 0 0)
(def-ldatomic ldadda 0 1 0)
(def-ldatomic ldaddal 0 1 1)

(def-ldatomic ldeor #b001000 0 0)
(def-ldatomic ldeora #b001000 1 0)
(def-ldatomic ldeoral #b001000 1 1)

(def-ldatomic ldset #b001100 0 0)
(def-ldatomic ldseta #b001100 1 0)
(def-ldatomic ldsetal #b001100 1 1)

(define-instruction-format (ldaddb 32)
  (size :field (byte 2 30))
  (op2 :field (byte 6 24) :value #b111000)
  (a :field (byte 1 23))
  (r :field (byte 1 22))
  (o1 :field (byte 1 21) :value #b1)
  (rs :field (byte 5 16) :type 'w-reg)
  (opc :field (byte 6 10) :value #b0)
  (rn :field (byte 5 5) :type 'x-reg-sp)
  (rt :field (byte 5 0) :type 'w-reg))

(defmacro def-ldaddb (name size a r)
  `(define-instruction ,name (segment rs rt rn)
     (:printer ldaddb ((size ,size) (a ,a) (r ,r))
               '(:name :tab rs ", " rt ", [" rn "]"))
     (:emitter
      (emit-ldatomic segment
                     ,size
                     ,a
                     ,r
                     0
                     (gpr-offset rs)
                     (gpr-offset rn)
                     (gpr-offset rt)))))

(def-ldaddb ldaddb 0 0 0)
(def-ldaddb ldaddab 0 1 0)
(def-ldaddb ldaddalb 0 1 1)
(def-ldaddb ldaddh 1 0 0)
(def-ldaddb ldaddah 1 1 0)
(def-ldaddb ldaddalh 1 1 1)

;;;

(def-emitter cond-branch
    (#b01010100 8 24)
  (imm 19 5)
  (#b0 1 4)
  (cond 4 0))

(define-instruction-format (cond-branch 32
                            :default-printer '(:name cond :tab target))
  (op1 :field (byte 8 24) :value #b01010100)
  (target :field (byte 19 5) :type 'label)
  (op2 :field (byte 1 4) :value #b0)
  (cond :field (byte 4 0) :type 'cond))

(def-emitter uncond-branch
  (op 1 31)
  (#b00101 5 26)
  (imm 26 0))

(define-instruction-format (uncond-branch 32
                            :default-printer '(:name :tab target))
  (op :field (byte 1 31))
  (op2 :field (byte 5 26) :value #b00101)
  (target :field (byte 26 0) :type 'label))

(define-instruction b (segment cond-or-label &optional label)
  (:printer cond-branch ())
  (:printer uncond-branch ((op 0)))
  (:emitter
   (cond ((and (fixup-p cond-or-label)
               (not label))
          (note-fixup segment :uncond-branch cond-or-label)
          (emit-uncond-branch segment 0 0))
         ((fixup-p label)
          (note-fixup segment :cond-branch cond-or-label)
          (emit-cond-branch segment 0 (conditional-opcode cond-or-label)))
         (t
          (emit-back-patch segment 4
                           (cond (label
                                  (aver (label-p label))
                                  (lambda (segment posn)
                                    (emit-cond-branch segment
                                                      (ash (- (label-position label) posn) -2)
                                                      (conditional-opcode cond-or-label))))
                                 (t
                                  (aver (label-p cond-or-label))
                                  (lambda (segment posn)
                                    (emit-uncond-branch segment
                                                        0
                                                        (ash (- (label-position cond-or-label) posn) -2))))))))))

(define-instruction bl (segment label)
  (:printer uncond-branch ((op 1)))
  (:emitter
   (etypecase label
     (fixup
      (note-fixup segment :uncond-branch label)
      (emit-uncond-branch segment 1 0))
     (label
      (emit-back-patch segment 4
                       (lambda (segment posn)
                         (emit-uncond-branch segment
                                             1
                                             (ash (- (label-position label) posn) -2))))))))

(def-emitter uncond-branch-reg
  (#b1101011 7 25)
  (opc 4 21)
  (#b11111000000 11 10)
  (rn 5 5)
  (#b00000 5 0))

(define-instruction-format (uncond-branch-reg 32
                            :default-printer '(:name :tab rn))
  (op2 :field (byte 7 25) :value #b1101011)
  (op :field (byte 4 21))
  (op3 :field (byte 11 10) :value #b11111000000)
  (rn :field (byte 5 5) :type 'reg-sp)
  (op4 :field (byte 5 0) :value #b00000))

(define-instruction br (segment register)
  (:printer uncond-branch-reg ((op 0)))
  (:emitter
   (emit-uncond-branch-reg segment 0 (gpr-offset register))))

(define-instruction blr (segment register)
  (:printer uncond-branch-reg ((op 1)))
  (:emitter
   (emit-uncond-branch-reg segment 1 (gpr-offset register))))

(define-instruction ret (segment &optional (register sb-vm::lr-tn))
  (:printer uncond-branch-reg ((op #b10)))
  (:printer uncond-branch-reg ((op #b10) (rn sb-vm::lr-offset))
            '(:name))
  (:emitter
   (emit-uncond-branch-reg segment #b10 (gpr-offset register))))

;;;

(def-emitter compare-branch-imm
  (size 1 31)
  (#b011010 6 25)
  (op 1 24)
  (imm 19 5)
  (rt 5 0))

(define-instruction-format (compare-branch-imm 32
                            :default-printer '(:name :tab rt ", " label))
  (size :field (byte 1 31))
  (op1 :field (byte 6 25) :value #b011010)
  (op  :field (byte 1 24))
  (label :field (byte 19 5) :type 'label)
  (rt :field (byte 5 0) :type 'reg))

(macrolet ((def (name op)
             `(define-instruction ,name (segment rt label)
                (:printer compare-branch-imm ((op ,op)))
                (:emitter
                 (cond ((fixup-p label)
                        (note-fixup segment :cond-branch label)
                        (emit-compare-branch-imm segment
                                                 +64-bit-size+
                                                 ,op
                                                 0
                                                 (gpr-offset rt)))
                       (t
                        (emit-back-patch segment 4
                                         (lambda (segment posn)
                                           (emit-compare-branch-imm segment
                                                                    +64-bit-size+
                                                                    ,op
                                                                    (ash (- (label-position label) posn) -2)
                                                                    (gpr-offset rt))))))))))
  (def cbz 0)
  (def cbnz 1))

(def-emitter test-branch-imm
  (b5 1 31)
  (#b011011 6 25)
  (op 1 24)
  (b40 5 19)
  (label 14 5)
  (rt 5 0))

(define-instruction-format (test-branch-imm 32
                            :default-printer '(:name :tab rt ", " index ", " label))
  (op1 :field (byte 6 25) :value #b011011)
  (op  :field (byte 1 24))
  (index :fields (list (byte 1 31) (byte 5 19)) :type 'test-branch-immediate)
  (label :field (byte 14 5) :type 'label)
  (rt :field (byte 5 0) :type 'reg))

(define-instruction tbz (segment rt bit label)
  (:printer test-branch-imm ((op 0)))
  (:emitter
   (aver (label-p label))
   (check-type bit (integer 0 63))
   (emit-back-patch segment 4
                    (lambda (segment posn)
                      (emit-test-branch-imm segment
                                            (ldb (byte 1 5) bit)
                                            0
                                            (ldb (byte 5 0) bit)
                                            (the (signed-byte 14) (ash (- (label-position label) posn) -2))
                                            (gpr-offset rt))))))

(define-instruction tbnz (segment rt bit label)
  (:printer test-branch-imm ((op 1)))
  (:emitter
   (aver (label-p label))
   (check-type bit (integer 0 63))
   (emit-back-patch segment 4
                    (lambda (segment posn)
                      (emit-test-branch-imm segment
                                            (ldb (byte 1 5) bit)
                                            1
                                            (ldb (byte 5 0) bit)
                                            (the (signed-byte 14) (ash (- (label-position label) posn) -2))
                                            (gpr-offset rt))))))

(define-instruction tbnz* (segment rt bit label)
  (:emitter
   (aver (label-p label))
   (check-type bit (integer 0 63))
   (labels ((compute-delta (position &optional magic-value)
              (ash (- (label-position label
                                      (when magic-value position)
                                      magic-value)
                      position)
                   -2))
            (multi-instruction-emitter (segment position)
              (declare (ignore position))
              (assemble (segment)
                (inst tst rt (ash 1 bit))
                (inst b :ne label)))
            (one-instruction-emitter (segment posn)
              (emit-test-branch-imm segment
                                    (ldb (byte 1 5) bit)
                                    1
                                    (ldb (byte 5 0) bit)
                                    (compute-delta posn)
                                    (gpr-offset rt)))
            (multi-instruction-maybe-shrink (segment chooser posn magic-value)
              (declare (ignore chooser))
              (let ((delta (compute-delta posn magic-value)))
                (when (typep delta '(signed-byte 14))
                  (emit-back-patch segment 4
                                   #'one-instruction-emitter)
                  t))))
     (emit-chooser
      segment 8 2
      #'multi-instruction-maybe-shrink
      #'multi-instruction-emitter))))

(define-instruction tbz* (segment rt bit label)
  (:emitter
   (aver (label-p label))
   (check-type bit (integer 0 63))
   (labels ((compute-delta (position &optional magic-value)
              (ash (- (label-position label
                                      (when magic-value position)
                                      magic-value)
                      position)
                   -2))
            (multi-instruction-emitter (segment position)
              (declare (ignore position))
              (assemble (segment)
                (inst tst rt (ash 1 bit))
                (inst b :eq label)))
            (one-instruction-emitter (segment posn)
              (emit-test-branch-imm segment
                                    (ldb (byte 1 5) bit)
                                    0
                                    (ldb (byte 5 0) bit)
                                    (compute-delta posn)
                                    (gpr-offset rt)))
            (multi-instruction-maybe-shrink (segment chooser posn magic-value)
              (declare (ignore chooser))
              (let ((delta (compute-delta posn magic-value)))
                (when (typep delta '(signed-byte 14))
                  (emit-back-patch segment 4
                                   #'one-instruction-emitter)
                  t))))
     (emit-chooser
      segment 8 2
      #'multi-instruction-maybe-shrink
      #'multi-instruction-emitter))))


;;;
(def-emitter exception
    (#b11010100 8 24)
  (opc 3 21)
  (imm 16 5)
  (#b000 3 2)
  (ll 2 0))

(define-instruction-format (exception 32 :default-printer '(:name :tab imm))
  (op2 :field (byte 8 24) :value #b11010100)
  (op  :field (byte 3 21))
  (imm :field (byte 16 5) :type 'unsigned-immediate)
  (ll :field (byte 2 0)))

(defmacro def-exception (name opc ll &rest printer-options)
  `(define-instruction ,name (segment imm)
     (:printer exception ((op ,opc) (ll ,ll))
               ,@printer-options)
     (:emitter
      (emit-exception segment ,opc imm ,ll))))

(def-exception brk #b001 #b00
  '(:name :tab imm) :control #'brk-control)

(def-exception hlt #b010 #b00)

;;;

(def-emitter pc-relative
  (op 1 31)
  (immlo 2 29)
  (#b10000 5 24)
  (immhi 19 5)
  (rd 5 0))

(define-instruction-format (pc-relative 32
                            :default-printer '(:name :tab rd ", " label))
  (op :field (byte 1 31))
  (op2 :field (byte 5 24) :value #b10000)
  (label :fields (list (byte 2 29) (byte 19 5)) :type 'label)
  (rd :field (byte 5 0) :type 'x-reg))

(defun emit-pc-relative-inst (op segment rd label &optional (offset 0))
  (aver (label-p label))
  (aver (register-p rd))
  (emit-back-patch segment 4
                   (lambda (segment posn)
                     (let ((offset (+ (- (label-position label) posn)
                                      offset)))
                       (emit-pc-relative segment
                                         op
                                         (ldb (byte 2 0) offset)
                                         (ldb (byte 19 2) offset)
                                         (gpr-offset rd))))))

(define-instruction adr (segment rd label &optional (offset 0))
  (:printer pc-relative ((op 0)))
  (:emitter
   (cond ((fixup-p label)
          (aver (zerop offset))
          (note-fixup segment :pc-relative label)
          (emit-pc-relative segment 1 0 0 (gpr-offset rd)) ; ADRP
          (assemble (segment)
            (inst add rd rd 0)))
         (t
          (emit-pc-relative-inst 0 segment rd label offset)))))

(define-instruction adrp (segment rd label)
  (:printer pc-relative ((op 1)))
  (:emitter
   (emit-pc-relative-inst 1 segment rd label)))

;;;

(def-emitter system-reg
  (#b1101010100 10 22)
  (l 1 21)
  (sys-reg 16 5)
  (rt 5 0))

(define-instruction-format (sys-reg 32)
  (op :field (byte 10 22) :value #b1101010100)
  (l :field (byte 1 21))
  (sys-reg :field (byte 16 5) :type 'sys-reg)
  (rt :field (byte 5 0) :type 'x-reg))

(defun decode-sys-reg (reg)
  (ecase reg
    (#b1101101000010000 :nzcv)
    (#b1101101000100000 :fpcr)
    (#b1101101000100001 :fpsr)
    (#b1101110011101000 :ccnt)
    (#b1101111010000010 :tpidr_el0)
    (#b1101111010000011 :tpidrro_el0)
    (#b1101100000000111 :dczid_el0)))

(defun encode-sys-reg (reg)
  (ecase reg
    (:nzcv #b1101101000010000)
    (:fpcr #b1101101000100000)
    (:fpsr #b1101101000100001)
    (:ccnt #b1101110011101000)
    (:tpidr_el0 #b1101111010000010)
    (:tpidrro_el0 #b1101111010000011)
    (:dczid_el0 #b1101100000000111)))

(define-instruction msr (segment sys-reg rt)
  (:printer sys-reg ((l 0)) '(:name :tab sys-reg ", " rt))
  (:emitter
   (emit-system-reg segment 0 (encode-sys-reg sys-reg) (gpr-offset rt))))

(define-instruction mrs (segment rt sys-reg)
  (:printer sys-reg ((l 1)) '(:name :tab rt ", " sys-reg))
  (:emitter
   (emit-system-reg segment 1 (encode-sys-reg sys-reg) (gpr-offset rt))))

;;;

(def-emitter system
  (#b11010101000000110011 20 12)
  (crm 4 8)
  (op 3 5)
  (#b11111 5 0))

(define-instruction-format (system 32)
  (op1 :field (byte 20 12) :value #b11010101000000110011)
  (crm :field (byte 4 8))
  (op :field (byte 3 5))
  (op2 :field (byte 5 0) :value #b11111))


(define-instruction clrex (segment &optional (imm 15))
  (:printer system ((op #b010))
            '(:name (:unless (crm :constant 15) :tab "#" crm)))
  (:emitter
   (emit-system segment imm  #b010)))

(defglobal **mem-bar-kinds**
    '((:sy . #b1111)
      (:st . #b1110)
      (:ld . #b1101)
      (:ish . #b1011)
      (:ishst . #b1010)
      (:ishld . #b1001)
      (:nsh . #b0111)
      (:nsht . #b0110)
      (:osh . #b0011)
      (:oshst . #b0010)
      (:oshld . #b0001)))

(defmacro def-mem-bar (name op)
  `(define-instruction ,name (segment &optional (kind :sy))
     (:printer system ((op ,op))
               '(:name :tab (:using #'print-mem-bar-kind crm)))
     (:emitter
      (emit-system segment
                   (cond ((integerp kind)
                          kind)
                         ((cdr (assoc kind **mem-bar-kinds**)))
                         (t
                          (error "Unknown memory barrier kind: ~s" kind)))
                   ,op))))

(def-mem-bar dsb #b100)
(def-mem-bar dmb #b101)
(def-mem-bar isb #b110)

;;;

(def-emitter hint
  (#b110101010000001100100000 24 8)
  (imm 3 5)
  (#b11111 5 0))

(define-instruction-format (hint 32 :default-printer '(:name))
  (op1 :field (byte 24 8) :value #b110101010000001100100000)
  (imm :field (byte 3 5))
  (op2 :field (byte 5 0) :value #b11111))

(define-instruction nop (segment)
  (:printer hint ((imm 0)))
  (:emitter
   (emit-hint segment 0)))



;;; Floating point

(defun fp-reg-type (reg)
  (ecase (sc-name (tn-sc reg))
    (single-reg
     0)
    ((double-reg complex-single-reg)
     1)
    (complex-double-reg
     #b10)))

(def-emitter fp-compare
  (#b00011110 8 24)
  (type 2 22)
  (#b1 1 21)
  (rm 5 16)
  (#b001000 6 10)
  (rn 5 5)
  (e 1 4)
  (z 1 3)
  (#b000 3 0))

(define-instruction-format (fp-compare 32
                            :default-printer '(:name :tab rn ", " rm))
  (op1 :field (byte 9 23) :value #b000111100)
  (type :field (byte 1 22))
  (rm :field (byte 5 16) :type 'float-reg)
  (op2 :field (byte 6 10) :value #b001000)
  (rn :field (byte 5 5) :type 'float-reg)
  (op :field (byte 1 4))
  (z :field (byte 1 3))
  (op3 :field (byte 3 0) :value #b0))

(defmacro def-fp-compare (name op)
  `(define-instruction ,name (segment rn rm)
     (:printer fp-compare ((op ,op)))
     (:printer fp-compare ((op ,op) (z 1) (type 0))
               '(:name :tab rn ", " 0f0))
     (:printer fp-compare ((op ,op) (z 1) (type 1))
               '(:name :tab rn ", " 0d0))
     (:emitter
      (assert (or (eql rm 0)
                  (eq (tn-sc rn)
                      (tn-sc rm)))
              (rn rm)
              "Arguments should have the same FP storage class: ~s ~s" rn rm)
      (emit-fp-compare segment
                       (fp-reg-type rn)
                       (if (eql rm 0)
                           0
                           (fpr-offset rm))
                       (fpr-offset rn)
                       ,op
                       (if (eql rm 0)
                           1
                           0)))))

(def-fp-compare fcmp #b0)
(def-fp-compare fcmpe #b1)

(define-instruction-format (fp-data-processing 32)
  (rn :field (byte 5 5) :type 'float-reg)
  (rd :field (byte 5 0) :type 'float-reg))

(def-emitter fp-data-processing-1
  (#b000111100 9 23)
  (type 1 22)
  (#b100 3 19)
  (opcode 4 15)
  (#b10000 5 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format
    (fp-data-processing-1 32
     :include fp-data-processing
     :default-printer '(:name :tab rd ", " rn))
  (op2 :field (byte 9 23) :value #b000111100)
  (op3 :field (byte 3 19) :value #b100)
  (op :field (byte 4 15))
  (:op4 :field (byte 5 10) :value #b10000))

(def-emitter fp-data-processing-2
  (#b000111100 9 23)
  (type 1 22)
  (#b1 1 21)
  (rm 5 16)
  (opcode 4 12)
  (#b10 2 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format
    (fp-data-processing-2 32
     :include fp-data-processing
     :default-printer '(:name :tab rd ", " rn ", " rm))
  (op2 :field (byte 9 23) :value #b000111100)
  (op3 :field (byte 1 21) :value #b1)
  (rm :field (byte 5 16) :type 'float-reg)
  (op :field (byte 4 12))
  (:op4 :field (byte 2 10) :value #b10))

(def-emitter fp-data-processing-3
  (#b000111110 9 23)
  (type 1 22)
  (o1 1 21)
  (rm 5 16)
  (o2 1 15)
  (ra 5 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format
    (fp-data-processing-3 32
     :include fp-data-processing
     :default-printer '(:name :tab rd ", " rn ", " rm ", " ra))
  (op4 :field (byte 9 23) :value #b000111110)
  (op1 :field (byte 1 21))
  (op2 :field (byte 1 15))
  (rm :field (byte 5 16) :type 'float-reg)
  (ra :field (byte 5 10) :type 'float-reg))

(def-emitter fp-conversion
  (size 1 31)
  (#b00111100 8 23)
  (type 1 22)
  (#b1 1 21)
  (opcode 5 16)
  (#b00000 6 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (fp-conversion 32
                            :include fp-data-processing
                            :default-printer '(:name :tab rd ", " rn))
  (op2 :field (byte 8 23) :value #b00111100)
  (type :field (byte 1 22))
  (op1 :field (byte 1 21) :value #b1)
  (op :field (byte 5 16))
  (op3 :field (byte 6 10) :value #b0))

(defmacro def-fp-data-processing-1 (name op)
  `(define-instruction ,name (segment rd rn)
     (:printer fp-data-processing-1 ((op ,op)))
     (:emitter
      (assert (and (eq (tn-sc rd)
                       (tn-sc rn)))
              (rd rn)
              "Arguments should have the same FP storage class: ~s ~s." rd rn)
      (emit-fp-data-processing-1 segment
                                 (fp-reg-type rn)
                                 ,op
                                 (fpr-offset rn)
                                 (fpr-offset rd)))))

(def-fp-data-processing-1 fabs #b0001)
(def-fp-data-processing-1 fneg #b0010)
(def-fp-data-processing-1 fsqrt #b0011)
(def-fp-data-processing-1 frintn #b1000)
(def-fp-data-processing-1 frintp #b1001)
(def-fp-data-processing-1 frintm #b1010)
(def-fp-data-processing-1 frintz #b1011)
(def-fp-data-processing-1 frinta #b1100)
(def-fp-data-processing-1 frintx #b1110)
(def-fp-data-processing-1 frinti #b1111)

(define-instruction-format (fcvt 32
                            :include fp-data-processing-1
                            :default-printer '(:name :tab rd ", " rn))
  (op :field (byte 2 17) :value #b01)
  (rn :fields (list (byte 1 22) (byte 5 5)))
  (rd :fields (list (byte 2 15) (byte 5 0))))

(define-instruction fcvt (segment rd rn)
  (:printer fcvt ())
  (:emitter
   (emit-fp-data-processing-1 segment
                              (fp-reg-type rn)
                              (logior #b100 (fp-reg-type rd))
                              (fpr-offset rn)
                              (fpr-offset rd))))

(defmacro def-fp-data-processing-2 (name op)
  `(define-instruction ,name (segment rd rn rm)
     (:printer fp-data-processing-2 ((op ,op)))
     (:emitter
      (assert (and (eq (tn-sc rd)
                       (tn-sc rn))
                   (eq (tn-sc rd)
                       (tn-sc rm)))
              (rd rn rm)
              "Arguments should have the same FP storage class: ~s ~s ~s." rd rn rm)
      (emit-fp-data-processing-2 segment
                                 (fp-reg-type rn)
                                 (fpr-offset rm)
                                 ,op
                                 (fpr-offset rn)
                                 (fpr-offset rd)))))

(def-fp-data-processing-2 fmul #b0000)
(def-fp-data-processing-2 fdiv #b0001)
(def-fp-data-processing-2 fadd #b0010)
(def-fp-data-processing-2 fsub #b0011)
(def-fp-data-processing-2 fmax #b0100)
(def-fp-data-processing-2 fmin #b0101)
(def-fp-data-processing-2 fmaxnm #b0110)
(def-fp-data-processing-2 fminnm #b0111)
(def-fp-data-processing-2 fnmul #b1000)

(defmacro def-fp-data-processing-3 (name o1 o2)
  `(define-instruction ,name (segment rd rn rm ra)
     (:printer fp-data-processing-3 ((op1 ,o1) (op2 ,o2)))
     (:emitter
      (assert (and (eq (tn-sc rd)
                       (tn-sc rn))
                   (eq (tn-sc rd)
                       (tn-sc rm))
                   (eq (tn-sc rd)
                       (tn-sc ra)))
              (rd rn rm ra)
              "Arguments should have the same FP storage class: ~s ~s ~s ~s." rd rn rm ra)
      (emit-fp-data-processing-3 segment
                                 (fp-reg-type rn)
                                 ,o1
                                 (fpr-offset rm)
                                 ,o2
                                 (fpr-offset ra)
                                 (fpr-offset rn)
                                 (fpr-offset rd)))))

(def-fp-data-processing-3 fmadd 0 0)
(def-fp-data-processing-3 fmsub 0 1)
(def-fp-data-processing-3 fnmadd 1 0)
(def-fp-data-processing-3 fnmsub 1 1)

;;;

(defmacro def-fp-conversion (name op &optional from-int)
  `(define-instruction ,name (segment rd rn)
     (:printer fp-conversion ((op ,op) (,(if from-int
                                               'rn
                                               'rd)
                                          nil :type 'reg)))
     (:emitter
      ,@(if from-int
            `((assert (fp-register-p rd)
                      (rd)
                      "Destination ~d should be an FP register." rd)
              (assert (register-p rn)
                      (rn)
                      "Source ~d should be an integer register." rn))
            `((assert (register-p rd)
                      (rd)
                      "Destination ~d should be an integer register." rn)
              (assert (fp-register-p rn)
                      (rn)
                      "Source ~d should be an FP register." rn)))
      (emit-fp-conversion segment
                         +64-bit-size+
                         (fp-reg-type ,(if from-int
                                           'rd
                                           'rn))
                         ,op
                         (reg-offset rn)
                         (reg-offset rd)))))

(def-fp-conversion fcvtns #b00000)
(def-fp-conversion fcvtnu #b00001)
(def-fp-conversion scvtf #b00010 t)
(def-fp-conversion ucvtf #b00011 t)
(def-fp-conversion fcvtas #b00100)
(def-fp-conversion fcvtau #b00101)
(def-fp-conversion fcvtps #b01000)
(def-fp-conversion fcvtpu #b01001)
(def-fp-conversion fcvtms #b10000)
(def-fp-conversion fcvtmu #b10001)
(def-fp-conversion fcvtzs #b11000)
(def-fp-conversion fcvtzu #b11001)

(defun encode-fp-immediate (float)
  (typecase float
    (double-float
     (let* ((bits (double-float-bits float))
            (sign (ldb (byte 1 63) bits))
            (exp (ldb (byte 11 52) bits))
            (frac (ldb (byte 52 0) bits)))
       (when (and (zerop (ldb (byte 48 0) frac))
                  (=
                   (ldb (byte 8 2) exp)
                   (if (zerop (ldb (byte 1 10) exp))
                       (ldb (byte 8 0) -1)
                       0)))
         (logior (ash sign 7)
                 (ash (logandc1 (ldb (byte 1 10) exp) 1) 6)
                 (ash (ldb (byte 2 0) exp) 4)
                 (ldb (byte 4 48) frac)))))
    (single-float
     (let* ((bits (single-float-bits float))
            (sign (ldb (byte 1 31) bits))
            (exp (ldb (byte 8 23) bits))
            (frac (ldb (byte 23 0) bits)))
       (when (and (zerop (ldb (byte 19 0) frac))
                  (=  (ldb (byte 5 2) exp)
                      (if (zerop (ldb (byte 1 7) exp))
                          (ldb (byte 5 0) -1)
                          0)))
         (logior (ash sign 7)
                 (ash (logandc1 (ldb (byte 1 7) exp) 1) 6)
                 (ash (ldb (byte 2 0) exp) 4)
                 (ldb (byte 4 19) frac)))))))

(def-emitter fp-immediate
  (m 1 31)
  (#b0 1 30)
  (s 1 29)
  (#b11110 5 24)
  (type 2 22)
  (#b1 1 21)
  (imm8 8 13)
  (#b100 3 10)
  (imm5 5 5)
  (rd 5 0))

(define-instruction-format (fp-immediate 32
                            :default-printer '(:name :tab rd ", " imm))
  (#b0 :field (byte 1 31))
  (#b0 :field (byte 1 30))
  (#b0 :field (byte 1 29))
  (#b11110 :field (byte 5 24))
  (#b1 :field (byte 1 21))
  (imm :fields (list (byte 2 22) (byte 8 13)) :type 'fp-imm)
  (#b100 :field (byte 3 10))
  (#b0 :field (byte 5 5))
  (rd :field (byte 5 0) :type 'float-reg))

(define-instruction fmov (segment rd rn)
  (:printer fp-conversion ((op #b110) (rd nil :type 'reg)))
  (:printer fp-conversion ((op #b111) (rn nil :type 'reg)))
  (:printer fp-data-processing-1 ((op #b0)))
  (:printer fp-immediate ())
  (:emitter
   (cond ((double-float-p rn)
          (aver (sc-is rd double-reg))
          (emit-fp-immediate segment 0 0 #b01
                             (encode-fp-immediate rn)
                             0
                             (fpr-offset rd)))
         ((single-float-p rn)
          (aver (sc-is rd single-reg))
          (emit-fp-immediate segment 0 0 #b00
                             (encode-fp-immediate rn)
                             0
                             (fpr-offset rd)))
         ((or (sc-is rd complex-double-reg complex-single-reg)
              (sc-is rn complex-double-reg complex-single-reg))
          (break "Implement"))
         ((and (fp-register-p rd)
               (fp-register-p rn))
          (assert (and (eq (tn-sc rd) (tn-sc rn))) (rd rn)
                  "Arguments should have the same fp storage class: ~s ~s."
                  rd rn)
          (emit-fp-data-processing-1 segment (fp-reg-type rn) 0
                                     (fpr-offset rn) (fpr-offset rd)))
         ((and (register-p rd)
               (fp-register-p rn))
          (let* ((type (fp-reg-type rn))
                 (128-p (= type #b10)))
            (emit-fp-conversion segment (if 128-p
                                            1
                                            type)
                                type
                                (if 128-p
                                    #b01111
                                    #b110)
                                (fpr-offset rn) (gpr-offset rd))))
         ((and (register-p rn)
               (fp-register-p rd))
          (let* ((type (fp-reg-type rd))
                 (128-p (= type #b10)))
            (emit-fp-conversion segment (if 128-p
                                            1
                                            type)
                                type
                                (if 128-p
                                    #b01111
                                    #b111)
                                (gpr-offset rn) (fpr-offset rd)))))))

(define-instruction load-from-label (segment dest label &optional lip)
  (:vop-var vop)
  (:emitter
   (labels ((compute-delta (position &optional magic-value)
              (- (label-position label
                                 (when magic-value position)
                                 magic-value)
                 position))
            (multi-instruction-emitter (segment position)
              (let* ((delta (compute-delta position))
                     (negative (minusp delta))
                     (low (ldb (byte 19 0) delta))
                     (high (ldb (byte 16 19) delta)))
               ;; ADR
               (emit-pc-relative segment 0
                                 (ldb (byte 2 0) low)
                                 (ldb (byte 19 2) low)
                                 (gpr-offset lip))
               (assemble (segment vop)
                 (inst movz dest high 16)
                 (inst ldr dest (@ lip (extend dest (if negative
                                                        :sxtw
                                                        :lsl)
                                               3))))))
            (one-instruction-emitter (segment position)
              (emit-ldr-literal segment
                                (sc-case dest
                                  ((32-bit-reg single-reg) #b00)
                                  (complex-double-reg #b10)
                                  (t #b01))
                                (if (fp-register-p dest)
                                    1
                                    0)
                                (ldb (byte 19 0)
                                     (ash (compute-delta position) -2))
                                (reg-offset dest)))
            (multi-instruction-maybe-shrink (segment chooser posn magic-value)
              (declare (ignore chooser))
              (let ((delta (compute-delta posn magic-value)))
                (when (typep delta '(signed-byte 21))
                  (emit-back-patch segment 4
                                   #'one-instruction-emitter)
                  t))))
     (if lip
         (emit-chooser
          segment 12 2
          #'multi-instruction-maybe-shrink
          #'multi-instruction-emitter)
         (emit-back-patch segment 4 #'one-instruction-emitter)))))

(define-instruction load-constant (segment dest index &optional lip)
  (:vop-var vop)
  (:emitter
   (labels ((compute-delta (position &optional magic-value)
              (+ (- (label-position (segment-origin segment)
                                    (when magic-value position)
                                    magic-value)
                    (component-header-length)
                    position)
                 index))
            (multi-instruction-emitter (segment position)
              (let* ((delta (compute-delta position))
                     (negative (minusp delta))
                     (low (ldb (byte 19 0) delta))
                     (high (ldb (byte 16 19) delta)))
                ;; ADR
                (emit-pc-relative segment 0
                                  (ldb (byte 2 0) low)
                                  (ldb (byte 19 2) low)
                                  (gpr-offset lip))
                (assemble (segment vop)
                  (inst movz dest high 16)
                  (inst ldr dest (@ lip (extend dest (if negative
                                                         :sxtw
                                                         :lsl)
                                                3))))))
            (one-instruction-emitter (segment position)
              (emit-ldr-literal segment
                                #b01
                                0
                                (ldb (byte 19 0)
                                     (ash (compute-delta position) -2))
                                (reg-offset dest)))
            (multi-instruction-maybe-shrink (segment chooser posn magic-value)
              (declare (ignore chooser))
              (let ((delta (compute-delta posn magic-value)))
                (when (typep delta '(signed-byte 21))
                  (emit-back-patch segment 4
                                   #'one-instruction-emitter)
                  t))))
     (if lip
         (emit-chooser
          segment 12 2
          #'multi-instruction-maybe-shrink
          #'multi-instruction-emitter)
         (emit-back-patch segment 4 #'one-instruction-emitter)))))

;;; SIMD
(def-emitter simd-three-diff
    (#b0 1 31)
  (q 1 30)
  (u 1 29)
  (#b01110 5 24)
  (size 2 22)
  (#b1 1 21)
  (rm 5 16)
  (opc 4 12)
  (0 2 10)
  (rn 5 5)
  (rd 5 0))

(def-emitter simd-three-same
  (#b0 1 31)
  (q 1 30)
  (u 1 29)
  (#b01110 5 24)
  (size 2 22)
  (#b1 1 21)
  (rm 5 16)
  (opc 5 11)
  (#b1 1 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (simd-three-same 32
                            :default-printer '(:name :tab rd ", " rn ", " rm))
  (op3 :field (byte 1 31) :value #b0)
  (u :field (byte 1 29))
  (op4 :field (byte 5 24) :value #b01110)
  (size :field (byte 2 22))
  (op5 :field (byte 1 21) :value #b1)
  (rm :fields (list (byte 1 30) (byte 5 16)) :type 'simd-reg)
  (op :field (byte 5 11))
  (op6 :field (byte 1 10) :value #b1)
  (rn :fields (list (byte 1 30) (byte 5 5)) :type 'simd-reg)
  (rd :fields (list (byte 1 30) (byte 5 0)) :type 'simd-reg))

(define-instruction-format (simd-three-same-sized 32
                            :include simd-three-same
                            :default-printer '(:name :tab rd ", " rn ", " rm))
  (rm :fields (list (byte 1 30) (byte 2 22) (byte 5 16)) :type 'simd-reg)
  (rn :fields (list (byte 1 30) (byte 2 22) (byte 5 5)) :type 'simd-reg)
  (rd :fields (list (byte 1 30) (byte 2 22) (byte 5 0)) :type 'simd-reg))

(defun encode-vector-size (size)
  (ecase size
    (:8b (values 0 0))
    (:16b (values 1 0))
    (:4h (values 0 #b01))
    (:8h (values 1 #b01))
    (:2s (values 0 #b10))
    (:4s (values 1 #b10))
    (:2d (values 1 #b11))))

(define-instruction-macro s-mov (rd rn &optional (size :16b))
  `(let ((rd ,rd)
         (rn ,rn)
         (size ,size))
     (inst s-orr rd rn rn size)))

(macrolet ((def (name u size op &rest printer)
             `(define-instruction ,name (segment rd rn rm &optional (size :16b))
                (:printer simd-three-same ((u ,u) (size ,size) (op ,op))
                          ,@printer)
                (:emitter
                 (emit-simd-three-same segment
                                       (encode-vector-size size)
                                       ,u
                                       ,size
                                       (fpr-offset rm)
                                       ,op
                                       (fpr-offset rn)
                                       (fpr-offset rd))))))
  (def s-orr #b0 #b10 #b00011
    '((:cond
        ((rn :same-as rm) 'mov)
        (t 'orr))
      :tab rd  ", " rn (:unless (:same-as rn) ", " rm)))
  (def s-and #b0 #b00 #b00011)
  (def s-eor #b1 #b00 #b00011))

(macrolet ((def (name u op)
             `(define-instruction ,name (segment rd rn rm &optional (size :16b))
                (:printer simd-three-same-sized ((u ,u) (op ,op)))
                (:emitter
                 (multiple-value-bind (q size) (encode-vector-size size)
                   (emit-simd-three-same segment
                                         q
                                         ,u
                                         size
                                         (fpr-offset rm)
                                         ,op
                                         (fpr-offset rn)
                                         (fpr-offset rd)))))))
  (def s-sub #b1 #b10000)
  (def cmeq #b1 #b10001)
  (def cmgt #b0 #b00110)
  (def cmge #b0 #b00111)
  (def cmhi #b1 #b00110)
  (def cmhs #b1 #b00111))

(def-emitter simd-scalar-three-same
    (#b01 2 30)
  (u 1 29)
  (#b11110 5 24)
  (size 2 22)
  (#b1 1 21)
  (rm 5 16)
  (opc 5 11)
  (#b1 1 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (simd-scalar-three-same 32
                            :default-printer '(:name :tab rd ", " rn ", " rm))
  (op3 :field (byte 2 30) :value #b1)
  (u :field (byte 1 29))
  (op4 :field (byte 5 24) :value #b11110)
  (size :field (byte 2 22))
  (op5 :field (byte 1 21) :value #b1)
  (rm :fields (list (byte 1 30) (byte 5 16)) :type 'simd-reg)
  (op :field (byte 5 11))
  (op6 :field (byte 1 10) :value #b1)
  (rn :fields (list (byte 1 30) (byte 5 5)) :type 'simd-reg)
  (rd :fields (list (byte 1 30) (byte 5 0)) :type 'simd-reg))

;;;

(def-emitter simd-extract
  (#b0 1 31)
  (q 1 30)
  (#b101110000 9 21)
  (rm 5 16)
  (#b0 1 15)
  (imm4 4 11)
  (#b0 1 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (simd-extract 32
                            :default-printer '(:name :tab rd ", " rn ", " rm ", #" imm4))
  (op3 :field (byte 1 31) :value #b0)
  (op4 :field (byte 9 21) :value #b101110000)
  (rm :fields (list (byte 1 30) (byte 5 16)) :type 'simd-reg)
  (imm4 :field (byte 4 11))
  (op6 :field (byte 1 15) :value #b0)
  (rn :fields (list (byte 1 30) (byte 5 5)) :type 'simd-reg)
  (rd :fields (list (byte 1 30) (byte 5 0)) :type 'simd-reg))

(define-instruction ext (segment rd rn rm index &optional (size :16b))
  (:printer simd-extract ())
  (:emitter
   (emit-simd-extract segment
                      (encode-vector-size size)
                      (fpr-offset rm)
                      index
                      (fpr-offset rn)
                      (fpr-offset rd))))

;;;

(def-emitter simd-copy
  (#b0 1 31)
  (q 1 30)
  (op 1 29)
  (#b01110000 8 21)
  (imm5 5 16)
  (#b0 1 15)
  (imm4 4 11)
  (#b1 1 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (simd-copy 32
                            :default-printer '(:name :tab rd ", " rn))
  (op3 :field (byte 1 31) :value #b0)
  (q :field (byte 1 30))
  (op :field (byte 1 29))
  (op4 :field (byte 8 21) :value #b01110000)
  (imm5 :field (byte 5 16))
  (op5 :field (byte 1 15) :value #b0)
  (imm4 :field (byte 4 11))
  (op6 :field (byte 1 10) :value #b1)
  (rn :fields (list (byte 5 5) (byte 5 16) (byte 4 11)) :type 'simd-copy-reg)
  (rd :fields (list (byte 5 0) (byte 5 16)) :type 'simd-copy-reg))

(define-instruction ins (segment rd index1 rn index2 size)
  (:printer simd-copy ((q 1) (op 1)))
  (:emitter
   (let ((size (position size '(:B :H :S :D))))
     (emit-simd-copy segment
                     1
                     1
                     (logior (ash index1 (1+ size))
                             (ash 1 size))
                     (ash index2 size)
                     (fpr-offset rn)
                     (fpr-offset rd)))))

(define-instruction-format (simd-copy-to-general 32
                            :include simd-copy
                            :default-printer '(:name :tab rd ", " rn))
  (rn :fields (list (byte 5 5) (byte 5 16)) :type 'simd-copy-reg)
  (rd :fields (list (byte 1 30) (byte 5 0)) :type 'sized-reg))

(define-instruction umov (segment rd rn index size)
  (:printer simd-copy-to-general ((op 0) (imm4 #b0111)))
  (:emitter
   (let ((size (position size '(:B :H :S :D))))
     (emit-simd-copy segment
                     (case size
                       (:d 1)
                       (t 0))
                     0
                     (logior (ash index (1+ size))
                             (ash 1 size))
                     #b0111
                     (fpr-offset rn)
                     (gpr-offset rd)))))

(define-instruction dup (segment rd rn size)
  (:printer simd-copy-to-general
            ((op 0) (imm4 1)
                    (rn nil :fields (list (byte 5 5) (byte 1 30) (byte 5 16))
                            :type 'simd-dup-reg))
            '(:name :tab rn ", " rd))
  (:emitter
   (multiple-value-bind (q imm)
       (ecase size
         (:8b (values 0 #b1))
         (:16b (values 1 #b1))
         (:4h (values 0 #b10))
         (:8h (values 1 #b10))
         (:2s (values 0 #b100))
         (:4s (values 1 #b100))
         (:2d (values 1 #b1000)))
     (emit-simd-copy segment
                     q
                     0
                     imm
                     1
                     (gpr-offset rn)
                     (fpr-offset rd)))))

(def-emitter simd-across-lanes
    (#b0 1 31)
  (q 1 30)
  (u 1 29)
  (#b01110 5 24)
  (size 2 22)
  (#b11000 5 17)
  (op 5 12)
  (#b10 2 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (simd-across-lanes 32
                            :default-printer '(:name :tab rd ", " rn))
  (o1 :field (byte 1 31) :value #b0)
  (q :field (byte 1 30))
  (u :field (byte 1 29))
  (op2 :field (byte 5 24) :value #b01110)
  (size :field (byte 2 22))
  (op3 :field (byte 5 17) :value #b11000)
  (op :field (byte 5 12))
  (op4 :field (byte 2 10) :value #b10)
  (rn :fields (list (byte 1 30) (byte 2 22) (byte 5 5)) :type 'vx.t)
  (rd :fields (list (byte 2 22) (byte 5 0)) :type 'vbhs))


(def-emitter simd-two-misc
    (#b0 1 31)
  (q 1 30)
  (u 1 29)
  (#b01110 5 24)
  (size 2 22)
  (#b10000 5 17)
  (op 5 12)
  (#b10 2 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction addv (segment rd rn size)
  (:printer simd-across-lanes  ((u 0) (op #b11011)))
  (:emitter
   (multiple-value-bind (size q)
       (ecase size
         (:8b (values 0 0))
         (:16b (values 0 1))
         (:4h (values 1 0))
         (:8h (values 1 1))
         (:4s (values 2 1)))
     (emit-simd-across-lanes
      segment
      q
      0
      size
      #b11011
      (fpr-offset rn)
      (fpr-offset rd)))))

(define-instruction uaddlv (segment rd sized rn sizen)
  (:printer simd-across-lanes  ((u 1) (op #b00011)
                                      (rd nil :type 'vhsd)))
  (:emitter
   (emit-simd-across-lanes
    segment
    (ecase sizen
      (:8b 0)
      (:16b 1)
      (:4h 0)
      (:8h 1)
      (:4s 1))
    1
    (ecase sized
      (:h 0)
      (:s 1)
      (:d 2))
    #b00011
    (fpr-offset rn)
    (fpr-offset rd))))

(macrolet ((def (name u op)
             `(define-instruction ,name (segment rd rn size)
                (:printer simd-across-lanes  ((u ,u) (op ,op)
                                              (rd nil :type 'vhsd)))
                (:emitter
                 (multiple-value-bind (q size) (encode-vector-size size)
                   (emit-simd-across-lanes
                    segment
                    q
                    ,u
                    size
                    ,op
                    (fpr-offset rn)
                    (fpr-offset rd)))))))
  (def uminv 1 #b11010)
  (def umaxv 1 #b01010))

(define-instruction-format (simd-two-misc 32
                            :default-printer '(:name :tab rd ", " rn))
  (o1 :field (byte 1 31) :value #b0)
  (q :field (byte 1 30))
  (u :field (byte 1 29))
  (op2 :field (byte 5 24) :value #b01110)
  (size :field (byte 2 22))
  (op3 :field (byte 5 17) :value #b10000)
  (op :field (byte 4 12))
  (op4 :field (byte 2 10) :value #b10)
  (rn :fields (list (byte 1 30) (byte 2 22) (byte 5 5)) :type 'simd-reg)
  (rd :fields (list (byte 1 30) (byte 2 22) (byte 5 0)) :type 'simd-reg))

(macrolet
    ((def (name u op &optional (sizes '(:8b :16b)))
       `(define-instruction ,name (segment rd rn &optional (size :16b))
          (:printer simd-two-misc ((u ,u) (op ,op)))
          (:emitter
           (check-type size (member ,@sizes))
           (multiple-value-bind (q size) (encode-vector-size size)
             (emit-simd-two-misc segment
                                 q
                                 ,u
                                 size
                                 ,op
                                 (fpr-offset rn)
                                 (fpr-offset rd)))))))
  (def cnt #b0 #b00101)
  (def s-rev16 #b0 #b00001)
  (def s-rev32 #b1 #b00000 (:8b :16b :4h :8h))
  (def rev64 #b0 #b00000 (:8b :16b :4h :8h :2s :4s))
  (def not #b1 #b00101))

(def-emitter simd-shift-by-imm
  (#b0 1 31)
  (q 1 30)
  (u 1 29)
  (#b011110 6 23)
  (immh 4 19)
  (immb 3 16)
  (op 5 11)
  (#b1 1 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (simd-shift-by-imm 32
                            :default-printer '(:name :tab rd ", " rn ", " shift))
  (o1 :field (byte 1 31) :value #b0)
  (q :field (byte 1 30))
  (u :field (byte 1 29))
  (op2 :field (byte 6 23) :value #b011110)
  (shift :fields (list (byte 4 19) (byte 3 16)) :type 'simd-immh-shift-left)
  (op :field (byte 5 11))
  (op3 :field (byte 1 10) :value #b1)
  (rn :fields (list (byte 1 30) (byte 4 19) (byte 5 5)) :type 'simd-immh-reg)
  (rd :fields (list (byte 4 19) (byte 5 0)) :type 'simd-immh-reg))

(macrolet
    ((def (name q u op)
       `(define-instruction ,name (segment rd sized rn sizen &optional (shift 0))
          (:printer simd-shift-by-imm ((q ,q) (u ,u) (op ,op)))
          (:emitter
           (let ((immh 0)
                 (immb 0))
             (ecase sized
               (:8h
                (aver (eq sizen
                          (if (zerop ,q)
                              :8B
                              :16B)))
                (setf immh #b1))
               (:4s
                (aver (eq sizen
                          (if (zerop ,q)
                              :4H
                              :8H)))
                (setf immh #b10)
                (setf (ldb (byte 1 0) immh) (ldb (byte 1 3) shift)))
               (:2d
                (aver (eq sizen
                          (if (zerop ,q)
                              :2S
                              :4S)))
                (setf immh #b100)
                (setf (ldb (byte 2 0) immh) (ldb (byte 2 3) shift))))
             (setf immb (ldb (byte 3 0) shift))
             (emit-simd-shift-by-imm segment
                                     ,q
                                     ,u
                                     immh
                                     immb
                                     ,op
                                     (fpr-offset rn)
                                     (fpr-offset rd)))))))
  (def ushll #b0 #b1 #b10100)
  (def ushll2 #b1 #b1 #b10100))

(macrolet
    ((def (name u op &optional right)
       `(define-instruction ,name (segment rd rn size shift)
          ;; Conflicts with simd-modified-imm where immh=0
          ,@(loop for (size pos) in '((4 19)
                                      (3 20)
                                      (2 21))
                  collect
                  `(:printer simd-shift-by-imm ((u ,u) (op ,op)
                                                (immh #b1 :field (byte ,size ,pos))
                                                ,@(if right
                                                      `((shift nil :type 'simd-immh-shift-right)))
                                                (rd nil :fields (list (byte 1 30) (byte 4 19) (byte 5 0))
                                                        :type 'simd-immh-reg))))
          (:emitter
           (let ((immh 0)
                 (immb 0)
                 (q 0)
                 (shift ,(if right
                             `(ldb (byte 6 0) (- shift))
                             `shift)))
             (ecase size
               (:8b
                (setf immh #b1
                      q 0))
               (:16b
                (setf immh #b1
                      q 1))
               (:4h
                (setf immh #b10
                      q 0))
               (:8h
                (setf immh #b10
                      q 1))
               (:2s
                (setf immh #b100
                      q 0))
               (:4s
                (setf immh #b100
                      q 1)))
             (setf immh (logior immh (ldb (byte (1- (integer-length immh)) 3) shift))
                   immb (ldb (byte 3 0) shift))
             (emit-simd-shift-by-imm segment
                                     q
                                     ,u
                                     immh
                                     immb
                                     ,op
                                     (fpr-offset rn)
                                     (fpr-offset rd)))))))
  (def sli #b1 #b01010)
  (def sri #b1 #b01000 t)
  (def ushr #b1 #b00000 t)
  (def shrn #b0 #b10000 t))

(def-emitter simd-modified-imm
  (#b0 1 31)
  (q 1 30)
  (op 1 29)
  (#b0111100000 10 19)
  (abc 3 16)
  (cmode 4 12)
  (o2 1 11)
  (#b1 1 10)
  (defgh 5 5)
  (rd 5 0))

(define-instruction-format (simd-modified-imm 32
                            :default-printer '(:name :tab rd ", #" imm))
  (o1 :field (byte 1 31) :value #b0)
  (q :field (byte 1 30))
  (op :field (byte 1 29))
  (op2 :field (byte 10 19) :value #b0111100000)
  (cmode :field (byte 4 12))
  (o2 :field (byte 1 11))
  (op3 :field (byte 1 10) :value #b1)
  (imm :fields (list (byte 3 16) (byte 4 12) (byte 5 5)) :type 'simd-modified-imm)
  (rd :fields (list (byte 1 30) (byte 4 12) (byte 5 0)) :type 'simd-reg-cmode))

(macrolet
    ((def (name o2)
       `(define-instruction ,name (segment rd imm size &optional (shift 0))
          ;; 8-bit
          (:printer simd-modified-imm ((o2 ,o2) (op 0)))
          (:emitter
           (let ((abc 0)
                 (defgh 0)
                 (cmode 0)
                 (op 0))
             (setf abc (ldb (byte 3 5) imm)
                   defgh (ldb (byte 5 0) imm))
             (ecase size
               ((:8b :16b)
                (setf cmode #b1110))
               ((:4h :8h)
                (setf cmode (ecase shift
                              (8 #b1010)
                              (0 #b1000))))
               ((:2s :4s)
                (setf cmode
                      (ash (ecase shift
                             (0 0)
                             (8 1)
                             (16 2)
                             (24 3))
                           1))))
             (emit-simd-modified-imm segment
                                     (encode-vector-size size)
                                     op
                                     abc
                                     cmode
                                     ,o2
                                     defgh
                                     (fpr-offset rd)))))))
  (def movi 0))

(def-emitter fp-cond-select
  (0 1 31)
  (0 1 30)
  (0 1 29)
  (#b11110 5 24)
  (ptype 2 22)
  (#b1 1 21)
  (rm 5 16)
  (cond 4 12)
  (#b11 2 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (fp-cond-select 32
                            :default-printer '(:name :tab rd  ", " rn ", " rm ", " cond))
  (m :field (byte 1 31) :value #b0)
  (op1 :field (byte 1 30) :value #b0)
  (s :field (byte 1 29) :value #b0)
  (op2 :field (byte 5 24) :value #b11110)
  (ptype :field (byte 2 22))
  (rm :fields (list (byte 2 22) (byte 5 16)) :type 'float-reg)
  (cond :field (byte 4 12) :type 'cond)
  (op3 :field (byte 2 10) :value #b11)
  (rn :fields (list (byte 2 22) (byte 5 5)) :type 'float-reg)
  (rd :fields (list (byte 2 22) (byte 5 0)) :type 'float-reg))

(define-instruction fcsel (segment rd rn rm cond)
  (:printer fp-cond-select ())
  (:emitter
   (emit-fp-cond-select
    segment
    (fp-reg-type rd)
    (fpr-offset rm)
    (conditional-opcode cond)
    (fpr-offset rn)
    (fpr-offset rd))))

(def-emitter simd-three-extension
  (#b0 1 31)
  (q 1 30)
  (u 1 29)
  (#b01110 5 24)
  (size 2 22)
  (#b0 1 21)
  (rm 5 16)
  (#b1 1 15)
  (opc 4 11)
  (#b1 1 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (simd-three-extension 32
                            :default-printer '(:name :tab rd ", " rn ", " rm))
  (op3 :field (byte 1 31) :value #b0)
  (u :field (byte 1 29))
  (op4 :field (byte 5 24) :value #b01110)
  (size :field (byte 2 22))
  (op5 :field (byte 1 21) :value #b0)
  (rm :fields (list (byte 1 30) (byte 2 22) (byte 5 16)) :type 'simd-reg)
  (op6 :field (byte 1 15) :value #b1)
  (op :field (byte 4 11))
  (op7 :field (byte 1 10) :value #b1)
  (rn :fields (list (byte 1 30) (byte 2 22) (byte 5 5)) :type 'simd-reg)
  (rd :fields (list (byte 1 30) (byte 2 22) (byte 5 0)) :type 'simd-reg))



(define-instruction fcadd (segment rd rn rm &optional size (rot 90))
  (:printer simd-three-extension ((op #b1100) (u 1))
            '(:name :tab rd ", " rn ", " rm ", #90"))
  (:printer simd-three-extension ((op #b1110) (u 1))
            '(:name :tab rd ", " rn ", " rm ", #270"))
  (:emitter
   (multiple-value-bind (q size) (encode-vector-size size)
     (emit-simd-three-extension segment
                                q
                                1 size
                           (fpr-offset rm)
                           (ecase rot
                             (90 #b1100)
                             (270 #b1110))
                           (fpr-offset rn)
                           (fpr-offset rd)))))

(define-instruction fcmla (segment rd rn rm &optional size (rot 90))
  (:printer simd-three-extension ((op #b1000) (u 1))
            '(:name :tab rd ", " rn ", " rm ", #0"))
  (:printer simd-three-extension ((op #b1001) (u 1))
            '(:name :tab rd ", " rn ", " rm ", #90"))
  (:printer simd-three-extension ((op #b1010) (u 1))
            '(:name :tab rd ", " rn ", " rm ", #180"))
  (:printer simd-three-extension ((op #b1011) (u 1))
            '(:name :tab rd ", " rn ", " rm ", #270"))
  (:emitter
   (multiple-value-bind (q size) (encode-vector-size size)
     (emit-simd-three-extension segment
                                q
                                1 size
                                (fpr-offset rm)
                                (ecase rot
                                  (0 #b1000)
                                  (90 #b1001)
                                  (180 #b1010)
                                  (270 #b1011))
                                (fpr-offset rn)
                                (fpr-offset rd)))))

(def-emitter simd-table
  (#b0 1 31)
  (q 1 30)
  (#b001110000 9 21)
  (rm 5 16)
  (#b0 1 15)
  (len 2 13)
  (op 1 12)
  (#b00 2 10)
  (rn 5 5)
  (rd 5 0))

(define-instruction-format (simd-table 32
                            :default-printer '(:name :tab rd ", " rn ", " rm))
  (o1 :field (byte 1 31) :value #b0)
  (q :field (byte 1 30))
  (o2 :field (byte 9 21) :value #b001110000)
  (rm :fields (list (byte 1 30) (byte 5 16)) :type 'simd-b-reg)
  (o4 :field (byte 1 15) :value #b0)
  (len :field (byte 2 13))
  (op :field (byte 1 12))
  (op4 :field (byte 2 10) :value #b00)
  (rn :fields (list (byte 2 13) (byte 5 5)) :type 'simd-table-regs)
  (rd :fields (list (byte 1 30) (byte 5 0)) :type 'simd-b-reg))

(macrolet
    ((def (name op)
       `(define-instruction ,name (segment rd rns rm &optional (size :16b))
          (:printer simd-table ((op ,op)))
          (:emitter
           (assert (<= 1 (length rns) 4))
           (loop for (rn next) on rns
                 unless (or (not next)
                            (= (1+ (fpr-offset rn))
                               (fpr-offset next)))
                 do (error "Non-consecutive registers ~a" rns))
           (emit-simd-table segment
                            (ecase size
                              (:16b 1)
                              (:8b 0))
                            (fpr-offset rm)
                            (1- (length rns))
                            ,op
                            (fpr-offset (car rns))
                            (fpr-offset rd))))))
  (def tbl 0)
  (def tbx 1))


;;; Inline constants
(defun canonicalize-inline-constant (constant)
  (let ((first (car constant))
        alignedp)
    (when (eql first :aligned)
      (setf alignedp t)
      (pop constant)
      (setf first (car constant)))
    (typecase first
      ((cons (eql :fixup))
       (setf constant (list :fixup (cdr first))))
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
        (setf constant (list :complex-double-float first)))))
    (destructuring-bind (type value) constant
      (ecase type
        ((:byte :word :dword :qword :oword)
         (etypecase value
           ((or integer (cons (eql :layout-id)))
            (cons type value))))
        (:base-char
         #+sb-unicode (aver (typep value 'base-char))
         (cons :byte (char-code value)))
        (:character
         (aver (characterp value))
         (cons :dword (char-code value)))
        (:single-float
         (aver (typep value 'single-float))
         (cons (if alignedp :oword :dword)
               (ldb (byte 32 0) (single-float-bits value))))
        (:double-float
         (aver (typep value 'double-float))
         (cons (if alignedp :oword :qword)
               (ldb (byte 64 0) (double-float-bits value))))
        (:complex-single-float
         (aver (typep value '(complex single-float)))
         (cons (if alignedp :oword :qword)
               (ldb (byte 64 0)
                    (logior (ash (single-float-bits (imagpart value)) 32)
                            (ldb (byte 32 0)
                                 (single-float-bits (realpart value)))))))
        (:complex-double-float
         (aver (typep value '(complex double-float)))
         (cons :oword
               (logior (ash (ldb (byte 64 0) (double-float-bits (imagpart value))) 64)
                       (ldb (byte 64 0) (double-float-bits (realpart value))))))
        ((:fixup :jump-table)
         (cons type value))))))

(defun inline-constant-value (constant)
  (let ((label (gen-label))
        (size  (ecase (car constant)
                 ((:byte :word :dword :qword) (car constant))
                 ((:fixup :jump-table) :qword)
                 ((:oword) :oword))))
    (values label (cons size label))))

(defun size-nbyte (size)
  (ecase size
    (:byte  1)
    ;; These keywords are completely wrong for AARCH64 but I don't want to touch them.
    ;; The correct definitions would have :HWORD (halfword) for 2 bytes, :WORD for 4,
    ;; :DWORD for 8, and :QWORD for 16.
    (:word  2)
    (:dword 4)
    ((:qword :fixup :jump-table) 8)
    (:oword 16)))

(defun sort-inline-constants (constants)
  (stable-sort constants #'> :key (lambda (constant)
                                    (size-nbyte (caar constant)))))

(sb-assem::%def-inst-encoder
 '.layout-id
 (lambda (segment layout)
   (sb-c:note-fixup segment :layout-id (sb-c:make-fixup layout :layout-id))
   (sb-assem::%emit-skip segment 4)))

(defun emit-inline-constant (section constant label)
  (let* ((type (car constant))
         (val (cdr constant))
         (size (size-nbyte type)))
    (emit section
          `(.align ,(integer-length (1- size)))
          label
          (cond ((typep val '(cons (eql :layout-id)))
                 `(.layout-id ,(cadr val)))

                ((eql type :fixup)
                 ;; Use the DWORD emitter which knows how to emit fixups
                 `(dword ,(apply #'make-fixup val)))
                ((eq type :jump-table)
                 `(.lispword ,@(coerce val 'list)))
                (t
                 ;; Could add pseudo-ops for .WORD, .INT, .QUAD, .OCTA just like gcc has.
                 ;; But it works fine to emit as a sequence of bytes
                 ;; FIXME: missing support for big-endian. Do we care?
                 `(.byte ,@(loop repeat size
                                 collect (prog1 (ldb (byte 8 0) val)
                                           (setf val (ash val -8))))))))))

(defun sb-vm:fixup-code-object (code offset value kind flavor)
  (declare (type index offset) (ignore flavor))
  (unless (zerop (rem offset sb-assem:+inst-alignment-bytes+))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (let ((sap (code-instructions code)))
    (ecase kind
      (:absolute
       (incf (sap-ref-word sap offset) value))
      (:layout-id
       (setf (signed-sap-ref-32 sap offset) value))
      (:cond-branch
       (setf (ldb (byte 19 5) (sap-ref-32 sap offset))
             (ash (- value (+ (sap-int sap) offset)) -2)))
      (:uncond-branch
       (setf (ldb (byte 26 0) (sap-ref-32 sap offset))
             (ash (- value (+ (sap-int sap) offset)) -2)))
      (:pc-relative
       (let ((page-displacement
               (- (ash value -12)
                  (ash (+ (sap-int sap) offset) -12))))
         (setf (ldb (byte 2 29) (sap-ref-32 sap offset))
               (ldb (byte 2 0) page-displacement))
         (setf (ldb (byte 19 5) (sap-ref-32 sap offset))
               (ldb (byte 19 2) page-displacement)))
       (setf (ldb (byte 12 10) (sap-ref-32 sap (+ offset 4)))
             (ldb (byte 12 0) value)))
      (:pc-relative-ldr-str
       (let ((page-displacement
               (- (ash value -12)
                  (ash (+ (sap-int sap) offset) -12))))
         (setf (ldb (byte 2 29) (sap-ref-32 sap offset))
               (ldb (byte 2 0) page-displacement))
         (setf (ldb (byte 19 5) (sap-ref-32 sap offset))
               (ldb (byte 19 2) page-displacement))
         (unless (zerop (logand value (1- n-word-bytes)))
           (error "Unaligned LDR/STR fixup at #x~x?" value))
         (setf (ldb (byte 12 10) (sap-ref-32 sap (+ offset 4)))
               (ash (ldb (byte 12 0) value) (- word-shift)))))
      (:ldr-str
       (setf (ldb (byte 12 10) (sap-ref-32 sap offset))
             (ash (the (unsigned-byte #.(+ 12 word-shift)) value)
                  (- word-shift))))
      (:ubfm-imms
       ;; The 'imms' value is is the inclusive index of the final
       ;; bit in the unsigned bitfield to copy (or "move").
       (setf (ldb (byte 6 10) (sb-vm::sap-ref-32 sap offset))
             (+ sb-vm::gencgc-card-shift value -1)))
      (:move-wide
       (setf (ldb (byte 16 5) (sap-ref-32 sap offset))
             (the (unsigned-byte 16) value)))))
  nil)

;;; Even though non darwin-jit arm64 targets can store directly in the
;;; code object, having two codepaths is cumbersome and, now that
;;; there's no reg-code, STRB needs to load a literal first, which
;;; isn't clearly a win compared to using a vector.
(define-instruction store-coverage-mark (segment mark-index temp vector)
  (:emitter
   ;; No backpatch is needed to compute the offset into the code header
   ;; because COMPONENT-HEADER-LENGTH is known at this point.
   segment mark-index temp vector
   (flet ((encode-index (offset &optional word)
            (cond
              ((if word
                   (typep offset '(integer 0 255))
                   (typep offset '(integer 0 4095)))
               offset)
              ((typep offset '(unsigned-byte 16))
               (inst* segment 'movz temp offset 0)
               temp)
              ((typep offset '(unsigned-byte 32))
               (inst* segment 'movz temp (ldb (byte 16 16) offset) 16)
               (inst* segment 'movk temp (ldb (byte 16 0) offset) 0)
               temp)
              (t
               (error "Bad offset ~a" offset)))))
     (let* ((vector-offset (* n-word-bytes
                              (- (length (ir2-component-constants
                                          (component-info *component-being-compiled*)))
                                 2)))
            (offset (+ (* sb-vm:vector-data-offset n-word-bytes)
                       mark-index
                       (- other-pointer-lowtag)))
            (addr
              (@ vector (encode-index offset))))
       (inst* segment 'load-constant vector vector-offset)
       (inst* segment 'strb sb-vm::null-tn addr)))))

(defun conditional-branch-p (stmt)
  (and (eq (stmt-mnemonic stmt) 'b)
       (= (length (stmt-operands stmt)) 2)))

(defpattern "cmp 0 + branch" ((subs) (b)) (stmt next)
  (let ((next-next (stmt-next next)))
    (when (and (not (stmt-labels next))
               next-next
               (not (conditional-branch-p next-next)))
      (destructuring-bind (target value cmp) (stmt-operands stmt)
        (when (and (eql cmp 0)
                   (eq target zr-tn))
          (destructuring-bind (flag label) (stmt-operands next)
            (unless (eq (sb-assem::label-comment label) :merged-ifs)
              (when (case flag
                      (:eq
                       (setf (stmt-mnemonic stmt) 'cbz
                             (stmt-operands stmt)
                             (list value label)))
                      (:ne
                       (setf (stmt-mnemonic stmt) 'cbnz
                             (stmt-operands stmt)
                             (list value label)))
                      (:ge
                       (setf (stmt-mnemonic stmt) 'tbz*
                             (stmt-operands stmt)
                             (list value (1- n-word-bits) label)))
                      (:lt
                       (setf (stmt-mnemonic stmt) 'tbnz*
                             (stmt-operands stmt)
                             (list value (1- n-word-bits) label))))
                (delete-stmt next)
                next-next))))))))

(defpattern "tst one bit + branch" ((ands) (b)) (stmt next)
  (let ((next-next (stmt-next next)))
    (when (and (not (stmt-labels next))
               next-next
               (not (conditional-branch-p next-next)))
      (destructuring-bind (target value mask) (stmt-operands stmt)
        (when (and (integerp mask)
                   (= (logcount (ldb (byte n-word-bits 0) mask)) 1)
                   (eq target zr-tn))
          (destructuring-bind (flag label) (stmt-operands next)
            (when (case flag
                    (:eq
                     (setf (stmt-mnemonic stmt) 'tbz*
                           (stmt-operands stmt)
                           (list value (1- (integer-length mask)) label)))
                    (:ne
                     (setf (stmt-mnemonic stmt) 'tbnz*
                           (stmt-operands stmt)
                           (list value (1- (integer-length mask)) label))))
              (delete-stmt next)
              next-next)))))))

(defun stmt-delete-safe-p (dst1 dst2 &optional safe-translates
                                               safe-vops)
  (or (location= dst1 dst2)
      (and
       (not (tn-ref-next (sb-c::tn-reads dst1)))
       (let ((vop (tn-ref-vop (sb-c::tn-reads dst1))))
         (and vop
              (or
               (memq (vop-name vop) safe-vops)
               (and vop
                    (loop for fun in safe-translates
                          thereis (memq (sb-c::vop-info vop)
                                        (sb-c::fun-info-templates (sb-c::fun-info-or-lose fun)))))
               (and (not safe-vops)
                    (not safe-translates))))))))

(defun tagged-mask-p (x)
  (and (integerp x)
       (plusp x)
       (zerop (ldb (byte 1 0) x))
       (let ((untag (ash x -1)))
         (= (integer-length untag)
            (logcount untag)))))

(defun untagged-mask-p (x)
  (and (integerp x)
       (plusp x)
       (= (integer-length x)
          (logcount x))))

;;; Tagging and applying a tagged mask can be done in one step.
(defpattern "lsl + and -> ubfiz" ((ubfm) (and)) (stmt next)
  (destructuring-bind (dst1 src1 immr imms) (stmt-operands stmt)
    (destructuring-bind (dst2 src2 mask) (stmt-operands next)
      (let (tagged)
        (when (and (location= dst1 src2)
                   (or (setf tagged (tagged-mask-p mask))
                       (untagged-mask-p mask))
                   (= immr 63)
                   (= imms 62)
                   (stmt-delete-safe-p dst1 dst2 '(logand)))
          (setf (stmt-mnemonic next) 'ubfm
                (stmt-operands next) (list dst2 src1 63 (+ (logcount mask)
                                                           (if tagged
                                                               -1
                                                               -2))))
          (add-stmt-labels next (stmt-labels stmt))
          (delete-stmt stmt)
          next)))))

;;; Helps with SBIT
(defpattern "and + lsl -> ubfiz" ((and) (ubfm)) (stmt next)
  (destructuring-bind (dst1 src1 mask) (stmt-operands stmt)
    (destructuring-bind (dst2 src2 immr imms) (stmt-operands next)
      (when (and (location= dst1 src2)
                 (untagged-mask-p mask)
                 (= immr 63)
                 (= imms 62)
                 (stmt-delete-safe-p dst1 dst2 nil '(sb-vm::move-from-word/fixnum)))
        (setf (stmt-mnemonic next) 'ubfm
              (stmt-operands next) (list dst2 src1 63 (1- (logcount mask))))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

;;; If the sign bit gets cut off it can be done with just a logical shift.
(defpattern "asr + and -> lsr" ((sbfm) (and)) (stmt next)
  (destructuring-bind (dst1 src1 immr imms) (stmt-operands stmt)
    (destructuring-bind (dst2 src2 mask) (stmt-operands next)
      (when (and (location= dst1 src2)
                 (untagged-mask-p mask)
                 (= (integer-length mask) 63)
                 (= immr 1)
                 (= imms 63)
                 (stmt-delete-safe-p dst1 dst2 '(logand)))
        (setf (stmt-mnemonic next) 'ubfm
              (stmt-operands next) (list dst2 src1 immr imms))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

;;; Applying a tagged mask and untagging
(defpattern "and + asr -> ubfx" ((and) (sbfm)) (stmt next)
  (destructuring-bind (dst1 src1 mask) (stmt-operands stmt)
    (destructuring-bind (dst2 src2 immr imms) (stmt-operands next)
      (when (and (location= dst1 src2)
                 (tagged-mask-p mask)
                 (= immr 1)
                 (= imms 63)
                 (stmt-delete-safe-p dst1 dst2 nil '(sb-vm::move-to-word/fixnum)))
        ;; Leave the ASR if the sign bit is left,
        ;; but the AND is not needed.
        (if (= (integer-length mask) 64)
            (setf (stmt-operands next) (list dst2 src1 immr imms))
            (setf (stmt-mnemonic next) 'ubfm
                  (stmt-operands next) (list dst2 src1 1 (logcount mask))))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

(defpattern "and + and -> and" ((and) (and)) (stmt next)
  (destructuring-bind (dst1 src1 mask1) (stmt-operands stmt)
    (destructuring-bind (dst2 src2 mask2) (stmt-operands next)
      (when
          (and (location= dst1 src2)
               (integerp mask1)
               (integerp mask2)
               (stmt-delete-safe-p dst1 dst2 '(logand)))
        (let ((mask (logand mask1 mask2)))
          (when (or (zerop mask)
                    (encode-logical-immediate mask))
           (if (zerop mask)
               (setf (stmt-mnemonic next) 'orr
                     (stmt-operands next) (list dst2 zr-tn zr-tn))
               (setf (stmt-operands next) (list dst2 src1 mask)))
           (add-stmt-labels next (stmt-labels stmt))
           (delete-stmt stmt)
           next))))))

(defpattern "lsl + arith -> arith" ((ubfm) (add and orr eor)) (stmt next)
  (destructuring-bind (dst1 src1 immr imms) (stmt-operands stmt)
    (destructuring-bind (dst2 srcn srcm) (stmt-operands next)
      (when (and (/= imms 63)
                 (= (1+ imms) immr)
                 (tn-p srcm)
                 (or
                  (location= dst1 srcm)
                  (location= dst1 srcn))
                 (not (location= srcn srcm))
                 (stmt-delete-safe-p dst1 dst2
                                     '(+ sb-vm::+-mod64 sb-vm::+-modfx
                                       logand logior logxor)))
        (setf (stmt-operands next) (list dst2 (if (location= dst1 srcm)
                                                  srcn
                                                  srcm)
                                         (lsl src1 (- 63 imms))))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

(defpattern "asr + arith -> arith" ((sbfm) (add and orr eor)) (stmt next)
  (destructuring-bind (dst1 src1 immr imms) (stmt-operands stmt)
    (destructuring-bind (dst2 srcn srcm) (stmt-operands next)
      (when (and (= imms 63)
                 (tn-p srcm)
                 (or
                  (location= dst1 srcm)
                  (location= dst1 srcn))
                 (not (location= srcn srcm))
                 (stmt-delete-safe-p dst1 dst2
                                     '(+ sb-vm::+-mod64 sb-vm::+-modfx
                                       logand logior logxor)))
        (setf (stmt-operands next) (list dst2 (if (location= dst1 srcm)
                                                  srcn
                                                  srcm)
                                         (asr src1 immr)))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

(defpattern "lsl + sub -> sub" ((ubfm) (sub)) (stmt next)
  (destructuring-bind (dst1 src1 immr imms) (stmt-operands stmt)
    (destructuring-bind (dst2 srcn srcm) (stmt-operands next)
      (when (and (/= imms 63)
                 (= (1+ imms) immr)
                 (tn-p srcm)
                 (location= dst1 srcm)
                 (not (location= srcn srcm))
                 (stmt-delete-safe-p dst1 dst2
                                     '(- sb-vm::--mod64 sb-vm::--modfx
                                       %negate)))
        (setf (stmt-operands next) (list dst2 srcn (lsl src1 (- 63 imms))))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

(defpattern "asr + sub -> sub" ((sbfm) (sub)) (stmt next)
  (destructuring-bind (dst1 src1 immr imms) (stmt-operands stmt)
    (destructuring-bind (dst2 srcn srcm) (stmt-operands next)
      (when (and (= imms 63)
                 (tn-p srcm)
                 (location= dst1 srcm)
                 (not (location= srcn srcm))
                 (stmt-delete-safe-p dst1 dst2
                                     '(- sb-vm::--mod64 sb-vm::--modfx
                                       %negate)))
        (setf (stmt-operands next) (list dst2 srcn (asr src1 immr)))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

(defpattern "lsl + lsl -> lsl" ((ubfm) (ubfm)) (stmt next)
  (destructuring-bind (dst1 src1 immr1 imms1) (stmt-operands stmt)
    (destructuring-bind (dst2 src2 immr2 imms2) (stmt-operands next)
      (when (and (/= imms1 63)
                 (/= imms2 63)
                 (= (1+ imms1) immr1)
                 (= (1+ imms2) immr2)
                 (location= dst1 src2)
                 (stmt-delete-safe-p dst1 dst2
                                     '(ash
                                       sb-vm::ash-left-mod64
                                       sb-vm::ash-left-modfx)
                                     '(sb-vm::move-from-word/fixnum)))
        (let ((shift (+ (- 63 imms1)
                        (- 63 imms2))))
          (when (<= shift 63)
            (setf (stmt-operands next) (list dst2 src1 (mod (- shift) 64) (- 63 shift)))
            (add-stmt-labels next (stmt-labels stmt))
            (delete-stmt stmt)
            next))))))

(defpattern "asr + asr -> asr" ((sbfm) (sbfm)) (stmt next)
  (destructuring-bind (dst1 src1 immr1 imms1) (stmt-operands stmt)
    (destructuring-bind (dst2 src2 immr2 imms2) (stmt-operands next)
      (when (and (= imms1 imms2 63)
                 (location= dst1 src2)
                 (stmt-delete-safe-p dst1 dst2
                                     nil '(sb-vm::move-to-word/fixnum)))
        (setf (stmt-operands next)
              (list dst2 src1 (min (+ immr1 immr2) 63) 63))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

(defpattern "sbfm + ubfm -> sbfm" ((sbfm) (ubfm)) (stmt next)
  (destructuring-bind (dst1 src1 immr1 imms1) (stmt-operands stmt)
    (destructuring-bind (dst2 src2 immr2 imms2) (stmt-operands next)
      (cond ((and (= immr1 0)
                  (= (1+ imms2) immr2)
                  (location= dst1 src2)
                  (stmt-delete-safe-p dst1 dst2
                                      '(ash
                                        sb-vm::ash-left-mod64
                                        sb-vm::ash-left-modfx)
                                      '(sb-vm::move-from-word/fixnum)))
             (setf (stmt-mnemonic next) 'sbfm
                   (stmt-operands next)
                   (list dst2 src1 immr2 imms1))
             (add-stmt-labels next (stmt-labels stmt))
             (delete-stmt stmt)
             next)
            ((and (> immr1 imms1)
                  (= (1+ imms2) immr2)
                  (location= dst1 src2)
                  (stmt-delete-safe-p dst1 dst2
                                      '(ash
                                        sb-vm::ash-left-mod64
                                        sb-vm::ash-left-modfx)))
             (setf (stmt-mnemonic next) 'sbfm
                   (stmt-operands next)
                   (list dst2 src1 (mod (+ immr1 immr2) 64) imms1))
             (add-stmt-labels next (stmt-labels stmt))
             (delete-stmt stmt)
             next)))))

;;; An even number can be shifted right and then negated,
;;; and fixnums are even.
(defpattern "neg + asr -> neg" ((sub) (sbfm)) (stmt next)
  (destructuring-bind (dst1 srcn srcm) (stmt-operands stmt)
    (destructuring-bind (dst2 src2 immr imms) (stmt-operands next)
      (when (and (= imms 63)
                 (= immr 1)
                 (tn-p srcm)
                 (sc-is srcm sb-vm::any-reg)
                 (location= srcn zr-tn)
                 (location= dst1 src2)
                 (stmt-delete-safe-p dst1 dst2 nil '(sb-vm::move-to-word/fixnum)))
        (setf (stmt-mnemonic next) 'sub
              (stmt-operands next) (list dst2 srcn (asr srcm 1)))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

(defpattern "mul + sub -> msub" ((madd) (sub)) (stmt next)
  (destructuring-bind (dst1 srcn1 srcm1 srca) (stmt-operands stmt)
    (destructuring-bind (dst2 srcn2 srcm2) (stmt-operands next)
      (when (and (tn-p srcm2)
                 (location= dst1 srcm2)
                 (location= srca zr-tn)
                 (not (location= srcn2 srcm2))
                 (stmt-delete-safe-p dst1 dst2
                                     '(- sb-vm::--mod64 sb-vm::--modfx)))
        (setf (stmt-mnemonic next) 'msub
              (stmt-operands next) (list dst2  srcn1 srcm1 srcn2))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

(defpattern "mul + add -> madd" ((madd) (add)) (stmt next)
  (destructuring-bind (dst1 srcn1 srcm1 srca) (stmt-operands stmt)
    (destructuring-bind (dst2 srcn2 srcm2) (stmt-operands next)
      (when (and (tn-p srcm2)
                 (location= srca zr-tn)
                 (not (location= srcn2 srcm2))
                 (or (location= dst1 srcm2)
                     (location= dst1 srcn2))
                 (stmt-delete-safe-p dst1 dst2
                                     '(+ sb-vm::+-mod64 sb-vm::+-modfx)))
        (setf (stmt-mnemonic next) 'madd
              (stmt-operands next) (list dst2
                                         srcn1 srcm1
                                         (if (location= dst1 srcm2)
                                             srcn2
                                             srcm2)))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))

;;; Fused multiply-accumulate will give different results as it
;;; performs a single rounding.
#+(or)
(defpattern "fmul + fsub -> fmsub" ((fmul) (fsub)) (stmt next)
  (destructuring-bind (dst1 srcn1 srcm1) (stmt-operands stmt)
    (destructuring-bind (dst2 srcn2 srcm2) (stmt-operands next)
      (when (and (location= dst1 srcm2)
                 (not (location= srcn2 srcm2))
                 (stmt-delete-safe-p dst1 dst2 '(-)))
        (setf (stmt-mnemonic next) 'fmsub
              (stmt-operands next) (list dst2 srcn1 srcm1 srcn2))
        (add-stmt-labels next (stmt-labels stmt))
        (delete-stmt stmt)
        next))))
