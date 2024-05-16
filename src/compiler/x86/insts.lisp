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

(in-package "SB-X86-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from this package into SB-VM
  (import '(conditional-opcode negate-condition
            register-p ; FIXME: rename to GPR-P
            make-ea ea-disp width-bits) "SB-VM")
  ;; Imports from SB-VM into this package
  (import '(sb-vm::tn-byte-offset sb-vm::frame-byte-offset sb-vm::ebp-tn
            sb-vm::registers sb-vm::float-registers sb-vm::stack))) ; SB names

(deftype reg () '(unsigned-byte 3))

(defconstant +default-operand-size+ :dword)

(defparameter *default-address-size*
  ;; Actually, :DWORD is the only one really supported.
  :dword)

(defun width-bits (width)
  (ecase width
    (:byte 8)
    (:word 16)
    (:dword 32)
    (:float 32)
    (:double 64)))

;;;; disassembler argument types

(define-arg-type displacement
  :sign-extend t
  :use-label (lambda (value dstate)
               (ldb (byte 32 0) (+ (dstate-next-addr dstate) value)))
  :printer (lambda (value stream dstate)
             (maybe-note-assembler-routine value nil dstate)
             (print-label value stream dstate)))

(define-arg-type accum
  :printer (lambda (value stream dstate)
             (declare (ignore value))
             (print-reg 0 stream dstate)))

(define-arg-type word-accum
  :printer (lambda (value stream dstate)
             (declare (ignore value))
             (print-word-reg 0 stream dstate)))

(define-arg-type reg :printer #'print-reg)

(define-arg-type addr-reg :printer #'print-addr-reg)

(define-arg-type word-reg :printer #'print-word-reg)

(define-arg-type imm-addr
  :prefilter (lambda (dstate)
               (read-suffix (width-bits *default-address-size*) dstate))
  :printer #'print-label)

(define-arg-type imm-data
  :prefilter (lambda (dstate)
               (read-suffix (width-bits (inst-operand-size dstate)) dstate)))

(define-arg-type signed-imm-data
  :prefilter (lambda (dstate)
               (read-signed-suffix (width-bits (inst-operand-size dstate)) dstate)))

(define-arg-type imm-byte
  :prefilter (lambda (dstate)
               (read-suffix 8 dstate)))

(define-arg-type signed-imm-byte
  :prefilter (lambda (dstate)
               (read-signed-suffix 8 dstate)))

(define-arg-type signed-imm-dword
  :prefilter (lambda (dstate)
               (read-signed-suffix 32 dstate)))

(define-arg-type imm-word
  :prefilter (lambda (dstate)
               (let ((width (inst-word-operand-size dstate)))
                 (read-suffix (width-bits width) dstate))))

(define-arg-type signed-imm-word
  :prefilter (lambda (dstate)
               (let ((width (inst-word-operand-size dstate)))
                 (read-signed-suffix (width-bits width) dstate))))

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
(define-arg-type byte-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-byte-reg/mem)
(define-arg-type word-reg/mem
  :prefilter #'prefilter-reg/mem
  :printer #'print-word-reg/mem)

(define-arg-type fp-reg
  :printer
  (lambda (value stream dstate)
    (declare (ignore dstate))
    (format stream "FR~D" value)))

(define-arg-type width
  :prefilter #'prefilter-width
  :printer (lambda (value stream dstate)
             (declare (ignore value))
             (princ (schar (symbol-name (inst-operand-size dstate)) 0)
                    stream)))

(defconstant +fs-segment-prefix+ #b00001)
(defconstant +gs-segment-prefix+ #b00010)
(defconstant +operand-size-8+    #b00100)
(defconstant +operand-size-16+   #b01000)
(defconstant +imm-size-8+        #b10000)

;;; Used to capture the effect of the #x66 operand size override prefix.
(define-arg-type x66
  :prefilter (lambda (dstate junk)
               (declare (ignore junk))
               (dstate-setprop dstate +operand-size-16+)))

;;; Used to capture the effect of the #x64 and #x65 segment override
;;; prefixes.
(define-arg-type seg
  :prefilter (lambda (dstate value)
               (dstate-setprop dstate (ash 1 (the bit value)))))

(defconstant-eqx +conditions+
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
    (:nle . 15) (:g . 15))
  #'equal)
(defconstant-eqx +condition-name-vec+
  (let ((vec (make-array 16 :initial-element nil)))
    (dolist (cond +conditions+ vec)
      (when (null (aref vec (cdr cond)))
        (setf (aref vec (cdr cond)) (car cond)))))
  #'equalp)

(define-arg-type condition-code :printer +condition-name-vec+)

(defun conditional-opcode (condition)
  (cdr (assoc condition +conditions+ :test #'eq)))
(defun negate-condition (name)
  (aref +condition-name-vec+ (logxor 1 (conditional-opcode name))))

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

(define-instruction-format (three-bytes 24 :default-printer '(:name))
  (op :fields (list (byte 8 0) (byte 8 8) (byte 8 16))))

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
  ;; XXX: the following comment is bogus. x86-64 has 48-bit instructions.
  ;; The disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the offset.
  (label :type 'displacement
         :prefilter (lambda (dstate)
                      (read-signed-suffix 32 dstate))))

(define-instruction-format (near-jump 8 :default-printer '(:name :tab label))
  (op    :field (byte 8 0))
  ;; XXX: the following comment is bogus. x86-64 has 48-bit instructions.
  ;; The disassembler currently doesn't let you have an instruction > 32 bits
  ;; long, so we fake it by using a prefilter to read the address.
  (label :type 'displacement
         :prefilter (lambda (dstate)
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

(define-bitfield-emitter emit-mod-reg-r/m-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))

(define-bitfield-emitter emit-sib-byte 8
  (byte 2 6) (byte 3 3) (byte 3 0))

;;;; fixup emitters

(defun emit-absolute-fixup (segment fixup &optional (extra-addend 0))
  (note-fixup segment :absolute fixup)
  (let ((addend (fixup-offset fixup)))
    ;; Fixups are dumped without an explicit addend. The value in the fixed-up
    ;; location is implicitly added to the address resolved by the loader.
    ;; A fixup into the code object has a position denoted by a label,
    ;; and so the addend - the offset from the tagged code pointer to the label -
    ;; is indeterminate until label positions are fully computed.
    ;; This requires a backpatch. Additionally, if there is a displacement
    ;; beyond the label, it is incorporated into the implicit addend.
    (if (typep addend 'label)
        (emit-back-patch
           segment
           4
           (lambda (segment posn)
             (declare (ignore posn))
             (emit-dword segment
                         (+ (component-header-length)
                            (- (segment-header-skew segment))
                            (- other-pointer-lowtag)
                            (label-position addend)
                            extra-addend))))
        (emit-dword segment addend))))

(defun emit-relative-fixup (segment fixup)
  (note-fixup segment :relative fixup)
  (emit-dword segment (fixup-offset fixup)))

;;;; the effective-address (ea) structure

(declaim (ftype (sfunction (tn) (mod 8)) reg-tn-encoding))
(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (aver (eq (sb-name (sc-sb (tn-sc tn))) 'registers))
  (let ((offset (tn-offset tn)))
    (logior (ash (logand offset 1) 2)
            (ash offset -1))))

(defmacro emit-bytes (segment &rest bytes)
  `(progn ,@(mapcar (lambda (x) `(emit-byte ,segment ,x)) bytes)))
(defun opcode+size-bit (opcode size)
  (if (eq size :byte) opcode (logior opcode 1)))
(defun emit-byte+reg (seg byte reg)
  (emit-byte seg (+ byte (reg-tn-encoding reg))))

(deftype asm-expression ()
  '(cons (eql +) (cons label (cons integer null))))

(defstruct (ea (:constructor make-ea (size &key base index scale disp))
               (:copier nil))
  (size nil :type (member :byte :word :dword) :read-only t)
  (base nil :type (or tn null) :read-only t)
  (index nil :type (or tn null) :read-only t)
  (scale 1 :type (member 1 2 4 8) :read-only t)
  (disp 0 :type (or (unsigned-byte 32) (signed-byte 32)
                    fixup asm-expression)
          :read-only t))

(defmethod print-object ((ea ea) stream)
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
           (write-string (sb-c:location-print-name (ea-base ea)) stream)
           (when (ea-index ea)
             (write-string "+" stream)))
         (when (ea-index ea)
           (write-string (sb-c:location-print-name (ea-index ea)) stream))
         (unless (= (ea-scale ea) 1)
           (format stream "*~A" (ea-scale ea)))
         (typecase (ea-disp ea)
           (null)
           (integer
            (format stream "~@D" (ea-disp ea)))
           (t
            (format stream "+~A" (ea-disp ea))))
         (write-char #\] stream))))

(defun emit-ea (segment thing reg)
  (etypecase thing
    (tn
     (ecase (sb-name (sc-sb (tn-sc thing)))
       (registers
        (emit-mod-reg-r/m-byte segment #b11 reg (reg-tn-encoding thing)))
       (stack
        ;; Could this be refactored to fall into the EA case below instead
        ;; of consing a new EA? Probably.  Does it matter? Probably not.
        (emit-ea segment
                 (make-ea :dword :base ebp-tn
                          :disp (frame-byte-offset (tn-offset thing)))
                 reg))
       (constant
        (emit-mod-reg-r/m-byte segment #b00 reg #b101)
        (emit-absolute-fixup segment
                             (make-fixup nil
                                         :code-object
                                         (- (tn-byte-offset thing)
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
              (if (integerp disp)
                  (emit-dword segment disp)
                  (multiple-value-bind (fixup extra-offset)
                      (if (typep disp 'fixup)
                          (values disp 0)
                          (values (make-fixup nil :code-object (second disp)) (third disp)))
                    (emit-absolute-fixup segment fixup extra-offset)))))))))

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

(defun dword-reg-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)
       (eq (sb-c:sc-operand-size (tn-sc thing)) :dword)))

(defun register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)))

(defun accumulator-p (thing)
  (and (register-p thing)
       (= (tn-offset thing) 0)))

;;;; utilities

(defconstant +operand-size-prefix-byte+ #b01100110)

(defun maybe-emit-operand-size-prefix (segment size)
  (unless (or (eq size :byte) (eq size +default-operand-size+))
    (emit-byte segment +operand-size-prefix-byte+)))

(defun operand-size (thing)
  (typecase thing
    (tn
     (or (sb-c:sc-operand-size (tn-sc thing))
         (error "can't tell the size of ~S ~S" thing (sc-name (tn-sc thing)))))
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

(defun emit-imm-operand (segment value size)
  ;; In order by descending popularity
  (ecase size
    (:byte  (emit-byte segment value))
    (:dword (emit-dword segment value))
    (:word  (emit-word segment value))))

;;;; prefixes

(define-instruction x66 (segment)
  (:printer x66 () nil :print-name nil))

(defun emit-prefix (segment name)
  (ecase name
    ((nil))
    (:lock
     #+sb-thread
     (emit-byte segment #xf0))
    (:fs
     (emit-byte segment #x64))
    (:gs
     (emit-byte segment #x65))))

(define-instruction fs (segment)
  (:printer seg ((fsgs #b0)) nil :print-name nil))

(define-instruction gs (segment)
  (:printer seg ((fsgs #b1)) nil :print-name nil))

(define-instruction lock (segment)
  (:printer byte ((op #b11110000)) nil))

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
  (:printer reg ((op #b1011) (imm nil :type 'signed-imm-data))
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
   (let ((size (matching-operand-size dst src))
         (imm-src-p (typep src '(or integer fixup))))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((register-p dst)
            (cond (imm-src-p ; immediate to register
                   (emit-byte+reg segment (if (eq size :byte) #xB0 #xB8) dst))
                  (t ; mem to register or register to register
                   (emit-byte segment (if (eq size :byte) #x8A #x8B))
                   (emit-ea segment src (reg-tn-encoding dst)))))
           ((register-p src) ; register to memory
            (emit-byte segment (if (eq size :byte) #x88 #x89))
            (emit-ea segment dst (reg-tn-encoding src)))
           (imm-src-p ; immediate to memory
            (emit-byte segment (if (eq size :byte) #xC6 #xC7))
            (emit-ea segment dst #b000))
           (t
            (error "bogus arguments to MOV: ~S ~S" dst src)))
     (when imm-src-p
       (if (fixup-p src)
           (emit-absolute-fixup segment src)
           (emit-imm-operand segment src size))))))

(flet ((emit* (segment dst src opcode)
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
          (emit-ea segment src (reg-tn-encoding dst)))))))))

(define-instruction movsx (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011111)
                             (reg nil :type 'word-reg)
                             (reg/mem nil :type 'sized-reg/mem)))
  (:emitter (emit* segment dst src #b10111110)))

(define-instruction movzx (segment dst src)
  (:printer ext-reg-reg/mem ((op #b1011011)
                             (reg nil :type 'word-reg)
                             (reg/mem nil :type 'sized-reg/mem)))
  (:emitter (emit* segment dst src #b10110110))))

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
                   (emit-byte+reg segment #x50 src))
                  (t
                   (emit-byte segment #b11111111)
                   (emit-ea segment src #b110))))))))

(define-instruction pop (segment dst)
  (:printer reg-no-width ((op #b01011)))
  (:printer reg/mem ((op '(#b1000111 #b000)) (width 1)))
  (:emitter
   (let ((size (operand-size dst)))
     (aver (not (eq size :byte)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((register-p dst)
            (emit-byte+reg segment #x58 dst))
           (t
            (emit-byte segment #b10001111)
            (emit-ea segment dst #b000))))))

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
                    (emit-byte+reg segment #x90 something)
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

(macrolet ((def (mnemonic opcode)
             `(define-instruction ,mnemonic (segment)
                (:printer byte ((op ,opcode)))
                (:emitter (emit-byte segment ,opcode)))))
  (def daa   #x27) ; Decimal Adjust After Addition
  (def das   #x2F) ; Decimal Adjust after Subtraction
  (def aaa   #x37) ; ASCII Adjust After Addition
  (def aas   #x3F) ; ASCII Adjust After Subtraction
  (def pusha #x60) ; push all regs
  (def popa  #x61) ; pop all regs
  (def wait  #x9B) ; Wait
  (def pushf #x9C) ; Push flags
  (def popf  #x9D) ; Pop flags
  (def sahf  #x9E) ; Store AH into flags
  (def lahf  #x9F) ; Load AH from flags
  (def leave #xC9)
  (def into  #xCE) ; Interrupt if Overflow
  (def iret  #xCF) ; Interrupt Return
  (def xlat  #xD7) ; Translate Byte
  (def icebp #xF1) ; ICE breakpoint
  (def hlt   #xF4) ; Halt
  (def cmc   #xF5) ; Complement Carry Flag
  (def clc   #xF8) ; Clear Carry Flag
  (def stc   #xF9) ; Set Carry Flag
  (def cli   #xFA) ; Clear Iterrupt Enable Flag
  (def sti   #xFB) ; Set Interrupt Enable Flag
  (def cld   #xFC) ; Clear Direction Flag
  (def std   #xFD) ; Set Direction Flag
)

;;;; arithmetic

(defun emit-random-arith-inst (name segment dst src opcode)
  (let ((size (matching-operand-size dst src)))
    (maybe-emit-operand-size-prefix segment size)
    (cond
     ((or (integerp src)
          (and (fixup-p src) (memq (fixup-flavor src) '(:card-table-index-mask :layout-id))))
      (cond ((and (neq size :byte) (typep src '(signed-byte 8)))
             (emit-byte segment #b10000011)
             (emit-ea segment dst opcode)
             (emit-byte segment src))
            (t
             (cond ((accumulator-p dst)
                    (emit-byte segment
                               (dpb opcode
                                    (byte 3 3)
                                    (if (eq size :byte)
                                        #b00000100
                                        #b00000101))))
                   (t
                    (emit-byte segment (if (eq size :byte) #b10000000 #b10000001))
                    (emit-ea segment dst opcode)))
             (if (fixup-p src)
                 (emit-absolute-fixup segment src)
                 (emit-imm-operand segment src size)))))
     ((register-p src)
      (emit-byte segment
                 (dpb opcode
                      (byte 3 3)
                      (if (eq size :byte) #b00000000 #b00000001)))
      (emit-ea segment dst (reg-tn-encoding src)))
     ((register-p dst)
      (emit-byte segment
                 (dpb opcode
                      (byte 3 3)
                      (if (eq size :byte) #b00000010 #b00000011)))
      (emit-ea segment src (reg-tn-encoding dst)))
     (t
      (error "bogus operands to ~A" name)))))

(macrolet ((define (name subop)
             `(define-instruction ,name (segment dst src &optional prefix)
                (:printer accum-imm ((op ,(dpb subop (byte 3 2) #b0000010))))
                (:printer reg/mem-imm ((op '(#b1000000 ,subop))))
                (:printer reg/mem-imm ((op '(#b1000001 ,subop))
                                       (imm nil :type 'signed-imm-byte)))
                (:printer reg-reg/mem-dir ((op ,(dpb subop (byte 3 1) #b000000))))
                (:emitter
                 (emit-prefix segment prefix)
                 (emit-random-arith-inst ,(string name) segment dst src ,subop)))))
  (define add #b000)
  (define adc #b010)
  (define sub #b101)
  (define sbb #b011)
  (define cmp #b111)
  (define and #b100)
  (define or  #b001)
  (define xor #b110))

(define-instruction inc (segment dst)
  ;; Register.
  (:printer reg-no-width ((op #b01000)))
  ;; Register/Memory
  (:printer reg/mem ((op '(#b1111111 #b000))))
  (:emitter
   (let ((size (operand-size dst)))
     (maybe-emit-operand-size-prefix segment size)
     (cond ((and (not (eq size :byte)) (register-p dst))
            (emit-byte+reg segment #x40 dst))
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
            (emit-byte+reg segment #x48 dst))
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

(define-instruction mul (segment dst src)
  (:printer accum-reg/mem ((op '(#b1111011 #b100))))
  (:emitter
   (let ((size (matching-operand-size dst src)))
     (aver (accumulator-p dst))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment (if (eq size :byte) #b11110110 #b11110111))
     (emit-ea segment src #b100))))

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
                      (size-nbyte (inst-operand-size dstate)))))
             (read-signed-suffix (* nbytes 8) dstate)))))

(define-instruction imul (segment dst &optional src imm)
  (:printer accum-reg/mem ((op '(#b1111011 #b101))) '(:name :tab reg/mem))
  (:printer ext-reg-reg/mem-no-width ((op #xAF)))
  (:printer imul-3-operand () '(:name :tab reg ", " reg/mem ", " imm))
  (:emitter
   (let ((operand-size (matching-operand-size dst src)))
     (cond ((not src) ; 1-operand form affects EDX:EAX or subregisters thereof
            (aver (not imm))
            (maybe-emit-operand-size-prefix segment operand-size)
            (emit-byte segment (opcode+size-bit #xF6 operand-size))
            (emit-ea segment dst #b101))
           (t
            (aver (neq operand-size :byte))
            ;; If two operands and the second is immediate, it's really 3-operand
            ;; form with the same dst and src, which has to be a register.
            (when (and (integerp src) (not imm))
              (setq imm src src dst))
            (let ((imm-size (if (typep imm '(signed-byte 8)) :byte operand-size)))
              (maybe-emit-operand-size-prefix segment operand-size)
              (if imm
                  (emit-byte segment (if (eq imm-size :byte) #x6B #x69))
                  (emit-bytes segment #x0F #xAF))
              (emit-ea segment src (reg-tn-encoding dst))
              (if imm
                  (emit-imm-operand segment imm imm-size))))))))

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
   (emit-byte+reg segment #xC8 dst)))

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

(macrolet ((define (name direction-bit op)
             `(define-instruction ,name (segment dst src amt)
                (:declare (type (or (member :cl) (mod 32)) amt))
                (:printer ext-reg-reg/mem-no-width ((op ,(logior op #b100))
                                                    (imm nil :type 'imm-byte))
                          '(:name :tab reg/mem ", " reg ", " imm))
                (:printer ext-reg-reg/mem-no-width ((op ,(logior op #b101)))
                          '(:name :tab reg/mem ", " reg ", " 'cl))
                (:emitter
                 (emit-double-shift segment ,direction-bit dst src amt)))))
  (define shld 0 #b10100000)
  (define shrd 1 #b10101000))

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
                                (if (eq size :byte) #b10101000 #b10101001)))
                    (t
                     (emit-byte segment
                                (if (eq size :byte) #b11110110 #b11110111))
                     (emit-ea segment something #b000)))
              (emit-imm-operand segment immed size))
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

(macrolet ((define (inst opcode-extension)
             `(define-instruction ,inst (segment src index &optional prefix)
                (:printer ext-reg/mem-no-width+imm8
                          ((op '(#xBA ,opcode-extension))
                           (reg/mem nil :type 'sized-reg/mem)))
                (:printer ext-reg-reg/mem-no-width
                          ((op ,(dpb opcode-extension (byte 3 3) #b10000011))
                           (reg/mem nil :type 'sized-reg/mem))
                          '(:name :tab reg/mem ", " reg))
                (:emitter
                 (emit-prefix segment prefix)
                 (emit-bit-test-and-mumble segment src index
                                           ,opcode-extension)))))
  (define bt  4)
  (define bts 5)
  (define btr 6)
  (define btc 7))


;;;; control transfer

(defun emit-byte-displacement-backpatch (segment target)
  (emit-back-patch segment 1
                   (lambda (segment posn)
                     (emit-byte segment
                                (the (signed-byte 8)
                                  (- (label-position target) (1+ posn)))))))

(defun emit-dword-displacement-backpatch (segment target)
  (emit-back-patch segment 4
                   (lambda (segment posn)
                     (emit-dword segment (- (label-position target)
                                            (+ 4 posn))))))

(define-instruction call (segment where)
  (:printer near-jump ((op #b11101000)))
  (:printer reg/mem ((op '(#b1111111 #b010)) (width 1)))
  (:emitter
   (typecase where
     (label
      (emit-byte segment #b11101000)
      (emit-dword-displacement-backpatch segment where))
     (fixup
      (emit-byte segment #b11101000)
      (emit-relative-fixup segment where))
     (t
      (emit-byte segment #b11111111)
      (emit-ea segment where #b010)))))

;;; We try to be more clever than usual here in computing the maximum allowed
;;; branch displacement eligible for 1-byte encoding.
;;; For a forward branch, this chooser itself is going to shrink, making the
;;; anticipated LABEL-POSITION off by the amount of shrinkage.
;;; (The position is correct for backward branch).
;;; Essentially, if shrinkage occurs, the target moves move closer to the
;;; branch instruction, unless there is an intervening .ALIGN directive
;;; in which case it might not. (It might, but we don't know)
(define-instruction jmp (segment cond &optional where)
  ;; conditional jumps
  (:printer short-cond-jump ((op #b0111)) '('j cc :tab label))
  (:printer near-cond-jump () '('j cc :tab label))
  ;; unconditional jumps
  (:printer short-jump ((op #b1011)))
  (:printer near-jump ((op #b11101001)) )
  (:printer reg/mem ((op '(#b1111111 #b100)) (width 1)))
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
          (emit-chooser
           segment 6 2 ; emit either 2 or 6 bytes
           ;; The difference in encoding lengths is 4, therefore this
           ;; preserves 4-byte alignment ("2 bits" as we put it).
           (lambda (segment chooser posn delta-if-after)
             (let ((disp (- (label-position where posn delta-if-after)
                            (+ posn 2))))
               (when (byte-disp-p chooser where disp 4)
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
          (emit-chooser ; emit either 2 or 5 bytes; no alignment is preserved
           segment 5 0
           (lambda (segment chooser posn delta-if-after)
             (let ((disp (- (label-position where posn delta-if-after)
                            (+ posn 2))))
               (when (byte-disp-p chooser where disp 3)
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
          (emit-ea segment where #b100))))))

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

(define-instruction set (segment cond dst)
  (:printer cond-set ())
  (:emitter
   (emit-byte segment #b00001111)
   (emit-byte segment (dpb (conditional-opcode cond) (byte 4 0) #b10010000))
   (emit-ea segment dst #b000)))

(define-instruction enter (segment disp &optional (level 0))
  (:declare (type (unsigned-byte 16) disp)
            (type (unsigned-byte 8) level))
  (:printer enter-format ((op #b11001000)))
  (:emitter
   (emit-byte segment #b11001000)
   (emit-word segment disp)
   (emit-byte segment level)))

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

(define-instruction break (segment &optional (code nil codep))
  (:printer byte-imm ((op #xCC)) :default :print-name 'int3 :control #'break-control)
  (:printer word-imm ((op #x0B0F)) :default :print-name 'ud2 :control #'break-control)
  (:emitter
   #-ud2-breakpoints (emit-byte segment #xCC)
   #+ud2-breakpoints (emit-word segment #x0B0F)
   (when codep (emit-byte segment (the (unsigned-byte 8) code)))))

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

(define-instruction bound (segment reg bounds)
  (:emitter
   (let ((size (matching-operand-size reg bounds)))
     (when (eq size :byte)
       (error "can't bounds-test bytes: ~S" reg))
     (maybe-emit-operand-size-prefix segment size)
     (emit-byte segment #b01100010)
     (emit-ea segment bounds (reg-tn-encoding reg)))))


;;;; processor control

(define-instruction nop (segment)
  (:printer byte ((op #b10010000)))
  (:printer ext-reg/mem-no-width ((op '(#x1F 0))))
  (:emitter
   (emit-byte segment #b10010000)))


;;;; miscellaneous hackery

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

(defun emit-header-data (segment type)
  (emit-back-patch
   segment
   4
   (lambda (segment posn)
     (emit-dword segment
                 (logior type
                         (ash (+ (component-header-length)
                                 (- (segment-header-skew segment))
                                 posn)
                              (- n-widetag-bits word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  (:emitter
   (emit-header-data segment simple-fun-widetag)))

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
   (emit-byte segment #xc7)
   (emit-byte segment #xf8)
   (if where
       ;; emit 32-bit, signed relative offset for where
       (emit-dword-displacement-backpatch segment where)
       ;; nowhere to jump: simply jump to the next instruction
       (emit-dword segment 0))))

(define-instruction xend (segment)
  (:printer three-bytes ((op '(#x0f #x01 #xd5))))
  (:emitter
   (emit-byte segment #x0f)
   (emit-byte segment #x01)
   (emit-byte segment #xd5)))

(define-instruction xabort (segment reason)
  (:printer xabort ())
  (:emitter
   (aver (<= 0 reason #xff))
   (emit-byte segment #xc6)
   (emit-byte segment #xf8)
   (emit-byte segment reason)))

(define-instruction xtest (segment)
  (:printer three-bytes ((op '(#x0f #x01 #xd6))))
  (:emitter
   (emit-byte segment #x0f)
   (emit-byte segment #x01)
   (emit-byte segment #xd6)))

(define-instruction xacquire (segment) ;; same prefix byte as repne/repnz
  (:emitter
   (emit-byte segment #xf2)))

(define-instruction xrelease (segment) ;; same prefix byte as rep/repe/repz
  (:emitter
   (emit-byte segment #xf3)))

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
                                        (double-float-low-bits value)))))
      ((:jump-table)
       (cons :jump-table value)))))

(defun inline-constant-value (constant)
  (let ((label (gen-label))
        (size  (ecase (car constant)
                 ((:byte :word :dword) (car constant))
                 ((:double-float :jump-table) :dword))))
    (values label (make-ea size
                           :disp (make-fixup nil :code-object label)))))

(defun size-nbyte (size)
  (ecase size
    (:byte  1)
    (:word  2)
    (:dword 4)
    (:double-float 8)))

(defun align-of (constant)
  (case (car constant)
    (:jump-table n-word-bytes)
    (t (size-nbyte (car constant)))))

(defun sort-inline-constants (constants)
  ;; Each constant is ((size . bits) . label)
  (stable-sort constants #'> :key (lambda (x) (align-of (car x)))))

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
              `(.byte ,@(let ((val (cdr constant)))
                          (loop repeat size
                                collect (prog1 (ldb (byte 8 0) val)
                                          (setf val (ash val -8))))))))))

;;; This gets called by LOAD to resolve newly positioned objects
;;; with things (like code instructions) that have to refer to them.
(defun fixup-code-object (code offset value kind flavor)
  (declare (type index offset))
  (let ((sap (code-instructions code)))
    (ecase kind
      (:absolute
       (case flavor
         (:layout-id
          (setf (signed-sap-ref-32 sap offset) value))
         (:card-table-index-mask
          ;; the VALUE is nbits, so convert it to an AND mask
          (setf (sap-ref-32 sap offset) (1- (ash 1 value))))
         (t
          ;; 32-bit quantity at SAP + offset contains an
          ;; addend to be replaced by adding it to VALUE.
          (setf (sap-ref-32 sap offset) (+ value (sap-ref-32 sap offset))))))
      (:relative
       ;; VALUE is the actual address wanted.
       ;; Replace word with displacement to get there.
       (let* ((loc-sap (+ (sap-int sap) offset))
              ;; Use modular arithmetic so that if the offset
              ;; doesn't fit into signed-byte-32 it'll wrap around
              ;; when added to EIP.
              ;; "-4" is for the number of remaining bytes in the current instruction.
              ;; The CPU calculates based off the next instruction.
              (rel-val (ldb (byte 32 0) (- value loc-sap 4))))
         (declare (type (unsigned-byte 32) loc-sap rel-val))
         (setf (sap-ref-32 sap offset) rel-val)))))
  nil)

(defun sb-c::pack-fixups-for-reapplication (fixup-notes)
  (let (abs-fixups rel-fixups imm-fixups)
    (dolist (note fixup-notes)
      (let* ((fixup (fixup-note-fixup note))
             (offset (fixup-note-position note))
             (kind (fixup-note-kind note))
             (flavor (fixup-flavor fixup)))
        (cond ((and (eq kind :absolute) (eq flavor :code-object))
               ;; If there are N jump table entries, then any code-object fixups occurring
               ;; at offsets 4, 8, 12, ..., 4*N are to patch in the jump vectors, and
               ;; do need need to be explicitly retained. GC knows how to fix them again.
               ;; (Offset 0 is the jump table count word)
               ;; We care where the patch occurs, and NOT where the patch points to.
               (unless (<= offset (* sb-vm:n-word-bytes (sb-c::component-n-jump-table-entries)))
                 (push offset abs-fixups)))
              ((and (eq kind :relative) (member flavor '(:assembly-routine :foreign)))
               (push offset rel-fixups))
              ((eq flavor :card-table-index-mask)
               (push offset imm-fixups))
              ((or (and (eq kind :absolute)
                        (member flavor '(:assembly-routine :foreign :foreign-dataref)))
                   (member flavor '(:layout-id :symbol-tls-index)))) ; discard
              (t
               (bug "Unexpected fixup")))))
    (sb-c:pack-code-fixup-locs abs-fixups rel-fixups imm-fixups)))

;;; Coverage support

(define-instruction store-coverage-mark (segment mark-index)
  (:emitter
   (let ((offset (+ (component-header-length)
                    ;; skip over jump table word and entries
                    (* (1+ (component-n-jump-table-entries))
                       n-word-bytes)
                    mark-index
                    (- other-pointer-lowtag))))
     (assemble (segment)
       (inst mov (make-ea :byte :disp (make-fixup nil :code-object offset)) 1)))))

;;; Perform exhaustive analysis here because of the extreme degree
;;; of confusion I have about what is allowed to reach the instruction
;;; emitter as a raw fixup, a fixup wrapped in an EA, a label wrapped
;;; in a fixup wrapped in an EA etc.
;;; (The x86-64 assembler is more understandable - we need to kill this one)
(defun sb-assem::%mark-used-labels (operand)
  (named-let recurse ((operand operand))
    (etypecase operand
      ((or integer tn keyword null))
      #+sb-xc-host ((cons (eql sb-assem::entry)))
      (ea
       (let ((disp (ea-disp operand)))
         (etypecase disp
           (label (setf (label-usedp disp) t))
           (fixup (recurse disp))
           (integer))))
      (fixup
       (let ((offset (fixup-offset operand)))
         (cond ((label-p offset)
                (setf (label-usedp offset) t))
               ((consp offset)
                (setf (label-usedp (car offset)) t))))))))
