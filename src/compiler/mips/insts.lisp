;;; the instruction set definition for MIPS

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-MIPS-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from this package into SB-VM
  (import '(reg-tn-encoding) "SB-VM")
  ;; Imports from SB-VM into this package
  (import '(;; SBs, SCs, and TNs
            sb-vm::immediate-constant
            sb-vm::registers sb-vm::float-registers
            sb-vm::zero
            sb-vm::lip-tn sb-vm::zero-tn)))

;;;; Constants, types, conversion functions, some disassembler stuff.

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (zero sb-vm::zero-offset)
    (null sb-vm::null-offset)
    (t
     (if (eq (sb-name (sc-sb (tn-sc tn))) 'registers)
         (tn-offset tn)
         (error "~S isn't a register." tn)))))

(defun fp-reg-tn-encoding (tn)
  (declare (type tn tn))
  (unless (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers)
    (error "~S isn't a floating-point register." tn))
  (tn-offset tn))

(defvar *disassem-use-lisp-reg-names* t)

(defun location-number (loc)
  (etypecase loc
    (null)
    (number)
    (label)
    (fixup)
    (tn
     (ecase (sb-name (sc-sb (tn-sc loc)))
       (immediate-constant
        ;; Can happen if $ZERO or $NULL are passed in.
        nil)
       (registers
        (unless (zerop (tn-offset loc))
          (tn-offset loc)))
       (float-registers
        (+ (tn-offset loc) 32))))
    (symbol
     (ecase loc
       (:memory 0)
       (:hi-reg 64)
       (:low-reg 65)
       (:float-status 66)
       (:ctrl-stat-reg 67)))))

(defparameter *reg-symbols*
  (map 'vector
       (lambda (name)
         (and name
              (make-symbol (concatenate 'string "$" name))))
       sb-vm::*register-names*))

(define-arg-type reg
  :printer #'(lambda (value stream dstate)
               (declare (fixnum value))
               (let ((regname (aref *reg-symbols* value)))
                 (cond (stream
                        (princ regname stream)
                        (maybe-note-associated-storage-ref
                         value 'registers regname dstate))
                       (t
                        (operand regname dstate))))))

(define-arg-type load-store-annotation
  :printer (lambda (value stream dstate)
             ;; We don't need to print anything if not disassembling
             ;; to text, because rs + immediate were already shown.
             ;; The load-store-annotation is overlayed on those fields
             ;; (which I didn't think was allowed at all)
             (when stream
               (destructuring-bind (reg offset) value
                 (when (= reg sb-vm::code-offset)
                   (note-code-constant offset dstate))))))

(defparameter *float-reg-symbols*
  #.(coerce
     (loop for n from 0 to 31 collect (make-symbol (format nil "$F~d" n)))
     'vector))

(define-arg-type fp-reg
  :printer #'(lambda (value stream dstate)
               (declare (fixnum value))
               (let ((regname (aref *float-reg-symbols* value)))
                 (cond (stream
                        (princ regname stream)
                        (maybe-note-associated-storage-ref
                         value 'float-registers regname dstate))
                       (t
                        (operand regname dstate))))))

(define-arg-type control-reg :printer "(CR:#x~X)")

(define-arg-type relative-label
  :sign-extend t
  :use-label #'(lambda (value dstate)
                 (declare (type (signed-byte 16) value)
                          (type disassem-state dstate))
                 (+ (ash (1+ value) 2) (dstate-cur-addr dstate))))

(deftype float-format ()
  '(member :s :single :d :double :w :word))

(defun float-format-value (format)
  (ecase format
    ((:s :single) 0)
    ((:d :double) 1)
    ((:w :word) 4)))

(define-arg-type float-format
  :printer #'(lambda (value stream dstate)
               (declare (fixnum value))
               (let ((char (case value
                             (0 's)
                             (1 'd)
                             (4 'w)
                             (t '?))))
                 (if stream
                     (princ char stream)
                     (operand char dstate)))))

(defconstant-eqx compare-kinds
  '(:f :un :eq :ueq :olt :ult :ole :ule :sf :ngle :seq :ngl :lt :nge :le :ngt)
  #'equalp)

(defconstant-eqx compare-kinds-vec #.(apply #'vector compare-kinds)
  #'equalp)

(deftype compare-kind ()
  `(member ,@compare-kinds))

(defun compare-kind (kind)
  (or (position kind compare-kinds)
      (error "Unknown floating point compare kind: ~S~%Must be one of: ~S"
             kind
             compare-kinds)))

(define-arg-type compare-kind :printer compare-kinds-vec)

(defconstant-eqx float-operations '(+ - * /) #'equalp)

(deftype float-operation ()
  `(member ,@float-operations))

(defconstant-eqx float-operation-names
  ;; this gets used for output only
  #(add sub mul div)
  #'equalp)

(defun float-operation (op)
  (or (position op float-operations)
      (error "Unknown floating point operation: ~S~%Must be one of: ~S"
             op
             float-operations)))

(define-arg-type float-operation :printer float-operation-names)



;;;; Constants used by instruction emitters.

(defconstant special-op #b000000)
(defconstant bcond-op #b000001)
(defconstant cop0-op #b010000)
(defconstant cop1-op #b010001)
(defconstant cop2-op #b010010)
(defconstant cop3-op #b010011)



;;;; dissassem:define-instruction-formats

(defconstant-eqx immed-printer
  '(:name :tab rt (:unless (:same-as rt) ", " rs) ", " immediate)
  #'equalp)

;;; for things that use rt=0 as a nop
(defconstant-eqx immed-zero-printer
  '(:name :tab rt (:unless (:constant 0) ", " rs) ", " immediate)
  #'equalp)

(define-instruction-format (immediate 32 :default-printer immed-printer)
  (op :field (byte 6 26))
  (rs :field (byte 5 21) :type 'reg)
  (rt :field (byte 5 16) :type 'reg)
  (immediate :field (byte 16 0) :sign-extend t))

(defconstant-eqx load-store-printer
    '(:name :tab
      rt ", "
      rs
      (:unless (:constant 0) "[" immediate "]"))
  #'equalp)

(define-instruction-format
    (load-store 32 :default-printer '(:name :tab
                                      rt ", "
                                      rs
                                      (:unless (:constant 0) "[" immediate "]")
                                      load-store-annotation)
                   :include immediate)
    (load-store-annotation :fields (list (byte 5 21) (byte 16 0))
                           :type 'load-store-annotation))

(define-instruction-format (jump 32 :default-printer '(:name :tab target))
  (op :field (byte 6 26))
  (target :field (byte 26 0) :printer #'jump-printer))

(defconstant-eqx reg-printer
  '(:name :tab rd (:unless (:same-as rd) ", " rs) ", " rt)
  #'equalp)

(define-instruction-format (register 32 :default-printer reg-printer)
  (op :field (byte 6 26))
  (rs :field (byte 5 21) :type 'reg)
  (rt :field (byte 5 16) :type 'reg)
  (rd :field (byte 5 11) :type 'reg)
  (shamt :field (byte 5 6) :value 0)
  (funct :field (byte 6 0)))

(define-instruction-format
    (break 32 :default-printer
           '(:name :tab code (:unless (:constant 0) ", " subcode)))
  (op :field (byte 6 26) :value special-op)
  (code :field (byte 10 16) :reader break-code)
  (subcode :field (byte 10 6) :reader break-subcode)
  (funct :field (byte 6 0) :value #b001101))

(define-instruction-format (coproc-branch 32
                            :default-printer '(:name :tab offset))
  (op :field (byte 6 26))
  (funct :field (byte 10 16))
  (offset :field (byte 16 0)))

(defconstant-eqx float-fmt-printer
  '((:unless :constant funct)
    (:choose (:unless :constant sub-funct) nil)
    "." format)
  #'equalp)

(defconstant-eqx float-printer
  `(:name ,@float-fmt-printer
          :tab
          fd
          (:unless (:same-as fd) ", " fs)
          ", " ft)
  #'equalp)

(define-instruction-format (float 32 :default-printer float-printer)
  (op :field (byte 6 26) :value cop1-op)
  (filler :field (byte 1 25) :value 1)
  (format :field (byte 4 21) :type 'float-format)
  (ft :field (byte 5 16) :value 0)
  (fs :field (byte 5 11) :type 'fp-reg)
  (fd :field (byte 5 6) :type 'fp-reg)
  (funct :field (byte 6 0)))

(define-instruction-format (float-aux 32 :default-printer float-printer)
  (op :field (byte 6 26) :value cop1-op)
  (filler-1 :field (byte 1 25) :value 1)
  (format :field (byte 4 21) :type 'float-format)
  (ft :field (byte 5 16) :type 'fp-reg)
  (fs :field (byte 5 11) :type 'fp-reg)
  (fd :field (byte 5 6) :type 'fp-reg)
  (funct :field (byte 2 4))
  (sub-funct :field (byte 4 0)))

(define-instruction-format
    (float-op 32
              :include float
              :default-printer
                '('f funct "." format
                  :tab
                  fd
                  (:unless (:same-as fd) ", " fs)
                  ", " ft))
  (funct        :field (byte 2 0) :type 'float-operation)
  (funct-filler :field (byte 4 2) :value 0)
  (ft           :value nil :type 'fp-reg))


;;;; Primitive emitters.

(define-bitfield-emitter emit-word 32
  (byte 32 0))

(define-bitfield-emitter emit-immediate-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 16 0))

(define-bitfield-emitter emit-jump-inst 32
  (byte 6 26) (byte 26 0))

(define-bitfield-emitter emit-register-inst 32
  (byte 6 26) (byte 5 21) (byte 5 16) (byte 5 11) (byte 5 6) (byte 6 0))

(define-bitfield-emitter emit-break-inst 32
  (byte 6 26) (byte 10 16) (byte 10 6) (byte 6 0))

(define-bitfield-emitter emit-float-inst 32
  (byte 6 26) (byte 1 25) (byte 4 21) (byte 5 16)
  (byte 5 11) (byte 5 6) (byte 6 0))



;;;; Math instructions.

(defun emit-math-inst (segment dst src1 src2 reg-opcode immed-opcode
                               &optional allow-fixups)
  (unless src2
    (setf src2 src1)
    (setf src1 dst))
  (etypecase src2
    (tn
     (emit-register-inst segment special-op (reg-tn-encoding src1)
                         (reg-tn-encoding src2) (reg-tn-encoding dst)
                         0 reg-opcode))
    (integer
     (emit-immediate-inst segment immed-opcode (reg-tn-encoding src1)
                          (reg-tn-encoding dst) src2))
    (fixup
     (unless allow-fixups
       (error "Fixups aren't allowed."))
     (note-fixup segment :addi src2)
     (emit-immediate-inst segment immed-opcode (reg-tn-encoding src1)
                          (reg-tn-encoding dst) 0))))

(define-instruction add (segment dst src1 &optional src2)
  (:declare (type tn dst)
            (type (or tn (signed-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b100000)))
  (:printer immediate ((op #b001000)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100000 #b001000)))

;;; Note: ADDU doesn't really mean "unsigned" when used with an immediate operand.
;;; From the processor manual: "ALL arithmetic immediate values are sign-extended.
;;; After that, they are handled as signed or unsigned 32 bit numbers,
;;; depending upon the instruction. The only difference between signed and unsigned
;;; instructions is that signed instructions can generate an overflow exception
;;; and unsigned instructions can not."
(define-instruction addu (segment dst src1 &optional src2)
  (:declare (type tn dst)
            (type (or tn (signed-byte 16) fixup null) src1 src2))
  (:printer register ((op special-op) (funct #b100001)))
  (:printer immediate ((op #b001001)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100001 #b001001 t)))

(define-instruction sub (segment dst src1 &optional src2)
  (:declare
   (type tn dst)
   (type (or tn (integer #.(- 1 (ash 1 15)) #.(ash 1 15)) null) src1 src2))
  (:printer register ((op special-op) (funct #b100010)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (unless src2
     (setf src2 src1)
     (setf src1 dst))
   (emit-math-inst segment dst src1
                   (if (integerp src2) (- src2) src2)
                   #b100010 #b001000)))

(define-instruction subu (segment dst src1 &optional src2)
  (:declare
   (type tn dst)
   (type
    (or tn (integer #.(- 1 (ash 1 15)) #.(ash 1 15)) fixup null) src1 src2))
  (:printer register ((op special-op) (funct #b100011)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (unless src2
     (setf src2 src1)
     (setf src1 dst))
   (emit-math-inst segment dst src1
                   (if (integerp src2) (- src2) src2)
                   #b100011 #b001001 t)))

(define-instruction and (segment dst src1 &optional src2)
  (:declare (type tn dst)
            (type (or tn (unsigned-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b100100)))
  (:printer immediate ((op #b001100) (immediate nil :sign-extend nil)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100100 #b001100)))

(define-instruction or (segment dst src1 &optional src2)
  (:declare (type tn dst)
            (type (or tn (unsigned-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b100101)))
  (:printer immediate ((op #b001101)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100101 #b001101)))

(define-instruction xor (segment dst src1 &optional src2)
  (:declare (type tn dst)
            (type (or tn (unsigned-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b100110)))
  (:printer immediate ((op #b001110)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100110 #b001110)))

(define-instruction nor (segment dst src1 &optional src2)
  (:declare (type tn dst src1) (type (or tn null) src2))
  (:printer register ((op special-op) (funct #b100111)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b100111 #b000000)))

(define-instruction slt (segment dst src1 &optional src2)
  (:declare (type tn dst)
            (type (or tn (signed-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b101010)))
  (:printer immediate ((op #b001010)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b101010 #b001010)))

(define-instruction sltu (segment dst src1 &optional src2)
  (:declare (type tn dst)
            (type (or tn (signed-byte 16) null) src1 src2))
  (:printer register ((op special-op) (funct #b101011)))
  (:printer immediate ((op #b001011)))
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-math-inst segment dst src1 src2 #b101011 #b001011)))

(defconstant-eqx divmul-printer '(:name :tab rs ", " rt) #'equalp)

(define-instruction div (segment src1 src2)
  (:declare (type tn src1 src2))
  (:printer register ((op special-op) (rd 0) (funct #b011010)) divmul-printer)
  (:dependencies (reads src1) (reads src2) (writes :hi-reg) (writes :low-reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
                       (reg-tn-encoding src2) 0 0 #b011010)))

(define-instruction divu (segment src1 src2)
  (:declare (type tn src1 src2))
  (:printer register ((op special-op) (rd 0) (funct #b011011))
            divmul-printer)
  (:dependencies (reads src1) (reads src2) (writes :hi-reg) (writes :low-reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
                       (reg-tn-encoding src2) 0 0 #b011011)))

(define-instruction mult (segment src1 src2)
  (:declare (type tn src1 src2))
  (:printer register ((op special-op) (rd 0) (funct #b011000)) divmul-printer)
  (:dependencies (reads src1) (reads src2) (writes :hi-reg) (writes :low-reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
                       (reg-tn-encoding src2) 0 0 #b011000)))

(define-instruction multu (segment src1 src2)
  (:declare (type tn src1 src2))
  (:printer register ((op special-op) (rd 0) (funct #b011001)))
  (:dependencies (reads src1) (reads src2) (writes :hi-reg) (writes :low-reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src1)
                       (reg-tn-encoding src2) 0 0 #b011001)))

(defun emit-shift-inst (segment opcode dst src1 src2)
  (unless src2
    (setf src2 src1)
    (setf src1 dst))
  (etypecase src2
    (tn
     (emit-register-inst segment special-op (reg-tn-encoding src2)
                         (reg-tn-encoding src1) (reg-tn-encoding dst)
                         0 (logior #b000100 opcode)))
    ((unsigned-byte 5)
     (emit-register-inst segment special-op 0 (reg-tn-encoding src1)
                         (reg-tn-encoding dst) src2 opcode))))

(defconstant-eqx shift-printer
  '(:name :tab
          rd
          (:unless (:same-as rd) ", " rt)
          ", " (:cond ((rs :constant 0) shamt)
                      (t rs)))
  #'equalp)

(define-instruction sll (segment dst src1 &optional src2)
  (:declare (type tn dst)
            (type (or tn (unsigned-byte 5) null) src1 src2))
  (:printer register ((op special-op) (rs 0) (shamt nil) (funct #b000000))
            shift-printer)
  (:printer register ((op special-op) (funct #b000100)) shift-printer)
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-shift-inst segment #b00 dst src1 src2)))

(define-instruction sra (segment dst src1 &optional src2)
  (:declare (type tn dst)
            (type (or tn (unsigned-byte 5) null) src1 src2))
  (:printer register ((op special-op) (rs 0) (shamt nil) (funct #b000011))
            shift-printer)
  (:printer register ((op special-op) (funct #b000111)) shift-printer)
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-shift-inst segment #b11 dst src1 src2)))

(define-instruction srl (segment dst src1 &optional src2)
  (:declare (type tn dst)
            (type (or tn (unsigned-byte 5) null) src1 src2))
  (:printer register ((op special-op) (rs 0) (shamt nil) (funct #b000010))
            shift-printer)
  (:printer register ((op special-op) (funct #b000110)) shift-printer)
  (:dependencies (reads src1) (if src2 (reads src2) (reads dst)) (writes dst))
  (:delay 0)
  (:emitter
   (emit-shift-inst segment #b10 dst src1 src2)))


;;;; Floating point math.

(define-instruction float-op (segment operation format dst src1 src2)
  (:declare (type float-operation operation)
            (type float-format format)
            (type tn dst src1 src2))
  (:printer float-op ())
  (:dependencies (reads src1) (reads src2) (writes dst))
  (:delay 0)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format)
                    (fp-reg-tn-encoding src2) (fp-reg-tn-encoding src1)
                    (fp-reg-tn-encoding dst) (float-operation operation))))

(defconstant-eqx float-unop-printer
  `(:name ,@float-fmt-printer :tab fd (:unless (:same-as fd) ", " fs))
  #'equalp)

(define-instruction fabs (segment format dst &optional (src dst))
  (:declare (type float-format format) (type tn dst src))
  (:printer float ((funct #b000101)) float-unop-printer)
  (:dependencies (reads src) (writes dst))
  (:delay 0)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format)
                    0 (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
                    #b000101)))

(define-instruction fneg (segment format dst &optional (src dst))
  (:declare (type float-format format) (type tn dst src))
  (:printer float ((funct #b000111)) float-unop-printer)
  (:dependencies (reads src) (writes dst))
  (:delay 0)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format)
                    0 (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
                    #b000111)))

(define-instruction fcvt (segment format1 format2 dst src)
  (:declare (type float-format format1 format2) (type tn dst src))
  (:printer float-aux ((funct #b10) (sub-funct nil :type 'float-format))
           `(:name "." sub-funct "." format :tab fd ", " fs))
  (:dependencies (reads src) (writes dst))
  (:delay 0)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format2) 0
                    (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
                    (logior #b100000 (float-format-value format1)))))

(define-instruction fcmp (segment operation format fs ft)
  (:declare (type compare-kind operation)
            (type float-format format)
            (type tn fs ft))
  (:printer float-aux ((fd 0) (funct #b11) (sub-funct nil :type 'compare-kind))
            `(:name "-" sub-funct "." format :tab fs ", " ft))
  (:dependencies (reads fs) (reads ft) (writes :float-status))
  (:delay 1)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format)
                    (fp-reg-tn-encoding ft) (fp-reg-tn-encoding fs) 0
                    (logior #b110000 (compare-kind operation)))))


;;;; Branch/Jump instructions.

(defun emit-relative-branch (segment opcode r1 r2 target)
  (emit-chooser
   segment 20 2
      #'(lambda (segment chooser posn magic-value)
          (declare (ignore chooser magic-value))
          (let ((delta (ash (- (label-position target) (+ posn 4)) -2)))
            (when (typep delta '(signed-byte 16))
              (emit-back-patch segment 4
                #'(lambda (segment posn)
                    (emit-immediate-inst segment
                                         opcode
                                         (if (fixnump r1)
                                             r1
                                             (reg-tn-encoding r1))
                                         (if (fixnump r2)
                                             r2
                                             (reg-tn-encoding r2))
                                         (ash (- (label-position target)
                                                 (+ posn 4))
                                              -2))))
              t)))
      #'(lambda (segment posn)
          (declare (ignore posn))
          (let ((linked))
            ;; invert branch condition
            (if (or (= opcode bcond-op) (= opcode cop1-op))
                (setf r2 (logxor r2 #b00001))
                (setf opcode (logxor opcode #b00001)))
            ;; check link flag
            (if (= opcode bcond-op)
                (if (logand r2 #b10000)
                    (progn (setf r2 (logand r2 #b01111))
                           (setf linked t))))
            (emit-immediate-inst segment
                                 opcode
                                 (if (fixnump r1) r1 (reg-tn-encoding r1))
                                 (if (fixnump r2) r2 (reg-tn-encoding r2))
                                 4)
            (emit-nop segment)
            (emit-back-patch segment 8
              #'(lambda (segment posn)
                  (declare (ignore posn))
                  (emit-immediate-inst segment #b001111 0
                                       (reg-tn-encoding lip-tn)
                                       (ldb (byte 16 16)
                                            (label-position target)))
                  (emit-immediate-inst segment #b001101 0
                                       (reg-tn-encoding lip-tn)
                                       (ldb (byte 16 0)
                                            (label-position target)))))
            (emit-register-inst segment special-op (reg-tn-encoding lip-tn)
                                0 (if linked 31 0) 0
                                (if linked #b001001 #b001000))))))

(define-instruction b (segment target)
  (:declare (type label target))
  (:printer immediate ((op #b000100) (rs 0) (rt 0)
                       (immediate nil :type 'relative-label))
            '(:name :tab immediate))
  (:attributes branch)
  (:delay 1)
  (:emitter
   (emit-relative-branch segment #b000100 0 0 target)))

(define-instruction bal (segment target)
  (:declare (type label target))
  (:printer immediate ((op bcond-op) (rs 0) (rt #b01001)
                       (immediate nil :type 'relative-label))
            '(:name :tab immediate))
  (:attributes branch)
  (:dependencies (writes lip-tn))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment bcond-op 0 #b10001 target)))

(define-instruction beq (segment r1 r2-or-target &optional target)
  (:declare (type tn r1)
            (type (or tn fixnum label) r2-or-target)
            (type (or label null) target))
  (:printer immediate ((op #b000100) (immediate nil :type 'relative-label)))
  (:attributes branch)
  (:dependencies (reads r1) (if target (reads r2-or-target)))
  (:delay 1)
  (:emitter
   (unless target
     (setf target r2-or-target)
     (setf r2-or-target 0))
   (emit-relative-branch segment #b000100 r1 r2-or-target target)))

(define-instruction bne (segment r1 r2-or-target &optional target)
  (:declare (type tn r1)
            (type (or tn fixnum label) r2-or-target)
            (type (or label null) target))
  (:printer immediate ((op #b000101) (immediate nil :type 'relative-label)))
  (:attributes branch)
  (:dependencies (reads r1) (if target (reads r2-or-target)))
  (:delay 1)
  (:emitter
   (unless target
     (setf target r2-or-target)
     (setf r2-or-target 0))
   (emit-relative-branch segment #b000101 r1 r2-or-target target)))

(defconstant-eqx cond-branch-printer
  '(:name :tab rs ", " immediate)
  #'equalp)

(define-instruction blez (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op #b000110) (rt 0) (immediate nil :type 'relative-label))
            cond-branch-printer)
  (:attributes branch)
  (:dependencies (reads reg))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment #b000110 reg 0 target)))

(define-instruction bgtz (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op #b000111) (rt 0) (immediate nil :type 'relative-label))
            cond-branch-printer)
  (:attributes branch)
  (:dependencies (reads reg))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment #b000111 reg 0 target)))

(define-instruction bltz (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op bcond-op) (rt 0) (immediate nil :type 'relative-label))
            cond-branch-printer)
  (:attributes branch)
  (:dependencies (reads reg))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment bcond-op reg #b00000 target)))

(define-instruction bgez (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op bcond-op) (rt 1) (immediate nil :type 'relative-label))
            cond-branch-printer)
  (:attributes branch)
  (:dependencies (reads reg))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment bcond-op reg #b00001 target)))

(define-instruction bltzal (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op bcond-op) (rt #b01000) (immediate nil :type 'relative-label))
            cond-branch-printer)
  (:attributes branch)
  (:dependencies (reads reg) (writes lip-tn))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment bcond-op reg #b10000 target)))

(define-instruction bgezal (segment reg target)
  (:declare (type label target) (type tn reg))
  (:printer
   immediate ((op bcond-op) (rt #b01001) (immediate nil :type 'relative-label))
            cond-branch-printer)
  (:attributes branch)
  (:delay 1)
  (:dependencies (reads reg) (writes lip-tn))
  (:emitter
   (emit-relative-branch segment bcond-op reg #b10001 target)))

(defconstant-eqx j-printer
  '(:name :tab (:choose rs target))
  #'equalp)

(define-instruction j (segment target)
  (:declare (type tn target))
  (:printer register ((op special-op) (rt 0) (rd 0) (funct #b001000))
            j-printer)
  (:printer jump ((op #b000010)) j-printer)
  (:attributes branch)
  (:dependencies (reads target))
  (:delay 1)
  (:emitter
      (emit-register-inst segment special-op (reg-tn-encoding target)
                          0 0 0 #b001000)))

(define-instruction jal (segment reg-or-target &optional target)
  (:declare (type (or null tn) target)
            (type (or tn fixup) reg-or-target))
  (:printer register ((op special-op) (rt 0) (funct #b001001)) j-printer)
  (:printer jump ((op #b000011)) j-printer)
  (:attributes branch)
  (:dependencies (cond
                   (target
                    (writes reg-or-target) (reads target))
                   (t
                    (writes lip-tn)
                    (when (tn-p reg-or-target)
                      (reads reg-or-target)))))
  (:delay 1)
  (:emitter
   (unless target
     (setf target reg-or-target
           reg-or-target lip-tn))
   (emit-register-inst segment special-op (reg-tn-encoding target) 0
                       (reg-tn-encoding reg-or-target) 0 #b001001)))

(define-instruction bc1f (segment target)
  (:declare (type label target))
  (:printer coproc-branch ((op cop1-op) (funct #x100)
                           (offset nil :type 'relative-label)))
  (:attributes branch)
  (:dependencies (reads :float-status))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment cop1-op #b01000 #b00000 target)))

(define-instruction bc1t (segment target)
  (:declare (type label target))
  (:printer coproc-branch ((op cop1-op) (funct #x101)
                           (offset nil :type 'relative-label)))
  (:attributes branch)
  (:dependencies (reads :float-status))
  (:delay 1)
  (:emitter
   (emit-relative-branch segment cop1-op #b01000 #b00001 target)))



;;;; Random movement instructions.

(define-instruction lui (segment reg value)
  (:declare (type tn reg)
            (type (or fixup (signed-byte 16) (unsigned-byte 16)) value))
  (:printer immediate ((op #b001111)
                       (immediate nil :sign-extend nil :printer "#x~4,'0X")))
  (:dependencies (writes reg))
  (:delay 0)
  (:emitter
   (when (fixup-p value)
     (note-fixup segment :lui value)
     (setf value 0))
   (emit-immediate-inst segment #b001111 0 (reg-tn-encoding reg) value)))

(defconstant-eqx mvsreg-printer '(:name :tab rd)
  #'equalp)

(define-instruction mfhi (segment reg)
  (:declare (type tn reg))
  (:printer register ((op special-op) (rs 0) (rt 0) (funct #b010000))
            mvsreg-printer)
  (:dependencies (reads :hi-reg) (writes reg))
  (:delay 2)
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
                        #b010000)))

(define-instruction mthi (segment reg)
  (:declare (type tn reg))
  (:printer register ((op special-op) (rs 0) (rt 0) (funct #b010001))
            mvsreg-printer)
  (:dependencies (reads reg) (writes :hi-reg))
  (:delay 0)
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
                        #b010001)))

(define-instruction mflo (segment reg)
  (:declare (type tn reg))
  (:printer register ((op special-op) (rs 0) (rt 0) (funct #b010010))
            mvsreg-printer)
  (:dependencies (reads :low-reg) (writes reg))
  (:delay 2)
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
                        #b010010)))

(define-instruction mtlo (segment reg)
  (:declare (type tn reg))
  (:printer register ((op special-op) (rs 0) (rt 0) (funct #b010011))
            mvsreg-printer)
  (:dependencies (reads reg) (writes :low-reg))
  (:delay 0)
  (:emitter
   (emit-register-inst segment special-op 0 0 (reg-tn-encoding reg) 0
                        #b010011)))

(define-instruction move (segment dst src)
  (:declare (type tn dst src))
  (:printer register ((op special-op) (rt 0) (funct #b100001))
            '(:name :tab rd ", " rs))
  (:attributes flushable)
  (:dependencies (reads src) (writes dst))
  (:delay 0)
  (:emitter
   (emit-register-inst segment special-op (reg-tn-encoding src) 0
                       (reg-tn-encoding dst) 0 #b100001)))

(define-instruction fmove (segment format dst src)
  (:declare (type float-format format) (type tn dst src))
  (:printer float ((funct #b000110)) '(:name "." format :tab fd ", " fs))
  (:attributes flushable)
  (:dependencies (reads src) (writes dst))
  (:delay 0)
  (:emitter
   (emit-float-inst segment cop1-op 1 (float-format-value format) 0
                    (fp-reg-tn-encoding src) (fp-reg-tn-encoding dst)
                    #b000110)))

(defun %li (reg value)
  (etypecase value
    ((unsigned-byte 16)
     (inst or reg zero-tn value))
    ((signed-byte 16)
     (inst addu reg zero-tn value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (inst lui reg (ldb (byte 16 16) value))
     (when (logtest value #xFFFF)
       (inst or reg (ldb (byte 16 0) value))))
    (fixup
     (inst lui reg value)
     (inst addu reg value))))

(define-instruction-macro li (reg value)
  `(%li ,reg ,value))

(defconstant-eqx sub-op-printer '(:name :tab rd ", " rt) #'equalp)

(define-instruction mtc1 (segment to from)
  (:declare (type tn to from))
  (:printer register ((op cop1-op) (rs #b00100) (funct 0)) sub-op-printer)
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00100 (reg-tn-encoding from)
                       (fp-reg-tn-encoding to) 0 0)))

(define-instruction mtc1-odd (segment to from)
  (:declare (type tn to from))
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00100 (reg-tn-encoding from)
                       (1+ (fp-reg-tn-encoding to)) 0 0)))

(define-instruction mfc1 (segment to from)
  (:declare (type tn to from))
  (:printer register ((op cop1-op) (rs 0) (rd nil :type 'fp-reg) (funct 0))
            sub-op-printer)
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (reg-tn-encoding to)
                       (fp-reg-tn-encoding from) 0 0)))

(define-instruction mfc1-odd (segment to from)
  (:declare (type tn to from))
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (reg-tn-encoding to)
                       (1+ (fp-reg-tn-encoding from)) 0 0)))

(define-instruction mfc1-odd2 (segment to from)
  (:declare (type tn to from))
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (1+ (reg-tn-encoding to))
                       (fp-reg-tn-encoding from) 0 0)))

(define-instruction mfc1-odd3 (segment to from)
  (:declare (type tn to from))
  (:dependencies (reads from) (writes to))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00000 (1+ (reg-tn-encoding to))
                       (1+ (fp-reg-tn-encoding from)) 0 0)))

(define-instruction cfc1 (segment reg cr)
  (:declare (type tn reg) (type (unsigned-byte 5) cr))
  (:printer register ((op cop1-op) (rs #b00010) (rd nil :type 'control-reg)
                      (funct 0)) sub-op-printer)
  (:dependencies (reads :ctrl-stat-reg) (writes reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00010 (reg-tn-encoding reg)
                       cr 0 0)))

(define-instruction ctc1 (segment reg cr)
  (:declare (type tn reg) (type (unsigned-byte 5) cr))
  (:printer register ((op cop1-op) (rs #b00110) (rd nil :type 'control-reg)
                      (funct 0)) sub-op-printer)
  (:dependencies (reads reg) (writes :ctrl-stat-reg))
  (:delay 1)
  (:emitter
   (emit-register-inst segment cop1-op #b00110 (reg-tn-encoding reg)
                       cr 0 0)))



;;;; Random system hackery and other noise

(define-instruction break (segment code &optional (subcode 0))
  (:declare (type (unsigned-byte 10) code subcode))
  (:printer break ((op special-op) (funct #b001101))
            '(:name :tab code (:unless (:constant 0) ", " subcode))
            :control #'break-control)
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-break-inst segment special-op code subcode #b001101)))

(define-instruction syscall (segment)
  (:printer register ((op special-op) (rd 0) (rt 0) (rs 0) (funct #b001110))
            '(:name))
  :pinned
  (:delay 0)
  (:emitter
   (emit-register-inst segment special-op 0 0 0 0 #b001110)))

(define-instruction nop (segment)
  (:printer register ((op 0) (rd 0) (rs 0) (funct 0)) '(:name))
  (:attributes flushable)
  (:delay 0)
  (:emitter
   (emit-word segment 0)))

(defun emit-nop (segment)
  (emit-word segment 0))

(define-instruction word (segment word)
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (etypecase word
     (fixup
      (note-fixup segment :absolute word)
      (emit-word segment 0))
     (integer
      (emit-word segment word)))))

(define-instruction byte (segment byte)
  (:declare (type (or (unsigned-byte 8) (signed-byte 8)) byte))
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-byte segment byte)))


(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
   #'(lambda (segment posn)
       (emit-word segment
                  (logior type
                          (ash (+ posn (component-header-length))
                               (- n-widetag-bits word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-header-data segment simple-fun-widetag)))

(define-instruction lra-header-word (segment)
  :pinned
  (:cost 0)
  (:delay 0)
  (:emitter
   (emit-header-data segment return-pc-widetag)))


(defun emit-compute-inst (segment vop dst src label temp calc)
  (emit-chooser
   ;; We emit either 12 or 4 bytes, so we maintain 8 byte alignments.
   segment 12 3
   #'(lambda (segment chooser posn delta-if-after)
       (declare (ignore chooser))
       (let ((delta (funcall calc label posn delta-if-after)))
          (when (typep delta '(signed-byte 16))
            (emit-back-patch segment 4
                             #'(lambda (segment posn)
                                 (assemble (segment vop)
                                           (inst addu dst src
                                                 (funcall calc label posn 0)))))
            t)))
   #'(lambda (segment posn)
       (let ((delta (funcall calc label posn 0)))
         (assemble (segment vop)
                   (inst lui temp (ldb (byte 16 16) delta))
                   (inst or temp (ldb (byte 16 0) delta))
                   (inst addu dst src temp))))))

;; code = lip - header - label-offset + other-pointer-lowtag
(define-instruction compute-code-from-lip (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (- other-pointer-lowtag
                             (label-position label posn delta-if-after)
                             (component-header-length))))))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
;;      = lra - (header + label-offset)
(define-instruction compute-code-from-lra (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (- (+ (label-position label posn delta-if-after)
                                (component-header-length)))))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
;;     = code + header + label-offset
(define-instruction compute-lra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (+ (label-position label posn delta-if-after)
                             (component-header-length))))))


;;;; Loads and Stores

(defun emit-load/store-inst (segment opcode reg base index
                                     &optional (oddhack 0))
  (when (fixup-p index)
    (note-fixup segment :addi index)
    (setf index 0))
  (emit-immediate-inst segment opcode (reg-tn-encoding reg)
                       (+ (reg-tn-encoding base) oddhack) index))

(define-instruction lb (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100000)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100000 base reg index)))

(define-instruction lh (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100001)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100001 base reg index)))

(define-instruction lwl (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100010)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100010 base reg index)))

(define-instruction lw (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup label) index))
  (:printer load-store ((op #b100011)))
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (if (and (label-p index) (eq base sb-vm::code-tn))
       (emit-back-patch segment 4
        (lambda (segment posn)
          (declare (ignore posn))
          (emit-load/store-inst segment #b100011
                                base reg
                                (+ (component-header-length)
                                   (label-position index)
                                   (- sb-vm:other-pointer-lowtag)))))
       (emit-load/store-inst segment #b100011 base reg index))))

;; next is just for ease of coding double-in-int c-call convention
(define-instruction lw-odd (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100011 base reg index 1)))

(define-instruction lbu (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100100)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100100 base reg index)))

(define-instruction lhu (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100101)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100101 base reg index)))

(define-instruction lwr (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b100110)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-load/store-inst segment #b100110 base reg index)))

(define-instruction sb (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b101000)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-load/store-inst segment #b101000 base reg index)))

(define-instruction sh (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b101001)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-load/store-inst segment #b101001 base reg index)))

(define-instruction swl (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b101010)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-load/store-inst segment #b101010 base reg index)))

(define-instruction sw (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b101011)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-load/store-inst segment #b101011 base reg index)))

(define-instruction swr (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b101110)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-load/store-inst segment #b101110 base reg index)))


(defun emit-fp-load/store-inst (segment opcode reg odd base index)
  (when (fixup-p index)
    (note-fixup segment :addi index)
    (setf index 0))
  (emit-immediate-inst segment opcode (reg-tn-encoding base)
                       (+ (fp-reg-tn-encoding reg) odd) index))

(define-instruction lwc1 (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b110001) (rt nil :type 'fp-reg)) load-store-printer)
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-fp-load/store-inst segment #b110001 reg 0 base index)))

(define-instruction lwc1-odd (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:dependencies (reads base) (reads :memory) (writes reg))
  (:delay 1)
  (:emitter
   (emit-fp-load/store-inst segment #b110001 reg 1 base index)))

(define-instruction swc1 (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:printer immediate ((op #b111001) (rt nil :type 'fp-reg)) load-store-printer)
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-fp-load/store-inst segment #b111001 reg 0 base index)))

(define-instruction swc1-odd (segment reg base &optional (index 0))
  (:declare (type tn reg base)
            (type (or (signed-byte 16) fixup) index))
  (:dependencies (reads base) (reads reg) (writes :memory))
  (:delay 0)
  (:emitter
   (emit-fp-load/store-inst segment #b111001 reg 1 base index)))

;;; This mechanism is more complicated than minimally necessary for it to do its job.
;;; Consequently each backend has its own completely screwy way of canonicalizing
;;; because each one is better than the other.
;;; CONSTANT is just the &REST list passed to REGISTER-INLINE-CONSTANT which acts as
;;; a key in an EQUAL table for collapsing multiple references to the same data
;;; so that we only emit it once (or possibly not even once, if we fuse bytes from
;;; adjacent constants as suggested by comments in codegen. e.g. an 8-byte constant
;;; may contain a naturally-aligned 4-byte constant whose bytes match).
;;; The question is - why doesn't the vop just pass the proper key in the first place?
(defun canonicalize-inline-constant (constant)
  constant)

;;; Again this is too complex in the simplest case- you return an assembler label,
;;; and a cookie to hand to the consumer of the constant, which is {often,always}
;;; redundant, because the consumer knows what shape the value is - float, octaword, etc.
;;; The label alone conveys enough data to access the bits stored, however
;;; in some cases the sorting logic might need a way to determine the storage size.
(defun inline-constant-value (constant)
  (declare (ignore constant))
  (let ((label (gen-label)))
    (values label label)))

;;; Trivial "sort"
(defun sort-inline-constants (constants) constants)

;;; This is called once per unboxed constant, to emit its bytes.
;;; In general the bytes may be literal octets, or a fixup (as here)
;;; which is in turn emitted with a pseudo-instruction.
(defun emit-inline-constant (section constant label)
  (aver (typep constant '(cons (eql :layout-id) (cons t null))))
  (emit section
        `(.align 2) ; 2 bits of alignment (just to be pedantic I suppose)
        label
        `(.layout-id ,(cadr constant))))

(sb-assem::%def-inst-encoder
 '.layout-id
 (lambda (segment layout)
   (sb-c:note-fixup segment :absolute (sb-c:make-fixup layout :layout-id))
   (sb-assem::%emit-skip segment 4)))

(defun sb-vm:fixup-code-object (code offset value kind flavor)
  (declare (type index offset))
  (declare (ignore flavor))
  (unless (zerop (rem offset sb-assem:+inst-alignment-bytes+))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (let ((sap (code-instructions code)))
    (ecase kind
      (:absolute
       (setf (sap-ref-32 sap offset) value))
      (:jump
       (aver (zerop (ash value -28)))
       (setf (ldb (byte 26 0) (sap-ref-32 sap offset))
             (ash value -2)))
      (:lui
       (setf (ldb (byte 16 0) (sap-ref-32 sap offset))
             (ash (1+ (ash value -15)) -1)))
      (:addi
       (setf (ldb (byte 16 0) (sap-ref-32 sap offset))
             (ldb (byte 16 0) value)))))
  nil)

(define-instruction store-coverage-mark (segment path-index)
  ;; Don't need to annotate the dependence on code-tn, I think?
  (:dependencies (writes :memory))
  (:delay 0)
  (:emitter
   ;; No backpatch is needed to compute the offset into the code header
   ;; because COMPONENT-HEADER-LENGTH is known at this point.
   ;;
   ;; If someone wants to be clever and allow larger offsets from code-tn,
   ;; feel free to try to improve this, but given that ASSEM-SCHEDULER-P is T
   ;; for MIPS, I very much suspect that something would go wrong
   ;; by emitting more than 1 CPU instruction from within an emitter.
   (let ((offset (+ (component-header-length)
                    n-word-bytes ; skip over jump table word
                    path-index
                    (- other-pointer-lowtag))))
     (inst* segment 'sb sb-vm::null-tn sb-vm::code-tn
            (the (unsigned-byte 15) offset)))))
