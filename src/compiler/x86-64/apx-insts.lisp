;;;; Intel APX (Advanced Performance Extensions) Instruction Support for x86-64
;;;;
;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files and newer ISA extensions were written from scratch after the
;;;; fork from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "SB-X86-64-ASM")

;;; Register helper functions for operand inspection

(defun operand-reg-num (r)
  (cond ((register-p r) (reg-id-num (reg-id r)))
        ((and (tn-p r) (tn-offset r)) (tn-offset r))
        ((integerp r) r)
        (t nil)))

(defun operand-bit3 (r)
  (let ((num (operand-reg-num r)))
    (if (and num (logbitp 3 num)) 1 0)))

(defun operand-bit4 (r)
  (let ((num (operand-reg-num r)))
    (if (and num (logbitp 4 num)) 1 0)))

(defun get-apx-gpr (size number)
  "Construct or retrieve a general-purpose register descriptor for registers 0..31."
  (declare (type (member :qword :dword :word :byte) size)
           (type (mod 32) number))
  (svref (load-time-value
          (coerce (append
                   (loop for i from 0 below 32 collect (!make-reg (logior (ash i 3) 0)))   ; qword
                   (loop for i from 0 below 32 collect (!make-reg (logior (ash i 3) 2)))   ; dword
                   (loop for i from 0 below 32 collect (!make-reg (logior (ash i 3) 4)))   ; word
                   (loop for i from 0 below 32 collect (!make-reg (logior (ash i 3) 6))))  ; byte
                  'vector)
          t)
         (+ number
            (ecase size
              (:qword 0)
              (:dword 32)
              (:word  64)
              (:byte  96)))))

;;; REX2 Prefix (0xD5)

(defun emit-rex2 (segment &key (w nil) (r nil) (x nil) (b nil) (map 0))
  "Emit 2-byte REX2 prefix (0xD5) for extended GPR addressing."
  (flet ((reg-bit3 (reg) (operand-bit3 reg))
         (reg-bit4 (reg) (operand-bit4 reg)))
    (emit-byte segment #xD5)
    (emit-byte segment
               (logior (ash (if (eql map 1) 1 0) 7)
                       (ash (reg-bit4 r) 6)
                       (ash (reg-bit4 x) 5)
                       (ash (reg-bit4 b) 4)
                       (ash (if w 1 0) 3)
                       (ash (reg-bit3 r) 2)
                       (ash (reg-bit3 x) 1)
                       (reg-bit3 b)))))

;;; APX EVEX Prefix (0x62) Encoding

(defun apx-encode-mm (m-mmmm)
  (ecase m-mmmm
    ((:map4 4) #b100)
    ((:map7 7) #b111)))

(defun apx-encode-pp (pp)
  (ecase pp
    ((nil 0) 0)
    ((#x66 1) #b01)
    ((#xF3 2) #b10)
    ((#xF2 3) #b11)))

(defun determine-apx-flags (thing reg vvvv)
  "Extract APX EVEX prefix flags from operands.
Returns: (values r x b r-prime b-prime v-prime x-prime)"
  (let* ((r (operand-bit3 reg))
         (r-prime (operand-bit4 reg))
         (v-prime (operand-bit4 vvvv))
         (ea-p (ea-p thing))
         (base (and ea-p (ea-base thing)))
         (index (and ea-p (ea-index thing)))
         (b (cond (ea-p
                   (if (and base (neq base rip-tn))
                       (operand-bit3 base)
                       0))
                  (t (operand-bit3 thing))))
         (b-prime (cond (ea-p
                         (if (and base (neq base rip-tn))
                             (operand-bit4 base)
                             0))
                        (t (operand-bit4 thing))))
         (x (if index (operand-bit3 index) 0))
         (x-prime (if index (operand-bit4 index) 0)))
    (values r x b r-prime b-prime v-prime x-prime)))

(defun emit-apx-evex (segment r x b r-prime opcode-prefix w vvvv pp z ll evex-b v-prime aaa
                      &key (b-prime 0) (x-prime 0) (nd 0) (nf 0) scc dfv)
  (emit-bytes segment
              #x62
              ;; P1: R X B R' B4 mmm
              (logior (ash (logxor 1 r) 7)
                      (ash (logxor 1 x) 6)
                      (ash (logxor 1 b) 5)
                      (ash (logxor 1 r-prime) 4)
                      (ash (if (eql b-prime 1) 1 0) 3)
                      (apx-encode-mm opcode-prefix))
              ;; P2: W vvvv ~X4 pp
              (logior (ash (if (eql w 1) 1 0) 7)
                      (if dfv
                          (ash (logand dfv #b1111) 3)
                          (ash (logandc1 vvvv #b1111) 3))
                      (ash (logxor 1 (if (eql x-prime 1) 1 0)) 2)
                      (apx-encode-pp pp))
              ;; P3: z L'L b/ND V' aaa/SCC/NF
              (if scc
                  (logior (ash (if (eql nd 1) 1 0) 4)
                          (logand scc #b1111))
                  (logior (ash z 7)
                          (ash ll 5)
                          (ash (if (plusp nd) nd evex-b) 4)
                          (ash (logxor 1 (if (eql v-prime 1) 1 0)) 3)
                          (if (plusp nf) (logior (ash nf 2) aaa) aaa)))))

(defun emit-apx-inst (segment thing reg opcode
                      &key (opcode-prefix 4)
                           (w 1)
                           vvvv
                           (pp 0)
                           (nd 0)
                           (nf 0)
                           scc
                           dfv
                           (remaining-bytes 0))
  (multiple-value-bind (r x b r-prime b-prime v-prime x-prime)
      (determine-apx-flags thing reg vvvv)
    (let ((vvvv-num (or (operand-reg-num vvvv) 0)))
      (emit-apx-evex segment r x b r-prime opcode-prefix w
                     vvvv-num pp 0 0 0 v-prime 0
                     :b-prime b-prime :x-prime x-prime :nd nd :nf nf :scc scc :dfv dfv)
      (emit-bytes segment opcode)
      (emit-ea segment thing reg :remaining-bytes remaining-bytes))))

;;; 3-Operand NDD ALU Emitter

(defun emit-apx-alu-ndd (segment dst src1 src2 subop &key (nf 0))
  (let* ((size (or (operand-size dst) (operand-size src1) (operand-size src2) :qword))
         (w (if (eq size :qword) 1 0))
         (pp (if (eq size :word) 1 0)))
    (cond ((integerp src2)
           (let* ((imm-size (if (and (typep src2 '(signed-byte 8)) (neq size :byte))
                                :byte
                                size))
                  (opcode (cond ((eq size :byte) #x80)
                                ((eq imm-size :byte) #x83)
                                (t #x81))))
             (emit-apx-inst segment src1 subop opcode :vvvv dst :nd 1 :nf nf :w w :pp pp
                            :remaining-bytes (size-nbyte imm-size))
             (emit-imm-operand segment src2 imm-size)))
          ((ea-p src2)
           (let ((opcode (if (eq size :byte)
                             (dpb subop (byte 3 3) 2)
                             (dpb subop (byte 3 3) 3))))
             (emit-apx-inst segment src2 src1 opcode :vvvv dst :nd 1 :nf nf :w w :pp pp)))
          (t
           (let ((opcode (if (eq size :byte)
                             (dpb subop (byte 3 3) 0)
                             (dpb subop (byte 3 3) 1))))
             (emit-apx-inst segment src1 src2 opcode :vvvv dst :nd 1 :nf nf :w w :pp pp))))))

;;; Unary NDD Emitter

(defun emit-apx-unary-ndd (segment dst src opcode subop &key (nf 0))
  (let* ((size (or (operand-size dst) (operand-size src) :qword))
         (w (if (eq size :qword) 1 0))
         (pp (if (eq size :word) 1 0)))
    (emit-apx-inst segment src subop (if (eq size :byte) (logand opcode #xFE) opcode)
                   :vvvv dst :nd 1 :nf nf :w w :pp pp)))

;;; Shift / Rotate NDD Emitter

(defun emit-apx-shift-ndd (segment dst src count subop &key (nf 0))
  (let* ((size (or (operand-size dst) (operand-size src) :qword))
         (w (if (eq size :qword) 1 0))
         (pp (if (eq size :word) 1 0)))
    (cond ((eq count :cl)
           (let ((opcode (if (eq size :byte) #xD2 #xD3)))
             (emit-apx-inst segment src subop opcode :vvvv dst :nd 1 :nf nf :w w :pp pp)))
          ((eql count 1)
           (let ((opcode (if (eq size :byte) #xD0 #xD1)))
             (emit-apx-inst segment src subop opcode :vvvv dst :nd 1 :nf nf :w w :pp pp)))
          ((integerp count)
           (let ((opcode (if (eq size :byte) #xC0 #xC1)))
             (emit-apx-inst segment src subop opcode :vvvv dst :nd 1 :nf nf :w w :pp pp
                            :remaining-bytes 1)
             (emit-byte segment (logand count #x3F)))))))

;;; Stack Pair Operations: PUSH2 / POP2

(define-instruction push2 (segment src1 src2)
  (:emitter
   (emit-apx-inst segment src2 6 #xFF :vvvv src1 :nd 1 :w 0)))

(define-instruction pop2 (segment dst1 dst2)
  (:emitter
   (emit-apx-inst segment dst2 0 #x8F :vvvv dst1 :nd 1 :w 0)))

;;; Conditional Operations: CFCMOVcc

(defconstant-eqx +apx-conditions+
    '((:o . 0) (:no . 1) (:b . 2) (:c . 2) (:nae . 2)
      (:nb . 3) (:nc . 3) (:ae . 3)
      (:z . 4) (:e . 4) (:nz . 5) (:ne . 5)
      (:be . 6) (:na . 6) (:nbe . 7) (:a . 7)
      (:s . 8) (:ns . 9) (:p . 10) (:pe . 10)
      (:np . 11) (:po . 11)
      (:l . 12) (:nge . 12) (:nl . 13) (:ge . 13)
      (:le . 14) (:ng . 14) (:nle . 15) (:g . 15))
  #'equalp)

(defun parse-apx-condition (cond)
  (if (numberp cond)
      (logand cond 15)
      (or (cdr (assoc cond +apx-conditions+ :test #'eq))
          (error "Unknown APX condition: ~S" cond))))

(define-instruction cfcmov (segment cond dst src)
  (:emitter
   (let* ((cc (parse-apx-condition cond))
          (opcode (+ #x40 cc))
          (size (or (operand-size dst) (operand-size src) :qword))
          (w (if (eq size :qword) 1 0))
          (pp (if (eq size :word) 1 0)))
     (emit-apx-inst segment src dst opcode :w w :pp pp :nd 0))))

;;; Condition Testing: CCMP / CTEST

(defun parse-dfv (dfv)
  (if (numberp dfv)
      (logand dfv 15)
      (let ((v 0))
        (dolist (f dfv v)
          (case f
            ((:cf :c) (setf v (logior v 1)))
            ((:zf :z) (setf v (logior v 2)))
            ((:sf :s) (setf v (logior v 4)))
            ((:of :o) (setf v (logior v 8))))))))

(define-instruction ccmp (segment cond op1 op2 &optional (dfv 0))
  (:emitter
   (let* ((cc (parse-apx-condition cond))
          (dfv-val (parse-dfv dfv))
          (size (or (operand-size op1) (operand-size op2) :qword))
          (w (if (eq size :qword) 1 0))
          (pp (if (eq size :word) 1 0)))
     (cond ((integerp op2)
            (let* ((imm-size (if (and (typep op2 '(signed-byte 8)) (neq size :byte))
                                 :byte
                                 size))
                   (opcode (cond ((eq size :byte) #x80)
                                 ((eq imm-size :byte) #x83)
                                 (t #x81))))
              (emit-apx-inst segment op1 7 opcode :w w :pp pp :scc cc :dfv dfv-val
                             :remaining-bytes (size-nbyte imm-size))
              (emit-imm-operand segment op2 imm-size)))
           (t
            (let ((opcode (if (eq size :byte) #x38 #x39)))
              (emit-apx-inst segment op1 op2 opcode :w w :pp pp :scc cc :dfv dfv-val)))))))

(define-instruction ctest (segment cond op1 op2 &optional (dfv 0))
  (:emitter
   (let* ((cc (parse-apx-condition cond))
          (dfv-val (parse-dfv dfv))
          (size (or (operand-size op1) (operand-size op2) :qword))
          (w (if (eq size :qword) 1 0))
          (pp (if (eq size :word) 1 0)))
     (cond ((integerp op2)
            (let* ((opcode (if (eq size :byte) #xF6 #xF7))
                   (imm-size (if (eq size :byte) :byte (if (eq size :qword) :dword size))))
              (emit-apx-inst segment op1 0 opcode :w w :pp pp :scc cc :dfv dfv-val
                             :remaining-bytes (size-nbyte imm-size))
              (emit-imm-operand segment op2 imm-size)))
           (t
            (let ((opcode (if (eq size :byte) #x84 #x85)))
              (emit-apx-inst segment op1 op2 opcode :w w :pp pp :scc cc :dfv dfv-val)))))))

;;; 3-Operand NDD ALU Instructions

(macrolet ((def-alu-ndd (name subop)
             `(define-instruction ,name (segment dst src1 src2 &key (nf 0))
                (:emitter (emit-apx-alu-ndd segment dst src1 src2 ,subop :nf nf)))))
  (def-alu-ndd add-ndd #b000)
  (def-alu-ndd or-ndd  #b001)
  (def-alu-ndd adc-ndd #b010)
  (def-alu-ndd sbb-ndd #b011)
  (def-alu-ndd and-ndd #b100)
  (def-alu-ndd sub-ndd #b101)
  (def-alu-ndd xor-ndd #b110))

(macrolet ((def-unary-ndd (name opcode subop)
             `(define-instruction ,name (segment dst src &key (nf 0))
                (:emitter (emit-apx-unary-ndd segment dst src ,opcode ,subop :nf nf)))))
  (def-unary-ndd not-ndd #xF7 #b010)
  (def-unary-ndd neg-ndd #xF7 #b011)
  (def-unary-ndd inc-ndd #xFF #b000)
  (def-unary-ndd dec-ndd #xFF #b001))

(macrolet ((def-shift-ndd (name subop)
             `(define-instruction ,name (segment dst src count &key (nf 0))
                (:emitter (emit-apx-shift-ndd segment dst src count ,subop :nf nf)))))
  (def-shift-ndd rol-ndd #b000)
  (def-shift-ndd ror-ndd #b001)
  (def-shift-ndd shl-ndd #b100)
  (def-shift-ndd shr-ndd #b101)
  (def-shift-ndd sar-ndd #b111))

(define-instruction imul-ndd (segment dst src1 src2)
  (:emitter
   (let* ((size (or (operand-size dst) (operand-size src1) :qword))
          (w (if (eq size :qword) 1 0))
          (pp (if (eq size :word) 1 0)))
     (cond ((integerp src2)
            (let ((imm-size (if (typep src2 '(signed-byte 8)) :byte size)))
              (emit-apx-inst segment src1 0 (if (eq imm-size :byte) #x6B #x69)
                             :vvvv dst :nd 1 :w w :pp pp
                             :remaining-bytes (size-nbyte imm-size))
              (emit-imm-operand segment src2 imm-size)))
           (t
            (emit-apx-inst segment src1 src2 #xAF :vvvv dst :nd 1 :w w :pp pp
                           :opcode-prefix 7))))))
