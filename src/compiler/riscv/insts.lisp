;;;; that part of the description of the RISC-V instruction set which
;;;; can live on the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-RISCV-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from SB-VM into this package
  (import '(sb-vm::u-and-i-inst-immediate
            sb-vm::lip-tn
            sb-vm::zero-tn
            sb-vm::null-tn
            sb-vm::short-immediate
            sb-vm::short-immediate-fixnum
            sb-vm::u+i-immediate)))


;;;; disassembler field definitions

(define-arg-type reg :printer #'print-reg)
(define-arg-type s-imm :printer #'print-s-imm)
(define-arg-type fp-reg :printer #'print-fp-reg)
(define-arg-type control-reg :printer "(CR:#x~X)")
(define-arg-type fence-ordering :printer #'print-fence-ordering)
(define-arg-type a-ordering :printer #'print-a-ordering)
(define-arg-type float-fmt :printer #'print-float-fmt)
(define-arg-type float-rm :printer #'print-float-rm)
;; We don't use :sign-extend, since the immediate fields are hairy.
(define-arg-type relative-b-label :use-label #'use-b-label)
(define-arg-type relative-j-label :use-label #'use-j-label)
(define-arg-type load-annotation :printer #'print-load-annotation)
(define-arg-type store-annotation :printer #'print-store-annotation)
(define-arg-type jalr-annotation :printer #'print-jalr-annotation)


(define-instruction byte (segment byte)
  (:emitter (emit-byte segment byte)))

(define-bitfield-emitter emit-word 32
  (byte 32 0))

(define-instruction word (segment word)
  (:emitter
   (etypecase word
     (fixup
      (note-fixup segment :absolute word)
      (emit-word segment 0))
     (integer
      (emit-word segment word)))))

(define-bitfield-emitter emit-machine-word n-machine-word-bits
  (byte n-machine-word-bits 0))

(define-instruction machine-word (segment word)
  (:emitter
   (etypecase word
     (fixup
      (note-fixup segment :absolute word)
      (emit-machine-word segment 0))
     (integer
      (emit-machine-word segment word)))))

(defconstant-eqx reg-printer
    '(:name :tab rd ", " rs1 ", " rs2)
  #'equalp)

(define-instruction-format (r 32 :default-printer reg-printer)
  (funct7 :field (byte 7 25))
  (rs2 :field (byte 5 20) :type 'reg)
  (rs1 :field (byte 5 15) :type 'reg)
  (funct3 :field (byte 3 12))
  (rd :field (byte 5 7) :type 'reg)
  (opcode :field (byte 7 0)))

(define-bitfield-emitter %emit-r-inst 32
  (byte 7 25) (byte 5 20) (byte 5 15) (byte 3 12) (byte 5 7) (byte 7 0))
(defun emit-r-inst (segment funct7 rs2 rs1 funct3 rd opcode)
  (%emit-r-inst segment funct7 (tn-offset rs2) (tn-offset rs1) funct3 (tn-offset rd) opcode))

(defconstant-eqx i-printer
    '(:name :tab rd ", " rs1 ", " imm)
  #'equalp)

(define-instruction-format (i 32 :default-printer i-printer)
  (i-annotation :fields (list (byte 5 15) (byte 12 20)))
  (l/a :field (byte 1 30))
  (shamt :field (byte (integer-length n-word-bits) 20))
  (imm :field (byte 12 20) :sign-extend t)
  (rs1 :field (byte 5 15) :type 'reg)
  (funct3 :field (byte 3 12))
  (rd :field (byte 5 7) :type 'reg)
  (opcode :field (byte 7 0)))

(define-bitfield-emitter %emit-i-inst 32
  (byte 12 20) (byte 5 15) (byte 3 12) (byte 5 7) (byte 7 0))
(defun emit-i-inst (segment imm rs1 funct3 rd opcode)
  (etypecase imm
    (short-immediate
     (%emit-i-inst segment imm (tn-offset rs1) funct3 (tn-offset rd) opcode))
    (fixup
     (note-fixup segment :i-type imm)
     (%emit-i-inst segment 0 (tn-offset rs1) funct3 (tn-offset rd) opcode))))

(defconstant-eqx s-printer
    '(:name :tab rs2 ", " "(" imm ")" rs1)
  #'equalp)

(define-instruction-format (s 32 :default-printer s-printer)
  (store-annotation :fields (list (byte 5 15) (byte 7 25) (byte 5 7)) :type 'store-annotation)
  (imm :fields (list (byte 7 25) (byte 5 7)) :type 's-imm)
  (rs2 :field (byte 5 20) :type 'reg)
  (rs1 :field (byte 5 15) :type 'reg)
  (funct3 :field (byte 3 12))
  (opcode :field (byte 7 0)))

(define-bitfield-emitter %emit-s-inst 32
  (byte 7 25) (byte 5 20) (byte 5 15) (byte 3 12) (byte 5 7) (byte 7 0))
(defun emit-s-inst (segment imm rs2 rs1 funct3 opcode)
  (etypecase imm
    (short-immediate
     (%emit-s-inst segment (ldb (byte 7 5) imm) (tn-offset rs2) (tn-offset rs1) funct3 (ldb (byte 5 0) imm) opcode))
    (fixup
     (note-fixup segment :s-type imm)
     (%emit-s-inst segment 0 (tn-offset rs2) (tn-offset rs1) funct3 0 opcode))))

(defconstant-eqx cond-branch-printer
  '(:name :tab rs1 ", " rs2 ", " imm)
  #'equalp)

(define-instruction-format (b 32 :default-printer cond-branch-printer)
  (imm :fields (list (byte 1 31) (byte 1 7) (byte 6 25) (byte 4 8)) :type 'relative-b-label)
  (rs2 :field (byte 5 20) :type 'reg)
  (rs1 :field (byte 5 15) :type 'reg)
  (funct3 :field (byte 3 12))
  (opcode :field (byte 7 0)))

(define-bitfield-emitter %emit-b-inst 32
  (byte 1 31) (byte 6 25) (byte 5 20) (byte 5 15) (byte 3 12) (byte 4 8) (byte 1 7) (byte 7 0))
(defun emit-b-inst (segment imm rs2 rs1 funct3 opcode)
  (aver (not (logbitp 0 imm)))
  (%emit-b-inst segment (ldb (byte 1 12) imm) (ldb (byte 6 5) imm)
                (tn-offset rs2) (tn-offset rs1) funct3 (ldb (byte 4 1) imm)
                (ldb (byte 1 11) imm) opcode))

(defconstant-eqx u-printer
    '(:name :tab rd ", " imm)
  #'equalp)

(define-instruction-format (u 32 :default-printer u-printer)
  (imm :field (byte 20 12) :printer "#x~5,'0X")
  (rd :field (byte 5 7) :type 'reg)
  (opcode :field (byte 7 0)))

(define-bitfield-emitter %emit-u-inst 32
  (byte 20 12) (byte 5 7) (byte 7 0))
(defun emit-u-inst (segment imm rd opcode)
  (etypecase imm
    (integer
     (%emit-u-inst segment imm (tn-offset rd) opcode))
    (fixup
     (note-fixup segment :u-type imm)
     (%emit-u-inst segment 0 (tn-offset rd) opcode))))

(defconstant-eqx j-printer
  '(:name :tab rd ", " imm)
  #'equalp)

(define-instruction-format (j 32 :default-printer j-printer)
  (imm :fields (list (byte 1 31) (byte 8 12) (byte 1 20) (byte 10 21)) :type 'relative-j-label)
  (rd :field (byte 5 7) :type 'reg)
  (opcode :field (byte 7 0)))

(define-bitfield-emitter %emit-j-inst 32
  (byte 1 31) (byte 10 21) (byte 1 20) (byte 8 12) (byte 5 7) (byte 7 0))
(defun emit-j-inst (segment imm rd opcode)
  (aver (not (logbitp 0 imm)))
  (%emit-j-inst segment (ldb (byte 1 20) imm) (ldb (byte 10 1) imm)
                (ldb (byte 1 11) imm) (ldb (byte 8 12) imm)
                (tn-offset rd) opcode))

(define-instruction lui (segment rd ui)
  (:printer u ((opcode #b0110111)))
  (:emitter
   (emit-u-inst segment ui rd #b0110111)))

(define-instruction auipc (segment rd ui)
  (:printer u ((opcode #b0010111)))
  (:emitter
   (emit-u-inst segment ui rd #b0010111)))

;;;; Branch and Jump instructions.

(defun relative-offset (target posn &optional delta-if-after)
  (- (label-position target (and delta-if-after posn) delta-if-after) posn))

(defun emit-short-jump-at (segment lr target posn)
  (declare (ignore posn))
  (emit-back-patch
   segment 4
   (lambda (segment posn)
     (emit-j-inst segment (relative-offset target posn) lr #b1101111))))

(defconstant-eqx jalr-printer
    '(:name :tab rd ", " rs1 ", " imm i-annotation)
  #'equal)

(define-instruction jalr (segment lr rs offset)
  (:printer i ((funct3 #b000)
               (opcode #b1100111)
               (i-annotation nil :type 'jalr-annotation))
            jalr-printer)
  (:emitter
   (emit-i-inst segment offset rs #b000 lr #b1100111)))

;;; For unconditional jumps, we either emit a one instruction or two
;;; instruction sequence.
(define-instruction jal (segment lr target)
  (:printer j ((opcode #b1101111)))
  (:emitter
   (typecase target
     (fixup
      (assemble (segment)
        (inst lui lip-tn target)
        (inst jalr lr lip-tn target)))
     (t
      (emit-chooser
       segment 8 2
       (lambda (segment chooser posn delta-if-after)
         (declare (ignore chooser))
         (when (typep (ash (relative-offset target posn delta-if-after) -1)
                      '(signed-byte 20))
           (emit-short-jump-at segment lr target posn)
           t))
       (emit-long-jump-at-fun lr target))))))

(defun emit-long-jump-at-fun (lr target)
  (lambda (segment posn)
    (declare (ignore posn))
    (emit-back-patch
     segment 8
     (lambda (segment posn)
       (multiple-value-bind (hi lo)
           (u-and-i-inst-immediate (relative-offset target posn))
         (assemble (segment)
           (inst auipc lip-tn hi)
           (inst jalr lr lip-tn lo)))))))

(define-instruction-macro j (target)
  `(inst jal zero-tn ,target))

(defun emit-relative-branch (segment opcode funct3 rs1 rs2 target)
  (emit-chooser
   segment 12 2
   (lambda (segment chooser posn delta-if-after)
     (declare (ignore chooser))
     (typecase (ash (relative-offset target posn delta-if-after) -1)
       (short-immediate
        (emit-back-patch
         segment 4
         (lambda (segment posn)
           (emit-b-inst segment (relative-offset target posn) rs2 rs1 funct3 opcode)))
        t)
       ((signed-byte 20)
        ;; Emit the sequence:
        ;; b(invert(funct3)) rs1 rs2 8
        ;; jal x0 target
        (emit-b-inst segment 8 rs2 rs1 (logxor funct3 1) opcode)
        (emit-short-jump-at segment zero-tn target posn)
        t)))
   (lambda (segment posn)
     (emit-b-inst segment 12 rs2 rs1 (logxor funct3 1) opcode)
     (funcall (emit-long-jump-at-fun zero-tn target) segment posn))))

(macrolet ((define-branch-instruction (name funct3)
             `(define-instruction ,name (segment rs1 rs2 offset)
                (:printer b ((funct3 ,funct3) (opcode #b1100011)))
                (:dependencies (writes lip-tn))
                (:emitter
                 (emit-relative-branch segment #b1100011 ,funct3 rs1 rs2 offset)))))
  (define-branch-instruction beq #b000)
  (define-branch-instruction bne #b001)
  (define-branch-instruction blt #b100)
  (define-branch-instruction bge #b101)
  (define-branch-instruction bltu #b110)
  (define-branch-instruction bgeu #b111))

(macrolet ((define-load-instruction (name funct3 &optional wordp)
             `(define-instruction ,name (segment rd rs offset)
                (:printer i
                          ((funct3 ,funct3)
                           (opcode #b0000011)
                           (i-annotation nil :type 'load-annotation))
                          '(:name :tab rd ", (" imm ")" rs1
                            ,(when wordp 'i-annotation)))
                (:emitter
                 (emit-i-inst segment offset rs ,funct3 rd #b0000011)))))
  (define-load-instruction lb #b000)
  (define-load-instruction lh #b001)
  (define-load-instruction lw #b010 #-64-bit t)
  #+64-bit
  (progn
    (define-load-instruction ld #b011 t)
    (define-load-instruction lwu #b110))
  (define-load-instruction lbu #b100)
  (define-load-instruction lhu #b101))

(macrolet ((define-store-instruction (name funct3 &optional wordp)
             `(define-instruction ,name (segment rs2 rs1 offset)
                (:printer s ((funct3 ,funct3) (opcode #b0100011))
                          '(:name :tab rs2 ", " "(" imm ")" rs1
                            ,(when wordp 'store-annotation)))
                (:emitter
                 (emit-s-inst segment offset rs2 rs1 ,funct3 #b0100011)))))
  (define-store-instruction sb #b000)
  (define-store-instruction sh #b001)
  (define-store-instruction sw #b010 #-64-bit t)
  #+64-bit
  (define-store-instruction sd #b011 t))

(macrolet ((define-immediate-arith-instruction (name funct3 &optional word-name)
             `(progn
                (define-instruction ,name (segment rd rs imm)
                  (:printer i ((funct3 ,funct3)
                               (opcode #b0010011)))
                  (:emitter
                   (emit-i-inst segment imm rs ,funct3 rd #b0010011)))
                ,(when word-name
                   #+64-bit
                   `(define-instruction ,word-name (segment rd rs imm)
                      (:printer i ((funct3 ,funct3)
                                   (opcode #b0011011)))
                      (:emitter
                       (emit-i-inst segment imm rs ,funct3 rd #b0011011))))))
           (define-immediate-shift-instruction (name funct3 l/a word-name)
             `(progn
                (define-instruction ,name (segment rd rs imm)
                  (:printer i ((funct3 ,funct3)
                               (opcode #b0010011)
                               (l/a ,l/a))
                            '(:name :tab rd ", " rs1 ", " shamt))
                  (:emitter
                   (aver (< imm ,n-word-bits))
                   ,@(when (= l/a 1)
                       `((setf (ldb (byte 1 10) imm) 1)))
                   (emit-i-inst segment imm rs ,funct3 rd #b0010011)))
                ,(when word-name
                   #+64-bit
                   `(define-instruction ,word-name (segment rd rs imm)
                      (:printer i ((funct3 ,funct3)
                                   (opcode #b0011011)
                                   (l/a ,l/a))
                                '(:name :tab rd ", " rs1 ", " shamt))
                      (:emitter
                       (aver (< imm ,n-word-bits))
                       ,@(when (= l/a 1)
                           `((setf (ldb (byte 1 10) imm) 1)))
                       (emit-i-inst segment imm rs ,funct3 rd #b0011011)))))))
  (define-immediate-arith-instruction addi #b000 addiw)
  (define-immediate-arith-instruction slti #b010)
  (define-immediate-arith-instruction sltiu #b011)
  (define-immediate-arith-instruction xori #b100)
  (define-immediate-arith-instruction ori #b110)
  (define-immediate-arith-instruction andi #b111)
  (define-immediate-shift-instruction slli #b001 0 slliw)
  (define-immediate-shift-instruction srli #b101 0 srliw)
  (define-immediate-shift-instruction srai #b101 1 sraiw))

(define-instruction-macro subi (rd rs imm)
  `(inst addi ,rd ,rs (- ,imm)))

(defmacro define-register-arith-instruction (name funct7 funct3 opcode)
  `(define-instruction ,name (segment rd rs1 rs2)
     (:printer r ((funct7 ,funct7) (funct3 ,funct3) (opcode ,opcode)))
     (:emitter
      (emit-r-inst segment ,funct7 rs2 rs1 ,funct3 rd ,opcode))))

(macrolet ((define-riscvi-arith-instruction (name funct7 funct3 &optional word-variant)
             `(progn
                (define-register-arith-instruction ,name ,funct7 ,funct3 #b0110011)
                ,(when word-variant
                   #+64-bit
                  `(define-register-arith-instruction ,word-variant ,funct7 ,funct3 #b0111011)))))
  (define-riscvi-arith-instruction add #b0000000 #b000 addw)
  (define-riscvi-arith-instruction sub #b0100000 #b000 subw)
  (define-riscvi-arith-instruction sll #b0000000 #b001 sllw)
  (define-riscvi-arith-instruction slt #b0000000 #b010)
  (define-riscvi-arith-instruction sltu #b0000000 #b011)
  (define-riscvi-arith-instruction xor #b0000000 #b100)
  (define-riscvi-arith-instruction srl #b0000000 #b101 srlw)
  (define-riscvi-arith-instruction sra #b0100000 #b101 sraw)
  (define-riscvi-arith-instruction or #b0000000 #b110)
  (define-riscvi-arith-instruction and #b0000000 #b111))

(defun coerce-signed (unsigned-value width)
  (if (logbitp (1- width) unsigned-value)
      (dpb unsigned-value (byte (1- width) 0) -1)
      unsigned-value))

(defun coerce-unsigned (signed-value width)
  (if (minusp signed-value)
      (+ (ash 1 width) signed-value)
      signed-value))

(defun load-signed-byte-32-immediate (rd immediate)
  (multiple-value-bind (hi lo)
      (u-and-i-inst-immediate immediate)
    (cond ((zerop hi)
           (inst addi rd zero-tn immediate))
          (t
           (inst lui rd hi)
           (unless (zerop lo)
             (inst addi rd rd lo))))))

(defun %li (reg value)
  (etypecase value
    (u+i-immediate
     (load-signed-byte-32-immediate reg (coerce-signed value n-word-bits)))
    #+64-bit
    ((or (signed-byte 64) (unsigned-byte 64))
     ;; It would be better to use a dynamic programming approach here.
     (let* ((value (coerce-unsigned value 64))
            (integer-length (integer-length value))
            (2^k (ash 1 integer-length))
            (2^.k-1 (ash 1 (1- integer-length)))
            (complement (mod (lognot value) (ash 1 64))))
       (cond ((zerop (logand (1+ value) value))
              ;; Common special case: the immediate is of the form #xfff...
              (inst addi reg zero-tn -1)
              (unless (= integer-length 64)
                (inst srli reg reg (- 64 integer-length))))
             ((let ((delta (- 2^k value)))
                (and (typep (ash delta (- 64 integer-length)) 'short-immediate)
                     (logand delta (1- delta))))
              ;; Common special case: the immediate is of the form
              ;; #x00fff...00, where there are a small number of
              ;; zeroes at the end.
              (inst addi reg zero-tn (ash (- value 2^k) (- 64 integer-length)))
              (inst srli reg reg (- 64 integer-length)))
             ((zerop (logand complement (1+ complement)))
              ;; #xfffffff...00000
              (inst addi reg zero-tn -1)
              (inst slli reg reg (integer-length complement)))
             ((typep (- value 2^k) 'short-immediate)
              ;; Common special case: loading an immediate which is a
              ;; signed 12 bit constant away from a power of 2.
              (cond ((= integer-length 64)
                     (inst addi reg zero-tn (- value 2^k)))
                    (t
                     (inst addi reg zero-tn 1)
                     (inst slli reg reg integer-length)
                     (inst addi reg reg (- value 2^k)))))
             ((typep (- value 2^.k-1) 'short-immediate)
              ;; Common special case: loading an immediate which is a
              ;; signed 12 bit constant away from a power of 2.
              (inst addi reg zero-tn 1)
              (inst slli reg reg (1- integer-length))
              (unless (= value 2^.k-1)
                (inst addi reg reg (- value 2^.k-1))))
             (t
              ;; The "generic" case.
              ;; Load in the first 31 non zero most significant bits.
              (let ((chunk (ldb (byte 12 (- integer-length 31)) value)))
                (inst lui reg (ldb (byte 20 (- integer-length 19)) value))
                (cond ((= (1- (ash 1 12)) chunk)
                       (inst addi reg reg (1- (ash 1 11)))
                       (inst addi reg reg (1- (ash 1 11)))
                       (inst addi reg reg 1))
                      ((logbitp 11 chunk)
                       (inst addi reg reg (1- (ash 1 11)))
                       (inst addi reg reg (- chunk (1- (ash 1 11)))))
                      (t
                       (inst addi reg reg chunk))))
              ;; Now we need to load in the rest of the bits properly, in
              ;; chunks of 11 to avoid sign extension.
              (do ((i (- integer-length 31) (- i 11)))
                  ((< i 11)
                   (inst slli reg reg i)
                   (unless (zerop (ldb (byte i 0) value))
                     (inst addi reg reg (ldb (byte i 0) value))))
                (inst slli reg reg 11)
                (inst addi reg reg (ldb (byte 11 (- i 11)) value)))))))
    (fixup
     (inst lui reg value)
     (inst addi reg reg value))))

(define-instruction-macro li (reg value)
  `(%li ,reg ,value))

(defconstant-eqx fence-printer
    '(:name :tab pred ", " succ)
  #'equal)

(define-instruction-format (fence 32 :default-printer fence-printer)
  (funct4 :field (byte 4 28) :value #b0000)
  (pred :field (byte 4 24) :type 'fence-ordering)
  (succ :field (byte 4 20) :type 'fence-ordering)
  (rs1 :field (byte 5 15) :value #b00000)
  (funct3 :field (byte 3 12))
  (rd :field (byte 5 7) :value #b00000)
  (opcode :field (byte 7 0) :value #b0001111))

(defun fence-encoding (ops)
  (let ((vals '(:i 8 :o 4 :r 2 :w 1)))
    (etypecase ops
      ((unsigned-byte 4) ops)
      (list
       (let ((result 0))
         (dolist (op ops result)
           (setq result (logior result (getf vals op))))))
      ((or string symbol)
       (let ((ops (string ops))
             (result 0))
         (dovector (op ops result)
           (setq result (logior result (getf vals (keywordicate op))))))))))

(define-bitfield-emitter %emit-fence-inst 32
  (byte 4 28) (byte 4 24) (byte 4 20) (byte 5 15) (byte 3 12) (byte 5 7) (byte 7 0))
(defun emit-fence-inst (segment pred succ funct3)
  (%emit-fence-inst segment #b0000 (fence-encoding pred) (fence-encoding succ)
                    #b00000 funct3 #b00000 #b0001111))

(define-instruction fence (segment pred succ)
  (:printer fence ((funct3 #b000)))
  (:emitter
   (emit-fence-inst segment pred succ #b000)))
(define-instruction fence.i (segment)
  (:printer fence ((funct3 #b001)))
  (:emitter
   (emit-fence-inst segment #b0000 #b0000 #b001)))

(define-instruction-format (break 32 :default-printer '(:name :tab code))
  (opcode-32 :field (byte 32 0))
  ;; We use a prefilter in order to read trap codes in order to avoid
  ;; attempting to include the code in the decoded instruction proper
  ;; (requiring moving to a 40-bit instruction for disassembling trap
  ;; codes).
  (code :prefilter (lambda (dstate) (read-suffix 8 dstate))
        :reader trap-code))

(define-instruction ecall (segment)
  (:printer i ((imm #b000000000000) (rs1 #b00000) (funct3 #b000)
               (rd #b00000) (opcode #b1110011)))
  (:emitter
   (%emit-i-inst segment #b000000000000 #b00000 #b000 #b00000 #b1110011)))
(define-instruction ebreak (segment &optional (code nil codep))
  (:printer break ((opcode-32 #x100073))
            :default :control #'break-control)
  (:emitter
   (%emit-i-inst segment #b000000000001 #b00000 #b000 #b00000 #b1110011)
   (when codep (emit-byte segment code))))

(defun csr-encoding (kind)
  (ecase kind
    (:fflags #x001)
    (:frm #x002)
    (:fcsr #x003)))

(defun emit-csr-inst (segment csr funct3 rs rd)
  (%emit-i-inst segment (csr-encoding csr) (tn-offset rs) funct3 (tn-offset rd) #b1110011))

(defun emit-csr-i-inst (segment csr funct3 zimm rd)
  (%emit-i-inst segment (csr-encoding csr) zimm funct3 (tn-offset rd) #b1110011))

(macrolet ((define-csr-instruction (name funct3)
             `(define-instruction ,name (segment rd csr rs)
                (:printer i ((funct3 ,funct3) (opcode #b1110011)))
                (:emitter
                 (emit-csr-inst segment csr ,funct3 rs rd))))
           (define-csr-i-instruction (name funct3)
             `(define-instruction ,name (segment csr rd zimm)
                (:printer i ((funct3 ,funct3) (opcode #b1110011)))
                (:emitter
                 (emit-csr-i-inst segment csr ,funct3 zimm rd)))))
  (define-csr-instruction   csrrw  #b001)
  (define-csr-instruction   csrrs  #b010)
  (define-csr-instruction   csrrc  #b011)
  (define-csr-i-instruction csrrwi #b101)
  (define-csr-i-instruction csrrsi #b110)
  (define-csr-i-instruction csrrci #b111))

(define-instruction-macro csrr (rd csr) `(inst csrrs ,rd ,csr zero-tn))
(define-instruction-macro csrw (csr rs) `(inst csrrw zero-tn ,csr ,rs))
(define-instruction-macro csrs (csr rs) `(inst csrrs zero-tn ,csr ,rs))
(define-instruction-macro csrc (csr rs) `(inst csrrc zero-tn ,csr ,rs))
(define-instruction-macro csrwi (csr imm) `(inst csrrw zero-tn ,csr ,imm))
(define-instruction-macro csrsi (csr imm) `(inst csrrs zero-tn ,csr ,imm))
(define-instruction-macro csrci (csr imm) `(inst csrrc zero-tn ,csr ,imm))

(macrolet ((define-riscvm-arith-instruction (name funct3)
             `(define-register-arith-instruction ,name #b0000001 ,funct3 #b0110011)))
  (define-riscvm-arith-instruction mul #b000)
  (define-riscvm-arith-instruction mulh #b001)
  (define-riscvm-arith-instruction mulhsu #b010)
  (define-riscvm-arith-instruction mulhu #b011)
  (define-riscvm-arith-instruction div #b100)
  (define-riscvm-arith-instruction divu #b101)
  (define-riscvm-arith-instruction rem #b110)
  (define-riscvm-arith-instruction remu #b111))

(defun parse-atomic-flags (flags)
  (let ((aq 0) (rl 0))
    (dolist (flag flags)
      (ecase flag
        ((nil))
        (:aq (setq aq 1))
        (:rl (setq rl 1))))
    (values aq rl)))

(defconstant-eqx r-atomic-printer
    '(:name " " ordering :tab rd ", " rs2 ", " rs1)
  #'equal)

(define-instruction-format (r-atomic 32 :default-printer r-atomic-printer)
  (funct5 :field (byte 5 27))
  (ordering :field (byte 2 25) :type 'a-ordering)
  (rs2 :field (byte 5 20) :type 'reg)
  (rs1 :field (byte 5 15) :type 'reg)
  (funct3 :field (byte 3 12))
  (rd :field (byte 5 7) :type 'reg)
  (opcode :field (byte 7 0) :value #b0101111))

(defconstant-eqx lr-printer
    '(:name " " ordering :tab rd ", " rs1)
  #'equal)

(macrolet ((define-load-reserved-instruction (name funct3)
             `(define-instruction ,name (segment rd rs &optional flag1 flag2)
                (:printer r-atomic ((funct5 #b00010) (funct3 ,funct3))
                          lr-printer)
                (:emitter
                 (multiple-value-bind (aq rl)
                     (parse-atomic-flags (list flag1 flag2))
                   (emit-i-inst segment
                                (logior (ash #b00010 7)
                                        (ash aq 6)
                                        (ash rl 5))
                                rs ,funct3 rd #b0101111))))))
  (define-load-reserved-instruction lr.w #b010)
  (define-load-reserved-instruction lr.d #b011))

(define-instruction-macro lr (rd rs &optional flag1 flag2)
  `(inst #-64-bit lr.w #+64-bit lr.d ,rd ,rs ,flag1 ,flag2))

(macrolet ((%define-riscva-instruction (name funct5 funct3)
             `(define-instruction ,name (segment rd rs2 rs1 &optional flag1 flag2)
                (:printer r-atomic ((funct5 ,funct5) (funct3 ,funct3)))
                (:emitter
                 (multiple-value-bind (aq rl)
                     (parse-atomic-flags (list flag1 flag2))
                   (emit-r-inst segment
                                (logior (ash ,funct5 2)
                                        (ash aq 1)
                                        (ash rl 0))
                                rs2 rs1 ,funct3 rd #b0101111)))))
           (define-riscva-instruction (name funct5)
             (let ((w-name (symbolicate name ".W"))
                   (d-name (symbolicate name ".D")))
               `(progn
                  (%define-riscva-instruction ,w-name ,funct5 #b010)
                  (%define-riscva-instruction ,d-name ,funct5 #b011)
                  (define-instruction-macro ,name (rd rs2 rs1 &optional flag1 flag2)
                    `(inst #-64-bit ,',w-name #+64-bit ,',d-name
                           ,rd ,rs2 ,rs1 ,flag1 ,flag2))))))
  (define-riscva-instruction sc      #b00011)
  (define-riscva-instruction amoswap #b00001)
  (define-riscva-instruction amoadd  #b00000)
  (define-riscva-instruction amoxor  #b00100)
  (define-riscva-instruction amoand  #b01100)
  (define-riscva-instruction amoor   #b01000)
  (define-riscva-instruction amomin  #b10000)
  (define-riscva-instruction amomax  #b10100)
  (define-riscva-instruction amominu #b11000)
  (define-riscva-instruction amomaxu #b11100))

;;; Floating point
(defconstant-eqx r-float-printer
    '(:name fmt :tab rd ", " rs1 ", " rs2 ", " rm)
  #'equal)

(defconstant-eqx r-float-unop-printer
    '(:name fmt :tab rd ", " rs1 ", " rm)
  #'equal)

(define-instruction-format (r-float 32 :default-printer r-float-printer)
  (rs3/funct5 :field (byte 5 27))
  (fmt :field (byte 2 25) :type 'float-fmt)
  (rs2 :field (byte 5 20) :type 'fp-reg)
  (rs1 :field (byte 5 15) :type 'fp-reg)
  (rm :field (byte 3 12) :type 'float-rm)
  (rd :field (byte 5 7) :type 'fp-reg)
  (opcode :field (byte 7 0)))

(defun ensure-tn-offset (imm/tn)
  (etypecase imm/tn
    ((integer 0 31) imm/tn)
    (tn (tn-offset imm/tn))))

(defun emit-r-float-inst (segment rs3/funct5 fmt rs2 rs1 rm rd opcode)
  (%emit-r-inst segment
                (dpb (ensure-tn-offset rs3/funct5)
                     (byte 5 2)
                     (ecase fmt
                       (:single #b00)
                       (:double #b01)
                       (:quad #b10)))
                (ensure-tn-offset rs2)
                (tn-offset rs1)
                rm
                (tn-offset rd)
                opcode))

(defun rm-encoding (rm)
  (or (position rm #(:rne :rtz :rdn :rup :rmm :unused1 :unused2 :dynamic))
      (error "Invalid rounding mode mnemonic ~a." rm)))

(macrolet ((define-riscv-float-arith-instruction
               (name funct5 &key rm rs2)
             `(define-instruction ,name
                  (segment fmt rd rs1
                           ,@(unless rs2 '(rs2))
                           ,@(unless rm '(&optional (rm :rne))) )
                (:printer r-float ((rs3/funct5 ,funct5)
                                   ,@(when rs2 `((rs2 ,rs2)))
                                   ,@(when rm `((rm ,rm)))
                                   (opcode #b1010011))
                          ,@(when rs2 '(r-float-unop-printer)))
                (:emitter
                 (emit-r-float-inst segment ,funct5 fmt
                                    ,(if rs2 rs2 'rs2) rs1
                                    ,(if rm rm '(rm-encoding rm))
                                    rd #b1010011)))))
  (define-riscv-float-arith-instruction fadd     #b00000)
  (define-riscv-float-arith-instruction fsub     #b00001)
  (define-riscv-float-arith-instruction fmul     #b00010)
  (define-riscv-float-arith-instruction fdiv     #b00011)
  (define-riscv-float-arith-instruction fsqrt    #b01011 :rs2 #b00000)
  (define-riscv-float-arith-instruction fsgnj    #b00100 :rm  #b000)
  (define-riscv-float-arith-instruction fsgnjn   #b00100 :rm  #b001)
  (define-riscv-float-arith-instruction fsgnjx   #b00100 :rm  #b010)
  (define-riscv-float-arith-instruction fmin     #b00101 :rm  #b000)
  (define-riscv-float-arith-instruction fmax     #b00101 :rm  #b001)
  (define-riscv-float-arith-instruction fcvtw<-  #b11000 :rs2 #b00000)
  (define-riscv-float-arith-instruction fcvtwu<- #b11000 :rs2 #b00001)
  #+64-bit
  (define-riscv-float-arith-instruction fcvtl<-  #b11000 :rs2 #b00010)
  #+64-bit
  (define-riscv-float-arith-instruction fcvtlu<- #b11000 :rs2 #b00011)
  (define-riscv-float-arith-instruction fmvx<-   #b11100 :rs2 #b00000 :rm #b000)
  (define-riscv-float-arith-instruction feq      #b10100 :rm  #b010)
  (define-riscv-float-arith-instruction flt      #b10100 :rm  #b001)
  (define-riscv-float-arith-instruction fle      #b10100 :rm  #b000)
  (define-riscv-float-arith-instruction fclass   #b11100 :rs2 #b00000 :rm #b001)
  (define-riscv-float-arith-instruction fcvtw->  #b11010 :rs2 #b00000)
  (define-riscv-float-arith-instruction fcvtwu-> #b11010 :rs2 #b00001)
  #+64-bit
  (define-riscv-float-arith-instruction fcvtl->  #b11010 :rs2 #b00010)
  #+64-bit
  (define-riscv-float-arith-instruction fcvtlu-> #b11010 :rs2 #b00011)
  (define-riscv-float-arith-instruction fmvx->   #b11110 :rs2 #b00000 :rm #b000)
  (define-riscv-float-arith-instruction fcvtd->  #b01000 :rs2 #b00001)
  (define-riscv-float-arith-instruction fcvts->  #b01000 :rs2 #b00000))

(macrolet ((define-3-arg-float-arith-instruction (name opcode)
             `(define-instruction ,name (segment fmt rd rs1 rs2 rs3 &optional (rm :rne))
                (:printer r-float ((opcode ,opcode)))
                (:emitter
                 (emit-r-float-inst segment rs3 fmt rs2 rs1 (rm-encoding rm) rd ,opcode)))))
  (define-3-arg-float-arith-instruction fmadd #b1000011)
  (define-3-arg-float-arith-instruction fmsub #b1000111)
  (define-3-arg-float-arith-instruction fnmsub #b1001011)
  (define-3-arg-float-arith-instruction fnmadd #b1001111))

(macrolet ((def (name op)
             `(define-instruction-macro ,name (format dst src)
                `(inst ,',op ,format ,dst ,src ,src))))
  (def fmove fsgnj)
  (def fneg fsgnjn)
  (def fabs fsgnjx))

(define-instruction-macro fcvt (to-format from-format dst src &optional (rm :rne))
  (case to-format
    (:word `(inst #-64-bit fcvtw<- #+64-bit fcvtl<- ,from-format ,dst ,src ,rm))
    (:unsigned-word `(inst #-64-bit fcvtwu<- #+64-bit fcvtlu<- ,from-format ,dst ,src ,rm))
    (otherwise
     `(inst ,(ecase from-format
               (:double 'fcvtd->)
               (:single 'fcvts->)
               (:word #-64-bit 'fcvtw-> #+64-bit 'fcvtl->)
               (:unsigned-word #-64-bit 'fcvtwu-> #+64-bit 'fcvtlu->))
            ,to-format ,dst ,src ,rm))))

(labels ((fmt-funct3 (fmt)
           (ecase fmt
             (:single #b010)
             (:double #b011)))
         (funct3-fmt (funct3)
           (case funct3
             (#b010 's)
             (#b011 'd)
             (t '?)))
         (funct3-printer (funct3 stream dstate)
           (declare (ignore dstate))
           (princ (funct3-fmt funct3)
                  stream)))
  (define-instruction fload (segment fmt rd rs offset)
    (:printer i ((opcode #b0000111)
                 (rd nil :type 'fp-reg)
                 (rs1 nil :type 'reg)
                 (funct3 nil :printer #'funct3-printer)
                 (imm nil :sign-extend t))
              '(:name funct3 :tab rd ", (" imm ")" rs1))
    (:emitter
     (emit-i-inst segment offset rs (fmt-funct3 fmt) rd #b0000111)))

  (define-instruction fstore (segment fmt rs1 rs2 offset)
    (:printer s ((opcode #b0100111)
                 (rs1 nil :type 'reg)
                 (rs2 nil :type 'fp-reg)
                 (funct3 nil :printer #'funct3-printer)
                 (imm nil :type 's-imm))
              '(:name funct3 :tab rs2 ", " "(" imm ")" rs1))
    (:emitter
     (emit-s-inst segment offset rs1 rs2 (fmt-funct3 fmt) #b0100111))))

;;;; Boxed-object computation instructions (for LRA and CODE)

;;; Try to compute DEST from SRC if possible. Otherwise, fall back to
;;; using a PC relative calculation as the worst case.
(defun emit-compute (segment vop dest lip pc-relative-delta src-relative-delta &optional src)
  (labels ((pc-relative-emitter (segment position)
             (multiple-value-bind (u i)
                 (u-and-i-inst-immediate (funcall pc-relative-delta position))
               (assemble (segment vop)
                 (inst auipc lip u)
                 (inst addi dest lip i))))
           (src-relative-emitter (segment position)
             (assemble (segment vop)
               (inst addi dest src (funcall src-relative-delta position))))
           (maybe-shrink (segment chooser position magic-value)
             (declare (ignore chooser))
             (when (and src
                        (typep (funcall src-relative-delta position magic-value)
                               'short-immediate))
               (emit-back-patch segment 4 #'src-relative-emitter)
               t)))
    (emit-chooser
     segment 8 2
     #'maybe-shrink
     #'pc-relative-emitter)))

;;; FIXME: Could potentially optimize away an instruction in
;;; XEP-ALLOCATE-FRAME in some cases when the code can be computed off
;;; of the register used to call the function, like MIPS. Probably
;;; requires always using LR as a lip tn though. Also, if the return
;;; register is fixed, could compute code in one instruction in values
;;; receiving routines.
(define-instruction compute-code (segment code lip label &optional src)
  (:vop-var vop)
  (:emitter
   (emit-compute segment vop code lip
                 (lambda (position &optional magic-value)
                   (declare (ignore magic-value))
                   (- other-pointer-lowtag
                      position
                      (component-header-length)))
                 ;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tagged
                 ;;      = lra - (header + label-offset)
                 (lambda (position &optional (magic-value 0))
                   (- (+ (label-position label position magic-value)
                         (component-header-length))))
                 src)))

(define-instruction compute-lra (segment dest lip lra-label &optional src)
  (:vop-var vop)
  (:emitter
   (emit-compute segment vop dest lip
                 (lambda (position &optional magic-value)
                   (- (+ (label-position lra-label
                                         (when magic-value position)
                                         magic-value)
                         other-pointer-lowtag)
                      position))
                 ;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
                 ;;     = code + header + label-offset
                 (lambda (position &optional (magic-value 0))
                   (+ (label-position lra-label position magic-value)
                      (component-header-length)))
                 src)))

(defun emit-header-data (segment type)
  (emit-back-patch
   segment n-word-bytes
   #'(lambda (segment posn)
       (emit-machine-word segment
                          (logior type
                                  (ash (+ posn (component-header-length))
                                       (- n-widetag-bits word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  (:emitter
   (emit-header-data segment simple-fun-widetag)))

(define-instruction lra-header-word (segment)
  (:emitter
   (emit-header-data segment return-pc-widetag)))

(define-instruction-macro load-layout-id (reg layout)
  `(progn (inst .layout-id-fixup ,layout)
          (inst lui ,reg #xfffff)
          (inst addi ,reg ,reg -1)))

(define-instruction .layout-id-fixup (segment layout)
 (:emitter (sb-c:note-fixup segment :u+i-type (sb-c:make-fixup layout :layout-id))))

(defun sb-vm:fixup-code-object (code offset value kind flavor)
  (declare (type index offset))
  (unless (zerop (rem offset sb-assem:+inst-alignment-bytes+))
    (error "Unaligned instruction?  offset=#x~X." offset))
  #+64-bit
  (unless (typep value 'u+i-immediate)
    (error "Tried to fixup with ~a." value))
  (let ((sap (code-instructions code)))
    (multiple-value-bind (u i) (u-and-i-inst-immediate value)
      (ecase kind
        (:absolute
         (setf (sap-ref-32 sap offset) value))
        (:u-type
         (setf (ldb (byte 20 12) (sap-ref-32 sap offset)) u))
        (:i-type
         (setf (ldb (byte 12 20) (sap-ref-32 sap offset)) i))
        (:u+i-type
         (sb-vm:fixup-code-object code offset u :u-type flavor)
         (sb-vm:fixup-code-object code (+ offset 4) i :i-type flavor))
        (:s-type
         (setf (ldb (byte 5 7) (sap-ref-32 sap offset))
               (ldb (byte 5 0) i))
         (setf (ldb (byte 7 25) (sap-ref-32 sap offset))
               (ldb (byte 7 5) i))))))
   nil)

(define-instruction store-coverage-mark (segment path-index)
  (:emitter
   ;; No backpatch is needed to compute the offset into the code header
   ;; because COMPONENT-HEADER-LENGTH is known at this point.
   (let ((offset (+ (component-header-length)
                    n-word-bytes ; skip over jump table word
                    path-index
                    (- other-pointer-lowtag))))
     (inst* segment 'sb sb-vm::null-tn sb-vm::code-tn
            (the (unsigned-byte 15) offset)))))
