;;;; that part of the description of the RV32 instruction set which
;;;; can live on the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-RV32-ASM")

(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

(define-instruction-format (r 32)
  (funct7 :field (byte 7 25))
  (rs2 :field (byte 5 20))
  (rs1 :field (byte 5 15))
  (funct3 :field (byte 3 12))
  (rd :field (byte 5 7))
  (opcode (byte 7 0)))

(define-bitfield-emitter %emit-r-inst 32
  (byte 7 25) (byte 5 20) (byte 5 15) (byte 3 12) (byte 5 7) (byte 7 0))
(defun emit-r-inst (segment funct7 rs2 rs1 funct3 rd opcode)
  (%emit-r-inst segment funct7 (tn-offset rs2) (tn-offset rs1) funct3 (tn-offset rd) opcode))

(define-instruction-format (i 32)
  (imm :field (byte 12 20))
  (rs1 :field (byte 5 15))
  (funct3 :field (byte 3 12))
  (rd :field (byte 5 7))
  (opcode (byte 7 0)))

(define-bitfield-emitter %emit-i-inst 32
  (byte 12 20) (byte 5 15) (byte 3 12) (byte 5 7) (byte 7 0))
(defun emit-i-inst (segment imm rs1 funct3 rd opcode)
  (%emit-i-inst segment imm (tn-offset rs1) funct3 (tn-offset rd) opcode))

(define-instruction-format (s 32)
  (imm :fields (list (byte 7 25) (byte 5 7)))
  (rs2 :field (byte 5 20))
  (rs1 :field (byte 5 15))
  (funct3 :field (byte 3 12))
  (opcode (byte 7 0)))

(define-bitfield-emitter %emit-s-inst 32
  (byte 7 25) (byte 5 20) (byte 5 15) (byte 3 12) (byte 5 7) (byte 7 0))
(defun emit-s-inst (segment imm rs2 rs1 funct3 opcode)
  (%emit-s-inst segment (ldb (byte 7 5) imm) (tn-offset rs2) (tn-offset rs1) funct3 (ldb (byte 5 0) imm) opcode))

(define-instruction-format (b 32)
  (imm :fields (list (byte 1 31) (byte 1 7) (byte 6 25) (byte 4 8)))
  (rs2 :field (byte 5 20))
  (rs1 :field (byte 5 15))
  (funct3 :field (byte 3 12))
  (opcode (byte 7 0)))

(define-bitfield-emitter %emit-b-inst 32
  (byte 1 31) (byte 6 25) (byte 5 20) (byte 5 15) (byte 3 12) (byte 4 8) (byte 1 7) (byte 7 0))
(defun emit-b-inst (segment imm rs2 rs1 funct3 opcode)
  (aver (not (logbitp 0 imm)))
  (%emit-b-inst segment (ldb (byte 1 12) imm) (ldb (byte 6 5) imm)
                (tn-offset rs2) (tn-offset rs1) funct3 (ldb (byte 4 1) imm)
                (ldb (byte 1 11) imm) opcode))

(define-instruction-format (u 32)
  (imm :field (byte 20 12))
  (rd :field (byte 5 7))
  (opcode (byte 7 0)))

(define-bitfield-emitter %emit-u-inst 32
  (byte 20 12) (byte 5 7) (byte 7 0))
(defun emit-u-inst (segment imm rd opcode)
  (aver (= 0 (ldb (byte 12 0) imm)))
  (%emit-u-inst segment imm (tn-offset rd) opcode))

(define-instruction-format (j 32)
  (imm :fields (list (byte 1 31) (byte 8 12) (byte 1 20) (byte 10 21)))
  (rd :field (byte 5 7))
  (opcode (byte 7 0)))

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

(define-instruction jal (segment lr offset)
  (:printer j ((opcode #b1101111)))
  (:emitter
   (emit-j-inst segment offset lr #b1101111)))

(define-instruction jalr (segment lr rs offset)
  (:printer i ((funct3 #b000) (opcode #b1100111)))
  (:emitter
   (emit-i-inst segment offset rs #b000 lr opcode)))

(macrolet ((define-branch-instruction (name funct3)
             `(define-instruction ,name (segment rs1 rs2 offset)
                (:printer b ((funct3 ,funct3) (opcode #b1100011)))
                (:emitter
                 (emit-b-inst segment offset rs2 rs1 ,funct3 #b1100011)))))
  (define-branch-instruction beq #b000)
  (define-branch-instruction bne #b001)
  (define-branch-instruction blt #b100)
  (define-branch-instruction bge #b101)
  (define-branch-instruction bltu #b110)
  (define-branch-instruction bgeu #b111))

(macrolet ((define-load-instruction (name funct3)
             `(define-instruction ,name (segment rd rs offset)
                (:printer i ((funct3 ,funct3) (opcode #b0000011)))
                (:emitter
                 (emit-i-inst segment offset rs ,funct3 rd #b0000011)))))
  (define-load-instruction lb #b000)
  (define-load-instruction lh #b001)
  (define-load-instruction lw #b010)
  (define-load-instruction lbu #b100)
  (define-load-instruction lhu #b101))

(macrolet ((define-store-instruction (name funct3)
             `(define-instruction ,name (segment rs2 rs1 offset)
                (:printer s ((funct3 ,funct3) (opcode #b0100011)))
                (:emitter
                 (emit-s-inst segment offset rs2 rs1 ,funct3 opcode)))))
  (define-store-instruction sb #b000)
  (define-store-instruction sh #b001)
  (define-store-instruction sw #b010))

(macrolet ((define-immediate-arith-instruction (name funct3 &optional (imm 'imm))
             `(define-instruction ,name (segment rd rs imm)
                (:printer i ((funct3 ,funct3) (opcode #b0010011)))
                (:emitter
                 (let ((imm ,imm))
                   (emit-i-inst segment imm rs ,funct3 rd #b0010011))))))
  (define-immediate-arith-instruction addi #b000)
  (define-immediate-arith-instruction slti #b010)
  (define-immediate-arith-instruction sltiu #b011)
  (define-immediate-arith-instruction xori #b100)
  (define-immediate-arith-instruction ori #b110)
  (define-immediate-arith-instruction andi #b111)
  (define-immediate-arith-instruction slli #b001
    (progn (aver (< imm n-word-bits)) imm))
  (define-immediate-arith-instruction srli #b101
    (progn (aver (< imm n-word-bits)) imm))
  (define-immediate-arith-instruction srai #b101
    (progn (aver (< imm n-word-bits)) (dpb 1 (byte 1 10) imm))))

(defmacro define-register-arith-instruction (name funct7 funct3 opcode)
  `(define-instruction ,name (segment rd rs1 rs2)
     (:printer r ((funct7 ,funct7) (funct3 ,funct3) (opcode ,opcode)))
     (:emitter
      (emit-r-inst segment ,funct7 rs2 rs1 ,funct3 rd ,opcode))))

(macrolet ((define-rv32i-arith-instruction (name funct7 funct3)
             `(define-register-arith-instruction ,name ,funct7 ,funct3 #b0110011)))
  (define-rv32i-arith-instruction add #b0000000 #b000)
  (define-rv32i-arith-instruction sub #b0100000 #b000)
  (define-rv32i-arith-instruction sll #b0000000 #b001)
  (define-rv32i-arith-instruction slt #b0000000 #b010)
  (define-rv32i-arith-instruction sltu #b0000000 #b011)
  (define-rv32i-arith-instruction xor #b0000000 #b100)
  (define-rv32i-arith-instruction srl #b0000000 #b101)
  (define-rv32i-arith-instruction sra #b0100000 #b101)
  (define-rv32i-arith-instruction or #b0000000 #b110)
  (define-rv32i-arith-instruction and #b0000000 #b111))

(define-instruction-format (fence 32)
  (funct4 (byte 4 28) :value #b0000)
  (pred (byte 4 24))
  (succ (byte 4 20))
  (rs1 (byte 5 15) :value #b00000)
  (funct3 (byte 3 12))
  (rd (byte 5 7) :value #b00000)
  (opcode (byte 7 0) :value #b0001111))

(defun fence-encoding (ops)
  (let ((vals '(:i 8 :o 4 :r 2 :w 1)))
    (typecase ops
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
  (:printer fence ())
  (:emitter
   (emit-fence-inst segment pred succ #b000)))
(define-instruction fence.i (segment)
  (:printer fence ((pred #b0000) (succ #b0000)))
  (:emitter
   (emit-fence-inst segment #b0000 #b0000 #b001)))

(define-instruction ecall (segment)
  (:printer i ((imm #b000000000000) (rs1 #b00000) (funct3 #b000)
               (rd #b00000) (opcode #b1110011)))
  (:emitter
   (%emit-i-inst segment #b000000000000 #b00000 #b000 #b00000 #b1110011)))
(define-instruction ebreak (segment)
  (:printer i ((imm #b000000000001) (rs1 #b00000) (funct3 #b000)
               (rd #b00000) (opcode #b1110011)))
  (:emitter
   (%emit-i-inst segment #b000000000001 #b00000 #b000 #b00000 #b1110011)))

;;; save CSR instructions for later - CSR

(macrolet ((define-rv32m-arith-instruction (name funct3)
             `(define-register-arith-instruction ,name #b0000001 ,funct3 #b0110011)))
  (define-rv32m-arith-instruction mul #b000)
  (define-rv32m-arith-instruction mulh #b001)
  (define-rv32m-arith-instruction mulhsu #b010)
  (define-rv32m-arith-instruction mulhu #b011)
  (define-rv32m-arith-instruction div #b100)
  (define-rv32m-arith-instruction divu #b101)
  (define-rv32m-arith-instruction rem #b110)
  (define-rv32m-arith-instruction remu #b111))
