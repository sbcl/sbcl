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

(define-instruction-format (b 32)
  (imm :fields (list (byte 1 31) (byte 1 7) (byte 6 25) (byte 4 8)))
  (rs2 :field (byte 5 20))
  (rs1 :field (byte 5 15))
  (funct3 :field (byte 3 12))
  (opcode (byte 7 0)))

(define-instruction-format (u 32)
  (imm :field (byte 20 12))
  (rd :field (byte 5 7))
  (opcode (byte 7 0)))

(define-instruction-format (j 32)
  (imm :fields (list (byte 1 31) (byte 8 12) (byte 1 20) (byte 10 21)))
  (rd :field (byte 5 7))
  (opcode (byte 7 0)))

(define-instruction lw (segment rd rs1 offset)
  (:printer i ((funct3 #b010) (opcode #b0000011)))
  (:emitter
   (emit-i-inst segment offset rs1 #b010 rd #b0000011)))

(define-instruction add (segment rd rs1 rs2)
  (:printer r ((funct7 #b0000000) (funct3 #b000) (opcode #b0110011)))
  (:emitter
   (emit-r-inst segment #b0000000 rs2 rs1 #b000 rd #b0110011)))
