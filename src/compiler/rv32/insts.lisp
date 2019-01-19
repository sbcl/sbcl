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

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Imports from SB-VM into this package
  (import '(sb-vm::u-and-i-inst-immediate
            sb-vm::lip-tn
            sb-vm::zero-tn
            sb-vm::null-tn)))

(deftype short-immediate () `(signed-byte 12))
(deftype short-immediate-fixnum () `(signed-byte ,(- 12 n-fixnum-tag-bits)))


(define-instruction byte (segment byte)
  (:emitter
   (emit-byte segment byte)))

(define-bitfield-emitter emit-machine-word n-machine-word-bits
  (byte n-machine-word-bits 0))

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
  (etypecase imm
    (short-immediate
     (%emit-i-inst segment imm (tn-offset rs1) funct3 (tn-offset rd) opcode))
    (fixup
     (note-fixup segment :i-type imm)
     (%emit-i-inst segment 0 (tn-offset rs1) funct3 (tn-offset rd) opcode))))

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
  (etypecase imm
    (integer
     (%emit-u-inst segment imm (tn-offset rd) opcode))
    (fixup
     (note-fixup segment :u-type imm)
     (%emit-u-inst segment 0 (tn-offset rd) opcode))))

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

;;;; Branch and Jump instructions.

(defun jalr-offset (target number-of-instructions posn &optional delta-if-after)
  (- (label-position target (and delta-if-after posn) delta-if-after)
     (+ posn (* 4 number-of-instructions))))

(defun branch/jal-offset (target number-of-instructions posn &optional delta-if-after)
  (ash (jalr-offset target number-of-instructions posn delta-if-after) -1))

(defun emit-short-jump-at (segment lr target posn)
  (declare (ignore posn))
  (emit-back-patch
   segment 4
   (lambda (segment posn)
     (emit-j-inst segment (branch/jal-offset target 1 posn) lr #b1101111))))

(defun emit-long-jump-at-fun (lr target)
  (lambda (segment posn)
    (declare (ignore posn))
    (emit-back-patch
     segment 8
     (lambda (segment posn)
       ;; We emit auipc + jalr
       (let ((disp (jalr-offset target 2 posn)))
         (emit-u-inst segment (dpb 0 (byte 12 0) disp) lip-tn #b0010111)
         (emit-i-inst segment (ldb (byte 12 0) disp) lip-tn #b000 lr #b1100111))))))

;;; For unconditional jumos, we either emit a one instruction or two
;;; instruction sequence.
(define-instruction jal (segment lr target)
  (:printer j ((opcode #b1101111)))
  (:emitter
   (emit-chooser
    segment 8 2
    (lambda (segment chooser posn delta-if-after)
      (declare (ignore chooser))
      (when (typep (branch/jal-offset target 1 posn delta-if-after)
                   '(signed-byte 20))
        (emit-short-jump-at segment lr target posn)
        t))
    (emit-long-jump-at-fun lr target))))

(define-instruction jalr (segment lr rs offset)
  (:printer i ((funct3 #b000) (opcode #b1100111)))
  (:emitter
   (emit-i-inst segment offset rs #b000 lr #b1100111)))

(define-instruction-macro j (target &optional (type :relative))
  (ecase type
    (:relative `(inst jal zero-tn ,target))
    (:fixup `(progn
               (inst lui lip-tn ,target)
               (inst jalr zero-tn lip-tn ,target)))))

(defun emit-relative-branch (segment opcode funct3 rs1 rs2 target)
  (emit-chooser
   segment 12 2
   (lambda (segment chooser posn delta-if-after)
     (declare (ignore chooser))
     (typecase (branch/jal-offset target 1 posn delta-if-after)
       (short-immediate
        (emit-back-patch
         segment 4
         (lambda (segment posn)
           (emit-b-inst segment (branch/jal-offset target 1 posn) rs2 rs1 funct3 opcode)))
        t)
       ((signed-byte 20)
        ;; Emit the sequence:
        ;; b(invert(funct3)) rs1 rs2 2
        ;; jal x0 target
        (emit-b-inst segment 2 rs2 rs1 (logxor funct3 1) opcode)
        (emit-short-jump-at segment zero-tn target posn)
        t)))
   (lambda (segment posn)
     (emit-b-inst segment 4 rs2 rs1 (logxor funct3 1) opcode)
     (funcall (emit-long-jump-at-fun zero-tn target) segment posn))))

(macrolet ((define-branch-instruction (name funct3)
             `(define-instruction ,name (segment rs1 rs2 offset)
                (:printer b ((funct3 ,funct3) (opcode #b1100011)))
                (:emitter
                 (emit-relative-branch segment #b1100011 ,funct3 rs1 rs2 offset)))))
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
             `(define-instruction ,name (segment rs1 rs2 offset)
                (:printer s ((funct3 ,funct3) (opcode #b0100011)))
                (:emitter
                 (emit-s-inst segment offset rs2 rs1 ,funct3 #b0100011)))))
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

(define-instruction-macro subi (rd rs imm)
  `(inst addi ,rd ,rs (- ,imm)))

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

(defun coerce-signed (unsigned-value width)
  (if (logbitp (1- width) unsigned-value)
      (dpb unsigned-value (byte (1- width) 0) -1)
      unsigned-value))

(defun %li (reg value)
  (etypecase value
    ((or (signed-byte 32) (unsigned-byte 32))
     (let ((value (coerce-signed value 32)))
       (etypecase value
         (short-immediate
          (inst addi reg zero-tn value))
         ((signed-byte 32)
          (multiple-value-bind (hi lo) (u-and-i-inst-immediate value)
            (inst lui reg hi)
            (inst addi reg reg lo))))))
    (fixup
     (inst lui reg value)
     (inst addi reg reg value))))

(define-instruction-macro li (reg value)
  `(%li ,reg ,value))

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

;;; Floating point
(define-instruction-format (r-float 32)
  (rs3/funct5 :field (byte 5 27))
  (fmt :field (byte 2 25))
  (rs2 :field (byte 5 20))
  (rs1 :field (byte 5 15))
  (rm :field (byte 3 12))
  (rd :field (byte 5 7))
  (opcode (byte 7 0)))

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

(macrolet ((define-rv32-float-arith-instruction
               (name funct5 &key rm rs2)
             `(define-instruction ,name
                  (segment fmt rd rs1
                           ,@(unless rs2 '(rs2))
                           ,@(unless rm '(&optional (rm :rne))) )
                (:printer r-float ((rs3/funct5 ,funct5)
                                   ,@(when rs2 `((rs2 ,rs2)))
                                   ,@(when rm `((rm ,rm)))
                                   (opcode #b1010011)))
                (:emitter
                 (emit-r-float-inst segment ,funct5 fmt
                                    ,(if rs2 rs2 'rs2) rs1
                                    ,(if rm rm '(rm-encoding rm))
                                    rd #b1010011)))))
  (define-rv32-float-arith-instruction fadd     #b00000)
  (define-rv32-float-arith-instruction fsub     #b00001)
  (define-rv32-float-arith-instruction fmul     #b00010)
  (define-rv32-float-arith-instruction fdiv     #b00011)
  (define-rv32-float-arith-instruction fsqrt    #b01011 :rs2 #b00000)
  (define-rv32-float-arith-instruction fsgnj    #b00100 :rm  #b000)
  (define-rv32-float-arith-instruction fsgnjn   #b00100 :rm  #b001)
  (define-rv32-float-arith-instruction fsgnjx   #b00100 :rm  #b010)
  (define-rv32-float-arith-instruction fmin     #b00101 :rm  #b000)
  (define-rv32-float-arith-instruction fmax     #b00101 :rm  #b001)
  (define-rv32-float-arith-instruction fcvtw<-  #b11000 :rs2 #b00000)
  (define-rv32-float-arith-instruction fcvtwu<- #b11000 :rs2 #b00001)
  (define-rv32-float-arith-instruction fmvx<-   #b11100 :rs2 #b00000 :rm #b000)
  (define-rv32-float-arith-instruction feq      #b10100 :rm  #b010)
  (define-rv32-float-arith-instruction flt      #b10100 :rm  #b001)
  (define-rv32-float-arith-instruction fle      #b10100 :rm  #b000)
  (define-rv32-float-arith-instruction fclass   #b11100 :rs2 #b00000 :rm #b001)
  (define-rv32-float-arith-instruction fcvtw->  #b11010 :rs2 #b00000)
  (define-rv32-float-arith-instruction fcvtwu-> #b11010 :rs2 #b00001)
  (define-rv32-float-arith-instruction fmvx->   #b11110 :rs2 #b00000 :rm #b000)
  (define-rv32-float-arith-instruction fcvtd->  #b01000 :rs2 #b00001)
  (define-rv32-float-arith-instruction fcvts->  #b01000 :rs2 #b00000))

(macrolet ((define-3-arg-float-arith-instruction (name opcode)
             `(define-instruction ,name (segment fmt rd rs1 rs2 rs3 &optional (rm :rne))
                (:printer r ((opcode ,opcode)))
                (:emitter
                 (emit-r-float-inst segment rs3 fmt rs2 rs1 (rm-encoding rm) rd ,opcode)))))
  (define-3-arg-float-arith-instruction fmadd #b1000011)
  (define-3-arg-float-arith-instruction fmsub #b1000111)
  (define-3-arg-float-arith-instruction fnmsub #b1001011)
  (define-3-arg-float-arith-instruction fnmadd #b1001111))

(macrolet ((def (name op)
             `(define-instruction-macro ,name (format dst src)
                `(inst ,',op ,format ,dst ,src ,src))))
  (def fmv fsgnj)
  (def fneg fsgnjn)
  (def fabs fsgnjx))

(define-instruction-macro fcvt (to-format from-format dst src &optional (rm :rne))
  (case to-format
    (:word `(inst fcvtw<- ,from-format ,dst ,src ,rm))
    (:unsigned-word `(inst fcvtwu<- ,from-format ,dst ,src ,rm))
    (otherwise
     `(inst ,(ecase from-format
               (:double 'fcvtd->)
               (:single 'fcvts->)
               (:word 'fcvtw->)
               (:unsigned-word 'fcvtwu->))
            ,to-format ,dst ,src ,rm))))

(flet ((fmt-funct3 (fmt)
         (ecase fmt
           (:single #b010)
           (:double #b011))))
  (define-instruction fload (segment fmt rd rs offset)
    (:printer i ((opcode #b0000111)))
    (:emitter
     (emit-i-inst segment offset rs (fmt-funct3 fmt) rd #b0000111)))

  (define-instruction fstore (segment fmt rs1 rs2 offset)
    (:printer s ((opcode #b0100111)))
    (:emitter
     (emit-s-inst segment offset rs2 rs1 (fmt-funct3 fmt) #b0100111))))

;;;; Boxed-object computation instructions (for LRA and CODE)

;;; Compute the address of a CODE object by parsing the header of a
;;; nearby LRA or SIMPLE-FUN.
(define-instruction compute-code (segment code lip object-label temp)
  (:vop-var vop)
  (:emitter
   (emit-back-patch
    segment 20
    (lambda (segment position)
      (assemble (segment vop)
        ;; Calculate the address of the code component.  This is an
        ;; exercise in excess cleverness.  First, we calculate (from
        ;; our program counter only) the address of OBJECT-LABEL plus
        ;; OTHER-POINTER-LOWTAG.
        (let ((offset (- ;; The 4 below is the displacement
                       ;; from reading the program counter.
                       ;; XXX may or may not be correct
                       (+ (label-position object-label)
                          other-pointer-lowtag)
                       (+ position 4))))
          (multiple-value-bind (hi lo)
              (u-and-i-inst-immediate offset)
            (inst auipc temp hi)
            (inst addi lip temp lo)))
        ;; Next, we read the function header.
        (inst lw temp lip (- other-pointer-lowtag))
        ;; And finally we use the header value (a count in words),
        ;; plus the fact that the top two bits of the widetag are
        ;; clear (SIMPLE-FUN-WIDETAG is #x2A and
        ;; RETURN-PC-WIDETAG is #x36) to compute the boxed
        ;; address of the code component.
        (inst srli temp temp (- 8 word-shift))
        (inst sub code lip temp))))))

(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
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
