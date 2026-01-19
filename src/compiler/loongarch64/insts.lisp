;;;; that part of the description of the LoongArch instruction set which
;;;; can live on the cross-compilation host

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.
(in-package "SB-LOONGARCH64-ASM")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(;; SBs, SCs, and TNs
            sb-vm::immediate-constant
            sb-vm::registers
            sb-vm::float-registers
            sb-vm::zero
            sb-vm::zero-offset
            sb-vm::lip-tn
            sb-vm::t7-tn
            sb-vm::t8-tn
            sb-vm::zero-tn
            sb-vm::ra-tn
            sb-vm::tn-byte-offset
            ; Types
            sb-vm::u-and-i-inst-immediate
            sb-vm::short-immediate
            sb-vm::short-immediate-fixnum
            sb-vm::u+i-immediate)))

;;;; Constants, types, conversion functions, some disassembler stuff.
(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (sc-case tn
    (zero zero-offset)
    (t
     (case (sb-name (sc-sb (tn-sc tn)))
       ((registers float-registers) (tn-offset tn))
       (t (error "~S isn't a register." tn))))))

(define-arg-type reg :printer #'print-reg)
(define-arg-type s-imm :printer #'print-s-imm)
(define-arg-type fp-reg :printer #'print-fp-reg)
(define-arg-type control-reg :printer "(CR:#x~X)")
(define-arg-type a-ordering :printer #'print-a-ordering)
(define-arg-type float-fmt :printer #'print-float-fmt)
(define-arg-type float-rm :printer #'print-float-rm)
(define-arg-type relative-b-label :use-label #'use-b-label)
(define-arg-type relative-j-label :use-label #'use-j-label)
(define-arg-type load-annotation :printer #'print-load-annotation)
(define-arg-type store-annotation :printer #'print-store-annotation)
(define-arg-type jirl-annotation :printer #'print-jirl-annotation)

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

(defconstant-eqx r-printer
    '(:name :tab rd ", " rj ", " rk)
  #'equal)

(define-instruction-format (r 32 :default-printer r-printer)
  (opcode :field (byte 17 15))
  (rk :field (byte 5 10) :type 'reg)
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-bitfield-emitter %emit-r-inst 32
  (byte 17 15)
  (byte 5 10)
  (byte 5 5)
  (byte 5 0))

(defun emit-r-inst (segment opcode rk rj rd)
  (%emit-r-inst segment opcode (reg-tn-encoding rk) (reg-tn-encoding rj) (reg-tn-encoding rd)))

(defconstant-eqx i1-printer
    '(:name :tab rd ", " rj ", " imm)
  #'equal)

(define-instruction-format (i1 32 :default-printer i1-printer)
  (i1-annotation :fields (list (byte 5 5) (byte 12 10)))
  (opcode :field (byte 10 22))
  (imm :field (byte 12 10) :type 's-imm)
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-bitfield-emitter %emit-i1-inst 32
  (byte 10 22)
  (byte 12 10)
  (byte 5 5)
  (byte 5 0))

(defun normalize-imm (imm)
  (if (and (integerp imm) (> imm (1- (expt 2 63))))
      (- imm (expt 2 64))
      imm))

(defun emit-i1-inst (segment opcode rd rj imm)
  (setf imm (normalize-imm imm))
    (cond
    ((member opcode '(#b0000001101 #b0000001110 #b0000001111))
     (etypecase imm
       ((integer 0 4095)
        (%emit-i1-inst segment opcode imm
                       (reg-tn-encoding rj)
                       (reg-tn-encoding rd)))
       (fixup
        (note-fixup segment :s-type imm)
        (%emit-i1-inst segment opcode 0
                       (reg-tn-encoding rj)
                       (reg-tn-encoding rd)))))
    (t
     (etypecase imm
       ((integer -2048 2047)
        (%emit-i1-inst segment opcode imm
                       (reg-tn-encoding rj)
                       (reg-tn-encoding rd)))
       (fixup
        (note-fixup segment :s-type imm)
        (%emit-i1-inst segment opcode 0
                       (reg-tn-encoding rj)
                       (reg-tn-encoding rd)))))))

(defconstant-eqx s-printer
    '(:name :tab rd ", " rj "," imm )
  #'equal)

(define-instruction-format (s 32 :default-printer s-printer)
  (opcode :field (byte 10 22))
  (imm :field (byte 12 10) :type 's-imm)
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-bitfield-emitter %emit-s-inst 32
  (byte 10 22)
  (byte 12 10)
  (byte 5 5)
  (byte 5 0))

(defun emit-s-inst (segment opcode rd rj imm)
  (etypecase imm
    (short-immediate
     (%emit-s-inst segment opcode imm
               (reg-tn-encoding rj) (reg-tn-encoding rd)))
    (fixup
     (note-fixup segment :s-type imm)
     (%emit-s-inst segment opcode 0
               (reg-tn-encoding rj) (reg-tn-encoding rd)))))

(defconstant-eqx b-printer
  '(:name :tab rj ", " rd ", " imm)
  #'equal)

(define-instruction-format (b 32 :default-printer b-printer)
  (opcode :field (byte 6 26))
  (imm :field (byte 16 10)  :type 'relative-b-label)
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-bitfield-emitter %emit-b-inst 32
  (byte 6 26)
  (byte 16 10)
  (byte 5 5)
  (byte 5 0))

(defun emit-b-inst (segment opcode imm rj rd)
  (let ((imm16 (ash imm -2)))
    (aver (typep imm16 '(signed-byte 16)))
    (%emit-b-inst segment opcode imm16
                  (reg-tn-encoding rj)
                  (reg-tn-encoding rd))))

(defconstant-eqx b1-printer
    '(:name :tab rj ", " imm)
  #'equal)

(define-instruction-format (b1 32 :default-printer b1-printer)
  (opcode :field (byte 6 26))
  (imm :fields (list (byte 5 0) (byte 20 5)))
  (rj :field (byte 5 5)
       :type 'reg))

(define-bitfield-emitter %emit-b1-inst 32
  (byte 6 26)
  (byte 16 10)
  (byte 5 5)
  (byte 5 0))

(defun emit-b1-inst (segment opcode rj imm)
     (aver (not (logbitp 0 imm)))
     (let ((imm1 (ash imm -2)))
     (%emit-b1-inst segment opcode (ldb (byte 16 10) imm1) (reg-tn-encoding rj) (ldb (byte 5 5) imm1))))

(defconstant-eqx u-printer
    '(:name :tab rj ", " imm)
  #'equal)

(define-instruction-format (u 32 :default-printer u-printer)
  (opcode :field (byte 7 25))
  (imm :field (byte 20 5))
  (rj :field (byte 5 0)
       :type 'reg))

(define-bitfield-emitter %emit-u-inst 32
  (byte 7 25)
  (byte 20 5)
  (byte 5 0))

(defun emit-u-inst (segment opcode rj imm)
  (etypecase imm
    (integer
     (%emit-u-inst segment opcode imm (reg-tn-encoding rj)))
    (fixup
     (note-fixup segment :u-type imm)
     (%emit-u-inst segment opcode 0 (reg-tn-encoding rj)))))

(defconstant-eqx bl-printer
    '(:name :tab imm)
  #'equal)

(define-bitfield-emitter %emit-bl-inst 32
  (byte 6 26)
  (byte 16 10)
  (byte 10 0))

(define-instruction-format (bl 32 :default-printer bl-printer)
  (opcode :field (byte 7 0))
  (imm :fields (list (byte 10 0) (byte 16 10))))

(defun emit-bl-inst (segment opcode imm)
  (cond
    ((typep imm '(signed-byte 26))
     (let ((imm26 (ash imm -2)))
       (let ((imm-low  (ldb (byte 16 0) imm26))
             (imm-high (ldb (byte 10 16) imm26)))
         (%emit-bl-inst segment opcode imm-low imm-high))))
    ((typep imm 'fixup)
     (note-fixup segment :i-type imm)
     (%emit-bl-inst segment opcode 0 0))))

(define-instruction bl (segment offset)
  (:emitter
   (emit-bl-inst segment #b010101 offset)))

(define-instruction b (segment offset)
  (:emitter
   (emit-bl-inst segment #b010100 offset)))

(defconstant-eqx beqz-printer
    '(:name :tab rj ", " imm)
  #'equal)

(define-instruction beqz (segment rj offset)
  (:printer b ((opcode #b010000)
               (u-annotation nil :type 'jirl-annotation))
            beqz-printer)
  (:emitter
   (emit-b1-inst segment #b010000 rj offset)))

(defconstant-eqx bnez-printer
    '(:name :tab rj ", " imm)
  #'equal)

(define-instruction bnez (segment rj offset)
  (:printer b ((opcode #b010001)
               (u-annotation nil :type 'jirl-annotation))
            bnez-printer)
  (:emitter
   (emit-b1-inst segment #b010001 rj offset)))

(defconstant-eqx b2-printer
    '(:name :tab cj ", " imm)
  #'equal)

(define-bitfield-emitter %emit-b2-inst 32
  (byte 6 26)
  (byte 16 10)
  (byte 2 8)
  (byte 3 5)
  (byte 5 0))

(defun emit-b2-inst (segment opcode funct2 cj imm)
     (let ((imm1 (ash imm -2)))
     (aver (typep imm1 '(signed-byte 20)))
     (%emit-b2-inst segment opcode (ldb (byte 16 0) imm1)
       funct2 cj (ldb (byte 5 16) imm1))))

(define-instruction-format (b2 32 :default-printer b2-printer)
  (opcode :field (byte 6 26))
  (imm1 :field (byte 15 10))
  (funct2 :field (byte 2 8))
  (cj :field (byte 3 5))
  (imm2 :field (byte 5 0)))

(defun emit-relative-branch1 (segment opcode funct2 cj target)
  (emit-chooser
   segment 8 2
   (lambda (segment chooser posn delta-if-after)
     (declare (ignore chooser))
     (let ((offset (ash (relative-offset target posn delta-if-after) -1)))
       (when (typep offset '(signed-byte 20))
         (emit-back-patch
          segment 4
          (lambda (segment posn)
            (emit-b2-inst segment opcode funct2 cj (relative-offset target posn))))
         t)))
   ;; worst-case
   (lambda (segment posn)
     (emit-b2-inst segment opcode (logxor funct2 1) cj target)
     (funcall (emit-long-jump-at-fun zero-tn target) segment posn))))

(defconstant-eqx bceqz-printer
    '(:name :tab cj ", " imm)
  #'equal)

(define-instruction bceqz (segment cj offset)
  (:emitter
   (emit-relative-branch1 segment #b010010 #b00 cj offset)))

(defconstant-eqx bcnez-printer
    '(:name :tab rj ", " imm)
  #'equal)

(define-instruction bcnez (segment cj offset)
  (:emitter
   (emit-relative-branch1 segment #b010010 #b01 cj offset)))

(defconstant-eqx w-printer
    '(:name :tab rd ", " rj ", " imm)
  #'equal)

(defconstant-eqx d-printer
    '(:name :tab rd ", " rj ", " imm)
  #'equal)

(define-instruction-format (w 32 :default-printer w-printer)
  (opcode :field (byte 17 15))
  (imm :field (byte 5 10))
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-instruction-format (d 32 :default-printer d-printer)
  (opcode :field (byte 16 16))
  (imm :field (byte 6 10))
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-bitfield-emitter %emit-w-inst 32
  (byte 17 15)
  (byte 5 10)
  (byte 5 5)
  (byte 5 0) )

(define-bitfield-emitter %emit-d-inst 32
  (byte 16 16)
  (byte 6 10)
  (byte 5 5)
  (byte 5 0))

(defun emit-w-inst (segment opcode rd rj imm)
  (etypecase imm
    (short-immediate
     (%emit-w-inst segment opcode imm
      (reg-tn-encoding rj) (reg-tn-encoding rd)))))

(defun emit-d-inst (segment opcode rd rj imm)
  (etypecase imm
    (short-immediate
     (%emit-d-inst segment opcode imm
       (reg-tn-encoding rj) (reg-tn-encoding rd)))))

(define-instruction slli.w (segment rd rj imm)
  (:printer w ((opcode #b00000000010000001))
            w-printer)
  (:emitter
   (emit-w-inst segment #b00000000010000001 rd rj imm)))

(define-instruction slli.d (segment rd rj imm)
  (:printer d ((opcode #b0000000001000001))
            d-printer)
  (:emitter
   (emit-d-inst segment #b0000000001000001 rd rj imm)))

(define-instruction srli.w (segment rd rj imm)
  (:printer w ((opcode #b00000000010001001))
            w-printer)
  (:emitter
   (emit-w-inst segment #b00000000010001001 rd rj imm)))

(define-instruction srli.d (segment rd rj imm)
  (:printer d ((opcode #b0000000001000101))
            d-printer)
  (:emitter
   (emit-d-inst segment #b0000000001000101 rd rj imm)))

(define-instruction srai.w (segment rd rj imm)
  (:printer w ((opcode #b00000000010010001))
            w-printer)
  (:emitter
   (emit-w-inst segment #b00000000010010001 rd rj imm)))

(define-instruction srai.d (segment rd rj imm)
  (:printer d ((opcode #b0000000001001001))
            d-printer)
  (:emitter
   (emit-d-inst segment #b0000000001001001 rd rj imm)))

(defconstant-eqx bstr-printer
    '(:name :tab rd ", " rj ", " msbw ", " lsbw)
  #'equal)

(define-instruction-format (bstr 32 :default-printer bstr-printer)
  (opcode :field (byte 10 22))
  (lsbw :field (byte 6 16))
  (msbw :field (byte 6 10))
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-bitfield-emitter %emit-bstr-inst 32
  (byte 10 22)
  (byte 6 16)
  (byte 6 10)
  (byte 5 5)
  (byte 5 0))

(defun emit-bstr-inst (segment opcode rd rj msbw lsbw)
     (%emit-bstr-inst segment opcode lsbw msbw (reg-tn-encoding rj) (reg-tn-encoding rd)))

(define-instruction bstrins.d (segment rd rj msbw lsbw)
  (:printer bstr ((opcode #b0000000010))
            bstr-printer)
  (:emitter
   (emit-bstr-inst segment #b0000000010 rd rj msbw lsbw)))

(define-instruction bstrpick.d (segment rd rj msbw lsbw)
  (:printer bstr ((opcode #b0000000011))
            bstr-printer)
  (:emitter
   (emit-bstr-inst segment #b0000000011 rd rj msbw lsbw)))

(defconstant-eqx ato-printer
    '(:name :tab rd ", " rj ", " offset)
  #'equal)

(define-instruction-format (ato 32 :default-printer ato-printer)
  (opcode :field (byte 8 24))
  (offset :field (byte 14 10))
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-bitfield-emitter %emit-ato-inst 32
  (byte 8 24)
  (byte 14 10)
  (byte 5 5)
  (byte 5 0))

(macrolet ((define-load-instruction (name opcode)
             `(define-instruction ,name (segment rd rj offset)
                (:printer i1
                          ((opcode ,opcode)
                          (i1-annotation nil :type 'load-annotation))
                          '(:name :tab rd ", " rj " , (" imm ")" i1-annotation))
                (:emitter
                 (emit-i1-inst segment ,opcode rd rj offset)))))
  (define-load-instruction ld.b #b0010100000)
  (define-load-instruction ld.bu #b0010101000)
  (define-load-instruction ld.h #b0010100001)
  (define-load-instruction ld.hu #b0010101001)
  (define-load-instruction ld.w #b0010100010)
  (define-load-instruction fld.s #b0010101100)
  (define-load-instruction ld.d #b0010100011)
  (define-load-instruction ld.wu #b0010101010)
  (define-load-instruction fld.d #b0010101110))

(defun emit-ll-inst (segment opcode rd rj imm)
  (cond
    ((and (integerp imm) (<= -2048 imm 2047))
     (%emit-ato-inst segment opcode (ash imm -2) (reg-tn-encoding rj)
                    (reg-tn-encoding rd)))
    ((typep imm 'short-immediate)
     (%emit-ato-inst segment opcode (ash imm -2) (reg-tn-encoding rj)
                    (reg-tn-encoding rd)))))

(define-instruction ll.w (segment rd rj offset)
  (:printer ato ((opcode #b00100000))
            ato-printer)
  (:emitter
   (emit-ll-inst segment #b00100000 rd rj offset)))

(define-instruction ll.d (segment rd rj offset)
  (:printer ato ((opcode #b00100010))
            ato-printer)
  (:emitter
   (emit-ll-inst segment #b00100010 rd rj offset)))

(macrolet ((define-store-instruction (name opcode)
             `(define-instruction ,name (segment rd rj offset)
                (:printer s ((opcode ,opcode))
                          s-printer)
                (:emitter
                 (emit-s-inst segment ,opcode rd rj offset)))))
  (define-store-instruction st.b #b0010100100)
  (define-store-instruction st.h #b0010100101)
  (define-store-instruction st.w #b0010100110)
  (define-store-instruction fst.s #b0010101101)
  (define-store-instruction st.d #b0010100111)
  (define-store-instruction fst.d #b0010101111))

(defun emit-sc-inst (segment opcode rd rj imm)
  (etypecase imm
    (short-immediate
     (%emit-ato-inst segment opcode (ash imm -2)
               (reg-tn-encoding rj) (reg-tn-encoding rd)))
    (fixup
     (note-fixup segment :i-type imm)
     (%emit-ato-inst segment opcode 0
               (reg-tn-encoding rj) (reg-tn-encoding rd)))))

(define-instruction sc.w (segment rd rj offset)
  (:printer ato ((opcode #b00100001))
            ato-printer)
  (:emitter
   (emit-sc-inst segment #b00100001 rd rj offset)))

(define-instruction sc.d (segment rd rj offset)
  (:printer ato ((opcode #b00100011))
            ato-printer)
  (:emitter
   (emit-sc-inst segment #b00100011 rd rj offset)))

(defconstant-eqx am-printer
    '(:name :tab rd ", " rj)
  #'equal)

(define-instruction-format (am 32 :default-printer am-printer)
  (opcode :field (byte 22 10))
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-bitfield-emitter %emit-am-inst 32
  (byte 22 10)
  (byte 5 5)
  (byte 5 0))

(defun emit-am-inst (segment opcode rd rj)
  (%emit-am-inst segment opcode (reg-tn-encoding rj) (reg-tn-encoding rd)))

(defmacro define-register-am-instruction (name opcode)
  `(define-instruction ,name (segment rd rj)
     (:printer am ((opcode ,opcode))
               '(:name :tab rd ", " rj))
     (:emitter
     (emit-am-inst segment ,opcode rd rj))))

(macrolet ((define-la-am-instruction (name opcode)
             `(progn
                (define-register-am-instruction ,name ,opcode))))
  (define-la-am-instruction llacq.w #b0011100001010111100000)
  (define-la-am-instruction screl.w #b0011100001010111100001)
  (define-la-am-instruction llacq.d #b0011100001010111100010)
  (define-la-am-instruction screl.d #b0011100001010111100011))

(define-instruction-macro fstore (&optional format fd rj offset)
  `(case ,format
     (:double (inst fst.d ,fd ,rj ,offset))
     (:single (inst fst.s ,fd ,rj ,offset))))

(define-instruction-macro fload (&optional format fd rj offset)
  `(case ,format
     (:double (inst fld.d ,fd ,rj ,offset))
     (:single (inst fld.s ,fd ,rj ,offset))))

(macrolet ((define-i-immediate-arith-instruction (name opcode)
             `(progn
             (define-instruction ,name (segment rd rj imm)
                (:printer i1 ((opcode ,opcode)))
                (:emitter
                     (when (member ',name '(andi ori xori))
                      (setf imm (logand imm #xfff)))
                    (emit-i1-inst segment ,opcode rd rj imm))))))
  (define-i-immediate-arith-instruction addi.d #b0000001011)
  (define-i-immediate-arith-instruction addi.w #b0000001010)
  (define-i-immediate-arith-instruction lu52i.d #b0000001100)
  (define-i-immediate-arith-instruction andi #b0000001101)
  (define-i-immediate-arith-instruction ori  #b0000001110)
  (define-i-immediate-arith-instruction xori #b0000001111)
  (define-i-immediate-arith-instruction slti #b0000001000)
  (define-i-immediate-arith-instruction sltui #b0000001001))

(macrolet ((define-immediate-arith-instruction (name opcode)
             `(progn
               (define-instruction ,name (segment rd imm)
                  (:printer u ((opcode ,opcode)))
                  (:emitter
                   (emit-u-inst segment ,opcode rd imm))))))
  (define-immediate-arith-instruction lu12i.w #b0001010)
  (define-immediate-arith-instruction lu32i.d #b0001011)
  (define-immediate-arith-instruction pcaddi #b0001100)
  (define-immediate-arith-instruction pcalau12i  #b0001101)
  (define-immediate-arith-instruction pcaddu12i #b0001110)
  (define-immediate-arith-instruction pcaddu18i #b0001111))

(defun relative-offset (target posn &optional delta-if-after)
  (- (label-position target (and delta-if-after posn) delta-if-after) posn))

(defconstant-eqx j-printer
    '(:name :tab rd ", " rj ", " imm)
  #'equal)

(define-instruction-format (j 32 :default-printer j-printer)
  (opcode :field (byte 6 26))
  (imm :field (byte 16 10) :type 'relative-j-label)
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(define-bitfield-emitter %emit-j-inst 32
  (byte 6 26)
  (byte 16 10)
  (byte 5 5)
  (byte 5 0))

(defun emit-j-inst (segment opcode imm rj rd)
  (cond
    ((typep imm '(signed-byte 16))
    (let ((imm16 (ash imm -2)))
     (%emit-j-inst segment opcode imm16
                   (reg-tn-encoding rj)
                   (reg-tn-encoding rd))))
    ((typep imm 'fixup)
     (note-fixup segment :i-type imm)
     (%emit-j-inst segment opcode 0
                   (reg-tn-encoding rj)
                   (reg-tn-encoding rd)))))

(defun emit-short-jump-at (segment rd target posn)
  (declare (ignore posn))
  (emit-back-patch
   segment 8
   (lambda (segment posn)
       (assemble (segment)
       (inst pcaddi t7-tn 1)
       (emit-j-inst segment #b010011 (- (relative-offset target posn) 4) t7-tn rd)))))

(define-instruction jirl (segment rd rj offset)
  (:printer j ((opcode #b010011)
               (i1-annotation nil :type 'jirl-annotation))
            j-printer)
  (:emitter
   (emit-j-inst segment #b010011 offset rj rd)))

(defun emit-long-jump-at-fun (rd target)
  (lambda (segment posn)
  (declare (ignore posn))
    (emit-back-patch
     segment 8
     (lambda (segment posn)
       (multiple-value-bind (hi lo)
           (u-and-i-inst-immediate (relative-offset target posn))
         (assemble (segment)
           (inst pcaddu12i t7-tn hi)
           (inst jirl rd t7-tn lo)))))))

(define-instruction jal (segment rd target)
  (:emitter
   (typecase target
     (fixup
      (assemble (segment)
        (inst lu12i.w lip-tn target)
        (inst jirl rd lip-tn target)))
     (t
      (emit-chooser
       segment 8 2
       (lambda (segment chooser posn delta-if-after)
         (declare (ignore chooser))
         (when (typep (ash (relative-offset target posn delta-if-after) -1)
                      '(signed-byte 14))
           (emit-short-jump-at segment rd target posn)
           t))
       (emit-long-jump-at-fun rd target))))))

(define-instruction-macro j (target)
  `(inst jal zero-tn ,target))

(defun emit-relative-branch (segment opcode target rj rd)
  (emit-chooser
   segment 12 2
   (lambda (segment chooser posn delta-if-after)
     (declare (ignore chooser))
     (let ((offset (ash (relative-offset target posn delta-if-after) -1)))
       (when (typep offset '(signed-byte 16))
         (emit-back-patch
          segment 4
          (lambda (segment posn)
            (emit-b-inst segment opcode (relative-offset target posn) rj rd)))
         t)))
   (lambda (segment posn)
     (emit-b-inst segment (logxor opcode 1) 12 rj rd)
     (funcall (emit-long-jump-at-fun zero-tn target) segment posn))))

(macrolet ((define-branch-instruction (name opcode)
             `(define-instruction ,name (segment rj rd offset)
                (:printer b ((opcode ,opcode))
                          b-printer)
                (:dependencies (writes lip-tn))
                (:emitter
                 (emit-relative-branch segment ,opcode offset rj rd)))))
  (define-branch-instruction beq #b010110)
  (define-branch-instruction bne #b010111)
  (define-branch-instruction blt #b011000)
  (define-branch-instruction bge #b011001)
  (define-branch-instruction bltu #b011010)
  (define-branch-instruction bgeu #b011011))

(define-instruction-macro subi (rd rj imm)
  `(inst addi.d ,rd ,rj (- ,imm)))

(define-instruction-macro move (rd rj)
  `(inst addi.d ,rd ,rj 0))

(define-instruction-macro nop()
  `(inst andi zero-tn zero-tn 0))

(defmacro define-register-store-instruction (name opcode)
  `(define-instruction ,name (segment rd rj rk)
     (:printer r ((opcode ,opcode))
               '(:name :tab rd ", " rj ", " rk))
     (:emitter
      (emit-r-inst segment ,opcode rk rj rd))))

(macrolet ((define-la-store-instruction (name opcode)
             `(progn
                (define-register-store-instruction ,name ,opcode))))
  (define-la-store-instruction ldx.b #b00111000000000000)
  (define-la-store-instruction ldx.h #b00111000000001000)
  (define-la-store-instruction ldx.w #b00111000000010000)
  (define-la-store-instruction ldx.d #b00111000000011000)
  (define-la-store-instruction stx.b #b00111000000100000)
  (define-la-store-instruction stx.h #b00111000000101000)
  (define-la-store-instruction stx.w #b00111000000110000)
  (define-la-store-instruction stx.d #b00111000000111000)
  (define-la-store-instruction ldx.bu #b00111000001000000)
  (define-la-store-instruction ldx.hu #b00111000001001000)
  (define-la-store-instruction ldx.wu #b00111000001010000))

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
           (inst addi.d rd zero-tn immediate))
          (t
           (inst lu12i.w rd hi)
           (unless (zerop lo)
             (inst addi.d rd rd lo))))))

;;;; li should optimization by lu32i、lu52i
(defun %li (reg value)
  (etypecase value
    (u+i-immediate
     (load-signed-byte-32-immediate reg (coerce-signed value n-word-bits)))
    ((or (signed-byte 64) (unsigned-byte 64))
     ;; It would be better to use a dynamic programming approach here.
     (let* ((value (coerce-unsigned value 64))
            (integer-length (integer-length value))
            (2^k (ash 1 integer-length))
            (2^.k-1 (ash 1 (1- integer-length)))
            (complement (mod (lognot value) (ash 1 64))))
       (cond ((zerop (logand (1+ value) value))
              ;; Common special case: the immediate is of the form #xfff...
              (inst addi.d reg zero-tn -1)
              (unless (= integer-length 64)
                (inst srli.d reg reg (- 64 integer-length))))
             ((let ((delta (- 2^k value)))
                (and (typep (ash delta (- 64 integer-length)) 'short-immediate)
                     (logand delta (1- delta))))
              ;; Common special case: the immediate is of the form
              ;; #x00fff...00, where there are a small number of
              ;; zeroes at the end.
              (inst addi.d reg zero-tn (ash (- value 2^k) (- 64 integer-length)))
              (inst srli.d reg reg (- 64 integer-length)))
             ((zerop (logand complement (1+ complement)))
              ;; #xfffffff...00000
              (inst addi.d reg zero-tn -1)
              (inst slli.d reg reg (integer-length complement)))
             ((typep (- value 2^k) 'short-immediate)
              ;; Common special case: loading an immediate which is a
              ;; signed 12 bit constant away from a power of 2.
              (cond ((= integer-length 64)
                     (inst addi.d reg zero-tn (- value 2^k)))
                    (t
                     (inst addi.d reg zero-tn 1)
                     (inst slli.d reg reg integer-length)
                     (inst addi.d reg reg (- value 2^k)))))
             ((typep (- value 2^.k-1) 'short-immediate)
              ;; Common special case: loading an immediate which is a
              ;; signed 12 bit constant away from a power of 2.
              (inst addi.d reg zero-tn 1)
              (inst slli.d reg reg (1- integer-length))
              (unless (= value 2^.k-1)
                (inst addi.d reg reg (- value 2^.k-1))))
             (t
              ;; The "generic" case.
              ;; Load in the first 31 non zero most significant bits.
              (let ((chunk (ldb (byte 12 (- integer-length 31)) value)))
                (inst lu12i.w reg (ldb (byte 20 (- integer-length 19)) value))
                (cond ((= (1- (ash 1 12)) chunk)
                       (inst addi.d reg reg (1- (ash 1 11)))
                       (inst addi.d reg reg (1- (ash 1 11)))
                       (inst addi.d reg reg 1))
                      ((logbitp 11 chunk)
                       (inst addi.d reg reg (1- (ash 1 11)))
                       (inst addi.d reg reg (- chunk (1- (ash 1 11)))))
                      (t
                       (inst addi.d reg reg chunk))))
              ;; Now we need to load in the rest of the bits properly, in
              ;; chunks of 11 to avoid sign extension.
              (do ((i (- integer-length 31) (- i 11)))
                  ((< i 11)
                   (inst slli.d reg reg i)
                   (unless (zerop (ldb (byte i 0) value))
                     (inst addi.d reg reg (ldb (byte i 0) value))))
                (inst slli.d reg reg 11)
                (inst addi.d reg reg (ldb (byte 11 (- i 11)) value)))))))
    (fixup
     (inst lu12i.w reg value)
     (inst addi.d reg reg value))))

(define-instruction-macro li (reg value)
  `(%li ,reg ,value))

(defmacro define-register-arith-instruction (name opcode)
  `(define-instruction ,name (segment rd rj rk)
     (:printer r ((opcode ,opcode))
               '(:name :tab rd ", " rj ", " rk))
     (:emitter
     (emit-r-inst segment ,opcode rk rj rd))))

(macrolet ((define-la-arith-instruction (name opcode)
             `(progn
                (define-register-arith-instruction ,name ,opcode))))
  (define-la-arith-instruction add.w #b00000000000100000)
  (define-la-arith-instruction add.d #b00000000000100001)
  (define-la-arith-instruction sub.w #b00000000000100010)
  (define-la-arith-instruction sub.d #b00000000000100011)
  (define-la-arith-instruction slt #b00000000000100100)
  (define-la-arith-instruction sltu #b00000000000100101)
  (define-la-arith-instruction maskeqz #b00000000000100110)
  (define-la-arith-instruction masknez #b00000000000100111)
  (define-la-arith-instruction nor #b00000000000101000)
  (define-la-arith-instruction and #b00000000000101001)
  (define-la-arith-instruction or #b00000000000101010)
  (define-la-arith-instruction xor #b00000000000101011)
  (define-la-arith-instruction orn #b00000000000101100)
  (define-la-arith-instruction andn #b00000000000101101)
  (define-la-arith-instruction sll.w #b00000000000101110)
  (define-la-arith-instruction srl.w #b00000000000101111)
  (define-la-arith-instruction sra.w #b00000000000110000)
  (define-la-arith-instruction sll.d #b00000000000110001)
  (define-la-arith-instruction srl.d #b00000000000110010)
  (define-la-arith-instruction sra.d #b00000000000110011)
  (define-la-arith-instruction rotr.w #b00000000000110110)
  (define-la-arith-instruction rotr.d #b00000000000110111)
  (define-la-arith-instruction mul.w #b00000000000111000)
  (define-la-arith-instruction mulh.w #b00000000000111001)
  (define-la-arith-instruction mulh.wu #b00000000000111010)
  (define-la-arith-instruction mul.d #b00000000000111011)
  (define-la-arith-instruction mulh.d #b00000000000111100)
  (define-la-arith-instruction mulh.du #b00000000000111101)
  (define-la-arith-instruction mulw.d.w #b00000000000111110)
  (define-la-arith-instruction mulw.d.wu #b00000000000111111)
  (define-la-arith-instruction div.w #b00000000001000000)
  (define-la-arith-instruction mod.w #b00000000001000001)
  (define-la-arith-instruction div.wu #b00000000001000010)
  (define-la-arith-instruction mod.wu #b00000000001000011)
  (define-la-arith-instruction div.d #b00000000001000100)
  (define-la-arith-instruction mod.d #b00000000001000101)
  (define-la-arith-instruction div.du #b00000000001000110)
  (define-la-arith-instruction mod.du #b00000000001000111))

(defun %andi (rd rj imm)
  (let* ((imm12 (ldb (byte 12 0) imm))
         (simm12 (if (logbitp 11 imm12)
                     (- imm12 #x1000)
                     imm12)))
    (if (minusp simm12)
        (let ((temp t7-tn))
          (inst li temp simm12)
          (inst and rd rj temp))
        (inst andi rd rj simm12))))

(define-instruction-macro s_andi (rd rj imm)
  `(%andi ,rd ,rj ,imm))

(defun %ori (rd rj imm)
  (let* ((imm12 (ldb (byte 12 0) imm))
         (simm12 (if (logbitp 11 imm12)
                     (- imm12 #x1000)
                     imm12)))
    (if (minusp simm12)
        (let ((temp t7-tn))
          (inst li temp simm12)
          (inst or rd rj temp))
        (inst ori rd rj simm12))))

(define-instruction-macro s_ori (rd rj imm)
  `(%ori ,rd ,rj ,imm))

(defun %xori (rd rj imm)
  (let* ((imm12 (ldb (byte 12 0) imm))
         (simm12 (if (logbitp 11 imm12)
                     (- imm12 #x1000)
                     imm12)))
    (if (minusp simm12)
        (let ((temp t7-tn))
          (inst li temp simm12)
          (inst xor rd rj temp))
        (inst xori rd rj simm12))))

(define-instruction-macro s_xori (rd rj imm)
  `(%xori ,rd ,rj ,imm))

(define-instruction-format (dbar 32 :default-printer '(:name :tab hint))
  (opcode :field (byte 6 0)   :value #b00111000011100100)
  (hint    :field (byte 10 15)))

(define-instruction-format (ibar 32 :default-printer '(:name :tab hint))
  (opcode :field (byte 6 0)   :value #b00111000011100101)
  (hint   :field (byte 10 15)))

(define-bitfield-emitter %emit-dbar 32
  (byte 17 15) (byte 15 0))

(defun emit-dbar (segment hint)
  (%emit-dbar segment #b00111000011100100 hint))

(define-bitfield-emitter %emit-ibar 32
  (byte 17 15) (byte 15 0))

(defun emit-ibar (segment hint)
  (%emit-ibar segment #b00111000011100101 hint))

(define-instruction dbar (segment hint)
  (:emitter
   (emit-dbar segment hint)))

(define-instruction ibar (segment hint)
  (:emitter
   (emit-ibar segment hint)))

(define-instruction-format (break 32 :default-printer '(:name :tab code))
  (opcode :field (byte 17 15))
  (code    :field (byte 15 0) :reader break-code))

(define-bitfield-emitter %emit-break 32
  (byte 17 15)
  (byte 15 0))

(defun emit-break-inst (segment code)
    (%emit-break segment #b00000000001010100 code))

(define-instruction break (segment code)
  (:printer break ((opcode #b00000000001010100))
            :default :control #'break-control)
  (:emitter (emit-break-inst segment code)))

(define-instruction-format (syscall 32 :default-printer '(:name :tab code))
  (opcode :field (byte 17 15))
  (code    :field (byte 15 0) :reader syscall-code))

(define-bitfield-emitter %emit-syscall 32
  (byte 17 15)
  (byte 15 0))

(defun emit-syscall-inst (segment code)
    (%emit-syscall segment #b00000000001010110 code))

(define-instruction syscall (segment code)
  (:printer syscall ((opcode #b00000000001010110)))
  (:emitter (emit-syscall-inst segment code)))

(define-bitfield-emitter %emit-cpucfg 32
  (byte 22 10)
  (byte 5 5)
  (byte 5 0))

(defconstant-eqx cpucfg-printer
    '(:name :tab rd ", " rj)
  #'equal)

(define-instruction-format (cpucfg 32 :default-printer cpucfg-printer)
  (opcode :field (byte 22 10))
  (rj :field (byte 5 5) :type 'reg)
  (rd :field (byte 5 0) :type 'reg))

(defun emit-cpucfg-inst (segment rd rj)
    (%emit-cpucfg segment #b0000000000000000011011 (reg-tn-encoding rj) (reg-tn-encoding rd)))

(define-instruction cpucfg (segment rd rj)
  (:printer cpucfg ((opcode #b0000000000000000011011)))
  (:emitter (emit-cpucfg-inst segment rd rj)))

(defconstant-eqx la-amo-printer
  '(:name :tab rd ", " rk ", " rj)
  #'equal)

(define-instruction-format (la-amo 32 :default-printer la-amo-printer)
  (opcode :field (byte 17 15) )
  (rd :field (byte 5 0) :type 'reg)
  (rj :field (byte 5 5) :type 'reg)
  (rk :field (byte 5 10) :type 'reg) )

(macrolet ((define-loongarch64-atomic-instruction (name opcode)
             `(define-instruction ,name (segment rd rk rj)
                (:printer la-amo ((opcode ,opcode)))
                (:emitter
                   (emit-r-inst segment
                                ,opcode
                                rk rj rd)))))
(define-loongarch64-atomic-instruction amcas.b #b00111000010110000)
(define-loongarch64-atomic-instruction amcas.h #b00111000010110001)
(define-loongarch64-atomic-instruction amcas.w #b00111000010110010)
(define-loongarch64-atomic-instruction amcas.d #b00111000010110011)
(define-loongarch64-atomic-instruction amcas_db.b #b00111000010110100)
(define-loongarch64-atomic-instruction amcas_db.h #b00111000010110101)
(define-loongarch64-atomic-instruction amcas_db.w #b00111000010110110)
(define-loongarch64-atomic-instruction amcas_db.d #b00111000010110111)
(define-loongarch64-atomic-instruction amswap.b #b00111000010111000)
(define-loongarch64-atomic-instruction amswap.h #b00111000010111001)
(define-loongarch64-atomic-instruction amadd.b #b00111000010111010)
(define-loongarch64-atomic-instruction amadd.h #b00111000010111011)
(define-loongarch64-atomic-instruction amswap_db.b #b00111000010111100)
(define-loongarch64-atomic-instruction amswap_db.h #b00111000010111101)
(define-loongarch64-atomic-instruction amadd_db.b #b00111000010111110)
(define-loongarch64-atomic-instruction amadd_db.h #b00111000010111111)
(define-loongarch64-atomic-instruction amswap.w #b00111000011000000)
(define-loongarch64-atomic-instruction amswap.d #b00111000011000001)
(define-loongarch64-atomic-instruction amadd.w #b00111000011000010)
(define-loongarch64-atomic-instruction amadd.d #b00111000011000011)
(define-loongarch64-atomic-instruction amand.w #b00111000011000100)
(define-loongarch64-atomic-instruction amand.d #b00111000011000101)
(define-loongarch64-atomic-instruction amor.w #b00111000011000110)
(define-loongarch64-atomic-instruction amor.d #b00111000011000111)
(define-loongarch64-atomic-instruction amxor.w #b00111000011001000)
(define-loongarch64-atomic-instruction amxor.d #b00111000011001001)
(define-loongarch64-atomic-instruction ammax.w #b00111000011001010)
(define-loongarch64-atomic-instruction ammax.d #b00111000011001011)
(define-loongarch64-atomic-instruction ammin.w #b00111000011001100)
(define-loongarch64-atomic-instruction ammin.d #b00111000011001101)
(define-loongarch64-atomic-instruction ammax.wu #b00111000011001110)
(define-loongarch64-atomic-instruction ammax.du #b00111000011001111)
(define-loongarch64-atomic-instruction ammin.wu #b00111000011010000)
(define-loongarch64-atomic-instruction ammin.du #b00111000011010001)
(define-loongarch64-atomic-instruction amswap_db.w #b00111000011010010)
(define-loongarch64-atomic-instruction amswap_db.d #b00111000011010011)
(define-loongarch64-atomic-instruction amadd_db.w #b00111000011010100)
(define-loongarch64-atomic-instruction amadd_db.d #b00111000011010101)
(define-loongarch64-atomic-instruction amand_db.w #b00111000011010110)
(define-loongarch64-atomic-instruction amand_db.d #b00111000011010111)
(define-loongarch64-atomic-instruction amor_db.w #b00111000011011000)
(define-loongarch64-atomic-instruction amor_db.d #b00111000011011001)
(define-loongarch64-atomic-instruction amxor_db.w #b00111000011011010)
(define-loongarch64-atomic-instruction amxor_db.d #b00111000011011011)
(define-loongarch64-atomic-instruction ammax_db.w #b00111000011011100)
(define-loongarch64-atomic-instruction ammax_db.d #b00111000011011101)
(define-loongarch64-atomic-instruction ammin_db.w #b00111000011011110)
(define-loongarch64-atomic-instruction ammin_db.d #b00111000011011111)
(define-loongarch64-atomic-instruction ammax_db.wu #b00111000011100000)
(define-loongarch64-atomic-instruction ammax_db.du #b00111000011100001)
(define-loongarch64-atomic-instruction ammin_db.wu #b00111000011100010)
(define-loongarch64-atomic-instruction ammin_db.du #b00111000011100011))

(defconstant-eqx la-float-printer
  '(:name :tab fd ", " fj ", " fk)
  #'equal)

(defconstant-eqx la-float1-printer
  '(:name :tab fd ", " fj )
  #'equal)

(defconstant-eqx la-float2-printer
  '(:name :tab fd ", " rj )
  #'equal)

(defconstant-eqx la-float3-printer
  '(:name :tab rd ", " fj )
  #'equal)

(defconstant-eqx la-float4-printer
  '(:name :tab fcsr ", " rj )
  #'equal)

(defconstant-eqx la-float5-printer
  '(:name :tab rd ", " fcsr )
  #'equal)

(define-instruction-format (la-float 32 :default-printer la-float-printer)
  (opcode :field (byte 17 15))
  (fk :field (byte 5 10) :type 'fp-reg)
  (fj :field (byte 5 5) :type 'fp-reg)
  (fd :field (byte 5 0) :type 'fp-reg))

(define-instruction-format (la-float1 32 :default-printer la-float1-printer)
  (opcode :field (byte 22 10))
  (fj :field (byte 5 5) :type 'fp-reg)
  (fd :field (byte 5 0) :type 'fp-reg))

(define-instruction-format (la-float2 32 :default-printer la-float2-printer)
  (opcode :field (byte 22 10))
  (rj :field (byte 5 5) :type 'reg)
  (fd :field (byte 5 0) :type 'fp-reg))

(define-instruction-format (la-float3 32 :default-printer la-float3-printer)
  (opcode :field (byte 22 10))
  (fj :field (byte 5 5) :type 'fp-reg)
  (rd :field (byte 5 0) :type 'reg))

(define-instruction-format (la-float4 32 :default-printer la-float4-printer)
  (opcode :field (byte 22 10))
  (rj :field (byte 5 5) :type 'reg)
  (fcsr :field (byte 5 0)))

(define-instruction-format (la-float5 32 :default-printer la-float5-printer)
  (opcode :field (byte 22 10))
  (fcsr :field (byte 5 5))
  (rd :field (byte 5 0) :type 'reg))

(define-bitfield-emitter %emit-fl-inst 32
  (byte 22 10)
  (byte 5 5)
  (byte 5 0))

(defun ensure-reg-tn-encoding (imm/tn)
  (etypecase imm/tn
    ((integer 0 31) imm/tn)
    (tn (reg-tn-encoding imm/tn))))

(defun emit-la-float-inst (segment opcode fd fj fk )
  (%emit-r-inst segment
                opcode
                (reg-tn-encoding fk)
                (reg-tn-encoding fj)
                (reg-tn-encoding fd)))

(defun emit-la-float1-inst (segment opcode fd fj )
  (%emit-fl-inst segment
                opcode
                (reg-tn-encoding fj)
                (reg-tn-encoding fd)))

(defun emit-la-float2-inst (segment opcode fd rj)
  (%emit-fl-inst segment
                opcode
                (reg-tn-encoding rj)
                (reg-tn-encoding fd)))

(defun emit-la-float3-inst (segment opcode rd fj)
  (%emit-fl-inst segment
                opcode
                (reg-tn-encoding fj)
                (reg-tn-encoding rd)))

(defun emit-la-float4-inst (segment opcode fcsr rj)
  (%emit-fl-inst segment
                opcode
                (reg-tn-encoding rj)
                fcsr))

(defun emit-la-float5-inst (segment opcode rd fcsr)
  (%emit-fl-inst segment
                opcode
                 fcsr
                (reg-tn-encoding rd)))

(macrolet ((define-la-float-instruction (name opcode)
            `(define-instruction ,name (segment fd fj fk)
               (:printer la-float
                         ((opcode ,opcode)))
               (:emitter
                (emit-la-float-inst segment ,opcode fd fj fk)))))
  (define-la-float-instruction fadd.s #b00000001000000001)
  (define-la-float-instruction fadd.d #b00000001000000010)
  (define-la-float-instruction fsub.s #b00000001000000101)
  (define-la-float-instruction fsub.d #b00000001000000110)
  (define-la-float-instruction fmul.s #b00000001000001001)
  (define-la-float-instruction fmul.d #b00000001000001010)
  (define-la-float-instruction fdiv.s #b00000001000001101)
  (define-la-float-instruction fdiv.d #b00000001000001110)
  (define-la-float-instruction fmax.s #b00000001000010001)
  (define-la-float-instruction fmax.d #b00000001000010010)
  (define-la-float-instruction fmin.s #b00000001000010101)
  (define-la-float-instruction fmin.d #b00000001000010110)
  (define-la-float-instruction fmaxa.s #b00000001000011001)
  (define-la-float-instruction fmaxa.d #b00000001000011010)
  (define-la-float-instruction fmina.s #b00000001000011101)
  (define-la-float-instruction fmina.d #b00000001000011110)
  (define-la-float-instruction fscaleb.s #b00000001000100001)
  (define-la-float-instruction fscaleb.d #b00000001000100010)
  (define-la-float-instruction fcopysign.s #b00000001000100101)
  (define-la-float-instruction fcopysign.d #b00000001000100110))

(macrolet ((define-la-float1-instruction (name opcode)
            `(define-instruction ,name (segment fd fj)
               (:printer la-float1
                         ((opcode ,opcode)))
               (:emitter
                (emit-la-float1-inst segment ,opcode fd fj)))))
  (define-la-float1-instruction fabs.s #b0000000100010100000001)
  (define-la-float1-instruction fabs.d #b0000000100010100000010)
  (define-la-float1-instruction fneg.s #b0000000100010100000101)
  (define-la-float1-instruction fneg.d #b0000000100010100000110)
  (define-la-float1-instruction flogb.s #b0000000100010100001001)
  (define-la-float1-instruction flogb.d #b0000000100010100001010)
  (define-la-float1-instruction fclass.s #b0000000100010100001101)
  (define-la-float1-instruction fclass.d #b0000000100010100001110)
  (define-la-float1-instruction fsqrt.s #b0000000100010100010001)
  (define-la-float1-instruction fsqrt.d #b0000000100010100010010)
  (define-la-float1-instruction frecip.s #b0000000100010100010101)
  (define-la-float1-instruction frecip.d #b0000000100010100010110)
  (define-la-float1-instruction frsqrt.s #b0000000100010100011001)
  (define-la-float1-instruction frsqrt.d #b0000000100010100011010)
  (define-la-float1-instruction frecipe.s #b0000000100010100011101)
  (define-la-float1-instruction frecipe.d #b0000000100010100011110)
  (define-la-float1-instruction frsqrte.s #b0000000100010100100001)
  (define-la-float1-instruction frsqrte.d #b0000000100010100100010)
  (define-la-float1-instruction fmov.s #b0000000100010100100101)
  (define-la-float1-instruction fmov.d #b0000000100010100100110)
  (define-la-float1-instruction fcvt.s.d #b0000000100011001000110)
  (define-la-float1-instruction fcvt.d.s #b0000000100011001001001)
  (define-la-float1-instruction ftintrm.w.s #b0000000100011010000001)
  (define-la-float1-instruction ftintrm.w.d #b0000000100011010000010)
  (define-la-float1-instruction ftintrm.l.s #b0000000100011010001001)
  (define-la-float1-instruction ftintrm.l.d #b0000000100011010001010)
  (define-la-float1-instruction ftintrp.w.s #b0000000100011010010001)
  (define-la-float1-instruction ftintrp.w.d #b0000000100011010010010)
  (define-la-float1-instruction ftintrp.l.s #b0000000100011010011001)
  (define-la-float1-instruction ftintrp.l.d #b0000000100011010011010)
  (define-la-float1-instruction ftintrz.w.s #b0000000100011010100001)
  (define-la-float1-instruction ftintrz.w.d #b0000000100011010100010)
  (define-la-float1-instruction ftintrz.l.s #b0000000100011010101001)
  (define-la-float1-instruction ftintrz.l.d #b0000000100011010101010)
  (define-la-float1-instruction ftintrne.w.s #b0000000100011010110001)
  (define-la-float1-instruction ftintrne.w.d #b0000000100011010110010)
  (define-la-float1-instruction ftintrne.l.s #b0000000100011010111001)
  (define-la-float1-instruction ftintrne.l.d #b0000000100011010111010)
  (define-la-float1-instruction ftint.w.s #b0000000100011011000001)
  (define-la-float1-instruction ftint.w.d #b0000000100011011000010)
  (define-la-float1-instruction ftint.l.s #b0000000100011011001001)
  (define-la-float1-instruction ftint.l.d #b0000000100011011001010)
  (define-la-float1-instruction ffint.s.w #b0000000100011101000100)
  (define-la-float1-instruction ffint.s.l #b0000000100011101000110)
  (define-la-float1-instruction ffint.d.w #b0000000100011101001000)
  (define-la-float1-instruction ffint.d.l #b0000000100011101001010))

(define-instruction-macro fmove (&optional format fd fs)
  `(case ,format
     (:double (inst fmov.d ,fd ,fs))
     (:single (inst fmov.s ,fd ,fs))))

(define-instruction-macro fadd (&optional format fd fj fk)
  `(case ,format
     (:double (inst fadd.d ,fd ,fj ,fk))
     (:single (inst fadd.s ,fd ,fj ,fk))))

(define-instruction-macro fmul (&optional format fd fj fk)
  `(case ,format
     (:double (inst fmul.d ,fd ,fj ,fk))
     (:single (inst fmul.s ,fd ,fj ,fk))))

(define-instruction-macro fsub (&optional format fd fj fk)
  `(case ,format
     (:double (inst fsub.d ,fd ,fj ,fk))
     (:single (inst fsub.s ,fd ,fj ,fk))))

(define-instruction-macro fdiv (&optional format fd fj fk)
  `(case ,format
     (:double (inst fdiv.d ,fd ,fj ,fk))
     (:single (inst fdiv.s ,fd ,fj ,fk))))

(define-instruction-macro fabs (&optional format fd fj)
  `(case ,format
     (:double (inst fabs.d ,fd ,fj))
     (:single (inst fabs.s ,fd ,fj))))

(define-instruction-macro fneg (&optional format fd fj)
  `(case ,format
     (:double (inst fneg.d ,fd ,fj))
     (:single (inst fneg.s ,fd ,fj))))

(define-instruction-macro ftint (to-format from-format dst src &optional (rm :rne))
  (ecase rm
    (:rz `(inst ,(ecase to-format
                   (:word  (ecase from-format
                              (:single 'ftintrz.l.s)
                              (:double 'ftintrz.l.d)))
                   (:unsigned-word (ecase from-format
                                     (:single 'ftintrz.l.s)
                                     (:double 'ftintrz.l.d))))
             ,dst ,src))
    (:rm `(inst ,(ecase to-format
                   (:word  (ecase from-format
                              (:single 'ftintrm.l.s)
                              (:double 'ftintrm.l.d)))
                   (:unsigned-word (ecase from-format
                                     (:single 'ftintrm.l.s)
                                     (:double 'ftintrm.l.d))))
             ,dst ,src))
    (:rp `(inst ,(ecase to-format
                   (:word  (ecase from-format
                              (:single 'ftintrp.l.s)
                              (:double 'ftintrp.l.d)))
                   (:unsigned-word (ecase from-format
                                     (:single 'ftintrp.l.s)
                                     (:double 'ftintrp.l.d))))
             ,dst ,src))
    (:rne `(inst ,(ecase to-format
                    (:word  (ecase from-format
                               (:single 'ftintrne.l.s)
                               (:double 'ftintrne.l.d)))
                    (:unsigned-word (ecase from-format
                                      (:single 'ftintrne.l.s)
                                      (:double 'ftintrne.l.d))))
              ,dst ,src))))

(define-instruction-macro fcvt (to-format from-format dst src &optional (rm :rne))
  (case to-format
    (:word
     (ecase from-format
       (:single
        `(progn
           (inst ftint :word :single ,dst ,src ,rm)
           (inst movfr2gr.d ,dst ,dst)))
       (:double
        `(progn
           (inst ftint :word :double ,dst ,src ,rm)
           (inst movfr2gr.d ,dst ,dst)))))
    (:single
      (ecase from-format
       (:word
        `(progn
          (inst movgr2fr.d ,dst ,src)
          (inst ffint.s.l ,dst ,dst)))
    (:unsigned-word
     `(progn
        ;; if src < 0 then special handling
        (let ((label-neg (gen-label))
              (label-done (gen-label)))
          (inst blt ,src zero-tn label-neg)
          ;; fast path: fits in signed range
          (inst movgr2fr.d ,dst ,src)
          (inst ffint.s.l ,dst ,dst)
          (inst j label-done)
          (emit-label label-neg)
          ;; emulate unsigned
          ;; t0 = (src >> 1) | (src & 1)
          (inst s_andi t8-tn ,src 1)
          (inst srli.d ,src ,src 1)
          (inst or t8-tn t8-tn ,src)
          (inst movgr2fr.d ,dst t8-tn)
          (inst ffint.s.l ,dst ,dst)
          (inst fadd.s ,dst ,dst ,dst) ; x = x * 2
          (emit-label label-done))))
    (:double
     `(progn
        (inst fcvt.s.d ,dst ,src)))))
    (:double
     (ecase from-format
       (:word
        `(progn
           (inst movgr2fr.d ,dst ,src)
           (inst ffint.d.l ,dst ,dst)))
       (:unsigned-word
     `(progn
        ;; if src < 0 then special handling
        (let ((label-neg (gen-label))
              (label-done (gen-label)))
          (inst blt ,src zero-tn label-neg)
          ;; fast path: fits in signed range
          (inst movgr2fr.d ,dst ,src)
          (inst ffint.d.l ,dst ,dst)
          (inst j label-done)
          (emit-label label-neg)
          ;; emulate unsigned
          ;; t0 = (src >> 1) | (src & 1)
          (inst s_andi t8-tn ,src 1)
          (inst srli.d ,src ,src 1)
          (inst or t8-tn t8-tn ,src)
          (inst movgr2fr.d ,dst t8-tn)
          (inst ffint.d.l ,dst ,dst)
          (inst fadd.d ,dst ,dst ,dst) ; x = x * 2
          (emit-label label-done))))
       (:single
        `(progn
            (inst fcvt.d.s ,dst ,src)))))))

(macrolet ((define-la-float2-instruction (name opcode)
            `(define-instruction ,name (segment fd rj)
               (:printer la-float2
                         ((opcode ,opcode)))
               (:emitter
                (emit-la-float2-inst segment ,opcode fd rj)))))
  (define-la-float2-instruction movgr2fr.w #b0000000100010100101001)
  (define-la-float2-instruction movgr2fr.d #b0000000100010100101010)
  (define-la-float2-instruction movgr2frh.w #b0000000100010100101011))

(macrolet ((define-la-float3-instruction (name opcode)
            `(define-instruction ,name (segment rd fj)
               (:printer la-float3
                         ((opcode ,opcode)))
               (:emitter
                (emit-la-float3-inst segment ,opcode rd fj)))))
  (define-la-float3-instruction movfr2gr.s #b0000000100010100101101)
  (define-la-float3-instruction movfr2gr.d #b0000000100010100101110)
  (define-la-float3-instruction movfrh2gr.s #b0000000100010100101111))

(define-instruction movgr2fcsr (segment fcsr rj)
  (:emitter
   (emit-la-float4-inst segment #b0000000100010100110000 fcsr rj)))

(define-instruction  movfcsr2gr (segment rd fcsr)
  (:emitter
   (emit-la-float5-inst segment #b0000000100010100110010 rd fcsr)))

(define-instruction-macro movgr2fr (&optional format fd rj)
  `(case ,format
     (:double (inst movgr2fr.d ,fd ,rj))
     (:single (inst movgr2fr.w ,fd ,rj))))

(define-instruction-macro fsqrt (&optional format fd fj)
  `(case ,format
     (:double (inst fsqrt.d ,fd ,fj))
     (:single (inst fsqrt.s ,fd ,fj))))

(define-instruction-macro movfr2gr (&optional format rd fj)
  `(case ,format
     (:double (inst movfr2gr.d ,rd ,fj))
     (:single (inst movfr2gr.s ,rd ,fj))))

(defconstant-eqx la-fcmp-printer
  '(:name :tab funct5 "," cd ", " fj ", " fk)
  #'equal)

(define-instruction-format (la-fcmp 32 :default-printer la-fcmp-printer)
  (opcode :field (byte 12 20))
  (funct5 :field (byte 5 15))
  (fk :field (byte 5 10) :type 'fp-reg)
  (fj :field (byte 5 5) :type 'fp-reg)
  (funct2 :field (byte 2 3))
  (cd :field (byte 3 0)))

(define-bitfield-emitter %emit-fcmp-inst 32
  (byte 12 20)
  (byte 5 15)
  (byte 5 10)
  (byte 5 5)
  (byte 2 3)
  (byte 3 0))

(defun emit-la-fcmp-inst (segment opcode funct5 fk fj cd)
  (%emit-fcmp-inst segment
                   opcode
                   funct5
                   (reg-tn-encoding fk)
                   (reg-tn-encoding fj)
                    #b00 cd))

(macrolet ((define-la-float-cmp-instruction (name opcode funct5)
             `(define-instruction ,name (segment cd fj fk)
                (:printer la-fcmp
                  ((opcode ,opcode)
                   (funct5 ,funct5)))
                (:emitter
                  (emit-la-fcmp-inst segment ,opcode ,funct5 fk fj cd)))))
  ;; --------- fcmp.cond.s ----------
  (define-la-float-cmp-instruction fcmp.caf.s  #b000011000001 #b00000)
  (define-la-float-cmp-instruction fcmp.cun.s  #b000011000001 #b01000)
  (define-la-float-cmp-instruction fcmp.ceq.s  #b000011000001 #b00100)
  (define-la-float-cmp-instruction fcmp.cueq.s #b000011000001 #b01100)
  (define-la-float-cmp-instruction fcmp.clt.s  #b000011000001 #b00010)
  (define-la-float-cmp-instruction fcmp.cult.s #b000011000001 #b01010)
  (define-la-float-cmp-instruction fcmp.cle.s  #b000011000001 #b00110)
  (define-la-float-cmp-instruction fcmp.cule.s #b000011000001 #b01110)
  (define-la-float-cmp-instruction fcmp.cne.s  #b000011000001 #b10000)
  (define-la-float-cmp-instruction fcmp.cor.s  #b000011000001 #b10100)
  (define-la-float-cmp-instruction fcmp.cune.s #b000011000001 #b11000)
  (define-la-float-cmp-instruction fcmp.saf.s  #b000011000001 #b00001)
  (define-la-float-cmp-instruction fcmp.sun.s  #b000011000001 #b01001)
  (define-la-float-cmp-instruction fcmp.seq.s  #b000011000001 #b00101)
  (define-la-float-cmp-instruction fcmp.sueq.s #b000011000001 #b01101)
  (define-la-float-cmp-instruction fcmp.slt.s  #b000011000001 #b00011)
  (define-la-float-cmp-instruction fcmp.sult.s #b000011000001 #b01011)
  (define-la-float-cmp-instruction fcmp.sle.s  #b000011000001 #b00111)
  (define-la-float-cmp-instruction fcmp.sule.s #b000011000001 #b01111)
  (define-la-float-cmp-instruction fcmp.sne.s  #b000011000001 #b10001)
  (define-la-float-cmp-instruction fcmp.sor.s  #b000011000001 #b10101)
  (define-la-float-cmp-instruction fcmp.sune.s #b000011000001 #b11001)
  ;; --------- fcmp.cond.d ----------
  (define-la-float-cmp-instruction fcmp.caf.d  #b000011000010 #b00000)
  (define-la-float-cmp-instruction fcmp.cun.d  #b000011000010 #b01000)
  (define-la-float-cmp-instruction fcmp.ceq.d  #b000011000010 #b00100)
  (define-la-float-cmp-instruction fcmp.cueq.d #b000011000010 #b01100)
  (define-la-float-cmp-instruction fcmp.clt.d  #b000011000010 #b00010)
  (define-la-float-cmp-instruction fcmp.cult.d #b000011000010 #b01010)
  (define-la-float-cmp-instruction fcmp.cle.d  #b000011000010 #b00110)
  (define-la-float-cmp-instruction fcmp.cule.d #b000011000010 #b01110)
  (define-la-float-cmp-instruction fcmp.cne.d  #b000011000010 #b10000)
  (define-la-float-cmp-instruction fcmp.cor.d  #b000011000010 #b10100)
  (define-la-float-cmp-instruction fcmp.cune.d #b000011000010 #b11000)
  (define-la-float-cmp-instruction fcmp.saf.d  #b000011000010 #b00001)
  (define-la-float-cmp-instruction fcmp.sun.d  #b000011000010 #b01001)
  (define-la-float-cmp-instruction fcmp.seq.d  #b000011000010 #b00101)
  (define-la-float-cmp-instruction fcmp.sueq.d #b000011000010 #b01101)
  (define-la-float-cmp-instruction fcmp.slt.d  #b000011000010 #b00011)
  (define-la-float-cmp-instruction fcmp.sult.d #b000011000010 #b01011)
  (define-la-float-cmp-instruction fcmp.sle.d  #b000011000010 #b00111)
  (define-la-float-cmp-instruction fcmp.sule.d #b000011000010 #b01111)
  (define-la-float-cmp-instruction fcmp.sne.d  #b000011000010 #b10001)
  (define-la-float-cmp-instruction fcmp.sor.d  #b000011000010 #b10101)
  (define-la-float-cmp-instruction fcmp.sune.d #b000011000010 #b11001))


(defun emit-compute (segment vop dest src lip pc-relative-delta src-relative-delta)
  (labels ((pc-relative-emitter (segment position)
             (multiple-value-bind (u i)
                 (u-and-i-inst-immediate (funcall pc-relative-delta position))
               (assemble (segment vop)
                 (inst pcaddu12i lip u)
                 (inst addi.d dest lip i))))
           (src-relative-emitter (segment position)
             (assemble (segment vop)
               (inst addi.d dest src (funcall src-relative-delta position 0))))
           (maybe-shrink (segment chooser position delta-if-after)
             (declare (ignore chooser))
             (when (and src
                        (typep (funcall src-relative-delta position delta-if-after)
                               'short-immediate))
               (emit-back-patch segment 4 #'src-relative-emitter)
               t)))
    (emit-chooser
     segment 8 2
     #'maybe-shrink
     #'pc-relative-emitter)))

(define-instruction compute-code-from-fn (segment dest src lip label)
  (:vop-var vop)
  (:emitter
   (emit-compute segment vop dest src lip
                 (lambda (position)
                   (- other-pointer-lowtag
                      position
                      (component-header-length)))
                 ;; code = fn - fn-ptr-type - header - label-offset + other-pointer-tag
                 (lambda (position delta-if-after)
                   (- other-pointer-lowtag
                      (+ fun-pointer-lowtag
                         (label-position label position delta-if-after)
                         (component-header-length)))))))

(define-instruction compute-code-from-ra (segment dest src lip label)
  (:vop-var vop)
  (:emitter
   (emit-compute segment vop dest src lip
                 (lambda (position)
                   (- other-pointer-lowtag
                      position
                      (component-header-length)))
                 ;; code = ra - header - label-offset + other-pointer-tag
                 ;;      = ra + other-pointer-tag - (header + label-offset)
                 (lambda (position delta-if-after)
                   (- other-pointer-lowtag
                      (+ (label-position label position delta-if-after)
                         (component-header-length)))))))

(define-instruction compute-ra-from-code (segment dest src lip label)
  (:vop-var vop)
  (:emitter
   (emit-compute segment vop dest src lip
                 (lambda (position)
                   (- (label-position label) position))
                 ;; ra = code - other-pointer-tag + header + label-offset
                 ;;    = code + header + label-offset - other-pointer-tag
                 (lambda (position delta-if-after)
                   (- (+ (label-position label position delta-if-after)
                         (component-header-length))
                      other-pointer-lowtag)))))

(define-instruction load-far-constant (segment dest src)
  (:emitter
   ;; pc - (code - other-pointer-tag) = header + position
   ;; (code - other-pointer-tag) - pc + const = const - header - position
   (let ((offset (- (tn-byte-offset src) (component-header-length))))
     (emit-back-patch
      segment 8
      (lambda (segment position)
        (assemble (segment)
          (multiple-value-bind (u i)
              (u-and-i-inst-immediate (- offset position))
            (inst pcaddu12i lip-tn u)
            (inst ld.d dest lip-tn i))))))))

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

(define-instruction-macro load-layout-id (reg layout)
  `(progn
     (inst .layout-id-fixup ,layout)
     (inst lu12i.w ,reg #xfffff)
     (inst addi.d ,reg ,reg -1)))

(define-instruction .layout-id-fixup (segment layout)
  (:emitter
   (sb-c:note-fixup segment :u+i-type (sb-c:make-fixup layout :layout-id))))

(defun sb-vm:fixup-code-object (code offset value kind flavor)
  (declare (type index offset))
  (unless (zerop (rem offset sb-assem:+inst-alignment-bytes+))
    (error "Unaligned instruction?  offset=#x~X." offset))
  (unless (typep value 'u+i-immediate)
    (error "Tried to fixup with ~a." value))
  (let ((sap (code-instructions code)))
    (multiple-value-bind (u i) (u-and-i-inst-immediate value)
      (ecase kind
        (:absolute
         (setf (sap-ref-32 sap offset) value))
        (:u-type
         (setf (ldb (byte 20 5) (sap-ref-32 sap offset)) u))
        (:i-type
         (let ((imm12 (ash i -2)))
         (setf (ldb (byte 16 10) (sap-ref-32 sap offset)) imm12)))
        (:u+i-type
         (sb-vm:fixup-code-object code offset u :u-type flavor)
         (sb-vm:fixup-code-object code (+ offset 4) i :s-type flavor))
        (:s-type
         (setf (ldb (byte 12 10) (sap-ref-32 sap offset)) i)))))
   nil)

(define-instruction store-coverage-mark (segment mark-index)
  (:emitter
   ;; No backpatch is needed to compute the offset into the code header
   ;; because COMPONENT-HEADER-LENGTH is known at this point.
   (let ((offset (+ (component-header-length)
                    ;; skip over jump table word and entries
                    (* (1+ (component-n-jump-table-entries))
                       n-word-bytes)
                    mark-index
                    (- other-pointer-lowtag))))
     (assemble (segment)
       (inst st.b sb-vm::null-tn sb-vm::code-tn
             (the (unsigned-byte 11) offset))))))
