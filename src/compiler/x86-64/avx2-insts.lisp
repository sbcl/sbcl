(in-package "SB-X86-64-ASM")

(define-arg-type k-vvvv-reg
  :prefilter #'invert-4
  :printer #'print-kreg)

(define-arg-type ymmreg
  :prefilter #'prefilter-reg-r
  :printer #'print-ymmreg)

(define-arg-type ymmreg-b
  :prefilter #'prefilter-reg-b
  :printer #'print-ymmreg)

(define-arg-type ymm-vvvv-reg
  :prefilter #'invert-4
  :printer #'print-ymmreg)

(define-arg-type vvvv-reg
  :prefilter #'invert-4
  :printer #'print-reg)

(define-arg-type ymm-reg-is4
  :printer #'print-ymmreg)

(define-arg-type ymmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-ymmreg/mem)

(define-arg-type half-ymmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-half-ymmreg/mem)

(macrolet ((define-disp-arg-type (name n)
             `(define-arg-type ,name
               :prefilter (lambda (dstate mod r/m)
                            (decode-mod-r/m dstate mod r/m 'fpr :disp-n ,n))
               :printer #'print-ymmreg/mem)))
  (define-disp-arg-type evex-ymmreg/mem-disp1 1)
  (define-disp-arg-type evex-ymmreg/mem-disp2 2)
  (define-disp-arg-type evex-ymmreg/mem-disp4 4)
  (define-disp-arg-type evex-ymmreg/mem-disp8 8)
  (define-disp-arg-type evex-ymmreg/mem-disp16 16)
  (define-disp-arg-type evex-ymmreg/mem-disp32 32)
  (define-disp-arg-type evex-ymmreg/mem-disp64 64))

(define-arg-type vm
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-ymmreg/mem)

(define-arg-type sized-xmmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-sized-xmmreg/mem)

(define-arg-type sized-byte-xmmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-sized-byte-xmmreg/mem)

(define-arg-type sized-word-xmmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-sized-word-xmmreg/mem)

(define-arg-type sized-dword-xmmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-sized-dword-xmmreg/mem)

(define-arg-type sized-xmmreg/mem-default-qword
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-sized-xmmreg/mem-default-qword)
;;; General indicator that we are decoding an EVEX instruction.
;;; EVEX has 128-, 256-, and 512-bit forms; L'L=00 is 128-bit and sets
;;; neither +vex-l+ nor +evex-l1+, so this separate bit is required for
;;; code that needs to know "EVEX vs VEX/legacy".
;;; (Bit 14 is currently unused by other dstate properties.)
(defconstant +evex+ #x4000)
;; EVEX L'L=10 (512-bit) sets bit 11; L'L=01 (256-bit) sets bit 10 (=+vex-l+)
(defconstant +vex-l+ #x400)
;; EVEX R' bit (reg bit 4, for registers 16-31 in ModR/M.reg)
(defconstant +evex-l1+ #x800)
;; EVEX X bit as B' (r/m bit 4, for registers 16-31 in ModR/M.r/m, reg-direct only)
(defconstant +evex-r-prime+ #x1000)
;;; EVEX V' bit (vvvv bit 4, for registers 16-31 in the vvvv field).
;;; The 4-bit vvvv field in the EVEX prefix is extended with this bit.
;;; (Bit 15 is currently unused by other dstate properties.)
(defconstant +evex-v-prime+ #x8000)

(defconstant +vsib+ #x10000)

(define-arg-type evex-vsib/mem
  :prefilter (lambda (dstate mod r/m)
               (dstate-setprop dstate +vsib+)
               (decode-mod-r/m dstate mod r/m 'fpr))
  :printer #'print-vsib/mem)

(define-arg-type evex-vsib-disp4
  :prefilter (lambda (dstate mod r/m)
               (dstate-setprop dstate +vsib+)
               (decode-mod-r/m dstate mod r/m 'fpr :disp-n 4))
  :printer #'print-vsib/mem)

(define-arg-type evex-vsib-disp8
  :prefilter (lambda (dstate mod r/m)
               (dstate-setprop dstate +vsib+)
               (decode-mod-r/m dstate mod r/m 'fpr :disp-n 8))
  :printer #'print-vsib/mem)

(define-arg-type vex-l :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value) +vex-l+ 0))))

(define-arg-type vex-w :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value) +rex-w+ 0))))

(define-arg-type vex-r :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value) 0 +rex-r+))))

(define-arg-type vex-x :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value) 0 +rex-x+))))

(define-arg-type vex-b :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value) 0 +rex-b+))))

(define-arg-type evex-ymmreg-b
  :prefilter (lambda (dstate value)
               (let ((full-reg (if (dstate-getprop dstate +rex-b+) (+ value 8) value)))
                 (get-fpr :xmm
                          (if (and (dstate-getprop dstate +evex+)
                                   (dstate-getprop dstate +rex-x+))
                              (+ full-reg 16)
                              full-reg))))
  :printer #'print-ymmreg-rm)

(defconstant-eqx +avx-conditions+
                 #(:eq :lt :le :unord :neq :nlt :nle :ord :eq_uq :nge :ngt
                   :false :neq_oq :ge :gt :true :eq_os :lt_oq :le_oq :unord_s
                   :neq_us :nlt_uq :nle_uq :ord_s :eq_us :nge_uq :ngt_uq
                   :false_os :neq_os :ge_oq :gt_oq :true_us)
                 #'equalp)

(define-arg-type avx-condition-code
  :type 'imm-byte
  :printer +avx-conditions+)

;;; EVEX arg-types

;; EVEX R' extends ModR/M.reg bit 4 (inverted in prefix)
;; R'=0 in prefix means bit4=1 (register 16-31)

(define-arg-type evex-r-prime
  :prefilter
  (lambda (dstate value)
    (dstate-setprop dstate
                    (if (zerop value)  +evex-r-prime+ 0))))

(define-arg-type evex-fixed
  :prefilter
  (lambda (dstate value)
    (declare (ignore value))
    (dstate-setprop dstate +evex+)))

(define-arg-type evex-v-prime
  :prefilter
  (lambda (dstate value)
    (dstate-setprop dstate
                    (if (zerop value) +evex-v-prime+ 0))))

(define-arg-type evex-ll
  :prefilter
  (lambda (dstate value)
    (dstate-setprop dstate (ash value 10))))

(define-arg-type evex-w
  :prefilter
  (lambda (dstate value)
    (dstate-setprop dstate
                    (if (plusp value) +rex-w+ 0))))

(define-arg-type evex-ymm-vvvv-reg
  :prefilter #'invert-4
  :printer #'print-ymmreg-vvvv)

(define-arg-type opmask-reg
  :printer #'print-opmask-register)

(define-arg-type kreg
  :prefilter
  (lambda (dstate value)
    (declare (ignore dstate))
    (get-fpr :kreg value))
  :printer #'print-kreg)

(define-arg-type kreg/mem
  :prefilter (lambda (dstate mod r/m)
               (if (= mod 3)
                   (get-fpr :kreg r/m)
                   (decode-mod-r/m dstate mod r/m 'gpr)))
  :printer #'print-kreg/mem)

(define-instruction-format (vex2 16)
  (vex
    :field (byte 8 0)
    :value 197)
  (r
    :field (byte 1 (+ 8 7))
    :type 'vex-r)
  (vvvv
    :field (byte 4 (+ 8 3))
    :type 'ymm-vvvv-reg)
  (l
    :field (byte 1 (+ 8 2))
    :type 'vex-l)
  (pp :field (byte 2 (+ 8 0))))

(define-instruction-format (vex3 24)
  (vex
    :field (byte 8 0)
    :value 196)
  (r
    :field (byte 1 (+ 8 7))
    :type 'vex-r)
  (x
    :field (byte 1 (+ 8 6))
    :type 'vex-x)
  (b
    :field (byte 1 (+ 8 5))
    :type 'vex-b)
  (m-mmmm :field (byte 5 (+ 8 0)))
  (w
    :field (byte 1 (+ 16 7))
    :type 'vex-w)
  (vvvv
    :field (byte 4 (+ 16 3))
    :type 'ymm-vvvv-reg)
  (l
    :field (byte 1 (+ 16 2))
    :type 'vex-l)
  (pp :field (byte 2 (+ 16 0))))

(define-instruction-format (vex2-op 24
                             :include vex2
                             :default-printer '(:name))
  (op :field (byte 8 16)))

(defmacro define-vex-instruction-format
          ((format-name length-in-bits &key default-printer include)
           &body arg-specs)
  `(progn
    (define-instruction-format (,(symbolicate "VEX2-" format-name) (+ 16 ,length-in-bits)
                                 :include ,(if include
                                               (symbolicate "VEX2-" include)
                                               'vex2)
                                 :default-printer ,default-printer)
      ,@(subst 16 'start arg-specs))
    (define-instruction-format (,(symbolicate "VEX3-" format-name) (+ 24 ,length-in-bits)
                                 :include ,(if include
                                               (symbolicate "VEX3-" include)
                                               'vex3)
                                 :default-printer ,default-printer)
      ,@(subst 24 'start arg-specs))))

(define-vex-instruction-format (ymm-ymm/mem 16 :default-printer '(:name :tab reg ", " reg/mem))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'ymmreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'ymmreg)
  (imm))

(define-vex-instruction-format (ymm-ymm/mem-imm 16 :default-printer
                                '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'ymmreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'ymmreg)
  (imm :type 'imm-byte))

(define-vex-instruction-format (ymm-ymm/mem-ymm 24 :default-printer
                                '(:name :tab reg ", " vvvv ", " reg/mem ", " reg4))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'ymmreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'ymmreg)
  (reg4
    :field (byte 4 (+ start 16 4))
    :type 'ymm-reg-is4))

(define-vex-instruction-format (ymm-ymm/mem-dir 16
                                 :include ymm-ymm/mem
                                 :default-printer `(:name :tab
                                                    (:if (reg/mem :test machine-ea-p)
                                                     (:if (dir :constant 0) (reg ", " reg/mem) (reg/mem ", " reg))
                                                     (reg ", " vvvv ", " reg/mem))))
  (op :field (byte 7 (+ start 1)))
  (dir :field (byte 1 (+ start 0))))

(define-vex-instruction-format (ymm-ymm-imm 16 :default-printer '(:name :tab vvvv ", " reg ", " imm))
  (op :field (byte 8 (+ start 0)))
  (/i :field (byte 3 (+ start 11)))
  (b11
    :field (byte 2 (+ start 14))
    :value 3)
  (reg
    :field (byte 3 (+ start 8))
    :type 'ymmreg-b)
  (imm :type 'imm-byte))

(define-vex-instruction-format (reg-ymm/mem 16
                                 :include ymm-ymm/mem
                                 :default-printer '(:name :tab reg ", " reg/mem))
  (reg
    :field (byte 3 (+ start 11))
    :type 'reg))

(define-instruction-format (evex 32)
  (evex-prefix
    :field (byte 8 0)
    :value #x62)
  ;; Byte 1
  (r
    :field (byte 1 15)
    :type 'vex-r)
  (x
    :field (byte 1 14)
    :type 'vex-x)
  (b
    :field (byte 1 13)
    :type 'vex-b)
  (r-prime
    :field (byte 1 12)
    :type 'evex-r-prime)
  (reserved
    :field (byte 1 11)
    :value 0) ; bit 11 reserved (0 in EVEX)
  (mm       :field (byte 3 8)) ; bits 10:8 are mmm (maps 1, 2, 3, 5, 6)
  ;; Byte 2
  (w
    :field (byte 1 23)
    :type 'evex-w)
  (vvvv
    :field (byte 4 19)
    :type 'evex-ymm-vvvv-reg)
  (evex-fixed
    :field (byte 1 18)
    :value 1
    :type 'evex-fixed) ; must be 1 for EVEX
  (pp         :field (byte 2 16))
  ;; Byte 3
  (z-bit   :field (byte 1 31))
  (ll
    :field (byte 2 29)
    :type 'evex-ll)
  (evex-b  :field (byte 1 28))
  (v-prime
    :field (byte 1 27)
    :type 'evex-v-prime)
  (aaa
    :field (byte 3 24)
    :type 'opmask-reg))

(define-vex-instruction-format (kreg-kreg/mem 16 :default-printer '(:name :tab reg ", " reg/mem))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'kreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'kreg))

(define-vex-instruction-format (kreg-reg/mem 16 :default-printer '(:name :tab reg ", " reg/mem))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'reg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'kreg))

(define-vex-instruction-format (reg-kreg/mem 16 :default-printer '(:name :tab reg ", " reg/mem))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'kreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'reg))

(define-vex-instruction-format (kreg-kreg/mem-k 16 :default-printer '(:name :tab reg ", " vvvv ", " reg/mem))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'kreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'kreg)
  (vvvv :type 'k-vvvv-reg))

(define-vex-instruction-format (kreg-kreg/mem-imm 16 :default-printer
                                '(:name :tab reg ", " reg/mem ", " imm))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'kreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'kreg)
  (imm :type 'imm-byte))

(defmacro define-evex-instruction-format
          ((format-name length-in-bits &key default-printer include)
           &body arg-specs)
  `(define-instruction-format (,(symbolicate "EVEX-" format-name) (+ 32 ,length-in-bits)
                                :include ,(if include
                                              (symbolicate "EVEX-" include)
                                              'evex)
                                :default-printer ,default-printer)
    ,@(subst 32 'start arg-specs)))

(define-evex-instruction-format (ymm-ymm-imm 16
                                 :default-printer '(:name :tab vvvv ", " reg ", " imm))
  (op  :field (byte 8 (+ start 0)))
  (/i  :field (byte 3 (+ start 11)))
  (b11
    :field (byte 2 (+ start 14))
    :value #b11)
  (reg
    :field (byte 3 (+ start 8))
    :type 'evex-ymmreg-b)
  (imm :type 'imm-byte))

(define-evex-instruction-format (kreg-reg/mem 16 :default-printer '(:name :tab reg ", " vvvv ", " reg/mem))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'reg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'kreg))

(define-evex-instruction-format (kreg-reg/mem-imm 16 :default-printer
                                 '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'reg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'kreg)
  (imm :type 'imm-byte))

(define-evex-instruction-format (ymm-ymm/mem 16 :default-printer '(:name :tab reg ", " reg/mem))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'ymmreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'ymmreg)
  (imm))

(define-evex-instruction-format (ymm-ymm/mem-imm 16 :default-printer
                                 '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'ymmreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'ymmreg)
  (imm :type 'imm-byte))

(define-evex-instruction-format (ymm-ymm/mem-dir 16
                                  :include ymm-ymm/mem
                                  :default-printer `(:name :tab
                                                     (:if (reg/mem :test machine-ea-p)
                                                      (:if (dir :constant 0) (reg ", " reg/mem) (reg/mem ", " reg))
                                                      (reg ", " vvvv ", " reg/mem))))
  (op :field (byte 7 (+ start 1)))
  (dir :field (byte 1 (+ start 0))))

(define-evex-instruction-format (reg-ymm/mem 16
                                  :include ymm-ymm/mem
                                  :default-printer '(:name :tab reg ", " reg/mem))
  (reg
    :field (byte 3 (+ start 11))
    :type 'reg))

(define-evex-instruction-format (ymm-ymm/mem-ymm 24 :default-printer
                                 '(:name :tab reg ", " vvvv ", " reg/mem ", " reg4))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'ymmreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'ymmreg)
  (reg4
    :field (byte 4 (+ start 16 4))
    :type 'ymm-reg-is4))

(define-evex-instruction-format (2mask-nds 16 :default-printer
                                 '(:name :tab reg ", " aaa ", " vvvv ", " reg/mem))
  (op :field (byte 8 (+ start 0)))
  (reg/mem
    :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
    :type 'ymmreg/mem)
  (reg
    :field (byte 3 (+ start 11))
    :type 'kreg))

(define-instruction-format (evex-vex-gpr (+ 32 16)
                             :include evex
                             :default-printer '(:name :tab reg ", " vvvv ", " reg/mem))
  (op :field (byte 8 (+ 32 0)))
  (vvvv :type 'vvvv-reg)
  (reg/mem
    :fields (list (byte 2 (+ 32 14)) (byte 3 (+ 32 8)))
    :type 'reg/mem)
  (reg
    :field (byte 3 (+ 32 11))
    :type 'reg))

;;; APX EVEX arg-types and instruction formats

(define-arg-type apx-evex-r
  :prefilter (lambda (dstate value)
               (when (zerop value)
                 (dstate-setprop dstate +rex-r+))))

(define-arg-type apx-evex-x
  :prefilter (lambda (dstate value)
               (when (zerop value)
                 (dstate-setprop dstate +rex-x+))))

(define-arg-type apx-evex-b
  :prefilter (lambda (dstate value)
               (when (zerop value)
                 (dstate-setprop dstate +rex-b+))))

(define-arg-type apx-evex-r-prime
  :prefilter (lambda (dstate value)
               (when (zerop value)
                 (dstate-setprop dstate +rex2-r4+))))

(define-arg-type apx-evex-b4
  :prefilter (lambda (dstate value)
               (when (plusp value)
                 (dstate-setprop dstate +rex2-b4+))))

(define-arg-type apx-evex-x4-prime
  :prefilter (lambda (dstate value)
               (when (zerop value)
                 (dstate-setprop dstate +rex2-x4+))))

(define-arg-type apx-evex-w
  :prefilter (lambda (dstate value)
               (when (plusp value)
                 (dstate-setprop dstate +rex-w+))))

(define-arg-type apx-evex-pp
  :prefilter (lambda (dstate value)
               (when (= value 1)
                 (dstate-setprop dstate +operand-size-16+))))

(define-arg-type apx-evex-v-prime
  :prefilter (lambda (dstate value)
               (when (zerop value)
                 (dstate-setprop dstate +evex-v-prime+))))

(define-arg-type apx-gpr-vvvv
  :printer #'print-apx-gpr-vvvv)

(define-arg-type apx-gpr-vvvv-default-qword
  :printer #'print-apx-gpr-vvvv-default-qword)

(define-arg-type apx-nf
  :printer #'print-apx-nf)

;;; APX EVEX base format: 32 bits (prefix #x62, Map 4)
(define-instruction-format (apx-evex 32)
  (evex-prefix :field (byte 8 0) :value #x62)
  ;; Byte 1
  (r        :field (byte 1 15) :type 'apx-evex-r)
  (x        :field (byte 1 14) :type 'apx-evex-x)
  (b        :field (byte 1 13) :type 'apx-evex-b)
  (r-prime  :field (byte 1 12) :type 'apx-evex-r-prime)
  (b4       :field (byte 1 11) :type 'apx-evex-b4)
  (mm       :field (byte 3 8)  :value #b100) ; Map 4
  ;; Byte 2
  (w        :field (byte 1 23) :type 'apx-evex-w)
  (vvvv     :field (byte 4 19) :type 'apx-gpr-vvvv)
  (x4-prime :field (byte 1 18) :type 'apx-evex-x4-prime)
  (pp       :field (byte 2 16) :type 'apx-evex-pp)
  ;; Byte 3
  (z-bit    :field (byte 1 31))
  (ll       :field (byte 2 29))
  (nd       :field (byte 1 28))
  (v-prime  :field (byte 1 27) :type 'apx-evex-v-prime)
  (nf       :field (byte 1 26) :type 'apx-nf)
  (aaa      :field (byte 2 24)))

(defmacro define-apx-instruction-format ((format-name length-in-bits
                                          &key default-printer include)
                                         &body arg-specs)
  `(define-instruction-format (,(symbolicate "APX-" format-name) (+ 32 ,length-in-bits)
                               :include ,(if include
                                             (symbolicate "APX-" include)
                                             'apx-evex)
                               :default-printer ,default-printer)
     ,@(subst 32 'start arg-specs)))

(define-apx-instruction-format (ndd-reg-reg 16
                                :default-printer '(nf :name :tab vvvv ", " reg/mem ", " reg))
  (op      :field (byte 8 (+ start 0)))
  (width   :field (byte 1 (+ start 0)) :type 'width)
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'reg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'reg))

(define-apx-instruction-format (ndd-reg-mem 16
                                :default-printer '(nf :name :tab vvvv ", " reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (width   :field (byte 1 (+ start 0)) :type 'width)
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'reg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'reg))

(define-apx-instruction-format (ndd-reg-imm 16
                                :default-printer '(nf :name :tab vvvv ", " reg/mem ", " imm))
  (op      :field (byte 8 (+ start 0)))
  (width   :field (byte 1 (+ start 0)) :type 'width)
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 (+ start 11)))
  (imm     :type 'signed-imm-data))

(define-apx-instruction-format (ndd-reg-imm8 16
                                :default-printer '(nf :name :tab vvvv ", " reg/mem ", " imm))
  (op      :field (byte 8 (+ start 0)))
  (width   :field (byte 1 (+ start 0)) :type 'width)
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 (+ start 11)))
  (imm     :type 'signed-imm-byte))

(define-apx-instruction-format (ndd-unary 16
                                :default-printer '(nf :name :tab vvvv ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (width   :field (byte 1 (+ start 0)) :type 'width)
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 (+ start 11))))

(define-apx-instruction-format (ndd-shift-imm 16
                                :default-printer '(nf :name :tab vvvv ", " reg/mem ", " imm))
  (op      :field (byte 8 (+ start 0)))
  (width   :field (byte 1 (+ start 0)) :type 'width)
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 (+ start 11)))
  (imm     :type 'imm-byte))

(define-apx-instruction-format (ndd-shift-1 16
                                :default-printer '(nf :name :tab vvvv ", " reg/mem ", 1"))
  (op      :field (byte 8 (+ start 0)))
  (width   :field (byte 1 (+ start 0)) :type 'width)
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 (+ start 11))))

(define-apx-instruction-format (ndd-shift-cl 16
                                :default-printer '(nf :name :tab vvvv ", " reg/mem ", CL"))
  (op      :field (byte 8 (+ start 0)))
  (width   :field (byte 1 (+ start 0)) :type 'width)
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 (+ start 11))))

(define-apx-instruction-format (push2 16
                                :default-printer '(:name :tab vvvv ", " reg/mem))
  (vvvv    :type 'apx-gpr-vvvv-default-qword)
  (op      :field (byte 8 (+ start 0)) :value #xFF)
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'sized-reg/mem-default-qword)
  (reg     :field (byte 3 (+ start 11)) :value 6))

(define-apx-instruction-format (pop2 16
                                :default-printer '(:name :tab vvvv ", " reg/mem))
  (vvvv    :type 'apx-gpr-vvvv-default-qword)
  (op      :field (byte 8 (+ start 0)) :value #x8F)
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'sized-reg/mem-default-qword)
  (reg     :field (byte 3 (+ start 11)) :value 0))

(define-instruction-format (apx-cfcmov (+ 32 16)
                             :include apx-evex
                             :default-printer '('cfcmov cc :tab reg ", " reg/mem))
  (op      :field (byte 4 (+ 32 4)) :value #x4)
  (cc      :field (byte 4 (+ 32 0)) :type 'condition-code)
  (reg/mem :fields (list (byte 2 (+ 32 14)) (byte 3 (+ 32 8)))
           :type 'reg/mem)
  (reg     :field (byte 3 (+ 32 11))
           :type 'reg))

(define-instruction-format (apx-ccmp (+ 32 16)
                             :include apx-evex
                             :default-printer '('ccmp scc :tab reg/mem ", " reg ", " dfv))
  (scc     :field (byte 4 24) :type 'condition-code)
  (dfv     :field (byte 4 19))
  (op      :field (byte 8 (+ 32 0)))
  (reg/mem :fields (list (byte 2 (+ 32 14)) (byte 3 (+ 32 8)))
           :type 'reg/mem)
  (reg     :field (byte 3 (+ 32 11))
           :type 'reg))

(define-instruction-format (apx-ccmp-imm (+ 32 16)
                                 :include apx-evex
                                 :default-printer '('ccmp scc :tab reg/mem ", " imm ", " dfv))
  (scc     :field (byte 4 24) :type 'condition-code)
  (dfv     :field (byte 4 19))
  (op      :field (byte 8 (+ 32 0)))
  (reg/mem :fields (list (byte 2 (+ 32 14)) (byte 3 (+ 32 8)))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 (+ 32 11)))
  (imm     :type 'signed-imm-data))

(define-instruction-format (apx-ccmp-imm8 (+ 32 16)
                                  :include apx-evex
                                  :default-printer '('ccmp scc :tab reg/mem ", " imm ", " dfv))
  (scc     :field (byte 4 24) :type 'condition-code)
  (dfv     :field (byte 4 19))
  (op      :field (byte 8 (+ 32 0)))
  (reg/mem :fields (list (byte 2 (+ 32 14)) (byte 3 (+ 32 8)))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 (+ 32 11)))
  (imm     :type 'signed-imm-byte))

(define-instruction-format (apx-ctest (+ 32 16)
                             :include apx-evex
                             :default-printer '('ctest scc :tab reg/mem ", " reg ", " dfv))
  (scc     :field (byte 4 24) :type 'condition-code)
  (dfv     :field (byte 4 19))
  (op      :field (byte 8 (+ 32 0)))
  (reg/mem :fields (list (byte 2 (+ 32 14)) (byte 3 (+ 32 8)))
           :type 'reg/mem)
  (reg     :field (byte 3 (+ 32 11))
           :type 'reg))

(define-instruction-format (apx-ctest-imm (+ 32 16)
                                  :include apx-evex
                                  :default-printer '('ctest scc :tab reg/mem ", " imm ", " dfv))
  (scc     :field (byte 4 24) :type 'condition-code)
  (dfv     :field (byte 4 19))
  (op      :field (byte 8 (+ 32 0)))
  (reg/mem :fields (list (byte 2 (+ 32 14)) (byte 3 (+ 32 8)))
           :type 'sized-reg/mem)
  (reg     :field (byte 3 (+ 32 11)))
  (imm     :type 'signed-imm-data))


(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun vex-encode-pp (pp)
    (ecase pp
      ((nil 0) 0)
      ((#x66 1) #b01)
      ((#xF3 2) #b10)
      ((#xF2 3) #b11)))

  (defun vex-encode-m-mmmm (m-mmmm)
    (ecase m-mmmm
      (#x0F #b00001)
      (#x0F38 #b00010)
      (#x0F3A #b00011)))

  (defun evex-encode-mm (m-mmmm)
    (ecase m-mmmm
      (#x0F   #b001)
      (#x0F38 #b010)
      (#x0F3A #b011)
      ((:map4 4) #b100)
      ((:map5 5) #b101)
      ((:map6 6) #b110)
      ((:map7 7) #b111))))

(defun emit-two-byte-vex (segment r vvvv l pp)
  (emit-bytes segment
              #xC5
              (logior (ash (logxor 1 r) 7)
                      (ash (logxor vvvv #b1111)
                           3)
                      (ash L 2)
                      (vex-encode-pp pp))))

(defun emit-three-byte-vex (segment r x b m-mmmm w vvvv l pp)
  (emit-bytes segment
              #xC4
              (logior (ash (logxor 1 r) 7)
                      (ash (logxor 1 x) 6)
                      (ash (logxor 1 b) 5)
                      (vex-encode-m-mmmm m-mmmm))
              (logior (ash w 7)
                      (ash (logxor vvvv #b1111) 3)
                      (ash L 2)
                      (vex-encode-pp pp))))

(defun determine-vex-flags (thing reg l)
  (flet ((reg-7-p (reg-id)
           (if (<= (reg-id-num reg-id) 7)
               0
               1))
         (xmm-size (r)
           (cond ((is-ymm-id-p (reg-id r))
                  1)
                 ((xmm-register-p r)
                  0))))
    (let ((l (cond ((eq l :from-thing)
                    (xmm-size thing))
                   (l)
                   ((xmm-register-p reg)
                    (xmm-size reg))
                   ((xmm-register-p thing)
                    (xmm-size thing))
                   (t
                    0)))
          (r (if (or (null reg)
                     (integerp reg))
                 0
                 (reg-7-p (reg-id reg))))
          (x (cond ((and (ea-p thing)
                         (ea-index thing))
                    (let ((index (ea-index thing)))
                      (cond ((gpr-p index)
                             (reg-7-p (reg-id (tn-reg index))))
                            ((<= (tn-offset index) 7)
                             0)
                            (t
                             1))))
                   (t 0)))
          (b
            (reg-7-p
             (cond ((ea-p thing)
                    (let ((base (ea-base thing)))
                      (if (and base (neq base rip-tn))
                          (reg-id (if (register-p base) base (tn-reg base)))
                          0)))
                   ((register-p thing)
                    (reg-id thing))
                   (t 0)))))
      (values l r x b))))

(defun emit-vex (segment vvvv thing reg prefix opcode-prefix l w)
  (multiple-value-bind (l r x b) (determine-vex-flags thing reg l)
    (let ((vvvv (if vvvv
                    (reg-id-num (reg-id vvvv))
                    0)))
      (if (and (= 0 x w b)
               (= opcode-prefix #x0F))
          (emit-two-byte-vex segment r vvvv l prefix)
          (emit-three-byte-vex segment r x b opcode-prefix
                               w vvvv l prefix)))))

;;; EVEX prefix encoding for AVX-512
;;; EVEX is a 4-byte prefix: 62h | P1 | P2 | P3
;;; P1: R(7) X(6) B(5) R'(4) 0(3) mmm(2:0)
;;; P2: W(7) vvvv(6:3) 1(2) pp(1:0)
;;; P3: z(7) L'(6) L(5) b(4) V'(3) aaa(2:0)
;;; R, X, B, R', V' are inverted. vvvv is inverted.

(defun emit-evex (segment r x b r-prime opcode-prefix w vvvv pp z ll evex-b v-prime aaa
                  &key (b-prime 0) (x-prime 0) (nd 0) (nf 0) scc dfv)
  (emit-bytes segment
              #x62
              ;; P1: R X B R' B4 mmm
              (logior (ash (logxor 1 r) 7)
                      (ash (logxor 1 x) 6)
                      (ash (logxor 1 b) 5)
                      (ash (logxor 1 r-prime) 4)
                      (ash (if (eql b-prime 1) 1 0) 3)
                      (evex-encode-mm opcode-prefix))
              ;; P2: W vvvv ~X4 pp
              (logior (ash (if (eql w 1) 1 0) 7)
                      (if dfv
                          (ash (logand dfv #b1111) 3)
                          (ash (logandc1 vvvv #b1111) 3))
                      (ash (logxor 1 (if (eql x-prime 1) 1 0)) 2)
                      (vex-encode-pp pp))
              ;; P3: z L'L b/ND V' aaa/SCC/NF
              (if scc
                  (logior (ash (if (eql nd 1) 1 0) 4)
                          (logand scc #b1111))
                  (logior (ash z 7)
                          (ash ll 5)
                          (ash (if (plusp nd) nd evex-b) 4)
                          (ash (logxor 1 (if (eql v-prime 1) 1 0)) 3)
                          (if (plusp nf) (logior (ash nf 2) aaa) aaa)))))

(defun operand-reg-num (r)
  (cond ((register-p r) (reg-id-num (reg-id r)))
        ((and (tn-p r) (tn-offset r)) (tn-offset r))
        (t nil)))

(defun operand-bit3 (r)
  (let ((num (operand-reg-num r)))
    (if (and num (logbitp 3 num)) 1 0)))

(defun operand-bit4 (r)
  (let ((num (operand-reg-num r)))
    (if (and num (logbitp 4 num)) 1 0)))

(defun determine-evex-flags (thing reg ll vvvv &optional vsib)
  "Extract EVEX prefix flags from operands.
Returns: ll, r, x, b, r-prime, v-prime, b-prime, x-prime.
EVEX uses independent bit3 (R/B) and bit4 (R'/X) for 32-register encoding.
VSIB, when true, means THING's EA-index is a vector register (gather/
scatter addressing) rather than a GPR index or APX-extended GPR base."
  (flet ((fpr-size (r)
           (cond ((is-zmm-id-p (reg-id r)) #b10)
                 ((is-ymm-id-p (reg-id r)) #b01)
                 ((xmm-register-p r)       #b00))))
    (let* ((ll (cond (ll ll)
                     ;; Skip k-registers - they pass xmm-register-p but
                     ;; aren't XMM/YMM/ZMM, so fpr-size returns wrong value
                     ((and reg (xmm-register-p reg)
                               (not (is-kreg-id-p (reg-id reg))))
                      (fpr-size reg))
                     ((and (register-p thing) (xmm-register-p thing)
                           (not (is-kreg-id-p (reg-id thing))))
                      (fpr-size thing))
                     ;; Also check vvvv as fallback
                     ((and vvvv (register-p vvvv) (xmm-register-p vvvv))
                      (fpr-size vvvv))
                     (t #b00)))
           ;; R from reg (ModR/M reg field) - bit 3
           (r (operand-bit3 reg))
           ;; R' from reg - bit 4
           (r-prime (if (or (null reg) (k-register-p reg)) 0 (operand-bit4 reg)))
           (ea-p (ea-p thing))
           (base (and ea-p (ea-base thing)))
           (index (and ea-p (ea-index thing)))
           ;; X from EA index, or bit 4 of r/m reg for register-direct.
           ;; In EVEX, X doubles as B' (bit 4 of r/m) when mod=11 (reg-direct).
           ;; For VSIB, the index register is a vector register handled via
           ;; its own bit3/bit4 (X/V') below, not through this path.
           (x (cond (vsib 0)
                    (index (operand-bit3 index))
                    ((register-p thing) (operand-bit4 thing))
                    (t 0)))
           (x-prime (if (and index (not vsib)) (operand-bit4 index) 0))
           ;; B from thing (ModR/M r/m field) - bit 3
           (b (cond (ea-p
                     (if (and base (neq base rip-tn))
                         (operand-bit3 base)
                         0))
                    ((register-p thing)
                     (operand-bit3 thing))
                    (t 0)))
           ;; B' from EA base (bit 4 of base register)
           (b-prime (cond (ea-p
                           (if (and base (neq base rip-tn))
                               (operand-bit4 base)
                               0))
                          (t 0)))
           ;; V' from vvvv - bit 4 of vvvv register number. For VSIB, the
           ;; vector index register's bit 4 is carried in V' instead.
           (v-prime (if vsib
                        (if index (operand-bit4 index) 0)
                        (operand-bit4 vvvv))))
      (values ll r x b r-prime v-prime b-prime x-prime))))

(defun emit-avx512-inst (segment thing reg prefix opcode
                         &key (remaining-bytes 0)
                              ll
                              (opcode-prefix #x0F)
                              (w 0)
                              vvvv
                              (aaa 0)
                              (z 0)
                              (evex-b 0)
                              vm
                              (disp-n 0))
  "Emit an EVEX-encoded instruction.
DISP-N is the compressed displacement scale factor (Intel tuple N).
Common values: 64 for full 512-bit loads, 32 for 256-bit or half-vector,
16 for 128-bit or quarter-vector, 8 for qword, 4 for dword.
Default is 0 (force disp32, never use disp8) for safety -- the CPU
always applies compression to EVEX disp8, so using the wrong N
produces silently wrong addresses.
VM, when supplied, is the VSIB vector index register; it is threaded
into DETERMINE-EVEX-FLAGS as the VSIB flag so gather/scatter addressing
computes X/V' from the vector index rather than from THING's GPR index."
  (multiple-value-bind (ll r x b r-prime v-prime b-prime x-prime)
      (determine-evex-flags thing reg ll vvvv vm)
    (let ((vvvv-num (or (operand-reg-num vvvv) 0)))
      (emit-evex segment r x b r-prime opcode-prefix w
                 vvvv-num prefix z ll evex-b v-prime aaa
                 :b-prime b-prime :x-prime x-prime))
    (emit-bytes segment opcode)
    (emit-ea segment thing reg
             :remaining-bytes remaining-bytes
             :xmm-index vm
             :disp-n disp-n)))

(defun emit-avx2-inst (segment thing reg prefix opcode
                       &key (remaining-bytes 0)
                            l
                            (opcode-prefix #x0F)
                            (w 0)
                            evex-w
                            vvvv
                            is4
                            vm
                            (disp-n 0))
  ;; Auto-detect ZMM operands or APX extended GPR operands and delegate to EVEX encoding
  (flet ((evex-reg-p (r)
           (or (and (register-p r)
                    (or (is-zmm-id-p (reg-id r))
                        (is-kreg-id-p (reg-id r))))
               (let ((num (operand-reg-num r)))
                 (and num (>= num 16))))))
    (when (or (evex-reg-p reg)
              (evex-reg-p thing)
              (evex-reg-p vvvv)
              (and (ea-p thing)
                   (or (and (ea-base thing) (evex-reg-p (ea-base thing)))
                       (and (ea-index thing) (evex-reg-p (ea-index thing))))))
      (return-from emit-avx2-inst
        (emit-avx512-inst segment thing reg prefix opcode
                          :remaining-bytes remaining-bytes
                          ;; Don't pass VEX L as EVEX L'L; let determine-evex-flags
                          ;; auto-detect the vector length from register types.
                          ;; However, if an operand is a GPR (vmovd/vmovq), EVEX L'L must be 0 (128-bit).
                          :ll (if (or (gpr-p reg) (gpr-p thing)) 0 nil)
                          :opcode-prefix opcode-prefix
                          :w (or evex-w w)
                          :vvvv vvvv
                          :vm vm
                          ;; Force disp32 (disp-n=0 disables disp8 compression)
                          ;; unless the caller passed an explicit disp-n --
                          ;; the correct N depends on tuple type, which a
                          ;; generic auto-promoted VEX instruction doesn't
                          ;; know, but some callers (e.g. narrowing
                          ;; conversions) compute it themselves.
                          :disp-n disp-n))))
  (emit-vex segment vvvv thing reg prefix opcode-prefix l w)
  (emit-bytes segment opcode)
  (when is4
    (incf remaining-bytes))
  ;; FIXME: :xmm-index should be removed and we should alter the EA
  ;; to have the proper FPR as the index reg when appropriate.
  (emit-ea segment thing reg :remaining-bytes remaining-bytes :xmm-index vm)
  (when is4
    (emit-byte segment (ash (reg-id-num (reg-id is4)) 4))))

(defun emit-avx512-inst-imm (segment thing reg imm prefix opcode /i
                             &key ll
                                  (w 0)
                                  (opcode-prefix #x0F)
                                  (aaa 0) (z 0) (evex-b 0))
  "Emit EVEX-encoded instruction with /i field and immediate byte.
THING is the destination (NDD, encoded in vvvv).
REG is the source (encoded in ModR/M.r/m).
/I is encoded in ModR/M.reg bits 5:3."
  (aver (<= 0 /i 7))
  ;; thing = destination -> goes in vvvv (NDD encoding)
  ;; reg = source -> goes in r/m
  (multiple-value-bind (ll r x b r-prime v-prime)
      (determine-evex-flags reg nil ll thing)
    (let ((vvvv-num (if thing (reg-id-num (reg-id thing)) 0)))
      (emit-evex segment r x b r-prime opcode-prefix w
                 vvvv-num prefix z ll evex-b v-prime aaa)))
  (emit-bytes segment opcode)
  (emit-byte segment (logior (ash (logior #b11000 /i) 3)
                             (reg-encoding reg segment)))
  (emit-byte segment imm))

(defun emit-avx2-inst-imm (segment thing reg imm prefix opcode /i
                           &key l
                                (w 0)
                                evex-w
                                (opcode-prefix #x0F))
  (aver (<= 0 /i 7))
  (flet ((evex-reg-p (r)
           (and (register-p r)
                (or (is-zmm-id-p (reg-id r))
                    (>= (reg-id-num (reg-id r)) 16)))))
    ;; Auto-detect ZMM operands and delegate to EVEX encoding
    (when (or (evex-reg-p reg)
              (evex-reg-p thing))
      (return-from emit-avx2-inst-imm
        (emit-avx512-inst-imm segment thing reg imm prefix opcode /i
                              :ll l :w (or evex-w w) :opcode-prefix opcode-prefix))))
  (emit-vex segment thing reg nil prefix opcode-prefix l w)
  (emit-bytes segment opcode)
  (emit-byte segment (logior (ash (logior #b11000 /i) 3)
                             (reg-encoding reg segment)))
  (emit-byte segment imm))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun avx512-inst-printer-list
         (inst-format-stem prefix opcode
          &key more-fields printer (opcode-prefix 15) reg-mem-size
          xmmreg-mem-size w ll nds disp-n (evex-b 0))
    (let* ((format-name (symbolicate "EVEX-" inst-format-stem))
           (reg/mem-override (assoc 'reg/mem more-fields))
           (fields
            `((pp ,(vex-encode-pp prefix)) (mm ,(evex-encode-mm opcode-prefix))
              (op ,opcode)
              ,@(when (member 'aaa more-fields :key #'car)
                  `((aaa ,(second (assoc 'aaa more-fields)))))
              ,@(when w `((w ,w))) ,@(when ll `((ll ,ll))) (evex-b ,evex-b)
              ;; A memory operand's displacement, when mod=01, is compressed
              ;; as DISP8*N (N = the tuple size, in bytes). The reg/mem arg
              ;; must use the disp-n-aware variant of its type so that the
              ;; disassembler decodes the same displacement that was encoded.
              ;; Only formats whose REG/MEM arg is actually a vector
              ;; register-or-memory operand need this -- e.g. YMM-YMM-IMM's
              ;; only operand besides VVVV is a register-only "REG" arg, and
              ;; overriding a nonexistent REG/MEM arg would fabricate a
              ;; bogus field with no bits to extract. A fixed-size GPR or
              ;; XMM reg/mem (REG-MEM-SIZE / XMMREG-MEM-SIZE) isn't part of
              ;; the vector tuple-size scheme at all, and takes priority.
              ,@(cond (reg/mem-override (list reg/mem-override))
                      (xmmreg-mem-size
                       `((reg/mem nil :type
                          ',(case xmmreg-mem-size
                              (:qword 'sized-xmmreg/mem-default-qword)
                              (:dword 'sized-dword-xmmreg/mem)
                              (:word 'sized-word-xmmreg/mem)
                              (:byte 'sized-byte-xmmreg/mem)
                              (:sized 'sized-xmmreg/mem)))))
                      (reg-mem-size
                       `((reg/mem nil :type
                          ',(case reg-mem-size
                              (:qword 'sized-reg/mem-default-qword)
                              (:dword 'sized-dword-reg/mem)
                              (:word 'sized-word-reg/mem)
                              (:byte 'sized-byte-reg/mem)
                              (:sized 'sized-reg/mem)))))
                      ((and disp-n
                            (member inst-format-stem
                                    '(ymm-ymm/mem ymm-ymm/mem-imm
                                      ymm-ymm/mem-dir ymm-ymm/mem-ymm
                                      reg-ymm/mem 2mask-nds)))
                       `((reg/mem nil :type
                          ',(symbolicate! "SB-X86-64-ASM"
                                          "EVEX-YMMREG/MEM-DISP"
                                          (princ-to-string disp-n))))))
              ,@(remove-if
                 (lambda (field)
                   (or (eq (car field) 'aaa) (eq (car field) 'reg/mem)
                       (eq (car field) 'disp-n)))
                 more-fields))))
      (list `(:printer ,format-name ,fields
              ,@(cond (printer `(',printer))
                      ((eq nds 'to-mem)
                       `('(:name :tab reg/mem ", " vvvv ", " reg)))
                      (nds `('(:name :tab reg ", " vvvv ", " reg/mem))))))))
  (defun avx2-inst-printer-list
         (inst-format-stem prefix opcode
          &key more-fields printer (opcode-prefix 15) reg-mem-size
          xmmreg-mem-size w l nds disp-n (auto-evex t) evex)
    (let* ((more-fields (remove 'aaa more-fields :key #'car))
           (evex-more-fields (remove 'reg/mem more-fields :key #'car))
           (fields
            `((pp ,(vex-encode-pp prefix))
              (m-mmmm ,(vex-encode-m-mmmm opcode-prefix)) (op ,opcode)
              ,@(and w `((w ,w))) ,@(and l `((l ,l)))
              ,@(cond
                 (xmmreg-mem-size
                  `((reg/mem nil :type
                     ',(case xmmreg-mem-size
                         (:qword 'sized-xmmreg/mem-default-qword)
                         (:dword 'sized-dword-xmmreg/mem)
                         (:word 'sized-word-xmmreg/mem)
                         (:byte 'sized-byte-xmmreg/mem)
                         (:sized 'sized-xmmreg/mem)))))
                 (reg-mem-size
                  `((reg/mem nil :type
                     ',(case reg-mem-size
                         (:qword 'sized-reg/mem-default-qword)
                         (:dword 'sized-dword-reg/mem)
                         (:word 'sized-word-reg/mem)
                         (:byte 'sized-byte-reg/mem)
                         (:sized 'sized-reg/mem))))))
              ,@more-fields))
           (inst-formats
            (if (or (eql w 1) (/= opcode-prefix 15))
                (list (symbolicate "VEX3-" inst-format-stem))
                (list (symbolicate "VEX2-" inst-format-stem)
                      (symbolicate "VEX3-" inst-format-stem))))
           (auto-evex-p (or evex auto-evex)))
      (append
       ;; VEX printers
       (mapcar (lambda (inst-format)
                 `(:printer ,inst-format ,fields
                            ,@(cond (printer
                                     `(',printer))
                                    ((eq nds 'to-mem)
                                     `('(:name :tab reg/mem ", " vvvv ", " reg)))
                                    (nds
                                     `('(:name :tab reg ", " vvvv ", " reg/mem))))))
               inst-formats)

       ;; EVEX printers (auto-promotion). The skip-list of VEX/EVEX name
       ;; mismatches has been replaced by an explicit :auto-evex nil on
       ;; each such instruction (see below), so this now allows all
       ;; opcode maps -- including the vpmovsx*/vpmovzx* sign/zero
       ;; extension range in map 0F38 that used to need a special carve-out.
       (when auto-evex-p
         (let ((broadcast-opcodes #(24 25 26 88 89 90 120 121)))
           (let ((broadcast
                  (and (= opcode-prefix 3896) (find opcode broadcast-opcodes))))
             (if broadcast
                 (avx512-inst-printer-list inst-format-stem prefix opcode
                   :more-fields evex-more-fields
                   :printer printer
                   :opcode-prefix opcode-prefix
                   :w w
                   :ll 2
                   :disp-n disp-n
                   :nds nds
                   :reg-mem-size reg-mem-size
                   :xmmreg-mem-size xmmreg-mem-size)
                 (loop for (ll n) in '((0 16) (1 32) (2 64))
                       appending (avx512-inst-printer-list inst-format-stem prefix opcode
                                   :more-fields evex-more-fields
                                   :printer printer
                                   :opcode-prefix opcode-prefix
                                   :w w
                                   :ll ll
                                   :disp-n n
                                   :nds nds
                                   :reg-mem-size reg-mem-size
                                   :xmmreg-mem-size xmmreg-mem-size))))))))))

(macrolet ((def (name opcode /i)
             `(define-instruction ,name (segment dst src imm)
               ,@(avx2-inst-printer-list 'ymm-ymm-imm #x66 opcode
                 :more-fields `((/i ,/i))
                 :w 0)
               (:emitter
                (emit-avx2-inst-imm segment dst src imm #x66 ,opcode ,/i)))))
  (def vpslldq 115 7)
  (def vpsrldq 115 3))

(macrolet ((def (name opcode vopcode /i &optional (evex-w 0))
             `(define-instruction ,name (segment dst src src2/imm)
               ,@(avx2-inst-printer-list 'ymm-ymm-imm #x66 opcode
                 :w evex-w
                 :more-fields `((/i ,/i)))
               ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 vopcode
                 :nds t
                 :w evex-w)
               (:emitter
                (if (integerp src2/imm)
                    (emit-avx2-inst-imm segment dst src src2/imm #x66 ,opcode ,/i
                      :evex-w ,evex-w
                      :w ,evex-w)
                    (emit-avx2-inst segment src2/imm dst #x66 ,vopcode
                      :evex-w ,evex-w
                      :w ,evex-w
                      :vvvv src))))))
  (def vpsllw 113 241 6)
  (def vpslld 114 242 6)
  (def vpsllq 115 243 6 1)
  (def vpsraw 113 225 4)
  (def vpsrad 114 226 4)
  (def vpsrlw 113 209 2)
  (def vpsrld 114 210 2)
  (def vpsrlq 115 211 2 1))

(macrolet ((def
               (name prefix opcode
                &key (opcode-prefix 15) (evex-w 0) (auto-evex t))
             `(define-instruction ,name (segment dst src src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix opcode
                 :nds t
                 :opcode-prefix opcode-prefix
                 :w evex-w
                 :auto-evex auto-evex)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,opcode
                  :opcode-prefix ,opcode-prefix
                  :evex-w ,evex-w
                  :w ,evex-w
                  :vvvv src)))))
  (def vandpd #x66 84
    :opcode-prefix #x0F
    :evex-w 1)
  (def vandps nil 84)
  (def vandnpd #x66 85
    :opcode-prefix #x0F
    :evex-w 1)
  (def vandnps nil 85)
  (def vorpd #x66 86
    :opcode-prefix #x0F
    :evex-w 1)
  (def vorps nil 86)
  (def vpand #x66 219 :auto-evex nil)
  (def vpandn #x66 223 :auto-evex nil)
  (def vpor #x66 235 :auto-evex nil)
  (def vpxor #x66 239 :auto-evex nil)
  (def vxorpd #x66 87
    :opcode-prefix #x0F
    :evex-w 1)
  (def vxorps nil 87)
  (def vcomisd #x66 47)
  (def vcomiss nil 47)
  (def vucomisd #x66 46)
  (def vucomiss nil 46)
  (def vpcmpeqb #x66 116)
  (def vpcmpeqw #x66 117)
  (def vpcmpeqd #x66 118)
  (def vpcmpgtb #x66 100)
  (def vpcmpgtw #x66 101)
  (def vpcmpgtd #x66 102)
  (def vmaxpd #x66 95
    :opcode-prefix #x0F
    :evex-w 1)
  (def vmaxps nil 95)
  (def vmaxsd #xF2 95)
  (def vmaxss #xF3 95)
  (def vminpd #x66 93
    :opcode-prefix #x0F
    :evex-w 1)
  (def vminps nil 93)
  (def vminsd #xF2 93)
  (def vminss #xF3 93)
  (def vpmaxsw #x66 238)
  (def vpmaxub #x66 222)
  (def vpminsw #x66 234)
  (def vpminub #x66 218)
  (def vaddpd #x66 88
    :opcode-prefix #x0F
    :evex-w 1)
  (def vaddps nil 88)
  (def vaddsd #xF2 88)
  (def vaddss #xF3 88)
  (def vaddsubpd #x66 208)
  (def vaddsubps #xF2 208)
  (def vdivpd #x66 94
    :opcode-prefix #x0F
    :evex-w 1)
  (def vdivps nil 94)
  (def vdivsd #xF2 94)
  (def vdivss #xF3 94)
  (def vhaddpd #x66 124)
  (def vhaddps #xF2 124)
  (def vhsubpd #x66 125)
  (def vhsubps #xF2 125)
  (def vmulpd #x66 89
    :opcode-prefix #x0F
    :evex-w 1)
  (def vmulps nil 89)
  (def vmulsd #xF2 89)
  (def vmulss #xF3 89)
  (def vsubpd #x66 92
    :opcode-prefix #x0F
    :evex-w 1)
  (def vsubps nil 92)
  (def vsubsd #xF2 92)
  (def vsubss #xF3 92)
  (def vunpckhpd #x66 21
    :opcode-prefix #x0F
    :evex-w 1)
  (def vunpckhps nil 21)
  (def vunpcklpd #x66 20
    :opcode-prefix #x0F
    :evex-w 1)
  (def vunpcklps nil 20)
  (def vpaddb #x66 252)
  (def vpaddw #x66 253)
  (def vpaddd #x66 254)
  (def vpaddq #x66 212
    :opcode-prefix #x0F
    :evex-w 1)
  (def vpaddsb #x66 236)
  (def vpaddsw #x66 237)
  (def vpaddusb #x66 220)
  (def vpaddusw #x66 221)
  (def vpavgb #x66 224)
  (def vpavgw #x66 227)
  (def vpmaddwd #x66 245)
  (def vpmulhuw #x66 228)
  (def vpmulhw #x66 229)
  (def vpmullw #x66 213)
  (def vpmuludq #x66 244
    :opcode-prefix #x0F
    :evex-w 1)
  (def vpsadbw #x66 246)
  (def vpsubb    #x66 #xf8)
  (def vpsubw    #x66 #xf9)
  (def vpsubd    #x66 #xfa)
  (def vpsubq #x66 #xfb
    :opcode-prefix #x0F
    :evex-w 1)
  (def vpsubsb   #x66 #xe8)
  (def vpsubsw   #x66 #xe9)
  (def vpsubusb  #x66 #xd8)
  (def vpsubusw  #x66 #xd9)

  ;; integer
  (def vpacksswb  #x66 #x63)
  (def vpackssdw  #x66 #x6b)
  (def vpackuswb  #x66 #x67)
  (def vpunpckhbw #x66 #x68)
  (def vpunpckhwd #x66 #x69)
  (def vpunpckhdq #x66 #x6a)
  (def vpunpckhqdq #x66 #x6d)
  (def vpunpcklbw #x66 #x60)
  (def vpunpcklwd #x66 #x61)
  (def vpunpckldq #x66 #x62)
  (def vpunpcklqdq #x66 #x6c)

  (def vpshufb #x66 #x00 :opcode-prefix #x0f38)
  (def vphaddw #x66 #x01 :opcode-prefix #x0f38)
  (def vphaddd #x66 #x02 :opcode-prefix #x0f38)
  (def vphaddsw #x66 #x03 :opcode-prefix #x0f38)
  (def vpmaddubsw #x66 #x04 :opcode-prefix #x0f38)
  (def vphsubw #x66 #x05 :opcode-prefix #x0f38)
  (def vphsubd #x66 #x06 :opcode-prefix #x0f38)
  (def vphsubsw #x66 #x07 :opcode-prefix #x0f38)
  (def vpsignb #x66 #x08 :opcode-prefix #x0f38)
  (def vpsignw #x66 #x09 :opcode-prefix #x0f38)
  (def vpsignd #x66 #x0a :opcode-prefix #x0f38)
  (def vpmulhrsw #x66 #x0b :opcode-prefix #x0f38)

  (def vpmuldq #x66 #x28
    :opcode-prefix #x0f38
    :evex-w 1)
  (def vpcmpeqq #x66 #x29
    :opcode-prefix #x0f38
    :evex-w 1)
  (def vpackusdw #x66 #x2b :opcode-prefix #x0f38)

  (def vpcmpgtq #x66 #x37
    :opcode-prefix #x0f38
    :evex-w 1)
  (def vpminsb  #x66 #x38 :opcode-prefix #x0f38)
  (def vpminsd  #x66 #x39 :opcode-prefix #x0f38)
  (def vpminuw  #x66 #x3a :opcode-prefix #x0f38)
  (def vpminud  #x66 #x3b :opcode-prefix #x0f38)
  (def vpmaxsb  #x66 #x3c :opcode-prefix #x0f38)
  (def vpmaxsd  #x66 #x3d :opcode-prefix #x0f38)
  (def vpmaxuw  #x66 #x3e :opcode-prefix #x0f38)
  (def vpmaxud  #x66 #x3f :opcode-prefix #x0f38)

  (def vpmulld      #x66 #x40 :opcode-prefix #x0f38)
  (def vphminposuw  #x66 #x41 :opcode-prefix #x0f38)

  (def vaesenc      #x66 #xdc :opcode-prefix #x0f38)
  (def vaesenclast  #x66 #xdd :opcode-prefix #x0f38)
  (def vaesdec      #x66 #xde :opcode-prefix #x0f38)
  (def vaesdeclast  #x66 #xdf :opcode-prefix #x0f38))

;;; Two arg instructions
(macrolet ((def (name prefix opcode &optional (opcode-prefix #x0F) l (evex-w 0))
             `(define-instruction ,name (segment dst src)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix opcode
                  :opcode-prefix opcode-prefix
                  :w evex-w
                  :more-fields (case l
                                 (:from-thing '((reg nil :type 'xmmreg)))
                                 (:xmm-src '((reg/mem nil :type 'xmmreg/mem)))
                                 (:half-src '((reg/mem nil :type 'half-ymmreg/mem)))))
                (:emitter
                 (emit-avx2-inst segment src dst ,prefix ,opcode
                                 :opcode-prefix ,opcode-prefix
                                 :evex-w ,evex-w
                                 :w ,evex-w
                                 ,@(and (or (numberp l) (eq l :from-thing))
                                       `(:l ,l)))))))
  ;; Everything else this macro used to cover (vmovshdup/vmovsldup/vmovddup,
  ;; vrcpps, vrsqrtps, vsqrtpd/vsqrtps, vptest, vpabsb/w/d,
  ;; vpmovsx*/vpmovzx*, vcvtdq2ps, vcvtpd2dq/ps, vcvtps2dq/pd, vcvttpd2dq)
  ;; moved to the per-length disp-n aware macro below; only the scalar and
  ;; still-uncovered forms remain here.
  (def vrcpss    #xf3 #x53)
  (def vrsqrtss  #xf3 #x52)
  (def vsqrtsd   #xf2 #x51)
  (def vsqrtss   #xf3 #x51)
  (def vcvtsd2ss #xf2 #x5a)
  (def vcvtss2sd #xf3 #x5a)
  (def vcvttps2dq #xf3 #x5b)
  (def vaesimc #x66 #xdb #x0f38))

(macrolet ((def
               (name prefix opcode
                &key (opcode-prefix 15) l (evex-w 0) disp-ns narrow)
             ;; NARROW handles size-changing moves whose register-direct
             ;; source is narrower than the (LL-derived) destination width:
             ;; :FIXED-XMM for a source that is always XMM regardless of
             ;; destination width (4x/8x element-size expansion), and
             ;; :ONE-SIZE-DOWN for a source that is always one step below
             ;; the destination width (2x expansion, e.g. VPMOVZXBW).
             `(define-instruction ,name (segment dst src)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix opcode
                 :opcode-prefix opcode-prefix
                 :w evex-w
                 :more-fields (case narrow
                                (:fixed-xmm '((reg/mem nil :printer #'print-xmmreg/mem)))
                                (:one-size-down
                                 '((reg/mem nil :printer #'print-ymmreg/mem-one-size-down)))
                                (t (and (eq l :from-thing) '((reg nil :type 'xmmreg)))))
                 :auto-evex nil)
               ,@(loop for ll in '(0 1 2)
                       for n in disp-ns
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                :opcode-prefix opcode-prefix
                                :w evex-w
                                :ll ll
                                :disp-n n
                                :more-fields (case narrow
                                 (:fixed-xmm
                                  `((reg/mem nil
                                      :prefilter (lambda (dstate mod r/m)
                                                   (decode-mod-r/m dstate mod r/m 'fpr
                                                                   :disp-n ,n))
                                      :printer #'print-xmmreg/mem)))
                                 (:one-size-down
                                  `((reg/mem nil
                                      :prefilter (lambda (dstate mod r/m)
                                                   (decode-mod-r/m dstate mod r/m 'fpr
                                                                   :disp-n ,n))
                                      :printer #'print-ymmreg/mem-one-size-down)))
                                 (t (and (eq l :from-thing)
                                         '((reg nil :type 'xmmreg)))))))
               (:emitter
                (emit-avx2-inst segment src dst ,prefix ,opcode :opcode-prefix
                                ,opcode-prefix :evex-w ,evex-w :w ,evex-w
                                ,@(and l `(:l ,l)) :disp-n
                                (cond ((zmm-register-p dst) (third ',disp-ns))
                                      ((ymm-register-p dst) (second ',disp-ns))
                                      ((xmm-register-p dst) (first ',disp-ns))
                                      (t 0)))))))
  (def vmovshdup #xF3 22 :disp-ns (16 32 64))
  (def vmovsldup #xF3 18 :disp-ns (16 32 64))
  (def vmovddup #xF2 18 :disp-ns (16 32 64))
  (def vrcpps nil 83 :disp-ns (16 32 64))
  (def vrsqrtps nil 82 :disp-ns (16 32 64))
  (def vsqrtpd #x66 81
    :evex-w 1
    :disp-ns (16 32 64))
  (def vsqrtps nil 81 :disp-ns (16 32 64))
  (def vptest #x66 23
    :opcode-prefix #x0F38
    :disp-ns (16 32 64))
  (def vpabsb #x66 28
    :opcode-prefix #x0F38
    :disp-ns (16 32 64))
  (def vpabsw #x66 29
    :opcode-prefix #x0F38
    :disp-ns (16 32 64))
  (def vpabsd #x66 30
    :opcode-prefix #x0F38
    :disp-ns (16 32 64))
  (def vpmovsxbw #x66 32 :opcode-prefix #x0F38 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vpmovsxbd #x66 33 :opcode-prefix #x0F38 :disp-ns (4 8 16) :narrow
   :fixed-xmm)
  (def vpmovsxbq #x66 34 :opcode-prefix #x0F38 :disp-ns (2 4 8) :narrow
   :fixed-xmm)
  (def vpmovsxwd #x66 35 :opcode-prefix #x0F38 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vpmovsxwq #x66 36 :opcode-prefix #x0F38 :disp-ns (4 8 16) :narrow
   :fixed-xmm)
  (def vpmovsxdq #x66 37 :opcode-prefix #x0F38 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vpmovzxbw #x66 48 :opcode-prefix #x0F38 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vpmovzxbd #x66 49 :opcode-prefix #x0F38 :disp-ns (4 8 16) :narrow
   :fixed-xmm)
  (def vpmovzxbq #x66 50 :opcode-prefix #x0F38 :disp-ns (2 4 8) :narrow
   :fixed-xmm)
  (def vpmovzxwd #x66 51 :opcode-prefix #x0F38 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vpmovzxwq #x66 52 :opcode-prefix #x0F38 :disp-ns (4 8 16) :narrow
   :fixed-xmm)
  (def vpmovzxdq #x66 53 :opcode-prefix #x0F38 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vcvtdq2pd #xF3 230 :disp-ns (8 16 32))
  (def vcvtdq2ps nil 91 :disp-ns (16 32 64))
  (def vcvtpd2dq #xF2 230 :l :from-thing :disp-ns (16 32 64))
  (def vcvtpd2ps #x66 90 :l :from-thing :disp-ns (16 32 64))
  (def vcvtps2dq #x66 91 :disp-ns (16 32 64))
  (def vcvtps2pd nil 90 :disp-ns (8 16 32))
  (def vcvttpd2dq #x66 230 :l :from-thing :disp-ns (16 32 64)))

(macrolet ((def (name prefix &key (evex-w 0))
             `(define-instruction ,name (segment dst src pattern)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix 112
                 :printer '(:name :tab reg ", " reg/mem ", " imm)
                 :w evex-w)
               (:emitter
                (emit-avx2-inst segment src dst ,prefix 112 :remaining-bytes 1)
                (emit-byte segment pattern)))))
  (def vpshufd #x66)
  (def vpshufhw #xF3)
  (def vpshuflw #xF2))

(macrolet ((def (name prefix &key (evex-w 0))
             `(define-instruction ,name (segment dst src src2 pattern)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix 198 :w evex-w)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix 198
                  :vvvv src
                  :remaining-bytes 1)
                (emit-byte segment pattern)))))
  (def vshufpd #x66 :evex-w 1)
  (def vshufps nil))

(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src src2 imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                  :opcode-prefix #x0F3A)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,opcode
                  :opcode-prefix #x0F3A
                  :vvvv src)
                (emit-byte segment imm))))
           (def-two (name prefix opcode &key (auto-evex t))
             `(define-instruction ,name (segment dst src imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                 :opcode-prefix #x0F3A
                 :printer '(:name :tab reg ", " reg/mem ", " imm)
                 :auto-evex auto-evex)
               (:emitter
                (emit-avx2-inst segment src dst ,prefix ,opcode :opcode-prefix
                                3898)
                (emit-byte segment imm)))))
  (def-two vroundps #x66 8)
  (def-two vroundpd #x66 9)
  (def-two vroundss #x66 10 :auto-evex nil)
  (def-two vroundsd #x66 11 :auto-evex nil)
  (def vblendps #x66 12)
  (def vblendpd #x66 13)
  (def vpblendw #x66 14)
  (def vpalignr #x66 15)
  (def vdpps #x66 64)
  (def vdppd #x66 65)
  (def vmpsadbw #x66 66)
  (def vpclmulqdq #x66 68)
  (def-two vpcmpestrm #x66 96)
  (def-two vpcmpestri #x66 97)
  (def-two vpcmpistrm #x66 98)
  (def-two vpcmpistri #x66 99)
  (def-two vaeskeygenassist #x66 223))

(macrolet ((def (name prefix opcode
                 name-suffix &key (evex-w 0) scalar)
             `(define-instruction ,name (segment condition dst src src2 &optional mask)
                ;; :auto-evex nil: the loop below already emits the full set
                ;; of EVEX printers (k=0..7), with REG constrained to type
                ;; 'opmask-reg. An auto-promoted EVEX printer from this VEX
                ;; printer would treat REG as a plain vector register instead,
                ;; conflicting with (rather than specializing) the k=0 entry.
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                  :more-fields `((imm nil :type 'avx-condition-code))
                  :printer `("VCMP" imm ,name-suffix
                                    :tab reg ", " vvvv ", " reg/mem)
                  :auto-evex nil)
                ,@(loop for k from 0 to 7
                        append
                        (avx512-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                          :opcode-prefix #x0F
                          :w evex-w
                          :more-fields `((reg nil :type 'opmask-reg)
                                         (imm nil :type 'avx-condition-code)
                                         ,@(when (plusp k) `((aaa ,k))))
                          :printer (if (plusp k)
                                       `("VCMP" imm ,name-suffix
                                                :tab reg " {" aaa "}, " vvvv ", " reg/mem)
                                       `("VCMP" imm ,name-suffix
                                                :tab reg ", " vvvv ", " reg/mem))))
                (:emitter
                 (multiple-value-bind (cond-arg dst-reg src1 src2-arg mask-val)
                     (if (register-p condition)
                         ;; (inst vcmp dst src1 src2 condition &optional mask)
                         (values src2 condition dst src (or mask 0))
                         ;; (inst vcmp condition dst src1 src2 &optional mask)
                         (values condition dst src src2 (or mask 0)))
                   (let ((imm (or (position cond-arg +avx-conditions+)
                                  (and (integerp cond-arg) (<= 0 cond-arg 31) cond-arg)
                                  (error "~s not one of ~s or 0..31"
                                         cond-arg
                                         +avx-conditions+)))
                         (mask-num (cond ((null mask-val) 0)
                                         ((integerp mask-val) mask-val)
                                         ((k-register-p mask-val) (reg-id-num (reg-id mask-val)))
                                         (t 0))))
                     (cond
                       ((or (k-register-p dst-reg)
                            (zmm-register-p src1)
                            (and (register-p src2-arg) (zmm-register-p src2-arg))
                            (plusp mask-num))
                        (aver (k-register-p dst-reg))
                        (let ((disp-n ,(if scalar
                                           `(if (zerop ,evex-w) 4 8)
                                           `(cond ((zmm-register-p src1) 64)
                                                  ((ymm-register-p src1) 32)
                                                  (t 16)))))
                          (emit-avx512-inst segment src2-arg dst-reg ,prefix ,opcode
                            :opcode-prefix #x0F
                            :vvvv src1
                            :w ,evex-w
                            :aaa mask-num
                            :disp-n disp-n
                            :remaining-bytes 1)
                          (emit-byte segment imm)))
                       (t
                        (emit-avx2-inst segment src2-arg dst-reg ,prefix ,opcode
                          :evex-w ,evex-w
                          :vvvv src1
                          :remaining-bytes 1)
                        (emit-byte segment imm)))))))))
  (def vcmppd #x66 #xc2 "PD" :evex-w 1)
  (def vcmpps nil  #xc2 "PS" :evex-w 0)
  (def vcmpsd #xf2 #xc2 "SD"
    :evex-w 1
    :scalar t)
  (def vcmpss #xf3 #xc2 "SS"
    :evex-w 0
    :scalar t))

(macrolet ((def (name prefix op)
             `(define-instruction ,name (segment dst src src2 mask)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-ymm prefix op
                 :w 0
                 :opcode-prefix #x0F3A)
               (:emitter (aver (xmm-register-p dst))
                (aver (xmm-register-p mask))
                (emit-avx2-inst segment src2 dst ,prefix ,op
                  :opcode-prefix #x0F3A
                  :vvvv src
                  :w 0
                  :is4 mask)))))
  (def vpblendvb #x66 76)
  (def vblendvps #x66 74)
  (def vblendvpd #x66 75))

(macrolet ((def
               (name prefix opcode-from opcode-to
                &key force-to-mem reg-reg-name l (opcode-prefix 15) (evex-w 0)
                nds (auto-evex t))
             `(progn
               ,(when reg-reg-name
                  `(define-instruction ,reg-reg-name
                    (segment dst src
                     ,@(if nds
                           '(src2)))
                    (:emitter (aver (xmm-register-p dst))
                     (aver (xmm-register-p src))
                     (emit-avx2-inst segment dst
                                     ,(if nds
                                          'src2
                                          'src)
                                     ,prefix ,opcode-from :opcode-prefix
                                     ,opcode-prefix :evex-w ,evex-w :l ,l
                                     ,@(and nds `(:vvvv src))))))
               (define-instruction ,name
                (segment dst src
                 ,@(if nds
                       '(&optional src2)))
                ,@(when opcode-from
                    (avx2-inst-printer-list 'ymm-ymm/mem prefix opcode-from
                      :opcode-prefix opcode-prefix
                      :nds nds
                      :w evex-w
                      :auto-evex auto-evex))
                ,@(when opcode-to
                    (avx2-inst-printer-list 'ymm-ymm/mem prefix opcode-to
                      :printer '(:name :tab reg/mem ", " reg)
                      :opcode-prefix opcode-prefix
                      :w evex-w
                      :auto-evex auto-evex))
                (:emitter ,@(when nds `((aver (register-p src))))
                 (cond
                  ,@(when opcode-from
                      `(((xmm-register-p dst)
                         ,(when force-to-mem
                            `(aver
                              (not
                               (register-p
                                ,(if nds
                                     'src2
                                     'src)))))
                         (emit-avx2-inst segment
                                         ,(if nds
                                              'src2
                                              'src)
                                         dst ,prefix ,opcode-from
                                         :opcode-prefix ,opcode-prefix :evex-w
                                         ,evex-w :w ,evex-w :disp-n
                                         (cond ((zmm-register-p dst) 64)
                                               ((ymm-register-p dst) 32)
                                               ((xmm-register-p dst) 16) (t 0))
                                         ,@(and nds `(:vvvv src)) :l ,l))))
                  (t (aver (xmm-register-p src))
                   ,(when force-to-mem `(aver (not (register-p dst))))
                   (emit-avx2-inst segment dst src ,prefix ,opcode-to
                     :opcode-prefix ,opcode-prefix
                     :evex-w ,evex-w
                     :w ,evex-w
                     :disp-n (cond ((zmm-register-p src) 64)
                                   ((ymm-register-p src) 32)
                                   ((xmm-register-p src) 16) (t 0))
                     :l ,l))))))))
  (def vmovapd #x66 40 41 :evex-w 1)
  (def vmovaps nil 40 41)
  (def vmovdqa #x66 111 127 :auto-evex nil)
  (def vmovdqu #xF3 111 127 :auto-evex nil)
  (def vmovupd #x66 16 17 :evex-w 1)
  (def vmovups nil 16 17)
  (def vmovntdq #x66 nil 231 :force-to-mem t)
  (def vmovntdqa #x66 42 nil
    :force-to-mem t
    :opcode-prefix #x0F38)
  (def vmovntpd #x66 nil 43
    :force-to-mem t
    :evex-w 1)
  (def vmovntps nil nil 43 :force-to-mem t)
  (def vmovhpd #x66 22 23
    :force-to-mem t
    :l 0
    :nds t)
  (def vmovhps nil 22 23
    :reg-reg-name vmovlhps
    :l 0
    :nds t)
  (def vmovlpd #x66 18 19
    :force-to-mem t
    :l 0
    :nds t)
  (def vmovlps nil 18 19
    :reg-reg-name vmovhlps
    :l 0
    :nds t))

(define-instruction vlddqu (segment dst src)
 (:emitter (aver (xmm-register-p dst)) (aver (ea-p src))
  (emit-avx2-inst segment src dst #xF2 240
    :opcode-prefix #x0F
    :evex-w 0
    :l nil))
 . #.(avx2-inst-printer-list 'ymm-ymm/mem #xF2 240))

(macrolet ((def (name prefix)
             `(define-instruction ,name (segment dst src &optional src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-dir prefix 8)
               (:emitter
                (cond
                 ((and (xmm-register-p dst) (ea-p src))
                  (emit-avx2-inst segment src dst ,prefix 16 :l 0))
                 ((xmm-register-p dst)
                  (emit-avx2-inst segment src2 dst ,prefix 16
                    :vvvv src
                    :l 0))
                 (t (aver (xmm-register-p src))
                  (emit-avx2-inst segment dst src ,prefix 17 :l 0)))))))
  (def vmovsd #xF2)
  (def vmovss #xF3))

(flet ((move-ymm<->gpr (segment dst src w)
         (cond
          ((xmm-register-p dst)
           (emit-avx2-inst segment src dst #x66 110
             :l 0
             :w w))
          (t (aver (xmm-register-p src))
           (emit-avx2-inst segment dst src #x66 126
             :l 0
             :w w)))))
  (define-instruction vmovd (segment dst src)
   (:emitter (move-ymm<->gpr segment dst src 0))
   . #.(append
        (avx2-inst-printer-list 'ymm-ymm/mem #x66 110
          :more-fields '((reg/mem nil :type 'sized-reg/mem))
          :w 0)
        (avx2-inst-printer-list 'ymm-ymm/mem #x66 126
          :more-fields '((reg/mem nil :type 'sized-reg/mem))
          :printer '(:name :tab reg/mem ", " reg)
          :w 0)))
  (define-instruction vmovq (segment dst src)
   (:emitter
    (cond ((or (gpr-p src) (gpr-p dst)) (move-ymm<->gpr segment dst src 1))
          ((xmm-register-p dst) (emit-avx2-inst segment src dst #xF3 126 :l 0))
          (t (aver (xmm-register-p src))
           (emit-avx2-inst segment dst src #x66 214 :l 0))))
   . #.(append
        (avx2-inst-printer-list 'ymm-ymm/mem #x66 110
          :w 1
          :more-fields '((reg/mem nil :type 'sized-reg/mem-default-qword)))
        (avx2-inst-printer-list 'ymm-ymm/mem #x66 126
          :w 1
          :more-fields '((reg/mem nil :type 'sized-reg/mem-default-qword))
          :printer '(:name :tab reg/mem ", " reg))
        (avx2-inst-printer-list 'ymm-ymm/mem #xF3 126)
        (avx2-inst-printer-list 'ymm-ymm/mem #x66 214 :printer
         '(:name :tab reg/mem ", " reg)))))

(macrolet ((def-insert (name prefix op size &key (op-prefix 3898) (w 0))
             `(define-instruction ,name (segment dst src src2 imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op
                 :w w
                 :opcode-prefix op-prefix
                 :reg-mem-size size
                 :more-fields `((imm nil :type 'imm-byte))
                 :printer `(:name :tab reg ", " vvvv ", " reg/mem ", " imm)
                 :evex t)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,op
                  :opcode-prefix ,op-prefix
                  :vvvv src
                  :w ,w
                  :l 0
                  :remaining-bytes 1)
                (emit-byte segment imm))))
           (def-extract (name prefix op size &key (w 0))
             `(define-instruction ,name (segment dst src imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op
                 :w w
                 :opcode-prefix #x0F3A
                 :reg-mem-size size
                 :more-fields `((imm nil :type 'imm-byte))
                 :printer `(:name :tab reg/mem ", " reg ", " imm)
                 :evex t)
               (:emitter
                (aver (and (xmm-register-p src) (not (xmm-register-p dst))))
                (emit-avx2-inst segment dst src ,prefix ,op
                  :w ,w
                  :l 0
                  :opcode-prefix #x0F3A
                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def-insert vpinsrb #x66 32 :byte)
  (def-insert vpinsrq #x66 34 :qword :w 1)
  (def-insert vpinsrw #x66 196 :word :op-prefix 15)
  (def-insert vpinsrd #x66 34 :dword)
  (def-insert vinsertps #x66 33 nil)
  (def-extract vpextrb #x66 20 :byte)
  (def-extract vpextrd #x66 22 :dword)
  (def-extract vpextrq #x66 22 :qword :w 1)
  (def-extract vextractps #x66 23 nil))

(define-instruction vpextrw (segment dst src imm)
 (:emitter (aver (xmm-register-p src))
  (if (gpr-p dst)
      (emit-avx2-inst segment dst src #x66 197
        :l 0
        :remaining-bytes 1)
      (emit-avx2-inst segment dst src #x66 21
        :opcode-prefix #x0F3A
        :l 0
        :remaining-bytes 1))
  (emit-byte segment imm))
 . #.(append
      (avx2-inst-printer-list 'ymm-ymm/mem #x66 21
        :w 0
        :opcode-prefix #x0F3A
        :more-fields `((imm nil :type 'imm-byte))
        :printer '(:name :tab reg/mem ", " reg ", " imm))
      (avx2-inst-printer-list 'ymm-ymm/mem #x66 197
        :w 0
        :more-fields `((imm nil :type 'imm-byte))
        :printer '(:name :tab reg/mem ", " reg ", " imm))))

(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix opcode
                  :reg-mem-size :sized :nds t)
               (:emitter (aver (xmm-register-p dst))
                (let ((src-size (operand-size src2)))
                  (emit-avx2-inst segment src2 dst ,prefix ,opcode
                    :l 0
                    :vvvv src
                    :w (case src-size
                         (:qword 1)
                         (:dword 0)
                         (t 1))))))))
  (def vcvtsi2sd #xF2 42)
  (def vcvtsi2ss #xF3 42))

(macrolet ((def (name prefix opcode &key reg-only)
             `(define-instruction ,name (segment dst src)
               ,@(avx2-inst-printer-list 'reg-ymm/mem prefix opcode)
               (:emitter (aver (gpr-p dst))
                ,(when reg-only `(aver (xmm-register-p src)))
                (let ((dst-size (operand-size dst)))
                  (aver (or (eq dst-size :qword) (eq dst-size :dword)))
                  (emit-avx2-inst segment src dst ,prefix ,opcode :w
                                  (ecase dst-size (:qword 1) (:dword 0))
                                  ,@(unless reg-only '(:l 0))))))))
  (def vcvtsd2si #xF2 45)
  (def vcvtss2si #xF3 45)
  (def vcvttsd2si #xF2 44)
  (def vcvttss2si #xF3 44)
  (def vmovmskpd #x66 80 :reg-only t)
  (def vmovmskps nil 80 :reg-only t)
  (def vpmovmskb #x66 215 :reg-only t))

(define-instruction vzeroupper (segment)
 (:printer vex2-op ((op 119) (l 0) (r 1) (pp 0)))
 (:emitter (emit-two-byte-vex segment 0 0 0 nil) (emit-byte segment 119)))

(define-instruction vzeroall (segment)
 (:printer vex2-op ((op 119) (l 1) (r 1) (pp 0)))
 (:emitter (emit-two-byte-vex segment 0 0 1 nil) (emit-byte segment 119)))

(macrolet ((def-vbroadcast ()
             `(progn
                (define-instruction vbroadcastss (segment dst src)
                  ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x18 :opcode-prefix #x0f38 :xmmreg-mem-size :dword
                    :w 0
                    :l nil)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x18
                    :opcode-prefix #x0f38
                    :w 0
                    :disp-n 4)
                  (:emitter
                   (cond
                     ((zmm-register-p dst)
                      (emit-avx512-inst segment src dst #x66 #x18
                        :opcode-prefix #x0f38
                        :w 0
                        :disp-n 4))
                     (t
                      (emit-avx2-inst segment src dst #x66 #x18
                        :opcode-prefix #x0f38
                        :evex-w 0
                        :w 0
                        :l nil)))))

                (define-instruction vbroadcastsd (segment dst src)
                  ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x19 :opcode-prefix #x0f38 :xmmreg-mem-size :qword
                    :w 0
                    :l 1)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x19
                    :opcode-prefix #x0f38
                    :w 1
                    :disp-n 8)
                  (:emitter
                   (cond
                     ((zmm-register-p dst)
                      (emit-avx512-inst segment src dst #x66 #x19
                        :opcode-prefix #x0f38
                        :w 1
                        :disp-n 8))
                     (t
                      (emit-avx2-inst segment src dst #x66 #x19
                        :opcode-prefix #x0f38
                        :evex-w 1
                        :w 0
                        :l 1)))))

                (define-instruction vpbroadcastd (segment dst src)
                  ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x58 :opcode-prefix #x0f38 :xmmreg-mem-size :dword
                    :w 0
                    :l nil)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x58
                    :opcode-prefix #x0f38
                    :w 0
                    :disp-n 4)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x7c
                                              :opcode-prefix #x0f38
                                              :w 0
                                              :reg-mem-size :dword
                                              :printer '(:name :tab reg ", " reg/mem))
                  (:emitter
                   (cond
                     ((gpr-p src)
                      (emit-avx512-inst segment src dst #x66 #x7c
                        :opcode-prefix #x0f38
                        :w 0))
                     ((or (zmm-register-p dst)
                          (and (register-p src) (zmm-register-p src)))
                      (emit-avx512-inst segment src dst #x66 #x58
                        :opcode-prefix #x0f38
                        :w 0
                        :disp-n 4))
                     (t
                      (emit-avx2-inst segment src dst #x66 #x58
                        :opcode-prefix #x0f38
                        :evex-w 0
                        :w 0
                        :l nil)))))

                (define-instruction vpbroadcastq (segment dst src)
                  ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x59 :opcode-prefix #x0f38 :xmmreg-mem-size :qword
                    :w 0
                    :l nil)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x59
                    :opcode-prefix #x0f38
                    :w 1
                    :disp-n 8)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x7c
                                              :opcode-prefix #x0f38
                                              :w 1
                                              :reg-mem-size :qword
                                              :printer '(:name :tab reg ", " reg/mem))
                  (:emitter
                   (cond
                     ((gpr-p src)
                      (emit-avx512-inst segment src dst #x66 #x7c
                        :opcode-prefix #x0f38
                        :w 1))
                     ((or (zmm-register-p dst)
                          (and (register-p src) (zmm-register-p src)))
                      (emit-avx512-inst segment src dst #x66 #x59
                        :opcode-prefix #x0f38
                        :w 1
                        :disp-n 8))
                     (t
                      (emit-avx2-inst segment src dst #x66 #x59
                        :opcode-prefix #x0f38
                        :evex-w 1
                        :w 0
                        :l nil))))))))
  (def-vbroadcast))

(macrolet ((def (name opcode
                 &key l (mem-size :qword) (evex-w 0) (disp-n 0) (auto-evex t))
             `(define-instruction ,name (segment dst src)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 opcode
                  :opcode-prefix #x0f38
                  :xmmreg-mem-size mem-size
                  :w evex-w
                  :l l
                  :disp-n disp-n
                  :auto-evex auto-evex)
                (:emitter
                 (emit-avx2-inst segment src dst #x66 ,opcode
                   :opcode-prefix #x0f38
                   :evex-w ,evex-w
                   :l ,l)))))
  (def vbroadcastf128 #x1a :l 1 :mem-size :qword
    :disp-n 16
    :auto-evex nil)
  (def vbroadcasti128 #x5a :l 1 :mem-size :qword
    :disp-n 16
    :auto-evex nil)
  (def vpbroadcastb   #x78 :l nil :mem-size :byte  :disp-n 1)
  (def vpbroadcastw   #x79 :l nil :mem-size :word  :disp-n 2))

(macrolet ((def-insert (name prefix op)
             `(define-instruction ,name (segment dst src src2 imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op
                 :w 0
                 :l 1
                 :auto-evex nil
                 :opcode-prefix #x0F3A)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,op
                  :opcode-prefix #x0F3A
                  :vvvv src
                  :w 0
                  :l 1
                  :remaining-bytes 1)
                (emit-byte segment imm))))
           (def-extract (name prefix op)
             `(define-instruction ,name (segment dst src imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op
                 :w 0
                 :l 1
                 :auto-evex nil
                 :opcode-prefix #x0F3A
                 :printer `(:name :tab reg/mem ", " reg ", " imm))
               (:emitter
                (emit-avx2-inst segment dst src ,prefix ,op
                  :w 0
                  :l 1
                  :opcode-prefix #x0F3A
                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def-insert vinsertf128 #x66 24)
  (def-insert vinserti128 #x66 56)
  (def-extract vextractf128 #x66 25)
  (def-extract vextracti128 #x66 57))

(macrolet ((def (name prefix op &optional l)
             `(define-instruction ,name (segment dst src src2 imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op
                 :w 0
                 :l l
                 :opcode-prefix #x0F3A)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,op
                  :opcode-prefix #x0F3A
                  :vvvv src
                  :w 0
                  :l ,l
                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vperm2f128 #x66 6 1)
  (def vperm2i128 #x66 70 1)
  (def vpblendd #x66 2))

(macrolet ((def (name prefix op)
             `(define-instruction ,name (segment dst src imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op
                 :w 1
                 :l 1
                 :opcode-prefix #x0F3A
                 :printer '(:name :tab reg ", " reg/mem ", " imm))
               (:emitter
                (emit-avx2-inst segment src dst ,prefix ,op
                  :opcode-prefix #x0F3A
                  :w 1
                  :l 1
                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vpermpd #x66 1)
  (def vpermq #x66 0))

(macrolet ((def (name op)
             `(define-instruction ,name (segment dst src src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 op
                 :w 0
                 :l 1
                 :nds t
                 :opcode-prefix #x0F38)
               (:emitter
                (emit-avx2-inst segment src2 dst #x66 ,op
                  :opcode-prefix #x0F38
                  :vvvv src
                  :w 0
                  :l 1)))))
  (def vpermps 22)
  (def vpermd 54))

(macrolet ((def (name op op-imm)
             `(define-instruction ,name (segment dst src src2/imm)
               ,@(append
                  (avx2-inst-printer-list 'ymm-ymm/mem-imm #x66 op-imm
                    :w 0
                    :opcode-prefix #x0F3A
                    :printer '(:name :tab reg ", " reg/mem ", " imm))
                  (avx2-inst-printer-list 'ymm-ymm/mem #x66 op
                    :w 0
                    :nds t
                    :opcode-prefix #x0F38))
               (:emitter
                (cond
                 ((integerp src2/imm)
                  (emit-avx2-inst segment src dst #x66 ,op-imm
                    :opcode-prefix #x0F3A
                    :w 0
                    :remaining-bytes 1)
                  (emit-byte segment src2/imm))
                 (t
                  (emit-avx2-inst segment src2/imm dst #x66 ,op
                    :opcode-prefix #x0F38
                    :vvvv src
                    :w 0)))))))
  (def vpermilps 12 4)
  (def vpermilpd 13 5))

(macrolet ((def (name prefix op to-mem-op)
             `(define-instruction ,name (segment dst src src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op
                 :w 0
                 :nds t
                 :auto-evex nil
                 :opcode-prefix #x0F38)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix to-mem-op
                 :w 0
                 :nds 'to-mem
                 :auto-evex nil
                 :opcode-prefix #x0F38)
               (:emitter
                (cond
                 ((xmm-register-p dst) (aver (ea-p src2))
                  (emit-avx2-inst segment src2 dst ,prefix ,op
                    :opcode-prefix #x0F38
                    :vvvv src
                    :w 0))
                 (t (aver (ea-p dst))
                  (emit-avx2-inst segment dst src2 ,prefix ,to-mem-op
                    :opcode-prefix #x0F38
                    :vvvv src
                    :w 0)))))))
  (def vmaskmovps #x66 44 46)
  (def vmaskmovpd #x66 45 47))

(macrolet ((def (name prefix op w)
             `(define-instruction ,name (segment dst src src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op
                 :w w
                 :nds t
                 :opcode-prefix #x0F38)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,op
                  :opcode-prefix #x0F38
                  :vvvv src
                  :w ,w)))))
  (def vpsrlvd #x66 69 0)
  (def vpsrlvq #x66 69 1)
  (def vpsravd #x66 70 0)
  (def vpsllvd #x66 71 0)
  (def vpsllvq #x66 71 1))

(define-arg-type vmx/y
  :prefilter #'prefilter-reg/mem
  :printer #'print-vmx/y)

(define-arg-type vmx
  :prefilter #'prefilter-reg/mem
  :printer #'print-vmx)

(macrolet ((def (name op w sizing)
             `(define-instruction ,name (segment dst vm mask)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 op
                 :w w
                 :opcode-prefix #x0F38
                 :printer '(:name :tab reg ", " reg/mem ", " vvvv)
                 :more-fields (ecase sizing
                                (xmm/ymm-vmx/y '((reg/mem nil :type 'vmx/y)))
                                (xmm-vmx/y
                                 '((reg nil :type 'xmmreg) (reg/mem nil :type 'vmx/y)))
                                (xmm/ymm-vmx '((reg/mem nil :type 'vmx)))))
               (:emitter
                (emit-avx2-inst segment vm dst #x66 ,op
                  :opcode-prefix #x0F38
                  :vvvv mask
                  :w ,w
                  :l ,(ecase sizing
                        ((xmm/ymm-vmx/y xmm/ymm-vmx)
                         `(if (ymm-register-p dst)
                              1
                              0))
                        (xmm-vmx/y
                         `(if (ymm-register-p (ea-index vm))
                              1
                              0)))
                  :vm t)))))
  (def vpgatherdd 144 0 xmm/ymm-vmx/y)
  (def vpgatherqd 145 0 xmm-vmx/y)
  (def vpgatherdq 144 1 xmm/ymm-vmx)
  (def vpgatherqq 145 1 xmm/ymm-vmx/y)
  (def vgatherdps 146 0 xmm/ymm-vmx/y)
  (def vgatherdpd 146 1 xmm/ymm-vmx)
  (def vgatherqps 147 0 xmm-vmx/y)
  (def vgatherqpd 147 1 xmm/ymm-vmx/y))

(macrolet ((def-insert (name prefix op &key w l disp-ns scalar-disp-n)
             `(define-instruction ,name (segment dst src src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op
                 :w w
                 :l l
                 :nds t
                 :opcode-prefix #x0F38
                 :auto-evex nil)
               ,@(cond
                  (disp-ns
                   (loop for ll in '(0 1 2)
                         for n in disp-ns
                         append (avx512-inst-printer-list 'ymm-ymm/mem prefix op
                                  :w w
                                  :nds t
                                  :opcode-prefix #x0F38
                                  :ll ll
                                  :disp-n n)))
                  (scalar-disp-n
                   (avx512-inst-printer-list 'ymm-ymm/mem prefix op
                     :w w
                     :nds t
                     :opcode-prefix #x0F38
                     :ll 0
                     :disp-n scalar-disp-n)))
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,op
                  :opcode-prefix #x0F38
                  :vvvv src
                  :w ,w
                  :l ,l
                  :disp-n ,(cond
                            (disp-ns
                             `(cond
                               ((zmm-register-p dst) (third ',disp-ns))
                               ((ymm-register-p dst) (second ',disp-ns))
                               ((xmm-register-p dst) (first ',disp-ns))
                               (t 0)))
                            (scalar-disp-n scalar-disp-n) (t 0)))))))
  (def-insert vfmadd132ps #x66 152
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmadd213ps #x66 168
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmadd231ps #x66 184
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmadd132pd #x66 152
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmadd213pd #x66 168
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmadd231pd #x66 184
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfnmadd132ps #x66 156
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfnmadd213ps #x66 172
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfnmadd231ps #x66 188
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfnmadd132pd #x66 156
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfnmadd213pd #x66 172
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfnmadd231pd #x66 188
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfnmsub132ps #x66 158
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfnmsub213ps #x66 174
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfnmsub231ps #x66 190
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfnmsub132pd #x66 158
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfnmsub213pd #x66 174
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfnmsub231pd #x66 190
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmaddsub132ps #x66 150
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmaddsub213ps #x66 166
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmaddsub231ps #x66 182
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmaddsub132pd #x66 150
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmaddsub213pd #x66 166
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmaddsub231pd #x66 182
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmsubadd132ps #x66 151
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmsubadd213ps #x66 167
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmsubadd231ps #x66 183
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmsubadd132pd #x66 151
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmsubadd213pd #x66 167
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmsubadd231pd #x66 183
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmsub132ps #x66 154
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmsub213ps #x66 170
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmsub231ps #x66 186
    :w 0
    :disp-ns (16 32 64))
  (def-insert vfmsub132pd #x66 154
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmsub213pd #x66 170
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmsub231pd #x66 186
    :w 1
    :disp-ns (16 32 64))
  (def-insert vfmadd132ss #x66 153
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfmadd213ss #x66 169
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfmadd231ss #x66 185
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfmadd132sd #x66 153
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfmadd213sd #x66 169
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfmadd231sd #x66 185
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfnmadd132ss #x66 157
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfnmadd213ss #x66 173
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfnmadd231ss #x66 189
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfnmadd132sd #x66 157
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfnmadd213sd #x66 173
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfnmadd231sd #x66 189
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfnmsub132ss #x66 159
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfnmsub213ss #x66 175
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfnmsub231ss #x66 191
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfnmsub132sd #x66 159
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfnmsub213sd #x66 175
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfnmsub231sd #x66 191
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfmsub132ss #x66 155
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfmsub213ss #x66 171
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfmsub231ss #x66 187
    :w 0
    :l 0
    :scalar-disp-n 4)
  (def-insert vfmsub132sd #x66 155
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfmsub213sd #x66 171
    :w 1
    :l 0
    :scalar-disp-n 8)
  (def-insert vfmsub231sd #x66 187
    :w 1
    :l 0
    :scalar-disp-n 8))

(define-instruction vcvtph2ps (segment dst src)
 (:emitter
  (emit-avx2-inst segment src dst #x66 19
    :opcode-prefix #x0F38
    :w 0
    :disp-n (cond ((zmm-register-p dst) 32) ((ymm-register-p dst) 16)
                  ((xmm-register-p dst) 8) (t 0))))
 . #.(append
      (avx2-inst-printer-list 'ymm-ymm/mem #x66 19
        :w 0
        :opcode-prefix #x0F38
        :auto-evex nil)
      (loop for (ll n) in '((0 8) (1 16) (2 32))
            append (avx512-inst-printer-list 'ymm-ymm/mem #x66 19
                     :w 0
                     :opcode-prefix #x0F38
                     :ll ll
                     :disp-n n))))

(define-instruction vcvtps2ph (segment dst src imm)
 (:emitter
  (emit-avx2-inst segment dst src #x66 29
    :opcode-prefix #x0F3A
    :w 0
    :disp-n (cond ((zmm-register-p src) 32) ((ymm-register-p src) 16)
                  ((xmm-register-p src) 8) (t 0))
    :remaining-bytes 1)
  (emit-byte segment imm))
 . #.(append
      (avx2-inst-printer-list 'ymm-ymm/mem #x66 29
        :w 0
        :opcode-prefix #x0F3A
        :auto-evex nil
        :printer '(:name :tab reg/mem ", " reg ", " imm))
      (loop for (ll n) in '((0 8) (1 16) (2 32))
            append (avx512-inst-printer-list 'ymm-ymm/mem #x66 29
                     :w 0
                     :opcode-prefix #x0F3A
                     :ll ll
                     :disp-n n
                     :printer '(:name :tab reg/mem ", " reg ", " imm)))))

(define-instruction vgf2p8mulb (segment dst src1 src2)
 (:emitter
  (emit-avx2-inst segment src2 dst #x66 207
    :opcode-prefix #x0F38
    :vvvv src1
    :w 0
    :disp-n (cond ((zmm-register-p dst) 64) ((ymm-register-p dst) 32)
                  ((xmm-register-p dst) 16) (t 0))))
 . #.(append
      (avx2-inst-printer-list 'ymm-ymm/mem #x66 207
        :opcode-prefix #x0F38
        :w 0
        :nds t
        :auto-evex nil)
      (loop for (ll n) in '((0 16) (1 32) (2 64))
            append (avx512-inst-printer-list 'ymm-ymm/mem #x66 207
                     :opcode-prefix #x0F38
                     :w 0
                     :nds t
                     :ll ll
                     :disp-n n))))

(macrolet ((def (name opcode)
             `(define-instruction ,name (segment dst src1 src2 imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm #x66 opcode
                 :opcode-prefix #x0F3A
                 :w 1
                 :auto-evex nil)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem-imm #x66 opcode
                                :opcode-prefix #x0F3A
                                :w 1
                                :ll ll
                                :disp-n n))
               (:emitter
                (emit-avx2-inst segment src2 dst #x66 ,opcode
                  :opcode-prefix #x0F3A
                  :vvvv src1
                  :w 1
                  :disp-n (cond ((zmm-register-p dst) 64)
                                ((ymm-register-p dst) 32)
                                ((xmm-register-p dst) 16) (t 0))
                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vgf2p8affineqb 206)
  (def vgf2p8affineinvqb 207))

(define-instruction xsave (segment dst)
 (:printer ext-reg/mem-no-width ((op '(174 4))))
 (:emitter (aver (not (register-p dst)))
  (emit-prefixes segment dst nil :do-not-set) (emit-byte segment 15)
  (emit-byte segment 174) (emit-ea segment dst 4)))

(define-instruction xrstor (segment dst)
 (:printer ext-reg/mem-no-width ((op '(174 5))))
 (:emitter (aver (not (register-p dst)))
  (emit-prefixes segment dst nil :do-not-set) (emit-byte segment 15)
  (emit-byte segment 174) (emit-ea segment dst 5)))

(define-instruction-format (vex3-vex-gpr (+ 24 16)
                             :include vex3
                             :default-printer '(:name :tab reg ", " vvvv ", " reg/mem))
  (op :field (byte 8 (+ 24 0)))
  (vvvv :type 'vvvv-reg)
  (reg/mem
    :fields (list (byte 2 (+ 24 14)) (byte 3 (+ 24 8)))
    :type 'reg/mem)
  (reg
    :field (byte 3 (+ 24 11))
    :type 'reg))

(define-instruction mulx (segment &prefix prefix hi lo src)
 (:emitter
  (emit-avx2-inst segment src hi #xF2 246
    :opcode-prefix #x0F38
    :vvvv lo
    :w (ecase (pick-operand-size prefix lo src)
         (:qword 1)
         (:dword 0))))
 . #.(avx2-inst-printer-list 'vex-gpr #xF2 246
       :nds t
       :opcode-prefix #x0F38))

(define-instruction adcx (segment &prefix prefix dst src)
 (:printer ext-2byte-prefix-reg-reg/mem ((prefix #x66) (op1 56) (op2 246)))
 (:emitter
  (let ((size (pick-operand-size prefix dst src)))
    (aver (memq size '(:dword :qword)))
    (emit-sse-inst-2byte segment dst src #x66 56 246 :operand-size size))))

(define-instruction adox (segment &prefix prefix dst src)
 (:printer ext-2byte-prefix-reg-reg/mem ((prefix #xF3) (op1 56) (op2 246)))
 (:emitter
  (let ((size (pick-operand-size prefix dst src)))
    (aver (memq size '(:dword :qword)))
    (emit-sse-inst-2byte segment dst src #xF3 56 246 :operand-size size))))

(macrolet ((def (name prefix opcode third-op-name)
             `(define-instruction ,name
               (segment &prefix prefix dst src ,third-op-name)
               ,@(avx2-inst-printer-list 'vex-gpr prefix opcode
                 :printer '(:name :tab reg ", " reg/mem ", " vvvv)
                 :nds t
                 :opcode-prefix #x0F38)
               (:emitter
                (emit-avx2-inst segment src dst ,prefix ,opcode
                  :opcode-prefix #x0F38
                  :vvvv ,third-op-name
                  :l 0
                  :w (ecase (pick-operand-size prefix dst src)
                       (:qword 1)
                       (:dword 0)))))))
  (def shrx #xF2 247 count)
  (def shlx #x66 247 count)
  (def sarx #xF3 247 count)
  (def bzhi nil 245 position)
  (def bextr nil 247 control))

(macrolet ((def (name prefix opcode &optional (third-op-name 'src2) printer)
             `(define-instruction ,name
               (segment &prefix prefix dst src ,third-op-name)
               ,@(avx2-inst-printer-list 'vex-gpr prefix opcode
                 :nds t
                 :printer printer
                 :opcode-prefix #x0F38)
               (:emitter
                (emit-avx2-inst segment ,third-op-name dst ,prefix ,opcode
                  :opcode-prefix #x0F38
                  :vvvv src
                  :l 0
                  :w (ecase (pick-operand-size prefix dst src)
                       (:qword 1)
                       (:dword 0)))))))
  (def pext #xF3 245 mask)
  (def pdep #xF2 245 mask)
  (def andn nil 242))

(define-instruction rorx (segment &prefix prefix dst src imm)
 (:emitter
  (emit-avx2-inst segment src dst #xF2 240
    :opcode-prefix #x0F3A
    :l 0
    :w (ecase (pick-operand-size prefix dst src)
         (:qword 1)
         (:dword 0))
    :remaining-bytes 1)
  (emit-byte segment imm))
 . #.(avx2-inst-printer-list 'vex-gpr #xF2 240
       :opcode-prefix #x0F3A
       :printer '(:name :tab reg ", " reg/mem)))

(macrolet ((def (name reg)
             `(define-instruction ,name (segment &prefix prefix dst src)
               ,@(avx2-inst-printer-list 'vex-gpr nil 243
                 :opcode-prefix #x0F38
                 :more-fields `((reg ,reg))
                 :printer '(:name :tab vvvv ", " reg/mem))
               (:emitter
                (emit-avx2-inst segment src ,reg nil 243
                  :opcode-prefix #x0F38
                  :vvvv dst
                  :l 0
                  :w (ecase (pick-operand-size prefix dst src)
                       (:qword 1)
                       (:dword 0)))))))
  (def blsr 1)
  (def blsmsk 2)
  (def blsi 3))

;;;; Intel APX (Advanced Performance Extensions) Support

(defun determine-apx-flags (thing reg vvvv)
  "Extract APX EVEX prefix flags from operands.
Returns: r, x, b, r-prime, b-prime, v-prime, x-prime."
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
      (emit-evex segment r x b r-prime opcode-prefix w
                 vvvv-num pp 0 0 0 v-prime 0
                 :b-prime b-prime :x-prime x-prime :nd nd :nf nf :scc scc :dfv dfv)
      (emit-bytes segment opcode)
      (emit-ea segment thing reg :remaining-bytes remaining-bytes))))

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

(defun emit-apx-unary-ndd (segment dst src opcode subop &key (nf 0))
  (let* ((size (or (operand-size dst) (operand-size src) :qword))
         (w (if (eq size :qword) 1 0))
         (pp (if (eq size :word) 1 0)))
    (emit-apx-inst segment src subop (if (eq size :byte) (logand opcode #xFE) opcode)
                   :vvvv dst :nd 1 :nf nf :w w :pp pp)))

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

;;; APX PUSH2 / POP2
(define-instruction push2 (segment src1 src2)
  (:printer apx-push2 ())
  (:emitter
   (emit-apx-inst segment src2 6 #xFF :vvvv src1 :nd 1 :w 0)))

(define-instruction pop2 (segment dst1 dst2)
  (:printer apx-pop2 ())
  (:emitter
   (emit-apx-inst segment dst2 0 #x8F :vvvv dst1 :nd 1 :w 0)))

;;; APX CFCMOVcc
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
  (:printer apx-cfcmov ())
  (:emitter
   (let* ((cc (parse-apx-condition cond))
          (opcode (+ #x40 cc))
          (size (or (operand-size dst) (operand-size src) :qword))
          (w (if (eq size :qword) 1 0))
          (pp (if (eq size :word) 1 0)))
     (emit-apx-inst segment src dst opcode :w w :pp pp :nd 0))))

;;; APX CCMP / CTEST
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
  (:printer apx-ccmp ((op #x39)))
  (:printer apx-ccmp ((op #x38)))
  (:printer apx-ccmp-imm ((op #x81) (reg 7)))
  (:printer apx-ccmp-imm ((op #x80) (reg 7)))
  (:printer apx-ccmp-imm8 ((op #x83) (reg 7)))
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
  (:printer apx-ctest ((op #x85)))
  (:printer apx-ctest ((op #x84)))
  (:printer apx-ctest-imm ((op #xF7) (reg 0)))
  (:printer apx-ctest-imm ((op #xF6) (reg 0)))
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

;;; Explicit NDD ALU instructions
(macrolet ((def-alu-ndd (name subop)
             (let ((msym (intern (subseq (string name) 0 (- (length (string name)) 4)))))
               `(define-instruction ,name (segment dst src1 src2 &key (nf 0))
                  (:printer apx-ndd-reg-reg ((op ,(dpb subop (byte 3 3) 1)))
                            :default :print-name ',msym)
                  (:printer apx-ndd-reg-reg ((op ,(dpb subop (byte 3 3) 0)))
                            :default :print-name ',msym)
                  (:printer apx-ndd-reg-mem ((op ,(dpb subop (byte 3 3) 3)))
                            :default :print-name ',msym)
                  (:printer apx-ndd-reg-mem ((op ,(dpb subop (byte 3 3) 2)))
                            :default :print-name ',msym)
                  (:printer apx-ndd-reg-imm ((op #x81) (reg ,subop))
                            :default :print-name ',msym)
                  (:printer apx-ndd-reg-imm ((op #x80) (reg ,subop))
                            :default :print-name ',msym)
                  (:printer apx-ndd-reg-imm8 ((op #x83) (reg ,subop))
                            :default :print-name ',msym)
                  (:emitter (emit-apx-alu-ndd segment dst src1 src2 ,subop :nf nf))))))
  (def-alu-ndd add-ndd #b000)
  (def-alu-ndd or-ndd  #b001)
  (def-alu-ndd adc-ndd #b010)
  (def-alu-ndd sbb-ndd #b011)
  (def-alu-ndd and-ndd #b100)
  (def-alu-ndd sub-ndd #b101)
  (def-alu-ndd xor-ndd #b110))

(macrolet ((def-unary-ndd (name opcode subop)
             (let ((msym (intern (subseq (string name) 0 (- (length (string name)) 4)))))
               `(define-instruction ,name (segment dst src &key (nf 0))
                  (:printer apx-ndd-unary ((op ,opcode) (reg ,subop))
                            :default :print-name ',msym)
                  (:printer apx-ndd-unary ((op ,(logand opcode #xFE)) (reg ,subop))
                            :default :print-name ',msym)
                  (:emitter (emit-apx-unary-ndd segment dst src ,opcode ,subop :nf nf))))))
  (def-unary-ndd not-ndd #xF7 #b010)
  (def-unary-ndd neg-ndd #xF7 #b011)
  (def-unary-ndd inc-ndd #xFF #b000)
  (def-unary-ndd dec-ndd #xFF #b001))

(macrolet ((def-shift-ndd (name subop)
             (let ((msym (intern (subseq (string name) 0 (- (length (string name)) 4)))))
               `(define-instruction ,name (segment dst src count &key (nf 0))
                  (:printer apx-ndd-shift-imm ((op #xC1) (reg ,subop))
                            :default :print-name ',msym)
                  (:printer apx-ndd-shift-imm ((op #xC0) (reg ,subop))
                            :default :print-name ',msym)
                  (:printer apx-ndd-shift-1 ((op #xD1) (reg ,subop))
                            :default :print-name ',msym)
                  (:printer apx-ndd-shift-1 ((op #xD0) (reg ,subop))
                            :default :print-name ',msym)
                  (:printer apx-ndd-shift-cl ((op #xD3) (reg ,subop))
                            :default :print-name ',msym)
                  (:printer apx-ndd-shift-cl ((op #xD2) (reg ,subop))
                            :default :print-name ',msym)
                  (:emitter (emit-apx-shift-ndd segment dst src count ,subop :nf nf))))))
  (def-shift-ndd rol-ndd #b000)
  (def-shift-ndd ror-ndd #b001)
  (def-shift-ndd shl-ndd #b100)
  (def-shift-ndd shr-ndd #b101)
  (def-shift-ndd sar-ndd #b111))

(define-instruction imul-ndd (segment dst src1 src2)
  (:printer apx-ndd-reg-reg ((op #xAF))
            :default :print-name 'imul)
  (:printer apx-ndd-reg-imm ((op #x69) (reg 0))
            :default :print-name 'imul)
  (:printer apx-ndd-reg-imm8 ((op #x6B) (reg 0))
            :default :print-name 'imul)
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
            (emit-apx-inst segment src1 src2 #xAF :vvvv dst :nd 1 :w w :pp pp))))))


