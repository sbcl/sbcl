
(in-package "SB-X86-64-ASM")


(define-arg-type k-vvvv-reg :prefilter #'invert-4 :printer #'print-kreg)


(define-arg-type ymmreg :prefilter #'prefilter-reg-r :printer #'print-ymmreg)


(define-arg-type ymmreg-b :prefilter #'prefilter-reg-b :printer #'print-ymmreg)


(define-arg-type ymm-vvvv-reg :prefilter #'invert-4 :printer #'print-ymmreg)


(define-arg-type vvvv-reg :prefilter #'invert-4 :printer #'print-reg)


(define-arg-type ymm-reg-is4 :printer #'print-ymmreg)


(define-arg-type ymmreg/mem :prefilter #'prefilter-xmmreg/mem :printer
 #'print-ymmreg/mem)


(define-arg-type half-ymmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-half-ymmreg/mem)

(macrolet ((define-disp-arg-type (name n)
             `(define-arg-type ,name :prefilter
               (lambda (dstate mod r/m)
                 (decode-mod-r/m dstate mod r/m 'fpr :disp-n ,n))
               :printer #'print-ymmreg/mem)))
  (define-disp-arg-type evex-ymmreg/mem-disp1 1)
  (define-disp-arg-type evex-ymmreg/mem-disp2 2)
  (define-disp-arg-type evex-ymmreg/mem-disp4 4)
  (define-disp-arg-type evex-ymmreg/mem-disp8 8)
  (define-disp-arg-type evex-ymmreg/mem-disp16 16)
  (define-disp-arg-type evex-ymmreg/mem-disp32 32)
  (define-disp-arg-type evex-ymmreg/mem-disp64 64))


(define-arg-type vm :prefilter #'prefilter-xmmreg/mem :printer
 #'print-ymmreg/mem)


(define-arg-type sized-xmmreg/mem :prefilter #'prefilter-xmmreg/mem :printer
 #'print-sized-xmmreg/mem)


(define-arg-type sized-byte-xmmreg/mem :prefilter #'prefilter-xmmreg/mem
 :printer #'print-sized-byte-xmmreg/mem)


(define-arg-type sized-word-xmmreg/mem :prefilter #'prefilter-xmmreg/mem
 :printer #'print-sized-word-xmmreg/mem)


(define-arg-type sized-dword-xmmreg/mem :prefilter #'prefilter-xmmreg/mem
 :printer #'print-sized-dword-xmmreg/mem)


(define-arg-type sized-xmmreg/mem-default-qword :prefilter
 #'prefilter-xmmreg/mem :printer #'print-sized-xmmreg/mem-default-qword)


(defvar *evex-emitter-table*
  nil
  "EVEX emitter lookup table. Set by evex-emitters.lisp.")


(defconstant +evex+ 16384)


(defconstant +vex-l+ 1024)


(defconstant +evex-l1+ 2048)


(defconstant +evex-r-prime+ 4096)


(defconstant +evex-b-prime+ 8192)


(defconstant +evex-v-prime+ 32768)


(defconstant +vsib+ 65536)


(define-arg-type evex-vsib/mem :prefilter
 (lambda (dstate mod r/m)
   (dstate-setprop dstate +vsib+)
   (decode-mod-r/m dstate mod r/m 'fpr))
 :printer #'print-vsib/mem)


(define-arg-type evex-vsib-disp4 :prefilter
 (lambda (dstate mod r/m)
   (dstate-setprop dstate +vsib+)
   (decode-mod-r/m dstate mod r/m 'fpr :disp-n 4))
 :printer #'print-vsib/mem)


(define-arg-type evex-vsib-disp8 :prefilter
 (lambda (dstate mod r/m)
   (dstate-setprop dstate +vsib+)
   (decode-mod-r/m dstate mod r/m 'fpr :disp-n 8))
 :printer #'print-vsib/mem)


(define-arg-type vex-l :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value)
                       +vex-l+
                       0))))


(define-arg-type vex-w :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value)
                       +rex-w+
                       0))))


(define-arg-type vex-r :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value)
                       0
                       +rex-r+))))


(define-arg-type vex-x :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value)
                       0
                       +rex-x+))))


(define-arg-type vex-b :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value)
                       0
                       +rex-b+))))


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


(define-arg-type avx-condition-code :type 'imm-byte :printer +avx-conditions+)


(define-arg-type evex-r-prime :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (zerop value)
                       +evex-r-prime+
                       0))))


(define-arg-type evex-fixed :prefilter
 (lambda (dstate value)
   (declare (ignore value))
   (dstate-setprop dstate +evex+)))


(define-arg-type evex-v-prime :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (zerop value)
                       +evex-v-prime+
                       0))))


(define-arg-type evex-ll :prefilter
 (lambda (dstate value) (dstate-setprop dstate (ash value 10))))


(define-arg-type evex-w :prefilter
 (lambda (dstate value)
   (dstate-setprop dstate
                   (if (plusp value)
                       +rex-w+
                       0))))


(define-arg-type evex-ymm-vvvv-reg :prefilter #'invert-4 :printer
 #'print-ymmreg-vvvv)


(define-arg-type opmask-reg :printer #'print-opmask-register)


(define-arg-type kreg :prefilter
 (lambda (dstate value) (declare (ignore dstate)) (get-fpr :kreg value))
 :printer #'print-kreg)


(define-arg-type kreg/mem :prefilter
 (lambda (dstate mod r/m)
   (if (= mod 3)
       (get-fpr :kreg r/m)
       (decode-mod-r/m dstate mod r/m 'gpr)))
 :printer #'print-kreg/mem)


(define-instruction-format (vex2 16) (vex :field (byte 8 0) :value 197)
 (r :field (byte 1 (+ 8 7)) :type 'vex-r)
 (vvvv :field (byte 4 (+ 8 3)) :type 'ymm-vvvv-reg)
 (l :field (byte 1 (+ 8 2)) :type 'vex-l) (pp :field (byte 2 (+ 8 0))))


(define-instruction-format (vex3 24) (vex :field (byte 8 0) :value 196)
 (r :field (byte 1 (+ 8 7)) :type 'vex-r)
 (x :field (byte 1 (+ 8 6)) :type 'vex-x)
 (b :field (byte 1 (+ 8 5)) :type 'vex-b) (m-mmmm :field (byte 5 (+ 8 0)))
 (w :field (byte 1 (+ 16 7)) :type 'vex-w)
 (vvvv :field (byte 4 (+ 16 3)) :type 'ymm-vvvv-reg)
 (l :field (byte 1 (+ 16 2)) :type 'vex-l) (pp :field (byte 2 (+ 16 0))))


(define-instruction-format (vex2-op 24 :include vex2 :default-printer '(:name))
 (op :field (byte 8 16)))


(defmacro define-vex-instruction-format
          ((format-name length-in-bits &key default-printer include)
           &body arg-specs)
  `(progn
    (define-instruction-format
     (,(symbolicate "VEX2-" format-name) (+ 16 ,length-in-bits) :include
      ,(if include
           (symbolicate "VEX2-" include)
           'vex2)
      :default-printer ,default-printer)
     ,@(subst 16 'start arg-specs))
    (define-instruction-format
     (,(symbolicate "VEX3-" format-name) (+ 24 ,length-in-bits) :include
      ,(if include
           (symbolicate "VEX3-" include)
           'vex3)
      :default-printer ,default-printer)
     ,@(subst 24 'start arg-specs))))


(define-vex-instruction-format
 (ymm-ymm/mem 16 :default-printer '(:name :tab reg ", " reg/mem))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'ymmreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'ymmreg) (imm))


(define-vex-instruction-format
 (ymm-ymm/mem-imm 16 :default-printer
  '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'ymmreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'ymmreg) (imm :type 'imm-byte))


(define-vex-instruction-format
 (ymm-ymm/mem-ymm 24 :default-printer
  '(:name :tab reg ", " vvvv ", " reg/mem ", " reg4))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'ymmreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'ymmreg)
 (reg4 :field (byte 4 (+ start 16 4)) :type 'ymm-reg-is4))


(define-vex-instruction-format
 (ymm-ymm/mem-dir 16 :include ymm-ymm/mem :default-printer
  `(:name :tab
    (:if (reg/mem :test machine-ea-p)
     (:if (dir :constant 0) (reg ", " reg/mem) (reg/mem ", " reg))
     (reg ", " vvvv ", " reg/mem))))
 (op :field (byte 7 (+ start 1))) (dir :field (byte 1 (+ start 0))))


(define-vex-instruction-format
 (ymm-ymm-imm 16 :default-printer '(:name :tab vvvv ", " reg ", " imm))
 (op :field (byte 8 (+ start 0))) (/i :field (byte 3 (+ start 11)))
 (b11 :field (byte 2 (+ start 14)) :value 3)
 (reg :field (byte 3 (+ start 8)) :type 'ymmreg-b) (imm :type 'imm-byte))


(define-vex-instruction-format
 (reg-ymm/mem 16 :include ymm-ymm/mem :default-printer
  '(:name :tab reg ", " reg/mem))
 (reg :field (byte 3 (+ start 11)) :type 'reg))

(define-instruction-format (evex 32)
  (evex-prefix :field (byte 8 0) :value #x62)
  ;; Byte 1
  (r        :field (byte 1 15) :type 'vex-r)
  (x        :field (byte 1 14) :type 'vex-x)
  (b        :field (byte 1 13) :type 'vex-b)
  (r-prime  :field (byte 1 12) :type 'evex-r-prime)
  (reserved :field (byte 1 11) :value 0) ; bit 11 reserved (0 in EVEX)
  (mm       :field (byte 3 8))          ; bits 10:8 are mmm (maps 1, 2, 3, 5, 6)
  ;; Byte 2
  (w          :field (byte 1 23) :type 'evex-w)
  (vvvv       :field (byte 4 19) :type 'evex-ymm-vvvv-reg)
  (evex-fixed :field (byte 1 18) :value 1 :type 'evex-fixed) ; must be 1 for EVEX
  (pp         :field (byte 2 16))
  ;; Byte 3
  (z-bit   :field (byte 1 31))
  (ll      :field (byte 2 29) :type 'evex-ll)
  (evex-b  :field (byte 1 28))
  (v-prime :field (byte 1 27) :type 'evex-v-prime)
  (aaa     :field (byte 3 24) :type 'opmask-reg))

(define-vex-instruction-format
 (kreg-kreg/mem 16 :default-printer '(:name :tab reg ", " reg/mem))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'kreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'kreg))


(define-vex-instruction-format
 (kreg-reg/mem 16 :default-printer '(:name :tab reg ", " reg/mem))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'reg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'kreg))


(define-evex-instruction-format (ymm-ymm-imm 16
                                 :default-printer '(:name :tab vvvv ", " reg ", " imm))
  (op  :field (byte 8 (+ start 0)))
  (/i  :field (byte 3 (+ start 11)))
  (b11 :field (byte 2 (+ start 14))
       :value #b11)
  (reg :field (byte 3 (+ start 8))
       :type 'evex-ymmreg-b)
  (imm :type 'imm-byte))


(define-vex-instruction-format
 (reg-kreg/mem 16 :default-printer '(:name :tab reg ", " reg/mem))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'kreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'reg))


(define-vex-instruction-format
 (kreg-kreg/mem-k 16 :default-printer '(:name :tab reg ", " vvvv ", " reg/mem))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'kreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'kreg) (vvvv :type 'k-vvvv-reg))


(define-vex-instruction-format
 (kreg-kreg/mem-imm 16 :default-printer
  '(:name :tab reg ", " reg/mem ", " imm))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'kreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'kreg) (imm :type 'imm-byte))


(define-instruction-format (evex 32) (evex-prefix :field (byte 8 0) :value 98)
 (r :field (byte 1 15) :type 'vex-r) (x :field (byte 1 14) :type 'vex-x)
 (b :field (byte 1 13) :type 'vex-b)
 (r-prime :field (byte 1 12) :type 'evex-r-prime)
 (reserved :field (byte 2 10) :value 0) (mm :field (byte 2 8))
 (w :field (byte 1 23) :type 'evex-w)
 (vvvv :field (byte 4 19) :type 'evex-ymm-vvvv-reg)
 (evex-fixed :field (byte 1 18) :value 1 :type 'evex-fixed)
 (pp :field (byte 2 16)) (z-bit :field (byte 1 31))
 (ll :field (byte 2 29) :type 'evex-ll) (evex-b :field (byte 1 28))
 (v-prime :field (byte 1 27) :type 'evex-v-prime)
 (aaa :field (byte 3 24) :type 'opmask-reg))


(defmacro define-evex-instruction-format
          ((format-name length-in-bits &key default-printer include)
           &body arg-specs)
  `(define-instruction-format
    (,(symbolicate "EVEX-" format-name) (+ 32 ,length-in-bits) :include
     ,(if include
          (symbolicate "EVEX-" include)
          'evex)
     :default-printer ,default-printer)
    ,@(subst 32 'start arg-specs)))


(define-evex-instruction-format
 (kreg-reg/mem 16 :default-printer '(:name :tab reg ", " vvvv ", " reg/mem))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'reg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'kreg))


(define-evex-instruction-format
 (kreg-reg/mem-imm 16 :default-printer
  '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'reg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'kreg) (imm :type 'imm-byte))


(define-evex-instruction-format
 (ymm-ymm/mem 16 :default-printer '(:name :tab reg ", " reg/mem))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'ymmreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'ymmreg) (imm))


(define-evex-instruction-format
 (ymm-ymm/mem-imm 16 :default-printer
  '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'ymmreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'ymmreg) (imm :type 'imm-byte))


(define-evex-instruction-format
 (ymm-ymm/mem-dir 16 :include ymm-ymm/mem :default-printer
  `(:name :tab
    (:if (reg/mem :test machine-ea-p)
     (:if (dir :constant 0) (reg ", " reg/mem) (reg/mem ", " reg))
     (reg ", " vvvv ", " reg/mem))))
 (op :field (byte 7 (+ start 1))) (dir :field (byte 1 (+ start 0))))


(define-evex-instruction-format
 (ymm-ymm-imm 16 :default-printer '(:name :tab vvvv ", " reg ", " imm))
 (op :field (byte 8 (+ start 0))) (/i :field (byte 3 (+ start 11)))
 (b11 :field (byte 2 (+ start 14)) :value 3)
 (reg :field (byte 3 (+ start 8)) :type 'ymmreg-b) (imm :type 'imm-byte))


(define-evex-instruction-format
 (reg-ymm/mem 16 :include ymm-ymm/mem :default-printer
  '(:name :tab reg ", " reg/mem))
 (reg :field (byte 3 (+ start 11)) :type 'reg))


(define-evex-instruction-format
 (ymm-ymm/mem-ymm 24 :default-printer
  '(:name :tab reg ", " vvvv ", " reg/mem ", " reg4))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'ymmreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'ymmreg)
 (reg4 :field (byte 4 (+ start 16 4)) :type 'ymm-reg-is4))


(define-evex-instruction-format
 (2mask-nds 16 :default-printer
  '(:name :tab reg ", " aaa ", " vvvv ", " reg/mem))
 (op :field (byte 8 (+ start 0)))
 (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8))) :type
  'ymmreg/mem)
 (reg :field (byte 3 (+ start 11)) :type 'kreg))


(define-instruction-format
 (evex-vex-gpr (+ 32 16) :include evex :default-printer
  '(:name :tab reg ", " vvvv ", " reg/mem))
 (op :field (byte 8 (+ 32 0))) (vvvv :type 'vvvv-reg)
 (reg/mem :fields (list (byte 2 (+ 32 14)) (byte 3 (+ 32 8))) :type 'reg/mem)
 (reg :field (byte 3 (+ 32 11)) :type 'reg))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vex-encode-pp (pp) (ecase pp ((nil) 0) (102 1) (243 2) (242 3)))
  (defun vex-encode-m-mmmm (m-mmmm) (ecase m-mmmm (15 1) (3896 2) (3898 3)))
  (defun evex-encode-mm (m-mmmm)
    (ecase m-mmmm
      (#x0F   #b001)
      (#x0F38 #b010)
      (#x0F3A #b011)
      ((:map5 5) #b101)
      ((:map6 6) #b110))))

(defun emit-two-byte-vex (segment r vvvv l pp)
  (emit-bytes segment 197
   (logior (ash (logxor 1 r) 7) (ash (logxor vvvv 15) 3) (ash l 2)
           (vex-encode-pp pp))))


(defun emit-three-byte-vex (segment r x b m-mmmm w vvvv l pp)
  (emit-bytes segment 196
   (logior (ash (logxor 1 r) 7) (ash (logxor 1 x) 6) (ash (logxor 1 b) 5)
           (vex-encode-m-mmmm m-mmmm))
   (logior (ash w 7) (ash (logxor vvvv 15) 3) (ash l 2) (vex-encode-pp pp))))


(defun determine-vex-flags (thing reg l)
  (flet ((reg-7-p (reg-id)
           (if (<= (reg-id-num reg-id) 7)
               0
               1))
         (xmm-size (r)
           (cond ((is-ymm-id-p (reg-id r)) 1) ((xmm-register-p r) 0))))
    (let ((l
           (cond ((eq l :from-thing) (xmm-size thing)) (l)
                 ((xmm-register-p reg) (xmm-size reg))
                 ((xmm-register-p thing) (xmm-size thing)) (t 0)))
          (r
           (if (or (null reg) (integerp reg))
               0
               (reg-7-p (reg-id reg))))
          (x
           (cond
            ((and (ea-p thing) (ea-index thing))
             (let ((index (ea-index thing)))
               (cond ((gpr-p index) (reg-7-p (reg-id (tn-reg index))))
                     ((<= (tn-offset index) 7) 0) (t 1))))
            (t 0)))
          (b
           (reg-7-p
            (cond
             ((ea-p thing)
              (let ((base (ea-base thing)))
                (if (and base (neq base rip-tn))
                    (reg-id (tn-reg base))
                    0)))
             ((register-p thing) (reg-id thing)) (t 0)))))
      (values l r x b))))


(defun emit-vex (segment vvvv thing reg prefix opcode-prefix l w)
  (multiple-value-bind (l r x b)
      (determine-vex-flags thing reg l)
    (let ((vvvv
           (if vvvv
               (reg-id-num (reg-id vvvv))
               0)))
      (if (and (= 0 x w b) (= opcode-prefix 15))
          (emit-two-byte-vex segment r vvvv l prefix)
          (emit-three-byte-vex segment r x b opcode-prefix w vvvv l prefix)))))

;;; EVEX prefix encoding for AVX-512
;;; EVEX is a 4-byte prefix: 62h | P1 | P2 | P3
;;; P1: R(7) X(6) B(5) R'(4) 0(3) mmm(2:0)
;;; P2: W(7) vvvv(6:3) 1(2) pp(1:0)
;;; P3: z(7) L'(6) L(5) b(4) V'(3) aaa(2:0)
;;; R, X, B, R', V' are inverted. vvvv is inverted.

(defun emit-evex (segment r x b r-prime opcode-prefix w vvvv pp z ll evex-b v-prime aaa)
  (emit-bytes segment
              #x62
              ;; P1: R X B R' 0 mmm
              (logior (ash (logxor 1 r) 7)
                      (ash (logxor 1 x) 6)
                      (ash (logxor 1 b) 5)
                      (ash (logxor 1 r-prime) 4)
                      ;; bit 3 is reserved (0), bits 2:0 are mmm (maps 1, 2, 3, 5, 6)
                      (evex-encode-mm opcode-prefix))
              ;; P2: W vvvv 1 pp
              (logior (ash w 7)
                      (ash (logandc1 vvvv #b1111) 3)
                      #b100  ; bit 2 always 1
                      (vex-encode-pp pp))
              ;; P3: z L'L b V' aaa
              (logior (ash z 7)
                      (ash ll 5)
                      (ash evex-b 4)
                      (ash (logxor 1 v-prime) 3)
                      aaa)))

(defun determine-evex-flags (thing reg ll vvvv &optional vsib)
  "Extract EVEX prefix flags from operands.
Returns: ll, r, x, b, r-prime, v-prime.
EVEX uses independent bit3 (R/B) and bit4 (R'/X) for 32-register encoding."
  (flet ((reg-bit3 (reg-id)
           (if (logbitp 3 (reg-id-num reg-id))
               1
               0))
         (reg-bit4 (reg-id)
           (if (logbitp 4 (reg-id-num reg-id))
               1
               0))
         (fpr-size (r)
           (cond ((is-zmm-id-p (reg-id r)) 2) ((is-ymm-id-p (reg-id r)) 1)
                 ((xmm-register-p r) 0))))
    (let ((ll
           (cond (ll ll)
                 ((and reg (xmm-register-p reg)
                       (not (is-kreg-id-p (reg-id reg))))
                  (fpr-size reg))
                 ((and (register-p thing) (xmm-register-p thing)
                       (not (is-kreg-id-p (reg-id thing))))
                  (fpr-size thing))
                 ((and vvvv (register-p vvvv) (xmm-register-p vvvv))
                  (fpr-size vvvv))
                 (t 0)))
          (r
           (if (null reg)
               0
               (reg-bit3 (reg-id reg))))
          (r-prime
           (if (or (null reg) (k-register-p reg))
               0
               (reg-bit4 (reg-id reg))))
          (x
           (cond (vsib 0)
                 ((and (ea-p thing) (ea-index thing))
                  (let ((index (ea-index thing)))
                    (cond ((gpr-p index) (reg-bit3 (reg-id (tn-reg index))))
                          ((<= (tn-offset index) 7) 0) (t 1))))
                 ((register-p thing) (reg-bit4 (reg-id thing))) (t 0)))
          (b
           (reg-bit3
            (cond
             ((ea-p thing)
              (let ((base (ea-base thing)))
                (if (and base (neq base rip-tn))
                    (reg-id (tn-reg base))
                    0)))
             ((register-p thing) (reg-id thing)) (t 0))))
          (v-prime
           (cond
            (vsib
             (let ((index (and (ea-p thing) (ea-index thing))))
               (if index
                   (if (logbitp 4 (tn-offset index))
                       1
                       0)
                   0)))
            (vvvv (reg-bit4 (reg-id vvvv))) (t 0))))
      (values ll r x b r-prime v-prime))))


;;; True if THING is an XMM/YMM/ZMM register numbered 16-31. Such a register
;;; can only be addressed via EVEX (using the R'/X'/V' extension bits), even
;;; when the operation itself is only XMM- or YMM-width.
(defun high-fpr-p (thing)
  (and (register-p thing)
       (let ((id (reg-id thing)))
         (and (not (is-gpr-id-p id)) (not (is-kreg-id-p id))
              (>= (reg-id-num id) 16)))))


(defun lookup-evex-disp-n (prefix opcode opcode-prefix w ll)
  "Return the compressed displacement scale factor N for an EVEX instruction,
   or 0 if not found."
  (let* ((pp (vex-encode-pp prefix))
         (map (evex-encode-mm opcode-prefix))
         (key (list pp opcode map w))
         (entries (cdr (assoc key *evex-emitter-table* :test #'equal))))
    (when entries
      (or (getf (find-if (lambda (e) (= (getf e :ll) ll)) entries) :disp-n)
          (getf (car entries) :disp-n) 0))))


(defun emit-avx512-inst
       (segment thing reg prefix opcode
        &key (remaining-bytes 0) ll (opcode-prefix 15) (w 0) vvvv (aaa 0) (z 0)
        (evex-b 0) vm (disp-n 0))
  (multiple-value-bind (ll r x b r-prime v-prime)
      (determine-evex-flags thing reg ll vvvv vm)
    (let* ((vvvv-num
            (if vvvv
                (reg-id-num (reg-id vvvv))
                0))
           (effective-disp-n
            (if (zerop disp-n)
                (or (lookup-evex-disp-n prefix opcode opcode-prefix w ll) 0)
                disp-n)))
      (emit-evex segment r x b r-prime opcode-prefix w vvvv-num prefix z ll
                 evex-b v-prime aaa)
      (emit-bytes segment opcode)
      (emit-ea segment thing reg :remaining-bytes remaining-bytes :xmm-index vm
               :disp-n effective-disp-n))))

(defun emit-avx512-inst-imm
       (segment thing reg imm prefix opcode /i
        &key ll (w 0) (opcode-prefix 15) (aaa 0) (z 0) (evex-b 0) (disp-n 0))
  (declare (ignorable disp-n))
  (aver (<= 0 /i 7))
  (multiple-value-bind (ll r x b r-prime v-prime)
      (determine-evex-flags reg nil ll thing)
    (let* ((effective-disp-n
            (if (zerop disp-n)
                (or (lookup-evex-disp-n prefix opcode opcode-prefix w ll) 0)
                disp-n))
           (vvvv-num
            (if thing
                (reg-id-num (reg-id thing))
                0)))
      (declare (ignorable effective-disp-n))
      (emit-evex segment r x b r-prime opcode-prefix w vvvv-num prefix z ll
                 evex-b v-prime aaa)))
  (emit-bytes segment opcode)
  (emit-byte segment
             (logior (ash (logior 24 /i) 3) (reg-encoding reg segment)))
  (emit-byte segment imm))


(defun emit-avx512-inst-imm-nonds
       (segment dst src imm prefix opcode /i
        &key ll (w 0) (opcode-prefix 15) (aaa 0) (z 0) (evex-b 0) (disp-n 0))
  (aver (<= 0 /i 7))
  (multiple-value-bind (ll r x b r-prime v-prime)
      (determine-evex-flags src dst ll nil)
    (let* ((vvvv-num 15)
           (effective-disp-n
            (if (zerop disp-n)
                (or (lookup-evex-disp-n prefix opcode opcode-prefix w ll) 0)
                disp-n)))
      (emit-evex segment r x b r-prime opcode-prefix w vvvv-num prefix z ll
                 evex-b v-prime aaa)
      (emit-bytes segment opcode)
      (emit-ea segment src dst :disp-n effective-disp-n)
      (emit-byte segment imm))))


(defun emit-avx2-inst-imm
       (segment thing reg imm prefix opcode /i
        &key l (w 0) evex-w (opcode-prefix 15) (disp-n 0))
  (aver (<= 0 /i 7))
  (when (or (and (register-p reg) (is-zmm-id-p (reg-id reg)))
            (high-fpr-p reg) (high-fpr-p thing))
    (multiple-value-bind (ll r x b r-prime v-prime)
        (determine-evex-flags reg nil l thing)
      (declare (ignorable r x b r-prime v-prime))
      (let ((effective-disp-n
             (if (zerop disp-n)
                 (or
                  (lookup-evex-disp-n prefix opcode opcode-prefix (or evex-w w)
                   ll)
                  0)
                 disp-n)))
        (return-from emit-avx2-inst-imm
          (emit-avx512-inst-imm segment thing reg imm prefix opcode /i :ll ll
                                :w (or evex-w w) :opcode-prefix opcode-prefix
                                :disp-n effective-disp-n)))))
  (emit-vex segment thing reg nil prefix opcode-prefix l w)
  (emit-bytes segment opcode)
  (emit-byte segment
             (logior (ash (logior 24 /i) 3) (reg-encoding reg segment)))
  (emit-byte segment imm))


(defun emit-avx2-inst
       (segment thing reg prefix opcode
        &key (remaining-bytes 0) l (opcode-prefix 15) (w 0) evex-w vvvv is4 vm
        (disp-n 0))
  (when
      (or (and (register-p reg) (is-zmm-id-p (reg-id reg)))
          (and (register-p thing) (is-zmm-id-p (reg-id thing)))
          (and vvvv (register-p vvvv) (is-zmm-id-p (reg-id vvvv)))
          (high-fpr-p reg) (high-fpr-p thing) (high-fpr-p vvvv))
    ;; L is a single VEX bit (0 or 1) and can't represent the ZMM (LL=2)
    ;; case, so it must not be reused as the EVEX LL hint here -- LL has to
    ;; be derived from the actual register widths instead.
    (multiple-value-bind (ll r x b r-prime v-prime)
        (determine-evex-flags thing reg nil vvvv vm)
      (declare (ignorable r x b r-prime v-prime))
      (let ((effective-disp-n
             (if (zerop disp-n)
                 (or
                  (lookup-evex-disp-n prefix opcode opcode-prefix (or evex-w w)
                   ll)
                  0)
                 disp-n)))
        (return-from emit-avx2-inst
          (emit-avx512-inst segment thing reg prefix opcode :remaining-bytes
                            remaining-bytes :ll ll :opcode-prefix opcode-prefix
                            :w (or evex-w w) :vvvv vvvv :vm vm :disp-n
                            effective-disp-n)))))
  (emit-vex segment vvvv thing reg prefix opcode-prefix l w)
  (emit-bytes segment opcode)
  (when is4 (incf remaining-bytes))
  (emit-ea segment thing reg :remaining-bytes remaining-bytes :xmm-index vm)
  (when is4 (emit-byte segment (ash (reg-id-num (reg-id is4)) 4))))


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
                  :more-fields evex-more-fields :printer printer :opcode-prefix
                  opcode-prefix :w w :ll 2 :disp-n disp-n :nds nds
                  :reg-mem-size reg-mem-size :xmmreg-mem-size xmmreg-mem-size)
                 (loop for (ll n) in '((0 16) (1 32) (2 64))
                       appending (avx512-inst-printer-list inst-format-stem
                                  prefix opcode :more-fields evex-more-fields
                                  :printer printer :opcode-prefix opcode-prefix
                                  :w w :ll ll :disp-n n :nds nds :reg-mem-size
                                  reg-mem-size :xmmreg-mem-size
                                  xmmreg-mem-size))))))))))


(macrolet ((def (name opcode /i)
             `(define-instruction ,name (segment dst src imm)
               ,@(avx2-inst-printer-list 'ymm-ymm-imm 102 opcode :more-fields
                  `((/i ,/i)) :w 0)
               (:emitter
                (emit-avx2-inst-imm segment dst src imm 102 ,opcode ,/i)))))
  (def vpslldq 115 7)
  (def vpsrldq 115 3))


(macrolet ((def (name opcode vopcode /i &optional (evex-w 0))
             `(define-instruction ,name (segment dst src src2/imm)
               ,@(avx2-inst-printer-list 'ymm-ymm-imm 102 opcode :w evex-w
                  :more-fields `((/i ,/i)))
               ,@(avx2-inst-printer-list 'ymm-ymm/mem 102 vopcode :nds t :w
                  evex-w)
               (:emitter
                (if (integerp src2/imm)
                    (emit-avx2-inst-imm segment dst src src2/imm 102 ,opcode
                                        ,/i :evex-w ,evex-w :w ,evex-w)
                    (emit-avx2-inst segment src2/imm dst 102 ,vopcode :evex-w
                                    ,evex-w :w ,evex-w :vvvv src))))))
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
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix opcode :nds t
                  :opcode-prefix opcode-prefix :w evex-w :auto-evex auto-evex)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,opcode :opcode-prefix
                                ,opcode-prefix :evex-w ,evex-w :w ,evex-w :vvvv
                                src)))))
  (def vandpd 102 84 :opcode-prefix 15 :evex-w 1)
  (def vandps nil 84)
  (def vandnpd 102 85 :opcode-prefix 15 :evex-w 1)
  (def vandnps nil 85)
  (def vorpd 102 86 :opcode-prefix 15 :evex-w 1)
  (def vorps nil 86)
  (def vpand 102 219 :auto-evex nil)
  (def vpandn 102 223 :auto-evex nil)
  (def vpor 102 235 :auto-evex nil)
  (def vpxor 102 239 :auto-evex nil)
  (def vxorpd 102 87 :opcode-prefix 15 :evex-w 1)
  (def vxorps nil 87)
  (def vcomisd 102 47)
  (def vcomiss nil 47)
  (def vucomisd 102 46)
  (def vucomiss nil 46)
  (def vpcmpeqb 102 116)
  (def vpcmpeqw 102 117)
  (def vpcmpeqd 102 118)
  (def vpcmpgtb 102 100)
  (def vpcmpgtw 102 101)
  (def vpcmpgtd 102 102)
  (def vmaxpd 102 95 :opcode-prefix 15 :evex-w 1)
  (def vmaxps nil 95)
  (def vmaxsd 242 95)
  (def vmaxss 243 95)
  (def vminpd 102 93 :opcode-prefix 15 :evex-w 1)
  (def vminps nil 93)
  (def vminsd 242 93)
  (def vminss 243 93)
  (def vpmaxsw 102 238)
  (def vpmaxub 102 222)
  (def vpminsw 102 234)
  (def vpminub 102 218)
  (def vaddpd 102 88 :opcode-prefix 15 :evex-w 1)
  (def vaddps nil 88)
  (def vaddsd 242 88)
  (def vaddss 243 88)
  (def vaddsubpd 102 208)
  (def vaddsubps 242 208)
  (def vdivpd 102 94 :opcode-prefix 15 :evex-w 1)
  (def vdivps nil 94)
  (def vdivsd 242 94)
  (def vdivss 243 94)
  (def vhaddpd 102 124)
  (def vhaddps 242 124)
  (def vhsubpd 102 125)
  (def vhsubps 242 125)
  (def vmulpd 102 89 :opcode-prefix 15 :evex-w 1)
  (def vmulps nil 89)
  (def vmulsd 242 89)
  (def vmulss 243 89)
  (def vsubpd 102 92 :opcode-prefix 15 :evex-w 1)
  (def vsubps nil 92)
  (def vsubsd 242 92)
  (def vsubss 243 92)
  (def vunpckhpd 102 21 :opcode-prefix 15 :evex-w 1)
  (def vunpckhps nil 21)
  (def vunpcklpd 102 20 :opcode-prefix 15 :evex-w 1)
  (def vunpcklps nil 20)
  (def vpaddb 102 252)
  (def vpaddw 102 253)
  (def vpaddd 102 254)
  (def vpaddq 102 212 :opcode-prefix 15 :evex-w 1)
  (def vpaddsb 102 236)
  (def vpaddsw 102 237)
  (def vpaddusb 102 220)
  (def vpaddusw 102 221)
  (def vpavgb 102 224)
  (def vpavgw 102 227)
  (def vpmaddwd 102 245)
  (def vpmulhuw 102 228)
  (def vpmulhw 102 229)
  (def vpmullw 102 213)
  (def vpmuludq 102 244 :opcode-prefix 15 :evex-w 1)
  (def vpsadbw 102 246)
  (def vpsubb    #x66 #xf8)
  (def vpsubw    #x66 #xf9)
  (def vpsubd    #x66 #xfa)
  (def vpsubq    #x66 #xfb :opcode-prefix #x0F :evex-w 1)
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

  (def vpmuldq   #x66 #x28 :opcode-prefix #x0f38 :evex-w 1)
  (def vpcmpeqq  #x66 #x29 :opcode-prefix #x0f38 :evex-w 1)
  (def vpackusdw #x66 #x2b :opcode-prefix #x0f38)

  (def vpcmpgtq #x66 #x37 :opcode-prefix #x0f38 :evex-w 1)
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
                  :opcode-prefix opcode-prefix :w evex-w :more-fields
                  (case narrow
                    (:fixed-xmm '((reg/mem nil :printer #'print-xmmreg/mem)))
                    (:one-size-down
                     '((reg/mem nil :printer #'print-ymmreg/mem-one-size-down)))
                    (t (and (eq l :from-thing) '((reg nil :type 'xmmreg)))))
                  :auto-evex nil)
               ,@(loop for ll in '(0 1 2)
                       for n in disp-ns
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix opcode-prefix :w evex-w
                               :ll ll :disp-n n :more-fields
                               (case narrow
                                 (:fixed-xmm
                                  `((reg/mem nil :prefilter
                                     (lambda (dstate mod r/m)
                                       (decode-mod-r/m dstate mod r/m 'fpr
                                                       :disp-n ,n))
                                     :printer #'print-xmmreg/mem)))
                                 (:one-size-down
                                  `((reg/mem nil :prefilter
                                     (lambda (dstate mod r/m)
                                       (decode-mod-r/m dstate mod r/m 'fpr
                                                       :disp-n ,n))
                                     :printer
                                     #'print-ymmreg/mem-one-size-down)))
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
  (def vmovshdup 243 22 :disp-ns (16 32 64))
  (def vmovsldup 243 18 :disp-ns (16 32 64))
  (def vmovddup 242 18 :disp-ns (16 32 64))
  (def vrcpps nil 83 :disp-ns (16 32 64))
  (def vrsqrtps nil 82 :disp-ns (16 32 64))
  (def vsqrtpd 102 81 :evex-w 1 :disp-ns (16 32 64))
  (def vsqrtps nil 81 :disp-ns (16 32 64))
  (def vptest 102 23 :opcode-prefix 3896 :disp-ns (16 32 64))
  (def vpabsb 102 28 :opcode-prefix 3896 :disp-ns (16 32 64))
  (def vpabsw 102 29 :opcode-prefix 3896 :disp-ns (16 32 64))
  (def vpabsd 102 30 :opcode-prefix 3896 :disp-ns (16 32 64))
  (def vpmovsxbw 102 32 :opcode-prefix 3896 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vpmovsxbd 102 33 :opcode-prefix 3896 :disp-ns (4 8 16) :narrow
   :fixed-xmm)
  (def vpmovsxbq 102 34 :opcode-prefix 3896 :disp-ns (2 4 8) :narrow
   :fixed-xmm)
  (def vpmovsxwd 102 35 :opcode-prefix 3896 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vpmovsxwq 102 36 :opcode-prefix 3896 :disp-ns (4 8 16) :narrow
   :fixed-xmm)
  (def vpmovsxdq 102 37 :opcode-prefix 3896 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vpmovzxbw 102 48 :opcode-prefix 3896 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vpmovzxbd 102 49 :opcode-prefix 3896 :disp-ns (4 8 16) :narrow
   :fixed-xmm)
  (def vpmovzxbq 102 50 :opcode-prefix 3896 :disp-ns (2 4 8) :narrow
   :fixed-xmm)
  (def vpmovzxwd 102 51 :opcode-prefix 3896 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vpmovzxwq 102 52 :opcode-prefix 3896 :disp-ns (4 8 16) :narrow
   :fixed-xmm)
  (def vpmovzxdq 102 53 :opcode-prefix 3896 :disp-ns (8 16 32) :narrow
   :one-size-down)
  (def vcvtdq2pd 243 230 :disp-ns (8 16 32))
  (def vcvtdq2ps nil 91 :disp-ns (16 32 64))
  (def vcvtpd2dq 242 230 :l :from-thing :disp-ns (16 32 64))
  (def vcvtpd2ps 102 90 :l :from-thing :disp-ns (16 32 64))
  (def vcvtps2dq 102 91 :disp-ns (16 32 64))
  (def vcvtps2pd nil 90 :disp-ns (8 16 32))
  (def vcvttpd2dq 102 230 :l :from-thing :disp-ns (16 32 64)))


(macrolet ((def (name prefix &key (evex-w 0))
             `(define-instruction ,name (segment dst src pattern)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix 112 :printer
                  '(:name :tab reg ", " reg/mem ", " imm) :w evex-w)
               (:emitter
                (emit-avx2-inst segment src dst ,prefix 112 :remaining-bytes 1)
                (emit-byte segment pattern)))))
  (def vpshufd 102)
  (def vpshufhw 243)
  (def vpshuflw 242))


(macrolet ((def (name prefix &key (evex-w 0))
             `(define-instruction ,name (segment dst src src2 pattern)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix 198 :w evex-w)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix 198 :vvvv src
                                :remaining-bytes 1)
                (emit-byte segment pattern)))))
  (def vshufpd 102 :evex-w 1)
  (def vshufps nil))


(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src src2 imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                  :opcode-prefix 3898)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,opcode :opcode-prefix
                                3898 :vvvv src)
                (emit-byte segment imm))))
           (def-two (name prefix opcode &key (auto-evex t))
             `(define-instruction ,name (segment dst src imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                  :opcode-prefix 3898 :printer
                  '(:name :tab reg ", " reg/mem ", " imm) :auto-evex auto-evex)
               (:emitter
                (emit-avx2-inst segment src dst ,prefix ,opcode :opcode-prefix
                                3898)
                (emit-byte segment imm)))))
  (def-two vroundps 102 8)
  (def-two vroundpd 102 9)
  (def-two vroundss 102 10 :auto-evex nil)
  (def-two vroundsd 102 11 :auto-evex nil)
  (def vblendps 102 12)
  (def vblendpd 102 13)
  (def vpblendw 102 14)
  (def vpalignr 102 15)
  (def vdpps 102 64)
  (def vdppd 102 65)
  (def vmpsadbw 102 66)
  (def vpclmulqdq 102 68)
  (def-two vpcmpestrm 102 96)
  (def-two vpcmpestri 102 97)
  (def-two vpcmpistrm 102 98)
  (def-two vpcmpistri 102 99)
  (def-two vaeskeygenassist 102 223))

(macrolet ((def (name prefix opcode
                 name-suffix &key (evex-w 0) scalar)
             `(define-instruction ,name (segment condition dst src src2 &optional mask)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                                          :more-fields `((imm nil :type 'avx-condition-code))
                                          :printer `("VCMP" imm ,name-suffix
                                                            :tab reg ", " vvvv ", " reg/mem))
                ,@(loop for k from 0 to 7
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem-imm prefix opcode
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
  (def vcmpsd #xf2 #xc2 "SD" :evex-w 1 :scalar t)
  (def vcmpss #xf3 #xc2 "SS" :evex-w 0 :scalar t))

(macrolet ((def (name prefix op)
             `(define-instruction ,name (segment dst src src2 mask)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-ymm prefix op :w 0
                  :opcode-prefix 3898)
               (:emitter (aver (xmm-register-p dst))
                (aver (xmm-register-p mask))
                (emit-avx2-inst segment src2 dst ,prefix ,op :opcode-prefix
                                3898 :vvvv src :w 0 :is4 mask)))))
  (def vpblendvb 102 76)
  (def vblendvps 102 74)
  (def vblendvpd 102 75))


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
                     :opcode-prefix opcode-prefix :nds nds :w evex-w :auto-evex
                     auto-evex))
                ,@(when opcode-to
                    (avx2-inst-printer-list 'ymm-ymm/mem prefix opcode-to
                     :printer '(:name :tab reg/mem ", " reg) :opcode-prefix
                     opcode-prefix :w evex-w :auto-evex auto-evex))
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
                                   :opcode-prefix ,opcode-prefix :evex-w
                                   ,evex-w :w ,evex-w :disp-n
                                   (cond ((zmm-register-p src) 64)
                                         ((ymm-register-p src) 32)
                                         ((xmm-register-p src) 16) (t 0))
                                   :l ,l))))))))
  (def vmovapd 102 40 41 :evex-w 1)
  (def vmovaps nil 40 41)
  (def vmovdqa 102 111 127 :auto-evex nil)
  (def vmovdqu 243 111 127 :auto-evex nil)
  (def vmovupd 102 16 17 :evex-w 1)
  (def vmovups nil 16 17)
  (def vmovntdq 102 nil 231 :force-to-mem t)
  (def vmovntdqa 102 42 nil :force-to-mem t :opcode-prefix 3896)
  (def vmovntpd 102 nil 43 :force-to-mem t :evex-w 1)
  (def vmovntps nil nil 43 :force-to-mem t)
  (def vmovhpd 102 22 23 :force-to-mem t :l 0 :nds t)
  (def vmovhps nil 22 23 :reg-reg-name vmovlhps :l 0 :nds t)
  (def vmovlpd 102 18 19 :force-to-mem t :l 0 :nds t)
  (def vmovlps nil 18 19 :reg-reg-name vmovhlps :l 0 :nds t))


(define-instruction vlddqu (segment dst src)
 (:emitter (aver (xmm-register-p dst)) (aver (ea-p src))
  (emit-avx2-inst segment src dst 242 240 :opcode-prefix 15 :evex-w 0 :l nil))
 . #.(avx2-inst-printer-list 'ymm-ymm/mem 242 240))


(macrolet ((def (name prefix)
             `(define-instruction ,name (segment dst src &optional src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-dir prefix 8)
               (:emitter
                (cond
                 ((and (xmm-register-p dst) (ea-p src))
                  (emit-avx2-inst segment src dst ,prefix 16 :l 0))
                 ((xmm-register-p dst)
                  (emit-avx2-inst segment src2 dst ,prefix 16 :vvvv src :l 0))
                 (t (aver (xmm-register-p src))
                  (emit-avx2-inst segment dst src ,prefix 17 :l 0)))))))
  (def vmovsd 242)
  (def vmovss 243))


(flet ((move-ymm<->gpr (segment dst src w)
         (cond
          ((xmm-register-p dst)
           (emit-avx2-inst segment src dst 102 110 :l 0 :w w))
          (t (aver (xmm-register-p src))
           (emit-avx2-inst segment dst src 102 126 :l 0 :w w)))))
  (define-instruction vmovd (segment dst src)
   (:emitter (move-ymm<->gpr segment dst src 0))
   . #.(append
        (avx2-inst-printer-list 'ymm-ymm/mem 102 110 :more-fields
         '((reg/mem nil :type 'sized-reg/mem)) :w 0)
        (avx2-inst-printer-list 'ymm-ymm/mem 102 126 :more-fields
         '((reg/mem nil :type 'sized-reg/mem)) :printer
         '(:name :tab reg/mem ", " reg) :w 0)))
  (define-instruction vmovq (segment dst src)
   (:emitter
    (cond ((or (gpr-p src) (gpr-p dst)) (move-ymm<->gpr segment dst src 1))
          ((xmm-register-p dst) (emit-avx2-inst segment src dst 243 126 :l 0))
          (t (aver (xmm-register-p src))
           (emit-avx2-inst segment dst src 102 214 :l 0))))
   . #.(append
        (avx2-inst-printer-list 'ymm-ymm/mem 102 110 :w 1 :more-fields
         '((reg/mem nil :type 'sized-reg/mem-default-qword)))
        (avx2-inst-printer-list 'ymm-ymm/mem 102 126 :w 1 :more-fields
         '((reg/mem nil :type 'sized-reg/mem-default-qword)) :printer
         '(:name :tab reg/mem ", " reg))
        (avx2-inst-printer-list 'ymm-ymm/mem 243 126)
        (avx2-inst-printer-list 'ymm-ymm/mem 102 214 :printer
         '(:name :tab reg/mem ", " reg)))))


(macrolet ((def-insert (name prefix op size &key (op-prefix 3898) (w 0))
             `(define-instruction ,name (segment dst src src2 imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op :w w
                  :opcode-prefix op-prefix :reg-mem-size size :more-fields
                  `((imm nil :type 'imm-byte)) :printer
                  `(:name :tab reg ", " vvvv ", " reg/mem ", " imm) :evex t)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,op :opcode-prefix
                                ,op-prefix :vvvv src :w ,w :l 0
                                :remaining-bytes 1)
                (emit-byte segment imm))))
           (def-extract (name prefix op size &key (w 0))
             `(define-instruction ,name (segment dst src imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op :w w
                  :opcode-prefix 3898 :reg-mem-size size :more-fields
                  `((imm nil :type 'imm-byte)) :printer
                  `(:name :tab reg/mem ", " reg ", " imm) :evex t)
               (:emitter
                (aver (and (xmm-register-p src) (not (xmm-register-p dst))))
                (emit-avx2-inst segment dst src ,prefix ,op :w ,w :l 0
                                :opcode-prefix 3898 :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def-insert vpinsrb 102 32 :byte)
  (def-insert vpinsrq 102 34 :qword :w 1)
  (def-insert vpinsrw 102 196 :word :op-prefix 15)
  (def-insert vpinsrd 102 34 :dword)
  (def-insert vinsertps 102 33 nil)
  (def-extract vpextrb 102 20 :byte)
  (def-extract vpextrd 102 22 :dword)
  (def-extract vpextrq 102 22 :qword :w 1)
  (def-extract vextractps 102 23 nil))


(define-instruction vpextrw (segment dst src imm)
 (:emitter (aver (xmm-register-p src))
  (if (gpr-p dst)
      (emit-avx2-inst segment dst src 102 197 :l 0 :remaining-bytes 1)
      (emit-avx2-inst segment dst src 102 21 :opcode-prefix 3898 :l 0
                      :remaining-bytes 1))
  (emit-byte segment imm))
 . #.(append
      (avx2-inst-printer-list 'ymm-ymm/mem 102 21 :w 0 :opcode-prefix 3898
       :more-fields `((imm nil :type 'imm-byte)) :printer
       '(:name :tab reg/mem ", " reg ", " imm))
      (avx2-inst-printer-list 'ymm-ymm/mem 102 197 :w 0 :more-fields
       `((imm nil :type 'imm-byte)) :printer
       '(:name :tab reg/mem ", " reg ", " imm))))


(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix opcode
                  :reg-mem-size :sized :nds t)
               (:emitter (aver (xmm-register-p dst))
                (let ((src-size (operand-size src2)))
                  (emit-avx2-inst segment src2 dst ,prefix ,opcode :l 0 :vvvv
                                  src :w
                                  (case src-size
                                    (:qword 1)
                                    (:dword 0)
                                    (t 1))))))))
  (def vcvtsi2sd 242 42)
  (def vcvtsi2ss 243 42))


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
  (def vcvtsd2si 242 45)
  (def vcvtss2si 243 45)
  (def vcvttsd2si 242 44)
  (def vcvttss2si 243 44)
  (def vmovmskpd 102 80 :reg-only t)
  (def vmovmskps nil 80 :reg-only t)
  (def vpmovmskb 102 215 :reg-only t))


(define-instruction vzeroupper (segment)
 (:printer vex2-op ((op 119) (l 0) (r 1) (pp 0)))
 (:emitter (emit-two-byte-vex segment 0 0 0 nil) (emit-byte segment 119)))


(define-instruction vzeroall (segment)
 (:printer vex2-op ((op 119) (l 1) (r 1) (pp 0)))
 (:emitter (emit-two-byte-vex segment 0 0 1 nil) (emit-byte segment 119)))

(macrolet ((def-vbroadcast ()
             `(progn
                (define-instruction vbroadcastss (segment dst src)
                  ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x18
                                            :opcode-prefix #x0f38
                                            :xmmreg-mem-size :dword
                                            :w 0 :l nil)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x18
                                              :opcode-prefix #x0f38
                                              :w 0 :disp-n 4)
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
                                      :evex-w 0 :w 0 :l nil)))))

                (define-instruction vbroadcastsd (segment dst src)
                  ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x19
                                            :opcode-prefix #x0f38
                                            :xmmreg-mem-size :qword
                                            :w 0 :l 1)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x19
                                              :opcode-prefix #x0f38
                                              :w 1 :disp-n 8)
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
                                      :evex-w 1 :w 0 :l 1)))))

                (define-instruction vpbroadcastd (segment dst src)
                  ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x58
                                            :opcode-prefix #x0f38
                                            :xmmreg-mem-size :dword
                                            :w 0 :l nil)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x58
                                              :opcode-prefix #x0f38
                                              :w 0 :disp-n 4)
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
                                      :evex-w 0 :w 0 :l nil)))))

                (define-instruction vpbroadcastq (segment dst src)
                  ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x59
                                            :opcode-prefix #x0f38
                                            :xmmreg-mem-size :qword
                                            :w 0 :l nil)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x59
                                              :opcode-prefix #x0f38
                                              :w 1 :disp-n 8)
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
                                      :evex-w 1 :w 0 :l nil))))))))
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
                                 :evex-w ,evex-w :l ,l)))))
  (def vbroadcastf128 #x1a :l 1   :mem-size :qword :disp-n 16 :auto-evex nil)
  (def vbroadcasti128 #x5a :l 1   :mem-size :qword :disp-n 16 :auto-evex nil)
  (def vpbroadcastb   #x78 :l nil :mem-size :byte  :disp-n 1)
  (def vpbroadcastw   #x79 :l nil :mem-size :word  :disp-n 2))

(macrolet ((def-insert (name prefix op)
             `(define-instruction ,name (segment dst src src2 imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op :w 0 :l 1
                  :auto-evex nil :opcode-prefix 3898)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,op :opcode-prefix
                                3898 :vvvv src :w 0 :l 1 :remaining-bytes 1)
                (emit-byte segment imm))))
           (def-extract (name prefix op)
             `(define-instruction ,name (segment dst src imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op :w 0 :l 1
                  :auto-evex nil :opcode-prefix 3898 :printer
                  `(:name :tab reg/mem ", " reg ", " imm))
               (:emitter
                (emit-avx2-inst segment dst src ,prefix ,op :w 0 :l 1
                                :opcode-prefix 3898 :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def-insert vinsertf128 102 24)
  (def-insert vinserti128 102 56)
  (def-extract vextractf128 102 25)
  (def-extract vextracti128 102 57))


(macrolet ((def (name prefix op &optional l)
             `(define-instruction ,name (segment dst src src2 imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op :w 0 :l l
                  :opcode-prefix 3898)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,op :opcode-prefix
                                3898 :vvvv src :w 0 :l ,l :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vperm2f128 102 6 1)
  (def vperm2i128 102 70 1)
  (def vpblendd 102 2))


(macrolet ((def (name prefix op)
             `(define-instruction ,name (segment dst src imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op :w 1 :l 1
                  :opcode-prefix 3898 :printer
                  '(:name :tab reg ", " reg/mem ", " imm))
               (:emitter
                (emit-avx2-inst segment src dst ,prefix ,op :opcode-prefix 3898
                                :w 1 :l 1 :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vpermpd 102 1)
  (def vpermq 102 0))


(macrolet ((def (name op)
             `(define-instruction ,name (segment dst src src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem 102 op :w 0 :l 1 :nds t
                  :opcode-prefix 3896)
               (:emitter
                (emit-avx2-inst segment src2 dst 102 ,op :opcode-prefix 3896
                                :vvvv src :w 0 :l 1)))))
  (def vpermps 22)
  (def vpermd 54))


(macrolet ((def (name op op-imm)
             `(define-instruction ,name (segment dst src src2/imm)
               ,@(append
                  (avx2-inst-printer-list 'ymm-ymm/mem-imm 102 op-imm :w 0
                   :opcode-prefix 3898 :printer
                   '(:name :tab reg ", " reg/mem ", " imm))
                  (avx2-inst-printer-list 'ymm-ymm/mem 102 op :w 0 :nds t
                   :opcode-prefix 3896))
               (:emitter
                (cond
                 ((integerp src2/imm)
                  (emit-avx2-inst segment src dst 102 ,op-imm :opcode-prefix
                                  3898 :w 0 :remaining-bytes 1)
                  (emit-byte segment src2/imm))
                 (t
                  (emit-avx2-inst segment src2/imm dst 102 ,op :opcode-prefix
                                  3896 :vvvv src :w 0)))))))
  (def vpermilps 12 4)
  (def vpermilpd 13 5))


(macrolet ((def (name prefix op to-mem-op)
             `(define-instruction ,name (segment dst src src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op :w 0 :nds t
                  :auto-evex nil :opcode-prefix 3896)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix to-mem-op :w 0
                  :nds 'to-mem :auto-evex nil :opcode-prefix 3896)
               (:emitter
                (cond
                 ((xmm-register-p dst) (aver (ea-p src2))
                  (emit-avx2-inst segment src2 dst ,prefix ,op :opcode-prefix
                                  3896 :vvvv src :w 0))
                 (t (aver (ea-p dst))
                  (emit-avx2-inst segment dst src2 ,prefix ,to-mem-op
                                  :opcode-prefix 3896 :vvvv src :w 0)))))))
  (def vmaskmovps 102 44 46)
  (def vmaskmovpd 102 45 47))


(macrolet ((def (name prefix op w)
             `(define-instruction ,name (segment dst src src2)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op :w w :nds t
                  :opcode-prefix 3896)
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,op :opcode-prefix
                                3896 :vvvv src :w ,w)))))
  (def vpsrlvd 102 69 0)
  (def vpsrlvq 102 69 1)
  (def vpsravd 102 70 0)
  (def vpsllvd 102 71 0)
  (def vpsllvq 102 71 1))


(define-arg-type vmx/y :prefilter #'prefilter-reg/mem :printer #'print-vmx/y)


(define-arg-type vmx :prefilter #'prefilter-reg/mem :printer #'print-vmx)


(macrolet ((def (name op w sizing)
             `(define-instruction ,name (segment dst vm mask)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem 102 op :w w
                  :opcode-prefix 3896 :printer
                  '(:name :tab reg ", " reg/mem ", " vvvv) :more-fields
                  (ecase sizing
                    (xmm/ymm-vmx/y '((reg/mem nil :type 'vmx/y)))
                    (xmm-vmx/y
                     '((reg nil :type 'xmmreg) (reg/mem nil :type 'vmx/y)))
                    (xmm/ymm-vmx '((reg/mem nil :type 'vmx)))))
               (:emitter
                (emit-avx2-inst segment vm dst 102 ,op :opcode-prefix 3896
                                :vvvv mask :w ,w :l
                                ,(ecase sizing
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
               ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op :w w :l l :nds
                  t :opcode-prefix 3896 :auto-evex nil)
               ,@(cond
                  (disp-ns
                   (loop for ll in '(0 1 2)
                         for n in disp-ns
                         append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                                 op :w w :nds t :opcode-prefix 3896 :ll ll
                                 :disp-n n)))
                  (scalar-disp-n
                   (avx512-inst-printer-list 'ymm-ymm/mem prefix op :w w :nds t
                    :opcode-prefix 3896 :ll 0 :disp-n scalar-disp-n)))
               (:emitter
                (emit-avx2-inst segment src2 dst ,prefix ,op :opcode-prefix
                                3896 :vvvv src :w ,w :l ,l :disp-n
                                ,(cond
                                  (disp-ns
                                   `(cond
                                     ((zmm-register-p dst) (third ',disp-ns))
                                     ((ymm-register-p dst) (second ',disp-ns))
                                     ((xmm-register-p dst) (first ',disp-ns))
                                     (t 0)))
                                  (scalar-disp-n scalar-disp-n) (t 0)))))))
  (def-insert vfmadd132ps 102 152 :w 0 :disp-ns (16 32 64))
  (def-insert vfmadd213ps 102 168 :w 0 :disp-ns (16 32 64))
  (def-insert vfmadd231ps 102 184 :w 0 :disp-ns (16 32 64))
  (def-insert vfmadd132pd 102 152 :w 1 :disp-ns (16 32 64))
  (def-insert vfmadd213pd 102 168 :w 1 :disp-ns (16 32 64))
  (def-insert vfmadd231pd 102 184 :w 1 :disp-ns (16 32 64))
  (def-insert vfnmadd132ps 102 156 :w 0 :disp-ns (16 32 64))
  (def-insert vfnmadd213ps 102 172 :w 0 :disp-ns (16 32 64))
  (def-insert vfnmadd231ps 102 188 :w 0 :disp-ns (16 32 64))
  (def-insert vfnmadd132pd 102 156 :w 1 :disp-ns (16 32 64))
  (def-insert vfnmadd213pd 102 172 :w 1 :disp-ns (16 32 64))
  (def-insert vfnmadd231pd 102 188 :w 1 :disp-ns (16 32 64))
  (def-insert vfnmsub132ps 102 158 :w 0 :disp-ns (16 32 64))
  (def-insert vfnmsub213ps 102 174 :w 0 :disp-ns (16 32 64))
  (def-insert vfnmsub231ps 102 190 :w 0 :disp-ns (16 32 64))
  (def-insert vfnmsub132pd 102 158 :w 1 :disp-ns (16 32 64))
  (def-insert vfnmsub213pd 102 174 :w 1 :disp-ns (16 32 64))
  (def-insert vfnmsub231pd 102 190 :w 1 :disp-ns (16 32 64))
  (def-insert vfmaddsub132ps 102 150 :w 0 :disp-ns (16 32 64))
  (def-insert vfmaddsub213ps 102 166 :w 0 :disp-ns (16 32 64))
  (def-insert vfmaddsub231ps 102 182 :w 0 :disp-ns (16 32 64))
  (def-insert vfmaddsub132pd 102 150 :w 1 :disp-ns (16 32 64))
  (def-insert vfmaddsub213pd 102 166 :w 1 :disp-ns (16 32 64))
  (def-insert vfmaddsub231pd 102 182 :w 1 :disp-ns (16 32 64))
  (def-insert vfmsubadd132ps 102 151 :w 0 :disp-ns (16 32 64))
  (def-insert vfmsubadd213ps 102 167 :w 0 :disp-ns (16 32 64))
  (def-insert vfmsubadd231ps 102 183 :w 0 :disp-ns (16 32 64))
  (def-insert vfmsubadd132pd 102 151 :w 1 :disp-ns (16 32 64))
  (def-insert vfmsubadd213pd 102 167 :w 1 :disp-ns (16 32 64))
  (def-insert vfmsubadd231pd 102 183 :w 1 :disp-ns (16 32 64))
  (def-insert vfmsub132ps 102 154 :w 0 :disp-ns (16 32 64))
  (def-insert vfmsub213ps 102 170 :w 0 :disp-ns (16 32 64))
  (def-insert vfmsub231ps 102 186 :w 0 :disp-ns (16 32 64))
  (def-insert vfmsub132pd 102 154 :w 1 :disp-ns (16 32 64))
  (def-insert vfmsub213pd 102 170 :w 1 :disp-ns (16 32 64))
  (def-insert vfmsub231pd 102 186 :w 1 :disp-ns (16 32 64))
  (def-insert vfmadd132ss 102 153 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfmadd213ss 102 169 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfmadd231ss 102 185 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfmadd132sd 102 153 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfmadd213sd 102 169 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfmadd231sd 102 185 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfnmadd132ss 102 157 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfnmadd213ss 102 173 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfnmadd231ss 102 189 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfnmadd132sd 102 157 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfnmadd213sd 102 173 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfnmadd231sd 102 189 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfnmsub132ss 102 159 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfnmsub213ss 102 175 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfnmsub231ss 102 191 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfnmsub132sd 102 159 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfnmsub213sd 102 175 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfnmsub231sd 102 191 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfmsub132ss 102 155 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfmsub213ss 102 171 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfmsub231ss 102 187 :w 0 :l 0 :scalar-disp-n 4)
  (def-insert vfmsub132sd 102 155 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfmsub213sd 102 171 :w 1 :l 0 :scalar-disp-n 8)
  (def-insert vfmsub231sd 102 187 :w 1 :l 0 :scalar-disp-n 8))


(define-instruction vcvtph2ps (segment dst src)
 (:emitter
  (emit-avx2-inst segment src dst 102 19 :opcode-prefix 3896 :w 0 :disp-n
                  (cond ((zmm-register-p dst) 32) ((ymm-register-p dst) 16)
                        ((xmm-register-p dst) 8) (t 0))))
 . #.(append
      (avx2-inst-printer-list 'ymm-ymm/mem 102 19 :w 0 :opcode-prefix 3896
       :auto-evex nil)
      (loop for (ll n) in '((0 8) (1 16) (2 32))
            append (avx512-inst-printer-list 'ymm-ymm/mem 102 19 :w 0
                    :opcode-prefix 3896 :ll ll :disp-n n))))


(define-instruction vcvtps2ph (segment dst src imm)
 (:emitter
  (emit-avx2-inst segment dst src 102 29 :opcode-prefix 3898 :w 0 :disp-n
                  (cond ((zmm-register-p src) 32) ((ymm-register-p src) 16)
                        ((xmm-register-p src) 8) (t 0))
                  :remaining-bytes 1)
  (emit-byte segment imm))
 . #.(append
      (avx2-inst-printer-list 'ymm-ymm/mem 102 29 :w 0 :opcode-prefix 3898
       :auto-evex nil :printer '(:name :tab reg/mem ", " reg ", " imm))
      (loop for (ll n) in '((0 8) (1 16) (2 32))
            append (avx512-inst-printer-list 'ymm-ymm/mem 102 29 :w 0
                    :opcode-prefix 3898 :ll ll :disp-n n :printer
                    '(:name :tab reg/mem ", " reg ", " imm)))))


(define-instruction vgf2p8mulb (segment dst src1 src2)
 (:emitter
  (emit-avx2-inst segment src2 dst 102 207 :opcode-prefix 3896 :vvvv src1 :w 0
                  :disp-n
                  (cond ((zmm-register-p dst) 64) ((ymm-register-p dst) 32)
                        ((xmm-register-p dst) 16) (t 0))))
 . #.(append
      (avx2-inst-printer-list 'ymm-ymm/mem 102 207 :opcode-prefix 3896 :w 0
       :nds t :auto-evex nil)
      (loop for (ll n) in '((0 16) (1 32) (2 64))
            append (avx512-inst-printer-list 'ymm-ymm/mem 102 207
                    :opcode-prefix 3896 :w 0 :nds t :ll ll :disp-n n))))


(macrolet ((def (name opcode)
             `(define-instruction ,name (segment dst src1 src2 imm)
               ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm 102 opcode
                  :opcode-prefix 3898 :w 1 :auto-evex nil)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem-imm 102
                               opcode :opcode-prefix 3898 :w 1 :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx2-inst segment src2 dst 102 ,opcode :opcode-prefix
                                3898 :vvvv src1 :w 1 :disp-n
                                (cond ((zmm-register-p dst) 64)
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


(define-instruction-format
 (vex3-vex-gpr (+ 24 16) :include vex3 :default-printer
  '(:name :tab reg ", " vvvv ", " reg/mem))
 (op :field (byte 8 (+ 24 0))) (vvvv :type 'vvvv-reg)
 (reg/mem :fields (list (byte 2 (+ 24 14)) (byte 3 (+ 24 8))) :type 'reg/mem)
 (reg :field (byte 3 (+ 24 11)) :type 'reg))


(define-instruction mulx (segment &prefix prefix hi lo src)
 (:emitter
  (emit-avx2-inst segment src hi 242 246 :opcode-prefix 3896 :vvvv lo :w
                  (ecase (pick-operand-size prefix lo src)
                    (:qword 1)
                    (:dword 0))))
 . #.(avx2-inst-printer-list 'vex-gpr 242 246 :nds t :opcode-prefix 3896))


(define-instruction adcx (segment &prefix prefix dst src)
 (:printer ext-2byte-prefix-reg-reg/mem ((prefix 102) (op1 56) (op2 246)))
 (:emitter
  (let ((size (pick-operand-size prefix dst src)))
    (aver (memq size '(:dword :qword)))
    (emit-sse-inst-2byte segment dst src 102 56 246 :operand-size size))))


(define-instruction adox (segment &prefix prefix dst src)
 (:printer ext-2byte-prefix-reg-reg/mem ((prefix 243) (op1 56) (op2 246)))
 (:emitter
  (let ((size (pick-operand-size prefix dst src)))
    (aver (memq size '(:dword :qword)))
    (emit-sse-inst-2byte segment dst src 243 56 246 :operand-size size))))


(macrolet ((def (name prefix opcode third-op-name)
             `(define-instruction ,name
               (segment &prefix prefix dst src ,third-op-name)
               ,@(avx2-inst-printer-list 'vex-gpr prefix opcode :printer
                  '(:name :tab reg ", " reg/mem ", " vvvv) :nds t
                  :opcode-prefix 3896)
               (:emitter
                (emit-avx2-inst segment src dst ,prefix ,opcode :opcode-prefix
                                3896 :vvvv ,third-op-name :l 0 :w
                                (ecase (pick-operand-size prefix dst src)
                                  (:qword 1)
                                  (:dword 0)))))))
  (def shrx 242 247 count)
  (def shlx 102 247 count)
  (def sarx 243 247 count)
  (def bzhi nil 245 position)
  (def bextr nil 247 control))


(macrolet ((def (name prefix opcode &optional (third-op-name 'src2) printer)
             `(define-instruction ,name
               (segment &prefix prefix dst src ,third-op-name)
               ,@(avx2-inst-printer-list 'vex-gpr prefix opcode :nds t :printer
                  printer :opcode-prefix 3896)
               (:emitter
                (emit-avx2-inst segment ,third-op-name dst ,prefix ,opcode
                                :opcode-prefix 3896 :vvvv src :l 0 :w
                                (ecase (pick-operand-size prefix dst src)
                                  (:qword 1)
                                  (:dword 0)))))))
  (def pext 243 245 mask)
  (def pdep 242 245 mask)
  (def andn nil 242))


(define-instruction rorx (segment &prefix prefix dst src imm)
 (:emitter
  (emit-avx2-inst segment src dst 242 240 :opcode-prefix 3898 :l 0 :w
                  (ecase (pick-operand-size prefix dst src)
                    (:qword 1)
                    (:dword 0))
                  :remaining-bytes 1)
  (emit-byte segment imm))
 . #.(avx2-inst-printer-list 'vex-gpr 242 240 :opcode-prefix 3898 :printer
      '(:name :tab reg ", " reg/mem)))


(macrolet ((def (name reg)
             `(define-instruction ,name (segment &prefix prefix dst src)
               ,@(avx2-inst-printer-list 'vex-gpr nil 243 :opcode-prefix 3896
                  :more-fields `((reg ,reg)) :printer
                  '(:name :tab vvvv ", " reg/mem))
               (:emitter
                (emit-avx2-inst segment src ,reg nil 243 :opcode-prefix 3896
                                :vvvv dst :l 0 :w
                                (ecase (pick-operand-size prefix dst src)
                                  (:qword 1)
                                  (:dword 0)))))))
  (def blsr 1)
  (def blsmsk 2)
  (def blsi 3))

