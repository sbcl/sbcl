(in-package "SB-X86-64-ASM")

(define-arg-type ymmreg
  :prefilter #'prefilter-reg-r
  :printer #'print-ymmreg)

(define-arg-type ymmreg-b
  :prefilter #'prefilter-reg-b
  :printer #'print-ymmreg)

(define-arg-type ymm-vvvv-reg
  :prefilter #'invert-4
  :printer #'print-ymmreg)

(define-arg-type ymm-reg-is4
  :printer #'print-ymmreg)

(define-arg-type ymmreg/mem
  :prefilter #'prefilter-xmmreg/mem
  :printer #'print-ymmreg/mem)

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

(defconstant +vex-l+ #b10000000000)

(define-arg-type vex-l
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (if (plusp value) +vex-l+ 0))))

(define-arg-type vex-w
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (if (plusp value) +rex-w+ 0))))

(define-arg-type vex-r
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (if (plusp value) 0 +rex-r+))))

(define-arg-type vex-x
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (if (plusp value) 0 +rex-x+))))

(define-arg-type vex-b
  :prefilter  (lambda (dstate value)
                (dstate-setprop dstate (if (plusp value) 0 +rex-b+))))

(defconstant-eqx +avx-conditions+
    #(:eq :lt :le :unord :neq :nlt :nle :ord :eq_uq
      :nge :ngt :false :neq_oq :ge :gt :true :eq_os :lt_oq
      :le_oq :unord_s :neq_us :nlt_uq :nle_uq :ord_s :eq_us
      :nge_uq :ngt_uq :false_os :neq_os :ge_oq :gt_oq :true_us)
  #'equalp)

(define-arg-type avx-condition-code
  :type 'imm-byte
  :printer +avx-conditions+)


(define-instruction-format (vex2 16)
                           (vex :field (byte 8 0) :value #xC5)
                           (r :field (byte 1 (+ 8 7)) :type 'vex-r)
                           (vvvv :field (byte 4 (+ 8 3)) :type 'ymm-vvvv-reg)
                           (l  :field (byte 1 (+ 8 2)) :type 'vex-l)
                           (pp :field (byte 2 (+ 8 0))))

(define-instruction-format (vex3 24)
                           (vex :field (byte 8 0) :value #xC4)
                           (r :field (byte 1 (+ 8 7)) :type 'vex-r)
                           (x :field (byte 1 (+ 8 6)) :type 'vex-x)
                           (b :field (byte 1 (+ 8 5)) :type 'vex-b)
                           (m-mmmm :field (byte 5 (+ 8 0)))
                           (w :field (byte 1 (+ 16 7)) :type 'vex-w)
                           (vvvv :field (byte 4 (+ 16 3)) :type 'ymm-vvvv-reg)
                           (l  :field (byte 1 (+ 16 2)) :type 'vex-l)
                           (pp :field (byte 2 (+ 16 0))))

(define-instruction-format (vex2-op 24
                            :include vex2
                            :default-printer '(:name))
  (op :field (byte 8 16)))

(defmacro define-vex-instruction-format ((format-name length-in-bits
                                          &key default-printer include)
                                         &body arg-specs)
  `
  (progn
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

(define-vex-instruction-format (ymm-ymm/mem 16
                                :default-printer
                                '(:name :tab reg ", " reg/mem))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'ymmreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg)
  ;; optional fields
  (imm))

(define-vex-instruction-format (ymm-ymm/mem-imm 16
                                :default-printer
                                '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'ymmreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg)
  (imm :type 'imm-byte))

(define-vex-instruction-format (ymm-ymm/mem-ymm 24
                                :default-printer
                                '(:name :tab reg  ", " vvvv ", " reg/mem ", " reg4))
  (op      :field (byte 8 (+ start 0)))
  (reg/mem :fields (list (byte 2 (+ start 14)) (byte 3 (+ start 8)))
           :type 'ymmreg/mem)
  (reg     :field (byte 3 (+ start 11))
           :type 'ymmreg)
  (reg4    :field (byte 4 (+ start 16 4))
           :type 'ymm-reg-is4))

;;; Same as ymm-ymm/mem etc., but with a direction bit.
(define-vex-instruction-format (ymm-ymm/mem-dir 16
                                :include ymm-ymm/mem
                                :default-printer
                                `(:name
                                  :tab
                                  (:if (reg/mem :test machine-ea-p)
                                       (:if (dir :constant 0)
                                            (reg ", " reg/mem)
                                            (reg/mem ", " reg))
                                       (reg ", " vvvv ", " reg/mem))))
  (op  :field (byte 7 (+ start 1)))
  (dir :field (byte 1 (+ start 0))))

(define-vex-instruction-format (ymm-ymm-imm 16
                                :default-printer
                                '(:name :tab vvvv ", " reg ", " imm))
  (op      :field (byte 8 (+ start 0)))
  (/i :field (byte 3 (+ start 11)))
  (b11 :field (byte 2 (+ start 14)) :value #b11)
  (reg :field (byte 3 (+ start 8)) :type 'ymmreg-b)
  (imm :type 'imm-byte))

(define-vex-instruction-format (reg-ymm/mem 16
                                :include ymm-ymm/mem
                                :default-printer
                                '(:name :tab reg ", " reg/mem))
  (reg :field (byte 3 (+ start 11))
       :type 'reg))

(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun vex-encode-pp (pp)
    (ecase pp
      ((nil) 0)
      (#x66 #b01)
      (#xF3 #b10)
      (#xF2 #b11)))

  (defun vex-encode-m-mmmm (m-mmmm)
    (ecase m-mmmm
      (#x0F #b00001)
      (#x0F38 #b00010)
      (#x0F3A #b00011))))

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
    (let ((l (cond (l)
                   ((xmm-register-p reg)
                    (xmm-size reg))
                   ((xmm-register-p thing)
                    (xmm-size thing))
                   (t
                    0)))
          (r (if (null reg)
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
                   (0)))
          (b
            (reg-7-p
             (cond ((ea-p thing)
                    (let ((base (ea-base thing)))
                      (if (and base (neq base rip-tn))
                          (reg-id (tn-reg base))
                          0)))
                   ((register-p thing)
                    (reg-id thing))
                   (0)))))
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

(defun emit-avx2-inst (segment thing reg prefix opcode
                       &key (remaining-bytes 0)
                            l
                            (opcode-prefix #x0F)
                            (w 0)
                            vvvv
                            is4
                            vm)
  (emit-vex segment vvvv thing reg prefix opcode-prefix l w)
  (emit-bytes segment opcode)
  (when is4
    (incf remaining-bytes))
  ;; FIXME: :xmm-index should be removed and we should alter the EA
  ;; to have the proper FPR as the index reg when appropriate.
  (emit-ea segment thing reg :remaining-bytes remaining-bytes :xmm-index vm)
  (when is4
    (emit-byte segment (ash (reg-id-num (reg-id is4)) 4))))

(defun emit-avx2-inst-imm (segment thing reg imm prefix opcode /i
                           &key l
                                (w 0)
                                (opcode-prefix #x0F))
  (aver (<= 0 /i 7))
  (emit-vex segment thing reg nil prefix opcode-prefix l w)
  (emit-bytes segment opcode)
  (emit-byte segment (logior (ash (logior #b11000 /i) 3)
                             (reg-encoding reg segment)))
  (emit-byte segment imm))


(eval-when (#-sb-xc :compile-toplevel :load-toplevel :execute)
  (defun avx2-inst-printer-list (inst-format-stem prefix opcode
                                 &key more-fields printer
                                      (opcode-prefix #x0F)
                                      reg-mem-size
                                      xmmreg-mem-size
                                      w
                                      l
                                      nds)
    (let ((fields `((pp ,(vex-encode-pp prefix))
                    (m-mmmm ,(vex-encode-m-mmmm opcode-prefix))
                    (op ,opcode)
                    ,@(and w `((w ,w)))
                    ,@(and l `((l ,l)))
                    ,@(cond (xmmreg-mem-size
                             `((reg/mem nil :type ',(case xmmreg-mem-size
                                                      (:qword 'sized-xmmreg/mem-default-qword)
                                                      (:dword 'sized-dword-xmmreg/mem)
                                                      (:word 'sized-word-xmmreg/mem)
                                                      (:byte 'sized-byte-xmmreg/mem)
                                                      (:sized 'sized-xmmreg/mem)))))
                            (reg-mem-size
                             `((reg/mem nil :type ',(case reg-mem-size
                                                      (:qword 'sized-reg/mem-default-qword)
                                                      (:dword 'sized-dword-reg/mem)
                                                      (:word 'sized-word-reg/mem)
                                                      (:byte 'sized-byte-reg/mem)
                                                      (:sized 'sized-reg/mem))))))
                    ,@more-fields))
          (inst-formats (if (or (eql w 1)
                                (/= opcode-prefix #x0F))
                            (list (symbolicate "VEX3-" inst-format-stem))
                            (list (symbolicate "VEX2-" inst-format-stem)
                                  (symbolicate "VEX3-" inst-format-stem)))))
      (mapcar (lambda (inst-format)
                `(:printer ,inst-format ,fields
                           ,@(cond (printer
                                    `(',printer))
                                   ((eq nds 'to-mem)
                                    `('(:name :tab reg/mem ", " vvvv ", " reg)))
                                   (nds
                                    `('(:name :tab reg ", " vvvv ", " reg/mem))))))
              inst-formats))))

(macrolet
    ((def (name opcode /i)
       `(define-instruction ,name (segment dst src imm)
          ,@(avx2-inst-printer-list 'ymm-ymm-imm #x66 opcode
                                    :more-fields `((/i ,/i)))
          (:emitter
           (emit-avx2-inst-imm segment dst src imm
                               #x66 ,opcode ,/i)))))
  (def vpslldq #x73 7)
  (def vpsllw-imm #x71 6)               ; FIXME: get rid of that -IMM
  (def vpslld-imm #x72 6)
  (def vpsllq-imm #x73 6)

  (def vpsraw-imm #x71 4)
  (def vpsrad-imm #x72 4)

  (def vpsrldq #x73 3)
  (def vpsrlw-imm #x71 2)
  (def vpsrld-imm #x72 2)
  (def vpsrlq-imm #x73 2))

(macrolet ((def (name prefix opcode &optional (opcode-prefix #x0F))
             `(define-instruction ,name (segment dst src src2)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix opcode :nds t
                                          :opcode-prefix opcode-prefix)
                (:emitter
                 (emit-avx2-inst segment src2 dst ,prefix ,opcode
                                 :opcode-prefix ,opcode-prefix
                                 :vvvv src)))))
  ;; logical
  (def vandpd    #x66 #x54)
  (def vandps    nil  #x54)
  (def vandnpd   #x66 #x55)
  (def vandnps   nil  #x55)
  (def vorpd     #x66 #x56)
  (def vorps     nil  #x56)
  (def vpand     #x66 #xdb)
  (def vpandn    #x66 #xdf)
  (def vpor      #x66 #xeb)
  (def vpxor     #x66 #xef)
  (def vxorpd    #x66 #x57)
  (def vxorps    nil  #x57)
  ;; comparison
  (def vcomisd   #x66 #x2f)
  (def vcomiss   nil  #x2f)
  (def vucomisd  #x66 #x2e)
  (def vucomiss  nil  #x2e)
  ;; integer comparison
  (def vpcmpeqb  #x66 #x74)
  (def vpcmpeqw  #x66 #x75)
  (def vpcmpeqd  #x66 #x76)
  (def vpcmpgtb  #x66 #x64)
  (def vpcmpgtw  #x66 #x65)
  (def vpcmpgtd  #x66 #x66)
  ;; max/min
  (def vmaxpd    #x66 #x5f)
  (def vmaxps    nil  #x5f)
  (def vmaxsd    #xf2 #x5f)
  (def vmaxss    #xf3 #x5f)
  (def vminpd    #x66 #x5d)
  (def vminps    nil  #x5d)
  (def vminsd    #xf2 #x5d)
  (def vminss    #xf3 #x5d)
  ;; integer max/min
  (def vpmaxsw   #x66 #xee)
  (def vpmaxub   #x66 #xde)
  (def vpminsw   #x66 #xea)
  (def vpminub   #x66 #xda)
  ;; arithmetic
  (def vaddpd    #x66 #x58)
  (def vaddps    nil  #x58)
  (def vaddsd    #xf2 #x58)
  (def vaddss    #xf3 #x58)
  (def vaddsubpd #x66 #xd0)
  (def vaddsubps #xf2 #xd0)
  (def vdivpd    #x66 #x5e)
  (def vdivps    nil  #x5e)
  (def vdivsd    #xf2 #x5e)
  (def vdivss    #xf3 #x5e)
  (def vhaddpd   #x66 #x7c)
  (def vhaddps   #xf2 #x7c)
  (def vhsubpd   #x66 #x7d)
  (def vhsubps   #xf2 #x7d)
  (def vmulpd    #x66 #x59)
  (def vmulps    nil  #x59)
  (def vmulsd    #xf2 #x59)
  (def vmulss    #xf3 #x59)

  (def vsubpd    #x66 #x5c)
  (def vsubps    nil  #x5c)
  (def vsubsd    #xf2 #x5c)
  (def vsubss    #xf3 #x5c)
  (def vunpckhpd #x66 #x15)
  (def vunpckhps nil  #x15)
  (def vunpcklpd #x66 #x14)
  (def vunpcklps nil  #x14)
  ;; integer arithmetic
  (def vpaddb    #x66 #xfc)
  (def vpaddw    #x66 #xfd)
  (def vpaddd    #x66 #xfe)
  (def vpaddq    #x66 #xd4)
  (def vpaddsb   #x66 #xec)
  (def vpaddsw   #x66 #xed)
  (def vpaddusb  #x66 #xdc)
  (def vpaddusw  #x66 #xdd)
  (def vpavgb    #x66 #xe0)
  (def vpavgw    #x66 #xe3)
  (def vpmaddwd  #x66 #xf5)
  (def vpmulhuw  #x66 #xe4)
  (def vpmulhw   #x66 #xe5)
  (def vpmullw   #x66 #xd5)
  (def vpmuludq  #x66 #xf4)
  (def vpsadbw   #x66 #xf6)
  (def vpsllw    #x66 #xf1)
  (def vpslld    #x66 #xf2)
  (def vpsllq    #x66 #xf3)
  (def vpsraw    #x66 #xe1)
  (def vpsrad    #x66 #xe2)
  (def vpsrlw    #x66 #xd1)
  (def vpsrld    #x66 #xd2)
  (def vpsrlq    #x66 #xd3)
  (def vpsubb    #x66 #xf8)
  (def vpsubw    #x66 #xf9)
  (def vpsubd    #x66 #xfa)
  (def vpsubq    #x66 #xfb)
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

  (def vpshufb #x66 #x00 #x0f38)
  (def vphaddw #x66 #x01 #x0f38)
  (def vphaddd #x66 #x02 #x0f38)
  (def vphaddsw #x66 #x03 #x0f38)
  (def vpmaddubsw #x66 #x04 #x0f38)
  (def vphsubw #x66 #x05 #x0f38)
  (def vphsubd #x66 #x06 #x0f38)
  (def vphsubsw #x66 #x07 #x0f38)
  (def vpsignb #x66 #x08 #x0f38)
  (def vpsignw #x66 #x09 #x0f38)
  (def vpsignd #x66 #x0a #x0f38)
  (def vpmulhrsw #x66 #x0b #x0f38)

  (def vpmuldq #x66 #x28 #x0f38)
  (def vpcmpeqq #x66 #x29 #x0f38)
  (def vpackusdw #x66 #x2b #x0f38)

  (def vpcmpgtq #x66 #x37 #x0f38)
  (def vpminsb #x66 #x38 #x0f38)
  (def vpminsd #x66 #x39 #x0f38)
  (def vpminuw #x66 #x3a #x0f38)
  (def vpminud #x66 #x3b #x0f38)
  (def vpmaxsb #x66 #x3c #x0f38)
  (def vpmaxsd #x66 #x3d #x0f38)
  (def vpmaxuw #x66 #x3e #x0f38)
  (def vpmaxud #x66 #x3f #x0f38)

  (def vpmulld #x66 #x40 #x0f38)
  (def vphminposuw #x66 #x41 #x0f38)

  (def vaesenc #x66 #xdc #x0f38)
  (def vaesenclast #x66 #xdd #x0f38)
  (def vaesdec #x66 #xde #x0f38)
  (def vaesdeclast #x66 #xdf #x0f38))

;;; Two arg instructions
(macrolet ((def (name prefix opcode &optional (opcode-prefix #x0F))
             `(define-instruction ,name (segment dst src)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix opcode
                                          :opcode-prefix opcode-prefix)
                (:emitter
                 (emit-avx2-inst segment src dst ,prefix ,opcode
                                 :opcode-prefix ,opcode-prefix)))))
  ;; moves
  (def vmovshdup #xf3 #x16)
  (def vmovsldup #xf3 #x12)
  (def vmovddup  #xf2 #x12)

  (def vrcpps    nil  #x53)
  (def vrcpss    #xf3 #x53)
  (def vrsqrtps  nil  #x52)
  (def vrsqrtss  #xf3 #x52)
  (def vsqrtpd   #x66 #x51)
  (def vsqrtps   nil  #x51)
  (def vsqrtsd   #xf2 #x51)
  (def vsqrtss   #xf3 #x51)
  ;; conversion
  (def vcvtdq2pd #xf3 #xe6)
  (def vcvtdq2ps nil  #x5b)
  (def vcvtpd2dq #xf2 #xe6)
  (def vcvtpd2ps #x66 #x5a)
  (def vcvtps2dq #x66 #x5b)
  (def vcvtps2pd nil  #x5a)
  (def vcvtsd2ss #xf2 #x5a)
  (def vcvtss2sd #xf3 #x5a)
  (def vcvttpd2dq #x66 #xe6)
  (def vcvttps2dq #xf3 #x5b)

  (def vptest #x66 #x17 #x0f38)
  (def vpabsb #x66 #x1c #x0f38)
  (def vpabsw #x66 #x1d #x0f38)
  (def vpabsd #x66 #x1e #x0f38)

  (def vaesimc #x66 #xdb #x0f38)

  (def vpmovsxbw #x66 #x20 #x0f38)
  (def vpmovsxbd #x66 #x21 #x0f38)
  (def vpmovsxbq #x66 #x22 #x0f38)
  (def vpmovsxwd #x66 #x23 #x0f38)
  (def vpmovsxwq #x66 #x24 #x0f38)
  (def vpmovsxdq #x66 #x25 #x0f38)

  (def vpmovzxbw #x66 #x30 #x0f38)
  (def vpmovzxbd #x66 #x31 #x0f38)
  (def vpmovzxbq #x66 #x32 #x0f38)
  (def vpmovzxwd #x66 #x33 #x0f38)
  (def vpmovzxwq #x66 #x34 #x0f38)
  (def vpmovzxdq #x66 #x35 #x0f38))

(macrolet ((def (name prefix)
             `(define-instruction ,name (segment dst src pattern)
                ,@(avx2-inst-printer-list
                   'ymm-ymm/mem-imm prefix #x70
                   :printer '(:name :tab reg ", " reg/mem ", " imm))
                (:emitter
                 (emit-avx2-inst segment dst src ,prefix #x70
                                 :remaining-bytes 1)
                 (emit-byte segment pattern)))))
  (def vpshufd  #x66)
  (def vpshufhw #xf3)
  (def vpshuflw #xf2))

(macrolet ((def (name prefix)
             `(define-instruction ,name (segment dst src src2 pattern)
                ,@(avx2-inst-printer-list
                   'ymm-ymm/mem-imm prefix #xc6)
                (:emitter
                 (emit-avx2-inst segment src2 dst ,prefix #xc6
                                 :vvvv src
                                 :remaining-bytes 1)
                 (emit-byte segment pattern)))))
  (def vshufpd #x66)
  (def vshufps nil))

(macrolet
    ((def (name prefix opcode)
       `(define-instruction ,name (segment dst src src2 imm)
          ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix opcode :opcode-prefix #x0f3a)
          (:emitter
           (emit-avx2-inst segment src2 dst ,prefix ,opcode
                           :opcode-prefix #x0f3a
                           :vvvv src)
           (emit-byte segment imm))))
     (def-two (name prefix opcode)
       `(define-instruction ,name (segment dst src imm)
          ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix opcode :opcode-prefix #x0f3a
                                    :printer '(:name :tab reg ", " reg/mem ", " imm))
          (:emitter
           (emit-avx2-inst segment src dst ,prefix ,opcode
                           :opcode-prefix #x0f3a)
           (emit-byte segment imm)))))
  (def-two vroundps #x66 #x08)
  (def-two vroundpd #x66 #x09)
  (def-two vroundss #x66 #x0a)
  (def-two vroundsd #x66 #x0b)
  (def vblendps #x66 #x0c)
  (def vblendpd #x66 #x0d)
  (def vpblendw #x66 #x0e)
  (def vpalignr #x66 #x0f)
  (def vdpps    #x66 #x40)
  (def vdppd    #x66 #x41)

  (def vmpsadbw #x66 #x42)
  (def vpclmulqdq #x66 #x44)

  (def-two vpcmpestrm #x66 #x60)
  (def-two vpcmpestri #x66 #x61)
  (def-two vpcmpistrm #x66 #x62)
  (def-two vpcmpistri #x66 #x63)

  (def-two vaeskeygenassist #x66 #xdf))

(macrolet ((def (name prefix opcode
                 name-suffix)
             `(define-instruction ,name (segment condition dst src src2)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                                          :more-fields `((imm nil :type 'avx-condition-code))
                                          :printer `("VCMP" imm ,name-suffix
                                                            :tab reg ", " vvvv ", " reg/mem))
                (:emitter
                 (emit-avx2-inst segment src2 dst ,prefix ,opcode
                                 :vvvv src)
                 (emit-byte segment (or (position condition +avx-conditions+)
                                        (error "~s not one of ~s"
                                               condition
                                               +avx-conditions+)))))))
  (def vcmppd #x66 #xc2 "PD")
  (def vcmpps nil  #xc2 "PS")
  (def vcmpsd #xf2 #xc2 "SD")
  (def vcmpss #xf3 #xc2 "SS"))

(macrolet ((def (name prefix op)
             `(define-instruction ,name (segment dst src src2 mask)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-ymm prefix op :w 0
                                                                     :opcode-prefix #x0f3a)
                (:emitter
                 (aver (xmm-register-p dst))
                 (aver (xmm-register-p mask))
                 (emit-avx2-inst segment src2 dst ,prefix ,op
                                 :opcode-prefix #x0f3a
                                 :vvvv src
                                 :w 0
                                 :is4 mask)))))

  (def vpblendvb #x66 #x4C)
  (def vblendvps #x66 #x4A)
  (def vblendvpd #x66 #x4B))

(macrolet ((def (name prefix opcode-from opcode-to
                                 &key force-to-mem
                                      reg-reg-name
                                      l
                                      (opcode-prefix #x0F)
                                      nds)
             `(progn
                ,(when reg-reg-name
                   `(define-instruction ,reg-reg-name (segment dst src ,@(if nds '(src2)))
                      (:emitter
                       (aver (xmm-register-p dst))
                       (aver (xmm-register-p src))
                       (emit-avx2-inst segment dst
                                       ,(if nds
                                            'src2
                                            'src)
                                       ,prefix ,opcode-from
                                       :opcode-prefix ,opcode-prefix
                                       :l ,l
                                       ,@(and nds
                                              `(:vvvv src))))))
                (define-instruction ,name (segment dst src ,@(if nds '(&optional src2)))
                  ,@(when opcode-from
                      (avx2-inst-printer-list 'ymm-ymm/mem prefix opcode-from
                                              :opcode-prefix opcode-prefix
                                              :nds nds))
                  ,@(when opcode-to
                      (avx2-inst-printer-list
                       'ymm-ymm/mem prefix opcode-to
                       :printer '(:name :tab reg/mem ", " reg)
                       :opcode-prefix opcode-prefix))
                  (:emitter
                   ,@(when nds
                       `((aver (register-p src))))
                   (cond ,@(when opcode-from
                             `(((xmm-register-p dst)
                                ,(when force-to-mem
                                   `(aver (not (register-p ,(if nds
                                                                'src2
                                                                'src)))))
                                (emit-avx2-inst segment ,(if nds
                                                             'src2
                                                             'src)
                                                dst
                                                ,prefix ,opcode-from
                                                :opcode-prefix ,opcode-prefix
                                                ,@(and nds
                                                       `(:vvvv src))
                                                :l ,l))))
                         (t
                          (aver (xmm-register-p src))
                          ,(when force-to-mem
                             `(aver (not (register-p dst))))
                          (emit-avx2-inst segment
                                          dst src
                                          ,prefix ,opcode-to
                                          :opcode-prefix ,opcode-prefix
                                          :l ,l))))))))
  ;; direction bit?
  (def vmovapd #x66 #x28 #x29)
  (def vmovaps nil  #x28 #x29)
  (def vmovdqa #x66 #x6f #x7f)
  (def vmovdqu #xf3 #x6f #x7f)
  (def vmovupd #x66 #x10 #x11)
  (def vmovups nil  #x10 #x11)

  ;; streaming
  (def vmovntdq #x66 nil #xe7 :force-to-mem t)
  (def vmovntdqa #x66 #x2a nil :force-to-mem t :opcode-prefix #x0F38)
  (def vmovntpd #x66 nil #x2b :force-to-mem t)
  (def vmovntps nil  nil #x2b :force-to-mem t)

  ;; use vmovhps for vmovlhps and vmovlps for vmovhlps
  (def vmovhpd #x66 #x16 #x17 :force-to-mem t :l 0 :nds t)
  (def vmovhps nil  #x16 #x17 :reg-reg-name vmovlhps :l 0 :nds t)
  (def vmovlpd #x66 #x12 #x13 :force-to-mem t :l 0 :nds t)
  (def vmovlps nil  #x12 #x13 :reg-reg-name vmovhlps :l 0 :nds t))

(macrolet ((def (name prefix)
             `(define-instruction ,name (segment dst src &optional src2)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-dir prefix #b0001000)
                (:emitter
                 (cond ((and (xmm-register-p dst)
                             (ea-p src))
                        (emit-avx2-inst segment src dst ,prefix #x10 :l 0))
                       ((xmm-register-p dst)
                        (emit-avx2-inst segment src2 dst ,prefix #x10 :vvvv src :l 0))
                       (t
                        (aver (xmm-register-p src))
                        (emit-avx2-inst segment dst src ,prefix #x11 :l 0)))))))
  (def vmovsd #xf2)
  (def vmovss #xf3))

(flet ((move-ymm<->gpr (segment dst src w)
         (cond ((xmm-register-p dst)
                (emit-avx2-inst segment src dst #x66 #x6e :l 0 :w w))
               (t
                (aver (xmm-register-p src))
                (emit-avx2-inst segment dst src #x66 #x7e :l 0 :w w)))))
  (define-instruction vmovd (segment dst src)
    (:emitter (move-ymm<->gpr segment dst src 0))
    . #.(append (avx2-inst-printer-list 'ymm-ymm/mem #x66 #x6e
                                        :more-fields '((reg/mem nil :type 'sized-reg/mem))
                                        :w 0)
                (avx2-inst-printer-list 'ymm-ymm/mem #x66 #x7e
                                        :more-fields '((reg/mem nil :type 'sized-reg/mem))
                                        :printer '(:name :tab reg/mem ", " reg)
                                        :w 0)))
  (define-instruction vmovq (segment dst src)
    (:emitter
     (cond ((or (gpr-p src) (gpr-p dst))
            (move-ymm<->gpr segment dst src 1))
           (t
            (cond ((xmm-register-p dst)
                   (emit-avx2-inst segment dst src #xf3 #x7e :l 0))
                  (t
                   (aver (xmm-register-p src))
                   (emit-avx2-inst segment src dst #x66 #xd6 :l 0))))))
    . #.(append (avx2-inst-printer-list 'ymm-ymm/mem #x66 #x6e
                                        :w 1
                                        :more-fields '((reg/mem nil :type 'sized-reg/mem-default-qword)))
                (avx2-inst-printer-list 'ymm-ymm/mem #x66 #x7e
                                        :w 1
                                        :more-fields '((reg/mem nil :type 'sized-reg/mem-default-qword))
                                        :printer '(:name :tab reg/mem ", " reg))
                (avx2-inst-printer-list 'ymm-ymm/mem #xf3 #x7e)
                (avx2-inst-printer-list 'ymm-ymm/mem #x66 #xd6
                                        :printer '(:name :tab reg/mem ", " reg)))))

(macrolet ((def-insert (name prefix op size &key (op-prefix #x0f3a)
                                                                (w 0))
             `(define-instruction ,name (segment dst src src2 imm)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op
                                          :w w
                                          :opcode-prefix op-prefix
                                          :reg-mem-size size ;; FIXME: it has r32/m8, but we print as r8/m8
                                          :more-fields `((imm nil :type 'imm-byte))
                                          :printer `(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
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
                                           :opcode-prefix #x0f3a
                                           :reg-mem-size size ;; FIXME: it has r32/m8, but we print as r8/m8
                                           :more-fields `((imm nil :type 'imm-byte))
                                           :printer `(:name :tab reg/mem ", " reg ", "imm))
                (:emitter
                 (aver (and (xmm-register-p src) (not (xmm-register-p dst))))
                 (emit-avx2-inst segment dst src ,prefix ,op
                                 :w ,w
                                 :l 0
                                 :opcode-prefix #x0f3a
                                 :remaining-bytes 1)
                 (emit-byte segment imm)))))

  (def-insert vpinsrb #x66 #x20 :byte)
  (def-insert vpinsrq #x66 #x22 :qword :w 1)
  (def-insert vpinsrw #x66 #xc4 :word :op-prefix #x0f)
  (def-insert vpinsrd #x66 #x22 :dword)
  (def-insert vinsertps #x66 #x21 nil)

  (def-extract vpextrb #x66 #x14 :byte)
  (def-extract vpextrd #x66 #x16 :dword)
  (def-extract vpextrq #x66 #x16 :qword :w 1)
  (def-extract vextractps #x66 #x17 nil))

(define-instruction vpextrw (segment dst src imm)
  (:emitter
   (aver (xmm-register-p src))
   (if (gpr-p dst)
       (emit-avx2-inst segment dst src #x66 #xc5 :l 0 :remaining-bytes 1)
       (emit-avx2-inst segment dst src #x66 #x15 :opcode-prefix #x0f3a :l 0 :remaining-bytes 1))
   (emit-byte segment imm))
  . #.(append
       (avx2-inst-printer-list 'ymm-ymm/mem #x66 #x15
                               :w 0
                               :opcode-prefix #x0f3a
                               :more-fields `((imm nil :type 'imm-byte))
                               :printer '(:name :tab reg/mem ", " reg ", " imm))
       (avx2-inst-printer-list 'ymm-ymm/mem #x66 #xc5
                               :w 0
                               :more-fields `((imm nil :type 'imm-byte))
                               :printer '(:name :tab reg/mem ", " reg ", " imm)))) ; FIXME: it's reg/m16

(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src src2)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix opcode
                                          :reg-mem-size :sized
                                          :nds t)
                (:emitter
                 (aver (xmm-register-p dst))
                 (let ((src-size (operand-size src2)))
                   (emit-avx2-inst segment src2 dst ,prefix ,opcode
                                   :l 0
                                   :vvvv src
                                   :w (case src-size ;; FIXME: EAs no longer have size attached
                                        (:qword 1)
                                        (:dword 0)
                                        (t 1))))))))
  (def vcvtsi2sd #xf2 #x2a)
  (def vcvtsi2ss #xf3 #x2a))

(macrolet ((def (name prefix opcode &key reg-only)
             `(define-instruction ,name (segment dst src)
                ,@(avx2-inst-printer-list 'reg-ymm/mem prefix opcode)
                (:emitter
                 (aver (gpr-p dst))
                 ,(when reg-only
                    `(aver (xmm-register-p src)))
                 (let ((dst-size (operand-size dst)))
                   (aver (or (eq dst-size :qword) (eq dst-size :dword)))
                   (emit-avx2-inst segment src dst ,prefix ,opcode
                                   :w (ecase dst-size
                                        (:qword 1)
                                        (:dword 0))
                                   ,@(unless reg-only
                                       '(:l 0))))))))
  (def vcvtsd2si  #xf2 #x2d)
  (def vcvtss2si  #xf3 #x2d)
  (def vcvttsd2si #xf2 #x2c)
  (def vcvttss2si #xf3 #x2c)
  (def vmovmskpd  #x66 #x50 :reg-only t)
  (def vmovmskps  nil  #x50 :reg-only t)
  (def vpmovmskb  #x66 #xd7 :reg-only t))

;;; AVX/AVX2 instructions

(define-instruction vzeroupper (segment)
  (:printer vex2-op ((op #x77) (l 0) (r 1) (pp 0)))
  (:emitter
   (emit-two-byte-vex segment 0 0 0 nil)
   (emit-byte segment #x77)))

(define-instruction vzeroall (segment)
  (:printer vex2-op ((op #x77) (l 1) (r 1) (pp 0)))
  (:emitter
   (emit-two-byte-vex segment 0 0 1 nil)
   (emit-byte segment #x77)))

(macrolet ((def (name opcode &optional l (mem-size :qword))
             `(define-instruction ,name (segment dst src)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                          :opcode-prefix #x0f38
                                          :xmmreg-mem-size mem-size
                                          :w 0 :l l)
                (:emitter
                 (emit-avx2-inst segment src dst #x66 ,opcode
                                 :opcode-prefix #x0f38
                                 :w 0 :l ,l)))))
  (def vbroadcastss #x18 nil :dword)
  (def vbroadcastsd #x19 1)
  (def vbroadcastf128 #x1a 1)
  (def vbroadcasti128 #x5a 1)
  (def vpbroadcastb #x78 nil :byte)
  (def vpbroadcastw #x79 nil :word)
  (def vpbroadcastd #x58 nil :dword)
  (def vpbroadcastq #x59))

(macrolet ((def-insert (name prefix op)
             `(define-instruction ,name (segment dst src src2 imm)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op
                                          :w 0 :l 1
                                          :opcode-prefix #x0f3a)
                (:emitter
                 (emit-avx2-inst segment src2 dst ,prefix ,op
                                 :opcode-prefix #x0f3a
                                 :vvvv src
                                 :w 0 :l 1
                                 :remaining-bytes 1)
                 (emit-byte segment imm))))

           (def-extract (name prefix op)
             `(define-instruction ,name (segment dst src imm)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op
                                          :w 0 :l 1
                                          :opcode-prefix #x0f3a
                                          :printer `(:name :tab reg/mem ", " reg ", "imm))
                (:emitter
                 (emit-avx2-inst segment dst src ,prefix ,op
                                 :w 0 :l 1
                                 :opcode-prefix #x0f3a
                                 :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def-insert vinsertf128 #x66 #x18)
  (def-insert vinserti128 #x66 #x38)

  (def-extract vextractf128 #x66 #x19)
  (def-extract vextracti128 #x66 #x39))

(macrolet ((def (name prefix op &optional l)
             `(define-instruction ,name (segment dst src src2 imm)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op
                                          :w 0 :l l
                                          :opcode-prefix #x0f3a)
                (:emitter
                 (emit-avx2-inst segment src2 dst ,prefix ,op
                                 :opcode-prefix #x0f3a
                                 :vvvv src
                                 :w 0 :l ,l
                                 :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vperm2f128 #x66 #x06 1)
  (def vperm2i128 #x66 #x46 1)
  (def vpblendd #x66 #x02))

(macrolet ((def (name prefix op)
             `(define-instruction ,name (segment dst src imm)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-imm prefix op
                                          :w 1 :l 1
                                          :opcode-prefix #x0f3a
                                          :printer '(:name :tab reg ", " reg/mem ", " imm))
                (:emitter
                 (emit-avx2-inst segment src dst ,prefix ,op
                                 :opcode-prefix #x0f3a
                                 :w 1 :l 1
                                 :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vpermpd #x66 #x01)
  (def vpermpq #x66 #x00))

(define-instruction vpermps (segment dst src src2)
  (:emitter
   (emit-avx2-inst segment src2 dst #x66 #x16
                   :opcode-prefix #x0f38
                   :vvvv src
                   :w 0 :l 1))
  . #.(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x16
                              :w 0 :l 1
                              :nds t
                              :opcode-prefix #x0f38))

(macrolet ((def (name op op-imm)
             `(define-instruction ,name (segment dst src src2/imm)
                ,@(append
                   (avx2-inst-printer-list 'ymm-ymm/mem-imm #x66 op-imm
                                           :w 0
                                           :opcode-prefix #x0f3a
                                           :printer '(:name :tab reg ", " reg/mem ", " imm))
                   (avx2-inst-printer-list 'ymm-ymm/mem #x66 op
                                           :w 0
                                           :nds t
                                           :opcode-prefix #x0f38))
                (:emitter
                 (cond ((integerp src2/imm)
                        (emit-avx2-inst segment src dst #x66 ,op-imm
                                        :opcode-prefix #x0f3a
                                        :w 0 :remaining-bytes 1)
                        (emit-byte segment src2/imm))
                       (t
                        (emit-avx2-inst segment src2/imm dst #x66 ,op
                                        :opcode-prefix #x0f38
                                        :vvvv src
                                        :w 0)))))))
  (def vpermilps #x0C #x04)
  (def vpermilpd #x0D #x05))

(macrolet ((def (name prefix op to-mem-op)
             `(define-instruction ,name (segment dst src src2)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op
                                          :w 0
                                          :nds t
                                          :opcode-prefix #x0f38)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix to-mem-op
                                          :w 0
                                          :nds 'to-mem
                                          :opcode-prefix #x0f38)
                (:emitter
                 (cond ((xmm-register-p dst)
                        (aver (ea-p src2))
                        (emit-avx2-inst segment src2 dst ,prefix ,op
                                        :opcode-prefix #x0f38
                                        :vvvv src
                                        :w 0))
                       (t
                        (aver (ea-p dst))
                        (emit-avx2-inst segment dst src2 ,prefix ,to-mem-op
                                        :opcode-prefix #x0f38
                                        :vvvv src
                                        :w 0)))))))
  (def vmaskmovps #x66 #x2c #x2e)
  (def vmaskmovpd #x66 #x2d #x2f))

(macrolet ((def (name prefix op w)
             `(define-instruction ,name (segment dst src src2)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op
                                          :w w
                                          :nds t
                                          :opcode-prefix #x0f38)
                (:emitter
                 (emit-avx2-inst segment src2 dst ,prefix ,op
                                 :opcode-prefix #x0f38
                                 :vvvv src
                                 :w ,w)))))
  (def vpsrlvd #x66 #x45 0)
  (def vpsrlvq #x66 #x45 1)

  (def vpsravd #x66 #x46 0)

  (def vpsllvd #x66 #x47 0)
  (def vpsllvq #x66 #x47 1))

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
                                          :opcode-prefix #x0f38
                                          :printer '(:name :tab reg ", " reg/mem ", " vvvv)
                                          :more-fields
                                          (ecase sizing
                                            (xmm/ymm-vmx/y
                                             '((reg/mem nil :type 'vmx/y)))
                                            (xmm-vmx/y
                                             '((reg nil :type 'xmmreg)
                                               (reg/mem nil :type 'vmx/y)))
                                            (xmm/ymm-vmx
                                             '((reg/mem nil :type 'vmx)))))
                (:emitter
                 (emit-avx2-inst segment vm dst #x66 ,op
                                 :opcode-prefix #x0f38
                                 :vvvv mask
                                 :w ,w
                                 :l ,(ecase sizing
                                       ((xmm/ymm-vmx/y xmm/ymm-vmx)
                                        `(if (is-ymm-id-p (reg-id dst))
                                             1
                                             0))
                                       (xmm-vmx/y
                                        `(if (eq (sc-name (tn-sc (ea-index vm))) 'ymm-reg)
                                             1
                                             0)))
                                 :vm t)))))
  (def vpgatherdd #x90 0 xmm/ymm-vmx/y)
  (def vpgatherqd #x91 0 xmm-vmx/y)
  (def vpgatherdq #x90 1 xmm/ymm-vmx)
  (def vpgatherqq #x91 1 xmm/ymm-vmx/y)

  (def vgatherdps #x92 0 xmm/ymm-vmx/y)
  (def vgatherdpd #x92 1 xmm/ymm-vmx)
  (def vgatherqps #x93 0 xmm-vmx/y)
  (def vgatherqpd #x93 1 xmm/ymm-vmx/y))

;;; FMA

(macrolet ((def-insert (name prefix op &key w l)
             `(define-instruction ,name (segment dst src src2)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem prefix op
                                          :w w :l l
                                          :nds t
                                          :opcode-prefix #x0f38)
                (:emitter
                 (emit-avx2-inst segment src2 dst ,prefix ,op
                                 :opcode-prefix #x0f38
                                 :vvvv src
                                 :w ,w :l ,l)))))
  (def-insert vfmadd132ps #x66 #x98 :w 0)
  (def-insert vfmadd213ps #x66 #xa8 :w 0)
  (def-insert vfmadd231ps #x66 #xb8 :w 0)
  (def-insert vfmadd132pd #x66 #x98 :w 1)
  (def-insert vfmadd213pd #x66 #xa8 :w 1)
  (def-insert vfmadd231pd #x66 #xb8 :w 1)

  (def-insert vfmadd132ss #x66 #x99 :w 0 :l 0)
  (def-insert vfmadd213ss #x66 #xa9 :w 0 :l 0)
  (def-insert vfmadd231ss #x66 #xb9 :w 0 :l 0)
  (def-insert vfmadd132sd #x66 #x99 :w 1 :l 0)
  (def-insert vfmadd213sd #x66 #xa9 :w 1 :l 0)
  (def-insert vfmadd231sd #x66 #xb9 :w 1 :l 0)

  (def-insert vfnmadd132ps #x66 #x9c :w 0)
  (def-insert vfnmadd213ps #x66 #xac :w 0)
  (def-insert vfnmadd231ps #x66 #xbc :w 0)
  (def-insert vfnmadd132pd #x66 #x9c :w 1)
  (def-insert vfnmadd213pd #x66 #xac :w 1)
  (def-insert vfnmadd231pd #x66 #xbc :w 1)

  (def-insert vfnmadd132ss #x66 #x9d :w 0 :l 0)
  (def-insert vfnmadd213ss #x66 #xad :w 0 :l 0)
  (def-insert vfnmadd231ss #x66 #xbd :w 0 :l 0)
  (def-insert vfnmadd132sd #x66 #x9d :w 1 :l 0)
  (def-insert vfnmadd213sd #x66 #xad :w 1 :l 0)
  (def-insert vfnmadd231sd #x66 #xbd :w 1 :l 0)

  (def-insert vfmaddsub132ps #x66 #x96 :w 0)
  (def-insert vfmaddsub213ps #x66 #xa6 :w 0)
  (def-insert vfmaddsub231ps #x66 #xb6 :w 0)
  (def-insert vfmaddsub132pd #x66 #x96 :w 1)
  (def-insert vfmaddsub213pd #x66 #xa6 :w 1)
  (def-insert vfmaddsub231pd #x66 #xb6 :w 1)

  (def-insert vfmsubadd132ps #x66 #x97 :w 0)
  (def-insert vfmsubadd213ps #x66 #xa7 :w 0)
  (def-insert vfmsubadd231ps #x66 #xb7 :w 0)
  (def-insert vfmsubadd132pd #x66 #x97 :w 1)
  (def-insert vfmsubadd213pd #x66 #xa7 :w 1)
  (def-insert vfmsubadd231pd #x66 #xb7 :w 1)

  (def-insert vfmsub132ps #x66 #x9a :w 0)
  (def-insert vfmsub213ps #x66 #xaa :w 0)
  (def-insert vfmsub231ps #x66 #xba :w 0)
  (def-insert vfmsub132pd #x66 #x9a :w 1)
  (def-insert vfmsub213pd #x66 #xaa :w 1)
  (def-insert vfmsub231pd #x66 #xba :w 1)

  (def-insert vfmsub132ss #x66 #x9b :w 0 :l 0)
  (def-insert vfmsub213ss #x66 #xab :w 0 :l 0)
  (def-insert vfmsub231ss #x66 #xbb :w 0 :l 0)
  (def-insert vfmsub132sd #x66 #x9b :w 1 :l 0)
  (def-insert vfmsub213sd #x66 #xab :w 1 :l 0)
  (def-insert vfmsub231sd #x66 #xbb :w 1 :l 0))
;;; F16C

(define-instruction vcvtph2ps (segment dst src)
  (:emitter
   (emit-avx2-inst segment src dst #x66 #x13
                   :opcode-prefix #x0f38
                   :w 0))
  . #.(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x13
                              :w 0
                              :opcode-prefix #x0f38))

(define-instruction vcvtps2ph (segment dst src imm)
  (:emitter
   (emit-avx2-inst segment dst src #x66 #x1d
                   :opcode-prefix #x0f3a
                   :w 0
                   :remaining-bytes 1)
   (emit-byte segment imm))
  . #.(avx2-inst-printer-list 'ymm-ymm/mem #x66 #x1d
                              :w 0
                              :opcode-prefix #x0f3a
                              :printer '(:name :tab reg/mem ", " reg ", " imm)))
