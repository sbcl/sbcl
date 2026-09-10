
(in-package "SB-X86-64-ASM")

;;;; AVX-512 Instruction Support
;;;;
;;;; Implemented subsets:
;;;;   AVX-512F, AVX-512BW, AVX-512DQ, AVX-512CD, AVX-512IFMA,
;;;;   AVX-512VBMI, AVX-512VBMI2, AVX-512VPOPCNTDQ, AVX-512BITALG,
;;;;   AVX-512VNNI, AVX-512BF16, AVX-512FP16,
;;;;   GFNI (in avx2-insts.lisp),
;;;;   VPCLMULQDQ-256/512 (via VEX auto-promotion to EVEX),
;;;;   VAES-256/512 (wide forms of vaesenc/vaesdec),
;;;;   EVEX Compare-to-Opmask (vcmpps, vcmppd, vcmpss, vcmpsd),
;;;;   EVEX Masked Arithmetic & Logic (vadd*, vsub*, vmul*, vdiv*, vsqrt*, vpadd*, vpsub*, vpand*, vpor*, vpxor* with {k} and {z}),
;;;;   EVEX Broadcasts (vbroadcasts*, vpbroadcast* from memory, XMM, and GPRs),
;;;;   EVEX Opmask Transfers & Manipulation (kmov*, kand*, kor*, kxor*, knot*, etc.),
;;;;   EVEX Gather & Scatter (vpgather*, vgather*, vpscatter*, vscatter*),
;;;;   VP2INTERSECT (vp2intersectd/q).
;;;;
;;;; Not yet implemented / Future extensions:
;;;;   AVX-512VL  - Explicit EVEX 128/256-bit forms with masking/broadcast
;;;;                (auto-promotion handles basic ZMM; full VL needs
;;;;                explicit EVEX for XMM/YMM with masking)
;;;;   AVX-512ER/PF - vexp2ps/pd, prefetch (Knights Landing Xeon Phi, deprecated)

;;;; AVX-512 (EVEX-only) instruction definitions

(defun full-vector-disp-n (reg)
  (cond ((zmm-register-p reg) 64) ((ymm-register-p reg) 32)
        ((xmm-register-p reg) 16) (t 0)))


(macrolet ((def (name prefix opcode-from opcode-to w)
             `(define-instruction ,name (segment dst src)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode-from :w w :ll ll :disp-n n))
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode-to :w w :ll ll :disp-n n :printer
                               '(:name :tab reg/mem ", " reg)))
               (:emitter
                (cond
                 ((xmm-register-p dst)
                  (emit-avx512-inst segment src dst ,prefix ,opcode-from
                                    :opcode-prefix 15 :w ,w :disp-n
                                    (full-vector-disp-n dst)))
                 (t (aver (xmm-register-p src))
                  (emit-avx512-inst segment dst src ,prefix ,opcode-to
                                    :opcode-prefix 15 :w ,w :disp-n
                                    (full-vector-disp-n src))))))))
  (def vmovdqa32 102 111 127 0)
  (def vmovdqa64 102 111 127 1)
  (def vmovdqu8 242 111 127 0)
  (def vmovdqu16 242 111 127 1)
  (def vmovdqu32 243 111 127 0)
  (def vmovdqu64 243 111 127 1))


(macrolet ((def (name prefix w)
             `(define-instruction ,name (segment dst src &optional src2)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-dir prefix #b0001000)
                (:emitter
                 (cond ((ea-p src)
                        (if (zmm-register-p dst)
                            (emit-avx512-inst segment src dst ,prefix #x10 :w ,w)
                            (emit-avx2-inst segment src dst ,prefix #x10 :l 0 :w ,w)))

                       ((and (ea-p dst) (zmm-register-p src))
                        (emit-avx512-inst segment dst src ,prefix #x11 :w ,w))

                       ((and (integerp src) src2 (register-p src2))
                        (if (or (zmm-register-p dst) (zmm-register-p src2))
                            (emit-avx512-inst segment src2 dst ,prefix #x10 :w ,w)
                            (emit-avx2-inst segment src2 dst ,prefix #x10 :l 0 :w ,w)))

                       ((and src2 (or (zmm-register-p dst)
                                      (zmm-register-p src)
                                      (zmm-register-p src2)))
                        (emit-avx512-inst segment src2 dst ,prefix #x10 :vvvv src :w ,w))

                       ((or (zmm-register-p dst)
                            (zmm-register-p src))
                        (emit-avx512-inst segment src dst ,prefix #x10 :vvvv dst :w ,w))

                       ((and src src2 dst (xmm-register-p dst))
                        (emit-avx2-inst segment src2 dst ,prefix #x10 :vvvv src :l 0 :w ,w))

                       ((xmm-register-p dst)
                        (if (register-p src)
                            (emit-avx2-inst segment src dst ,prefix #x10 :vvvv dst :l 0 :w ,w)
                            (emit-avx2-inst segment src dst ,prefix #x10 :l 0 :w ,w)))

                       (t
                        (aver (xmm-register-p src))
                        (emit-avx2-inst segment dst src ,prefix #x11 :l 0 :w ,w)))))))
  (def vmovsd #xf2 1)
  (def vmovss #xf3 0))

;;; Ternary logic
(macrolet ((def (name w)
             `(define-instruction ,name (segment dst src1 src2 imm)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm 102 37
                  :opcode-prefix 3898 :w w)
               (:emitter
                (emit-avx512-inst segment src2 dst 102 37 :opcode-prefix 3898
                                  :vvvv src1 :w ,w :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vpternlogd 0)
  (def vpternlogq 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :nds t :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpermt2d 126 0)
  (def vpermt2q 126 1)
  (def vpermt2ps 127 0)
  (def vpermt2pd 127 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem-imm 102
                               opcode :opcode-prefix 3898 :w w :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3898 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0))
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vshuff32x4 35 0)
  (def vshuff64x2 35 1)
  (def vshufi32x4 67 0)
  (def vshufi64x2 67 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :nds t :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vblendmps 101 0)
  (def vblendmpd 101 1)
  (def vpblendmd 100 0)
  (def vpblendmq 100 1))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix 3896 :w w :ll ll :disp-n n
                               :printer '(:name :tab reg/mem ", " reg)))
               (:emitter
                (emit-avx512-inst segment dst src ,prefix ,opcode
                                  :opcode-prefix 3896 :w ,w :disp-n
                                  (cond ((zmm-register-p src) 64)
                                        ((ymm-register-p src) 32)
                                        ((xmm-register-p src) 16) (t 0)))))))
  (def vcompressps 102 138 0)
  (def vcompresspd 102 138 1)
  (def vpcompressd 102 139 0)
  (def vpcompressq 102 139 1))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix 3896 :w w :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src dst ,prefix ,opcode
                                  :opcode-prefix 3896 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vexpandps 102 136 0)
  (def vexpandpd 102 136 1)
  (def vpexpandd 102 137 0)
  (def vpexpandq 102 137 1))


(macrolet ((def (name prefix opcode w disp-ns)
             `(define-instruction ,name (segment dst src)
               ,@(loop for ll in '(0 1 2)
                       for n in disp-ns
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix 3896 :w w :ll ll :disp-n n
                               :printer '(:name :tab reg/mem ", " reg)))
               (:emitter
                (emit-avx512-inst segment dst src ,prefix ,opcode
                                  :opcode-prefix 3896 :w ,w :disp-n
                                  (cond
                                   ((zmm-register-p src) (third ',disp-ns))
                                   ((ymm-register-p src) (second ',disp-ns))
                                   ((xmm-register-p src) (first ',disp-ns))
                                   (t 0)))))))
  (def vpmovqd 243 53 0 (8 16 32))
  (def vpmovqw 243 52 0 (4 8 16))
  (def vpmovqb 243 50 0 (2 4 8))
  (def vpmovdw 243 51 0 (8 16 32))
  (def vpmovdb 243 49 0 (4 8 16))
  (def vpmovwb 243 48 0 (8 16 32)))


(macrolet ((def (name opcode w &key (opcode-prefix 3896))
             `(define-instruction ,name (segment dst src)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                  :opcode-prefix opcode-prefix :w w)
               (:emitter
                (emit-avx512-inst segment src dst 102 ,opcode :opcode-prefix
                                  ,opcode-prefix :w ,w))))
           (def-imm (name opcode w)
             `(define-instruction ,name (segment dst src imm)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm 102 opcode
                  :opcode-prefix 3898 :w w :printer
                  '(:name :tab reg ", " reg/mem ", " imm))
               (:emitter
                (emit-avx512-inst segment src dst 102 ,opcode :opcode-prefix
                                  3898 :w ,w :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vgetexpps 66 0)
  (def vgetexppd 66 1)
  (def-imm vgetmantps 38 0)
  (def-imm vgetmantpd 38 1))


(macrolet ((def (name opcode w &key scalar-disp-n)
             `(define-instruction ,name (segment dst src imm)
               ,@(if scalar-disp-n
                     (avx512-inst-printer-list 'ymm-ymm/mem-imm 102 opcode
                      :opcode-prefix 3898 :w w :disp-n scalar-disp-n :printer
                      '(:name :tab reg ", " reg/mem ", " imm))
                     (loop for (ll n) in '((0 16) (1 32) (2 64))
                           append (avx512-inst-printer-list 'ymm-ymm/mem-imm
                                   102 opcode :opcode-prefix 3898 :w w :ll ll
                                   :disp-n n :printer
                                   '(:name :tab reg ", " reg/mem ", " imm))))
               (:emitter
                (emit-avx512-inst segment src dst 102 ,opcode :opcode-prefix
                                  3898 :w ,w :disp-n
                                  ,(if scalar-disp-n
                                       scalar-disp-n
                                       '(cond ((zmm-register-p dst) 64)
                                              ((ymm-register-p dst) 32)
                                              ((xmm-register-p dst) 16) (t 0)))
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vrndscaleps 8 0)
  (def vrndscalepd 9 1)
  (def vrndscaless 10 0 :scalar-disp-n 4)
  (def vrndscalesd 11 1 :scalar-disp-n 8))


(macrolet ((def (name opcode w &key scalar-disp-n)
             `(define-instruction ,name (segment dst src1 src2 imm)
               ,@(if scalar-disp-n
                     (avx512-inst-printer-list 'ymm-ymm/mem-imm 102 opcode
                      :opcode-prefix 3898 :w w :disp-n scalar-disp-n)
                     (loop for (ll n) in '((0 16) (1 32) (2 64))
                           append (avx512-inst-printer-list 'ymm-ymm/mem-imm
                                   102 opcode :opcode-prefix 3898 :w w :ll ll
                                   :disp-n n)))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3898 :vvvv src1 :w ,w :disp-n
                                  ,(if scalar-disp-n
                                       scalar-disp-n
                                       '(cond ((zmm-register-p dst) 64)
                                              ((ymm-register-p dst) 32)
                                              ((xmm-register-p dst) 16) (t 0)))
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vfixupimmps 84 0)
  (def vfixupimmpd 84 1)
  (def vfixupimmss 85 0 :scalar-disp-n 4)
  (def vfixupimmsd 85 1 :scalar-disp-n 8))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem-imm 102
                               opcode :opcode-prefix 3898 :w w :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3898 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0))
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vrangeps 80 0)
  (def vrangepd 80 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src imm)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem-imm 102
                               opcode :opcode-prefix 3898 :w w :ll ll :disp-n n
                               :printer
                               '(:name :tab reg ", " reg/mem ", " imm)))
               (:emitter
                (emit-avx512-inst segment src dst 102 ,opcode :opcode-prefix
                                  3898 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0))
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vreduceps 86 0)
  (def vreducepd 86 1))


(macrolet ((def (name opcode w &key scalar-disp-n)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(if scalar-disp-n
                     (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                      :opcode-prefix 3896 :w w :nds t :disp-n scalar-disp-n)
                     (loop for (ll n) in '((0 16) (1 32) (2 64))
                           append (avx512-inst-printer-list 'ymm-ymm/mem 102
                                   opcode :opcode-prefix 3896 :w w :nds t :ll
                                   ll :disp-n n)))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n
                                  ,(if scalar-disp-n
                                       scalar-disp-n
                                       '(cond ((zmm-register-p dst) 64)
                                              ((ymm-register-p dst) 32)
                                              ((xmm-register-p dst) 16)
                                              (t 0))))))))
  (def vscalefps 44 0)
  (def vscalefpd 44 1)
  (def vscalefss 45 0 :scalar-disp-n 4)
  (def vscalefsd 45 1 :scalar-disp-n 8))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kmov-printer-list (format-stem prefix opcode w &key printer)
    (let ((pp (vex-encode-pp prefix)) (m-mmmm (vex-encode-m-mmmm 15)))
      (flet ((make-printer (inst-format fields)
               `(:printer ,inst-format ,fields ,@(when printer `(',printer)))))
        (if (eql w 1)
            (list
             (make-printer (symbolicate "VEX3-" format-stem)
              `((pp ,pp) (m-mmmm ,m-mmmm) (w ,w) (op ,opcode))))
            (list
             (make-printer (symbolicate "VEX2-" format-stem)
              `((pp ,pp) (op ,opcode)))
             (make-printer (symbolicate "VEX3-" format-stem)
              `((pp ,pp) (m-mmmm ,m-mmmm) (w ,w) (op ,opcode)))))))))

;;; These use VEX encoding (not EVEX), with k registers in ModR/M fields
(macrolet ((def (name kk-prefix gr-prefix store-mem-prefix load-mem-prefix
                     op-k-k op-k-r op-r-k op-m-k op-k-m w &optional (gr-w w))
             `(define-instruction ,name (segment dst src)
               (:emitter
                (cond
                 ((and (k-register-p dst) (k-register-p src))
                  (emit-vex segment nil src dst ,kk-prefix #x0F 0 ,w)
                  (emit-bytes segment ,op-k-k) (emit-ea segment src dst))

                 ((and (k-register-p dst) (gpr-p src))
                  ;; VEX: k1 <- r32/r64
                  (emit-vex segment nil src dst ,gr-prefix #x0F 0 ,gr-w)
                  (emit-bytes segment ,op-k-r)
                  (emit-ea segment src dst))

                 ((and (gpr-p dst) (k-register-p src))
                  ;; VEX: r32/r64 <- k1
                  (emit-vex segment nil src dst ,gr-prefix #x0F 0 ,gr-w)
                  (emit-bytes segment ,op-r-k)
                  (emit-ea segment src dst))

                 ((and (k-register-p dst) (or (ea-p src) (tn-p src)))
                  ;; VEX: k1 <- m16/m32/m64
                  (emit-vex segment nil src dst ,load-mem-prefix #x0F 0 ,w)
                  (emit-bytes segment ,op-k-m)
                  (emit-ea segment src dst))

                 ((and (or (ea-p dst) (tn-p dst)) (k-register-p src))
                  ;; VEX: m16/m32/m64 <- k1
                  (emit-vex segment nil dst src ,store-mem-prefix #x0F 0 ,w)
                  (emit-bytes segment ,op-m-k)
                  (emit-ea segment dst src))

                 (t
                  (error "invalid operands for ~A: ~S, ~S" ',name dst src))))

                ;; printers:
                ;; K <- K and K <- memory share the same opcode
                ;; and are both decoded by kreg-kreg/mem.
                ,@(kmov-printer-list 'kreg-kreg/mem kk-prefix op-k-k w)

                ;; K <- GPR
                ,@(kmov-printer-list 'kreg-reg/mem gr-prefix op-k-r gr-w)

                ;; GPR <- K
                ,@(kmov-printer-list 'reg-kreg/mem gr-prefix op-r-k gr-w)

                ;; memory <- K
                ;; ModRM.reg = K, ModRM.r/m = memory.
                ;; kreg-kreg/mem can decode r/m as memory.
                ,@(kmov-printer-list 'kreg-kreg/mem store-mem-prefix op-m-k w
                                     :printer '(:name :tab reg/mem ", " reg)))))

  ;;         kk       gr    store load  k<-k k<-r r<-k m<-k k<-m  w  gr-w
  (def kmovw nil      nil   nil   nil   #x90 #x92 #x93 #x91 #x90  0  0)
  (def kmovb #x66     #x66  #x66  #x66  #x90 #x92 #x93 #x91 #x90  0  0)
  (def kmovd #x66     #xf2  #x66  #x66  #x90 #x92 #x93 #x91 #x90  1  0)
  (def kmovq nil      #xf2  nil   nil   #x90 #x92 #x93 #x91 #x90  1  1))

;;; KAND, KOR, KXOR, etc. - Opmask logical operations (VEX.L1)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun klogical-printer-list (prefix opcode w)
    (let ((pp (vex-encode-pp prefix)) (m-mmmm (vex-encode-m-mmmm 15)))
      (flet ((make-printer (inst-format fields)
               `(:printer ,inst-format ,fields)))
        (if (eql w 1)
            (list
             (make-printer (symbolicate "VEX3-" 'kreg-kreg/mem-k)
              `((pp ,pp) (m-mmmm ,m-mmmm) (w ,w) (l 1) (op ,opcode))))
            (list
             (make-printer (symbolicate "VEX2-" 'kreg-kreg/mem-k)
              `((pp ,pp) (l 1) (op ,opcode)))
             (make-printer (symbolicate "VEX3-" 'kreg-kreg/mem-k)
              `((pp ,pp) (m-mmmm ,m-mmmm) (w ,w) (l 1) (op ,opcode)))))))))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               (:emitter (emit-vex segment src1 src2 dst ,prefix 15 1 ,w)
                (emit-bytes segment ,opcode) (emit-ea segment src2 dst))
               ,@(klogical-printer-list prefix opcode w))))
  (def kandw nil 65 0)
  (def kandb 102 65 0)
  (def kandd 102 65 1)
  (def kandq 242 65 1)
  (def kandnw nil 66 0)
  (def kandnb 102 66 0)
  (def kandnd 102 66 1)
  (def kandnq 242 66 1)
  (def korw nil 69 0)
  (def korb 102 69 0)
  (def kord 102 69 1)
  (def korq 242 69 1)
  (def kxorw nil 71 0)
  (def kxorb 102 71 0)
  (def kxord 102 71 1)
  (def kxorq 242 71 1)
  (def kxnorw nil 70 0)
  (def kxnorb 102 70 0)
  (def kxnord 102 70 1)
  (def kxnorq 242 70 1))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
               (:emitter (emit-vex segment nil src dst ,prefix 15 0 ,w)
                (emit-bytes segment ,opcode) (emit-ea segment src dst))
               ,@(kmov-printer-list 'kreg-kreg/mem prefix opcode w))))
  (def knotw nil 68 0)
  (def knotb 102 68 0)
  (def knotd 102 68 1)
  (def knotq 242 68 1)
  (def ktestw nil 153 0)
  (def ktestb 102 153 0)
  (def ktestd 102 153 1)
  (def ktestq 242 153 1)
  (def kortestw nil 152 0)
  (def kortestb 102 152 0)
  (def kortestd 102 152 1)
  (def kortestq 242 152 1))


(macrolet ((def (name prefix w)
             `(define-instruction ,name (segment dst src1 src2)
               (:emitter (emit-vex segment src1 src2 dst ,prefix 15 nil ,w)
                (emit-bytes segment 75) (emit-ea segment src2 dst))
               ,@(klogical-printer-list prefix 75 w))))
  (def kunpckbw 102 0)
  (def kunpckwd nil 0)
  (def kunpckdq nil 1))


(macrolet ((def-insert (name prefix op w disp-n)
             `(define-instruction ,name (segment dst src src2 imm)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm prefix op :w w
                  :opcode-prefix 3898 :disp-n disp-n)
               (:emitter
                (emit-avx512-inst segment src2 dst ,prefix ,op :opcode-prefix
                                  3898 :vvvv src :w ,w :disp-n ,disp-n
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def-insert vinsertf32x4 102 24 0 16)
  (def-insert vinsertf64x2 102 24 1 16)
  (def-insert vinsertf32x8 102 26 0 32)
  (def-insert vinsertf64x4 102 26 1 32)
  (def-insert vinserti32x4 102 56 0 16)
  (def-insert vinserti64x2 102 56 1 16)
  (def-insert vinserti32x8 102 58 0 32)
  (def-insert vinserti64x4 102 58 1 32))


(macrolet ((def-extract (name prefix op w disp-n)
             `(define-instruction ,name (segment dst src imm)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm prefix op :w w
                  :opcode-prefix 3898 :disp-n disp-n :printer
                  '(:name :tab reg/mem ", " reg ", " imm))
               (:emitter
                (emit-avx512-inst segment dst src ,prefix ,op :w ,w
                                  :opcode-prefix 3898 :disp-n ,disp-n
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def-extract vextractf32x4 102 25 0 16)
  (def-extract vextractf64x2 102 25 1 16)
  (def-extract vextractf32x8 102 27 0 32)
  (def-extract vextractf64x4 102 27 1 32)
  (def-extract vextracti32x4 102 57 0 16)
  (def-extract vextracti64x2 102 57 1 16)
  (def-extract vextracti32x8 102 59 0 32)
  (def-extract vextracti64x4 102 59 1 32))


(macrolet ((def (name prefix opcode w &optional (opcode-prefix 3896))
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix opcode-prefix :w w :nds t
                               :ll ll :disp-n n))
               (:emitter
                (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                  :opcode-prefix ,opcode-prefix :vvvv src1 :w
                                  ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpermi2d 102 118 0)
  (def vpermi2q 102 118 1)
  (def vpermi2ps 102 119 0)
  (def vpermi2pd 102 119 1)
  (def vpmaxsq 102 61 1)
  (def vpmaxuq 102 63 1)
  (def vpminsq 102 57 1)
  (def vpminuq 102 59 1)
  (def vprolvd 102 21 0)
  (def vprolvq 102 21 1)
  (def vprorvd 102 20 0)
  (def vprorvq 102 20 1)
  (def vpsravq 102 70 1)
  (def vpandd 102 219 0 15)
  (def vpandq 102 219 1 15)
  (def vpandnd 102 223 0 15)
  (def vpandnq 102 223 1 15)
  (def vpord 102 235 0 15)
  (def vporq 102 235 1 15)
  (def vpxord 102 239 0 15)
  (def vpxorq 102 239 1 15)
  (def vaesenc 102 220 0 3896)
  (def vaesenclast 102 221 0 3896)
  (def vaesdec 102 222 0 3896)
  (def vaesdeclast 102 223 0 3896))


(macrolet ((def (name prefix opcode w &optional (opcode-prefix 3896))
             `(define-instruction ,name (segment dst src)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix opcode-prefix :w w :ll ll
                               :disp-n n))
               (:emitter
                (emit-avx512-inst segment src dst ,prefix ,opcode
                                  :opcode-prefix ,opcode-prefix :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpabsq 102 31 1)
  (def vrcp14ps 102 76 0)
  (def vrcp14pd 102 76 1)
  (def vrsqrt14ps 102 78 0)
  (def vrsqrt14pd 102 78 1))


(macrolet ((def (name opcode w disp-n)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                  :opcode-prefix 3896 :w w :nds t :disp-n disp-n)
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n ,disp-n)))))
  (def vrcp14ss 77 0 4)
  (def vrcp14sd 77 1 8)
  (def vrsqrt14ss 79 0 4)
  (def vrsqrt14sd 79 1 8))


(macrolet ((def (name opcode w &key scalar-disp-n)
             `(define-instruction ,name (segment dst src1 src2 imm)
               ,@(if scalar-disp-n
                     (avx512-inst-printer-list 'ymm-ymm/mem-imm 102 opcode
                      :opcode-prefix 3898 :w w :disp-n scalar-disp-n)
                     (loop for (ll n) in '((0 16) (1 32) (2 64))
                           append (avx512-inst-printer-list 'ymm-ymm/mem-imm
                                   102 opcode :opcode-prefix 3898 :w w :ll ll
                                   :disp-n n)))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3898 :vvvv src1 :w ,w :disp-n
                                  ,(if scalar-disp-n
                                       scalar-disp-n
                                       '(cond ((zmm-register-p dst) 64)
                                              ((ymm-register-p dst) 32)
                                              ((xmm-register-p dst) 16) (t 0)))
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def valignd 3 0)
  (def valignq 3 1)
  (def vrangess 81 0 :scalar-disp-n 4)
  (def vrangesd 81 1 :scalar-disp-n 8)
  (def vreducess 87 0 :scalar-disp-n 4)
  (def vreducesd 87 1 :scalar-disp-n 8)
  (def vgetmantss 39 0 :scalar-disp-n 4)
  (def vgetmantsd 39 1 :scalar-disp-n 8))


(macrolet ((def (name opcode w scalar-disp-n)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                  :opcode-prefix 3896 :w w :nds t :disp-n scalar-disp-n)
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n
                                  ,scalar-disp-n)))))
  (def vgetexpss 67 0 4)
  (def vgetexpsd 67 1 8))


(macrolet ((def (name opcode /i w)
             `(define-instruction ,name (segment dst src imm)
               ,@(avx512-inst-printer-list 'ymm-ymm-imm 102 opcode :w w
                  :more-fields (list (list '/i /i)))
               (:emitter
                (emit-avx512-inst-imm segment dst src imm 102 ,opcode ,/i :w
                                      ,w)))))
  (def vprold 114 1 0)
  (def vprolq 114 1 1)
  (def vprord 114 0 0)
  (def vprorq 114 0 1))


(define-instruction vpsraq (segment dst src imm)
 (:emitter (emit-avx512-inst-imm segment dst src imm 102 114 4 :w 1))
 . #.(avx512-inst-printer-list 'ymm-ymm-imm 102 114 :w 1 :more-fields
      '((/i 4))))


(macrolet ((def (name prefix opcode w disp-ns &optional (opcode-prefix 15))
             `(define-instruction ,name (segment dst src)
               ,@(loop for ll in '(0 1 2)
                       for n in disp-ns
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix opcode-prefix :w w :ll ll
                               :disp-n n))
               (:emitter
                (emit-avx512-inst segment src dst ,prefix ,opcode
                                  :opcode-prefix ,opcode-prefix :w ,w :disp-n
                                  (cond
                                   ((zmm-register-p dst) (third ',disp-ns))
                                   ((ymm-register-p dst) (second ',disp-ns))
                                   ((xmm-register-p dst) (first ',disp-ns))
                                   (t 0)))))))
  (def vcvtps2udq nil 121 0 (16 32 64))
  (def vcvtpd2udq nil 121 1 (16 32 64))
  (def vcvttps2udq nil 120 0 (16 32 64))
  (def vcvttpd2udq nil 120 1 (16 32 64))
  (def vcvtudq2ps 242 122 0 (16 32 64))
  (def vcvtudq2pd 243 122 0 (8 16 32)))


(macrolet ((def (name prefix opcode disp-n)
             `(define-instruction ,name (segment dst src)
               ,@(avx512-inst-printer-list 'reg-ymm/mem prefix opcode :w 0
                  :disp-n disp-n)
               ,@(avx512-inst-printer-list 'reg-ymm/mem prefix opcode :w 1
                  :disp-n disp-n)
               (:emitter (aver (gpr-p dst))
                (let ((dst-size (operand-size dst)))
                  (aver (or (eq dst-size :qword) (eq dst-size :dword)))
                  (emit-avx512-inst segment src dst ,prefix ,opcode :w
                                    (ecase dst-size (:qword 1) (:dword 0))
                                    :disp-n ,disp-n))))))
  (def vcvtss2usi 243 121 4)
  (def vcvtsd2usi 242 121 8)
  (def vcvttss2usi 243 120 4)
  (def vcvttsd2usi 242 120 8))


(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src src2)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode :nds t :w
                  0 :disp-n 4)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode :nds t :w
                  1 :disp-n 8)
               (:emitter (aver (xmm-register-p dst))
                (let ((src-size (operand-size src2)))
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode :vvvv src
                                    :w
                                    (case src-size (:qword 1) (:dword 0) (t 1))
                                    :disp-n
                                    (case src-size
                                      (:qword 8)
                                      (:dword 4)
                                      (t 8))))))))
  (def vcvtusi2ss 243 123)
  (def vcvtusi2sd 242 123))


(macrolet ((def (name prefix opcode w disp-ns)
             `(define-instruction ,name (segment dst src)
               ,@(loop for ll in '(0 1 2)
                       for n in disp-ns
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix 3896 :w w :ll ll :disp-n n
                               :printer '(:name :tab reg/mem ", " reg)))
               (:emitter
                (emit-avx512-inst segment dst src ,prefix ,opcode
                                  :opcode-prefix 3896 :w ,w :disp-n
                                  (cond
                                   ((zmm-register-p src) (third ',disp-ns))
                                   ((ymm-register-p src) (second ',disp-ns))
                                   ((xmm-register-p src) (first ',disp-ns))
                                   (t 0)))))))
  (def vpmovsqd 243 37 0 (8 16 32))
  (def vpmovsqw 243 36 0 (4 8 16))
  (def vpmovsqb 243 34 0 (2 4 8))
  (def vpmovusqd 243 21 0 (8 16 32))
  (def vpmovusqw 243 20 0 (4 8 16))
  (def vpmovusqb 243 18 0 (2 4 8))
  (def vpmovsdw 243 35 0 (8 16 32))
  (def vpmovsdb 243 33 0 (2 4 8))
  (def vpmovusdw 243 19 0 (8 16 32))
  (def vpmovusdb 243 17 0 (2 4 8))
  (def vpmovswb 243 32 0 (4 8 16))
  (def vpmovuswb 243 16 0 (4 8 16)))


(macrolet ((def (name opcode w disp-n)
             `(define-instruction ,name (segment dst src)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                  :opcode-prefix 3896 :w w :disp-n disp-n)
               (:emitter
                (emit-avx512-inst segment src dst 102 ,opcode :opcode-prefix
                                  3896 :w ,w :disp-n ,disp-n)))))
  (def vbroadcastf32x4 26 0 16)
  (def vbroadcastf64x4 27 1 32)
  (def vbroadcasti32x4 90 0 16)
  (def vbroadcasti64x4 91 1 32))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem 243 opcode
                  :opcode-prefix 3896 :w w)
               (:emitter
                (emit-avx512-inst segment src dst 243 ,opcode :opcode-prefix
                                  3896 :w ,w)))))
  (def vpbroadcastmb2q 42 1)
  (def vpbroadcastmw2d 58 0))


;;; Compatibility aliases for vpbroadcast from GPR
(defmacro vpbroadcastd-gpr (segment dst src)
  `(vpbroadcastd ,segment ,dst ,src))
(defmacro vpbroadcastq-gpr (segment dst src)
  `(vpbroadcastq ,segment ,dst ,src))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kshift-printer-list (prefix opcode w)
    (let ((fields
           `((pp ,(vex-encode-pp prefix)) (m-mmmm ,(vex-encode-m-mmmm 3898))
             (w ,w) (l 0) (op ,opcode) (imm nil :type 'imm-byte))))
      (list `(:printer ,(symbolicate "VEX3-" 'kreg-kreg/mem-imm) ,fields)))))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src imm)
               (:emitter (emit-vex segment nil src dst ,prefix 3898 0 ,w)
                (emit-bytes segment ,opcode)
                (emit-ea segment src dst :remaining-bytes 1)
                (emit-byte segment imm))
               ,@(kshift-printer-list prefix opcode w))))
  (def kshiftlb 102 50 0)
  (def kshiftlw 102 50 1)
  (def kshiftld 102 51 0)
  (def kshiftlq 102 51 1)
  (def kshiftrb 102 48 0)
  (def kshiftrw 102 48 1)
  (def kshiftrd 102 49 0)
  (def kshiftrq 102 49 1))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               (:emitter (emit-vex segment src1 src2 dst ,prefix 15 1 ,w)
                (emit-bytes segment ,opcode) (emit-ea segment src2 dst))
               ,@(klogical-printer-list prefix opcode w))))
  (def kaddb 102 74 0)
  (def kaddw nil 74 0)
  (def kaddd 102 74 1)
  (def kaddq 242 74 1))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem-imm prefix
                               opcode :opcode-prefix 3898 :w w :ll ll :disp-n n
                               :more-fields '((reg nil :type 'opmask-reg))))
               (:emitter
                (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                  :opcode-prefix 3898 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p src1) 64)
                                        ((ymm-register-p src1) 32)
                                        ((xmm-register-p src1) 16) (t 0))
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vpcmpd 102 31 0)
  (def vpcmpud 102 30 0)
  (def vpcmpq 102 31 1)
  (def vpcmpuq 102 30 1))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix 3896 :w w :nds t :ll ll
                               :disp-n n :more-fields
                               '((reg nil :type 'opmask-reg))))
               (:emitter
                (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                  :opcode-prefix 3896 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p src1) 64)
                                        ((ymm-register-p src1) 32)
                                        ((xmm-register-p src1) 16) (t 0)))))))
  (def vptestmd 102 39 0)
  (def vptestmq 102 39 1)
  (def vptestnmd 243 39 0)
  (def vptestnmq 243 39 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :nds t :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpblendmb 102 0)
  (def vpblendmw 102 1))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem-imm prefix
                               opcode :opcode-prefix 3898 :w w :ll ll :disp-n n
                               :more-fields '((reg nil :type 'opmask-reg))))
               (:emitter
                (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                  :opcode-prefix 3898 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p src1) 64)
                                        ((ymm-register-p src1) 32)
                                        ((xmm-register-p src1) 16) (t 0))
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vpcmpb 102 63 0)
  (def vpcmpub 102 62 0)
  (def vpcmpw 102 63 1)
  (def vpcmpuw 102 62 1))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix 3896 :w w :nds t :ll ll
                               :disp-n n :more-fields
                               '((reg nil :type 'opmask-reg))))
               (:emitter
                (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                  :opcode-prefix 3896 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p src1) 64)
                                        ((ymm-register-p src1) 32)
                                        ((xmm-register-p src1) 16) (t 0)))))))
  (def vptestmb 102 38 0)
  (def vptestmw 102 38 1)
  (def vptestnmb 243 38 0)
  (def vptestnmw 243 38 1))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                  :opcode-prefix 3896 :w w)
               (:emitter
                (emit-avx512-inst segment src dst ,prefix ,opcode
                                  :opcode-prefix 3896 :w ,w)))))
  (def vpmovb2m 243 41 0)
  (def vpmovw2m 243 41 1)
  (def vpmovm2b 243 40 0)
  (def vpmovm2w 243 40 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :nds t :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpermw 141 1)
  (def vpermi2w 117 1)
  (def vpermt2w 125 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :nds t :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpsllvw 18 1)
  (def vpsravw 17 1)
  (def vpsrlvw 16 1))


(define-instruction vdbpsadbw (segment dst src1 src2 imm)
 (:emitter
  (emit-avx512-inst segment src2 dst 102 66 :opcode-prefix 3898 :vvvv src1 :w 0
                    :disp-n
                    (cond ((zmm-register-p dst) 64) ((ymm-register-p dst) 32)
                          ((xmm-register-p dst) 16) (t 0))
                    :remaining-bytes 1)
  (emit-byte segment imm))
 . #.(loop for (ll n) in '((0 16) (1 32) (2 64))
           append (avx512-inst-printer-list 'ymm-ymm/mem-imm 102 66
                   :opcode-prefix 3898 :w 0 :ll ll :disp-n n)))


(macrolet ((def (name opcode w &key scalar-disp-n)
             `(define-instruction ,name (segment dst src imm)
               ,@(if scalar-disp-n
                     (avx512-inst-printer-list 'ymm-ymm/mem-imm 102 opcode
                      :opcode-prefix 3898 :w w :ll 0 :disp-n scalar-disp-n
                      :more-fields '((reg nil :type 'opmask-reg)) :printer
                      '(:name :tab reg ", " reg/mem ", " imm))
                     (loop for (ll n) in '((0 16) (1 32) (2 64))
                           append (avx512-inst-printer-list 'ymm-ymm/mem-imm
                                   102 opcode :opcode-prefix 3898 :w w :ll ll
                                   :disp-n n :more-fields
                                   '((reg nil :type 'opmask-reg)) :printer
                                   '(:name :tab reg ", " reg/mem ", " imm))))
               (:emitter
                (emit-avx512-inst segment src dst 102 ,opcode :opcode-prefix
                                  3898 :w ,w :disp-n
                                  ,(if scalar-disp-n
                                       scalar-disp-n
                                       `(cond ((zmm-register-p src) 64)
                                              ((ymm-register-p src) 32)
                                              ((xmm-register-p src) 16) (t 0)))
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vfpclassps 102 0)
  (def vfpclasspd 102 1)
  (def vfpclassss 103 0 :scalar-disp-n 4)
  (def vfpclasssd 103 1 :scalar-disp-n 8))


(define-instruction vpmullq (segment dst src1 src2)
 (:emitter
  (emit-avx512-inst segment src2 dst 102 64 :opcode-prefix 3896 :vvvv src1 :w 1
                    :disp-n
                    (cond ((zmm-register-p dst) 64) ((ymm-register-p dst) 32)
                          ((xmm-register-p dst) 16) (t 0))))
 . #.(loop for (ll n) in '((0 16) (1 32) (2 64))
           append (avx512-inst-printer-list 'ymm-ymm/mem 102 64 :opcode-prefix
                   3896 :w 1 :nds t :ll ll :disp-n n)))


(macrolet ((def (name prefix opcode w disp-ns &optional (opcode-prefix 15))
             `(define-instruction ,name (segment dst src)
               ,@(loop for ll in '(0 1 2)
                       for n in disp-ns
                       append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                               opcode :opcode-prefix opcode-prefix :w w :ll ll
                               :disp-n n))
               (:emitter
                (emit-avx512-inst segment src dst ,prefix ,opcode
                                  :opcode-prefix ,opcode-prefix :w ,w :disp-n
                                  (cond
                                   ((zmm-register-p dst) (third ',disp-ns))
                                   ((ymm-register-p dst) (second ',disp-ns))
                                   ((xmm-register-p dst) (first ',disp-ns))
                                   (t 0)))))))
  (def vcvtps2qq 102 123 0 (8 16 32))
  (def vcvtps2uqq 102 121 0 (8 16 32))
  (def vcvttps2qq 102 122 0 (8 16 32))
  (def vcvttps2uqq 102 120 0 (8 16 32))
  (def vcvtpd2qq 102 123 1 (16 32 64))
  (def vcvtpd2uqq 102 121 1 (16 32 64))
  (def vcvttpd2qq 102 122 1 (16 32 64))
  (def vcvttpd2uqq 102 120 1 (16 32 64))
  (def vcvtqq2ps nil 91 1 (16 32 64))
  (def vcvtqq2pd 243 230 1 (16 32 64))
  (def vcvtuqq2ps 242 122 1 (16 32 64))
  (def vcvtuqq2pd 243 122 1 (16 32 64)))


(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                  :opcode-prefix 3896 :w w)
               (:emitter
                (emit-avx512-inst segment src dst ,prefix ,opcode
                                  :opcode-prefix 3896 :w ,w)))))
  (def vpmovd2m 243 57 0)
  (def vpmovq2m 243 57 1)
  (def vpmovm2d 243 56 0)
  (def vpmovm2q 243 56 1))


(macrolet ((def (name opcode w disp-n)
             `(define-instruction ,name (segment dst src)
               ,@(avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                  :opcode-prefix 3896 :w w :disp-n disp-n)
               (:emitter
                (emit-avx512-inst segment src dst 102 ,opcode :opcode-prefix
                                  3896 :w ,w :disp-n ,disp-n)))))
  (def vbroadcastf32x2 25 0 8)
  (def vbroadcastf64x2 26 1 16)
  (def vbroadcasti32x2 89 0 8)
  (def vbroadcasti64x2 90 1 16)
  (def vbroadcastf32x8 27 0 32)
  (def vbroadcasti32x8 91 0 32))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :nds t :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpmadd52luq 180 1)
  (def vpmadd52huq 181 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :nds t :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpermb 141 0)
  (def vpermi2b 117 0)
  (def vpermt2b 125 0)
  (def vpmultishiftqb 131 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :ll ll :disp-n n
                               :printer '(:name :tab reg/mem ", " reg)))
               (:emitter
                (emit-avx512-inst segment dst src 102 ,opcode :opcode-prefix
                                  3896 :w ,w :disp-n
                                  (cond ((zmm-register-p src) 64)
                                        ((ymm-register-p src) 32)
                                        ((xmm-register-p src) 16) (t 0)))))))
  (def vpcompressb 99 0)
  (def vpcompressw 99 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :ll ll :disp-n n))
               (:emitter
                (emit-avx512-inst segment src dst 102 ,opcode :opcode-prefix
                                  3896 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpexpandb 98 0)
  (def vpexpandw 98 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem-imm 102
                               opcode :opcode-prefix 3898 :w w :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3898 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0))
                                  :remaining-bytes 1)
                (emit-byte segment imm)))))
  (def vpshldw 112 1)
  (def vpshldd 113 0)
  (def vpshldq 113 1)
  (def vpshrdw 114 1)
  (def vpshrdd 115 0)
  (def vpshrdq 115 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :nds t :ll ll :disp-n
                               n))
               (:emitter
                (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                  3896 :vvvv src1 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpshldvw 112 1)
  (def vpshldvd 113 0)
  (def vpshldvq 113 1)
  (def vpshrdvw 114 1)
  (def vpshrdvd 115 0)
  (def vpshrdvq 115 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :ll ll :disp-n n))
               (:emitter
                (emit-avx512-inst segment src dst 102 ,opcode :opcode-prefix
                                  3896 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpopcntd 85 0)
  (def vpopcntq 85 1))


(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
               ,@(loop for (ll n) in '((0 16) (1 32) (2 64))
                       append (avx512-inst-printer-list 'ymm-ymm/mem 102 opcode
                               :opcode-prefix 3896 :w w :ll ll :disp-n n))
               (:emitter
                (emit-avx512-inst segment src dst 102 ,opcode :opcode-prefix
                                  3896 :w ,w :disp-n
                                  (cond ((zmm-register-p dst) 64)
                                        ((ymm-register-p dst) 32)
                                        ((xmm-register-p dst) 16) (t 0)))))))
  (def vpopcntb 84 0)
  (def vpopcntw 84 1))


(define-instruction vpshufbitqmb (segment dst src1 src2)
 (:emitter
  (emit-avx512-inst segment src2 dst 102 143 :opcode-prefix 3896 :vvvv src1 :w
                    0 :disp-n
                    (cond ((zmm-register-p src1) 64) ((ymm-register-p src1) 32)
                          ((xmm-register-p src1) 16) (t 0))))
 . #.(loop for (ll n) in '((0 16) (1 32) (2 64))
           append (avx512-inst-printer-list 'ymm-ymm/mem 102 143 :opcode-prefix
                   3896 :w 0 :nds t :ll ll :disp-n n :more-fields
                   '((reg nil :type 'opmask-reg)))))

;;; 3-operand NDS with opmask: (inst name dst src1 src2 mask &optional zeroing)
;;; mask is 1-7 (k1-k7; k0 means no masking, or opmask register TN).
;;; zeroing is :z, 1, or 0/nil (merge-masking default).
(macrolet ((def (name prefix opcode w &optional (opcode-prefix #x0f))
             `(define-instruction ,name (segment dst src1 src2 mask &optional (zeroing 0))
                ,@(loop for k from 1 to 7
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix opcode-prefix
                         :w w
                         :nds t
                         :more-fields `((aaa ,k) (z-bit 0))
                         :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}"))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix opcode-prefix
                         :w w
                         :nds t
                         :more-fields `((aaa ,k) (z-bit 1))
                         :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "} {z}")))
                (:emitter
                 (let ((mask-num (cond ((integerp mask) mask)
                                       ((k-register-p mask) (reg-id-num (reg-id mask)))
                                       (t (error "Invalid mask ~S" mask))))
                       (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                   (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                     :opcode-prefix ,opcode-prefix
                                     :vvvv src1
                                     :w ,w
                                     :aaa mask-num
                                     :z z-num
                                     :disp-n (full-vector-disp-n dst)))))))
  ;; Byte/word arithmetic masked
  (def vpaddb-masked #x66 #xfc 0)
  (def vpaddw-masked #x66 #xfd 0)
  (def vpsubb-masked #x66 #xf8 0)
  (def vpsubw-masked #x66 #xf9 0)
  ;; Integer arithmetic (qword)
  (def vpaddq-masked   #x66 #xd4 1)
  (def vpsubq-masked   #x66 #xfb 1)
  (def vpmullq-masked  #x66 #x40 1 #x0f38)
  ;; Integer arithmetic (dword)
  (def vpaddd-masked   #x66 #xfe 0)
  (def vpsubd-masked   #x66 #xfa 0)
  (def vpmulld-masked  #x66 #x40 0 #x0f38)
  ;; Integer logical (qword)
  (def vpandq-masked   #x66 #xdb 1)
  (def vpandnq-masked  #x66 #xdf 1)
  (def vporq-masked    #x66 #xeb 1)
  (def vpxorq-masked   #x66 #xef 1)
  ;; Integer logical (dword)
  (def vpandd-masked   #x66 #xdb 0)
  (def vpandnd-masked  #x66 #xdf 0)
  (def vpord-masked    #x66 #xeb 0)
  (def vpxord-masked   #x66 #xef 0)
  ;; FP arithmetic (double)
  (def vaddpd-masked   #x66 #x58 1)
  (def vsubpd-masked   #x66 #x5c 1)
  (def vmulpd-masked   #x66 #x59 1)
  (def vdivpd-masked   #x66 #x5e 1)
  (def vminpd-masked   #x66 #x5d 1)
  (def vmaxpd-masked   #x66 #x5f 1)
  ;; FP arithmetic (single)
  (def vaddps-masked   nil  #x58 0)
  (def vsubps-masked   nil  #x5c 0)
  (def vmulps-masked   nil  #x59 0)
  (def vdivps-masked   nil  #x5e 0)
  (def vminps-masked   nil  #x5d 0)
  (def vmaxps-masked   nil  #x5f 0)
  ;; Signed dword min/max
  (def vpminsd-masked #x66 #x39 0 #x0f38)
  (def vpmaxsd-masked #x66 #x3d 0 #x0f38)
  ;; Unsigned dword min/max
  (def vpminud-masked #x66 #x3b 0 #x0f38)
  (def vpmaxud-masked #x66 #x3f 0 #x0f38)
  ;; Signed qword min/max
  (def vpminsq-masked #x66 #x39 1 #x0f38)
  (def vpmaxsq-masked #x66 #x3d 1 #x0f38)
  ;; Unsigned qword min/max
  (def vpminuq-masked #x66 #x3b 1 #x0f38)
  (def vpmaxuq-masked #x66 #x3f 1 #x0f38)
  ;; FMA masked
  (def vfmadd132ps-masked #x66 #x98 0 #x0f38)
  (def vfmadd132pd-masked #x66 #x98 1 #x0f38)
  (def vfmadd213ps-masked #x66 #xa8 0 #x0f38)
  (def vfmadd213pd-masked #x66 #xa8 1 #x0f38)
  (def vfmadd231ps-masked #x66 #xb8 0 #x0f38)
  (def vfmadd231pd-masked #x66 #xb8 1 #x0f38)
    ;; FMA subtract masked
  (def vfmsub132ps-masked #x66 #x9a 0 #x0f38)
  (def vfmsub132pd-masked #x66 #x9a 1 #x0f38)
  (def vfmsub213ps-masked #x66 #xaa 0 #x0f38)
  (def vfmsub213pd-masked #x66 #xaa 1 #x0f38)
  (def vfmsub231ps-masked #x66 #xba 0 #x0f38)
  (def vfmsub231pd-masked #x66 #xba 1 #x0f38)
  ;; FMA negative-add masked
  (def vfnmadd132ps-masked #x66 #x9c 0 #x0f38)
  (def vfnmadd132pd-masked #x66 #x9c 1 #x0f38)
  (def vfnmadd213ps-masked #x66 #xac 0 #x0f38)
  (def vfnmadd213pd-masked #x66 #xac 1 #x0f38)
  (def vfnmadd231ps-masked #x66 #xbc 0 #x0f38)
  (def vfnmadd231pd-masked #x66 #xbc 1 #x0f38)
  (def vfnmsub132ps-masked #x66 #x9e 0 #x0f38)
  (def vfnmsub132pd-masked #x66 #x9e 1 #x0f38)
  (def vfnmsub213ps-masked #x66 #xae 0 #x0f38)
  (def vfnmsub213pd-masked #x66 #xae 1 #x0f38)
  (def vfnmsub231ps-masked #x66 #xbe 0 #x0f38)
  (def vfnmsub231pd-masked #x66 #xbe 1 #x0f38)
  ;; FMA add/subtract masked
  (def vfmaddsub132ps-masked #x66 #x96 0 #x0f38)
  (def vfmaddsub132pd-masked #x66 #x96 1 #x0f38)
  (def vfmaddsub213ps-masked #x66 #xa6 0 #x0f38)
  (def vfmaddsub213pd-masked #x66 #xa6 1 #x0f38)
  (def vfmaddsub231ps-masked #x66 #xb6 0 #x0f38)
  (def vfmaddsub231pd-masked #x66 #xb6 1 #x0f38)
  ;; FMA subtract/add masked
  (def vfmsubadd132ps-masked #x66 #x97 0 #x0f38)
  (def vfmsubadd132pd-masked #x66 #x97 1 #x0f38)
  (def vfmsubadd213ps-masked #x66 #xa7 0 #x0f38)
  (def vfmsubadd213pd-masked #x66 #xa7 1 #x0f38)
  (def vfmsubadd231ps-masked #x66 #xb7 0 #x0f38)
  (def vfmsubadd231pd-masked #x66 #xb7 1 #x0f38)
  ;; Variable shift masked
  (def vpsllvd-masked #x66 #x47 0 #x0f38)
  (def vpsllvq-masked #x66 #x47 1 #x0f38)
  (def vpsravd-masked #x66 #x46 0 #x0f38)
  (def vpsravq-masked #x66 #x46 1 #x0f38)
  (def vpsrlvd-masked #x66 #x45 0 #x0f38)
  (def vpsrlvq-masked #x66 #x45 1 #x0f38)
  ;; VNNI dot-product masked
  (def vpdpbusd-masked  #x66 #x50 0 #x0f38)
  (def vpdpbusds-masked #x66 #x51 0 #x0f38)
  (def vpdpwssd-masked  #x66 #x52 0 #x0f38)
  (def vpdpwssds-masked #x66 #x53 0 #x0f38)
  ;; Permute masked (dword/qword)
  (def vpermi2d-masked  #x66 #x76 0 #x0f38)
  (def vpermi2q-masked  #x66 #x76 1 #x0f38)
  (def vpermi2ps-masked #x66 #x77 0 #x0f38)
  (def vpermi2pd-masked #x66 #x77 1 #x0f38)
  (def vpermt2d-masked  #x66 #x7e 0 #x0f38)
  (def vpermt2q-masked  #x66 #x7e 1 #x0f38)
  (def vpermt2ps-masked #x66 #x7f 0 #x0f38)
  (def vpermt2pd-masked #x66 #x7f 1 #x0f38)
  ;; Permute masked (byte/word)
  (def vpermb-masked   #x66 #x8d 0 #x0f38)
  (def vpermi2b-masked #x66 #x75 0 #x0f38)
  (def vpermt2b-masked #x66 #x7d 0 #x0f38)
  (def vpermw-masked   #x66 #x8d 1 #x0f38)
  (def vpermi2w-masked #x66 #x75 1 #x0f38)
  (def vpermt2w-masked #x66 #x7d 1 #x0f38)
  ;; IFMA masked
  (def vpmadd52luq-masked #x66 #xb4 1 #x0f38)
  (def vpmadd52huq-masked #x66 #xb5 1 #x0f38)
  ;; VBMI2 variable shift double masked
  (def vpshldvw-masked #x66 #x70 1 #x0f38)
  (def vpshldvd-masked #x66 #x71 0 #x0f38)
  (def vpshldvq-masked #x66 #x71 1 #x0f38)
  (def vpshrdvw-masked #x66 #x72 1 #x0f38)
  (def vpshrdvd-masked #x66 #x73 0 #x0f38)
  (def vpshrdvq-masked #x66 #x73 1 #x0f38)
  ;; Scale masked
  (def vscalefps-masked #x66 #x2c 0 #x0f38)
  (def vscalefpd-masked #x66 #x2c 1 #x0f38)
  ;; GFNI multiply masked
  (def vgf2p8mulb-masked #x66 #xcf 0 #x0f38)
  ;; VAES masked
  (def vaesenc-masked      #x66 #xdc 0 #x0f38)
  (def vaesenclast-masked  #x66 #xdd 0 #x0f38)
  (def vaesdec-masked      #x66 #xde 0 #x0f38)
  (def vaesdeclast-masked  #x66 #xdf 0 #x0f38)
  ;; VPMULTISHIFTQB masked
  (def vpmultishiftqb-masked #x66 #x83 1 #x0f38))

;;; 2-operand with opmask (vsqrtps/vsqrtpd)
(macrolet ((def (name prefix opcode w &optional (opcode-prefix #x0f))
             `(define-instruction ,name (segment dst src mask &optional (zeroing 0))
                ,@(loop for k from 1 to 7
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix opcode-prefix
                         :w w
                         :more-fields `((aaa ,k) (z-bit 0))
                         :printer '(:name :tab reg ", " reg/mem " {" aaa "}"))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix opcode-prefix
                         :w w
                         :more-fields `((aaa ,k) (z-bit 1))
                         :printer '(:name :tab reg ", " reg/mem " {" aaa "} {z}")))
                (:emitter
                 (let ((mask-num (cond ((integerp mask) mask)
                                       ((k-register-p mask) (reg-id-num (reg-id mask)))
                                       (t (error "Invalid mask ~S" mask))))
                       (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                   (emit-avx512-inst segment src dst ,prefix ,opcode
                                     :opcode-prefix ,opcode-prefix
                                     :w ,w
                                     :aaa mask-num
                                     :z z-num
                                     :disp-n (full-vector-disp-n dst)))))))
  (def vsqrtps-masked nil  #x51 0)
  (def vsqrtpd-masked #x66 #x51 1))
;;; Zeroing masked arithmetic variants
(macrolet ((def-z (name prefix opcode w &optional (opcode-prefix #x0f))
             (let ((zero-name (symbolicate name "-Z"))
                   (mask-printer
                    '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}{z}")))
               `(define-instruction ,zero-name (segment dst src1 src2 mask)
                 ,@(loop for k from 1 to 7
                         append (loop for (ll n) in '((0 16) (1 32) (2 64))
                                      append (avx512-inst-printer-list
                                              'ymm-ymm/mem prefix opcode
                                              :opcode-prefix opcode-prefix :w w
                                              :nds t :ll ll :disp-n n
                                              :more-fields
                                              `((aaa ,k) (z-bit 1)) :printer
                                              mask-printer)))
                 (:emitter
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                    :opcode-prefix ,opcode-prefix :vvvv src1 :w
                                    ,w :aaa mask :z 1
                                    :disp-n
                                    (cond ((zmm-register-p dst) 64)
                                          ((ymm-register-p dst) 32)
                                          ((xmm-register-p dst) 16) (t 0))))))))
  (def-z vpaddb-masked 102 252 0)
  (def-z vpaddw-masked 102 253 0)
  (def-z vpsubb-masked 102 248 0)
  (def-z vpsubw-masked 102 249 0)
  (def-z vpaddq-masked 102 212 1)
  (def-z vpsubq-masked 102 251 1)
  (def-z vpaddd-masked 102 254 0)
  (def-z vpsubd-masked 102 250 0)
  (def-z vpandq-masked 102 219 1)
  (def-z vpandnq-masked 102 223 1)
  (def-z vporq-masked 102 235 1)
  (def-z vpxorq-masked 102 239 1)
  (def-z vaddpd-masked 102 88 1)
  (def-z vsubpd-masked 102 92 1)
  (def-z vmulpd-masked 102 89 1)
  (def-z vaddps-masked nil 88 0)
  (def-z vsubps-masked nil 92 0)
  (def-z vmulps-masked nil 89 0)
  (def-z vdivps-masked nil 94 0)
  (def-z vdivpd-masked 102 94 1)
  (def-z vminps-masked nil 93 0)
  (def-z vmaxps-masked nil 95 0)
  (def-z vminpd-masked 102 93 1)
  (def-z vmaxpd-masked 102 95 1)
  (def-z vfmadd132ps-masked 102 152 0 3896)
  (def-z vfmadd132pd-masked 102 152 1 3896)
  (def-z vfmadd213ps-masked 102 168 0 3896)
  (def-z vfmadd213pd-masked 102 168 1 3896)
  (def-z vfmadd231ps-masked 102 184 0 3896)
  (def-z vfmadd231pd-masked 102 184 1 3896)
  (def-z vpminsd-masked 102 57 0 3896)
  (def-z vpmaxsd-masked 102 61 0 3896)
  (def-z vpminud-masked 102 59 0 3896)
  (def-z vpmaxud-masked 102 63 0 3896)
  (def-z vpminsq-masked 102 57 1 3896)
  (def-z vpmaxsq-masked 102 61 1 3896)
  (def-z vpminuq-masked 102 59 1 3896)
  (def-z vpmaxuq-masked 102 63 1 3896)
  (def-z vfmsub132ps-masked 102 154 0 3896)
  (def-z vfmsub132pd-masked 102 154 1 3896)
  (def-z vfmsub213ps-masked 102 170 0 3896)
  (def-z vfmsub213pd-masked 102 170 1 3896)
  (def-z vfmsub231ps-masked 102 186 0 3896)
  (def-z vfmsub231pd-masked 102 186 1 3896)
  (def-z vfnmadd132ps-masked 102 156 0 3896)
  (def-z vfnmadd132pd-masked 102 156 1 3896)
  (def-z vfnmadd213ps-masked 102 172 0 3896)
  (def-z vfnmadd213pd-masked 102 172 1 3896)
  (def-z vfnmadd231ps-masked 102 188 0 3896)
  (def-z vfnmadd231pd-masked 102 188 1 3896)
  (def-z vfnmsub132ps-masked 102 158 0 3896)
  (def-z vfnmsub132pd-masked 102 158 1 3896)
  (def-z vfnmsub213ps-masked 102 174 0 3896)
  (def-z vfnmsub213pd-masked 102 174 1 3896)
  (def-z vfnmsub231ps-masked 102 190 0 3896)
  (def-z vfnmsub231pd-masked 102 190 1 3896)
  (def-z vfmaddsub132ps-masked 102 150 0 3896)
  (def-z vfmaddsub132pd-masked 102 150 1 3896)
  (def-z vfmaddsub213ps-masked 102 166 0 3896)
  (def-z vfmaddsub213pd-masked 102 166 1 3896)
  (def-z vfmaddsub231ps-masked 102 182 0 3896)
  (def-z vfmaddsub231pd-masked 102 182 1 3896)
  (def-z vfmsubadd132ps-masked 102 151 0 3896)
  (def-z vfmsubadd132pd-masked 102 151 1 3896)
  (def-z vfmsubadd213ps-masked 102 167 0 3896)
  (def-z vfmsubadd213pd-masked 102 167 1 3896)
  (def-z vfmsubadd231ps-masked 102 183 0 3896)
  (def-z vfmsubadd231pd-masked 102 183 1 3896)
  (def-z vpsllvd-masked 102 71 0 3896)
  (def-z vpsllvq-masked 102 71 1 3896)
  (def-z vpsravd-masked 102 70 0 3896)
  (def-z vpsravq-masked 102 70 1 3896)
  (def-z vpsrlvd-masked 102 69 0 3896)
  (def-z vpsrlvq-masked 102 69 1 3896)
  (def-z vpdpbusd-masked 102 80 0 3896)
  (def-z vpdpbusds-masked 102 81 0 3896)
  (def-z vpdpwssd-masked 102 82 0 3896)
  (def-z vpdpwssds-masked 102 83 0 3896)
  (def-z vpermi2d-masked 102 118 0 3896)
  (def-z vpermi2q-masked 102 118 1 3896)
  (def-z vpermi2ps-masked 102 119 0 3896)
  (def-z vpermi2pd-masked 102 119 1 3896)
  (def-z vpermt2d-masked 102 126 0 3896)
  (def-z vpermt2q-masked 102 126 1 3896)
  (def-z vpermt2ps-masked 102 127 0 3896)
  (def-z vpermt2pd-masked 102 127 1 3896)
  (def-z vpermb-masked 102 141 0 3896)
  (def-z vpermi2b-masked 102 117 0 3896)
  (def-z vpermt2b-masked 102 125 0 3896)
  (def-z vpermw-masked 102 141 1 3896)
  (def-z vpermi2w-masked 102 117 1 3896)
  (def-z vpermt2w-masked 102 125 1 3896)
  (def-z vpmultishiftqb-masked 102 131 1 3896)
  (def-z vpmadd52luq-masked 102 180 1 3896)
  (def-z vpmadd52huq-masked 102 181 1 3896)
  (def-z vpshldvw-masked 102 112 1 3896)
  (def-z vpshldvd-masked 102 113 0 3896)
  (def-z vpshldvq-masked 102 113 1 3896)
  (def-z vpshrdvw-masked 102 114 1 3896)
  (def-z vpshrdvd-masked 102 115 0 3896)
  (def-z vpshrdvq-masked 102 115 1 3896)
  (def-z vscalefps-masked 102 44 0 3896)
  (def-z vscalefpd-masked 102 44 1 3896)
  (def-z vgf2p8mulb-masked 102 207 0 3896)
  (def-z vaesenc-masked 102 220 0 3896)
  (def-z vaesenclast-masked 102 221 0 3896)
  (def-z vaesdec-masked 102 222 0 3896)
  (def-z vaesdeclast-masked 102 223 0 3896))


(macrolet ((def-bcast (name prefix opcode w)
             (let ((disp-n
                    (if (= w 0)
                        4
                        8))
                   (bcast-list
                    (if (= w 0)
                        '((0 "{1to4}") (1 "{1to8}") (2 "{1to16}"))
                        '((0 "{1to2}") (1 "{1to4}") (2 "{1to8}")))))
               `(define-instruction ,name (segment dst src1 src2)
                 ,@(loop for (ll bcast) in bcast-list
                         append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                                 opcode :opcode-prefix 15 :w w :nds t :ll ll
                                 :disp-n disp-n :evex-b 1 :printer
                                 (list :name :tab 'reg ", " 'vvvv ", " 'reg/mem
                                       " " bcast)))
                 (:emitter (aver (not (register-p src2)))
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                    :opcode-prefix 15 :vvvv src1 :w ,w :evex-b
                                    1 :disp-n ,disp-n))))))
  (def-bcast vaddps-bcast nil 88 0)
  (def-bcast vmulps-bcast nil 89 0)
  (def-bcast vaddpd-bcast 102 88 1)
  (def-bcast vmulpd-bcast 102 89 1)
  (def-bcast vsubps-bcast nil 92 0)
  (def-bcast vsubpd-bcast 102 92 1)
  (def-bcast vdivps-bcast nil 94 0)
  (def-bcast vdivpd-bcast 102 94 1)
  (def-bcast vminps-bcast nil 93 0)
  (def-bcast vmaxps-bcast nil 95 0)
  (def-bcast vminpd-bcast 102 93 1)
  (def-bcast vmaxpd-bcast 102 95 1))


(macrolet ((def-ibcast (name opcode w)
             (let ((disp-n
                    (if (= w 0)
                        4
                        8))
                   (bcast-list
                    (if (= w 0)
                        '((0 "{1to4}") (1 "{1to8}") (2 "{1to16}"))
                        '((0 "{1to2}") (1 "{1to4}") (2 "{1to8}")))))
               `(define-instruction ,name (segment dst src1 src2)
                 ,@(loop for (ll bcast) in bcast-list
                         append (avx512-inst-printer-list 'ymm-ymm/mem 102
                                 opcode :opcode-prefix 15 :w w :nds t :ll ll
                                 :disp-n disp-n :evex-b 1 :printer
                                 (list :name :tab 'reg ", " 'vvvv ", " 'reg/mem
                                       " " bcast)))
                 (:emitter (aver (not (register-p src2)))
                  (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                    15 :vvvv src1 :w ,w :evex-b 1 :disp-n
                                    ,disp-n))))))
  (def-ibcast vpaddd-bcast 254 0)
  (def-ibcast vpaddq-bcast 212 1)
  (def-ibcast vpsubd-bcast 250 0)
  (def-ibcast vpsubq-bcast 251 1)
  (def-ibcast vpandd-bcast 219 0)
  (def-ibcast vpandq-bcast 219 1)
  (def-ibcast vpandnd-bcast 223 0)
  (def-ibcast vpandnq-bcast 223 1)
  (def-ibcast vpord-bcast 235 0)
  (def-ibcast vporq-bcast 235 1)
  (def-ibcast vpxord-bcast 239 0)
  (def-ibcast vpxorq-bcast 239 1))


(macrolet ((def-ibcast-38 (name opcode w)
             (let ((disp-n
                    (if (= w 0)
                        4
                        8))
                   (bcast-list
                    (if (= w 0)
                        '((0 "{1to4}") (1 "{1to8}") (2 "{1to16}"))
                        '((0 "{1to2}") (1 "{1to4}") (2 "{1to8}")))))
               `(define-instruction ,name (segment dst src1 src2)
                 ,@(loop for (ll bcast) in bcast-list
                         append (avx512-inst-printer-list 'ymm-ymm/mem 102
                                 opcode :opcode-prefix 3896 :w w :nds t :ll ll
                                 :disp-n disp-n :evex-b 1 :printer
                                 (list :name :tab 'reg ", " 'vvvv ", " 'reg/mem
                                       " " bcast)))
                 (:emitter (aver (not (register-p src2)))
                  (emit-avx512-inst segment src2 dst 102 ,opcode :opcode-prefix
                                    3896 :vvvv src1 :w ,w :evex-b 1 :disp-n
                                    ,disp-n))))))
  (def-ibcast-38 vpminsd-bcast 57 0)
  (def-ibcast-38 vpmaxsd-bcast 61 0)
  (def-ibcast-38 vpminud-bcast 59 0)
  (def-ibcast-38 vpmaxud-bcast 63 0)
  (def-ibcast-38 vpminsq-bcast 57 1)
  (def-ibcast-38 vpmaxsq-bcast 61 1)
  (def-ibcast-38 vpminuq-bcast 59 1)
  (def-ibcast-38 vpmaxuq-bcast 63 1)
  (def-ibcast-38 vfmadd132ps-bcast 152 0)
  (def-ibcast-38 vfmadd132pd-bcast 152 1)
  (def-ibcast-38 vfmadd213ps-bcast 168 0)
  (def-ibcast-38 vfmadd213pd-bcast 168 1)
  (def-ibcast-38 vfmadd231ps-bcast 184 0)
  (def-ibcast-38 vfmadd231pd-bcast 184 1)
  (def-ibcast-38 vfmsub132ps-bcast 154 0)
  (def-ibcast-38 vfmsub132pd-bcast 154 1)
  (def-ibcast-38 vfmsub213ps-bcast 170 0)
  (def-ibcast-38 vfmsub213pd-bcast 170 1)
  (def-ibcast-38 vfmsub231ps-bcast 186 0)
  (def-ibcast-38 vfmsub231pd-bcast 186 1)
  (def-ibcast-38 vfnmadd132ps-bcast 156 0)
  (def-ibcast-38 vfnmadd132pd-bcast 156 1)
  (def-ibcast-38 vfnmadd213ps-bcast 172 0)
  (def-ibcast-38 vfnmadd213pd-bcast 172 1)
  (def-ibcast-38 vfnmadd231ps-bcast 188 0)
  (def-ibcast-38 vfnmadd231pd-bcast 188 1)
  (def-ibcast-38 vfnmsub132ps-bcast 158 0)
  (def-ibcast-38 vfnmsub132pd-bcast 158 1)
  (def-ibcast-38 vfnmsub213ps-bcast 174 0)
  (def-ibcast-38 vfnmsub213pd-bcast 174 1)
  (def-ibcast-38 vfnmsub231ps-bcast 190 0)
  (def-ibcast-38 vfnmsub231pd-bcast 190 1)
  (def-ibcast-38 vfmaddsub132ps-bcast 150 0)
  (def-ibcast-38 vfmaddsub132pd-bcast 150 1)
  (def-ibcast-38 vfmaddsub213ps-bcast 166 0)
  (def-ibcast-38 vfmaddsub213pd-bcast 166 1)
  (def-ibcast-38 vfmaddsub231ps-bcast 182 0)
  (def-ibcast-38 vfmaddsub231pd-bcast 182 1)
  (def-ibcast-38 vfmsubadd132ps-bcast 151 0)
  (def-ibcast-38 vfmsubadd132pd-bcast 151 1)
  (def-ibcast-38 vfmsubadd213ps-bcast 167 0)
  (def-ibcast-38 vfmsubadd213pd-bcast 167 1)
  (def-ibcast-38 vfmsubadd231ps-bcast 183 0)
  (def-ibcast-38 vfmsubadd231pd-bcast 183 1)
  (def-ibcast-38 vpdpbusd-bcast 80 0)
  (def-ibcast-38 vpdpbusds-bcast 81 0)
  (def-ibcast-38 vpdpwssd-bcast 82 0)
  (def-ibcast-38 vpdpwssds-bcast 83 0)
  (def-ibcast-38 vscalefps-bcast 44 0)
  (def-ibcast-38 vscalefpd-bcast 44 1))


(macrolet ((def-bcast-3a (name prefix opcode w &key (nds nil))
             (let* ((disp-n
                     (if (= w 0)
                         4
                         8))
                    (bcast-list
                     (if (= w 0)
                         '((0 "{1to4}") (1 "{1to8}") (2 "{1to16}"))
                         '((0 "{1to2}") (1 "{1to4}") (2 "{1to8}"))))
                    (printer-forms
                     (loop for (ll bcast) in bcast-list
                           append (avx512-inst-printer-list 'ymm-ymm/mem-imm
                                   prefix opcode :opcode-prefix 3898 :w w :nds
                                   nds :ll ll :disp-n disp-n :evex-b 1 :printer
                                   (if nds
                                       (list :name :tab 'reg ", " 'vvvv ", "
                                             'reg/mem " " bcast)
                                       (list :name :tab 'reg ", " 'reg/mem " "
                                             bcast))))))
               `(define-instruction ,name
                 ,(if nds
                      `(segment dst src1 src2 imm)
                      `(segment dst src imm))
                 ,@printer-forms
                 (:emitter
                  ,(if nds
                       `(progn
                         (aver (not (register-p src2)))
                         (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                           :opcode-prefix 3898 :vvvv src1 :w ,w
                                           :evex-b 1 :disp-n ,disp-n
                                           :remaining-bytes 1)
                         (emit-byte segment imm))
                       `(progn
                         (aver (not (register-p src)))
                         (emit-avx512-inst segment src dst ,prefix ,opcode
                                           :opcode-prefix 3898 :w ,w :evex-b 1
                                           :disp-n ,disp-n :remaining-bytes 1)
                         (emit-byte segment imm))))))))
  (def-bcast-3a vreduceps-bcast 102 86 0)
  (def-bcast-3a vreducepd-bcast 102 86 1)
  (def-bcast-3a vrndscaleps-bcast 102 8 0)
  (def-bcast-3a vrndscalepd-bcast 102 9 1)
  (def-bcast-3a vfixupimmps-bcast 102 84 0 :nds t)
  (def-bcast-3a vfixupimmpd-bcast 102 84 1 :nds t))


(macrolet ((def (name opcode w)
             (let ((vsib-arg-type
                    (if (= w 0)
                        'evex-vsib-disp4
                        'evex-vsib-disp8)))
               `(define-instruction ,name (segment dst vm mask)
                 ,@(loop for k from 1 to 7
                         append (loop for (ll n) in '((0 16) (1 32) (2 64))
                                      append (avx512-inst-printer-list
                                              'ymm-ymm/mem 102 opcode
                                              :opcode-prefix 3896 :w w :nds t
                                              :ll ll :disp-n n :more-fields
                                              `((aaa ,k) (z-bit 0)
                                                (reg/mem nil :type
                                                 ',vsib-arg-type))
                                              :printer
                                              '(:name :tab reg ", " reg/mem
                                                " {" aaa "}"))))
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment vm dst 102 ,opcode :opcode-prefix
                                    3896 :w ,w :aaa mask :vm t :disp-n
                                    ,(if (= w 0)
                                         4
                                         8)))))))
  (def vpgatherdd-z 144 0)
  (def vpgatherdq-z 144 1)
  (def vgatherdps-z 146 0)
  (def vgatherdpd-z 146 1)
  (def vpgatherqd-z 145 0)
  (def vpgatherqq-z 145 1)
  (def vgatherqps-z 147 0)
  (def vgatherqpd-z 147 1))


(macrolet ((def-zero (name opcode w)
             (let ((zero-name (symbolicate name "-ZERO"))
                   (vsib-arg-type
                    (if (= w 0)
                        'evex-vsib-disp4
                        'evex-vsib-disp8)))
               `(define-instruction ,zero-name (segment dst vm mask)
                 ,@(loop for k from 1 to 7
                         append (loop for (ll n) in '((0 16) (1 32) (2 64))
                                      append (avx512-inst-printer-list
                                              'ymm-ymm/mem 102 opcode
                                              :opcode-prefix 3896 :w w :nds t
                                              :ll ll :disp-n n :more-fields
                                              `((aaa ,k) (z-bit 1)
                                                (reg/mem nil :type
                                                 ',vsib-arg-type))
                                              :printer
                                              '(:name :tab reg ", " reg/mem
                                                " {" aaa "}{z}"))))
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment vm dst 102 ,opcode :opcode-prefix
                                    3896 :w ,w :aaa mask :vm t :z 1 :disp-n
                                    ,(if (= w 0)
                                         4
                                         8)))))))
  (def-zero vpgatherdd-z 144 0)
  (def-zero vpgatherdq-z 144 1)
  (def-zero vgatherdps-z 146 0)
  (def-zero vgatherdpd-z 146 1)
  (def-zero vpgatherqd-z 145 0)
  (def-zero vpgatherqq-z 145 1)
  (def-zero vgatherqps-z 147 0)
  (def-zero vgatherqpd-z 147 1))


(macrolet ((def (name opcode w)
             (let ((vsib-arg-type
                    (if (= w 0)
                        'evex-vsib-disp4
                        'evex-vsib-disp8)))
               `(define-instruction ,name (segment vm src mask)
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list
                           'ymm-ymm/mem #x66 opcode
                           :opcode-prefix #x0f38
                           :w w
                           :nds 'to-mem
                           :more-fields `((aaa ,k)
                                          (reg/mem nil :type ',vsib-arg-type))
                           :printer '(:name :tab reg/mem ", " reg " {" aaa "}")))
                  (:emitter
                   (aver (and (integerp mask) (<= 1 mask 7)))
                   (emit-avx512-inst segment vm src #x66 ,opcode
                                     :opcode-prefix #x0f38
                                     :w ,w
                                     :aaa mask
                                     :vm t
                                     :disp-n ,(if (= w 0) 4 8)))))))
  (def vpscatterdd-z  #xa0 0)
  (def vpscatterdq-z  #xa0 1)
  (def vscatterdps-z  #xa2 0)
  (def vscatterdpd-z  #xa2 1)
  (def vpscatterqd-z  #xa1 0)
  (def vpscatterqq-z  #xa1 1)
  (def vscatterqps-z  #xa3 0)
  (def vscatterqpd-z  #xa3 1))

;;;; ---- AVX-512CD instructions ----

;;; Conflict detection (2-operand)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n (full-vector-disp-n dst))))))
  (def vpconflictd #xc4 0)
  (def vpconflictq #xc4 1)
  (def vplzcntd    #x44 0)
  (def vplzcntq    #x44 1))

;;; Conflict detection with opmask
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src mask &optional (zeroing 0))
                ,@(loop for k from 1 to 7
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38
                         :w w
                         :more-fields `((aaa ,k) (z-bit 0))
                         :printer '(:name :tab reg ", " reg/mem " {" aaa "}"))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38
                         :w w
                         :more-fields `((aaa ,k) (z-bit 1))
                         :printer '(:name :tab reg ", " reg/mem " {" aaa "} {z}")))
                (:emitter
                 (let ((mask-num (cond ((integerp mask) mask)
                                       ((k-register-p mask) (reg-id-num (reg-id mask)))
                                       (t (error "Invalid mask ~S" mask))))
                       (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                   (emit-avx512-inst segment src dst #x66 ,opcode
                                     :opcode-prefix #x0f38
                                     :w ,w
                                     :aaa mask-num
                                     :z z-num
                                     :disp-n (full-vector-disp-n dst)))))))
  (def vpconflictd-masked #xc4 0)
  (def vpconflictq-masked #xc4 1)
  (def vplzcntd-masked    #x44 0)
  (def vplzcntq-masked    #x44 1))

;;;; ---- AVX-512VNNI instructions ----

;;; Vector Neural Network Instructions (3-operand NDS)
(macrolet ((def (name opcode)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w 0 :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w 0
                                   :disp-n (full-vector-disp-n dst))))))
  (def vpdpbusd   #x50)
  (def vpdpbusds  #x51)
  (def vpdpwssd   #x52)
  (def vpdpwssds  #x53))

;;; Vector Neural Network Instructions with opmask
(macrolet ((def (name opcode)
             `(define-instruction ,name (segment dst src1 src2 mask &optional (zeroing 0))
                ,@(loop for k from 1 to 7
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38
                         :w 0
                         :nds t
                         :more-fields `((aaa ,k) (z-bit 0))
                         :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}"))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38
                         :w 0
                         :nds t
                         :more-fields `((aaa ,k) (z-bit 1))
                         :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "} {z}")))
                (:emitter
                 (let ((mask-num (cond ((integerp mask) mask)
                                       ((k-register-p mask) (reg-id-num (reg-id mask)))
                                       (t (error "Invalid mask ~S" mask))))
                       (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                   (emit-avx512-inst segment src2 dst #x66 ,opcode
                                     :opcode-prefix #x0f38
                                     :vvvv src1
                                     :w 0
                                     :aaa mask-num
                                     :z z-num
                                     :disp-n (full-vector-disp-n dst)))))))
  (def vpdpbusd-masked   #x50)
  (def vpdpbusds-masked  #x51)
  (def vpdpwssd-masked   #x52)
  (def vpdpwssds-masked  #x53))

;;;; ---- AVX-512BF16 instructions ----

;;; Convert two single-precision vectors to bfloat16 (3-operand NDS)
(macrolet ((def (name opcode prefix)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix #x0f38 :w 0 :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w 0
                                   :disp-n (full-vector-disp-n dst))))))
  (def vcvtne2ps2bf16 #x72 #xf2)
  (def vdpbf16ps      #x52 #xf3))

;;; 3-operand BF16 with opmask
(macrolet ((def (name opcode prefix)
             `(define-instruction ,name (segment dst src1 src2 mask &optional (zeroing 0))
                ,@(loop for k from 1 to 7
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix #x0f38
                         :w 0
                         :nds t
                         :more-fields `((aaa ,k) (z-bit 0))
                         :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}"))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix #x0f38
                         :w 0
                         :nds t
                         :more-fields `((aaa ,k) (z-bit 1))
                         :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "} {z}")))
                (:emitter
                 (let ((mask-num (cond ((integerp mask) mask)
                                       ((k-register-p mask) (reg-id-num (reg-id mask)))
                                       (t (error "Invalid mask ~S" mask))))
                       (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                   (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                     :opcode-prefix #x0f38
                                     :vvvv src1
                                     :w 0
                                     :aaa mask-num
                                     :z z-num
                                     :disp-n (full-vector-disp-n dst)))))))
  (def vcvtne2ps2bf16-masked #x72 #xf2)
  (def vdpbf16ps-masked      #x52 #xf3))

;;; Convert single-precision vector to bfloat16 (2-operand)
(macrolet ((def ()
             `(define-instruction vcvtneps2bf16 (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #xf3 #x72
                                            :opcode-prefix #x0f38 :w 0)
                (:emitter
                 (let ((ll (cond ((or (zmm-register-p src) (ymm-register-p dst)) #b10)
                                 ((ymm-register-p src) #b01)
                                 (t #b00)))
                       (disp-n (if (or (zmm-register-p src) (ymm-register-p dst)) 64 (full-vector-disp-n dst))))
                   (emit-avx512-inst segment src dst #xf3 #x72
                                     :opcode-prefix #x0f38
                                     :w 0
                                     :ll ll
                                     :disp-n disp-n))))))
  (def))

(macrolet ((def ()
             `(define-instruction vcvtneps2bf16-masked (segment dst src mask &optional (zeroing 0))
                ,@(loop for k from 1 to 7
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #xf3 #x72
                         :opcode-prefix #x0f38
                         :w 0
                         :more-fields `((aaa ,k) (z-bit 0))
                         :printer '(:name :tab reg ", " reg/mem " {" aaa "}"))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #xf3 #x72
                         :opcode-prefix #x0f38
                         :w 0
                         :more-fields `((aaa ,k) (z-bit 1))
                         :printer '(:name :tab reg ", " reg/mem " {" aaa "} {z}")))
                (:emitter
                 (let ((mask-num (cond ((integerp mask) mask)
                                       ((k-register-p mask) (reg-id-num (reg-id mask)))
                                       (t (error "Invalid mask ~S" mask))))
                       (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0))
                       (ll (cond ((or (zmm-register-p src) (ymm-register-p dst)) #b10)
                                 ((ymm-register-p src) #b01)
                                 (t #b00)))
                       (disp-n (if (or (zmm-register-p src) (ymm-register-p dst)) 64 (full-vector-disp-n dst))))
                   (emit-avx512-inst segment src dst #xf3 #x72
                                     :opcode-prefix #x0f38
                                     :w 0
                                     :ll ll
                                     :aaa mask-num
                                     :z z-num
                                     :disp-n disp-n))))))
  (def))

;;;; ---- AVX-512_FP16 instructions ----

;;; 3-operand vector arithmetic (Map 5 & Map 6)
(macrolet ((def (name opcode &optional (opcode-prefix :map5) (prefix nil) (w 0))
             (let ((masked-name (symbolicate name "-MASKED")))
               `(progn
                  (define-instruction ,name (segment dst src1 src2 &optional mask (zeroing 0))
                    ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                                :opcode-prefix opcode-prefix :w w :nds t)
                    ,@(loop for k from 1 to 7
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w :nds t
                             :more-fields `((aaa ,k) (z-bit 0))
                             :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}"))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w :nds t
                             :more-fields `((aaa ,k) (z-bit 1))
                             :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "} {z}")))
                    (:emitter
                     (let ((mask-num (cond ((null mask) 0)
                                           ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :vvvv src1
                                         :w ,w
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n (full-vector-disp-n dst)))))
                  (define-instruction ,masked-name (segment dst src1 src2 mask &optional (zeroing 0))
                    (:emitter
                     (let ((mask-num (cond ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :vvvv src1
                                         :w ,w
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n (full-vector-disp-n dst)))))))))
  ;; Map 5 arithmetic
  (def vaddph #x58)
  (def vsubph #x5c)
  (def vmulph #x59)
  (def vdivph #x5e)
  (def vminph #x5d)
  (def vmaxph #x5f)
  ;; Map 6 scalef
  (def vscalefph #x2c :map6 #x66)
  ;; Map 6 FMA (vector)
  (def vfmadd132ph #x98 :map6 #x66)
  (def vfmadd213ph #xa8 :map6 #x66)
  (def vfmadd231ph #xb8 :map6 #x66)
  (def vfmsub132ph #x9a :map6 #x66)
  (def vfmsub213ph #xaa :map6 #x66)
  (def vfmsub231ph #xba :map6 #x66)
  (def vfnmadd132ph #x9c :map6 #x66)
  (def vfnmadd213ph #xac :map6 #x66)
  (def vfnmadd231ph #xbc :map6 #x66)
  (def vfnmsub132ph #x9e :map6 #x66)
  (def vfnmsub213ph #xae :map6 #x66)
  (def vfnmsub231ph #xbe :map6 #x66)
  (def vfmaddsub132ph #x96 :map6 #x66)
  (def vfmaddsub213ph #xa6 :map6 #x66)
  (def vfmaddsub231ph #xb6 :map6 #x66)
  (def vfmsubadd132ph #x97 :map6 #x66)
  (def vfmsubadd213ph #xa7 :map6 #x66)
  (def vfmsubadd231ph #xb7 :map6 #x66)
  ;; Map 6 Complex FMA
  (def vfcmaddcph #x56 :map6 #xf2)
  (def vfmaddcph  #x56 :map6 #xf3))

;;; 2-operand vector arithmetic (sqrt, rcp, rsqrt)
(macrolet ((def (name opcode &optional (opcode-prefix :map5) (prefix nil) (w 0))
             (let ((masked-name (symbolicate name "-MASKED")))
               `(progn
                  (define-instruction ,name (segment dst src &optional mask (zeroing 0))
                    ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                                :opcode-prefix opcode-prefix :w w)
                    ,@(loop for k from 1 to 7
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w
                             :more-fields `((aaa ,k) (z-bit 0))
                             :printer '(:name :tab reg ", " reg/mem " {" aaa "}"))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w
                             :more-fields `((aaa ,k) (z-bit 1))
                             :printer '(:name :tab reg ", " reg/mem " {" aaa "} {z}")))
                    (:emitter
                     (let ((mask-num (cond ((null mask) 0)
                                           ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :w ,w
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n (full-vector-disp-n dst)))))
                  (define-instruction ,masked-name (segment dst src mask &optional (zeroing 0))
                    (:emitter
                     (let ((mask-num (cond ((null mask) 0)
                                           ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :w ,w
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n (full-vector-disp-n dst)))))))))
  (def vsqrtph  #x51 :map5 nil)
  (def vrcpph   #x4c :map6 #x66)
  (def vrsqrtph #x4e :map6 #x66))

;;; Scalar arithmetic (Map 5 & Map 6, 3-operand, ll=0, disp-n=2)
(macrolet ((def (name opcode &optional (opcode-prefix :map5) (prefix #xf3) (w 0))
             (let ((masked-name (symbolicate name "-MASKED")))
               `(progn
                  (define-instruction ,name (segment dst src1 src2 &optional mask (zeroing 0))
                    ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                                :opcode-prefix opcode-prefix :w w :ll 0 :nds t)
                    ,@(loop for k from 1 to 7
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w :ll 0 :nds t
                             :more-fields `((aaa ,k) (z-bit 0))
                             :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}"))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w :ll 0 :nds t
                             :more-fields `((aaa ,k) (z-bit 1))
                             :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "} {z}")))
                    (:emitter
                     (let ((mask-num (cond ((null mask) 0)
                                           ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :vvvv src1
                                         :w ,w
                                         :ll 0
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n 2))))
                  (define-instruction ,masked-name (segment dst src1 src2 mask &optional (zeroing 0))
                    (:emitter
                     (let ((mask-num (cond ((null mask) 0)
                                           ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :vvvv src1
                                         :w ,w
                                         :ll 0
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n 2))))))))
  (def vaddsh #x58)
  (def vsubsh #x5c)
  (def vmulsh #x59)
  (def vdivsh #x5e)
  (def vminsh #x5d)
  (def vmaxsh #x5f)
  (def vsqrtsh #x51)
  (def vrcpsh #x4d :map6 #x66)
  (def vrsqrtsh #x4f :map6 #x66)
  (def vscalefsh #x2d :map6 #x66)
  ;; Scalar FMA
  (def vfmadd132sh #x99 :map6 #x66)
  (def vfmadd213sh #xa9 :map6 #x66)
  (def vfmadd231sh #xb9 :map6 #x66)
  (def vfmsub132sh #x9b :map6 #x66)
  (def vfmsub213sh #xab :map6 #x66)
  (def vfmsub231sh #xbb :map6 #x66)
  (def vfnmadd132sh #x9d :map6 #x66)
  (def vfnmadd213sh #xad :map6 #x66)
  (def vfnmadd231sh #xbd :map6 #x66)
  (def vfnmsub132sh #x9f :map6 #x66)
  (def vfnmsub213sh #xaf :map6 #x66)
  (def vfnmsub231sh #xbf :map6 #x66))

;;; Comparisons & Classification
(macrolet ((def-cmp (name prefix name-suffix &key scalar)
             `(define-instruction ,name (segment condition dst src src2 &optional mask)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm prefix #xc2
                                            :opcode-prefix #x0f3a
                                            :w 0
                                            :ll (if scalar 0 nil)
                                            :more-fields `((reg nil :type 'opmask-reg)
                                                           (imm nil :type 'avx-condition-code))
                                            :printer `("VCMP" imm ,name-suffix
                                                              :tab reg ", " vvvv ", " reg/mem))
                ,@(loop for k from 1 to 7
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem-imm prefix #xc2
                         :opcode-prefix #x0f3a
                         :w 0
                         :ll (if scalar 0 nil)
                         :more-fields `((reg nil :type 'opmask-reg)
                                        (imm nil :type 'avx-condition-code)
                                        (aaa ,k))
                         :printer `("VCMP" imm ,name-suffix
                                           :tab reg " {" aaa "}, " vvvv ", " reg/mem)))
                (:emitter
                 (multiple-value-bind (cond-arg dst-reg src1 src2-arg mask-val)
                     (if (register-p condition)
                         (values src2 condition dst src (or mask 0))
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
                     (aver (k-register-p dst-reg))
                     (let ((disp-n ,(if scalar
                                        2
                                        `(cond ((zmm-register-p src1) 64)
                                               ((ymm-register-p src1) 32)
                                               (t 16)))))
                       (emit-avx512-inst segment src2-arg dst-reg ,prefix #xc2
                                         :opcode-prefix #x0f3a
                                         :vvvv src1
                                         :w 0
                                         :ll ,(if scalar 0 nil)
                                         :aaa mask-num
                                         :disp-n disp-n
                                         :remaining-bytes 1)
                       (emit-byte segment imm))))))))
  (def-cmp vcmpph nil  "PH")
  (def-cmp vcmpsh #xf3 "SH" :scalar t))

(macrolet ((def ()
             `(progn
                (define-instruction vcomish (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem nil #x2f :opcode-prefix :map5 :w 0 :ll 0)
                  (:emitter
                   (emit-avx512-inst segment src dst nil #x2f
                                     :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2)))

                (define-instruction vucomish (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem nil #x2e :opcode-prefix :map5 :w 0 :ll 0)
                  (:emitter
                   (emit-avx512-inst segment src dst nil #x2e
                                     :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2)))

                (define-instruction vfpclassph (segment dst src imm &optional mask)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x66 :opcode-prefix #x0f3a :w 0
                                              :more-fields '((reg nil :type 'opmask-reg))
                                              :printer '(:name :tab reg ", " reg/mem ", " imm))
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x66 :opcode-prefix #x0f3a :w 0
                                                    :more-fields `((reg nil :type 'opmask-reg) (aaa ,k))
                                                    :printer '(:name :tab reg " {" aaa "}, " reg/mem ", " imm)))
                  (:emitter
                   (let ((mask-num (cond ((null mask) 0)
                                         ((integerp mask) mask)
                                         ((k-register-p mask) (reg-id-num (reg-id mask)))
                                         (t 0))))
                     (emit-avx512-inst segment src dst nil #x66
                                       :opcode-prefix #x0f3a
                                       :w 0
                                       :aaa mask-num
                                       :disp-n (full-vector-disp-n src)
                                       :remaining-bytes 1)
                     (emit-byte segment imm))))

                (define-instruction vfpclasssh (segment dst src imm &optional mask)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x67 :opcode-prefix #x0f3a :w 0 :ll 0
                                              :more-fields '((reg nil :type 'opmask-reg))
                                              :printer '(:name :tab reg ", " reg/mem ", " imm))
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x67 :opcode-prefix #x0f3a :w 0 :ll 0
                                                    :more-fields `((reg nil :type 'opmask-reg) (aaa ,k))
                                                    :printer '(:name :tab reg " {" aaa "}, " reg/mem ", " imm)))
                  (:emitter
                   (let ((mask-num (cond ((null mask) 0)
                                         ((integerp mask) mask)
                                         ((k-register-p mask) (reg-id-num (reg-id mask)))
                                         (t 0))))
                     (emit-avx512-inst segment src dst nil #x67
                                       :opcode-prefix #x0f3a
                                       :w 0
                                       :ll 0
                                       :aaa mask-num
                                       :disp-n 2
                                       :remaining-bytes 1)
                     (emit-byte segment imm))))

                (define-instruction vrndscaleph (segment dst src imm &optional mask (zeroing 0))
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x08 :opcode-prefix #x0f3a :w 0
                                              :printer '(:name :tab reg ", " reg/mem ", " imm))
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x08 :opcode-prefix #x0f3a :w 0
                                                    :more-fields `((aaa ,k) (z-bit 0))
                                                    :printer '(:name :tab reg ", " reg/mem ", " imm " {" aaa "}"))
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x08 :opcode-prefix #x0f3a :w 0
                                                    :more-fields `((aaa ,k) (z-bit 1))
                                                    :printer '(:name :tab reg ", " reg/mem ", " imm " {" aaa "} {z}")))
                  (:emitter
                   (let ((mask-num (cond ((null mask) 0)
                                         ((integerp mask) mask)
                                         ((k-register-p mask) (reg-id-num (reg-id mask)))
                                         (t 0)))
                         (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                     (emit-avx512-inst segment src dst nil #x08
                                       :opcode-prefix #x0f3a
                                       :w 0
                                       :aaa mask-num
                                       :z z-num
                                       :disp-n (full-vector-disp-n dst)
                                       :remaining-bytes 1)
                     (emit-byte segment imm))))

                (define-instruction vrndscalesh (segment dst src1 src2 imm &optional mask (zeroing 0))
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x0a :opcode-prefix #x0f3a :w 0 :ll 0
                                              :printer '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x0a :opcode-prefix #x0f3a :w 0 :ll 0
                                                    :more-fields `((aaa ,k) (z-bit 0))
                                                    :printer '(:name :tab reg ", " vvvv ", " reg/mem ", " imm " {" aaa "}"))
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x0a :opcode-prefix #x0f3a :w 0 :ll 0
                                                    :more-fields `((aaa ,k) (z-bit 1))
                                                    :printer '(:name :tab reg ", " vvvv ", " reg/mem ", " imm " {" aaa "} {z}")))
                  (:emitter
                   (let ((mask-num (cond ((null mask) 0)
                                         ((integerp mask) mask)
                                         ((k-register-p mask) (reg-id-num (reg-id mask)))
                                         (t 0)))
                         (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                     (emit-avx512-inst segment src2 dst nil #x0a
                                       :opcode-prefix #x0f3a
                                       :vvvv src1
                                       :w 0
                                       :ll 0
                                       :aaa mask-num
                                       :z z-num
                                       :disp-n 2
                                       :remaining-bytes 1)
                     (emit-byte segment imm)))))))
  (def))

;;; Conversions between FP16 and single-precision float (vcvtph2psx, vcvtps2phx)
(macrolet ((def ()
             `(progn
                (define-instruction vcvtph2psx (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x13 :opcode-prefix :map6 :w 0)
                  (:emitter
                   (let ((ll (cond ((zmm-register-p dst) #b10)
                                   ((ymm-register-p dst) #b01)
                                   (t #b00)))
                         (disp-n (cond ((zmm-register-p dst) 32)
                                       ((ymm-register-p dst) 16)
                                       (t 8))))
                     (emit-avx512-inst segment src dst #x66 #x13
                                       :opcode-prefix :map6
                                       :w 0
                                       :ll ll
                                       :disp-n disp-n))))

                (define-instruction vcvtps2phx (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x1d :opcode-prefix :map5 :w 0)
                  (:emitter
                   (let ((ll (cond ((zmm-register-p src) #b10)
                                   ((ymm-register-p src) #b01)
                                   (t #b00)))
                         (disp-n (cond ((zmm-register-p src) 64)
                                       ((ymm-register-p src) 32)
                                       (t 16))))
                     (emit-avx512-inst segment src dst #x66 #x1d
                                       :opcode-prefix :map5
                                       :w 0
                                       :ll ll
                                       :disp-n disp-n))))

                (define-instruction vcvtdq2ph (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem nil #x5b :opcode-prefix :map5 :w 0)
                  (:emitter
                   (let ((ll (cond ((zmm-register-p src) #b10)
                                   ((ymm-register-p src) #b01)
                                   (t #b00)))
                         (disp-n (cond ((zmm-register-p src) 64)
                                       ((ymm-register-p src) 32)
                                       (t 16))))
                     (emit-avx512-inst segment src dst nil #x5b
                                       :opcode-prefix :map5
                                       :w 0
                                       :ll ll
                                       :disp-n disp-n)))))))
  (def))

(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode :opcode-prefix :map5 :w 0)
                (:emitter
                 (let ((ll (cond ((zmm-register-p dst) #b10)
                                 ((ymm-register-p dst) #b01)
                                 (t #b00)))
                       (disp-n (cond ((zmm-register-p dst) 32)
                                     ((ymm-register-p dst) 16)
                                     (t 8))))
                   (emit-avx512-inst segment src dst ,prefix ,opcode
                                     :opcode-prefix :map5
                                     :w 0
                                     :ll ll
                                     :disp-n disp-n))))))
  (def vcvtph2dq  #x66 #x5b)
  (def vcvttph2dq #xf3 #x5b))

;;; Conversions between FP16 and 16-bit integers (1:1 width)
(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode :opcode-prefix :map5 :w 0)
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix :map5
                                   :w 0
                                   :disp-n (full-vector-disp-n dst))))))
  (def vcvtuw2ph  #xf2 #x7d)
  (def vcvtw2ph   #xf3 #x7d)
  (def vcvtph2w   #x66 #x7d)
  (def vcvtph2uw  nil  #x7d)
  (def vcvttph2w  #x66 #x7c)
  (def vcvttph2uw nil  #x7c))

;;; Scalar conversions
(macrolet ((def ()
             `(progn
                (define-instruction vcvtsd2sh (segment dst src1 src2)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #xf2 #x5a :opcode-prefix :map5 :w 1 :ll 0 :nds t)
                  (:emitter
                   (aver (xmm-register-p dst))
                   (emit-avx512-inst segment src2 dst #xf2 #x5a
                                     :opcode-prefix :map5
                                     :vvvv src1
                                     :w 1
                                     :ll 0
                                     :disp-n 8)))

                (define-instruction vcvtss2sh (segment dst src1 src2)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem nil #x1d :opcode-prefix :map5 :w 0 :ll 0 :nds t)
                  (:emitter
                   (aver (xmm-register-p dst))
                   (emit-avx512-inst segment src2 dst nil #x1d
                                     :opcode-prefix :map5
                                     :vvvv src1
                                     :w 0
                                     :ll 0
                                     :disp-n 4)))

                (define-instruction vcvtsi2sh (segment dst src1 src2)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #xf3 #x2a
                                              :opcode-prefix :map5
                                              :reg-mem-size :sized
                                              :nds t :ll 0)
                  (:emitter
                   (aver (xmm-register-p dst))
                   (let ((src-size (operand-size src2)))
                     (emit-avx512-inst segment src2 dst #xf3 #x2a
                                       :opcode-prefix :map5
                                       :ll 0
                                       :vvvv src1
                                       :w (case src-size
                                            (:qword 1)
                                            (:dword 0)
                                            (t 1))
                                       :disp-n (case src-size
                                                 (:qword 8)
                                                 (t 4)))))))))
  (def))

(macrolet ((def (name opcode)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'reg-ymm/mem #xf3 opcode
                                            :opcode-prefix :map5 :ll 0)
                (:emitter
                 (aver (gpr-p dst))
                 (let ((dst-size (operand-size dst)))
                   (aver (or (eq dst-size :qword) (eq dst-size :dword)))
                   (emit-avx512-inst segment src dst #xf3 ,opcode
                                     :opcode-prefix :map5
                                     :ll 0
                                     :w (ecase dst-size
                                          (:qword 1)
                                          (:dword 0))
                                     :disp-n 2))))))
  (def vcvtsh2si  #x2d)
  (def vcvttsh2si #x2c))

;;; FP16 moves (vmovsh, vmovw)
(macrolet ((def ()
             `(progn
                (define-instruction vmovsh (segment dst src &optional src2)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem-dir #xf3 #b0001000 :opcode-prefix :map5 :w 0 :ll 0)
                  (:emitter
                   (cond ((ea-p src)
                          (aver (xmm-register-p dst))
                          (emit-avx512-inst segment src dst #xf3 #x10
                                            :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2))
                         ((ea-p dst)
                          (aver (xmm-register-p src))
                          (emit-avx512-inst segment dst src #xf3 #x11
                                            :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2))
                         (src2
                          (aver (and (xmm-register-p dst) (xmm-register-p src) (xmm-register-p src2)))
                          (emit-avx512-inst segment src2 dst #xf3 #x10
                                            :opcode-prefix :map5 :w 0 :ll 0 :vvvv src))
                         (t
                          (aver (and (xmm-register-p dst) (xmm-register-p src)))
                          (emit-avx512-inst segment src dst #xf3 #x10
                                            :opcode-prefix :map5 :w 0 :ll 0 :vvvv dst)))))

                (define-instruction vmovw (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x6e :opcode-prefix :map5 :w 0 :ll 0
                                              :reg-mem-size :dword)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x7e :opcode-prefix :map5 :w 0 :ll 0
                                              :reg-mem-size :dword
                                              :printer '(:name :tab reg/mem ", " reg))
                  (:emitter
                   (cond ((gpr-p dst)
                          (aver (xmm-register-p src))
                          (emit-avx512-inst segment dst src #x66 #x7e
                                            :opcode-prefix :map5 :w 0 :ll 0))
                         ((gpr-p src)
                          (aver (xmm-register-p dst))
                          (emit-avx512-inst segment src dst #x66 #x6e
                                            :opcode-prefix :map5 :w 0 :ll 0))
                         ((ea-p dst)
                          (aver (xmm-register-p src))
                          (emit-avx512-inst segment dst src #x66 #x7e
                                            :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2))
                         ((ea-p src)
                          (aver (xmm-register-p dst))
                          (emit-avx512-inst segment src dst #x66 #x6e
                                            :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2))
                         (t
                          (error "Unsupported operands for VMOVW: ~S, ~S" dst src))))))))
  (def))
;;;; ---- VP2INTERSECT ----

(macrolet ((def (name opcode prefix w)
             `(define-instruction ,name (segment k1 k2 src1 src2)
               ,@(loop for k1num from 1 to 7
                       append (loop for k2num from 1 to 7
                                    append (avx512-inst-printer-list '2mask-nds
                                            prefix opcode :opcode-prefix 3896
                                            :w w :ll 2 :disp-n 64 :more-fields
                                            `((aaa ,k2num)
                                              (reg ,k1num :type 'kreg))
                                            :printer
                                            '(:name :tab reg ", " aaa ", " vvvv
                                              ", " reg/mem))))
               (:emitter (aver (k-register-p k1)) (aver (k-register-p k2))
                (aver (not (zerop (reg-id-num (reg-id k1)))))
                (aver (not (zerop (reg-id-num (reg-id k2)))))
                (emit-avx512-inst segment src2 k1 ,prefix ,opcode
                                  :opcode-prefix 3896 :w ,w :ll 2 :vvvv src1
                                  :aaa (reg-id-num (reg-id k2)) :disp-n 64)))))
  (def vp2intersectd 104 242 0)
  (def vp2intersectq 104 242 1))


(macrolet ((def-scalar-fma-masked (name prefix opcode w &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}{z}")
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}")))
                    (disp-n
                     (if (= w 0)
                         4
                         8))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                                   opcode :opcode-prefix 3896 :w w :nds t :ll 0
                                   :disp-n disp-n :evex-b 0 :more-fields
                                   (list (list 'aaa k) (list 'z-bit z-bit))
                                   :printer mask-printer))))
               `(define-instruction ,ins-name (segment dst src1 src2 mask)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                    :opcode-prefix 3896 :vvvv src1 :w ,w :aaa
                                    mask :z ,z-bit :disp-n ,disp-n))))))
  (def-scalar-fma-masked vfmadd132ss 102 153 0)
  (def-scalar-fma-masked vfmadd132sd 102 153 1)
  (def-scalar-fma-masked vfmadd213ss 102 169 0)
  (def-scalar-fma-masked vfmadd213sd 102 169 1)
  (def-scalar-fma-masked vfmadd231ss 102 185 0)
  (def-scalar-fma-masked vfmadd231sd 102 185 1)
  (def-scalar-fma-masked vfmsub132ss 102 155 0)
  (def-scalar-fma-masked vfmsub132sd 102 155 1)
  (def-scalar-fma-masked vfmsub213ss 102 171 0)
  (def-scalar-fma-masked vfmsub213sd 102 171 1)
  (def-scalar-fma-masked vfmsub231ss 102 187 0)
  (def-scalar-fma-masked vfmsub231sd 102 187 1)
  (def-scalar-fma-masked vfnmadd132ss 102 157 0)
  (def-scalar-fma-masked vfnmadd132sd 102 157 1)
  (def-scalar-fma-masked vfnmadd213ss 102 173 0)
  (def-scalar-fma-masked vfnmadd213sd 102 173 1)
  (def-scalar-fma-masked vfnmadd231ss 102 189 0)
  (def-scalar-fma-masked vfnmadd231sd 102 189 1)
  (def-scalar-fma-masked vfnmsub132ss 102 159 0)
  (def-scalar-fma-masked vfnmsub132sd 102 159 1)
  (def-scalar-fma-masked vfnmsub213ss 102 175 0)
  (def-scalar-fma-masked vfnmsub213sd 102 175 1)
  (def-scalar-fma-masked vfnmsub231ss 102 191 0)
  (def-scalar-fma-masked vfnmsub231sd 102 191 1)
  (def-scalar-fma-masked vfmadd132ss 102 153 0 :z t)
  (def-scalar-fma-masked vfmadd132sd 102 153 1 :z t)
  (def-scalar-fma-masked vfmadd213ss 102 169 0 :z t)
  (def-scalar-fma-masked vfmadd213sd 102 169 1 :z t)
  (def-scalar-fma-masked vfmadd231ss 102 185 0 :z t)
  (def-scalar-fma-masked vfmadd231sd 102 185 1 :z t)
  (def-scalar-fma-masked vfmsub132ss 102 155 0 :z t)
  (def-scalar-fma-masked vfmsub132sd 102 155 1 :z t)
  (def-scalar-fma-masked vfmsub213ss 102 171 0 :z t)
  (def-scalar-fma-masked vfmsub213sd 102 171 1 :z t)
  (def-scalar-fma-masked vfmsub231ss 102 187 0 :z t)
  (def-scalar-fma-masked vfmsub231sd 102 187 1 :z t)
  (def-scalar-fma-masked vfnmadd132ss 102 157 0 :z t)
  (def-scalar-fma-masked vfnmadd132sd 102 157 1 :z t)
  (def-scalar-fma-masked vfnmadd213ss 102 173 0 :z t)
  (def-scalar-fma-masked vfnmadd213sd 102 173 1 :z t)
  (def-scalar-fma-masked vfnmadd231ss 102 189 0 :z t)
  (def-scalar-fma-masked vfnmadd231sd 102 189 1 :z t)
  (def-scalar-fma-masked vfnmsub132ss 102 159 0 :z t)
  (def-scalar-fma-masked vfnmsub132sd 102 159 1 :z t)
  (def-scalar-fma-masked vfnmsub213ss 102 175 0 :z t)
  (def-scalar-fma-masked vfnmsub213sd 102 175 1 :z t)
  (def-scalar-fma-masked vfnmsub231ss 102 191 0 :z t)
  (def-scalar-fma-masked vfnmsub231sd 102 191 1 :z t)
  (def-scalar-fma-masked vrcp14ss 102 77 0)
  (def-scalar-fma-masked vrcp14sd 102 77 1)
  (def-scalar-fma-masked vrsqrt14ss 102 79 0)
  (def-scalar-fma-masked vrsqrt14sd 102 79 1)
  (def-scalar-fma-masked vrcp14ss 102 77 0 :z t)
  (def-scalar-fma-masked vrcp14sd 102 77 1 :z t)
  (def-scalar-fma-masked vrsqrt14ss 102 79 0 :z t)
  (def-scalar-fma-masked vrsqrt14sd 102 79 1 :z t)
  (def-scalar-fma-masked vgetexpss 102 67 0)
  (def-scalar-fma-masked vgetexpsd 102 67 1)
  (def-scalar-fma-masked vscalefss 102 45 0)
  (def-scalar-fma-masked vscalefsd 102 45 1)
  (def-scalar-fma-masked vgetexpss 102 67 0 :z t)
  (def-scalar-fma-masked vgetexpsd 102 67 1 :z t)
  (def-scalar-fma-masked vscalefss 102 45 0 :z t)
  (def-scalar-fma-masked vscalefsd 102 45 1 :z t))


(macrolet ((def-scalar-arith-masked (name prefix opcode w &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}{z}")
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}")))
                    (disp-n
                     (if (= w 0)
                         4
                         8))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                                   opcode :opcode-prefix 15 :w w :nds t :ll 0
                                   :disp-n disp-n :evex-b 0 :more-fields
                                   (list (list 'aaa k) (list 'z-bit z-bit))
                                   :printer mask-printer))))
               `(define-instruction ,ins-name (segment dst src1 src2 mask)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                    :opcode-prefix 15 :vvvv src1 :w ,w :aaa
                                    mask :z ,z-bit :disp-n ,disp-n))))))
  (def-scalar-arith-masked vaddsd 242 88 1)
  (def-scalar-arith-masked vaddsd 242 88 1 :z t)
  (def-scalar-arith-masked vaddss 243 88 0)
  (def-scalar-arith-masked vaddss 243 88 0 :z t))


(macrolet ((def-bf16-masked (name prefix opcode w &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}{z}")
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}")))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for ll in '(0 1 2)
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem prefix opcode
                                                :opcode-prefix 3896 :w w :nds t
                                                :ll ll :disp-n
                                                (case ll (0 16) (1 32) (2 64))
                                                :evex-b 0 :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src1 src2 mask)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                    :opcode-prefix 3896 :vvvv src1 :w ,w :aaa
                                    mask :z ,z-bit :disp-n
                                    (cond ((zmm-register-p dst) 64)
                                          ((ymm-register-p dst) 32)
                                          ((xmm-register-p dst) 16) (t 0))))))))
  (def-bf16-masked vcvtne2ps2bf16 242 114 0)
  (def-bf16-masked vdpbf16ps 242 82 0)
  (def-bf16-masked vcvtne2ps2bf16 242 114 0 :z t)
  (def-bf16-masked vdpbf16ps 242 82 0 :z t))


(macrolet ((def-bf16-2op-masked (name prefix opcode w &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg ", " reg/mem " {" aaa "}{z}")
                         '(:name :tab reg ", " reg/mem " {" aaa "}")))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for ll in '(0 1 2)
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem prefix opcode
                                                :opcode-prefix 3896 :w w :ll ll
                                                :disp-n
                                                (case ll (0 16) (1 32) (2 64))
                                                :evex-b 0 :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src mask)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment src dst ,prefix ,opcode
                                    :opcode-prefix 3896 :w ,w :aaa mask :z
                                    ,z-bit :disp-n
                                    (cond ((zmm-register-p dst) 64)
                                          ((ymm-register-p dst) 32)
                                          ((xmm-register-p dst) 16) (t 0))))))))
  (def-bf16-2op-masked vcvtneps2bf16 242 115 0)
  (def-bf16-2op-masked vcvtneps2bf16 242 115 0 :z t))


(macrolet ((def-range-masked (name prefix opcode w &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}{z}")
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}")))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for (ll n) in '((0 16) (1 32) (2 64))
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem-imm prefix opcode
                                                :opcode-prefix 3898 :w w :nds t
                                                :ll ll :disp-n n :evex-b 0
                                                :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src1 src2 mask imm)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                    :opcode-prefix 3898 :vvvv src1 :w ,w :aaa
                                    mask :z ,z-bit :disp-n
                                    (cond ((zmm-register-p dst) 64)
                                          ((ymm-register-p dst) 32)
                                          ((xmm-register-p dst) 16) (t 0))
                                    :remaining-bytes 1)
                  (emit-byte segment imm)))))
           (def-range-bcast (name prefix opcode w)
             (let* ((disp-n
                     (if (= w 0)
                         4
                         8))
                    (bcast-list
                     (if (= w 0)
                         '((0 "{1to4}") (1 "{1to8}") (2 "{1to16}"))
                         '((0 "{1to2}") (1 "{1to4}") (2 "{1to8}"))))
                    (printer-forms
                     (loop for (ll bcast) in bcast-list
                           append (avx512-inst-printer-list 'ymm-ymm/mem-imm
                                   prefix opcode :opcode-prefix 3898 :w w :nds
                                   t :ll ll :disp-n disp-n :evex-b 1 :printer
                                   (list :name :tab 'reg ", " 'vvvv ", "
                                         'reg/mem " " bcast)))))
               `(define-instruction ,name (segment dst src1 src2 imm)
                 ,@printer-forms
                 (:emitter (aver (not (register-p src2)))
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                    :opcode-prefix 3898 :vvvv src1 :w ,w
                                    :evex-b 1 :disp-n ,disp-n :remaining-bytes
                                    1)
                  (emit-byte segment imm))))))
  (def-range-masked vrangeps 102 80 0)
  (def-range-masked vrangepd 102 80 1)
  (def-range-masked vrangeps 102 80 0 :z t)
  (def-range-masked vrangepd 102 80 1 :z t)
  (def-range-bcast vrangeps-bcast 102 80 0)
  (def-range-bcast vrangepd-bcast 102 80 1)
  (def-range-masked vfixupimmps 102 84 0)
  (def-range-masked vfixupimmpd 102 84 1)
  (def-range-masked vfixupimmps 102 84 0 :z t)
  (def-range-masked vfixupimmpd 102 84 1 :z t)
  (def-range-masked vgf2p8affineqb 102 206 1)
  (def-range-masked vgf2p8affineinvqb 102 207 1)
  (def-range-masked vgf2p8affineqb 102 206 1 :z t)
  (def-range-masked vgf2p8affineinvqb 102 207 1 :z t)
  (def-range-masked vpshldw 102 112 1)
  (def-range-masked vpshldd 102 113 0)
  (def-range-masked vpshldq 102 113 1)
  (def-range-masked vpshrdw 102 114 1)
  (def-range-masked vpshrdd 102 115 0)
  (def-range-masked vpshrdq 102 115 1)
  (def-range-masked vpshldw 102 112 1 :z t)
  (def-range-masked vpshldd 102 113 0 :z t)
  (def-range-masked vpshldq 102 113 1 :z t)
  (def-range-masked vpshrdw 102 114 1 :z t)
  (def-range-masked vpshrdd 102 115 0 :z t)
  (def-range-masked vpshrdq 102 115 1 :z t)
  (def-range-masked vpternlogd 102 37 0)
  (def-range-masked vpternlogq 102 37 1)
  (def-range-masked vpternlogd 102 37 0 :z t)
  (def-range-masked vpternlogq 102 37 1 :z t)
  (def-range-masked vshuff32x4 102 35 0)
  (def-range-masked vshuff64x2 102 35 1)
  (def-range-masked vshufi32x4 102 67 0)
  (def-range-masked vshufi64x2 102 67 1)
  (def-range-masked vshuff32x4 102 35 0 :z t)
  (def-range-masked vshuff64x2 102 35 1 :z t)
  (def-range-masked vshufi32x4 102 67 0 :z t)
  (def-range-masked vshufi64x2 102 67 1 :z t))


(macrolet ((def-insert-masked (name prefix opcode w disp-n &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}{z}")
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}")))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for ll in '(0 1 2)
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem-imm prefix opcode
                                                :opcode-prefix 3898 :w w :nds t
                                                :ll ll :disp-n disp-n :evex-b 0
                                                :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src1 src2 mask imm)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                    :opcode-prefix 3898 :vvvv src1 :w ,w :aaa
                                    mask :z ,z-bit :disp-n ,disp-n
                                    :remaining-bytes 1)
                  (emit-byte segment imm))))))
  (def-insert-masked vinsertf32x4 102 24 0 16)
  (def-insert-masked vinsertf64x2 102 24 1 16)
  (def-insert-masked vinsertf32x8 102 26 0 32)
  (def-insert-masked vinsertf64x4 102 26 1 32)
  (def-insert-masked vinserti32x4 102 56 0 16)
  (def-insert-masked vinserti64x2 102 56 1 16)
  (def-insert-masked vinserti32x8 102 58 0 32)
  (def-insert-masked vinserti64x4 102 58 1 32)
  (def-insert-masked vinsertf32x4 102 24 0 16 :z t)
  (def-insert-masked vinsertf64x2 102 24 1 16 :z t)
  (def-insert-masked vinsertf32x8 102 26 0 32 :z t)
  (def-insert-masked vinsertf64x4 102 26 1 32 :z t)
  (def-insert-masked vinserti32x4 102 56 0 16 :z t)
  (def-insert-masked vinserti64x2 102 56 1 16 :z t)
  (def-insert-masked vinserti32x8 102 58 0 32 :z t)
  (def-insert-masked vinserti64x4 102 58 1 32 :z t))


(macrolet ((def-2op-masked (name prefix opcode w &key z store-p)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         (if store-p
                             '(:name :tab reg/mem ", " reg " {" aaa "}{z}")
                             '(:name :tab reg ", " reg/mem " {" aaa "}{z}"))
                         (if store-p
                             '(:name :tab reg/mem ", " reg " {" aaa "}")
                             '(:name :tab reg ", " reg/mem " {" aaa "}"))))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for ll in '(0 1 2)
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem prefix opcode
                                                :opcode-prefix 3896 :w w :ll ll
                                                :disp-n
                                                (case ll (0 16) (1 32) (2 64))
                                                :evex-b 0 :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src mask)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  ,(if store-p
                       `(emit-avx512-inst segment dst src ,prefix ,opcode
                                          :opcode-prefix 3896 :w ,w :aaa mask
                                          :z ,z-bit :disp-n
                                          (cond ((zmm-register-p src) 64)
                                                ((ymm-register-p src) 32)
                                                ((xmm-register-p src) 16)
                                                (t 0)))
                       `(emit-avx512-inst segment src dst ,prefix ,opcode
                                          :opcode-prefix 3896 :w ,w :aaa mask
                                          :z ,z-bit :disp-n
                                          (cond ((zmm-register-p dst) 64)
                                                ((ymm-register-p dst) 32)
                                                ((xmm-register-p dst) 16)
                                                (t 0)))))))))
  (def-2op-masked vpopcntd 102 85 0)
  (def-2op-masked vpopcntq 102 85 1)
  (def-2op-masked vpopcntb 102 84 0)
  (def-2op-masked vpopcntw 102 84 1)
  (def-2op-masked vpopcntd 102 85 0 :z t)
  (def-2op-masked vpopcntq 102 85 1 :z t)
  (def-2op-masked vpopcntb 102 84 0 :z t)
  (def-2op-masked vpopcntw 102 84 1 :z t)
  (def-2op-masked vgetexpps 102 66 0)
  (def-2op-masked vgetexppd 102 66 1)
  (def-2op-masked vgetexpps 102 66 0 :z t)
  (def-2op-masked vgetexppd 102 66 1 :z t)
  (def-2op-masked vpabsq 102 31 1)
  (def-2op-masked vpabsq 102 31 1 :z t)
  (def-2op-masked vpconflictd 102 196 0)
  (def-2op-masked vpconflictq 102 196 1)
  (def-2op-masked vplzcntd 102 68 0)
  (def-2op-masked vplzcntq 102 68 1)
  (def-2op-masked vpconflictd 102 196 0 :z t)
  (def-2op-masked vpconflictq 102 196 1 :z t)
  (def-2op-masked vplzcntd 102 68 0 :z t)
  (def-2op-masked vplzcntq 102 68 1 :z t)
  (def-2op-masked vcompressps 102 138 0 :store-p t)
  (def-2op-masked vcompresspd 102 138 1 :store-p t)
  (def-2op-masked vpcompressd 102 139 0 :store-p t)
  (def-2op-masked vpcompressq 102 139 1 :store-p t)
  (def-2op-masked vpcompressb 102 99 0 :store-p t)
  (def-2op-masked vpcompressw 102 99 1 :store-p t)
  (def-2op-masked vcompressps 102 138 0 :store-p t :z t)
  (def-2op-masked vcompresspd 102 138 1 :store-p t :z t)
  (def-2op-masked vpcompressd 102 139 0 :store-p t :z t)
  (def-2op-masked vpcompressq 102 139 1 :store-p t :z t)
  (def-2op-masked vpcompressb 102 99 0 :store-p t :z t)
  (def-2op-masked vpcompressw 102 99 1 :store-p t :z t)
  (def-2op-masked vexpandps 102 136 0)
  (def-2op-masked vexpandpd 102 136 1)
  (def-2op-masked vpexpandd 102 137 0)
  (def-2op-masked vpexpandq 102 137 1)
  (def-2op-masked vpexpandb 102 98 0)
  (def-2op-masked vpexpandw 102 98 1)
  (def-2op-masked vexpandps 102 136 0 :z t)
  (def-2op-masked vexpandpd 102 136 1 :z t)
  (def-2op-masked vpexpandd 102 137 0 :z t)
  (def-2op-masked vpexpandq 102 137 1 :z t)
  (def-2op-masked vpexpandb 102 98 0 :z t)
  (def-2op-masked vpexpandw 102 98 1 :z t))


(macrolet ((def-2op-masked (name prefix opcode w &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg ", " reg/mem " {" aaa "}{z}")
                         '(:name :tab reg ", " reg/mem " {" aaa "}")))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for ll in '(0 1 2)
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem prefix opcode
                                                :opcode-prefix 3896 :w w :ll ll
                                                :disp-n
                                                (case ll (0 16) (1 32) (2 64))
                                                :evex-b 0 :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src mask)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment src dst ,prefix ,opcode
                                    :opcode-prefix 3896 :w ,w :aaa mask :z
                                    ,z-bit :disp-n
                                    (cond ((zmm-register-p dst) 64)
                                          ((ymm-register-p dst) 32)
                                          ((xmm-register-p dst) 16) (t 0)))))))
           (def-2op-bcast (name prefix opcode w)
             (let* ((disp-n
                     (if (= w 0)
                         4
                         8))
                    (bcast-list
                     (if (= w 0)
                         '((0 "{1to4}") (1 "{1to8}") (2 "{1to16}"))
                         '((0 "{1to2}") (1 "{1to4}") (2 "{1to8}"))))
                    (printer-forms
                     (loop for (ll bcast) in bcast-list
                           append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                                   opcode :opcode-prefix 3896 :w w :ll ll
                                   :disp-n disp-n :evex-b 1 :printer
                                   (list :name :tab 'reg ", " 'reg/mem " "
                                         bcast)))))
               `(define-instruction ,name (segment dst src) ,@printer-forms
                 (:emitter (aver (not (register-p src)))
                  (emit-avx512-inst segment src dst ,prefix ,opcode
                                    :opcode-prefix 3896 :w ,w :evex-b 1 :disp-n
                                    ,disp-n))))))
  (def-2op-masked vrcp14ps 102 76 0)
  (def-2op-masked vrcp14pd 102 76 1)
  (def-2op-masked vrsqrt14ps 102 78 0)
  (def-2op-masked vrsqrt14pd 102 78 1)
  (def-2op-masked vrcp14ps 102 76 0 :z t)
  (def-2op-masked vrcp14pd 102 76 1 :z t)
  (def-2op-masked vrsqrt14ps 102 78 0 :z t)
  (def-2op-masked vrsqrt14pd 102 78 1 :z t)
  (def-2op-bcast vrcp14ps-bcast 102 76 0)
  (def-2op-bcast vrcp14pd-bcast 102 76 1)
  (def-2op-bcast vrsqrt14ps-bcast 102 78 0)
  (def-2op-bcast vrsqrt14pd-bcast 102 78 1))


(macrolet ((def-2op-imm-masked (name prefix opcode w &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg ", " reg/mem " {" aaa "}{z}")
                         '(:name :tab reg ", " reg/mem " {" aaa "}")))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for ll in '(0 1 2)
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem-imm prefix opcode
                                                :opcode-prefix 3898 :w w :ll ll
                                                :disp-n 64 :evex-b 0
                                                :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src mask imm)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment src dst ,prefix ,opcode
                                    :opcode-prefix 3898 :w ,w :aaa mask :z
                                    ,z-bit :disp-n
                                    (cond ((zmm-register-p dst) 64)
                                          ((ymm-register-p dst) 32)
                                          ((xmm-register-p dst) 16) (t 0))
                                    :remaining-bytes 1)
                  (emit-byte segment imm))))))
  (def-2op-imm-masked vreduceps 102 86 0)
  (def-2op-imm-masked vreducepd 102 86 1)
  (def-2op-imm-masked vreduceps 102 86 0 :z t)
  (def-2op-imm-masked vreducepd 102 86 1 :z t)
  (def-2op-imm-masked vrndscaleps 102 8 0)
  (def-2op-imm-masked vrndscalepd 102 9 1)
  (def-2op-imm-masked vrndscaleps 102 8 0 :z t)
  (def-2op-imm-masked vrndscalepd 102 9 1 :z t)
  (def-2op-imm-masked vgetmantps 102 38 0)
  (def-2op-imm-masked vgetmantpd 102 38 1)
  (def-2op-imm-masked vgetmantps 102 38 0 :z t)
  (def-2op-imm-masked vgetmantpd 102 38 1 :z t))


(macrolet ((def-scalar-imm-masked (name prefix opcode w &key z (nds nil))
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         (if nds
                             '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                               "}{z}")
                             '(:name :tab reg ", " reg/mem " {" aaa "}{z}"))
                         (if nds
                             '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                               "}")
                             '(:name :tab reg ", " reg/mem " {" aaa "}"))))
                    (disp-n
                     (if (= w 0)
                         4
                         8))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (avx512-inst-printer-list 'ymm-ymm/mem-imm
                                   prefix opcode :opcode-prefix 3898 :w w :nds
                                   nds :ll 0 :disp-n disp-n :evex-b 0
                                   :more-fields
                                   (list (list 'aaa k) (list 'z-bit z-bit))
                                   :printer mask-printer))))
               `(define-instruction ,ins-name
                 ,(if nds
                      `(segment dst src1 src2 mask imm)
                      `(segment dst src mask imm))
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  ,(if nds
                       `(progn
                         (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                           :opcode-prefix 3898 :vvvv src1 :w ,w
                                           :aaa mask :z ,z-bit :disp-n ,disp-n
                                           :remaining-bytes 1)
                         (emit-byte segment imm))
                       `(progn
                         (emit-avx512-inst segment src dst ,prefix ,opcode
                                           :opcode-prefix 3898 :w ,w :aaa mask
                                           :z ,z-bit :disp-n ,disp-n
                                           :remaining-bytes 1)
                         (emit-byte segment imm))))))))
  (def-scalar-imm-masked vgetmantss 102 39 0 :nds t)
  (def-scalar-imm-masked vgetmantsd 102 39 1 :nds t)
  (def-scalar-imm-masked vrangess 102 81 0 :nds t)
  (def-scalar-imm-masked vrangesd 102 81 1 :nds t)
  (def-scalar-imm-masked vreducess 102 87 0 :nds t)
  (def-scalar-imm-masked vreducesd 102 87 1 :nds t)
  (def-scalar-imm-masked vfixupimmss 102 85 0 :nds t)
  (def-scalar-imm-masked vfixupimmsd 102 85 1 :nds t)
  (def-scalar-imm-masked vgetmantss 102 39 0 :nds t :z t)
  (def-scalar-imm-masked vgetmantsd 102 39 1 :nds t :z t)
  (def-scalar-imm-masked vrangess 102 81 0 :nds t :z t)
  (def-scalar-imm-masked vrangesd 102 81 1 :nds t :z t)
  (def-scalar-imm-masked vreducess 102 87 0 :nds t :z t)
  (def-scalar-imm-masked vreducesd 102 87 1 :nds t :z t)
  (def-scalar-imm-masked vfixupimmss 102 85 0 :nds t :z t)
  (def-scalar-imm-masked vfixupimmsd 102 85 1 :nds t :z t)
  (def-scalar-imm-masked vrndscaless 102 10 0)
  (def-scalar-imm-masked vrndscalesd 102 11 1)
  (def-scalar-imm-masked vrndscaless 102 10 0 :z t)
  (def-scalar-imm-masked vrndscalesd 102 11 1 :z t))


(macrolet ((def-bf16-bcast (name prefix opcode w)
             (let* ((disp-n
                     (if (= w 0)
                         4
                         8))
                    (bcast-list
                     (if (= w 0)
                         '((0 "{1to4}") (1 "{1to8}") (2 "{1to16}"))
                         '((0 "{1to2}") (1 "{1to4}") (2 "{1to8}"))))
                    (printer-forms
                     (loop for (ll bcast) in bcast-list
                           append (avx512-inst-printer-list 'ymm-ymm/mem prefix
                                   opcode :opcode-prefix 3896 :w w :nds t :ll
                                   ll :disp-n disp-n :evex-b 1 :printer
                                   (list :name :tab 'reg ", " 'vvvv ", "
                                         'reg/mem " " bcast)))))
               `(define-instruction ,name (segment dst src1 src2)
                 ,@printer-forms
                 (:emitter (aver (not (register-p src2)))
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                    :opcode-prefix 3896 :vvvv src1 :w ,w
                                    :evex-b 1 :disp-n ,disp-n))))))
  (def-bf16-bcast vcvtne2ps2bf16-bcast 242 114 0)
  (def-bf16-bcast vdpbf16ps-bcast 242 82 0))


(macrolet ((def-f16c-masked (name prefix opcode &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg ", " reg/mem " {" aaa "}{z}")
                         '(:name :tab reg ", " reg/mem " {" aaa "}")))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for (ll n) in '((0 8) (1 16) (2 32))
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem prefix opcode
                                                :opcode-prefix 3896 :w 0 :ll ll
                                                :disp-n n :evex-b 0
                                                :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src mask)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment src dst ,prefix ,opcode
                                    :opcode-prefix 3896 :w 0 :aaa mask :z
                                    ,z-bit :disp-n
                                    (cond ((zmm-register-p dst) 32)
                                          ((ymm-register-p dst) 16)
                                          ((xmm-register-p dst) 8) (t 0))))))))
  (def-f16c-masked vcvtph2ps 102 19)
  (def-f16c-masked vcvtph2ps 102 19 :z t))


(macrolet ((def-f16c-store-masked (name prefix opcode &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg/mem ", " reg " {" aaa "}{z}")
                         '(:name :tab reg/mem ", " reg " {" aaa "}")))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for (ll n) in '((0 8) (1 16) (2 32))
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem-imm prefix opcode
                                                :opcode-prefix 3898 :w 0 :ll ll
                                                :disp-n n :evex-b 0
                                                :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src mask imm)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment dst src ,prefix ,opcode
                                    :opcode-prefix 3898 :w 0 :aaa mask :z
                                    ,z-bit :disp-n
                                    (cond ((zmm-register-p src) 32)
                                          ((ymm-register-p src) 16)
                                          ((xmm-register-p src) 8) (t 0))
                                    :remaining-bytes 1)
                  (emit-byte segment imm))))))
  (def-f16c-store-masked vcvtps2ph 102 29)
  (def-f16c-store-masked vcvtps2ph 102 29 :z t))


(macrolet ((def-narrow-store-masked (name prefix opcode w disp-ns &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg/mem ", " reg " {" aaa "}{z}")
                         '(:name :tab reg/mem ", " reg " {" aaa "}")))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for (ll n) in (list
                                                       (list 0 (first disp-ns))
                                                       (list 1
                                                             (second disp-ns))
                                                       (list 2
                                                             (third disp-ns)))
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem prefix opcode
                                                :opcode-prefix 3896 :w w :ll ll
                                                :disp-n n :evex-b 0
                                                :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src mask)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment dst src ,prefix ,opcode
                                    :opcode-prefix 3896 :w ,w :aaa mask :z
                                    ,z-bit :disp-n
                                    (cond
                                     ((zmm-register-p src) ,(third disp-ns))
                                     ((ymm-register-p src) ,(second disp-ns))
                                     ((xmm-register-p src) ,(first disp-ns))
                                     (t 0))))))))
  (def-narrow-store-masked vpmovqd 243 53 0 (8 16 32))
  (def-narrow-store-masked vpmovqw 243 52 0 (4 8 16))
  (def-narrow-store-masked vpmovqb 243 50 0 (2 4 8))
  (def-narrow-store-masked vpmovdw 243 51 0 (8 16 32))
  (def-narrow-store-masked vpmovdb 243 49 0 (4 8 16))
  (def-narrow-store-masked vpmovwb 243 48 0 (8 16 32))
  (def-narrow-store-masked vpmovqd 243 53 0 (8 16 32) :z t)
  (def-narrow-store-masked vpmovqw 243 52 0 (4 8 16) :z t)
  (def-narrow-store-masked vpmovqb 243 50 0 (2 4 8) :z t)
  (def-narrow-store-masked vpmovdw 243 51 0 (8 16 32) :z t)
  (def-narrow-store-masked vpmovdb 243 49 0 (4 8 16) :z t)
  (def-narrow-store-masked vpmovwb 243 48 0 (8 16 32) :z t)
  (def-narrow-store-masked vpmovsqd 243 37 0 (8 16 32))
  (def-narrow-store-masked vpmovsqw 243 36 0 (4 8 16))
  (def-narrow-store-masked vpmovsqb 243 34 0 (2 4 8))
  (def-narrow-store-masked vpmovusqd 243 21 0 (8 16 32))
  (def-narrow-store-masked vpmovusqw 243 20 0 (4 8 16))
  (def-narrow-store-masked vpmovusqb 243 18 0 (2 4 8))
  (def-narrow-store-masked vpmovsdw 243 35 0 (8 16 32))
  (def-narrow-store-masked vpmovsdb 243 33 0 (2 4 8))
  (def-narrow-store-masked vpmovusdw 243 19 0 (8 16 32))
  (def-narrow-store-masked vpmovusdb 243 17 0 (2 4 8))
  (def-narrow-store-masked vpmovswb 243 32 0 (4 8 16))
  (def-narrow-store-masked vpmovuswb 243 16 0 (4 8 16))
  (def-narrow-store-masked vpmovsqd 243 37 0 (8 16 32) :z t)
  (def-narrow-store-masked vpmovsqw 243 36 0 (4 8 16) :z t)
  (def-narrow-store-masked vpmovsqb 243 34 0 (2 4 8) :z t)
  (def-narrow-store-masked vpmovusqd 243 21 0 (8 16 32) :z t)
  (def-narrow-store-masked vpmovusqw 243 20 0 (4 8 16) :z t)
  (def-narrow-store-masked vpmovusqb 243 18 0 (2 4 8) :z t)
  (def-narrow-store-masked vpmovsdw 243 35 0 (8 16 32) :z t)
  (def-narrow-store-masked vpmovsdb 243 33 0 (2 4 8) :z t)
  (def-narrow-store-masked vpmovusdw 243 19 0 (8 16 32) :z t)
  (def-narrow-store-masked vpmovusdb 243 17 0 (2 4 8) :z t)
  (def-narrow-store-masked vpmovswb 243 32 0 (4 8 16) :z t)
  (def-narrow-store-masked vpmovuswb 243 16 0 (4 8 16) :z t))


(macrolet ((def-vdbpsadbw-masked (name prefix opcode w &key z)
             (let* ((z-bit
                     (if z
                         1
                         0))
                    (ins-name
                     (symbolicate name
                                  (if z
                                      "-MASKED-Z"
                                      "-MASKED")))
                    (mask-printer
                     (if z
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}{z}")
                         '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa
                           "}")))
                    (printer-forms
                     (loop for k from 1 to 7
                           append (loop for (ll n) in '((0 16) (1 32) (2 64))
                                        append (avx512-inst-printer-list
                                                'ymm-ymm/mem-imm prefix opcode
                                                :opcode-prefix 3898 :w w :nds t
                                                :ll ll :disp-n n :evex-b 0
                                                :more-fields
                                                (list (list 'aaa k)
                                                      (list 'z-bit z-bit))
                                                :printer mask-printer)))))
               `(define-instruction ,ins-name (segment dst src1 src2 mask imm)
                 ,@printer-forms
                 (:emitter (aver (and (integerp mask) (<= 1 mask 7)))
                  (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                    :opcode-prefix 3898 :vvvv src1 :w ,w :aaa
                                    mask :z ,z-bit :disp-n
                                    (cond ((zmm-register-p dst) 64)
                                          ((ymm-register-p dst) 32)
                                          ((xmm-register-p dst) 16) (t 0))
                                    :remaining-bytes 1)
                  (emit-byte segment imm))))))
  (def-vdbpsadbw-masked vdbpsadbw 102 66 0)
  (def-vdbpsadbw-masked vdbpsadbw 102 66 0 :z t))


;;;; ---- AVX-512_FP16 instructions ----

;;; 3-operand vector arithmetic (Map 5 & Map 6)
(macrolet ((def (name opcode &optional (opcode-prefix :map5) (prefix nil) (w 0))
             (let ((masked-name (symbolicate name "-MASKED")))
               `(progn
                  (define-instruction ,name (segment dst src1 src2 &optional mask (zeroing 0))
                    ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                                :opcode-prefix opcode-prefix :w w :nds t)
                    ,@(loop for k from 1 to 7
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w :nds t
                             :more-fields `((aaa ,k) (z-bit 0))
                             :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}"))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w :nds t
                             :more-fields `((aaa ,k) (z-bit 1))
                             :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "} {z}")))
                    (:emitter
                     (let ((mask-num (cond ((null mask) 0)
                                           ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :vvvv src1
                                         :w ,w
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n (full-vector-disp-n dst)))))
                  (define-instruction ,masked-name (segment dst src1 src2 mask &optional (zeroing 0))
                    (:emitter
                     (let ((mask-num (cond ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :vvvv src1
                                         :w ,w
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n (full-vector-disp-n dst)))))))))
  ;; Map 5 arithmetic
  (def vaddph #x58)
  (def vsubph #x5c)
  (def vmulph #x59)
  (def vdivph #x5e)
  (def vminph #x5d)
  (def vmaxph #x5f)
  ;; Map 6 scalef
  (def vscalefph #x2c :map6 #x66)
  ;; Map 6 FMA (vector)
  (def vfmadd132ph #x98 :map6 #x66)
  (def vfmadd213ph #xa8 :map6 #x66)
  (def vfmadd231ph #xb8 :map6 #x66)
  (def vfmsub132ph #x9a :map6 #x66)
  (def vfmsub213ph #xaa :map6 #x66)
  (def vfmsub231ph #xba :map6 #x66)
  (def vfnmadd132ph #x9c :map6 #x66)
  (def vfnmadd213ph #xac :map6 #x66)
  (def vfnmadd231ph #xbc :map6 #x66)
  (def vfnmsub132ph #x9e :map6 #x66)
  (def vfnmsub213ph #xae :map6 #x66)
  (def vfnmsub231ph #xbe :map6 #x66)
  (def vfmaddsub132ph #x96 :map6 #x66)
  (def vfmaddsub213ph #xa6 :map6 #x66)
  (def vfmaddsub231ph #xb6 :map6 #x66)
  (def vfmsubadd132ph #x97 :map6 #x66)
  (def vfmsubadd213ph #xa7 :map6 #x66)
  (def vfmsubadd231ph #xb7 :map6 #x66)
  ;; Map 6 Complex FMA
  (def vfcmaddcph #x56 :map6 #xf2)
  (def vfmaddcph  #x56 :map6 #xf3))

;;; 2-operand vector arithmetic (sqrt, rcp, rsqrt)
(macrolet ((def (name opcode &optional (opcode-prefix :map5) (prefix nil) (w 0))
             (let ((masked-name (symbolicate name "-MASKED")))
               `(progn
                  (define-instruction ,name (segment dst src &optional mask (zeroing 0))
                    ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                                :opcode-prefix opcode-prefix :w w)
                    ,@(loop for k from 1 to 7
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w
                             :more-fields `((aaa ,k) (z-bit 0))
                             :printer '(:name :tab reg ", " reg/mem " {" aaa "}"))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w
                             :more-fields `((aaa ,k) (z-bit 1))
                             :printer '(:name :tab reg ", " reg/mem " {" aaa "} {z}")))
                    (:emitter
                     (let ((mask-num (cond ((null mask) 0)
                                           ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :w ,w
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n (full-vector-disp-n dst)))))
                  (define-instruction ,masked-name (segment dst src mask &optional (zeroing 0))
                    (:emitter
                     (let ((mask-num (cond ((null mask) 0)
                                           ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :w ,w
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n (full-vector-disp-n dst)))))))))
  (def vsqrtph  #x51 :map5 nil)
  (def vrcpph   #x4c :map6 #x66)
  (def vrsqrtph #x4e :map6 #x66))

;;; Scalar arithmetic (Map 5 & Map 6, 3-operand, ll=0, disp-n=2)
(macrolet ((def (name opcode &optional (opcode-prefix :map5) (prefix #xf3) (w 0))
             (let ((masked-name (symbolicate name "-MASKED")))
               `(progn
                  (define-instruction ,name (segment dst src1 src2 &optional mask (zeroing 0))
                    ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                                :opcode-prefix opcode-prefix :w w :ll 0 :nds t)
                    ,@(loop for k from 1 to 7
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w :ll 0 :nds t
                             :more-fields `((aaa ,k) (z-bit 0))
                             :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}"))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix opcode-prefix :w w :ll 0 :nds t
                             :more-fields `((aaa ,k) (z-bit 1))
                             :printer '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "} {z}")))
                    (:emitter
                     (let ((mask-num (cond ((null mask) 0)
                                           ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :vvvv src1
                                         :w ,w
                                         :ll 0
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n 2))))
                  (define-instruction ,masked-name (segment dst src1 src2 mask &optional (zeroing 0))
                    (:emitter
                     (let ((mask-num (cond ((null mask) 0)
                                           ((integerp mask) mask)
                                           ((k-register-p mask) (reg-id-num (reg-id mask)))
                                           (t (error "Invalid mask ~S" mask))))
                           (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                       (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                         :opcode-prefix ,opcode-prefix
                                         :vvvv src1
                                         :w ,w
                                         :ll 0
                                         :aaa mask-num
                                         :z z-num
                                         :disp-n 2))))))))
  (def vaddsh #x58)
  (def vsubsh #x5c)
  (def vmulsh #x59)
  (def vdivsh #x5e)
  (def vminsh #x5d)
  (def vmaxsh #x5f)
  (def vsqrtsh #x51)
  (def vrcpsh #x4d :map6 #x66)
  (def vrsqrtsh #x4f :map6 #x66)
  (def vscalefsh #x2d :map6 #x66)
  ;; Scalar FMA
  (def vfmadd132sh #x99 :map6 #x66)
  (def vfmadd213sh #xa9 :map6 #x66)
  (def vfmadd231sh #xb9 :map6 #x66)
  (def vfmsub132sh #x9b :map6 #x66)
  (def vfmsub213sh #xab :map6 #x66)
  (def vfmsub231sh #xbb :map6 #x66)
  (def vfnmadd132sh #x9d :map6 #x66)
  (def vfnmadd213sh #xad :map6 #x66)
  (def vfnmadd231sh #xbd :map6 #x66)
  (def vfnmsub132sh #x9f :map6 #x66)
  (def vfnmsub213sh #xaf :map6 #x66)
  (def vfnmsub231sh #xbf :map6 #x66))

;;; Comparisons & Classification
(macrolet ((def-cmp (name prefix name-suffix &key scalar)
             `(define-instruction ,name (segment condition dst src src2 &optional mask)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm prefix #xc2
                                            :opcode-prefix #x0f3a
                                            :w 0
                                            :ll (if scalar 0 nil)
                                            :more-fields `((reg nil :type 'opmask-reg)
                                                           (imm nil :type 'avx-condition-code))
                                            :printer `("VCMP" imm ,name-suffix
                                                              :tab reg ", " vvvv ", " reg/mem))
                ,@(loop for k from 1 to 7
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem-imm prefix #xc2
                         :opcode-prefix #x0f3a
                         :w 0
                         :ll (if scalar 0 nil)
                         :more-fields `((reg nil :type 'opmask-reg)
                                        (imm nil :type 'avx-condition-code)
                                        (aaa ,k))
                         :printer `("VCMP" imm ,name-suffix
                                           :tab reg " {" aaa "}, " vvvv ", " reg/mem)))
                (:emitter
                 (multiple-value-bind (cond-arg dst-reg src1 src2-arg mask-val)
                     (if (register-p condition)
                         (values src2 condition dst src (or mask 0))
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
                     (aver (k-register-p dst-reg))
                     (let ((disp-n ,(if scalar
                                        2
                                        `(cond ((zmm-register-p src1) 64)
                                               ((ymm-register-p src1) 32)
                                               (t 16)))))
                       (emit-avx512-inst segment src2-arg dst-reg ,prefix #xc2
                                         :opcode-prefix #x0f3a
                                         :vvvv src1
                                         :w 0
                                         :ll ,(if scalar 0 nil)
                                         :aaa mask-num
                                         :disp-n disp-n
                                         :remaining-bytes 1)
                       (emit-byte segment imm))))))))
  (def-cmp vcmpph nil  "PH")
  (def-cmp vcmpsh #xf3 "SH" :scalar t))

(macrolet ((def ()
             `(progn
                (define-instruction vcomish (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem nil #x2f :opcode-prefix :map5 :w 0 :ll 0)
                  (:emitter
                   (emit-avx512-inst segment src dst nil #x2f
                                     :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2)))

                (define-instruction vucomish (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem nil #x2e :opcode-prefix :map5 :w 0 :ll 0)
                  (:emitter
                   (emit-avx512-inst segment src dst nil #x2e
                                     :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2)))

                (define-instruction vfpclassph (segment dst src imm &optional mask)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x66 :opcode-prefix #x0f3a :w 0
                                              :more-fields '((reg nil :type 'opmask-reg))
                                              :printer '(:name :tab reg ", " reg/mem ", " imm))
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x66 :opcode-prefix #x0f3a :w 0
                                                    :more-fields `((reg nil :type 'opmask-reg) (aaa ,k))
                                                    :printer '(:name :tab reg " {" aaa "}, " reg/mem ", " imm)))
                  (:emitter
                   (let ((mask-num (cond ((null mask) 0)
                                         ((integerp mask) mask)
                                         ((k-register-p mask) (reg-id-num (reg-id mask)))
                                         (t 0))))
                     (emit-avx512-inst segment src dst nil #x66
                                       :opcode-prefix #x0f3a
                                       :w 0
                                       :aaa mask-num
                                       :disp-n (full-vector-disp-n src)
                                       :remaining-bytes 1)
                     (emit-byte segment imm))))

                (define-instruction vfpclasssh (segment dst src imm &optional mask)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x67 :opcode-prefix #x0f3a :w 0 :ll 0
                                              :more-fields '((reg nil :type 'opmask-reg))
                                              :printer '(:name :tab reg ", " reg/mem ", " imm))
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x67 :opcode-prefix #x0f3a :w 0 :ll 0
                                                    :more-fields `((reg nil :type 'opmask-reg) (aaa ,k))
                                                    :printer '(:name :tab reg " {" aaa "}, " reg/mem ", " imm)))
                  (:emitter
                   (let ((mask-num (cond ((null mask) 0)
                                         ((integerp mask) mask)
                                         ((k-register-p mask) (reg-id-num (reg-id mask)))
                                         (t 0))))
                     (emit-avx512-inst segment src dst nil #x67
                                       :opcode-prefix #x0f3a
                                       :w 0
                                       :ll 0
                                       :aaa mask-num
                                       :disp-n 2
                                       :remaining-bytes 1)
                     (emit-byte segment imm))))

                (define-instruction vrndscaleph (segment dst src imm &optional mask (zeroing 0))
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x08 :opcode-prefix #x0f3a :w 0
                                              :printer '(:name :tab reg ", " reg/mem ", " imm))
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x08 :opcode-prefix #x0f3a :w 0
                                                    :more-fields `((aaa ,k) (z-bit 0))
                                                    :printer '(:name :tab reg ", " reg/mem ", " imm " {" aaa "}"))
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x08 :opcode-prefix #x0f3a :w 0
                                                    :more-fields `((aaa ,k) (z-bit 1))
                                                    :printer '(:name :tab reg ", " reg/mem ", " imm " {" aaa "} {z}")))
                  (:emitter
                   (let ((mask-num (cond ((null mask) 0)
                                         ((integerp mask) mask)
                                         ((k-register-p mask) (reg-id-num (reg-id mask)))
                                         (t 0)))
                         (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                     (emit-avx512-inst segment src dst nil #x08
                                       :opcode-prefix #x0f3a
                                       :w 0
                                       :aaa mask-num
                                       :z z-num
                                       :disp-n (full-vector-disp-n dst)
                                       :remaining-bytes 1)
                     (emit-byte segment imm))))

                (define-instruction vrndscalesh (segment dst src1 src2 imm &optional mask (zeroing 0))
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x0a :opcode-prefix #x0f3a :w 0 :ll 0
                                              :printer '(:name :tab reg ", " vvvv ", " reg/mem ", " imm))
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x0a :opcode-prefix #x0f3a :w 0 :ll 0
                                                    :more-fields `((aaa ,k) (z-bit 0))
                                                    :printer '(:name :tab reg ", " vvvv ", " reg/mem ", " imm " {" aaa "}"))
                          append
                          (avx512-inst-printer-list 'ymm-ymm/mem-imm nil #x0a :opcode-prefix #x0f3a :w 0 :ll 0
                                                    :more-fields `((aaa ,k) (z-bit 1))
                                                    :printer '(:name :tab reg ", " vvvv ", " reg/mem ", " imm " {" aaa "} {z}")))
                  (:emitter
                   (let ((mask-num (cond ((null mask) 0)
                                         ((integerp mask) mask)
                                         ((k-register-p mask) (reg-id-num (reg-id mask)))
                                         (t 0)))
                         (z-num (if (or (eq zeroing :z) (eql zeroing 1)) 1 0)))
                     (emit-avx512-inst segment src2 dst nil #x0a
                                       :opcode-prefix #x0f3a
                                       :vvvv src1
                                       :w 0
                                       :ll 0
                                       :aaa mask-num
                                       :z z-num
                                       :disp-n 2
                                       :remaining-bytes 1)
                     (emit-byte segment imm)))))))
  (def))

;;; Conversions between FP16 and single-precision float (vcvtph2psx, vcvtps2phx)
(macrolet ((def ()
             `(progn
                (define-instruction vcvtph2psx (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x13 :opcode-prefix :map6 :w 0)
                  (:emitter
                   (let ((ll (cond ((zmm-register-p dst) #b10)
                                   ((ymm-register-p dst) #b01)
                                   (t #b00)))
                         (disp-n (cond ((zmm-register-p dst) 32)
                                       ((ymm-register-p dst) 16)
                                       (t 8))))
                     (emit-avx512-inst segment src dst #x66 #x13
                                       :opcode-prefix :map6
                                       :w 0
                                       :ll ll
                                       :disp-n disp-n))))

                (define-instruction vcvtps2phx (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x1d :opcode-prefix :map5 :w 0)
                  (:emitter
                   (let ((ll (cond ((zmm-register-p src) #b10)
                                   ((ymm-register-p src) #b01)
                                   (t #b00)))
                         (disp-n (cond ((zmm-register-p src) 64)
                                       ((ymm-register-p src) 32)
                                       (t 16))))
                     (emit-avx512-inst segment src dst #x66 #x1d
                                       :opcode-prefix :map5
                                       :w 0
                                       :ll ll
                                       :disp-n disp-n))))

                (define-instruction vcvtdq2ph (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem nil #x5b :opcode-prefix :map5 :w 0)
                  (:emitter
                   (let ((ll (cond ((zmm-register-p src) #b10)
                                   ((ymm-register-p src) #b01)
                                   (t #b00)))
                         (disp-n (cond ((zmm-register-p src) 64)
                                       ((ymm-register-p src) 32)
                                       (t 16))))
                     (emit-avx512-inst segment src dst nil #x5b
                                       :opcode-prefix :map5
                                       :w 0
                                       :ll ll
                                       :disp-n disp-n)))))))
  (def))

(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode :opcode-prefix :map5 :w 0)
                (:emitter
                 (let ((ll (cond ((zmm-register-p dst) #b10)
                                 ((ymm-register-p dst) #b01)
                                 (t #b00)))
                       (disp-n (cond ((zmm-register-p dst) 32)
                                     ((ymm-register-p dst) 16)
                                     (t 8))))
                   (emit-avx512-inst segment src dst ,prefix ,opcode
                                     :opcode-prefix :map5
                                     :w 0
                                     :ll ll
                                     :disp-n disp-n))))))
  (def vcvtph2dq  #x66 #x5b)
  (def vcvttph2dq #xf3 #x5b))

;;; Conversions between FP16 and 16-bit integers (1:1 width)
(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode :opcode-prefix :map5 :w 0)
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix :map5
                                   :w 0
                                   :disp-n (full-vector-disp-n dst))))))
  (def vcvtuw2ph  #xf2 #x7d)
  (def vcvtw2ph   #xf3 #x7d)
  (def vcvtph2w   #x66 #x7d)
  (def vcvtph2uw  nil  #x7d)
  (def vcvttph2w  #x66 #x7c)
  (def vcvttph2uw nil  #x7c))

;;; Scalar conversions
(macrolet ((def ()
             `(progn
                (define-instruction vcvtsd2sh (segment dst src1 src2)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #xf2 #x5a :opcode-prefix :map5 :w 1 :ll 0 :nds t)
                  (:emitter
                   (aver (xmm-register-p dst))
                   (emit-avx512-inst segment src2 dst #xf2 #x5a
                                     :opcode-prefix :map5
                                     :vvvv src1
                                     :w 1
                                     :ll 0
                                     :disp-n 8)))

                (define-instruction vcvtss2sh (segment dst src1 src2)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem nil #x1d :opcode-prefix :map5 :w 0 :ll 0 :nds t)
                  (:emitter
                   (aver (xmm-register-p dst))
                   (emit-avx512-inst segment src2 dst nil #x1d
                                     :opcode-prefix :map5
                                     :vvvv src1
                                     :w 0
                                     :ll 0
                                     :disp-n 4)))

                (define-instruction vcvtsi2sh (segment dst src1 src2)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #xf3 #x2a
                                              :opcode-prefix :map5
                                              :reg-mem-size :sized
                                              :nds t :ll 0)
                  (:emitter
                   (aver (xmm-register-p dst))
                   (let ((src-size (operand-size src2)))
                     (emit-avx512-inst segment src2 dst #xf3 #x2a
                                       :opcode-prefix :map5
                                       :ll 0
                                       :vvvv src1
                                       :w (case src-size
                                            (:qword 1)
                                            (:dword 0)
                                            (t 1))
                                       :disp-n (case src-size
                                                 (:qword 8)
                                                 (t 4)))))))))
  (def))

(macrolet ((def (name opcode)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'reg-ymm/mem #xf3 opcode
                                            :opcode-prefix :map5 :ll 0)
                (:emitter
                 (aver (gpr-p dst))
                 (let ((dst-size (operand-size dst)))
                   (aver (or (eq dst-size :qword) (eq dst-size :dword)))
                   (emit-avx512-inst segment src dst #xf3 ,opcode
                                     :opcode-prefix :map5
                                     :ll 0
                                     :w (ecase dst-size
                                          (:qword 1)
                                          (:dword 0))
                                     :disp-n 2))))))
  (def vcvtsh2si  #x2d)
  (def vcvttsh2si #x2c))

;;; FP16 moves (vmovsh, vmovw)
(macrolet ((def ()
             `(progn
                (define-instruction vmovsh (segment dst src &optional src2)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem-dir #xf3 #b0001000 :opcode-prefix :map5 :w 0 :ll 0)
                  (:emitter
                   (cond ((ea-p src)
                          (aver (xmm-register-p dst))
                          (emit-avx512-inst segment src dst #xf3 #x10
                                            :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2))
                         ((ea-p dst)
                          (aver (xmm-register-p src))
                          (emit-avx512-inst segment dst src #xf3 #x11
                                            :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2))
                         (src2
                          (aver (and (xmm-register-p dst) (xmm-register-p src) (xmm-register-p src2)))
                          (emit-avx512-inst segment src2 dst #xf3 #x10
                                            :opcode-prefix :map5 :w 0 :ll 0 :vvvv src))
                         (t
                          (aver (and (xmm-register-p dst) (xmm-register-p src)))
                          (emit-avx512-inst segment src dst #xf3 #x10
                                            :opcode-prefix :map5 :w 0 :ll 0 :vvvv dst)))))

                (define-instruction vmovw (segment dst src)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x6e :opcode-prefix :map5 :w 0 :ll 0
                                              :reg-mem-size :dword)
                  ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x7e :opcode-prefix :map5 :w 0 :ll 0
                                              :reg-mem-size :dword
                                              :printer '(:name :tab reg/mem ", " reg))
                  (:emitter
                   (cond ((gpr-p dst)
                          (aver (xmm-register-p src))
                          (emit-avx512-inst segment dst src #x66 #x7e
                                            :opcode-prefix :map5 :w 0 :ll 0))
                         ((gpr-p src)
                          (aver (xmm-register-p dst))
                          (emit-avx512-inst segment src dst #x66 #x6e
                                            :opcode-prefix :map5 :w 0 :ll 0))
                         ((ea-p dst)
                          (aver (xmm-register-p src))
                          (emit-avx512-inst segment dst src #x66 #x7e
                                            :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2))
                         ((ea-p src)
                          (aver (xmm-register-p dst))
                          (emit-avx512-inst segment src dst #x66 #x6e
                                            :opcode-prefix :map5 :w 0 :ll 0 :disp-n 2))
                         (t
                          (error "Unsupported operands for VMOVW: ~S, ~S" dst src))))))))
  (def))
