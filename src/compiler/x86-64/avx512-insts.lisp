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

;;; Return the compressed-displacement scale N for a full-vector
;;; load/store instruction.  The vector width determines N:
;;;   128-bit -> 16
;;;   256-bit -> 32
;;;   512-bit -> 64
(defun full-vector-disp-n (reg)
  (cond ((zmm-register-p reg) 64)
        ((ymm-register-p reg) 32)
        ((xmm-register-p reg) 16)
        (t 0)))

;;; EVEX-only aligned/unaligned moves
(macrolet ((def (name prefix opcode-from opcode-to w)
             `(define-instruction ,name (segment dst src)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode-from
                         :w w :ll ll :disp-n n))
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode-to
                         :w w :ll ll :disp-n n
                         :printer '(:name :tab reg/mem ", " reg)))
                (:emitter
                 (cond ((xmm-register-p dst)
                        (emit-avx512-inst segment src dst ,prefix ,opcode-from
                                         :opcode-prefix #x0F :w ,w
                                         :disp-n (full-vector-disp-n dst)))
                       (t
                        (aver (xmm-register-p src))
                        (emit-avx512-inst segment dst src ,prefix ,opcode-to
                                         :opcode-prefix #x0F :w ,w
                                         :disp-n (full-vector-disp-n src))))))))
  (def vmovdqa32 #x66 #x6f #x7f 0)
  (def vmovdqa64 #x66 #x6f #x7f 1)
  (def vmovdqu8  #xf2 #x6f #x7f 0)
  (def vmovdqu16 #xf2 #x6f #x7f 1)
  (def vmovdqu32 #xf3 #x6f #x7f 0)
  (def vmovdqu64 #xf3 #x6f #x7f 1))

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
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm #x66 #x25
                                            :opcode-prefix #x0f3a :w w)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 #x25
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vpternlogd 0)
  (def vpternlogq 1))

;;; Two-source permute
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w :nds t
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpermt2d  #x7e 0)
  (def vpermt2q  #x7e 1)
  (def vpermt2ps #x7f 0)
  (def vpermt2pd #x7f 1))

;;; Cross-lane shuffle with immediate
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem-imm #x66 opcode
                         :opcode-prefix #x0f3a :w w
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0))
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vshuff32x4  #x23 0)
  (def vshuff64x2  #x23 1)
  (def vshufi32x4  #x43 0)
  (def vshufi64x2  #x43 1))

;;; Blend with mask
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w :nds t
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vblendmps #x65 0)
  (def vblendmpd #x65 1)
  (def vpblendmd #x64 0)
  (def vpblendmq #x64 1))

;;; Compress store
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix #x0f38 :w w
                         :ll ll :disp-n n
                         :printer '(:name :tab reg/mem ", " reg)))
                (:emitter
                 (emit-avx512-inst segment dst src ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p src) 64)
                                                 ((ymm-register-p src) 32)
                                                 ((xmm-register-p src) 16)
                                                 (t 0)))))))
  (def vcompressps  #x66 #x8a 0)
  (def vcompresspd  #x66 #x8a 1)
  (def vpcompressd  #x66 #x8b 0)
  (def vpcompressq  #x66 #x8b 1))

;;; Expand load
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix #x0f38 :w w
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vexpandps  #x66 #x88 0)
  (def vexpandpd  #x66 #x88 1)
  (def vpexpandd  #x66 #x89 0)
  (def vpexpandq  #x66 #x89 1))

;;; Down-convert (pack and store)
(macrolet ((def (name prefix opcode w disp-ns)
             `(define-instruction ,name (segment dst src)
                ,@(loop for ll in '(#b00 #b01 #b10)
                        for n in disp-ns
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix #x0f38 :w w
                         :ll ll :disp-n n
                         :printer '(:name :tab reg/mem ", " reg)))
                (:emitter
                 (emit-avx512-inst segment dst src ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p src) (third ',disp-ns))
                                                 ((ymm-register-p src) (second ',disp-ns))
                                                 ((xmm-register-p src) (first ',disp-ns))
                                                 (t 0)))))))
  (def vpmovqd #xf3 #x35 0 (8 16 32))
  (def vpmovqw #xf3 #x34 0 (4 8 16))
  (def vpmovqb #xf3 #x32 0 (2 4 8))
  (def vpmovdw #xf3 #x33 0 (8 16 32))
  (def vpmovdb #xf3 #x31 0 (4 8 16))
  (def vpmovwb #xf3 #x30 0 (8 16 32)))

;;; Get exponent / mantissa
(macrolet ((def (name opcode w &key (opcode-prefix #x0f38))
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix opcode-prefix :w w)
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix ,opcode-prefix
                                   :w ,w))))
           (def-imm (name opcode w)
             `(define-instruction ,name (segment dst src imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm #x66 opcode
                                            :opcode-prefix #x0f3a :w w
                                            :printer '(:name :tab reg ", " reg/mem ", " imm))
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :w ,w
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vgetexpps  #x42 0)
  (def vgetexppd  #x42 1)
  (def-imm vgetmantps  #x26 0)
  (def-imm vgetmantpd  #x26 1))

;;; Round to scale
(macrolet ((def (name opcode w &key scalar-disp-n)
             `(define-instruction ,name (segment dst src imm)
                ,@(if scalar-disp-n
                      (avx512-inst-printer-list
                       'ymm-ymm/mem-imm #x66 opcode
                       :opcode-prefix #x0f3a :w w
                       :disp-n scalar-disp-n
                       :printer '(:name :tab reg ", " reg/mem ", " imm))
                      (loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem-imm #x66 opcode
                             :opcode-prefix #x0f3a :w w
                             :ll ll :disp-n n
                             :printer '(:name :tab reg ", " reg/mem ", " imm))))
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :w ,w
                                   :disp-n ,(if scalar-disp-n
                                                scalar-disp-n
                                                '(cond ((zmm-register-p dst) 64)
                                                       ((ymm-register-p dst) 32)
                                                       ((xmm-register-p dst) 16)
                                                       (t 0)))
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  ;; packed full-vector
  (def vrndscaleps #x08 0)
  (def vrndscalepd #x09 1)
  ;; scalar fixed-width
  (def vrndscaless #x0a 0 :scalar-disp-n 4)
  (def vrndscalesd #x0b 1 :scalar-disp-n 8))

;;; Fixup
(macrolet ((def (name opcode w &key scalar-disp-n)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(if scalar-disp-n
                      (avx512-inst-printer-list
                       'ymm-ymm/mem-imm #x66 opcode
                       :opcode-prefix #x0f3a :w w
                       :disp-n scalar-disp-n)
                      (loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem-imm #x66 opcode
                             :opcode-prefix #x0f3a :w w
                             :ll ll :disp-n n)))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :disp-n ,(if scalar-disp-n
                                                scalar-disp-n
                                                '(cond ((zmm-register-p dst) 64)
                                                       ((ymm-register-p dst) 32)
                                                       ((xmm-register-p dst) 16)
                                                       (t 0)))
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  ;; packed full-vector
  (def vfixupimmps #x54 0)
  (def vfixupimmpd #x54 1)
  ;; scalar fixed-width
  (def vfixupimmss #x55 0 :scalar-disp-n 4)
  (def vfixupimmsd #x55 1 :scalar-disp-n 8))

;;; Range
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem-imm #x66 opcode
                         :opcode-prefix #x0f3a :w w
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0))
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vrangeps #x50 0)
  (def vrangepd #x50 1))

;;; Reduce
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src imm)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem-imm #x66 opcode
                         :opcode-prefix #x0f3a :w w
                         :ll ll :disp-n n
                         :printer '(:name :tab reg ", " reg/mem ", " imm)))
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0))
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vreduceps #x56 0)
  (def vreducepd #x56 1))

;;; Scale
(macrolet ((def (name opcode w &key scalar-disp-n)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(if scalar-disp-n
                      (avx512-inst-printer-list
                       'ymm-ymm/mem #x66 opcode
                       :opcode-prefix #x0f38 :w w :nds t
                       :disp-n scalar-disp-n)
                      (loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem #x66 opcode
                             :opcode-prefix #x0f38 :w w :nds t
                             :ll ll :disp-n n)))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n ,(if scalar-disp-n
                                                scalar-disp-n
                                                '(cond ((zmm-register-p dst) 64)
                                                       ((ymm-register-p dst) 32)
                                                       ((xmm-register-p dst) 16)
                                                       (t 0))))))))
  ;; packed full-vector
  (def vscalefps #x2c 0)
  (def vscalefpd #x2c 1)
  ;; scalar fixed-width
  (def vscalefss #x2d 0 :scalar-disp-n 4)
  (def vscalefsd #x2d 1 :scalar-disp-n 8))

;;; Opmask instructions
;;; KMOV - Move to/from opmask registers
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kmov-printer-list (format-stem prefix opcode w &key printer)
    (let ((pp (vex-encode-pp prefix))
          (m-mmmm (vex-encode-m-mmmm #x0F)))
      (flet ((make-printer (inst-format fields)
               `(:printer ,inst-format ,fields
                          ,@(when printer `(',printer)))))
        (if (eql w 1)
            (list
             (make-printer
              (symbolicate "VEX3-" format-stem)
              `((pp ,pp)
                (m-mmmm ,m-mmmm)
                (w ,w)
                (op ,opcode))))
            (list
             (make-printer
              (symbolicate "VEX2-" format-stem)
              `((pp ,pp)
                (op ,opcode)))
             (make-printer
              (symbolicate "VEX3-" format-stem)
              `((pp ,pp)
                (m-mmmm ,m-mmmm)
                (w ,w)
                (op ,opcode)))))))))

;;; These use VEX encoding (not EVEX), with k registers in ModR/M fields
(macrolet ((def (name kk-prefix gr-prefix store-mem-prefix load-mem-prefix
                     op-k-k op-k-r op-r-k op-m-k op-k-m w &optional (gr-w w))
             `(define-instruction ,name (segment dst src)
                (:emitter
                 (cond
                   ((and (k-register-p dst) (k-register-p src))
                    ;; VEX: k1 <- k2
                    (emit-vex segment nil src dst ,kk-prefix #x0F 0 ,w)
                    (emit-bytes segment ,op-k-k)
                    (emit-ea segment src dst))

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
    (let ((pp (vex-encode-pp prefix))
          (m-mmmm (vex-encode-m-mmmm #x0F)))
      (flet ((make-printer (inst-format fields)
               `(:printer ,inst-format ,fields)))
        (if (eql w 1)
            (list
             (make-printer
              (symbolicate "VEX3-" 'kreg-kreg/mem-k)
              `((pp ,pp)
                (m-mmmm ,m-mmmm)
                (w ,w)
                (l 1)
                (op ,opcode))))
            (list
             (make-printer
              (symbolicate "VEX2-" 'kreg-kreg/mem-k)
              `((pp ,pp)
                (l 1)
                (op ,opcode)))
             (make-printer
              (symbolicate "VEX3-" 'kreg-kreg/mem-k)
              `((pp ,pp)
                (m-mmmm ,m-mmmm)
                (w ,w)
                (l 1)
                (op ,opcode)))))))))

(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                (:emitter
                 (emit-vex segment src1 src2 dst ,prefix #x0F 1 ,w)
                 (emit-bytes segment ,opcode)
                 (emit-ea segment src2 dst))

                ,@(klogical-printer-list prefix opcode w))))

  (def kandw  nil  #x41 0)
  (def kandb  #x66 #x41 0)
  (def kandd  #x66 #x41 1)
  (def kandq  #xf2 #x41 1)
  (def kandnw nil  #x42 0)
  (def kandnb #x66 #x42 0)
  (def kandnd #x66 #x42 1)
  (def kandnq #xf2 #x42 1)
  (def korw   nil  #x45 0)
  (def korb   #x66 #x45 0)
  (def kord   #x66 #x45 1)
  (def korq   #xf2 #x45 1)
  (def kxorw  nil  #x47 0)
  (def kxorb  #x66 #x47 0)
  (def kxord  #x66 #x47 1)
  (def kxorq  #xf2 #x47 1)
  (def kxnorw nil  #x46 0)
  (def kxnorb #x66 #x46 0)
  (def kxnord #x66 #x46 1)
  (def kxnorq #xf2 #x46 1))

;;; KNOT, KTEST - single-source opmask operations
;;; Encoding: ModRM.reg = dst, VEX.vvvv = src1, ModRM.r/m = src2
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
                (:emitter
                 (emit-vex segment nil src dst ,prefix #x0F 0 ,w)
                 (emit-bytes segment ,opcode)
                 (emit-ea segment src dst))

                ,@(kmov-printer-list 'kreg-kreg/mem prefix opcode w))))

  (def knotw  nil  #x44 0)
  (def knotb  #x66 #x44 0)
  (def knotd  #x66 #x44 1)
  (def knotq  #xf2 #x44 1)
  (def ktestw nil  #x99 0)
  (def ktestb #x66 #x99 0)
  (def ktestd #x66 #x99 1)
  (def ktestq #xf2 #x99 1)
  (def kortestw nil  #x98 0)
  (def kortestb #x66 #x98 0)
  (def kortestd #x66 #x98 1)
  (def kortestq #xf2 #x98 1))

;;; KUNPCK - Unpack and interleave opmask (VEX.L1)
(macrolet ((def (name prefix w)
             `(define-instruction ,name (segment dst src1 src2)
                (:emitter
                 (emit-vex segment src1 src2 dst ,prefix #x0F nil ,w)
                 (emit-bytes segment #x4b)
                 (emit-ea segment src2 dst))

                ,@(klogical-printer-list prefix #x4b w))))

  (def kunpckbw #x66 0)
  (def kunpckwd nil  0)
  (def kunpckdq nil  1))

;;; EVEX insert/extract for 256-bit lanes in 512-bit
(macrolet ((def-insert (name prefix op w disp-n)
             `(define-instruction ,name (segment dst src src2 imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm prefix op
                                            :w w
                                            :opcode-prefix #x0f3a
                                            :disp-n disp-n)
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,op
                                   :opcode-prefix #x0f3a
                                   :vvvv src
                                   :w ,w
                                   :disp-n ,disp-n
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def-insert vinsertf32x4  #x66 #x18 0 16)
  (def-insert vinsertf64x2  #x66 #x18 1 16)
  (def-insert vinsertf32x8  #x66 #x1a 0 32)
  (def-insert vinsertf64x4  #x66 #x1a 1 32)
  (def-insert vinserti32x4  #x66 #x38 0 16)
  (def-insert vinserti64x2  #x66 #x38 1 16)
  (def-insert vinserti32x8  #x66 #x3a 0 32)
  (def-insert vinserti64x4  #x66 #x3a 1 32))

(macrolet ((def-extract (name prefix op w disp-n)
             `(define-instruction ,name (segment dst src imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm prefix op
                                            :w w
                                            :opcode-prefix #x0f3a
                                            :disp-n disp-n
                                            :printer '(:name :tab reg/mem ", " reg ", " imm))
                (:emitter
                 (emit-avx512-inst segment dst src ,prefix ,op
                                   :w ,w
                                   :opcode-prefix #x0f3a
                                   :disp-n ,disp-n
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def-extract vextractf32x4 #x66 #x19 0 16)
  (def-extract vextractf64x2 #x66 #x19 1 16)
  (def-extract vextractf32x8 #x66 #x1b 0 32)
  (def-extract vextractf64x4 #x66 #x1b 1 32)
  (def-extract vextracti32x4 #x66 #x39 0 16)
  (def-extract vextracti64x2 #x66 #x39 1 16)
  (def-extract vextracti32x8 #x66 #x3b 0 32)
  (def-extract vextracti64x4 #x66 #x3b 1 32))

;;;; ---- AVX-512F additional instructions ----

;;; 3-operand NDS (dst, src1, src2)
;;; Full-vector 3-operand NDS (dst, src1, src2)
(macrolet ((def (name prefix opcode w &optional (opcode-prefix #x0f38))
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix opcode-prefix :w w :nds t
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix ,opcode-prefix
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  ;; Two-source permute
  (def vpermi2d   #x66 #x76 0)
  (def vpermi2q   #x66 #x76 1)
  (def vpermi2ps  #x66 #x77 0)
  (def vpermi2pd  #x66 #x77 1)
  ;; Integer max/min 64-bit
  (def vpmaxsq    #x66 #x3d 1)
  (def vpmaxuq    #x66 #x3f 1)
  (def vpminsq    #x66 #x39 1)
  (def vpminuq    #x66 #x3b 1)
  ;; Variable rotate
  (def vprolvd    #x66 #x15 0)
  (def vprolvq    #x66 #x15 1)
  (def vprorvd    #x66 #x14 0)
  (def vprorvq    #x66 #x14 1)
  ;; Variable arithmetic shift 64-bit
  (def vpsravq    #x66 #x46 1)
  ;; Integer logical (dword/qword granularity)
  (def vpandd     #x66 #xdb 0 #x0f)
  (def vpandq     #x66 #xdb 1 #x0f)
  (def vpandnd    #x66 #xdf 0 #x0f)
  (def vpandnq    #x66 #xdf 1 #x0f)
  (def vpord      #x66 #xeb 0 #x0f)
  (def vporq      #x66 #xeb 1 #x0f)
  (def vpxord     #x66 #xef 0 #x0f)
  (def vpxorq     #x66 #xef 1 #x0f)
  ;; VAES full-vector EVEX forms
  (def vaesenc     #x66 #xdc 0 #x0f38)
  (def vaesenclast #x66 #xdd 0 #x0f38)
  (def vaesdec     #x66 #xde 0 #x0f38)
  (def vaesdeclast #x66 #xdf 0 #x0f38))

;;; 2-operand (dst, src)
(macrolet ((def (name prefix opcode w &optional (opcode-prefix #x0f38))
             `(define-instruction ,name (segment dst src)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix opcode-prefix :w w
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix ,opcode-prefix :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpabsq     #x66 #x1f 1)
  (def vrcp14ps   #x66 #x4c 0)
  (def vrcp14pd   #x66 #x4c 1)
  (def vrsqrt14ps #x66 #x4e 0)
  (def vrsqrt14pd #x66 #x4e 1))

;;; Scalar reciprocal approximations (3-operand NDS)
(macrolet ((def (name opcode w disp-n)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list
                   'ymm-ymm/mem #x66 opcode
                   :opcode-prefix #x0f38 :w w :nds t
                   :disp-n disp-n)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n ,disp-n)))))
  (def vrcp14ss   #x4d 0 4)
  (def vrcp14sd   #x4d 1 8)
  (def vrsqrt14ss #x4f 0 4)
  (def vrsqrt14sd #x4f 1 8))

;;; 3-operand NDS + imm8
(macrolet ((def (name opcode w &key scalar-disp-n)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(if scalar-disp-n
                      (avx512-inst-printer-list
                       'ymm-ymm/mem-imm #x66 opcode
                       :opcode-prefix #x0f3a :w w
                       :disp-n scalar-disp-n)
                      (loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem-imm #x66 opcode
                             :opcode-prefix #x0f3a :w w
                             :ll ll :disp-n n)))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :disp-n ,(if scalar-disp-n
                                                scalar-disp-n
                                                '(cond ((zmm-register-p dst) 64)
                                                       ((ymm-register-p dst) 32)
                                                       ((xmm-register-p dst) 16)
                                                       (t 0)))
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  ;; packed full-vector
  (def valignd    #x03 0)
  (def valignq    #x03 1)
  ;; scalar fixed-width
  (def vrangess   #x51 0 :scalar-disp-n 4)
  (def vrangesd   #x51 1 :scalar-disp-n 8)
  (def vreducess  #x57 0 :scalar-disp-n 4)
  (def vreducesd  #x57 1 :scalar-disp-n 8)
  (def vgetmantss #x27 0 :scalar-disp-n 4)
  (def vgetmantsd #x27 1 :scalar-disp-n 8))

;;; Scalar getexp (3-operand NDS)
(macrolet ((def (name opcode w scalar-disp-n)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t
                                            :disp-n scalar-disp-n)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n ,scalar-disp-n)))))
  (def vgetexpss #x43 0 4)
  (def vgetexpsd #x43 1 8))

;;; Immediate rotate via /i field
(macrolet ((def (name opcode /i w)
             `(define-instruction ,name (segment dst src imm)
                ,@(avx512-inst-printer-list 'ymm-ymm-imm #x66 opcode
                                            :w w
                                            :more-fields (list (list '/i /i)))
                (:emitter
                 (emit-avx512-inst-imm segment dst src imm
                                       #x66 ,opcode ,/i
                                       :w ,w)))))
  (def vprold    #x72 1 0)
  (def vprolq    #x72 1 1)
  (def vprord    #x72 0 0)
  (def vprorq    #x72 0 1))

;;; VPSRAQ immediate form.
;;; The variable-shift form VPSRAVQ is defined separately.
(define-instruction vpsraq (segment dst src imm)
  (:emitter
   (emit-avx512-inst-imm segment dst src imm
                         #x66 #x72 4
                         :w 1))
  . #.(avx512-inst-printer-list 'ymm-ymm-imm #x66 #x72
                                :w 1
                                :more-fields '((/i 4))))

;;; Unsigned conversions (2-operand)
(macrolet ((def (name prefix opcode w disp-ns &optional (opcode-prefix #x0f))
             `(define-instruction ,name (segment dst src)
                ,@(loop for ll in '(#b00 #b01 #b10)
                        for n in disp-ns
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix opcode-prefix :w w
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix ,opcode-prefix :w ,w
                                   :disp-n (cond ((zmm-register-p dst) (third ',disp-ns))
                                                 ((ymm-register-p dst) (second ',disp-ns))
                                                 ((xmm-register-p dst) (first ',disp-ns))
                                                 (t 0)))))))
  (def vcvtps2udq  nil  #x79 0 (16 32 64))
  (def vcvtpd2udq  nil  #x79 1 (16 32 64))
  (def vcvttps2udq nil  #x78 0 (16 32 64))
  (def vcvttpd2udq nil  #x78 1 (16 32 64))
  (def vcvtudq2ps  #xf2 #x7a 0 (16 32 64))
  (def vcvtudq2pd  #xf3 #x7a 0 (8 16 32)))

;;; Scalar unsigned conversions (2-operand, dst=gpr)
(macrolet ((def (name prefix opcode disp-n)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'reg-ymm/mem prefix opcode
                                            :w 0 :disp-n disp-n)
                ,@(avx512-inst-printer-list 'reg-ymm/mem prefix opcode
                                            :w 1 :disp-n disp-n)
                (:emitter
                 (aver (gpr-p dst))
                 (let ((dst-size (operand-size dst)))
                   (aver (or (eq dst-size :qword) (eq dst-size :dword)))
                   (emit-avx512-inst segment src dst ,prefix ,opcode
                                     :w (ecase dst-size
                                          (:qword 1)
                                          (:dword 0))
                                     :disp-n ,disp-n))))))
  (def vcvtss2usi  #xf3 #x79 4)
  (def vcvtsd2usi  #xf2 #x79 8)
  (def vcvttss2usi #xf3 #x78 4)
  (def vcvttsd2usi #xf2 #x78 8))

;;; Scalar unsigned convert to FP (3-operand NDS)
(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :nds t :w 0 :disp-n 4)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :nds t :w 1 :disp-n 8)
                (:emitter
                 (aver (xmm-register-p dst))
                 (let ((src-size (operand-size src2)))
                   (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                     :vvvv src
                                     :w (case src-size
                                          (:qword 1)
                                          (:dword 0)
                                          (t 1))
                                     :disp-n (case src-size
                                               (:qword 8)
                                               (:dword 4)
                                               (t 8))))))))
  (def vcvtusi2ss #xf3 #x7b)
  (def vcvtusi2sd #xf2 #x7b))

;;; Saturating truncations (reversed encoding: src in reg, dst in r/m)
(macrolet ((def (name prefix opcode w disp-ns)
             `(define-instruction ,name (segment dst src)
                ,@(loop for ll in '(#b00 #b01 #b10)
                        for n in disp-ns
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix #x0f38 :w w
                         :ll ll :disp-n n
                         :printer '(:name :tab reg/mem ", " reg)))
                (:emitter
                 (emit-avx512-inst segment dst src ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p src) (third ',disp-ns))
                                                 ((ymm-register-p src) (second ',disp-ns))
                                                 ((xmm-register-p src) (first ',disp-ns))
                                                 (t 0)))))))
  (def vpmovsqd  #xf3 #x25 0 (8 16 32))
  (def vpmovsqw  #xf3 #x24 0 (4 8 16))
  (def vpmovsqb  #xf3 #x22 0 (2 4 8))
  (def vpmovusqd #xf3 #x15 0 (8 16 32))
  (def vpmovusqw #xf3 #x14 0 (4 8 16))
  (def vpmovusqb #xf3 #x12 0 (2 4 8))
  (def vpmovsdw  #xf3 #x23 0 (8 16 32))
  (def vpmovsdb  #xf3 #x21 0 (2 4 8))
  (def vpmovusdw #xf3 #x13 0 (8 16 32))
  (def vpmovusdb #xf3 #x11 0 (2 4 8))
  (def vpmovswb  #xf3 #x20 0 (4 8 16))
  (def vpmovuswb #xf3 #x10 0 (4 8 16)))

;;; Broadcast-memory
(macrolet ((def (name opcode w disp-n)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38
                                            :w w
                                            :disp-n disp-n)
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n ,disp-n)))))
  (def vbroadcastf32x4 #x1a 0 16)
  (def vbroadcastf64x4 #x1b 1 32)
  (def vbroadcasti32x4 #x5a 0 16)
  (def vbroadcasti64x4 #x5b 1 32))

;;; Broadcast from mask-register
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #xf3 opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst #xf3 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vpbroadcastmb2q #x2a 1)
  (def vpbroadcastmw2d #x3a 0))

;;; Compatibility aliases for vpbroadcast from GPR
(defmacro vpbroadcastd-gpr (segment dst src)
  `(vpbroadcastd ,segment ,dst ,src))
(defmacro vpbroadcastq-gpr (segment dst src)
  `(vpbroadcastq ,segment ,dst ,src))

;;; VEX-encoded kshift (dst, src, imm8)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun kshift-printer-list (prefix opcode w)
    (let ((fields
            `((pp ,(vex-encode-pp prefix))
              (m-mmmm ,(vex-encode-m-mmmm #x0F3A))
              (w ,w)
              (l 0)
              (op ,opcode)
              (imm nil :type 'imm-byte))))
      (list
       `(:printer ,(symbolicate "VEX3-" 'kreg-kreg/mem-imm)
                  ,fields)))))

(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src imm)
                (:emitter
                 (emit-vex segment nil src dst ,prefix #x0F3A 0 ,w)
                 (emit-bytes segment ,opcode)
                 (emit-ea segment src dst :remaining-bytes 1)
                 (emit-byte segment imm))

                ,@(kshift-printer-list prefix opcode w))))

  (def kshiftlb #x66 #x32 0)
  (def kshiftlw #x66 #x32 1)
  (def kshiftld #x66 #x33 0)
  (def kshiftlq #x66 #x33 1)

  (def kshiftrb #x66 #x30 0)
  (def kshiftrw #x66 #x30 1)
  (def kshiftrd #x66 #x31 0)
  (def kshiftrq #x66 #x31 1))

;;; VEX-encoded kadd (VEX.L1)
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                (:emitter
                 (emit-vex segment src1 src2 dst ,prefix #x0F 1 ,w)
                 (emit-bytes segment ,opcode)
                 (emit-ea segment src2 dst))

                ,@(klogical-printer-list prefix opcode w))))

  (def kaddb #x66 #x4a 0)
  (def kaddw nil  #x4a 0)
  (def kaddd #x66 #x4a 1)
  (def kaddq #xf2 #x4a 1))

;;; Compare-to-k (kdst, src1, src2, imm8) - k-reg in ModR/M reg
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                                                  :opcode-prefix #x0f3a :w w
                                                  :ll ll :disp-n n
                                                  :more-fields '((reg nil :type 'opmask-reg))))
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p src1) 64)
                                                 ((ymm-register-p src1) 32)
                                                 ((xmm-register-p src1) 16)
                                                 (t 0))
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vpcmpd   #x66 #x1f 0)
  (def vpcmpud  #x66 #x1e 0)
  (def vpcmpq   #x66 #x1f 1)
  (def vpcmpuq  #x66 #x1e 1))

;;; Test-to-k (kdst, src1, src2) - k-reg in ModR/M reg
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                                  :opcode-prefix #x0f38 :w w :nds t
                                                  :ll ll :disp-n n
                                                  :more-fields '((reg nil :type 'opmask-reg))))
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p src1) 64)
                                                 ((ymm-register-p src1) 32)
                                                 ((xmm-register-p src1) 16)
                                                 (t 0)))))))
  (def vptestmd  #x66 #x27 0)
  (def vptestmq  #x66 #x27 1)
  (def vptestnmd #xf3 #x27 0)
  (def vptestnmq #xf3 #x27 1))

;;;; ---- AVX-512BW instructions ----

;;; Blend with mask (byte/word)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w :nds t
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpblendmb #x66 0)
  (def vpblendmw #x66 1))

;;; Compare byte/word to k - k-reg in ModR/M reg
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem-imm prefix opcode
                         :opcode-prefix #x0f3a :w w
                         :ll ll :disp-n n
                         :more-fields '((reg nil :type 'opmask-reg))))
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p src1) 64)
                                                 ((ymm-register-p src1) 32)
                                                 ((xmm-register-p src1) 16)
                                                 (t 0))
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vpcmpb    #x66 #x3f 0)
  (def vpcmpub   #x66 #x3e 0)
  (def vpcmpw    #x66 #x3f 1)
  (def vpcmpuw   #x66 #x3e 1))

;;; Test byte/word to k - k-reg in ModR/M reg
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix #x0f38 :w w :nds t
                         :ll ll :disp-n n
                         :more-fields '((reg nil :type 'opmask-reg))))
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p src1) 64)
                                                 ((ymm-register-p src1) 32)
                                                 ((xmm-register-p src1) 16)
                                                 (t 0)))))))
  (def vptestmb  #x66 #x26 0)
  (def vptestmw  #x66 #x26 1)
  (def vptestnmb #xf3 #x26 0)
  (def vptestnmw #xf3 #x26 1))

;;; Move mask (byte/word to/from k)
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vpmovb2m  #xf3 #x29 0)
  (def vpmovw2m  #xf3 #x29 1)
  (def vpmovm2b  #xf3 #x28 0)
  (def vpmovm2w  #xf3 #x28 1))

;;; Permute word
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w :nds t
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpermw    #x8d 1)
  (def vpermi2w  #x75 1)
  (def vpermt2w  #x7d 1))

;;; Variable shift word
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w :nds t
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpsllvw   #x12 1)
  (def vpsravw   #x11 1)
  (def vpsrlvw   #x10 1))

;;; Double-block packed SAD
(define-instruction vdbpsadbw (segment dst src1 src2 imm)
  (:emitter
   (emit-avx512-inst segment src2 dst #x66 #x42
                     :opcode-prefix #x0f3a
                     :vvvv src1
                     :w 0
                     :disp-n (cond ((zmm-register-p dst) 64)
                                   ((ymm-register-p dst) 32)
                                   ((xmm-register-p dst) 16)
                                   (t 0))
                     :remaining-bytes 1)
   (emit-byte segment imm))
  . #.(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
            append
            (avx512-inst-printer-list 'ymm-ymm/mem-imm #x66 #x42
                                      :opcode-prefix #x0f3a :w 0
                                      :ll ll :disp-n n)))

;;;; ---- AVX-512DQ instructions ----

;;; FP classify
(macrolet ((def (name opcode w &key scalar-disp-n)
             `(define-instruction ,name (segment dst src imm)
                ,@(if scalar-disp-n
                      (avx512-inst-printer-list
                       'ymm-ymm/mem-imm #x66 opcode
                       :opcode-prefix #x0f3a :w w
                       :ll #b00
                       :disp-n scalar-disp-n
                       :more-fields '((reg nil :type 'opmask-reg))
                       :printer '(:name :tab reg ", " reg/mem ", " imm))
                      (loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem-imm #x66 opcode
                             :opcode-prefix #x0f3a :w w
                             :ll ll
                             :disp-n n
                             :more-fields '((reg nil :type 'opmask-reg))
                             :printer '(:name :tab reg ", " reg/mem ", " imm))))
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :w ,w
                                   :disp-n
                                   ,(if scalar-disp-n
                                        scalar-disp-n
                                        `(cond ((zmm-register-p src) 64)
                                               ((ymm-register-p src) 32)
                                               ((xmm-register-p src) 16)
                                               (t 0)))
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  ;; packed full-vector
  (def vfpclassps #x66 0)
  (def vfpclasspd #x66 1)
  ;; scalar fixed-width
  (def vfpclassss #x67 0 :scalar-disp-n 4)
  (def vfpclasssd #x67 1 :scalar-disp-n 8))

;;; Multiply low qword
(define-instruction vpmullq (segment dst src1 src2)
  (:emitter
   (emit-avx512-inst segment src2 dst #x66 #x40
                     :opcode-prefix #x0f38
                     :vvvv src1
                     :w 1
                     :disp-n (cond ((zmm-register-p dst) 64)
                                   ((ymm-register-p dst) 32)
                                   ((xmm-register-p dst) 16)
                                   (t 0))))
  . #.(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
            append
            (avx512-inst-printer-list 'ymm-ymm/mem #x66 #x40
                                      :opcode-prefix #x0f38 :w 1 :nds t
                                      :ll ll :disp-n n)))

;;; Convert packed integers to/from FP (DQ extensions)
(macrolet ((def (name prefix opcode w disp-ns &optional (opcode-prefix #x0f))
             `(define-instruction ,name (segment dst src)
                ,@(loop for ll in '(#b00 #b01 #b10)
                        for n in disp-ns
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem prefix opcode
                         :opcode-prefix opcode-prefix :w w
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix ,opcode-prefix :w ,w
                                   :disp-n (cond ((zmm-register-p dst) (third ',disp-ns))
                                                 ((ymm-register-p dst) (second ',disp-ns))
                                                 ((xmm-register-p dst) (first ',disp-ns))
                                                 (t 0)))))))
  ;; single -> qword (memory operand is half width)
  (def vcvtps2qq   #x66 #x7b 0 (8 16 32))
  (def vcvtps2uqq  #x66 #x79 0 (8 16 32))
  (def vcvttps2qq  #x66 #x7a 0 (8 16 32))
  (def vcvttps2uqq #x66 #x78 0 (8 16 32))
  ;; double -> qword (full width)
  (def vcvtpd2qq   #x66 #x7b 1 (16 32 64))
  (def vcvtpd2uqq  #x66 #x79 1 (16 32 64))
  (def vcvttpd2qq  #x66 #x7a 1 (16 32 64))
  (def vcvttpd2uqq #x66 #x78 1 (16 32 64))
  ;; qword -> single/double (full width)
  (def vcvtqq2ps   nil  #x5b 1 (16 32 64))
  (def vcvtqq2pd   #xf3 #xe6 1 (16 32 64))
  (def vcvtuqq2ps  #xf2 #x7a 1 (16 32 64))
  (def vcvtuqq2pd  #xf3 #x7a 1 (16 32 64)))

;;; Move mask (dword/qword to/from k)
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vpmovd2m  #xf3 #x39 0)
  (def vpmovq2m  #xf3 #x39 1)
  (def vpmovm2d  #xf3 #x38 0)
  (def vpmovm2q  #xf3 #x38 1))

;;; DQ broadcast variants
(macrolet ((def (name opcode w disp-n)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38
                                            :w w
                                            :disp-n disp-n)
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n ,disp-n)))))
  (def vbroadcastf32x2 #x19 0 8)
  (def vbroadcastf64x2 #x1a 1 16)
  (def vbroadcasti32x2 #x59 0 8)
  (def vbroadcasti64x2 #x5a 1 16)
  (def vbroadcastf32x8 #x1b 0 32)
  (def vbroadcasti32x8 #x5b 0 32))

;;;; ---- AVX-512IFMA instructions ----

(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w :nds t
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpmadd52luq #xb4 1)
  (def vpmadd52huq #xb5 1))

;;;; ---- AVX-512VBMI instructions ----

;;; Permute and multishift
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w :nds t
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpermb       #x8d 0)
  (def vpermi2b     #x75 0)
  (def vpermt2b     #x7d 0)
  (def vpmultishiftqb #x83 1))

;;;; ---- AVX-512VBMI2 instructions ----

;;; Compress byte/word (reversed encoding)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w
                         :ll ll :disp-n n
                         :printer '(:name :tab reg/mem ", " reg)))
                (:emitter
                 (emit-avx512-inst segment dst src #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p src) 64)
                                                 ((ymm-register-p src) 32)
                                                 ((xmm-register-p src) 16)
                                                 (t 0)))))))
  (def vpcompressb #x63 0)
  (def vpcompressw #x63 1))

;;; Expand byte/word
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpexpandb #x62 0)
  (def vpexpandw #x62 1))

;;; VBMI2 concatenate and shift (immediate)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem-imm #x66 opcode
                         :opcode-prefix #x0f3a :w w
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0))
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vpshldw   #x70 1)
  (def vpshldd   #x71 0)
  (def vpshldq   #x71 1)
  (def vpshrdw   #x72 1)
  (def vpshrdd   #x73 0)
  (def vpshrdq   #x73 1))

;;; VBMI2 concatenate and shift (variable)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w :nds t
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpshldvw  #x70 1)
  (def vpshldvd  #x71 0)
  (def vpshldvq  #x71 1)
  (def vpshrdvw  #x72 1)
  (def vpshrdvd  #x73 0)
  (def vpshrdvq  #x73 1))

;;;; ---- AVX-512VPOPCNTDQ instructions ----

;;; VPOPCNTDQ
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpopcntd  #x55 0)
  (def vpopcntq  #x55 1))

;;;; ---- AVX-512BITALG instructions ----

;;; BITALG popcount
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
                        append
                        (avx512-inst-printer-list
                         'ymm-ymm/mem #x66 opcode
                         :opcode-prefix #x0f38 :w w
                         :ll ll :disp-n n))
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :disp-n (cond ((zmm-register-p dst) 64)
                                                 ((ymm-register-p dst) 32)
                                                 ((xmm-register-p dst) 16)
                                                 (t 0)))))))
  (def vpopcntb  #x54 0)
  (def vpopcntw  #x54 1))

;;; Shuffle bits (result to k)
(define-instruction vpshufbitqmb (segment dst src1 src2)
  (:emitter
   (emit-avx512-inst segment src2 dst #x66 #x8f
                     :opcode-prefix #x0f38
                     :vvvv src1
                     :w 0
                     :disp-n (cond ((zmm-register-p src1) 64)
                                   ((ymm-register-p src1) 32)
                                   ((xmm-register-p src1) 16)
                                   (t 0))))
  . #.(loop for (ll n) in '((#b00 16) (#b01 32) (#b10 64))
            append
            (avx512-inst-printer-list 'ymm-ymm/mem #x66 #x8f
                                      :opcode-prefix #x0f38 :w 0 :nds t
                                      :ll ll :disp-n n
                                      :more-fields '((reg nil :type 'opmask-reg)))))

;;;; ---- Masked arithmetic (EVEX with opmask {k}) ----

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
                    '(:name :tab reg ", " vvvv ", " reg/mem
                      " {" aaa "}{z}")))
               `(define-instruction ,zero-name (segment dst src1 src2 mask)
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list
                           'ymm-ymm/mem prefix opcode
                           :opcode-prefix opcode-prefix
                           :w w
                           :nds t
                           :more-fields `((aaa ,k) (z-bit 1))
                           :printer mask-printer))
                  (:emitter
                   (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                     :opcode-prefix ,opcode-prefix
                                     :vvvv src1
                                     :w ,w
                                     :aaa mask
                                     :z 1))))))
  ;; Byte/word arithmetic zeroing masked
  (def-z vpaddb-masked #x66 #xfc 0)
  (def-z vpaddw-masked #x66 #xfd 0)
  (def-z vpsubb-masked #x66 #xf8 0)
  (def-z vpsubw-masked #x66 #xf9 0)
  ;; Integer arithmetic (qword)
  (def-z vpaddq-masked  #x66 #xd4 1)
  (def-z vpsubq-masked  #x66 #xfb 1)
  ;; Integer arithmetic (dword)
  (def-z vpaddd-masked  #x66 #xfe 0)
  (def-z vpsubd-masked  #x66 #xfa 0)
  ;; Integer logical (qword)
  (def-z vpandq-masked  #x66 #xdb 1)
  (def-z vpandnq-masked #x66 #xdf 1)
  (def-z vporq-masked   #x66 #xeb 1)
  (def-z vpxorq-masked  #x66 #xef 1)
  ;; FP arithmetic (double)
  (def-z vaddpd-masked  #x66 #x58 1)
  (def-z vsubpd-masked  #x66 #x5c 1)
  (def-z vmulpd-masked  #x66 #x59 1)
  ;; FP arithmetic (single)
  (def-z vaddps-masked  nil  #x58 0)
  (def-z vsubps-masked  nil  #x5c 0)
  (def-z vmulps-masked  nil  #x59 0)
  ;; FP division (single/double)
  (def-z vdivps-masked nil  #x5e 0)
  (def-z vdivpd-masked #x66 #x5e 1)
  ;; FP min/max
  (def-z vminps-masked  nil  #x5d 0)
  (def-z vmaxps-masked  nil  #x5f 0)
  (def-z vminpd-masked  #x66 #x5d 1)
  (def-z vmaxpd-masked  #x66 #x5f 1)
  ;; FMA zeroing masked
  (def-z vfmadd132ps-masked #x66 #x98 0 #x0f38)
  (def-z vfmadd132pd-masked #x66 #x98 1 #x0f38)
  (def-z vfmadd213ps-masked #x66 #xa8 0 #x0f38)
  (def-z vfmadd213pd-masked #x66 #xa8 1 #x0f38)
  (def-z vfmadd231ps-masked #x66 #xb8 0 #x0f38)
  (def-z vfmadd231pd-masked #x66 #xb8 1 #x0f38)
  ;; Signed dword min/max
  (def-z vpminsd-masked #x66 #x39 0 #x0f38)
  (def-z vpmaxsd-masked #x66 #x3d 0 #x0f38)
  ;; Unsigned dword min/max
  (def-z vpminud-masked #x66 #x3b 0 #x0f38)
  (def-z vpmaxud-masked #x66 #x3f 0 #x0f38)
  ;; Signed qword min/max
  (def-z vpminsq-masked #x66 #x39 1 #x0f38)
  (def-z vpmaxsq-masked #x66 #x3d 1 #x0f38)
  ;; Unsigned qword min/max
  (def-z vpminuq-masked #x66 #x3b 1 #x0f38)
  (def-z vpmaxuq-masked #x66 #x3f 1 #x0f38)
  ;; FMA subtract zeroing masked
  (def-z vfmsub132ps-masked #x66 #x9a 0 #x0f38)
  (def-z vfmsub132pd-masked #x66 #x9a 1 #x0f38)
  (def-z vfmsub213ps-masked #x66 #xaa 0 #x0f38)
  (def-z vfmsub213pd-masked #x66 #xaa 1 #x0f38)
  (def-z vfmsub231ps-masked #x66 #xba 0 #x0f38)
  (def-z vfmsub231pd-masked #x66 #xba 1 #x0f38)
  ;; FMA negative-add zeroing masked
  (def-z vfnmadd132ps-masked #x66 #x9c 0 #x0f38)
  (def-z vfnmadd132pd-masked #x66 #x9c 1 #x0f38)
  (def-z vfnmadd213ps-masked #x66 #xac 0 #x0f38)
  (def-z vfnmadd213pd-masked #x66 #xac 1 #x0f38)
  (def-z vfnmadd231ps-masked #x66 #xbc 0 #x0f38)
  (def-z vfnmadd231pd-masked #x66 #xbc 1 #x0f38)
  ;; FMA add/subtract zeroing masked
  (def-z vfmaddsub132ps-masked #x66 #x96 0 #x0f38)
  (def-z vfmaddsub132pd-masked #x66 #x96 1 #x0f38)
  (def-z vfmaddsub213ps-masked #x66 #xa6 0 #x0f38)
  (def-z vfmaddsub213pd-masked #x66 #xa6 1 #x0f38)
  (def-z vfmaddsub231ps-masked #x66 #xb6 0 #x0f38)
  (def-z vfmaddsub231pd-masked #x66 #xb6 1 #x0f38)
  ;; FMA subtract/add zeroing masked
  (def-z vfmsubadd132ps-masked #x66 #x97 0 #x0f38)
  (def-z vfmsubadd132pd-masked #x66 #x97 1 #x0f38)
  (def-z vfmsubadd213ps-masked #x66 #xa7 0 #x0f38)
  (def-z vfmsubadd213pd-masked #x66 #xa7 1 #x0f38)
  (def-z vfmsubadd231ps-masked #x66 #xb7 0 #x0f38)
  (def-z vfmsubadd231pd-masked #x66 #xb7 1 #x0f38)
  ;; Variable shift zeroing masked
  (def-z vpsllvd-masked #x66 #x47 0 #x0f38)
  (def-z vpsllvq-masked #x66 #x47 1 #x0f38)
  (def-z vpsravd-masked #x66 #x46 0 #x0f38)
  (def-z vpsravq-masked #x66 #x46 1 #x0f38)
  (def-z vpsrlvd-masked #x66 #x45 0 #x0f38)
  (def-z vpsrlvq-masked #x66 #x45 1 #x0f38)
  ;; VNNI dot-product zeroing masked
  (def-z vpdpbusd-masked  #x66 #x50 0 #x0f38)
  (def-z vpdpbusds-masked #x66 #x51 0 #x0f38)
  (def-z vpdpwssd-masked  #x66 #x52 0 #x0f38)
  (def-z vpdpwssds-masked #x66 #x53 0 #x0f38)
  ;; Permute zeroing masked (dword/qword)
  (def-z vpermi2d-masked  #x66 #x76 0 #x0f38)
  (def-z vpermi2q-masked  #x66 #x76 1 #x0f38)
  (def-z vpermi2ps-masked #x66 #x77 0 #x0f38)
  (def-z vpermi2pd-masked #x66 #x77 1 #x0f38)
  (def-z vpermt2d-masked  #x66 #x7e 0 #x0f38)
  (def-z vpermt2q-masked  #x66 #x7e 1 #x0f38)
  (def-z vpermt2ps-masked #x66 #x7f 0 #x0f38)
  (def-z vpermt2pd-masked #x66 #x7f 1 #x0f38)
  ;; Permute zeroing masked (byte/word)
  (def-z vpermb-masked   #x66 #x8d 0 #x0f38)
  (def-z vpermi2b-masked #x66 #x75 0 #x0f38)
  (def-z vpermt2b-masked #x66 #x7d 0 #x0f38)
  (def-z vpermw-masked   #x66 #x8d 1 #x0f38)
  (def-z vpermi2w-masked #x66 #x75 1 #x0f38)
  (def-z vpermt2w-masked #x66 #x7d 1 #x0f38)
  ;; IFMA zeroing masked
  (def-z vpmadd52luq-masked #x66 #xb4 1 #x0f38)
  (def-z vpmadd52huq-masked #x66 #xb5 1 #x0f38)
  ;; VBMI2 variable shift double zeroing masked
  (def-z vpshldvw-masked #x66 #x70 1 #x0f38)
  (def-z vpshldvd-masked #x66 #x71 0 #x0f38)
  (def-z vpshldvq-masked #x66 #x71 1 #x0f38)
  (def-z vpshrdvw-masked #x66 #x72 1 #x0f38)
  (def-z vpshrdvd-masked #x66 #x73 0 #x0f38)
  (def-z vpshrdvq-masked #x66 #x73 1 #x0f38)
  ;; Scale zeroing masked
  (def-z vscalefps-masked #x66 #x2c 0 #x0f38)
  (def-z vscalefpd-masked #x66 #x2c 1 #x0f38)
  ;; GFNI multiply zeroing masked
  (def-z vgf2p8mulb-masked #x66 #xcf 0 #x0f38)
  ;; VAES zeroing masked
  (def-z vaesenc-masked      #x66 #xdc 0 #x0f38)
  (def-z vaesenclast-masked  #x66 #xdd 0 #x0f38)
  (def-z vaesdec-masked      #x66 #xde 0 #x0f38)
  (def-z vaesdeclast-masked  #x66 #xdf 0 #x0f38)
  ;; VPMULTISHIFTQB zeroing masked
  (def-z vpmultishiftqb-masked #x66 #x83 1 #x0f38))

;;;; ---- Embedded broadcast forms ----

(macrolet ((def-bcast (name prefix opcode w)
             (let ((disp-n (if (= w 0) 4 8))
                   (bcast-list
                    (if (= w 0)
                        '((#b00 "{1to4}") (#b01 "{1to8}") (#b10 "{1to16}"))
                        '((#b00 "{1to2}") (#b01 "{1to4}") (#b10 "{1to8}")))))
               `(define-instruction ,name (segment dst src1 src2)
                  ,@(loop for (ll bcast) in bcast-list
                          append
                          (avx512-inst-printer-list
                           'ymm-ymm/mem prefix opcode
                           :opcode-prefix #x0f
                           :w w
                           :nds t
                           :ll ll
                           :disp-n disp-n
                           :evex-b 1
                           :printer (list :name :tab 'reg ", " 'vvvv ", "
                                          'reg/mem " " bcast)))
                  (:emitter
                   (aver (not (register-p src2))) ; must be memory
                   (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                     :opcode-prefix #x0f
                                     :vvvv src1
                                     :w ,w
                                     :evex-b 1
                                     :disp-n ,disp-n))))))
  (def-bcast vaddps-bcast nil  #x58 0)
  (def-bcast vmulps-bcast nil  #x59 0)
  (def-bcast vaddpd-bcast #x66 #x58 1)
  (def-bcast vmulpd-bcast #x66 #x59 1)
  (def-bcast vsubps-bcast nil  #x5c 0)
  (def-bcast vsubpd-bcast #x66 #x5c 1)
  (def-bcast vdivps-bcast nil  #x5e 0)
  (def-bcast vdivpd-bcast #x66 #x5e 1)
  (def-bcast vminps-bcast nil  #x5d 0)
  (def-bcast vmaxps-bcast nil  #x5f 0)
  (def-bcast vminpd-bcast #x66 #x5d 1)
  (def-bcast vmaxpd-bcast #x66 #x5f 1))

(macrolet ((def-ibcast (name opcode w)
             (let ((disp-n (if (= w 0) 4 8))
                   (bcast-list
                     (if (= w 0)
                         '((#b00 "{1to4}") (#b01 "{1to8}") (#b10 "{1to16}"))
                         '((#b00 "{1to2}") (#b01 "{1to4}") (#b10 "{1to8}")))))
               `(define-instruction ,name (segment dst src1 src2)
                  ,@(loop for (ll bcast) in bcast-list
                          append
                          (avx512-inst-printer-list
                           'ymm-ymm/mem #x66 opcode
                           :opcode-prefix #x0f
                           :w w
                           :nds t
                           :ll ll
                           :disp-n disp-n
                           :evex-b 1
                           :printer (list :name :tab 'reg ", " 'vvvv ", "
                                          'reg/mem " " bcast)))
                  (:emitter
                   (aver (not (register-p src2))) ; must be memory
                   (emit-avx512-inst segment src2 dst #x66 ,opcode
                                     :opcode-prefix #x0f
                                     :vvvv src1
                                     :w ,w
                                     :evex-b 1
                                     :disp-n ,disp-n))))))
  ;; Integer arithmetic broadcast
  (def-ibcast vpaddd-bcast #xfe 0)
  (def-ibcast vpaddq-bcast #xd4 1)
  (def-ibcast vpsubd-bcast #xfa 0)
  (def-ibcast vpsubq-bcast #xfb 1)
  (def-ibcast vpandd-bcast #xdb 0)
  (def-ibcast vpandq-bcast #xdb 1)
  (def-ibcast vpandnd-bcast #xdf 0)
  (def-ibcast vpandnq-bcast #xdf 1)
  (def-ibcast vpord-bcast #xeb 0)
  (def-ibcast vporq-bcast #xeb 1)
  (def-ibcast vpxord-bcast #xef 0)
  (def-ibcast vpxorq-bcast #xef 1))

(macrolet ((def-ibcast-38 (name opcode w)
             (let ((disp-n (if (= w 0) 4 8))
                   (bcast-list
                     (if (= w 0)
                         '((#b00 "{1to4}") (#b01 "{1to8}") (#b10 "{1to16}"))
                         '((#b00 "{1to2}") (#b01 "{1to4}") (#b10 "{1to8}")))))
               `(define-instruction ,name (segment dst src1 src2)
                  ,@(loop for (ll bcast) in bcast-list
                          append
                          (avx512-inst-printer-list
                           'ymm-ymm/mem #x66 opcode
                           :opcode-prefix #x0f38
                           :w w
                           :nds t
                           :ll ll
                           :disp-n disp-n
                           :evex-b 1
                           :printer (list :name :tab 'reg ", " 'vvvv ", "
                                          'reg/mem " " bcast)))
                  (:emitter
                   (aver (not (register-p src2))) ; must be memory
                   (emit-avx512-inst segment src2 dst #x66 ,opcode
                                     :opcode-prefix #x0f38
                                     :vvvv src1
                                     :w ,w
                                     :evex-b 1
                                     :disp-n ,disp-n))))))
  ;; Signed dword min/max
  (def-ibcast-38 vpminsd-bcast #x39 0)
  (def-ibcast-38 vpmaxsd-bcast #x3d 0)
  ;; Unsigned dword min/max
  (def-ibcast-38 vpminud-bcast #x3b 0)
  (def-ibcast-38 vpmaxud-bcast #x3f 0)
  ;; Signed qword min/max
  (def-ibcast-38 vpminsq-bcast #x39 1)
  (def-ibcast-38 vpmaxsq-bcast #x3d 1)
  ;; Unsigned qword min/max
  (def-ibcast-38 vpminuq-bcast #x3b 1)
  (def-ibcast-38 vpmaxuq-bcast #x3f 1)
  ;; FMA broadcast
  (def-ibcast-38 vfmadd132ps-bcast #x98 0)
  (def-ibcast-38 vfmadd132pd-bcast #x98 1)
  (def-ibcast-38 vfmadd213ps-bcast #xa8 0)
  (def-ibcast-38 vfmadd213pd-bcast #xa8 1)
  (def-ibcast-38 vfmadd231ps-bcast #xb8 0)
  (def-ibcast-38 vfmadd231pd-bcast #xb8 1)
  ;; FMA subtract broadcast
  (def-ibcast-38 vfmsub132ps-bcast #x9a 0)
  (def-ibcast-38 vfmsub132pd-bcast #x9a 1)
  (def-ibcast-38 vfmsub213ps-bcast #xaa 0)
  (def-ibcast-38 vfmsub213pd-bcast #xaa 1)
  (def-ibcast-38 vfmsub231ps-bcast #xba 0)
  (def-ibcast-38 vfmsub231pd-bcast #xba 1)
  ;; FMA negative-add broadcast
  (def-ibcast-38 vfnmadd132ps-bcast #x9c 0)
  (def-ibcast-38 vfnmadd132pd-bcast #x9c 1)
  (def-ibcast-38 vfnmadd213ps-bcast #xac 0)
  (def-ibcast-38 vfnmadd213pd-bcast #xac 1)
  (def-ibcast-38 vfnmadd231ps-bcast #xbc 0)
  (def-ibcast-38 vfnmadd231pd-bcast #xbc 1)
  ;; FMA add/subtract broadcast
  (def-ibcast-38 vfmaddsub132ps-bcast #x96 0)
  (def-ibcast-38 vfmaddsub132pd-bcast #x96 1)
  (def-ibcast-38 vfmaddsub213ps-bcast #xa6 0)
  (def-ibcast-38 vfmaddsub213pd-bcast #xa6 1)
  (def-ibcast-38 vfmaddsub231ps-bcast #xb6 0)
  (def-ibcast-38 vfmaddsub231pd-bcast #xb6 1)
  ;; FMA subtract/add broadcast
  (def-ibcast-38 vfmsubadd132ps-bcast #x97 0)
  (def-ibcast-38 vfmsubadd132pd-bcast #x97 1)
  (def-ibcast-38 vfmsubadd213ps-bcast #xa7 0)
  (def-ibcast-38 vfmsubadd213pd-bcast #xa7 1)
  (def-ibcast-38 vfmsubadd231ps-bcast #xb7 0)
  (def-ibcast-38 vfmsubadd231pd-bcast #xb7 1)
  ;; VNNI dot-product broadcast
  (def-ibcast-38 vpdpbusd-bcast  #x50 0)
  (def-ibcast-38 vpdpbusds-bcast #x51 0)
  (def-ibcast-38 vpdpwssd-bcast  #x52 0)
  (def-ibcast-38 vpdpwssds-bcast #x53 0)
  ;; Scale broadcast
  (def-ibcast-38 vscalefps-bcast #x2c 0)
  (def-ibcast-38 vscalefpd-bcast #x2c 1))

;;;; ---- Immediate broadcast (2- and 3-operand) ----
(macrolet
    ((def-bcast-3a (name prefix opcode w &key (nds nil))
       (let* ((disp-n (if (= w 0) 4 8))
              (bcast-list
                (if (= w 0)
                    '((#b00 "{1to4}") (#b01 "{1to8}") (#b10 "{1to16}"))
                    '((#b00 "{1to2}") (#b01 "{1to4}") (#b10 "{1to8}"))))
              (printer-forms
                (loop for (ll bcast) in bcast-list
                      append
                      (avx512-inst-printer-list
                       'ymm-ymm/mem-imm prefix opcode
                       :opcode-prefix #x0f3a
                       :w w
                       :nds nds
                       :ll ll
                       :disp-n disp-n
                       :evex-b 1
                       :printer
                       (if nds
                           (list :name :tab 'reg ", " 'vvvv ", "
                                 'reg/mem " " bcast)
                           (list :name :tab 'reg ", " 'reg/mem " " bcast))))))
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
                                        :opcode-prefix #x0f3a
                                        :vvvv src1
                                        :w ,w
                                        :evex-b 1
                                        :disp-n ,disp-n
                                        :remaining-bytes 1)
                      (emit-byte segment imm))
                   `(progn
                      (aver (not (register-p src)))
                      (emit-avx512-inst segment src dst ,prefix ,opcode
                                        :opcode-prefix #x0f3a
                                        :w ,w
                                        :evex-b 1
                                        :disp-n ,disp-n
                                        :remaining-bytes 1)
                      (emit-byte segment imm))))))))
  ;; 2-operand immediate broadcast forms
  (def-bcast-3a vreduceps-bcast #x66 #x56 0)
  (def-bcast-3a vreducepd-bcast #x66 #x56 1)
  (def-bcast-3a vrndscaleps-bcast #x66 #x08 0)
  (def-bcast-3a vrndscalepd-bcast #x66 #x09 1)
  ;; 3-operand immediate broadcast forms
  (def-bcast-3a vfixupimmps-bcast #x66 #x54 0 :nds t)
  (def-bcast-3a vfixupimmpd-bcast #x66 #x54 1 :nds t))

;;;; ---- EVEX gather/scatter (ZMM width) ----

;;; EVEX gather: dst {k1}, vm (index in vector register, mask in k1-k7)
;;; Usage: (inst vpgatherqq-z dst (ea disp base zmm-index scale) mask)
;;;   where mask is 1-7 (must be k1-k7; k0 not allowed for gather/scatter)
;;;   The CPU reads 8 qwords from [base + zmm-index[i]*scale + disp] for
;;;   each lane i where k1 bit i is set; lane's mask bit is cleared on load.
(macrolet ((def (name opcode w)
             (let ((vsib-arg-type (if (= w 0)
                                      'evex-vsib-disp4
                                      'evex-vsib-disp8)))
               `(define-instruction ,name (segment dst vm mask)
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list
                           'ymm-ymm/mem #x66 opcode
                           :opcode-prefix #x0f38
                           :w w
                           :nds t
                           :more-fields `((aaa ,k)
                                          (z-bit 0)
                                          (reg/mem nil :type ',vsib-arg-type))
                           :printer '(:name :tab reg ", " reg/mem " {" aaa "}")))
                  (:emitter
                   (aver (and (integerp mask) (<= 1 mask 7)))
                   (emit-avx512-inst segment vm dst #x66 ,opcode
                                     :opcode-prefix #x0f38
                                     :w ,w
                                     :aaa mask
                                     :vm t
                                     :disp-n ,(if (= w 0) 4 8)))))))
  (def vpgatherdd-z  #x90 0)
  (def vpgatherdq-z  #x90 1)
  (def vgatherdps-z  #x92 0)
  (def vgatherdpd-z  #x92 1)
  (def vpgatherqd-z  #x91 0)
  (def vpgatherqq-z  #x91 1)
  (def vgatherqps-z  #x93 0)
  (def vgatherqpd-z  #x93 1))

;;; Zeroing EVEX gather variants
(macrolet ((def-zero (name opcode w)
             (let ((zero-name (symbolicate name "-ZERO"))
                   (vsib-arg-type (if (= w 0)
                                      'evex-vsib-disp4
                                      'evex-vsib-disp8)))
               `(define-instruction ,zero-name (segment dst vm mask)
                  ,@(loop for k from 1 to 7
                          append
                          (avx512-inst-printer-list
                           'ymm-ymm/mem #x66 opcode
                           :opcode-prefix #x0f38
                           :w w
                           :nds t
                           :more-fields `((aaa ,k)
                                          (z-bit 1)
                                          (reg/mem nil :type ',vsib-arg-type))
                           :printer '(:name :tab reg ", " reg/mem
                                      " {" aaa "}{z}")))
                  (:emitter
                   (aver (and (integerp mask) (<= 1 mask 7)))
                   (emit-avx512-inst segment vm dst #x66 ,opcode
                                     :opcode-prefix #x0f38
                                     :w ,w
                                     :aaa mask
                                     :vm t
                                     :z 1
                                     :disp-n ,(if (= w 0) 4 8)))))))
  (def-zero vpgatherdd-z  #x90 0)
  (def-zero vpgatherdq-z  #x90 1)
  (def-zero vgatherdps-z  #x92 0)
  (def-zero vgatherdpd-z  #x92 1)
  (def-zero vpgatherqd-z  #x91 0)
  (def-zero vpgatherqq-z  #x91 1)
  (def-zero vgatherqps-z  #x93 0)
  (def-zero vgatherqpd-z  #x93 1))

;;; EVEX scatter: vm {k1}, src (reverse direction)
;;; Usage: (inst vpscatterqq-z (ea disp base zmm-index scale) src mask)
(macrolet ((def (name opcode w)
             (let ((vsib-arg-type (if (= w 0)
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

;;; Broadcast mask bit to vector register
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #xf3 opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst #xf3 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vpbroadcastmb2q #x2a 1)
  (def vpbroadcastmw2d #x3a 0))

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
                        append
                        (loop for k2num from 1 to 7
                              append
                              (avx512-inst-printer-list
                               '2mask-nds prefix opcode
                               :opcode-prefix #x0f38
                               :w w
                               :ll #b10
                               :disp-n 64
                               :more-fields `((aaa ,k2num)
                                              (reg ,k1num :type 'kreg))
                               :printer '(:name :tab reg ", " aaa ", " vvvv ", " reg/mem))))
                (:emitter
                 (aver (k-register-p k1))
                 (aver (k-register-p k2))
                 (aver (not (zerop (reg-id-num (reg-id k1)))))
                 (aver (not (zerop (reg-id-num (reg-id k2)))))
                 (emit-avx512-inst segment src2 k1 ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :ll #b10
                                   :vvvv src1
                                   :aaa (reg-id-num (reg-id k2))
                                   :disp-n 64)))))
  (def vp2intersectd #x68 #xf2 0)
  (def vp2intersectq #x68 #xf2 1))

;;;; ---- Scalar FMA masked/zeroing ----

(macrolet
    ((def-scalar-fma-masked (name prefix opcode w &key z)
       (let* ((z-bit (if z 1 0))
              (ins-name (symbolicate name (if z "-MASKED-Z" "-MASKED")))
              (mask-printer
                (if z
                    '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}{z}")
                    '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}")))
              (disp-n (if (= w 0) 4 8))
              (printer-forms
                (loop for k from 1 to 7
                      append
                      (avx512-inst-printer-list
                       'ymm-ymm/mem prefix opcode
                       :opcode-prefix #x0f38
                       :w w
                       :nds t
                       :ll #b00
                       :disp-n disp-n
                       :evex-b 0
                       :more-fields (list (list 'aaa k)
                                          (list 'z-bit z-bit))
                       :printer mask-printer))))
         `(define-instruction ,ins-name (segment dst src1 src2 mask)
            ,@printer-forms
            (:emitter
              (aver (and (integerp mask) (<= 1 mask 7)))
              (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                :opcode-prefix #x0f38
                                :vvvv src1
                                :w ,w
                                :aaa mask
                                :z ,z-bit
                                :disp-n ,disp-n))))))

  ;; Masked scalar FMA forms
  (def-scalar-fma-masked vfmadd132ss #x66 #x99 0)
  (def-scalar-fma-masked vfmadd132sd #x66 #x99 1)
  (def-scalar-fma-masked vfmadd213ss #x66 #xa9 0)
  (def-scalar-fma-masked vfmadd213sd #x66 #xa9 1)
  (def-scalar-fma-masked vfmadd231ss #x66 #xb9 0)
  (def-scalar-fma-masked vfmadd231sd #x66 #xb9 1)

  (def-scalar-fma-masked vfmsub132ss #x66 #x9b 0)
  (def-scalar-fma-masked vfmsub132sd #x66 #x9b 1)
  (def-scalar-fma-masked vfmsub213ss #x66 #xab 0)
  (def-scalar-fma-masked vfmsub213sd #x66 #xab 1)
  (def-scalar-fma-masked vfmsub231ss #x66 #xbb 0)
  (def-scalar-fma-masked vfmsub231sd #x66 #xbb 1)

  (def-scalar-fma-masked vfnmadd132ss #x66 #x9d 0)
  (def-scalar-fma-masked vfnmadd132sd #x66 #x9d 1)
  (def-scalar-fma-masked vfnmadd213ss #x66 #xad 0)
  (def-scalar-fma-masked vfnmadd213sd #x66 #xad 1)
  (def-scalar-fma-masked vfnmadd231ss #x66 #xbd 0)
  (def-scalar-fma-masked vfnmadd231sd #x66 #xbd 1)

  ;; Zeroing scalar FMA forms
  (def-scalar-fma-masked vfmadd132ss #x66 #x99 0 :z t)
  (def-scalar-fma-masked vfmadd132sd #x66 #x99 1 :z t)
  (def-scalar-fma-masked vfmadd213ss #x66 #xa9 0 :z t)
  (def-scalar-fma-masked vfmadd213sd #x66 #xa9 1 :z t)
  (def-scalar-fma-masked vfmadd231ss #x66 #xb9 0 :z t)
  (def-scalar-fma-masked vfmadd231sd #x66 #xb9 1 :z t)

  (def-scalar-fma-masked vfmsub132ss #x66 #x9b 0 :z t)
  (def-scalar-fma-masked vfmsub132sd #x66 #x9b 1 :z t)
  (def-scalar-fma-masked vfmsub213ss #x66 #xab 0 :z t)
  (def-scalar-fma-masked vfmsub213sd #x66 #xab 1 :z t)
  (def-scalar-fma-masked vfmsub231ss #x66 #xbb 0 :z t)
  (def-scalar-fma-masked vfmsub231sd #x66 #xbb 1 :z t)

  (def-scalar-fma-masked vfnmadd132ss #x66 #x9d 0 :z t)
  (def-scalar-fma-masked vfnmadd132sd #x66 #x9d 1 :z t)
  (def-scalar-fma-masked vfnmadd213ss #x66 #xad 0 :z t)
  (def-scalar-fma-masked vfnmadd213sd #x66 #xad 1 :z t)
  (def-scalar-fma-masked vfnmadd231ss #x66 #xbd 0 :z t)
  (def-scalar-fma-masked vfnmadd231sd #x66 #xbd 1 :z t)
  ;; Scalar reciprocal/rsqrt masked
  (def-scalar-fma-masked vrcp14ss #x66 #x4d 0)
  (def-scalar-fma-masked vrcp14sd #x66 #x4d 1)
  (def-scalar-fma-masked vrsqrt14ss #x66 #x4f 0)
  (def-scalar-fma-masked vrsqrt14sd #x66 #x4f 1)
  ;; Scalar reciprocal/rsqrt zeroing
  (def-scalar-fma-masked vrcp14ss #x66 #x4d 0 :z t)
  (def-scalar-fma-masked vrcp14sd #x66 #x4d 1 :z t)
  (def-scalar-fma-masked vrsqrt14ss #x66 #x4f 0 :z t)
  (def-scalar-fma-masked vrsqrt14sd #x66 #x4f 1 :z t)
  ;; Scalar getexp masked
  (def-scalar-fma-masked vgetexpss #x66 #x43 0)
  (def-scalar-fma-masked vgetexpsd #x66 #x43 1)
  ;; Scalar scale masked
  (def-scalar-fma-masked vscalefss #x66 #x2d 0)
  (def-scalar-fma-masked vscalefsd #x66 #x2d 1)
  ;; Scalar getexp zeroing
  (def-scalar-fma-masked vgetexpss #x66 #x43 0 :z t)
  (def-scalar-fma-masked vgetexpsd #x66 #x43 1 :z t)
  ;; Scalar scale zeroing
  (def-scalar-fma-masked vscalefss #x66 #x2d 0 :z t)
  (def-scalar-fma-masked vscalefsd #x66 #x2d 1 :z t))

;;;; ---- BF16 masked/zeroing ----

;;; 3-operand masked/zeroing
(macrolet
    ((def-bf16-masked (name prefix opcode w &key z)
       (let* ((z-bit (if z 1 0))
              (ins-name (symbolicate name (if z "-MASKED-Z" "-MASKED")))
              (mask-printer
                (if z
                    '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}{z}")
                    '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}")))
              (printer-forms
                (loop for k from 1 to 7
                      append
                      (loop for ll in '(#b00 #b01 #b10)
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix #x0f38
                             :w w
                             :nds t
                             :ll ll
                             :disp-n (case ll
                                       (#b00 16)
                                       (#b01 32)
                                       (#b10 64))
                             :evex-b 0
                             :more-fields (list (list 'aaa k)
                                                (list 'z-bit z-bit))
                             :printer mask-printer)))))
         `(define-instruction ,ins-name (segment dst src1 src2 mask)
            ,@printer-forms
            (:emitter
              (aver (and (integerp mask) (<= 1 mask 7)))
              (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                :opcode-prefix #x0f38
                                :vvvv src1
                                :w ,w
                                :aaa mask
                                :z ,z-bit
                                :disp-n (cond ((zmm-register-p dst) 64)
                                              ((ymm-register-p dst) 32)
                                              ((xmm-register-p dst) 16)
                                              (t 0))))))))

  ;; Masked and zeroing BF16 forms
  (def-bf16-masked vcvtne2ps2bf16 #xf2 #x72 0)
  (def-bf16-masked vdpbf16ps     #xf2 #x52 0)
  (def-bf16-masked vcvtne2ps2bf16 #xf2 #x72 0 :z t)
  (def-bf16-masked vdpbf16ps     #xf2 #x52 0 :z t))

;;; 2-operand masked/zeroing
(macrolet
    ((def-bf16-2op-masked (name prefix opcode w &key z)
       (let* ((z-bit (if z 1 0))
              (ins-name (symbolicate name (if z "-MASKED-Z" "-MASKED")))
              (mask-printer
                (if z
                    '(:name :tab reg ", " reg/mem " {" aaa "}{z}")
                    '(:name :tab reg ", " reg/mem " {" aaa "}")))
              (printer-forms
                (loop for k from 1 to 7
                      append
                      (loop for ll in '(#b00 #b01 #b10)
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix #x0f38
                             :w w
                             :ll ll
                             :disp-n (case ll
                                       (#b00 16)
                                       (#b01 32)
                                       (#b10 64))
                             :evex-b 0
                             :more-fields (list (list 'aaa k)
                                                (list 'z-bit z-bit))
                             :printer mask-printer)))))
         `(define-instruction ,ins-name (segment dst src mask)
            ,@printer-forms
            (:emitter
              (aver (and (integerp mask) (<= 1 mask 7)))
              (emit-avx512-inst segment src dst ,prefix ,opcode
                                :opcode-prefix #x0f38
                                :w ,w
                                :aaa mask
                                :z ,z-bit
                                :disp-n (cond ((zmm-register-p dst) 64)
                                              ((ymm-register-p dst) 32)
                                              ((xmm-register-p dst) 16)
                                              (t 0))))))))
  ;; Masked and zeroing forms
  (def-bf16-2op-masked vcvtneps2bf16 #xf2 #x73 0)
  (def-bf16-2op-masked vcvtneps2bf16 #xf2 #x73 0 :z t))

;;;; ---- Range masked/zeroing/broadcast ----

(macrolet
    ((def-range-masked (name prefix opcode w &key z)
       (let* ((z-bit (if z 1 0))
              (ins-name (symbolicate name (if z "-MASKED-Z" "-MASKED")))
              (mask-printer
                (if z
                    '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}{z}")
                    '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}")))
              (printer-forms
                (loop for k from 1 to 7
                      append
                      (loop for ll in '(#b00 #b01 #b10)
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem-imm prefix opcode
                             :opcode-prefix #x0f3a
                             :w w
                             :nds t
                             :ll ll
                             :disp-n 64
                             :evex-b 0
                             :more-fields (list (list 'aaa k)
                                                (list 'z-bit z-bit))
                             :printer mask-printer)))))
         `(define-instruction ,ins-name (segment dst src1 src2 mask imm)
            ,@printer-forms
            (:emitter
             (aver (and (integerp mask) (<= 1 mask 7)))
             (emit-avx512-inst segment src2 dst ,prefix ,opcode
                               :opcode-prefix #x0f3a
                               :vvvv src1
                               :w ,w
                               :aaa mask
                               :z ,z-bit
                               :disp-n
                               (cond ((zmm-register-p dst) 64)
                                     ((ymm-register-p dst) 32)
                                     ((xmm-register-p dst) 16)
                                     (t 0))
                               :remaining-bytes 1)
             (emit-byte segment imm)))))

     (def-range-bcast (name prefix opcode w)
       (let* ((disp-n (if (= w 0) 4 8))
              (bcast-list
                (if (= w 0)
                    '((#b00 "{1to4}") (#b01 "{1to8}") (#b10 "{1to16}"))
                    '((#b00 "{1to2}") (#b01 "{1to4}") (#b10 "{1to8}"))))
              (printer-forms
                (loop for (ll bcast) in bcast-list
                      append
                      (avx512-inst-printer-list
                       'ymm-ymm/mem-imm prefix opcode
                       :opcode-prefix #x0f3a
                       :w w
                       :nds t
                       :ll ll
                       :disp-n disp-n
                       :evex-b 1
                       :printer (list :name :tab 'reg ", " 'vvvv ", "
                                      'reg/mem " " bcast)))))
         `(define-instruction ,name (segment dst src1 src2 imm)
            ,@printer-forms
            (:emitter
             (aver (not (register-p src2)))
             (emit-avx512-inst segment src2 dst ,prefix ,opcode
                               :opcode-prefix #x0f3a
                               :vvvv src1
                               :w ,w
                               :evex-b 1
                               :disp-n ,disp-n
                               :remaining-bytes 1)
             (emit-byte segment imm))))))
  ;; Masked and zeroing forms
  (def-range-masked vrangeps #x66 #x50 0)
  (def-range-masked vrangepd #x66 #x50 1)
  (def-range-masked vrangeps #x66 #x50 0 :z t)
  (def-range-masked vrangepd #x66 #x50 1 :z t)
  ;; Broadcast forms
  (def-range-bcast vrangeps-bcast #x66 #x50 0)
  (def-range-bcast vrangepd-bcast #x66 #x50 1)
  ;; Fixup masked
  (def-range-masked vfixupimmps #x66 #x54 0)
  (def-range-masked vfixupimmpd #x66 #x54 1)
  ;; Fixup zeroing masked
  (def-range-masked vfixupimmps #x66 #x54 0 :z t)
  (def-range-masked vfixupimmpd #x66 #x54 1 :z t)
  ;; GFNI affine masked
  (def-range-masked vgf2p8affineqb    #x66 #xce 1)
  (def-range-masked vgf2p8affineinvqb #x66 #xcf 1)
  ;; GFNI affine zeroing masked
  (def-range-masked vgf2p8affineqb    #x66 #xce 1 :z t)
  (def-range-masked vgf2p8affineinvqb #x66 #xcf 1 :z t)
  ;; VBMI2 immediate shift double masked
  (def-range-masked vpshldw #x66 #x70 1)
  (def-range-masked vpshldd #x66 #x71 0)
  (def-range-masked vpshldq #x66 #x71 1)
  (def-range-masked vpshrdw #x66 #x72 1)
  (def-range-masked vpshrdd #x66 #x73 0)
  (def-range-masked vpshrdq #x66 #x73 1)
  ;; VBMI2 immediate shift double zeroing masked
  (def-range-masked vpshldw #x66 #x70 1 :z t)
  (def-range-masked vpshldd #x66 #x71 0 :z t)
  (def-range-masked vpshldq #x66 #x71 1 :z t)
  (def-range-masked vpshrdw #x66 #x72 1 :z t)
  (def-range-masked vpshrdd #x66 #x73 0 :z t)
  (def-range-masked vpshrdq #x66 #x73 1 :z t))

;;;; ---- Popcount masked/zeroing ----
(macrolet
    ((def-2op-masked (name prefix opcode w &key z store-p)
       (let* ((z-bit (if z 1 0))
              (ins-name (symbolicate name (if z "-MASKED-Z" "-MASKED")))
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
                      append
                      (loop for ll in '(#b00 #b01 #b10)
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix #x0f38
                             :w w
                             :ll ll
                             :disp-n (case ll
                                       (#b00 16)
                                       (#b01 32)
                                       (#b10 64))
                             :evex-b 0
                             :more-fields (list (list 'aaa k)
                                                (list 'z-bit z-bit))
                             :printer mask-printer)))))
         `(define-instruction ,ins-name (segment dst src mask)
            ,@printer-forms
            (:emitter
              (aver (and (integerp mask) (<= 1 mask 7)))
              ,(if store-p
                   `(emit-avx512-inst segment dst src ,prefix ,opcode
                                      :opcode-prefix #x0f38
                                      :w ,w
                                      :aaa mask
                                      :z ,z-bit
                                      :disp-n
                                      (cond ((zmm-register-p src) 64)
                                            ((ymm-register-p src) 32)
                                            ((xmm-register-p src) 16)
                                            (t 0)))
                   `(emit-avx512-inst segment src dst ,prefix ,opcode
                                      :opcode-prefix #x0f38
                                      :w ,w
                                      :aaa mask
                                      :z ,z-bit
                                      :disp-n
                                      (cond ((zmm-register-p dst) 64)
                                            ((ymm-register-p dst) 32)
                                            ((xmm-register-p dst) 16)
                                            (t 0)))))))))
  ;; Masked and zeroing popcount forms
  (def-2op-masked vpopcntd #x66 #x55 0)
  (def-2op-masked vpopcntq #x66 #x55 1)
  (def-2op-masked vpopcntb #x66 #x54 0)
  (def-2op-masked vpopcntw #x66 #x54 1)
  (def-2op-masked vpopcntd #x66 #x55 0 :z t)
  (def-2op-masked vpopcntq #x66 #x55 1 :z t)
  (def-2op-masked vpopcntb #x66 #x54 0 :z t)
  (def-2op-masked vpopcntw #x66 #x54 1 :z t)
  ;; Get exponent masked/zeroing
  (def-2op-masked vgetexpps #x66 #x42 0)
  (def-2op-masked vgetexppd #x66 #x42 1)
  (def-2op-masked vgetexpps #x66 #x42 0 :z t)
  (def-2op-masked vgetexppd #x66 #x42 1 :z t)
  ;; Absolute value qword masked/zeroing
  (def-2op-masked vpabsq #x66 #x1f 1)
  (def-2op-masked vpabsq #x66 #x1f 1 :z t)
  ;; AVX-512CD masked/zeroing
  (def-2op-masked vpconflictd #x66 #xc4 0)
  (def-2op-masked vpconflictq #x66 #xc4 1)
  (def-2op-masked vplzcntd #x66 #x44 0)
  (def-2op-masked vplzcntq #x66 #x44 1)

  (def-2op-masked vpconflictd #x66 #xc4 0 :z t)
  (def-2op-masked vpconflictq #x66 #xc4 1 :z t)
  (def-2op-masked vplzcntd #x66 #x44 0 :z t)
  (def-2op-masked vplzcntq #x66 #x44 1 :z t)
  ;; Compress masked/zeroing
  (def-2op-masked vcompressps  #x66 #x8a 0 :store-p t)
  (def-2op-masked vcompresspd  #x66 #x8a 1 :store-p t)
  (def-2op-masked vpcompressd  #x66 #x8b 0 :store-p t)
  (def-2op-masked vpcompressq  #x66 #x8b 1 :store-p t)
  (def-2op-masked vpcompressb  #x66 #x63 0 :store-p t)
  (def-2op-masked vpcompressw  #x66 #x63 1 :store-p t)

  (def-2op-masked vcompressps  #x66 #x8a 0 :store-p t :z t)
  (def-2op-masked vcompresspd  #x66 #x8a 1 :store-p t :z t)
  (def-2op-masked vpcompressd  #x66 #x8b 0 :store-p t :z t)
  (def-2op-masked vpcompressq  #x66 #x8b 1 :store-p t :z t)
  (def-2op-masked vpcompressb  #x66 #x63 0 :store-p t :z t)
  (def-2op-masked vpcompressw  #x66 #x63 1 :store-p t :z t)
  ;; Expand masked/zeroing (load direction, default store-p=nil)
  (def-2op-masked vexpandps  #x66 #x88 0)
  (def-2op-masked vexpandpd  #x66 #x88 1)
  (def-2op-masked vpexpandd  #x66 #x89 0)
  (def-2op-masked vpexpandq  #x66 #x89 1)
  (def-2op-masked vpexpandb  #x66 #x62 0)
  (def-2op-masked vpexpandw  #x66 #x62 1)

  (def-2op-masked vexpandps  #x66 #x88 0 :z t)
  (def-2op-masked vexpandpd  #x66 #x88 1 :z t)
  (def-2op-masked vpexpandd  #x66 #x89 0 :z t)
  (def-2op-masked vpexpandq  #x66 #x89 1 :z t)
  (def-2op-masked vpexpandb  #x66 #x62 0 :z t)
  (def-2op-masked vpexpandw  #x66 #x62 1 :z t))

;;;; ---- Reciprocal/rsqrt masked/zeroing/broadcast ----

(macrolet
    ((def-2op-masked (name prefix opcode w &key z)
       (let* ((z-bit (if z 1 0))
              (ins-name (symbolicate name (if z "-MASKED-Z" "-MASKED")))
              (mask-printer
                (if z
                    '(:name :tab reg ", " reg/mem " {" aaa "}{z}")
                    '(:name :tab reg ", " reg/mem " {" aaa "}")))
              (printer-forms
                (loop for k from 1 to 7
                      append
                      (loop for ll in '(#b00 #b01 #b10)
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix #x0f38
                             :w w
                             :ll ll
                             :disp-n (case ll
                                       (#b00 16)
                                       (#b01 32)
                                       (#b10 64))
                             :evex-b 0
                             :more-fields (list (list 'aaa k)
                                                (list 'z-bit z-bit))
                             :printer mask-printer)))))
         `(define-instruction ,ins-name (segment dst src mask)
            ,@printer-forms
            (:emitter
             (aver (and (integerp mask) (<= 1 mask 7)))
             (emit-avx512-inst segment src dst ,prefix ,opcode
                               :opcode-prefix #x0f38
                               :w ,w
                               :aaa mask
                               :z ,z-bit
                               :disp-n
                               (cond ((zmm-register-p dst) 64)
                                     ((ymm-register-p dst) 32)
                                     ((xmm-register-p dst) 16)
                                     (t 0)))))))

     (def-2op-bcast (name prefix opcode w)
       (let* ((disp-n (if (= w 0) 4 8))
              (bcast-list
                (if (= w 0)
                    '((#b00 "{1to4}") (#b01 "{1to8}") (#b10 "{1to16}"))
                    '((#b00 "{1to2}") (#b01 "{1to4}") (#b10 "{1to8}"))))
              (printer-forms
                (loop for (ll bcast) in bcast-list
                      append
                      (avx512-inst-printer-list
                       'ymm-ymm/mem prefix opcode
                       :opcode-prefix #x0f38
                       :w w
                       :ll ll
                       :disp-n disp-n
                       :evex-b 1
                       :printer (list :name :tab 'reg ", " 'reg/mem " " bcast)))))
         `(define-instruction ,name (segment dst src)
            ,@printer-forms
            (:emitter
             (aver (not (register-p src)))
             (emit-avx512-inst segment src dst ,prefix ,opcode
                               :opcode-prefix #x0f38
                               :w ,w
                               :evex-b 1
                               :disp-n ,disp-n))))))
  ;; Masked and zeroing forms
  (def-2op-masked vrcp14ps #x66 #x4c 0)
  (def-2op-masked vrcp14pd #x66 #x4c 1)
  (def-2op-masked vrsqrt14ps #x66 #x4e 0)
  (def-2op-masked vrsqrt14pd #x66 #x4e 1)
  (def-2op-masked vrcp14ps #x66 #x4c 0 :z t)
  (def-2op-masked vrcp14pd #x66 #x4c 1 :z t)
  (def-2op-masked vrsqrt14ps #x66 #x4e 0 :z t)
  (def-2op-masked vrsqrt14pd #x66 #x4e 1 :z t)
  ;; Broadcast forms
  (def-2op-bcast vrcp14ps-bcast #x66 #x4c 0)
  (def-2op-bcast vrcp14pd-bcast #x66 #x4c 1)
  (def-2op-bcast vrsqrt14ps-bcast #x66 #x4e 0)
  (def-2op-bcast vrsqrt14pd-bcast #x66 #x4e 1))

;;;; ---- Reduce/round 2-operand masked/zeroing ----

(macrolet
    ((def-2op-imm-masked (name prefix opcode w &key z)
       (let* ((z-bit (if z 1 0))
              (ins-name (symbolicate name (if z "-MASKED-Z" "-MASKED")))
              (mask-printer
                (if z
                    '(:name :tab reg ", " reg/mem " {" aaa "}{z}")
                    '(:name :tab reg ", " reg/mem " {" aaa "}")))
              (printer-forms
                (loop for k from 1 to 7
                      append
                      (loop for ll in '(#b00 #b01 #b10)
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem-imm prefix opcode
                             :opcode-prefix #x0f3a
                             :w w
                             :ll ll
                             :disp-n 64
                             :evex-b 0
                             :more-fields (list (list 'aaa k)
                                                (list 'z-bit z-bit))
                             :printer mask-printer)))))
         `(define-instruction ,ins-name (segment dst src mask imm)
            ,@printer-forms
            (:emitter
              (aver (and (integerp mask) (<= 1 mask 7)))
              (emit-avx512-inst segment src dst ,prefix ,opcode
                                :opcode-prefix #x0f3a
                                :w ,w
                                :aaa mask
                                :z ,z-bit
                                :disp-n
                                (cond ((zmm-register-p dst) 64)
                                      ((ymm-register-p dst) 32)
                                      ((xmm-register-p dst) 16)
                                      (t 0))
                                :remaining-bytes 1)
              (emit-byte segment imm))))))
  ;; Reduce masked and zeroing
  (def-2op-imm-masked vreduceps #x66 #x56 0)
  (def-2op-imm-masked vreducepd #x66 #x56 1)
  (def-2op-imm-masked vreduceps #x66 #x56 0 :z t)
  (def-2op-imm-masked vreducepd #x66 #x56 1 :z t)
  ;; Round masked and zeroing
  (def-2op-imm-masked vrndscaleps #x66 #x08 0)
  (def-2op-imm-masked vrndscalepd #x66 #x09 1)
  (def-2op-imm-masked vrndscaleps #x66 #x08 0 :z t)
  (def-2op-imm-masked vrndscalepd #x66 #x09 1 :z t)
  ;; Get mantissa masked/zeroing
  (def-2op-imm-masked vgetmantps #x66 #x26 0)
  (def-2op-imm-masked vgetmantpd #x66 #x26 1)
  (def-2op-imm-masked vgetmantps #x66 #x26 0 :z t)
  (def-2op-imm-masked vgetmantpd #x66 #x26 1 :z t))

;;;; ---- Scalar immediate masked/zeroing ----

(macrolet
    ((def-scalar-imm-masked (name prefix opcode w &key z (nds nil))
       (let* ((z-bit (if z 1 0))
              (ins-name (symbolicate name (if z "-MASKED-Z" "-MASKED")))
              (mask-printer
                (if z
                    (if nds
                        '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}{z}")
                        '(:name :tab reg ", " reg/mem " {" aaa "}{z}"))
                    (if nds
                        '(:name :tab reg ", " vvvv ", " reg/mem " {" aaa "}")
                        '(:name :tab reg ", " reg/mem " {" aaa "}"))))
              (disp-n (if (= w 0) 4 8))
              (printer-forms
                (loop for k from 1 to 7
                      append
                      (avx512-inst-printer-list
                       'ymm-ymm/mem-imm prefix opcode
                       :opcode-prefix #x0f3a
                       :w w
                       :nds nds
                       :ll #b00
                       :disp-n disp-n
                       :evex-b 0
                       :more-fields (list (list 'aaa k)
                                          (list 'z-bit z-bit))
                       :printer mask-printer))))
         `(define-instruction ,ins-name
              ,(if nds
                   `(segment dst src1 src2 mask imm)
                   `(segment dst src mask imm))
            ,@printer-forms
            (:emitter
              (aver (and (integerp mask) (<= 1 mask 7)))
              ,(if nds
                   `(progn
                      (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                        :opcode-prefix #x0f3a
                                        :vvvv src1
                                        :w ,w
                                        :aaa mask
                                        :z ,z-bit
                                        :disp-n ,disp-n
                                        :remaining-bytes 1)
                      (emit-byte segment imm))
                   `(progn
                      (emit-avx512-inst segment src dst ,prefix ,opcode
                                        :opcode-prefix #x0f3a
                                        :w ,w
                                        :aaa mask
                                        :z ,z-bit
                                        :disp-n ,disp-n
                                        :remaining-bytes 1)
                      (emit-byte segment imm))))))))

  ;; 3-operand immediate scalar masked/zeroing
  (def-scalar-imm-masked vgetmantss #x66 #x27 0 :nds t)
  (def-scalar-imm-masked vgetmantsd #x66 #x27 1 :nds t)
  (def-scalar-imm-masked vrangess #x66 #x51 0 :nds t)
  (def-scalar-imm-masked vrangesd #x66 #x51 1 :nds t)
  (def-scalar-imm-masked vreducess #x66 #x57 0 :nds t)
  (def-scalar-imm-masked vreducesd #x66 #x57 1 :nds t)
  (def-scalar-imm-masked vfixupimmss #x66 #x55 0 :nds t)
  (def-scalar-imm-masked vfixupimmsd #x66 #x55 1 :nds t)

  (def-scalar-imm-masked vgetmantss #x66 #x27 0 :nds t :z t)
  (def-scalar-imm-masked vgetmantsd #x66 #x27 1 :nds t :z t)
  (def-scalar-imm-masked vrangess #x66 #x51 0 :nds t :z t)
  (def-scalar-imm-masked vrangesd #x66 #x51 1 :nds t :z t)
  (def-scalar-imm-masked vreducess #x66 #x57 0 :nds t :z t)
  (def-scalar-imm-masked vreducesd #x66 #x57 1 :nds t :z t)
  (def-scalar-imm-masked vfixupimmss #x66 #x55 0 :nds t :z t)
  (def-scalar-imm-masked vfixupimmsd #x66 #x55 1 :nds t :z t)

  ;; 2-operand immediate scalar masked/zeroing
  (def-scalar-imm-masked vrndscaless #x66 #x0a 0)
  (def-scalar-imm-masked vrndscalesd #x66 #x0b 1)
  (def-scalar-imm-masked vrndscaless #x66 #x0a 0 :z t)
  (def-scalar-imm-masked vrndscalesd #x66 #x0b 1 :z t))

;;;; ---- BF16 embedded broadcast ----

(macrolet
    ((def-bf16-bcast (name prefix opcode w)
       (let* ((disp-n (if (= w 0) 4 8))
              (bcast-list
                (if (= w 0)
                    '((#b00 "{1to4}") (#b01 "{1to8}") (#b10 "{1to16}"))
                    '((#b00 "{1to2}") (#b01 "{1to4}") (#b10 "{1to8}"))))
              (printer-forms
                (loop for (ll bcast) in bcast-list
                      append
                      (avx512-inst-printer-list
                       'ymm-ymm/mem prefix opcode
                       :opcode-prefix #x0f38
                       :w w
                       :nds t
                       :ll ll
                       :disp-n disp-n
                       :evex-b 1
                       :printer (list :name :tab 'reg ", " 'vvvv ", "
                                      'reg/mem " " bcast)))))
         `(define-instruction ,name (segment dst src1 src2)
            ,@printer-forms
            (:emitter
             (aver (not (register-p src2)))
             (emit-avx512-inst segment src2 dst ,prefix ,opcode
                               :opcode-prefix #x0f38
                               :vvvv src1
                               :w ,w
                               :evex-b 1
                               :disp-n ,disp-n))))))
  ;; BF16 3-operand broadcast forms
  (def-bf16-bcast vcvtne2ps2bf16-bcast #xf2 #x72 0)
  (def-bf16-bcast vdpbf16ps-bcast      #xf2 #x52 0))

;;;; ---- F16C vcvtph2ps masked/zeroing ----

(macrolet
    ((def-f16c-masked (name prefix opcode &key z)
       (let* ((z-bit (if z 1 0))
              (ins-name (symbolicate name (if z "-MASKED-Z" "-MASKED")))
              (mask-printer
                (if z
                    '(:name :tab reg ", " reg/mem " {" aaa "}{z}")
                    '(:name :tab reg ", " reg/mem " {" aaa "}")))
              (printer-forms
                (loop for k from 1 to 7
                      append
                      (loop for (ll n) in '((#b00 8) (#b01 16) (#b10 32))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem prefix opcode
                             :opcode-prefix #x0f38
                             :w 0
                             :ll ll
                             :disp-n n
                             :evex-b 0
                             :more-fields (list (list 'aaa k)
                                                (list 'z-bit z-bit))
                             :printer mask-printer)))))
         `(define-instruction ,ins-name (segment dst src mask)
            ,@printer-forms
            (:emitter
              (aver (and (integerp mask) (<= 1 mask 7)))
              (emit-avx512-inst segment src dst ,prefix ,opcode
                                :opcode-prefix #x0f38
                                :w 0
                                :aaa mask
                                :z ,z-bit
                                :disp-n
                                (cond ((zmm-register-p dst) 32)
                                      ((ymm-register-p dst) 16)
                                      ((xmm-register-p dst) 8)
                                      (t 0))))))))

  ;; Masked and zeroing forms
  (def-f16c-masked vcvtph2ps #x66 #x13)
  (def-f16c-masked vcvtph2ps #x66 #x13 :z t))

;;;; ---- F16C vcvtps2ph masked/zeroing ----

(macrolet
    ((def-f16c-store-masked (name prefix opcode &key z)
       (let* ((z-bit (if z 1 0))
              (ins-name (symbolicate name (if z "-MASKED-Z" "-MASKED")))
              (mask-printer
                (if z
                    '(:name :tab reg/mem ", " reg " {" aaa "}{z}")
                    '(:name :tab reg/mem ", " reg " {" aaa "}")))
              (printer-forms
                (loop for k from 1 to 7
                      append
                      (loop for (ll n) in '((#b00 8) (#b01 16) (#b10 32))
                            append
                            (avx512-inst-printer-list
                             'ymm-ymm/mem-imm prefix opcode
                             :opcode-prefix #x0f3a
                             :w 0
                             :ll ll
                             :disp-n n
                             :evex-b 0
                             :more-fields (list (list 'aaa k)
                                                (list 'z-bit z-bit))
                             :printer mask-printer)))))
         `(define-instruction ,ins-name (segment dst src mask imm)
            ,@printer-forms
            (:emitter
              (aver (and (integerp mask) (<= 1 mask 7)))
              (emit-avx512-inst segment dst src ,prefix ,opcode
                                :opcode-prefix #x0f3a
                                :w 0
                                :aaa mask
                                :z ,z-bit
                                :disp-n
                                (cond ((zmm-register-p src) 32)
                                      ((ymm-register-p src) 16)
                                      ((xmm-register-p src) 8)
                                      (t 0))
                                :remaining-bytes 1)
              (emit-byte segment imm))))))

  ;; Masked and zeroing forms
  (def-f16c-store-masked vcvtps2ph #x66 #x1d)
  (def-f16c-store-masked vcvtps2ph #x66 #x1d :z t))
