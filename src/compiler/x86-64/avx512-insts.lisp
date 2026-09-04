(in-package "SB-X86-64-ASM")

;;;; AVX-512 Instruction Support
;;;;
;;;; Implemented subsets:
;;;;   AVX-512F, AVX-512BW, AVX-512DQ, AVX-512CD, AVX-512IFMA,
;;;;   AVX-512VBMI, AVX-512VBMI2, AVX-512VPOPCNTDQ, AVX-512BITALG,
;;;;   AVX-512VNNI, AVX-512BF16,
;;;;   GFNI (in avx2-insts.lisp),
;;;;   VPCLMULQDQ-256/512 (via VEX auto-promotion to EVEX),
;;;;   EVEX Compare-to-Opmask (vcmpps, vcmppd, vcmpss, vcmpsd),
;;;;   EVEX Masked Arithmetic & Logic (vadd*, vsub*, vmul*, vdiv*, vsqrt*, vpadd*, vpsub*, vpand*, vpor*, vpxor* with {k} and {z}),
;;;;   EVEX Broadcasts (vbroadcasts*, vpbroadcast* from memory, XMM, and GPRs),
;;;;   EVEX Opmask Transfers & Manipulation (kmov*, kand*, kor*, kxor*, knot*, etc.),
;;;;   EVEX Gather & Scatter (vpgather*, vgather*, vpscatter*, vscatter*).
;;;;
;;;; Not yet implemented / Future extensions:
;;;;   AVX-512VL  - Explicit EVEX 128/256-bit forms with masking/broadcast
;;;;                (auto-promotion handles basic ZMM; full VL needs
;;;;                explicit EVEX for XMM/YMM with masking)
;;;;   AVX-512FP16 - Half-precision FP16 arithmetic (~100 instructions)
;;;;   VAES-256/512 - Wide forms of vaesenc/vaesdec
;;;;   VP2INTERSECT - vp2intersectd/q
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

(macrolet ((def (name prefix)
             `(define-instruction ,name (segment dst src &optional src2)
                ,@(avx2-inst-printer-list 'ymm-ymm/mem-dir prefix #b0001000)
                (:emitter
                 (cond ((ea-p src)
                        (if (zmm-register-p dst)
                            (emit-avx512-inst segment src dst ,prefix #x10)
                            (emit-avx2-inst segment src dst ,prefix #x10 :l 0)))

                       ((and (ea-p dst) (zmm-register-p src))
                        (emit-avx512-inst segment dst src ,prefix #x11))

                       ((and (integerp src) src2 (register-p src2))
                        (if (or (zmm-register-p dst) (zmm-register-p src2))
                            (emit-avx512-inst segment src2 dst ,prefix #x10)
                            (emit-avx2-inst segment src2 dst ,prefix #x10 :l 0)))

                       ((and src2 (or (zmm-register-p dst)
                                      (zmm-register-p src)
                                      (zmm-register-p src2)))
                        (emit-avx512-inst segment src dst ,prefix #x10 :vvvv src2))

                       ((or (zmm-register-p dst)
                            (zmm-register-p src))
                        (emit-avx512-inst segment src dst ,prefix #x10))

                       ((and src src2 dst (xmm-register-p dst))
                        (emit-avx2-inst segment src dst ,prefix #x10 :vvvv src2 :l 0))

                       ((xmm-register-p dst)
                        (emit-avx2-inst segment src dst ,prefix #x10 :l 0))

                       (t
                        (aver (xmm-register-p src))
                        (emit-avx2-inst segment dst src ,prefix #x11 :l 0)))))))
  (def vmovsd #xf2)
  (def vmovss #xf3))

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
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vpermt2d  #x7e 0)
  (def vpermt2q  #x7e 1)
  (def vpermt2ps #x7f 0)
  (def vpermt2pd #x7f 1))

;;; Cross-lane shuffle with immediate
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm #x66 opcode
                                            :opcode-prefix #x0f3a :w w)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vshuff32x4  #x23 0)
  (def vshuff64x2  #x23 1)
  (def vshufi32x4  #x43 0)
  (def vshufi64x2  #x43 1))

;;; Blend with mask
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vblendmps #x65 0)
  (def vblendmpd #x65 1)
  (def vpblendmd #x64 0)
  (def vpblendmq #x64 1))

;;; Compress store
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix #x0f38 :w w
                                            :printer '(:name :tab reg/mem ", " reg))
                (:emitter
                 (emit-avx512-inst segment dst src ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vcompressps  #x66 #x8a 0)
  (def vcompresspd  #x66 #x8a 1)
  (def vpcompressd  #x66 #x8b 0)
  (def vpcompressq  #x66 #x8b 1))

;;; Expand load
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vexpandps  #x66 #x88 0)
  (def vexpandpd  #x66 #x88 1)
  (def vpexpandd  #x66 #x89 0)
  (def vpexpandq  #x66 #x89 1))

;;; Down-convert (pack and store)
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix #x0f38 :w w
                                            :printer '(:name :tab reg/mem ", " reg))
                (:emitter
                 (emit-avx512-inst segment dst src ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vpmovqd  #xf3 #x35 0)
  (def vpmovqw  #xf3 #x34 0)
  (def vpmovqb  #xf3 #x32 0)
  (def vpmovdw  #xf3 #x33 0)
  (def vpmovdb  #xf3 #x31 0)
  (def vpmovwb  #xf3 #x30 0))

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
(macrolet ((def (name opcode w)
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
  (def vrndscaleps #x08 0)
  (def vrndscalepd #x09 1)
  (def vrndscaless #x0a 0)
  (def vrndscalesd #x0b 1))

;;; Fixup
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm #x66 opcode
                                            :opcode-prefix #x0f3a :w w)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vfixupimmps #x54 0)
  (def vfixupimmpd #x54 1)
  (def vfixupimmss #x55 0)
  (def vfixupimmsd #x55 1))

;;; Range
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm #x66 opcode
                                            :opcode-prefix #x0f3a :w w)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vrangeps #x50 0)
  (def vrangepd #x50 1))

;;; Reduce
(macrolet ((def (name opcode w)
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
  (def vreduceps #x56 0)
  (def vreducepd #x56 1))

;;; Scale
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vscalefps #x2c 0)
  (def vscalefpd #x2c 1)
  (def vscalefss #x2d 0)
  (def vscalefsd #x2d 1))

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
(macrolet ((def-insert (name prefix op w)
             `(define-instruction ,name (segment dst src src2 imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm prefix op
                                            :w w
                                            :opcode-prefix #x0f3a)
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,op
                                   :opcode-prefix #x0f3a
                                   :vvvv src
                                   :w ,w
                                   :remaining-bytes 1)
                 (emit-byte segment imm))))
           (def-extract (name prefix op w)
             `(define-instruction ,name (segment dst src imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm prefix op
                                            :w w
                                            :opcode-prefix #x0f3a
                                            :printer '(:name :tab reg/mem ", " reg ", " imm))
                (:emitter
                 (emit-avx512-inst segment dst src ,prefix ,op
                                   :w ,w
                                   :opcode-prefix #x0f3a
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def-insert vinsertf32x4  #x66 #x18 0)
  (def-insert vinsertf64x2  #x66 #x18 1)
  (def-insert vinsertf32x8  #x66 #x1a 0)
  (def-insert vinsertf64x4  #x66 #x1a 1)
  (def-insert vinserti32x4  #x66 #x38 0)
  (def-insert vinserti64x2  #x66 #x38 1)
  (def-insert vinserti32x8  #x66 #x3a 0)
  (def-insert vinserti64x4  #x66 #x3a 1)
  (def-extract vextractf32x4 #x66 #x19 0)
  (def-extract vextractf64x2 #x66 #x19 1)
  (def-extract vextractf32x8 #x66 #x1b 0)
  (def-extract vextractf64x4 #x66 #x1b 1)
  (def-extract vextracti32x4 #x66 #x39 0)
  (def-extract vextracti64x2 #x66 #x39 1)
  (def-extract vextracti32x8 #x66 #x3b 0)
  (def-extract vextracti64x4 #x66 #x3b 1))

;;;; ---- AVX-512F additional instructions ----

;;; 3-operand NDS (dst, src1, src2)
(macrolet ((def (name prefix opcode w &optional (opcode-prefix #x0f38))
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix opcode-prefix :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix ,opcode-prefix :vvvv src1 :w ,w)))))
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
  (def vpxorq     #x66 #xef 1 #x0f))

;;; 2-operand (dst, src)
(macrolet ((def (name prefix opcode w &optional (opcode-prefix #x0f38))
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix opcode-prefix :w w)
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix ,opcode-prefix :w ,w)))))
  (def vpabsq     #x66 #x1f 1)
  (def vrcp14ps   #x66 #x4c 0)
  (def vrcp14pd   #x66 #x4c 1)
  (def vrsqrt14ps #x66 #x4e 0)
  (def vrsqrt14pd #x66 #x4e 1))

;;; Scalar reciprocal approximations (3-operand NDS)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vrcp14ss   #x4d 0)
  (def vrcp14sd   #x4d 1)
  (def vrsqrt14ss #x4f 0)
  (def vrsqrt14sd #x4f 1))

;;; 3-operand NDS + imm8
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm #x66 opcode
                                            :opcode-prefix #x0f3a :w w)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def valignd    #x03 0)
  (def valignq    #x03 1)
  (def vrangess   #x51 0)
  (def vrangesd   #x51 1)
  (def vreducess  #x57 0)
  (def vreducesd  #x57 1)
  (def vgetmantss #x27 0)
  (def vgetmantsd #x27 1))

;;; Scalar getexp (3-operand NDS)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vgetexpss  #x43 0)
  (def vgetexpsd  #x43 1))

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
(macrolet ((def (name prefix opcode w &optional (opcode-prefix #x0f))
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix opcode-prefix :w w)
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix ,opcode-prefix :w ,w)))))
  (def vcvtps2udq  nil  #x79 0)
  (def vcvtpd2udq  nil  #x79 1)
  (def vcvttps2udq nil  #x78 0)
  (def vcvttpd2udq nil  #x78 1)
  (def vcvtudq2ps  #xf2 #x7a 0)
  (def vcvtudq2pd  #xf3 #x7a 0))

;;; Scalar unsigned conversions (2-operand, dst=gpr)
(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'reg-ymm/mem prefix opcode :w 0)
                ,@(avx512-inst-printer-list 'reg-ymm/mem prefix opcode :w 1)
                (:emitter
                 (aver (gpr-p dst))
                 (let ((dst-size (operand-size dst)))
                   (aver (or (eq dst-size :qword) (eq dst-size :dword)))
                   (emit-avx512-inst segment src dst ,prefix ,opcode
                                     :w (ecase dst-size
                                          (:qword 1)
                                          (:dword 0))))))))
  (def vcvtss2usi  #xf3 #x79)
  (def vcvtsd2usi  #xf2 #x79)
  (def vcvttss2usi #xf3 #x78)
  (def vcvttsd2usi #xf2 #x78))

;;; Scalar unsigned convert to FP (3-operand NDS)
(macrolet ((def (name prefix opcode)
             `(define-instruction ,name (segment dst src src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :nds t)
                (:emitter
                 (aver (xmm-register-p dst))
                 (let ((src-size (operand-size src2)))
                   (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                     :vvvv src
                                     :w (case src-size
                                          (:qword 1)
                                          (:dword 0)
                                          (t 1))))))))
  (def vcvtusi2ss #xf3 #x7b)
  (def vcvtusi2sd #xf2 #x7b))

;;; Saturating truncations (reversed encoding: src in reg, dst in r/m)
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix #x0f38 :w w
                                            :printer '(:name :tab reg/mem ", " reg))
                (:emitter
                 (emit-avx512-inst segment dst src ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vpmovsqd  #xf3 #x25 0)
  (def vpmovsqw  #xf3 #x24 0)
  (def vpmovsqb  #xf3 #x22 0)
  (def vpmovusqd #xf3 #x15 0)
  (def vpmovusqw #xf3 #x14 0)
  (def vpmovusqb #xf3 #x12 0)
  (def vpmovsdw  #xf3 #x23 0)
  (def vpmovsdb  #xf3 #x21 0)
  (def vpmovusdw #xf3 #x13 0)
  (def vpmovusdb #xf3 #x11 0)
  (def vpmovswb  #xf3 #x20 0)
  (def vpmovuswb #xf3 #x10 0))

;;; Broadcast-memory
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vbroadcastf32x4 #x1a 0)
  (def vbroadcastf64x4 #x1b 1)
  (def vbroadcasti32x4 #x5a 0)
  (def vbroadcasti64x4 #x5b 1))

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
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                                            :opcode-prefix #x0f3a :w w
                                            :more-fields '((reg nil :type 'opmask-reg)))
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vpcmpd   #x66 #x1f 0)
  (def vpcmpud  #x66 #x1e 0)
  (def vpcmpq   #x66 #x1f 1)
  (def vpcmpuq  #x66 #x1e 1))

;;; Test-to-k (kdst, src1, src2) - k-reg in ModR/M reg
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix #x0f38 :w w :nds t
                                            :more-fields '((reg nil :type 'opmask-reg)))
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vptestmd  #x66 #x27 0)
  (def vptestmq  #x66 #x27 1)
  (def vptestnmd #xf3 #x27 0)
  (def vptestnmq #xf3 #x27 1))

;;;; ---- AVX-512BW instructions ----

;;; Blend with mask (byte/word)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vpblendmb #x66 0)
  (def vpblendmw #x66 1))

;;; Compare byte/word to k - k-reg in ModR/M reg
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm prefix opcode
                                            :opcode-prefix #x0f3a :w w
                                            :more-fields '((reg nil :type 'opmask-reg)))
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vpcmpb    #x66 #x3f 0)
  (def vpcmpub   #x66 #x3e 0)
  (def vpcmpw    #x66 #x3f 1)
  (def vpcmpuw   #x66 #x3e 1))

;;; Test byte/word to k - k-reg in ModR/M reg
(macrolet ((def (name prefix opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix #x0f38 :w w :nds t
                                            :more-fields '((reg nil :type 'opmask-reg)))
                (:emitter
                 (emit-avx512-inst segment src2 dst ,prefix ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
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
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vpermw    #x8d 1)
  (def vpermi2w  #x75 1)
  (def vpermt2w  #x7d 1))

;;; Variable shift word
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
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
                     :remaining-bytes 1)
   (emit-byte segment imm))
  . #.(avx512-inst-printer-list 'ymm-ymm/mem-imm #x66 #x42
                                :opcode-prefix #x0f3a :w 0))

;;;; ---- AVX-512DQ instructions ----

;;; FP classify
(macrolet ((def (name opcode w)
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
  (def vfpclassps #x66 0)
  (def vfpclasspd #x66 1)
  (def vfpclassss #x67 0)
  (def vfpclasssd #x67 1))

;;; Multiply low qword
(define-instruction vpmullq (segment dst src1 src2)
  (:emitter
   (emit-avx512-inst segment src2 dst #x66 #x40
                     :opcode-prefix #x0f38
                     :vvvv src1
                     :w 1))
  . #.(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x40
                                :opcode-prefix #x0f38 :w 1 :nds t))

;;; Convert packed integers to/from FP (DQ extensions)
(macrolet ((def (name prefix opcode w &optional (opcode-prefix #x0f))
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem prefix opcode
                                            :opcode-prefix opcode-prefix :w w)
                (:emitter
                 (emit-avx512-inst segment src dst ,prefix ,opcode
                                   :opcode-prefix ,opcode-prefix :w ,w)))))
  (def vcvtps2qq   #x66 #x7b 0)
  (def vcvtpd2qq   #x66 #x7b 1)
  (def vcvtps2uqq  #x66 #x79 0)
  (def vcvtpd2uqq  #x66 #x79 1)
  (def vcvttps2qq  #x66 #x7a 0)
  (def vcvttpd2qq  #x66 #x7a 1)
  (def vcvttps2uqq #x66 #x78 0)
  (def vcvttpd2uqq #x66 #x78 1)
  (def vcvtqq2ps   nil  #x5b 1)
  (def vcvtqq2pd   #xf3 #xe6 1)
  (def vcvtuqq2ps  #xf2 #x7a 1)
  (def vcvtuqq2pd  #xf3 #x7a 1))

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
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vbroadcastf32x2 #x19 0)
  (def vbroadcastf64x2 #x1a 1)
  (def vbroadcasti32x2 #x59 0)
  (def vbroadcasti64x2 #x5a 1)
  (def vbroadcastf32x8 #x1b 0)
  (def vbroadcasti32x8 #x5b 0))

;;;; ---- AVX-512IFMA instructions ----

(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vpmadd52luq #xb4 1)
  (def vpmadd52huq #xb5 1))

;;;; ---- AVX-512VBMI instructions ----

(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vpermb       #x8d 0)
  (def vpermi2b     #x75 0)
  (def vpermt2b     #x7d 0)
  (def vpmultishiftqb #x83 1))

;;;; ---- AVX-512VBMI2 instructions ----

;;; Compress byte/word (reversed encoding)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w
                                            :printer '(:name :tab reg/mem ", " reg))
                (:emitter
                 (emit-avx512-inst segment dst src #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vpcompressb #x63 0)
  (def vpcompressw #x63 1))

;;; Expand byte/word
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vpexpandb #x62 0)
  (def vpexpandw #x62 1))

;;; Concatenate and shift (immediate)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2 imm)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem-imm #x66 opcode
                                            :opcode-prefix #x0f3a :w w)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f3a
                                   :vvvv src1
                                   :w ,w
                                   :remaining-bytes 1)
                 (emit-byte segment imm)))))
  (def vpshldw   #x70 1)
  (def vpshldd   #x71 0)
  (def vpshldq   #x71 1)
  (def vpshrdw   #x72 1)
  (def vpshrdd   #x73 0)
  (def vpshrdq   #x73 1))

;;; Concatenate and shift (variable)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src1 src2)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w :nds t)
                (:emitter
                 (emit-avx512-inst segment src2 dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :vvvv src1
                                   :w ,w)))))
  (def vpshldvw  #x70 1)
  (def vpshldvd  #x71 0)
  (def vpshldvq  #x71 1)
  (def vpshrdvw  #x72 1)
  (def vpshrdvd  #x73 0)
  (def vpshrdvq  #x73 1))

;;;; ---- AVX-512VPOPCNTDQ instructions ----

(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vpopcntd  #x55 0)
  (def vpopcntq  #x55 1))

;;;; ---- AVX-512BITALG instructions ----

(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst src)
                ,@(avx512-inst-printer-list 'ymm-ymm/mem #x66 opcode
                                            :opcode-prefix #x0f38 :w w)
                (:emitter
                 (emit-avx512-inst segment src dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w)))))
  (def vpopcntb  #x54 0)
  (def vpopcntw  #x54 1))

;;; Shuffle bits (result to k)
(define-instruction vpshufbitqmb (segment dst src1 src2)
  (:emitter
   (emit-avx512-inst segment src2 dst #x66 #x8f
                     :opcode-prefix #x0f38
                     :vvvv src1
                     :w 0))
  . #.(avx512-inst-printer-list 'ymm-ymm/mem #x66 #x8f
                                :opcode-prefix #x0f38 :w 0 :nds t))

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
  (def vmaxps-masked   nil  #x5f 0))

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

;;;; ---- EVEX gather/scatter (ZMM width) ----

;;; EVEX gather: dst {k1}, vm (index in vector register, mask in k1-k7)
;;; Usage: (inst vpgatherqq-z dst (ea disp base zmm-index scale) mask)
;;;   where mask is 1-7 (must be k1-k7; k0 not allowed for gather/scatter)
;;;   The CPU reads 8 qwords from [base + zmm-index[i]*scale + disp] for
;;;   each lane i where k1 bit i is set; lane's mask bit is cleared on load.
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment dst vm mask)
                (:emitter
                 (aver (and (integerp mask) (<= 1 mask 7)))
                 (emit-avx512-inst segment vm dst #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :aaa mask
                                   :vm t)))))
  ;; Dword destinations (8 lanes, YMM dst; index is ZMM qword)
  (def vpgatherqd-z #x91 1)
  (def vgatherqps-z #x93 1)
  ;; Qword destinations (8 lanes, ZMM dst; index is ZMM qword)
  (def vpgatherqq-z #x91 1)
  (def vgatherqpd-z #x93 1)
  ;; Dword indices (16 lanes for W0, 8 for W1 with YMM index)
  (def vpgatherdd-z #x90 0)
  (def vgatherdps-z #x92 0)
  (def vpgatherdq-z #x90 1)
  (def vgatherdpd-z #x92 1))

;;; EVEX scatter: vm {k1}, src (reverse direction)
;;; Usage: (inst vpscatterqq-z (ea disp base zmm-index scale) src mask)
(macrolet ((def (name opcode w)
             `(define-instruction ,name (segment vm src mask)
                (:emitter
                 (aver (and (integerp mask) (<= 1 mask 7)))
                 (emit-avx512-inst segment vm src #x66 ,opcode
                                   :opcode-prefix #x0f38
                                   :w ,w
                                   :aaa mask
                                   :vm t)))))
  (def vpscatterqd-z #xa1 1)
  (def vscatterqps-z #xa3 1)
  (def vpscatterqq-z #xa1 1)
  (def vscatterqpd-z #xa3 1)
  (def vpscatterdd-z #xa0 0)
  (def vscatterdps-z #xa2 0)
  (def vpscatterdq-z #xa0 1)
  (def vscatterdpd-z #xa2 1))

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
