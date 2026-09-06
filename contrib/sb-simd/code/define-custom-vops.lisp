(in-package #:sb-vm)

(macrolet
    ((define-custom-vop (name &body clauses)
       (with-accessors ((name sb-simd-internals:instruction-record-name)
                        (vop sb-simd-internals:instruction-record-vop)
                        (argument-records sb-simd-internals:instruction-record-argument-records)
                        (result-records sb-simd-internals:instruction-record-result-records)
                        (cost sb-simd-internals:instruction-record-cost)
                        (encoding sb-simd-internals:instruction-record-encoding)
                        (instruction-set sb-simd-internals:instruction-record-instruction-set))
           (sb-simd-internals:find-function-record name)
         (assert (eq encoding :custom))
         (when (sb-simd-internals:instruction-set-available-p instruction-set)
           (labels ((find-clauses (key)
                      (remove key clauses :test-not #'eq :key #'first))
                    (find-clause (key)
                      (let ((found (find-clauses key)))
                        (assert (= 1 (length found)))
                        (rest (first found)))))
             `(sb-c:define-vop (,vop)
                (:translate ,vop)
                (:policy :fast-safe)
                (:arg-types ,@(mapcar #'sb-simd-internals:value-record-primitive-type argument-records))
                (:result-types ,@(mapcar #'sb-simd-internals:value-record-primitive-type result-records))
                (:args
                 ,@(loop for arg in (find-clause :args)
                         for argument-record in argument-records
                         collect `(,@arg :scs ,(sb-simd-internals:value-record-scs argument-record))))
                ,@(find-clauses :info)
                ,@(find-clauses :temporary)
                (:results
                 ,@(loop for result in (find-clause :results)
                         for result-record in result-records
                         collect `(,@result :scs ,(sb-simd-internals:value-record-scs result-record))))
                (:generator ,cost ,@(find-clause :generator))))))))
#+x86-64
(progn
  ;; SSE
  (macrolet ((def (name cmp)
               `(define-custom-vop ,name
                    (:args (a :target tmp) (b))
                  (:temporary (:sc single-reg :from (:argument 0)) tmp)
                  (:results (dst))
                  (:generator
                   (unless (location= a tmp)
                     (inst xorps tmp tmp)
                     (inst movss tmp a))
                   (inst cmpss ,cmp tmp b)
                   (inst movq dst tmp)))))
    (def sb-simd-sse::two-arg-f32= :eq)
    (def sb-simd-sse::two-arg-f32/= :neq)
    (def sb-simd-sse::two-arg-f32< :lt)
    (def sb-simd-sse::two-arg-f32<= :le)
    (def sb-simd-sse::two-arg-f32> :nle)
    (def sb-simd-sse::two-arg-f32>= :nlt))
  (define-custom-vop sb-simd-sse::f32-from-s64
      (:args (src))
    (:results (dst))
    (:generator
     (inst xorps dst dst)
     (inst cvtsi2ss dst src)))
  (define-custom-vop sb-simd-sse::f32!-from-p128
      (:args (src :target dst))
    (:temporary (:sc single-sse-reg :from (:argument 0)) tmp)
    (:results (dst))
    (:generator
     (move tmp src)
     (inst xorps dst dst)
     (inst movss dst tmp)))
  ;; SSE2
  (macrolet ((def (name cmp)
               `(define-custom-vop ,name
                    (:args (a :target tmp) (b))
                  (:temporary (:sc single-reg :from (:argument 0)) tmp)
                  (:results (dst))
                  (:generator
                   (unless (location= a tmp)
                     (inst xorpd tmp tmp)
                     (inst movsd tmp a))
                   (inst cmpsd ,cmp tmp b)
                   (inst movq dst tmp)))))
    (def sb-simd-sse2::two-arg-f64= :eq)
    (def sb-simd-sse2::two-arg-f64/= :neq)
    (def sb-simd-sse2::two-arg-f64< :lt)
    (def sb-simd-sse2::two-arg-f64<= :le)
    (def sb-simd-sse2::two-arg-f64> :nle)
    (def sb-simd-sse2::two-arg-f64>= :nlt))
  (define-custom-vop sb-simd-sse2::f64-from-s64
      (:args (src))
    (:results (dst))
    (:generator
     (inst xorpd dst dst)
     (inst cvtsi2sd dst src)))
  (define-custom-vop sb-simd-sse2::f64!-from-p128
      (:args (src :target tmp))
    (:temporary (:sc double-sse-reg :from (:argument 0)) tmp)
    (:results (dst))
    (:generator
     (move tmp src)
     (inst xorpd dst dst)
     (inst movsd dst tmp)))
  ;; AVX
  (macrolet ((def (name cmp)
               `(define-custom-vop ,name
                    (:args (a :target tmp) (b))
                  (:temporary (:sc single-reg :from (:argument 0)) tmp)
                  (:results (dst))
                  (:generator
                   (unless (location= a tmp)
                     (inst vxorps tmp tmp tmp))
                   (inst vcmpss ,cmp tmp a b)
                   (inst vmovq dst tmp)))))
    (def sb-simd-avx::two-arg-f32= :eq)
    (def sb-simd-avx::two-arg-f32/= :neq)
    (def sb-simd-avx::two-arg-f32< :lt)
    (def sb-simd-avx::two-arg-f32<= :le)
    (def sb-simd-avx::two-arg-f32> :nle)
    (def sb-simd-avx::two-arg-f32>= :nlt))
  (macrolet ((def (name cmp)
               `(define-custom-vop ,name
                    (:args (a :target tmp) (b))
                  (:temporary (:sc single-reg :from (:argument 0)) tmp)
                  (:results (dst))
                  (:generator
                   (unless (location= a tmp)
                     (inst vxorpd tmp tmp tmp))
                   (inst vcmpsd ,cmp tmp a b)
                   (inst vmovq dst tmp)))))
    (def sb-simd-avx::two-arg-f64= :eq)
    (def sb-simd-avx::two-arg-f64/= :neq)
    (def sb-simd-avx::two-arg-f64< :lt)
    (def sb-simd-avx::two-arg-f64<= :le)
    (def sb-simd-avx::two-arg-f64> :nle)
    (def sb-simd-avx::two-arg-f64>= :nlt))
  (define-custom-vop sb-simd-avx::f32-from-s64
      (:args (src :to :save))
    (:results (dst))
    (:generator
     (inst vxorpd dst dst dst)
     (inst vcvtsi2ss dst dst src)))
  (define-custom-vop sb-simd-avx::f64-from-s64
      (:args (src :to :save))
    (:results (dst))
    (:generator
     (inst vxorpd dst dst dst)
     (inst vcvtsi2sd dst dst src)))
  (define-custom-vop sb-simd-avx::f32!-from-p128
      (:args (src :to :save))
    (:results (dst))
    (:generator
     (inst vxorps dst dst dst)
     (inst movss dst src)))
  (define-custom-vop sb-simd-avx::f32!-from-p256
      (:args (src :to :save))
    (:results (dst))
    (:generator
     (inst vxorps dst dst dst)
     (inst movss dst src)))
  (define-custom-vop sb-simd-avx::f64!-from-p128
      (:args (src :to :save))
    (:results (dst))
    (:generator
     (inst vxorpd dst dst dst)
     (inst movsd dst src)))
  (define-custom-vop sb-simd-avx::f64!-from-p256
      (:args (src :to :save))
    (:results (dst))
    (:generator
     (inst vxorpd dst dst dst)
     (inst movsd dst src)))
  (define-custom-vop sb-simd-avx512f::f32!-from-p512
      (:args (src :to :save))
    (:results (dst))
    (:generator
     (inst vxorps dst dst dst)
     (inst vmovss dst dst src)))
  (define-custom-vop sb-simd-avx512f::f64!-from-p512
      (:args (src :to :save))
    (:results (dst))
    (:generator
     (inst vxorpd dst dst dst)
     (inst vmovsd dst dst src)))
  ;; AVX-512F
  (macrolet ((def-f32 (name cmp)
               `(define-custom-vop ,name
                    (:args (a) (b))
                  (:temporary (:sc mask-reg) k)
                  (:results (dst))
                  (:generator
                   (inst vcmpps ,cmp k a b)
                   (inst vpmovm2d dst k))))
             (def-f64 (name cmp)
               `(define-custom-vop ,name
                    (:args (a) (b))
                  (:temporary (:sc mask-reg) k)
                  (:results (dst))
                  (:generator
                   (inst vcmppd ,cmp k a b)
                   (inst vpmovm2q dst k))))
             (def-i32 (name inst imm)
               `(define-custom-vop ,name
                    (:args (a) (b))
                  (:temporary (:sc mask-reg) k)
                  (:results (dst))
                  (:generator
                   (inst ,inst k a b ,imm)
                   (inst vpmovm2d dst k))))
             (def-i64 (name inst imm)
               `(define-custom-vop ,name
                    (:args (a) (b))
                  (:temporary (:sc mask-reg) k)
                  (:results (dst))
                  (:generator
                   (inst ,inst k a b ,imm)
                   (inst vpmovm2q dst k))))
             (def-i8 (name inst imm)
               `(define-custom-vop ,name
                    (:args (a) (b))
                  (:temporary (:sc mask-reg) k)
                  (:results (dst))
                  (:generator
                   (inst ,inst k a b ,imm)
                   (inst vpmovm2b dst k))))
             (def-i16 (name inst imm)
               `(define-custom-vop ,name
                    (:args (a) (b))
                  (:temporary (:sc mask-reg) k)
                  (:results (dst))
                  (:generator
                   (inst ,inst k a b ,imm)
                   (inst vpmovm2w dst k)))))
    (def-f32 sb-simd-avx512f::two-arg-f32.16=  :eq)
    (def-f32 sb-simd-avx512f::two-arg-f32.16/= :neq)
    (def-f32 sb-simd-avx512f::two-arg-f32.16<  :lt)
    (def-f32 sb-simd-avx512f::two-arg-f32.16<= :le)
    (def-f32 sb-simd-avx512f::two-arg-f32.16>  :gt)
    (def-f32 sb-simd-avx512f::two-arg-f32.16>= :ge)

    (def-f64 sb-simd-avx512f::two-arg-f64.8=  :eq)
    (def-f64 sb-simd-avx512f::two-arg-f64.8/= :neq)
    (def-f64 sb-simd-avx512f::two-arg-f64.8<  :lt)
    (def-f64 sb-simd-avx512f::two-arg-f64.8<= :le)
    (def-f64 sb-simd-avx512f::two-arg-f64.8>  :gt)
    (def-f64 sb-simd-avx512f::two-arg-f64.8>= :ge)

    (def-i32 sb-simd-avx512f::two-arg-s32.16=  vpcmpd  0)
    (def-i32 sb-simd-avx512f::two-arg-s32.16/= vpcmpd  4)
    (def-i32 sb-simd-avx512f::two-arg-s32.16<  vpcmpd  1)
    (def-i32 sb-simd-avx512f::two-arg-s32.16<= vpcmpd  2)
    (def-i32 sb-simd-avx512f::two-arg-s32.16>  vpcmpd  6)
    (def-i32 sb-simd-avx512f::two-arg-s32.16>= vpcmpd  5)

    (def-i32 sb-simd-avx512f::two-arg-u32.16=  vpcmpud 0)
    (def-i32 sb-simd-avx512f::two-arg-u32.16/= vpcmpud 4)
    (def-i32 sb-simd-avx512f::two-arg-u32.16<  vpcmpud 1)
    (def-i32 sb-simd-avx512f::two-arg-u32.16<= vpcmpud 2)
    (def-i32 sb-simd-avx512f::two-arg-u32.16>  vpcmpud 6)
    (def-i32 sb-simd-avx512f::two-arg-u32.16>= vpcmpud 5)

    (def-i64 sb-simd-avx512f::two-arg-s64.8=  vpcmpq  0)
    (def-i64 sb-simd-avx512f::two-arg-s64.8/= vpcmpq  4)
    (def-i64 sb-simd-avx512f::two-arg-s64.8<  vpcmpq  1)
    (def-i64 sb-simd-avx512f::two-arg-s64.8<= vpcmpq  2)
    (def-i64 sb-simd-avx512f::two-arg-s64.8>  vpcmpq  6)
    (def-i64 sb-simd-avx512f::two-arg-s64.8>= vpcmpq  5)

    (def-i64 sb-simd-avx512f::two-arg-u64.8=  vpcmpuq 0)
    (def-i64 sb-simd-avx512f::two-arg-u64.8/= vpcmpuq 4)
    (def-i64 sb-simd-avx512f::two-arg-u64.8<  vpcmpuq 1)
    (def-i64 sb-simd-avx512f::two-arg-u64.8<= vpcmpuq 2)
    (def-i64 sb-simd-avx512f::two-arg-u64.8>  vpcmpuq 6)
    (def-i64 sb-simd-avx512f::two-arg-u64.8>= vpcmpuq 5)

    (def-i8 sb-simd-avx512bw::two-arg-s8.64=  vpcmpb  0)
    (def-i8 sb-simd-avx512bw::two-arg-s8.64/= vpcmpb  4)
    (def-i8 sb-simd-avx512bw::two-arg-s8.64<  vpcmpb  1)
    (def-i8 sb-simd-avx512bw::two-arg-s8.64<= vpcmpb  2)
    (def-i8 sb-simd-avx512bw::two-arg-s8.64>  vpcmpb  6)
    (def-i8 sb-simd-avx512bw::two-arg-s8.64>= vpcmpb  5)

    (def-i8 sb-simd-avx512bw::two-arg-u8.64=  vpcmpub 0)
    (def-i8 sb-simd-avx512bw::two-arg-u8.64/= vpcmpub 4)
    (def-i8 sb-simd-avx512bw::two-arg-u8.64<  vpcmpub 1)
    (def-i8 sb-simd-avx512bw::two-arg-u8.64<= vpcmpub 2)
    (def-i8 sb-simd-avx512bw::two-arg-u8.64>  vpcmpub 6)
    (def-i8 sb-simd-avx512bw::two-arg-u8.64>= vpcmpub 5)

    (def-i16 sb-simd-avx512bw::two-arg-s16.32=  vpcmpw  0)
    (def-i16 sb-simd-avx512bw::two-arg-s16.32/= vpcmpw  4)
    (def-i16 sb-simd-avx512bw::two-arg-s16.32<  vpcmpw  1)
    (def-i16 sb-simd-avx512bw::two-arg-s16.32<= vpcmpw  2)
    (def-i16 sb-simd-avx512bw::two-arg-s16.32>  vpcmpw  6)
    (def-i16 sb-simd-avx512bw::two-arg-s16.32>= vpcmpw  5)

    (def-i16 sb-simd-avx512bw::two-arg-u16.32=  vpcmpuw 0)
    (def-i16 sb-simd-avx512bw::two-arg-u16.32/= vpcmpuw 4)
    (def-i16 sb-simd-avx512bw::two-arg-u16.32<  vpcmpuw 1)
    (def-i16 sb-simd-avx512bw::two-arg-u16.32<= vpcmpuw 2)
    (def-i16 sb-simd-avx512bw::two-arg-u16.32>  vpcmpuw 6)
    (def-i16 sb-simd-avx512bw::two-arg-u16.32>= vpcmpuw 5))

  (define-custom-vop sb-simd-avx512f::f32.16-blend
      (:args (a) (b) (mask))
    (:temporary (:sc mask-reg) k)
    (:temporary (:sc single-avx512-reg) mask-reg tmp)
    (:temporary (:sc int-avx512-reg) zero)
    (:results (dst))
    (:generator
     (inst vpxorq zero zero zero)
     (inst vpcmpd k mask zero 1)
     (inst vpmovm2d mask-reg k)
     (inst vandps tmp b mask-reg)
     (inst vandnps dst mask-reg a)
     (inst vorps dst dst tmp)))

  (define-custom-vop sb-simd-avx512f::f64.8-blend
      (:args (a) (b) (mask))
    (:temporary (:sc mask-reg) k)
    (:temporary (:sc double-avx512-reg) mask-reg tmp)
    (:temporary (:sc int-avx512-reg) zero)
    (:results (dst))
    (:generator
     (inst vpxorq zero zero zero)
     (inst vpcmpq k mask zero 1)
     (inst vpmovm2q mask-reg k)
     (inst vandpd tmp b mask-reg)
     (inst vandnpd dst mask-reg a)
     (inst vorpd dst dst tmp)))

  (macrolet ((def-blend (name)
               `(define-custom-vop ,name
                    (:args (a) (b) (mask))
                  (:temporary (:sc mask-reg) k)
                  (:temporary (:sc int-avx512-reg) zero tmp mask-reg)
                  (:results (dst))
                  (:generator
                   (inst vpxorq zero zero zero)
                   (inst vpcmpb k mask zero 1)
                   (inst vpmovm2b mask-reg k)
                   (inst vpandd tmp b mask-reg)
                   (inst vpandnd dst mask-reg a)
                   (inst vpord dst dst tmp)))))
    (def-blend sb-simd-avx512f::u32.16-blend)
    (def-blend sb-simd-avx512f::s32.16-blend)
    (def-blend sb-simd-avx512f::u64.8-blend)
    (def-blend sb-simd-avx512f::s64.8-blend)
    (def-blend sb-simd-avx512bw::u8.64-blend)
    (def-blend sb-simd-avx512bw::s8.64-blend)
    (def-blend sb-simd-avx512bw::u16.32-blend)
    (def-blend sb-simd-avx512bw::s16.32-blend))

  (macrolet ((def-not (name)
               `(define-custom-vop ,name
                    (:args (a))
                  (:results (dst))
                  (:generator
                   (inst vpternlogd dst a a #x55)))))
    (def-not sb-simd-avx512f::f32.16-not)
    (def-not sb-simd-avx512f::f64.8-not)
    (def-not sb-simd-avx512f::u32.16-not)
    (def-not sb-simd-avx512f::u64.8-not)
    (def-not sb-simd-avx512f::s32.16-not)
    (def-not sb-simd-avx512f::s64.8-not)
    (def-not sb-simd-avx512bw::u8.64-not)
    (def-not sb-simd-avx512bw::u16.32-not)
    (def-not sb-simd-avx512bw::s8.64-not)
    (def-not sb-simd-avx512bw::s16.32-not)
    (def-not sb-simd-avx512fp16::f16.32-not)))
;; Neon
#+arm64
(progn
 (define-custom-vop sb-simd-neon::f32!-from-p128
     (:args (src :scs (single-neon-reg) :to :save))
   (:results (dst :scs (single-reg)))
   (:generator
    (inst movi dst 0 :4s)
    (inst dup dst src nil 0)))

 (define-custom-vop sb-simd-neon::f32.4!-from-f32
     (:args (src :scs (single-reg) :target dst))
   (:results (dst :scs (single-neon-reg)))
   (:generator
    (unless (location= src dst)
      (inst ins dst 0 src 0 :s))))

 (define-custom-vop sb-simd-neon:f32.4-dup
     (:args (src :scs (single-neon-reg)))
   (:info lane)
   (:results (dst :scs (single-neon-reg)))
   (:generator
    (inst dup dst src :4s lane)))

 (define-custom-vop sb-simd-neon:f32.4-lane-extract
     (:args (src :scs (single-neon-reg)))
   (:info lane)
   (:results (dst :scs (single-reg)))
   (:generator
    (unless (and (location= dst src)
                 (eql lane 0))
      (inst dup dst src nil lane))))

 (define-custom-vop sb-simd-neon:f32.4-lane-insert
     (:args (src1 :scs (single-neon-reg) :target dst)
            (src2 :scs (single-reg) :to :save))
   (:info lane)
   (:results (dst :scs (single-neon-reg)))
   (:generator
    (unless (location= dst src1)
      (inst mov dst src1 :16b))
    (inst ins dst lane
          (make-random-tn (sc-or-lose 'single-neon-reg) (tn-offset src2)) 0
          :s)))

 (define-custom-vop sb-simd-neon:f32.4-ins
     (:args (src1 :scs (single-neon-reg) :target dst)
            (src2 :scs (single-neon-reg) :to :save))
   (:info idx1 idx2)
   (:results (dst :scs (single-neon-reg)))
   (:generator
    (unless (location= dst src1)
      (inst mov dst src1 :16b))
    (inst ins dst idx1 src2 idx2 :s)))

 (define-custom-vop sb-simd-neon::f64!-from-p128
     (:args (src :scs (double-neon-reg) :to :save))
   (:results (dst :scs (double-reg)))
   (:generator
    (inst movi dst 0 :2d)
    (inst dup dst src nil 0)))

 (define-custom-vop sb-simd-neon::f64.2!-from-f64
     (:args (src :scs (double-reg) :target dst))
   (:results (dst :scs (double-neon-reg)))
   (:generator
    (unless (location= src dst)
      (inst ins dst 0 src 0 :d))))

 (define-custom-vop sb-simd-neon:f64.2-dup
     (:args (src :scs (double-neon-reg)))
   (:info lane)
   (:results (dst :scs (double-neon-reg)))
   (:generator
    (inst dup dst src :2d lane)))

 (define-custom-vop sb-simd-neon:f64.2-lane-extract
     (:args (src :scs (double-neon-reg)))
   (:info lane)
   (:results (dst :scs (double-reg)))
   (:generator
    (inst dup dst src nil lane)))

 (define-custom-vop sb-simd-neon:f64.2-lane-insert
     (:args (src1 :scs (double-neon-reg) :target dst)
            (src2 :scs (double-reg) :to :save))
   (:info lane)
   (:results (dst :scs (double-neon-reg)))
   (:generator
    (unless (location= dst src1)
      (inst mov dst src1 :16b))
    (inst ins dst lane
          (make-random-tn (sc-or-lose 'single-neon-reg) (tn-offset src2)) 0
          :d)))

 (define-custom-vop sb-simd-neon:f64.2-ins
     (:args (src1 :scs (double-neon-reg) :target dst)
            (src2 :scs (double-neon-reg) :to :save))
   (:info idx1 idx2)
   (:results (dst :scs (double-neon-reg)))
   (:generator
    (unless (location= dst src1)
      (inst mov dst src1 :16b))
    (inst ins dst idx1 src2 idx2 :d)))

 (define-custom-vop sb-simd-neon:u8.16-shuffle
     (:args (src1 :scs (single-neon-reg))
            (control :scs (single-neon-reg)))
   (:results (dst :scs (single-neon-reg)))
   (:generator
    (inst tbl dst (list src1) control :16b)))

 (macrolet
     ((def (sign width count)
        (multiple-value-bind (arrangement partial-arrangement smaller-arrangement smaller-partial-arrangement larger-arrangement element)
            (ecase width
              (8 (values :16b :8b nil nil :8h :b))
              (16 (values :8h :4h :16b :8b :4s :h))
              (32 (values :4s :2s :8h :4h :2d :s))
              (64 (values :2d nil :4s :2s nil :d)))
          (let ((smaller-width (/ width 2))
                (smaller-count (* count 2))
                (scalar-reg (if (eql sign :s) 'signed-reg 'unsigned-reg)))
            (flet ((name (format-control &rest format-arguments)
                     (intern (apply #'format nil format-control format-arguments)
                             (find-package :sb-simd-neon))))
              `(progn
                 (define-custom-vop ,(name "~a~d.~d!-FROM-~a~a" sign width count sign width)
                     (:args (src :scs (,scalar-reg) :target dst))
                   (:results (dst :scs (int-neon-reg)))
                   (:generator
                    (inst fmov (make-random-tn (sc-or-lose ',(if (eql width 64) 'double-reg 'single-reg)) (tn-offset dst)) src)))
                 (define-custom-vop ,(name "~a~d.~d-DUP" sign width count)
                     (:args (src :scs (int-neon-reg)))
                   (:info lane)
                   (:results (dst :scs (int-neon-reg)))
                   (:generator
                    (inst dup dst src ,arrangement lane)))
                 (define-custom-vop ,(name "~a~d.~d-LANE-EXTRACT" sign width count)
                     (:args (src :scs (int-neon-reg)))
                   (:info lane)
                   (:results (dst :scs (,scalar-reg)))
                   (:generator
                    (inst ,(if (and (eql sign :s) (not (eql width 64)))
                               'smov
                               'umov)
                          dst src lane ,element)))
                 (define-custom-vop ,(name "~a~d.~d-LANE-INSERT" sign width count)
                     (:args (src1 :scs (int-neon-reg) :target dst)
                            (src2 :scs (,scalar-reg) :to :save))
                   (:info lane)
                   (:results (dst :scs (int-neon-reg)))
                   (:generator
                    (unless (location= dst src1)
                      (inst mov dst src1 :16b))
                    (inst ins dst lane src2 nil ,element)))
                 (define-custom-vop ,(name "~a~d.~d-INS" sign width count)
                     (:args (src1 :scs (int-neon-reg) :target dst)
                            (src2 :scs (int-neon-reg) :to :save))
                   (:info idx1 idx2)
                   (:results (dst :scs (int-neon-reg)))
                   (:generator
                    (unless (location= dst src1)
                      (inst mov dst src1 :16b))
                    (inst ins dst idx1 src2 idx2 ,element)))
                 ;; Conversion from the smaller type.
                 ,@(when smaller-arrangement
                     `((define-custom-vop ,(name "~a~d.~d-FROM-~a~d.~d" sign width count sign smaller-width smaller-count)
                           (:args (src :scs (int-neon-reg)))
                         (:results (dst :scs (int-neon-reg)))
                         (:generator
                          (inst ,(if (eql sign :s) 'sshll 'ushll) dst ,arrangement src ,smaller-partial-arrangement)))
                       (define-custom-vop ,(name "~a~d.~d-FROM-~a~d.~d-HI" sign width count sign smaller-width smaller-count)
                           (:args (src :scs (int-neon-reg)))
                         (:results (dst :scs (int-neon-reg)))
                         (:generator
                          (inst ,(if (eql sign :s) 'sshll2 'ushll2) dst ,arrangement src ,smaller-arrangement)))))
                 (define-custom-vop ,(name "~a~d.~d-SHIFTR" sign width count)
                     (:args (src :scs (int-neon-reg)))
                   (:info shift)
                   (:results (dst :scs (int-neon-reg)))
                   (:generator
                    (inst ,(if (eql sign :s) 'sshr 'ushr) dst src shift ,arrangement)))
                 (define-custom-vop ,(name "~a~d.~d-SHIFTL" sign width count)
                     (:args (src :scs (int-neon-reg)))
                   (:info shift)
                   (:results (dst :scs (int-neon-reg)))
                   (:generator
                    (inst shl dst src shift ,arrangement)))
                 ,@(when larger-arrangement
                     `((define-custom-vop ,(name "~a~d.~d-SHIFTL-LONG" sign width count)
                           (:args (src :scs (int-neon-reg)))
                         (:info shift)
                         (:results (dst :scs (int-neon-reg)))
                         (:generator
                          (inst ,(if (eql sign :s) 'sshll 'ushll) dst ,larger-arrangement src ,partial-arrangement shift)))
                       (define-custom-vop ,(name "~a~d.~d-SHIFTL-LONG-HI" sign width count)
                           (:args (src :scs (int-neon-reg)))
                         (:info shift)
                         (:results (dst :scs (int-neon-reg)))
                         (:generator
                          (inst ,(if (eql sign :s) 'sshll2 'ushll2) dst ,larger-arrangement src ,arrangement shift)))))))))))
   (def :u 8 16)
   (def :u 16 8)
   (def :u 32 4)
   (def :u 64 2)
   (def :s 8 16)
   (def :s 16 8)
   (def :s 32 4)
   (def :s 64 2)))
)
