(in-package #:sb-vm)

(macrolet
    ((define-custom-vop (name &body clauses)
       (with-accessors ((name sb-simd-internals:instruction-record-name)
                        (vop sb-simd-internals:instruction-record-vop)
                        (argument-records sb-simd-internals:instruction-record-argument-records)
                        (result-records sb-simd-internals:instruction-record-result-records)
                        (cost sb-simd-internals:instruction-record-cost)
                        (encoding sb-simd-internals:instruction-record-encoding))
           (sb-simd-internals:find-function-record name)
         (assert (eq encoding :custom))
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
              (:generator ,cost ,@(find-clause :generator)))))))
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
     (inst movsd dst src))))
