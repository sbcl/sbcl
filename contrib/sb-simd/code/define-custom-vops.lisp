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

;; Neon

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
                   (inst umov dst src lane ,element)))
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
  (def :s 64 2))
)
