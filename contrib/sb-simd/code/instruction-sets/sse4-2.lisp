(in-package #:sb-simd-sse4.2)

(define-instruction-set :sse4.2
  (:include :sse4.1)
  (:test (sse4.2-supported-p))
  (:instructions
   ;; u64.2
   (two-arg-u64.2>~ #:pcmpgtq (u64.2) (u64.2 u64.2) :cost 3 :encoding :sse)
   (two-arg-u64.2>  nil       (u64.2) (u64.2 u64.2) :cost 3 :encoding :fake-vop)
   (two-arg-u64.2>= nil       (u64.2) (u64.2 u64.2) :cost 4 :encoding :fake-vop)
   (two-arg-u64.2<  nil       (u64.2) (u64.2 u64.2) :cost 3 :encoding :fake-vop)
   (two-arg-u64.2<= nil       (u64.2) (u64.2 u64.2) :cost 4 :encoding :fake-vop)
   ;; s64.2
   (two-arg-s64.2>  #:pcmpgtq (u64.2) (s64.2 s64.2) :cost 3 :encoding :sse)
   (two-arg-s64.2>= nil       (u64.2) (s64.2 s64.2) :cost 4 :encoding :fake-vop)
   (two-arg-s64.2<  nil       (u64.2) (s64.2 s64.2) :cost 3 :encoding :fake-vop)
   (two-arg-s64.2<= nil       (u64.2) (s64.2 s64.2) :cost 4 :encoding :fake-vop))
  (:comparisons
   (u64.2<  two-arg-u64.2<  u64.2-and +u64-true+)
   (u64.2<= two-arg-u64.2<= u64.2-and +u64-true+)
   (u64.2>  two-arg-u64.2>  u64.2-and +u64-true+)
   (u64.2>= two-arg-u64.2>= u64.2-and +u64-true+)
   (s64.2<  two-arg-s64.2<  u64.2-and +u64-true+)
   (s64.2<= two-arg-s64.2<= u64.2-and +u64-true+)
   (s64.2>  two-arg-s64.2>  u64.2-and +u64-true+)
   (s64.2>= two-arg-s64.2>= u64.2-and +u64-true+)))
