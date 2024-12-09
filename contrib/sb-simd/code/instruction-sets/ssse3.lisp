(in-package #:sb-simd-ssse3)

(define-instruction-set :ssse3
  (:include :sse3)
  (:test (ssse3-supported-p))
  (:instructions
   ;; u16.8
   (u16.8-hadd    #:phaddw    (u16.8) (u16.8 u16.8) :cost 3 :encoding :sse)
   (u16.8-hsub    #:phsubw    (u16.8) (u16.8 u16.8) :cost 3 :encoding :sse)
   ;; u32.4
   (u32.4-hadd    #:phaddd    (u32.4) (u32.4 u32.4) :cost 3 :encoding :sse)
   (u32.4-hsub    #:phsubd    (u32.4) (u32.4 u32.4) :cost 3 :encoding :sse)
   ;; s8.16
   (s8.16-shuffle #:pshufb    (s8.16) (s8.16 s8.16) :cost 1 :encoding :sse)
   (s8.16-abs     #:pabsb     (s8.16) (s8.16)       :cost 2)
   (s8.16-sign    #:psignb    (s8.16) (s8.16 s8.16) :cost 1 :encoding :sse)
   ;; s16.8
   (two-arg-s16.8-mulhrs #:pmulhrsw  (s16.8) (s16.8 s16.8) :cost 1 :encoding :sse :associative t)
   (s16.8-abs     #:pabsw     (s16.8) (s16.8)       :cost 2)
   (s16.8-maddubs #:pmaddubsw (s16.8) (u8.16 s8.16) :cost 2 :encoding :sse)
   (s16.8-sign    #:psignw    (s16.8) (s16.8 s16.8) :cost 2 :encoding :sse)
   ;; s16.8
   (s16.8-hadd    #:phaddw    (s16.8) (s16.8 s16.8) :cost 3 :encoding :sse)
   (s16.8-hsub    #:phsubw    (s16.8) (s16.8 s16.8) :cost 3 :encoding :sse)
   ;; s32.4
   (s32.4-abs     #:pabsd     (s32.4) (s32.4)       :cost 2)
   (s32.4-sign    #:psignd    (s32.4) (s32.4 s32.4) :cost 3 :encoding :sse)
   (s32.4-hadd    #:phaddd    (s32.4) (s32.4 s32.4) :cost 3 :encoding :sse)
   (s32.4-hsub    #:phsubd    (s32.4) (s32.4 s32.4) :cost 3 :encoding :sse))
  (:associatives
   (s16.8-mulhrs two-arg-s16.8-mulhrs 1)))
