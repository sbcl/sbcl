(in-package #:sb-simd-avx512fp16)

(define-instruction-set :avx512fp16
  (:test (avx512fp16-supported-p))
  (:include :avx512dq)
  (:scalars
   (f16 16 (unsigned-byte 16) #:unsigned-num (#:unsigned-reg)))
  (:simd-packs
   (f16.32 f16 512 #:simd-pack-512-ub16 (#:int-avx512-reg))
   (f16.16 f16 256 #:simd-pack-256-ub16 (#:int-avx2-reg))
   (f16.8  f16 128 #:simd-pack-ub16     (#:int-sse-reg)))
  (:simd-casts
   (f16.32 f16.32-broadcast)
   (f16.16 f16.16-broadcast)
   (f16.8  f16.8-broadcast))
  (:reinterpret-casts
   (f16.32! f16.32!-from-p128 f16.32!-from-p256 f16.32!-from-p512)
   (f16.16! f16.16!-from-p128 f16.16!-from-p256 f16.16!-from-p512)
   (f16.8!  f16.8!-from-p128  f16.8!-from-p256  f16.8!-from-p512))
  (:instructions
   ;; Reinterprets
   (f16.32!-from-p128   #:vmovdqu64    (f16.32) (p128)          :cost 1 :encoding :move :always-translatable nil)
   (f16.32!-from-p256   #:vmovdqu64    (f16.32) (p256)          :cost 1 :encoding :move :always-translatable nil)
   (f16.32!-from-p512   #:vmovdqu64    (f16.32) (p512)          :cost 1 :encoding :move :always-translatable nil)

   (f16.16!-from-p128   #:vmovdqu      (f16.16) (p128)          :cost 1 :encoding :move :always-translatable nil)
   (f16.16!-from-p256   #:vmovdqu      (f16.16) (p256)          :cost 1 :encoding :move :always-translatable nil)
   (f16.16!-from-p512   #:vextracti32x8 (f16.16) (p512)         :cost 1 :suffix '(0) :always-translatable nil)

   (f16.8!-from-p128    #:movdqu       (f16.8)  (p128)          :cost 1 :encoding :move :always-translatable nil)
   (f16.8!-from-p256    #:vextracti128 (f16.8)  (p256)          :cost 1 :suffix '(0) :always-translatable nil)
   (f16.8!-from-p512    #:vextracti32x4 (f16.8) (p512)          :cost 1 :suffix '(0) :always-translatable nil)

   ;; f16.32
   (f16.32-broadcast    nil            (f16.32) (f16)           :cost 1 :encoding :fake-vop)
   (two-arg-f16.32+     #:vaddph       (f16.32) (f16.32 f16.32) :cost 1 :associative t)
   (two-arg-f16.32-     #:vsubph       (f16.32) (f16.32 f16.32) :cost 1)
   (two-arg-f16.32*     #:vmulph       (f16.32) (f16.32 f16.32) :cost 1 :associative t)
   (two-arg-f16.32/     #:vdivph       (f16.32) (f16.32 f16.32) :cost 1)
   (f16.32-sqrt         #:vsqrtph      (f16.32) (f16.32)        :cost 1)
   (two-arg-f16.32-min  #:vminph       (f16.32) (f16.32 f16.32) :cost 1 :associative t)
   (two-arg-f16.32-max  #:vmaxph       (f16.32) (f16.32 f16.32) :cost 1 :associative t)
   (f16.32-rcp          #:vrcpph       (f16.32) (f16.32)        :cost 1)
   (f16.32-rsqrt        #:vrsqrtph     (f16.32) (f16.32)        :cost 1)
   (f16.32-scalef       #:vscalefph    (f16.32) (f16.32 f16.32) :cost 1)

   (f16.32-fmadd        #:vfmadd213ph  (f16.32) (f16.32 f16.32 f16.32) :cost 1 :encoding :fma)
   (f16.32-fmsub        #:vfmsub213ph  (f16.32) (f16.32 f16.32 f16.32) :cost 1 :encoding :fma)
   (f16.32-fnmadd       #:vfnmadd213ph (f16.32) (f16.32 f16.32 f16.32) :cost 1 :encoding :fma)
   (f16.32-fnmsub       #:vfnmsub213ph (f16.32) (f16.32 f16.32 f16.32) :cost 1 :encoding :fma)

   (two-arg-f16.32-and  #:vpandd       (f16.32) (f16.32 f16.32) :cost 1 :associative t)
   (two-arg-f16.32-or   #:vpord        (f16.32) (f16.32 f16.32) :cost 1 :associative t)
   (two-arg-f16.32-xor  #:vpxord       (f16.32) (f16.32 f16.32) :cost 1 :associative t)
   (f16.32-andc1        #:vpandnd      (f16.32) (f16.32 f16.32) :cost 1)
   (f16.32-not          nil            (f16.32) (f16.32)        :cost 1 :encoding :custom)

   ;; f16.16
   (f16.16-broadcast    nil            (f16.16) (f16)           :cost 1 :encoding :fake-vop)
   (two-arg-f16.16+     #:vaddph       (f16.16) (f16.16 f16.16) :cost 1 :associative t)
   (two-arg-f16.16-     #:vsubph       (f16.16) (f16.16 f16.16) :cost 1)
   (two-arg-f16.16*     #:vmulph       (f16.16) (f16.16 f16.16) :cost 1 :associative t)
   (two-arg-f16.16/     #:vdivph       (f16.16) (f16.16 f16.16) :cost 1)
   (f16.16-sqrt         #:vsqrtph      (f16.16) (f16.16)        :cost 1)
   (two-arg-f16.16-min  #:vminph       (f16.16) (f16.16 f16.16) :cost 1 :associative t)
   (two-arg-f16.16-max  #:vmaxph       (f16.16) (f16.16 f16.16) :cost 1 :associative t)
   (f16.16-rcp          #:vrcpph       (f16.16) (f16.16)        :cost 1)
   (f16.16-rsqrt        #:vrsqrtph     (f16.16) (f16.16)        :cost 1)

   (f16.16-fmadd        #:vfmadd213ph  (f16.16) (f16.16 f16.16 f16.16) :cost 1 :encoding :fma)
   (f16.16-fmsub        #:vfmsub213ph  (f16.16) (f16.16 f16.16 f16.16) :cost 1 :encoding :fma)
   (f16.16-fnmadd       #:vfnmadd213ph (f16.16) (f16.16 f16.16 f16.16) :cost 1 :encoding :fma)
   (f16.16-fnmsub       #:vfnmsub213ph (f16.16) (f16.16 f16.16 f16.16) :cost 1 :encoding :fma)

   ;; f16.8
   (f16.8-broadcast     nil            (f16.8)  (f16)           :cost 1 :encoding :fake-vop)
   (two-arg-f16.8+      #:vaddph       (f16.8)  (f16.8 f16.8)   :cost 1 :associative t)
   (two-arg-f16.8-      #:vsubph       (f16.8)  (f16.8 f16.8)   :cost 1)
   (two-arg-f16.8*      #:vmulph       (f16.8)  (f16.8 f16.8)   :cost 1 :associative t)
   (two-arg-f16.8/      #:vdivph       (f16.8)  (f16.8 f16.8)   :cost 1)
   (f16.8-sqrt          #:vsqrtph      (f16.8)  (f16.8)         :cost 1)
   (two-arg-f16.8-min   #:vminph       (f16.8)  (f16.8 f16.8)   :cost 1 :associative t)
   (two-arg-f16.8-max   #:vmaxph       (f16.8)  (f16.8 f16.8)   :cost 1 :associative t)
   (f16.8-rcp           #:vrcpph       (f16.8)  (f16.8)         :cost 1)
   (f16.8-rsqrt         #:vrsqrtph     (f16.8)  (f16.8)         :cost 1)

   (f16.8-fmadd         #:vfmadd213ph  (f16.8)  (f16.8 f16.8 f16.8) :cost 1 :encoding :fma)
   (f16.8-fmsub         #:vfmsub213ph  (f16.8)  (f16.8 f16.8 f16.8) :cost 1 :encoding :fma)
   (f16.8-fnmadd        #:vfnmadd213ph (f16.8)  (f16.8 f16.8 f16.8) :cost 1 :encoding :fma)
   (f16.8-fnmsub        #:vfnmsub213ph (f16.8)  (f16.8 f16.8 f16.8) :cost 1 :encoding :fma)

   ;; Conversions
   (f32.16-from-f16.16  #:vcvtph2psx   (f32.16) (f16.16)        :cost 3)
   (f16.16-from-f32.16  #:vcvtps2phx   (f16.16) (f32.16)        :cost 3)
   (f16.16-from-s32.16  #:vcvtdq2ph    (f16.16) (s32.16)        :cost 3)
   (s32.16-from-f16.16  #:vcvtph2dq    (s32.16) (f16.16)        :cost 3))
  (:associatives
   (f16.32+ two-arg-f16.32+ 0)
   (f16.32* two-arg-f16.32* 1)
   (f16.32-min two-arg-f16.32-min nil)
   (f16.32-max two-arg-f16.32-max nil)
   (f16.32-and two-arg-f16.32-and +u16-true+)
   (f16.32-or  two-arg-f16.32-or  +u16-false+)
   (f16.32-xor two-arg-f16.32-xor +u16-false+)

   (f16.16+ two-arg-f16.16+ 0)
   (f16.16* two-arg-f16.16* 1)
   (f16.16-min two-arg-f16.16-min nil)
   (f16.16-max two-arg-f16.16-max nil)

   (f16.8+ two-arg-f16.8+ 0)
   (f16.8* two-arg-f16.8* 1)
   (f16.8-min two-arg-f16.8-min nil)
   (f16.8-max two-arg-f16.8-max nil))
  (:reducers
   (f16.32- two-arg-f16.32- 0)
   (f16.32/ two-arg-f16.32/ 1)
   (f16.16- two-arg-f16.16- 0)
   (f16.16/ two-arg-f16.16/ 1)
   (f16.8-  two-arg-f16.8-  0)
   (f16.8/  two-arg-f16.8/  1)))
