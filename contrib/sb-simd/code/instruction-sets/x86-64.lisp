(in-package #:sb-simd-x86-64)

(define-instruction-set :x86-64
  (:include :sb-simd)
  (:scalars
   (imm1 1 (unsigned-byte 1) (:constant (unsigned-byte 1)))
   (imm2 2 (unsigned-byte 2) (:constant (unsigned-byte 2)))
   (imm3 3 (unsigned-byte 3) (:constant (unsigned-byte 3)))
   (imm4 4 (unsigned-byte 4) (:constant (unsigned-byte 4)))
   (imm5 5 (unsigned-byte 5) (:constant (unsigned-byte 5)))
   (imm6 6 (unsigned-byte 6) (:constant (unsigned-byte 6)))
   (imm7 7 (unsigned-byte 7) (:constant (unsigned-byte 7)))
   (imm8 8 (unsigned-byte 8) (:constant (unsigned-byte 8)))))
