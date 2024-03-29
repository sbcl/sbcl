(in-package "SB-ROTATE-BYTE")

;;; 32-bit

(define-vop (%32bit-rotate-byte/c)
  (:policy :fast-safe)
  (:translate %unsigned-32-rotate-byte)
  (:note "inline 32-bit constant rotation")
  (:args (integer :scs (sb-vm::unsigned-reg) :target result))
  (:info count)
  (:arg-types (:constant (integer -31 31)) sb-vm::unsigned-num)
  (:results (result :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 5
    (aver (not (= count 0)))
    (inst ror (sb-vm::32-bit-reg result) (sb-vm::32-bit-reg integer)
          (if (plusp count)
              (- 32 count)
              (abs count)))))

(define-vop (%32bit-rotate-byte)
  (:policy :fast-safe)
  (:translate %unsigned-32-rotate-byte)
  (:note "inline 32-bit rotation")
  (:args (count :scs (sb-vm::signed-reg))
         (integer :scs (sb-vm::unsigned-reg)))
  (:arg-types sb-vm::tagged-num sb-vm::unsigned-num)
  (:results (res :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 10
    (inst neg sb-vm::tmp-tn count)
    (inst ror (sb-vm::32-bit-reg res)
          (sb-vm::32-bit-reg integer) (sb-vm::32-bit-reg sb-vm::tmp-tn))))

;;; 64-bit
(define-vop (%64bit-rotate-byte/c)
  (:policy :fast-safe)
  (:translate %unsigned-64-rotate-byte)
  (:note "inline 64-bit constant rotation")
  (:args (integer :scs (sb-vm::unsigned-reg) :target result))
  (:info count)
  (:arg-types (:constant (integer -63 63)) sb-vm::unsigned-num)
  (:results (result :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 5
    (aver (not (= count 0)))
    (inst ror result integer (if (plusp count)
                                 (- 64 count)
                                 (abs count)))))

(define-vop (%64bit-rotate-byte)
  (:policy :fast-safe)
  (:translate %unsigned-64-rotate-byte)
  (:note "inline 64-bit rotation")
  (:args (count :scs (sb-vm::signed-reg))
         (integer :scs (sb-vm::unsigned-reg)))
  (:arg-types sb-vm::tagged-num sb-vm::unsigned-num)
  (:results (res :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 10
    (inst neg sb-vm::tmp-tn count)
    (inst ror res integer sb-vm::tmp-tn)))
