(in-package "SB-ROTATE-BYTE")

;;; 64-bit
(define-vop (%64bit-rotate-byte/c)
  (:policy :fast-safe)
  (:translate %unsigned-64-rotate-byte)
  (:note "inline 64-bit constant rotation")
  (:args (integer :scs (sb-vm::unsigned-reg) :target result))
  (:info count)
  (:arg-types (:constant (integer -63 63)) sb-vm::unsigned-num)
  (:temporary (:sc sb-vm::unsigned-reg) temp)
  (:results (result :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 5
    (aver (not (= count 0)))
    (let ((count (if (plusp count)
		     count
		     (+ 64 count))))
      (inst slli result integer count)
      (inst srli temp integer (- 64 count))
      (inst or result result temp))))


(define-vop (%64bit-rotate-byte)
  (:policy :fast-safe)
  (:translate %unsigned-64-rotate-byte)
  (:note "inline 64-bit rotation")
  (:args (count :scs (sb-vm::signed-reg))
         (integer :scs (sb-vm::unsigned-reg)))
  (:arg-types sb-vm::tagged-num sb-vm::unsigned-num)
  (:temporary (:sc sb-vm::unsigned-reg) temp)
  (:temporary (:sc sb-vm::signed-reg) count-temp)
  (:results (result :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-num)
  (:generator 10
    (inst slti count-temp count 0)
    (inst slli count-temp count-temp 6)
    (inst add count-temp count-temp count)
    (inst sll result integer count-temp)
    (inst addi temp sb-vm::zero-tn 64)
    (inst sub count-temp temp count-temp)
    (inst srl temp integer count-temp)
    (inst or result result temp)))

