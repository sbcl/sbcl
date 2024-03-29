(in-package "SB-ROTATE-BYTE")


;;; 32-bit rotates

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
    (move result integer)
    (if (> count 0)
        (inst rol :dword result count)
        (inst ror :dword result (- count)))))

(define-vop (%32bit-rotate-byte)
  (:policy :fast-safe)
  (:translate %unsigned-32-rotate-byte)
  (:args (count :scs (sb-vm::signed-reg) :target rcx)
         (integer :scs (sb-vm::unsigned-reg) :target result))
  (:arg-types sb-vm::tagged-num sb-vm::unsigned-num)
  (:temporary (:sc sb-vm::signed-reg :offset sb-vm::rcx-offset)
              rcx)
  (:results (result :scs (sb-vm::unsigned-reg) :from :load))
  (:result-types sb-vm::unsigned-num)
  (:generator 10
    (move result integer)
    (move rcx count :dword)
    (inst rol :dword result :cl)))

;;; 64-bit rotates

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
    (move result integer)
    (if (> count 0)
        (inst rol result count)
        (inst ror result (- count)))))

(define-vop (%64bit-rotate-byte)
  (:policy :fast-safe)
  (:translate %unsigned-64-rotate-byte)
  (:args (count :scs (sb-vm::signed-reg) :target rcx)
         (integer :scs (sb-vm::unsigned-reg) :target result))
  (:arg-types sb-vm::tagged-num sb-vm::unsigned-num)
  (:temporary (:sc sb-vm::signed-reg :offset sb-vm::rcx-offset)
              rcx)
  (:results (result :scs (sb-vm::unsigned-reg) :from :load))
  (:result-types sb-vm::unsigned-num)
  (:generator 10
    (move result integer)
    (move rcx count :dword)
    (inst rol result :cl)))
