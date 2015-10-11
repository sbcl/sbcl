(in-package "SB-ROTATE-BYTE")

(define-vop (%32bit-rotate-byte/c)
  (:policy :fast-safe)
  (:translate %unsigned-32-rotate-byte)
  (:note "inline 32-bit constant rotation")
  (:info count)
  (:args (integer :scs (sb-vm::unsigned-reg) :target res))
  (:arg-types (:constant (integer -31 31)) sb-vm::unsigned-byte-32)
  (:results (res :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-byte-32)
  (:generator 5
    ;; the 0 case is an identity operation and should be
    ;; DEFTRANSFORMed away.
    (aver (not (= count 0)))
    (inst mov res (sb-vm::ror integer (if (plusp count)
                                          (- 32 count)
                                          count)))))

(define-vop (%32bit-rotate-byte-fixnum/c)
  (:policy :fast-safe)
  (:translate %unsigned-32-rotate-byte)
  (:note "inline 32-bit constant rotation")
  (:info count)
  (:args (integer :scs (sb-vm::any-reg) :target res))
  (:arg-types (:constant (integer -31 31)) sb-vm::positive-fixnum)
  (:results (res :scs (sb-vm::unsigned-reg)))
  (:result-types sb-vm::unsigned-byte-32)
  (:generator 5
    (aver (not (= count 0)))
    (cond
      ((= count n-fixnum-tag-bits))
      (t
       (inst mov res (sb-vm::ror integer (if (> count 2)
                                             (- (+ 32 n-fixnum-tag-bits) count)
                                             (- 2 count))))))))

(macrolet ((def (name arg-type)
             `(define-vop (,name)
               (:policy :fast-safe)
               (:translate %unsigned-32-rotate-byte)
               (:note "inline 32-bit rotation")
               (:args (count :scs (sb-vm::signed-reg) :target res)
                      (integer :scs (sb-vm::unsigned-reg)))
               (:arg-types sb-vm::tagged-num ,arg-type)
               (:results (res :scs (sb-vm::unsigned-reg)))
               (:result-types sb-vm::unsigned-byte-32)
               (:generator 10
                (inst cmp count 0)
                (inst rsb :gt res count 32)
                (inst rsb :le res count 0)
                (inst mov res (sb-vm::ror integer res))))))
  (def %32bit-rotate-byte sb-vm::unsigned-byte-32)
  ;; FIXME: see x86-vm.lisp
  (def %32bit-rotate-byte-fixnum sb-vm::positive-fixnum))
