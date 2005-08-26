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
    (move res integer)
    (if (> count 0)
        (inst rol res count)
        (inst ror res (- count)))))

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
    (inst mov res integer)
    (cond
      ;; FIXME: all these 2s should be n-fixnum-tag-bits.
      ((= count 2))
      ((> count 2) (inst rol res (- count 2)))
      (t (inst ror res (- 2 count))))))

(macrolet ((def (name arg-type)
             `(define-vop (,name)
               (:policy :fast-safe)
               (:translate %unsigned-32-rotate-byte)
               (:note "inline 32-bit rotation")
               (:args (count :scs (sb-vm::signed-reg) :target ecx)
                      (integer :scs (sb-vm::unsigned-reg) :target res))
               (:arg-types sb-vm::tagged-num ,arg-type)
               (:temporary (:sc sb-vm::signed-reg :offset sb-vm::ecx-offset)
                           ecx)
               (:results (res :scs (sb-vm::unsigned-reg) :from :load))
               (:result-types sb-vm::unsigned-byte-32)
               (:generator 10
                (let ((label (gen-label))
                      (end (gen-label)))
                  (move res integer)
                  (move ecx count)
                  (inst cmp ecx 0)
                  (inst jmp :ge label)
                  (inst neg ecx)
                  (inst ror res :cl)
                  (inst jmp end)
                  (emit-label label)
                  (inst rol res :cl)
                  (emit-label end))))))
  (def %32bit-rotate-byte sb-vm::unsigned-byte-32)
  ;; FIXME: it's not entirely clear to me why we need this second
  ;; definition -- or rather, why the compiler isn't smart enough to
  ;; MOVE a POSITIVE-FIXNUM argument to an UNSIGNED-BYTE-32 argument,
  ;; and then go from there.  Still, not having it leads to scary
  ;; compilation messages of the form:
  ;;
  ;;    unable to do inline 32-bit constant rotation (cost 5) because:
  ;;    This shouldn't happen!  Bug?
  ;;    argument types invalid
  ;;    argument primitive types:
  ;;  (SB-VM::POSITIVE-FIXNUM SB-VM::POSITIVE-FIXNUM)
  ;;
  ;; so better leave it in.
  (def %32bit-rotate-byte-fixnum sb-vm::positive-fixnum))
