;;;; x86-64 definition of character operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Space optimization: As the upper 32 bits of (tagged or untagged)
;;; characters are always zero many operations can be done on 32-bit
;;; registers. This often leads to smaller encodings as the REX prefix
;;; is then only needed if registers R8 - R15 are used.

;;;; moves and coercions

;;; Move a tagged char to an untagged representation.
(define-vop (move-to-character)
  (:args (x :scs (any-reg descriptor-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (character-reg)
               :load-if (not (location= x y))))
  (:note "character untagging")
  (:generator 1
    (cond ((and (sc-is y character-reg) (sc-is x any-reg descriptor-reg))
           (unless (location= x y)
             (inst mov :dword y x)))
          (t
           (move y x)))
    (inst shr :dword y n-widetag-bits)))
(define-move-vop move-to-character :move
  (any-reg)
  (character-reg))

;;; Move an untagged char to a tagged representation.
(define-vop (move-from-character)
  (:args (x :scs (character-reg) :target y))
  (:results (y :scs (any-reg descriptor-reg)))
  (:note "character tagging")
  (:generator 1
    (unless (location= x y)
      (inst mov :dword y x))
    (inst shl :dword y n-widetag-bits)
    (inst or :dword y character-widetag)))
(define-move-vop move-from-character :move
  (character-reg)
  (any-reg descriptor-reg))

;;; Move untagged character values.
(define-vop (character-move)
  (:args (x :target y
            :scs (character-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (character-reg character-stack)
               :load-if (not (location= x y))))
  (:note "character move")
  (:generator 0
    (move y x)))
(define-move-vop character-move :move
  (character-reg) (character-reg character-stack))

;;; Move untagged character arguments/return-values.
(define-vop (move-character-arg)
  (:args (x :target y
            :scs (character-reg))
         (fp :scs (any-reg)
             :load-if (not (sc-is y character-reg))))
  (:results (y))
  (:note "character arg move")
  (:generator 0
    (sc-case y
      (character-reg
       (move y x))
      (character-stack
       (if (= (tn-offset fp) esp-offset)
           (storew x fp (tn-offset y))  ; c-call
           (storew x fp (frame-word-offset (tn-offset y))))))))
(define-move-vop move-character-arg :move-arg
  (any-reg character-reg) (character-reg))

;;; Use standard MOVE-ARG + coercion to move an untagged character
;;; to a descriptor passing location.
(define-move-vop move-arg :move-arg
  (character-reg) (any-reg descriptor-reg))

;;;; other operations

(define-vop (char-code)
  (:translate char-code)
  (:policy :fast-safe)
  (:args (ch :scs (character-reg character-stack) :target res))
  (:arg-types character)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (move res ch))) ; FIXME: shouldn't this be :dword ?

(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (unsigned-reg unsigned-stack) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (character-reg)))
  (:result-types character)
  (:generator 1
    ;; While we could use a byte-sized move here for non-unicode builds,
    ;; I think it's better to move 32 bits from source to destination either way.
    ;; The non-unicode case used to perform two moves - first CODE into EAX and then
    ;; AL to result. I think it was blindly copied from the 32-bit vm definition.
    ;; The 32-bit vm could use both the low and high byte subregisters of a word-sized
    ;; register, thus you had to carefully avoid affecting other bits of the physical
    ;; destination register. And since not all source registers had an accessible 8-bit
    ;; subregister, it forced going through one of EAX,EBX,ECX,EDX.
    ;; Those considerations do not pertain to the 64-bit vm.
    (unless (location= code res)
      (inst mov :dword res code))))

;;; comparison of CHARACTERs
(define-vop (character-compare)
  (:args (x :scs (character-reg character-stack))
         (y :scs (character-reg)
            :load-if (not (and (sc-is x character-reg)
                               (sc-is y character-stack)))))
  (:arg-types character character)
  (:info)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:generator 3
    (inst cmp x y)))

(define-vop (fast-char=/character character-compare)
  (:translate char=)
  (:conditional :e))

(define-vop (fast-char</character character-compare)
  (:translate char<)
  (:conditional :b))

(define-vop (fast-char>/character character-compare)
  (:translate char>)
  (:conditional :a))

(define-vop (character-compare/c)
  (:args (x :scs (character-reg character-stack)))
  (:arg-types character (:constant character))
  (:info y)
  (:policy :fast-safe)
  (:note "inline constant comparison")
  (:generator 2
    (inst cmp x (sb-xc:char-code y))))

(define-vop (fast-char=/character/c character-compare/c)
  (:translate char=)
  (:conditional :e))

(define-vop (fast-char</character/c character-compare/c)
  (:translate char<)
  (:conditional :b))

(define-vop (fast-char>/character/c character-compare/c)
  (:translate char>)
  (:conditional :a))

#+sb-unicode
(define-vop (base-char-p)
  (:args (value :scs (any-reg descriptor-reg)))
  (:arg-types *)
  (:translate base-char-p)
  (:temporary (:sc unsigned-reg :from (:argument 0)) temp)
  (:conditional :z)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 4
    (inst lea :dword temp (ea (- character-widetag) value))
    (inst test :dword temp (lognot #x7F00))))

#+sb-unicode
(define-vop (base-char-p-character)
  (:args (value :scs (any-reg)))
  (:arg-types character)
  (:translate base-char-p)
  (:conditional :z)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 3
    (inst test :dword value (lognot #x7FFF))))

#+sb-unicode
(define-vop (base-char-p-character-reg)
  (:args (value :scs (character-reg)))
  (:arg-types character)
  (:translate base-char-p)
  (:conditional :l)
  (:save-p :compute-only)
  (:policy :fast-safe)
  (:generator 2
    (inst cmp :dword value base-char-code-limit)))
