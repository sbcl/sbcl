;;;; x86 definition of character operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; moves and coercions

;;; Move a tagged char to an untagged representation.
#+sb-unicode
(define-vop (move-to-character)
  (:args (x :scs (any-reg descriptor-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (character-reg)
               :load-if (not (location= x y))))
  (:note "character untagging")
  (:generator 1
    (move y x)
    (inst shr y n-widetag-bits)))
#-sb-unicode
(define-vop (move-to-character)
  (:args (x :scs (any-reg control-stack) :target al))
  (:temporary (:sc byte-reg :offset al-offset
                   :from (:argument 0) :to (:eval 0)) al)
  (:ignore al)
  (:temporary (:sc byte-reg :offset ah-offset :target y
                   :from (:argument 0) :to (:result 0)) ah)
  (:results (y :scs (character-reg character-stack)))
  (:note "character untagging")
  (:generator 1
    (move eax-tn x)
    (move y ah)))
(define-move-vop move-to-character :move
  (any-reg #-sb-unicode control-stack)
  (character-reg #-sb-unicode character-stack))

;;; Move an untagged char to a tagged representation.
#+sb-unicode
(define-vop (move-from-character)
  (:args (x :scs (character-reg) :target y))
  (:results (y :scs (any-reg descriptor-reg)))
  (:note "character tagging")
  (:generator 1
    (move y x)
    (inst shl y n-widetag-bits)
    (inst or y character-widetag)))
#-sb-unicode
(define-vop (move-from-character)
  (:args (x :scs (character-reg character-stack) :target ah))
  (:temporary (:sc byte-reg :offset al-offset :target y
                   :from (:argument 0) :to (:result 0)) al)
  (:temporary (:sc byte-reg :offset ah-offset
                   :from (:argument 0) :to (:result 0)) ah)
  (:results (y :scs (any-reg descriptor-reg control-stack)))
  (:note "character tagging")
  (:generator 1
    (move ah x)                         ; Maybe move char byte.
    (inst mov al character-widetag)     ; x86 to type bits
    (inst and eax-tn #xffff)            ; Remove any junk bits.
    (move y eax-tn)))
(define-move-vop move-from-character :move
  (character-reg #-sb-unicode character-stack)
  (any-reg descriptor-reg #-sb-unicode control-stack))

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
       #-sb-unicode
       (inst mov
             ;; XXX: If the sb-unicode case needs to handle c-call,
             ;; why does the non-unicode case not need to?
             (make-ea :byte :base fp :disp (frame-byte-offset (tn-offset y)))
             x)
       #+sb-unicode
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
  (:args #-sb-unicode (ch :scs (character-reg character-stack))
         #+sb-unicode (ch :scs (character-reg character-stack) :target res))
  (:arg-types character)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    #-sb-unicode
    (inst movzx res ch)
    #+sb-unicode
    (move res ch)))

#+sb-unicode
(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (unsigned-reg unsigned-stack) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (character-reg)))
  (:result-types character)
  (:generator 1
    (move res code)))
#-sb-unicode
(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (unsigned-reg unsigned-stack) :target eax))
  (:arg-types positive-fixnum)
  (:temporary (:sc unsigned-reg :offset eax-offset :target res
                   :from (:argument 0) :to (:result 0))
              eax)
  (:results (res :scs (character-reg)))
  (:result-types character)
  (:generator 1
    (move eax code)
    (move res al-tn)))

;;; comparison of CHARACTERs
(define-vop (character-compare)
  (:args (x :scs (character-reg character-stack))
         (y :scs (character-reg)
            :load-if (not (and (sc-is x character-reg)
                               (sc-is y character-stack)))))
  (:arg-types character character)
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
    (inst cmp x (char-code y))))

(define-vop (fast-char=/character/c character-compare/c)
  (:translate char=)
  (:conditional :e))

(define-vop (fast-char</character/c character-compare/c)
  (:translate char<)
  (:conditional :b))

(define-vop (fast-char>/character/c character-compare/c)
  (:translate char>)
  (:conditional :a))
