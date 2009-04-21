;;;; x86-64 definition of character operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; Space optimization: As the upper 32 bits of (tagged or untagged)
;;; characters are always zero many operations can be done on 32-bit
;;; registers. This often leads to smaller encodings as the REX prefix
;;; is then only needed if registers R8 - R15 are used.

;;;; moves and coercions

;;; Move a tagged char to an untagged representation.
#!+sb-unicode
(define-vop (move-to-character)
  (:args (x :scs (any-reg descriptor-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (character-reg)
               :load-if (not (location= x y))))
  (:note "character untagging")
  (:generator 1
    (cond ((and (sc-is y character-reg) (sc-is x any-reg descriptor-reg))
           (let ((y-dword (make-dword-tn y)))
             (unless (location= x y)
               (inst mov y-dword (make-dword-tn x)))
             (inst shr y-dword n-widetag-bits)))
          (t
           (move y x)
           (inst shr y n-widetag-bits)))))
#!-sb-unicode
(define-vop (move-to-character)
  (:args (x :scs (any-reg control-stack)))
  (:results (y :scs (character-reg #+nil character-stack)))
  (:note "character untagging")
  (:generator 1
    (let ((y-wide-tn (make-random-tn
                      :kind :normal
                      :sc (sc-or-lose 'any-reg)
                      :offset (tn-offset y))))
      (move y-wide-tn x)
      (inst shr y-wide-tn 8)
      (inst and y-wide-tn #xff))))
(define-move-vop move-to-character :move
  (any-reg #!-sb-unicode control-stack)
  (character-reg))

;;; Move an untagged char to a tagged representation.
#!+sb-unicode
(define-vop (move-from-character)
  (:args (x :scs (character-reg) :target y))
  (:results (y :scs (any-reg descriptor-reg)))
  (:note "character tagging")
  (:generator 1
    (let ((y-dword (make-dword-tn y)))
      (unless (location= x y)
        (inst mov y-dword (make-dword-tn x)))
      (inst shl y-dword n-widetag-bits)
      (inst or y-dword character-widetag))))
#!-sb-unicode
(define-vop (move-from-character)
  (:args (x :scs (character-reg character-stack)))
  (:results (y :scs (any-reg descriptor-reg #+nil control-stack)))
  (:note "character tagging")
  (:generator 1
    (move (make-random-tn :kind :normal :sc (sc-or-lose 'character-reg)
                          :offset (tn-offset y))
          x)
    (inst shl y n-widetag-bits)
    (inst or y character-widetag)
    (inst and y #xffff)))
(define-move-vop move-from-character :move
  (character-reg)
  (any-reg descriptor-reg #!-sb-unicode control-stack))

;;; Move untagged character values.
(define-vop (character-move)
  (:args (x :target y
            :scs (character-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (character-reg character-stack)
               :load-if (not (location= x y))))
  (:note "character move")
  (:effects)
  (:affected)
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
       #!-sb-unicode
       (inst mov
             ;; XXX: If the sb-unicode case needs to handle c-call,
             ;; why does the non-unicode case not need to?
             (make-ea :byte :base fp :disp (frame-byte-offset (tn-offset y)))
             x)
       #!+sb-unicode
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
  (:args #!-sb-unicode (ch :scs (character-reg character-stack))
         #!+sb-unicode (ch :scs (character-reg character-stack) :target res))
  (:arg-types character)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    #!-sb-unicode
    (inst movzx res ch)
    #!+sb-unicode
    (move res ch)))

#!+sb-unicode
(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (unsigned-reg unsigned-stack) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (character-reg)))
  (:result-types character)
  (:generator 1
    (move res code)))
#!-sb-unicode
(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (unsigned-reg unsigned-stack) :target eax))
  (:arg-types positive-fixnum)
  (:temporary (:sc unsigned-reg :offset rax-offset :target res
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
    (inst cmp x (sb!xc:char-code y))))

(define-vop (fast-char=/character/c character-compare/c)
  (:translate char=)
  (:conditional :e))

(define-vop (fast-char</character/c character-compare/c)
  (:translate char<)
  (:conditional :b))

(define-vop (fast-char>/character/c character-compare/c)
  (:translate char>)
  (:conditional :a))
