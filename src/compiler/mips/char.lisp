;;;; the MIPS VM definition of character operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; Moves and coercions:

;;; Move a tagged char to an untagged representation.
(define-vop (move-to-character)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (character-reg)))
  (:note "character untagging")
  (:generator 1
    (inst srl y x n-widetag-bits)))

(define-move-vop move-to-character :move
  (any-reg descriptor-reg) (character-reg))

;;; Move an untagged char to a tagged representation.
(define-vop (move-from-character)
  (:args (x :scs (character-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:note "character tagging")
  (:generator 1
    (inst sll y x n-widetag-bits)
    (inst or y character-widetag)))

(define-move-vop move-from-character :move
  (character-reg) (any-reg descriptor-reg))

;;; Move untagged character values.
(define-vop (character-move)
  (:args (x :target y
            :scs (character-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (character-reg)
               :load-if (not (location= x y))))
  (:note "character move")
  (:generator 0
    (move y x)))

(define-move-vop character-move :move
  (character-reg) (character-reg))

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
       (storew x fp (tn-offset y))))))

(define-move-vop move-character-arg :move-arg
  (any-reg character-reg) (character-reg))

;;; Use standard MOVE-ARG + coercion to move an untagged character to
;;; a descriptor passing location.
(define-move-vop move-arg :move-arg
  (character-reg) (any-reg descriptor-reg))

;;;; Other operations:
(define-vop (char-code)
  (:translate char-code)
  (:policy :fast-safe)
  (:args (ch :scs (character-reg) :target res))
  (:arg-types character)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst sll res ch n-fixnum-tag-bits)))

(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (any-reg) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (character-reg)))
  (:result-types character)
  (:generator 1
    (inst srl res code n-fixnum-tag-bits)))

;;; Comparison of characters.
(define-vop (character-compare)
  (:args (x :scs (character-reg))
         (y :scs (character-reg)))
  (:arg-types character character)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition)
  (:generator 3
    (three-way-comparison x y condition :unsigned not-p target temp)))

(define-vop (fast-char=/character character-compare)
  (:translate char=)
  (:variant :eq))

(define-vop (fast-char</character character-compare)
  (:translate char<)
  (:variant :lt))

(define-vop (fast-char>/character character-compare)
  (:translate char>)
  (:variant :gt))

