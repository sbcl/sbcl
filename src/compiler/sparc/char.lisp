;;;; the Sparc VM definition of character operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; moves and coercions:

;;; Move a tagged char to an untagged representation.
(define-vop (move-to-base-char)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (base-char-reg)))
  (:note "character untagging")
  (:generator 1
    (inst srl y x n-widetag-bits)))

(define-move-vop move-to-base-char :move
  (any-reg descriptor-reg) (base-char-reg))


;;; Move an untagged char to a tagged representation.
(define-vop (move-from-base-char)
  (:args (x :scs (base-char-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:note "character tagging")
  (:generator 1
    (inst sll y x n-widetag-bits)
    (inst or y base-char-widetag)))

(define-move-vop move-from-base-char :move
  (base-char-reg) (any-reg descriptor-reg))

;;; Move untagged base-char values.
(define-vop (base-char-move)
  (:args (x :target y
	    :scs (base-char-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (base-char-reg)
	       :load-if (not (location= x y))))
  (:note "character move")
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))

(define-move-vop base-char-move :move
  (base-char-reg) (base-char-reg))


;;; Move untagged base-char arguments/return-values.
(define-vop (move-base-char-arg)
  (:args (x :target y
	    :scs (base-char-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y base-char-reg))))
  (:results (y))
  (:note "character arg move")
  (:generator 0
    (sc-case y
      (base-char-reg
       (move y x))
      (base-char-stack
       (storew x fp (tn-offset y))))))

(define-move-vop move-base-char-arg :move-arg
  (any-reg base-char-reg) (base-char-reg))


;;; Use standard MOVE-ARG + coercion to move an untagged base-char
;;; to a descriptor passing location.
(define-move-vop move-arg :move-arg
  (base-char-reg) (any-reg descriptor-reg))



;;;; Other operations:

(define-vop (char-code)
  (:translate char-code)
  (:policy :fast-safe)
  (:args (ch :scs (base-char-reg) :target res))
  (:arg-types base-char)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst sll res ch n-fixnum-tag-bits)))

(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (any-reg) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (base-char-reg)))
  (:result-types base-char)
  (:generator 1
    (inst srl res code n-fixnum-tag-bits)))


;;; Comparison of base-chars.
(define-vop (base-char-compare)
  (:args (x :scs (base-char-reg))
	 (y :scs (base-char-reg)))
  (:arg-types base-char base-char)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition not-condition)
  (:generator 3
    (inst cmp x y)
    (inst b (if not-p not-condition condition) target)
    (inst nop)))

(define-vop (fast-char=/base-char base-char-compare)
  (:translate char=)
  (:variant :eq :ne))

(define-vop (fast-char</base-char base-char-compare)
  (:translate char<)
  (:variant :ltu :geu))

(define-vop (fast-char>/base-char base-char-compare)
  (:translate char>)
  (:variant :gtu :leu))
