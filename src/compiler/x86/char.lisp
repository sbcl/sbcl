;;;; x86 definition of character operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; moves and coercions

;;; Move a tagged char to an untagged representation.
(define-vop (move-to-character)
  (:args (x :scs (any-reg control-stack) :target y))
  (:results (y :scs (character-reg character-stack)))
  (:note "character untagging")
  (:generator 1
    (move y x)
    (inst shr y n-widetag-bits)))
(define-move-vop move-to-character :move
  (any-reg control-stack) (character-reg character-stack))

;;; Move an untagged char to a tagged representation.
(define-vop (move-from-character)
  (:args (x :scs (character-reg character-stack)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:note "character tagging")
  (:generator 1
    ;; FIXME: is this inefficient?  Is there a better way of writing
    ;; it?  (fixnum tagging is done with LEA).  We can't use SHL
    ;; because we either scribble over the source register or briefly
    ;; have a non-descriptor in a descriptor register, unless we
    ;; introduce a temporary.
    (inst imul y x (ash 1 n-widetag-bits))
    (inst or y character-widetag)))
(define-move-vop move-from-character :move
  (character-reg character-stack) (any-reg descriptor-reg control-stack))

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
       ;; copied blindly.  No idea if it's right
       (if (= (tn-offset fp) esp-offset)
	   (storew x fp (tn-offset y))	; c-call
	   (storew x fp (- (1+ (tn-offset y)))))))))
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
  (:args (ch :scs (character-reg character-stack)))
  (:arg-types character)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst mov res ch)))

(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (unsigned-reg unsigned-stack)))
  (:arg-types positive-fixnum)
  (:results (res :scs (character-reg)))
  (:result-types character)
  (:generator 1
    (inst mov res code)))

;;; comparison of CHARACTERs
(define-vop (character-compare)
  (:args (x :scs (character-reg character-stack))
	 (y :scs (character-reg)
	    :load-if (not (and (sc-is x character-reg)
			       (sc-is y character-stack)))))
  (:arg-types character character)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars condition not-condition)
  (:generator 3
    (inst cmp x y)
    (inst jmp (if not-p not-condition condition) target)))

(define-vop (fast-char=/character character-compare)
  (:translate char=)
  (:variant :e :ne))

(define-vop (fast-char</character character-compare)
  (:translate char<)
  (:variant :b :nb))

(define-vop (fast-char>/character character-compare)
  (:translate char>)
  (:variant :a :na))

(define-vop (character-compare/c)
  (:args (x :scs (character-reg character-stack)))
  (:arg-types character (:constant character))
  (:conditional)
  (:info target not-p y)
  (:policy :fast-safe)
  (:note "inline constant comparison")
  (:variant-vars condition not-condition)
  (:generator 2
    (inst cmp x (sb!xc:char-code y))
    (inst jmp (if not-p not-condition condition) target)))

(define-vop (fast-char=/character/c character-compare/c)
  (:translate char=)
  (:variant :e :ne))

(define-vop (fast-char</character/c character-compare/c)
  (:translate char<)
  (:variant :b :nb))

(define-vop (fast-char>/character/c character-compare/c)
  (:translate char>)
  (:variant :a :na))
