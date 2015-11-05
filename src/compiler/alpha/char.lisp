;;;; the Alpha VM definition of character operations

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
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (character-reg)))
  (:generator 1
    (inst srl x n-widetag-bits y)))
(define-move-vop move-to-character :move
  (any-reg descriptor-reg) (character-reg))

;;; Move an untagged char to a tagged representation.
(define-vop (move-from-character)
  (:args (x :scs (character-reg)))
  (:results (y :scs (any-reg descriptor-reg)))
  (:generator 1
    (inst sll x n-widetag-bits y)
    (inst bis y character-widetag y)))
(define-move-vop move-from-character :move
  (character-reg) (any-reg descriptor-reg))

;;; Move untagged character values.
(define-vop (character-move)
  (:args (x :target y
            :scs (character-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (character-reg)
               :load-if (not (location= x y))))
  (:effects)
  (:affected)
  (:generator 0
    (move x y)))
(define-move-vop character-move :move
  (character-reg) (character-reg))

;;; Move untagged character arguments/return-values.
(define-vop (move-character-arg)
  (:args (x :target y
            :scs (character-reg))
         (fp :scs (any-reg)
             :load-if (not (sc-is y character-reg))))
  (:results (y))
  (:generator 0
    (sc-case y
      (character-reg
       (move x y))
      (character-stack
       (storew x fp (tn-offset y))))))
(define-move-vop move-character-arg :move-arg
  (any-reg character-reg) (character-reg))

;;; Use standard MOVE-ARG + coercion to move an untagged character
;;; to a descriptor passing location.
;;;
(define-move-vop move-arg :move-arg
  (character-reg) (any-reg descriptor-reg))

;;;; other operations
(define-vop (char-code)
  (:translate char-code)
  (:policy :fast-safe)
  (:args (ch :scs (character-reg) :target res))
  (:arg-types character)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 1
    (inst sll ch n-fixnum-tag-bits res)))

(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (any-reg) :target res))
  (:arg-types positive-fixnum)
  (:results (res :scs (character-reg)))
  (:result-types character)
  (:generator 1
    (inst srl code n-fixnum-tag-bits res)))

;;;; comparison of CHARACTERs

(define-vop (character-compare)
  (:args (x :scs (character-reg))
         (y :scs (character-reg)))
  (:arg-types character character)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:note "inline comparison")
  (:variant-vars cond)
  (:generator 3
    (ecase cond
      (:eq (inst cmpeq x y temp))
      (:lt (inst cmplt x y temp))
      (:gt (inst cmplt y x temp)))
    (if not-p
        (inst beq temp target)
        (inst bne temp target))))

(define-vop (fast-char=/character character-compare)
  (:translate char=)
  (:variant :eq))

(define-vop (fast-char</character character-compare)
  (:translate char<)
  (:variant :lt))

(define-vop (fast-char>/character character-compare)
  (:translate char>)
  (:variant :gt))

(define-vop (character-compare/c)
  (:args (x :scs (character-reg)))
  (:arg-types character
              ;; KLUDGE: having a SATISFIES type here is too hairy for
              ;; the cross-compiler (running on an arbitrary CL host)
              ;; to cope with.  Since we know we only have standard
              ;; characters in the build anyway, we can restrict the
              ;; cross-compiler's arg type to standard char, and all
              ;; is well.
              #+sb-xc-host
              (:constant standard-char)
              #-sb-xc-host
              (:constant (satisfies inlinable-character-constant-p)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:conditional)
  (:info target not-p y)
  (:policy :fast-safe)
  (:note "inline constant comparison")
  (:variant-vars cond)
  (:generator 2
    (ecase cond
      (:eq (inst cmpeq x (sb!xc:char-code y) temp))
      (:lt (inst cmplt x (sb!xc:char-code y) temp))
      (:gt (inst cmple x (sb!xc:char-code y) temp)))
    (if not-p
        (if (eq cond :gt)
            (inst bne temp target)
            (inst beq temp target))
        (if (eq cond :gt)
            (inst beq temp target)
            (inst bne temp target)))))

(define-vop (fast-char=/character/c character-compare/c)
  (:translate char=)
  (:variant :eq))

(define-vop (fast-char</character/c character-compare/c)
  (:translate char<)
  (:variant :lt))

(define-vop (fast-char>/character/c character-compare/c)
  (:translate char>)
  (:variant :gt))
