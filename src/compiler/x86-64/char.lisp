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
;;; The best possible instruction sequences for various X and Y SC's are:
;;; * reg-to-reg: dword move, then shift
;;; * reg-to-mem: dword move to temp, shift temp, store as qword
;;; * mem-to-reg: misaligned dword load (read 4 bytes at byte index 1)
;;; * mem-to-mem: misaligned dword load to temp, store as a qword
;;;
;;; Since there's no such thing as a 3-byte load, the misaligned loads
;;; overrun a naturally aligned :dword within the :qword, but that's fine.
;;; However, we can't actually implement that because we lack a way to
;;; create an arbitrary EA given a stack TN. So, instead do:
;;; * mem-to-reg: dword load, shift
;;; * mem-to-mem: dword load to temp, shift temp, store as a qword
(defun untagify-char (dst src shift temp)
  (if (and (location= src dst) (stack-tn-p dst)) ; shift right in memory
      (inst shr :dword dst shift)
      (let ((reg (if (stack-tn-p dst) temp dst)))
        (move reg src :dword)
        (inst shr :dword reg shift)
        (when (stack-tn-p dst) ; store as qword to ensure upper bytes are 0
          (inst mov dst temp)))))
(define-vop (move-to-character)
  (:args (x :scs (any-reg descriptor-reg control-stack) :target y :load-if nil))
  (:results (y :scs (character-reg character-stack) :load-if nil))
  (:temporary (:sc unsigned-reg) temp)
  (:note "character untagging")
  (:generator 1 (untagify-char y x n-widetag-bits temp)))
(define-move-vop move-to-character :move
  (any-reg)
  (character-reg))

;;; Move a tagged character in an {ANY|DESCRIPTOR}-REG or control stack
;;; to a fixnum in those same SCs.  This is untaggeding and fixnumizing
;;; in one right shift, not a right + left shift.
;;; ASSUMPTION: the topmost bit in character-widetag is 0
(eval-when (:compile-toplevel) ; Verify assumption.
  (assert (not (logbitp 7 character-widetag))))
(define-vop (tagged-char-code) ; valid only if N-FIXNUM-TAG-BITS = 1
  (:args (x :scs (any-reg descriptor-reg control-stack) :target y :load-if nil))
  (:results (y :scs (any-reg descriptor-reg control-stack) :load-if nil))
  (:temporary (:sc unsigned-reg) temp)
  (:note "character untagging")
  (:generator 1
    (untagify-char y x (- n-widetag-bits n-fixnum-tag-bits) temp)
    (when (> n-fixnum-tag-bits 1) (inst and :dword y fixnum-tag-mask))))

;;; Move an untagged char to a tagged representation.
(define-vop (move-from-character)
  (:args (x :scs (character-reg) :target y))
  (:results (y :scs (any-reg descriptor-reg)))
  (:note "character tagging")
  (:generator 1
    (move y x :dword)
    (inst shl :dword y n-widetag-bits)
    (inst or :dword y character-widetag)))
(define-move-vop move-from-character :move
  (character-reg)
  (any-reg descriptor-reg))

;;; Move untagged character values.
(defun move-raw-char-code (dst src temp) ; move SRC to DST
  (unless (location= src dst)
    ;; Aways store as :qword to stack (storing the upper 32 zero bits) so that
    ;; loading as either :dword or :qword is ok.
    ;; We can see immediate constants here which become untagged integers.
    (inst mov (if (stack-tn-p dst) :qword :dword) dst
          (cond ((stack-tn-p src) (inst mov :qword temp src) temp)
                ((encode-value-if-immediate src nil))))))
(define-vop (character-move)
  (:args (x :target y :scs (character-reg character-stack) :load-if nil))
  (:results (y :scs (character-reg character-stack) :load-if nil))
  (:temporary (:sc unsigned-reg) temp)
  (:note "character move")
  (:generator 0 (move-raw-char-code y x temp)))
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
       (if (= (tn-offset fp) rsp-offset)
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
  (:args (ch :scs (character-reg character-stack) :target res :load-if nil))
  (:arg-types character)
  (:results (res :scs (unsigned-reg unsigned-stack) :load-if nil))
  (:result-types positive-fixnum)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 1 (move-raw-char-code res ch temp)))

(define-vop (code-char)
  (:translate code-char)
  (:policy :fast-safe)
  (:args (code :scs (unsigned-reg unsigned-stack) :target res :load-if nil))
  (:arg-types positive-fixnum)
  (:results (res :scs (character-reg character-stack) :load-if nil))
  (:result-types character)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 1 (move-raw-char-code res code temp)))

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
    (inst cmp :dword x y)))

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
    (inst cmp :dword x (char-code y))))

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

;;; Replace the triple (MOVE-TO-CHARACTER CHAR-CODE MOVE-FROM-WORD/FIXNUM)
;;; with just the TAGGED-CHAR-CODE vop.
(defoptimizer (sb-c::vop-optimize move-to-character) (vop)
  (when (and (sb-c:next-vop-is vop 'char-code)
             (sb-c:next-vop-is (sb-c::next-vop vop) 'move-from-word/fixnum))
    (sb-c:replace-vops 3 vop 'tagged-char-code)))
