;;;; the RV32 VM definition of operand loading/saving and the MOVE vop

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun load-immediate-word (y val))

(define-move-fun (load-immediate 1) (vop x y)
    ((immediate) (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer (load-immediate-word y (fixnumize val))))))

(define-move-fun (load-number 1) (vop x y)
    ((immediate) (signed-reg unsigned-reg))
  (load-immediate-word y (tn-value x)))

(define-move-fun (load-stack 5) (vop x y)
  ((control-stack) (any-reg descriptor-reg))
  (load-stack-tn y x))

(define-move-fun (load-number-stack 5) (vop x y)
  ((character-stack) (character-reg)
   (sap-stack) (sap-reg)
   (signed-stack) (signed-reg)
   (unsigned-stack) (unsigned-reg))
  (loadw y (current-nfp-tn vop) (tn-offset x)))

(define-move-fun (store-stack 5) (vop x y)
  ((any-reg descriptor-reg) (control-stack))
  (store-stack-tn y x))

(define-move-fun (store-number-stack 5) (vop x y)
  ((character-reg) (character-stack)
   (sap-reg) (sap-stack)
   (signed-reg) (signed-stack)
   (unsigned-reg) (unsigned-stack))
  (storew x (current-nfp-tn vop) (tn-offset y)))

(define-vop (move)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (any-reg descriptor-reg control-stack)))
  (:generator 0))
(define-move-vop move :move
  (any-reg descriptor-reg) (any-reg descriptor-reg))

(define-vop (move-to-word/fixnum)
  (:args (x :scs (any-reg descriptor-reg)))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:arg-types tagged-num)
  (:generator 1))
(define-move-vop move-to-word/fixnum :move
  (any-reg descriptor-reg) (signed-reg unsigned-reg))
(define-vop (move-from-word/fixnum)
  (:args (x :scs (signed-reg unsigned-reg) :target y))
  (:results (y :scs (any-reg descriptor-reg)))
  (:result-types tagged-num)
  (:generator 1))
(define-move-vop move-from-word/fixnum :move
  (signed-reg unsigned-reg) (any-reg descriptor-reg))

(define-vop (move-from-signed)
  (:args (x :scs (signed-reg unsigned-reg) :target y))
  (:results (y :scs (any-reg descriptor-reg)))
  (:generator 18))
(define-move-vop move-from-signed :move
  (signed-reg) (descriptor-reg))
(define-vop (move-from-unsigned)
  (:args (x :scs (signed-reg unsigned-reg) :target y))
  (:results (y :scs (any-reg descriptor-reg)))
  (:generator 20))
(define-move-vop move-from-unsigned :move
  (unsigned-reg) (descriptor-reg))

(define-vop (word-move)
  (:args (x :scs (signed-reg unsigned-reg) :target y))
  (:results (y :scs (signed-reg unsigned-reg)))
  (:generator 0))
