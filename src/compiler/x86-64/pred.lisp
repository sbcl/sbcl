;;;; predicate VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; the branch VOP

;;; The unconditional branch, emitted when we can't drop through to the desired
;;; destination. Dest is the continuation we transfer control to.
(define-vop (branch)
  (:info dest)
  (:generator 5
    (inst jmp dest)))


;;;; conditional VOPs

;;; Note: a constant-tn is allowed in CMP; it uses an EA displacement,
;;; not immediate data.
(define-vop (if-eq)
  (:args (x :scs (any-reg descriptor-reg control-stack constant)
	    :load-if (not (and (sc-is x immediate)
			       (sc-is y any-reg descriptor-reg
				      control-stack constant))))
	 (y :scs (any-reg descriptor-reg immediate)
	    :load-if (not (and (sc-is x any-reg descriptor-reg immediate)
			       (sc-is y control-stack constant)))))
  (:temporary (:sc descriptor-reg) temp)
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:translate eq)
  (:generator 3
    (cond
     ((sc-is y immediate)
      (let ((val (tn-value y)))
	(etypecase val
	  (integer
	   (if (and (zerop val) (sc-is x any-reg descriptor-reg))
	       (inst test x x) ; smaller
	     (let ((fixnumized (fixnumize val)))
	       (if (typep fixnumized
			  '(or (signed-byte 32) (unsigned-byte 31)))
		   (inst cmp x fixnumized)
		 (progn
		   (inst mov temp fixnumized)
		   (inst cmp x temp))))))
	  (symbol
	   (inst cmp x (+ nil-value (static-symbol-offset val))))
	  (character
	   (inst cmp x (logior (ash (char-code val) n-widetag-bits)
			       character-widetag))))))
     ((sc-is x immediate) ; and y not immediate
      ;; Swap the order to fit the compare instruction.
      (let ((val (tn-value x)))
	(etypecase val
	  (integer
	   (if (and (zerop val) (sc-is y any-reg descriptor-reg))
	       (inst test y y) ; smaller
	     (let ((fixnumized (fixnumize val)))
	       (if (typep fixnumized
			  '(or (signed-byte 32) (unsigned-byte 31)))
		   (inst cmp y fixnumized)
		 (progn
		   (inst mov temp fixnumized)
		   (inst cmp y temp))))))
	  (symbol
	   (inst cmp y (+ nil-value (static-symbol-offset val))))
	  (character
	   (inst cmp y (logior (ash (char-code val) n-widetag-bits)
			       character-widetag))))))
      (t
       (inst cmp x y)))

    (inst jmp (if not-p :ne :e) target)))
