(in-package "SB!VM")


;;;; Multiplication and Division helping routines.

;;; ?? FIXME: Where are generic-* and generic-/?
#+sb-assembling
(define-assembly-routine
    multiply
    ((:arg x (signed-reg) nl0-offset)
     (:arg y (signed-reg) nl1-offset)

     (:res res (signed-reg) nl2-offset)

     (:temp tmp (unsigned-reg) nl3-offset)
     (:temp sign (unsigned-reg) nl4-offset))

  ;; Determine the sign of the result.
  (inst extrs x 0 1 sign :=)
  (inst sub zero-tn x x)
  (inst extrs y 0 1 tmp :=)
  (inst sub zero-tn y y)
  (inst xor sign tmp sign)

  ;; Make sure X is less then Y.
  (inst comclr x y tmp :<<)
  (inst xor x y tmp)
  (inst xor x tmp x)
  (inst xor y tmp y)
  ;; Blow out of here if the result is zero.
  (inst comb := x zero-tn done)
  (inst li 0 res)

  LOOP
  (inst extru x 31 1 zero-tn :ev)
  (inst add y res res)
  (inst extru x 30 1 zero-tn :ev)
  (inst sh1add y res res)
  (inst extru x 29 1 zero-tn :ev)
  (inst sh2add y res res)
  (inst extru x 28 1 zero-tn :ev)
  (inst sh3add y res res)

  (inst srl x 4 x)
  (inst comb :<> x zero-tn loop)
  (inst sll y 4 y)

  DONE
  (inst xor res sign res)
  (inst add res sign res))


#+sb-assembling
(define-assembly-routine
    (truncate)
    ((:arg dividend signed-reg nl0-offset)
     (:arg divisor signed-reg nl1-offset)

     (:res quo signed-reg nl2-offset)
     (:res rem signed-reg nl3-offset))
  
  ;; Move abs(divident) into quo.
  (inst move dividend quo :>=)
  (inst sub zero-tn quo quo)
  ;; Do one divive-step with -divisor to prime V  (use rem as a temp)
  (inst sub zero-tn divisor rem)
  (inst ds zero-tn rem zero-tn)
  ;; Shift the divident/quotient one bit, setting the carry flag.
  (inst add quo quo quo)
  ;; The first real divive-step.
  (inst ds zero-tn divisor rem)
  (inst addc quo quo quo)
  ;; And 31 more of them.
  (dotimes (i 31)
    (inst ds rem divisor rem)
    (inst addc quo quo quo))
  ;; If the remainder is negative, we need to add the absolute value of the
  ;; divisor.
  (inst comb :>= rem zero-tn remainder-positive)
  (inst comclr divisor zero-tn zero-tn :<)
  (inst add rem divisor rem :tr)
  (inst sub rem divisor rem)
  REMAINDER-POSITIVE
  ;; Now we have to fix the signs of quo and rem.
  (inst xor divisor dividend zero-tn :>=)
  (inst sub zero-tn quo quo)
  (inst move dividend zero-tn :>=)
  (inst sub zero-tn rem rem))



;;;; Generic arithmetic.

(define-assembly-routine (generic-+
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate +)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst extru x 31 2 zero-tn :=)
  (inst b do-static-fun :nullify t)
  (inst extru y 31 2 zero-tn :=)
  (inst b do-static-fun :nullify t)
  (inst addo x y res)
  (lisp-return lra :offset 1)

  DO-STATIC-FUN
  (inst ldw (static-fun-offset 'two-arg-+) null-tn lip)
  (inst li (fixnumize 2) nargs)
  (inst move cfp-tn ocfp)
  (inst bv lip)
  (inst move csp-tn cfp-tn))

(define-assembly-routine (generic--
			  (:cost 10)
			  (:return-style :full-call)
			  (:translate -)
			  (:policy :safe)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) a0-offset)
			  (:arg y (descriptor-reg any-reg) a1-offset)

			  (:res res (descriptor-reg any-reg) a0-offset)

			  (:temp lip interior-reg lip-offset)
			  (:temp lra descriptor-reg lra-offset)
			  (:temp nargs any-reg nargs-offset)
			  (:temp ocfp any-reg ocfp-offset))
  (inst extru x 31 2 zero-tn :=)
  (inst b do-static-fun :nullify t)
  (inst extru y 31 2 zero-tn :=)
  (inst b do-static-fun :nullify t)
  (inst subo x y res)
  (lisp-return lra :offset 1)

  DO-STATIC-FUN
  (inst ldw (static-fun-offset 'two-arg--) null-tn lip)
  (inst li (fixnumize 2) nargs)
  (inst move cfp-tn ocfp)
  (inst bv lip)
  (inst move csp-tn cfp-tn))



;;;; Comparison routines.

(macrolet
    ((define-cond-assem-rtn (name translate static-fn cond)
       `(define-assembly-routine (,name
				  (:cost 10)
				  (:return-style :full-call)
				  (:policy :safe)
				  (:translate ,translate)
				  (:save-p t))
				 ((:arg x (descriptor-reg any-reg) a0-offset)
				  (:arg y (descriptor-reg any-reg) a1-offset)
				  
				  (:res res descriptor-reg a0-offset)
				  
				  (:temp lip interior-reg lip-offset)
				  (:temp lra descriptor-reg lra-offset)
				  (:temp nargs any-reg nargs-offset)
				  (:temp ocfp any-reg ocfp-offset))
	  (inst extru x 31 2 zero-tn :=)
	  (inst b do-static-fn :nullify t)
	  (inst extru y 31 2 zero-tn :=)
	  (inst b do-static-fn :nullify t)

	  (inst comclr x y zero-tn ,cond)
	  (inst move null-tn res :tr)
	  (load-symbol res t)
	  (lisp-return lra :offset 1)

	  DO-STATIC-FN
	  (inst ldw (static-fun-offset ',static-fn) null-tn lip)
	  (inst li (fixnumize 2) nargs)
	  (inst move cfp-tn ocfp)
	  (inst bv lip)
	  (inst move csp-tn cfp-tn))))

  (define-cond-assem-rtn generic-< < two-arg-< :<)
  (define-cond-assem-rtn generic-> > two-arg-> :>))


(define-assembly-routine
    (generic-eql
     (:cost 10)
     (:return-style :full-call)
     (:policy :safe)
     (:translate eql)
     (:save-p t))
    ((:arg x (descriptor-reg any-reg) a0-offset)
     (:arg y (descriptor-reg any-reg) a1-offset)
     
     (:res res descriptor-reg a0-offset)
     
     (:temp lip interior-reg lip-offset)
     (:temp lra descriptor-reg lra-offset)
     (:temp nargs any-reg nargs-offset)
     (:temp ocfp any-reg ocfp-offset))

  (inst comb := x y return-t :nullify t)
  (inst extru x 31 2 zero-tn :<>)
  (inst b return-nil :nullify t)
  (inst extru y 31 2 zero-tn :=)
  (inst b do-static-fn :nullify t)

  RETURN-NIL
  (inst move null-tn res)
  (lisp-return lra :offset 1)

  DO-STATIC-FN
  (inst ldw (static-fun-offset 'eql) null-tn lip)
  (inst li (fixnumize 2) nargs)
  (inst move cfp-tn ocfp)
  (inst bv lip)
  (inst move csp-tn cfp-tn)

  RETURN-T
  (load-symbol res t))

(define-assembly-routine
    (generic-=
     (:cost 10)
     (:return-style :full-call)
     (:policy :safe)
     (:translate =)
     (:save-p t))
    ((:arg x (descriptor-reg any-reg) a0-offset)
     (:arg y (descriptor-reg any-reg) a1-offset)
     
     (:res res descriptor-reg a0-offset)
     
     (:temp lip interior-reg lip-offset)
     (:temp lra descriptor-reg lra-offset)
     (:temp nargs any-reg nargs-offset)
     (:temp ocfp any-reg ocfp-offset))

  (inst comb := x y return-t :nullify t)
  (inst extru x 31 2 zero-tn :=)
  (inst b do-static-fn :nullify t)
  (inst extru y 31 2 zero-tn :=)
  (inst b do-static-fn :nullify t)

  (inst move null-tn res)
  (lisp-return lra :offset 1)

  DO-STATIC-FN
  (inst ldw (static-fun-offset 'two-arg-=) null-tn lip)
  (inst li (fixnumize 2) nargs)
  (inst move cfp-tn ocfp)
  (inst bv lip)
  (inst move csp-tn cfp-tn)

  RETURN-T
  (load-symbol res t))
