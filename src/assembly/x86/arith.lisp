;;;; simple cases for generic arithmetic

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; addition, subtraction, and multiplication

(macrolet ((define-generic-arith-routine ((fun cost) &body body)
	     `(define-assembly-routine (,(symbolicate "GENERIC-" fun)
					(:cost ,cost)
					(:return-style :full-call)
					(:translate ,fun)
					(:policy :safe)
					(:save-p t))
		((:arg x (descriptor-reg any-reg) edx-offset)
		 (:arg y (descriptor-reg any-reg)
		       ;; this seems wrong esi-offset -- FIXME: What's it mean?
		       edi-offset)

		 (:res res (descriptor-reg any-reg) edx-offset)

		 (:temp eax unsigned-reg eax-offset)
		 (:temp ebx unsigned-reg ebx-offset)
		 (:temp ecx unsigned-reg ecx-offset))

		(declare (ignorable ebx))

		(inst test x 3)	 ; fixnum?
		(inst jmp :nz DO-STATIC-FUN) ; no - do generic
		(inst test y 3)	 ; fixnum?
		(inst jmp :z DO-BODY)   ; yes - doit here

		DO-STATIC-FUN
		(inst pop eax)
		(inst push ebp-tn)
		(inst lea
		      ebp-tn
		      (make-ea :dword :base esp-tn :disp word-bytes))
		(inst sub esp-tn (fixnumize 2))
		(inst push eax)	 ; callers return addr
		(inst mov ecx (fixnumize 2)) ; arg count
		(inst jmp
		      (make-ea :dword
			       :disp (+ nil-value
					(static-function-offset
					 ',(symbolicate "TWO-ARG-" fun)))))

		DO-BODY
		,@body)))

  (define-generic-arith-routine (+ 10)
    (move res x)
    (inst add res y)
    (inst jmp :no OKAY)
    (inst rcr res 1)		      ; carry has correct sign
    (inst sar res 1)		      ; remove type bits

    (move ecx res)

    (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset))
      (storew ecx res bignum-digits-offset other-pointer-lowtag))

    OKAY)

  (define-generic-arith-routine (- 10)
    ;; FIXME: This is screwed up.
      ;;; I can't figure out the flags on subtract. Overflow never gets
      ;;; set and carry always does. (- 0 most-negative-fixnum) can't be
      ;;; easily detected so just let the upper level stuff do it.
    (inst jmp DO-STATIC-FUN)

    (move res x)
    (inst sub res y)
    (inst jmp :no OKAY)
    (inst rcr res 1)
    (inst sar res 1)		      ; remove type bits

    (move ecx res)

    (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset))
      (storew ecx res bignum-digits-offset other-pointer-lowtag))
    OKAY)

  (define-generic-arith-routine (* 30)
    (move eax x)			  ; must use eax for 64-bit result
    (inst sar eax 2)		      ; remove *4 fixnum bias
    (inst imul y)			 ; result in edx:eax
    (inst jmp :no okay)		   ; still fixnum

    ;; zzz jrd changed edx to ebx in here, as edx isn't listed as a temp, above
    ;;     pfw says that loses big -- edx is target for arg x and result res
    ;;     note that 'edx' is not defined -- using x
    (inst shrd eax x 2)		   ; high bits from edx
    (inst sar x 2)			; now shift edx too

    (move ecx x)			  ; save high bits from cdq
    (inst cdq)			    ; edx:eax <- sign-extend of eax
    (inst cmp x ecx)
    (inst jmp :e SINGLE-WORD-BIGNUM)

    (with-fixed-allocation (res bignum-type (+ bignum-digits-offset 2))
      (storew eax res bignum-digits-offset other-pointer-lowtag)
      (storew ecx res (1+ bignum-digits-offset) other-pointer-lowtag))
    (inst jmp DONE)

    SINGLE-WORD-BIGNUM

    (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset))
      (storew eax res bignum-digits-offset other-pointer-lowtag))
    (inst jmp DONE)

    OKAY
    (move res eax)
    DONE))

;;;; negation

(define-assembly-routine (generic-negate
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate %negate)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) edx-offset)
			  (:res res (descriptor-reg any-reg) edx-offset)

			  (:temp eax unsigned-reg eax-offset)
			  (:temp ecx unsigned-reg ecx-offset))
  (inst test x 3)
  (inst jmp :z FIXNUM)

  (inst pop eax)
  (inst push ebp-tn)
  (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
  (inst sub esp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 1))	  ; arg count
  (inst jmp (make-ea :dword
		     :disp (+ nil-value (static-function-offset '%negate))))

  FIXNUM
  (move res x)
  (inst neg res)			; (- most-negative-fixnum) is BIGNUM
  (inst jmp :no OKAY)
  (inst shr res 2)		      ; sign bit is data - remove type bits
  (move ecx res)

  (with-fixed-allocation (res bignum-type (1+ bignum-digits-offset))
    (storew ecx res bignum-digits-offset other-pointer-lowtag))

  OKAY)

;;;; comparison

(macrolet ((define-cond-assem-rtn (name translate static-fn test)
	     `(define-assembly-routine (,name
					(:cost 10)
					(:return-style :full-call)
					(:policy :safe)
					(:translate ,translate)
					(:save-p t))
		((:arg x (descriptor-reg any-reg) edx-offset)
		 (:arg y (descriptor-reg any-reg) edi-offset)

		 (:res res descriptor-reg edx-offset)

		 (:temp eax unsigned-reg eax-offset)
		 (:temp ecx unsigned-reg ecx-offset))

		;; KLUDGE: The "3" here is a mask for the bits which will be
		;; zero in a fixnum. It should have a symbolic name. (Actually,
		;; it might already have a symbolic name which the coder
		;; couldn't be bothered to use..) -- WHN 19990917
		(inst test x 3)
		(inst jmp :nz TAIL-CALL-TO-STATIC-FN)
		(inst test y 3)
		(inst jmp :z INLINE-FIXNUM-COMPARE)

		TAIL-CALL-TO-STATIC-FN
		(inst pop eax)
		(inst push ebp-tn)
		(inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
		(inst sub esp-tn (fixnumize 2)) ; FIXME: Push 2 words on stack,
						; weirdly?
		(inst push eax)
		(inst mov ecx (fixnumize 2)) ; FIXME: FIXNUMIZE and
					; SINGLE-FLOAT-BITS are parallel,
					; should be named parallelly.
		(inst jmp (make-ea :dword
				   :disp (+ nil-value
					    (static-function-offset
					     ',static-fn))))

		INLINE-FIXNUM-COMPARE
		(inst cmp x y)
		(inst jmp ,test RETURN-TRUE)
		(inst mov res nil-value)
		;; FIXME: A note explaining this return convention, or a
		;; symbolic name for it, would be nice. (It looks as though we
		;; should be hand-crafting the same return sequence as would be
		;; produced by GENERATE-RETURN-SEQUENCE, but in that case it's
		;; not clear why we don't just jump to the end of this function
		;; to share the return sequence there.
		(inst pop eax)
		(inst add eax 2)
		(inst jmp eax)

		RETURN-TRUE
		(load-symbol res t))))

  (define-cond-assem-rtn generic-< < two-arg-< :l)
  (define-cond-assem-rtn generic-> > two-arg-> :g))

(define-assembly-routine (generic-eql
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate eql)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) edx-offset)
			  (:arg y (descriptor-reg any-reg) edi-offset)

			  (:res res descriptor-reg edx-offset)

			  (:temp eax unsigned-reg eax-offset)
			  (:temp ecx unsigned-reg ecx-offset))
  (inst cmp x y)
  (inst jmp :e RETURN-T)
  (inst test x 3)
  (inst jmp :z RETURN-NIL)
  (inst test y 3)
  (inst jmp :nz DO-STATIC-FN)

  RETURN-NIL
  (inst mov res nil-value)
  (inst pop eax)
  (inst add eax 2)
  (inst jmp eax)

  DO-STATIC-FN
  (inst pop eax)
  (inst push ebp-tn)
  (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
  (inst sub esp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 2))
  (inst jmp (make-ea :dword
		     :disp (+ nil-value (static-function-offset 'eql))))

  RETURN-T
  (load-symbol res t)
  ;; FIXME: I don't understand how we return from here..
  )

(define-assembly-routine (generic-=
			  (:cost 10)
			  (:return-style :full-call)
			  (:policy :safe)
			  (:translate =)
			  (:save-p t))
			 ((:arg x (descriptor-reg any-reg) edx-offset)
			  (:arg y (descriptor-reg any-reg) edi-offset)

			  (:res res descriptor-reg edx-offset)

			  (:temp eax unsigned-reg eax-offset)
			  (:temp ecx unsigned-reg ecx-offset)
			  )
  (inst test x 3)		       ; descriptor?
  (inst jmp :nz DO-STATIC-FN)          ; yes, do it here
  (inst test y 3)		       ; descriptor?
  (inst jmp :nz DO-STATIC-FN)
  (inst cmp x y)
  (inst jmp :e RETURN-T)		; ok

  (inst mov res nil-value)
  (inst pop eax)
  (inst add eax 2)
  (inst jmp eax)

  DO-STATIC-FN
  (inst pop eax)
  (inst push ebp-tn)
  (inst lea ebp-tn (make-ea :dword :base esp-tn :disp word-bytes))
  (inst sub esp-tn (fixnumize 2))
  (inst push eax)
  (inst mov ecx (fixnumize 2))
  (inst jmp (make-ea :dword
		     :disp (+ nil-value (static-function-offset 'two-arg-=))))

  RETURN-T
  (load-symbol res t))


;;; Support for the Mersenne Twister, MT19937, random number generator
;;; due to Matsumoto and Nishimura.
;;;
;;; Makoto Matsumoto and T. Nishimura, "Mersenne twister: A
;;; 623-dimensionally equidistributed uniform pseudorandom number
;;; generator.", ACM Transactions on Modeling and Computer Simulation,
;;; 1997, to appear.
;;;
;;; State:
;;;  0-1:   Constant matrix A. [0, #x9908b0df] (not used here)
;;;  2:     Index; init. to 1.
;;;  3-626: State.

;;; This assembly routine is called from the inline VOP and updates
;;; the state vector with new random numbers. The state vector is
;;; passed in the EAX register.
#+sb-assembling ; We don't want a vop for this one.
(define-assembly-routine
    (random-mt19937-update)
    ((:temp state unsigned-reg eax-offset)
     (:temp k unsigned-reg ebx-offset)
     (:temp y unsigned-reg ecx-offset)
     (:temp tmp unsigned-reg edx-offset))

  ;; Save the temporary registers.
  (inst push k)
  (inst push y)
  (inst push tmp)

  ;; Generate a new set of results.
  (inst xor k k)
  LOOP1
  (inst mov y (make-ea :dword :base state :index k :scale 4
		       :disp (- (* (+ 3 sb!vm:vector-data-offset)
				   sb!vm:word-bytes)
				sb!vm:other-pointer-lowtag)))
  (inst mov tmp (make-ea :dword :base state :index k :scale 4
			 :disp (- (* (+ 1 3 sb!vm:vector-data-offset)
				     sb!vm:word-bytes)
				  sb!vm:other-pointer-lowtag)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip1)
  (inst xor y #x9908b0df)
  SKIP1
  (inst xor y (make-ea :dword :base state :index k :scale 4
		       :disp (- (* (+ 397 3 sb!vm:vector-data-offset)
				   sb!vm:word-bytes)
				sb!vm:other-pointer-lowtag)))
  (inst mov (make-ea :dword :base state :index k :scale 4
		     :disp (- (* (+ 3 sb!vm:vector-data-offset)
				 sb!vm:word-bytes)
			      sb!vm:other-pointer-lowtag))
	y)
  (inst inc k)
  (inst cmp k (- 624 397))
  (inst jmp :b loop1)
  LOOP2
  (inst mov y (make-ea :dword :base state :index k :scale 4
		       :disp (- (* (+ 3 sb!vm:vector-data-offset)
				   sb!vm:word-bytes)
				sb!vm:other-pointer-lowtag)))
  (inst mov tmp (make-ea :dword :base state :index k :scale 4
			 :disp (- (* (+ 1 3 sb!vm:vector-data-offset)
				     sb!vm:word-bytes)
				  sb!vm:other-pointer-lowtag)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip2)
  (inst xor y #x9908b0df)
  SKIP2
  (inst xor y (make-ea :dword :base state :index k :scale 4
		       :disp (- (* (+ (- 397 624) 3 sb!vm:vector-data-offset)
				   sb!vm:word-bytes)
				sb!vm:other-pointer-lowtag)))
  (inst mov (make-ea :dword :base state :index k :scale 4
		     :disp (- (* (+ 3 sb!vm:vector-data-offset)
				 sb!vm:word-bytes)
			      sb!vm:other-pointer-lowtag))
	y)
  (inst inc k)
  (inst cmp k (- 624 1))
  (inst jmp :b loop2)

  (inst mov y (make-ea :dword :base state
		       :disp (- (* (+ (- 624 1) 3 sb!vm:vector-data-offset)
				   sb!vm:word-bytes)
				sb!vm:other-pointer-lowtag)))
  (inst mov tmp (make-ea :dword :base state
			 :disp (- (* (+ 0 3 sb!vm:vector-data-offset)
				     sb!vm:word-bytes)
				  sb!vm:other-pointer-lowtag)))
  (inst and y #x80000000)
  (inst and tmp #x7fffffff)
  (inst or y tmp)
  (inst shr y 1)
  (inst jmp :nc skip3)
  (inst xor y #x9908b0df)
  SKIP3
  (inst xor y (make-ea :dword :base state
		       :disp (- (* (+ (- 397 1) 3 sb!vm:vector-data-offset)
				   sb!vm:word-bytes)
				sb!vm:other-pointer-lowtag)))
  (inst mov (make-ea :dword :base state
		     :disp (- (* (+ (- 624 1) 3 sb!vm:vector-data-offset)
				 sb!vm:word-bytes)
			      sb!vm:other-pointer-lowtag))
	y)

  ;; Restore the temporary registers and return.
  (inst pop tmp)
  (inst pop y)
  (inst pop k)
  (inst ret))
