;;;; the VM definition of arithmetic VOPs for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; unary operations

(define-vop (fast-safe-arith-op)
  (:policy :fast-safe)
  (:effects)
  (:affected))

(define-vop (fixnum-unop fast-safe-arith-op)
  (:args (x :scs (any-reg) :target res))
  (:results (res :scs (any-reg)))
  (:note "inline fixnum arithmetic")
  (:arg-types tagged-num)
  (:result-types tagged-num))

(define-vop (signed-unop fast-safe-arith-op)
  (:args (x :scs (signed-reg) :target res))
  (:results (res :scs (signed-reg)))
  (:note "inline (signed-byte 32) arithmetic")
  (:arg-types signed-num)
  (:result-types signed-num))

(define-vop (fast-negate/fixnum fixnum-unop)
  (:translate %negate)
  (:generator 1
    (move res x)
    (inst neg res)))

(define-vop (fast-negate/signed signed-unop)
  (:translate %negate)
  (:generator 2
    (move res x)
    (inst neg res)))

(define-vop (fast-lognot/fixnum fixnum-unop)
  (:translate lognot)
  (:generator 2
    (move res x)
    (inst xor res (fixnumize -1))))

(define-vop (fast-lognot/signed signed-unop)
  (:translate lognot)
  (:generator 1
    (move res x)
    (inst not res)))

;;;; binary fixnum operations

;;; Assume that any constant operand is the second arg...

(define-vop (fast-fixnum-binop fast-safe-arith-op)
  (:args (x :target r :scs (any-reg)
	    :load-if (not (and (sc-is x control-stack)
			       (sc-is y any-reg)
			       (sc-is r control-stack)
			       (location= x r))))
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg) :from (:argument 0)
	       :load-if (not (and (sc-is x control-stack)
				  (sc-is y any-reg)
				  (sc-is r control-stack)
				  (location= x r)))))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg)
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is y unsigned-reg)
			       (sc-is r unsigned-stack)
			       (location= x r))))
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg) :from (:argument 0)
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is y unsigned-reg)
			       (sc-is r unsigned-stack)
			       (location= x r)))))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

(define-vop (fast-signed-binop fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg)
	    :load-if (not (and (sc-is x signed-stack)
			       (sc-is y signed-reg)
			       (sc-is r signed-stack)
			       (location= x r))))
	 (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 0)
	    :load-if (not (and (sc-is x signed-stack)
			       (sc-is y signed-reg)
			       (sc-is r signed-stack)
			       (location= x r)))))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(define-vop (fast-fixnum-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (any-reg control-stack)))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 29)))
  (:results (r :scs (any-reg)
	       :load-if (not (location= x r))))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

;; 31 not 64 because it's hard work loading 64 bit constants, and since
;; sign-extension of immediates causes problems with 32.
(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg unsigned-stack)))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 31)))
  (:results (r :scs (unsigned-reg)
	       :load-if (not (location= x r))))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

;; 32 not 64 because it's hard work loading 64 bit constants
(define-vop (fast-signed-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (signed-reg signed-stack)))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)
	       :load-if (not (location= x r))))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic"))

(macrolet ((define-binop (translate untagged-penalty op)
	     `(progn
		(define-vop (,(symbolicate "FAST-" translate "/FIXNUM=>FIXNUM")
			     fast-fixnum-binop)
		  (:translate ,translate)
		  (:generator 2
			      (move r x)
			      (inst ,op r y)))
		(define-vop (,(symbolicate 'fast- translate '-c/fixnum=>fixnum)
			     fast-fixnum-binop-c)
		  (:translate ,translate)
		  (:generator 1
		  (move r x)
		  (inst ,op r (fixnumize y))))
		(define-vop (,(symbolicate "FAST-" translate "/SIGNED=>SIGNED")
			     fast-signed-binop)
		  (:translate ,translate)
		  (:generator ,(1+ untagged-penalty)
		  (move r x)
		  (inst ,op r y)))
		(define-vop (,(symbolicate 'fast- translate '-c/signed=>signed)
			     fast-signed-binop-c)
		  (:translate ,translate)
		  (:generator ,untagged-penalty
		  (move r x)
		  (inst ,op r y)))
		(define-vop (,(symbolicate "FAST-"
					   translate
					   "/UNSIGNED=>UNSIGNED")
		fast-unsigned-binop)
		  (:translate ,translate)
		  (:generator ,(1+ untagged-penalty)
		  (move r x)
		  (inst ,op r y)))
		(define-vop (,(symbolicate 'fast-
					   translate
					   '-c/unsigned=>unsigned)
			     fast-unsigned-binop-c)
		  (:translate ,translate)
		  (:generator ,untagged-penalty
		  (move r x)
		  (inst ,op r y))))))

  ;;(define-binop + 4 add)
  (define-binop - 4 sub)
  (define-binop logand 2 and)
  (define-binop logior 2 or)
  (define-binop logxor 2 xor))

;;; Special handling of add on the x86; can use lea to avoid a
;;; register load, otherwise it uses add.
(define-vop (fast-+/fixnum=>fixnum fast-safe-arith-op)
  (:translate +)
  (:args (x :scs (any-reg) :target r
	    :load-if (not (and (sc-is x control-stack)
			       (sc-is y any-reg)
			       (sc-is r control-stack)
			       (location= x r))))
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg) :from (:argument 0)
	       :load-if (not (and (sc-is x control-stack)
				  (sc-is y any-reg)
				  (sc-is r control-stack)
				  (location= x r)))))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:generator 2
    (cond ((and (sc-is x any-reg) (sc-is y any-reg) (sc-is r any-reg)
		(not (location= x r)))
	   (inst lea r (make-ea :qword :base x :index y :scale 1)))
	  (t
	   (move r x)
	   (inst add r y)))))

(define-vop (fast-+-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate +)
  (:args (x :target r :scs (any-reg control-stack)))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 29)))
  (:results (r :scs (any-reg)
	       :load-if (not (location= x r))))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:generator 1
    (cond ((and (sc-is x any-reg) (sc-is r any-reg) (not (location= x r)))
	   (inst lea r (make-ea :qword :base x :disp (fixnumize y))))
	  (t
	   (move r x)
	   (inst add r (fixnumize y))))))

(define-vop (fast-+/signed=>signed fast-safe-arith-op)
  (:translate +)
  (:args (x :scs (signed-reg) :target r
	    :load-if (not (and (sc-is x signed-stack)
			       (sc-is y signed-reg)
			       (sc-is r signed-stack)
			       (location= x r))))
	 (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 0)
	       :load-if (not (and (sc-is x signed-stack)
				  (sc-is y signed-reg)
				  (location= x r)))))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:generator 5
    (cond ((and (sc-is x signed-reg) (sc-is y signed-reg) (sc-is r signed-reg)
		(not (location= x r)))
	   (inst lea r (make-ea :qword :base x :index y :scale 1)))
	  (t
	   (move r x)
	   (inst add r y)))))


;;;; Special logand cases: (logand signed unsigned) => unsigned

(define-vop (fast-logand/signed-unsigned=>unsigned
	     fast-logand/unsigned=>unsigned)
  (:args (x :target r :scs (signed-reg)
	    :load-if (not (and (sc-is x signed-stack)
			       (sc-is y unsigned-reg)
			       (sc-is r unsigned-stack)
			       (location= x r))))
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types signed-num unsigned-num))

(define-vop (fast-logand-c/signed-unsigned=>unsigned
	     fast-logand-c/unsigned=>unsigned)
  (:args (x :target r :scs (signed-reg signed-stack)))
  (:arg-types signed-num (:constant (unsigned-byte 32))))

(define-vop (fast-logand/unsigned-signed=>unsigned
	     fast-logand/unsigned=>unsigned)
  (:args (x :target r :scs (unsigned-reg)
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is y signed-reg)
			       (sc-is r unsigned-stack)
			       (location= x r))))
	 (y :scs (signed-reg signed-stack)))
  (:arg-types unsigned-num signed-num))


(define-vop (fast-+-c/signed=>signed fast-safe-arith-op)
  (:translate +)
  (:args (x :target r :scs (signed-reg signed-stack)))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)
	       :load-if (not (location= x r))))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:generator 4
    (cond ((and (sc-is x signed-reg) (sc-is r signed-reg)
		(not (location= x r)))
	   (inst lea r (make-ea :qword :base x :disp y)))
	  (t
	   (move r x)
	   (if (= y 1)
	       (inst inc r)
	     (inst add r y))))))

(define-vop (fast-+/unsigned=>unsigned fast-safe-arith-op)
  (:translate +)
  (:args (x :scs (unsigned-reg) :target r
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is y unsigned-reg)
			       (sc-is r unsigned-stack)
			       (location= x r))))
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:results (r :scs (unsigned-reg) :from (:argument 0)
	       :load-if (not (and (sc-is x unsigned-stack)
				  (sc-is y unsigned-reg)
				  (sc-is r unsigned-stack)
				  (location= x r)))))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:generator 5
    (cond ((and (sc-is x unsigned-reg) (sc-is y unsigned-reg)
		(sc-is r unsigned-reg) (not (location= x r)))
	   (inst lea r (make-ea :qword :base x :index y :scale 1)))
	  (t
	   (move r x)
	   (inst add r y)))))

(define-vop (fast-+-c/unsigned=>unsigned fast-safe-arith-op)
  (:translate +)
  (:args (x :target r :scs (unsigned-reg unsigned-stack)))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 32)))
  (:results (r :scs (unsigned-reg)
	       :load-if (not (location= x r))))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:generator 4
    (cond ((and (sc-is x unsigned-reg) (sc-is r unsigned-reg)
		(not (location= x r)))
	   (inst lea r (make-ea :qword :base x :disp y)))
	  (t
	   (move r x)
	   (if (= y 1)
	       (inst inc r)
	     (inst add r y))))))

;;;; multiplication and division

(define-vop (fast-*/fixnum=>fixnum fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (any-reg) :target r)
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:results (r :scs (any-reg) :from (:argument 0)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:generator 4
    (move r x)
    (inst sar r 3)
    (inst imul r y)))

(define-vop (fast-*-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (any-reg control-stack)))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 29))) 
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:generator 3
    (inst imul r x y)))

(define-vop (fast-*/signed=>signed fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (signed-reg) :target r)
	 (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:results (r :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:generator 5
    (move r x)
    (inst imul r y)))

(define-vop (fast-*-c/signed=>signed fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (signed-reg signed-stack)))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:generator 4
    (inst imul r x y)))

(define-vop (fast-*/unsigned=>unsigned fast-safe-arith-op)
  (:translate *)
  (:args (x :scs (unsigned-reg) :target eax)
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :target result
		   :from (:argument 0) :to :result) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset
		   :from :eval :to :result) edx)
  (:ignore edx)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 6
    (move eax x)
    (inst mul eax y)
    (move result eax)))


(define-vop (fast-truncate/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg) :target eax)
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:temporary (:sc signed-reg :offset eax-offset :target quo
		   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target rem
		   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (any-reg))
	    (rem :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:note "inline fixnum arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 31
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (if (sc-is y any-reg)
	  (inst test y y)  ; smaller instruction
	  (inst cmp y 0))
      (inst jmp :eq zero))
    (move eax x)
    (inst cqo)
    (inst idiv eax y)
    (if (location= quo eax)
	(inst shl eax 3)
	(inst lea quo (make-ea :qword :index eax :scale 8)))
    (move rem edx)))

(define-vop (fast-truncate-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg) :target eax))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 29)))
  (:temporary (:sc signed-reg :offset eax-offset :target quo
		   :from :argument :to (:result 0)) eax)
  (:temporary (:sc any-reg :offset edx-offset :target rem
		   :from :eval :to (:result 1)) edx)
  (:temporary (:sc any-reg :from :eval :to :result) y-arg)
  (:results (quo :scs (any-reg))
	    (rem :scs (any-reg)))
  (:result-types tagged-num tagged-num)
  (:note "inline fixnum arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 30
    (move eax x)
    (inst cqo)
    (inst mov y-arg (fixnumize y))
    (inst idiv eax y-arg)
    (if (location= quo eax)
	(inst shl eax 3)
	(inst lea quo (make-ea :qword :index eax :scale 8)))
    (move rem edx)))

(define-vop (fast-truncate/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :target eax)
	 (y :scs (unsigned-reg signed-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :target quo
		   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target rem
		   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (if (sc-is y unsigned-reg)
	  (inst test y y)  ; smaller instruction
	  (inst cmp y 0))
      (inst jmp :eq zero))
    (move eax x)
    (inst xor edx edx)
    (inst div eax y)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate-c/unsigned=>unsigned fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (unsigned-reg) :target eax))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 32)))
  (:temporary (:sc unsigned-reg :offset eax-offset :target quo
		   :from :argument :to (:result 0)) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :target rem
		   :from :eval :to (:result 1)) edx)
  (:temporary (:sc unsigned-reg :from :eval :to :result) y-arg)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 32
    (move eax x)
    (inst xor edx edx)
    (inst mov y-arg y)
    (inst div eax y-arg)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :target eax)
	 (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:temporary (:sc signed-reg :offset eax-offset :target quo
		   :from (:argument 0) :to (:result 0)) eax)
  (:temporary (:sc signed-reg :offset edx-offset :target rem
		   :from (:argument 0) :to (:result 1)) edx)
  (:results (quo :scs (signed-reg))
	    (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 33
    (let ((zero (generate-error-code vop division-by-zero-error x y)))
      (if (sc-is y signed-reg)
	  (inst test y y)  ; smaller instruction
	  (inst cmp y 0))
      (inst jmp :eq zero))
    (move eax x)
    (inst cqo)
    (inst idiv eax y)
    (move quo eax)
    (move rem edx)))

(define-vop (fast-truncate-c/signed=>signed fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (signed-reg) :target eax))
  (:info y)
  (:arg-types signed-num (:constant (signed-byte 32)))
  (:temporary (:sc signed-reg :offset eax-offset :target quo
		   :from :argument :to (:result 0)) eax)
  (:temporary (:sc signed-reg :offset edx-offset :target rem
		   :from :eval :to (:result 1)) edx)
  (:temporary (:sc signed-reg :from :eval :to :result) y-arg)
  (:results (quo :scs (signed-reg))
	    (rem :scs (signed-reg)))
  (:result-types signed-num signed-num)
  (:note "inline (signed-byte 32) arithmetic")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 32
    (move eax x)
    (inst cqo)
    (inst mov y-arg y)
    (inst idiv eax y-arg)
    (move quo eax)
    (move rem edx)))



;;;; Shifting
(define-vop (fast-ash-c/fixnum=>fixnum)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (any-reg) :target result
		 :load-if (not (and (sc-is number any-reg control-stack)
				    (sc-is result any-reg control-stack)
				    (location= number result)))))
  (:info amount)
  (:arg-types tagged-num (:constant integer))
  (:results (result :scs (any-reg)
		    :load-if (not (and (sc-is number control-stack)
				       (sc-is result control-stack)
				       (location= number result)))))
  (:result-types tagged-num)
  (:note "inline ASH")
  (:generator 2
    (cond ((and (= amount 1) (not (location= number result)))
	   (inst lea result (make-ea :qword :index number :scale 2)))
	  ((and (= amount 2) (not (location= number result)))
	   (inst lea result (make-ea :qword :index number :scale 4)))
	  ((and (= amount 3) (not (location= number result)))
	   (inst lea result (make-ea :qword :index number :scale 8)))
	  (t
	   (move result number)
           (cond ((plusp amount)
                  ;; We don't have to worry about overflow because of the
                  ;; result type restriction.
                  (inst shl result amount))
                 (t                                    
                  ;; Since the shift instructions take the shift amount
                  ;; modulo 64 we must special case amounts of 64 and more.
                  ;; Because fixnums have only 61 bits, the result is 0 or
                  ;; -1 for all amounts of 60 or more, so use this as the
                  ;; limit instead.
                  (inst sar result (min (- n-word-bits n-fixnum-tag-bits 1)
                                        (- amount)))
                  (inst and result (lognot fixnum-tag-mask))))))))

(define-vop (fast-ash-left/fixnum=>fixnum)
  (:translate ash)
  (:args (number :scs (any-reg) :target result
		 :load-if (not (and (sc-is number control-stack)
				    (sc-is result control-stack)
				    (location= number result))))
	 (amount :scs (unsigned-reg) :target ecx))
  (:arg-types tagged-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (any-reg) :from (:argument 0)
		    :load-if (not (and (sc-is number control-stack)
				       (sc-is result control-stack)
				       (location= number result)))))
  (:result-types tagged-num)
  (:policy :fast-safe)
  (:note "inline ASH")
  (:generator 3
    (move result number)
    (move ecx amount)
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)))

(define-vop (fast-ash-c/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result
		 :load-if (not (and (sc-is number signed-stack)
				    (sc-is result signed-stack)
				    (location= number result)))))
  (:info amount)
  (:arg-types signed-num (:constant integer))
  (:results (result :scs (signed-reg)
		    :load-if (not (and (sc-is number signed-stack)
				       (sc-is result signed-stack)
				       (location= number result)))))
  (:result-types signed-num)
  (:note "inline ASH")
  (:generator 3
    (cond ((and (= amount 1) (not (location= number result)))
	   (inst lea result (make-ea :qword :index number :scale 2)))
	  ((and (= amount 2) (not (location= number result)))
	   (inst lea result (make-ea :qword :index number :scale 4)))
	  ((and (= amount 3) (not (location= number result)))
	   (inst lea result (make-ea :qword :index number :scale 8)))
	  (t
	   (move result number)
	   (cond ((plusp amount) (inst shl result amount))
		 (t (inst sar result (min 63 (- amount)))))))))

(define-vop (fast-ash-c/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result
		 :load-if (not (and (sc-is number unsigned-stack)
				    (sc-is result unsigned-stack)
				    (location= number result)))))
  (:info amount)
  (:arg-types unsigned-num (:constant integer))
  (:results (result :scs (unsigned-reg)
		    :load-if (not (and (sc-is number unsigned-stack)
				       (sc-is result unsigned-stack)
				       (location= number result)))))
  (:result-types unsigned-num)
  (:note "inline ASH")
  (:generator 3
    (cond ((and (= amount 1) (not (location= number result)))
	   (inst lea result (make-ea :qword :index number :scale 2)))
	  ((and (= amount 2) (not (location= number result)))
	   (inst lea result (make-ea :qword :index number :scale 4)))
	  ((and (= amount 3) (not (location= number result)))
	   (inst lea result (make-ea :qword :index number :scale 8)))
	  (t
	   (move result number)
	   (cond ((< -64 amount 64) ;; XXXX
                  ;; this code is used both in ASH and ASH-MOD32, so
                  ;; be careful
                  (if (plusp amount)
                      (inst shl result amount)
                      (inst shr result (- amount))))
		 (t (if (sc-is result unsigned-reg)
                        (inst xor result result)
                        (inst mov result 0))))))))

(define-vop (fast-ash-left/signed=>signed)
  (:translate ash)
  (:args (number :scs (signed-reg) :target result
		 :load-if (not (and (sc-is number signed-stack)
				    (sc-is result signed-stack)
				    (location= number result))))
	 (amount :scs (unsigned-reg) :target ecx))
  (:arg-types signed-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (signed-reg) :from (:argument 0)
		    :load-if (not (and (sc-is number signed-stack)
				       (sc-is result signed-stack)
				       (location= number result)))))
  (:result-types signed-num)
  (:policy :fast-safe)
  (:note "inline ASH")
  (:generator 4
    (move result number)
    (move ecx amount)
    (inst shl result :cl)))

(define-vop (fast-ash-left/unsigned=>unsigned)
  (:translate ash)
  (:args (number :scs (unsigned-reg) :target result
		 :load-if (not (and (sc-is number unsigned-stack)
				    (sc-is result unsigned-stack)
				    (location= number result))))
	 (amount :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (unsigned-reg) :from (:argument 0)
		    :load-if (not (and (sc-is number unsigned-stack)
				       (sc-is result unsigned-stack)
				       (location= number result)))))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:note "inline ASH")
  (:generator 4
    (move result number)
    (move ecx amount)
    (inst shl result :cl)))

(define-vop (fast-ash/signed=>signed)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg) :target result)
	 (amount :scs (signed-reg) :target ecx))
  (:arg-types signed-num signed-num)
  (:results (result :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:note "inline ASH")
  (:generator 5
    (move result number)
    (move ecx amount)
    (inst or ecx ecx)
    (inst jmp :ns positive)
    (inst neg ecx)
    (inst cmp ecx 63)
    (inst jmp :be okay)
    (inst mov ecx 63)
    OKAY
    (inst sar result :cl)
    (inst jmp done)

    POSITIVE
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)

    DONE))

(define-vop (fast-ash/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
	 (amount :scs (signed-reg) :target ecx))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:note "inline ASH")
  (:generator 5
    (move result number)
    (move ecx amount)
    (inst or ecx ecx)
    (inst jmp :ns positive)
    (inst neg ecx)
    (inst cmp ecx 63)
    (inst jmp :be okay)
    (inst xor result result)
    (inst jmp done)
    OKAY
    (inst shr result :cl)
    (inst jmp done)

    POSITIVE
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)

    DONE))

(in-package "SB!C")

(defknown %lea (integer integer (member 1 2 4 8 16) (signed-byte 64))
  integer
  (foldable flushable movable))

(defoptimizer (%lea derive-type) ((base index scale disp))
  (when (and (constant-lvar-p scale)
	     (constant-lvar-p disp))
    (let ((scale (lvar-value scale))
	  (disp (lvar-value disp))
	  (base-type (lvar-type base))
	  (index-type (lvar-type index)))
      (when (and (numeric-type-p base-type)
		 (numeric-type-p index-type))
	(let ((base-lo (numeric-type-low base-type))
	      (base-hi (numeric-type-high base-type))
	      (index-lo (numeric-type-low index-type))
	      (index-hi (numeric-type-high index-type)))
	  (make-numeric-type :class 'integer
			     :complexp :real
			     :low (when (and base-lo index-lo)
				    (+ base-lo (* index-lo scale) disp))
			     :high (when (and base-hi index-hi)
				     (+ base-hi (* index-hi scale) disp))))))))

(defun %lea (base index scale disp)
  (+ base (* index scale) disp))

(in-package "SB!VM")

(define-vop (%lea/unsigned=>unsigned)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (unsigned-reg))
	 (index :scs (unsigned-reg)))
  (:info scale disp)
  (:arg-types unsigned-num unsigned-num
	      (:constant (member 1 2 4 8))
	      (:constant (signed-byte 64)))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst lea r (make-ea :qword :base base :index index
			 :scale scale :disp disp))))

(define-vop (%lea/signed=>signed)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (signed-reg))
	 (index :scs (signed-reg)))
  (:info scale disp)
  (:arg-types signed-num signed-num
	      (:constant (member 1 2 4 8))
	      (:constant (signed-byte 64)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 4
    (inst lea r (make-ea :qword :base base :index index
			 :scale scale :disp disp))))

(define-vop (%lea/fixnum=>fixnum)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (any-reg))
	 (index :scs (any-reg)))
  (:info scale disp)
  (:arg-types tagged-num tagged-num
	      (:constant (member 1 2 4 8))
	      (:constant (signed-byte 64)))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:generator 3
    (inst lea r (make-ea :qword :base base :index index
			 :scale scale :disp disp))))

;;; FIXME: before making knowledge of this too public, it needs to be
;;; fixed so that it's actually _faster_ than the non-CMOV version; at
;;; least on my Celeron-XXX laptop, this version is marginally slower
;;; than the above version with branches.  -- CSR, 2003-09-04
(define-vop (fast-cmov-ash/unsigned=>unsigned)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (unsigned-reg) :target result)
	 (amount :scs (signed-reg) :target ecx))
  (:arg-types unsigned-num signed-num)
  (:results (result :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:temporary (:sc any-reg :from (:eval 0) :to (:eval 1)) zero)
  (:note "inline ASH")
  (:guard (member :cmov *backend-subfeatures*))
  (:generator 4
    (move result number)
    (move ecx amount)
    (inst or ecx ecx)
    (inst jmp :ns positive)
    (inst neg ecx)
    (inst xor zero zero)
    (inst shr result :cl)
    (inst cmp ecx 63)
    (inst cmov :nbe result zero)
    (inst jmp done)
    
    POSITIVE
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)

    DONE))

;;; Note: documentation for this function is wrong - rtfm
(define-vop (signed-byte-64-len)
  (:translate integer-length)
  (:note "inline (signed-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (signed-reg) :target res))
  (:arg-types signed-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 28
    (move res arg)
    (inst cmp res 0)
    (inst jmp :ge POS)
    (inst not res)
    POS
    (inst bsr res res)
    (inst jmp :z zero)
    (inst inc res)
    (inst jmp done)
    ZERO
    (inst xor res res)
    DONE))

(define-vop (unsigned-byte-64-len)
  (:translate integer-length)
  (:note "inline (unsigned-byte 32) integer-length")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 26
    (inst bsr res arg)
    (inst jmp :z zero)
    (inst inc res)
    (inst jmp done)
    ZERO
    (inst xor res res)
    DONE))


(define-vop (unsigned-byte-64-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 64) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:sc unsigned-reg :from (:argument 0)) temp)
  (:temporary (:sc unsigned-reg :from (:argument 0)) t1)
  (:generator 60
    (move result arg)
    (move t1 arg)

    (inst mov temp result)  
    (inst shr temp 1)
    (inst and result #x55555555) 	; note these masks will restrict the 
    (inst and temp #x55555555)		; count to the lower half of arg
    (inst add result temp)

    (inst mov temp result)
    (inst shr temp 2)
    (inst and result #x33333333)
    (inst and temp #x33333333)
    (inst add result temp)

    (inst mov temp result)
    (inst shr temp 4)
    (inst and result #x0f0f0f0f)
    (inst and temp #x0f0f0f0f)
    (inst add result temp)

    (inst mov temp result)
    (inst shr temp 8)
    (inst and result #x00ff00ff)
    (inst and temp #x00ff00ff)
    (inst add result temp)

    (inst mov temp result)
    (inst shr temp 16)
    (inst and result #x0000ffff)
    (inst and temp #x0000ffff)
    (inst add result temp)

    ;;; now do the upper half
    (inst shr t1 32)

    (inst mov temp t1)  
    (inst shr temp 1)
    (inst and t1 #x55555555) 
    (inst and temp #x55555555)
    (inst add t1 temp)

    (inst mov temp t1)
    (inst shr temp 2)
    (inst and t1 #x33333333)
    (inst and temp #x33333333)
    (inst add t1 temp)

    (inst mov temp t1)
    (inst shr temp 4)
    (inst and t1 #x0f0f0f0f)
    (inst and temp #x0f0f0f0f)
    (inst add t1 temp)

    (inst mov temp t1)
    (inst shr temp 8)
    (inst and t1 #x00ff00ff)
    (inst and temp #x00ff00ff)
    (inst add t1 temp)

    (inst mov temp t1)
    (inst shr temp 16)
    (inst and t1 #x0000ffff)
    (inst and temp #x0000ffff)
    (inst add t1 temp)
    (inst add result t1)))



;;;; binary conditional VOPs

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

;;; constant variants are declared for 32 bits not 64 bits, because
;;; loading a 64 bit constant is silly

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg)
	    :load-if (not (and (sc-is x control-stack)
			       (sc-is y any-reg))))
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg control-stack)))
  (:arg-types tagged-num (:constant (signed-byte 29)))
  (:info target not-p y))

(define-vop (fast-conditional/signed fast-conditional)
  (:args (x :scs (signed-reg)
	    :load-if (not (and (sc-is x signed-stack)
			       (sc-is y signed-reg))))
	 (y :scs (signed-reg signed-stack)))
  (:arg-types signed-num signed-num)
  (:note "inline (signed-byte 32) comparison"))

(define-vop (fast-conditional-c/signed fast-conditional/signed)
  (:args (x :scs (signed-reg signed-stack)))
  (:arg-types signed-num (:constant (signed-byte 31)))
  (:info target not-p y))

(define-vop (fast-conditional/unsigned fast-conditional)
  (:args (x :scs (unsigned-reg)
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is y unsigned-reg))))
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:note "inline (unsigned-byte 32) comparison"))

(define-vop (fast-conditional-c/unsigned fast-conditional/unsigned)
  (:args (x :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num (:constant (unsigned-byte 31)))
  (:info target not-p y))

(macrolet ((define-conditional-vop (tran cond unsigned not-cond not-unsigned)
	     `(progn
		,@(mapcar
		   (lambda (suffix cost signed)
		     `(define-vop (;; FIXME: These could be done more
				   ;; cleanly with SYMBOLICATE.
				   ,(intern (format nil "~:@(FAST-IF-~A~A~)"
						    tran suffix))
				   ,(intern
				     (format nil "~:@(FAST-CONDITIONAL~A~)"
					     suffix)))
			(:translate ,tran)
			(:generator ,cost
				    (inst cmp x
					  ,(if (eq suffix '-c/fixnum)
					       '(fixnumize y)
					       'y))
				    (inst jmp (if not-p
						  ,(if signed
						       not-cond
						       not-unsigned)
						  ,(if signed
						       cond
						       unsigned))
					  target))))
		   '(/fixnum -c/fixnum /signed -c/signed /unsigned -c/unsigned)
;		   '(/fixnum  /signed  /unsigned)
		   '(4 3 6 5 6 5)
		   '(t t t t nil nil)))))

  (define-conditional-vop < :l :b :ge :ae)
  (define-conditional-vop > :g :a :le :be))

(define-vop (fast-if-eql/signed fast-conditional/signed)
  (:translate eql)
  (:generator 6
    (inst cmp x y)
    (inst jmp (if not-p :ne :e) target)))

(define-vop (fast-if-eql-c/signed fast-conditional-c/signed)
  (:translate eql)
  (:generator 5
    (cond ((and (sc-is x signed-reg) (zerop y))
	   (inst test x x))  ; smaller instruction
	  (t
	   (inst cmp x y)))
    (inst jmp (if not-p :ne :e) target)))

(define-vop (fast-if-eql/unsigned fast-conditional/unsigned)
  (:translate eql)
  (:generator 6
    (inst cmp x y)
    (inst jmp (if not-p :ne :e) target)))

(define-vop (fast-if-eql-c/unsigned fast-conditional-c/unsigned)
  (:translate eql)
  (:generator 5
    (cond ((and (sc-is x unsigned-reg) (zerop y))
	   (inst test x x))  ; smaller instruction
	  (t
	   (inst cmp x y)))
    (inst jmp (if not-p :ne :e) target)))

;;; EQL/FIXNUM is funny because the first arg can be of any type, not just a
;;; known fixnum.

;;; These versions specify a fixnum restriction on their first arg. We have
;;; also generic-eql/fixnum VOPs which are the same, but have no restriction on
;;; the first arg and a higher cost. The reason for doing this is to prevent
;;; fixnum specific operations from being used on word integers, spuriously
;;; consing the argument.

(define-vop (fast-eql/fixnum fast-conditional)
  (:args (x :scs (any-reg)
	    :load-if (not (and (sc-is x control-stack)
			       (sc-is y any-reg))))
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison")
  (:translate eql)
  (:generator 4
    (inst cmp x y)
    (inst jmp (if not-p :ne :e) target)))
(define-vop (generic-eql/fixnum fast-eql/fixnum)
  (:args (x :scs (any-reg descriptor-reg)
	    :load-if (not (and (sc-is x control-stack)
			       (sc-is y any-reg))))
	 (y :scs (any-reg control-stack)))
  (:arg-types * tagged-num)
  (:variant-cost 7))


(define-vop (fast-eql-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg control-stack)))
  (:arg-types tagged-num (:constant (signed-byte 29)))
  (:info target not-p y)
  (:translate eql)
  (:generator 2
    (cond ((and (sc-is x any-reg) (zerop y))
	   (inst test x x))  ; smaller instruction
	  (t
	   (inst cmp x (fixnumize y))))
    (inst jmp (if not-p :ne :e) target)))

(define-vop (generic-eql-c/fixnum fast-eql-c/fixnum)
  (:args (x :scs (any-reg descriptor-reg control-stack)))
  (:arg-types * (:constant (signed-byte 29)))
  (:variant-cost 6))

;;;; 32-bit logical operations

(define-vop (merge-bits)
  (:translate merge-bits)
  (:args (shift :scs (signed-reg unsigned-reg) :target ecx)
	 (prev :scs (unsigned-reg) :target result)
	 (next :scs (unsigned-reg)))
  (:arg-types tagged-num unsigned-num unsigned-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 0)) ecx)
  (:results (result :scs (unsigned-reg) :from (:argument 1)))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 4
    (move ecx shift)
    (move result prev)
    (inst shrd result next :cl)))

;;; Only the lower 6 bits of the shift amount are significant.
(define-vop (shift-towards-someplace)
  (:policy :fast-safe)
  (:args (num :scs (unsigned-reg) :target r)
	 (amount :scs (signed-reg) :target ecx))
  (:arg-types unsigned-num tagged-num)
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (r :scs (unsigned-reg) :from (:argument 0)))
  (:result-types unsigned-num))

(define-vop (shift-towards-start shift-towards-someplace)
  (:translate shift-towards-start)
  (:note "SHIFT-TOWARDS-START")
  (:generator 1
    (move r num)
    (move ecx amount)
    (inst shr r :cl)))

(define-vop (shift-towards-end shift-towards-someplace)
  (:translate shift-towards-end)
  (:note "SHIFT-TOWARDS-END")
  (:generator 1
    (move r num)
    (move ecx amount)
    (inst shl r :cl)))

;;;; Modular functions

(macrolet ((def (name -c-p)
             (let ((fun64 (intern (format nil "~S-MOD64" name)))
                   (vopu (intern (format nil "FAST-~S/UNSIGNED=>UNSIGNED" name)))
                   (vopcu (intern (format nil "FAST-~S-C/UNSIGNED=>UNSIGNED" name)))
                   (vopf (intern (format nil "FAST-~S/FIXNUM=>FIXNUM" name)))
                   (vopcf (intern (format nil "FAST-~S-C/FIXNUM=>FIXNUM" name)))
                   (vop64u (intern (format nil "FAST-~S-MOD64/UNSIGNED=>UNSIGNED" name)))
                   (vop64f (intern (format nil "FAST-~S-MOD64/FIXNUM=>FIXNUM" name)))
                   (vop64cu (intern (format nil "FAST-~S-MOD64-C/UNSIGNED=>UNSIGNED" name)))
                   (vop64cf (intern (format nil "FAST-~S-MOD64-C/FIXNUM=>FIXNUM" name)))
                   (sfun61 (intern (format nil "~S-SMOD61" name)))
                   (svop61f (intern (format nil "FAST-~S-SMOD61/FIXNUM=>FIXNUM" name)))
                   (svop61cf (intern (format nil "FAST-~S-SMOD61-C/FIXNUM=>FIXNUM" name))))
               `(progn
                  (define-modular-fun ,fun64 (x y) ,name :unsigned 64)
                  (define-modular-fun ,sfun61 (x y) ,name :signed 61)
                  (define-vop (,vop64u ,vopu) (:translate ,fun64))
                  (define-vop (,vop64f ,vopf) (:translate ,fun64))
                  (define-vop (,svop61f ,vopf) (:translate ,sfun61))
                  ,@(when -c-p
                      `((define-vop (,vop64cu ,vopcu) (:translate ,fun64))
                        (define-vop (,svop61cf ,vopcf) (:translate ,sfun61))))))))
  (def + t)
  (def - t)
  ;; (no -C variant as x86 MUL instruction doesn't take an immediate)
  (def * nil))

;;; (no -C variant as x86 MUL instruction doesn't take an immediate)

(define-vop (fast-ash-left-mod64-c/unsigned=>unsigned
             fast-ash-c/unsigned=>unsigned)
  (:translate ash-left-mod64))
(define-vop (fast-ash-left-mod64/unsigned=>unsigned
             fast-ash-left/unsigned=>unsigned))
(deftransform ash-left-mod64 ((integer count)
			      ((unsigned-byte 64) (unsigned-byte 6)))
  (when (sb!c::constant-lvar-p count)
    (sb!c::give-up-ir1-transform))
  '(%primitive fast-ash-left-mod64/unsigned=>unsigned integer count))

(define-vop (fast-ash-left-smod61-c/fixnum=>fixnum
             fast-ash-c/fixnum=>fixnum)
  (:translate ash-left-smod61))
(define-vop (fast-ash-left-smod61/fixnum=>fixnum
             fast-ash-left/fixnum=>fixnum))
(deftransform ash-left-smod61 ((integer count)
                               ((signed-byte 61) (unsigned-byte 6)))
  (when (sb!c::constant-lvar-p count)
    (sb!c::give-up-ir1-transform))
  '(%primitive fast-ash-left-smod61/fixnum=>fixnum integer count))

(in-package "SB!C")

(defknown sb!vm::%lea-mod64 (integer integer (member 1 2 4 8) (signed-byte 64))
  (unsigned-byte 64)
  (foldable flushable movable))
(defknown sb!vm::%lea-smod61 (integer integer (member 1 2 4 8) (signed-byte 64))
  (signed-byte 61)
  (foldable flushable movable))

(define-modular-fun-optimizer %lea ((base index scale disp) :unsigned :width width)
  (when (and (<= width 64)
	     (constant-lvar-p scale)
	     (constant-lvar-p disp))
    (cut-to-width base :unsigned width)
    (cut-to-width index :unsigned width)
    'sb!vm::%lea-mod64))
(define-modular-fun-optimizer %lea ((base index scale disp) :signed :width width)
  (when (and (<= width 61)
	     (constant-lvar-p scale)
	     (constant-lvar-p disp))
    (cut-to-width base :signed width)
    (cut-to-width index :signed width)
    'sb!vm::%lea-smod61))

#+sb-xc-host
(progn
  (defun sb!vm::%lea-mod64 (base index scale disp)
    (ldb (byte 64 0) (%lea base index scale disp)))
  (defun sb!vm::%lea-smod61 (base index scale disp)
    (mask-signed-field 61 (%lea base index scale disp))))
#-sb-xc-host
(progn
  (defun sb!vm::%lea-mod64 (base index scale disp)
    (let ((base (logand base #xffffffffffffffff))
	  (index (logand index #xffffffffffffffff)))
      ;; can't use modular version of %LEA, as we only have VOPs for
      ;; constant SCALE and DISP.
      (ldb (byte 64 0) (+ base (* index scale) disp))))
  (defun sb!vm::%lea-smod61 (base index scale disp)
    (let ((base (mask-signed-field 61 base))
          (index (mask-signed-field 61 index)))
      ;; can't use modular version of %LEA, as we only have VOPs for
      ;; constant SCALE and DISP.
      (mask-signed-field 61 (+ base (* index scale) disp)))))

(in-package "SB!VM")

(define-vop (%lea-mod64/unsigned=>unsigned
	     %lea/unsigned=>unsigned)
  (:translate %lea-mod64))
(define-vop (%lea-smod61/fixnum=>fixnum
	     %lea/fixnum=>fixnum)
  (:translate %lea-smod61))

;;; logical operations
(define-modular-fun lognot-mod64 (x) lognot :unsigned 64)
(define-vop (lognot-mod64/unsigned=>unsigned)
  (:translate lognot-mod64)
  (:args (x :scs (unsigned-reg unsigned-stack) :target r
	    :load-if (not (and (sc-is x unsigned-stack)
			       (sc-is r unsigned-stack)
			       (location= x r)))))
  (:arg-types unsigned-num)
  (:results (r :scs (unsigned-reg)
	       :load-if (not (and (sc-is x unsigned-stack)
				  (sc-is r unsigned-stack)
				  (location= x r)))))
  (:result-types unsigned-num)
  (:policy :fast-safe)
  (:generator 1
    (move r x)
    (inst not r)))

(define-modular-fun logxor-mod64 (x y) logxor :unsigned 64)
(define-vop (fast-logxor-mod64/unsigned=>unsigned
             fast-logxor/unsigned=>unsigned)
  (:translate logxor-mod64))
(define-vop (fast-logxor-mod64-c/unsigned=>unsigned
             fast-logxor-c/unsigned=>unsigned)
  (:translate logxor-mod64))
(define-vop (fast-logxor-mod64/fixnum=>fixnum
             fast-logxor/fixnum=>fixnum)
  (:translate logxor-mod64))
(define-vop (fast-logxor-mod64-c/fixnum=>fixnum
             fast-logxor-c/fixnum=>fixnum)
  (:translate logxor-mod64))

(define-source-transform logeqv (&rest args)
  (if (oddp (length args))
      `(logxor ,@args)
      `(lognot (logxor ,@args))))
(define-source-transform logandc1 (x y)
  `(logand (lognot ,x) ,y))
(define-source-transform logandc2 (x y)
  `(logand ,x (lognot ,y)))
(define-source-transform logorc1 (x y)
  `(logior (lognot ,x) ,y))
(define-source-transform logorc2 (x y)
  `(logior ,x (lognot ,y)))
(define-source-transform lognor (x y)
  `(lognot (logior ,x ,y)))
(define-source-transform lognand (x y)
  `(lognot (logand ,x ,y)))

;;;; bignum stuff

(define-vop (bignum-length get-header-data)
  (:translate sb!bignum:%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb!bignum:%bignum-set-length)
  (:policy :fast-safe))

(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb!bignum:%bignum-ref)

(define-full-setter bignum-set * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb!bignum:%bignum-set)

(define-vop (digit-0-or-plus)
  (:translate sb!bignum:%digit-0-or-plusp)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:conditional)
  (:info target not-p)
  (:generator 3
    (inst or digit digit)
    (inst jmp (if not-p :s :ns) target)))


;;; For add and sub with carry the sc of carry argument is any-reg so
;;; the it may be passed as a fixnum or word and thus may be 0, 1, or
;;; 4. This is easy to deal with and may save a fixnum-word
;;; conversion.
(define-vop (add-w/carry)
  (:translate sb!bignum:%add-with-carry)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :target result)
	 (b :scs (unsigned-reg unsigned-stack) :to :eval)
	 (c :scs (any-reg) :target temp))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:temporary (:sc any-reg :from (:argument 2) :to :eval) temp)
  (:results (result :scs (unsigned-reg) :from (:argument 0))
	    (carry :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 4
    (move result a)
    (move temp c)
    (inst neg temp) ; Set the carry flag to 0 if c=0 else to 1
    (inst adc result b)
    (inst mov carry 0)
    (inst adc carry carry)))

;;; Note: the borrow is the oppostite of the x86 convention - 1 for no
;;; borrow and 0 for a borrow.
(define-vop (sub-w/borrow)
  (:translate sb!bignum:%subtract-with-borrow)
  (:policy :fast-safe)
  (:args (a :scs (unsigned-reg) :to :eval :target result)
	 (b :scs (unsigned-reg unsigned-stack) :to :result)
	 (c :scs (any-reg control-stack)))
  (:arg-types unsigned-num unsigned-num positive-fixnum)
  (:results (result :scs (unsigned-reg) :from :eval)
	    (borrow :scs (unsigned-reg)))
  (:result-types unsigned-num positive-fixnum)
  (:generator 5
    (inst cmp c 1) ; Set the carry flag to 1 if c=0 else to 0
    (move result a)
    (inst sbb result b)
    (inst mov borrow 0)
    (inst adc borrow borrow)
    (inst xor borrow 1)))


(define-vop (bignum-mult-and-add-3-arg)
  (:translate sb!bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
	 (y :scs (unsigned-reg unsigned-stack))
	 (carry-in :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
		   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 1)
		   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (inst add eax carry-in)
    (inst adc edx 0)
    (move hi edx)
    (move lo eax)))

(define-vop (bignum-mult-and-add-4-arg)
  (:translate sb!bignum:%multiply-and-add)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
	 (y :scs (unsigned-reg unsigned-stack))
	 (prev :scs (unsigned-reg unsigned-stack))
	 (carry-in :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
		   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 1)
		   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (inst add eax prev)
    (inst adc edx 0)
    (inst add eax carry-in)
    (inst adc edx 0)
    (move hi edx)
    (move lo eax)))


(define-vop (bignum-mult)
  (:translate sb!bignum:%multiply)
  (:policy :fast-safe)
  (:args (x :scs (unsigned-reg) :target eax)
	 (y :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 0)
		   :to (:result 1) :target lo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 1)
		   :to (:result 0) :target hi) edx)
  (:results (hi :scs (unsigned-reg))
	    (lo :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 20
    (move eax x)
    (inst mul eax y)
    (move hi edx)
    (move lo eax)))

(define-vop (bignum-lognot lognot-mod64/unsigned=>unsigned)
  (:translate sb!bignum:%lognot))

(define-vop (fixnum-to-digit)
  (:translate sb!bignum:%fixnum-to-digit)
  (:policy :fast-safe)
  (:args (fixnum :scs (any-reg control-stack) :target digit))
  (:arg-types tagged-num)
  (:results (digit :scs (unsigned-reg)
		   :load-if (not (and (sc-is fixnum control-stack)
				      (sc-is digit unsigned-stack)
				      (location= fixnum digit)))))
  (:result-types unsigned-num)
  (:generator 1
    (move digit fixnum)
    (inst sar digit 3)))

(define-vop (bignum-floor)
  (:translate sb!bignum:%floor)
  (:policy :fast-safe)
  (:args (div-high :scs (unsigned-reg) :target edx)
	 (div-low :scs (unsigned-reg) :target eax)
	 (divisor :scs (unsigned-reg unsigned-stack)))
  (:arg-types unsigned-num unsigned-num unsigned-num)
  (:temporary (:sc unsigned-reg :offset eax-offset :from (:argument 1)
		   :to (:result 0) :target quo) eax)
  (:temporary (:sc unsigned-reg :offset edx-offset :from (:argument 0)
		   :to (:result 1) :target rem) edx)
  (:results (quo :scs (unsigned-reg))
	    (rem :scs (unsigned-reg)))
  (:result-types unsigned-num unsigned-num)
  (:generator 300
    (move edx div-high)
    (move eax div-low)
    (inst div eax divisor)
    (move quo eax)
    (move rem edx)))

(define-vop (signify-digit)
  (:translate sb!bignum:%fixnum-digit-with-correct-sign)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg unsigned-stack) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (any-reg signed-reg)
		 :load-if (not (and (sc-is digit unsigned-stack)
				    (sc-is res control-stack signed-stack)
				    (location= digit res)))))
  (:result-types signed-num)
  (:generator 1
    (move res digit)
    (when (sc-is res any-reg control-stack)
      (inst shl res 3))))

(define-vop (digit-ashr)
  (:translate sb!bignum:%ashr)
  (:policy :fast-safe)
  (:args (digit :scs (unsigned-reg unsigned-stack) :target result)
	 (count :scs (unsigned-reg) :target ecx))
  (:arg-types unsigned-num positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (unsigned-reg) :from (:argument 0)
		    :load-if (not (and (sc-is result unsigned-stack)
				       (location= digit result)))))
  (:result-types unsigned-num)
  (:generator 1
    (move result digit)
    (move ecx count)
    (inst sar result :cl)))

(define-vop (digit-lshr digit-ashr)
  (:translate sb!bignum:%digit-logical-shift-right)
  (:generator 1
    (move result digit)
    (move ecx count)
    (inst shr result :cl)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb!bignum:%ashl)
  (:generator 1
    (move result digit)
    (move ecx count)
    (inst shl result :cl)))

;;;; static functions

(define-static-fun two-arg-/ (x y) :translate /)

(define-static-fun two-arg-gcd (x y) :translate gcd)
(define-static-fun two-arg-lcm (x y) :translate lcm)

(define-static-fun two-arg-and (x y) :translate logand)
(define-static-fun two-arg-ior (x y) :translate logior)
(define-static-fun two-arg-xor (x y) :translate logxor)


(in-package "SB!C")

(defun *-transformer (y)
  (cond
    ((= y (ash 1 (integer-length y)))
     ;; there's a generic transform for y = 2^k
     (give-up-ir1-transform))
    ((member y '(3 5 9))
     ;; we can do these multiplications directly using LEA
     `(%lea x x ,(1- y) 0))
    (t
     ;; A normal 64-bit multiplication takes 4 cycles on Athlon 64/Opteron.
     ;; Optimizing multiplications (other than the above cases) to
     ;; shifts/adds/leas gives a maximum improvement of 1 cycle, but requires
     ;; quite a lot of hairy code.
     (give-up-ir1-transform))))

(deftransform * ((x y)
		 ((unsigned-byte 64) (constant-arg (unsigned-byte 64)))
		 (unsigned-byte 64))
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer y)))
(deftransform sb!vm::*-mod64
    ((x y) ((unsigned-byte 64) (constant-arg (unsigned-byte 64)))
     (unsigned-byte 64))
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer y)))

(deftransform * ((x y)
		 ((signed-byte 61) (constant-arg (unsigned-byte 64)))
		 (signed-byte 61))
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer y)))
(deftransform sb!vm::*-smod61
    ((x y) ((signed-byte 61) (constant-arg (unsigned-byte 64)))
     (signed-byte 61))
  "recode as leas, shifts and adds"
  (let ((y (lvar-value y)))
    (*-transformer y)))
