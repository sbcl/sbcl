;;;; the VM definition arithmetic VOPs for the x86

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
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:results (r :scs (any-reg)
	       :load-if (not (location= x r))))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic"))

(define-vop (fast-unsigned-binop-c fast-safe-arith-op)
  (:args (x :target r :scs (unsigned-reg unsigned-stack)))
  (:info y)
  (:arg-types unsigned-num (:constant (unsigned-byte 32)))
  (:results (r :scs (unsigned-reg)
	       :load-if (not (location= x r))))
  (:result-types unsigned-num)
  (:note "inline (unsigned-byte 32) arithmetic"))

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
	   (inst lea r (make-ea :dword :base x :index y :scale 1)))
	  (t
	   (move r x)
	   (inst add r y)))))

(define-vop (fast-+-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate +)
  (:args (x :target r :scs (any-reg control-stack)))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 30)))
  (:results (r :scs (any-reg)
	       :load-if (not (location= x r))))
  (:result-types tagged-num)
  (:note "inline fixnum arithmetic")
  (:generator 1
    (cond ((and (sc-is x any-reg) (sc-is r any-reg) (not (location= x r)))
	   (inst lea r (make-ea :dword :base x :disp (fixnumize y))))
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
	   (inst lea r (make-ea :dword :base x :index y :scale 1)))
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
	   (inst lea r (make-ea :dword :base x :disp y)))
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
	   (inst lea r (make-ea :dword :base x :index y :scale 1)))
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
	   (inst lea r (make-ea :dword :base x :disp y)))
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
    (inst sar r 2)
    (inst imul r y)))

(define-vop (fast-*-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate *)
  ;; We need different loading characteristics.
  (:args (x :scs (any-reg control-stack)))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 30)))
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
    (inst cdq)
    (inst idiv eax y)
    (if (location= quo eax)
	(inst shl eax 2)
	(inst lea quo (make-ea :dword :index eax :scale 4)))
    (move rem edx)))

(define-vop (fast-truncate-c/fixnum=>fixnum fast-safe-arith-op)
  (:translate truncate)
  (:args (x :scs (any-reg) :target eax))
  (:info y)
  (:arg-types tagged-num (:constant (signed-byte 30)))
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
    (inst cdq)
    (inst mov y-arg (fixnumize y))
    (inst idiv eax y-arg)
    (if (location= quo eax)
	(inst shl eax 2)
	(inst lea quo (make-ea :dword :index eax :scale 4)))
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
    (inst cdq)
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
    (inst cdq)
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
	   (inst lea result (make-ea :dword :index number :scale 2)))
	  ((and (= amount 2) (not (location= number result)))
	   (inst lea result (make-ea :dword :index number :scale 4)))
	  ((and (= amount 3) (not (location= number result)))
	   (inst lea result (make-ea :dword :index number :scale 8)))
	  (t
	   (move result number)
	   (cond ((plusp amount)
		  ;; We don't have to worry about overflow because of the
		  ;; result type restriction.
		  (inst shl result amount))
		 (t
		  ;; If the amount is greater than 31, only shift by 31. We
		  ;; have to do this because the shift instructions only look
		  ;; at the low five bits of the result.
		  (inst sar result (min 31 (- amount)))
		  ;; Fixnum correction.
		  (inst and result #xfffffffc)))))))

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

(define-vop (fast-ash-c)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg unsigned-reg) :target result
		 :load-if (not (and (sc-is number signed-stack unsigned-stack)
				    (sc-is result signed-stack unsigned-stack)
				    (location= number result)))))
  (:info amount)
  (:arg-types (:or signed-num unsigned-num) (:constant integer))
  (:results (result :scs (signed-reg unsigned-reg)
		    :load-if (not
			      (and (sc-is number signed-stack unsigned-stack)
				   (sc-is result signed-stack unsigned-stack)
				   (location= number result)))))
  (:result-types (:or signed-num unsigned-num))
  (:note "inline ASH")
  (:generator 3
    (cond ((and (= amount 1) (not (location= number result)))
	   (inst lea result (make-ea :dword :index number :scale 2)))
	  ((and (= amount 2) (not (location= number result)))
	   (inst lea result (make-ea :dword :index number :scale 4)))
	  ((and (= amount 3) (not (location= number result)))
	   (inst lea result (make-ea :dword :index number :scale 8)))
	  (t
	   (move result number)
	   (cond ((plusp amount)
		  ;; We don't have to worry about overflow because of the
		  ;; result type restriction.
		  (inst shl result amount))
		 ((sc-is number signed-reg signed-stack)
		  ;; If the amount is greater than 31, only shift by 31. We
		  ;; have to do this because the shift instructions only look
		  ;; at the low five bits of the result.
		  (inst sar result (min 31 (- amount))))
		 (t
		  (inst shr result (min 31 (- amount)))))))))

(define-vop (fast-ash-left)
  (:translate ash)
  (:args (number :scs (signed-reg unsigned-reg) :target result
		 :load-if (not (and (sc-is number signed-stack unsigned-stack)
				    (sc-is result signed-stack unsigned-stack)
				    (location= number result))))
	 (amount :scs (unsigned-reg) :target ecx))
  (:arg-types (:or signed-num unsigned-num) positive-fixnum)
  (:temporary (:sc unsigned-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:results (result :scs (signed-reg unsigned-reg) :from (:argument 0)
		    :load-if (not
			      (and (sc-is number signed-stack unsigned-stack)
				   (sc-is result signed-stack unsigned-stack)
				   (location= number result)))))
  (:result-types (:or signed-num unsigned-num))
  (:policy :fast-safe)
  (:note "inline ASH")
  (:generator 4
    (move result number)
    (move ecx amount)
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)))

(define-vop (fast-ash)
  (:translate ash)
  (:policy :fast-safe)
  (:args (number :scs (signed-reg unsigned-reg) :target result)
	 (amount :scs (signed-reg) :target ecx))
  (:arg-types (:or signed-num unsigned-num) signed-num)
  (:results (result :scs (signed-reg unsigned-reg) :from (:argument 0)))
  (:result-types (:or signed-num unsigned-num))
  (:temporary (:sc signed-reg :offset ecx-offset :from (:argument 1)) ecx)
  (:note "inline ASH")
  (:generator 5
    (move result number)
     (move ecx amount)
    (inst or ecx ecx)
    (inst jmp :ns positive)
    (inst neg ecx)
    (inst cmp ecx 31)
    (inst jmp :be okay)
    (inst mov ecx 31)
    OKAY
    (sc-case number
      (signed-reg (inst sar result :cl))
      (unsigned-reg (inst shr result :cl)))
    (inst jmp done)

    POSITIVE
    ;; The result-type ensures us that this shift will not overflow.
    (inst shl result :cl)

    DONE))

;;; Note: documentation for this function is wrong - rtfm
(define-vop (signed-byte-32-len)
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

(define-vop (unsigned-byte-32-len)
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

(define-vop (unsigned-byte-32-count)
  (:translate logcount)
  (:note "inline (unsigned-byte 32) logcount")
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg)))
  (:arg-types unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:sc unsigned-reg :from (:argument 0)) temp)
  (:generator 30
    (move result arg)

    (inst mov temp result)
    (inst shr temp 1)
    (inst and result #x55555555)
    (inst and temp #x55555555)
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
    (inst add result temp)))

;;;; binary conditional VOPs

(define-vop (fast-conditional)
  (:conditional)
  (:info target not-p)
  (:effects)
  (:affected)
  (:policy :fast-safe))

(define-vop (fast-conditional/fixnum fast-conditional)
  (:args (x :scs (any-reg)
	    :load-if (not (and (sc-is x control-stack)
			       (sc-is y any-reg))))
	 (y :scs (any-reg control-stack)))
  (:arg-types tagged-num tagged-num)
  (:note "inline fixnum comparison"))

(define-vop (fast-conditional-c/fixnum fast-conditional/fixnum)
  (:args (x :scs (any-reg control-stack)))
  (:arg-types tagged-num (:constant (signed-byte 30)))
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
  (:arg-types signed-num (:constant (signed-byte 32)))
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
  (:arg-types unsigned-num (:constant (unsigned-byte 32)))
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
  (:arg-types tagged-num (:constant (signed-byte 30)))
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
  (:arg-types * (:constant (signed-byte 30)))
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

(define-source-transform 32bit-logical-not (x)
  `(logand (lognot (the (unsigned-byte 32) ,x)) #.(1- (ash 1 32))))

(deftransform 32bit-logical-and ((x y))
  '(logand x y))

(define-source-transform 32bit-logical-nand (x y)
  `(32bit-logical-not (32bit-logical-and ,x ,y)))

(deftransform 32bit-logical-or ((x y))
  '(logior x y))

(define-source-transform 32bit-logical-nor (x y)
  `(32bit-logical-not (32bit-logical-or ,x ,y)))

(deftransform 32bit-logical-xor ((x y))
  '(logxor x y))

(define-source-transform 32bit-logical-eqv (x y)
  `(32bit-logical-not (32bit-logical-xor ,x ,y)))

(define-source-transform 32bit-logical-orc1 (x y)
  `(32bit-logical-or (32bit-logical-not ,x) ,y))

(define-source-transform 32bit-logical-orc2 (x y)
  `(32bit-logical-or ,x (32bit-logical-not ,y)))

(define-source-transform 32bit-logical-andc1 (x y)
  `(32bit-logical-and (32bit-logical-not ,x) ,y))

(define-source-transform 32bit-logical-andc2 (x y)
  `(32bit-logical-and ,x (32bit-logical-not ,y)))

;;; Only the lower 5 bits of the shift amount are significant.
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

(define-modular-fun +-mod32 (x y) + 32)
(define-vop (fast-+-mod32/unsigned=>unsigned fast-+/unsigned=>unsigned)
  (:translate +-mod32))
(define-vop (fast-+-mod32-c/unsigned=>unsigned fast-+-c/unsigned=>unsigned)
  (:translate +-mod32))

;;; logical operations
(define-modular-fun lognot-mod32 (x) lognot 32)
(define-vop (lognot-mod32/unsigned=>unsigned)
  (:translate lognot-mod32)
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

(define-modular-fun logxor-mod32 (x y) logxor 32)
(define-vop (fast-logxor-mod32/unsigned=>unsigned
             fast-logxor/unsigned=>unsigned)
  (:translate logxor-mod32))
(define-vop (fast-logxor-mod32-c/unsigned=>unsigned
             fast-logxor-c/unsigned=>unsigned)
  (:translate logxor-mod32))

;;;; bignum stuff

(define-vop (bignum-length get-header-data)
  (:translate sb!bignum::%bignum-length)
  (:policy :fast-safe))

(define-vop (bignum-set-length set-header-data)
  (:translate sb!bignum::%bignum-set-length)
  (:policy :fast-safe))

(define-full-reffer bignum-ref * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb!bignum::%bignum-ref)

(define-full-setter bignum-set * bignum-digits-offset other-pointer-lowtag
  (unsigned-reg) unsigned-num sb!bignum::%bignum-set)

(define-vop (digit-0-or-plus)
  (:translate sb!bignum::%digit-0-or-plusp)
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
  (:translate sb!bignum::%add-with-carry)
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
  (:translate sb!bignum::%subtract-with-borrow)
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
  (:translate sb!bignum::%multiply-and-add)
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
  (:translate sb!bignum::%multiply-and-add)
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
  (:translate sb!bignum::%multiply)
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

(define-vop (bignum-lognot lognot-mod32/unsigned=>unsigned)
  (:translate sb!bignum::%lognot))

(define-vop (fixnum-to-digit)
  (:translate sb!bignum::%fixnum-to-digit)
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
    (inst sar digit 2)))

(define-vop (bignum-floor)
  (:translate sb!bignum::%floor)
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
  (:translate sb!bignum::%fixnum-digit-with-correct-sign)
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
      (inst shl res 2))))

(define-vop (digit-ashr)
  (:translate sb!bignum::%ashr)
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
  (:translate sb!bignum::%digit-logical-shift-right)
  (:generator 1
    (move result digit)
    (move ecx count)
    (inst shr result :cl)))

(define-vop (digit-ashl digit-ashr)
  (:translate sb!bignum::%ashl)
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
(defknown random-mt19937 ((simple-array (unsigned-byte 32) (*)))
  (unsigned-byte 32) ())
(define-vop (random-mt19937)
  (:policy :fast-safe)
  (:translate random-mt19937)
  (:args (state :scs (descriptor-reg) :to :result))
  (:arg-types simple-array-unsigned-byte-32)
  (:temporary (:sc unsigned-reg :from (:eval 0) :to :result) k)
  (:temporary (:sc unsigned-reg :offset eax-offset
		   :from (:eval 0) :to :result) tmp)
  (:results (y :scs (unsigned-reg) :from (:eval 0)))
  (:result-types unsigned-num)
  (:generator 50
    (inst mov k (make-ea :dword :base state
			 :disp (- (* (+ 2 vector-data-offset)
				     n-word-bytes)
				  other-pointer-lowtag)))
    (inst cmp k 624)
    (inst jmp :ne no-update)
    (inst mov tmp state)	; The state is passed in EAX.
    (inst call (make-fixup 'random-mt19937-update :assembly-routine))
    ;; Restore k, and set to 0.
    (inst xor k k)
    NO-UPDATE
    ;; y = ptgfsr[k++];
    (inst mov y (make-ea :dword :base state :index k :scale 4
			 :disp (- (* (+ 3 vector-data-offset)
				     n-word-bytes)
				  other-pointer-lowtag)))
    ;; y ^= (y >> 11);
    (inst shr y 11)
    (inst xor y (make-ea :dword :base state :index k :scale 4
			 :disp (- (* (+ 3 vector-data-offset)
				     n-word-bytes)
				  other-pointer-lowtag)))
    ;; y ^= (y << 7) & #x9d2c5680
    (inst mov tmp y)
    (inst inc k)
    (inst shl tmp 7)
    (inst mov (make-ea :dword :base state
		       :disp (- (* (+ 2 vector-data-offset)
				   n-word-bytes)
				other-pointer-lowtag))
	  k)
    (inst and tmp #x9d2c5680)
    (inst xor y tmp)
    ;; y ^= (y << 15) & #xefc60000
    (inst mov tmp y)
    (inst shl tmp 15)
    (inst and tmp #xefc60000)
    (inst xor y tmp)
    ;; y ^= (y >> 18);
    (inst mov tmp y)
    (inst shr tmp 18)
    (inst xor y tmp)))

(in-package "SB!C")

(defknown %lea ((or (signed-byte 32) (unsigned-byte 32))
		(or (signed-byte 32) (unsigned-byte 32))
		(member 1 2 4 8) (signed-byte 32))
  (or (signed-byte 32) (unsigned-byte 32))
  (foldable flushable))

(defoptimizer (%lea derive-type) ((base index scale disp))
  (when (and (constant-continuation-p scale)
	     (constant-continuation-p disp))
    (let ((scale (continuation-value scale))
	  (disp (continuation-value disp))
	  (base-type (continuation-type base))
	  (index-type (continuation-type index)))
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
	      (:constant (signed-byte 32)))
  (:results (r :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst lea r (make-ea :dword :base base :index index
			 :scale scale :disp disp))))

(define-vop (%lea/signed=>signed)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (signed-reg))
	 (index :scs (signed-reg)))
  (:info scale disp)
  (:arg-types signed-num signed-num
	      (:constant (member 1 2 4 8))
	      (:constant (signed-byte 32)))
  (:results (r :scs (signed-reg)))
  (:result-types signed-num)
  (:generator 4
    (inst lea r (make-ea :dword :base base :index index
			 :scale scale :disp disp))))

(define-vop (%lea/fixnum=>fixnum)
  (:translate %lea)
  (:policy :fast-safe)
  (:args (base :scs (any-reg))
	 (index :scs (any-reg)))
  (:info scale disp)
  (:arg-types tagged-num tagged-num
	      (:constant (member 1 2 4 8))
	      (:constant (signed-byte 32)))
  (:results (r :scs (any-reg)))
  (:result-types tagged-num)
  (:generator 3
    (inst lea r (make-ea :dword :base base :index index
			 :scale scale :disp disp))))

(in-package "SB!C")

;;; This is essentially a straight implementation of the algorithm in
;;; "Strength Reduction of Multiplications by Integer Constants",
;;; Youfeng Wu, ACM SIGPLAN Notices, Vol. 30, No.2, February 1995.
(defun basic-decompose-multiplication (arg num n-bits condensed)
  (case (aref condensed 0)
    (0
     (let ((tmp (min 3 (aref condensed 1))))
       (decf (aref condensed 1) tmp)
       `(truly-the (unsigned-byte 32)
	 (%lea ,arg
	       ,(decompose-multiplication
		 arg (ash (1- num) (- tmp)) (1- n-bits) (subseq condensed 1))
	       ,(ash 1 tmp) 0))))
    ((1 2 3)
     (let ((r0 (aref condensed 0)))
       (incf (aref condensed 1) r0)
       `(truly-the (unsigned-byte 32)
	 (%lea ,(decompose-multiplication
		 arg (- num (ash 1 r0)) (1- n-bits) (subseq condensed 1))
	       ,arg
	       ,(ash 1 r0) 0))))
    (t (let ((r0 (aref condensed 0)))
	 (setf (aref condensed 0) 0)
	 `(truly-the (unsigned-byte 32)
	   (ash ,(decompose-multiplication
		  arg (ash num (- r0)) n-bits condensed)
	        ,r0))))))

(defun decompose-multiplication (arg num n-bits condensed)
  (cond
    ((= n-bits 0) 0)
    ((= num 1) arg)
    ((= n-bits 1)
     `(truly-the (unsigned-byte 32) (ash ,arg ,(1- (integer-length num)))))
    ((let ((max 0) (end 0))
       (loop for i from 2 to (length condensed)
	     for j = (reduce #'+ (subseq condensed 0 i))
	     when (and (> (- (* 2 i) 3 j) max)
		       (< (+ (ash 1 (1+ j))
			     (ash (ldb (byte (- 32 (1+ j)) (1+ j)) num)
				  (1+ j)))
			  (ash 1 32)))
	       do (setq max (- (* 2 i) 3 j)
			end i))
       (when (> max 0)
	 (let ((j (reduce #'+ (subseq condensed 0 end))))
	   (let ((n2 (+ (ash 1 (1+ j))
			(ash (ldb (byte (- 32 (1+ j)) (1+ j)) num) (1+ j))))
		 (n1 (1+ (ldb (byte (1+ j) 0) (lognot num)))))
	   `(truly-the (unsigned-byte 32)
	     (- ,(optimize-multiply arg n2) ,(optimize-multiply arg n1))))))))
    ((dolist (i '(9 5 3))
       (when (integerp (/ num i))
	 (when (< (logcount (/ num i)) (logcount num))
	   (let ((x (gensym)))
	     (return `(let ((,x ,(optimize-multiply arg (/ num i))))
		       (truly-the (unsigned-byte 32)
			(%lea ,x ,x (1- ,i) 0)))))))))
    (t (basic-decompose-multiplication arg num n-bits condensed))))
	   
(defun optimize-multiply (arg x)
  (let* ((n-bits (logcount x))
	 (condensed (make-array n-bits)))
    (let ((count 0) (bit 0))
      (dotimes (i 32)
	(cond ((logbitp i x)
	       (setf (aref condensed bit) count)
	       (setf count 1)
	       (incf bit))
	      (t (incf count)))))
    (decompose-multiplication arg x n-bits condensed)))

(deftransform * ((x y)
		 ((unsigned-byte 32) (constant-arg (unsigned-byte 32)))
		 (unsigned-byte 32))
  "recode as leas, shifts and adds"
  (let ((y (continuation-value y)))
    (cond
      ((= y (ash 1 (integer-length y)))
       ;; there's a generic transform for y = 2^k
       (give-up-ir1-transform))
      ((member y '(3 5 9))
       ;; we can do these multiplications directly using LEA
       `(%lea x x ,(1- y) 0))
      ((member :pentium4 *backend-subfeatures*)
       ;; the pentium4's multiply unit is reportedly very good
       (give-up-ir1-transform))
      ;; FIXME: should make this more fine-grained.  If nothing else,
      ;; there should probably be a cutoff of about 9 instructions on
      ;; pentium-class machines.
      (t (optimize-multiply 'x y)))))

;;; FIXME: we should also be able to write an optimizer or two to
;;; convert (+ (* x 2) 17), (- (* x 9) 5) to a %LEA.
