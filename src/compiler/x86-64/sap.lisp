;;;; SAP operations for the x86 VM

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

;;; Move a tagged SAP to an untagged representation.
(define-vop (move-to-sap)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (sap-reg)))
  (:note "pointer to SAP coercion")
  (:generator 1
    (loadw y x sap-pointer-slot other-pointer-lowtag)))
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))

;;; Move an untagged SAP to a tagged representation.
(define-vop (move-from-sap)
  (:args (sap :scs (sap-reg) :to :result))
  (:results (res :scs (descriptor-reg) :from :argument))
  (:note "SAP to pointer coercion")
  (:node-var node)
  (:generator 20
    (with-fixed-allocation (res sap-widetag sap-size node)
      (storew sap res sap-pointer-slot other-pointer-lowtag))))
(define-move-vop move-from-sap :move
  (sap-reg) (descriptor-reg))

;;; Move untagged sap values.
(define-vop (sap-move)
  (:args (x :target y
	    :scs (sap-reg)
	    :load-if (not (location= x y))))
  (:results (y :scs (sap-reg)
	       :load-if (not (location= x y))))
  (:note "SAP move")
  (:effects)
  (:affected)
  (:generator 0
    (move y x)))
(define-move-vop sap-move :move
  (sap-reg) (sap-reg))

;;; Move untagged sap arguments/return-values.
(define-vop (move-sap-arg)
  (:args (x :target y
	    :scs (sap-reg))
	 (fp :scs (any-reg)
	     :load-if (not (sc-is y sap-reg))))
  (:results (y))
  (:note "SAP argument move")
  (:generator 0
    (sc-case y
      (sap-reg
       (move y x))
      (sap-stack
       (if (= (tn-offset fp) esp-offset)
	   (storew x fp (tn-offset y))	; c-call
	   (storew x fp (- (1+ (tn-offset y)))))))))
(define-move-vop move-sap-arg :move-arg
  (descriptor-reg sap-reg) (sap-reg))

;;; Use standard MOVE-ARG + coercion to move an untagged sap to a
;;; descriptor passing location.
(define-move-vop move-arg :move-arg
  (sap-reg) (descriptor-reg))

;;;; SAP-INT and INT-SAP

;;; The function SAP-INT is used to generate an integer corresponding
;;; to the system area pointer, suitable for passing to the kernel
;;; interfaces (which want all addresses specified as integers). The
;;; function INT-SAP is used to do the opposite conversion. The
;;; integer representation of a SAP is the byte offset of the SAP from
;;; the start of the address space.
(define-vop (sap-int)
  (:args (sap :scs (sap-reg) :target int))
  (:arg-types system-area-pointer)
  (:results (int :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:translate sap-int)
  (:policy :fast-safe)
  (:generator 1
    (move int sap)))
(define-vop (int-sap)
  (:args (int :scs (unsigned-reg) :target sap))
  (:arg-types unsigned-num)
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:translate int-sap)
  (:policy :fast-safe)
  (:generator 1
    (move sap int)))

;;;; POINTER+ and POINTER-

(define-vop (pointer+)
  (:translate sap+)
  (:args (ptr :scs (sap-reg) :target res
	      :load-if (not (location= ptr res)))
	 (offset :scs (signed-reg immediate)))
  (:arg-types system-area-pointer signed-num)
  (:results (res :scs (sap-reg) :from (:argument 0)
		 :load-if (not (location= ptr res))))
  (:result-types system-area-pointer)
  (:temporary (:sc signed-reg) temp)
  (:policy :fast-safe)
  (:generator 1
    (cond ((and (sc-is ptr sap-reg) (sc-is res sap-reg)
		(not (location= ptr res)))
	   (sc-case offset
	     (signed-reg
	      (inst lea res (make-ea :qword :base ptr :index offset :scale 1)))
	     (immediate
	      (let ((value (tn-value offset)))
		(cond ((typep value '(or (signed-byte 32) (unsigned-byte 31)))
		       (inst lea res (make-ea :qword :base ptr :disp value)))
		      (t
		       (inst mov temp value)
		       (inst lea res (make-ea :qword :base ptr
					      :index temp
					      :scale 1))))))))
	  (t
	   (move res ptr)
	   (sc-case offset
	     (signed-reg
	      (inst add res offset))
	     (immediate
	      (let ((value (tn-value offset)))
		(cond ((typep value '(or (signed-byte 32) (unsigned-byte 31)))
		       (inst add res (tn-value offset)))
		      (t
		       (inst mov temp value)
		       (inst add res temp))))))))))

(define-vop (pointer-)
  (:translate sap-)
  (:args (ptr1 :scs (sap-reg) :target res)
	 (ptr2 :scs (sap-reg)))
  (:arg-types system-area-pointer system-area-pointer)
  (:policy :fast-safe)
  (:results (res :scs (signed-reg) :from (:argument 0)))
  (:result-types signed-num)
  (:generator 1
    (move res ptr1)
    (inst sub res ptr2)))

;;;; mumble-SYSTEM-REF and mumble-SYSTEM-SET

(macrolet ((def-system-ref-and-set (ref-name
				    set-name
				    sc
				    type
				    size
				    &optional signed)
	     (let ((ref-name-c (symbolicate ref-name "-C"))
		   (set-name-c (symbolicate set-name "-C"))
		   (temp-sc (symbolicate size "-REG")))
	       `(progn
		  (define-vop (,ref-name)
		    (:translate ,ref-name)
		    (:policy :fast-safe)
		    (:args (sap :scs (sap-reg))
			   (offset :scs (signed-reg)))
		    (:arg-types system-area-pointer signed-num)
		    ,@(unless (eq size :qword)
			`((:temporary (:sc ,temp-sc
				       :from (:eval 0)
				       :to (:eval 1))
				      temp)))
		    (:results (result :scs (,sc)))
		    (:result-types ,type)
		    (:generator 5
				(inst mov ,(if (eq size :qword) 'result 'temp)
				      (make-ea ,size :base sap :index offset))
				,@(unless (eq size :qword)
				    `((inst ,(if signed 'movsx 'movzx)
					    result temp)))))
		  (define-vop (,ref-name-c)
		    (:translate ,ref-name)
		    (:policy :fast-safe)
		    (:args (sap :scs (sap-reg)))
		    (:arg-types system-area-pointer
				(:constant (signed-byte 64)))
		    (:info offset)
		    ,@(unless (eq size :qword)
			`((:temporary (:sc ,temp-sc
				       :from (:eval 0)
				       :to (:eval 1))
				      temp)))
		    (:results (result :scs (,sc)))
		    (:result-types ,type)
		    (:generator 4
				(inst mov ,(if (eq size :qword) 'result 'temp)
				      (make-ea ,size :base sap :disp offset))
				,@(unless (eq size :qword)
				    `((inst ,(if signed 'movsx 'movzx)
					    result temp)))))
		  (define-vop (,set-name)
		    (:translate ,set-name)
		    (:policy :fast-safe)
		    (:args (sap :scs (sap-reg) :to (:eval 0))
			   (offset :scs (signed-reg) :to (:eval 0))
			   (value :scs (,sc)
				  :target ,(if (eq size :qword)
					       'result
					       'temp)))
		    (:arg-types system-area-pointer signed-num ,type)
		    ,@(unless (eq size :qword)
			`((:temporary (:sc ,temp-sc :offset rax-offset
					   :from (:argument 2) :to (:result 0)
					   :target result)
				      temp)))
		    (:results (result :scs (,sc)))
		    (:result-types ,type)
		    (:generator 5
				,@(unless (eq size :qword)
				    `((move rax-tn value)))
				(inst mov (make-ea ,size
						   :base sap
						   :index offset)
				      ,(if (eq size :qword) 'value 'temp))
				(move result
				      ,(if (eq size :qword) 'value 'rax-tn))))
		  (define-vop (,set-name-c)
		    (:translate ,set-name)
		    (:policy :fast-safe)
		    (:args (sap :scs (sap-reg) :to (:eval 0))
			   (value :scs (,sc)
				  :target ,(if (eq size :qword)
					       'result
					       'temp)))
		    (:arg-types system-area-pointer
				(:constant (signed-byte 64)) ,type)
		    (:info offset)
		    ,@(unless (eq size :qword)
			`((:temporary (:sc ,temp-sc :offset rax-offset
					   :from (:argument 2) :to (:result 0)
					   :target result)
				      temp)))
		    (:results (result :scs (,sc)))
		    (:result-types ,type)
		    (:generator 4
				,@(unless (eq size :qword)
				    `((move rax-tn value)))
				(inst mov
				      (make-ea ,size :base sap :disp offset)
				      ,(if (eq size :qword) 'value 'temp))
				(move result ,(if (eq size :qword)
						  'value
						  'rax-tn))))))))

  (def-system-ref-and-set sap-ref-8 %set-sap-ref-8
    unsigned-reg positive-fixnum :byte nil)
  (def-system-ref-and-set signed-sap-ref-8 %set-signed-sap-ref-8
    signed-reg tagged-num :byte t)
  (def-system-ref-and-set sap-ref-16 %set-sap-ref-16
    unsigned-reg positive-fixnum :word nil)
  (def-system-ref-and-set signed-sap-ref-16 %set-signed-sap-ref-16
    signed-reg tagged-num :word t)
  (def-system-ref-and-set sap-ref-32 %set-sap-ref-32
    unsigned-reg unsigned-num :dword nil)
  (def-system-ref-and-set signed-sap-ref-32 %set-signed-sap-ref-32
    signed-reg signed-num :dword t)
  (def-system-ref-and-set sap-ref-64 %set-sap-ref-64
    unsigned-reg unsigned-num :qword nil)
  (def-system-ref-and-set signed-sap-ref-64 %set-signed-sap-ref-64
    signed-reg signed-num :qword t)
  (def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
    sap-reg system-area-pointer :qword))

;;;; SAP-REF-DOUBLE

(define-vop (sap-ref-double)
  (:translate sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
     (inst movsd result (make-ea :qword :base sap :index offset))))

(define-vop (sap-ref-double-c)
  (:translate sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 64)))
  (:info offset)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
     (inst movsd result (make-ea :qword :base sap :disp offset))))

(define-vop (%set-sap-ref-double)
  (:translate %set-sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (offset :scs (signed-reg) :to (:eval 0))
	 (value :scs (double-reg)))
  (:arg-types system-area-pointer signed-num double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
    (inst movsd (make-ea :qword :base sap :index offset) value)
    (move result value)))

(define-vop (%set-sap-ref-double-c)
  (:translate %set-sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (value :scs (double-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 64)) double-float)
  (:info offset)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
    (inst movsd (make-ea :qword :base sap :disp offset) value)
    (move result value)))

;;;; SAP-REF-SINGLE

(define-vop (sap-ref-single)
  (:translate sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
     (inst movss result (make-ea :dword :base sap :index offset))))

(define-vop (sap-ref-single-c)
  (:translate sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)))
  (:info offset)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
     (inst movss result (make-ea :dword :base sap :disp offset))))

(define-vop (%set-sap-ref-single)
  (:translate %set-sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (offset :scs (signed-reg) :to (:eval 0))
	 (value :scs (single-reg)))
  (:arg-types system-area-pointer signed-num single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (inst movss (make-ea :dword :base sap :index offset) value)
    (move result value)))

(define-vop (%set-sap-ref-single-c)
  (:translate %set-sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (value :scs (single-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)) single-float)
  (:info offset)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
    (inst movss (make-ea :dword :base sap :disp offset) value)
    (move result value)))


;;; noise to convert normal lisp data objects into SAPs

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target sap))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (move sap vector)
    (inst add
	  sap
	  (- (* vector-data-offset n-word-bytes) other-pointer-lowtag))))


