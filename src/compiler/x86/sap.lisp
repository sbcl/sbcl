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
    (loadw y x sap-pointer-slot other-pointer-type)))
(define-move-vop move-to-sap :move
  (descriptor-reg) (sap-reg))

;;; Move an untagged SAP to a tagged representation.
(define-vop (move-from-sap)
  (:args (sap :scs (sap-reg) :to :result))
  (:results (res :scs (descriptor-reg) :from :argument))
  (:note "SAP to pointer coercion")
  (:node-var node)
  (:generator 20
    (with-fixed-allocation (res sap-type sap-size node)
      (storew sap res sap-pointer-slot other-pointer-type))))
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
(define-vop (move-sap-argument)
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
(define-move-vop move-sap-argument :move-argument
  (descriptor-reg sap-reg) (sap-reg))

;;; Use standard MOVE-ARGUMENT + coercion to move an untagged sap to a
;;; descriptor passing location.
(define-move-vop move-argument :move-argument
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
  (:policy :fast-safe)
  (:generator 1
    (cond ((and (sc-is ptr sap-reg) (sc-is res sap-reg)
		(not (location= ptr res)))
	   (sc-case offset
	     (signed-reg
	      (inst lea res (make-ea :dword :base ptr :index offset :scale 1)))
	     (immediate
	      (inst lea res (make-ea :dword :base ptr
				     :disp (tn-value offset))))))
	  (t
	   (move res ptr)
	   (sc-case offset
	     (signed-reg
	      (inst add res offset))
	     (immediate
	      (inst add res (tn-value offset))))))))

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
		    ,@(unless (eq size :dword)
			`((:temporary (:sc ,temp-sc
				       :from (:eval 0)
				       :to (:eval 1))
				      temp)))
		    (:results (result :scs (,sc)))
		    (:result-types ,type)
		    (:generator 5
				(inst mov ,(if (eq size :dword) 'result 'temp)
				      (make-ea ,size :base sap :index offset))
				,@(unless (eq size :dword)
				    `((inst ,(if signed 'movsx 'movzx)
					    result temp)))))
		  (define-vop (,ref-name-c)
		    (:translate ,ref-name)
		    (:policy :fast-safe)
		    (:args (sap :scs (sap-reg)))
		    (:arg-types system-area-pointer
				(:constant (signed-byte 32)))
		    (:info offset)
		    ,@(unless (eq size :dword)
			`((:temporary (:sc ,temp-sc
				       :from (:eval 0)
				       :to (:eval 1))
				      temp)))
		    (:results (result :scs (,sc)))
		    (:result-types ,type)
		    (:generator 4
				(inst mov ,(if (eq size :dword) 'result 'temp)
				      (make-ea ,size :base sap :disp offset))
				,@(unless (eq size :dword)
				    `((inst ,(if signed 'movsx 'movzx)
					    result temp)))))
		  (define-vop (,set-name)
		    (:translate ,set-name)
		    (:policy :fast-safe)
		    (:args (sap :scs (sap-reg) :to (:eval 0))
			   (offset :scs (signed-reg) :to (:eval 0))
			   (value :scs (,sc)
				  :target ,(if (eq size :dword)
					       'result
					       'temp)))
		    (:arg-types system-area-pointer signed-num ,type)
		    ,@(unless (eq size :dword)
			`((:temporary (:sc ,temp-sc :offset eax-offset
					   :from (:argument 2) :to (:result 0)
					   :target result)
				      temp)))
		    (:results (result :scs (,sc)))
		    (:result-types ,type)
		    (:generator 5
				,@(unless (eq size :dword)
				    `((move eax-tn value)))
				(inst mov (make-ea ,size
						   :base sap
						   :index offset)
				      ,(if (eq size :dword) 'value 'temp))
				(move result
				      ,(if (eq size :dword) 'value 'eax-tn))))
		  (define-vop (,set-name-c)
		    (:translate ,set-name)
		    (:policy :fast-safe)
		    (:args (sap :scs (sap-reg) :to (:eval 0))
			   (value :scs (,sc)
				  :target ,(if (eq size :dword)
					       'result
					       'temp)))
		    (:arg-types system-area-pointer
				(:constant (signed-byte 32)) ,type)
		    (:info offset)
		    ,@(unless (eq size :dword)
			`((:temporary (:sc ,temp-sc :offset eax-offset
					   :from (:argument 2) :to (:result 0)
					   :target result)
				      temp)))
		    (:results (result :scs (,sc)))
		    (:result-types ,type)
		    (:generator 4
				,@(unless (eq size :dword)
				    `((move eax-tn value)))
				(inst mov
				      (make-ea ,size :base sap :disp offset)
				      ,(if (eq size :dword) 'value 'temp))
				(move result ,(if (eq size :dword)
						  'value
						  'eax-tn))))))))

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
  (def-system-ref-and-set sap-ref-sap %set-sap-ref-sap
    sap-reg system-area-pointer :dword))

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
     (with-empty-tn@fp-top(result)
	(inst fldd (make-ea :dword :base sap :index offset)))))

(define-vop (sap-ref-double-c)
  (:translate sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)))
  (:info offset)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
     (with-empty-tn@fp-top(result)
	(inst fldd (make-ea :dword :base sap :disp offset)))))

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
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0.
	   (inst fstd (make-ea :dword :base sap :index offset))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fstd (make-ea :dword :base sap :index offset))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0.
		  (unless (location= value result)
			  (inst fstd result))
		  (inst fxch value)))))))

(define-vop (%set-sap-ref-double-c)
  (:translate %set-sap-ref-double)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (value :scs (double-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)) double-float)
  (:info offset)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0.
	   (inst fstd (make-ea :dword :base sap :disp offset))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fstd (make-ea :dword :base sap :disp offset))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0.
		  (unless (location= value result)
			  (inst fstd result))
		  (inst fxch value)))))))

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
     (with-empty-tn@fp-top(result)
	(inst fld (make-ea :dword :base sap :index offset)))))

(define-vop (sap-ref-single-c)
  (:translate sap-ref-single)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)))
  (:info offset)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
     (with-empty-tn@fp-top(result)
	(inst fld (make-ea :dword :base sap :disp offset)))))

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
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fst (make-ea :dword :base sap :index offset))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fst result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fst (make-ea :dword :base sap :index offset))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fst value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fst result))
		  (inst fxch value)))))))

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
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (inst fst (make-ea :dword :base sap :disp offset))
	   (unless (zerop (tn-offset result))
		   ;; Value is in ST0 but not result.
		   (inst fst result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (inst fst (make-ea :dword :base sap :disp offset))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fst value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
			  (inst fst result))
		  (inst fxch value)))))))

;;;; SAP-REF-LONG

(define-vop (sap-ref-long)
  (:translate sap-ref-long)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (offset :scs (signed-reg)))
  (:arg-types system-area-pointer signed-num)
  (:results (result :scs (#!+long-float long-reg #!-long-float double-reg)))
  (:result-types #!+long-float long-float #!-long-float double-float)
  (:generator 5
     (with-empty-tn@fp-top(result)
	(inst fldl (make-ea :dword :base sap :index offset)))))

(define-vop (sap-ref-long-c)
  (:translate sap-ref-long)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:arg-types system-area-pointer (:constant (signed-byte 32)))
  (:info offset)
  (:results (result :scs (#!+long-float long-reg #!-long-float double-reg)))
  (:result-types #!+long-float long-float #!-long-float double-float)
  (:generator 4
     (with-empty-tn@fp-top(result)
	(inst fldl (make-ea :dword :base sap :disp offset)))))

#!+long-float
(define-vop (%set-sap-ref-long)
  (:translate %set-sap-ref-long)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to (:eval 0))
	 (offset :scs (signed-reg) :to (:eval 0))
	 (value :scs (long-reg)))
  (:arg-types system-area-pointer signed-num long-float)
  (:results (result :scs (long-reg)))
  (:result-types long-float)
  (:generator 5
    (cond ((zerop (tn-offset value))
	   ;; Value is in ST0
	   (store-long-float (make-ea :dword :base sap :index offset))
	   (unless (zerop (tn-offset result))
	     ;; Value is in ST0 but not result.
	     (inst fstd result)))
	  (t
	   ;; Value is not in ST0.
	   (inst fxch value)
	   (store-long-float (make-ea :dword :base sap :index offset))
	   (cond ((zerop (tn-offset result))
		  ;; The result is in ST0.
		  (inst fstd value))
		 (t
		  ;; Neither value or result are in ST0
		  (unless (location= value result)
		    (inst fstd result))
		  (inst fxch value)))))))

;;; noise to convert normal lisp data objects into SAPs

(define-vop (vector-sap)
  (:translate vector-sap)
  (:policy :fast-safe)
  (:args (vector :scs (descriptor-reg) :target sap))
  (:results (sap :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 2
    (move sap vector)
    (inst add sap (- (* vector-data-offset word-bytes) other-pointer-type))))
