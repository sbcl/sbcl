;;; -*- Package: ALPHA; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;

;;;
;;; **********************************************************************
;;;
;;; Compiler support for the new whizzy debugger.
;;;
;;; Written by William Lott.
;;; Converted by Sean Hallgren.
;;; 
(in-package "SB!VM")



(define-vop (debug-cur-sp)
  (:translate current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move csp-tn res)))

(define-vop (debug-cur-fp)
  (:translate current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer)
  (:generator 1
    (move cfp-tn res)))

(define-vop (read-control-stack)
  (:translate stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (any-reg)))
  (:arg-types system-area-pointer positive-fixnum)
  (:temporary (:scs (sap-reg) :from :eval) sap)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst addq object offset sap)
    (inst ldl result 0 sap)))

(define-vop (read-control-stack-c)
  (:translate stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg)))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 14)))
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 4
    (inst ldl result (* offset word-bytes) object)))

(define-vop (write-control-stack)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (object :scs (sap-reg) :target sap)
	 (offset :scs (any-reg))
	 (value :scs (descriptor-reg) :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:temporary (:scs (sap-reg) :from (:argument 1)) sap)
  (:generator 2
    (inst addq object offset sap)
    (inst stl value 0 sap)
    (move value result)))

(define-vop (write-control-stack-c)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
	 (value :scs (descriptor-reg) :target result))
  (:info offset)
  (:arg-types system-area-pointer (:constant (signed-byte 14)) *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 1
    (inst stl value (* offset word-bytes) sap)
    (move value result)))


(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg)))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:variant-vars lowtag)
  (:generator 5
    (let ((bogus (gen-label))
	  (done (gen-label)))
      (loadw temp thing 0 lowtag)
      (inst srl temp sb!vm:type-bits temp)
      (inst beq temp bogus)
      (inst sll temp (1- (integer-length sb!vm:word-bytes)) temp)
      (unless (= lowtag sb!vm:other-pointer-type)
	(inst subq temp (- sb!vm:other-pointer-type lowtag) temp))
      (inst subq thing temp code)
      (emit-label done)
      (assemble (*elsewhere*)
	(emit-label bogus)
	(move null-tn code)
	(inst br zero-tn done)))))

(define-vop (code-from-lra code-from-mumble)
  (:translate lra-code-header)
  (:variant sb!vm:other-pointer-type))

(define-vop (code-from-function code-from-mumble)
  (:translate function-code-header)
  (:variant sb!vm:function-pointer-type))

(define-vop (make-lisp-obj)
  (:policy :fast-safe)
  (:translate make-lisp-obj)
  (:args (value :scs (unsigned-reg) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)))
  (:generator 1
    (move value result)))

(define-vop (get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg) :target result))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 1
    (move thing result)))

(define-vop (function-word-offset)
  (:policy :fast-safe)
  (:translate function-word-offset)
  (:args (fun :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (loadw res fun 0 function-pointer-type)
    (inst srl res sb!vm:type-bits res)))




(defknown make-number-stack-pointer ((unsigned-byte 32)) system-area-pointer
  (movable foldable flushable))

(define-vop (make-number-stack-pointer)
  (:policy :fast-safe)
  (:translate make-number-stack-pointer)
  (:args (arg :scs (unsigned-reg) :to (:argument 1)))
  (:arg-types unsigned-num)
  (:results (res :scs (sap-reg) :from (:argument 0)))
  (:result-types system-area-pointer)
  (:generator 5
    (inst mskll nsp-tn 0 res)
    (inst bis res arg res)))
