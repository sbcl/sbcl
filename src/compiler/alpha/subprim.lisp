;;; -*- Package: ALPHA; Log: C.Log -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;

;;;
;;; **********************************************************************
;;;
;;;    Linkage information for standard static functions, and random vops.
;;;
;;; Written by William Lott.
;;; Converted by Sean Hallgren.
;;; 
(in-package "SB!VM")




;;;; Length

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg) :target ptr))
  (:arg-types list)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) ptr)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (any-reg) :type fixnum :to (:result 0) :target result)
	      count)
  (:results (result :scs (any-reg descriptor-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 50
    (move object ptr)
    (move zero-tn count)
    
    LOOP
    
    (inst cmpeq ptr null-tn temp)
    (inst bne temp done)
    
    (inst and ptr lowtag-mask temp)
    (inst xor temp list-pointer-type temp)
    (inst bne temp not-list)
    
    (loadw ptr ptr cons-cdr-slot list-pointer-type)
    (inst addq count (fixnumize 1) count)
    (inst br zero-tn loop)
    
    NOT-LIST
    (cerror-call vop done object-not-list-error ptr)
    
    DONE
    (move count result)))
       

(define-static-function length (object) :translate length)



