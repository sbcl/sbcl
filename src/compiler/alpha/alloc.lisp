;;; -*- Package: ALPHA -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;;

;;;
;;; **********************************************************************
;;;
;;; Allocation VOPs for the Alpha port.
;;;
;;; Written by William Lott.
;;; Converted by Sean Hallgren.
;;; 

(in-package "SB!VM")



;;;; LIST and LIST*

(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
	      res)
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:generator 0
    (cond ((zerop num)
	   (move null-tn result))
	  ((and star (= num 1))
	   (move (tn-ref-tn things) result))
	  (t
	   (macrolet
	       ((store-car (tn list &optional (slot cons-car-slot))
		  `(let ((reg
			  (sc-case ,tn
			    ((any-reg descriptor-reg) ,tn)
			    (zero zero-tn)
			    (null null-tn)
			    (control-stack
			     (load-stack-tn temp ,tn)
			     temp))))
		     (storew reg ,list ,slot list-pointer-type))))
	     (let ((cons-cells (if star (1- num) num)))
	       (pseudo-atomic (:extra (* (pad-data-block cons-size)
					 cons-cells))
		 (inst bis alloc-tn list-pointer-type res)
		 (move res ptr)
		 (dotimes (i (1- cons-cells))
		   (store-car (tn-ref-tn things) ptr)
		   (setf things (tn-ref-across things))
		   (inst lda ptr (pad-data-block cons-size) ptr)
		   (storew ptr ptr
			   (- cons-cdr-slot cons-size)
			   list-pointer-type))
		 (store-car (tn-ref-tn things) ptr)
		 (cond (star
			(setf things (tn-ref-across things))
			(store-car (tn-ref-tn things) ptr cons-cdr-slot))
		       (t
			(storew null-tn ptr
				cons-cdr-slot list-pointer-type)))
		 (assert (null (tn-ref-across things)))
		 (move res result))))))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))


;;;; Special purpose inline allocators.

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
	 (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) unboxed)
  (:generator 100
    (inst li (lognot lowtag-mask) ndescr)
    (inst lda boxed (fixnumize (1+ code-trace-table-offset-slot))
	  boxed-arg)
    (inst and boxed ndescr boxed)
    (inst srl unboxed-arg word-shift unboxed)
    (inst lda unboxed lowtag-mask unboxed)
    (inst and unboxed ndescr unboxed)
    (inst sll boxed (- type-bits word-shift) ndescr)
    (inst bis ndescr code-header-type ndescr)
    
    (pseudo-atomic ()
      (inst bis alloc-tn other-pointer-type result)
      (storew ndescr result 0 other-pointer-type)
      (storew unboxed result code-code-size-slot other-pointer-type)
      (storew null-tn result code-entry-points-slot other-pointer-type)
      (inst addq alloc-tn boxed alloc-tn)
      (inst addq alloc-tn unboxed alloc-tn))

    (storew null-tn result code-debug-info-slot other-pointer-type)))

(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:generator 37
    (with-fixed-allocation (result temp fdefn-type fdefn-size)
      (storew name result fdefn-name-slot other-pointer-type)
      (storew null-tn result fdefn-function-slot other-pointer-type)
      (inst li (make-fixup "undefined_tramp" :foreign) temp)
      (storew temp result fdefn-raw-addr-slot other-pointer-type))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let ((size (+ length closure-info-offset)))
      (inst li (logior (ash (1- size) type-bits) closure-header-type) temp)
      (pseudo-atomic (:extra (pad-data-block size))
	(inst bis alloc-tn function-pointer-type result)
	(storew temp result 0 function-pointer-type))
      (storew function result closure-function-slot function-pointer-type))))

;;; The compiler likes to be able to directly make value cells.
;;; 
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg null zero)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation
	(result temp value-cell-header-type value-cell-size))
    (storew value result value-cell-value-slot other-pointer-type)))


;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li unbound-marker-type result)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (pseudo-atomic (:extra (pad-data-block words))
      (inst bis alloc-tn lowtag result)
      (when type
	(inst li (logior (ash (1- words) type-bits) type) temp)
	(storew temp result 0 lowtag)))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:scs (non-descriptor-reg)) bytes)
  (:generator 6
    (inst lda bytes (* (1+ words) word-bytes) extra)
    (inst sll bytes (- type-bits 2) header)
    (inst lda header (+ (ash -2 type-bits) type) header)
    (inst srl bytes lowtag-bits bytes)
    (inst sll bytes lowtag-bits bytes)
    (pseudo-atomic ()
      (inst bis alloc-tn lowtag result)
      (storew header result 0 lowtag)
      (inst addq alloc-tn bytes alloc-tn))))
