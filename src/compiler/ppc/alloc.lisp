;;;
;;; Written by William Lott.
;;; 

(in-package "SB!VM")


;;;; LIST and LIST*

(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
	      res)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:generator 0
    (cond ((zerop num)
	   (move result null-tn))
	  ((and star (= num 1))
	   (move result (tn-ref-tn things)))
	  (t
	   (macrolet
	       ((maybe-load (tn)
		  (once-only ((tn tn))
		    `(sc-case ,tn
		       ((any-reg descriptor-reg zero null)
			,tn)
		       (control-stack
			(load-stack-tn temp ,tn)
			temp)))))
	     (let* ((cons-cells (if star (1- num) num))
		    (alloc (* (pad-data-block cons-size) cons-cells)))
	       (pseudo-atomic (pa-flag :extra alloc)
		 (inst clrrwi res alloc-tn n-lowtag-bits)
		 (inst ori res res list-pointer-lowtag)
		 (move ptr res)
		 (dotimes (i (1- cons-cells))
		   (storew (maybe-load (tn-ref-tn things)) ptr
			   cons-car-slot list-pointer-lowtag)
		   (setf things (tn-ref-across things))
		   (inst addi ptr ptr (pad-data-block cons-size))
		   (storew ptr ptr
			   (- cons-cdr-slot cons-size)
			   list-pointer-lowtag))
		 (storew (maybe-load (tn-ref-tn things)) ptr
			 cons-car-slot list-pointer-lowtag)
		 (storew (if star
			     (maybe-load (tn-ref-tn (tn-ref-across things)))
			     null-tn)
			 ptr cons-cdr-slot list-pointer-lowtag))
	       (move result res)))))))

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
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 100
    (inst addi boxed boxed-arg (fixnumize (1+ code-trace-table-offset-slot)))
    (inst clrrwi boxed boxed n-lowtag-bits)
    (inst srwi unboxed unboxed-arg word-shift)
    (inst addi unboxed unboxed lowtag-mask)
    (inst clrrwi unboxed unboxed n-lowtag-bits)
    (pseudo-atomic (pa-flag)
      ;; Note: we don't have to subtract off the 4 that was added by
      ;; pseudo-atomic, because oring in other-pointer-lowtag just adds
      ;; it right back.
      (inst ori result alloc-tn other-pointer-lowtag)
      (inst add alloc-tn alloc-tn boxed)
      (inst add alloc-tn alloc-tn unboxed)
      (inst slwi ndescr boxed (- n-widetag-bits word-shift))
      (inst ori ndescr ndescr code-header-widetag)
      (storew ndescr result 0 other-pointer-lowtag)
      (storew unboxed result code-code-size-slot other-pointer-lowtag)
      (storew null-tn result code-entry-points-slot other-pointer-lowtag)
      (storew null-tn result code-debug-info-slot other-pointer-lowtag))))

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result pa-flag temp fdefn-widetag fdefn-size)
      (inst lr temp  (make-fixup "undefined_tramp" :foreign))
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))


(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:ignore stack-allocate-p)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let ((size (+ length closure-info-offset)))
      (pseudo-atomic (pa-flag :extra (pad-data-block size))
	(inst clrrwi. result alloc-tn n-lowtag-bits)
	(inst ori result result fun-pointer-lowtag)
	(inst lr temp (logior (ash (1- size) n-widetag-bits) closure-header-widetag))
	(storew temp result 0 fun-pointer-lowtag)))
    ;(inst lis temp (ash 18 10))
    ;(storew temp result closure-jump-insn-slot function-pointer-type)
    (storew function result closure-fun-slot fun-pointer-lowtag)))

;;; The compiler likes to be able to directly make value cells.
;;; 
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation
	(result pa-flag temp value-cell-header-widetag value-cell-size))
    (storew value result value-cell-value-slot other-pointer-lowtag)))



;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst li result unbound-marker-widetag)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 4
    (pseudo-atomic (pa-flag :extra (pad-data-block words))
      (cond ((logbitp 2 lowtag)
	     (inst ori result alloc-tn lowtag))
	    (t
	     (inst clrrwi result alloc-tn n-lowtag-bits)
	     (inst ori result  result lowtag)))
      (when type
	(inst lr temp (logior (ash (1- words) n-widetag-bits) type))
	(storew temp result 0 lowtag)))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 6
    (inst addi bytes extra (* (1+ words) n-word-bytes))
    (inst slwi header bytes (- n-widetag-bits 2))
    (inst addi header header (+ (ash -2 n-widetag-bits) type))
    (inst clrrwi bytes bytes n-lowtag-bits)
    (pseudo-atomic (pa-flag)
      (cond ((logbitp 2 lowtag)
	     (inst ori result alloc-tn lowtag))
	    (t
	     (inst clrrwi result alloc-tn n-lowtag-bits)
	     (inst ori result result lowtag)))
      (storew header result 0 lowtag)
      (inst add alloc-tn alloc-tn bytes))))
