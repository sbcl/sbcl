;;;; allocation VOPs for the x86

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; LIST and LIST*

(define-vop (list-or-list*)
  (:args (things :more t))
  (:temporary (:sc unsigned-reg) ptr temp)
  (:temporary (:sc unsigned-reg :to (:result 0) :target result) res)
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :safe)
  (:node-var node)
  (:generator 0
    (cond ((zerop num)
	   ;; (move result nil-value)
	   (inst mov result nil-value))
	  ((and star (= num 1))
	   (move result (tn-ref-tn things)))
	  (t
	   (macrolet
	       ((store-car (tn list &optional (slot sb!vm:cons-car-slot))
		  `(let ((reg
			  (sc-case ,tn
			    ((any-reg descriptor-reg) ,tn)
			    ((control-stack)
			     (move temp ,tn)
			     temp))))
		     (storew reg ,list ,slot sb!vm:list-pointer-type))))
	     (let ((cons-cells (if star (1- num) num)))
	       (pseudo-atomic
		(allocation res (* (pad-data-block cons-size) cons-cells) node)
		(inst lea res
		      (make-ea :byte :base res :disp list-pointer-type))
		(move ptr res)
		(dotimes (i (1- cons-cells))
		  (store-car (tn-ref-tn things) ptr)
		  (setf things (tn-ref-across things))
		  (inst add ptr (pad-data-block cons-size))
		  (storew ptr ptr (- cons-cdr-slot cons-size)
			  list-pointer-type))
		(store-car (tn-ref-tn things) ptr)
		(cond (star
		       (setf things (tn-ref-across things))
		       (store-car (tn-ref-tn things) ptr cons-cdr-slot))
		      (t
		       (storew nil-value ptr cons-cdr-slot
			       list-pointer-type)))
		(assert (null (tn-ref-across things)))))
	     (move result res))))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))

;;;; special-purpose inline allocators

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg) :target boxed)
	 (unboxed-arg :scs (any-reg) :target unboxed))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :from :eval) temp)
  (:temporary (:sc unsigned-reg :from (:argument 0)) boxed)
  (:temporary (:sc unsigned-reg :from (:argument 1)) unboxed)
  (:generator 100
    (move boxed boxed-arg)
    (inst add boxed (fixnumize (1+ code-trace-table-offset-slot)))
    (inst and boxed (lognot lowtag-mask))
    (move unboxed unboxed-arg)
    (inst shr unboxed word-shift)
    (inst add unboxed lowtag-mask)
    (inst and unboxed (lognot lowtag-mask))
    (pseudo-atomic
     ;; comment from CMU CL code:
     ;;   now loading code into static space cause it can't move
     ;;
     ;; KLUDGE: What? What's all the cruft about saving fixups for then?
     ;; I think what's happened is that ALLOCATE-CODE-OBJECT is the basic
     ;; CMU CL primitive; this ALLOCATE-CODE-OBJECT was hacked for
     ;; static space only in a simple-minded port to the X86; and then
     ;; in an attempt to improve the port to the X86,
     ;; ALLOCATE-DYNAMIC-CODE-OBJECT was defined. If that's right, I'd like
     ;; to know why not just go back to the basic CMU CL behavior of
     ;; ALLOCATE-CODE-OBJECT, where it makes a relocatable code object.
     ;;   -- WHN 19990916
     ;;
     ;; FIXME: should have a check for overflow of static space
     (load-symbol-value temp sb!vm:*static-space-free-pointer*)
     (inst lea result (make-ea :byte :base temp :disp other-pointer-type))
     (inst add temp boxed)
     (inst add temp unboxed)
     (store-symbol-value temp sb!vm:*static-space-free-pointer*)
     (inst shl boxed (- type-bits word-shift))
     (inst or boxed code-header-type)
     (storew boxed result 0 other-pointer-type)
     (storew unboxed result code-code-size-slot other-pointer-type)
     (inst mov temp nil-value)
     (storew temp result code-entry-points-slot other-pointer-type))
    (storew temp result code-debug-info-slot other-pointer-type)))

(define-vop (allocate-dynamic-code-object)
  (:args (boxed-arg :scs (any-reg) :target boxed)
	 (unboxed-arg :scs (any-reg) :target unboxed))
  (:results (result :scs (descriptor-reg) :from :eval))
  (:temporary (:sc unsigned-reg :from (:argument 0)) boxed)
  (:temporary (:sc unsigned-reg :from (:argument 1)) unboxed)
  (:node-var node)
  (:generator 100
    (move boxed boxed-arg)
    (inst add boxed (fixnumize (1+ code-trace-table-offset-slot)))
    (inst and boxed (lognot lowtag-mask))
    (move unboxed unboxed-arg)
    (inst shr unboxed word-shift)
    (inst add unboxed lowtag-mask)
    (inst and unboxed (lognot lowtag-mask))
    (inst mov result boxed)
    (inst add result unboxed)
    (pseudo-atomic
     (allocation result result node)
     (inst lea result (make-ea :byte :base result :disp other-pointer-type))
     (inst shl boxed (- type-bits word-shift))
     (inst or boxed code-header-type)
     (storew boxed result 0 other-pointer-type)
     (storew unboxed result code-code-size-slot other-pointer-type)
     (storew nil-value result code-entry-points-slot other-pointer-type))
    (storew nil-value result code-debug-info-slot other-pointer-type)))

(define-vop (make-fdefn)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:results (result :scs (descriptor-reg) :from :argument))
  (:node-var node)
  (:generator 37
    (with-fixed-allocation (result fdefn-type fdefn-size node)
      (storew name result fdefn-name-slot other-pointer-type)
      (storew nil-value result fdefn-function-slot other-pointer-type)
      (storew (make-fixup (extern-alien-name "undefined_tramp") :foreign)
	      result fdefn-raw-addr-slot other-pointer-type))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length)
  (:temporary (:sc any-reg) temp)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 10
   (pseudo-atomic
    (let ((size (+ length closure-info-offset)))
      (allocation result (pad-data-block size) node)
      (inst lea result
	    (make-ea :byte :base result :disp function-pointer-type))
      (storew (logior (ash (1- size) type-bits) closure-header-type)
	      result 0 function-pointer-type))
    (loadw temp function closure-function-slot function-pointer-type)
    (storew temp result closure-function-slot function-pointer-type))))

;;; The compiler likes to be able to directly make value cells.
(define-vop (make-value-cell)
  (:args (value :scs (descriptor-reg any-reg) :to :result))
  (:results (result :scs (descriptor-reg) :from :eval))
  (:node-var node)
  (:generator 10
    (with-fixed-allocation
	(result value-cell-header-type value-cell-size node))
    (storew value result value-cell-value-slot other-pointer-type)))

;;;; automatic allocators for primitive objects

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (inst mov result unbound-marker-type)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 50
    (pseudo-atomic
     (allocation result (pad-data-block words) node)
     (inst lea result (make-ea :byte :base result :disp lowtag))
     (when type
       (storew (logior (ash (1- words) type-bits) type) result 0 lowtag)))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg) :from (:eval 1)))
  (:temporary (:sc any-reg :from :eval :to (:eval 1)) bytes)
  (:temporary (:sc any-reg :from :eval :to :result) header)
  (:node-var node)
  (:generator 50
    (inst lea bytes
	  (make-ea :dword :base extra :disp (* (1+ words) word-bytes)))
    (inst mov header bytes)
    (inst shl header (- type-bits 2))	; w+1 to length field

    (inst lea header			; (w-1 << 8) | type
	  (make-ea :dword :base header :disp (+ (ash -2 type-bits) type)))
    (inst and bytes (lognot lowtag-mask))
    (pseudo-atomic
     (allocation result bytes node)
     (inst lea result (make-ea :byte :base result :disp lowtag))
     (storew header result 0 lowtag))))

(define-vop (make-symbol)
  (:policy :fast-safe)
  (:translate make-symbol)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:sc unsigned-reg :from :eval) temp)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:node-var node)
  (:generator 37
    (with-fixed-allocation (result symbol-header-type symbol-size node)
      (storew name result symbol-name-slot other-pointer-type)
      (storew unbound-marker-type result symbol-value-slot other-pointer-type)
      ;; Set up a random hash value for the symbol. Perhaps the object
      ;; address could be used for even faster and smaller code!
      ;; FIXME: We don't mind the symbol hash not being repeatable, so
      ;; we might as well add in the object address here, too. (Adding entropy
      ;; is good, even if ANSI doesn't understand that.)
      (inst imul temp
	    (make-fixup (extern-alien-name "fast_random_state") :foreign)
	    1103515245)
      (inst add temp 12345)
      (inst mov (make-fixup (extern-alien-name "fast_random_state") :foreign)
	    temp)
      ;; We want a positive fixnum for the hash value, so discard the LS bits.
      (inst shr temp 1)
      (inst and temp #xfffffffc)
      (storew temp result symbol-hash-slot other-pointer-type)
      (storew nil-value result symbol-plist-slot other-pointer-type)
      (storew nil-value result symbol-package-slot other-pointer-type))))
