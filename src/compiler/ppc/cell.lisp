;;;; the VM definition of various primitive memory access VOPs for the
;;;; PPC

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; Data object ref/set stuff.

(define-vop (slot)
  (:args (object :scs (descriptor-reg)))
  (:info name offset lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (loadw result object offset lowtag)))

(define-vop (set-slot)
  (:args (object :scs (descriptor-reg))
	 (value :scs (descriptor-reg any-reg)))
  (:info name offset lowtag)
  (:ignore name)
  (:results)
  (:generator 1
    (storew value object offset lowtag)))



;;;; Symbol hacking VOPs:

;;; The compiler likes to be able to directly SET symbols.
;;;
(define-vop (set cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

;;; Do a cell ref with an error check for being unbound.
;;;
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
;;;
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (move obj-temp object)
    (loadw value obj-temp symbol-value-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop unbound-symbol-error obj-temp)))
      (inst cmpwi value unbound-marker-widetag)
      (inst beq err-lab))))

;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value))

(define-vop (boundp boundp-frob)
  (:translate boundp)
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-lowtag)
    (inst cmpwi value unbound-marker-widetag)
    (inst b? (if not-p :eq :ne) target)))

(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:policy :fast)
  (:translate symbol-value))


;;;; Fdefinition (fdefn) objects.

(define-vop (fdefn-fun cell-ref)
  (:variant fdefn-fun-slot other-pointer-lowtag))

(define-vop (safe-fdefn-fun)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp)
  (:generator 10
    (move obj-temp object)
    (loadw value obj-temp fdefn-fun-slot other-pointer-lowtag)
    (inst cmpw value null-tn)
    (let ((err-lab (generate-error-code vop undefined-fun-error obj-temp)))
      (inst beq err-lab))))

(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:translate (setf fdefn-fun))
  (:args (function :scs (descriptor-reg) :target result)
	 (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (let ((normal-fn (gen-label)))
      (load-type type function (- fun-pointer-lowtag))
      (inst cmpwi type simple-fun-header-widetag)
      ;;(inst mr lip function)
      (inst addi lip function
	    (- (ash simple-fun-code-offset word-shift) fun-pointer-lowtag))
      (inst beq normal-fn)
      (inst lr lip  (make-fixup (extern-alien-name "closure_tramp") :foreign))
      (emit-label normal-fn)
      (storew lip fdefn fdefn-raw-addr-slot other-pointer-lowtag)
      (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
      (move result function))))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (storew null-tn fdefn fdefn-fun-slot other-pointer-lowtag)
    (inst lr temp  (make-fixup (extern-alien-name "undefined_tramp") :foreign))
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result fdefn)))



;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.

(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
	 (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst addi bsp-tn bsp-tn (* 2 n-word-bytes))
    (storew temp bsp-tn (- binding-value-slot binding-size))
    (storew symbol bsp-tn (- binding-symbol-slot binding-size))
    (storew val symbol symbol-value-slot other-pointer-lowtag)))


(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
    (loadw value bsp-tn (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))
    (inst subi bsp-tn bsp-tn (* 2 n-word-bytes))))


(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (let ((loop (gen-label))
	  (skip (gen-label))
	  (done (gen-label)))
      (move where arg)
      (inst cmpw where bsp-tn)
      (inst beq done)

      (emit-label loop)
      (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
      (inst cmpwi symbol 0)
      (inst beq skip)
      (loadw value bsp-tn (- binding-value-slot binding-size))
      (storew value symbol symbol-value-slot other-pointer-lowtag)
      (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))

      (emit-label skip)
      (inst subi bsp-tn bsp-tn (* 2 n-word-bytes))
      (inst cmpw where bsp-tn)
      (inst bne loop)

      (emit-label done))))



;;;; Closure indexing.

(define-vop (closure-index-ref word-index-ref)
  (:variant closure-info-offset fun-pointer-lowtag)
  (:translate %closure-index-ref))

(define-vop (funcallable-instance-info word-index-ref)
  (:variant funcallable-instance-info-offset fun-pointer-lowtag)
  (:translate %funcallable-instance-info))

(define-vop (set-funcallable-instance-info word-index-set)
  (:variant funcallable-instance-info-offset fun-pointer-lowtag)
  (:translate %set-funcallable-instance-info))

(define-vop (funcallable-instance-lexenv cell-ref)
  (:variant funcallable-instance-lexenv-slot fun-pointer-lowtag))


(define-vop (closure-ref slot-ref)
  (:variant closure-info-offset fun-pointer-lowtag))

(define-vop (closure-init slot-set)
  (:variant closure-info-offset fun-pointer-lowtag))


;;;; Value Cell hackery.

(define-vop (value-cell-ref cell-ref)
  (:variant value-cell-value-slot other-pointer-lowtag))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-lowtag))



;;;; Instance hackery:

(define-vop (instance-length)
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw temp struct 0 instance-pointer-lowtag)
    (inst srwi res temp n-widetag-bits)))

(define-vop (instance-ref slot-ref)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:policy :fast-safe)
  (:translate %instance-ref)
  (:arg-types * (:constant index)))

#+nil
(define-vop (instance-set slot-set)
  (:policy :fast-safe)
  (:translate %instance-set)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance (:constant index) *))

(define-vop (instance-index-ref word-index-ref)
  (:policy :fast-safe) 
  (:translate %instance-ref)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance positive-fixnum))

(define-vop (instance-index-set word-index-set)
  (:policy :fast-safe) 
  (:translate %instance-set)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance positive-fixnum *))




;;;; Code object frobbing.

(define-vop (code-header-ref word-index-ref)
  (:translate code-header-ref)
  (:policy :fast-safe)
  (:variant 0 other-pointer-lowtag))

(define-vop (code-header-set word-index-set)
  (:translate code-header-set)
  (:policy :fast-safe)
  (:variant 0 other-pointer-lowtag))

