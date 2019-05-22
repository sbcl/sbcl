;;;; various primitive memory access VOPs for the RISC-V VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

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

(define-vop (init-slot set-slot)
  (:info name dx-p offset lowtag)
  (:ignore name dx-p))

;;;; Symbol hacking VOPs:

;;; The compiler likes to be able to directly SET symbols.
(define-vop (set cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

;;; Do a cell ref with an error check for being unbound.
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :to :save))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (non-descriptor-reg)) temp))

;;; With Symbol-Value, we check that the value isn't the trap object.
;;; So Symbol-Value of NIL is NIL.
(define-vop (symbol-value checked-cell-ref)
  (:translate symeval)
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error object)))
      (inst xori temp value unbound-marker-widetag)
      (inst beq temp zero-tn err-lab))))

;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value)
  (:temporary (:scs (non-descriptor-reg)) temp))

(define-vop (boundp boundp-frob)
  (:translate boundp)
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-lowtag)
    (inst xori temp value unbound-marker-widetag)
    (if not-p
        (inst beq temp zero-tn target)
        (inst bne temp zero-tn target))))

(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:policy :fast)
  (:translate symeval))

;;; On unithreaded builds these are just copies of the non-global versions.
(define-vop (%set-symbol-global-value set))
(define-vop (symbol-global-value symbol-value)
  (:translate sym-global-val))
(define-vop (fast-symbol-global-value fast-symbol-value)
  (:translate sym-global-val))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    ;; The symbol-hash slot of NIL holds NIL because it is also the
    ;; cdr slot, so we have to strip off the two low bits to make
    ;; sure it is a fixnum.  The lowtag selection magic that is
    ;; required to ensure this is explained in the comment in
    ;; objdef.lisp
    (loadw temp symbol symbol-hash-slot other-pointer-lowtag)
    (inst andi res temp (lognot fixnum-tag-mask))))

;;;; Fdefinition (fdefn) objects.
(define-vop (fdefn-fun cell-ref)
  (:variant fdefn-fun-slot other-pointer-lowtag))

(define-vop (safe-fdefn-fun)
  (:translate safe-fdefn-fun)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to :save))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 10
    (loadw value object fdefn-fun-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop 'undefined-fun-error object)))
      (inst beq value null-tn err-lab))))

(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:translate (setf fdefn-fun))
  (:args (function :scs (descriptor-reg) :target result)
         (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 3
    (load-type type function (- fun-pointer-lowtag))
    (inst xori type type simple-fun-widetag)
    (move lip function)
    (inst beq type zero-tn simple-fun)
    (inst li lip (make-fixup 'closure-tramp :assembly-routine))
    SIMPLE-FUN
    (storew lip fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
    (move result function)))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (storew null-tn fdefn fdefn-fun-slot other-pointer-lowtag)
    (inst li temp (make-fixup 'undefined-tramp :assembly-routine))
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result fdefn)))

;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
(define-vop (dynbind)
  (:args (value :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (any-reg)) bsp-temp)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (load-binding-stack-pointer bsp-temp)
    (inst addi bsp-temp bsp-temp (* binding-size n-word-bytes))
    (store-binding-stack-pointer bsp-temp)
    (storew temp bsp-temp (- binding-value-slot binding-size))
    (storew symbol bsp-temp (- binding-symbol-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)))

(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (any-reg)) bsp-temp)
  (:generator 0
    (load-binding-stack-pointer bsp-temp)
    (loadw symbol bsp-temp (- binding-symbol-slot binding-size))
    (loadw value bsp-temp (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    (storew zero-tn bsp-temp (- binding-symbol-slot binding-size))
    (storew zero-tn bsp-temp (- binding-value-slot binding-size))
    (inst subi bsp-temp bsp-temp (* binding-size n-word-bytes))
    (store-binding-stack-pointer bsp-temp)))

(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (any-reg)) bsp)
  (:generator 0
    (load-binding-stack-pointer bsp)
    (move where arg)
    (inst beq where bsp DONE)

    LOOP
    (loadw symbol bsp (- binding-symbol-slot binding-size))
    (inst beq symbol zero-tn skip)
    (loadw value bsp (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    (storew zero-tn bsp (- binding-symbol-slot binding-size))

    SKIP
    (storew zero-tn bsp (- binding-value-slot binding-size))
    (inst subi bsp bsp (* binding-size n-word-bytes))
    (inst bne where bsp loop)

    DONE
    (store-binding-stack-pointer bsp)))

;;;; Closure indexing.

(define-full-reffer closure-index-ref *
  closure-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %closure-index-ref)

(define-full-setter set-funcallable-instance-info *
  funcallable-instance-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %set-funcallable-instance-info)

(define-full-reffer funcallable-instance-info *
  funcallable-instance-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %funcallable-instance-info)

(define-vop (closure-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:info offset)
  (:generator 4
    (loadw value object (+ closure-info-offset offset) fun-pointer-lowtag)))

(define-vop (closure-init)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:info offset)
  (:generator 4
    (storew value object (+ closure-info-offset offset) fun-pointer-lowtag)))

(define-vop (closure-init-from-fp)
  (:args (object :scs (descriptor-reg)))
  (:info offset)
  (:generator 4
    (storew cfp-tn object (+ closure-info-offset offset) fun-pointer-lowtag)))

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
    #-64-bit
    (inst srli res temp n-widetag-bits)
    #+64-bit
    (let ((n-length-bits (integer-length short-header-max-words)))
      (inst slli temp temp (- n-word-bits n-length-bits n-widetag-bits))
      (inst srli res temp (- n-word-bits n-length-bits)))))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-set)

;;;; Code object frobbing.

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (descriptor-reg any-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-lowtag
  (descriptor-reg any-reg) * code-header-set)

;;;; raw instance slot accessors

(macrolet ((define-raw-slot-word-vops (name value-sc value-primtype)
             `(progn
                (define-full-reffer ,(symbolicate "RAW-INSTANCE-REF/" name) * instance-slots-offset
                  instance-pointer-lowtag (,value-sc) ,value-primtype
                  ,(symbolicate "%RAW-INSTANCE-REF/" name))
                (define-full-setter ,(symbolicate "RAW-INSTANCE-SET/" name) * instance-slots-offset
                  instance-pointer-lowtag (,value-sc) ,value-primtype
                  ,(symbolicate "%RAW-INSTANCE-SET/" name)))))
  (define-raw-slot-word-vops word unsigned-reg unsigned-num)
  (define-raw-slot-word-vops signed-word signed-reg signed-num))

(macrolet ((define-raw-slot-float-vops (name value-primtype value-sc size format &optional complexp)
             (let ((ref-vop (symbolicate "RAW-INSTANCE-REF/" name))
                   (set-vop (symbolicate "RAW-INSTANCE-SET/" name)))
               `(progn
                  (,(if complexp
                        'define-complex-float-reffer
                        'define-float-reffer)
                   ,ref-vop * ,size ,format instance-slots-offset
                   instance-pointer-lowtag (,value-sc) ,value-primtype nil "raw instance access"
                   ,(symbolicate "%" ref-vop))
                  (,(if complexp
                        'define-complex-float-setter
                        'define-float-setter)
                   ,set-vop * ,size ,format instance-slots-offset
                   instance-pointer-lowtag (,value-sc) ,value-primtype nil "raw instance store"
                   ,(symbolicate "%" set-vop))))))
  (define-raw-slot-float-vops single single-float single-reg 4 :single)
  (define-raw-slot-float-vops double double-float double-reg 8 :double)
  (define-raw-slot-float-vops complex-single complex-single-float complex-single-reg 4 :single t)
  (define-raw-slot-float-vops complex-double complex-double-float complex-double-reg 8 :double t))
