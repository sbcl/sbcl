;;;; the VM definition of various primitive memory access VOPs for the
;;;; ARM

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
  (:translate symeval)
  (:generator 9
    (move obj-temp object)
    (loadw value obj-temp symbol-value-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error obj-temp)))
      (inst cmp value unbound-marker-widetag)
      (inst b :eq err-lab))))

;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value)
  (:translate boundp)
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-lowtag)
    (inst cmp value unbound-marker-widetag)
    (inst b (if not-p :eq :ne) target)))

(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:policy :fast)
  (:translate symeval))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    ;; The symbol-hash slot of NIL holds NIL because it is also the
    ;; car slot, so we have to strip off the two low bits to make sure
    ;; it is a fixnum.  The lowtag selection magic that is required to
    ;; ensure this is explained in the comment in objdef.lisp
    (loadw temp symbol symbol-hash-slot other-pointer-lowtag)
    (inst bic res temp fixnum-tag-mask)))

;;; On unithreaded builds these are just copies of the non-global versions.
(define-vop (%set-symbol-global-value set))
(define-vop (symbol-global-value symbol-value)
  (:translate sym-global-val))
(define-vop (fast-symbol-global-value fast-symbol-value)
  (:translate sym-global-val))

;;;; Fdefinition (fdefn) objects.

(define-vop (fdefn-fun cell-ref)
  (:variant fdefn-fun-slot other-pointer-lowtag))

(define-vop (safe-fdefn-fun)
  (:translate safe-fdefn-fun)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp)
  (:generator 10
    (move obj-temp object)
    (loadw value obj-temp fdefn-fun-slot other-pointer-lowtag)
    (inst cmp value null-tn)
    (let ((err-lab (generate-error-code vop 'undefined-fun-error obj-temp)))
      (inst b :eq err-lab))))

(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:translate (setf fdefn-fun))
  (:args (function :scs (descriptor-reg) :target result)
         (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (let ((closure-tramp-fixup (gen-label)))
      (assemble (:elsewhere)
        (emit-label closure-tramp-fixup)
        (inst word (make-fixup 'closure-tramp :assembly-routine)))
      (load-type type function (- fun-pointer-lowtag))
      (inst cmp type simple-fun-widetag)
      (inst mov :eq lip function)
      (inst load-from-label :ne lip lip closure-tramp-fixup)
      (storew lip fdefn fdefn-raw-addr-slot other-pointer-lowtag)
      (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
      (move result function))))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 38
    (let ((undefined-tramp-fixup (gen-label)))
      (assemble (:elsewhere)
        (emit-label undefined-tramp-fixup)
        (inst word (make-fixup 'undefined-tramp :assembly-routine)))
      (storew null-tn fdefn fdefn-fun-slot other-pointer-lowtag)
      (inst load-from-label temp lip undefined-tramp-fixup)
      (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag))))


;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.

(define-vop (dynbind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) value-temp)
  (:temporary (:scs (any-reg)) bsp-temp)
  (:generator 5
    (loadw value-temp symbol symbol-value-slot other-pointer-lowtag)
    (load-symbol-value bsp-temp *binding-stack-pointer*)
    (inst add bsp-temp bsp-temp (* 2 n-word-bytes))
    (store-symbol-value bsp-temp *binding-stack-pointer*)
    (storew value-temp bsp-temp (- binding-value-slot binding-size))
    (storew symbol bsp-temp (- binding-symbol-slot binding-size))
    (storew val symbol symbol-value-slot other-pointer-lowtag)))

(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (any-reg)) bsp-temp)
  (:temporary (:scs (any-reg)) zero-temp)
  (:generator 0
    (inst mov zero-temp 0)
    (load-symbol-value bsp-temp *binding-stack-pointer*)
    (loadw symbol bsp-temp (- binding-symbol-slot binding-size))
    (loadw value bsp-temp (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    (storew zero-temp bsp-temp (- binding-symbol-slot binding-size))
    (storew zero-temp bsp-temp (- binding-value-slot binding-size))
    (inst sub bsp-temp bsp-temp (* 2 n-word-bytes))
    (store-symbol-value bsp-temp *binding-stack-pointer*)))

(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (any-reg)) bsp-temp zero-temp)
  (:generator 0
    (load-symbol-value bsp-temp *binding-stack-pointer*)
    (inst mov zero-temp 0)
    (move where arg)
    (inst cmp where bsp-temp)
    (inst b :eq DONE)

    LOOP
    (loadw symbol bsp-temp (- binding-symbol-slot binding-size))
    (inst cmp symbol 0)
    (loadw value bsp-temp (- binding-value-slot binding-size) 0 :ne)
    (storew value symbol symbol-value-slot other-pointer-lowtag :ne)
    (storew zero-temp bsp-temp (- binding-symbol-slot binding-size) 0 :ne)

    (storew zero-temp bsp-temp (- binding-value-slot binding-size))
    (inst sub bsp-temp bsp-temp (* 2 n-word-bytes))
    (inst cmp where bsp-temp)
    (inst b :ne LOOP)

    DONE
    (store-symbol-value bsp-temp *binding-stack-pointer*)))

;;;; Closure indexing.

(define-full-reffer closure-index-ref *
  closure-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %closure-index-ref)

(define-full-setter %closure-index-set *
  closure-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg null) * %closure-index-set)

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

(define-vop ()
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw res struct 0 instance-pointer-lowtag)
    (inst mov res (lsr res instance-length-shift))))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg null) * %instance-set)

;;;; Code object frobbing.

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (descriptor-reg any-reg) * code-header-ref)

(define-vop (code-header-set)
  (:translate code-header-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (any-reg descriptor-reg)))
  (:arg-types * tagged-num *)
  (:temporary (:scs (non-descriptor-reg)) temp #+gencgc card)
  #+gencgc (:temporary (:scs (interior-reg)) lip)
  #+gencgc (:temporary (:sc non-descriptor-reg) pa-flag)
  (:generator 10
    #+cheneygc
    (progn  (inst sub temp index other-pointer-lowtag)
            (inst str value (@ object temp)))
    #+gencgc
    (let ((mask-fixup-label (gen-label))
          (table-fixup-label (gen-label)))
      (inst load-from-label temp lip mask-fixup-label)
      (inst ldr temp (@ temp))
      (inst ldr temp (@ temp))
      (pseudo-atomic (pa-flag)
        ;; Compute card mark index
        (inst mov card (lsr object gencgc-card-shift))
        (inst and card card temp)
        ;; Load mark table base
        (inst load-from-label temp lip table-fixup-label)
        (inst ldr temp (@ temp))
        (inst ldr temp (@ temp))
        ;; Touch the card mark byte.
        (inst mov lip 0)
        (inst strb lip (@ temp card))
        ;; set 'written' flag in the code header
        ;; If two threads get here at the same time, they'll write the same byte.
        (let ((byte (- #+little-endian 3 other-pointer-lowtag)))
          (inst ldrb temp (@ object byte))
          (inst orr temp temp #x40)
          (inst strb temp (@ object byte)))
        (inst sub temp index other-pointer-lowtag)
        (inst str value (@ object temp)))
      (assemble (:elsewhere)
        (emit-label mask-fixup-label)
        (inst word (make-fixup "gc_card_table_mask" :foreign-dataref))
        (emit-label table-fixup-label)
        (inst word (make-fixup "gc_card_mark" :foreign-dataref))))))

;;;; raw instance slot accessors

(macrolet
    ((define-raw-slot-vops (name ref-inst set-inst value-primtype value-sc
                                 &key use-lip (move-macro 'move))
       (labels ((emit-generator (instruction move-result)
                  `((inst add offset index
                          (- (* instance-slots-offset n-word-bytes)
                             instance-pointer-lowtag))
                    ,@(if use-lip
                          `((inst add lip object offset)
                            (inst ,instruction value (@ lip)))
                          `((inst ,instruction value (@ object offset))))
                    ,@(when move-result
                        `((,move-macro result value))))))
           `(progn
              (define-vop ()
                (:translate ,(symbolicate "%RAW-INSTANCE-REF/" name))
                (:policy :fast-safe)
                (:args (object :scs (descriptor-reg))
                       (index :scs (any-reg)))
                (:arg-types * positive-fixnum)
                (:results (value :scs (,value-sc)))
                (:result-types ,value-primtype)
                (:temporary (:scs (non-descriptor-reg)) offset)
                ,@(when use-lip '((:temporary (:scs (interior-reg)) lip)))
                (:generator 5 ,@(emit-generator ref-inst nil)))
              (define-vop ()
                (:translate ,(symbolicate "%RAW-INSTANCE-SET/" name))
                (:policy :fast-safe)
                (:args (object :scs (descriptor-reg))
                       (index :scs (any-reg))
                       (value :scs (,value-sc)))
                (:arg-types * positive-fixnum ,value-primtype)
                (:temporary (:scs (non-descriptor-reg)) offset)
                ,@(when use-lip '((:temporary (:scs (interior-reg)) lip)))
                (:generator 5 ,@(emit-generator set-inst nil)))))))
  (define-raw-slot-vops word ldr str unsigned-num unsigned-reg)
  (define-raw-slot-vops signed-word ldr str signed-num signed-reg)
  (define-raw-slot-vops single flds fsts single-float single-reg
                        :use-lip t :move-macro move-single)
  (define-raw-slot-vops double fldd fstd double-float double-reg
                        :use-lip t :move-macro move-double)
  (define-raw-slot-vops complex-single load-complex-single store-complex-single complex-single-float complex-single-reg
                        :use-lip t :move-macro move-complex-single)
  (define-raw-slot-vops complex-double load-complex-double store-complex-double complex-double-float complex-double-reg
                        :use-lip t :move-macro move-complex-double))
