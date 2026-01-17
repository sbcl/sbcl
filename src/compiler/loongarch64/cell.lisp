;;;; various primitive memory access VOPs for the LoongArch VM

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
         (value :scs (descriptor-reg any-reg zero)))
  (:info name offset lowtag)
  (:ignore name)
  (:results)
  (:generator 1
    (storew value object offset lowtag)))

(define-vop (compare-and-swap-slot)
  (:args (object :scs (descriptor-reg))
         (old :scs (descriptor-reg any-reg))
         (new :scs (descriptor-reg any-reg)))
  (:temporary (:sc non-descriptor-reg) temp t1 t2)
  (:info name offset lowtag)
  (:ignore name)
  (:temporary (:sc interior-reg) lip)
  (:results (result :scs (descriptor-reg) :from :load))
  (:generator 5
    (inst addi.d lip object (- (* offset n-word-bytes) lowtag))
    (inst li t2 2)
    (inst cpucfg t1 t2)
    (inst li t2 #x20000000)
    (inst and temp t1 t2)
    (inst beq temp zero-tn LOOPF)
   LOOP
    (inst llacq.d result lip)
    (inst bne result old EXIT)
    (inst move temp new)
    (inst screl.d temp lip)
    (inst beq temp zero-tn LOOP)
    (inst j EXIT)
   LOOPF
    (inst dbar #x14)
    (inst ll.d result lip 0)
    (inst bne result old EXIT)
    (inst move temp new)
    (inst sc.d temp lip 0)
    (inst beq temp zero-tn LOOPF)
    (inst dbar #x12)
   EXIT))


;;;; Symbol hacking VOPs:
(define-vop (%compare-and-swap-symbol-value)
  (:translate %compare-and-swap-symbol-value)
  (:args (symbol :scs (descriptor-reg))
         (old :scs (descriptor-reg any-reg))
         (new :scs (descriptor-reg any-reg)))
  (:temporary (:sc non-descriptor-reg) temp t1 t2)
  (:temporary (:sc interior-reg) lip)
  (:results (result :scs (descriptor-reg any-reg)
                    :from :load))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 15
    #+sb-thread
    (assemble ()
      (load-tls-index temp symbol)
      (inst add.d lip thread-base-tn temp)
      (loadw result lip)
      (inst bne result old DONT-STORE-TLS)
      (storew new lip)
      DONT-STORE-TLS
      (inst s_xori temp result no-tls-value-marker)
      (inst bne temp zero-tn CHECK-UNBOUND))
    (inst addi.d lip symbol (- (* symbol-value-slot n-word-bytes)
                             other-pointer-lowtag))
    (inst li t2 2)
    (inst cpucfg t1 t2)
    (inst li t2 #x20000000)
    (inst and temp t1 t2)
    (inst beq temp zero-tn LOOPF)
    LOOP
    (inst llacq.d result lip)
    (inst bne result old CHECK-UNBOUND)
    (inst move temp new)
    (inst screl.d temp lip)
    (inst beq temp zero-tn LOOP)
    (inst j CHECK-UNBOUND)
    LOOPF
    (inst dbar #x14)
    (inst ll.d result lip 0)
    (inst bne result old CHECK-UNBOUND)
    (inst move temp new)
    (inst sc.d temp lip 0)
    (inst beq temp zero-tn LOOPF)
    (inst dbar #x12)

    CHECK-UNBOUND
    (inst s_xori temp result unbound-marker-widetag)
    (inst beq temp zero-tn (generate-error-code vop 'unbound-symbol-error symbol))))

;;; The compiler likes to be able to directly SET symbols.
#+sb-thread
(define-vop (set)
  (:args (symbol :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc any-reg) tls-slot)
  (:temporary (:sc interior-reg) lip)
  (:generator 4
    (load-tls-index tls-slot symbol)
    (inst add.d lip thread-base-tn tls-slot)
    (loadw temp lip)
    (inst s_xori temp temp no-tls-value-marker)
    (inst bne temp zero-tn TLS-VALUE)
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    (inst j DONE)
    TLS-VALUE
    (storew value lip)
    DONE))

#-sb-thread
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
#+sb-thread
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:temporary (:scs (interior-reg)) lip)
  (:variant-vars check-boundp)
  (:variant t)
  (:generator 9
    (load-tls-index value object)
    (inst add.d lip thread-base-tn value)
    (loadw value lip)
    (inst s_xori temp value no-tls-value-marker)
    (inst bne temp zero-tn CHECK-UNBOUND)
    (loadw value object symbol-value-slot other-pointer-lowtag)
    CHECK-UNBOUND
    (when check-boundp
      (let ((err-lab (generate-error-code vop 'unbound-symbol-error object)))
        (inst s_xori temp value unbound-marker-widetag)
        (inst beq temp zero-tn err-lab)))))

#-sb-thread
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error object)))
      (inst s_xori temp value unbound-marker-widetag)
      (inst beq temp zero-tn err-lab))))

(define-vop (boundp)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value)
  #+sb-thread (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate boundp)
  #+sb-thread
  (:generator 9
    (load-tls-index value object)
    (inst add.d lip thread-base-tn value)
    (loadw value lip)
    (inst s_xori temp value no-tls-value-marker)
    (inst bne temp zero-tn CHECK-UNBOUND)
    (loadw value object symbol-value-slot other-pointer-lowtag)
    CHECK-UNBOUND
    (inst s_xori temp value unbound-marker-widetag)
    (if not-p
        (inst beq temp zero-tn target)
        (inst bne temp zero-tn target)))
  #-sb-thread
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-lowtag)
    (inst s_xori temp value unbound-marker-widetag)
    (if not-p
        (inst beq temp zero-tn target)
        (inst bne temp zero-tn target))))

#+sb-thread
(define-vop (fast-symbol-value symbol-value)
  (:policy :fast)
  (:variant nil)
  (:variant-cost 7))

#-sb-thread
(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:policy :fast)
  (:translate symbol-value))

(define-vop (%set-symbol-global-value cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

(define-vop (fast-symbol-global-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:policy :fast)
  (:translate symbol-global-value))

(define-vop (symbol-global-value)
  (:policy :fast-safe)
  (:translate symbol-global-value)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:temporary (:sc non-descriptor-reg) tmp)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 9
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error object)))
      (loadw value object symbol-value-slot other-pointer-lowtag)
      (inst li tmp unbound-marker-widetag)
      (inst beq value tmp err-lab))))

(progn
(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst srli.d res res n-symbol-hash-discard-bits)))
(define-vop (symbol-name-hash symbol-hash)
  (:translate symbol-name-hash hash-as-if-symbol-name)
  (:generator 1
    (inst ld.wu res symbol (- (+ 4 (ash symbol-hash-slot word-shift)) other-pointer-lowtag))))

(define-vop ()
  (:args (symbol :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:translate symbol-package-id)
  (:policy :fast-safe)
  (:generator 1
   (inst ld.hu result symbol (- 2 other-pointer-lowtag)))))

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
  (:args (function :scs (descriptor-reg))
         (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:generator 3
    (load-type type function (- fun-pointer-lowtag))
    (inst s_xori type type simple-fun-widetag)
    (move lip function)
    (inst beq type zero-tn simple-fun)
    (inst li lip (make-fixup 'closure-tramp :assembly-routine))
    SIMPLE-FUN
    (storew lip fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (storew function fdefn fdefn-fun-slot other-pointer-lowtag)))


(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 38
    (storew null-tn fdefn fdefn-fun-slot other-pointer-lowtag)
    (inst li temp (make-fixup 'undefined-tramp :assembly-routine))
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)))


#+sb-thread
(define-vop (dynbind)
  (:args (value :scs (any-reg descriptor-reg zero))
         (symbol :scs (descriptor-reg) :target alloc-tls-symbol))
  (:temporary (:scs (descriptor-reg) :offset l0-offset) alloc-tls-symbol)
  (:temporary (:scs (descriptor-reg) :offset l1-offset) value-temp)
  (:temporary (:scs (non-descriptor-reg) :offset nl0-offset) tls-index)
  (:temporary (:scs (non-descriptor-reg) :offset nl1-offset) bsp-temp)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
     (load-tls-index tls-index symbol)
     (inst bne tls-index zero-tn TLS-VALID)
     (move alloc-tls-symbol symbol)
     (inst jal lip (make-fixup 'alloc-tls-index :assembly-routine))
     TLS-VALID
     (inst add.d lip thread-base-tn tls-index)
     (loadw value-temp lip)
     (storew value lip)
     (load-binding-stack-pointer bsp-temp)
     (inst addi.d bsp-temp bsp-temp (* binding-size n-word-bytes))
     (store-binding-stack-pointer bsp-temp)
     (storew value-temp bsp-temp (- binding-value-slot binding-size))
     (storew tls-index bsp-temp (- binding-symbol-slot binding-size))))

#-sb-thread
(define-vop (dynbind)
  (:args (value :scs (any-reg descriptor-reg zero))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (any-reg)) bsp-temp)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (load-binding-stack-pointer bsp-temp)
    (inst addi.d bsp-temp bsp-temp (* binding-size n-word-bytes))
    (store-binding-stack-pointer bsp-temp)
    (storew temp bsp-temp (- binding-value-slot binding-size))
    (storew symbol bsp-temp (- binding-symbol-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)))

#+sb-thread
(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) value)
  (:temporary (:scs (any-reg)) tls-index bsp-temp)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 0
    (load-binding-stack-pointer bsp-temp)
    (loadw tls-index bsp-temp (- binding-symbol-slot binding-size))
    (loadw value bsp-temp (- binding-value-slot binding-size))
    (inst add.d lip thread-base-tn tls-index)
    (storew value lip)
    (storew zero-tn bsp-temp (- binding-symbol-slot binding-size))
    (storew zero-tn bsp-temp (- binding-value-slot binding-size))
    (inst subi bsp-temp bsp-temp (* binding-size n-word-bytes))
    (store-binding-stack-pointer bsp-temp)))

#-sb-thread
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
  #+sb-thread
  (:temporary (:scs (interior-reg)) lip)
  (:generator 0
    (load-binding-stack-pointer bsp)
    (move where arg)
    (inst beq where bsp DONE)

    LOOP
    (loadw symbol bsp (- binding-symbol-slot binding-size))
    (inst beq symbol zero-tn skip)
    (loadw value bsp (- binding-value-slot binding-size))
    #-sb-thread
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    #+sb-thread
    (progn
      (inst add.d lip thread-base-tn symbol)
      (storew value lip))
    (storew zero-tn bsp (- binding-symbol-slot binding-size))

    SKIP
    (storew zero-tn bsp (- binding-value-slot binding-size))
    (inst subi bsp bsp (* binding-size n-word-bytes))
    (inst bne where bsp loop)

    DONE
    (store-binding-stack-pointer bsp)))

(define-full-reffer closure-index-ref *
  closure-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %closure-index-ref)

(define-full-setter %closure-index-set *
  closure-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg zero) * %closure-index-set)

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
  (:info offset dx)
  (:ignore dx)
  (:generator 4
    (storew value object (+ closure-info-offset offset) fun-pointer-lowtag)))

(define-vop (closure-init-from-fp)
  (:args (object :scs (descriptor-reg)))
  (:info offset)
  (:generator 4
    (storew cfp-tn object (+ closure-info-offset offset) fun-pointer-lowtag)))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-lowtag))

(define-vop ()
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw res struct 0 instance-pointer-lowtag)
    (inst srli.d res res instance-length-shift)))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg zero) * %instance-set)

(define-full-casser instance-index-cas * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-cas)

;;;; Code object frobbing.
(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (descriptor-reg any-reg) * code-header-ref)

(define-vop (code-header-set)
  (:translate code-header-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (any-reg descriptor-reg zero)))
  (:arg-types * tagged-num *)
  (:temporary (:scs (non-descriptor-reg)) temp card)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:generator 10
    (inst li temp (make-fixup "gc_card_table_mask" :foreign-dataref))
    (loadw temp temp) ; address of gc_card_table_mask
    (inst ld.wu temp temp 0) ; value of gc_card_table_mask (4-byte int)
    (pseudo-atomic (pa-flag)
      ;; Compute card mark index
      (inst srli.d card object gencgc-card-shift)
      (inst and card card temp)
      ;; Load mark table base
      (inst li temp (make-fixup "gc_card_mark" :foreign-dataref)) ; address of linkage entry
      (loadw temp temp) ; address of gc_card_mark
      (loadw temp temp) ; value of gc_card_mark (pointer)
      ;; Touch the card mark byte.
      (inst add.d temp temp card)
      (inst st.b null-tn temp 0)
      ;; set 'written' flag in the code header
      ;; If two threads get here at the same time, they'll write the same byte.
      (let ((byte (- 3 other-pointer-lowtag)))
        (inst ld.bu temp object byte)
        (inst s_ori temp temp #x40)
        (inst st.b temp object byte))
      ;; No need for LIP register because this is pseudo-atomic
      (inst slli.d temp index (- word-shift n-fixnum-tag-bits))
      (inst add.d temp object temp)
      (inst st.d value temp (- other-pointer-lowtag)))))


(macrolet ((define-raw-slot-word-vops (name value-sc value-primtype)
             `(progn
                (define-full-reffer ,(symbolicate "%RAW-INSTANCE-REF/" name) * instance-slots-offset
                  instance-pointer-lowtag (,value-sc) ,value-primtype
                  ,(symbolicate "%RAW-INSTANCE-REF/" name))
                (define-full-setter ,(symbolicate "%RAW-INSTANCE-SET/" name) * instance-slots-offset
                  instance-pointer-lowtag (,value-sc) ,value-primtype
                  ,(symbolicate "%RAW-INSTANCE-SET/" name))
                (define-full-casser ,(symbolicate "RAW-INSTANCE-CAS/" name) instance instance-slots-offset
                  instance-pointer-lowtag (,value-sc) ,value-primtype
                  ,(symbolicate "%RAW-INSTANCE-CAS/" name)))))
  (define-raw-slot-word-vops word unsigned-reg unsigned-num)
  (define-raw-slot-word-vops signed-word signed-reg signed-num))

(macrolet ((define-raw-slot-float-vops (name value-primtype value-sc size format &optional complexp)
             (let ((ref-vop (symbolicate "%RAW-INSTANCE-REF/" name))
                   (set-vop (symbolicate "%RAW-INSTANCE-SET/" name)))
               `(progn
                  (,(if complexp
                        'define-complex-float-reffer
                        'define-float-reffer)
                   ,ref-vop * ,size ,format instance-slots-offset
                   instance-pointer-lowtag (,value-sc) ,value-primtype nil "raw instance access"
                   ,ref-vop)
                  (,(if complexp
                        'define-complex-float-setter
                        'define-float-setter)
                   ,set-vop * ,size ,format instance-slots-offset
                   instance-pointer-lowtag (,value-sc) ,value-primtype nil "raw instance store"
                   ,set-vop)))))
  (define-raw-slot-float-vops single single-float single-reg 4 :single)
  (define-raw-slot-float-vops double double-float double-reg 8 :double)
  (define-raw-slot-float-vops complex-single complex-single-float complex-single-reg 4 :single t)
  (define-raw-slot-float-vops complex-double complex-double-float complex-double-reg 8 :double t))

(define-atomic-frobber raw-instance-incf/word amadd_db.d * instance-slots-offset
  instance-pointer-lowtag (unsigned-reg) unsigned-num  %raw-instance-atomic-incf/word)
