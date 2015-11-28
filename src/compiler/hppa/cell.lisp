;;;; the VM definition of various primitive memory access VOPs for
;;;; HPPA

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
         (value :scs (descriptor-reg any-reg null zero)))
  (:info name offset lowtag)
  (:ignore name)
  (:results)
  (:generator 1
    (storew value object offset lowtag)))

(define-vop (init-slot set-slot))

;;;; Symbol hacking VOPs:

;;; The compiler likes to be able to directly SET symbols.
(define-vop (set cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

;;; Do a cell ref with an error check for being unbound.
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (move object obj-temp)
    (loadw value obj-temp symbol-value-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error obj-temp)))
      (inst li unbound-marker-widetag temp)
      (inst bc := nil value temp err-lab))))

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
    (inst li unbound-marker-widetag temp)
    (inst bc :<> not-p value temp target)))

(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:policy :fast)
  (:translate symbol-value))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    (loadw temp symbol symbol-hash-slot other-pointer-lowtag)
    (inst dep 0 31 n-fixnum-tag-bits temp)
    ;; we must go through an temporary to avoid gc
    (move temp res)))

;;; On unithreaded builds these are just copies of the non-global versions.
(define-vop (%set-symbol-global-value set))
(define-vop (symbol-global-value symbol-value)
  (:translate symbol-global-value))
(define-vop (fast-symbol-global-value fast-symbol-value)
  (:translate symbol-global-value))

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
    (let ((err-lab (generate-error-code vop 'undefined-fun-error obj-temp)))
      (inst bc := nil value null-tn err-lab))))

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
      (inst addi (- simple-fun-header-widetag) type type)
      (inst comb := type zero-tn normal-fn)
      (inst addi (- (ash simple-fun-code-offset word-shift) fun-pointer-lowtag)
            function lip)
      (inst li (make-fixup 'closure-tramp :assembly-routine) lip)
      (emit-label normal-fn)
      (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
      (storew lip fdefn fdefn-raw-addr-slot other-pointer-lowtag)
      (move function result))))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (storew null-tn fdefn fdefn-fun-slot other-pointer-lowtag)
    (inst li (make-fixup "undefined_tramp" :foreign) temp)
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move fdefn result)))


;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
;;;
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.

(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst addi (* 2 n-word-bytes) bsp-tn bsp-tn)
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
    (storew zero-tn bsp-tn (- binding-value-slot binding-size))
    (inst addi (- (* 2 n-word-bytes)) bsp-tn bsp-tn)))

(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (let ((loop (gen-label))
          (skip (gen-label))
          (done (gen-label)))
      (move arg where)
      (inst comb := where bsp-tn done :nullify t)

      (emit-label loop)
      (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
      (inst comb := symbol zero-tn skip)
      (loadw value bsp-tn (- binding-value-slot binding-size))
      (storew value symbol symbol-value-slot other-pointer-lowtag)
      (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))

      (emit-label skip)
      (storew zero-tn bsp-tn (- binding-value-slot binding-size))
      (inst addi (* -2 n-word-bytes) bsp-tn bsp-tn)
      (inst comb :<> where bsp-tn loop)
      (inst nop)
      (emit-label done))))


;;;; Closure indexing.

(define-full-reffer closure-index-ref *
  closure-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %closure-index-ref)

(define-full-setter set-funcallable-instance-info *
  funcallable-instance-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg null zero) * %set-funcallable-instance-info)

(define-full-reffer funcallable-instance-info *
  funcallable-instance-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %funcallable-instance-info)

(define-vop (closure-ref slot-ref)
  (:variant closure-info-offset fun-pointer-lowtag))

(define-vop (closure-init slot-set)
  (:variant closure-info-offset fun-pointer-lowtag))

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
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw res struct 0 instance-pointer-lowtag)
    (inst srl res n-widetag-bits res)))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg null zero) * %instance-set)



;;;; Code object frobbing.

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (descriptor-reg any-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-lowtag
  (descriptor-reg any-reg null zero) * code-header-set)


;;;; raw instance slot accessors
(macrolet ((lfloat (imm base dst &key side)
             `(cond
                ((< ,imm (ash 1 4))
                  (inst flds ,imm ,base ,dst :side ,side))
                ((and (< ,imm (ash 1 13))
                      (> ,imm 0))
                  (progn
                    (inst li ,imm offset)
                    (inst fldx offset ,base ,dst :side ,side)))
                (t
                  (error "inst fldx cant handle offset-register loaded with immediate ~s" ,imm))))
           (sfloat (src imm base &key side)
             `(cond
                ((< ,imm (ash 1 4))
                  (inst fsts ,src ,imm ,base :side ,side))
                ((and (< ,imm (ash 1 13))
                      (> ,imm 0))
                  (progn
                    (inst ldo ,imm zero-tn offset)
                    (inst fstx ,src offset ,base :side ,side)))
                (t
                  (error "inst fstx cant handle offset-register loaded with immediate ~s" ,imm))))
           (raw-instance ((type inc-offset-by set &optional complex)
                          &body body)
             (let ((name (symbolicate "RAW-INSTANCE-"
                                      (if set "SET/" "REF/")
                                      (if (eq type 'unsigned)
                                        "WORD"
                                        (or complex type))))
                   (type-num (cond
                               ((eq type 'single)
                                 (if complex 'complex-single-float
                                             'single-float))
                               ((eq type 'double)
                                 (if complex 'complex-double-float
                                             'double-float))
                               (t (symbolicate type "-NUM"))))
                   (type-reg (symbolicate (or complex type) "-REG")))
             `(define-vop (,name)
                (:translate ,(symbolicate "%" name))
                (:policy :fast-safe)
                (:args (object :scs (descriptor-reg))
                       (index :scs (any-reg))
                       ,@(if set
                           `((value :scs (,type-reg) :target result))))
                (:arg-types * positive-fixnum ,@(if set `(,type-num)))
                (:results (,(if set 'result 'value) :scs (,type-reg)))
                (:temporary (:scs (non-descriptor-reg)) offset)
                (:temporary (:scs (interior-reg)) lip)
                (:result-types ,type-num)
                (:generator 5
                  (loadw offset object 0 instance-pointer-lowtag)
                  (inst srl offset n-widetag-bits offset)
                  (inst sll offset 2 offset)
                  (inst sub offset index offset)
                  (inst addi ,(* inc-offset-by n-word-bytes)
                             offset offset)
                  (inst add offset object lip)
                  ,@body)))))
  (raw-instance (unsigned -1 nil)
    (inst ldw (- (* instance-slots-offset n-word-bytes)
              instance-pointer-lowtag) lip value))

  (raw-instance (unsigned -1 t)
    (inst stw value (- (* instance-slots-offset n-word-bytes)
                          instance-pointer-lowtag) lip)
    (move value result))

  (raw-instance (single -1 nil)
    (let ((io (- (* instance-slots-offset n-word-bytes)
                 instance-pointer-lowtag)))
      (lfloat io lip value)))

  (raw-instance (single -1 t)
    (let ((io (- (* instance-slots-offset n-word-bytes)
                 instance-pointer-lowtag)))
      (sfloat value io lip))
    (unless (location= result value)
      (inst funop :copy value result)))

  (raw-instance (double -2 nil)
    (let ((io (- (* instance-slots-offset n-word-bytes)
                 instance-pointer-lowtag)))
      (lfloat io lip value)))

  (raw-instance (double -2 t)
    (let ((io (- (* instance-slots-offset n-word-bytes)
                 instance-pointer-lowtag)))
      (sfloat value io lip :side 0))
    (let ((io (- (* (1+ instance-slots-offset) n-word-bytes)
                 instance-pointer-lowtag)))
      (sfloat value io lip :side 1))
    (unless (location= result value)
      (inst funop :copy value result)))

  (raw-instance (single -2 nil complex-single)
    (let ((io (- (* instance-slots-offset n-word-bytes)
                 instance-pointer-lowtag)))
      (lfloat io lip (complex-single-reg-real-tn value)))
    (let ((io (- (* (1+ instance-slots-offset) n-word-bytes)
                 instance-pointer-lowtag)))
      (lfloat io lip (complex-single-reg-imag-tn value))))

  (raw-instance (single -2 t complex-single)
    (let ((value-real (complex-single-reg-real-tn value))
          (result-real (complex-single-reg-real-tn result))
          (io (- (* instance-slots-offset n-word-bytes)
                 instance-pointer-lowtag)))

      (sfloat value-real io lip)
      (unless (location= result-real value-real)
        (inst funop :copy value-real result-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
          (result-imag (complex-single-reg-imag-tn result))
          (io (- (* (1+ instance-slots-offset) n-word-bytes)
                 instance-pointer-lowtag)))
      (sfloat value-imag io lip)
      (unless (location= result-imag value-imag)
        (inst funop :copy value-imag result-imag))))

  (raw-instance (double -4 nil complex-double)
    (let ((r0 (- (* instance-slots-offset n-word-bytes)
                 instance-pointer-lowtag))
          (r1 (- (* (+ instance-slots-offset 1) n-word-bytes)
                 instance-pointer-lowtag))
          (i0 (- (* (+ instance-slots-offset 2) n-word-bytes)
                 instance-pointer-lowtag))
          (i1 (- (* (+ instance-slots-offset 3) n-word-bytes)
                 instance-pointer-lowtag)))
      (lfloat r0 lip (complex-double-reg-real-tn value) :side 0)
      (lfloat r1 lip (complex-double-reg-real-tn value) :side 1)
      (lfloat i0 lip (complex-double-reg-imag-tn value) :side 0)
      (lfloat i1 lip (complex-double-reg-imag-tn value) :side 1)))

  (raw-instance (double -4 t complex-double)
    (let ((value-real  (complex-double-reg-real-tn value))
          (result-real (complex-double-reg-real-tn result))
          (value-imag (complex-double-reg-imag-tn value))
          (result-imag (complex-double-reg-imag-tn result))
          (r0 (- (* instance-slots-offset n-word-bytes)
                 instance-pointer-lowtag))
          (r1 (- (* (+ instance-slots-offset 1) n-word-bytes)
                 instance-pointer-lowtag))
          (i0 (- (* (+ instance-slots-offset 2) n-word-bytes)
                 instance-pointer-lowtag))
          (i1 (- (* (+ instance-slots-offset 3) n-word-bytes)
                      instance-pointer-lowtag)))
      (sfloat value-real r0 lip :side 0)
      (sfloat value-real r1 lip :side 1)
      (sfloat value-imag i0 lip :side 0)
      (sfloat value-imag i1 lip :side 1)
      (unless (location= result-real value-real)
        (inst funop :copy value-real result-real))
      (unless (location= result-imag value-imag)
         (inst funop :copy value-imag result-imag)))))
