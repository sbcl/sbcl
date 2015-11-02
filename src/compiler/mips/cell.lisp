;;;; the VM definition of various primitive memory access VOPs for
;;;; MIPS

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
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
;;;
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (move obj-temp object)
    (loadw value obj-temp symbol-value-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error obj-temp)))
      (inst xor temp value unbound-marker-widetag)
      (inst beq temp err-lab)
      (inst nop))))

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
    (inst xor temp value unbound-marker-widetag)
    (if not-p
        (inst beq temp target)
        (inst bne temp target))
    (inst nop)))

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
    ;; The symbol-hash slot of NIL holds NIL because it is also the
    ;; cdr slot, so we have to strip off the two low bits to make sure
    ;; it is a fixnum.  The lowtag selection magic that is required to
    ;; ensure this is explained in the comment in objdef.lisp
    (loadw temp symbol symbol-hash-slot other-pointer-lowtag)
    (inst srl temp n-fixnum-tag-bits)
    (inst sll res temp n-fixnum-tag-bits)))

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
      (inst beq value null-tn err-lab))
    (inst nop)))

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
      (inst nop)
      (inst xor type simple-fun-header-widetag)
      (inst beq type normal-fn)
      (inst addu lip function
            (- (ash simple-fun-code-offset word-shift)
               fun-pointer-lowtag))
      (inst li lip (make-fixup "closure_tramp" :foreign))
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
    (inst li temp (make-fixup "undefined_tramp" :foreign))
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result fdefn)))



;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.

(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst addu bsp-tn bsp-tn (* 2 n-word-bytes))
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
    (inst addu bsp-tn bsp-tn (* -2 n-word-bytes))))


(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (let ((loop (gen-label))
          (skip (gen-label))
          (done (gen-label)))
      (move where arg)
      (inst beq where bsp-tn done)
      (inst nop)

      (emit-label loop)
      (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
      (inst beq symbol skip)
      (loadw value bsp-tn (- binding-value-slot binding-size))
      (storew value symbol symbol-value-slot other-pointer-lowtag)
      (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))

      (emit-label skip)
      (storew zero-tn bsp-tn (- binding-value-slot binding-size))
      (inst addu bsp-tn bsp-tn (* -2 n-word-bytes))
      (inst bne where bsp-tn loop)
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
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw temp struct 0 instance-pointer-lowtag)
    (inst srl res temp n-widetag-bits)))

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

(define-vop (raw-instance-ref/word)
  (:translate %raw-instance-ref/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (unsigned-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types unsigned-num)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits)
    (inst sll offset n-fixnum-tag-bits)
    (inst subu offset index)
    (inst subu offset n-word-bytes)
    (inst addu lip offset object)
    (inst lw value lip (- (* instance-slots-offset n-word-bytes)
                          instance-pointer-lowtag))))

(define-vop (raw-instance-set/word)
  (:translate %raw-instance-set/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (unsigned-reg) :target result))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types unsigned-num)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits)
    (inst sll offset n-fixnum-tag-bits)
    (inst subu offset index)
    (inst subu offset n-word-bytes)
    (inst addu lip offset object)
    (inst sw value lip (- (* instance-slots-offset n-word-bytes)
                          instance-pointer-lowtag))
    (move result value)))

(define-vop (raw-instance-ref/single)
  (:translate %raw-instance-ref/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (single-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types single-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits)
    (inst sll offset n-fixnum-tag-bits)
    (inst subu offset index)
    (inst subu offset n-word-bytes)
    (inst addu lip offset object)
    (inst lwc1 value lip (- (* instance-slots-offset n-word-bytes)
                            instance-pointer-lowtag))))

(define-vop (raw-instance-set/single)
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg) :target result))
  (:arg-types * positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types single-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits)
    (inst sll offset n-fixnum-tag-bits)
    (inst subu offset index)
    (inst subu offset n-word-bytes)
    (inst addu lip offset object)
    (inst swc1 value lip (- (* instance-slots-offset n-word-bytes)
                            instance-pointer-lowtag))
    (unless (location= result value)
      (inst fmove :single result value))))

(define-vop (raw-instance-ref/double)
  (:translate %raw-instance-ref/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (double-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types double-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits)
    (inst sll offset n-fixnum-tag-bits)
    (inst subu offset index)
    (inst subu offset (* 2 n-word-bytes))
    (inst addu lip offset object)
    (let ((immediate-offset (- (* instance-slots-offset n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst lwc1 value lip immediate-offset))
        (:little-endian (inst lwc1-odd value lip immediate-offset))))
    (let ((immediate-offset (- (* (1+ instance-slots-offset) n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst lwc1-odd value lip immediate-offset))
        (:little-endian (inst lwc1 value lip immediate-offset))))))

(define-vop (raw-instance-set/double)
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg) :target result))
  (:arg-types * positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types double-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits)
    (inst sll offset n-fixnum-tag-bits)
    (inst subu offset index)
    (inst subu offset (* 2 n-word-bytes))
    (inst addu lip offset object)
    (let ((immediate-offset (- (* instance-slots-offset n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst swc1 value lip immediate-offset))
        (:little-endian (inst swc1-odd value lip immediate-offset))))
    (let ((immediate-offset (- (* (1+ instance-slots-offset) n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst swc1-odd value lip immediate-offset))
        (:little-endian (inst swc1 value lip immediate-offset))))
    (unless (location= result value)
      (inst fmove :double result value))))

(define-vop (raw-instance-ref/complex-single)
  (:translate %raw-instance-ref/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-single-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits)
    (inst sll offset n-fixnum-tag-bits)
    (inst subu offset index)
    (inst subu offset (* 2 n-word-bytes))
    (inst addu lip offset object)
    (inst lwc1
          (complex-single-reg-real-tn value)
          lip
          (- (* instance-slots-offset n-word-bytes)
             instance-pointer-lowtag))
    (inst lwc1
          (complex-single-reg-imag-tn value)
          lip
          (- (* (1+ instance-slots-offset) n-word-bytes)
             instance-pointer-lowtag))))

(define-vop (raw-instance-set/complex-single)
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg) :target result))
  (:arg-types * positive-fixnum complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-single-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits)
    (inst sll offset n-fixnum-tag-bits)
    (inst subu offset index)
    (inst subu offset (* 2 n-word-bytes))
    (inst addu lip offset object)
    (let ((value-real (complex-single-reg-real-tn value))
          (result-real (complex-single-reg-real-tn result)))
      (inst swc1
            value-real
            lip
            (- (* instance-slots-offset n-word-bytes)
               instance-pointer-lowtag))
      (unless (location= result-real value-real)
        (inst fmove :single result-real value-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
          (result-imag (complex-single-reg-imag-tn result)))
      (inst swc1
            value-imag
            lip
            (- (* (1+ instance-slots-offset) n-word-bytes)
               instance-pointer-lowtag))
      (unless (location= result-imag value-imag)
        (inst fmove :single result-imag value-imag)))))

(define-vop (raw-instance-ref/complex-double)
  (:translate %raw-instance-ref/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-double-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits)
    (inst sll offset n-fixnum-tag-bits)
    (inst subu offset index)
    (inst subu offset (* 4 n-word-bytes))
    (inst addu lip offset object)
    (let ((immediate-offset (- (* instance-slots-offset n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst lwc1
                           (complex-double-reg-real-tn value)
                           lip
                           immediate-offset))
        (:little-endian (inst lwc1-odd
                              (complex-double-reg-real-tn value)
                              lip
                              immediate-offset))))
    (let ((immediate-offset (- (* (1+ instance-slots-offset) n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst lwc1-odd
                           (complex-double-reg-real-tn value)
                           lip
                           immediate-offset))
        (:little-endian (inst lwc1
                              (complex-double-reg-real-tn value)
                              lip
                              immediate-offset))))
    (let ((immediate-offset (- (* (+ instance-slots-offset 2) n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst lwc1
                           (complex-double-reg-imag-tn value)
                           lip
                           immediate-offset))
        (:little-endian (inst lwc1-odd
                              (complex-double-reg-imag-tn value)
                              lip
                              immediate-offset))))
    (let ((immediate-offset (- (* (+ instance-slots-offset 3) n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst lwc1-odd
                           (complex-double-reg-imag-tn value)
                           lip
                           immediate-offset))
        (:little-endian (inst lwc1
                              (complex-double-reg-imag-tn value)
                              lip
                              immediate-offset))))))

(define-vop (raw-instance-set/complex-double)
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg) :target result))
  (:arg-types * positive-fixnum complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-double-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits)
    (inst sll offset n-fixnum-tag-bits)
    (inst subu offset index)
    (inst subu offset (* 4 n-word-bytes))
    (inst addu lip offset object)
    (let ((value-real (complex-double-reg-real-tn value))
          (result-real (complex-double-reg-real-tn result)))
      (let ((immediate-offset (- (* instance-slots-offset n-word-bytes)
                                 instance-pointer-lowtag)))
        (ecase *backend-byte-order*
          (:big-endian (inst swc1
                             value-real
                             lip
                             immediate-offset))
          (:little-endian (inst swc1-odd
                                value-real
                                lip
                                immediate-offset))))
      (let ((immediate-offset (- (* (1+ instance-slots-offset) n-word-bytes)
                                 instance-pointer-lowtag)))
        (ecase *backend-byte-order*
          (:big-endian (inst swc1-odd
                             value-real
                             lip
                             immediate-offset))
          (:little-endian (inst swc1
                                value-real
                                lip
                                immediate-offset))))
      (unless (location= result-real value-real)
        (inst fmove :double result-real value-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
          (result-imag (complex-double-reg-imag-tn result)))
      (let ((immediate-offset (- (* (+ instance-slots-offset 2) n-word-bytes)
                                 instance-pointer-lowtag)))
        (ecase *backend-byte-order*
          (:big-endian (inst swc1
                             value-imag
                             lip
                             immediate-offset))
          (:little-endian (inst swc1-odd
                                value-imag
                                lip
                                immediate-offset))))
      (let ((immediate-offset (- (* (+ instance-slots-offset 3) n-word-bytes)
                                 instance-pointer-lowtag)))
        (ecase *backend-byte-order*
          (:big-endian (inst swc1-odd
                             value-imag
                             lip
                             immediate-offset))
          (:little-endian (inst swc1
                                value-imag
                                lip
                                immediate-offset))))
      (unless (location= result-imag value-imag)
        (inst fmove :double result-imag value-imag)))))
