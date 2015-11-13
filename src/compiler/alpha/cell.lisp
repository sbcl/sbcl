;;;; the VM definition of various primitive memory access VOPs for the
;;;; Alpha

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; data object ref/set stuff

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
  (:info name offset lowtag #!+gengc remember)
  (:ignore name)
  (:results)
  (:generator 1
    #!+gengc
    (if remember
        (storew-and-remember-slot value object offset lowtag)
        (storew value object offset lowtag))
    #!-gengc
    (storew value object offset lowtag)))

(define-vop (init-slot set-slot))

;;;; symbol hacking VOPs

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

;;; With SYMBOL-VALUE, we check that the value isn't the trap object.
;;; So SYMBOL-VALUE of NIL is NIL.
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (move object obj-temp)
    (loadw value obj-temp symbol-value-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error obj-temp)))
      (inst xor value unbound-marker-widetag temp)
      (inst beq temp err-lab))))

;;; like CHECKED-CELL-REF, only we are a predicate to see if the cell
;;; is bound
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
    (inst xor value unbound-marker-widetag temp)
    (if not-p
        (inst beq temp target)
        (inst bne temp target))))

(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:policy :fast)
  (:translate symbol-value))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    ;; The symbol-hash slot of NIL holds NIL because it is also the
    ;; cdr slot, so we have to strip off the two low bits to make sure
    ;; it is a fixnum.  The lowtag selection magic that is required to
    ;; ensure this is explained in the comment in objdef.lisp
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst bic res #.(ash lowtag-mask -1) res)))

;;; On unithreaded builds these are just copies of the non-global versions.
(define-vop (%set-symbol-global-value set))
(define-vop (symbol-global-value symbol-value)
  (:translate symbol-global-value))
(define-vop (fast-symbol-global-value fast-symbol-value)
  (:translate symbol-global-value))

;;;; fdefinition (FDEFN) objects

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
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 10
    (move object obj-temp)
    (loadw value obj-temp fdefn-fun-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop 'undefined-fun-error obj-temp)))
      (inst cmpeq value null-tn temp)
      (inst bne temp err-lab))))

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
      (inst xor type simple-fun-header-widetag type)
      (inst addq function
            (- (ash simple-fun-code-offset word-shift) fun-pointer-lowtag)
            lip)
      (inst beq type normal-fn)
      (inst li (make-fixup "closure_tramp" :foreign) lip)
      (emit-label normal-fn)
      (storew lip fdefn fdefn-raw-addr-slot other-pointer-lowtag)
      (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
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
    (move fdefn result)
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)))

;;;; binding and Unbinding

;;; Establish VAL as a binding for SYMBOL. Save the old value and the
;;; symbol on the binding stack and stuff the new value into the symbol.
;;;
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.

(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst addq bsp-tn (* 2 n-word-bytes) bsp-tn)
    (storew temp bsp-tn (- binding-value-slot binding-size))
    (storew symbol bsp-tn (- binding-symbol-slot binding-size))
    (#!+gengc storew-and-remember-slot #!-gengc storew
             val symbol symbol-value-slot other-pointer-lowtag)))


(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
    (loadw value bsp-tn (- binding-value-slot binding-size))
    (#!+gengc storew-and-remember-slot #!-gengc storew
             value symbol symbol-value-slot other-pointer-lowtag)
    (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))
    (storew zero-tn bsp-tn (- binding-value-slot binding-size))
    (inst subq bsp-tn (* 2 n-word-bytes) bsp-tn)))


(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 0
    (let ((loop (gen-label))
          (skip (gen-label))
          (done (gen-label)))
      (move arg where)
      (inst cmpeq where bsp-tn temp)
      (inst bne temp done)

      (emit-label loop)
      (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
      (loadw value bsp-tn (- binding-value-slot binding-size))
      (inst beq symbol skip)
      (#!+gengc storew-and-remember-slot #!-gengc storew
               value symbol symbol-value-slot other-pointer-lowtag)
      (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))

      (emit-label skip)
      (storew zero-tn bsp-tn (- binding-value-slot binding-size))
      (inst subq bsp-tn (* 2 n-word-bytes) bsp-tn)
      (inst cmpeq where bsp-tn temp)
      (inst beq temp loop)

      (emit-label done))))

;;;; closure indexing

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

;;;; value cell hackery

(define-vop (value-cell-ref cell-ref)
  (:variant value-cell-value-slot other-pointer-lowtag))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-lowtag))

;;;; instance hackery

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

;;;; code object frobbing

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (descriptor-reg any-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-lowtag
  (descriptor-reg any-reg null zero) * code-header-set)

;;;; mutator accessing

#!+gengc
(progn

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; SBCL has never had GENGC. Now that we have Alpha support, it
  ;; would probably be nice to restore GENGC support so that the Alpha
  ;; doesn't have to crawl along with stop'n'copy. When we do, the CMU
  ;; CL code below will need updating to the SBCL way of looking at
  ;; things, e.g. at least using "SB-KERNEL" or "SB!KERNEL" instead of
  ;; :KERNEL. -- WHN 2001-05-08
  (error "This code is stale as of sbcl-0.6.12."))

(define-vop (mutator-ub32-ref)
  (:policy :fast-safe)
  (:args)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant-vars slot)
  (:generator 2
    (loadw res mutator-tn slot)))

(define-vop (mutator-descriptor-ref mutator-ub32-ref)
  (:results (res :scs (any-reg descriptor-reg)))
  (:result-types *))

(define-vop (mutator-sap-ref mutator-ub32-ref)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer))


(define-vop (mutator-ub32-set)
  (:policy :fast-safe)
  (:args (arg :scs (unsigned-reg) :target res))
  (:arg-types unsigned-num)
  (:results (res :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:variant-vars slot)
  (:generator 2
    (storew arg mutator-tn slot)
    (move res arg)))

(define-vop (mutator-descriptor-set mutator-ub32-set)
  (:args (arg :scs (any-reg descriptor-reg null zero) :target res))
  (:arg-types *)
  (:results (res :scs (any-reg descriptor-reg)))
  (:result-types *))

(define-vop (mutator-sap-set mutator-ub32-set)
  (:args (arg :scs (sap-reg) :target res))
  (:arg-types system-area-pointer)
  (:results (res :scs (sap-reg)))
  (:result-types system-area-pointer))


(macrolet ((define-mutator-accessors (slot type writable)
             (let ((ref (symbolicate "MUTATOR-" slot "-REF"))
                   (set (and writable (symbolicate "MUTATOR-" slot "-SET")))
                   (offset (symbolicate "MUTATOR-" slot "-SLOT"))
                   (fn
                    (let ((*package* (find-package :kernel)))
             (symbolicate "MUTATOR-" slot))))
               (multiple-value-bind
                   (lisp-type ref-vop set-vop)
                   (ecase type
                     (:des
                      (values t
                              'mutator-descriptor-ref
                              'mutator-descriptor-set))
                     (:ub32
                      (values '(unsigned-byte 32)
                              'mutator-ub32-ref
                              'mutator-ub32-set))
                     (:sap
                      (values 'system-area-pointer
                              'mutator-sap-ref
                              'mutator-sap-set)))
                 `(progn
                    (export ',fn :kernel)
                    (defknown ,fn () ,lisp-type (flushable))
                    (define-vop (,ref ,ref-vop)
                      (:translate ,fn)
                      (:variant ,offset))
                    ,@(when writable
                        `((defknown ((setf ,fn)) (,lisp-type) ,lisp-type
                            ())
                          (define-vop (,set ,set-vop)
                            (:translate (setf ,fn))
                            (:variant ,offset)))))))))
  (define-mutator-accessors thread :des t)
  (define-mutator-accessors suspends-disabled-count :ub32 t)
  (define-mutator-accessors suspend-pending :ub32 t)
  (define-mutator-accessors control-stack-base :sap nil)
  (define-mutator-accessors control-stack-end :sap nil)
  (define-mutator-accessors current-unwind-protect :sap nil)
  (define-mutator-accessors current-catch-block :sap nil)
  (define-mutator-accessors binding-stack-base :sap nil)
  (define-mutator-accessors binding-stack-end :sap nil)
  (define-mutator-accessors number-stack-base :sap nil)
  (define-mutator-accessors number-stack-end :sap nil)
  (define-mutator-accessors nursery-start :sap nil)
  (define-mutator-accessors nursery-end :sap nil)
  (define-mutator-accessors storebuf-start :sap nil)
  (define-mutator-accessors storebuf-end :sap nil)
  (define-mutator-accessors words-consed :ub32 nil))

); #+gengc progn



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
    (inst srl offset n-widetag-bits offset)
    (inst sll offset 2 offset)
    (inst subq offset index offset)
    (inst subq offset n-word-bytes offset)
    (inst addq object offset lip)
    (inst ldl
          value
          (- (* instance-slots-offset n-word-bytes)
             instance-pointer-lowtag)
          lip)
    (inst mskll value 4 value)))

(define-vop (raw-instance-set/word)
  (:translate %raw-instance-set/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types unsigned-num)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits offset)
    (inst sll offset 2 offset)
    (inst subq offset index offset)
    (inst subq offset n-word-bytes offset)
    (inst addq object offset lip)
    (inst stl
          value
          (- (* instance-slots-offset n-word-bytes)
             instance-pointer-lowtag)
          lip)
    (move value result)))

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
    (inst srl offset n-widetag-bits offset)
    (inst sll offset 2 offset)
    (inst subq offset index offset)
    (inst subq offset n-word-bytes offset)
    (inst addq object offset lip)
    (inst lds
          value
          (- (* instance-slots-offset n-word-bytes)
             instance-pointer-lowtag)
          lip)))

(define-vop (raw-instance-set/single)
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg)))
  (:arg-types * positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types single-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits offset)
    (inst sll offset 2 offset)
    (inst subq offset index offset)
    (inst subq offset n-word-bytes offset)
    (inst addq object offset lip)
    (inst sts
          value
          (- (* instance-slots-offset n-word-bytes)
             instance-pointer-lowtag)
          lip)
    (unless (location= result value)
      (inst fmove value result))))

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
    (inst srl offset n-widetag-bits offset)
    (inst sll offset 2 offset)
    (inst subq offset index offset)
    (inst subq offset (* 2 n-word-bytes) offset)
    (inst addq object offset lip)
    (inst ldt
          value
          (- (* instance-slots-offset n-word-bytes)
             instance-pointer-lowtag)
          lip)))

(define-vop (raw-instance-set/double)
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg)))
  (:arg-types * positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types double-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits offset)
    (inst sll offset 2 offset)
    (inst subq offset index offset)
    (inst subq offset (* 2 n-word-bytes) offset)
    (inst addq object offset lip)
    (inst stt
          value
          (- (* instance-slots-offset n-word-bytes)
             instance-pointer-lowtag)
          lip)
    (unless (location= result value)
      (inst fmove value result))))

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
    (inst srl offset n-widetag-bits offset)
    (inst sll offset 2 offset)
    (inst subq offset index offset)
    (inst subq offset (* 2 n-word-bytes) offset)
    (inst addq object offset lip)
    (inst lds
          (complex-double-reg-real-tn value)
          (- (* instance-slots-offset n-word-bytes)
             instance-pointer-lowtag)
          lip)
    (inst lds
          (complex-double-reg-imag-tn value)
          (- (* (1+ instance-slots-offset) n-word-bytes)
             instance-pointer-lowtag)
          lip)))

(define-vop (raw-instance-set/complex-single)
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg)))
  (:arg-types * positive-fixnum complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-single-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits offset)
    (inst sll offset 2 offset)
    (inst subq offset index offset)
    (inst subq offset (* 2 n-word-bytes) offset)
    (inst addq object offset lip)
    (let ((value-real (complex-single-reg-real-tn value))
          (result-real (complex-single-reg-real-tn result)))
      (inst sts
            value-real
            (- (* instance-slots-offset n-word-bytes)
               instance-pointer-lowtag)
            lip)
      (unless (location= result-real value-real)
        (inst fmove value-real result-real)))
    (let ((value-imag (complex-single-reg-imag-tn value))
          (result-imag (complex-single-reg-imag-tn result)))
      (inst sts
            value-imag
            (- (* (1+ instance-slots-offset) n-word-bytes)
               instance-pointer-lowtag)
            lip)
      (unless (location= result-imag value-imag)
        (inst fmove value-imag result-imag)))))

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
    (inst srl offset n-widetag-bits offset)
    (inst sll offset 2 offset)
    (inst subq offset index offset)
    (inst subq offset (* 4 n-word-bytes) offset)
    (inst addq object offset lip)
    (inst ldt
          (complex-double-reg-real-tn value)
          (- (* instance-slots-offset n-word-bytes)
             instance-pointer-lowtag)
          lip)
    (inst ldt
          (complex-double-reg-imag-tn value)
          (- (* (+ instance-slots-offset 2) n-word-bytes)
             instance-pointer-lowtag)
          lip)))

(define-vop (raw-instance-set/complex-double)
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg)))
  (:arg-types * positive-fixnum complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-double-float)
  (:generator 5
    (loadw offset object 0 instance-pointer-lowtag)
    (inst srl offset n-widetag-bits offset)
    (inst sll offset 2 offset)
    (inst subq offset index offset)
    (inst subq offset (* 4 n-word-bytes) offset)
    (inst addq object offset lip)
    (let ((value-real (complex-double-reg-real-tn value))
          (result-real (complex-double-reg-real-tn result)))
      (inst stt
            value-real
            (- (* instance-slots-offset n-word-bytes)
               instance-pointer-lowtag)
            lip)
      (unless (location= result-real value-real)
        (inst fmove value-real result-real)))
    (let ((value-imag (complex-double-reg-imag-tn value))
          (result-imag (complex-double-reg-imag-tn result)))
      (inst stt
            value-imag
            (- (* (+ instance-slots-offset 2) n-word-bytes)
               instance-pointer-lowtag)
            lip)
      (unless (location= result-imag value-imag)
        (inst fmove value-imag result-imag)))))
