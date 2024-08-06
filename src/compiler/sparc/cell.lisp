;;;; the VM definition of various primitive memory access VOPs for the
;;;; Sparc

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; data object ref/set stuff.
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
(define-vop (set cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

;;; Do a cell ref with an error check for being unbound.
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp))

;;; With Symbol-Value, we check that the value isn't the trap object.
(define-vop (symbol-value checked-cell-ref)
  (:translate symbol-value)
  (:generator 9
    (move obj-temp object)
    (loadw value obj-temp symbol-value-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error obj-temp)))
      (inst cmp value unbound-marker-widetag)
      (inst b :eq err-lab)
      (inst nop))))

;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell
;;; is bound.
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
    (inst b (if not-p :eq :ne) target)
    (inst nop)))

(define-vop (fast-symbol-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:policy :fast)
  (:translate symbol-value))

;;; On unithreaded builds these are just copies of the non-global versions.
(define-vop (%set-symbol-global-value set))
(define-vop (symbol-global-value symbol-value)
  (:translate symbol-global-value))
(define-vop (fast-symbol-global-value fast-symbol-value)
  (:translate symbol-global-value))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:temporary (:sc unsigned-reg) tmp)
  (:generator 2
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    ;; Clear the 3 highest bits, ensuring the result is positive fixnum.
    ;; I am not as smart as the C compiler which uses ANDN for 1 fewer instruction.
    (inst li tmp #x1fffffff)
    (inst and res res tmp)))

(define-vop (symbol-name-hash symbol-hash)
  (:translate symbol-name-hash)
  (:ignore tmp)
  (:generator 2
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst srl res 3))) ; shift out the 3 pseudorandom bits

;;;; FDEFINITION (fdefn) objects.

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
      (inst b :eq err-lab))
    (inst nop)))

(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg))
         (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:generator 38
    (load-type type function (- fun-pointer-lowtag))
    (inst cmp type simple-fun-widetag)
    (inst b :eq SIMPLE)
    (inst move lip function)
    (inst li lip (make-fixup 'closure-tramp :assembly-routine))
    SIMPLE
    (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
    (storew lip fdefn fdefn-raw-addr-slot other-pointer-lowtag)))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 38
    (storew null-tn fdefn fdefn-fun-slot other-pointer-lowtag)
    (inst li temp (make-fixup 'undefined-tramp :assembly-routine))
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)))


;;;; Binding and Unbinding.

;;; Establish VAL as a binding for SYMBOL.  Save the old value and the
;;; symbol on the binding stack and stuff the new value into the
;;; symbol.
;;;
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.

(define-vop (dynbind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst add bsp-tn bsp-tn (* 2 n-word-bytes))
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
    (inst sub bsp-tn bsp-tn (* 2 n-word-bytes))))

(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (let ((loop (gen-label))
          (skip (gen-label))
          (done (gen-label)))
      (move where arg)
      (inst cmp where bsp-tn)
      (inst b :eq done)
      (inst nop)

      (emit-label loop)
      (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
      (inst cmp symbol)
      (inst b :eq skip)
      (loadw value bsp-tn (- binding-value-slot binding-size))
      (storew value symbol symbol-value-slot other-pointer-lowtag)
      (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))

      (emit-label skip)
      (storew zero-tn bsp-tn (- binding-value-slot binding-size))
      (inst sub bsp-tn bsp-tn (* 2 n-word-bytes))
      (inst cmp where bsp-tn)
      (inst b :ne loop)
      (inst nop)

      (emit-label done))))

;;;; closure indexing.

(define-vop (closure-index-ref word-index-ref)
  (:variant closure-info-offset fun-pointer-lowtag)
  (:translate %closure-index-ref))

(define-vop (%closure-index-set word-index-set)
  (:variant closure-info-offset fun-pointer-lowtag)
  (:translate %closure-index-set))

(define-vop (funcallable-instance-info word-index-ref)
  (:variant funcallable-instance-info-offset fun-pointer-lowtag)
  (:translate %funcallable-instance-info))

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

;;;; value cell hackery.

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-lowtag))

;;;; instance hackery:

(define-vop ()
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw res struct 0 instance-pointer-lowtag)
    (inst srl res res instance-length-shift)))

(define-vop (instance-index-ref word-index-ref)
  (:policy :fast-safe)
  (:translate %instance-ref)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types * positive-fixnum))

(define-vop (instance-index-set word-index-set)
  (:policy :fast-safe)
  (:translate %instance-set)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types * positive-fixnum *))

;;;; Code object frobbing.

(define-vop (code-header-ref word-index-ref)
  (:translate code-header-ref)
  (:policy :fast-safe)
  (:variant 0 other-pointer-lowtag))

(define-vop (code-header-set)
  (:translate code-header-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (any-reg descriptor-reg)))
  (:arg-types * tagged-num *)
  (:temporary (:scs (non-descriptor-reg)) temp card)
  (:generator 10
    ;; Load the card mask
    (inst li temp (make-fixup "gc_card_table_mask" :foreign-dataref)) ; linkage entry
    (inst ld temp temp) ; address of gc_card_table_mask
    (inst ld temp temp) ; value of gc_card_table_mask
    (pseudo-atomic (temp)
      ;; Compute card mark index
      (inst srl card object gencgc-card-shift)
      (inst and card card temp)
      ;; Load mark table base
      (inst li temp (make-fixup "gc_card_mark" :foreign-dataref)) ; linkage entry
      (inst ld temp temp) ; address of gc_card_mark
      (inst ld temp temp) ; value of gc_card_mark
      ;; Touch the card mark byte.
      (inst stb null-tn temp card)
      ;; set 'written' flag in the code header
      ;; If two threads get here at the same time, they'll write the same byte.
      (let ((byte #+big-endian (- other-pointer-lowtag) #+little-endian (bug "Wat")))
        (inst ldub temp object byte)
        (inst or temp temp #x40)
        (inst stb temp object byte))
      (inst add temp index (- other-pointer-lowtag))
      (inst st value object temp))))

;;;; raw instance slot accessors

(macrolet ((def (signedp sc result-type)
             `(progn
               (define-vop ()
                 (:translate ,(symbolicate "%RAW-INSTANCE-REF/" signedp "WORD"))
                 (:policy :fast-safe)
                 (:args (object :scs (descriptor-reg))
                        (index :scs (any-reg)))
                 (:arg-types * positive-fixnum)
                 (:results (value :scs (,sc)))
                 (:temporary (:scs (non-descriptor-reg)) offset)
                 (:result-types ,result-type)
                 (:generator 5
                   (inst add offset index (- (ash instance-slots-offset word-shift)
                                             instance-pointer-lowtag))
                   (inst ld value object offset)))

               (define-vop ()
                 (:translate ,(symbolicate "%RAW-INSTANCE-SET/" signedp "WORD"))
                 (:policy :fast-safe)
                 (:args (object :scs (descriptor-reg))
                        (index :scs (any-reg))
                        (value :scs (,sc)))
                 (:arg-types * positive-fixnum ,result-type)
                 (:temporary (:scs (non-descriptor-reg)) offset)
                 (:generator 5
                   (inst add offset index (- (ash instance-slots-offset word-shift)
                                             instance-pointer-lowtag))
                   (inst st value object offset))))))
  (def "" unsigned-reg unsigned-num)
  (def "SIGNED-" signed-reg signed-num))

(define-vop ()
  (:translate %raw-instance-ref/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (single-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:result-types single-float)
  (:generator 5
    (inst add offset index (- (ash instance-slots-offset word-shift)
                              instance-pointer-lowtag))
    (inst ldf value object offset)))

(define-vop ()
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg)))
  (:arg-types * positive-fixnum single-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst add offset index (- (ash instance-slots-offset word-shift)
                              instance-pointer-lowtag))
    (inst stf value object offset)))

(define-vop ()
  (:translate %raw-instance-ref/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (double-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:result-types double-float)
  (:generator 5
    (inst add offset index (- (ash instance-slots-offset word-shift)
                              instance-pointer-lowtag))
    (inst lddf value object offset)))

(define-vop ()
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg)))
  (:arg-types * positive-fixnum double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst add offset index (- (ash instance-slots-offset word-shift)
                              instance-pointer-lowtag))
    (inst stdf value object offset)))

(define-vop ()
  (:translate %raw-instance-ref/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:result-types complex-single-float)
  (:generator 5
    (inst add offset index (- (ash instance-slots-offset word-shift)
                              instance-pointer-lowtag))
    (inst ldf (complex-single-reg-real-tn value) object offset)
    (inst add offset offset n-word-bytes)
    (inst ldf (complex-single-reg-imag-tn value) object offset)))

(define-vop ()
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg)))
  (:arg-types * positive-fixnum complex-single-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst add offset index (- (ash instance-slots-offset word-shift)
                              instance-pointer-lowtag))
    (inst stf (complex-single-reg-real-tn value) object offset)
    (inst add offset offset n-word-bytes)
    (inst stf (complex-single-reg-imag-tn value) object offset)))

(define-vop ()
  (:translate %raw-instance-ref/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:result-types complex-double-float)
  (:generator 5
    (inst add offset index (- (ash instance-slots-offset word-shift)
                              instance-pointer-lowtag))
    (inst lddf (complex-double-reg-real-tn value) object offset)
    (inst add offset offset (* 2 n-word-bytes))
    (inst lddf (complex-double-reg-imag-tn value) object offset)))

(define-vop ()
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg)))
  (:arg-types * positive-fixnum complex-double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst add offset index (- (ash instance-slots-offset word-shift)
                              instance-pointer-lowtag))
    (inst stdf (complex-double-reg-real-tn value) object offset)
    (inst add offset offset (* 2 n-word-bytes))
    (inst stdf (complex-double-reg-imag-tn value) object offset)))
