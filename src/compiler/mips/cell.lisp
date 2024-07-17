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
         (value :scs (descriptor-reg any-reg null zero)))
  (:info name offset lowtag)
  (:temporary (:sc non-descriptor-reg) temp)
  (:vop-var vop)
  (:generator 1
    (without-scheduling ()
      (emit-gengc-barrier object nil temp (vop-nth-arg 1 vop) value name)
      (storew value object offset lowtag))))

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

;;; With Symbol-Value, we check that the value isn't the trap object.
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
(define-vop (boundp)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:translate boundp)
  (:generator 9
    (inst lb temp object (+ (- (ash symbol-value-slot word-shift) other-pointer-lowtag)
                            #+big-endian 3))
    (inst xor temp temp unbound-marker-widetag)
    (if not-p
        (inst beq temp target)
        (inst bne temp target))
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

;;;; Fdefinition (fdefn) objects.

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
  (:args (function :scs (descriptor-reg))
         (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:generator 38
      (without-scheduling ()
        (emit-gengc-barrier fdefn nil type)) ; type = temp
      (load-type type function (- fun-pointer-lowtag))
      (inst xor type simple-fun-widetag)
      (inst beq type SIMPLE)
      (inst addu lip function
            (- (ash simple-fun-insts-offset word-shift)
               fun-pointer-lowtag))
      ;; XXX: it is questionable to load LIP with a value that is not an
      ;; interior pointer relative to a boxed register. If there were any
      ;; tagged pointer below LIP, then LIP might get "fixed" (i.e. ruined)
      ;; by a GC if interrupted after this instruction. And it's even more
      ;; questionable because 'li' is in fact two instructions.
      ;; However, there are mitigating factors:
      ;;   - there should be no boxed objects at a lower address.
      ;;   - lack of thread support makes a GC interrupt somewhat unlikely,
      ;;     (though any interrupt could call into lisp, also unlikely)
      (inst li lip (make-fixup 'closure-tramp :assembly-routine))
    SIMPLE
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


;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.

(define-vop (dynbind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) temp2)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst addu bsp-tn bsp-tn (* 2 n-word-bytes))
    (storew temp bsp-tn (- binding-value-slot binding-size))
    (storew symbol bsp-tn (- binding-symbol-slot binding-size))
    (without-scheduling ()
      (emit-gengc-barrier symbol nil temp2)
      (storew val symbol symbol-value-slot other-pointer-lowtag))))

(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 0
    (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
    (loadw value bsp-tn (- binding-value-slot binding-size))
    (without-scheduling ()
      (emit-gengc-barrier symbol nil temp)
      (storew value symbol symbol-value-slot other-pointer-lowtag))
    (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))
    (storew zero-tn bsp-tn (- binding-value-slot binding-size))
    (inst addu bsp-tn bsp-tn (* -2 n-word-bytes))))


(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 0
      (move where arg)
      (inst beq where bsp-tn done)
      (inst nop)

      LOOP
      (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
      (inst beq symbol skip)
      (loadw value bsp-tn (- binding-value-slot binding-size))
      (without-scheduling ()
        (emit-gengc-barrier symbol nil temp)
        (storew value symbol symbol-value-slot other-pointer-lowtag))
      (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))

      SKIP
      (storew zero-tn bsp-tn (- binding-value-slot binding-size))
      (inst addu bsp-tn bsp-tn (* -2 n-word-bytes))
      (inst bne where bsp-tn loop)
      (inst nop)

      DONE))



;;;; Closure indexing.

(define-full-reffer closure-index-ref *
  closure-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %closure-index-ref)

(define-full-setter %closure-index-set *
  closure-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg null zero) * %closure-index-set)

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
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:generator 4
    (without-scheduling ()
      (unless dx
        (emit-gengc-barrier object nil temp))
      (storew value object (+ closure-info-offset offset) fun-pointer-lowtag))))

(define-vop (closure-init-from-fp)
  (:args (object :scs (descriptor-reg)))
  (:info offset)
  (:generator 4
    (storew cfp-tn object (+ closure-info-offset offset) fun-pointer-lowtag)))

;;;; Value Cell hackery.

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
    (inst srl res res instance-length-shift)))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg null zero) * %instance-set)



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
  (:temporary (:scs (non-descriptor-reg)) temp card)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:generator 10
    (inst li temp (make-fixup "gc_card_table_mask" :foreign-dataref))
    (loadw temp temp) ; address of gc_card_table_mask
    (inst lw temp temp 0) ; value of gc_card_table_mask (4-byte int)
    (pseudo-atomic (pa-flag)
      ;; Compute card mark index
      (inst srl card object gencgc-card-shift)
      (inst and card card temp)
      ;; Load mark table base
      (inst li temp (make-fixup "gc_card_mark" :foreign-dataref)) ; address of linkage entry
      (loadw temp temp) ; address of gc_card_mark
      (loadw temp temp) ; value of gc_card_mark (pointer)
      ;; Touch the card mark byte.
      (inst add temp temp card)
      (inst sb null-tn temp 0)
      ;; set 'written' flag in the code header
      ;; If two threads get here at the same time, they'll write the same byte.
      (let ((byte (- #+little-endian 3 other-pointer-lowtag)))
        (inst lbu temp object byte)
        (inst or temp temp #x40)
        (inst sb temp object byte))
      ;; No need for LIP register because this is pseudo-atomic
      (inst sll temp index (- word-shift n-fixnum-tag-bits))
      (inst add temp object temp)
      (inst sw value temp (- other-pointer-lowtag)))))

;;;; raw instance slot accessors

(macrolet ((def (suffix sc primtype)
             `(progn
                (define-vop ()
                  (:translate ,(symbolicate "%RAW-INSTANCE-REF/" suffix))
                  (:policy :fast-safe)
                  (:args (object :scs (descriptor-reg))
                         (index :scs (any-reg)))
                  (:arg-types * positive-fixnum)
                  (:results (value :scs (,sc)))
                  (:temporary (:scs (interior-reg)) lip)
                  (:result-types ,primtype)
                  (:generator 5
                    (inst addu lip object index)
                    (loadw value lip instance-slots-offset instance-pointer-lowtag)))

                (define-vop ()
                  (:translate ,(symbolicate "%RAW-INSTANCE-SET/" suffix))
                  (:policy :fast-safe)
                  (:args (object :scs (descriptor-reg))
                         (index :scs (any-reg))
                         (value :scs (,sc)))
                  (:arg-types * positive-fixnum ,primtype)
                  (:temporary (:scs (interior-reg)) lip)
                  (:generator 5
                    (inst addu lip object index)
                    (storew value lip instance-slots-offset instance-pointer-lowtag))))))
  (def word unsigned-reg unsigned-num)
  (def signed-word signed-reg signed-num))

(define-vop ()
  (:translate %raw-instance-ref/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (single-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:result-types single-float)
  (:generator 5
    (inst addu lip object index)
    (inst lwc1 value lip (- (* instance-slots-offset n-word-bytes)
                            instance-pointer-lowtag))))

(define-vop ()
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg)))
  (:arg-types * positive-fixnum single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
    (inst addu lip object index)
    (inst swc1 value lip (- (* instance-slots-offset n-word-bytes)
                            instance-pointer-lowtag))))

(define-vop ()
  (:translate %raw-instance-ref/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (double-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:result-types double-float)
  (:generator 5
    (inst addu lip object index)
    (let ((immediate-offset (- (* instance-slots-offset n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst lwc1-odd value lip immediate-offset))
        (:little-endian (inst lwc1 value lip immediate-offset))))
    (let ((immediate-offset (- (* (1+ instance-slots-offset) n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst lwc1 value lip immediate-offset))
        (:little-endian (inst lwc1-odd value lip immediate-offset))))))

(define-vop ()
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg)))
  (:arg-types * positive-fixnum double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
    (inst addu lip object index)
    (let ((immediate-offset (- (* instance-slots-offset n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst swc1-odd value lip immediate-offset))
        (:little-endian (inst swc1 value lip immediate-offset))))
    (let ((immediate-offset (- (* (1+ instance-slots-offset) n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst swc1 value lip immediate-offset))
        (:little-endian (inst swc1-odd value lip immediate-offset))))))

(define-vop ()
  (:translate %raw-instance-ref/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-single-float)
  (:generator 5
    (inst addu lip object index)
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

(define-vop ()
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg)))
  (:arg-types * positive-fixnum complex-single-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
    (inst addu lip object index)
    (inst swc1 (complex-single-reg-real-tn value)
          lip (- (* instance-slots-offset n-word-bytes)
                 instance-pointer-lowtag))
    (inst swc1
          (complex-single-reg-imag-tn value)
          lip (- (* (1+ instance-slots-offset) n-word-bytes)
                 instance-pointer-lowtag))))

(define-vop (raw-instance-ref/complex-double)
  (:translate %raw-instance-ref/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:result-types complex-double-float)
  (:generator 5
    (inst addu lip object index)
    (let ((immediate-offset (- (* instance-slots-offset n-word-bytes)
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
    (let ((immediate-offset (- (* (1+ instance-slots-offset) n-word-bytes)
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
    (let ((immediate-offset (- (* (+ instance-slots-offset 2) n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst lwc1-odd
                           (complex-double-reg-imag-tn value)
                           lip
                           immediate-offset))
        (:little-endian (inst lwc1
                              (complex-double-reg-imag-tn value)
                              lip
                              immediate-offset))))
    (let ((immediate-offset (- (* (+ instance-slots-offset 3) n-word-bytes)
                               instance-pointer-lowtag)))
      (ecase *backend-byte-order*
        (:big-endian (inst lwc1
                           (complex-double-reg-imag-tn value)
                           lip
                           immediate-offset))
        (:little-endian (inst lwc1-odd
                              (complex-double-reg-imag-tn value)
                              lip
                              immediate-offset))))))

(define-vop ()
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg)))
  (:arg-types * positive-fixnum complex-double-float)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 5
    (inst addu lip object index)
    (let ((value-real (complex-double-reg-real-tn value)))
      (let ((immediate-offset (- (* instance-slots-offset n-word-bytes)
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
      (let ((immediate-offset (- (* (1+ instance-slots-offset) n-word-bytes)
                                 instance-pointer-lowtag)))
        (ecase *backend-byte-order*
          (:big-endian (inst swc1
                             value-real
                             lip
                             immediate-offset))
          (:little-endian (inst swc1-odd
                                value-real
                                lip
                                immediate-offset)))))
    (let ((value-imag (complex-double-reg-imag-tn value)))
      (let ((immediate-offset (- (* (+ instance-slots-offset 2) n-word-bytes)
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
      (let ((immediate-offset (- (* (+ instance-slots-offset 3) n-word-bytes)
                                 instance-pointer-lowtag)))
        (ecase *backend-byte-order*
          (:big-endian (inst swc1
                             value-imag
                             lip
                             immediate-offset))
          (:little-endian (inst swc1-odd
                                value-imag
                                lip
                                immediate-offset)))))))
