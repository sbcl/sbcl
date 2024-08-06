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

(define-vop (compare-and-swap-slot)
  (:args (object :scs (descriptor-reg))
         (old :scs (descriptor-reg any-reg))
         (new :scs (descriptor-reg any-reg)))
  (:temporary (:sc non-descriptor-reg) temp)
  (:info name offset lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg) :from :load))
  (:generator 5
    (inst sync)
    (inst li temp (- (* offset n-word-bytes) lowtag))
    LOOP
    (inst lwarx result temp object)
    (inst cmpw result old)
    (inst bne EXIT)
    (inst stwcx. new temp object)
    (inst bne LOOP)
    EXIT
    (inst isync)))


;;;; Symbol hacking VOPs:

(define-vop (%compare-and-swap-symbol-value)
  (:translate %compare-and-swap-symbol-value)
  (:args (symbol :scs (descriptor-reg))
         (old :scs (descriptor-reg any-reg))
         (new :scs (descriptor-reg any-reg)))
  (:temporary (:sc non-descriptor-reg) temp)
  (:results (result :scs (descriptor-reg any-reg) :from :load))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 15
    (inst sync)
    #+sb-thread
    (assemble ()
      (loadw temp symbol symbol-tls-index-slot other-pointer-lowtag)
      ;; Thread-local area, no synchronization needed.
      (inst lwzx result thread-base-tn temp)
      (inst cmpw result old)
      (inst bne DONT-STORE-TLS)
      (inst stwx new thread-base-tn temp)
      DONT-STORE-TLS

      (inst cmpwi result no-tls-value-marker)
      (inst bne CHECK-UNBOUND))

    (inst li temp (- (* symbol-value-slot n-word-bytes)
                     other-pointer-lowtag))
    LOOP
    (inst lwarx result symbol temp)
    (inst cmpw result old)
    (inst bne CHECK-UNBOUND)
    (inst stwcx. new symbol temp)
    (inst bne LOOP)

    CHECK-UNBOUND
    (inst isync)
    (inst cmpwi result unbound-marker-widetag)
    (inst beq (generate-error-code vop 'unbound-symbol-error symbol))))

;;; The compiler likes to be able to directly SET symbols.
(define-vop (%set-symbol-global-value cell-set)
  (:variant symbol-value-slot other-pointer-lowtag))

;;; Do a cell ref with an error check for being unbound.
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :target obj-temp))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) obj-temp))

;;; With SYMBOL-VALUE, we check that the value isn't the trap object.
(define-vop (symbol-global-value checked-cell-ref)
  (:translate symbol-global-value)
  (:generator 9
    (move obj-temp object)
    (loadw value obj-temp symbol-value-slot other-pointer-lowtag)
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error obj-temp)))
      (inst cmpwi value unbound-marker-widetag)
      (inst beq err-lab))))

(define-vop (fast-symbol-global-value cell-ref)
  (:variant symbol-value-slot other-pointer-lowtag)
  (:policy :fast)
  (:translate symbol-global-value))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    ;; Clear the 3 highest bits, ensuring the result is positive fixnum
    (inst clrlwi res res 3)))

(define-vop (symbol-name-hash symbol-hash)
  (:translate symbol-name-hash)
  (:generator 2
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst srwi res res 3))) ; shift out the 3 pseudorandom bits

#+sb-thread
(progn
  (define-vop (set)
    (:args (symbol :scs (descriptor-reg))
           (value :scs (descriptor-reg any-reg)))
    (:temporary (:sc any-reg) tls-slot temp)
    (:generator 4
      (loadw tls-slot symbol symbol-tls-index-slot other-pointer-lowtag)
      (inst lwzx temp thread-base-tn tls-slot)
      (inst cmpwi temp no-tls-value-marker)
      (inst beq GLOBAL-VALUE)
      (inst stwx value thread-base-tn tls-slot)
      (inst b DONE)
      GLOBAL-VALUE
      (storew value symbol symbol-value-slot other-pointer-lowtag)
      DONE))

  ;; With Symbol-Value, we check that the value isn't the trap object.
  (define-vop (symbol-value)
    (:translate symbol-value)
    (:policy :fast-safe)
    (:args (object :scs (descriptor-reg) :to (:result 1)))
    (:results (value :scs (descriptor-reg any-reg)))
    (:vop-var vop)
    (:save-p :compute-only)
    (:generator 9
      (loadw value object symbol-tls-index-slot other-pointer-lowtag)
      (inst lwzx value thread-base-tn value)
      (inst cmpwi value no-tls-value-marker)
      (inst bne CHECK-UNBOUND)
      (loadw value object symbol-value-slot other-pointer-lowtag)
      CHECK-UNBOUND
      (inst cmpwi value unbound-marker-widetag)
      (inst beq (generate-error-code vop 'unbound-symbol-error object))))

  (define-vop (fast-symbol-value symbol-value)
    ;; KLUDGE: not really fast, in fact, because we're going to have to
    ;; do a full lookup of the thread-local area anyway.  But half of
    ;; the meaning of FAST-SYMBOL-VALUE is "do not signal an error if
    ;; unbound", which is used in the implementation of COPY-SYMBOL.  --
    ;; CSR, 2003-04-22
    (:policy :fast)
    (:translate symbol-value)
    (:generator 8
      (loadw value object symbol-tls-index-slot other-pointer-lowtag)
      (inst lwzx value thread-base-tn value)
      (inst cmpwi value no-tls-value-marker)
      (inst bne DONE)
      (loadw value object symbol-value-slot other-pointer-lowtag)
      DONE)))

;;; On unithreaded builds these are just copies of the global versions.
#-sb-thread
(progn
  (define-vop (symbol-value symbol-global-value)
    (:translate symbol-value))
  (define-vop (fast-symbol-value fast-symbol-global-value)
    (:translate symbol-value))
  (define-vop (set %set-symbol-global-value)))

;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell
;;; is bound.
(define-vop (boundp)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value)
  (:translate boundp)
  #+sb-thread
  (:generator 9
    (loadw value object symbol-tls-index-slot other-pointer-lowtag)
    (inst lwzx value thread-base-tn value)
    (inst cmpwi value no-tls-value-marker)
    (inst bne CHECK-UNBOUND)
    (loadw value object symbol-value-slot other-pointer-lowtag)
    CHECK-UNBOUND
    (inst cmpwi value unbound-marker-widetag)
    (inst b? (if not-p :eq :ne) target))
  #-sb-thread
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-lowtag)
    (inst cmpwi value unbound-marker-widetag)
    (inst b? (if not-p :eq :ne) target)))

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
    (inst cmpw value null-tn)
    (let ((err-lab (generate-error-code vop 'undefined-fun-error obj-temp)))
      (inst beq err-lab))))

(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg))
         (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:generator 38
      (load-type type function (- fun-pointer-lowtag))
      (inst cmpwi type simple-fun-widetag)
      ;;(inst mr lip function)
      (inst addi lip function
            (- (ash simple-fun-insts-offset word-shift) fun-pointer-lowtag))
      (inst beq SIMPLE)
      (load-asm-rtn-addr lip 'closure-tramp)
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
    (load-asm-rtn-addr temp 'undefined-tramp)
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)))

;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.
#+sb-thread
(define-vop (dynbind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp tls-index)
  (:generator 5
     (loadw tls-index symbol symbol-tls-index-slot other-pointer-lowtag)
     (inst twi :eq tls-index 0)
     (inst lwzx temp thread-base-tn tls-index)
     (inst addi bsp-tn bsp-tn (* binding-size n-word-bytes))
     (storew temp bsp-tn (- binding-value-slot binding-size))
     (storew tls-index bsp-tn (- binding-symbol-slot binding-size))
     (inst stwx val thread-base-tn tls-index)))

#-sb-thread
(define-vop (dynbind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst addi bsp-tn bsp-tn (* binding-size n-word-bytes))
    (storew temp bsp-tn (- binding-value-slot binding-size))
    (storew symbol bsp-tn (- binding-symbol-slot binding-size))
    (storew val symbol symbol-value-slot other-pointer-lowtag)))

#+sb-thread
(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) tls-index value)
  (:generator 0
    (loadw tls-index bsp-tn (- binding-symbol-slot binding-size))
    (loadw value bsp-tn (- binding-value-slot binding-size))
    (inst stwx value thread-base-tn tls-index)
    (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))
    (storew zero-tn bsp-tn (- binding-value-slot binding-size))
    (inst subi bsp-tn bsp-tn (* binding-size n-word-bytes))))

#-sb-thread
(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:generator 0
    (loadw symbol bsp-tn (- binding-symbol-slot binding-size))
    (loadw value bsp-tn (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))
    (storew zero-tn bsp-tn (- binding-value-slot binding-size))
    (inst subi bsp-tn bsp-tn (* binding-size n-word-bytes))))


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
      #+sb-thread
      (inst stwx value thread-base-tn symbol)
      #-sb-thread
      (storew value symbol symbol-value-slot other-pointer-lowtag)
      (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))

      (emit-label skip)
      (storew zero-tn bsp-tn (- binding-value-slot binding-size))
      (inst subi bsp-tn bsp-tn (* binding-size n-word-bytes))
      (inst cmpw where bsp-tn)
      (inst bne loop)

      (emit-label done))))



;;;; Closure indexing.

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
    (inst srwi res res instance-length-shift)))

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

(define-vop (%instance-cas word-index-cas)
  (:policy :fast-safe)
  (:translate %instance-cas)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance tagged-num * *))
(define-vop (%raw-instance-cas/word %instance-cas)
  (:args (object)
         (index)
         (old-value :scs (unsigned-reg))
         (new-value :scs (unsigned-reg)))
  (:arg-types * tagged-num unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:translate %raw-instance-cas/word))


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
  (:temporary (:scs (non-descriptor-reg)) temp #+gencgc card)
  #+gencgc (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:generator 10
    #+cheneygc
    (progn (inst addi temp index (- other-pointer-lowtag))
           (inst stwx value object temp))
    #+gencgc
    (progn
    ;; Load the card mask
    (inst lr temp (make-fixup "gc_card_table_mask" :foreign-dataref)) ; address of linkage entry
    (loadw temp temp)      ; address of gc_card_table_mask
    (inst lwz temp temp 0) ; value of gc_card_table_mask
    (pseudo-atomic (pa-flag)
      ;; Compute card mark index
      ;; Maybe these 2 steps should be one RLWINM, but I'm not that clever.
      (inst srawi card object gencgc-card-shift)
      (inst and card card temp)
      ;; Load mark table base
      (inst lr temp (make-fixup "gc_card_mark" :foreign-dataref)) ; address of linkage entry
      (loadw temp temp) ; address of gc_card_mark
      (loadw temp temp) ; value of gc_card_mark
      ;; Touch the card mark byte.
      (inst stbx null-tn temp card)
      ;; set 'written' flag in the code header
      ;; If two threads get here at the same time, they'll write the same byte.
      (let ((byte #+big-endian (- other-pointer-lowtag) #+little-endian (bug "Wat")))
        (inst lbz temp object byte)
        (inst ori temp temp #x40)
        (inst stb temp object byte))
      (inst addi temp index (- other-pointer-lowtag))
      (inst stwx value object temp)))))

;;;; raw instance slot accessors

(defun offset-for-raw-slot (index &optional (displacement 0))
  (- (ash (+ index displacement instance-slots-offset) word-shift)
     instance-pointer-lowtag))

(macrolet ((def (suffix sc primtype)
             `(progn
                (define-vop (,(symbolicate "%RAW-INSTANCE-REF/" suffix) word-index-ref)
                  (:policy :fast-safe)
                  (:translate ,(symbolicate "%RAW-INSTANCE-REF/" suffix))
                  (:variant instance-slots-offset instance-pointer-lowtag)
                  (:arg-types instance positive-fixnum)
                  (:results (value :scs (,sc)))
                  (:result-types ,primtype))
                (define-vop (,(symbolicate "%RAW-INSTANCE-SET/" suffix) word-index-set)
                  (:policy :fast-safe)
                  (:translate ,(symbolicate "%RAW-INSTANCE-SET/" suffix))
                  (:variant instance-slots-offset instance-pointer-lowtag)
                  (:arg-types instance positive-fixnum ,primtype)
                  (:args (object) (index) (value :scs (,sc)))))))
  (def word unsigned-reg unsigned-num)
  (def signed-word signed-reg signed-num))

(define-vop (raw-instance-atomic-incf/word)
  (:translate %raw-instance-atomic-incf/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)) ; FIXME: allow immediate
         (diff :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num)
  (:temporary (:sc unsigned-reg) offset)
  (:temporary (:sc non-descriptor-reg) sum)
  (:results (result :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:generator 4
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    ;; load the slot value, add DIFF, write the sum back, and return
    ;; the original slot value, atomically, and include a memory
    ;; barrier.
    (inst sync)
    LOOP
    (inst lwarx result offset object)
    (inst add sum result diff)
    (inst stwcx. sum offset object)
    (inst bne LOOP)
    (inst isync)))

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
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (inst lfsx value object offset)))

(define-vop ()
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg)))
  (:arg-types * positive-fixnum single-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (inst stfsx value object offset)))

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
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (inst lfdx value object offset)))

(define-vop ()
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg)))
  (:arg-types * positive-fixnum double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (inst stfdx value object offset)))

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
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (inst lfsx (complex-single-reg-real-tn value) object offset)
    (inst addi offset offset n-word-bytes)
    (inst lfsx (complex-single-reg-imag-tn value) object offset)))

(define-vop ()
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg)))
  (:arg-types * positive-fixnum complex-single-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (inst stfsx (complex-single-reg-real-tn value) object offset)
    (inst addi offset offset n-word-bytes)
    (inst stfsx (complex-single-reg-imag-tn value) object offset)))

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
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (inst lfdx (complex-double-reg-real-tn value) object offset)
    (inst addi offset offset (* 2 n-word-bytes))
    (inst lfdx (complex-double-reg-imag-tn value) object offset)))

(define-vop ()
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg)))
  (:arg-types * positive-fixnum complex-double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (inst stfdx (complex-double-reg-real-tn value) object offset)
    (inst addi offset offset (* 2 n-word-bytes))
    (inst stfdx (complex-double-reg-imag-tn value) object offset)))
