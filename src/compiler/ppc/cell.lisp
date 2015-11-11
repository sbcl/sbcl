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

(define-vop (init-slot set-slot))

#!+compare-and-swap-vops
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

#!+compare-and-swap-vops
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
    #!+sb-thread
    (assemble ()
      (loadw temp symbol symbol-tls-index-slot other-pointer-lowtag)
      ;; Thread-local area, no synchronization needed.
      (inst lwzx result thread-base-tn temp)
      (inst cmpw result old)
      (inst bne DONT-STORE-TLS)
      (inst stwx new thread-base-tn temp)
      DONT-STORE-TLS

      (inst cmpwi result no-tls-value-marker-widetag)
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
;;; So SYMBOL-VALUE of NIL is NIL.
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

#!+sb-thread
(progn
  (define-vop (set)
    (:args (symbol :scs (descriptor-reg))
           (value :scs (descriptor-reg any-reg)))
    (:temporary (:sc any-reg) tls-slot temp)
    (:generator 4
      (loadw tls-slot symbol symbol-tls-index-slot other-pointer-lowtag)
      (inst lwzx temp thread-base-tn tls-slot)
      (inst cmpwi temp no-tls-value-marker-widetag)
      (inst beq GLOBAL-VALUE)
      (inst stwx value thread-base-tn tls-slot)
      (inst b DONE)
      GLOBAL-VALUE
      (storew value symbol symbol-value-slot other-pointer-lowtag)
      DONE))

  ;; With Symbol-Value, we check that the value isn't the trap object. So
  ;; Symbol-Value of NIL is NIL.
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
      (inst cmpwi value no-tls-value-marker-widetag)
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
      (inst cmpwi value no-tls-value-marker-widetag)
      (inst bne DONE)
      (loadw value object symbol-value-slot other-pointer-lowtag)
      DONE)))

;;; On unithreaded builds these are just copies of the global versions.
#!-sb-thread
(progn
  (define-vop (symbol-value symbol-global-value)
    (:translate symbol-value))
  (define-vop (fast-symbol-value fast-symbol-global-value)
    (:translate symbol-value))
  (define-vop (set %set-symbol-global-value)))

;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell
;;; is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value))

#!+sb-thread
(define-vop (boundp boundp-frob)
  (:translate boundp)
  (:generator 9
    (loadw value object symbol-tls-index-slot other-pointer-lowtag)
    (inst lwzx value thread-base-tn value)
    (inst cmpwi value no-tls-value-marker-widetag)
    (inst bne CHECK-UNBOUND)
    (loadw value object symbol-value-slot other-pointer-lowtag)
    CHECK-UNBOUND
    (inst cmpwi value unbound-marker-widetag)
    (inst b? (if not-p :eq :ne) target)))

#!-sb-thread
(define-vop (boundp boundp-frob)
  (:translate boundp)
  (:generator 9
    (loadw value object symbol-value-slot other-pointer-lowtag)
    (inst cmpwi value unbound-marker-widetag)
    (inst b? (if not-p :eq :ne) target)))

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
    (inst clrrwi res res n-fixnum-tag-bits)))

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
    (inst cmpw value null-tn)
    (let ((err-lab (generate-error-code vop 'undefined-fun-error obj-temp)))
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
      (inst lr lip  (make-fixup "closure_tramp" :foreign))
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
    (inst lr temp  (make-fixup "undefined_tramp" :foreign))
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result fdefn)))



;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.
#!+sb-thread
(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg :offset nl3-offset) pa-flag)
  (:temporary (:scs (descriptor-reg)) temp tls-index)
  (:generator 5
     (loadw tls-index symbol symbol-tls-index-slot other-pointer-lowtag)
     (inst cmpwi tls-index 0)
     (inst bne TLS-VALID)

     ;; No TLS slot allocated, so allocate one.
     (pseudo-atomic (pa-flag)
       (without-scheduling ()
         (assemble ()
           (inst li temp (+ (static-symbol-offset '*tls-index-lock*)
                            (ash symbol-value-slot word-shift)
                            (- other-pointer-lowtag)))
           OBTAIN-LOCK
           (inst lwarx tls-index null-tn temp)
           (inst cmpwi tls-index 0)
           (inst bne OBTAIN-LOCK)
           (inst stwcx. thread-base-tn null-tn temp)
           (inst bne OBTAIN-LOCK)
           (inst isync)

           ;; Check to see if the TLS index was set while we were waiting.
           (loadw tls-index symbol symbol-tls-index-slot other-pointer-lowtag)
           (inst cmpwi tls-index 0)
           (inst bne RELEASE-LOCK)

           (load-symbol-value tls-index *free-tls-index*)
           ;; FIXME: Check for TLS index overflow.
           (inst addi tls-index tls-index n-word-bytes)
           (store-symbol-value tls-index *free-tls-index*)
           (inst addi tls-index tls-index (- n-word-bytes))
           (storew tls-index symbol symbol-tls-index-slot other-pointer-lowtag)

           ;; The sync instruction doesn't need to happen if we branch
           ;; directly to RELEASE-LOCK as we didn't do any stores in that
           ;; case.
           (inst sync)
           RELEASE-LOCK
           (inst stwx zero-tn null-tn temp)

           ;; temp is a boxed register, but we've been storing crap in it.
           ;; fix it before we leave pseudo-atomic.
           (inst li temp 0))))

     TLS-VALID
     (inst lwzx temp thread-base-tn tls-index)
     (inst addi bsp-tn bsp-tn (* binding-size n-word-bytes))
     (storew temp bsp-tn (- binding-value-slot binding-size))
     (storew tls-index bsp-tn (- binding-symbol-slot binding-size))
     (inst stwx val thread-base-tn tls-index)))

#!-sb-thread
(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:scs (descriptor-reg)) temp)
  (:generator 5
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst addi bsp-tn bsp-tn (* binding-size n-word-bytes))
    (storew temp bsp-tn (- binding-value-slot binding-size))
    (storew symbol bsp-tn (- binding-symbol-slot binding-size))
    (storew val symbol symbol-value-slot other-pointer-lowtag)))

#!+sb-thread
(define-vop (unbind)
  (:temporary (:scs (descriptor-reg)) tls-index value)
  (:generator 0
    (loadw tls-index bsp-tn (- binding-symbol-slot binding-size))
    (loadw value bsp-tn (- binding-value-slot binding-size))
    (inst stwx value thread-base-tn tls-index)
    (storew zero-tn bsp-tn (- binding-symbol-slot binding-size))
    (storew zero-tn bsp-tn (- binding-value-slot binding-size))
    (inst subi bsp-tn bsp-tn (* binding-size n-word-bytes))))

#!-sb-thread
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
      #!+sb-thread
      (inst stwx value thread-base-tn symbol)
      #!-sb-thread
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

(define-vop (funcallable-instance-info word-index-ref)
  (:variant funcallable-instance-info-offset fun-pointer-lowtag)
  (:translate %funcallable-instance-info))

(define-vop (set-funcallable-instance-info word-index-set)
  (:variant funcallable-instance-info-offset fun-pointer-lowtag)
  (:translate %set-funcallable-instance-info))

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
    (inst srwi res temp n-widetag-bits)))

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

#!+compare-and-swap-vops
(define-vop (%instance-cas word-index-cas)
  (:policy :fast-safe)
  (:translate %instance-cas)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance tagged-num * *))


;;;; Code object frobbing.

(define-vop (code-header-ref word-index-ref)
  (:translate code-header-ref)
  (:policy :fast-safe)
  (:variant 0 other-pointer-lowtag))

(define-vop (code-header-set word-index-set)
  (:translate code-header-set)
  (:policy :fast-safe)
  (:variant 0 other-pointer-lowtag))



;;;; raw instance slot accessors

(defun offset-for-raw-slot (index &optional (displacement 0))
  (- (ash (+ index displacement instance-slots-offset) word-shift)
     instance-pointer-lowtag))

(define-vop (raw-instance-init/word)
  (:args (object :scs (descriptor-reg))
         (value :scs (unsigned-reg)))
  (:arg-types * unsigned-num)
  (:info index)
  (:generator 4
    (inst stw value object (offset-for-raw-slot index))))

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

(define-vop (raw-instance-ref/word word-index-ref)
  (:policy :fast-safe)
  (:translate %raw-instance-ref/word)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance positive-fixnum)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (raw-instance-set/word word-index-set)
  (:policy :fast-safe)
  (:translate %raw-instance-set/word)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance positive-fixnum unsigned-num)
  (:args (object) (index) (value :scs (unsigned-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num))

(define-vop (raw-instance-init/single)
  (:args (object :scs (descriptor-reg))
         (value :scs (single-reg)))
  (:arg-types * single-float)
  (:info index)
  (:generator 4
    (inst stfs value object (offset-for-raw-slot index))))

(define-vop (raw-instance-ref/single)
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

(define-vop (raw-instance-set/single)
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg) :target result))
  (:arg-types * positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (inst stfsx value object offset)
    (unless (location= result value)
      (inst frsp result value))))

(define-vop (raw-instance-init/double)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-reg)))
  (:arg-types * double-float)
  (:info index)
  (:generator 4
    (inst stfd value object (offset-for-raw-slot index))))

(define-vop (raw-instance-ref/double)
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

(define-vop (raw-instance-set/double)
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg) :target result))
  (:arg-types * positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (inst stfdx value object offset)
    (unless (location= result value)
      (inst fmr result value))))

(define-vop (raw-instance-init/complex-single)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-single-reg)))
  (:arg-types * complex-single-float)
  (:info index)
  (:generator 4
    (inst stfs (complex-single-reg-real-tn value)
          object (offset-for-raw-slot index))
    (inst stfs (complex-single-reg-imag-tn value)
          object (offset-for-raw-slot index 1))))

(define-vop (raw-instance-ref/complex-single)
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

(define-vop (raw-instance-set/complex-single)
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg) :target result))
  (:arg-types * positive-fixnum complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (let ((value-real (complex-single-reg-real-tn value))
          (result-real (complex-single-reg-real-tn result)))
      (inst stfsx value-real object offset)
      (unless (location= result-real value-real)
        (inst frsp result-real value-real)))
    (inst addi offset offset n-word-bytes)
    (let ((value-imag (complex-single-reg-imag-tn value))
          (result-imag (complex-single-reg-imag-tn result)))
      (inst stfsx value-imag object offset)
      (unless (location= result-imag value-imag)
        (inst frsp result-imag value-imag)))))

(define-vop (raw-instance-init/complex-double)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-double-reg)))
  (:arg-types * complex-double-float)
  (:info index)
  (:generator 4
    (inst stfd (complex-single-reg-real-tn value)
          object (offset-for-raw-slot index))
    (inst stfd (complex-double-reg-imag-tn value)
          object (offset-for-raw-slot index 2))))

(define-vop (raw-instance-ref/complex-double)
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

(define-vop (raw-instance-set/complex-double)
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg) :target result))
  (:arg-types * positive-fixnum complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:temporary (:scs (non-descriptor-reg)) offset)
  (:generator 5
    (inst addi offset index (- (ash instance-slots-offset word-shift)
                               instance-pointer-lowtag))
    (let ((value-real (complex-double-reg-real-tn value))
          (result-real (complex-double-reg-real-tn result)))
      (inst stfdx value-real object offset)
      (unless (location= result-real value-real)
        (inst fmr result-real value-real)))
    (inst addi offset offset (* 2 n-word-bytes))
    (let ((value-imag (complex-double-reg-imag-tn value))
          (result-imag (complex-double-reg-imag-tn result)))
      (inst stfdx value-imag object offset)
      (unless (location= result-imag value-imag)
        (inst fmr result-imag value-imag)))))
