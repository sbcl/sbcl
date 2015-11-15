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

(define-vop (compare-and-swap-slot)
  (:args (object :scs (descriptor-reg))
         (old :scs (descriptor-reg any-reg))
         (new :scs (descriptor-reg any-reg)))
  (:info name offset lowtag)
  (:ignore name)
  (:temporary (:sc interior-reg) lip)
  (:results (result :scs (descriptor-reg any-reg) :from :load))
  (:generator 5
    (inst dsb)
    (inst add-sub lip object (- (* offset n-word-bytes) lowtag))
    LOOP
    (inst ldxr result lip)
    (inst cmp result old)
    (inst b :ne EXIT)
    (inst stlxr tmp-tn new lip)
    (inst cbnz tmp-tn LOOP)
    EXIT
    (inst clrex)
    (inst dmb)))

;;;; Symbol hacking VOPs:

;;; Do a cell ref with an error check for being unbound.
;;;
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :to :save))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only))
;;; Like CHECKED-CELL-REF, only we are a predicate to see if the cell is bound.
(define-vop (boundp-frob)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value))

;;; With Symbol-Value, we check that the value isn't the trap object.  So
;;; Symbol-Value of NIL is NIL.
#!+sb-thread
(progn
  (define-vop (set)
    (:args (object :scs (descriptor-reg))
           (value :scs (descriptor-reg any-reg null)))
    (:temporary (:sc any-reg) tls-index)
    (:generator 4
      (inst ldr (32-bit-reg tls-index) (tls-index-of object))
      (inst ldr tmp-tn (@ thread-tn tls-index))
      (inst cmp tmp-tn no-tls-value-marker-widetag)
      (inst b :ne LOCAL)
      (storew value object symbol-value-slot other-pointer-lowtag)
      (inst b DONE)
      LOCAL
      (inst str value (@ thread-tn tls-index))
      DONE))

  (define-vop (symbol-value checked-cell-ref)
    (:translate symbol-value)
    (:temporary (:sc any-reg) tls-index)
    (:variant-vars check-boundp)
    (:variant t)
    (:generator 9
      (inst ldr (32-bit-reg tls-index) (tls-index-of object))
      (inst ldr value (@ thread-tn tls-index))
      (inst cmp value no-tls-value-marker-widetag)
      (inst b :ne LOCAL)
      (loadw value object symbol-value-slot other-pointer-lowtag)
      LOCAL
      (when check-boundp
        (let ((err-lab (generate-error-code vop 'unbound-symbol-error object)))
          (inst cmp value unbound-marker-widetag)
          (inst b :eq err-lab)))))

  (define-vop (fast-symbol-value symbol-value)
    (:policy :fast)
    (:variant nil)
    (:variant-cost 5))

  (define-vop (boundp boundp-frob)
    (:translate boundp)
    (:temporary (:sc any-reg) tls-index)
    (:generator 9
      (inst ldr (32-bit-reg tls-index) (tls-index-of object))
      (inst ldr value (@ thread-tn tls-index))
      (inst cmp value no-tls-value-marker-widetag)
      (inst b :ne LOCAL)
      (loadw value object symbol-value-slot other-pointer-lowtag)
      LOCAL
      (inst cmp value unbound-marker-widetag)
      (inst b (if not-p :eq :ne) target))))

#!-sb-thread
(progn
  (define-vop (set cell-set)
    (:variant symbol-value-slot other-pointer-lowtag))
  (define-vop (symbol-value checked-cell-ref)
    (:translate symbol-value)
    (:generator 9
                (loadw value object symbol-value-slot other-pointer-lowtag)
                (let ((err-lab (generate-error-code vop 'unbound-symbol-error object)))
                  (inst cmp value unbound-marker-widetag)
                  (inst b :eq err-lab))))
  (define-vop (fast-symbol-value cell-ref)
    (:variant symbol-value-slot other-pointer-lowtag)
    (:policy :fast)
    (:translate symbol-value))

  (define-vop (boundp boundp-frob)
    (:translate boundp)
    (:generator 9
      (loadw value object symbol-value-slot other-pointer-lowtag)
      (inst cmp value unbound-marker-widetag)
      (inst b (if not-p :eq :ne) target))))

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
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 9
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error object)))
      (loadw value object symbol-value-slot other-pointer-lowtag)
      (inst cmp value unbound-marker-widetag)
      (inst b :eq err-lab))))

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
    (inst and res temp (bic-mask fixnum-tag-mask))))

(define-vop (%compare-and-swap-symbol-value)
  (:translate %compare-and-swap-symbol-value)
  (:args (symbol :scs (descriptor-reg))
         (old :scs (descriptor-reg any-reg))
         (new :scs (descriptor-reg any-reg)))
  (:results (result :scs (descriptor-reg any-reg) :from :load))
  #!+sb-thread
  (:temporary (:sc any-reg) tls-index)
  (:temporary (:sc interior-reg) lip)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 15
    (inst dsb)
    #!+sb-thread
    (assemble ()
      (inst ldr (32-bit-reg tls-index) (tls-index-of symbol))
      ;; Thread-local area, no synchronization needed.
      (inst ldr result (@ thread-tn tls-index))
      (inst cmp result old)
      (inst b :ne DONT-STORE-TLS)
      (inst str new (@ thread-tn tls-index))
      DONT-STORE-TLS

      (inst cmp result no-tls-value-marker-widetag)
      (inst b :ne CHECK-UNBOUND))
    (inst add-sub lip symbol (- (* symbol-value-slot n-word-bytes)
                                other-pointer-lowtag))
    LOOP
    (inst ldxr result lip)
    (inst cmp result old)
    (inst b :ne CLEAR)
    (inst stlxr tmp-tn new lip)
    (inst cbnz tmp-tn LOOP)

    CLEAR
    (inst clrex)

    CHECK-UNBOUND
    (inst dmb)
    (inst cmp result unbound-marker-widetag)
    (inst b :eq (generate-error-code vop 'unbound-symbol-error symbol))))

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
    (inst cmp value null-tn)
    (inst b :eq (generate-error-code vop 'undefined-fun-error object))))

(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:translate (setf fdefn-fun))
  (:args (function :scs (descriptor-reg) :target result)
         (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (interior-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (inst add-sub lip function (- (* simple-fun-code-offset n-word-bytes)
                                  fun-pointer-lowtag))
    (load-type type function (- fun-pointer-lowtag))
    (inst cmp type simple-fun-header-widetag)
    (inst b :eq SIMPLE-FUN)
    (load-inline-constant lip '(:fixup "closure_tramp" :foreign) lip)
    SIMPLE-FUN
    (storew lip fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
    (move result function)))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (interior-reg)) lip)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (storew null-tn fdefn fdefn-fun-slot other-pointer-lowtag)
    (load-inline-constant temp '(:fixup "undefined_tramp" :foreign) lip)
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result fdefn)))



;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
#!+sb-thread
(progn
  (define-vop (bind)
    (:args (value :scs (any-reg descriptor-reg) :to :save)
           (symbol :scs (descriptor-reg)))
    (:temporary (:sc descriptor-reg) value-temp)
    (:temporary (:sc descriptor-reg :offset r0-offset :from (:argument 1)) alloc-tls-symbol)
    (:temporary (:sc non-descriptor-reg :offset nl0-offset) tls-index)
    (:temporary (:sc non-descriptor-reg :offset nl1-offset) free-tls-index)
    (:temporary (:sc interior-reg) lip)
    (:ignore free-tls-index)
    (:temporary (:scs (any-reg)) bsp)
     (:generator 5
      (load-binding-stack-pointer bsp)
      (inst ldr (32-bit-reg tls-index) (tls-index-of symbol))
      (inst add bsp bsp (* binding-size n-word-bytes))
      (store-binding-stack-pointer bsp)
      (inst cbnz (32-bit-reg tls-index) TLS-INDEX-VALID)
      (move alloc-tls-symbol symbol)
      (load-inline-constant value-temp '(:fixup alloc-tls-index :assembly-routine) lip)
      (inst blr value-temp)
      TLS-INDEX-VALID

      (inst ldr value-temp (@ thread-tn tls-index))
      (inst stp value-temp tls-index (@ bsp (* (- binding-value-slot binding-size)
                                               n-word-bytes)))
      (inst str value (@ thread-tn tls-index))))

  (define-vop (unbind)
    (:temporary (:sc descriptor-reg) value)
    (:temporary (:sc any-reg) tls-index bsp)
    (:generator 0
      (load-binding-stack-pointer bsp)
      (inst ldp value tls-index (@ bsp (* (- binding-value-slot binding-size)
                                          n-word-bytes)))
      (inst str value (@ thread-tn tls-index))

      ;; The order of stores here is reversed with respect to interrupt safety,
      ;; but STP cannot be interrupted in the middle.
      (inst stp zr-tn zr-tn (@ bsp (* (- binding-value-slot binding-size)
                                      n-word-bytes)
                                   :pre-index))
      (store-binding-stack-pointer bsp))))

(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (any-reg)) bsp)
  (:generator 0
    (load-binding-stack-pointer bsp)
    (move where arg)
    (inst cmp where bsp)
    (inst b :eq DONE)

    LOOP
    ;; on sb-thread SYMBOL is actually a tls-index
    (inst ldp value symbol (@ bsp (* (- binding-value-slot binding-size)
                                     n-word-bytes)))
    (inst cbz symbol ZERO)
    #!-sb-thread
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    #!+sb-thread
    (inst str value (@ thread-tn symbol))
    ZERO
    (inst stp zr-tn zr-tn (@ bsp (* (- binding-value-slot binding-size)
                                    n-word-bytes)
                                 :pre-index))

    (inst cmp where bsp)
    (inst b :ne LOOP)

    DONE
    (store-binding-stack-pointer bsp)))
#!-sb-thread
(progn
  (define-vop (bind)
    (:args (val :scs (any-reg descriptor-reg))
           (symbol :scs (descriptor-reg)))
    (:temporary (:scs (descriptor-reg)) value-temp)
    (:temporary (:scs (any-reg)) bsp-temp)
    (:generator 5
      (loadw value-temp symbol symbol-value-slot other-pointer-lowtag)
      (load-symbol-value bsp-temp *binding-stack-pointer*)
      (inst add bsp-temp bsp-temp (* binding-size n-word-bytes))
      (store-symbol-value bsp-temp *binding-stack-pointer*)
      (inst stp value-temp symbol (@ bsp-temp (* (- binding-value-slot binding-size) n-word-bytes)))
      (storew val symbol symbol-value-slot other-pointer-lowtag)))

 (define-vop (unbind)
   (:temporary (:scs (descriptor-reg)) symbol value)
   (:temporary (:scs (any-reg)) bsp-temp)
   (:generator 0
     (load-symbol-value bsp-temp *binding-stack-pointer*)
     (inst ldp value symbol (@ bsp-temp (* (- binding-value-slot binding-size)
                                           n-word-bytes)))
     (storew value symbol symbol-value-slot other-pointer-lowtag)
     ;; The order of stores here is reversed with respect to interrupt safety,
     ;; but STP cannot be interrupted in the middle.
     (inst stp zr-tn zr-tn (@ bsp-temp (* (- binding-value-slot binding-size)
                                          n-word-bytes)
                                       :pre-index))
     (store-symbol-value bsp-temp *binding-stack-pointer*))))

;;;; Closure indexing.

(define-full-reffer closure-index-ref *
  closure-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %closure-index-ref)

(define-full-setter set-funcallable-instance-info *
  funcallable-instance-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg null) * %set-funcallable-instance-info)

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
    (inst lsr res temp n-widetag-bits)))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg null) * %instance-set)

(define-vop (%instance-cas word-index-cas)
  (:policy :fast-safe)
  (:translate %instance-cas)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance tagged-num * *))

;;;; Code object frobbing.

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (descriptor-reg any-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-lowtag
  (descriptor-reg any-reg null) * code-header-set)

;;;; raw instance slot accessors

(macrolet
    ((define-raw-slot-vops (name value-primtype value-sc
                            &optional (move-macro 'move))
       (labels ((emit-generator (instruction move-result)
                  `((sc-case index
                      (immediate
                       (inst ,instruction value
                             (@ object
                                (load-store-offset (- (* (+ instance-slots-offset
                                                            (tn-value index))
                                                         n-word-bytes)
                                                      instance-pointer-lowtag)))))
                      (t
                       (inst lsl offset index (- word-shift n-fixnum-tag-bits))
                       (inst add offset offset (- (* instance-slots-offset
                                                     n-word-bytes)
                                                  instance-pointer-lowtag))
                       (inst ,instruction value (@ object offset))))
                    ,@(when move-result
                        `((,move-macro result value))))))
         (let ((ref-vop (symbolicate "RAW-INSTANCE-REF/" name))
               (set-vop (symbolicate "RAW-INSTANCE-SET/" name)))
           `(progn
              (define-vop (,ref-vop)
                (:translate ,(symbolicate "%" ref-vop))
                (:policy :fast-safe)
                (:args (object :scs (descriptor-reg))
                       (index :scs (any-reg immediate)))
                (:arg-types * positive-fixnum)
                (:results (value :scs (,value-sc)))
                (:result-types ,value-primtype)
                (:temporary (:scs (non-descriptor-reg)) offset)
                (:generator 5 ,@(emit-generator 'ldr nil)))
              (define-vop (,set-vop)
                (:translate ,(symbolicate "%" set-vop))
                (:policy :fast-safe)
                (:args (object :scs (descriptor-reg))
                       (index :scs (any-reg immediate))
                       (value :scs (,value-sc) :target result))
                (:arg-types * positive-fixnum ,value-primtype)
                (:results (result :scs (,value-sc)))
                (:result-types ,value-primtype)
                (:temporary (:scs (non-descriptor-reg)) offset)
                (:generator 5 ,@(emit-generator 'str t))))))))
  (define-raw-slot-vops word unsigned-num unsigned-reg)
  (define-raw-slot-vops single single-float single-reg
    move-float)
  (define-raw-slot-vops double double-float double-reg
    move-float)
  (define-raw-slot-vops complex-single complex-single-float complex-single-reg
    move-float)
  (define-raw-slot-vops complex-double complex-double-float complex-double-reg
    move-complex-double))

(define-vop (raw-instance-atomic-incf/word)
  (:translate %raw-instance-atomic-incf/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (diff :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num)
  (:temporary (:sc non-descriptor-reg) sum)
  (:temporary (:sc interior-reg) lip)
  (:results (result :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:generator 4
    (inst add lip object (lsl index (- word-shift n-fixnum-tag-bits)))
    (inst add lip lip (- (* instance-slots-offset
                            n-word-bytes)
                         instance-pointer-lowtag))

    (inst dsb)
    LOOP
    (inst ldxr result lip)
    (inst add sum result diff)
    (inst stlxr tmp-tn sum lip)
    (inst cbnz tmp-tn LOOP)
    (inst dmb)))
