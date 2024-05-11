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
         (value :scs (descriptor-reg any-reg zero)))
  (:info name offset lowtag)
  (:results)
  (:vop-var vop)
  (:generator 1
    (emit-gengc-barrier object nil tmp-tn (vop-nth-arg 1 vop) value name)
    (storew value object offset lowtag)))

(define-vop (compare-and-swap-slot)
  (:args (object :scs (descriptor-reg))
         (old :scs (descriptor-reg any-reg zero))
         (new :scs (descriptor-reg any-reg zero)))
  (:info name offset lowtag)
  (:ignore name)
  (:temporary (:sc non-descriptor-reg) lip)
  (:results (result :scs (descriptor-reg any-reg) :from :load))
  (:vop-var vop)
  (:generator 5
    (emit-gengc-barrier object nil lip (vop-nth-arg 2 vop) new)
    (inst add-sub lip object (- (* offset n-word-bytes) lowtag))
    (cond ((member :arm-v8.1 *backend-subfeatures*)
           (move result old)
           (inst casal result new lip))
          (t
           (assemble ()
             (inst dsb)
             LOOP
             (inst ldxr result lip)
             (inst cmp result old)
             (inst b :ne EXIT)
             (inst stlxr tmp-tn new lip)
             (inst cbnz tmp-tn LOOP)
             EXIT
             (inst clrex)
             (inst dmb))))))

;;;; Symbol hacking VOPs:

;;; Do a cell ref with an error check for being unbound.
;;;
(define-vop (checked-cell-ref)
  (:args (object :scs (descriptor-reg) :to :save))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only))

;;; With Symbol-Value, we check that the value isn't the trap object.
#+sb-thread
(progn
  (eval-when (:compile-toplevel)
    ;; Assert that "CMN reg, 1" is the same as "CMP reg, NO-TLS-VALUE-MARKER"
    (aver (= (ldb (byte 64 0) -1) no-tls-value-marker)))
  (defun no-tls-marker (x symbol set target)
    ;; Pointers wouldn't have the sign bit set (unless they are
    ;; tagged, which they shouldn't be)
    (let* ((symbol (cond ((symbolp symbol)
                          symbol)
                         ((sc-is symbol constant)
                          (tn-value symbol))
                         ))
           (pointer (and symbol
                         (not (types-equal-or-intersect
                               (info :variable :type symbol)
                               (specifier-type '(or integer single-float)))))))
      (cond (pointer
             (if set
                 (inst tbnz* x 63 target)
                 (inst tbz* x 63 target)))
            (t
             (inst cmn x 1)
             (inst b (if set :eq :ne) target)))))
  (define-vop (set)
    (:args (object :scs (descriptor-reg)
                   :load-if (not (symbol-always-has-tls-value-p object node)))
           (value :scs (descriptor-reg any-reg zero)))
    (:temporary (:sc any-reg) tls-index)
    (:node-var node)
    (:vop-var vop)
    (:generator 4
      (sc-case object
        (constant
         (inst str value (@ thread-tn (make-fixup (tn-value object) :symbol-tls-index))))
        (t
         (assemble ()
           (inst ldr (32-bit-reg tls-index) (tls-index-of object))
           (inst ldr tmp-tn (@ thread-tn tls-index))
           (no-tls-marker tmp-tn object nil LOCAL)
           (emit-gengc-barrier object nil tls-index (vop-nth-arg 1 vop) value)
           (storew value object symbol-value-slot other-pointer-lowtag)
           (inst b DONE)
           LOCAL
           (inst str value (@ thread-tn tls-index))
           DONE)))))

  (define-vop (symbol-value checked-cell-ref)
    (:translate symbol-value)
    (:args (symbol :scs (descriptor-reg) :to :save
                   :load-if (not (and (sc-is symbol constant)
                                      (let ((pkg (sb-xc:symbol-package (tn-value symbol))))
                                        (or (and pkg (system-package-p pkg))
                                            (eq pkg *cl-package*)))))))
    (:arg-refs symbol-tn-ref)
    (:temporary (:sc any-reg) tls-index)
    (:variant-vars check-boundp)
    (:variant t)
    (:node-var node)
    (:generator 9
      (let* ((known-symbol-p (sc-is symbol constant))
             (known-symbol (and known-symbol-p (tn-value symbol)))
             (fixup (and known-symbol
                         (make-fixup known-symbol :symbol-tls-index))))
        (cond
          (t
           (cond
             (known-symbol              ; e.g. CL:*PRINT-BASE*
              ;; Known nonzero TLS index, but possibly no per-thread value.
              ;; The TLS value and global value can be loaded independently.
              (inst ldr value (@ thread-tn fixup)))
             (t
              (inst ldr (32-bit-reg tls-index) (tls-index-of symbol))
              (inst ldr value (@ thread-tn tls-index))))
           (unless (symbol-always-has-tls-value-p symbol-tn-ref node)
             (assemble ()
               (no-tls-marker value symbol nil LOCAL)
               (when known-symbol
                 (load-constant vop symbol (setf symbol (tn-ref-load-tn symbol-tn-ref))))
               (loadw value symbol symbol-value-slot other-pointer-lowtag)
               LOCAL)))))

      (when check-boundp
        (assemble ()
          (let* ((*location-context* (make-restart-location RETRY value))
                 (err-lab (generate-error-code vop 'unbound-symbol-error symbol)))
            (inst cmp value unbound-marker-widetag)
            (inst b :eq err-lab))
          RETRY))))

  (define-vop (fast-symbol-value symbol-value)
    (:policy :fast)
    (:variant nil)
    (:variant-cost 5)))

#-sb-thread
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
    (:translate symbol-value)))

(define-vop (boundp)
  (:args (object :scs (descriptor-reg)))
  (:conditional)
  (:info target not-p)
  (:policy :fast-safe)
  (:temporary (:scs (descriptor-reg)) value)
  (:translate boundp)
  #+sb-thread
  (:generator 9
    (inst ldr (32-bit-reg value) (tls-index-of object))
    (inst ldr value (@ thread-tn value))
    (no-tls-marker value object nil LOCAL)
    (loadw value object symbol-value-slot other-pointer-lowtag)
    LOCAL
    (inst cmp value unbound-marker-widetag)
    (inst b (if not-p :eq :ne) target))
  #-sb-thread
  (:generator 9
      (loadw value object symbol-value-slot other-pointer-lowtag)
      (inst cmp value unbound-marker-widetag)
      (inst b (if not-p :eq :ne) target)))

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
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst lsr res res n-symbol-hash-discard-bits)))
(define-vop (symbol-name-hash symbol-hash)
  (:translate symbol-name-hash #-relocatable-static-space hash-as-if-symbol-name)
  (:generator 1 ; ASSUMPTION: little-endian
    (inst ldr (32-bit-reg res)
          (@ symbol (- (+ 4 (ash symbol-hash-slot word-shift)) other-pointer-lowtag)))))

(define-vop ()
  (:args (symbol :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:translate symbol-package-id)
  (:policy :fast-safe)
  (:generator 1
    #.(assert (= sb-impl::package-id-bits 16))
    (inst ldrh result (@ symbol (+ (ash symbol-name-slot word-shift)
                                  (- other-pointer-lowtag)
                                  6))))) ; little-endian
(define-vop ()
  (:policy :fast-safe)
  (:translate symbol-name)
  (:args (symbol :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:generator 5
    (pseudo-atomic (tmp-tn :sync nil)
      (loadw tmp-tn symbol symbol-name-slot other-pointer-lowtag)
      (inst and result tmp-tn (1- (ash 1 sb-impl::symbol-name-bits))))))

;; Is it worth eliding the GC store barrier for a thread-local CAS?
;; Not really, because why does such an operation even exists?
;; No other thread can see our TLS except via SAP-REF.
(define-vop (%compare-and-swap-symbol-value)
  (:translate %compare-and-swap-symbol-value)
  (:args (symbol :scs (descriptor-reg))
         (old :scs (descriptor-reg any-reg zero))
         (new :scs (descriptor-reg any-reg zero)))
  (:results (result :scs (descriptor-reg any-reg) :from :load))
  #+sb-thread
  (:temporary (:sc any-reg) tls-index)
  (:temporary (:sc non-descriptor-reg) lip)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 15
    (emit-gengc-barrier symbol nil lip (vop-nth-arg 2 vop) new)
    (inst dsb)
    #+sb-thread
    (assemble ()
      (inst ldr (32-bit-reg tls-index) (tls-index-of symbol))
      ;; Thread-local area, no synchronization needed.
      (inst ldr result (@ thread-tn tls-index))
      (inst cmp result old)
      (inst b :ne DONT-STORE-TLS)
      (inst str new (@ thread-tn tls-index))
      DONT-STORE-TLS

      (no-tls-marker result symbol nil CHECK-UNBOUND))
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

(define-vop (%compare-and-swap-symbol-value-v8.1)
  (:translate %compare-and-swap-symbol-value)
  (:args (symbol :scs (descriptor-reg))
         (old :scs (descriptor-reg any-reg))
         (new :scs (descriptor-reg any-reg)))
  (:results (result :scs (descriptor-reg any-reg) :from :load))
  #+sb-thread
  (:temporary (:sc any-reg) tls-index)
  (:temporary (:sc non-descriptor-reg) lip)
  (:policy :fast-safe)
  (:vop-var vop)
  (:guard (member :arm-v8.1 *backend-subfeatures*))
  (:generator 14
    (emit-gengc-barrier symbol nil lip (vop-nth-arg 2 vop) new)
    #+sb-thread
    (assemble ()
      (inst ldr (32-bit-reg tls-index) (tls-index-of symbol))
      ;; Thread-local area, no synchronization needed.
      (inst ldr result (@ thread-tn tls-index))
      (inst cmp result old)
      (inst b :ne DONT-STORE-TLS)
      (inst str new (@ thread-tn tls-index))
      DONT-STORE-TLS

      (no-tls-marker result symbol nil CHECK-UNBOUND))
    (inst add-sub lip symbol (- (* symbol-value-slot n-word-bytes)
                                other-pointer-lowtag))
    (move result old)
    (inst casal result new lip)
    CHECK-UNBOUND
    (inst cmp result unbound-marker-widetag)
    (inst b :eq (generate-error-code vop 'unbound-symbol-error symbol))))


;;;; Fdefinition (fdefn) objects.

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
    (inst b :eq
          (let ((*location-context* (make-restart-location RETRY value)))
            (generate-error-code vop 'undefined-fun-error object)))
    RETRY))

(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:args (function :scs (descriptor-reg))
         (fdefn :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) lip)
  (:temporary (:scs (non-descriptor-reg)) type)
  (:generator 38
    (emit-gengc-barrier fdefn nil lip)
    (inst add-sub lip function (- (* simple-fun-insts-offset n-word-bytes)
                                  fun-pointer-lowtag))
    (load-type type function (- fun-pointer-lowtag))
    (inst cmp type simple-fun-widetag)
    (inst b :eq SIMPLE-FUN)
    (load-asm-routine lip 'closure-tramp)
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
    (load-asm-routine temp 'undefined-tramp)
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)))


;;;; Binding and Unbinding.

;;; BIND -- Establish VAL as a binding for SYMBOL.  Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
#+sb-thread
(progn
  (define-vop (dynbind)
    (:args (value :scs (any-reg descriptor-reg zero) :to :save)
           (symbol :scs (descriptor-reg)
                   :target alloc-tls-symbol))
    (:temporary (:sc descriptor-reg :offset r8-offset :from (:argument 1)) alloc-tls-symbol)
    (:temporary (:sc non-descriptor-reg :offset nl0-offset) tls-index)
    (:temporary (:sc non-descriptor-reg :offset nl1-offset) free-tls-index)
    (:temporary (:sc non-descriptor-reg :offset lr-offset) lr)
    (:ignore free-tls-index)
    (:temporary (:scs (any-reg)) bsp)
    (:generator 5
      (load-binding-stack-pointer bsp)
      (inst add bsp bsp (* binding-size n-word-bytes))
      (store-binding-stack-pointer bsp)
      (inst ldr (32-bit-reg tls-index) (tls-index-of symbol))
      (inst cbnz (32-bit-reg tls-index) TLS-INDEX-VALID)
      (move alloc-tls-symbol symbol)
      (invoke-asm-routine 'alloc-tls-index lr)
      TLS-INDEX-VALID
      (inst ldr alloc-tls-symbol (@ thread-tn tls-index))
      (inst stp alloc-tls-symbol tls-index
        (@ bsp (* (- binding-value-slot binding-size)
                 n-word-bytes)))
      (inst str value (@ thread-tn tls-index))))

  (defun load-time-tls-index (symbol)
    (let ((component (component-info *component-being-compiled*)))
      (ash (or (position-if (lambda (x)
                              (and (typep x '(cons (eql :tls-index)))
                                   (eq (cadr x) symbol)))
                            (ir2-component-constants component))
               (vector-push-extend (list :tls-index symbol)
                                   (ir2-component-constants component)))
           word-shift)))

  (defun bind (bsp symbol tls-index tls-value &optional bsp-loaded)
    (unless bsp-loaded
      (load-binding-stack-pointer bsp)
      (inst add tls-index bsp (* binding-size n-word-bytes))
      (store-binding-stack-pointer tls-index))
    (let* ((pkg (sb-xc:symbol-package symbol))
           ;; These symbols should have a small enough index to be
           ;; immediately encoded.
           (known-symbol-p (or (and pkg (system-package-p pkg))
                               (eq pkg *cl-package*)))
           (tls-index-reg tls-index)
           (tls-index (if known-symbol-p
                          (make-fixup symbol :symbol-tls-index)
                          tls-index)))
      (cond (known-symbol-p
             (inst movz tls-index-reg tls-index))
            (t
             ;; TODO: a fixup could replace this with an immediate
             (inst load-constant tls-index (load-time-tls-index symbol))))
      (inst ldr tls-value (@ thread-tn tls-index))
      (inst stp tls-value tls-index-reg (if (and bsp-loaded
                                                 (neq bsp-loaded :last))
                                            (@ bsp (* binding-size n-word-bytes) :post-index)
                                            (@ bsp)))
      tls-index-reg))

  (define-vop (bind)
    (:args (value :scs (any-reg descriptor-reg zero) :to :save))
    (:temporary (:sc non-descriptor-reg) tls-index)
    (:temporary (:sc descriptor-reg) tls-value)
    (:info symbol)
    (:temporary (:scs (any-reg)) bsp)
    (:generator 5
      (inst str value (@ thread-tn (bind bsp symbol tls-index tls-value)))))

  (define-vop (rebind)
    (:temporary (:sc non-descriptor-reg) tls-index)
    (:temporary (:sc descriptor-reg) value tls-value)
    (:info symbol)
    (:vop-var vop)
    (:node-var node)
    (:temporary (:scs (any-reg)) bsp)
    (:generator 5
      (let ((tls-index (bind bsp symbol tls-index tls-value)))
        (unless (symbol-always-has-tls-value-p symbol node)
          (assemble ()
            (no-tls-marker tls-value symbol nil DONE)
            (load-constant vop (emit-constant symbol) value)
            (loadw value value symbol-value-slot other-pointer-lowtag)
            (inst str value (@ thread-tn tls-index))
            DONE)))))

  (define-vop (bind-n)
    (:args (values :more t :scs (descriptor-reg any-reg control-stack constant immediate)))
    (:temporary (:sc non-descriptor-reg) tls-index)
    (:temporary (:sc descriptor-reg) tls-value temp)
    (:info symbols)
    (:temporary (:scs (any-reg)) bsp)
    (:vop-var vop)
    (:generator 5
      (load-binding-stack-pointer bsp)
      (inst add tls-value bsp (add-sub-immediate (* binding-size n-word-bytes (length symbols))))
      (store-binding-stack-pointer tls-value)
      (let (prev-constant)
        (loop for (symbol . next) on symbols
              do
              (inst str (maybe-load (tn-ref-tn values))
                    (@ thread-tn (bind bsp symbol tls-index tls-value (or next
                                                                          :last))))
              (setf values (tn-ref-across values))))))

  (define-vop (unbind-n)
    (:info symbols)
    (:temporary (:sc descriptor-reg) value)
    (:temporary (:sc any-reg) tls-index bsp)
    (:generator 0
      (load-binding-stack-pointer bsp)
      (loop for symbol in symbols
            do
            (inst ldp value tls-index (@ bsp (* (- binding-value-slot binding-size)
                                                n-word-bytes)))
            (inst str value (@ thread-tn tls-index))

            ;; The order of stores here is reversed with respect to interrupt safety,
            ;; but STP cannot be interrupted in the middle.
            (inst stp zr-tn zr-tn (@ bsp (* (- binding-value-slot binding-size)
                                            n-word-bytes)
                                         :pre-index)))
      (store-binding-stack-pointer bsp))))

(defun unbind-to-here (where symbol value bsp)
  (assemble ()
    (load-binding-stack-pointer bsp)
    (inst cmp where bsp)
    (inst b :eq DONE)

    LOOP
    ;; on sb-thread SYMBOL is actually a tls-index
    (inst ldp value symbol (@ bsp (* (- binding-value-slot binding-size)
                                     n-word-bytes)))
    (inst cbz symbol ZERO)
    #-sb-thread
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    #+sb-thread
    (inst str value (@ thread-tn symbol))
    ZERO
    (inst stp zr-tn zr-tn (@ bsp (* (- binding-value-slot binding-size)
                                    n-word-bytes)
                                 :pre-index))

    (inst cmp where bsp)
    (inst b :ne LOOP)

    DONE
    (store-binding-stack-pointer bsp)))

(define-vop (unbind-to-here)
  (:args (arg :scs (descriptor-reg any-reg) :target where))
  (:temporary (:scs (any-reg) :from (:argument 0)) where)
  (:temporary (:scs (descriptor-reg)) symbol value)
  (:temporary (:scs (any-reg)) bsp)
  (:generator 0
    (move where arg)
    (unbind-to-here where symbol value bsp)))

#-sb-thread
(progn
  (define-vop (dynbind)
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

(define-full-setter %closure-index-set *
  closure-info-offset fun-pointer-lowtag
  (descriptor-reg any-reg) * %closure-index-set)

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
    (inst lsr res res instance-length-shift)))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (descriptor-reg any-reg) * %instance-set)

(define-vop (instance-set-multiple)
  (:args (instance :scs (descriptor-reg))
         (values :more t :scs (descriptor-reg any-reg)))
  (:temporary (:sc descriptor-reg) val-temp)
  (:info indices)
  (:vop-var vop)
  (:generator 1
    (emit-gengc-barrier instance nil tmp-tn values)
    (do ((tn-ref values (tn-ref-across tn-ref))
         (indices indices (cdr indices)))
        ((null tn-ref))
      (let* ((value (tn-ref-tn tn-ref))
             (source
               (sc-case value
                 (immediate
                  (load-immediate vop value val-temp)
                  val-temp)
                 (constant
                  (inst load-constant val-temp (tn-byte-offset value))
                  val-temp)
                 (control-stack
                  (load-stack-tn val-temp value)
                  val-temp)
                 ((any-reg descriptor-reg)
                  value))))
        (inst str source
              (@ instance (load-store-offset
                           (- (ash (+ instance-slots-offset (car indices)) word-shift)
                              instance-pointer-lowtag))))))))

(define-vop (%instance-cas word-index-cas)
  (:policy :fast-safe)
  (:translate %instance-cas)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance tagged-num * *))
(define-vop (%raw-instance-cas/word %instance-cas)
  (:args (object)
         (index)
         (old-value :scs (unsigned-reg))
         (new-value :scs (unsigned-reg zero)))
  (:arg-types * tagged-num unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:translate %raw-instance-cas/word))

(define-vop (%raw-instance-cas/signed-word %instance-cas)
  (:args (object)
         (index)
         (old-value :scs (signed-reg))
         (new-value :scs (signed-reg zero)))
  (:arg-types * tagged-num signed-num signed-num)
  (:results (result :scs (signed-reg) :from :load))
  (:result-types signed-num)
  (:translate %raw-instance-cas/signed-word))

(define-vop (%instance-cas-v8.1 word-index-cas-v8.1)
  (:policy :fast-safe)
  (:translate %instance-cas)
  (:variant instance-slots-offset instance-pointer-lowtag)
  (:arg-types instance tagged-num * *))

(define-vop (%raw-instance-cas/word-v8.1 %instance-cas-v8.1)
  (:args (object)
         (index)
         (old-value :scs (unsigned-reg) :target result)
         (new-value :scs (unsigned-reg zero)))
  (:arg-types * tagged-num unsigned-num unsigned-num)
  (:results (result :scs (unsigned-reg) :from (:argument 2)))
  (:result-types unsigned-num)
  (:translate %raw-instance-cas/word))

(define-vop (%raw-instance-cas/signed-word-v8.1 %instance-cas-v8.1)
  (:args (object)
         (index)
         (old-value :scs (signed-reg) :target result)
         (new-value :scs (signed-reg zero)))
  (:arg-types * tagged-num signed-num signed-num)
  (:results (result :scs (signed-reg) :from (:argument 2)))
  (:result-types signed-num)
  (:translate %raw-instance-cas/signed-word))

;;;; Code object frobbing.

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (descriptor-reg any-reg) * code-header-ref)

#-darwin-jit
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
    (pseudo-atomic (pa-flag)
      #+immobile-space
      (progn
        #-sb-thread
        (error "doesn't work yet")
        (loadw temp thread-tn thread-text-space-addr-slot)
        (inst sub temp object temp)
        (inst lsr card temp (1- (integer-length immobile-card-bytes)))
        (loadw temp thread-tn thread-text-card-count-slot)
        (inst cmp card temp)
        (inst b :hs try-dynamic-space)
        (loadw temp thread-tn thread-text-card-marks-slot)

        ;; compute &((unsigned long*)text_page_touched_bits)[page_index/64]
        (inst lsr pa-flag card 6)
        (inst lsl pa-flag pa-flag 3)
        (inst add temp temp pa-flag)

        ;; compute 1U << (page_index & 63)
        (inst mov pa-flag 1)
        (inst and card card 63)
        (inst lsl pa-flag pa-flag card)

        (cond ((member :arm-v8.1 *backend-subfeatures*)
               (inst ldset pa-flag pa-flag temp))
              (t
               (let ((loop (gen-label)))
                 (emit-label LOOP)
                 (inst ldaxr card temp)
                 (inst orr card card pa-flag)
                 (inst stlxr pa-flag card temp)
                 (inst cbnz (32-bit-reg pa-flag) LOOP))))
        (inst b store))
      TRY-DYNAMIC-SPACE
      (load-foreign-symbol temp "gc_card_table_mask" :dataref t)
      (inst ldr (32-bit-reg temp) (@ temp)) ; 4-byte int
      ;; Compute card mark index
      (inst lsr card object gencgc-card-shift)
      (inst and card card temp)
      ;; Load mark table base
      (load-foreign-symbol temp "gc_card_mark" :dataref t)
      (inst ldr temp (@ temp))
      ;; Touch the card mark byte.
      (inst strb null-tn (@ temp card))

      STORE
      ;; set 'written' flag in the code header
      ;; If two threads get here at the same time, they'll write the same byte.
      (let ((byte (- #+big-endian 4 #+little-endian 3 other-pointer-lowtag)))
        (inst ldrb temp (@ object byte))
        (inst orr temp temp #x40)
        (inst strb temp (@ object byte)))
      (inst lsl temp index (- word-shift n-fixnum-tag-bits))
      (inst sub temp temp other-pointer-lowtag)
      (inst str value (@ object temp)))))

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
           `(progn
              (define-vop ()
                (:translate ,(symbolicate "%RAW-INSTANCE-REF/" name))
                (:policy :fast-safe)
                (:args (object :scs (descriptor-reg))
                       (index :scs (any-reg immediate)))
                (:arg-types * positive-fixnum)
                (:results (value :scs (,value-sc)))
                (:result-types ,value-primtype)
                (:temporary (:scs (non-descriptor-reg)) offset)
                (:generator 5 ,@(emit-generator 'ldr nil)))
              (define-vop ()
                (:translate ,(symbolicate "%RAW-INSTANCE-SET/" name))
                (:policy :fast-safe)
                (:args (object :scs (descriptor-reg))
                       (index :scs (any-reg immediate))
                       (value :scs (,value-sc)))
                (:arg-types * positive-fixnum ,value-primtype)
                (:temporary (:scs (non-descriptor-reg)) offset)
                (:generator 5 ,@(emit-generator 'str nil)))))))
  (define-raw-slot-vops word unsigned-num unsigned-reg)
  (define-raw-slot-vops signed-word signed-num signed-reg)
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
  (:temporary (:sc non-descriptor-reg) lip)
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

(define-vop (raw-instance-atomic-incf/word-v8.1)
  (:translate %raw-instance-atomic-incf/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (diff :scs (unsigned-reg)))
  (:arg-types * positive-fixnum unsigned-num)
  (:temporary (:sc non-descriptor-reg) lip)
  (:results (result :scs (unsigned-reg) :from :load))
  (:result-types unsigned-num)
  (:guard (member :arm-v8.1 *backend-subfeatures*))
  (:generator 3
    (inst add lip object (lsl index (- word-shift n-fixnum-tag-bits)))
    (inst add lip lip (- (* instance-slots-offset
                            n-word-bytes)
                         instance-pointer-lowtag))

    (inst ldaddal diff result lip)))
