;;;; various primitive memory access VOPs for the x86 VM

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
         (value :scs (descriptor-reg any-reg immediate)))
  (:info name offset lowtag)
  (:ignore name)
  (:results)
  (:generator 1
     (storew (encode-value-if-immediate value) object offset lowtag)))

(define-vop (init-slot set-slot))

(define-vop (compare-and-swap-slot)
  (:args (object :scs (descriptor-reg) :to :eval)
         (old :scs (descriptor-reg any-reg) :target eax)
         (new :scs (descriptor-reg any-reg)))
  (:temporary (:sc descriptor-reg :offset eax-offset
                   :from (:argument 1) :to :result :target result)
              eax)
  (:info name offset lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 5
     (move eax old)
     (inst cmpxchg (make-ea :dword :base object
                            :disp (- (* offset n-word-bytes) lowtag))
           new :lock)
     (move result eax)))

;;;; symbol hacking VOPs

(define-vop (%compare-and-swap-symbol-value)
  (:translate %compare-and-swap-symbol-value)
  (:args (symbol :scs (descriptor-reg) :to (:result 1))
         (old :scs (descriptor-reg any-reg) :target eax)
         (new :scs (descriptor-reg any-reg)))
  (:temporary (:sc descriptor-reg :offset eax-offset) eax)
  #!+sb-thread
  (:temporary (:sc descriptor-reg) tls)
  (:results (result :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 15
    ;; This code has to pathological cases: NO-TLS-VALUE-MARKER
    ;; or UNBOUND-MARKER as NEW: in either case we would end up
    ;; doing possible damage with CMPXCHG -- so don't do that!
    (let ((unbound (generate-error-code vop 'unbound-symbol-error symbol))
          (check (gen-label)))
      (move eax old)
      #!+sb-thread
      (progn
        (loadw tls symbol symbol-tls-index-slot other-pointer-lowtag)
        ;; Thread-local area, no LOCK needed.
        (with-tls-ea (EA :base tls :base-already-live-p t)
          (inst cmpxchg EA new :maybe-fs))
        (inst cmp eax no-tls-value-marker-widetag)
        (inst jmp :ne check)
        (move eax old))
      (inst cmpxchg (make-ea :dword :base symbol
                             :disp (- (* symbol-value-slot n-word-bytes)
                                      other-pointer-lowtag))
            new :lock)
      (emit-label check)
      (move result eax)
      (inst cmp result unbound-marker-widetag)
      (inst jmp :e unbound))))

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
      (inst jmp :e err-lab))))

#!+sb-thread
(progn
  (define-vop (set)
    (:args (symbol :scs (descriptor-reg))
           (value :scs (descriptor-reg any-reg)))
    (:temporary (:sc descriptor-reg) tls)
    (:generator 4
      (let ((global-val (gen-label))
            (done (gen-label)))
        (loadw tls symbol symbol-tls-index-slot other-pointer-lowtag)
        (with-tls-ea (EA :base tls :base-already-live-p t)
          (inst cmp EA no-tls-value-marker-widetag :maybe-fs)
          (inst jmp :z global-val)
          (inst mov EA value :maybe-fs))
        (inst jmp done)
        (emit-label global-val)
        (storew value symbol symbol-value-slot other-pointer-lowtag)
        (emit-label done))))

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
      (let* ((check-unbound-label (gen-label))
             (err-lab (generate-error-code vop 'unbound-symbol-error object))
             (ret-lab (gen-label)))
        (loadw value object symbol-tls-index-slot other-pointer-lowtag)
        (with-tls-ea (EA :base value :base-already-live-p t)
          (inst mov value EA :maybe-fs))
        (inst cmp value no-tls-value-marker-widetag)
        (inst jmp :ne check-unbound-label)
        (loadw value object symbol-value-slot other-pointer-lowtag)
        (emit-label check-unbound-label)
        (inst cmp value unbound-marker-widetag)
        (inst jmp :e err-lab)
        (emit-label ret-lab))))

  (define-vop (fast-symbol-value symbol-value)
    ;; KLUDGE: not really fast, in fact, because we're going to have to
    ;; do a full lookup of the thread-local area anyway.  But half of
    ;; the meaning of FAST-SYMBOL-VALUE is "do not signal an error if
    ;; unbound", which is used in the implementation of COPY-SYMBOL.  --
    ;; CSR, 2003-04-22
    (:policy :fast)
    (:translate symbol-value)
    (:generator 8
      (let ((ret-lab (gen-label)))
        (loadw value object symbol-tls-index-slot other-pointer-lowtag)
        (with-tls-ea (EA :base value :base-already-live-p t)
          (inst mov value EA :maybe-fs))
        (inst cmp value no-tls-value-marker-widetag)
        (inst jmp :ne ret-lab)
        (loadw value object symbol-value-slot other-pointer-lowtag)
        (emit-label ret-lab)))))

#!-sb-thread
(progn
  (define-vop (symbol-value symbol-global-value)
    (:translate symbol-value))
  (define-vop (fast-symbol-value fast-symbol-global-value)
    (:translate symbol-value))
  (define-vop (set %set-symbol-global-value)))

#!+sb-thread
(define-vop (boundp)
  (:translate boundp)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:conditional :ne)
  (:temporary (:sc descriptor-reg #+nil(:from (:argument 0))) value)
  (:generator 9
    (let ((check-unbound-label (gen-label)))
      (loadw value object symbol-tls-index-slot other-pointer-lowtag)
      (with-tls-ea (EA :base value :base-already-live-p t)
        (inst mov value EA :maybe-fs))
      (inst cmp value no-tls-value-marker-widetag)
      (inst jmp :ne check-unbound-label)
      (loadw value object symbol-value-slot other-pointer-lowtag)
      (emit-label check-unbound-label)
      (inst cmp value unbound-marker-widetag))))

#!-sb-thread
(define-vop (boundp)
  (:translate boundp)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:conditional :ne)
  (:generator 9
    (inst cmp (make-ea-for-object-slot object symbol-value-slot
                                       other-pointer-lowtag)
          unbound-marker-widetag)))


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
    (inst and res (lognot #b11))))

;;;; fdefinition (FDEFN) objects

(define-vop (fdefn-fun cell-ref)        ; /pfw - alpha
  (:variant fdefn-fun-slot other-pointer-lowtag))

(define-vop (safe-fdefn-fun)
  (:translate safe-fdefn-fun)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 10
    (loadw value object fdefn-fun-slot other-pointer-lowtag)
    (inst cmp value nil-value)
    (let ((err-lab (generate-error-code vop 'undefined-fun-error object)))
      (inst jmp :e err-lab))))

(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:translate (setf fdefn-fun))
  (:args (function :scs (descriptor-reg) :target result)
         (fdefn :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) raw)
  (:temporary (:sc byte-reg) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (load-type type function (- fun-pointer-lowtag))
    (inst lea raw
          (make-ea-for-object-slot function simple-fun-code-offset
                                   fun-pointer-lowtag))
    (inst cmp type simple-fun-header-widetag)
    (inst jmp :e normal-fn)
    (inst lea raw (make-fixup "closure_tramp" :foreign))
    NORMAL-FN
    (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
    (storew raw fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result function)))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (storew nil-value fdefn fdefn-fun-slot other-pointer-lowtag)
    (storew (make-fixup "undefined_tramp" :foreign)
            fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result fdefn)))

;;;; binding and unbinding

;;; BIND -- Establish VAL as a binding for SYMBOL. Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.
;;
;;; FIXME: Split into DYNBIND and BIND: DYNBIND needs to ensure
;;; TLS-INDEX, whereas BIND should assume it is already in place. Make
;;; LET &co compile into BIND, and PROGV into DYNBIND, plus ensure
;;; TLS-INDEX at compile-time, and make loader and dumper preserve
;;; the existence of a TLS-INDEX.
#!+sb-thread
(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) tls-index bsp
                                 #!+win32 temp)
  (:generator 10
     (load-binding-stack-pointer bsp)
     (loadw tls-index symbol symbol-tls-index-slot other-pointer-lowtag)
     (inst add bsp (* binding-size n-word-bytes))
     (store-binding-stack-pointer bsp)
     (inst test tls-index tls-index)
     (inst jmp :ne tls-index-valid)
     (inst mov tls-index symbol)
     (inst call (make-fixup
                 (ecase (tn-offset tls-index)
                   (#.eax-offset 'alloc-tls-index-in-eax)
                   (#.ebx-offset 'alloc-tls-index-in-ebx)
                   (#.ecx-offset 'alloc-tls-index-in-ecx)
                   (#.edx-offset 'alloc-tls-index-in-edx)
                   (#.edi-offset 'alloc-tls-index-in-edi)
                   (#.esi-offset 'alloc-tls-index-in-esi))
                 :assembly-routine))
     TLS-INDEX-VALID
     ;; with-tls-ea on win32 causes tls-index to be an absolute address
     ;; which is problematic when UNBIND uses with-tls-ea too.
     #!+win32(move temp tls-index)
     (with-tls-ea (EA :base tls-index :base-already-live-p t)
       (inst push EA :maybe-fs)
       (popw bsp (- binding-value-slot binding-size))
       (storew #!-win32 tls-index
               #!+win32 temp
               bsp (- binding-symbol-slot binding-size))
       (inst mov EA val :maybe-fs))))

#!-sb-thread
(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) temp bsp)
  (:generator 5
    (load-symbol-value bsp *binding-stack-pointer*)
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst add bsp (* binding-size n-word-bytes))
    (store-symbol-value bsp *binding-stack-pointer*)
    (storew temp bsp (- binding-value-slot binding-size))
    (storew symbol bsp (- binding-symbol-slot binding-size))
    (storew val symbol symbol-value-slot other-pointer-lowtag)))

#!+sb-thread
(define-vop (unbind)
  (:temporary (:sc unsigned-reg) temp bsp tls-index)
  (:generator 0
    (load-binding-stack-pointer bsp)
    ;; Load SYMBOL from stack, and get the TLS-INDEX.
    (loadw tls-index bsp (- binding-symbol-slot binding-size))
    ;; Load VALUE from stack, then restore it to the TLS area.
    (loadw temp bsp (- binding-value-slot binding-size))
    (with-tls-ea (EA :base tls-index :base-already-live-p t)
      (inst mov EA temp :maybe-fs))
    ;; Zero out the stack.
    (inst sub bsp (* binding-size n-word-bytes))
    (storew 0 bsp binding-symbol-slot)
    (storew 0 bsp binding-value-slot)
    (store-binding-stack-pointer bsp)))

#!-sb-thread
(define-vop (unbind)
  (:temporary (:sc unsigned-reg) symbol value bsp)
  (:generator 0
    (load-symbol-value bsp *binding-stack-pointer*)
    (loadw symbol bsp (- binding-symbol-slot binding-size))
    (loadw value bsp (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    (storew 0 bsp (- binding-symbol-slot binding-size))
    (storew 0 bsp (- binding-value-slot binding-size))
    (inst sub bsp (* binding-size n-word-bytes))
    (store-symbol-value bsp *binding-stack-pointer*)))


(define-vop (unbind-to-here)
  (:args (where :scs (descriptor-reg any-reg)))
  (:temporary (:sc unsigned-reg) symbol value bsp)
  (:generator 0
    (load-binding-stack-pointer bsp)
    (inst cmp where bsp)
    (inst jmp :e done)

    LOOP
    (inst sub bsp (* binding-size n-word-bytes))
    (loadw symbol bsp binding-symbol-slot)
    (inst test symbol symbol)
    (inst jmp :z skip)
    ;; Bind stack debug sentinels have the unbound marker in the symbol slot
    (inst cmp symbol unbound-marker-widetag)
    (inst jmp :eq skip)
    (loadw value bsp binding-value-slot)
    #!-sb-thread (storew value symbol symbol-value-slot other-pointer-lowtag)
    #!+sb-thread (with-tls-ea (EA :base symbol :base-already-live-p t)
                   (inst mov EA value :maybe-fs))
    (storew 0 bsp binding-symbol-slot)

    SKIP
    (storew 0 bsp binding-value-slot)
    (inst cmp where bsp)
    (inst jmp :ne loop)
    (store-binding-stack-pointer bsp)

    DONE))

;;;; closure indexing

(define-full-reffer closure-index-ref *
  closure-info-offset fun-pointer-lowtag
  (any-reg descriptor-reg) * %closure-index-ref)

(define-full-setter set-funcallable-instance-info *
  funcallable-instance-info-offset fun-pointer-lowtag
  (any-reg descriptor-reg) * %set-funcallable-instance-info)

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
    (storew ebp-tn object (+ closure-info-offset offset) fun-pointer-lowtag)))

;;;; value cell hackery

(define-vop (value-cell-ref cell-ref)
  (:variant value-cell-value-slot other-pointer-lowtag))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-lowtag))

;;;; structure hackery

(define-vop (instance-length)
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (loadw res struct 0 instance-pointer-lowtag)
    (inst shr res n-widetag-bits)))

(define-full-reffer instance-index-ref *
  instance-slots-offset instance-pointer-lowtag
  (any-reg descriptor-reg) *
  %instance-ref)

(define-full-setter instance-index-set *
  instance-slots-offset instance-pointer-lowtag
  (any-reg descriptor-reg) *
  %instance-set)

(define-full-compare-and-swap %instance-cas instance
  instance-slots-offset instance-pointer-lowtag
  (any-reg descriptor-reg) *
  %instance-cas)
(define-full-compare-and-swap %raw-instance-cas/word instance
  instance-slots-offset instance-pointer-lowtag
  (unsigned-reg) unsigned-num %raw-instance-cas/word)

;;;; code object frobbing

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (any-reg descriptor-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-lowtag
  (any-reg descriptor-reg) * code-header-set)

;;;; raw instance slot accessors

(defun make-ea-for-raw-slot (object index &optional (displacement 0))
  ;; instance-init vops pass a literal integer, ref/set can use an immediate tn
  (let ((imm-index (cond ((integerp index) index)
                         ((sc-is index immediate) (tn-value index)))))
    (make-ea :dword :base object
             ;; If index is a register, it needs no scaling - it has tag bits.
             :index (unless imm-index index) :scale 1
             :disp (- (ash (+ (or imm-index 0) displacement instance-slots-offset)
                           word-shift) instance-pointer-lowtag))))

(define-vop (raw-instance-ref/word)
  (:translate %raw-instance-ref/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)) (index :scs (any-reg immediate)))
  (:arg-types * tagged-num)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst mov value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-set/word)
  (:translate %raw-instance-set/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (unsigned-reg) :target result))
  (:arg-types * tagged-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst mov (make-ea-for-raw-slot object index) value)
    (move result value)))

(define-vop (raw-instance-init/word)
  (:args (object :scs (descriptor-reg))
         (value :scs (unsigned-reg)))
  (:arg-types * unsigned-num)
  (:info index)
  (:generator 5
    (inst mov (make-ea-for-raw-slot object index) value)))

(define-vop (raw-instance-atomic-incf/word)
  (:translate %raw-instance-atomic-incf/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (diff :scs (unsigned-reg) :target result))
  (:arg-types * tagged-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst xadd (make-ea-for-raw-slot object index) diff :lock)
    (move result diff)))

(define-vop (raw-instance-ref/single)
  (:translate %raw-instance-ref/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)) (index :scs (any-reg immediate)))
  (:arg-types * tagged-num)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (with-empty-tn@fp-top(value)
      (inst fld (make-ea-for-raw-slot object index)))))

(define-vop (raw-instance-set/single)
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (single-reg) :target result))
  (:arg-types * tagged-num single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (unless (zerop (tn-offset value))
      (inst fxch value))
    (inst fst (make-ea-for-raw-slot object index))
    (cond
      ((zerop (tn-offset value))
        (unless (zerop (tn-offset result))
          (inst fst result)))
      ((zerop (tn-offset result))
        (inst fst value))
      (t
        (unless (location= value result)
          (inst fst result))
        (inst fxch value)))))

(define-vop (raw-instance-init/single)
  (:args (object :scs (descriptor-reg))
         (value :scs (single-reg)))
  (:arg-types * single-float)
  (:info index)
  (:generator 5
    (with-tn@fp-top (value)
      (inst fst (make-ea-for-raw-slot object index)))))

(define-vop (raw-instance-ref/double)
  (:translate %raw-instance-ref/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)) (index :scs (any-reg immediate)))
  (:arg-types * tagged-num)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
    (with-empty-tn@fp-top(value)
      (inst fldd (make-ea-for-raw-slot object index)))))

(define-vop (raw-instance-set/double)
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (double-reg) :target result))
  (:arg-types * tagged-num double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
    (unless (zerop (tn-offset value))
      (inst fxch value))
    (inst fstd (make-ea-for-raw-slot object index))
    (cond
      ((zerop (tn-offset value))
        (unless (zerop (tn-offset result))
          (inst fstd result)))
      ((zerop (tn-offset result))
        (inst fstd value))
      (t
        (unless (location= value result)
          (inst fstd result))
        (inst fxch value)))))

(define-vop (raw-instance-init/double)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-reg)))
  (:arg-types * double-float)
  (:info index)
  (:generator 5
    (with-tn@fp-top (value)
      (inst fstd (make-ea-for-raw-slot object index)))))

(define-vop (raw-instance-ref/complex-single)
  (:translate %raw-instance-ref/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (let ((real-tn (complex-single-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
        (inst fld (make-ea-for-raw-slot object index))))
    (let ((imag-tn (complex-single-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
        (inst fld (make-ea-for-raw-slot object index 1))))))

(define-vop (raw-instance-set/complex-single)
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (complex-single-reg) :target result))
  (:arg-types * positive-fixnum complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value))
          (result-real (complex-single-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
             ;; Value is in ST0.
             (inst fst (make-ea-for-raw-slot object index))
             (unless (zerop (tn-offset result-real))
               ;; Value is in ST0 but not result.
               (inst fst result-real)))
            (t
             ;; Value is not in ST0.
             (inst fxch value-real)
             (inst fst (make-ea-for-raw-slot object index))
             (cond ((zerop (tn-offset result-real))
                    ;; The result is in ST0.
                    (inst fst value-real))
                   (t
                    ;; Neither value or result are in ST0
                    (unless (location= value-real result-real)
                      (inst fst result-real))
                    (inst fxch value-real))))))
    (let ((value-imag (complex-single-reg-imag-tn value))
          (result-imag (complex-single-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fst (make-ea-for-raw-slot object index 1))
      (unless (location= value-imag result-imag)
        (inst fst result-imag))
      (inst fxch value-imag))))

(define-vop (raw-instance-init/complex-single)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-single-reg)))
  (:arg-types * complex-single-float)
  (:info index)
  (:generator 5
    (let ((value-real (complex-single-reg-real-tn value)))
      (with-tn@fp-top (value-real)
        (inst fst (make-ea-for-raw-slot object index))))
    (let ((value-imag (complex-single-reg-imag-tn value)))
      (with-tn@fp-top (value-imag)
        (inst fst (make-ea-for-raw-slot object index 1))))))

(define-vop (raw-instance-ref/complex-double)
  (:translate %raw-instance-ref/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 7
    (let ((real-tn (complex-double-reg-real-tn value)))
      (with-empty-tn@fp-top (real-tn)
        (inst fldd (make-ea-for-raw-slot object index))))
    (let ((imag-tn (complex-double-reg-imag-tn value)))
      (with-empty-tn@fp-top (imag-tn)
        (inst fldd (make-ea-for-raw-slot object index 2))))))

(define-vop (raw-instance-set/complex-double)
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (value :scs (complex-double-reg) :target result))
  (:arg-types * positive-fixnum complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 20
    (let ((value-real (complex-double-reg-real-tn value))
          (result-real (complex-double-reg-real-tn result)))
      (cond ((zerop (tn-offset value-real))
             ;; Value is in ST0.
             (inst fstd (make-ea-for-raw-slot object index))
             (unless (zerop (tn-offset result-real))
               ;; Value is in ST0 but not result.
               (inst fstd result-real)))
            (t
             ;; Value is not in ST0.
             (inst fxch value-real)
             (inst fstd (make-ea-for-raw-slot object index))
             (cond ((zerop (tn-offset result-real))
                    ;; The result is in ST0.
                    (inst fstd value-real))
                   (t
                    ;; Neither value or result are in ST0
                    (unless (location= value-real result-real)
                      (inst fstd result-real))
                    (inst fxch value-real))))))
    (let ((value-imag (complex-double-reg-imag-tn value))
          (result-imag (complex-double-reg-imag-tn result)))
      (inst fxch value-imag)
      (inst fstd (make-ea-for-raw-slot object index 2))
      (unless (location= value-imag result-imag)
        (inst fstd result-imag))
      (inst fxch value-imag))))

(define-vop (raw-instance-init/complex-double)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-double-reg)))
  (:arg-types * complex-double-float)
  (:info index)
  (:generator 20
    (let ((value-real (complex-double-reg-real-tn value)))
      (with-tn@fp-top (value-real)
        (inst fstd (make-ea-for-raw-slot object index))))
    (let ((value-imag (complex-double-reg-imag-tn value)))
      (with-tn@fp-top (value-imag)
        (inst fstd (make-ea-for-raw-slot object index 2))))))
