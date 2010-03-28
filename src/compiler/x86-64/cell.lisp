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
  (:temporary (:sc descriptor-reg) temp)
  (:info name offset lowtag)
  (:ignore name)
  (:results)
  (:generator 1
    (if (sc-is value immediate)
        (let ((val (tn-value value)))
          (move-immediate (make-ea :qword
                                   :base object
                                   :disp (- (* offset n-word-bytes)
                                            lowtag))
                          (etypecase val
                            (integer
                             (fixnumize val))
                            (symbol
                             (+ nil-value (static-symbol-offset val)))
                            (character
                             (logior (ash (char-code val) n-widetag-bits)
                                     character-widetag)))
                          temp))
        ;; Else, value not immediate.
        (storew value object offset lowtag))))

(define-vop (compare-and-swap-slot)
  (:args (object :scs (descriptor-reg) :to :eval)
         (old :scs (descriptor-reg any-reg) :target rax)
         (new :scs (descriptor-reg any-reg)))
  (:temporary (:sc descriptor-reg :offset rax-offset
                   :from (:argument 1) :to :result :target result)
              rax)
  (:info name offset lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 5
     (move rax old)
     (inst cmpxchg (make-ea :qword :base object
                            :disp (- (* offset n-word-bytes) lowtag))
           new :lock)
     (move result rax)))

;;;; symbol hacking VOPs

(define-vop (%compare-and-swap-symbol-value)
  (:translate %compare-and-swap-symbol-value)
  (:args (symbol :scs (descriptor-reg) :to (:result 1))
         (old :scs (descriptor-reg any-reg) :target rax)
         (new :scs (descriptor-reg any-reg)))
  (:temporary (:sc descriptor-reg :offset rax-offset) rax)
  #!+sb-thread
  (:temporary (:sc descriptor-reg) tls)
  (:results (result :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 15
    ;; This code has two pathological cases: NO-TLS-VALUE-MARKER
    ;; or UNBOUND-MARKER as NEW: in either case we would end up
    ;; doing possible damage with CMPXCHG -- so don't do that!
    (let ((unbound (generate-error-code vop 'unbound-symbol-error symbol))
          (check (gen-label)))
      (move rax old)
      #!+sb-thread
      (progn
        (loadw tls symbol symbol-tls-index-slot other-pointer-lowtag)
        ;; Thread-local area, no LOCK needed.
        (inst cmpxchg (make-ea :qword :base thread-base-tn
                               :index tls :scale 1)
              new)
        (inst cmp rax no-tls-value-marker-widetag)
        (inst jmp :ne check)
        (move rax old))
      (inst cmpxchg (make-ea :qword :base symbol
                             :disp (- (* symbol-value-slot n-word-bytes)
                                      other-pointer-lowtag)
                             :scale 1)
            new :lock)
      (emit-label check)
      (move result rax)
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
        (inst cmp (make-ea :qword :base thread-base-tn :scale 1 :index tls)
              no-tls-value-marker-widetag)
        (inst jmp :z global-val)
        (inst mov (make-ea :qword :base thread-base-tn :scale 1 :index tls)
              value)
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
        (inst mov value (make-ea :qword :base thread-base-tn
                                 :index value :scale 1))
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
        (inst mov value
              (make-ea :qword :base thread-base-tn :index value :scale 1))
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
      (inst mov value
            (make-ea :qword :base thread-base-tn :index value :scale 1))
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
    ;; cdr slot, so we have to strip off the three low bits to make sure
    ;; it is a fixnum.  The lowtag selection magic that is required to
    ;; ensure this is explained in the comment in objdef.lisp
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst and res (lognot #b111))))

;;;; fdefinition (FDEFN) objects

(define-vop (fdefn-fun cell-ref)        ; /pfw - alpha
  (:variant fdefn-fun-slot other-pointer-lowtag))

(define-vop (safe-fdefn-fun)
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
          (make-ea :byte :base function
                   :disp (- (* simple-fun-code-offset n-word-bytes)
                            fun-pointer-lowtag)))
    (inst cmp type simple-fun-header-widetag)
    (inst jmp :e NORMAL-FUN)
    (inst lea raw (make-fixup "closure_tramp" :foreign))
    NORMAL-FUN
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

#!+sb-thread
(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) tls-index bsp)
  (:generator 10
    (let ((tls-index-valid (gen-label)))
      (load-binding-stack-pointer bsp)
      (loadw tls-index symbol symbol-tls-index-slot other-pointer-lowtag)
      (inst add bsp (* binding-size n-word-bytes))
      (store-binding-stack-pointer bsp)
      (inst or tls-index tls-index)
      (inst jmp :ne tls-index-valid)
      (inst mov tls-index symbol)
      (inst lea temp-reg-tn
            (make-ea :qword :disp
                     (make-fixup (ecase (tn-offset tls-index)
                                   (#.rax-offset 'alloc-tls-index-in-rax)
                                   (#.rcx-offset 'alloc-tls-index-in-rcx)
                                   (#.rdx-offset 'alloc-tls-index-in-rdx)
                                   (#.rbx-offset 'alloc-tls-index-in-rbx)
                                   (#.rsi-offset 'alloc-tls-index-in-rsi)
                                   (#.rdi-offset 'alloc-tls-index-in-rdi)
                                   (#.r8-offset  'alloc-tls-index-in-r8)
                                   (#.r9-offset  'alloc-tls-index-in-r9)
                                   (#.r10-offset 'alloc-tls-index-in-r10)
                                   (#.r12-offset 'alloc-tls-index-in-r12)
                                   (#.r13-offset 'alloc-tls-index-in-r13)
                                   (#.r14-offset 'alloc-tls-index-in-r14)
                                   (#.r15-offset 'alloc-tls-index-in-r15))
                                 :assembly-routine)))
      (inst call temp-reg-tn)
      (emit-label tls-index-valid)
      (inst push (make-ea :qword :base thread-base-tn :scale 1 :index tls-index))
      (popw bsp (- binding-value-slot binding-size))
      (storew symbol bsp (- binding-symbol-slot binding-size))
      (inst mov (make-ea :qword :base thread-base-tn :scale 1 :index tls-index)
            val))))

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
    ;; Load SYMBOL from stack, and get the TLS-INDEX
    (loadw temp bsp (- binding-symbol-slot binding-size))
    (loadw tls-index temp symbol-tls-index-slot other-pointer-lowtag)
    ;; Load VALUE from stack, the restore it to the TLS area.
    (loadw temp bsp (- binding-value-slot binding-size))
    (inst mov (make-ea :qword :base thread-base-tn :scale 1 :index tls-index)
          temp)
    ;; Zero out the stack.
    (storew 0 bsp (- binding-symbol-slot binding-size))
    (storew 0 bsp (- binding-value-slot binding-size))
    (inst sub bsp (* binding-size n-word-bytes))
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
  (:temporary (:sc unsigned-reg) symbol value bsp #!+sb-thread tls-index)
  (:generator 0
    (load-binding-stack-pointer bsp)
    (inst cmp where bsp)
    (inst jmp :e DONE)

    LOOP
    (loadw symbol bsp (- binding-symbol-slot binding-size))
    (inst or symbol symbol)
    (inst jmp :z SKIP)
    ;; Bind stack debug sentinels have the unbound marker in the symbol slot
    (inst cmp symbol unbound-marker-widetag)
    (inst jmp :eq SKIP)
    (loadw value bsp (- binding-value-slot binding-size))
    #!-sb-thread
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    #!+sb-thread
    (loadw tls-index symbol symbol-tls-index-slot other-pointer-lowtag)
    #!+sb-thread
    (inst mov (make-ea :qword :base thread-base-tn :scale 1 :index tls-index)
          value)
    (storew 0 bsp (- binding-symbol-slot binding-size))

    SKIP
    (storew 0 bsp (- binding-value-slot binding-size))
    (inst sub bsp (* binding-size n-word-bytes))
    (inst cmp where bsp)
    (inst jmp :ne LOOP)
    (store-binding-stack-pointer bsp)

    DONE))

(define-vop (bind-sentinel)
  (:temporary (:sc unsigned-reg) bsp)
  (:generator 1
     (load-binding-stack-pointer bsp)
     (inst add bsp (* binding-size n-word-bytes))
     (storew unbound-marker-widetag bsp (- binding-symbol-slot binding-size))
     (storew rbp-tn bsp (- binding-value-slot binding-size))
     (store-binding-stack-pointer bsp)))

(define-vop (unbind-sentinel)
  (:temporary (:sc unsigned-reg) bsp)
  (:generator 1
     (load-binding-stack-pointer bsp)
     (storew 0 bsp (- binding-value-slot binding-size))
     (storew 0 bsp (- binding-symbol-slot binding-size))
     (inst sub bsp (* binding-size n-word-bytes))
     (store-binding-stack-pointer bsp)))




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

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (any-reg descriptor-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (any-reg descriptor-reg) * %instance-set)

(define-full-compare-and-swap %compare-and-swap-instance-ref instance
  instance-slots-offset instance-pointer-lowtag
  (any-reg descriptor-reg) *
  %compare-and-swap-instance-ref)

;;;; code object frobbing

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (any-reg descriptor-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-lowtag
  (any-reg descriptor-reg) * code-header-set)

;;;; raw instance slot accessors

(defun make-ea-for-raw-slot (object index instance-length
                             &optional (adjustment 0))
  (if (integerp instance-length)
      ;; For RAW-INSTANCE-INIT/* VOPs, which know the exact instance length
      ;; at compile time.
      (make-ea :qword
               :base object
               :disp (+ (* (- instance-length instance-slots-offset index)
                           n-word-bytes)
                        (- instance-pointer-lowtag)
                        adjustment))
      (etypecase index
        (tn
         (make-ea :qword :base object :index instance-length
                  :disp (+ (* (1- instance-slots-offset) n-word-bytes)
                           (- instance-pointer-lowtag)
                           adjustment)))
        (integer
         (make-ea :qword :base object :index instance-length
                  :scale 8
                  :disp (+ (* (1- instance-slots-offset) n-word-bytes)
                           (- instance-pointer-lowtag)
                           adjustment
                           (* index (- n-word-bytes))))))))

(define-vop (raw-instance-ref/word)
  (:translate %raw-instance-ref/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)) (index :scs (any-reg)))
  (:arg-types * tagged-num)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp n-fixnum-tag-bits)
    (inst sub tmp index)
    (inst mov value (make-ea-for-raw-slot object index tmp))))

(define-vop (raw-instance-ref-c/word)
  (:translate %raw-instance-ref/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.sb!vm:n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst mov value (make-ea-for-raw-slot object index tmp))))

(define-vop (raw-instance-set/word)
  (:translate %raw-instance-set/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (unsigned-reg) :target result))
  (:arg-types * tagged-num unsigned-num)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp n-fixnum-tag-bits)
    (inst sub tmp index)
    (inst mov (make-ea-for-raw-slot object index tmp) value)
    (move result value)))

(define-vop (raw-instance-set-c/word)
  (:translate %raw-instance-set/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (unsigned-reg) :target result))
  (:arg-types * (:constant (load/store-index #.sb!vm:n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset))
              unsigned-num)
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst mov (make-ea-for-raw-slot object index tmp) value)
    (move result value)))

(define-vop (raw-instance-init/word)
  (:args (object :scs (descriptor-reg))
         (value :scs (unsigned-reg)))
  (:arg-types * unsigned-num)
  (:info instance-length index)
  (:generator 4
    (inst mov (make-ea-for-raw-slot object index instance-length) value)))

(define-vop (raw-instance-atomic-incf-c/word)
  (:translate %raw-instance-atomic-incf/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (diff :scs (unsigned-reg) :target result))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset))
              unsigned-num)
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst xadd (make-ea-for-raw-slot object index tmp) diff :lock)
    (move result diff)))

(define-vop (raw-instance-ref/single)
  (:translate %raw-instance-ref/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp n-fixnum-tag-bits)
    (inst sub tmp index)
    (inst movss value (make-ea-for-raw-slot object index tmp))))

(define-vop (raw-instance-ref-c/single)
  (:translate %raw-instance-ref/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.sb!vm:n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst movss value (make-ea-for-raw-slot object index tmp))))

(define-vop (raw-instance-set/single)
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg) :target result))
  (:arg-types * positive-fixnum single-float)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp n-fixnum-tag-bits)
    (inst sub tmp index)
    (inst movss (make-ea-for-raw-slot object index tmp) value)
    (move result value)))

(define-vop (raw-instance-set-c/single)
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (single-reg) :target result))
  (:arg-types * (:constant (load/store-index #.sb!vm:n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset))
              single-float)
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst movss (make-ea-for-raw-slot object index tmp) value)
    (move result value)))

(define-vop (raw-instance-init/single)
  (:args (object :scs (descriptor-reg))
         (value :scs (single-reg)))
  (:arg-types * single-float)
  (:info instance-length index)
  (:generator 4
    (inst movss (make-ea-for-raw-slot object index instance-length) value)))

(define-vop (raw-instance-ref/double)
  (:translate %raw-instance-ref/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp n-fixnum-tag-bits)
    (inst sub tmp index)
    (inst movsd value (make-ea-for-raw-slot object index tmp))))

(define-vop (raw-instance-ref-c/double)
  (:translate %raw-instance-ref/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.sb!vm:n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst movsd value (make-ea-for-raw-slot object index tmp))))

(define-vop (raw-instance-set/double)
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg) :target result))
  (:arg-types * positive-fixnum double-float)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp n-fixnum-tag-bits)
    (inst sub tmp index)
    (inst movsd (make-ea-for-raw-slot object index tmp) value)
    (move result value)))

(define-vop (raw-instance-set-c/double)
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-reg) :target result))
  (:arg-types * (:constant (load/store-index #.sb!vm:n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset))
              double-float)
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst movsd (make-ea-for-raw-slot object index tmp) value)
    (move result value)))

(define-vop (raw-instance-init/double)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-reg)))
  (:arg-types * double-float)
  (:info instance-length index)
  (:generator 4
    (inst movsd (make-ea-for-raw-slot object index instance-length) value)))

(define-vop (raw-instance-ref/complex-single)
  (:translate %raw-instance-ref/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp n-fixnum-tag-bits)
    (inst sub tmp index)
    (inst movq value (make-ea-for-raw-slot object index tmp))))

(define-vop (raw-instance-ref-c/complex-single)
  (:translate %raw-instance-ref/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.sb!vm:n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst movq value (make-ea-for-raw-slot object index tmp))))

(define-vop (raw-instance-set/complex-single)
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg) :target result))
  (:arg-types * positive-fixnum complex-single-float)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp n-fixnum-tag-bits)
    (inst sub tmp index)
    (move result value)
    (inst movq (make-ea-for-raw-slot object index tmp) value)))

(define-vop (raw-instance-set-c/complex-single)
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-single-reg) :target result))
  (:arg-types * (:constant (load/store-index #.sb!vm:n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset))
              complex-single-float)
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (move result value)
    (inst movq (make-ea-for-raw-slot object index tmp) value)))

(define-vop (raw-instance-init/complex-single)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-single-reg)))
  (:arg-types * complex-single-float)
  (:info instance-length index)
  (:generator 4
    (inst movq (make-ea-for-raw-slot object index instance-length) value)))

(define-vop (raw-instance-ref/complex-double)
  (:translate %raw-instance-ref/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp n-fixnum-tag-bits)
    (inst sub tmp index)
    (inst movdqu value (make-ea-for-raw-slot object index tmp -8))))

(define-vop (raw-instance-ref-c/complex-double)
  (:translate %raw-instance-ref/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.sb!vm:n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst movdqu value (make-ea-for-raw-slot object index tmp -8))))

(define-vop (raw-instance-set/complex-double)
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg) :target result))
  (:arg-types * positive-fixnum complex-double-float)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 5
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst shl tmp n-fixnum-tag-bits)
    (inst sub tmp index)
    (move result value)
    (inst movdqu (make-ea-for-raw-slot object index tmp -8) value)))

(define-vop (raw-instance-set-c/complex-double)
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-double-reg) :target result))
  (:arg-types * (:constant (load/store-index #.sb!vm:n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset))
              complex-double-float)
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (move result value)
    (inst movdqu (make-ea-for-raw-slot object index tmp -8) value)))

(define-vop (raw-instance-init/complex-double)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-double-reg)))
  (:arg-types * complex-double-float)
  (:info instance-length index)
  (:generator 4
    (inst movdqu (make-ea-for-raw-slot object index instance-length -8) value)))
