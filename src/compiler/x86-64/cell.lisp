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

(define-vop (init-slot set-slot))

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

(macrolet (;; Logic common to thread-aware SET and CAS. CELL is assigned
           ;; to the location that should be accessed to modify SYMBOL's
           ;; value either in the TLS or the symbol's value slot as follows:
           ;; (1) make it look as if the TLS cell were a symbol by biasing
           ;;     upward by other-pointer-lowtag less 1 word.
           ;; (2) conditionally make CELL point to the symbol itself
           (compute-virtual-symbol ()
             `(progn
                (inst mov (reg-in-size cell :dword) (tls-index-of symbol))
                (inst lea cell
                      (make-ea :qword :base thread-base-tn :index cell
                               :disp (- other-pointer-lowtag
                                        (ash symbol-value-slot word-shift))))
                (inst cmp (access-value-slot cell :dword) ; TLS reference
                      no-tls-value-marker-widetag)
                (inst cmov :e cell symbol))) ; now possibly get the symbol
           (tls-index-of (sym)
             `(make-ea-for-object-slot-half ,sym symbol-tls-index-slot
                                            other-pointer-lowtag))
           (access-tls-val (index size)
             `(make-ea ,size :base thread-base-tn :index ,index :scale 1))
           (access-value-slot (sym &optional (size :qword))
             (ecase size
               (:dword `(make-ea-for-object-slot-half
                         ,sym symbol-value-slot other-pointer-lowtag))
               (:qword `(make-ea-for-object-slot
                         ,sym symbol-value-slot other-pointer-lowtag)))))

;; FIXME: this incorrectly indentated code results from minimizing a diff.
;; Attempting to reorder, reimplement, and reindent the #![-+]sb-thread cases
;; left the code so unintelligible that the author could not parse it.
(define-vop (%compare-and-swap-symbol-value)
  (:translate %compare-and-swap-symbol-value)
  (:args (symbol :scs (descriptor-reg) :to (:result 0))
         (old :scs (descriptor-reg any-reg) :target rax)
         (new :scs (descriptor-reg any-reg)))
  (:temporary (:sc descriptor-reg :offset rax-offset
               :from (:argument 1) :to (:result 0)) rax)
  #!+sb-thread
  (:temporary (:sc descriptor-reg :to (:result 0)) cell)
  (:results (result :scs (descriptor-reg any-reg)))
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 15
    ;; This code has two pathological cases: NO-TLS-VALUE-MARKER
    ;; or UNBOUND-MARKER as NEW: in either case we would end up
    ;; doing possible damage with CMPXCHG -- so don't do that!
    ;; Even worse: don't supply old=NO-TLS-VALUE with a symbol whose
    ;; tls-index=0, because that would succeed, assigning NEW to each
    ;; symbol in existence having otherwise no thread-local value.
    (let ((unbound (generate-error-code vop 'unbound-symbol-error symbol)))
      #!+sb-thread (progn (compute-virtual-symbol)
                          (move rax old)
                          (inst cmpxchg (access-value-slot cell) new :lock))
      #!-sb-thread (progn (move rax old)
                          ;; is the :LOCK is necessary?
                          (inst cmpxchg (access-value-slot symbol) new :lock))
      (inst cmp (reg-in-size rax :dword) unbound-marker-widetag)
      (inst jmp :e unbound)
      (move result rax))))

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
      (inst cmp (reg-in-size value :dword) unbound-marker-widetag)
      (inst jmp :e err-lab))))

#!+sb-thread
(progn
  (define-vop (set)
    (:args (symbol :scs (descriptor-reg))
           (value :scs (descriptor-reg any-reg)))
    (:temporary (:sc descriptor-reg) cell)
    (:generator 4
      ;; Compute the address into which to store. CMOV can only move into
      ;; a register, so we can't conditionally move into the TLS and
      ;; conditionally move in the opposite flag sense to the symbol.
      (compute-virtual-symbol)
      (inst mov (access-value-slot cell) value)))

  ;; With Symbol-Value, we check that the value isn't the trap object. So
  ;; Symbol-Value of NIL is NIL.
  (define-vop (symbol-value)
    (:translate symbol-value)
    (:policy :fast-safe)
    (:args (symbol :scs (descriptor-reg)))
    (:temporary (:sc unsigned-reg) temp)
    (:results (value :scs (descriptor-reg any-reg)))
    (:vop-var vop)
    (:save-p :compute-only)
    (:generator 9
      (let ((err-lab (generate-error-code vop 'unbound-symbol-error symbol)))
        ;; The order of the first two instructions matters- SYMBOL and VALUE
        ;; might arbitrarily be packed in the same register, or not.
        (inst mov (reg-in-size temp :dword) (tls-index-of symbol))
        (inst mov value (access-value-slot symbol))
        (inst cmp (access-tls-val temp :dword) no-tls-value-marker-widetag)
        (inst cmov :ne value (access-tls-val temp :qword))
        (inst cmp (reg-in-size value :dword) unbound-marker-widetag)
        (inst jmp :e err-lab))))

  (define-vop (fast-symbol-value symbol-value)
    ;; KLUDGE: not really fast, in fact, because we're going to have to
    ;; do a full lookup of the thread-local area anyway.  But half of
    ;; the meaning of FAST-SYMBOL-VALUE is "do not signal an error if
    ;; unbound", which is used in the implementation of COPY-SYMBOL.  --
    ;; CSR, 2003-04-22
    (:policy :fast)
    (:args (symbol :scs (descriptor-reg) :target temp))
    (:temporary (:sc unsigned-reg :from (:argument 0)) temp)
    (:translate symbol-value)
    (:generator 8
      (inst mov value (access-value-slot symbol)) ; prefetch
      ;; We're going to lose the symbol, because it should be in the same
      ;; register as TEMP. That's ok, we don't need it for error signaling.
      (inst mov (reg-in-size temp :dword) (tls-index-of symbol))
      (inst cmp (access-tls-val temp :dword) no-tls-value-marker-widetag)
      (inst cmov :ne value (access-tls-val temp :qword)))))

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
  (:temporary (:sc dword-reg) temp)
  (:generator 9
    (inst mov temp (tls-index-of object))
    (inst mov temp (access-tls-val temp :dword))
    (inst cmp temp no-tls-value-marker-widetag)
    (inst cmov :e temp (access-value-slot object :dword))
    (inst cmp temp unbound-marker-widetag)))

#!-sb-thread
(define-vop (boundp)
  (:translate boundp)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:conditional :ne)
  (:generator 9
    (inst cmp (access-value-slot object :dword) unbound-marker-widetag)))

) ; END OF MACROLET

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
    (inst and res (lognot fixnum-tag-mask))))

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
  (:temporary (:sc unsigned-reg) type)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (load-type type function (- fun-pointer-lowtag))
    (inst lea raw
          (make-ea :byte :base function
                   :disp (- (* simple-fun-code-offset n-word-bytes)
                            fun-pointer-lowtag)))
    (inst cmp (reg-in-size type :byte) simple-fun-header-widetag)
    (inst jmp :e NORMAL-FUN)
    (inst mov raw (make-fixup "closure_tramp" :foreign))
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
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.

#!+sb-thread
(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg) :target tmp
                 :to :load))
  (:temporary (:sc unsigned-reg) tls-index bsp tmp)
  (:generator 10
    (load-binding-stack-pointer bsp)
    (loadw tls-index symbol symbol-tls-index-slot other-pointer-lowtag)
    (inst add bsp (* binding-size n-word-bytes))
    (store-binding-stack-pointer bsp)
    (inst test tls-index tls-index)
    (inst jmp :ne TLS-INDEX-VALID)
    (inst mov tls-index symbol)
    (inst mov tmp (subprimitive-tls-allocator tls-index))
    (inst call tmp)
    TLS-INDEX-VALID
    (inst mov tmp (make-ea :qword :base thread-base-tn :index tls-index))
    (storew tmp bsp (- binding-value-slot binding-size))
    (storew tls-index bsp (- binding-symbol-slot binding-size))
    (inst mov (make-ea :qword :base thread-base-tn :index tls-index) val)))

#!+sb-thread
;; Nikodemus hypothetically terms the above VOP DYNBIND (in x86/cell.lisp)
;; with this "new" one being BIND, but to re-purpose concepts in that way
;; - though it be rational - is fraught with peril.
;; So BIND/LET is for (LET ((*a-special* ...)))
;;
(define-vop (bind/let)
  (:args (val :scs (any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg) bsp tmp)
  (:info symbol)
  (:generator 10
    (inst mov bsp (* binding-size n-word-bytes))
    (inst xadd
          (make-ea :qword :base thread-base-tn
                   :disp (ash thread-binding-stack-pointer-slot word-shift))
          bsp)
    (let* ((tls-index (or (symbol-known-tls-index symbol)
                          (make-fixup symbol :symbol-tls-index)))
           (tls-cell (make-ea :qword :base thread-base-tn :disp tls-index)))
      ;; Too bad we can't use "XCHG [r12+disp], val" to write the new value
      ;; and read the old value in one step. It will violate the constraints
      ;; prescribed in the internal documentation on special binding.
      (inst mov tmp tls-cell)
      (storew tmp bsp binding-value-slot)
      ;; Indices are small enough to be written as :DWORDs which avoids
      ;; a REX prefix if 'bsp' happens to be any of the low 8 registers.
      (inst mov (make-ea :dword :base bsp
                         :disp (ash binding-symbol-slot word-shift)) tls-index)
      (inst mov tls-cell val))
    ;; Emission of this VOP informs the compiler that later SYMBOL-VALUE calls
    ;; might want to use a load-time fixup instead of reading from the symbol's
    ;; tls-index, admitting a possible optimization (NOT DONE):
    ;;  MOV RES,[R12+N] ; CMP RES,NO_TLS_VALUE ; CMOV :E RES,GLOBAL-VALUE
    ;; In contrast, if the symbol is not known to ever have been thread-locally
    ;; bound, reading it should not force the loader to assign a TLS index.
    (setf (info :variable :wired-tls-index symbol) t)))

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
    (inst sub bsp (* binding-size n-word-bytes))
    ;; Load TLS-INDEX of the SYMBOL from stack
    (loadw tls-index bsp binding-symbol-slot)
    ;; Load VALUE from stack, then restore it to the TLS area.
    (loadw temp bsp binding-value-slot)
    (inst mov (make-ea :qword :base thread-base-tn :index tls-index)
          temp)
    ;; Zero out the stack.
    (zeroize temp)

    (storew temp bsp binding-symbol-slot)
    (storew temp bsp binding-value-slot)
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
  (:temporary (:sc unsigned-reg) symbol value bsp zero)
  (:generator 0
    (load-binding-stack-pointer bsp)
    (inst cmp where bsp)
    (inst jmp :e DONE)
    (zeroize zero)
    LOOP
    (inst sub bsp (* binding-size n-word-bytes))
    ;; on sb-thread symbol is actually a tls-index
    (loadw symbol bsp binding-symbol-slot)
    (inst test symbol symbol)
    (inst jmp :z SKIP)
    ;; Bind stack debug sentinels have the unbound marker in the symbol slot
    (inst cmp symbol unbound-marker-widetag)
    (inst jmp :eq SKIP)
    (loadw value bsp binding-value-slot)
    #!-sb-thread
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    #!+sb-thread
    (inst mov (make-ea :qword :base thread-base-tn :index symbol)
          value)
    (storew zero bsp binding-symbol-slot)

    SKIP
    (storew zero bsp binding-value-slot)

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

(define-vop (closure-init-from-fp)
  (:args (object :scs (descriptor-reg)))
  (:info offset)
  (:generator 4
    (storew rbp-tn object (+ closure-info-offset offset) fun-pointer-lowtag)))

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

(defun make-ea-for-raw-slot (object instance-length
                             &key (index nil) (adjustment 0) (scale 1))
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
        (null
         (make-ea :qword :base object :index instance-length :scale scale
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
    (inst mov value (make-ea-for-raw-slot object tmp :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (raw-instance-ref-c/word)
  (:translate %raw-instance-ref/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst mov value (make-ea-for-raw-slot object tmp :index index))))

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
    (inst mov (make-ea-for-raw-slot object tmp :scale (ash 1 (- word-shift n-fixnum-tag-bits))) value)
    (move result value)))

(define-vop (raw-instance-set-c/word)
  (:translate %raw-instance-set/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (unsigned-reg) :target result))
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
    (inst mov (make-ea-for-raw-slot object tmp :index index) value)
    (move result value)))

(define-vop (raw-instance-init/word)
  (:args (object :scs (descriptor-reg))
         (value :scs (unsigned-reg)))
  (:arg-types * unsigned-num)
  (:info instance-length index)
  (:generator 4
    (inst mov (make-ea-for-raw-slot object instance-length :index index) value)))

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
    (inst xadd (make-ea-for-raw-slot object tmp :index index) diff :lock)
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
    (inst movss value (make-ea-for-raw-slot object tmp :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (raw-instance-ref-c/single)
  (:translate %raw-instance-ref/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst movss value (make-ea-for-raw-slot object tmp :index index))))

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
    (inst movss (make-ea-for-raw-slot object tmp :scale (ash 1 (- word-shift n-fixnum-tag-bits))) value)
    (move result value)))

(define-vop (raw-instance-set-c/single)
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (single-reg) :target result))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
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
    (inst movss (make-ea-for-raw-slot object tmp :index index) value)
    (move result value)))

(define-vop (raw-instance-init/single)
  (:args (object :scs (descriptor-reg))
         (value :scs (single-reg)))
  (:arg-types * single-float)
  (:info instance-length index)
  (:generator 4
    (inst movss (make-ea-for-raw-slot object instance-length :index index) value)))

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
    (inst movsd value (make-ea-for-raw-slot object tmp :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (raw-instance-ref-c/double)
  (:translate %raw-instance-ref/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst movsd value (make-ea-for-raw-slot object tmp :index index))))

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
    (inst movsd (make-ea-for-raw-slot object tmp :scale (ash 1 (- word-shift n-fixnum-tag-bits))) value)
    (move result value)))

(define-vop (raw-instance-set-c/double)
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-reg) :target result))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
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
    (inst movsd (make-ea-for-raw-slot object tmp :index index) value)
    (move result value)))

(define-vop (raw-instance-init/double)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-reg)))
  (:arg-types * double-float)
  (:info instance-length index)
  (:generator 4
    (inst movsd (make-ea-for-raw-slot object instance-length :index index) value)))

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
    (inst movq value (make-ea-for-raw-slot object tmp :scale (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop (raw-instance-ref-c/complex-single)
  (:translate %raw-instance-ref/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst movq value (make-ea-for-raw-slot object tmp :index index))))

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
    (inst movq (make-ea-for-raw-slot object tmp :scale (ash 1 (- word-shift n-fixnum-tag-bits))) value)))

(define-vop (raw-instance-set-c/complex-single)
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-single-reg) :target result))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
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
    (inst movq (make-ea-for-raw-slot object tmp :index index) value)))

(define-vop (raw-instance-init/complex-single)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-single-reg)))
  (:arg-types * complex-single-float)
  (:info instance-length index)
  (:generator 4
    (inst movq (make-ea-for-raw-slot object instance-length :index index) value)))

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
    (inst movdqu value (make-ea-for-raw-slot object tmp :scale (ash 1 (- word-shift n-fixnum-tag-bits)) :adjustment -8))))

(define-vop (raw-instance-ref-c/complex-double)
  (:translate %raw-instance-ref/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:temporary (:sc unsigned-reg) tmp)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 4
    (loadw tmp object 0 instance-pointer-lowtag)
    (inst shr tmp n-widetag-bits)
    (inst movdqu value (make-ea-for-raw-slot object tmp :index index :adjustment -8))))

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
    (inst movdqu (make-ea-for-raw-slot object tmp :scale (ash 1 (- word-shift n-fixnum-tag-bits)) :adjustment -8) value)))

(define-vop (raw-instance-set-c/complex-double)
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-double-reg) :target result))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
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
    (inst movdqu (make-ea-for-raw-slot object tmp :index index :adjustment -8) value)))

(define-vop (raw-instance-init/complex-double)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-double-reg)))
  (:arg-types * complex-double-float)
  (:info instance-length index)
  (:generator 4
    (inst movdqu (make-ea-for-raw-slot object instance-length :index index :adjustment -8) value)))

#|
For lack of a better location for these comments about the manipulation
of static symbols, note that we now have unfortunately two ways of doing it.
The following code rebinds *ALIEN-STACK-POINTER* to itself,
then subtracts 16 bytes.

* (disassemble '(lambda () (with-alien ((x (array char 16))) (print x))))

1) Load the symbol into a register, load the tls-index from the symbol,
   and access [thread_base+index]. This is an ordinary SYMBOL-VALUE call.

;       B80F0B1020       MOV EAX, 537922319
;       488B5021         MOV RDX, [RAX+33]
;       498B1414         MOV RDX, [R12+RDX]
;       4883FA61         CMP RDX, 97
;       7504             JNE L0
;       488B50F9         MOV RDX, [RAX-7]
; L0:   4883FA51         CMP RDX, 81
;       0F84EC000000     JEQ L3

2) Use a constant offset from the thread-base. This is the BIND/LET VOP.

;       498B8C24A8000000 MOV RCX, [R12+168]
;       488908           MOV [RAX], RCX
;       C74008A8000000   MOV DWORD PTR [RAX+8], 168
;       49899424A8000000 MOV [R12+168], RDX

We could benefit from additional INFO for special variables:
 - an indicator of whether to prefer that SYMBOL-VALUE use a known tls-index.
   This would be the same load-time mechanism as for BIND/LET. It is bad
   to force the loader to assign a tls-index for all reads of a symbol in
   general. Many symbols are never dynamically bound, and the potential number
   of globals (not DEFGLOBAL necessarily) in use by some applications
   could quickly exhaust the TLS.
 - an indicator of whether the symbol will definitely have a thread-local
   binding whenever it is read, such as for *ALIEN-STACK-POINTER*.
|#
