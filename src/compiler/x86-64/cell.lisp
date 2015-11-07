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

;; Return T if SYMBOL will be (at no later than load time) wired to a TLS index.
;; When a symbol gets a tls index, it is permanently at that index, so it is
;; by some definition "wired", but here the term specifically means that an
;; index can/should be encoded into the instruction for reading that symbol's
;; thread-local value, as contrasted with reading its index from itself.
;; The BIND vop forces wiring; the BOUNDP vop never does, the SET vop doesn't
;; but could, and {FAST-}SYMBOL-VALUE vops optimize their output appropriately
;; based on wiring. Except when a symbol maps into a thread slot, the compiler
;; doesn't care what the actual index is - that's the loader's purview.
(defun wired-tls-symbol-p (symbol)
  (not (null (info :variable :wired-tls symbol))))

;; Return the DISP field to use in an EA relative to thread-base-tn
(defun load-time-tls-offset (symbol)
  (let ((where (info :variable :wired-tls symbol)))
    (cond ((integerp where) where)
          (t (make-fixup symbol :symbol-tls-index)))))

;; Return T if reference to symbol doesn't need to check for no-tls-value.
;; True of thread struct slots. More generally we could add a declaration.
(defun symbol-always-thread-local-p (symbol)
  (let ((where (info :variable :wired-tls symbol)))
    (or (eq where :ALWAYS-THREAD-LOCAL) (integerp where))))

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
           (access-wired-tls-val (sym) ; SYM is a symbol
             `(make-ea :qword :disp (load-time-tls-offset ,sym)
                       :base thread-base-tn))
           (access-tls-val (index size)
             `(make-ea ,size :base thread-base-tn :index ,index :scale 1))
           (access-value-slot (sym &optional (size :qword)) ; SYM is a TN
             (ecase size
               (:dword `(make-ea-for-object-slot-half
                         ,sym symbol-value-slot other-pointer-lowtag))
               (:qword `(make-ea-for-object-slot
                         ,sym symbol-value-slot other-pointer-lowtag)))))

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

  #!+sb-thread
  (progn
    ;; TODO: SET could be shorter for any known wired-tls symbol.
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
      (:args (symbol :scs (descriptor-reg constant) :to (:result 1)))
      (:temporary (:sc descriptor-reg) symbol-reg)
      (:results (value :scs (descriptor-reg any-reg)))
      (:vop-var vop)
      (:save-p :compute-only)
      (:variant-vars check-boundp)
      (:variant t)
      (:generator 9
        (cond
          ((not (sc-is symbol constant)) ; SYMBOL-VALUE of a random symbol
           ;; These reads are inextricably data-dependent
           (inst mov (reg-in-size symbol-reg :dword) (tls-index-of symbol))
           (inst mov value (access-tls-val symbol-reg :qword))
           (setq symbol-reg symbol))
          ((wired-tls-symbol-p (tn-value symbol)) ; e.g. CL:*PRINT-BASE*
           (inst mov symbol-reg symbol) ; = MOV Rxx, [RIP-N]
           (inst mov value (access-wired-tls-val (tn-value symbol))))
          (t ; commonest case: constant-tn for the symbol, not wired tls
           ;; Same data-dependencies as the non-constant-tn case.
           (inst mov symbol-reg symbol) ; = MOV REG, [RIP-N]
           (inst mov (reg-in-size value :dword) (tls-index-of symbol-reg))
           (inst mov value (access-tls-val value :qword))))
        (unless (and (sc-is symbol constant)
                     (symbol-always-thread-local-p (tn-value symbol)))
          (inst cmp (reg-in-size value :dword) no-tls-value-marker-widetag)
          (inst cmov :e value (access-value-slot symbol-reg)))
        (when check-boundp
          (inst cmp (reg-in-size value :dword) unbound-marker-widetag)
          (inst jmp :e (generate-error-code vop 'unbound-symbol-error
                                            symbol-reg)))))

    (define-vop (fast-symbol-value symbol-value)
    ;; KLUDGE: not really fast, in fact, because we're going to have to
    ;; do a full lookup of the thread-local area anyway.  But half of
    ;; the meaning of FAST-SYMBOL-VALUE is "do not signal an error if
    ;; unbound", which is used in the implementation of COPY-SYMBOL.  --
    ;; CSR, 2003-04-22
      (:policy :fast)
      (:variant nil)
      (:variant-cost 5))

    ;; SYMBOL-VALUE of a static symbol with a wired TLS index does not
    ;; load the symbol except in the case of error, and uses no temp either.
    ;; There's no way to express the lack of encumbrances in the general vop.
    (define-vop (symeval/static-wired)
      (:translate symbol-value)
      (:policy :fast-safe)
      ;; The predicates are orthogonal. Symbols can satisfy one, the other,
      ;; both, or neither. This vop applies only if both.
      (:arg-types (:constant (and (satisfies static-symbol-p)
                                  (satisfies wired-tls-symbol-p))))
      (:info symbol)
      (:results (value :scs (descriptor-reg any-reg)))
      (:vop-var vop)
      (:save-p :compute-only)
      (:variant-vars check-boundp)
      (:variant t)
      (:generator 5
        (inst mov value (access-wired-tls-val symbol))
        (unless (symbol-always-thread-local-p symbol)
          (inst cmp (reg-in-size value :dword) no-tls-value-marker-widetag)
          (inst cmov :e value (static-symbol-value-ea symbol)))
        (when check-boundp
          (let ((err-label (gen-label)))
            (assemble (*elsewhere*)
              (emit-label err-label)
              (inst mov value (+ nil-value (static-symbol-offset symbol)))
              (emit-error-break vop error-trap
                                (error-number-or-lose 'unbound-symbol-error)
                                (list value)))
            (inst cmp (reg-in-size value :dword) unbound-marker-widetag)
            (inst jmp :e err-label)))))

    (define-vop (fast-symeval/static-wired symeval/static-wired)
      (:policy :fast)
      (:variant nil)
      (:variant-cost 3))

    ;; Would it be worthwhile to make a static/wired boundp vop?
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
        (inst cmp temp unbound-marker-widetag))))

) ; END OF MACROLET

#!-sb-thread
(progn
  (define-vop (symbol-value symbol-global-value)
    (:translate symbol-value))
  (define-vop (fast-symbol-value fast-symbol-global-value)
    (:translate symbol-value))
  (define-vop (set %set-symbol-global-value))
  (define-vop (boundp)
    (:translate boundp)
    (:policy :fast-safe)
    (:args (symbol :scs (descriptor-reg)))
    (:conditional :ne)
    (:generator 9
      (inst cmp (make-ea-for-object-slot-half
                 symbol symbol-value-slot other-pointer-lowtag)
            unbound-marker-widetag))))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    ;; The symbol-hash slot of NIL holds NIL because it is also the
    ;; cdr slot, so we have to zero the fixnum tag bit(s) to make sure
    ;; it is a fixnum.  The lowtag selection magic that is required to
    ;; ensure this is explained in the comment in objdef.lisp
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst and res (lognot fixnum-tag-mask))))

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
    ;; byte comparison works because lowtags of function and nil differ
    (inst cmp (reg-in-size value :byte) (logand nil-value #xff))
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
(progn
(define-vop (bind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg) :target tmp
                 :to :load))
  (:temporary (:sc unsigned-reg :offset rax-offset) tls-index)
  (:temporary (:sc unsigned-reg) bsp tmp)
  (:generator 10
    (load-binding-stack-pointer bsp)
    (inst mov (reg-in-size tls-index :dword) (tls-index-of symbol))
    (inst add bsp (* binding-size n-word-bytes))
    (store-binding-stack-pointer bsp)
    (inst test (reg-in-size tls-index :dword) (reg-in-size tls-index :dword))
    (inst jmp :ne TLS-INDEX-VALID)
    (inst mov tls-index symbol)
    (inst mov tmp (make-fixup 'alloc-tls-index :assembly-routine))
    (inst call tmp)
    TLS-INDEX-VALID
    (inst mov tmp (make-ea :qword :base thread-base-tn :index tls-index))
    (storew tmp bsp (- binding-value-slot binding-size))
    (storew tls-index bsp (- binding-symbol-slot binding-size))
    (inst mov (make-ea :qword :base thread-base-tn :index tls-index) val)))

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
    (let* ((tls-index (load-time-tls-offset symbol))
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
    ;; should use a load-time constant displacement to the TLS for this symbol.
    (unless (info :variable :wired-tls symbol)
      ;; setting INFO is inefficient when done repeatedly for no reason.
      ;; (I should fix that)
      (setf (info :variable :wired-tls symbol) :always-has-tls)))))

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

(define-full-compare-and-swap %instance-cas instance
  instance-slots-offset instance-pointer-lowtag
  (any-reg descriptor-reg) * %instance-cas)
(define-full-compare-and-swap %raw-instance-cas/word instance
  instance-slots-offset instance-pointer-lowtag
  (unsigned-reg) unsigned-num %raw-instance-cas/word)

;;;; code object frobbing

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (any-reg descriptor-reg) * code-header-ref)

(define-full-setter code-header-set * 0 other-pointer-lowtag
  (any-reg descriptor-reg) * code-header-set)

;;;; raw instance slot accessors

(defun make-ea-for-raw-slot (object index)
  (etypecase index
    (integer
       (make-ea :qword
                :base object
                :disp (+ (* (+ instance-slots-offset index)
                            n-word-bytes)
                         (- instance-pointer-lowtag))))
    (tn
       (make-ea :qword
                :base object
                :index index
                :scale (ash 1 (- word-shift n-fixnum-tag-bits))
                :disp (+ (* instance-slots-offset n-word-bytes)
                         (- instance-pointer-lowtag))))))

(define-vop (raw-instance-ref/word)
  (:translate %raw-instance-ref/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)) (index :scs (any-reg)))
  (:arg-types * tagged-num)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst mov value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-ref-c/word)
  (:translate %raw-instance-ref/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:results (value :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (inst mov value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-set/word)
  (:translate %raw-instance-set/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (unsigned-reg) :target result))
  (:arg-types * tagged-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst mov (make-ea-for-raw-slot object index) value)
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
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (inst mov (make-ea-for-raw-slot object index) value)
    (move result value)))

(define-vop (raw-instance-init/word)
  (:args (object :scs (descriptor-reg))
         (value :scs (unsigned-reg)))
  (:arg-types * unsigned-num)
  (:info index)
  (:generator 4
    (inst mov (make-ea-for-raw-slot object index) value)))

(define-vop (raw-instance-atomic-incf/word)
  (:translate %raw-instance-atomic-incf/word)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (diff :scs (unsigned-reg) :target result))
  (:arg-types * tagged-num unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst xadd (make-ea-for-raw-slot object index) diff :lock)
    (move result diff)))

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
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 4
    (inst xadd (make-ea-for-raw-slot object index) diff :lock)
    (move result diff)))

(define-vop (raw-instance-ref/single)
  (:translate %raw-instance-ref/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (inst movss value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-ref-c/single)
  (:translate %raw-instance-ref/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:results (value :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
    (inst movss value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-set/single)
  (:translate %raw-instance-set/single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (single-reg) :target result))
  (:arg-types * positive-fixnum single-float)
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 5
    (inst movss (make-ea-for-raw-slot object index) value)
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
  (:results (result :scs (single-reg)))
  (:result-types single-float)
  (:generator 4
    (inst movss (make-ea-for-raw-slot object index) value)
    (move result value)))

(define-vop (raw-instance-init/single)
  (:args (object :scs (descriptor-reg))
         (value :scs (single-reg)))
  (:arg-types * single-float)
  (:info index)
  (:generator 4
    (inst movss (make-ea-for-raw-slot object index) value)))

(define-vop (raw-instance-ref/double)
  (:translate %raw-instance-ref/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
    (inst movsd value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-ref-c/double)
  (:translate %raw-instance-ref/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:results (value :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
    (inst movsd value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-set/double)
  (:translate %raw-instance-set/double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (double-reg) :target result))
  (:arg-types * positive-fixnum double-float)
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 5
    (inst movsd (make-ea-for-raw-slot object index) value)
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
  (:results (result :scs (double-reg)))
  (:result-types double-float)
  (:generator 4
    (inst movsd (make-ea-for-raw-slot object index) value)
    (move result value)))

(define-vop (raw-instance-init/double)
  (:args (object :scs (descriptor-reg))
         (value :scs (double-reg)))
  (:arg-types * double-float)
  (:info index)
  (:generator 4
    (inst movsd (make-ea-for-raw-slot object index) value)))

(define-vop (raw-instance-ref/complex-single)
  (:translate %raw-instance-ref/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (inst movq value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-ref-c/complex-single)
  (:translate %raw-instance-ref/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:results (value :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (inst movq value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-set/complex-single)
  (:translate %raw-instance-set/complex-single)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-single-reg) :target result))
  (:arg-types * positive-fixnum complex-single-float)
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 5
    (move result value)
    (inst movq (make-ea-for-raw-slot object index) value)))

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
  (:results (result :scs (complex-single-reg)))
  (:result-types complex-single-float)
  (:generator 4
    (move result value)
    (inst movq (make-ea-for-raw-slot object index) value)))

(define-vop (raw-instance-init/complex-single)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-single-reg)))
  (:arg-types * complex-single-float)
  (:info index)
  (:generator 4
    (inst movq (make-ea-for-raw-slot object index) value)))

(define-vop (raw-instance-ref/complex-double)
  (:translate %raw-instance-ref/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg)))
  (:arg-types * positive-fixnum)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 5
    (inst movdqu value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-ref-c/complex-double)
  (:translate %raw-instance-ref/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg)))
  (:arg-types * (:constant (load/store-index #.n-word-bytes
                                             #.instance-pointer-lowtag
                                             #.instance-slots-offset)))
  (:info index)
  (:results (value :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 4
    (inst movdqu value (make-ea-for-raw-slot object index))))

(define-vop (raw-instance-set/complex-double)
  (:translate %raw-instance-set/complex-double)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (complex-double-reg) :target result))
  (:arg-types * positive-fixnum complex-double-float)
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 5
    (move result value)
    (inst movdqu (make-ea-for-raw-slot object index) value)))

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
  (:results (result :scs (complex-double-reg)))
  (:result-types complex-double-float)
  (:generator 4
    (move result value)
    (inst movdqu (make-ea-for-raw-slot object index) value)))

(define-vop (raw-instance-init/complex-double)
  (:args (object :scs (descriptor-reg))
         (value :scs (complex-double-reg)))
  (:arg-types * complex-double-float)
  (:info index)
  (:generator 4
    (inst movdqu (make-ea-for-raw-slot object index) value)))
