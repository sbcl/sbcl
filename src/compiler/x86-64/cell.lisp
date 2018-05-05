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

;; This vop is selected by name from vm-ir2tran for converting any
;; setter or setf'er that is defined by 'objdef'
(define-vop (set-slot)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg immediate)))
  (:info name offset lowtag)
 ;(:ignore name)
  (:results)
  (:generator 1
    (progn name) ; ignore it
    (gen-cell-set (make-ea-for-object-slot object offset lowtag) value nil)))

;; INIT-SLOT has to know about the :COMPACT-INSTANCE-HEADER feature.
(define-vop (init-slot set-slot)
  (:info name dx-p offset lowtag)
  (:generator 1
    (progn name dx-p)
    (cond #!+compact-instance-header
          ((and (eq name '%make-structure-instance) (eql offset :layout))
        ;; The layout is in the upper half of the header word.
           (inst mov
              (make-ea :dword :base object :disp (- 4 instance-pointer-lowtag))
              (if (sc-is value immediate)
                  (make-fixup (tn-value value) :layout)
                  (reg-in-size value :dword))))
          ((sc-is value immediate)
           (move-immediate (make-ea :qword
                                    :base object
                                    :disp (- (* offset n-word-bytes) lowtag))
                           (encode-value-if-immediate value)
                           temp-reg-tn (not dx-p)))
          (t
           (storew value object offset lowtag))))) ; Else, value not immediate.

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

(define-vop (%set-symbol-global-value)
  (:args (object :scs (descriptor-reg immediate))
         (value :scs (descriptor-reg any-reg immediate)))
  (:policy :fast-safe)
  (:generator 4
    (gen-cell-set (cond ((sc-is object immediate)
                         (symbol-slot-ea (tn-value object) symbol-value-slot))
                        (t
                         (make-ea-for-object-slot object symbol-value-slot
                                                  other-pointer-lowtag)))
                  value nil)))

(define-vop (fast-symbol-global-value)
  (:args (object :scs (descriptor-reg immediate)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast)
  (:translate sym-global-val)
  (:generator 4
    (cond ((sc-is object immediate)
           (inst mov value (symbol-slot-ea (tn-value object) symbol-value-slot)))
          (t
           (loadw value object symbol-value-slot other-pointer-lowtag)))))

(define-vop (symbol-global-value)
  (:policy :fast-safe)
  (:translate sym-global-val)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 9
    (let ((err-lab (generate-error-code vop 'unbound-symbol-error object)))
      (loadw value object symbol-value-slot other-pointer-lowtag)
      (inst cmp (reg-in-size value :dword) unbound-marker-widetag)
      (inst jmp :e err-lab))))

;; Return the DISP field to use in an EA relative to thread-base-tn
(defun load-time-tls-offset (symbol)
  (let ((where (info :variable :wired-tls symbol)))
    (cond ((integerp where) where)
          (t (make-fixup symbol :symbol-tls-index)))))

(deftransform %compare-and-swap-symbol-value ((symbol old new)
                                              ((constant-arg symbol) t t))
  (if (eq (info :variable :kind (sb!c::lvar-value symbol)) :global)
      `(%cas-symbol-global-value symbol old new)
      (sb!c::give-up-ir1-transform)))

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
             `(thread-tls-ea (load-time-tls-offset ,sym)))
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

  (define-vop (%cas-symbol-global-value)
    (:translate %cas-symbol-global-value)
    (:args (symbol :scs (descriptor-reg immediate) :to (:result 0))
           (old :scs (descriptor-reg any-reg) :target rax)
           (new :scs (descriptor-reg any-reg)))
    (:temporary (:sc descriptor-reg :offset rax-offset
                 :from (:argument 1) :to (:result 0)) rax)
    (:results (result :scs (descriptor-reg any-reg)))
    (:policy :fast-safe)
    (:generator 10
      (move rax old)
      (inst cmpxchg
            (if (sc-is symbol immediate)
                (symbol-slot-ea (tn-value symbol) symbol-value-slot)
                (access-value-slot symbol))
            new :lock)
      (move result rax)))

  #!+sb-thread
  (progn
    ;; TODO: SET could be shorter for any known wired-tls symbol.
    (define-vop (set)
      (:args (symbol :scs (descriptor-reg))
             (value :scs (descriptor-reg any-reg immediate)))
      (:temporary (:sc descriptor-reg) cell)
      (:generator 4
        ;; Compute the address into which to store. CMOV can only move into
        ;; a register, so we can't conditionally move into the TLS and
        ;; conditionally move in the opposite flag sense to the symbol.
        (compute-virtual-symbol)
        (gen-cell-set (access-value-slot cell) value nil)))

    ;; This code is tested by 'codegen.impure.lisp'
    (defun emit-symeval (value symbol symbol-reg check-boundp vop)
      (let* ((known-symbol-p (sc-is symbol constant immediate))
             (known-symbol (and known-symbol-p (tn-value symbol))))
        ;; In order from best to worst.
        (cond
         ((symbol-always-has-tls-value-p known-symbol) ; e.g. *HANDLER-CLUSTERS*
          (inst mov value (access-wired-tls-val known-symbol)))
         (t
          (cond
            ((symbol-always-has-tls-index-p known-symbol) ; e.g. CL:*PRINT-BASE*
             ;; Known nonzero TLS index, but possibly no per-thread value.
             ;; The TLS value and global value can be loaded independently.
             (inst mov value (access-wired-tls-val known-symbol))
             (when (sc-is symbol constant)
               (inst mov symbol-reg symbol))) ; = MOV Rxx, [RIP-N]

            (known-symbol-p ; unknown TLS index, possibly 0
             (sc-case symbol
               (immediate
               ;; load the TLS index from the symbol. TODO: use [RIP-n] mode
               ;; for immobile code to make it automatically relocatable.
               (inst mov (reg-in-size value :dword)
                     ;; slot index 1/2 is the high half of the header word.
                     (symbol-slot-ea known-symbol 1/2 :dword))
               ;; read the TLS value using that index
               (inst mov value (thread-tls-ea value :qword)))
               (constant

               ;; These reads are inextricably data-dependent
               (inst mov symbol-reg symbol) ; = MOV REG, [RIP-N]
               (inst mov (reg-in-size value :dword) (tls-index-of symbol-reg))
               (inst mov value (thread-tls-ea value :qword)))))

            (t ; SYMBOL-VALUE of a random symbol
             (inst mov (reg-in-size symbol-reg :dword) (tls-index-of symbol))
             (inst mov value (thread-tls-ea symbol-reg :qword))
             (setq symbol-reg symbol)))

          ;; Load the global value if the TLS value didn't exist
          (inst cmp (reg-in-size value :dword) no-tls-value-marker-widetag)
          (inst cmov :e value
                (if (and known-symbol-p (sc-is symbol immediate))
                    (symbol-slot-ea known-symbol symbol-value-slot) ; MOV Rxx, imm32
                    (access-value-slot symbol-reg)))))

        (when check-boundp
          (assemble ()
            (inst cmp (reg-in-size value :dword) unbound-marker-widetag)
            (let* ((immediatep (sc-is symbol immediate))
                   (staticp (and immediatep (static-symbol-p known-symbol)))
                   (*location-context* (make-restart-location RETRY value)))
              ;; IMMEDIATE sc symbols are not in a register (they are accessed
              ;; via absolute address), nor are they present in the code header.
              ;; So emit a MOV just before the INT opcode for such symbols,
              ;; out of the normal execution path. Most static symbols are
              ;; DEFCONSTANTs or DEFGLOBALs, so this case is infrequent.
              (inst jmp :e (generate-error-code+
                            (if staticp
                                (lambda ()
                                  (load-immediate vop symbol symbol-reg)))
                            vop 'unbound-symbol-error
                            (if (and immediatep (not staticp))
                                symbol
                                symbol-reg))))
            RETRY))))

  ;; With Symbol-Value, we check that the value isn't the trap object. So
  ;; Symbol-Value of NIL is NIL.
    (define-vop (symbol-value)
      (:translate symeval)
      (:policy :fast-safe)
      (:args (symbol :scs (descriptor-reg constant immediate) :to (:result 1)))
      ;; TODO: use no temp if the symbol is known to be thread-local
      ;; (probably IR1 should go SYMBOL-VALUE -> SYMBOL-TLS-VALUE)
      (:temporary (:sc descriptor-reg) symbol-reg)
      (:results (value :scs (descriptor-reg any-reg)))
      (:vop-var vop)
      (:save-p :compute-only)
      (:variant-vars check-boundp)
      (:variant t)
      (:generator 9 (emit-symeval value symbol symbol-reg check-boundp vop)))

    (define-vop (fast-symbol-value symbol-value)
    ;; KLUDGE: not really fast, in fact, because we're going to have to
    ;; do a full lookup of the thread-local area anyway.  But half of
    ;; the meaning of FAST-SYMBOL-VALUE is "do not signal an error if
    ;; unbound", which is used in the implementation of COPY-SYMBOL.  --
    ;; CSR, 2003-04-22
      (:policy :fast)
      (:variant nil)
      (:variant-cost 5))

    (define-vop (boundp)
      (:translate boundp)
      (:policy :fast-safe)
      (:args (object :scs (descriptor-reg)))
      (:conditional :ne)
      (:temporary (:sc dword-reg) temp)
      (:generator 9
        (inst mov temp (tls-index-of object))
        (inst mov temp (thread-tls-ea temp :dword))
        (inst cmp temp no-tls-value-marker-widetag)
        (inst cmov :e temp (access-value-slot object :dword))
        (inst cmp temp unbound-marker-widetag))))

) ; END OF MACROLET

#!-sb-thread
(progn
  (define-vop (symbol-value symbol-global-value)
    (:translate symeval))
  (define-vop (fast-symbol-value fast-symbol-global-value)
    (:translate symeval))
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
    (let* ((*location-context* (make-restart-location RETRY value))
           (err-lab (generate-error-code vop 'undefined-fun-error object)))
      (inst jmp :e err-lab))
    RETRY))

#!-immobile-code
(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:translate (setf fdefn-fun))
  (:args (function :scs (descriptor-reg) :target result)
         (fdefn :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) raw)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (inst mov raw (make-fixup 'closure-tramp :assembly-routine))
    (inst cmp (make-ea :byte :base function :disp (- fun-pointer-lowtag))
          simple-fun-widetag)
    (inst cmov :e raw
          (make-ea :qword :base function
                   :disp (- (* simple-fun-self-slot n-word-bytes) fun-pointer-lowtag)))
    (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
    (storew raw fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result function)))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:results (result :scs (descriptor-reg)))
  (:vop-var vop)
  #!+immobile-code (:temporary (:sc unsigned-reg) temp)
  (:generator 38
    #!+immobile-code
    (let ((tramp (make-fixup 'undefined-fdefn :assembly-routine)))
     (if (sb!c::code-immobile-p vop)
         (inst lea temp (make-ea :qword :base rip-tn :disp tramp))
         (inst mov temp tramp))
     ;; Compute displacement from the call site
     (inst sub (reg-in-size temp :dword) (reg-in-size fdefn :dword))
     (inst sub (reg-in-size temp :dword)
           (+ (- other-pointer-lowtag) (ash fdefn-raw-addr-slot word-shift) 5))
     ;; Compute the encoding of a "CALL rel32" instruction
     (inst shl temp 8)
     (inst or (reg-in-size temp :byte) #xE8)
     ;; Store
     (storew nil-value fdefn fdefn-fun-slot other-pointer-lowtag)
     (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag))
    #!-immobile-code
    (progn
     (storew nil-value fdefn fdefn-fun-slot other-pointer-lowtag)
     (storew (make-fixup 'undefined-tramp :assembly-routine)
             fdefn fdefn-raw-addr-slot other-pointer-lowtag))
    (move result fdefn)))

;;;; binding and unbinding

;;; BIND -- Establish VAL as a binding for SYMBOL. Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.

#!+sb-thread
(progn
(define-vop (dynbind) ; bind a symbol in a PROGV form
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset rax-offset) tls-index)
  (:temporary (:sc unsigned-reg) bsp tmp)
  (:vop-var vop)
  (:generator 10
    (load-binding-stack-pointer bsp)
    (inst mov (reg-in-size tls-index :dword) (tls-index-of symbol))
    (inst add bsp (* binding-size n-word-bytes))
    (store-binding-stack-pointer bsp)
    (inst test (reg-in-size tls-index :dword) (reg-in-size tls-index :dword))
    (inst jmp :ne TLS-INDEX-VALID)
    (inst mov tls-index symbol)
    (invoke-asm-routine 'call 'alloc-tls-index vop)
    TLS-INDEX-VALID
    (inst mov tmp (thread-tls-ea tls-index))
    (storew tmp bsp (- binding-value-slot binding-size))
    (storew tls-index bsp (- binding-symbol-slot binding-size))
    (inst mov (thread-tls-ea tls-index) val)))

(define-vop (bind) ; bind a known symbol
  (:args (val :scs (any-reg descriptor-reg)))
  (:temporary (:sc unsigned-reg) bsp tmp)
  (:info symbol)
  (:generator 10
    (inst mov bsp (* binding-size n-word-bytes))
    (inst xadd
          (thread-tls-ea (ash thread-binding-stack-pointer-slot word-shift))
          bsp)
    (let* ((tls-index (load-time-tls-offset symbol))
           (tls-cell (thread-tls-ea tls-index)))
      ;; Too bad we can't use "XCHG [thread + disp], val" to write new value
      ;; and read the old value in one step. It will violate the constraints
      ;; prescribed in the internal documentation on special binding.
      (inst mov tmp tls-cell)
      (storew tmp bsp binding-value-slot)
      ;; Indices are small enough to be written as :DWORDs which avoids
      ;; a REX prefix if 'bsp' happens to be any of the low 8 registers.
      (inst mov (make-ea :dword :base bsp
                         :disp (ash binding-symbol-slot word-shift)) tls-index)
      (inst mov tls-cell val)))))

#!-sb-thread
(define-vop (dynbind)
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
  (:temporary (:sc unsigned-reg) temp bsp)
  (:temporary (:sc complex-double-reg) zero)
  (:info symbols)
  (:generator 0
    (load-binding-stack-pointer bsp)
    (inst xorpd zero zero)
    (loop for symbol in symbols
          for tls-index = (load-time-tls-offset symbol)
          for tls-cell = (thread-tls-ea tls-index)
          do
          (inst sub bsp (* binding-size n-word-bytes))

          ;; Load VALUE from stack, then restore it to the TLS area.
          (loadw temp bsp binding-value-slot)
          (inst mov tls-cell temp)
          ;; Zero out the stack.
          (inst movapd (make-ea :qword :base bsp) zero))
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
  (:temporary (:sc complex-double-reg) zero)
  (:generator 0
    (load-binding-stack-pointer bsp)
    (inst cmp where bsp)
    (inst jmp :e DONE)
    (inst xorpd zero zero)
    LOOP
    (inst sub bsp (* binding-size n-word-bytes))
    ;; on sb-thread symbol is actually a tls-index, and it fits into
    ;; 32-bits.
    #!+sb-thread
    (let ((tls-index (reg-in-size symbol :dword)))
      (inst mov tls-index
            (make-ea :dword :base bsp :disp (* binding-symbol-slot n-word-bytes)))
      (inst test tls-index tls-index))
    #!-sb-thread
    (progn
      (loadw symbol bsp binding-symbol-slot)
      (inst test symbol symbol))
    (inst jmp :z SKIP)
    (loadw value bsp binding-value-slot)
    #!-sb-thread
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    #!+sb-thread
    (inst mov (thread-tls-ea symbol) value)

    SKIP
    (inst movapd (make-ea :qword :base bsp) zero)

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

(define-vop (closure-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:info offset)
  (:generator 4
    (loadw value object (+ closure-info-offset offset) fun-pointer-lowtag)))

(define-vop (closure-init)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:info offset)
  (:generator 4
    (storew value object (+ closure-info-offset offset) fun-pointer-lowtag)))

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
  ;; Explicitly fixnumizing admits a 32-bit left-shift which might encode
  ;; in 1 less byte. (Other backends return an UNSIGNED-REG here.)
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (let ((res (reg-in-size res :dword)))
      (inst movzx res (make-ea :word :base struct
                               :disp (1+ (- instance-pointer-lowtag))))
      (inst shl res n-fixnum-tag-bits))))

#!+compact-instance-header
(progn
 (define-vop (%instance-layout)
   (:translate %instance-layout)
   (:policy :fast-safe)
   (:args (object :scs (descriptor-reg)))
   (:results (res :scs (descriptor-reg)))
   (:variant-vars lowtag)
   (:variant instance-pointer-lowtag)
   (:generator 1
    (inst mov (reg-in-size res :dword) (make-ea :dword :base object :disp (- 4 lowtag)))))
 (define-vop (%set-instance-layout)
   (:translate %set-instance-layout)
   (:policy :fast-safe)
   (:args (object :scs (descriptor-reg))
          (value :scs (any-reg descriptor-reg) :target res))
   (:results (res :scs (any-reg descriptor-reg)))
   (:variant-vars lowtag)
   (:variant instance-pointer-lowtag)
   (:generator 2
    (inst mov (make-ea :dword :base object :disp (- 4 lowtag)) (reg-in-size value :dword))
    (move res value)))
 (define-vop (%funcallable-instance-layout %instance-layout)
   (:translate %funcallable-instance-layout)
   (:variant fun-pointer-lowtag))
 (define-vop (%set-funcallable-instance-layout %set-instance-layout)
   (:translate %set-funcallable-instance-layout)
   (:variant fun-pointer-lowtag)))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (any-reg descriptor-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (any-reg descriptor-reg immediate) * %instance-set)

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

(flet ((make-ea-for-raw-slot (object index)
         (etypecase index
           (integer
              (make-ea :qword
                       :base object
                       :disp (+ (* (+ instance-slots-offset index) n-word-bytes)
                                (- instance-pointer-lowtag))))
           (tn
              (make-ea :qword
                       :base object
                       :index index
                       :scale (ash 1 (- word-shift n-fixnum-tag-bits))
                       :disp (+ (* instance-slots-offset n-word-bytes)
                                (- instance-pointer-lowtag)))))))
  (macrolet
      ((def (suffix result-sc result-type inst &optional (inst/c inst))
         `(progn
            (define-vop (,(symbolicate "RAW-INSTANCE-REF/" suffix))
              (:translate ,(symbolicate "%RAW-INSTANCE-REF/" suffix))
              (:policy :fast-safe)
              (:args (object :scs (descriptor-reg)) (index :scs (any-reg)))
              (:arg-types * tagged-num)
              (:results (value :scs (,result-sc)))
              (:result-types ,result-type)
              (:generator 5
                (inst ,inst value (make-ea-for-raw-slot object index))))
            (define-vop (,(symbolicate "RAW-INSTANCE-REF-C/" suffix))
              (:translate ,(symbolicate "%RAW-INSTANCE-REF/" suffix))
              (:policy :fast-safe)
              (:args (object :scs (descriptor-reg)))
              ;; Why are we pedantic about the index constraint here
              ;; if we're not equally so in the init vop?
              (:arg-types * (:constant (load/store-index #.n-word-bytes
                                                         #.instance-pointer-lowtag
                                                         #.instance-slots-offset)))
              (:info index)
              (:results (value :scs (,result-sc)))
              (:result-types ,result-type)
              (:generator 4
                (inst ,inst/c value (make-ea-for-raw-slot object index))))
            (define-vop (,(symbolicate "RAW-INSTANCE-SET/" suffix))
              (:translate ,(symbolicate "%RAW-INSTANCE-SET/" suffix))
              (:policy :fast-safe)
              (:args (object :scs (descriptor-reg))
                     (index :scs (any-reg))
                     (value :scs (,result-sc) :target result))
              (:arg-types * tagged-num ,result-type)
              (:results (result :scs (,result-sc)))
              (:result-types ,result-type)
              (:generator 5
                (inst ,inst (make-ea-for-raw-slot object index) value)
                (move result value)))
            (define-vop (,(symbolicate "RAW-INSTANCE-SET-C/" suffix))
              (:translate ,(symbolicate "%RAW-INSTANCE-SET/" suffix))
              (:policy :fast-safe)
              (:args (object :scs (descriptor-reg))
                     (value :scs (,result-sc) :target result))
              (:arg-types * (:constant (load/store-index #.n-word-bytes
                                                         #.instance-pointer-lowtag
                                                         #.instance-slots-offset))
                          ,result-type)
              (:info index)
              (:results (result :scs (,result-sc)))
              (:result-types ,result-type)
              (:generator 4
                (inst ,inst/c (make-ea-for-raw-slot object index) value)
                (move result value)))
            (define-vop (,(symbolicate "RAW-INSTANCE-INIT/" suffix))
              (:args (object :scs (descriptor-reg))
                     (value :scs (,result-sc)))
              (:arg-types * ,result-type)
              (:info index)
              (:generator 4
                (inst ,inst/c (make-ea-for-raw-slot object index) value))))))
    (def word unsigned-reg unsigned-num mov)
    (def signed-word signed-reg signed-num mov)
    (def single single-reg single-float movss)
    (def double double-reg double-float movsd)
    (def complex-single complex-single-reg complex-single-float movq)
    (def complex-double complex-double-reg complex-double-float
         movupd (if (oddp index) 'movapd 'movupd)))

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
      (move result diff))))

;;;;

(defknown %cons-cas-pair (cons t t t t) (values t t))
(defknown %vector-cas-pair (simple-vector index t t t t) (values t t))
;; %INSTANCE-CAS-PAIR only operates on tagged slots (for now)
(defknown %instance-cas-pair (instance index t t t t) (values t t))

(defun generate-dblcas (memory-operand old-lo old-hi new-lo new-hi
                        rax rbx rcx rdx result-lo result-hi)
  (move rax old-lo)
  (move rdx old-hi)
  (move rbx new-lo)
  (move rcx new-hi)
  (inst cmpxchg16b memory-operand :lock)
  ;; RDX:RAX hold the actual old contents of memory.
  ;; Manually analyze result lifetimes to avoid clobbering.
  (cond ((and (location= result-lo rdx) (location= result-hi rax))
         (inst xchg rax rdx)) ; unlikely, but possible
        ((location= result-lo rdx) ; result-hi is not rax
         (move result-hi rdx) ; move high part first
         (move result-lo rax))
        (t                    ; result-lo is not rdx
         (move result-lo rax) ; move low part first
         (move result-hi rdx))))

(macrolet
    ((define-cmpxchg-vop (name memory-operand more-stuff &optional index-arg)
       `(define-vop (,name)
          (:policy :fast-safe)
          ,@more-stuff
          (:args (object :scs (descriptor-reg) :to :eval)
                 ,@index-arg
                 (expected-old-lo :scs (descriptor-reg any-reg) :target eax)
                 (expected-old-hi :scs (descriptor-reg any-reg) :target edx)
                 (new-lo :scs (descriptor-reg any-reg) :target ebx)
                 (new-hi :scs (descriptor-reg any-reg) :target ecx))
          (:results (result-lo :scs (descriptor-reg any-reg))
                    (result-hi :scs (descriptor-reg any-reg)))
          (:temporary (:sc unsigned-reg :offset eax-offset
                       :from (:argument 2) :to (:result 0)) eax)
          (:temporary (:sc unsigned-reg :offset edx-offset
                       :from (:argument 3) :to (:result 0)) edx)
          (:temporary (:sc unsigned-reg :offset ebx-offset
                       :from (:argument 4) :to (:result 0)) ebx)
          (:temporary (:sc unsigned-reg :offset ecx-offset
                       :from (:argument 5) :to (:result 0)) ecx)
          (:generator 7
           (generate-dblcas ,memory-operand
                            expected-old-lo expected-old-hi new-lo new-hi
                            eax ebx ecx edx result-lo result-hi)))))
  (define-cmpxchg-vop compare-and-exchange-pair
      (make-ea :dword :base object :disp (- list-pointer-lowtag))
      ((:translate %cons-cas-pair)))
  (define-cmpxchg-vop compare-and-exchange-pair-indexed
      (make-ea :dword :base object :disp offset :index index
                      :scale (ash n-word-bytes (- n-fixnum-tag-bits)))
      ((:variant-vars offset))
      ((index :scs (descriptor-reg any-reg) :to :eval))))

;; The CPU requires 16-byte alignment for the memory operand.
;; A vector's data portion starts on a 16-byte boundary,
;; so any even numbered index is OK.
(define-vop (%vector-cas-pair compare-and-exchange-pair-indexed)
  (:translate %vector-cas-pair)
  (:variant (- (* n-word-bytes vector-data-offset) other-pointer-lowtag)))

;; Here you specify an odd numbered slot, otherwise get a bus error.
;; An instance's first user-visible slot at index 1 is 16-byte-aligned.
(define-vop (%instance-cas-pair compare-and-exchange-pair-indexed)
  (:translate %instance-cas-pair)
  (:variant (- (* n-word-bytes instance-slots-offset) instance-pointer-lowtag)))
