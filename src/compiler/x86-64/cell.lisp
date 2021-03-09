;;;; various primitive memory access VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

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
  (:results)
  (:vop-var vop)
  (:generator 1
    (cond ((emit-code-page-write-barrier-p name)
           ;; Don't assume that the immediate is acceptable to 'push'
           ;; (only MOV can take a 64-bit immediate)
           (inst mov temp-reg-tn (encode-value-if-immediate value))
           (inst push temp-reg-tn)
           ;; Push the slot index into code, then code itself
           (cond ((= lowtag fun-pointer-lowtag)
                  ;; compute code address similarly to CODE-FROM-FUNCTION vop
                  (inst mov :dword temp-reg-tn (ea (- fun-pointer-lowtag) object))
                  (inst shr :dword temp-reg-tn n-widetag-bits)
                  ;; now temp-reg-tn holds the difference in words from code to fun.
                  (inst push temp-reg-tn)
                  (inst add :qword (ea 0 rsp-tn) offset) ; for particular slot
                  ;; finish computing the code address
                  (inst neg temp-reg-tn)
                  (inst lea temp-reg-tn
                        (ea (- other-pointer-lowtag fun-pointer-lowtag)
                            object temp-reg-tn n-word-bytes))
                  (inst push temp-reg-tn))
                 (t ; is code already
                  (inst push offset)
                  (inst push object)))
           (invoke-asm-routine 'call 'code-header-set vop))
          ((equal name '(setf %funcallable-instance-fun))
           (gen-cell-set (object-slot-ea object offset lowtag) value nil vop t))
          (t
           (gen-cell-set (object-slot-ea object offset lowtag) value nil)))))

;; INIT-SLOT has to know about the :COMPACT-INSTANCE-HEADER feature.
(define-vop (init-slot set-slot)
  (:info name dx-p offset lowtag)
  (:generator 1
    (progn name dx-p)
    (cond #+compact-instance-header
          ((and (eq name '%make-structure-instance) (eql offset :layout))
        ;; The layout is in the upper half of the header word.
           (inst mov :dword (ea (- 4 instance-pointer-lowtag) object)
                 (if (sc-is value immediate)
                     (make-fixup (tn-value value) :layout)
                     value)))
          ((sc-is value immediate)
           (move-immediate (ea (- (* offset n-word-bytes) lowtag) object)
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
     (inst cmpxchg :lock (ea (- (* offset n-word-bytes) lowtag) object) new)
     (move result rax)))

;;;; symbol hacking VOPs

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst mov result (logior (+ sb-vm:static-space-start #x100)
                             unbound-marker-widetag))))

(define-vop (%set-symbol-global-value)
  (:args (object :scs (descriptor-reg immediate))
         (value :scs (descriptor-reg any-reg immediate)))
  (:policy :fast-safe)
  (:generator 4
    (gen-cell-set (cond ((sc-is object immediate)
                         (symbol-slot-ea (tn-value object) symbol-value-slot))
                        (t
                         (object-slot-ea object symbol-value-slot
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
      (inst cmp :byte value unbound-marker-widetag)
      (inst jmp :e err-lab))))

;; Return the DISP field to use in an EA relative to thread-base-tn
(defun load-time-tls-offset (symbol)
  (let ((where (info :variable :wired-tls symbol)))
    (cond ((integerp where) where)
          (t (make-fixup symbol :symbol-tls-index)))))

(deftransform %compare-and-swap-symbol-value ((symbol old new)
                                              ((constant-arg symbol) t t))
  (if (eq (info :variable :kind (sb-c::lvar-value symbol)) :global)
      `(%cas-symbol-global-value symbol old new)
      (sb-c::give-up-ir1-transform)))

(macrolet (;; Logic common to thread-aware SET and CAS. CELL is assigned
           ;; to the location that should be accessed to modify SYMBOL's
           ;; value either in the TLS or the symbol's value slot as follows:
           ;; (1) make it look as if the TLS cell were a symbol by biasing
           ;;     upward by other-pointer-lowtag less 1 word.
           ;; (2) conditionally make CELL point to the symbol itself
           (compute-virtual-symbol ()
             `(progn
                (inst mov :dword cell (tls-index-of symbol))
                (inst lea cell
                      (ea (- other-pointer-lowtag (ash symbol-value-slot word-shift))
                          thread-base-tn cell))
                (inst cmp :dword (symbol-value-slot-ea cell) ; TLS reference
                      no-tls-value-marker-widetag)
                (inst cmov :e cell symbol))) ; now possibly get the symbol
           (access-wired-tls-val (sym) ; SYM is a symbol
             `(thread-tls-ea (load-time-tls-offset ,sym)))
           (symbol-value-slot-ea (sym) ; SYM is a TN
             `(ea (- (* symbol-value-slot n-word-bytes) other-pointer-lowtag)
                  ,sym)))

  (define-vop (%compare-and-swap-symbol-value)
    (:translate %compare-and-swap-symbol-value)
    (:args (symbol :scs (descriptor-reg) :to (:result 0))
           (old :scs (descriptor-reg any-reg) :target rax)
           (new :scs (descriptor-reg any-reg)))
    (:temporary (:sc descriptor-reg :offset rax-offset
                 :from (:argument 1) :to (:result 0)) rax)
    #+sb-thread
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
        #+sb-thread (progn (compute-virtual-symbol)
                            (move rax old)
                            (inst cmpxchg :lock (symbol-value-slot-ea cell) new))
        #-sb-thread (progn (move rax old)
                            ;; is the :LOCK is necessary?
                            (inst cmpxchg :lock (symbol-value-slot-ea symbol) new))
        (inst cmp :byte rax unbound-marker-widetag)
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
      (inst cmpxchg :lock
            (if (sc-is symbol immediate)
                (symbol-slot-ea (tn-value symbol) symbol-value-slot)
                (symbol-value-slot-ea symbol))
            new)
      (move result rax)))

  #+sb-thread
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
        (gen-cell-set (symbol-value-slot-ea cell) value nil)))

    ;; This code is tested by 'codegen.impure.lisp'
    (defun emit-symeval (value symbol symbol-reg check-boundp vop)
      (let* ((known-symbol-p (sc-is symbol constant immediate))
             (known-symbol (and known-symbol-p (tn-value symbol))))
        ;; In order from best to worst.
        (cond
          ((symbol-always-has-tls-value-p known-symbol)
           (setq symbol-reg nil)
           (inst mov value (access-wired-tls-val known-symbol)))
          (t
           (cond
             ((symbol-always-has-tls-index-p known-symbol) ; e.g. CL:*PRINT-BASE*
              ;; Known nonzero TLS index, but possibly no per-thread value.
              ;; The TLS value and global value can be loaded independently.
              (inst mov value (access-wired-tls-val known-symbol))
              (when (sc-is symbol constant)
                (inst mov symbol-reg symbol))) ; = MOV Rxx, [RIP-N]

             (known-symbol-p           ; unknown TLS index, possibly 0
              (sc-case symbol
                (immediate
                 ;; load the TLS index from the symbol. TODO: use [RIP-n] mode
                 ;; for immobile code to make it automatically relocatable.
                 (inst mov :dword value
                       ;; slot index 1/2 is the high half of the header word.
                       (symbol-slot-ea known-symbol 1/2))
                 ;; read the TLS value using that index
                 (inst mov value (thread-tls-ea value)))
                (constant

                 ;; These reads are inextricably data-dependent
                 (inst mov symbol-reg symbol) ; = MOV REG, [RIP-N]
                 (inst mov :dword value (tls-index-of symbol-reg))
                 (inst mov value (thread-tls-ea value)))))

             (t                      ; SYMBOL-VALUE of a random symbol
              (inst mov :dword symbol-reg (tls-index-of symbol))
              (inst mov value (thread-tls-ea symbol-reg))
              (setq symbol-reg symbol)))

           ;; Load the global value if the TLS value didn't exist
           (inst cmp :dword value no-tls-value-marker-widetag)
           (inst cmov :e value
                 (if (and known-symbol-p (sc-is symbol immediate))
                     (symbol-slot-ea known-symbol symbol-value-slot) ; MOV Rxx, imm32
                     (symbol-value-slot-ea symbol-reg)))))

        (when check-boundp
          (assemble ()
            (inst cmp :byte value unbound-marker-widetag)
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
                            (if (or (not symbol-reg) (and immediatep (not staticp)))
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
      (:temporary (:sc unsigned-reg) temp)
      (:generator 9
        (inst mov :dword temp (tls-index-of object))
        (inst mov :dword temp (thread-tls-ea temp))
        (inst cmp :dword temp no-tls-value-marker-widetag)
        (inst cmov :dword :e temp (symbol-value-slot-ea object))
        (inst cmp :byte temp unbound-marker-widetag))))

) ; END OF MACROLET

#-sb-thread
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
      (inst cmp :byte (object-slot-ea
                 symbol symbol-value-slot other-pointer-lowtag)
            unbound-marker-widetag))))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:args-var args)
  (:generator 2
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    ;; The symbol-hash slot of NIL holds NIL because it is also the
    ;; car slot, so we have to zero the fixnum tag bit(s) to make sure
    ;; it is a fixnum.  The lowtag selection magic that is required to
    ;; ensure this is explained in the comment in objdef.lisp
    (unless (not-nil-tn-ref-p args)
      (inst and res (lognot fixnum-tag-mask)))))

;;; Combine SYMBOL-HASH and the lisp fallback code into one vop.
(define-vop ()
  (:policy :fast-safe)
  (:translate ensure-symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:vop-var vop)
  (:generator 5
    (when (location= res symbol) (move temp-reg-tn symbol)) ; save a backup
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst test :dword res res)
    (inst jmp :ne good)
    (inst push (if (location= res symbol) temp-reg-tn symbol))
    (invoke-asm-routine 'call 'ensure-symbol-hash vop)
    (inst pop res)
    GOOD
    (inst and res (lognot fixnum-tag-mask)))) ; redundant (but ok) if asm routine used

(define-vop ()
  (:policy :fast-safe)
  (:translate sb-impl::install-hash-table-lock)
  (:args (arg :scs (descriptor-reg)))
  (:results (res :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 5
    (inst push arg)
    (invoke-asm-routine 'call 'sb-impl::install-hash-table-lock vop)
    (inst pop res)))

(eval-when (:compile-toplevel)
  ;; assumption: any object can be read 1 word past its base pointer
  (assert (= sb-vm:symbol-hash-slot 1)))

(define-vop (symbol-hash*)
  (:policy :fast-safe)
  (:translate symbol-hash*)
  (:args (symbol :scs (descriptor-reg)))
  (:info satisfies)
  (:arg-types * (:constant (member symbolp non-null-symbol-p)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (when (eq satisfies 'symbolp) ; mask to a fixnum
      (inst and res (lognot fixnum-tag-mask)))))
(define-vop (symbol-hash*-random) ; this vop needs a temp; the above doesn't
  (:policy :fast-safe)
  (:translate symbol-hash*)
  (:args (object :scs (descriptor-reg)))
  (:info satisfies)
  ;; arg can not target the temp because they both have to be live
  ;; in order that the tagged pointer not disappear.
  ;; But temp and output could be in the same register.
  (:temporary (:sc unsigned-reg :to (:result 0)) base-ptr)
  (:arg-types * (:constant (eql nil)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:ignore satisfies)
  (:generator 4
    (inst mov base-ptr object)
    (inst and base-ptr (lognot lowtag-mask))
    (inst mov res (ea n-word-bytes base-ptr)) ; 1 word beyond the header
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
    (inst cmp :byte value (logand nil-value #xff))
    (let* ((*location-context* (make-restart-location RETRY value))
           (err-lab (generate-error-code vop 'undefined-fun-error object)))
      (inst jmp :e err-lab))
    RETRY))

#-immobile-code
(define-vop (set-fdefn-fun)
  (:policy :fast-safe)
  (:translate (setf fdefn-fun))
  (:args (function :scs (descriptor-reg) :target result)
         (fdefn :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) raw)
  (:results (result :scs (descriptor-reg)))
  (:generator 38
    (inst mov raw (make-fixup 'closure-tramp :assembly-routine))
    (inst cmp :byte (ea (- fun-pointer-lowtag) function)
          simple-fun-widetag)
    (inst cmov :e raw
          (ea (- (* simple-fun-self-slot n-word-bytes) fun-pointer-lowtag) function))
    (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
    (storew raw fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result function)))
#+immobile-code
(progn
(define-vop (set-fdefn-fun)
  (:args (fdefn :scs (descriptor-reg))
         (function :scs (descriptor-reg))
         (raw-word :scs (unsigned-reg)))
  (:vop-var vop)
  (:generator 38
    (pseudo-atomic ()
      (inst push fdefn)
      (invoke-asm-routine 'call 'touch-gc-card vop)
      (inst mov (ea (- (ash fdefn-fun-slot word-shift) other-pointer-lowtag) fdefn)
            function)
      (inst mov (ea (- (ash fdefn-raw-addr-slot word-shift) other-pointer-lowtag) fdefn)
            raw-word)
      ;; Ensure that the header contains a JMP instruction, not INT3.
      ;; This store is aligned
      (inst mov :word (ea (- 2 other-pointer-lowtag) fdefn) #x25FF))))
(define-vop (set-undefined-fdefn-fun)
  ;; Do not set the raw-addr slot and do not change the header
  (:args (fdefn :scs (descriptor-reg))
         (function :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 38
    (pseudo-atomic ()
      (inst push fdefn)
      (invoke-asm-routine 'call 'touch-gc-card vop)
      (inst mov (ea (- (ash fdefn-fun-slot word-shift) other-pointer-lowtag) fdefn)
            function)))))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg) :target result))
  (:results (result :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 38
    ;; Change the JMP instruction to INT3 so that a trap occurs in the fdefn
    ;; itself, otherwise we've no way of knowing what function name was invoked.
    (inst mov :word (ea (- 2 other-pointer-lowtag) fdefn)
          (logand undefined-fdefn-header #xFFFF))
    ;; Once the opcode is written, the values in 'fun' and 'raw-addr' become irrelevant.
    ;; These stores act primarily to clear the reference from a GC perspective.
    (storew nil-value fdefn fdefn-fun-slot other-pointer-lowtag)
    ;; With #+immobile-code we never call via the raw-addr slot for undefined
    ;; functions if the single instruction "call <fdefn>" form is used. The INT3
    ;; raises sigtrap which we catch, then load RAX with the address of the fdefn
    ;; and resume at undefined-tramp. However, CALL-SYMBOL jumps via raw-addr if
    ;; its callable object was not a function. In that case RAX holds a symbol,
    ;; so we're OK because we can identify the undefined function.
    (storew (make-fixup 'undefined-tramp :assembly-routine)
            fdefn fdefn-raw-addr-slot other-pointer-lowtag)
    (move result fdefn)))

;;;; binding and unbinding

;;; BIND -- Establish VAL as a binding for SYMBOL. Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.

#+sb-thread
(progn
(define-vop (dynbind) ; bind a symbol in a PROGV form
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :offset rax-offset) tls-index)
  (:temporary (:sc unsigned-reg) bsp tmp)
  (:vop-var vop)
  (:generator 10
    (load-binding-stack-pointer bsp)
    (inst mov :dword tls-index (tls-index-of symbol))
    (inst add bsp (* binding-size n-word-bytes))
    (store-binding-stack-pointer bsp)
    (inst test :dword tls-index tls-index)
    (inst jmp :ne TLS-INDEX-VALID)
    (inst mov tls-index symbol)
    (invoke-asm-routine 'call 'alloc-tls-index vop)
    TLS-INDEX-VALID
    (inst mov tmp (thread-tls-ea tls-index))
    (storew tmp bsp (- binding-value-slot binding-size))
    (storew tls-index bsp (- binding-symbol-slot binding-size))
    (inst mov (thread-tls-ea tls-index) val)))

(define-vop (bind) ; bind a known symbol
  (:args (val :scs (any-reg descriptor-reg)
              :load-if (not (let ((imm (encode-value-if-immediate val)))
                              (or (fixup-p imm)
                                  (plausible-signed-imm32-operand-p imm))))))
  (:temporary (:sc unsigned-reg) bsp tmp)
  (:info symbol)
  (:generator 10
    (inst mov bsp (* binding-size n-word-bytes))
    (inst xadd (thread-slot-ea thread-binding-stack-pointer-slot) bsp)
    (let* ((tls-index (load-time-tls-offset symbol))
           (tls-cell (thread-tls-ea tls-index)))
      ;; Too bad we can't use "XCHG [thread + disp], val" to write new value
      ;; and read the old value in one step. It will violate the constraints
      ;; prescribed in the internal documentation on special binding.
      (inst mov tmp tls-cell)
      (storew tmp bsp binding-value-slot)
      ;; Indices are small enough to be written as :DWORDs which avoids
      ;; a REX prefix if 'bsp' happens to be any of the low 8 registers.
      (inst mov :dword (ea (ash binding-symbol-slot word-shift) bsp) tls-index)
      (inst mov :qword tls-cell (encode-value-if-immediate val))))))

#-sb-thread
(define-vop (dynbind)
  (:args (val :scs (any-reg descriptor-reg))
         (symbol :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) temp bsp)
  (:generator 5
    (load-binding-stack-pointer bsp)
    (loadw temp symbol symbol-value-slot other-pointer-lowtag)
    (inst add bsp (* binding-size n-word-bytes))
    (store-binding-stack-pointer bsp)
    (storew temp bsp (- binding-value-slot binding-size))
    (storew symbol bsp (- binding-symbol-slot binding-size))
    (storew val symbol symbol-value-slot other-pointer-lowtag)))

#+sb-thread
(define-vop (unbind-n)
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
          (inst movapd (ea bsp) zero))
    (store-binding-stack-pointer bsp)))

#-sb-thread
(define-vop (unbind)
  (:temporary (:sc unsigned-reg) symbol value bsp)
  (:generator 0
    (load-binding-stack-pointer bsp)
    (loadw symbol bsp (- binding-symbol-slot binding-size))
    (loadw value bsp (- binding-value-slot binding-size))
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    (storew 0 bsp (- binding-symbol-slot binding-size))
    (storew 0 bsp (- binding-value-slot binding-size))
    (inst sub bsp (* binding-size n-word-bytes))
    (store-binding-stack-pointer bsp)))

(defun unbind-to-here (where symbol value bsp
                       zero)
  (assemble ()
    (load-binding-stack-pointer bsp)
    (inst cmp where bsp)
    (inst jmp :e DONE)
    (inst xorpd zero zero)
    LOOP
    (inst sub bsp (* binding-size n-word-bytes))
    ;; on sb-thread symbol is actually a tls-index, and it fits into
    ;; 32-bits.
    #+sb-thread
    (progn
      (inst mov :dword symbol (ea (* binding-symbol-slot n-word-bytes) bsp))
      (inst test :dword symbol symbol))
    #-sb-thread
    (progn
      (loadw symbol bsp binding-symbol-slot)
      (inst test symbol symbol))
    (inst jmp :z SKIP)
    (loadw value bsp binding-value-slot)
    #-sb-thread
    (storew value symbol symbol-value-slot other-pointer-lowtag)
    #+sb-thread
    (inst mov (thread-tls-ea symbol) value)

    SKIP
    (inst movapd (ea bsp) zero)

    (inst cmp where bsp)
    (inst jmp :ne LOOP)
    (store-binding-stack-pointer bsp)

    DONE))

(define-vop (unbind-to-here)
  (:args (where :scs (descriptor-reg any-reg)))
  (:temporary (:sc unsigned-reg) symbol value bsp)
  (:temporary (:sc complex-double-reg) zero)
  (:generator 0
    (unbind-to-here where symbol value bsp zero)))

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

(define-vop ()
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 4
    (inst mov :dword res (ea (- instance-pointer-lowtag) struct))
    ;; Returning fixnum/any-reg elides some REX prefixes due to the shifts
    ;; being small. Maybe the asm optimizer could figure it out now?
    (inst shr :dword res (- instance-length-shift n-fixnum-tag-bits))
    (inst and :dword res (fixnumize instance-length-mask))))

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

;; CODE-HEADER-SET of a constant index is seldom used, so we specifically
;; ask not to get it defined. (It's not impossible to use, it's just not
;; helpful to have - either you want to access CODE-FIXUPS or CODE-DEBUG-INFO,
;; which do things their own way, or you want a variable index.)
(define-full-setter (code-header-set :no-constant-variant)
  * 0 other-pointer-lowtag
  (any-reg descriptor-reg) * code-header-set)

;;;; raw instance slot accessors

(flet ((instance-slot-ea (object index)
         (etypecase index
           (integer
              (ea (+ (* (+ instance-slots-offset index) n-word-bytes)
                     (- instance-pointer-lowtag))
                  object))
           (tn
              (ea (+ (* instance-slots-offset n-word-bytes)
                     (- instance-pointer-lowtag))
                  object index (ash 1 (- word-shift n-fixnum-tag-bits)))))))
  (macrolet
      ((def (suffix result-sc result-type inst &optional (inst/c (list 'quote inst)))
         `(progn
            (define-vop (,(symbolicate "RAW-INSTANCE-REF/" suffix))
              (:translate ,(symbolicate "%RAW-INSTANCE-REF/" suffix))
              (:policy :fast-safe)
              (:args (object :scs (descriptor-reg)) (index :scs (any-reg)))
              (:arg-types * tagged-num)
              (:results (value :scs (,result-sc)))
              (:result-types ,result-type)
              (:generator 5
                (inst ,inst value (instance-slot-ea object index))))
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
                (inst* ,inst/c value (instance-slot-ea object index))))
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
                (inst ,inst (instance-slot-ea object index) value)
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
                (inst* ,inst/c (instance-slot-ea object index) value)
                (move result value)))
            (define-vop (,(symbolicate "RAW-INSTANCE-INIT/" suffix))
              (:args (object :scs (descriptor-reg))
                     (value :scs (,result-sc)))
              (:arg-types * ,result-type)
              (:info index)
              (:generator 4
                (inst* ,inst/c (instance-slot-ea object index) value))))))
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
      (inst xadd :lock (instance-slot-ea object index) diff)
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
      (inst xadd :lock (instance-slot-ea object index) diff)
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
  (inst cmpxchg16b :lock memory-operand)
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
          (:temporary (:sc unsigned-reg :offset rax-offset
                       :from (:argument 2) :to (:result 0)) eax)
          (:temporary (:sc unsigned-reg :offset rdx-offset
                       :from (:argument 3) :to (:result 0)) edx)
          (:temporary (:sc unsigned-reg :offset rbx-offset
                       :from (:argument 4) :to (:result 0)) ebx)
          (:temporary (:sc unsigned-reg :offset rcx-offset
                       :from (:argument 5) :to (:result 0)) ecx)
          (:generator 7
           (generate-dblcas ,memory-operand
                            expected-old-lo expected-old-hi new-lo new-hi
                            eax ebx ecx edx result-lo result-hi)))))
  (define-cmpxchg-vop compare-and-exchange-pair
      (ea (- list-pointer-lowtag) object)
      ((:translate %cons-cas-pair)))
  (define-cmpxchg-vop compare-and-exchange-pair-indexed
      (ea offset object index (ash n-word-bytes (- n-fixnum-tag-bits)))
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
