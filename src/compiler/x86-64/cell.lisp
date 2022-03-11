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

(defconstant vector-len-op-size #+ubsan :dword #-ubsan :qword)
(defmacro vector-len-ea (v &optional (lowtag sb-vm:other-pointer-lowtag))
  #+ubsan `(ea (- 4 ,lowtag) ,v) ; high 4 bytes of header
  #-ubsan `(ea (- (ash vector-length-slot word-shift) ,lowtag) ,v))

(define-vop (slot)
  (:args (object :scs (descriptor-reg)))
  (:info name offset lowtag)
  #-ubsan (:ignore name)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
   (cond #+ubsan
         ((member name '(sb-c::vector-length %array-fill-pointer)) ; half-sized slot
          (inst mov :dword result (vector-len-ea object)))
         (t
          (loadw result object offset lowtag)))))

;; This vop is selected by name from vm-ir2tran for converting any
;; setter or setf'er that is defined by 'objdef'
(define-vop (set-slot)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg immediate)))
  (:info name offset lowtag)
  (:results)
  (:vop-var vop)
  (:temporary (:sc unsigned-reg) val-temp)
  (:generator 1
    (cond #+ubsan
          ((and (eql offset sb-vm:array-fill-pointer-slot) ; half-sized slot
                (or (eq name 'make-array)
                    (equal name '(setf %array-fill-pointer))))
           (when (eq name 'make-array) ; nullify the creating PC location
             (inst mov :qword (object-slot-ea object 1 lowtag) nil-value))
           (inst mov :dword (vector-len-ea object)
                 (or (encode-value-if-immediate value) value)))
          (t
           ;; gencgc does not need to emit the barrier for constructors
           (unless (member name '(%make-structure-instance make-weak-pointer
                                  %make-ratio %make-complex))
             (emit-gc-store-barrier object nil val-temp (vop-nth-arg 1 vop) value))
           (gen-cell-set (object-slot-ea object offset lowtag) value val-temp)))))

(define-vop (compare-and-swap-slot)
  (:args (object :scs (descriptor-reg) :to :eval)
         (old :scs (descriptor-reg any-reg) #|:target rax|#)
         (new :scs (descriptor-reg any-reg)))
  ;; if OLD were LOCATION= to RAX then we'd clobber OLD
  ;; while computing the EA for the barrier.
  (:temporary (:sc descriptor-reg :offset rax-offset
                   #|:from (:argument 1)|# :to :result :target result)
              rax)
  (:info name offset lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:generator 5
     (emit-gc-store-barrier object nil rax (vop-nth-arg 2 vop) new)
     (move rax old)
     (inst cmpxchg :lock (ea (- (* offset n-word-bytes) lowtag) object) new)
     (move result rax)))

;;;; symbol hacking VOPs

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst mov result (unbound-marker-bits))))

(define-vop (%set-symbol-global-value)
  (:args (object :scs (descriptor-reg immediate))
         (value :scs (descriptor-reg any-reg immediate)))
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg) val-temp)
  (:vop-var vop)
  (:generator 4
    (emit-gc-store-barrier object nil val-temp (vop-nth-arg 1 vop) value)
    (gen-cell-set (if (sc-is object immediate)
                      (symbol-slot-ea (tn-value object) symbol-value-slot)
                      (object-slot-ea object symbol-value-slot other-pointer-lowtag))
                  value val-temp)))

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

;; Return the DISP field to use in an EA relative to thread-base
(defun load-time-tls-offset (symbol)
  (let ((where (info :variable :wired-tls symbol)))
    (cond ((integerp where) where)
          (t (make-fixup symbol :symbol-tls-index)))))

(deftransform %compare-and-swap-symbol-value ((symbol old new)
                                              ((constant-arg symbol) t t))
  (if (eq (info :variable :kind (sb-c:lvar-value symbol)) :global)
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
                #+gs-seg (inst rdgsbase thread-temp)
                (inst lea cell
                      (ea (- other-pointer-lowtag (ash symbol-value-slot word-shift))
                          #+gs-seg thread-temp
                          #-gs-seg thread-tn
                          cell))
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
    (:temporary (:sc descriptor-reg :to (:result 0)) cell)
    #+gs-seg (:temporary (:sc unsigned-reg) thread-temp)
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
      ;; Possible optimization: don't frob the card mark when storing into TLS
      (emit-gc-store-barrier symbol nil cell (vop-nth-arg 2 vop) new)
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
           (old :scs (descriptor-reg any-reg) #|:target rax|#)
           (new :scs (descriptor-reg any-reg)))
    ;; if OLD were LOCATION= to RAX then we'd clobber OLD
    ;; while computing the EA for the barrier.
    (:temporary (:sc descriptor-reg :offset rax-offset
                 #|:from (:argument 1)|# :to (:result 0)) rax)
    (:results (result :scs (descriptor-reg any-reg)))
    (:policy :fast-safe)
    (:vop-var vop)
    (:generator 10
      (emit-gc-store-barrier symbol nil rax (vop-nth-arg 2 vop) new)
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
    ;; Note that the 32-bit x86 code prefers to use branching code here, where it accesses
    ;; either the symbol's slot or the segment-relative absolute displacement to the TLS.
    ;; This prefers CMOV, which means we always need the thread's address in a GPR.
    (define-vop (set)
      (:args (symbol :scs (descriptor-reg))
             (value :scs (descriptor-reg any-reg immediate)))
      (:temporary (:sc descriptor-reg) cell)
      (:temporary (:sc unsigned-reg) val-temp)
      #+gs-seg (:temporary (:sc unsigned-reg) thread-temp)
      (:vop-var vop)
      (:generator 4
        ;; Possible optimization: don't frob the card mark when storing into TLS
        (emit-gc-store-barrier symbol nil val-temp (vop-nth-arg 1 vop) value)
        ;; Compute the address into which to store. CMOV can only move into
        ;; a register, so we can't conditionally move into the TLS and
        ;; conditionally move in the opposite flag sense to the symbol.
        (compute-virtual-symbol)
        (gen-cell-set (symbol-value-slot-ea cell) value val-temp)))

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

    ;; TODO: this vop doesn't see that when (INFO :VARIABLE :KIND) = :GLOBAL
    ;; there is no need to check the TLS. Probably this is better handled in IR1
    ;; rather than IR2. It would need a new GLOBAL-BOUNDP function.
    ;; On the other hand, how many users know that you can declaim
    ;; a variable GLOBAL without using DEFGLOBAL ?
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
  (:results (res :from :load :scs (any-reg))) ; force it to conflict with arg 0
  (:result-types positive-fixnum)
  (:vop-var vop)
  (:generator 5
    (aver (not (location= res symbol)))
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst test :dword res res)
    (inst jmp :ne good)
    (inst push symbol)
    (invoke-asm-routine 'call 'ensure-symbol-hash vop)
    (inst pop res)
    GOOD
    (inst and res (lognot fixnum-tag-mask)))) ; redundant (but ok) if asm routine used

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

(define-vop ()
  (:args (symbol :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:translate sb-impl::symbol-package-id)
  (:policy :fast-safe)
  (:generator 2 ; ASSUMPTION: symbol-package-bits = 16
   (inst mov :word result (object-slot-ea symbol symbol-name-slot (- other-pointer-lowtag 6)))
   (inst movzx '(:word :dword) result result)))
(define-vop ()
  (:args (symbol :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg) :from :load))
  (:translate symbol-name)
  (:policy :fast-safe)
  (:generator 2
   (inst mov result (1- (ash 1 sb-impl::symbol-name-bits)))
   (inst and result (object-slot-ea symbol symbol-name-slot other-pointer-lowtag))))

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
    (emit-gc-store-barrier fdefn nil raw)
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
    ;; N.B. concerning the use of pseudo-atomic here,
    ;;      refer to doc/internals-notes/fdefn-gc-safety
    ;; No barrier here, because fdefns in immobile space rely on the SIGSEGV signal
    ;; to manage the card marks.
    (pseudo-atomic ()
      (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
      (storew raw-word fdefn fdefn-raw-addr-slot other-pointer-lowtag)
      ;; Ensure that the header contains a JMP instruction, not INT3.
      ;; This store is aligned
      (inst mov :word (ea (- 2 other-pointer-lowtag) fdefn) #x25FF))))
(define-vop (set-undefined-fdefn-fun)
  ;; Do not set the raw-addr slot and do not change the header
  ;; This vop is specifically for SB-C::INSTALL-GUARD-FUNCTION
  (:args (fdefn :scs (descriptor-reg))
         (function :scs (descriptor-reg)))
  (:vop-var vop)
  (:generator 1 (storew function fdefn fdefn-fun-slot other-pointer-lowtag))))

(define-vop (fdefn-makunbound)
  (:policy :fast-safe)
  (:translate fdefn-makunbound)
  (:args (fdefn :scs (descriptor-reg)))
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
            fdefn fdefn-raw-addr-slot other-pointer-lowtag)))

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
    (emit-gc-store-barrier symbol nil temp)
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
    (emit-gc-store-barrier symbol nil value) ; VALUE is the card-mark temp
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
    #-sb-thread (progn (emit-gc-store-barrier symbol nil value) ; VALUE is the card-mark temp
                       (loadw value bsp binding-value-slot)
                       (storew value symbol symbol-value-slot other-pointer-lowtag))
    #+sb-thread (progn (loadw value bsp binding-value-slot)
                       (inst mov (thread-tls-ea symbol) value))
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

(define-full-setter %closure-index-set * closure-info-offset fun-pointer-lowtag
  (any-reg descriptor-reg) * %closure-index-set)

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
    ;; TODO: gencgc does not need EMIT-GC-STORE-BARRIER here, but other other GC strategies might.
    (storew value object (+ closure-info-offset offset) fun-pointer-lowtag)))

(define-vop (closure-init-from-fp)
  (:args (object :scs (descriptor-reg)))
  (:info offset)
  (:generator 4
    ;; TODO: gencgc does not need EMIT-GC-STORE-BARRIER here, but other other GC strategies might.
    (storew rbp-tn object (+ closure-info-offset offset) fun-pointer-lowtag)))

;;;; value cell hackery

(define-vop (value-cell-ref cell-ref)
  (:variant value-cell-value-slot other-pointer-lowtag))

(define-vop (value-cell-set cell-set)
  (:variant value-cell-value-slot other-pointer-lowtag))

;;;; structure hackery

(defun load-instance-length (result instance taggedp)
  (inst mov :dword result (ea (- instance-pointer-lowtag) instance))
  ;; Returning fixnum/any-reg elides some REX prefixes due to the shifts
  ;; being small. Maybe the asm optimizer could figure it out now?
  (cond (taggedp
         (inst shr :dword result (- instance-length-shift n-fixnum-tag-bits))
         (inst and :dword result (fixnumize instance-length-mask)))
        (t
         (inst shr :dword result instance-length-shift)
         (inst and :dword result instance-length-mask))))

(define-vop ()
  (:policy :fast-safe)
  (:translate %instance-length)
  (:args (struct :scs (descriptor-reg)))
  (:results (res :scs (any-reg)))
  (:result-types positive-fixnum)
  (:generator 4 (load-instance-length res struct t)))

(define-full-reffer instance-index-ref * instance-slots-offset
  instance-pointer-lowtag (any-reg descriptor-reg) * %instance-ref)

(define-full-setter instance-index-set * instance-slots-offset
  instance-pointer-lowtag (any-reg descriptor-reg immediate constant) * %instance-set)

;;; Try to group consecutive %INSTANCE-SET vops on the same instance
;;; so that:
;;; 1) we can potentially utilize multi-word stores,
;;; 2) a GC store barrier need occur once only (depending on the kind of barrier)
;;;
;;; in the absence of barriers, we would like to be allowed to rearrange
;;; stores; in particular, storing the constant 0 to clear out a structure
;;; should not require that you remember the slot order.
;;; all the more so if we are permitted to optimize the slot order of the defstruct
;;; by putting all tagged slots together, then all raw slots together.
;;;
(define-vop (instance-set-multiple)
  (:args (instance :scs (descriptor-reg))
         (values :more t :scs (descriptor-reg constant immediate)))
  (:temporary (:sc unsigned-reg) val-temp)
  ;; Would like to try to store adjacent 0s (and/or NILs) using 16 byte stores.
  (:temporary (:sc int-sse-reg) xmm-temp)
  (:info indices)
  (:generator 1
    (let* ((max-index (reduce #'max indices))
           ;;(min-index (reduce #'min indices))
           ;;(count (length indices))
           (zerop-mask 0) ; slots which become a zero
           (constantp-mask 0) ; slots which become any constant
           (const-vals (make-array (1+ max-index) :initial-element nil))
           (use-xmm-p))
      (do ((tn-ref values (tn-ref-across tn-ref))
           (indices indices (cdr indices)))
          ((null tn-ref))
        (let ((tn (tn-ref-tn tn-ref)))
          (when (constant-tn-p tn)
            (let ((slot (car indices))
                  (val (tn-value tn)))
              (setf constantp-mask (logior constantp-mask (ash 1 slot))
                    zerop-mask (logior zerop-mask (if (eql val 0) (ash 1 slot) 0))
                    (aref const-vals slot) val)))))
      ;; If there are at least 3 zeros stored or any pair of adjacent 0s
      ;; then load the xmm-temp with 0.
      (setq use-xmm-p (or (>= (logcount zerop-mask) 3)
                          (loop for slot below max-index
                             thereis (= (ldb (byte 2 slot) zerop-mask) #b11))))
      (emit-gc-store-barrier instance nil val-temp values)
      (when use-xmm-p
        (inst xorpd xmm-temp xmm-temp))
      (loop
       (let* ((slot (pop indices))
              (val (tn-ref-tn values))
              (ea (ea (- (ash (+ instance-slots-offset slot) word-shift)
                         instance-pointer-lowtag)
                      instance)))
         (aver (tn-p instance))
         (setq values (tn-ref-across values))
         ;; If the xmm temp was loaded with 0 and this value is 0,
         ;; and possibly the next, then store through the temp
         (cond
           ((and use-xmm-p (constant-tn-p val) (eql (tn-value val) 0))
            (let* ((next-slot (car indices))
                   (next-val (if next-slot (tn-ref-tn values))))
              (cond ((and (eql (1+ slot) next-slot)
                          (constant-tn-p next-val)
                          (eql (tn-value next-val) 0))
                     (inst movupd ea xmm-temp)
                     (pop indices)
                     (setq values (tn-ref-across values)))
                    (t
                     (inst movsd ea xmm-temp)))))
           ((stack-tn-p val)
            (inst mov val-temp val)
            (inst mov ea val-temp))
           (t
            (gen-cell-set ea val val-temp)))
         (unless indices (return)))))
    (aver (not values))))

(define-full-compare-and-swap %instance-cas instance
  instance-slots-offset instance-pointer-lowtag
  (any-reg descriptor-reg) * %instance-cas)
(define-full-compare-and-swap %raw-instance-cas/word instance
  instance-slots-offset instance-pointer-lowtag
  (unsigned-reg) unsigned-num %raw-instance-cas/word)
(define-vop ()
  (:translate %raw-instance-xchg/word)
  (:policy :fast-safe)
  (:args (instance :scs (descriptor-reg))
         (newval :scs (unsigned-reg immediate constant) :target result))
  (:info index)
  (:arg-types * (:constant integer) unsigned-num)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 3
    ;; Use RESULT as the source of the exchange, unless doing so
    ;; would clobber NEWVAL
    (let ((source (if (location= result instance) temp result)))
      (if (sc-is newval immediate)
          (inst mov source (constantize (tn-value newval)))
          (move source newval))
      (inst xchg (ea (- (ash (+ instance-slots-offset index) word-shift)
                        instance-pointer-lowtag) instance)
            source)
      (unless (eq source result)
        (move result temp)))))

;;;; code object frobbing

(define-full-reffer code-header-ref * 0 other-pointer-lowtag
  (any-reg descriptor-reg) * code-header-ref)

(define-vop ()
  (:translate code-header-set)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg))
         (value :scs (any-reg descriptor-reg)))
  (:arg-types * unsigned-num *)
  (:vop-var vop)
  (:temporary (:sc unsigned-reg :offset rax-offset) rax) ; for the asm routine
  (:temporary (:sc unsigned-reg :offset rdx-offset) rdx)
  (:temporary (:sc unsigned-reg :offset rdi-offset) rdi)
  (:ignore rax rdx rdi)
  (:generator 10
    (inst push value)
    (inst push index)
    (inst push object)
    (invoke-asm-routine 'call 'code-header-set vop)))

;;;; raw instance slot accessors

(flet ((instance-slot-ea (object index)
         (let ((constant-index
                (if (integerp index) index
                    (if (sc-is index immediate) (tn-value index)))))
           (if constant-index
               (ea (+ (* (+ instance-slots-offset constant-index) n-word-bytes)
                      (- instance-pointer-lowtag))
                   object)
               (ea (+ (* instance-slots-offset n-word-bytes)
                      (- instance-pointer-lowtag))
                   object index (ash 1 (- word-shift n-fixnum-tag-bits)))))))
  (macrolet
      ((def (suffix result-sc result-type inst)
         `(progn
            (define-vop ()
              (:translate ,(symbolicate "%RAW-INSTANCE-REF/" suffix))
              (:policy :fast-safe)
              (:args (object :scs (descriptor-reg)) (index :scs (any-reg immediate)))
              (:arg-types * tagged-num)
              (:results (value :scs (,result-sc)))
              (:result-types ,result-type)
              (:generator 1 (inst ,inst value (instance-slot-ea object index))))
            (define-vop ()
              (:translate ,(symbolicate "%RAW-INSTANCE-SET/" suffix))
              (:policy :fast-safe)
              (:args (object :scs (descriptor-reg))
                     (index :scs (any-reg immediate))
                     (value :scs (,result-sc)))
              (:arg-types * tagged-num ,result-type)
              (:generator 1 (inst ,inst (instance-slot-ea object index) value))))))
    (def word unsigned-reg unsigned-num mov)
    (def signed-word signed-reg signed-num mov)
    (def single single-reg single-float movss)
    (def double double-reg double-float movsd)
    (def complex-single complex-single-reg complex-single-float movq)
    (def complex-double complex-double-reg complex-double-float
      ;; Todo: put back the choice of using APD or UPD.
      ;; But was it even right for either +/- compact-instance-header?
         movupd #| (if (oddp index) 'movapd 'movupd) |#))

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

;;; TODO: these GC-STORE-BARRIERs are inadequate if the GC strategy
;;; requires that 2 old pointees and 2 new pointees all be greyed.
(macrolet
    ((define-dblcas (translate indexedp &rest rest)
       `(define-vop ()
          (:policy :fast-safe)
          (:translate ,translate)
          (:args (object :scs (descriptor-reg) :to :eval)
                 ,@(when indexedp '((index :scs (any-reg) :to :eval)))
                 (expected-old-lo :scs (descriptor-reg any-reg) :target eax)
                 (expected-old-hi :scs (descriptor-reg any-reg) :target edx)
                 (new-lo :scs (descriptor-reg any-reg) :target ebx)
                 (new-hi :scs (descriptor-reg any-reg) :target ecx))
          ,@(when indexedp '((:arg-types * positive-fixnum * * * *)))
          ,@rest
          (:results (result-lo :scs (descriptor-reg any-reg))
                    (result-hi :scs (descriptor-reg any-reg)))
          ;; this is sufficiently confusing that I don't want to try reusing
          ;; one of the other declared temps as the EA for the store barrier.
          (:temporary (:sc unsigned-reg) temp)
          (:temporary (:sc unsigned-reg :offset rax-offset
                       :from (:argument 2) :to (:result 0)) eax)
          (:temporary (:sc unsigned-reg :offset rdx-offset
                       :from (:argument 3) :to (:result 0)) edx)
          (:temporary (:sc unsigned-reg :offset rbx-offset
                       :from (:argument 4) :to (:result 0)) ebx)
          (:temporary (:sc unsigned-reg :offset rcx-offset
                       :from (:argument 5) :to (:result 0)) ecx))))

  (define-dblcas %cons-cas-pair nil
    (:generator 2
      (emit-gc-store-barrier object nil temp)
      (generate-dblcas (ea (- list-pointer-lowtag) object)
                       expected-old-lo expected-old-hi new-lo new-hi
                       eax ebx ecx edx result-lo result-hi)))

  ;; The CPU requires 16-byte alignment for the memory operand.
  ;; A vector's data portion starts on a 16-byte boundary, so any even numbered index is OK.
  (define-dblcas %vector-cas-pair t
    (:generator 2
      (let ((ea (ea (- (* n-word-bytes vector-data-offset) other-pointer-lowtag)
                    object index (ash n-word-bytes (- n-fixnum-tag-bits)))))
        (emit-gc-store-barrier object ea temp)
        (generate-dblcas ea expected-old-lo expected-old-hi new-lo new-hi
                         eax ebx ecx edx result-lo result-hi))))

  ;; Here you have to specify an odd numbered slot.
  ;; An instance's first user-visible slot at index 1 is 16-byte-aligned.
  ;; (Hmm, does the constraint differ by +/- compact-instance-header?)
  (define-dblcas %instance-cas-pair t
    (:generator 2
      (emit-gc-store-barrier object nil temp)
      (let ((ea (ea (- (* n-word-bytes instance-slots-offset) instance-pointer-lowtag)
                    object index (ash n-word-bytes (- n-fixnum-tag-bits)))))
        (generate-dblcas ea expected-old-lo expected-old-hi new-lo new-hi
                         eax ebx ecx edx result-lo result-hi)))))
