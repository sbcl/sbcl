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
          #-soft-card-marks
          ((or (equal name '(setf %funcallable-instance-fun)) (eq name '%set-fun-layout))
           ;; If soft card marks are disabled, then EMIT-GENGC-BARRIER is disabled too.
           ;; But funcallable-instances are on PAGE_TYPE_CODE, and code pages do not use
           ;; MMU-based protection regardless of this feature.
           ;; So we have to alter the card mark differently.
           (pseudo-atomic ()
             (emit-code-page-gengc-barrier object val-temp)
             (emit-store (object-slot-ea object offset lowtag) value val-temp)))
          (t
           (emit-gengc-barrier object nil val-temp (vop-nth-arg 1 vop) value name)
           (emit-store (object-slot-ea object offset lowtag) value val-temp)))))

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
     (emit-gengc-barrier object nil rax (vop-nth-arg 2 vop) new)
     (move rax old)
     (inst cmpxchg :lock (ea (- (* offset n-word-bytes) lowtag) object) new)
     (move result rax)))

;;;; symbol hacking VOPs

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst mov result (unbound-marker-bits))))

(defmacro emit-symbol-write-barrier (sym &rest rest)
  ;; IMMEDIATE sc means that the symbol is static or immobile.
  ;; Static symbols are roots, and immobile symbols use page fault handling.
  `(unless (sc-is ,sym immediate) (emit-gengc-barrier ,sym ,@rest)))

(define-vop (%set-symbol-global-value)
  (:args (symbol :scs (descriptor-reg immediate))
         (value :scs (descriptor-reg any-reg immediate)))
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg) val-temp)
  (:vop-var vop)
  (:generator 4
    (emit-symbol-write-barrier symbol nil val-temp (vop-nth-arg 1 vop) value)
    (emit-store (if (sc-is symbol immediate)
                      (symbol-slot-ea (tn-value symbol) symbol-value-slot)
                      (object-slot-ea symbol symbol-value-slot other-pointer-lowtag))
                  value val-temp)))

;;; This does not resolve the TLS-INDEX at load-time, because we don't want to
;;; waste TLS indices for symbols that may never get thread-locally bound.
;;; So instead we make a fixup to the address of the 4-byte TLS field in the symbol.
(defun symbol-tls-index-ea (symbol)
  (ea (make-fixup (tn-value symbol) :immobile-symbol (- 4 other-pointer-lowtag))))

(defun get-symbol-value-slot-ea (ea-tn symbol)
  (if (sc-is symbol immediate)
      (inst mov ea-tn (make-fixup (tn-value symbol) :immobile-symbol
                                  (- (ash symbol-value-slot word-shift) other-pointer-lowtag)))
      (inst lea ea-tn (object-slot-ea symbol symbol-value-slot other-pointer-lowtag))))

;;; As implemented for x86-64, SET performs at most one conditional branch and no
;;; unconditional jump, contrasting with the other common approach of branching around
;;; a store to either the TLS or the global value, followed by an unconditional jump
;;; out, and in the middle doing the opposite of whichever store was jumped over.
;;; (see the arm64, ppc64, and riscv implementations)
;;; I would surmise that one unconditional branch is preferable.
;;; Slightly in favor of that claim is that if you run this through a C compiler:
;;;   void f1(long val, int test, long *a, long *b) {
;;;       if (test) *a = val; else *b = val;
;;;   }
;;; it generates the same code as if you had written "*(test?a:b) = val;"
;;; So while that's not 100% analagous to this situation here, it does imply
;;; slight favoritism for branch-free code. It might be possible to use CMOV
;;; if we don't need to emit the GC barrier such as when storing a fixnum.
;;; On the other hand, Torvalds has ranted against writing branchless code
;;; for its own sake (https://yarchive.net/comp/linux/cmov.html)
;;;
#+sb-thread
(define-vop (set)
  (:args (symbol :scs (descriptor-reg immediate)
                 ;; Don't need a register load from the constant pool
                 ;; if we're only going to reference TLS
                 :load-if
                 (not (typep (info :variable :wired-tls (known-symbol-use-p vop symbol))
                             '(or (eql :always-thread-local) integer))))
         (value :scs (descriptor-reg any-reg immediate)))
  (:temporary (:sc unsigned-reg) val-temp cell)
  #+gs-seg (:temporary (:sc unsigned-reg) thread-temp)
  (:vop-var vop)
  (:generator 4
    (let ((known-sym (known-symbol-use-p vop symbol)))
      (when (typep (info :variable :wired-tls known-sym)
                   '(or (eql :always-thread-local) integer))
        ;; Never emit a GC barrier for TLS
        (return-from set
          (emit-store (ea (make-fixup known-sym :symbol-tls-index) thread-tn)
                      value val-temp)))
      (cond ((info :variable :wired-tls known-sym)
             ;; The TLS index is arbitrary but known to be nonzero,
             ;; so we can resolve the displacement of the thread-local value
             ;; at load-time, saving one instruction over the general case.
             (inst lea cell (ea (make-fixup known-sym :symbol-tls-index) thread-tn)))
            (t
             ;; These MOVs look the same, but when the symbol is immediate, this is
             ;; a load from an absolute address. Needless to say, the names of these
             ;; accessor macros are arbitrary - the difference is not very apparent.
             (inst mov :dword cell (if (sc-is symbol immediate)
                                       (symbol-tls-index-ea symbol)
                                       (tls-index-of symbol)))
             (inst add cell thread-tn))))
    (inst cmp :qword (ea cell) no-tls-value-marker)
    (inst jmp :ne STORE)
    (emit-symbol-write-barrier symbol nil val-temp (vop-nth-arg 1 vop) value)
    (get-symbol-value-slot-ea cell symbol)
    STORE
    (emit-store (ea cell) value val-temp)))

(define-vop (fast-symbol-global-value)
  (:args (object :scs (descriptor-reg immediate)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:policy :fast)
  (:translate symbol-global-value)
  (:generator 4
    (cond ((sc-is object immediate)
           (inst mov value (symbol-slot-ea (tn-value object) symbol-value-slot)))
          (t
           (loadw value object symbol-value-slot other-pointer-lowtag)))))

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

(defmacro symbol-value-slot-ea (sym)    ; SYM is a TN
  `(ea (- (* symbol-value-slot n-word-bytes) other-pointer-lowtag)
       ,sym))

(macrolet ((access-wired-tls-val (sym) ; SYM is a symbol
             `(thread-tls-ea (load-time-tls-offset ,sym)))
           (load-oldval ()
             `(if (sc-is old immediate)
                  (inst mov rax (encode-value-if-immediate old))
                  (move rax old))))

(define-vop (%cas-symbol-global-value)
    (:translate %cas-symbol-global-value)
    (:args (symbol :scs (descriptor-reg immediate) :to (:result 0))
           (old :scs (descriptor-reg any-reg constant immediate))
           (new :scs (descriptor-reg any-reg)))
    ;; RAX is the temp for computing the card mark, so it has to conflict
    ;; with OLD and therefore the default lifetime spec is fine
    (:temporary (:sc descriptor-reg :offset rax-offset :to (:result 0)) rax)
    (:results (result :scs (descriptor-reg any-reg)))
    (:policy :fast-safe)
    (:vop-var vop)
    (:generator 10
      (emit-symbol-write-barrier symbol nil rax (vop-nth-arg 2 vop) new)
      (load-oldval)
      (inst cmpxchg :lock (if (sc-is symbol immediate)
                              (symbol-slot-ea (tn-value symbol) symbol-value-slot)
                              (symbol-value-slot-ea symbol))
            new)
      (move result rax)))

(define-vop (%compare-and-swap-symbol-value)
    (:translate %compare-and-swap-symbol-value)
    (:args (symbol :scs (descriptor-reg immediate) :to (:result 0))
           (old :scs (descriptor-reg any-reg constant immediate) :target rax)
           (new :scs (descriptor-reg any-reg)))
    (:temporary (:sc descriptor-reg :offset rax-offset
                 :from (:argument 1) :to (:result 0)) rax)
    (:temporary (:sc descriptor-reg :to (:result 0)) cell)
    #+gs-seg (:temporary (:sc unsigned-reg) thread-temp)
    (:results (result :scs (descriptor-reg any-reg)))
    (:policy :fast-safe)
    (:vop-var vop)
    (:node-var node)
    (:generator 15
    ;; This code has two pathological cases: NO-TLS-VALUE-MARKER
    ;; or UNBOUND-MARKER as NEW: in either case we would end up
    ;; doing possible damage with CMPXCHG -- so don't do that!
    ;; Even worse: don't supply old=NO-TLS-VALUE with a symbol whose
    ;; tls-index=0, because that would succeed, assigning NEW to each
    ;; symbol in existence having otherwise no thread-local value.
      #+sb-thread (progn
      (inst mov :dword cell (if (sc-is symbol immediate)
                                (symbol-tls-index-ea symbol)
                                (tls-index-of symbol)))
      (inst add cell thread-tn)
      (inst cmp :qword (ea cell) no-tls-value-marker)
      (inst jmp :ne CAS))
      ;; GLOBAL. All logic that follows is for both + and - sb-thread
      (emit-symbol-write-barrier symbol nil cell (vop-nth-arg 2 vop) new)
      (get-symbol-value-slot-ea cell symbol)
      CAS
      (load-oldval)
      (inst cmpxchg :lock (ea cell) new)
      ;; FIXME: if :ALWAYS-BOUND then elide the BOUNDP check.
      ;; But we don't accept a constant or immediate, so how to know
      ;; what symbol is being CASed? I kind of feel like whether to perform
      ;; the check ought to be supplied as codegen info so that we don't
      ;; conflate storage-class of an arg with whether we could elide the check.
      (unless (policy node (= safety 0))
        (inst cmp :byte rax unbound-marker-widetag)
        (inst jmp :e (generate-error-code vop 'unbound-symbol-error symbol)))
      (move result rax)))

  #+sb-thread
  (progn
    ;; This code is tested by 'codegen.impure.lisp'
    (defun emit-symeval (value symbol symbol-ref symbol-reg check-boundp vop)
      (let* ((known-symbol-p (sc-is symbol constant immediate))
             (known-symbol (and known-symbol-p (tn-value symbol))))
        ;; In order from best to worst.
        (cond
          ((symbol-always-has-tls-value-p symbol-ref (sb-c::vop-node vop))
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
           (inst cmp :qword value no-tls-value-marker)
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

  ;; With Symbol-Value, we check that the value isn't the trap object.
    (define-vop (symbol-value)
      (:translate symbol-value)
      (:policy :fast-safe)
      (:args (symbol :scs (descriptor-reg constant immediate) :to (:result 1)))
      (:arg-refs symbol-ref)
      ;; TODO: use no temp if the symbol is known to be thread-local
      ;; (probably IR1 should go SYMBOL-VALUE -> SYMBOL-TLS-VALUE)
      (:temporary (:sc descriptor-reg) symbol-reg)
      (:results (value :scs (descriptor-reg any-reg)))
      (:vop-var vop)
      (:save-p :compute-only)
      (:variant-vars check-boundp)
      (:variant t)
      (:generator 9 (emit-symeval value symbol symbol-ref symbol-reg check-boundp vop)))

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
        (inst mov :qword temp (thread-tls-ea temp))
        (inst cmp :qword temp no-tls-value-marker)
        (inst cmov :dword :e temp (symbol-value-slot-ea object))
        (inst cmp :byte temp unbound-marker-widetag))))

) ; END OF MACROLET

#-sb-thread
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
      (inst cmp :byte (object-slot-ea
                 symbol symbol-value-slot other-pointer-lowtag)
            unbound-marker-widetag))))

(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst shr res n-symbol-hash-discard-bits)))

(eval-when (:compile-toplevel)
  ;; assumption: any object can be read 1 word past its base pointer
  (assert (= sb-vm:symbol-hash-slot 1))
  ;; also: whatever pointer tag a non-immediate object has, we can subtract
  ;; 3 without reading any byte outside of the object.
  (aver (= (- (+ 4 (ash symbol-hash-slot word-shift)) other-pointer-lowtag)
           -3))) ; 3 is the smallest pointer tag

(define-vop (symbol-name-hash symbol-hash)
  ;; identical translations believe it or not
  (:translate symbol-name-hash hash-as-if-symbol-name)
  (:generator 1
    ;; NIL gets 0 for its name hash since its upper 4 address bytes are 0
    (inst mov :dword res
          (ea (- (+ 4 (ash symbol-hash-slot word-shift)) other-pointer-lowtag)
              symbol))))

(aver (= sb-impl::package-id-bits 16))
(define-vop ()
  (:args (symbol :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:translate symbol-package-id)
  (:policy :fast-safe)
  (:generator 2
   (inst movzx '(:word :dword) result
         (object-slot-ea symbol symbol-name-slot (- other-pointer-lowtag 6)))))
(define-vop ()
  (:args (symbol :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg) :from :load))
  (:translate symbol-name)
  (:policy :fast-safe)
  (:generator 2
   (inst mov result (1- (ash 1 sb-impl::symbol-name-bits)))
   (inst and result (object-slot-ea symbol symbol-name-slot other-pointer-lowtag))))

;;;; fdefinition (FDEFN) objects

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
  (:args (function :scs (descriptor-reg))
         (fdefn :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) raw)
  (:generator 38
    (emit-gengc-barrier fdefn nil raw)
    (inst mov raw (make-fixup 'closure-tramp :assembly-routine))
    (inst cmp :byte (ea (- fun-pointer-lowtag) function)
          simple-fun-widetag)
    (inst cmov :e raw
          (ea (- (* simple-fun-self-slot n-word-bytes) fun-pointer-lowtag) function))
    (storew function fdefn fdefn-fun-slot other-pointer-lowtag)
    (storew raw fdefn fdefn-raw-addr-slot other-pointer-lowtag)))
#+immobile-code
(progn
(define-vop (set-direct-callable-fdefn-fun)
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
  (:temporary (:sc unsigned-reg) temp)
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
    (let ((fixup (make-fixup 'undefined-tramp :assembly-routine)))
      (if (sb-c::code-immobile-p vop)
          (inst lea temp (rip-relative-ea fixup)) ; avoid a preserved fixup
          (inst mov temp (ea fixup))))
    (storew temp fdefn fdefn-raw-addr-slot other-pointer-lowtag)))

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

(defun bind (bsp symbol tmp)
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
    (values tls-cell tmp)))

(define-vop (bind) ; bind a known symbol
  (:args (val :scs (any-reg descriptor-reg)
              :load-if (not (let ((imm (encode-value-if-immediate val)))
                              (or (fixup-p imm)
                                  (plausible-signed-imm32-operand-p imm))))))
  (:temporary (:sc unsigned-reg) bsp tmp)
  (:info symbol)
  (:generator 10
    (inst mov :qword (bind bsp symbol tmp) (encode-value-if-immediate val))))

(define-vop (rebind)
  (:temporary (:sc unsigned-reg) bsp tls-value)
  (:temporary (:sc descriptor-reg
               :unused-if (eq (immediate-constant-sc symbol) immediate-sc-number))
              symbol-reg)
  (:info symbol)
  (:node-var node)
  (:vop-var vop)
  (:generator 10
    (multiple-value-bind (tls-cell tls-value) (bind bsp symbol tls-value)
      (unless (symbol-always-has-tls-value-p symbol node)
        (unless (eq (tn-kind symbol-reg) :unused)
          (load-constant vop (emit-constant symbol) symbol-reg))
        (inst cmp tls-value no-tls-value-marker)
        (inst cmov :e tls-value
              (if (eq (tn-kind symbol-reg) :unused)
                  (symbol-slot-ea symbol symbol-value-slot)
                  (symbol-value-slot-ea symbol-reg)))
        (inst mov tls-cell tls-value))))))

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
    (emit-gengc-barrier symbol nil temp)
    (storew val symbol symbol-value-slot other-pointer-lowtag)))

#+sb-thread
(define-vop (unbind-n)
  (:temporary (:sc unsigned-reg) temp bsp)
  (:temporary (:sc complex-double-reg) zero)
  (:info symbols)
  (:vop-var vop)
  (:generator 0
    (load-binding-stack-pointer bsp)
    (inst xorpd zero zero)
    (loop for symbol in symbols
          for tls-index = (load-time-tls-offset symbol)
          for tls-cell = (thread-tls-ea tls-index)
          do
          #+ultrafutex
          (when (eq symbol '*current-mutex*)
            (let ((uncontested (gen-label)))
              (inst mov temp tls-cell) ; load the current value
              (inst mov :qword (mutex-slot temp %owner) 0)
              (inst dec :lock :byte (mutex-slot temp state))
              (inst jmp :z uncontested) ; if ZF then previous value was 1, no waiters
              (invoke-asm-routine 'call 'mutex-wake-waiter vop)
              (emit-label uncontested)))

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
    (emit-gengc-barrier symbol nil value) ; VALUE is the card-mark temp
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
      ;; Maybe I should say that PROGV can never be allowed to bind *CURRENT-MUTEX*
      ;; and I can eliminate this code here. However, UNWIND-TO-FRAME-AND-CALL
      ;; still has to do this check.  Maybe one extra arg it needed to this function
      ;; indicating whether it is an ordinary PROGV versus extra magical.
      #+ultrafutex
      (let ((notmutex (gen-label)))
        (inst cmp :dword symbol (make-fixup '*current-mutex* :symbol-tls-index))
        (inst jmp :ne notmutex)
        (inst call (ea (make-fixup 'mutex-unlock :assembly-routine)))
        (emit-label notmutex))
      (inst test :dword symbol symbol))
    #-sb-thread
    (progn
      (loadw symbol bsp binding-symbol-slot)
      (inst test symbol symbol))
    (inst jmp :z SKIP)
    #-sb-thread (progn (emit-gengc-barrier symbol nil value) ; VALUE is the card-mark temp
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
    ;; Perhaps this vop should become an unbind-to-here asm routine especially for
    ;; #+ultrafutex due to extra steps to unwind through a held mutex. On other other,
    ;; the vop is fairly rare, used only by PROGV (and the debugger)
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

;;; This is arguably a kludge without which you would hit the 'lose("Feh.")' in gencgc
;;; if trying to fold funinstance setters into closure-index-set.
;;; (Is's incorrect to manually manipulate the mark on a non-code page.)
#-soft-card-marks
(progn
#-sb-xc-host
(defun (setf %funcallable-instance-info) (newval fin index)
  (%primitive %set-funinstance-info fin index newval)
  newval)
(define-vop (%set-funinstance-info)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg))
         (value :scs (any-reg descriptor-reg)))
  (:arg-types * tagged-num *)
  (:temporary (:sc unsigned-reg) val-temp)
  (:generator 4
   (let ((ea (ea (- (* funcallable-instance-info-offset n-word-bytes) fun-pointer-lowtag)
                 object index (index-scale n-word-bytes index))))
     (pseudo-atomic ()
       (emit-code-page-gengc-barrier object val-temp)
       (emit-store ea value val-temp))))))

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
  (:vop-var vop)
  ;; temp is wasted if we don't need a barrier, which we almost never do
  (:temporary (:sc unsigned-reg) temp)
  (:generator 4
    (unless dx
      (let* ((value-tn (tn-ref-tn (tn-ref-across (vop-args vop))))
             (prim-type (sb-c::tn-primitive-type value-tn))
             (scs (and prim-type (sb-c::primitive-type-scs prim-type))))
        (when (and (not (singleton-p scs))
                   (member descriptor-reg-sc-number scs))
          (emit-gengc-barrier object nil temp (vop-nth-arg 1 vop) value))))
    (storew value object (+ closure-info-offset offset) fun-pointer-lowtag)))

(define-vop (closure-init-from-fp)
  (:args (object :scs (descriptor-reg)))
  (:info offset)
  (:generator 4
    ;; RBP-TN looks like a fixnum (non-pointer) so no barrier
    (storew rbp-tn object (+ closure-info-offset offset) fun-pointer-lowtag)))

;;;; value cell hackery

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
  instance-pointer-lowtag (any-reg descriptor-reg constant) * %instance-set)

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
      (emit-gengc-barrier instance nil val-temp values)
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
            (emit-store ea val val-temp)))
         (unless indices (return)))))
    (aver (not values))))

(define-full-compare-and-swap %instance-cas instance
  instance-slots-offset instance-pointer-lowtag
  (any-reg descriptor-reg) * %instance-cas)
(define-full-compare-and-swap %raw-instance-cas/word instance
  instance-slots-offset instance-pointer-lowtag
  (unsigned-reg) unsigned-num %raw-instance-cas/word)
(define-full-compare-and-swap %raw-instance-cas/signed-word instance
  instance-slots-offset instance-pointer-lowtag
  (signed-reg) signed-num %raw-instance-cas/signed-word)

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
      (emit-gengc-barrier object nil temp)
      (generate-dblcas (ea (- list-pointer-lowtag) object)
                       expected-old-lo expected-old-hi new-lo new-hi
                       eax ebx ecx edx result-lo result-hi)))

  ;; The CPU requires 16-byte alignment for the memory operand.
  ;; A vector's data portion starts on a 16-byte boundary, so any even numbered index is OK.
  (define-dblcas %vector-cas-pair t
    (:generator 2
      (let ((ea (ea (- (* n-word-bytes vector-data-offset) other-pointer-lowtag)
                    object index (ash n-word-bytes (- n-fixnum-tag-bits)))))
        (emit-gengc-barrier object ea temp)
        (generate-dblcas ea expected-old-lo expected-old-hi new-lo new-hi
                         eax ebx ecx edx result-lo result-hi))))

  ;; Here you have to specify an odd numbered slot.
  ;; An instance's first user-visible slot at index 1 is 16-byte-aligned.
  ;; (Hmm, does the constraint differ by +/- compact-instance-header?)
  (define-dblcas %instance-cas-pair t
    (:generator 2
      (emit-gengc-barrier object nil temp)
      (let ((ea (ea (- (* n-word-bytes instance-slots-offset) instance-pointer-lowtag)
                    object index (ash n-word-bytes (- n-fixnum-tag-bits)))))
        (generate-dblcas ea expected-old-lo expected-old-hi new-lo new-hi
                         eax ebx ecx edx result-lo result-hi)))))
