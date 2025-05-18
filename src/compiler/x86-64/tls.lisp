;;;; This contains the #+sb-thread vops for SYMBOL-VALUE, BOUNDP, etc

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defmacro symbol-value-slot-ea (sym)    ; SYM is a TN
  `(ea (- (* symbol-value-slot n-word-bytes) other-pointer-lowtag)
       ,sym))

(defun emit-lea-symbol-value-slot (ea-tn symbol) ; dest, src
  (if (sc-is symbol immediate)
      (inst mov ea-tn (make-fixup (tn-value symbol) :immobile-symbol
                                  (- (ash symbol-value-slot word-shift) other-pointer-lowtag)))
      (inst lea ea-tn (object-slot-ea symbol symbol-value-slot other-pointer-lowtag))))

;;; This does not resolve the TLS-INDEX at load-time, because we don't want to
;;; waste TLS indices for symbols that may never get thread-locally bound.
;;; So instead we make a fixup to the address of the 4-byte TLS field in the symbol.
(defun symbol-tls-index-ea (symbol)
  (ea (make-fixup (tn-value symbol) :immobile-symbol (- 4 other-pointer-lowtag))))

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

(macrolet ((access-wired-tls-val (sym) ; SYM is a symbol
             `(thread-tls-ea (load-time-tls-offset ,sym))))

(flet ((emit-cas (ea symbol old new rax result vop &aux (node (sb-c::vop-node vop)))
         (if (sc-is old immediate) (move-immediate rax (immediate-tn-repr old)) (move rax old))
         (inst cmpxchg :lock ea new)
         (unless (or (and (sc-is symbol immediate) (sb-c::always-boundp (tn-value symbol) node))
                     (policy node (= safety 0)))
           (inst cmp :byte rax unbound-marker-widetag)
           (inst jmp :e (generate-error-code vop 'unbound-symbol-error symbol)))
         (move result rax)))
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
    (emit-symbol-write-barrier vop symbol rax (vop-nth-arg 2 vop))
    (emit-cas (if (sc-is symbol immediate)
                  (symbol-slot-ea (tn-value symbol) symbol-value-slot)
                  (symbol-value-slot-ea symbol))
              symbol old new rax result vop)))

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
  (:generator 15
  ;; This code has two pathological cases: NO-TLS-VALUE-MARKER
  ;; or UNBOUND-MARKER as NEW: in either case we would end up
  ;; doing possible damage with CMPXCHG -- so don't do that!
  ;; Even worse: don't supply old=NO-TLS-VALUE with a symbol whose
  ;; tls-index=0, because that would succeed, assigning NEW to each
  ;; symbol in existence having otherwise no thread-local value.
    (inst mov :dword cell (if (sc-is symbol immediate)
                              (symbol-tls-index-ea symbol)
                              (tls-index-of symbol)))
    (inst add cell thread-tn)
    (inst cmp :qword (ea cell) no-tls-value-marker)
    (inst jmp :ne CAS)
    ;; GLOBAL
    (emit-symbol-write-barrier vop symbol cell (vop-nth-arg 2 vop))
    (emit-lea-symbol-value-slot cell symbol)
    CAS
    (emit-cas (ea cell) symbol old new rax result vop))))

;; This code is tested by 'codegen.impure.lisp'
(defun emit-symeval (value symbol symbol-ref symbol-reg check-boundp vop)
  (let ((known-symbol (and (constant-tn-p symbol) (tn-value symbol))))
    (cond
      ((symbol-always-has-tls-value-p symbol-ref (sb-c::vop-node vop))
       (setq symbol-reg nil)
       (inst mov value (access-wired-tls-val known-symbol)))
      (t
       ;; Step 1: load the TLS index and then then the slot of the thread
       (cond
         ((symbol-always-has-tls-index-p known-symbol) ; e.g. CL:*PRINT-BASE*
          ;; Known nonzero TLS index, but possibly no per-thread value.
          ;; The TLS value and global value can be loaded independently.
          (inst mov value (access-wired-tls-val known-symbol))
          (when (sc-is symbol constant)
            (inst mov symbol-reg symbol))) ; = MOV Rxx, [RIP-N]

         ((sc-is symbol descriptor-reg)
          (inst mov :dword symbol-reg (tls-index-of symbol))
          (inst mov value (thread-tls-ea symbol-reg))
          (setq symbol-reg symbol))

         (t ; unknown TLS index, possibly 0
          (sc-case symbol
            (immediate
             ;; load the TLS index from the symbol.
             (inst mov :dword value
                   ;; slot index 1/2 is the high half of the header word.
                   (symbol-slot-ea known-symbol 1/2))
             ;; read the TLS value using that index
             (inst mov value (thread-tls-ea value)))
            (constant
             ;; These reads are inextricably data-dependent
             (inst mov symbol-reg symbol) ; = MOV REG, [RIP-N]
             (inst mov :dword value (tls-index-of symbol-reg))
             (inst mov value (thread-tls-ea value))))))

       ;; Step 2: Load VALUE from symbol's slot if TLS value didn't exist
       (inst cmp :qword value no-tls-value-marker)
       (inst cmov :e value
             (if (sc-is symbol immediate)
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
) ; end macrolet

(define-vop (symbol-value)
  (:translate symbol-value)
  (:policy :fast-safe)
  (:args (symbol :scs (descriptor-reg constant immediate) :to (:result 1)))
  (:arg-refs symbol-ref)
  (:temporary (:unused-if (symbol-always-has-tls-value-p symbol-ref (sb-c::vop-node vop))
               :sc descriptor-reg)
              temp)
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:variant-vars check-boundp)
  (:variant t)
  (:generator 9 (emit-symeval value symbol symbol-ref temp check-boundp vop)))

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
  (:args (symbol :scs (descriptor-reg constant immediate)))
  (:node-var node)
  (:conditional :ne)
  (:temporary (:sc unsigned-reg) temp)
  (:generator 9
   (cond
     ((sc-is symbol descriptor-reg)
      (inst mov :dword temp (tls-index-of symbol))
      (inst mov :qword temp (thread-tls-ea temp))
      (inst cmp :qword temp no-tls-value-marker)
      (inst cmov :dword :e temp (symbol-value-slot-ea symbol))
      (inst cmp :byte temp unbound-marker-widetag))
     ((and (sc-is symbol constant) (not (constant-tn-p symbol)))
      ;; The easy way would use a second temp reg, but it's avoidable using only 1 more
      ;; instruction that would have been needed regardless for loading another temp.
      (inst mov :qword temp symbol)
      (inst mov :dword temp (tls-index-of temp))
      ;; TEMP will address the TLS area as if a symbol whose value slot is the TLS word
      (let ((ea (object-slot-ea temp symbol-value-slot other-pointer-lowtag)))
        (inst lea temp (ea (- (ea-disp ea)) thread-tn temp))
        (inst cmp :qword ea no-tls-value-marker)
        (inst cmov :qword :e temp symbol) ; change TEMP to SYMBOL if no TLS value
        (inst cmp :byte ea unbound-marker-widetag)))
     ((symbol-always-has-tls-value-p symbol node)
      ;; check only the TLS
      (inst cmp :byte (thread-tls-ea (load-time-tls-offset (tn-value symbol)))
            unbound-marker-widetag))
     ((eq (info :variable :kind (tn-value symbol)) :global)
      ;; Call should have been elided in IR1 if always bound.
      ;; (what about ':EVENTUALLY and compiling to memory- is is always bound yet?)
      ;; Difficulty: this has to be aware of the policy decision
      ;; that inhibits the BOUNDP transform.
      ;; (aver (neq (info :variable :always-bound (tn-value symbol)) :always-bound))
      (inst cmp :byte (cond ((sc-is symbol constant)
                             (inst mov temp symbol)
                             (object-slot-ea temp symbol-value-slot other-pointer-lowtag))
                            (t
                             (symbol-slot-ea (tn-value symbol) symbol-value-slot)))
            unbound-marker-widetag))
     (t
      ;; For a known symbol that is not known to be either aways global or thread-local,
      ;; wire in a TLS index. It's unlikely that TLS will be exhausted by doing this, in contrast
      ;; to emit-symeval, where it is inadvisable to waste hundreds of TLS slots by eagerly
      ;; giving a symbol a fixed index just because the compiler observed a reference.
      (inst mov :qword temp (thread-tls-ea (load-time-tls-offset (tn-value symbol))))
      (inst cmp :qword temp no-tls-value-marker)
      (if (sc-is symbol immediate)
          (inst cmov :dword :e temp (symbol-slot-ea (tn-value symbol) symbol-value-slot))
          (assemble ()
           (inst jmp :ne THREAD-LOCAL)
           (inst mov temp symbol)
           (inst mov :dword temp (object-slot-ea temp symbol-value-slot other-pointer-lowtag))
           THREAD-LOCAL))
      (inst cmp :byte temp unbound-marker-widetag)))))

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
    (emit-symbol-write-barrier vop symbol val-temp (vop-nth-arg 1 vop))
    (emit-lea-symbol-value-slot cell symbol)
    STORE
    (emit-store (ea cell) value val-temp)))

;;;; binding and unbinding

;;; BIND -- Establish VAL as a binding for SYMBOL. Save the old value and
;;; the symbol on the binding stack and stuff the new value into the
;;; symbol.
;;; See the "Chapter 9: Specials" of the SBCL Internals Manual.

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
        (inst mov tls-cell tls-value)))))

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

(define-vop (atomic-inc-symbol-global-value cell-xadd)
  (:translate %atomic-inc-symbol-global-value)
  ;; The function which this vop translates will not
  ;; be used unless the variable is proclaimed as fixnum.
  ;; All stores are checked in a safe policy, so this
  ;; vop is safe because it increments a known fixnum.
  (:policy :fast-safe)
  (:arg-types * tagged-num)
  (:variant symbol-value-slot other-pointer-lowtag))

(define-vop (atomic-dec-symbol-global-value cell-xsub)
  (:translate %atomic-dec-symbol-global-value)
  (:policy :fast-safe)
  (:arg-types * tagged-num)
  (:variant symbol-value-slot other-pointer-lowtag))
