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
  (:info name offset lowtag barrier)
  (:results)
  (:vop-var vop)
  (:temporary (:sc unsigned-reg) val-temp)
  (:gc-barrier 0 1 0)
  (:ignore name)
  (:generator 1
    (cond #+ubsan
          ((and (eql offset sb-vm:array-fill-pointer-slot) ; half-sized slot
                (or (eq name 'make-array)
                    (equal name '(setf %array-fill-pointer))))
           (when (eq name 'make-array) ; nullify the creating PC location
             (inst mov :qword (object-slot-ea object 1 lowtag) null-tn))
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
           (when barrier (emit-gengc-barrier object nil val-temp t))
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
  (:results (result :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:generator 5
     (let ((newval-tn-ref (vop-nth-arg 2 vop)))
       (if (eq name 'sb-impl::cas-symbol-%info)
           (emit-symbol-write-barrier vop object rax newval-tn-ref)
           (emit-gengc-barrier object nil rax newval-tn-ref)))
     (move rax old)
     (inst cmpxchg :lock (ea (- (* offset n-word-bytes) lowtag) object) new)
     (move result rax)))

;;;; symbol hacking VOPs

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst mov result unbound-marker-widetag)))

(defun emit-symbol-write-barrier (vop symbol temp newval-tn-ref)
  (declare (ignorable vop))
  #+permgen
  (when (require-gengc-barrier-p symbol newval-tn-ref)
    (unless (and (sc-is symbol immediate) (static-symbol-p (tn-value symbol)))
      (inst push symbol)
      (invoke-asm-routine 'call 'gc-remember-symbol vop)))
  ;; IMMEDIATE sc means that the symbol is static or immobile.
  ;; Static symbols are roots, and immobile symbols use page fault handling.
  (unless (sc-is symbol immediate)
    (emit-gengc-barrier symbol nil temp newval-tn-ref)))

;; Return the effective address of the value slot of static SYMBOL.
(defun static-symbol-value-ea (symbol &optional (byte 0))
  (ea (+ (static-symbol-offset symbol)
         (ash symbol-value-slot word-shift)
         byte
         (- other-pointer-lowtag))
      null-tn))

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

(define-vop (%set-symbol-global-value)
  (:args (symbol :scs (descriptor-reg immediate))
         (value :scs (descriptor-reg any-reg immediate)))
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg) val-temp)
  (:vop-var vop)
  (:generator 4
    (emit-symbol-write-barrier vop symbol val-temp (vop-nth-arg 1 vop))
    (emit-store (if (sc-is symbol immediate)
                      (symbol-slot-ea (tn-value symbol) symbol-value-slot)
                      (object-slot-ea symbol symbol-value-slot other-pointer-lowtag))
      value val-temp)))

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
            unbound-marker-widetag)))

  (define-vop (%compare-and-swap-symbol-value)
    (:translate %compare-and-swap-symbol-value %cas-symbol-global-value)
    (:args (symbol :scs (descriptor-reg) :to (:result 0))
           (old :scs (descriptor-reg any-reg constant immediate))
           (new :scs (descriptor-reg any-reg)))
    (:temporary (:sc descriptor-reg :offset rax-offset :to (:result 0)) rax)
    (:results (result :scs (descriptor-reg any-reg)))
    (:policy :fast-safe)
    (:vop-var vop)
    (:node-var node)
    (:generator 15
      (emit-symbol-write-barrier vop symbol rax (vop-nth-arg 2 vop))
      (if (sc-is old immediate)
          (move-immediate rax (immediate-tn-repr old))
          (move rax old))
      (inst cmpxchg :lock (object-slot-ea symbol symbol-value-slot other-pointer-lowtag) new)
      (unless (policy node (= safety 0))
        (inst cmp :byte rax unbound-marker-widetag)
        (inst jmp :e (generate-error-code vop 'unbound-symbol-error symbol)))
      (move result rax)))

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
      (store-binding-stack-pointer bsp))))

;;; I don't know which of SYMBOL-HASH or SYMBOL-NAME-HASH is dynamically executed most.
;;; Whichever one that is can slightly optimized more by avoiding a load from the hash of NIL
;;; and using NULL-TN directly. Currently this favors SYMBOL-HASH.
;;; To favor SYMBOL-NAME-HASH, we would need to flip the position of the hash and fname-index
;;; fields within the hash slot.  Putting HASH in the low 4 bytes allows a :DWORD XOR with
;;; NULL-TN to achieve position-independence of NIL's hash.
;;; Such minutia I do not care to deal with at the moment.
(define-vop (symbol-hash)
  (:policy :fast-safe)
  (:translate symbol-hash)
  (:args (symbol :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 2
    (loadw res symbol symbol-hash-slot other-pointer-lowtag)
    (inst xor res null-tn)
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
    ;; The number of times this vop is executed on an arg _not_ known to be a symbol
    ;; is about 20x the number of times it _is_ known to be a symbol. (Think of all the
    ;; CASE expressions where the arg type was not pre-tested)
    ;; It is quick to unconditionally XOR with a word that is likely in L1 cache already
    ;; (CAR of NIL) versus conditionally branching, making this vop more efficient than
    ;; with the alternate definition of SYMBOL slots which put HASH near the end.
    (inst mov :dword res
          (ea (- (+ 4 (ash symbol-hash-slot word-shift)) other-pointer-lowtag)
              symbol))
    (inst xor :dword res (ea (- 4 list-pointer-lowtag) null-tn))))

(aver (= sb-impl::package-id-bits 16))
(define-vop (symbol-package-id)
  (:args (symbol :scs (descriptor-reg)))
  (:results (result :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:translate symbol-package-id)
  (:policy :fast-safe)
  (:generator 2
   (inst movzx '(:word :dword) result (ea (- 1 other-pointer-lowtag) symbol))))

;;;; fdefinition (FDEFN) objects

#+sb-xc-host ; not needed post-build
(macrolet ((gcbar ()
             `(assemble ()
                #+permgen
                (progn
                  (inst cmp :byte (object-slot-ea object 0 other-pointer-lowtag) fdefn-widetag)
                  (inst jmp :e SKIP)
                  (inst push object)
                  (invoke-asm-routine 'call 'gc-remember-symbol vop))
                SKIP
                (emit-gengc-barrier object nil temp))))
(define-vop (set-fname-linkage-index)
  (:args (object :scs (descriptor-reg))
         (index :scs (unsigned-reg))
         (linkage-cell :scs (sap-reg))
         (linkage-val :scs (unsigned-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:vop-var vop)
  (:generator 1
    (gcbar)
    (inst cmp :byte (ea (- other-pointer-lowtag) object) fdefn-widetag)
    (inst jmp :ne SYMBOL)
    (inst mov :dword (ea (- 4 other-pointer-lowtag) object) index)
    (inst jmp CELL-SET)
    SYMBOL
    (inst or :dword :lock
          (object-slot-ea object symbol-hash-slot other-pointer-lowtag) index)
    CELL-SET
    (inst mov (ea linkage-cell) linkage-val)))
(define-vop (set-fname-fun)
  (:args (object :scs (descriptor-reg))
         (function :scs (descriptor-reg))
         (linkage-cell :scs (sap-reg))
         (linkage-val :scs (unsigned-reg immediate)))
  (:temporary (:sc unsigned-reg) temp)
  (:vop-var vop)
  (:generator 1
    (gcbar)
    (storew function object fdefn-fun-slot other-pointer-lowtag)
    (unless (and (sc-is linkage-val immediate) (zerop (tn-value linkage-val)))
      (inst mov (ea linkage-cell) linkage-val)))))

(define-vop (fdefn-fun) ; This vop works on symbols and fdefns
  (:args (fdefn :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg)))
  (:policy :fast-safe)
  (:translate fdefn-fun)
  (:generator 2
    (loadw result fdefn fdefn-fun-slot other-pointer-lowtag)
    (inst test :dword result result)
    (inst cmov :z result null-tn)))

(define-vop (safe-fdefn-fun)
  (:translate safe-fdefn-fun)
  (:policy :fast-safe)
  (:args (object :scs (descriptor-reg) :to (:result 1)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 10
    (loadw value object fdefn-fun-slot other-pointer-lowtag)
    (inst test :dword value value)
    (let* ((*location-context* (make-restart-location RETRY value))
           (err-lab (generate-error-code vop 'undefined-fun-error object)))
      (inst jmp :e err-lab))
    RETRY))


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
        ;; "Call [ea]" is generally fine except during genesis.
        ;; (ASM-ROUTINE-INDIRECT-ADDRESS complains)
        (if (or #+(and sb-xc-host immobile-code) t)
            (inst call (make-fixup 'mutex-unlock :assembly-routine))
            (inst call (ea (make-fixup 'mutex-unlock :assembly-routine))))
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
          (emit-gengc-barrier object nil temp (vop-nth-arg 1 vop)))))
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
  (:arg-refs obj-ref)
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
      (when (eq (tn-ref-type obj-ref) (specifier-type 'layout))
        (bug "unexpected set-multiple"))
      (emit-gengc-barrier instance nil val-temp values)
      (when use-xmm-p
        (inst xorpd xmm-temp xmm-temp))
      (loop
       (let* ((index (pop indices))
              (val (tn-ref-tn values))
              (ea (object-slot-ea instance (+ instance-slots-offset index)
                                  instance-pointer-lowtag)))
         (setq values (tn-ref-across values))
         ;; If the xmm temp was loaded with 0 and this value is 0,
         ;; and possibly the next, then store through the temp
         (cond
           ((and use-xmm-p (constant-tn-p val) (eql (tn-value val) 0))
            (let* ((next-index (car indices))
                   (next-val (if next-index (tn-ref-tn values))))
              (cond ((and (eql (1+ index) next-index)
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
    (let ((source (if (location= result instance) temp result))
          (slot (+ instance-slots-offset index)))
      (if (sc-is newval immediate)
          (inst mov source (constantize (tn-value newval)))
          (move source newval))
      (inst xchg (object-slot-ea instance slot instance-pointer-lowtag) source)
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
