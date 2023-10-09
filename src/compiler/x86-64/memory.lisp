;;;; the x86 definitions of some general purpose memory reference VOPs
;;;; inherited by basic memory reference operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(defun symbol-slot-ea (symbol slot)
  (ea (let ((offset (- (* slot n-word-bytes) other-pointer-lowtag)))
             (if (static-symbol-p symbol)
                 (+ nil-value (static-symbol-offset symbol) offset)
                 (make-fixup symbol :immobile-symbol offset)))))

;;; TODOs:
;;; 1. Sometimes people write constructors like
;;;     (defun make-foo (&key a b c)
;;;      (let ((new-foo (really-make-foo)))
;;;        (when should-set-a (setf (foo-a new-foo) a))
;;;        (when should-set-b (setf (foo-b new-foo) b))
;;;        ...
;;;    In this case, the asssignments are constructor-like. Even though
;;;    they look mutating, the store barrier can be omitted.
;;;    I think the general idea is that if a slot of a newly
;;;    constructed thing receives the value of an incoming
;;;    argument, the object in that argument can't possibly
;;;    be younger than the newly constructed thing.
;;; 2. hash-table k/v pair should mark once only.
;;;    (the vector elements are certainly on the same card)
(defun emit-gengc-barrier (object cell-address scratch-reg &optional value-tn-ref value-tn)
  #-soft-card-marks (declare (ignore object cell-address scratch-reg value-tn-ref value-tn))
  #+soft-card-marks
  (progn
  (when (sc-is object constant immediate)
    (aver (symbolp (tn-value object))))
  (when (require-gengc-barrier-p object value-tn-ref value-tn)
    (if cell-address ; for SIMPLE-VECTOR, the page holding the specific element index gets marked
        (inst lea scratch-reg cell-address)
        ;; OBJECT could be a symbol in immobile space
        (inst mov scratch-reg (encode-value-if-immediate object)))
    (inst shr scratch-reg gencgc-card-shift)
    ;; gc_allocate_ptes() asserts mask to be < 32 bits, which is hugely generous.
    (inst and :dword scratch-reg card-index-mask)
    ;; I wanted to use thread-tn as the source of the store, but it isn't 256-byte-aligned
    ;; due to presence of negatively indexed thread header slots.
    ;; Probably word-alignment is enough, because we can just check the lowest bit,
    ;; borrowing upon the idea from PSEUDO-ATOMIC which uses RBP-TN as the source.
    ;; I'd like to measure to see if using a register is actually better.
    ;; If all threads store 0, it might be easier on the CPU's store buffer.
    ;; Otherwise, it has to remember who "wins". 0 makes it indifferent.
    (inst mov :byte (ea gc-card-table-reg-tn scratch-reg) CARD-MARKED))))

#-soft-card-marks
(defun emit-code-page-gengc-barrier (object scratch-reg)
  (inst mov scratch-reg object)
  (inst shr scratch-reg gencgc-card-shift)
  (inst and :dword scratch-reg card-index-mask)
  (inst mov :byte (ea gc-card-table-reg-tn scratch-reg) CARD-MARKED))

(defun branch-if-barrier-on (where)
  (when *in-pseudoatomic* (error "can't emit CMS barrier if already pseudoatomic"))
  (inst cmp :byte (thread-slot-ea thread-gc-phase-slot) GC-PHASE-SYNC1)
  (let ((label (gen-label)))
    (emit-label label)
    (push label (sb-assem::asmstream-pseudo-atomic-locs sb-assem:*asmstream*)))
  (inst jmp :ae where))

;;; FIXME: if and when I implement SIGUSR2 as the signal to cooperate
;;; with GC - so that we don't switch over to safepoints -
;;; then the only instruction sequences which will be _implicitly_ pseudo-atomic
;;; will be:
;;;   inst cmp [phase], 2
;;;   inst jmp :a elsewhere
;;;   inst mov [ea], something # any number of stores
;;; so if the interrupt handler sees that it has been interrupted in lisp code
;;; at a mov to memory, it will emulate the mov without allowing a phase change.
;;; It will NOT work to have either of the following:
;;;   inst cmp [phase], 2       |  inst cmp [phase], 2
;;;   inst jmp :a elsewhere     |  inst jmp :a elsewhere
;;;   inst mov temp, [rip-n]    |  inst mov temp, imm        <-- interrupted here
;;;   inst mov [ea], temp       |  inst mov [ea], temp
;;; In this instruction sequence, if we allow the interrupt to take place,
;;; and it changes the GC phase, then the preceding comparison answer can not be
;;; taken as correct. Perhaps we should have branched to the fallback code to
;;; perform a barrier, but we did not, because the phase looked like it was ASYNC.
;;; Somehow the GC phase has to be examined just before performing the store.
;;;
(defun emit-store (vop barrierp object ea value val-temp
                   &optional slot-name
                   &aux (asm-routine
                         (case barrierp
                           (:weak 'weak-vector-set)
                           (:untagged 'gc-barrier-store-untagged)
                           (t 'gc-barrier-store)))
                   notinline done)
  (declare (type (member t nil :weak :untagged) barrierp))
  (declare (ignorable slot-name))

  (when (and barrierp (stack-consed-p object))
    ;; barriers do not pertain to stack objects
    (setq barrierp nil))
  #+smlgc-telemetry
  (inst inc :qword (thread-slot-ea (if (stack-consed-p object)
                                       thread-ct-stack-obj-stores-slot
                                       thread-ct-heap-obj-stores-slot)))
  (labels ((imm (operation)
             (let ((bits (encode-value-if-immediate value)))
               ;; Try to move imm-to-mem if BITS fits
               (acond ((or (and (fixup-p bits)
                                ;; immobile-object fixups must fit in 32 bits
                                (eq (fixup-flavor bits) :immobile-symbol)
                                bits)
                           (plausible-signed-imm32-operand-p bits))
                       (if (eq operation 'push)
                           (inst push it) ; real good
                           (progn (barrier)
                                  (inst mov :qword ea it))))
                      (t
                       (inst mov val-temp bits)
                       (if (eq operation 'push)
                           (inst push val-temp)
                           (progn (barrier)
                                  (inst mov ea val-temp)))))))
           (barrier ()
             (when barrierp
               (branch-if-barrier-on (setq notinline (gen-label))))))
    (when (eq barrierp :weak) ; always call C
      (if (sc-is value immediate) (imm 'push) (inst push value))
      (inst lea val-temp ea)
      (inst push val-temp)
      (inst push (encode-value-if-immediate object))
      (invoke-asm-routine 'call asm-routine vop)
      (return-from emit-store))
    (sc-case value
     (immediate (imm 'store))
     (constant
      (inst mov val-temp value)
      (barrier)
      (inst mov :qword ea val-temp))
     (t
      (barrier)
      (inst mov :qword ea value)))
    (when barrierp
      (emit-label (setq done (gen-label)))
      (assemble (:elsewhere)
        (emit-label NOTINLINE)
        (if (sc-is value immediate) (imm 'push) (inst push value))
        (inst lea val-temp ea)
        (inst push val-temp)
        (inst push (encode-value-if-immediate object))
        (invoke-asm-routine 'call asm-routine vop)
        (inst jmp DONE)))))

;;; RAX is loaded with the expected oldval. Return from the slow path
;;; is the same as if by the fast path (actual oldval is in RAX)
(defun emit-cmpxchg (vop barrierp object ea old new rax
                     &aux (asm-routine
                           (if (eq barrierp :untagged)
                               'gc-barrier-cmpxchg-untagged
                               'gc-barrier-cmpxchg))
                           (notinline (gen-label)))
  (unless barrierp
    (inst mov rax (encode-value-if-immediate old))
    (inst cmpxchg :lock ea new)
    (return-from emit-cmpxchg))
  (assemble ()
    ;; optimistically assume we're taking the fast path
    (inst mov rax (encode-value-if-immediate old))
    (branch-if-barrier-on NOTINLINE)
    (inst cmpxchg :lock ea new)
    DONE
    (assemble (:elsewhere)
      (emit-label NOTINLINE)
      (inst push new)
      (inst lea rax ea)
      (inst push rax)
      (inst push (encode-value-if-immediate object))
      (inst mov rax (encode-value-if-immediate old))
      (invoke-asm-routine 'call asm-routine vop)
      (inst jmp done))))

;;; CELL-REF and CELL-SET are used to define VOPs like CAR, where the
;;; offset to be read or written is a property of the VOP used.
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (check-alivep object lowtag)
    (loadw value object offset lowtag)))
;; This vop's sole purpose is to be an ancestor for other vops, to assign
;; default operands, policy, and generator.
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg immediate)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg) val-temp)
  (:vop-var vop)
  (:generator 4
    (check-alivep object lowtag)
    (emit-gengc-barrier object nil val-temp (vop-nth-arg 1 vop) value)
    (emit-store vop t object (object-slot-ea object offset lowtag) value val-temp)))

;;; X86 special
(define-vop (cell-xadd)
  (:args (object :scs (descriptor-reg) :to :result)
         (value :scs (any-reg) :target result))
  (:results (result :scs (any-reg) :from (:argument 1)))
  (:result-types tagged-num)
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (move result value)
    (inst xadd :lock (object-slot-ea object offset lowtag) result)))

(define-vop (cell-xsub cell-xadd)
  (:args (object)
         (value :scs (any-reg immediate) :target result))
  (:generator 5
    ;; For constant delta we can avoid a mov followed by neg
    ;; but if 'delta' is most-negative-fixnum, don't negate it.
    ;; Decrementing by most-negative-fixnum is the same as incrementing.
    (sc-case value
     (immediate
      (let ((k (tn-value value)))
        (inst mov result (fixnumize (if (= k most-negative-fixnum) k (- k))))))
     (t
      (move result value)
      (inst neg result)))
    (inst xadd :lock (object-slot-ea object offset lowtag) result)))

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

(macrolet
    ((def-atomic (fun-name inherit slot)
       `(progn
          (define-vop (,(symbolicate fun-name "/FAST") ,inherit)
            (:translate ,fun-name)
            (:policy :fast)
            (:arg-types * tagged-num)
            (:variant ,slot list-pointer-lowtag))
          (define-vop (,(symbolicate fun-name "/SAFE"))
            (:translate ,fun-name)
            (:policy :fast-safe)
            (:args (cell :scs (descriptor-reg))
                   (delta :scs (any-reg immediate)))
            (:results (result :scs (any-reg)))
            (:temporary (:sc descriptor-reg :offset rax-offset) rax)
            (:temporary (:sc any-reg) newval)
            (:arg-types * tagged-num)
            (:result-types tagged-num)
            (:vop-var vop)
            (:generator 10
             (let ((err (generate-error-code vop 'object-not-fixnum-error rax))
                   (const (if (sc-is delta immediate)
                              (fixnumize ,(if (eq inherit 'cell-xsub)
                                              `(let ((x (tn-value delta)))
                                                 (if (= x most-negative-fixnum)
                                                     x (- x)))
                                              `(tn-value delta)))))
                   (retry (gen-label)))
               (loadw rax cell ,slot list-pointer-lowtag)
               (emit-label retry)
               (inst test rax fixnum-tag-mask)
               (inst jmp :nz err)
               (if const
                   (cond ((typep const '(signed-byte 32))
                          (inst lea newval (ea const rax)))
                         (t
                          (inst mov newval const)
                          (inst add newval rax)))
                   ,(if (eq inherit 'cell-xsub)
                        `(progn (move newval rax)
                                (inst sub newval delta))
                        `(inst lea newval (ea rax delta))))
               (inst cmpxchg :lock
                     (object-slot-ea cell ,slot list-pointer-lowtag)
                     newval)
               (inst jmp :ne retry)
               (inst mov result rax)))))))
  (def-atomic %atomic-inc-car cell-xadd cons-car-slot)
  (def-atomic %atomic-inc-cdr cell-xadd cons-cdr-slot)
  (def-atomic %atomic-dec-car cell-xsub cons-car-slot)
  (def-atomic %atomic-dec-cdr cell-xsub cons-cdr-slot))

;; Atomically set a bit of an instance header word
(define-vop (set-instance-hashed)
  (:args (x :scs (descriptor-reg)))
  (:generator 1
    (inst or :lock :byte (ea (- 1 instance-pointer-lowtag) x)
          ;; Bit index is 0-based. Subtract 8 since we're using the EA
          ;; to select byte 1 of the header word.
          (ash 1 (- stable-hash-required-flag 8)))))

(defmacro compute-splat-bits (value)
  ;; :SAFE-DEFAULT means any unspecific value that is safely a default.
  ;; Heap allocation uses 0 since that costs nothing.
  ;; If the user wanted a specific value, it could have been explicitly given.
  `(if (typep ,value 'sb-vm:word)
       ,value
       (case ,value
         (:unbound (unbound-marker-bits))
         ((nil) (bug "Should not see SPLAT NIL"))
         (t #+ubsan unwritten-vector-element-marker
            #-ubsan 0))))

;;; This logic was formerly in ALLOCATE-VECTOR-ON-STACK.
;;; Choosing amongst 3 vops gets potentially better register allocation
;;; by not wasting registers in the cases that don't use them.
(define-vop (splat-word)
  (:policy :fast-safe)
  (:translate splat)
  (:args (vector :scs (descriptor-reg)))
  (:info words value)
  (:arg-types * (:constant (eql 1)) (:constant t))
  (:results (result :scs (descriptor-reg)))
  (:generator 1
   (progn words) ; don't put it in :ignore, which gets inherited
   (inst mov :qword
         (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag) vector)
         (compute-splat-bits value))
   (move result vector)))

(define-vop (splat-small splat-word)
  (:arg-types * (:constant (integer 2 10)) (:constant t))
  (:temporary (:sc complex-double-reg) zero)
  (:generator 5
   (let ((bits (compute-splat-bits value)))
     (if (= bits 0)
         (inst xorpd zero zero)
         (inst movdqa zero
               (register-inline-constant :oword (logior (ash bits 64) bits)))))
   (let ((data-addr (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                        vector)))
     (multiple-value-bind (double single) (truncate words 2)
       (dotimes (i double)
         (inst movapd data-addr zero)
         (setf data-addr (ea (+ (ea-disp data-addr) (* n-word-bytes 2))
                             (ea-base data-addr))))
       (unless (zerop single)
         (inst movaps data-addr zero))))
   (move result vector)))

(define-vop (splat-any splat-word)
  ;; vector has to conflict with everything so that a tagged pointer
  ;; corresponding to RDI always exists
  (:args (vector :scs (descriptor-reg) :to (:result 0))
         (words :scs (unsigned-reg immediate) :target rcx))
  (:info value)
  (:arg-types * positive-fixnum (:constant t))
  (:temporary (:sc any-reg :offset rdi-offset :from (:argument 0)
               :to (:result 0)) rdi)
  (:temporary (:sc any-reg :offset rcx-offset :from (:argument 1)
               :to (:result 0)) rcx)
  (:temporary (:sc any-reg :offset rax-offset :from :eval
               :to (:result 0)) rax)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
   (inst lea rdi (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                        vector))
   (let ((bits (compute-splat-bits value)))
     (cond ((and (= bits 0)
                 (constant-tn-p words)
                 (typep (tn-value words) '(unsigned-byte 7)))
            (zeroize rax)
            (inst lea :dword rcx (ea (tn-value words) rax))) ; smaller encoding
           (t
            ;; words could be in RAX, so read it first, then zeroize
            (inst mov rcx (or (and (constant-tn-p words) (tn-value words)) words))
            (if (= bits 0) (zeroize rax) (inst mov rax bits)))))
   (inst rep)
   (inst stos :qword)
   (move result vector)))

(dolist (name '(splat-word splat-small splat-any))
  ;; It wants a function, not a symbol
  (setf (sb-c::vop-info-optimizer (template-or-lose name))
        (lambda (vop) (sb-c::elide-zero-fill vop))))

;; Placeholders until non-STW GC becomes a reality
#+weak-vector-readbarrier
(progn
  (define-full-setter %weakvec-set * vector-data-offset other-pointer-lowtag
    (any-reg descriptor-reg) * %weakvec-set)

  (define-vop (%weakvec-ref)
    (:translate %weakvec-ref)
    (:policy :fast-safe)
    (:args (object :scs (descriptor-reg))
           (index :scs (any-reg signed-reg unsigned-reg)))
    (:arg-types * tagged-num)
    (:results (value :scs (descriptor-reg)))
    (:result-types *)
(:temporary (:sc unsigned-reg) junk)
    (:vop-var vop)
    (:generator 10
      (pseudo-atomic ()
        (inst mov junk (ea (make-fixup "weakrefget_ct" :foreign-dataref)))
        (inst add :dword :lock (ea junk) 1)
        (inst cmp :byte (thread-slot-ea thread-gc-phase-slot) GC-PHASE-ASYNC)
        (inst jmp :e FAST)
        (inst push object)
        (inst push index)
        (unless (sc-is index any-reg) (inst shl (ea rsp-tn) n-fixnum-tag-bits)) ; pass as fixnum
        (invoke-asm-routine 'call 'weak-vector-ref vop)
        FAST
        (inst mov value (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag)
                          object index (index-scale n-word-bytes index))))))

  (define-vop (%weak-pointer-value)
    (:policy :fast-safe)
    (:args (weakptr :scs (descriptor-reg)))
    (:results (value :scs (descriptor-reg)))
    (:vop-var vop)
    (:temporary (:sc unsigned-reg) junk)
    (:generator 10
      (pseudo-atomic ()
        (inst mov junk (ea (make-fixup "weakrefget_ct" :foreign-dataref)))
        (inst add :dword :lock (ea junk) 1)
        (inst cmp :byte (thread-slot-ea thread-gc-phase-slot) GC-PHASE-ASYNC)
        (inst jmp :e FAST)
        (inst push weakptr)
        (invoke-asm-routine 'call 'weak-pointer-ref vop)
        FAST
        (loadw value weakptr weak-pointer-value-slot other-pointer-lowtag)))))
