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
  (let ((offset (- (* slot n-word-bytes) other-pointer-lowtag)))
    (if (static-symbol-p symbol)
        (ea (+ (static-symbol-offset symbol) offset) null-tn)
        (ea (make-fixup symbol :immobile-symbol offset)))))

;;; The GC card table base is either an imm8 displacement from NULL-TN, or that
;;; plus one backend page if #+sb-safepoint. The reason for sliding the table up
;;; with #+sb-safepoint is that the safepoint trap address wants to be an imm8 away
;;; from NIL, as there are far more safepoint instructions than GC barriers.
(defun mark-gc-card (addr-or-card-index &optional (compute t))
  (when compute
    (inst shr addr-or-card-index gencgc-card-shift)
    ;; :DWORD suffices because gc_allocate_ptes() asserts mask to be < 32 bits
    (inst and :dword addr-or-card-index card-index-mask))
  (inst mov :byte (ea nil-cardtable-disp null-tn addr-or-card-index) CARD-MARKED))

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
(defun emit-gengc-barrier (object cell-address scratch-reg &optional value-tn-ref allocator)
  #-soft-card-marks (declare (ignore object cell-address scratch-reg value-tn-ref allocator))
  #+soft-card-marks
  (progn
    (when (sc-is object constant immediate)
      (aver (symbolp (tn-value object))))
    (cond ((or (eq value-tn-ref t)
               (require-gengc-barrier-p object value-tn-ref allocator))
           (if cell-address ; for SIMPLE-VECTOR, the page holding the specific element index gets marked
               (inst lea scratch-reg cell-address)
               ;; OBJECT could be a symbol in immobile space
               (inst mov scratch-reg (encode-value-if-immediate object)))
           (mark-gc-card scratch-reg))
          #+debug-gc-barriers
          (t
           (flet ((encode (x)
                    (sc-case x
                      (constant
                       (load-constant nil x scratch-reg)
                       scratch-reg)
                      (t
                       (let ((value (encode-value-if-immediate x)))
                         (if (integerp value)
                             (constantize value)
                             value))))))
             (when value-tn-ref
               (loop do
                     (unless (and (sc-is (tn-ref-tn value-tn-ref) immediate)
                                  (typep (tn-value (tn-ref-tn value-tn-ref)) '(or integer boolean)))
                       (inst push 1)
                       (inst push (encode object))
                       (inst push (encode (tn-ref-tn value-tn-ref)))
                       (invoke-asm-routine 'call 'check-barrier sb-assem::*current-vop*))
                     (setf value-tn-ref (tn-ref-across value-tn-ref))
                     while value-tn-ref)))))))

#-soft-card-marks
(defun emit-code-page-gengc-barrier (object scratch-reg)
  (inst mov scratch-reg object)
  (mark-gc-card scratch-reg))

(defun emit-store (ea value val-temp &optional (tag-immediate t))
  (sc-case value
   (immediate
      (let ((bits (immediate-tn-repr value tag-immediate)))
        ;; Try to move imm-to-mem if BITS fits
        (acond ((or (and (fixup-p bits)
                         ;; immobile-object fixups must fit in 32 bits
                         (eq (fixup-flavor bits) :immobile-symbol)
                         bits)
                    (plausible-signed-imm32-operand-p bits))
                (inst mov :qword ea it))
               ((tn-p bits) (inst mov ea bits)) ; null-tn
               (t
                (move-immediate val-temp bits)
                (inst mov ea val-temp)))))
   (constant
      (inst mov val-temp value)
      (inst mov :qword ea val-temp))
   (t
      (inst mov :qword ea value))))

;; This vop's sole purpose is to provide the implementation of value-cell-set.
;; It could be removed, for x86-64 anyway.
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg immediate)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:temporary (:sc unsigned-reg) val-temp)
  (:vop-var vop)
  (:generator 4
    (emit-gengc-barrier object nil val-temp (vop-nth-arg 1 vop))
    (let ((ea (object-slot-ea object offset lowtag)))
      (emit-store ea value val-temp))))

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

(defun compute-splat-bits (value)
  ;; :SAFE-DEFAULT means any unspecific value that is safely a default.
  ;; Heap allocation uses 0 since that costs nothing.
  ;; If the user wanted a specific value, it could have been explicitly given.
  (if (typep value 'sb-vm:word)
      value
      (case value
         (:unbound unbound-marker-widetag)
         ((nil) null-tn)
         (t #+ubsan unwritten-vector-element-marker
            #-ubsan 0))))

;;; This logic was formerly in ALLOCATE-VECTOR-ON-STACK.
;;; Choosing amongst 3 vops gets potentially better register allocation
;;; by not wasting registers in the cases that don't use them.
;;; (Actually now that :UNUSED-IF is an option, these can probably
;;; all be combined into one vop which can indicate which temps aren't
;;; used. When these vops were first written, it wasn't an option)
(define-vop (splat-word)
  (:policy :fast-safe)
  (:translate splat)
  (:args (vector :scs (descriptor-reg)))
  (:info words value)
  (:arg-types * (:constant (eql 1)) (:constant t))
  (:results (result :scs (descriptor-reg)))
  (:generator 1
   (progn words) ; don't put it in :ignore, which gets inherited
   (let ((bits (compute-splat-bits value)))
     (aver (or (tn-p bits) (plausible-signed-imm32-operand-p bits)))
     (inst mov :qword (object-slot-ea vector vector-data-offset other-pointer-lowtag) bits))
   (move result vector)))

(define-vop (splat-small splat-word)
  (:arg-types * (:constant (integer 2 10)) (:constant t))
  (:temporary (:sc complex-double-reg) pattern)
  (:generator 5
   (let ((bits (compute-splat-bits value)))
     (cond ((and (eq bits null-tn) (eql words 2)) ; don't use XMM register
            (storew null-tn vector vector-data-offset other-pointer-lowtag)
            (storew null-tn vector (1+ vector-data-offset) other-pointer-lowtag))
           (t
            (cond ((eq bits null-tn)
                   (inst movaps pattern (ea (- list-pointer-lowtag) null-tn)))
                  ((/= bits 0) (inst movddup pattern (register-inline-constant :qword bits)))
                  (t (inst xorps pattern pattern)))
            (let ((data-addr (object-slot-ea vector vector-data-offset other-pointer-lowtag)))
              (multiple-value-bind (quo rem) (truncate words 2)
                (dotimes (i quo)
                  (inst movaps data-addr pattern) ; 1 byte shorter encoding than movapd
                  (setf data-addr (ea (+ (ea-disp data-addr) (* n-word-bytes 2))
                                      (ea-base data-addr))))
                (unless (zerop rem)
                  (inst movsd data-addr pattern)))))))
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
   (inst lea rdi (object-slot-ea vector vector-data-offset other-pointer-lowtag))
   (let ((bits (compute-splat-bits value)))
     (cond ((and (eql bits 0)
                 (constant-tn-p words)
                 (typep (tn-value words) '(unsigned-byte 7)))
            (zeroize rax)
            (inst lea :dword rcx (ea (tn-value words) rax))) ; smaller encoding
           (t
            ;; words could be in RAX, so read it first, then zeroize
            (inst mov rcx (or (and (constant-tn-p words) (tn-value words)) words))
            (if (eql bits 0) (zeroize rax) (inst mov rax bits)))))
   (inst rep)
   (inst stos :qword)
   (move result vector)))

(dolist (name '(splat-word splat-small splat-any))
  ;; It wants a function, not a symbol
  (setf (sb-c::vop-info-optimizer (template-or-lose name))
        (lambda (vop) (sb-c::elide-zero-fill vop))))
