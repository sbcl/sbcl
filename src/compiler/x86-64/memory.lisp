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

(defun gen-cell-set (ea value result &optional vop pseudo-atomic)
  (when pseudo-atomic
    ;; (SETF %FUNCALLABLE-INSTANCE-FUN) and (SETF %FUNCALLABLE-INSTANCE-INFO)
    ;; pass in pseudo-atomic = T.
    (pseudo-atomic ()
     (inst push (ea-base ea))
     (invoke-asm-routine 'call 'touch-gc-card vop)
     (gen-cell-set ea value result))
    (return-from gen-cell-set))
  (when (sc-is value immediate)
    (let ((bits (encode-value-if-immediate value)))
      (cond ((not result)
             ;; Try to move imm-to-mem if BITS fits
             (acond ((or (and (fixup-p bits)
                              ;; immobile-object fixups must fit in 32 bits
                              (eq (fixup-flavor bits) :immobile-symbol)
                              bits)
                         (plausible-signed-imm32-operand-p bits))
                     (inst mov :qword ea it))
                    (t
                     (inst mov temp-reg-tn bits)
                     (inst mov ea temp-reg-tn)))
             (return-from gen-cell-set))
            ;; Move the immediate value into RESULT provided that doing so
            ;; doesn't clobber EA. If it would, use TEMP-REG-TN instead.
            ;; TODO: if RESULT is unused, and the immediate fits in an
            ;; imm32 operand, then perform imm-to-mem move, but as the comment
            ;; observes, there's no easy way to spot an unused TN.
            ((or (location= (ea-base ea) result)
                 (awhen (ea-index ea) (location= it result)))
             (inst mov temp-reg-tn bits)
             (setq value temp-reg-tn))
            (t
             ;; Can move into RESULT, then into EA
             (inst mov result bits)
             (inst mov ea result)
             (return-from gen-cell-set)))))
  (inst mov :qword ea value) ; specify the size for when VALUE is an integer
  (when result
    ;; Ideally we would skip this move if RESULT is unused hereafter,
    ;; but unfortunately (NOT (TN-READS RESULT)) isn't equivalent
    ;; to there being no reads from the TN at all.
    (move result value)))

;;; CELL-REF and CELL-SET are used to define VOPs like CAR, where the
;;; offset to be read or written is a property of the VOP used.
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))
;; This vop's sole purpose is to be an ancestor for other vops, to assign
;; default operands, policy, and generator.
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg immediate)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (gen-cell-set (object-slot-ea object offset lowtag) value nil)))

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

;;; This logic was formerly in ALLOCATE-VECTOR-ON-STACK.
;;; Splitting it into 3 vops potentially gets better register allocation
;;; by not wasting registers in the cases that don't use them.
(define-vop (zero-fill-word)
  (:policy :fast-safe)
  (:translate zero-fill)
  (:args (vector :scs (descriptor-reg)))
  (:info words elidable)
  (:arg-types * (:constant (eql 1)) (:constant boolean))
  (:results (result :scs (descriptor-reg)))
  (:ignore elidable)
  (:generator 1
   (progn words) ; don't put it in :ignore, which gets inherited
   (inst mov :qword
         (ea (- (* vector-data-offset n-word-bytes) other-pointer-lowtag) vector)
         0)
   (move result vector)))

(define-vop (zero-fill-small zero-fill-word)
  (:arg-types * (:constant (integer 2 10)) (:constant boolean))
  (:temporary (:sc complex-double-reg) zero)
  (:generator 5
   (inst xorpd zero zero)
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

(define-vop (zero-fill-any zero-fill-word)
  ;; vector has to conflict with everything so that a tagged pointer
  ;; corresponding to RDI always exists
  (:args (vector :scs (descriptor-reg) :to (:result 0))
         (words :scs (unsigned-reg immediate) :target rcx))
  (:info elidable)
  (:arg-types * positive-fixnum (:constant boolean))
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
   (cond ((and (constant-tn-p words) (typep (tn-value words) '(unsigned-byte 7)))
          (zeroize rax)
          (inst lea :dword rcx (ea (tn-value words) rax))) ; smaller encoding
         (t
          ;; words could be in RAX, so read it first, then zeroize
          (inst mov rcx (or (and (constant-tn-p words) (tn-value words)) words))
          (zeroize rax)))
   (inst rep)
   (inst stos rax)
   (move result vector)))

(dolist (name '(zero-fill-word zero-fill-small zero-fill-any))
  ;; It wants a function, not a symbol
  (setf (sb-c::vop-info-optimizer (template-or-lose name))
        (lambda (vop) (sb-c::elide-zero-fill vop))))
