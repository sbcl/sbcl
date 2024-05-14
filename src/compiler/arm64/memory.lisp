;;;; the ARM definitions of some general purpose memory reference VOPs
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

(defun emit-gengc-barrier (object cell-address temp &optional value-tn-ref value-tn allocator)
  (multiple-value-bind (require #+debug-gc-barriers why-not)
      (or (eq value-tn-ref t)
          (require-gengc-barrier-p object value-tn-ref value-tn allocator))
    (cond (require
           (inst ubfm temp (or cell-address object) gencgc-card-shift (make-fixup nil :card-table-index-mask))
           (inst strb zr-tn (@ cardtable-tn temp)))
          #+debug-gc-barriers
          (t
           (labels ((encode (x)
                      (cond ((integerp x)
                             (inst mov temp (fixnumize x))
                             temp)
                            (t
                             (sc-case x
                               (constant
                                (load-constant nil x temp)
                                temp)
                               (control-stack
                                (load-stack-tn temp x)
                                temp)
                               (t
                                x)))))
                    (stack-push (x)
                      x
                      (inst str (encode x) (@ csp-tn n-word-bytes :post-index))))
             (cond (value-tn
                    (unless (sc-is value-tn immediate)
                      (stack-push (if (eq why-not :consecutive)
                                      zr-tn
                                      55))
                      (stack-push object)
                      (stack-push value-tn)
                      (invoke-asm-routine 'check-barrier temp)))
                   (value-tn-ref
                    (loop do
                          (unless (sc-is (tn-ref-tn value-tn-ref) immediate)
                            (stack-push (if (eq why-not :consecutive)
                                            zr-tn
                                            55))
                            (stack-push object)
                            (stack-push (tn-ref-tn value-tn-ref))
                            (invoke-asm-routine 'check-barrier temp))
                          (setf value-tn-ref (tn-ref-across value-tn-ref))
                          while value-tn-ref))))))))

;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the
;;; offset to be read or written is a property of the VOP used.

(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))

(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (emit-gengc-barrier object nil tmp-tn (vop-nth-arg 1 vop) value)
    (storew value object offset lowtag)))

;;;
(define-vop (word-index-cas)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (old-value :scs (any-reg descriptor-reg zero))
         (new-value :scs (any-reg descriptor-reg zero)))
  (:arg-types * tagged-num * *)
  (:temporary (:sc non-descriptor-reg) lip)
  (:results (result :scs (any-reg descriptor-reg) :from :load))
  (:result-types *)
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (cond
      ((sc-is index immediate)
       (inst add-sub lip object (- (ash (+ (tn-value index) offset) word-shift)
                                   lowtag)))
      (t
       (inst add lip object (lsl index (- word-shift n-fixnum-tag-bits)))
       (inst add-sub lip lip (- (* offset n-word-bytes) lowtag))))
    ;; Avoid emitting a barrier for %RAW-INSTANCE-CAS/{SIGNED-}WORD
    ;; which inherits the generator of this vop.
    (when (sc-is new-value descriptor-reg)
      ;; LOWTAG determines whether to mark the card containing the object header
      ;; (if instance-pointer-lowtag) versus element (if other-pointer-lowtag)
      (emit-gengc-barrier object (if (eq lowtag other-pointer-lowtag) lip nil)
                          tmp-tn (vop-nth-arg 3 vop) new-value))
    (inst dsb)
    LOOP
    ;; If this were 'ldaxr' instead of 'ldxr' maybe we wouldn't need the 'dsb' ?
    (inst ldxr result lip)
    (inst cmp result old-value)
    (inst b :ne EXIT)
    (inst stlxr tmp-tn new-value lip)
    (inst cbnz (32-bit-reg tmp-tn) LOOP)
    EXIT
    (inst clrex)
    (inst dmb)))

(define-vop (word-index-cas-v8.1)
  (:args (object :scs (descriptor-reg) :to :save)
         (index :scs (any-reg immediate))
         (old-value :scs (any-reg descriptor-reg) :target result)
         (new-value :scs (any-reg descriptor-reg zero)))
  (:arg-types * tagged-num * *)
  (:temporary (:sc non-descriptor-reg) lip)
  (:results (result :scs (any-reg descriptor-reg) :from (:argument 2)))
  (:result-types *)
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:guard (member :arm-v8.1 *backend-subfeatures*))
  (:vop-var vop)
  (:generator 3
    (cond
      ((sc-is index immediate)
       (inst add-sub lip object (- (ash (+ (tn-value index) offset) word-shift)
                                   lowtag)))
      (t
       (inst add lip object (lsl index (- word-shift n-fixnum-tag-bits)))
       (inst add-sub lip lip (- (* offset n-word-bytes) lowtag))))
    ;; just like above
    (when (sc-is new-value descriptor-reg)
      (emit-gengc-barrier object (if (eq lowtag other-pointer-lowtag) lip nil)
                          tmp-tn (vop-nth-arg 3 vop) new-value))
    (move result old-value)
    (inst casal result new-value lip)))

#+sb-thread
(define-vop (set-instance-hashed-return-address)
  (:args (object :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) baseptr header)
  (:results (result :scs (unsigned-reg)))
  (:result-types unsigned-num)
  (:generator 5
    (inst sub baseptr object instance-pointer-lowtag)
    LOOP
    (cond ((member :arm-v8.1 *backend-subfeatures*)
           (inst movz header (ash 1 stable-hash-required-flag))
           (inst ldset header zr-tn baseptr))
          (t
           (inst ldaxr header baseptr)
           (inst orr header header (ash 1 stable-hash-required-flag))
           (inst stlxr tmp-tn header baseptr)
           (inst cbnz (32-bit-reg tmp-tn) LOOP)))
    (move result object)))
