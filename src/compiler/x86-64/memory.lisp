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

(in-package "SB!VM")

;;; CELL-REF and CELL-SET are used to define VOPs like CAR, where the
;;; offset to be read or written is a property of the VOP used.
;;; CELL-SETF is similar to CELL-SET, but delivers the new value as
;;; the result. CELL-SETF-FUN takes its arguments as if it were a
;;; SETF function (new value first, as apposed to a SETF macro, which
;;; takes the new value last).
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
  (:generator 4
    (storew value object offset lowtag)))
(define-vop (cell-setf)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg) :target result))
  (:results (result :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)
    (move result value)))
(define-vop (cell-setf-fun)
  (:args (value :scs (descriptor-reg any-reg) :target result)
         (object :scs (descriptor-reg)))
  (:results (result :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)
    (move result value)))

;;; Define accessor VOPs for some cells in an object. If the operation
;;; name is NIL, then that operation isn't defined. If the translate
;;; function is null, then we don't define a translation.
(defmacro define-cell-accessors (offset lowtag
                                        ref-op ref-trans set-op set-trans)
  `(progn
     ,@(when ref-op
         `((define-vop (,ref-op cell-ref)
             (:variant ,offset ,lowtag)
             ,@(when ref-trans
                 `((:translate ,ref-trans))))))
     ,@(when set-op
         `((define-vop (,set-op cell-setf)
             (:variant ,offset ,lowtag)
             ,@(when set-trans
                 `((:translate ,set-trans))))))))

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
    (inst xadd (make-ea-for-object-slot object offset lowtag) result :lock)))

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
    (inst xadd (make-ea-for-object-slot object offset lowtag) result :lock)))

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
                          (inst lea newval
                                (make-ea :qword :base rax :disp const)))
                         (t
                          (inst mov newval const)
                          (inst add newval rax)))
                   ,(if (eq inherit 'cell-xsub)
                        `(progn (move newval rax)
                                (inst sub newval delta))
                        `(inst lea newval
                               (make-ea :qword :base rax :index delta))))
               (inst cmpxchg
                     (make-ea-for-object-slot cell ,slot list-pointer-lowtag)
                     newval :lock)
               (inst jmp :ne retry)
               (inst mov result rax)))))))
  (def-atomic %atomic-inc-car cell-xadd cons-car-slot)
  (def-atomic %atomic-inc-cdr cell-xadd cons-cdr-slot)
  (def-atomic %atomic-dec-car cell-xsub cons-car-slot)
  (def-atomic %atomic-dec-cdr cell-xsub cons-cdr-slot))

;;; SLOT-REF and SLOT-SET are used to define VOPs like CLOSURE-REF,
;;; where the offset is constant at compile time, but varies for
;;; different uses.
(define-vop (slot-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (loadw value object (+ base offset) lowtag)))
(define-vop (slot-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg immediate)))
  (:temporary (:sc unsigned-reg) temp)
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
     (if (sc-is value immediate)
         (let ((val (tn-value value)))
           (move-immediate (make-ea :qword :base object
                                    :disp (- (* (+ base offset) n-word-bytes)
                                             lowtag))
                           (etypecase val
                             (integer
                              (fixnumize val))
                             (symbol
                              (+ nil-value (static-symbol-offset val)))
                             (character
                              (logior (ash (char-code val) n-widetag-bits)
                                      character-widetag)))
                           temp))
         ;; Else, value not immediate.
         (storew value object (+ base offset) lowtag))))

(define-vop (slot-set-conditional)
  (:args (object :scs (descriptor-reg) :to :eval)
         (old-value :scs (descriptor-reg any-reg) :target eax)
         (new-value :scs (descriptor-reg any-reg) :target temp))
  (:temporary (:sc descriptor-reg :offset eax-offset
                   :from (:argument 1) :to :result :target result)  eax)
  (:temporary (:sc descriptor-reg :from (:argument 2) :to :result) temp)
  (:variant-vars base lowtag)
  (:results (result :scs (descriptor-reg)))
  (:info offset)
  (:generator 4
    (move eax old-value)
    (move temp new-value)
    (inst cmpxchg (make-ea :dword :base object
                           :disp (- (* (+ base offset) n-word-bytes) lowtag))
          temp)
    (move result eax)))

;;; X86 special
;;; FIXME: Figure out whether we should delete this.
;;; It looks just like 'cell-xadd' and is buggy in the same ways:
;;;   - modifies 'value' operand which *should* be in the same physical reg
;;;     as 'result' operand, but would cause harm if not.
;;;   - operand width needs to be :qword
;;;   - :LOCK is missing
(define-vop (slot-xadd)
  (:args (object :scs (descriptor-reg) :to :result)
         (value :scs (any-reg) :target result))
  (:results (result :scs (any-reg) :from (:argument 1)))
  (:result-types tagged-num)
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (move result value)
    (inst xadd (make-ea :dword :base object
                        :disp (- (* (+ base offset) n-word-bytes) lowtag))
          value)))
