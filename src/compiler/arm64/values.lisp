;;;; unknown-values VOPs for the ARM VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-vop (reset-stack-pointer)
  (:args (ptr :scs (any-reg descriptor-reg control-stack)))
  (:generator 1
    (if (sc-is ptr control-stack)
        (load-stack-tn csp-tn ptr)
        (move csp-tn ptr))))

(define-vop (%%nip-values)
  (:args (last-nipped-ptr :scs (any-reg) :target dest)
         (last-preserved-ptr :scs (any-reg) :target src)
         (moved-ptrs :scs (any-reg) :more t))
  (:results (r-moved-ptrs :scs (any-reg) :more t))
  (:temporary (:sc any-reg) src)
  (:temporary (:sc any-reg) dest)
  (:temporary (:sc descriptor-reg) temp)
  (:ignore r-moved-ptrs)
  (:generator 1
    (move src last-preserved-ptr)
    (move dest last-nipped-ptr)
    (inst cmp csp-tn src)
    (inst b :le DONE)
    LOOP
    (loadw temp src)
    (inst add dest dest n-word-bytes)
    (inst add src src n-word-bytes)
    (storew temp dest -1)
    (inst cmp csp-tn src)
    (inst b :gt LOOP)
    DONE
    (move csp-tn dest)
    (inst sub src src dest)
    (loop for moved = moved-ptrs then (tn-ref-across moved)
          while moved
          do (sc-case (tn-ref-tn moved)
               ((descriptor-reg any-reg)
                (inst sub (tn-ref-tn moved) (tn-ref-tn moved) src))
               ((control-stack)
                (load-stack-tn temp (tn-ref-tn moved))
                (inst sub temp temp src)
                (store-stack-tn (tn-ref-tn moved) temp))))))

;;; Push some values onto the stack, returning the start and number of values
;;; pushed as results.  It is assumed that the Vals are wired to the standard
;;; argument locations.  Nvals is the number of values to push.
;;;
;;; The generator cost is pseudo-random.  We could get it right by defining a
;;; bogus SC that reflects the costs of the memory-to-memory moves for each
;;; operand, but this seems unworthwhile.
;;;
(define-vop (push-values)
  (:args (vals :more t :scs (descriptor-reg any-reg immediate control-stack constant)))
  (:results (start :scs (any-reg) :from :load)
            (count :scs (any-reg)))
  (:info nvals)
  (:temporary (:scs (descriptor-reg)) temp)
  (:vop-var vop)
  (:generator 20
    (unless (eq (tn-kind start) :unused)
      (move start csp-tn))
    (let (prev-constant)
      (flet ((load-tn (tn-ref)
               (let ((tn (tn-ref-tn tn-ref)))
                 (sc-case tn
                   ((descriptor-reg any-reg)
                    tn)
                   ((immediate constant)
                    (cond ((not (constant-tn-p tn))
                           (load-constant vop tn temp)
                           temp)
                          ((eql (tn-value tn) 0)
                           zr-tn)
                          ((or (eql prev-constant (tn-value tn))
                               (progn
                                 (setf prev-constant (tn-value tn))
                                 nil))
                           temp)
                          ((sc-is tn constant)
                           (load-constant vop tn temp)
                           temp)
                          (t
                           (load-immediate vop tn temp)
                           temp)))
                   (control-stack
                    (setf prev-constant nil)
                    (load-stack-tn temp tn)
                    temp)))))
        (cond ((= nvals 1)
               (inst str (load-tn vals) (@ csp-tn n-word-bytes :post-index)))
              (t
               (do ((val vals (tn-ref-across val))
                    (i 0 (1+ i)))
                   ((null val))
                 (inst str (load-tn val) (@ csp-tn n-word-bytes :post-index)))))))
    (unless (eq (tn-kind count) :unused)
      (inst mov count (fixnumize nvals)))))

;;; Push a list of values on the stack, returning Start and Count as used in
;;; unknown values continuations.
(define-vop (values-list)
  (:args (arg :scs (descriptor-reg) :target list))
  (:arg-refs arg-ref)
  (:policy :fast-safe)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) list)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:vop-var vop)
  (:node-var node)
  (:save-p :compute-only)
  (:generator 0
    (move list arg)

    (unless (eq (tn-kind start) :unused)
      (move start csp-tn))
    (when (and (policy node (> safety 0))
               (not (csubtypep (tn-ref-type arg-ref) (specifier-type 'list))))
      (inst b type-check))

    LOOP
    (inst cmp list null-tn)
    (loadw temp list cons-car-slot list-pointer-lowtag)
    (inst b :eq DONE)
    (loadw list list cons-cdr-slot list-pointer-lowtag)
    (inst str temp (@ csp-tn n-word-bytes :post-index))
    TYPE-CHECK
    (cond ((policy node (> safety 0))
           (test-type list ndescr LOOP nil (list-pointer-lowtag))
           (cerror-call vop 'bogus-arg-to-values-list-error list))
          (t
           (inst b LOOP)))

    DONE
    (unless (eq (tn-kind count) :unused)
      (inst sub count csp-tn start)
      (inst asr count count (- word-shift n-fixnum-tag-bits)))))

(define-vop (sb-c::reverse-values-list)
  (:args (arg :scs (descriptor-reg) :target list)
         (length :scs (any-reg) :target count))
  (:arg-refs arg-ref)
  (:policy :fast-safe)
  (:results (start :scs (any-reg) :from (:argument 0))
            (count :scs (any-reg)))
  (:temporary (:scs (descriptor-reg) :from (:argument 0)) list)
  (:temporary (:scs (descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (non-descriptor-reg)) end)
  (:vop-var vop)
  (:node-var node)
  (:save-p :compute-only)
  (:generator 0
    (move list arg)

    (unless (eq (tn-kind start) :unused)
      (move start csp-tn))
    (unless (eq (tn-kind count) :unused)
      (move count length))

    (inst add csp-tn csp-tn (lsl length (- word-shift n-fixnum-tag-bits)))
    (inst sub end csp-tn n-word-bytes)
    (when (and (policy node (> safety 0))
               (not (csubtypep (tn-ref-type arg-ref) (specifier-type 'list))))
      (inst b type-check))
    LOOP
    (inst cmp list null-tn)
    (loadw temp list cons-car-slot list-pointer-lowtag)
    (inst b :eq DONE)
    (loadw list list cons-cdr-slot list-pointer-lowtag)
    (inst str temp (@ end (- n-word-bytes) :post-index))
    TYPE-CHECK
    (cond ((policy node (> safety 0))
           (test-type list ndescr LOOP nil (list-pointer-lowtag))
           (error-call vop 'bogus-arg-to-values-list-error list))
          (t
           (inst b LOOP)))
    DONE))

;;; Copy the more arg block to the top of the stack so we can use them
;;; as function arguments.
(define-vop (%more-arg-values)
  (:args (context :scs (descriptor-reg any-reg) :to :save)
         (num :scs (any-reg) :target count :to (:result 1)))
  (:arg-types * positive-fixnum)
  (:temporary (:sc descriptor-reg) temp)
  (:temporary (:sc any-reg) i)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:generator 20
    (cond ((eq (tn-kind count) :unused)
           (setf count num))
          (t
           (move count num)))
    (when (eq (tn-kind start) :unused)
      (setf start tmp-tn))
    (move start csp-tn)
    ;; Shift and check for zero in one go
    (inst adds i zr-tn (lsl count (- word-shift n-fixnum-tag-bits)))
    (inst b :eq DONE)
    (inst add csp-tn csp-tn i)
    LOOP
    (inst subs i i n-word-bytes)
    (inst ldr temp (@ context i))
    (inst str temp (@ start i))
    (inst b :ne LOOP)
    DONE))

(define-vop (%more-arg-values-skip)
  (:args (context :scs (descriptor-reg any-reg) :target src)
         (skip :scs (any-reg immediate))
         (num :scs (any-reg) :target count :to (:result 1)))
  (:temporary (:sc any-reg :from (:argument 0)) src)
  (:arg-types * positive-fixnum positive-fixnum)
  (:temporary (:sc descriptor-reg) temp)
  (:temporary (:sc any-reg) i)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:generator 20
    (sc-case skip
      (immediate
       (inst add src context (* (tn-value skip) n-word-bytes))
       (inst subs i num (fixnumize (add-sub-immediate (tn-value skip)))))
      (any-reg
       (inst add src context (lsl skip (- word-shift n-fixnum-tag-bits)))
       (inst subs i num skip)))
    (unless (eq (tn-kind count) :unused)
      (inst csel count zr-tn i :le))

    (when (eq (tn-kind start) :unused)
      (setf start tmp-tn))
    (move start csp-tn)

    (inst b :le DONE)
    LOOP
    (inst ldr temp (@ src 8 :post-index))
    (inst str temp (@ csp-tn 8 :post-index))
    (inst subs i i (fixnumize 1))
    (inst b :ne LOOP)
    DONE))
