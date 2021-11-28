;;;; unknown-values VOPs for the x86 VM

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
  (:args (ptr :scs (any-reg control-stack)))
  (:generator 1
    (move rsp-tn ptr)))

(define-vop (%%nip-values)
  (:args (last-nipped-ptr :scs (any-reg) :target rdi)
         (last-preserved-ptr :scs (any-reg) :target rsi)
         (moved-ptrs :scs (any-reg) :more t))
  (:results (r-moved-ptrs :scs (any-reg) :more t)
            ;; same as MOVED-PTRS
            )
  (:temporary (:sc any-reg :offset rsi-offset) rsi)
  (:temporary (:sc any-reg :offset rdi-offset) rdi)
  (:temporary (:sc descriptor-reg) temp-qword)
  (:ignore r-moved-ptrs)
  (:generator 1
    (move rdi last-nipped-ptr)
    (move rsi last-preserved-ptr)
    (inst sub rsi n-word-bytes)
    (inst sub rdi n-word-bytes)
    (inst cmp rsp-tn rsi)
    (inst jmp :a DONE)
    LOOP
    (inst mov temp-qword (ea rsi))
    (inst mov (ea rdi) temp-qword)
    (inst sub rsi n-word-bytes)
    (inst sub rdi n-word-bytes)
    (inst cmp rsp-tn rsi)
    (inst jmp :be loop)
    DONE
    (inst lea rsp-tn (ea n-word-bytes rdi))
    (inst sub rdi rsi)
    (loop for moved = moved-ptrs then (tn-ref-across moved)
          while moved
          do (inst add (tn-ref-tn moved) rdi))))

;;; Push some values onto the stack, returning the start and number of values
;;; pushed as results. It is assumed that the Vals are wired to the standard
;;; argument locations. Nvals is the number of values to push.
;;;
;;; The generator cost is pseudo-random. We could get it right by defining a
;;; bogus SC that reflects the costs of the memory-to-memory moves for each
;;; operand, but this seems unworthwhile.
(define-vop (push-values)
  (:args (vals :more t
               :scs (descriptor-reg)))
  (:results (start :from :load) (count))
  (:info nvals)
  (:generator 20
    (unless (eq (tn-kind start) :unused)
      (move start rsp-tn))
    (do ((val vals (tn-ref-across val)))
        ((null val))
      (inst push (let ((value (encode-value-if-immediate (tn-ref-tn val))))
                   (if (integerp value)
                       (constantize value)
                       value))))
    (unless (eq (tn-kind count) :unused)
      (inst mov count (fixnumize nvals)))))

;;; Push a list of values on the stack, returning Start and Count as used in
;;; unknown values continuations.
(define-vop (values-list)
  (:args (arg :scs (descriptor-reg) :target list))
  (:arg-types list)
  (:policy :fast-safe)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:temporary (:sc descriptor-reg :from (:argument 0) :to (:result 1)) list)
  (:temporary (:sc unsigned-reg :offset rax-offset :to (:result 1)) rax)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (move list arg)

    (unless (eq (tn-kind start) :unused)
      (move start rsp-tn))               ; WARN pointing 1 below

    LOOP
    (inst cmp list nil-value)
    (inst jmp :e DONE)
    (pushw list cons-car-slot list-pointer-lowtag)
    (loadw list list cons-cdr-slot list-pointer-lowtag)
    (%test-lowtag list rax LOOP nil list-pointer-lowtag)
    (cerror-call vop 'bogus-arg-to-values-list-error list)

    DONE
    (unless (eq (tn-kind count) :unused)
      (inst mov count start)            ; start is high address
      (inst sub count rsp-tn)           ; stackp is low address
      #-#.(cl:if (cl:= sb-vm:word-shift sb-vm:n-fixnum-tag-bits) '(and) '(or))
      (inst shr count (- word-shift n-fixnum-tag-bits)))))

;;; Copy the more arg block to the top of the stack so we can use them
;;; as function arguments.
;;;
;;; Accepts a context as produced by more-arg-context; points to the first
;;; value on the stack, not 4 bytes above as in other contexts.
;;;
;;; Return a context that is 4 bytes above the first value, suitable for
;;; defining a new stack frame.
(define-vop (%more-arg-values)
  (:args (context :scs (descriptor-reg any-reg) :target src)
         (num :scs (any-reg) :target loop-index))
  (:arg-types * positive-fixnum)
  (:temporary (:sc any-reg :offset rsi-offset :from (:argument 0)) src)
  (:temporary (:sc descriptor-reg :offset rax-offset) temp)
  (:temporary (:sc unsigned-reg :offset rcx-offset :from (:argument 2)) loop-index)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:generator 20
    (move src context)
    (unless (eq (tn-kind count) :unused)
      (move count num))
    (if (location= loop-index num)
        (inst shl num (- word-shift n-fixnum-tag-bits))
        (inst lea loop-index (ea nil num (ash 1 (- word-shift n-fixnum-tag-bits)))))
    (unless (eq (tn-kind start) :unused)
      (inst mov start rsp-tn))
    (inst test rcx-tn rcx-tn)
    (inst jmp :z DONE)  ; check for 0 count?

    (inst sub rsp-tn loop-index)
    (inst sub src loop-index)

    LOOP
    (inst mov temp (ea src loop-index))
    (inst sub loop-index n-word-bytes)
    (inst mov (ea rsp-tn loop-index) temp)
    (inst jmp :nz LOOP)

    DONE))

