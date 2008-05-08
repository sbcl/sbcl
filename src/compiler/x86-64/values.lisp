;;;; unknown-values VOPs for the x86 VM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(define-vop (reset-stack-pointer)
  (:args (ptr :scs (any-reg)))
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
  (:ignore r-moved-ptrs)
  (:generator 1
    (move rdi last-nipped-ptr)
    (move rsi last-preserved-ptr)
    (inst sub rsi n-word-bytes)
    (inst sub rdi n-word-bytes)
    (inst cmp rsp-tn rsi)
    (inst jmp :a DONE)
    (inst std)
    LOOP
    (inst movs :qword)
    (inst cmp rsp-tn rsi)
    (inst jmp :be LOOP)
    (inst cld)
    DONE
    (inst lea rsp-tn (make-ea :qword :base rdi :disp n-word-bytes))
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
  (:args (vals :more t))
  (:temporary (:sc unsigned-reg :to (:result 0) :target start) temp)
  (:results (start) (count))
  (:info nvals)
  (:generator 20
    (move temp rsp-tn)                  ; WARN pointing 1 below
    (do ((val vals (tn-ref-across val)))
        ((null val))
      (inst push (tn-ref-tn val)))
    (move start temp)
    (inst mov count (fixnumize nvals))))

;;; Push a list of values on the stack, returning Start and Count as used in
;;; unknown values continuations.
(define-vop (values-list)
  (:args (arg :scs (descriptor-reg) :target list))
  (:arg-types list)
  (:policy :fast-safe)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:temporary (:sc descriptor-reg :from (:argument 0) :to (:result 1)) list)
  (:temporary (:sc descriptor-reg :to (:result 1)) nil-temp)
  (:temporary (:sc unsigned-reg :offset rax-offset :to (:result 1)) rax)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (move list arg)
    (move start rsp-tn)                 ; WARN pointing 1 below
    (inst mov nil-temp nil-value)

    LOOP
    (inst cmp list nil-temp)
    (inst jmp :e DONE)
    (pushw list cons-car-slot list-pointer-lowtag)
    (loadw list list cons-cdr-slot list-pointer-lowtag)
    (inst mov rax list)
    (inst and al-tn lowtag-mask)
    (inst cmp al-tn list-pointer-lowtag)
    (inst jmp :e LOOP)
    (error-call vop 'bogus-arg-to-values-list-error list)

    DONE
    (inst mov count start)              ; start is high address
    (inst sub count rsp-tn)))           ; stackp is low address

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
         (skip :scs (any-reg immediate))
         (num :scs (any-reg) :target count))
  (:arg-types * positive-fixnum positive-fixnum)
  (:temporary (:sc any-reg :offset rsi-offset :from (:argument 0)) src)
  (:temporary (:sc descriptor-reg :offset rax-offset) temp)
  (:temporary (:sc unsigned-reg :offset rcx-offset) loop-index)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:generator 20
    (sc-case skip
      (immediate
       (cond ((zerop (tn-value skip))
              (move src context)
              (move count num))
             (t
              (inst lea src (make-ea :dword :base context
                                     :disp (- (* (tn-value skip)
                                                 n-word-bytes))))
              (move count num)
              (inst sub count (* (tn-value skip) n-word-bytes)))))

      (any-reg
       (move src context)
       (inst sub src skip)
       (move count num)
       (inst sub count skip)))

    (move loop-index count)
    (inst mov start rsp-tn)
    (inst jrcxz DONE)  ; check for 0 count?

    (inst sub rsp-tn count)
    (inst sub src count)

    LOOP
    (inst mov temp (make-ea :qword :base src :index loop-index))
    (inst sub loop-index n-word-bytes)
    (inst mov (make-ea :qword :base rsp-tn :index loop-index) temp)
    (inst jmp :nz LOOP)

    DONE))

