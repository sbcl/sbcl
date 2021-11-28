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
    (move esp-tn ptr)))

(define-vop (%%nip-values)
  (:args (last-nipped-ptr :scs (any-reg) :target edi)
         (last-preserved-ptr :scs (any-reg) :target esi)
         (moved-ptrs :scs (any-reg) :more t))
  (:results (r-moved-ptrs :scs (any-reg) :more t)
            ;; same as MOVED-PTRS
            )
  (:temporary (:sc any-reg :offset esi-offset) esi)
  (:temporary (:sc any-reg :offset edi-offset) edi)
  (:temporary (:sc descriptor-reg) temp-dword)
  (:ignore r-moved-ptrs)
  (:generator 1
    (move edi last-nipped-ptr)
    (move esi last-preserved-ptr)
    (inst sub esi n-word-bytes)
    (inst sub edi n-word-bytes)
    (inst cmp esp-tn esi)
    (inst jmp :a done)
    LOOP
    (inst mov temp-dword (make-ea :dword :base esi))
    (inst mov (make-ea :dword :base edi) temp-dword)
    (inst sub esi n-word-bytes)
    (inst sub edi n-word-bytes)
    (inst cmp esp-tn esi)
    (inst jmp :be loop)
    DONE
    (inst lea esp-tn (make-ea :dword :base edi :disp n-word-bytes))
    (inst sub edi esi)
    (loop for moved = moved-ptrs then (tn-ref-across moved)
          while moved
          do (inst add (tn-ref-tn moved) edi))))

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
    (move start esp-tn)                 ; WARN pointing 1 below
    (do ((val vals (tn-ref-across val)))
        ((null val))
      (inst push (encode-value-if-immediate (tn-ref-tn val))))
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
  (:temporary (:sc unsigned-reg :offset eax-offset :to (:result 1)) eax)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 0
    (move list arg)
    (move start esp-tn)                 ; WARN pointing 1 below
    (inst mov nil-temp nil-value)

    LOOP
    (inst cmp list nil-temp)
    (inst jmp :e done)
    (pushw list cons-car-slot list-pointer-lowtag)
    (loadw list list cons-cdr-slot list-pointer-lowtag)
    (inst mov eax list)
    (inst and al-tn lowtag-mask)
    (inst cmp al-tn list-pointer-lowtag)
    (inst jmp :e loop)
    (cerror-call vop 'bogus-arg-to-values-list-error list)

    DONE
    (inst mov count start)              ; start is high address
    (inst sub count esp-tn)))           ; stackp is low address

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
         (num :scs (any-reg) :target count))
  (:arg-types * positive-fixnum)
  (:temporary (:sc any-reg :offset esi-offset :from (:argument 0)) src)
  (:temporary (:sc descriptor-reg :offset eax-offset) temp)
  (:temporary (:sc unsigned-reg :offset ecx-offset) loop-index)
  (:results (start :scs (any-reg))
            (count :scs (any-reg)))
  (:generator 20
    (move src context)
    (move count num)

    (move loop-index count)
    (inst mov start esp-tn)
    (inst test ecx-tn ecx-tn)
    (inst jmp :z done)  ; check for 0 count?

    (inst sub esp-tn count)
    (inst sub src count)

    LOOP
    (inst mov temp (make-ea :dword :base src :index loop-index))
    (inst sub loop-index n-word-bytes)
    (inst mov (make-ea :dword :base esp-tn :index loop-index) temp)
    (inst jmp :nz LOOP)

    DONE
    ))

