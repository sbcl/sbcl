;;;; Undefined-function and closure trampoline definitions

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.

(in-package "SB-VM")

;;; It is arbitrary whether each of the next 4 routines is named ALLOC-something,
;;; exporting an additional entry named CONS-something, versus being named CONS-something
;;; and exporting ALLOC-something.  It just depends on which of those you would like
;;; to have to set and clear the low bit to match ALLOC-DISPATCH.
;;; Think of "cons" as meaning the generic sense of "consing", not as actually
;;; allocating conses, because fixed-sized non-cons object allocation may enter
;;; through the cons entry point. Cons cells *must* use that entry point.
(define-assembly-routine (alloc->rnn (:export cons->rnn)) ()
  (inst or :byte (ea 8 rsp-tn) 1)
  CONS->RNN
  (with-registers-preserved (c xmm)
    (inst mov rdi-tn (ea 16 rbp-tn))
    (inst call (make-fixup 'alloc-dispatch :assembly-routine))
    (inst mov (ea 16 rbp-tn) rax-tn))) ; result onto stack

#+avx2
(define-assembly-routine (alloc->rnn.avx2 (:export cons->rnn.avx2)) ()
  (inst or :byte (ea 8 rsp-tn) 1)
  CONS->RNN.AVX2
  (with-registers-preserved (c ymm)
    (inst mov rdi-tn (ea 16 rbp-tn))
    (inst call (make-fixup 'alloc-dispatch :assembly-routine))
    (inst mov (ea 16 rbp-tn) rax-tn))) ; result onto stack

(define-assembly-routine (alloc->r11 (:export cons->r11) (:return-style :none)) ()
  (inst or :byte (ea 8 rsp-tn) 1)
  CONS->R11
  (with-registers-preserved (c xmm :except r11-tn)
    (inst mov rdi-tn (ea 16 rbp-tn))
    (inst call (make-fixup 'alloc-dispatch :assembly-routine))
    (inst mov r11-tn rax-tn))
  (inst ret 8)) ; pop argument

#+avx2
(define-assembly-routine (alloc->r11.avx2 (:export cons->r11.avx2) (:return-style :none)) ()
  (inst or :byte (ea 8 rsp-tn) 1)
  CONS->R11.AVX2
  (with-registers-preserved (c ymm :except r11-tn)
    (inst mov rdi-tn (ea 16 rbp-tn))
    (inst call (make-fixup 'alloc-dispatch :assembly-routine))
    (inst mov r11-tn rax-tn))
  (inst ret 8)) ; pop argument

(define-assembly-routine (alloc-dispatch (:return-style :none)) ()
  ;; If RDI has a 0 in the low bit, then we're allocating cons cells.
  ;; A 1 bit signifies anything other than cons cells, and is equivalent
  ;; to "could this object consume large-object pages" in gencgc.
  (inst test :byte rdi-tn 1)
  (inst jmp :z (make-fixup "alloc_list" :foreign))
  (inst xor :byte rdi-tn 1) ; clear the bit
  (inst jmp  (make-fixup "alloc" :foreign)))

;;; These routines are for the deterministic consing profiler.
;;; The C support routine's argument is the return PC.
;;; FIXME: we're missing routines that preserve YMM. I guess nobody cares.
(define-assembly-routine (enable-alloc-counter) ()
  (with-registers-preserved (c xmm)
    (inst lea rdi-tn (ea 8 rbp-tn))
    (pseudo-atomic () (inst call (make-fixup "allocation_tracker_counted" :foreign)))))

(define-assembly-routine (enable-sized-alloc-counter) ()
  (with-registers-preserved (c xmm)
    (inst lea rdi-tn (ea 8 rbp-tn))
    (pseudo-atomic () (inst call (make-fixup "allocation_tracker_sized" :foreign)))))

(define-assembly-routine (undefined-tramp (:return-style :none))
    ((:temp rax descriptor-reg rax-offset))
  (inst pop (ea n-word-bytes rbp-tn))
  (emit-error-break nil cerror-trap (error-number-or-lose 'undefined-fun-error) (list rax))
  (inst push (ea n-word-bytes rbp-tn))
  (inst jmp (ea (- (* closure-fun-slot n-word-bytes) fun-pointer-lowtag) rax)))

#+win32
(define-assembly-routine
    (undefined-alien-tramp (:return-style :none))
    ()
  (error-call nil 'undefined-alien-fun-error rbx-tn))

#-win32
(define-assembly-routine
    (undefined-alien-tramp (:return-style :none))
    ()
  (inst push rax-tn) ; save registers in case we want to see the old values
  (inst push rbx-tn)
  ;; load RAX with the PC after the call site
  (inst mov rax-tn (ea 16 rsp-tn))
  ;; load RBX with the signed 32-bit immediate from the call instruction
  (inst movsx '(:dword :qword) rbx-tn (ea -4 rax-tn))
  ;; if at [PC-5] we see #x25 then it was a call with 32-bit mem addr
  ;; if ...              #xE8 then ...                32-bit offset
  (inst cmp :byte (ea -5 rax-tn) #x25)
  (inst jmp :e ABSOLUTE)
  (inst cmp :byte (ea -5 rax-tn) #xE8)
  (inst jmp :e RELATIVE)
  ;; failing those, assume RBX was valid. ("can't happen")
  (inst mov rbx-tn (ea rsp-tn)) ; restore pushed value of RBX
  (inst jmp trap)
  ABSOLUTE
  (inst lea rbx-tn (ea -8 rbx-tn))
  (inst jmp TRAP)
  RELATIVE
  (inst add rbx-tn rax-tn)
  TRAP
  (error-call nil 'undefined-alien-fun-error rbx-tn))

;;; the closure trampoline - entered when a global function is a closure
;;; and the function is called "by name" (normally, as when it is the
;;; head of a form) via an FDEFN. Register %RAX holds the fdefn address,
;;; but the simple-fun which underlies the closure expects %RAX to be the
;;; closure itself. So we grab the closure out of the fdefn pointed to,
;;; then jump to the simple-fun that the closure points to.
;;;
;;; Immobile code uses a different strategy to call a closure that has been
;;; installed as a globally named function. The fdefn contains a jump opcode
;;; to a tiny code component specific to the particular closure.
;;; The trampoline is responsible for loading RAX, since named calls don't.
#-immobile-code
(define-assembly-routine
    (closure-tramp (:return-style :none))
    ()
  (loadw rax-tn rax-tn fdefn-fun-slot other-pointer-lowtag)
  (inst jmp (object-slot-ea rax-tn closure-fun-slot fun-pointer-lowtag)))

(define-assembly-routine
    (funcallable-instance-tramp (:return-style :none))
    ()
  (loadw rax-tn rax-tn funcallable-instance-function-slot fun-pointer-lowtag)
  (inst jmp (object-slot-ea rax-tn closure-fun-slot fun-pointer-lowtag)))

(define-assembly-routine (ensure-symbol-hash (:return-style :raw)) ()
  #+avx2
  (progn
    (test-cpu-feature cpu-has-ymm-registers)
    (inst jmp :z call-preserving-xmm-only)
    (with-registers-preserved (lisp ymm :except r11-tn)
      (inst mov rdx-tn (ea 16 rbp-tn)) ; arg
      (call-static-fun 'ensure-symbol-hash 1)
      (inst mov (ea 16 rbp-tn) rdx-tn)) ; result to arg passing loc
    (inst ret))
  call-preserving-xmm-only
  (with-registers-preserved (lisp xmm :except r11-tn)
    (inst mov rdx-tn (ea 16 rbp-tn)) ; arg
    (call-static-fun 'ensure-symbol-hash 1)
    (inst mov (ea 16 rbp-tn) rdx-tn))) ; result to arg passing loc

(define-assembly-routine (sb-impl::install-hash-table-lock
                          (:return-style :raw))
  ()
  #+avx2
  (progn
    (test-cpu-feature cpu-has-ymm-registers)
    (inst jmp :z call-preserving-xmm-only)
    (with-registers-preserved (lisp ymm :except r11-tn)
      (inst mov rdx-tn (ea 16 rbp-tn)) ; arg
      (call-static-fun 'sb-impl::install-hash-table-lock 1)
      (inst mov (ea 16 rbp-tn) rdx-tn)) ; result to arg passing loc
    (inst ret))
  call-preserving-xmm-only
  (with-registers-preserved (lisp xmm :except r11-tn)
    (inst mov rdx-tn (ea 16 rbp-tn)) ; arg
    (call-static-fun 'sb-impl::install-hash-table-lock 1)
    (inst mov (ea 16 rbp-tn) rdx-tn))) ; result to arg passing loc

(define-assembly-routine (invalid-layout-trap (:return-style :none)) ()
  #+avx2
  (progn
    (test-cpu-feature cpu-has-ymm-registers)
    (inst jmp :z call-preserving-xmm-only)
    (with-registers-preserved (lisp ymm :except r11-tn)
      (inst mov rdx-tn (ea 16 rbp-tn)) ; arg
      (call-static-fun 'update-object-layout 1)
      (inst mov (ea 16 rbp-tn) rdx-tn)) ; result to arg passing loc
    (inst ret)) ; Caller pops the result
  call-preserving-xmm-only
  (with-registers-preserved (lisp xmm :except r11-tn)
    (inst mov rdx-tn (ea 16 rbp-tn)) ; arg
    (call-static-fun 'update-object-layout 1)
    (inst mov (ea 16 rbp-tn) rdx-tn)) ; result to arg passing loc
  (inst ret)) ;  Caller pops the result
