;;;; linkage information for standard static functions, and
;;;; miscellaneous VOPs

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; LENGTH

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg control-stack) :target ptr))
  (:arg-types list)
  (:temporary (:sc unsigned-reg :offset rax-offset) rax)
  (:temporary (:sc descriptor-reg :from (:argument 0)) ptr)
  (:results (count :scs (any-reg)))
  (:result-types positive-fixnum)
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 40
    ;; Move OBJECT into a temp we can bash on, and initialize the count.
    (move ptr object)
    (zeroize count)
    ;; If we are starting with NIL, then it's really easy.
    (inst cmp ptr nil-value)
    (inst jmp :e DONE)
    ;; Note: we don't have to test to see whether the original argument is a
    ;; list, because this is a :fast-safe vop.
    LOOP
    ;; Get the CDR and boost the count.
    (loadw ptr ptr cons-cdr-slot list-pointer-lowtag)
    (inst add count (fixnumize 1))
    ;; If we hit NIL, then we are done.
    (inst cmp ptr nil-value)
    (inst jmp :e DONE)
    ;; Otherwise, check to see whether we hit the end of a dotted list. If
    ;; not, loop back for more.
    (%test-lowtag ptr rax LOOP nil list-pointer-lowtag)
    ;; It's dotted all right. Flame out.
    (error-call vop 'object-not-list-error ptr)
    ;; We be done.
    DONE))

(define-vop (fast-length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg control-stack) :target ptr))
  (:arg-types list)
  (:temporary (:sc descriptor-reg :from (:argument 0)) ptr)
  (:results (count :scs (any-reg)))
  (:result-types positive-fixnum)
  (:policy :fast)
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 30
    ;; Get a copy of OBJECT in a register we can bash on, and
    ;; initialize COUNT.
    (move ptr object)
    (zeroize count)
    ;; If we are starting with NIL, we be done.
    (inst cmp ptr nil-value)
    (inst jmp :e DONE)
    ;; Indirect the next cons cell, and boost the count.
    LOOP
    (loadw ptr ptr cons-cdr-slot list-pointer-lowtag)
    (inst add count (fixnumize 1))
    ;; If we aren't done, go back for more.
    (inst cmp ptr nil-value)
    (inst jmp :ne LOOP)
    DONE))
