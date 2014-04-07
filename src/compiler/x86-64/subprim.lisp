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

(in-package "SB!VM")

;;;; LENGTH

(define-vop (length/list)
  (:translate length)
  (:args (object :scs (descriptor-reg control-stack) :target ptr))
  (:arg-types list)
  (:temporary (:sc dword-reg :offset eax-offset) eax)
  (:temporary (:sc descriptor-reg :from (:argument 0)) ptr)
  (:results (count :scs (any-reg)))
  (:result-types positive-fixnum)
  (:policy :fast-safe)
  (:vop-var vop)
  (:save-p :compute-only)
  (:ignore eax)
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
    (%test-lowtag ptr LOOP nil list-pointer-lowtag)
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

(define-static-fun length (object) :translate length)
(define-static-fun %coerce-callable-to-fun (callable)
  :translate %coerce-callable-to-fun)

(defun subprimitive-tls-allocator (tn)
  (make-fixup (ecase (tn-offset tn)
                (#.rax-offset 'alloc-tls-index-in-rax)
                (#.rcx-offset 'alloc-tls-index-in-rcx)
                (#.rdx-offset 'alloc-tls-index-in-rdx)
                (#.rbx-offset 'alloc-tls-index-in-rbx)
                (#.rsi-offset 'alloc-tls-index-in-rsi)
                (#.rdi-offset 'alloc-tls-index-in-rdi)
                (#.r8-offset  'alloc-tls-index-in-r8)
                (#.r9-offset  'alloc-tls-index-in-r9)
                (#.r10-offset 'alloc-tls-index-in-r10)
                (#.r12-offset 'alloc-tls-index-in-r12)
                (#.r13-offset 'alloc-tls-index-in-r13)
                (#.r14-offset 'alloc-tls-index-in-r14)
                (#.r15-offset 'alloc-tls-index-in-r15))
              :assembly-routine))

;; Make sure that SYMBOL has a TLS-INDEX, and return that.
;; It would be nice to have GC help recycle TLS indices.
;; We can use the TLS area itself as a linked list of free cells, each
;; storing the index of the next free cell. GC can push back into
;; the so-represented list when it trashes a symbol.
;; In addition to the GC complication, PROGV would need to be both
;; cas-lock-protected and pseudo-atomic most likely.
;;
#!+sb-thread ; no SYMBOL-TLS-INDEX-SLOT without threads
(define-vop (ensure-symbol-tls-index)
  (:translate ensure-symbol-tls-index)
  (:args (symbol :scs (descriptor-reg) :to (:result 1)))
  (:results (tls-index :scs (descriptor-reg any-reg)))
  (:temporary (:sc unsigned-reg) tmp)
  (:policy :fast-safe)
  (:generator 10
    (inst mov (reg-in-size tls-index :dword) (tls-index-of symbol))
    (inst test tls-index tls-index)
    (inst jmp :ne TLS-INDEX-VALID)
    (move tls-index symbol)
    (inst mov tmp (subprimitive-tls-allocator tls-index))
    (inst call tmp)
    TLS-INDEX-VALID
    (inst shl tls-index n-fixnum-tag-bits)))
