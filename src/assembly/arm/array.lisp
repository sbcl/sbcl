;;;; various array operations that are too expensive (in space) to do
;;;; inline

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-assembly-routine (allocate-vector-on-heap
                          (:policy :fast-safe)
                          (:arg-types positive-fixnum
                                      positive-fixnum
                                      positive-fixnum))
    ((:arg type any-reg r0-offset)
     (:arg length any-reg r1-offset)
     (:arg words any-reg r2-offset)
     (:res result descriptor-reg r0-offset)

     (:temp ndescr non-descriptor-reg nl2-offset)
     (:temp pa-flag non-descriptor-reg ocfp-offset)
     (:temp vector descriptor-reg r8-offset))
  ;; Why :LINK NIL?
  ;; Either LR or PC need to always point into the code object.
  ;; Since this is a static assembly routine, PC is already not pointing there.
  ;; But it's called using blx, so LR is still good.
  ;; Normally PSEUDO-ATOMIC calls do_pending_interrupt using BLX too,
  ;; which will make LR point here, now GC can collect the parent function away.
  ;; But the call to do_pending_interrupt is at the end, and there's
  ;; nothing more needed to be done by the routine, so
  ;; do_pending_interrupt can return to the parent function directly.
  ;; This still uses the normal :return-style, BX LR, since the call
  ;; to do_pending_interrupt interrupt is conditional.
  (pseudo-atomic (pa-flag :link nil)
    ;; boxed words == unboxed bytes
    (inst add ndescr words (* (1+ vector-data-offset) n-word-bytes))
    (inst bic ndescr ndescr lowtag-mask)
    (allocation nil ndescr other-pointer-lowtag vector :flag-tn pa-flag)
    (inst mov ndescr (lsr type word-shift))
    (storew ndescr vector 0 other-pointer-lowtag)
    ;; Touch the last element, to ensure that null-terminated strings
    ;; passed to C do not cause a WP violation in foreign code.
    ;; Do that before storing length, since nil-arrays don't have any
    ;; space, but may have non-zero length.
    #-gencgc
    (inst mov ndescr 0)
    #-gencgc
    (storew ndescr pa-flag -1)
    (storew length vector vector-length-slot other-pointer-lowtag)
    (move result vector)))

(define-assembly-routine (allocate-vector-on-stack
                          (:policy :fast-safe)
                          (:arg-types positive-fixnum
                                      positive-fixnum
                                      positive-fixnum))
    ((:arg type any-reg r0-offset)
     (:arg length any-reg r1-offset)
     (:arg words any-reg r2-offset)
     (:res result descriptor-reg r0-offset)

     (:temp ndescr non-descriptor-reg nl2-offset)
     (:temp pa-flag non-descriptor-reg ocfp-offset)
     (:temp vector descriptor-reg r8-offset))
  ;; See why :LINK NIL is needed in ALLOCATE-VECTOR-ON-HEAP above.
  (pseudo-atomic (pa-flag :link nil)
    ;; boxed words == unboxed bytes
    (inst add ndescr words (* (1+ vector-data-offset) n-word-bytes))
    (inst bic ndescr ndescr lowtag-mask)
    (generate-stack-overflow-check nil ndescr pa-flag vector)
    (allocation nil ndescr other-pointer-lowtag vector
                :flag-tn pa-flag
                :stack-allocate-p t)
    (inst mov pa-flag (lsr type word-shift))
    (storew pa-flag vector 0 other-pointer-lowtag)
    ;; Zero fill
    (let ((loop (gen-label)))
      (inst sub result vector (- other-pointer-lowtag n-word-bytes))
      ;; The header word has already been set, skip it.
      (inst sub ndescr ndescr (fixnumize 1))
      (inst mov pa-flag 0)
      (emit-label loop)
      (inst str pa-flag (@ result n-word-bytes :post-index))
      (inst subs ndescr ndescr (fixnumize 1))
      (inst b :gt loop))
    (storew length vector vector-length-slot other-pointer-lowtag)
    (move result vector)))
