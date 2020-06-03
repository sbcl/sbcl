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
    ((:arg type any-reg a0-offset)
     (:arg length any-reg a1-offset)
     (:arg words any-reg a2-offset)
     (:res result descriptor-reg a0-offset)

     (:temp ndescr non-descriptor-reg nl0-offset)
     (:temp pa-flag non-descriptor-reg nl3-offset)
     (:temp vector descriptor-reg a3-offset)
     (:temp temp non-descriptor-reg nl2-offset))
  (pseudo-atomic (pa-flag)
    (inst addi ndescr words (fixnumize (1+ vector-data-offset)))
    (inst rldicr ndescr ndescr (- word-shift n-fixnum-tag-bits) (- 63 n-lowtag-bits))
    (allocation nil ndescr other-pointer-lowtag vector
                :temp-tn temp
                :flag-tn pa-flag)
    (inst srwi ndescr type n-fixnum-tag-bits)
    (storew ndescr vector 0 other-pointer-lowtag)
    (storew length vector vector-length-slot other-pointer-lowtag))
  (move result vector))

(define-assembly-routine (allocate-vector-on-stack
                          (:policy :fast-safe)
                          (:arg-types positive-fixnum
                                      positive-fixnum
                                      positive-fixnum))
    ((:arg type any-reg a0-offset)
     (:arg length any-reg a1-offset)
     (:arg words any-reg a2-offset)
     (:res result descriptor-reg a0-offset)

     (:temp ndescr non-descriptor-reg nl0-offset)
     (:temp zero non-descriptor-reg zero-offset)
     (:temp pa-flag non-descriptor-reg nl3-offset)
     (:temp vector descriptor-reg a3-offset)
     (:temp temp non-descriptor-reg nl2-offset))
  (pseudo-atomic (pa-flag)
    (inst addi ndescr words (fixnumize (1+ vector-data-offset)))
    (inst rldicr ndescr ndescr (- word-shift n-fixnum-tag-bits) (- 63 n-lowtag-bits))
    (align-csp temp)
    (inst ori vector csp-tn other-pointer-lowtag)
    (inst add csp-tn csp-tn ndescr)
    (inst srwi temp type n-fixnum-tag-bits)
    (storew temp vector 0 other-pointer-lowtag)
    ;; Our storage is allocated, but not initialized, and our contract
    ;; calls for it to be zero-fill.  Do so now.
    (inst li zero 0)
    (let ((loop (gen-label)))
      (inst addi temp vector (- n-word-bytes other-pointer-lowtag))
      ;; The header word has already been set, skip it.
      (inst addi ndescr ndescr (- (fixnumize 1)))
      (emit-label loop)
      (inst addic. ndescr ndescr (- (fixnumize 1)))
      (storew zero temp 0)
      (inst addi temp temp n-word-bytes)
      (inst bgt loop))
    ;; Our zero-fill loop always executes at least one store, so to
    ;; ensure that there is at least one slot available to be
    ;; clobbered, we defer setting the vector-length slot until now.
    (storew length vector vector-length-slot other-pointer-lowtag))
  (move result vector))
