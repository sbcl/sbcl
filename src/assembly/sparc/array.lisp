;;;; support routines for arrays and vectors

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
                          (:temp gc-temp non-descriptor-reg nl1-offset)
                          (:temp vector descriptor-reg a3-offset))
  (pseudo-atomic (gc-temp)
    ;; boxed words == unboxed bytes
    (inst add ndescr words (* (1+ vector-data-offset) n-word-bytes))
    (inst andn ndescr 7)
    (allocation nil ndescr other-pointer-lowtag vector :temp-tn gc-temp)
    (inst srl ndescr type n-fixnum-tag-bits)
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
                          (:temp gc-temp non-descriptor-reg nl1-offset)
                          (:temp vector descriptor-reg a3-offset))
  (pseudo-atomic (gc-temp)
    ;; boxed words == unboxed bytes
    (inst add ndescr words (* (1+ vector-data-offset) n-word-bytes))
    (inst andn ndescr 7)
    (generate-stack-overflow-check nil ndescr gc-temp)
    (allocation nil ndescr other-pointer-lowtag vector :temp-tn gc-temp
                :stack-p t)
    ;; We're pseudo-atomic here, so we can do whatever is most convenient
    ;; to zeroize the array data. Clear two words at a time upward from VECTOR
    ;; until reaching the new CSP-TN value. Don't pre-check for 0 payload words,
    ;; just consider the first two words as part of the array data.
    (let ((loop (gen-label)))
      (inst add ndescr vector (- other-pointer-lowtag))
      (emit-label loop)
      (storew zero-tn ndescr 0 0) ; next store is in the branch delay slot
      (inst add ndescr ndescr (* n-word-bytes 2))
      (inst cmp ndescr csp-tn)
      (inst b :lt loop)
      (storew zero-tn ndescr -1 0)) ; -1 because ptr was already bumped
    (inst srl ndescr type n-fixnum-tag-bits)
    (storew ndescr vector 0 other-pointer-lowtag)
    (storew length vector vector-length-slot other-pointer-lowtag))
  (move result vector))
