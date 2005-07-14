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

(in-package "SB!VM")

(define-assembly-routine (allocate-vector
                          (:policy :fast-safe)
                          (:translate allocate-vector)
                          (:arg-types positive-fixnum
                                      positive-fixnum
                                      positive-fixnum))
                         ((:arg type any-reg a0-offset)
                          (:arg length any-reg a1-offset)
                          (:arg words any-reg a2-offset)
                          (:res result descriptor-reg a0-offset)

                          (:temp ndescr non-descriptor-reg nl0-offset)
                          (:temp pa-flag non-descriptor-reg nl4-offset))
  ;; This is kinda sleezy, changing words like this.  But we can because
  ;; the vop thinks it is temporary.
  (inst addu words (+ (1- (ash 1 n-lowtag-bits))
                      (* vector-data-offset n-word-bytes)))
  (inst li ndescr (lognot lowtag-mask))
  (inst and words ndescr)
  (inst srl ndescr type word-shift)

  (pseudo-atomic (pa-flag)
    (inst or result alloc-tn other-pointer-lowtag)
    (inst addu alloc-tn words)
    (storew ndescr result 0 other-pointer-lowtag)
    (storew length result vector-length-slot other-pointer-lowtag)))
