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
     (:temp pa-flag non-descriptor-reg nl3-offset)
     (:temp lra-save non-descriptor-reg nl5-offset)
     (:temp vector descriptor-reg r8-offset)
     (:temp lr interior-reg lr-offset))
  (pseudo-atomic (pa-flag)
    (inst lsl ndescr words (- word-shift n-fixnum-tag-bits))
    (inst add ndescr ndescr (* (1+ vector-data-offset) n-word-bytes))
    (inst and ndescr ndescr (bic-mask lowtag-mask)) ; double-word align
    (move lra-save lr) ;; The call to alloc_tramp will overwrite LR
    (allocation vector ndescr other-pointer-lowtag :flag-tn pa-flag
                                                   :lip nil) ;; keep LR intact as per above

    (move lr lra-save)
    (inst lsr ndescr type n-fixnum-tag-bits)
    (storew ndescr vector 0 other-pointer-lowtag)
    ;; Touch the last element, to ensure that null-terminated strings
    ;; passed to C do not cause a WP violation in foreign code.
    ;; Do that before storing length, since nil-arrays don't have any
    ;; space, but may have non-zero length.
    #!-gencgc
    (storew zr-tn pa-flag -1)
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

     (:temp temp non-descriptor-reg nl0-offset))
  (pseudo-atomic (temp)
    (inst lsr temp type n-fixnum-tag-bits)
    (inst lsl words words (- word-shift n-fixnum-tag-bits))
    (inst add words words (* (1+ vector-data-offset) n-word-bytes))
    (inst and words words (bic-mask lowtag-mask)) ; double-word align
    (allocation result words nil :stack-allocate-p t)

    (inst stp temp length (@ result))
    ;; Zero fill
    (assemble ()
      ;; The header word has already been set, skip it.
      (inst add temp result (* n-word-bytes 2))
      (inst add words result words)
      LOOP
      (inst stp zr-tn zr-tn (@ temp (* n-word-bytes 2) :post-index))
      (inst cmp temp words)
      (inst b :lt LOOP))
    (inst orr result result other-pointer-lowtag)))
