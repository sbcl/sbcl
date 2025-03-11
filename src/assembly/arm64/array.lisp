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

(macrolet ((def (name sys)
             `(define-assembly-routine (,name
                                        (:policy :fast-safe)
                                        (:arg-types positive-fixnum
                                                    positive-fixnum
                                                    positive-fixnum))
                  ((:arg type any-reg r2-offset)
                   (:arg length any-reg r1-offset)
                   (:arg words any-reg r0-offset)
                   (:res result descriptor-reg r0-offset)

                   (:temp ndescr non-descriptor-reg nl2-offset)
                   (:temp pa-flag non-descriptor-reg nl3-offset)
                   (:temp lra-save non-descriptor-reg nl5-offset)
                   (:temp lr non-descriptor-reg lr-offset))
                (pseudo-atomic (pa-flag)
                  (inst lsl ndescr words (- word-shift n-fixnum-tag-bits))
                  (inst add ndescr ndescr (* (1+ vector-data-offset) n-word-bytes))
                  (inst and ndescr ndescr (bic-mask lowtag-mask)) ; double-word align
                  (move lra-save lr) ;; The call to alloc_tramp will overwrite LR
                  (allocation nil ndescr other-pointer-lowtag result
                              :flag-tn pa-flag :systemp ,sys)

                  (move lr lra-save)
                  (inst lsr ndescr type n-fixnum-tag-bits)
                  ;; Touch the last element, to ensure that null-terminated strings
                  ;; passed to C do not cause a WP violation in foreign code.
                  ;; Do that before storing length, since nil-arrays don't have any
                  ;; space, but may have non-zero length.
                  #-generational
                  (storew zr-tn pa-flag -1)
                  (storew-pair ndescr 0 length vector-length-slot tmp-tn)))))
  (def allocate-vector-on-heap nil)
  (def sys-allocate-vector-on-heap t))

(define-assembly-routine (allocate-vector-on-stack
                          (:policy :fast-safe)
                          (:arg-types positive-fixnum
                                      positive-fixnum
                                      positive-fixnum))
    ((:arg type any-reg r0-offset)
     (:arg length any-reg r1-offset)
     (:arg words any-reg r2-offset)
     (:res result descriptor-reg r0-offset)

     (:temp temp non-descriptor-reg nl0-offset)
     (:temp bytes non-descriptor-reg nl2-offset))
  (inst lsr temp type n-fixnum-tag-bits)
  (inst lsl bytes words (- word-shift n-fixnum-tag-bits))
  (inst add bytes bytes (* (1+ vector-data-offset) n-word-bytes))
  (inst and bytes bytes (bic-mask lowtag-mask)) ; double-word align

  (generate-stack-overflow-check nil bytes)
  (allocation nil bytes other-pointer-lowtag result :stack-allocate-p t :systemp nil)

  (inst stp temp length (@ tmp-tn))
  ;; Zero fill
  ;; The header word has already been set, skip it.
  (inst add temp tmp-tn (* n-word-bytes 2))
  (inst add bytes tmp-tn bytes)
  LOOP
  (inst stp zr-tn zr-tn (@ temp (* n-word-bytes 2) :post-index))
  (inst cmp temp bytes)
  (inst b :lt LOOP))

(define-assembly-routine (allocate-vector-on-number-stack
                          (:policy :fast-safe)
                          (:arg-types positive-fixnum
                                      positive-fixnum
                                      positive-fixnum))
    ((:arg type any-reg r0-offset)
     (:arg length any-reg r1-offset)
     (:arg words any-reg r2-offset)
     (:res result descriptor-reg r0-offset)

     (:temp temp non-descriptor-reg nl0-offset))
  (inst lsr temp type n-fixnum-tag-bits)
  (inst lsl words words (- word-shift n-fixnum-tag-bits))
  (inst add words words (* (1+ vector-data-offset) n-word-bytes))
  (inst and words words (bic-mask lowtag-mask)) ; double-word align

  (inst sub nsp-tn nsp-tn (extend words :lsl 0)) ;; to make go to SP instead of ZR.
  (inst mov-sp tmp-tn nsp-tn)
  (inst add result tmp-tn other-pointer-lowtag)

  ;; Zero fill
  (inst add words tmp-tn words)
  LOOP
  (inst stp zr-tn zr-tn (@ words (- (* n-word-bytes 2)) :pre-index))
  (inst cmp words tmp-tn)
  (inst b :gt LOOP)
  (inst stp temp length (@ tmp-tn)))

(define-assembly-routine (%data-vector-and-index
                          (:translate %data-vector-and-index)
                          (:policy :fast-safe)
                          (:arg-types t positive-fixnum)
                          (:result-types t positive-fixnum))
    ((:arg array descriptor-reg r0-offset)
     (:arg index any-reg r1-offset)
     (:res result descriptor-reg r0-offset)
     (:res offset any-reg r1-offset))
  (declare (ignore result offset))
  LOOP
  (inst ldrb tmp-tn (@ array (- other-pointer-lowtag)))

  (inst cmp tmp-tn simple-array-widetag)
  (inst b :eq SKIP)
  (inst cmp tmp-tn complex-base-string-widetag)
  (inst b :lt DONE)
  SKIP

  (loadw tmp-tn array array-displacement-slot other-pointer-lowtag)
  (inst add index index tmp-tn)
  (loadw array array array-data-slot other-pointer-lowtag)
  (inst b LOOP)
  DONE)

(define-assembly-routine (%data-vector-and-index/check-bound
                          (:translate %data-vector-and-index/check-bound)
                          (:policy :fast-safe)
                          (:arg-types t positive-fixnum)
                          (:result-types t positive-fixnum))
    ((:arg array descriptor-reg r0-offset)
     (:arg index any-reg r1-offset)
     (:res result descriptor-reg r0-offset)
     (:res offset any-reg r1-offset))
  (declare (ignore result offset))
  (let ((error
          (assemble (:elsewhere)
            error
            ;; Fake up a stack frame so that backtraces come out right.
            (inst mov ocfp-tn cfp-tn)
            (inst mov cfp-tn csp-tn)
            (inst stp ocfp-tn lr-tn (@ csp-tn 16 :post-index))
            (emit-error-break nil error-trap
                              (error-number-or-lose 'invalid-array-index-error)
                              (list array tmp-tn index))
            (progn error))))
    (assemble ()

      (inst ldrb tmp-tn (@ array (- other-pointer-lowtag)))
      (inst cmp tmp-tn simple-array-widetag)
      (inst b :eq HEADER)
      (inst cmp tmp-tn complex-base-string-widetag)
      (inst b :ge HEADER)

      (loadw tmp-tn array array-fill-pointer-slot other-pointer-lowtag)
      (inst cmp tmp-tn index)
      (inst b :ls ERROR)
      (inst b DONE)

      HEADER
      (loadw tmp-tn array array-elements-slot other-pointer-lowtag)
      (inst cmp tmp-tn index)
      (inst b :ls error)

      LOOP
      (loadw tmp-tn array array-displacement-slot other-pointer-lowtag)
      (inst add index index tmp-tn)
      (loadw array array array-data-slot other-pointer-lowtag)


      (inst ldrb tmp-tn (@ array (- other-pointer-lowtag)))
      (inst cmp tmp-tn simple-array-widetag)
      (inst b :eq LOOP)
      (inst cmp tmp-tn complex-base-string-widetag)
      (inst b :ge LOOP)

      DONE)))

(define-assembly-routine (%data-vector-pop
                          (:translate %data-vector-pop)
                          (:policy :fast-safe)
                          (:arg-types t)
                          (:result-types t positive-fixnum))
    ((:arg array descriptor-reg r0-offset)
     (:res result descriptor-reg r0-offset)
     (:res offset any-reg r1-offset))
  (declare (ignore result))
  (let ((error
          (assemble (:elsewhere)
            error
            ;; Fake up a stack frame so that backtraces come out right.
            (inst mov ocfp-tn cfp-tn)
            (inst mov cfp-tn csp-tn)
            (inst stp ocfp-tn lr-tn (@ csp-tn 16 :post-index))
            (emit-error-break nil error-trap
                              (error-number-or-lose 'fill-pointer-error)
                              (list array))
            (progn error))))
    (assemble ()
      (inst ldr tmp-tn (@ array (- other-pointer-lowtag)))
      (inst tbnz tmp-tn (1- (integer-length (ash sb-vm:+array-fill-pointer-p+
                                                 sb-vm:array-flags-data-position)))
            error)
      (inst and tmp-tn tmp-tn #xFF)
      (inst cmp tmp-tn complex-base-string-widetag)
      (inst b :lt ERROR)

      (loadw offset array array-fill-pointer-slot other-pointer-lowtag)
      (inst cbz offset error)
      (inst sub offset offset (fixnumize 1))
      (storew offset array array-fill-pointer-slot other-pointer-lowtag)

      LOOP
      (loadw tmp-tn array array-displacement-slot other-pointer-lowtag)
      (inst add offset offset tmp-tn)
      (loadw array array array-data-slot other-pointer-lowtag)


      (inst ldrb tmp-tn (@ array (- other-pointer-lowtag)))
      (inst cmp tmp-tn simple-array-widetag)
      (inst b :eq LOOP)
      (inst cmp tmp-tn complex-base-string-widetag)
      (inst b :ge LOOP)

      DONE)))

(define-assembly-routine (%data-vector-push
                          (:translate %data-vector-push)
                          (:policy :fast-safe)
                          (:arg-types t)
                          (:result-types t t))
    ((:arg array descriptor-reg r0-offset)
     (:res result descriptor-reg r0-offset)
     (:res offset descriptor-reg r1-offset))
  (declare (ignore result))
  (let ((error
          (assemble (:elsewhere)
            error
            ;; Fake up a stack frame so that backtraces come out right.
            (inst mov ocfp-tn cfp-tn)
            (inst mov cfp-tn csp-tn)
            (inst stp ocfp-tn lr-tn (@ csp-tn 16 :post-index))
            (emit-error-break nil error-trap
                              (error-number-or-lose 'fill-pointer-error)
                              (list array))
            (progn error))))
    (assemble ()

      (inst ldr tmp-tn (@ array (- other-pointer-lowtag)))
      (inst tbnz tmp-tn (1- (integer-length (ash sb-vm:+array-fill-pointer-p+
                                                 sb-vm:array-flags-data-position)))
            error)
      (inst and tmp-tn tmp-tn #xFF)
      (inst cmp tmp-tn complex-base-string-widetag)
      (inst b :lt ERROR)

      (loadw offset array array-fill-pointer-slot other-pointer-lowtag)
      (loadw tmp-tn array array-elements-slot other-pointer-lowtag)
      (inst cmp tmp-tn offset)
      (inst b :ne SKIP)
      (inst mov offset null-tn)
      (inst ret)

      SKIP
      (inst add tmp-tn offset (fixnumize 1))
      (storew tmp-tn array array-fill-pointer-slot other-pointer-lowtag)

      LOOP
      (loadw tmp-tn array array-displacement-slot other-pointer-lowtag)
      (inst add offset offset tmp-tn)
      (loadw array array array-data-slot other-pointer-lowtag)


      (inst ldrb tmp-tn (@ array (- other-pointer-lowtag)))
      (inst cmp tmp-tn simple-array-widetag)
      (inst b :eq LOOP)
      (inst cmp tmp-tn complex-base-string-widetag)
      (inst b :ge LOOP)

      DONE)))
