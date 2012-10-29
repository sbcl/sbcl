(in-package "SB!VM")

;;;; Non-local exit noise.

(define-assembly-routine (unwind
                          (:return-style :none)
                          (:translate %continue-unwind)
                          (:policy :fast-safe))
                         ((:arg block (any-reg descriptor-reg) r0-offset)
                          (:arg start (any-reg descriptor-reg) r8-offset)
                          (:arg count (any-reg descriptor-reg) nargs-offset)
                          (:temp ocfp non-descriptor-reg ocfp-offset)
                          (:temp lra descriptor-reg lra-offset)
                          (:temp cur-uwp any-reg nfp-offset))
  (declare (ignore start count))

  (let ((error (generate-error-code nil ocfp 'invalid-unwind-error)))
    (inst cmp block 0)
    (inst b :eq error))

  (load-symbol-value cur-uwp *current-unwind-protect-block*)
  (loadw ocfp block unwind-block-current-uwp-slot)
  (inst cmp cur-uwp ocfp)

  (loadw ocfp cur-uwp unwind-block-current-uwp-slot 0 :ne)
  (store-symbol-value ocfp *current-unwind-protect-block* :ne)

  (move cur-uwp block :eq)

  (loadw fp-tn cur-uwp unwind-block-current-cont-slot)
  (loadw code-tn cur-uwp unwind-block-current-code-slot)
  (loadw lra cur-uwp unwind-block-entry-pc-slot)
  ;; Shouldn't use LISP-RETURN here because we don't need to signal
  ;; single / multiple values.
  (inst sub pc-tn lra (- other-pointer-lowtag 4)))
