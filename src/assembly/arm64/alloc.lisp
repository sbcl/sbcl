(in-package "SB!VM")

#!+sb-thread
(define-assembly-routine (alloc-tls-index
                          (:translate ensure-symbol-tls-index)
                          (:result-types positive-fixnum)
                          (:policy :fast-safe))
    ((:arg symbol (descriptor-reg) r0-offset)
     (:temp free-tls-index (non-descriptor-reg) nl1-offset)

     (:res result (unsigned-reg) nl0-offset))
  ;; *free-tls-index* is [lock][tls-index]
  (inst mov free-tls-index (+ nil-value (static-symbol-offset '*free-tls-index*)
                              (- (* symbol-value-slot n-word-bytes)
                                 other-pointer-lowtag)
                              #!+little-endian 4))
  (pseudo-atomic (free-tls-index)
    (assemble ()
      RETRY
      ;; Acquire the lock
      (inst ldaxr (32-bit-reg result) free-tls-index)
      (inst cbnz (32-bit-reg result) RETRY)
      (inst stxr result (32-bit-reg null-tn) free-tls-index)
      (inst cbnz (32-bit-reg result) RETRY)

      ;; Now we hold the spinlock. With it held, see if the symbol's
      ;; tls-index has been set in the meantime.
      (inst ldr (32-bit-reg result) (tls-index-of symbol))
      (inst cbnz (32-bit-reg result) RELEASE-LOCK)

      ;; Allocate a new tls-index.
      (inst ldr (32-bit-reg result) (@ free-tls-index #!+little-endian -4 #!+big-endian 4))
      (inst cmp (32-bit-reg result) (* tls-size n-word-bytes))
      (inst b :hs tls-full)

      (inst add (32-bit-reg tmp-tn) (32-bit-reg result) n-word-bytes)
      (inst str (32-bit-reg tmp-tn) (@ free-tls-index #!+little-endian -4 #!+big-endian 4))
      (inst str (32-bit-reg result) (tls-index-of symbol))

      RELEASE-LOCK
      (inst stlr (32-bit-reg zr-tn) free-tls-index)))
  (inst ret)
  TLS-FULL
  (inst stlr (32-bit-reg zr-tn) free-tls-index) ;; release the lock
  (error-call nil 'tls-exhausted-error))
