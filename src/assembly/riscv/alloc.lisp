;;;; allocating simple objects

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

#+sb-thread
(define-assembly-routine (alloc-tls-index
                          (:translate ensure-symbol-tls-index)
                          (:result-types positive-fixnum)
                          (:policy :fast-safe))
    ((:arg symbol (descriptor-reg) l0-offset)
     (:temp free-tls-index (any-reg) l1-offset)
     (:temp temp (non-descriptor-reg) nl1-offset)
     (:res tls-index (unsigned-reg) nl0-offset))
  (inst addi free-tls-index null-tn (+ (static-symbol-offset '*free-tls-index*)
                                       (ash symbol-value-slot word-shift)
                                       (- other-pointer-lowtag)))
  (pseudo-atomic (temp)
    ACQUIRE-LOCK
    (inst li temp 1)
    (inst slli temp temp (1- n-word-bits))
    WITH-LOCK-BIT
    (loadw tls-index free-tls-index)
    ;; The MSB (i.e. sign) is the semaphore.
    (inst blt tls-index zero-tn WITH-LOCK-BIT)
    (inst amoor temp temp free-tls-index :aq)
    (inst bne tls-index temp ACQUIRE-LOCK)

    ;; With the spinlock now held, see if the symbol's tls-index has
    ;; been set in the meantime.
    (load-tls-index tls-index symbol)
    (inst bne tls-index zero-tn RELEASE-LOCK)

    ;; Allocate a new tls-index.
    (move tls-index temp)
    (loadw temp thread-base-tn thread-tls-size-slot)
    (inst bgeu tls-index temp tls-full)

    (store-tls-index tls-index symbol)
    (inst addi temp tls-index n-word-bytes)

    RELEASE-LOCK
    ;; Update the free-tls-index and release the lock in one go.
    (inst amoswap zero-tn temp free-tls-index :rl))
  (inst jalr zero-tn lip-tn 0)
  TLS-FULL
  ;; Release the lock.
  (storew tls-index free-tls-index)
  (clear-pseudo-atomic-bit)
  (error-call nil 'tls-exhausted-error))
