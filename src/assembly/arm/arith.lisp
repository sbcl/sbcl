;;;; stuff to handle simple cases for generic arithmetic

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;;; division

(define-assembly-routine (signed-truncate
                          (:note "(signed-byte 32) truncate")
                          (:cost 60)
                          (:policy :fast-safe)
                          (:translate truncate)
                          (:arg-types signed-num signed-num)
                          (:result-types signed-num signed-num))

                         ((:arg dividend signed-reg nl2-offset)
                          (:arg divisor signed-reg nargs-offset)

                          (:res quo signed-reg nl2-offset)
                          (:res rem signed-reg nl3-offset)

                          (:temp quo-sign non-descriptor-reg ocfp-offset)
                          (:temp rem-sign descriptor-reg r8-offset))

  (inst eor quo-sign dividend divisor)
  (inst bic rem-sign dividend fixnum-tag-mask)
  (inst mov rem 0)

  (inst tst divisor divisor)
  (let ((error (generate-error-code nil 'division-by-zero-error
                                    dividend divisor)))
    (inst b :eq error))
  (inst rsb :mi divisor divisor 0)
  (inst tst dividend dividend)
  (inst rsb :mi dividend dividend 0)

  (dotimes (i 33)
    (inst cmp rem divisor)
    (inst sub :hs rem rem divisor)
    (inst adcs quo quo quo)
    (unless (= i 32)
      (inst adc rem rem rem)))

  ;; If the quo-sign is negative, we need to negate quo.
  (inst tst quo-sign quo-sign)
  (inst rsb :mi quo quo 0)

  ;; If the rem-sign is negative, we need to negate rem.
  (inst tst rem-sign rem-sign)
  (inst rsb :mi rem rem 0))
