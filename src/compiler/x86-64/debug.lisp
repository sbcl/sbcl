;;;; x86 support for the debugger

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-vop ()
  (:translate current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg sap-stack)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res rsp-tn)))

(define-vop (current-fp-sap)
  (:translate current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg sap-stack)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res rbp-tn)))

;;; Stack-ref and %set-stack-ref can be used to read and store
;;; descriptor objects on the control stack. Use the sap-ref
;;; functions to access other data types.
(define-vop ()
  (:translate stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to :eval)
         (offset :scs (any-reg) :target temp))
  (:arg-types system-area-pointer positive-fixnum)
  (:temporary (:sc unsigned-reg :from (:argument 1)) temp)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 9
    (move temp offset)
    (inst neg temp)
    (inst mov result
          (ea (frame-byte-offset 0) sap
              temp (ash 1 (- word-shift n-fixnum-tag-bits))))))

(define-vop ()
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to :eval)
         (offset :scs (any-reg) :target temp)
         (value :scs (descriptor-reg)))
  (:arg-types system-area-pointer positive-fixnum *)
  (:temporary (:sc unsigned-reg :from (:argument 1)) temp)
  (:generator 9
    (move temp offset)
    (inst neg temp)
    (inst mov
          (ea (frame-byte-offset 0) sap
              temp (ash 1 (- word-shift n-fixnum-tag-bits)))
          value)))

(define-vop ()
  (:translate fun-code-header)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg)))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg :to (:result 0) :target code) temp)
  (:generator 5
    ;; This vop is supposed to return NIL if the simple-fun has an invalid header
    ;; as determined by the relative backpointer being 0.  I don't know why a simple-fun
    ;; would ever be constructed like that.
    ;; The largest displacement in words from a code header to
    ;; the header word of a contained function is #xFFFFFF.
    ;; (See FUN_HEADER_NWORDS_MASK in 'gc.h')
    (inst mov :dword temp (ea (- fun-pointer-lowtag) thing))
    (inst shr :dword temp n-widetag-bits)
    (inst neg temp)
    (inst lea code (ea (- other-pointer-lowtag fun-pointer-lowtag)
                       thing temp n-word-bytes))
    ;; note that LEA does not affect any flags
    (inst cmov :z code null-tn))) ; put NIL into CODE if ZF

(define-vop (%make-lisp-obj)
  (:policy :fast-safe)
  (:translate %make-lisp-obj)
  (:args (value :scs (unsigned-reg unsigned-stack) :target result))
  (:arg-types unsigned-num)
  (:results (result :scs (descriptor-reg)
                    :load-if (not (sc-is value unsigned-reg))
                    ))
  (:generator 1
    (move result value)))

(define-vop (get-lisp-obj-address)
  (:policy :fast-safe)
  (:translate sb-di::get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg control-stack) :target result))
  (:results (result :scs (unsigned-reg)
                    :load-if (not (and (sc-is thing descriptor-reg)
                                       (sc-is result unsigned-stack)))))
  (:result-types unsigned-num)
  (:generator 1
    (move result thing)))
