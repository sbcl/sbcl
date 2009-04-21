;;;; x86 support for the debugger

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(define-vop (debug-cur-sp)
  (:translate current-sp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg sap-stack)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res esp-tn)))

(define-vop (debug-cur-fp)
  (:translate current-fp)
  (:policy :fast-safe)
  (:results (res :scs (sap-reg sap-stack)))
  (:result-types system-area-pointer)
  (:generator 1
    (move res ebp-tn)))

;;; Stack-ref and %set-stack-ref can be used to read and store
;;; descriptor objects on the control stack. Use the sap-ref
;;; functions to access other data types.
(define-vop (read-control-stack)
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
          (make-ea :dword :base sap :disp (frame-byte-offset 0) :index temp))))

(define-vop (read-control-stack-c)
  (:translate stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg)))
  (:info index)
  (:arg-types system-area-pointer (:constant (signed-byte 30)))
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst mov result (make-ea :dword :base sap
                              :disp (frame-byte-offset index)))))

(define-vop (write-control-stack)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg) :to :eval)
         (offset :scs (any-reg) :target temp)
         (value :scs (descriptor-reg) :to :result :target result))
  (:arg-types system-area-pointer positive-fixnum *)
  (:temporary (:sc unsigned-reg :from (:argument 1) :to :result) temp)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 9
    (move temp offset)
    (inst neg temp)
    (inst mov
          (make-ea :dword :base sap :disp (frame-byte-offset 0) :index temp)
          value)
    (move result value)))

(define-vop (write-control-stack-c)
  (:translate %set-stack-ref)
  (:policy :fast-safe)
  (:args (sap :scs (sap-reg))
         (value :scs (descriptor-reg) :target result))
  (:info index)
  (:arg-types system-area-pointer (:constant (signed-byte 30)) *)
  (:results (result :scs (descriptor-reg)))
  (:result-types *)
  (:generator 5
    (inst mov (make-ea :dword :base sap :disp (frame-byte-offset index))
          value)
    (move result value)))

(define-vop (code-from-mumble)
  (:policy :fast-safe)
  (:args (thing :scs (descriptor-reg)))
  (:results (code :scs (descriptor-reg)))
  (:temporary (:sc unsigned-reg) temp)
  (:variant-vars lowtag)
  (:generator 5
    (let ((bogus (gen-label))
          (done (gen-label)))
      (loadw temp thing 0 lowtag)
      (inst shr temp n-widetag-bits)
      (inst jmp :z bogus)
      (inst shl temp (1- (integer-length n-word-bytes)))
      (unless (= lowtag other-pointer-lowtag)
        (inst add temp (- lowtag other-pointer-lowtag)))
      (move code thing)
      (inst sub code temp)
      (emit-label done)
      (assemble (*elsewhere*)
        (emit-label bogus)
        (inst mov code nil-value)
        (inst jmp done)))))

(define-vop (code-from-lra code-from-mumble)
  (:translate sb!di::lra-code-header)
  (:variant other-pointer-lowtag))

(define-vop (code-from-function code-from-mumble)
  (:translate sb!di::fun-code-header)
  (:variant fun-pointer-lowtag))

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
  (:translate sb!di::get-lisp-obj-address)
  (:args (thing :scs (descriptor-reg control-stack) :target result))
  (:results (result :scs (unsigned-reg)
                    :load-if (not (and (sc-is thing descriptor-reg)
                                       (sc-is result unsigned-stack)))))
  (:result-types unsigned-num)
  (:generator 1
    (move result thing)))


(define-vop (fun-word-offset)
  (:policy :fast-safe)
  (:translate sb!di::fun-word-offset)
  (:args (fun :scs (descriptor-reg)))
  (:results (res :scs (unsigned-reg)))
  (:result-types positive-fixnum)
  (:generator 5
    (loadw res fun 0 fun-pointer-lowtag)
    (inst shr res n-widetag-bits)))
