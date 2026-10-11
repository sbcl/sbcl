;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

;;; Used for computing the caller's NFP for use in known-values return.
#-c-stack-is-control-stack
(define-vop (compute-old-nfp)
  (:results (val :scs (any-reg)))
  (:vop-var vop)
  (:generator 1
    (let ((nfp (current-nfp-tn vop)))
      (when nfp
        #+fp-and-pc-standard-save
        (loadw val cfp-tn ocfp-save-offset)
        #-fp-and-pc-standard-save
        (let* ((block (ir2-block-block (vop-block vop)))
               (ocfp (sb-c::ir2-environment-old-fp (sb-c::environment-info
                                                    (sb-c::block-environment block)))))
          (maybe-load-stack-tn val ocfp))
        (loadw val val nfp-save-offset)))))

(macrolet ((def (name lowtag)
             `(define-vop ()
                (:translate ,name)
                (:args (x :scs (descriptor-reg)))
                (:results (res :scs (unsigned-reg)))
                (:result-types unsigned-num)
                (:generator 2 (loadw res x 0 ,lowtag)))))
  (def function-header-word fun-pointer-lowtag)
  (def instance-header-word instance-pointer-lowtag))

(define-vop (sign-extend-word-move word-move)
  (:args (x :target y
            :scs (signed-reg unsigned-reg)
            :load-if (not (location= x y))))
  (:results (y :scs (signed-reg)
               :load-if (not (location= x y)))))

(define-vop (sign-extend-fixnum-move move-from-word/fixnum)
  (:args (x :scs (signed-reg unsigned-reg)))
  (:results (y :scs (any-reg descriptor-reg))))

(define-vop (touch-object)
  (:translate touch-object)
  (:args (object))
  (:ignore object)
  (:arg-types t)
  (:generator 0))

#+(or x86 x86-64)
(define-vop ()
  (:translate touch-object-identity)
  (:args (object :scs (descriptor-reg any-reg sap-reg
                       signed-reg unsigned-reg)
                 :target res))
  (:results (res :scs (descriptor-reg)))
  (:generator 0
    (move res object)))

;;; By pure coincidence, almost all the architectures that lack a +-MODFX vop
;;; have an instruction named ADD with the same argument order.
;;; MIPS needs to use ADDU to avoid trapping on overflow.
#+(or arm mips ppc sparc)
(define-vop (+-modfx)
  (:translate +-modfx)
  (:args (x :scs (any-reg)) (y :scs (any-reg)))
  (:arg-types tagged-num tagged-num)
  (:results (res :scs (any-reg)))
  (:result-types fixnum)
  (:generator 1 (inst #+mips addu #-mips add res x y)))

;;; This vop is safe even if the user calls ALIEN-SAP by hand - the compiler will assert
;;; the arg to be of type SB-ALIEN-INTERNALS:ALIEN-VALUE (unless checking is dsabled).
;;; *** produces a bad build on ppc and ppc64 - why? ***
#+(or arm64 loongarch64 riscv x86 x86-64)
(define-vop (alien-sap)
  (:translate alien-sap)
  (:args (x :scs (descriptor-reg)))
  (:results (r :scs (descriptor-reg)))
  (:generator 1
   (loadw r x (+ (get-dsd-index alien-value sb-kernel::sap) instance-slots-offset)
          instance-pointer-lowtag)))
