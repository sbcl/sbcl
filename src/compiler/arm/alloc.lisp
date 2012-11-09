;;;; allocation VOPs for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Special purpose inline allocators.

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (pa-flag)
        (if stack-allocate-p
            #!-(or)
            (error "Stack allocation for MAKE-CLOSURE not yet implemented")
            #!+(or)
            (progn
              (align-csp result)
              (inst clrrwi. result csp-tn n-lowtag-bits)
              (inst addi csp-tn csp-tn alloc-size)
              (inst ori result result fun-pointer-lowtag)
              (inst lr temp (logior (ash (1- size) n-widetag-bits) closure-header-widetag)))
            (progn
              (allocation result alloc-size
                          fun-pointer-lowtag :flag-tn pa-flag)
              (inst mov pa-flag (ash (1- size) n-widetag-bits))
              (inst orr pa-flag pa-flag closure-header-widetag)))
        (storew pa-flag result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
;;;
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:info stack-allocate-p)
  (:ignore stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation (result pa-flag value-cell-header-widetag value-cell-size)
      (storew value result value-cell-value-slot other-pointer-lowtag))))

;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst mov result unbound-marker-widetag)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:results (result :scs (any-reg)))
  (:generator 1
    (let ((fixup (gen-label)))
      (inst ldr result fixup)
      (assemble (*elsewhere*)
        (emit-label fixup)
        (inst word (make-fixup "funcallable_instance_tramp" :foreign))))))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:generator 4
    (with-fixed-allocation (result pa-flag type words :lowtag lowtag)
      )))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:generator 6
    ;; Build the object header.
    (inst add bytes extra (* words n-word-bytes))
    (inst mov header (lsl bytes (- n-widetag-bits n-fixnum-tag-bits)))
    (inst add header header type)
    ;; Round up to the actual allocation granularity.
    (inst add bytes n-word-bytes)
    (inst bic bytes lowtag-mask)
    ;; Allocate the object and set its header.
    (pseudo-atomic (pa-flag)
      (allocation result bytes lowtag :flag-tn pa-flag)
      (storew header result 0 lowtag))))
