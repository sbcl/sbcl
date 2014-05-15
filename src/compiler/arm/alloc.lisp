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

(define-vop (list-or-list*)
  (:args (things :more t :scs (control-stack)))
  (:temporary (:scs (descriptor-reg) :type list) ptr)
  (:temporary (:scs (any-reg)) temp)
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
              res)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:info num)
  (:results (result :scs (descriptor-reg)))
  (:variant-vars star)
  (:policy :fast-safe)
  (:node-var node)
  (:generator 0
    (cond ((zerop num)
           (move result null-tn))
          ((and star (= num 1))
           (move result (tn-ref-tn things)))
          (t
           (macrolet
               ((maybe-load (tn)
                  (once-only ((tn tn))
                    `(sc-case ,tn
                       ((any-reg descriptor-reg null)
                        ,tn)
                       (control-stack
                        (load-stack-tn temp ,tn)
                        temp)))))
             (let* ((cons-cells (if star (1- num) num))
                    (alloc (* (pad-data-block cons-size) cons-cells)))
               (pseudo-atomic (pa-flag)
                 (allocation res alloc list-pointer-lowtag
                             :flag-tn pa-flag
                             :stack-allocate-p (node-stack-allocate-p node))
                 (move ptr res)
                 (dotimes (i (1- cons-cells))
                   (storew (maybe-load (tn-ref-tn things)) ptr
                       cons-car-slot list-pointer-lowtag)
                   (setf things (tn-ref-across things))
                   (inst add ptr ptr (pad-data-block cons-size))
                   (storew ptr ptr
                       (- cons-cdr-slot cons-size)
                       list-pointer-lowtag))
                 (storew (maybe-load (tn-ref-tn things)) ptr
                     cons-car-slot list-pointer-lowtag)
                 (storew (if star
                             (maybe-load (tn-ref-tn (tn-ref-across things)))
                             null-tn)
                     ptr cons-cdr-slot list-pointer-lowtag))
               (move result res)))))))

(define-vop (list list-or-list*)
  (:variant nil))

(define-vop (list* list-or-list*)
  (:variant t))

;;;; Special purpose inline allocators.

(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
         (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (non-descriptor-reg)) size)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg) :from (:argument 1)) unboxed)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:generator 100
    (inst add boxed boxed-arg (fixnumize (1+ code-trace-table-offset-slot)))
    (inst bic boxed boxed lowtag-mask)
    (inst mov unboxed (lsr unboxed-arg word-shift))
    (inst add unboxed unboxed lowtag-mask)
    (inst bic unboxed unboxed lowtag-mask)
    (inst mov ndescr (lsl boxed (- n-widetag-bits word-shift)))
    (inst orr ndescr ndescr code-header-widetag)
    (inst add size boxed unboxed)
    (pseudo-atomic (pa-flag)
      (allocation result size other-pointer-lowtag :flag-tn pa-flag)
      (storew ndescr result 0 other-pointer-lowtag)
      (storew unboxed result code-code-size-slot other-pointer-lowtag)
      (storew null-tn result code-entry-points-slot other-pointer-lowtag)
      (storew null-tn result code-debug-info-slot other-pointer-lowtag))))

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:temporary (:sc interior-reg) lip)
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (let ((undefined-tramp-fixup (gen-label)))
      (with-fixed-allocation (result pa-flag fdefn-widetag fdefn-size)
        (inst load-from-label temp lip undefined-tramp-fixup)
        (storew name result fdefn-name-slot other-pointer-lowtag)
        (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
        (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))
      (assemble (*elsewhere*)
        (emit-label undefined-tramp-fixup)
        (inst word (make-fixup "undefined_tramp" :foreign))))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (pa-flag)
        (allocation result alloc-size
                    fun-pointer-lowtag
                    :flag-tn pa-flag
                    :stack-allocate-p stack-allocate-p)
        (inst mov pa-flag (ash (1- size) n-widetag-bits))
        (inst orr pa-flag pa-flag closure-header-widetag)
        (storew pa-flag result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
;;;
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:info stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation (result pa-flag value-cell-header-widetag
                            value-cell-size :stack-allocate-p stack-allocate-p)
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
  (:temporary (:sc interior-reg) lip)
  (:generator 1
    (let ((fixup (gen-label)))
      (inst load-from-label result lip fixup)
      (assemble (*elsewhere*)
        (emit-label fixup)
        (inst word (make-fixup "funcallable_instance_tramp" :foreign))))))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:generator 4
    (with-fixed-allocation (result pa-flag type words
                            :lowtag lowtag
                            :stack-allocate-p stack-allocate-p))))

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
    ;; Build the object header, assuming that the header was in WORDS
    ;; but should not be in the header
    (inst add bytes extra (* (1- words) n-word-bytes))
    (inst mov header (lsl bytes (- n-widetag-bits n-fixnum-tag-bits)))
    (inst add header header type)
    ;; Add the object header to the allocation size and round up to
    ;; the allocation granularity
    (inst add bytes bytes (* 2 n-word-bytes))
    (inst bic bytes bytes lowtag-mask)
    ;; Allocate the object and set its header
    (pseudo-atomic (pa-flag)
      (allocation result bytes lowtag :flag-tn pa-flag)
      (storew header result 0 lowtag))))
