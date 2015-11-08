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
  (:temporary (:scs (descriptor-reg) :type list :to (:result 0) :target result)
              res)
  (:temporary (:sc non-descriptor-reg) pa-flag temp)
  (:temporary (:scs (interior-reg)) lip)
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
                             :stack-allocate-p (node-stack-allocate-p node)
                             :lip lip)
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
#!-gencgc
(define-vop (allocate-code-object)
  (:args (boxed-arg :scs (any-reg))
         (unboxed-arg :scs (any-reg)))
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) ndescr)
  (:temporary (:scs (non-descriptor-reg)) size)
  (:temporary (:scs (any-reg) :from (:argument 0)) boxed)
  (:temporary (:scs (non-descriptor-reg)) unboxed)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 100
    (inst add boxed boxed-arg (fixnumize (1+ code-constants-offset)))
    (inst and boxed boxed (bic-mask lowtag-mask))
    (inst lsr unboxed unboxed-arg word-shift)
    (inst add unboxed unboxed lowtag-mask)
    (inst and unboxed unboxed (bic-mask lowtag-mask))
    (inst lsl ndescr boxed (- n-widetag-bits word-shift))
    (inst orr ndescr ndescr code-header-widetag)
    (inst add size boxed unboxed)
    (pseudo-atomic (pa-flag)
      (allocation result size other-pointer-lowtag :flag-tn pa-flag :lip lip)
      (storew ndescr result 0 other-pointer-lowtag)
      (storew unboxed-arg result code-code-size-slot other-pointer-lowtag)
      (storew null-tn result code-entry-points-slot other-pointer-lowtag)
      (storew null-tn result code-debug-info-slot other-pointer-lowtag))))

(define-vop (make-fdefn)
  (:args (name :scs (descriptor-reg) :to :eval))
  (:temporary (:sc non-descriptor-reg) pa-flag temp)
  (:temporary (:scs (interior-reg)) lip)
  (:results (result :scs (descriptor-reg) :from :argument))
  (:policy :fast-safe)
  (:translate make-fdefn)
  (:generator 37
    (with-fixed-allocation (result pa-flag fdefn-widetag fdefn-size :lip lip)
      (load-inline-constant temp '(:fixup "undefined_tramp" :foreign) lip)
      (storew name result fdefn-name-slot other-pointer-lowtag)
      (storew null-tn result fdefn-fun-slot other-pointer-lowtag)
      (storew temp result fdefn-raw-addr-slot other-pointer-lowtag))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info length stack-allocate-p)
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:temporary (:scs (interior-reg)) lip)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (pa-flag)
        (allocation result alloc-size
                    fun-pointer-lowtag
                    :flag-tn pa-flag
                    :stack-allocate-p stack-allocate-p
                    :lip lip)
        (load-immediate-word pa-flag
                             (logior
                              (ash (1- size) n-widetag-bits)
                              closure-header-widetag))
        (storew pa-flag result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
;;;
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:temporary (:scs (interior-reg)) lip)
  (:info stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation (result pa-flag value-cell-header-widetag
                            value-cell-size :stack-allocate-p stack-allocate-p
                            :lip lip)
      (storew value result value-cell-value-slot other-pointer-lowtag))))

;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst mov result unbound-marker-widetag)))

(define-vop (make-funcallable-instance-tramp)
  (:args)
  (:temporary (:scs (interior-reg)) lip)
  (:results (result :scs (any-reg)))
  (:generator 1
    (load-inline-constant result '(:fixup "funcallable_instance_tramp" :foreign) lip)))

(define-vop (fixed-alloc)
  (:args)
  (:info name words type lowtag stack-allocate-p)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:sc non-descriptor-reg) pa-flag)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 4
    (with-fixed-allocation (result pa-flag type words
                            :lowtag lowtag
                            :stack-allocate-p stack-allocate-p
                            :lip lip))))

(define-vop (var-alloc)
  (:args (extra :scs (any-reg)))
  (:arg-types positive-fixnum)
  (:info name words type lowtag)
  (:ignore name)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg) :from :argument) bytes)
  (:temporary (:sc non-descriptor-reg) pa-flag header)
  (:temporary (:scs (interior-reg)) lip)
  (:generator 6
    ;; Build the object header, assuming that the header was in WORDS
    ;; but should not be in the header
    (inst lsl bytes extra (- word-shift n-fixnum-tag-bits))
    (inst add bytes bytes (add-sub-immediate (* (1- words) n-word-bytes)))
    (inst lsl header bytes (- n-widetag-bits word-shift))
    (inst add header header type)
    ;; Add the object header to the allocation size and round up to
    ;; the allocation granularity
    (inst add bytes bytes (* 2 n-word-bytes))
    (inst and bytes bytes (bic-mask lowtag-mask))
    ;; Allocate the object and set its header
    (pseudo-atomic (pa-flag)
      (allocation result bytes lowtag :flag-tn pa-flag :lip lip)
      (storew header result 0 lowtag))))
