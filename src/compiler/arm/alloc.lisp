;;;; allocation VOPs for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB-VM")

(define-vop (list)
  (:args (things :more t :scs (any-reg descriptor-reg null control-stack)))
  (:temporary (:scs (descriptor-reg)) ptr)
  (:temporary (:scs (any-reg)) temp)
  (:temporary (:scs (descriptor-reg) :to (:result 0) :target result)
              res)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:info star cons-cells)
  (:results (result :scs (descriptor-reg)))
  (:node-var node)
  (:generator 0
    (macrolet ((maybe-load (tn)
                  (once-only ((tn tn))
                    `(sc-case ,tn
                       ((any-reg descriptor-reg null)
                        ,tn)
                       (control-stack
                        (load-stack-tn temp ,tn)
                        temp)))))
      (let ((alloc (* (pad-data-block cons-size) cons-cells)))
        (pseudo-atomic (pa-flag)
                 (allocation 'list alloc list-pointer-lowtag res
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
        (move result res)))))

;;;; Special purpose inline allocators.

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
      (assemble (:elsewhere)
        (emit-label undefined-tramp-fixup)
        (inst word (make-fixup 'undefined-tramp :assembly-routine))))))

(define-vop (make-closure)
  (:args (function :to :save :scs (descriptor-reg)))
  (:info label length stack-allocate-p)
  (:ignore label)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (let* ((size (+ length closure-info-offset))
           (alloc-size (pad-data-block size)))
      (pseudo-atomic (pa-flag)
        (allocation nil alloc-size fun-pointer-lowtag result
                    :flag-tn pa-flag
                    :stack-allocate-p stack-allocate-p)
        (load-immediate-word pa-flag
                             (logior
                              (ash (1- size) n-widetag-bits)
                              closure-widetag))
        (storew pa-flag result 0 fun-pointer-lowtag)
        (storew function result closure-fun-slot fun-pointer-lowtag)))))

;;; The compiler likes to be able to directly make value cells.
;;;
(define-vop (make-value-cell)
  (:args (value :to :save :scs (descriptor-reg any-reg)))
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:results (result :scs (descriptor-reg)))
  (:generator 10
    (with-fixed-allocation (result pa-flag value-cell-widetag
                            value-cell-size)
      (storew value result value-cell-value-slot other-pointer-lowtag))))

;;;; Automatic allocators for primitive objects.

(define-vop (make-unbound-marker)
  (:args)
  (:results (result :scs (descriptor-reg any-reg)))
  (:generator 1
    (inst mov result unbound-marker-widetag)))

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
  (:info name words type lowtag stack-allocate-p)
  (:ignore name stack-allocate-p)
  (:results (result :scs (descriptor-reg)))
  (:temporary (:scs (any-reg)) bytes)
  (:temporary (:scs (non-descriptor-reg)) header)
  (:temporary (:sc non-descriptor-reg :offset ocfp-offset) pa-flag)
  (:generator 6
    ;; Build the object header, assuming that the header was in WORDS
    ;; but should not be in the header
    (if (= type code-header-widetag)
        (inst add bytes extra 0)
        (inst add bytes extra (* (1- words) n-word-bytes)))
    (inst mov header (lsl bytes (- (length-field-shift type) n-fixnum-tag-bits)))
    (inst add header header type)
    ;; Add the object header to the allocation size and round up to
    ;; the allocation granularity
    ;; The specified EXTRA value is the exact value placed in the header
    ;; as the word count when allocating code.
    (unless (= type code-header-widetag)
      (inst add bytes bytes (* 2 n-word-bytes)))
    (inst bic bytes bytes lowtag-mask)
    ;; Allocate the object and set its header
    (pseudo-atomic (pa-flag)
      (allocation nil bytes lowtag result :flag-tn pa-flag)
      (storew header result 0 lowtag))))
